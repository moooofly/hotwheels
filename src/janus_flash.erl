%%
%% 思考：
%% 1. 为何 send 行为分散到两个模块中
%% 2. 为何仅针对 subscribe 和 unsubscribe 进行异步 cast 处理
%%
%% 该模块实际上为 transport 的 API 调用封装
%% 1. 提供了协议解析功能
%% 2. 提供了针对简单协议的直接处理回复功能
%%

-module(janus_flash).

-export([process/2, forward/2, start/1, stop/1]).

-record(state, {
          socket,   %% client socket
          data,     %% 缓存
          proxy,    %% client_proxy 进程 pid
          token     %% 标识 client_proxy 进程的随机十六进制字符串
         }).

start(Socket) ->
    %% [Node] [Bin, 1] 的设计意图，协议数据分隔符
    Send = fun(Bin) -> gen_tcp:send(Socket, [Bin, 1]) end,
    {ok, Proxy, Token} = client_proxy:start(Send),
    State = #state{
        socket = Socket, 
        proxy = Proxy, 
        token = Token
     },
    error_logger:info_msg("janus_flash:start => send {\"timestamp\":xxx, \"token\":xxx} to peer.~n", []),
    JSON = {struct,
                [{<<"timestamp">>, tuple_to_list(now())},
                 {<<"token">>, Token}
            ]},
    send(mochijson2:encode(JSON), State).

stop(State) ->
    catch client_proxy:detach(State#state.proxy),
    ok.

%% 直接转发，针对订阅 Topic 的广播消息
forward(Bin, State)
  when is_binary(Bin) ->
    send(Bin, State).

%% janus_flash:process 仅在 transport 模块中调用，用于处理 client socket 上收到的内容
%% 
%% 来自 client_proxy 的 "!"
%% 当 publish 消息到 peer 后 30s 内无新消息要发送，则发送 PING
process(heartbeat, State) ->
    error_logger:info_msg("janus_flash:process => send PING(heartbeat) to peer.~n", []),
    send(<<"PING">>, State);

%% 来自 client_proxy 的 "!" ，对应 订阅 或 取消订阅 成功应答
process(ack, State) ->
    error_logger:info_msg("janus_flash:process => send ACK to peer.~n", []),
    send(<<"ACK">>, State);

process(<<>>, State) ->
    error_logger:info_msg("janus_flash:process => no more data.~n", []),
    {ok, keep_alive, State};

%% 处理尚有缓存数据的情况
process(Bin, State) 
  when is_binary(State#state.data),
       is_binary(Bin) ->
    error_logger:info_msg("janus_flash:process => with buffer-data.~n", []),
    process(list_to_binary([State#state.data, Bin]),
            State#state{data = undefined});

%% 处理带 "<regular-socket/>" 头的情况
process(<<"<regular-socket/>", 0, Bin/binary>>, State) ->
    error_logger:info_msg("janus_flash:process => with head.~n", []),
    process(Bin, State);

%% 处理没有缓存数据的情况
process(Bin, State) 
  when is_binary(Bin) ->
    error_logger:info_msg("janus_flash:process => without buffer-data, Bin = ~p~n", [Bin]),
    process(bin:split("\\000", Bin), State);

process({more, Bin}, State) ->
    {ok, keep_alive, State#state{data = Bin}};

process({ok, <<>>, <<>>}, State) ->
    {ok, keep_alive, State};

process({ok, <<>>, Rest}, State) ->
    process(Rest, State);

%% 收到来自 peer 的 PING
process({ok, <<"PING">>, Rest}, State) ->
    error_logger:info_msg("janus_flash:process => recv PING from peer.~n", []),
    process(Rest, State);

%% 收到来自 peer 的 PONG
process({ok, <<"PONG">>, Rest}, State) ->
    error_logger:info_msg("janus_flash:process => recv PONG(heartbeat) from peer.~n", []),
    process(Rest, State);

%% 收到 PUBLISH 指令
process({ok, <<"PUBLISH">>, Rest}, State) ->
    JSON = {struct, [{<<"topic">>, Topic},
                     {<<"event">>, _},
                     {<<"message_id">>, _},
                     {<<"data">>, _}
                    ]} = mochijson2:decode(Rest),
    error_logger:info_msg("janus_flash:process => recv PUBLISH, topman start to publish(Topic:~p)~n", [Topic]),
    %% 向指定 Topic 进行 publish
    topman:publish(JSON, Topic),
    {ok, shutdown, State};

%% 收到来自 flashbot 的 subscribe 或 unsubscribe
process({ok, Bin, Rest}, State) ->
    {struct,
        [{<<"action">>, Action}, 
         {<<"data">>, Topic}
     ]} = mochijson2:decode(Bin),
     error_logger:info_msg("janus_flash:process => recv {\"action\":~p, \"topic\":~p} from peer.~n", [Action, Topic]),
     error_logger:info_msg("janus_flash:process => cast (un)subscribe info to client_proxy.~n", []),
    %% 发送 subscribe 或 unsubscribe 给 client_proxy
    gen_server:cast(State#state.proxy, {Action, Topic}),
    process(Rest, State).

send(Data, State) ->
    Keep = case gen_tcp:send(State#state.socket, [Data, 1]) of
               ok ->
                   keep_alive;
               _ ->
                   shutdown
           end,
    {ok, Keep, State}.

