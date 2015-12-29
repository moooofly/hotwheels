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
          socket,
          data,
          proxy,
          token
         }).

start(Socket) ->
    Send = fun(Bin) -> gen_tcp:send(Socket, [Bin, 1]) end,
    {ok, Proxy, Token} = client_proxy:start(Send),
    State = #state{socket=Socket, proxy=Proxy, token=Token},
    lager:info("[janus_flash] start => send {\"timestamp\":xxx, \"token\":xxx} to peer.", []),
    JSON = {struct,
                [{<<"timestamp">>, tuple_to_list(now())},
                 {<<"token">>, Token}
            ]},
    send(mochijson2:encode(JSON), State).

stop(State) ->
    catch client_proxy:detach(State#state.proxy),
    ok.

forward(Bin, State)
  when is_binary(Bin) ->
    send(Bin, State).

process(heartbeat, State) ->
    lager:info("[janus_flash] process => send PING(heartbeat) to peer.", []),
    send(<<"PING">>, State);

process(ack, State) ->
    lager:info("[janus_flash] process => send ACK to peer.", []),
    send(<<"ACK">>, State);

process(<<>>, State) ->
    % lager:info("[janus_flash] process => no more data", []),
    {ok, keep_alive, State};

process(<<"<regular-socket/>", 0, Bin/binary>>, State) ->
    lager:info("[janus_flash] process => with head.", []),
    process(Bin, State);

process(Bin, State) 
  when is_binary(State#state.data),
       is_binary(Bin) ->
    lager:info("[janus_flash] process => with buffer-data.", []),
    process(list_to_binary([State#state.data, Bin]),
            State#state{data = undefined});

process(Bin, State) 
  when is_binary(Bin) ->
    % lager:info("[janus_flash] process => without buffer-data, Bin = ~p", [Bin]),
    process(bin:split("\\000", Bin), State);

process({more, Bin}, State) ->
    {ok, keep_alive, State#state{data = Bin}};

process({ok, <<>>, <<>>}, State) ->
    {ok, keep_alive, State};

process({ok, <<>>, Rest}, State) ->
    process(Rest, State);

process({ok, <<"PING">>, Rest}, State) ->
    lager:info("[janus_flash] process => recv PING from peer.", []),
    process(Rest, State);

process({ok, <<"PONG">>, Rest}, State) ->
    lager:info("[janus_flash] process => recv PONG(heartbeat) from peer.", []),
    process(Rest, State);

process({ok, <<"PUBLISH">>, Rest}, State) ->
    JSON = {struct, [{<<"topic">>, Topic},
                     {<<"event">>, _},
                     {<<"message_id">>, _},
                     {<<"data">>, _}
                    ]} = mochijson2:decode(Rest),
    lager:info("[janus_flash] process => recv PUBLISH, topman start to publish(Topic:~p).", [Topic]),
    topman:publish(JSON, Topic),
    {ok, shutdown, State};

process({ok, Bin, Rest}, State) ->
    {struct,
        [{<<"action">>, Action}, 
         {<<"topic">>, Topic}
     ]} = mochijson2:decode(Bin),
     lager:info("[janus_flash] process => recv {\"action\":~p, \"topic\":~p} from peer.", [Action, Topic]),
     lager:debug("[janus_flash] process => cast (un)subscribe info to client_proxy.", []),
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

