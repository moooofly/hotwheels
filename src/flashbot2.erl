%%% Copyright (c) 2009 Oortle, Inc

%%% Permission is hereby granted, free of charge, to any person 
%%% obtaining a copy of this software and associated documentation 
%%% files (the "Software"), to deal in the Software without restriction, 
%%% including without limitation the rights to use, copy, modify, merge, 
%%% publish, distribute, sublicense, and/or sell copies of the Software, 
%%% and to permit persons to whom the Software is furnished to do so, 
%%% subject to the following conditions:

%%% The above copyright notice and this permission notice shall be included 
%%% in all copies or substantial portions of the Software.

%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
%%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
%%% DEALINGS IN THE SOFTWARE.

%% flashbot 即闪刷机器人 -- 定制版本

%% 1. gen_fsm 与 {active, once} 的处理结合
%% 2. 

-module(flashbot2).
-behavior(gen_fsm).

-export([start/1]).

-export([not_connected/2, no_token/2, 
         not_subscribed/2, subscribed/2]).

-export([init/1, handle_event/3, handle_sync_event/4, 
         handle_info/3, terminate/3, code_change/4]).

-record(state, {
          parent,       %% 调用发起者 pid ，即调用 bot:test/1,2,4 的进程
          host,         %% janus 的监听地址
          port,         %% janus 的监听端口
          token,        %% janus 生成随机十六进制字符串，用于标识 client_proxy
          start,        %% 收到 subscribe ack 的时刻
          timer,        %% 用于延时停止当前 flashbot 的定时器
          barrier,      %% counter barrier pid
          expected,     %% 希望收到的消息内容
          topic,        %% 订阅 topic
          socket,       %% connect socket
          data,         %% 数据接收缓冲
          latency       %% janus 发出消息到 flashbot 收到消息之间的延迟
         }).

start(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

init([Parent, Host, Port, Expected, Barrier]) ->
    %% [Note]
    process_flag(save_calls, 64),

    {struct, [{<<"topic">>, Topic},
              {<<"event">>, _},
              {<<"message_id">>,_},
              {<<"data">>, _}]} = Expected,

    State = #state{
      parent = Parent,
      host = Host,
      port = Port,
      expected = Expected,
      topic = Topic,
      barrier = Barrier
     },
    catch erlang:monitor(process, Barrier),
    reconnect(),
    {ok, not_connected, State}.

not_connected(connect, State) ->
    case gen_tcp:connect(State#state.host, 
                         State#state.port, 
                         [binary, 
                          {active, once}, 
                          {packet, 0},
                          {reuseaddr, true}
                         ], 3000) of
        {ok, Sock} ->
            %% 告知 TCP 建链成功
            State#state.parent ! connected,
            %% 获取用于标识 client_proxy 的 token
            %% 此处保证了先发送 PING 后处理 token 的逻辑
            ping(State#state{socket = Sock}, no_token);
        _ ->
            reconnect(),
            {next_state, not_connected, State}
    end.

%% 得到 token 后发起订阅
no_token({struct, [{<<"token">>, Token}]}, #state{topic=Topic}=State) ->
    %% 告知 subscribe 发起中
    State#state.parent ! subscribing,
    JSON = mochijson2:encode({struct, [{action, <<"subscribe">>},
                                       {data, Topic}
                                      ]}),
    Data = [<<"<regular-socket/>">>, 0, JSON],
    %% 发起订阅
    send(Data, State#state{token = Token}, not_subscribed).

%% 订阅成功
not_subscribed(ack, State) ->
    error_logger:info_msg("recv subscribe-ack~n", []),
    %% 增加计数器的值 +1
    barrier:bump(State#state.barrier),
    {next_state, subscribed, State#state{start = now()}}.

%% 
subscribed(token_timeout, State) ->
    {next_state, subscribed, State};

%% 当全部 flashbot 成功 subscribe 后，进入该状态，并在 5s 后停止当前 flashbot
subscribed(ready, State) ->
    Ref = gen_fsm:send_event_after(5000000, timeout),
    {next_state, subscribed, State#state{start = now(), timer = Ref}};

%% 当前 flashbot 通过超时定时器被停止
subscribed(timeout, State) ->
    %% 告知当前 flashbot 停止工作
    State#state.parent ! failure,
    {stop, timeout, State};

%% 收到期望的订阅内容
%% Expected -> 例如
%%            {struct, [{<<"topic">>, <<"kick_off_mt">>},
%%                      {<<"event">>, <<"test_event">>},
%%                      {<<"message_id">>,<<>>},
%%                      {<<"data">>, <<"test">>}]},
subscribed(Expected, State) ->
    %% 告知收到订阅消息
    State#state.parent ! {success, State#state.latency},
    error_logger:info_msg("recv Expected msg => ~n~p~n", [Expected]),
    {next_state, subscribed, State}.

%% [Note]
handle_event(Event, Where, State) ->
    {stop, {unknown_event, Event, Where}, State}.

handle_sync_event(Event, From, Where, State) ->
    {stop, {unknown_sync_event, Event, From, Where}, State}.

%% 若发现 barrier 停止了，则表明所有 flashbot 已经成功订阅
handle_info({'DOWN', _, process, Pid, normal}, Where, State)
  when Pid == State#state.barrier ->
    %% 此处 Where 只能是 subscribed
    ?MODULE:Where(ready, State);

%% 收到 PING
%% 作为订阅者，在收到发布消息之后的 30s 内，未收到新发布消息，则会收到 PING
handle_info({tcp, Sock, <<"PING", 1>>}, Where, State) ->
    error_logger:info_msg("recv PING, send PONG. Where:~p~n", [Where]),
    inet:setopts(Sock, [{active, once}]),
    send(<<"PONG">>, State, Where);

%% 未完整数据处理
handle_info({tcp, Sock, Bin}, Where, State) 
  when State#state.data /= undefined ->
    inet:setopts(Sock, [{active, once}]),
    Bin1 = list_to_binary([State#state.data, Bin]),
    %% [Note] 清空缓冲，特别用意
    State1 = State#state{data = undefined},
    ?MODULE:handle_info({tcp, Sock, Bin1}, Where, State1);

%% 收到订阅成功 ACK
handle_info({tcp, Sock, <<"ACK", 1>>}, Where, State) ->
    inet:setopts(Sock, [{active, once}]),
    %% 此处 Where 只能是 not_subscribed
    ?MODULE:Where(ack, State);

handle_info({tcp, Sock, Bin}, Where, State) ->
    inet:setopts(Sock, [{active, once}]),
    case bin:split("\\001", Bin) of
        {more, Bin} ->  %% 数据不足
            {next_state, Where, State#state{data = Bin}};
        {ok, <<>>, <<>>} -> %% [Note] 这啥情况
            {next_state, Where, State#state{data = Bin}};
        {error, Error} ->   %% [Note] bin:split/2 不会返回这种情况，so？
            {stop, {packet_split, Error}, State};
        {ok, Bin1, Rest} ->
            Now = now(),
            JSON = mochijson2:decode(Bin1),
            %% grab the timestamp
            %% L -> [{<<"token">>, Token}]
            {struct, [{<<"timestamp">>, TS}|L]} = JSON,
            %% 从 janus 发送，到 flashbot 接收之间的延迟
            Delta = timer:now_diff(Now, list_to_tuple(TS)),
            State1 = State#state{latency = Delta, data = undefined},
            %% 发起订阅
            %% 此处的 where 只能是 no_token 或 subscribed
            case ?MODULE:Where({struct, L}, State1) of 
                {next_state, Where1, State2} ->
                    ?MODULE:handle_info({tcp, Sock, Rest}, Where1, State2);
                Other ->
                    Other
            end
    end;

%% TCP 链路关闭或异常
%% [Note] tcp_closed 和 tcp_error 分别代表什么
handle_info(X, _, State) 
  when element(1, X) == tcp_closed;
       element(1, X) == tcp_error ->
    %% 告知 TCP 链路出现问题
    State#state.parent ! disconnected,
    %% [Note] 为何此时停止定时器，如何保证此时一定有定定时器？
    catch gen_fsm:cancel_timer(State#state.timer),
    reconnect(),
    {next_state, not_connected, State#state{timer = none}};

handle_info(Info, Where, State) ->
    {stop, {unknown_info, Info, Where}, State}.

terminate(_Reason, _Where, State) ->
    catch gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVsn, Where, State, _Extra) ->
    {ok, Where, State}.

%% 数据发送 + FSM 状态切换
send(Bin, State, Where) -> 
    case gen_tcp:send(State#state.socket, [Bin, 0]) of
        ok ->
            {next_state, Where, State};
        _ ->
            reconnect(),
            {next_state, not_connected, State}
    end.

%% Where -> 需要切换 FSM 到哪个状态，此处为 no_token
ping(State, Where) ->
    %% [Note] 写法
    Data = [<<"<regular-socket/>", 0>>, <<"PING">>],
    send(Data, State, Where).

%% 清空进程邮箱 + 随机延时发送 connect 事件
reconnect() ->
    flush(),
    %% [Note] 技巧
    gen_fsm:send_event_after(random:uniform(100), connect).

%% [Note] 清空当前进程中的所有消息
%% 哪些场景下需要这种实现
flush() ->
    receive 
        _ ->
            flush()
    after 0 ->
            ok
    end.
