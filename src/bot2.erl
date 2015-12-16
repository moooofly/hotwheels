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

-module(bot2).

-export([run/6, test/1, test/2, test/4, publish/5]).
-export([test/5]).

-include_lib("kernel/include/inet.hrl").

-record(state, {
          bot,          %% 目前为 flashbot
          barrier,      %% 目前用作 counter
          args,
          n,            %% 运行 bot 的目标数目
          good,         %% 成功收到订阅消息的 flashbot 数目
          bad,          %% 停止工作的 flashbot 数目
          min,          %% 最小 latency
          max,          %% 最大 latency
          avg,          %% 平均 latency
          host,         %% 目的 ip 地址
          port,         %% 目的端口
          message,      %% 待发送消息内容
          expected,     %% 期望接收的内容
          start,        %% 开始运行 bot:test 的时刻
          launchers,    %% 当前可用 lancher 数量
          where,        %% dict() 字典，用于维护每个 lancher 进程被使用过的次数
          setup,        %% 从启动 bot:test 到全部 flashbot 订阅到 janus 的时间差
          stats
         }).


%%
%% Message  -> 例如 {<<"events">>, <<"test_event">>, <<"test">>}
%% Expected -> 例如
%%            {struct, [{<<"topic">>, <<"events">>},
%%                      {<<"event">>, <<"test_event">>},
%%                      {<<"message_id">>,<<>>},
%%                      {<<"data">>, <<"test">>}]},
%%
run(Bot, Host, Port, Message, Expected, N) 
  when is_tuple(Message),
       is_integer(N) ->
    {ok, #hostent{h_addr_list = [Addr | _]}} = inet:gethostbyname(Host),
    %% 启动 bot 计数器进程
    {ok, Barrier} = barrier:start(counter, N),
    State = #state{
      bot = Bot,
      barrier = Barrier,
      n = N,
      good = 0, 
      bad = 0,
      min = 99999999,
      max = 0,
      avg = 0,
      host = Addr,
      port = Port,
      message = Message,
      expected = Expected,
      where = dict:new(),
      start = now(),
      stats = []
     },
    run(State, N).

run(State, 0) ->
    %% 通过 monitor barrier 进程的退出，以确认所有 flashbot 创建完成
    erlang:monitor(process, State#state.barrier),
    wait(State, 0, 0);

run(State, N) ->
    %% 从 launcher 进程组中获取一个成员进程 Srv（lancher 进程）
    {Srv, L} = launcher:next(State#state.launchers),
    %% 令该成员进程启动相应的工作
    %% Pid -> 创建的 flashbot 进程 pid
    {ok, Pid} = launcher:launch(Srv, State#state.bot, 
                                [self(),
                                 State#state.host,
                                 State#state.port,
                                 State#state.expected,
                                 State#state.barrier]),
    %% [Note] 特意断开 link 的原因
    unlink(Pid),
    Where = State#state.where,
    %% 更新针对被选中 lancher 进程的使用计数
    %% update_counter(Key, Increment, Dict1) -> Dict2
    State1 = State#state{where = dict:update_counter(Srv, 1, Where)},
    run(State1#state{launchers = L}, N - 1).

%% N -> 与 janus 成功建立 TCP 连接的 flashbot 数量
%% M -> 进入 subscribing 状态的 flashbot 数量
wait(State, N, M) 
  when State#state.good + State#state.bad < State#state.n ->
    receive
        connected ->    %% flashbot 与 janus 成功建立 TCP 连接
            wait(State, N + 1, M);
        disconnected -> %% flashbot 与 janus 之间的 TCP 连接出现问题
            wait(State, N - 1, M);
        subscribing ->  %% 订阅发起中
            wait(State, N, M + 1);
        {success, Latency} ->   %% 收到来自 janus 的订阅内容
            State1 = update_latency(State, Latency),
            State2 = State1#state{
                       good = State1#state.good + 1,
                       stats = [Latency|State1#state.stats]
                      },
            wait(State2, N, M);
        failure ->      %% 当前 flashbot 已停止工作（进程结束）
            wait(State#state{bad = State#state.bad + 1}, N, M);
        {tcp, _, _} ->  %% 
            wait(State, N, M);
        {'DOWN', _, process, Pid, normal}   %% 发现 barrier 进程退出，即所有 flashbot 成功订阅到 janus
          when State#state.barrier == Pid ->
            %% barrier exited
            Delta = timer:now_diff(now(), State#state.start),
            % {Topic, Event, Data} = State#state.message,
            %% 进行消息发布 publish
            % bot:publish(Topic, Event, Data, 
            %             State#state.host, State#state.port),
            wait(State#state{setup = Delta}, N, M);
        X ->
            error_logger:error_report({unknown_message, X})
    end;

%% 全部统计信息输出（此时全部 flashbot 完成了订阅消息的接收）
wait(State, _, _) ->
    %% 全部 flashbot 完成订阅消息接收的时间差
    Delta = timer:now_diff(now(), State#state.start) / 1000,
    error_logger:info_msg("setup: ~.2.0fms, good: ~p, bad: ~p, run: ~.2.0fms~n",
                          [State#state.setup / 1000,
                           State#state.good,
                           State#state.bad,
                           Delta]),
    Histo = histo:build(State#state.max, State#state.stats),
    Len = length(State#state.stats),
    io:format("~10.4. fms | ~-6.. w~n", [State#state.min / 1000, min]),
    [io:format("~10.4. fms | ~-6.. w - ~6.2. f%~n", 
               [N / 1000, C, C / Len * 100]) || {N, C} <- Histo],
    io:format("~10.4. fms | ~-6.. w~n", [State#state.max / 1000, max]),
    ok.


%% 通过 bot 进行测试

test(Bot) ->
    test(Bot, 1, localhost, 8081).

%% bot:test(flashbot, 200).
test(Bot, N) ->
    test(Bot, N, localhost, 8081).

test(Bot, N, Host, Port) ->
  test(Bot, N, Host, Port, <<"no-topic-set">>).

test(Bot, N, Host, Port, Topic)
  when is_atom(Bot),
       is_integer(N),
       is_integer(Port),
       is_binary(Topic) ->
    %% [Note] 新建名为 lancher 的空进程组
    %% 这里有一个奇怪的点事，进程组创建是否成功，需要依靠 pg2:get_members
    %% 进行判定，而 pg2:create 只会返回 ok
    pg2:create(launcher),
    case pg2:get_members(launcher) of
        [] -> 
            launcher:start(true);
        {error, {no_such_group, launcher}} ->
            launcher:start(true);
        _ ->
            ok
    end,
    Event = <<"test_event">>,
    Data = <<"test">>,
    Message = {Topic, Event, Data},
    Expected = {struct, [{<<"topic">>, Topic},
                         {<<"event">>, Event},
                         {<<"message_id">>,<<>>},
                         {<<"data">>, Data}]},
    run(Bot, Host, Port, Message, Expected, N).

%% 更新最小、最大、平均延迟时间
update_latency(State, Latency) ->
    Min = 
        if 
            Latency < State#state.min ->
                Latency;
            true ->
                State#state.min
        end,
    Max = 
        if 
            Latency > State#state.max ->
                Latency;
            true ->
                State#state.max
        end,
    Avg = (State#state.avg + Latency) / 2,
    State#state{min = Min, max = Max, avg = Avg}.

%% 消息发布
publish(Topic, Event, Data, Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, 
                                                {nodelay, true},
                                                {active, once},
                                                {packet, 0}]),
    Data1 = {struct, [{<<"topic">>, Topic},
                      {<<"event">>, Event},
                      {<<"message_id">>,<<>>},
                      {<<"data">>, Data}]},
    Data2 = mochijson2:encode(Data1),
    ok = gen_tcp:send(Socket, [<<"<regular-socket/>">>, 0,
                               <<"PUBLISH">>, 0, Data2]),
    gen_tcp:close(Socket).

