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

-module(bot).

-export([run/6, test/1, test/2, test/4, publish/5]).

-include_lib("kernel/include/inet.hrl").

-record(state, {
          bot,          %% 目前为 flashbot
          barrier,      %% 目前用作 counter
          args,
          n,            %% 运行 bot 的数目
          good, 
          bad,
          min,
          max, 
          avg,
          host,         %% 目的 ip 地址
          port,         %% 目的端口
          message,      %% 待发送消息内容
          expected,     %% 期望接收的内容
          start,        %% 开始时间
          launchers,
          where,
          setup,
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
    erlang:monitor(process, State#state.barrier),
    wait(State, 0, 0);

run(State, N) ->
    %% 从 launcher 进程组中得到一个成员进程 Srv（lancher 进程）
    {Srv, L} = launcher:next(State#state.launchers),
    {ok, Pid} = launcher:launch(Srv, State#state.bot, 
                                [self(),
                                 State#state.host,
                                 State#state.port,
                                 State#state.expected,
                                 State#state.barrier]),
    unlink(Pid),
    Where = State#state.where,
    State1 = State#state{where = dict:update_counter(Srv, 1, Where)},
    run(State1#state{launchers = L}, N - 1).

wait(State, N, M) 
  when State#state.good + State#state.bad < State#state.n ->
    receive
        connected ->    %% flashbot 与 janus 成功建立 TCP 连接
            wait(State, N + 1, M);
        disconnected ->
            wait(State, N - 1, M);
        subscribing ->
            wait(State, N, M + 1);
        {success, Latency} ->
            State1 = update_latency(State, Latency),
            State2 = State1#state{
                       good = State1#state.good + 1,
                       stats = [Latency|State1#state.stats]
                      },
            wait(State2, N, M);
        failure ->
            wait(State#state{bad = State#state.bad + 1}, N, M);
        {tcp, _, _} ->
            wait(State, N, M);
        {'DOWN', _, process, Pid, normal}
        when State#state.barrier == Pid ->
            %% barrier exited
            Delta = timer:now_diff(now(), State#state.start),
            {Topic, Event, Data} = State#state.message,
            bot:publish(Topic, Event, Data, 
                        State#state.host, State#state.port),
            wait(State#state{setup = Delta}, N, M);
        X ->
            error_logger:error_report({unknown_message, X})
    end;

wait(State, _, _) ->
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

test(Bot, N, Host, Port) 
  when is_atom(Bot),
       is_integer(N),
       is_integer(Port) ->
    %% [Note]
    pg2:create(launcher),
    case pg2:get_members(launcher) of
        [] -> 
            launcher:start(true);
        {error, {no_such_group, launcher}} ->
            launcher:start(true);
        _ ->
            ok
    end,
    Topic = <<"events">>,
    Event = <<"test_event">>,
    Data = <<"test">>,
    Message = {Topic, Event, Data},
    Expected = {struct, [{<<"topic">>, Topic},
                         {<<"event">>, Event},
                         {<<"message_id">>,<<>>},
                         {<<"data">>, Data}]},
    run(Bot, Host, Port, Message, Expected, N).

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

