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

%%
%% 维护特定 topic 的进程
%%
%% 1. 通过 ets 表维护订阅指定 topic 的进程集合
%% 2. 对所有订阅到指定 topic（当前 pubsub 进程维护）的 client_proxy 进程进行 monitor
%%

-module(pubsub).

-export([publish/2, subscribe/2, unsubscribe/2,
         start/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          topic,                        %% 当前 pubsub 进程负责处理的 topic
          subs = ets:new(subs, [set])   %% {Key, Value} -> {Pid, Ref}
         }).                            %% Pid -> 订阅当前 topic 的 client_proxy 进程 pid
                                        %% Ref -> monitor(Pid)

%% 发布（广播消息给所有订阅者）
publish(Ref, Msg) ->
    gen_server:cast(Ref, {publish, Msg}).

%% 订阅
%% Ref -> 维护特定 Topic 的 pubsub 进程 pid
%% Pid -> client_proxy 进程 pid
subscribe(Ref, Pid) ->
    gen_server:cast(Ref, {subscribe, Pid}).

%% 取消订阅
unsubscribe(Ref, Pid) ->
    gen_server:cast(Ref, {unsubscribe, Pid}).

start(Topic) ->
    gen_server:start_link(?MODULE, [Topic], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).

init([Topic]) ->
    process_flag(trap_exit, true),
    {ok, #state{topic = Topic}}.

handle_cast(stop, State) ->
    {stop, normal, State};

%% Pid -> 订阅当前 topic 的进程 pid (client_proxy)
handle_cast({subscribe, Pid}, State) ->
    %% automatically unsubscribe when dead
    Ref = erlang:monitor(process, Pid),
    error_logger:info_msg("[pubsub] handle_cast => recv {subscribe, ~p} to Topic(~p) and ! to client_proxy(~p) ack~n", 
        [Pid, State#state.topic, Pid]),
    %% 告知订阅成功
    Pid ! ack,
    ets:insert(State#state.subs, {Pid, Ref}),
    {noreply, State};

handle_cast({unsubscribe, Pid}, State) ->
    unsubscribe1(Pid, State);

handle_cast({publish, Msg}, State) ->
    %% 这里通过 io:format/2 输出打印，而没有通过 error_logger:xxx 输出，应该是因为速度问题
    %%io:format("[pubsub] handle_cast => recv {publish, Msg}~nets:info(subs): ~p~n", [ets:info(State#state.subs)]),
    %%error_logger:info_msg("[pubsub] handle_cast => recv {publish, Msg}~nets:info(subs): ~p~n", [ets:info(State#state.subs)]),

    error_logger:info_msg("[pubsub] handle_cast => recv {publish, ~p}~n", [Msg]),

    %% 为 Msg 内容添加时间戳
    Start = now(),
    {struct, L} = Msg,
    TS = tuple_to_list(Start),
    JSON = {struct, [{<<"timestamp">>, TS}|L]},
    Msg1 = {message, iolist_to_binary(mochijson2:encode(JSON))},
    %% 广播给所有订阅当前 topic 的 client_proxy 进程（内部临时调高进程优先级）
    F = fun({Pid, _Ref}, _) -> Pid ! Msg1 end,
    %% [Note]
    erlang:process_flag(priority, high),
    ets:foldr(F, ignore, State#state.subs),
    %%End = now(),
    erlang:process_flag(priority, normal),
    %%io:format("time: ~p~n", [timer:now_diff(End, Start) / 1000]),
    %%error_logger:info_msg("time: ~p~n", [timer:now_diff(End, Start) / 1000]),
    {noreply, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    unsubscribe1(Pid, State);

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

unsubscribe1(Pid, State) ->
    case ets:lookup(State#state.subs, Pid) of
        [{_, Ref, _}] ->
            erlang:demonitor(Ref),
            ets:delete(State#state.subs, Pid);
        _ ->
            ok
    end,
    {noreply, State}.
