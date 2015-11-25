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

%%% Distributed test launcher
%% 分布式测试发起器

-module(launcher).
-behaviour(gen_server).

-export([start/1, stop/1, launch/2, launch/3, next/0, next/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          trace = false     %% 用于表示是否启动 trace 功能的布尔值
         }).

new(Trace) ->
    #state{
     trace = Trace
    }.

%% Trace -> true | false
start(Trace) ->
    gen_server:start_link(?MODULE, [Trace], []).

%% 随便找个 lancher 进程启动 flashbot
launch(Mod, Args) ->
    {ok, Pid} = util:get_random_pid(?MODULE),
    launch(Pid, Mod, Args).

%% 
%% 在指定 lancher 进程上（Pid）启动 flashbot 
%% Pid -> 从 lancher 进程组中找到的成员进程 pid
%% Mod -> flashbot
%% 
launch(Pid, Mod, Args) ->
    gen_server:call(Pid, {launch, Mod, Args}).


%% 从 launcher 进程组中获取一个成员进程
next() ->
    next([]).

next(undefined) ->
    next([]);

next([]) ->
    %% 获取进程组 launcher 中的所有进程
    case pg2:get_members(?MODULE) of
        [] ->
            timer:sleep(100),
            next([]);
        L ->
            next(L)
    end;

next([H|T]) ->
    {H, T}.

init([Trace]) ->
    process_flag(trap_exit, true),
    %% 新建名为 launcher 的进程组，并将当前 launcher 进程添加到该进程组中
    pg2:create(?MODULE),
    ok = pg2:join(?MODULE, self()),
    {ok, new(Trace)}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, _State) ->
    ok.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call({launch, Mod, Args}, _From, State) ->
    %% 启动 flashbot
    Reply = Mod:start(Args),
    {reply, Reply, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'DOWN', _, process, _, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', _, _}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

