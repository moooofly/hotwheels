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

%%%
%%% Topic manager
%%% 

-module(topman).

-export([publish/2, subscribe/2, unsubscribe/2,
         start/0, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          topic_xref,       %% {key, value} -> {Topic, PubSubPid}
          server_xref       %% {key, value} -> {PubSubPid, {Topic, monitor(PubSubPid)}}
         }).

%% 只有 publish 采用广播
%% [Note] 这里采用 abcast 的深层原因
%% 参考：http://stackoverflow.com/questions/31541856/confusion-regarding-abcast-function-and-uniqueness-of-gen-server-names
publish(Msg, Topic) 
  when is_binary(Topic) ->
    gen_server:abcast(?MODULE, {publish, Msg, Topic});

publish(Msg, Topic) 
  when is_list(Topic) ->
    publish(Msg, list_to_binary(Topic)).

%% Pid -> 发起订阅的进程，即 client_proxy 进程 pid
subscribe(Pid, Topic) 
  when is_binary(Topic) ->
    gen_server:cast(?MODULE, {subscribe, Pid, Topic});

subscribe(Pid, Topic)
  when is_list(Topic) ->
    subscribe(Pid, list_to_binary(Topic)).

unsubscribe(Pid, Topic)
  when is_binary(Topic) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid, Topic});

unsubscribe(Pid, Topic) 
  when is_list(Topic) ->
    unsubscribe(Pid, list_to_binary(Topic)).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).

init([]) ->
    process_flag(trap_exit, true),
    State = #state{
        topic_xref = dict:new(), 
        server_xref = dict:new()
     },
    {ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

%% Pid -> client_proxy 进程 pid
handle_cast({subscribe, Pid, Topic}, State) ->
    %% Srv -> 与 Topic 关联的 pubsub 进程 pid
    {Srv, State1} = ensure_server(Topic, State),
    %% 将 client_proxy 进程 pid “注册”到与 Topic 关联的 pubsub 进程中
    pubsub:subscribe(Srv, Pid),
    {noreply, State1};

handle_cast({unsubscribe, Pid, Topic}, State) ->
    case dict:find(Topic, State#state.topic_xref) of
        {ok, Srv} ->
            %% found a topic server
            pubsub:unsubscribe(Srv, Pid);
        _ ->
            ignore
    end,
    {noreply, State};

handle_cast({publish, Msg, Topic}, State) ->
    {Srv, State1} = ensure_server(Topic, State),
    pubsub:publish(Srv, Msg),
    {noreply, State1};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    %% topic manager died, we don't care why
    case dict:find(Pid, State#state.server_xref) of
        {ok, {Topic, Ref}} ->
            erlang:demonitor(Ref),
            Xref1 = dict:erase(Topic, State#state.topic_xref),
            Xref2 = dict:erase(Pid, State#state.server_xref),
            State1 = State#state{topic_xref = Xref1, server_xref = Xref2};
        _ ->
            State1 = State
    end,
    {noreply, State1};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 找到与指定 Topic 关联的 pubsub 进程 pid
%% 该函数会确保有 pubsub 进程与指定 Topic 关联
%% 并返回与指定 Topic 关联的 pubsub 进程 pid
ensure_server(Topic, State) ->
    Xref = State#state.topic_xref,
    case dict:find(Topic, Xref) of
        {ok, Srv} ->
            %% found a topic server
            Xref1 = Xref,
            Xref2 = State#state.server_xref;
        _ ->
            %% start a new topic server
            %% 启动与指定 Topic 关联的 pubsub 进程
            {ok, Srv} = pubsub:start(Topic),
            %% 对 pubsub 进程进行 monitor
            Ref = erlang:monitor(process, Srv),
            %% store(Key, Value, Dict1) -> Dict2
            %% 以键值（Key - Value）对的形式存储在字典里。
            %% 如果字典里已经存在 Key 的键，则把跟 Key 相关的值替换为 Value
            Xref1 = dict:store(Topic, Srv, Xref),
            Xref2 = dict:store(Srv, {Topic, Ref}, State#state.server_xref)
    end,
    State1 = State#state{topic_xref = Xref1, server_xref = Xref2},
    {Srv, State1}.
