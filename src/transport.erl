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

-module(transport).
-behavior(gen_server).

-export([start_link/1, stop/1, set_socket/2]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          port,
          socket,       %% client socket
          transport,    %% 目前为 janus_flash
          state
         }).

%% janus_acceptor 通过此接口进行 socket 控制权转移的同步告知
%% 而该模块内部则通过异步 cast 消息进行实现，两者不矛盾
%% Ref -> 处理客户端连接的进程的 pid ，应该就是当前进程 pid
set_socket(Ref, Sock) ->
    gen_server:cast(Ref, {set_socket, Sock}).

start_link(Port) 
  when is_integer(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).

init([Port]) ->
    process_flag(trap_exit, true),
    {ok, #state{port = Port, transport = janus_flash }}.

%% [Note] 在对 socket 属性不做变更前，默认设置为
handle_cast({set_socket, Socket}, State) ->
    inet:setopts(Socket, [{active, once},
                          {packet, 0},
                          binary]),
    error_logger:info_msg("transport:handle_cast => recv {set_socket, Socket}, call janus_flash:start~n", []),
    %% 调用 janus_flash:start
    {ok, Keep, Ref} = (State#state.transport):start(Socket),
    keep_alive_or_close(Keep, State#state{socket = Socket, state = Ref});

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown, Event}, State}.

handle_call(Event, From, State) ->
    {stop, {unknown, Event, From}, State}.

%% 收到针对订阅 Topic 的广播消息，通过 socket 直接发送给客户端
handle_info({message, Msg}, State) ->
    Mod = State#state.transport,
    error_logger:info_msg("transport:handle_info => recv {message, ~p}, call janus_flash:forward~n", [Msg]),
    %% 调用 janus_flash:forward
    {ok, Keep, TS} = Mod:forward(Msg, State#state.state),
    keep_alive_or_close(Keep, State#state{state = TS});

handle_info({tcp_closed, Socket}, State) 
  when Socket == State#state.socket ->
    {stop, normal, State};

%% handle_info({tcp, Socket, Bin}, State)
%%   when Socket == State#state.socket ->
%%     inet:setopts(Socket, [{active, once}]),
%%     dispatch(Bin, publish, State);

%% 处理带有 "<regular-socket/>" 头的数据
handle_info({tcp, Socket, <<"<regular-socket/>", 0, Bin/binary>>}, State)
  when Socket == State#state.socket ->
    inet:setopts(Socket, [{active, once}]),
    error_logger:info_msg("transport:handle_info => recv Bin(~p), dispatch~n", [Bin]),
    dispatch(Bin, janus_flash, State);

handle_info({'EXIT', _, _}, State) ->
    %% ignore proxy exit
    {noreply, State};

%% 处理
%% 1.没有 "<regular-socket/>" 头的、来自 client socket 的数据
%% 2.来自 client_proxy 进程、通过 ! 发送的 heartbeat 和 ack 消息
handle_info(Info, State) 
  when State#state.transport /= undefined ->
    Mod = State#state.transport,
    %% 调用 janus_flash:process
    {ok, Keep, TS} = Mod:process(Info, State#state.state),
    keep_alive_or_close(Keep, State#state{state = TS});

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, State) 
  when State#state.transport /= undefined ->
    Mod = State#state.transport,
    Mod:stop(State#state.state),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Brand new transport

%%% Existing connection 

%% [Note] 设计意图 采用不同 Mod 处理不同数据
dispatch(Data, Mod, State = #state{transport = Mod}) ->
    {ok, Keep, TS} = Mod:process(Data, State#state.state),
    keep_alive_or_close(Keep, State#state{state = TS}).

%% 判定当前 socket 连接是否需要保持，对应当前 gen_server 进程是否要保持
keep_alive_or_close(Keep, State) ->
    if 
        Keep /= keep_alive ->
            gen_tcp:close(State#state.socket),
            {stop, normal, State};
        true ->
            {noreply, State}
    end.
    
    
