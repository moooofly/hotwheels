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
%% 此模块实现要点：
%% 1. proc_lib:start_link + proc_lib:init_ack
%% 2. 针对 gen_tcp:F 均使用了 catch
%% 3. 为每个连接上的 client 在 janus_transport_sup 下动态创建一个 transport 进程进行处理
%%



-module(janus_acceptor).

-export([start_link/3]).

-export([acceptor_init/3, acceptor_loop/1]).

-record(state, {
          parent,
          module,  % Handling module 目前为 transport
          port,
          listener % Listening socket
         }).

%% Parent -> janus_app pid
%% Module -> transport 模块
start_link(Parent, Port, Module) 
  when is_pid(Parent),
       is_list(Port), 
       is_atom(Module) ->
    start_link(Parent, list_to_integer(Port), Module);

start_link(Parent, Port, Module) 
  when is_pid(Parent),
       is_integer(Port), 
       is_atom(Module) ->
    Args = [Parent, Port, Module],
    %% [Note]
    proc_lib:start_link(?MODULE, acceptor_init, Args).

acceptor_init(Parent, Port, Module) ->
    State = #state{ 
      parent = Parent,
      port = Port,
      module = Module
     },
    error_logger:info_msg("Listening on port ~p~n", [Port]),
    case (catch do_init(State)) of
        {ok, ListenSocket} ->
            %% [Note] 用法 + 位置
            proc_lib:init_ack(State#state.parent, {ok, self()}),
            acceptor_loop(State#state{listener = ListenSocket});
        Error ->
            proc_lib:init_ack(Parent, Error),
            error
    end.
   
do_init(State) ->
    Opts = [binary, 
            {packet, 0}, 
            {reuseaddr, true},
            {backlog, 1024},
            {active, false}],
    case gen_tcp:listen(State#state.port, Opts) of
        {ok, ListenSocket} ->
            {ok, ListenSocket};
        {error, Reason} ->
            throw({error, {listen, Reason}})
    end.

acceptor_loop(State) ->
    case (catch gen_tcp:accept(State#state.listener, 50000)) of
        {ok, Socket} ->
            handle_connection(State, Socket),
            ?MODULE:acceptor_loop(State);
        {error, Reason} ->
            handle_error(Reason),
            ?MODULE:acceptor_loop(State);
        {'EXIT', Reason} ->
            handle_error({'EXIT', Reason}),
            ?MODULE:acceptor_loop(State)
    end.


handle_connection(State, Socket) ->
    %% 启动 用于处理客户端连接 的进程
    {ok, Pid} = janus_app:start_transport(State#state.port),
    ok = gen_tcp:controlling_process(Socket, Pid),
    %% Instruct the new handler to own the socket.
    %% 同步控制
    (State#state.module):set_socket(Pid, Socket).

handle_error(timeout) ->
    ok;

handle_error({enfile, _}) ->
    %% Out of sockets...
    sleep(200);

handle_error(emfile) ->
    %% Too many open files -> Out of sockets...
    sleep(200);

%% 这点说明有点意思
handle_error(closed) ->
    error_logger:info_report("The accept socket was closed by " 
			     "a third party. "
			     "This will not have an impact on janus "
			     "that will open a new accept socket and " 
			     "go on as nothing happened. It does however "
			     "indicate that some other software is behaving "
			     "badly."),
    exit(normal);

%% This will only happen when the client is terminated abnormaly
%% and is not a problem for the server, so we want
%% to terminate normal so that we can restart without any 
%% error messages.

handle_error(econnreset) ->
    exit(normal);

handle_error(econnaborted) ->
    ok;

handle_error({'EXIT', Reason}) ->
    String = lists:flatten(io_lib:format("Accept exit: ~p", [Reason])),
    accept_failed(String);

handle_error(Reason) ->
    String = lists:flatten(io_lib:format("Accept error: ~p", [Reason])),
    accept_failed(String).

accept_failed(String) ->
    error_logger:error_report(String),
    exit({accept_failed, String}).    

sleep(T) -> receive after T -> ok end.


