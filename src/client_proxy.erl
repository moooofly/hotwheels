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

-module(client_proxy).
-behavior(gen_server).

-export([start/1, stop/1, locate/1, poll/1,
         attach/2, detach/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%%% Timeout between requests to pick up messages (poll)

-define(HEARTBEAT, 30000).

-record(state, {
          token,        %% 标识当前进程的随机十六进制字符串
          parent,       %% transport 进程 pid
          send,         %% fun(Bin) -> gen_tcp:send(Socket, [Bin, 1]) end
          heartbeat,    %% 心跳定时器 Ref
          killswitch,   %% 未使用
          messages      %% 缓存
         }).

%% 在 mapper 中查找 Token 对应的 pid
locate(Token) ->
    mapper:where(client_proxy_mapper, Token).

%% 提取当前缓存的所有消息（提取后清空原有消息缓存）
poll(Ref) ->
    gen_server:call(Ref, messages).

attach(Ref, Send) ->
    gen_server:cast(Ref, {attach, self(), Send}).

detach(Ref) ->
    gen_server:cast(Ref, {detach, self()}).

%% Send 定义为 fun(Bin) -> gen_tcp:send(Socket, [Bin, 1]) end
%% 该接口仅被 janus_flash 模块调用
start(Send) ->
    %% 生成随机十六进程字符串作为 token
    Token = common:random_token(),
    {ok, Pid} = gen_server:start_link(?MODULE, [Token, self(), Send], []),
    {ok, Pid, Token}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

%% Parent -> 为 transport 进程 pid
init([Token, Parent, Send]) ->
    process_flag(trap_exit, true),
    %% 将当前 client_proxy 进程 pid 以 token 作为标识保存到 ets 表中
    ok = mapper:add(client_proxy_mapper, Token),
    State = #state{
        token = Token,
        parent = Parent,
        send = Send,
        messages = []
     },
   {ok, State}.

%% 附着 
%%
%% Parent -> 收消息的进程
%% Send   -> 发送消息的 socket 和方式
handle_cast({attach, Parent, Send}, State) ->
    {noreply, State#state{parent = Parent, send = Send}};

%%
%% 分离
%%
handle_cast({detach, Who}, State) 
  when Who == State#state.parent ->
    %% transport is gone, session stays
    Send = fun(_) -> ok end,
    {noreply, State#state{parent = disconnected, send = Send}};

handle_cast({detach, _}, State) ->
    {noreply, State};

%% 处理来自 flashbot 的 subscribe ，消息来自 janus_flash:process
%% 将自身订阅到指定 Topic 上
handle_cast({<<"subscribe">>, Topic}, State) ->
    error_logger:info_msg("[client_proxy] handle_cast => subscribe Self(~p) to Topic(~p)~n", [self(), Topic]),
    topman:subscribe(self(), Topic),
    {noreply, State};

%% 处理来自 flashbot 的 unsubscribe ，消息来自 janus_flash:process
%% 将自身从指定 Topic 上取消订阅
handle_cast({<<"unsubscribe">>, Topic}, State) ->
    error_logger:info_msg("[client_proxy] handle_cast => unsubscribe Pid(~p) from Topic(~p)~n", [self(), Topic]),
    topman:unsubscribe(self(), Topic),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

%% 提取当前缓存的所有消息（提取后清空原有消息缓存）
handle_call(messages, _From, State) ->
    State1 = start_heartbeat(State),
    {reply, State1#state.messages, State1#state{messages = []}};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

%% 收到来自 pubsub 针对特定 Topic 的广播消息
handle_info({message, Msg}, State) 
  when is_pid(State#state.parent),
       is_binary(Msg) ->
    %% send immediately
    %% State#state.parent ! Event,
    error_logger:info_msg("[client_proxy] handle_info => recv {message, ~p} from pubsub, send to flashbot~n", [Msg]),
    %% 通过 socket 发布推送消息
    (State#state.send)(Msg),
    %% 启动 30s 定时器，当推送后的 30s 内没有新消息需要推送，则触发 PING 发送
    {noreply, start_heartbeat(State)};

%% 待发送消息缓存（原本用于 parent 不存在的情况）
handle_info({message, Msg}, State) ->
    %% buffer messages
    Messages1 = [Msg|State#state.messages],
    {noreply, State#state{messages = Messages1}};

handle_info(heartbeat, State) 
  when is_pid(State#state.parent) ->
    error_logger:info_msg("[client_proxy] handle_info => 30s elapse after last Msg, PING peer by transport(~p)~n", 
        [State#state.parent]),
    State#state.parent ! heartbeat,
    {noreply, State};

handle_info(heartbeat, State) ->
    %% no transport attached
    {noreply, State};

%% 收到来自 pubsub 的 topic 订阅成功应答
handle_info(ack, State) 
  when is_pid(State#state.parent) ->
    error_logger:info_msg("[client_proxy] handle_info => recv subsribe-ack from pubsub and ! to transport(~p)~n", [State#state.parent]),
    State#state.parent ! ack,
    {noreply, State};

handle_info(ack, State) ->
    %% no transport attached
    Messages1 = [<<"'ack'">>|State#state.messages],
    {noreply, State#state{messages = Messages1}};
    
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(Event, State) ->
    {stop, {unknown_info, Event}, State}.

terminate(_Reason, State) ->
    cancel_heartbeat(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cancel_heartbeat(State) ->
    catch erlang:cancel_timer(State#state.heartbeat),
    State#state{heartbeat = undefined}.

%% 更新 heartbeat 定时器
start_heartbeat(State) ->
    error_logger:info_msg("[client_proxy] start_heartbeat => reset heartbeat timer!~n"),
    Timer = erlang:send_after(?HEARTBEAT, self(), heartbeat),
    State1 = cancel_heartbeat(State),
    State1#state{heartbeat = Timer}.

