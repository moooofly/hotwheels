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


-module(flashbot2).
-behavior(gen_fsm).

-export([start/1]).

-export([not_connected/2, no_token/2, 
         not_subscribed/2, subscribed/2]).

-export([init/1, handle_event/3, handle_sync_event/4, 
         handle_info/3, terminate/3, code_change/4]).

-record(state, {
          parent,
          host,
          port,
          token,
          start,
          timer,
          barrier,
          expected,
          topic,
          socket,
          data,
          latency
         }).

start(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

init([Parent, Host, Port, Expected, Barrier]) ->
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
            State#state.parent ! connected,
            ping(State#state{socket = Sock}, no_token);
        _ ->
            reconnect(),
            {next_state, not_connected, State}
    end.

no_token({struct, [{<<"token">>, Token}]}, #state{topic=Topic}=State) ->
    State#state.parent ! subscribing,
    JSON = mochijson2:encode({struct, [{action, <<"subscribe">>},
                                       {topic, Topic}
                                      ]}),
    Data = [<<"<regular-socket/>">>, 0, JSON],
    send(Data, State#state{token = Token}, not_subscribed).

not_subscribed(ack, State) ->
    error_logger:info_msg("[flashbot2] recv ACK to Subscribe.~n", []),
    barrier:bump(State#state.barrier),
    {next_state, subscribed, State#state{start = now()}}.

subscribed(token_timeout, State) ->
    {next_state, subscribed, State};

subscribed(ready, State) ->
    % Ref = gen_fsm:send_event_after(5000, timeout),
    % {next_state, subscribed, State#state{start = now(), timer = Ref}};
    {next_state, subscribed, State#state{start = now()}};

subscribed(timeout, State) ->
    State#state.parent ! failure,
    {stop, timeout, State};

subscribed(Expected, State) ->
    State#state.parent ! {success, State#state.latency},
    error_logger:info_msg("[flashbot2] recv PUBLISH Msg =>~n~p~n", [Expected]),
    {next_state, subscribed, State}.

handle_event(Event, Where, State) ->
    {stop, {unknown_event, Event, Where}, State}.

handle_sync_event(Event, From, Where, State) ->
    {stop, {unknown_sync_event, Event, From, Where}, State}.

handle_info({'DOWN', _, process, Pid, normal}, Where, State)
  when Pid == State#state.barrier ->
    error_logger:info_msg("[flashbot2] recv 'DOWN' from barrier, ready for PUBLISH Msg.~n", []),
    ?MODULE:Where(ready, State);

handle_info({tcp, Sock, <<"PING", 1>>}, Where, State) ->
    error_logger:info_msg("[flashbot2] recv PING, send PONG.~n", []),
    inet:setopts(Sock, [{active, once}]),
    send(<<"PONG">>, State, Where);

handle_info({tcp, Sock, Bin}, Where, State) 
  when State#state.data /= undefined ->
    inet:setopts(Sock, [{active, once}]),
    Bin1 = list_to_binary([State#state.data, Bin]),
    State1 = State#state{data = undefined},
    ?MODULE:handle_info({tcp, Sock, Bin1}, Where, State1);

handle_info({tcp, Sock, <<"ACK", 1>>}, Where, State) ->
    inet:setopts(Sock, [{active, once}]),
    ?MODULE:Where(ack, State);

handle_info({tcp, Sock, Bin}, Where, State) ->
    inet:setopts(Sock, [{active, once}]),
    case bin:split("\\001", Bin) of
        {more, Bin} ->
            {next_state, Where, State#state{data = Bin}};
        {ok, <<>>, <<>>} ->
            {next_state, Where, State#state{data = Bin}};
        {error, Error} ->
            {stop, {packet_split, Error}, State};
        {ok, Bin1, Rest} ->
            Now = now(),
            JSON = mochijson2:decode(Bin1),
            %% grab the timestamp
            {struct, [{<<"timestamp">>, TS}|L]} = JSON,
            Delta = timer:now_diff(Now, list_to_tuple(TS)),
            State1 = State#state{latency = Delta, data = undefined},
            case ?MODULE:Where({struct, L}, State1) of 
                {next_state, Where1, State2} ->
                    ?MODULE:handle_info({tcp, Sock, Rest}, Where1, State2);
                Other ->
                    Other
            end
    end;

handle_info(X, _, State) 
  when element(1, X) == tcp_closed;
       element(1, X) == tcp_error ->
    State#state.parent ! disconnected,
    % catch gen_fsm:cancel_timer(State#state.timer),
    reconnect(),
    {next_state, not_connected, State#state{timer = none}};

handle_info(Info, Where, State) ->
    {stop, {unknown_info, Info, Where}, State}.

terminate(_Reason, _Where, State) ->
    catch gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVsn, Where, State, _Extra) ->
    {ok, Where, State}.

send(Bin, State, Where) -> 
    case gen_tcp:send(State#state.socket, [Bin, 0]) of
        ok ->
            {next_state, Where, State};
        _ ->
            reconnect(),
            {next_state, not_connected, State}
    end.

ping(State, Where) ->
    Data = [<<"<regular-socket/>", 0>>, <<"PING">>],
    send(Data, State, Where).

reconnect() ->
    flush(),
    gen_fsm:send_event_after(random:uniform(100), connect).

flush() ->
    receive 
        _ ->
            flush()
    after 0 ->
            ok
    end.
