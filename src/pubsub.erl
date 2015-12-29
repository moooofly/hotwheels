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

-module(pubsub).

-export([publish/2, subscribe/2, unsubscribe/2,
         start/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          topic,
          subs
         }).

publish(Ref, Msg) ->
    gen_server:cast(Ref, {publish, Msg}).

subscribe(Ref, Pid) ->
    gen_server:cast(Ref, {subscribe, Pid}).

unsubscribe(Ref, Pid) ->
    gen_server:cast(Ref, {unsubscribe, Pid}).

start(Topic) ->
    gen_server:start_link(?MODULE, [Topic], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).


init([Topic]) when is_binary(Topic) ->
    process_flag(trap_exit, true),
    {ok, #state{topic=Topic, subs=ets:new(binary_to_atom(Topic, latin1) , [named_table, set])}};

init([Topic]) when is_atom(Topic) ->
    process_flag(trap_exit, true),
    {ok, #state{topic=Topic, subs=ets:new(Topic, [named_table, set])}}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({subscribe, Pid}, State=#state{topic=Topic}) ->
    %% automatically unsubscribe when dead
    Ref = erlang:monitor(process, Pid),
    lager:debug("[pubsub] handle_cast => recv {subscribe,...} from client_proxy(~p) to Topic(~p), ack.", 
        [Pid, State#state.topic]),
    Pid ! ack,
    ets:insert(State#state.subs, {{Topic, Pid}, Ref}),
    {noreply, State};

handle_cast({unsubscribe, Pid}, State) ->
    lager:debug("[pubsub] handle_cast => recv {unsubscribe,...} from client_proxy(~p), ack", [Pid]),
    unsubscribe1(Pid, State);

handle_cast({publish, Msg}, State) ->
    %%io:format("[pubsub] handle_cast => recv {publish, Msg}~nets:info(subs): ~p~n", [ets:info(State#state.subs)]),
    lager:debug("[pubsub] handle_cast => recv {publish, ~p}", [Msg]),

    Start = now(),
    {struct, L} = Msg,
    TS = tuple_to_list(Start),
    JSON = {struct, [{<<"timestamp">>, TS}|L]},
    Msg1 = {message, iolist_to_binary(mochijson2:encode(JSON))},
    F = fun({{_Topic, Pid}, _Ref}, _) -> Pid ! Msg1 end,
    erlang:process_flag(priority, high),
    ets:foldr(F, ignore, State#state.subs),
    End = now(),
    erlang:process_flag(priority, normal),

    %%io:format("time: ~p~n", [timer:now_diff(End, Start) / 1000]),
    lager:debug("time: ~p~n", [timer:now_diff(End, Start) / 1000]),

    {noreply, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    lager:notice("[pubsub] handle_cast => recv {'DOWN',...} from client_proxy(~p)", [Pid]),
    unsubscribe1(Pid, State);

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

unsubscribe1(Pid, State=#state{topic=Topic}) ->
    Pid ! ack,
    case ets:lookup(State#state.subs, {Topic,Pid}) of
        [{{Topic,_Pid}, Ref}] ->
            erlang:demonitor(Ref),
            ets:delete(State#state.subs, {Topic,Pid});
        _ ->
            ok
    end,
    {noreply, State}.
