-module(janus_flash).

-export([process/2, forward/2, start/1, stop/1]).

-record(state, {
          socket,
          data,
          proxy,  %% client_proxy 进程 pid
          token   %% 标识 client_proxy 进程的随机十六进制字符串
         }).

start(Socket) ->
    %% [Node] [Bin, 1] 的设计意图
    Send = fun(Bin) -> gen_tcp:send(Socket, [Bin, 1]) end,
    {ok, Proxy, Token} = client_proxy:start(Send),
    State = #state{
        socket = Socket, 
        proxy = Proxy, 
        token = Token
     },
    JSON = {struct,
            [{<<"timestamp">>, tuple_to_list(now())},
             {<<"token">>, Token}
            ]},
    send(mochijson2:encode(JSON), State).

stop(State) ->
    catch client_proxy:detach(State#state.proxy),
    ok.

forward(Bin, State)
  when is_binary(Bin) ->
    send(Bin, State).

process(heartbeat, State) ->
    send(<<"PING">>, State);

process(ack, State) ->
    send(<<"ACK">>, State);

process(<<>>, State) ->
    {ok, keep_alive, State};

process(Bin, State) 
  when is_binary(State#state.data),
       is_binary(Bin) ->
    process(list_to_binary([State#state.data, Bin]),
            State#state{data = undefined});

process(<<"<regular-socket/>", 0, Bin/binary>>, State) ->
    process(Bin, State);

process(Bin, State) 
  when is_binary(Bin) ->
    process(bin:split("\\000", Bin), State);

process({more, Bin}, State) ->
    {ok, keep_alive, State#state{data = Bin}};

process({ok, <<>>, <<>>}, State) ->
    {ok, keep_alive, State};

process({ok, <<>>, Rest}, State) ->
    process(Rest, State);

process({ok, <<"PING">>, Rest}, State) ->
    process(Rest, State);

process({ok, <<"PONG">>, Rest}, State) ->
    process(Rest, State);

process({ok, <<"PUBLISH">>, Rest}, State) ->
    JSON = {struct, [{<<"topic">>, Topic},
                     {<<"event">>, _},
                     {<<"message_id">>, _},
                     {<<"data">>, _}
                    ]} = mochijson2:decode(Rest),
    topman:publish(JSON, Topic),
    {ok, shutdown, State};

process({ok, Bin, Rest}, State) ->
    {struct,
     [{<<"action">>, Action}, 
      {<<"data">>, Topic}
     ]} = mochijson2:decode(Bin),
    gen_server:cast(State#state.proxy, {Action, Topic}),
    process(Rest, State).

send(Data, State) ->
    Keep = case gen_tcp:send(State#state.socket, [Data, 1]) of
               ok ->
                   keep_alive;
               _ ->
                   shutdown
           end,
    {ok, Keep, State}.

