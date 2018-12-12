-module(eventsource_srv_erl).
-export([start/1]).
-author("Martin Carlson <spearalot@gmail.com>").

%%% ============================================================================ 
%%% API
%%% ============================================================================ 

%% Starts the client handler.
start(Socket) ->
    spawn(fun() -> init(Socket) end).

%%% ============================================================================ 
%%% Server
%%% ============================================================================ 
init(Socket) ->
    {ok, Ref} = eventsource:subscribe(self()),
    loop(Socket, Ref).

loop(Socket, Ref) ->
    receive
        {tcp_closed, Socket} ->
            eventsource:unsubscribe(Ref);
        {tcp, Socket, {data, Term}} ->
            handle_request(Ref, binary_to_term(Term)),
            loop(Socket, Ref);
        {event, Ref, Event} ->
            gen_tcp:send(Socket, Event),
            loop(Socket, Ref);
        _ ->
            loop(Socket, Ref)
    end.

handle_request(Ref, {event, Event}) ->
    eventsource:publish(Ref, Event);
handle_request(_, _) ->
    ok.
