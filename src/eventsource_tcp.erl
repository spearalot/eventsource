-module(eventsource_tcp).
-author("Martin Carlson <spearalot@gmail.com>").
-export([start_link/2]).
-export([init/4]).
-export([
    system_continue/3,
    system_terminate/4,
    write_debug/3,
    system_get_state/1, 
    system_replace_state/2
]).

-record(st, {
    socket,
    acceptor,
    handler
}).

-define(tcp, gen_tcp).
-define(tcp_options(HOST), [binary, {packet, 2}, {active, true}, {ip, HOST}]).

%%% ============================================================================ 
%%% API
%%% ============================================================================ 

start_link(Hostname, Port) ->
    start_link(Hostname, Port, fun(Socket) -> eventsource_srv_erl:start(Socket) end).

%% @doc Starts the tcp listener.
start_link(Hostname, Port, Handler) ->
    proc_lib:start_link(?MODULE, init, [self(), Hostname, Port, Handler]).

%%% ============================================================================ 
%%% Server
%%% ============================================================================ 
init(Parent, Hostname, Port, Handler) ->
    Debug = sys:debug_options([]),
    case ?tcp:listen(Port, ?tcp_options(Hostname)) of
        {ok, Sock} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(Parent, Debug, #st{
                socket = Sock,
                acceptor = spawn_acceptor(Sock, Handler), 
                handler = Handler
            });
        {error, Reason} ->
            proc_lib:init_ack(Parent, {error, Reason})
    end.

loop(Parent, Debug, #st{socket = Socket, handler = Handler, acceptor = {Pid, Ref}} = St) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, St);
        {accepted, Pid} ->
            erlang:demonitor(Ref),
            loop(Parent, Debug, St#st{acceptor = spawn_acceptor(Socket, Handler)});
        {'DOWN', Ref, process, Pid, _Reason} ->
            loop(Parent, Debug, St#st{acceptor = spawn_acceptor(Socket, Handler)});
        _ ->
            loop(Parent, Debug, St)
    end.
    
%%% ============================================================================ 
%%% Sys
%%% ============================================================================ 
system_continue(Parent, Debug, St) ->
    loop(Parent, Debug, St).

system_terminate(Reason, _Parent, _Debug, _St) ->
    exit(Reason).

system_get_state(St) ->
    {ok, St}.

system_replace_state(Transform, St) ->
    St1 = Transform(St),
    {ok, St1, St1}.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

%%% ============================================================================ 
%%% Internal
%%% ============================================================================ 
spawn_acceptor(Socket, Handler) ->
    Parent = self(),
    spawn_monitor(fun() -> accept(Parent, Socket, Handler) end).

accept(Parent, Socket, Handler) ->
    case ?tcp:accept(Socket) of
        {ok, Client} ->
            Parent ! {accepted, self()},
            Handler(Client);
        {error, Reason} ->
            exit(Reason)
    end.
