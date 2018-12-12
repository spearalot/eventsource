-module(eventsource_mem).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-record(st, {n, events, subscribers}).

%%% ============================================================================ 
%%% API
%%% ============================================================================ 
start_link() ->
    gen_server:start_link({local, eventsource}, ?MODULE, [], []).

%%% ============================================================================ 
%%% Server
%%% ============================================================================ 
init([]) ->
    {ok, #st{events = [], subscribers = #{}}}.

terminate(_St, _Reason) ->
    ok.

handle_call({subscribe, Pid}, _From, #st{events = Es, subscribers = S} = St) ->
    Ref = erlang:monitor(process, Pid),
    send_history(Pid, Ref, Es),
    {reply, {ok, Ref}, St#st{subscribers = maps:put(Ref, Pid, S)}};
handle_call({unsubscribe, Ref}, _From, #st{subscribers = S} = St) ->
    erlang:demonitor(Ref),
    {reply, {ok, Ref}, St#st{subscribers = maps:remove(Ref, S)}};
handle_call({publish, _Ref, Event}, _From, #st{events = Es, subscribers = S} = St) ->
    send_event(S, Event),
    {reply, ok, St#st{events = [Event | Es]}};
handle_call(_, _From, St) ->
    {reply, ok, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

code_change(_, _, St) ->
    {ok, St}.


%%% ============================================================================ 
%%% Internal
%%% ============================================================================ 
send_history(Pid, Ref, Events) ->
    spawn(fun() -> lists:foreach(fun(E) -> Pid ! {event, Ref, E} end, lists:reverse(Events)) end).

send_event(Subs, Event) ->
    lists:foreach(fun({R, P}) -> P ! {event, R, Event} end, maps:to_list(Subs)).
