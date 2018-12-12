-module(eventsource).
-export([subscribe/1, unsubscribe/1, publish/2]).

%%% ============================================================================ 
%%% API
%%% ============================================================================ 
subscribe(Pid) ->
    gen_server:call(eventsource, {subscribe, Pid}).

unsubscribe(Ref) ->
    gen_server:call(eventsource, {unsubscribe, Ref}).

publish(Ref, Event) ->
    gen_server:call(eventsource, {publish, Ref, Event}).
