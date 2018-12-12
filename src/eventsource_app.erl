%%%-------------------------------------------------------------------
%% @doc eventsource public API
%% @end
%%%-------------------------------------------------------------------
-module(eventsource_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    eventsource_sup:start_link(get_env(backing_mod), get_env(hostname), get_env(port)).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
get_env(Key) ->
    case application:get_env(eventsource, Key) of
        undefined -> exit({env, Key});
        {ok, Val} -> Val
    end.
