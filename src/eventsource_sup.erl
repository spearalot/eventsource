%%%-------------------------------------------------------------------
%% @doc eventsource top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(eventsource_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(Backing, Hostname, Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Backing, Hostname, Port]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Backing, Hostname, Port]) ->
    Source = #{id => source, start => {Backing, start_link, []}},
    Server = #{id => tcp_server, start => {eventsource_tcp, start_link, [Hostname, Port]}},
    {ok, {#{}, [Source, Server]}}.

%%====================================================================
%% Internal functions
%%====================================================================
