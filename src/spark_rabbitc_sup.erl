
-module(spark_rabbitc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(DEFAULT_RESTART,{one_for_one, 5, 10}).
-define(SPAWN_OPTS, {fullsweep_after, 60}).
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [{spawn_opts, ?SPAWN_OPTS}]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [
	?CHILD(spark_rabbitc_srv,worker)
		
	],
    {ok, { ?DEFAULT_RESTART, Children} }.

