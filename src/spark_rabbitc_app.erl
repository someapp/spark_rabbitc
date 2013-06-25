-module(spark_rabbitc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    spark_app_config_sup:start_link(),
    spark_rabbitc_sup:start_link().

stop(_State) ->
    ok.
