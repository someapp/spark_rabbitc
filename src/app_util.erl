-module(app_util).

-export([start_app/1,stop_app/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


start_app(ok)-> ok;
start_app({error, {already_started, App}})
		when is_atom(App) -> ok;
start_app({error, {Reason, App}}) 
		when is_atom(App) ->
	{error, {Reason, App}};
start_app({E, {Reason, App}}) ->
	{E, {Reason, App}};
start_app(_)-> {error, badarg}.

stop_app(ok)-> ok;
stop_app({error,{not_started,App}})
		when is_atom(App)-> ok;
stop_app({error, {Reason, App}}) 
		when is_atom(App) ->
	{error, {Reason, App}};
stop_app({E, {Reason, App}})-> 
	{E, {Reason, App}};
stop_app(_)-> {error, badarg}.



%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
app_helper_test_() ->
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
%       {"Should start app",fun start_app_test/0},      
%       {"Should start app",fun start_app_test/0}	
      ]
    }.

setup() ->
    ok.

cleanup(_Ctx) ->
    ok.



-endif.