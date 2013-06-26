-module(ejabberd_auth_spark_config).
-author('etsang@spark.net').

-export([
         load_config/0,
	 load_config/1,
	 get_log_config/0,
         get_full_resource_url/2,
	 spark_api_endpoint/1, 
	 spark_app_id/1,
	 spark_client_secret/1,
	 spark_oauth_access_token/1, 
	 spark_communityid_brandid_map/1,
	 auth_profile_miniProfile/1, 
	 profile_memberstatus/1, 
%	 send_missed_im/1,
	 rabbitmq_endpoint/1,
 	 rest_client_timeout_in_sec/1,
 	 rest_call_retry_attempt/1,
 	 rabbitmq_client_timeout_in_sec/1,
 	 rabbitmq_client_retry_attempt/1
       ]).

-include("ejabberd.hrl").
-type url() :: string() | undefined.
-type accessToken() :: string() | undefined.
-type fatalError() :: {'EXIT', {error, {atom(), not_found}}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-define(APP_ENV,ejabberd_auth_spark).

-define(DEFAULT_RESTCONN_TIMEOUT,5).
-define(DEFAULT_RESTCONN_RETRY,3).
-define(DEFAULT_RABBITCONN_TIMEOUT,5).
-define(DEFAULT_RABBITCONN_RETRY,3).


%% ===================================================================
%% Public API
%% ===================================================================
get_log_config()-> get_log_config(?APP_ENV).
get_log_config(Name)->
   get_config_value_env(log_config, Name, required).

load_config()->
   M = atom_to_list(?APP_ENV),
   F = lists:concat([M,".config"]),
   load_config(F).
load_config(File)->
   {ok, Cwd} = file:get_cwd(),
   {ok, [CfgList]} = file:consult(File),
   CfgList.

get_full_resource_url(BaseUrl, ResourceUrl) ->
   lists:concat([BaseUrl,ResourceUrl]).

%% @doc Get spark api endpoint environment variable.
%% @end
-spec spark_api_endpoint(list())-> url() | undefined.
spark_api_endpoint(CfgList) ->
    get_config_value_env(spark_api_endpoint,CfgList).

-spec spark_app_id(list()) -> integer() | fatalError().
spark_app_id(CfgList)->
     get_config_value_env(spark_app_id,CfgList, required).   

-spec spark_client_secret(list()) -> string() | fatalError().
spark_client_secret(CfgList)->
     get_config_value_env(spark_client_secret,CfgList, required).

%% @doc Get the oauth access token environment variable.
-spec spark_oauth_access_token(list()) -> accessToken() | undefined.
spark_oauth_access_token(CfgList) ->
    get_config_value_env(spark_create_oauth_accesstoken, CfgList).  

spark_communityid_brandid_map(CfgList)->
    get_config_value_env(community2brandId, CfgList,required).

%% @doc Get the stun server list environment variable.
%% @end
%-spec stun_server_list(list()) -> list() | undefined.
%% stun_server_list(CfgList) ->
%%    get_config_value_env(stun_server_list,CfgList).

%% @doc Get the backup stun server list environment variable.
%% @end
%-spec stun_server_backup_list(list()) -> list() | undefined.
%%stun_server_backup_list(CfgList) ->
%%    get_config_value_env(stun_server_backup_list,CfgList,[]).

%% @doc Get api miniprofile environment variable.
%% @end
-spec auth_profile_miniProfile(list())-> string() | undefined.
auth_profile_miniProfile(CfgList) ->
    get_config_value_env(auth_profile_miniProfile, CfgList).

profile_memberstatus(CfgList)->
     get_config_value_env(profile_memberstatus,CfgList).   


%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rabbitmq_endpoint(list()) -> string() | undefined.
rabbitmq_endpoint(CfgList) ->
    get_config_value_env(rabbitmq_endpoint,CfgList).

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rest_client_timeout_in_sec(list()) -> integer().
rest_client_timeout_in_sec(CfgList) ->
      get_config_value_env(rest_client_timeout_in_sec,CfgList, ?DEFAULT_RESTCONN_TIMEOUT).    

%% @doc Get rest client call retry attempt environment variable.
%% @end
-spec rest_call_retry_attempt(list()) -> integer().
rest_call_retry_attempt(CfgList) ->
       get_config_value_env(rest_call_retry_attempt,CfgList, ?DEFAULT_RESTCONN_RETRY).

%% @doc Get the rabbitmq client connection timeout environment variable.
%% @end
-spec rabbitmq_client_timeout_in_sec(list()) -> integer().
rabbitmq_client_timeout_in_sec(CfgList) ->
       get_config_value_env(rabbitmq_client_timeout_in_sec,CfgList, ?DEFAULT_RABBITCONN_TIMEOUT).

%% @doc Get ther abbitmq client connection retry attempt  variable.
%% @end
-spec rabbitmq_client_retry_attempt(list())-> integer().
rabbitmq_client_retry_attempt(CfgList) ->
       get_config_value_env(rabbitmq_client_retry_attempt,CfgList,?DEFAULT_RABBITCONN_RETRY).

%% @private
get_config_value_env(Key,CfgList) ->
   app_helper:get_config_from_list(Key,CfgList).

get_config_value_env(Key, CfgList,required)->
   app_helper:get_config_from_list(Key,CfgList,required);
get_config_value_env(Key, CfgList,Default) ->
   app_helper:get_config_from_list(Key,CfgList,Default).

%===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-define(DEFAULTVAL, "DefaultTestVal").
-define(TESTAPPENV, ?APP_ENV).

ejabberd_auth_spark_config_test_() -> 
    { setup,
      fun setup/0,
      fun cleanup/1,
      [
        {"Should load nonempty config list", fun load_config_test_case/0},
	{"Should get api endpoint",fun spark_api_endpoint_test_case/0},
	{"Should get client secrete",fun  spark_client_secret_test_case/0},
	{"Should get community-brandi map",fun spark_communityid_brandid_map_test_case/0},
	{"Should get member status endpoint",fun profile_memberstatus_test_case/0},
	{"Should get miniprofile endpoint",fun auth_profile_miniProfile_test_case/0},
	{"Should get rest client timeout",fun rest_client_timeout_in_sec_test_case/0},
	{"Should get rest call timeout",fun rest_call_retry_attempt_test_case/0},
	{"Should get rabbitc timeout",fun rabbitmq_client_timeout_in_sec_test_case/0},
	{"Should get rabbitc retry attempt",fun rabbitmq_client_retry_attempt_test_case/0}
      ]
    }.

load_test_config()->
   {ok, Cwd} = file:get_cwd(),
   FileName = lists:concat([Cwd,"/",?APP_ENV,".config"]),
   L = load_config(FileName), 
   L.

load_config_test_case()->
   L = load_test_config(),
   ?assert(length(L) > 0).

spark_api_test_case()->
   L = load_test_config(),
   ?assertEqual("http://api.stgv3.spark.net/v2", spark_api_endpoint(L)).

spark_app_id_test_case()->
   L = load_test_config(),
   ?assertEqual("1054",spark_app_id(L)).


spark_api_endpoint_test_case()->
   L = load_test_config(),
   TestData = "http://api.stgv3.spark.net/v2",
   ?assertMatch(TestData, spark_api_endpoint(L)).

spark_client_secret_test_case()->
   L = load_test_config(),
    TestData = "nZGVVfj8dfaBPKsx_dmcRXQml8o5N-iivf5lBkrAmLQ1",
   ?assertMatch(TestData,  spark_client_secret(L)).	

spark_communityid_brandid_map_test_case()->
   L = load_test_config(),
    TestData = [{spark,"1","1001"},
               {jdate,"3","1003"},
               {cupid,"10","1015"},
               {bbw,"23","90410"},
               {blacksingle,"24","90510"}],
   ?assertEqual( TestData, spark_communityid_brandid_map(L)).

profile_memberstatus_test_case()->
    L = load_test_config(),
    TestData = "/brandId/{brandId}/application/{applicationId}/member/{memberId}/status",
    ?assertEqual(TestData, profile_memberstatus(L)).

auth_profile_miniProfile_test_case()->
    L = load_test_config(),
    TestData = "/brandId/{brandId}/profile/attributeset/miniProfile/{targetMemberId}",
    ?assertEqual(TestData, auth_profile_miniProfile(L)).

rest_client_timeout_in_sec_test_case() ->
    L = load_test_config(),
    TestData = ?DEFAULT_RESTCONN_TIMEOUT,
    ?assertEqual(TestData, rest_client_timeout_in_sec(L)).
	
rest_call_retry_attempt_test_case() ->
    L = load_test_config(),
    TestData = ?DEFAULT_RESTCONN_RETRY,
    ?assertEqual(TestData, rest_call_retry_attempt(L)).
	
rabbitmq_client_timeout_in_sec_test_case() ->
    L = load_test_config(),
    TestData = ?DEFAULT_RABBITCONN_TIMEOUT,
    ?assertEqual(TestData, rabbitmq_client_timeout_in_sec(L)).
 
rabbitmq_client_retry_attempt_test_case() ->
    L = load_test_config(),
    TestData = ?DEFAULT_RABBITCONN_RETRY,
    ?assertEqual(TestData, rabbitmq_client_retry_attempt(L)).


setup() ->   
   ok.

cleanup(_Pid) ->
   ok.


-endif.
