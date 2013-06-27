-module(spark_rabbitc_app_config).
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

-type url() :: string() | undefined.
-type accessToken() :: string() | undefined.
-type fatalError() :: {'EXIT', {error, {atom(), not_found}}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("lager/include/lager.hrl").
-include_lib("spark_rabbitc.hrl").

%-define(APP_ENV,ejabberd_auth_spark).


-deinfe(SERVER,?MODULE).
-define(SERVER_CONF, spark_config).

%% ===================================================================
%% Public API
%% ===================================================================
get_default_tag()->
   atom_to_list(?SERVER_CONF).

get_log_config()-> get_log_config(get_default_tag()).
get_log_config(Name)->
   get_config_value_env(log_config, Name, required).


load_config()->
   M = get_default_tag(),
   F = lists:concat([M,".config"]),
   load_config(F).

load_config(File)->
   {ok, Cwd} = file:get_cwd(),
   {ok, [CfgList]} = file:consult(File),
   CfgList.

load_config(FileName, Key) ->
    case file:consult(FileName) of
        {ok, Term} ->
             CfgList = proplists:get_value(Key, Term, []),
             {ok, CfgList};
        {error, Reason}->
            ErrorMsg = [{filename, FileName},
                        {reason,   Reason}],
            lager:log(error,"load config error:~n~p~n", [ErrorMsg]),
            {error, Reason}
    end.

get_full_resource_url(BaseUrl, ResourceUrl) ->
   ApiVsn = get_api_vsn(CfgList),
   FulllUrl = lists:concat([BaseUrl, ResourceUrl]),
   ReplaceOpts = [global, {return, list}],
   Ret = re:replace(FullUrl,"ApiVsn", ApiVsn, ReplaceOpts)
   list_to_binary(Ret).

get_https_or_http()->
  http.

get_ssl()->
  false;

get_rabbit_environment(CfgList) ->
   get_config_value_env(rabbitc_env, CfgList, required). 

get_rest_environment(CfgList) ->
   get_config_value_env(rest_env, CfgList, required).

get_api_vsn(CfgList)->
   get_config_value_env(api_vsn,CfgList, required).

%% @doc Get spark api endpoint environment variable.
%% @end
-spec spark_api_endpoint(list())-> url() | fatalError().
spark_api_endpoint(CfgList) ->
   get_config_value_env(spark_api_endpoint,CfgList, required).

-spec spark_app_id(list()) -> integer() | fatalError().
spark_app_id(CfgList)->
   get_config_value_env(spark_app_id,CfgList, required).   

-spec spark_app_id(list()) -> integer() | undef.
spark_brand_id(CfgList)->
   get_config_value_env(spark_brand_id,CfgList, undef). 

-spec spark_client_secret(list()) -> string() | fatalError().
spark_client_secret(CfgList)->
   get_config_value_env(spark_client_secret,CfgList, required).

%% @doc Get the oauth access token environment variable.
-spec spark_oauth_access_token(list()) -> accessToken() | undefined.
spark_oauth_access_token(CfgList) ->
   get_full_resource_url(CfgList, spark_create_oauth_accesstoken).

spark_communityid_brandid_map(CfgList)->
   get_config_value_env(community2brandId, CfgList,required).

%% @doc Get api miniprofile environment variable.
%% @end
-spec auth_profile_miniProfile(list())-> string() | undefined.
auth_profile_miniProfile(CfgList) ->
   get_full_resource_url(CfgList,auth_profile_miniProfile).

profile_memberstatus(CfgList)->
   get_full_resource_url(CfgList, profile_memberstatus).

get_full_resource_url(CfgList, Key)->
   BaseUrl = spark_api_endpoint(CfgList),
   ResUrl = get_config_value_env(Key,CfgList),
   get_full_resource_url(BaseUrl, ResUrl).
     

%% @doc Get the rabbitmq endpoint environment variable.
%% @end
-spec rest_client_timeout_in_sec(list()) -> integer().
rest_client_timeout_in_sec(CfgList) ->
   get_config_value_env(rest_client_timeout_in_sec,CfgList, 5000).    

%% @doc Get rest client call retry attempt environment variable.
%% @end
-spec rest_call_retry_attempt(list()) -> integer().
rest_call_retry_attempt(CfgList) ->
   get_config_value_env(rest_call_retry_attempt,CfgList, 3).

%% ======================RabbitMq ======================================
get_rabbit_username(CfgList) ->
   get_config_value_env(username,ConfList,<<"guest">>).

get_rabbit_password(CfgList) ->
   get_config_value_env(password,ConfList,<<"guest">>).

get_rabbit_vhost(CfgList) ->
   get_config_value_env(virtual_host,ConfList,<<"/">>).

get_rabbit_exchange(CfgList) ->
   get_config_value_env(exchange,ConfList,<<"">>).

get_rabbit_client_properties(CfgList) ->
   get_config_value_env(client_properties,ConfList,[]).

get_rabbit_conneciton_timeout(CfgList) ->
   get_config_value_env(connection_timeout,ConfList,infinity).

get_rabbit_ticket(CfgList) ->
   get_config_value_env(ticket,ConfList,0).

get_rabbit_type(CfgList) ->
   get_config_value_env(type,ConfList,<<"direct">>).

get_rabbit_passive(CfgList) ->
   get_config_value_env(passive,ConfList,false).

get_rabbit_durable(CfgList) ->
   get_config_value_env(durable,ConfList,false).

get_rabbit_auto_delete(CfgList) ->
   get_config_value_env(auto_delete,ConfList,false).

get_rabbit_internal(CfgList) ->
   get_config_value_env(internal,ConfList,false).

get_rabbit_nowait(CfgList) ->
   get_config_value_env(nowait,ConfList,false).

get_rabbit_arguments(CfgList) ->
   get_config_value_env(arguments,ConfList,[]).

get_rabbit_channel_count(CfgList) ->
   get_config_value_env(channel_count,ConfList,[]).

%% ======================================================================


%% @private
get_config_value_env(Key,CfgList) ->
   R = proplists:get_value(Key,ConfList,{warn, undef}),
   to_bitstring(R).

get_config_value_env(Key, CfgList,required)->
   case proplists:get_value(Key,ConfList,{error, undef}) of
        {error, undef} -> erlang:exit(undef);
        V -> to_bitstring(V)
   end.

get_config_value_env(Key, CfgList,Default) ->
   R = proplists:get_value(Key,ConfList,Default),
   to_bitstring(R).

to_bistring(S) when is_list(S)->
  erlang:list_to_bitstring(S).

to_bitstring(_)->
  <<"">>.

load_setting(ApiConf,RabbitConf),
   AmqpParam = load_amqp_config(RabbitConf),
   RestParam = load_rest_config(ApiConf).
   {ok, #state{
 	      amqp_param = AmqpParm,
	      rest_param = RestParm
   }}.

load_amqp_config(RabbitConf) ->
   ConfList = load_config(RabbitConf, spark_rabbit),
   Environment	= get_rabbit_environment(ConfList),
   UserName    = get_rabbit_username(ConfList),
   Password    = get_rabbit_password(ConfList),
   VirtualHost = get_rabbit_virtual_host(ConfList),
   Exchange     = get_rabbit_exchange(ConfList),
   Client_properties = get_rabbit_client_properties(ConfList),
   Connection_timeout = get_rabbit_conneciton_timeout(ConfList),
   Ticket       = get_rabbit_ticket(ConfList),
   Type         = get_rabbit_type(ConfList),
   Passive      = get_rabbit_passive(ConfList),
   Durable      = get_rabbit_durable(ConfList),
   AutoDelete   = get_rabbit_auto_delete(ConfList),
   Internal     = get_rabbit_internal(ConfList),
   Arguments    = get_rabbit_arguments(ConfList),
   NoWait       = get_rabbit_nowait(ConfList),
   ChannelCount = get_rabbit_channel_count(ConfList,[]),
   AmqpParam   = #amqp_params_network{
                  	username     = ensure_binary(UserName),
                        password     = Password,
                        virtual_host = ensure_binary(VirtualHost),
                        host         = Host,
                        port         = Port
                 },
   #rabbit_conf{


   }.

load_rest_config(ApiConf)->
   ConfList = load_config(ApiConf, spark_rest),
   Environment	= get_rest_environment(CfgList),
   Api_endpoint = spark_api_endpoint(CfgList),
   App_id = spark_app_id(CfgList),
   Brand_id = spark_brand_id(ConfList),
   Client_secret = spark_client_secret(CfgList),
   Create_oauth_accesstoken = spark_oauth_access_token(CfgList),
   Auth_profile_miniProfile= auth_profile_miniProfile(CfgList),
   Member_Status = profile_memberstatus(CfgList),
   IdMap = spark_communityid_brandid_map(CfgList),
   ResUrls0 =  dict:new(),
   ResUrl1 = dict:append(oauth_access_token, Create_oauth_accesstoken, ResUrls0),
   ResUrl2 = dict:append(auth_profile_miniProfile, Auth_profile_miniProfile, ResUrls1),
   ResUrl3 = dict:append(profile_memberstatus, Member_Status, ResUrl2),

   #rest_conf{
	     base_url = Api_endpoint,
	     resource_urls = ResUrl3,
	     app_id = App_id,
	     client_secret = Client_secret,
       idMap = IdMap,
       retry = rest_call_retry_attempt(),
       timeout = rest_client_timeout_in_sec(),
	     http_method = get_https_or_http(),
	     ssl_key = get_ssl()
   }.


ensure_binary(undefined)->
    undefined;
ensure_binary(Value) when is_binary(Value)->
    Value;
ensure_binary(Value) when is_list(Value)->
    list_to_binary(Value).

%% ================ default =========================

default_api_endpoint()->
  "http://api.{environment}.spark.net/{api_vsn}/".

default_client_secret()->
  "nZGVVfj8dfaBPKsx_dmcRXQml8o5N-iivf5lBkrAmLQ1". 

default_spark_create_oauth_accesstoken()->
  "/brandId/{brandId}/oauth2/accesstoken/application/{applicationId}".

default_auth_profile_miniProfile()->
  "/brandId/{brandId}/profile/attributeset/miniProfile/{targetMemberId}".

default_profile_memberstatus()->
  "/brandId/{brandId}/application/{applicationId}/member/{memberId}/status".

default_community2brandId()->
  [{spark,"1","1001"},
   {jdate,"3","1003"},
   {cupid,"10","1015"},
   {bbw,"23","90410"},
   {blacksingle,"24","90510"}]. 


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
