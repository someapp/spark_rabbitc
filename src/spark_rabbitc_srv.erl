-module(spark_rabbitc_srv).
-behaviour(gen_server).


-export([publish_chat_msg/1,
	 consume_chat_msg/0,
	 post_chat_msg/1]).

-export([start/0,stop/0,
	 start_link/0, start_link/1]).
-export([
	init/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-endif.

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("lager/include/lager.hrl").
-include_lib("spark_rabbitc.hrl").

-type state()::#state{}.
-type conn_state()::#conn_state{}.

-define(SERVER, ?MODULE).

publish_chat_message([])->
  ok;
publish_chat_message([Message|Tail])->
  
  Ret.

consume_chat_msg()->,
  
  Ret.

post_chat_msg([])->
  ok;
post_chat_msg([Message|Tail])->
  

  ok.

start()->
  app_helper:ensure_app_started(?SERVER).

stop()->
  app_helper:ensure_app_stopped(?SERVER).

start_link()->
  lager:log(info,"Start linking ~p with 0 Args ",[?MODULE]),
  gen_server:start_link({local, ?SERVER},
			?SERVER,[],[]).

start_link(Args)->
  lager:log(info,"Start linking ~p with Args ~p",[?MODULE, Args]),
  gen_server:start_link(Args).

init()-> init([]).
init([])->
  {ok, RabbitConf} = application:get_env(rabbit_config),
  {ok, ApiConf} = application:get_env(rest_config),
  
  Setting = load_setting(ApiConf,RabbitConf),
  {ok, #state{}}. 

init(_Args)->
  init([]).

handle_call({publish_msg, Messages}, From, State)->

  {reply, Reply, State};

handle_call({consume_msg}, From, State)->

  {reply, Reply, State};

handle_call({post_msg}, From, State)->
  
  {reply, Reply, State};

handle_call(Message, _From, State)->
  lager:log(warn," ~p is unsupported",[Message]),  
  {reply, ok, State}.



handle_cast(stop, State)->
  {stop,normal, State};
handle_cast(_Event, State)->
  {noreply, State}.

handle_info(timeout,State) ->
  {noreply, State, hibernate};
handle_info(timeout, State)->
  lager:log(info,"Timeout with State ~p",[State]),
  {noreply, State};
handle_info(_BadMsg, State)->
  {noreply, State}.


terminate(normal, _State)->
  ok;
terminate(Reason, State)->
  lager:log(warn, "Abnormal termination reason: ~p",[Reason]),
  ok.

code_change(_OldVsn, State, _Extra)->
  {ok, State}.


load_setting(ApiConf,RabbitConf),
   AmqpParam = spark_rabbitc_app_config:load_amqp_config(RabbitConf),
   RestParam = spark_rabbitc_app_config:load_rest_config(ApiConf),
   {ok, #state{
  	 amqp_param = AmqpParm,
	 rest_param = RestParm
   }}.


get_fun(cast)->
    fun amqp_channel:cast/3;
get_fun(call)->
    fun amqp_channel:call/3.

ensure_binary(undefined)->
    undefined;
ensure_binary(Value) when is_binary(Value)->
    Value;
ensure_binary(Value) when is_list(Value)->
    list_to_binary(Value).
 
