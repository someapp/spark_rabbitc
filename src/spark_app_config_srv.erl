-module(spark_app_config_srv).
-behaviour(gen_server).

-export([
	load_config/0,
	load_config/1,
	load_config/2,
	lookup/1,
	lookup/2,
	update/2,
	remove/1
	]).
-export([start_link/0]).
-export([
	init/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

-define(SERVER, ?MODULE).
-define(CONFPATH,"../conf").
-define(BADKEY,not_an_atom).

-record(state,{conf}).
load_config()->
  {ok, ConfDir}= cwd(),
  load_config(ConfDir, "spark_config.config").

load_config(File) ->
  {ok, ConfDir}= cwd(),
  load_config(ConfDir,File).

load_config(ConfDir,File) when is_list(ConfDir), 
			  is_list(File)->
  FileFullPath = list:concat([ConfDir, File]),
  {ok, #state{conf=ConfList}} = file:consult(FileFullPath),
  {ok, #state{conf=ConfList}}.

lookup(Key) when is_atom(Key)->
  gen_server:call(?SERVER,{lookup, Key});
lookup(Key) ->
  gen_server:call(?SERVER,{error, Key, ?BADKEY}).

lookup(Key, required) when is_atom(Key)->
  gen_server:call(?SERVER,{lookup, Key, required});

lookup(Key, Default) when is_atom(Key)->
  gen_server:call(?SERVER,{lookup, Key, Default});
lookup(Key, _) ->
  gen_server:call(?SERVER,{error, Key, ?BADKEY}).


update(Key,Value) when is_atom(Key)->
  gen_server:call(?SERVER,{update, Key, Value});   
update(Key,_V)-> 
  gen_server:call(?SERVER,{error, Key, ?BADKEY}).

remove(Key) when is_atom(Key)->
  update(Key, undef);
remove(Key)-> 
  gen_server:call(?SERVER,{error, Key, ?BADKEY}).


cwd()->
  {ok, Cwd} = file:get_cwd(),
  lists:concat([Cwd,"/",?CONFPATH]).

start_link()->
  gen_server:start_link({}).

init()-> init([]).
init([])->
  load_config();
init([ConfDir, File])->
  load_config(ConfDir,File).


handle_call({lookup, Key}, _From, State)->
   Val = app_helper:get_config_from_list(Key,State#state.conf),
   {reply, Val, State};
handle_call({update, Key, Value}, _From, State)->
   NewConf = app_heler:update_config(Key,State#state.conf,Value),
   {reply, ok, State#state{conf = NewConf}};

handle_call({lookup, Key, required}, _From, State)->
   Val = app_helper:get_config_from_list(Key,State#state.conf,required),
   {reply, Val, State};

handle_call({lookup, Key, Default}, _From, State)->
   Val = app_helper:get_config_from_list(Key,State#state.conf,Default),
   {reply, Val, State};


handle_call({error, Who, Reason}, _From, State)->
  {reply,{error,Who,Reason},State};
handle_call(BadMsg,_From,State)->
  {reply,{error,BadMsg, unsupported}, State}.

handle_cast(_BagMsg, State)->
  {noreply, State}.

handle_info(_BadMsg, State)->
  {noreply, State}.

terminate(_Reason, _State)->
  ok.

code_change(_OldVsn, State, _Extra)->
  {ok, State}.


