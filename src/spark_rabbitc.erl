-module (spark_rabbitc).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("lager/include/lager.hrl").
-include_lib("spark_rabbitc.hrl").

-export([
		connect/0,
		connect/1,
		reconnect/0,
		disconnect/0,
		subscribe/0,
		subscribe/1,
		consume/1,
		status/0
		]).

-export([start_link/0]).
-export([init/1, 
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-record(cur_con, {
	connection :: pid(), % rabbitMQ connection
	connection_ref :: reference(), % connection monitor ref
	channel :: pid(), % rabbitMQ channel
	channel_ref :: reference() % channel monitor ref
}).
 
%% Internal state
-record(state, {
	cur_con = #cur_con{},
	rabbitmq_restart_timeout = 5000 :: pos_integer(), % restart timeout
	amqp_params =  #amqp_params_network{},
	rest_params = #rest_conf{}
}).

connect()->

	ok.

connect(Conf)->

	ok.

reconnect()->

	ok.

disconnect()->

	ok.

subscribe()->

	ok.

subscribe(Conf)->

	ok.

consume(Mod, Func)->

	ok.

status()->

	ok.


%%==========
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init(_Args) ->
	gen_server:cast(self(), connect),
	{ok, #state{rabbitmq_restart_timeout = 5000}}.
 
%%
%% @doc Handling all messages from RabbitMQ
%% @end
handle_info({#'basic.deliver'{delivery_tag = Tag, routing_key = _Queue}, 
	#amqp_msg{props = #'P_basic'{reply_to = ReplyTo}, payload = Body}} = _Msg, 
	Channel = #cur_con.channel,
	#state{Channel = Channel} = State) ->
	amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
	try
		Message = binary_to_term(Body);
		%
		% Message is your payload
		%
	catch
		Class:Reason -> {Class, Reason}
 	end,
	{noreply, State};
 
handle_info({'DOWN', ConnectionRef, process, Connection, Reason}, #state{connection = Connection, connection_ref = ConnectionRef} = State) ->
	error_logger:error_report("AMQP connection error"),
	restart_me(State);
 
handle_info({'DOWN', ChannelRef, process, Channel, Reason}, #state{channel = Channel, channel_ref = ChannelRef} = State) ->
	error_logger:error_report("AMQP channel error"),
	restart_me(State);
 
handle_info(_Info, State) ->
	error_logger:error_report("Unsupported info message"),
	{noreply, State}.
 
handle_cast(connect, State) ->
	% connection parameters
	AMQP_Param = #amqp_params_network{
					host = "localhost",
					username = <<"username">>,
					password = <<"password">>,
					port = 5672,
					virtual_host = <<"vhost">>,
					heartbeat = 5 %% --- important to keep your connection alive
				},
	% connection...
	case amqp_connection:start(AMQP_Param) of
		{ok, Connection} ->
		% start connection monitor
					ConnectionRef = erlang:monitor(process, Connection),
					case amqp_connection:open_channel(Connection) of
					{ok, Channel} ->
								% add monitor to catch message when connection is 'DOWN'
								ChannelRef = erlang:monitor(process, Channel),%
		%
		% Here you have to subscribe to queues you want to listen
		%						
		%						
								
								{noreply, 
								 State#state{
									cur_con = #cur_con{
												connection = Connection,
			 									connection_ref = ConnectionRef,
												channel = Channel,
												channel_ref = ChannelRef
									}
								}};
					_Reason2 ->
								error_logger:error_report("AMQP channel error"),
								restart_me(State)
					end;
		_Reason1 ->
				error_logger:error_report("AMQP connection error"),
				restart_me(State)
	end;
 
handle_cast(_Msg, State) ->
	error_logger:error_report("Unsupported cast message"),
	{noreply, State}.
 
handle_call(_Request, _From, State) ->
	error_logger:error_report("Unsupported call message"),
	{reply, ok, State}.
 
terminate(_Reason, #state{connection = Connection, channel = Channel} = _State) ->
	ok = can_channel(is_alive(Channel), Channel),
	ok = can_connection(is_alive(Connection), Connection),
	ok.

is_alive(P)->
 	erlang:is_process_alive(P).
close_channel(true, Close)->
	close(amqp_channel,Close).
close_connection(true, Close)->
	close(amqp_connection,Close).
close(M) when is_atom(M)->
	ok = M:close(C);
close(_) ->
	ok.

 
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
 
%%
%% This function is called when client lost connection to RabbitMQ
restart_me(#state{rabbitmq_restart_timeout = Wait} = State) ->
	timer:sleep(Wait), % Sleep for rabbitmq_restart_timeout seconds
	{stop, error, State}.

