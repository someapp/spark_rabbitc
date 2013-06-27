-ifndef(SPARK_RABBITC_HRL).
-define(SPARK_RABBITC_HRL, true).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(rabbit_conf, {
		 path		    = ?DEFAULT_PATH, 
		 format		    = ?DEFAULT_FORMAT,
		 idMap		    = [],
		 host		     = <<"">>,
		 connection_timeout  = ?HIBERNATE_TIMEOUT,
	  	 name = ?DEFAULT_NAME,
    		 exchange = ?DEFAULT_EXCHANGE, 
		 queue=?DEFAULT_QUEUE,
		 amqp_params = #amqp_params_network {}
}).

-record(rest_conf,{
	base_url = [],
	resource_urls = orddict:new(),
	app_id = -1,
	client_secret = 0
	http_method = http,
	ssl_key = false
}).

-record(state,{
	rabbitconf= #rabbit_conf{},
	restconf = #rest_conf{}, 
	node = node(),
	rest_conf_file = [],
	rabbit_conf_file = [],
	logfile_conf_file = []
}).



-endif.
