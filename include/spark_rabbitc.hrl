-ifndef(SPARK_RABBITC_HRL).
-define(SPARK_RABBITC_HRL, true).

-include_lib("amqp_client/include/amqp_client.hrl").


-record(rabbit_conf, {
		 environment	= <<"stgv3">>,
		 
		 format		    = text,
		 host		     = <<"">>,
		 connection_timeout  = 5000,
	  	 name = <<>>,
    		 exchange = <<>>, 
		 queue= <<>>,
		 amqp_params = #amqp_params_network {
		     host = "localhost",
		     username = <<"guest">>,
		     password = <<"guest">>,
		     port = 5672,
		     virtual_host = <<"/">>,
		     heartbeat = 5
		 }
}).

-record(rest_conf,{
	base_url = [],
	rest_env = <<"">>,
	api_vsn = <<"2">>,
	api_endpoint = [],
	app_id = -1,
	client_secret = [],
 	resource_urls = dict:new(),
	idMap = dict:new(),
	retry = -1,
	timeout = -1,
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
