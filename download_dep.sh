#!/bin/bash

get_ejabberd_header(){
	echo "Downloading ejabberd.hrl and jlib.hrl"
	curl -O "https://raw.github.com/processone/ejabberd/master/include/ejabberd.hrl"
	curl -O "https://raw.github.com/processone/ejabberd/master/include/jlib.hrl"
}

get_ejabberd_config(){

	echo "Downloading rabbitmq_server gen_server2"
	curl -O "https://raw.github.com/rabbitmq/rabbitmq-server/master/src/gen_server2.erl"

}

move_header(){
	echo "Move header files to include dir"
	mv -vf ejabberd.hrl ./include
	mv -vf jlib.hrl ./include
}

move_ejabberd_files(){
	echo "Move header files to include dir"

	mv -vf gen_server2.erl ./src
}

is_file_exists(){
	local f="$1"
	[[ -f "$f" ]] && return 0 || return 1
}

echo "Download files"
get_ejabberd_config
move_ejabberd_files
