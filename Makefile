ERLANG_HOME = /usr/local/lib/erlang
ERL_LIBS = ${ERLANG_HOME}/lib
PACKAGE_NAME = spark_rabbitc_app
PACKAGE_VERSION = spark_rabbitc_app
VERBOSE = -vvv
RABBITMQ_ERL_C = ${PWD}/deps/rabbitmq-erlang-client

.PHONY: deps compile rel test

all: compile

compile: deps
	@pwd
	@cd ${RABBITMQ_ERL_C}; make
	@rsync -a -v ${RABBITMQ_ERL_C}/dist/amqp_client-0.0.0 ${PWD}/deps
	@rsync -a -v ${RABBITMQ_ERL_C}/dist/rabbit_common-0.0.0 ${PWD}/deps
	@./rebar ${VERBOSE} compile

deps:
	@./rebar ${VERBOSE} get-deps

check:
	@echo "Dependencies"
	@./rebar ${VERBOSE} check-deps

clean:
	@cd ${RABBITMQ_ERL_C}; make --debug clean

	@./rebar ${VERBOSE} clean

rel: all
#@(cp -Rf ./etc ./rel/files/etc)
	@(make rel_erlang)
#@(chmod u+x ./rel/${PACKAGE_NAME}/bin/*)

rel_erlang:
	@./rebar ${VERBOSE} generate force=1

doc:
	@./rebar ${VERBOSE} doc skip_deps=true

package:
	@(mkdir -p ./builds)
	@(tar -C rel -c ${PACKAGE_NAME} | gzip > ./builds/${PACKAGE_NAME}-${PACKAGE_VERSION}.tar.gz)

test: compile
	@./test/bootstrap.sh
ifdef suite
	@./rebar ${VERBOSE} skip_deps=true eunit suite=$(suite)
else
	@./rebar ${VERBOSE} skip_deps=true eunit
endif
