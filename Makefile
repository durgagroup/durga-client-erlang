PROJECT = durga_client_erlang

# dependencies

DEPS = websocket_client msgpack fast_key

dep_msgpack = git https://github.com/msgpack/msgpack-erlang.git 7cc8266afd06639d24a33e1ed60234983aab6443
dep_websocket_client = git https://github.com/jeremyong/websocket_client master
dep_fast_key = git https://github.com/camshaft/fast_key.git master

include erlang.mk

repl: all bin/start
	@bin/start durga_client_erlang

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: repl
