-module(durga_client_erlang_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  durga_client_erlang_sup:start_link().

stop(_State) ->
  ok.
