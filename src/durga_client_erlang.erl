-module(durga_client_erlang).

-behaviour(websocket_client_handler).

%% API.
-export([start/0]).
-export([start_link/1]).
-export([all/0]).
-export([resolve/4]).
-export([resolve/5]).
-export([stop/1]).

%% websocket_client_handler

-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-record(state, {
  tab
}).

-define(MSGPACK_OPTIONS, [
  {allow_atom, pack},
  {format, map}
]).

start() ->
  {ok, _} = application:ensure_all_started(durga_client_erlang),
  ok.

start_link(Opts) ->
  verify_args([url], Opts),
  websocket_client:start_link(fast_key:get(url, Opts), ?MODULE, Opts).

verify_args([], _) ->
  ok;
verify_args([Arg|Args], Opts) ->
  case fast_key:get(Arg, Opts) of
    undefined ->
      exit({missing_arg, Arg});
    _ ->
      verify_args(Args, Opts)
  end.

all() ->
  ets:foldl(fun({Key, Url}, Acc) ->
    case fast_key:get(Key, Acc) of
      undefined ->
        fast_key:set(Key, [Url], Acc);
      Urls ->
        fast_key:set(Key, [Url|Urls], Acc)
    end
  end, #{}, ?MODULE).

resolve(Type, Mod, Fun, Args) ->
  resolve(Type, Mod, Fun, Args, <<"production">>).

resolve(Type, Mod, Fun, Args, Env) when is_list(Args) ->
  resolve(Type, Mod, Fun, length(Args), Env);
resolve(Type, Mod, Fun, Args, Env) when is_map(Args) ->
  resolve(Type, Mod, Fun, maps:size(Args), Env);
resolve(Type, Mod, Fun, Args, Env) ->
  case ets:lookup(?MODULE, {Type, Mod, Fun, Args, Env}) of
    [] ->
      {error, notfound};
    Services ->
      {ok, [Service || {_K, Service} <- Services]}
  end.

stop(Pid) ->
  Pid ! stop.

init(_Opts, _Req) ->
  %% TODO auth
  Tab = ets:new(?MODULE, [named_table, {read_concurrency, true}, protected, bag]),

  {ok, #state{tab = Tab}, 10000}.

websocket_handle({binary, Bin}, _, State) ->
  case catch msgpack:unpack(Bin, ?MSGPACK_OPTIONS) of
    {ok, Res} ->
      handle_response(Res, State#state.tab),
      {ok, State};
    Error ->
      error_logger:error_msg("Unable to decode response ~p~n~p~n", [Bin, Error]),
      {ok, State}
  end;
websocket_handle({pong, _}, _, State) ->
  {ok, State};
websocket_handle(Other, _, State) ->
  io:format("OTHER ~p~n", [Other]),
  {ok, State}.

websocket_info(_, _ConnState, State) ->
  {ok, State}.

websocket_terminate(Reason, _ConnState, State) ->
  io:format("Websocket closed in state ~p wih reason ~p~n",
            [State, Reason]),
  ok.

handle_response(Res, Tab) ->
  catch ets:insert(Tab, [
    {{Type, Mod, Fn, safe_length(Args), Env}, binary_to_list(Url)} ||
     #{<<"type">> := Type, <<"module">> := Mod, <<"method">> := Fn, <<"arguments">> := Args,
       <<"env">> := Env, <<"url">> := Url} <- Res]).

safe_length(L) when is_list(L) ->
  length(L);
safe_length(_) ->
  0.
