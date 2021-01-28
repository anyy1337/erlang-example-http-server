-module(gateway_sup).
-behaviour(supervisor).

-export([start_link/0, start_client/0, stop_client/1]).
-export([init/1]).

-define(STRATEGY, {
    simple_one_for_one,
    10,
    60
}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client() ->
    supervisor:start_child(?MODULE, []).

stop_client(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
     ChildSpec = [{client, {client, start_link, []},
          temporary, brutal_kill, worker, [client]}],
     {ok, {?STRATEGY, ChildSpec}}.