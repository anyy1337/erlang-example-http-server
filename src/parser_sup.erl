-module(parser_sup).
-behaviour(supervisor).

-export([start_link/0, start_parser/0, stop_parser/1]).
-export([init/1]).

-define(STRATEGY, {
    simple_one_for_one,
    10,
    60
}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_parser() ->
    supervisor:start_child(?MODULE, []).

stop_parser(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
     ChildSpec = [{http_parser, {http_parser, start_link, []},
          temporary, brutal_kill, worker, [http_parser]}],
     {ok, {?STRATEGY, ChildSpec}}.