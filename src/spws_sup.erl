-module(spws_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(STRATEGY, {
    one_for_one,
    5,
    3600
}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecList = [childSpec(tcp_listen, worker), 
                    childSpec(gateway_sup, supervisor) %childSpec(parser_sup, supervisor)
                    ],
    {ok, {?STRATEGY, ChildSpecList}}.

childSpec(Module, Type) ->
    {Module, {Module, start_link, []}, permanent, 2000, Type, [Module]}.