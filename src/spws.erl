-module(spws).
-behaviour(application).

-export([start/2, stop/1]).
-export([init/1]).

-define(STRATEGY, {
    one_for_one,
    5,
    3600
}).

start(_Type, _Args) ->
    start_link().
stop(_State) ->
    ok.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = [{spws_sup, {spws_sup, start_link, []}, permanent, 2000, supervisor, [spws_sup]}],
    {ok, {?STRATEGY, ChildSpec}}.