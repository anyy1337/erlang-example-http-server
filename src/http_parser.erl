-module(http_parser).
-behaviour(gen_server).

%%API
-export([start_link/0, stop/1]).
-export([decode/2]).
%%gen_server API
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

-record(parser, {
    owner,
    ip,
    http_protocol,
    request_method,
    data = [],
    query_string = [],
    http_accept,
    http_accept_encoding,
    connection,
    http_host,
    http_user_agent,
    request_uri,
    post_string = []
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Reason) ->
   gen_server:call(?SERVER, {stop, Reason}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({decode, Owner, IP, Req}, _From, State) ->
    [X | Xs] = binary:split(Bin, <<"\r\n">>, [global, trim_all]),
    HeaderOne = binary:split(X, <<" ">>, [global]),
    {Method, URI, Protocol} = list_to_tuple(HeaderOne),
    lists:reverse(render(Xs)); 
handle_call({stop, Reason}, _From, State) ->
    {stop, normal, Reason, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    {ok, Reason}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

decode(Pid, {Owner, IP, Req}) when is_pid(Pid), is_pid(Owner), is_tuple(IP), is_binary(Req) ->
    gen_server:call(Pid, {decode, Owner, IP, Req});
decode(_, {_, _, _}) ->
    {error, bad_args}.

render(List) ->
    render(List, []).
render([], Acc) ->
    Acc;
render([X | Xs], Acc) ->
    B = binary:split(X, <<": ">>, [global, trim_all]),
    Y = list_to_tuple(B),
    render(Xs, [Y | Acc]).