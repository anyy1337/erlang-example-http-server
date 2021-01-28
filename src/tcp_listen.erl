-module(tcp_listen).
-behaviour(gen_server).

%% API
-export([stop/1, start_link/0, getPort/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TCP_OPTIONS, [
    binary,
    {active, false},
    {packet, 0},
    {reuseaddr, false},
    {keepalive, true},
    {backlog, 65000}
]).

-record(state, {
    listen,
    accept
}).

init([]) ->
    process_flag(trap_exit, true),
    Port = getPort(),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, Listen} ->
            {ok, Ref} = prim_inet:async_accept(Listen, -1),
            {ok, #state{
                listen = Listen,
                accept = Ref
            }};
        {error, R} ->
            {stop, R}
    end.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info({inet_async, Listen, Ref, {ok, Socket}}, 
            #state{listen = Listen, accept = Ref} = State) ->
    try
        case set_sockopt(Listen, Socket) of
            ok -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end, 

        {ok, Client} = gateway_sup:start_client(),
        gen_tcp:controlling_process(Socket, Client),
        client:set_socket(Client, Socket),

        case prim_inet:async_accept(Listen, -1) of
            {ok, NewRef} -> ok;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#state{accept = NewRef}}
    catch exit:Why ->
         error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
         {stop, Why, State}
    end;
handle_info({inet_async, Listen, Ref, Error}, #state{listen=Listen, accept=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};
handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen),
    ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

getPort() ->
    {ok, P} = application:get_env(spws, port),
    P.

set_sockopt(ListSock, CliSocket) ->
        true = inet_db:register_socket(CliSocket, inet_tcp),
        case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSocket, Opts) of
            ok    -> ok;
            Error -> gen_tcp:close(CliSocket), Error
            end;
        Error ->
            gen_tcp:close(CliSocket), Error
        end.

stop(Name) ->
    gen_server:call(Name, stop).
             
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).