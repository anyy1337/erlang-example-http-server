-module(client).
-behaviour(gen_server).

%%API
-export([start_link/0, stop/1, set_socket/2, response/2]).
%%gen_server API
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, addr}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{socket = null, addr = null}}.

handle_call({stop, Reason}, _From, State) ->
    {stop, normal, Reason, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({socket_ready, Socket}, _State) ->
    inet:setopts(Socket, [binary, {active, once}, {packet, 0}]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    NewState = #state{socket = Socket, addr = IP},
    {noreply, NewState};
handle_cast({request, Req}, #state{addr = IP} = State) ->
    {ok, Pid} = parser_sup:start_parser(),
    case http_parser:decode(Pid, {self(), IP, Req}) of
        ok -> {noreply, State};
        {error, R} -> {stop, R, State}
    end;
handle_cast({response, Res}, #state{socket = S} = State) ->
    case gen_tcp:send(S, Res) of
        ok -> {stop, finish, State};
        {error, R} -> {stop, R, State}
    end;
handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info({tcp, Socket, Bin}, #state{socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]), 
    request(self(), Bin),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket),
    {ok, Reason}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).
    
stop(Reason) ->
    gen_server:call(?SERVER, {stop, Reason}).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_server:cast(Pid, {socket_ready, Socket}).

request(Pid, Req) when is_pid(Pid), is_binary(Req) ->
    gen_server:cast(Pid, {request, Req}).

response(Pid, Res) when is_pid(Pid), is_binary(Res) ->
    gen_server:cast(Pid, {response, Res}).