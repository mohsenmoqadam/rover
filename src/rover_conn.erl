-module(rover_conn).

-behaviour(gen_server).

%% API
-export([ start/3
	, send/2
	]).

%% gen_server callbacks
-export([ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	, format_status/2
	]).

-define(SERVER, ?MODULE).

-include("rover.hrl").

-record(state, {transport, socket}).

%%%===================================================================
%%% API
%%%===================================================================

start(Transport, Socket, Opts) -> 
    init([Transport, Socket, Opts]).

send(P, Message) ->
    gen_server:cast(P, Message).
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Transport, Socket, _Opts]) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = Transport:setopts(Socket, [{active, once}]),
    State = #state{transport = Transport, socket = Socket},
    ?LOG_DEBUG("New Socket Created, Pid: ~p, Transport: ~p, Socket: ~p", [self(), Transport, Socket]),
    gen_server:enter_loop(?MODULE, [], State). 

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Message, #state{transport = Transport, socket = Socket} = State) ->
    ?LOG_DEBUG("Message is: ~p", [Message]),
    Transport:send(Socket, Message),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, _Port, Data}, #state{ socket = Socket
				      , transport = Transport
				      } = State) ->
    ?LOG_DEBUG("[~p] incoming binary: ~p", [self(), Data]),
    Transport:setopts(Socket, [{active, once}]),  
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
