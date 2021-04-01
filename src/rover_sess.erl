-module(rover_sess).

-behaviour(gen_server).

%% API
-export([ handle_message/1
	, get_pid_by_uid/1
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

-include("rover.hrl").
-include("sess.hrl").
-include("message.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec handle_message(message()) -> ok. 
handle_message(#message{sess = PID} = Message) ->
    SESS = 
	case PID of 
	    undefined -> get_pid(Message);
	    Else -> Else
	end,
    gen_server:cast(SESS, Message).    

-spec get_pid_by_uid(uid()) -> pid().     
get_pid_by_uid(UID) ->
    get_pid(#message{uid = UID}).
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(#message{uid = UID} = Message) ->
    process_flag(trap_exit, true),
    PID = self(),
    ok = rover_cluster:set_actor({?SESS_ACTOR, UID}, PID),
    ok = gen_server:cast(PID, initialize_state),
    ok = gen_server:cast(PID, Message),
    {ok, #sess{uid = UID}}.

handle_call(Message, _From, State) ->
    ?LOG_DEBUG("[ROVER-SESS: ~p] Unhandled message (call): ~p ", [self(), Message]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(initialize_state, State) ->
    ?LOG_DEBUG("[ROVER-SESS: ~p] initialize state.", [self()]),
    {noreply, State};
handle_cast(#message{} = Message, State) ->
    ?LOG_DEBUG("[ROVER-SESS: ~p] Handle message (cast): ~p ", [self(), Message]),
    {noreply, State};
handle_cast(Message, State) ->
    ?LOG_DEBUG("[ROVER-SESS: ~p] Unhandled message (cast): ~p ", [self(), Message]),
    {noreply, State}.

handle_info(Message, State) ->
    ?LOG_DEBUG("[ROVER-SESS: ~p] Unhandled message (info): ~p ", [self(), Message]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_DEBUG("[ROVER-SESS: ~p] Terminate reason: ~p ", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_pid(#message{uid = UID} = Message) ->
    case rover_cluster:get_actor_pid({?SESS_ACTOR, UID}) of
	{ok, PID} ->
	    PID;
	_ ->
	    {ok, PID} = gen_server:start(?MODULE, [Message], []),
	    PID
    end.
