-module(rover_db_worker).

-behaviour(gen_server).

%% API
-export([ start_link/5
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

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link( DatabaseIP
	  , DatabaseName
	  , DatabaseUser
	  , DatabasePass
	  , ConnectionRetryInterval) ->
    gen_server:start_link( ?MODULE
			 , [ DatabaseIP
			   , DatabaseName
			   , DatabaseUser
			   , DatabasePass
			   , ConnectionRetryInterval
			   ]
			 , []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ DatabaseIP
     , DatabaseName
     , DatabaseUser
     , DatabasePass
     , ConnectionRetryInterval
     ]) ->
    ?LOG_DEBUG("New DB-Worker Created (Pid: ~p, DB-IP: ~p, BD-Name: ~p, DB-User: ~p, DB-Pass: ~p, Retry-Int: ~p)"
	      , [ self()
		, DatabaseIP
		, DatabaseName
		, DatabaseUser
		, DatabasePass
		, ConnectionRetryInterval
		]
	      ),
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(Task, _From, State) ->
    ?LOG_DEBUG("New Task: ~p in: ~p", [Task, self()]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

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
