-module(rover_db_worker).

-behaviour(gen_server).

%% API
-export([ start_link/5]).

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

-record(state, { db_ip     :: string()
	       , db_name   :: string()
	       , db_user   :: string()
	       , db_pass   :: string()
	       , conn      :: pid()
	       , conn_mntr :: reference()
	       , conn_retry_interval :: non_neg_integer()
	       }
       ).

%%%===================================================================
%%% API
%%%===================================================================

start_link( DataBaseIP
	  , DataBaseName
	  , DatabaseUsr
	  , DataBasePass
	  , ConnectionRetryInterval) ->
    gen_server:start_link( ?MODULE
			 , [ DataBaseIP
			   , DataBaseName
			   , DatabaseUsr
			   , DataBasePass
			   , ConnectionRetryInterval], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ DatabaseIP
     , DatabaseName
     , DatabaseUser
     , DatabasePass
     , ConnRetryInterval]) ->
    process_flag(trap_exit, true),
    
    gen_server:cast(self(), connect),

    {ok, #state{ db_ip = DatabaseIP
	       , db_name = DatabaseName
	       , db_user = DatabaseUser
	       , db_pass = DatabasePass
	       , conn_retry_interval = ConnRetryInterval 
	       }
    }.

handle_call({do_query, Query, Params}, _From, #state{conn = Conn} = State) ->
    Reply = mysql:query(Conn, Query, Params),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(connect, #state{ db_ip = DBIP
			   , db_name = DBName
			   , db_user = DBUser
			   , db_pass = DBPass} = State) ->
    {ok, Conn} = mysql:start_link( [ {host, DBIP}
				   , {user, DBUser}
				   , {keep_alive, true}
				   , {password, DBPass}
				   , {database, DBName}
				   ]
				 ),
    ConnMntr = erlang:monitor(process, Conn),
    NewState = State#state{ conn = Conn
			  , conn_mntr = ConnMntr},
    ?LOG_INFO("[DB-WORKER] Connected to DB: ~p", [self()]),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', ConnMntr, process, Conn, Reason},
            #state{ conn = Conn
		  , conn_mntr = ConnMntr
		  , conn_retry_interval = ConnRetryInterval
                  } = State) ->
    ?LOG_ERROR( "[DB-WORKER] Disconnected From DB with reason: ~p"
	      , [Reason]),
    erlang:demonitor(ConnMntr),
    erlang:send_after(ConnRetryInterval, self(), do_connect),
    NewState = State#state{conn = undefined},
    {noreply, NewState};

handle_info(do_connect, State) ->
    gen_server:cast(self(), connect),
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_ERROR( "[DB-WORKER] Unhandled Info: ~p"
	      , [Info]),
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
