-module(rover_uda).

-behaviour(gen_server).

%% API
-export([ start/0
	, start_link/0
	, send_data/2
	, recev_data/2
	, flush_inbox/1
	, open/1
	, close/1]).

-include("rover.hrl").
-include("message.hrl").

%% gen_server callbacks
-export([ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	, format_status/2]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 300).
-define(DELAY, 10).

-record(state, { conn_pid :: pid()
	       , port :: integer()
	       , host :: string()
	       , path :: string()
	       , ref :: string()
               , sequence :: integer()
               , inbox :: map()
               , opts :: list()
	       , tid = 1 :: non_neg_integer()
	       }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> {ok, pid()}.
start() ->
    gen_server:start(?MODULE, [], []).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec send_data(pid(), object()) -> {ok, object()} | {error, timeout}.
send_data(Pid, Obj) ->
    {ok, TID} = gen_server:call(Pid, {send_data, Obj}),
    recev_data(Pid, TID).

-spec recev_data(pid(), integer()) -> {ok, object()} | {error, timeout}.
recev_data(Pid, TrackingID) ->
    T1 = ?NOW_MILLI(),
    case gen_server:call(Pid, {recev_data, TrackingID}) of
        {ok, Resp} ->
            {ok, Resp};

        {error, not_found} ->
            T2 = ?NOW_MILLI(),
            Delta = T2 - T1,
            if Delta > ?TIMEOUT ->
                    {error, timeout};
               true ->
                    timer:sleep(?DELAY),
                    recev_data(Pid, TrackingID)
            end
    end.

-spec flush_inbox(pid()) -> map().
flush_inbox(Pid) ->
    gen_server:call(Pid, flush_inbox).

-spec open(pid()) -> ok.
open(Pid) ->
    gen_server:call(Pid, open).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:call(Pid, close).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, Port} = rover_conf:get('ws.port'),
    {ok, Host} = rover_conf:get('ws.test.host'),
    {ok, Path} = rover_conf:get('ws.path'),
    {ok, _Timeout} = rover_conf:get('ws.conn.idle.timeout'),

    HttpOpts = #{ keepalive => 1000000, version => 'HTTP/1.1'},
    Opts = #{http_opts => HttpOpts, protocols => [http], retry => 5},

    {ok, ConnPid} = gun:open(Host, Port, Opts),

    {ok, #state{ conn_pid= ConnPid
	       , port = Port
	       , host = Host
	       , path = Path
	       , sequence = 0
	       , inbox = #{}
	       , opts = Opts
	       }
    }.

handle_call( { send_data, #object{} = Obj }
	   , _From
	   , #state{ conn_pid = ConnPid
		   , tid = TID 
		   } = State
	   ) ->
    {ok, Frame} = rover_json_codec:encode(Obj#object{ tid = TID
						    , type = request
						    }
					 ),
    gun:ws_send(ConnPid, {text, Frame}),
    {reply, {ok, TID}, State#state{tid = TID + 1}};

handle_call({recev_data, TrackingID}, _From, #state{inbox = Inbox} = State) ->
    case maps:find(TrackingID, Inbox) of
        {ok, Resp} ->
            NewInbox = maps:remove(TrackingID, Inbox),
            NewState = State#state{inbox = NewInbox},
            Reply = {ok, Resp},
            {reply, Reply, NewState};
        error ->
            Reply = {error, not_found},
            {reply, Reply, State}
    end;

handle_call(flush_inbox, _From, State) ->
    Reply = State#state.inbox,
    NewState = State#state{inbox = #{}},
    {reply, Reply, NewState};

handle_call(close, _From, State) ->
    ok = gun:close(State#state.conn_pid),
    {reply, ok, State};

handle_call(open, _From, #state{host = Host, port = Port, opts = Opts} = State) ->
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    Reply = ok,
    NewState = State#state{conn_pid = ConnPid},
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Request, State) ->
    ?LOG_DEBUG("cast ==> ~p", [Request]),
    {noreply, State}.

handle_info({gun_up, ConnPid, _},
            #state{conn_pid = ConnPid, path = Path} = State) ->
    Ref = gun:ws_upgrade(ConnPid, Path, []),
    NewState = State#state{ref = Ref},
    {noreply, NewState};

handle_info({gun_down, ConnPid, _, _Reason, _, _},
            #state{conn_pid = ConnPid} = State) ->
    {noreply, State};

handle_info({gun_ws, ConnPid, Ref, {text, PlainMessage}},
            #state{ conn_pid = ConnPid
		  , ref = Ref
		  , sequence = _RequestID
		  , inbox = Inbox} = State) ->
    {ok, Message} = rover_json_codec:decode(PlainMessage),
    TrackingID = Message#object.tid,
    NewInbox = Inbox#{TrackingID => Message},
    NewState = State#state{inbox = NewInbox},
    {noreply, NewState};

handle_info({gun_error, ConnPid, {badstate, _}}, #state{conn_pid = ConnPid, path = Path} = State) ->
    Ref = gun:ws_upgrade(ConnPid, Path, []),
    NewState = State#state{ref = Ref},
    {noreply, NewState};

handle_info({gun_upgrade, _ConnPid, _Ref, _, _}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_DEBUG("info ==> ~p,~n conn_state: ~p", [Info, State]),
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
