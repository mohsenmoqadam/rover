-module(rover_app_conn).

-export([ init/2
	, websocket_init/1
	, websocket_handle/2
	, websocket_info/2
	, terminate/3
	]).

-include("rover.hrl").
-include("conn.hrl").
-include("message.hrl").

init(Req, [WsConnIdleTimeout]) ->
    ?LOG_DEBUG("[ROVER-CONN ~p] Open New HTTP Connection.", [self()]),
    {IP, _Port} = maps:get(peer, Req),
    State = #conn{peer_ip = inet:ntoa(IP)},
    WsOpts = #{idle_timeout => WsConnIdleTimeout},
    {cowboy_websocket, Req, State, WsOpts}.

websocket_init(State0) ->
    ?LOG_DEBUG("[ROVER-CONN ~p] Upgrade HTTP to WEBSocket.", [self()]),
    State = State0#conn{conn = self()},
    {ok, State}.

websocket_handle({text, ObjectBin}, #conn{ uid = UID
					 , aid = AID
					 , conn = Conn
					 , sess = Sess
					 , cnt_request = CntReq
					 , cnt_decoder_error = CntDecErr
					 , cnt_unknown_object = CntUko
					 } = State0) ->
    ?LOG_DEBUG("[ROVER-CONN ~p] Receive New Object: ~p, Session: ~p" ,[Conn, ObjectBin, Sess]),
    case rover_json_codec:decode(ObjectBin) of
	{ok, Object} ->
	    case rover_obj:get_nodes(Object#object.name) of
		{ok, Nodes} ->
		    Message = #message{ conn = Conn
				      , sess = Sess
				      , aid = AID
				      , uid = UID
				      , path = input
				      , nodes = Nodes
				      , object = Object
				      },
		    {ok, State1} = rover_messages:do_and_route(Message, State0),
		    NewState = State1#conn{cnt_request = CntReq + 1},
		    {ok, NewState};
		{error, unknown_object} ->
		    Message = #message{conn = Conn},
		    ok = rover_messages:unknown_request(Message),
		    NewState = State0#conn{cnt_unknown_object = CntUko + 1},
		    {ok, NewState}
		end;
	{error, decoder_error} ->
	    Message = #message{conn = Conn},
	    ok = rover_messages:malformed_request(Message),
	    NewState = State0#conn{cnt_decoder_error = CntDecErr + 1},
	    {ok, NewState}
    end; 

websocket_handle(Data, #conn{conn = Conn} = State) ->
    ?LOG_INFO("[ROVER-CONN ~p] Not-permitted, Data: ~p", [Conn, Data]),
    {stop, State}.

websocket_info( #message{ object = Object
			, path = output
			, kill_conn = KillConn
			} = _Message
	      , #conn{ conn = Conn
		     , cnt_reply = CntRep
		     , cnt_reflect = CntRef
		     , cnt_signal = CntSig
		     , cnt_push = CntPus
		     } = State
	      ) ->
    if 
	KillConn =:= true -> 
	    self() ! {stop, by_message};
	true ->
	    ok
    end,
    {ok, ObjectBin} = rover_json_codec:encode(Object),
    ?LOG_DEBUG("[ROVER-CONN ~p] Send New Object: ~p", [Conn, ObjectBin]),
    NewState = case Object#object.type of
		   reply -> State#conn{cnt_reply = CntRep + 1};
		   reflect -> State#conn{cnt_reflect = CntRef + 1};
		   signal -> State#conn{cnt_signal = CntSig + 1};
		   push -> State#conn{cnt_push = CntPus + 1}
	       end,
    {reply, [{text, ObjectBin}], NewState};
websocket_info({stop, by_message}, #conn{conn = Conn} = State) ->
    ?LOG_INFO("[ROVER-CONN ~p] Connection stopped by message.", [Conn]),
    {stop, State};
websocket_info(Info, #conn{conn = Conn} = State) ->
    ?LOG_INFO("[ROVER-CONN ~p] Unhandled message!, Message: ~p", [Conn, Info]),
    {ok, State}.

terminate(Reason, _, #conn{ uid = UID
			  , aid = AID
			  , conn = Conn
			  } = State) ->
    ?LOG_INFO( "[ROVER-CONN ~p] Terminated!, Reason: ~p, UID: ~p, AID: ~p, State: ~p"
	     , [Conn, Reason, UID, AID, State]),
    ok.

%%% ==========================================================================
%%% Private Functions
%%% ==========================================================================
