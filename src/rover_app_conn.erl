-module(rover_app_conn).

-export([ init/2
	, websocket_init/1
	, websocket_handle/2
	, websocket_info/2
	, terminate/3
	]).

-include("rover.hrl").

init(Req, [WsConnIdleTimeout]) ->
    ?LOG_DEBUG("[ROVER_APP_CONN] New HTTP Socket.", []),
    State = #{},
    WsOpts = #{idle_timeout => WsConnIdleTimeout},
    {cowboy_websocket, Req, State, WsOpts}.

websocket_init(State) ->
    ?LOG_DEBUG("[ROVER_APP_CONN] Upgraded HTTP Socket to WEBSocket, pid:~p", [self()]),
    {ok, State}.

websocket_handle({text, PlainRequest}, State) ->
    ?LOG_DEBUG("[ROVER_APP_CONN] Receive New Message: ~p" ,[PlainRequest]),
    {ok, State}; 
websocket_handle(PlainRequest, State) ->
    ?LOG_INFO("[ROVER_APP_CONN] Invalid Frame => ~p", [PlainRequest]),
    {stop, State}.

websocket_info(Signal, State) ->
    ?LOG_INFO("[ROVER_APP_CONN] Unhandled message! => Message: ~p", [Signal]),
    {reply, [{text, Signal}], State}.

terminate(Reason, _, State) ->
    ?LOG_INFO("[ROVER_APP_CONN] Terminated! => Pid: ~p, Reason: ~p, State: ~p", [self(), Reason, State]),
    ok.

%%% ==========================================================================
%%% Private Function
%%% ==========================================================================
