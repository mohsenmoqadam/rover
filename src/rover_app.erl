%%%-------------------------------------------------------------------
%% @doc rover public API
%% @end
%%%-------------------------------------------------------------------

-module(rover_app).

-behaviour(application).

%% Application callbacks
-export([ start/2
	, stop/1
	]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% === Initialize Database
    {ok, PoolSize} = rover_conf:get('db.pool.size'),
    {ok, DatabaseIP} = rover_conf:get('db.ip'),
    {ok, DatabaseName} = rover_conf:get('db.name'),
    {ok, DatabaseUser} = rover_conf:get('db.user'),
    {ok, DatabasePass} = rover_conf:get('db.pass'),
    {ok, ConnectionRetryInterval} = rover_conf:get('db.conn.retry.interval'),
    ok = rover_db_pool:start( PoolSize
			    , DatabaseIP
			    , DatabaseName
			    , DatabaseUser
			    , DatabasePass
			    , ConnectionRetryInterval),

    %% === Initialize IoT Socket
    {ok, ExternalPort} = rover_conf:get('iot.socket.port'),
    {ok, ExternalBacklog} = rover_conf:get('iot.socket.backlog'),
    {ok, ExternalSockPool} = rover_conf:get('iot.socket.pool'),
    {ok, ExternalKeepalive} = rover_conf:get('iot.socket.keepalive'),
    TecipeExternalListenerOpts = [ {monitor, true}
				 , {pool, ExternalSockPool}
				 ],
    TecipeExternalTransportOpts = [ binary
				  , {keepalive, ExternalKeepalive}
				  , {backlog, ExternalBacklog}
				  , {reuseaddr, true}
				  ],
    {ok, _ExternalPid} = tecipe:start_listener( iot_nodes_conn
					      , ExternalPort
					      , {rover_conn, start, []}
					      , TecipeExternalListenerOpts
					      , TecipeExternalTransportOpts
					      ),   
    
    %% === Initialize Mobile Application Socket
    {ok, WsIp0} = rover_conf:get('ws.host'),
    {ok, WsIp} = inet:parse_ipv4_address(WsIp0),
    {ok, WsPort} = rover_conf:get('ws.port'),
    {ok, WsPath} = rover_conf:get('ws.path'),
    {ok, WsPoolSize} = rover_conf:get('ws.pool.size'),
    {ok, WsConnIdleTimeout} = rover_conf:get('ws.conn.idle.timeout'),
    WsDispatch = cowboy_router:compile([{'_', [{ WsPath
					       , rover_app_conn
					       , [WsConnIdleTimeout]}
					      ]}
				       ]),
    {ok, _} = cowboy:start_clear(websockets, WsPoolSize, [ {ip, WsIp}
							 , {port, WsPort}
							 ],
				 #{ env => #{dispatch => WsDispatch}
				  , http10_keepalive => false
				  }),

    rover_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
