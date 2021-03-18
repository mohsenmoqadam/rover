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
    %% === Get Configurations
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

    rover_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
