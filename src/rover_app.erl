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
    ExternalPort = 5151,
    ExternalBacklog = 256,
    ExternalSockPool = 100,
    ExternalKeepalive = false, 
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
