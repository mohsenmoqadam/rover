-module(rover_obj).

-export([ init/0
	, get_nodes/1
	]).

-include("rover.hrl").

init() ->
    ok.

-spec get_nodes(bitstring()) -> 
	  {ok, list()} | 
	  {error, unknown_object}.
get_nodes(<<"get_registration_token">>) ->
    {ok, [ {rover_conn_hdlr_mems, null}
	 ]
    };
get_nodes(_) ->
    {error, unknown_object}.

    
