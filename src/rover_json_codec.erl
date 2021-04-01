-module(rover_json_codec).

-export([ decode/1
	, encode/1
	]).

-include("rover.hrl").
-include("message.hrl").

-spec decode(object_bin()) -> 
	  {ok, object()} | 
	  {error, unknown_object} | 
	  {error, decoder_error}.
decode(ObjectBin) ->
    try 
	Obj = jiffy:decode(ObjectBin, [return_maps, {null_term, undefined}]),
	ObjHeader = maps:get(<<"header">>, Obj),
	{ok, #object{ name = maps:get(<<"name">>, ObjHeader)
		    , type = maps:get(<<"type">>, ObjHeader)
		    , tid = maps:get(<<"tid">>, ObjHeader)
		    , body = maps:get(<<"body">>, Obj)
		    }
	} 
    catch
	_:_ ->
	    {error, decoder_error}
    end.

-spec encode(object()) -> 
	  {ok, object_bin()} | 
	  error.
encode(#object{ name = Name
	      , tid = TID
	      , type = Type
	      , body = Body
	      } = Object) ->
    {ok, jiffy:encode(#{ header => #{ name => Name
				    , type => Type
				    , tid => TID
				    }
		       , body => Body
		       }
		     )
    }.
