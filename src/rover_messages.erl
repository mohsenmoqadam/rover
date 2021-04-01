-module(rover_messages).

-include("message.hrl").

-export([ unknown_request/1
	, malformed_request/1
	]).

-export([ do_and_route/2
	]).

-include("rover.hrl").
-include("message.hrl").

-spec unknown_request(message()) -> ok.
unknown_request(#message{} = Message) ->
    Object = #object{ name = unknown_request
		    , tid = 0
		    , body = #{}
		    , type = error
		    },
    send_to_uda(Message#message{ object = Object
			       , path = output
			       }
	       ),
    ok.

-spec malformed_request(message()) -> ok.
malformed_request(#message{} = Message) ->
    Object = #object{ name = malformed_request
		    , tid = 0
		    , body = #{}
		    , type = error
		    },
    send_to_uda(Message#message{ object = Object
			       , path = output
			       }
	       ),
    ok.

do_and_route(#message{nodes = [{CurrentCtrl, NextNode} | R]} = Message0
	    , State0) ->
    Message1 = Message0#message{nodes = R},
    {NewMessage, NewState} = 
	case CurrentCtrl:do_message(Message1, State0) of
	    {reply, Message2, State1} ->
		send_to_uda(Message2),
		{Message2, State1};
	    %% === TODO: REFLECT
	    %% === TODO: SIGNAL
	    {noreply, Message2, State1} ->
		{Message2, State1}
	end,
    case NextNode of
	null -> ok;
	_ -> NextNode:do_message(NewMessage)
    end,
    {ok, NewState}.	    

%%% ==========================================================================
%%% Private Functions
%%% ==========================================================================
send_to_uda(#message{conn = Conn, object = Object} = Message) ->
    Conn ! Message#message{ path = output
			  , object = Object#object{type = reply} 
			  }.
