-module(rover_conn_hdlr_mems).

-export([do_message/2]).

-include("rover.hrl").
-include("message.hrl").
-include("conn.hrl").

do_message(#message{ object = #object{ name = <<"get_registration_token">>
				     , body = RequestObjectBody
				     , type = <<"request">> 
				     } = RequestObject
		   } = Message
	  , State0) ->
    %% === 1. Save UDA information in state
    RegistrationToken = rover_utils:get_register_token(),
    RegistrationInfo = 
	#registration_info{ app_version = maps:get(<<"app_version">>, RequestObjectBody)
			  , device_cpu_architecture = maps:get(<<"device_cpu_architacture">>, RequestObjectBody)
			  , device_manufacture = maps:get(<<"device_manufacture">>, RequestObjectBody)
			  , device_name = maps:get(<<"device_name">>, RequestObjectBody)
			  , device_unique_id = maps:get(<<"device_unique_id">>, RequestObjectBody)
			  , device_os_type = maps:get(<<"device_os_type">>, RequestObjectBody) 
			  , device_os_version = maps:get(<<"device_os_version">>, RequestObjectBody) 
			  , registration_token = RegistrationToken
			  },
    %% === 2. Make reply object
	ReplyObject = RequestObject#object{ name = <<"registration_token">>
					  , body = #{ registration_token => RegistrationToken
						    , country_code => <<"IR">>
						    , ping_interval => 5000
						    } 
					  },

    %% === 3. Send reply object
    NewMessage = Message#message{object = ReplyObject},
    
    %% === 4. Update State
    NewState = State0#conn{registration_info = RegistrationInfo},

    %% === 5. Debug
    ?LOG_DEBUG("[ROVER-CONN-HDLR-MEMS ~p, final state: ~p]", [self(), NewState]),
    
    {reply, NewMessage, NewState}.
    
