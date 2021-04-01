%% -*- mode:erlang -*-

-ifndef(HEADER_ROVER_CONN).
-define(HEADER_ROVER_CONN, true).

-record(conn, { peer_ip = undefined :: list()
	      , uid = undefined :: uid()
	      , aid = undefined :: aid()
	      , conn = undefined :: pid()
	      , sess = undefined :: pid()
	      , registration_info :: registration_info()
	      , cnt_decoder_error = 0
	      , cnt_unknown_object = 0
	      , cnt_error = 0
	      , cnt_request = 0 
	      , cnt_reply = 0
	      , cnt_reflect = 0
	      , cnt_signal = 0
	      , cnt_push = 0
	      }
       ).

-record(registration_info, { app_version :: non_neg_integer()
			   , device_cpu_architecture  :: bitstring()
			   , device_manufacture :: bitstring()
			   , device_name :: bitstring()
			   , device_unique_id :: bitstring()
			   , device_os_type :: bitstring()
			   , device_os_version :: bitstring()
			   , registration_token :: bitstring()
			   , verification_token :: non_neg_integer()
			   , vericication_token_resend = 0 :: non_neg_integer()
			   }).

-type conn() ::  #conn{}.
-type registration_info() :: #registration_info{}.

-endif.
