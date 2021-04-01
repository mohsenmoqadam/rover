%% -*- mode:erlang -*-

-ifndef(HEADER_ROVER_MESSAGE).
-define(HEADER_ROVER_MESSAGE, true).

-record(message, { conn :: pid() 
		 , sess :: pid()
		 , aid :: aid()
		 , uid :: uid()
		 , path :: path()
		 , nodes :: list() 
		 , object :: object()
		 , kill_conn = false :: true | false
		 }
       ).

-record(object, { name  :: bitstring()
		, tid :: tid()
		, body :: map()
		, type :: error | request | reply | reflect | signal | push
		}).

-type message() :: #message{}.
-type object() :: #object{}.
-type path() :: input | output.
-type object_bin() :: bitstring().
-type tid() :: non_neg_integer().

-endif.
