%% -*- mode:erlang -*-

-ifndef(HEADER_ROVER_SESS).
-define(HEADER_ROVER_SESS, true).

-record(sess, { uid :: uid() }
       ).

-define(SESS_ACTOR, session_actor).

-endif.
