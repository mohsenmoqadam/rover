-module(rover_db_mems).

-export([ add_user/1
	]).

-include("rover.hrl").


%% =================================================================================
%% ========= SP: rover_db.sp_add_user | SPC: 10001
%% =================================================================================
-spec add_user(pno()) ->
	  {ok, new_user | old_user, uid()} | {error, replicated_global_id | unknown}.
add_user(Phone) ->
    Query = <<"CALL rover_db.sp_add_user(?)">>,
    Params = [Phone],
    HandleResult = fun({ok, _, [[10001, 1]]}) ->
			   %% === Name: sp_add_user 
			   %% === SPC: 10001
			   %% === RC: 1 -> Database roolback
			   {error, unknown};    
		      ({ok, _, [[10001, 2, UID]]}) ->
			   %% === Name: sp_add_user 
			   %% === SPC: 10001
			   %% === RC: 2 -> New User
			   {ok, new_user, UID};
		      ({ok, _, [[10001, 3, UID]]}) ->
			   %% === Name: sp_add_user 
			   %% === SPC: 10001
			   %% === RC: 3 -> Old User
			   {ok, old_user, UID};
		      (_) ->
			   {error, unknown}
		   end,
    rover_db_query:do(Query, Params, HandleResult).

%% =================================================================================
%% ========= SP: xxx.yyy | SPC: 10002
%% =================================================================================

%% =================================================================================
%% ========= do duery
%% =================================================================================     

