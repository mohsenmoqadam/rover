-module(rover_db_mems).

-export([ add_user/1
	]).

-include("rover.hrl").

-define(POOL_NAME, db_worrkers_pool).

%% =================================================================================
%% ========= SP: rover_db.sp_add_user | SPC: 10001
%% =================================================================================
-spec add_user(pno()) ->
	  {ok, new_user | old_user, uid()} | {error, replicated_global_id | unknown}.
add_user(Phone) ->
    Query = <<"CALL rover_db.sp_add_user(?)">>,
    Params = [Phone],
    Result = do_query(Query, Params),
    ?LOG_DEBUG("[rover_db.sp_add_user] Query Result: ~p", [Result]),
    handle_add_user_result(Result).

handle_add_user_result({ok, _, [[10001, 1]]}) ->
    %% === Name: sp_add_user 
    %% === SPC: 10001
    %% === RC: 1 -> Database roolback
    {error, unknown};    
handle_add_user_result({ok, _, [[10001, 2, UID]]}) ->
    %% === Name: sp_add_user 
    %% === SPC: 10001
    %% === RC: 2 -> New User
    {ok, new_user, UID};
handle_add_user_result({ok, _, [[10001, 3, UID]]}) ->
    %% === Name: sp_add_user 
    %% === SPC: 10001
    %% === RC: 3 -> Old User
    {ok, old_user, UID};
handle_add_user_result(_) ->
    {error, unknown}.

%% =================================================================================
%% ========= SP: xxx.yyy | SPC: 10002
%% =================================================================================

%% =================================================================================
%% ========= do duery
%% =================================================================================     
do_query(Query, Params) -> 
    {ok, Conn} = toveri:get_pid(?POOL_NAME),
    gen_server:call(Conn, {do_query, Query, Params}).

