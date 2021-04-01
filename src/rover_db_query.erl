-module(rover_db_query).

-export([do/3]).

-include("rover.hrl").

-define(POOL_NAME, db_worrkers_pool).

do(Query, Params, Callback) ->
    {ok, Conn} = toveri:get_pid(?POOL_NAME),
    Result = gen_server:call(Conn, {do_query, Query, Params}),
    ?LOG_DEBUG("[rover_db.sp_add_user] Query Result: ~p", [Result]),
    Callback(Result).

    
