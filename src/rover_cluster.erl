-module(rover_cluster).

-export([ set_actor/2
	, get_actor_pid/1
	, get_actor_counts/1
	]).

-type actor_name() :: any().
-type actor_pid() :: pid().

-define(BACKEND, syn).

-spec set_actor(actor_name(), actor_pid()) ->
	  ok | {error, taken_name}.
set_actor(Name, PID) ->
    case ?BACKEND:register(Name, PID) of
	ok ->
	    ok;
	{error, taken} ->
	    {error, taken_name}
    end.

-spec get_actor_pid(actor_name()) ->
	  {ok, actor_pid()} | {error, not_found}.
get_actor_pid(Name) ->
    case ?BACKEND:whereis(Name) of
	undefined ->
	    {error, not_found};
	PID ->
	    {ok, PID}
    end.

-spec get_actor_counts(atom()) -> non_neg_integer().
get_actor_counts(Node) ->
    ?BACKEND:registry_count(Node).
