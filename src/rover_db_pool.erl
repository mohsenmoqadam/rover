-module(rover_db_pool).

-export([start/6]).

-define(POOL_NAME, db_worrkers_pool).

start( PoolSize
     , DatabaseIP
     , DatabaseName
     , DatabaseUser
     , DatabasePass
     , ConnectionRetryInterval) ->
    {ok, _} = toveri:new(?POOL_NAME, PoolSize),    
    MFA = { rover_db_worker
	  , start_link
	  , [ DatabaseIP
	    , DatabaseName
	    , DatabaseUser
	    , DatabasePass
	    , ConnectionRetryInterval
	    ]
	  },
    [ begin
	  ok = toveri:add_child(?POOL_NAME, MFA)
      end || _ <- lists:seq(1, PoolSize)
    ],

    ok. 
