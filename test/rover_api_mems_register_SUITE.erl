-module(rover_api_mems_register_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("rover.hrl").
-include("message.hrl").

suite() ->
    [{timetrap,{seconds,60}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(cowlib),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(toveri),
    {ok, _} = application:ensure_all_started(tecipe),
    {ok, _} = application:ensure_all_started(syn),
    {ok, _} = application:ensure_all_started(rover),

    %% === start client
    {ok, UDA1} = rover_uda:start(),

    %% === Simple Delay
    timer:sleep(1000),
    
    [ {uda1, UDA1}
    ].

end_per_suite(Config) ->
    UDA1 = ?config(uda1, Config),

    %% === stop and close clients
    ok = rover_uda:close(UDA1),
    
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [ t1_registeration
    ].

t1_registeration(Config) ->
    UDA = ?config(uda1, Config),
    
    %% === Get Registration Token
    GetRegistrationToken = 
	#object{ name = get_registration_token
	       , body = #{ device_unique_id => <<"---DUID---">>
			 , device_os_type => <<"ANDROID">>
			 , device_os_version => <<"---OSV---">>
			 , app_version => 010000
			 , device_name => <<"---DNAME---">>
			 , device_manufacture => <<"---DMNF---">>
			 , device_cpu_architacture => <<"---DCPU---">>
			 }
	       },
    {ok, RegistrationToken} = rover_uda:send_data(UDA, GetRegistrationToken),
    %RegistrationToken = maps:get(<<"registration_token">>, RegistrationToken),

    ok.
