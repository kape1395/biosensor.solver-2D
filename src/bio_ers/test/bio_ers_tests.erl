-module(bio_ers_tests).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Test descriptions
%%

basic_test_() ->
    {setup, fun start/0, fun stop/1, fun test_returns_string/1}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Setup functions
%%

start() ->
    {ok, Pid} = bio_ers:start_link(),
    Pid.

stop(_Pid) ->
    bio_ers:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Actual tests
%%

test_returns_string(_Pid) ->
    [?_assertEqual("This is a test", bio_ers:test())].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions
%%


