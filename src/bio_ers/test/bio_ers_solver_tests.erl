%
% Copyright 2012 Karolis Petrauskas
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
-module(bio_ers_solver_tests).
-include_lib("eunit/include/eunit.hrl").
-export([start/0]).
-include("bio_ers_solver.hrl").

%
% P = erlang:open_port({spawn_executable, "priv/bio_ers_solver_port"}, [{packet, 2}, use_stdio, exit_status, binary]).
% erlang:port_command(P, erlang:term_to_binary({test, 123})).
% erlang:port_close(P).
%
%   READ: count=2 good=1 bad=0 eof=0 fail=0
%   DATA: 0 ''
%   DATA: 9 '       '
%   READ: count=9 good=1 bad=0 eof=0 fail=0
%   DATA: ffffff83 '�'
%   DATA: 6b 'k'
%   DATA: 0 ''
%   DATA: 5 ''
%   DATA: 6c 'l'
%   DATA: 61 'a'
%   DATA: 62 'b'
%   DATA: 61 'a'
%   DATA: 73 's'
%   MSG: size=9
%
% file:read_file("test/bio_ers_model_tests-AllElems.xml")

-ifdef(VALGRIND).
-define(PORT_NAME, "test/bio_ers_solver_port-t01-valgrind").
-define(PORT_SLEEP, 1000).
-else.
-define(PORT_NAME, "test/bio_ers_solver_port-t01-proxy").
-define(PORT_SLEEP, 100).
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Test descriptions
%%
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start_test_() ->
    ?setup(fun is_started/1).

cancel_test_() ->
    ?setup(fun is_canceled_at_init/1).

suspend_test_() ->
    ?setup(fun test_suspend/1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Setup functions
%%

start() ->
    {ok, Model} = file:read_file("test/bio_ers_model_tests-CNT-2D.xml"),
    State = #solver_state_v1{model = Model, datadir = "tmp/S1", params = [
        #param{name = 'V_max', value = 12.3},
        #param{name = 'K_M', value = 3.12}
    ]},
    {ok, Pid} = bio_ers_solver:start_link(State, ?PORT_NAME),
    Pid.

stop(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> bio_ers_solver:cancel(Pid);
        false -> ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Actual tests
%%

is_started(Pid) ->
    {Status} = bio_ers_solver:status(Pid),
    [
        ?_assertEqual(init, Status),
        ?_assert(erlang:is_process_alive(Pid))
    ].

is_canceled_at_init(Pid) ->
    timer:sleep(15),
    {Status1} = bio_ers_solver:status(Pid),
    bio_ers_solver:cancel(Pid),
    timer:sleep(100), % The following call is failing without this sleep.
    Status2 = bio_ers_solver:status(Pid),
    [
        ?_assertEqual(init, Status1),
        ?_assertEqual(down, Status2),
        ?_assertNot(erlang:is_process_alive(Pid))
    ].

test_suspend(Pid) ->
    {S1} = bio_ers_solver:status(Pid), bio_ers_solver:run(Pid),     timer:sleep(?PORT_SLEEP),
    {S2} = bio_ers_solver:status(Pid), bio_ers_solver:run(Pid),     timer:sleep(?PORT_SLEEP),
    {S3} = bio_ers_solver:status(Pid), bio_ers_solver:suspend(Pid), timer:sleep(?PORT_SLEEP),
    {S4} = bio_ers_solver:status(Pid), bio_ers_solver:suspend(Pid), timer:sleep(?PORT_SLEEP),
    {S5} = bio_ers_solver:status(Pid), bio_ers_solver:run(Pid),     timer:sleep(?PORT_SLEEP),
    {S6} = bio_ers_solver:status(Pid), bio_ers_solver:suspend(Pid), timer:sleep(?PORT_SLEEP),
    {S7} = bio_ers_solver:status(Pid), bio_ers_solver:cancel(Pid),  timer:sleep(?PORT_SLEEP),
    S8 = bio_ers_solver:status(Pid),
    [
        ?_assertEqual(init, S1),
        ?_assertEqual(running, S2),
        ?_assertEqual(running, S3),
        ?_assertEqual(suspended, S4),
        ?_assertEqual(suspended, S5),
        ?_assertEqual(running, S6),
        ?_assertEqual(suspended, S7),
        ?_assertEqual(down, S8)
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions
%%


