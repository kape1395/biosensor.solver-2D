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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Test descriptions
%%

basic_test_() ->
    {setup, fun start/0, fun stop/1, fun test_returns_string/1}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Setup functions
%%

start() ->
    {ok, Model} = file:read_file("test/bio_ers_model_tests-CNT-2D.xml"),
    State = #solver_state_v1{model = Model, datadir = "tmp/S1"},
    {ok, Pid} = bio_ers_solver:start_link(bio_ers_solver, State, []),
    Pid.

stop(Pid) ->
    bio_ers_solver:cancel(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Actual tests
%%

test_returns_string(_Pid) ->
    [?_assertEqual("This is a test", bio_ers:test())].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions
%%


