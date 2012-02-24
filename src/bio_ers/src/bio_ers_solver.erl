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
-module(bio_ers_solver).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([running/2]).


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

init(_Args) ->
    {ok, running, {}}.


handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, "Reply", StateName, StateData}.


handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, _StateData) ->
    ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  State callbacks.
%%

running(_Event, StateData) ->
    {stop, normal, StateData}.

