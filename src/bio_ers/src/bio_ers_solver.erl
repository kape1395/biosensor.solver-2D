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
-export([start_link/1, run/1, suspend/1, cancel/1, status/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([init/2, running/2, suspended/2]).
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

-define(PORT_PROGRAM, "priv/bio_ers_solver_port").
-record(fsm_state, {solver, port}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Public API.
%%

start_link(State) ->
    gen_fsm:start_link(?MODULE, State, []).

run(FsmRef) ->
    gen_fsm:send_event(FsmRef, run).

suspend(FsmRef) ->
    gen_fsm:send_event(FsmRef, suspend).

cancel(FsmRef) ->
    gen_fsm:send_event(FsmRef, cancel).

status(FsmRef) ->
    gen_fsm:sync_send_all_state_event(FsmRef, status).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

init(SolverState) when is_record(SolverState, solver_state_v1) ->
    {ok, init, #fsm_state{solver = SolverState, port = undefined}}.


handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


handle_sync_event(status, _From, StateName, StateData) ->
    {reply, {StateName}, StateName, StateData};
handle_sync_event(Event, From, StateName, StateData) ->
    io:format("# handle_info: Event=~p From=~p StateName=~p~n", [Event, From, StateName]),
    {reply, undefined, StateName, StateData}.

%%
%%  This function will be invoked for any other messages than normal events.
%%  They will include:
%%      messages from the port.
%%      port exit status ...
%%
handle_info({Port, {exit_status, RC}}, StateName, StateData) ->
    io:format("# handle_info: Port ~p process terminated with RC=~p at StateName=~p~n", [Port, RC, StateName]),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    io:format("# handle_info: Info=~p StateName=~p~n", [Info, StateName]),
    {next_state, StateName, StateData}.


terminate(Reason, StateName, _StateData) ->
    io:format("# terminate: Reason=~p StateName=~p~n", [Reason, StateName]),
    ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  State callbacks.
%%

%
%   States:
%       init
%       running
%       suspending
%       suspended
%
%   Events:
%       run
%       suspend
%       cancel
%       simulation_state
%       simulation_done
%       simulation_failed
%

init(run, #fsm_state{solver = SolverState}) ->
    Port = start_solver_port(SolverState),
    {next_state, running, #fsm_state{solver = SolverState, port = Port}};
init(cancel, State) ->
    {stop, normal, State};
init(Event, State)
    io:format("# Ignoring event=~p in the state init~n"),
    {next_state, State).



running(suspend, State = #fsm_state{port = Port}) ->
    stop_solver_port(Port),
    {next_state, suspended, State#fsm_state{port = undefined}};
running(cancel, State = #fsm_state{port = Port}) ->
    stop_solver_port(Port),
    {stop, normal, State#fsm_state{port = undefined}};
running(simulation_state, StateData) ->
    % save state
    {next_state, running, StateData};
running(simulation_done, StateData) ->
    {stop, normal, StateData};
running(simulation_failed, StateData) ->
    {stop, normal, StateData}.



suspending(simulation_stopped, State) ->
    {next_state, suspended, State};
suspending(cancel, State) ->
    {stop, normal, State};
suspending(_Event, State) ->
    {next_state, suspending, State}.
    


suspended(run, #fsm_state{solver = SolverState}) ->
    Port = start_solver_port(SolverState),
    {next_state, running, #fsm_state{solver = SolverState, port = Port}};

suspended(cancel, StateData) ->
    {stop, normal, StateData}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions.
%%

%%
%%  @doc Creates and configures the solver port.
%%
%%
start_solver_port(SolverState) ->
    Port = erlang:open_port(
       {spawn_executable, ?PORT_PROGRAM},
       [{packet, 2}, use_stdio, exit_status, binary]
    ),
    erlang:port_command(Port, erlang:term_to_binary({config, self(), SolverState})),
    Port.

%%
%%  @doc Sents stop command to the solver. The solver should terminate gracefully 
%%  and send corresponding messages to this process. Those messages should be
%%  handled by handle_info/3.
%%
stop_solver_port(Port) ->
    erlang:port_command(Port, erlang:term_to_binary({stop, self()})).



