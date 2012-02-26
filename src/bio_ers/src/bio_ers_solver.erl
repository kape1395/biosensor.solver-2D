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
-export([init/2, running/2, restarting/2, suspending/2, suspended/2]).
-include("bio_ers_solver.hrl").


-define(PORT_PROGRAM, "priv/bio_ers_solver_port").
-record(fsm_state, {state, solver, port}).


%%
%%  @todo module doc.
%%  @todo update #fsm_state.state correctly.
%%  see doc/bio_ers_uml/bio_ers_solver.svg
%%
%%  States:
%%      init
%%      running
%%      suspending
%%      suspended
%%      restarting
%%
%%  Events:
%%      solver_run
%%      solver_suspend
%%      solver_cancel (global)
%%      solver_status (global)
%%      simulation_done
%%      simulation_failed
%%      simulation_stopped
%%      simulation_state (global)
%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Public API.
%%

%%
%%  @doc Start the solver. The solver is started in the init state, waiting
%%  for a command to run the simulation.
%%
start_link(State) ->
    gen_fsm:start_link(?MODULE, State, []).

%%
%%  @doc Run the actual simulation or resume it, if the simulation was suspended.
%%
run(FsmRef) ->
    gen_fsm:send_event(FsmRef, solver_run).

%%
%%  @doc Suspend the simulation, if it is running now.
%%
suspend(FsmRef) ->
    gen_fsm:send_event(FsmRef, solver_suspend).

%%
%%  @doc Cancel the simulation and delete all the related data.
%%  Use this in the case, when the simulation is not relevant anymore.
%%
cancel(FsmRef) ->
    gen_fsm:send_all_state_event(FsmRef, solver_cancel).

%%
%%  @doc Returns current status of the solver.
%%
status(FsmRef) ->
    gen_fsm:sync_send_all_state_event(FsmRef, solver_status).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

%%
%%  @doc Initialize the FSM.
%%  @todo read process state from DB if needed.
%%
init(SolverState) when is_record(SolverState, solver_state_v1) ->
    process_flag(trap_exit, true),
    {ok, init, #fsm_state{state = init, solver = SolverState, port = undefined}}.


%%
%%  @doc The solver_cancel event can occur at any time during lifecycle
%%  of the solver.
%%
handle_event(solver_cancel, _StateName, StateData) ->
    {stop, normal, StateData#fsm_state{state = canceled}};
handle_event(simulation_state, StateName, StateData) ->
    % @todo save data.
    {next_state, StateName, StateData}.


%%
%%  @doc Synchronous query for the current status can be invoked at any time.
%%
handle_sync_event(solver_status, _From, StateName, StateData) ->
    {reply, {StateName}, StateName, StateData}.


%%
%%  @doc This function will be invoked for any other messages than normal events.
%%  The events will include:
%%      messages from the port.
%%      port exit status
%%      trap exit?
%%
handle_info({Port, {exit_status, RC}}, StateName, StateData) ->
    io:format("# handle_info: Port ~p process terminated with RC=~p at StateName=~p~n", [Port, RC, StateName]),
    % @todo check RC.
    {stop, normal, StateData#fsm_state{state = done}};
handle_info(Info, StateName, StateData) ->
    io:format("# handle_info: Info=~p StateName=~p~n", [Info, StateName]),
    {next_state, StateName, StateData}.


%%
%%  @doc Termination of the FSM. Reasons can be the following:
%%      normal   - when the solver is done with its job,
%%      shutdown - when the OTP application will be going down.
%%      _        - just in case.
%%  The final states can be the following:
%%      done     - simulation was completed successfully.
%%      canceled - when the FSM was canceled by the user,
%%      failed   - when the simulation fails (unstable scheme, etc),
%%
terminate(normal, StateName, #fsm_state{state = canceled}) ->
    error_logger:info_msg("Cancel at state=~p~n", [StateName]),
    ok;
terminate(normal, StateName, #fsm_state{state = failed}) ->
    error_logger:warning_msg("Simulation failed at state=~p~n", [StateName]),
    ok;
terminate(normal, StateName, #fsm_state{state = done}) ->
    error_logger:info_msg("Simulation done at state=~p~n", [StateName]),
    ok;
terminate(shutdown, StateName, _StateData) ->
    error_logger:info_msg("Shutdown at state=~p~n", [StateName]),
    ok;
terminate(Reason, StateName, _StateData) ->
    error_logger:error_msg(
        "Unexpected termination Reason=~p StateName=~p~n",
        [Reason, StateName]
    ),
    ok.


%%
%%  @doc Handler for a code_change. There are no plans to use it.
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  State callbacks.
%%

%%
%%  @doc Initialized solver. It can be either started or canceled.
%%
init(solver_run, SolverData = #fsm_state{solver = SolverState}) ->
    Port = start_solver_port(SolverState),
    {next_state, running, SolverData#fsm_state{state = running, port = Port}};
init(solver_suspend, StateData) ->
    {next_state, init, StateData}.


%%
%%  @doc Simulation is actually executed in this state. The solver
%%  can be suspended in this state.
%%
running(solver_run, StateData) ->
    {next_state, running, StateData};
running(solver_suspend, StateData = #fsm_state{port = Port}) ->
    stop_solver_port(Port),
    {next_state, suspending, StateData#fsm_state{state = suspended, port = undefined}};
running(simulation_done, StateData) ->
    {stop, normal, StateData#fsm_state{state = done}};
running(simulation_failed, StateData) ->
    {stop, normal, StateData#fsm_state{state = failed}};
running(simulation_stopped, StateData) ->
    % @todo Restart solver.
    {next_state, running, StateData}.

%%
%%  @doc The state restarting is considered for the cases, when
%%  someone is sending solver_run event while solver is in the
%%  suspending state.
%%
restarting(solver_run, StateData) ->
    {next_state, restarting, StateData};
restarting(solver_suspend, StateData) ->
    {next_state, suspending, StateData#fsm_state{state = suspended}};
restarting(simulation_done, StateData) ->
    {stop, normal, StateData#fsm_state{state = done}};
restarting(simulation_failed, StateData) ->
    {stop, normal, StateData#fsm_state{state = failed}};
restarting(simulation_stopped, StateData) ->
    % @todo Restart solver.
    {next_state, running, StateData}.


%%
%%  @doc The solver is requested to be suspended. Now we are waiting
%%  the solver to notify, that the simulation is stopped gracefully.
%%
suspending(solver_run, StateData) ->
    {next_state, restarting, StateData};
suspending(solver_suspend, StateData) ->
    {next_state, suspending, StateData};
suspending(simulation_done, StateData) ->
    {stop, normal, StateData#fsm_state{state = done}};
suspending(simulation_failed, StateData) ->
    {stop, normal, StateData#fsm_state{state = failed}};
suspending(simulation_stopped, StateData) ->
    {next_state, suspended, StateData}.


%%
%%  @doc The solver is already suspended completely. One can resume it.
%%
suspended(solver_run, StateData = #fsm_state{solver = SolverState}) ->
    Port = start_solver_port(SolverState),
    {next_state, running, StateData#fsm_state{port = Port}};
suspended(solver_suspend, StateData) ->
    {next_state, suspended, StateData}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper functions.
%%

%%
%%  @doc Creates and configures the solver port.
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


