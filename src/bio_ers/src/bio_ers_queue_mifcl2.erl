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


%%
%%  @doc Queue implementation for delegating calculations to the "MIF cluster v2".
%%  A structure of this module is the following:
%%  ```
%%  + bio_ers_queue_mifcl2_sup.erl (main supervisor)
%%    |       Supervisor for all the queue implementation (mifcl2).
%%    |
%%    + bio_ers_queue_mifcl2.erl  (api)
%%    |       Interfafe module for all the queue implementation.
%%    |       A user should invoke functions in this module only.
%%    |
%%    +bio_ers_queue_mifcl2_ssh_sup.erl (ssh supervisor)
%%      |     Supervisor for the ssh channel.
%%      |     It is needed becase ssh channel is crashing for some reason
%%      |     after some time of inactivity.
%%      |
%%      + bio_ers_queue_mifcl2_ssh_chan.erl (ssh channel)
%%            Implementation of the communication via SSH.
%%  '''
%%
%%  The main task for this module (apart from being interface) is to store
%%  all ongoing calls for the case, if the {@link bio_ers_queue_mifcl2_ssh_chan. SSH channel}
%%  is restarted and should redo the operations, that were performed at that time.
%%
-module(bio_ers_queue_mifcl2).
-behaviour(gen_server).
-export([start/2, start_link/2, stop/1]). % API
-export([check/1, submit/2, delete/2, cancel/2, status/2, result/2]). % API
-export([get_simulation_id/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]). % Callbacks.

-include("bio_ers.hrl").
-record(state, {ssh_channel}).

%%
%%  @doc Start this module (called by {@link bio_ers_queue_mifcl2_sup}).
%%  @spec start(Supervisor, SshChildSpec) -> QueueRef
%%
start(Supervisor, SshChildSpec) ->
    gen_server:start(?MODULE, {Supervisor, SshChildSpec}, []).


%%
%%  @doc Start and link this module (called by {@link bio_ers_queue_mifcl2_sup}).
%%  @spec start_link(Supervisor, SshChildSpec) -> QueueRef
%%  @see start/2
%%
start_link(Supervisor, SshChildSpec) ->
    gen_server:start_link(?MODULE, {Supervisor, SshChildSpec}, []).


%%
%%  @doc Stop this mofule.
%%  @spec stop(QueueRef) -> ok
%%
stop(Ref) ->
    ssh_channel:cast(Ref, stop).


%%
%%  @doc Check, if this queue is operational.
%%  @spec check(QueueRef) -> ok
%%  @todo remove from API
%%
check(Ref) ->
    ssh_channel:call(Ref, check).


%%
%%  @doc Submit new simulation to this queue.
%%  @spec submit(QueueRef, Simulation) -> ok
%%
submit(Ref, Simulation) when is_record(Simulation, simulation) ->
    ssh_channel:cast(Ref, {submit_simulation, Simulation#simulation{id = get_simulation_id(Simulation)}}).


%%
%%  @doc Delete the specified simulation including its results.
%%  @spec delete(QueueRef, Simulation) -> ok
%%
delete(Ref, Simulation) ->
    ssh_channel:cast(Ref, {delete_simulation, get_simulation_id(Simulation)}).


%%
%%  @doc Cancel the specified simulation (data will not be deleted).
%%  @spec cancel(QueueRef, Simulation) -> ok
%%
cancel(Ref, Simulation) ->
    ssh_channel:cast(Ref, {cancel_simulation, get_simulation_id(Simulation)}).


%%
%%  @doc Returns status of the specified simulation.
%%  @spec status(QueueRef, Simulation) -> Status
%%
status(Ref, Simulation) ->
    ssh_channel:call(Ref, {simulation_status, get_simulation_id(Simulation)}).


%%
%%  @doc Returns results of the simulation.
%%  @spec result(QueueRef, Simulation) -> SimulationResult
%%
result(Ref, Simulation) ->
    ssh_channel:call(Ref, {simulation_result, get_simulation_id(Simulation)}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

%%
%%
%%
init({Supervisor, SshChildSpec}) ->
    self() ! {start_ssh_channel, Supervisor, SshChildSpec},
    {ok, #state{}}.
%%
%%
%%
terminate(_Reason, _State) ->
    ok.

%%
%%  Sync calls.
%%
handle_call(_Message, _From, State) ->
    {stop, badarg, State}.

%%
%%  Async calls.
%%
handle_cast(_Message, State) ->
    {noreply, State}.

%%
%%
%%
handle_info({start_ssh_channel, Supervisor, SshChildSpec}, State) ->
    {ok, Pid} = supervisor:start_child(Supervisor, SshChildSpec),
    {noreply, State#state{ssh_channel = Pid}};
handle_info(_, State) ->
    {noreply, State}.

%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%  @doc Get simulation id, or generate it.
%%  @spec get_simulation_id(Simulation | SimulationId) -> SimulationId
%%
get_simulation_id(Simulation) when is_record(Simulation, simulation) ->
    case Simulation#simulation.id of
        undefined -> bio_ers:get_id(Simulation);
        SimId     -> SimId
    end;
get_simulation_id(SimulationId) when is_list(SimulationId) ->
    SimulationId.

