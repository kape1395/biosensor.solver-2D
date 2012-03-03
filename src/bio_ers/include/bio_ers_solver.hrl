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

-include("bio_ers.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Data structures for communicating with the solver port.
%%


%%
%%  @type configure_port() = #configure_port{
%%      self = pid(),
%%      model = any(),
%%      }
%%  A message sent to the biosensor simulation port to configure it
%%  and start the simulation.
%%
-record(configure_port, {
    self,
    id,
    model,
    params,
    checkpoint
    }).


%%
%%  @type stop_port() = #stop_port{
%%      self = pid()
%%      }
%%  A message sent to the biosensor simulation port to stop it gracefully.
%%
-record(stop_port, {
    self
    }).


%%
%%  @type checkpoint_port() = #checkpoint_port{
%%      self = pid()
%%  }
%%  A message sent to the biosensor simulation port to ask to make checkpoint
%%  (send add its internal data to the connected process).
%%
-record(checkpoint_port, {
    self
    }).


%%
%%  @type port_stopped() = #port_stopped{
%%      pid = pid()
%%      }
%%  This message is sent from the port before its exit,
%%  if port was asked to stop.
%%
-record(port_stopped, {
    pid
    }).


%%
%%  @type port_output() = #port_output{
%%      pid = pid()
%%      }
%%  This message is sent regularly from the port to report
%%  output data of the simulation. 
%%
-record(port_output, {pid}).


%%
%%  @type port_checkpoint() = #port_checkpoint{
%%      pid = pid()
%%      }
%%  This message is sent from port to the connected process after
%%  the port was asked to produce the checkpoint. This message is
%%  used to transver actual data.
%%
-record(port_checkpoint, {pid}).


%%  @deprecated
%%  @type solver_state_v1() = #solver_state_v1{
%%      state = atom(),
%%      model = binary(),
%%      datadir = string(),
%%      params = [#param{}],
%%      concentrations = binary() | atom(),
%%      response = term()
%%      }.
%%  This biosensor state record basically matches the parameters for invoking
%%  bio-solver command. The records should be versioned, I am not sure yet,
%%  shat approach should be taken to handle that.
%%
-record(solver_state_v1, {
    state,                 % State of the solver, corresponds to the state of the FSM.
    model,                 % Model description XML as one binary: {ok, Model) = file:read_file("file").
    datadir,               % String represending data directory for the simulations.
    params = [],           % List of parameters of type #param{}.
    concentrations,        % Contents of a concentrations file or empty.
    response               % Simulated response of the biosensor.
    }).

