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
%%  @type model() = #model{
%%      type = atom(),
%%      definition = any()
%%  }
%%  Describes biosensor model definition. Types can be
%%      `kpxml1_xml' for raw XML as a binary and
%%      `kpxml1_parsed' for the model transformed to tuples.
%%  Contents of the `definition' depends on the type.
%%
-record(model, {
    type,
    definition
    }).


%%
%%  @type param() = #param{
%%      name = atom(),
%%      value = number()
%%      }
%%  This type should be used for specifying simulation parameter overrides.
%%  I.e. list of such records will be passed to the solver along with the
%%  model definition when performing parameter variations.
%%
-record(param, {
    name,    % Parameter name.
    value    % Parameter value.
    }).


%%
%%  @type checkpoint() = #checkpoint{
%%      step = pos_integer(),
%%      time = float(),
%%      concentrations = binary(),
%%      listeners = [listener()]
%%      }
%%  where
%%      listener() = {Name::atom(), [{Property::atom(), Value::string()}]}
%%  The record describes solver state, from which the simulation
%%  could be resumed. Here
%%      `step' stands for simulation step number by time (in),
%%      `time' is the simulated time,
%%      `concentrations' is for storing all the species concentrations
%%          in all domains, and 
%%      `listeners' is a list of tuples describing solver listeners.
-record(checkpoint, {
    step,
    time,
    concentrations,
    listeners
    }).


%%
%%  @type output() = #output{
%%      type = atom(),
%%      name = atom(),
%%      data = {Step::pos_integer(), Time::float(), Value::term()}
%%      }
%%  Describes output produced by one output generator (usually a listener).
%%
-record(output, {
    type,
    name,
    data
    }).


%%
%%  @type simulation() = #simulation{
%%      id = string(),
%%      version = pos_integer(),
%%      model = #model{},
%%      params = [#param{}],
%%      state = atom(),
%%      checkpoints = [#checkpoint()],
%%      outputs = []
%%  }
%%  Describes one numerical simulation (experiment).
%%
-record(simulation, {
    id,
    version = 1,
    model,
    params,
    state,
    checkpoints,
    outputs
    }).


%%
%%  @type biosensor() = #biosensor{id::atom(), description::string()}
%%  Describes one biosensor, for which several models can be defined.
%%  I am not sure, wether will it be needed. It could be used for
%%  grouping simulations, but labels and series are also suitable for this. 
%%
-record(biosensor, {id, description}).

%%
%%  @type series() = #series{
%%      id = string(),
%%      comment = string(),
%%      model = #model{},
%%      default = [#param()],
%%      vectors = [[#param()]],
%%      labels = [string()]
%%      }
%%  Describes several simulations for the same biosensor model
%%  with different parameter values.
%%
-record(series, {
    id,
    comment,
    model,
    default,
    vectors,
    labels
    }).


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

