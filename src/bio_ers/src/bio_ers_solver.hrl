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
%%  @type param() = #param{
%%      name = atom(),
%%      value = number()
%%      }.
%%
-record(param, {
    name,
    value
    }).

%%
%%  This biosensor state record basically matches the parameters for invoking
%%  bio-solver command. The records should be versioned, I am not sure yet,
%%  shat approach should be taken to handle that.
%%
-record(solver_state_v1, {
    model,                 % Model description XML as one binary: {ok, Model) = file:read_file("file").
    datadir,               % String represending data directory for the simulations.
    params = [],           % List of symbols.
    concentrations = <<>>  % Contents of a concentrations file or empty.
    }).

