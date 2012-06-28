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
%%  @private
%%  @doc Supervisor for the {@link bio_ers_queue_mifcl2} and related modules.
%%  The {@link bio_ers_queue_mifcl2. interfacing module} (api) is started by this
%%  supervisor automatically, and the second child --
%%  {@link bio_ers_queue_mifcl2_ssh_sup. ssh channel supervisor} (ssh_sup)
%%  is started dynamically, on the startup of the  the api module.
%%
%%  @see bio_ers_queue_mifcl2
%%
-module(bio_ers_queue_mifcl2_sup).
-behaviour(supervisor).
-export([start_link/0]). % API
-export([init/1]). % Callbacks

-define(SSH_SUP_DEF, {ssh_sup,
        {bio_ers_queue_mifcl2_ssh_sup, start_link, []},
        permanent, 10, supervisor, [bio_ers_queue_mifcl2_ssh_sup]
    }).

%%
%%  @doc Start and link this supervisor.
%%  @spec start_link() -> PID
%%
start_link() ->
    supervisor:start_link(?MODULE, []).

%%
%%  @doc Configures this supervisor (callback).
%%  @spec init(Args) -> SupervisorDefinition
%%
init(_Args) ->
    {ok, {{one_for_all, 1, 60}, [
        {api,
            {bio_ers_queue_mifcl2, start_link, [self(), ?SSH_SUP_DEF]},
            permanent, brutal_kill, worker, [bio_ers_queue_mifcl2]
        }
    ]}}.

