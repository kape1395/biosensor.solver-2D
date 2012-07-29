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
%%  @see bio_ers_queue_mifcl2
%%
-module(bio_ers_queue_mifcl2_sup).
-behaviour(supervisor).
-export([start_link/2, create_ssh_sup/4]). % API
-export([init/1]). % Callbacks
-include("bio_ers_queue_mifcl2.hrl").

%% =============================================================================
%%  API functions.
%% =============================================================================

%%
%%  @doc Start and link this supervisor.
%%  `Name' is used to register the queue process and
%%  `External' is used as a configuration in an external form (see {@link bio_ers_queue_mifcl2}).
%%
-spec start_link({local, atom()}, {}) -> {ok, pid()} | term().
start_link(Name, ExternalCfg) ->
    supervisor:start_link(?MODULE, {Name, ExternalCfg}).


%%
%% @doc Create SSH_SUP and pass the queue PID to it.
%%
-spec create_ssh_sup(pid(), atom(), #part_cfg{}, pid()) -> {ok, pid()} | term().
create_ssh_sup(Supervisor, Name, PartCfg, Queue) when is_record(PartCfg, part_cfg)->
    SSH = bio_ers_queue_mifcl2_ssh_sup,
    SSHSpec = {
        {ssh_sup, Name},
        {SSH, start_link, [PartCfg, Queue]},
        permanent, brutal_kill, supervisor, [SSH]
    }, 
    supervisor:start_child(Supervisor, SSHSpec).



%% =============================================================================
%%  Callbacks.
%% =============================================================================

%%
%%  @doc Configures this supervisor (callback).
%%
init({Name, ExternalCfg}) ->
    QUE = bio_ers_queue_mifcl2,
    QUESpec = {queue, {QUE, start_link, [Name, ExternalCfg, self()]}, permanent, brutal_kill, worker, [QUE]}, 
    {ok, {{one_for_all, 1, 60}, [QUESpec]}}.

