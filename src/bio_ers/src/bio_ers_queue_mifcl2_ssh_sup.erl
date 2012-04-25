-module(bio_ers_queue_mifcl2_ssh_sup).
-behaviour(supervisor).
-export([start_link/0]). % API
-export([init/1]). % Callbacks

-define(SSH_CHAN_DEF, {ssh_chan,
        {bio_ers_queue_mifcl2_ssh_chan, start_link, []},
        temporary, 10, worker, [bio_ers_queue_mifcl2_ssh_chan]
    }).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    {ok, {{one_for_all, 1, 60}, [
        {ssh_mgr,
            {bio_ers_queue_mifcl2_ssh_mgr, start_link, [self(), ?SSH_CHAN_DEF]},
            permanent, brutal_kill, worker, [bio_ers_queue_mifcl2_ssh_mgr]
        }
    ]}}.
