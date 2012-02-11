-module(bio_ers_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%%
%%  Start supervisor.
%%
start_link() ->
    supervisor:start_link(bio_ers_sup, []).

%%
%%  Configure supervisor.
%%
init(_Args) ->
    {ok, {{one_for_one, 1, 60}, [
        {bio_ers_mgr, {bio_ers_mgr, start_link, []}, permanent, brutal_kill, worker, [bio_ers_mgr]}
    ]}}.
