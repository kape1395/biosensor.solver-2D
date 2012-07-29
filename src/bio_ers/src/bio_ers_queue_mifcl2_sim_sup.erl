-module(bio_ers_queue_mifcl2_sim_sup).
-behaviour(supervisor).
-export([]).
-export([init/1]).
-define(SERVER, ?MODULE).

init([]) ->
    AChild = {'AName',{'AModule',start_link,[]},
	      permanent,2000,worker,['AModule']},
    {ok,{{one_for_all,0,1}, [AChild]}}.
