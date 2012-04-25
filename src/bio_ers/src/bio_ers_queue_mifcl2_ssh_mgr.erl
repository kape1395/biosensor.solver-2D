-module(bio_ers_queue_mifcl2_ssh_mgr).
-behaviour(gen_server).
-export([start/2, start_link/2]). % API
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]). % Callbacks.

-record(state, {ssh_channel}).


start(Supervisor, SshChildSpec) ->
    gen_server:start(?MODULE, {Supervisor, SshChildSpec}, []).

start_link(Supervisor, SshChildSpec) ->
    gen_server:start_link(?MODULE, {Supervisor, SshChildSpec}, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

%%
%%
%%
init({Supervisor, SshChildSpec}) ->
    self() ! {start_ssh_shannel, Supervisor, SshChildSpec},
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
handle_info({start_ssh_shannel, Supervisor, SshChildSpec}, State) ->
    {ok, Pid} = supervisor:start_child(Supervisor, SshChildSpec),
    {noreply, State#state{ssh_channel = Pid}};
handle_info(_, State) ->
    {noreply, State}.

%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


