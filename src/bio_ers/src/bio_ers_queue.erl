-module(bio_ers_queue).
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]). % Callbacks


%%
%%  This module defined bio_ers_queue behaviour and therefore defines an interface
%%  for all the queue implementations.
%%

%%
%%  Interface for the callback module.
%%
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: term()) ->
    term().

-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(), Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

-callback handle_submit(Simulation :: term()) -> ok.
-callback handle_cancel(Simulation :: term()) -> ok.
-callback handle_delete(Simulation :: term()) -> ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

%%
%%
%%
init(_Args) ->
    {ok, []}.

%%
%%
%%
terminate(_Reason, _State) ->
    ok.

%%
%%  Sync calls.
%%
handle_call(_Msg, _From, State) ->
    {stop, badarg, State}.

%%
%%  Async calls.
%%
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%%
%%
handle_info(_, State) ->
    {noreply, State}.

%%
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

