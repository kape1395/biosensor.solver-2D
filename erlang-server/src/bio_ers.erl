-module(bio_ers).
-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([test/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Initialization.
%%

start_link() ->
    gen_server:start_link({local, bio_ers}, bio_ers, [], []).

stop() ->
    gen_server:cast(bio_ers, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Public interface.
%%

test() ->
    gen_server:call(bio_ers, test).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks.
%%

init(_Args) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

%%
%%  Sync calls.
%%
handle_call(test, _From, State) ->
    {reply, "This is a test", State}.

%%
%%  Async calls.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

