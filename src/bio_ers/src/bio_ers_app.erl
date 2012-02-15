-module(bio_ers_app).
-behaviour(application).
-export([startup/0, shutdown/0]).
-export([start/2, stop/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Interface functions.
%%

%%
%%  Start the application from the command line.
%%  Example:
%%      erl -pa ebin -s bio_ers_app startup
%%
startup() ->
    application:start(bio_ers).

%%
%%  Sutdown the application.
%%
shutdown() ->
    application:stop(bio_ers).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Callbacks
%%

start(normal, _Args) ->
    bio_ers_sup:start_link().
 
stop(_State) ->
    ok.

