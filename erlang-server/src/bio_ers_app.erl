-module(bio_ers_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
    bio_ers_sup:start_link().
 
stop(_State) ->
    ok.

