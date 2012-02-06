-module(bio_erser).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
    %ppool_supersup:start_link().
    {error, "Not implemented yet."}.
 
stop(_State) ->
    ok.

