-module(starter).
-export([start/0, stop/0]).

start() ->
    case application:load(spws) of
        ok -> case application:start(spws) of
                ok -> {start, webserver};
                {error, R} -> {error, R}
            end;
        {error, R} -> {error, R}
    end.

stop() ->
    application:stop(spws),
    application:unload(spws).