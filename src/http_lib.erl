-module(http_lib).
-export([check_method/1]).

check_method(Method) ->
    case string:uppercase(Method) of
        <<"HEAD">> -> ok;
        <<"GET">> -> ok;
        <<"POST">> -> ok;
        _Other -> {error, 404}
    end.

check_protocol(Protocol) ->
    P = string:uppercase(Protocol),
    if 
        P == <<"HTTP/1.1">> -> ok;
        true -> error
    end.
