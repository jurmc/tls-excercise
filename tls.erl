-module(tls).

-compile(export_all).

connect() ->
    ssl:start(),
    {ok, Socket} = ssl:connect("localhost", 10021, [{verify, verify_none}, {cacertfile, "domain.pem"},  {active, once}], infinity),
    ssl:send(Socket, "foo"),
    ssl:close(Socket),
    ssl:stop().

