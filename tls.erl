-module(tls).

-compile(export_all).

connect() ->
    ssl:start(),
    {ok, Socket} = ssl:connect("localhost", 10022, [{verify, verify_none}, {cacertfile, "certs/server.pem"},  {active, once}], infinity),
    %%timer:sleep(5 * 1000),
    ok = ssl:send(Socket, "foo"),
    %%timer:sleep(15 * 1000),
    ssl:close(Socket),
    ssl:stop().

