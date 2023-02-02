-module(tls).

-export([connect/0, connect2/0]).

-define(SERVER_ADDRESS, "localhost").
-define(PORT, 10022).
-define(CONNECT_TIMEOUT, infinity).

%-define(CLIENT_CERTFILE, "/home/amj018/iwf-z13-certs/iwf.cert.pem"),
%-define(CLIENT_KEYFILE, "/home/amj018/iwf-z13-certs/private/iwf.key.pem"),
%-define(CLIENT_PASSPHRASE, "/home/amj018/iwf-z13-certs/private/iwf.key.passphrase"),
%-define(SERVER_CERTFILE, "/home/amj018/iwf-z13-certs/ats.cert.pem"),

-define(CLIENT_CERTFILE, "certs/client.pem").
-define(CLIENT_KEYFILE, "certs/client.key").
-define(SERVER_CERTFILE, "certs/server.pem").

connect() ->
    ssl:start(),
    {ok, Socket} = ssl:connect(?SERVER_ADDRESS, ?PORT, [{verify, verify_none}, {cacertfile, "certs/server.pem"},  {active, once}], ?CONNECT_TIMEOUT),
    %%timer:sleep(5 * 1000),
    ok = ssl:send(Socket, "foo1"),
    %%timer:sleep(15 * 1000),
    ssl:close(Socket),
    ssl:stop().

connect2() ->

    TlsOptions = [
                   {mode, binary},
                   {active, false},
                   %{tos, @tos_7},
                   %{versions, tls_versions()},
                   %{ciphers, tls_ciphers()},
                   {certfile, ?CLIENT_CERTFILE},
                   {keyfile, ?CLIENT_KEYFILE},
                   %{password, passphrase_from_file(CLIENT_PASSPHRASE)},
                  
                   {verify, verify_none},
                   %{verify_fun, fingerprint_verify_fun(SERVER_CERTFILE)},
                   {cacertfile, ?SERVER_CERTFILE},
                   
                   {keepalive, true},
                   {nodelay, true}],

    ssl:start(),
    {ok, Socket} = ssl:connect(?SERVER_ADDRESS, ?PORT, TlsOptions, ?CONNECT_TIMEOUT),
    %%timer:sleep(5 * 1000),
    ok = ssl:send(Socket, "foo2"),
    %%timer:sleep(15 * 1000),
    ssl:close(Socket),
    ssl:stop().

