-module(tls).

-export([client/0, iwf_client/0, client_with_wrong_server_certificate/0, server/0]).

%{crypto_gateway, [
%    {ip_address, "10.140.192.25"},
%    {port_number, 9000},
%    {transport, tls},
%    {tls_certfile, "/usr/app/iwf/certs/iwf.cert.pem"},
%    {tls_keyfile, "/usr/app/iwf/certs/private/iwf.key.pem"},
%    {tls_passphrase, "/usr/app/iwf/certs/private/iwf.key.passphrase"},
%]},

-define(SERVER_ADDRESS, "localhost").
%-define(SERVER_ADDRESS, "10.140.192.25").
-define(PORT, 10021).
%-define(PORT, 9000).
-define(CONNECT_TIMEOUT, infinity).

-define(PASSPHRASE, "").
%-define(PASSPHRASE, "MBddfR6tCOk9sEgaKPXSHF9Lg9WMgX8vAnBBNoCk6loTe2jSYzptc2VOphlguyEG+ThZSvDExsGyl257tSjP4FQ8tGchMQuDEOTxrQXYTyOb+2RFDiSaggpcRvHg0+ts67FkFRIH8MEdtNiDb/CZ3E4icUr/E3jSpzRgSK4p/A+bfLNEyruDObCC3eqSBmqrFGcL3futb806CoLgy8gmodkGMlYyrG0sHeHjF1BqnHfZHhtc2VW18OGgMw0pBBv2EuMzKKz2n+OswQ1P9m8axbwbsL/l//K1PzwS36NPwCOqND0Z8mcDshTMWMH8UfNrb7hmt1wfV5iYKYMefWCJbw==").

-define(CLIENT_CERTFILE, "/root/tls-handshake-excercise/certs/client.pem").
%-define(CLIENT_CERTFILE, "/usr/app/iwf/certs/iwf.cert.pem").

-define(CLIENT_KEYFILE, "certs/client.key").
%-define(CLIENT_KEYFILE, "/root/tls-handshake-excercise/certs/iwf.key").

-define(SERVER_CERTFILE, "certs/server.pem").
%-define(SERVER_CERTFILE, "/usr/app/iwf/certs/ats.cert.pem").

client() ->
    ssl:start(),
    TlsOptions = [
                  {mode, binary},
                  {active, once},
                  {verify, verify_peer},
                  {cacertfile, ?SERVER_CERTFILE}
                 ],

    {ok, Socket} = ssl:connect(?SERVER_ADDRESS, ?PORT, TlsOptions, ?CONNECT_TIMEOUT),
    Data = <<"erl ping">>,
    ok = ssl:send(Socket, Data),
    io:format("Send    : ~p~n", [Data]),
    {ok, Data} = ssl:recv(Socket, 0, infinity),
    io:format("Received: ~p~n", [Data]),
    ssl:close(Socket),
    ssl:stop().

client_with_wrong_server_certificate() ->
    ssl:start(),
    TlsOptions = [
                  {mode, binary},
                  {active, false},
                  {verify, verify_peer},
                  {cacertfile, "certs/invalid.pem"}
                 ],

    {ok, Socket} = ssl:connect(?SERVER_ADDRESS, ?PORT, TlsOptions, ?CONNECT_TIMEOUT),
    Data = <<"erl ping">>,
    ok = ssl:send(Socket, Data),
    io:format("Send    : ~p~n", [Data]),
    {ok, Data} = ssl:recv(Socket, 0, infinity),
    io:format("Received: ~p~n", [Data]),
    ssl:close(Socket),
    ssl:stop().

iwf_client() ->

    io:format("Trying to connect~n", []),

    TlsOptions = [
                   {mode, binary},
                   {active, false},
                   {tos, 224},
                   %{versions, tls_versions()},
                   %{ciphers, tls_ciphers()},
                   {certfile, ?CLIENT_CERTFILE},
                   {keyfile, ?CLIENT_KEYFILE},
                   %{password, ?PASSPHRASE},

                   %{verify, verify_none},
                   {verify, verify_peer},
                   %{verify_fun, fingerprint_verify_fun(SERVER_CERTFILE)},
                   {cacertfile, ?SERVER_CERTFILE},

                   {keepalive, true},
                   {nodelay, true}],

    ssl:start(),
    {ok, Socket} = ssl:connect(?SERVER_ADDRESS, ?PORT, TlsOptions, ?CONNECT_TIMEOUT),
    %%timer:sleep(5 * 1000),
    %%ok = ssl:send(Socket, "foo2"),

    %%io:format("Sleeping...~n", []), 
    %%timer:sleep(1 * 1000),

    case ssl:recv(Socket, 0, 1 * 1000) of
        {ok, Data} -> io:format("Client received: ~p~n", [Data]);
        {error, closed} -> io:format("Socket closed~n", []);
        {error, Reason} -> io:format("Error, reason: ~p~n", [Reason])
    end,

    %%io:format("Receiving...~n", []), 
    %%{ok, Data} = ssl:recv(Socket, 1024, infinity),
    %%io:format("Client received: ~p~n", [Data]),

    io:format("Sleeping...~n", []), 
    timer:sleep(15 * 1000),
    io:format("End of sleeping...~n", []), 

    ssl:close(Socket),
    ssl:stop().

server() ->
    ssl:start(),

    TlsOptions = [{certs_keys, [#{certfile => "certs/server.pem",
                                  keyfile => "certs/server.key"}]},
                  {reuseaddr, true}
                 ],

    {ok, ListenSocket} = ssl:listen(10022, TlsOptions),
    listen(ListenSocket).

listen(ListenSocket) ->
    io:format("Server ready~n", []),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
    {ok, Socket} = ssl:handshake(TLSTransportSocket),
    receive
        {ssl, Socket, Data} ->
            io:format("Server received: ~p~n", [Data]),
            ok = ssl:send(Socket, Data)
    end,
    ssl:shutdown(Socket, read_write),
    ssl:close(Socket),
    timer:sleep(2 * 1000),
    listen(ListenSocket).

