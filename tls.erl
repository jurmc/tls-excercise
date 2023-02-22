-module(tls).

% TLS Server
-export([server/0]).
% Clients that can connect to the local TLS server (defined in this module too)
-export([client_certificate_valid/0, client_certificate_invalid/0]).
% Client that can connect to the ATS TLS server:
% - you need to supply password to client key somehow)
% - if you'd like to have server side verification failing, then replace client fingerprint in the ATS config
-export([iwf_client/0]).

-define(LOCAL_SERVER_ADDRESS, "localhost").
-define(IWF_SERVER_ADDRESS, "10.140.192.25").

-define(PORT, 9000).
-define(CONNECT_TIMEOUT, infinity).

-define(LOCAL_CLIENT_CERT, "/home/amj018/tmp/otp_certs/client/cert.pem").
-define(LOCAL_CLIENT_KEY, "/home/amj018/tmp/otp_certs/client/key.pem").
-define(IWF_CLIENT_CERT, "/usr/app/iwf/certs/iwf.cert.pem").
-define(IWF_CLIENT_KEY, "/usr/app/iwf/certs/private/iwf.key.pem").
%-define(CLIENT_PASSWORD, "").

-define(LOCAL_SERVER_CERT, "/home/amj018/tmp/otp_certs/server/cert.pem").
-define(LOCAL_SERVER_KEY, "/home/amj018/tmp/otp_certs/server/key.pem").

-define(LOCAL_INVALID_CERT, "/home/amj018/tls-excercise/certs/invalid.pem").
-define(LOCAL_INVALID_KEY, "/home/amj018/tls-excercise/certs/invalid.key").

-define(LOCAL_CACERTS, "/home/amj018/tmp/otp_certs/cacerts.pem").
-define(ATS_CACERTS, "/usr/app/iwf/certs/ats.cert.pem").

%
% Exported functions
%

server() ->
    server(?LOCAL_SERVER_CERT, ?LOCAL_SERVER_KEY, ?LOCAL_CACERTS).

client_certificate_valid() ->
    client(?LOCAL_CLIENT_CERT, ?LOCAL_CLIENT_KEY, ?LOCAL_CACERTS).

client_certificate_invalid() ->
    client(?LOCAL_INVALID_CERT, ?LOCAL_INVALID_KEY,  ?LOCAL_CACERTS).

iwf_client() ->
    client(?IWF_CLIENT_CERT, ?IWF_CLIENT_KEY, ?ATS_CACERTS).

%
% Internal functions
%

client(ClientCert, ClientKey, Cacerts) ->
    ssl:start(),
    TlsOptions = [
                  {mode, binary},
                  {active, false},
                  {tos, 224},
                  {versions, ['tlsv1.2']},
                  %{ciphers, ???},
                  {certfile, ClientCert},
                  {keyfile, ClientKey},
                  %{password, ?CLIENT_PASSWORD},
                  {certs_keys, [#{certfile => ClientCert, keyfile => ClientKey}]},
                  {verify_fun, {fun(_,{bad_cert, _} = Reason, _) ->
                                        %io:format("line: ~p~n", [?LINE]),
                                        {fail, Reason};
                                   (_,{extension, _}, UserState) ->
                                        %io:format("line: ~p~n", [?LINE]),
                                        {unknown, UserState};
                                   (_, valid, UserState) ->
                                        %io:format("line: ~p~n", [?LINE]),
                                        {valid, UserState};
                                   (_, valid_peer, UserState) ->
                                        %io:format("line: ~p~n", [?LINE]),
                                        {valid, UserState}
                                end, []}
                  },
                  {keepalive, true},
                  {nodelay, true}, %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                  {cacertfile, Cacerts},
                  {verify, verify_peer},
                  {server_name_indication, disable}
                 ],

    io:format("Before ssl:connect~n"),
    {ok, Socket} = ssl:connect(?LOCAL_SERVER_ADDRESS, ?PORT, TlsOptions, ?CONNECT_TIMEOUT),
    io:format("After ssl:connect~n"),
    {ok, Data} = ssl:recv(Socket, 0, infinity),
    io:format("Received: ~p~n", [Data]),
    ok = ssl:send(Socket, Data),
    io:format("Send    : ~p~n", [Data]),
    timer:sleep(5 * 1000),
    ssl:close(Socket),
    ssl:stop().

server(ServerCert, ServerKey, Cacerts) ->
    ssl:start(),
    TlsOptions = [{certs_keys, [#{certfile => ServerCert,
                                  keyfile => ServerKey}]},
                  {reuseaddr, true},
                  {versions, ['tlsv1.2']},
                  {cacertfile, Cacerts},

                  {verify, verify_peer},
                  {verify_fun, {fun(_,{bad_cert, _} = Reason, _) ->
                                        %io:format("line: ~p~n", [?LINE]),
                                        {fail, Reason};
                                   (_,{extension, _}, UserState) ->
                                        %io:format("line: ~p~n", [?LINE]),
                                        {unknown, UserState};
                                   (_, valid, UserState) ->
                                        %io:format("line: ~p~n", [?LINE]),
                                        {valid, UserState};
                                   (_, valid_peer, UserState) ->
                                        %io:format("line: ~p~n", [?LINE]),
                                        {valid, UserState}
                                end, []}
                  }
                 ],

    HandshakeTlsOptions = [
                           [{verify, verify_peer},
                            {fail_if_no_peer_cert, true},
                            {cacertfile, Cacerts},
                            {certs_keys, [#{certfile => ServerCert, keyfile => ServerKey}]}]
                          ],

    {ok, ListenSocket} = ssl:listen(?PORT, TlsOptions),
    listen(ListenSocket, HandshakeTlsOptions).

listen(ListenSocket, HandshakeTlsOptions) ->
    io:format("Server ready~n", []),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
    {ok, Socket} = ssl:handshake(TLSTransportSocket, HandshakeTlsOptions),
    Data = "server ping",
    ok = ssl:send(Socket, Data),
    io:format("Server send: ~p~n", [Data]),
    receive
        {ssl, Socket, Data} ->
            io:format("Server received: ~p~n", [Data])
    end,
    ssl:shutdown(Socket, read_write),
    ssl:close(Socket),
    timer:sleep(2 * 1000),
    listen(ListenSocket, HandshakeTlsOptions).

