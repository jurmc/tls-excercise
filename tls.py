#!/usr/bin/env python3

import socket, ssl, sys, os

PORT= 10021

def client():
    context = ssl.create_default_context()
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    context.load_verify_locations("domain.crt")
    conn = context.wrap_socket(socket.socket(socket.AF_INET),
                               server_hostname="localhost")
    conn.connect(("localhost", PORT))
    conn.send(b"ping!")

def server():
    #context = ssl.create_default_context(ssl.Purpose.CLIENT_AUTH)
    context = ssl.SSLContext(ssl.PROTOCOL_TLSv1_2)
    context.load_default_certs()
    context.load_cert_chain(certfile="domain.crt", keyfile="domain.key")
    bindsocket = socket.socket()
    bindsocket.bind(('localhost', PORT))
    bindsocket.listen(5)

    while True:
        newsocket, fromaddr = bindsocket.accept()
        connstream = context.wrap_socket(newsocket, server_side=True)
        try:
            deal_with_client(connstream)
        finally:
            connstream.shutdown(socket.SHUT_RDWR)
            connstream.close()

def deal_with_client(connstream):
    data = connstream.recv(1024)
    # empty data means the client is finished with us
    while data:
        if not print("-----------------\nconnstream: {}\ndata: {}".format(repr(connstream), repr(data))):
            # we'll assume do_something returns False
            # when we're finished with client
            break
        data = connstream.recv(1024)
    # finished with client

if len(sys.argv) < 2:
    print("Usage: {} [client|server]".format(os.path.basename(sys.argv[0])))
    sys.exit(1)

match os.path.basename(sys.argv[1]):
    case "client":
        client()
    case "server":
        server()


