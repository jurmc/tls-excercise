#!/usr/bin/env python3

import socket, ssl, sys, os, time

PORT= 10021
SERVER_CERT="certs/server.pem"
SERVER_KEY="certs/server.key"

CLIENT_CERT="certs/client.pem"
CLIENT_KEY="certs/client.key"

INVALID_CERT="certs/invalid.pem"
INVALID_KEY="certs/invalid.key"

class Server:
    def __init__(self, mode, port, server_cert, server_key, client_cert = None):
        self.mode = mode
        self.port = port
        self.server_cert = server_cert
        self.server_key = server_key
        self.client_cert = client_cert

    def start(self):
        context = ssl.SSLContext(ssl.PROTOCOL_TLSv1_2)

        context.load_default_certs()
        context.load_cert_chain(certfile=self.server_cert, keyfile=self.server_key)
        if self.client_cert != None:
            context.verify_mode = ssl.CERT_REQUIRED
            context.load_verify_locations(CLIENT_CERT)

        bindsocket = socket.socket()
        bindsocket.bind(('localhost', self.port))
        bindsocket.listen()

        while True:
            try:
                print("[{}] Waiting for new client".format(self.mode))
                newsocket, fromaddr = bindsocket.accept()
                try:
                    connstream = context.wrap_socket(newsocket, server_side=True)
                    print("Client connected: {}".format(repr(connstream)))
                    self.deal_with_client(connstream)
                except ssl.SSLError as e:
                    print("Unsuccessfull connection attempt: {}".format(e))
            except KeyboardInterrupt:
                print("KeyboardInterrupt")
                if 'connstream' in locals():
                    connstream.shutdown(socket.SHUT_RDWR)
                    connstream.close()
                sys.exit(0)

    def deal_with_client(self, connstream):
        while True:
            data = connstream.recv(1024)
            if data != b'':
                print("Received (p:{}): {}".format(self.port, repr(data)))
                connstream.send(data)
                time.sleep(1)
                print("Done with client")
                connstream.shutdown(socket.SHUT_RDWR)
                connstream.close()
                return

def client(mode, port, server_cert, client_cert = None, client_key = None):
    print("[{}]: Started".format(mode))
    context = ssl.create_default_context(ssl.Purpose.SERVER_AUTH, cafile=server_cert)
    if client_cert != None:
        context.load_cert_chain(certfile=client_cert, keyfile=client_key)
    conn = context.wrap_socket(socket.socket(socket.AF_INET),
                               server_hostname="localhost")
    conn.connect(("localhost", PORT))
    to_send = b"ping"
    conn.send(to_send)
    print("Send    : {}".format(to_send))
    data = conn.recv(1024)
    print("Received: {}".format(data))
    conn.shutdown(socket.SHUT_RDWR)
    conn.close()

def usage():
    print("Usage: {} [mode]".format(os.path.basename(sys.argv[0])))
    print("  mode = [ client | iwf-client | client-with-wrong-own-certificate | client-with-wrong-server-certificate ]".format(os.path.basename(sys.argv[0])))
    print("  mode = [ server | ats-server ]".format(os.path.basename(sys.argv[0])))
    sys.exit(1)

if len(sys.argv) < 2:
    usage()

mode = os.path.basename(sys.argv[1])

if mode not in ["client", "iwf-client", "client-with-wrong-own-certificate", "client-with-wrong-server-certificate", "server", "ats-server"]:
    usage()

match mode:
    case "client":
        client(mode, PORT, SERVER_CERT)
    case "iwf-client": # This client can verify itself to the server
        client(mode, PORT, SERVER_CERT, CLIENT_CERT, CLIENT_KEY)
    case "client-with-wrong-own-certificate":
        client(mode, PORT, SERVER_CERT, INVALID_CERT, INVALID_KEY)
    case "client-with-wrong-server-certificate":
        client(mode, PORT, INVALID_CERT, CLIENT_CERT, CLIENT_KEY)
    case "server":
        Server(mode, PORT, SERVER_CERT, SERVER_KEY).start()
    case "ats-server": # This server will request certificate from client
        Server(mode, PORT, SERVER_CERT, SERVER_KEY, CLIENT_CERT).start()

