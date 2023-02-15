#!/usr/bin/bash

cd certs

#
# Server
#

# Private key
openssl genrsa -out server.key 2048
# Cate signing request (CSR)
openssl req -new -key server.key -out server.csr --subj "/C=PL/ST=./L=./O=. /OU=./CN=localhost"
# Self signed certificate
openssl x509 -signkey server.key -in server.csr -req -days 365 -out server.pem

#
# Client
#

# Private key
openssl genrsa -out client.key 2048
# Certificate signing request (CSR)
openssl req -new -key client.key -out client.csr --subj "/C=PL/ST=./L=./O=. /OU=./CN=localhost"
# Self signed certificate
openssl x509 -signkey client.key -in client.csr -req -days 365 -out client.pem

#
# Invalid key
# 

# Private key
openssl genrsa -out invalid.key 2048
# Certificate signing request (CSR)
openssl req -new -key invalid.key -out invalid.csr --subj "/C=PL/ST=./L=./O=. /OU=./CN=localhost"
# Self signed certificate
openssl x509 -signkey invalid.key -in invalid.csr -req -days 365 -out invalid.pem
