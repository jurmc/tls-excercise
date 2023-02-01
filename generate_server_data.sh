#!/usr/bin/bash

cd certs

# Private key
openssl genrsa -out server.key 2048
# Cate signing request (CSR)
openssl req -new -key server.key -out server.csr --subj "/C=PL/ST=./L=./O=. /OU=./CN=localhost"
# Self signed certificate
openssl x509 -signkey server.key -in server.csr -req -days 365 -out server.pem
