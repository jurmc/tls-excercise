#!/usr/bin/bash

cd certs

# Private key
openssl genrsa -out client.key 2048
# Certificate signing request (CSR)
openssl req -new -key client.key -out client.csr --subj "/C=PL/ST=./L=./O=. /OU=./CN=localhost"
# Self signed certificate
openssl x509 -signkey client.key -in client.csr -req -days 365 -out client.pem
