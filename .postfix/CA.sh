#!/bin/sh

CERTROOT=/etc/ssl
PRIVROOT="${CERTROOT}/private"
RSAROOT="${CERTROOT}"

DSAPARAM="${CERTROOT}/dsa1024.pem"
DSACERT="${CERTROOT}/dsacert.pem"
RSACERT="${CERTROOT}/server.crt"
CRT="${CERTROOT}/server.crt"

DSAKEY="${PRIVROOT}/dsakey.pem"
RSAKEY="${PRIVROOT}/server.key"
CSR="${PRIVROOT}/server.csr"

echo "===> Generating DSA parameter set"
echo "        DSA params: ${DSAPARAM}"
openssl dsaparam 1024 -out "${DSAPARAM}"

echo "===> Generating DSA pair:"
echo "       Certificate: ${DSACERT}"
echo "       Unencrypted key: ${DSAKEY}"
openssl req -x509 -nodes -newkey dsa:dsa1024.pem \
    -out "${DSACERT}" -keyout "${DSAKEY}"

echo "===> Generating RSA key:"
echo "       Unencrypted key: ${RSAKEY}"
openssl genrsa -out "${RSAKEY}" 1024


echo "===> Generating a Certificate Signing Request"
echo "       CSR: ${CSR}"
echo "       For key: ${RSAKEY}"
openssl req -new -key "${RSAKEY}" -out "${CSR}"

echo "===> Self-signing RSA key"
echo "       CSR: ${CSR}"
echo "       Key: ${RSAKEY}"
echo "       Signed certificate: ${RSACERT}"
openssl x509 -req -days 365 -in "${CSR}" -signkey "${RSAKEY}" \
    -out "${RSACERT}"
