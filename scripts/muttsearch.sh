#!/bin/sh

echo -n "Query: "
read QUERY
[ -n "${QUERY}" ] || exit 0

mairix "${QUERY}"
