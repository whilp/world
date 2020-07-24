#!/usr/bin/env bash

actual="$1"
shift
exec "$actual" "$@"
