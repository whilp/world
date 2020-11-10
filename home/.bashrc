#!/usr/bin/env bash

set -o vi
export PROMPT_DIRTRIM=2
export PS1='\w\n ▶ '

export PATH="$HOME/bin:$PATH"

export LC_ALL=C.UTF-8
export LANG=C.UTF-8
export LANGUAGE=C.UTF-8

mkdir -p ~/.ssh
chmod 700 ~/.ssh
chmod 400 ~/.ssh/config
