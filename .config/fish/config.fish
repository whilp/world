fish_hybrid_key_bindings

set -x TERM screen-256color-s
set -x VIS_PATH ~/.config/vis

set -x LANG en_US.UTF-8

set -x CDPATH . ~/src/* $CDPATH

set -x PATH /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin

# Go.
set -x GOPATH $HOME

# Java.
set -x JAVA_HOME /usr/lib/jvm/(update-java-alternatives --list | cut -f1 -d\  | head -1 2>/dev/null)
set PATH $JAVA_HOME/bin $PATH

# Python.
set PATH ~/.local/bin $PATH
set PATH ~/.python/bin $PATH

# Me.
set PATH ~/bin $PATH
