fish_hybrid_key_bindings

set CDPATH . ~/src/* $CDPATH

set PATH ~/bin $PATH

set -x GOPATH $HOME

set JAVA_HOME /usr/lib/jvm/(update-java-alternatives --list | cut -f1 -d\  | head -1 2>/dev/null)
set PATH $JAVA_HOME/bin $PATH

