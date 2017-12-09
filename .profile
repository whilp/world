set -o vi

export PAGER=cat
export PS1="$ "

export GOPATH=$HOME
export GITPATH=$GOPATH/src # git-get
mkdir -p $GITPATH
export CDPATH=$(ls $GITPATH | tr '
' ':')

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export PATH=$HOME/.python/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$GOPATH/bin:$PATH
export PATH=$HOME/bin:$PATH

source $HOME/.nvm/nvm.sh