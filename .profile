set -o vi

export PAGER=cat
export PS1="$ "

export GOPATH=$HOME
export GITPATH=$GOPATH/src # git-get
mkdir -p $GITPATH
export CDPATH=.:$GITPATH

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export PATH=$HOME/.python/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$GOPATH/bin:$PATH
export PATH=$HOME/bin:$PATH

# TODO: install node
# source $HOME/.nvm/nvm.sh

export FZF_DEFAULT_COMMAND=fnd
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

if [ which hub >/dev/null 2>&1 ]; then
	alias git=hub
fi

