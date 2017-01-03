cd () {
	builtin cd "$@" && awd ""
}

export PAGER=cat
export PS1="$ "

export GOPATH=/w
export GITPATH=$GOPATH/src # git-get
export CDPATH=$(ls $GITPATH | tr '
' ':')

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export PATH=/usr/local/go/bin:$PATH
export PATH=~/plan9port/bin:$PATH
export PATH=$HOME/.python/bin:$PATH
export PATH=$GOPATH/bin:$PATH
export PATH=$HOME/bin:$PATH
