export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export PATH=/usr/local/go/bin:$PATH
export PATH=~/plan9port/bin:$PATH
export PATH=$HOME/.python/bin:$PATH
export PATH=$HOME/prefix/vis/bin:$PATH
export PATH=$HOME/bin:$PATH
export PAGER=cat

export PS1="$ "
export CDPATH=$(ls /src | tr '
' ':')

export GOPATH=$HOME

cd () {
	builtin cd "$@" && awd ""
}