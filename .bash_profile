. ~/.profile

shopt -s histappend
shopt -s checkwinsize

unset MAILCHECK

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
