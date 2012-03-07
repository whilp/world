. ~/.profile

shopt -s histappend
shopt -s checkwinsize

unset MAILCHECK
export PS1="\a\h:$(tput bold)\w$(tput sgr0) \$ "

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
