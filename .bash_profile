. ~/.profile

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

shopt -s histappend
shopt -s checkwinsize

unset MAILCHECK
export PS1="\a\h:$(tput bold)\w$(tput sgr0) \$ "

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Setting PATH for EPD-7.2-2
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/EPD64.framework/Versions/Current/bin:${PATH}"
export PATH

MKL_NUM_THREADS=1
export MKL_NUM_THREADS
