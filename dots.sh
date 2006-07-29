#!/bin/sh

CVSDIR="${HOME}/CVS"
DOTSDIR="${CVSDIR}/dotfiles"


MANIFEST='Xresources
Xsession
cvsignore
cvsrc
elinks
hgrc
ion3
irssi
login
mailcap
nethackrc
profile
profiles
screen
snownews
ssh
twmrc
vim
vimrc
wmii
xinitrc
zsh
zshrc'

for DOT in ${MANIFEST}; do
	ln -sf ${DOTSDIR}/${DOT} ${HOME}/.${DOT}
done
