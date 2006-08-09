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
remind
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
    SRC="${DOTSDIR}/${DOT}"
    DST="${HOME}/.${DOT}"
    mv -f "${DST}" "${DST}.old"
    ln -sf "${SRC}" "${DST}"
done
