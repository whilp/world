#!/bin/sh

CVSDIR="${HOME}/HG"
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
    mv -f "${DST}" "${DST}.old" 2>/dev/null
    ln -sf "${SRC}" "${DST}"
done
