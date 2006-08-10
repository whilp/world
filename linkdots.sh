#!/bin/sh


DOTS="Xresources
Xsession
cvsignore
cvsrc
elinks
hgrc
ion3
irssi
login
mailcap
ncmpc
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
zshrc"
CVSDIR="${HOME}/CVS"
DOTSDIR="${CVSDIR}/dotfiles"

# Link dotfiles into ${HOME}.
for DOT in ${DOTS}; do
    DST="${HOME}/.${DOT}"
    SRC="${DOTSDIR}/${DOT}"
    if [ -e "${DST}" ]; then 
        echo "===> ${DST} exists; creating backup."
        mv -f "${DST}" "${DST}.old"
    fi
    echo "===> Creating ${DST}."
    ln -sf "${SRC}" "${DST}"
done

# Clean up.
rm -ri $(find ${HOME} -maxdepth 1 -mindepth 1 -name "*.old")
