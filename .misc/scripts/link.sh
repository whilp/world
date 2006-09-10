#!/bin/sh

CVSDIR=$HOME/CVS/dotfiles
FILES="zshrc profile screen ssh ion3 cvsrc cvsignore elinks \
	login vimrc xinitrc Xresources zshrc"

cd $HOME
for FILE in $FILES; do
	if [ -e $HOME/.$FILE ]; then
		mv $HOME/.$FILE $HOME/.$FILE-bak
	fi
	ln -s $CVSDIR/$FILE $HOME/.$FILE
done
