#!/bin/sh

for file in $(echo /home/will/CVS/dotfiles/nbsmtp/*); do
    filebase=$(basename ${file})
    if [ -e $HOME/.${filebase} ]; then
	mv $HOME/.${filebase} $HOME/.${filebase}-bak
    fi
    ln -s ${file} $HOME/.${filebase}
    echo "Linked ${file} to $HOME/.${filebase}"
done
