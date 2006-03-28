#!/bin/sh

screen -x terms -X eval "screen -t $$" other \
&& xterm -e screen -x terms -p $$ \
|| xterm -e screen -c $HOME/.screen/rc-terms -S terms -t $$ > /dev/null 2>&1
