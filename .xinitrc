WM=~/bin/dwm

export PATH=/sbin:${HOME}/bin:/usr/X11R6/bin:${PATH}

xset b off
xsetroot -solid black
setxkbmap -layout us -option compose:ralt -option ctrl:nocaps

(pgrep -x aucat || aucat -l &) >/dev/null 2>&1
uxterm -T master &
dzen-wrapper &

exec ${WM}
