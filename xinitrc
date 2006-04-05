xsetroot -solid black

if [ -x $(which kdeinit) ]; then
    kdeinit&
fi

# switch caps and control
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "remove Control = Control_L"
xmodmap -e "keysym Control_L = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Lock = Caps_Lock"
xmodmap -e "add Control = Control_L"

# choose a background
if [ -x $(which xsetbg) && -e $HOME/.backgrounds ]; then
    BGPATH=$HOME/.backgrounds
    IMAGECOUNT=$(ls $BGPATH/ | wc -l | sed -e 's/[^0-9]//g')
    NUMBER=0
    while [ $NUMBER = 0 ]; do
        NUMBER=$((RANDOM%IMAGECOUNT))
    done
    IMAGE="$BGPATH/$(ls $BGPATH | sed -e $NUMBER!d)"
    xsetbg -brighten 80 -fullscreen -onroot $IMAGE
fi
xterm&

#exec enlightenment
xset b off
unclutter -idle 5 -jitter 20&
exec ion3
#exec wmii
#xterm&
#exec twm
