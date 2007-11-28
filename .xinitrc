xsetroot -solid black
[ -f ~/.bkg.png ] && xsetbg -background black -center -brighten 10 ~/.bkg.png

# switch caps and control
cat <<EOF | xmodmap -
    remove Lock = Caps_Lock
    remove Control = Control_L
    keysym Control_L = Caps_Lock
    keysym Caps_Lock = Control_L
    add Lock = Caps_Lock
    add Control = Control_L
    keycode 234 = Page_Down
    keycode 233 = Page_Up
EOF

# Terminals
[ -r "${HOME}/.xinitrc.local" ] && . ${HOME}/.xinitrc.local

xset b off

exec cwm -s
