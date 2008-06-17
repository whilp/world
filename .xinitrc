TERMINUS=/usr/local/lib/X11/fonts/terminus
BKG="${HOME}/.bkg.png"
XLOADIMAGE="$(which xloadimage)"
LOCALTERMS="${HOME}/.xinitrc.local"

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

# Fonts.
[ -d "${TERMINUS}" ] && xset +fp "${TERMINUS}"

xsetroot -solid black
if [ -f "${BKG}" -a -x "${XLOADIMAGE}" ]; then
    "${XLOADIMAGE}" -onroot -quiet \
        -background black -center -brighten 10 "${BKG}"
fi

# Terminals
[ -r "${LOCALTERMS}" ] && . "${LOCALTERMS}"


xset b off
until cwm; do
    sleep 1
done
