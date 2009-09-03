TERMINUS=/usr/local/lib/X11/fonts/terminus
BKG="${HOME}/.bkg.png"
XLOADIMAGE="$(which xloadimage)"
LOCALTERMS="${HOME}/.xinitrc.local"

switch-caps

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
while i3; do
    sleep 3
done
