TERMINUS=/usr/local/lib/X11/fonts/terminus
BKG="${HOME}/.bkg.png"
XLOADIMAGE="$(which xloadimage)"
HOSTXINIT="${HOME}/.xinit.d/$(hostname -s)"
HOSTNAME=$(hostname)
OSNAME=$(uname)
EVILWM="evilwm -term xterm"
WM=cwm
DOCKED=0
EXTSCREEN=0

export PATH=/sbin:${HOME}/bin:/usr/X11R6/bin:${PATH}

# Fonts.
[ -d "${TERMINUS}" ] && xset +fp "${TERMINUS}"

xset b off
xsetroot -solid black
if [ -f "${BKG}" -a -x "${XLOADIMAGE}" ]; then
    "${XLOADIMAGE}" -onroot -quiet \
        -background black -center -brighten 10 "${BKG}"
fi

if dmesg | grep -q 'acpidock.*GDCK docked'; then
    DOCKED=1
fi
if xrandr | grep -q "VGA connected"; then
    EXTSCREEN=1
fi

[ -s "${HOSTXINIT}" ] && . "${HOSTXINIT}"

while :; do
    ${WM}
done
