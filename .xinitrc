TERMINUS=/usr/local/lib/X11/fonts/terminus
BKG="${HOME}/.bkg.png"
XLOADIMAGE="$(which xloadimage)"
LOCALXINIT="${HOME}/.xinitrc.local"
HOSTXINIT="${HOME}/.xinitrc.$(hostname -s)"
HOSTNAME=$(hostname)
WM=cwm
DOCKED=0

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

init_zipser () {
    pgrep -x aucat || aucat -l &
    if [ "${DOCKED}" -eq 1 ]; then
        xrandr --output VGA --auto --output LVDS --off
    else
        switch-caps
    fi
    xterm -T master &
    firefox &
}

case "${HOSTNAME}" in
   zipser.*) init_zipser;;
esac

exec ${WM}
