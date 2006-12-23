#!/usr/local/bin/python
# vim: set nospell:

import sys, os

from time import strftime, sleep, time
from socket import gethostname, socket

fifo = "~/.dwm/fifo"
f = open(os.path.expanduser(fifo), 'w')

name = gethostname().split('.')[0]
uptime = None

# System-specific methods of figuring out when we booted.
if 'openbsd' in sys.platform:
    sysctl = os.popen('sysctl -n kern.boottime', 'r', 1)
    boot = int(sysctl.read())
    sysctl.close()
elif 'linux' in sys.platform:
    proc = open('/proc/uptime', 'r')
    boot = proc.read().split()[0]
    boot = time() - int(boot.split('.')[0])
    proc.close()
else:
    boot = False

# Calculate uptime.
def calcUptime(boottime):
    boot = int(boottime)
    now = time()
    up = int(now - boot)

    # Constant time values (in seconds).
    minute = 60
    hour = 60 * minute
    day = 24 * hour

    # Actual values.
    minutes = int((up % hour) / minute)
    hours = int((up % day) / hour)
    days = int(up / day)

    # Build the uptime string.
    uptime = ''
    if days:
        uptime += '%dd' % days
    if hours:
        if uptime:
            uptime += ' '
        uptime += '%dh' % hours
    if minutes:
        if uptime:
            uptime += ' '
        uptime += '%dm' % minutes

    return uptime

def nowPlaying(server='localhost', port=6600):
    mpd = socket()

    mpd.connect((server, port))

    mpd.send('currentsong\n')
    resp = mpd.recv(1000)
    if 'OK MPD' not in resp:
        return None

    info = dict([(x.split(': ', 1)) for x in resp.split('\n') if len(x.split(': ', 1)) == 2])

    artist = info.get('Artist')
    album = info.get('Album')
    title = info.get('Title')

    nowplaying = []
    if artist:
        nowplaying.append(artist)
    if album:
        nowplaying.append('(%s)' % album)
    if title:
        nowplaying.append('- %s' % title)

    return ' '.join(nowplaying)

i = 0
while True:
    if i >= 3600:
        i = 0
    i += 1

    # Always recalculate the date.
    date = strftime('%a %d %b %H:%M:%S UTC%z %Y')
    if or not i % 60:
        # Stuff to do once a minute.
        #uptime = boot and calcUptime(boot) or ''
        mpd = nowPlaying()

    f.write('[NP: %(mpd)s][%(name)s][%(date)s]' % locals())
    f.flush()
    sleep(1)
f.close()
