#!/usr/local/bin/python
# vim: set nospell:

# http://www.weather.gov/data/current_obs/KMSN.xml

import sys, os

from time import strftime, sleep, time
from socket import socket

lockfile = "~/.dwm/.status-lock"

# Test for lockfile and die if found.
if os.path.exists(os.path.expanduser(lockfile)):
    sys.stderr.write("ERROR: Lockfile '%s' found" % lockfile)
    sys.exit()

fifo = "~/.dwm/fifo"
f = open(os.path.expanduser(fifo), 'w')

def nowPlaying(server='localhost', port=6600):
    """ Query a running MPD server.
    """

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

if __name__ == '__main__':
    i = 0
    mpd = False
    while True:
        if i >= 3600:
            i = 0
        i += 1

        # Always recalculate the date.
        date = strftime('%a %d %b %H:%M %Z %Y')
        if not mpd or not i % 60:
            # Stuff to do once a minute.
            mpd = nowPlaying()

        f.write('[%(mpd)s][%(date)s]' % locals())
        f.flush()
        sleep(1)
    f.close()
