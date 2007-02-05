#!/usr/local/bin/python
# vim: set nospell:

import sys, os

from time import strftime, sleep, time
from socket import gethostname, socket

fifo = "~/.dwm/fifo"
f = open(os.path.expanduser(fifo), 'w')

name = gethostname().split('.')[0]

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
        mpd = nowPlaying()

    f.write('[NP: %(mpd)s][%(name)s][%(date)s]' % locals())
    f.flush()
    sleep(1)
f.close()
