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

def scroll(fetch, max, out=False):
    """ Generator to scroll text input.
    """
    # Create counters.
    l = -1
    m = max - 1
    out = ''
    count = 0

    # Generator loop.
    while True:
        if not out:
            # Fetch input.
            print 'No output.'
            out = fetch()

        if len(out) <= max:
            yield out
            count += 1
            print 'Too small; looping.'
            print '[%s]' % out
            if count > 30:
                count = 0
        else:
            print 'Scrolling.'
            if m >= len(out):
                # Reset.
                l = -1
                m = max
                out = fetch()
            else:
                # Increment values.
                l += 1
                m += 1
                yield out[l:m]

def nowPlaying(server='localhost', port=6600):
    """ Query a running MPD server.
    """

    mpd = socket()
    mpd.connect((server, port))

    # Find out what's playing.
    mpd.send('currentsong\n')
    resp = mpd.recv(1000)
    mpd.close()
    if 'OK MPD' not in resp:
        # Can't talk to the server.
        return None

    # Create a dictionary based on the output from MPD.
    info = dict([(x.split(': ', 1)) for x in resp.split('\n') if len(x.split(': ', 1)) == 2])

    # Fill in interesting values.
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

    print 'FETCHED!'
    return ' '.join(nowplaying)

if __name__ == '__main__':
    i = 0
    np = scroll(nowPlaying, 30)
    while True:
        # Always recalculate the date.
        date = strftime('%a %d %b %H:%M %Z %Y')

        # Check mpd.
        mpd = np.next()

        f.write('[%(mpd)s][%(date)s]' % locals())
        f.flush()
        sleep(.5)
    f.close()
