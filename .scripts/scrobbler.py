#!/usr/bin/env python

# See http://www.audioscrobbler.net/development/protocol/ for more
# info.

import md5
import re
import time as _time
import urllib2

from urllib import urlencode

AS_POST = 'http://post.audioscrobbler.com/'
AS_PROTOCOL = 1.2
CLIENT_ID = 'tst'
CLIENT_VER = 1.0
USER_AGENT = 'dj_kije/0.1 +http://www.lfod.us/'

class Error(Exception):
    """Base error class."""
    pass

class AudioScrobblerError(Error):
    """Base Audioscrobbler error class."""
    pass

class SubmitError(Error):
    """Raised when we fail to submit."""
    pass

class HandshakeError(AudioScrobblerError):
    """Raised when something goes wrong during the handshake."""
    pass

class BannedError(HandshakeError):
    """Raised when the client version is banned by the server."""
    pass

class BadauthError(HandshakeError):
    """Raised when the authentication info is incorrect."""
    pass

class BadtimeError(HandshakeError):
    """Raised when the timestamp provided is off."""
    pass

class FailureError(HandshakeError):
    """Raised when the handshake fails for a reason."""
    pass

class HardError(HandshakeError):
    """Raised when the server fails in a bad, bad way."""
    pass

class SessionError(AudioScrobblerError):
    """Raised when an established session goes bad."""
    pass

class HTTPClient(object):
    debug = False

    def request(self, url, data=None, headers={}, debug=False):
        """Send a request for 'url' and return a file-like object."""

        request = urllib2.Request(url)

        for k, v in headers.items():
            request.add_header(k, v)

        if hasattr(self, 'user_agent'):
            request.add_header('User-Agent', self.user_agent)

        if data is not None:
            try:
                data = urlencode(data)
            except TypeError:
                pass
            request.add_data(data)

        handlers = []
        debug = debug or self.debug
        if debug:
            handlers = [urllib2.HTTPHandler(debug),]

        opener = urllib2.build_opener(*handlers)

        return opener.open(request)

class Scrobbler(HTTPClient):

    def __init__(self, user, password, protocol=AS_PROTOCOL,
            client_id=CLIENT_ID, client_ver=CLIENT_VER, debug=False):
        self.hs = 'true'
        self.user = user
        self.password = md5.md5(password).hexdigest()
        self.protocol = protocol
        self.client_id = client_id
        self.client_ver = client_ver
        self.debug = debug

        self.post = AS_POST
        self.user_agent = USER_AGENT
        self.session = None

    def hand_shake(self):
        """Perform the handshake with the server and return a Session object."""
        timestamp = str(int(_time.time()))
        authtoken = md5.md5(self.password + timestamp).hexdigest()

        data = {
            'hs': self.hs,
            'p': str(self.protocol),
            'c': self.client_id,
            'v': str(self.client_ver),
            'u': self.user,
            't': timestamp,
            'a': authtoken}

        response = self.request(self.post, data)
        session = Session(response)

        response.close()

        return session

    def session_required(f):
        """Require that a valid session exist before running function 'f'."""
        def decorated(self, *args, **kw):
            if self.session is None:
                self.session = self.hand_shake()

            tries = 0
            while tries < 3:
                try:
                    returned = f(self, *args, **kw)
                    break
                except SessionError:
                    self.session = self.hand_shake()
                    tries += 1
            else:
                raise SessionError("Exceeded session retry limit (3).")

            return returned

        return decorated

    @session_required
    def now_playing(self, artist, track, album='', secs='',
            tracknumber='', mb_trackid=''):
        """Send a 'now playing' update to last.fm."""
        data = {
            's': self.session.id,
            'a': artist,
            't': track,
            'b': album,
            'l': secs,
            'n': tracknumber,
            'm': mb_trackid}

        response = self.request(self.session.now_playing, data).read()

        status = response.strip()

        if status == 'OK':
            return True
        elif status == 'BADSESSION':
            raise SessionError("Bad session.")

    @session_required
    def submit(self, artist, track, time=None, source='P',
            secs='180', rating='L', album='', tracknumber='',
            mb_trackid='', index=0):
        """Submit a single song to last.fm."""

        if time is None:
            # Must be UTC.
            # XXX: but this causes 'in NN hours' thingies to show
            # up...
            #time = str(int(_time.mktime(_time.gmtime())))
            time = str(int(_time.time()))

        data = {
            's': self.session.id,
            'a[%d]' % index: artist,
            't[%d]' % index: track,
            'i[%d]' % index: time,
            'o[%d]' % index: source,
            'r[%d]' % index: rating,
            'l[%d]' % index: secs,
            'b[%d]' % index: album,
            'n[%d]' % index: tracknumber,
            'm[%d]' % index: mb_trackid}

        response = self.request(self.session.submission, data).read()

        return response

    @session_required
    def submit_many(self, songs):
        """Send submissions for several songs at once.
        
        'songs' should be a sequence of dictionaries, each with the
        following keys and appropriate values:

            artist, track, time, source, rating, secs, album,
            tracknumber, mb_trackid

        Refer to the Audioscrobbler protocol documentation for more
        information.
        """
        index = 0
        data = {'s': self.session.id}

        for song in songs:
            songdata = {
                'a[%d]' % index: song['artist'],
                't[%d]' % index: song['track'],
                'i[%d]' % index: song['time'],
                'o[%d]' % index: song['source'],
                'r[%d]' % index: song['rating'],
                'l[%d]' % index: song['secs'],
                'b[%d]' % index: song['album'],
                'n[%d]' % index: song['tracknumber'],
                'm[%d]' % index: song['mb_trackid']}

            data.update(songdata)
            index += 1

        response = self.request(self.session.submission, data).read()

        return response

class Session(object):

    def __init__(self, response):
        if hasattr(response, 'readline'):
            response = response.readlines()
        elif hasattr(response, 'splitlines'):
            response = response.splitlines()
        elif hasattr(response, 'read'):
            response = response.read().splitlines()

        self.response = [x.strip() for x in response]
        self.status = self.response[0]

        if self.status == 'OK':
            self.id = self.response[1]
            self.now_playing = self.response[2]
            self.submission = self.response[3]
        elif self.status == 'BANNED':
            raise BannedError("Server rejected this client's version.")
        elif self.status == 'BADAUTH':
            raise BadauthError("Server rejected the authentication info.")
        elif self.status == 'BADTIME':
            raise BadtimeError("Supplied timestamp is unreal.")
        elif self.status.startswith == 'FAILED':
            reason = self.status.split()[1]
            raise FailureError("Handshake failed: %s." % reason)
        else:
            raise HardError("Handshake failed.")

def query(artist, track):
    """Send a query to MusicBrainz and return kwargs suitable for Scrobbler.submit()."""
    kwargs = {'artist': artist, 'track': track}

    # Prepare the track and artist fields.
    if 'feat.' in artist:
        artist, feat = [x.strip() for x in artist.split('feat.', 1)]

    track = track.replace('(remix)', '')

    q = Query()
    f = TrackFilter(title=track, artistName=artist)

    results = q.getTracks(f)
    # MB wants us to sleep at least one second between queries.
    time.sleep(1.5)

    if results:
        # Take the best result.
        results = sorted(results, key=attrgetter('score'))
        result = results[-1]
        track = result.getTrack()
        artist = track.getArtist()

        # Update kwargs.
        kwargs['track'] = track.title
        kwargs['artist'] = artist.name
        kwargs['mb_trackid'] = track.id
        if track.duration:
            kwargs['secs'] = track.duration
    else:
        kwargs['artist'], kwargs['track'] = \
                [capwords(x.strip()) for x in (artist, track)]

    return kwargs

if __name__ == '__main__':
    import sys
    import time

    from operator import attrgetter
    from string import capwords

    try:
        from musicbrainz2.webservice import Query, TrackFilter, WebServiceError
        from musicbrainz2.model import Track
    except ImportError:
        Query = False

    DELIMITER = ' - '
    USER = 'dj_kije'
    PASSWD = 'lesheros'

    if Query:
        q = Query()
    else:
        sys.stderr.write("MusicBrainz support disabled.")

    submit = True
    track = sys.argv[1]

    # If the user passed -n, don't actually submit results and look
    # for the song info in argv[2].
    if sys.argv[1] == '-n':
        submit = False
        track = sys.argv[2]

    # If track is a MusicBrainz URL, extract the ID.
    if track.startswith('http://'):
        # MusicBrainz URL, like:
        #   http://musicbrainz.org/track/fc1f032b-fe4c-4e9d-9b4e-144d8c6355b9.html
        track = track.split('/')[-1]
        track = track.split('.')[0]

    if Query and len(track) == 36 and len(track.split('-')) == 5:
        # MusicBrainz ID, like:
        #   fc1f032b-fe4c-4e9d-9b4e-144d8c6355b9
        mbid = track
        track = q.getTrackById(mbid)
    else:
        # artist - track, like:
        #   Ol' Dirty Bastard - Dirty Dancin' (feat. Method Man)
        artist, track = track.split(DELIMITER, 1)
        if Query:
            f = TrackFilter(title=track, artistName=artist)
            results = q.getTracks(f)

        if Query and results:
            # results is a list of results, sorted in order with the
            # most likely match first.
            result = results[0]
            track = result.getTrack()

    kwargs = {}
    if isinstance(track, Track):
        artist = track.getArtist()
        kwargs['mb_trackid'] = track.id
        kwargs['track'] = track.title
        kwargs['artist'] = artist.name
        if track.duration:
            kwargs['secs'] = track.duration
    else:
        kwargs['artist'], kwargs['track'] = \
                [capwords(x) for x in (artist, track)]

    # Some tracks and artists have weird characters in them.
    # Translate any such characters before submitting/printing.
    char_map = {u'\xbf': '', u'\xf3': 'o'}
    ascii = lambda s: ''.join([char_map.get(x, x) for x in s])
    kwargs['artist'] = ascii(kwargs['artist'])
    kwargs['track'] = ascii(kwargs['track'])

    if submit:
        scrobbler = Scrobbler(USER, PASSWD)
        try:
            scrobbler.submit(**kwargs)
        except:
            sys.stderr.write("Submission failed; retrying in 5 seconds.")
            time.sleep(5)
            scrobbler = Scrobbler(USER, PASSWD, debug=2)
            scrobbler.submit(**kwargs)
    else:
        sys.stdout.write("%s\n" % kwargs)
