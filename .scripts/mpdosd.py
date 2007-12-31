#!/usr/bin/env python

"""This client provides a quick interface to a running MPD server."""

import re
import socket

MPD_PORT = 6600
ACK_RE = re.compile(r"""
        ^ACK\ 
        \[(?P<error>\d+)@(?P<command_listnum>\d+)\]\ 
        \{(?P<current_command>\S+)\}\ 
        (?P<message_text>.*)$
        """, re.VERBOSE)

class MPDException(Exception):
    """Base exception class."""
    pass

class MPDConnectionFailedError(MPDException):
    """Raised when connection attempts fail."""
    pass

class MPDCommandError(MPDException):
    """Raised when a command sent to the server returns an error code."""

    def __init__(self, **kwargs):
        # Defined in ack.h.
        self.error = int(kwargs.get('error', -1))

        # Offset of the command that caused the error within the
        # command list (counts from 0).
        self.command_list_num = int(kwargs.get('command_list_num', -1))

        # Name of command that caused the error.
        self.current_command = kwargs.get('current_command')
        self.message_text = kwargs.get('message_text')

        self.message = self.message_text
        self.args = (self.message,)

class MPD(object):
    """Connection to an MPD server.
    
    Server host and port information may optionally be specified; if
    so, a connection attempt will be made immediately.
    """

    def __init__(self, host=None, port=None):
        self.sock = None
        self.file = None

        if host is not None:
            self.connect(host, port)
        else:
            self.host = None
            self.port = None

    def connect(self, host, port=None):
        """Connect to an MPD server at host:port."""
        if port is None:
            port = MPD_PORT

        # This approach is similar to smtplib.SMTP.connect(). Try to
        # open a socket to the specified host and port.
        for addr in socket.getaddrinfo(host, port, 0, socket.SOCK_STREAM):
            af, socktype, proto, canonname, sa = addr
            try:
                self.sock = socket.socket(af, socktype, proto)
                self.sock.connect(sa)
            except socket.error, msg:
                if self.sock is not None:
                    self.sock.close()
                self.sock = None
                continue
            break

        # Fail if we couldn't connect. If the connection attempt
        # succeeded, provide a file-like interface to the server
        # connection as well.
        if self.sock is None:
            raise MPDConnectionFailedError("Failed to connect to "
                    "server %s:%d" % (host, port))
        else:
            self.file = self.sock.makefile('rb')

        # The server should send us some status info; if that's OK,
        # return True.
        status = self.file.readline().strip()

        if status.startswith('OK'):
            return status.split(' ', 1)[1]

    def close(self):
        """Close the connection to the server."""
        if self.file is not None:
            self.file.close()
        self.file = None

        if self.sock is not None:
            self.sock.close()
        self.sock = None

    def send(self, msg):
        """Send a message to the server."""
        try:
            self.sock.sendall(msg)
        except socket.error:
            self.close()
            raise MPDDisconnected("No connection to server.")

    def recv(self):
        """Read a message from the server."""
        response = []
        while True:
            line = self.file.readline().strip()

            # Check server's response. 'OK' means all went well and
            # we can return the message we received. An ACK match
            # means something went wrong, so we raise an exception.
            # Anything else is part of the response.
            if line == 'OK':
                break

            m = ACK_RE.match(line)
            if m:
                raise MPDCommandError(**m.groupdict())

            response.append(line)

        return '\n'.join(response)

    def do(self, command):
        """Send a command to the server and return its response.
        
        If the command produces an errors in the server,
        MPDCommandError will be raised. Otherwise, the text of the
        server's response will be returned as a string.
        """
        self.send('%s\n' % command)
        return self.recv()

    def _cmd_factory(command):
        """Return a function to execute the given command on an MPD server."""

        def mpd_command(self):
            return dictify(self.do(command))

        return mpd_command

    # MPD commands. See:
    #   http://mpd.wikia.com/wiki/MusicPlayerDaemonCommands
    currentsong = _cmd_factory('currentsong')
    status = _cmd_factory('status')

    def playlistinfo(self, songid=None):
        """Return information about the songs in the current playlist.
        
        If songid is specified, only return information about that
        song.
        """
        command = 'playlistinfo'
        if songid is not None:
            command += ' %d' % songid

        return dictify(self.do(command))

def intify(intstr):
    """Attempt to coerce a string to an int. 
    
    If the coercion fails, simply return the string as-is.
    """
    try:
        out = int(intstr)
    except ValueError:
        out = intstr

    return out

def dictify(output):
    """Parse the output of a command, returning a dict.

    Dictionary keys will be lowercased strings; values, when
    possible, will be coerced into useful types (int, float).
    Otherwise, values will be strings.
    """
    d = {}
    for line in output.splitlines():
        key, value = line.split(': ', 1)
        d[key.lower()] = intify(value)

    return d

if __name__ == '__main__':
    import time

    import pyosd

    # Settings.
    ALIGN = pyosd.ALIGN_RIGHT
    COLOR = '#FFFFFF'
    DELAY = 5
    FONT = '-xos4-terminus-bold-*-*-*-13-*-*-*-*-*-*-*'
    HOST = 'localhost'
    OFFSET = 2
    POSITION = pyosd.POS_TOP
    SHADOW = 2

    server = MPD(HOST)

    np = server.currentsong()
    status = server.status()
    elapsed, total = [int(x) for x in status['time'].split(':')]

    state = status['state']
    if state.startswith('play'):
        state = 'playing'
    elif state.startswith('pause'):
        state = 'paused'
    elif state.startswith('stop'):
        state = 'stopped'

    status['state'] = state
    status['song'] += 1
    status['elapsed'], status['total'] = \
            ['%d:%d' % divmod(x, 60) for x in (elapsed, total)]
    status['pct'] = 100.0 * elapsed/total

    output = ["%(artist)s - %(title)s" % np]
    output.append("[%(state)s] %(song)d/%(playlistlength)d "
            "%(elapsed)s/%(total)s (%(pct)d%%)" % status)

    osd = pyosd.osd(
            timeout=DELAY,
            pos=POSITION,
            align=ALIGN,
            lines=len(output),
            offset=OFFSET,
            font=FONT,
            colour=COLOR,
            shadow=SHADOW)
    osd.set_outline_colour("#000000")
    osd.set_outline_offset(2)
    osd.set_shadow_colour("#00FF00")
    osd.set_shadow_offset(2)

    # XXX: it'd be nice to join with '\n' here, but pyosd doesn't
    # seem to like that.
    output = ' '.join(output)
    print output
    osd.display(output)
    time.sleep(DELAY)
