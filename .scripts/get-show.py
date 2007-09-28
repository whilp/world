#!/usr/bin/env python

""" Fetch a show from WMSE.org.

    WMSE is a college radio station Milwaukee, Wisconsin that makes
    its archives available. We parse its archive page, choose a
    random show (that we haven't already downloaded), and fetch it.

    TODO:
        * Provide a progress meter of some sort
        * Automatically clean up stuff that's already been
          downloaded
"""

from HTMLParser import HTMLParser

class ArchiveParser(HTMLParser):

    def __init__(self):
        HTMLParser.__init__(self)
        self.shows = []

    def is_show(self, url):
        """ Return True if 'url' is a downloadable show.

        This method is left to the caller to implement.
        """
        raise NotImplementedError

    def handle_starttag(self, tag, attrs):
        if tag.lower() == 'a':
            attrs = dict(attrs)
            href = attrs['href']
            if self.is_show(href):
                self.shows.append(href)

    def handle_endtag(self, tag):
        pass

if __name__ == '__main__':
    import sys
    import urllib

    from random import choice

    w = lambda x: sys.stderr.write('=!=> %s\n' % x)

    BASE_URL = 'http://wmse.org/archive.php'
    BITRATE = '128k'
    FORMAT = 'mp3'
    CHUNKSIZE = 1024

    try:
        show = sys.argv[1]
    except IndexError:
        w("Must supply a show name")
        sys.exit(1)

    if show == 'bts.wrkng':
        dow = 'Mon'
        hour = 210
    else:
        w("Must supply a show name")
        sys.exit(1)

    # Build the URL.
    URL = '?'.join((BASE_URL, 'dow=%s&hour=%d' % (dow, hour)))

    # Fetch the page.
    page = urllib.urlopen(URL)

    def is_show(url):
        """ Return True if url is a valid show.
        """

        if BITRATE in url and url.endswith(FORMAT):
            return True

    # Parse the page.
    parser = ArchiveParser()
    parser.is_show = is_show
    parser.feed(page.read())

    # Ignore shows we've already downloaded. As we download shows,
    # we log each URL on a separate line in a 'seen' file. If that
    # file doesn't exist, consider all shows. If that file does
    # exist, only consider shows that aren't in that file.
    try:
        seenfile = open('seen', 'ra')
        seen = seenfile.readlines()
    except IOError:
        seenfile = open('seen', 'w')
        seen = []

    shows = [x for x in parser.shows if x not in seen]

    # Choose a random show.
    url = choice(shows)

    # Download the file.
    filename = url.split('/')[-1]
    fetcher = urllib.FancyURLopener()
    blah = fetcher.retrieve(url, filename)

    # Log it in our seenfile.
    seenfile.write(url + '\n')
    seenfile.close()
