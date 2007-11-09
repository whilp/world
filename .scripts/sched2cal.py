#!/usr/bin/env python

import re

from HTMLParser import HTMLParser
from datetime import datetime, date
from time import strptime

whitespace_re = re.compile('\S+')

class ScheduleParser(HTMLParser):

    def __init__(self):
        HTMLParser.__init__(self)
        self.games = []
        self.buffer = ''

    def handle_starttag(self, tag, attrs):
        raise NotImplementedError

    def handle_endtag(self, tag):
        pass

class BadgerSchedParser(ScheduleParser):
    in_sched_table = False
    in_game_row = False
    in_game_data = False
    game_data = []
    headers = ('date', 'opponent', 'city', 'location', 'media',
            'score', 'results')

    def handle_starttag(self, tag, attrs):
        tag = tag.lower()
        attrs = dict(attrs)

        if tag == 'table':
            if 'id' in attrs and 'schedule' in attrs['id']:
                self.in_sched_table = True
        elif self.in_sched_table:
            if tag == 'tr': 
                if attrs['class'] in ('even', 'odd'):
                    self.in_game_row = True
            elif tag == 'td':
                if self.in_game_row:
                    self.in_game_data = True

    def handle_data(self, data):
        if self.in_game_data:
            self.buffer = ' '.join((self.buffer, data))

    def handle_endtag(self, tag):
        tag = tag.lower()
        if tag == 'table' and self.in_sched_table:
            self.in_sched_table = False
        elif tag == 'tr' and self.in_game_row:
            self.in_game_row = False
            game = Game(**dict(zip(self.headers, self.game_data)))
            self.games.append(game)
            self.game_data = []
        elif tag == 'td' and self.in_game_data:
            self.in_game_data = False
            self.game_data.append(self.buffer)
            self.buffer = ''

class Game(object):

    def __init__(self, **kwargs):
        info = dict([(x, ' '.join(y.split())) for x, y in kwargs.items()])

        self.date = self.parse_date(info['date'])

        self.opponent = info['opponent'].replace('TBA ', '')
        self.tba = False
        if self.opponent != info['opponent']:
            self.tba = True

        self.city = info['city']
        self.location = info['location']
        self.media = info['media']
        self.score = info['score']
        self.results = info['results']

    def __repr__(self):
        return '%s: %s' % (self.date, self.opponent)

    def parse_date(self, datestr):
        if datestr.endswith('TBA'):
            datestr = datestr.replace(' TBA', '')
            pattern = '%m/%d/%Y'
            self.tba = True
            return date(*strptime(datestr, pattern)[:3])
        else:
            pattern = '%m/%d/%Y %I:%M %p'
            return datetime(*strptime(datestr, pattern)[:6])


if __name__ == '__main__':
    import sys

    source = sys.argv[1]

    try:
        page = open(source)
    except IOError:
        from urllib import urlopen
        page = urlopen(source)

    parser = BadgerSchedParser()
    parser.feed(page.read())

    # The games should be in the right order, since the source
    # tables are sorted by date.
    games = parser.games

    def make_entry(game, home=None):
        at = ' '
        if home is not None and game.location != home:
            at = ' at '

        date = game.date.strftime('%m.%d')
        if game.tba:
            descr = 5 * ' '
        else:
            descr = game.date.strftime('%H:%M')

        descr = at.join((descr, game.opponent))
        if game.media:
            descr += ' (%s)' % game.media

        return '\t'.join((date, descr))

    cal = [make_entry(x, 'Kohl Center') for x in games]

    print '\n'.join(cal)
