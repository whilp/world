#!/usr/bin/env python

import sys

from urllib2 import urlopen

from icalendar import Calendar
from pytz import UTC, timezone

Default = object()

TIMEZONE = timezone('America/Chicago')

class cacheable(object):

    def __init__(self, callable):
        self.callable = callable

    def __call__(self, *args, **kwargs):
        self.init_cache()
        if self.cache is Default:
            self.cache = self.callable(*args, **kwargs)

        return self.cache

    def init_cache(self):
        if not hasattr(self, 'cache'):
            setattr(self, 'cache', Default)

class Event(object):
    start_fmt = '%d %b %Y AT %H:%M'
    remind_fmt = 'REM %(start)s MSG %(summary)s %(description)s'

    def __init__(self, schedule, ics_event):
        self.schedule = schedule
        self.ics_event = ics_event

    @property
    def remind(self):
        start = self.start.strftime(self.start_fmt)
        kw = self.__dict__
        print self.__dict__
        kw.update(locals())

        return self.remind_fmt % kw

    @property
    def timezone(self):
        return self.schedule.timezone

    @property
    def start(self):
        start = self.ics_event['DTSTART'].dt
        return start.astimezone(self.timezone)
    
    @property
    def duration(self):
        return self.ics_event['DURATION']

    @property
    def summary(self):
        return self.ics_event['SUMMARY']

    @property
    def description(self):
        return self.ics_event['DESCRIPTION']

class BadgerEvent(Event):

    @property
    def summary(self):
        return self.ics_event['SUMMARY'].replace('Basketball-M: ', '')
    
    @property
    def description(self):
        descr = ' '.join(self.ics_event['DESCRIPTION'].split())
        return descr.replace('Media: ', '')

class NetEvent(Event):
    pass

class IcsSchedule(object):
    timezone = UTC

    def __init__(self, url, event_factory=Event, timezone=UTC):
        self.url = url
        self.event_factory = event_factory
        self.timezone = timezone

    @property
    def remind(self):
        remind_sched = [x.remind for x in self.events]
        return '\n'.join(remind_sched)

    @property
    def events(self):
        events = []
        for e in self.ics.walk():
            if e.name == 'VEVENT':
                event = self.event_factory(self, e)
                events.append(event)

        return events

    @property
    @cacheable
    def ics(self):
        return Calendar.from_string(self.fetch(self.url))

    def fetch(self, url):
        return urlopen(url).read()

URLS = {
        'badgers':
        ("http://uwbadgers.com/ical_schedules/116/UW-Athletics.ics",
            BadgerEvent),
        'nets': ("http://sports.yahoo.com/nba/teams/njn/ical.ics",
            NetEvent),
}

try:
    team = sys.argv[1]
except IndexError:
    sys.stderr.write("You must specify an argument\n")
    sys.stderr.write("Valid arguments: %s\n" % ', '.join(URLS))
    sys.exit(1)

url, event_factory = URLS[team]
sched = IcsSchedule(url, event_factory, TIMEZONE)

print sched.remind
