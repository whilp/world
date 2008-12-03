#!/usr/bin/env python

from urllib2 import urlopen

from icalendar import Calendar
from pytz import timezone

SCHED_URL = "http://uwbadgers.com/ical_schedules/116/UW-Athletics.ics"
TIMEZONE = timezone('America/Chicago')

sched = urlopen(SCHED_URL).read()

cal = Calendar.from_string(sched)
events = [x for x in cal.walk() if x.name == 'VEVENT']

for event in events:
    # Convert to localtimezone.
    start = event['DTSTART'].dt
    if hasattr(start, 'astimezone'):
        start = start.astimezone(TIMEZONE)

    start = start.strftime('%d %b %Y AT %H:%M')

    duration = event['DURATION']
    summary = event['SUMMARY'].replace('Basketball-M: ', '')
    description = ' '.join(event['DESCRIPTION'].split()).replace('Media: ', '')

    print 'REM %s MSG [a("%s %s")]' % (start, summary, description)
