#!/usr/local/bin/python

import re

from pprint import pprint
from time import strptime, localtime
from datetime import datetime, timedelta

# radmind logs here.
log = '/var/log/messages'

# We'll save the most recent entries in this dict for each host.
entries = {}

# These hosts should ping radmind regularly. We specify them by
# their CN, which must be unique.
# XXX: Actually, we should probably check for a (hostname, CN) pair.
hosts = ('vger.lfod.us', 'master.lfod.us', 'kammron.lfod.us',
        'zeroth.lfod.us')

# We care about hosts that haven't reported in the last hour.
delta = timedelta(hours=1)

# Syslog doesn't have a year in the log entries, so we'll assume
# that the current year is correct.
year = localtime()[0]

radmind_re = re.compile(r"""
        ^(\S{3}\ [0-9 ]\d\ [0-9:]{8})\  # MMM D?D HH:MM:SS
        (\S+)\                          # syslog host
        radmind\[\d+\]:\                # pid
        report\ (\S+)\ ([0-9\.]+)\ (\S+)\  # hostname, IP, CN
        -\ (\S+)\ (.*)$                 # action, message
        """, re.VERBOSE)

messages = open(log, 'r')

for line in messages:
    match = radmind_re.match(line)
    if match:
        # Process the entry.
        date,loghost,fqdn,ip,cn,action,message = match.groups()

        # Make a date object.
        date = datetime(year, *strptime(date, '%b %d %H:%M:%S')[1:-2])

        if not cn in entries:
            entries[cn] = date
        else:
            if date > entries[cn]:
                entries[cn] = date

# Missing hosts.
missing = [(None,h) for h in hosts if not h in entries]
old = sorted([(d,h) for h,d in entries.items() if datetime.now() - d > delta])
old.extend(missing)

# Print a report.
if old:
    print "The following hosts haven't pulled from the radmind server" 
    print "in the last %s:" % delta
    print 
    print "  LAST SEEN            CERTIFICATE CN"
    for date, host in old:
        if not date:
            print '  <no record found>    %s' % host
        else:
            print '  %s  %s' % (date.strftime('%Y-%m-%d.%H:%M:%S'), host)

# Close up the messages log.
messages.close()
