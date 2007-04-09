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

# These hosts are known to be AWOL.
acknowledged = ('kammron.lfod.us') 

# We care about hosts that haven't reported in the last hour.
delta = timedelta(minutes=1)

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

def timecmp(x,y):
    """ Compare timedeltas; if x or y is set to None, consider it
        less than any delta. We check _specifically_ for None.
    """
    # The second element is the datetime object.
    x, y = x[1], y[1]
    if x == y:
        return 0
    elif x == None:
        return 1
    elif y == None:
        return -1
    
    return x > y and 1 or -1
        

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
old = sorted([(h,d) for h,d in entries.items() if datetime.now() - d > delta])
old = dict(old + [(h,None) for h in hosts if not h in entries])

# Print a report.
if old:
    print "The following hosts haven't pulled from the radmind server" 
    print "in the last %s:" % delta
    print 
    print "  LAST SEEN            CERTIFICATE CN"
    for host,date in sorted(old.items(), timecmp):
        if not date:
            print '  <no record found>    %s' % host
        else:
            print '  %s  %s' % (date.strftime('%Y-%m-%d.%H:%M:%S'), host)

# Close up the messages log.
messages.close()
