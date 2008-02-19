#!/usr/bin/env python

import sys
import urllib2

from urllib import urlencode

USER = 'will@lfod.us'
PASS = 'LXhg%nYPnf1'
USERAGENT = 'dj_kije/0.1 +http://www.lfod.us/'
URL = 'http://twitter.com/statuses/update.xml'

status = urlencode({'status': ' '.join(sys.argv[1:])})

auth_handler = urllib2.HTTPBasicAuthHandler()
auth_handler.add_password('Twitter API', 'twitter.com', USER, PASS)

request = urllib2.Request(URL)
request.add_header('User-Agent', USERAGENT)
request.add_data(status)

opener = urllib2.build_opener(auth_handler)

try:
    opener.open(request)
except:
    sys.exit(1)

sys.exit()
