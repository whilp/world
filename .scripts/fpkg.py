#!/usr/local/bin/python
# vim: set nospell:

from sys import argv,stderr,exit
#from sre import search

index = '/usr/ports/INDEX'

if len(argv) != 2:
    stderr.write('%s: Must specify an argument.' % argv[0])
    exit(1)

f = open(index, 'r')
raw = f.readlines()
f.close()

def strip(line, upto='|'):
    """Get rid of extra information in OpenBSD ports INDEX files.
    """
    index = line.find(upto)
    return line[0:index]

for line in raw:
    line = strip(line)
    if str(argv[1]).lower() in line.lower():
        print line
