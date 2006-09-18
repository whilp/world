#!/usr/local/bin/python
# vim: set nospell:

from sys import argv,stderr,exit

index = '/usr/ports/INDEX'

if len(argv) != 2:
    stderr.write('%s: Must specify an argument.' % argv[0])
    exit(1)

f = open(index, 'r')
raw = f.readlines()
f.close()

for line in raw:
    line = line.split('|')[0]
    if str(argv[1]).lower() in line.lower():
        print line
