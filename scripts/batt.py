#!/usr/bin/env python

import os

batt = '/proc/acpi/battery/BAT0'
info = os.path.join(batt, 'info')
alarm = os.path.join(batt, 'alarm')
state = os.path.join(batt, 'state')

infoFile = open(info, 'r')
infoContents = infoFile.readlines()
infoFile.close()

stateFile = open(state, 'r')
stateContents = stateFile.readlines()
stateFile.close()

remainingCapacity = ''
fullCapacity = ''

files = [stateContents, infoContents]

for file in files:
    for line in file:
	if line.find('last full capacity') > -1:
	    fullCapacity = line.split()[3]
	elif line.find('remaining capacity') > -1:
	    remainingCapacity = line.split()[2]

percent = str(100 * (float(remainingCapacity)/float(fullCapacity)))

print "Current level:", percent[:4]
