#!/usr/bin/env python

from os.path import join
from time import sleep
from sys import exit

batt = '/proc/acpi/battery/BAT0'
info = join(batt, 'info')
state = join(batt, 'state')

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
	if line.find('charged') == 25:
	    exit()
	elif line.find('last full capacity') > -1:
	    fullCapacity = line.split()[3]
	elif line.find('remaining capacity') > -1:
	    remainingCapacity = line.split()[2]

percent = 100 * (float(remainingCapacity)/float(fullCapacity))
percent = str(percent).split('.')[0]

if percent != '100':
    print percent[:4]
