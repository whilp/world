#!/usr/bin/env python

# lockdown.py - apply file modes extracted from a configuration
# file.
# Config files should look like so:
#
# [home]
# .ssh/ = 0700
# .twmrc = 0600

import os, sys
from ConfigParser import ConfigParser

# Settings.
config_file = os.path.expanduser('~/.hgmode')

# Functions.
def error(msg):
    sys.stderr.write('=!=> %s.' % str(msg))

config = ConfigParser()
configs = config.read(config_file)

if len(configs) < 1:
    error("Configuration file %s couldn't be read" % config_file)
    sys.exit()

for section in config.sections():
    options = config.options(section)
    if section == 'home':
        root = os.path.expanduser('~/')
    else:
        root = section
    options.sort()
    for option in options:
        path = os.path.join(root, option)

        # os.chmod() wants an octal.
        mode = int(config.get(section, option), 8)
        os.chmod(path, mode)
