#!/usr/bin/env python

import shutil, os

cwd = os.getcwd()

dirContents = os.listdir(cwd)

for item in dirContents:
    dst = ''.join('.', item)
    shutil.copy(item, dst)
