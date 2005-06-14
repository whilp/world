#!/usr/bin/env python

import shutil, os, sys

cwd = os.getcwd()
home = os.getenv('HOME')
skipList = ['updatedots.py', 'CVS', '42.gpg', 'config', 'gaim', 'ion3', 'elinks']
execList = ['updatedots.py']

dirContents = os.listdir(cwd)

debugOn = False

def debug(msg=False):
    if debugOn:
	print "Debug:", msg

def link(src, dst):
    try:
	os.symlink(src, dst)
    except OSError:
	os.unlink(dst)
	os.symlink(src, dst)
	
for item in dirContents:
    if os.path.isdir(item) or item in execList:
	os.system('chmod -R 700 %s' % item)
    else:
	os.chmod(item, 0600)
    dst = os.path.join(home, ''.join(('.', item)))
    src = os.path.join(os.getcwd(), item)
    if item in skipList or item.startswith('.'):
	debug(item)
	pass
    else:
	print "Linking", item, "to", dst
	link(src, dst)

