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

def copy(src, dst):
    os.umask(077)
    if os.path.exists(dst):
	newName = ''.join((os.path.split(dst)[1], '.BAK'))
	debug(newName)
	newDst = os.path.join(os.path.split(dst)[0], newName)
	debug(newDst)
	if os.path.exists(newDst):
	    if os.path.isdir(newDst):
		shutil.rmtree(newDst)
	    else:
		os.remove(newDst)
	os.rename(dst, newDst)
    if os.path.isdir(src):
	shutil.copytree(src, dst)
    else:
	shutil.copy(src, dst)
	
for item in dirContents:
    if os.path.isdir(item) or item in execList:
	os.system('chmod -R 700 %s' % item)
    else:
	os.chmod(item, 0600)
    dst = os.path.join(home, ''.join(('.', item)))
    if item in skipList or item.startswith('.'):
	debug(item)
	pass
    else:
	print "Copying", item, "to", dst
	copy(item, dst)

