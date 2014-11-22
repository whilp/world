#!/usr/bin/python
# https://gist.github.com/areina/3879626
import re, os, sys

def get_password_emacs(machine, login, port):
    s = "machine %s login %s port %s password ([^ ]*)\n" % (machine, login, port)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    return p.search(authinfo).group(1)

if __name__ == "__main__":
    print get_password_emacs(*sys.argv[1:])
