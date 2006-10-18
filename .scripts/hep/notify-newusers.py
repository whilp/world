import os
from string import Template
from tempfile import mkstemp
from ConfigParser import ConfigParser

tmpl = """
Hi $shortname-

I just created an account on the UW-HEP cluster for you. The login
is "$username" and the password is "$password".

Please log into login.hep.wisc.edu using SSH and change your AFS
password using "kpasswd" as soon as possible. If you do not change
your password within 48 hours, your account will be locked.

Remote SSH access to the UW-HEP cluster is restricted to the
login.hep.wisc.edu systems only. This restriction applies to the
"ssh" command, as well as "slogin", "scp" and "sftp".

Please review the University of Wisconsin's acceptable use policy:

 http://www.doit.wisc.edu/security/policies/appropriate_use.asp

Please do not share your password with anyone else.

If you have any questions about this account or the UW-HEP cluster,
please don't hesitate to contact me.

-- 

o--------------------------{ Will Maier }--------------------------o
| jabber:..wcmaier@jabber.ccc.de | email:..will.maier@hep.wisc.edu |
| AIM:.................willmaier | cell:..............608.438.6162 |
*--------------------[ UW High Energy Physics ]--------------------*
"""

config = ConfigParser()
config.read(os.path.expanduser('~/TMP/hepmassmail'))

userinfo = dict()
for user in config.sections():
    # Build a dict filled with the user's details.
    userinfo = dict()
    userinfo['username'] = user
    userinfo['password'] = config.get(user, 'pass', raw=True)
    userinfo['fullname'] = config.get(user, 'fullname', raw=True)
    userinfo['shortname'] = userinfo['fullname'].split(' ', 1)[0]
    userinfo['address'] = config.get(user, 'address', raw=True)

    # Process the template.
    template = Template(tmpl)
    out = template.substitute(userinfo)

    # Write the processed template out to a tempfile.
    fd, tmp = mkstemp(dir='/tmp')
    os.write(fd, out)
    os.close(fd)

    # Send the mail.
    cmdinfo = (tmp, userinfo['fullname'], userinfo['address'])
    cmd = "mutt -i '%s' -s 'New UW-HEP cluster account' '%s <%s>' < /dev/null" % cmdinfo
    print('Emailing %s.' % userinfo['fullname'])
    os.system(cmd)

    # Clean up.
    os.remove(tmp)

    #DEBUG:
    #print userinfo
