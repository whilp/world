##################  BEGIN HEADERS
# Filename	: $HOME/.login
# Use		: configures environment on shell login
# Version	: $Revision: 1.8 $
# Author	: Will Maier <willmaier@ml1.net>
# Updated	: $Date: 2006/01/25 18:13:17 $
# CVS		: $Id: login,v 1.8 2006/01/25 18:13:17 will Exp $
# Copyright	: Copyright (c) 2005 Will Maier
# License	: Expat; see <http://www.opensource.org/licenses/mit-license.php>
##################  END HEADERS

echo "TEST"
wsconsctl 'keyboard.map+=keysym Caps_Lock = Control_L' > /dev/null
stty	newcrt crterase
stty	erase 
