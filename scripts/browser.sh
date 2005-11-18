#!/bin/sh

URL=$*

elinks -session-ring 1 -remote "openURL($URL, new-tab)"
