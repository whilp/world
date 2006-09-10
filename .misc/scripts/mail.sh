#!/usr/bin/env sh

#/bin/tar -czf /home/will/Backups/mail-`date +%Y.%m.%d`-back.tbz2 /home/will/Maildir

unset PATH

RSYNC=/usr/local/bin/rsync
MV=/bin/mv
CPIO=/bin/cpio
FIND=/usr/bin/find
MKDIR=/bin/mkdir

BACKUPDIR=$HOME/Backups/Mail
MAILDIR=$HOME/Maildir/

BACKUP='mail-hourly'
BACKUP0=$BACKUP.0
BACKUP1=$BACKUP.1
BACKUP2=$BACKUP.2
BACKUP3=$BACKUP.3

cd $BACKUPDIR

mkdir $BACKUP0 2>/dev/null
mkdir $BACKUP1 2>/dev/null
mkdir $BACKUP2 2>/dev/null
mkdir $BACKUP3 2>/dev/null

$MV $BACKUP3 backup.tmp
$MV $BACKUP2 $BACKUP3
$MV $BACKUP1 $BACKUP2
$MV $BACKUP0 $BACKUP1
$MV backup.tmp $BACKUP0
cd $BACKUP1/ && $FIND . -print | $CPIO -dpl ../$BACKUP0
cd ..
$RSYNC -a --delete $MAILDIR $BACKUP0/
