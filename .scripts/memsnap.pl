#!/usr/bin/perl

use strict;
use warnings;

# We'll need to explore the process table...
use Proc::ProcessTable;

# Options
my $verbose     = 1;
my $logFile     = '/var/log/memsnap.log';
my $targetUser  = 'leonard';

# Create a table object; the object doesn't need to be refreshed --
# it will continuously reflect the realtime status of the system.
# $procTable is a reference to an array of process objects.
# The following fields are available (see $tobj->fields):
#   uid, gid, pid, ppid, pgrp, sess, priority, ttynum, flags,
#   minflt, cminflt, majflt, cmajflt, utime, stime, cutime, cstime,
#   time, ctime, size, rss, wchan, fname, start, pctcpu, state,
#   pctmem, cmndline, exec, euid, suid, fuid, egid, sgid, fgid, cwd

my $tobj = new Proc::ProcessTable;
my $procTable = $tobj->table();

# The 'fname' field seems to be truncated at 15 chars.
for ( @$procTable ) {
    my $pid = $_->pid;
    my $uid = $_->uid;
    my $name = $_->fname;
    my $size = $_->size;
    my $created = $_->start;
    my $targetUid = (getpwnam($targetUser))[2];

    if ( $uid eq $targetUid ) {
        my $logLine = "$pid:$uid:$name:$created";
        &notify($logLine);
    }
}

# Subroutines
sub notify {
    my $level;
    # If the user didn't supply a notification level, assume 1.
    if ( scalar @_ lt 2 ) {
        $level = $_[0];
    } else {
        $level = 1;
    }
    my $message = shift;

    if ( $level ge $verbose ) {
        print("===> $message\n");
    }
}
sub warn {
    my $message = shift;
    print("=!=> $message\n");
}
