#!/usr/bin/perl
#
# $Id: mklocal.pl,v 1.1 2006/06/03 16:20:56 will Exp $
#

$force = 0;
$rw = 0;

while ( $ARGV[0] =~ /^-/ ) {
  if ( $ARGV[0] eq '-h' ) { &usage; exit; }
  if ( $ARGV[0] eq '-f' ) { $force = 1; shift @ARGV; next; }
  if ( $ARGV[0] eq '-rw' ) { $rw = 1; shift @ARGV; next; }
  print "mklocal: unknown arg: $ARGV[0]\n";
  &usage; 
  exit 1;
}

if ( $#ARGV != 0 ) {
  &usage;
  exit 1;
}

$src = $ARGV[0];

if ( ! -f "$src" && ! -d "$src" ) { 
  print "mklocal: $src: No such file or directory\n";
  exit 1;
}

$dst = "$src.orig";

if ( ! $force && ( -f "$dst" || -d "$dst" ) ) {
  $base = $dst;
  $i = 1;
  while ( -f "$dst" || -d "$dst" ) {
    $dst = "$base.$i";
    $i++;
  }
}

if ( $force ) {
  print "/bin/rm -rf $dst\n";
  print `/bin/rm -rf $dst\n`;
}

print "/bin/mv $src $dst\n";
print `/bin/mv $src $dst`;
if ( $rw ) {
  print "/bin/cp $dst $src\n";
  print `/bin/cp $dst $src`;
  print "/bin/chmod u+w $src\n";
  print `/bin/chmod u+w $src`;
} else {
  print "/bin/cp -R -p $dst $src\n";
  print `/bin/cp -R -p $dst $src`;
  print "/usr/bin/find $src -exec touch {} \\;\n";
  print `/usr/bin/find $src -exec touch {} \\;`;
}

exit;

#------------------------------------------------------------------

sub usage {
print <<EOT;
usage: mklocal [args] FILE
  -f   over-write the destination
  -rw  make destination writable
  -h   for this (help) message
EOT
}
