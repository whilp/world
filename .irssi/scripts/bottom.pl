use strict;
use warnings;
use vars qw($VERSION %IRSSI);
use Irssi 20020324;

$VERSION = "0.02";
%IRSSI = (
  name        => "bottom.pl",
  description => "makes text in new windows appear at the bottom instead of the top",
  url         => "http://explodingferret.com/linux/irssi/bottom.pl",
  authors     => "ferret",
  contact     => "ferret tA xelam teNtoD",
  licence     => "Public Domain",
  changed     => "2010-02-04",
  changes     => "modified from 0.01 to support /clear as well",
  modules     => "",
  commands    => "",
);

Irssi::print "$IRSSI{name} version $VERSION loaded.";

sub window_to_bottom($)
{
  my $winrec = shift;
  for ( 1..( $winrec->{height} ) ) {
    $winrec->print( '', MSGLEVEL_NEVER )
  }
}

Irssi::signal_add_last 'window created' => sub {
  my ( $winrec ) = @_;
  &window_to_bottom($winrec);
};

Irssi::signal_add_last 'command clear' => sub {
  my $winrec = Irssi::active_win();
  &window_to_bottom($winrec);
};
