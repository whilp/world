use strict;
use vars qw($VERSION %IRSSI);
use Irssi;

$VERSION = '0.2';
%IRSSI = (
    authors	=> 'Tijmen "timing" Ruizendaal',
    contact	=> 'timing@fokdat.nl',
    name	=> 'bitlbee_blist',
    description	=> '/blist <all|online|offline|away> <word>,  greps <word> from blist for bitlbee',
    license	=> 'GPLv2',
    url		=> 'http://the-timing.nl/stuff/irssi-bitlbee',
    changed	=> '2005-12-05',
);

my ($list, $word);
my $bitlbee_server_tag = "localhost";
my $bitlbee_channel = "&bitlbee";

Irssi::signal_add_last 'channel sync' => sub {
  my( $channel ) = @_;
  if( $channel->{topic} eq "Welcome to the control channel. Type \x02help\x02 for help information." ){
    #print Dumper($channel);
    $bitlbee_server_tag = $channel->{server}->{tag};
    $bitlbee_channel = $channel->{name};
  }
};

sub blist {
  my ($args, $server, $winit) = @_;
  ($list, $word) = split(/ /, $args, 2);
  if (Irssi::active_win->{'active'}->{'name'} eq $bitlbee_channel) {
    Irssi::active_win()->command("msg $bitlbee_channel blist $list");
    Irssi::signal_add('event privmsg', 'grep');  
  } else {
    print "Only use in &bitlbee channel please.";
  }
}
sub grep {
  my ($server, $data, $nick, $address) = @_;
  my ($target, $text) = split(/ :/, $data, 2);
  if ($text =~ /$word/ && $target =~ /$bitlbee_channel/){
  ##do nothing
  } else {Irssi::signal_stop();}
  if ($text =~ /buddies/ && $target =~/$bitlbee_channel/){Irssi::signal_remove('event privmsg', 'grep');} 
}
Irssi::command_bind('blist','blist');
