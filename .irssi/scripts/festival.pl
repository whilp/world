# Talking irssi gadget (c) Petr Baudis <pasky@ucw.cz>, BSD licence.

use strict;

use vars qw($forked $wh $type $lang);

use Irssi;
use Irssi::Irc;
use Speech::Synthesiser;

# Spawn a helper which will feed our Festival backend, so that we do not
# block the main irssi process while pushing data all around.
sub fork_me {
  my ($rh, $pid);
  pipe($rh, $wh);
  $forked = 1;
  $pid = fork( );
  if ($pid > 0) {
    # The main irssi process
    close $rh;
    # This makes sure we do not get a zombie
    Irssi::pidwait_add($pid);
    return;
  } else {
    # The helper child
    close($wh);
    my $synth = new Speech::Synthesiser(-type => $type);
    start $synth;
    if ($lang) { voice $synth $lang; }
    while (my $in = <$rh>) {
      chomp $in;
      speak $synth $in;
    }
    stop $synth;
    close($rh);
    POSIX::_exit(0);
  }
}

# The incoming message event handler.
sub event_privmsg {
  my ($server, $data, $nick, $address) = @_;
  my ($msgtarget, $text) = split(/ :/, $data, 2);
  my (@channels) = split(/\s+/, Irssi::settings_get_str('speech_channels'));

  # The ~ substitution
  return unless (grep {s/^~$/$server->{nick}/x; $_ eq $msgtarget} @channels);

  # Restart the backend if something changed.
  my ($otype, $olang) = ($type, $lang);
  $type = Irssi::settings_get_str('speech_backend');
  $lang = Irssi::settings_get_str('speech_language');
  if ($forked and ($type ne $otype or $lang ne $olang)) {
    print $wh "\n";
    close($wh);
    $forked = 0;
  }
  if (!$forked) {
    fork_me( );
  }

  # Some emoticon replacements (e.g. ":-)"->"hehe!"
  # add your own if you need more!
  $text =~ s/:.?\)/hehe!/g;
  $text =~ s/:.?\(/sniff/g;

  # The exclamation point helps to get the right intonation.
  print $wh "$nick! $text\n";
}

# Our command interface.
sub cmd_speech {
  my ($cmd) = @_;
  if ($cmd =~ /^languages/i) {
    my $synth = new Speech::Synthesiser(
                      -type => Irssi::settings_get_str('speech_backend'));
    start $synth;
    my @voices = voice_list $synth;
    Irssi::print("These languages are supported: @voices");
    stop $synth;
  }
}

Irssi::command_bind('speech', \&cmd_speech);
Irssi::signal_add("event privmsg", "event_privmsg");
Irssi::settings_add_str('speech', 'speech_backend', 'Festival');
Irssi::settings_add_str('speech', 'speech_language', '');
Irssi::settings_add_str('speech', 'speech_channels', '~ #irchacks');
