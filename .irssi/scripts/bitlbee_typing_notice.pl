# INSTALLATION
# [&bitlbee] set typing_notice true
# <@root> typing_notice = `true'
# AND
# /statusbar window add typing_notice
#
# Don't forget to change the $bitlbee_* variables below.
#
# SETTINGS
# [bitlbee]
# bitlbee_send_typing =
#   -> send typing messages to buddies
# bitlbee_typing_allwin = OFF
#   -> show typing notifications in all windows
#
# 
# Changelog:
#
# 2005-12-15 (version 1.42):
# * Fixed small bug with typing notices disappearing under certain circumstances
#   in channels
# * Fixed bug that caused outgoing notifications not to work 
# * root cares not about our typing status.
#
# 2005-12-04 (version 1.41):
# * Implemented stale states in statusbar (shows "(stale)" for OSCAR connections) 
# * Introduced bitlbee_typing_allwin (default OFF). Set this to ON to make
#   typing notifications visible in all windows.
#
# 2005-12-03 (version 1.4):
# * Major code cleanups and rewrites for bitlbee 1.0 with the updated typing
#   scheme. TYPING 0, TYPING 1, and TYPING 2 are now supported from the server.
# * Stale states (where user has typed in text but has stopped typing) are now
#   recognized.
# * Bug where user thinks you are still typing if you close the window after
#   typing something and then erasing it quickly.. fixed.
# * If a user signs off while they are still typing, the notification is removed
# This update by Matt "f0rked" Sparks
#
# 2005-08-26:
# Some fixes for AIM, Thanks to Dracula.
#
# 2005-08-16:
# AIM supported, for sending notices, using CTCP TYPING 0. (Use the AIM patch from Hanji http://get.bitlbee.org/patches/)
# 
# 2004-10-31:
# Sends typing notice to the bitlbee server when typing a message in irssi. bitlbee > 0.92
#
# 2004-06-11:
# shows [typing: ] in &bitlbee with multiple users.
#
use strict;
use Irssi::TextUI;

use vars qw($VERSION %IRSSI);

$VERSION = '1.42';
%IRSSI = (
    authors     => 'Tijmen "timing" Ruizendaal, Matt "f0rked" Sparks',
    contact     => 'tijmen@fokdat.nl, root@f0rked.com',
    name        => 'BitlBee_typing_notice',
    description => '1. Adds an item to the status bar wich shows [typing] when someone is typing a message on the supported IM-networks
                    2. Sending typing notices to the supported IM networks (the other way arround)',
    license     => 'GPLv2',
    url         => 'http://the-timing.nl/stuff/irssi-bitlbee,
                    http://f0rked.com',
    changed     => '2005-12-17',
);

my $bitlbee_channel = "&bitlbee";
my $bitlbee_server_tag = "localhost";

my $KEEP_TYPING_TIMEOUT = 1;
my $STOP_TYPING_TIMEOUT = 7; # How often to check if we are typing, or on msn,
                             # how long to keep the typing notice up, or check
                             # if the other user is still typing...

my %timer_tag;

my %typing;
my %tag;
my $line;
my %out_typing;
my $lastkey;
my $keylog_active = 1;
my $command_char = Irssi::settings_get_str('cmdchars');
my $to_char = Irssi::settings_get_str("completion_char");

sub chan_sync {
    my($channel) = @_;
    if ($channel->{topic} eq "Welcome to the control channel. Type \x02help\x02 for help information.") {
        #print Dumper($channel);
        $bitlbee_server_tag = $channel->{server}->{tag};
        $bitlbee_channel = $channel->{name};
    }
}

sub event_ctcp_msg {
    my ($server, $msg, $from, $address) = @_;
    #print "CTCP: $msg $from $address";
    return if $server->{tag} ne $bitlbee_server_tag;
    if (my($type) = $msg =~ "TYPING ([0-9])") {
        Irssi::signal_stop();
        if ($type == 0) {
            unset_typing($from);
        }
        elsif ($type == 1) {
            $typing{$from}=1;
            if ($address !~ /\@login\.oscar\.aol\.com/
                and $address !~ /\@YAHOO/
                and $address !~ /\@login\.icq\.com/) {
                Irssi::timeout_remove($tag{$from});
                $tag{$from}=Irssi::timeout_add_once($STOP_TYPING_TIMEOUT*1000,"unset_typing",$from);
            }
            redraw($from);
        }
        elsif ($type == 2) {
            stale_typing($from);
        }
    }
}

sub unset_typing {
    my($from,$no_redraw)=@_;
    delete $typing{$from} if $typing{$from};
    Irssi::timeout_remove($tag{$from});
    redraw($from) if !$no_redraw;
}

sub stale_typing {
    my($from)=@_;
    $typing{$from}=2;
    redraw($from);
}

sub redraw {
    my($from)=@_;
    my $window = Irssi::active_win();
    my $channel = $window->get_active_name();
    if ($from eq $channel || $channel eq $bitlbee_channel
        || $channel =~ /&chat_0/ 
        || Irssi::settings_get_bool("bitlbee_typing_allwin")) {
        Irssi::statusbar_items_redraw('typing_notice');
    }
}    

sub event_msg {
    my ($server,$data,$from,$address,$target) = @_;
    return if $server->{tag} ne $bitlbee_server_tag;
    my $channel=Irssi::active_win()->get_active_name();
    unset_typing $from, "no redraw";
    unset_typing $channel;
}

sub event_quit {
    my($server,$nick,$address,$reason)=@_;
    return if $server->{tag} ne $bitlbee_server_tag;
    unset_typing $nick;
}

sub typing_notice {
    my ($item, $get_size_only) = @_;
    my $window = Irssi::active_win();
    my $channel = $window->get_active_name();
    
    if (exists($typing{$channel})) {
        my $append=$typing{$channel}==2 ? " (stale)" : "";
        $item->default_handler($get_size_only, "{sb typing$append}", 0, 1);
    }
    else {
        $item->default_handler($get_size_only, "", 0, 1);
        Irssi::timeout_remove($tag{$channel});
    }
    if ($channel eq $bitlbee_channel || $channel =~ /&chat_0/
        || Irssi::settings_get_bool("bitlbee_typing_allwin")) {
        foreach my $key (keys(%typing)) {
            $line .= " ".$key;
            if ($typing{$key}==2) { $line .= " (stale)"; }
        }
        if ($line ne "") {
            $item->default_handler($get_size_only, "{sb typing:$line}", 0, 1);
            $line = "";
        }
    } 
}

sub empty {
    my $from = shift;
    delete($typing{$from});
    Irssi::statusbar_items_redraw('typing_notice');
}

sub window_change {
    Irssi::statusbar_items_redraw('typing_notice');
    my $win = !Irssi::active_win() ? undef : Irssi::active_win()->{active};
    if (ref $win && ($win->{server}->{tag} eq $bitlbee_server_tag)) {
        if (!$keylog_active) {
            $keylog_active = 1;
            Irssi::signal_add_last('gui key pressed', 'key_pressed');
            #print "Keylog started";
        }
    } 
    else {
        if ($keylog_active) {
            $keylog_active = 0;
            Irssi::signal_remove('gui key pressed', 'key_pressed');
            #print "Keylog stopped";
        }
    }
}

sub key_pressed {
    return if !Irssi::settings_get_str("bitlbee_send_typing");
    my $key = shift;
    if ($key != 9 && $key != 10 && $lastkey != 27 && $key != 27 
       && $lastkey != 91 && $key != 126 && $key != 127) 
    {
        my $server = Irssi::active_server();
        my $window = Irssi::active_win();
        my $nick = $window->get_active_name();
        if ($server->{tag} eq $bitlbee_server_tag && 
            $nick ne "(status)" &&
            $nick ne "root") 
        {
            if ($nick eq $bitlbee_channel) {
                my $input = Irssi::parse_special("\$L");
                my ($first_word) = split(/ /,$input);
                if ($input !~ /^$command_char.*/ && $first_word =~ s/$to_char$//){
                    send_typing($first_word);
                }
            } 
            else {
                my $input = Irssi::parse_special("\$L");
                if ($input !~ /^$command_char.*/ && length($input) > 0){
                    send_typing($nick);
                }
            }
        }
    }
    $lastkey = $key;
}

sub out_empty {
    my ($a) = @_;
    my($nick,$tag)=@{$a};
    delete($out_typing{$nick});
    #print $winnum."|".$nick;
    if (my $server=Irssi::server_find_tag($tag)) {
        $server->command("^CTCP $nick TYPING 0");
    }
}

sub send_typing {
    my $nick = shift;
    if (!exists($out_typing{$nick}) || time - $out_typing{$nick} > $KEEP_TYPING_TIMEOUT) {
        my $server = Irssi::active_server();
        $server->command("^CTCP $nick TYPING 1");
               
        $out_typing{$nick} = time;
        
        ### Reset 'stop-typing' timer
        if ($timer_tag{$nick}) {
            Irssi::timeout_remove($timer_tag{$nick});
            delete($timer_tag{$nick});
        }
        $timer_tag{$nick} = Irssi::timeout_add_once($STOP_TYPING_TIMEOUT*1000, 'out_empty', ["$nick", $server->{tag}]);
    }
}

Irssi::settings_add_str("bitlbee","bitlbee_send_typing",undef);
Irssi::settings_add_bool("bitlbee","bitlbee_typing_allwin",0);

Irssi::signal_add("ctcp msg", "event_ctcp_msg");
Irssi::signal_add("message private", "event_msg");
Irssi::signal_add("message public", "event_msg");
Irssi::signal_add("message quit", "event_quit");
Irssi::signal_add_last('window changed', 'window_change');
Irssi::signal_add_last('gui key pressed', 'key_pressed');
Irssi::signal_add_last('channel sync','chan_sync');
Irssi::statusbar_item_register('typing_notice', undef, 'typing_notice');
Irssi::statusbars_recreate_items();
