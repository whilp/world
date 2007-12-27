# TTY_AWAY.PL
# vim: set noet sts=0:
# /SET
#	autoaway_msg
#		if you are not away and type /away without arguments, this string will
#		be used as the default away reason.
#	autoaway_sec
#		number of seconds before marking away.
#	autoaway_verbose
#		verbosity level: currently, an integer on the range [0,2], where 2 is
#		very verbose, and 0 is silent.
#	autoaway_poll
#		polling interval -- number of miliseconds between
#		stats of the system ttys.

use strict;
use Irssi;
use Irssi::TextUI;
use File::Find;

our $VERSION = "0.01";
our %IRSSI = (
	authors		=> 'greg smith',
	contact		=> 'gasmith@gmail.com',
	name		=> 'tty_away',
	description	=> 'an auto-away script that monitors tty devices',
	license		=> 'BSD',
	note1		=> 'thanks: will maier (willmaier@ml1.net) and larry daffner (vizzie@airmail.net)',
);

# internal state vars & constants
my ($wait_time, $is_idle, $is_away, $override, $tout_ptr, $away_msg,
    $verbose, $poll_int);

my $user     = getlogin();
my $my_uid   = getpwnam($user) or die;
my $tty_gid  = getgrnam("tty") or die;

# ----------------------------------------------- launch 
Irssi::settings_add_str("misc", "autoaway_msg", "I'm away from the terminal. (autoaway)");
Irssi::settings_add_int("misc", "autoaway_sec", 0);
Irssi::settings_add_int("misc", "autoaway_verbose", 0);
Irssi::settings_add_int("misc", "autoaway_poll", 5000);
Irssi::signal_add("setup changed" => \&ta_checkconf);
Irssi::command_bind("safeaway", "ta_setsafeaway");

ta_checkconf();

# ----------------------------------------------- subroutines
sub ta_checkconf {
	$away_msg  = Irssi::settings_get_str("autoaway_msg");
	$wait_time = Irssi::settings_get_int("autoaway_sec");
	$verbose   = Irssi::settings_get_int("autoaway_verbose");
	$poll_int  = Irssi::settings_get_int("autoaway_poll");
	ta_set_timer();
}

sub ta_set_timer {
	if (defined($tout_ptr)) {
		Irssi::timeout_remove($tout_ptr);
		$tout_ptr = undef;
	}
	if (($wait_time != 0) && (not $override)) {
		$tout_ptr = Irssi::timeout_add($poll_int, "ta_check_ttys", "");
	}
}	

sub ta_wanted {
	my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
		$atime,$mtime,$ctime,$blksize,$blocks) = lstat($_);
	my $tty_name = $File::Find::name;
	
	if (($uid == $my_uid) and ($tty_name =~ /\/dev\/tty.*|pts\/.*/)) {
		if ((time() - $atime) < $wait_time) {
			$is_idle = 0;
			my $idle = time() - $atime;

			if ($verbose > 1) {
				Irssi::print("-- tty: $File::Find::name, idle: $idle sec"); 
			}
		}
	}
}

sub ta_check_ttys {
	if ($wait_time != 0) {
		$is_idle = 1;
		if ($verbose > 1) {
			Irssi::print("Polling ttys for $user...");
		}
		File::Find::find ({wanted=>\&ta_wanted}, '/dev');
	}

	if    (($is_idle) && (not $is_away))	{ ta_setaway($away_msg); }
	elsif (($is_away) && (not $is_idle))	{ ta_setaway(''); }

	ta_set_timer();
}

sub ta_setaway {
	my ($arg) = @_;
	my ($status_msg, @servers);

	if ($arg) {
		$status_msg = "TTY_AWAY: i'm setting you as away.";
		$is_away = 1;
	}
	else {
		$status_msg = "TTY_AWAY: glad to see you're back.";
		$is_away = 0;
	}

	# Counter-intuitively, the lines below do NOT issue the 'away' command to
	# the first server exclusively. Instead, it issues it to Irssi... so it's
	# the same as if you would simply type '/away [msg]' in Irssi. IE, it will,
	# by default, send the command to all servers.
	
	if ($verbose > 0) {
		Irssi::print($status_msg);
	}
	@servers = Irssi::servers();
	#$servers[0]->command("AWAY $arg");
	for my $server (@servers) {
		if ($server->{chat_type} !~ /^icb$/i) {
			$server->command("AWAY $arg");
		}
	}
}

sub ta_setsafeaway {
	my ($arg) = @_;
	my ($status_msg, @servers);
	
	if ($arg) {
		$is_away	= 1;
		$override	= 1;	# halt the timer juggernaut
		$status_msg = "TTY_AWAY: safe away was set; no longer monitoring for idleness.";
	}
	else {
		$is_away	= 0;
		$override	= 0;
		$status_msg = "TTY_AWAY: safe away was removed; monitoring for idleness.";
	}

	if ($verbose > 0) {
		Irssi::print($status_msg);
	}
	@servers = Irssi::servers();
	for my $server (@servers) {
		if ($server->{chat_type} !~ /^icb$/i) {
			$server->command("AWAY $arg");
		}
	}

	ta_set_timer();		# will start/stop timer, depending on $override
}
