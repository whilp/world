use strict;
use vars qw($VERSION %IRSSI);
use Irssi qw(signal_add_last settings_add_str signal_stop);
use Irssi::Irc;

$VERSION = '1.0';

%IRSSI = (
    authors     => 'Tijmen "timing" Ruizendaal & Wilmer van der Gaast',
    contact     => 'tijmen@fokdat.nl',
    name        => 'BitlBee_tab_completion',
    description => 'Intelligent Tab-completion for Bitlbee commands.',
    license     => 'GPLv2',
    url         => 'http://the-timing.nl/stuff/irssi-bitlbee',
    changed     => '2005-12-05',
);

my $debug = 0; ## change this into 1 if you want to see some output in your control panel, it's not much, so don't be scared.

## Hardcoded defaults, most of these will be auto-guessed when the BitlBee server supports this.

my $root_nick = 'root';
my $bitlbee_channel = '&bitlbee';
my $getting_completions = '0';

my @commands = ('account','allow','block','blist','help','identify','info','nick','qlist','register','remove','rename','save','set');
my @setlist = ('auto_connect','auto_reconnect','auto_reconnect_delay','away_devoice','buddy_sendbuffer','buddy_sendbuffer_delay','charset','debug','handle_unknown','html','ops','private','save_on_quit','typing_notice','to_char');
my @helplist = ('away','commands','groupchats','groupchats2','groupchats3','index','quickstart','quickstart2','quickstart3','quickstart4','quickstart5','smileys');

my @accountlist = ('add','del','list','on','off');
my @blist = ('all','away','offline','online');
my @boolean = ('true', 'false');
my @handle_unknown = ('root', 'add', 'add_private', 'add_channel', 'ignore');
my @ops = ('both', 'root', 'user', 'none');
my @html = ('strip', 'nostrip');

##pfft, done with that...

my $i;

for $i ( @commands )
{
	@helplist = ( @helplist, $i );
}

signal_add_last 'channel sync' => sub {
	my( $channel ) = @_;
	my( $server ) = $channel->{server};

	if( $channel->{topic} eq "Welcome to the control channel. Type \x02help\x02 for help information." )
	{
		$bitlbee_channel = $channel->{name};
		$getting_completions = 1;
		$server->send_raw( 'COMPLETIONS' );
		if($debug == 1){
		print( 'Detected a &bitlbee: ' . $channel->{name} );
		}
	}
};

signal_add_last 'message irc notice' => sub {
	my( $server, $msg, $from, $address, $target ) = @_;
	
	## Ignore the notice if we have the completions already.
	return unless $getting_completions;
	
	if( $msg =~ s/^COMPLETIONS // )
	{
		$root_nick = $from;
		if( $msg eq 'OK' )
		{
			## We're sure that the server supports the COMPLETIONS
			## command now, so let's flush our hardcoded stuff.
			@commands = @setlist = @helplist = ();
			if($debug == 1)
			{
			print( 'COMPLETIONS fetching supported!' );
			}
		}
		elsif( $msg eq 'END' )
		{
			## Ignore further notices.
			$getting_completions = 0;
			if($debug == 1)
			{
			print( 'COMPLETIONS fetching finished!' );
			}
		}
		elsif( $msg =~ s/^help // )
		{
			@helplist = ( @helplist, $msg );
		}
		elsif( $msg =~ s/^set // )
		{
			@setlist = ( @setlist, $msg );
		}
		else
		{
			@commands = ( @commands, $msg );
		}
		
		signal_stop();
	}
};

signal_add_last 'complete word' => sub {
	my ($complist, $window, $word, $linestart, $want_space) = @_;
	my $channel = $window->get_active_name();
	if ($channel eq $bitlbee_channel or $channel eq $root_nick or $linestart =~ /^\/(msg|query) \Q$root_nick\E */i){
		$linestart =~ s/^\/(msg|query) \Q$root_nick\E *//i;
		$linestart =~ s/^\Q$root_nick\E[:,] *//i;
		if ($linestart eq ""){
			foreach my $command(@commands)
			{	
				if ($command =~ /^$word/i)
			    	{
					push @$complist, $command;
			    	}
			}
		}elsif ($linestart eq "set" or $linestart eq "help set")
		{
			foreach my $set(@setlist)
			{
				if ($set =~ /^$word/i)
			    	{
					push @$complist, $set;
	    			}
			}
		}elsif ($linestart eq "help")
		{
			foreach my $help(@helplist)
			{
				if ($help =~ /^$word/i)
			    	{
					push @$complist, $help;
	    			}
			}
		}elsif ($linestart eq "blist")
		{
			foreach my $list(@blist)
			{
				if ($list =~ /^$word/i)
			    	{
					push @$complist, $list;
	    			}
			}
		}elsif ($linestart eq "account" || $linestart eq "help account")
		{
			foreach my $account(@accountlist)
			{
				if ($account =~ /^$word/i)
				{
					push @$complist, $account;
				}
			}
		}elsif($linestart eq 'set away_devoice' || $linestart eq 'set auto_connect' || $linestart eq 'set auto_reconnect' || $linestart eq 'set buddy_sendbuffer' || $linestart eq 'set debug' || $linestart eq 'set private' || $linestart eq 'set save_on_quit' || $linestart eq 'set typing_notice')
		{
			foreach my $bool(@boolean)
			{
				if ($bool =~ /^$word/i)
				{
					push @$complist, $bool;
				}
			}
		}elsif($linestart eq 'set handle_unknown')
		{
			foreach my $handle(@handle_unknown)
			{
				if ($handle =~ /^$word/i)
				{
					push @$complist, $handle;
				}
			}
		}elsif($linestart eq 'set ops')
		{
			foreach my $op(@ops)
			{
				if ($op =~ /^$word/i)
                                {
                                        push @$complist, $op;
                                }
			}
		}elsif($linestart eq 'set html')	
		{
			 foreach my $strip(@html)
                         {
				if ($strip =~ /^$word/i)
				{
					push @$complist, $strip;
				}
			}
		}
	}
};
