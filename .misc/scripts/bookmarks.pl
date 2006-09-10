#!/usr/local/bin/perl -w

use strict;
use IO::File;

my $ffdir = "/home/will/.mozilla/firefox/";
my $file = "bookmarks.html";

my $bookmarks = join('', $ffdir, $file);

my $f = new IO::File($bookmarks, 'r');

my %links = {};
while (<$f>) {
    my $urlRe = 'HREF="([^"]+)".';
    my $titleRe = '">([^<]+)<\/a>';
    if ( $_ =~ /HREF="(.+?)".*?>(.*?)</ ) {
	$links{$1} = $2;
    }
}
undef $f;

while (my ($url, $title) = each(%links)) {
    print("$title\t$url\t0\n");
}
