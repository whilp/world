#!/usr/bin/perl
use strict;
use warnings;

use LWP::Simple;
use Net::DNS;

my (%tlds,%availableNames,%unavailableNames,$i);

my $verbose = 0;
my $max = 10;       # Don't print more than $max results
my $sourceSite = 'http://www.norid.no/domenenavnbaser/domreg.html';
my $query = "lfod";

# TLDs available from dyndns.org DNS registration.
my @dyndnsTlds = ('biz', 'cc', 'cn', 'com', 'info', 
    'name', 'net', 'org', 'tv', 'co.uk', 'me.uk', 
    'org.uk', 'us');


# my $page = get($sourceSite);

# while ( $page =~ m!.*>\.([a-z]+)<.th>!g ) {
#     if ( ! $tlds{$1} ) {
#         my $domain = "$query.$1";
#         &check_dns($domain);
#         $tlds{$1} = 1;
#     }
# }

foreach my $tld ( @dyndnsTlds ) {
    my $domain = "$query.$tld";
    &check_dns($domain);
}

# Summary
print("Searched for '$query' in ", scalar(keys(%tlds)), " TLDs.\n");
print("  ", scalar(keys(%availableNames)), " available names.\n");
print("\tAvailable names included (first $max results):\n");
my @availableArray = sort(keys(%availableNames));
for ( $i = 0; $i < $max; $i++ ) {
    if ( $availableArray[$i] ) {
        print("\t $availableArray[$i]\n");
    }
}

print("  ", scalar(keys(%unavailableNames)), " unavailable names.\n");
print("\tUnavailable names included (first $max results):\n");
my @unavailableArray = sort(keys(%unavailableNames));
for ( $i = 0; $i < $max; $i++ ) {
    if ( $unavailableArray[$i] ) {
        print("\t $unavailableArray[$i]\n");
    }
}

# &check_dns("lfasdfakljsdf.com");
sub notify {
    my $level = shift;
    my $message = shift;
    if ( $verbose ge $level ) {
        print("===> $message\n");
    }
}
sub check_dns {
    my $domain = shift;
    my $res = Net::DNS::Resolver->new;
    my $dnsQuery = $res->search($domain);
    if ( $dnsQuery ) {
        foreach my $answer ( $dnsQuery->answer ) {
            if ( $answer->type eq "A" ) {
                &notify(1, "Domain '$domain' has an A record.");
                if ( ! $unavailableNames{$domain} ) {
                    $unavailableNames{$domain} = 1;
                }
                next;
            }
        }
    } else {
        &notify(1, "Query for '$domain' unsuccessful.");
        if ( ! $availableNames{$domain} ) {
            $availableNames{$domain} = 1;
        }
    }
}
