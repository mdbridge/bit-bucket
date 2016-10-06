#!/usr/bin/perl

use strict;
use warnings;
use Net::LDAP;         # requires installation of perl-LDAP...

sub findUser {
    my ($filter) = @_;

    # search for the LDAP user
    my $ldap = Net::LDAP->new('ldap.hp.com');
    $ldap->bind;

    my $mesg = $ldap->search( base   => "o=hp.com",
			      filter => "($filter)",
			        # remove attrs => for all attributes...
			      attrs  => ['employeeNumber',
					 'cn',
					 'l', 
					 'uid', 'mail',
					 'managerEmployeeNumber',
					 'ntUserDomainID']
	);
    if($mesg->entries > 1) {
	print "FAIL: Filter $filter is ambiguous: ";
	my $max = $mesg->count;
	for(my $i = 0; $i < $max; $i++) {
	    my $entry = $mesg->entry($i);

	    if ($entry->get_value("ntUserDomainID")) {
		print $entry->get_value("ntUserDomainID")." ";
	    }
	    print $entry->get_value("cn")." ";
	    if ($entry->get_value("l")) {
		print $entry->get_value("l")." "; 
	    }
	    print $entry->get_value("uid").";  ";
	}
	exit;
    } elsif($mesg->entries < 1) {
	print "FAIL: Filter $filter could not be found\n";
	exit;
    }


    my $entry = $mesg->entry(0);

    #print $entry->get_value("uid")."\n";
    print $entry->get_value("mail")."\n";

#    foreach my $attr ( $entry->attributes ) {
#       print join( "\n ", $attr, $entry->get_value( $attr ) ), "\n";
#    }
}

die "Must specify a filter (e.g., cn=*Oskar*Batuner*)\n"
    unless (scalar(@ARGV));
my ($filter) = @ARGV;

findUser($filter);
