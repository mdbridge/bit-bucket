#!/usr/bin/perl

use strict;
use warnings;
use Net::LDAP;         # requires installation of perl-LDAP...

sub findUser {
    my ($filter) = @_;

    # search for the LDAP user
    my $ldap = Net::LDAP->new('hpe-pro-ods-ed.infra.hpecorp.net');
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

    # uid is supposed to be SEA, which is more reliable than mail,
    # which may have many entries including @hp.com ones:
    my $sea = $entry->get_value("uid");

    my $email = $sea;

    # backup: if email not a @hpe.com, switch to SEA but with @hpe.com
    # *if* that email address is in the mail entries:
    if ($email !~ /[@]hpe.com/) {
	my $target = $sea . "";
	$target =~ s/([@]hp)(.com)/$1e$2/;
	foreach my $mail ( $entry->get_value("mail") ) {
	    if ($mail eq $target) {
		$email = $mail;
	    }
	}
    }

    # backup: if email still not a @hpe.com, switch to *first* mail entry
    # with @hpe.com if any:
    if ($email !~ /[@]hpe.com/) {
	foreach my $mail ( $entry->get_value("mail") ) {
	    if ($mail =~ /[@]hpe.com/) {
		$email = $mail;
		last;
	    }
	}
    }

    print $email."\n";


#    print $entry->get_value("uid")."\n";
#

    #print $entry->dump;

#    foreach my $attr ( $entry->attributes ) {
#       print join( "\n ", $attr, $entry->get_value( $attr ) ), "\n";
#    }
}

die "Must specify a filter (e.g., cn=*Oskar*Batuner*)\n"
    unless (scalar(@ARGV));
my ($filter) = @ARGV;

findUser($filter);
