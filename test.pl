
require 5.005; # we need m/...\z/
# Time-stamp: "2001-04-24 02:02:50 MDT"

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..2\n"; }
END {print "not ok 1\n" unless $loaded;}
use RTF::Writer ('rtfesc');

$loaded = 1;
print "ok 1\n";

print "RTF::Writer version: $RTF::Writer::VERSION\n";

# Pretty elementary test.

my $x = rtfesc("foo{\\\n");
if($x eq "foo\\'7b\\'5c\n\\line ") {
  print "ok 2\n";
} else {
  print "fail 2 <$x>\n"
}

$x = '';
my $r = RTF::Writer->new_to_string(\$x);
$r->print("foo{\\\n");
$r->close;
if($x eq "foo\\'7b\\'5c\n\\line ") {
  print "ok 3\n";
} else {
  print "fail 3 <$x>\n"
}


###########################################################################
$| = 1;

