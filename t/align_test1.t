
use strict;
use Test;

BEGIN { plan tests => 13 }

use RTF::Writer;
ok 1;

my $filename = "aligny1.rtf";
chdir "t" if -e "t";


my $doc = RTF::Writer->new_to_file($filename);
$doc->prolog;
my $t = RTF::Writer::TableRowDecl->new( 
 borders => 'none',
 align => ['ne', 'w', '', 's' ],
);

my @them = ([\'\fs50', "It's like this"], 'aa', 'ba', 'ca');

my $x = 'This module is for generating documents in Rich Text Format.';

foreach my $align ( [qw<e nw n ne>], [qw<e w c e>], [qw<e sw s se>] ) {
  $doc->row(RTF::Writer::TableRowDecl->new('align' => $align) => @them);
  for(@them) { ref($_) or ++$_ }
}

$doc->close;
undef $doc;

print "# Now checking $filename...\n";
open IN, $filename or die "Can't read-open $filename: $!";
local $/;
my $rtf = <IN>;
close(IN);
ok 1;

ok scalar( grep 1, $rtf =~ m/(\\qc)\b/g),         3;
ok scalar( grep 1, $rtf =~ m/(\\qr)\b/g),         6;
ok scalar( grep 1, $rtf =~ m/(\\ql)\b/g),         3;

ok scalar( grep 1, $rtf =~ m/(\\clvertalc)\b/g),   6;
ok scalar( grep 1, $rtf =~ m/(\\clvertalt)\b/g),   3;
ok scalar( grep 1, $rtf =~ m/(\\clvertalb)\b/g),   3;

ok scalar( grep 1, $rtf =~ m/(\\clbrdrl)\b/g),    12;
ok scalar( grep 1, $rtf =~ m/(\\clbrdrr)\b/g),    12;
ok scalar( grep 1, $rtf =~ m/(\\clbrdrt)\b/g),    12;
ok scalar( grep 1, $rtf =~ m/(\\clbrdrb)\b/g),    12;

print "# Bye\n";
ok 1;

