
use strict;
use Test;

BEGIN { plan tests => 7 }

use RTF::Writer;
ok 1;

chdir "t" if -e "t";

my $f = "bordery.rtf";

my $doc = RTF::Writer->new_to_file($f);
$doc->prolog;
my $t = RTF::Writer::TableRowDecl->new( 

 borders => 't-20 l-33-wavy', #['n s e', 'n-wavy s-wavy'],

);

my $x = 'This module is for generating documents in Rich Text Format.';
$doc->row($t, $x, $x, "$x $x", $x);
$doc->row($t, $x, $x, $x,      $x);

$doc->close;
undef $doc;

ok 1;
{
  my $rtf;
  open IN, $f or die $!;
  local $/;
  $rtf = <IN>;
  close(IN);
  ok $rtf, '/\\\\brdrs\\b/';
  ok $rtf, '/\\\\brdrwavy\\b/';
  ok scalar( grep 1, $rtf =~ m/(\\brdrs)\b/g),    8;
  ok scalar( grep 1, $rtf =~ m/(\\brdrwavy)\b/g), 8;
}

print "# Bye...\n";
ok 1;
