
use strict;
use Test;

BEGIN { plan tests => 6 }

use RTF::Writer;
ok 1;

chdir "t" if -e "t";

foreach my $i (qw(png jpg)) {
  my $filename = "hypnocat2_$i.rtf";

  print "# Writing to $filename...\n";
  my $rtf = RTF::Writer->new_to_file($filename);
  $rtf->prolog( 'title' => "Greetings, $i hyoomon" );
  $rtf->number_pages;
  $rtf->paragraph(
    \'\fs40\b\i',  # 20pt, bold, italic
    "Hi there!"
  );

  $rtf->paragraph(
    "Blah blah blah ",
    $rtf->image('filename' => "hypnocat.$i"),
    " blah blah -- it's a $i!",
  );
  
  $rtf->close;
  ok 1;

  undef $rtf;
  {
    print "# Now checking $filename...\n";
    open IN, $filename or die "Can't read-open $filename: $!";
    local $/;
    my $rtf = <IN>;
    close(IN);
    
    ok $rtf, '/\\\\pict/';  # simple sanity
  }
}

print "# Byebye\n";
ok 1;

