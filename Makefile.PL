# This -*- perl -*- script writes the Makefile for this dist
# Time-stamp: "2001-04-24 01:49:39 MDT"
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

require 5.005; # we need m/...\z/
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'	=> 'RTF::Writer',
    'VERSION_FROM' => 'RTF/Writer.pm', # finds $VERSION
    'dist'        => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);

package MY;

sub libscan
{ # Determine things that should *not* be installed
    my($self, $path) = @_;
    return '' if $path =~ m/~/;
    $path;
}

__END__
