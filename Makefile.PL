
# Time-stamp: "2003-09-23 20:59:02 ADT"
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

require 5.005; # we need m/...\z/
use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'	   => 'RTF::Writer',
    'VERSION_FROM' => 'lib/RTF/Writer.pm', # finds $VERSION
    'ABSTRACT_FROM' => 'lib/RTF/Writer.pod',
    'PREREQ_PM'     => {
       'Exporter' => 0,
       'Carp'   => 0,
       #'utf8'   => 0,
       'strict' => 0,
       'UNIVERSAL' => 0,
    },
    'dist'         => { COMPRESS => 'gzip -6f', SUFFIX => 'gz', },
);

package MY;

sub libscan
{ # Determine things that should *not* be installed
    my($self, $path) = @_;
    return '' if $path =~ m/~/;
    $path;
}

__END__
