
require 5;
package RTF::Writer::TableRowDecl;
use strict;  # Time-stamp: "2001-07-27 13:49:47 MDT"
use Carp ();

#sub DEBUG () {1};
#--------------------------------------------------------------------------
#   INSIDES:
#    0: the right-ends table
#    1: the left-margin setting
#    2: the inbetween setting

sub new {
  my($it, %h) = @_;
  my $new;

  my(@reaches);
  if(ref $it) { # clone
    $new = $it->clone();
  } else {
    $new = bless [
      \@reaches,
      int( $h{'left_start'} || 0) || 0,
      int( $h{'inbetween' } || 0) || 120,  # 6 points, 1/12th-inch, about 2mm
    ];
  }
  
  my $x; # scratch
  if($x = $h{'widths'}) {
    Carp::croak("'widths' value has to be an arrayref")
     unless ref($x) eq 'ARRAY';
    my $start = $new->[1];
    foreach my $w (map int($_), @$x) {
      push @reaches, ($start += ($w < 1 ) ? 1 : $w);
    }
  } elsif($x = $h{'reaches'}) {
    Carp::croak("'reaches' value has to be an arrayref")
     unless ref($h{'reaches'}) eq 'ARRAY';
    @reaches = sort {$a <=> $b} map int($_), @$x;
  }

  return $new;
}

#--------------------------------------------------------------------------

sub clone {
  # sufficient to our task, I think
  bless [ map {;
            (!defined $_) ? undef
            : (ref($_) eq 'ARRAY') ? [@$_]
            : (ref($_) eq 'HASH' ) ? {%$_}
            : $_
          } @{$_[0]}
        ],
        ref $_[0];
}

#--------------------------------------------------------------------------

sub new_auto_for_rows {
  my $class = shift;
  my $max_cols = 1;
  foreach my $r (@_) {
    next unless defined $r and ref $r eq 'ARRAY';
    $max_cols = @$r if @$r > $max_cols;
  }
  return
   $class->new( 'width' => [ ((6.5 * 1440) / $max_cols) x scalar(@_) ] );
}

#--------------------------------------------------------------------------

sub decl_code {
  my $it = shift;
  my $reaches = $it->[0];
  my $cell_count = int($_[0] || 0) || scalar @$reaches;
  
  if($cell_count > @$reaches) {
    # Uncommon case -- we need to ad-hoc pad this decl.
    $reaches = [@$reaches];   # so we won't mutate the original
    while(@$reaches < $cell_count) {
      if(@$reaches == 0) {
        push @$reaches, $it->[1] + 1440;
         # sane and noticeable default width, I think: 1 inch, 2.54cm
      } elsif(@$reaches == 1) {
        push @$reaches, 2 * $reaches->[0] - $it->[1];
         # The left-margin setting
      } else {
        push @$reaches, 2 * $reaches->[-1] - $reaches->[-2];
          # i.e., last + (last - one_before)
        # DEBUG and printf "Improvised the width %d based on %d,%d\n",
        #    $reaches->[-1], $reaches->[-3], $reaches->[-2];
      }
    }
  }
  
  return \join '',
    sprintf('\trowd\trleft%d\trgaph%d', $it->[1], $it->[2]), 
    map(sprintf("\\cellx%d", $_), @$reaches),
  ;
}

#--------------------------------------------------------------------------
1;

__END__

=head1 NAME

RTF::Writer::TableRowDecl - class for RTF table settings

=head1 SYNOPSIS

  # see RTF::Writer

=head1 DESCRIPTION

See L<RTF::Writer|RTF::Writer>.

=head1 AUTHOR

Sean M. Burke, E<lt>sburke@cpan.orgE<gt>

=cut

# so there.

