
require 5.005;   # we need m/...\z/
use strict;      # Time-stamp: "2001-05-16 21:49:30 MDT"
package RTF::Writer;
use vars qw($VERSION @ISA @EXPORT_OK
            $AUTOLOAD $AUTO_NL $WRAP %Escape $Unicode);

$AUTO_NL = 1 unless defined $AUTO_NL;     # TODO: document
$WRAP    = 1 unless defined $WRAP;        # TODO: document
$Unicode = 0;  # USE OF THIS IS NOT YET IMPLEMENTED (nor documented)

require Exporter;
@ISA = ('Exporter');
$VERSION = '1.03';
@EXPORT_OK = qw( inch inches in point points pt cm rtfesc );

sub DEBUG () {0}
use Carp  ();
use RTF::Writer::TableRowDecl ();


#**************************************************************************
# Not yet documented, and use of $Unicode is not yet implemented

sub CHARSET_LATIN1 {
  $Escape{"\xA0"} = "\\~";
  $Escape{"\xAD"} = "\\-";
  $Unicode = 0;
  return;
}

sub CHARSET_UNICODE {
  $Escape{"\xA0"} = "\\~";
  $Escape{"\xAD"} = "\\-";
  $Unicode = 1;
    # In future versions, this should be used
    #  as a flag to enable doing the Right Thing.
  return;
}

sub CHARSET_OTHER {
  $Escape{"\xA0"} = "\\'a0";
  $Escape{"\xAD"} = "\\'ad";
  $Unicode = 0;
  return;
}

#--------------------------------------------------------------------------
# Init:

%Escape = (
  map( (chr($_),chr($_)),       # things not apparently needing escaping
       0x20 .. 0x7E ),
  map( (chr($_),sprintf("\\'%02x", $_)),    # apparently escapeworthy things
       0x00 .. 0x1F, 0x5c, 0x7b, 0x7d, 0x7f .. 0xFF),
       
  # And some refinements:
  
  "\cm"  => '',
  "\cj"  => '',
  "\n"   => "\n\\line ",
   # This bit of voodoo means that whichever of \cm | \cj isn't synonymous
   #  with \n, is aliased to empty-string, and whichever of them IS "\n",
   #  turns into the "\n\\line ".
  
  "\t"   => "\\tab ",     # Tabs (altho theoretically raw \t's might be okay)
  "\f"   => "\n\\page\n", # Formfeed
  "-"    => "\\_",        # Turn plaintext '-' into a non-breaking hyphen
                          #   I /think/ that's for the best.
  "\xA0" => "\\~",        # \xA0 is Latin-1/Unicode non-breaking space
  "\xAD" => "\\-",        # \xAD is Latin-1/Unicode soft (optional) hyphen
);

#--------------------------------------------------------------------------

# The conversion functions, for export:
sub inch   { int(.5 + $_[0] * 1440) }
sub inches { int(.5 + $_[0] * 1440) }
sub in     { int(.5 + $_[0] * 1440) }
sub points { int(.5 + $_[0] *   20) }
sub point  { int(.5 + $_[0] *   20) }
sub pt     { int(.5 + $_[0] *   20) }
sub cm     { int(.5 + $_[0] * (1440 / 2.54) ) } # approx 567

sub rtfesc {
  # Note that this doesn't apply our wrapping algorithm, because
  # I don't forsee this being used for many-line things.
  
  shift if @_ and ref($_[0] || '') and UNIVERSAL::isa($_[0], __PACKAGE__);
    # that's so we can double as a method
  
  my $x; # scratch
  if(!defined wantarray) { # void context: alter in-place!
    for(@_) {
         s/([\x00-\x1F\-\\\{\}\x7F-\xFF])/$Escape{$1}/g;  # ESCAPER
    }
    return;
  } elsif(wantarray) {  # return an array
    return map {; ($x = $_) =~
         s/([\x00-\x1F\-\\\{\}\x7F-\xFF])/$Escape{$1}/g;  # ESCAPER
         $x;
        } @_;
  } else { # return a single scalar
    ($x = ((@_ == 1) ? $_[0] : join '', @_)
    ) =~
         s/([\x00-\x1F\-\\\{\}\x7F-\xFF])/$Escape{$1}/g;  # ESCAPER
             # Escape \, {, }, -, control chars, and 7f-ff.
    return $x;
  }
}


#**************************************************************************

sub new_to_file {
  # just a wrapeer around new_to_fh
  my $class = shift;
  defined $_[0] or Carp::croak "undef isn't a good filename for new_to_file";
  length $_[0] or Carp::croak "\"\" isn't a good filename for new_to_file";
  local(*FH);
  open(FH, ">$_[0]") or Carp::croak "Can't write-open $_[0]: $1";
  DEBUG and print "Opened-file $_[0] -> ", *FH{IO}, "\n";
  my $new = $class->new_to_fh(*FH{IO});
  return $new;
}

sub new_to_fh {
  Carp::croak "Open to what filehandle?"
   unless defined $_[1] and length $_[1];
  my $fh = $_[1];
  DEBUG and print "Opened-fh $fh\n";

  my $class = shift;
  my $last_was_command = 0;
  my $new = bless [
    _make_emitter_closure($fh),
    '', # things to be printed, on closing
    $fh,
  ], ref($class) || $class;
  return $new;
}

sub new_to_string {
  Carp::croak "Open to what scalar-ref?"
   unless defined $_[1] and ref($_[1]) eq 'SCALAR';
  my($class, $sr) = @_;
  DEBUG and print "Opened-sr $sr\n";

  my $new = bless [
    _make_emitter_closure(undef,$sr),
    '', # things to be printed, on closing
    undef,
  ], ref($class) || $class;
  return $new;
}

#**************************************************************************
# Think twice before outright overriding this method:

sub print {
  ref $_[0] or Carp::croak(__PACKAGE__ .
   "'s print(...) is supposed to be an object method!");
  DEBUG > 1 and print "Calling $_[0][0]\n";
  goto &{
   $_[0][0] ||    # call the closure
   Carp::croak("That " . __PACKAGE__ . " object has been closed!?")
  };
}

#**************************************************************************
sub printf {
  ref $_[0] or Carp::croak(__PACKAGE__ .
   "'s printf(...) is supposed to be an object method!");
  my($it,$format) = splice(@_,0,2);
  $format = '' unless defined $format;
  
  if(ref($format) ne 'SCALAR') {
    # Example: $it->printf("%04d: %s\n", @stuff)
    DEBUG and print "Nonescaped format <$format> on <@_>\n";
    my $x = sprintf($format, @_);
    DEBUG and print "Formatted (not yet esc): $x\n";
    $it->print( $x );
    # And, in escaping, this will be wrapped.
  } else {
    # Example: $it->printf(\'{\f30\b %s:} {\i %d}', @stuff)
    DEBUG and print "Escaped format <", $$format, "> on <@_>\n";

    my $str;  # scratch
      
    # Escape anything non-numeric:
    for(my $i = 0; $i < @_; ++$i) {
      next if !defined($_[$i]) or !length($_[$i]) or
       $_[$i] =~ m/^[+-]?(?=\d|\.\d)\d*(?:\.\d*)?(?:[Ee](?:[+-]?\d+))?\z/s;
      
      ($str = $_[$i]) =~
         s/([\x00-\x1F\-\\\{\}\x7F-\xFF])/$Escape{$1}/g;  # ESCAPER
      # Don't bother applying wrapping, I guess.
      
      DEBUG > 2 and print "Escaping <$_[$i]> to <$str>\n";
      splice @_, $i, 1, $str;
       # MAGIC!  makes it so we don't alter the original.
    }

    my $x = sprintf $$format, @_;
    DEBUG and print "Formatted (esc): $x\n";
    $it->print( \$x );     # No wrapping applied.

    # We mustn't escape things that we might intend, in the sprintf
    #  format, to treat as numbers, since escaping would turn '-'
    #  to '\_', and that would turn something numeric like "-14"
    #  or "1.5E-9" into something non-numeric like "\_14"
    #  or "1.5E\_9".  So we use this regexp.
    # The solution here /could/ fail to apply the escaping of
    #  "-" -> "\_", to number-seeming things we were really going
    #  to use as strings, but that seems relatively harmless.
    # The only completely correct way to do that would be to
    #  completely reimplement sprintf in pure Perl, or at least
    #  enough of it that we parse the format -- so not only could we
    #  tell what items from @_ were to be treated as numbers and
    #  which as strings, but also so we could take the output of
    #  formatting numbers, and /then/ apply the '-' -> '\_'
    #  escaping.
    # However, the /only/ benefit of this would be to get the
    #  '-' -> '\_' escaping to apply.  And in practice, this could
    #  be a problem only in two cases: a leading minus-sign, as
    #  in '-53.3', which presumably won't occur in a context
    #  where a word-processor would hyphenate; and after an "E",
    #  as in "1.5E-9".  While it's more likely that a word-precessor
    #  might hyphenate there, I that think scientific-notation
    #  numbers are in practive relatively rare.  So there.
  }
}

#--------------------------------------------------------------------------
sub AUTOLOAD {
  DEBUG and print "**** $_[0] hits autoload for $AUTOLOAD\n";
  if(ref($_[0]) and $AUTOLOAD =~ m<::([A-Z][a-z]*(?:_?[0-9]+)?)$>s) {
    my $cmd = "\\" . lc($1);
    $cmd =~ tr<_><->;    # So: $x->fi_180 -> $x->print(\'\fs-180')
    my $it = shift;
    if(@_) {
      return $it->print(\'{', \$cmd, @_, \'}');
       # So: $it->Lang1234(...) -> $it->print([\'\lang123', ... ]);
       #  (Well, the { ... } is just an incidental optimization.)
    } else {
      return $it->print(\$cmd);
       # So: $it->Lang1234() -> $it->print(\'\lang123');
    }
  } else {
    Carp::croak "Can't locate object method \"$AUTOLOAD\" via package \""
      . (ref($_[0]) || $_[0]) . '"';
  }
}

#--------------------------------------------------------------------------
sub close {
  return unless $_[0][0];  # Already closed?!
  DEBUG > 1 and print "Closing $_[0]\n";
  $_[0]->print(\$_[0][1]) if length $_[0][1];
  undef $_[0][0];   # ...presumably clausing any FH to close and destroy.
  $_[0][1] = '';
  return;
}

#--------------------------------------------------------------------------
sub DESTROY {
  # just a rudimentary version of $fh->close()
  $_[0]->print(\$_[0][1]) if $_[0][0] and $_[0][1];
}

#**************************************************************************
use UNIVERSAL ();

sub table {
  # Wrapper around row().
  my $it = shift;
  Carp::croak "table isn't a class method" unless ref $it;
  my $decl = shift
    if @_ and defined $_[0] and ref($_[0])
          and UNIVERSAL::isa($_[0], __PACKAGE__ . '::TableRowDecl');
  # Remaining items are row-arrayrefs.

  push @_, [''] unless @_; # avoid table with no rows!

  $decl ||= RTF::Writer::TableRowDecl->new_auto_for_rows(@_);

  $it->print(\'\par\pard');
   # Because ill things happen unless the paragraph
   #  that the table starts in, is virgin.
  foreach my $row_content (@_) {
    Carp::croak "table's row-parameters have to be arrayrefs"
     unless ref($row_content || '') eq 'ARRAY';
    $it->row($decl, @$row_content);
  }
  return scalar @_;
}

#--------------------------------------------------------------------------

sub row {
  # Generate a table row.
  my $it = shift;
  Carp::croak "row isn't a class method" unless ref $it;
  Carp::croak "row's first parameter has to be a table row declaration"
   unless @_ and defined $_[0] and ref($_[0])
             and UNIVERSAL::isa($_[0], __PACKAGE__ . '::TableRowDecl');
  my $decl = shift;

  # Pad with blank cells, if need be:
  push @_, (\'') x scalar(@{$decl->[0]} - @_) if @{$decl->[0]} > @_;
  # We have to avoid having a cell-less row:
  push @_, \'' unless @_;
  
  my $cell_count = @_;
  unshift @_, \'\pard\intbl';
  for(my $i = 1; $i < @_; $i += 2) {
    if(defined($_) and ref($_) eq '' and -1 != index($_[$i], "\f")) {
      # The one case where we need to mess with things: if there's a
      #  formfeed in this plaintext.
      my $x = $_[$i];
      $x =~ tr/\f/\n/;
      splice @_, $i, 1, $x;  # Swap in the copy, not touching the original.
    }
    splice(@_, $i + 1, 0, \'\cell\pard\intbl');
  }
  $_[-1] = \'\cell\row\pard';

  $it->print(
    \'{',
    $decl->decl_code($cell_count),
    @_,
    \'}',
  );
  return $cell_count;   # Might as well return somehting.
}

#--------------------------------------------------------------------------

sub number_pages {
  my $r = shift;
  $r->print(
    \"\n{\\header \\pard\\qr\\plain\\f0",
    @_,
    \"\\chpgn\\par}\n\n"
  );
  # This is actually a section attribute.  To reset, \'\sect\sectd'
  # to start a new section.
}

#**************************************************************************

sub paragraph {
  my $r = shift;
  $r->print(\"{\\pard\n", @_, \"\n\\par}\n\n");
}

#**************************************************************************

sub prolog {
  # Emit prolog with given parameters
  DEBUG and print "Prolog args: <@_>\n";
  my($it, %h) = (@_);

  my $x;  #scratch
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  $h{'revtim' } = time unless exists $h{'revtim'};
  $h{'creatim'} = time unless exists $h{'creatim'};
  $h{'doccomm'} =
    escape_broadly(sprintf 'written by %s [Perl %s v%s]',
                           $0, ref($it), $it->VERSION())
    unless exists $h{'doccomm'};
   # So you can set each to undef if you want it suppressed.

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  my $fonts = $h{'fonts'} || $h{'font_table'} || $h{'fonttable'} || [];
  $fonts = [$fonts] unless ref $fonts;
  push @$fonts, \'\froman Times New Roman'
   if ref($fonts) eq 'ARRAY' and ! @$fonts; # avoid having an empty font table

  my $font_count = -1;
  $fonts = \join '',
    # '{' \fonttbl (<fontinfo> | ('{' <fontinfo> '}'))+  '}'
    "{\\fonttbl\n",
    map( ref($_)
      ? ("{\\f", ++$font_count, ' ', $$_, ";}\n")
      : ("{\\f", ++$font_count, '\fnil ', escape_broadly($_), ";}\n"),
      @$fonts
      #  <fontnum> <fontfamily>
      #  <fcharset>? <fprq>? <panose>? <nontaggedname>? <fontemb>? <codepage>?
      #  <fontname> <fontaltname>? ';' 
    ), "}\n"
   if ref $fonts eq 'ARRAY'
  ;
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  my $info = join '',
    # And the info group:
    "\n{\\info \n",

     # \version? & \vern? & \edmins? & \nofpages? & \nofwords? \nofchars?
     # & \id?
     # & <title>? & <subject>? & <author>? & <manager>? & <company>?
     # & <operator>? & <category>? & <keywords>? & <comment>? & <doccomm>?

    # Time things, all optional:
    map(
      (!defined($x = $h{$_})) ? () : (
        "{\\$_ ", (
        ref($x) eq 'SCALAR' ? $$x :
        ref($x) eq 'ARRAY'  ? _time_to_rtf(@$x) :
        $x =~ m<^\d+$>      ? _time_to_rtf( $x) :
                              $x,  # dubious, but let it thru
        ),
        "}\n"
      ),
      qw(creatim revtim printim buptim)
    ),

    map( # Optional integer things:
      (!defined($x = $h{$_})) ? () :
      $x =~ m<^[0-9]+$> ? "\\$_$x\n" :
      Carp::croak("value for \"$_\" must be an integer, not \"$_\""),

      qw(version vern edmins nofpages nofwords nofchars nofcharsws id)
    ),

    # Optional non-time non-integer things:
    map(
      (!defined($x = $h{$_})) ? () : (
        "{\\$_ ",
        (ref($x) eq 'SCALAR') ? $$x : $x,
        "}\n"
      ),
      qw(title subject author manager company operator category
         keywords comment doccomm hlinkbase)
    ),

    ref( $h{'more_info'} || '' ) eq 'SCALAR'
     ? ${ $h{'more_info'} }  : ( $h{'more_info'} || '' ),

    "}\n\n", # end of info group
  ;

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Cook up the color table.
  #
  # Note that you might want to feed this a null 0th entry:
  #  as in:  [ undef, [255,0,0], [0,0,255], ... ]
 
  my $color_table = ($h{'colors'} || $h{'color_table'}
                  || $h{'colortable'} || $h{'colortbl'} || '');
  if(ref($color_table) eq 'ARRAY') {
    #print "R ", ref($color_table), "<", @$color_table, "> =$color_table\n";
    $color_table = \join '',
     '{\colortbl ',
     map(
         (ref($_ || '') eq 'ARRAY' ) ? sprintf('\red%d\green%d\blue%d;',
                                               $_->[0] || 0,
                                               $_->[1] || 0,
                                               $_->[2] || 0,
                                       )
       : (ref($_ || '') eq 'SCALAR') ? (
            ($$_ =~ m/;[\cm\cj\n]*\z/s) ? $$_ : ($$_ . ';') )
             # Make sure it ends with a semicolon
       : ';', # null entry
       @$color_table
     ),
     '}'
    ;
  } elsif(ref($color_table) eq 'SCALAR') {
    # pass it thru
  } else {
    $color_table = \'{\colortbl;\red255\green0\blue0;\red0\green0\blue255;}';
  }
  
  $h{'colortbl'} = $color_table;
  #print "Color table: <", ${$h{'colortbl'}}, ">\n";

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  #
  # Now emit the table:
  #
  # \rtf <charset> \deff? <fonttbl> <filetbl>? <colortbl>? <stylesheet>?
  #  <listtables>? <revtbl>?

  $it->print( \join '',
    '{\rtf' ,
    defined($h{'rtf_version'}) ? $h{'rtf_version'} : '1',

    "\\" . ($h{'charset'} || 'ansi'),
    "\\deff" . int($h{'deff'} || 0),

    (!defined($x = $h{'more_default'})) ? ''  # place to sneak in more stuff
     : ref($x) eq 'SCALAR' ? $$x
     : $x,
    
    $$fonts,

    map( ref( $h{$_} || '' ) eq 'SCALAR'
       ?  ${ $h{$_} }  : ( $h{$_} || '' ),
       qw( filetbl colortbl stylesheet listtables revtbl )
    ),

    $info,

  );
  #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  $it->[1] .= '}';
  DEBUG > 2 and print "Setting $it\'s out-buffer to <$it->[1]>\n";
   # to close the group that this document opened in its first char
  return 1;
}


# Two subs used in the "prolog" method:

sub escape_broadly {
  # Non-destructively quote anything fishy.
  my $scratch = $_[0];
  $scratch =~ s/([\x00-\x1F\\\{\}\x7F-\xFF])/"\\'".(unpack("H2",$1))/eg;
   # Escape \, {, }, -, control chars, and 7f-ff.
  return $scratch;
}

sub _time_to_rtf {
  # accepts no-params (meaning now), an epoch time, or a timelist
  push @_, time() unless @_;
  if(@_ == 1) { # normal case
    @_ = (localtime(shift @_))[5,4,3,2,1,0];
    $_[0] += 1900;  # RTF counts 2023 as 2023, not 123.
    $_[1]++;        # RTF counts January as 1, not 0.
  }
  return sprintf '\yr%d\mo%d\dy%d\hr%d\min%d\sec%d', @_;
}

#**************************************************************************
#
# The following makes the scary scary emitter-closure:
#

my $counter = 0;  # for debug purposes

sub _make_emitter_closure {
  my($fh, $sr) = @_;
   # sr should either be undef, or a scalar-ref
  my $scratch;

  # A closure on $fh or $sr, for printing to it.
  
  sub {
    my $this = shift;
    DEBUG > 1 and print "Writing (@_) to ", $sr ? "S_$sr\n" : "F_$fh\n";

    foreach my $x (@_) {
      next unless defined $x;
      if(ref($x) eq 'ARRAY') {
        next if @$x == 0;
        $sr ? ( $$sr .= '{' ) : print $fh '{';
        DEBUG > 1 and print " $counter: wrote {\n";
        $this->[0]->($this, @$x);   # recurse!
        $sr ? ( $$sr .= '}' ) : print $fh '}';
        DEBUG > 2 and print " wrote }\n";
      } elsif(ref($x) eq 'SCALAR') {
        if(!defined($$x) or !length($$x)) {
          # no-op
          DEBUG > 2 and print " $counter: skipping null sr\n";
        } elsif( not( $AUTO_NL and $$x =~ m<[a-zA-Z0-9]\z>s )) {
          $sr ? ( $$sr .= $$x ) : print $fh $$x;
          DEBUG > 2 and print " $counter: wrote sr $$x\n";
        } else {
          # $AUTO_NL is true, and $$x's last char is in [a-zA-Z0-9]
          $sr ? ( $$sr .= $$x . "\n" ) : print $fh $$x, "\n";
          DEBUG > 2 and print " $counter: wrote sr $$x +nl\n";

          # Why emit a newline?  Because that string might end in a
          #  command, and we want to do the Right Thing in the case of:
          #  $r->print(\'\i', 'donuts')
          #  i.e., printing "\i[newline]donuts", not "\idonuts"
          #
          # And why not emit "\i[space]donuts"?  because we if we emit a
          #  space and the thing we emitted WASN'T a control word, then
          #  we did a bad thing!  Spaces are tricky -- sometimes they're
          #  meaningless, and sometimes they mean a literal space.
          #  But newlines are always ignored -- well, unless preceded
          #  by an escaping backslash, but to get that, the user would
          #  have to have the previous group end in an unmatched backslash,
          #  as in $h->print(\"\\foo\\", ...) So don't do that!
        }
        
      } elsif(length $x) { # It's plaintext
        ($scratch = $x) =~
            s/([\x00-\x1F\-\\\{\}\x7F-\xFF])/$Escape{$1}/g;  # ESCAPER
         # Escape \, {, }, -, control chars, and 7f-ff.

        # And now: a not terribly clever algorithm for inserting newlines
        # at a guaranteed harmless place: before a block of whitespace
        # after the 65th column.
        $scratch =~
         s/(
            [^\cm\cj\n]{65}        # Snare 65 characters from a line
            [^\cm\cj\n\x20]{0,50}  #  and finish any current word
           )
           (\x20{1,10})(?![\cm\cj\n]) # capture some spaces not at line-end
          /$1\n$2/gx     # and put a NL before those spaces
        if $WRAP;
         # This may wrap at well past the 65th column, but not past the 120th.
        $sr ? ( $$sr .= $scratch ) : print $fh $scratch;
        DEBUG > 2 and print " $counter: wrote scalar <$scratch>\n";
        $scratch = '';
      }
       # otherwise it's 0-length plaintext, so ignore.
    }
    DEBUG > 3 and print $fh "{\\v $^T/", ++$counter, "}\n";
    return 1;
  };
}

#--------------------------------------------------------------------------
1;


