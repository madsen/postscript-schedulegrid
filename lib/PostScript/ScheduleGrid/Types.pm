#---------------------------------------------------------------------
package PostScript::ScheduleGrid::Types;
#
# Copyright 2010 Christopher J. Madsen
#
# Author: Christopher J. Madsen <perl@cjmweb.net>
# Created: 31 Dec 2010
#
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either the
# GNU General Public License or the Artistic License for more details.
#
# ABSTRACT: type library for PostScript::ScheduleGrid
#---------------------------------------------------------------------

our $VERSION = '0.03';
# This file is part of {{$dist}} {{$dist_version}} ({{$date}})

use MooseX::Types -declare => [qw(
  BWColor Color Dimension FontMetrics RGBColor RGBColorHex Style
  TimeHeaders
)];
use MooseX::Types::Moose qw(ArrayRef Num Str);

use POSIX qw(floor modf);

class_type FontMetrics, { class => 'PostScript::File::Metrics' };

role_type Style, { role => 'PostScript::ScheduleGrid::Role::Style' };

#---------------------------------------------------------------------
subtype Dimension,
  as Num,
  where { (modf($_ * 32))[0] == 0 };

coerce Dimension,
  from Num,
  via { floor($_ * 32 + 0.5) * (1/32); };

#---------------------------------------------------------------------
subtype BWColor,
  as Num,
  where { $_ >= 0 and $_ <= 1 };

subtype RGBColor,
  as ArrayRef[BWColor],
  where { @$_ == 3 };

subtype RGBColorHex,
  as Str,
  # Must have a multiple of 3 hex digits after initial '#':
  where { /^#((?:[0-9a-f]{3})+)$/i };

coerce RGBColor,
  from RGBColorHex,
  via {
    my $color = substr($_, 1);

    my $digits = int(length($color) / 3); # Number of digits per color
    my $max    = hex('F' x $digits);      # Max intensity per color

    [ map {
        my $n = sprintf('%.3f',
                        hex(substr($color, $_ * $digits, $digits)) / $max);
        $n =~ s/\.?0+$//;
        $n
      } 0 .. 2 ];
  };

subtype Color,
  as BWColor|RGBColor;

#---------------------------------------------------------------------
subtype TimeHeaders,
  as ArrayRef[Str],
  where { @$_ == 2 };

1;

__END__

=head1 DESCRIPTION

These are the custom types used by L<PostScript::ScheduleGrid>.

=head1 TYPES

=head2 Color

This is a number in the range 0 to 1 (where 0 is black and 1 is
white), or an arrayref of three numbers C<[ Red, Green, Blue ]> where
each number is in the range 0 to 1.

In addition, you can specify an RGB color as a string in the HTML hex
triplet form prefixed by C<#> (like C<#FFFF00> or C<#FF0> for yellow).

=head2 Dimension

A floating-point number rounded to the nearest 1/32.  Helps avoid
round-off errors in PostScript calculations.

=head2 FontMetrics

A L<PostScript::File::Metrics>.

=head2 Style

A class that does L<PostScript::ScheduleGrid::Role::Style>.

=head1 SEE ALSO

L<MooseX::Types>, L<MooseX::Types::Moose>.

=for Pod::Loom-omit
CONFIGURATION AND ENVIRONMENT
INCOMPATIBILITIES
BUGS AND LIMITATIONS
