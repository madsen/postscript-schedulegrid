#---------------------------------------------------------------------
package PostScript::ScheduleGrid::Style::Stripe;
#
# Copyright 2011 Christopher J. Madsen
#
# Author: Christopher J. Madsen <perl@cjmweb.net>
# Created:  5 Oct 2011
#
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either the
# GNU General Public License or the Artistic License for more details.
#
# ABSTRACT: Striped background style
#---------------------------------------------------------------------

# VERSION
# This file is part of {{$dist}} {{$dist_version}} ({{$date}})

use 5.010;
use Moose;

with 'PostScript::ScheduleGrid::Role::Style';

use Moose::Util::TypeConstraints;
use PostScript::ScheduleGrid::Types ':all';

use namespace::autoclean;

#=====================================================================

=attr color

The background color for the cell (default light gray 0.85).

=attr direction

The direction of the slant, either C<left> (meaning C<\>) or C<right>
(meaning C</>).  Default C<left>.

=attr text_color

The color for the cell's text (default black).

=cut

has color => (
  is      => 'ro',
  isa     => Color,
  coerce  => 1,
  default => '0.85',            # light gray
);

has direction => (
  is      => 'ro',
  isa     => enum([qw( left right )]),
  default => 'left',
);

has text_color => (
  is      => 'ro',
  isa     => Color,
  coerce  => 1,
  default => '0',               # black
);

#=====================================================================
sub define_style
{
  my ($self, $grid) = @_;

  # Hack to allow me to use the development version directly.
  # $VERSION does not exist in this module until Dist::Zilla inserts it.
  our $VERSION //= $PostScript::ScheduleGrid::VERSION;

  $grid->ps->add_function('PostScript_ScheduleGrid_Style_Stripe',
                          <<'END FUNCTIONS', $VERSION);
/sStripe-R % round X down to a multiple of N
{				% X N
  exch	1 index			% N X N
  div  truncate  mul
} bind def

/sStripe-P % common prep
{
  setColor
  6 setlinewidth
  2 setlinecap
  clippath pathbbox newpath     % (LLX LLY URX URY)
  4 2 roll                      % (URX URY LLX LLY)
  18 sStripe-R                  % (URX URY LLX LLY1)
  4 1 roll                      % (LLY1 URX URY LLX)
  18 sStripe-R                  % (LLY1 URX URY LLX1)
  4 1 roll                      % (LLX1 LLY1 URX URY)
  2 index                       % (LLX Bot URX URY LLY)
  sub                           % (LLX Bot URX Height)
} def
END FUNCTIONS

  my $code = <<'END PROLOGUE';
/$name
{
  $color sStripe-P              % (LLX Bot URX Height)
END PROLOGUE

  if ($self->direction eq 'left') {
    $code .= <<'END LEFT';
  neg dup neg 3 -1 roll add     % (Left Bot -Height Right)
  4 -1 roll                     % (Bot -Height Right Left)
  18   3 -1 roll                % (Bot Height Left 18 Right)
  % stack in FOR: (Bot Height X)
  {
    2 index moveto              % (Bot Height)
    dup dup neg rlineto stroke
  } for
END LEFT
  } else {
    $code .= <<'END RIGHT';
  dup neg 5 -1 roll add         % (Bot URX Height Left)
  18   4 -1 roll                % (Bot Height Left 18 Right)
  % stack in FOR: (Bot Height X)
  {
    2 index moveto              % (Bot Height)
    dup dup rlineto stroke
  } for
END RIGHT
  }

  $code .=  <<'END EPILOGUE';
  pop pop
  $text_color setColor
} def
END EPILOGUE

  $self->_ps_eval(\$code);
  $code;
} # end define_style

#=====================================================================
# Package Return Value:

1;

__END__

=head1 DESCRIPTION

This L<Style|PostScript::ScheduleGrid::Role::Style> produces a striped
background.

=for Pod::Coverage
^define_style$
