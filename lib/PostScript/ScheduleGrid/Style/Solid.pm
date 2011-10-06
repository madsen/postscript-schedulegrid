#---------------------------------------------------------------------
package PostScript::ScheduleGrid::Style::Solid;
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
# ABSTRACT: Solid background style
#---------------------------------------------------------------------

our $VERSION = '0.01';
# This file is part of {{$dist}} {{$dist_version}} ({{$date}})

use 5.010;
use Moose;

with 'PostScript::ScheduleGrid::Role::Style';

use PostScript::ScheduleGrid::Types ':all';

use namespace::autoclean;

#=====================================================================
has color => (
  is      => 'ro',
  isa     => Color,
  coerce  => 1,
  default => '0.85',            # light gray
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

  my $code = <<'END PS';
/$name
{
  $color setColor
  clippath fill
  $text_color setColor
} def
END PS

  $self->_ps_eval(\$code);
  $code;
} # end define_style

#=====================================================================
# Package Return Value:

1;

__END__
