#---------------------------------------------------------------------
package PostScript::ScheduleGrid::Role::Style;
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
# ABSTRACT: Something that customizes a cell's appearance
#---------------------------------------------------------------------

our $VERSION = '0.01';
# This file is part of {{$dist}} {{$dist_version}} ({{$date}})

use 5.010;
use Moose::Role;

use MooseX::Types::Moose qw(Str);

use namespace::autoclean;

#=====================================================================

=attr name

This is the name of the PostScript function that the Style must
define.  It may also use any identifiers beginning with C<name>
followed by a hyphen for any purposes it likes.

=cut

has name => (
  is       => 'ro',
  isa      => Str,
  required => 1,
);

=method define_style

  $style->define_style($schedule_grid);

This method must return a string containg PostScript code to define
the function specified by C<name>.  C<$schedule_grid> is the
PostScript::ScheduleGrid object using the style.

=cut

requires 'define_style';

=method _ps_eval

  $style->_ps_eval(\$string, ...);

This method is provided by this role.  It substitutes values from the
object into each C<$string> (which is modified in-place).  Any number
of string references may be passed.

The following substitutions are performed on each C<$string>:

First, any C<$> followed by an identifier are replaced by calling that
method on the object and passing its return value to
PostScript::File's C<str> function.

Second, any C<%{...}> is replaced with the result of evaluating ... (which
may not contain braces).

=cut

# Import _ps_eval method from PostScript::ScheduleGrid:
__PACKAGE__->meta->add_method(_ps_eval => \&PostScript::ScheduleGrid::_ps_eval);

#=====================================================================
# Package Return Value:

1;

__END__

=head1 DESCRIPTION

This role describes a style for displaying the contents of a cell in
the grid.  The class must provide a C<define_style> method.

If a style has PostScript definitions that are invariant, it may
define them by adding a function block under its class name (with
C<::> replaced by C<_>).  If it needs more than one function block, it
may append a hyphen and an arbitrary identifier.

The definitions must begin with the class name (with
C<PostScript::ScheduleGrid::Style::> replaced by C<s> and any
remaining C<::> replaced by C<_>.  That prefix may be followed by a
hypen and any legal PostScript identifier.  For example,
C<PostScript::ScheduleGrid::Style::Stripe> can define PostScript
identifiers beginning with C<sStripe->.

The C<name> function created by the C<define_style> method will be
called with no parameters.  The clipping path is set to the boundaries
of the cell, and the current font is the grid's C<cell_font>.  The
graphics state has been saved and will be restored after the cell's
text is drawn.

The function must perform whatever drawing it wants to, and leave the
color and font set for the cell's text.
