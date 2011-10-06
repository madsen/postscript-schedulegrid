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
has name => (
  is       => 'ro',
  isa      => Str,
  required => 1,
);

requires 'define_style';

#=====================================================================
# Package Return Value:

1;

__END__
