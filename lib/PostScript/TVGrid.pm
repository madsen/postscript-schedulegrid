#---------------------------------------------------------------------
package PostScript::TVGrid;
#
# Copyright 2010 Christopher J. Madsen
#
# Author: Christopher J. Madsen <perl@cjmweb.net>
# Created: 28 Dec 2010
#
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either the
# GNU General Public License or the Artistic License for more details.
#
# ABSTRACT: Print TV listings in a grid format
#---------------------------------------------------------------------

our $VERSION = '0.01';
# This file is part of {{$dist}} {{$dist_version}} ({{$date}})

use 5.010;
use Moose;

use MooseX::Types::Moose qw(ArrayRef Bool HashRef Int Num Str);
use MooseX::Types::DateTime (); # Just load coercions
use PostScript::TVGrid::Types ':all';

use List::Util qw(max min);
use POSIX qw(floor);
use PostScript::File 2.10;      # Need improved API

use namespace::autoclean;

sub iStart () { 0 }
sub iEnd   () { 1 }
sub iShow  () { 2 }
sub iMark  () { 3 }

#=====================================================================
has cell_font => (
  is      => 'ro',
  isa     => Str,
  default => 'Helvetica',
);

has cell_font_size => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  default => 7,
);

has _metrics => (
  is       => 'ro',
  isa      => FontMetrics,
  init_arg => undef,
  lazy     => 1,
  default  => sub {
    my $s = shift;
    $s->ps->get_metrics($s->cell_font . '-iso', $s->cell_font_size);
  },
);

has extra_height => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  lazy    => 1,
  default => sub { shift->cell_font_size },
);

has heading_font => (
  is      => 'ro',
  isa     => Str,
  default => 'Helvetica-Bold',
);

has heading_font_size => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  default => 12,
);

has title_font => (
  is      => 'ro',
  isa     => Str,
  default => 'Helvetica-Bold',
);

has title_font_size => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  default => 9,
);

has grid_hours => (
  is      => 'ro',
  isa     => Int,
  default => 6,
);

has channel_width => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  lazy    => 1,
  builder => '_compute_channel_width',
);

has five_min_width => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  lazy    => 1,
  builder => '_compute_five_min_width',
);

has half_width => (
  is       => 'ro',
  isa      => Dimension,
  coerce   => 1,
  lazy     => 1,
  init_arg => undef,
  default  => sub { shift->five_min_width * 6 },
);

has hour_width => (
  is       => 'ro',
  isa      => Dimension,
  coerce   => 1,
  lazy     => 1,
  init_arg => undef,
  default  => sub { shift->five_min_width * 12 },
);

has channels => (
  is       => 'ro',
  isa      => ArrayRef[HashRef],
  required => 1,
);

has grid_height => (
  is       => 'ro',
  isa      => Dimension,
  coerce   => 1,
  lazy     => 1,
  init_arg => undef,
  default  => sub { my $s = shift; my $c = $s->channels; my $extra = 0;
    $extra += $_->{lines} - 1 for @$c;
    (2 + scalar @$c) * $s->line_height +
    $extra * $s->extra_height;
  },
);

has grid_width => (
  is       => 'ro',
  isa      => Dimension,
  coerce   => 1,
  lazy     => 1,
  init_arg => undef,
  default  => sub { my $s = shift;
    $s->channel_width + $s->five_min_width * 12 * $s->grid_hours
  },
);

has cell_bot => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  default => 2.5,
);

has cell_left => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  default => 1.4,
);

has line_height => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  default => 10,
);

has heading_baseline => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  default => 3,
);

has start_date => (
  is       => 'rw',
  isa      => 'DateTime',
  required => 1,
);

has end_date => (
  is       => 'rw',
  isa      => 'DateTime',
  required => 1,
);

has time_zone => (
  is       => 'rw',
  isa      => 'DateTime::TimeZone',
  coerce   => 1,
  default  => 'local',
);

has title_baseline => (
  is      => 'ro',
  isa     => Dimension,
  coerce  => 1,
  default => 1.7,
);

#=====================================================================

=attr-fmt ps_parameters

This is a hashref of additional parameters to pass to
PostScript::File's constructor.  These values will override the
parameters that PostScript::Report generates itself (but you should
reserve this for things that can't be controlled through
other PostScript::Report attributes).

=cut

has ps_parameters => (
  is       => 'ro',
  isa      => HashRef,
  default  => sub { {} },
);

=attr-fmt paper_size

This the paper size (default C<Letter>).  See L<PostScript::File/paper>.

=cut

has paper_size => (
  is      => 'ro',
  isa     => Str,
  default => 'Letter',
);

=attr-fmt top_margin

This is the top margin (default 36, or 1/2 inch).

=attr-fmt bottom_margin

This is the bottom margin (default 36, or 1/2 inch).

=attr-fmt left_margin

This is the left margin (default 22, or about 0.3 inch).

=attr-fmt right_margin

This is the right margin (default 22, or about 0.3 inch).

=cut

has top_margin => (
  is      => 'ro',
  isa     => Int,
  default => 36,
);

has bottom_margin => (
  is      => 'ro',
  isa     => Int,
  default => 36,
);

has left_margin => (
  is      => 'ro',
  isa     => Int,
  default => 22,
);

has right_margin => (
  is      => 'ro',
  isa     => Int,
  default => 22,
);

=attr-fmt landscape

If set to a true value, the report will be printed in landscape mode.
The default is false.

=cut

has landscape => (
  is      => 'ro',
  isa     => Bool,
  default => 0,
);

=attr-o ps

This is the L<PostScript::File> object containing the grid.

=method output

  $rpt->output($filename [, $dir]) # save to file
  $rpt->output()                   # return as string

This method takes the same parameters as L<PostScript::File/output>.
You can pass a filename (and optional directory name) to store the
report in a file.  (No extension will be added to C<$filename>, so it
should normally end in ".ps".)

If you don't pass a filename, then the PostScript code is returned as
a string.

=cut

has ps => (
  is         => 'ro',
  isa        => 'PostScript::File',
  lazy_build => 1,
  handles    => ['output'],
);

sub _build_ps
{
  my ($self) = @_;

  PostScript::File->new(
    paper       => $self->paper_size,
    top         => $self->top_margin,
    bottom      => $self->bottom_margin,
    left        => $self->left_margin,
    right       => $self->right_margin,
    title       => 'TV Grid',
    order       => 'Ascend',
    reencode    => 'cp1252',
    file_ext    => '',
    font_suffix => '-iso',
    landscape   => $self->landscape,
    newpage     => 0,
    %{ $self->ps_parameters },
  );
} # end _build_ps

#---------------------------------------------------------------------
sub _compute_channel_width
{
  my $self = shift;

  my $metrics = $self->ps->get_metrics($self->title_font . '-iso',
                                       $self->title_font_size);

  my $width = max( map { $metrics->width($_->{name}) } @{ $self->channels });

  $width + 2 * $self->cell_left; # Add some padding
} # end _compute_channel_width

#---------------------------------------------------------------------
sub _compute_five_min_width
{
  my $self = shift;

  floor(8 * ($self->ps->get_printable_width - $self->channel_width) /
        (3 * $self->grid_hours)) * (1/32);
} # end _compute_five_min_width

#---------------------------------------------------------------------
sub _ps_functions
{
  my $self = shift;

  my $functions = <<'END PS INIT';
/pixel {72 mul 300 div} def % 300 dpi only

/C                             % HEIGHT WIDTH LEFT VPOS C
{
  gsave
  newpath moveto                % HEIGHT WIDTH
  dup 0 rlineto                 % HEIGHT WIDTH
  0 3 -1 roll rlineto           % WIDTH
  -1 mul 0 rlineto
  closepath clip
} def

/R {grestore} def

/G
{
  0.9 setgray
  clippath fill
  0 setgray
} def

/GL % Fill clippath with grey bars slanting to the left
{
  0.9 setgray
  6 setlinewidth
  2 setlinecap
  clippath pathbbox newpath     % (LLX LLY URX URY)
  2 index                       % (LLX Bot URX URY LLY)
  sub                           % (LLX Bot URX Height)
  neg dup neg 3 -1 roll add     % (Left Bot -Height Right)
  4 -1 roll                     % (Bot -Height Right Left)
  18   3 -1 roll                % (Bot Height Left 18 Right)
  % stack in FOR: (Bot Height X)
  {
    2 index moveto              % (Bot Height)
    dup dup neg rlineto stroke
  } for
  pop pop
  0 setgray
} def

/GR % Fill clippath with grey bars slanting to the right
{
  0.9 setgray
  6 setlinewidth
  2 setlinecap
  clippath pathbbox newpath     % (LLX LLY URX URY)
  2 index                       % (LLX Bot URX URY LLY)
  sub                           % (LLX Bot URX Height)
  dup neg 5 -1 roll add         % (Bot URX Height Left)
  18   4 -1 roll                % (Bot Height Left 18 Right)
  % stack in FOR: (Bot Height X)
  {
    2 index moveto              % (Bot Height)
    dup dup rlineto stroke
  } for
  pop pop
  0 setgray
} def

/H                             % YPOS H
{
  newpath
  0 exch moveto
  $grid_width 0 rlineto
  stroke
} def

/P1 {1 pixel setlinewidth} def
/P2 {2 pixel setlinewidth} def

/S                             % STRING X Y S
{
 newpath moveto show
} def

/V                             % XPOS YPOS HEIGHT V
{
  newpath
  3 1 roll
  moveto
  0 exch rlineto
  stroke
} def

%---------------------------------------------------------------------
% Set the color:  RGBarray|BWnumber setColor

/setColor
{
  dup type (arraytype) eq {
    % We have an array, so it's RGB:
    aload pop
    setrgbcolor
  }{
    % Otherwise, it must be a gray level:
    setgray
  } ifelse
} bind def

%---------------------------------------------------------------------
% Print text centered at a point:  X Y STRING showcenter
%
% Centers text horizontally

/showcenter
{
  newpath
  0 0 moveto
  % stack X Y STRING
  dup 4 1 roll                          % Put a copy of STRING on bottom
  % stack STRING X Y STRING
  false charpath flattenpath pathbbox   % Compute bounding box of STRING
  % stack STRING X Y Lx Ly Ux Uy
  pop exch pop                          % Discard Y values (... Lx Ux)
  add 2 div neg                         % Compute X offset
  % stack STRING X Y Ox
  0                                     % Use 0 for y offset
  newpath
  moveto
  rmoveto
  show
} def

%---------------------------------------------------------------------
% Print the date, times, channel names, & exterior grid:
%
% HEADER TIME1 TIME2 ... TIME12
%
% Enter with CellFont selected
% Leaves the linewidth set to 2 pixels

/prg
{
  (Channel) $cell_left %{$grid_height - $line_height + $cell_bot} S
  (Channel) $cell_left $cell_bot S

  TitleFont setfont
  %{$channel_width + $hour_width * $grid_hours - $half_width/2}
  -$half_width $channel_width
  % stack (TIME XPOS)
  {
    dup %{$grid_height - $line_height + $title_baseline} 3 index showcenter
    $title_baseline 3 -1 roll showcenter
  } for

END PS INIT

  my @hlines;
  my $channels = $self->channels;
  my $ps       = $self->ps;
  my $cell_left    = $self->cell_left;
  my $line_height    = $self->line_height;
  my $title_baseline = $self->title_baseline;
  my $extra_height   = $self->extra_height;
  my $vpos = $self->grid_height - $line_height;
  $functions .= '  ';
  foreach my $c (@$channels) {
      push @hlines, $vpos;
      my $ex = ($c->{lines} - 1) * $extra_height;
      $vpos -= $line_height + $ex;
      $c->{vpos} = $vpos;
      $functions .= $ps->pstr($c->{name}) . ($vpos+$title_baseline+$ex/2);
  }
  $functions .= "\n  " . @$channels . " {$cell_left exch S} repeat\n\n";
  push @hlines, $line_height;

  $functions .= <<'EOT';
  HeadFont setfont
  $channel_width %{$grid_height + $heading_baseline} S

  P1
  newpath
  0 0 moveto
  $grid_width 0 rlineto
  $grid_width $grid_height lineto
  0 $grid_height lineto
  closepath stroke

  %{$channel_width + $half_width} $hour_width %{$grid_width - $five_min_width}
  {dup %{$grid_height-$line_height} $line_height V 0 $line_height V} for
EOT

    $functions .=  '  '.join(' ',@hlines)."\n  ".scalar @hlines;
    $functions .= <<'EOT';
 {H} repeat

  P2
  %{$channel_width + $hour_width} $hour_width %{$grid_width-1}
  {dup %{$grid_height-$line_height} $line_height V 0 $line_height V} for
  $channel_width 0 $grid_height V
} def
EOT

  $self->_ps_eval(\$functions);

  # Append time, because this should not be substituted for any other version:
  return (sprintf('PostScript_TVGrid_%s_%s', $$, time), $functions, $VERSION);
} # end _ps_functions

#---------------------------------------------------------------------
sub _ps_eval
{
  my $self = shift;

  foreach my $psRef (@_) {
    $$psRef =~ s/\$([a-z0-9_]+)/ $self->$1 /ieg;
    $$psRef =~ s[%\{([^\}]+)\}][$1]eeg;
  }
} # end _ps_eval

#---------------------------------------------------------------------
sub _normalize_channels
{
  my $self = shift;

  my $channels = $self->channels;
  my $tz       = $self->time_zone;

  for my $c (@$channels) {
    $c->{lines} ||= 1;
    my $schedule = $c->{schedule};

    # Convert any floating times to specified time zone:
    for my $rec (@$schedule) {
      for my $date (@$rec[iStart, iEnd]) {
        $date->set_time_zone($tz) if $date->time_zone->is_floating;
      }
    } # end for $rec in @$schedule

    # Make sure the schedule is sorted:
    @$schedule = sort {
      DateTime->compare_ignore_floating($a->[iStart], $b->[iStart])
    } @$schedule;
  } # end for $c in @$channels
} # end _normalize_channels

#---------------------------------------------------------------------
sub run
{
  my $self = shift;

  $self->_normalize_channels;

  # Initialise PostScript::File object:
  my $ps = $self->ps;

  $ps->need_resource(font => $self->cell_font, $self->heading_font,
                     $self->title_font);

  $ps->add_function($self->_ps_functions);

  { my $setup = <<'END SETUP';
/CellFont   /$cell_font-iso    findfont  $cell_font_size    scalefont  def
/HeadFont   /$heading_font-iso findfont  $heading_font_size scalefont  def
/TitleFont  /$title_font-iso   findfont  $title_font_size   scalefont  def
END SETUP
    $self->_ps_eval(\$setup);
    $ps->add_setup($setup);
  }

  my $channels     = $self->channels;
  my $grid_height  = $self->grid_height;
  my $line_height  = $self->line_height;
  my $extra_height = $self->extra_height;
  my $start        = $self->start_date;
  my $stop_date    = $self->end_date;
  my $left_mar     = $self->left_margin;

  # Make sure start_date & end_date are in the specified time zone:
  {
    my $tz = $self->time_zone;
    $start->set_time_zone($tz);
    $stop_date->set_time_zone($tz);
  }

  # Decide if we have room for multiple grids on a page:
  my @grid_offsets;
  {
    my $bottom_margin = $self->bottom_margin;
    my $top_margin    = $self->top_margin;

    my $total_height = ($grid_height + $self->heading_baseline +
                        $self->heading_font_size);
    my @bb = $ps->get_bounding_box;

    push @grid_offsets, $bb[3] - $total_height;

    my $page_height = $bb[3] - $bb[1];

    my $grids = floor($page_height / $total_height);
    if ($grids > 1) {
      my $spacing = to_Dimension(
        $total_height + ($page_height - $grids * $total_height) / ($grids-1)
      );
      push @grid_offsets, (-$spacing) x ($grids-1);
    } # end if multiple grids
  } # end block for computing @grid_offsets

  # Loop for each page:
 PAGE:
  while (1) {
    $ps->newpage;
    $ps->add_to_page("$left_mar 0 translate\n");

    foreach my $grid_offset (@grid_offsets) {
      my $end = $start->clone->add(hours => $self->grid_hours);

      my $vpos = $grid_height - $line_height;

      $ps->add_to_page("0 $grid_offset translate\n" .
                       "CellFont setfont\n0 setlinecap\n");

      for my $channel (@$channels) {
        my $two_line = $channel->{lines} - 1; # FIXME
        $vpos = $channel->{vpos};
        my $height = $line_height + $two_line*$extra_height;

        my $schedule = $channel->{schedule};

        shift @$schedule while @$schedule and $schedule->[0][iEnd] < $start;

        while (@$schedule and $schedule->[0][iStart] < $end) {
          my $s = shift @$schedule;

          my $left = $self->_add_vline(max($s->[iStart], $start), $height,$vpos);
          my $right = $self->_add_vline(min($s->[iEnd], $end), $height,$vpos);
          $ps->add_to_page(sprintf "%s %s %s %s C\n%s",
                           $height, $right - $left, $left, $vpos,
                           defined $s->[iMark] ? "$s->[iMark]\n" : '');
          if ($two_line) {
            $self->_two_line_box($left,$right,$vpos,$s->[iShow]);
          } else {
            $self->_one_line_box($left,$right,$vpos,$s->[iShow]);
          }
          $ps->add_to_page("R\n");
          if ($s->[iEnd] > $end) {
            unshift @$schedule, $s;
            last;
          }
        } # end while @$schedule
      } # end for @$channels

      $self->_end_grid_page;
      $self->start_date($start = $end);

      last PAGE unless $start < $stop_date;
    } # end foreach grid
  } # end PAGE loop

  $self->output(@_) if @_;
} # end run

has _vlines => (
  is       => 'ro',
  isa      => ArrayRef[HashRef],
  init_arg => undef,
  default  => sub { [ {}, {} ] },
);

sub _add_vline
{
  my ($self,$time,$height,$vpos) = @_;

  my $minutes = $time->subtract_datetime($self->start_date)->in_units('minutes');

#  printf "  %s - %s = %s\n", $self->start_date, $time, $minutes;

  my $channel_width = $self->channel_width;

  my $hash = $self->_vlines->[ $minutes % 60 == 0 ];
  $hash->{$height} = [] unless $hash->{$height};
  my $list = $hash->{$height};
  my $hpos = ($channel_width + int(($minutes + 3) / 5) * $self->five_min_width);

  my $entry = "$hpos $vpos";
  push @$list, $entry
      unless $hpos == $channel_width or $hpos == $self->grid_width or
             (@$list and $list->[-1] eq $entry);
  $hpos;
} # end _add_vline

sub _end_grid_page
{
  my $self = shift;

  my $vpos = $self->grid_height - $self->line_height;
  my $time = $self->start_date->clone;

  my $code = $self->ps->pstr($time->format_cldr('EEEE, MMMM d, y'));
  for (1 .. $self->grid_hours) {
    $code .= $time->format_cldr('(h a)(h:30)');
    $time->add(hours => 1);
  }

  $code .= "prg\n";
  $code .= $self->_print_vlines(1);
  $code .= "P1\n";
  $code .= $self->_print_vlines(0);

  $self->ps->add_to_page($code);
} # end _end_grid_page

sub _print_vlines
{
  my ($self,$vlines) = @_;

  $vlines = $self->_vlines->[$vlines];
  my $code = '';
  while (my ($height, $list) = each %$vlines) {
      $code .= join(' ',@$list)."\n".scalar @$list." {$height V} repeat\n";
  }
  %$vlines = ();

  return $code;
} # end _print_vlines

#---------------------------------------------------------------------
sub _one_line_box
{
    my ($self, $left, $right, $vpos, $show) = @_;

    my $width = $right - $left - $self->cell_left;
    my $sw = $self->_metrics->width($show);
    $show =~ s/^The +// if $sw > $width;

    $self->ps->add_to_page(sprintf("%s %s %s S\n",
                                   $self->ps->pstr($show),
                                   $left + $self->cell_left,
                                   $vpos + $self->cell_bot));
} # end _one_line_box

#---------------------------------------------------------------------
sub _two_line_box
{
    my ($self, $left, $right, $vpos, $show) = @_;
    $left += $self->cell_left;
    $vpos += $self->cell_bot;
    my $width = $right - $left;

    my $metrics = $self->_metrics;

  BREAKDOWN: {
        my ($line1,$line2) = ($show,'');

        while ($metrics->width($line1) > $width
              and $line1 =~ m![- /]!) {
            $line1 =~ s!\s*([^- /]*.)\Z!!;
            $line2 = "$1 $line2";
        }
        $line2 =~ s!([-/])\s+!$1!g;
        $line2 =~ s/\s+$//;
        redo BREAKDOWN
            if $metrics->width($line2) > $width
               and $show =~ s/^(?:The|New|A|(?:Real )?Adventures of) +//i;

        my $ps = $self->ps;
        my $code = sprintf("%s %s %s S\n", $ps->pstr($line1),
                           $left, $vpos + $self->extra_height);
        $code .= $ps->pstr($line2) . " $left $vpos S\n" if length $line2;
        $ps->add_to_page($code);
    } # end BREAKDOWN
} # end _two_line_box

#=====================================================================
# Package Return Value:

no Moose;
__PACKAGE__->meta->make_immutable;
1;

__END__

=head1 SYNOPSIS

  use PostScript::TVGrid;
