package CAD::Drawing::GUI::View;

require Tk::Zinc;
require Tk::Derived;

use warnings;
use strict;
use Tk;

use Carp;
use CAD::Calc qw(
	pi
	dist2d
	);
use CAD::DXF::Color qw(
	aci2hex
	);

our $VERSION = '0.01';
our @ISA = qw(
	Tk::Derived
	Tk::Zinc
	);

=head1 NAME

CAD::Drawing::GUI::View - 2D graphics for CAD built on Tk::Zinc

=head1 DESCRIPTION

This module provides methods to turn a CAD::Drawing object into a Zinc
canvas.

=head1 SYNOPSIS

write me

=head1 AUTHOR

Eric L. Wilhelm <ewilhelm at cpan dot org>

http://scratchcomputing.com

=head1 COPYRIGHT

This module is copyright (C) 2004-2006 by Eric L. Wilhelm.

=head1 LICENSE

This module is distributed under the same terms as Perl.  See the Perl
source package for details.

You may use this software under one of the following licenses:

  (1) GNU General Public License
    (found at http://www.gnu.org/copyleft/gpl.html)
  (2) Artistic License
    (found at http://www.perl.com/pub/language/misc/Artistic.html)

=head1 Modifications

The source code of this module is made freely available and
distributable under the GPL or Artistic License.  Modifications to and
use of this software must adhere to one of these licenses.  Changes to
the code should be noted as such and this notification (as well as the
above copyright information) must remain intact on all copies of the
code.

Additionally, while the author is actively developing this code,
notification of any intended changes or extensions would be most helpful
in avoiding repeated work for all parties involved.  Please contact the
author with any such development plans.

=head1 SEE ALSO

  CAD::Drawing::GUI
  Tk::Zinc

=cut

Construct Tk::Widget 'CADView';


=head1 Overridden Methods

These make me behave like a Tk widget.

=head2 ClassInit

  $view->ClassInit();

=cut
sub ClassInit {
	my $self = shift;
	my ($mw) = @_;
    $self->SUPER::ClassInit($mw);
} # end subroutine ClassInit definition
########################################################################

=head2 InitObject

  $view->InitObject();

=cut
sub InitObject {
	my $self = shift;
    my ($args) = @_;
    my $pData = $self->privateData;
    # $pData->{'bbox'} = [0, 0, -1, -1];
    $pData->{'scale'} = 1;
    $pData->{'move'} = [0,0];
    # $pData->{'bboxvalid'} = 1;
    $pData->{'width'} = $self->width;
    $pData->{'height'} = $self->height;
	$pData->{'-pandist'} = 5;
	# strip other args
	$args = {$self->args_filter(%$args)};
	$pData->{group} = $self->add('group', 1, -visible => 1);
    $self->configure(-confine => 0);
	# $self->configure('-highlightbackground' => '#FF0000');
    $self->ConfigSpecs(
		'-bandColor' => ['PASSIVE', 'bandColor', 'BandColor', 'red'],
		'-bandcolor' => '-bandColor',
		'-changeView' => ['CALLBACK', 'changeView', 'ChangeView', undef],
		'-changeview'  => '-changeView'
		);
	$self->Tk::bind('<Enter>' =>
		sub {
			# print "hi\n"
			$self->active(1);
		}
	);
	$self->Tk::bind('<Leave>' =>
		sub {
			# print "bye\n"
			$self->active(0);
		}
	);
	# $self->Tk::bind('<ResizeRequest>' => sub {print "no\n"});
	## $self->Tk::bind('<Configure>' =>
	## 	sub {
	## 		print "hi @_\n";
	## 	}
	## 	);
	$self->SUPER::InitObject($args);
} # end subroutine InitObject definition
########################################################################

=head2 configure

  $view->configure(%args);

=cut
sub configure {
	my $self = shift;
	my %args = @_;
	%args = $self->args_filter(%args);
	$self->SUPER::configure(%args);
} # end subroutine configure definition
########################################################################

=head2 args_filter

Filters configure arguments and adds non-tk args to our private data.

  %args = $view->args_filter(%args);

=cut
sub args_filter {
	my $self = shift;
	my %args = @_;
	my $pdata = $self->privateData();
	my %req = (
		map({$_ => 1} 
			qw(
			-parent
			)),
		map({$_ => 0}
			qw(
			-pandist
			)
			)
		);
	foreach my $key (keys(%req)) {
		if(exists($args{$key})) {
			# print "configuring $key\n";
			$pdata->{$key} = $args{$key};
			delete($args{$key});
		}
		else {
			if($req{$key}) {
				exists($pdata->{$key}) or
					croak("required option $key missing\n");
			}
		}
	}
	return(%args);
} # end subroutine args_filter definition
########################################################################

=head1 privateData accessor methods

=head2 group_is

This object expects you to draw all of your items in this group.

  $view->group_is();

=cut
sub group_is {
	my $self = shift;
	my $pdata = $self->privateData();
	return($pdata->{group});
} # end subroutine group_is definition
########################################################################

=head2 active

  $view->active() or print "no\n";
  $view->active(1);
  $view->active(0);

=cut
sub active {
	my $self = shift;
	my $pdata = $self->privateData();
	if(@_) {
		my $act = $_[0];
		if($act == 1) {
			$self->configure('-highlightbackground' => '#FF0000');
		}
		elsif($act == 0) {
			$self->configure('-highlightbackground' => '#666666');
		}
		else {
			croak("act must be 1 or 0");
		}
		$pdata->{active} = $act;
		return(1);
	}
	else {
		return($pdata->{active});
	}
} # end subroutine active definition
########################################################################

=head2 gui_parent

Retrieves or sets the -parent attribute (not to be confused with a
parent window.)

  $gui = $view->gui_parent();
  $view->gui_parent($gui);

=cut
sub gui_parent {
	my $self = shift;
	my $pdata = $self->privateData();
	if(@_) {
		$pdata->{-parent} = $_[0];
	}
	else {
		return($pdata->{-parent});
	}
} # end subroutine gui_parent definition
########################################################################

=head1 Drawing Methods

The following methods handle the drawing of items from CAD::Drawing
objects.

=head2 add_drawing

Adds drawing $drw as number $number.  This tags all of the items drawn
by "$number:$type:$id:$layer".

  $view->add_drawing($number, $drw);

=cut
sub add_drawing {
	my $self = shift;
	my ($n, $drw) = @_;
	foreach my $addr (@{$drw->select_addr()}) {
		# print "draw $addr as $tag\n";
		my $tag = addr_to_tag($n, $addr);
		my $obj = $drw->getobj($addr);
		$self->draw_item($obj, $tag);
		if(0) {
			require YAML;
			print YAML::Dump($obj), "\n";
		}

	}
} # end subroutine add_drawing definition
########################################################################

=head2 drawing_update

Updates the canvas with the item at $addr.

  $view->drawing_update($n, $drw, $addr);

=cut
sub drawing_update {
	my $self = shift;
	my ($n, $drw, $addr) = @_;
	my $tag = addr_to_tag($n, $addr);
	# XXX select?
	my $obj = $drw->getobj($addr);
	$self->redraw_item($obj, $tag);
} # end subroutine drawing_update definition
########################################################################

#these return the tk::zinc type and a data list.
our %trans_subs = (
	lines => sub {
		my ($self,$o) = @_;
		my $data = [
			[map({[$self->cnv_pt(@$_)]} @{$o->{pts}})],

		];
		my $args = {
			-linecolor => undef(),
		};
		return(['curve', $data, $args]);
	},
	plines => sub {
		my ($self,$o) = @_;
		my $data = [
			[map({[$self->cnv_pt(@$_)]} @{$o->{pts}})],

		];
		my $args = {
			($o->{closed} ? (-closed => 1) : ()),
			-linecolor => undef(),
		};
		return(['curve', $data, $args]);
	},
	points => sub {
		my ($self,$o) = @_;
		my @pt = $self->cnv_pt(@{$o->{pt}});
		# ack! the size of this needs to float!
		# print "drawing point\n";
		my $sz = 1;
		my $pts = [
			[$pt[0] - $sz, $pt[1] - $sz],
			[$pt[0] + $sz, $pt[1] + $sz],
			];
		my $args = {
			-linecolor => undef(),
		};
		return(['arc', [$pts, -closed => 1], $args]);
	},
	arcs => sub {
		my ($self, $o) = @_;
		my $data = [
			[
				# this might make a mess:
				# [$self->cnv_pt(map({$_ - $o->{rad}} @{$o->{pt}}))],
				# [$self->cnv_pt(map({$_ + $o->{rad}} @{$o->{pt}}))],

				# XXX will somebody please explain why every toolkit
				# must be so incredibly braindead!
				[ # "top left" rectangle point
				$self->cnv_pt(
					$o->{pt}[0] - $o->{rad},
					$o->{pt}[1] + $o->{rad},
					)
				],
				[ # "bottom right" rectangle point
				$self->cnv_pt(
					$o->{pt}[0] + $o->{rad},
					$o->{pt}[1] - $o->{rad},
					)
				],
			],
		];
		my $args = {
			-startangle => $o->{angs}[0] * 180 / pi,
			-extent => abs(($o->{angs}[1] - $o->{angs}[0]) * 180 / pi),
			-linecolor => undef(),
		};
		return(['arc', $data, $args]);
	},
	circles => sub {
		my ($self, $o) = @_;
		my $data = [
			[
				# XXX will somebody please explain why every toolkit
				# must be so incredibly braindead!
				[ # "top left" rectangle point
				$self->cnv_pt(
					$o->{pt}[0] - $o->{rad},
					$o->{pt}[1] + $o->{rad},
					)
				],
				[ # "bottom right" rectangle point
				$self->cnv_pt(
					$o->{pt}[0] + $o->{rad},
					$o->{pt}[1] - $o->{rad},
					)
				],
			],
		];
		# print "points: $data->[0][0][0],$data->[0][0][1] and ",
		# 	"$data->[0][1][0],$data->[0][1][1]\n";
		my $args = {
			-startangle => 0,
			-closed => 1,
			# -extent => 360,
			-linecolor => undef(),
		};
		return(['arc', $data, $args]);
	},
	texts => sub {
		my ($self, $o) = @_;
		my @pt = $self->cnv_pt(@{$o->{pt}});
		# XXX there's some kind of buffer under my text!
		$pt[1] += 3/12;
		my $data = [
		];
		my $args = {
			-position => [@pt],
			-text => $o->{string},
			-composescale => 1, # enable scaling!
			-composerotation => 1,
			-anchor => 'sw',
			# -font => $self->fontCreate(
			# 	$o,
			# 	-family => 'Courier',
			# 	-size => 12,
			# 	),
			# -font => 'lucidiasans-' . 2,
			# XXX okay, assume a 12pt font is 9px high:
			# -font => '-adobe-helvetica-bold-r-normal--12-120-*-*-*-*-*-*',
			# XXX or, 24pt is 15 high:
			-font => '-adobe-helvetica-*-r-normal--24-240-*-*-*-*-*-*',
			-color => undef(),
		};
		## print "text add at point: ",
		## 	join(",", $self->cnv_pt(@{$o->{pt}})),
		## 	"(", join(",", @{$o->{pt}}), ")", "\n";
		return(['text', $data, $args]);
	},
	);

=head2 draw_item

Draws the $obj (possibly in multiple pieces), using $tag as the
identifier.

  $view->draw_item($obj, $tag);

=cut
sub draw_item {
	my $self = shift;
	my ($obj, $tag) = @_;
	my $group = $self->group_is();
	if(my $sub = $trans_subs{$obj->{addr}{type}}) {
		## print "found sub for $obj->{addr}{type}\n";
		my @bits = $sub->($self, $obj);
		foreach my $bit (@bits) {
			my $type = $bit->[0];
			my $data = $bit->[1];
			my %args = %{$bit->[2]};
			# XXX try to handle colors in *ONE* place
			foreach my $key (keys(%args)) {
				if($key =~ m/color$/) {
					my $c = $obj->{color};
					# XXX still punting bylayer/byblock colors
					#     ack! we would need the drawing for that info!
					($c == 256) and ($c = 255);
					($c == 0) and ($c = 255);
					$args{$key} = "#" . aci2hex($c);
				}
			}

			## print "using data @$data\n";
			my $item = $self->add($type, $group,
				@$data, %args,
				-tags => [$tag],
				);
			# XXX I guess I need to index these?
			# print "item $item for $tag\n";
			if(1 and $type eq 'text') {
				my $font = $self->itemcget($item, '-font');
				my $base = $self->itemcget($item, '-position');
				## print "text has font $font and @$base\n";
				# ack! why is this so hard!
				$self->itemconfigure($item, -position => [0,0]);
				# XXX how do I find the height of this text?
				# (see assumption above:
				#   1 unit is 12 pt and takes 9 pixels)
				#   1 unit is 24 pt and takes 15 pixels)
				my $scale = $obj->{height} / 15; 
				## print "initial scaling text by $scale\n";
				$self->scale($item, $scale, $scale);
				if($obj->{ang}) {
					die "need text-angle support\n";
				}
				$self->itemconfigure($item, -position => $base);
			}
			## print "mapping:  $tag -> $item\n";
		}
	}
	else {
		warn("no sub for $obj->{addr}{type}\n");
	}
} # end subroutine draw_item definition
########################################################################

=head2 redraw_item

  $view->redraw_item();

=cut
sub redraw_item {
	my $self = shift;
	my ($obj, $tag) = @_;
	## print "item $tag ", $self->coords($tag), "\n";
	if(my $sub = $trans_subs{$obj->{addr}{type}}) {
		## print "found sub for $obj->{addr}{type}\n";
		my @bits = $sub->($self, $obj);
		foreach my $bit (@bits) {
			# XXX ack!
			my $type = $bit->[0];
			my $data = $bit->[1];
			my %args = %{$bit->[2]};
			foreach my $key (keys(%args)) {
				if($key =~ m/color$/) {
					my $c = $obj->{color};
					# XXX still punting bylayer/byblock colors
					#     ack! we would need the drawing for that info!
					($c == 256) and ($c = 255);
					($c == 0) and ($c = 255);
					$args{$key} = "#" . aci2hex($c);
				}
			}
			0 and print "configure $tag to be @$data, ",
				join(" ", each(%args)), " etc\n";
			$self->itemconfigure($tag, %args);
			if(ref($data->[0]) eq "ARRAY") {
				## XXX the transform is off!
				## print "input data: ", join(" ", map({join(",", @$_)} @{$data->[0]})), "\n";
				## print "current coords: ", join(" ", map({join(",", @$_)} $self->coords($tag))), "\n";
				# ack! XXX this is screwy!
				my @coords = map({
					my @p = $self->world_pt(@$_); [$p[0], -$p[1]]
					} @{$data->[0]});
				## print "input data2: ", join(" ", map({join(",", @$_)} @coords)), "\n";
				$self->coords($tag, \@coords);
				## print "current coords: ", join(" ", map({join(",", @$_)} $self->coords($tag))), "\n";
			}
		}
	}
} # end subroutine redraw_item definition
########################################################################



=head1 Useful Methods

=head2 viewAll

  $view->viewAll();

=cut
sub viewAll {
	my $self = shift;
	# ($self->width == 1 and $self->height == 1) and return();
    if (!$self->type($self->group_is())) {return;} # can't find anything!
	my @bbox = $self->bbox('all');
	$self->viewArea(@bbox);
} # end subroutine viewAll definition
########################################################################

=head2 viewArea

  $view->viewArea(@bbox);

=cut
sub viewArea {
	my $self = shift;
	my (@bbox) = @_;
	# let's be nice and sort these for the caller:
	($bbox[0],$bbox[2]) = sort({$a<=>$b} $bbox[0],$bbox[2]);
	($bbox[1],$bbox[3]) = sort({$a<=>$b} $bbox[1],$bbox[3]);
	my @span = ($bbox[2]-$bbox[0], $bbox[3]-$bbox[1]);
	($span[0] and $span[1]) or return(); # nothing on canvas
	## print "bbox says @bbox (@span)\n";
	my @d_cent = map({$_ / 2} $bbox[2]+$bbox[0], $bbox[3]+$bbox[1]);
	my @view = ($self->width, $self->height);
	my @c_cent = map({$_ / 2} @view);
	## print "change center @c_cent to @d_cent\n";
	my @move = map({$c_cent[$_] - $d_cent[$_]} 0,1);
	my $pdata = $self->privateData();
	if(abs($move[0]) >= 1 and abs($move[1]) >=1) {
		## print "move by @move\n";
		$pdata->{move}[$_] += $move[$_] * $pdata->{scale} foreach 0,1;
		$self->translate($pdata->{group}, @move);
	}
	my $scale = (sort({$a<=>$b} map({$view[$_] / $span[$_]} 0,1)))[0];
	$self->zoom($scale);
} # end subroutine viewArea definition
########################################################################

=head2 viewWorldArea

  $view->viewWorldArea([$x1,$y1],[$x2,$y2]);

=cut
sub viewWorldArea {
	my $self = shift;
	my (@rec) = @_;
	my @bbox = (
		$self->cnv_pt(@{$rec[0]}),
		$self->cnv_pt(@{$rec[1]})
		);
	$self->viewArea(@bbox);
} # end subroutine viewWorldArea definition
########################################################################

=head2 zoom

  $view->zoom($factor);

=cut
sub zoom {
	my $self = shift;
	my $scale = shift;
	my @view = ($self->width, $self->height);
	my @c_cent = map({$_ / 2} @view);
	my $pdata = $self->privateData();
	$self->scale($pdata->{group}, $scale, $scale, @c_cent);
	$pdata->{scale} *= $scale;
} # end subroutine zoom definition
########################################################################

=head2 windowzoom

Creates temporary bindings for drawing a rubber-band box and zooming on
the area described by it.  This will put back your existing bindings.

  $view->windowzoom();

=cut
sub windowzoom {
	my $self = shift;
	# XXX how to get my stl?
	my $stl = shift;
	$stl and $stl->configure(-text=>"Pick window corners");
	my %was;
	my %tmp; # must declare before declaring
	%tmp = (
		'<ButtonPress-1>' => sub {
			$self->rubberBand(0);
		},
		'<B1-Motion>' => sub {
			$self->rubberBand(1);
		},
		'<ButtonRelease-1>' => sub {
			my @box = $self->rubberBand(2);
			## print "box is @box\n";
			$self->viewArea(@box);
			my $parent = $self->gui_parent();
			$parent->event_done();
			$stl and  $stl->configure(-text=>"");
		},
	);
	%was = $self->bind_on(\%tmp);
	return(\%tmp, \%was);
} # end subroutine windowzoom definition
########################################################################

=head2 free_dist

This is a freehand measuring tape.  Maybe we'll have some snaps someday
(but likely not with this graphical toolkit.)

  $view->free_dist();

=cut
sub free_dist {
	my $self = shift;
	my $stl = shift;
	$stl and $stl->configure(-text=>"Pick ends");
	my %was;
	my %tmp;
	%tmp = (
		'<ButtonPress-1>' => sub {
			$self->rubberBand(0);
		},
		'<B1-Motion>' => sub {
			$self->rubberBand(1, 'line');
		},
		'<ButtonRelease-1>' => sub {
			my @box = $self->rubberBand(2);
			# this needs to involve the parent
			# XXX how to make it cleaner?
			my $parent = $self->gui_parent();
			$parent->event_done();
			# print "box is @box\n";
			my @pts = map({[$self->world_pt(@$_)]}
				[@box[0,1]],[@box[2,3]]
				);
			my $dist = dist2d(@pts);
			my $dx = $pts[1][0] - $pts[0][0];
			my $dy = $pts[1][1] - $pts[0][1];
			$stl and $stl->configure(-text=>"$dist ($dx,$dy)");
			warn("measure: $dist ($dx,$dy)\n");
		},
		);
	%was = $self->bind_on(\%tmp);
	return(\%tmp, \%was);
} # end subroutine free_dist definition
########################################################################

=head2 pan

  $view->pan($x,$y);

=cut
sub pan {
	my $self = shift;
	my (@move) = @_;
	my $pdata = $self->privateData();
	# print "pan\n";
	$pdata->{move}[$_] += $move[$_] * $pdata->{scale} foreach 0,1;
	$self->translate($pdata->{group}, @move);
} # end subroutine pan definition
########################################################################

=head1 Additional Methods

=head2 click_bind

Binds a subroutine to mouse button-1 clicks.  In addition to creating
the binding, this subroutine is guaranteed to be passed world
coordinates. (its arguments are: $view, $x, $y)

  $view->click_bind($sub, $button);

The $button argument is optional, and defaults to 1.

I advise you to not use 2 if view_bindings() is active.

=cut
sub click_bind {
	my $self = shift;
	my ($sub, $num)  = @_;
	$num or ($num = 1);
	(ref($sub) eq "CODE") or croak("cannot bind without code\n");
	# sorry, no restore method here!
	$self->Tk::bind(
		"<ButtonPress-$num>" =>
			sub {
				my @loc = $self->eventLocation();
				@loc = $self->world_pt(@loc);
				$sub->($self, @loc);
			}
		);
} # end subroutine click_bind definition
########################################################################

=head2 view_bindings

Sets-up the wheel-zoom and middle-button pan.  (This over-writes any
bindings that you have made.)

  $view->view_bindings();

=cut
sub view_bindings {
	my $self = shift;
	$self->Tk::bind('<Double-Button-2>' => sub{ $self->viewAll(); });
	$self->Tk::bind('<4>' => sub{
				## print "zoom in\n";
				$self->zoom(1.125);
				## print "zoom in done\n";
			}
			);
	$self->Tk::bind('<5>' => sub{
				$self->zoom(1/1.125);
			}
			);
	my $pdata = $self->privateData();
	my @pan_start;
	my $drag_current;
	$self->Tk::bind(
		'<ButtonPress-2>' => sub {
			@pan_start = $self->eventLocation();
			## print "starting pan at @pan_start\n";
		});
	# have to have this here to prevent spurious panning with double-clicks
	$self->Tk::bind('<B2-Motion>' => sub {
				$drag_current = 1;
				my @pan_stop = $self->eventLocation();
				my @diff = map({$pan_stop[$_] - $pan_start[$_]} 0,1);
				if(sqrt($diff[0]**2 + $diff[1]**2) > $pdata->{-pandist}) {
					$self->pan(@diff);
					@pan_start = @pan_stop;
				}

			}
		);
	$self->Tk::bind(
		'<ButtonRelease-2>' => sub {
			$drag_current || return();
			my @pan_stop = $self->eventLocation();
			# my $scale = $self->pixelSize();
#            print "\tdouble: $isdouble\n";
#            print "\tdrag: $drag_current\n";
#            print "scale is $scale\n";
#            print "stopping pan at @pan_stop\n";
			my @diff = map({$pan_stop[$_] - $pan_start[$_]} 0,1);
#            my $panx = abs($diff[0])/$scale;
#            my $pany = abs($diff[1])/$scale;
#            print "pixels: ($panx,$pany)\n";
#            my $dopan = ( $panx > 10) | ( $pany > 10);
#            $dopan && print "panning by @diff\n";
#            $dopan && $self->panWorld(@diff);
			$self->pan(@diff);
			$drag_current = 0;
		});

} # end subroutine view_bindings definition
########################################################################

=head1 Coordinate System Methods

=head2 world_pt

Change a canvas coordinate into a world coordinate.

  @w_pt = $view->world_pt(@cnv_pt);

=cut
sub world_pt {
	my $self = shift;
	my (@pt) = @_;
	## print "start with @pt\n";
	# print "look at transform effect:", 
	# 	join(",", $self->transform($self->group_is(), \@pt)), "\n";
	# # XXX use scale and movement
	# my $pdata = $self->privateData();
	# $pdata->{scale} or die("no scale!");
	# @pt = map({$pt[$_] - $pdata->{move}[$_]} 0,1);
	# print "after move: @pt\n";
	# # XXX scaling has happened about canvas center
	# @pt = map({$pt[$_] / $pdata->{scale}} 0,1);
	@pt = $self->transform($self->group_is(), \@pt);
	return($pt[0], -$pt[1]);
} # end subroutine world_pt definition
########################################################################

=head2 cnv_pt

Change a world coordinate into a canvas coordinate.

  @cnv_pt = $view->cnv_pt(@w_pt);

=cut
sub cnv_pt {
	my $self = shift;
	my (@pt) = @_;
	@pt = $self->transform($self->group_is(), 'device', [$pt[0], -$pt[1]]);
	return(@pt);
} # end subroutine cnv_pt definition
########################################################################

=head2 eventLocation

Returns the canvas (x,y) coordinates of the last event.

  my ($x,$y) = $view->eventLocation();

=cut
sub eventLocation {
    my ($canvas) = @_;
    my $ev = $canvas->XEvent;
    return ($ev->x,$ev->y) if defined $ev;
    return;
} # end subroutine eventLocation definition
########################################################################

=head2 event_coords

Returns the world (x,y) coordinates of the last event.

  ($x,$y) = $view->event_coords();

=cut
sub event_coords {
	my $self = shift;
	my ($x,$y) = $self->eventLocation();
	return($self->world_pt($x,$y));
} # end subroutine event_coords definition
########################################################################

########################################################################

=head2 rubberBand

almost straight from WorldCanvas

=cut
sub rubberBand {
    my ($canvas, $step, $thing) = @_;

    my $pData = $canvas->privateData;
    if($step >= 1 and not defined $pData->{'RubberBand'}) {
		return();
	}

    my $ev = $canvas->XEvent;
    my $x = $ev->x;
    my $y = $ev->y;

    if ($step == 0) {
        # create anchor for rubberband
        _killBand($canvas);
        $pData->{'RubberBand'} = [$x, $y, $x, $y];
    } elsif ($step == 1) {
        # update end of rubber band and redraw
        $pData->{'RubberBand'}[2] = $x;
        $pData->{'RubberBand'}[3] = $y;
        _killBand($canvas);
		$thing or ($thing = "rectangle");
        _makeBand($canvas, $thing);
    } elsif ($step == 2) {
        # step == 2: done
        _killBand($canvas) or return;

        my ($x1, $y1, $x2, $y2) = @{$pData->{'RubberBand'}};
        undef($pData->{'RubberBand'});

        return ($x1, $y1, $x2, $y2);
    }
}

sub _killBand {
    my ($canvas) = @_;

    my $id = $canvas->privateData->{'RubberBandID'};
    return 0 if !defined($id);

    $canvas->SUPER::remove($id);
    undef($canvas->privateData->{'RubberBandID'});

    return 1;
}

sub _makeBand {
    my ($canvas, $thing) = @_;

    my $pData = $canvas->privateData;
    my $rb = $pData->{'RubberBand'};
    die "Error: RubberBand is not defined" if !$rb;
    die "Error: RubberBand does not have 4 values." if @$rb != 4;

    my $crbx1 = $rb->[0];
    my $crbx2 = $rb->[2];
    my $crby1 = $rb->[1];
    my $crby2 = $rb->[3];

    my $color = $canvas->cget('-bandColor');
	# print "color: $color\n";
	# print "points: $crbx1, $crby1, $crbx2, $crbx1\n";
	$color = '#FF0000';
	my $id;
	if($thing eq "rectangle") {
		$id = $canvas->add(
			'rectangle', 1,
			[$crbx1, $crby1, $crbx2, $crby2],
			-linecolor => $color
			);
	}
	elsif($thing eq "line") {
		$id = $canvas->add(
			'curve', 1,
			[[$crbx1, $crby1], [$crbx2, $crby2]],
			-linecolor => $color
			);
	}
	else {
		croak("'thing' must be (currently) rectangle or line\n");
	}
    $pData->{'RubberBandID'} = $id;
}
########################################################################

=head2 bind_on

Sets all of the bindings specified in %tmp and returns the old
bindings in %was.

  %was = $view->bind_on(\%tmp);

=cut
sub bind_on {
	my $self = shift;
	my ($tmp) = @_;
	my %was;
	my %tmp = %$tmp;
	foreach my $key (keys(%tmp)) {
		if(my $sub = $self->Tk::bind($key)) {
			$was{$key} = $sub;
		}
		$self->Tk::bind($key, $tmp{$key});
	}
	return(%was);
} # end subroutine bind_on definition
########################################################################

=head2 bind_off

Replaces the %was bindings and removes any leftover from %tmp.

  $view->bind_off(\%tmp, \%was);

=cut
sub bind_off {
	my $self = shift;
	my ($tmp, $was) = @_;
	my %tmp = %$tmp;
	my %was = %$was;
	foreach my $item (keys(%tmp)) {
		# print "item: $item\n";
		if($was{$item}) {
			$self->Tk::bind($item => $was{$item});
		}
		else {
			$self->Tk::bind($item => "");
		}
	}
} # end subroutine bind_off definition
########################################################################

=head1 Functions

=head2 addr_to_tag

  $tag = addr_to_tag($n, $addr);

=cut
sub addr_to_tag {
	my ($n, $addr) = @_;
	my $tag = join(":",
		$n,
		$addr->{type},
		$addr->{id},
		$addr->{layer}
		);
	return($tag);
} # end subroutine addr_to_tag definition
########################################################################

=head2 tag_to_addr

  ($n, $addr) = tag_to_addr($tag);

=cut
sub tag_to_addr {
} # end subroutine tag_to_addr definition
########################################################################

1;
