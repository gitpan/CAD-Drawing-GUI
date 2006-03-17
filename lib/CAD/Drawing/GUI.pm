package CAD::Drawing::GUI;

use CAD::Drawing::GUI::View;

our $VERSION = '0.01_01';

use strict;
use warnings;
use Carp;

=head1 NAME

CAD::Drawing::GUI - A can of worms uncapped and recapped.

=head1 DESCRIPTION

This module organizes one or more CAD::Drawing::GUI::View objects around
one or more CAD::Drawing objects.  This should be fun.

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

  CAD::Drawing::GUI::View

=cut


=head1 Constructor

=head2 new

  $gui = CAD::Drawing::GUI->new();

=cut
sub new {
	my $caller = shift;
	my $class = ref($caller) || $caller;
	my $self = {@_};
	$self->{b_cache} = {}; # binding cache?
	bless($self, $class);
	return($self);
} # end subroutine new definition
########################################################################

=head1 Drawing Management Methods

=head2 add_drawing

Adds a drawing to the control of the $gui object.  Returns the id for
this drawing.  These id's are not re-used.

  $number = $gui->add_drawing($drw, \%options);

=cut
sub add_drawing {
	my $self = shift;
	my ($drw, $opts) = @_;
	$self->{drws} or ($self->{drws} = []);
	my $index = scalar(@{$self->{drws}});
	$self->{drws}[$index] = $drw;
	# this should show-up on all existing views and be drawn on any new
	# views.
	if($self->{views}) {
		foreach my $view (@{$self->{views}}) {
			$view->add_drawing($index, $drw);
		}
	}
	return($index);
} # end subroutine add_drawing definition
########################################################################

=head2 drw_update

Updates all of the canvases with the drawing numbered $n and item $addr.

  $gui->drw_update($n, $addr);

=cut
sub drw_update {
	my $self = shift;
	my ($n, $addr) = @_;
	$self->{views} or croak("no views");
	my $drw = $self->{drws}[$n];
	$drw or croak("no drawing numbered $n");
	foreach my $view (@{$self->{views}}) {
		$view->drawing_update($n, $drw, $addr);
	}
} # end subroutine drw_update definition
########################################################################

=head1 View Methods

=head2 add_view

  my $view = $mw->CADView();
  $gui->add_view($view);

=cut
sub add_view {
	my $self = shift;
	my ($view) = @_;
	$self->{views} or ($self->{views} = []);
	my $index = scalar(@{$self->{views}});
	$self->{views}[$index] = $view;
	$view or croak('add_view($view)');
	$view->configure('-parent' => $self);
	for(my $d = 0; $d < @{$self->{drws}}; $d++) {
		my $drawing = $self->{drws}[$d];
		$drawing or next;
		$view->add_drawing($d, $drawing);
	}
} # end subroutine add_view definition
########################################################################

=head2 new_view

Creates a new CAD::Drawing::GUI::View object, packs, and returns it.  If
you want more control, see add_view().

  my $view = $gui->new_view($mw, \%options);

=cut
sub new_view {
	my $self = shift;
	my ($mw, $opts) = @_;
	my %options;
	if(defined($opts)) {
		if(ref($opts) eq "HASH") {
			%options = %$opts;
		}
		else {
			croak("not a hash reference: ", ref($opts), "\n");
		}
	}
	my %default = (
		width    => 800,
		height   => 600,
		zoom     => "fit",
		);
	unless($options{size}) {
		foreach my $item ("width", "height") {
			my $val = $options{$item};
			$val || ($val = $default{$item});
			push(@{$options{size}}, $val);
		}
	}
	my ($width,$height) = @{$options{size}};
	my $view = $mw->CADView(
		# XXX need to inform view about it's owner ($self)!
		-parent => $self,
		-width => $width,
		-height => $height,
		-backcolor => 'white',
		);
	$view->pack(-fill => 'both', -expand => 1);
	# $view->configure('-parent' => $self);
	# print "widget options:", join("\n  ", "", map({join(":", @$_)} $view->configure())), "\n";
	$view->view_bindings();
	$self->add_view($view);
	$view->update();
	$view->viewAll();
	return($view);
} # end subroutine new_view definition
########################################################################

=head2 view_bindings

Activate the view_bindings for all views (this is done automatically
with the new_view() method.)

  $gui->view_bindings();

=cut
sub view_bindings {
	my $self = shift;
	my $toggle = shift;
	# XXX toggle is currently ignored
	$self->{views} or croak("no views to bind");
	foreach my $view (@{$self->{views}}) {
		$view->view_bindings();
	}
} # end subroutine view_bindings definition
########################################################################

=head2 key_bindings

Activates the default keybindings for all mainwindows.

  $gui->key_bindings();

These are

  Esc - Stop current command
  q   - Exit (should this call some hook?)

=cut
sub key_bindings {
	my $self = shift;
	$self->{views} or croak("no views to bind");
	my %bindings = (
		'<q>' => sub {
			my $top = shift;
			# my $view = $self->{views}[0];
			# my $mw = $view->toplevel();
			## print "bye\n";
			$top->destroy();
		},
		'<Escape>' => sub {
			$self->event_done();
		},
	);
	my %tops = $self->view_parents();
	foreach my $string (keys(%tops)) {
		my ($top, @views) = @{$tops{$string}};
		foreach my $key (keys(%bindings)) {
			$top->bind($key,
				sub {
					$bindings{$key}->($top);
				}
			);
		}

	}
} # end subroutine key_bindings definition
########################################################################

=head2 view_key_bindings

Activates view-related (zoom, measure, etc) keybindings for all
mainwindows.

  $gui->view_key_bindings();

=cut
sub view_key_bindings {
	my $self = shift;
	$self->{views} or croak("no views to bind");
	my %bindings = (
		'<m>' => sub {
			my $self = shift;
			# XXX need a way to hand these an statusline object
			## print "measuring $self\n";
			$self->free_dist();
		},
		'<z>' => sub {
			my $self = shift;
			## print "zooming $self\n";
			# XXX need a way to hand these an statusline object
			$self->windowzoom();
		},
	);
	my %tops = $self->view_parents();
	# Each toplevel gets the binding
	foreach my $string (keys(%tops)) {
		my ($top, @views) = @{$tops{$string}};
		foreach my $key (keys(%bindings)) {
			my $current = $top->bind($key);
			## $current and print "bind $key has a function\n";
			$top->bind($key,
				sub {
					# unhook anything that was already going
					$self->event_done();
					foreach my $view (@views) {
						## print "bind: $view\n";
						$self->{b_cache}{$view} = [
							$view, $bindings{$key}->($view)
							];
					}
				}
			);
		}
	}

} # end subroutine view_key_bindings definition
########################################################################

=head2 view_parents

Returns a hash of the parents of the gui object's views.  This hash is
keyed by the string representation of the toplevel, and each entry
contains an array reference with the toplevel and it's views.

  $gui->view_parents();

=cut
sub view_parents {
	my $self = shift;
	$self->{views} or croak("no views to unbind");
	my %tops;
	foreach my $view (@{$self->{views}}) {
		my $top = $view->parent();
		## print "top is $top\n";
		$tops{$top} or ($tops{$top} = [$top]);
		push(@{$tops{$top}}, $view);
	}
	return(%tops);
} # end subroutine view_parents definition
########################################################################

=head2 event_done

Calls bind_off() for all views.

  $gui->event_done();

=cut
sub event_done {
	my $self = shift;
	$self->{views} or croak("no views to unbind");
	foreach my $key (keys(%{$self->{b_cache}})) {
		my ($view, $tmp, $was) = @{$self->{b_cache}{$key}};
		## print "unhook $view\n";
		$view->bind_off($tmp, $was);
		delete($self->{b_cache}{$key});
	}
} # end subroutine event_done definition
########################################################################

=head2 click_bind

Calls $view->click_bind() for each active view.  See the
documentation of CAD::GUI::View ($button is optional and defaults to
1.)

  $gui->click_bind($sub, $button);

=cut
sub click_bind {
	my $self = shift;
	$self->{views} or croak("no views to bind");
	foreach my $view (@{$self->{views}}) {
		$view->click_bind(@_);
	}
} # end subroutine click_bind definition
########################################################################

1;
