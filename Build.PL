
use strict;
use warnings;
use Module::Build;
my $build_class = 'Module::Build';
my $custom_build = 'Module::Build::Live';
eval("require $custom_build;");
unless($@) {
	warn "using custom build class:  '$custom_build'\n";
	$build_class = $custom_build;
}

my $builder = $build_class->new(
	module_name         => 'CAD::Drawing::GUI',
	license             => 'perl',
	dist_version_from   => 'lib/CAD/Drawing/GUI.pm',
	requires => {
		'Test::More'      => 0,
		'CAD::Drawing'    => 0.26,
		'CAD::Calc'       => 0.25,
		'Tk::Zinc'        => 3.30,
		'CAD::DXF::Color' => 0,
	},
	add_to_cleanup      => [ qw(CAD-Drawing-GUI-* META.yml)],
);

$builder->create_build_script();
