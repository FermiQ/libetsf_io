AM_FCFLAGS = @NETCDF_INCLUDES@ -I$(top_builddir)/src/low_level -I$(top_builddir)/src/group_level -I$(top_builddir)/src/utils

EXTRA_DIST = MPI_output_of_a_density.f90 README.f90

noinst_PROGRAMS = create_a_crystal_den_file \
	read_write_sub_access \
	convert_to_xyz \
	mix_ETSF_and_non_ETSF \
	read_a_file

create_a_crystal_den_file_SOURCES = create_a_crystal_den_file.f90
create_a_crystal_den_file_LDFLAGS = -L$(top_builddir)/src/group_level
create_a_crystal_den_file_LDADD = libetsf_io.la

read_write_sub_access_SOURCES = read_write_sub_access.f90
read_write_sub_access_LDFLAGS = -L$(top_builddir)/src/group_level -L$(top_builddir)/src/utils
read_write_sub_access_LDADD = libetsf_io.la libetsf_io_utils.la

read_a_file_SOURCES = read_a_file.f90
read_a_file_LDFLAGS = -L$(top_builddir)/src/group_level -L$(top_builddir)/src/utils
read_a_file_LDADD = libetsf_io.la libetsf_io_utils.la

convert_to_xyz_SOURCES = convert_to_xyz.f90
convert_to_xyz_LDFLAGS = -L$(top_builddir)/src/utils -L$(top_builddir)/src/group_level
convert_to_xyz_LDADD = libetsf_io_utils.la libetsf_io.la

mix_ETSF_and_non_ETSF_SOURCES = mix_ETSF_and_non_ETSF.f90
mix_ETSF_and_non_ETSF_LDFLAGS = -L$(top_builddir)/src/group_level
mix_ETSF_and_non_ETSF_LDADD = libetsf_io.la
