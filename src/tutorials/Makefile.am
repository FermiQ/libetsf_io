vpath %.a $(top_builddir)/src/group_level \
	$(top_builddir)/src/utils

AM_FCFLAGS = -I@NETCDF_CFLAGS@ -I$(top_builddir)/src/low_level -I$(top_builddir)/src/group_level -I$(top_builddir)/src/utils

noinst_PROGRAMS = create_a_crystal_den_file \
	read_write_sub_access \
	convert_to_xyz

create_a_crystal_den_file_SOURCES = create_a_crystal_den_file.f90
create_a_crystal_den_file_LDFLAGS = -L$(top_builddir)/src/group_level
create_a_crystal_den_file_LDADD = -letsf_io

read_write_sub_access_SOURCES = read_write_sub_access.f90
read_write_sub_access_LDFLAGS = -L$(top_builddir)/src/group_level
read_write_sub_access_LDADD = -letsf_io

convert_to_xyz_SOURCES = convert_to_xyz.f90
convert_to_xyz_LDFLAGS = -L$(top_builddir)/src/utils -L$(top_builddir)/src/group_level
convert_to_xyz_LDADD = -letsf_io_utils -letsf_io

#dependencies
create_a_crystal_den_file.o: create_a_crystal_den_file.f90 \
	libetsf_io.a

read_write_sub_access.o: read_write_sub_access.f90 \
	libetsf_io.a

convert_to_xyz.o: convert_to_xyz.f90 \
	libetsf_io.a \
	libetsf_io_utils.a
