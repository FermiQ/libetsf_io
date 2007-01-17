vpath %.a $(top_builddir)/src/group_level

AM_FCFLAGS = -I$(top_builddir)/src/low_level -I$(top_builddir)/src/group_level

noinst_PROGRAMS = create_a_crystal_den_file read_write_sub_access

create_a_crystal_den_file_SOURCES = create_a_crystal_den_file.f90
create_a_crystal_den_file_LDFLAGS = -L$(top_builddir)/src/group_level
create_a_crystal_den_file_LDADD = -letsf_io

read_write_sub_access_SOURCES = read_write_sub_access.f90
read_write_sub_access_LDFLAGS = -L$(top_builddir)/src/group_level
read_write_sub_access_LDADD = -letsf_io

#dependencies
create_a_crystal_den_file.o: create_a_crystal_den_file.f90 \
	libetsf_io.a

read_write_sub_access.o: read_write_sub_access.f90 \
	libetsf_io.a
