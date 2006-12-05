vpath %.a $(top_builddir)/src/group_level

AM_FCFLAGS = -I$(top_builddir)/src/low_level -I$(top_builddir)/src/group_level

noinst_PROGRAMS = create_a_crystal_den_file

create_a_crystal_den_file_SOURCES = create_a_crystal_den_file.f90
create_a_crystal_den_file_LDFLAGS = -L$(top_builddir)/src/group_level
create_a_crystal_den_file_LDADD = -letsf_io

#dependencies
create_a_crystal_den_file.o: create_a_crystal_den_file.f90 \
	libetsf_io.a
