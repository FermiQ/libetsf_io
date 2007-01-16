vpath %.a $(top_builddir)/src/group_level

bin_PROGRAMS = etsf_io

lib_LIBRARIES = libetsf_io_file.a

module_DATA = etsf_io_file.@MODULE_EXT@

AM_FCFLAGS = -I$(top_builddir)/src/low_level -I$(top_builddir)/src/group_level

libetsf_io_file_a_SOURCES = etsf_io_file.f90

etsf_io_SOURCES = etsf_io.f90
etsf_io_LDFLAGS = -L$(top_builddir)/src/group_level -L.
etsf_io_LDADD = -letsf_io_file -letsf_io


#dependencies
etsf_io_file.o: libetsf_io.a

etsf_io.o: libetsf_io_file.a
