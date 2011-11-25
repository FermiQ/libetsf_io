lib_LIBRARIES = libetsf_io_low_level.a

EXTRA_DIST = public_variables.f90 \
	read_routines_auto.f90 \
	read_routines.f90 \
	write_routines.f90 \
	write_routines_auto.f90

if CAPITALIZE
module_DATA = ETSF_IO_LOW_LEVEL.@MODULE_EXT@
else
module_DATA = etsf_io_low_level.@MODULE_EXT@
endif

AM_FCFLAGS = @NETCDF_INCLUDES@ -I$(srcdir)

libetsf_io_low_level_a_SOURCES = etsf_io_low_level.f90



#dependencies
etsf_io_low_level.o: etsf_io_low_level.f90 \
	public_variables.f90 \
	read_routines.f90 \
	read_routines_auto.f90 \
	write_routines.f90 \
	write_routines_auto.f90

etsf_io_low_level.@MODULE_EXT@ ETSF_IO_LOW_LEVEL.@MODULE_EXT@: etsf_io_low_level.o
