vpath %.o $(top_builddir)/src/low_level

test_dependencies = \
	check_att_t01.nc \
	check_var_t01.nc \
	open_read_t01.nc \
	open_read_t02.nc \
	open_read_t03.nc \
	open_read_t04.nc \
	read_dim_t01.nc \
	read_var_t01.nc

EXTRA_DIST = \
	check_att_t01.cdl \
	open_read_t01.cdl \
	open_read_t02.cdl \
	open_read_t03.cdl \
	open_read_t04.cdl \
	read_var_t01.cdl \
	$(test_dependencies) \
	tests_run.sh

AM_FCFLAGS = -I$(top_builddir)/src/low_level -I@NETCDF_CFLAGS@

check_PROGRAMS = tests_read \
	tests_write

tests_read_SOURCES = tests_read.f90

tests_read_LDFLAGS = -L$(top_builddir)/src/low_level

tests_read_LDADD = -letsf_io_low_level


tests_write_SOURCES = tests_write.f90

tests_write_LDFLAGS = -L$(top_builddir)/src/low_level

tests_write_LDADD = -letsf_io_low_level

TESTS = run

CLEANFILES = run tests_read.log tests_write.log

#dependencies
tests_read.o: tests_read.f90 \
	etsf_io_low_level.o

tests_write.o: tests_write.f90 \
	etsf_io_low_level.o

#additional rules
run: $(srcdir)/tests_run.sh tests_read.log tests_write.log
	\cp $(srcdir)/tests_run.sh run

run-tests: tests_read.log tests_write.log
	cat *.log

run-tests-read tests_read.log: $(test_dependencies) tests_read
	./tests_read $(srcdir) > tests_read.log

run-tests-write tests_write.log: $(test_dependencies) tests_write
	\rm -f open_create_t*.nc
	./tests_write $(srcdir) > tests_write.log
