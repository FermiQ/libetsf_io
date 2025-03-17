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

AM_FCFLAGS = -I$(top_builddir)/src/low_level @NETCDF_INCLUDES@

check_PROGRAMS = tests_read \
	tests_write

tests_read_SOURCES = tests_read.f90

tests_read_LDFLAGS = -L$(top_builddir)/src/low_level

tests_read_LDADD = $(top_builddir)/src/low_level/libetsf_io_low_level.la


tests_write_SOURCES = tests_write.f90

tests_write_LDFLAGS = -L$(top_builddir)/src/low_level

tests_write_LDADD = $(top_builddir)/src/low_level/libetsf_io_low_level.la

TESTS = run

CLEANFILES = run tests_read.log tests_write.log

#additional rules
run: $(srcdir)/tests_run.sh tests_read.log tests_write.log
	\cp $(srcdir)/tests_run.sh run

run-tests: tests_read.log tests_write.log
	cat *.log

run-tests-read tests_read.log: $(test_dependencies) tests_read
	./tests_read $(srcdir) | tee tests_read.log

run-tests-write tests_write.log: $(test_dependencies) tests_write
	\rm -f open_create_t*.nc
	./tests_write $(srcdir) | tee tests_write.log
