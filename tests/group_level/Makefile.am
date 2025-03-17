AM_FCFLAGS = -I$(top_builddir)/src/low_level -I$(top_builddir)/src/group_level @NETCDF_INCLUDES@

EXTRA_DIST = \
	test_split_electrons_part1.cdl \
	test_split_electrons_part1.nc \
	test_split_electrons_part2.cdl \
	test_split_electrons_part2.nc \
	test_split_electrons_part3.cdl \
	test_split_electrons_part3.nc \
	tests_run.sh

check_PROGRAMS = tests_init tests_write tests_read tests_copy

tests_init_SOURCES = tests_init.f90
tests_init_LDFLAGS = -L$(top_builddir)/src/group_level
tests_init_LDADD = $(top_builddir)/src/group_level/libetsf_io.la

tests_write_SOURCES = tests_module.f90 tests_write.f90
tests_write_LDFLAGS = -L$(top_builddir)/src/group_level
tests_write_LDADD = $(top_builddir)/src/group_level/libetsf_io.la

tests_read_SOURCES = tests_module.f90 tests_read.f90
tests_read_LDFLAGS = -L$(top_builddir)/src/group_level
tests_read_LDADD = $(top_builddir)/src/group_level/libetsf_io.la

tests_copy_SOURCES = tests_module.f90 tests_copy.f90
tests_copy_LDFLAGS = -L$(top_builddir)/src/group_level
tests_copy_LDADD = $(top_builddir)/src/group_level/libetsf_io.la

TESTS = run

CLEANFILES = run tests_init.log tests_write.log tests_read.log tests_copy.log tests.mod tests.MOD TESTS.MOD test_init_*.nc test_write_*.nc test_read_*.nc test_copy_*.nc

#additional rules
run: $(srcdir)/tests_run.sh tests_init.log tests_write.log tests_read.log tests_copy.log
	\cp $(srcdir)/tests_run.sh run

run-tests: tests_init.log tests_write.log tests_read.log tests_copy.log
	cat *.log

run-tests-init tests_init.log: tests_init
	\rm -f test_init_*.nc
	./tests_init | tee tests_init.log

run-tests-write tests_write.log: tests_write tests_init.log
	\rm -f test_write_*.nc
	./tests_write | tee tests_write.log

run-tests-read tests_read.log: tests_read tests_init.log
	\rm -f test_read_*.nc
	./tests_read | tee tests_read.log

run-tests-copy tests_copy.log: tests_copy tests_write.log
	\rm -f test_copy_*.nc
	./tests_copy | tee tests_copy.log
