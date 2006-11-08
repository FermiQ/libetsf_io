vpath %.a $(top_builddir)/src/group_level

AM_FCFLAGS = -I$(top_builddir)/src/low_level -I$(top_builddir)/src/group_level

noinst_PROGRAMS = tests_init tests_write

tests_init_SOURCES = tests_init.f90
tests_init_LDFLAGS = -L$(top_builddir)/src/group_level
tests_init_LDADD = -letsf_io

tests_write_SOURCES = tests_write.f90
tests_write_LDFLAGS = -L$(top_builddir)/src/group_level
tests_write_LDADD = -letsf_io


#dependencies
tests_init.o: tests_init.f90 \
	libetsf_io.a

#additional rules
run-tests: run-tests-init run-tests-write

run-tests-init: tests_init
	\rm -f test*.nc
	./tests_init

run-tests-write: tests_write 
	\rm -f test_write_*.nc
	./tests_write
