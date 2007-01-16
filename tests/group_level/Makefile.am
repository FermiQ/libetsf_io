vpath %.a $(top_builddir)/src/group_level

AM_FCFLAGS = -I$(top_builddir)/src/low_level -I$(top_builddir)/src/group_level

noinst_PROGRAMS = tests_init tests_write tests_read tests_copy

tests_init_SOURCES = tests_init.f90
tests_init_LDFLAGS = -L$(top_builddir)/src/group_level
tests_init_LDADD = -letsf_io

tests_write_SOURCES = tests_write.f90
tests_write_LDFLAGS = -L$(top_builddir)/src/group_level
tests_write_LDADD = -letsf_io

tests_read_SOURCES = tests_read.f90
tests_read_LDFLAGS = -L$(top_builddir)/src/group_level
tests_read_LDADD = -letsf_io

tests_copy_SOURCES = tests_copy.f90
tests_copy_LDFLAGS = -L$(top_builddir)/src/group_level
tests_copy_LDADD = -letsf_io


#dependencies
tests_init.o: tests_init.f90 \
	libetsf_io.a

tests_copy.o: libetsf_io.a

#additional rules
run-tests: run-tests-init run-tests-write run-tests-read run-tests-copy

run-tests-init tests_init_done: tests_init
	\rm -f test*.nc
	./tests_init
	touch tests_init_done

run-tests-write: tests_write tests_init_done
	\rm -f test_write_*.nc
	./tests_write

run-tests-read: tests_read tests_init_done
	\rm -f test_read_*.nc
	./tests_read

run-tests-copy: tests_copy
	\rm -f test_copy_*.nc
	./tests_copy $(srcdir)
