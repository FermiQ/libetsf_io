EXTRA_DIST = doc

if BUILD_TESTS
subdir_tests = tests/low_level \
	tests/group_level
else
subdir_tests = 
endif

if BUILD_TUTORIALS
subdir_tutorials = src/tutorials
else
subdir_tutorials = 
endif

SUBDIRS = \
	src/low_level \
	src/group_level \
	$(subdir_tests) \
	$(subdir_tutorials)

run-tests: run-tests-low-level run-tests-group-level

run-tests-group-level: 
	cd tests/group_level; \
	make run-tests

run-tests-low-level:
	cd tests/low_level; \
	make run-tests
