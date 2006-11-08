EXTRA_DIST = doc

SUBDIRS = \
	src/low_level \
	src/group_level \
	tests/low_level \
	tests/group_level

run-tests: run-tests-low-level run-tests-group-level

run-tests-group-level: 
	cd tests/group_level; \
	make run-tests

run-tests-low-level:
	cd tests/low_level; \
	make run-tests
