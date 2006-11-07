EXTRA_DIST = doc

SUBDIRS = \
	src/low_level \
	src/group_level \
	tests/low_level

run-tests: run-tests-low-level

run-tests-low-level: run-tests-low-level-read

run-tests-low-level-read:
	cd tests/low_level; \
	make run-tests
