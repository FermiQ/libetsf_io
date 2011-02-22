if BUILD_TUTORIALS
subdir_tutorials = src/tutorials
else
subdir_tutorials = 
endif

ACLOCAL_AMFLAGS = -I config/m4

man1_MANS = doc/etsf_io.1

SUBDIRS = \
	src/low_level \
	src/group_level \
	src/utils \
	tests/low_level \
	tests/group_level \
	tests/utils \
	doc/www \
	$(subdir_tutorials)
