EXTRA_DIST = \
	si1002x1-o_DS1_DEN-etsf.nc \
	si1002x1-o_DS1_DEN-etsf.ref \
	wfs_complex-etsf.nc \
	wfs_complex-etsf.ref \
	wfs_real-etsf.nc \
	wfs_real-etsf.ref

TESTS = runSi runRspc1 runRspc2

CLEANFILES = si1002x1-o_DS1_DEN-etsf.log wfs_complex-etsf.log wfs_real-etsf.log \
	runSi runRspc1 runRspc2

#additional rules
runSi: si1002x1-o_DS1_DEN-etsf.nc si1002x1-o_DS1_DEN-etsf.ref
	echo "#!/bin/sh" > runSi; chmod u+x runSi
	echo "$(top_builddir)/src/utils/etsf_io -a content $(srcdir)/si1002x1-o_DS1_DEN-etsf.nc > si1002x1-o_DS1_DEN-etsf.log" >> runSi
	echo 'diff=`diff $(srcdir)/si1002x1-o_DS1_DEN-etsf.ref si1002x1-o_DS1_DEN-etsf.log`' >> runSi
	echo "if test -z \"\$$diff\" ; then exit 0; else exit 1; fi" >> runSi

runRspc1: wfs_complex-etsf.nc wfs_complex-etsf.ref
	echo "#!/bin/sh" > runRspc1; chmod u+x runRspc1
	echo "$(top_builddir)/src/utils/etsf_io -a content $(srcdir)/wfs_complex-etsf.nc > wfs_complex-etsf.log" >> runRspc1
	echo 'diff=`diff $(srcdir)/wfs_complex-etsf.ref wfs_complex-etsf.log`' >> runRspc1
	echo "if test -z \"\$$diff\" ; then exit 0; else exit 1; fi" >> runRspc1

runRspc2: wfs_real-etsf.nc wfs_real-etsf.ref
	echo "#!/bin/sh" > runRspc2; chmod u+x runRspc2
	echo "$(top_builddir)/src/utils/etsf_io -a content $(srcdir)/wfs_real-etsf.nc > wfs_real-etsf.log" >> runRspc2
	echo 'diff=`diff $(srcdir)/wfs_real-etsf.ref wfs_real-etsf.log`' >> runRspc2
	echo "if test -z \"\$$diff\" ; then exit 0; else exit 1; fi" >> runRspc2
