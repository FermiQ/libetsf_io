dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)

dnl Init basic data for package
dnl define following variables :
dnl  - PACKAGE_NAME, the name ;
dnl  - PACKAGE_TARNAME, the name used for the tarball ;
dnl  - PACKAGE_VERSION, the version ;
dnl  - PACKAGE_STRING, the name + the version ;
dnl  - PACKAGE_BUGREPORT, the mail.
AC_INIT([ETSF - IO library], [0.6], [caliste@pcpm.ucl.ac.be], [etsf_io])

dnl All temporary building files are put in this directory.
dnl his must be called before AM_INIT_AUTOMAKE
AC_CONFIG_AUX_DIR(config)

dnl Init something, don't know exactly what...
AM_INIT_AUTOMAKE

dnl Defining fortran language for tests.
AC_LANG(Fortran)
AC_FC_SRCEXT(f90)

dnl Set th default prefix to /opt (see the discussion
dnl on the nanoquanta mailing list of october 2006).
AC_PREFIX_DEFAULT(/opt)

dnl If FC environement variable is not already set, it
dnl looks for a modern Fortran compiler, prefering newest
dnl fortran available (e. g. Fortran95). Then FC variable
dnl is set.
AC_PROG_FC()
dnl Imported script from ABINIT to defined which compiler is under usage.
dnl It will define fc_type and fc_version that are required when installing.
ABI_PROG_FC()

dnl We custom the module naming scheme depending on the compiler
dnl and the platform.
dnl Not done yet
AC_SUBST(MODULE_EXT, "mod")

dnl Set the FCFLAGS and FFLAGS variable
if test -z "$FFLAGS" ; then
  AC_SUBST(FFLAGS, "-O2")
fi
if test -z "$FCFLAGS" ; then
  AC_SUBST(FCFLAGS, "-O2")
fi

dnl Look for ranlib and canonicalize it with the $host variable
AC_CHECK_TOOL(RANLIB, ranlib, :)
if test "$ranlib" = ":" ; then
  AC_MSG_ERROR(["No 'ranlib' program found."])
fi
dnl Look for ar and canonicalize it with the $host variable
AC_CHECK_TOOL(AR, ar, :)
if test "$ar" = ":" ; then
  AC_MSG_ERROR(["No 'ar' program found."])
fi

dnl Get the NetCDF module
AC_ARG_WITH(netcdf-module-path, AS_HELP_STRING([--with-netcdf-module-path], [Give the path of the NetCDF Fortran90 module (default = /usr/include).]),
            ac_netcdf_mod=$withval, ac_netcdf_mod=/usr/include)
AC_CHECK_FILE(${ac_netcdf_mod}/netcdf.mod, withnetcdf=yes, withnetcdf=no)
if test "$withnetcdf" = "no" ; then
  AC_CHECK_FILE(${ac_netcdf_mod}/NETCDF.mod, withnetcdf=yes, withnetcdf=no)
  if test "$withnetcdf" = "no" ; then
    AC_CHECK_FILE(${ac_netcdf_mod}/NETCDF.MOD, withnetcdf=yes, withnetcdf=no)
  fi
fi
if test "$withnetcdf" = "no" ; then
  echo "Action: install NetCDF and set its path with --with-netcdf-module."
  AC_MSG_ERROR(["No 'NetCDF' module found."])
fi
AC_SUBST(NETCDF_CFLAGS, "$ac_netcdf_mod")

dnl Get the NetCDF library
ac_netcdf_dir=/usr/lib
AC_ARG_WITH(netcdf-ldflags, AS_HELP_STRING([--with-netcdf-ldflags], [Give the flags required to link with the NetCDF library (default is -L/usr/lib).]),
            ac_netcdf_dir=$withval, ac_netcdf_dir=)
if test -n "$ac_netcdf_dir" ; then
  LDFLAGS="$LDFLAGS $ac_netcdf_dir"
fi
FCFLAGS_SVG=$FCFLAGS
FCFLAGS="$FCFLAGS -I$ac_netcdf_mod"
LIBS="$LIBS -lnetcdf"
AC_MSG_CHECKING([for netcdf library])
AC_LINK_IFELSE([
program main
  use netcdf
 integer :: s, ncid
 s = nf90_open(path = "", mode = NF90_NOWRITE, ncid = ncid)
end program main
], withnetcdf=yes, withnetcdf=no)
AC_MSG_RESULT([$withnetcdf])
FCFLAGS=$FCFLAGS_SVG
if test "$withnetcdf" = "no" ; then
  echo "LDFLAGS was $LDFLAGS"
  echo "Action: install NetCDF and set the libaray link path with --with-netcdf-ldflags."
  AC_MSG_ERROR(["No 'NetCDF' library found."])
fi

dnl Test if only the library must be built
ac_build_tests="yes"
AC_ARG_ENABLE(build-tests,
              AS_HELP_STRING([--disable-build-tests],
                             [Don't create the tests binaries (enable by default).]),
              ac_build_tests=$enableval, ac_build_tests="yes")
AM_CONDITIONAL(BUILD_TESTS, test "$ac_build_tests" = "yes")
ac_build_tutorials="no"
AC_ARG_ENABLE(build-tutorials,
              AS_HELP_STRING([--enable-build-tutorials],
                             [Create the tutorial binaries (disable by default).]),
              ac_build_tutorials=$enableval, ac_build_tutorials="no")
AM_CONDITIONAL(BUILD_TUTORIALS, test "$ac_build_tutorials" = "yes")

dnl default installation directories
moduledir=${includedir}/${fc_type}
AC_SUBST(moduledir)

dnl Append $FCFLAGS_SRCEXT to $FCFLAGS since the former is not
dnl added automatically in the Makefile.in.
FCFLAGS="$FCFLAGS $FCFLAGS_SRCEXT"

dnl Give the name of file.in to transform to file
AC_CONFIG_FILES([
Makefile
src/low_level/Makefile
src/group_level/Makefile
src/tutorials/Makefile
src/utils/Makefile
tests/low_level/Makefile
tests/group_level/Makefile
])
AC_OUTPUT

echo "
Basics:
  Prefix:                 $prefix
  Fortran90 compiler:     $FC
  Fortran90 flags:        $FCFLAGS
  Linker flags:           $LDFLAGS
  Linked libraries:       $LIBS
  Installed module dir:   $moduledir
  Installed lib dir:      $libdir
"
