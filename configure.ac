dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)

dnl Init basic data for package
dnl define following variables :
dnl  - PACKAGE_NAME, the name ;
dnl  - PACKAGE_TARNAME, the name used for the tarball ;
dnl  - PACKAGE_VERSION, the version ;
dnl  - PACKAGE_STRING, the name + the version ;
dnl  - PACKAGE_BUGREPORT, the mail.
AC_INIT([ETSF - IO library], [0.1], [caliste@pcpm.ucl.ac.be], [etsf_io])

dnl All temporary building files are put in this directory.
dnl his must be called before AM_INIT_AUTOMAKE
AC_CONFIG_AUX_DIR(config)

dnl Init something, don't know exactly what...
AM_INIT_AUTOMAKE

dnl Defining fortran language for tests.
AC_LANG(Fortran)

dnl If FC environement variable is not already set, it
dnl looks for a modern Fortran compiler, prefering newest
dnl fortran available (e. g. Fortran95). Then FC variable
dnl is set.
AC_PROG_FC()

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

dnl Get the NetCDF library
ac_netcdf_dir=/usr/lib
AC_ARG_WITH(netcdf-path, AS_HELP_STRING([--with-netcdf-path], [Give the path of the NetCDF Fortran90 library (default = /usr/lib).]),
            ac_netcdf_dir=$withval, ac_netcdf_dir=)
if test -n "$ac_lapack_dir" ; then
  LDFLAGS="$LDFLAGS -L$ac_lapack_dir"
fi
AC_CHECK_LIB(netcdf, nf90_open, withnetcdf=yes, withnetcdf=no)
if test "$withnetcdf" = "no" ; then
  AC_MSG_ERROR(["No 'NetCDF' library found."])
  echo "LDFLAGS was $(LDFLAGS)"
  echo "Action: install NetCDF and set its path with --with-netcdf-path."
fi


dnl Give the name of file.in to transform to file
AC_CONFIG_FILES([
Makefile
src/low_level/Makefile
tests/low_level/Makefile
])
AC_OUTPUT

echo "
Basics:
  Prefix:                 $prefix
  Fortran90 compiler:     $FC
  Fortran90 flags:        $FCFLAGS
  Linker flags:           $LDFLAGS
  Linked libraries:       $LIBS
"
