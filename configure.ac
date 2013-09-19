dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.62)

dnl Init basic data for package
dnl define following variables :
dnl  - PACKAGE_NAME, the name ;
dnl  - PACKAGE_TARNAME, the name used for the tarball ;
dnl  - PACKAGE_VERSION, the version ;
dnl  - PACKAGE_STRING, the name + the version ;
dnl  - PACKAGE_BUGREPORT, the mail.
AC_INIT([ETSF - IO library], [1.0.4], [https://bugs.launchpad.net/etsf-io], [etsf_io])

dnl All temporary building files are put in this directory.
dnl his must be called before AM_INIT_AUTOMAKE
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config/m4])

dnl Init something, don't know exactly what...
AM_INIT_AUTOMAKE(1.10)

dnl Defining fortran language for tests.
AC_LANG([Fortran])
AC_FC_SRCEXT([f90])

dnl Set th default prefix to /opt (see the discussion
dnl on the nanoquanta mailing list of october 2006).
AC_PREFIX_DEFAULT([/opt])

dnl If FC environement variable is not already set, it
dnl looks for a modern Fortran compiler, prefering newest
dnl fortran available (e. g. Fortran95). Then FC variable
dnl is set.
AC_PROG_FC()
dnl Imported script from ABINIT to defined which compiler is under usage.
dnl It will define fc_type and fc_version that are required when installing.
ABI_PROG_FC()

dnl Set if the module name are capitalized or not with the FC compiler.
AC_MSG_CHECKING([for module extension for compiler '$fc_type'])
case "$fc_type" in
  "pathscale")
     capitalize_module=yes
     capitalize_module_ext="mod" ;;
  "open64")
     capitalize_module=yes
     capitalize_module_ext="mod" ;;
  *)
     capitalize_module=no
     capitalize_module_ext="mod" ;;
esac
AC_MSG_RESULT([$capitalize_module_ext])

dnl We customize the module naming scheme depending on the compiler
dnl and the platform.
dnl Not done yet
AM_CONDITIONAL(CAPITALIZE, test "$capitalize_module" = "yes")
AC_SUBST(MODULE_EXT, $capitalize_module_ext)

dnl Look for ranlib and canonicalize it with the $host variable
AC_CHECK_TOOL(RANLIB, ranlib, :)
if test "$RANLIB" = ":" ; then
  AC_MSG_ERROR(["No 'ranlib' program found."])
fi
dnl Look for ar and canonicalize it with the $host variable
AC_CHECK_TOOL(AR, ar, :)
if test "$AR" = ":" ; then
  AC_MSG_ERROR(["No 'ar' program found."])
fi

dnl NetCDF options
AC_ARG_WITH([netcdf-incs],
  AS_HELP_STRING([--with-netcdf-incs],
    [Include flags for the NetCDF Fortran90 modules (default = /usr/include).]),
  [ac_netcdf_incs="$withval"],
  [ac_netcdf_incs="-I/usr/include"])
AC_ARG_WITH([netcdf-libs],
  AS_HELP_STRING([--with-netcdf-libs],
    [NetCDF link flags.]),
  [ac_netcdf_libs="$withval"],
  [ac_netcdf_libs="-lnetcdff -lnetcdf"])
AC_ARG_WITH([netcdf-prefix],
  AS_HELP_STRING([--with-netcdf-prefix],
    [Prefix directory where to look for NetCDF components.]),
  [ac_netcdf_prefix="$withval"],
  [ac_netcdf_prefix=""])

dnl Look for NetCDF parameters
if test "$ac_netcdf_prefix" != ""; then
  AC_MSG_CHECKING([for nc-config (will override configure options)])
  nc_config_bin="$ac_netcdf_prefix/bin/nc-config"
  if test -x "$nc_config_bin"; then
    AC_MSG_RESULT([yes])
    ac_netcdf_incs="-I`$nc_config_bin --includedir`"
    ac_netcdf_libs=`$nc_config_bin --flibs`
    ac_netcdf_fcflags=`$nc_config_bin --fflags`
  else
    AC_MSG_RESULT([no])
  fi
fi
AC_MSG_CHECKING([for NetCDF includes])
AC_MSG_RESULT([$ac_netcdf_incs])
AC_MSG_CHECKING([for NetCDF libraries])
AC_MSG_RESULT([$ac_netcdf_libs])
AC_MSG_CHECKING([for NetCDF Fortran flags])
AC_MSG_RESULT([$ac_netcdf_fcflags])
AC_SUBST(NETCDF_INCLUDES, $ac_netcdf_incs)

dnl Set the FCFLAGS and FFLAGS variables
if test -z "$FCFLAGS" ; then
  AC_SUBST(FCFLAGS, "$ac_netcdf_fcflags")
fi
if test -z "$FFLAGS" ; then
  AC_SUBST(FFLAGS, "$ac_netcdf_fcflags")
fi

dnl Check whether NetCDF is working
AC_MSG_CHECKING([for NetCDF])
FCFLAGS_SVG="$FCFLAGS"
LIBS_SVG="$LIBS"
FCFLAGS="$FCFLAGS $ac_netcdf_incs"
LIBS="$ac_netcdf_libs $LIBS"
AC_LINK_IFELSE([
program main
  use netcdf
  integer :: s, ncid
  s = nf90_open(path = "", mode = NF90_NOWRITE, ncid = ncid)
end program main
], withnetcdf="yes", withnetcdf="no")
AC_MSG_RESULT([$withnetcdf])
if test "$withnetcdf" = "no" ; then
  echo "Action: install NetCDF and set its parameters through --with-netcdf-incs and --with-netcdf-libs."
  AC_MSG_ERROR(["No 'NetCDF' library found."])
fi

dnl Check whether NetCDF has the comm argument for parallel IO
AC_MSG_CHECKING([for parallel IO with NetCDF])
FCFLAGS_SVG="$FCFLAGS"
LIBS_SVG="$LIBS"
FCFLAGS="$FCFLAGS $ac_netcdf_incs"
LIBS="$ac_netcdf_libs $LIBS"
AC_LINK_IFELSE([
program main
  use netcdf
  integer :: s, ncid
  s = nf90_open(path = "", mode = NF90_NOWRITE, ncid = ncid, comm = 1, info = 1)
end program main
], withpio="yes", withpio="no")
AC_MSG_RESULT([$withpio])
LIBS="$LIBS_SVG"
FCFLAGS="$FCFLAGS_SVG"
AM_CONDITIONAL(HAVE_PARALLEL_IO, test "$withpio" = "yes")

dnl Test if only the library must be built
ac_build_tutorials="no"
AC_ARG_ENABLE(build-tutorials,
              AS_HELP_STRING([--enable-build-tutorials],
                             [Create the tutorial binaries (disable by default).]),
              ac_build_tutorials=$enableval, ac_build_tutorials="no")
AM_CONDITIONAL(BUILD_TUTORIALS, test "$ac_build_tutorials" = "yes")

dnl default installation directories
AC_ARG_WITH(moduledir,
            AS_HELP_STRING([--with-moduledir],
                           [installation directory for module files [[INCLUDEDIR/FC_TYPE]]]),
              ac_moduledir=$withval, ac_moduledir="no")
if test x"$ac_moduledir" != x"no" ; then
  moduledir=$ac_moduledir
else
  moduledir=${includedir}/${fc_type}
fi
AC_SUBST(moduledir)

dnl Append $FCFLAGS_SRCEXT to $FCFLAGS since the former is not
dnl added automatically in the Makefile.in.
FCFLAGS="$FCFLAGS $FCFLAGS_f90"

lowleveldocdir=$docdir"/low_level"
AC_SUBST(lowleveldocdir)
groupleveldocdir=$docdir"/group_level"
AC_SUBST(groupleveldocdir)
utilsdocdir=$docdir"/utils"
AC_SUBST(utilsdocdir)
tutorialsdocdir=$docdir"/tutorials"
AC_SUBST(tutorialsdocdir)

dnl Give the name of file.in to transform to file
AC_CONFIG_FILES([
Makefile
src/low_level/Makefile
src/group_level/Makefile
src/tutorials/Makefile
src/utils/Makefile
tests/low_level/Makefile
tests/group_level/Makefile
tests/utils/Makefile
doc/www/Makefile
doc/www/low_level/Makefile
doc/www/group_level/Makefile
doc/www/utils/Makefile
doc/www/tutorials/Makefile
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
  Installed doc dir:      $docdir
"
