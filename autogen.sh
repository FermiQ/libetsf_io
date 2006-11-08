#!/bin/sh

# Script used to generate configure script from directives.
echo "== Generate sources =="
echo "Generates read routines for nDimensional arrays."
config/scripts/autogen_low_read_var.sh
echo "Generates write routines for nDimensional arrays."
config/scripts/autogen_low_write_var.sh
echo "Generate the group level module."
config/scripts/autogen_module.py
echo "Generate the group level subroutines & Makefile.am."
config/scripts/autogen_subroutines.py
echo "Generate the group level tests."
config/scripts/autogen_tests.py
echo

echo "== Generate documentations =="
echo "Use ROBODoc."
robodoc --rc config/robodoc/robodoc-f90.rc > robodoc.log 2>&1
echo

echo "== Generate build system =="
echo "Listing known macro with 'aclocal'."
aclocal
echo "Creating configure script  with 'autoconf'."
autoconf
echo "Creating required files for autotools."
automake --add-missing --copy
echo
