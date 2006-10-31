#!/bin/sh

# Script used to generate configure script from directives.
echo "Generates read routines for nDimensional arrays."
scripts/autogen_read_arrays.sh
echo "Generates write routines for nDimensional arrays."
scripts/autogen_write_arrays.sh
echo "Listing known macro with 'aclocal'."
aclocal
echo "Creating configure script  with 'autoconf'."
autoconf
echo "Creating required files for autotools."
automake --add-missing --copy
