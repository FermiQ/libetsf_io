!!****g* tutorials/etsf_io_tutorials
!! NAME
!!  etsf_io_tutorials -- ESTF I/O examples and tutorials
!!
!! FUNCTION
!!  Tutorials are directly Fortran source code, highly commented, located in
!!  src/tutorials. These codes can be compiled and executed to create example
!!  files.
!!
!!  The following tutorials are available:
!!  * Tutorial 1 - basics of file creation
!!    create_a_crystal_den_file, the first tutorial, is intended to explain the
!!    basics and the philosophy of this library. It details the first steps
!!    required to create a density file, using high level routines
!!    (etsf_io_data_<action>). It shows how to use the pointers and the unformatted
!!    ones (used to map any shape arrays between the ETSF definition and the main
!!    program memory).
!!  * Tutorial 2 - advanced writing, sub-access on k point and spin
!!    read_write_sub_access, the second tutorial, introduces the group level
!!    routines and explain how to access only sub part of arrays. This sub access
!!    is possible when one array has a dimension on spin or k points. Then one can
!!    access data for one k point or spin at a time. This is controlled by some
!!    attributes in the concerned groups, called
!!    <short_var_name>__[spin|kpoint]_access</code>. In this tutorial a
!!    wave-function file is created and the coefficients of wave-functions are
!!    written for one k point at a time.
!!  * Tutorial 3 - a converter tool, usage of validity checks
!!    convert_to_xyz, the third tutorial, shows how to use high level modules
!!    etsf_io_file and etsf_io_tools to check the conformance of an input ETSF file
!!    on cristalographic specifications and then to read atomic coordinates and
!!    names to create a simple XYZ file.
!!  * Tutorial 4 - how to use split capabilities in conjonction with MPI?
!!    MPI_output_of_a_density, the fourth tutorial, shows how to use the split
!!    definitions as defined in the specifications to handle MPI computations. This
!!    is possible with the help of the etsf_split structure. This tutorial create a
!!    density file with a paralelisation on z planes. Each process compute a
!!    gaussian in its own z planes and create an ETSF file with a split on
!!    number_of_grid_points_vector3. Thanks to etsf_io the created files can be then
!!    gathered into one unique file.
!!  * Tutorial 5 - mixing ETSF and non-ETSF variables in file creation (tutorial 1 enhancement)
!!    mix_ETSF_and_non_ETSF, the fifth tutorial, is not focus on the low level API
!!    but it uses it in several areas. This tutorial shows how to write an ETSF file
!!    with additional non-ETSF variables. These variables are defined and written
!!    directly by using the low level API. Besides it also shows how to use the
!!    etsf_io_<group>_put() methods in the context of a concurrent list of ETSF and
!!    non-ETSF variables.
!!  * Tutorial 6 - simple read of a wavefunction file (continuation of tutorial 2)
!!    read_a_file, the sixth tutorial, introduces the read actions in a simple case.
!!    Here, we know that the file should contains the variables of a wavefunction
!!    description. This tutorial uses the file created by tutorial 2 but does not
!!    read it with sub access. Everything is read once as a bloc.
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!!***
