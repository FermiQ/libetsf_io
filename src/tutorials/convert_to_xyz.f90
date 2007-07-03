!!****e* tutorial_group_level/convert_to_xyz
!! NAME
!!  convert_to_xyz
!!
!! FUNCTION
!!  In this example, we will describe how to use the high level routines from
!!  etsf_io_file and etsf_io_tools (from library etsf_io_utils). Doing it, we will
!!  read a cristallographic file, check its validity and convert it to XYZ file,
!!  reading the coordinates of atoms and getting their names.
!!
!!  To compile this exemple, use (assuming default installation paths):
!!   ${F90} -I/opt/include/${F90} -o convert_to_xyz convert_to_xyz.f90
!!          -L/opt/lib -letsf_io_utils -letsf_io -L/usr/lib -lnetcdf
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! SOURCE
program convert_to_xyz

  use etsf_io_low_level
  use etsf_io
  use etsf_io_file
  use etsf_io_tools

  implicit none

  integer :: iargc
  character(len = 4096) :: filename, error_string
  logical :: lstat
  type(etsf_io_low_error) :: error_data
  integer :: i_atom
  double precision :: coord(etsf_3dimlen)

!! NOTES
!!  In this tutorial, we will open an ETSF file, and some variable of the
!!  geometry group (see the first tutorial on how to create_a_crystal_den_file
!!  for further explanations on groups and especially etsf_geometry).
!!
!!  The required data to create an XYZ file are:
!!  * primitive_vectors for the box definition,
!!  * reduced_atom_positions for the atom coordinates,
!!  * atom_species for the nature of elements.
!!
!! SOURCE
  integer :: ncid
  type(etsf_dims) :: dims_data
  type(etsf_geometry) :: geometry_data
  double precision, allocatable, target :: primitive_vectors(:,:)
  double precision, allocatable, target :: reduced_atom_positions(:,:)
  integer, allocatable, target :: atom_species(:)

!! NOTES
!!  The names of atoms receives a special treatment since it can be found in several
!!  variables. The specifications are clear on preference and we will use the
!!  etsf_io_tools_get_atom_names() routine to handle this preference and read
!!  the atom names.
!!
!! SOURCE
  character(len = etsf_charlen), allocatable :: atom_names(:)

!! NOTES
!!  We read the number of argument and get the input filename from the command line.
!!
!! SOURCE
  ! Read number of program argument, should be one.
  if (iargc() /= 1) then
     write(0, *) "Error: one argument is required."
     stop
  end if
  ! Read name of input file.
  call getarg(1, filename)

!! NOTES
!!  Before doing anything else, we check that our file is a valid crystallographic
!!  file. To do it, we use the module etsf_io_file and its routine
!!  etsf_io_file_check(). This routine will open the given file and check that it
!!  machtes one or several requirements (see flags in ETSF_IO_VALIDITY_FLAGS). Flags
!!  can be added to form a complex validation on several specifications.
!!
!!  If an error occurs, we transform the error data to a string and output it on
!!  the standard error.
!!
!! SOURCE
  call etsf_io_file_check(trim(filename), etsf_crystallographic_data, &
       & lstat, error_data)
  if (.not. lstat) then
     write(0, *) "Error: invalid input file, it does not match crystallographic"
     write(0, *) "       requirements. Given reason:"
     call etsf_io_low_error_to_str(error_string, error_data)
     write(0, "(A)") trim(error_string)
     stop
  end if

!! NOTES
!!  Now that our file is valid, we will follow a step by step procedure to
!!  reopen it, read the dimensions, allocate our temporary arrays, read the required
!!  informations, get the atoms names, close the file and output the informations
!!  in XYZ format.
!!
!! SOURCE
  call etsf_io_low_open_read(ncid, trim(filename), lstat, error_data = error_data)
  if (.not. lstat) then
     call etsf_io_low_error_to_str(error_string, error_data)
     write(0, "(A)") trim(error_string)
     stop
  end if

!! NOTES
!!  The dimensions are read and stored into dims_data.
!!
!! SOURCE
  call etsf_io_dims_get(ncid, dims_data, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_to_str(error_string, error_data)
     write(0, "(A)") trim(error_string)
     stop
  end if

!! NOTES
!!  We allocate the local arrays where to put the read informations.
!!
!! SOURCE
  allocate(primitive_vectors(dims_data%number_of_cartesian_directions, &
       & dims_data%number_of_vectors))
  allocate(reduced_atom_positions(dims_data%number_of_reduced_dimensions, &
       & dims_data%number_of_atoms))
  allocate(atom_species(dims_data%number_of_atoms))
  allocate(atom_names(dims_data%number_of_atom_species))
  geometry_data%primitive_vectors => primitive_vectors
  geometry_data%reduced_atom_positions => reduced_atom_positions
  geometry_data%atom_species => atom_species

!! NOTES
!!  We get the informations from the NetCDF file for the pointers that have been
!!  associated in geometry_data.
!!
!! SOURCE
  call etsf_io_geometry_get(ncid, geometry_data, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_to_str(error_string, error_data)
     write(0, "(A)") trim(error_string)
     stop
  end if

!! NOTES
!!  We use the high level routine that get the names of atoms. If the file is valid,
!!  it always returns string informations (into @atom_names), but atomic numbers can
!!  also be returned as double values in an optional array (see @atom_numbers). We
!!  don't need here the double values, so we don't use the optional argument.
!!
!! SOURCE
  call etsf_io_tools_get_atom_names(ncid, atom_names, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_to_str(error_string, error_data)
     write(0, "(A)") trim(error_string)
     stop
  end if

!! NOTES
!!  We don't forget to close the file.
!!
!! SOURCE
  call etsf_io_low_close(ncid, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_to_str(error_string, error_data)
     write(0, "(A)") trim(error_string)
     stop
  end if

!! NOTES
!!  Finally we output informations in XYZ format.
!!
!! SOURCE
  write(*, "(I0)") dims_data%number_of_atoms
  write(*, "(3A)") "Converted from '", trim(filename), "'"
  do i_atom = 1, dims_data%number_of_atoms, 1
     coord(1) = primitive_vectors(1, 1) * reduced_atom_positions(1, i_atom) + &
          & primitive_vectors(2, 1) * reduced_atom_positions(2, i_atom) + &
          & primitive_vectors(3, 1) * reduced_atom_positions(3, i_atom)
     coord(2) = primitive_vectors(1, 2) * reduced_atom_positions(1, i_atom) + &
          & primitive_vectors(2, 2) * reduced_atom_positions(2, i_atom) + &
          & primitive_vectors(3, 2) * reduced_atom_positions(3, i_atom)
     coord(3) = primitive_vectors(1, 3) * reduced_atom_positions(1, i_atom) + &
          & primitive_vectors(2, 3) * reduced_atom_positions(2, i_atom) + &
          & primitive_vectors(3, 3) * reduced_atom_positions(3, i_atom)
     write(*, "(A,3E16.6)") trim(atom_names(atom_species(i_atom))), coord
  end do

  deallocate(primitive_vectors)
  deallocate(reduced_atom_positions)
  deallocate(atom_species)
  deallocate(atom_names)
end program convert_to_xyz
!!***
