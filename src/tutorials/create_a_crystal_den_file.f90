!!****e* etsf_io_tutorials/create_a_crystal_den_file
!! NAME
!!  create_a_crystal_den_file
!!
!! FUNCTION
!!  In this example, we will describe how to use the etsf_io_data_init() routine.
!!  This routine creates a file, conforming to the ETSF specifications, with
!!  several uninitialised variables in it. Then we will see how to write values
!!  into this file, using etsf_io_data_write().
!!
!!  To compile this exemple, use (assuming default installation paths):
!!   ${F90} -I/opt/include/${F90} -o create_a_crystal_den_file create_a_crystal_den_file.f90
!!          -L/opt/lib -letsf_io -L/usr/lib -lnetcdf
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! SOURCE
program create_a_crystal_den_file

  use etsf_io

  integer :: i
  
!! NOTES
!!  All routines from the group level requires two output arguments:
!!   * lstat which is a logical. When .false. something goes wrong in
!!     the routine and the action is aborted. No actions are atomic, which
!!     means that if lstat is .false., the status of the NetCDF file (what
!!     have been done) is not guarantee.
!!   * error_data which a of type #etsf_io_low_error. It contains many informations
!!     about the error if lstat is .false.. One can use etsf_io_low_error_to_str
!!     to get a character(len = 1024) describing the error, or one can implement
!!     one itself since the type is public and documented.
!!
!! SOURCE
  logical                 :: lstat
  type(etsf_io_low_error) :: error_data

!! NOTES
!!  To create a NetCDF, we need to give at creation time all the dimensions that
!!  define the variables. The file will then be allocated on disk and may be write
!!  with values later. All dimensions declared in the ETSF specifications are stored
!!  in a type called etsf_dims. Some of these dimensions are fixed by the specifications
!!  such as character_string_length and will be set by the etsf_io_data_init() routine itself.
!!  Other values are free to be chosen.
!!
!! SOURCE
  type(etsf_dims)         :: dims
  
!! NOTES
!!  To write values in one call into an already defined ETSF file, the type etsf_groups
!!  is used as a container for several groups. Here our container will have associated
!!  pointers on an etsf_geometry and an etsf_main. So we declare them. All the structures
!!  used in this library are only containers and do not have the allocated memory. This
!!  is done to avoid memory duplication when using the library with a code with its own
!!  variables. So we also need some variables (in a real case, they are declared in
!!  the main program) to stored our density and geometric informations.
!!
!! SOURCE
  ! Specific variables required by the library
  type(etsf_groups_flags)     :: flags
  type(etsf_groups)           :: groups
  type(etsf_geometry), target :: geometry
  type(etsf_main), target     :: main
  ! Variables that are declared in the main program in a real case
  double precision, allocatable, target :: density(:)
  integer, target                       :: space_group
  double precision, target              :: primitive_vector(3, 3)
  double precision, allocatable, target :: reduced_atom_positions(:,:)
  integer, allocatable, target          :: atom_species(:)
  character(len=2), allocatable, target :: chemical_symbols(:)
  integer, allocatable, target          :: reduced_symmetry_matrices(:,:,:)
  double precision, allocatable, target :: reduced_symmetry_translations(:,:)
  
!! NOTES
!!  We will create for example a file for the density
!!  of the silane molecule, without spin nor spin-orbit, 1 k point.
!!  We imagine that the molecule no symmetry except identity.
!!
!! SOURCE
  dims%max_number_of_coefficients = 1400
  dims%max_number_of_states = 6
  dims%number_of_atoms = 5
  dims%number_of_atom_species = 2
  dims%number_of_components = 1
  dims%number_of_grid_points_vector1 = 36
  dims%number_of_grid_points_vector2 = 36
  dims%number_of_grid_points_vector3 = 36
  dims%number_of_kpoints = 1
  dims%number_of_spinor_components = 1
  dims%number_of_spins = 1
  dims%number_of_symmetry_operations = 1
  
!! NOTES
!!  Now that dimensions have been stored in the appropriated structure, we can call the
!!  etsf_io_data_init() routine itself. The 'groups' argument is very important
!!  It will tell which variables will we allocated
!!  on disk. All variables are gathered by groups and one can choose one or several
!!  groups to be defined. To do it, use the flags from #FLAGS_VARIABLES,
!!  in a summation for each group in the etsf_groups_flags structure. By default
!!  no group will be defined, to add the geometry group, we will use the value
!!  etsf_geometry_all (from #FLAGS_VARIABLES) ; and to add the density variable (from
!!  the main group), and only this one, we will use etsf_main_denisty.
!!
!!  Other arguments of the routine are quite easy to understand. The optional k_dependent
!!  argument is here to handle the case of reduced_coordinates_of_plane_waves which
!!  shape depends on the value of this attribute. If k_dependent is given .false. (default
!!  is .true.), then all variables with this attribute will be labelled "no" and the
!!  variable reduced_coordinates_of_plane_waves will be a two dimensional array.
!!
!! SOURCE
  flags%geometry = etsf_geometry_all
  flags%main     = etsf_main_density
  call etsf_io_data_init("create_a_crystal_den_file.nc", flags, dims, &
                       & "Tutorial ETSF_IO, create a density file", &
                       & "Created by the tutorial example of the library", &
                       & lstat, error_data)
!! NOTES
!!  The required variables for a density file are in etsf_geometry and
!!  in etsf_main, that's why the groups argument is the sum of the two flags.
!!
!!  We can now, handle the error, if one occured. The method etsf_io_low_error_handle()
!!  is used to print the contains of an error type on the standrard output. If one is
!!  interested on printing the error on something different than the standard output,
!!  one should convert the error into a character(len = 1024) with etsf_io_low_error_to_str()
!!  before.
!! SOURCE
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  
!! NOTES
!!  At this time of the example, the disk space to store the density and the geometric
!!  informations has been reserved. In a real case, we let the main program computing
!!  the density and setting up the geometric informations.
!! SOURCE
  ! The main program allocate memory for its computation.
  allocate(density(36 * 36 * 36))
  allocate(reduced_atom_positions(3,5))
  allocate(atom_species(5))
  allocate(chemical_symbols(2))
  allocate(reduced_symmetry_matrices(3, 3, 1))
  allocate(reduced_symmetry_translations(3, 1))
  
  ! The main program compute all symmetries and set up the positions...
  space_group = 1
  primitive_vector = reshape( (/ 10, 0, 0, 0, 10, 0, 0, 0, 10 /), (/ 3, 3 /))
  reduced_symmetry_matrices = reshape( (/ 1, 0, 0, 0, 1, 0, 0, 0, 1 /), (/ 3, 3, 1 /))
  reduced_symmetry_translations = reshape( (/ 0, 0, 0 /), (/ 3, 1 /))
  reduced_atom_positions = reshape( (/ 0.5d0, 0.5d0, 0.5d0, &
                                     & 0.6d0, 0.6d0, 0.6d0, &
                                     & 0.6d0, 0.4d0, 0.4d0, &
                                     & 0.4d0, 0.4d0, 0.6d0, &
                                     & 0.4d0, 0.6d0, 0.4d0 /), (/ 3, 5 /))
  atom_species = (/ 2, 1, 1, 1, 1 /)
  chemical_symbols = (/ "H ", "Si" /)

  ! We compute the density with a powerful algorithm.
  density = (/ (0.d0 + i, i = 1, 36 * 36 * 36) /)
  
!! NOTES
!!  Before calling the etsf_io_data_write() routine, we associate the pointers of our
!!  group types to the main program memory data. Only associated pointers will be written.
!!  All other defined variables will be let untouched. Some variable are defined with
!!  a type called etsf_io_low_var_double or etsf_io_low_var_integer. These variables
!!  are arrays which could have a different shape in the main program and in the
!!  specifications. For instance, our density is 1D only whereas in the specifications
!!  the density is 5D. So we use the attribute %data1D of the structure 
!!  etsf_io_low_var_double for the density. This will work because data in the main program
!!  memory has the same number of elements than the space defined in the ETSF file AND
!!  data are ordered in the same way (elements along X axis are varying quicker than
!!  along Y or Z).
!! SOURCE
  ! We associate the geometry
  geometry%space_group => space_group
  geometry%primitive_vectors => primitive_vector
  geometry%reduced_symmetry_matrices => reduced_symmetry_matrices
  geometry%reduced_symmetry_translations => reduced_symmetry_translations
  geometry%atom_species => atom_species
  geometry%reduced_atom_positions => reduced_atom_positions
  geometry%chemical_symbols => chemical_symbols
  ! We associate the main data
  ! We don't want to dupplicate the density data even if ours is 1D
  ! and ETSF is 5D, so we use the unformatted pointer in the etsf_main
  ! structure.
  main%density%data1D => density
  ! We associate our two group in the container.
  groups%geometry => geometry
  groups%main => main

  ! We write.
  call etsf_io_data_write("create_a_crystal_den_file.nc", &
                        & groups, lstat, error_data)
  ! We handle the error
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  
  ! The main program will deallocate its own memory.
  deallocate(density)
  deallocate(reduced_atom_positions)
  deallocate(atom_species)
  deallocate(chemical_symbols)
  deallocate(reduced_symmetry_matrices)
  deallocate(reduced_symmetry_translations)
end program create_a_crystal_den_file
!!***
