!!****e* tutorial_group_level/mix_ETSF_and_non_ETSF
!! NAME
!!  mix_ETSF_and_non_ETSF
!!
!! FUNCTION
!!  This tutorial is based on the first tutorial that create a density
!!  file. In this example, we introduce how to mix ETSF variables (the
!!  density) and non-ETSF variables (user defined, program dependent values...).
!!
!!  The main difference is to use the etsf_io_<group>_put() routines instead
!!  of the all-in-one etsf_io_data_write() as introduced in the first tutorial.
!!  The changed lines of the first tutorial are kept as commentaries for comparison
!!  purposes.
!!
!!  To compile this exemple, use (assuming default installation paths):
!!   ${F90} -I/opt/include/${F90} -o mix_ETSF_and_non_ETSF mix_ETSF_and_non_ETSF.f90
!!          -L/opt/lib -letsf_io_utils -letsf_io -L/usr/lib -lnetcdf
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! SOURCE
program mix_ETSF_and_non_ETSF

  use etsf_io_low_level
  use etsf_io

  integer :: i
  
!! NOTES
!!  In the variable declarations relative to ETSF_IO, the etsf_group structure
!!  is not used anymore, since the def and put actions will be more atomic
!!  (but not as atomic as handling each variable).
!!
!! SOURCE
  integer                 :: ncid
  logical                 :: lstat
  type(etsf_io_low_error) :: error_data
  type(etsf_groups_flags) :: flags
  type(etsf_dims)         :: dims
  ! Specific variables required by the library
  !FIRST# type(etsf_groups)           :: groups
  type(etsf_geometry), target :: geometry
  type(etsf_main), target     :: main

!! NOTES
!!  The variable declared in the main program are left unchanged.
!!
!! SOURCE
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
!!  The definition of the dimensions is still the same
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
!!  The declaration of the file is almost left unchanged. A file is allocated on disk
!!  with the given dimensions (see the dims variable).
!!
!!  But, the main group is not defined here. This is done because the main group
!!  (density, coefficients of wavefunctions...) in the ETSF_IO specifications
!!  must be declared last.
!!
!! SOURCE
  !FIRST# the etsf_grp_main was declared here.
  flags%geometry = etsf_geometry_all
  flags%main     = etsf_main_none
  call etsf_io_data_init("mix_ETSF_and_non_ETSF.nc", flags, dims, &
                       & "Tutorial ETSF_IO, create a density file", &
                       & "Created by the tutorial example of the library", &
                       & lstat, error_data, overwrite = .true.)
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
!!  The associations between the structures used in the group level and
!!  variable in the main program memory are also kept. Only the gathering of
!!  all groups in the etsf_groups structure is not done.
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
  !FIRST# ! We associate our two group in the container.
  !FIRST# groups%geometry => geometry
  !FIRST# groups%main => main

!! NOTES
!!  The write action is modified. We prefer to do it to avoid to open the file
!!  for the ETSF variables, close it, and reopen it for the non-ETSF variable.
!!  This is of course possible, but the idea of this tutorial is to show how
!!  to use a lower level of access for the ETSF variables.
!!
!!  Then, we open the created file with etsf_io_low_open_modify().
!!  The file is then in a define mode, so we can easily define the non-ETSF
!!  variables.
!!
!! SOURCE
  ! Open file for writing
  call etsf_io_low_open_modify(ncid,"mix_ETSF_and_non_ETSF.nc", &
       & lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if

  ! We define some private non-ETSF variables (and dimensions if necessary).
  call etsf_io_low_def_var(ncid, "age_of_captain", etsf_io_low_integer, &
       & lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  call etsf_io_low_write_dim(ncid, "number_of_captains_children", 2, &
       & lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  call etsf_io_low_def_var(ncid, "age_of_captains_children", etsf_io_low_integer, &
       & (/ "number_of_captains_children" /), lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if

!! NOTES
!!  Now that the non-ETSF variables has been added, we can defined the main
!!  ETSF variables that will be at the end of the file, as required in the
!!  specifications.
!!
!! SOURCE
  call etsf_io_main_def(ncid, lstat, error_data, flags = etsf_main_density)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if

!! NOTES
!!  The all-in-one routine etsf_io_data_write() is replaced here by a group per
!!  group put action.
!!  
!! SOURCE
  ! We write the ETSF variable with the group methods.
  call etsf_io_geometry_put(ncid, geometry, lstat, error_data)
  !FIRST# call etsf_io_data_write("create_a_crystal_den_file.nc", &
  !FIRST#                       & etsf_grp_main + etsf_grp_geometry, &
  !FIRST#                       & groups, lstat, error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  call etsf_io_main_put(ncid, main, lstat, error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if

!! NOTES
!!  After that, we change the file status for write access with
!!  etsf_io_low_set_write_mode() (this is automatically done by the put()
!!  routines in the group level. The  non-ETSF variables are written.
!!  
!! SOURCE
  ! We switch to write mode.
  call etsf_io_low_set_write_mode(ncid, lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if

  ! We write the non-ETSF variables by hand.
  call etsf_io_low_write_var(ncid, "age_of_captain", 42, &
       & lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  call etsf_io_low_write_var(ncid, "age_of_captains_children", (/ 12, 13 /), &
       & lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if

!! NOTES
!!  We don't forget to close the file!
!!  
!! SOURCE
  call etsf_io_low_close(ncid, lstat, error_data = error_data)
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
end program mix_ETSF_and_non_ETSF
!!***
