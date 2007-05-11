!!****e* tutorial_group_level/MPI_output_of_a_density
!! NAME
!!  MPI_output_of_a_density
!!
!! FUNCTION
!!  In this example, we run an MPI computation a density (a centered gaussian),
!!  with a distribution of real space mesh through z planes among processes. The
!!  ETSF files will have a split definition on number_of_grid_points_vector3.
!!
!!  To do it, almost every steps are the same than for the first tutorial
!!  (create_a_crystal_den_file), except that we have now an array (my_grid_points)
!!  that has the definition of the points our part of the density is defined.
!!  Then, we associate this array into a split (see etsf_split) definition and
!!  we use this split definition when the ETSF file is initialised with
!!  etsf_io_data_init().
!!
!!  To compile this example an MPI wrapper must be installed and assuming default
!!  installation paths for ETSF_IO, simply use:
!!   ${MPIF90} -I/opt/include/${F90} -o MPI_output_of_a_density MPI_output_of_a_density.f90
!!             -L/opt/lib -letsf_io -L/usr/lib -lnetcdf
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! SOURCE
program MPI_output_of_a_density
  
  use etsf_io_low_level
  use etsf_io

  implicit none

  include "mpif.h"

  integer :: i, j, k, i_proc, n_proc, ierr
  integer :: my_number_of_planes
  character(len = 256) :: my_filename
  real :: x2, y2, z2
  
  logical                 :: lstat
  type(etsf_io_low_error) :: error_data

!! NOTES
!!  As explained in previous tutorials, the ETSF_IO library requires to defined some
!!  variable that are structures of pointers, or that store the dimensions of arrays.
!!  
!!  We have here a new structure: etsf_split. This structure acts a bit like group
!!  structures (like etsf_electrons) since it is a gathering of pointers. These
!!  pointers can be associated to the arrays that defined a local process definition
!!  of a split variable as defined in the specifications.
!!
!! SOURCE
  ! Specific variables required by the library
  type(etsf_dims)             :: dims
  type(etsf_split)            :: split
  type(etsf_groups)           :: groups
  type(etsf_geometry), target :: geometry
  type(etsf_main), target     :: main

  ! Variables that are declared in the main program in a real case
  double precision, allocatable, target :: density(:, :, :)
  integer, allocatable, target          :: my_grid_points(:)
  double precision, target              :: primitive_vector(3, 3)
  
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, i_proc, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, n_proc, ierr)

!! NOTES
!!  Here, we put all unused variables to etsf_no_dimension (see ETSF_IO_CONSTANTS)
!!  then, all these dimensions will not be defined in the output file and all
!!  depending variables will not be created. This should be used with care since
!!  all variables that may depend on a dimension that has been set to
!!  etsf_no_dimension, will silently be ignored by etsf_io_def_<something>.
!!
!! SOURCE
  dims%max_number_of_angular_momenta  = etsf_no_dimension
  dims%max_number_of_coefficients     = etsf_no_dimension
  dims%max_number_of_projectors       = etsf_no_dimension
  dims%max_number_of_states           = etsf_no_dimension
  dims%number_of_atoms                = etsf_no_dimension
  dims%number_of_atom_species         = etsf_no_dimension
  dims%number_of_kpoints              = etsf_no_dimension
  dims%number_of_spinor_components    = etsf_no_dimension
  dims%number_of_spins                = etsf_no_dimension
  dims%number_of_symmetry_operations  = etsf_no_dimension
  dims%real_or_complex_coefficients   = etsf_no_dimension
  dims%real_or_complex_gw_corrections = etsf_no_dimension
  dims%real_or_complex_potential      = etsf_no_dimension
  dims%real_or_complex_wavefunctions  = etsf_no_dimension

  dims%number_of_components          = 1
  dims%number_of_grid_points_vector1 = 36
  dims%number_of_grid_points_vector2 = 36
  dims%number_of_grid_points_vector3 = 120
  dims%real_or_complex_density       = 1

  ! We compute here the number of planes my process will focus on.
  if (i_proc == n_proc - 1) then
     my_number_of_planes = 120 - 120 / n_proc * (n_proc - 1)
  else
     my_number_of_planes = 120 / n_proc
  end if
!! NOTES
!!  Since we only focus on some z planes and not all, we set the number
!!  of planes we used, as explained in the specifications. To do it, we use
!!  the special dimensions my_<something>, here my_number_of_grid_points_vect3.
!!
!! SOURCE
  dims%my_number_of_grid_points_vect3 = my_number_of_planes

  ! We compute the list of plane ids that will be handled by my process.
  allocate(my_grid_points(my_number_of_planes))
  my_grid_points(:) = (/ (i + 1, &
       & i = i_proc * 120 / n_proc, min((i_proc + 1) * 120 / n_proc, 120)) /)
!! NOTES
!!  The split variable is used by the library (as other groups) with only its
!!  associated pointers. Here, we split only on the z axis, so we associate
!!  my_grid_points_vector3.
!!
!! SOURCE
  split%my_grid_points_vector3 => my_grid_points

!! NOTES
!!  This is the point where the ETSF file is created. It uses the same routine
!!  that the one presented in the first tutorial (create_a_crystal_den_file). The
!!  only difference here is that we pass the optional argument split_definition
!!  with the list of z planes our process is handling.
!!
!! SOURCE
  write(my_filename, "(A,I2.2,A)") "MPI_density_", i_proc, ".nc"
  call etsf_io_data_init(trim(my_filename), etsf_main_density, &
       & etsf_grp_geometry + etsf_grp_main, dims, &
       & "Tutorial ETSF_IO, create a density file with MPI", &
       & "Created by the tutorial example of the library", &
       & lstat, error_data, overwrite = .true., split_definition = split)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if

  ! Computation of the gaussian density
  primitive_vector = 0.d0
  primitive_vector(1, 1) = 18.d0
  primitive_vector(2, 2) = 18.d0
  primitive_vector(3, 3) = 60.d0
  geometry%primitive_vectors => primitive_vector
  groups%geometry => geometry

  allocate(density(36, 36, my_number_of_planes))
  main%density%data3D => density
  groups%main => main
  
  ! We put a gaussian in the density
  do k = 1, my_number_of_planes, 1
     z2 = (real(my_grid_points(k) - 60) / 60.) ** 2
     do j = 1, 36, 1
        y2 = (real(j - 18) / 18.) ** 2
        do i = 1, 36, 1
           x2 = (real(i - 18) / 18.) ** 2
           density(i, j, k) = exp(-(x2 + y2 + z2))
        end do
     end do
  end do

!! NOTES
!!  The write part is not modified by the usage of split data.
!!
!! SOURCE
  call etsf_io_data_write(trim(my_filename), etsf_grp_main + etsf_grp_geometry, &
       & groups, lstat, error_data)
  ! We handle the error
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if


  ! Finalisation and deallocation.
  deallocate(my_grid_points)
  deallocate(density)

  call MPI_FINALIZE(ierr)

end program MPI_output_of_a_density
!!***
