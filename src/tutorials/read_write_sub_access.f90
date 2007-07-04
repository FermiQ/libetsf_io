!!****e* tutorial_group_level/read_write_sub_access
!! NAME
!!  read_write_sub_access
!!
!! FUNCTION
!!  In this example, we will describe how to read or write sub part of arrays.
!!  For example, to write the wavefunction each k point per k point, we need to
!!  use the wfs_pw__kpoint_access. This tutorial will show how to use all this
!!  kind of <varname>__something_access attributes existing in the different
!!  groups (see etsf_main for instance).
!!
!!  To compile this exemple, use (assuming default installation paths):
!!   ${F90} -I/opt/include/${F90} -o read_write_sub_access read_write_sub_access.f90
!!          -L/opt/lib -letsf_io -letsf_io_utils -L/usr/lib -lnetcdf
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! SOURCE
program read_write_sub_access

  use etsf_io
  use etsf_io_tools

  integer :: i, i_kpt, ncid
  
!! NOTES
!!  All groups than contain arrays that can have a sub access (on spin or on k points)
!!  have attributes built on the following scheme:
!!   <short_variable_name>__[spin|kpoint]_access
!!  When all spin or k points values must be read or write at once, one can let
!!  the default value (etsf_no_sub_access) ; but if one want to read or write only
!!  one spin or one k point, one should put the desired value in this attribute.
!!
!!  All this tutorial is oriented for writing, but it can be adapted easily
!!  for reading.
!!
!!  In the beginning of this tutorial, we define an ETSF file with 2 kpoints. This
!!  file will contain the kpoints group (etsf_kpoints), the group of wave data
!!  (etsf_basisdata) and the main group (etsf_main) with only
!!  the coefficient_of_wavefunctions array.
!!
!!  As shown in the first tutorial (create_a_crystal_den_file), the classical
!!  status variable lstat and error_data are created.
!! SOURCE
  logical                 :: lstat
  type(etsf_io_low_error) :: error_data

  ! Specific variables required by the library
  type(etsf_groups_flags) :: flags
  type(etsf_dims)         :: dims
  type(etsf_kpoints)      :: kpoints
  type(etsf_basisdata)    :: basisdata
  type(etsf_main)         :: main
  
!! NOTES
!!  The following variables are used in the main program to store the informations.
!!  The pointers in the library will be used to point on them. Only some parts of
!!  each group will be used.
!!   * coef_pw: is a two dimensional array that store all the coefficients of
!!   plane waves, but only for one k point.
!!   * red_coord_pw_k: is a two dimensional array that stores the coordinates of plane
!!   waves for each band, but restricted on one k point.
!!
!! SOURCE
  ! Variables that are declared in the main program in a real case
  double precision, allocatable, target :: coef_pw_k(:, :)
  ! Variables that will be used in the basisdata group.
  integer, allocatable, target          :: number_of_coefficients(:)
  integer, allocatable, target          :: red_coord_pw_k(:, :)
  ! Variables that will be used in the kpoints group.
  double precision, allocatable, target :: red_coord_kpt(:, :)
  double precision, allocatable, target :: kpoint_weights(:)
  ! Variable to store the definition of the basis set
  character(len = etsf_charlen), target :: basis
  
!! NOTES
!!  We set the dimension (2 k points, no spin, 5 bands and 100 planewave
!!  coefficients).
!!
!! SOURCE
  dims%max_number_of_coefficients = 100
  dims%max_number_of_states = 5
  dims%number_of_kpoints = 2
  dims%number_of_spinor_components = 1
  dims%number_of_spins = 1
  dims%real_or_complex_coefficients = 2
  
  
!! NOTES
!!  As in the first tutorial (create_a_crystal_den_file), we use the high level
!!  routine etsf_io_data_init to define all dimensions and variables for the file
!!  we want to create.
!!
!!  In that case, we will use a precise definition of variables, not creating all
!!  variables of each included groups. For instance, the basis set will be limited
!!  to the required variables for a plane wave description.
!!
!! SOURCE
  flags%basisdata = etsf_basisdata_basis_set + &
       & etsf_basisdata_red_coord_pw + &
       & etsf_basisdata_n_coeff
  flags%kpoints   = etsf_kpoints_red_coord_kpt + etsf_kpoints_kpoint_weights
  flags%main      = etsf_main_wfs_coeff
  call etsf_io_data_init("read_write_sub_access.nc", flags, dims, &
                       & "Tutorial ETSF_IO, use sub access to read or write", &
                       & "Created by the tutorial example of the library", &
                       & lstat, error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  
!! NOTES
!!  At this time of the example, the disk space to store the wave-function
!!  informations has been reserved. In a real case, we let the main program computing
!!  the plane waves and the arrays that describe them.
!! SOURCE
  write(basis, "(A)") "plane_waves"

  ! The main program allocate memory for its computation.
  allocate(coef_pw_k(2, dims%max_number_of_coefficients * dims%max_number_of_states))
  allocate(number_of_coefficients(dims%number_of_kpoints))
  allocate(red_coord_pw_k(3, dims%max_number_of_coefficients))
  allocate(red_coord_kpt(3, dims%number_of_kpoints))
  allocate(kpoint_weights(dims%number_of_kpoints))
  
  ! The main program compute all coordinates for k points and plane waves...
  red_coord_kpt = reshape( (/ 0.0d0, 0.0d0, 0.0d0, &
                            & 0.5d0, 0.5d0, 0.5d0 /), (/ 3, 2 /))
  kpoint_weights = (/ 0.5d0, 0.5d0 /)
  number_of_coefficients = (/ dims%max_number_of_coefficients, &
       & dims%max_number_of_coefficients /)

!! NOTES
!!  To read or write with sub access, there is no high level routine such as
!!  etsf_io_data_write(). Then, we need to open the file and set it a write state.
!!  The way to open a file for writing is to use the routine etsf_io_low_open_modify()
!!  and then to call etsf_io_low_set_write_mode(). The first call will check that the
!!  header is correct.
!!
!!  When the file is not needed anymore, the ncid id must be released and the file
!!  closed, using etsf_io_low_close(). This is mandatory because without this call
!!  the write action may be not done.
!! SOURCE
  ! Open file for writing
  call etsf_io_low_open_modify(ncid, "read_write_sub_access.nc", &
       & lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  
  ! We switch to write mode.
  call etsf_io_low_set_write_mode(ncid, lstat, error_data = error_data)
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if

!! NOTES
!!  We begin the big loop on k points. In this loop, the main program will compute
!!  the plane waves and the coordinates of the coefficients. Then, it will use the
!!  library to write the values for the current k point.
!! SOURCE
  do i_kpt = 1, dims%number_of_kpoints, 1
     ! We compute the plane wave coefficient with the famous
     ! algorithm that works well.
     do i = 1, dims%max_number_of_coefficients, 1
        red_coord_pw_k(:, i) = (/ -i, 0, i /)
     end do
     coef_pw_k(1, :) = (/ (i, i = 1, &
          & dims%max_number_of_coefficients * dims%max_number_of_states) /)
     coef_pw_k(2, :) = (/ (-i, i = 1, &
          & dims%max_number_of_coefficients * dims%max_number_of_states) /)
  
!! NOTES
!!  We associate the pointers of groups we want to write with the data in memory.
!! SOURCE
     ! We associate the data
     main%coefficients_of_wavefunctions%data2D => coef_pw_k
     ! We set the sub access.
     main%wfs_coeff__kpoint_access = i_kpt
     ! Idem for the reduced coordinates of coefficients.
     basisdata%reduced_coordinates_of_plane_waves%data2D => red_coord_pw_k
     basisdata%red_coord_pw__kpoint_access = i_kpt

!! NOTES
!!  Now that all the arrays we want to write are associated, we can call the write
!!  routine. This routine will read automatically the <var>__kpoint_access attribute
!!  and will check the dimensions of the associated arrays.
!! SOURCE
     ! We use the group level write routine.
     call etsf_io_main_put(ncid, main, lstat, error_data)
     if (.not. lstat) then
        call etsf_io_low_error_handle(error_data)
        stop
     end if
     call etsf_io_basisdata_put(ncid, basisdata, lstat, error_data)
     if (.not. lstat) then
        call etsf_io_low_error_handle(error_data)
        stop
     end if

     ! End of the kpoint big loop.
  end do


!! NOTES
!!  We write the other data that are independent of the kpoint loop.
!!
!! WARNINGS
!!  It is important to associate to nullify the already used pointers
!!  to avoid to write them again.
!! SOURCE
  ! We set the associations.
  kpoints%reduced_coordinates_of_kpoints => red_coord_kpt
  kpoints%kpoint_weights => kpoint_weights
  basisdata%basis_set => basis
  basisdata%reduced_coordinates_of_plane_waves%data2D => null()
  basisdata%number_of_coefficients => number_of_coefficients
  ! We call the group level write routines.
  call etsf_io_kpoints_put(ncid, kpoints, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if
  call etsf_io_basisdata_put(ncid, basisdata, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if

!! NOTES
!!  We then set the use_time_reversal_at_gamma attribute for this file using
!!  the etsf_io_tools module. We write it after the other data since the routine
!!  will check that the basis set is indeed a plane wave one and the two variables
!!  impacted by this attributes must already exist.
!!
!! SOURCE
  call etsf_io_tools_set_time_reversal_symmetry(ncid, .false., lstat, error_data)
  ! We handle the error
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  
!! NOTES
!!  As said before, we need to close the file.
!! SOURCE
  ! We close the file.
  call etsf_io_low_close(ncid, lstat, error_data)
  ! We handle the error
  if (.not. lstat) then
    call etsf_io_low_error_handle(error_data)
    stop
  end if
  
  ! The main program will deallocate its own memory.
  deallocate(coef_pw_k)
  deallocate(number_of_coefficients)
  deallocate(red_coord_pw_k)
  deallocate(red_coord_kpt)
  deallocate(kpoint_weights)
end program read_write_sub_access
!!***
