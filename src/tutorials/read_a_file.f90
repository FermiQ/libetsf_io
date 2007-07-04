!!****e* tutorial_group_level/read_a_file
!! NAME
!!  read_a_file
!!
!! FUNCTION
!!  In this example, we will describe how to read some variables from a file.
!!  This is a basic tutorial where the common ETSF routines (low level and
!!  specification level) will be used. We will see how to handle errors.
!!
!!  This tutorial assume that the second tutorial (read_write_sub_access) has been
!!  done and has produced its file (read_write_sub_access.nc).
!!
!!  To compile this exemple, use (assuming default installation paths):
!!   ${F90} -I/opt/include/${F90} -o read_a_file read_a_file.f90
!!          -L/opt/lib -letsf_io -letsf_io_utils -L/usr/lib -lnetcdf
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! SOURCE
program read_a_file

  use etsf_io_low_level
  use etsf_io
  use etsf_io_tools

  integer :: i, j, k
  logical :: symmetry

  ! Variables related to ETSF reading
  ! ---------------------------------
  ! An id to access the read file.
  integer                 :: ncid
  ! A flag for all etsf_io routine to know if everything went right.
  logical                 :: lstat
  ! The storage for the detailled error.
  type(etsf_io_low_error) :: error_data
  ! The ETSF_IO structure to store all relevant dimensions.
  type(etsf_dims)         :: dims
  ! The ETSF_IO structure to store all the split definitions.
  type(etsf_split)        :: split
  ! The ETSF_IO structure to store the basis set and the k points definitions.
  type(etsf_kpoints)      :: kpoints
  type(etsf_basisdata)    :: basisdata
  type(etsf_main)         :: main

  ! Variables independent from ETSF
  ! -------------------------------
  ! This array will store the wavefunctions.
  double precision, allocatable, target :: pw_coeff(:, :, :, :)
  ! Variables that will be used in the basisdata group.
  integer, allocatable, target          :: number_of_coefficients(:)
  integer, allocatable, target          :: red_coord_pw(:, :, :)
  ! Variables that will be used in the kpoints group.
  double precision, allocatable, target :: red_coord_kpt(:, :)
  double precision, allocatable, target :: kpoint_weights(:)
  ! Variable to store the definition of the basis set
  character(len = etsf_charlen), target :: basis

!! NOTES
!!  The file is simply open using a low level routine. We simply want to read
!!  its content so we specify it in the routine we use.
!!
!!  By default, this routine will check that the header is a valid ETSF one, with the
!!  right Convention global attribute, as for the file_format global attribute.
!!
!!  We also check that the file is at least version 2.1 using the optional argument
!!  @version_min.
!! SOURCE
  call etsf_io_low_open_read(ncid, "read_write_sub_access.nc", lstat, &
       & error_data = error_data, version_min = 2.1)
  if (.not. lstat) then
     ! We use the default writing of the error to stderr.
     call etsf_io_low_error_handle(error_data)
     stop
  end if

!! NOTES
!!  We consider that the file contains the wavefunction description in plane waves.
!!  We thus read the dimensions first to allocate the program arrays.
!! SOURCE
  call etsf_io_dims_get(ncid, dims, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if

!! NOTES
!!  The coefficients of the wavefunctions may be splitted. We know this, thanks to
!!  the my_<something> attributes of the dims structure we have just read.
!!
!!  In the case of splitting, we allocate a new structure called split with
!!  etsf_io_split_allocate() and we read its contents with etsf_io_split_get(). In
!!  the case where the file contains no split informations, then all these routines
!!  will do nothing.
!!
!!  A split that has been allocated must be freed after use with etsf_io_split_free().
!!  Since the split informations are not relevent for the purpose of this tutorial
!!  we will free it just after having output some informations to the user.
!! SOURCE
  call etsf_io_split_allocate(split, dims)
  call etsf_io_split_get(ncid, split, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if
  ! We warn the user.
  write(*,"(A,L1)") " Split over kpoints     : ", associated(split%my_kpoints)
  write(*,"(A,L1)") " Split over spins       : ", associated(split%my_spins)
  write(*,"(A,L1)") " Split over states      : ", associated(split%my_states)
  write(*,"(A,L1)") " Split over coefficients: ", associated(split%my_coefficients)
  ! We don't use the split informations further so we free them.
  call etsf_io_split_free(split)

!! NOTES
!!  Before reading the coefficients of wavefunctions, we will get the definition
!!  of the basis set and the kpoints definitions.
!!
!!  This is done using the structure of types etsf_kpoints and etsf_basisdata and
!!  the etsf_io level etsf_io_kpoints_get() and etsf_io_basisdata_get(). As for the
!!  put routines, we associate the variables we want to read and only them.
!!
!!  Then we read the coefficients as all other variables, using the main group.
!! SOURCE
  ! The main program allocate memory for storage of the basis set.
  allocate(pw_coeff(dims%real_or_complex_coefficients, &
       & dims%max_number_of_coefficients, &
       & dims%max_number_of_states, &
       & dims%number_of_spins * &
       & dims%number_of_kpoints * &
       & dims%number_of_spinor_components))
  allocate(number_of_coefficients(dims%number_of_kpoints))
  allocate(red_coord_pw(dims%number_of_reduced_dimensions, &
       & dims%max_number_of_coefficients, dims%number_of_kpoints))
  allocate(red_coord_kpt(dims%number_of_reduced_dimensions, dims%number_of_kpoints))
  allocate(kpoint_weights(dims%number_of_kpoints))

  ! We set the associations.
  kpoints%reduced_coordinates_of_kpoints => red_coord_kpt
  kpoints%kpoint_weights => kpoint_weights
  basisdata%basis_set => basis
  basisdata%reduced_coordinates_of_plane_waves%data3D => red_coord_pw
  basisdata%number_of_coefficients => number_of_coefficients
  main%coefficients_of_wavefunctions%data4D => pw_coeff

  ! We call the get routines.
  call etsf_io_kpoints_get(ncid, kpoints, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if
  call etsf_io_basisdata_get(ncid, basisdata, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if
  call strip(basis)
  call etsf_io_main_get(ncid, main, lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if

!! NOTES
!!  We poll the file using an etsf_io_tools routine to know if the number of
!!  coefficients have been reduced using the time reversal symmetry at Gamma.
!! SOURCE
  call etsf_io_tools_get_time_reversal_symmetry(ncid, symmetry, &
       & lstat, error_data)
  if (.not. lstat) then
     call etsf_io_low_error_handle(error_data)
     stop
  end if

!! NOTES
!!  The following is just output on screen.
!! SOURCE
  ! We output the informations to the user.
  write(*,*)
  write(*,"(A,I0)") " Number of k points     : ", dims%number_of_kpoints
  write(*,*) "k point weights        : ", kpoints%kpoint_weights
  write(*,"(A)") " k point coordinates    : "
  do i = 1, dims%number_of_kpoints, 1
     write(*, "(3F10.5)") red_coord_kpt(:, i)
  end do
  write(*,*)
  write(*,"(A,A)") " Used basis set         : ", trim(basis)
  write(*,"(A,L1)") " Time reversal symmetry : ", symmetry
  write(*,"(A,I0)") " Max number of coeffs   : ", dims%max_number_of_coefficients
  do i = 1, dims%number_of_kpoints, 1
     write(*,*)
     write(*,"(A,I0)") " Informations at k point: ", i
     write(*,"(A,I0)") " Number of coefficients : ", number_of_coefficients(i)
     write(*,"(A)") " Coordinates of g vector: "
     do j = 1, min(dims%max_number_of_coefficients, 5), 1
        write(*, "(3I5,A,I2,A)") red_coord_pw(:, j, i), " (g vector ", j, ")"
     end do
     if (j < dims%max_number_of_coefficients) then
        write(*,*) "   ..."
     end if
     write(*,"(A)") " Coeffs of wavefunctions: "
     do k = 1, dims%max_number_of_states, 1
        write(*,"(A,I0)") " Band number            : ", k
        do j = 1, min(dims%max_number_of_coefficients, 5), 1
           write(*, "(2F12.5,A,I2,A)") pw_coeff(:, j, k, i), " (g vector ", j, ")"
        end do
        if (j < dims%max_number_of_coefficients) then
           write(*,*) "   ..."
        end if
     end do
  end do

  ! We deallocate everything
  deallocate(pw_coeff)
  deallocate(number_of_coefficients)
  deallocate(red_coord_pw)
  deallocate(kpoint_weights)
  deallocate(red_coord_kpt)

end program read_a_file
!!***
