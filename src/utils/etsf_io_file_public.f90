!!****m* etsf_io_file/etsf_io_file_merge
!! NAME
!!  etsf_io_file_merge
!!
!! FUNCTION
!!  This is a high level routine to merge several files into one single. The files
!!  to be merged should conform to the ETSF specification on splitted files. The
!!  given input files must not be a complete list to create a non-splitted file.
!!  In the case some arrays are still partial, the created output file is a splitted
!!  one again, gathering what was possible with respect to the given input files.
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! INPUTS
!! * dest_file = 
!!     the path to the file to be created. It must not already exist.
!! * source_files = 
!!     a list of path where input files can be found.
!! OUTPUT
!! * lstat = 
!!     return .true. if all the actions succeed, if not the status
!!     of the output file is undefined.
!! * error_data <type(etsf_io_low_error)> = 
!!     contains the details of the error is @lstat is false.
!!
!! SOURCE
  subroutine etsf_io_file_merge(dest_file, source_files, lstat, error_data)
    character(len = *), intent(in) :: dest_file
    character(len = 256), intent(in) :: source_files(:)
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(out) :: error_data

    ! Local variables
    integer :: ncid_to, n_files, i_file, i, ncid
    integer :: etsf_group, etsf_main, grp, main
    type(file_infos_type), allocatable :: infos_file(:)
    type(etsf_split) :: output_split
    type(etsf_dims) :: output_dims
    character(len = *), parameter :: me = "etsf_io_file_merge"

    lstat = .false.
    n_files = size(source_files)
    if (n_files <= 0) then
       call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
            & errmess = "argument 'source_files' has a wrong size.")
       return
    end if

    ! We allocate the dimension definitions.
    allocate(infos_file(n_files))

    !*************************************
    ! Read all definitions from all files.
    !*************************************
    ! We read the different dimensions.
    etsf_group = etsf_grp_none
    etsf_main  = etsf_main_none
    do i_file = 1, n_files, 1
       ! We copy the path.
       infos_file(i_file)%path = source_files(i_file)
       ! We get the list of used groups and main variables
       ! We also get the list of all variables in the files.
       call etsf_io_data_contents(trim(source_files(i_file)), &
            & infos_file(i_file)%dims, infos_file(i_file)%split, &
            & main, grp, lstat, error_data, &
            & vars_infos = infos_file(i_file)%var_list)
       if (.not. lstat) then
          call file_infos_free(infos_file, i_file)
          deallocate(infos_file)
          return
       end if
       etsf_group = ior(etsf_group, grp)
       etsf_main  = ior(etsf_main, main)
    end do


    !*********************************************
    ! Merge the dimensions and split informations.
    !*********************************************
    ! We merge the dimensions, checking that all no my_something
    ! are equal and we create an output_split for all not complete
    ! dimensions after merge.
    output_dims = infos_file(1)%dims
    ! Sum all my_something dimensions, to know if the merging is complete or not.
    do i_file = 2, n_files, 1
       call etsf_io_dims_merge(output_dims, infos_file(i_file)%dims, &
            & lstat, error_data)
       if (.not. lstat) then
          call file_infos_free(infos_file, n_files)
          deallocate(infos_file)
          return
       end if
    end do
    call etsf_io_split_allocate(output_split, output_dims)
    ! We create a new split definition with the split(i) values.
    do i_file = 1, n_files, 1
       call etsf_io_split_merge(output_split, infos_file(i_file)%split, &
            & lstat, error_data)
       if (.not. lstat) goto 1000
    end do


    !*****************************************************
    ! Define all ETSF (non main) variables and dimensions.
    !*****************************************************
    ! We create an output file and define all the variables and dimensions.
    ! All defined dimensions and variables are related to ETSF only,
    ! all other variables and dimensions are ignored.
    ! The main group is also ignored at that time to allow to add new
    ! non ETSF variables.
    if (etsf_main /= etsf_main_none) then
       etsf_group = etsf_group - etsf_grp_main
    end if
    call etsf_io_data_init(trim(dest_file), etsf_main_none, etsf_group, output_dims, &
         & "Merging files", "", lstat, error_data = error_data, &
         & split_definition = output_split) 
    if (.not. lstat) goto 1000


    !******************************************************
    ! Treat non-ETSF part, define variables and dimensions.
    !******************************************************
    ! We reopen the destination file to add non ETSF elements
    ! and to later add the main group.
    call etsf_io_low_open_modify(ncid_to, trim(dest_file), lstat, &
         & error_data = error_data)
    if (.not. lstat) goto 1000

    ! We define all dimensions and variables that are non-part of ETSF.
    call non_etsf_init(ncid_to, infos_file, lstat, error_data)
    if (.not. lstat) goto 1000

    ! We add the main group.
    if (etsf_main /= etsf_main_none) then
       call etsf_io_main_def(ncid_to, etsf_main, lstat, error_data, &
            & split = output_split)
       if (.not. lstat) goto 1000
    end if

    ! We close the file after the definitions.
    call etsf_io_low_close(ncid_to, lstat, error_data = error_data)
    if (.not. lstat) goto 1000


    !*************************
    ! Copy all ETSF variables.
    !*************************
    ! We copy all the data from read files to the new output file.
    do i_file = 1, n_files, 1
       call etsf_io_data_copy(trim(dest_file), trim(source_files(i_file)), &
            & infos_file(i_file)%dims, lstat, error_data, infos_file(i_file)%split)
       if (.not. lstat) goto 1000
    end do


    !*****************************
    ! Copy all non-ETSF variables.
    !*****************************
    ! We reopen the destination file to copy non ETSF values.
    call etsf_io_low_open_modify(ncid_to, trim(dest_file), lstat, &
         & error_data = error_data)
    if (.not. lstat) goto 1000

    call etsf_io_low_set_write_mode(ncid_to, lstat, error_data = error_data)
    if (.not. lstat) goto 1000

    ! We copy all variables that are non-part of ETSF.
    call non_etsf_copy(ncid_to, infos_file, lstat, error_data)
    if (.not. lstat) goto 1000

    ! We close the file after the copy.
    call etsf_io_low_close(ncid_to, lstat, error_data = error_data)
    if (.not. lstat) goto 1000


    ! If we arrived there, then everything went right.
    lstat = .true.

    ! Last deallocations and/or error freeing before return.
    1000 continue
    call file_infos_free(infos_file, n_files)
    deallocate(infos_file)
    call etsf_io_split_free(output_split)
  end subroutine etsf_io_file_merge
!!***

!!****m* etsf_io_file/etsf_io_file_check
!! NAME
!!  etsf_io_file_check
!!
!! FUNCTION
!!  This is a high level routine to check that a file is valid to
!!  the specifications. This validity is done on presence of required
!!  variables and on conform variable definition. The presence of attributes
!!  when required is also done.
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! INPUTS
!! * file_name = 
!!     a list of path where input files can be found.
!! * file_flags =
!!     a serie of flags to check the file on. These flags are defined in the
!!     module etsf_io (see ETSF_IO_VALIDITY_FLAGS). To use several flags,
!!     simply add each of them.
!! OUTPUT
!! * lstat = 
!!     return .true. if the file is valid.
!! * error_data <type(etsf_io_low_error)> = 
!!     contains the details of the error is @lstat is false.
!!
!! SOURCE
  subroutine etsf_io_file_check(file_name, file_flags, lstat, error_data)
    character(len = *), intent(in)       :: file_name
    integer, intent(in)                  :: file_flags
    logical, intent(out)                 :: lstat
    type(etsf_io_low_error), intent(out) :: error_data

    integer :: read_flags
    type(etsf_io_low_error), dimension(etsf_nspecs_data) :: errors
    integer :: i
    
    call etsf_io_file_contents(read_flags, errors, file_name, lstat, error_data)
    if (.not. lstat) return
    
    do i = 1, etsf_nspecs_data
       if (iand(file_flags, 2 ** (i - 1)) /= 0 .and. &
            & iand(read_flags, 2 ** (i - 1)) == 0) then
          lstat = .false.
          error_data = errors(i)
          return
       end if
    end do
    lstat = .true.
  end subroutine etsf_io_file_check
!!***
