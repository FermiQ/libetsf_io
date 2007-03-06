!!****h* utils/etsf_io_file
!! NAME
!!  etsf_io_file
!!
!! FUNCTION
!!  This module contains different high level routines to access ETSF files. It
!!  actually contains:
!!  * etsf_io_file_merge(): a routine to read several files and merge their data
!!                          into a single output file.
!!
!! COPYRIGHT
!!  Copyright (C) 2006
!!  This file is distributed under the terms of the
!!  GNU General Public License, see ~abinit/COPYING
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!***
module etsf_io_file

  use etsf_io

  implicit none

  private

  !* This type is a private type to store informations about a file.
  !* These informations are the dims values, the split definitions and
  !* the list of variable definitions.
  type file_infos_type
     !* The path to the file.
     character(len = 256) :: path
     !* The ETSF dimensions of the file (including names and values).
     type(etsf_dims)      :: dims
     !* The ETSF split definitions for the file (allocated arrays).
     type(etsf_split)     :: split
     !* The comprehensive list of variables of the file (with
     !* their dimension definitions, names...).
     type(etsf_vars)      :: var_list
  end type file_infos_type

  public :: etsf_io_file_merge

contains

  !* This routine free the nsize first element of the array
  !* file_infos.
  subroutine file_infos_free(file_infos, n_size)
    type(file_infos_type), intent(inout) :: file_infos(:)
    integer, intent(in) :: n_size

    integer :: i_file

    if (n_size > size(file_infos)) then
       write(0, *) "   *** ETSF I/O Internal error ***"
       write(0, *) "   file_infos_free n_size out of range: ", n_size
       return
    end if

    do i_file = 1, n_size, 1
       call etsf_io_split_free(file_infos(i_file)%split)
       call etsf_io_vars_free(file_infos(i_file)%var_list)
    end do
  end subroutine file_infos_free

  !* This routine is a basic implementation of a defining merge for
  !* non ETSF variables. Given a list of variables and their definitions
  !* the dimensions are defined variables per variables and then the variables
  !* themselves are added.
  subroutine non_etsf_init(ncid, infos_file, lstat, error_data)
    integer, intent(in) :: ncid
    type(file_infos_type), intent(in) :: infos_file(:)
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(out) :: error_data

    character(len=*),parameter :: me = 'non_etsf_init'
    integer :: i_file, i_var, i_dim
    integer :: dimvalue
    type(etsf_io_low_var_infos) :: infos_var

    ! In a merge action, all variables should be the same in the different
    ! files, then will only define dimensions and variables from the first
    ! element of array infos_file. We only check that dimensions and variable
    ! exist for the other elements.
    lstat = .false.

    i_file = 1
    ! For each file, we read the list of variables
    do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
       ! For each non-ETSF variable, we read the list of dimensions
       if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
            & .not. infos_file(i_file)%var_list%split(i_var)) then
          do i_dim = 1, infos_file(i_file)%var_list%parent(i_var)%ncshape, 1
             ! For each dimension, we write it to the destination file.
             call etsf_io_low_write_dim(ncid, &
                  & infos_file(i_file)%var_list%parent(i_var)%ncdimnames(i_dim), &
                  & infos_file(i_file)%var_list%parent(i_var)%ncdims(i_dim), &
                  & lstat, error_data = error_data)
             if (.not.lstat) then
                return
             end if
          end do
       end if
    end do

    ! Now, we define the variables.
    do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
       if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
            & .not. infos_file(i_file)%var_list%split(i_var)) then
          if (infos_file(i_file)%var_list%parent(i_var)%ncshape > 0) then
             call etsf_io_low_def_var(&
                  & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                  & infos_file(i_file)%var_list%parent(i_var)%nctype, &
                  & infos_file(i_file)%var_list%parent(i_var)%ncdimnames, lstat, &
                  & error_data = error_data)
             if (.not.lstat) return
          else
             call etsf_io_low_def_var( &
                  & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                  & infos_file(i_file)%var_list%parent(i_var)%nctype, &
                  & lstat, error_data = error_data)
             if (.not.lstat) return
          end if
       end if
    end do

    ! Now we check dimensions and variables for all other elements of input array
    ! infos_file.
    do i_file = 2, size(infos_file), 1
       do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
          if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
               & .not. infos_file(i_file)%var_list%split(i_var)) then
             ! We check that dimensions of this variable is in the destination
             ! file.
             do i_dim = 1, infos_file(i_file)%var_list%parent(i_var)%ncshape, 1
                call etsf_io_low_read_dim(ncid, &
                     & trim(infos_file(i_file)%var_list%parent(i_var)%ncdimnames(i_dim)), &
                     & dimvalue, lstat, error_data = error_data)
                if (.not. lstat .or. dimvalue /= &
                     & infos_file(i_file)%var_list%parent(i_var)%ncdims(i_dim)) then
                   call etsf_io_low_error_handle(error_data)
                   call etsf_io_low_error_set( &
                        & error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                        & errmess = "dimension '"// &
                        & trim(infos_file(i_file)%var_list%parent(i_var)%ncdimnames(i_dim)) &
                        & //"' is not present in all files or has different values.")
                   lstat = .false.
                   return
                end if
             end do
             ! We check variables
             call etsf_io_low_read_var_infos( &
                  & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                  & infos_var, lstat, error_data = error_data)
             if (.not. lstat .or. &
                  & infos_var%nctype /= infos_file(1)%var_list%parent(i_var)%nctype .or. &
                  & infos_var%ncshape /= infos_file(1)%var_list%parent(i_var)%ncshape) then
                call etsf_io_low_error_set( &
                     & error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                     & errmess = "variable '"// &
                     & trim(infos_file(i_file)%var_list%parent(i_var)%name) &
                     & //"' is not present in all files or has different definitions.")
                lstat = .false.
                return
             end if
          end if
       end do
    end do
    lstat = .true.
  end subroutine non_etsf_init

  !* Basic implementation of a copy routine for all non-ETSF variables.
  !* Values are only copied from the first file, and no check is done
  !* regarding to other files. This is surely crude and may be upgarded
  !* later.
  subroutine non_etsf_copy(ncid, infos_file, lstat, error_data)
    integer, intent(in) :: ncid
    type(file_infos_type), intent(in) :: infos_file(:)
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(out) :: error_data

    integer, allocatable               :: integer_data(:)
    real, allocatable                  :: real_data(:)
    double precision, allocatable      :: double_data(:)
    character(len = 1024), allocatable :: string_data(:)
    
    character(len=*),parameter :: me = 'non_etsf_copy'
    integer :: ncid_from
    integer :: i_file, i_var
    integer :: n_size
    logical :: lstat_

    i_file = 1
    call etsf_io_low_open_read(ncid_from, trim(infos_file(i_file)%path), lstat, &
         & error_data = error_data)
    if (.not. lstat) return

    do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
       if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
            & .not. infos_file(i_file)%var_list%split(i_var)) then
          ! Read the values
          if (infos_file(i_file)%var_list%parent(i_var)%ncshape > 0) then
             n_size = product(infos_file(i_file)%var_list%parent(i_var)%ncdims( &
                  & 1:infos_file(i_file)%var_list%parent(i_var)%ncshape))
          else
             n_size = 1
          end if
          select case (infos_file(i_file)%var_list%parent(i_var)%nctype)
             ! Case integer values.
          case (etsf_io_low_integer)
             allocate(integer_data(n_size))
             call etsf_io_low_read_var( &
                  & ncid_from, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                  & integer_data, lstat, error_data = error_data)
             if (lstat) then
                call etsf_io_low_write_var( &
                     & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                     & integer_data, lstat, error_data = error_data)
             end if
             ! Case real values.
          case (etsf_io_low_real)
             write(0, *) "   *** ETSF I/O Internal error ***"
             write(0, *) "   real variables not implemented, using double instead."
             allocate(double_data(n_size))
             call etsf_io_low_read_var( &
                  & ncid_from, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                  & double_data, lstat, error_data = error_data)
             if (lstat) then
                call etsf_io_low_write_var( &
                     & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                     & double_data, lstat, error_data = error_data)
             end if
             ! Case double values.
          case (etsf_io_low_double)
             allocate(double_data(n_size))
             call etsf_io_low_read_var( &
                  & ncid_from, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                  & double_data, lstat, error_data = error_data)
             if (lstat) then
                call etsf_io_low_write_var( &
                     & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                     & double_data, lstat, error_data = error_data)
             end if
             ! Case string values.
          case (etsf_io_low_character)
             if (infos_file(i_file)%var_list%parent(i_var)%ncshape == 0) then
                call etsf_io_low_error_set( &
                     & error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                     & tgtname = trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                     & errmess = "character variables must be arrays.")
                lstat = .false.
                exit
             end if

             n_size = n_size / infos_file(i_file)%var_list%parent(i_var)%ncdims(1)
             allocate(string_data(n_size))
             call etsf_io_low_read_var( &
                  & ncid_from, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                  & string_data, infos_file(i_file)%var_list%parent(i_var)%ncdims(1), &
                  & lstat, error_data = error_data)
             if (lstat) then
                call etsf_io_low_write_var( &
                     & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                     & string_data, infos_file(i_file)%var_list%parent(i_var)%ncdims(1), &
                     & lstat, error_data = error_data)
             end if
          end select

          ! Deallocate all memory
          if (allocated(integer_data)) then
             deallocate(integer_data)
          end if
          if (allocated(real_data)) then
             deallocate(real_data)
          end if
          if (allocated(double_data)) then
             deallocate(double_data)
          end if
          if (allocated(string_data)) then
             deallocate(string_data)
          end if
          if (.not. lstat) exit
       end if
    end do

    ! Notice: we ignore close errors if any.
    call etsf_io_low_close(ncid_from, lstat_)
  end subroutine non_etsf_copy

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
!!  Copyright (C) 2006
!!  This file is distributed under the terms of the
!!  GNU General Public License, see ~abinit/COPYING
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

end module etsf_io_file
