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

  public :: etsf_io_file_merge

contains

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
    type(etsf_split), allocatable :: split(:)
    type(etsf_split) :: output_split
    type(etsf_dims), allocatable :: dims(:)
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
    allocate(dims(n_files), split(n_files))

    ! We read the different dimensions.
    etsf_group = etsf_grp_none
    etsf_main  = etsf_main_none
    do i_file = 1, n_files, 1
       ! We get the list of used groups and main variables
       call etsf_io_data_contents(trim(source_files(i_file)), dims(i_file), &
            & split(i_file), main, grp, lstat, error_data)
       if (.not. lstat) then
          do i = 1, i_file, 1
             call etsf_io_split_free(split(i))
          end do
          deallocate(dims, split)
          return
       end if
       etsf_group = ior(etsf_group, grp)
       etsf_main  = ior(etsf_main, main)
    end do

    ! We merge the dimensions, checking that all no my_something
    ! are equal and we create an output_split for all not complete
    ! dimensions after merge.
    output_dims = dims(1)
    ! Sum all my_something dimensions, to know if the merging is complete or not.
    do i_file = 2, n_files, 1
       call etsf_io_dims_merge(output_dims, dims(i_file), lstat, error_data)
       if (.not. lstat) then
          do i = 1, i_file, 1
             call etsf_io_split_free(split(i))
          end do
          deallocate(dims, split)
          return
       end if
    end do
    call etsf_io_split_allocate(output_split, output_dims)
    ! We create a new split definition with the split(i) values.
    do i_file = 1, n_files, 1
       call etsf_io_split_merge(output_split, split(i_file), lstat, error_data)
       if (.not. lstat) then
          do i = 1, i_file, 1
             call etsf_io_split_free(split(i))
          end do
          deallocate(dims, split)
          call etsf_io_split_free(output_split)
          return
       end if
    end do

    ! We create an output file and define all the variables and dimensions.
    call etsf_io_data_init(trim(dest_file), etsf_main, etsf_group, output_dims, &
         & "Merging files", "", lstat, error_data = error_data, &
         & split_definition = output_split) 
    if (.not. lstat) then
       do i = 1, n_files, 1
          call etsf_io_split_free(split(i))
       end do
       deallocate(dims, split)
       call etsf_io_split_free(output_split)
       return
    end if

    ! We copy all the data from read files to the new output file.
    do i_file = 1, n_files, 1
       call etsf_io_data_copy(trim(dest_file), trim(source_files(i_file)), &
            & dims(i_file), lstat, error_data, split(i_file))
       if (.not. lstat) then
          do i = 1, i_file, 1
             call etsf_io_split_free(split(i))
          end do
          deallocate(dims, split)
          call etsf_io_split_free(output_split)
          return
       end if
    end do

    do i = 1, n_files, 1
       call etsf_io_split_free(split(i))
    end do
    deallocate(dims, split)
    call etsf_io_split_free(output_split)
    lstat = .true.
  end subroutine etsf_io_file_merge
!!***

end module etsf_io_file
