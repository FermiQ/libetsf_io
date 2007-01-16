program tests_copy

  use etsf_io_low_level
  use etsf_io
  
  implicit none

  integer :: nArg, iargc
  character(len = 256) :: path
  
  nArg = iargc()
  if (nArg > 0) then
    call getarg(1, path)
  else
    write(path, "(A)") "."
  end if

  call test_copy_electrons(trim(path))

  
contains

  subroutine tests_read_status(name, lstat, error)
    character(len = *), intent(in)      :: name
    logical, intent(in)                 :: lstat
    type(etsf_io_low_error), intent(in) :: error
    
    if (lstat) then
      write(*, "(A,A,A,A)") "== ", name, repeat(" ", 68 - len(name)), "OK     =="
    else
      write(*, "(A,A,A,A)") "== ", name, repeat(" ", 68 - len(name)), "Failed =="
      call etsf_io_low_error_handle(error)
    end if
  end subroutine tests_read_status

  subroutine test_copy_electrons(path)
    character(len = *), intent(in) :: path
    integer :: ncid_to, ncid_from
    type(etsf_io_low_error) :: error
    type(etsf_split) :: split
    type(etsf_dims) :: dims
    character(len = *), parameter :: me = "test_copy_electrons"
    logical :: lstat

    write(*,*)
    write(*,*) "Testing etsf_io_electrons_copy()..."

    ! We open the file to read and to write.
    call etsf_io_low_open_read(ncid_from, path//"/test_split_electrons_part1.nc", &
         & lstat, error_data = error)
    if (.not. lstat) then
       write(*,*) "Abort, can't open file '", path, "/test_split_electrons_part1.nc'"
       call etsf_io_low_error_handle(error)
       return
    end if

    call etsf_io_low_open_create(ncid_to, "test_copy_electrons.nc", 2.1, lstat, error_data = error)
    if (.not. lstat) then
       write(*,*) "Abort, can't open file '", path, "/test_split_electrons_part1.nc'"
       call etsf_io_low_error_handle(error)
       return
    end if

    ! We read the dimensions from ncid_from
    call etsf_io_dims_get(ncid_from, dims, lstat, error)
    if (.not. lstat) then
       write(*,*) "Abort, read dimensions"
       call etsf_io_low_error_handle(error)
       return
    end if

    ! We allocate a split definition, from the dims, if required.
    call etsf_io_split_allocate(split, dims)

    ! We put the dimensions to ncid_to
    call etsf_io_dims_def(ncid_to, dims, lstat, error)
    if (.not. lstat) then
       write(*,*) "Abort, write dimensions"
       call etsf_io_low_error_handle(error)
       return
    end if

    ! We define the split arrays, if required.
!    call etsf_io_split_def(ncid_to, dims, lstat, error)
!    if (.not. lstat) then
!       write(*,*) "Abort, define the split defintion arrays."
!       call etsf_io_low_error_handle(error)
!       return
!    end if

    ! We define the electrons group.
    call etsf_io_electrons_def(ncid_to, lstat, error) !, split = split)
    if (.not. lstat) then
       write(*,*) "Abort, define electrons group"
       call etsf_io_low_error_handle(error)
       return
    end if

    ! We put the file in a data mode.
    call etsf_io_low_set_write_mode(ncid_to, lstat, error)
    if (.not. lstat) then
       write(*,*) "Abort, switch to data mode"
       call etsf_io_low_error_handle(error)
       return
    end if

    ! We read the split informations, if required
    call etsf_io_split_get(ncid_from, split, lstat, error)
    if (.not. lstat) then
       write(*,*) "Abort, read split informations"
       call etsf_io_low_error_handle(error)
       return
    end if

    ! We copy the split informations, if required.
!    call etsf_io_split_copy(ncid_to, ncid_from, dims, lstat, error)
!    if (.not. lstat) then
!       write(*,*) "Abort, copy split informations"
!       call etsf_io_low_error_handle(error)
!       return
!    end if

    ! We copy the variables
    call etsf_io_electrons_copy(ncid_to, ncid_from, dims, lstat, error, split = split)
    call tests_read_status("valid copy: copy all variables", lstat, error)

    ! We free the split definition
    call etsf_io_split_free(split)

    ! We close the two files.
    call etsf_io_low_close(ncid_from, lstat, error)
    if (.not. lstat) then
       write(*,*) "Abort, close the from file"
       call etsf_io_low_error_handle(error)
       return
    end if
    call etsf_io_low_close(ncid_to, lstat, error)
    if (.not. lstat) then
       write(*,*) "Abort, close the to file"
       call etsf_io_low_error_handle(error)
       return
    end if

    write(*,*)

  end subroutine test_copy_electrons

end program tests_copy
