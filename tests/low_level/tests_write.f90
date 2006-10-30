program tests_write

  use etsf_io_low_level
  
  implicit none

  integer :: nArg
  character(len = 256) :: path
  
  nArg = iargc()
  if (nArg > 0) then
    call getarg(1, path)
  else
    write(path, "(A)") "."
  end if

  call tests_write_create(trim(path))
  call tests_write_modify(trim(path))
  
contains

  subroutine tests_write_status(name, lstat, error)
    character(len = *), intent(in)      :: name
    logical, intent(in)                 :: lstat
    type(etsf_io_low_error), intent(in) :: error
    
    if (lstat) then
      write(*, "(A,A,A,A)") "== ", name, repeat(" ", 68 - len(name)), "OK     =="
    else
      write(*, "(A,A,A,A)") "== ", name, repeat(" ", 68 - len(name)), "Failed =="
      call etsf_io_low_error_handle(error)
    end if
  end subroutine tests_write_status

  subroutine tests_write_create(path)
    character(len = *), intent(in) :: path
    integer :: ncid, s
    logical :: lstat
    type(etsf_io_low_error) :: error
    character(len = 80) :: title
    character(len = 1024) :: history
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_open_create()..."
    ! We test an IO error, trying to write in a hardly existing place on the disk.
    call etsf_io_low_open_create(ncid, "/pouet/pouet.nc", 1.3, lstat, error_data = error)
    call tests_write_status("argument filename: no write access", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_IO .and. error%target_type_id == ERROR_TYPE_OWR), error)
    
    ! We create the file with a minimal header, we will test it later.
    call etsf_io_low_open_create(ncid, "open_create_t01.nc", 1.3, lstat, error_data = error)
    call tests_write_status("argument filename: creation, minimal header", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if
    
    ! We test the opening of this minimal file.
    call etsf_io_low_open_read(ncid, "open_create_t01.nc", lstat, error_data = error)
    call tests_write_status(" | opening test", lstat, error)
    
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if
    
    ! We create a new file with a complete header, we will test it later.
    call etsf_io_low_open_create(ncid, "open_create_t02.nc", 2.0, lstat, &
                               & title = "Testing header", history = "Testing suite", &
                               & error_data = error)
    call tests_write_status("argument filename: creation, full header", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    ! We test the opening of this minimal file.
    call etsf_io_low_open_read(ncid, "open_create_t02.nc", lstat, error_data = error)
    call tests_write_status(" | opening test", lstat, error)

    ! We test the two fields title and history.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "title", 80, title, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading title", lstat, error)
    if (trim(title) /= "Testing header") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      call tests_write_status(" | value title", lstat, error)
    end if
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "history", 1024, history, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading history", lstat, error)
    if (trim(history) /= "Testing suite") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      call tests_write_status(" | value history", lstat, error)
    end if
    
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if
    
    write(*,*) 
  end subroutine tests_write_create

  subroutine tests_write_modify(path)
    character(len = *), intent(in) :: path
    integer :: ncid, s
    logical :: lstat
    type(etsf_io_low_error) :: error
    character(len = 80) :: title
    character(len = 1024) :: history
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_open_modify()..."
    ! We test an IO error, trying to modify a none existing file.
    call etsf_io_low_open_modify(ncid, "pouet.nc", lstat, error_data = error)
    call tests_write_status("argument filename: wrong filename", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_IO .and. error%target_type_id == ERROR_TYPE_OWR), error)
      
    ! We try to open a no valid file.
    call etsf_io_low_open_modify(ncid, "tests_write", lstat, error_data = error)
    call tests_write_status("argument filename: wrong file type", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_IO .and. error%target_type_id == ERROR_TYPE_OWR), error)
    
    ! We try to open a file without header.
    call etsf_io_low_open_modify(ncid, path//"/open_read_t01.nc", lstat, error_data = error)
    call tests_write_status("argument filename: NetCDF without header", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ .and. error%target_type_id == ERROR_TYPE_ATT), error)

    ! We open a file without header modification.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, error_data = error)
    call tests_write_status("argument filename: NetCDF without header modification", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    ! We open a file with header modification: title creation.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, &
                               & title = "Testing title" , error_data = error)
    call tests_write_status("argument filename: NetCDF with header modification", lstat, error)
    ! We test the title.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "title", 80, title, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading title", lstat, error)
    if (trim(title) /= "Testing title") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      call tests_write_status(" | value title", lstat, error)
    end if
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    ! We open a file with header modification: title modification.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, &
                               & title = "Modifying title" , error_data = error)
    call tests_write_status("argument filename: NetCDF with header modification", lstat, error)
    ! We test the title.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "title", 80, title, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading title", lstat, error)
    if (trim(title) /= "Modifying title") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      call tests_write_status(" | value title", lstat, error)
    end if
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    ! We open a file with header modification: history creation.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, &
                               & history = "Testing history" , error_data = error)
    call tests_write_status("argument filename: NetCDF with header modification", lstat, error)
    ! We test the title.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "history", 1024, history, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading history", lstat, error)
    if (trim(title) /= "Testing history") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      call tests_write_status(" | value history", lstat, error)
    end if
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    ! We open a file with header modification: history appending.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, &
                               & history = "Modifying history" , error_data = error)
    call tests_write_status("argument filename: NetCDF with header modification", lstat, error)
    ! We test the title.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "history", 1024, history, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading history", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if
  end subroutine tests_write_modify

end program tests_write
