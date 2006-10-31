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
  call tests_write_dim(trim(path))
  call tests_def_var(trim(path))
  
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
      lstat = .false.
    end if
    call tests_write_status(" | value title", lstat, error)
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "history", 1024, history, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading history", lstat, error)
    if (trim(history) /= "Testing suite") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | value history", lstat, error)
    
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
    call tests_write_status("argument filename: NetCDF with title creation", lstat, error)
    ! We test the title.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "title", 80, title, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading title", lstat, error)
    if (trim(title) /= "Testing title") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | value title", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    ! We open a file with header modification: title modification.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, &
                               & title = "Modifying title" , error_data = error)
    call tests_write_status("argument filename: NetCDF with title modification", lstat, error)
    ! We test the title.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "title", 80, title, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading title", lstat, error)
    if (trim(title) /= "Modifying title") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | value title", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    ! We open a file with header modification: history creation.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, &
                               & history = "Testing history" , error_data = error)
    call tests_write_status("argument filename: NetCDF with history creation", lstat, error)
    ! We test the title.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "history", 1024, history, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading history", lstat, error)
    if (trim(history) /= "Testing history") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | value history", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    ! We open a file with header modification: history appending.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, &
                               & history = "Modifying history" , error_data = error)
    call tests_write_status("argument filename: NetCDF with history updating", lstat, error)
    ! We test the title.
    call etsf_io_low_read_att(ncid, NF90_GLOBAL, "history", 1024, history, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading history", lstat, error)
    if (trim(history) /= "Testing history"//char(10)//"Modifying history") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value: '"//trim(history)//"'"
      lstat = .false.
    end if
    call tests_write_status(" | value history", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if
    
    write(*,*) 
  end subroutine tests_write_modify

  subroutine tests_write_dim(path)
    character(len = *), intent(in) :: path

    logical :: lstat
    integer :: ncid, value
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_write_dim()..."
    ! We test an IO error, trying to write a dim in a none existing file.
    call etsf_io_low_write_dim(0, "pouet", 5, lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_DEF .and. error%target_type_id == ERROR_TYPE_DIM), error)
    
    ! We open a file to write in.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, error_data = error)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    ! We test the writing action
    call etsf_io_low_write_dim(ncid, "number_of_atoms", 4, lstat, error_data = error)
    call tests_write_status("argument dimname: write a new value", lstat, error)
    ! We test we can read and fetch the right value
    call etsf_io_low_read_dim(ncid, "number_of_atoms", value, lstat, error_data = error)
    call tests_write_status(" | reading dimension", lstat, error)
    if (value /= 4) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%target_name = "number_of_atoms"
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking value", lstat, error)
    
    ! We test the writing action with a bad value
    call etsf_io_low_write_dim(ncid, "character_string_length", -80, lstat, error_data = error)
    call tests_write_status("argument value: wrong negative value", (.not. lstat), error)

    ! We test the over-writing action
    call etsf_io_low_write_dim(ncid, "character_string_length", 80, lstat, error_data = error)
    if (.not. lstat) then
      write(*,*) "Abort, can't add a dimension"
      return
    end if
    call etsf_io_low_write_dim(ncid, "character_string_length", 1, lstat, error_data = error)
    call tests_write_status("argument dimname: overwriting (should fail)", (.not. lstat), error)
    ! We test we can read and fetch the right value
    call etsf_io_low_read_dim(ncid, "character_string_length", value, lstat, error_data = error)
    call tests_write_status(" | reading dimension", lstat, error)
    if (value /= 80) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking value", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if
    
    write(*,*) 
  end subroutine tests_write_dim


  subroutine tests_def_var(path)
    character(len = *), intent(in) :: path

    logical :: lstat
    integer :: ncid, value, ncvarid, vardims(1)
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_def_var()..."
    ! We test an IO error, trying to write a dim in a none existing file.
    call etsf_io_low_def_var(0, "pouet", NF90_INT, lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_DEF .and. error%target_type_id == ERROR_TYPE_VAR), error)
      
    ! We open a file to write in.
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat, error_data = error)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    ! We add a single value as variable, but wrong type
    call etsf_io_low_def_var(ncid, "number_of_electrons", -2, lstat, error_data = error)
    call tests_write_status("single value: wrong type", (.not. lstat), error)
    ! We add a single variable
    call etsf_io_low_def_var(ncid, "number_of_electrons", NF90_INT, lstat, error_data = error)
    call tests_write_status("single value: adding a new variable", lstat, error)
    ! We add single variable, but overwriting is not allowed.
    call etsf_io_low_def_var(ncid, "number_of_electrons", NF90_DOUBLE, lstat, error_data = error)
    call tests_write_status("single value: overwriting (should fail)", (.not. lstat), error)
    ! We check the definition.
    call etsf_io_low_check_var(ncid, ncvarid, "number_of_electrons", NF90_INT, &
                             & (/ 0 /), 0, lstat, error_data = error)
    call tests_write_status("single value: check definition", lstat, error)

    ! We add an array as variable, but wrong type
    call etsf_io_low_def_var(ncid, "atom_species", -2, lstat, error_data = error)
    call tests_write_status("1D array: wrong type", (.not. lstat), error)
    ! We add a single variable, but unknown dimension
    call etsf_io_low_def_var(ncid, "atom_species", NF90_INT, (/ "pouet" /), &
                           & lstat, error_data = error)
    call tests_write_status("1D array: wrong dimension", (.not. lstat), error)
    ! We add a single variable
    call etsf_io_low_def_var(ncid, "atom_species", NF90_INT, (/ "number_of_atoms" /), &
                           & lstat, error_data = error)
    call tests_write_status("1D array: adding a new variable", lstat, error)
    ! We add single variable, but overwriting is not allowed.
    call etsf_io_low_def_var(ncid, "atom_species", NF90_INT, lstat, error_data = error)
    call tests_write_status("1D array: overwriting (should fail)", (.not. lstat), error)
    ! We check the definition.
    call etsf_io_low_check_var(ncid, ncvarid, "atom_species", NF90_INT, &
                             & (/ 4 /), 1, lstat, error_data = error)
    call tests_write_status("1D array: check definition", lstat, error)

    ! We add a string as variable
    call etsf_io_low_def_var(ncid, "exchange_functional", NF90_CHAR, &
                           & (/ "character_string_length" /), lstat, error_data = error)
    call tests_write_status("string: adding a new variable", lstat, error)
    ! We check the definition.
    call etsf_io_low_check_var(ncid, ncvarid, "exchange_functional", NF90_CHAR, &
                             & (/ 80 /), 1, lstat, error_data = error)
    call tests_write_status("string: check definition", lstat, error)

    call etsf_io_low_write_dim(ncid, "number_of_reduced_dimensions", 3, lstat, error_data = error)
    ! We add a 2D array as variable
    call etsf_io_low_def_var(ncid, "reduced_atom_positions", NF90_DOUBLE, &
                           & (/ "number_of_reduced_dimensions", "number_of_atoms" /), &
                           & lstat, error_data = error)
    call tests_write_status("2D array: adding a new variable", lstat, error)
    ! We check the definition.
    call etsf_io_low_check_var(ncid, ncvarid, "reduced_atom_positions", NF90_DOUBLE, &
                             & (/ 3, 4 /), 2, lstat, error_data = error)
    call tests_write_status("2D array: check definition", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if
    
    write(*,*) 
  end subroutine tests_def_var
end program tests_write
