program tests_write

  use etsf_io_low_level
  
  implicit none

  integer :: nArg, iargc
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
  call tests_write_att_integer(trim(path))
  call tests_write_att_real(trim(path))
  call tests_write_att_double(trim(path))
  call tests_write_att_character(trim(path))
  call tests_def_var(trim(path))
  call tests_write_var_integer(trim(path))
  call tests_write_var_double(trim(path))
  call tests_write_var_character(trim(path))
  
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
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "title", 80, title, &
                            & lstat, error_data = error)
    call tests_write_status(" | reading title", lstat, error)
    if (trim(title) /= "Testing header") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | value title", lstat, error)
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "history", 1024, history, &
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
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "title", 80, title, &
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
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "title", 80, title, &
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
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "history", 1024, history, &
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
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "history", 1024, history, &
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
    call etsf_io_low_write_dim(ncid, "character_string_length", 80, lstat, error_data = error)
    call tests_write_status("argument dimname: overwriting (same value)", lstat, error)
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

  subroutine tests_write_att_integer(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid, var(3)
    logical :: lstat
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_write_att_integer()..."
    call etsf_io_low_write_att(0, etsf_io_low_global_att, "test_att_integer", &
                             & 2, lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat), error)
    
    call etsf_io_low_open_modify(ncid, "open_create_t02.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_write_att(ncid, etsf_io_low_global_att, "test_att_integer", &
                             & 2, lstat, error_data = error)
    call tests_write_status("argument att: good value (0D)", lstat, error)
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "test_att_integer", &
                            & var(1), lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 2)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_write_att(ncid, etsf_io_low_global_att, "test_att_integer_1D", &
                             & (/ 2, 3, 4 /), lstat, error_data = error)
    call tests_write_status("argument att: good value (1D)", lstat, error)
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "test_att_integer_1D", &
                            & 3, var, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 2 .and. var(2) == 3 .and. var(3) == 4)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    write(*,*) 
  end subroutine tests_write_att_integer

  subroutine tests_write_att_real(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid
    real :: var(3)
    logical :: lstat
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_write_att_real()..."
    call etsf_io_low_write_att(0, etsf_io_low_global_att, "test_att_real", &
                             & 2., lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat), error)
    
    call etsf_io_low_open_modify(ncid, "open_create_t02.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_write_att(ncid, etsf_io_low_global_att, "test_att_real", &
                             & 2., lstat, error_data = error)
    call tests_write_status("argument att: good value (0D)", lstat, error)
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "test_att_real", &
                            & var(1), lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 2.)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_write_att(ncid, etsf_io_low_global_att, "test_att_real_1D", &
                             & (/ 2., 3., 4. /), lstat, error_data = error)
    call tests_write_status("argument att: good value (1D)", lstat, error)
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "test_att_real_1D", &
                            & 3, var, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 2. .and. var(2) == 3. .and. var(3) == 4.)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    write(*,*) 
  end subroutine tests_write_att_real

  subroutine tests_write_att_double(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid
    double precision :: var(3)
    logical :: lstat
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_write_att_double()..."
    call etsf_io_low_write_att(0, etsf_io_low_global_att, "test_att_double", &
                             & 2.d0, lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat), error)
    
    call etsf_io_low_open_modify(ncid, "open_create_t02.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_write_att(ncid, etsf_io_low_global_att, "test_att_double", &
                             & 2.d0, lstat, error_data = error)
    call tests_write_status("argument att: good value (0D)", lstat, error)
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "test_att_double", &
                            & var(1), lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 2.d0)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_write_att(ncid, etsf_io_low_global_att, "test_att_double_1D", &
                             & (/ 2.d0, 3.d0, 4.d0 /), lstat, error_data = error)
    call tests_write_status("argument att: good value (1D)", lstat, error)
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "test_att_double_1D", &
                            & 3, var, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 2.d0 .and. var(2) == 3.d0 .and. var(3) == 4.d0)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    write(*,*) 
  end subroutine tests_write_att_double

  subroutine tests_write_att_character(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid
    character(len = 80) :: var
    logical :: lstat
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_write_att_character()..."
    call etsf_io_low_write_att(0, etsf_io_low_global_att, "test_att_character", &
                             & "toto", lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat), error)
    
    call etsf_io_low_open_modify(ncid, "open_create_t02.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_write_att(ncid, etsf_io_low_global_att, "test_att_character", &
                             & "toto", lstat, error_data = error)
    call tests_write_status("argument att: good value", lstat, error)
    call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "test_att_character", &
                            & 80, var, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (trim(var) == "toto")) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    write(*,*) 
  end subroutine tests_write_att_character


  subroutine tests_def_var(path)
    character(len = *), intent(in) :: path

    logical :: lstat
    integer :: ncid, value, ncvarid, vardims(1)
    type(etsf_io_low_error) :: error
    type(etsf_io_low_var_infos) :: infos
    
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
    ! We add single variable, overwriting, with the same definition.
    call etsf_io_low_def_var(ncid, "number_of_electrons", NF90_INT, lstat, error_data = error)
    call tests_write_status("single value: overwriting (matching definition)", lstat, error)
    ! We check the definition.
    call etsf_io_low_read_var_infos(ncid, "number_of_electrons", infos, lstat, error_data = error)
    call tests_write_status("single value: read definition", lstat, error)
    if (.not. (infos%nctype == etsf_io_low_integer .and. infos%ncshape == 0)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_VAR
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | check definition", lstat, error)
    

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
    ! We add single variable, overwriting, with the same definition.
    call etsf_io_low_def_var(ncid, "atom_species", NF90_INT, (/ "number_of_atoms" /), &
                           & lstat, error_data = error)
    call tests_write_status("1D array: overwriting (matching definition)", lstat, error)
    ! We check the definition.
    call etsf_io_low_read_var_infos(ncid, "atom_species", infos, lstat, error_data = error)
    call tests_write_status("1D array: read definition", lstat, error)
    if (.not. (infos%nctype == etsf_io_low_integer .and. &
             & infos%ncshape == 1 .and. infos%ncdims(1) == 4)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_VAR
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | check definition", lstat, error)

    ! We add a 2D integer array for future testing.
    call etsf_io_low_def_var(ncid, "test_integer_2d", NF90_INT, &
                           & (/ "number_of_atoms", "number_of_atoms" /), &
                           & lstat, error_data = error)
    call tests_write_status("2D array: adding a new variable", lstat, error)

    ! We add a string as variable
    call etsf_io_low_def_var(ncid, "exchange_functional", NF90_CHAR, &
                           & (/ "character_string_length" /), lstat, error_data = error)
    call tests_write_status("string: adding a new variable", lstat, error)
    ! We check the definition.
    call etsf_io_low_read_var_infos(ncid, "exchange_functional", infos, lstat, error_data = error)
    call tests_write_status("string: read definition", lstat, error)
    if (.not. (infos%nctype == etsf_io_low_character .and. &
             & infos%ncshape == 1 .and. infos%ncdims(1) == 80)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_VAR
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | check definition", lstat, error)

    call etsf_io_low_write_dim(ncid, "number_of_reduced_dimensions", 3, lstat, error_data = error)
    ! We add a 2D array as variable
    call etsf_io_low_def_var(ncid, "reduced_atom_positions", NF90_DOUBLE, &
                           & (/ pad("number_of_reduced_dimensions"), pad("number_of_atoms") /), &
                           & lstat, error_data = error)
    call tests_write_status("2D array: adding a new variable", lstat, error)
    ! We check the definition.
    call etsf_io_low_read_var_infos(ncid, "reduced_atom_positions", infos, lstat, error_data = error)
    call tests_write_status("2D array: read definition", lstat, error)
    if (.not. (infos%nctype == etsf_io_low_double .and. &
             & infos%ncshape == 2 .and. infos%ncdims(1) == 3 .and. &
             & infos%ncdims(2) == 4)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_VAR
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | check definition", lstat, error)

    ! We add a 4D array for future testing.
    call etsf_io_low_def_var(ncid, "density", NF90_DOUBLE, &
                           & (/ "number_of_reduced_dimensions", "number_of_reduced_dimensions", &
                              & "number_of_reduced_dimensions", "number_of_atoms             " /), &
                           & lstat, error_data = error)
    call tests_write_status("4D array: adding a new variable", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if
    
    write(*,*) 
  end subroutine tests_def_var

  subroutine tests_write_var_integer(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid
    integer, target :: var(4), var2d(2, 2)
    character(len = 4) :: varc
    logical :: lstat
    type(etsf_io_low_error) :: error
    type(etsf_io_low_var_integer) :: var_gen
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_write_var_integer()..."
    call etsf_io_low_write_var(0, "atom_species", var, lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ), error)
    
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if
    call etsf_io_low_set_write_mode(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't switch to data mode"
      return
    end if

    call etsf_io_low_write_var(ncid, "pouet", var, lstat, error_data = error)
    call tests_write_status("argument varname: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ .and. error%target_type_id == ERROR_TYPE_VID), &
      & error)

    call etsf_io_low_write_var(ncid, "atom_species", varc, 4, lstat, error_data = error)
    call tests_write_status("argument var: wrong type", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_write_var(ncid, "atom_species", var(1:3), lstat, error_data = error)
    call tests_write_status("argument var: wrong dimensions", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_ARG), &
      & error)

    call etsf_io_low_write_var(ncid, "number_of_electrons", 12, lstat, error_data = error)
    call tests_write_status("argument var: good value (0D)", lstat, error)
    call etsf_io_low_read_var(ncid, "number_of_electrons", var(1), lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 12)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)
    
    var(:) = (/ 1, 2, 3, 4 /)
    call etsf_io_low_write_var(ncid, "atom_species", var, lstat, error_data = error)
    call tests_write_status("argument var: good value (1D)", lstat, error)
    call etsf_io_low_read_var(ncid, "atom_species", var, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 1 .and. var(2) == 2 .and. var(3) == 3 .and. var(4) == 4)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)


    var2d = reshape((/ 4, 5, 6, 7 /), (/ 2, 2/))
    call etsf_io_low_write_var(ncid, "atom_species", var2d(1:1, :), &
                            & lstat, error_data = error)
    call tests_write_status("argument var: wrong matching (2D <-> 1D)", (.not. lstat), error)

    call etsf_io_low_write_var(ncid, "atom_species", var2d, &
                            & lstat, error_data = error)
    call tests_write_status("argument var: good matching (2D <-> 1D)", lstat, error)
    call etsf_io_low_read_var(ncid, "atom_species", var, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 4 .and. var(2) == 5 .and. var(3) == 6 .and. var(4) == 7)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    var = (/ 7, 5, 3, 9 /)
    call etsf_io_low_write_var(ncid, "test_integer_2d", var, &
                            & lstat, start = (/ 1, 2, 3 /), count = (/ 0, 1, 1 /), &
                            & error_data = error)
    call tests_write_status("argument sub: wrong size", (.not. lstat), error)

    call etsf_io_low_write_var(ncid, "test_integer_2d", var, &
                            & lstat, start = (/ 1, 6 /), count = (/ 0, 1 /), error_data = error)
    call tests_write_status("argument sub: out-of-bounds", (.not. lstat), error)

    call etsf_io_low_write_var(ncid, "test_integer_2d", var(1:3), &
                            & lstat, start = (/ 1, 2 /), count = (/ 0, 1 /), error_data = error)
    call tests_write_status("argument sub: wrong dimensions", (.not. lstat), error)

    call etsf_io_low_write_var(ncid, "test_integer_2d", var, &
                            & lstat, start = (/ 1, 2 /), count = (/ 0, 1 /), error_data = error)
    call tests_write_status("argument sub: good dimensions", lstat, error)
    call etsf_io_low_read_var(ncid, "test_integer_2d", var, lstat, &
                            & start = (/ 1, 2 /), count = (/ 0, 1 /), error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 7 .and. var(2) == 5 .and. var(3) == 3 .and. var(4) == 9)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    var = (/ 1, 2, 3, 4 /)
    var_gen%data1D => var
    call etsf_io_low_write_var(ncid, "atom_species", var_gen, lstat, error_data = error)
    call tests_write_status("argument var: generic pointer (1D)", lstat, error)
    var(:) = 0
    call etsf_io_low_read_var(ncid, "atom_species", var_gen, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1) == 1 .and. var(2) == 2 .and. var(3) == 3 .and. var(4) == 4)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't close file"
      return
    end if

    write(*,*) 
  end subroutine tests_write_var_integer

  subroutine tests_write_var_double(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid, i
    double precision, target :: var(3, 4), bigvar(12), density(27)
    character(len = 3) :: varc(4)
    logical :: lstat
    type(etsf_io_low_error) :: error
    type(etsf_io_low_var_double) :: var_gen
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_write_var_double()..."
    call etsf_io_low_write_var(0, "reduced_atom_positions", var, lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ), error)
    
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if
    call etsf_io_low_set_write_mode(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't switch to data mode"
      return
    end if

    call etsf_io_low_write_var(ncid, "pouet", var, lstat, error_data = error)
    call tests_write_status("argument varname: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ .and. error%target_type_id == ERROR_TYPE_VID), &
      & error)

    call etsf_io_low_write_var(ncid, "reduced_atom_positions", varc, 3, lstat, error_data = error)
    call tests_write_status("argument var: wrong type", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_write_var(ncid, "reduced_atom_positions", var(:, 1:3), lstat, error_data = error)
    call tests_write_status("argument var: wrong dimensions", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_ARG), &
      & error)

    var = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /), (/ 3, 4 /))
    call etsf_io_low_write_var(ncid, "reduced_atom_positions", var, lstat, error_data = error)
    call tests_write_status("argument var: good value (2D)", lstat, error)
    call etsf_io_low_read_var(ncid, "reduced_atom_positions", var, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1, 1) == 1 .and. var(2, 1) == 2 .and. var(3, 1) == 3 .and. &
      & var(1, 2) == 4 .and. var(2, 2) == 5 .and. var(3, 2) == 6 .and. &
      & var(1, 3) == 7 .and. var(2, 3) == 8 .and. var(3, 3) == 9 .and. &
      & var(1, 4) == 10 .and. var(2, 4) == 11 .and. var(3, 4) == 12) ) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    bigvar = (/ 1d0, 2d0, 3d0, 4d0, 5d0, 6d0, 7d0, 8d0, 9d0, 10d0, 11d0, 0.5d0 /)
    call etsf_io_low_write_var(ncid, "reduced_atom_positions", bigvar(1:10), &
                            & lstat, error_data = error)
    call tests_write_status("argument var: wrong matching (2D <-> 1D)", (.not. lstat), error)

    call etsf_io_low_write_var(ncid, "reduced_atom_positions", bigvar, &
                            & lstat, error_data = error)
    call tests_write_status("argument var: good matching (2D <-> 1D)", lstat, error)
    call etsf_io_low_read_var(ncid, "reduced_atom_positions", var, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1, 1) == 1 .and. var(2, 1) == 2 .and. var(3, 1) == 3 .and. &
      & var(1, 2) == 4 .and. var(2, 2) == 5 .and. var(3, 2) == 6 .and. &
      & var(1, 3) == 7 .and. var(2, 3) == 8 .and. var(3, 3) == 9 .and. &
      & var(1, 4) == 10 .and. var(2, 4) == 11 .and. var(3, 4) == 0.5d0) ) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    density = (/ (-real(i) / 2.d0, i = 1, 27) /)
    call etsf_io_low_write_var(ncid, "density", density, &
                            & lstat, start = (/ 1, 1, 1, 2 /), count = (/ 0, 0, 0, 1 /), &
                            & error_data = error)
    call tests_write_status("argument var + sub: good matching (3D <-> 1D)", lstat, error)
    call etsf_io_low_read_var(ncid, "density", density, lstat, &
                            & start = (/ 1, 1, 1, 2 /), count = (/ 0, 0, 0, 1 /), &
                            & error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (density(1) == -0.5d0 .and. density(2) == -1.d0 .and. density(3) == -1.5d0) ) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    var = reshape((/ (real(i) / 2.d0, i = 1, 12) /), (/ 3, 4 /))
    var_gen%data2D => var
    call etsf_io_low_write_var(ncid, "reduced_atom_positions", var_gen, lstat, error_data = error)
    call tests_write_status("argument var: generic pointer (2D)", lstat, error)
    var(:, :) = 0.d0
    call etsf_io_low_read_var(ncid, "reduced_atom_positions", var_gen, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    if (.not. (var(1, 1) == 0.5d0 .and. var(2, 1) == 1.d0 .and. var(3, 1) == 1.5d0 .and. &
      & var(1, 2) == 2.d0 .and. var(2, 2) == 2.5d0 .and. var(3, 2) == 3.d0 .and. &
      & var(1, 3) == 3.5d0 .and. var(2, 3) == 4.d0 .and. var(3, 3) == 4.5d0 .and. &
      & var(1, 4) == 5.d0 .and. var(2, 4) == 5.5d0 .and. var(3, 4) == 6.d0) ) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_write_var_double

  subroutine tests_write_var_character(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid, pos
    character(len = 80) :: var
    integer :: vari(80)
    logical :: lstat
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_write_var_character()..."
    call etsf_io_low_write_var(0, "exchange_functional", var, 80, lstat, error_data = error)
    call tests_write_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ), error)
    
    call etsf_io_low_open_modify(ncid, "open_create_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if
    call etsf_io_low_set_write_mode(ncid, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't switch to data mode"
      return
    end if

    call etsf_io_low_write_var(ncid, "pouet", var, 80, lstat, error_data = error)
    call tests_write_status("argument varname: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ .and. error%target_type_id == ERROR_TYPE_VID), &
      & error)

    call etsf_io_low_write_var(ncid, "exchange_functional", vari, lstat, error_data = error)
    call tests_write_status("argument var: wrong type", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_write_var(ncid, "exchange_functional", var(1:50), 50, lstat, error_data = error)
    call tests_write_status("argument var: wrong dimensions", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_ARG), &
      & error)

    write(var, "(A)") "This is a wonderful functional"
    call etsf_io_low_write_var(ncid, "exchange_functional", var, 80, lstat, error_data = error)
    call tests_write_status("argument var: good value (one string)", lstat, error)
    call etsf_io_low_read_var(ncid, "exchange_functional", var, 80, lstat, error_data = error)
    call tests_write_status(" | reading variable", lstat, error)
    pos = index(var, char(0))
    if (pos > 0) then
      var(pos:len(var)) = repeat(" ", len(var) - pos + 1)
    end if
    if (trim(var) /= "This is a wonderful functional") then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_ATT
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_write_status(" | checking values", lstat, error)

    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_write_var_character

end program tests_write
