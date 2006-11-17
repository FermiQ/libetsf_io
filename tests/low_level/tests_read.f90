program tests_read

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

  call tests_read_open(trim(path))
  call tests_read_dim(trim(path))
  call tests_read_var_infos(trim(path))
  call tests_check_var(trim(path))
  call tests_check_att(trim(path))
  call tests_read_var_integer(trim(path))
  call tests_read_var_double(trim(path))
  call tests_read_var_character(trim(path))
  
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

  subroutine tests_read_open(path)
    character(len = *), intent(in) :: path
    integer :: ncid
    logical :: lstat
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_open_read()..."
    call etsf_io_low_open_read(ncid, "", lstat, error_data = error)
    call tests_read_status("argument filename: unknown file", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_IO .and. error%target_type_id == ERROR_TYPE_ORD), error)
    
    call etsf_io_low_open_read(ncid, path//"/tests_read.f90", lstat, error_data = error)
    call tests_read_status("argument filename: text file", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_IO .and. error%target_type_id == ERROR_TYPE_ORD), error)
    
    call etsf_io_low_open_read(ncid, path//"/open_read_t01.nc", lstat, error_data = error)
    call tests_read_status("argument filename: NetCDF without header", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ .and. error%target_type_id == ERROR_TYPE_ATT), error)
    
    call etsf_io_low_open_read(ncid, path//"/open_read_t02.nc", lstat, error_data = error)
    call tests_read_status("argument filename: NetCDF with wrong file_format header", &
                         & (.not. lstat .and. error%access_mode_id == ERROR_MODE_SPEC), error)
                         
    call etsf_io_low_open_read(ncid, path//"/open_read_t03.nc", lstat, error_data = error)
    call tests_read_status("argument filename: NetCDF with obsolete file_format_version", &
                         & (.not. lstat .and. error%access_mode_id == ERROR_MODE_SPEC), error)
                         
    call etsf_io_low_open_read(ncid, path//"/open_read_t03.nc", lstat, version_min = 1.3, &
                            &  error_data = error)
    call tests_read_status("argument version_min: NetCDF with obsolete file_format_version", &
                         & (.not. lstat .and. error%access_mode_id == ERROR_MODE_SPEC), error)
                         
    call etsf_io_low_open_read(ncid, path//"/open_read_t04.nc", lstat, error_data = error)
    call tests_read_status("argument filename: NetCDF with a valid header", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_read_open
  
  subroutine tests_read_dim(path)
    character(len = *), intent(in) :: path
    integer :: ncid, dimvalue
    logical :: lstat
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_read_dim()..."
    call etsf_io_low_read_dim(0, "", dimvalue, lstat, error_data = error)
    call tests_read_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ), error)
    
    call etsf_io_low_open_read(ncid, path//"/read_dim_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if
    call etsf_io_low_read_dim(ncid, "pouet", dimvalue, lstat, error_data = error)
    call tests_read_status("argument dimname: wrong value", (.not. lstat), error)
    
    call etsf_io_low_read_dim(ncid, "number_of_atoms", dimvalue, lstat, error_data = error)
    call tests_read_status("argument dimname: good value", (lstat .and. (dimvalue == 5)), error)

    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_read_dim

  subroutine tests_read_var_infos(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid, vartype, vardims(2)
    logical :: lstat
    type(etsf_io_low_error) :: error
    type(etsf_io_low_var_infos) :: var_infos
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_read_var_infos()..."
    call etsf_io_low_read_var_infos(0, "", var_infos, lstat, error_data = error)
    call tests_read_status("argument ncid: wrong value", (.not. lstat), error)
    
    call etsf_io_low_open_read(ncid, path//"/check_var_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_read_var_infos(ncid, "pouet", var_infos, lstat, error_data = error)
    call tests_read_status("argument varname: wrong value", (.not. lstat), error)

    call etsf_io_low_read_var_infos(ncid, "atom_species", var_infos, lstat, error_data = error)
    call tests_read_status("argument varname: good value", lstat, error)
    if (.not. (var_infos%nctype == etsf_io_low_integer)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_VAR
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_read_status(" | checking type value", lstat, error)
    if (.not. (var_infos%ncshape == 1)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_VAR
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_read_status(" | checking shape value", lstat, error)
    if (.not. (var_infos%ncdims(1) == 5)) then
      error%access_mode_id = ERROR_MODE_SPEC
      error%target_type_id = ERROR_TYPE_VAR
      error%error_message = "wrong value"
      lstat = .false.
    end if
    call tests_read_status(" | checking dimension values", lstat, error)
    
    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_read_var_infos

  subroutine tests_check_var(path)
    character(len = *), intent(in) :: path
    logical :: lstat
    integer :: level
    type(etsf_io_low_error) :: error
    type(etsf_io_low_var_infos) :: var_from, var_to
    
    write(*,*)
    error%access_mode_id = ERROR_MODE_SPEC
    error%target_type_id = ERROR_TYPE_VAR
    error%error_message = "wrong value"
    
    write(*,*) "Testing etsf_io_low_check_var()..."
    var_from%nctype = etsf_io_low_character
    var_to%nctype = etsf_io_low_double
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error)
    call tests_read_status("field nctype: incompatible values", (.not. lstat), error)
    
    var_from%ncshape = 0
    var_to%ncshape = 0
    
    var_from%nctype = etsf_io_low_integer
    var_to%nctype = etsf_io_low_double
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error)
    call tests_read_status("field nctype: compatible values", (lstat .and. &
                         & modulo(level / etsf_io_low_var_type_dif, 2) == 1), error)
                         
    var_from%nctype = etsf_io_low_double
    var_to%nctype = etsf_io_low_double
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error)
    call tests_read_status("field nctype: matching values", (lstat .and. &
                         & modulo(level / etsf_io_low_var_type_dif, 2) == 0), error)
                         
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error)
    call tests_read_status("field ncshape (0D): matching values", (lstat .and. &
                         & modulo(level / etsf_io_low_var_shape_dif, 2) == 0), error)
                         
    var_from%ncshape = 4
    var_to%ncshape = 2
    var_from%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    var_to%ncdims(1:2) = (/ 25, 2 /)
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error)
    call tests_read_status("field ncshape: uncompatible values", (.not. lstat), error)

    var_from%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    var_to%ncdims(1:2) = (/ 27, 2 /)
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error)
    call tests_read_status("field ncshape: compatible values", (lstat .and. &
                         & modulo(level / etsf_io_low_var_shape_dif, 2) == 1), error)
                         
    var_from%ncshape = 4
    var_to%ncshape = 4
    var_from%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    var_to%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error)
    call tests_read_status("field ncshape (nD): matching values", (lstat .and. level == 0), error)

    var_from%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    var_to%ncdims(1:4) = (/ 3, 3, 2, 2 /)
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error, sub = 3)
    call tests_read_status("sub: incompatible values", (.not. lstat), error)

    var_from%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    var_to%ncdims(1:4) = (/ 3, 3, 5, 5 /)
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error, sub = 2)
    call tests_read_status("sub: compatible values", lstat, error)

    var_from%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    var_to%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error, sub = 4)
    call tests_read_status("sub: compatible values (matching case)", lstat, error)

    var_from%ncshape = 4
    var_to%ncshape = 2
    var_from%ncdims(1:4) = (/ 3, 3, 3, 2 /)
    var_to%ncdims(1:2) = (/ 27, 2 /)
    call etsf_io_low_check_var(var_from, var_to, lstat, level = level, error_data = error, sub = 4)
    call tests_read_status("sub: compatible values (diff shape)", lstat, error)
                         
    write(*,*) 
  end subroutine tests_check_var

  subroutine tests_check_att(path)
    character(len = *), intent(in) :: path
    integer :: ncid, atttype, attlen
    logical :: lstat
    type(etsf_io_low_error) :: error
    type(etsf_io_low_var_infos) :: var_infos
        
    write(*,*)
    write(*,*) "Testing etsf_io_low_check_att()..."
    call etsf_io_low_check_att(0, etsf_io_low_global_att, "", atttype, attlen, lstat, error_data = error)
    call tests_read_status("argument ncid: wrong value", (.not. lstat), error)
    
    call etsf_io_low_open_read(ncid, path//"/check_att_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if
    call etsf_io_low_read_var_infos(ncid, "atom_species", var_infos, lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't get variable"
      call etsf_io_low_close(ncid, lstat)
      return
    end if

    call etsf_io_low_check_att(ncid, etsf_io_low_global_att, "file_format", NF90_CHAR, 80, lstat, &
                             & error_data = error)
    call tests_read_status("argument ncvarid: etsf_io_low_global_att value", lstat, error)

    call etsf_io_low_check_att(ncid, 0, "comment", NF90_CHAR, 80, lstat, error_data = error)
    call tests_read_status("argument ncvarid: wrong value", (.not. lstat), error)

    call etsf_io_low_check_att(ncid, var_infos%ncid, "mass", NF90_FLOAT, 1, lstat, &
                             & error_data = error)
    call tests_read_status("argument ncvarid: valid variable attribute (0D)", lstat, error)

    call etsf_io_low_check_att(ncid, var_infos%ncid, "comment", NF90_CHAR, 80, lstat, &
                             & error_data = error)
    call tests_read_status("argument ncvarid: valid variable attribute (1D)", lstat, error)

    call etsf_io_low_check_att(ncid, etsf_io_low_global_att, "file_format", NF90_INT, 80, lstat, &
                             & error_data = error)
    call tests_read_status("argument atttype: wrong type", (.not. lstat), error)

    call etsf_io_low_check_att(ncid, etsf_io_low_global_att, "file_format_version", NF90_FLOAT, 2, lstat, &
                             & error_data = error)
    call tests_read_status("argument attlen: wrong dimension", (.not. lstat), error)

    call etsf_io_low_check_att(ncid, etsf_io_low_global_att, "file_format_version", NF90_FLOAT, 1, lstat, &
                             & error_data = error)
    call tests_read_status("argument attlen: good value", lstat, error)
    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_check_att
  
  subroutine tests_read_var_integer(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid
    integer, target :: var(5), var2d(2, 2), bigvar(4), var2d_snd(2, 3)
    character(len = 5) :: varc
    logical :: lstat
    type(etsf_io_low_error) :: error
    type(etsf_io_low_var_integer) :: atom_species
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_read_var_integer()..."
    call etsf_io_low_read_var(0, "atom_species", var, lstat, error_data = error)
    call tests_read_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ), error)
    
    call etsf_io_low_open_read(ncid, path//"/read_var_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_read_var(ncid, "pouet", var, lstat, error_data = error)
    call tests_read_status("argument varname: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ .and. error%target_type_id == ERROR_TYPE_VID), &
      & error)

    call etsf_io_low_read_var(ncid, "atom_species", varc, 5, lstat, error_data = error)
    call tests_read_status("argument var: wrong type", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_read_var(ncid, "atom_species", var(1:4), lstat, error_data = error)
    call tests_read_status("argument var: wrong dimensions", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_read_var(ncid, "space_group", var(1), lstat, error_data = error)
    call tests_read_status("argument var: good value (0D)", (lstat .and. &
                         & var(1) == 1), error)

    call etsf_io_low_read_var(ncid, "atom_species", var, lstat, error_data = error)
    call tests_read_status("argument var: good value (1D)", (lstat .and. &
                         & var(1) == 1 .and. var(2) == 2 .and. var(3) == 2 .and. &
                         & var(4) == 2 .and. var(5) == 2), error)

    call etsf_io_low_read_var(ncid, "test_integer_2d", var2d, &
                            & lstat, error_data = error)
    call tests_read_status("argument var: good value (2D)", (lstat .and. &
                         & var2d(1, 1) == 1 .and. var2d(2, 1) == 2 .and. &
                         & var2d(1, 2) == 3 .and. var2d(2, 2) == 4), error)

    call etsf_io_low_read_var(ncid, "test_integer_2d", bigvar(1:3), &
                            & lstat, error_data = error)
    call tests_read_status("argument var: wrong matching (2D <-> 1D)", (.not. lstat), error)

    call etsf_io_low_read_var(ncid, "test_integer_2d", bigvar, &
                            & lstat, error_data = error)
    call tests_read_status("argument var: good matching (2D <-> 1D)", (lstat .and. &
                         & bigvar(1) == 1 .and. bigvar(2) == 2 .and. &
                         & bigvar(3) == 3 .and. bigvar(4) == 4), error)

    call etsf_io_low_read_var(ncid, "test_integer_2d", bigvar(1:2), &
                            & lstat, sub = (/ 0, 2, 3 /), error_data = error)
    call tests_read_status("argument sub: wrong size", (.not. lstat), error)

    call etsf_io_low_read_var(ncid, "test_integer_2d", bigvar(1:2), &
                            & lstat, sub = (/ 0, 3 /), error_data = error)
    call tests_read_status("argument sub: out-of-bounds", (.not. lstat), error)

    call etsf_io_low_read_var(ncid, "test_integer_2d", bigvar(1:3), &
                            & lstat, sub = (/ 0, 2 /), error_data = error)
    call tests_read_status("argument sub: wrong dimensions", (.not. lstat), error)

    call etsf_io_low_read_var(ncid, "test_integer_2d", bigvar(1:2), &
                            & lstat, sub = (/ 0, 2 /), error_data = error)
    call tests_read_status("argument sub: good dimensions", (lstat .and. &
                         & bigvar(1) == 3 .and. bigvar(2) == 4), error)

    call etsf_io_low_read_var(ncid, "test_integer_2d", bigvar, &
                            & lstat, sub = (/ 0, 0 /), error_data = error)
    call tests_read_status("argument sub: all reading", (lstat .and. &
                         & bigvar(1) == 1 .and. bigvar(2) == 2 .and. &
                         & bigvar(3) == 3 .and. bigvar(4) == 4), error)

    call etsf_io_low_read_var(ncid, "test_integer_4d", var2d_snd, &
                            & lstat, sub = (/ 0, 0, 2, 2 /), error_data = error)
    call tests_read_status("argument sub: two sub reading", (lstat .and. &
                         & var2d_snd(1, 1) == -7 .and. var2d_snd(2, 1) == -8), error)

    atom_species%data1D => var
    call etsf_io_low_read_var(ncid, "atom_species", atom_species, lstat, error_data = error)
    call tests_read_status("argument var: generic pointer (1D)", (lstat .and. &
                         & var(1) == 1 .and. var(2) == 2 .and. var(3) == 2 .and. &
                         & var(4) == 2 .and. var(5) == 2), error)

    atom_species%data1D => null()
    atom_species%data2D => var2d
    call etsf_io_low_read_var(ncid, "test_integer_2d", atom_species, lstat, error_data = error)
    call tests_read_status("argument var: generic pointer (2D)", (lstat .and. &
                         & var2d(1, 1) == 1 .and. var2d(2, 1) == 2 .and. &
                         & var2d(1, 2) == 3 .and. var2d(2, 2) == 4), error)

    atom_species%data1D => var2d(2, :)
    call etsf_io_low_read_var(ncid, "test_integer_2d", atom_species, &
                            & lstat, sub = (/ 0, 2 /), error_data = error)
    call tests_read_status("argument var + sub: generic pointer", (lstat .and. &
                         & var2d(2, 1) == 3 .and. var2d(2, 2) == 4), error)

    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_read_var_integer

  subroutine tests_read_var_double(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid
    double precision, target :: var(3), var2d(3, 3), bigvar(15), density(27)
    character(len = 3) :: varc
    logical :: lstat
    type(etsf_io_low_error) :: error
    type(etsf_io_low_var_double) :: var_gen
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_read_var_double()..."
    call etsf_io_low_read_var(0, "test_double_1d", var, lstat, error_data = error)
    call tests_read_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ), error)
    
    call etsf_io_low_open_read(ncid, path//"/read_var_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_read_var(ncid, "pouet", var, lstat, error_data = error)
    call tests_read_status("argument varname: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ .and. error%target_type_id == ERROR_TYPE_VID), &
      & error)

    call etsf_io_low_read_var(ncid, "test_double_1d", varc, 3, lstat, error_data = error)
    call tests_read_status("argument var: wrong type", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_read_var(ncid, "test_double_1d", var(1:2), lstat, error_data = error)
    call tests_read_status("argument var: wrong dimensions", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_read_var(ncid, "test_double_0d", var(1), lstat, error_data = error)
    call tests_read_status("argument var: good value (0D)", (lstat .and. &
                         & var(1) == 3.14d0), error)

    call etsf_io_low_read_var(ncid, "test_double_1d", var, lstat, error_data = error)
    call tests_read_status("argument var: good value (1D)", (lstat .and. &
                         & var(1) == 1. .and. var(2) == 2. .and. var(3) == 3.), error)

    call etsf_io_low_read_var(ncid, "primitive_vectors", var2d, &
                            & lstat, error_data = error)
    call tests_read_status("argument var: good value (2D)", (lstat .and. &
                         & var2d(1, 1) == 10. .and. var2d(2, 1) == 0. .and. &
                         & var2d(3, 1) == 0. .and. var2d(1, 2) == 0. .and. &
                         & var2d(2, 2) == 10. .and. var2d(3, 2) == 0. .and. &
                         & var2d(1, 3) == 0. .and. var2d(2, 3) == 0. .and. &
                         & var2d(3, 3) == 10.), error)

    call etsf_io_low_read_var(ncid, "reduced_atom_positions", bigvar(1:10), &
                            & lstat, error_data = error)
    call tests_read_status("argument var: wrong matching (2D <-> 1D)", (.not. lstat), error)

    call etsf_io_low_read_var(ncid, "reduced_atom_positions", bigvar, &
                            & lstat, error_data = error)
    call tests_read_status("argument var: good matching (2D <-> 1D)", (lstat .and. &
                         & bigvar(1) == 0.5d0 .and. bigvar(2) == 0.5d0 .and. &
                         & bigvar(3) == 0.5d0 .and. bigvar(4) == 0.6d0), error)

    call etsf_io_low_read_var(ncid, "density", density, &
                            & lstat, sub = (/ 0, 0, 0, 2 /), error_data = error)
    call tests_read_status("argument var + sub: good matching (3D <-> 1D)", (lstat .and. &
                         & density(1) == -0.d0 .and. density(2) == -1.d0 .and. &
                         & density(3) == -0.d0 .and. density(4) == -1.d0 .and. &
                         & density(5) == -2.d0 .and. density(6) == -1.d0 .and. &
                         & density(7) == -0.d0 .and. density(8) == -1.d0 .and. &
                         & density(9) == -0.d0), error)

    var_gen%data1D => bigvar
    call etsf_io_low_read_var(ncid, "reduced_atom_positions", var_gen, &
                            & lstat, error_data = error)
    call tests_read_status("argument var: generic pointer (1D)", (lstat .and. &
                         & bigvar(1) == 0.5d0 .and. bigvar(2) == 0.5d0 .and. &
                         & bigvar(3) == 0.5d0 .and. bigvar(4) == 0.6d0), error)

    var_gen%data1D => var2d(2, :)
    call etsf_io_low_read_var(ncid, "primitive_vectors", var_gen, &
                            & lstat, sub = (/ 0, 2 /), error_data = error)
    call tests_read_status("argument var + sub: generic pointer", (lstat .and. &
                         & var2d(2, 1) == 0. .and. var2d(2, 2) == 10. .and. &
                         & var2d(2, 3) == 0.), error)

    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_read_var_double

  subroutine tests_read_var_character(path)
    character(len = *), intent(in) :: path
    integer :: ncid, ncvarid
    character(len = 80) :: var(2)
    integer :: vari(5)
    logical :: lstat
    type(etsf_io_low_error) :: error
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_read_var_character()..."
    call etsf_io_low_read_var(0, "atom_species_names", var, 80, lstat, error_data = error)
    call tests_read_status("argument ncid: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ), error)
    
    call etsf_io_low_open_read(ncid, path//"/read_var_t01.nc", lstat)
    if (.not. lstat) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_read_var(ncid, "pouet", var, 80, lstat, error_data = error)
    call tests_read_status("argument varname: wrong value", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_INQ .and. error%target_type_id == ERROR_TYPE_VID), &
      & error)

    call etsf_io_low_read_var(ncid, "atom_species_names", vari, lstat, error_data = error)
    call tests_read_status("argument var: wrong type", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_read_var(ncid, "atom_species_names", var(1:1), 80, lstat, error_data = error)
    call tests_read_status("argument var: wrong dimensions", (.not. lstat .and. &
      & error%access_mode_id == ERROR_MODE_SPEC .and. error%target_type_id == ERROR_TYPE_VAR), &
      & error)

    call etsf_io_low_read_var(ncid, "atom_species_names", var, 80, lstat, error_data = error)
    call tests_read_status("argument var: good value (1D)", (lstat .and. &
                         & var(1)(1:2) == "Si" .and. var(2)(1:1) == "H"), error)

    call etsf_io_low_close(ncid, lstat)
    
    write(*,*) 
  end subroutine tests_read_var_character
  
end program tests_read
