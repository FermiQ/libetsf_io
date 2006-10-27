program tests_read

  use etsf_io_low_level
  
  integer :: ncid
  logical :: stat
  integer :: dimval
  
  call tests_read_open()
  call tests_read_dim()
  call tests_check_var()
  call tests_check_att()
  
contains

  subroutine tests_read_status(name, ierr)
    character(len = *), intent(in) :: name
    logical, intent(in)            :: ierr
    
    if (ierr) then
      write(*, "(A,A,A,A)") "== ", name, repeat(" ", 68 - len(name)), "OK     =="
    else
      write(*, "(A,A,A,A)") "== ", name, repeat(" ", 68 - len(name)), "Failed =="
    end if
  end subroutine tests_read_status

  subroutine tests_read_open()
    integer :: ncid
    logical :: ierr
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_open_read()..."
    call etsf_io_low_open_read(ncid, "", ierr)
    call tests_read_status("argument filename: unknown file", (.not. ierr))
    
    call etsf_io_low_open_read(ncid, "tests_read.f90", ierr)
    call tests_read_status("argument filename: text file", (.not. ierr))
    
    call etsf_io_low_open_read(ncid, "open_read_t01.nc", ierr)
    call tests_read_status("argument filename: NetCDF without header", (.not. ierr))
    
    call etsf_io_low_open_read(ncid, "open_read_t02.nc", ierr)
    call tests_read_status("argument filename: NetCDF with wrong file_format header", &
                         & (.not. ierr))
                         
    call etsf_io_low_open_read(ncid, "open_read_t03.nc", ierr)
    call tests_read_status("argument filename: NetCDF with obsolete file_format_version", &
                         & (.not. ierr))
                         
    call etsf_io_low_open_read(ncid, "open_read_t03.nc", ierr, version_min = 1.3)
    call tests_read_status("argument version_min: NetCDF with obsolete file_format_version", &
                         & (.not. ierr))
                         
    call etsf_io_low_open_read(ncid, "open_read_t04.nc", ierr, &
                             & error_handle=etsf_io_low_error_handle)
    call tests_read_status("argument filename: NetCDF with a valid header", ierr)
    call etsf_io_low_close(ncid, ierr)
    
    write(*,*) 
  end subroutine tests_read_open
  
  subroutine tests_read_dim()
    integer :: ncid, dimvalue
    logical :: ierr
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_read_dim()..."
    call etsf_io_low_read_dim(0, "", dimvalue, ierr)
    call tests_read_status("argument ncid: wrong value", (.not. ierr))
    
    call etsf_io_low_open_read(ncid, "read_dim_t01.nc", ierr)
    if (.not. ierr) then
      write(*,*) "Abort, can't open file"
      return
    end if
    call etsf_io_low_read_dim(ncid, "pouet", dimvalue, ierr)
    call tests_read_status("argument dimname: wrong value", (.not. ierr))
    
    call etsf_io_low_read_dim(ncid, "number_of_atoms", dimvalue, ierr)
    call tests_read_status("argument dimname: good value", (ierr .and. (dimvalue == 5)))

    call etsf_io_low_close(ncid, ierr)
    
    write(*,*) 
  end subroutine tests_read_dim

  subroutine tests_check_var()
    integer :: ncid, ncvarid, vartype, vardims(2)
    logical :: ierr
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_check_var()..."
    call etsf_io_low_check_var(0, ncvarid, "", vartype, vardims, 2, ierr)
    call tests_read_status("argument ncid: wrong value", (.not. ierr))
    
    call etsf_io_low_open_read(ncid, "check_var_t01.nc", ierr)
    if (.not. ierr) then
      write(*,*) "Abort, can't open file"
      return
    end if

    call etsf_io_low_check_var(ncid, ncvarid, "pouet", vartype, vardims, 2, ierr)
    call tests_read_status("argument varname: wrong value", (.not. ierr))

    vartype = NF90_DOUBLE
    vardims(1) = 5
    call etsf_io_low_check_var(ncid, ncvarid, "atom_species", vartype, vardims(1:1), 1, ierr)
    call tests_read_status("argument vartype: wrong value", (.not. ierr))
    vartype = NF90_INT
    vardims(1) = 5
    call etsf_io_low_check_var(ncid, ncvarid, "atom_species", vartype, vardims(1:1), 1, ierr, &
                             & error_handle=etsf_io_low_error_handle)
    call tests_read_status("argument vartype: good value", ierr)

    vartype = NF90_INT
    vardims(:) = (/ 5, 3 /)
    call etsf_io_low_check_var(ncid, ncvarid, "atom_species", vartype, vardims, 2, ierr)
    call tests_read_status("argument vardims: wrong dimension", (.not. ierr))
    vartype = NF90_INT
    vardims(1) = 10
    call etsf_io_low_check_var(ncid, ncvarid, "atom_species", vartype, vardims(1:1), 1, ierr)
    call tests_read_status("argument vardims: wrong values", (.not. ierr))
    vartype = NF90_DOUBLE
    vardims = (/ 3, 5 /)
    call etsf_io_low_check_var(ncid, ncvarid, "reduced_atom_positions", &
                             & vartype, vardims, 2, ierr, error_handle=etsf_io_low_error_handle)
    call tests_read_status("argument vardims: good values", ierr)
    
    call etsf_io_low_close(ncid, ierr)
    
    write(*,*) 
  end subroutine tests_check_var

  subroutine tests_check_att()
    integer :: ncid, atttype, attlen, ncvarid
    logical :: ierr
    
    write(*,*)
    write(*,*) "Testing etsf_io_low_check_att()..."
    call etsf_io_low_check_att(0, NF90_GLOBAL, "", atttype, attlen, ierr)
    call tests_read_status("argument ncid: wrong value", (.not. ierr))
    
    call etsf_io_low_open_read(ncid, "check_att_t01.nc", ierr)
    if (.not. ierr) then
      write(*,*) "Abort, can't open file"
      return
    end if
    call etsf_io_low_check_var(ncid, ncvarid, "atom_species", NF90_INT, (/ 5 /), 1, ierr)
    if (.not. ierr) then
      write(*,*) "Abort, can't get variable"
      call etsf_io_low_close(ncid, ierr)
      return
    end if

    call etsf_io_low_check_att(ncid, NF90_GLOBAL, "file_format", NF90_CHAR, 80, ierr, &
                             & error_handle=etsf_io_low_error_handle)
    call tests_read_status("argument ncvarid: NF90_GLOBAL value", ierr)

    call etsf_io_low_check_att(ncid, 0, "comment", NF90_CHAR, 80, ierr)
    call tests_read_status("argument ncvarid: wrong value", (.not. ierr))

    call etsf_io_low_check_att(ncid, ncvarid, "comment", NF90_CHAR, 80, ierr, &
                             & error_handle=etsf_io_low_error_handle)
    call tests_read_status("argument ncvarid: valid variable attribute", ierr)

    call etsf_io_low_check_att(ncid, NF90_GLOBAL, "file_format", NF90_INT, 80, ierr)
    call tests_read_status("argument atttype: wrong value", (.not. ierr))

    call etsf_io_low_check_att(ncid, NF90_GLOBAL, "file_format_version", NF90_DOUBLE, 2, ierr)
    call tests_read_status("argument attlen: wrong value", (.not. ierr))

    call etsf_io_low_check_att(ncid, NF90_GLOBAL, "file_format_version", NF90_DOUBLE, 1, ierr, &
                             & error_handle=etsf_io_low_error_handle)
    call tests_read_status("argument attlen: good value", ierr)
    call etsf_io_low_close(ncid, ierr)
    
    write(*,*) 
  end subroutine tests_check_att
  
!  subroutine tests_get_var_integer()
!    integer :: ncid, ncvarid
!    integer :: var(5)
!    logical :: ierr
!    
!    write(*,*)
!    write(*,*) "Testing etsf_io_low_get_var_integer()..."
!    call etsf_io_low_get_var(0, 0, var, 5, ierr)
!    call tests_read_status("argument ncid: wrong value", (.not. ierr))
!    
!    call etsf_io_low_open_read(ncid, "get_var_t01.nc", ierr)
!    if (.not. ierr) then
!      write(*,*) "Abort, can't open file"
!      return
!    end if
!    call etsf_io_low_check_var(ncid, ncvarid, "atom_species", NF90_INT, (/ 5 /), 1, ierr)
!    if (.not. ierr) then
!      write(*,*) "Abort, can't get variable"
!      call etsf_io_low_close(ncid, ierr)
!      return
!    end if
!
!    call etsf_io_low_get_var(ncid, 0, var, 5, ierr)
!    call tests_read_status("argument ncvarid: wrong value", (.not. ierr))
!
!    call etsf_io_low_get_var(ncid, ncvarid, var, 5, ierr, &
!                           & error_handle=etsf_io_low_error_handle)
!    call tests_read_status("argument ncvarid: good value", (ierr .and. &
!                         & var(1) == 1 .and. var(2) == 2 .and. var(3) == 2 .and. &
!                         & var(4) == 2 .and. var(5) == 2))
!
!    call etsf_io_low_close(ncid, ierr)
!    
!    write(*,*) 
!  end subroutine tests_get_var_integer
!
!  subroutine tests_get_var_double()
!    integer :: ncid, ncvarid, s
!    double precision :: var(3, 3), toto(9)
!    logical :: ierr
!    
!    write(*,*)
!    write(*,*) "Testing etsf_io_low_get_var_double()..."
!    call etsf_io_low_get_var(0, 0, var(1, :), 3, ierr)
!    call tests_read_status("argument ncid: wrong value", (.not. ierr))
!    
!    call etsf_io_low_open_read(ncid, "get_var_t01.nc", ierr)
!    if (.not. ierr) then
!      write(*,*) "Abort, can't open file"
!      return
!    end if
!    call etsf_io_low_check_var(ncid, ncvarid, "primitive_vectors", NF90_DOUBLE, (/ 3, 3 /), 2, !ierr)
!    if (.not. ierr) then
!      write(*,*) "Abort, can't get variable"
!      call etsf_io_low_close(ncid, ierr)
!      return
!    end if
!
!    call etsf_io_low_get_var(ncid, 0, var(1, :), 3, ierr)
!    call tests_read_status("argument ncvarid: wrong value", (.not. ierr))
!  
!    call etsf_io_low_get_var_double(ncid, ncvarid, toto, 9, ierr, &
!                           & error_handle=etsf_io_low_error_handle)
!    s = nf90_get_var(ncid, ncvarid, values = toto)
!    write(*,*) toto
!    s = nf90_get_var(ncid, ncvarid, values = var)
!    write(*,*) var
!    call tests_read_status("argument ncvarid: good value", (ierr .and. &
!                         & var(1, 1) == 10. .and. var(1, 2) == 0. .and. var(1, 3) == 0. .and. &
!                         & var(2, 1) == 0. .and. var(2, 2) == 10. .and. var(2, 3) == 0. .and. &
!                         & var(3, 1) == 0. .and. var(3, 2) == 0. .and. var(3, 3) == 10.))
!
!    call etsf_io_low_close(ncid, ierr)
!    
!    write(*,*) 
!  end subroutine tests_get_var_double
  
end program tests_read
