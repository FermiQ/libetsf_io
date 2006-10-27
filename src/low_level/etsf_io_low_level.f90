module etsf_io_low_level

  use netcdf

  implicit none
  
  ! Error handling
  integer, parameter, private :: ERROR_DEF = 1, ERROR_GET = 2, ERROR_IO   = 3, &
                               & ERROR_INQ = 4, ERROR_PUT = 5, ERROR_SPEC = 6
  character(len = 15), dimension(6) :: etsf_io_low_error_mode = (/ &
                                     & "define         ", &
                                     & "get            ", &
                                     & "input/output   ", &
                                     & "inquire        ", &
                                     & "put            ", &
                                     & "specifications " /)

  integer, parameter, private :: ERROR_ATT =  1, ERROR_DID =  2, ERROR_DIM = 3, &
                               & ERROR_END =  4, ERROR_INI =  5, ERROR_OCR = 6, &
                               & ERROR_ORD =  7, ERROR_OWR =  8, ERROR_VAR = 9, &
                               & ERROR_VID = 10, ERROR_CLO = 11
  character(len = 22), dimension(11), parameter :: etsf_io_low_error_type = (/ &
    & "attribute             ", "dimension ID          ", &
    & "dimension             ", "end definitions       ", &
    & "initialize file       ", "create file           ", &
    & "open file for reading ", "open file for writing ", &
    & "variable              ", "variable ID           ", &
    & "close file            " /)
    
  interface
    subroutine error_handle(mode, type, tgt, parent, err)
      integer, intent(in)  :: mode, type
      character(len = *), intent(in)  :: tgt, parent
      character(len = 80), intent(in) :: err
    end subroutine error_handle
  end interface

  !Generic interface of the routines etsf_io_low_get_var
!  interface etsf_io_low_get_var
!    module procedure etsf_io_low_get_var_integer
!    module procedure etsf_io_low_get_var_double
!    module procedure etsf_io_low_get_var_character
!  end interface etsf_io_low_get_var
  !End of the generic interface of etsf_io_low_get_var
  
contains

  subroutine etsf_io_low_error_handle(mode, type, tgt, parent, err)
    integer, intent(in)  :: mode, type
    character(len = *), intent(in)  :: tgt, parent
    character(len = 80), intent(in) :: err
      
    ! Error handling
    write(*,*) 
    write(*,*) "    ***"
    write(*,*) "    *** ETSF I/O ERROR"
    write(*,*) "    ***"
    write(*,*) "    *** Calling subprogram : ", parent
    write(*,*) "    *** Action performed   : ", etsf_io_low_error_mode(mode), &
             & " ", etsf_io_low_error_type(type)
    write(*,*) "    *** Target             : ", tgt
    write(*,*) "    *** Error message      : ", trim(err)
    write(*,*) "    ***"
    write(*,*) 
  end subroutine etsf_io_low_error_handle

  subroutine etsf_io_low_read_dim(ncid, dimname, dimvalue, ierr, error_handle)
    integer, intent(in)            :: ncid
    character(len = *), intent(in) :: dimname
    integer, intent(out)           :: dimvalue
    logical, intent(out)           :: ierr
    optional                       :: error_handle

    !local
    character(len = *), parameter :: me = "etsf_io_low_read_dim"
    integer :: s, dimid

    ierr = .false.
    ! will inq_dimid() and inq_dimlen() + error handling
    s = nf90_inq_dimid(ncid, dimname, dimid)
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_INQ, ERROR_DID, dimname, me, nf90_strerror(s))
      end if
      return
    end if
    s = nf90_inquire_dimension(ncid, dimid, len = dimvalue)
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_INQ, ERROR_DIM, dimname, me, nf90_strerror(s))
      end if
      return
    end if
    ierr = .true.    
  end subroutine etsf_io_low_read_dim
  
  subroutine etsf_io_low_check_var(ncid, ncvarid, varname, vartype, vardims, &
                                 & nbvardims, ierr, error_handle)
    integer, intent(in)            :: ncid
    integer, intent(out)           :: ncvarid
    character(len = *), intent(in) :: varname
    integer, intent(in)            :: vartype, nbvardims
    integer, intent(in)            :: vardims(1:nbvardims)
    logical, intent(out)           :: ierr
    optional                       :: error_handle

    !Local
    character(len = *), parameter :: me = "etsf_io_low_check_var"
    character(len = 80) :: err
    integer :: i, s, nctype, ncdims, dimvalue
    integer, allocatable :: ncdimids(:)
    
    ierr = .false.
    ! will inq_varid()
    s = nf90_inq_varid(ncid, varname, ncvarid)
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_INQ, ERROR_VID, varname, me, nf90_strerror(s))
      end if
      return
    end if
    ! will inq_vartype()
    ! will inq_varndims()
    s = nf90_inquire_variable(ncid, ncvarid, xtype = nctype, ndims = ncdims)
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_INQ, ERROR_VAR, varname, me, nf90_strerror(s))
      end if
      return
    end if
    ! Check the type
    if (nctype /= vartype) then
      write(err, "(A,I5,A,I5,A)") "wrong type (read = ", nctype, &
                                & ", awaited = ", vartype, ")"
      if (present(error_handle)) then
        call error_handle(ERROR_SPEC, ERROR_VAR, varname, me, err)
      end if
      return
    end if
    ! Check the dimensions
    if (ncdims /= nbvardims) then
      write(err, "(A,I5,A,I5,A)") "wrong number of dimensions (read = ", ncdims, &
                                & ", awaited = ", nbvardims, ")"
      if (present(error_handle)) then
        call error_handle(ERROR_SPEC, ERROR_VAR, varname, me, err)
      end if
      return
    end if
    ! will inq_vardimid()
    allocate(ncdimids(1:nbvardims))
    s = nf90_inquire_variable(ncid, ncvarid, dimids = ncdimids)
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_INQ, ERROR_VAR, varname, me, nf90_strerror(s))
      end if
      deallocate(ncdimids)
      return
    end if
    do i = 1, nbvardims, 1
      ! will inq_dimlen()
      s = nf90_inquire_dimension(ncid, ncdimids(i), len = dimvalue)
      if (s /= nf90_noerr) then
        if (present(error_handle)) then
          call error_handle(ERROR_INQ, ERROR_DIM, &
                          & "name not available", me, nf90_strerror(s))
        end if
        deallocate(ncdimids)
        return
      end if
      ! Test the dimensions
      if (dimvalue /= vardims(i)) then
        write(err, "(A,I0,A,I5,A,I5,A)") "wrong dimension length for index ", i, &
                                       & " (read = ", dimvalue, &
                                       & ", awaited = ", vardims(i), ")"
        if (present(error_handle)) then
          call error_handle(ERROR_SPEC, ERROR_VAR, varname, me, err)
        end if
        deallocate(ncdimids)
        return
      end if
    end do
    deallocate(ncdimids)
    ierr = .true.
  end subroutine etsf_io_low_check_var
  
!  subroutine etsf_io_low_get_var_integer(ncid, ncvarid, var, vardim, ierr, error_handle)
!    integer, intent(in)   :: ncid
!    integer, intent(in)   :: ncvarid
!    integer, intent(in)   :: vardim
!    integer, intent(out)  :: var(1:vardim)
!    logical, intent(out)  :: ierr
!    optional              :: error_handle
!
!    !Local
!    character(len = *), parameter :: me = "etsf_io_low_get_var_integer"
!    integer :: s
!
!    ierr = .false.    
!    s = nf90_get_var(ncid, ncvarid, values = var)
!    if (s /= nf90_noerr) then
!      if (present(error_handle)) then
!        call error_handle(ERROR_GET, ERROR_VAR, "name not available", me, nf90_strerror(s))
!      end if
!      return
!    end if
!    ierr = .true.
!  end subroutine etsf_io_low_get_var_integer
!  subroutine etsf_io_low_get_var_double(ncid, ncvarid, var, vardim, ierr, error_handle)
!    integer, intent(in)            :: ncid
!    integer, intent(in)            :: ncvarid
!    integer, intent(in)            :: vardim
!    double precision, intent(out)  :: var(1:vardim)
!    logical, intent(out)           :: ierr
!    optional                       :: error_handle
!
!    !Local
!    character(len = *), parameter :: me = "etsf_io_low_get_var_double"
!    integer :: s
!
!    ierr = .false.    
!    s = nf90_get_var(ncid, ncvarid, values = var)
!    if (s /= nf90_noerr) then
!      if (present(error_handle)) then
!        call error_handle(ERROR_GET, ERROR_VAR, "name not available", me, nf90_strerror(s))
!      end if
!      return
!    end if
!    ierr = .true.
!  end subroutine etsf_io_low_get_var_double
!  subroutine etsf_io_low_get_var_character(ncid, ncvarid, var, vardim, &
!                                          & chardim, ierr, error_handle)
!    integer, intent(in)                    :: ncid
!    integer, intent(in)                    :: ncvarid
!    integer, intent(in)                    :: vardim, chardim
!    character(len = chardim), intent(out)  :: var(1:vardim)
!    logical, intent(out)                   :: ierr
!    optional                               :: error_handle
!
!    !Local
!    character(len = *), parameter :: me = "etsf_io_low_get_var_character"
!    integer :: s
!
!    ierr = .false.    
!    s = nf90_get_var(ncid, ncvarid, values = var)
!    if (s /= nf90_noerr) then
!      if (present(error_handle)) then
!        call error_handle(ERROR_GET, ERROR_VAR, "name not available", me, nf90_strerror(s))
!      end if
!      return
!    end if
!    ierr = .true.
!  end subroutine etsf_io_low_get_var_character
  
  subroutine etsf_io_low_check_att(ncid, ncvarid, attname, atttype, attlen, ierr, error_handle)
    integer, intent(in)            :: ncid
    integer, intent(in)            :: ncvarid
    character(len = *), intent(in) :: attname
    integer, intent(in)            :: atttype, attlen
    logical, intent(out)           :: ierr
    optional                       :: error_handle
    
    !Local
    character(len = *), parameter :: me = "etsf_io_low_check_att"
    character(len = 80) :: err
    integer :: s, nctype, nclen

    ierr = .false.    
    s = nf90_inquire_attribute(ncid, ncvarid, attname, xtype = nctype, len = nclen) 
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_INQ, ERROR_ATT, attname, me, nf90_strerror(s))
      end if
      return
    end if
    ! Check the type
    if (nctype /= atttype) then
      write(err, "(A,I5,A,I5,A)") "wrong type (read = ", nctype, &
                                & ", awaited = ", atttype, ")"
      if (present(error_handle)) then
        call error_handle(ERROR_SPEC, ERROR_ATT, attname, me, err)
      end if
      return
    end if
    ! Check the dimensions
    if ((atttype == NF90_CHAR .and. nclen > attlen) .or. &
      & (atttype /= NF90_CHAR .and. nclen /= attlen)) then
      write(err, "(A,I5,A,I5,A)") "wrong length (read = ", nclen, &
                                & ", awaited = ", attlen, ")"
      if (present(error_handle)) then
        call error_handle(ERROR_SPEC, ERROR_ATT, attname, me, err)
      end if
      return
    end if
    ierr = .true.
  end subroutine etsf_io_low_check_att
  
  subroutine etsf_io_low_open_read(ncid, filename, ierr, version_min, error_handle)
    integer, intent(out)           :: ncid
    character(len = *), intent(in) :: filename
    real, intent(in), optional     :: version_min
    logical, intent(out)           :: ierr
    optional                       :: error_handle

    !Local
    character(len = *), parameter :: me = "etsf_io_low_open_read"
    character(len = 80) :: err, format
    integer :: s, nctype, nclen
    real :: version_real
    double precision :: version_double
    logical :: stat
    
    ierr = .false.
    ! Open file for reading
    s = nf90_open(path = filename, mode = NF90_NOWRITE, ncid = ncid)
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_IO, ERROR_ORD, filename, me, nf90_strerror(s))
      end if
      return
    end if
    ! From now on the file is open. If an error occur,
    ! we should close it.
    
    ! Check the header
    if (present(error_handle)) then
      call etsf_io_low_check_att(ncid, NF90_GLOBAL, "file_format", &
                               & NF90_CHAR, 80, stat, error_handle) 
    else
      call etsf_io_low_check_att(ncid, NF90_GLOBAL, "file_format", NF90_CHAR, 80, stat) 
    end if
    if (.not. stat) then
      call etsf_io_low_close(ncid, stat)
      return
    end if
    write(format, "(A80)") " "
    s = nf90_get_att(ncid, NF90_GLOBAL, "file_format", format)
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_GET, ERROR_ATT, "file_format", me, nf90_strerror(s))
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    if (trim(adjustl(format)) /= "ETSF Nanoquanta") then
      write(err, "(A,A,A)") "wrong value: '", trim(adjustl(format(1:60))), "'"
      if (present(error_handle)) then
        call error_handle(ERROR_SPEC, ERROR_ATT, "file_format", me, err)
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! Check the version
    version_real = 1.
    call etsf_io_low_check_att(ncid, NF90_GLOBAL, "file_format_version", NF90_FLOAT, 1, stat) 
    if (.not. stat) then
      call etsf_io_low_check_att(ncid, NF90_GLOBAL, "file_format_version", NF90_DOUBLE, 1, stat)
      version_real = 0.
      if (.not. stat) then
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    if (version_real == 1.) then
      s = nf90_get_att(ncid, NF90_GLOBAL, "file_format_version", version_real)
    else
      s = nf90_get_att(ncid, NF90_GLOBAL, "file_format_version", version_double)
      version_real = real(version_double)
    end if
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_GET, ERROR_ATT, "file_format_version", me, nf90_strerror(s))
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    if (present(version_min)) then
      stat = (version_real >= version_min)
    else
      stat = (version_real >= 1.3)
    end if
    if (.not. stat) then
      write(err, "(A)") "wrong value"
      if (present(error_handle)) then
        call error_handle(ERROR_SPEC, ERROR_ATT, "file_format_version", me, err)
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! Check for the Conventions flag
    if (present(error_handle)) then
      call etsf_io_low_check_att(ncid, NF90_GLOBAL, "Conventions", &
                               & NF90_CHAR, 80, stat, error_handle) 
    else
      call etsf_io_low_check_att(ncid, NF90_GLOBAL, "Conventions", NF90_CHAR, 80, stat) 
    end if
    if (.not. stat) then
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ierr = .true.
  end subroutine etsf_io_low_open_read
  
  subroutine etsf_io_low_close(ncid, ierr, error_handle)
    integer, intent(in) :: ncid
    logical, intent(out)           :: ierr
    optional                       :: error_handle

    !Local
    character(len = *), parameter :: me = "etsf_io_low_close"
    integer :: s
    
    ierr = .false.
    ! Close file
    s = nf90_close(ncid)
    if (s /= nf90_noerr) then
      if (present(error_handle)) then
        call error_handle(ERROR_IO, ERROR_CLO, "no name available", me, nf90_strerror(s))
      end if
      return
    end if
    ierr = .true.
  end subroutine etsf_io_low_close

end module etsf_io_low_level
