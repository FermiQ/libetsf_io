  subroutine etsf_io_low_open_create(ncid, filename, version, lstat, &
                                   & title, history, error_data)
    integer, intent(out)                           :: ncid
    character(len = *), intent(in)                 :: filename
    real, intent(in)                               :: version
    logical, intent(out)                           :: lstat
    character(len = *), intent(in), optional       :: title
    character(len = *), intent(in), optional       :: history
    type(etsf_io_low_error), intent(out), optional :: error_data
    
    !Local
    character(len = *), parameter :: me = "etsf_io_low_open_create"
    integer :: s
    logical :: stat
    
    lstat = .false.
    ! Open file for writing
    ! We don't use the 64bits flag since the specifications advice
    ! to split huge quantities of data into several smaller files.
    s = nf90_create(path = filename, cmode = NF90_NOCLOBBER, ncid = ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call set_error(error_data, ERROR_MODE_IO, ERROR_TYPE_OWR, me, tgtname = filename, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    ! From now on the file is open. If an error occur,
    ! we should close it.

    ! We create the header.
    ! The file format
    s = nf90_put_att(ncid, NF90_GLOBAL, "file_format", etsf_io_low_file_format)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call set_error(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "file_format", &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! The version
    s = nf90_put_att(ncid, NF90_GLOBAL, "file_format_version", version)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call set_error(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, &
                     & tgtname = "file_format_version", errid = s, errmess = nf90_strerror(s))
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! The conventions
    s = nf90_put_att(ncid, NF90_GLOBAL, "Conventions", etsf_io_low_conventions)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call set_error(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "Conventions", &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! The title if present
    if (present(title)) then
      s = nf90_put_att(ncid, NF90_GLOBAL, "title", title(1:min(80, len(title))))
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call set_error(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "title", &
                       & errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    ! The history if present
    if (present(history)) then
      s = nf90_put_att(ncid, NF90_GLOBAL, "history", history(1:min(1024, len(history))))
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call set_error(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "history", &
                       & errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    
    lstat = .true.
  end subroutine etsf_io_low_open_create
  
  subroutine etsf_io_low_open_modify(ncid, filename, lstat, &
                                   & title, history, version, error_data)
    integer, intent(out)                           :: ncid
    character(len = *), intent(in)                 :: filename
    logical, intent(out)                           :: lstat
    character(len = *), intent(in), optional       :: title
    character(len = *), intent(in), optional       :: history
    real, intent(in), optional                     :: version
    type(etsf_io_low_error), intent(out), optional :: error_data
    
    !Local
    character(len = *), parameter :: me = "etsf_io_low_open_modify"
    character(len = 1024) :: current_history
    integer :: s
    logical :: stat
    
    lstat = .false.
    ! Open file for writing
    s = nf90_open(path = filename, mode = NF90_WRITE, ncid = ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call set_error(error_data, ERROR_MODE_IO, ERROR_TYPE_OWR, me, tgtname = filename, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    ! From now on the file is open. If an error occur,
    ! we should close it.
    
    ! Before according access to modifications, we check
    ! that the header is valid.
    if (present(error_data)) then
      call etsf_io_low_check_header(ncid, stat, error_data = error_data)
    else
      call etsf_io_low_check_header(ncid, stat)
    end if
    if (.not. stat) then
      call etsf_io_low_close(ncid, stat)
      return
    end if

    ! If a modification is necessary, we switch to define mode.
    if (present(title) .or. present(history) .or. present(version)) then
      s = nf90_redef(ncid)
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call set_error(error_data, ERROR_MODE_IO, ERROR_TYPE_DEF, me, tgtid = ncid, &
                       & tgtname = filename, errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    ! If a title is given, we change it.
    if (present(title)) then
      s = nf90_put_att(ncid, NF90_GLOBAL, "title", title(1:min(80, len(title))))
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call set_error(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "title", &
                       & errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    ! If a new version is given, we change it.
    if (present(version)) then
      s = nf90_put_att(ncid, NF90_GLOBAL, "file_format_version", version)
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call set_error(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, &
                       & tgtname = "file_format_version", &
                       & errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    ! If an history value is given, we append it.
    if (present(history)) then
      call etsf_io_low_read_att(ncid, NF90_GLOBAL, "history", 1024, current_history, stat)
      if (stat) then
        ! appending mode
        if (len(trim(current_history)) + len(history) < 1024) then
          current_history = trim(current_history) // char(10) // history
        end if
      else
        ! overwriting mode
        current_history = history
      end if
      s = nf90_put_att(ncid, NF90_GLOBAL, "history", current_history)
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call set_error(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, &
                       & tgtname = "history", &
                       & errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    ! If a modification has been done, we switch back to normal mode.
    if (present(title) .or. present(history) .or. present(version)) then
      s = nf90_enddef(ncid)
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call set_error(error_data, ERROR_MODE_IO, ERROR_TYPE_END, me, tgtid = ncid, &
                       & tgtname = filename, errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    
    lstat = .true.
  end subroutine etsf_io_low_open_modify
