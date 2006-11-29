  !!****m* etsf_io_low_file_group/etsf_io_low_open_create
  !! NAME
  !!  etsf_io_low_open_create
  !!
  !! FUNCTION
  !!  This method is used to open a NetCDF file. The file should not already exist.
  !!  The ETSF header for the created file is automatically added by this method.
  !!  When finished, the file handled by @ncid, is in define mode, which means
  !!  that dimensions (see etsf_io_low_write_dim()), variables (see
  !!  etsf_io_low_def_var()) and attributes (see etsf_io_low_write_att()) can be defined.
  !!  To use etsf_io_low_write_var(), the file should be switched to data mode using
  !!  etsf_io_low_set_write_mode().
  !!
  !!  If title or history are given and are too long, they will be truncated.
  !!
  !!  If one wants to modify an already existing file, one should use
  !!  etsf_io_low_open_modify() instead.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/gpl.txt .
  !!
  !! INPUTS
  !!  * filename = the path to the file to open.
  !!  * version = the number of version to be created.
  !!  * title = (optional) a title for the file (80 characters max).
  !!  * history = (optional) the first line of history (1024 characters max).
  !!
  !! OUTPUT
  !!  * ncid = the NetCDF handler, opened with write access (define mode).
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
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
    character(len = 256) :: err
    integer :: s
    logical :: stat
    
    lstat = .false.
    ! Checking that @version argument is valid.
    if (version < 1.0) then
      if (present(error_data)) then
        write(err, "(A,I0,A)") "Wrong version argument (given: ", version, " ; awaited >= 1.0)"
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, me, &
                     & tgtname = "file_format_version", errmess = err)
      end if
      return
    end if
    ! Open file for writing
    ! We don't use the 64bits flag since the specifications advice
    ! to split huge quantities of data into several smaller files.
    s = nf90_create(path = filename, cmode = NF90_NOCLOBBER, ncid = ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_IO, ERROR_TYPE_OWR, me, tgtname = filename, &
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
        call etsf_io_low_error_set(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "file_format", &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! The version
    s = nf90_put_att(ncid, NF90_GLOBAL, "file_format_version", version)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, &
                     & tgtname = "file_format_version", errid = s, errmess = nf90_strerror(s))
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! The conventions
    s = nf90_put_att(ncid, NF90_GLOBAL, "Conventions", etsf_io_low_conventions)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "Conventions", &
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
          call etsf_io_low_error_set(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "title", &
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
          call etsf_io_low_error_set(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "history", &
                       & errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    
    lstat = .true.
  end subroutine etsf_io_low_open_create
  !!***
    
  !!****m* etsf_io_low_file_group/etsf_io_low_open_modify
  !! NAME
  !!  etsf_io_low_open_modify
  !!
  !! FUNCTION
  !!  This method is used to open a NetCDF file for modifications. The file should
  !!  already exist and have a valid ETSF header.
  !!
  !!  When finished, the file handled by @ncid, is in define mode, which means
  !!  that dimensions (see etsf_io_low_write_dim()), variables (see
  !!  etsf_io_low_def_var()) and attributes (see etsf_io_low_write_att()) can be defined.
  !!  To use etsf_io_low_write_var(), the file should be switched to data mode using
  !!  etsf_io_low_set_write_mode().
  !!
  !!  If title or history are given and are too long, they will be truncated. Moreover
  !!  the given history is appended to the already existing history (if enough
  !!  place exists).
  !!
  !!  If one wants to create a new file, one should use etsf_io_low_open_create() instead.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/gpl.txt .
  !!
  !! INPUTS
  !!  * filename = the path to the file to open.
  !!  * title = (optional) a title for the file (80 characters max).
  !!  * history = (optional) the first line of history (1024 characters max).
  !!  * version = (optional) the number of version to be changed (>= 1.0).
  !!
  !! OUTPUT
  !!  * ncid = the NetCDF handler, opened with write access (define mode).
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
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
    character(len = 256) :: err
    character(len = 1024) :: current_history
    integer :: s
    logical :: stat
    
    lstat = .false.
    ! Checking that @version argument is valid.
    if (present(version)) then
      if (version < 1.0) then
        if (present(error_data)) then
          write(err, "(A,I0,A)") "Wrong version argument (given: ", version, " ; awaited >= 1.0)"
          call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, me, &
                       & tgtname = "file_format_version", errmess = err)
        end if
        return
      end if
    end if
    ! Open file for writing
    s = nf90_open(path = filename, mode = NF90_WRITE, ncid = ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_IO, ERROR_TYPE_OWR, me, tgtname = filename, &
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

    ! We switch to define mode to set attributes.
    if (present(error_data)) then
      call etsf_io_low_set_define_mode(ncid, stat, error_data = error_data)
    else
      call etsf_io_low_set_define_mode(ncid, stat)
    end if
    if (.not. stat) then
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! We switch to define mode to set attributes.
    if (present(error_data)) then
      call etsf_io_low_set_define_mode(ncid, stat, error_data = error_data)
    else
      call etsf_io_low_set_define_mode(ncid, stat)
    end if
    if (.not. stat) then
      call etsf_io_low_close(ncid, stat)
      return
    end if

    ! If a title is given, we change it.
    if (present(title)) then
      s = nf90_put_att(ncid, NF90_GLOBAL, "title", title(1:min(80, len(title))))
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, tgtname = "title", &
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
          call etsf_io_low_error_set(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, &
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
          call etsf_io_low_error_set(error_data, ERROR_MODE_PUT, ERROR_TYPE_ATT, me, &
                       & tgtname = "history", &
                       & errid = s, errmess = nf90_strerror(s))
        end if
        call etsf_io_low_close(ncid, stat)
        return
      end if
    end if
    
    lstat = .true.
  end subroutine etsf_io_low_open_modify
  !!***
  
  !!****m* etsf_io_low_write_group/etsf_io_low_write_dim
  !! NAME
  !!  etsf_io_low_write_dim
  !!
  !! FUNCTION
  !!  This method is a wraper add a dimension to a NetCDF file. As in pure NetCDF
  !!  calls, overwriting a value is not permitted. Nevertheless, the method returns
  !!  .true. in @lstat, if the dimension already exists and has the same value.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/gpl.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with write access (define mode).
  !!  * dimname = a string identifying a dimension.
  !!  * dimvalue = a positive integer which is the length of the dimension.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * ncdimid = (optional) the id used by NetCDF to identify the written dimension.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_write_dim(ncid, dimname, dimvalue, lstat, ncdimid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: dimname
    integer, intent(in)                            :: dimvalue
    logical, intent(out)                           :: lstat
    integer, intent(out), optional                 :: ncdimid
    type(etsf_io_low_error), intent(out), optional :: error_data
    
    ! Local
    character(len = *), parameter :: me = "etsf_io_low_write_dim"
    character(len = 500) :: message
    integer :: s, dimid, readvalue
    
    ! Check if dimension already exists.
    call etsf_io_low_read_dim(ncid, dimname, readvalue, lstat)
    if (lstat) then
      ! Dimension already exists.
      if (readvalue /= dimvalue) then
        ! Dimension differs, raise error.
        if (present(error_data)) then
          write(message, "(2A,I0,A,I0,A)") "dimension already exists with a different", &
                                         & " value (read = ", readvalue, " ; write = ", &
                                         & dimvalue, ")."
          call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_DIM, me, &
                                   & tgtname = dimname, errmess = message)
        end if
        lstat = .false.
        return
      else
        ! Dimension matches, return.
        return
      end if        
    end if
    ! Define dimension since it don't already exist.
    lstat = .false.
    s = nf90_def_dim(ncid, dimname, dimvalue, dimid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_DIM, me, &
                                 & tgtname = dimname, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    if (present(ncdimid)) then
      ncdimid = dimid
    end if    
    lstat = .true.
  end subroutine etsf_io_low_write_dim
  !!***
  
  ! Interfaced routine:
  ! See etsf_io_low_level.f90 for documentation
  subroutine etsf_io_low_def_var_0D(ncid, varname, vartype, lstat, ncvarid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    integer, intent(in)                            :: vartype
    logical, intent(out)                           :: lstat
    integer, intent(out), optional                 :: ncvarid
    type(etsf_io_low_error), intent(out), optional :: error_data
    
    ! Local
    character(len = *), parameter :: me = "etsf_io_low_def_var"
    type(etsf_io_low_var_infos) :: var_infos
    integer :: s, varid

    ! Check if dimension already exists.
    call etsf_io_low_read_var_infos(ncid, varname, var_infos, lstat)
    if (lstat) then
      ! Variable already exists.
      lstat = (var_infos%nctype == vartype .and. var_infos%ncshape == 0)
      if (.not. lstat) then
        ! Variable differs, raise error.
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_VAR, me, &
                                   & tgtname = varname, errmess = &
                                   & "variable already exists with a different definition.")
        end if
        lstat = .false.
        return
      else
        ! Dimension matches, return.
        return
      end if        
    end if
    ! Define variable since it don't already exist.
    lstat = .false.    
    ! Special case where dimension is null
    s = nf90_def_var(ncid, varname, vartype, varid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_VAR, me, &
                      & tgtname = varname, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    if (present(ncvarid)) then
      ncvarid = varid
    end if    
    lstat = .true.
  end subroutine etsf_io_low_def_var_0D
  
  ! Interfaced routine:
  ! See etsf_io_low_level.f90 for documentation
  subroutine etsf_io_low_def_var_nD(ncid, varname, vartype, vardims, lstat, ncvarid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    integer, intent(in)                            :: vartype
    character(len = *), intent(in)                 :: vardims(:)
    logical, intent(out)                           :: lstat
    integer, intent(out), optional                 :: ncvarid
    type(etsf_io_low_error), intent(out), optional :: error_data
    
    ! Local
    character(len = *), parameter :: me = "etsf_io_low_def_var"
    type(etsf_io_low_var_infos) :: var_infos
    integer :: s, varid, ndims, i
    integer, allocatable :: ncdims(:, :)
    logical :: stat

    lstat = .false.
    ! The dimension are given by their names, we must first fetch them.
    ndims = size(vardims)
    allocate(ncdims(0:1, 1:ndims))
    do i = 1, ndims, 1
      if (present(error_data)) then
        call etsf_io_low_read_dim(ncid, trim(vardims(i)), ncdims(0, i), &
                                & stat, ncdimid = ncdims(1, i), error_data = error_data)
      else
        call etsf_io_low_read_dim(ncid, trim(vardims(i)), ncdims(0, i), &
                                & stat, ncdimid = ncdims(1, i))
      end if
      if (.not. stat) then
        deallocate(ncdims)
        return
      end if
    end do
    ! Check if dimension already exists.
    call etsf_io_low_read_var_infos(ncid, varname, var_infos, lstat)
    if (lstat) then
      ! Variable already exists.
      lstat = (var_infos%nctype == vartype .and. var_infos%ncshape == ndims)
      do i = 1, min(var_infos%ncshape, ndims), 1
        lstat = lstat .and. (ncdims(0, i) == var_infos%ncdims(i))
      end do
      if (.not. lstat) then
        ! Variable differs, raise error.
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_VAR, me, &
                                   & tgtname = varname, errmess = &
                                   & "variable already exists with a different definition.")
        end if
      end if        
      return
    end if
    ! Define variable since it don't already exist.
    s = nf90_def_var(ncid, varname, vartype, ncdims(1, :), varid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_VAR, me, &
                      & tgtname = varname, errid = s, errmess = nf90_strerror(s))
      end if
      deallocate(ncdims)
      return
    end if
    deallocate(ncdims)
    if (present(ncvarid)) then
      ncvarid = varid
    end if    
    lstat = .true.
  end subroutine etsf_io_low_def_var_nD
  
  ! Generic routine, documented in the module file.
  subroutine write_var_double_var(ncid, varname, var, lstat, &
                                & start, count, map, ncvarid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    type(etsf_io_low_var_double), intent(in)       :: var
    logical, intent(out)                           :: lstat
    integer, intent(in), optional                  :: start(:), count(:), map(:)
    integer, intent(out), optional                 :: ncvarid
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "write_var_double_var"
    integer :: varid
    type(etsf_io_low_error) :: error
    
    if (associated(var%data1D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_double_1D(ncid, varname, var%data1D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_double_1D(ncid, varname, var%data1D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_double_1D(ncid, varname, var%data1D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data2D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_double_2D(ncid, varname, var%data2D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_double_2D(ncid, varname, var%data2D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_double_2D(ncid, varname, var%data2D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data3D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_double_3D(ncid, varname, var%data3D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_double_3D(ncid, varname, var%data3D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_double_3D(ncid, varname, var%data3D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data4D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_double_4D(ncid, varname, var%data4D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_double_4D(ncid, varname, var%data4D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_double_4D(ncid, varname, var%data4D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data5D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_double_5D(ncid, varname, var%data5D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_double_5D(ncid, varname, var%data5D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_double_5D(ncid, varname, var%data5D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data6D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_double_6D(ncid, varname, var%data6D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_double_6D(ncid, varname, var%data6D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_double_6D(ncid, varname, var%data6D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data7D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_double_7D(ncid, varname, var%data7D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_double_7D(ncid, varname, var%data7D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_double_7D(ncid, varname, var%data7D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else
      call etsf_io_low_error_set(error, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                   & tgtname = "var", errmess = "no data array associated")
      lstat = .false.
    end if
    if (present(error_data)) then
      error_data = error
    end if
    if (present(ncvarid)) then
      ncvarid = varid
    end if
  end subroutine write_var_double_var
  
  ! Generic routine, documented in the module file.
  subroutine write_var_integer_var(ncid, varname, var, lstat, &
                                 & start, count, map, ncvarid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    type(etsf_io_low_var_integer), intent(in)      :: var
    logical, intent(out)                           :: lstat
    integer, intent(in), optional                  :: start(:), count(:), map(:)
    integer, intent(out), optional                 :: ncvarid
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "write_var_integer_var"
    integer :: varid
    type(etsf_io_low_error) :: error
    
    if (associated(var%data1D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_integer_1D(ncid, varname, var%data1D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_integer_1D(ncid, varname, var%data1D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_integer_1D(ncid, varname, var%data1D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data2D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_integer_2D(ncid, varname, var%data2D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_integer_2D(ncid, varname, var%data2D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_integer_2D(ncid, varname, var%data2D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data3D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_integer_3D(ncid, varname, var%data3D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_integer_3D(ncid, varname, var%data3D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_integer_3D(ncid, varname, var%data3D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data4D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_integer_4D(ncid, varname, var%data4D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_integer_4D(ncid, varname, var%data4D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_integer_4D(ncid, varname, var%data4D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data5D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_integer_5D(ncid, varname, var%data5D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_integer_5D(ncid, varname, var%data5D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_integer_5D(ncid, varname, var%data5D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data6D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_integer_6D(ncid, varname, var%data6D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_integer_6D(ncid, varname, var%data6D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_integer_6D(ncid, varname, var%data6D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data7D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call write_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call write_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call write_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call write_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call write_var_integer_7D(ncid, varname, var%data7D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call write_var_integer_7D(ncid, varname, var%data7D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call write_var_integer_7D(ncid, varname, var%data7D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call write_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else
      call etsf_io_low_error_set(error, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                   & tgtname = "var", errmess = "no data array associated")
      lstat = .false.
    end if
    if (present(error_data)) then
      error_data = error
    end if
    if (present(ncvarid)) then
      ncvarid = varid
    end if
  end subroutine write_var_integer_var
