  !!****m* etsf_io_low_read_group/etsf_io_low_read_dim
  !! NAME
  !!  etsf_io_low_read_dim
  !!
  !! FUNCTION
  !!  This method is a wraper to get in one call the id of one dimension
  !!  and its value.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * dimname = a string identifying a dimension.
  !!
  !! OUTPUT
  !!  * dimvalue = a positive integer which is the length of the dimension.
  !!  * lstat = .true. if operation succeed.
  !!  * ncdimid = (optional) the id used by NetCDF to identify the read dimension.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_read_dim(ncid, dimname, dimvalue, lstat, ncdimid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: dimname
    integer, intent(out)                           :: dimvalue
    logical, intent(out)                           :: lstat
    integer, intent(out), optional                 :: ncdimid
    type(etsf_io_low_error), intent(out), optional :: error_data

    !local
    character(len = *), parameter :: me = "etsf_io_low_read_dim"
    integer :: s, dimid

    lstat = .false.
    ! will inq_dimid() and inq_dimlen() + error handling
    s = nf90_inq_dimid(ncid, dimname, dimid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_DID, me, tgtname = dimname, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    s = nf90_inquire_dimension(ncid, dimid, len = dimvalue)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_DIM, me, tgtname = dimname, &
                     & tgtid = dimid, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    if (present(ncdimid)) then
      ncdimid = dimid
    end if
    lstat = .true.    
  end subroutine etsf_io_low_read_dim
  !!***
  
  !!****m* etsf_io_low_read_group/etsf_io_low_read_var_infos
  !! NAME
  !!  etsf_io_low_read_var_infos
  !!
  !! FUNCTION
  !!  This method is used to retrieve informations about a variable:
  !!  * its NetCDF id ;
  !!  * its type (see #ETSF_IO_LOW_CONSTANTS) ;
  !!  * its shape and length for each dimension.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * varname = a string identifying a variable.
  !!
  !! OUTPUT
  !!  * var_infos <type(etsf_io_low_var_infos)> = store, type, shape, dimensions and NetCDF id.
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_read_var_infos(ncid, varname, var_infos, lstat, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    type(etsf_io_low_var_infos), intent(out)       :: var_infos
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_read_var_infos"
    integer :: i, s
    integer, allocatable :: ncdimids(:)
    
    lstat = .false.
    var_infos%name = varname(1:min(80, len(varname)))
    ! will inq_varid()
    s = nf90_inq_varid(ncid, varname, var_infos%ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VID, me, tgtname = varname, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    ! will inq_vartype()
    ! will inq_varndims()
    s = nf90_inquire_variable(ncid, var_infos%ncid, xtype = var_infos%nctype, &
                            & ndims = var_infos%ncshape)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VAR, me, tgtname = varname, &
                     & tgtid = var_infos%ncid, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    if (var_infos%ncshape > 0) then
      ! will inq_vardimid()
      allocate(ncdimids(1:var_infos%ncshape))
      s = nf90_inquire_variable(ncid, var_infos%ncid, dimids = ncdimids)
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VAR, me, tgtname = varname, &
                       & tgtid = var_infos%ncid, errid = s, errmess = nf90_strerror(s))
        end if
        deallocate(ncdimids)
        return
      end if
      do i = 1, var_infos%ncshape, 1
        ! will inq_dimlen()
        s = nf90_inquire_dimension(ncid, ncdimids(i), len = var_infos%ncdims(i))
        if (s /= nf90_noerr) then
          if (present(error_data)) then
            call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_DIM, me, &
                         & tgtid = ncdimids(i), errid = s, errmess = nf90_strerror(s))
          end if
          deallocate(ncdimids)
          return
        end if
      end do
      deallocate(ncdimids)
    end if
    lstat = .true.
  end subroutine etsf_io_low_read_var_infos
  !!***
  
  !!****m* etsf_io_low_check_group/etsf_io_low_check_var
  !! NAME
  !!  etsf_io_low_check_var
  !!
  !! FUNCTION
  !!  This method is used to compare the informations (type, shape...) of two
  !!  given variables. It returns .true. if the variables are compatible (data
  !!  from one can be transfered to the other). It can also say if the match 
  !!  is perfect or if the transfer requires convertion (type or shape).
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * var_ref <type(etsf_io_low_var_infos)> = store, type, shape, dimensions and NetCDF id.
  !!  * var <type(etsf_io_low_var_infos)> = store, type, shape, dimensions and NetCDF id.
  !!  * sub = (optional) restrict the check to the lower dimensions (0 < sub <= var_ref%ncshape).
  !!
  !! OUTPUT
  !!  * lstat = .true. if the two variable definitions are compatible.
  !!  * level = (optional) when variables are compatibles (lstat = .true.),
  !!            this flag gives information on matching (see #FLAGS_MATCHING).
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_check_var(var_ref, var, lstat, sub, level, error_data)
    type(etsf_io_low_var_infos), intent(in)        :: var_ref
    type(etsf_io_low_var_infos), intent(in)        :: var
    logical, intent(out)                           :: lstat
    integer, intent(in), optional                  :: sub
    integer, intent(out), optional                 :: level
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_check_var"
    character(len = 80) :: err
    integer :: i, s, lvl, nb_ele_ref, nb_ele, sub_shape
    integer :: nclendims(1:7)
    logical :: stat
    
    lstat = .false.
    lvl = etsf_io_low_var_match
    ! Check the type, if both numeric or both strings, vars are compatible.
    if ((var_ref%nctype == NF90_CHAR .and. var%nctype /= NF90_CHAR) .or. &
      & (var_ref%nctype /= NF90_CHAR .and. var%nctype == NF90_CHAR)) then
      write(err, "(A)") "incompatible type, both must be either numeric or character."
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
                     & errmess = err)
      end if
      return
    end if
    if (var_ref%nctype /= var%nctype) then
      lvl = lvl + etsf_io_low_var_type_dif
    end if

    ! The sub argument is used to restrain check to lower indices
    ! in the shape.
    if (present(sub)) then
      sub_shape = sub
    else
      sub_shape = var_ref%ncshape
    end if
    if (.not.((var_ref%ncshape > 0 .and. sub_shape > 0 .and. sub_shape <= var_ref%ncshape) .or. &
            & (var_ref%ncshape == 0 .and. sub_shape == 0))) then
      write(err, "(A)") "wrong sub argument ( 0 < sub <= var_ref%ncshape)."
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                     & tgtname = "sub", errmess = err)
      end if
      return
    end if
    
    ! Check the shape
    if (var_ref%ncshape == var%ncshape) then
      ! In the case when shapes are identical, one must check all dimensions
      do i = 1, sub_shape, 1
        ! Test the dimensions
        if (var_ref%ncdims(i) /= var%ncdims(i)) then
          write(err, "(A,I0,A,I5,A,I5,A)") "wrong dimension length for index ", i, &
                                          & " (var_from = ", var_ref%ncdims(i), &
                                          & ", var_to = ", var%ncdims(i), ")"
          if (present(error_data)) then
            call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
                         & errmess = err)
          end if
          return
        end if
      end do
    else
      ! The argument has a different shape that the store variable.
      ! We check the compatibility, product(var_to%ncdims) == product(var_from%ncdims)
      lvl = lvl + etsf_io_low_var_shape_dif
      if (var_ref%ncshape == 0) then
        nb_ele_ref = 1
      else
        nb_ele_ref = product(var_ref%ncdims(1:sub_shape))
      end if
      if (var%ncshape == 0) then
        nb_ele = 1
      else
        nb_ele = product(var%ncdims(1:var%ncshape))
      end if
      if (nb_ele_ref /= nb_ele) then
        write(err, "(A,I5,A,I5,A)") "incompatible number of data (var_ref = ", &
                                  & nb_ele_ref, ", var = ", nb_ele, ")"
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
                       & errmess = err)
        end if
        return
      end if
    end if
    if (present(level)) then
      level = lvl
    end if
    lstat = .true.
  end subroutine etsf_io_low_check_var
  !!***
  
  !!****m* etsf_io_low_check_group/etsf_io_low_check_att
  !! NAME
  !!  etsf_io_low_check_att
  !!
  !! FUNCTION
  !!  This method is used to check that an attribute:
  !!  * exists in the read NetCDF file ;
  !!  * has the right type ;
  !!  * has the right length (1 for scalar, > 1 for arrays).
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * ncvarid = the id of the variable the attribute is attached to.
  !!              in the case of global attributes, use the constance
  !!              NF90_GLOBAL (when linking against NetCDF) or #etsf_io_low_global_att
  !!              which is a wrapper exported by this module (see #ETSF_IO_LOW_CONSTANTS).
  !!  * attname = a string identifying an attribute.
  !!  * atttype = an integer identifying the type (see #ETSF_IO_LOW_CONSTANTS).
  !!  * attlen = the size of the array, or 1 when the attribute is a scalar.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! ERRORS
  !!  * ERROR_MODE_INQ & ERROR_TYPE_ATT: when the attribute doesn't exist.
  !!  * ERROR_MODE_SPEC & ERROR_TYPE_ATT: when the attribute has a wrong type or dimension.
  !!
  !! SOURCE
  subroutine etsf_io_low_check_att(ncid, ncvarid, attname, atttype, attlen, lstat, error_data)
    integer, intent(in)                            :: ncid
    integer, intent(in)                            :: ncvarid
    character(len = *), intent(in)                 :: attname
    integer, intent(in)                            :: atttype, attlen
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data
    
    !Local
    character(len = *), parameter :: me = "etsf_io_low_check_att"
    character(len = 80) :: err
    integer :: s, nctype, nclen

    lstat = .false.    
    s = nf90_inquire_attribute(ncid, ncvarid, attname, xtype = nctype, len = nclen) 
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_ATT, me, tgtname = attname, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    ! Check the type
    if (nctype /= atttype) then
      write(err, "(A,I5,A,I5,A)") "wrong type (read = ", nctype, &
                                & ", awaited = ", atttype, ")"
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, me, tgtname = attname, &
                     & errmess = err)
      end if
      return
    end if
    ! Check the dimensions
    if ((atttype == NF90_CHAR .and. nclen > attlen) .or. &
      & (atttype /= NF90_CHAR .and. nclen /= attlen)) then
      write(err, "(A,I5,A,I5,A)") "wrong length (read = ", nclen, &
                                & ", awaited = ", attlen, ")"
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, me, tgtname = attname, &
                     & errmess = err)
      end if
      return
    end if
    lstat = .true.
  end subroutine etsf_io_low_check_att
  !!***
  
  !!****m* etsf_io_low_check_group/etsf_io_low_check_header
  !! NAME
  !!  etsf_io_low_check_header
  !!
  !! FUNCTION
  !!  This method is specific to ETSF files. It checks if the header is
  !!  conform to the specifications, which means having the right "file_format"
  !!  attribute, the right "file_format_version" one and also an attribute named
  !!  "Conventions". Moreover, the routine can do a check on the value of the
  !!  file_format_version to ensure high enough value.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * version_min = (optional) the number of minimal version to be read.
  !                   when not specified, 1.3 is the minimum value by default.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_check_header(ncid, lstat, version_min, error_data)
    integer, intent(in)                            :: ncid
    logical, intent(out)                           :: lstat
    real, intent(in), optional                     :: version_min
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_check_header"
    character(len = 80) :: err, format
    integer :: s
    real :: version_real
    logical :: stat

    lstat = .false.
    ! Check the header
    write(format, "(A80)") " "
    if (present(error_data)) then
      call etsf_io_low_read_att(ncid, NF90_GLOBAL, "file_format", 80, format, stat, error_data) 
    else
      call etsf_io_low_read_att(ncid, NF90_GLOBAL, "file_format", 80, format, stat) 
    end if
    if (.not. stat) then
      call etsf_io_low_close(ncid, stat)
      return
    end if
    if (trim(adjustl(format)) /= "ETSF Nanoquanta") then
      write(err, "(A,A,A)") "wrong value: '", trim(adjustl(format(1:60))), "'"
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, me, tgtname = "file_format", &
                     & errmess = err)
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! Check the version
    if (present(error_data)) then
      call etsf_io_low_read_att(ncid, NF90_GLOBAL, "file_format_version", &
                              & version_real, stat, error_data) 
    else
      call etsf_io_low_read_att(ncid, NF90_GLOBAL, "file_format_version", &
                              & version_real, stat)
    end if
    if (.not. stat) then
      call etsf_io_low_close(ncid, stat)
      return
    end if
    if (present(version_min)) then
      stat = (version_real >= version_min)
    else
      stat = (version_real >= 1.3)
    end if
    if (.not. stat) then
      write(err, "(A,F10.5)") "wrong value: ", version_real
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, me, tgtname = "file_format_version", &
                     & errmess = err)
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! Check for the Conventions flag
    if (present(error_data)) then
      call etsf_io_low_check_att(ncid, NF90_GLOBAL, "Conventions", &
                               & NF90_CHAR, 80, stat, error_data) 
    else
      call etsf_io_low_check_att(ncid, NF90_GLOBAL, "Conventions", NF90_CHAR, 80, stat) 
    end if
    if (.not. stat) then
      call etsf_io_low_close(ncid, stat)
      return
    end if
    lstat = .true.
  end subroutine etsf_io_low_check_header
  !!***
  
  !!****m* etsf_io_low_file_group/etsf_io_low_open_read
  !! NAME
  !!  etsf_io_low_open_read
  !!
  !! FUNCTION
  !!  This method is used to open a NetCDF file with read access only. Moreover,
  !!  a check on the header is done to verify that the file is conformed to
  !!  specifications (see etsf_io_low_check_header()).
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006
  !!  This file is distributed under the terms of the
  !!  GNU General Public License, see ~abinit/COPYING
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * filename = the path to the file to open.
  !!
  !! OUTPUT
  !!  * ncid = the NetCDF handler, opened with read access.
  !!  * lstat = .true. if operation succeed.
  !!  * version_min = (optional) the number of minimal version to be read.
  !                   when not specified, 1.3 is the minimum value by default.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_open_read(ncid, filename, lstat, version_min, error_data)
    integer, intent(out)                           :: ncid
    character(len = *), intent(in)                 :: filename
    logical, intent(out)                           :: lstat
    real, intent(in), optional                     :: version_min
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_open_read"
    integer :: s
    
    lstat = .false.
    ! Open file for reading
    s = nf90_open(path = filename, mode = NF90_NOWRITE, ncid = ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_IO, ERROR_TYPE_ORD, me, tgtname = filename, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    ! From now on the file is open. If an error occur,
    ! we should close it.

    if (present(error_data)) then
      if (present(version_min)) then
        call etsf_io_low_check_header(ncid, lstat, version_min, error_data)
      else
        call etsf_io_low_check_header(ncid, lstat, error_data = error_data)
      end if
    else
      if (present(version_min)) then
        call etsf_io_low_check_header(ncid, lstat, version_min = version_min)
      else
        call etsf_io_low_check_header(ncid, lstat)
      end if
    end if
  end subroutine etsf_io_low_open_read
  !!***

  ! Generic routine, documented in the module file.
  subroutine read_var_double_var(ncid, varname, var, lstat, sub, ncvarid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    type(etsf_io_low_var_double), intent(inout)      :: var
    logical, intent(out)                           :: lstat
    integer, intent(in), optional                  :: sub(:)
    integer, intent(out), optional                 :: ncvarid
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "read_var_double_var"
    character(len = 80) :: err
    integer :: varid
    type(etsf_io_low_error) :: error
    
    if (associated(var%data1D)) then
      if (present(sub)) then
        call read_var_double_1D(ncid, varname, var%data1D, lstat, &
                              & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_double_1D(ncid, varname, var%data1D, lstat, &
                              & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data2D)) then
      if (present(sub)) then
        call read_var_double_2D(ncid, varname, var%data2D, lstat, &
                              & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_double_2D(ncid, varname, var%data2D, lstat, &
                              & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data3D)) then
      if (present(sub)) then
        call read_var_double_3D(ncid, varname, var%data3D, lstat, &
                              & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_double_3D(ncid, varname, var%data3D, lstat, &
                              & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data4D)) then
      if (present(sub)) then
        call read_var_double_4D(ncid, varname, var%data4D, lstat, &
                              & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_double_4D(ncid, varname, var%data4D, lstat, &
                              & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data5D)) then
      if (present(sub)) then
        call read_var_double_5D(ncid, varname, var%data5D, lstat, &
                              & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_double_5D(ncid, varname, var%data5D, lstat, &
                              & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data6D)) then
      if (present(sub)) then
        call read_var_double_6D(ncid, varname, var%data6D, lstat, &
                              & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_double_6D(ncid, varname, var%data6D, lstat, &
                              & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data7D)) then
      if (present(sub)) then
        call read_var_double_7D(ncid, varname, var%data7D, lstat, &
                              & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_double_7D(ncid, varname, var%data7D, lstat, &
                              & ncvarid = varid, error_data = error)
      end if
    else
      write(err, "(A,F10.5)") "no data array associated"
      call etsf_io_low_error_set(error, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                   & tgtname = "var", errmess = err)
      lstat = .false.
    end if
    if (present(error_data)) then
      error_data = error
    end if
    if (present(ncvarid)) then
      ncvarid = varid
    end if
  end subroutine read_var_double_var
  
  ! Generic routine, documented in the module file.
  subroutine read_var_integer_var(ncid, varname, var, lstat, sub, ncvarid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    type(etsf_io_low_var_integer), intent(inout)   :: var
    logical, intent(out)                           :: lstat
    integer, intent(in), optional                  :: sub(:)
    integer, intent(out), optional                 :: ncvarid
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "read_var_integer_var"
    character(len = 80) :: err
    integer :: varid
    type(etsf_io_low_error) :: error
    
    if (associated(var%data1D)) then
      if (present(sub)) then
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data2D)) then
      if (present(sub)) then
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data3D)) then
      if (present(sub)) then
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data4D)) then
      if (present(sub)) then
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data5D)) then
      if (present(sub)) then
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data6D)) then
      if (present(sub)) then
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data7D)) then
      if (present(sub)) then
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & sub = sub, ncvarid = varid, error_data = error)
      else
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else
      write(err, "(A,F10.5)") "no data array associated"
      call etsf_io_low_error_set(error, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                   & tgtname = "var", errmess = err)
      lstat = .false.
    end if
    if (present(error_data)) then
      error_data = error
    end if
    if (present(ncvarid)) then
      ncvarid = varid
    end if
  end subroutine read_var_integer_var
