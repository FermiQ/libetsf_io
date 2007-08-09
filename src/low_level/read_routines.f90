  !!****m* etsf_io_low_read_group/etsf_io_low_read_dim
  !! NAME
  !!  etsf_io_low_read_dim
  !!
  !! FUNCTION
  !!  This method is a wraper to get in one call the id of one dimension
  !!  and its value.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
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
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_DID, me, &
             & tgtname = dimname, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    s = nf90_inquire_dimension(ncid, dimid, len = dimvalue)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_DIM, me, &
             & tgtname = dimname, tgtid = dimid, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    if (present(ncdimid)) then
      ncdimid = dimid
    end if
    lstat = .true.    
  end subroutine etsf_io_low_read_dim
  !!***
  
  subroutine read_var_infos_name(ncid, varname, var_infos, lstat, error_data, &
                               & dim_name, att_name)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    type(etsf_io_low_var_infos), intent(out)       :: var_infos
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data
    logical, intent(in), optional                  :: dim_name
    logical, intent(in), optional                  :: att_name

    !Local
    character(len = *), parameter :: me = "read_var_infos_name"
    integer :: s
    logical :: my_dim_name, my_att_name
    
    lstat = .false.
    var_infos%name = varname(1:min(80, len(varname)))
    ! will inq_varid()
    s = nf90_inq_varid(ncid, varname, var_infos%ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VID, me, &
             & tgtname = varname, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    if (present(dim_name)) then
      my_dim_name = dim_name
    else
      my_dim_name = .false.
    end if
    if (present(att_name)) then
      my_att_name = att_name
    else
      my_att_name = .false.
    end if
    if (present(error_data)) then
      call read_var_infos(ncid, var_infos, my_dim_name, my_att_name, &
           & lstat, error_data)
      if (.not. lstat) call etsf_io_low_error_update(error_data, me)
    else
      call read_var_infos(ncid, var_infos, my_dim_name, my_att_name, lstat)
    end if
  end subroutine read_var_infos_name
  subroutine read_var_infos_id(ncid, varid, var_infos, lstat, error_data, &
                               & dim_name, att_name)
    integer, intent(in)                            :: ncid
    integer, intent(in)                            :: varid
    type(etsf_io_low_var_infos), intent(out)       :: var_infos
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data
    logical, intent(in), optional                  :: dim_name
    logical, intent(in), optional                  :: att_name

    !Local
    character(len = *), parameter :: me = "read_var_infos_id"
    integer :: s
    character(len = NF90_MAX_NAME) :: varname
    logical :: my_dim_name, my_att_name
    
    lstat = .false.
    var_infos%ncid = varid
    ! will inq_varid()
    s = nf90_inquire_variable(ncid, var_infos%ncid, name = varname)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VAR, me, &
             & tgtid = varid, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    var_infos%name = varname(1:min(80, len(varname)))
    if (present(dim_name)) then
      my_dim_name = dim_name
    else
      my_dim_name = .false.
    end if
    if (present(att_name)) then
      my_att_name = att_name
    else
      my_att_name = .false.
    end if
    if (present(error_data)) then
      call read_var_infos(ncid, var_infos, my_dim_name, my_att_name, &
           & lstat, error_data)
      if (.not. lstat) call etsf_io_low_error_update(error_data, me)
    else
      call read_var_infos(ncid, var_infos, my_dim_name, my_att_name, lstat)
    end if
  end subroutine read_var_infos_id
  ! Read dimensions..., varid must be set, varname is left untouched.
  subroutine read_var_infos(ncid, var_infos, with_dim_name, with_att_name, &
       & lstat, error_data)
    integer, intent(in)                            :: ncid
    type(etsf_io_low_var_infos), intent(inout)     :: var_infos
    logical, intent(in)                            :: with_dim_name
    logical, intent(in)                            :: with_att_name
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "read_var_infos"
    integer :: i, s, n
    integer, allocatable :: ncdimids(:)
    character(len = NF90_MAX_NAME) :: ncname    
    
    lstat = .false.
    ! will inq_vartype()
    ! will inq_varndims()
    s = nf90_inquire_variable(ncid, var_infos%ncid, xtype = var_infos%nctype, &
                            & ndims = var_infos%ncshape, nAtts = n)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VAR, me, &
                                 & tgtname = var_infos%name, tgtid = var_infos%ncid, &
                                 & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    if (var_infos%ncshape > 0) then
      ! will inq_vardimid()
      allocate(ncdimids(1:var_infos%ncshape))
      if (with_dim_name) then
        allocate(var_infos%ncdimnames(1:var_infos%ncshape))
      end if
      s = nf90_inquire_variable(ncid, var_infos%ncid, dimids = ncdimids)
      if (s /= nf90_noerr) then
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VAR, &
               & me, tgtname = var_infos%name, tgtid = var_infos%ncid, &
               & errid = s, errmess = nf90_strerror(s))
        end if
        deallocate(ncdimids)
        return
      end if
      do i = 1, var_infos%ncshape, 1
        ! will inq_dimlen()
        if (with_dim_name) then
          s = nf90_inquire_dimension(ncid, ncdimids(i), len = var_infos%ncdims(i), &
                                   & name = ncname)
          write(var_infos%ncdimnames(i), "(A)") ncname(1:min(80, len(ncname)))
        else
          s = nf90_inquire_dimension(ncid, ncdimids(i), len = var_infos%ncdims(i))
        end if
        if (s /= nf90_noerr) then
          if (present(error_data)) then
            call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, &
                 & ERROR_TYPE_DIM, me, tgtid = ncdimids(i), errid = s, &
                 & errmess = nf90_strerror(s))
          end if
          deallocate(ncdimids)
          call etsf_io_low_free_var_infos(var_infos)
          return
        end if
      end do
      deallocate(ncdimids)
    end if
    ! will inquire the number of attributes and their names.
    if (with_att_name) then
       if (n > 0) then
          allocate(var_infos%ncattnames(1:n))
          do i = 1, n, 1
             s = nf90_inq_attname(ncid, var_infos%ncid, i, ncname)
             if (s /= nf90_noerr) then
                if (present(error_data)) then
                   call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, &
                        & ERROR_TYPE_ATT, me, tgtid = i, errid = s, &
                        & errmess = nf90_strerror(s))
                end if
                call etsf_io_low_free_var_infos(var_infos)
                return
             end if
             write(var_infos%ncattnames(i), "(A)") ncname(1:min(80, len(ncname)))
          end do
       else
          var_infos%ncattnames => null()
       end if
    end if
    lstat = .true.
  end subroutine read_var_infos

  !!****s* etsf_io_low_var_infos/etsf_io_low_read_all_var_infos
  !! NAME
  !!  etsf_io_low_read_all_var_infos
  !!
  !! FUNCTION
  !!  Read a NetCDF file and create an array storing all variable
  !!  informations. These informations are stored in an array allocated in
  !!  this routine. It must be deallocated after use. The retrieved informations
  !!  include NetCDF varid, variable name, shape and dimensions. If the
  !!  with_dim_name is set to .true., the names of dimensions are also stored.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * with_dim_name = (optional) if set to .true., the dimension names are also
  !!                    retrieved. In that case, each element of output array
  !!                    @var_infos_array must be freed using
  !!                    etsf_io_low_free_var_infos().
  !!  * with_att_name = (optional) if set to .true., the attribute names are also
  !!                    retrieved. In that case, each element of output array
  !!                    @var_infos_array must be freed using
  !!                    etsf_io_low_free_var_infos().
  !!
  !! OUTPUT
  !!  * var_infos_array <type(etsf_io_low_var_infos)> = a pointer on an array to
  !!                                                    store the informations. This
  !!                                                    pointer must be null() on
  !!                                                    enter. If no variables are
  !!                                                    found or an error occurs, the
  !!                                                    pointer is let null().
  !!  * lstat = .true. if the file has been read without error.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !!
  !! SOURCE
  subroutine etsf_io_low_read_all_var_infos(ncid, var_infos_array, lstat, &
       & error_data, with_dim_name, with_att_name)
    integer, intent(in)                               :: ncid
    type(etsf_io_low_var_infos), pointer              :: var_infos_array(:)
    logical, intent(out)                              :: lstat
    type(etsf_io_low_error), intent(out), optional    :: error_data
    logical, optional, intent(in)                     :: with_dim_name
    logical, optional, intent(in)                     :: with_att_name

    !Local
    character(len = *), parameter :: me = "etsf_io_low_read_all_var_infos"
    integer :: i, j, s, nvars
    logical :: my_with_dim_name, my_with_att_name

    lstat = .false.
    if (present(with_dim_name))then
       my_with_dim_name = with_dim_name
    else
       my_with_dim_name = .false.
    end if
    if (present(with_att_name))then
       my_with_att_name = with_att_name
    else
       my_with_att_name = .false.
    end if
    ! Consistency checks...
    if (associated(var_infos_array)) then
       if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_ARG, me, &
               & tgtname = "var_infos_array", errid = 0, &
               & errmess = "pointer already allocated.")
       end if
       return
    end if
    var_infos_array => null()
    ! Inquire the NetCDF file for number of variables
    s = nf90_inquire(ncid, nVariables = nvars)
    if (s /= nf90_noerr) then
       if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VAR, me, &
               & errid = s, errmess = nf90_strerror(s))
       end if
       return
    end if
    if (nvars == 0) then
       ! No variables in the file.
       lstat = .true.
       return
    end if
    ! Allocate the var_infos_array argument and read var_infos for each
    ! variables in the NetCDF file.
    allocate(var_infos_array(nvars))
    do i = 1, nvars, 1
       if (present(error_data))then
          call read_var_infos_id(ncid, i, var_infos_array(i), lstat, error_data, &
               & dim_name = my_with_dim_name, att_name = my_with_att_name)
          if (.not. lstat) call etsf_io_low_error_update(error_data, me)
       else
          call read_var_infos_id(ncid, i, var_infos_array(i), lstat, &
               & dim_name = my_with_dim_name, att_name = my_with_att_name)
       end if
       ! Handle the error, if required.
       if (.not. lstat) then
          ! Free the var_infos_array argument before leaving
          do j = 1, i, 1
             call etsf_io_low_free_var_infos(var_infos_array(i))
          end do
          deallocate(var_infos_array)
          var_infos_array => null()
          return
       end if
    end do
    lstat = .true.
  end subroutine etsf_io_low_read_all_var_infos
  !!***

  subroutine read_flag_id(ncid, flag, ncvarid, attname, lstat, error_data)
    logical, intent(out)                           :: flag
    integer, intent(in)                            :: ncid, ncvarid
    character(len = *), intent(in)                 :: attname
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    character(len = *), parameter :: me = "read_flag_id"
    character(len = 3) :: value

    flag = .false.

    call etsf_io_low_read_att(ncid, ncvarid, attname, 3, value, lstat, error_data)
    if (.not. lstat) then
       call etsf_io_low_error_update(error_data, me)
       return
    end if
    flag = (value == "Yes" .or. value == "YES" .or. value == "yes")
  end subroutine read_flag_id
  subroutine read_flag(ncid, flag, varname, attname, lstat, error_data)
    logical, intent(out)                           :: flag
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    character(len = *), intent(in)                 :: attname
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    character(len = *), parameter :: me = "read_flag"
    character(len = 3) :: value

    flag = .false.

    call etsf_io_low_read_att(ncid, varname, attname, 3, value, lstat, error_data)
    if (.not. lstat) then
       call etsf_io_low_error_update(error_data, me)
       return
    end if
    flag = (value == "Yes" .or. value == "YES" .or. value == "yes")
  end subroutine read_flag

  ! Create the start, count and map arrays for a put or a get action using the
  ! NetCDF routines.
  subroutine etsf_io_low_make_access(start, count, map, var_infos, lstat, &
                                   & opt_start, opt_count, opt_map, error_data)
    integer, intent(out)                           :: start(16), count(16), map(16)
    type(etsf_io_low_var_infos), intent(in)        :: var_infos
    logical, intent(out)                           :: lstat
    integer, intent(in), optional                  :: opt_start(:), opt_count(:), opt_map(:)
    type(etsf_io_low_error), intent(out), optional :: error_data
    
    !Local
    character(len = *), parameter :: me = "etsf_io_low_make_access"
    integer :: i, val(16), j
    logical :: permut
    
    lstat = .true.
    if (var_infos%ncshape < 1) then
      return
    end if
    
    ! We create the start, count and map arguments required by NetCDF.
    if (present(opt_start)) then
      ! Size checks.
      if (size(opt_start) /= var_infos%ncshape) then
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, &
               & me, tgtname = "opt_start", errmess = "inconsistent length")
        end if
        lstat = .false.
        return
      end if
      ! Copy start
      start(1:var_infos%ncshape) = opt_start(1:var_infos%ncshape)
    else
      start(1:max(1, var_infos%ncshape)) = 1
    end if
    ! The count array
    if (present(opt_count)) then
      ! Size checks.
      if (size(opt_count) /= var_infos%ncshape) then
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, &
               & me, tgtname = "opt_count", errmess = "inconsistent length")
        end if
        lstat = .false.
        return
      end if
      ! Copy count excempt when negative
      do i = 1, var_infos%ncshape, 1
        if (opt_count(i) > 0) then
          count(i) = opt_count(i)
        else
          count(i) = var_infos%ncdims(i)
        end if
      end do
    else
      count(1:max(1, var_infos%ncshape)) = var_infos%ncdims(1:max(1, var_infos%ncshape))
    end if
    ! The map array
    if (present(opt_map)) then
      ! Size checks.
      if (size(opt_map) /= var_infos%ncshape) then
        if (present(error_data)) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, &
               & me, tgtname = "opt_map", errmess = "inconsistent length")
        end if
        lstat = .false.
        return
      end if
      ! Copy map if all positive, else apply permutations
      permut = .false.
      do i = 1, var_infos%ncshape, 1
        if (opt_map(i) <= 0) then
          permut = .true.
        end if
        if (permut .and. opt_map(i) > 0) then
          call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, &
               & me, tgtname = "opt_map", errmess = "inconsistent values")
        end if
      end do
      if (permut) then
        ! Create a map from the count information
        val(1) = 1
        ! j is the index of previous non unity count value
        if (count(1) > 1) then
          j = 1
        else
          j = 0
        end if
        do i = 2, var_infos%ncshape, 1
          if (count(i) > 1) then
            if (j > 0) then
              val(i) = val(i - 1) * count(j)
            else
              val(i) = val(i - 1)
            end if
            j = i
          else
            val(i) = val(i - 1)
          end if
        end do
        ! We do the permutations.
        do i = 1, var_infos%ncshape, 1
          if (-opt_map(i) <= 0 .or. -opt_map(i) > var_infos%ncshape) then
            if (present(error_data)) then
              call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, &
                   & me, tgtname = "opt_map", errmess = "out of bounds permutation")
            end if
            lstat = .false.
            return
          end if
          map(i) = val(-opt_map(i))
        end do
      else
        ! Copy map
        map(1:var_infos%ncshape) = opt_map(1:var_infos%ncshape)
      end if
    else
      ! Create a map from the count information
      map(1) = 1
      ! j is the index of previous non unity count value
      if (count(1) > 1) then
        j = 1
      else
        j = 0
      end if
      do i = 2, var_infos%ncshape, 1
        if (count(i) > 1) then
          if (j > 0) then
            map(i) = map(i - 1) * count(j)
          else
            map(i) = map(i - 1)
          end if
          j = i
        else
          map(i) = map(i - 1)
        end if
      end do
    end if
  end subroutine etsf_io_low_make_access
  
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
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
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
  subroutine etsf_io_low_check_var(var_ref, var, start, count, map, lstat, error_data)
    type(etsf_io_low_var_infos), intent(in)        :: var_ref
    type(etsf_io_low_var_infos), intent(in)        :: var
    integer, intent(in)                            :: start(:), count(:), map(:)
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_check_var"
    character(len = 80) :: err
    integer :: i, s, nb_ele_ref, nb_ele, sub_shape
    integer :: nclendims(1:7)
    
    lstat = .false.
    ! Check the type, if both numeric or both strings, vars are compatible.
    if ((var_ref%nctype == NF90_CHAR .and. var%nctype /= NF90_CHAR) .or. &
      & (var_ref%nctype /= NF90_CHAR .and. var%nctype == NF90_CHAR)) then
      write(err, "(A)") "incompatible type, both must be either numeric or character."
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
                     & tgtname = var_ref%name, errmess = err)
      end if
      return
    end if

    ! Size checks.
    if (var_ref%ncshape > 1 .and. (size(start) /= var_ref%ncshape .or. &
      & size(count) /= var_ref%ncshape .or. size(map) /= var_ref%ncshape)) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
             & tgtname = trim(var_ref%name) // " (start | count | map)", &
             & errmess = "inconsistent length")
      end if
      return
    end if
    ! Checks on start.
    do i = 1, var_ref%ncshape, 1
      if (start(i) <= 0 .or. start(i) > var_ref%ncdims(i)) then
        if (present(error_data)) then
          write(err, "(A,I0,A,I0,A,I5,A)") "wrong start value for index ", i, &
                                         & " (start(", i, ") = ", start(i), ")"
          call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, &
               & me, tgtname = trim(var_ref%name)//" (start)", errmess = err)
        end if
        return
      end if
    end do
    ! Checks on count.
    do i = 1, var_ref%ncshape, 1
      if (count(i) <= 0 .or. count(i) > var_ref%ncdims(i)) then
        if (present(error_data)) then
          write(err, "(A,I0,A,I0,A,I5,A)") "wrong count value for index ", i, &
                                         & " (count(", i, ") = ", count(i), ")"
          call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, &
               & me, tgtname = trim(var_ref%name)//" (count)", errmess = err)
        end if
        return
      end if
    end do
    ! Checks on map
    ! We get the number of destination elements
    if (var%ncshape == 0) then
      nb_ele = 1
    else
      nb_ele = product(var%ncdims(1:var%ncshape))
    end if
    ! We check that the mapping will not exceed the number of destination elements.
    nb_ele_ref = 1
    if (var%ncshape == 0) then
      ! if the destination variable is a scalar,
      ! we can ignore the map argument.
      nb_ele_ref = 1
    else
      do i = 1, var_ref%ncshape, 1
        nb_ele_ref = nb_ele_ref + map(i) * (count(i) - 1)
      end do
    end if
    if (nb_ele_ref > nb_ele) then
      if (present(error_data)) then
        write(err, "(A,A,I5,A,I5,A)") "wrong map value ", &
                                  & " (map address = ", nb_ele_ref, &
                                  & " & max address = ", nb_ele , ")"
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
             & tgtname = trim(var_ref%name)//" (map)", errmess = err)
      end if
      return
    end if
    
    ! The argument has a different shape that the store variable.
    ! We check the compatibility, product(var_to%ncdims) == product(var_from%ncdims)
    if (var_ref%ncshape == 0 .or. var%ncshape == 0) then
      ! If var shape is scalar, then always one element will be accessed.
      nb_ele_ref = 1
    else
      nb_ele_ref = product(count(1:var_ref%ncshape))
    end if
    if (nb_ele_ref /= nb_ele) then
      write(err, "(A,I5,A,I5,A)") "incompatible number of data (var_ref = ", &
                                & nb_ele_ref, " & var = ", nb_ele, ")"
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
                      & tgtname = var_ref%name, errmess = err)
      end if
      return
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
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
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
        call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_ATT, &
             & me, tgtname = attname, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    ! Check the type
    if (nctype /= atttype) then
      write(err, "(A,I5,A,I5,A)") "wrong type (read = ", nctype, &
                                & ", awaited = ", atttype, ")"
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, &
             & me, tgtname = attname, errmess = err)
      end if
      return
    end if
    ! Check the dimensions
    if ((atttype == NF90_CHAR .and. nclen > attlen) .or. &
      & (atttype /= NF90_CHAR .and. nclen /= attlen)) then
      write(err, "(A,I5,A,I5,A)") "wrong length (read = ", nclen, &
                                & ", awaited = ", attlen, ")"
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, &
             & me, tgtname = attname, errmess = err)
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
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
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
      call etsf_io_low_read_att(ncid, NF90_GLOBAL, "file_format", 80, format, &
           & stat, error_data) 
      if (.not. stat) call etsf_io_low_error_update(error_data, me)
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
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, &
             & me, tgtname = "file_format", errmess = err)
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! Check the version
    if (present(error_data)) then
      call etsf_io_low_read_att(ncid, NF90_GLOBAL, "file_format_version", &
                              & version_real, stat, error_data) 
      if (.not. stat) call etsf_io_low_error_update(error_data, me)
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
        call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ATT, &
             & me, tgtname = "file_format_version", errmess = err)
      end if
      call etsf_io_low_close(ncid, stat)
      return
    end if
    ! Check for the Conventions flag
    if (present(error_data)) then
      call etsf_io_low_check_att(ncid, NF90_GLOBAL, "Conventions", &
                               & NF90_CHAR, 80, stat, error_data) 
      if (.not. stat) call etsf_io_low_error_update(error_data, me)
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
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
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
  !!  * with_etsf_header = (optional) if true, will check that there is a header
  !!                       as defined in the ETSF specifications (default is .true.).
  !!
  !! SOURCE
  subroutine etsf_io_low_open_read(ncid, filename, lstat, version_min, &
                                 & error_data, with_etsf_header)
    integer, intent(out)                           :: ncid
    character(len = *), intent(in)                 :: filename
    logical, intent(out)                           :: lstat
    real, intent(in), optional                     :: version_min
    type(etsf_io_low_error), intent(out), optional :: error_data
    logical, intent(in), optional                  :: with_etsf_header

    !Local
    character(len = *), parameter :: me = "etsf_io_low_open_read"
    integer :: s
    logical :: my_with_etsf_header
    
    lstat = .false.
    ! Open file for reading
    s = nf90_open(path = filename, mode = NF90_NOWRITE, ncid = ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_IO, ERROR_TYPE_ORD, &
             & me, tgtname = filename, errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    ! From now on the file is open. If an error occur,
    ! we should close it.

    if (present(with_etsf_header)) then
      my_with_etsf_header = with_etsf_header
    else
      my_with_etsf_header = .true.
    end if
    if (my_with_etsf_header) then
      if (present(error_data)) then
        if (present(version_min)) then
          call etsf_io_low_check_header(ncid, lstat, version_min, error_data)
        else
          call etsf_io_low_check_header(ncid, lstat, error_data = error_data)
        end if
        if (.not. lstat) call etsf_io_low_error_update(error_data, me)
      else
        if (present(version_min)) then
          call etsf_io_low_check_header(ncid, lstat, version_min = version_min)
        else
          call etsf_io_low_check_header(ncid, lstat)
        end if
      end if
    else
      lstat = .true.
    end if
  end subroutine etsf_io_low_open_read
  !!***

  ! Generic routine, documented in the module file.
  subroutine read_var_double_var(ncid, varname, var, lstat, &
                               & start, count, map, ncvarid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    type(etsf_io_low_var_double), intent(inout)    :: var
    logical, intent(out)                           :: lstat
    integer, intent(in), optional                  :: start(:), count(:), map(:)
    integer, intent(out), optional                 :: ncvarid
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "read_var_double_var"
    character(len = 80) :: err
    integer :: varid
    type(etsf_io_low_error) :: error
    
    if (associated(var%data1D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_double_1D(ncid, varname, var%data1D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_double_1D(ncid, varname, var%data1D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_double_1D(ncid, varname, var%data1D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_double_1D(ncid, varname, var%data1D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data2D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_double_2D(ncid, varname, var%data2D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_double_2D(ncid, varname, var%data2D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_double_2D(ncid, varname, var%data2D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_double_2D(ncid, varname, var%data2D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data3D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_double_3D(ncid, varname, var%data3D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_double_3D(ncid, varname, var%data3D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_double_3D(ncid, varname, var%data3D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_double_3D(ncid, varname, var%data3D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data4D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_double_4D(ncid, varname, var%data4D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_double_4D(ncid, varname, var%data4D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_double_4D(ncid, varname, var%data4D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_double_4D(ncid, varname, var%data4D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data5D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_double_5D(ncid, varname, var%data5D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_double_5D(ncid, varname, var%data5D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_double_5D(ncid, varname, var%data5D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_double_5D(ncid, varname, var%data5D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data6D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_double_6D(ncid, varname, var%data6D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_double_6D(ncid, varname, var%data6D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_double_6D(ncid, varname, var%data6D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_double_6D(ncid, varname, var%data6D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data7D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_double_7D(ncid, varname, var%data7D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_double_7D(ncid, varname, var%data7D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_double_7D(ncid, varname, var%data7D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_double_7D(ncid, varname, var%data7D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
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
      if (.not. lstat) call etsf_io_low_error_update(error_data, me)
    end if
    if (present(ncvarid)) then
      ncvarid = varid
    end if
  end subroutine read_var_double_var
  
  ! Generic routine, documented in the module file.
  subroutine read_var_integer_var(ncid, varname, var, lstat, &
                                & start, count, map, ncvarid, error_data)
    integer, intent(in)                            :: ncid
    character(len = *), intent(in)                 :: varname
    type(etsf_io_low_var_integer), intent(inout)   :: var
    logical, intent(out)                           :: lstat
    integer, intent(in), optional                  :: start(:), count(:), map(:)
    integer, intent(out), optional                 :: ncvarid
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "read_var_integer_var"
    character(len = 80) :: err
    integer :: varid
    type(etsf_io_low_error) :: error
   
    if (associated(var%data1D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_integer_1D(ncid, varname, var%data1D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data2D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_integer_2D(ncid, varname, var%data2D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data3D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_integer_3D(ncid, varname, var%data3D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data4D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_integer_4D(ncid, varname, var%data4D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data5D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_integer_5D(ncid, varname, var%data5D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data6D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
      else
        call read_var_integer_6D(ncid, varname, var%data6D, lstat, &
                               & ncvarid = varid, error_data = error)
      end if
    else if (associated(var%data7D)) then
      if (present(start) .and. present(count) .and. present(map)) then
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(count)) then
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(start) .and. present(map)) then
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & start = start, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(count) .and. present(map)) then
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, &
                               & count = count, map = map, &
                               & ncvarid = varid, error_data = error)
      else if (present(start)) then
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, start = start, &
                               & ncvarid = varid, error_data = error)
      else if (present(count)) then
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, count = count, &
                               & ncvarid = varid, error_data = error)
      else if (present(map)) then
        call read_var_integer_7D(ncid, varname, var%data7D, lstat, map = map, &
                               & ncvarid = varid, error_data = error)
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
      if (.not. lstat) call etsf_io_low_error_update(error_data, me)
    end if
    if (present(ncvarid)) then
      ncvarid = varid
    end if
  end subroutine read_var_integer_var
