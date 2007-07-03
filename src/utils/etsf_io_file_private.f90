subroutine test_var(ncid, var_infos_ref, lstat, error_data)
  integer, intent(in)                     :: ncid
  type(etsf_io_low_var_infos), intent(in) :: var_infos_ref
  logical, intent(out)                    :: lstat
  type(etsf_io_low_error), intent(out)    :: error_data

  character(len=*),parameter              :: me = 'test_var'
  character(len = 256)                    :: errmess
  type(etsf_io_low_var_infos)             :: var_infos
  integer                                 :: i

  ! Test variable existence.
  call etsf_io_low_read_var_infos(ncid, trim(var_infos_ref%name), var_infos, &
                                & lstat, error_data, .true.)
  if (.not. lstat) then
     call etsf_io_low_error_update(error_data, me)
     return
  end if
  ! Now test variable definition.
  !  Type.
  if (var_infos_ref%nctype /= var_infos%nctype) then
    call etsf_io_low_free_var_infos(var_infos)
    lstat = .false.
    call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, &
                             & ERROR_TYPE_ARG, me, tgtname = trim(var_infos%name), &
                             & errmess = "wrong type.")
    return
  end if
  !  Shape.
  if (var_infos_ref%ncshape /= var_infos%ncshape) then
    call etsf_io_low_free_var_infos(var_infos)
    lstat = .false.
    write(errmess, "(A,I0,A)") "wrong shape definition, it should be ", &
         & var_infos_ref%ncshape, "."
    call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, &
                             & ERROR_TYPE_ARG, me, tgtname = trim(var_infos%name), &
                             & errmess = errmess)
    return
  end if
  !  Dimensions name.
  do i = 1, var_infos%ncshape, 1
    if (trim(var_infos_ref%ncdimnames(i)) /= trim(var_infos%ncdimnames(i))) then
      call etsf_io_low_free_var_infos(var_infos)
      lstat = .false.
      call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, &
                               & ERROR_TYPE_ARG, me, tgtname = trim(var_infos%name), &
                               & errmess = "wrong dimension definition, '" // &
                               & trim(var_infos_ref%ncdimnames(i)) // "' awaited.")
      return
    end if
  end do

  call etsf_io_low_free_var_infos(var_infos)
  lstat = .true.
end subroutine test_var

  !* This routine free the nsize first element of the array
  !* file_infos.
subroutine file_infos_free(file_infos, n_size)
  type(file_infos_type), intent(inout) :: file_infos(:)
  integer, intent(in) :: n_size

  integer :: i_file

  if (n_size > size(file_infos)) then
     write(0, *) "   *** ETSF I/O Internal error ***"
     write(0, *) "   file_infos_free n_size out of range: ", n_size
     return
  end if

  do i_file = 1, n_size, 1
     call etsf_io_split_free(file_infos(i_file)%split)
     call etsf_io_vars_free(file_infos(i_file)%var_list)
  end do
end subroutine file_infos_free

!* This routine is a basic implementation of a defining merge for
!* non ETSF variables. Given a list of variables and their definitions
!* the dimensions are defined variables per variables and then the variables
!* themselves are added.
subroutine non_etsf_init(ncid, infos_file, lstat, error_data)
  integer, intent(in) :: ncid
  type(file_infos_type), intent(in) :: infos_file(:)
  logical, intent(out) :: lstat
  type(etsf_io_low_error), intent(out) :: error_data

  character(len=*),parameter :: me = 'non_etsf_init'
  integer :: i_file, i_var, i_dim
  integer :: dimvalue
  type(etsf_io_low_var_infos) :: infos_var

  ! In a merge action, all variables should be the same in the different
  ! files, then will only define dimensions and variables from the first
  ! element of array infos_file. We only check that dimensions and variable
  ! exist for the other elements.
  lstat = .false.

  i_file = 1
  ! For each file, we read the list of variables
  do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
     ! For each non-ETSF variable, we read the list of dimensions
     if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
          & .not. infos_file(i_file)%var_list%split(i_var)) then
        do i_dim = 1, infos_file(i_file)%var_list%parent(i_var)%ncshape, 1
           ! For each dimension, we write it to the destination file.
           call etsf_io_low_write_dim(ncid, &
                & infos_file(i_file)%var_list%parent(i_var)%ncdimnames(i_dim), &
                & infos_file(i_file)%var_list%parent(i_var)%ncdims(i_dim), &
                & lstat, error_data = error_data)
           if (.not.lstat) then
              call etsf_io_low_error_update(error_data, me)
              return
           end if
        end do
     end if
  end do

  ! Now, we define the variables.
  do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
     if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
          & .not. infos_file(i_file)%var_list%split(i_var)) then
        if (infos_file(i_file)%var_list%parent(i_var)%ncshape > 0) then
           call etsf_io_low_def_var(&
                & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                & infos_file(i_file)%var_list%parent(i_var)%nctype, &
                & infos_file(i_file)%var_list%parent(i_var)%ncdimnames, lstat, &
                & error_data = error_data)
           if (.not.lstat) then
              call etsf_io_low_error_update(error_data, me)
              return
           end if
        else
           call etsf_io_low_def_var( &
                & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                & infos_file(i_file)%var_list%parent(i_var)%nctype, &
                & lstat, error_data = error_data)
           if (.not.lstat) then
              call etsf_io_low_error_update(error_data, me)
              return
           end if
        end if
     end if
  end do

  ! Now we check dimensions and variables for all other elements of input array
  ! infos_file.
  do i_file = 2, size(infos_file), 1
     do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
        if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
             & .not. infos_file(i_file)%var_list%split(i_var)) then
           ! We check that dimensions of this variable is in the destination
           ! file.
           do i_dim = 1, infos_file(i_file)%var_list%parent(i_var)%ncshape, 1
              call etsf_io_low_read_dim(ncid, &
                   & trim(infos_file(i_file)%var_list%parent(i_var)%ncdimnames(i_dim)), &
                   & dimvalue, lstat, error_data = error_data)
              if (.not. lstat .or. dimvalue /= &
                   & infos_file(i_file)%var_list%parent(i_var)%ncdims(i_dim)) then
                 call etsf_io_low_error_handle(error_data)
                 call etsf_io_low_error_set( &
                      & error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                      & errmess = "dimension '"// &
                      & trim(infos_file(i_file)%var_list%parent(i_var)%ncdimnames(i_dim)) &
                      & //"' is not present in all files or has different values.")
                 lstat = .false.
                 return
              end if
           end do
           ! We check variables
           call etsf_io_low_read_var_infos( &
                & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                & infos_var, lstat, error_data = error_data)
           if (.not. lstat .or. &
                & infos_var%nctype /= infos_file(1)%var_list%parent(i_var)%nctype .or. &
                & infos_var%ncshape /= infos_file(1)%var_list%parent(i_var)%ncshape) then
              call etsf_io_low_error_set( &
                   & error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                   & errmess = "variable '"// &
                   & trim(infos_file(i_file)%var_list%parent(i_var)%name) &
                   & //"' is not present in all files or has different definitions.")
              lstat = .false.
              return
           end if
        end if
     end do
  end do
  lstat = .true.
end subroutine non_etsf_init

!* Basic implementation of a copy routine for all non-ETSF variables.
!* Values are only copied from the first file, and no check is done
!* regarding to other files. This is surely crude and may be upgarded
!* later.
subroutine non_etsf_copy(ncid, infos_file, lstat, error_data)
  integer, intent(in) :: ncid
  type(file_infos_type), intent(in) :: infos_file(:)
  logical, intent(out) :: lstat
  type(etsf_io_low_error), intent(out) :: error_data

  integer, allocatable               :: integer_data(:)
  real, allocatable                  :: real_data(:)
  double precision, allocatable      :: double_data(:)
  character(len = 1024), allocatable :: string_data(:)

  character(len=*),parameter :: me = 'non_etsf_copy'
  integer :: ncid_from
  integer, allocatable :: varids_to(:)
  integer :: i_file, i_var
  integer :: n_size
  logical :: lstat_

  i_file = 1
  call etsf_io_low_open_read(ncid_from, trim(infos_file(i_file)%path), lstat, &
       & error_data = error_data)
  if (.not.lstat) then
     call etsf_io_low_error_update(error_data, me)
     return
  end if

  call etsf_io_low_set_write_mode(ncid, lstat, error_data = error_data)
  if (.not.lstat) then
     call etsf_io_low_error_update(error_data, me)
     return
  end if

  allocate(varids_to(infos_file(i_file)%var_list%n_vars))
  do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
     if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
          & .not. infos_file(i_file)%var_list%split(i_var)) then
        ! Read the values
        if (infos_file(i_file)%var_list%parent(i_var)%ncshape > 0) then
           n_size = product(infos_file(i_file)%var_list%parent(i_var)%ncdims( &
                & 1:infos_file(i_file)%var_list%parent(i_var)%ncshape))
        else
           n_size = 1
        end if
        select case (infos_file(i_file)%var_list%parent(i_var)%nctype)
           ! Case integer values.
        case (etsf_io_low_integer)
           allocate(integer_data(n_size))
           call etsf_io_low_read_var( &
                & ncid_from, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                & integer_data, lstat, error_data = error_data)
           if (lstat) then
              call etsf_io_low_write_var( &
                   & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                   & integer_data, lstat, error_data = error_data, &
                   & ncvarid = varids_to(i_var))
           end if
           ! Case real values.
        case (etsf_io_low_real)
           write(0, *) "   *** ETSF I/O Internal error ***"
           write(0, *) "   real variables not implemented, using double instead."
           allocate(double_data(n_size))
           call etsf_io_low_read_var( &
                & ncid_from, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                & double_data, lstat, error_data = error_data)
           if (lstat) then
              call etsf_io_low_write_var( &
                   & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                   & double_data, lstat, error_data = error_data, &
                   & ncvarid = varids_to(i_var))
           end if
           ! Case double values.
        case (etsf_io_low_double)
           allocate(double_data(n_size))
           call etsf_io_low_read_var( &
                & ncid_from, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                & double_data, lstat, error_data = error_data)
           if (lstat) then
              call etsf_io_low_write_var( &
                   & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                   & double_data, lstat, error_data = error_data, &
                   & ncvarid = varids_to(i_var))
           end if
           ! Case string values.
        case (etsf_io_low_character)
           if (infos_file(i_file)%var_list%parent(i_var)%ncshape == 0) then
              call etsf_io_low_error_set( &
                   & error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
                   & tgtname = trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                   & errmess = "character variables must be arrays.")
              lstat = .false.
              exit
           end if

           n_size = n_size / infos_file(i_file)%var_list%parent(i_var)%ncdims(1)
           allocate(string_data(n_size))
           call etsf_io_low_read_var( &
                & ncid_from, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                & string_data, infos_file(i_file)%var_list%parent(i_var)%ncdims(1), &
                & lstat, error_data = error_data)
           if (lstat) then
              call etsf_io_low_write_var( &
                   & ncid, trim(infos_file(i_file)%var_list%parent(i_var)%name), &
                   & string_data, infos_file(i_file)%var_list%parent(i_var)%ncdims(1), &
                   & lstat, error_data = error_data, ncvarid = varids_to(i_var))
           end if
        end select

        ! Deallocate all memory
        if (allocated(integer_data)) then
           deallocate(integer_data)
        end if
        if (allocated(real_data)) then
           deallocate(real_data)
        end if
        if (allocated(double_data)) then
           deallocate(double_data)
        end if
        if (allocated(string_data)) then
           deallocate(string_data)
        end if
        if (.not.lstat) then
           call etsf_io_low_error_update(error_data, me)
           exit
        end if
     end if
  end do

  call etsf_io_low_set_define_mode(ncid, lstat, error_data = error_data)
  if (.not. lstat) then
     deallocate(varids_to)
     call etsf_io_low_error_update(error_data, me)
     return
  end if
  do i_var = 1, infos_file(i_file)%var_list%n_vars, 1
     if (infos_file(i_file)%var_list%group(i_var) == etsf_grp_none .and. &
          & .not. infos_file(i_file)%var_list%split(i_var)) then
        ! We copy all the attributes.
        call etsf_io_low_copy_all_att(ncid_from, ncid, &
             & infos_file(i_file)%var_list%parent(i_var)%ncid, varids_to(i_var), &
             & lstat, error_data = error_data)
        if (.not.lstat) then
           call etsf_io_low_error_update(error_data, me)
           exit
        end if
     end if
  end do
  deallocate(varids_to)

  ! Notice: we ignore close errors if any.
  call etsf_io_low_close(ncid_from, lstat_)
end subroutine non_etsf_copy
