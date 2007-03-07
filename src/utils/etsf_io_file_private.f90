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
  if (.not. lstat) return
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
