module tests

  use etsf_io_low_level

  implicit none

  interface tests_init_variable
     module procedure allocate_int_0D
     module procedure allocate_int_1D
     module procedure allocate_int_2D
     module procedure allocate_int_3D
     module procedure allocate_int_4D
     module procedure allocate_int_5D
     module procedure allocate_int_6D
     module procedure allocate_int_7D
     module procedure allocate_dbl_0D
     module procedure allocate_dbl_1D
     module procedure allocate_dbl_2D
     module procedure allocate_dbl_3D
     module procedure allocate_dbl_4D
     module procedure allocate_dbl_5D
     module procedure allocate_dbl_6D
     module procedure allocate_dbl_7D
     module procedure allocate_str_0D
     module procedure allocate_str_1D
  end interface

  interface tests_check_variable
     module procedure check_read_0D
     module procedure check_read_nD
  end interface

  interface tests_check_values
     module procedure check_mem_int_0D
     module procedure check_mem_int_nD
     module procedure check_mem_dbl_0D
     module procedure check_mem_dbl_nD
     module procedure check_mem_str_0D
     module procedure check_mem_str_1D
  end interface
  
contains

  subroutine tests_status(name, lstat, error)
    character(len = *), intent(in)      :: name
    logical, intent(in)                 :: lstat
    type(etsf_io_low_error), intent(in) :: error
    
    if (lstat) then
      write(*, "(A,A,A,A)") "== ", name, repeat(" ", 68 - len(name)), "OK     =="
    else
      write(*, "(A,A,A,A)") "== ", name, repeat(" ", 68 - len(name)), "Failed =="
      call etsf_io_low_error_handle(error)
    end if
  end subroutine tests_status
  
  subroutine check_read_0D(ncid, varname, type, lstat, error_data)
    integer, intent(in) :: ncid
    character(len = *), intent(in) :: varname
    character(len = *), intent(in) :: type
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(inout) :: error_data

    character(len = *), parameter :: me = "check_read_0D"

    if (type == "integer") then
       call check_read_nD(ncid, varname, type, (/ 1 /), lstat, error_data)
    else if (type == "real double_precision") then
       call check_read_nD(ncid, varname, type, (/ 1 /), lstat, error_data)
    else
       call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
            & tgtname = "check_read_0D", errmess = "unknown type")
       lstat = .false.
    end if
  end subroutine check_read_0D

  subroutine check_read_nD(ncid, varname, type, dims, lstat, error_data)
    integer, intent(in) :: ncid
    character(len = *), intent(in) :: varname
    character(len = *), intent(in) :: type
    integer, intent(in) :: dims(:)
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(inout) :: error_data

    character(len = *), parameter :: me = "check_read_nD"
    integer, allocatable :: int_array(:)
    double precision, allocatable :: dbl_array(:)
    character(len = dims(1)), allocatable :: strings(:)

    ! Allocate temporary read space
    if (type == "integer") then
       allocate(int_array(product(dims)))
    else if (type == "real double_precision") then
       allocate(dbl_array(product(dims)))
    else if (type == "string") then
       if (size(dims) == 1) then
          allocate(strings(1))
       else
          allocate(strings(dims(2)))
       end if
    else
       call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
            & tgtname = me, errmess = "unknown type")
       lstat = .false.
    end if

    ! Read
    if (type == "integer") then
       call etsf_io_low_read_var(ncid, varname, int_array, &
            & lstat, error_data = error_data)
    else if (type == "real double_precision") then
       call etsf_io_low_read_var(ncid, varname, dbl_array, &
            & lstat, error_data = error_data)
    else if (type == "string") then
       call etsf_io_low_read_var(ncid, varname, strings, dims(1), &
            & lstat, error_data = error_data)
    end if
    call tests_status(" | read '"// varname //"' values", lstat, error_data)
    if (.not. lstat) return

    ! Check values
    if (type == "integer") then
       call check_mem_int_nD(int_array, varname, lstat, error_data)
       deallocate(int_array)
    else if (type == "real double_precision") then
       call check_mem_dbl_nD(dbl_array, varname, lstat, error_data)
       deallocate(dbl_array)
    else if (type == "string") then
       if (size(dims) == 1) then
          call check_mem_str_0D(strings(1), dims(1), varname, lstat, error_data)
       else
          call check_mem_str_1D(strings, dims, varname, lstat, error_data)
       end if
       deallocate(strings)
    end if
  end subroutine check_read_nD

  !------------------!
  ! The integer case !
  !------------------!
  subroutine allocate_int_0D(array)
    integer, pointer :: array

    allocate(array)
    array = 1
  end subroutine allocate_int_0D
  subroutine allocate_int_1D(array, dims)
    integer, pointer :: array(:)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1)))
    array = (/ (i, i = 1, product(dims)) /)
  end subroutine allocate_int_1D
  subroutine allocate_int_2D(array, dims)
    integer, pointer :: array(:, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:2) )
  end subroutine allocate_int_2D
  subroutine allocate_int_3D(array, dims)
    integer, pointer :: array(:, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:3) )
  end subroutine allocate_int_3D
  subroutine allocate_int_4D(array, dims)
    integer, pointer :: array(:, :, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3), dims(4)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:4) )
  end subroutine allocate_int_4D
  subroutine allocate_int_5D(array, dims)
    integer, pointer :: array(:, :, :, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:5) )
  end subroutine allocate_int_5D
  subroutine allocate_int_6D(array, dims)
    integer, pointer :: array(:, :, :, :, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:6) )
  end subroutine allocate_int_6D
  subroutine allocate_int_7D(array, dims)
    integer, pointer :: array(:, :, :, :, :, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6), dims(7)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:7) )
  end subroutine allocate_int_7D

  subroutine check_mem_int_0D(value, varname, lstat, error_data)
    integer, intent(in) :: value
    character(len = *), intent(in) :: varname
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(inout) :: error_data

    call check_mem_int_nD((/ value /), varname, lstat, error_data)
  end subroutine check_mem_int_0D

  subroutine check_mem_int_nD(array, varname, lstat, error_data)
    integer, intent(in) :: array(:)
    character(len = *), intent(in) :: varname
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(inout) :: error_data

    character(len = *), parameter :: me = "check_mem_int_nD"
    integer, allocatable :: read_array(:)
    integer :: i

    ! Check values
    lstat = .true.
    do i = 1, size(array), 1
       lstat = (array(i) == i) .and. lstat
    end do
    if (.not. lstat) then
       call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
            & tgtname = varname, errmess = "wrong values")
       lstat = .false.
    end if
    call tests_status(" | check '"// varname //"' values", lstat, error_data)
  end subroutine check_mem_int_nD

  !-----------------!
  ! The double case !
  !-----------------!
  subroutine allocate_dbl_0D(array)
    double precision, pointer :: array

    allocate(array)
    array = 1.d0
  end subroutine allocate_dbl_0D
  subroutine allocate_dbl_1D(array, dims)
    double precision, pointer :: array(:)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1)))
    array = (/ (i, i = 1, product(dims)) /)
  end subroutine allocate_dbl_1D
  subroutine allocate_dbl_2D(array, dims)
    double precision, pointer :: array(:, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:2) )
  end subroutine allocate_dbl_2D
  subroutine allocate_dbl_3D(array, dims)
    double precision, pointer :: array(:, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:3) )
  end subroutine allocate_dbl_3D
  subroutine allocate_dbl_4D(array, dims)
    double precision, pointer :: array(:, :, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3), dims(4)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:4) )
  end subroutine allocate_dbl_4D
  subroutine allocate_dbl_5D(array, dims)
    double precision, pointer :: array(:, :, :, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:5) )
  end subroutine allocate_dbl_5D
  subroutine allocate_dbl_6D(array, dims)
    double precision, pointer :: array(:, :, :, :, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:6) )
  end subroutine allocate_dbl_6D
  subroutine allocate_dbl_7D(array, dims)
    double precision, pointer :: array(:, :, :, :, :, :, :)
    integer, intent(in) :: dims(:)

    integer :: i

    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6), dims(7)))
    array = reshape( (/ (i, i = 1, product(dims)) /) , dims(1:7) )
  end subroutine allocate_dbl_7D

  subroutine check_mem_dbl_0D(value, varname, lstat, error_data)
    double precision, intent(in) :: value
    character(len = *), intent(in) :: varname
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(inout) :: error_data

    call check_mem_dbl_nD((/ value /), varname, lstat, error_data)
  end subroutine check_mem_dbl_0D

  subroutine check_mem_dbl_nD(array, varname, lstat, error_data)
    double precision, intent(in) :: array(:)
    character(len = *), intent(in) :: varname
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(inout) :: error_data

    character(len = *), parameter :: me = "check_mem_dbl_nD"
    integer :: i

    ! Check values
    lstat = .true.
    do i = 1, size(array), 1
       lstat = (array(i) == i) .and. lstat
    end do
    if (.not. lstat) then
       call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
            & tgtname = varname, errmess = "wrong values")
       lstat = .false.
    end if
    call tests_status(" | check '"// varname //"' values", lstat, error_data)
  end subroutine check_mem_dbl_nD

  !-----------------!
  ! The string case !
  !-----------------!
  subroutine allocate_str_0D(string, dims)
    integer, intent(in) :: dims(:)
    character(len = dims(1)), pointer :: string

    allocate(string)
    write(string, "(A)") repeat("a", dims(1))
  end subroutine allocate_str_0D

  subroutine allocate_str_1D(string, dims)
    integer, intent(in) :: dims(:)
    character(len = dims(1)), pointer :: string(:)

    integer :: i
    character(len = 1) :: chr

    allocate(string(dims(2)))
    do i = 1, dims(2), 1
       write(chr, "(A1)") char(97 + modulo(i - 1, 26))
       write(string(i), "(A)") repeat(chr, dims(1))
    end do
  end subroutine allocate_str_1D

  subroutine check_mem_str_0D(string, length, varname, lstat, error_data)
    character(len = length), intent(in) :: string
    integer, intent(in) :: length
    character(len = *), intent(in) :: varname
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(inout) :: error_data

    character(len = *), parameter :: me = "check_mem_str"

    ! Check values
    lstat = (string == repeat("a", length))
    if (.not. lstat) then
       call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
            & tgtname = varname, errmess = "wrong values")
       lstat = .false.
    end if
    call tests_status(" | check '"// varname //"' values", lstat, error_data)
  end subroutine check_mem_str_0D
  subroutine check_mem_str_1D(string, dims, varname, lstat, error_data)
    integer, intent(in) :: dims(:)
    character(len = dims(1)), intent(in) :: string(:)
    character(len = *), intent(in) :: varname
    logical, intent(out) :: lstat
    type(etsf_io_low_error), intent(inout) :: error_data

    character(len = *), parameter :: me = "check_mem_str_1D"
    character(len = 1) :: chr
    integer :: i

    ! Check values
    lstat = .true.
    do i = 1, dims(2), 1
       write(chr, "(A1)") char(97 + modulo(i - 1, 26))
       lstat = lstat .and. (string(i) == repeat(chr, dims(1)))
    end do
    if (.not. lstat) then
       call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
            & tgtname = varname, errmess = "wrong values")
       lstat = .false.
    end if
    call tests_status(" | check '"// varname //"' values", lstat, error_data)
  end subroutine check_mem_str_1D

end module tests
