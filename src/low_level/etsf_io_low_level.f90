module etsf_io_low_level

  use netcdf

  implicit none

  ! Basic variables  
  character(len = *), parameter, private :: &
    & etsf_io_low_file_format = "ETSF Nanoquanta"
  character(len = *), parameter, private :: &
    & etsf_io_low_conventions = "http://www.etsf.eu/fileformats/"

  ! Error handling
  integer, parameter, private :: nb_access_mode = 6
  character(len = 15), dimension(nb_access_mode), parameter, private :: &
    & etsf_io_low_error_mode = (/ "define         ", &
                                & "get            ", &
                                & "input/output   ", &
                                & "inquire        ", &
                                & "put            ", &
                                & "specifications " /)

  integer, parameter, private :: nb_target_type = 11
  character(len = 22), dimension(nb_target_type), parameter, private :: &
    & etsf_io_low_error_type = (/ "attribute             ", "dimension ID          ", &
                                & "dimension             ", "end definitions       ", &
                                & "define mode           ", "create file           ", &
                                & "open file for reading ", "open file for writing ", &
                                & "variable              ", "variable ID           ", &
                                & "close file            " /)
  private :: set_error


  
  include "public_variables.f90"
    
  !Generic interface of the routines etsf_io_low_read_var
  interface etsf_io_low_read_var
    module procedure etsf_io_low_read_var_integer_1D
    module procedure etsf_io_low_read_var_integer_2D
    module procedure etsf_io_low_read_var_integer_3D
    module procedure etsf_io_low_read_var_integer_4D
    module procedure etsf_io_low_read_var_integer_5D
    module procedure etsf_io_low_read_var_integer_6D
    module procedure etsf_io_low_read_var_integer_7D
    module procedure etsf_io_low_read_var_double_1D
    module procedure etsf_io_low_read_var_double_2D
    module procedure etsf_io_low_read_var_double_3D
    module procedure etsf_io_low_read_var_double_4D
    module procedure etsf_io_low_read_var_double_5D
    module procedure etsf_io_low_read_var_double_6D
    module procedure etsf_io_low_read_var_double_7D
    module procedure etsf_io_low_read_var_character_2D
    module procedure etsf_io_low_read_var_character_3D
    module procedure etsf_io_low_read_var_character_4D
    module procedure etsf_io_low_read_var_character_5D
    module procedure etsf_io_low_read_var_character_6D
    module procedure etsf_io_low_read_var_character_7D
  end interface etsf_io_low_read_var
  !End of the generic interface of etsf_io_low_read_var
  
  !Generic interface of the routines etsf_io_low_read_att
  interface etsf_io_low_read_att
    module procedure etsf_io_low_read_att_integer
    module procedure etsf_io_low_read_att_real
    module procedure etsf_io_low_read_att_double
    module procedure etsf_io_low_read_att_integer_1D
    module procedure etsf_io_low_read_att_real_1D
    module procedure etsf_io_low_read_att_double_1D
    module procedure etsf_io_low_read_att_character_1D
  end interface etsf_io_low_read_att
  !End of the generic interface of etsf_io_low_read_att

  !Generic interface of the routines etsf_io_low_def_var
  interface etsf_io_low_def_var
    module procedure etsf_io_low_def_var_0D
    module procedure etsf_io_low_def_var_nD
  end interface etsf_io_low_def_var
  !End of the generic interface of etsf_io_low_def_var

  !Generic interface of the routines etsf_io_low_write_att
  interface etsf_io_low_write_att
    module procedure etsf_io_low_write_att_integer
    module procedure etsf_io_low_write_att_real
    module procedure etsf_io_low_write_att_double
    module procedure etsf_io_low_write_att_integer_1D
    module procedure etsf_io_low_write_att_real_1D
    module procedure etsf_io_low_write_att_double_1D
    module procedure etsf_io_low_write_att_character_1D
  end interface etsf_io_low_write_att
  !End of the generic interface of etsf_io_low_write_att

  !Generic interface of the routines etsf_io_low_write_var
  interface etsf_io_low_write_var
    module procedure etsf_io_low_write_var_integer_1D
    module procedure etsf_io_low_write_var_integer_2D
    module procedure etsf_io_low_write_var_integer_3D
    module procedure etsf_io_low_write_var_integer_4D
    module procedure etsf_io_low_write_var_integer_5D
    module procedure etsf_io_low_write_var_integer_6D
    module procedure etsf_io_low_write_var_integer_7D
    module procedure etsf_io_low_write_var_double_1D
    module procedure etsf_io_low_write_var_double_2D
    module procedure etsf_io_low_write_var_double_3D
    module procedure etsf_io_low_write_var_double_4D
    module procedure etsf_io_low_write_var_double_5D
    module procedure etsf_io_low_write_var_double_6D
    module procedure etsf_io_low_write_var_double_7D
    module procedure etsf_io_low_write_var_character_2D
    module procedure etsf_io_low_write_var_character_3D
    module procedure etsf_io_low_write_var_character_4D
    module procedure etsf_io_low_write_var_character_5D
    module procedure etsf_io_low_write_var_character_6D
    module procedure etsf_io_low_write_var_character_7D
  end interface etsf_io_low_write_var
  !End of the generic interface of etsf_io_low_write_var
  
contains

  subroutine set_error(error_data, mode, type, parent, tgtid, tgtname, errid, errmess)
    type(etsf_io_low_error), intent(out)     :: error_data
    integer, intent(in)                      :: mode, type
    character(len = *), intent(in)           :: parent
    integer, intent(in), optional            :: tgtid, errid
    character(len = *), intent(in), optional :: tgtname, errmess

    ! Consistency checkings    
    if (mode < 1 .or. mode > nb_access_mode) then
      write(0, *) "   *** ETSF I/O Internal error ***"
      write(0, *) "   mode argument out of range: ", mode
      return
    end if
    if (type < 1 .or. type > nb_target_type) then
      write(0, *) "   *** ETSF I/O Internal error ***"
      write(0, *) "   type argument out of range: ", type
      return
    end if
    
    ! Storing informations
    error_data%parent = parent(1:min(80, len(parent)))
    error_data%access_mode_id = mode
    error_data%access_mode_str = etsf_io_low_error_mode(mode)
    error_data%target_type_id = type
    error_data%target_type_str = etsf_io_low_error_type(type)
    if (present(tgtid)) then
      error_data%target_id = tgtid
    else
      error_data%target_id = -1
    end if
    if (present(tgtname)) then
      error_data%target_name = tgtname(1:min(80, len(tgtname)))
    else
      error_data%target_name = ""
    end if
    if (present(errid)) then
      error_data%error_id = errid
    else
      error_data%error_id = -1
    end if
    if (present(errmess)) then
      error_data%error_message = errmess(1:min(256, len(errmess)))
    else
      error_data%error_message = ""
    end if
  end subroutine set_error

  subroutine etsf_io_low_error_handle(error_data)
    type(etsf_io_low_error), intent(in) :: error_data
      
    ! Error handling
    write(*,*) 
    write(*,*) "    ***"
    write(*,*) "    *** ETSF I/O ERROR"
    write(*,*) "    ***"
    write(*,*) "    *** Calling subprogram : ", trim(error_data%parent)
    write(*,*) "    *** Action performed   : ", trim(error_data%access_mode_str), &
             & " ", trim(error_data%target_type_str)
    if (trim(error_data%target_name) /= "") then
      write(*,*) "    *** Target (name)      : ", trim(error_data%target_name)
    end if
    if (error_data%target_id >= 0) then
      write(*,*) "    *** Target (id)        : ", error_data%target_id
    end if
    if (trim(error_data%error_message) /= "") then
      write(*,*) "    *** Error message      : ", trim(error_data%error_message)
    end if
    if (error_data%error_id >= 0) then
      write(*,*) "    *** Error id           : ", error_data%error_id
    end if
    write(*,*) "    ***"
    write(*,*) 
  end subroutine etsf_io_low_error_handle

  subroutine etsf_io_low_close(ncid, lstat, error_data)
    integer, intent(in)                            :: ncid
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_close"
    integer :: s
    
    lstat = .false.
    ! Close file
    s = nf90_close(ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call set_error(error_data, ERROR_MODE_IO, ERROR_TYPE_CLO, me, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    lstat = .true.
  end subroutine etsf_io_low_close

  include "read_routines.f90"
  include "read_routines_auto.f90"

  include "write_routines.f90"
  include "write_routines_auto.f90"

end module etsf_io_low_level
