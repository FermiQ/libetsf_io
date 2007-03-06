program etsf_io

  use etsf_io_low_level
  use etsf_io_file

  implicit none

  integer :: nArg, iargc, i_arg
  character(len = 256) :: opt_value

  logical :: lstat
  type(etsf_io_low_error) :: error
  
  character(len = 80) :: action = ""
  ! Variables used for the merge action
  integer :: n_input_files = 0
  character(len = 256) :: input_files(256)
  character(len = 256) :: output_file = ""
  
  
  nArg = iargc()
  i_arg = 1
  do
     if (get_option("a", "action", opt_value, i_arg)) then
        action = opt_value(1:80)
     else if (get_option("i", "input-file", opt_value, i_arg)) then
        n_input_files = n_input_files + 1
        input_files(n_input_files) = opt_value
     else if (get_option("o", "output-file", opt_value, i_arg)) then
        output_file = opt_value
     else
        call getarg(i_arg, opt_value)
        write(*, "(A)") "Error: unknown option or argument '", trim(opt_value), "'"
        call usage()
        stop
     end if
     i_arg = i_arg + 1
     if (i_arg > narg) then
        exit
     end if
  end do

  if (trim(action) == "merge") then
     if (n_input_files < 2) then
        write(*, "(A)") "Error: not enough input files for action merge."
        call usage()
        stop
     end if
     if (trim(output_file) == "") then
        write(*, "(A)") "Error: no output file for action merge."
        call usage()
        stop
     end if
!     write(*,*) "output file: ", trim(output_file)
     call etsf_io_file_merge(output_file, input_files(1:n_input_files), &
          & lstat, error)
     if (.not. lstat) then
        call etsf_io_low_error_handle(error)
        stop
     end if
  else
     write(*, "(A)") "Error: missing action, use -a option."
     call usage()
     stop
  end if
  
contains

  function get_option(code, name, value, i_arg)
    implicit none

    character(len = 1), intent(in) :: code
    character(len = *), intent(in) :: name
    character(len = 256), intent(out) :: value
    integer, intent(inout) :: i_arg
    logical :: get_option
    character(len = 256) :: arg_value
    integer :: start

    call getarg(i_arg, arg_value)
    if (arg_value(1:2) == "-"//code) then
       start = 3
       get_option = .true.
    else if (arg_value(1:len(name) + 2) == "--"//name) then
       start = len(name) + 3 
       get_option = .true.
    else
       get_option = .false.
    end if
    if (get_option) then
       if (arg_value(start:start) /= " ") then
          if (arg_value(start:start) == "=") then
             value = arg_value(start + 1:256)
          else
             value = arg_value(start:256)
          end if
       else
          i_arg = i_arg + 1
          call getarg(i_arg, arg_value)
          value = adjustl(arg_value)
       end if
    end if
  end function get_option

  subroutine usage()
    write(*, "(A)") ""
    write(*, "(A)") "Usage: etsf_io -a action [[-i file]...] [-o file]"
    write(*, "(A)") ""
    write(*, "(A)") "   Handle ETSF files, see --action option."
    write(*, "(A)") "-a --action           : give the action to perform."
    write(*, "(A)") "                        Possible action is, 'merge' to gather"
    write(*, "(A)") "                        several files that have been splitted."
    write(*, "(A)") "-o --output-file      : give the path to the output ETSF file."
    write(*, "(A)") "-i --input-file       : give the path for an input file. This"
    write(*, "(A)") "                        option can be used one or several times."
  end subroutine usage

end program etsf_io
