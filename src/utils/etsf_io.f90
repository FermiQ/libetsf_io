program etsf_io_ploumploum

  use etsf_io_low_level
  use etsf_io
  use etsf_io_file

  implicit none

  integer :: nArg, iargc, i_arg
  character(len = 256) :: opt_value

  logical :: lstat
  type(etsf_io_low_error) :: error
  integer :: read_flags, check_flags, i, j
  type(etsf_io_low_error) :: errors(etsf_nspecs_data)
  
  character(len = 80) :: action = ""
  ! Variables used for the merge action
  logical :: get_help = .false.
  integer :: n_input_args = 0
  character(len = 256) :: input_args(256)
  integer :: n_input_files = 0
  character(len = 256) :: input_files(256)
  character(len = 256) :: output_file = ""
  logical :: get_specs_list = .false.
  integer :: n_input_flags = 0
  character(len = 256) :: input_flags(256)
  
  
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
     else if (get_option("l", "list", opt_value, i_arg, .false.)) then
        get_specs_list = .true.
     else if (get_option("f", "flag", opt_value, i_arg)) then
        n_input_flags = n_input_flags + 1
        input_flags(n_input_flags) = opt_value
     else if (get_option("h", "help", opt_value, i_arg, .false.)) then
        get_help = .true.
     else
        ! This should be the end of option.
        call getarg(i_arg, opt_value)
        if (opt_value(1:2) == "-") then
           write(*, "(A)") "Error: unknown option or argument '", trim(opt_value), "'"
           call usage()
           stop
        else
           ! Store remaining argments in an array.
           n_input_args = 0
           do
              call getarg(i_arg, opt_value)
              n_input_args = n_input_args + 1
              write(input_args(n_input_args), "(A)") adjustl(opt_value)
              i_arg = i_arg + 1
              if (i_arg > narg) then
                 exit
              end if
           end do
        end if
     end if
     i_arg = i_arg + 1
     if (i_arg > narg) then
        exit
     end if
  end do

  if (get_help) then
     call usage()
     stop
  end if

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
  else if (trim(action) == "content") then
     if (n_input_args /= 1) then
        write(*, "(A)") "Error: exactly one arguments is required for action content."
        call usage()
        stop
     end if
     write(*,"(3A)") "Analyse file '", trim(input_args(1)), "'"
     call etsf_io_file_contents(read_flags, errors, trim(input_args(1)), &
          & lstat, error)
     if (.not. lstat) then
        call etsf_io_low_error_handle(error)
        stop
     end if
     do i = 1, etsf_nspecs_data, 1
        if (iand(read_flags, 2 ** (i - 1)) /= 0) then
           write(*,"(3A)") " - Ok - ", trim(etsf_specs_names(i)), "."
        else
           write(*,"(3A)") " - No - ", trim(etsf_specs_names(i)), "."
           write(*,"(4A)") "        given reason, '", trim(errors(i)%target_name), &
                & "' -> ", trim(errors(i)%error_message)
        end if
     end do
  else if (trim(action) == "check") then
     if (get_specs_list) then
        write(*,"(A)") "Available flags for specification checkings:"
        do i = 1, etsf_nspecs_data, 1
           write(*,"(A)") trim(etsf_specs_names(i))
        end do
        stop
     end if
     if (n_input_flags < 1) then
        write(*, "(A)") "Error: not enough flags for action check."
        call usage()
        stop
     end if
     if (n_input_args /= 1) then
        write(*, "(A)") "Error: exactly one arguments is required for action check."
        call usage()
        stop
     end if
     check_flags = 0
     do j = 1, n_input_args, 1
        do i = 1, etsf_nspecs_data, 1
           if (trim(etsf_specs_names(i)) == trim(input_flags(j))) then
              check_flags = check_flags + 2 ** (i - 1)
              exit
           end if
        end do
     end do
     call etsf_io_file_check(trim(input_args(1)), check_flags, lstat, error)
     if (.not. lstat) then
        call etsf_io_low_error_handle(error)
        stop
     end if
  else
     write(*, "(A)") "Error: missing or unknown action, use -a option."
     call usage()
     stop
  end if
  
contains

  function get_option(code, name, value, i_arg, with_value)
    implicit none

    character(len = 1), intent(in) :: code
    character(len = *), intent(in) :: name
    character(len = 256), intent(out) :: value
    integer, intent(inout) :: i_arg
    logical, intent(in), optional :: with_value
    logical :: get_option, my_with_value
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
    if (present(with_value)) then
       my_with_value = with_value
    else
       my_with_value = .true.
    end if
    if (get_option .and. my_with_value) then
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
    write(*, "(A)") "Usage: etsf_io [-h | -a action] [[-i file]...] [[-f flag]...]"
    write(*, "(A)") "               [-o file] [arguments]"
    write(*, "(A)") ""
    write(*, "(A)") "   Handle ETSF files, see --action option."
    write(*, "(A)") "-h --help             : show this little help."
    write(*, "(A)") "-a --action value     : give the action to perform."
    write(*, "(A)") "                        Possible action may be:"
    write(*, "(A)") "                        * 'merge' to gather several files that"
    write(*, "(A)") "                          have been splitted."
    write(*, "(A)") "                        * 'content' to get the name of"
    write(*, "(A)") "                          specifications the file matches."
    write(*, "(A)") "                        * 'check' to check the validity of"
    write(*, "(A)") "                          the file against specifications."
    write(*, "(A)") "-o --output-file file : give the path to the output ETSF file."
    write(*, "(A)") "-i --input-file file  : give the path for an input file. This"
    write(*, "(A)") "                        option can be used one or several times."
    write(*, "(A)") "-l --list             : when action is check, it give the list"
    write(*, "(A)") "                        of available flags."
    write(*, "(A)") "-f --flag value       : give a flag name (get valid names from"
    write(*, "(A)") "                        -l option)."
    write(*, "(A)") ""
    write(*, "(A)") "   Examples:"
    write(*, "(A)") "Merge three files, etsf_io -a merge -i file1.nc -i file2.nc"
    write(*, "(A)") "                   -i file3.nc -o output.nc"
    write(*, "(A)") ""
    write(*, "(A)") "Get the contents of file test.nc, etsf_io -a contents test.nc"
    write(*, "(A)") ""
    write(*, "(A)") "Get the list of flags for validity checks, etsf_io -a check -l"
    write(*, "(A)") ""
    write(*, "(A)") "Checks with two flags, etsf_io -a check -f flag1 -f flag2 test.nc"
  end subroutine usage

end program etsf_io_ploumploum
