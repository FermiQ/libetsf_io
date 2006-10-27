program test

  use etsf_io_low_level
  
  integer :: ncid
  logical :: stat
  integer :: dimval
  
  call etsf_io_low_open_read(ncid, "/home/caliste/t01_o-etsf.nc", stat, error_handle=etsf_io_low_error_handle)
  if (.not. stat) stop
  
  call etsf_io_low_read_dim(ncid, "number_of_atoms", dimval, stat, error_handle=etsf_io_low_error_handle)
  if (.not. stat) stop
  write(*,*) "Number of atoms", dimval
  
  call etsf_io_low_close(ncid, stat, error_handle=etsf_io_low_error_handle)
end program test
