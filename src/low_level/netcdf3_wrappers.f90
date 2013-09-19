subroutine wrap_nf90_create(s, filename, cmode, ncid, useMPI, MPI_comm, MPI_info)
  use netcdf
  implicit none
  integer, intent(out) :: s
  character(len = *), intent(in) :: filename
  integer, intent(in) :: cmode
  integer, intent(out) :: ncid
  logical, intent(in) :: useMPI
  integer, intent(in) :: MPI_comm, MPI_info

  ! Ignore useMPI parameter, not supported by NetCDF.
  s = nf90_create(path = filename, cmode = cmode, ncid = ncid)
end subroutine wrap_nf90_create

subroutine wrap_nf90_open(s, filename, mode, ncid, useMPI, MPI_comm, MPI_info)
  use netcdf
  implicit none
  integer, intent(out) :: s
  character(len = *), intent(in) :: filename
  integer, intent(in) :: mode
  integer, intent(out) :: ncid
  logical, intent(in) :: useMPI
  integer, intent(in) :: MPI_comm, MPI_info

  ! Ignore useMPI parameter, not supported by NetCDF.
  s = nf90_open(path = filename, mode = mode, ncid = ncid)
end subroutine wrap_nf90_open

logical function wrap_nf90_support_parallel()
  wrap_nf90_support_parallel = .false.
end function wrap_nf90_support_parallel
