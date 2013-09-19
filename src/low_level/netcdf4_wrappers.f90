subroutine wrap_nf90_create(s, filename, cmode, ncid, useMPI, MPI_comm, MPI_info)
  use netcdf
  implicit none
  integer, intent(out) :: s
  character(len = *), intent(in) :: filename
  integer, intent(in) :: cmode
  integer, intent(out) :: ncid
  logical, intent(in) :: useMPI
  integer, intent(in) :: MPI_comm, MPI_info

  integer :: cmode_

  if (useMPI) then
     cmode_ = ior(cmode, nf90_netcdf4)
     cmode_ = ior(cmode_, nf90_classic_model)
     !cmode_ = ior(cmode_, nf90_mpiposix)
     s = nf90_create(path = filename, cmode = cmode_ + 16384, ncid = ncid, &
          & comm = mpi_comm, info = mpi_info)
  else
     s = nf90_create(path = filename, cmode = cmode, ncid = ncid)
  end if
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

  if (useMPI) then
     s = nf90_open(path = filename, mode = mode + 16384, ncid = ncid, &
          & comm = mpi_comm, info = mpi_info)
  else
     s = nf90_open(path = filename, mode = mode, ncid = ncid)
  end if
end subroutine wrap_nf90_open

logical function wrap_nf90_support_parallel()
  wrap_nf90_support_parallel = .true.
end function wrap_nf90_support_parallel
