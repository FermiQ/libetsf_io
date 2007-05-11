!!****h* utils/etsf_io_tools
!! NAME
!!  etsf_io_tools
!!
!! FUNCTION
!!  This module contains different non mandatory routines to handle internals
!!  from ETSF files. It actually contains:
!!  * etsf_io_tools_get_atom_names(): a routine to read the three variables defining
!!                                    atoms and returning names informations as
!!                                    defined in the specifications.
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!***
module etsf_io_tools

  use etsf_io_low_level
  use etsf_io

  implicit none

  private

  public :: etsf_io_tools_get_atom_names

contains

!!****m* etsf_io_tools/etsf_io_tools_get_atom_names
!! NAME
!!  etsf_io_tools_get_atom_names
!!
!! FUNCTION
!!  In the specifications, atom names can be read from these three variables:
!!  atomic_numbers, atom_species_names and chemical_symbols. The first listed
!!  variable is prefered. This routine is a convenient way to access to atom
!!  names directly, following specifications preferences.
!!
!!  The output argument @atom_numbers is set if the NetCDF variable 'atomic_numbers'
!!  is present, and @atom_names contains 'atom_species_names' or 'chemical_symbols'
!!  if present or a string equivalent to 'atomic_numbers' if not. Then a string
!!  representation is always available.
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!! INPUTS
!! * ncid = 
!!     an opened NetCDF file with read access.
!! OUTPUT
!! * atom_names = an allocated array to store the atom names (indexed by species).
!! * lstat = 
!!     return .true. if all the actions succeed.
!! * error_data <type(etsf_io_low_error)> = 
!!     contains the details of the error is @lstat is false.
!! * atom_numbers = <optional> a pointer to store the atomic numbers, it will be
!!                  associated only if 'atomic_numbers' variable is present.
!!
!! SOURCE
  subroutine etsf_io_tools_get_atom_names(ncid, atom_names, lstat, error_data, &
       & atom_numbers)
    integer, intent(in)                        :: ncid
    character(len = etsf_charlen), intent(out) :: atom_names(:)
    logical, intent(out)                       :: lstat
    type(etsf_io_low_error), intent(out)       :: error_data
    double precision, pointer, optional        :: atom_numbers(:)

    ! Local variables
    character(len = *), parameter            :: me = "etsf_io_tools_get_atom_names"
    logical                                  :: valid
    double precision, pointer                :: my_atom_numbers(:)
    character(len=etsf_chemlen), allocatable :: symbols(:)
    integer                                  :: number_of_atom_species, i

    if (present(atom_numbers)) then
       atom_numbers => null()
    end if

    ! Read the array dimension.
    call etsf_io_low_read_dim(ncid, "number_of_atom_species", &
         & number_of_atom_species, lstat, error_data = error_data)
    if (.not. lstat) return

    ! Check that argument size matches number_of_atoms.
    if (size(atom_names) /= number_of_atom_species) then
       call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_ARG, me, &
            & tgtname = "atom_names", errmess = "wrong argument size.")
       lstat = .false.
       return
    end if

    valid = .false.
    allocate(my_atom_numbers(number_of_atom_species))
    call etsf_io_low_read_var(ncid, "atomic_numbers", &
         & my_atom_numbers, lstat, error_data = error_data)
    if (.not. lstat .and. error_data%access_mode_id /= ERROR_MODE_INQ) then
       ! Case where variable is found but has problems.
       deallocate(my_atom_numbers)
       return
    end if
    if (lstat) then
       ! Ok, we have the informations we were looking for.
       ! But we will try for better string description than figures.
       do i = 1, number_of_atom_species, 1
          write(atom_names(i), "(F6.2)") my_atom_numbers(i)
       end do
       if (present(atom_numbers)) then
          atom_numbers => my_atom_numbers
       else
          deallocate(my_atom_numbers)
       end if
       valid = .true.
    else
       deallocate(my_atom_numbers)
    end if

    ! 'atomic_numbers' was not found, try to fall back to 'atom_species_names'
    call etsf_io_low_read_var(ncid, "atom_species_names", &
         & atom_names, etsf_charlen, lstat, error_data = error_data)
    if (.not. lstat .and. error_data%access_mode_id /= ERROR_MODE_INQ) then
       ! Case where variable is found but has problems.
       return
    end if
    if (lstat) then
       ! Ok, we have the informations we were looking for.
       ! But we check that given values are not void.
       valid = .true.
       do i = 1, number_of_atom_species, 1
          call strip(atom_names(i))
          valid = valid .and. (trim(atom_names(i)) /= "")
       end do
       if (valid) return
    end if

    ! 'atomic_numbers' was not found, try to fall back to 'atom_species_names'
    allocate(symbols(number_of_atom_species))
    call etsf_io_low_read_var(ncid, "chemical_symbols", &
         & symbols, etsf_chemlen, lstat, error_data = error_data)
    if (.not. lstat .and. error_data%access_mode_id /= ERROR_MODE_INQ) then
       ! Case where variable is found but has problems.
       deallocate(symbols)
       return
    end if
    if (lstat) then
       ! Ok, we have the informations we were looking for.
       do i = 1, number_of_atom_species, 1
          call strip(symbols(i))
          write(atom_names(i), "(A)") symbols(i)
       end do
       deallocate(symbols)
       return
    end if
    deallocate(symbols)
    
    ! If nthing has worked, we raise an error.
    if (.not. lstat .and. .not. valid) then
       call etsf_io_low_error_set(error_data, ERROR_MODE_INQ, ERROR_TYPE_VAR, me, &
            & tgtname = "atomic_numbers, atom_species_names, chemical_symbols", &
            & errmess = "no variable exists, can't get atom names.")
    end if
  end subroutine etsf_io_tools_get_atom_names
!!***

end module etsf_io_tools
