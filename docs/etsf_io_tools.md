# `etsf_io_tools.f90` - Utility Tools for ETSF Data

## Overview

This file is part of the `etsf_io_tools` module and provides various helper subroutines for common tasks when working with ETSF (Electronic Structure Task Force) formatted data files. These tools offer convenient ways to extract specific information like atom names or to get/set flags related to ETSF data conventions, such as time-reversal symmetry for plane-wave calculations.

## Key Components

### Module: `etsf_io_tools`
This module encapsulates the utility subroutines.

### Public Subroutines

-   **`etsf_io_tools_get_atom_names(ncid, atom_names, lstat, error_data, atom_numbers)`**:
    -   **Overview**: Retrieves atom names from an ETSF file. It follows a specific order of preference for sourcing these names:
        1.  `atomic_numbers`: If found, these are converted to string representations for `atom_names`.
        2.  `atom_species_names`: If `atomic_numbers` is not found or if string names are preferred by the logic, this variable is tried.
        3.  `chemical_symbols`: If neither of the above yields satisfactory names, this variable is used.
        The routine ensures that `atom_names` is populated if any of these sources are valid.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF identifier of the opened ETSF file.
        -   `atom_names(:)` (`CHARACTER(LEN=etsf_charlen), INTENT(OUT)`): Array to be filled with atom names. Its size must match the `number_of_atom_species` dimension in the file.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation (`.TRUE.` if successful).
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT)`): Structure for error reporting.
        -   `atom_numbers(:)` (`DOUBLE PRECISION, POINTER, OPTIONAL, INTENT(OUT)`): If present and the `atomic_numbers` variable exists in the file and is successfully read, this pointer will be associated with an allocated array containing the read atomic numbers. Otherwise, it will be nullified.
    -   **Functionality**:
        1.  Reads the `number_of_atom_species` dimension to know the expected size.
        2.  Checks if the provided `atom_names` array has the correct size.
        3.  Attempts to read `atomic_numbers`. If successful:
            -   Converts these numbers to formatted strings (F6.2) and stores them in `atom_names`.
            -   If the `atom_numbers` argument is present, allocates memory for `my_atom_numbers`, copies the read data, and associates `atom_numbers` with `my_atom_numbers`.
            -   Sets an internal `valid` flag to `.TRUE.`.
        4.  If `atomic_numbers` was not found (or if logic implies preference for string names even if `atomic_numbers` was found but `valid` is still false, though the current code sets `valid=.true.` if `atomic_numbers` is read):
            -   Attempts to read `atom_species_names` into `atom_names`. If successful and the read names are not blank (after stripping spaces), the operation is considered successful, and `valid` is set.
        5.  If `atom_species_names` was not found or provided only blank names:
            -   Attempts to read `chemical_symbols`. If successful, these symbols are stripped of trailing spaces and copied to `atom_names`.
        6.  If none of the above methods result in `lstat` being true from a read operation or `valid` being true from a successful name extraction, `lstat` is set to `.FALSE.`, and an error is reported indicating that no suitable variable for atom names was found.

-   **`etsf_io_tools_get_time_reversal_symmetry(ncid, symmetry, lstat, error_data)`**:
    -   **Overview**: Checks if time-reversal symmetry at the Gamma point is indicated as used for a plane-wave basis set. It verifies that the `basis_set` variable is "plane_waves" and then checks for consistency of the `use_time_reversal_at_gamma` attribute on both `reduced_coordinates_of_plane_waves` and `coefficients_of_wavefunctions` variables.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF identifier of the opened ETSF file.
        -   `symmetry` (`LOGICAL, INTENT(OUT)`): `.TRUE.` if time-reversal symmetry is consistently indicated as "yes", `.FALSE.` otherwise (including if not applicable or inconsistently defined).
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation and checks. `.TRUE.` if checks are performed successfully and values are consistent.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT)`): Error reporting structure.
    -   **Functionality**:
        1.  Reads the `basis_set` variable. If not "plane_waves", sets `lstat` to `.FALSE.` with an appropriate error and returns.
        2.  Attempts to read the `use_time_reversal_at_gamma` attribute from `reduced_coordinates_of_plane_waves`. If the attribute is not found (and no other read error occurs), its value defaults to "no".
        3.  Attempts to read the `use_time_reversal_at_gamma` attribute from `coefficients_of_wavefunctions`. If not found, its value also defaults to "no".
        4.  Checks if the two attribute values (potentially defaulted to "no") are consistent (i.e., both start with 'y' for "yes" or both start with 'n' for "no"). If inconsistent, sets `lstat` to `.FALSE.` with an error and returns.
        5.  If consistent, sets `symmetry` to `.TRUE.` if the (consistent) attribute value indicates "yes" (starts with 'y'). Sets `lstat` to `.TRUE.`.

-   **`etsf_io_tools_set_time_reversal_symmetry(ncid, symmetry, lstat, error_data)`**:
    -   **Overview**: Sets the `use_time_reversal_at_gamma` attribute on both `reduced_coordinates_of_plane_waves` and `coefficients_of_wavefunctions` variables to indicate whether time-reversal symmetry at Gamma is used. It first verifies that the `basis_set` variable in the file is "plane_waves". The file must be opened with write access.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF identifier of the opened ETSF file (must be writable).
        -   `symmetry` (`LOGICAL, INTENT(IN)`): The desired symmetry status to set: `.TRUE.` will write "yes", `.FALSE.` will write "no".
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT)`): Error reporting structure.
    -   **Functionality**:
        1.  Reads the `basis_set` variable. If not "plane_waves", sets `lstat` to `.FALSE.` with an error and returns.
        2.  Puts the file into define mode using `etsf_io_low_set_define_mode`.
        3.  Determines the attribute string: "yes" if `symmetry` is `.TRUE.`, "no" otherwise.
        4.  Writes this attribute string to `reduced_coordinates_of_plane_waves` using `etsf_io_low_write_att`.
        5.  Writes the same attribute string to `coefficients_of_wavefunctions` using `etsf_io_low_write_att`.
        6.  Sets `lstat` to `.TRUE.` if all operations succeed. (Note: The file is left in define mode; the caller might need to call `etsf_io_low_set_write_mode` or `etsf_io_low_close` which handles this).

## Important Variables/Constants

Constants from `etsf_io` module:
-   `etsf_charlen`: Default character length for strings like `atom_names` and `basis_set`.
-   `etsf_chemlen`: Character length for `chemical_symbols`.

Local constants within subroutines:
-   `me` (`CHARACTER(LEN=*), PARAMETER`): Used in each subroutine (e.g., `me = "etsf_io_tools_get_atom_names"`) to store its own name for error backtracing purposes.

## Usage Examples

**Getting atom names:**
```fortran
USE etsf_io_tools
USE etsf_io_low_level, ONLY: etsf_io_low_error, etsf_io_low_error_handle, &
                             etsf_io_low_open_read, etsf_io_low_close, &
                             etsf_io_low_read_dim
USE etsf_io, ONLY: etsf_charlen
IMPLICIT NONE

INTEGER :: ncid, num_species, i
CHARACTER(LEN=etsf_charlen), ALLOCATABLE :: names_arr(:)
DOUBLE PRECISION, POINTER :: atomic_numbers_ptr(:) => NULL()
LOGICAL :: status_op
TYPE(etsf_io_low_error) :: error_info

CALL etsf_io_low_open_read(ncid, "my_molecule.nc", status_op, error_data=error_info)
IF (.NOT. status_op) THEN; CALL etsf_io_low_error_handle(error_info); STOP 1; END IF

! Determine number_of_atom_species to allocate names_arr array
CALL etsf_io_low_read_dim(ncid, "number_of_atom_species", num_species, status_op, error_data=error_info)
IF (.NOT. status_op) THEN
    CALL etsf_io_low_error_handle(error_info)
    CALL etsf_io_low_close(ncid, status_op, error_data=error_info)
    STOP 1
END IF

ALLOCATE(names_arr(num_species))

CALL etsf_io_tools_get_atom_names(ncid, names_arr, status_op, error_info, atom_numbers=atomic_numbers_ptr)
IF (status_op) THEN
    DO i = 1, num_species
        WRITE(*,*) "Species ", i, ": Name='", TRIM(names_arr(i)), "'"
        IF (PRESENT(atomic_numbers_ptr) .AND. ASSOCIATED(atomic_numbers_ptr)) THEN
            WRITE(*,*) "  Atomic Number: ", atomic_numbers_ptr(i)
        END IF
    END DO
ELSE
    CALL etsf_io_low_error_handle(error_info)
END IF

IF (ASSOCIATED(atomic_numbers_ptr)) DEALLOCATE(atomic_numbers_ptr)
DEALLOCATE(names_arr)
CALL etsf_io_low_close(ncid, status_op, error_data=error_info)
```

## Dependencies and Interactions

### Internal Module Dependencies
-   This file defines the `etsf_io_tools` module. The subroutines are all part of this module.

### External ETSF I/O Library Dependencies
-   **`etsf_io_low_level` module**: Extensively used for:
    -   `etsf_io_low_error` type and error handling constants/routines (`etsf_io_low_error_set`, `etsf_io_low_error_update`).
    -   `etsf_io_low_read_dim` for reading dimension sizes.
    -   `etsf_io_low_read_var` for reading variables (`atomic_numbers`, `atom_species_names`, `chemical_symbols`, `basis_set`).
    -   `etsf_io_low_read_att` for reading attributes (`use_time_reversal_at_gamma`).
    -   `etsf_io_low_write_att` for writing the `use_time_reversal_at_gamma` attribute.
    -   `etsf_io_low_set_define_mode` to switch file mode for writing attributes.
    -   `strip` utility for character strings.
-   **`etsf_io` module**: Provides constants such as `etsf_charlen` and `etsf_chemlen`. Error constants like `ERROR_MODE_INQ`, `ERROR_TYPE_VAR` etc. are also used (though defined in `etsf_io_low_level`, they are often accessed via `etsf_io` if it re-exports them).

### External Library Dependencies
-   Indirectly depends on the NetCDF library through its use of `etsf_io_low_level` routines.

### Global Variables/Modules Accessed
-   Uses parameters and types from `etsf_io_low_level` and `etsf_io`.
```
