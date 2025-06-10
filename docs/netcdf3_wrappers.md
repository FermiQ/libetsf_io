# `netcdf3_wrappers.f90` - NetCDF 3 Serial I/O Wrappers

## Overview

This file provides wrapper subroutines for NetCDF file creation and opening operations. Contrary to what its name might broadly imply in a context with parallel I/O, this specific implementation **always performs serial NetCDF operations** (`nf90_create`, `nf90_open`) and **reports that parallel I/O is not supported**.

The primary purpose of these wrappers within the ETSF I/O library, when this file is used (likely through conditional compilation), is to:
-   Provide a consistent interface for file creation and opening operations, regardless of whether a parallel NetCDF library is available or being used.
-   Explicitly use the standard serial NetCDF functions.
-   Signal that parallel capabilities are disabled through `wrap_nf90_support_parallel()`.

This file is likely part of a pair with `netcdf4_wrappers.f90`, where the latter would contain the actual logic for parallel NetCDF operations.

## Key Components

The subroutines and function in this file are standalone and are typically made available by including this file directly or by being part of a module that conditionally includes it. (The provided source does not show them inside a module, but they might be included into one).

### Subroutines and Functions

-   **`wrap_nf90_create(s, path, cmode, ncid, useMPI, MPI_comm, MPI_info)`**:
    -   **Overview**: Wraps the standard serial NetCDF file creation call (`nf90_create`). The arguments related to parallel I/O (`useMPI`, `MPI_comm`, `MPI_info`) are ignored.
    -   **Arguments**:
        -   `s` (`INTEGER, INTENT(OUT)`): Status code from the `nf90_create` call.
        -   `path` (`CHARACTER(LEN=*), INTENT(IN)`): File path for creation.
        -   `cmode` (`INTEGER, INTENT(IN)`): Creation mode flags (e.g., `NF90_CLOBBER`, `NF90_NOCLOBBER`).
        -   `ncid` (`INTEGER, INTENT(OUT)`): NetCDF ID of the created file.
        -   `useMPI` (`LOGICAL, INTENT(IN)`): *Ignored in this implementation.*
        -   `MPI_comm` (`INTEGER, INTENT(IN)`): *Ignored in this implementation.*
        -   `MPI_info` (`INTEGER, INTENT(IN)`): *Ignored in this implementation.*
    -   **Functionality**: Calls `nf90_create(path, cmode, ncid)` and sets the output status `s`.

-   **`wrap_nf90_open(s, path, omode, ncid, useMPI, MPI_comm, MPI_info)`**:
    -   **Overview**: Wraps the standard serial NetCDF file opening call (`nf90_open`). The arguments related to parallel I/O are ignored.
    -   **Arguments**:
        -   `s` (`INTEGER, INTENT(OUT)`): Status code from the `nf90_open` call.
        -   `path` (`CHARACTER(LEN=*), INTENT(IN)`): File path to open.
        -   `omode` (`INTEGER, INTENT(IN)`): Open mode flags (e.g., `NF90_NOWRITE`, `NF90_WRITE`).
        -   `ncid` (`INTEGER, INTENT(OUT)`): NetCDF ID of the opened file.
        -   `useMPI` (`LOGICAL, INTENT(IN)`): *Ignored in this implementation.*
        -   `MPI_comm` (`INTEGER, INTENT(IN)`): *Ignored in this implementation.*
        -   `MPI_info` (`INTEGER, INTENT(IN)`): *Ignored in this implementation.*
    -   **Functionality**: Calls `nf90_open(path, omode, ncid)` and sets the output status `s`.

-   **`wrap_nf90_support_parallel()`**:
    -   **Overview**: A logical function that indicates whether parallel NetCDF I/O is supported by this wrapper set.
    -   **Returns**: (`LOGICAL`) Always returns `.FALSE.`.
    -   **Functionality**: This function consistently returns `.FALSE.`, signifying that this set of wrappers does not provide or support parallel NetCDF operations.

## Important Variables/Constants

This file does not define any public Fortran parameters or constants. Its behavior regarding parallel support is hardcoded.

## Usage Examples

These wrappers are intended for internal use by the `etsf_io_low_level` module, specifically within routines like `etsf_io_low_open_create` and `etsf_io_low_open_modify`. When this version of the wrappers is compiled into the library, ETSF I/O operations will be serial.

Conceptual example of how `etsf_io_low_open_create` might use `wrap_nf90_create` from this file:
```fortran
! Within etsf_io_low_level module or similar,
! assuming this wrapper version is linked/included.

SUBROUTINE etsf_io_low_open_create(ncid_out, file_path, ..., mpi_comm_in, mpi_info_in, lstat, err)
    ! No specific "USE netcdf_wrapper_module" if these are directly included or globally available
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: ncid_out
    CHARACTER(LEN=*), INTENT(IN) :: file_path
    INTEGER, INTENT(IN), OPTIONAL :: mpi_comm_in, mpi_info_in ! These will be ignored by this wrapper
    LOGICAL, INTENT(OUT) :: lstat
    TYPE(etsf_io_low_error), INTENT(OUT) :: err

    INTEGER :: status_nc, nc_cmode
    LOGICAL :: use_parallel_arg ! This argument to wrap_nf90_create will be ignored

    nc_cmode = NF90_NOCLOBBER
    use_parallel_arg = PRESENT(mpi_comm_in) .AND. PRESENT(mpi_info_in)

    ! Even if mpi_comm_in and mpi_info_in are present, this call will resolve to serial nf90_create
    CALL wrap_nf90_create(status_nc, file_path, nc_cmode, ncid_out, &
                        & use_parallel_arg, mpi_comm_in, mpi_info_in)

    IF (status_nc /= NF90_NOERR) THEN
        lstat = .FALSE.
        ! Set error structure (err)
        RETURN
    END IF
    ! ... proceed with setting attributes etc. ...
    lstat = .TRUE.
END SUBROUTINE
```

## Dependencies and Interactions

### Internal Dependencies
-   These wrapper functions are called by higher-level routines in the ETSF I/O library, likely found in `src/low_level/write_routines.f90` (which contains `etsf_io_low_open_create` and `etsf_io_low_open_modify`).

### External Library Dependencies
-   **NetCDF Library (`netcdf` module)**: This module directly calls the standard serial NetCDF functions:
    -   `nf90_create`
    -   `nf90_open`
    It relies on constants like `NF90_NOERR`, `NF90_CLOBBER`, `NF90_NOWRITE` from the NetCDF module being available in the calling scope.

### Global Variables/Modules Accessed
-   Relies on the NetCDF module (`use netcdf`) being accessible for the definitions of `nf90_create` and `nf90_open`.
```
