# `netcdf4_wrappers.f90` - NetCDF 4 Wrappers with Parallel I/O Support

## Overview

This file provides wrapper subroutines for NetCDF operations, designed to be used in environments where NetCDF-4 features are available and parallel I/O capabilities (via MPI) are intended to be utilized. It works in conjunction with the main ETSF I/O library, likely being selected at compile time over `netcdf3_wrappers.f90` when parallel support is desired and available in the linked NetCDF library.

The key functions of these wrappers are:
-   To enable parallel NetCDF file creation and opening by appropriately setting mode flags and passing MPI communicator information to the standard NetCDF routines.
-   To consistently report that parallel I/O is supported via `wrap_nf90_support_parallel()`.
-   To ensure that files created in parallel mode are explicitly set to use the NetCDF-4 classic model format.

## Key Components

The subroutines and function in this file are standalone and are typically made available by including this file directly or by being part of a module that conditionally includes it. (The provided source does not show them inside a module, but they might be included into one, potentially named `netcdf_wrapper_module`).

### Subroutines and Functions

-   **`wrap_nf90_create(s, path, cmode, ncid, useMPI, MPI_comm, MPI_info)`**:
    -   **Overview**: Wraps the NetCDF file creation call. If `useMPI` is `.TRUE.`, it modifies the creation mode `cmode` to include flags for NetCDF-4 format (`NF90_NETCDF4`), classic model (`NF90_CLASSIC_MODEL`), and a flag indicating parallel I/O (by adding 16384, which corresponds to `NF90_MPIIO` or `NF90_MPIPOSIX` in some NetCDF versions, or is interpreted by MPI-enabled NetCDF libraries). It then calls the standard `nf90_create` routine, passing the MPI communicator and info objects. If `useMPI` is `.FALSE.`, it calls `nf90_create` for serial access.
    -   **Arguments**:
        -   `s` (`INTEGER, INTENT(OUT)`): Status code from the NetCDF call.
        -   `path` (`CHARACTER(LEN=*), INTENT(IN)`): File path for creation.
        -   `cmode` (`INTEGER, INTENT(IN)`): Base creation mode flags (e.g., `NF90_CLOBBER`).
        -   `ncid` (`INTEGER, INTENT(OUT)`): NetCDF ID of the created file.
        -   `useMPI` (`LOGICAL, INTENT(IN)`): Flag indicating if parallel creation is requested.
        -   `MPI_comm` (`INTEGER, INTENT(IN)`): MPI communicator (used if `useMPI` is `.TRUE.`).
        -   `MPI_info` (`INTEGER, INTENT(IN)`): MPI info object (used if `useMPI` is `.TRUE.`).
    -   **Functionality**:
        -   If `useMPI` is `.TRUE.`:
            -   `cmode_ = ior(cmode, nf90_netcdf4)`
            -   `cmode_ = ior(cmode_, nf90_classic_model)`
            -   Calls `nf90_create(path, cmode_ + 16384, ncid, comm=MPI_comm, info=MPI_info)`. The value `16384` is a numerical flag for parallel I/O.
        -   Else (serial access):
            -   Calls `nf90_create(path, cmode, ncid)`.
        -   The status `s` is set by the chosen NetCDF function.

-   **`wrap_nf90_open(s, path, omode, ncid, useMPI, MPI_comm, MPI_info)`**:
    -   **Overview**: Wraps the NetCDF file opening call. If `useMPI` is `.TRUE.`, it modifies the open mode `omode` by adding the parallel I/O flag (16384) and then calls the standard `nf90_open` routine, passing the MPI communicator and info objects. If `useMPI` is `.FALSE.`, it calls `nf90_open` for serial access.
    -   **Arguments**:
        -   `s` (`INTEGER, INTENT(OUT)`): Status code.
        -   `path` (`CHARACTER(LEN=*), INTENT(IN)`): File path to open.
        -   `omode` (`INTEGER, INTENT(IN)`): Base open mode flags (e.g., `NF90_NOWRITE`, `NF90_WRITE`).
        -   `ncid` (`INTEGER, INTENT(OUT)`): NetCDF ID of the opened file.
        -   `useMPI` (`LOGICAL, INTENT(IN)`): Flag for parallel opening.
        -   `MPI_comm` (`INTEGER, INTENT(IN)`): MPI communicator.
        -   `MPI_info` (`INTEGER, INTENT(IN)`): MPI info object.
    -   **Functionality**:
        -   If `useMPI` is `.TRUE.`:
            -   Calls `nf90_open(path, omode + 16384, ncid, comm=MPI_comm, info=MPI_info)`.
        -   Else (serial access):
            -   Calls `nf90_open(path, omode, ncid)`.
        -   The status `s` is set accordingly.

-   **`wrap_nf90_support_parallel()`**:
    -   **Overview**: A logical function that indicates whether this set of wrappers is configured to support parallel NetCDF I/O.
    -   **Returns**: (`LOGICAL`) Always returns `.TRUE.`.
    -   **Functionality**: This function consistently returns `.TRUE.`, signifying that these wrappers are intended for use with a NetCDF library capable of parallel operations (when MPI arguments are provided).

## Important Variables/Constants

-   **`nf90_netcdf4` (NetCDF Constant)**: Used in `wrap_nf90_create` to ensure files created in parallel are in NetCDF-4 format.
-   **`nf90_classic_model` (NetCDF Constant)**: Used in `wrap_nf90_create` in conjunction with `nf90_netcdf4` to specify the NetCDF-4 classic model format for parallel created files.
-   **Numerical value `16384`**: This integer is added to the mode flags for `nf90_create` and `nf90_open` when `useMPI` is true. This value corresponds to `NF90_MPIIO` or `NF90_MPIPOSIX` flags found in NetCDF header files, used to signal the library to perform parallel I/O using the provided MPI communicator and info objects.

## Usage Examples

These wrappers are used internally by the `etsf_io_low_level` module, specifically in routines like `etsf_io_low_open_create` and `etsf_io_low_open_modify`. When this version of the wrappers is compiled, the ETSF I/O library can perform parallel file operations if the calling code provides MPI communicators and the underlying NetCDF library supports MPI.

```fortran
! Conceptual example within a higher-level ETSF I/O routine
SUBROUTINE etsf_create_file_potentially_parallel(filepath, clobber_mode, &
                                               use_parallel_io, mpi_comm_handle, mpi_info_handle, &
                                               ncid_returned, status_returned)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: filepath
    INTEGER, INTENT(IN) :: clobber_mode ! e.g., NF90_CLOBBER or NF90_NOCLOBBER
    LOGICAL, INTENT(IN) :: use_parallel_io
    INTEGER, INTENT(IN) :: mpi_comm_handle, mpi_info_handle
    INTEGER, INTENT(OUT) :: ncid_returned
    INTEGER, INTENT(OUT) :: status_returned

    ! These calls would resolve to the ones in netcdf4_wrappers.f90 if linked
    IF (use_parallel_io .AND. wrap_nf90_support_parallel()) THEN
        PRINT *, "Attempting parallel file creation using NetCDF-4 wrappers."
        CALL wrap_nf90_create(status_returned, filepath, clobber_mode, ncid_returned, &
                            & .TRUE., mpi_comm_handle, mpi_info_handle)
    ELSE
        PRINT *, "Attempting serial file creation."
        CALL wrap_nf90_create(status_returned, filepath, clobber_mode, ncid_returned, &
                            & .FALSE., 0, 0) ! MPI args conventionally 0 or ignored for serial
    END IF

    IF (status_returned == NF90_NOERR) THEN
        ! Proceed with file definitions
    ELSE
        ! Handle error
    END IF
END SUBROUTINE etsf_create_file_potentially_parallel
```

## Dependencies and Interactions

### Internal Dependencies
-   These wrappers are called by higher-level routines within the ETSF I/O library, such as those found in `src/low_level/write_routines.f90` (which includes `etsf_io_low_open_create` and `etsf_io_low_open_modify`).

### External Library Dependencies
-   **NetCDF Library (`netcdf` module)**: Directly calls the standard NetCDF functions:
    -   `nf90_create`
    -   `nf90_open`
    It relies on these functions being able to handle MPI communicator/info arguments and modified mode flags if the NetCDF library was built with parallel support. It also uses NetCDF constants like `NF90_NETCDF4`, `NF90_CLASSIC_MODEL`.
-   **MPI Library**: An MPI implementation is required at runtime if parallel I/O is used, as valid MPI communicator and info handles must be passed to the NetCDF calls.

### Global Variables/Modules Accessed
-   Relies on the NetCDF module (`use netcdf`) being accessible for function definitions and constants.
-   The function `wrap_nf90_support_parallel()` from this file will always indicate that parallel I/O is supported by these wrappers.
```
