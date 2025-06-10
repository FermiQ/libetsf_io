# `write_routines.f90` - Low-Level ETSF File Write Routines

## Overview

This file implements various low-level subroutines for writing data and metadata to ETSF (Electronic Structure Task Force) files. These routines are integral to the `etsf_io_low_level` module and provide the core functionality for creating and modifying ETSF files. They handle direct interactions with the NetCDF library for defining dimensions, variables, attributes, and writing data to these variables.

## Key Components

### Subroutines

-   **`etsf_io_low_open_create(ncid, filename, version, lstat, title, history, error_data, with_etsf_header, overwrite, mpi_comm, mpi_info)`**:
    -   **Overview**: Creates a new ETSF file. It sets up the basic ETSF header attributes (file_format, file_format_version, Conventions) and can optionally set a title and history. The file is left in define mode.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(OUT)`): NetCDF file identifier for the created file.
        -   `filename` (`CHARACTER(LEN=*), INTENT(IN)`): Path for the new file.
        -   `version` (`REAL, INTENT(IN)`): ETSF file format version to write in the header.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation.
        -   `title` (`CHARACTER(LEN=*), INTENT(IN), OPTIONAL`): Title for the file (max 80 chars).
        -   `history` (`CHARACTER(LEN=*), INTENT(IN), OPTIONAL`): Initial history entry (max 1024 chars).
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error reporting structure.
        -   `with_etsf_header` (`LOGICAL, INTENT(IN), OPTIONAL`): If `.TRUE.` (default), writes standard ETSF header.
        -   `overwrite` (`LOGICAL, INTENT(IN), OPTIONAL`): If `.TRUE.`, overwrites the file if it exists. Default is `.FALSE.`.
        -   `mpi_comm` (`INTEGER, INTENT(IN), OPTIONAL`): MPI communicator for parallel I/O.
        -   `mpi_info` (`INTEGER, INTENT(IN), OPTIONAL`): MPI info object for parallel I/O.
    -   **Functionality**: Calls `wrap_nf90_create` (a wrapper around `nf90_create` or `nf90_create_par`). If `with_etsf_header` is true, it then calls `nf90_put_att` to write standard global attributes like "file_format", "file_format_version", "Conventions", and optional "title" and "history".

-   **`etsf_io_low_open_modify(ncid, filename, lstat, title, history, version, error_data, with_etsf_header, mpi_comm, mpi_info)`**:
    -   **Overview**: Opens an existing ETSF file for modification. It can check the ETSF header and update title, history, or version. The file is left in define mode.
    -   **Arguments**: (Largely similar to `etsf_io_low_open_create`, but `version` is optional for updating). `filename` is the path to the existing file.
    -   **Functionality**: Calls `wrap_nf90_open` (wrapper for `nf90_open` or `nf90_open_par`) with `NF90_WRITE`. If `with_etsf_header` is true, it calls `etsf_io_low_check_header`. Then, it switches to define mode using `etsf_io_low_set_define_mode` and updates attributes ("title", "file_format_version", "history") if provided.

-   **`etsf_io_low_write_dim(ncid, dimname, dimvalue, lstat, ncdimid, error_data)`**:
    -   **Overview**: Defines a new dimension in the NetCDF file or checks if an existing dimension matches the given value. The file must be in define mode.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file identifier (must be in define mode).
        -   `dimname` (`CHARACTER(LEN=*), INTENT(IN)`): Name of the dimension.
        -   `dimvalue` (`INTEGER, INTENT(IN)`): Length of the dimension.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation.
        -   `ncdimid` (`INTEGER, INTENT(OUT), OPTIONAL`): NetCDF ID of the dimension.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: First tries to read the dimension using `etsf_io_low_read_dim`. If it exists and matches `dimvalue`, `lstat` is `.TRUE.`. If it exists but differs, `lstat` is `.FALSE.` with an error. If it doesn't exist, `nf90_def_dim` is called to create it.

-   **`etsf_io_low_def_var_0D(ncid, varname, vartype, lstat, ncvarid, error_data)`**:
    -   **Overview**: Defines a scalar (0-dimensional) variable. Part of the `etsf_io_low_def_var` interface. The file must be in define mode.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file ID (define mode).
        -   `varname` (`CHARACTER(LEN=*), INTENT(IN)`): Name of the variable.
        -   `vartype` (`INTEGER, INTENT(IN)`): NetCDF data type of the variable (e.g., `ETSF_IO_LOW_INTEGER`).
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Operation status.
        -   `ncvarid` (`INTEGER, INTENT(OUT), OPTIONAL`): NetCDF ID of the new variable.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: Checks if the variable already exists using `etsf_io_low_read_var_infos`. If it exists and matches (type and 0D shape), `lstat` is `.TRUE.`. If it exists but definition differs, an error is reported. If new, calls `nf90_def_var` with no dimensions.

-   **`etsf_io_low_def_var_nD(ncid, varname, vartype, vardims, lstat, ncvarid, error_data)`**:
    -   **Overview**: Defines an N-dimensional variable. Part of the `etsf_io_low_def_var` interface. The file must be in define mode.
    -   **Arguments**:
        -   `vardims(:)` (`CHARACTER(LEN=*), INTENT(IN)`): Array of dimension names for the variable.
        -   (Other arguments similar to `etsf_io_low_def_var_0D`)
    -   **Functionality**: Similar to `_0D`, but first resolves dimension names in `vardims` to their NetCDF IDs and lengths using `etsf_io_low_read_dim`. Then checks for existing variable consistency. If new, calls `nf90_def_var` with the array of resolved dimension IDs.

-   **`etsf_io_low_copy_all_att(ncid_from, ncid_to, ncvarid_from, ncvarid_to, lstat, error_data)`**:
    -   **Overview**: Copies all attributes from a source variable (or global) to a target variable (or global), potentially between different files. The target file (`ncid_to`) must be in define mode.
    -   **Arguments**:
        -   `ncid_from` (`INTEGER, INTENT(IN)`): NetCDF ID of the source file.
        -   `ncid_to` (`INTEGER, INTENT(IN)`): NetCDF ID of the target file (must be in define mode).
        -   `ncvarid_from` (`INTEGER, INTENT(IN)`): Variable ID in source (or `etsf_io_low_global_att`).
        -   `ncvarid_to` (`INTEGER, INTENT(IN)`): Variable ID in target (or `etsf_io_low_global_att`).
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Operation status.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: Reads attribute names from the source using `read_var_infos_id` (if `ncvarid_from` is a variable) or `nf90_inquire` and `nf90_inq_attname` (if global). Then iterates and calls `nf90_copy_att` for each attribute.

-   **`write_var_double_var(ncid, varname, var, lstat, start, count, map, ncvarid, error_data)`**:
    -   **Overview**: These are specific implementations for the generic `etsf_io_low_write_var` interface, designed for `etsf_io_low_var_double` type. They determine the dimensionality of the associated pointer (`data1D` to `data7D`) within the `var` argument and call the corresponding dimension-specific write routine (e.g., `write_var_double_1D`, `write_var_double_3D`). The file must be in data mode.
    -   **Arguments**: Similar to the generic `etsf_io_low_write_var` interface, but `var` is of type `etsf_io_low_var_double`. Optional arguments `start`, `count`, `map` are passed through.
    -   **Functionality**: Checks which `dataXD` pointer within `var` is associated and dispatches the call to the appropriate typed and dimensioned subroutine (e.g., `write_var_double_1D`, etc., likely found in `write_routines_auto.f90`). These specific routines handle the actual NetCDF `nf90_put_var` calls.

-   **`write_var_integer_var(ncid, varname, var, lstat, start, count, map, ncvarid, error_data)`**:
    -   **Overview**: Specific implementation for the generic `etsf_io_low_write_var` interface, tailored for `etsf_io_low_var_integer` type. Similar to `write_var_double_var`, it dispatches to dimension-specific routines. The file must be in data mode.
    -   **Arguments**: Similar to the generic `etsf_io_low_write_var` interface, but `var` is of type `etsf_io_low_var_integer`. Optional arguments `start`, `count`, `map` are passed through.
    -   **Functionality**: Checks which `dataXD` pointer within `var` is associated and dispatches the call to the appropriate typed and dimensioned subroutine (e.g., `write_var_integer_1D`, etc., likely found in `write_routines_auto.f90`).

(Specific, dimensioned write routines like `write_att_integer_0D`, `write_var_character_2D`, etc., are numerous and typically auto-generated or follow a very repetitive pattern, often residing in `write_routines_auto.f90`. They are the concrete procedures for the `etsf_io_low_write_att` and `etsf_io_low_write_var` interfaces. Documenting each one individually here would be excessively verbose. They generally take similar arguments to their generic interface, plus the actual data to be written.)

## Important Variables/Constants

This file primarily contains executable code (subroutines). Constants related to ETSF specifications or NetCDF interaction are generally defined in `etsf_io_low_level.f90` or `public_variables.f90`.

-   **`me` (`CHARACTER(LEN=*), PARAMETER`):** Used locally within many subroutines to store the subroutine's name (e.g., `me = "etsf_io_low_open_create"`). This is for populating the backtrace in the `error_data` structure during error handling.
-   **`etsf_io_low_file_format` (`CHARACTER(LEN=*), PARAMETER, PRIVATE` from `etsf_io_low_level`):** Used by `etsf_io_low_open_create` for the "file_format" attribute. Value: "ETSF Nanoquanta".
-   **`etsf_io_low_conventions` (`CHARACTER(LEN=*), PARAMETER, PRIVATE` from `etsf_io_low_level`):** Used by `etsf_io_low_open_create` for the "Conventions" attribute. Value: "http://www.etsf.eu/fileformats/".

## Usage Examples

See `etsf_io_low_level.md` for usage examples of the public interfaces like `etsf_io_low_open_create`, `etsf_io_low_def_var`, `etsf_io_low_write_var`, etc. The routines in this file are mostly internal implementations for those interfaces.

Example of using `etsf_io_low_write_dim` (typically called after opening a file with `etsf_io_low_open_create` or `etsf_io_low_open_modify` which leave the file in define mode):
```fortran
USE etsf_io_low_level
IMPLICIT NONE
INTEGER :: ncid, dim_id
LOGICAL :: lstat
TYPE(etsf_io_low_error) :: err_data

! Assume ncid is valid and file is in define mode.
! For example, after:
! CALL etsf_io_low_open_create(ncid, "new_file.nc", 1.3, lstat, error_data=err_data)
! IF (.NOT. lstat) THEN; CALL etsf_io_low_error_handle(err_data); STOP; END IF

CALL etsf_io_low_write_dim(ncid, "number_of_atoms", 10, lstat, &
                         & ncdimid=dim_id, error_data=err_data)
IF (lstat) THEN
    WRITE(*,*) "Dimension 'number_of_atoms' defined with ID:", dim_id
ELSE
    CALL etsf_io_low_error_handle(err_data)
END IF

! ... further definitions or close file ...
CALL etsf_io_low_close(ncid, lstat, error_data=err_data)
```

## Dependencies and Interactions

### Internal Dependencies
-   **`etsf_io_low_level.f90` (module `etsf_io_low_level`)**: The subroutines in this file are part of the `etsf_io_low_level` module. They utilize types (e.g., `etsf_io_low_error`, `etsf_io_low_var_infos`), constants, and other routines (e.g., `etsf_io_low_read_dim`, `etsf_io_low_check_header`, `etsf_io_low_set_define_mode`) from this module or its included files.
-   **`public_variables.f90`**: Provides constants (e.g., `ERROR_MODE_DEF`, `ERROR_TYPE_VAR`, NetCDF type kinds, `NF90_CLOBBER`, `NF90_WRITE`) used for error handling, NetCDF flags, and data types.
-   **`read_routines.f90`**: Routines like `etsf_io_low_read_dim` and `etsf_io_low_read_var_infos` (from `read_routines.f90`) are called by some write/definition routines to check for pre-existing entities.
-   **`write_routines_auto.f90`**: The generic `write_var_double_var` and `write_var_integer_var` (and similar for attributes) in this file dispatch to more specific, often auto-generated, routines that are expected to be in `write_routines_auto.f90`. These auto-generated routines handle the actual NetCDF calls for specific data types and dimensions.
-   **Wrappers for NetCDF parallel I/O (`wrap_nf90_create`, `wrap_nf90_open`)**: These are mentioned and likely reside in `netcdf3_wrappers.f90` or `netcdf4_wrappers.f90` to abstract parallel NetCDF calls.

### External Library Dependencies
-   **NetCDF Library (`netcdf`)**: All file creation, definition, and writing operations ultimately rely on the NetCDF library. Functions such as `nf90_create`, `nf90_open`, `nf90_put_att`, `nf90_def_dim`, `nf90_def_var`, `nf90_put_var`, `nf90_copy_att` are fundamental.
-   **MPI Library (`mpif.h`)**: Indirectly, if parallel I/O is enabled, the MPI library is a dependency for the parallel NetCDF functions. `etsf_io_low_open_create` and `etsf_io_low_open_modify` can take MPI communicators.

### Global Variables/Modules Accessed
-   The subroutines use constants and types defined in the `etsf_io_low_level` module scope.
-   They modify the `error_data` argument.
-   Global attributes of the NetCDF file are written by `etsf_io_low_open_create` and potentially modified by `etsf_io_low_open_modify`.
```
