# `read_routines.f90` - Low-Level ETSF File Read Routines

## Overview

This file contains the implementation of various low-level subroutines dedicated to reading data and metadata from ETSF (Electronic Structure Task Force) files. These routines are typically part of the `etsf_io_low_level` module and provide the core logic for data retrieval operations, complementing the main API definitions found in `etsf_io_low_level.f90`. They handle interactions with the NetCDF library for reading dimensions, attributes, variable information, and variable data.

## Key Components

### Subroutines

-   **`etsf_io_low_read_dim(ncid, dimname, dimvalue, lstat, ncdimid, error_data)`**:
    -   **Overview**: Reads the length and, optionally, the NetCDF ID of a specified dimension in an opened ETSF file.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file identifier.
        -   `dimname` (`CHARACTER(LEN=*), INTENT(IN)`): Name of the dimension to read.
        -   `dimvalue` (`INTEGER, INTENT(OUT)`): On successful return, the length of the dimension.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation (`.TRUE.` if successful).
        -   `ncdimid` (`INTEGER, INTENT(OUT), OPTIONAL`): On successful return, the NetCDF ID of the dimension.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Structure to store error details if the operation fails.
    -   **Functionality**: Calls `nf90_inq_dimid` to get the dimension ID and `nf90_inquire_dimension` to get its length. Handles errors and populates `error_data`.

-   **`read_var_infos_name(ncid, varname, var_infos, lstat, error_data, dim_name, att_name)`**:
    -   **Overview**: Retrieves information (metadata) about a variable specified by its name. This is part of the `etsf_io_low_read_var_infos` interface.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file identifier.
        -   `varname` (`CHARACTER(LEN=*), INTENT(IN)`): Name of the variable.
        -   `var_infos` (`TYPE(etsf_io_low_var_infos), INTENT(OUT)`): Structure to store the retrieved variable metadata.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error reporting structure.
        -   `dim_name` (`LOGICAL, INTENT(IN), OPTIONAL`): If `.TRUE.`, retrieves dimension names.
        -   `att_name` (`LOGICAL, INTENT(IN), OPTIONAL`): If `.TRUE.`, retrieves attribute names.
    -   **Functionality**: Gets the variable ID using `nf90_inq_varid` and then calls `read_var_infos` to populate the `var_infos` structure.

-   **`read_var_infos_id(ncid, varid, var_infos, lstat, error_data, dim_name, att_name)`**:
    -   **Overview**: Retrieves information (metadata) about a variable specified by its NetCDF ID. This is part of the `etsf_io_low_read_var_infos` interface.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file identifier.
        -   `varid` (`INTEGER, INTENT(IN)`): NetCDF ID of the variable.
        -   `var_infos` (`TYPE(etsf_io_low_var_infos), INTENT(OUT)`): Structure to store the retrieved variable metadata.
        -   (Other arguments similar to `read_var_infos_name`)
    -   **Functionality**: Gets the variable name using `nf90_inquire_variable` and then calls `read_var_infos`.

-   **`read_var_infos(ncid, var_infos, with_dim_name, with_att_name, lstat, error_data)`**:
    -   **Overview**: Core routine (private to the module, called by `read_var_infos_name` and `read_var_infos_id`) to populate the `etsf_io_low_var_infos` structure with details about a variable (type, shape, dimensions, attributes). `var_infos%ncid` (and `var_infos%name` if called from `read_var_infos_name`) must be set before calling.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file ID.
        -   `var_infos` (`TYPE(etsf_io_low_var_infos), INTENT(INOUT)`): Structure to be filled.
        -   `with_dim_name` (`LOGICAL, INTENT(IN)`): If `.TRUE.`, retrieves and stores dimension names.
        -   `with_att_name` (`LOGICAL, INTENT(IN)`): If `.TRUE.`, retrieves and stores attribute names.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Operation status.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: Uses `nf90_inquire_variable` to get type, number of dimensions, and number of attributes. If `with_dim_name` is true, it allocates and fills `var_infos%ncdimnames`. If `with_att_name` is true, it allocates and fills `var_infos%ncattnames`.

-   **`etsf_io_low_read_all_var_infos(ncid, var_infos_array, lstat, error_data, with_dim_name, with_att_name)`**:
    -   **Overview**: Reads metadata for all variables in a NetCDF file and stores them in an array of `etsf_io_low_var_infos` structures.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file identifier.
        -   `var_infos_array` (`TYPE(etsf_io_low_var_infos), POINTER :: var_infos_array(:)`): Pointer to an array that will be allocated and filled with variable information. Must be `NULL()` on entry.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation.
        -   (Other arguments similar to `read_var_infos_name`)
    -   **Functionality**: Inquires the total number of variables, allocates `var_infos_array`, and then iteratively calls `read_var_infos_id` for each variable.

-   **`read_flag_id(ncid, flag, ncvarid, attname, lstat, error_data)`**:
    -   **Overview**: Reads a character attribute (identified by variable ID and attribute name) and interprets it as a boolean flag. Sets `flag` to `.TRUE.` if the attribute value is "Yes", "YES", or "yes". Part of the `etsf_io_low_read_flag` interface.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file ID.
        -   `flag` (`LOGICAL, INTENT(OUT)`): Resulting boolean flag.
        -   `ncvarid` (`INTEGER, INTENT(IN)`): NetCDF ID of the variable owning the attribute (or `etsf_io_low_global_att`).
        -   `attname` (`CHARACTER(LEN=*), INTENT(IN)`): Name of the attribute.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Operation status.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: Calls `etsf_io_low_read_att` to get the attribute's string value and then performs the comparison.

-   **`read_flag(ncid, flag, varname, attname, lstat, error_data)`**:
    -   **Overview**: Similar to `read_flag_id`, but identifies the variable by its name instead of ID. Part of the `etsf_io_low_read_flag` interface.
    -   **Arguments**: (Similar to `read_flag_id`, with `varname` instead of `ncvarid`)
    -   **Functionality**: Calls `etsf_io_low_read_att` (version that takes `varname`) and then performs the comparison.

-   **`etsf_io_low_make_access(start, count, map, var_infos, lstat, opt_start, opt_count, opt_map, error_data)`**:
    -   **Overview**: A utility subroutine to prepare `start`, `count`, and `map` arrays required for NetCDF's `nf90_get_var` (and `nf90_put_var`) routines, based on optional user-provided partial access arguments and variable metadata.
    -   **Arguments**:
        -   `start(16)` (`INTEGER, INTENT(OUT)`): Calculated start indices for NetCDF access.
        -   `count(16)` (`INTEGER, INTENT(OUT)`): Calculated count for each dimension for NetCDF access.
        -   `map(16)` (`INTEGER, INTENT(OUT)`): Calculated map (stride) for NetCDF access.
        -   `var_infos` (`TYPE(etsf_io_low_var_infos), INTENT(IN)`): Metadata of the variable being accessed.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Operation status.
        -   `opt_start(:)` (`INTEGER, INTENT(IN), OPTIONAL`): User-provided start indices.
        -   `opt_count(:)` (`INTEGER, INTENT(IN), OPTIONAL`): User-provided counts (0 means read full dimension).
        -   `opt_map(:)` (`INTEGER, INTENT(IN), OPTIONAL`): User-provided map/stride or permutation.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: Validates `opt_start`, `opt_count`, `opt_map` against `var_infos`. If arguments are omitted, defaults are used (read entire variable). Handles special `opt_count` value (0) and permutation logic for `opt_map`.

-   **`etsf_io_low_check_var(var_ref, var, start, count, map, lstat, error_data)`**:
    -   **Overview**: Compares metadata of two variables (`var_ref` from file, `var` representing the memory buffer) along with access parameters (`start`, `count`, `map`) to ensure they are compatible for a read/write operation.
    -   **Arguments**:
        -   `var_ref` (`TYPE(etsf_io_low_var_infos), INTENT(IN)`): Metadata of the source/destination variable in the file.
        -   `var` (`TYPE(etsf_io_low_var_infos), INTENT(IN)`): Metadata of the source/destination variable in memory.
        -   `start(:)` (`INTEGER, INTENT(IN)`): Start indices for access.
        -   `count(:)` (`INTEGER, INTENT(IN)`): Counts for access.
        -   `map(:)` (`INTEGER, INTENT(IN)`): Map/stride for access.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): `.TRUE.` if compatible.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: Checks for type compatibility (numeric vs. character). Validates `start`, `count`, and `map` values against `var_ref` dimensions. Crucially, ensures that the total number of elements to be accessed/transferred matches between the file selection and the memory buffer.

-   **`etsf_io_low_check_att(ncid, ncvarid, attname, atttype, attlen, lstat, error_data)`**:
    -   **Overview**: Checks if a specified attribute exists, has the correct data type, and has the correct length.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file ID.
        -   `ncvarid` (`INTEGER, INTENT(IN)`): Variable ID owning the attribute (or `NF90_GLOBAL`).
        -   `attname` (`CHARACTER(LEN=*), INTENT(IN)`): Name of the attribute.
        -   `atttype` (`INTEGER, INTENT(IN)`): Expected NetCDF data type of the attribute.
        -   `attlen` (`INTEGER, INTENT(IN)`): Expected length of the attribute (1 for scalar).
        -   `lstat` (`LOGICAL, INTENT(OUT)`): `.TRUE.` if the attribute matches expectations.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: Uses `nf90_inquire_attribute` to get the attribute's actual type and length, then compares them with the expected values.

-   **`etsf_io_low_check_header(ncid, lstat, version_min, error_data)`**:
    -   **Overview**: Specifically checks if the global attributes of an ETSF file conform to the ETSF specifications (checks `file_format`, `file_format_version`, `Conventions`).
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF file ID.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): `.TRUE.` if the header is valid.
        -   `version_min` (`REAL, INTENT(IN), OPTIONAL`): Minimum acceptable `file_format_version`. Defaults to 1.3 if not provided.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
    -   **Functionality**: Reads "file_format", "file_format_version", and "Conventions" global attributes using `etsf_io_low_read_att` and `etsf_io_low_check_att`, comparing their values against expected ETSF standards.

-   **`etsf_io_low_open_read(ncid, filename, lstat, version_min, error_data, with_etsf_header)`**:
    -   **Overview**: Opens an ETSF file in read-only mode and optionally checks its header for ETSF conformance.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(OUT)`): On successful return, the NetCDF file identifier.
        -   `filename` (`CHARACTER(LEN=*), INTENT(IN)`): Path to the ETSF file.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Operation status.
        -   `version_min` (`REAL, INTENT(IN), OPTIONAL`): Minimum file format version (passed to `etsf_io_low_check_header`).
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT), OPTIONAL`): Error structure.
        -   `with_etsf_header` (`LOGICAL, INTENT(IN), OPTIONAL`): If `.TRUE.` (default), calls `etsf_io_low_check_header`.
    -   **Functionality**: Calls `nf90_open` with `NF90_NOWRITE`. If `with_etsf_header` is true, it then calls `etsf_io_low_check_header`.

-   **`read_var_double_var(ncid, varname, var, lstat, start, count, map, ncvarid, error_data)`**:
    -   **Overview**: Specific implementation for the generic `etsf_io_low_read_var` interface, tailored for `etsf_io_low_var_double` type. It determines the dimensionality of the associated pointer (`data1D` to `data7D`) within the `var` argument and calls the corresponding dimension-specific read routine (e.g., `read_var_double_1D`, `read_var_double_3D`) which are expected to be in `read_routines_auto.f90`.
    -   **Arguments**: Similar to the generic `etsf_io_low_read_var` interface, but `var` is of type `etsf_io_low_var_double`. Optional arguments `start`, `count`, `map` are passed through.
    -   **Functionality**: Checks which `dataXD` pointer within `var` is associated and dispatches the call to the appropriate typed and dimensioned subroutine (e.g., `read_var_double_1D`, etc., likely found in `read_routines_auto.f90`).

-   **`read_var_integer_var(ncid, varname, var, lstat, start, count, map, ncvarid, error_data)`**:
    -   **Overview**: Specific implementation for the generic `etsf_io_low_read_var` interface, tailored for `etsf_io_low_var_integer` type. Similar to `read_var_double_var`, it dispatches to dimension-specific routines.
    -   **Arguments**: Similar to the generic `etsf_io_low_read_var` interface, but `var` is of type `etsf_io_low_var_integer`. Optional arguments `start`, `count`, `map` are passed through.
    -   **Functionality**: Checks which `dataXD` pointer within `var` is associated and dispatches the call to the appropriate typed and dimensioned subroutine (e.g., `read_var_integer_1D`, etc., likely found in `read_routines_auto.f90`).

(Specific, dimensioned read routines like `read_var_double_1D`, `read_var_integer_0D`, `read_att_id_character_1D`, etc., are numerous and typically auto-generated or follow a very repetitive pattern, often residing in `read_routines_auto.f90`. They are the concrete procedures for the `etsf_io_low_read_var` and `etsf_io_low_read_att` interfaces. Documenting each one individually here would be excessively verbose. They generally take similar arguments to their generic interface, plus the actual data array/scalar.)

## Important Variables/Constants

This file primarily contains executable code (subroutines). Constants and parameters are generally defined in `etsf_io_low_level.f90` or `public_variables.f90`.

-   **`me` (`CHARACTER(LEN=*), PARAMETER`):** Used within many subroutines as a local constant to store the subroutine's own name (e.g., `me = "etsf_io_low_read_dim"`). This is used for populating the backtrace in the `error_data` structure.

## Usage Examples

See `etsf_io_low_level.md` for usage examples of the public interfaces like `etsf_io_low_read_dim`, `etsf_io_low_read_var`, etc. The routines in this file are mostly internal implementations for those interfaces.

An example of directly using `etsf_io_low_check_header` (though it's usually called by `etsf_io_low_open_read`):
```fortran
USE etsf_io_low_level ! Provides etsf_io_low_check_header and error types
IMPLICIT NONE
INTEGER :: ncid
LOGICAL :: lstat_open, lstat_check
TYPE(etsf_io_low_error) :: err

! Assume my_file.nc is an ETSF file
CALL nf90_open("my_file.nc", NF90_NOWRITE, ncid, lstat_open)
IF (.NOT. lstat_open) THEN
    WRITE(*,*) "Failed to open file."
    STOP
END IF

CALL etsf_io_low_check_header(ncid, lstat_check, error_data=err)
IF (lstat_check) THEN
    WRITE(*,*) "File header is valid ETSF."
ELSE
    WRITE(*,*) "File header is NOT valid ETSF."
    CALL etsf_io_low_error_handle(err)
END IF

CALL nf90_close(ncid, lstat_open)
```

## Dependencies and Interactions

### Internal Dependencies
-   **`etsf_io_low_level.f90` (module `etsf_io_low_level`)**: This file's subroutines are part of the `etsf_io_low_level` module. They use types (like `etsf_io_low_error`, `etsf_io_low_var_infos`) and constants defined within that module or included files like `public_variables.f90`.
-   **`public_variables.f90`**: Provides constants (e.g., `ERROR_MODE_INQ`, `ERROR_TYPE_DIM`, `NF90_NOERR`, NetCDF type kinds) used extensively for error handling and NetCDF calls.
-   **`read_routines_auto.f90`**: The generic `read_var_double_var` and `read_var_integer_var` (and similar for attributes) in this file dispatch to more specific, often auto-generated, routines that are expected to be in `read_routines_auto.f90`. These auto-generated routines handle the actual NetCDF calls for specific data types and dimensions.

### External Library Dependencies
-   **NetCDF Library (`netcdf`)**: All routines that interact with the file system do so through the NetCDF library. Functions like `nf90_inq_dimid`, `nf90_inquire_dimension`, `nf90_inq_varid`, `nf90_get_var`, `nf90_inquire_attribute`, `nf90_open`, `nf90_close` are fundamental to the operations in this file.

### Global Variables/Modules Accessed
-   The subroutines use constants and types defined in the `etsf_io_low_level` module scope (which includes `public_variables.f90`).
-   They modify the `error_data` argument, which is a shared error structure.
```
