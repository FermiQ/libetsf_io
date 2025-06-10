# `etsf_io_low_level.f90` - ETSF I/O Low-Level API

## Overview

This file provides the low-level API for ETSF (Electronic Structure Task Force) file format input/output operations. It serves as a wrapper around NetCDF library calls, offering a safer and more convenient interface tailored for reading and writing files that adhere to ETSF specifications. This module is crucial for direct data manipulation and ensures that operations are performed with automatic dimension checks and optional error handling.

## Key Components

### Modules

-   **`etsf_io_low_level`**: The main module in this file. It encapsulates all the low-level ETSF I/O functionalities, including routines for file access (opening, closing, creating, modifying), data reading/writing (variables, attributes, dimensions), error handling, and utility functions.

### Public Data Types

-   **`etsf_io_low_error`**: A derived type used for structured error reporting. It stores detailed information about errors encountered during I/O operations, such as the routine where the error occurred, the type of operation, and an error message.
    -   `backtrace(100)` (`CHARACTER(LEN=80)`): Array to store the call stack.
    -   `backtraceId` (`INTEGER`): Current depth of the backtrace.
    -   `access_mode_id` (`INTEGER`): Identifier for the access mode when the error occurred.
    -   `access_mode_str` (`CHARACTER(LEN=15)`): String representation of the access mode.
    -   `target_type_id` (`INTEGER`): Identifier for the target type of the operation.
    -   `target_type_str` (`CHARACTER(LEN=22)`): String representation of the target type.
    -   `target_id` (`INTEGER`): Numerical ID of the target (e.g., variable ID, dimension ID).
    -   `target_name` (`CHARACTER(LEN=80)`): Name of the target (e.g., variable name, dimension name).
    -   `error_id` (`INTEGER`): Error code (often from NetCDF).
    -   `error_message` (`CHARACTER(LEN=256)`): Descriptive error message.

-   **`etsf_io_low_var_integer`**: A derived type for handling integer arrays of undefined shape (up to 7 dimensions) without predefining their exact dimensionality. This allows for flexible data handling where array dimensions might not be known until runtime or vary.
    -   `data1D` (`INTEGER, POINTER :: data1D(:)`): Pointer to a 1D integer array.
    -   `data2D` (`INTEGER, POINTER :: data2D(:,:)`): Pointer to a 2D integer array.
    -   ... (up to `data7D`)

-   **`etsf_io_low_var_double`**: Similar to `etsf_io_low_var_integer`, but for double precision floating-point arrays.
    -   `data1D` (`DOUBLE PRECISION, POINTER :: data1D(:)`): Pointer to a 1D double precision array.
    -   `data2D` (`DOUBLE PRECISION, POINTER :: data2D(:,:)`): Pointer to a 2D double precision array.
    -   ... (up to `data7D`)

-   **`etsf_io_low_var_infos`**: A derived type to store metadata about a NetCDF variable, such as its name, ID, data type, shape, and dimension information.
    -   `name` (`CHARACTER(LEN=80)`): Name of the variable.
    -   `ncid` (`INTEGER`): NetCDF ID of the variable.
    -   `nctype` (`INTEGER`): NetCDF data type of the variable.
    -   `ncshape` (`INTEGER`): Number of dimensions of the variable.
    -   `ncdims(7)` (`INTEGER`): Array storing the size of each dimension.
    -   `ncdimnames(:)` (`CHARACTER(LEN=80), POINTER`): Pointer to an array of dimension names.
    -   `ncattnames(:)` (`CHARACTER(LEN=80), POINTER`): Pointer to an array of attribute names.

### Key Public Subroutines & Functions (Grouped by Functionality)

**Error Handling:**
-   **`etsf_io_low_error_set(error_data, mode, type, parent, tgtid, tgtname, errid, errmess)`**: Initializes an `etsf_io_low_error` object with details about an error.
-   **`etsf_io_low_error_update(error, method)`**: Adds a method name to the backtrace of an existing `etsf_io_low_error` object.
-   **`etsf_io_low_error_to_str(str, error_data)`**: Converts an `etsf_io_low_error` object into a formatted string.
-   **`etsf_io_low_error_handle(error_data)`**: Prints the details of an `etsf_io_low_error` object to standard output.

**File Operations:**
-   **`etsf_io_low_open_create(ncid, filename, version, lstat, title, history, error_data, ...)`**: Creates a new ETSF file with the specified NetCDF ID, filename, and version. Optionally sets title and history.
-   **`etsf_io_low_open_read(ncid, filename, lstat, version_min, error_data, ...)`**: Opens an existing ETSF file for reading.
-   **`etsf_io_low_open_modify(ncid, filename, lstat, title, history, version, error_data, ...)`**: Opens an existing ETSF file for modification.
-   **`etsf_io_low_close(ncid, lstat, error_data)`**: Closes an opened NetCDF file.
-   **`etsf_io_low_set_write_mode(ncid, lstat, error_data)`**: Puts the NetCDF file into data mode (ends define mode).
-   **`etsf_io_low_set_define_mode(ncid, lstat, error_data)`**: Puts the NetCDF file into define mode (re-enters define mode).

**Data Reading:**
-   **`etsf_io_low_read_dim(ncid, dimname, dimvalue, lstat, ncdimid, error_data)`**: Reads the ID and length of a dimension.
-   **`etsf_io_low_read_att(ncid, ncvarid/varname, attname, attlen, att, lstat, error_data)`**: Generic interface to read attributes of different types (integer, real, double, character).
-   **`etsf_io_low_read_flag(ncid, flag, ncvarid/varname, attname, lstat, error_data)`**: Reads a character attribute and interprets it as a boolean flag ("yes"/.true.).
-   **`etsf_io_low_read_var(ncid, varname, var, [charlen,] lstat, ncvarid, start, count, map, error_data)`**: Generic interface to read variables of different types and dimensions. Supports partial reads.
-   **`etsf_io_low_read_var_infos(ncid, varname/varid, var_infos, lstat, error_data, ...)`**: Retrieves metadata (type, shape, dimensions, attributes) about a variable.
-   **`etsf_io_low_read_all_var_infos(ncid, var_infos_array, lstat, error_data, ...)`**: Reads metadata for all variables in a file.

**Data Writing & Definition:**
-   **`etsf_io_low_write_dim(ncid, dimname, dimvalue, lstat, ncdimid, error_data)`**: Defines a new dimension or checks an existing one.
-   **`etsf_io_low_def_var(ncid, varname, vartype, [vardims,] lstat, ncvarid, error_data)`**: Defines a new variable (scalar or N-dimensional).
-   **`etsf_io_low_write_att(ncid, ncvarid, attname, att, lstat, error_data)`**: Writes/defines an attribute for a variable or globally.
-   **`etsf_io_low_write_var(ncid, varname, var, [charlen,] lstat, ncvarid, start, count, map, error_data)`**: Generic interface to write data to variables. Supports partial writes.
-   **`etsf_io_low_copy_all_att(ncid_from, ncid_to, ncvarid_from, ncvarid_to, lstat, error_data)`**: Copies all attributes from one variable to another, potentially across files.

**Variable Information & Utilities:**
-   **`etsf_io_low_var_associated(array)`**: Checks if the data pointer within an `etsf_io_low_var_integer` or `etsf_io_low_var_double` type is associated.
-   **`etsf_io_low_var_multiply(array, factor)`**: Multiplies the elements of an `etsf_io_low_var_integer` or `etsf_io_low_var_double` array by a factor.
-   **`etsf_io_low_free_var_infos(var_infos)`**: Deallocates memory associated with an `etsf_io_low_var_infos` structure (specifically `ncdimnames` and `ncattnames`).
-   **`etsf_io_low_free_all_var_infos(var_infos_array)`**: Deallocates an array of `etsf_io_low_var_infos` structures and their internal pointers.
-   **`etsf_io_low_check_parallel_io()`**: Logical function returning `.TRUE.` if the library was compiled with parallel I/O support.
-   **`pad(string)`**: Utility function to format a string to a constant length (256 characters).
-   **`strip(string)`**: Utility subroutine to replace trailing null characters (`CHAR(0)`) with spaces in a string.

**Checking Routines:**
-   **`etsf_io_low_check_att(ncid, ncvarid, attname, atttype, attlen, lstat, error_data)`**: Checks if an attribute exists with the correct type and length.
-   **`etsf_io_low_check_header(ncid, lstat, version_min, error_data)`**: Checks if the ETSF file header conforms to specifications.
-   **`etsf_io_low_check_var(var_ref, var, start, count, map, lstat, error_data)`**: Compares the metadata of two variables to check for compatibility for data transfer.

## Important Variables/Constants

### Private Parameters (Error Handling Configuration)
-   **`nb_access_mode` (`INTEGER, PARAMETER`):** Number of defined access modes for error reporting (Value: 7).
-   **`etsf_io_low_error_mode(nb_access_mode)` (`CHARACTER(LEN=15), DIMENSION, PARAMETER`):** Array of strings describing access modes (e.g., "define", "get", "input/output").
-   **`nb_target_type` (`INTEGER, PARAMETER`):** Number of defined target types for error reporting (Value: 12).
-   **`etsf_io_low_error_type(nb_target_type)` (`CHARACTER(LEN=22), DIMENSION, PARAMETER`):** Array of strings describing target types (e.g., "attribute", "dimension ID", "variable").

### Private Parameters (File Format Information)
-   **`etsf_io_low_file_format` (`CHARACTER(LEN=*), PARAMETER`):** Default string for the "file_format" global attribute (Value: "ETSF Nanoquanta").
-   **`etsf_io_low_conventions` (`CHARACTER(LEN=*), PARAMETER`):** Default string for the "Conventions" global attribute (Value: "http://www.etsf.eu/fileformats/").

### Included Public Variables
- This module includes `public_variables.f90`, which likely defines various public constants such as:
    - `ETSF_IO_LOW_INTEGER`, `ETSF_IO_LOW_DOUBLE`, `ETSF_IO_LOW_CHARACTER` (for variable types)
    - `ETSF_IO_LOW_GLOBAL_ATT` (for global attributes)
    - `ERROR_MODE_...`, `ERROR_TYPE_...` (constants for error reporting)
    - `etsf_io_low_error_len` (length for error strings)
    (Refer to `public_variables.md` for a detailed list)

## Usage Examples

Reading a scalar integer variable:
```fortran
USE etsf_io_low_level
IMPLICIT NONE
INTEGER :: ncid, space_group_val
LOGICAL :: lstat
TYPE(etsf_io_low_error) :: err

! Assume ncid is an opened file handle
CALL etsf_io_low_open_read(ncid, "my_file.nc", lstat, error_data=err)
IF (.NOT. lstat) THEN
    CALL etsf_io_low_error_handle(err)
    STOP
END IF

CALL etsf_io_low_read_var(ncid, "space_group", space_group_val, lstat, error_data=err)
IF (lstat) THEN
    WRITE(*,*) "Space group:", space_group_val
ELSE
    CALL etsf_io_low_error_handle(err)
END IF

CALL etsf_io_low_close(ncid, lstat, error_data=err)
```

Defining a new 1D double precision variable:
```fortran
USE etsf_io_low_level
IMPLICIT NONE
INTEGER :: ncid, dimid, varid
LOGICAL :: lstat
TYPE(etsf_io_low_error) :: err
CHARACTER(LEN=80) :: dim_name
DOUBLE PRECISION, ALLOCATABLE :: my_data(:)

! Assume ncid is an opened file handle in define mode (e.g., after etsf_io_low_open_create)
dim_name = "number_of_points"
CALL etsf_io_low_write_dim(ncid, dim_name, 100, lstat, ncdimid=dimid, error_data=err)
IF (.NOT. lstat) THEN; CALL etsf_io_low_error_handle(err); STOP; END IF

CALL etsf_io_low_def_var(ncid, "my_variable", ETSF_IO_LOW_DOUBLE, &
                         (/ dim_name /), lstat, ncvarid=varid, error_data=err)
IF (.NOT. lstat) THEN; CALL etsf_io_low_error_handle(err); STOP; END IF

! Switch to data mode to write
CALL etsf_io_low_set_write_mode(ncid, lstat, error_data=err)
IF (.NOT. lstat) THEN; CALL etsf_io_low_error_handle(err); STOP; END IF

ALLOCATE(my_data(100))
my_data = [(DBLE(i), i=1,100)]
CALL etsf_io_low_write_var(ncid, "my_variable", my_data, lstat, error_data=err)
IF (.NOT. lstat) THEN; CALL etsf_io_low_error_handle(err); STOP; END IF

DEALLOCATE(my_data)
CALL etsf_io_low_close(ncid, lstat, error_data=err)
```

## Dependencies and Interactions

### Internal Dependencies
-   **`public_variables.f90`**: This file is included and provides definitions for various public constants and parameters used throughout the `etsf_io_low_level` module (e.g., type kinds, error codes, global attribute identifiers).
-   **`read_routines.f90`**: Included in this file. Contains the implementations for the generic `etsf_io_low_read_var` interface and other specific read operations.
-   **`read_routines_auto.f90`**: Included in this file. Likely contains auto-generated specific read routines for different data types and dimensions.
-   **`write_routines.f90`**: Included in this file. Contains the implementations for the generic `etsf_io_low_write_var`, `etsf_io_low_def_var`, `etsf_io_low_write_att` interfaces and other specific write/definition operations.
-   **`write_routines_auto.f90`**: Included in this file. Likely contains auto-generated specific write/definition routines.

### External Library Dependencies
-   **NetCDF Library (`netcdf`)**: This module heavily relies on the NetCDF library for all underlying file I/O operations. It uses NetCDF functions like `nf90_open`, `nf90_close`, `nf90_inq_varid`, `nf90_get_var`, `nf90_put_var`, `nf90_def_dim`, `nf90_def_var`, `nf90_put_att`, etc. The module wraps these calls to provide a more specialized and safer API for ETSF files.

### Global Variables/Modules Accessed
-   The module itself defines and uses several private parameters (constants) for its internal logic, primarily for error reporting and default file attributes.
-   It accesses public constants defined in `public_variables.f90`.
```
