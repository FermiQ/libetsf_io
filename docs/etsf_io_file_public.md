# `etsf_io_file_public.f90` - High-Level ETSF File Operations API

## Overview

This file defines public, high-level subroutines for performing operations on ETSF (Electronic Structure Task Force) files. These routines provide a more abstracted interface compared to the `etsf_io_low_level` API, designed for common tasks such as merging split files and validating file content against ETSF specifications. This file is part of the `etsf_io_file` module.

## Key Components

### Module: `etsf_io_file` (Public Interface Part)
This file contributes the public interface to the `etsf_io_file` module. The private implementations supporting these public routines are expected to be in `etsf_io_file_private.f90`.

### Public Subroutines

-   **`etsf_io_file_merge(dest_file, source_files, lstat, error_data)`**:
    -   **Overview**: Merges several ETSF files, which are assumed to be parts of a split dataset, into a single destination ETSF file. The routine correctly handles partial merges: if the provided `source_files` do not constitute a complete, non-split file, the `dest_file` will be a new split file representing the maximum possible merge of the given parts.
    -   **Arguments**:
        -   `dest_file` (`CHARACTER(LEN=*), INTENT(IN)`): Path to the output (merged) ETSF file. It must not already exist for a new merge operation.
        -   `source_files(:)` (`CHARACTER(LEN=256), INTENT(IN)`): An array of strings, where each string is a path to an input source ETSF file to be merged.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation (`.TRUE.` if successful).
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT)`): Structure to store error details if the operation fails.
    -   **Functionality**:
        1.  **Initialization**: Checks if `source_files` array is non-empty. Allocates an array `infos_file` to store metadata for each source file.
        2.  **Read Source Metadata**: Iterates through each `source_files`. For each file:
            -   Calls `etsf_io_data_contents` (from `etsf_io` module) to read its dimensions (`dims`), split definitions (`split`), main ETSF group flags (`grp`), other ETSF variable group flags (`etsf_variables`), and a list of all variables (`var_list`).
            -   Accumulates the main ETSF group flags from all source files.
        3.  **Merge Dimensions and Split Info**:
            -   Initializes `output_dims` with dimensions from the first source file.
            -   Merges dimensions from subsequent source files into `output_dims` using `etsf_io_dims_merge`.
            -   Allocates `output_split` based on the merged `output_dims` using `etsf_io_split_allocate`.
            -   Merges the split definitions from each source file's `split` into `output_split` using `etsf_io_split_merge`.
        4.  **Initialize Destination File (ETSF Part)**:
            -   Calls `etsf_io_data_init` to create `dest_file` and define its structure based on `etsf_variables` (excluding main group initially), `output_dims`, and `output_split`. Sets a default title and history.
        5.  **Handle Non-ETSF Variables and Dimensions (Definition)**:
            -   Re-opens `dest_file` for modification using `etsf_io_low_open_modify`.
            -   Calls `non_etsf_init` (a private routine) to define any non-ETSF dimensions and variables found in the `infos_file` array into `dest_file`.
        6.  **Define Main ETSF Group**: If any main ETSF groups were present in source files, defines them in `dest_file` using `etsf_io_main_def` with the `output_split` information.
        7.  Closes `dest_file` after definitions.
        8.  **Copy ETSF Data**: Iterates through each `source_files` and calls `etsf_io_data_copy` to copy data for ETSF variables from the source file to `dest_file`, respecting their respective split definitions.
        9.  **Copy Non-ETSF Data**:
            -   Re-opens `dest_file` for modification and switches to data mode.
            -   Calls `non_etsf_copy` (a private routine) to copy data for non-ETSF variables from `source_files` to `dest_file`.
        10. Closes `dest_file`.
        11. **Cleanup**: Deallocates `infos_file` and frees memory associated with `output_split`.

-   **`etsf_io_file_check(file_name, file_flags, lstat, error_data)`**:
    -   **Overview**: Checks if a given ETSF file is valid according to one or more specified ETSF data group flags (e.g., density, wavefunctions). This involves verifying the presence and correct definition of required variables, dimensions, and attributes for each of the given specifications.
    -   **Arguments**:
        -   `file_name` (`CHARACTER(LEN=*), INTENT(IN)`): Path to the ETSF file to be checked.
        -   `file_flags` (`INTEGER, INTENT(IN)`): A bitmask representing the ETSF data specifications/groups to check against. This is typically a sum of flags like `ETSF_DENSITY`, `ETSF_WAVEFUNCTIONS` (which are constants defined in the `etsf_io` module).
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the check (`.TRUE.` if the file is valid for *all* specified flags).
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT)`): Structure to store error details if the check fails. If multiple flags are checked and one fails, `error_data` will contain the error information for the first failing specification encountered.
    -   **Functionality**:
        1.  Calls `etsf_io_file_contents(read_flags, errors, file_name, lstat, error_data)` to determine which specifications the `file_name` actually conforms to. `read_flags` will be a bitmask of successfully validated specifications, and `errors` will be an array of `etsf_io_low_error` detailing issues for any specification that failed the content check.
        2.  If `etsf_io_file_contents` itself fails (e.g., cannot open file), `lstat` is set to `.FALSE.`, `error_data` is populated, and the routine returns.
        3.  Iterates from `i = 1` to `etsf_nspecs_data` (total number of known specifications).
        4.  For each `i`, it checks if the `i`-th bit is set in the input `file_flags` (meaning the user wants to check this specification) AND if the `i`-th bit is NOT set in `read_flags` (meaning `etsf_io_file_contents` found this specification to be invalid or missing).
        5.  If both conditions are true for any `i`, the overall check fails (`lstat = .FALSE.`). The specific `errors(i)` structure (from `etsf_io_file_contents`) corresponding to the failing specification is copied to `error_data`, and the routine returns.
        6.  If the loop completes without finding any requested flag that was missing in `read_flags`, the check passes (`lstat = .TRUE.`).

## Important Variables/Constants

This file primarily defines subroutines. It uses types and constants from other modules:
-   `etsf_io_low_level`: For `etsf_io_low_error` type, error mode constants (`ERROR_MODE_SPEC`, etc.), error type constants (`ERROR_TYPE_ARG`, etc.), and low-level file operations (`etsf_io_low_open_modify`, `etsf_io_low_close`, `etsf_io_low_set_write_mode`).
-   `etsf_io`: For `etsf_dims`, `etsf_split`, `etsf_groups_flags` types, various specification flags and constants (e.g., `etsf_main_none`, `etsf_nspecs_data`), and mid-level routines like `etsf_io_data_contents`, `etsf_io_dims_merge`, `etsf_io_split_allocate`, `etsf_io_split_merge`, `etsf_io_data_init`, `etsf_io_main_def`, `etsf_io_data_copy`, `etsf_io_split_free`.
-   `etsf_io_file_private` (implicitly, as part of the same module `etsf_io_file`): For internal helper types like `file_infos_type` and subroutines like `file_infos_free`, `non_etsf_init`, `non_etsf_copy`. The `etsf_io_file_contents` routine, though not public itself, is used by `etsf_io_file_check`.

Local to `etsf_io_file_merge`:
-   `etsf_main` (`INTEGER`): Stores combined main group flags from all source files.
-   `infos_file(:)` (`TYPE(file_infos_type), ALLOCATABLE`): Array to store metadata (`dims`, `split`, `var_list`) from each source file.
-   `output_split` (`TYPE(etsf_split)`): Merged split definition for the output file.
-   `output_dims` (`TYPE(etsf_dims)`): Merged dimension definitions for the output file.
-   `etsf_variables` (`TYPE(etsf_groups_flags)`): Flags for ETSF variable groups present in source files.

Local to `etsf_io_file_check`:
-   `read_flags` (`INTEGER`): Bitmask of specifications found to be valid in the file by `etsf_io_file_contents`.
-   `errors(:)` (`TYPE(etsf_io_low_error), DIMENSION(etsf_nspecs_data)`): Array storing detailed error information for each possible specification, as returned by `etsf_io_file_contents`.

## Usage Examples

**Merging files:**
```fortran
USE etsf_io_file
USE etsf_io_low_level, ONLY: etsf_io_low_error, etsf_io_low_error_handle
IMPLICIT NONE

CHARACTER(LEN=256) :: out_file = "merged_data.nc"
CHARACTER(LEN=256), DIMENSION(2) :: in_files
LOGICAL :: status
TYPE(etsf_io_low_error) :: err

in_files(1) = "data_part1.nc"
in_files(2) = "data_part2.nc"

CALL etsf_io_file_merge(out_file, in_files, status, err)

IF (status) THEN
    WRITE(*,*) "Files merged successfully into ", TRIM(out_file)
ELSE
    WRITE(*,*) "Error during merge:"
    CALL etsf_io_low_error_handle(err)
END IF
```

**Checking a file for density and geometry specifications:**
```fortran
USE etsf_io_file
USE etsf_io, ONLY: ETSF_DENSITY, ETSF_GEOMETRY ! Assuming these flags are defined in etsf_io
USE etsf_io_low_level, ONLY: etsf_io_low_error, etsf_io_low_error_handle
IMPLICIT NONE

CHARACTER(LEN=256) :: file_to_check = "my_simulation.nc"
INTEGER :: flags_to_check
LOGICAL :: is_valid
TYPE(etsf_io_low_error) :: err

flags_to_check = ETSF_DENSITY + ETSF_GEOMETRY ! Combine flags by adding or bitwise OR

CALL etsf_io_file_check(file_to_check, flags_to_check, is_valid, err)

IF (is_valid) THEN
    WRITE(*,*) TRIM(file_to_check), " is valid for the specified flags."
ELSE
    WRITE(*,*) TRIM(file_to_check), " is NOT valid for one or more specified flags:"
    CALL etsf_io_low_error_handle(err)
END IF
```

## Dependencies and Interactions

### Internal Module Dependencies
-   **`etsf_io_low_level` module**: Used for the `etsf_io_low_error` type, error handling constants, and various low-level file I/O routines.
-   **`etsf_io` module**: Provides fundamental ETSF data structures (like `etsf_dims`, `etsf_split`, `etsf_groups_flags`), specification constants/flags (e.g., `etsf_main_none`, `etsf_geometry_all`, `etsf_nspecs_data`), and mid-level routines essential for merging and content analysis (e.g., `etsf_io_data_contents`, `etsf_io_dims_merge`, `etsf_io_data_init`, `etsf_io_data_copy`).
-   **`etsf_io_file_private` (part of the same `etsf_io_file` module)**: Relies on private subroutines and types defined elsewhere in the `etsf_io_file` module (presumably in `etsf_io_file_private.f90`). Key private components used include:
    -   `file_infos_type` (derived type for storing info about each source file for merging).
    -   `file_infos_free` (for deallocating `file_infos_type` arrays).
    -   `non_etsf_init` (for defining non-ETSF variables/dimensions in the target file).
    -   `non_etsf_copy` (for copying data of non-ETSF variables).
    -   The `etsf_io_file_contents` routine (likely private) is a crucial dependency for `etsf_io_file_check`.

### External Library Dependencies
-   Indirectly depends on a NetCDF library, as the lower-level ETSF I/O routines it calls make extensive use of NetCDF for all file manipulations.

### Global Variables/Modules Accessed
-   Uses parameters, types, and routines from `etsf_io_low_level` and `etsf_io`.
```
