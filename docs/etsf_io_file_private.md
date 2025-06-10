# `etsf_io_file_private.f90` - Private Helper Routines for ETSF File Operations

## Overview

This file contains private subroutines that support the public API defined in `etsf_io_file_public.f90`. These routines are part of the `etsf_io_file` module but are not intended for direct use outside of this module. They handle internal logic for tasks such as managing information about multiple source files during a merge, initializing and copying non-ETSF standard variables, and testing variable definitions.

## Key Components

This file contributes private components to the `etsf_io_file` module.

### Private Derived Types

*(No explicit private derived types like `file_infos_type` are defined *within this specific file*. `file_infos_type` is used by routines here, so it's assumed to be defined elsewhere within the `etsf_io_file` module, likely in a shared private definitions part or `etsf_io_file_public.f90` if it's accessible module-wide).*

### Private Subroutines

-   **`test_var(ncid, var_infos_ref, lstat, error_data)`**:
    -   **Overview**: Tests if a variable in an opened NetCDF file (`ncid`) matches a reference definition (`var_infos_ref`). It checks for existence, correct data type, correct shape (rank), and matching dimension names.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF ID of the file to check.
        -   `var_infos_ref` (`TYPE(etsf_io_low_var_infos), INTENT(IN)`): A structure holding the reference metadata (name, type, shape, dimension names) for the variable.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the test (`.TRUE.` if the variable exists and matches the reference).
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT)`): Structure to store error details if the test fails (e.g., variable not found, type mismatch, shape mismatch, dimension name mismatch).
    -   **Functionality**:
        1.  Calls `etsf_io_low_read_var_infos` to get the actual metadata of the variable named `var_infos_ref%name` from the file.
        2.  Compares `var_infos_ref%nctype` with the actual type.
        3.  Compares `var_infos_ref%ncshape` with the actual shape.
        4.  If shapes match and are > 0, iterates through dimensions to compare `var_infos_ref%ncdimnames(i)` with the actual dimension names.
        5.  Sets `lstat` and `error_data` based on comparison results. Frees metadata read from the file.

-   **`file_infos_free(file_infos, n_size)`**:
    -   **Overview**: Deallocates memory associated with an array of `file_infos_type` structures (this type is assumed to be defined elsewhere in the `etsf_io_file` module). Specifically, it calls `etsf_io_split_free` for the `split` component and `etsf_io_vars_free` for the `var_list` component of each element.
    -   **Arguments**:
        -   `file_infos(:)` (`TYPE(file_infos_type), INTENT(INOUT)`): The array of `file_infos_type` structures to be cleaned up.
        -   `n_size` (`INTEGER, INTENT(IN)`): The number of elements in `file_infos` to process.
    -   **Note**: `file_infos_type` would typically contain components like `path`, `dims`, `split`, and `var_list`.

-   **`non_etsf_init(ncid, infos_file, lstat, error_data)`**:
    -   **Overview**: Initializes (defines) non-ETSF standard dimensions and variables in the target file (`ncid`) during a merge operation. It iterates through the variable lists (`var_list`) of the first source file in `infos_file` and defines any dimensions or variables that are not part of the standard ETSF specification and are not marked as split variables. It then checks if these definitions are consistent across other source files.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF ID of the target/output file (must be in define mode).
        -   `infos_file(:)` (`TYPE(file_infos_type), INTENT(IN)`): Array of information about source files.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT)`): Error reporting structure.
    -   **Functionality**:
        1.  Focuses on `infos_file(1)` for definitions. Iterates through `infos_file(1)%var_list%parent(i_var)`.
        2.  If a variable is not an ETSF group variable (`etsf_grp_none`) and not a split variable:
            -   For each dimension of this variable, calls `etsf_io_low_write_dim` to define it in `ncid`.
        3.  Again, iterates through `infos_file(1)%var_list` for non-ETSF, non-split variables:
            -   Defines the variable in `ncid` using `etsf_io_low_def_var` (handles 0D and nD cases).
        4.  Then, iterates through the remaining `infos_file(2:)` to check for consistency:
            -   For each non-ETSF, non-split variable, it reads its dimension definitions from `ncid` and verifies they match the dimensions in the current `infos_file(i_file)`.
            -   Reads the variable definition from `ncid` and verifies its type and shape match those in `infos_file(1)`.

-   **`non_etsf_copy(ncid, infos_file, lstat, error_data)`**:
    -   **Overview**: Copies data for non-ETSF standard variables from the first source file (`infos_file(1)`) to the target file (`ncid`) during a merge operation. This is done after these variables have been defined by `non_etsf_init`.
    -   **Arguments**:
        -   `ncid` (`INTEGER, INTENT(IN)`): NetCDF ID of the target/output file.
        -   `infos_file(:)` (`TYPE(file_infos_type), INTENT(IN)`): Array of information about source files. Data is copied from `infos_file(1)`.
        -   `lstat` (`LOGICAL, INTENT(OUT)`): Status of the operation.
        -   `error_data` (`TYPE(etsf_io_low_error), INTENT(OUT)`): Error reporting structure.
    -   **Functionality**:
        1.  Opens the first source file (`infos_file(1)%path`) using `etsf_io_low_open_read`.
        2.  Puts the target file `ncid` into data mode using `etsf_io_low_set_write_mode`.
        3.  Iterates through each variable in `infos_file(1)%var_list`.
        4.  If the variable is not an ETSF group variable and not a split variable:
            -   Determines the variable type (integer, real (treated as double), double, character) and size.
            -   Allocates a temporary array of the appropriate type and size.
            -   Reads data from the source variable into the temporary array using `etsf_io_low_read_var`.
            -   Writes data from the temporary array to the corresponding variable in `ncid` using `etsf_io_low_write_var`, getting the `varids_to` for attribute copying.
            -   Deallocates the temporary array.
        5.  Switches `ncid` back to define mode.
        6.  For each non-ETSF, non-split variable, copies all attributes from the source variable (in `ncid_from`) to the newly written variable in `ncid` using `etsf_io_low_copy_all_att`.
        7.  Closes the source file.

*(Note: The routine `etsf_io_file_contents` mentioned in the public API documentation for `etsf_io_file_check` is not present in this private file. It is assumed to be located elsewhere, possibly in `etsf_io.f90` or another part of the `etsf_io_file` or `etsf_io` modules.)*

## Important Variables/Constants

This file primarily contains subroutines. It uses types and constants from other modules.
-   Local temporary arrays in `non_etsf_copy`: `integer_data(:)`, `real_data(:)`, `double_data(:)`, `string_data(:)` for holding data during copy.
-   `varids_to(:)` in `non_etsf_copy`: Stores NetCDF variable IDs in the target file for attribute copying.

## Usage Examples

These subroutines are private and used internally by the public routines in `etsf_io_file_public.f90`.
-   `etsf_io_file_merge` uses `file_infos_free`, `non_etsf_init`, and `non_etsf_copy`.
-   The `test_var` routine could be used for internal validation checks within the module, though its direct callers are not shown in this file.

## Dependencies and Interactions

### Internal Module Dependencies
-   The routines here are part of the `etsf_io_file` module and are called by the public API in `etsf_io_file_public.f90`.
-   Uses `file_infos_type`, `etsf_io_vars_free` which are assumed to be defined elsewhere in the `etsf_io_file` module or `etsf_io` module.

### `etsf_io_low_level` module
-   Extensively used for:
    -   `etsf_io_low_error` type and error handling (`etsf_io_low_error_set`, `etsf_io_low_error_update`, `etsf_io_low_error_handle`).
    -   Low-level file operations: `etsf_io_low_open_read`, `etsf_io_low_close`, `etsf_io_low_set_write_mode`, `etsf_io_low_set_define_mode`.
    -   Metadata reading: `etsf_io_low_read_var_infos`, `etsf_io_low_free_var_infos`, `etsf_io_low_read_dim`.
    -   Dimension/variable definition: `etsf_io_low_write_dim`, `etsf_io_low_def_var`.
    -   Generic data read/write: `etsf_io_low_read_var`, `etsf_io_low_write_var`.
    -   Attribute copying: `etsf_io_low_copy_all_att`.
    -   Constants for data types (`etsf_io_low_integer`, etc.).

### `etsf_io` module
-   `etsf_io_split_free`.
-   Constants like `etsf_grp_none`.
-   The `var_list` component of `file_infos_type` (likely `etsf_io_data_var_list`) uses types/structures from `etsf_io` for variable grouping and split status.

### External Library Dependencies
-   Indirectly depends on NetCDF through calls to `etsf_io_low_level` routines.
```
