# `etsf_io.f90` - ETSF I/O Command-Line Utility

## Overview

`etsf_io.f90` is a Fortran program, named `etsf_io_ploumploum` in the source, that provides a command-line interface (CLI) for performing various operations on ETSF (Electronic Structure Task Force) files. It utilizes the ETSF I/O library (specifically the `etsf_io_low_level`, `etsf_io`, and `etsf_io_file` modules) to interact with these files. The utility allows users to merge split files, inspect file contents (i.e., determine which ETSF specifications they conform to), and check files against specific ETSF data specifications.

## Key Components

### Program: `etsf_io_ploumploum`
This is the main program unit that parses command-line arguments and dispatches actions to the appropriate library functions.

### Core Functionality (Actions)
The program's behavior is determined by the `-a` or `--action` command-line argument.

-   **`merge`**:
    -   **Purpose**: Merges multiple input ETSF files (which are typically parts of a split dataset) into a single output ETSF file.
    -   **Required Arguments**:
        -   At least two input files specified with `-i` or `--input-file`.
        -   One output file specified with `-o` or `--output-file`.
    -   **Internal Call**: `CALL etsf_io_file_merge(output_file, input_files_array, lstat, error_data)` from the `etsf_io_file` module.

-   **`content`**:
    -   **Purpose**: Analyzes a single ETSF file and reports which ETSF data specifications (e.g., for density, wavefunctions, geometry) it conforms to.
    -   **Required Arguments**: One ETSF file path provided as a positional argument after all options have been parsed.
    -   **Internal Call**: `CALL etsf_io_file_contents(read_flags, errors_array, input_file_path, lstat, error_data)` from the `etsf_io_file` module.
    -   **Output**: Lists all known ETSF specifications and indicates if the file contains data conforming to them ("Ok" or "No"). If "No", a reason or specific error encountered during the check is provided.

-   **`check`**:
    -   **Purpose**: Checks the validity of an ETSF file against one or more specified ETSF data specifications (flags).
    -   **Required Arguments**:
        -   At least one specification flag specified with `-f` or `--flag`. To see a list of available flags, use the `-l` or `--list` option in conjunction with `-a check`.
        -   One ETSF file path provided as a positional argument.
    -   **Internal Call**: `CALL etsf_io_file_check(input_file_path, check_flags_combined, lstat, error_data)` from the `etsf_io_file` module.
    -   **Output**: Reports success or failure of the check. If it fails, error details from the `etsf_io_low_error` structure are displayed.

### Command-Line Option Parsing
The program manually parses command-line arguments using intrinsic Fortran functions (`iargc`, `getarg`) and a custom helper function `get_option` contained within the main program.

-   **`get_option(code, name, value, i_arg, with_value)` (Internal Function)**:
    -   **Purpose**: A helper function to parse a specific command-line option. It checks for a short form (e.g., `-a`) and a long form (e.g., `--action`).
    -   **Arguments**:
        -   `code` (`CHARACTER(LEN=1), INTENT(IN)`): Short option character (e.g., 'a').
        -   `name` (`CHARACTER(LEN=*), INTENT(IN)`): Long option string (e.g., "action").
        -   `value` (`CHARACTER(LEN=256), INTENT(OUT)`): Stores the value of the option if it takes one.
        -   `i_arg` (`INTEGER, INTENT(INOUT)`): Current argument index being processed.
        -   `with_value` (`LOGICAL, INTENT(IN), OPTIONAL`): If `.FALSE.`, the option is treated as a flag without an immediately following value (e.g. `-h`). Default is `.TRUE.`, meaning the option expects a value.
    -   **Returns**: (`LOGICAL`) `.TRUE.` if the option (either short or long form) is found at the current `i_arg`.
    -   **Behavior**: Handles options like `-aaction_val`, `-a action_val`, `--action=action_val`, and `--action action_val`.

-   **`usage()` (Internal Subroutine)**:
    -   **Purpose**: Prints a detailed help message describing the utility's usage, available actions, options, and examples.
    -   **Triggered by**: The `-h` or `--help` option, or by errors in command-line arguments (e.g., missing action, insufficient files for merge).

## Important Variables/Constants

-   `action` (`CHARACTER(LEN=80)`): Stores the action specified by `-a` (e.g., "merge", "content", "check").
-   `input_files(256)` (`CHARACTER(LEN=256)`): Array to store paths of input files for the 'merge' action, specified by `-i`.
-   `n_input_files` (`INTEGER`): Count of input files provided.
-   `output_file` (`CHARACTER(LEN=256)`): Path for the output file for the 'merge' action, specified by `-o`.
-   `get_specs_list` (`LOGICAL`): Set to `.TRUE.` if `-l` or `--list` is provided with `-a check`, to list available specification flags.
-   `input_flags(256)` (`CHARACTER(LEN=256)`): Array to store specification flags for the 'check' action, specified by `-f`.
-   `n_input_flags` (`INTEGER`): Count of input flags provided.
-   `input_args(256)` (`CHARACTER(LEN=256)`): Array to store positional arguments (typically the main file to operate on for 'content' and 'check' actions).
-   `n_input_args` (`INTEGER`): Count of positional arguments.
-   `lstat` (`LOGICAL`): General status variable for calls to ETSF I/O library functions.
-   `error` (`TYPE(etsf_io_low_error)`): Stores error information from ETSF I/O library calls, particularly for 'merge' and 'check' actions.
-   `errors(etsf_nspecs_data)` (`TYPE(etsf_io_low_error)`): Array from `etsf_io` module. Stores specific errors for each specification check in the 'content' action. `etsf_nspecs_data` is a constant from the `etsf_io` module.
-   `read_flags` (`INTEGER`): Bitmask an `etsf_io_file_contents` that indicates which specifications are present.
-   `check_flags` (`INTEGER`): Bitmask passed to `etsf_io_file_check`, constructed from user-provided flags.
-   `etsf_specs_names(etsf_nspecs_data)` (`CHARACTER(LEN=*)` from `etsf_io` module): Array of specification names, used for listing and matching flags.

## Usage Examples

**Merge three files:**
```bash
./etsf_io_ploumploum -a merge -i file1.nc -i file2.nc -i file3.nc -o output.nc
```

**Get the contents (conforming specifications) of `test.nc`:**
```bash
./etsf_io_ploumploum -a content test.nc
```

**List available specification flags for checking:**
```bash
./etsf_io_ploumploum -a check -l
```

**Check `test.nc` against 'crystallographic_group' and 'wavefunctions' specifications (assuming these are valid flag names):**
```bash
./etsf_io_ploumploum -a check -f crystallographic_group -f wavefunctions test.nc
```

**Display help:**
```bash
./etsf_io_ploumploum -h
# or
./etsf_io_ploumploum --help
```

## Dependencies and Interactions

### Internal Dependencies
-   **`get_option()` function (contained)**: Used for parsing command-line options.
-   **`usage()` subroutine (contained)**: Called to display help messages.

### ETSF I/O Library Modules
-   **`etsf_io_low_level`**: Provides the `etsf_io_low_error` derived type definition and the `etsf_io_low_error_handle` subroutine for printing error details.
-   **`etsf_io`**: Provides constants like `etsf_nspecs_data` (number of ETSF specifications) and `etsf_specs_names` (array of specification names).
-   **`etsf_io_file`**: This is the primary interface to the higher-level library functions that perform the core actions:
    -   `etsf_io_file_merge`
    -   `etsf_io_file_contents`
    -   `etsf_io_file_check`

### External Library Dependencies
-   Relies on the compiled ETSF I/O library (`libetsf_io.a` or similar dynamic library) which, in turn, depends on a NetCDF library.
-   Uses Fortran intrinsic functions for command-line argument processing:
    -   **`iargc()`**: Returns the number of command-line arguments.
    -   **`getarg(i_arg, value)`**: Retrieves the `i_arg`-th command-line argument into `value`.

## Fortran Intrinsics Used for Argument Parsing
-   **`iargc()`**: Returns the number of command-line arguments (excluding the program name).
-   **`getarg(i_arg, value)`**: Retrieves the `i_arg`-th command-line argument into `value`. Argument `0` is typically the program name.
```
