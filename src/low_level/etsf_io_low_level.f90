!!****h* low_level/etsf_io_low_level
!! NAME
!!  etsf_io_low_level -- ESTF I/O low level wrapper around NetCDF routines
!!
!! FUNCTION
!!  This module is used to wrap commonly used NetCDF calls. It gives an API
!!  which should be safe with automatic dimensions checks, and easy to use
!!  with methods only needed by a parser/writer library focused on the
!!  ETSF specifications. Nevertheless, this module can be used for other
!!  purpose than only reading/writing files conforming to ETSF specifications.
!!
!!  It also support an optional error handling structure. This structure
!!  can be used on any methods to get fine informations about any failure.
!!
!!  All methods have a logical argument that is set to .true. if everything
!!  went fine. In that case, all output arguments have relevant values. If @lstat
!!  is .false., no output values should be used since their values are not
!!  guaranteed.
!!
!! COPYRIGHT
!!  Copyright (C) 2006, 2007 (Damien Caliste)
!!  This file is distributed under the terms of the
!!  GNU Lesser General Public License, see the COPYING file
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!
!!***
module etsf_io_low_level

  use netcdf

  implicit none

  ! Basic variables  
  character(len = *), parameter, private :: &
    & etsf_io_low_file_format = "ETSF Nanoquanta"
  character(len = *), parameter, private :: &
    & etsf_io_low_conventions = "http://www.etsf.eu/fileformats/"

  ! Error handling
  integer, parameter, private :: nb_access_mode = 7
  character(len = 15), dimension(nb_access_mode), parameter, private :: &
    & etsf_io_low_error_mode = (/ "define         ", &
                                & "get            ", &
                                & "input/output   ", &
                                & "inquire        ", &
                                & "put            ", &
                                & "specifications ", &
                                & "copy           " /)

  integer, parameter, private :: nb_target_type = 12
  character(len = 22), dimension(nb_target_type), parameter, private :: &
    & etsf_io_low_error_type = (/ "attribute             ", "dimension ID          ", &
                                & "dimension             ", "end definitions       ", &
                                & "define mode           ", "create file           ", &
                                & "open file for reading ", "open file for writing ", &
                                & "variable              ", "variable ID           ", &
                                & "close file            ", "routine argument      " /)

  
  include "public_variables.f90"
    
  !!****m* etsf_io_low_read_group/etsf_io_low_read_var
  !! NAME
  !!  etsf_io_low_read_var
  !!
  !! SYNOPSIS
  !!  * call etsf_io_low_read_var(ncid, varname, var, lstat,
  !!                              ncvarid, start, count, map, error_data)
  !!  * call etsf_io_low_read_var(ncid, varname, var, charlen, lstat,
  !!                              ncvarid, start, count, map, error_data)
  !!
  !! FUNCTION
  !!  This is a generic interface to read values of a variables (either integer
  !!  or double or character). Before puting values in the @var argument, the
  !!  dimensions of the read data are compared with the assumed dimensions of @var.
  !!  The type is also checked, based on the type of the @var argument. Using
  !!  this routine is then a safe way to read data from a NetCDF file. The size and
  !!  shape of @var can be either a scalar, a one dimensional array or a multi
  !!  dimensional array. Strings should be given with their length. See
  !!  the example below on how to read a string. @var can also be a #etsf_io_low_var_double,
  !!  or a #etsf_io_low_var_integer. In this case, the associated pointer is used
  !!  as the storage area for the read values.
  !!
  !!  If the shape of the given storage variable (@var) and the definition of the
  !!  corresponding NetCDF variable differ ; the read is done only if the number of
  !!  elements are identical. Number of elements is the product over all dimensions
  !!  of the size (see example below).
  !!
  !!  It is also possible to read some particular dimensions of one variable using
  !!  the optional @start, @count and @map arguments. These are identical to their
  !!  counterpart in NetCDF, with small differences and improvements:
  !!  * start is used to define for each dimensions of the ETSF variable where to
  !!    start reading. Indexes are numbered from 1 to the size of their dimension.
  !!  * count is used to given the number of elements to be read for each dimenion.
  !!    The sum start(i) + count(i) - 1 must be lower than the size of the i dimension.
  !!    As an improvement compared to NetCDF count argument, if one wants to read all
  !!    values from the dimension i, one can put count(i) = 0 instead of the size
  !!    of the dimension itself which is not always easily accessible.
  !!  * map is used to describe where to write data in memory when reading an ETSF
  !!    variable. It gives for each dimension how many elements must be skip in memory.
  !!    It also can used to switch order of dimensions. For instance, for an ETSF
  !!    variable etsf_var(3,2) that we want to put in a variable my_var(2,3), we
  !!    will use a map (/ 2, 1 /) which means that all values from first index of the
  !!    etsf_var will put put every 2 elements in memory, while values from the second index
  !!    will be put every single element.
  !!  The order of dimensions are given in the Fortran order (inverse of the specification
  !!  order).
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * varname = a string identifying a variable.
  !!
  !! OUTPUT
  !!  * var = an allocated array to store the read values (or a simple scalar).
  !!  * lstat = .true. if operation succeed.
  !!  * start = (optional) an array, with the same size than the shape of the NetCDF
  !!            variable to be read. Give the first index to be read for each dimension.
  !!            By default value is 1 for each dimension.
  !!  * count = (optional) an array, with the same size than the shape of the NetCDF
  !!            variable to be read. Give the number of indexes to be read for each dimension.
  !!            By default value is the size for each dimension.
  !!  * map = (optional) an array, with the same size than the shape of the NetCDF
  !!          variable to be read. Give how values are written into memory. By default
  !!          map = (/ 1, (product(dims(1:i), i = 1, shape - 1) /)
  !!  * ncvarid = (optional) the id used by NetCDF to identify the read variable.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! EXAMPLE
  !!  Read a string stored in "exchange_functional" variable of length 80:
  !!   character(len = 80) :: var
  !!   call etsf_io_low_read_var(ncid, "exchange_functional", var, 80, lstat)
  !!
  !!  Get one single integer stored in "space_group":
  !!   integer :: sp
  !!   call etsf_io_low_read_var(ncid, "space_group", sp, lstat)
  !!
  !!  Get a 2 dimensional array storing reduced atom coordinates:
  !!   double precision :: coord(3, 5)
  !!   call etsf_io_low_read_var(ncid, "reduced_atom_positions", coord, lstat)
  !!
  !!  Get a 2 dimensional array stored as a four dimensional array:
  !!   NetCDF def: density(2, 3, 3, 3) # dimensions in NetCDF are reverted
  !!                                   # compared to Fortran style
  !!   double precision :: density(27, 2)
  !!   call etsf_io_low_read_var(ncid, "density", density, lstat)
  !!
  !!  Get the last 3 dimensions of a 4D array:
  !!   NetCDF def: density(2, 3, 4, 5) # dimensions in NetCDF are reverted
  !!                                   # compared to Fortran style
  !!   double precision :: density_down(5, 4, 3)
  !!   call etsf_io_low_read_var(ncid, "density", density_down, lstat, &
  !!                           & start = (/ 1, 1, 1, 2 /), count = (/ 0, 0, 0, 1 /))
  !!
  !!  Get the last 3 dimensions of a 4D array and store them into a 1D array:
  !!   NetCDF def: density(2, 3, 3, 3) # dimensions in NetCDF are reverted
  !!                                   # compared to Fortran style
  !!   double precision :: density_up(27)
  !!   call etsf_io_low_read_var(ncid, "density", density_up, lstat, &
  !!                           & start = (/ 1, 1, 1, 2 /), count = (/ 0, 0, 0, 1 /))
  !!
  !!  Read data to a dimension stored in the main program, without duplication
  !!  of data in memory:
  !!   integer, target :: atom_species(number_of_atoms)
  !!   ...
  !!   type(etsf_io_low_var_integer) :: var
  !!   var%data1D => atom_species
  !!   call etsf_io_low_read_var(ncid, "atom_species", var, lstat)
  !!***
  !Generic interface of the routines etsf_io_low_read_var
  interface etsf_io_low_read_var
    module procedure read_var_integer_var
    module procedure read_var_integer_0D
    module procedure read_var_integer_1D
    module procedure read_var_integer_2D
    module procedure read_var_integer_3D
    module procedure read_var_integer_4D
    module procedure read_var_integer_5D
    module procedure read_var_integer_6D
    module procedure read_var_integer_7D
    module procedure read_var_double_var
    module procedure read_var_double_0D
    module procedure read_var_double_1D
    module procedure read_var_double_2D
    module procedure read_var_double_3D
    module procedure read_var_double_4D
    module procedure read_var_double_5D
    module procedure read_var_double_6D
    module procedure read_var_double_7D
    module procedure read_var_character_1D
    module procedure read_var_character_2D
    module procedure read_var_character_3D
    module procedure read_var_character_4D
    module procedure read_var_character_5D
    module procedure read_var_character_6D
    module procedure read_var_character_7D
  end interface etsf_io_low_read_var
  !End of the generic interface of etsf_io_low_read_var
  
  !!****m* etsf_io_low_read_group/etsf_io_low_read_att
  !! NAME
  !!  etsf_io_low_read_att
  !!
  !! SYNOPSIS
  !!  * call etsf_io_low_read_att(ncid, ncvarid, attname, attlen, att, lstat, error_data)
  !!  * call etsf_io_low_read_att(ncid, ncvarid, attname, att, lstat, error_data)
  !!  * call etsf_io_low_read_att(ncid, varname, attname, attlen, att, lstat, error_data)
  !!  * call etsf_io_low_read_att(ncid, varname, attname, att, lstat, error_data)
  !!
  !! FUNCTION
  !!  This is a generic interface to read values of an attribute (either integer,
  !!  real, double or character). Before puting values in the @att argument, the
  !!  dimensions of the read data are compared with the given dimensions (@attlen).
  !!  The type is also checked, based on the type of the @att argument. Using
  !!  this routine is then a safe way to read attribute data from a NetCDF file.
  !!  The size and shape of @att can be either a scalar or a one dimensional array.
  !!  In the former case, the argument @attlen must be omitted. Strings are considered
  !!  to be one dimensional arrays. See the example below on how to read a string.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * ncvarid = the id of the variable the attribute is attached to.
  !!              in the case of global attributes, use the constance
  !!              NF90_GLOBAL (when linking against NetCDF) or #etsf_io_low_global_att
  !!              which is a wrapper exported by this module (see #ETSF_IO_LOW_CONSTANTS).
  !!  * varname = can be used instead of ncvarid to select a variable by its name.
  !!  * attname = a string identifying an attribute.
  !!  * attlen = the size of the array @att (when required).
  !!
  !! OUTPUT
  !!  * att = an allocated array to store the read values. When @attlen is
  !!          omitted, this argument @att must be a scalar, not an array.
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! EXAMPLE
  !!  Read a string stored in "symmorphic" attribute of length 80:
  !!   character(len = 80) :: att
  !!   call etsf_io_low_read_att(ncid, ncvarid, "symmorphic", 80, att, lstat)
  !!
  !!  Get one single real stored in "file_format_version" which is a global attribute:
  !!   real :: version
  !!   call etsf_io_low_read_att(ncid, etsf_io_low_global_att, "file_format_version", version, lstat)
  !!
  !!***
  !Generic interface of the routines etsf_io_low_read_att
  interface etsf_io_low_read_att
    module procedure read_att_id_integer_0D
    module procedure read_att_id_real_0D
    module procedure read_att_id_double_0D
    module procedure read_att_id_integer_1D
    module procedure read_att_id_real_1D
    module procedure read_att_id_double_1D
    module procedure read_att_id_character_1D
    module procedure read_att_integer_0D
    module procedure read_att_real_0D
    module procedure read_att_double_0D
    module procedure read_att_integer_1D
    module procedure read_att_real_1D
    module procedure read_att_double_1D
    module procedure read_att_character_1D
  end interface etsf_io_low_read_att
  !End of the generic interface of etsf_io_low_read_att

  !!****m* etsf_io_low_read_group/etsf_io_low_read_flag
  !! NAME
  !!  etsf_io_low_read_flag
  !!
  !! SYNOPSIS
  !!  * call etsf_io_low_read_flag(ncid, flag, ncvarid, attname, lstat, error_data)
  !!  * call etsf_io_low_read_flag(ncid, flag, varname, attname, lstat, error_data)
  !!
  !! FUNCTION
  !!  This method is a specialized version of etsf_io_low_read_att(). It reads
  !!  the attribute @attname of the given variable and set @flag to .true. if
  !!  the attribute value is "yes" or "YES" or "Yes", .false. otherwise.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * ncvarid = the id of the variable the attribute is attached to.
  !!              in the case of global attributes, use the constance
  !!              NF90_GLOBAL (when linking against NetCDF) or #etsf_io_low_global_att
  !!              which is a wrapper exported by this module (see #ETSF_IO_LOW_CONSTANTS).
  !!  * varname = can be used instead of ncvarid to select a variable by its name.
  !!  * attname = a string identifying an attribute.
  !!
  !! OUTPUT
  !!  * flag = .true. if the attribute match "yes" or its variant.
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!***
  interface etsf_io_low_read_flag
    module procedure read_flag_id
    module procedure read_flag
  end interface etsf_io_low_read_flag
  !End of the generic interface of etsf_io_low_read_flag

  !!****m* etsf_io_low_write_group/etsf_io_low_def_var
  !! NAME
  !!  etsf_io_low_def_var
  !!
  !! SYNOPSIS
  !!  * call etsf_io_low_def_var(ncid, varname, vartype, vardims, lstat, ncvarid, error_data)
  !!  * call etsf_io_low_def_var(ncid, varname, vartype, lstat, ncvarid, error_data)
  !!
  !! FUNCTION
  !!  In the contrary of dimensions or attributes, before using a write method on variables
  !!  they must be defined using such methods. This allow to choose the type, the shape
  !!  and the size of a new variable. Once defined, a variable can't be changed or removed.
  !!
  !!  One can add scalars, one dimensional arrays or multi-dimensional arrays (restricted
  !!  to a maximum of 7 dimensions). See the examples below to know how to use such methods.
  !!
  !!  As in pure NetCDF, it is impossible to overwrite the definition of a variable.
  !!  Nevertheless, the method returns .true. in @lstat, if the definition is done a second
  !!  time with the same type, shape and dimensions.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with write access (define mode).
  !!  * varname = the name for the new variable.
  !!  * vartype = the type of the new variable (see #ETSF_IO_LOW_CONSTANTS).
  !!  * vardims = an array with the size for each dimension of the variable.
  !!              Each size is given by the name of its dimension. Thus dimensions
  !!              must already exist (see etsf_io_low_write_dim()).
  !!              When omitted, the variable is considered as a scalar.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * ncvarid = (optional) the id used by NetCDF to identify the written variable.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! EXAMPLE
  !!  Define a string stored as "basis_set" of length "character_string_length":
  !!   call etsf_io_low_def_var(ncid, "basis_set", etsf_io_low_character, &
  !!                          & (/ "character_string_length" /), lstat)
  !!
  !!  Define one integer stored as "space_group":
  !!   call etsf_io_low_def_var(ncid, "space_group", etsf_io_low_integer, lstat)
  !!
  !!  Define a two dimensional array of double stored as "reduced_symetry_translations":
  !!   call etsf_io_low_def_var(ncid, "reduced_symetry_translations", etsf_io_low_double, &
  !!                          & (/ "number_of_reduced_dimensions", &
  !!                          &    "number_of_symetry_operations" /), lstat)
  !!***
  !Generic interface of the routines etsf_io_low_def_var
  interface etsf_io_low_def_var
    module procedure etsf_io_low_def_var_0D
    module procedure etsf_io_low_def_var_nD
  end interface etsf_io_low_def_var
  !End of the generic interface of etsf_io_low_def_var

  !!****m* etsf_io_low_write_group/etsf_io_low_write_att
  !! NAME
  !!  etsf_io_low_write_att
  !!
  !! SYNOPSIS
  !!  call etsf_io_low_write_att(ncid, ncvarid, attname, att, lstat, error_data)
  !!
  !! FUNCTION
  !!  When in defined mode, one can add attributes and set then a value in one call
  !!  using such a method. Attributes can be strings, scalar or one dimensional arrays
  !!  of integer, real or double precision.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with write access (define mode).
  !!  * ncvarid = the id of the variable the attribute is attached to.
  !!              in the case of global attributes, use the constance
  !!              NF90_GLOBAL (when linking against NetCDF) or #etsf_io_low_global_att
  !!              which is a wrapper exported by this module (see #ETSF_IO_LOW_CONSTANTS).
  !!  * attname = the name for the new attribute.
  !!  * att = the value, can be a string a scalar or a one-dimension array.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! EXAMPLE
  !!  Write a string stored as "symmorphic", attribute of varaible ncvarid:
  !!   call etsf_io_low_def_var(ncid, ncvarid, "symmorphic", "Yes", lstat)
  !!
  !!  Write one real stored as "file_format_version", global attribute:
  !!   call etsf_io_low_def_var(ncid, etsf_io_low_global_att, "file_format_version", &
  !!                          & 1.3, lstat)
  !!***
  !Generic interface of the routines etsf_io_low_write_att
  interface etsf_io_low_write_att
    module procedure write_att_integer_0D
    module procedure write_att_real_0D
    module procedure write_att_double_0D
    module procedure write_att_integer_1D
    module procedure write_att_real_1D
    module procedure write_att_double_1D
    module procedure write_att_character_1D
    module procedure write_att_id_integer_0D
    module procedure write_att_id_real_0D
    module procedure write_att_id_double_0D
    module procedure write_att_id_integer_1D
    module procedure write_att_id_real_1D
    module procedure write_att_id_double_1D
    module procedure write_att_id_character_1D
  end interface etsf_io_low_write_att
  !End of the generic interface of etsf_io_low_write_att

  !!****m* etsf_io_low_write_group/etsf_io_low_write_var
  !! NAME
  !!  etsf_io_low_write_var
  !!
  !! SYNOPSIS
  !!  * call etsf_io_low_write_var(ncid, varname, var, lstat,
  !!                               ncvarid, start, count, map, error_data)
  !!  * call etsf_io_low_write_var(ncid, varname, var, charlen, lstat,
  !!                               ncvarid, start, count, map, error_data)
  !!
  !! FUNCTION
  !!  This is a generic interface to write values of a variables (either integer
  !!  or double or strings). Before using such methods, variables must have been
  !!  defined using etsf_io_low_def_var(). Before writting values from the @var argument, the
  !!  dimensions of the given data are compared with the defined dimensions.
  !!  The type is also checked, based on the type of the @var argument. Using
  !!  this routine is then a safe way to write data from a NetCDF file. The size and
  !!  shape of @var can be either a scalar, a one dimensional array or a multi
  !!  dimensional array. Strings should be given with their length. See
  !!  the example below on how to write a string. @var can also be a #etsf_io_low_var_double,
  !!  or a #etsf_io_low_var_integer. In this case, the associated pointer is used
  !!  as the storage area for the written values.
  !!
  !!  If the shape of the input data variable (@var) and the definition of the
  !!  corresponding NetCDF variable differ ; the write action is performed only if the number of
  !!  elements are identical. Number of elements is the product over all dimensions
  !!  of the size (see example below).
  !!
  !!  It is also possible to write some particular dimensions of one variable using
  !!  the optional @start, @count and @map arguments. These are identical to their
  !!  counterpart in NetCDF, with small differences and improvements:
  !!  * start is used to define for each dimensions of the ETSF variable where to
  !!    start writing. Indexes are numbered from 1 to the size of their dimension.
  !!  * count is used to given the number of elements to be read for each dimenion.
  !!    The sum start(i) + count(i) - 1 must be lower than the size of the i dimension.
  !!    As an improvement compared to NetCDF count argument, if one wants to write all
  !!    values from the dimension i, one can put count(i) = 0 instead of the size
  !!    of the dimension itself which is not always easily accessible.
  !!  * map is used to describe where to read data in memory when writing an ETSF
  !!    variable. It gives for each dimension how many elements must be skip in memory.
  !!    It also can used to switch order of dimensions. For instance, for an ETSF
  !!    variable etsf_var(3,2) that we want to be put from a variable my_var(2,3), we
  !!    will use a map (/ 2, 1 /) which means that all values of first index of the
  !!    etsf_var will read from every 2 elements in memory, while values of the second index
  !!    will be read from every single element.
  !!  The order of dimensions are given in the Fortran order (inverse of the specification
  !!  order).
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * varname = a string identifying a variable.
  !!  * var = the values to be written, either a scalar or an array.
  !!  * charlen = when @var is a string or an array of strings, their size
  !!              must be given.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * start = (optional) an array, with the same size than the shape of the NetCDF
  !!            variable to be read. Give the first index to be read for each dimension.
  !!            By default value is 1 for each dimension.
  !!  * count = (optional) an array, with the same size than the shape of the NetCDF
  !!            variable to be read. Give the number of indexes to be read for each dimension.
  !!            By default value is the size for each dimension.
  !!  * map = (optional) an array, with the same size than the shape of the NetCDF
  !!          variable to be read. Give how values are written into memory. By default
  !!          map = (/ 1, (product(dims(1:i), i = 1, shape - 1) /)
  !!  * ncvarid = (optional) the id used by NetCDF to identify the written variable.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! EXAMPLE
  !!  Write a string stored in "exchange_functional" variable of length 80:
  !!   call etsf_io_low_read_var(ncid, "exchange_functional", "My functional", 80, lstat)
  !!
  !!  Write one single integer stored in "space_group":
  !!   call etsf_io_low_read_var(ncid, "space_group", 156, lstat)
  !!
  !!  Write a 2 dimensional array storing reduced atom coordinates:
  !!   double precision :: coord2d(3, 4)
  !!   coord2d = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /), (/ 3, 4 /))
  !!   call etsf_io_low_read_var(ncid, "reduced_atom_positions", coord2d, lstat)
  !!  or,
  !!   double precision :: coord(12)
  !!   coord = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /)
  !!   call etsf_io_low_read_var(ncid, "reduced_atom_positions", coord, lstat)
  !!
  !!  Write the last 3 dimensions of a 4D array:
  !!   NetCDF def: density(2, 3, 4, 5) # dimensions in NetCDF are reverted
  !!                                   # compared to Fortran style
  !!   double precision :: density_down(5, 4, 3)
  !!   call etsf_io_low_write_var(ncid, "density", density_down, lstat, &
  !!                            & start = (/ 1, 1, 1, 2 /), count = (/ 0, 0, 0, 1 /))
  !!
  !!  Write the last 3 dimensions of a 4D array and read them from a 1D array:
  !!   NetCDF def: density(2, 3, 3, 3) # dimensions in NetCDF are reverted
  !!                                   # compared to Fortran style
  !!   double precision :: density_up(27)
  !!   call etsf_io_low_write_var(ncid, "density", density_up, lstat, &
  !!                            & start = (/ 1, 1, 1, 2 /), count = (/ 0, 0, 0, 1 /))
  !!
  !!  Write data from a dimension stored in the main program, without duplication
  !!  of data in memory:
  !!   integer, target :: atom_species(number_of_atoms)
  !!   ...
  !!   type(etsf_io_low_var_integer) :: var
  !!   var%data1D => atom_species
  !!   call etsf_io_low_write_var(ncid, "atom_species", var, lstat)
  !!***
  !Generic interface of the routines etsf_io_low_write_var
  interface etsf_io_low_write_var
    module procedure write_var_integer_var
    module procedure write_var_integer_0D
    module procedure write_var_integer_1D
    module procedure write_var_integer_2D
    module procedure write_var_integer_3D
    module procedure write_var_integer_4D
    module procedure write_var_integer_5D
    module procedure write_var_integer_6D
    module procedure write_var_integer_7D
    module procedure write_var_double_var
    module procedure write_var_double_0D
    module procedure write_var_double_1D
    module procedure write_var_double_2D
    module procedure write_var_double_3D
    module procedure write_var_double_4D
    module procedure write_var_double_5D
    module procedure write_var_double_6D
    module procedure write_var_double_7D
    module procedure write_var_character_1D
    module procedure write_var_character_2D
    module procedure write_var_character_3D
    module procedure write_var_character_4D
    module procedure write_var_character_5D
    module procedure write_var_character_6D
    module procedure write_var_character_7D
  end interface etsf_io_low_write_var
  !End of the generic interface of etsf_io_low_write_var
  
  !!****f* etsf_io_low_var/etsf_io_low_var_associated
  !! NAME
  !!  etsf_io_low_var_associated
  !!
  !! FUNCTION
  !!  This function works as the associated() intrinsic function but with
  !!  pointers of undefined shapes (see #etsf_io_low_var_integer and #etsf_io_low_var_double).
  !!
  !! SYNOPSIS
  !!  call etsf_io_low_var_associated(array)
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * array <type(etsf_io_low_var_*)> = an undefined shape array.
  !!
  !! OUTPUT
  !!  * returns .true. if one of the array datanD is associated.
  !!***
  interface etsf_io_low_var_associated
    module procedure var_integer_associated
    module procedure var_double_associated
  end interface etsf_io_low_var_associated

  !!****f* etsf_io_low_var/etsf_io_low_var_multiply
  !! NAME
  !!  etsf_io_low_var_multiply
  !!
  !! FUNCTION
  !!  This subroutine is used to multiply the array of an unformatted pointer.
  !!  The factor must be of the same kind (integer or double) than the array.
  !!
  !! SYNOPSIS
  !!  call etsf_io_low_var_multiply(array, factor)
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * array <type(etsf_io_low_var_*)> = an undefined shape array.
  !!  * factor = the multiplying factor (either integr or double).
  !!***
  interface etsf_io_low_var_multiply
    module procedure var_integer_multiply
    module procedure var_double_multiply
  end interface

  !!****m* etsf_io_low_var_infos/etsf_io_low_read_var_infos
  !! NAME
  !!  etsf_io_low_read_var_infos
  !!
  !! FUNCTION
  !!  This method is used to retrieve informations about a variable:
  !!  * its NetCDF id or its name ;
  !!  * its type (see #ETSF_IO_LOW_CONSTANTS) ;
  !!  * its shape and length for each dimension.
  !!  One can get informations knowing the name or the id of a variable. Using
  !!  the dim_name argument to .true., the name of each used dimensions are
  !!  retrieved. In that case, the var_infos should be freed after use, calling
  !!  etsf_io_low_free_var_infos().
  !!
  !! SYNOPSIS
  !!  * call etsf_io_low_read_var_infos(ncid, varname, var_infos, lstat, error_data, dim_name, att_name)
  !!  * call etsf_io_low_read_var_infos(ncid, varid, var_infos, lstat, error_data, dim_name, att_name)
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with read access.
  !!  * varname = a string identifying a variable.
  !!  * varid = a integer identifying a variable.
  !!  * dim_name = (optional) if .true. retrieve the names of the dimensions,
  !!               and store them in a newly allocated array in the var_infos
  !!               structure (see etsf_io_low_free_var_infos()).
  !!  * att_name = (optional) if .true. retrieve the names of the attributes,
  !!               and store them in a newly allocated array in the var_infos
  !!               structure (see etsf_io_low_free_var_infos()).
  !!
  !! OUTPUT
  !!  * var_infos <type(etsf_io_low_var_infos)> = store, type, shape, dimensions and NetCDF id.
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!***
  interface etsf_io_low_read_var_infos
    module procedure read_var_infos_name
    module procedure read_var_infos_id
    module procedure read_var_infos
  end interface
  
  !!****g* etsf_io_low_level/etsf_io_low_error_group
  !! FUNCTION
  !!  These methods are used to handle errors generated by ETSF access.
  !!  For this a Fortran type is used and called #etsf_io_low_error. It
  !!  stores several informations such as the name of the method where the
  !!  error occured or a message describing the error. One can create an
  !!  error using etsf_io_low_error_set() and then put it into a nice
  !!  string for future use with etsf_io_low_error_to_str().
  !!
  !! SOURCE
  public :: etsf_io_low_error
  public :: etsf_io_low_error_set
  public :: etsf_io_low_error_update
  public :: etsf_io_low_error_to_str
  public :: etsf_io_low_error_handle
  !!***

  !!****g* etsf_io_low_level/etsf_io_low_file_group
  !! FUNCTION
  !!  When accessing a ETSF file, there is three routines to do that. One can:
  !!   * create a new file with etsf_io_low_open_create() ;
  !!   * read an already existing file with etsf_io_low_open_read() ;
  !!   * write data to a an already existing file with etsf_io_low_open_modify().
  !!
  !! SOURCE
  public :: etsf_io_low_close
  public :: etsf_io_low_open_create
  public :: etsf_io_low_open_modify
  public :: etsf_io_low_open_read
  !!***

  !!****g* etsf_io_low_level/etsf_io_low_check_group
  !! FUNCTION
  !!  These routines are used to check informations defined in an openend ETSF file.
  !!
  !! SOURCE
  public :: etsf_io_low_check_att
  public :: etsf_io_low_check_header
  public :: etsf_io_low_check_var
  !!***

  !!****g* etsf_io_low_level/etsf_io_low_read_group
  !! FUNCTION
  !!  These routines are used read data from an ETSF file. These data can be:
  !!  * dimensions ;
  !!  * attributes (global or not) ;
  !!  * variables.
  !!
  !! SOURCE
  public :: etsf_io_low_read_att
  public :: etsf_io_low_read_flag
  public :: etsf_io_low_read_dim
  public :: etsf_io_low_read_var
  !!***

  !!****g* etsf_io_low_level/etsf_io_low_write_group
  !! FUNCTION
  !!  These routines are used write (or define) data from an ETSF file. These data can be:
  !!  * dimensions ;
  !!  * attributes (global or not) ;
  !!  * variables.
  !!
  !! SOURCE
  public :: etsf_io_low_def_var
  public :: etsf_io_low_write_att
  public :: etsf_io_low_copy_all_att
  public :: etsf_io_low_write_dim
  public :: etsf_io_low_write_var
  !!***

  !!****g* etsf_io_low_level/etsf_io_low_var
  !! FUNCTION
  !!  These routines are used to defined an array without predefined shape.
  !!
  !! SOURCE
  public :: etsf_io_low_var_integer
  public :: etsf_io_low_var_double
  public :: etsf_io_low_var_multiply
  public :: etsf_io_low_var_associated
  !!***
  
  ! Private variables & methods.
  private :: var_integer_associated
  private :: var_double_associated
  private :: var_integer_multiply
  private :: var_double_multiply
contains

  !!****m* etsf_io_low_error_group/etsf_io_low_error_set
  !! NAME
  !!  etsf_io_low_error_set
  !!
  !! FUNCTION
  !!  This routine is used to initialise a #etsf_io_low_error object with values.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * mode = a value from #ERROR_MODE, specifying the action when the error occurs.
  !!  * type = a value from #ERROR_TYPE, specifying the kind of target.
  !!  * parent = the name of the routine in which the error occurs.
  !!  * tgtid = (optional) an id representing the target (or -1).
  !!  * tgtname = (optional) a name representing the target (or "").
  !!  * errmess = (optional) a string with an explanation.
  !!
  !! OUTPUT
  !!  * error_data <type(etsf_io_low_error)> = the error with sensible values in its fields.
  !!
  !! SOURCE
  subroutine etsf_io_low_error_set(error_data, mode, type, parent, tgtid, tgtname, errid, errmess)
    type(etsf_io_low_error), intent(out)     :: error_data
    integer, intent(in)                      :: mode, type
    character(len = *), intent(in)           :: parent
    integer, intent(in), optional            :: tgtid, errid
    character(len = *), intent(in), optional :: tgtname, errmess

    ! Consistency checkings    
    if (mode < 1 .or. mode > nb_access_mode) then
      write(0, *) "   *** ETSF I/O Internal error ***"
      write(0, *) "   mode argument out of range: ", mode
      return
    end if
    if (type < 1 .or. type > nb_target_type) then
      write(0, *) "   *** ETSF I/O Internal error ***"
      write(0, *) "   type argument out of range: ", type
      return
    end if
    
    ! Storing mandatory informations
    write(error_data%backtrace(1), "(A)") parent(1:min(80, len(parent)))
    error_data%backtraceId    = 1
    error_data%access_mode_id = mode
    write(error_data%access_mode_str, "(A)") etsf_io_low_error_mode(mode)
    error_data%target_type_id = type
    write(error_data%target_type_str, "(A)") etsf_io_low_error_type(type)

    ! Storing possible other informations
    if (present(tgtid)) then
      error_data%target_id = tgtid
    else
      error_data%target_id = -1
    end if
    if (present(tgtname)) then
      write(error_data%target_name, "(A)") trim(tgtname(1:min(80, len(tgtname))))
    else
      write(error_data%target_name, "(A)") ""
    end if
    if (present(errid)) then
      error_data%error_id = errid
    else
      error_data%error_id = nf90_noerr
    end if
    if (present(errmess)) then
      write(error_data%error_message, "(A)") trim(errmess(1:min(256, len(errmess))))
    else
      write(error_data%error_message, "(A)") ""
    end if
  end subroutine etsf_io_low_error_set
  !!***

  !!****m* etsf_io_low_error_group/etsf_io_low_error_update
  !! NAME
  !!  etsf_io_low_error_update
  !!
  !! FUNCTION
  !!  This method must be called when a routine receives an error and need
  !!  to propagate it further.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * method = the name of the routine that propagate the error.
  !!
  !! SIDE EFFECTS
  !!  * error <type(etsf_io_low_error)> = informations about an error.
  !!
  !! SOURCE
  subroutine etsf_io_low_error_update(error, method)
    type(etsf_io_low_error), intent(inout) :: error
    character(len = *), intent(in)         :: method

    if (error%backtraceId == 100) return

    error%backtraceId = error%backtraceId + 1
    write(error%backtrace(error%backtraceId), "(A)") method(1:min(80, len(method)))
  end subroutine etsf_io_low_error_update
  !!***

  !!****m* etsf_io_low_error_group/etsf_io_low_error_to_str
  !! NAME
  !!  etsf_io_low_error_to_str
  !!
  !! FUNCTION
  !!  This method can be used to get a string from the given error.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * error_data <type(etsf_io_low_error)>=informations about an error.
  !!
  !! OUTPUT
  !!  * str = a string to write the error message to.
  !!
  !! SOURCE
  subroutine etsf_io_low_error_to_str(str, error_data)
    character(len = etsf_io_low_error_len), intent(out)   :: str
    type(etsf_io_low_error), intent(in) :: error_data
    
    character(len = 80)  :: line_tgtname, line_tgtid, line_messid
    character(len = 256) :: line_mess
    integer              :: i
    
    if (trim(error_data%target_name) /= "") then
      write(line_tgtname, "(A,A,A)") "  Target (name)      : ", trim(error_data%target_name), char(10)
    else
      write(line_tgtname, "(A)") ""
    end if
    if (error_data%target_id >= 0) then
      write(line_tgtid, "(A,I0,A)") "  Target (id)        : ", error_data%target_id, char(10)
    else
      write(line_tgtid, "(A)") ""
    end if
    if (trim(error_data%error_message) /= "") then
      write(line_mess, "(A,A,A)") "  Error message      : ", trim(error_data%error_message), char(10)
    else
      write(line_mess, "(A)") ""
    end if
    if (error_data%error_id /= nf90_noerr) then
      write(line_messid, "(A,I0,A)") "  Error id           : ", error_data%error_id, char(10)
    else
      write(line_messid, "(A)") ""
    end if

    ! Write the back trace
    write(str, "(A,A,A)") "  Backtrace          : ", &
         & trim(error_data%backtrace(error_data%backtraceId)), "()"
    do i = error_data%backtraceId - 1, 1, -1
       if (len(trim(str)) + 80 + 26 < etsf_io_low_error_len) then
          write(str, "(5A)") trim(str(1:3900)), char(10), &
               & "                       ", trim(error_data%backtrace(i)), "()"
       end if
    end do

    ! Write all the rest.
    write(str, "(11A)") trim(str(1:3000)), char(10),&
               & "  Action performed   : ", trim(error_data%access_mode_str), &
               & " ", trim(error_data%target_type_str), char(10), &
               & trim(line_tgtname), &
               & trim(line_tgtid), &
               & trim(line_mess), &
               & trim(line_messid)
  end subroutine etsf_io_low_error_to_str
  !!***

  !!****m* etsf_io_low_error_group/etsf_io_low_error_handle
  !! NAME
  !!  etsf_io_low_error_handle
  !!
  !! FUNCTION
  !!  This method can be used to output the informations contained in an error
  !!  structure. The output is done on standard output. Write your own method
  !!  if custom error handling is required.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * error_data <type(etsf_io_low_error)>=informations about an error.
  !!
  !! SOURCE
  subroutine etsf_io_low_error_handle(error_data)
    type(etsf_io_low_error), intent(in) :: error_data

    integer :: i
      
    ! Error handling
    write(*,*) 
    write(*,*) "    ***"
    write(*,*) "    *** ETSF I/O ERROR"
    write(*,*) "    ***"
    write(*,*) "    *** Backtrace          : ", &
         & trim(error_data%backtrace(error_data%backtraceId)), "()"
    do i = error_data%backtraceId - 1, 1, -1
       write(*,*) "    ***                      ", trim(error_data%backtrace(i)), "()"
    end do
    write(*,*) "    *** Action performed   : ", trim(error_data%access_mode_str), &
             & " ", trim(error_data%target_type_str)
    if (trim(error_data%target_name) /= "") then
      write(*,*) "    *** Target (name)      : ", trim(error_data%target_name)
    end if
    if (error_data%target_id /= 0) then
      write(*,*) "    *** Target (id)        : ", error_data%target_id
    end if
    if (trim(error_data%error_message) /= "") then
      write(*,*) "    *** Error message      : ", trim(error_data%error_message)
    end if
    if (error_data%error_id /= nf90_noerr) then
      write(*,*) "    *** Error id           : ", error_data%error_id
    end if
    write(*,*) "    ***"
    write(*,*) 
  end subroutine etsf_io_low_error_handle
  !!***

  !!****m* etsf_io_low_file_group/etsf_io_low_close
  !! NAME
  !!  etsf_io_low_close
  !!
  !! FUNCTION
  !!  This method is used to close an openend NetCDF file.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with write access.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_close(ncid, lstat, error_data)
    integer, intent(in)                            :: ncid
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_close"
    integer :: s
    
    lstat = .false.
    ! Close file
    s = nf90_close(ncid)
    if (s /= nf90_noerr) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_IO, ERROR_TYPE_CLO, me, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    lstat = .true.
  end subroutine etsf_io_low_close
  !!***

  !!****m* etsf_io_low_file_group/etsf_io_low_set_write_mode
  !! NAME
  !!  etsf_io_low_set_write_mode
  !!
  !! FUNCTION
  !!  This method put the given NetCDF file handler in a data mode, by closing
  !!  a define mode. When a file is opened (see etsf_io_low_open_create() or
  !!  etsf_io_low_open_modify()), the NetCDF file handler is in a define mode.
  !!  This is convienient for all write accesses (create new dimensions, modifying
  !!  attribute values...) ; but when puting values into variables, the handler must
  !!  be in the data mode.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with write access.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_set_write_mode(ncid, lstat, error_data)
    integer, intent(in)                            :: ncid
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_set_write_mode"
    integer :: s
    
    lstat = .false.
    ! Change the mode.
    s = nf90_enddef(ncid)
    if (s /= nf90_noerr .and. s /= -38) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_IO, ERROR_TYPE_END, me, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    lstat = .true.
  end subroutine etsf_io_low_set_write_mode
  !!***

  !!****m* etsf_io_low_file_group/etsf_io_low_set_define_mode
  !! NAME
  !!  etsf_io_low_set_define_mode
  !!
  !! FUNCTION
  !!  This method put the given NetCDF file handler in a define mode, by closing
  !!  a data mode. When opening a file (create or modify), this is the default mode.
  !!  Use etsf_io_low_set_write_mode() to switch then to data mode to write
  !!  variable values. But to set attributes, the file must be in define mode
  !!  again. This method is then usefull.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * ncid = a NetCDF handler, opened with write access.
  !!
  !! OUTPUT
  !!  * lstat = .true. if operation succeed.
  !!  * error_data <type(etsf_io_low_error)> = (optional) location to store error data.
  !!
  !! SOURCE
  subroutine etsf_io_low_set_define_mode(ncid, lstat, error_data)
    integer, intent(in)                            :: ncid
    logical, intent(out)                           :: lstat
    type(etsf_io_low_error), intent(out), optional :: error_data

    !Local
    character(len = *), parameter :: me = "etsf_io_low_set_define_mode"
    integer :: s
    
    lstat = .false.
    ! Change the mode.
    s = nf90_redef(ncid)
    if (s /= nf90_noerr .and. s /= -39) then
      if (present(error_data)) then
        call etsf_io_low_error_set(error_data, ERROR_MODE_IO, ERROR_TYPE_DEF, me, &
                     & errid = s, errmess = nf90_strerror(s))
      end if
      return
    end if
    lstat = .true.
  end subroutine etsf_io_low_set_define_mode
  !!***

  !!****f* etsf_io_low_level/pad
  !! NAME
  !!  pad
  !!
  !! FUNCTION
  !!  Little tool to format chains to constant length (256). This is usefull
  !!  when calling the etsf_io_low_def_var() routine which takes an array of
  !!  strings as argument. Since not all compilers like to construct static
  !!  arrays from strings of different lengths, this function can wrap all
  !!  strings.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! INPUTS
  !!  * string = the string to convert to character(len = 256).
  !!
  !! OUTPUT
  !!
  !! SOURCE
  function pad(string)
    character(len = *), intent(in) :: string
    character(len = 256)           :: pad
    
    write(pad, "(A)") string(1:min(256, len(string)))
  end function pad
  !!***

  !!****m* etsf_io_low_level/strip
  !! NAME
  !!  strip
  !!
  !! FUNCTION
  !!  Little tool to change all final '\0' (end of string in C) characters to
  !!  ' ' (space).
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! SIDE EFFECTS
  !!  * string = the string to convert. It is done in-place.
  !!
  !! SOURCE
  subroutine strip(string)
    character(len = *), intent(inout) :: string

    integer :: i, l

    i = index(string, char(0))
    if (i > 0) then
       l = len(string)
       string(i:l) = repeat(" ", l - i + 1)
    end if
  end subroutine strip
  !!***

  !!****m* etsf_io_low_var_infos/etsf_io_low_free_all_var_infos
  !! NAME
  !!  etsf_io_low_free_all_var_infos
  !!
  !! FUNCTION
  !!  This method is used to free all associated memory in an array of
  !!  #etsf_io_low_var_infos elements. The array is also deallocated.
  !!  This routine is convenient after a call to etsf_io_low_read_all_var_infos()
  !!  with the optional argument @with_dim_name set to true.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! SIDE EFFECTS
  !!  * var_infos_array <type(etsf_io_low_var_infos)> = a pointer on an associated
  !!    array to be deallocated.
  !!
  !! SOURCE
  subroutine etsf_io_low_free_all_var_infos(var_infos_array)
    type(etsf_io_low_var_infos), pointer :: var_infos_array(:)
    
    integer :: i

    if (associated(var_infos_array)) then
       do i = 1, size(var_infos_array), 1
          call etsf_io_low_free_var_infos(var_infos_array(i))
       end do
       deallocate(var_infos_array)
    end if
  end subroutine etsf_io_low_free_all_var_infos
  !!***

  !!****m* etsf_io_low_var_infos/etsf_io_low_free_var_infos
  !! NAME
  !!  etsf_io_low_free_var_infos
  !!
  !! FUNCTION
  !!  This method free all internal allocated memory of a given #etsf_io_low_var_infos
  !!  object after use.
  !!
  !! COPYRIGHT
  !!  Copyright (C) 2006, 2007 (Damien Caliste)
  !!  This file is distributed under the terms of the
  !!  GNU Lesser General Public License, see the COPYING file
  !!  or http://www.gnu.org/copyleft/lesser.txt .
  !!
  !! SIDE EFFECTS
  !!  * var_infos <type(etsf_io_low_var_infos)> = the type object to be freed.
  !!
  !! SOURCE
  subroutine etsf_io_low_free_var_infos(var_infos)
    type(etsf_io_low_var_infos), intent(inout) :: var_infos
    
    if (associated(var_infos%ncdimnames)) then
      deallocate(var_infos%ncdimnames)
    end if
    var_infos%ncdimnames => null()
    if (associated(var_infos%ncattnames)) then
      deallocate(var_infos%ncattnames)
    end if
    var_infos%ncattnames => null()
  end subroutine etsf_io_low_free_var_infos
  !!***

  include "read_routines.f90"
  include "read_routines_auto.f90"

  include "write_routines.f90"
  include "write_routines_auto.f90"

  function var_integer_associated(array)
    type(etsf_io_low_var_integer), intent(in) :: array
    logical                                   :: var_integer_associated
    
    var_integer_associated = (associated(array%data1D) .or. &
                            & associated(array%data2D) .or. &
                            & associated(array%data3D) .or. &
                            & associated(array%data4D) .or. &
                            & associated(array%data5D) .or. &
                            & associated(array%data6D) .or. &
                            & associated(array%data7D))
  end function var_integer_associated

  function var_double_associated(array)
    type(etsf_io_low_var_double), intent(in) :: array
    logical                                  :: var_double_associated
    
    var_double_associated = (associated(array%data1D) .or. &
                           & associated(array%data2D) .or. &
                           & associated(array%data3D) .or. &
                           & associated(array%data4D) .or. &
                           & associated(array%data5D) .or. &
                           & associated(array%data6D) .or. &
                           & associated(array%data7D))
  end function var_double_associated

  subroutine var_integer_multiply(array, factor)
    type(etsf_io_low_var_integer), intent(inout) :: array
    integer                                      :: factor
    
    if (associated(array%data1D)) array%data1D = array%data1D * factor
    if (associated(array%data2D)) array%data2D = array%data2D * factor
    if (associated(array%data3D)) array%data3D = array%data3D * factor
    if (associated(array%data4D)) array%data4D = array%data4D * factor
    if (associated(array%data5D)) array%data5D = array%data5D * factor
    if (associated(array%data6D)) array%data6D = array%data6D * factor
    if (associated(array%data7D)) array%data7D = array%data7D * factor
  end subroutine var_integer_multiply

  subroutine var_double_multiply(array, factor)
    type(etsf_io_low_var_double), intent(inout) :: array
    double precision                            :: factor
    
    if (associated(array%data1D)) array%data1D = array%data1D * factor
    if (associated(array%data2D)) array%data2D = array%data2D * factor
    if (associated(array%data3D)) array%data3D = array%data3D * factor
    if (associated(array%data4D)) array%data4D = array%data4D * factor
    if (associated(array%data5D)) array%data5D = array%data5D * factor
    if (associated(array%data6D)) array%data6D = array%data6D * factor
    if (associated(array%data7D)) array%data7D = array%data7D * factor
  end subroutine var_double_multiply

end module etsf_io_low_level
