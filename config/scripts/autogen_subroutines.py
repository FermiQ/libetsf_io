#!/usr/bin/env python
#
# Copyright (c) 2006
# All rights reserved.
#
# This file is part of the ETSF_IO software package. For license information,
# please see the COPYING file in the top-level directory of the library source
# distribution.
#

from time import gmtime,strftime

import commands
import os
import re
import sys

# ---------------------------------------------------------------------------- #

#
# Subprograms
#

# Forwarder for grouped data access
def code_data(action):

 return eval("code_data_%s()" % (action))



# Code for grouped data initialization
def code_data_init():

 # Handle optional variables
 ret  = "! Get values for optional arguments, set default.\n"
 ret += "if (present(k_dependent)) then\n"
 ret += "  my_k_dependent = k_dependent\n"
 ret += "else\n"
 ret += "  my_k_dependent = .true.\n"
 ret += "end if\n"
 ret += "if (present(overwrite)) then\n"
 ret += "  my_overwrite = overwrite\n"
 ret += "else\n"
 ret += "  my_overwrite = .false.\n"
 ret += "end if\n"
 ret += "if (present(split_definition)) then\n"
 ret += "  my_split_definition = split_definition\n"
 ret += "end if\n"

 # Create a New NetCDF file and the constant dimensions values
 # and save all the dimensions to the file.
 ret += """
! Create the NetCDF file
call etsf_io_low_open_create(ncid, filename, etsf_file_format_version, lstat, &
                           & title = trim(title), history = trim(history), &
                           & error_data = error_data, overwrite = my_overwrite)
if (.not. lstat) return

! Define dimensions
dims%character_string_length        = etsf_charlen
dims%number_of_cartesian_directions = etsf_3dimlen
dims%number_of_reduced_dimensions   = etsf_3dimlen
dims%number_of_vectors              = etsf_3dimlen
dims%symbol_length                  = etsf_chemlen

! We set the size of split arrays, if required.
if (present(split_definition)) then
  call etsf_io_split_init(dims, split_definition)
end if

! We write the dimensions to the file.
call etsf_io_dims_def(ncid, dims, lstat, error_data)
if (.not. lstat) return

"""

 ret += "! Define split arrays.\n"
 # Call the def subroutine for possible splitted variables.
 ret += "if (present(split_definition)) then\n"
 ret += "  call etsf_io_split_def(ncid, dims, lstat, error_data)\n"
 ret += "  if (.not. lstat) return\n"
 ret += "end if\n"
 # Write the select case for the argument groups.
 ret += "! Define groups.\n"
 ret += code_data_select("def")
 
 # Close the NetCDF file.
 ret += """
! Write the split arrays.
if (present(split_definition)) then
  ! Begin by putting the file in write mode.
  call etsf_io_low_set_write_mode(ncid, lstat, error_data = error_data)
  if (.not. lstat) return
  ! Write the arrays.
  call etsf_io_split_put(ncid, split_definition, lstat, error_data)
  if (.not. lstat) return
end if

! End definitions and close file
call etsf_io_low_close(ncid, lstat, error_data = error_data)
if (.not. lstat) return"""

 return ret

# The copy code for data (all groups).
# It call the copy routine for each group.
def code_data_copy():
  ret = "lstat = .false.\n\n"

  # Open the files for reading and writing.
  ret += "! Open file for writing\n"
  ret += "call etsf_io_low_open_modify(ncid_to, trim(dest_file), &\n"
  ret += "  & lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) return\n"
  ret += "! Open file for reading\n"
  ret += "call etsf_io_low_open_read(ncid, trim(source_file), &\n"
  ret += "  & lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) return\n\n"
  
  # We copy the attributes
  ret += "! We copy all the global attributes (ETSF and non-ETSF).\n"
  ret += "call etsf_io_low_copy_all_att(ncid, ncid_to, etsf_io_low_global_att, etsf_io_low_global_att, &\n"
  ret += "                            & lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) return\n\n"

  ret += "! We switch to write mode.\n"
  ret += "call etsf_io_low_set_write_mode(ncid_to, lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) return\n"
  for grp in etsf_groups:
    ret += "if (present(split)) then\n"
    ret += "  call etsf_io_%s_copy(ncid_to, ncid, dims, &\n" % grp
    ret += "    & lstat, error_data, split)\n"
    ret += "else\n"
    ret += "  call etsf_io_%s_copy(ncid_to, ncid, dims, &\n" % grp
    ret += "    & lstat, error_data)\n"
    ret += "end if\n"
    ret += "if (.not. lstat) return\n\n"

  ret += "!Close files.\n"
  ret += "call etsf_io_low_close(ncid_to, lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) return\n"
  ret += "call etsf_io_low_close(ncid, lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) return\n"
  
  return ret

# Code for data contents
# Read a NetCDF file for main and one or several group.
def code_data_contents():
  ret = "lstat = .false.\n"
  ret += "if (present(vars_infos)) then\n"
  ret += "  vars_infos%n_vars = 0\n"
  ret += "  vars_infos%parent => null()\n"
  ret += "  with_dim_name = .true.\n"
  ret += "else\n"
  ret += "  with_dim_name = .false.\n"
  ret += "end if\n"
  ret += "\n"
  
  # Open the file for reading.
  ret += "! Open file for reading\n"
  ret += "call etsf_io_low_open_read(ncid, trim(filename), &\n"
  ret += "  & lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) return\n"

  # We read the dimensions.
  ret += "! Get the dimensions.\n"
  ret += "call etsf_io_dims_get(ncid, dims, lstat, error_data)\n"
  ret += "if (.not. lstat) return\n"

  # We handle the split definition, if required.
  ret += "! We allocate the split arrays.\n"
  ret += "call etsf_io_split_allocate(split, dims)\n"
  ret += "! We read the split informations.\n"
  ret += "call etsf_io_split_get(ncid, split, lstat, error_data)\n"
  ret += "if (.not. lstat) then\n"
  ret += "  call etsf_io_split_free(split)\n"
  ret += "  return\n"
  ret += "end if\n"

  # List all variables of the file.
  ret += "! Get all variables definitions.\n"
  ret += "! It will allocate my_vars_infos%parent array.\n"
  ret += "call etsf_io_low_read_all_var_infos(ncid, my_vars_infos%parent, &\n"
  ret += "  & lstat, error_data, with_dim_name = with_dim_name)\n"
  ret += "if (.not. lstat) then\n"
  ret += "  call etsf_io_split_free(split)\n"
  ret += "  return\n"
  ret += "end if\n\n"

  # Get for all variable its group and main id.
  ret += "etsf_group = etsf_grp_none\n"
  ret += "etsf_main  = etsf_main_none\n"
  ret += "if (associated(my_vars_infos%parent)) then\n"
  ret += "  my_vars_infos%n_vars = size(my_vars_infos%parent)\n"
  ret += "  if (present(vars_infos)) then\n"
  ret += "    ! Allocate vars_infos arrays for future use.\n"
  ret += "    vars_infos%n_vars = my_vars_infos%n_vars\n"
  ret += "    allocate(vars_infos%group(vars_infos%n_vars))\n"
  ret += "    allocate(vars_infos%varid(vars_infos%n_vars))\n"
  ret += "    allocate(vars_infos%split(vars_infos%n_vars))\n"
  ret += "  end if\n"
  ret += "  ! get the main_id and the group_id for all variables.\n"
  ret += "  do i = 1, my_vars_infos%n_vars, 1\n"
  ret += "    call etsf_io_data_get(group_id, var_id, &\n"
  ret += "      & split_id, my_vars_infos%parent(i)%name)\n"
  ret += "    etsf_group = ior(etsf_group, group_id)\n"
  ret += "    etsf_main  = ior(etsf_main, var_id)\n"
  ret += "    if (present(vars_infos)) then\n"
  ret += "      ! Update vars_infos arrays.\n"
  ret += "      vars_infos%group(i) = group_id\n"
  ret += "      vars_infos%varid(i) = var_id\n"
  ret += "      vars_infos%split(i) = split_id\n"
  ret += "    end if\n"
  ret += "  end do\n"
  ret += "end if\n"
  ret += "if (present(vars_infos)) then\n"
  ret += "  ! Associate vars_infos%parent to the one computed in my_vars_infos.\n"
  ret += "  vars_infos%parent => my_vars_infos%parent\n"
  ret += "else if (associated(my_vars_infos%parent)) then\n"
  ret += "  call etsf_io_low_free_all_var_infos(my_vars_infos%parent)\n"
  ret += "end if\n\n"

  ret += "! Close file\n"
  ret += "call etsf_io_low_close(ncid, lstat, error_data = error_data)\n"

  return ret



# Code for grouped data reading
# This code is put in etsf_io_data_read.
# It read a main group and as much as other group as specified.
def code_data_read():

  # Handling of optional variables in get mode
  ret  = "! Get values for optional arguments, set default.\n"
  ret += "if (present(use_atomic_units)) then\n"
  ret += "  my_use_atomic_units = use_atomic_units\n"
  ret += "else\n"
  ret += "  my_use_atomic_units = .true.\n"
  ret += "end if\n"

  # Open a NetCDF file for reading.
  ret += """
! Open file for reading
call etsf_io_low_open_read(ncid, trim(filename), lstat, error_data = error_data)
if (.not. lstat) return

! Get Data"""

  # Read the other groups.
  ret += code_data_select("get")

  # Close the file.
  ret += """

! Close file
call etsf_io_low_close(ncid, lstat, error_data = error_data)"""

  return ret



# Code for grouped data selection
# Write a loop on groups to call the
# def/put/get routine for this group (depending on @action).
# In case of read/write action, the corresponding group in
# group_folder must be associated.
def code_data_select(action):

 # Consistency check.
 ret = """
if (groups < 0 .or. groups >= 2 ** etsf_ngroups) then
  call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_ARG, my_name, &
                           & tgtname = "groups", errmess = "value out of bounds")
  lstat = .false.
  return
end if

"""

 ret += "do i = 1, etsf_ngroups\n"
 ret += "  select case ( iand(groups, 2 ** (i - 1)) )\n"

 for group in etsf_group_list:
   ret += "    case (etsf_grp_%s)\n" % group
   if ( action == "def" ):
    if (group == "main"):
     buf = "call etsf_io_main_def(ncid, mains, lstat, error_data, &\n" \
         + "                     & split = my_split_definition)\n"
    else:
     buf  = "call etsf_io_%s_def(ncid, lstat, error_data, &\n" % group
     buf += "                     & k_dependent = my_k_dependent, &\n" \
         +  "                     & split = my_split_definition)\n"
    buf += "if (.not. lstat) return\n"
   else:
    # Check the association
    buf = "if (.not. associated(group_folder%%%s)) then\n" % group
    buf += "  call etsf_io_low_error_set(error_data, ERROR_MODE_%s, ERROR_TYPE_VAR, &\n" % action.upper()
    buf += "                           & my_name, tgtname = \"%s\", &\n" % group
    buf += "                           & errmess = \"No data type associated\")\n"
    buf += "  lstat = .false.\n"
    buf += "  return\n"
    buf += "end if\n"
    # Call the action routine
    if (action == "get"):
     buf += "call etsf_io_%s_get(ncid, group_folder%%%s, lstat, error_data, &\n" % (group, group)
     buf += "                       & use_atomic_units = my_use_atomic_units)\n"
    else:
     buf += "call etsf_io_%s_put(ncid, group_folder%%%s, lstat, error_data)\n" % (group, group)
    buf += "if (.not. lstat) return\n"
   ret += indent_code(buf,3)
 ret += "  end select\n"
 ret += "end do\n"

 return ret



# Code for grouped data writing
# This code is put in etsf_io_data_write.
# It writes a main group and as much as other group as specified.
def code_data_write():

 # Open a NetCDF file for writing.
 ret = """! Open file for writing
call etsf_io_low_open_modify(ncid, trim(filename), lstat, error_data = error_data)
if (.not. lstat) return

! We switch to write mode.
call etsf_io_low_set_write_mode(ncid, lstat, error_data = error_data)
if (.not. lstat) return

! Put Data
"""

 # Write the other groups.
 ret += code_data_select("put")

 # Close the file.
 ret += """

! Close file
call etsf_io_low_close(ncid, lstat, error_data = error_data)"""

 return ret

# Give the group associated to a variable name.
def code_data_get():
  ret = ""
  ret += "etsf_group = etsf_grp_none\n"
  ret += "etsf_main  = etsf_main_none\n"
  ret += "etsf_split = .false.\n"
  fmt_else = ""
  for grp in etsf_groups:
    for var in etsf_groups[grp]:
      ret += "%sif (trim(varname) == \"%s\") then\n" % (fmt_else, var)
      ret += "  etsf_group = etsf_grp_%s\n" % grp
      if (grp == "main"):
        ret += "  etsf_main = etsf_main_%s\n" % etsf_variables_shortnames[var]
      if (fmt_else == ""):
        fmt_else = "else "
  # Add the split variables
  for var in etsf_variables:
    if var.startswith("my_"):
      ret += "else if (trim(varname) == \"%s\") then\n" % var
      ret += "  etsf_split = .true.\n"
  ret += "end if\n"
  return ret

# Code for split
def code_vars(action):
  ret  = ""
  ret += "! Deallocate all associated pointers.\n"
  ret += "if (associated(vars_infos%parent)) then\n"
  ret += "  call etsf_io_low_free_all_var_infos(vars_infos%parent)\n"
  ret += "end if\n"
  ret += "if (associated(vars_infos%group)) then\n"
  ret += "  deallocate(vars_infos%group)\n"
  ret += "end if\n"
  ret += "if (associated(vars_infos%varid)) then\n"
  ret += "  deallocate(vars_infos%varid)\n"
  ret += "end if\n"
  ret += "if (associated(vars_infos%split)) then\n"
  ret += "  deallocate(vars_infos%split)\n"
  ret += "end if\n"
  return ret


# Code for dimensions
def code_dims(action):
 ret = ""
 my_dims = []
 for dim in etsf_dimensions:
   props = 0
   if (dim in etsf_properties):
     props = etsf_properties[dim]
   
   if ( props & ETSF_PROP_DIM_SPLIT != 0):
     my_dims.append(limit_length("my_" + dim))
 all_dims = etsf_dimensions + my_dims
 if (action == "merge"):
   ret += "lstat = .false.\n"

 for dim in all_dims:
  if ( ret != "" ):
   ret += "\n"

  if ( action == "def" ):
    if (dim.startswith("my_")):
      ret += "if (dims%%%s /= etsf_no_dimension .and. &\n" % dim \
             +  "    dims%%%s /= dims%%%s) then\n" % (dim, expand_length(dim)[3:])
    else:
      ret += "if (dims%%%s /= etsf_no_dimension) then\n" % dim
    ret += "  call etsf_io_low_write_dim(ncid, \"%s\", &\n" % expand_length(dim) \
        +  "                           & dims%%%s, &\n" % dim \
        +  "                           & lstat, error_data = error_data)\n" \
        +  "  if (.not. lstat) return\n" \
        +  "end if\n"
  elif ( action == "get" ):
    ret += "call etsf_io_low_read_dim(ncid, \"%s\", &\n" % expand_length(dim) \
         + "                        & dims%%%s, &\n" % dim \
         + "                        & lstat, error_data = error_data)\n" \
         + "if (.not. lstat) then \n" \
         + "  if (error_data%access_mode_id == ERROR_MODE_INQ) then\n"
    if (dim.startswith("my_")):
      ret += "    dims%%%s = dims%%%s\n" % (dim, expand_length(dim)[3:])
    else:
      ret += "    dims%%%s = etsf_no_dimension\n" % dim
    ret += "  else\n" \
         + "    return\n" \
         + "  end if\n" \
         + "end if\n"
  elif ( action == "trace" ):
    ret += "write(*, \"(A42,A,I6)\") \"%s\", &\n" % dim
    ret += "  & \": \", dims%%%s" % dim
  elif ( action == "merge" ):
    if (dim.startswith("my_")):
      ret += "if (output_dims%%%s /= output_dims%%%s) then\n" % (dim, expand_length(dim)[3:])
      ret += "  output_dims%%%s = output_dims%%%s + &\n" % (dim, dim)
      ret += "    & dims%%%s\n" % dim
      ret += "end if\n"
    else:
      ret += "if (output_dims%%%s /= dims%%%s) then\n" % (dim, dim)
      ret += "  call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, &\n"
      ret += "    & ERROR_TYPE_DIM, my_name, &\n"
      ret += "    & errmess = \"incompatible dimension for merge.\")\n"
      ret += "  return\n"
      ret += "end if\n"

 if (action != "trace"):
   ret += "lstat = .true.\n"
 return ret

# Code for split
def code_split(action):
  ret = ""
  if (action == "def"):
    # We define the descriptive arrays of splitted variables
    for var in etsf_variables:
      if (var.startswith("my_")):
        var_desc = etsf_variables[var]
        ret += "if (dims%%%s /= etsf_no_dimension .and. &\n" % limit_length(var_desc[1])
        ret += "  & dims%%%s /= dims%%%s) then \n" % (limit_length(var_desc[1]), var_desc[1][3:])
        # Create the definition of the shape and dimensions
        if ( len(var_desc) > 1 ):
          dims = "pad(\"" + var_desc[1] + "\")"
          for dim in var_desc[2:]:
            dims = "pad(\""+ dim + "\"), &\n  &    " + dims
          dims = "(/ " + dims + " /),"
        else:
          dims = None

        # Handle the defintion
        buf = "call etsf_io_low_def_var(ncid, \"%s\", &\n" % var \
            + "  & %s, &\n" % nf90_type(var_desc)
        if (dims is not None):
          buf += "  & %s &\n" % dims
        buf += "  & lstat, error_data = error_data)\n"
        buf += "if (.not. lstat) return\n"
        ret += indent_code(buf, 1)
        ret += "end if\n"
  elif (action == "init"):
    # We set the dimension of my_something in dims from
    # the size of the associated array of split_definition
    for var in etsf_variables:
      if (var.startswith("my_")):
        ret += "if (associated(split_definition%%%s)) then\n" % var
        ret += "  dims%%%s = &\n" % limit_length(etsf_variables[var][1])
        ret += "    & size(split_definition%%%s)\n" % var
        ret += "else\n"
        ret += "  dims%%%s = &\n" % limit_length(etsf_variables[var][1])
        ret += "    & dims%%%s\n" % etsf_variables[var][1][3:]
        ret += "end if\n"
  elif (action == "put" or action == "get"):
    if (action == "put"):
      action_str = "write"
    else:
      action_str = "read"
    for var in etsf_variables:
      if (var.startswith("my_")):
        ret += "if (associated(split%%%s)) then\n" % var
        ret += "  call etsf_io_low_%s_var(ncid, \"%s\", &\n" % (action_str, var) \
            +  "                          & split%%%s, &\n" % var \
            +  "                          & lstat, error_data = error_data)\n"
        ret += "  if (.not. lstat) return\n"
        ret += "end if\n"
  elif (action == "allocate"):
    # Reading the dims argument, we allocate the etsf_split argument.
    for var in etsf_variables:
      if (var.startswith("my_")):
        var_desc = etsf_variables[var]
        ret += "if (dims%%%s /= etsf_no_dimension .and. &\n" % limit_length(var_desc[1])
        ret += "  & dims%%%s /= dims%%%s) then \n" % (limit_length(var_desc[1]), var_desc[1][3:])
        ret += "  allocate(split%%%s(dims%%%s))\n" % (var, limit_length(var_desc[1]))
        ret += "  split%%%s(:) = -1\n" % var
        ret += "end if\n"
  elif (action == "free"):
    for var in etsf_variables:
      if (var.startswith("my_")):
        var_desc = etsf_variables[var]
        ret += "if (associated(split%%%s)) then\n" % var
        ret += "  deallocate(split%%%s)\n" % var
        ret += "end if\n"
  elif (action == "copy"):
    for var in etsf_variables:
      if (var.startswith("my_")):
        var_desc = etsf_variables[var]
        ret += "if (dims%%%s /= etsf_no_dimension .and. &\n" % limit_length(var_desc[1])
        ret += "  & dims%%%s /= dims%%%s) then \n" % (limit_length(var_desc[1]), var_desc[1][3:])
        ret += "  allocate(split_array(dims%%%s))\n" % (limit_length(var_desc[1]))
        ret += "  call etsf_io_low_read_var(ncid_from, \"%s\", &\n" % var
        ret += "                          & split_array, lstat, error_data = error_data)\n"
        ret += "  if (.not. lstat) then\n"
        ret += "    deallocate(split_array)\n"
        ret += "    return\n"
        ret += "  end if\n"
        ret += "  call etsf_io_low_write_var(ncid_to, \"%s\", &\n" % var
        ret += "                           & split_array, lstat, error_data = error_data)\n"
        ret += "  if (.not. lstat) then\n"
        ret += "    deallocate(split_array)\n"
        ret += "    return\n"
        ret += "  end if\n"
        ret += "  deallocate(split_array)\n"
        ret += "end if\n"
  elif (action == "merge"):
    ret += "lstat = .false.\n"
    for var in etsf_variables:
      if (var.startswith("my_")):
        var_desc = etsf_variables[var]
        ret += "if (associated(output_split%%%s)) then\n" % var
        ret += "  if (.not. associated(split%%%s)) then\n" % var
        ret += "    call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, &\n"
        ret += "      & ERROR_TYPE_DIM, my_name, &\n"
        ret += "      & errmess = \"incompatible split for merge (not allocated).\")\n"
        ret += "    return\n"
        ret += "  end if\n"
        ret += "  do ivar = 1, size(output_split%%%s), 1\n" % var
        ret += "    if (output_split%%%s(ivar) < 0) then\n" % var
        ret += "      exit\n"
        ret += "    end if\n"
        ret += "  end do\n"
        ret += "  if ((ivar + size(split%%%s) - 1) > size(output_split%%%s)) then\n" % \
               (var, var)
        ret += "    call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, &\n"
        ret += "      & ERROR_TYPE_DIM, my_name, &\n"
        ret += "      & errmess = \"incompatible split for merge (wrong length).\")\n"
        ret += "    return\n"
        ret += "  end if\n"
        ret += "  output_split%%%s(ivar:ivar + size(split%%%s) - 1) = &\n" % (var, var)
        ret += "    & split%%%s\n" % var
        ret += "  ! We modify the split value to be used in accordance with\n"
        ret += "  ! the new output_split\n"
        ret += "  split%%%s = &\n" % var
        ret += "    & (/ (len, len = ivar, ivar + size(split%%%s) - 1, 1) /)\n" % var
        ret += "end if\n"
    ret += "lstat = .true.\n"
  
  return ret

# Code for global attributes
# WARNING! this definition is obsolete since
# all global attributes are handled by the low level part.
def code_globals(action):
  ret = ""

  for att in etsf_attributes.keys():
    att_desc = etsf_attributes[att]

    if ( att in etsf_properties ):
      att_specs = etsf_properties[att]
    else:
      att_specs = ETSF_PROP_NONE

    if ( (att_specs & ETSF_PROP_ATT_GLOBAL) != 0 ):
      # val is the name of the variable to read/write the value
      if ( (action == "get") or (len(att_desc) < 2) ):
        val = att.lower()
      else:
        val = "etsf_%s" % (att.lower())
      # att_len is the length of the variable, when needed
      if ( att_desc[0].startswith("string") ):
        att_len = etsf_constants[att_desc[0].split()[1]]
      else:
        att_len = ""

      if ( ret != "" ):
        ret += "\n"

      if ( action == "put" ):
        ret += code_attributes("write", "etsf_io_low_global_att", att, val, "")
        ret += "if (.not. lstat) return\n"
      elif ( action == "get" ):
        ret += code_attributes("read", "etsf_io_low_global_att", att, val, att_len)
        ret += "if (.not. lstat) return\n"

  return ret
# WARNING! this definition is obsolete

# Code for def action on a group
def code_group_def(group):

 # Store main code
 ret = ""
 # Store the set of dimensions used by this group
 # A boolean is associated to each name. If True,
 # then the dimension is splitted.
 set_of_dims = {}
 
 # Consistency check
 if (group == "main"):
  ret += """! Consistency checks.
if (mains < 0 .or. mains >= 2 ** etsf_main_nvars) then
  call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_ARG, my_name, &
                           & tgtname = "mains", errmess = "value out of bounds")
  lstat = .false.
  return
end if

"""

 # Handling of different optional variables
 ret += "! Get values for optional arguments, set default.\n"
 ret += "if (present(k_dependent)) then\n"
 ret += "  my_k_dependent = k_dependent\n"
 ret += "else\n"
 ret += "  my_k_dependent = .true.\n"
 ret += "end if\n"
 for var in etsf_groups[group]:
   var_desc = etsf_variables[var]
   if (len(var_desc) > 1):
     for dim in var_desc[1:]:
       set_of_dims[dim] = False
 buf = ""
 for var in etsf_variables:
   var_desc = etsf_variables[var]
   if (var.startswith("my_") and var_desc[1][3:] in set_of_dims):
     set_of_dims[var_desc[1][3:]] = True
     buf += "if (associated(split%%%s)) then\n" % var
     buf += "  write(split_dims%%%s, \"(A)\") &\n" % var_desc[1][3:]
     buf += "    & \"%s\"\n" % var_desc[1]
     buf += "end if\n"
 if (buf != ""):
   ret += "! Set the name for dimensions that could be splitted.\n"
   ret += "if (present(split)) then\n"
   ret += indent_code(buf, 1)
   ret += "end if\n"

 # Process each variable in the group
 ivar = 0
 ivar_sym_matrices = None
 for var in etsf_groups[group]:
  var_desc = etsf_variables[var]
  ivar += 1
  if (var == "reduced_symmetry_matrices"):
    ivar_sym_matrices = ivar

  if ( ret != "" ):
   ret += "\n"

  # put in char_len, the length of the string.
  # This information is required by the low level routines.
  if ( var_desc[0].startswith("string")):
    char_len = etsf_constants[var_desc[-1]] + ", "
  else:
    char_len = ""

  # Retrieve variable properties of interest.
  unformatted = False
  splitted    = False
  att_units   = False
  att_kdep    = False
  att_symm    = False
  if (var in etsf_properties):
      props = etsf_properties[var]
      unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED != 0)
      splitted    = ( props & ETSF_PROP_VAR_SUB_ACCESS != 0)
      att_symm    = ( props & ETSF_PROP_VAR_SYMMORPHIC != 0)
      att_units   = ( props & ETSF_PROP_VAR_UNITS != 0)
      att_kdep    = ( props & ETSF_PROP_VAR_KDEP != 0)
      
  # Create the definition of the shape and dimensions
  if ( len(var_desc) > 1 ):
      if (set_of_dims[var_desc[1]]):
          dims = "split_dims%%%s" % var_desc[1]
      else:
          dims = "pad(\"" + var_desc[1] + "\")"
      for dim in var_desc[2:]:
          if (set_of_dims[dim]):
              dims = "split_dims%%%s, &\n  &    " % dim + dims
          else:
              dims = "pad(\""+ dim + "\"), &\n  &    " + dims
      dims = "(/ " + dims + " /),"
      # Treat the special case of reduced_coordinates_of_plane_waves
      if (var == "reduced_coordinates_of_plane_waves"):
          if (set_of_dims[var_desc[2]]):
              dims_kdep = "split_dims%%%s" % var_desc[2]
          else:
              dims_kdep = "pad(\"" + var_desc[2] + "\")"
          for dim in var_desc[3:]:
              if (set_of_dims[dim]):
                  dims_kdep = "split_dims%%%s, &\n  &    " % dim + dims
              else:
                  dims_kdep = "pad(\""+ dim + "\"), &\n    &    " + dims_kdep
          dims_kdep = "(/ " + dims_kdep + " /),"
  else:
      dims = None

  # Handle the defintion
  buf = "call etsf_io_low_def_var(ncid, \"%s\", &\n" % var \
        + "  & %s, &\n" % nf90_type(var_desc)
  if (dims is not None):
      buf += "  & %s &\n" % dims
  buf += "  & lstat, ncvarid = ivar, error_data = error_data)\n"
  # The case of reduced_coordinates_of_plane_waves is special since
  # the number of dimension vary.
  if (var == "reduced_coordinates_of_plane_waves"):
      buf_kdep  = "if (.not. my_k_dependent) then\n" 
      buf_kdep += "  call etsf_io_low_def_var(ncid, \"%s\", &\n" % var \
                  + "    & %s, &\n" % nf90_type(var_desc)
      buf_kdep += "    & %s &\n" % dims_kdep
      buf_kdep += "    & lstat, ncvarid = ivar, error_data = error_data)\n"
      buf_kdep += "else\n"
      buf_kdep += indent_code(buf, 1)
      buf_kdep += "end if\n"
      buf = buf_kdep
  buf += "! We raise don't raise an error if a dimension is missing.\n"
  buf += "if (.not. lstat .and. (error_data%access_mode_id /= ERROR_MODE_INQ .or. &\n"
  buf += "  & error_data%target_type_id /= ERROR_TYPE_DID)) return\n"
  # Handle attributes of the variable
  if (att_units or att_kdep or var == "reduced_coordinates_of_plane_waves" or att_symm):
    buf += "if (ivar >= 0) then\n"
    if (att_units):
      buf += indent_code(code_attribute_units("write", "\"atomic units\"", "1.0d0", "ivar"), 1)
    if (att_kdep or var == "reduced_coordinates_of_plane_waves"):
      buf += indent_code(code_attribute_kdep("write", "ivar"), 1)
    if (att_symm):
      buf += indent_code(code_attribute_symm("write", "ivar", "\"yes\""), 1)
    buf += "end if\n"

  # Print the variable definition        
  if (group == "main"):
    ret += "if (iand(mains, etsf_main_%s) /= 0) then\n" % var_shortname(var)
    ret += indent_code(buf, 1)
    ret += "end if\n"
  else:
    ret += buf

 ret += "! If we reach the end, then it should be OK.\n"
 ret += "lstat = .true.\n"
 return ret

def code_split_write(dims, var, var_fortran, var_splitted, var_unformatted, string_len, var_span):
  ret = ""
  if (dims == []):
    if (not(var_splitted)):
      ret += "call etsf_io_low_write_var(ncid_to, \"%s\", &\n" % var
      ret += "                         & %s%s, lstat, &\n" % (var_fortran, string_len)
      ret += "                         & error_data = error_data, ncvarid = varids(2, nvarids))\n"
    else:
      ret += "call etsf_io_low_write_var(ncid_to, \"%s\", &\n" % var
      ret += "                         & %s(%s)%s, &\n" % \
             (var_fortran, var_span, string_len)
      ret += "                         & lstat, error_data = error_data, &\n"
      ret += "                         & start = start, count = count, ncvarid = varids(2, nvarids))\n"
    ret += "if (.not. lstat) then\n"
    ret += "  deallocate(%s)\n" % var_fortran
    if (var_splitted):
      ret += "  deallocate(start, count, istop)\n"
      if (not(var_unformatted)):
        ret += "  deallocate(jstart, jend)\n"
    ret += "  deallocate(varids)\n"
    ret += "  return\n"
    ret += "end if\n"
    if (var_splitted and var_unformatted):
      ret += "istart = istart + len\n"
    return ret

  if dims[0].startswith("my_"):
    # The dimension is potentialy splitted.
    # We get the name of the array used to index the
    # splitted values.
    dim_array = dim_get_split_array(dims[0])
    # If the dim_array is allocated, then we use it.
    ret += "do idim%d = 1, istop(%d), 1\n" % (len(dims), len(dims))
    ret += "  if (associated(split%%%s)) then\n" % dim_array
    ret += "    start(%d)  = split%%%s(idim%d)\n" % \
           (len(dims), dim_array, len(dims))
    if (not(var_unformatted)):
      ret += "    jstart(%d) = split%%%s(idim%d)\n" % \
             (len(dims), dim_array, len(dims))
      ret += "    jend(%d)   = split%%%s(idim%d)\n" % \
             (len(dims), dim_array, len(dims))
    ret += "  end if\n"
    ret += indent_code(code_split_write(dims[1:], var, var_fortran, \
                                        var_splitted, var_unformatted, \
                                        string_len, var_span), 1)
    ret += "end do\n"
  else:
    ret = code_split_write(dims[1:], var, var_fortran, var_splitted, \
                           var_unformatted, string_len, var_span)

  return ret
  
# Code to copy the values of a group from one file to another
# The routine has several arguments:
#  - ncid_to: the id of the NetCDF to write the data to ;
#  - ncid_from: the id of the NetCDF to read data from ;
#  - dims_from: the description of the dimensions used in the from file ;
#  - lstat: .false. if an error occured ;
#  - error_data: the error descriptor ;
#  - split: optional, if set, the data are writen in a full array
#           at the right places as defined in the split structure.
def code_group_copy(group):
  # Store main code
  ret =  "lstat = .false.\n"
  ret += "call etsf_io_low_set_write_mode(ncid_to, lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) return\n\n"
  ret += "allocate(varids(2,%d))\n" % len(etsf_groups[group])
  ret += "nvarids = 1\n\n"

  # Process each variable in the group
  for var in etsf_groups[group]:
    # We prepend a my_ prefix to all dimensions when they are subject to splitting.
    var_desc = etsf_variables[var][:]
    for dim in var_desc[1:]:
      if dim in etsf_properties:
        if (etsf_properties[dim] & ETSF_PROP_DIM_SPLIT != 0):
          var_desc[var_desc.index(dim)] = "my_" + dim

    # Retrieve variable properties of interest.
    unformatted = False
    att_units   = False
    att_kdep    = False
    att_symm    = False
    if (var in etsf_properties):
      props = etsf_properties[var]
      unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED != 0)
      att_symm    = ( props & ETSF_PROP_VAR_SYMMORPHIC != 0)
      att_units   = ( props & ETSF_PROP_VAR_UNITS != 0)
      att_kdep    = ( props & ETSF_PROP_VAR_KDEP != 0)
    
    # Allocate an array for the reading action
    ret += "! Variable '%s'\n" % var
    ret += "!  allocate and read data\n"
    var_fortran = "folder%%%s" % var
    if (var_desc[0].startswith("string")):
      string_len = ", dims%%%s" % var_desc[-1]
    else:
      string_len = ""
    if ((var_desc[0].startswith("string") and len(var_desc) < 3) or \
        (not(var_desc[0].startswith("string")) and len(var_desc) < 2)):
      # Case when the variable is a scalar
      var_splitted = False
      length = ""
    else:
      var_splitted = var_get_split_status(var)
      if (var_desc[0].startswith("string")):
        stop = -2
      else:
        stop = -1
      # If the variable is unformatted, we use data1D,
      # else, we use the right shape.
      length = "( &\n"
      # We compute the total length of data
      for dim in var_desc[1:stop]:
        if (unformatted):
          length += "  & dims%%%s * &\n" % limit_length(dim)
        else:
          length += "  & dims%%%s, &\n" % limit_length(dim)
      length += "  & dims%%%s)" % limit_length(var_desc[stop])
      if (unformatted):
        var_fortran = "folder%%%s%%data1D" % var
    ret += "allocate(%s%s)\n" % (var_fortran, length)
    ret += "call etsf_io_low_read_var(ncid_from, \"%s\", &\n" % var
    ret += "                        & %s%s, lstat, &\n" % (var_fortran, string_len)
    ret += "                        & error_data = error_data, ncvarid = varids(1, nvarids))\n"
    # Raise error only if the error is not an inquire one.
    ret += "if (.not. lstat .and. error_data%access_mode_id /= ERROR_MODE_INQ) then\n"
    ret += "  deallocate(%s)\n" % var_fortran
    ret += "  deallocate(varids)\n"
    ret += "  return\n"
    ret += "end if\n"
    ret += "!  write data and deallocate (if read succeed)\n"
    ret += "if (lstat) then\n"
    if (not(var_splitted)):
      ret += indent_code(code_split_write([], var, var_fortran, \
                                          False, False,string_len, ""), 1)
    else:
      ret += "  if (present(split)) then\n"
      ret += "    ! We use the split definition to write to appropriated locations.\n"
      ret += "    allocate(start(%d), count(%d))\n" % \
             (len(var_desc) + stop, len(var_desc) + stop)
      ret += "    count(:) = 0\n"
      ret += "    start(:) = 1\n"
      ret += "    ! For each dimension, set the do loop boundaries,\n"
      ret += "    ! and the array boundaries.\n"
      ret += "    allocate(istop(%d))\n" % (len(var_desc) + stop)
      if (unformatted):
        # If unformatted, the array boundaries are just istart and istart + len - 1
        ret += "    istart   = 1\n"
        ret += "    len      = 1\n"
      else:
        # If not, we store the array boundaries into jstart and jend arrays.
        ret += "    allocate(jstart(%d), jend(%d))\n" % \
               (len(var_desc) + stop, len(var_desc) + stop)
      # For each dimensions, we compute the indexes for reading,
      # istop is use as stop value for the do loops
      #  (either 1 or size(split%dim_array)) ;
      # jstart is used to restrict acces for each dimension
      #  when the array is unformatted (start value) ;
      # jend is identical to jstart but for end.
      var_span = ""
      for dim in var_desc[1:]:
        if (dim.startswith("my_")):
          dim_id = len(var_desc) - var_desc.index(dim)
          ret += "    if (.not. associated(split%%%s)) then\n" % \
                 dim_get_split_array(dim)
          ret += "      istop(%d)  = 1\n" % dim_id
          if (unformatted):
            ret += "      len = len * dims%%%s\n" % limit_length(dim)
          else:
            var_span = "jstart(%d):jend(%d), " % (dim_id, dim_id) + var_span
            ret += "      jstart(%d) = 1\n" % dim_id
            ret += "      jend(%d)   = dims%%%s\n" % (dim_id, dim)
          ret += "    else\n"
          ret += "      istop(%d) = size(split%%%s)\n" % \
                 (dim_id, dim_get_split_array(dim))
          ret += "      count(%d) = 1\n" % dim_id
          ret += "    end if\n"
        else:
          if (unformatted):
            ret += "    len = len * dims%%%s\n" % dim
          else:
            var_span = ":, " + var_span
      if (unformatted):
        var_span = "istart:istart + len - 1"
      else:
        if (var_span.endswith(", ")):
          var_span = var_span[:-2]
      # Treat the split possibility
      # For each possible splitted dimension, make a for loop on write action
      ret += indent_code(code_split_write(var_desc[1:], var, var_fortran, \
                                          True, unformatted, string_len, var_span), 2)
      ret += "    deallocate(start, count, istop)\n"
      if (not(unformatted)):
        ret += "    deallocate(jstart, jend)\n"
      ret += "  else\n"
      ret += "    ! No split information, we copy everything in the same shape.\n"
      ret += indent_code(code_split_write([], var, var_fortran, False, False, string_len, ""), 2)
      ret += "  end if\n"
    ret += "  nvarids = nvarids + 1\n"
    ret += "end if\n"
    ret += "deallocate(%s)\n" % var_fortran
    ret += "\n"
    ret += "lstat = .true.\n"

  # We copy the attributes
  ret += "\n! We copy all the attributes (ETSF and non-ETSF) of the group variables.\n"
  ret += "call etsf_io_low_set_define_mode(ncid_to, lstat, error_data = error_data)\n"
  ret += "if (.not. lstat) nvarids = 0\n"
  ret += "do len = 1, nvarids - 1, 1\n"
  ret += "  call etsf_io_low_copy_all_att(ncid_from, ncid_to, varids(1, len), varids(2, len), &\n"
  ret += "                              & lstat, error_data = error_data)\n"
  ret += "  if (.not. lstat) exit\n"
  ret += "end do\n"
  ret += "deallocate(varids)"
  return ret

# Generic code for a group
def code_group_generic(group,action):

  if (action == "def"):
    return code_group_def(group)
  elif (action == "copy"):
    return code_group_copy(group)

  # Store code for possible attributes.
  ret_att = ""
  # Store main code
  ret = ""
  # Store the set of dimensions used by this group
  # A boolean is associated to each name. If True,
  # then the dimension is splitted.
  set_of_dims = {}

  # Consistency check in def mode
  if (group == "main" and action == "def"):
    ret += """! Consistency checks.
if (mains < 0 .or. mains >= 2 ** etsf_main_nvars) then
  call etsf_io_low_error_set(error_data, ERROR_MODE_DEF, ERROR_TYPE_ARG, my_name, &
                           & tgtname = "mains", errmess = "value out of bounds")
  lstat = .false.
  return
end if

"""

  # Handling of different optional variables
  if (action == "get"):
    ret += "! Get values for optional arguments, set default.\n"
    ret += "if (present(use_atomic_units)) then\n"
    ret += "  my_use_atomic_units = use_atomic_units\n"
    ret += "else\n"
    ret += "  my_use_atomic_units = .true.\n"
    ret += "end if\n"

  ret += "\nallocate(varid(%d))\n" % len(etsf_groups[group])
  if (action == "put"):
    ret += "! Begin by putting the file in write mode.\n"
    ret += "call etsf_io_low_set_write_mode(ncid, lstat, error_data = error_data)\n"
    ret += "if (.not. lstat) return\n"

  # Process each variable in the group
  ivar = 0
  ivar_sym_matrices = None
  for var in etsf_groups[group]:
    var_desc = etsf_variables[var]
    ivar += 1
    if (var == "reduced_symmetry_matrices"):
      ivar_sym_matrices = ivar

    if ( ret != "" ):
      ret += "\n"

    # put in char_len, the length of the string.
    # This information is required by the low level routines.
    if ( var_desc[0].startswith("string")):
      char_len = etsf_constants[var_desc[-1]] + ", "
    else:
      char_len = ""

    # Retrieve variable properties of interest.
    unformatted = False
    splitted    = False
    att_units   = False
    att_kdep    = False
    att_symm    = False
    if (var in etsf_properties):
      props = etsf_properties[var]
      unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
      splitted    = ( props & ETSF_PROP_VAR_SUB_ACCESS == ETSF_PROP_VAR_SUB_ACCESS)
      att_symm    = ( props & ETSF_PROP_VAR_SYMMORPHIC == ETSF_PROP_VAR_SYMMORPHIC)
      att_units   = ( props & ETSF_PROP_VAR_UNITS == ETSF_PROP_VAR_UNITS)
      att_kdep    = ( props & ETSF_PROP_VAR_KDEP == ETSF_PROP_VAR_KDEP)

    if ( action == "put" ):
      action_str = "write"
    elif ( action == "get" ):
      action_str = "read"
    else:
      raise ValueError

    # Variable are read or write, only if associated.
    if (unformatted or splitted):
      code_if = "if (etsf_io_low_var_associated(folder%%%s)) then\n" % var
    else:
      code_if = "if (associated(folder%%%s)) then\n" % var
    ret += code_if

    # Special treatment for reduced_coordinates_of_plane_waves
    if (var == "reduced_coordinates_of_plane_waves"):
      ret += indent_code(code_attribute_kdep("read", "\"%s\"" % var, "flag"), 1)

    # If variable may be splitted, we build a block optional argument
    if (splitted):
      # Special treatment for reduced_coordinates_of_plane_waves
      if (var == "reduced_coordinates_of_plane_waves"):
        spl  = "if (flag(1:2) == \"no\") then\n"
        spl += "  allocate(start(%d), count(%d))\n" % \
               (len(var_desc) - 2, len(var_desc) - 2)
        spl += "else\n"
        spl += "  allocate(start(%d), count(%d))\n" % \
               (len(var_desc) - 1, len(var_desc) - 1)
        spl += "end if\n"
      else:
        spl  = "allocate(start(%d), count(%d))\n" % \
               (len(var_desc) - 1, len(var_desc) - 1)
      spl += "start(:) = 1\n"
      spl += "count(:) = 0\n"
      # For each max_something dimension, use the provided value.
      i = len(var_desc) - 1
      for dim in var_desc[1:]:
        if (dim.startswith("max_")):
          spl += "count(%d) = folder%%%s__%s\n" % (i, var_shortname(var), dim[4:])
        if (dim == "number_of_spins"):
          spl += "if (folder%%%s__spin_access /= etsf_no_sub_access) then\n" % \
                 var_shortname(var)
          spl += "  start(%d) = folder%%%s__spin_access\n" % \
                 (i, var_shortname(var))
          spl += "  count(%d) = 1\n" % (i)
          spl += "end if\n"
        if (dim == "number_of_kpoints"):
          # Special treatment for reduced_coordinates_of_plane_waves
          if (var == "reduced_coordinates_of_plane_waves"):
            spl += "if (flag(1:3) == \"yes\" .and. &\n"
            spl += "  & folder%%%s__kpoint_access /= etsf_no_sub_access) then\n" % var_shortname(var)
          else:
            spl += "if (folder%%%s__kpoint_access /= etsf_no_sub_access) then\n" % var_shortname(var)
          spl += "  start(%d) = folder%%%s__kpoint_access\n" % (i, var_shortname(var))
          spl += "  count(%d) = 1\n" % (i)
          spl += "end if\n"
        i -= 1
        sub_arg = ", start = start, count = count"
      ret += indent_code(spl, 1)
    else:
      sub_arg = ""

    # Treat the k_dependent attribute
    if (action == "get" and att_kdep):
      ret += indent_code(code_attribute_kdep("read", "\"%s\"" % var, "flag"), 1)
      ret += "  if (flag(1:2) == \"no\") then\n"
      ret += "    call etsf_io_low_read_dim(ncid, \"%s\", len, &\n" % etsf_kdep_fallback[var]
      ret += "                            & lstat, error_data = error_data)\n"
      ret += "    folder%%%s = len\n" % var
      ret += "  else\n"

    # If variable may be splitted, we append the start/count optional arguments
    buf =  "  call etsf_io_low_%s_var(ncid, \"%s\", &\n" % (action_str, var) \
        +  "                          & folder%%%s, %s&\n" % (var, char_len) \
        +  "                          & lstat, ncvarid = varid(%d), &\n" % ivar \
        +  "                          & error_data = error_data%s)\n" % sub_arg \

    # Treat the k_dependent attribute
    if (action == "get" and att_kdep):
      ret += indent_code(buf, 1)
      ret += "  end if\n"
    else:
      ret += buf

    # If variable may be splitted
    if (splitted):
      ret += "  deallocate(start, count)\n"
    ret += "  if (.not. lstat) return\n"

    # End the if associated
    ret += "end if\n"

    # Handle attributes of the variable
    if (att_units and action == "get"):
      ret_att += code_if
      ret_att += indent_code(code_attribute_units(action_str, \
                             "folder%%%s__units" % var_shortname(var), \
                             "folder%%%s__scale_to_atomic_units" % var_shortname(var), \
                             "varid(%d)" % ivar, var, splitted, unformatted), 1)
      ret_att += "end if\n\n"
    if (att_symm and action == "put"):
      ret_att += "if (associated(folder%reduced_symmetry_translations)) then\n"
      ret_att += indent_code(code_attribute_symm("test", "varid(%d)" % ivar_sym_matrices), 1)
      ret_att += "end if\n\n"

  # Add attribute code if some
  if (ret_att is not ""):
    ret += "\n! Handle all attributes for the group.\n"
    if (action == "put"):
      ret += "call etsf_io_low_set_define_mode(ncid, lstat, error_data = error_data)\n"
      ret += "if (.not. lstat) return\n"
    ret += ret_att
  if (action != "def"):
    ret += "\ndeallocate(varid)"
      
  return ret

# Code for the unit attribute, testing its value
# and the scale_to_atomic_units attribute.
def code_attribute_units(action, att_unit, att_scale, ivar, \
                         var = None, var_split = False, var_unform = False):
  ret = "! Handle the units attribute.\n"
  ret += code_attributes(action, ivar, "units", \
                         att_unit, "etsf_charlen")
  ret += "if (.not. lstat) return\n"
  # If unit exists, we test its value in the reading case.
  if (action == "read"):
    ret += "if (trim(%s) /= \"atomic units\") then\n" % att_unit
    ret += indent_code(code_attributes("read", ivar, \
                       "scale_to_atomic_units", att_scale, ""), 1)
    ret += "  if (.not. lstat) return\n"
    ret += "else\n"
    ret += "  %s = 1.0d0\n" % att_scale
    ret += "end if\n"
    ret += "if (my_use_atomic_units .and. &\n"
    ret += "  & %s /= 1.0d0) then\n" % att_scale
    if (var_unform or var_split):
      ret += "  call etsf_io_low_var_multiply(folder%%%s, &\n" % var
      ret += "                              & %s)\n" % att_scale
    else:
      ret += "  folder%%%s = folder%%%s * &\n" % (var, var)
      ret += "    & %s\n" % att_scale
    ret += "end if\n"
  else:
    ret += code_attributes("write", ivar, \
                           "scale_to_atomic_units", att_scale, "")
    ret += "if (.not. lstat) return\n"
    
  return ret
  
# Code for the k_dependent attribute.
def code_attribute_kdep(action, ivar, att_value = None):
  ret  = "! Handle the k_dependent attribute.\n"
  if (action == "write"):
    ret += "if (my_k_dependent) then\n"
    ret += indent_code(code_attributes(action, ivar, "k_dependent", "\"yes\"", "etsf_charlen"), 1)
    ret += "else\n"
    ret += indent_code(code_attributes(action, ivar, "k_dependent", "\"no\"", "etsf_charlen"), 1)
    ret += "end if\n"
  else:
    ret += code_attributes(action, ivar, "k_dependent", att_value, "etsf_charlen")
  ret += "if (.not. lstat) return\n"
    
  return ret

# Code for the symmorphic attribute.
def code_attribute_symm(action, ivar, att_value = None):
  ret  = "! Handle the symmorphic attribute.\n"
  if (action == "test"):
    ret += "! We test if translations are not nul\n"
    ret += "flag = \"yes\"\n"
    ret += "do len = 1, size(folder%reduced_symmetry_translations, 2), 1\n"
    ret += "  if (folder%reduced_symmetry_translations(1, len) /= 0.d0 .or. &\n"
    ret += "    & folder%reduced_symmetry_translations(2, len) /= 0.d0 .or. &\n"
    ret += "    & folder%reduced_symmetry_translations(3, len) /= 0.d0) then\n"
    ret += "  flag = \"no\"\n"
    ret += "  end if\n"
    ret += "end do\n"
    att_value = "trim(flag)"
    action = "write"
  ret += code_attributes(action, ivar, "symmorphic", att_value, "etsf_charlen")
  ret += "if (.not. lstat) return\n"
  return ret

# Code for read/write attributes.
def code_attributes(action, varid, attname, attvalue, attlen):
  if (action == "read" and attlen is not ""):
    lenarg = attlen + ", "
  else:
    lenarg = ""
  ret = "call etsf_io_low_%s_att(ncid, %s, &\n" % (action, varid) \
      + "                        & \"%s\", &\n" % attname \
      + "                        & %s%s, &\n" % (lenarg, attvalue) \
      + "                        & lstat, error_data = error_data)\n"
  return ret
    

# Transfer data to and from an optional argument
# WARNING! This definition is not used yet.
def code_optional_argument(group,action):

 ret = "if ( present(%s) ) then\n" % (group)

 if ( action == "get" ):
  ret += " call etsf_dims_get(ncid,dims)\n\n"

  for var in etsf_groups[group]:
   var_desc = etsf_variables[var]
   if ( len(var_desc) > 1 ):

    dim_list = ""
    dim_move = 32+len(group)+len(var)
    dim_stop = 0

   for dim_info in var_desc[1:]:
    if ( dim_list != "" ):
     if ( (len(dim_list)+dim_move)/72 > dim_stop ):
      dim_list += ", &\n  & "
      dim_stop += 1
     else:
      dim_list += ","
    dim_list += "dims%"+dim_info

    ret += " allocate(my_%s%%%s(%s))\n" % (group,var,dim_list)
  ret += " call etsf_%s_get(ncid,my_%s)\n" % (group,group)

 if ( action == "def" ):
  ret += " call etsf_%s_%s(ncid,my_%s)\n" % (group,action,group)

 for var in etsf_groups[group]:
  var_dims = len(etsf_variables[var])-1
  if ( action == "put" ):
   if ( var_dims > 0 ):
    ret += " my_%s%%%s => %s%%%s\n" % (group,var,group,var)
   else:
    ret += " my_%s%%%s = %s%%%s\n" % (group,var,group,var)
  elif ( action == "get" ):
    ret += " %s%%%s = my_%s%%%s\n" % (group,var,group,var)

 if ( action == "put" ):
  ret += " call etsf_put_%s(ncid,my_%s)\n" % (group,group)
 elif ( action == "get" ):
  for var in etsf_groups[group]:
   var_desc = etsf_variables[var]
   if ( len(var_desc) > 1 ):
    ret += "\n deallocate(my_%s%%%s)" % (group,var)

 ret += "\nendif"

 return ret
# WARNING! This definition is not used yet.



# Return a list of optional arguments for all groups
def group_args(action):

 if ( (action == "read") or (action == "get") ):
  intent = "out"
 elif ( (action == "write") or (action == "put") ):
  intent = "in"
 else:
  intent = action

 ret = []
 ret.append("main_var integer in")
 ret.append("groups integer in")
 ret.append("main type(etsf_main) %s" % (intent))
 for grp in etsf_group_list:
  if ( grp in etsf_properties ):
   grp_specs = etsf_properties[grp]
  else:
   grp_specs = ETSF_PROP_NONE

  if ( grp != "main" ):
   ret.append("%s type(etsf_%s) %s optional" % (grp,grp,intent))

 return ret



# Indent a piece of code
def indent_code(code,offset):

 tmp = ""
 for i in range(offset):
  tmp += "  "

 if (code.endswith('\n')):
  return tmp+re.sub("\n","\n"+tmp,code[:-1])+'\n'
 else:
  return tmp+re.sub("\n","\n"+tmp,code)


# Initialize a routine from a template
def init_routine(name,template,info,script,args,type="subroutine"):

 # Init
 ret = file("config/etsf/template.%s" % (template),"r").read()

 if ( type == "subroutine" ):
  ret = re.sub("@SUBPROGRAM@",type,ret)
 else:
  ret = re.sub("@SUBPROGRAM@","function",ret)
  ret = re.sub("@FUNCTYPE@",type,ret)

 ret = re.sub("@NAME@",name,ret)
 ret = re.sub("@SCRIPT@",script,ret)

 # Build the documentation for FUNCTION (replacing @INFO@)
 if (name in etsf_subs_doc_desc):
   ret = re.sub("\n@INFO@", re.sub("\n", "\n!!", etsf_subs_doc_desc[name]), ret)
 else:
   # Try to match with a joker
   key = "*_" + name[name.find("_") + 1:]
   if (key in etsf_subs_doc_desc):
     ret = re.sub("\n@INFO@", re.sub("\n", "\n!!", etsf_subs_doc_desc[key]), ret)
   else:
     ret = re.sub("@INFO@", info, ret)

 arg_list = ""
 arg_move = len(name)+19
 arg_stop = 0

 # Process arguments
 if ( args != None ):
  for arg_str in args:
   arg_info = arg_str.split()
   if ( arg_info[-1] != "local" ):
     if ( arg_list != "" ):
      if ( (len(arg_list)+arg_move)/72 > arg_stop ):
       arg_list += ", &\n  & "
       arg_stop += 1
      else:
       arg_list += ", "
     arg_list += arg_info[0]
  ret = re.sub("@ARG_LIST@",arg_list,ret)

  arg_desc = ""
  loc_vars = ""
  arg_doc = ["", "", ""]
  for arg_str in args:
   arg_info = arg_str.split()
   arg = arg_info[0]
   intent_id = -1
   if (arg_info[-1] == "optional"):
     intent_id = -2
   
   # Build the documentation for in, out, inout
   inout = None
   if (arg_info[intent_id] == "in"):
    inout = 0
   elif (arg_info[intent_id] == "out"):
    inout = 1
   elif (arg_info[intent_id] == "inout"):
    inout = 2
   if (inout is not None):
     arg_doc[inout] += "!! * %s" % arg
     if (arg_info[1].startswith("type")):
       arg_doc[inout] += " <%s>" % " ".join(arg_info[1:intent_id])
     arg_doc[inout] += " = "
     if ( intent_id == -2 ):
       arg_doc[inout] += "(optional) "
     key = arg + "-" + name
     if (key in etsf_subs_doc_args):
       arg_doc[inout] += re.sub("\n", "\n!!", etsf_subs_doc_args[key])
     else:
       key = arg + "-*_" + name.split("_", 2)[1]
       if (key in etsf_subs_doc_args):
         arg_doc[inout] += re.sub("\n", "\n!!", etsf_subs_doc_args[key])
       else:
         key = arg + "-*"
         if (key in etsf_subs_doc_args):
           arg_doc[inout] += re.sub("\n", "\n!!", etsf_subs_doc_args[key])
     arg_doc[inout] += "\n"

   # Optional arguments
   if ( intent_id == -2 ):
    opt = ", optional"
   else:
    opt = ""

   # The fortran definition
   fortran_def = " ".join(arg_info[1:intent_id])
   if (inout is not None):
     if (fortran_def.find("pointer") < 0):
       # The intent keyword is not allowed by several compilers
       # with the keyword pointer.
       arg_desc += "  %s%s, intent(%s) :: %s\n" % (fortran_def,opt,arg_info[intent_id],arg)
     else:
       arg_desc += "  %s%s :: %s\n" % (fortran_def,opt,arg)
     if ( (intent_id == -2) or (arg_info[-1] == "local") ):
       loc_vars += "  %s :: my_%s\n" % (fortran_def,arg)
   else:
     loc_vars += "  %s :: %s\n" % (fortran_def,arg)

   # Build the documentation string
   arg_doc_str = ""
   if (arg_doc[0] != ""):
     arg_doc_str += "!! INPUTS\n" + arg_doc[0]
   if (arg_doc[1] != ""):
     arg_doc_str += "!! OUTPUT\n" + arg_doc[1]
   if (arg_doc[2] != ""):
     arg_doc_str += "!! SIDE EFFECTS\n" + arg_doc[2]

  if ( type != "subroutine" ):
   loc_vars += "  %s :: etsf_%s\n" % (type,name)

  ret = re.sub("@ARG_DESC@\n",arg_desc,ret)
  ret = re.sub("@ARG_DOC@\n",arg_doc_str,ret)
  ret = re.sub("@LOCAL_VARS@",loc_vars,ret)
 else:
  ret = re.sub(", @ARG_LIST@","",ret)
  ret = re.sub("@ARG_DESC@\n","",ret)
  ret = re.sub("@ARG_DOC@\n", "",ret)
  ret = re.sub("@LOCAL_VARS@","",ret)

 return ret



# ---------------------------------------------------------------------------- #

#
# Main program
#

# Initial setup
my_name    = "autogen_subroutines.py"
my_configs = ["config/etsf/specs.cf",
              "config/etsf/library.cf",
              "config/etsf/code.cf",
              "config/etsf/functions.py"]

# Check if we are in the top of the ETSF_IO source tree
if ( not os.path.exists("configure.ac") ):
 print "%s: You must be in the top of the library source tree." % my_name
 print "%s: Aborting now." % my_name
 sys.exit(1)

# Read config file(s)
for cnf in my_configs:
 if ( os.path.exists(cnf) ):
  execfile(cnf)
 else:
  print "%s: Could not find config file (%s)." % (my_name,cnf)
  print "%s: Aborting now." % my_name
  sys.exit(2)

# Create routines
includes = []
for sub in etsf_subprograms.keys():
 dsc = etsf_subprograms[sub]

 # Look for peculiarities
 if ( sub in etsf_subs_properties ):
  specs = etsf_subs_properties[sub]
 else:
  specs = ETSF_SUBS_PROP_NONE

 # Check type
 if ( len(dsc) > 3 ):
  sub_type = dsc[3]
 else:
  sub_type = "subroutine"

 # Write action routines
 for action in dsc[2].split():
  if ( sub == "@GROUP@" ):
   sub_list = etsf_group_list
   sub_code = "group_generic"
   sub_cprm = "sub_name, action"
  else:
   sub_list = [sub]
   sub_code = sub
   sub_cprm = "action"

  for sub_name in sub_list:
   if ( sub_name in etsf_properties ):
    sub_specs = etsf_properties[sub_name]
   else:
    sub_specs = ETSF_PROP_NONE

   # Look for arguments
   try:
    sub_args = eval("etsf_subs_%s_args[sub_name]" % (action))
    if ( sub_args[0] == "@GROUPS@" ):
     sub_args = group_args(action)
   except KeyError:
    sub_args = None

   try:
    if ( sub_args == None ):
     sub_args = eval("etsf_subs_%s_args[sub]" % (action))
     if ( sub_args[0] == "@GROUPS@" ):
      sub_args = group_args(action)
   except KeyError:
    dummy = None

   # Load template
   src = init_routine("%s_%s" % (sub_name,action),dsc[0],dsc[1],my_name,
     sub_args,type=sub_type)

   # Substitute patterns
   src = re.sub("@ACTION_TEXT@",etsf_subs_actions[action].capitalize()+"s",src)
   src = re.sub("@GROUP@",sub_name,src)
   src = re.sub("@GROUP_TYPE@","type(etsf_%s)" % (sub_name),src)
   src = re.sub("@CODE@",
          indent_code(eval("code_%s(%s)" % (sub_code,sub_cprm)),1),src)

   # Write routine
   out = file("%s/etsf_io_%s_%s.f90" % (etsf_file_srcdir,sub_name,action),"w")
   out.write(src)
   out.close()
   includes.append("etsf_io_%s_%s.f90" % (sub_name,action))

# Make list of included files
includes_str = ""
includes_am = ""
for filename in includes:
  includes_str += "  include \"%s\"\n" % filename
  includes_am += "\\\n\t%s" % filename

# Write the includes in the module file.
src = file("%s/etsf_io.f90" % (etsf_file_srcdir), "r").read()
src = re.sub("@INCLUDES@", includes_str, src)
out = file("%s/etsf_io.f90" % (etsf_file_srcdir), "w")
out.write(src)
out.close()

# Create theMakefile.am
src = file("config/etsf/template.Makefile.am", "r").read()
src = re.sub("@INCLUDED_FILES@", includes_am, src)
out = file("%s/Makefile.am" % (etsf_file_srcdir), "w")
out.write(src)
out.close()
