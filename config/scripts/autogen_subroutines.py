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

call etsf_io_dims_def(ncid, dims, lstat, error_data)
if (.not. lstat) return
"""
 # Check the groups argument.
 ret += """
! Define groups
"""

 # Write the select case for the argument groups.
 ret += code_data_select("def")
 
 # Close the NetCDF file.
 ret += """
! End definitions and close file
call etsf_io_low_close(ncid, lstat, error_data = error_data)"""

 return ret



# Code for data contents
# Read a NetCDF file for main and one or several group.
# WARNING: this definition is not used yet.
def code_data_contents():

 # Open the file for reading and get the dimensions
 ret = """! Open file for reading
call etsf_io_low_open_read(ncid, trim(filename), lstat, error_data = error_data)
if (.not. lstat) return

! Get Dimensions
call etsf_io_dims_get(ncid, dims, lstat, error_data)
if (.not. lstat) return

"""

 # Looking in the file for elements of groups.
 # Do that by looking for the first variable of each group
 # in the file. Then 'groups' contains all flags for present groups.
 ret += "! Get group names\ngroups = 0\n"

 for grp in etsf_group_list:
  if ( grp != "main" ):
   ret += """
call etsf_io_low_read_var_infos(ncid, "%s", var_infos, lstat)
if ( lstat ) groups = groups + etsf_grp_%s
""" % (etsf_groups[grp][0],grp)

 # Look for the main variable
 ret += "\n! Get main variable name\nmain_var = 0\n"

 for var in etsf_main_names.keys():
  ret += """
if ( main_var == 0 ) then
  call etsf_io_low_read_var_infos(ncid, "%s", var_infos, lstat)
  if ( lstat ) main_var = etsf_main_%s
end if
""" % (var,etsf_main_names[var])

 ret += """
! Close file
call etsf_io_low_close(ncid, lstat, error_data = error_data)"""

 return ret
# WARNING: this definition is not used yet.



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
     buf = "call etsf_io_main_def(ncid, mains, lstat, error_data)\n"
    else:
     buf  = "call etsf_io_%s_def(ncid, lstat, error_data, &\n" % group
     buf += "                     & k_dependent = my_k_dependent)\n"
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
 


# Code for dimensions
def code_dims(action):

 ret = ""
 for dim in etsf_dimensions:
  if ( ret != "" ):
   ret += "\n"

  if ( action == "def" ):
    ret += "call etsf_io_low_write_dim(ncid, \"%s\", &\n" % dim \
         + "                         & dims%%%s, &\n" % dim \
         + "                         & lstat, error_data = error_data)\n" \
         + "if (.not. lstat) return\n"
  elif ( action == "get" ):
    ret += "call etsf_io_low_read_dim(ncid, \"%s\", &\n" % dim \
         + "                        & dims%%%s, &\n" % dim \
         + "                        & lstat, error_data = error_data)\n" \
         + "if (.not. lstat) return\n"

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



# Generic code for a group
def code_group_generic(group,action):

 # Look for peculiarities
 if ( group in etsf_properties ):
  specs = etsf_properties[group]
 else:
  specs = ETSF_PROP_NONE

 # Store code for possible attributes.
 ret_att = ""
 # Store main code
 ret = ""
 
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
 if (action == "def"):
   ret += "! Get values for optional arguments, set default.\n"
   ret += "if (present(k_dependent)) then\n"
   ret += "  my_k_dependent = k_dependent\n"
   ret += "else\n"
   ret += "  my_k_dependent = .true.\n"
   ret += "end if\n"

 ret += "allocate(varid(%d))\n" % len(etsf_groups[group])
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
  
  if ( action == "def" ):
    # Create the definition of the shape and dimensions
    if ( len(var_desc) > 1 ):
      dims = "pad(\"" + var_desc[1] + "\")"
      for dim in var_desc[2:]:
        dims = "pad(\""+ dim + "\"), &\n  &    " + dims
      dims = "(/ " + dims + " /),"
      # Treat the special case of reduced_coordinates_of_plane_waves
      if (var == "reduced_coordinates_of_plane_waves"):
        dims_kdep = "pad(\"" + var_desc[2] + "\")"
        for dim in var_desc[3:]:
          dims_kdep = "pad(\""+ dim + "\"), &\n    &    " + dims_kdep
        dims_kdep = "(/ " + dims_kdep + " /),"
    else:
      dims = None

    # Handle the defintion
    buf = "call etsf_io_low_def_var(ncid, \"%s\", &\n" % var \
        + "  & %s, &\n" % nf90_type(var_desc)
    if (dims is not None):
      buf += "  & %s &\n" % dims
    buf += "  & lstat, ncvarid = varid(%d), error_data = error_data)\n" % ivar
    # The case of reduced_coordinates_of_plane_waves is special since
    # the number of dimension vary.
    if (var == "reduced_coordinates_of_plane_waves"):
      buf_kdep  = "if (.not. my_k_dependent) then\n" 
      buf_kdep += "  call etsf_io_low_def_var(ncid, \"%s\", &\n" % var \
                + "    & %s, &\n" % nf90_type(var_desc)
      buf_kdep += "    & %s &\n" % dims_kdep
      buf_kdep += "    & lstat, ncvarid = varid(%d), error_data = error_data)\n" % ivar
      buf_kdep += "else\n"
      buf_kdep += indent_code(buf, 1)
      buf_kdep += "end if\n"
      buf = buf_kdep
    buf += "if (.not. lstat) return\n"
    # Handle attributes of the variable
    if (att_units):
      buf += code_attribute_units("write", "\"atomic units\"", "1.0d0", ivar)
    if (att_kdep or var == "reduced_coordinates_of_plane_waves"):
      buf += code_attribute_kdep("write", "varid(%d)" % ivar)
    if (att_symm):
      buf += code_attribute_symm("write", ivar, "\"yes\"")

    # Print the variable definition        
    if (group == "main"):
      ret += "if (iand(mains, etsf_main_%s) /= 0) then\n" % var_shortname(var)
      ret += indent_code(buf, 1)
      ret += "end if\n"
    else:
      ret += buf

  else:
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
        spl += "  allocate(start(%d), count(%d))\n" % (len(var_desc) - 2, len(var_desc) - 2)
        spl += "else\n"
        spl += "  allocate(start(%d), count(%d))\n" % (len(var_desc) - 1, len(var_desc) - 1)
        spl += "end if\n"
      else:
        spl  = "allocate(start(%d), count(%d))\n" % (len(var_desc) - 1, len(var_desc) - 1)
      spl += "start(:) = 1\n"
      spl += "count(:) = 0\n"
      # For each max_something dimension, use the provided value.
      i = len(var_desc) - 1
      for dim in var_desc[1:]:
        if (dim.startswith("max_")):
          spl += "count(%d) = folder%%%s__%s\n" % (i, var_shortname(var), dim[4:])
        if (dim == "number_of_spins"):
          spl += "if (folder%%%s__spin_access /= etsf_no_sub_access) then\n" % var_shortname(var)
          spl += "  start(%d) = folder%%%s__spin_access\n" % (i, var_shortname(var))
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
                             ivar, var, splitted, unformatted), 1)
      ret_att += "end if\n\n"
    if (att_symm and action == "put"):
      ret_att += "if (associated(folder%reduced_symmetry_translations)) then\n"
      ret_att += indent_code(code_attribute_symm("test", ivar_sym_matrices), 1)
      ret_att += "end if\n\n"

 # Add attribute code if some
 if (ret_att is not ""):
   ret += "\n! Handle all attributes for the group.\n"
   if (action == "put"):
     ret += "call etsf_io_low_set_define_mode(ncid, lstat, error_data = error_data)\n"
     ret += "if (.not. lstat) return\n"
   ret += ret_att
 ret += "\ndeallocate(varid)"
 return ret

# Code for the unit attribute, testing its value
# and the scale_to_atomic_units attribute.
def code_attribute_units(action, att_unit, att_scale, ivar, \
                         var = None, var_split = False, var_unform = False):
  ret = "! Handle the units attribute.\n"
  ret += code_attributes(action, "varid(%d)" % ivar, "units", \
                         att_unit, "etsf_charlen")
  ret += "if (.not. lstat) return\n"
  # If unit exists, we test its value in the reading case.
  if (action == "read"):
    ret += "if (trim(%s) /= \"atomic units\") then\n" % att_unit
    ret += indent_code(code_attributes("read", "varid(%d)" % ivar, \
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
    ret += code_attributes("write", "varid(%d)" % ivar, \
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
  ret += code_attributes(action, "varid(%d)" % ivar, "symmorphic", att_value, "etsf_charlen")
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
 ret = re.sub("@INFO@",info,ret)
 ret = re.sub("@SCRIPT@",script,ret)

 arg_list = ""
 arg_move = len(name)+19
 arg_stop = 0

 # Process arguments
 if ( args != None ):
  for arg_str in args:
   arg_info = arg_str.split()

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
   
   # Build the documentation for in, out, inout
   inout = None
   if (arg_info[2] == "in"):
    inout = 0
   elif (arg_info[2] == "out"):
    inout = 1
   elif (arg_info[2] == "inout"):
    inout = 2
   arg_doc[inout] += "!! * %s" % arg
   if (arg_info[1].startswith("type")):
     arg_doc[inout] += " <%s>" % arg_info[1]
   arg_doc[inout] += " = "
   if ( (len(arg_info) > 3) and (arg_info[3] == "optional") ):
     arg_doc[inout] += "(optional) "
   key = arg + "-" + name
   if (key in etsf_subs_doc_args):
     arg_doc[inout] += re.sub("\n", "\n!!", etsf_subs_doc_args[key])
   else:
     key = arg + "-*"
     if (key in etsf_subs_doc_args):
       arg_doc[inout] += re.sub("\n", "\n!!", etsf_subs_doc_args[key])
   arg_doc[inout] += "\n"

   # Optional arguments
   if ( (len(arg_info) > 3) and (arg_info[3] == "optional") ):
    opt = ", optional"
   else:
    opt = ""

   # The fortran definition
   arg_desc += "  %s%s, intent(%s) :: %s\n" % (arg_info[1],opt,arg_info[2],arg)
   if ( (len(arg_info) > 3) and 
        ((arg_info[3] == "optional") or (arg_info[3] == "local")) ):
    loc_vars += "  %s :: my_%s\n" % (arg_info[1],arg)

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
