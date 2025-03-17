#!/usr/bin/env python
#
# Copyright (C) 2005-2010 (Damien Caliste)
# All rights reserved.
#
# This file is part of the ETSF_IO software package. For license information,
# please see the COPYING file in the top-level directory of the library source
# distribution.
#

from time import gmtime,strftime

import subprocess
import os
import re
import sys

# ---------------------------------------------------------------------------- #

#
# Subprograms
#


# Indent a piece of code
def indent_code(code,offset):

 tmp = ""
 for i in range(offset):
  tmp += "  "

 return tmp+re.sub("\n","\n"+tmp,code)


def init_grp(grp_name, grp_id):
  # We define the variables using etsf_io_data_init
  ret  = "flags%%%s = etsf_%s_all\n" % (grp_name, grp_name)
  ret += "call etsf_io_data_init(\"test_init_%s.nc\", flags, &\n" % grp_name
  ret += "                     & dims, \"Test\", \"\", lstat, error)\n"
  ret += "call tests_write_status(\"Create file test_init_%s.nc\", lstat, error)\n" % grp_name
  ret += "flags%%%s = etsf_%s_none\n" % (grp_name, grp_name)

  # We open the file for checkings
  ret += "call etsf_io_low_open_read(ncid, \"test_init_%s.nc\", lstat, error_data = error)\n" % grp_name
  ret += "call tests_write_status(\" | opening\", lstat, error)\n"
  
  # We check each variable definition for this group.
  for var in etsf_groups[grp_name]:
    var_dsc = etsf_variables[var]
    
    # Retrieve variable properties of interest.
    att_units   = False
    if (var in etsf_properties):
      props = etsf_properties[var]
      att_units   = ( props & ETSF_PROP_VAR_UNITS == ETSF_PROP_VAR_UNITS)
      
    # Begin with all required dimensions
    for dim in var_dsc[1:]:
      ret += "call etsf_io_low_read_dim(ncid, \"%s\", dimvalue, lstat, error_data = error)\n" % dim
      ret += "call tests_write_status(\" | read dim '%s'\", lstat, error)\n" % dim
      ret += "if (dimvalue /= dims%%%s) then\n" % dim
      ret += "  call etsf_io_low_error_set(error, ERROR_MODE_SPEC, ERROR_TYPE_DIM, me, &\n"
      ret += "                           & tgtname = \"%s\", errmess = \"Wrong value\")\n" % dim
      ret += "  lstat = .false.\n"
      ret += "end if\n"
      ret += "call tests_write_status(\" | check dim value '%s'\", lstat, error)\n" % dim

    # Check the variable definition
    ret += "call etsf_io_low_read_var_infos(ncid, \"%s\", var_infos, lstat, error_data = error)\n" % var
    ret += "call tests_write_status(\" | read var '%s' characteristics\", lstat, error)\n" % var
    ret += "if (var_infos%%nctype /= %s .or. &\n" % nf90_type(var_dsc)
    ret += "  & var_infos%%ncshape /= %d .or. &\n" % (len(var_dsc) - 1)
    index = len(var_dsc) - 1
    for dim in var_dsc[1:]:
      ret += "  & var_infos%%ncdims(%d) /= dims%%%s .or. &\n" % (index, dim)
      index -= 1
    ret += "  & .false. ) then\n"
    ret += "  call etsf_io_low_error_set(error, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &\n"
    ret += "                           & tgtname = \"%s\", errmess = \"Wrong characteristic\")\n" %var
    ret += "  lstat = .false.\n"
    ret += "end if\n"
    ret += "call tests_write_status(\" | check var '%s' characteristics\", lstat, error)\n" % var
    
    # Check the mandatory attributes
    if (att_units):
      ret += "call etsf_io_low_check_att(ncid, var_infos%ncid, \"units\", &\n"
      ret += "                         & etsf_io_low_character, etsf_charlen, &\n"
      ret += "                         & lstat, error_data = error)\n"
      ret += "call tests_write_status(\" | check att 'units'\", lstat, error)\n"
      
  ret += "call etsf_io_low_close(ncid, lstat, error_data = error)\n"
  ret += "call tests_write_status(\" | closing\", lstat, error)\n\n"
  return indent_code(ret, 2)

def output_wrong_statement(action):
  if (action == "copy"):
    ret  = "call etsf_io_data_copy(\"Makefile\", \"pouet\", dims, lstat, error)\n"
    ret += "call tests_status(\"source_file: wrong value (no file)\", (.not. lstat), error)\n"
    ret += "\n"
    ret += "call etsf_io_data_copy(\"Makefile\", \"Makefile\", dims, lstat, error)\n"
    ret += "call tests_status(\"source_file: wrong value (text file)\", (.not. lstat), error)\n"
    ret += "\n"
    ret += "call etsf_io_data_copy(\"pouet\", \"test_write_geometry.nc\", dims, lstat, error)\n"
    ret += "call tests_status(\"dest_file: wrong value (no file)\", (.not. lstat), error)\n"
    ret += "\n"
    ret += "call etsf_io_data_copy(\"Makefile\", \"test_write_geometry.nc\", dims, lstat, error)\n"
    ret += "call tests_status(\"dest_file: wrong value (text file)\", (.not. lstat), error)\n"
    return ret
  else:
    ret  = "call etsf_io_data_%s(\"pouet\", grp, lstat, error)\n" % action
    ret += "call tests_status(\"dest_file: wrong value (no file)\", (.not. lstat), error)\n"
    ret += "\n"
    ret += "call etsf_io_data_%s(\"Makefile\", grp, lstat, error)\n" % action
    ret += "call tests_status(\"dest_file: wrong value (text file)\", (.not. lstat), error)\n"
    return ret
  
def output_init_statement(var):
  # Retrieve variable properties of interest.
  unformatted = False
  splitted = False
  if (var in etsf_properties):
    props = etsf_properties[var]
    unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
    splitted    = ( props & ETSF_PROP_VAR_SUB_ACCESS == ETSF_PROP_VAR_SUB_ACCESS)

  ret = ""
  var_dsc = etsf_variables[var]

  # We compute the total length of data
  if (splitted or unformatted):
    ret += "call tests_init_variable(group%%%s%%data1D, (/ &\n" % var
    for dim in var_dsc[1:-1]:
      ret += "  & dims%%%s * &\n" % dim
    ret += "  & dims%%%s /))\n" % var_dsc[-1]
  else:
    # We allocate with the fixed shape in a reverse order.
    var_dsc_cpy = var_dsc[1:]
    var_dsc_cpy.reverse()
    if (len(var_dsc) == 1):
      ret += "call tests_init_variable(group%%%s)\n" % var
    else:
      ret += "call tests_init_variable(group%%%s, (/ &\n" % var
      for dim in var_dsc_cpy[0:-1]:
        ret += "  & dims%%%s, &\n" % dim
      ret += "  & dims%%%s /))\n" % var_dsc[1]
  return ret

def output_check_statement(var, action):
  ret = ""
  var_dsc = etsf_variables[var]

  if (action == "read"):
    # Retrieve variable properties of interest.
    if (var_dsc[0].startswith("string")):
      if (len(var_dsc) == 2):
        ret += "call tests_check_values(group%%%s, &\n" % var
        ret += "                      & dims%%%s, \"%s\", &\n" % (var_dsc[1], var)
        ret += "                      & lstat, error_data)\n"
      else:
        ret += "call tests_check_values(group%%%s, &\n" % var
        ret += "                      & (/ dims%%%s, dims%%%s /), &\n" % (var_dsc[2], var_dsc[1])
        ret += "                      & \"%s\", lstat, error_data)\n" % var
    else:
      unformatted = False
      splitted = False
      if (var in etsf_properties):
        props = etsf_properties[var]
        unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
        splitted    = ( props & ETSF_PROP_VAR_SUB_ACCESS == ETSF_PROP_VAR_SUB_ACCESS)
      if (unformatted or splitted):
        ret += "call tests_check_values(group%%%s%%data1D, &\n" % var
        ret += "                      & \"%s\", lstat, error_data)\n" % var
      else:
        if (len(var_dsc) > 2):
          ret += "call tests_check_values(reshape(group%%%s, &\n" %var
          ret += "                      & (/ 1 /)), \"%s\", &\n" % var
          ret += "                      & lstat, error_data)\n"
        else:
          ret += "call tests_check_values(group%%%s, &\n" % var
          ret += "                      & \"%s\", lstat, error_data)\n" % var
    return ret
  else:
    # Case the variable is a scalar
    if (len(var_dsc) < 2):
      ret += "call tests_check_variable(ncid, \"%s\", \"%s\", lstat, error_data)\n" % (var, var_dsc[0])
      return ret

    # Case of a dimension
    var_dsc_cpy = var_dsc[1:]
    var_dsc_cpy.reverse()
    ret += "call tests_check_variable(ncid, \"%s\", \"%s\", (/ &\n" % (var, var_dsc[0])
    for dim in var_dsc_cpy[0:-1]:
      ret += "  & dims%%%s, &\n" % dim
    ret += "  & dims%%%s /), lstat, error_data)\n" % var_dsc_cpy[-1]
    return ret
  
def action_grp(grp_name, grp_id, action):
  """ Action argument can be "write", "read" or "copy"."""
  ret = "subroutine test_%s_%s()\n" % (action, grp)
  ret += "  type(etsf_dims) :: dims\n"
  ret += "  type(etsf_groups) :: groups\n"
  ret += "  type(etsf_groups_flags) :: flags\n"
  ret += "  type(etsf_%s), target :: group\n" % grp
  
  ret += "  logical :: lstat\n"
  ret += "  type(etsf_io_low_error) :: error_data\n"
  
  ret += "  character(len = *), parameter :: me = \"test_%s_%s\"\n" % (action, grp_name)
  ret += "  integer :: ncid\n"
  
  # We create a file for this group.
  ret += """
  groups%%%s => group
  
  write(*,*)
  write(*,*) "Testing etsf_io_data_%s()..."

""" % (grp_name, action)

  if (action == "copy"):
    ret += "call etsf_io_low_open_read(ncid, \"test_write_%s.nc\", lstat, error_data = error_data)\n" % grp_name
    ret += "call etsf_io_dims_get(ncid, dims, lstat, error_data)\n"
    ret += "call etsf_io_low_close(ncid, lstat, error_data)\n"
  else:
    ret += "dims%number_of_grid_points_vector3 = 5\n"
    ret += "dims%number_of_symmetry_operations = 4\n"
    ret += "dims%max_number_of_coefficients = 6\n"
    ret += "dims%max_number_of_projectors = 2\n"
    ret += "dims%max_number_of_states = 8\n"
    ret += "dims%number_of_atoms = 5\n"
    ret += "dims%number_of_atom_species = 3\n"
    ret += "dims%number_of_kpoints = 12\n"
    ret += "dims%number_of_components = 2\n"
  
  ret += "\n"
  
  ret += "  flags%%%s = etsf_%s_all\n" % (grp_name, grp_name)
  ret += "  call etsf_io_data_init(\"test_%s_%s.nc\", flags, &\n" % (action, grp_name)
  ret += "                       & dims, \"Test\", \"\", lstat, error_data)\n"
  ret += "  call tests_status(\"Create file test_%s_%s.nc\", lstat, error_data)\n" \
         % (action, grp_name)
  
  # We allocate space for the data to be written or read and
  # we put some values in it then we associate the data.
  if (action != "copy"):
    for var in etsf_groups[grp_name]:
      # We allocate and put init values.
      ret += "  ! Allocate and init %s\n" % var
      ret += indent_code(output_init_statement(var), 1)
    
    ret += "\n"
    # we call the write routine (both for testing or to write
    # for future read testing).
    ret += "  call etsf_io_data_write(\"test_%s_%s.nc\", &\n" % (action, grp_name)
    ret += "                        & groups, lstat, error_data)\n"
    ret += "  call tests_status(\"write data\", lstat, error_data)\n"
  else:
    # we call the copy routine.
    ret += "  call etsf_io_data_copy(\"test_%s_%s.nc\", \"test_write_%s.nc\", &\n" % \
           (action, grp_name, grp_name)
    ret += "                        & dims, lstat, error_data)\n"
    ret += "  call tests_status(\"copy data to test_%s_%s.nc\", lstat, error_data)\n" % (action, grp_name)
  
  # We open the file for low level access
  ret += "  ! check informations.\n"
  if (action == "write" or action == "copy"):
    # We check the previous write action with low level reading
    ret += "  call etsf_io_low_open_read(ncid, \"test_%s_%s.nc\", lstat, error_data = error_data)\n" % (action, grp_name)
    ret += "  call tests_status(\" | opening\", lstat, error_data)\n"
  elif (action == "read"):
    # we call the read action for testing purpose
    ret += "  call etsf_io_data_read(\"test_%s_%s.nc\", &\n" % (action, grp_name)
    ret += "                       & groups, lstat, error_data)\n"
    ret += "  call tests_status(\"read data\", lstat, error_data)\n"
  
  # We will check the values
  for var in etsf_groups[grp_name]:
    ret += output_check_statement(var, action)
  
  if (action == "write" or action == "copy"):
    # We close the file
    ret += """
  ! close file
  call etsf_io_low_close(ncid, lstat, error_data = error_data)
  call tests_status(" | closing", lstat, error_data)
  
"""
  # We deallocate
  if (action != "copy"):
    for var in etsf_groups[grp_name]:
      # Retrieve variable properties of interest.
      unformatted = False
      splitted = False
      if (var in etsf_properties):
        props = etsf_properties[var]
        unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
        splitted    = ( props & ETSF_PROP_VAR_SUB_ACCESS == ETSF_PROP_VAR_SUB_ACCESS)
      if (unformatted or splitted):
        ret += "  deallocate(group%%%s%%data1D)\n" % (var)
      else:
        ret += "  deallocate(group%%%s)\n" % (var)
  ret += "\n  write(*,*)\n"
  ret += "end subroutine test_%s_%s\n\n" % (action, grp)
  return indent_code(ret, 1)

# ---------------------------------------------------------------------------- #

#
# Main program
#

# Initial setup
my_name    = "autogen_tests.py"
my_configs = ["config/etsf/specs.cf",
              "config/etsf/library.cf",
              "config/etsf/code.cf",
              "config/etsf/functions.py"]

# Check if we are in the top of the ETSF_IO source tree
if ( not os.path.exists("configure.ac") ):
 print("%s: You must be in the top of the library source tree." % my_name)
 print("%s: Aborting now." % my_name)
 sys.exit(1)

# Read config file(s)
for cnf in my_configs:
 if ( os.path.exists(cnf) ):
  exec(compile(open(cnf, "rb").read(), cnf, 'exec'))
 else:
  print("%s: Could not find config file (%s)." % (my_name,cnf))
  print("%s: Aborting now." % my_name)
  sys.exit(2)

# Create tests for data_init()
# ============================
init_grp_src = ""
for grp in etsf_group_list:
  # Load template
  init_grp_src += init_grp(grp, "etsf_grp_" + grp)

ret = open("config/etsf/template.tests_init", "r").read()

# Substitute patterns
ret = re.sub("@INIT_GRP@", init_grp_src, ret)

# Write routine
out = open("%s/tests_init.f90" % (etsf_tests_srcdir),"w")
out.write(ret)
out.close()

for action in ["write", "read", "copy"]:
  # Create tests for data_action()
  # =============================
  action_grp_src = ""
  call_grp_src = ""
  for grp in etsf_group_list:
    # Load template
    action_grp_src += action_grp(grp, "etsf_grp_" + grp, action)
    call_grp_src += "  call test_%s_%s()\n" % (action, grp)

  ret = open("config/etsf/template.tests", "r").read()

  # Substitute patterns
  ret = re.sub("@SUBROUTINE_GRP@", action_grp_src, ret)
  ret = re.sub("@TEST_GRP@", output_wrong_statement(action), ret)
  ret = re.sub("@CALL_GROUP@", call_grp_src, ret)
  ret = re.sub("@ACTION@", action, ret)

  # Write routine
  out = open("%s/tests_%s.f90" % (etsf_tests_srcdir, action),"w")
  out.write(ret)
  out.close()
