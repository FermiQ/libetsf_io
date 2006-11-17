#!/usr/bin/env python
#
# Copyright (c) 2005-2006
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


# Indent a piece of code
def indent_code(code,offset):

 tmp = ""
 for i in range(offset):
  tmp += "  "

 return tmp+re.sub("\n","\n"+tmp,code)



def init_var(var_dsc, var_name, main_id, grp_id):
  ret = """
! Create a file with %s informations.
call etsf_io_data_init("test_%s.nc", %s, %s, &
  & dims, "Fichier de test", "history", lstat, error)
call tests_write_status("main_var: %s", lstat, error)
! check informations.    
call etsf_io_low_open_read(ncid, "test_%s.nc", lstat, error_data = error)
call tests_write_status(" | opening", lstat, error)
""" % (var_name, var_name, main_id, grp_id, var_name, var_name)

  for dim in var_dsc[1:]:
    ret += """
call etsf_io_low_read_dim(ncid, "%s", dimvalue, lstat, error_data = error)
call tests_write_status(" | read '%s'", lstat, error)
if (dimvalue /= dims%%%s) then
  call etsf_io_low_error_set(error, ERROR_MODE_SPEC, ERROR_TYPE_DIM, me, &
                           & tgtname = "%s", errmess = "Wrong value")
  lstat = .false.
end if
call tests_write_status(" | check value '%s'", lstat, error)
""" % (dim, dim, dim, dim, dim)

  ret += """
call etsf_io_low_read_var_infos(ncid, "%s", var_infos, lstat, error_data = error)
call tests_write_status(" | read '%s' characteristics", lstat, error)
if (var_infos%%nctype /= %s .or. &
  & var_infos%%ncshape /= %d .or. &
""" % (var_name, var_name, nf90_type(var_dsc), len(var_dsc) - 1)
  index = len(var_dsc) - 1
  for dim in var_dsc[1:]:
    ret += "  & var_infos%%ncdims(%d) /= dims%%%s .or. &\n" % (index, dim)
    index -= 1
  ret += """  & .false. ) then
  call etsf_io_low_error_set(error, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
                            & tgtname = "%s", errmess = "Wrong characteristic")
  lstat = .false.
end if
call tests_write_status(" | check '%s' characteristics", lstat, error)

call etsf_io_low_close(ncid, lstat, error_data = error)
call tests_write_status(" | closing", lstat, error)
""" % (var_name, var_name)
  return indent_code(ret, 2)

def init_grp(grp_name, grp_id):
  ret = ""
  for var in etsf_groups[grp_name]:
    var_dsc = etsf_variables[var]
    # Load template
    ret += init_var(var_dsc, var, "etsf_main_density", grp_id)

  return ret

def create_variables(grp_name, grp_id):
  # create the declaration of variables used by this grp
  ret = "type(etsf_%s), target :: group\n" % grp_name
  for var in etsf_groups[grp_name]:
    var_dsc = etsf_variables[var]
    # Retrieve variable properties of interest.
    unformatted = False
    splitted = False
    if (var in etsf_properties):
      props = etsf_properties[var]
      unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
      splitted    = ( props & ETSF_PROP_VAR_SPLITTED == ETSF_PROP_VAR_SPLITTED)
    # All variables for the test will be allocatable arrays.
    ret += "%s, allocatable, target :: %s(" % (fortran_type(var_dsc), var)
    
    # If the variable is splitted or unformatted, we
    # use 1D array as target.
    if (not(unformatted) and not(splitted)):
      if (var_dsc[0].startswith("string")):
        stop = -2
      else:
        stop = -1
      for i in var_dsc[1:stop]:
        ret += ":,"
    ret += ":)\n"
  ret += "\n"
  return ret
  
def output_allocate_init_statement(var):
  # Retrieve variable properties of interest.
  unformatted = False
  splitted = False
  if (var in etsf_properties):
    props = etsf_properties[var]
    unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
    splitted    = ( props & ETSF_PROP_VAR_SPLITTED == ETSF_PROP_VAR_SPLITTED)

  ret = ""
  var_dsc = etsf_variables[var]
  # Case the variable is a scalar
  if ((var_dsc[0].startswith("string") and len(var_dsc) < 3) or \
  (not(var_dsc[0].startswith("string")) and len(var_dsc) < 2)):
    ret += "allocate(%s(1))\n" % var
    if (var_dsc[0].startswith("string")):
      ret += "write(%s(1), \"(A)\") \"He\"\n" % var
    else:
      ret += "%s(1) = 456\n" % var
    return ret
  
  
  # The variable is a dimension
  if (var_dsc[0].startswith("string")):
    stop = -2
  else:
    stop = -1
  # We compute the total length of data
  ret += "length = &\n"
  for dim in var_dsc[1:stop]:
    ret += "  & dims%%%s * &\n" % dim
  ret += "  & dims%%%s\n" % var_dsc[stop]
  if (splitted or unformatted):
    # We allocate.
    ret += "allocate(%s(length))\n" % var
    if (var_dsc[0].startswith("string")):
      ret += "write(%s, \"(A)\") \"He\"\n" % var
    else:
      ret += "%s = (/ (i, i = 1, length) /)\n" % var
  else:
    # We allocate with the fixed shape in a reverse order.
    var_dsc_cpy = var_dsc[1:]
    var_dsc_cpy.reverse()
    ret += "allocate(%s( &\n" % var
    for dim in var_dsc_cpy[-1 - stop:-1]:
      ret += "  & dims%%%s, &\n" % dim
    ret += "  & dims%%%s))\n" % var_dsc_cpy[-1]
    if (var_dsc[0].startswith("string")):
      # Only 1D array of strings are supported.
      ret += "do i = 1, dims%%%s, 1\n" % var_dsc[1]
      ret += "  write(%s(i), \"(A)\") \"He\"\n" % var
      ret += "end do\n"
    else:
      # We put values with a reshape.
      ret += "%s = reshape( (/ (i, i = 1, length) /) , (/ & \n" % var
      for dim in var_dsc_cpy[-1 - stop:-1]:
        ret += "  & dims%%%s, &\n" % dim
      ret += "  & dims%%%s /) )\n" % var_dsc_cpy[-1]
  return ret
  
def output_associate_statement(var):
  # Retrieve variable properties of interest.
  unformatted = False
  splitted = False
  if (var in etsf_properties):
    props = etsf_properties[var]
    unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
    splitted    = ( props & ETSF_PROP_VAR_SPLITTED == ETSF_PROP_VAR_SPLITTED)

  ret = ""
  if (splitted):
    ret += "group%%%s%%array%%data1D => %s\n" % (var, var)
  elif (unformatted):
    ret += "group%%%s%%data1D => %s\n" % (var, var)
  else:
    var_dsc = etsf_variables[var]
    if ((var_dsc[0].startswith("string") and len(var_dsc) < 3) or \
    (not(var_dsc[0].startswith("string")) and len(var_dsc) < 2)):
      ret += "group%%%s => %s(1)\n" % (var, var)
    else:
      ret += "group%%%s => %s\n" % (var, var)
  return ret
  
def output_check_statement(var, action):
  # Retrieve variable properties of interest.
  unformatted = False
  splitted = False
  if (var in etsf_properties):
    props = etsf_properties[var]
    unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
    splitted    = ( props & ETSF_PROP_VAR_SPLITTED == ETSF_PROP_VAR_SPLITTED)

  ret = ""
  var_dsc = etsf_variables[var]
  if (var_dsc[0].startswith("string")):
    char_len = "dims%%%s, " % var_dsc[-1]
    stop = -2
  else:
    char_len = ""
    stop = -1
  
  # Case the variable is a scalar
  if ((var_dsc[0].startswith("string") and len(var_dsc) < 3) or \
  (not(var_dsc[0].startswith("string")) and len(var_dsc) < 2)):
    if (action == "write"):
      # In write mode, we read the value.
      ret += "call etsf_io_low_read_var(ncid, \"%s\", %s(1), &\n" % (var, var)
      ret += "                        & %slstat, error_data = error_data)\n" % char_len
      ret += "call tests_%s_status(\" | read '%s' values\", lstat, error_data)\n" % (action, var)
    # We make the test.
    if (var_dsc[0].startswith("string")):
      ret += "if (%s(1)(1:index(%s(1), char(0)) - 1) /= \"He\") then\n" % (var, var)
    else:
      ret += "if (int(%s(1)) /= 456) then\n" % var
    ret += "  call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &\n"
    ret += "                           & tgtname = \"%s\", errmess = \"wrong values\")\n" %var
    ret += "  lstat = .false.\n"
    ret += "end if\n"
    ret += "call tests_%s_status(\" | check '%s' values\", lstat, error_data)\n" % (action, var)
    return ret
  
  # case of array
  if (action == "write"):
    # In write mode, we read the value.
    ret += "call etsf_io_low_read_var(ncid, \"%s\", %s, &\n" % (var, var)
    ret += "                        & %slstat, error_data = error_data)\n" % char_len
    ret += "call tests_write_status(\" | read '%s' values\", lstat, error_data)\n" % var
  # We make the test.
  if (var_dsc[0].startswith("string")):
    ret += "lstat = .true.\n"
    ret += "do i = 1, dims%%%s, 1\n" % var_dsc[1]
    ret += "  if (index(%s(i), char(0)) > 0) then\n" % var
    ret += "    lstat = (%s(i)(1:index(%s(i), char(0)) - 1) == \"He\") .and. lstat\n" % (var, var)
    ret += "  else\n"
    ret += "    lstat = (trim(%s(i)) == \"He\") .and. lstat\n" % var
    ret += "  end if\n"
    ret += "end do\n"
  else:
    ret += "length = &\n"
    for dim in var_dsc[1:stop]:
      ret += "  & dims%%%s * &\n" % dim
    ret += "  & dims%%%s\n" % var_dsc[stop]
    if (var_dsc[0].startswith("integer")):
      tab = "int"
    else:
      tab = "dbl"
    ret += "allocate(test_%s_tab(length))\n" % tab
    ret += "test_%s_tab = reshape(%s, (/ length /))\n" % (tab, var)
    ret += "lstat = .true.\n"
    ret += "do i = 1, length, 1\n"
    ret += "  lstat = (int(test_%s_tab(i)) == i) .and. lstat\n" % tab
    ret += "end do\n"
    ret += "deallocate(test_%s_tab)\n" % tab
  ret += "if (.not. lstat) then\n"
  ret += "  call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &\n"
  ret += "                           & tgtname = \"%s\", errmess = \"wrong values\")\n" %var
  ret += "  lstat = .false.\n"
  ret += "end if\n"
  ret += "call tests_%s_status(\" | check '%s' values\", lstat, error_data)\n" % (action, var)
  return ret
  
def readwrite_grp(grp_name, grp_id, action):
  """ Action argument is either "write" or "read"."""
  ret = "subroutine test_%s_%s()\n" % (action, grp)
  ret += "  type(etsf_dims) :: dims\n"
  ret += "  type(etsf_groups) :: groups\n"
  ret += "  type(etsf_main) :: main\n"
  ret += "  integer :: i, length, ncid\n"
  ret += "  logical :: lstat\n"
  ret += "  integer, allocatable :: test_int_tab(:)\n"
  ret += "  double precision, allocatable :: test_dbl_tab(:)\n"
  ret += "  character(len = *), parameter :: me = \"test_%s_%s\"\n" % (action, grp_name)
  ret += "  type(etsf_io_low_error) :: error_data\n"
  ret += indent_code(create_variables(grp_name, grp_id), 1)
  # We create a file for this group.
  ret += """
  groups%%%s => group
  
  write(*,*)
  write(*,*) "Testing etsf_io_data_%s()..."

  dims%%number_of_grid_points_vector3 = 5
  dims%%number_of_symmetries = 4
  dims%%max_number_of_coefficients = 6
  dims%%max_number_of_projectors = 2
  dims%%max_number_of_states = 8
  dims%%number_of_atoms = 4
  dims%%number_of_kpoints = 12
  dims%%number_of_components = 2
  call etsf_io_data_init("test_%s_%s.nc", etsf_main_none, %s, &
                       & dims, "Fichier de test", "history", lstat, error_data)
  call tests_%s_status("init file with group '%s'", lstat, error_data)
""" % (grp_name, action, action, grp_name, grp_id, action, grp_name)
  # We allocate space for the data to be written or read and
  # we put some values in it then we associate the data.
  for var in etsf_groups[grp_name]:
    # We allocate and put init values.
    ret += "! Allocate and init %s\n" % var
    ret += output_allocate_init_statement(var)
    # We associate
    ret += "! Associate %s\n" % var
    # If the var is neither unformatted nor splitted, we associate directly
    ret += output_associate_statement(var)
    
  ret += "\n"
  # we call the write routine (both for testing or to write for future read testing).
  ret += "call etsf_io_data_write(\"test_%s_%s.nc\", etsf_main_none, %s, &\n" % (action, grp_name, grp_id)
  ret += "                      & main, groups, lstat, error_data)\n"
  ret += "call tests_%s_status(\"write data\", lstat, error_data)\n" % action
  
  # We open the file for low level access
  ret += "! check informations.\n"
  if (action == "write"):
    # We check the previous write action with low level reading
    ret += "call etsf_io_low_open_read(ncid, \"test_write_%s.nc\", lstat, error_data = error_data)\n" % (grp_name)
    ret += "call tests_write_status(\" | opening\", lstat, error_data)\n"
  else:
    # we call the read action for testing purpose
    ret += "call etsf_io_data_read(\"test_read_%s.nc\", etsf_main_none, %s, &\n" % (grp_name, grp_id)
    ret += "                     & main, groups, lstat, error_data)\n"
    ret += "call tests_read_status(\"read data\", lstat, error_data)\n"
  
  # We will check the values
  for var in etsf_groups[grp_name]:
    ret += output_check_statement(var, action)
  
  if (action == "write"):
    # We close the file
    ret += """
  ! close file
  call etsf_io_low_close(ncid, lstat, error_data = error_data)
  call tests_write_status(" | closing", lstat, error_data)
"""
  # We deallocate
  for var in etsf_groups[grp_name]:
    ret += "deallocate(%s)\n" % (var)
  ret += "  write(*,*)\n"
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

# Check if we are in the top of the ABINIT source tree
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

# Create tests for data_init()
# ============================
init_main_src = ""
for var in etsf_groups["main"]:
  var_dsc = etsf_variables[var]
  # Load template
  init_main_src += init_var(var_dsc, var, "etsf_main_" + etsf_main_names[var], "etsf_grp_none")

init_grp_src = ""
for grp in etsf_group_list[1:]:
  # Load template
  init_grp_src += init_grp(grp, "etsf_grp_" + grp)

ret = file("config/etsf/template.tests_init", "r").read()

# Substitute patterns
ret = re.sub("@INIT_MAIN@", init_main_src, ret)
ret = re.sub("@INIT_GRP@", init_grp_src, ret)

# Write routine
out = file("%s/tests_init.f90" % (etsf_tests_srcdir),"w")
out.write(ret)
out.close()

# Create tests for data_write()
# =============================
write_main_src = ""
write_assoc_src = ""
for var in etsf_groups["main"]:
  var_dsc = etsf_variables[var]
  # Load template
  write_main_src += "call test_write_var_unformatted(\"%s\", %s)\n" % \
    (var, "etsf_main_" + etsf_main_names[var])
  write_assoc_src += "main%%%s%%data1D => values\n" % var
write_main_src = indent_code(write_main_src, 2)
  
write_grp_src = ""
call_grp_src = ""
for grp in etsf_group_list[1:]:
  # Load template
  write_grp_src += readwrite_grp(grp, "etsf_grp_" + grp, "write")
  call_grp_src += "  call test_write_%s()\n" % grp

ret = file("config/etsf/template.tests_write", "r").read()

# Substitute patterns
#ret = re.sub("@WRITE_MAIN_ASSOCIATE@", write_assoc_src, ret)
#ret = re.sub("@WRITE_MAIN@", write_main_src, ret)
ret = re.sub("@WRITE_GRP@", write_grp_src, ret)
ret = re.sub("@CALL_WRITE_GROUP@", call_grp_src, ret)

# Write routine
out = file("%s/tests_write.f90" % (etsf_tests_srcdir),"w")
out.write(ret)
out.close()

# Create tests for data_read()
# =============================
read_main_src = ""
read_assoc_src = ""
for var in etsf_groups["main"]:
  var_dsc = etsf_variables[var]
  # Load template
  read_main_src += "call test_read_var_unformatted(\"%s\", %s)\n" % \
    (var, "etsf_main_" + etsf_main_names[var])
  read_assoc_src += "main%%%s%%data1D => values\n" % var
read_main_src = indent_code(read_main_src, 2)
  
read_grp_src = ""
call_grp_src = ""
for grp in etsf_group_list[1:]:
  # Load template
  read_grp_src += readwrite_grp(grp, "etsf_grp_" + grp, "read")
  call_grp_src += "  call test_read_%s()\n" % grp

ret = file("config/etsf/template.tests_read", "r").read()

# Substitute patterns
#ret = re.sub("@READ_MAIN_ASSOCIATE@", read_assoc_src, ret)
#ret = re.sub("@READ_MAIN@", read_main_src, ret)
ret = re.sub("@READ_GRP@", read_grp_src, ret)
ret = re.sub("@CALL_READ_GROUP@", call_grp_src, ret)

# Write routine
out = file("%s/tests_read.f90" % (etsf_tests_srcdir),"w")
out.write(ret)
out.close()
