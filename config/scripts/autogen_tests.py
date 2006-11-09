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
    if ((var_dsc[0].startswith("string") and len(var_dsc) > 2) or \
    (not(var_dsc[0].startswith("string")) and len(var_dsc) > 1)):
      ret += "%s, allocatable, target :: %s(" % (fortran_type(var_dsc), var)
      if (var_dsc[0].startswith("string")):
        stop = -2
      else:
        stop = -1
      for i in var_dsc[1:stop]:
        ret += ":,"
      ret += ":)\n"
  ret += "\n"
  return ret
  
def write_grp(grp_name, grp_id):
  ret = "subroutine test_write_%s()\n" % grp
  ret += "  type(etsf_dims) :: dims\n"
  ret += "  type(etsf_groups) :: groups\n"
  ret += "  type(etsf_main) :: main\n"
  ret += "  double precision, target :: density(10)\n"
  ret += "  integer :: i, length, ncid\n"
  ret += "  logical :: lstat\n"
  ret += "  integer, allocatable :: test_int_tab(:)\n"
  ret += "  double precision, allocatable :: test_dbl_tab(:)\n"
  ret += "  character(len = *), parameter :: me = \"test_write_%s\"\n" % grp_name
  ret += "  type(etsf_io_low_error) :: error_data\n"
  ret += indent_code(create_variables(grp_name, grp_id), 1)
  # We create a file for this group.
  ret += """
  main%%density%%data1D => density
  density(:) = 0.d0
  groups%%%s => group
  
  write(*,*)
  write(*,*) "Testing etsf_io_data_write()..."

  dims%%number_of_grid_points_vector3 = 5
  dims%%number_of_symmetries = 4
  dims%%max_number_of_coefficients = 6
  dims%%max_number_of_projectors = 2
  dims%%max_number_of_states = 8
  dims%%number_of_atoms = 4
  dims%%number_of_kpoints = 12
  dims%%number_of_components = 2
  call etsf_io_data_init("test_write_%s.nc", etsf_main_density, %s, &
                       & dims, "Fichier de test", "history", lstat, error_data)
  call tests_write_status("init file with group '%s'", lstat, error_data)
""" % (grp_name, grp_name, grp_id, grp_name)
  # We allocate space for the data to be written.
  for var in etsf_groups[grp_name]:
    var_dsc = etsf_variables[var]
    if ((var_dsc[0].startswith("string") and len(var_dsc) > 2) or \
    (not(var_dsc[0].startswith("string")) and len(var_dsc) > 1)):
      var_dsc_cpy = var_dsc[1:]
      var_dsc_cpy.reverse()
      ret += "allocate(%s( &\n" % (var)
      if (var_dsc[0].startswith("string")):
        stop = -2
      else:
        stop = -1
      for dim in var_dsc_cpy[-1 - stop:-1]:
        ret += "  & dims%%%s, &\n" % dim
      ret += "  & dims%%%s))\n" % var_dsc_cpy[-1]
      if (var_dsc[0].startswith("string")):
        ret += """
  do i = 1, dims%%%s, 1
    write(%s(i), "(A)") "He"
  end do
""" % (var_dsc[1], var)
      else:
        ret += "length = &\n"
        for dim in var_dsc[1:stop]:
          ret += "  & dims%%%s * &\n" % dim
        ret += "  & dims%%%s\n" % var_dsc[stop]
        ret += "%s = reshape( (/ (i, i = 1, length) /) , (/ & \n" % var
        for dim in var_dsc_cpy[-1 - stop:-1]:
          ret += "  & dims%%%s, &\n" % dim
        ret += "  & dims%%%s /) )\n" % var_dsc_cpy[-1]
    else:
      if (var_dsc[0].startswith("string")):
        ret += "write(group%%%s, \"(A)\") \"He\"\n" % var
      else:
        ret += "group%%%s = 456\n" % var
  # We associate
  for var in etsf_groups[grp_name]:
    var_dsc = etsf_variables[var]
    if ((var_dsc[0].startswith("string") and len(var_dsc) > 2) or \
    (not(var_dsc[0].startswith("string")) and len(var_dsc) > 1)):
      ret += "group%%%s => %s\n" % (var, var)
  ret += "\n"
  # we call the write routine
  ret += """
  call etsf_io_data_write("test_write_%s.nc", etsf_main_density, %s, &
                        & main, groups, lstat, error_data)
  call tests_write_status("write data", lstat, error_data)
""" % (grp_name, grp_id)
  # We open the file for low level access
  ret += """
  ! check informations.    
  call etsf_io_low_open_read(ncid, "test_write_%s.nc", lstat, error_data = error_data)
  call tests_write_status(" | opening", lstat, error_data)
""" % (grp_name)
  # We call the low level read routines
  for var in etsf_groups[grp_name]:
    var_dsc = etsf_variables[var]
    if (var_dsc[0].startswith("string")):
      char_len = "dims%%%s, " % var_dsc[-1]
      stop = -2
    else:
      char_len = ""
      stop = -1
    if ((var_dsc[0].startswith("string") and len(var_dsc) > 2) or \
    (not(var_dsc[0].startswith("string")) and len(var_dsc) > 1)):
      # case of array
      ret += """
  call etsf_io_low_read_var(ncid, "%s", %s, &
                          & %slstat, error_data = error_data)
  call tests_write_status(" | read '%s' values", lstat, error_data)
""" % (var, var, char_len, var)
      if (var_dsc[0].startswith("string")):
        ret += """
  lstat = .true.
  do i = 1, dims%%%s, 1
    if (index(%s(i), char(0)) > 0) then
      lstat = (%s(i)(1:index(%s(i), char(0)) - 1) == \"He\") .and. lstat
    else
      lstat = (trim(%s(i)) == \"He\") .and. lstat
    end if
  end do
  if (.not. lstat) then
""" % (var_dsc[1], var, var, var, var)
      else:
        ret += "length = &\n"
        for dim in var_dsc[1:stop]:
          ret += "  & dims%%%s * &\n" % dim
        ret += "  & dims%%%s\n" % var_dsc[stop]
        if (var_dsc[0].startswith("integer")):
          tab = "int"
        else:
          tab = "dbl"
        ret += "  allocate(test_%s_tab(length))\n" % tab
        ret += "  test_%s_tab = reshape(%s, (/ length /))\n" % (tab, var)
        ret += """
  lstat = .true.
  do i = 1, length, 1
    lstat = (int(test_%s_tab(i)) == i) .and. lstat
  end do
  deallocate(test_%s_tab)
  if (.not. lstat) then
""" % (tab, tab)
    else:
      # case of single value
      ret += """
  call etsf_io_low_read_var(ncid, "%s", group%%%s, &
                          & %slstat, error_data = error_data)
  call tests_write_status(" | read '%s' values", lstat, error_data)
""" % (var, var, char_len, var)
      if (var_dsc[0].startswith("string")):
        ret += "  if (group%%%s(1:index(group%%%s, char(0)) - 1) /= \"He\") then\n" % (var, var)
      else:
        ret += "  if (int(group%%%s) /= 456) then\n" % var
    ret += """
    call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, ERROR_TYPE_VAR, me, &
                             & tgtname = "%s", errmess = "wrong values")
    lstat = .false.
  end if
  call tests_write_status(" | check '%s' values", lstat, error_data)
""" % (var, var)
  # We close the file
  ret += """
  ! close file
  call etsf_io_low_close(ncid, lstat, error_data = error_data)
  call tests_write_status(" | closing", lstat, error_data)
"""
  # We deallocate
  for var in etsf_groups[grp_name]:
    var_dsc = etsf_variables[var]
    if ((var_dsc[0].startswith("string") and len(var_dsc) > 2) or \
    (not(var_dsc[0].startswith("string")) and len(var_dsc) > 1)):
      ret += "deallocate(%s)\n" % (var)
  ret += "  write(*,*)\n"
  ret += "end subroutine test_write_%s\n\n" %grp
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
  write_grp_src += write_grp(grp, "etsf_grp_" + grp)
  call_grp_src += "  call test_write_%s()\n" % grp

ret = file("config/etsf/template.tests_write", "r").read()

# Substitute patterns
ret = re.sub("@WRITE_MAIN_ASSOCIATE@", write_assoc_src, ret)
ret = re.sub("@WRITE_MAIN@", write_main_src, ret)
ret = re.sub("@WRITE_GRP@", write_grp_src, ret)
ret = re.sub("@CALL_WRITE_GROUP@", call_grp_src, ret)

# Write routine
out = file("%s/tests_write.f90" % (etsf_tests_srcdir),"w")
out.write(ret)
out.close()
