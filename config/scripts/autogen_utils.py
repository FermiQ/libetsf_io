#!/usr/bin/env python
#
# Copyright (C) 2006, 2007 (Damien Caliste)
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

 if (code.endswith("\n")):
   return tmp+re.sub("\n","\n"+tmp,code[:-1])+"\n"
 else:
   return tmp+re.sub("\n","\n"+tmp,code)
 
# Subpart of code_check_var
def sub_code_check_var(var_desc):
  ret  = "var_infos%%ncshape = %d\n" % len(var_desc)
  if (len(var_desc) > 0):
    ret += "allocate(var_infos%%ncdimnames(%d))\n" % len(var_desc)
    i = 0
    for dim in var_desc:
      # If the dim is potentialy splitable, we do something.
      if (dim in etsf_properties and \
          etsf_properties[dim] & ETSF_PROP_DIM_SPLIT != 0):
        ret += "if (associated(split%%%s)) then\n" % (dim_get_split_array("my_" + dim))
        ret += "  write(var_infos%%ncdimnames(%d), \"(A)\") \"%s\"\n" % \
               (len(var_desc) - i, limit_length("my_" + dim))
        ret += "else\n"
        ret += "  write(var_infos%%ncdimnames(%d), \"(A)\") \"%s\"\n" % \
               (len(var_desc) - i, dim)
        ret += "end if\n"
      else:
        ret += "write(var_infos%%ncdimnames(%d), \"(A)\") \"%s\"\n" % \
               (len(var_desc) - i, dim)
      i += 1
  return ret

# Create lines to check a variable.
def code_check_var(varname, condname):
  var_desc = etsf_variables[varname]
  ret  = ""
  ret += "! Variable %s\n" % varname
  ret += "write(var_infos%%name, \"(A)\") \"%s\"\n" % varname
  ret += "var_infos%%nctype  = %s\n" % nf90_type(var_desc)
  # We treat the variable 'reduced_coordinates_of_plane_waves' as a special case.
  if (varname == "reduced_coordinates_of_plane_waves"):
    ret += "call etsf_io_low_read_flag(ncid, valid, \"reduced_coordinates_of_plane_waves\", &\n"
    ret += "                         & \"k_dependent\", lstat, error_data = error_data)\n"
    ret += "if (valid) then\n"
    ret += indent_code(sub_code_check_var(var_desc[1:]), 1)
    ret += "else\n"
    ret += indent_code(sub_code_check_var(var_desc[2:]), 1)
    ret += "end if\n"
  else:
    ret += sub_code_check_var(var_desc[1:])
  ret += "call test_var(ncid, var_infos, %s, error_data)\n" % condname
  if (len(var_desc) > 1):
    ret += "deallocate(var_infos%ncdimnames)\n"
  return ret

# Create subroutine to test each specification.
def code_check_spec(mandatory, optional):
  ret  = ""
  for (type, value) in mandatory:
    if (type == "var"):
      # We check this existence and definition of this variable.
      ret += code_check_var(value, "lstat")
      ret += "if (.not. lstat) then\n" \
             + "  call etsf_io_split_free(split)\n" \
             + "  call etsf_io_low_error_update(error_data, me)\n" \
             + "  return\n" \
             + "end if\n"
      ret += "\n"
    elif (type == "list"):
      # We check the existence and definition of at least one of these variables.
      ret += "! Check from a list.\n"
      ret += "lstat = .false.\n"
      listname = ""
      for var in value:
        ret += code_check_var(var, "valid")
        ret += "if (.not. valid .and. error_data%access_mode_id == ERROR_MODE_SPEC) return\n"
        ret += "lstat = lstat .or. valid\n"
        listname += var + ", "
      if (len(listname) > 75):
        listname = listname[:74] + "..."
      else:
        listname = listname[:-2]
      ret += "if (.not. lstat) then\n"
      ret += "  call etsf_io_split_free(split)\n"
      ret += "  call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, &\n"
      ret += "                           & ERROR_TYPE_ARG, me, &\n"
      ret += " & tgtname = \"%s\", &\n" % listname
      ret += "                           & errmess = \"missing one among the list.\")\n"
      ret += "  return\n"
      ret += "end if\n"
      ret += "\n"
    elif(type == "test"):
      # We test a list of things depending on the presence of a given variable.
      ret += "! Check a list of conditions if a variable is set.\n"
      ret += "! Test the existence of a variable.\n"
      ret += "lstat = .false.\n"
      ret += "call etsf_io_low_read_var_infos(ncid, \"%s\", var_infos, &\n" % value[0]
      ret += "                              & lstat, error_data = error_data)\n"
      ret += "if (lstat) then\n"
      ret += "  ! Apply the conditions since variable exists.\n"
      ret += indent_code(code_check_spec(value[1], []), 1)
      ret += "end if\n"
    elif (type == "cond"):
      # We test the existence to this variable depending on the value
      # of another variable.
      ret += "! Check these variables depends on the value of another.\n"
      ret += "! Read the condition value.\n"
      ret += "call etsf_io_low_read_var(ncid, \"%s\", string_value, etsf_charlen, &\n" % value[0]
      ret += "                        & lstat, error_data = error_data)\n"
      ret += "if (.not. lstat) then\n" \
             + "  call etsf_io_split_free(split)\n" \
             + "  call etsf_io_low_error_update(error_data, me)\n" \
             + "  return\n" \
             + "end if\n"
      ret += "call strip(string_value)\n"
      first = True
      for key in value[1].keys():
        if (first):
          ret += "if (trim(string_value) == \"%s\") then\n" % key
          first = False
        else:
          ret += "else if (trim(string_value) == \"%s\") then\n" % key
        ret += indent_code(code_check_spec(value[1][key], []), 1)
      ret += "else\n"
      ret += "  call etsf_io_split_free(split)\n"
      ret += "  call etsf_io_low_error_set(error_data, ERROR_MODE_SPEC, &\n"
      ret += "                           & ERROR_TYPE_ARG, me, &\n"
      ret += "                           & tgtname = \"%s\", &\n" % value[0]
      ret += "                           & errmess = \"Empty or unknown value '\"//trim(string_value)//\"'.\")\n"
      ret += "  lstat = .false.\n"
      ret += "  return\n"
      ret += "end if\n"
  return ret

def code_contents():
  ret = ""
  i = 1
  for (id, stuff) in etsf_specifications_files.items():
    ret += "call etsf_io_file_check_%s(ncid, lstat, errors(%d))\n" % (id, i)
    ret += "if (lstat) then\n"
    ret += "  read_flags = read_flags + etsf_%s\n" % id
    ret += "else\n"
    ret += "  call etsf_io_low_error_update(errors(%d), me)\n" % i
    ret += "end if\n"
    i += 1
  return ret

# ---------------------------------------------------------------------------- #

#
# Main program
#

# Initial setup
my_name    = "autogen_utils.py"
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

# Create the validating routine
# =============================
str_check = indent_code(code_contents(), 1)

ret = file("config/etsf/template.utils_contents.f90", "r").read()

# Substitute patterns
ret = re.sub("@CODE@", str_check, ret)

# Write routine
out = file("%s/etsf_io_file_contents.f90" % (etsf_utils_srcdir),"w")
out.write(ret)
out.close()

makefile_list = ""
include_list = ""
public_list = ""
for (id, (mandatory, optional)) in etsf_specifications_files.items():
  makefile_list += "\tetsf_io_file_check_%s.f90 \\\n" % id
  include_list  += "  include \"etsf_io_file_check_%s.f90\"\n" % id
  public_list   += "  public :: etsf_io_file_check_%s\n" % id
  str_check = indent_code(code_check_spec(mandatory, optional), 1)
  ret = file("config/etsf/template.utils_check.f90", "r").read()
  ret = re.sub("@SPEC_NAME@", id, ret)
  ret = re.sub("@CODE@", str_check, ret)
  out = file("%s/etsf_io_file_check_%s.f90" % (etsf_utils_srcdir, id),"w")
  out.write(ret)
  out.close()

# Create the source and doc Makefile.am.
ret = file("config/etsf/template.utils_Makefile.am", "r").read()
ret = re.sub("@SPEC_CHECK_LIST@\n", makefile_list, ret)
out = file("%s/Makefile.am" % (etsf_utils_srcdir),"w")
out.write(ret)
out.close()
ret = file("config/etsf/template.doc_utils_Makefile.am", "r").read()
makefile_doc_list = re.sub(".f90", "_f90.html", makefile_list)
ret = re.sub("@SPEC_CHECK_LIST@\n", makefile_doc_list, ret)
out = file("doc/www/utils/Makefile.am", "w")
out.write(ret)
out.close()

ret = file("config/etsf/template.utils_file.f90", "r").read()
ret = re.sub("@SPEC_CHECK_INCLUDE@\n", include_list, ret)
ret = re.sub("@SPEC_CHECK_PUBLIC@\n", public_list, ret)
out = file("%s/etsf_io_file.f90" % (etsf_utils_srcdir),"w")
out.write(ret)
out.close()
