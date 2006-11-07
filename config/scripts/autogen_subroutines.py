#!/usr/bin/env python
#
# Copyright (c) 2005-2006 The ABINIT Group (Yann Pouillon)
# All rights reserved.
#
# This file is part of the ABINIT software package. For license information,
# please see the COPYING file in the top-level directory of the ABINIT source
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

 ret = """
! Create the NetCDF file
call etsf_io_low_open_create(ncid, filename, etsf_file_format_version, lstat, &
                           & title = trim(title), history = trim(history), &
                           & error_data = error_data)
if (.not. lstat) return

! Define dimensions
dims%character_string_length        = etsf_charlen
dims%number_of_cartesian_directions = etsf_3dimlen
dims%number_of_reduced_dimensions   = etsf_3dimlen
dims%number_of_vectors              = etsf_3dimlen
dims%symbol_length                  = etsf_chemlen

call etsf_io_dims_def(ncid, dims, lstat, error_data)
if (.not. lstat) return

! Define groups
"""

 ret += code_data_select("def")
 ret += "\n\n! Define main variable\n"
 ret += "call etsf_io_main_def(ncid, main_var, lstat, error_data)\n"
 ret += "if (.not. lstat) return\n"

 ret += """
! End definitions and close file
call etsf_io_low_close(ncid, lstat, error_data = error_data)"""

 return ret



# Code for data contents
def code_data_contents():

 ret = """! Open file for reading
call etsf_io_low_open_read(ncid, trim(filename), lstat, error_data = error_data)
if (.not. lstat) return

! Get Dimensions
call etsf_dims_get(ncid, dims, lstat, error_data)
if (.not. lstat) return

"""

 ret += "! Get group names\ngroups = 0\n"

 for grp in etsf_group_list:
  if ( grp != "main" ):
   ret += """
call etsf_io_low_read_var_infos(ncid, "%s", var_infos, lstat)
if ( lstat ) groups = groups + etsf_grp_%s
""" % (etsf_groups[grp][0],grp)

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



# Code for grouped data reading
def code_data_read():

 ret = """! Open file for reading
call etsf_io_low_open_read(ncid, trim(filename), lstat, error_data = error_data)
if (.not. lstat) return

! Get Data
"""

 ret += code_group_main("get", "main_") + "\n\n"

 ret += code_data_select("get")

 ret += """

! Close file
call etsf_io_low_close(ncid, lstat, error_data = error_data)"""

 return ret



# Code for grouped data selection
def code_data_select(action):

 ret = "do i = 1, etsf_ngroups\n\n select case ( iand(groups, 2 ** (i - 1)) )"

 for group in etsf_group_list:
  if ( group != "main" ):
   if ( action == "def" ):
    buf = "call etsf_io_%s_def(ncid, lstat, error_data)\n" % (group)
    buf += "if (.not. lstat) return"
   else:
    buf = "call etsf_io_%s_%s(ncid, group_folder%%%s, lstat, error_data)\n" % (group, action, group)
    buf += "if (.not. lstat) return"

   ret += "\n\n  case (etsf_grp_%s)\n   %s" % (group, indent_code(buf,3))

 ret += "\n\n end select\nend do"

 return ret



# Code for grouped data writing
def code_data_write():

 ret = """! Open file for writing
call etsf_io_low_open_modify(ncid, trim(filename), lstat, error_data = error_data)
if (.not. lstat) return

! Put Data
"""

 ret += code_group_main("put", "main_") + "\n\n"

 ret += code_data_select("put")

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
        att_len = etsf_constants[att_desc[0].split()[1]]+ ", "
      else:
        att_len = ""

      if ( ret != "" ):
        ret += "\n"

      if ( action == "put" ):
        ret += "call etsf_io_low_write_att(ncid, etsf_io_low_global_att, \"%s\", &\n" % att \
             + "                         & %s, &\n" % val \
             + "                         & lstat, error_data = error_data)\n" \
             + "if (.not. lstat) return\n"
      elif ( action == "get" ):
        ret += "call etsf_io_low_read_att(ncid, etsf_io_low_global_att, \"%s\", &\n" % att \
             + "                        & %s%s, &\n" % (att_len, val) \
             + "                        & lstat, error_data = error_data)\n" \
             + "if (.not. lstat) return\n"

  return ret



# Generic code for a group
def code_group_generic(group,action):

 if ( group == "main" ):
  return code_group_main(action)

 ret = ""

 # Look for peculiarities
 if ( group in etsf_properties ):
  specs = etsf_properties[group]
 else:
  specs = ETSF_PROP_NONE

 # Process each variable in the group
 for var in etsf_groups[group]:
  var_desc = etsf_variables[var]

  if ( ret != "" ):
   ret += "\n"

  if ( var_desc[0].startswith("string")):
    char_len = etsf_constants[var_desc[-1]] + ", "
  else:
    char_len = ""
  if ( action == "def" ):
    # Create the definition of the shape and dimensions
    if ( len(var_desc) > 1 ):
      dims = "pad(\"" + var_desc[1] + "\")"
      for dim in var_desc[2:]:
        dims = "pad(\""+ dim + "\"), &\n%4s& " % " " + dims
      dims = "(/ " + dims + " /), "
    else:
      dims = None

    ret += "  call etsf_io_low_def_var(ncid, \"%s\", &\n" % var \
         + "    & %s, &\n" % nf90_type(var_desc)
    if (dims is not None):
      ret += "    & %s &\n" % dims
    ret += "    & lstat, error_data = error_data)\n" \
         + "  if (.not. lstat) return\n"
  elif ( action == "put" ):
    ret += "  call etsf_io_low_write_var(ncid, \"%s\", &\n" % var \
         + "                           & folder%%%s, %s&\n" % (var, char_len) \
         + "                           & lstat, error_data = error_data)\n" \
         + "  if (.not. lstat) return\n"
  elif ( action == "get" ):
    ret += "  call etsf_io_low_read_var(ncid, \"%s\", &\n" % var \
         + "                          & folder%%%s, %s&\n" % (var, char_len) \
         + "                          & lstat, error_data = error_data)\n" \
         + "  if (.not. lstat) return\n"
  else:
    raise ValueError

 return ret



# Code for the main group
def code_group_main(action,prefix=""):

 ret = "select case ( main_var )\n"

 for var in etsf_groups["main"]:
  var_desc = etsf_variables[var]

  ret += "\ncase ( etsf_main_%s )\n" % (etsf_main_names[var])

  if ( action == "def" ):
    # Create the definition of the shape and dimensions
    if ( len(var_desc) > 1 ):
      dims = "pad(\"" + var_desc[1] + "\")"
      for dim in var_desc[2:]:
        dims = "pad(\""+ dim + "\"), &\n%4s& " % " " + dims
      dims = "(/ " + dims + " /), "
    else:
      dims = None

    ret += "  call etsf_io_low_def_var(ncid, \"%s\", &\n" % var \
         + "    & %s, &\n" % nf90_type(var_desc)
    if (dims is not None):
      ret += "    & %s &\n" % dims
    ret += "    & lstat, error_data = error_data)\n" \
         + "  if (.not. lstat) return\n"
  elif ( action == "put" ):
    ret += "  call etsf_io_low_write_var(ncid, \"%s\", &\n" % var \
         + "                           & %sfolder%%%s, &\n" % (prefix, var) \
         + "                           & lstat, error_data = error_data)\n" \
         + "  if (.not. lstat) return\n"
  elif ( action == "get" ):
    ret += "  call etsf_io_low_read_var(ncid, \"%s\", &\n" % var \
         + "                          & %sfolder%%%s, &\n" % (prefix, var) \
         + "                          & lstat, error_data = error_data)\n" \
         + "  if (.not. lstat) return\n"
  else:
    raise ValueError

 ret += "\nend select"

 return ret



# Transfer data to and from an optional argument
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
 arg_move = len(name)+16
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
  for arg_str in args:
   arg_info = arg_str.split()
   arg = arg_info[0]

   # Optional arguments
   if ( (len(arg_info) > 3) and (arg_info[3] == "optional") ):
    opt = ",optional"
   else:
    opt = ""

   # Arrays
   if ( len(arg_info) > 4 ):
    dim = ":"
    for i in range(int(arg_info[4])-1):
     dim += ",:"
    arg_desc += "  %s%s,pointer :: %s(%s)\n" % \
     (arg_info[1],opt,arg,dim)
    if ( (arg_info[3] == "optional") or (arg_info[3] == "local") ):
     loc_vars += "  %s,allocatable :: my_%s(%s)\n" % (arg_info[1],arg,dim)
   else:
    arg_desc += "  %s%s,intent(%s) :: %s\n" % (arg_info[1],opt,arg_info[2],arg)
    if ( (len(arg_info) > 3) and 
         ((arg_info[3] == "optional") or (arg_info[3] == "local")) ):
     loc_vars += "  %s :: my_%s\n" % (arg_info[1],arg)

  if ( type != "subroutine" ):
   loc_vars += "  %s :: etsf_%s\n" % (type,name)

  ret = re.sub("@ARG_DESC@\n",arg_desc,ret)
  ret = re.sub("@LOCAL_VARS@",loc_vars,ret)
 else:
  ret = re.sub(", @ARG_LIST@","",ret)
  ret = re.sub("@ARG_DESC@\n","",ret)
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
   sub_cprm = "sub_name,action"
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

# Generate etsf_handle_error routine
#src = init_routine("handle_error","error","",my_name,None)
#out = file("%s/etsf_handle_error.c" % (etsf_file_srcdir),"w")
#out.write(src)
#out.close()

# Fix etsf_data_init routine
#src = file("%s/etsf_data_init.F90" % (etsf_file_srcdir),"r").read()
#src = re.sub(" type.etsf_dims. :: dims","",src)
#out = file("%s/etsf_data_init.F90" % (etsf_file_srcdir),"w")
#out.write(src)
#out.close()
