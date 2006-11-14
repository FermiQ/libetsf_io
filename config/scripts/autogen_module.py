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
# Main program
#

# Initial setup
my_name    = "autogen_module"
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

# Global attributes
ead = " ! Global attributes"
for att in etsf_attributes.keys():
 att_desc = etsf_attributes[att]

 if ( att in etsf_properties ):
  specs = etsf_properties[att]
 else:
  specs = ETSF_PROP_NONE

 if ( ((specs & ETSF_PROP_ATT_GLOBAL) != 0) and (len(att_desc) > 1) ):
  if ( re.match("string",att_desc[0]) ):
   spc = "&\n  & "
  else:
   spc = ""
  ead += "\n %s,parameter :: etsf_%s = %s%s" % \
   (fortran_type(att_desc),att.lower(),spc,att_desc[1])

# Data type for dimensions
edt = "\n\n ! Data type for dimensions\n type etsf_dims\n"
for dim in etsf_dimensions:
 edt += "  %s :: %s = 1\n" % (fortran_type(["integer"]),dim)
edt += " end type etsf_dims"

# Data structures for each group of variables
egc = "\n\n ! Constants for groups of variables"
egc += "\n integer, parameter :: etsf_grp_%-16s = 0" % "none"
egf = "\n\n ! Folder for the groups of variables\n type etsf_groups"
egv = 1
egn = 0
est = ""
ema = ""
for grp in etsf_group_list:
 if ( grp != "main" ):
  egc += "\n integer, parameter :: etsf_grp_%-16s = %d" % (grp,egv)
  egf += "\n  type(etsf_%s), pointer :: %s => null()" % (grp,grp)
  egv *= 2
  egn += 1
  
 out_str = "\n\n ! Data type for %s\n type etsf_%s\n" % (grp,grp)

 for var in etsf_groups[grp]:
  dsc = etsf_variables[var]
  if ( len(dsc) > 1 ):
   if ( re.match("string",dsc[0]) ):
    dim_offset = 3
   else:
    dim_offset = 2
   # If the type is given as unformatted, we use
   # the etsf_io_low_var_* type to hide dimensions.
   if ( re.match(".*unformatted$",dsc[0]) ):
    dim_offset = len(dsc) + 1

   dim = ":"
   if ( len(dsc) >= dim_offset ):
    for i in range(len(dsc)-dim_offset):
     dim += ",:"
    
    out_str += "  %s, pointer :: %s(%s) => null()\n" % (fortran_type(dsc),var,dim)
   else:
    out_str += "  %s :: %s\n" % (fortran_type(dsc),var)
  else:
   out_str += "  %s :: %s\n" % (fortran_type(dsc),var)

 out_str += " end type etsf_%s" % (grp)
 if ( grp != "main" ):
  est += """
  !!****s* etsf_io/etsf_%s
  !! NAME
  !!  etsf_%s
  !!
  !! FUNCTION
  !!  This structure is a container that stores variables used in a same context.
  !!
  !! SOURCE
""" % (grp, grp)
  est += out_str
  est += """
  !!***"""
 else:
  ema += out_str

egf += "\n end type etsf_groups"

# Number of groups
egc += "\n integer, parameter :: etsf_%-20s = %d" % ("ngroups",egn)

# Main variables
emc = "\n\n ! Main variables (select only one at a time)"
emc += "\n integer, parameter :: etsf_main_%-15s = 0" % "none"
egv  = 1
egn  = 0
for var in etsf_groups["main"]:
 emc += "\n integer,parameter :: etsf_main_%-15s = %d" % \
         (etsf_main_names[var],egv)
 egv += 1
 egn += 1

# Number of main variables
emc += "\n integer,parameter :: etsf_%-20s = %d" % ("main_nvars",egn)

# Import template
src = file("config/etsf/template.%s" % (etsf_modules["etsf_io"]),"r").read()
src = re.sub("@SCRIPT@",my_name,src)
src = re.sub("@CONSTANTS@",ead,src)
src = re.sub("@FLAGS_GROUPS@",egc,src)
src = re.sub("@FLAGS_MAIN@",emc,src)
src = re.sub("@DIMENSIONS@",edt,src)
src = re.sub("@STRUCT_MAIN@",ema,src)
src = re.sub("@STRUCTURES@",est,src)
src = re.sub("@STRUCT_GROUPS@",egf,src)

# Write module
mod = file(etsf_file_module,"w")
mod.write(src)
mod.close()
