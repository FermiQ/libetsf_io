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
edt_split = ""
for dim in etsf_dimensions:
 props = 0
 if (dim in etsf_properties):
   props = etsf_properties[dim]
 split = ( props & ETSF_PROP_DIM_SPLIT != 0)
 if (dim in etsf_constants):
   default_value = etsf_constants[dim]
 else:
   default_value = "1"
 edt += "  %s :: %s = %s\n" % (fortran_type(["integer"]),dim, default_value)
 if (split):
   # Special handling of number_of_grid_points_vector that create a problem
   # because of a name longer than 31 characters.
   if (dim.startswith("number_of_grid_points_vector")):
     edt_split += "  %s :: my_number_of_grid_points_vect%s = etsf_no_dimension\n" % (fortran_type(["integer"]),dim[28])
   else:
     edt_split += "  %s :: my_%s = etsf_no_dimension\n" % (fortran_type(["integer"]),dim)
edt += "\n  !Dimensions for variables that can be splitted.\n"
edt += edt_split
edt += " end type etsf_dims"

# Data structures for each group of variables
# All attributes are made pointers and set to null() when initialised.
egc = "\n\n ! Constants for groups of variables"
egc += "\n integer, parameter :: etsf_grp_%-16s = 0" % "none"
egf = "\n\n ! Folder for the groups of variables\n type etsf_groups"
egv = 1
egn = 0
est = ""
for grp in etsf_group_list:
 egc += "\n integer, parameter :: etsf_grp_%-16s = %d" % (grp,egv)
 egf += "\n  type(etsf_%s), pointer :: %s => null()" % (grp,grp)
 egv *= 2
 egn += 1
  
 out_str = "\n\n ! Data type for %s\n type etsf_%s\n" % (grp,grp)
 out_att = ""  # Attributes code
 out_spe = ""  # Additional specific code
 
 for var in etsf_groups[grp]:
   dsc = etsf_variables[var]
   if ( len(dsc) > 1 ):
     if ( re.match("string",dsc[0]) ):
      dim_offset = 3
     else:
      dim_offset = 2
     # Retrieve variable properties of interest.
     unformatted = False
     splitted = False
     if (var in etsf_properties):
      props = etsf_properties[var]
      unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
      splitted    = ( props & ETSF_PROP_VAR_SUB_ACCESS == ETSF_PROP_VAR_SUB_ACCESS)

     if (unformatted or splitted):
      # Unformatted pointer case
      out_str += "  %s :: %s\n" % (fortran_type(dsc, props),var)
     else:
      if ( len(dsc) >= dim_offset ):
       dim = ":"
       for i in range(len(dsc)-dim_offset):
        dim += ",:"
       # Dimension case (either string or numbers).
       out_str += "  %s, pointer :: %s(%s) => null()\n" % (fortran_type(dsc),var,dim)
      else:
       # String case (dimension less)
       out_str += "  %s, pointer :: %s => null()\n" % (fortran_type(dsc),var)
   else:
     # Numbers
     out_str += "  %s, pointer :: %s => null()\n" % (fortran_type(dsc),var)

   # Retrieve properties of interest.
   att_units = False
   if (var in etsf_properties):
    props = etsf_properties[var]
    att_units = ( props & ETSF_PROP_VAR_UNITS == ETSF_PROP_VAR_UNITS)
   
   if (att_units):
    att_desc = etsf_attributes["units"]
    att_desc2 = etsf_attributes["scale_to_atomic_units"]
    out_att += "  ! Units attributes for variable %s\n" % var
    out_att += "  %s :: %s__%s = %s\n" % (fortran_type(att_desc), var_shortname(var), \
                                          "units", att_desc[1])
    out_att += "  %s :: %s__%s = %s\n" % (fortran_type(att_desc2), var_shortname(var), \
                                          "scale_to_atomic_units", att_desc2[1])
    
   # Check for a max_something dimension
   for dim in dsc[1:]:
     if (dim.startswith("max_")):
       out_spe += "  integer :: %s__%s = etsf_spec_dimension\n" % (var_shortname(var), dim[4:])
     if (splitted and dim == "number_of_spins"):
       out_spe += "  integer :: %s__spin_access = etsf_no_sub_access\n" % var_shortname(var)
     if (splitted and dim == "number_of_kpoints"):
       out_spe += "  integer :: %s__kpoint_access = etsf_no_sub_access\n" % var_shortname(var)
    
 if (out_att != ""):
   out_str += "\n  ! Attributes\n"
   out_str += out_att
 if (out_spe != ""):
   out_str += "\n  ! Specific dimensions (etsf_spec_dimension get the value\n"
   out_str += "  !  of the max_number_of_something when the variable is get\n"
   out_str += "  !  or put, change it to a lower value if less values are to\n"
   out_str += "  !  be accessed).\n"
   out_str += out_spe
 out_str += " end type etsf_%s" % (grp)

 est += """
 !!****s* etsf_groups/etsf_%s
 !! NAME
 !!  etsf_%s
 !!
 !! FUNCTION
 !!  All variables from the specifications have been gathered into types called
 !!  groups. These groups can be gathered into a container called #etsf_groups.
 !!  This container is the main argument of the routines etsf_io_data_read()
 !!  and etsf_io_data_write().
 !!
 !! SOURCE
""" % (grp, grp)
 est += out_str
 est += """
 !!***"""

egf += "\n end type etsf_groups"

# Number of groups
egc += "\n integer, parameter :: etsf_%-20s = %d" % ("ngroups",egn)

# Main variables
emc = "\n\n ! Main variables"
emc += "\n integer, parameter :: etsf_main_%-15s = 0" % "none"
egv  = 1
egn  = 0
for var in etsf_groups["main"]:
 emc += "\n integer, parameter :: etsf_main_%-15s = %d" % \
         (var_shortname(var),egv)
 egv *= 2
 egn += 1

# Number of main variables
emc += "\n integer, parameter :: etsf_%-20s = %d" % ("main_nvars",egn)

# Additional variables for splitted files.
vsp = " type etsf_split\n"
vsn = " type split_dim_names\n"
for var in etsf_variables:
  if (var.startswith("my_")):
    vsp += "  %s, pointer :: %s(:) => null()\n" % (fortran_type(etsf_variables[var]),var)
    vsn += "  character(len = 256) :: %s = \"%s\"\n" % (etsf_variables[var][1][3:], etsf_variables[var][1][3:])
vsp += " end type etsf_split\n"
vsn += " end type split_dim_names\n"

# Flags for specification files.
vlf = ""
vlf_id = ""
ivlf = 0
vlf += "  integer, parameter :: etsf_%-30s = 0\n" % "specs_none"
vlf_id = "  character(len = *), parameter :: etsf_specs_names(%d) = (/ &\n" % len(etsf_specifications_files)
for id in etsf_specifications_files.keys():
  vlf += "  integer, parameter :: etsf_%-30s = %d\n" % (id, 2 ** ivlf)
  vlf_id += "    & \"%-25s\", &\n" % id
  ivlf += 1
vlf += "  integer, parameter :: etsf_%-30s = %d\n" % ("nspecs_data", ivlf)
vlf_id = vlf_id[:-4] + " /)\n"
vlf += vlf_id
  

# Import template
src = file("config/etsf/template.%s" % (etsf_modules["etsf_io"]),"r").read()
src = re.sub("@SCRIPT@",my_name,src)
src = re.sub("@CONSTANTS@",ead,src)
src = re.sub("@FLAGS_GROUPS@",egc,src)
src = re.sub("@FLAGS_MAIN@",emc,src)
src = re.sub("@ETSF_IO_VALIDITY_FLAGS@",vlf,src)
src = re.sub("@DIMENSIONS@",edt,src)
src = re.sub("@STRUCTURES@",est,src)
src = re.sub("@STRUCT_GROUPS@",egf,src)
src = re.sub("@SPLIT_GROUP@",vsp,src)
src = re.sub("@SPLIT_NAME_GROUP@",vsn,src)

# Write module
mod = file(etsf_file_module,"w")
mod.write(src)
mod.close()
