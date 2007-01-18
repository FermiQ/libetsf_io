def fortran_type(var_desc, props = ETSF_PROP_NONE):

  values = var_desc[0].split()
  unformatted = ( props & ETSF_PROP_VAR_UNFORMATTED == ETSF_PROP_VAR_UNFORMATTED)
  splitted    = ( props & ETSF_PROP_VAR_SUB_ACCESS == ETSF_PROP_VAR_SUB_ACCESS)
  if (unformatted or splitted):
    ret = "type(etsf_io_low_var_"
    if ( values[0] == "integer" ):
      ret += "integer)"
    elif ( values[0] == "real" ):
      ret += "double)"
    else:
      raise ValueError
  else:
    if ( values[0] == "integer" ):
      ret = "integer"
    elif ( values[0] == "real" ):
      if ( values[1] == "single_precision" ):
        ret = "real"
      elif ( values[1] == "double_precision" ):
        ret = "double precision"
      else:
        raise ValueError
    elif ( values[0] == "string" ):
      if ( len(values) > 1):
        ret = "character(len=%s)" % (etsf_constants[values[1]])
      else:
        ret = "character(len=%s)" % (etsf_constants[var_desc[-1]])
    else:
      raise ValueError

  return ret



def nf90_type(var_desc):

  values = var_desc[0].split()
  if ( values[0] == "integer" ):
    ret = "etsf_io_low_integer"
  elif ( values[0] == "real" ):
    if ( values[1] == "single_precision" ):
      ret = "etsf_io_low_real"
    elif ( values[1] == "double_precision" ):
      ret = "etsf_io_low_double"
    else:
      raise ValueError
  elif ( values[0] == "string" ):
    ret = "etsf_io_low_character"
  else:
    raise ValueError

  return ret

def var_shortname(var):
  if (var in etsf_variables_shortnames):
    return etsf_variables_shortnames[var]
  else:
    return var

def dim_get_split_array(dim):
  for var in etsf_variables:
    if (var.startswith("my_") and etsf_variables[var][1] == dim):
      return var
  return None

def var_get_split_status(var):
  for dim in etsf_variables[var][1:]:
    if (dim in etsf_properties):
      if (etsf_properties[dim] & ETSF_PROP_DIM_SPLIT != 0):
        return True
  return False

def limit_length(var):
  """ This routine is used to change the name of the argument to be
      shorter than 31 characters. This is done, case by case. """
  if (var.startswith("my_number_of_grid_points_vector")):
    return "my_number_of_grid_points_vect" + var[31]
  else:
    return var

def expand_length(var):
  """ This routine is the inverse of limit_length. """
  if (var == "my_number_of_grid_points_vect1"):
    return "my_number_of_grid_points_vector1"
  elif (var == "my_number_of_grid_points_vect2"):
    return "my_number_of_grid_points_vector2"
  elif (var == "my_number_of_grid_points_vect3"):
    return "my_number_of_grid_points_vector3"
  else:
    return var