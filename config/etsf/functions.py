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
