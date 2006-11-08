  !!****d* etsf_io_low_level/ERROR_MODE
  !! NAME
  !!  ERROR_MODE
  !!
  !! FUNCTION
  !!  These values are used to index the action done when an error occurs. We found
  !!  the following values:
  !!   * ERROR_MODE_DEF  = error when defining a variable or a dimension.
  !!   * ERROR_MODE_GET  = error when read a value for a valid dimension,
  !!                       attribute or variable.
  !!   * ERROR_MODE_IO   = error when accessing one file (opening or closing).
  !!   * ERROR_MODE_INQ  = error when looking in the NetCDF file for informations.
  !!   * ERROR_MODE_PUT  = error when writing a value to a valid target.
  !!   * ERROR_MODE_SPEC = error of match between read value and awaited type or shape.
  !!
  !! SOURCE
  integer, parameter :: ERROR_MODE_DEF = 1, ERROR_MODE_GET = 2, ERROR_MODE_IO   = 3, &
                      & ERROR_MODE_INQ = 4, ERROR_MODE_PUT = 5, ERROR_MODE_SPEC = 6
  !!***

  !!****d* etsf_io_low_level/ERROR_TYPE
  !! NAME
  !!  ERROR_TYPE
  !!
  !! FUNCTION
  !!  These values are used to index the type of target when an error occurs. We found
  !!  the following values:
  !!   * ERROR_TYPE_ATT = error on attributes.
  !!   * ERROR_TYPE_DID = error on dimension ids.
  !!   * ERROR_TYPE_DIM = error on dimensions.
  !!   * ERROR_TYPE_END = error on ending define mode.
  !!   * ERROR_TYPE_DEF = error on switching to define mode.
  !!   * ERROR_TYPE_OCR = .
  !!   * ERROR_TYPE_ORD = error on opening for read access.
  !!   * ERROR_TYPE_OWR = error on opening for write access.
  !!   * ERROR_TYPE_VAR = error on variables.
  !!   * ERROR_TYPE_VID = error on variable ids.
  !!   * ERROR_TYPE_CLO = error on closing.
  !!   * ERROR_TYPE_ARG = error on routine argument.
  !!
  !! SOURCE
  integer, parameter :: ERROR_TYPE_ATT =  1, ERROR_TYPE_DID =  2, ERROR_TYPE_DIM =  3, &
                      & ERROR_TYPE_END =  4, ERROR_TYPE_DEF =  5, ERROR_TYPE_OCR =  6, &
                      & ERROR_TYPE_ORD =  7, ERROR_TYPE_OWR =  8, ERROR_TYPE_VAR =  9, &
                      & ERROR_TYPE_VID = 10, ERROR_TYPE_CLO = 11, ERROR_TYPE_ARG = 12
  !!***

  !!****d* etsf_io_low_level/ETSF_IO_LOW_CONSTANTS
  !! NAME
  !!  ETSF_IO_LOW_CONSTANTS
  !!
  !! FUNCTION
  !!  These values are identical to the ones defined in NetCDF. They are defined
  !!  to be able to use "implicit none" without linking with NetCDF library.
  !!
  !! SOURCE
  integer, parameter :: etsf_io_low_global_att = NF90_GLOBAL
  integer, parameter :: etsf_io_low_integer    = NF90_INT
  integer, parameter :: etsf_io_low_real       = NF90_FLOAT
  integer, parameter :: etsf_io_low_double     = NF90_DOUBLE
  integer, parameter :: etsf_io_low_character  = NF90_CHAR
  !!***

  !!****s* etsf_io_low_level/etsf_io_low_error
  !! NAME
  !!  etsf_io_low_error
  !!
  !! FUNCTION
  !!  This structure is used to store error informations. Three fields are mandatory
  !!  and can always be read:
  !!   * parent, which is a string with the name of the method where the error occurs ;
  !!   * access_mode_id, which is a #ERROR_MODE value ;
  !!   * target_type_id, which is a #ERROR_TYPE value.
  !!  All other fields may be filled depending on the calling method. When a field
  !!  is irrelevant, if an id, it is null or negative, and when a string it is
  !!  void string (trim(string) == "").
  !!
  !! SOURCE
  type etsf_io_low_error
    character(len = 80) :: parent
    
    integer :: access_mode_id
    character(len = 80) :: access_mode_str
    integer :: target_type_id
    character(len = 80) :: target_type_str
    
    integer :: target_id
    character(len = 80) :: target_name
    
    integer :: error_id
    character(len = 256) :: error_message
  end type etsf_io_low_error
  !!***

  !!****s* etsf_io_low_level/etsf_io_low_var_infos
  !! NAME
  !!  etsf_io_low_var_infos
  !!
  !! FUNCTION
  !!  This structure is used to store variable informations, such as
  !!  name, NetCDF id, type, shape and dimensions.
  !!
  !! SOURCE
  type etsf_io_low_var_infos
    character(len = 80) :: name
    
    integer :: ncid
    integer :: nctype
    integer :: ncshape
    integer :: ncdims(1:16)
  end type etsf_io_low_var_infos
  !!***
  
  !!****d* etsf_io_low_level/FLAGS_MATCHING
  !! NAME
  !!  FLAGS_MATCHING
  !!
  !! FUNCTION
  !!  These flags are used when comparing to variables (see etsf_io_low_check_var()).
  !!   * etsf_io_low_var_match: is used when two variables have the same type,
  !!                            shape and dimensions.
  !!   * etsf_io_low_var_type_dif: is used when the type is compatible but conversions
  !!                               will be done (between numerical values for instance).
  !!   * etsf_io_low_var_shape_dif: is used when the shape is different but the number
  !!                                of elements is still compatible.
  !!
  !! SOURCE
  integer, parameter :: etsf_io_low_var_match = 0, etsf_io_low_var_type_dif = 1, &
                      & etsf_io_low_var_shape_dif = 2
  !!***

  !!****s* etsf_io_low_level/etsf_io_low_var_double
  !! NAME
  !!  etsf_io_low_var_double
  !!
  !! FUNCTION
  !!  This structure is used as an abstraction on a storage for a variable. Only
  !!  one pointer can be associated at a time.  The shape of the stored data is
  !!  then defined by the associated pointer. This structure is used to read or
  !!  write data when the storage area in memory can have different shapes.
  !!
  !! SOURCE
  type etsf_io_low_var_double
    double precision, pointer :: data1D(:) => null()
    double precision, pointer :: data2D(:, :) => null()
    double precision, pointer :: data3D(:, :, :) => null()
    double precision, pointer :: data4D(:, :, :, :) => null()
    double precision, pointer :: data5D(:, :, :, :, :) => null()
    double precision, pointer :: data6D(:, :, :, :, :, :) => null()
    double precision, pointer :: data7D(:, :, :, :, :, :, :) => null()
  end type etsf_io_low_var_double
  !!***
  
  !!****s* etsf_io_low_level/etsf_io_low_var_integer
  !! NAME
  !!  etsf_io_low_var_integer
  !!
  !! FUNCTION
  !!  This structure is used as an abstraction on a storage for a variable. Only
  !!  one pointer can be associated at a time.  The shape of the stored data is
  !!  then defined by the associated pointer. This structure is used to read or
  !!  write data when the storage area in memory can have different shapes.
  !!
  !! SOURCE
  type etsf_io_low_var_integer
    integer, pointer :: data1D(:) => null()
    integer, pointer :: data2D(:, :) => null()
    integer, pointer :: data3D(:, :, :) => null()
    integer, pointer :: data4D(:, :, :, :) => null()
    integer, pointer :: data5D(:, :, :, :, :) => null()
    integer, pointer :: data6D(:, :, :, :, :, :) => null()
    integer, pointer :: data7D(:, :, :, :, :, :, :) => null()
  end type etsf_io_low_var_integer
  !!***
