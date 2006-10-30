  integer, parameter :: ERROR_MODE_DEF = 1, ERROR_MODE_GET = 2, ERROR_MODE_IO   = 3, &
                      & ERROR_MODE_INQ = 4, ERROR_MODE_PUT = 5, ERROR_MODE_SPEC = 6

  integer, parameter :: ERROR_TYPE_ATT =  1, ERROR_TYPE_DID =  2, ERROR_TYPE_DIM = 3, &
                      & ERROR_TYPE_END =  4, ERROR_TYPE_INI =  5, ERROR_TYPE_OCR = 6, &
                      & ERROR_TYPE_ORD =  7, ERROR_TYPE_OWR =  8, ERROR_TYPE_VAR = 9, &
                      & ERROR_TYPE_VID = 10, ERROR_TYPE_CLO = 11


! Public structure to gather error informations
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
