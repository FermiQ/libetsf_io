!!****h* utils/etsf_io_file
!! NAME
!!  etsf_io_file
!!
!! FUNCTION
!!  This module contains different high level routines to access ETSF files. It
!!  actually contains:
!!  * etsf_io_file_merge(): a routine to read several files and merge their data
!!                          into a single output file.
!!  * etsf_io_file_contents(): a routine to read a file and get what specifications
!!                             this file is matching (cristallographic data,
!!                             potential...).
!!  * etsf_io_file_check(): a routine to validate a file against one or several
!!                          specifications.
!!
!! COPYRIGHT
!!  Copyright (C) 2006
!!  This file is distributed under the terms of the
!!  GNU General Public License, see ~abinit/COPYING
!!  or http://www.gnu.org/copyleft/lesser.txt .
!!***
module etsf_io_file

  use etsf_io

  implicit none

  private

  !* This type is a private type to store informations about a file.
  !* These informations are the dims values, the split definitions and
  !* the list of variable definitions.
  type file_infos_type
     !* The path to the file.
     character(len = 256) :: path
     !* The ETSF dimensions of the file (including names and values).
     type(etsf_dims)      :: dims
     !* The ETSF split definitions for the file (allocated arrays).
     type(etsf_split)     :: split
     !* The comprehensive list of variables of the file (with
     !* their dimension definitions, names...).
     type(etsf_vars)      :: var_list
  end type file_infos_type

  public :: etsf_io_file_merge
  public :: etsf_io_file_check
  public :: etsf_io_file_contents
@SPEC_CHECK_PUBLIC@

contains

  include "etsf_io_file_contents.f90"
@SPEC_CHECK_INCLUDE@

  include "etsf_io_file_private.f90"
  include "etsf_io_file_public.f90"

end module etsf_io_file
