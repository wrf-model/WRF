!---------------------------------------------------------------------------
!
! WRF Parallel I/O
! Author:  Wei Huang huangwei@ucar.edu
! Date:    May 8, 2014
!
!---------------------------------------------------------------------------
!$Id$
!---------------------------------------------------------------------------

module wrf_data_pio

  use pio
  use pio_kinds

  integer                , parameter      :: FATAL            = 0
  integer                , parameter      :: WARN             = 0
  integer                , parameter      :: WrfDataHandleMax = 99
  integer                , parameter      :: MaxDims          = 1000 ! = NF_MAX_VARS
  integer                , parameter      :: MaxVars          = 3000
  integer                , parameter      :: MaxTimes         = 10000
  integer                , parameter      :: DateStrLen       = 19
  integer                , parameter      :: VarNameLen       = 31
  integer                , parameter      :: NO_DIM           = 0
  integer                , parameter      :: NVarDims         = 5
  integer                , parameter      :: NMDVarDims       = 2
  integer                , parameter      :: NOT_LAND_SOIL_VAR= 0
  integer                , parameter      :: LAND_CAT_VAR     = 1
  integer                , parameter      :: SOIL_CAT_VAR     = 2
  integer                , parameter      :: SOIL_LAYERS_VAR  = 3
  integer                , parameter      :: MDL_CPL_VAR      = 4
  integer                , parameter      :: ENSEMBLE_VAR     = 5
  integer                , parameter      :: BDY_VAR          = 10
  character (8)          , parameter      :: NO_NAME          = 'NULL'
  character (DateStrLen) , parameter      :: ZeroDate = '0000-00-00-00:00:00'

#include "wrf_io_flags.h"

  character (256)                         :: msg
  logical                                 :: WrfIOnotInitialized = .true.

  type :: wrf_data_handle
    character (255)                       :: FileName
    integer                               :: FileStatus
    logical                               :: Free
    logical                               :: Write
    character (5)                         :: TimesName
    integer                               :: TimeIndex
    integer                               :: CurrentTime  !Only used for read
    integer                               :: NumberTimes  !Only used for read
    character (DateStrLen), dimension(MaxTimes) :: Times
    integer               , dimension(MaxDims) :: DimLengths
    integer               , dimension(MaxDims) :: DimIDs
    character (31)        , dimension(MaxDims) :: DimNames
    integer                               :: DimUnlimID
    character (9)                         :: DimUnlimName
    integer               , dimension(MaxVars) :: VarIDs
    integer               , dimension(NVarDims-1, MaxVars) :: VarDimLens
    character (VarNameLen), dimension(MaxVars) :: VarNames
    integer                               :: CurrentVariable
    integer                               :: NumDims
    integer                               :: NumVars
! first_operation is set to .TRUE. when a new handle is allocated 
! or when open-for-write or open-for-read are committed.  It is set 
! to .FALSE. when the first field is read or written.  
    logical                               :: first_operation

!--PIO specific
   type (IOsystem_desc_t), pointer :: iosystem    ! PIO type handle to hold PIO-specific information
                                                  ! about a file IO decomposition
   type (File_desc_t)      :: file_handle ! file handle for normal PIO variables

   type (Var_desc_t)       :: vtime

   type (io_desc_t)        :: iodesc3d_m_double, iodesc3d_u_double, iodesc3d_v_double, iodesc3d_w_double
   type (io_desc_t)        :: iodesc2d_m_double, iodesc2d_u_double, iodesc2d_v_double, iodesc2d_char_double
   type (io_desc_t)        :: iodesc3d_m_real, iodesc3d_u_real, iodesc3d_v_real, iodesc3d_w_real
   type (io_desc_t)        :: iodesc2d_m_real, iodesc2d_u_real, iodesc2d_v_real, iodesc2d_char_real
   type (io_desc_t)        :: iodesc3d_m_int, iodesc3d_u_int, iodesc3d_v_int, iodesc3d_w_int
   type (io_desc_t)        :: iodesc2d_m_int, iodesc2d_u_int, iodesc2d_v_int, iodesc2d_char_int

  !type (io_desc_t)        :: iodesc1d_double
  !type (io_desc_t)        :: iodesc1d_real
  !type (io_desc_t)        :: iodesc1d_int

   type (io_desc_t)        :: iodesc3d_land_double, iodesc3d_soil_double, iodesc3d_soil_layers_double
   type (io_desc_t)        :: iodesc3d_land_real,   iodesc3d_soil_real, iodesc3d_soil_layers_real
   type (io_desc_t)        :: iodesc3d_land_int,    iodesc3d_soil_int, iodesc3d_soil_layers_int

   type (io_desc_t)        :: iodesc3d_mdl_cpl_double
   type (io_desc_t)        :: iodesc3d_mdl_cpl_real
   type (io_desc_t)        :: iodesc3d_mdl_cpl_int

   type (io_desc_t)        :: iodesc3d_ensemble_double
   type (io_desc_t)        :: iodesc3d_ensemble_real
   type (io_desc_t)        :: iodesc3d_ensemble_int

   type (io_desc_t)        :: iodesc3d_xsz_u_real, iodesc3d_xsz_u_double, iodesc3d_xsz_u_int
   type (io_desc_t)        :: iodesc3d_xsz_v_real, iodesc3d_xsz_v_double, iodesc3d_xsz_v_int
   type (io_desc_t)        :: iodesc3d_xsz_w_real, iodesc3d_xsz_w_double, iodesc3d_xsz_w_int
   type (io_desc_t)        :: iodesc3d_xsz_m_real, iodesc3d_xsz_m_double, iodesc3d_xsz_m_int

   type (io_desc_t)        :: iodesc3d_xez_u_real, iodesc3d_xez_u_double, iodesc3d_xez_u_int
   type (io_desc_t)        :: iodesc3d_xez_v_real, iodesc3d_xez_v_double, iodesc3d_xez_v_int
   type (io_desc_t)        :: iodesc3d_xez_w_real, iodesc3d_xez_w_double, iodesc3d_xez_w_int
   type (io_desc_t)        :: iodesc3d_xez_m_real, iodesc3d_xez_m_double, iodesc3d_xez_m_int

   type (io_desc_t)        :: iodesc3d_ysz_u_real, iodesc3d_ysz_u_double, iodesc3d_ysz_u_int
   type (io_desc_t)        :: iodesc3d_ysz_v_real, iodesc3d_ysz_v_double, iodesc3d_ysz_v_int
   type (io_desc_t)        :: iodesc3d_ysz_w_real, iodesc3d_ysz_w_double, iodesc3d_ysz_w_int
   type (io_desc_t)        :: iodesc3d_ysz_m_real, iodesc3d_ysz_m_double, iodesc3d_ysz_m_int

   type (io_desc_t)        :: iodesc3d_yez_u_real, iodesc3d_yez_u_double, iodesc3d_yez_u_int
   type (io_desc_t)        :: iodesc3d_yez_v_real, iodesc3d_yez_v_double, iodesc3d_yez_v_int
   type (io_desc_t)        :: iodesc3d_yez_w_real, iodesc3d_yez_w_double, iodesc3d_yez_w_int
   type (io_desc_t)        :: iodesc3d_yez_m_real, iodesc3d_yez_m_double, iodesc3d_yez_m_int

   type (io_desc_t)        :: iodesc2d_xs_m_real, iodesc2d_xs_m_double, iodesc2d_xs_m_int
   type (io_desc_t)        :: iodesc2d_xe_m_real, iodesc2d_xe_m_double, iodesc2d_xe_m_int
   type (io_desc_t)        :: iodesc2d_ys_m_real, iodesc2d_ys_m_double, iodesc2d_ys_m_int
   type (io_desc_t)        :: iodesc2d_ye_m_real, iodesc2d_ye_m_double, iodesc2d_ye_m_int

   type (Var_desc_t), dimension(MaxVars) :: descVar
   type (io_desc_t),  dimension(MaxVars) :: ioVar
   integer, dimension(MaxVars)           :: vartype

   integer(i4)             :: iostat       ! PIO-specific io status
   integer(i4)             :: myrank, nprocs
   integer(i4)             :: pioprocs, piostart, piostride, pioshift
                                           ! the 3D grid size used to write VDC data
  end type wrf_data_handle

  type(wrf_data_handle),target            :: WrfDataHandles(WrfDataHandleMax)

end module wrf_data_pio

