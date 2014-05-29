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
  integer                , parameter      :: MaxDims          = 2000 ! = NF_MAX_VARS
  integer                , parameter      :: MaxVars          = 3000
  integer                , parameter      :: MaxTimes         = 10000
  integer                , parameter      :: DateStrLen       = 19
  integer                , parameter      :: VarNameLen       = 31
  integer                , parameter      :: NO_DIM           = 0
  integer                , parameter      :: NVarDims         = 4
  integer                , parameter      :: NMDVarDims       = 2
  character (8)          , parameter      :: NO_NAME          = 'NULL'
  character (DateStrLen) , parameter      :: ZeroDate = '0000-00-00-00:00:00'

#include "wrf_io_flags.h"

  character (256)                         :: msg
  logical                                 :: WrfIOnotInitialized = .true.

  type :: wrf_data_handle
    character (255)                       :: FileName
    integer                               :: FileStatus
    integer                               :: Comm
    integer                               :: NCID
    logical                               :: Free
    logical                               :: Write
    character (5)                         :: TimesName
    integer                               :: TimeIndex
    integer                               :: CurrentTime  !Only used for read
    integer                               :: NumberTimes  !Only used for read
    character (DateStrLen), pointer       :: Times(:)
    integer                               :: TimesVarID
    integer               , pointer       :: DimLengths(:)
    integer               , pointer       :: DimIDs(:)
    character (31)        , pointer       :: DimNames(:)
    integer                               :: DimUnlimID
    character (9)                         :: DimUnlimName
    integer       , dimension(NVarDims)   :: DimID
    integer       , dimension(NVarDims)   :: Dimension
    integer               , pointer       :: MDVarIDs(:)
    integer               , pointer       :: MDVarDimLens(:)
    character (80)        , pointer       :: MDVarNames(:)
    integer               , pointer       :: VarIDs(:)
    integer               , pointer       :: VarDimLens(:,:)
    character (VarNameLen), pointer       :: VarNames(:)
    integer                               :: CurrentVariable  !Only used for read
    integer                               :: NumVars
! first_operation is set to .TRUE. when a new handle is allocated 
! or when open-for-write or open-for-read are committed.  It is set 
! to .FALSE. when the first field is read or written.  
    logical                               :: first_operation
! Whether pnetcdf file is in collective (.true.) or independent mode
! Collective mode is the default.
    logical                               :: Collective

!--PIO specific
   type (Var_desc_t)       :: v3d_handle, v2d_handle, v1d_handle, vtime
   type (File_desc_t)      :: file_handle
                            ! file handle for normal PIO variables
   type (IOsystem_desc_t), pointer :: iosystem
                            ! PIO type handle to hold PIO-specific information
                            ! about a file IO decomposition
   type (io_desc_t)        :: iodesc_3d, iodesc_2d
                            ! PIO-specific error code variable
   integer(i4)             :: iostat
                            ! used in the uncompressed PIO for defining
                            ! dimensions used in a *cdf file
   integer(i4)             :: dim_ids_3d(3), dim_ids_2d(2), dim_ids_1d(1)
                            ! used to tell PIO how many IO procs you want to
                            ! use,
                            ! functions as the max # of IO procs wanted when
                            ! using compression, less may be used
   integer(i4)             :: myrank, nprocs
   integer(i4)             :: pioprocs, piostart, piostride, pioshift
                            ! the 3D grid size used to write VDC data
   integer(i4)             :: dims_3d(3), dims_2d(2), dims_1d(1)
                            ! counter
   integer(kind=PIO_Offset),allocatable :: compdof_3d(:), compdof_2d(:)
                            !slice of computational data to the global grid
  end type wrf_data_handle

  type(wrf_data_handle),target            :: WrfDataHandles(WrfDataHandleMax)

end module wrf_data_pio

