!WRF:DRIVER_LAYER:CONSTANTS
!
!  This MODULE contains all of the constants used in the model.  These
!  are separated by usage within the code.

MODULE module_driver_constants

   !  0. The following tells the rest of the model what data ordering we are
   !     using

   INTEGER , PARAMETER :: DATA_ORDER_XYZ = 1
   INTEGER , PARAMETER :: DATA_ORDER_YXZ = 2
   INTEGER , PARAMETER :: DATA_ORDER_ZXY = 3
   INTEGER , PARAMETER :: DATA_ORDER_ZYX = 4
   INTEGER , PARAMETER :: DATA_ORDER_XZY = 5
   INTEGER , PARAMETER :: DATA_ORDER_YZX = 6
   INTEGER , PARAMETER :: DATA_ORDER_XY = DATA_ORDER_XYZ
   INTEGER , PARAMETER :: DATA_ORDER_YX = DATA_ORDER_YXZ


#include "model_data_order.inc"

   !  1. Following are constants for use in defining maximal values for array
   !     definitions.  
   !

   !  The maximum number of levels in the model is how deeply the domains may
   !  be nested.

   INTEGER , PARAMETER :: max_levels      =  20

   !  The maximum number of nests that can depend on a single parent and other way round

   INTEGER , PARAMETER :: max_nests        =  20

   !  The maximum number of parents that a nest can have (simplified assumption -> one only)

   INTEGER , PARAMETER :: max_parents      =  1

   !  The maximum number of domains is how many grids the model will be running.

   INTEGER , PARAMETER :: max_domains     =   ( MAX_DOMAINS_F - 1 ) / 2 + 1

   !  The maximum number of nest move specifications allowed in a namelist

   INTEGER , PARAMETER :: max_moves       =   50

   !  The maximum number of eta levels. With vertical refinement defining
   !  each domain separately, the aggregated number of levels could be large.

   INTEGER , PARAMETER :: max_eta         =   10001

   !  The maximum number of ocean levels in the 3d U Miami ocean.

   INTEGER , PARAMETER :: max_ocean       =   501

   !  The maximum number of pressure levels to interpolate to, for diagnostics

   INTEGER , PARAMETER :: max_plevs       =   100

   !  The maximum number of height levels to interpolate to, for diagnostics

   INTEGER , PARAMETER :: max_zlevs       =   100

   !  The maximum number of trackchem

   INTEGER , PARAMETER :: max_trackchem   =   100

   !  The maximum number of outer iterations (for DA minimisation)

   INTEGER , PARAMETER :: max_outer_iterations = 100

   !  The maximum number of cost functions to find sensitivity w.r.t (for custom sensitivity)

   INTEGER , PARAMETER :: max_sens = 100

   !  The maximum number of instruments (for radiance DA)

   INTEGER , PARAMETER :: max_instruments =   30

   !  The maximum number of obs indexes (for conventional DA obs) 

   INTEGER , PARAMETER :: num_ob_indexes  =   29

   !  The maximum number of bogus storms

   INTEGER , PARAMETER :: max_bogus =  5

   !  The maximum number of fields that can be sent or received in coupled mode

   INTEGER , PARAMETER :: max_cplfld = 20

   !  The maximum number of domains used by the external model with which wrf is communicating in coupled mode

   INTEGER , PARAMETER :: max_extdomains = 5

   !  2. Following related to driver level data structures for DM_PARALLEL communications

#ifdef DM_PARALLEL
   INTEGER , PARAMETER :: max_comms       =   1024
#else
   INTEGER , PARAMETER :: max_comms       =   1
#endif

   !  3. Following is information related to the file I/O.

   !  These are the bounds of the available FORTRAN logical unit numbers for the file I/O.
   !  Only logical unit numbers within these bounds will be chosen for I/O unit numbers.

   INTEGER , PARAMETER :: min_file_unit = 10
   INTEGER , PARAMETER :: max_file_unit = 99

   !  4. Unfortunately, the following definition is needed here (rather
   !     than the more logical place in share/module_model_constants.F)
   !     for the namelist reads in frame/module_configure.F, and for some
   !     conversions in share/set_timekeeping.F
   !     Actually, using it here will mean that we don't need to set it
   !     in share/module_model_constants.F, since this file will be
   !     included (USEd) in:
   !        frame/module_configure.F
   !     which will be USEd in:
   !        share/module_bc.F
   !     which will be USEd in:
   !        phys/module_radiation_driver.F
   !     which is the other important place for it to be, and where
   !     it is passed as a subroutine parameter to any physics subroutine.
   !
   !     P2SI is the number of SI seconds in an planetary solar day
   !     divided by the number of SI seconds in an earth solar day
#if defined MARS
   !     For Mars, P2SI = 88775.2/86400.
   REAL , PARAMETER :: P2SI = 1.0274907
#elif defined TITAN
   !     For Titan, P2SI = 1378080.0/86400.
   REAL , PARAMETER :: P2SI = 15.95
#else
   !     Default for Earth
   REAL , PARAMETER :: P2SI = 1.0
#endif
 CONTAINS
   SUBROUTINE init_module_driver_constants
   END SUBROUTINE init_module_driver_constants
 END MODULE module_driver_constants

! routines that external packages can call to get at WRF stuff that isn't available
! through argument lists; since they are external we don't want them using WRF 
! modules unnecessarily (complicates the build even more)
 SUBROUTINE inquire_of_wrf_data_order_xyz( data_order )
   USE module_driver_constants, ONLY : DATA_ORDER_XYZ
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: data_order
   data_order = DATA_ORDER_XYZ
 END SUBROUTINE inquire_of_wrf_data_order_xyz

 SUBROUTINE inquire_of_wrf_data_order_xzy( data_order )
   USE module_driver_constants, ONLY : DATA_ORDER_XZY
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: data_order
   data_order = DATA_ORDER_XZY
 END SUBROUTINE inquire_of_wrf_data_order_xzy

 SUBROUTINE inquire_of_wrf_iwordsize( iwordsz )
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: iwordsz
   iwordsz = IWORDSIZE
 END SUBROUTINE inquire_of_wrf_iwordsize

 SUBROUTINE inquire_of_wrf_rwordsize( rwordsz )
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: rwordsz
   rwordsz = RWORDSIZE
 END SUBROUTINE inquire_of_wrf_rwordsize

