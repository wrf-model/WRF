!WRF:DRIVER_LAYER:CONSTANTS
!
!  This MODULE contains all of the constants used in the model.  These
!  are separated by uage within the code.

MODULE module_driver_constants

   !  0. The following tells the rest of the model what data ordering we are
   !     using

   INTEGER , PARAMETER :: DATA_ORDER_XYZ = 1
   INTEGER , PARAMETER :: DATA_ORDER_YXZ = 2
   INTEGER , PARAMETER :: DATA_ORDER_ZXY = 3
   INTEGER , PARAMETER :: DATA_ORDER_ZYX = 4
   INTEGER , PARAMETER :: DATA_ORDER_XZY = 5
   INTEGER , PARAMETER :: DATA_ORDER_YZX = 6

#include <model_data_order.inc>

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

   !  2. Following related to driver leve data structures for DM_PARALLEL communications

#ifdef DM_PARALLEL
   INTEGER , PARAMETER :: max_comms       =   1024
#else
   INTEGER , PARAMETER :: max_comms       =   1
#endif

   !  3. Following is information related to the file I/O.

   !  These are the bounds of the available FORTRAN logical unit numbers for the file I/O.
   !  Only logical unti numbers within these bounds will be chosen for I/O unit numbers.

   INTEGER , PARAMETER :: min_file_unit = 10
   INTEGER , PARAMETER :: max_file_unit = 99
 CONTAINS
   SUBROUTINE init_module_driver_constants
   END SUBROUTINE init_module_driver_constants
 END MODULE module_driver_constants
