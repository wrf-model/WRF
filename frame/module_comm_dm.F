
MODULE module_comm_dm

   USE module_domain
   USE module_configure
   IMPLICIT NONE

   PRIVATE module_comm_dm_dummy

CONTAINS

   ! Avoid complaints about empty CONTAINS from some compilers.  
   SUBROUTINE module_comm_dm_dummy
     RETURN
   END SUBROUTINE module_comm_dm_dummy

! Registry-generated communication subroutines.  
#ifdef DM_PARALLEL
#include "REGISTRY_COMM_DM_subs.inc"
#endif

END MODULE module_comm_dm

