
MODULE module_comm_dm_3

   IMPLICIT NONE

   PRIVATE module_comm_dm_dummy_3

#ifdef DM_PARALLEL
   INTEGER, PRIVATE :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
   INTEGER, PRIVATE :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
   LOGICAL, EXTERNAL :: rsl_comm_iter
#endif

   INTEGER, PRIVATE :: idim1, idim2, idim3, idim4, idim5, idim6, idim7


CONTAINS

   ! Avoid complaints about empty CONTAINS from some compilers.  
   SUBROUTINE module_comm_dm_dummy_3
     USE module_domain, ONLY:domain
     USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
     USE module_state_description, ONLY:PARAM_FIRST_SCALAR
     USE module_driver_constants
     RETURN
   END SUBROUTINE module_comm_dm_dummy_3

! Registry-generated communication subroutines.  
#ifdef DM_PARALLEL
#include "REGISTRY_COMM_DM_3_subs.inc"
#endif

END MODULE module_comm_dm_3

