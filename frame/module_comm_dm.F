
MODULE module_comm_dm

   USE module_comm_dm_0
   USE module_comm_dm_1
   USE module_comm_dm_2
   USE module_comm_dm_3
   USE module_comm_dm_4_

   IMPLICIT NONE

   PRIVATE module_comm_dm_dummy

#ifdef DM_PARALLEL
   INTEGER, PRIVATE :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
   INTEGER, PRIVATE :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
#endif

   INTEGER, PRIVATE :: idim1, idim2, idim3, idim4, idim5, idim6, idim7


CONTAINS

   ! Avoid complaints about empty CONTAINS from some compilers.
   SUBROUTINE module_comm_dm_dummy
     USE module_domain, ONLY:domain
     USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
     USE module_state_description, ONLY:PARAM_FIRST_SCALAR
     USE module_driver_constants
     RETURN
   END SUBROUTINE module_comm_dm_dummy

! Registry-generated communication subroutines.
#ifdef DM_PARALLEL
#include "REGISTRY_COMM_DM_subs.inc"
#endif

END MODULE module_comm_dm

