SUBROUTINE shift_domain_em ( grid , disp_x, disp_y &
!
# include "dummy_new_args.inc"
!
                           )
   USE module_state_description
   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_domain_type, ONLY : fieldlist
   USE module_timing
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
#ifdef DM_PARALLEL
   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, itrace
   USE module_comm_dm, ONLY : SHIFT_HALO_sub
#else
   USE module_dm
#endif
   IMPLICIT NONE
  ! Arguments
   INTEGER disp_x, disp_y       ! number of parent domain points to move
   TYPE(domain) , POINTER                     :: grid
  ! Local 
   INTEGER i, j, ii, ipf
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
   TYPE (grid_config_rec_type)  :: config_flags
   TYPE( fieldlist ), POINTER :: p

   INTERFACE
       ! need to split this routine to avoid clobbering certain widely used compilers
       SUBROUTINE shift_domain_em2 ( grid , disp_x, disp_y &
!
# include "dummy_new_args.inc"
!
                           )
          USE module_state_description
          USE module_domain, ONLY : domain
          IMPLICIT NONE
         ! Arguments
          INTEGER disp_x, disp_y       ! number of parent domain points to move
          TYPE(domain) , POINTER                     :: grid

          !  Definitions of dummy arguments to solve
#include "dummy_new_decl.inc"
       END SUBROUTINE shift_domain_em2
   END INTERFACE

   !  Definitions of dummy arguments to solve
#include "dummy_new_decl.inc"

#ifdef MOVE_NESTS

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   px = isign(config_flags%parent_grid_ratio,disp_x)
   py = isign(config_flags%parent_grid_ratio,disp_y)

   grid%imask_nostag = 1
   grid%imask_xstag = 1
   grid%imask_ystag = 1
   grid%imask_xystag = 1

   grid%imask_nostag(ips:min(ide-1,ipe),jps:min(jde-1,jpe)) = 0
   grid%imask_xstag(ips:ipe,jps:min(jde-1,jpe)) = 0
   grid%imask_ystag(ips:min(ide-1,ipe),jps:jpe) = 0
   grid%imask_xystag(ips:ipe,jps:jpe) = 0

! shift the nest domain in x
   do ii = 1,abs(disp_x)
#include "SHIFT_HALO.inc"
#include "../frame/loop_based_x_shift_code.h"
   enddo

   CALL shift_domain_em2 ( grid , disp_x, disp_y &
!
# include "dummy_new_args.inc"
!
                           )

#endif

END SUBROUTINE shift_domain_em

SUBROUTINE shift_domain_em2 ( grid , disp_x, disp_y &
!
# include "dummy_new_args.inc"
!
                           )
   USE module_state_description
   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_domain_type, ONLY : fieldlist
   USE module_timing
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
#ifdef DM_PARALLEL
   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, itrace
   USE module_comm_dm, ONLY : SHIFT_HALO_sub
#else
   USE module_dm
#endif
   IMPLICIT NONE
  ! Arguments
   INTEGER disp_x, disp_y       ! number of parent domain points to move
   TYPE(domain) , POINTER                     :: grid
  ! Local 
   INTEGER i, j, ii, jpf
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
   TYPE (grid_config_rec_type)  :: config_flags
   TYPE( fieldlist ), POINTER :: p

   !  Definitions of dummy arguments to solve
#include "dummy_new_decl.inc"

#ifdef MOVE_NESTS

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   px = isign(config_flags%parent_grid_ratio,disp_x)
   py = isign(config_flags%parent_grid_ratio,disp_y)

! shift the nest domain in y
   do ii = 1,abs(disp_y)
#include "SHIFT_HALO.inc"
#include "../frame/loop_based_y_shift_code.h"
   enddo

#endif
END SUBROUTINE shift_domain_em2

