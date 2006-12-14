SUBROUTINE shift_domain_em ( grid , disp_x, disp_y &
!
# include <em_dummy_new_args.inc>
!
                           )
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_dm
   USE module_timing
   IMPLICIT NONE
  ! Arguments
   INTEGER disp_x, disp_y       ! number of parent domain points to move
   TYPE(domain) , POINTER                     :: grid
  ! Local 
   INTEGER i, j, ii
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   TYPE (grid_config_rec_type)  :: config_flags

   INTERFACE
       ! need to split this routine to avoid clobbering certain widely used compilers
       SUBROUTINE shift_domain_em2 ( grid , disp_x, disp_y &
!
# include <em_dummy_new_args.inc>
!
                           )
          USE module_domain
          USE module_timing
          USE module_configure
          USE module_dm
          USE module_timing
          IMPLICIT NONE
         ! Arguments
          INTEGER disp_x, disp_y       ! number of parent domain points to move
          TYPE(domain) , POINTER                     :: grid
          TYPE (grid_config_rec_type)  :: config_flags

          !  Definitions of dummy arguments to solve
#include <em_dummy_new_decl.inc>
       END SUBROUTINE shift_domain_em2
   END INTERFACE

   !  Definitions of dummy arguments to solve
#include <em_dummy_new_decl.inc>

#ifdef MOVE_NESTS
#ifdef DM_PARALLEL
#      include <em_data_calls.inc>
#endif

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
#include <em_shift_halo_x.inc>
   enddo

   CALL shift_domain_em2 ( grid , disp_x, disp_y &
!
# include <em_dummy_new_args.inc>
!
                           )

#endif

END SUBROUTINE shift_domain_em

SUBROUTINE shift_domain_em2 ( grid , disp_x, disp_y &
!
# include <em_dummy_new_args.inc>
!
                           )
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_dm
   USE module_timing
   IMPLICIT NONE
  ! Arguments
   INTEGER disp_x, disp_y       ! number of parent domain points to move
   TYPE(domain) , POINTER                     :: grid
  ! Local 
   INTEGER i, j, ii
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   TYPE (grid_config_rec_type)  :: config_flags

   !  Definitions of dummy arguments to solve
#include <em_dummy_new_decl.inc>

#ifdef MOVE_NESTS

#ifdef DM_PARALLEL
#      include <em_data_calls.inc>
#endif

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   px = isign(config_flags%parent_grid_ratio,disp_x)
   py = isign(config_flags%parent_grid_ratio,disp_y)

! shift the nest domain in y
   do ii = 1,abs(disp_y)
#include <em_shift_halo_y.inc>
   enddo

#endif
END SUBROUTINE shift_domain_em2

