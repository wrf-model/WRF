SUBROUTINE shift_domain_nmm ( grid , disp_x, disp_y &
!
# include "dummy_new_args.inc"
!
                           )
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_dm
   USE module_comm_dm
   USE module_timing
   IMPLICIT NONE
  ! Arguments
   INTEGER disp_x, disp_y       ! number of parent domain points to move
   TYPE(domain) , POINTER                     :: grid

  ! Local
   INTEGER  :: i, j, ii, ipf, jpf
   INTEGER  :: px, py       ! number and direction of nd points to move
   INTEGER  :: ids , ide , jds , jde , kds , kde , &
               ims , ime , jms , jme , kms , kme , &
               ips , ipe , jps , jpe , kps , kpe
   TYPE (grid_config_rec_type)  :: config_flags
   TYPE( fieldlist ), POINTER :: p

   LOGICAL :: E_BDY,N_BDY,S_BDY,W_BDY

   CHARACTER(LEN=255) :: message

   !  Definitions of dummy arguments to solve
#include "dummy_new_decl.inc"

IF ( grid%active_this_task ) THEN

#ifdef DM_PARALLEL
#      include "data_calls.inc"
#endif

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL get_ijk_from_grid (  grid ,                           &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe     )


   S_BDY=(JPS==JDS)
   N_BDY=(JPE==JDE)
   W_BDY=(IPS==IDS)
   E_BDY=(IPE==IDE)

   write(message,*)' S_BDY,N_BDY,W_BDY,E_BDY ', S_BDY,N_BDY,W_BDY,E_BDY
   CALL wrf_message(trim(message))

   grid%imask_nostag=0
#if 1
   IF ( disp_x > 0 ) THEN
      IF ( E_BDY ) THEN 
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe-2-mod(j+1,2)) 
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
!
   IF ( disp_y > 0 ) THEN
      IF ( N_BDY ) THEN
         DO J=min(jde-1,jpe-2),max(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 0
         END DO
         END DO
      ENDIF
   ELSEIF ( disp_y < 0 ) THEN
      IF ( S_BDY ) THEN
         DO J=jps,jps+1
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 0
         END DO
         END DO
      ENDIF
   ENDIF      !disp_y
!      
   ELSEIF ( disp_x < 0 ) THEN
      IF ( W_BDY ) THEN 
         DO J=jps,min(jde-1,jpe)
         DO I=ips+1,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
!
   IF ( disp_y > 0 ) THEN
      IF ( N_BDY ) THEN
         DO J=min(jde-1,jpe-2),max(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 0
         END DO
         END DO
      ENDIF
   ELSEIF ( disp_y < 0 ) THEN
      IF ( S_BDY ) THEN
         DO J=jps,jps+1
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 0
         END DO
         END DO
      ENDIF
   ENDIF      !disp_y
!      
   ELSE                            ! disp_x = 0
!
   IF ( disp_y > 0 ) THEN
      IF ( N_BDY ) THEN 
         DO J=jps,min(jde-1,jpe-3)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
   END IF
   IF ( disp_y < 0 ) THEN
      IF ( S_BDY ) THEN
         DO J=jps+2,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
   END IF
!
   END IF
!

#else
    grid%imask_nostag(ips:min(ide-4,ipe),jps:min(jde-1,jpe)) = 1
!   grid%imask_nostag(ips+1:min(ide-2,ipe),jps+1:min(jde-2,jpe)) = 1
!   grid%imask_nostag(ips+1:min(ide-1,ipe-1),jps+2:min(jde-1,jpe-2)) = 1
#endif

   px = isign(grid%parent_grid_ratio,disp_x)
   py = isign(grid%parent_grid_ratio,disp_y)

#ifdef DM_PARALLEL
! shift the nest domain in x
   do ii = 1,abs(disp_x)
#include "../inc/SHIFT_HALO_X_HALO.inc"
#include "../frame/loop_based_x_shift_code.h"
   enddo

! shift the nest domain in y
   do ii = 1,abs(disp_y)
#include "../inc/SHIFT_HALO_Y_HALO.inc"
#include "../frame/loop_based_y_shift_code.h"
   enddo
#endif

ENDIF

   RETURN

END SUBROUTINE shift_domain_nmm
