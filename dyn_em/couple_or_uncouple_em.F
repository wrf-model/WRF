!WRF:MEDIATION_LAYER:couple_uncouple_utility

SUBROUTINE couple_or_uncouple_em ( grid , config_flags , couple, &
!
#include "em_dummy_args.inc"
!
                 )


!  #undef DM_PARALLEL

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
! Mediation layer modules
! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   !  Subroutine interface block.

   TYPE(domain) , TARGET         :: grid

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   !  WRF state bcs
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   LOGICAL, INTENT(   IN) :: couple

   ! Local data

   INTEGER                         :: k_start , k_end
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

   INTEGER                         :: i,j,k, im
   INTEGER                         :: num_3d_c, num_3d_m
   REAL                            :: mu_factor

   REAL, DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33) :: mut_2, muut_2, muvt_2, muwt_2

!  De-reference dimension information stored in the grid data structure.

  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

   num_3d_m        = num_moist
   num_3d_c        = num_chem

   !  couple or uncouple mass-point variables
   !  first, compute mu or its reciprical as necessary

!   write(6,*) ' in couple '
!   write(6,*) ' x,y memory ', grid%sm31,grid%em31,grid%sm33,grid%em33
!   write(6,*) ' x,y patch ', ips, ipe, jps, jpe


!   if(couple) then
!      write(6,*) ' coupling variables for grid ',grid%id
!      write(6,*) ' ips, ipe, jps, jpe ',ips,ipe,jps,jpe
!   else
!      write(6,*) ' uncoupling variables for grid ',grid%id
!      write(6,*) ' ips, ipe, jps, jpe ',ips,ipe,jps,jpe
!      write(6,*) ' x, y, size ',size(mu_2,1),size(mu_2,2)
!   end if

#ifdef DM_PARALLEL
#      include <em_data_calls.inc>
#endif

#ifdef DM_PARALLEL
# include "HALO_EM_COUPLE_A.inc"
#endif

   !  computations go out one row and column to avoid having to communicate before solver

   IF( couple ) THEN

!     write(6,*) ' coupling: setting mu arrays '

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       mut_2(i,j) = mub(i,j) + mu_2(i,j)
       muwt_2(i,j) = (mub(i,j) + mu_2(i,j))/msft(i,j)
     ENDDO
     ENDDO

!  need boundary condition fixes for u and v ???

!     write(6,*) ' coupling: setting muv and muv arrays '

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muut_2(i,j) = 0.5*(mub(i,j)+mub(i-1,j) + mu_2(i,j) + mu_2(i-1,j))/msfu(i,j)
       muvt_2(i,j) = 0.5*(mub(i,j)+mub(i,j-1) + mu_2(i,j) + mu_2(i,j-1))/msfv(i,j)
     ENDDO
     ENDDO

     IF ( config_flags%nested .or. config_flags%specified ) THEN

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = (mub(i,j-1) + mu_2(i,j-1))/msfv(i,j)
         ENDDO
       ENDIF
       IF ( ipe .eq. ide ) THEN
         i = ide
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = (mub(i-1,j) + mu_2(i-1,j))/msfu(i,j)
         ENDDO
       ENDIF

     ELSE

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = 0.5*(mub(i,j)+mub(i,j-1) + mu_2(i,j) + mu_2(i,j-1))/msfv(i,j)
         ENDDO
       ENDIF
       IF ( ipe .eq. ide ) THEN
         i = ide       
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = 0.5*(mub(i,j)+mub(i-1,j) + mu_2(i,j) + mu_2(i-1,j))/msfu(i,j)
         ENDDO
       ENDIF

     END IF

   ELSE
   
!     write(6,*) ' uncoupling: setting mu arrays '

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       mut_2(i,j) = 1./(mub(i,j) + mu_2(i,j))
       muwt_2(i,j) = msft(i,j)/(mub(i,j) + mu_2(i,j))
     ENDDO
     ENDDO

!     write(6,*) ' uncoupling: setting muv arrays '

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muut_2(i,j) = 2.*msfu(i,j)/(mub(i,j)+mub(i-1,j) + mu_2(i,j) + mu_2(i-1,j))
     ENDDO
     ENDDO

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muvt_2(i,j) = 2.*msfv(i,j)/(mub(i,j)+mub(i,j-1) + mu_2(i,j) + mu_2(i,j-1))
     ENDDO
     ENDDO

     IF ( config_flags%nested .or. config_flags%specified ) THEN

       IF ( jpe .eq. jde ) THEN
         j = jde 
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = msfv(i,j)/(mub(i,j-1) + mu_2(i,j-1))
         ENDDO
       ENDIF
       IF ( ipe .eq. ide ) THEN
         i = ide
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = msfu(i,j)/(mub(i-1,j) + mu_2(i-1,j))
         ENDDO
       ENDIF

     ELSE

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = 2.*msfv(i,j)/(mub(i,j)+mub(i,j-1) + mu_2(i,j) + mu_2(i,j-1))
         ENDDO
       ENDIF
       IF ( ipe .eq. ide ) THEN
         i = ide       
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = 2.*msfu(i,j)/(mub(i,j)+mub(i-1,j) + mu_2(i,j) + mu_2(i-1,j))
         ENDDO
       ENDIF

     END IF

   END IF

   !  couple/uncouple mu point variables

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k,im )
   DO j = max(jds,jps),min(jde-1,jpe)

     DO k = kps,kpe
     DO i = max(ids,ips),min(ide-1,ipe)
       ph_2(i,k,j) = ph_2(i,k,j)*mut_2(i,j)
       w_2(i,k,j)  =  w_2(i,k,j)*muwt_2(i,j)
     ENDDO
     ENDDO

     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide-1,ipe)
       t_2(i,k,j)  =  t_2(i,k,j)*mut_2(i,j)
     ENDDO
     ENDDO

     IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_3d_m
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           moist_2(i,k,j,im)  =  moist_2(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

     IF (num_3d_c >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_3d_c
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           chem_2(i,k,j,im)  =  chem_2(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

!  do u and v

     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide,ipe)
       u_2(i,k,j)  =  u_2(i,k,j)*muut_2(i,j)
     ENDDO
     ENDDO

   ENDDO   ! j loop

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k )
   DO j = max(jds,jps),min(jde,jpe)
     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide-1,ipe)
       v_2(i,k,j)  =  v_2(i,k,j)*muvt_2(i,j)
     ENDDO
     ENDDO
   ENDDO

#ifdef DM_PARALLEL
# include "HALO_EM_COUPLE_B.inc"
#endif

!     write(6,*) ' coupling: finished '

END SUBROUTINE couple_or_uncouple_em

LOGICAL FUNCTION em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff

   ioff = 0 ; joff = 0 
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1

   em_cd_feedback_mask = ( pig .ge. ips_save+1        .and.      &
                           pjg .ge. jps_save+1        .and.      &
                           pig .le. ipe_save-1  +ioff .and.      &
                           pjg .le. jpe_save-1  +joff           )


END FUNCTION em_cd_feedback_mask

