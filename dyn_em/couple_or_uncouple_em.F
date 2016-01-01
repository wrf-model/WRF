!WRF:MEDIATION_LAYER:couple_uncouple_utility

SUBROUTINE couple_or_uncouple_em ( grid , config_flags , couple &
!
#include "dummy_new_args.inc"
!
                 )


!  #undef DM_PARALLEL

! Driver layer modules
   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_configure, ONLY : grid_config_rec_type
   USE module_driver_constants
   USE module_machine
   USE module_tiles
#ifdef DM_PARALLEL
   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic
   USE module_comm_dm, ONLY : halo_em_couple_a_sub,halo_em_couple_b_sub,period_em_couple_a_sub,period_em_couple_b_sub
#else
   USE module_dm
#endif
   USE module_bc
! Mediation layer modules
! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   !  Subroutine interface block.

   TYPE(domain) , TARGET         :: grid

   !  Definitions of dummy arguments to solve
#include "dummy_new_decl.inc"

   !  WRF state bcs
   TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

   LOGICAL, INTENT(   IN) :: couple

   ! Local data

   INTEGER                         :: k_start , k_end
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

   INTEGER                         :: i,j,k, im
   INTEGER                         :: num_3d_c, num_3d_m, num_3d_s
   REAL                            :: mu_factor

   REAL, DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33) :: mut_2, muut_2, muvt_2, muwt_2

!  De-reference dimension information stored in the grid data structure.
   IF ( .NOT. grid%active_this_task ) RETURN

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   num_3d_m        = num_moist
   num_3d_c        = num_chem
   num_3d_s        = num_scalar

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


   IF ( config_flags%periodic_x .OR. config_flags%periodic_y ) THEN
     CALL set_physical_bc2d( grid%mub, 't',  &
                             config_flags,           &
                             ids,ide, jds,jde,   & ! domain dims
                             ims,ime, jms,jme,   & ! memory dims
                             ips,ipe, jps,jpe,   & ! patch  dims
                             ips,ipe, jps,jpe   )
     CALL set_physical_bc2d( grid%mu_1, 't',  &
                             config_flags,           &
                             ids,ide, jds,jde,   & ! domain dims
                             ims,ime, jms,jme,   & ! memory dims
                             ips,ipe, jps,jpe,   & ! patch  dims
                             ips,ipe, jps,jpe   )
     CALL set_physical_bc2d( grid%mu_2, 't',  &
                             config_flags,           &
                             ids,ide, jds,jde,   & ! domain dims
                             ims,ime, jms,jme,   & ! memory dims
                             ips,ipe, jps,jpe,   & ! patch  dims
                             ips,ipe, jps,jpe   )
   ENDIF


#ifdef DM_PARALLEL
# include "HALO_EM_COUPLE_A.inc"
# include "PERIOD_EM_COUPLE_A.inc"
#endif

   !  computations go out one row and column to avoid having to communicate before solver

   IF( couple ) THEN

!     write(6,*) ' coupling: setting mu arrays '

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       mut_2(i,j) = grid%mub(i,j) + grid%mu_2(i,j)
       muwt_2(i,j) = (grid%mub(i,j) + grid%mu_2(i,j))/grid%msfty(i,j) ! w coupled with y
     ENDDO
     ENDDO

!  need boundary condition fixes for u and v ???

!     write(6,*) ' coupling: setting muv and muv arrays '

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muut_2(i,j) = 0.5*(grid%mub(i,j)+grid%mub(i-1,j) + grid%mu_2(i,j) + grid%mu_2(i-1,j))/grid%msfuy(i,j) ! u coupled with y
       muvt_2(i,j) = 0.5*(grid%mub(i,j)+grid%mub(i,j-1) + grid%mu_2(i,j) + grid%mu_2(i,j-1))/grid%msfvx(i,j) ! v coupled with x
     ENDDO
     ENDDO

     IF ( config_flags%nested .or. config_flags%specified .or. config_flags%polar ) THEN

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = (grid%mub(i,j-1) + grid%mu_2(i,j-1))/grid%msfvx(i,j) ! v coupled with x
         ENDDO
       ENDIF
       IF ( ipe .eq. ide .AND. .NOT. config_flags%periodic_x ) THEN
         i = ide
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = (grid%mub(i-1,j) + grid%mu_2(i-1,j))/grid%msfuy(i,j) ! u coupled with y
         ENDDO
       ENDIF

     ELSE

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = 0.5*(grid%mub(i,j)+grid%mub(i,j-1) + grid%mu_2(i,j) + grid%mu_2(i,j-1))/grid%msfvx(i,j) ! v coupled with x
         ENDDO
       ENDIF
       IF ( ipe .eq. ide ) THEN
         i = ide       
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = 0.5*(grid%mub(i,j)+grid%mub(i-1,j) + grid%mu_2(i,j) + grid%mu_2(i-1,j))/grid%msfuy(i,j) ! u coupled with y
         ENDDO
       ENDIF

     END IF

   ELSE
   
!     write(6,*) ' uncoupling: setting mu arrays '

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       mut_2(i,j) = 1./(grid%mub(i,j) + grid%mu_2(i,j))
       muwt_2(i,j) = grid%msfty(i,j)/(grid%mub(i,j) + grid%mu_2(i,j)) ! w coupled with y
     ENDDO
     ENDDO

!     write(6,*) ' uncoupling: setting muv arrays '

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muut_2(i,j) = 2.*grid%msfuy(i,j)/(grid%mub(i,j)+grid%mub(i-1,j) + grid%mu_2(i,j) + grid%mu_2(i-1,j)) ! u coupled with y
     ENDDO
     ENDDO

     DO j = max(jds,jps),min(jde-1,jpe)
     DO i = max(ids,ips),min(ide-1,ipe)
       muvt_2(i,j) = 2.*grid%msfvx(i,j)/(grid%mub(i,j)+grid%mub(i,j-1) + grid%mu_2(i,j) + grid%mu_2(i,j-1)) ! v coupled with x
     ENDDO
     ENDDO

     IF ( config_flags%nested .or. config_flags%specified .or. config_flags%polar ) THEN

       IF ( jpe .eq. jde ) THEN
         j = jde 
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = grid%msfvx(i,j)/(grid%mub(i,j-1) + grid%mu_2(i,j-1)) ! v coupled with x
         ENDDO
       ENDIF
       IF ( ipe .eq. ide .AND. .NOT. config_flags%periodic_x ) THEN
         i = ide
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = grid%msfuy(i,j)/(grid%mub(i-1,j) + grid%mu_2(i-1,j)) ! u coupled with y
         ENDDO
       ENDIF

     ELSE

       IF ( jpe .eq. jde ) THEN
         j = jde
         DO i = max(ids,ips),min(ide-1,ipe)
           muvt_2(i,j) = 2.*grid%msfvx(i,j)/(grid%mub(i,j)+grid%mub(i,j-1) + grid%mu_2(i,j) + grid%mu_2(i,j-1)) ! v coupled with x
         ENDDO
       ENDIF
       IF ( ipe .eq. ide ) THEN
         i = ide       
         DO j = max(jds,jps),min(jde-1,jpe)
           muut_2(i,j) = 2.*grid%msfuy(i,j)/(grid%mub(i,j)+grid%mub(i-1,j) + grid%mu_2(i,j) + grid%mu_2(i-1,j)) ! u coupled with y
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
       grid%ph_2(i,k,j) = grid%ph_2(i,k,j)*mut_2(i,j)
       grid%w_2(i,k,j)  =  grid%w_2(i,k,j)*muwt_2(i,j)
     ENDDO
     ENDDO

     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide-1,ipe)
       grid%t_2(i,k,j)  =  grid%t_2(i,k,j)*mut_2(i,j)
     ENDDO
     ENDDO

     IF (num_3d_m >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_3d_m
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           moist(i,k,j,im)  =  moist(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

     IF (num_3d_c >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_3d_c
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           chem(i,k,j,im)  =  chem(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

     IF (num_3d_s >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_3d_s
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           scalar(i,k,j,im)  =  scalar(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

     IF (num_tracer >= PARAM_FIRST_SCALAR )  THEN
       DO im = PARAM_FIRST_SCALAR, num_tracer
         DO k = kps,kpe-1
         DO i = max(ids,ips),min(ide-1,ipe)
           tracer(i,k,j,im)  =  tracer(i,k,j,im)*mut_2(i,j)
         ENDDO
         ENDDO
       ENDDO
     END IF

!  do u and v

     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide,ipe)
       grid%u_2(i,k,j)  =  grid%u_2(i,k,j)*muut_2(i,j)
     ENDDO
     ENDDO

   ENDDO   ! j loop
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( i,j,k )
   DO j = max(jds,jps),min(jde,jpe)
     DO k = kps,kpe-1
     DO i = max(ids,ips),min(ide-1,ipe)
       grid%v_2(i,k,j)  =  grid%v_2(i,k,j)*muvt_2(i,j)
     ENDDO
     ENDDO
   ENDDO
   !$OMP END PARALLEL DO

   IF ( config_flags%periodic_x .OR. config_flags%periodic_y ) THEN
     CALL set_physical_bc3d( grid%ph_1, 'w',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%ph_2, 'w',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%w_1, 'w',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%w_2, 'w',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%t_1, 't',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%t_2, 't',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%u_1, 'u',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%u_2, 'u',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%v_1, 'v',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     CALL set_physical_bc3d( grid%v_2, 'v',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )

     IF (num_3d_m >= PARAM_FIRST_SCALAR) THEN
       DO im = PARAM_FIRST_SCALAR , num_3d_m

     CALL set_physical_bc3d( moist(ims,kms,jms,im), 'p',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
       ENDDO
     ENDIF


     IF (num_3d_c >= PARAM_FIRST_SCALAR) THEN
       DO im = PARAM_FIRST_SCALAR , num_3d_c

     CALL set_physical_bc3d( chem(ims,kms,jms,im), 'p',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     ENDDO
     ENDIF

     IF (num_3d_s >= PARAM_FIRST_SCALAR) THEN
       DO im = PARAM_FIRST_SCALAR , num_3d_s

     CALL set_physical_bc3d( scalar(ims,kms,jms,im), 'p',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     ENDDO
     ENDIF

     IF (num_tracer >= PARAM_FIRST_SCALAR) THEN
       DO im = PARAM_FIRST_SCALAR , num_tracer
 
     CALL set_physical_bc3d( tracer(ims,kms,jms,im), 'p',        &
                             config_flags,                   &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             ips,ipe, jps,jpe, kps,kpe )
     ENDDO
     ENDIF

   ENDIF

#ifdef DM_PARALLEL
# include "HALO_EM_COUPLE_B.inc"
# include "PERIOD_EM_COUPLE_B.inc"
#endif

END SUBROUTINE couple_or_uncouple_em

LOGICAL FUNCTION cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, xstag, ystag )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: pig, ips_save, ipe_save , pjg, jps_save, jpe_save
   LOGICAL, INTENT(IN) :: xstag, ystag

   INTEGER ioff, joff, spec_zone

   CALL nl_get_spec_zone( 1, spec_zone )
   ioff = 0 ; joff = 0 
   IF ( xstag  ) ioff = 1
   IF ( ystag  ) joff = 1

   cd_feedback_mask = ( pig .ge. ips_save+spec_zone        .and.      &
                           pjg .ge. jps_save+spec_zone        .and.      &
                           pig .le. ipe_save-spec_zone  +ioff .and.      &
                           pjg .le. jpe_save-spec_zone  +joff           )


END FUNCTION cd_feedback_mask

