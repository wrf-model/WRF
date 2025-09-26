subroutine da_transfer_wrftl_lbc_t0 (grid)

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into WRFTL increments for LBC at T=0 
   !           (following xatowrf)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid

#ifdef VAR4D

   integer :: i, j, k, ii, jj
   integer :: spec_bdy_width

   if (trace_use) call da_trace_entry("da_transfer_wrftl_lbc_t0")

   IF ( ALLOCATED ( ubdy3dtemp1 ) ) DEALLOCATE ( ubdy3dtemp1 )
   IF ( ALLOCATED ( vbdy3dtemp1 ) ) DEALLOCATE ( vbdy3dtemp1 )
   IF ( ALLOCATED ( tbdy3dtemp1 ) ) DEALLOCATE ( tbdy3dtemp1 )
   IF ( ALLOCATED ( pbdy3dtemp1 ) ) DEALLOCATE ( pbdy3dtemp1 )
   IF ( ALLOCATED ( qbdy3dtemp1 ) ) DEALLOCATE ( qbdy3dtemp1 )
   IF ( ALLOCATED ( mbdy2dtemp1 ) ) DEALLOCATE ( mbdy2dtemp1 )
!  IF ( ALLOCATED ( wbdy3dtemp1 ) ) DEALLOCATE ( wbdy3dtemp1 )

   ALLOCATE ( ubdy3dtemp1(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( vbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( tbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( pbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( qbdy3dtemp1(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( mbdy2dtemp1(ims:ime,1:1,    jms:jme) )
!  ALLOCATE ( wbdy3dtemp1(ims:ime,jms:jme,kms:kme) )

   ubdy3dtemp1 = 0.0
   vbdy3dtemp1 = 0.0
   tbdy3dtemp1 = 0.0
   pbdy3dtemp1 = 0.0
   qbdy3dtemp1 = 0.0
   mbdy2dtemp1 = 0.0
!  wbdy3dtemp1 = 0.0

   spec_bdy_width =  grid%spec_bdy_width

#ifdef DM_PARALLEL
#include "HALO_EM_E_TL.inc"
#endif

   CALL g_couple ( model_config_flags, grid%mu_2 , grid%g_mu_2, grid%mub , qbdy3dtemp1 , grid%moist(:,:,:,P_G_QV), grid%g_moist(:,:,:,P_G_QV),  't' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_couple ( model_config_flags, grid%mu_2 , grid%g_mu_2, grid%mub , tbdy3dtemp1 , grid%t_2 , grid%g_t_2 , 't' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_couple ( model_config_flags, grid%mu_2 , grid%g_mu_2, grid%mub , pbdy3dtemp1 , grid%ph_2, grid%g_ph_2, 'h' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_couple ( model_config_flags, grid%mu_2 , grid%g_mu_2, grid%mub , ubdy3dtemp1 , grid%u_2 , grid%g_u_2 , 'u' , grid%msfuy , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_couple ( model_config_flags, grid%mu_2 , grid%g_mu_2, grid%mub , vbdy3dtemp1 , grid%v_2 , grid%g_v_2 , 'v' , grid%msfvx , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

!  CALL g_couple ( model_config_flags, grid%mu_2 , grid%g_mu_2, grid%mub , wbdy3dtemp1 , grid%w_2, grid%g_w_2, 'h' , grid%msfty , &
!                  grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                  grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                  grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   DO j = grid%sp32 , MIN(grid%ed32-1,grid%ep32)
      DO i = grid%sp31 , MIN(grid%ed31-1,grid%ep31)
         mbdy2dtemp1(i,1,j) = grid%g_mu_2(i,j)
      END DO
   END DO

   CALL g_stuff_bdy ( qbdy3dtemp1 , model_grid%g_moist_bxs(:,:,:,P_G_QV), model_grid%g_moist_bxe(:,:,:,P_G_QV),     &
                                    model_grid%g_moist_bys(:,:,:,P_G_QV), model_grid%g_moist_bye(:,:,:,P_G_QV),     &
                                    'T' , spec_bdy_width      ,               &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_stuff_bdy ( mbdy2dtemp1 , model_grid%g_mu_bxs, model_grid%g_mu_bxe, model_grid%g_mu_bys, model_grid%g_mu_bye,    &
                                    'M' , spec_bdy_width      ,               &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, 1, 1, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, 1, 1, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, 1, 1 )

   CALL g_stuff_bdy ( ubdy3dtemp1 , model_grid%g_u_bxs, model_grid%g_u_bxe, model_grid%g_u_bys, model_grid%g_u_bye, &
                                    'U' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_stuff_bdy ( vbdy3dtemp1 , model_grid%g_v_bxs, model_grid%g_v_bxe, model_grid%g_v_bys, model_grid%g_v_bye, &
                                    'V' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_stuff_bdy ( tbdy3dtemp1 , model_grid%g_t_bxs, model_grid%g_t_bxe, model_grid%g_t_bys, model_grid%g_t_bye, &
                                    'T' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_stuff_bdy ( pbdy3dtemp1 , model_grid%g_ph_bxs, model_grid%g_ph_bxe, model_grid%g_ph_bys, model_grid%g_ph_bye, &
                                    'W' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

!  CALL g_stuff_bdy ( wbdy3dtemp1 , model_grid%g_w_bxs, model_grid%g_w_bxe, model_grid%g_w_bys, model_grid%g_w_bye, &
!                                   'W' , spec_bdy_width      , &
!                                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   model_grid%g_w_bxs = 0.0
   model_grid%g_w_bxe = 0.0
   model_grid%g_w_bys = 0.0
   model_grid%g_w_bye = 0.0

   model_grid%g_w_btxs = 0.0
   model_grid%g_w_btxe = 0.0
   model_grid%g_w_btys = 0.0
   model_grid%g_w_btye = 0.0

   if (trace_use) call da_trace_exit("da_transfer_wrftl_lbc_t0")

#endif
end subroutine da_transfer_wrftl_lbc_t0


