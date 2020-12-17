subroutine da_transfer_wrftl_lbc_t0_adj (grid)

   !---------------------------------------------------------------------------
   ! Purpose: Adjoint of WRF boundary for LBC at T=0
   ! All adjoint of tendencies variable are kept , because they will be used in T6 calculation. 
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid

#ifdef VAR4D

   ! Local variables

   integer :: i, j, k, ii, jj, spec_bdy_width

   if (trace_use) call da_trace_entry("da_transfer_wrftl_lbc_t0_adj")

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

   spec_bdy_width = grid%spec_bdy_width

!  CALL a_stuff_bdytend_old ( wbdy3dtemp1 , REAL(interval_seconds) ,                 &
!                             model_grid%a_w_btxs, model_grid%a_w_btxe,     &
!                             model_grid%a_w_btys, model_grid%a_w_btye,     &
!                             'W' , &
!                             spec_bdy_width      , &
!                             grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                             grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                             grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_old ( ubdy3dtemp1 , REAL(interval_seconds) ,                 &
                              model_grid%a_u_btxs, model_grid%a_u_btxe,     &
                              model_grid%a_u_btys, model_grid%a_u_btye,     &
                              'U' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_old ( vbdy3dtemp1 , REAL(interval_seconds) ,                 &
                              model_grid%a_v_btxs, model_grid%a_v_btxe,     &
                              model_grid%a_v_btys, model_grid%a_v_btye,     &
                              'V' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_old ( tbdy3dtemp1 , REAL(interval_seconds) ,                 &
                              model_grid%a_t_btxs, model_grid%a_t_btxe,     &
                              model_grid%a_t_btys, model_grid%a_t_btye,     &
                              'T' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_old ( pbdy3dtemp1 , REAL(interval_seconds) ,                 &
                              model_grid%a_ph_btxs, model_grid%a_ph_btxe,     &
                              model_grid%a_ph_btys, model_grid%a_ph_btye,     &
                              'W' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_old ( qbdy3dtemp1 , REAL(interval_seconds) ,                 &
                              model_grid%a_moist_btxs(:,:,:,P_A_QV), model_grid%a_moist_btxe(:,:,:,P_A_QV),     &
                              model_grid%a_moist_btys(:,:,:,P_A_QV), model_grid%a_moist_btye(:,:,:,P_A_QV),     &
                              'T' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_old ( mbdy2dtemp1 , REAL(interval_seconds) ,                 &
                              model_grid%a_mu_btxs, model_grid%a_mu_btxe,     &
                              model_grid%a_mu_btys, model_grid%a_mu_btye,     &
                              'M' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, 1, 1, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, 1, 1, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, 1, 1 )

   CALL a_stuff_bdy ( qbdy3dtemp1 , model_grid%a_moist_bxs(:,:,:,P_A_QV), model_grid%a_moist_bxe(:,:,:,P_A_QV),     &
                                    model_grid%a_moist_bys(:,:,:,P_A_QV), model_grid%a_moist_bye(:,:,:,P_A_QV),     &
                                    'T' , spec_bdy_width      ,               &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdy ( mbdy2dtemp1 , model_grid%a_mu_bxs, model_grid%a_mu_bxe, model_grid%a_mu_bys, model_grid%a_mu_bye,    &
                                    'M' , spec_bdy_width      ,               &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, 1, 1, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, 1, 1, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, 1, 1 )

   CALL a_stuff_bdy ( ubdy3dtemp1 , model_grid%a_u_bxs, model_grid%a_u_bxe, model_grid%a_u_bys, model_grid%a_u_bye, &
                                    'U' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdy ( vbdy3dtemp1 , model_grid%a_v_bxs, model_grid%a_v_bxe, model_grid%a_v_bys, model_grid%a_v_bye, &
                                    'V' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdy ( tbdy3dtemp1 , model_grid%a_t_bxs, model_grid%a_t_bxe, model_grid%a_t_bys, model_grid%a_t_bye, &
                                    'T' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdy ( pbdy3dtemp1 , model_grid%a_ph_bxs, model_grid%a_ph_bxe, model_grid%a_ph_bys, model_grid%a_ph_bye, &
                                    'W' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

!  CALL a_stuff_bdy ( wbdy3dtemp1 , model_grid%a_w_bxs, model_grid%a_w_bxe, model_grid%a_w_bys, model_grid%a_w_bye, &
!                                   'W' , spec_bdy_width      , &
!                                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

#ifdef DM_PARALLEL
#include "HALO_EM_E.inc"
#endif

!  CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , wbdy3dtemp1 , grid%w_2, grid%a_w_2, 'h' , grid%msfty , &
!                                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , ubdy3dtemp1 , grid%u_2 , grid%a_u_2 , 'u' , grid%msfuy , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , vbdy3dtemp1 , grid%v_2 , grid%a_v_2 , 'v' , grid%msfvx , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , tbdy3dtemp1 , grid%t_2 , grid%a_t_2 , 't' , grid%msfty , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , pbdy3dtemp1 , grid%ph_2, grid%a_ph_2, 'h' , grid%msfty , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , qbdy3dtemp1 , grid%moist(:,:,:,P_QV), grid%a_moist(:,:,:,P_QV),  't' , grid%msfty , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   DO j = grid%sp32 , MIN(grid%ed32-1,grid%ep32)
      DO i = grid%sp31 , MIN(grid%ed31-1,grid%ep31)
         grid%a_mu_2(i,j) = grid%a_mu_2(i,j) + mbdy2dtemp1(i,1,j)
         mbdy2dtemp1(i,1,j) = 0.0
      END DO
   END DO

#ifdef DM_PARALLEL
   call da_halo_em_e_ad ( grid%a_mu_2 )
#endif

   DEALLOCATE ( ubdy3dtemp1 )
   DEALLOCATE ( vbdy3dtemp1 )
   DEALLOCATE ( tbdy3dtemp1 )
   DEALLOCATE ( pbdy3dtemp1 )
   DEALLOCATE ( qbdy3dtemp1 )
   DEALLOCATE ( mbdy2dtemp1 )
!  DEALLOCATE ( wbdy3dtemp1 )

   model_grid%a_w_bxs = 0.0
   model_grid%a_w_bxe = 0.0
   model_grid%a_w_bys = 0.0
   model_grid%a_w_bye = 0.0

   ! If no var4d_lbc , the adjoint of tendencies will be reset to ZERO
   if ( .not. var4d_lbc ) then
      model_grid%a_u_btxs = 0.0
      model_grid%a_u_btxe = 0.0
      model_grid%a_u_btys = 0.0
      model_grid%a_u_btye = 0.0
      model_grid%a_v_btxs = 0.0
      model_grid%a_v_btxe = 0.0
      model_grid%a_v_btys = 0.0
      model_grid%a_v_btye = 0.0
      model_grid%a_w_btxs = 0.0
      model_grid%a_w_btxe = 0.0
      model_grid%a_w_btys = 0.0
      model_grid%a_w_btye = 0.0
      model_grid%a_ph_btxs = 0.0
      model_grid%a_ph_btxe = 0.0
      model_grid%a_ph_btys = 0.0
      model_grid%a_ph_btye = 0.0
      model_grid%a_mu_btxs = 0.0
      model_grid%a_mu_btxe = 0.0
      model_grid%a_mu_btys = 0.0
      model_grid%a_mu_btye = 0.0
      model_grid%a_moist_btxs = 0.0
      model_grid%a_moist_btxe = 0.0
      model_grid%a_moist_btys = 0.0
      model_grid%a_moist_btye = 0.0
   endif

   if (trace_use) call da_trace_exit("da_transfer_wrftl_lbc_t0_adj")

#endif
end subroutine da_transfer_wrftl_lbc_t0_adj


