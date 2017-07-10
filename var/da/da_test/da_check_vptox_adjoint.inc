subroutine da_check_vptox_adjoint(grid, ne, be, ep, vp, cv_size)

   !---------------------------------------------------------------------------
   ! Purpose: Test Vp to X routine and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < x, x > = < v_adj, v >.
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)     :: grid

   integer, intent(in)              :: ne   ! Ensemble size.
   type (be_type), intent(in)       :: be   ! background errors.
   type (ep_type), intent(in)       :: ep   ! Ensemble perturbation type.
   type (vp_type),intent(inout)     :: vp   ! grdipt/level cv (local)
   integer, intent(in)              :: cv_size

! control variable
   real                             :: cv(1:cv_size), cv_2(1:cv_size) 

   real                             :: adj_par_lhs ! < x, x >
   real                             :: adj_par_rhs ! < v_adj, v >
   real                             :: adj_sum_lhs ! < x, x >
   real                             :: adj_sum_rhs ! < v_adj, v >
   real                             :: vp2_v1(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v2(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v3(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v4(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v5(ims:ime,jms:jme,kms:kme)

   real                             :: vp2_v6(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v7(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v8(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v9(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v10(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v11(ims:ime,jms:jme,kms:kme)

!  real                             :: vp2_alpha(ims:ime,jms:jme,kms:kme,1:ne)
   real                             :: vp2_alpha(ims_int:ime_int,jms_int:jme_int,kms_int:kme_int,1:ne)

   if (trace_use) call da_trace_entry("da_check_vptox_adjoint")

   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------


   call da_zero_x(grid%xa)

   vp2_v1(:,:,:) = vp % v1(:,:,:)
   vp2_v2(:,:,:) = vp % v2(:,:,:)

   call da_psichi_to_uv(vp % v1, vp % v2, grid%xb % coefx, &
                        grid%xb % coefy , grid%xa % u, grid%xa % v)

   adj_par_lhs = sum(grid%xa % u(its:ite,jts:jte,:)**2) / typical_u_rms**2
   adj_par_lhs = sum(grid%xa % v(its:ite,jts:jte,:)**2) / typical_v_rms**2 + &
      adj_par_lhs

   grid%xa % u(:,:,:) = grid%xa % u(:,:,:) / typical_u_rms**2
   grid%xa % v(:,:,:) = grid%xa % v(:,:,:) / typical_v_rms**2

   vp%v1(:,:,:)=0.0
   vp%v2(:,:,:)=0.0

   call da_psichi_to_uv_adj(grid%xa % u, grid%xa % v, grid%xb % coefx,   &
                             grid%xb % coefy, vp % v1, vp % v2)

   adj_par_rhs = sum(vp % v1(its:ite,jts:jte,:) * vp2_v1(its:ite,jts:jte,:))
   adj_par_rhs = sum(vp % v2(its:ite,jts:jte,:) * vp2_v2(its:ite,jts:jte,:)) + &
      adj_par_rhs
   
      write(unit=stdout, fmt='(/a/)') &
          'da_check_da_psichi_to_uv_adjoint: Test Results:'
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
          'Single Domain: < u_v,     u_v         > = ', adj_par_lhs, &
          'Single Domain: < psi_chi, psi_chi_adj > = ', adj_par_rhs

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)
   adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)

   if (rootproc) then

      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
          'Whole  Domain: < u_v,     u_v         > = ', adj_sum_lhs, &
          'Whole  Domain: < psi_chi, psi_chi_adj > = ', adj_sum_rhs
   end if
      write(unit=stdout, fmt='(/a/)') &
          'da_check_da_psichi_to_uv_adjoint: Test Finished:'

   vp%v1(:,:,:) = vp2_v1(:,:,:)
   vp%v2(:,:,:) = vp2_v2(:,:,:)

   call da_zero_x(grid%xa)

   vp2_v1(:,:,:) = vp % v1(:,:,:)
   vp2_v2(:,:,:) = vp % v2(:,:,:)
   vp2_v3(:,:,:) = vp % v3(:,:,:)
   vp2_v4(:,:,:) = vp % v4(:,:,:)
   vp2_v5(:,:,:) = vp % v5(:,:,:)

   if ( cloud_cv_options >= 2 ) then
      vp2_v6(:,:,:) = vp % v6(:,:,:)
      vp2_v7(:,:,:) = vp % v7(:,:,:)
      vp2_v8(:,:,:) = vp % v8(:,:,:)
      vp2_v9(:,:,:) = vp % v9(:,:,:)
      vp2_v10(:,:,:)= vp % v10(:,:,:)
   end if

   if ( use_cv_w ) then
      vp2_v11(:,:,:)= vp % v11(:,:,:)
   end if

   !hcl if (be % ne > 0) vp2_alpha(:,:,:,:) = vp % alpha(:,:,:,:)

   !-------------------------------------------------------------------------
   ! [2.0] Perform x = U vp transform:
   !-------------------------------------------------------------------------

   if ( cv_options == 3 ) then
      
      call random_number(cv(:))
      cv(:) = cv(:) - 0.5
      cv_2 = cv

      call da_apply_be( be, cv, vp, grid)
      call da_transform_bal( vp, be, grid)
 
   else

      call da_transform_vptox(grid, vp, be, ep)

   end if

   !-------------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation:
   !-------------------------------------------------------------------------

   !  grid%xa % u(:,:,:) = 0.0
   !  grid%xa % v(:,:,:) = 0.0
   !  grid%xa % t(:,:,:) = 0.0
   !  grid%xa % q(:,:,:) = 0.0
   !  grid%xa%psfc(:,:) = 0.0

   !  grid%xa % p(:,:,:) = 0.0
   !  grid%xa % rho(:,:,:) = 0.0
   !  grid%xa % w(:,:,:) = 0.0
   !  grid%xa % wh(:,:,:) = 0.0
   !  grid%xa % rh(:,:,:) = 0.0
   !  grid%xa % qt(:,:,:) = 0.0
   !  grid%xa % qcw(:,:,:) = 0.0
   !  grid%xa % qrn(:,:,:) = 0.0

   adj_par_lhs = sum(grid%xa%u(its:ite,jts:jte,:)**2)/typical_u_rms**2
   adj_par_lhs = sum(grid%xa%v(its:ite,jts:jte,:)**2)/typical_v_rms**2 + adj_par_lhs
   adj_par_lhs = sum(grid%xa%t(its:ite,jts:jte,:)**2)/typical_t_rms**2 + adj_par_lhs
   if ( (use_radar_rf .or. crtm_cloud) .and. (cloud_cv_options == 1) ) then
      adj_par_lhs = sum(grid%xa%qt(its:ite,jts:jte,:)**2)/typical_q_rms**2 + adj_par_lhs
   else
      adj_par_lhs = sum(grid%xa%q(its:ite,jts:jte,:)**2)/typical_q_rms**2 + adj_par_lhs
   end if
   adj_par_lhs = sum(grid%xa%psfc(its:ite,jts:jte)**2)/typical_p_rms**2 + adj_par_lhs

   adj_par_lhs = sum(grid%xa%p(its:ite,jts:jte,:)**2)/typical_p_rms**2 + adj_par_lhs
   adj_par_lhs = sum(grid%xa%rho(its:ite,jts:jte,:)**2)/typical_rho_rms**2 + &
      adj_par_lhs

   if (use_radarobs) then
      adj_par_lhs = adj_par_lhs &
                + sum(grid%xa % wh (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   else
      adj_par_lhs = adj_par_lhs &
                + sum(grid%xa % w  (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   end if

   if (cv_options_hum == cv_options_hum_relative_humidity) then
      adj_par_lhs = sum(grid%xa % rh(its:ite,jts:jte,:)**2) / &
         typical_rh_rms**2 + adj_par_lhs
   end if

   if ( cloud_cv_options >= 2 ) then
      adj_par_lhs = sum(grid%xa % qcw(its:ite,jts:jte,kts:kte)**2) / &
           typical_qcw_rms**2 + adj_par_lhs
      adj_par_lhs = sum(grid%xa % qrn(its:ite,jts:jte,kts:kte)**2) / &
           typical_qrn_rms**2 + adj_par_lhs
      adj_par_lhs = sum(grid%xa % qci (its:ite,jts:jte,kts:kte)**2) / &
           typical_qci_rms**2 + adj_par_lhs
      adj_par_lhs = sum(grid%xa % qsn(its:ite,jts:jte,kts:kte)**2) / &
           typical_qsn_rms**2 + adj_par_lhs
      adj_par_lhs = sum(grid%xa % qgr (its:ite,jts:jte,kts:kte)**2) / &
           typical_qgr_rms**2 + adj_par_lhs
   end if

   if (use_radar_rf .or. crtm_cloud) then
      if ( cloud_cv_options == 1 ) then
         adj_par_lhs = sum(grid%xa % qcw(its:ite,jts:jte,kts:kte)**2) / &
            typical_qcw_rms**2 + adj_par_lhs
         adj_par_lhs = sum(grid%xa % qrn(its:ite,jts:jte,kts:kte)**2) / &
            typical_qrn_rms**2 + adj_par_lhs
      end if
   end if

   !-------------------------------------------------------------------------
   ! [4.0] Rescale input to adjoint routine:
   !-------------------------------------------------------------------------
      
   grid%xa % u(:,:,:) = grid%xa % u(:,:,:) / typical_u_rms**2
   grid%xa % v(:,:,:) = grid%xa % v(:,:,:) / typical_v_rms**2
   grid%xa % t(:,:,:) = grid%xa % t(:,:,:) / typical_t_rms**2
   if ( (use_radar_rf .or. crtm_cloud) .and. (cloud_cv_options == 1) ) then
      grid%xa % qt(:,:,:) = grid%xa % qt(:,:,:) / typical_q_rms**2
   else
      grid%xa % q(:,:,:) = grid%xa % q(:,:,:) / typical_q_rms**2
   end if
   grid%xa%psfc(:,:) = grid%xa%psfc(:,:) / typical_p_rms**2
   
   grid%xa % p(:,:,:) = grid%xa % p(:,:,:) / typical_p_rms**2
   grid%xa % rho(:,:,:) = grid%xa % rho(:,:,:) / typical_rho_rms**2

   if (use_radarobs) then
      grid%xa %wh(:,:,:) = grid%xa %wh(:,:,:) / typical_w_rms**2
      grid%xa % w(:,:,:) = 0.0
   else
      grid%xa %w (:,:,:) = grid%xa %w (:,:,:) / typical_w_rms**2
   end if

   if (cv_options_hum == cv_options_hum_relative_humidity) then
      grid%xa % rh(:,:,:) = grid%xa % rh(:,:,:) / typical_rh_rms**2
   end if

   if ( cloud_cv_options >= 2 ) then
      grid%xa % qcw(:,:,:) = grid%xa % qcw(:,:,:) / typical_qcw_rms**2
      grid%xa % qrn(:,:,:) = grid%xa % qrn(:,:,:) / typical_qrn_rms**2
      grid%xa % qci(:,:,:) = grid%xa % qci(:,:,:) / typical_qci_rms**2
      grid%xa % qsn(:,:,:) = grid%xa % qsn(:,:,:) / typical_qsn_rms**2
      grid%xa % qgr(:,:,:) = grid%xa % qgr(:,:,:) / typical_qgr_rms**2
   end if

   if (use_radar_rf .or. crtm_cloud) then
      if ( cloud_cv_options == 1 ) then
         grid%xa % qcw(:,:,:) = grid%xa % qcw(:,:,:) / typical_qcw_rms**2
         grid%xa % qrn(:,:,:) = grid%xa % qrn(:,:,:) / typical_qrn_rms**2
      end if
   end if

   !-------------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !-------------------------------------------------------------------------

   call da_zero_vp_type (vp)

   if (cv_options == 3 ) then

      cv = 0.0

      call da_transform_bal_adj( vp, be, grid)
      call da_apply_be_adj( be, cv, vp, grid)

   else

       call da_transform_vptox_adj(grid, vp, be, ep)

   end if

   !-------------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !-------------------------------------------------------------------------

   adj_par_rhs = sum(vp % v1(its:ite,jts:jte,:) * vp2_v1(its:ite,jts:jte,:))
   adj_par_rhs = sum(vp % v2(its:ite,jts:jte,:) * vp2_v2(its:ite,jts:jte,:)) + &
      adj_par_rhs
   adj_par_rhs = sum(vp % v3(its:ite,jts:jte,:) * vp2_v3(its:ite,jts:jte,:)) + &
      adj_par_rhs
   adj_par_rhs = sum(vp % v4(its:ite,jts:jte,:) * vp2_v4(its:ite,jts:jte,:)) + &
      adj_par_rhs
   adj_par_rhs = sum(vp % v5(its:ite,jts:jte,:) * vp2_v5(its:ite,jts:jte,:)) + &
      adj_par_rhs

   if ( cloud_cv_options >= 2 ) then
      adj_par_rhs = sum(vp % v6(its:ite,jts:jte,:) * vp2_v6(its:ite,jts:jte,:)) + &
         adj_par_rhs
      adj_par_rhs = sum(vp % v7(its:ite,jts:jte,:) * vp2_v7(its:ite,jts:jte,:)) + &
         adj_par_rhs
      adj_par_rhs = sum(vp % v8(its:ite,jts:jte,:) * vp2_v8(its:ite,jts:jte,:)) + &
         adj_par_rhs
      adj_par_rhs = sum(vp % v9(its:ite,jts:jte,:) * vp2_v9(its:ite,jts:jte,:)) + &
         adj_par_rhs
      adj_par_rhs = sum(vp % v10(its:ite,jts:jte,:)* vp2_v10(its:ite,jts:jte,:)) + &
         adj_par_rhs
   end if

   if ( use_cv_w ) then
      adj_par_rhs = sum(vp % v11(its:ite,jts:jte,:)* vp2_v11(its:ite,jts:jte,:)) + &
         adj_par_rhs
   end if

   !hcl if (be % ne > 0) then
   !hcl    adj_par_rhs = sum(vp % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,:) * &
   !hcl       vp2_alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,:)) + adj_par_rhs
   !hcl end if

   if ( cv_options == 3 ) adj_par_rhs = sum (cv_2*cv)

   !-------------------------------------------------------------------------
   ! [7.0] Print output:
   !-------------------------------------------------------------------------

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)
   adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)

   write(unit=stdout, fmt='(/a/)') 'da_check_vptox_adjoint: Test Results:'
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Single Domain: < x, x >       = ', adj_par_lhs, &
         'Single Domain: < vp_adj, vp > = ', adj_par_rhs

   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Whole  Domain: < x, x >       = ', adj_sum_lhs, &
         'Whole  Domain: < vp_adj, vp > = ', adj_sum_rhs
   end if

   vp % v1(:,:,:) = vp2_v1(:,:,:)
   vp % v2(:,:,:) = vp2_v2(:,:,:)
   vp % v3(:,:,:) = vp2_v3(:,:,:)
   vp % v4(:,:,:) = vp2_v4(:,:,:)
   vp % v5(:,:,:) = vp2_v5(:,:,:)

   if ( cloud_cv_options >= 2 ) then
      vp % v6(:,:,:) = vp2_v6(:,:,:)
      vp % v7(:,:,:) = vp2_v7(:,:,:)
      vp % v8(:,:,:) = vp2_v8(:,:,:)
      vp % v9(:,:,:) = vp2_v9(:,:,:)
      vp % v10(:,:,:)= vp2_v10(:,:,:)
   end if

   if ( use_cv_w ) then
      vp % v11(:,:,:)= vp2_v11(:,:,:)
   end if

   !hcl if (be % ne > 0) vp % alpha(:,:,:,:) = vp2_alpha(:,:,:,:)

   write(unit=stdout, fmt='(/a/)') 'da_check_vptox_adjoint: Test Finished:'

   if (trace_use) call da_trace_exit("da_check_vptox_adjoint")
      
end subroutine da_check_vptox_adjoint


