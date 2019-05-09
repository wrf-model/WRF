subroutine da_check_vtox_adjoint(grid, cv_size, xbx, be, ep, cv1, vv, vp)

   !---------------------------------------------------------------------------
   ! Purpose: Test V to X routine and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < x, x > = < v_adj, v >.
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)               :: grid

   integer, intent(in)             :: cv_size
   type (xbx_type),intent(in)      :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)      :: be     ! background error structure.
   type (ep_type), intent(in)      :: ep     ! Ensemble perturbation structure.
   real, intent(in)                :: cv1(1:cv_size) ! control variable (local).
   type (vp_type), intent(inout)   :: vv     ! Grdipt/EOF CV.
   type (vp_type), intent(inout)   :: vp     ! Grdipt/level CV.

   real                   :: cv2(1:cv_size)    ! control variable (local).
   real                   :: adj_par_lhs ! < x, x >
   real                   :: adj_par_rhs ! < x, x >
   real                   :: adj_sum_lhs ! < x, x >
   real                   :: adj_sum_rhs ! < v_adj, v >

   if (trace_use) call da_trace_entry("da_check_vtox_adjoint")

   write(unit=stdout, fmt='(/a/)') &
      'da_check_vtox_adjoint: Adjoint Test Results:'

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------

   cv2(:) = 0.0
      
   !----------------------------------------------------------------------
   ! [2.0] Perform x = U v transform:
   !----------------------------------------------------------------------

   call da_zero_x (grid%xa)

   call da_transform_vtox(grid,cv_size, xbx, be, ep, cv1, vv, vp)
   call da_transform_xtoxa(grid)

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation: 
   !----------------------------------------------------------------------

   adj_par_lhs = sum(grid%xa % u(its:ite, jts:jte, kts:kte)**2) / typical_u_rms**2 &
               + sum(grid%xa % v(its:ite, jts:jte, kts:kte)**2) / typical_v_rms**2 &     
               + sum(grid%xa % p(its:ite, jts:jte, kts:kte)**2) / typical_p_rms**2 &     
               + sum(grid%xa % t(its:ite, jts:jte, kts:kte)**2) / typical_t_rms**2 &     
               + sum(grid%xa % q(its:ite, jts:jte, kts:kte)**2) / typical_q_rms**2 &     
               + sum(grid%xa % rho(its:ite,jts:jte,kts:kte)**2)/ typical_rho_rms**2 & 
               + sum(grid%xa % psfc(its:ite, jts:jte)**2) / typical_p_rms**2             

   if (cv_options_hum == cv_options_hum_relative_humidity) then
      adj_par_lhs = adj_par_lhs &
              + sum(grid%xa % rh(its:ite, jts:jte, kts:kte)**2) / typical_rh_rms**2
   end if
!
   if (use_radar_rf .or. use_radar_rhv .or.  crtm_cloud ) then
      adj_par_lhs = adj_par_lhs &
             + sum(grid%xa % qcw(its:ite, jts:jte, kts:kte)**2)/typical_qcw_rms**2 &
             + sum(grid%xa % qrn(its:ite, jts:jte, kts:kte)**2)/typical_qrn_rms**2
      if ( cloud_cv_options /= 1 ) then
         adj_par_lhs = adj_par_lhs &
             + sum(grid%xa % qci(its:ite, jts:jte, kts:kte)**2)/typical_qci_rms**2 &
             + sum(grid%xa % qsn(its:ite, jts:jte, kts:kte)**2)/typical_qsn_rms**2 &
             + sum(grid%xa % qgr(its:ite, jts:jte, kts:kte)**2)/typical_qgr_rms**2
      end if
   end if

   if (use_radarobs) then
      adj_par_lhs = adj_par_lhs &
         + sum(grid%xa % wh (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   else
      adj_par_lhs = adj_par_lhs &
         + sum(grid%xa % w  (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   end if

   !-------------------------------------------------------------------------
   ! [4.0] Rescale input to adjoint routine:
   !-------------------------------------------------------------------------

   grid%xa % u(:,:,:) = grid%xa % u(:,:,:) / typical_u_rms**2
   grid%xa % v(:,:,:) = grid%xa % v(:,:,:) / typical_v_rms**2
   grid%xa % p(:,:,:) = grid%xa % p(:,:,:) / typical_p_rms**2
   grid%xa % t(:,:,:) = grid%xa % t(:,:,:) / typical_t_rms**2
   grid%xa % q(:,:,:) = grid%xa % q(:,:,:) / typical_q_rms**2
   grid%xa % rho(:,:,:) = grid%xa % rho(:,:,:) / typical_rho_rms**2

   grid%xa % psfc(:,:) = grid%xa % psfc(:,:) / typical_p_rms**2

   if (cv_options_hum == cv_options_hum_relative_humidity) then
      grid%xa % rh(:,:,:) = grid%xa % rh(:,:,:) / typical_rh_rms**2
   end if

!
   if (use_radar_rf .or. use_radar_rhv .or. crtm_cloud ) then
      grid%xa % qcw(:,:,:) = grid%xa % qcw(:,:,:) / typical_qcw_rms**2
      grid%xa % qrn(:,:,:) = grid%xa % qrn(:,:,:) / typical_qrn_rms**2
      if ( cloud_cv_options /= 1 ) then
         grid%xa % qci(:,:,:) = grid%xa % qci(:,:,:) / typical_qci_rms**2
         grid%xa % qsn(:,:,:) = grid%xa % qsn(:,:,:) / typical_qsn_rms**2
         grid%xa % qgr(:,:,:) = grid%xa % qgr(:,:,:) / typical_qgr_rms**2
      end if
   end if

   if (use_radarobs) then
      grid%xa %wh(:,:,:) = grid%xa %wh(:,:,:) / typical_w_rms**2
      grid%xa % w(:,:,:) = 0.0
   else
      grid%xa %w (:,:,:) = grid%xa %w (:,:,:) / typical_w_rms**2
   end if

   !-------------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !-------------------------------------------------------------------------

   call da_transform_xtoxa_adj(grid)
   !as of V3.9, da_zero_vp_type(vp) is commented out in the da_transform_vtox_adj
   call da_zero_vp_type(vp) !as of V3.9, zero out vp here
   call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv2)

   !-------------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !-------------------------------------------------------------------------

   adj_par_rhs = sum(cv1(1:cv_size) * cv2(1:cv_size))

   !-------------------------------------------------------------------------
   ! [7.0] Print output:
   !-------------------------------------------------------------------------

   if (.not. global ) then
    if( num_procs == 1) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Single Domain: < x, x >     = ', adj_par_lhs, &
         'Single Domain: < v_adj, v > = ', adj_par_rhs
    else
      write(unit=stdout, fmt='(/a/,a/)')&
        'It is Multi Processor Run: ',&
        'For Single Domain: da_check_vtox_adjoint Test: Not Performed'
    endif
   end if

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)

   if (global) then
      adj_sum_rhs = adj_par_rhs
   else
      adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)
   end if

   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Whole  Domain: < x, x >     = ', adj_sum_lhs, &
         'Whole  Domain: < v_adj, v > = ', adj_sum_rhs
   end if

   write(unit=stdout, fmt='(/a/)') 'da_check_vtox_adjoint: Finished'

   if (trace_use) call da_trace_exit("da_check_vtox_adjoint")

end subroutine da_check_vtox_adjoint


