subroutine da_transform_vpatox(grid, be, ep, vp, nobwin) 

   !----------------------------------------------------------------------
   ! Purpose:  Does transform of alpha control variable (vp%alpha) to x
   !----------------------------------------------------------------------

   implicit none

   type(be_type),              intent(in)    :: be     ! background error structure.
   type(ep_type),              intent(in)    :: ep     ! Ensemble perturbation structure.
   type(vp_type),              intent(inout) :: vp     ! Grdipt/level CV.
   type(domain),               intent(inout) :: grid
   integer, intent(in), optional             :: nobwin

   integer :: iobwin

   if (be%ne <= 0 .or. alphacv_method /= alphacv_method_xa) return

   if (trace_use) call da_trace_entry("da_transform_vpatox")

   iobwin = 1
   if ( present(nobwin) ) then
      iobwin = nobwin
   end if

   if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
      !transform xa_ens from vp%alpha
      !xa_ens holds the increment from ensemble perturbations for certain fgat time
      grid%xa_ens%u = 0.0
      grid%xa_ens%v = 0.0
      grid%xa_ens%t = 0.0
      grid%xa_ens%q = 0.0
      grid%xa_ens%psfc = 0.0
      grid%xa_ens%qcw = 0.0
      grid%xa_ens%qrn = 0.0
      grid%xa_ens%qci = 0.0
      grid%xa_ens%qsn = 0.0
      grid%xa_ens%qgr = 0.0
      if ( anal_type_hybrid_dual_res ) then
         call da_calc_flow_dependence_xa_dual_res(grid, grid%xa_ens, be%ne, ep, vp, iobwin)
      else
         call da_calc_flow_dependence_xa(grid%xa_ens, be%ne, ep, vp, iobwin)
      end if
   end if

   if (trace_use) call da_trace_exit("da_transform_vpatox")

end subroutine da_transform_vpatox


