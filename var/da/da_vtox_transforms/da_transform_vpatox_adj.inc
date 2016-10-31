subroutine da_transform_vpatox_adj(grid, be, ep, vp, nobwin) 

   !----------------------------------------------------------------------
   ! Purpose:  Does adjoint transform of alpha control variable (vp%alpha) to x
   !----------------------------------------------------------------------

   implicit none

   type(be_type),              intent(in)    :: be     ! background error structure.
   type(ep_type),              intent(in)    :: ep     ! Ensemble perturbation structure.
   type(vp_type),              intent(inout) :: vp     ! Grdipt/level CV.
   type(domain),               intent(inout) :: grid
   integer, intent(in), optional             :: nobwin

   integer :: iobwin

   if (be%ne <= 0 .or. alphacv_method /= alphacv_method_xa) return

   if (trace_use) call da_trace_entry("da_transform_vpatox_adj")

   iobwin = 1
   if ( present(nobwin) ) then
      iobwin = nobwin
   end if

   if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
      !this is for getting vp%alpha only
      vp%alpha = 0.0
      if ( anal_type_hybrid_dual_res ) then
         call da_calc_flow_dependence_xa_adj_dual_res(be%ne, ep, grid, grid%xa, vp, iobwin)
      else
         call da_calc_flow_dependence_xa_adj(be%ne, ep, grid%xa, vp, iobwin)
      end if
   end if

   if (trace_use) call da_trace_exit("da_transform_vpatox_adj")

end subroutine da_transform_vpatox_adj


