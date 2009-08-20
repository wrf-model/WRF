subroutine da_transform_vtovv_adj(grid, cv_size, be, cv, vv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain),  intent(inout) :: grid
   integer,       intent(in)    :: cv_size ! Size of cv array.
   type(be_type), intent(in)    :: be   ! Background error structure.
   real,          intent(inout) :: cv(1:cv_size)   ! control variables.
   type(vp_type), intent(inout) :: vv   ! Grid point/EOF control var.

   integer :: n    ! Loop counter.
   integer :: mz   ! Vertical truncation.
   integer :: ne   ! Ensemble size.

   if (trace_use) call da_trace_entry("da_transform_vtovv_adj")

   !-------------------------------------------------------------------------
   ! [2.0] Perform VToVV Transform:
   !-------------------------------------------------------------------------

   ! [2.1] Transform 1st control variable:

   mz = be % v1 % mz
   if (mz > 0) then
      call da_transform_through_rf_adj(grid, mz, be % v1 % rf_alpha, be % v1 % val, vv % v1)
   end if

   ! [2.2] Transform 2nd control variable:

   mz = be % v2 % mz
   if (mz > 0) then
      call da_transform_through_rf_adj(grid, mz, be % v2 % rf_alpha, be % v2 % val, vv % v2)
   end if

   ! [2.3] Transform 3rd control variable

   mz = be % v3 % mz
   if (mz > 0) then
      call da_transform_through_rf_adj(grid, mz, be % v3 % rf_alpha, be % v3 % val, vv % v3)
   end if
   
   ! [2.4] Transform 4th control variable
      
   mz = be % v4 % mz
   if (mz > 0) then
      call da_transform_through_rf_adj(grid, mz, be % v4 % rf_alpha, be % v4 % val, vv % v4)
   end if

   ! [2.5] Transform 5th control variable

   mz = be % v5 % mz
   if (mz > 0) then
      call da_transform_through_rf_adj(grid, mz, be % v5 % rf_alpha, be % v5 % val, vv % v5)
   end if

   ! [2.6] Transform alpha control variable

   ne = be % ne
   if (ne > 0) then
      mz = be % alpha % mz
      do n = 1, ne
         call da_transform_through_rf_adj(grid, mz, be % alpha % rf_alpha, be % alpha % val, vv % alpha(:,:,:,n))
      end do
   end if

   !-------------------------------------------------------------------------
   ! [1.0] Fill 1D cv array from 3-dimensional vv arrays.
   !-------------------------------------------------------------------------

   call da_vv_to_cv( vv, grid%xp, (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz, be%ne /), &
                     cv_size, cv)

   if (trace_use) call da_trace_exit("da_transform_vtovv_adj")

end subroutine da_transform_vtovv_adj


