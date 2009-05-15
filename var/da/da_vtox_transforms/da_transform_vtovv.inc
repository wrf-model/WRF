subroutine da_transform_vtovv(grid, cv_size, be, cv, vv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain),  intent(inout) :: grid
   integer,       intent(in)    :: cv_size ! Size of cv array.
   type(be_type), intent(in)    :: be   ! Background error structure.
   real,          intent(in)    :: cv(1:cv_size)   ! control variables.
   type(vp_type), intent(inout) :: vv   ! Grid point/EOF equivalent.

   integer :: n    ! Loop counter.
   integer :: mz   ! Vertical truncation.
   integer :: ne   ! Ensemble size.

   if (trace_use) call da_trace_entry("da_transform_vtovv")

   !-------------------------------------------------------------------------
   ! [1.0] Fill vv arrays from 1-dimensional cv array.
   !-------------------------------------------------------------------------

   call da_cv_to_vv(cv_size, cv, (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz, be%ne /), vv)

   !-------------------------------------------------------------------------
   ! [2.0] Perform VToVV Transform:
   !-------------------------------------------------------------------------

   ! [2.1] Transform 1st control variable:

   mz = be % v1 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid,mz, be % v1 % rf_alpha, be % v1 % val, vv % v1)
   end if

   ! [2.2] Transform 2nd control variable:

   mz = be % v2 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % v2 % rf_alpha, be % v2 % val, vv % v2)
   end if

   ! [2.3] Transform 3rd control variable

   mz = be % v3 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % v3 % rf_alpha,be % v3 % val, vv % v3)
   end if

   ! [2.4] Transform 4th control variable
      
   mz = be % v4 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % v4 % rf_alpha, be % v4 % val, vv % v4)
   end if

   ! [2.5] Transform 5th control variable

   mz = be % v5 % mz
   if (mz > 0) then
      call da_transform_through_rf(grid, mz, be % v5 % rf_alpha, be % v5 % val, vv % v5)
   end if

   ! [2.6] Transform alpha control variable

   ne = be % ne
   if (ne > 0) then
      mz = be % alpha % mz
      do n = 1, ne
         call da_transform_through_rf(grid, mz, be % alpha % rf_alpha, be % alpha % val, vv % alpha(:,:,:,n) )
      end do
   end if

   if (trace_use) call da_trace_exit("da_transform_vtovv")

end subroutine da_transform_vtovv

