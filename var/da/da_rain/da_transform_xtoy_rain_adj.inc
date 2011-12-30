subroutine da_transform_xtoy_rain_adj(grid, iv, jo_grad_y, jo_grad_x)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.
   integer :: i, j     ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real, allocatable :: model_rainnc(:)
   real, allocatable :: model_rainc(:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_rain_adj")

      allocate (model_rainnc(iv%info(rain)%n1:iv%info(rain)%n2))
      allocate (model_rainc(iv%info(rain)%n1:iv%info(rain)%n2))

      model_rainnc=0.0
      model_rainc=0.0

      do n=iv%info(rain)%n1,iv%info(rain)%n2
         model_rainnc(n)  = model_rainnc(n) + jo_grad_y%rain(n)%rain
         model_rainc(n)   = model_rainc(n) + jo_grad_y%rain(n)%rain
         jo_grad_y%rain(n)%rain=0
      end do
      
      grid%g_rainc =0.0
      grid%g_rainnc = 0.0

      call da_interp_lin_2d_adj (grid%g_rainc, iv%info(rain), 1, model_rainc)
      call da_interp_lin_2d_adj (grid%g_rainnc, iv%info(rain), 1, model_rainnc)

      deallocate (model_rainc)
      deallocate (model_rainnc)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_rain_adj")

end subroutine da_transform_xtoy_rain_adj


