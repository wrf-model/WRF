subroutine da_transform_xtoy_rain(grid, iv, y, hr_rainc, hr_rainnc)
   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)
   real, dimension(ims:ime,jms:jme), intent(in) :: hr_rainc, hr_rainnc

   integer :: n        ! Loop counter.

   real, allocatable :: model_rainc(:)
   real, allocatable :: model_rainnc(:)

   if (trace_use) call da_trace_entry("da_transform_xtoy_rain")

   allocate (model_rainc(iv%info(rain)%n1:iv%info(rain)%n2))
   allocate (model_rainnc(iv%info(rain)%n1:iv%info(rain)%n2))

   model_rainc=0.0
   model_rainnc=0.0

   call da_interp_lin_2d (hr_rainc, iv%info(rain), 1, model_rainc)
   call da_interp_lin_2d (hr_rainnc, iv%info(rain), 1, model_rainnc)

   do n=iv%info(rain)%n1,iv%info(rain)%n2
      if (iv % rain(n) % rain % qc ==  missing_data) then
         y%rain(n)%rain = 0.0
      else
         y%rain(n)%rain = model_rainc(n) + model_rainnc(n)
      endif
   end do

   deallocate (model_rainc)
   deallocate (model_rainnc)

   if (trace_use) call da_trace_exit("da_transform_xtoy_rain")

end subroutine da_transform_xtoy_rain

