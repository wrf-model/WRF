subroutine da_check_xtoy_adjoint_rain(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n             ! loop counter.
   real    :: var

   if (trace_use) call da_trace_entry("da_check_xtoy_adjoint_rain")

   if (iv%info(rain)%nlocal > 0) then
      do n=iv%info(rain)%n1, iv%info(rain)%n2
         y%rain(n)%rain = y%rain(n)%rain/typical_rain_rms

         var = (y%rain(n)%rain) ** 2

         pertile_lhs = pertile_lhs + var

         if (iv%info(rain)%proc_domain(1,n)) then
            adjtest_lhs = adjtest_lhs + var
         end if

         y%rain(n)%rain = y%rain(n)%rain/typical_rain_rms
      end do
   end if

   if (trace_use) call da_trace_exit("da_check_xtoy_adjoint_rain")

end subroutine da_check_xtoy_adjoint_rain

