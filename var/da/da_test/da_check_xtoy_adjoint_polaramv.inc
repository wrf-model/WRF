subroutine da_check_xtoy_adjoint_polaramv (iv, y, adjtest_lhs, pertile_lhs)

   !-------------------------------------------------------------------------
   ! Purpose:  Adjoint Test for Polar AMVs Obs
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n, k          ! Loop counter.
   
   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_polaramv")

   do n=iv%info(polaramv)%n1, iv%info(polaramv)%n2
      do k=1, iv%info(polaramv)%levels(n)
         if (iv%info(polaramv)%proc_domain(k,n)) then
            adjtest_lhs = adjtest_lhs + (y%polaramv(n)%u(k)/typical_u_rms)**2 + (y%polaramv(n)%v(k)/typical_v_rms)**2
         end if
      end do

      do k=1, iv%info(polaramv)%levels(n)
         pertile_lhs = pertile_lhs + &
            (y%polaramv(n)%u(k)/typical_u_rms)**2 + (y%polaramv(n)%v(k)/typical_v_rms)**2

         y%polaramv(n)%u(k)= y%polaramv(n)%u(k) / typical_u_rms ** 2
         y%polaramv(n)%v(k)= y%polaramv(n)%v(k) / typical_v_rms ** 2
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_polaramv")

end subroutine da_check_xtoy_adjoint_polaramv


