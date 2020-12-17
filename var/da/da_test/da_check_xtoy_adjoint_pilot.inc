subroutine da_check_xtoy_adjoint_pilot(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs  

   integer  :: n, k          ! Loop counter.
   
   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_pilot")

   do n=iv%info(pilot)%n1, iv%info(pilot)%n2
      if (iv%info(pilot)%proc_domain(1,n)) then
         do k=1, iv%info(pilot)%levels(n)
            adjtest_lhs = adjtest_lhs + &
                (y%pilot(n)%u(k)/typical_u_rms)**2 + (y%pilot(n)%v(k)/typical_v_rms)**2
         end do
      end if

      do k=1, iv%info(pilot)%levels(n)
         pertile_lhs = pertile_lhs + &
            (y%pilot(n)%u(k)/typical_u_rms)**2 + (y%pilot(n)%v(k)/typical_v_rms)**2

         y%pilot(n)%u(k)= y%pilot(n)%u(k) / typical_u_rms ** 2
         y%pilot(n)%v(k)= y%pilot(n)%v(k) / typical_v_rms ** 2
      end do
   end do

   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_pilot")

end subroutine da_check_xtoy_adjoint_pilot


