subroutine da_check_xtoy_adjoint_bogus(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer  :: n, k          ! Loop counter.
   
   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_bogus")

   do n=iv%info(bogus)%n1, iv%info(bogus)%n2
      if (iv%info(bogus)%proc_domain(1,n)) then
         do k=1, iv%info(bogus)%levels(n)
            adjtest_lhs = adjtest_lhs + &
                          (y%bogus(n)%u(k)/typical_u_rms)**2 + &
                          (y%bogus(n)%v(k)/typical_v_rms)**2 + &
                          (y%bogus(n)%t(k)/typical_t_rms)**2 + &
                          (y%bogus(n)%q(k)/typical_q_rms)**2 
         end do
         adjtest_lhs = adjtest_lhs + (y%bogus(n)%slp/typical_p_rms)**2
      end if

      do k=1, iv%info(bogus)%levels(n)
         pertile_lhs = pertile_lhs + &
                       (y%bogus(n)%u(k)/typical_u_rms)**2 + &
                       (y%bogus(n)%v(k)/typical_v_rms)**2 + &
                       (y%bogus(n)%t(k)/typical_t_rms)**2 + &
                       (y%bogus(n)%q(k)/typical_q_rms)**2

         y%bogus(n)%u(k) = y%bogus(n)%u(k) / typical_u_rms ** 2
         y%bogus(n)%v(k) = y%bogus(n)%v(k) / typical_v_rms ** 2
         y%bogus(n)%t(k) = y%bogus(n)%t(k) / typical_t_rms ** 2
         y%bogus(n)%q(k) = y%bogus(n)%q(k) / typical_q_rms ** 2
      end do
      pertile_lhs = pertile_lhs + (y%bogus(n)%slp/typical_p_rms)**2
      y%bogus(n)%slp = y%bogus(n)%slp / typical_p_rms ** 2
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_bogus")

end subroutine da_check_xtoy_adjoint_bogus


