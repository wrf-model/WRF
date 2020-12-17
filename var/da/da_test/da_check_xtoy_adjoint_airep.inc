subroutine da_check_xtoy_adjoint_airep(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n, k          ! Loop counter.
   
   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_airep")

   do n=iv%info(airep)%n1, iv%info(airep)%n2
      do k=1, iv%info(airep)%levels(n)
         if (iv%info(airep)%proc_domain(k,n)) then
            adjtest_lhs = adjtest_lhs + &
               (y%airep(n)%u(k) / typical_u_rms)**2 + &
               (y%airep(n)%v(k) / typical_v_rms)**2 + &
               (y%airep(n)%t(k) / typical_t_rms)**2 + & 
               (y%airep(n)%q(k) / typical_q_rms)**2 
         end if
      end do

      do k=1, iv%info(airep)%levels(n)
         pertile_lhs = pertile_lhs + &
            (y%airep(n)%u(k) / typical_u_rms)**2 + &
            (y%airep(n)%v(k) / typical_v_rms)**2 + &
            (y%airep(n)%t(k) / typical_t_rms)**2 + &
            (y%airep(n)%q(k) / typical_q_rms)**2

        y%airep(n)%u(k) = y%airep(n)%u(k) / typical_u_rms ** 2
        y%airep(n)%v(k) = y%airep(n)%v(k) / typical_v_rms ** 2
        y%airep(n)%t(k) = y%airep(n)%t(k) / typical_t_rms ** 2
        y%airep(n)%q(k) = y%airep(n)%q(k) / typical_q_rms ** 2
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_airep")

end subroutine da_check_xtoy_adjoint_airep


