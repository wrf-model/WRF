subroutine da_check_xtoy_adjoint_mtgirs (iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)    :: iv            ! obs. inc. vector (o-b).
   type(y_type) , intent(inout) :: y             ! y = h (xa)
   real,          intent(inout) :: adjtest_lhs, pertile_lhs   

   integer :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_mtgirs")

   do n=iv%info(mtgirs)%n1, iv%info(mtgirs)%n2
      do k=1, iv%info(mtgirs)%levels(n)
         if (iv%info(mtgirs)%proc_domain(k,n)) then
            adjtest_lhs = adjtest_lhs + &
                          (y%mtgirs(n)%u(k)/typical_u_rms)**2 + &
                          (y%mtgirs(n)%v(k)/typical_v_rms)**2 + &
                          (y%mtgirs(n)%t(k)/typical_t_rms)**2 + &
                          (y%mtgirs(n)%q(k)/typical_q_rms)**2
         end if
      end do

      do k=1, iv%info(mtgirs)%levels(n)
         pertile_lhs = pertile_lhs + &
                       (y%mtgirs(n)%u(k)/typical_u_rms)**2 + &
                       (y%mtgirs(n)%v(k)/typical_v_rms)**2 + &
                       (y%mtgirs(n)%t(k)/typical_t_rms)**2 + &
                       (y%mtgirs(n)%q(k)/typical_q_rms)**2

         y%mtgirs(n)%u(k) = y%mtgirs(n)%u(k) / typical_u_rms ** 2
         y%mtgirs(n)%v(k) = y%mtgirs(n)%v(k) / typical_v_rms ** 2
         y%mtgirs(n)%t(k) = y%mtgirs(n)%t(k) / typical_t_rms ** 2
         y%mtgirs(n)%q(k) = y%mtgirs(n)%q(k) / typical_q_rms ** 2
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_mtgirs")

end subroutine da_check_xtoy_adjoint_mtgirs


