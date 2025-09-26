subroutine da_check_xtoy_adjoint_tamdar (iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)    :: iv            ! obs. inc. vector (o-b).
   type(y_type) , intent(inout) :: y             ! y = h (xa)
   real,          intent(inout) :: adjtest_lhs, pertile_lhs   

   integer :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_tamdar")

   do n=iv%info(tamdar)%n1, iv%info(tamdar)%n2
      do k=1, iv%info(tamdar)%levels(n)
         if (iv%info(tamdar)%proc_domain(k,n)) then
            adjtest_lhs = adjtest_lhs + &
                          (y%tamdar(n)%u(k)/typical_u_rms)**2 + &
                          (y%tamdar(n)%v(k)/typical_v_rms)**2 + &
                          (y%tamdar(n)%t(k)/typical_t_rms)**2 + &
                          (y%tamdar(n)%q(k)/typical_q_rms)**2
         end if
      end do

      do k=1, iv%info(tamdar)%levels(n)
         pertile_lhs = pertile_lhs + &
                       (y%tamdar(n)%u(k)/typical_u_rms)**2 + &
                       (y%tamdar(n)%v(k)/typical_v_rms)**2 + &
                       (y%tamdar(n)%t(k)/typical_t_rms)**2 + &
                       (y%tamdar(n)%q(k)/typical_q_rms)**2

         y%tamdar(n)%u(k) = y%tamdar(n)%u(k) / typical_u_rms ** 2
         y%tamdar(n)%v(k) = y%tamdar(n)%v(k) / typical_v_rms ** 2
         y%tamdar(n)%t(k) = y%tamdar(n)%t(k) / typical_t_rms ** 2
         y%tamdar(n)%q(k) = y%tamdar(n)%q(k) / typical_q_rms ** 2
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_tamdar")

end subroutine da_check_xtoy_adjoint_tamdar


