subroutine da_check_xtoy_adjoint_sound (iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)    :: iv            ! obs. inc. vector (o-b).
   type(y_type) , intent(inout) :: y             ! y = h (xa)
   real,          intent(inout) :: adjtest_lhs, pertile_lhs   

   integer :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_sound")

   do n=iv%info(sound)%n1, iv%info(sound)%n2
      do k=1, iv%info(sound)%levels(n)
         if (iv%info(sound)%proc_domain(k,n)) then
            adjtest_lhs = adjtest_lhs + &
                          (y%sound(n)%u(k)/typical_u_rms)**2 + &
                          (y%sound(n)%v(k)/typical_v_rms)**2 + &
                          (y%sound(n)%t(k)/typical_t_rms)**2 + &
                          (y%sound(n)%q(k)/typical_q_rms)**2
         end if
      end do

      do k=1, iv%info(sound)%levels(n)
         pertile_lhs = pertile_lhs + &
                       (y%sound(n)%u(k)/typical_u_rms)**2 + &
                       (y%sound(n)%v(k)/typical_v_rms)**2 + &
                       (y%sound(n)%t(k)/typical_t_rms)**2 + &
                       (y%sound(n)%q(k)/typical_q_rms)**2

         y%sound(n)%u(k) = y%sound(n)%u(k) / typical_u_rms ** 2
         y%sound(n)%v(k) = y%sound(n)%v(k) / typical_v_rms ** 2
         y%sound(n)%t(k) = y%sound(n)%t(k) / typical_t_rms ** 2
         y%sound(n)%q(k) = y%sound(n)%q(k) / typical_q_rms ** 2
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_sound")

end subroutine da_check_xtoy_adjoint_sound


