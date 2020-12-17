subroutine da_check_xtoy_adjoint_satob (iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type(y_type) , intent(inout)  :: y             ! y = h (xa)
   real,          intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n             ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_satob")

   do n=1, iv%num_satob
      if (iv%satob(n)%loc%proc_domain) then
         adjtest_lhs = adjtest_lhs + (y%satob(n)%u/typical_u_rms)**2 + &
                       (y%satob(n)%v/typical_v_rms)**2
      end if

      pertile_lhs = pertile_lhs + (y%satob(n)%u/typical_u_rms)**2 + &
                    (y%satob(n)%v/typical_v_rms)**2

      y%satob(n)%u = y%satob(n)%u/typical_u_rms ** 2
      y%satob(n)%v = y%satob(n)%v/typical_v_rms ** 2
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_satob")

end subroutine da_check_xtoy_adjoint_satob


