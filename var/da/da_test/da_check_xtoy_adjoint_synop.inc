subroutine da_check_xtoy_adjoint_synop(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer  :: n             ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_synop")

   do n=1, iv%info(synop)%nlocal
      if (iv%info(synop)%zk(1,n) < 1.0) then
         y%synop(n)%u = 0.0
         y%synop(n)%v = 0.0
         y%synop(n)%t = 0.0
         y%synop(n)%p = 0.0
         y%synop(n)%q = 0.0
         cycle
      end if

      y%synop(n)%u = y%synop(n)%u/typical_u_rms
      y%synop(n)%v = y%synop(n)%v/typical_v_rms
      y%synop(n)%t = y%synop(n)%t/typical_t_rms
      y%synop(n)%p = y%synop(n)%p/typical_p_rms
      y%synop(n)%q = y%synop(n)%q/typical_q_rms

      if (iv%info(synop)%proc_domain(1,n)) then
         adjtest_lhs = adjtest_lhs + (y%synop(n)%u)**2 + (y%synop(n)%v)**2 + (y%synop(n)%t)**2 &
            + (y%synop(n)%p)**2 + (y%synop(n)%q)**2
      end if

      pertile_lhs = pertile_lhs + (y%synop(n)%u)**2 + (y%synop(n)%v)**2 + (y%synop(n)%t)**2 &
         + (y%synop(n)%p)**2 + (y%synop(n)%q)**2

      y%synop(n)%u = y%synop(n)%u/typical_u_rms
      y%synop(n)%v = y%synop(n)%v/typical_v_rms
      y%synop(n)%t = y%synop(n)%t/typical_t_rms
      y%synop(n)%p = y%synop(n)%p/typical_p_rms
      y%synop(n)%q = y%synop(n)%q/typical_q_rms
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_synop")

end subroutine da_check_xtoy_adjoint_synop


