subroutine da_check_xtoy_adjoint_buoy(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n             ! Loop counter.
   
   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_buoy")

   do n=iv%info(buoy)%n1, iv%info(buoy)%n2
      if (iv%info(buoy)%zk(1,n) < 1.0 .and. sfc_assi_options /= 2) then
         y%buoy(n)%u = 0.0
         y%buoy(n)%v = 0.0
         y%buoy(n)%t = 0.0
         y%buoy(n)%p = 0.0
         y%buoy(n)%q = 0.0

         cycle
      end if

      if ( sfc_assi_options == 2 ) then
          if (iv%buoy(n)%u%qc < 0) y%buoy(n)%u = 0.0
          if (iv%buoy(n)%v%qc < 0) y%buoy(n)%v = 0.0
          if (iv%buoy(n)%t%qc < 0) y%buoy(n)%t = 0.0
          if (iv%buoy(n)%p%qc < 0) y%buoy(n)%p = 0.0
          if (iv%buoy(n)%q%qc < 0) y%buoy(n)%q = 0.0
      end if

      y%buoy(n)%u = y%buoy(n)%u/typical_u_rms
      y%buoy(n)%v = y%buoy(n)%v/typical_v_rms
      y%buoy(n)%t = y%buoy(n)%t/typical_t_rms
      y%buoy(n)%p = y%buoy(n)%p/typical_p_rms
      y%buoy(n)%q = y%buoy(n)%q/typical_q_rms

      if (iv%info(buoy)%proc_domain(1,n)) then
         adjtest_lhs = adjtest_lhs  &
                     + (y%buoy(n)%u)**2 &
                     + (y%buoy(n)%v)**2 &
                     + (y%buoy(n)%t)**2 &
                     + (y%buoy(n)%p)**2 &
                     + (y%buoy(n)%q)**2
      end if

      pertile_lhs = pertile_lhs &
                  + (y%buoy(n)%u)**2 &
                  + (y%buoy(n)%v)**2 &
                  + (y%buoy(n)%t)**2 &
                  + (y%buoy(n)%p)**2 &
                  + (y%buoy(n)%q)**2

      y%buoy(n)%u = y%buoy(n)%u/typical_u_rms
      y%buoy(n)%v = y%buoy(n)%v/typical_v_rms
      y%buoy(n)%t = y%buoy(n)%t/typical_t_rms
      y%buoy(n)%p = y%buoy(n)%p/typical_p_rms
      y%buoy(n)%q = y%buoy(n)%q/typical_q_rms
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_buoy")

end subroutine da_check_xtoy_adjoint_buoy


