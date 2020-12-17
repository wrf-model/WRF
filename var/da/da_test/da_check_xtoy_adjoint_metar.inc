subroutine da_check_xtoy_adjoint_metar(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type(y_type) , intent(inout)  :: y             ! y = h (xa)
   real,          intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n             ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_metar")

   do n=iv%info(metar)%n1, iv%info(metar)%n2
      if (iv%info(metar)%zk(1,n) < 1.0 .and. sfc_assi_options /= 2) then
         y%metar(n)%u = 0.0
         y%metar(n)%v = 0.0
         y%metar(n)%t = 0.0
         y%metar(n)%p = 0.0
         y%metar(n)%q = 0.0

         cycle
      end if

      if ( sfc_assi_options == 2 ) then
          if (iv%metar(n)%u%qc < 0) y%metar(n)%u = 0.0
          if (iv%metar(n)%v%qc < 0) y%metar(n)%v = 0.0
          if (iv%metar(n)%t%qc < 0) y%metar(n)%t = 0.0
          if (iv%metar(n)%p%qc < 0) y%metar(n)%p = 0.0
          if (iv%metar(n)%q%qc < 0) y%metar(n)%q = 0.0
      end if

      y%metar(n)%u = y%metar(n)%u/typical_u_rms
      y%metar(n)%v = y%metar(n)%v/typical_v_rms
      y%metar(n)%t = y%metar(n)%t/typical_t_rms
      y%metar(n)%p = y%metar(n)%p/typical_p_rms
      y%metar(n)%q = y%metar(n)%q/typical_q_rms

      if (iv%info(metar)%proc_domain(1,n)) then
         adjtest_lhs = adjtest_lhs  &
                     + (y%metar(n)%u)**2 &
                     + (y%metar(n)%v)**2 &
                     + (y%metar(n)%t)**2 &
                     + (y%metar(n)%p)**2 &
                     + (y%metar(n)%q)**2
      end if

      pertile_lhs = pertile_lhs &
                  + (y%metar(n)%u)**2 &
                  + (y%metar(n)%v)**2 &
                  + (y%metar(n)%t)**2 &
                  + (y%metar(n)%p)**2 &
                  + (y%metar(n)%q)**2

      y%metar(n)%u = y%metar(n)%u/typical_u_rms
      y%metar(n)%v = y%metar(n)%v/typical_v_rms
      y%metar(n)%t = y%metar(n)%t/typical_t_rms
      y%metar(n)%p = y%metar(n)%p/typical_p_rms
      y%metar(n)%q = y%metar(n)%q/typical_q_rms
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_metar")

end subroutine da_check_xtoy_adjoint_metar


