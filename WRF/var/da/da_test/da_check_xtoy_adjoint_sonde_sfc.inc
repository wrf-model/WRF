subroutine da_check_xtoy_adjoint_sonde_sfc(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n             ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_sonde_sfc")

   do n=iv%info(sound)%n1, iv%info(sound)%n2
      if (iv%info(sound)%zk(1,n) < 1.0 .and. sfc_assi_options /= 2) then
         y%sonde_sfc(n)%u = 0.0
         y%sonde_sfc(n)%v = 0.0
         y%sonde_sfc(n)%t = 0.0
         y%sonde_sfc(n)%p = 0.0
         y%sonde_sfc(n)%q = 0.0

         cycle
      end if

      if ( sfc_assi_options == 2 ) then
          if (iv%sonde_sfc(n)%u%qc < 0) y%sonde_sfc(n)%u = 0.0
          if (iv%sonde_sfc(n)%v%qc < 0) y%sonde_sfc(n)%v = 0.0
          if (iv%sonde_sfc(n)%t%qc < 0) y%sonde_sfc(n)%t = 0.0
          if (iv%sonde_sfc(n)%p%qc < 0) y%sonde_sfc(n)%p = 0.0
          if (iv%sonde_sfc(n)%q%qc < 0) y%sonde_sfc(n)%q = 0.0
      end if

      y%sonde_sfc(n)%u = y%sonde_sfc(n)%u/typical_u_rms
      y%sonde_sfc(n)%v = y%sonde_sfc(n)%v/typical_v_rms
      y%sonde_sfc(n)%t = y%sonde_sfc(n)%t/typical_t_rms
      y%sonde_sfc(n)%p = y%sonde_sfc(n)%p/typical_p_rms
      y%sonde_sfc(n)%q = y%sonde_sfc(n)%q/typical_q_rms

      if (iv%info(sound)%proc_domain(1,n)) then
         adjtest_lhs = adjtest_lhs  &
                     + (y%sonde_sfc(n)%u)**2 &
                     + (y%sonde_sfc(n)%v)**2 &
                     + (y%sonde_sfc(n)%t)**2 &
                     + (y%sonde_sfc(n)%p)**2 &
                     + (y%sonde_sfc(n)%q)**2
      end if

      pertile_lhs = pertile_lhs &
                  + (y%sonde_sfc(n)%u)**2 &
                  + (y%sonde_sfc(n)%v)**2 &
                  + (y%sonde_sfc(n)%t)**2 &
                  + (y%sonde_sfc(n)%p)**2 &
                  + (y%sonde_sfc(n)%q)**2

      y%sonde_sfc(n)%u = y%sonde_sfc(n)%u/typical_u_rms
      y%sonde_sfc(n)%v = y%sonde_sfc(n)%v/typical_v_rms
      y%sonde_sfc(n)%t = y%sonde_sfc(n)%t/typical_t_rms
      y%sonde_sfc(n)%p = y%sonde_sfc(n)%p/typical_p_rms
      y%sonde_sfc(n)%q = y%sonde_sfc(n)%q/typical_q_rms
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_sonde_sfc")

end subroutine da_check_xtoy_adjoint_sonde_sfc


