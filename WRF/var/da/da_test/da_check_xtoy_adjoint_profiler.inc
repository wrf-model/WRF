subroutine da_check_xtoy_adjoint_profiler(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs  

   integer :: n, k          ! Loop counter.
   
   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_profiler")

   do n=iv%info(profiler)%n1, iv%info(profiler)%n2
      if (iv%info(profiler)%proc_domain(1,n)) then
         do k=1, iv%info(profiler)%levels(n)
            adjtest_lhs = adjtest_lhs &
              + (y%profiler(n)%u(k)/typical_u_rms)**2 + (y%profiler(n)%v(k)/typical_v_rms)**2

         end do
      end if

      do k=1, iv%info(profiler)%levels(n)
         pertile_lhs = pertile_lhs &
            + (y%profiler(n)%u(k)/typical_u_rms)**2 + (y%profiler(n)%v(k)/typical_v_rms)**2

         y%profiler(n)%u(k)= y%profiler(n)%u(k) / typical_u_rms ** 2
         y%profiler(n)%v(k)= y%profiler(n)%v(k) / typical_v_rms ** 2
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_profiler")

end subroutine da_check_xtoy_adjoint_profiler


