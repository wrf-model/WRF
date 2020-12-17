subroutine da_check_xtoy_adjoint_ssmt1(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer  :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_ssmt1")

   do n=iv%info(ssmt1)%n1, iv%info(ssmt1)%n2
      if (iv%info(ssmt1)%proc_domain(1,n)) then
         do k=1, iv%info(ssmt1)%levels(n)
            adjtest_lhs = adjtest_lhs + (y%ssmt1(n)%t(k)/typical_t_rms)**2
         end do
      end if

      do k=1, iv%info(ssmt1)%levels(n)
         pertile_lhs = pertile_lhs + (y%ssmt1(n)%t(k)/typical_t_rms)**2

         y%ssmt1(n)%t(k) = y%ssmt1(n)%t(k) / typical_t_rms ** 2
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_ssmt1")

end subroutine da_check_xtoy_adjoint_ssmt1


