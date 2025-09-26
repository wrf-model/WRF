subroutine da_check_xtoy_adjoint_gpspw(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n             ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_gpspw")

   do n=iv%info(gpspw)%n1, iv%info(gpspw)%n2
      if (iv%info(gpspw)%proc_domain(1,n)) then
         adjtest_lhs = adjtest_lhs + (y%gpspw(n) %tpw/typical_tpw_rms) ** 2
      end if

      pertile_lhs = pertile_lhs + (y%gpspw(n) %tpw/typical_tpw_rms) ** 2

      y%gpspw (n)%tpw = y%gpspw (n)%tpw/typical_tpw_rms ** 2
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_gpspw")

end subroutine da_check_xtoy_adjoint_gpspw


