subroutine da_check_xtoy_adjoint_qscat(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n             ! Loop counter.
   
   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_qscat")

   do n=iv%info(qscat)%n1, iv%info(qscat)%n2
      if (iv%info(qscat)%proc_domain(1,n)) then
         adjtest_lhs = adjtest_lhs + (y%qscat(n)%u/typical_u_rms)**2 + (y%qscat(n)%v/typical_v_rms)**2
      end if

      pertile_lhs = pertile_lhs +(y%qscat(n)%u/typical_u_rms)**2 + (y%qscat(n)%v/typical_v_rms)**2

      y%qscat (n)%u = y%qscat (n)%u/typical_u_rms ** 2
      y%qscat (n)%v = y%qscat (n)%v/typical_v_rms ** 2
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_qscat")

end subroutine da_check_xtoy_adjoint_qscat


