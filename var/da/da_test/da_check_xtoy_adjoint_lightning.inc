subroutine da_check_xtoy_adjoint_lightning(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP) 
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_lightning")

   do n=iv%info(lightning)%n1, iv%info(lightning)%n2
      if (iv%info(lightning)%proc_domain(1,n)) then
         do k=1, iv%info(lightning)%levels(n)
            adjtest_lhs = adjtest_lhs + (y%lightning(n)%div(k)/typical_div_rms)**2 + (y%lightning(n)%qv(k)/typical_q_rms)**2
         end do
      end if

      do k=1, iv%info(lightning)%levels(n)
         pertile_lhs = pertile_lhs + (y%lightning(n)%qv(k)/typical_div_rms)**2 + (y%lightning(n)%qv(k)/typical_q_rms)**2
         y%lightning(n)%div(k) = y%lightning(n)%div(k)/typical_div_rms** 2	
         y%lightning(n)%qv(k) = y%lightning(n)%qv(k)/typical_q_rms** 2	 
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_lightning")

end subroutine da_check_xtoy_adjoint_lightning


