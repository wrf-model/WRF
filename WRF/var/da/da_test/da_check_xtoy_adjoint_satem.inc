subroutine da_check_xtoy_adjoint_satem(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_satem")

   do n=iv%info(satem)%n1, iv%info(satem)%n2
      if (iv%info(satem)%proc_domain(1,n)) then
         do k=1, iv%info(satem)%levels(n)
            adjtest_lhs = adjtest_lhs + (y%satem(n)%thickness(k)/typical_thickness_rms)**2
         end do
      end if

      do k=1, iv%info(satem)%levels(n)
         pertile_lhs = pertile_lhs + (y%satem(n)%thickness(k)/typical_thickness_rms)**2
         y%satem(n)%thickness(k) = y%satem(n)%thickness(k) / typical_thickness_rms ** 2
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_satem")

end subroutine da_check_xtoy_adjoint_satem


