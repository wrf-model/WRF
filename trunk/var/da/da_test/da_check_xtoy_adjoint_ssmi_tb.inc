subroutine da_check_xtoy_adjoint_ssmi_tb (iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: n             ! loop counter.
   real    :: var

   if (trace_use) call da_trace_entry("da_check_xtoy_adjoint_ssmi_tb")

   do n=iv%info(ssmi_tb)%n1, iv%info(ssmi_tb)%n2
      y%ssmi_tb(n)%tb19v = y%ssmi_tb(n)%tb19v/typical_tb19v_rms
      y%ssmi_tb(n)%tb19h = y%ssmi_tb(n)%tb19h/typical_tb19h_rms
      y%ssmi_tb(n)%tb22v = y%ssmi_tb(n)%tb22v/typical_tb22v_rms
      y%ssmi_tb(n)%tb37v = y%ssmi_tb(n)%tb37v/typical_tb37v_rms
      y%ssmi_tb(n)%tb37h = y%ssmi_tb(n)%tb37h/typical_tb37h_rms
      y%ssmi_tb(n)%tb85v = y%ssmi_tb(n)%tb85v/typical_tb85v_rms
      y%ssmi_tb(n)%tb85h = y%ssmi_tb(n)%tb85h/typical_tb85h_rms

       var = (y%ssmi_tb(n)%tb19v) ** 2 &
           + (y%ssmi_tb(n)%tb19h) ** 2 &
           + (y%ssmi_tb(n)%tb22v) ** 2 &
           + (y%ssmi_tb(n)%tb37v) ** 2 &
           + (y%ssmi_tb(n)%tb37h) ** 2 &
           + (y%ssmi_tb(n)%tb85v) ** 2 &
           + (y%ssmi_tb(n)%tb85h) ** 2 

      pertile_lhs = pertile_lhs + var

      if (iv%info(ssmi_tb)%proc_domain(1,n)) then
         adjtest_lhs = adjtest_lhs + var
      end if
      y%ssmi_tb(n)%tb19v = y%ssmi_tb(n)%tb19v/typical_tb19v_rms
      y%ssmi_tb(n)%tb19h = y%ssmi_tb(n)%tb19h/typical_tb19h_rms
      y%ssmi_tb(n)%tb22v = y%ssmi_tb(n)%tb22v/typical_tb22v_rms
      y%ssmi_tb(n)%tb37v = y%ssmi_tb(n)%tb37v/typical_tb37v_rms
      y%ssmi_tb(n)%tb37h = y%ssmi_tb(n)%tb37h/typical_tb37h_rms
      y%ssmi_tb(n)%tb85v = y%ssmi_tb(n)%tb85v/typical_tb85v_rms
      y%ssmi_tb(n)%tb85h = y%ssmi_tb(n)%tb85h/typical_tb85h_rms
   end do

   if (trace_use) call da_trace_exit("da_check_xtoy_adjoint_ssmi_tb")

end subroutine da_check_xtoy_adjoint_ssmi_tb


