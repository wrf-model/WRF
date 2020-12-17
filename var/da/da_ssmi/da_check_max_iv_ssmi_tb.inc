subroutine da_check_max_iv_ssmi_tb(iv, it)  

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it

   logical :: failed
   integer :: n

   if (trace_use) call da_trace_entry("da_check_max_iv_ssmi_tb")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(ssmi_tb)%n1, iv%info(ssmi_tb)%n2
      ! Tb19h

      call da_max_error_qc(it, iv%info(ssmi_tb), n, iv%ssmi_tb(n)%tb19h, max_error_tb, failed)

      ! Tb19v

      call da_max_error_qc(it, iv%info(ssmi_tb), n, iv%ssmi_tb(n)%tb19v, max_error_tb, failed)

      ! Tb22v

      call da_max_error_qc(it, iv%info(ssmi_tb), n, iv%ssmi_tb(n)%tb22v, max_error_tb, failed)

      ! Tb37h

      call da_max_error_qc(it, iv%info(ssmi_tb), n, iv%ssmi_tb(n)%tb37h, max_error_tb, failed)

      ! Tb37v

      call da_max_error_qc(it, iv%info(ssmi_tb), n, iv%ssmi_tb(n)%tb37v, max_error_tb, failed)

      ! Tb85h

      call da_max_error_qc(it, iv%info(ssmi_tb), n, iv%ssmi_tb(n)%tb85h, max_error_tb, failed)

      ! Tb85v

      call da_max_error_qc(it, iv%info(ssmi_tb), n, iv%ssmi_tb(n)%tb85v,max_error_tb, failed)
   end do

   if (trace_use) call da_trace_exit("da_check_max_iv_ssmi_tb")

end subroutine da_check_max_iv_ssmi_tb


