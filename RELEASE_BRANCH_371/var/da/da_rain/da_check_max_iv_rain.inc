subroutine da_check_max_iv_rain(iv,ob, it, num_qcstat_conv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration 
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),  intent(in)    :: ob      ! Observation structure.

   logical :: failed 
   integer :: n
   
   if (trace_use) call da_trace_entry("da_check_max_iv_rain")       


   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(rain)%n1,iv%info(rain)%n2
      failed=.false.
      if ( iv%rain(n)%rain%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it, iv%info(rain), n, iv%rain(n)%rain, max_error_rain, failed)
         if ( iv%info(rain)%proc_domain(1,n) ) then
            num_qcstat_conv(1,rain,10,1)= num_qcstat_conv(1,rain,10,1) + 1
            if (failed) then
               num_qcstat_conv(2,rain,10,1)= num_qcstat_conv(2,rain,10,1) + 1
               write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
                  'Rainfall','Rain',iv%info(rain)%lat(1,n),iv%info(rain)%lon(1,n),'-8888.88'   
            end if
         end if
      end if
   end do

   if (trace_use) call da_trace_exit("da_check_max_iv_rain")       

end subroutine da_check_max_iv_rain
