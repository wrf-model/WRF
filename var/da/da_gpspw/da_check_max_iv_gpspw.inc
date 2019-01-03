subroutine da_check_max_iv_gpspw(iv, it, num_qcstat_conv) 

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-------------------------------------------------------------------------

   implicit none


   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

   logical :: failed 
   integer :: n

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_gpspw")

   ! TPW:

   do n=iv%info(gpspw)%n1,iv%info(gpspw)%n2

      failed=.false.
      if( iv%gpspw(n)%tpw%qc >= obs_qc_pointer ) then 
      call da_max_error_qc(it, iv%info(gpspw), n, iv%gpspw(n)%tpw, max_error_pw,failed)
      if( iv%info(gpspw)%proc_domain(1,n) ) then
       num_qcstat_conv(1,gpspw,7,1) = num_qcstat_conv(1,gpspw,7,1) + 1
      if(failed) then
       num_qcstat_conv(2,gpspw,7,1) = num_qcstat_conv(2,gpspw,7,1) + 1
       if ( write_rej_obs_conv ) then
       write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
       'gpspw',ob_vars(7),iv%info(gpspw)%lat(1,n),iv%info(gpspw)%lon(1,n),'1013.25'                     
       end if
      end if
      end if
      end if
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_gpspw")

end subroutine da_check_max_iv_gpspw


