subroutine da_check_max_iv_polaramv(iv,it,num_qcstat_conv)   

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type),    intent(inout) :: iv
   integer,          intent(in)    :: it      ! Outer iteration
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer  :: k,n, ipr
   logical  :: failed
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_polaramv")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(polaramv)%n1,iv%info(polaramv)%n2
      do k = 1, iv%info(polaramv)%levels(n)
        call da_get_print_lvl(iv%polaramv(n)%p(k),ipr)
        failed=.false.
        if( iv%polaramv(n)%u(k)%qc >= obs_qc_pointer )  then
         call da_max_error_qc(it, iv%info(polaramv), n, iv%polaramv(n)%u(k), max_error_uv,failed)
         if( iv%info(polaramv)%proc_domain(k,n) ) then
         num_qcstat_conv(1,polaramv,1,ipr) = num_qcstat_conv(1,polaramv,1,ipr) + 1
         if(failed) then
         num_qcstat_conv(2,polaramv,1,ipr) = num_qcstat_conv(2,polaramv,1,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'polaramv',ob_vars(1),iv%info(polaramv)%lat(k,n),iv%info(polaramv)%lon(k,n),0.01*iv%polaramv(n)%p(k)
         end if
         end if
         end if

        failed=.false.
        if( iv%polaramv(n)%v(k)%qc >= obs_qc_pointer )  then
         call da_max_error_qc(it, iv%info(polaramv), n, iv%polaramv(n)%v(k), max_error_uv,failed)
         if( iv%info(polaramv)%proc_domain(k,n) ) then
         num_qcstat_conv(1,polaramv,2,ipr) = num_qcstat_conv(1,polaramv,2,ipr) + 1
         if(failed)then
           num_qcstat_conv(2,polaramv,2,ipr) = num_qcstat_conv(2,polaramv,2,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'polaramv',ob_vars(2),iv%info(polaramv)%lat(k,n),iv%info(polaramv)%lon(k,n),0.01*iv%polaramv(n)%p(k)
         end if
         end if
         end if
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_polaramv")

end subroutine da_check_max_iv_polaramv      
