subroutine da_check_max_iv_ssmt1(iv, it, num_qcstat_conv)              

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    ::  it      ! outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)



   integer ::  k,n, ipr
   logical :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_ssmt1")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(ssmt1)%n1,iv%info(ssmt1)%n2
      do k = 1, iv%info(ssmt1)%levels(n)
         call da_get_print_lvl(iv%ssmt1(n)%p(k),ipr)

        failed= .false.
        if( iv%ssmt1(n)%t(k)%qc >= obs_qc_pointer ) then 
        call da_max_error_qc(it, iv%info(ssmt1), n, iv%ssmt1(n)%t(k), max_error_t, failed)
        if( iv%info(ssmt1)%proc_domain(k,n) ) then
         num_qcstat_conv(1,ssmt1,3,ipr) = num_qcstat_conv(1,ssmt1,3,ipr) + 1
        if(failed) then
        num_qcstat_conv(2,ssmt1,3,ipr) = num_qcstat_conv(2,ssmt1,3,ipr) + 1
        if ( write_rej_obs_conv ) then
        write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
        'ssmt1',ob_vars(3),iv%info(ssmt1)%lat(k,n),iv%info(ssmt1)%lon(k,n),0.01*iv%ssmt1(n)%p(k)
        endif
        endif
        endif
        endif

      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_ssmt1")

end subroutine da_check_max_iv_ssmt1


