subroutine da_check_max_iv_airsr(iv, it,num_qcstat_conv)     

   !-------------------------------------------------------------------------
   ! Purpose: Applies max error check on AIRS retrievals
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: k,n, ipr
   logical :: failed
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_airsr")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(airsr)%n1,iv%info(airsr)%n2
      do k = 1, iv%info(airsr)%levels(n)
        call da_get_print_lvl(iv%airsr(n)%p(k),ipr)
        failed = .false.
        if( iv%airsr(n)%t(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it,iv%info(airsr), n, iv%airsr(n)%t(k), max_error_t ,failed)
        if( iv%info(airsr)%proc_domain(k,n) ) then
          num_qcstat_conv(1,airsr,3,ipr) = num_qcstat_conv(1,airsr,3,ipr) + 1
         if(failed)then
          num_qcstat_conv(2,airsr,3,ipr) = num_qcstat_conv(2,airsr,3,ipr) + 1
        write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
        'airsr',ob_vars(3),iv%info(airsr)%lat(k,n),iv%info(airsr)%lon(k,n),0.01*iv%airsr(n)%p(k)
         endif
        endif
        endif

        failed = .false.
        if( iv%airsr(n)%q(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it,iv%info(airsr), n, iv%airsr(n)%q(k), max_error_q ,failed)
         if( iv%info(airsr)%proc_domain(k,n) ) then
           num_qcstat_conv(1,airsr,4,ipr) = num_qcstat_conv(1,airsr,4,ipr) + 1
         if(failed)then
          num_qcstat_conv(2,airsr,4,ipr) = num_qcstat_conv(2,airsr,4,ipr) + 1
        write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
        'airsr',ob_vars(4),iv%info(airsr)%lat(k,n),iv%info(airsr)%lon(k,n),0.01*iv%airsr(n)%p(k)
         endif
        endif
        endif
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_airsr")
end subroutine da_check_max_iv_airsr
