subroutine da_check_max_iv_gpsref(iv,it, num_qcstat_conv, opt)        

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !
   !    Added argument: opt = 1, max error check, and count the total num. of obs;
   !                    opt = 2, only count the rejected num. of obs by all of QC
   !                             procedures specified for GPSREF.
   !        
   !                             Shu-Ya Chen and Y.-R. Guo, 12/30/2011  
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it       ! External iteration.
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)  

   integer                           :: k, n, ipr
   logical                           :: failed
   integer, intent(in) :: opt ! 1: only counting, 2: print out  (sychen add)
   integer, parameter :: qc_below = -31, qc_middle = -32, qc_above = -33
   integer, parameter :: qc_step1 = -34, qc_step2  = -35  ! refer to Poli et al. (2009)

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_gpsref")

   !----------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !----------------------------------------------------------------------------
         IF ( opt == 1 ) THEN
   do n = iv%info(gpsref)%n1,iv%info(gpsref)%n2
      do k = 1, iv%info(gpsref)%levels(n)
        call da_get_print_lvl(iv%gpsref(n)%p(k)%inv,ipr)
        if (iv%gpsref(n)%p(k)%inv == missing_r) ipr = 1
        failed=.false.
        if( iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer ) &
        call da_max_error_qc(it, iv%info(gpsref), n, iv%gpsref(n)%ref(k), max_error_ref, failed)  
        if( iv%info(gpsref)%proc_domain(k,n) ) &
                 num_qcstat_conv(1,gpsref,8,ipr) = num_qcstat_conv(1,gpsref,8,ipr) + 1
      end do
   end do
         ENDIF
  
         IF ( opt == 2 ) THEN
   do n = iv%info(gpsref)%n1,iv%info(gpsref)%n2
      do k = 1, iv%info(gpsref)%levels(n)
        call da_get_print_lvl(iv%gpsref(n)%p(k)%inv,ipr)
        if (iv%gpsref(n)%p(k)%inv == missing_r) ipr = 1
        failed=.false.
        if ( ( iv%gpsref(n)%ref(k)%qc == fails_error_max ) .or. &
             ( iv%gpsref(n)%ref(k)%qc == qc_below ) .or. &
             ( iv%gpsref(n)%ref(k)%qc == qc_middle ) .or. &
             ( iv%gpsref(n)%ref(k)%qc == qc_above ) .or. &
             ( iv%gpsref(n)%ref(k)%qc == qc_step1 ) .or. &
             ( iv%gpsref(n)%ref(k)%qc == qc_step2 ) .or. &
             ( iv%gpsref(n)%ref(k)%qc == missing_data ) ) then 
             failed=.true.
        endif
            if(failed) then
            num_qcstat_conv(2,gpsref,8,ipr) = num_qcstat_conv(2,gpsref,8,ipr) + 1
            write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2,I5)')&
             'gpsref',ob_vars(8),iv%info(gpsref)%lat(k,n), &
             iv%info(gpsref)%lon(k,n),0.01*iv%gpsref(n)%p(k)%inv, &
             iv%gpsref(n)%ref(k)%qc 
            end if
      end do
   end do
         ENDIF

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_gpsref")

end subroutine da_check_max_iv_gpsref


