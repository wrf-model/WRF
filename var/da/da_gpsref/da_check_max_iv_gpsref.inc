subroutine da_check_max_iv_gpsref(iv,it, num_qcstat_conv)        

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it       ! External iteration.
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)  

   integer                           :: k,n, ipr
   logical                           :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_gpsref")

   !----------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !----------------------------------------------------------------------------

   do n = iv%info(gpsref)%n1,iv%info(gpsref)%n2
      do k = 1, iv%info(gpsref)%levels(n)
        call da_get_print_lvl(iv%gpsref(n)%p(k)%inv,ipr)
        failed=.false.
        if( iv%gpsref(n)%ref(k)%qc >= obs_qc_pointer )  then
        call da_max_error_qc(it, iv%info(gpsref), n, iv%gpsref(n)%ref(k), max_error_ref, failed)  
        if( iv%info(gpsref)%proc_domain(k,n) ) then
                   num_qcstat_conv(1,gpsref,8,ipr) = num_qcstat_conv(1,gpsref,8,ipr) + 1
        if(failed) then
            num_qcstat_conv(2,gpsref,8,ipr) = num_qcstat_conv(2,gpsref,8,ipr) + 1
            write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
             'gpsref',ob_vars(8),iv%info(gpsref)%lat(k,n),iv%info(gpsref)%lon(k,n),0.01*iv%gpsref(n)%p(k)%inv
        end if
        end if
        end if

      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_gpsref")

end subroutine da_check_max_iv_gpsref


