subroutine da_check_max_iv_satem(iv, it, num_qcstat_conv)

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

   integer  :: k,n, ipr
   logical  :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_satem")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(satem)%n1,iv%info(satem)%n2
      do k = 1, iv%info(satem)%levels(n)
         call da_get_print_lvl(iv%satem(n)%p(k),ipr)
         failed=.false.
         if ( iv%satem(n)%thickness(k)%qc >= obs_qc_pointer ) then 
         ! Thickness
            call da_max_error_qc(it, iv%info(satem), n, iv%satem(n)%thickness(k),& 
                                 max_error_thickness, failed)
            if( iv%info(satem)%proc_domain(k,n) ) then
            num_qcstat_conv(1,satem,9,ipr-1) = num_qcstat_conv(1,satem,9,ipr-1) + 1
            if (failed) then
               num_qcstat_conv(2,satem,9,ipr-1) = num_qcstat_conv(2,satem,9,ipr-1) + 1
               if ( write_rej_obs_conv ) then
               write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                     'satem',ob_vars(9),iv%info(satem)%lat(k,n),iv%info(satem)%lon(k,n),0.01*iv%satem(n)%p(k)
               end if
            end if
            end if
            end if
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_satem")

end subroutine da_check_max_iv_satem
