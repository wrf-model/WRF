subroutine da_check_max_iv_chem_surf(iv,ob, it, num_qcstat_conv)

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
   integer :: n, ichem
   
   if (trace_use) call da_trace_entry("da_check_max_iv_chem_surf")       


   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do ichem = 1, num_chem_surf-1
   do n=iv%info(chem_surf)%n1,iv%info(chem_surf)%n2
      failed=.false.
      if ( iv%chem_surf(n)%chem(ichem)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it, iv%info(chem_surf), n, iv%chem_surf(n)%chem(ichem), max_error_chem_surf, failed)
         if ( iv%info(chem_surf)%proc_domain(1,n) ) then
            num_qcstat_conv(1,chem_surf,10,1)= num_qcstat_conv(1,chem_surf,10,1) + 1
            if (failed) then
               num_qcstat_conv(2,chem_surf,10,1)= num_qcstat_conv(2,chem_surf,10,1) + 1
!               write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
!                  'Rainfall','Rain',iv%info(chem_surf)%lat(1,n),iv%info(chem_surf)%lon(1,n),'-8888.88'   
            end if
         end if
      end if
   end do
   end do

   if (trace_use) call da_trace_exit("da_check_max_iv_chem_surf")       

end subroutine da_check_max_iv_chem_surf
