subroutine da_check_max_iv_chem_sfc(iv,ob, it, num_qcstat_conv)

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
   
   if (trace_use) call da_trace_entry("da_check_max_iv_chem_sfc")       


   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do ichem = num_chemic_surf, num_chemic_surf
   do n=iv%info(chemic_surf)%n1,iv%info(chemic_surf)%n2
      failed=.false.
      if ( iv%chemic_surf(n)%chem(ichem)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it, iv%info(chemic_surf), n, iv%chemic_surf(n)%chem(ichem), max_error_chemic_surf, failed)
         if ( iv%info(chemic_surf)%proc_domain(1,n) ) then
            num_qcstat_conv(1,chemic_surf,10,1)= num_qcstat_conv(1,chemic_surf,10,1) + 1
            if (failed) then
               num_qcstat_conv(2,chemic_surf,10,1)= num_qcstat_conv(2,chemic_surf,10,1) + 1
               write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
                  'Rainfall','Rain',iv%info(chemic_surf)%lat(1,n),iv%info(chemic_surf)%lon(1,n),'-8888.88'   
            end if
         end if
      end if
   end do
   end do

   if (trace_use) call da_trace_exit("da_check_max_iv_chem_sfc")       

end subroutine da_check_max_iv_chem_sfc
