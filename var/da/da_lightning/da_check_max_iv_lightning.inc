subroutine da_check_max_iv_lightning(iv,ob, it)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration 
   type(y_type),  intent(in)    :: ob      ! Observation structure.

   logical :: failed 
   integer :: n, k
   
   if (trace_use) call da_trace_entry("da_check_max_iv_lightning")       

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(lightning)%n1,iv%info(lightning)%n2
      do k = 1, iv%info(lightning)%levels(n)
         failed = .false.
         if(iv%lightning(n)%w(k)%qc >= obs_qc_pointer) then 
            call da_max_error_qc(it, iv%info(lightning), n, iv%lightning(n)%w(k),max_error_lda_w, failed)
            if(iv%info(lightning)%proc_domain(k,n)) then
              if(failed) then
                write(qcstat_conv_unit,'(2x, a10, 2x, a10, 4f12.3, a12)')&
                  'Lightning','lightning',iv%info(lightning)%lat(k,n),iv%info(lightning)%lon(k,n), iv%lightning(n)%w(k)%inv, ob%lightning(n)%w(k)
              end if
            end if
         end if

         failed = .false.
         if(iv%lightning(n)%div(k)%qc >= obs_qc_pointer) then
            call da_max_error_qc(it, iv%info(lightning), n, iv%lightning(n)%div(k),max_error_lda_div, failed)
            if(iv%info(lightning)%proc_domain(k,n)) then
              if(failed) then
                write(qcstat_conv_unit,'(2x, a10, 2x, a10, 4f12.3, a12)')&
                  'Lightning','lightning',iv%info(lightning)%lat(k,n),iv%info(lightning)%lon(k,n), iv%lightning(n)%div(k)%inv, ob%lightning(n)%div(k)
              end if
            end if
         end if
		 
        failed = .false.
         if(iv%lightning(n)%qv(k)%qc >= obs_qc_pointer) then 
            call da_max_error_qc(it, iv%info(lightning), n, iv%lightning(n)%qv(k),max_error_lda_qv, failed)
            if(iv%info(lightning)%proc_domain(k,n)) then
              if(failed)then
                write(qcstat_conv_unit,'(2x,a10,2x,a10,4f12.2,a12)')&
                  'Lightning','lightning',iv%info(lightning)%lat(k,n),iv%info(lightning)%lon(k,n), iv%lightning(n)%qv(k)%inv, ob%lightning(n)%qv(k)
              end if
            end if
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_check_max_iv_lightning")       

end subroutine da_check_max_iv_lightning
