subroutine da_ao_stats_lightning (stats_unit, iv, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)  
   !-----------------------------------------------------------------------
   
   implicit none

   integer,        intent (in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (inout) :: iv            ! iv
   type (y_type),  intent (in)    :: re            ! A - O

   type (stats_lightning_type) :: stats
   integer                 :: nw, ndiv, nqv
   integer                 :: n, k

   if (trace_use_dull) call da_trace_entry("da_ao_stats_lightning")

   nw   = 0
   ndiv = 0
   nqv  = 0

   stats%maximum%w   = maxmin_type (missing_r, 0, 0)
   stats%maximum%div = maxmin_type (missing_r, 0, 0)
   stats%maximum%qv  = maxmin_type (missing_r, 0, 0)
   stats%maximum%w   = maxmin_type(-missing_r, 0, 0)
   stats%minimum%div = maxmin_type(-missing_r, 0, 0)
   stats%minimum%qv  = maxmin_type(-missing_r, 0, 0)
   
   stats%average = residual_lightning1_type(0.0, 0.0, 0.0)
   stats%rms_err = stats%average

   do n = 1, iv%info(lightning)%nlocal
      if(iv%info(lightning)%proc_domain(1,n)) then
        do k = 1, iv%info(lightning)%levels(n)
 
           if(use_lightning_w) then
             call da_stats_calculate (n, k, iv%lightning(n)%w(k)%qc, &
                  re%lightning(n)%w(k), nw, &
                  stats%minimum%w, stats%maximum%w, &
                  stats%average%w, stats%rms_err%w)
           end if

           if(use_lightning_div) then
             call da_stats_calculate (n, k, iv%lightning(n)%div(k)%qc, & 
                  re%lightning(n)%div(k), ndiv, &
                  stats%minimum%div, stats%maximum%div, &
                  stats%average%div, stats%rms_err%div)
	   end if
			
	   if(use_lightning_qv) then
             call da_stats_calculate (n, k, iv%lightning(n)%qv(k)%qc, & 
                  re%lightning(n)%qv(k), nqv, &
                  stats%minimum%qv, stats%maximum%qv, &
                  stats%average%qv, stats%rms_err%qv)
           end if			
        end do 
      end if
   end do

   write(unit=stdout, fmt='(A)') 'finish da_ao_stats_lightning'

   if (trace_use_dull) call da_trace_exit("da_ao_stats_lightning")

end subroutine da_ao_stats_lightning
