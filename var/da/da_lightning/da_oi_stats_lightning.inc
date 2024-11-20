subroutine da_oi_stats_lightning (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)   
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in) :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (in) :: iv            ! OI

   type (stats_lightning_type) :: stats
   integer                 :: nw, nqv, ndiv
   integer                 :: n, k

   if (trace_use_dull) call da_trace_entry("da_oi_stats_lightning")

   nw   = 0
   ndiv = 0
   nqv  = 0
   
   stats%maximum%w   = maxmin_type(missing_r, 0, 0)   
   stats%minimum%w   = maxmin_type(-missing_r, 0, 0)
   stats%maximum%div = maxmin_type(missing_r, 0, 0)   
   stats%minimum%div = maxmin_type(-missing_r, 0, 0)
   stats%maximum%qv  = maxmin_type(missing_r, 0, 0)
   stats%minimum%qv  = maxmin_type(-missing_r, 0, 0)   

   stats%average = residual_lightning1_type(0.0, 0.0, 0.0)
   stats%rms_err = stats%average

   do n = 1, iv%info(lightning)%nlocal
      if(iv%info(lightning)%proc_domain(1,n)) then
        do k = 1, iv%info(lightning)%levels(n)
           if(use_lightning_w) then
             call da_stats_calculate(iv%info(lightning)%obs_global_index(n), &
                k, iv%lightning(n)%w(k)%qc, &
                iv%lightning(n)%w(k)%inv, nw, &
                stats%minimum%w, stats%maximum%w, &
                stats%average%w, stats%rms_err%w)
           end if

           if(use_lightning_div) then
             call da_stats_calculate(iv%info(lightning)%obs_global_index(n), &
                k, iv%lightning(n)%div(k)%qc, &
                iv%lightning(n)%div(k)%inv, ndiv, &
                stats%minimum%div, stats%maximum%div, &
                stats%average%div, stats%rms_err%div)
	   end if		    
			
           if(use_lightning_qv) then
             call da_stats_calculate(iv%info(lightning)%obs_global_index(n), &
                k, iv%lightning(n)%qv(k)%qc, &
                iv%lightning(n)%qv(k)%inv, nqv, &
                stats%minimum%qv, stats%maximum%qv, &
                stats%average%qv, stats%rms_err%qv)
	   end if			
			
        end do
      end if  
   end do

   ! Do inter-processor communication to gather statistics.
   if (use_lightning_w) then
      call da_proc_sum_int (nw)
      call da_proc_stats_combine(stats%average%w, stats%rms_err%w, &
         stats%minimum%w%value, stats%maximum%w%value, &
         stats%minimum%w%n, stats%maximum%w%n, &
         stats%minimum%w%l, stats%maximum%w%l)
   end if
         
   if (use_lightning_div) then
      call da_proc_sum_int (ndiv)
      call da_proc_stats_combine(stats%average%div, stats%rms_err%div, &
         stats%minimum%div%value, stats%maximum%div%value, &
         stats%minimum%div%n, stats%maximum%div%n, &
         stats%minimum%div%l, stats%maximum%div%l)
   end if 
   
   if (use_lightning_qv) then
      call da_proc_sum_int (nqv)
      call da_proc_stats_combine(stats%average%qv, stats%rms_err%qv, &
         stats%minimum%qv%value, stats%maximum%qv%value, &
         stats%minimum%qv%n, stats%maximum%qv%n, &
         stats%minimum%qv%l, stats%maximum%qv%l)
   end if

   if (rootproc) then
      if ( nw /= 0 .or. ndiv /= 0 .or. nqv /= 0 ) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for lightning'
         call da_print_stats_lightning(stats_unit, nw, ndiv, nqv, stats)
      end if
   end if
   
   if (trace_use_dull) call da_trace_exit("da_oi_stats_lightning")

end subroutine da_oi_stats_lightning
