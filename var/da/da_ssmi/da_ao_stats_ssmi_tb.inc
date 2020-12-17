subroutine da_ao_stats_ssmi_tb  (stats_unit, iv, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (inout) :: iv            ! iv
   type  (y_type), intent (in)    :: re            ! A - O

   integer                          :: ntb19v,ntb19h,ntb22v,ntb37v,ntb37h, &
                                       ntb85v,ntb85h
   integer                          :: n
   type (stats_ssmi_tb_type)        :: stats

   if (trace_use) call da_trace_entry("da_ao_stats_ssmi_tb")

   ntb19v = 0
   ntb19h = 0
   ntb22v = 0
   ntb37v = 0
   ntb37h = 0
   ntb85v = 0
   ntb85h = 0

   stats%maximum%tb19v = maxmin_type (missing_r, 0, 0)
   stats%maximum%tb19h = maxmin_type (missing_r, 0, 0)
   stats%maximum%tb22v = maxmin_type (missing_r, 0, 0)
   stats%maximum%tb37v = maxmin_type (missing_r, 0, 0)
   stats%maximum%tb37h = maxmin_type (missing_r, 0, 0)
   stats%maximum%tb85v = maxmin_type (missing_r, 0, 0)
   stats%maximum%tb85h = maxmin_type (missing_r, 0, 0)
   stats%minimum%tb19v = maxmin_type(-missing_r, 0, 0)
   stats%minimum%tb19h = maxmin_type(-missing_r, 0, 0)
   stats%minimum%tb22v = maxmin_type(-missing_r, 0, 0)
   stats%minimum%tb37v = maxmin_type(-missing_r, 0, 0)
   stats%minimum%tb37h = maxmin_type(-missing_r, 0, 0)
   stats%minimum%tb85v = maxmin_type(-missing_r, 0, 0)
   stats%minimum%tb85h = maxmin_type(-missing_r, 0, 0)
   stats%average = residual_ssmi_tb_type(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
   stats%rms_err = stats%average

   if (iv%info(ssmi_tb)%nlocal .gt. 0) then
      do n=1, iv%info(ssmi_tb)%nlocal 
         if (iv%info(ssmi_tb)%proc_domain(1,n)) then
            call da_stats_calculate (n, 0, iv%ssmi_tb(n)%tb19v%qc, &
               re%ssmi_tb(n)%tb19v, ntb19v,  &
               stats%minimum%tb19v,  stats%maximum%tb19v,&
               stats%average%tb19v,  stats%rms_err%tb19v)

            call da_stats_calculate (n, 0, iv%ssmi_tb(n)%tb19h%qc, &
               re%ssmi_tb(n)%tb19h, ntb19h,  &
               stats%minimum%tb19h,  stats%maximum%tb19h,&
               stats%average%tb19h,  stats%rms_err%tb19h)

            call da_stats_calculate (n, 0, iv%ssmi_tb(n)%tb22v%qc, &
               re%ssmi_tb(n)%tb22v, ntb22v,  &
               stats%minimum%tb22v,  stats%maximum%tb22v,&
               stats%average%tb22v,  stats%rms_err%tb22v)

            call da_stats_calculate (n, 0, iv%ssmi_tb(n)%tb37v%qc, &
               re%ssmi_tb(n)%tb37v, ntb37v,  &
               stats%minimum%tb37v,  stats%maximum%tb37v,&
               stats%average%tb37v,  stats%rms_err%tb37v)

            call da_stats_calculate (n, 0, iv%ssmi_tb(n)%tb37h%qc, &
               re%ssmi_tb(n)%tb37h, ntb37h,  &
               stats%minimum%tb37h,  stats%maximum%tb37h,&
               stats%average%tb37h,  stats%rms_err%tb37h)

            call da_stats_calculate (n, 0, iv%ssmi_tb(n)%tb85v%qc, &
               re%ssmi_tb(n)%tb85v, ntb85v,  &
               stats%minimum%tb85v,  stats%maximum%tb85v,&
               stats%average%tb85v,  stats%rms_err%tb85v)

            call  da_stats_calculate (n, 0, iv%ssmi_tb(n)%tb85h%qc, &
               re%ssmi_tb(n)%tb85h, ntb85h,  &
               stats%minimum%tb85h,  stats%maximum%tb85h,&
               stats%average%tb85h,  stats%rms_err%tb85h)
         end if    ! end if (iv%info(ssmi_tb)%proc_domain(1,n))
      end do
   end if

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (ntb19v)
   call da_proc_sum_int (ntb19h)
   call da_proc_sum_int (ntb22v)
   call da_proc_sum_int (ntb37v)
   call da_proc_sum_int (ntb37h)
   call da_proc_sum_int (ntb85v)
   call da_proc_sum_int (ntb85h)
   iv%nstats(ssmi_tb) = ntb19v + ntb19h + ntb22v + ntb37v + ntb37h + ntb85v + ntb85h

   call da_proc_stats_combine(stats%average%tb19v, stats%rms_err%tb19v, &
      stats%minimum%tb19v%value, stats%maximum%tb19v%value, &
      stats%minimum%tb19v%n, stats%maximum%tb19v%n, &
      stats%minimum%tb19v%l, stats%maximum%tb19v%l)

   call da_proc_stats_combine(stats%average%tb19h, stats%rms_err%tb19h, &
      stats%minimum%tb19h%value, stats%maximum%tb19h%value, &
      stats%minimum%tb19h%n, stats%maximum%tb19h%n, &
      stats%minimum%tb19h%l, stats%maximum%tb19h%l)

   call da_proc_stats_combine(stats%average%tb22v, stats%rms_err%tb22v, &
      stats%minimum%tb22v%value, stats%maximum%tb22v%value, &
      stats%minimum%tb22v%n, stats%maximum%tb22v%n, &
      stats%minimum%tb22v%l, stats%maximum%tb22v%l)

   call da_proc_stats_combine(stats%average%tb37v, stats%rms_err%tb37v, &
      stats%minimum%tb37v%value, stats%maximum%tb37v%value, &
      stats%minimum%tb37v%n, stats%maximum%tb37v%n, &
      stats%minimum%tb37v%l, stats%maximum%tb37v%l)

   call da_proc_stats_combine(stats%average%tb37h, stats%rms_err%tb37h, &
      stats%minimum%tb37h%value, stats%maximum%tb37h%value, &
      stats%minimum%tb37h%n, stats%maximum%tb37h%n, &
      stats%minimum%tb37h%l, stats%maximum%tb37h%l)

   call da_proc_stats_combine(stats%average%tb85v, stats%rms_err%tb85v, &
      stats%minimum%tb85v%value, stats%maximum%tb85v%value, &
      stats%minimum%tb85v%n, stats%maximum%tb85v%n, &
      stats%minimum%tb85v%l, stats%maximum%tb85v%l)

   call da_proc_stats_combine(stats%average%tb85h, stats%rms_err%tb85h, &
      stats%minimum%tb85h%value, stats%maximum%tb85h%value, &
      stats%minimum%tb85h%n, stats%maximum%tb85h%n, &
      stats%minimum%tb85h%l, stats%maximum%tb85h%l)

   if (rootproc) then
      if (ntb19v /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_tb'

         write(unit=stats_unit, fmt='(a/)') '   var           tb19v(m/s)     n'
 
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb19v

         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
            ' Minimum(n): ', stats%minimum%tb19v%value, stats%minimum%tb19v%n,     &
            ' Maximum(n): ', stats%maximum%tb19v%value, stats%maximum%tb19v%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%tb19v/real(ntb19v), &
            '    RMSE   : ', sqrt(stats%rms_err%tb19v/real(ntb19v))
      end if

      if (ntb19h /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_tb'

         write(unit=stats_unit, fmt='(a/)') '   var           tb19h(m/s)     n'

         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb19h

         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
            ' Minimum(n): ', stats%minimum%tb19h%value, stats%minimum%tb19h%n,     &
            ' Maximum(n): ', stats%maximum%tb19h%value, stats%maximum%tb19h%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%tb19h/real(ntb19h), &
            '    RMSE   : ', sqrt(stats%rms_err%tb19h/real(ntb19h))
      end if

      if (ntb22v /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_tb'

         write(unit=stats_unit, fmt='(a/)') '   var           tb22v(m/s)     n'

         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb22v

         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
            ' Minimum(n): ', stats%minimum%tb22v%value, stats%minimum%tb22v%n,     &
            ' Maximum(n): ', stats%maximum%tb22v%value, &
                                        stats%maximum%tb22v%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%tb22v/real(ntb22v), &
            '    RMSE   : ', sqrt(stats%rms_err%tb22v/real(ntb22v))
     end if

     if (ntb37v /= 0) then
        write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_tb'

        write(unit=stats_unit, fmt='(a/)') '   var           tb37v(m/s)     n'

        write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb37v

        write(unit=stats_unit, fmt='(a, f12.4,i5)') &
           ' Minimum(n): ', stats%minimum%tb37v%value, stats%minimum%tb37v%n,     &
           ' Maximum(n): ', stats%maximum%tb37v%value, stats%maximum%tb37v%n
        write(unit=stats_unit, fmt='(a, f12.4,5x)') &
           ' Average   : ', stats%average%tb37v/real(ntb37v), &
           '    RMSE   : ', sqrt(stats%rms_err%tb37v/real(ntb37v))
      end if

      if (ntb37h /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_tb'

         write(unit=stats_unit, fmt='(a/)') '   var           tb37h(m/s)     n'
  
         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb37h

         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
            ' Minimum(n): ', stats%minimum%tb37h%value, stats%minimum%tb37h%n,     &
            ' Maximum(n): ', stats%maximum%tb37h%value, stats%maximum%tb37h%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%tb37h/real(ntb37h), &
            '    RMSE   : ', sqrt(stats%rms_err%tb37h/real(ntb37h))
      end if

      if (ntb85v /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_tb'

         write(unit=stats_unit, fmt='(a/)') '   var           tb85v(m/s)     n' 

         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb85v

         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
            ' Minimum(n): ', stats%minimum%tb85v%value, stats%minimum%tb85v%n,     &
            ' Maximum(n): ', stats%maximum%tb85v%value, stats%maximum%tb85v%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%tb85v/real(ntb85v), &
            '    RMSE   : ', sqrt(stats%rms_err%tb85v/real(ntb85v))
      end if

      if (ntb85h /= 0) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of AO for ssmi_tb'

         write(unit=stats_unit, fmt='(a/)') '   var           tb85h(m/s)     n'

         write(unit=stats_unit, fmt='(a,i14)') '  Number: ', ntb85h

         write(unit=stats_unit, fmt='(a, f12.4,i5)') &
            ' Minimum(n): ', stats%minimum%tb85h%value, stats%minimum%tb85h%n,     &
            ' Maximum(n): ', stats%maximum%tb85h%value, stats%maximum%tb85h%n
         write(unit=stats_unit, fmt='(a, f12.4,5x)') &
            ' Average   : ', stats%average%tb85h/real(ntb85h), &
            '    RMSE   : ', sqrt(stats%rms_err%tb85h/real(ntb85h))
      end if
   end if

   if (trace_use) call da_trace_exit("da_ao_stats_ssmi_tb")

end subroutine da_ao_stats_ssmi_tb


