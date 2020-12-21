subroutine da_set_run_hours ()

   !-----------------------------------------------------------------------
   ! Purpose: Set run hours to match adtl_run_hours
   !    Author: JJ Guerrette,  CU Boulder,  Date: 07/25/2016
   !-----------------------------------------------------------------------

   implicit none
   
   integer :: alarmid
   integer :: end_year0,end_month0,end_day0,end_hour0
   integer :: end_year,end_month,end_day,end_hour
   integer :: start_year0,start_month0
   integer , dimension(12) :: days_in_month = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   if (trace_use_dull) call da_trace_entry("da_set_run_hours")

   if ( run_hours .gt. adtl_run_hours .and. run_days .eq. 0 ) then
      start_year0 = model_config_rec%start_year(head_grid%id)
      start_month0 = model_config_rec%start_month(head_grid%id)

      end_year0 = model_config_rec%end_year(head_grid%id)
      end_month0 = model_config_rec%end_month(head_grid%id)
      end_day0 = model_config_rec%end_day(head_grid%id)
      end_hour0 = model_config_rec%end_hour(head_grid%id)

      end_year  = end_year0
      end_month = end_month0
      end_day   = end_day0
      end_hour  = end_hour0

      end_hour = end_hour - ( run_hours - adtl_run_hours )
      if (end_hour .lt. 0) then
         end_day = end_day - 1 + end_hour / 24
         end_hour = 24 - mod(-end_hour,24)

         end_month = end_month0
         end_year = end_year0
         do while (end_day .lt. 1)
            end_month = end_month - 1
            !Account for leap years
            if(end_month .eq. 2) then
               if ( mod(end_year,4) .eq. 0 )then
                  if ( mod(end_year,100) .eq. 0 ) then
                     if ( mod(end_year,400) .eq. 0 ) then
                        days_in_month(2) = 29
                     else
                        days_in_month(2) = 28
                     end if
                  else
                     days_in_month(2) = 29
                  end if
               else
                  days_in_month(2) = 28
               end if
            end if
            if ( end_month .eq. 0 ) then
               end_month = 12
               end_year = end_year - 1
            end if
            end_day = end_day + days_in_month (end_month)
            if ( end_year .lt. start_year0 .or. &
                (end_year .eq. start_year0 .and. end_month .lt. start_month0) ) then
               end_month = start_month0
               end_year = start_year0
               exit
            end if
         end do
      end if

      model_config_rec%end_year(head_grid%id) = end_year
      model_config_rec%end_month(head_grid%id) = end_month
      model_config_rec%end_day(head_grid%id) = end_day
      model_config_rec%end_hour(head_grid%id) = end_hour

      model_config_rec%run_hours = adtl_run_hours

      IF ( ASSOCIATED( head_grid%domain_clock ) ) THEN
        IF ( head_grid%domain_clock_created ) THEN
          CALL WRFU_ClockDestroy( head_grid%domain_clock )
          head_grid%domain_clock_created = .FALSE.
        ENDIF
      ENDIF
      IF ( ASSOCIATED( head_grid%alarms ) .AND. &
           ASSOCIATED( head_grid%alarms_created ) ) THEN
        DO alarmid = 1, MAX_WRF_ALARMS
          IF ( head_grid%alarms_created( alarmid ) ) THEN
            CALL WRFU_AlarmDestroy( head_grid%alarms( alarmid ) )
            head_grid%alarms_created( alarmid ) = .FALSE.
          ENDIF
        ENDDO
      ENDIF
      call Setup_Timekeeping ( head_grid )
   end if

end subroutine da_set_run_hours
