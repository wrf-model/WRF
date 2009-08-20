SUBROUTINE adapt_timestep(grid, config_flags)

!--------------------------------------------------------------------------
!<DESCRIPTION>
!<pre>
!
! This routine sets the time step based on the cfl condition.  It's used to
!   dynamically adapt the timestep as the model runs.
!
! T. Hutchinson, WSI
! March 2007
!
!</pre>
!</DESCRIPTION>
!--------------------------------------------------------------------------

! Driver layer modules
  USE module_domain
  USE module_configure
  USE module_dm
  USE module_bc_em

  IMPLICIT NONE

  TYPE(domain) , TARGET , INTENT(INOUT)      :: grid
  TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  LOGICAL                                    :: use_last2
  REAL                                       :: curr_secs
  REAL                                       :: max_increase_factor
  REAL                                       :: time_to_output, &
                                                time_to_bc
  INTEGER                                    :: idex=0, jdex=0
  INTEGER                                    :: rc
  TYPE(WRFU_TimeInterval)                    :: tmpTimeInterval, dtInterval
  INTEGER                                    :: num_small_steps
  integer                                    :: tile
  LOGICAL                                    :: stepping_to_bc
  INTEGER                                    :: bc_time, output_time
  double precision                           :: dt
  INTEGER, PARAMETER                         :: precision = 100
  INTEGER                                    :: dt_num, dt_den, dt_whole
  REAL                                       :: factor
  INTEGER                                    :: num, den
  TYPE(WRFU_TimeInterval)                    :: last_dtInterval
  REAL                                       :: real_time

  !
  ! If use_last2 is true, this routine will use the time step
  !   from 2 steps ago to compute the next time step.  This
  !   is used along with step_to_output and step_to_bc

  use_last2 = .FALSE.

  !
  ! For nests, we only want to change nests' time steps when the time is 
  !    conincident with the parent's time.  So, if dtbc is not
  !    zero, simply return and leave the last time step in place.
  !
  if (config_flags%nested) then
     if (abs(grid%dtbc) > 0.0001) then
        return
     endif
  endif

  last_dtInterval = grid%last_dtInterval

  !
  ! Get current time
  !

  tmpTimeInterval = domain_get_current_time ( grid ) - &
       domain_get_start_time ( grid )

  !
  ! Calculate current time in seconds since beginning of model run.
  !   Unfortunately, ESMF does not seem to have a way to return
  !   floating point seconds based on a TimeInterval.  So, we will
  !   calculate it here--but, this is not clean!!
  !
  curr_secs = real_time(tmpTimeInterval)

  !
  ! Calculate the maximum allowable increase in the time step given
  !   the user input max_step_increase_pct value and the nest ratio.
  !
  if (config_flags%nested) then
     max_increase_factor = 1. + &
          grid%parent_time_step_ratio * grid%max_step_increase_pct / 100.     
  else
     max_increase_factor = 1. + grid%max_step_increase_pct / 100.
  endif

  !
  ! If this is the first time step of the model run (indicated by current_time 
  !    eq start_time), then set the time step to the input starting_time_step.
  !
  ! Else, calculate the time step based on cfl.
  !
  if (domain_get_current_time ( grid ) .eq. domain_get_start_time ( grid )) then
     CALL WRFU_TimeIntervalSet(dtInterval, Sn=grid%starting_time_step, Sd=1)
     curr_secs = 0
     CALL WRFU_TimeIntervalSet(last_dtInterval, Sn=0, Sd=1)

  else
     if (grid%max_cfl_val < 0.001) then 
        !
        ! If the max_cfl_val is small, then we increase dtInterval the maximum
        !    amount allowable.
        !
        num = INT(max_increase_factor * precision + 0.5)
        den = precision
        dtInterval = last_dtInterval * num / den

     else
        !
        ! If the max_cfl_val is greater than the user input target cfl, we 
        !    reduce the time step, 
        ! else, we increase it.
        !
        if (grid%max_cfl_val .gt. grid%target_cfl) then
           !
           ! If we are reducing the time step, we go below target cfl by half the
           !   difference between max and target (or 75% of the last time step,
           !   which ever is greater).
           !   This tends to keep the model more stable.
           !
           
           factor = MAX ( 0.75 , ( (grid%target_cfl - 0.5 * &
                (grid%max_cfl_val - grid%target_cfl) ) / grid%max_cfl_val ) )
           num = INT(factor * precision + 0.5)
           den = precision

           dtInterval = last_dtInterval * num / den

        else
           !
           ! Linearly increase dtInterval (we'll limit below)
           !
           
           factor = grid%target_cfl / grid%max_cfl_val
           num = INT(factor * precision + 0.5)
           den = precision
           dtInterval = last_dtInterval * num / den
        endif

     endif

  endif

  ! Limit the increase of dtInterval to the specified input limit

  num = NINT( max_increase_factor * precision )
  den = precision
  tmpTimeInterval = last_dtInterval * num / den
  if ( (domain_get_current_time ( grid ) .ne. domain_get_start_time ( grid )) &
       .and. (dtInterval .gt. tmpTimeInterval ) ) then
     dtInterval = tmpTimeInterval
  endif

  !
  ! Here, we round off dtInterval to nearest 1/100.  This prevents
  !    the denominator from getting too large and causing overflow.
  !
  dt = real_time(dtInterval)
  num = NINT(dt * precision)
  den = precision
  CALL WRFU_TimeIntervalSet(dtInterval, Sn=num, Sd=den)

  ! Limit the maximum dtInterval based on user input

  CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%max_time_step, Sd=1)
  if (dtInterval .gt. tmpTimeInterval ) then
     dtInterval = tmpTimeInterval
  endif

  ! Limit the minimum dtInterval based on user input.

  CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%min_time_step, Sd=1)
  if (dtInterval .lt. tmpTimeInterval ) then
     dtInterval = tmpTimeInterval
  endif

  !
  ! Now, if this is a nest, we round down the time step to the nearest 
  !   value that divides evenly into the parent time step.
  !
  if (config_flags%nested) then
     ! We'll calculate real numbers to get the number of small steps:
     
     dt = real_time(dtInterval)

     num_small_steps = CEILING( grid%parents(1)%ptr%dt / dt )

#ifdef DM_PARALLEL
     call wrf_dm_maxval(num_small_steps, idex, jdex)
#endif
     dtInterval = domain_get_time_step(grid%parents(1)%ptr) / &
          num_small_steps
  endif

  !
  ! Setup the values for several variables from the tile with the
  !   minimum dt.
  !
  dt = real_time(dtInterval)

#ifdef DM_PARALLEL
  call wrf_dm_mintile_double(dt, tile)
  CALL WRFU_TimeIntervalGet(dtInterval,Sn=dt_num,Sd=dt_den,S=dt_whole)
  call wrf_dm_tile_val_int(dt_num, tile)
  call wrf_dm_tile_val_int(dt_den, tile)
  call wrf_dm_tile_val_int(dt_whole, tile)
  CALL WRFU_TimeIntervalSet(dtInterval, Sn = dt_whole*dt_den + dt_num, Sd = dt_den)

  call wrf_dm_maxtile_real(grid%max_cfl_val, tile)
  call wrf_dm_maxtile_real(grid%max_vert_cfl, tile)
  call wrf_dm_maxtile_real(grid%max_horiz_cfl, tile)
#endif

  !
  ! Assure that we fall on a BC time.  Due to a bug in WRF, the time
  !   step must fall on the boundary times.  Only modify the dtInterval
  !   when this is not the first time step on this domain.
  !

  if ( real_time(domain_get_current_time(grid) - domain_get_start_time(grid)) .GT. 0.01 ) THEN
     time_to_bc = grid%interval_seconds - grid%dtbc
     num = INT(time_to_bc * precision + 0.5)
     den = precision
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=num, Sd=den)
      
     if ( ( tmpTimeInterval .LT. dtInterval * 2 ) .and. &
          ( tmpTimeInterval .GT. dtInterval ) ) then
        dtInterval = tmpTimeInterval / 2
        
        use_last2 = .TRUE.
        stepping_to_bc = .true.
   
     elseif (tmpTimeInterval .LE. dtInterval) then
   
        bc_time = NINT ( (curr_secs + time_to_bc) / ( grid%interval_seconds ) ) &
             * ( grid%interval_seconds )
        CALL WRFU_TimeIntervalSet(tmpTimeInterval, S=bc_time)
        dtInterval = tmpTimeInterval - &
             (domain_get_current_time(grid) - domain_get_start_time(grid))
   
        use_last2 = .TRUE.
        stepping_to_bc = .true.
     else
        stepping_to_bc = .false.
     endif
  else
     stepping_to_bc = .false.
     tmpTimeInterval = dtInterval
  endif

  !
  ! If the user has requested that we step to output, then
  !   assure that we fall on an output time.  We look out two time steps to
  !   avoid having a very short time step.  Very short time steps can cause model
  !   instability.
  !

  if ((grid%step_to_output_time) .and. (.not. stepping_to_bc) .and. &
       (.not. config_flags%nested)) then

     time_to_output = grid%history_interval*60 - &
          mod(curr_secs, grid%history_interval*60.0)
     num = INT(time_to_output * precision + 0.5)
     den = precision
     call WRFU_TimeIntervalSet(tmpTimeInterval, Sn=num, Sd=den)

     if ( ( tmpTimeInterval .LT. dtInterval * 2 ) .and. &
          ( tmpTimeInterval .GT. dtInterval ) ) then
        dtInterval = tmpTimeInterval / 2
        use_last2 = .TRUE.

     elseif (tmpTimeInterval .LE. dtInterval) then
        !
        ! We will do some tricks here to assure that we fall exactly on an 
        !   output time.  Without the tricks, round-off error causes problems!
        !

        !
        ! Calculate output time.  We round to nearest history time to assure 
        !    we don't have any rounding error.
        !
        output_time = NINT ( (curr_secs + time_to_output) /  &
             ( grid%history_interval * 60 ) ) * (grid%history_interval * 60)
        CALL WRFU_TimeIntervalSet(tmpTimeInterval, S=output_time)
        dtInterval = tmpTimeInterval - &
             (domain_get_current_time(grid) - domain_get_start_time(grid))

        use_last2 = .TRUE.
     endif
  endif

  if (use_last2) then
     grid%last_dtInterval = last_dtInterval
  else
     grid%last_dtInterval = dtInterval
  endif

  grid%dt = real_time(dtInterval)

  grid%last_max_vert_cfl = grid%max_vert_cfl

  !
  ! Update the clock based on the new time step
  !
  CALL WRFU_ClockSet ( grid%domain_clock,        &
       timeStep=dtInterval, &
       rc=rc )

  !
  ! Lateral boundary weight recomputation based on time step.
  !
  CALL lbc_fcx_gcx ( grid%fcx , grid%gcx , grid%spec_bdy_width , &
                     grid%spec_zone , grid%relax_zone , grid%dt , config_flags%spec_exp , &
                     config_flags%specified , config_flags%nested )

END SUBROUTINE adapt_timestep

FUNCTION real_time( timeinterval ) RESULT ( out_time )

  USE module_domain

  IMPLICIT NONE 

! This function returns a floating point time from an input time interval
!  
! Unfortunately, the ESMF did not provide this functionality.
!
! Be careful with the output because, due to rounding, the time is only
!   approximate.
!
! Todd Hutchinson, WSI
! 4/17/2007

! !RETURN VALUE:
      REAL :: out_time
      INTEGER :: dt_num, dt_den, dt_whole

! !ARGUMENTS:
      TYPE(WRFU_TimeInterval), intent(INOUT) :: timeinterval

      CALL WRFU_TimeIntervalGet(timeinterval,Sn=dt_num,Sd=dt_den,S=dt_whole)
      if (ABS(dt_den) < 1) then
         out_time = dt_whole
      else
         out_time = dt_whole + dt_num / REAL(dt_den)
      endif
END FUNCTION 


FUNCTION real_time_r8( timeinterval ) RESULT ( out_time )

  USE module_domain

  IMPLICIT NONE 

! This function returns a double precision floating point time from an input time interval
!  
! Unfortunately, the ESMF did not provide this functionality.
!
! Be careful with the output because, due to rounding, the time is only
!   approximate.
!
! Todd Hutchinson, WSI 4/17/2007
! Converted to r8, William.Gustafson@pnl.gov; 8-May-2008

! !RETURN VALUE:
      REAL(KIND=8) :: out_time
      INTEGER(KIND=8) :: dt_whole
      INTEGER :: dt_num, dt_den

! !ARGUMENTS:
      TYPE(WRFU_TimeInterval), intent(INOUT) :: timeinterval

      CALL WRFU_TimeIntervalGet(timeinterval,Sn=dt_num,Sd=dt_den,S_i8=dt_whole)
      if (ABS(dt_den) < 1) then
         out_time = dt_whole
      else
         out_time = dt_whole + REAL(dt_num/dt_den,8)
      endif
END FUNCTION real_time_r8
