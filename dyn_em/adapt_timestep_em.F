RECURSIVE SUBROUTINE adapt_timestep(grid, config_flags)

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
  USE module_dm, ONLY : wrf_dm_maxval, wrf_dm_minval, wrf_dm_mintile_double, wrf_dm_tile_val_int, wrf_dm_maxtile_real
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
  TYPE(WRFU_TimeInterval)                    :: dtInterval_horiz
  TYPE(WRFU_TimeInterval)                    :: dtInterval_vert  
  TYPE(WRFU_TimeInterval)                    :: parent_dtInterval
  INTEGER                                    :: num_small_steps
  integer                                    :: tile
  LOGICAL                                    :: stepping_to_bc
  INTEGER                                    :: bc_time, output_time
  double precision                           :: dt = 0
  INTEGER, PARAMETER                         :: precision = 100
  INTEGER                                    :: dt_num, dt_den, dt_whole
  INTEGER                                    :: num, den, history_interval_sec
  TYPE(WRFU_TimeInterval)                    :: last_dtInterval
  REAL                                       :: real_time
  REAL                                       :: max_vert_cfl, max_horiz_cfl

  !
  ! If use_last2 is true, this routine will use the time step
  !   from 2 steps ago to compute the next time step.  This
  !   is used along with step_to_output and step_to_bc

  use_last2 = .FALSE.

  !
  ! Assign last_dtInterval type values from restart file
  !

  CALL WRFU_TimeIntervalSet(grid%last_dtInterval,  S=grid%last_dt_sec, &
                         Sn=grid%last_dt_sec_num, Sd=grid%last_dt_sec_den)

  !
  ! If this step has already been adapted, no need to do it again.
  ! time step can already be adapted when adaptation_domain is
  ! enabled.
  !

  if (grid%last_step_updated == grid%itimestep) then
     return
  else
     grid%last_step_updated = grid%itimestep
  endif

  !
  ! For nests, set adapt_step_using_child to parent's value
  !
  if (grid%id .ne. 1) then
     grid%adapt_step_using_child = grid%parents(1)%ptr%adapt_step_using_child;
  endif

  !
  ! For nests, if we're not adapting using child nest, we only want to change 
  !    nests' time steps when the time is conincident with the parent's time.  
  !    So, if dtbc is not zero, simply return and leave the last time step in 
  !    place.
  !

!  if ((grid%id .ne. 1) .and. (.not. grid%adapt_step_using_child)) then
!     if (abs(grid%dtbc) > 0.0001) then
!        return
!     endif
!  endif

  last_dtInterval = grid%last_dtInterval

  !
  ! Get time since beginning of simulation start
  !

  tmpTimeInterval = domain_get_current_time ( grid ) - &
                    domain_get_sim_start_time ( grid )

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
  max_increase_factor = 1. + grid%max_step_increase_pct / 100.

  !
  ! If this is the first time step of the model run (indicated by time step #1),
  !    then set the time step to the input starting_time_step.
  !
  ! Else, calculate the time step based on cfl.
  !
  !BPR BEGIN
  !At the initial time advanceCount == 0, but the following line instead looked
  !for advanceCount == 1
  !if ( ( domain_get_advanceCount ( grid ) .EQ. 1 ) .AND. ( .NOT. config_flags%restart ) ) then
  if ( ( domain_get_advanceCount ( grid ) .EQ. 0 ) .AND. ( .NOT. config_flags%restart ) ) then
  !BPR END
     if ( grid%starting_time_step_den .EQ. 0 ) then
        CALL WRFU_TimeIntervalSet(dtInterval, Sn=grid%starting_time_step, Sd=1)
     else
        CALL WRFU_TimeIntervalSet(dtInterval, Sn=grid%starting_time_step, Sd=grid%starting_time_step_den)
     end if
     curr_secs = 0
     CALL WRFU_TimeIntervalSet(last_dtInterval, Sn=0, Sd=1)

  else

     if (grid%stepping_to_time) then
        max_vert_cfl = grid%last_max_vert_cfl
        max_horiz_cfl = grid%last_max_horiz_cfl
     else
        max_vert_cfl = grid%max_vert_cfl
        max_horiz_cfl = grid%max_horiz_cfl
     endif

     CALL calc_dt(dtInterval_vert, max_vert_cfl, max_increase_factor, &
          precision, last_dtInterval, grid%target_cfl)

     CALL calc_dt(dtInterval_horiz, max_horiz_cfl, max_increase_factor, &
          precision, last_dtInterval, grid%target_hcfl)

     if (dtInterval_vert < dtInterval_horiz) then
        dtInterval = dtInterval_vert
     else
        dtInterval = dtInterval_horiz
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

  if ( grid%max_time_step_den .EQ. 0 ) then
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%max_time_step, Sd=1)
  else
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%max_time_step, Sd=grid%max_time_step_den)
  end if
  if (dtInterval .gt. tmpTimeInterval ) then
     dtInterval = tmpTimeInterval
  endif

  ! Limit the minimum dtInterval based on user input.

  if ( grid%min_time_step_den .EQ. 0 ) then
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%min_time_step, Sd=1)
  else
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=grid%min_time_step, Sd=grid%min_time_step_den)
  end if
  if (dtInterval .lt. tmpTimeInterval ) then
     dtInterval = tmpTimeInterval
  endif

  !
  ! Now, if this is a nest, and we are adapting based upon parent, 
  !   we round down the time step to the nearest 
  !   value that divides evenly into the parent time step.
  ! If this is a nest, and we are adapting based upon the child (i.e., the 
  !   nest), we update the parent timestep to the next smallest multiple
  !   timestep.
  !
  if (grid%nested) then

     dt = real_time(dtInterval)
        
     if (.not. grid%adapt_step_using_child) then 

        ! We'll calculate real numbers to get the number of small steps:
     
        num_small_steps = CEILING( grid%parents(1)%ptr%dt / dt )

#ifdef DM_PARALLEL
        call wrf_dm_maxval(num_small_steps, idex, jdex)
#endif
        dtInterval = domain_get_time_step(grid%parents(1)%ptr) / &
             num_small_steps
     else

        num_small_steps = FLOOR( grid%parents(1)%ptr%dt / dt )

#ifdef DM_PARALLEL
        call wrf_dm_minval(num_small_steps, idex, jdex)
#endif
        if (num_small_steps < 1) then
           num_small_steps = 1
        endif

     endif
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

  call wrf_dm_maxtile_real(grid%max_vert_cfl, tile)
  call wrf_dm_maxtile_real(grid%max_horiz_cfl, tile)
#endif

  if ((grid%nested) .and. (grid%adapt_step_using_child)) then 

     grid%dt = real_time(dtInterval)

     ! Set parent step here.
     grid%parents(1)%ptr%dt = grid%dt * num_small_steps
     parent_dtInterval = dtInterval * num_small_steps

     !
     ! Update the parent clock based on the new time step
     !
     CALL WRFU_ClockSet ( grid%parents(1)%ptr%domain_clock,        &
          timeStep=parent_dtInterval, &
          rc=rc )
     
  endif


  !
  ! Assure that we fall on a BC time.  Due to a bug in WRF, the time
  !   step must fall on the boundary times.  Only modify the dtInterval
  !   when this is not the first time step on this domain.
  !

  grid%stepping_to_time = .FALSE.
  time_to_bc = grid%interval_seconds - grid%dtbc
  num = INT(time_to_bc * precision + 0.5)
  den = precision
  CALL WRFU_TimeIntervalSet(tmpTimeInterval, Sn=num, Sd=den)
  
  if ( ( tmpTimeInterval .LT. dtInterval * 2 ) .and. &
       ( tmpTimeInterval .GT. dtInterval ) ) then
     dtInterval = tmpTimeInterval / 2
     
     use_last2 = .TRUE.
     stepping_to_bc = .true.
     grid%stepping_to_time = .TRUE.
     
  elseif (tmpTimeInterval .LE. dtInterval) then
     
     bc_time = NINT ( (curr_secs + time_to_bc) / ( grid%interval_seconds ) ) &
          * ( grid%interval_seconds )
     CALL WRFU_TimeIntervalSet(tmpTimeInterval, S=bc_time)
     dtInterval = tmpTimeInterval - &
          (domain_get_current_time(grid) - domain_get_sim_start_time(grid))
     
     use_last2 = .TRUE.
     stepping_to_bc = .true.
     grid%stepping_to_time = .TRUE.
  else
     stepping_to_bc = .false.
  endif

  !
  ! If the user has requested that we step to output, then
  !   assure that we fall on an output time.  We look out two time steps to
  !   avoid having a very short time step.  Very short time steps can cause model
  !   instability.
  !

  if ((grid%step_to_output_time) .and. (.not. stepping_to_bc) .and. &
       (.not. grid%nested)) then

     IF ( grid%history_interval_m .EQ. 0 ) grid%history_interval_m = grid%history_interval
     history_interval_sec = grid%history_interval_s + grid%history_interval_m*60 + &
                            grid%history_interval_h*3600 + grid%history_interval_d*86400

     time_to_output = history_interval_sec - &
          mod( curr_secs, REAL(history_interval_sec) )
     num = INT(time_to_output * precision + 0.5)
     den = precision
     call WRFU_TimeIntervalSet(tmpTimeInterval, Sn=num, Sd=den)

     if ( ( tmpTimeInterval .LT. dtInterval * 2 ) .and. &
          ( tmpTimeInterval .GT. dtInterval ) ) then
        dtInterval = tmpTimeInterval / 2
        use_last2 = .TRUE.
        grid%stepping_to_time = .TRUE.

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
             (history_interval_sec) ) * (history_interval_sec)
        CALL WRFU_TimeIntervalSet(tmpTimeInterval, S=output_time)
        dtInterval = tmpTimeInterval - &
             (domain_get_current_time(grid) - domain_get_sim_start_time(grid))

        use_last2 = .TRUE.
        grid%stepping_to_time = .TRUE.
     endif
  endif

  !
  ! Now, set adapt_step_using_child only if we are not stepping to an
  !   output time, or, it's not the start of the model run.
  ! Note:  adapt_step_using_child is updated just before recursive call to
  !    adapt_timestep--see end of this function.
  !

  if (grid%id == 1) then
     if ((grid%adaptation_domain > 1) .and. &
          (grid%max_dom == 2) .and. &
          (.not. grid%stepping_to_time) .and. &
          (domain_get_current_time(grid) .ne. &
          domain_get_start_time(grid)) &
          ) then
        
        grid%adapt_step_using_child = .TRUE.
     else
        grid%adapt_step_using_child = .FALSE.
     endif
  endif


  if (use_last2) then
     grid%last_dtInterval = last_dtInterval
     grid%last_max_vert_cfl = grid%last_max_vert_cfl
     grid%last_max_horiz_cfl = grid%last_max_horiz_cfl
  else
     grid%last_dtInterval = dtInterval
     grid%last_max_vert_cfl = grid%max_vert_cfl
     grid%last_max_horiz_cfl = grid%max_horiz_cfl
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
  ! If we're are adapting based on the child time step, 
  !    we call the child from here.  This assures that 
  !    child and parent are updated in sync.
  ! Note: This is not necessary when we are adapting based
  !    upon parent.
  !
  if ((grid%id == 1) .and. (grid%adapt_step_using_child)) then
     !
     ! Finally, check if we can adapt using child.  If we are
     !   stepping to an output time, we cannot adapt based upon
     !   child.  So, we reset the variable before calling the child.  
     ! This covers the case that, within this parent time-step that
     !   we just calculated, we are stepping to an output time.
     !
     if (grid%stepping_to_time) then
        grid%adapt_step_using_child = .FALSE.
     endif
     call adapt_timestep(grid%nests(1)%ptr, config_flags)
  endif

  !
  ! Lateral boundary weight recomputation based on time step.
  !
  if (grid%id == 1) then
     CALL lbc_fcx_gcx ( grid%fcx , grid%gcx , grid%spec_bdy_width , &
          grid%spec_zone , grid%relax_zone , grid%dt , config_flags%spec_exp , &
          config_flags%specified , config_flags%nested )
  endif

! Update last timestep info for restart file

  CALL WRFU_TimeIntervalGet(grid%last_dtInterval,  S=grid%last_dt_sec, &
                         Sn=grid%last_dt_sec_num, Sd=grid%last_dt_sec_den)

END SUBROUTINE adapt_timestep

SUBROUTINE calc_dt(dtInterval, max_cfl, max_increase_factor, precision, &
     last_dtInterval, target_cfl)

  USE module_domain

  TYPE(WRFU_TimeInterval) ,INTENT(OUT)      :: dtInterval
  REAL                    ,INTENT(IN)       :: max_cfl
  REAL                    ,INTENT(IN)       :: max_increase_factor
  INTEGER                 ,INTENT(IN)       :: precision
  REAL                    ,INTENT(IN)       :: target_cfl
  TYPE(WRFU_TimeInterval) ,INTENT(IN)       :: last_dtInterval
  REAL                                      :: factor
  INTEGER                                   :: num, den
  

  if (max_cfl < 0.001) then 
     !
     ! If the max_cfl is small, then we increase dtInterval the maximum
     !    amount allowable.
     !
     num = INT(max_increase_factor * precision + 0.5)
     den = precision
     dtInterval = last_dtInterval * num / den

  else
     !
     ! If the max_cfl is greater than the user input target cfl, we 
     !    reduce the time step, 
     ! else, we increase it.
     !
     if (max_cfl .gt. target_cfl) then
        !
        ! If we are reducing the time step, we go below target cfl by half
        !   the difference between max and target.
        !   This tends to keep the model more stable.
        !
        
        factor = ( target_cfl - 0.5 * (max_cfl - target_cfl) ) / max_cfl

        ! BPR BEGIN
        ! Factor can be negative in some cases so prevent factor from being
        ! lower than 0.1
        ! Otherwise model crashes can occur in normalize_basetime noting that
        ! denominator of seconds cannot be negative
        factor = MAX(0.1,factor)
        ! BPR END

        num = INT(factor * precision + 0.5)
        den = precision

        dtInterval = last_dtInterval * num / den

     else
        !
        ! Linearly increase dtInterval (we'll limit below)
        !
        
        factor = target_cfl / max_cfl
        num = INT(factor * precision + 0.5)
        den = precision
        dtInterval = last_dtInterval * num / den
     endif
  endif

END SUBROUTINE calc_dt


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
      INTEGER(selected_int_kind(14)) :: dt_whole
      INTEGER :: dt_num, dt_den

! !ARGUMENTS:
      TYPE(WRFU_TimeInterval), intent(INOUT) :: timeinterval

      CALL WRFU_TimeIntervalGet(timeinterval,Sn=dt_num,Sd=dt_den,S_i8=dt_whole)
      if (ABS(dt_den) < 1) then
         out_time = REAL(dt_whole)
      else
         out_time = REAL(dt_whole) + REAL(dt_num,8)/REAL(dt_den,8)
      endif
END FUNCTION real_time_r8
