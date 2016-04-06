SUBROUTINE da_buddy_qc ( numobs, m_max, station_id, xob, yob, obs, qc_flag_small,    &
                         name, pressure, dx, buddy_weight , buddy_difference, &
                         iunit, print_buddy, xob_g, yob_g, obs_g, qc_flag_small_g, num_recv)

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Imported from BUDDY in RAWINS

!   Notes:
!      This routine performs error checks by comparing the difference
!         between the first guess field
!         and observations at the observing locations (xob, yob)
!         with the averaged difference at the nearby stations within
!         a given distance. If the difference value at the observation
!         point differs from the average of the nearby difference values
!         by more than a certain maximum (dependent on variable types,
!         pressure levels, and BUDWGT parameter in namelist), the
!         observation is flagged as suspect, and add the flag message
!         to the observation records.

!   *** Note that x and y are in natural coordinates now.
!         Need to check if the x and y are used correctly...

!   other variables:
!    err:       difference between observations and first guess fields
!
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!  This subroutine da_buddy_qc is adapted by Y.-R.Guo, NCAR/MMM, based on
!  the WRFV3/OBSGRID/src/qc2_module.F90. 
!
!  The algorithm for buddy check used here could be named as "Two Passes 
!  Innovation Buddy Check": the buddy members to a specific obs are
!  determined by going through two times of checks for innovation deprture 
!  from the buddy member's mean values. 
!
!  In OBSGRID, the buddy check is completed at each of  the pressure levels 
!  for all obs, i.e. all obs types are mixed to gether. In order to use this
!  buddy check in wrfvar, first we should sort the obs into the different 
!  pressure bins for 3-D observations,such as SOUND, then apply the buddy 
!  check for each of the obs types in da_get_innov_vector_?????.inc. For the 
!  2-D observations, such as SYNOP, no binning is needed.
!  
!  Most of the tolerances for each of variables at the different pressure 
!  levels are adapted from WRFV3/OBSGRID. The tolerances for specific humidity
!  used here are converted from rh and specified temperature. The tolerance
!  for PMSL is reduced to 350 Pa defined in da_control/da_control.f90.
!
!                                  Yong-Run Guo, 10/10/2008
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

  IMPLICIT NONE

  INTEGER,                       intent(in)    :: numobs, m_max, iunit
  REAL,    DIMENSION ( m_max ), intent(in)    :: obs , xob , yob
  INTEGER, DIMENSION ( m_max ), intent(inout) :: qc_flag_small
  REAL,    DIMENSION ( m_max, 0:num_procs-1 ), intent(in), optional :: obs_g , xob_g , yob_g
  INTEGER, DIMENSION ( m_max, 0:num_procs-1 ), intent(inout), optional :: qc_flag_small_g
  INTEGER, DIMENSION ( 0:num_procs-1 ),        intent(in), optional :: num_recv
  REAL,                    intent(in)          :: buddy_weight,   &
                                                  buddy_difference , &
                                                  dx          , &
                                                  pressure
  CHARACTER(LEN = 5), DIMENSION(m_max), intent(in) :: station_id
  CHARACTER(LEN = 8),                    intent(in) :: name
  LOGICAL           ,                    intent(in) :: print_buddy

  !  err:         difference between observations and first guess fields

  INTEGER                                    :: num, num3, num4, numj, &
                                                kob, n, ierr
  REAL                                       :: range,              &
                                                distance,           &
                                                sum,                &
                                                average,            &
                                                x, y,               &
                                                diff_numj,          &
                                                diff_check_1,       &
                                                diff_check_2
  REAL                                          plevel,             &
                             pp, tt, rh, t_error, rh_error, q_error
  REAL  , DIMENSION ( numobs )               :: err,                &
                                                difmax,             &
                                                diff_at_obs
#ifdef DM_PARALLEL
  REAL  , DIMENSION ( m_max*num_procs )      :: diff
  INTEGER, DIMENSION (3)                     :: i_dummy
#else
  REAL  , DIMENSION ( numobs )               :: diff
#endif
  INTEGER , DIMENSION ( numobs )             :: buddy_num,          &
                                                buddy_num_final
  REAL                                       :: scale
  INTEGER, DIMENSION ( m_max )               :: qc_flag


  real :: f_qv_from_rh
  external f_qv_from_rh

! [1.0] Store original qc flags:
  qc_flag = qc_flag_small

  plevel = pressure

! [2.0] Define distance within which buddy check is to find neighboring stations:

 ! 2.1 Define the horizontal range for buddy check:
  IF ( plevel .LE. 201.0 ) THEN
     range = 650.
  ELSE IF ( plevel .LE. 1000.1 ) THEN
     range = 300.
  ELSE
     range = 500.
  END IF

 ! 2.2 Define tolerance value for comparison:

  error_allowance_by_field : SELECT CASE ( name )

     CASE ( 'UU      ' , 'VV      ' )
        IF      ( plevel .LT. 401. ) THEN
           difmax = buddy_difference * 1.7
        ELSE IF ( plevel .LT. 701. ) THEN
           difmax = buddy_difference * 1.4
        ELSE IF ( plevel .LT. 851. ) THEN
           difmax = buddy_difference * 1.1
        ELSE
           difmax = buddy_difference
        END IF

     CASE ( 'PMSL    ' )
        difmax = buddy_difference

     CASE ( 'TT      ' )
        IF      ( plevel .LT. 401. ) THEN
           difmax = buddy_difference * 0.6
        ELSE IF ( plevel .LT. 701. ) THEN
           difmax = buddy_difference * 0.8
        ELSE
           difmax = buddy_difference
        END IF

     CASE ( 'QQ      ' )
!         difmax = buddy_difference

    ! Convert the rh error to q error:

        t_error = 2.0
        rh_error = buddy_difference

        pp = pressure * 100.0
        if ( pp > 85000.0 ) then
          rh = 90.0
          tt = 300.0
        else if ( pp > 70000.0 ) then
          rh = 80.0
          tt = 290.0
        else if ( pp > 35000.0 ) then
          rh = 70.0
          tt = 280.0
        else if ( pp > 10000.0 ) then
          rh = 60.0
          tt = 260.0
        else
          rh = 50.0
          tt = 240.0
        endif

        q_error = f_qv_from_rh( rh_error, t_error, rh, tt, pp)
        difmax = q_error

!         print '("p, t, rh, t_error, rh_error, difmax:",5f12.2,e15.5)', &
!            pp, tt, rh, t_error, rh_error, q_error

  END SELECT error_allowance_by_field

  difmax = difmax * buddy_weight

! [3.0] Compute the errors for buddy check:  

 ! 3.1 Loop through station locations (numobs) to compute buddy average
 !      at each observation locations.

!   station_loop_2 : DO num = numobs, 1, -1
  station_loop_2 : DO num = 1, numobs

     average = 0.0
 ! No buddy check is applied to a bad data (qc < 00:
     if ( qc_flag_small(num) < 0 ) cycle  station_loop_2

     !  Compute distance between all pairs of observations, 
     !  find number of observations within a given distance: buddy_num,
     !  and compute the difference between first guess and obs.

     buddy_num(num) = 0
#ifdef DM_PARALLEL
     if (present(xob_g) .and. present(yob_g) .and. present(obs_g) .and. present(qc_flag_small_g)) then
     else
         call da_error(__FILE__,__LINE__, &
                      (/"Buddy check in parallel mode needs global observations"/))
     endif
     pe_loop : DO n = 0, num_procs-1
     station_loop_3 : DO num3 = 1, num_recv(n)

        x = xob(num) - xob_g(num3,n)
        y = yob(num) - yob_g(num3,n)
        IF ( ABS(x) .GT. 1.E-20 .OR. ABS(y) .GT. 1.E-20 ) THEN
           distance = SQRT ( x*x + y*y ) * dx
           IF ( distance .LE. range .AND. distance .GE. 0.1 &
                .and. qc_flag_small_g(num3,n) >= 0 ) THEN
! Only good data are included in buddy check group:
              buddy_num(num) = buddy_num(num) + 1
! innovation (O-B) ==> diff:
              diff(buddy_num(num)) = obs_g(num3,n)
           END IF
        END IF

     END DO station_loop_3
     END DO pe_loop
#else
!      station_loop_3 : DO num3 = numobs, 1, -1
     station_loop_3 : DO num3 = 1, numobs, 1

        IF ( num3 .NE. num ) THEN
           x = xob(num) - xob(num3)
           y = yob(num) - yob(num3)
           distance = SQRT ( x*x + y*y ) * dx
           IF ( distance .LE. range .AND. distance .GE. 0.1 &
                .and. qc_flag_small(num3) >= 0 ) THEN
! Only good data are included in buddy check group:
              buddy_num(num) = buddy_num(num) + 1
! innovation (O-B) ==> diff:
              diff(buddy_num(num)) = obs(num3)
           END IF
        END IF

     END DO station_loop_3
#endif

! innovation at the specific obs(num):
     diff_at_obs(num) = obs(num)
!
! Summation of innovations over the surrounding obs:
     sum = 0.
     DO numj = 1, buddy_num(num)
        sum = sum + diff(numj)
     END DO

     !  Check to see if there are any buddies, compute the mean innovation.

     IF ( buddy_num(num) .GT. 0 ) average = sum / buddy_num(num)

 ! 3.2 Check if there is any bad obs among the obs located within the 
 !  the radius surrounding the test ob.

     diff_check_1 = difmax (1) * 1.25
     diff_check_2 = difmax (1)

     check_bad_ob : IF ( buddy_num(num) .GE. 2 ) THEN

        kob = buddy_num(num)
        remove_bad_ob : DO numj = 1, buddy_num(num)
           diff_numj = ABS ( diff ( numj ) - average )

           IF ( diff ( numj ) .GT. diff_check_1  &
               .AND. diff_numj .GT. diff_check_2 ) THEN
    ! Bad obs:                 Innovation itself: diff(numj) > diff_check_1
    !        The distance between the innovation and average > diff_check_2
              kob = kob - 1
              sum = sum - diff ( numj )
           END IF
        END DO remove_bad_ob

! The final number of buddies:
        buddy_num_final(num) = kob

        !  We may have removed too many observations.

        IF ( kob .GE. 2 ) THEN
        !  Information for buddy check for specific obs(num)
           average = sum / kob
           err(num) = diff_at_obs(num) - average
        ELSE
        !  No buddy check completed for this specific obs(num):
           err(num)     = 0.
! Not change the flag (YRG, 02/10/2009)
!            qc_flag_small(num) = qc_flag_small(num) + no_buddies
#ifdef DM_PARALLEL
! Not change the flag (YRG, 02/10/2009)
!            qc_flag_small_g(num,myproc) = qc_flag_small_g(num,myproc) + no_buddies
           i_dummy(1) = qc_flag_small_g(num,myproc)
           i_dummy(2) = num
           i_dummy(3) = myproc
           call mpi_bcast ( i_dummy, 3, mpi_integer, myproc, comm, ierr )
           qc_flag_small_g(i_dummy(2),i_dummy(3)) = i_dummy(1)
#endif
        END IF

     ELSE check_bad_ob
        ! Too less buddy numbers < 2, no buddy check completed:
        err(num)     = 0.
! Not change the flag (YRG, 02/10/20090
!         qc_flag_small(num) = qc_flag_small(num) + no_buddies
#ifdef DM_PARALLEL
! Not change the flag (YRG, 02/10/20090
!         qc_flag_small_g(num,myproc) = qc_flag_small_g(num,myproc) + no_buddies
         i_dummy(1) = qc_flag_small_g(num,myproc)
         i_dummy(2) = num
         i_dummy(3) = myproc
         call mpi_bcast ( i_dummy, 3, mpi_integer, myproc, comm, ierr )
         qc_flag_small_g(i_dummy(2),i_dummy(3)) = i_dummy(1)
#endif

     END IF check_bad_ob

 ! 3.3 If the buddy number is ONLY 2, increase the tolerance value:
     IF ( buddy_num_final(num) .EQ. 2 ) difmax(num) = 1.2 * difmax(num)

  END DO station_loop_2

! [4.0] Reset the qc flags according to the buddy check errors:
!
!       If an observation has been flagged as failing the buddy
!       check criteria (error [err] larger than the allowable 
!       difference [difmax]), then qc_flag for this variable, 
!       level, time has to be modified.  

   station_loop_4 : do n = 1, numobs
   ! if the difference at station: n from the buddy-average > difmax,
   ! failed the buddy check:
     if ( abs(err(n)) > difmax(n) ) then
        qc_flag_small(n) = fails_buddy_check
     endif
   enddo station_loop_4

! [5.0] This notifies the user which observations have been flagged as
!       suspect by the buddy check routine.

  IF ( print_buddy ) THEN
     n    = 0
     numj = 0
     num3 = 0
     num4 = 0
     station_loop_5 : DO num = 1 , numobs

        IF ( name(1:8) .EQ. 'PMSL    ' ) THEN
           scale = 100.
        ELSE IF ( name(1:8) .EQ. 'QQ      ' ) THEN
           scale = 0.001
        ELSE
           scale = 1.0
        END IF

        IF ( buddy_num_final(num) .GE. 2 ) THEN
          n = n + 1
          IF ( ABS ( err(num) ) .GT. difmax(num) ) THEN
             numj = numj + 1
             WRITE ( unit=iunit, FMT = '(3I8,&
                   &  " ID=",a5,&    
                   &  ",NAME="  ,a8, &
                   &  ",BUDDY TOLERANCE=",f5.1,&
                   &  ",LOC=(" ,f6.1,",",f6.1,")",&
                   &  ",OBS INV="       ,f7.2,&
                   &  ",DIFF ERR="      ,f7.2,&
                   &  ",NOBS BUDDY="    ,I4,&
                   &  ",QC_FLAG=",I3,&
                   &  "  QC_FLAG_NEW=",I3,"  FAILED")' ) num, n, numj,&
                  station_id(num),name,difmax(num)/scale, &
                  xob(num),yob(num),obs(num)/scale,&
                  err(num)/scale, buddy_num_final(num), &
                  qc_flag(num), qc_flag_small(num)
          ELSE
             num3 = num3 + 1
!              WRITE ( unit=iunit, FMT = '(3I8,&
!                    &  " ID=",a5,&    
!                    &  ",NAME="  ,a8, &
!                    &  ",BUDDY TOLERANCE=",f5.1,&
!                    &  ",LOC=(" ,f6.1,",",f6.1,")",&
!                    &  ",OBS INV="       ,f7.2,&
!                    &  ",DIFF ERR="      ,f7.2,&
!                    &  ",NOBS BUDDY="    ,I4,&
!                    &  ",QC_FLAG=",I3,&
!                    &  "  QC_FLAG_NEW=",I3,"  PASSED")' ) num, n, num3,&
!                   station_id(num),name,difmax(num)/scale, &
!                   xob(num),yob(num),obs(num)/scale,&
!                   err(num)/scale, buddy_num_final(num), &
!                   qc_flag(num), qc_flag_small(num)
          ENDIF
        ELSE
          num4 = num4 + 1
!              WRITE ( unit=iunit, FMT = '(3I8,&
!                    &  " ID=",a5,&    
!                    &  ",NAME="  ,a8, &
!                    &  ",BUDDY TOLERANCE=",f5.1,&
!                    &  ",LOC=(" ,f6.1,",",f6.1,")",&
!                    &  ",OBS INV="       ,f7.2,&
!                    &  ",DIFF ERR="      ,f7.2,&
!                    &  ",NOBS BUDDY="    ,I4,&
!                    &  ",QC_FLAG=",I3,&
!                    &  "  QC_FLAG_NEW=",I3,"NO BUDDY")' ) num, n, num4,&
!                   station_id(num),name,difmax(num)/scale, &
!                   xob(num),yob(num),obs(num)/scale,&
!                   err(num)/scale, buddy_num_final(num), &
!                   qc_flag(num), qc_flag_small(num)
        END IF
     END DO station_loop_5

     write(unit=iunit,fmt = '(5x,"NOB=",i6,&
          & "  Toltal N_buddy-checked =",i6,&
          & "  N_Passed =",i6,"  N_Failed=",i6,"  NO_Buddy=",i6, &
          & "  RANGE=",f10.2,"  DIFMAX=",f10.2)') &
          numobs, n, num3, numj, num4, range, difmax(1)/scale
  END IF

END SUBROUTINE da_buddy_qc

