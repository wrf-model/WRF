!WRF:DRIVER_LAYER:MAIN
!

!<DESCRIPTION>
! ESMF-specific modules for building WRF as an ESMF component.  
!
! This source file is only built when ESMF coupling is used.  
!
!</DESCRIPTION>



MODULE module_metadatautils
!<DESCRIPTION>
! This module defines component-independent "model metadata" utilities 
! used for ESMF coupling.  
!</DESCRIPTION>
!$$$ Upgrade this later to support multiple coupling intervals via Alarms 
!$$$ associated with top-level clock.  Do this by adding TimesAttachedToState() 
!$$$ inquiry function that will test an ESMF_State to see if the times are 
!$$$ present via names defined in this module.  Then call this for every 
!$$$ component and resolve conflicts (somehow) for cases where two components 
!$$$ define conflicting clocks.  Of course, a component is allowed to not define 
!$$$ a clock at all too...  
!
!$$$ Replace meta-data names with "model metadata" conventions (when they exist)
!
!$$$ Refactor to remove duplication of hard-coded names!  
!
   USE ESMF_Mod

   IMPLICIT NONE

   ! everything is private by default
   PRIVATE

   ! Public interfaces
   PUBLIC AttachTimesToState
   PUBLIC GetTimesFromStates
   PUBLIC AttachDecompToState
   PUBLIC GetDecompFromState

   ! private stuff
   CHARACTER (ESMF_MAXSTR) :: str


CONTAINS


   ! Attach time information to state as meta-data.  
   ! Update later to use some form of meta-data standards/conventions for 
   ! model "time" meta-data.  
   SUBROUTINE AttachTimesToState( state, startTime, stopTime, couplingInterval )
     TYPE(ESMF_State),        INTENT(INOUT) :: state
     TYPE(ESMF_Time),         INTENT(IN   ) :: startTime
     TYPE(ESMF_Time),         INTENT(IN   ) :: stopTime
     TYPE(ESMF_TimeInterval), INTENT(IN   ) :: couplingInterval
     ! locals
     INTEGER :: rc
     INTEGER :: year, month, day, hour, minute, second
     INTEGER(ESMF_KIND_I4) :: timevals(6)   ! big enough to hold the vars listed above
     ! start time
     CALL ESMF_TimeGet(startTime, yy=year, mm=month, dd=day, &
                       h=hour, m=minute, s=second, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_TimeGet(startTime) failed' )
     ENDIF
     timevals(1) = year
     timevals(2) = month
     timevals(3) = day
     timevals(4) = hour
     timevals(5) = minute
     timevals(6) = second
     CALL ESMF_StateSetAttribute(state, 'ComponentStartTime', &
                                 SIZE(timevals), timevals, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateSetAttribute(ComponentStartTime) failed' )
     ENDIF
     ! stop time
     CALL ESMF_TimeGet(stopTime, yy=year, mm=month, dd=day, &
                       h=hour, m=minute, s=second, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_TimeGet(stopTime) failed' )
     ENDIF
     timevals(1) = year
     timevals(2) = month
     timevals(3) = day
     timevals(4) = hour
     timevals(5) = minute
     timevals(6) = second
     CALL ESMF_StateSetAttribute(state, 'ComponentStopTime', &
                                 SIZE(timevals), timevals, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateSetAttribute(ComponentStopTime) failed' )
     ENDIF
     ! coupling time step
     CALL ESMF_TimeIntervalGet(couplingInterval, yy=year, mm=month, d=day, &
                               h=hour, m=minute, s=second, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_TimeIntervalGet(couplingInterval) failed' )
     ENDIF
     timevals(1) = year
     timevals(2) = month
     timevals(3) = day
     timevals(4) = hour
     timevals(5) = minute
     timevals(6) = second
     CALL ESMF_StateSetAttribute(state, 'ComponentCouplingInterval', &
                                 SIZE(timevals), timevals, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateSetAttribute(ComponentCouplingInterval) failed' )
     ENDIF
   END SUBROUTINE AttachTimesToState



   ! Extract time information attached as meta-data from a single 
   ! ESMF_State.  
   ! Update later to use some form of meta-data standards/conventions for 
   ! model "time" meta-data.  
   SUBROUTINE GetTimesFromState( state, startTime, stopTime, couplingInterval )
     TYPE(ESMF_State),        INTENT(IN   ) :: state
     TYPE(ESMF_Time),         INTENT(INOUT) :: startTime
     TYPE(ESMF_Time),         INTENT(INOUT) :: stopTime
     TYPE(ESMF_TimeInterval), INTENT(INOUT) :: couplingInterval
     ! locals
     INTEGER :: rc
     INTEGER :: year, month, day, hour, minute, second
     INTEGER(ESMF_KIND_I4) :: timevals(6)   ! big enough to hold the vars listed above
     ! start time
     CALL ESMF_StateGetAttribute(state, 'ComponentStartTime', &
                                 SIZE(timevals), timevals, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateGetAttribute(ComponentStartTime) failed' )
     ENDIF
     year   = timevals(1)
     month  = timevals(2)
     day    = timevals(3)
     hour   = timevals(4)
     minute = timevals(5)
     second = timevals(6)
     CALL ESMF_TimeSet(startTime, yy=year, mm=month, dd=day, &
                       h=hour, m=minute, s=second, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_TimeSet(startTime) failed' )
     ENDIF
     ! stop time
     CALL ESMF_StateGetAttribute(state, 'ComponentStopTime', &
                                 SIZE(timevals), timevals, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateGetAttribute(ComponentStopTime) failed' )
     ENDIF
     year   = timevals(1)
     month  = timevals(2)
     day    = timevals(3)
     hour   = timevals(4)
     minute = timevals(5)
     second = timevals(6)
     CALL ESMF_TimeSet(stopTime, yy=year, mm=month, dd=day, &
                       h=hour, m=minute, s=second, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_TimeSet(stopTime) failed' )
     ENDIF
     ! coupling time step
     CALL ESMF_StateGetAttribute(state, 'ComponentCouplingInterval', &
                                 SIZE(timevals), timevals, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_StateGetAttribute(ComponentCouplingInterval) failed' )
     ENDIF
     year   = timevals(1)
     month  = timevals(2)
     day    = timevals(3)
     hour   = timevals(4)
     minute = timevals(5)
     second = timevals(6)
     CALL ESMF_TimeIntervalSet(couplingInterval, yy=year, mm=month, d=day, &
                               h=hour, m=minute, s=second, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'ESMF_TimeIntervalSet(couplingInterval) failed' )
     ENDIF
   END SUBROUTINE GetTimesFromState



   ! Extract time information attached as meta-data from one or more 
   ! ESMF_States.  To use this with more than one ESMF_State, put the 
   ! ESMF_States into a single ESMF_State.  If times differ, an attempt 
   ! is made to reconcile them.  
   SUBROUTINE GetTimesFromStates( state, startTime, stopTime, couplingInterval )
     TYPE(ESMF_State),        INTENT(IN   ) :: state
     TYPE(ESMF_Time),         INTENT(INOUT) :: startTime
     TYPE(ESMF_Time),         INTENT(INOUT) :: stopTime
     TYPE(ESMF_TimeInterval), INTENT(INOUT) :: couplingInterval
     ! locals
     INTEGER :: rc
     INTEGER :: numItems, numStates, i, istate
     TYPE(ESMF_StateItemType), ALLOCATABLE :: itemTypes(:)
     TYPE(ESMF_State) :: tmpState
     CHARACTER (len=ESMF_MAXSTR), ALLOCATABLE :: itemNames(:)
     TYPE(ESMF_Time),         ALLOCATABLE :: startTimes(:)
     TYPE(ESMF_Time),         ALLOCATABLE :: stopTimes(:)
     TYPE(ESMF_TimeInterval), ALLOCATABLE :: couplingIntervals(:)

!$$$unfortunately, implementing this is an extraordinary pain in the @ss due 
!$$$to lack of sane iterators for ESMF_State!!!  @#$%!!  

     ! Since there are no convenient iterators for ESMF_State (@#$%), 
     ! write a lot of code...  
     ! Figure out how many items are in the ESMF_State
     CALL ESMF_StateGet(state, itemCount=numItems, rc=rc)
     IF ( rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( 'ESMF_StateGet(numItems) failed' )
     ENDIF
     ! allocate an array to hold the types of all items
     ALLOCATE( itemTypes(numItems) )
     ! allocate an array to hold the names of all items
     ALLOCATE( itemNames(numItems) )
     ! get the item types and names
     CALL ESMF_StateGet(state, stateitemtypeList=itemTypes, &
                        itemNameList=itemNames, rc=rc)
     IF ( rc /= ESMF_SUCCESS) THEN
       WRITE(str,*) 'ESMF_StateGet itemTypes failed with rc = ', rc
       CALL wrf_error_fatal ( str )
     ENDIF
     ! count how many items are ESMF_States
     numStates = 0
     DO i=1,numItems
       IF ( itemTypes(i) == ESMF_STATEITEM_STATE ) THEN
         numStates = numStates + 1
       ENDIF
     ENDDO
     ALLOCATE( startTimes(numStates), stopTimes(numStates), &
               couplingIntervals(numStates) )
     IF ( numStates > 0) THEN
       ! finally, extract nested ESMF_States by name, if there are any
       ! (should be able to do this by index at least -- @#%$)
       istate = 0
       DO i=1,numItems
         IF ( itemTypes(i) == ESMF_STATEITEM_STATE ) THEN
           CALL ESMF_StateGetState( state, nestedStateName=TRIM(itemNames(i)), &
                                    nestedState=tmpState, rc=rc )
           IF ( rc /= ESMF_SUCCESS) THEN
             WRITE(str,*) 'ESMF_StateGetState(',TRIM(itemNames(i)),') failed'
             CALL wrf_error_fatal ( str )
           ENDIF
           istate = istate + 1
           CALL GetTimesFromState( tmpState, startTimes(istate),         &
                                             stopTimes(istate),          &
                                             couplingIntervals(istate) )
         ENDIF
       ENDDO
       CALL ReconcileTimes( startTimes, stopTimes, couplingIntervals, &
                            startTime,  stopTime,  couplingInterval )
     ELSE
       ! there are no nested ESMF_States so use parent state only
       CALL GetTimesFromState( state, startTime, stopTime, &
                               couplingInterval )
     ENDIF

     ! deallocate locals
     DEALLOCATE( itemTypes )
     DEALLOCATE( itemNames )
     DEALLOCATE( startTimes, stopTimes, couplingIntervals )

   END SUBROUTINE GetTimesFromStates


   ! Reconcile all times and intervals in startTimes, stopTimes, and 
   ! couplingIntervals and return the results in startTime, stopTime, and 
   ! couplingInterval.  Abort if reconciliation is not possible.  
   SUBROUTINE ReconcileTimes( startTimes, stopTimes, couplingIntervals, &
                              startTime,  stopTime,  couplingInterval )
     TYPE(ESMF_Time),         INTENT(IN   ) :: startTimes(:)
     TYPE(ESMF_Time),         INTENT(IN   ) :: stopTimes(:)
     TYPE(ESMF_TimeInterval), INTENT(IN   ) :: couplingIntervals(:)
     TYPE(ESMF_Time),         INTENT(INOUT) :: startTime
     TYPE(ESMF_Time),         INTENT(INOUT) :: stopTime
     TYPE(ESMF_TimeInterval), INTENT(INOUT) :: couplingInterval
     ! locals
     INTEGER :: numTimes, numTimesTmp, i

     ! how many sets of time info?
     numTimes = SIZE(startTimes)
     IF ( numTimes < 2 ) THEN
       CALL wrf_error_fatal ( 'SIZE(startTimes) too small' )
     ENDIF
     numTimesTmp = SIZE(stopTimes)
     IF ( numTimes /= numTimesTmp ) THEN
       CALL wrf_error_fatal ( 'incorrect SIZE(stopTimes)' )
     ENDIF
     numTimesTmp = SIZE(couplingIntervals)
     IF ( numTimes /= numTimesTmp ) THEN
       CALL wrf_error_fatal ( 'incorrect SIZE(couplingIntervals)' )
     ENDIF

     ! reconcile
!For now this is very simple.  Fancy it up later.  
     DO i = 1, numTimes
       IF ( i == 1 ) THEN
         startTime = startTimes(i)
         stopTime = stopTimes(i)
         couplingInterval = couplingIntervals(i)
       ELSE
         IF ( startTimes(i) /= startTime ) THEN
           CALL wrf_error_fatal ( 'ReconcileTimes:  inconsistent startTimes' )
         ENDIF
         IF ( stopTimes(i) /= stopTime ) THEN
           CALL wrf_error_fatal ( 'ReconcileTimes:  inconsistent stopTimes' )
         ENDIF
         IF ( couplingIntervals(i) /= couplingInterval ) THEN
           CALL wrf_error_fatal ( 'ReconcileTimes:  inconsistent couplingIntervals' )
         ENDIF
       ENDIF

     ENDDO

   END SUBROUTINE ReconcileTimes



   !$$$ TBH:  Eliminate this once this information can be derived via other 
   !$$$ TBH:  means.  
   SUBROUTINE AttachDecompToState( state,                        & 
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   ips, ipe, jps, jpe, kps, kpe, &
                                   domdesc, bdy_mask )
     TYPE(ESMF_State), INTENT(INOUT) :: state
     INTEGER,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde
     INTEGER,          INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
     INTEGER,          INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
     INTEGER,          INTENT(IN   ) :: domdesc
     LOGICAL,          INTENT(IN   ) :: bdy_mask(4)
     ! locals
     INTEGER :: i, rc
     ! big enough to hold the integer values listed above
     INTEGER(ESMF_KIND_I4) :: intvals(19)
     ! big enough to hold the logical values listed above
     TYPE(ESMF_Logical) :: logvals(4)   ! #$%*ing insane

     ! first the logicals
     ! Usually, when writing an API for a target language, it is considered 
     ! good practice to use native types of the target language in the 
     ! interfaces.  
     logvals = ESMF_FALSE
     DO i = 1, SIZE(bdy_mask)
       IF (bdy_mask(i)) THEN
         logvals(i) = ESMF_TRUE
       ENDIF
     ENDDO
     CALL ESMF_StateSetAttribute(state, 'DecompositionLogicals', &
                                 SIZE(logvals), logvals, rc=rc)
     IF ( rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( 'ESMF_StateSetAttribute(DecompositionLogicals) failed' )
     ENDIF
     ! now the integers
     intvals(1) = ids
     intvals(2) = ide
     intvals(3) = jds
     intvals(4) = jde
     intvals(5) = kds
     intvals(6) = kde
     intvals(7) = ims
     intvals(8) = ime
     intvals(9) = jms
     intvals(10) = jme
     intvals(11) = kms
     intvals(12) = kme
     intvals(13) = ips
     intvals(14) = ipe
     intvals(15) = jps
     intvals(16) = jpe
     intvals(17) = kps
     intvals(18) = kpe
     intvals(19) = domdesc
     CALL ESMF_StateSetAttribute(state, 'DecompositionIntegers', &
                                 SIZE(intvals), intvals, rc=rc)
     IF ( rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( 'ESMF_StateSetAttribute(DecompositionIntegers) failed' )
     ENDIF
   END SUBROUTINE AttachDecompToState



   !$$$ TBH:  Eliminate this once this information can be derived via other 
   !$$$ TBH:  means.  
   SUBROUTINE GetDecompFromState( state,                        & 
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  ips, ipe, jps, jpe, kps, kpe, &
                                  domdesc, bdy_mask )
     TYPE(ESMF_State), INTENT(IN   ) :: state
     INTEGER,          INTENT(  OUT) :: ids, ide, jds, jde, kds, kde
     INTEGER,          INTENT(  OUT) :: ims, ime, jms, jme, kms, kme
     INTEGER,          INTENT(  OUT) :: ips, ipe, jps, jpe, kps, kpe
     INTEGER,          INTENT(  OUT) :: domdesc
     LOGICAL,          INTENT(  OUT) :: bdy_mask(4)
     ! locals
     INTEGER :: i, rc
     ! big enough to hold the integer values listed above
     INTEGER(ESMF_KIND_I4) :: intvals(19)
     ! big enough to hold the logical values listed above
     TYPE(ESMF_Logical) :: logvals(4)   ! #$%*ing insane

     ! first the logicals
     CALL ESMF_StateGetAttribute(state, 'DecompositionLogicals', &
                                 SIZE(logvals), logvals, rc=rc)
     IF ( rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( 'ESMF_StateGetAttribute(DecompositionLogicals) failed' )
     ENDIF
     ! Usually, when writing an API for a target language, it is considered 
     ! good practice to use native types of the target language in the 
     ! interfaces.  
     bdy_mask = .FALSE.
     DO i = 1, SIZE(logvals)
       IF (logvals(i) == ESMF_TRUE) THEN
         bdy_mask(i) = .TRUE.
       ENDIF
     ENDDO
     ! now the integers
     CALL ESMF_StateGetAttribute(state, 'DecompositionIntegers', &
                                 SIZE(intvals), intvals, rc=rc)
     IF ( rc /= ESMF_SUCCESS) THEN
       CALL wrf_error_fatal ( 'ESMF_StateGetAttribute(DecompositionIntegers) failed' )
     ENDIF
     ids = intvals(1)
     ide = intvals(2)
     jds = intvals(3)
     jde = intvals(4)
     kds = intvals(5)
     kde = intvals(6)
     ims = intvals(7)
     ime = intvals(8)
     jms = intvals(9)
     jme = intvals(10)
     kms = intvals(11)
     kme = intvals(12)
     ips = intvals(13)
     ipe = intvals(14)
     jps = intvals(15)
     jpe = intvals(16)
     kps = intvals(17)
     kpe = intvals(18)
     domdesc = intvals(19)
   END SUBROUTINE GetDecompFromState



END MODULE module_metadatautils



MODULE module_wrf_component_top
!<DESCRIPTION>
! This module defines wrf_component_init1(), wrf_component_init2(), 
! wrf_component_run(), and wrf_component_finalize() routines that are called 
! when WRF is run as an ESMF component.  
!</DESCRIPTION>

   USE module_wrf_top
   USE ESMF_Mod
   USE module_esmf_extensions
   USE module_metadatautils, ONLY: AttachTimesToState, AttachDecompToState



   IMPLICIT NONE

   ! everything is private by default
   PRIVATE

   ! Public entry points
   PUBLIC wrf_component_init1
   PUBLIC wrf_component_init2
   PUBLIC wrf_component_run
   PUBLIC wrf_component_finalize

   ! private stuff
   CHARACTER (ESMF_MAXSTR) :: str

CONTAINS


   SUBROUTINE wrf_component_init1( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF component init routine, phase 1.  Passes relevant coupling 
!     information back as metadata on exportState.  
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_GridComp), POINTER :: p_gcomp
     TYPE(ESMF_State),    POINTER :: p_importState
     TYPE(ESMF_State),    POINTER :: p_exportState
     TYPE(ESMF_Clock),    POINTER :: p_clock
     ! Time hackery
     TYPE(ESMF_Time) :: startTime
     TYPE(ESMF_Time) :: stopTime
     TYPE(ESMF_TimeInterval) :: couplingInterval
     ! decomposition hackery
     INTEGER :: ids, ide, jds, jde, kds, kde
     INTEGER :: ims, ime, jms, jme, kms, kme
     INTEGER :: ips, ipe, jps, jpe, kps, kpe
     INTEGER :: domdesc
     LOGICAL :: bdy_mask(4)
     CHARACTER(LEN=256) :: couplingIntervalString

     rc = ESMF_SUCCESS

     p_gcomp => gcomp
     p_importState => importState
     p_exportState => exportState
     p_clock => clock
     ! NOTE:  It will be possible to remove this call once ESMF supports 
     !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(), 
     !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().  
     CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                           exportState=p_exportState, clock=p_clock )

     ! Call WRF "init" routine, suppressing call to init_modules(1) since 
     ! it was already done by the AppDriver.  
     CALL wrf_init( no_init1=.TRUE. )

     ! For now, use settings from WRF component intialization to set up 
     ! top-level clock.  Per suggestion from ESMF Core team, these are passed 
     ! back as attributes on exportState.  
     CALL wrf_clockprint( 100, head_grid%domain_clock, &
            'DEBUG wrf_component_init1():  head_grid%domain_clock,' )
     CALL ESMF_ClockGet(head_grid%domain_clock, startTime=startTime, &
                        stopTime=stopTime, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_component_init1:  ESMF_ClockGet failed' )
     ENDIF
!     CALL wrf_debug( 100, 'DEBUG wrf_component_init1():  before wrf_findCouplingInterval' )
     CALL wrf_findCouplingInterval( startTime, stopTime, couplingInterval )
!     CALL wrf_debug( 100, 'DEBUG wrf_component_init1():  after wrf_findCouplingInterval' )
     CALL ESMF_TimeIntervalGet( couplingInterval, TimeString=couplingIntervalString, &
                                rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_component_init1:  ESMF_TimeIntervalGet failed' )
     ENDIF
     CALL wrf_debug( 100, 'DEBUG wrf_component_init1():  couplingInterval = '//TRIM(couplingIntervalString) )
     CALL AttachTimesToState( exportState, startTime, stopTime, couplingInterval )
     CALL wrf_getDecompInfo( ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe, &
                             domdesc, bdy_mask )
     CALL AttachDecompToState( exportState,                  &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               ips, ipe, jps, jpe, kps, kpe, &
                               domdesc, bdy_mask )

   END SUBROUTINE wrf_component_init1



   SUBROUTINE wrf_component_init2( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF component init routine, phase 2.  Initializes importState and 
!     exportState.  
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_GridComp), POINTER :: p_gcomp
     TYPE(ESMF_State),    POINTER :: p_importState
     TYPE(ESMF_State),    POINTER :: p_exportState
     TYPE(ESMF_Clock),    POINTER :: p_clock
     ! Time hackery
     TYPE(ESMF_Time) :: startTime
     TYPE(ESMF_Time) :: stopTime
     TYPE(ESMF_TimeInterval) :: couplingInterval
     ! decomposition hackery
     INTEGER :: ids, ide, jds, jde, kds, kde
     INTEGER :: ims, ime, jms, jme, kms, kme
     INTEGER :: ips, ipe, jps, jpe, kps, kpe
     INTEGER :: domdesc
     LOGICAL :: bdy_mask(4)
     TYPE(ESMF_StateType) :: statetype
     INTEGER :: itemCount, i
     CHARACTER (ESMF_MAXSTR) :: statename
     CHARACTER (ESMF_MAXSTR), ALLOCATABLE :: itemNames(:)
     TYPE(ESMF_StateItemType), ALLOCATABLE :: itemTypes(:)

     CALL wrf_debug ( 100, 'wrf_component_init2():  begin' )
     ! check exportState
     CALL ESMF_StateGet( exportState, itemCount=itemCount, &
                         statetype=statetype, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  ESMF_StateGet(exportState) failed" )
     ENDIF
     WRITE (str,*) 'wrf_component_init2: exportState itemCount = ', itemCount
     CALL wrf_debug ( 100 , TRIM(str) )
     IF ( statetype /= ESMF_STATE_EXPORT ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  exportState is not an export state" )
     ENDIF
     ! check importState
     CALL ESMF_StateGet( importState, itemCount=itemCount, &
                         statetype=statetype, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  ESMF_StateGet(importState) failed" )
     ENDIF
     WRITE (str,*) 'wrf_component_init2: importState itemCount = ', itemCount
     CALL wrf_debug ( 100 , TRIM(str) )
     IF ( statetype /= ESMF_STATE_IMPORT ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  importState is not an import state" )
     ENDIF

     p_gcomp => gcomp
     p_importState => importState
     p_exportState => exportState
     p_clock => clock
     ! NOTE:  It will be possible to remove this call once ESMF supports 
     !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(), 
     !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().  
     CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                           exportState=p_exportState, clock=p_clock )


     ! This bit of hackery causes wrf_component_run to advance the head_grid 
     ! of WRF up to the point where import and export states have been 
     ! initialized and then return.  
     head_grid%return_after_training_io = .TRUE.
     CALL wrf_component_run( gcomp, importState, exportState, clock, rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_component_init2:  wrf_component_run failed' )
     ENDIF

     ! examine importState
     WRITE (str,*) 'wrf_component_init2: EXAMINING importState...'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL ESMF_StateGet( importState, itemCount=itemCount, &
                         statetype=statetype, name=statename, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  ESMF_StateGet(importState) failed B" )
     ENDIF
     IF ( statetype /= ESMF_STATE_IMPORT ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  importState is not an import state" )
     ENDIF
     WRITE (str,*) 'wrf_component_init2: importState <',TRIM(statename), &
                   '> itemCount = ', itemCount
     CALL wrf_debug ( 100 , TRIM(str) )
     ALLOCATE ( itemNames(itemCount), itemTypes(itemCount) )
     CALL ESMF_StateGet( importState, itemNameList=itemNames, &
                         stateitemtypeList=itemTypes, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  ESMF_StateGet(importState) failed C" )
     ENDIF
     DO i=1, itemCount
       IF ( itemTypes(i) == ESMF_STATEITEM_FIELD ) THEN
         WRITE(str,*) 'wrf_component_init2: importState contains field <',TRIM(itemNames(i)),'>'
         CALL wrf_debug ( 100 , TRIM(str) )
       ENDIF
     ENDDO
     DEALLOCATE ( itemNames, itemTypes )
     WRITE (str,*) 'wrf_component_init2: DONE EXAMINING importState...'
     CALL wrf_debug ( 100 , TRIM(str) )

     ! examine exportState
     WRITE (str,*) 'wrf_component_init2: EXAMINING exportState...'
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL ESMF_StateGet( exportState, itemCount=itemCount, &
                         statetype=statetype, name=statename, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  ESMF_StateGet(exportState) failed B" )
     ENDIF
     IF ( statetype /= ESMF_STATE_EXPORT ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  exportState is not an export state" )
     ENDIF
     WRITE (str,*) 'wrf_component_init2: exportState <',TRIM(statename), &
                   '> itemCount = ', itemCount
     CALL wrf_debug ( 100 , TRIM(str) )
     ALLOCATE ( itemNames(itemCount), itemTypes(itemCount) )
     CALL ESMF_StateGet( exportState, itemNameList=itemNames, &
                         stateitemtypeList=itemTypes, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal("wrf_component_init2:  ESMF_StateGet(exportState) failed C" )
     ENDIF
     DO i=1, itemCount
       IF ( itemTypes(i) == ESMF_STATEITEM_FIELD ) THEN
         WRITE(str,*) 'wrf_component_init2: exportState contains field <',TRIM(itemNames(i)),'>'
         CALL wrf_debug ( 100 , TRIM(str) )
       ENDIF
     ENDDO
     DEALLOCATE ( itemNames, itemTypes )
     WRITE (str,*) 'wrf_component_init2: DONE EXAMINING exportState...'
     CALL wrf_debug ( 100 , TRIM(str) )

     CALL wrf_debug ( 100, 'DEBUG wrf_component_init2():  end' )

   END SUBROUTINE wrf_component_init2



   SUBROUTINE wrf_component_run( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF component run routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_GridComp), POINTER :: p_gcomp
     TYPE(ESMF_State),    POINTER :: p_importState
     TYPE(ESMF_State),    POINTER :: p_exportState
     TYPE(ESMF_Clock),    POINTER :: p_clock
     ! timing
     TYPE(ESMF_Time) :: currentTime, nextTime
     TYPE(ESMF_TimeInterval) :: runLength     ! how long to run in this call
     CHARACTER(LEN=256) :: timeStr

     CALL wrf_debug ( 100 , 'DEBUG wrf_component_run():  begin' )

     p_gcomp => gcomp
     p_importState => importState
     p_exportState => exportState
     p_clock => clock
     ! NOTE:  It will be possible to remove this call once ESMF supports 
     !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(), 
     !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().  
     CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                           exportState=p_exportState, clock=p_clock )

     ! connect ESMF clock with WRF domain clock
     CALL ESMF_ClockGet( clock, currTime=currentTime, timeStep=runLength, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal ( 'wrf_component_run:  ESMF_ClockGet failed' )
     ENDIF
     CALL wrf_clockprint(100, clock, &
            'DEBUG wrf_component_run():  clock,')
     nextTime = currentTime + runLength
     head_grid%start_subtime = currentTime
     head_grid%stop_subtime = nextTime
     CALL wrf_timetoa ( head_grid%start_subtime, timeStr )
     WRITE (str,*) 'wrf_component_run:  head_grid%start_subtime ',TRIM(timeStr)
     CALL wrf_debug ( 100 , TRIM(str) )
     CALL wrf_timetoa ( head_grid%stop_subtime, timeStr )
     WRITE (str,*) 'wrf_component_run:  head_grid%stop_subtime ',TRIM(timeStr)
     CALL wrf_debug ( 100 , TRIM(str) )

     ! Call WRF "run" routine
     CALL wrf_debug ( 100 , 'DEBUG wrf_component_run():  calling wrf_run()' )
     CALL wrf_run( )
     CALL wrf_debug ( 100 , 'DEBUG wrf_component_run():  back from wrf_run()' )

     ! This bit of hackery will cause the next call to wrf_run() to 
     ! resume advance of the head_grid from the point where import and 
     ! export states were initialized.  When grid%return_after_training_io 
     ! is .TRUE., wrf_run() returns right after import and export states 
     ! are initialized.  This hack is triggered in wrf_component_init2.
     IF ( head_grid%return_after_training_io ) THEN
       head_grid%return_after_training_io = .FALSE.
     ENDIF

     CALL wrf_debug ( 100 , 'DEBUG wrf_component_run():  end' )

   END SUBROUTINE wrf_component_run



   SUBROUTINE wrf_component_finalize( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF component finalize routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_GridComp), POINTER :: p_gcomp
     TYPE(ESMF_State),    POINTER :: p_importState
     TYPE(ESMF_State),    POINTER :: p_exportState
     TYPE(ESMF_Clock),    POINTER :: p_clock
     INTEGER :: rc
     p_gcomp => gcomp
     p_importState => importState
     p_exportState => exportState
     p_clock => clock
     ! NOTE:  It will be possible to remove this call once ESMF supports 
     !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(), 
     !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().  
     CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                           exportState=p_exportState, clock=p_clock )

     ! Call WRF "finalize" routine, suppressing call to MPI_FINALIZE so 
     ! ESMF can do it (if needed) during ESMF_Finalize().  
     CALL wrf_finalize( no_shutdown=.TRUE. )

     rc = ESMF_SUCCESS

   END SUBROUTINE wrf_component_finalize



   SUBROUTINE wrf_findCouplingInterval( startTime, stopTime, couplingInterval )
     TYPE(ESMF_Time),         INTENT(IN   ) :: startTime
     TYPE(ESMF_Time),         INTENT(IN   ) :: stopTime
     TYPE(ESMF_TimeInterval), INTENT(  OUT) :: couplingInterval
!<DESCRIPTION>
!     WRF convenience routine for deducing coupling interval.  The startTime 
!     and stopTime arguments are only used for determining a default value 
!     when coupling is not actually being done.  
!
!     The arguments are:
!       startTime          start time
!       stopTime           stop time
!       couplingInterval   coupling interval
!</DESCRIPTION>
     ! locals
     LOGICAL :: foundcoupling
     INTEGER :: rc
     INTEGER :: io_form
     ! external function prototype
     INTEGER, EXTERNAL :: use_package

     ! deduce coupling time-step
     foundcoupling = .FALSE.
!$$$here...  this bit just finds the FIRST case and extracts coupling interval
!$$$here...  add error-checking for over-specification
!$$$here...  add support for multiple coupling intervals later...  
!$$$here...  add support for coupling that does not begin immediately later...  
!$$$ get rid of this hideous duplication!!  
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput1( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT1_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT1_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput2( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT2_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT2_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput3( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT3_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT3_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput4( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT4_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT4_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput5( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT5_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT5_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput6( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT6_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT6_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput7( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT7_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT7_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput8( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT8_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT8_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput9( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT9_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT9_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_gfdda( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT10_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT10_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxinput11( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXINPUT11_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXINPUT11_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF


     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist1( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST1_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST1_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist2( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST2_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST2_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist3( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST3_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST3_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist4( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST4_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST4_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist5( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST5_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST5_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist6( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST6_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST6_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist7( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST7_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST7_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist8( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST8_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST8_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist9( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST9_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST9_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist10( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST10_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST10_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF
     IF ( .NOT. foundcoupling ) THEN
       CALL nl_get_io_form_auxhist11( 1, io_form )
       IF ( use_package( io_form ) == IO_ESMF ) THEN
         CALL ESMF_AlarmGet( head_grid%alarms( AUXHIST11_ALARM ), &
                             RingInterval=couplingInterval, rc=rc )
         IF ( rc /= ESMF_SUCCESS ) THEN
           CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ESMF_AlarmGet(AUXHIST11_ALARM) failed' )
         ENDIF
         foundcoupling = .TRUE.
       ENDIF
     ENDIF

     ! look for erroneous use of io_form...  
     CALL nl_get_io_form_restart( 1, io_form )
     IF ( use_package( io_form ) == IO_ESMF ) THEN
       CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ERROR:  ESMF cannot be used for WRF restart I/O' )
     ENDIF
     CALL nl_get_io_form_input( 1, io_form )
     IF ( use_package( io_form ) == IO_ESMF ) THEN
       CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ERROR:  ESMF cannot be used for WRF input' )
     ENDIF
     CALL nl_get_io_form_history( 1, io_form )
     IF ( use_package( io_form ) == IO_ESMF ) THEN
       CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ERROR:  ESMF cannot be used for WRF history output' )
     ENDIF
     CALL nl_get_io_form_boundary( 1, io_form )
     IF ( use_package( io_form ) == IO_ESMF ) THEN
       CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ERROR:  ESMF cannot be used for WRF boundary I/O' )
     ENDIF

     ! If nobody uses IO_ESMF, then default is to run WRF all the way to 
     ! the end.  
     IF ( .NOT. foundcoupling ) THEN
       couplingInterval = stopTime - startTime
       call wrf_debug ( 1, 'WARNING:  ESMF coupling not used in this WRF run' )
     ENDIF

   END SUBROUTINE wrf_findCouplingInterval



   SUBROUTINE wrf_getDecompInfo( ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe, &
                                 domdesc, bdy_mask )
     INTEGER, INTENT(OUT) :: ids, ide, jds, jde, kds, kde
     INTEGER, INTENT(OUT) :: ims, ime, jms, jme, kms, kme
     INTEGER, INTENT(OUT) :: ips, ipe, jps, jpe, kps, kpe
     INTEGER, INTENT(OUT) :: domdesc
     LOGICAL, INTENT(OUT) :: bdy_mask(4)
!<DESCRIPTION>
!     WRF convenience routine for deducing decomposition information.  
!     Note that domdesc is meaningful only for SPMD serial operation.  
!     For concurrent operation (SPMD or MPMD), we will need to create a new 
!     "domdesc" suitable for the task layout of the SST component.  For 
!     MPMD serial operation, we will need to serialize domdesc and store it 
!     as metadata within the export state.  Similar arguments apply
!     to [ij][mp][se] and bdy_mask.
!
!     The arguments are:
!       ids, ide, jds, jde, kds, kde    Domain extent.  
!       ims, ime, jms, jme, kms, kme    Memory extent.  
!       ips, ipe, jps, jpe, kps, kpe    Patch extent.  
!       domdesc                         Domain descriptor for external 
!                                       distributed-memory communication 
!                                       package (opaque to WRF).  
!       bdy_mask                        Boundary mask flags indicating which 
!                                       domain boundaries are on this task.  
!</DESCRIPTION>
     ! extract decomposition information from head_grid
     CALL get_ijk_from_grid( head_grid ,                   &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe  )
     domdesc = head_grid%domdesc
     bdy_mask = head_grid%bdy_mask
   END SUBROUTINE wrf_getDecompInfo


END MODULE module_wrf_component_top




MODULE module_wrf_setservices
!<DESCRIPTION>
! This module defines WRF "Set Services" method wrf_register() 
! used for ESMF coupling.  
!</DESCRIPTION>

   USE module_wrf_component_top, ONLY: wrf_component_init1, &
                                       wrf_component_init2, &
                                       wrf_component_run,   &
                                       wrf_component_finalize
   USE ESMF_Mod

   IMPLICIT NONE

   ! everything is private by default
   PRIVATE

   ! Public entry point for ESMF_GridCompSetServices()
   PUBLIC WRF_register

   ! private stuff
   CHARACTER (ESMF_MAXSTR) :: str

CONTAINS


   SUBROUTINE wrf_register(gcomp, rc)
     TYPE(ESMF_GridComp), INTENT(INOUT) :: gcomp
     INTEGER, INTENT(OUT) :: rc
!
!<DESCRIPTION>
!     WRF_register - Externally visible registration routine
!
!     User-supplied SetServices routine.
!     The Register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that these are
!     private to the module.
!
!     The arguments are:
!       gcomp           Component
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     rc = ESMF_SUCCESS
     ! Register the callback routines.
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
                                     wrf_component_init1, 1, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        CALL wrf_error_fatal ( 'wrf_register:  ESMF_GridCompSetEntryPoint(wrf_component_init1) failed' )
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
                                     wrf_component_init2, 2, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        CALL wrf_error_fatal ( 'wrf_register:  ESMF_GridCompSetEntryPoint(wrf_component_init2) failed' )
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, &
                                     wrf_component_run, ESMF_SINGLEPHASE, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        CALL wrf_error_fatal ( 'wrf_register:  ESMF_GridCompSetEntryPoint(wrf_component_run) failed' )
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, &
                                     wrf_component_finalize, ESMF_SINGLEPHASE, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        CALL wrf_error_fatal ( 'wrf_register:  ESMF_GridCompSetEntryPoint(wrf_component_finalize) failed' )
     ENDIF
     PRINT *,'WRF:  Registered Initialize, Run, and Finalize routines'

   END SUBROUTINE wrf_register

END MODULE module_wrf_setservices

