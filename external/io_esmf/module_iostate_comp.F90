
! "iostate_comp" allows the io_esmf external I/O package to define and store 
! package-specific state without adding package-specific dependencies 
! to WRF framework types defined in module domain.  It will be possible to 
! eliminate much of this module once the ESMF core team implements my request 
! for new ESMF interfaces ESMF_GridCompGetCurrent(), 
! ESMF_StateGetCurrentImport(), ESMF_StateGetCurentExport(), and 
! ESMF_ClockGetCurrent().  These proposed interfaces are identical in intent 
! to existing interface ESMF_VMGetCurrent().  

! This module works, but does nothing useful when the embedded ESMF 
! library in external/esmf_time_f90/ is used instead of io_esmf.  
! In this case, cpp token ESMF_COUPLING will *not* be defined.  
! This avoids having to add lots of dummy data types to esmf_time_f90.  

MODULE module_iostate_comp

  USE ESMF_Mod

  IMPLICIT NONE

  PRIVATE


  ! public routines
  PUBLIC init_iostate_comp
  ! TBH:  May be able to eliminate these "set" and "get" interfaces once 
  ! TBH:  ESMF_*GetCurrent() are implemented as described above...  
  PUBLIC set_iostate_comp
  PUBLIC get_iostate_comp
  PUBLIC get_iostate_comp_current
  PUBLIC final_iostate_comp


  ! private parameters
  ! get MAX_DOMAINS from cpp token
  INTEGER, PARAMETER :: MAX_IOSTATES = MAX_DOMAINS_F

  ! private typedefs
  TYPE iostate_comp
    INTEGER                         :: id          ! unique ID for this iostate
    TYPE(ESMF_GridComp) , POINTER   :: gcomp
    TYPE(ESMF_State)    , POINTER   :: importState
    TYPE(ESMF_State)    , POINTER   :: exportState
    TYPE(ESMF_Clock)    , POINTER   :: clock
    TYPE(ESMF_Grid)     , POINTER   :: grid
    LOGICAL                         :: in_use
  END TYPE iostate_comp

  ! private declarations
  LOGICAL, SAVE :: need_init = .TRUE.
  TYPE(iostate_comp) , DIMENSION(0:MAX_IOSTATES-1) :: comp_iostates
  INTEGER, SAVE :: current_iostate_id = -1   ! invalid initial value

  CHARACTER (256) :: msg  ! use this for error messages


CONTAINS


  ! Initialize this iostate_comp.  
  ! The first time this is called, it also allocates memory for all 
  ! comp_iostates.  
  ! This should be called once for every WRF domain that will use ESMF 
  ! coupling.  
  SUBROUTINE init_iostate_comp( id_in, numprocsX,    numprocsY,    &
                                       periodicX,    periodicY,    &
                                       coordXin,     coordYin,     &
                                       globalXcount, globalYcount, &
                                       myXstart,     myXend,       &
                                       myYstart,     myYend )
    INTEGER, INTENT(IN   ) :: id_in         ! index to comp_iostates
    INTEGER, INTENT(IN   ) :: numprocsX     ! number of tasks in "i" dimension
    INTEGER, INTENT(IN   ) :: numprocsY     ! number of tasks in "j" dimension
    LOGICAL, INTENT(IN   ) :: periodicX     ! periodicity in "i" dimension
    LOGICAL, INTENT(IN   ) :: periodicY     ! periodicity in "j" dimension
    INTEGER, INTENT(IN   ) :: globalXcount  ! domain count in "i" dimension
    INTEGER, INTENT(IN   ) :: globalYcount  ! domain count in "j" dimension
    ! ESMF_GridCreate() defines coordinates at *vertices*
    REAL   , INTENT(IN   ) :: coordXin(globalXcount+1) ! all domain "i" coordinates
    REAL   , INTENT(IN   ) :: coordYin(globalYcount+1) ! all domain "j" coordinates
    INTEGER, INTENT(IN   ) :: myXstart      ! task-local start in "i" dimension
    INTEGER, INTENT(IN   ) :: myYstart      ! task-local start in "j" dimension
    INTEGER, INTENT(IN   ) :: myXend    ! non-staggered task-local end   in "i" dimension
    INTEGER, INTENT(IN   ) :: myYend    ! non-staggered task-local end   in "j" dimension
#ifdef ESMF_COUPLING
    ! Local declarations
    INTEGER :: id, stat, rc
    INTEGER               :: myXcount      ! task-local count in "i" dimension
    INTEGER               :: myYcount      ! task-local count in "j" dimension
    INTEGER               :: allXStart(0:(numprocsX*numprocsY)-1)
    INTEGER               :: allXCount(0:(numprocsX*numprocsY)-1)
    INTEGER               :: dimXCount(0:numprocsX-1)
    INTEGER               :: allYStart(0:(numprocsX*numprocsY)-1)
    INTEGER               :: allYCount(0:(numprocsX*numprocsY)-1)
    INTEGER               :: dimYCount(0:numprocsY-1)
    INTEGER               ::   cellCounts(0:(numprocsX*numprocsY)-1,2)
    INTEGER               :: globalStarts(0:(numprocsX*numprocsY)-1,2)
    INTEGER               :: permuteTasks(numprocsX*numprocsY)
    REAL(ESMF_KIND_R8) :: coordX(globalXcount+1), coordY(globalYcount+1)
    INTEGER :: globalCellCounts(2)
    INTEGER :: numprocsXY(2)
    INTEGER :: myPE, i, j, pe, is, ie, js, je, is_min, js_min, ie_max, je_max
    TYPE(ESMF_VM) :: vm
    TYPE(ESMF_DELayout) :: taskLayout
    TYPE(ESMF_Logical) :: periodicXY(2)
    CHARACTER (32) :: gridname

    ! Allocate space for each element of comp_iostates, if it hasn't been done 
    ! already.  
    ! TBH:  Optimize later to only allocate elements that are actually used!  
    ! TBH:  For now leave as-is since ESMF objects are currently very 
    ! TBH:  lightweight on the Fortran side.  
    IF (need_init) THEN
      DO id = 0, MAX_IOSTATES-1
        ALLOCATE( comp_iostates( id )%gcomp,       &
                  comp_iostates( id )%importState, &
                  comp_iostates( id )%exportState, &
                  comp_iostates( id )%clock,       &
                  comp_iostates( id )%grid,        &
                  STAT=stat )
        IF ( stat /= 0 ) THEN
          WRITE( msg,* ) 'Could not allocate comp_iostates in ', &
                         __FILE__ ,                              &
                         ', line',                               &
                         __LINE__
          CALL wrf_error_fatal ( msg )
        ENDIF
        comp_iostates(id)%id = id
        comp_iostates(id)%in_use = .FALSE.
      ENDDO
      need_init = .FALSE.
    ENDIF

    ! Set up ESMF objects for WRF domain id_in.  

    ! Initialize iostate_comp for this WRF domain.  
    IF (comp_iostates(id_in)%in_use) THEN
      WRITE( msg,* ) 'Cannot initialize comp_iostates(',id_in, &
                     '), it is already in use',                &
                     __FILE__ ,                                &
                     ', line',                                 &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ELSE
      comp_iostates(id_in)%in_use = .TRUE.
    ENDIF

    ! get current ESMF virtual machine and set the task layout
    CALL ESMF_VMGetCurrent(vm, rc)
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_VMGetCurrent', &
                     __FILE__ ,                    &
                     ', line',                     &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
    numprocsXY = (/ numprocsX, numprocsY /)
    ! transpose tasks to match RSL
    pe = 0
    DO j = 0, numprocsY-1
      DO i = 0, numprocsX-1
      pe = pe + 1
      permuteTasks(pe) = (i*numprocsY) + j
      ENDDO
    ENDDO
!$$$here...  replace DEBUG prints with wrf_debug calls
PRINT *,'DEBUG:  permuteTasks = ',permuteTasks
    taskLayout = ESMF_DELayoutCreate( vm, numprocsXY, dePetList=permuteTasks, rc=rc ) 
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_DELayoutCreate', &
                     __FILE__ ,                      &
                     ', line',                       &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
    ! get ESMF task ids
    CALL ESMF_DELayoutGet(taskLayout, localDe=myPE, rc=rc)
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_DELayoutGet', &
                     __FILE__ ,                   &
                     ', line',                    &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF

    ! gather task-local information on all tasks since 
    ! ESMF_GridDistribute[Block] interface require global knowledge to set up 
    ! decompositions (@#$%)
    myXcount = myXend - myXstart + 1
    myYcount = myYend - myYstart + 1
    CALL GatherIntegerScalars_ESMF(myXcount, myPE, numprocsX*numprocsY, allXCount)
    CALL GatherIntegerScalars_ESMF(myXstart, myPE, numprocsX*numprocsY, allXStart)
    CALL GatherIntegerScalars_ESMF(myYcount, myPE, numprocsX*numprocsY, allYCount)
    CALL GatherIntegerScalars_ESMF(myYstart, myPE, numprocsX*numprocsY, allYStart)

    ! Create an ESMF_Grid
    ! For now we create only a 2D grid suitable for simple coupling of 2D 
    ! surface fields.  Later, create and subset one or more 3D grids.  
!TBH $$$:  NOTE that we'll have to use ESMF_GRID_HORZ_STAGGER_E_?? for NMM.  
!TBH $$$:  E-grid is not yet supported by ESMF.  Eventually pass staggering 
!TBH $$$:  info into this routine.  For now, hard-code it for WRF-ARW.  
    periodicXY = ESMF_FALSE
    IF (periodicX) periodicXY(1) = ESMF_TRUE
    IF (periodicY) periodicXY(2) = ESMF_TRUE
    coordX = coordXin
    coordY = coordYin
    WRITE ( gridname,'(a,i0)' ) 'WRF_grid_', id_in
    comp_iostates( id_in )%grid = ESMF_GridCreateHorzXY(                     &
                                    coord1=coordX, coord2=coordY,            &
                                    horzstagger=ESMF_GRID_HORZ_STAGGER_C_SW, &
! use this for 3D Grids once it is stable
!                                   coordorder=ESMF_COORD_ORDER_XZY,         &
                                    periodic=periodicXY,                     &
                                    name=TRIM(gridname), rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_GridCreateHorzXY', &
                     __FILE__ ,                        &
                     ', line',                         &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
    ! distribute the ESMF Grid
    ! ignore repeated values
    is_min = MINVAL(allXStart)
    js_min = MINVAL(allYStart)
    i = 0
    j = 0
PRINT *,'DEBUG:  is_min = ',is_min,'  allXStart = ',allXStart
PRINT *,'DEBUG:  js_min = ',js_min,'  allYStart = ',allYStart
PRINT *,'DEBUG:  allXCount = ',allXCount
PRINT *,'DEBUG:  allYCount = ',allYCount
    DO pe = 0, (numprocsX*numprocsY)-1
      IF (allXStart(pe) == is_min) THEN
        IF (j >= numprocsY) THEN
          WRITE( msg,* ) 'ASSERTION Error in ESMF_GridCreateHorzXY', &
                         __FILE__ ,                        &
                         ', line',                         &
                         __LINE__
          CALL wrf_error_fatal ( msg )
        ENDIF
PRINT *,'DEBUG:  dimYCount(',j,') = allYCount(',pe,')'
        dimYCount(j) = allYCount(pe)
        j = j + 1
      ENDIF
      IF (allYStart(pe) == js_min) THEN
        IF (i >= numprocsX) THEN
          WRITE( msg,* ) 'ASSERTION Error in ESMF_GridCreateHorzXY', &
                         __FILE__ ,                        &
                         ', line',                         &
                         __LINE__
          CALL wrf_error_fatal ( msg )
        ENDIF
PRINT *,'DEBUG:  dimXCount(',i,') = allXCount(',pe,')'
        dimXCount(i) = allXCount(pe)
        i = i + 1
      ENDIF
    ENDDO
PRINT *,'DEBUG:  i = ',i,'  dimXCount = ',dimXCount
PRINT *,'DEBUG:  j = ',j,'  dimYCount = ',dimYCount
!!!PRINT *,'DEBUG:  EARLY EXIT FOR DEBUG prior to calling ESMF_GridDistribute()'
!!!CALL wrf_error_fatal ( msg )
    CALL ESMF_GridDistribute( comp_iostates( id_in )%grid, &
                              delayout=taskLayout,         &
                              countsPerDEDim1=dimXCount,   &
                              countsPerDEDim2=dimYCount,   &
                              rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_GridDistribute', &
                     __FILE__ ,                      &
                     ', line',                       &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF

    ! Print out the ESMF decomposition info for debug comparison with WRF 
    ! decomposition info.  
    CALL ESMF_GridGet( comp_iostates( id_in )%grid,            &
                       horzrelloc=ESMF_CELL_CENTER,            &
                       globalStartPerDEPerDim=globalStarts,    &
                       cellCountPerDEPerDim=cellCounts,        &
                       globalCellCountPerDim=globalCellCounts, &
                       rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_GridGet', &
                     __FILE__ ,               &
                     ', line',                &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
! note that global indices in ESMF_Grid always start at zero
!$$$ replace with calls to wrf_debug
PRINT *, 'DEBUG:  ESMF task-id = ',myPE
PRINT *, 'DEBUG:  ESMF non-staggered     ips = ',1+globalStarts(myPE,1)
PRINT *, 'DEBUG:  ESMF non-staggered     ipe = ',1+globalStarts(myPE,1) + cellCounts(myPE,1) - 1
PRINT *, 'DEBUG:  ESMF non-staggered i count = ',  cellCounts(myPE,1)
PRINT *, 'DEBUG:  ESMF non-staggered     jps = ',1+globalStarts(myPE,2)
PRINT *, 'DEBUG:  ESMF non-staggered     jpe = ',1+globalStarts(myPE,2) + cellCounts(myPE,2) - 1
PRINT *, 'DEBUG:  ESMF non-staggered j count = ',  cellCounts(myPE,2)
    is_min = globalStarts(0,1)
    js_min = globalStarts(0,2)
    ie_max = globalStarts(0,1) + cellCounts(0,1) - 1
    je_max = globalStarts(0,2) + cellCounts(0,2) - 1
    DO pe = 1, (numprocsX*numprocsY)-1
      js = globalStarts(pe,2)
      je = globalStarts(pe,2) + cellCounts(pe,2) - 1
      IF ( js < js_min ) js_min = js
      IF ( je > je_max ) je_max = je
      is = globalStarts(pe,1)
      ie = globalStarts(pe,1) + cellCounts(pe,1) - 1
      IF ( is < is_min ) is_min = is
      IF ( ie > ie_max ) ie_max = ie
    ENDDO

!TBH: $$$  Selection of "root" task for I/O is hacked here.  Fix later...  
!    IF ( myPE == 0 ) THEN
!      ALLOCATE( show_domain( is_min:ie_max, js_min:je_max ) )
!      show_domain = -1    ! intentional bad value
!      DO pe = 0, (numprocsX*numprocsY)-1
!        js = globalStarts(pe,2)
!        je = globalStarts(pe,2) + cellCounts(pe,2) - 1
!        is = globalStarts(pe,1)
!        ie = globalStarts(pe,1) + cellCounts(pe,1) - 1
!        DO j = js, je
!          DO i = is, ie
!            show_domain(i,j) = pe - 1
!          ENDDO
!        ENDDO
!      ENDDO
!      !$$$ HACK FOR I/O!!!  Replace this...  
!      ! RSL writes show_domain_0000 transposed with staggered points included, Hmm...  
!      OPEN( UNIT=28, FILE='show_domain_ESMF', FORM='FORMATTED', STATUS='NEW' )
!      WRITE( 28, * ) 'DEBUG:  is_min,ie_max,js_min,je_max = ',is_min,ie_max,js_min,je_max
!      WRITE( 28, * ) 'domain=',id_in-1,', len_n=',ie_max-is_min+1,', len_m=',je_max-js_min+1
!      DO j=je_max,js_min,-1
!        WRITE( 28, 9030 ) ( show_domain(i,j), i=is_min,ie_max )
!      ENDDO
! 9030 FORMAT(1000i3)
!      CLOSE( UNIT=28 )
!      DEALLOCATE( show_domain )
!    ENDIF

    ! extract information about staggered grids for debugging
    CALL ESMF_GridGet( comp_iostates( id_in )%grid,            &
                       horzrelloc=ESMF_CELL_WFACE,             &
                       globalStartPerDEPerDim=globalStarts,    &
                       cellCountPerDEPerDim=cellCounts,        &
                       globalCellCountPerDim=globalCellCounts, &
                       rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_GridGet', &
                     __FILE__ ,               &
                     ', line',                &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
! note that global indices in ESMF_Grid always start at zero
!$$$ replace with calls to wrf_debug
PRINT *, 'DEBUG:  ESMF     staggered     ips = ',1+globalStarts(myPE,1)
PRINT *, 'DEBUG:  ESMF     staggered     ipe = ',1+globalStarts(myPE,1) + cellCounts(myPE,1) - 1
PRINT *, 'DEBUG:  ESMF     staggered i count = ',  cellCounts(myPE,1)
    CALL ESMF_GridGet( comp_iostates( id_in )%grid,            &
                       horzrelloc=ESMF_CELL_SFACE,             &
                       globalStartPerDEPerDim=globalStarts,    &
                       cellCountPerDEPerDim=cellCounts,        &
                       globalCellCountPerDim=globalCellCounts, &
                       rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_GridGet', &
                     __FILE__ ,               &
                     ', line',                &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
! note that global indices in ESMF_Grid always start at zero
!$$$ replace with calls to wrf_debug
PRINT *, 'DEBUG:  ESMF     staggered     jps = ',1+globalStarts(myPE,2)
PRINT *, 'DEBUG:  ESMF     staggered     jpe = ',1+globalStarts(myPE,2) + cellCounts(myPE,2) - 1
PRINT *, 'DEBUG:  ESMF     staggered j count = ',  cellCounts(myPE,2)

#endif

  END SUBROUTINE init_iostate_comp


#ifdef ESMF_COUPLING
  ! allgather for integers, ESMF_style (since ESMF does not do this yet)
  SUBROUTINE GatherIntegerScalars_ESMF( inval, pe, numprocs, outvals )
    INTEGER, INTENT(IN   ) :: inval                 ! input scalar on this task
    INTEGER, INTENT(IN   ) :: pe                    ! task id
    INTEGER, INTENT(IN   ) :: numprocs              ! number of tasks
    INTEGER, INTENT(  OUT) :: outvals(0:numprocs-1) ! gathered output vector
    ! Local declarations
    TYPE(ESMF_VM) :: vm
    INTEGER(ESMF_KIND_I4) :: allSnd(0:numprocs-1)
    INTEGER(ESMF_KIND_I4) :: allRcv(0:numprocs-1)
    INTEGER :: rc

    ! get current ESMF virtual machine for communication
    CALL ESMF_VMGetCurrent(vm, rc)
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_VMGetCurrent', &
                     __FILE__ ,                    &
                     ', line',                     &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
    allSnd = 0_ESMF_KIND_I4
    allSnd(pe) = inval
    ! Hack due to lack of ESMF_VMAllGather().  
    CALL ESMF_VMAllReduce(vm, allSnd, allRcv, numprocs, ESMF_SUM, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_VMAllReduce', &
                     __FILE__ ,                     &
                     ', line',                      &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
    outvals = allRcv

  END SUBROUTINE GatherIntegerScalars_ESMF
#endif


  ! assertion for correct initialization
  SUBROUTINE check_init( id, line )
    INTEGER, INTENT(IN   ) :: id     ! index to comp_iostates
    INTEGER, INTENT(IN   ) :: line   ! line number in this file
    IF ( need_init ) THEN
      WRITE( msg,* ) 'module_iostate_comp not initialized in ', &
                     __FILE__ ,                                 &
                     ', line', line
      CALL wrf_error_fatal ( msg )
    ELSE
      IF ( .NOT. comp_iostates(id)%in_use ) THEN
        WRITE( msg,* ) 'Cannot use uninitialized comp_iostates(',id,')', &
                       __FILE__ ,                                        &
                       ', line', line
        CALL wrf_error_fatal ( msg )
      ENDIF
    ENDIF
  END SUBROUTINE check_init


  ! range check on id
  SUBROUTINE check_id( id, line )
    INTEGER, INTENT(IN   ) :: id     ! index into comp_iostates
    INTEGER, INTENT(IN   ) :: line   ! line number in this file
    IF ( ( id < 0 ) .OR. ( id >= MAX_IOSTATES ) ) THEN
      WRITE( msg,* ) 'iostate id out of range in ', &
                     __FILE__ ,                     &
                     ', line', line
      CALL wrf_error_fatal ( msg )
    ENDIF
  END SUBROUTINE check_id


  ! Set values in a selected element of comp_iostates
  ! Also sets the id of the currently selected element of comp_iostates
  SUBROUTINE set_iostate_comp( id, gcomp, importState, exportState, clock )
    INTEGER,                       INTENT(IN   ) :: id  ! index to comp_iostates
    TYPE(ESMF_GridComp), OPTIONAL, INTENT(IN   ) :: gcomp
    TYPE(ESMF_State),    OPTIONAL, INTENT(IN   ) :: importState, exportState
    TYPE(ESMF_Clock),    OPTIONAL, INTENT(IN   ) :: clock

#ifdef ESMF_COUPLING
    CALL check_id  ( id, __LINE__ )  ! range check on id
    CALL check_init( id, __LINE__ )  ! check initialization

    ! set values of selected iostate
    current_iostate_id = id
    IF ( PRESENT( gcomp ) )       comp_iostates( id )%gcomp       = gcomp
    IF ( PRESENT( importState ) ) comp_iostates( id )%importState = importState
    IF ( PRESENT( exportState ) ) comp_iostates( id )%exportState = exportState
    IF ( PRESENT( clock ) )       comp_iostates( id )%clock       = clock
#endif

  END SUBROUTINE set_iostate_comp


  ! Get values from a selected element of comp_iostates
  SUBROUTINE get_iostate_comp( id, gcomp, importState, exportState, clock )
    INTEGER,                       INTENT(IN   ) :: id  ! index to comp_iostates
    TYPE(ESMF_GridComp), OPTIONAL, INTENT(  OUT) :: gcomp
    TYPE(ESMF_State),    OPTIONAL, INTENT(  OUT) :: importState, exportState
    TYPE(ESMF_Clock),    OPTIONAL, INTENT(  OUT) :: clock

#ifdef ESMF_COUPLING
    CALL check_id  ( id, __LINE__ )  ! range check on id
    CALL check_init( id, __LINE__ )  ! check initialization

    ! get values of selected iostate
    IF ( PRESENT( gcomp ) )       gcomp       = comp_iostates( id )%gcomp
    IF ( PRESENT( importState ) ) importState = comp_iostates( id )%importState
    IF ( PRESENT( exportState ) ) exportState = comp_iostates( id )%exportState
    IF ( PRESENT( clock ) )       clock       = comp_iostates( id )%clock
#endif

  END SUBROUTINE get_iostate_comp


  ! Get the ID of the current element of comp_iostates
  SUBROUTINE get_iostate_comp_current_id( id )
    INTEGER,                       INTENT(  OUT) :: id  ! index to comp_iostates
#ifdef ESMF_COUPLING
    id = current_iostate_id
#endif
  END SUBROUTINE get_iostate_comp_current_id


  ! Get values from the currently selected element of comp_iostates
  SUBROUTINE get_iostate_comp_current( gcomp, importState, exportState, clock )
    TYPE(ESMF_GridComp), OPTIONAL, INTENT(  OUT) :: gcomp
    TYPE(ESMF_State),    OPTIONAL, INTENT(  OUT) :: importState, exportState
    TYPE(ESMF_Clock),    OPTIONAL, INTENT(  OUT) :: clock
    ! Local declarations
    INTEGER :: id

#ifdef ESMF_COUPLING
    CALL get_iostate_comp_current_id( id )
    CALL get_iostate_comp( id, gcomp, importState, exportState, clock )
#endif

  END SUBROUTINE get_iostate_comp_current


  ! Deallocate this iostate_comp.  
  ! This should be called once for every WRF domain that will use ESMF 
  ! coupling.  
  ! If this is called for the last iostate_comp in use, it will free 
  ! memory for comp_iostates.  
  SUBROUTINE final_iostate_comp( id_in )
    INTEGER, INTENT(IN   ) :: id_in       ! index to comp_iostates
#ifdef ESMF_COUPLING
    ! Local declarations
    INTEGER :: id, rc
    TYPE(ESMF_DELayout) :: taskLayout
    LOGICAL :: noneLeft

    CALL check_id  ( id_in, __LINE__ )  ! range check on id
    CALL check_init( id_in, __LINE__ )  ! check initialization

    CALL ESMF_GridGet( comp_iostates( id_in )%grid, delayout=taskLayout, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_GridGet', &
                     __FILE__ ,               &
                     ', line',                &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
    ! I "know" I created this...  
    CALL ESMF_DELayoutDestroy( taskLayout, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_DELayoutDestroy', &
                     __FILE__ ,                       &
                     ', line',                        &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
    CALL ESMF_GridDestroy( comp_iostates(id_in)%grid, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE( msg,* ) 'Error in ESMF_GridDestroy', &
                     __FILE__ ,                   &
                     ', line',                    &
                     __LINE__
      CALL wrf_error_fatal ( msg )
    ENDIF
    comp_iostates(id_in)%in_use = .FALSE.

    noneLeft = .TRUE.
    DO id = 0, MAX_IOSTATES-1
      IF ( comp_iostates(id)%in_use ) THEN
        noneLeft = .FALSE.
      ENDIF
    ENDDO

    IF ( noneLeft ) THEN
      DO id = 0, MAX_IOSTATES-1
        DEALLOCATE( comp_iostates( id )%gcomp,       &
                    comp_iostates( id )%importState, &
                    comp_iostates( id )%exportState, &
                    comp_iostates( id )%clock,       &
                    comp_iostates( id )%grid )
        NULLIFY( comp_iostates( id )%gcomp,       &
                 comp_iostates( id )%importState, &
                 comp_iostates( id )%exportState, &
                 comp_iostates( id )%clock,       &
                 comp_iostates( id )%grid )
      ENDDO
      need_init = .TRUE.
    ENDIF

#endif

  END SUBROUTINE final_iostate_comp



END MODULE module_iostate_comp

