
MODULE module_ext_esmf

  USE ESMF_Mod
  USE module_esmf_extensions

  IMPLICIT NONE

  TYPE grid_ptr
    TYPE(ESMF_Grid), POINTER :: ptr
  END TYPE grid_ptr

  ! TBH:  should package this state into an object...  
  INTEGER, PARAMETER :: int_num_handles = 99
  LOGICAL, DIMENSION(int_num_handles) :: okay_to_write, okay_to_read,       &
                                         opened_for_write, opened_for_read, &
                                         int_handle_in_use
  TYPE(grid_ptr) :: grid(int_num_handles)

  ! convenience...
  CHARACTER (256) :: msg

  INCLUDE 'wrf_io_flags.h'
  INCLUDE 'wrf_status_codes.h'

  CONTAINS

    LOGICAL FUNCTION int_valid_handle( handle )
      IMPLICIT NONE
      INTEGER, INTENT(IN) ::  handle
      int_valid_handle = ( handle .ge. 8 .and. handle .le. int_num_handles ) 
    END FUNCTION int_valid_handle

    SUBROUTINE int_get_fresh_handle( retval )
      INTEGER i, retval

      retval = -1
! dont use first 8 handles
      DO i = 8, int_num_handles
        IF ( .NOT. int_handle_in_use(i) )  THEN
          retval = i
          GOTO 33
        ENDIF
      ENDDO
33    CONTINUE
      IF ( retval < 0 )  THEN
        CALL wrf_error_fatal( "io_esmf.F90: int_get_fresh_handle() out of handles")
      ENDIF
      int_handle_in_use(retval) = .TRUE.
    END SUBROUTINE int_get_fresh_handle

! parse comma separated list of VARIABLE=VALUE strings and return the
! value for the matching variable if such exists, otherwise return
! the empty string
SUBROUTINE get_value ( varname , str , retval )
  IMPLICIT NONE
  CHARACTER*(*) ::    varname
  CHARACTER*(*) ::    str
  CHARACTER*(*) ::    retval

  CHARACTER (128) varstr, tstr
  INTEGER i,j,n,varstrn
  LOGICAL nobreak, nobreakouter

  varstr = TRIM(varname)//"="
  varstrn = len(TRIM(varstr))
  n = len(TRIM(str))
  retval = ""
  i = 1
  nobreakouter = .TRUE.
  DO WHILE ( nobreakouter )
    j = 1
    nobreak = .TRUE.
    tstr = ""
    DO WHILE ( nobreak )
      nobreak = .FALSE.
      IF ( i .LE. n ) THEN
        IF (str(i:i) .NE. ',' ) THEN
           tstr(j:j) = str(i:i)
           nobreak = .TRUE.
        ENDIF
      ENDIF
      j = j + 1
      i = i + 1
    ENDDO
    IF ( i .GT. n ) nobreakouter = .FALSE.
    IF ( varstr(1:varstrn) .EQ. tstr(1:varstrn) ) THEN
      retval(1:) = TRIM(tstr(varstrn+1:))
      nobreakouter = .FALSE.
    ENDIF
  ENDDO
  RETURN
END SUBROUTINE get_value


    !--- ioinit
    SUBROUTINE init_module_ext_esmf
      IMPLICIT NONE
      INTEGER :: i
      DO i = 1, int_num_handles
        NULLIFY( grid( i )%ptr )
      ENDDO
      RETURN
    END SUBROUTINE init_module_ext_esmf


  ! Create the ESMF_Grid associated with index DataHandle.  
  ! TBH:  Note that periodicity is not supported by this interface.  If 
  ! TBH:  periodicity is needed, pass in via SysDepInfo in the call to 
  ! TBH:  ext_esmf_ioinit().  
  ! TBH:  Note that lat/lon coordinates are not supported by this interface 
  ! TBH:  since general curvilinear coordindates (needed for map projections 
  ! TBH:  used by WRF such as polar stereographic, mercator, lambert conformal)
  ! TBH:  are not supported by ESMF as of ESMF 2.1.0.  Once they are supported, 
  ! TBH:  add them via the "sieve" method used in ../io_mcel/.  
  SUBROUTINE create_grid( DataHandle, MemoryOrder, Stagger, &
                          DomainStart, DomainEnd,           &
                          MemoryStart, MemoryEnd,           &
                          PatchStart, PatchEnd )
    INTEGER,       INTENT(IN   ) :: DataHandle
    CHARACTER*(*), INTENT(IN   ) :: MemoryOrder
    CHARACTER*(*), INTENT(IN   ) :: Stagger
    INTEGER,       INTENT(IN   ) :: DomainStart(:), DomainEnd(:)
    INTEGER,       INTENT(IN   ) :: MemoryStart(:), MemoryEnd(:)
    INTEGER,       INTENT(IN   ) :: PatchStart(:),  PatchEnd(:)
    ! Local declarations
    INTEGER :: numprocs     ! total number of tasks
    INTEGER, ALLOCATABLE :: ipatchStarts(:), jpatchStarts(:)
    INTEGER :: numprocsX    ! number of tasks in "i" dimension
    INTEGER :: numprocsY    ! number of tasks in "j" dimension
    INTEGER, ALLOCATABLE :: permuteTasks(:)
    INTEGER :: globalXcount ! non-staggered domain count in "i" dimension
    INTEGER :: globalYcount ! non-staggered domain count in "j" dimension
    INTEGER :: myXstart     ! task-local start in "i" dimension
    INTEGER :: myYstart     ! task-local start in "j" dimension
    INTEGER :: myXend       ! non-staggered task-local end in "i" dimension
    INTEGER :: myYend       ! non-staggered task-local end in "j" dimension
    INTEGER, ALLOCATABLE :: allXStart(:)
    INTEGER, ALLOCATABLE :: allXCount(:)
    INTEGER, ALLOCATABLE :: dimXCount(:)
    INTEGER, ALLOCATABLE :: allYStart(:)
    INTEGER, ALLOCATABLE :: allYCount(:)
    INTEGER, ALLOCATABLE :: dimYCount(:)
    REAL(ESMF_KIND_R8), ALLOCATABLE :: coordX(:)
    REAL(ESMF_KIND_R8), ALLOCATABLE :: coordY(:)
    INTEGER, ALLOCATABLE :: cellCounts(:,:)
    INTEGER, ALLOCATABLE :: globalStarts(:,:)
    INTEGER :: rc
    INTEGER :: myXcount      ! task-local count in "i" dimension
    INTEGER :: myYcount      ! task-local count in "j" dimension
    INTEGER :: globalCellCounts(2)
    INTEGER :: numprocsXY(2)
    INTEGER :: myPE, i, j, pe, is, ie, js, je, is_min, js_min, ie_max, je_max
    INTEGER :: ips, ipe, jps, jpe, ids, ide, jds, jde
    TYPE(ESMF_VM) :: vm
    TYPE(ESMF_DELayout) :: taskLayout
    CHARACTER (32) :: gridname

    IF ( .NOT. ASSOCIATED( grid( DataHandle )%ptr ) ) THEN
      ALLOCATE( grid( DataHandle )%ptr )
      ! First, determine number of tasks and number of tasks in each decomposed 
      ! dimension (ESMF 2.1.0 is restricted to simple task layouts)
      ! get current ESMF virtual machine and inquire...  
      CALL ESMF_VMGetCurrent(vm, rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_VMGetCurrent', &
                       __FILE__ ,                    &
                       ', line',                     &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      ! TBH:  Note (PET==MPI process) assumption here.  This is OK in ESMF 
      ! TBH:  2.1.0 but may change in a future ESMF release.  If so, we will 
      ! TBH:  need another way to do this.  May want to grab mpiCommunicator 
      ! TBH:  instead and ask it directly for number of MPI tasks.  Of course, 
      ! TBH:  what if this is a serial run?  
      CALL ESMF_VMGet(vm, petCount=numprocs, localPet=myPE, rc=rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_VMGet', &
                       __FILE__ ,             &
                       ', line',              &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      ALLOCATE( ipatchStarts(0:numprocs-1), jpatchStarts(0:numprocs-1) )
      CALL GatherIntegerScalars_ESMF(PatchStart(1), myPE, numprocs, ipatchStarts)
      CALL GatherIntegerScalars_ESMF(PatchStart(2), myPE, numprocs, jpatchStarts)
      numprocsX = 0
      numprocsY = 0
      DO pe = 0, numprocs-1
        IF ( PatchStart(1) == ipatchStarts(pe) ) THEN
          numprocsX = numprocsX + 1
        ENDIF
        IF ( PatchStart(2) == jpatchStarts(pe) ) THEN
          numprocsY = numprocsY + 1
        ENDIF
      ENDDO
      DEALLOCATE( ipatchStarts, jpatchStarts )
      ! sanity check
      IF ( numprocs /= numprocsX*numprocsY ) THEN
        CALL wrf_error_fatal ( 'ASSERTION FAILED:  numprocs /= numprocsX*numprocsY' )
      ENDIF
      ! Next, create ESMF_DELayout
      numprocsXY = (/ numprocsX, numprocsY /)
      ! transpose tasks to match RSL
      ! TBH:  1-to-1 DE to PET mapping is assumed below...  
      ALLOCATE( permuteTasks(0:numprocs-1) )
      pe = 0
      DO j = 0, numprocsY-1
        DO i = 0, numprocsX-1
        permuteTasks(pe) = (i*numprocsY) + j
        pe = pe + 1
        ENDDO
      ENDDO
      WRITE( msg,* ) 'DEBUG:  permuteTasks = ',permuteTasks
      CALL wrf_debug ( 5 , msg )
      taskLayout = ESMF_DELayoutCreate( vm, numprocsXY, dePetList=permuteTasks, rc=rc ) 
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_DELayoutCreate', &
                       __FILE__ ,                      &
                       ', line',                       &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      DEALLOCATE( permuteTasks )
      ! compute indices for non-staggered grids cause thats the way ESMF likes 
      ! it
      ! the [ij][dp][se] bits are for convenience...  
      ids = DomainStart(1); ide = DomainEnd(1); 
      jds = DomainStart(2); jde = DomainEnd(2); 
      ips = PatchStart(1);  ipe = PatchEnd(1); 
      jps = PatchStart(2);  jpe = PatchEnd(2); 
      globalXcount = (ide-1) - ids + 1
      globalYcount = (jde-1) - jds + 1
      ! task-local numbers of points in patch for non-staggered arrays
      myXstart = ips
      myYstart = jps
      myXend = min(ipe, ide-1)
      myYend = min(jpe, jde-1)
      myXcount = myXend - myXstart + 1
      myYcount = myYend - myYstart + 1
      WRITE( msg,* ) 'DEBUG:  WRF non-staggered     ips = ', ips
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF non-staggered     ipe = ', min(ipe, ide-1)
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF non-staggered i count = ', myXCount
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF non-staggered     jps = ', jps
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF non-staggered     jpe = ', min(jpe, jde-1)
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF non-staggered j count = ', myYCount
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF     staggered     ips = ', ips
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF     staggered     ipe = ', ipe
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF     staggered i count = ', ipe-ips+1
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF     staggered     jps = ', jps
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF     staggered     jpe = ', jpe
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  WRF     staggered j count = ', jpe-jps+1
      CALL wrf_debug ( 5 , msg )
      ! gather task-local information on all tasks since 
      ! ESMF_GridDistribute[Block] interface require global knowledge to set up 
      ! decompositions (@#$%)
      ALLOCATE( allXStart(0:numprocs-1),  allXCount(0:numprocs-1),  &
                allYStart(0:numprocs-1),  allYCount(0:numprocs-1),  &
                dimXCount(0:numprocsX-1), dimYCount(0:numprocsY-1), &
                coordX(globalXcount+1),   coordY(globalYcount+1) )
      CALL GatherIntegerScalars_ESMF(myXcount, myPE, numprocs, allXCount)
      CALL GatherIntegerScalars_ESMF(myXstart, myPE, numprocs, allXStart)
      CALL GatherIntegerScalars_ESMF(myYcount, myPE, numprocs, allYCount)
      CALL GatherIntegerScalars_ESMF(myYstart, myPE, numprocs, allYStart)

      ! HACK:  ESMF does not yet support mercator, polar-stereographic, or 
      ! HACK:  lambert-conformal projections.  Therefore, we're using fake 
      ! HACK:  coordinates here.  This means that WRF will either have to 
      ! HACK:  couple to models that run on the same coorindate such that 
      ! HACK:  grid points are co-located or something else will have to 
      ! HACK:  perform the inter-grid interpolation computations.  Replace 
      ! HACK:  this once ESMF is upgraded to support the above map 
      ! HACK:  projections (via general curvilinear coordinates).  
      CALL wrf_message( 'WARNING:  Using artificial coordinates for ESMF coupling.' )
      CALL wrf_message( 'WARNING:  ESMF coupling interpolation will be incorrect' )
      CALL wrf_message( 'WARNING:  unless grid points in the coupled components' )
      CALL wrf_message( 'WARNING:  are co-located.  This limitation will be removed' )
      CALL wrf_message( 'WARNING:  once ESMF coupling supports common map projections.' )
      ! Note that ESMF defines coordinates at *vertices*
      coordX(1) = 0.0
      DO i = 2, globalXcount+1
        coordX(i) = coordX(i-1) + 1.0
      ENDDO
      coordY(1) = 0.0
      DO i = 2, globalYcount+1
        coordY(i) = coordY(i-1) + 1.0
      ENDDO
      ! Create an ESMF_Grid
      ! For now we create only a 2D grid suitable for simple coupling of 2D 
      ! surface fields.  Later, create and subset one or more 3D grids.  
!TBH $$$:  NOTE that we'll have to use ESMF_GRID_HORZ_STAGGER_E_?? for NMM.  
!TBH $$$:  E-grid is not yet supported by ESMF.  Eventually pass staggering 
!TBH $$$:  info into this routine.  For now, hard-code it for WRF-ARW.  
      WRITE ( gridname,'(a,i0)' ) 'WRF_grid_', DataHandle
      grid( DataHandle )%ptr = ESMF_GridCreateHorzXY(                     &
                                 coord1=coordX, coord2=coordY,            &
                                 horzstagger=ESMF_GRID_HORZ_STAGGER_C_SW, &
! use this for 3D Grids once it is stable
!                            coordorder=ESMF_COORD_ORDER_XZY,         &
                             name=TRIM(gridname), rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_GridCreateHorzXY', &
                       __FILE__ ,                        &
                       ', line',                         &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      ! distribute the ESMF_Grid
      ! ignore repeated values
      is_min = MINVAL(allXStart)
      js_min = MINVAL(allYStart)
      i = 0
      j = 0
      WRITE( msg,* ) 'DEBUG:  is_min = ',is_min,'  allXStart = ',allXStart
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  js_min = ',js_min,'  allYStart = ',allYStart
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  allXCount = ',allXCount
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  allYCount = ',allYCount
      CALL wrf_debug ( 5 , msg )
      DO pe = 0, numprocs-1
        IF (allXStart(pe) == is_min) THEN
          IF (j >= numprocsY) THEN
            WRITE( msg,* ) 'ASSERTION FAILED in ESMF_GridCreateHorzXY', &
                           __FILE__ ,                                   &
                           ', line',                                    &
                           __LINE__
            CALL wrf_error_fatal ( msg )
          ENDIF
      WRITE( msg,* ) 'DEBUG:  dimYCount(',j,') = allYCount(',pe,')'
      CALL wrf_debug ( 5 , msg )
          dimYCount(j) = allYCount(pe)
          j = j + 1
        ENDIF
        IF (allYStart(pe) == js_min) THEN
          IF (i >= numprocsX) THEN
            WRITE( msg,* ) 'ASSERTION FAILED in ESMF_GridCreateHorzXY', &
                           __FILE__ ,                                   &
                           ', line',                                    &
                           __LINE__
            CALL wrf_error_fatal ( msg )
          ENDIF
      WRITE( msg,* ) 'DEBUG:  dimXCount(',i,') = allXCount(',pe,')'
      CALL wrf_debug ( 5 , msg )
          dimXCount(i) = allXCount(pe)
          i = i + 1
        ENDIF
      ENDDO
      WRITE( msg,* ) 'DEBUG:  i = ',i,'  dimXCount = ',dimXCount
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  j = ',j,'  dimYCount = ',dimYCount
      CALL wrf_debug ( 5 , msg )
      CALL ESMF_GridDistribute( grid( DataHandle )%ptr,    &
                                delayout=taskLayout,       &
                                countsPerDEDim1=dimXCount, &
                                countsPerDEDim2=dimYCount, &
                                rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_GridDistribute', &
                       __FILE__ ,                      &
                       ', line',                       &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      DEALLOCATE( allXStart, allXCount, allYStart, allYCount, &
                  dimXCount, dimYCount, coordX, coordY )

      ! Print out the ESMF decomposition info for debug comparison with WRF 
      ! decomposition info.  
      ALLOCATE( cellCounts(0:numprocs-1,2), globalStarts(0:numprocs-1,2) )

      CALL ESMF_GridGet( grid( DataHandle )%ptr,                 &
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
      WRITE( msg,* ) 'DEBUG:  ESMF task-id = ',myPE
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF non-staggered     ips = ',1+globalStarts(myPE,1)
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF non-staggered     ipe = ',1+globalStarts(myPE,1) + cellCounts(myPE,1) - 1
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF non-staggered i count = ',  cellCounts(myPE,1)
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF non-staggered     jps = ',1+globalStarts(myPE,2)
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF non-staggered     jpe = ',1+globalStarts(myPE,2) + cellCounts(myPE,2) - 1
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF non-staggered j count = ',  cellCounts(myPE,2)
      CALL wrf_debug ( 5 , msg )
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

      ! extract information about staggered grids for debugging
      CALL ESMF_GridGet( grid( DataHandle )%ptr,                 &
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
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered     ips = ',1+globalStarts(myPE,1)
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered     ipe = ',1+globalStarts(myPE,1) + cellCounts(myPE,1) - 1
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered i count = ',  cellCounts(myPE,1)
      CALL wrf_debug ( 5 , msg )
      CALL ESMF_GridGet( grid( DataHandle )%ptr,                 &
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
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered     jps = ',1+globalStarts(myPE,2)
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered     jpe = ',1+globalStarts(myPE,2) + cellCounts(myPE,2) - 1
      CALL wrf_debug ( 5 , msg )
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered j count = ',  cellCounts(myPE,2)
      CALL wrf_debug ( 5 , msg )

      DEALLOCATE( cellCounts, globalStarts )

    ENDIF

  END SUBROUTINE create_grid



  ! Destroy the ESMF_Grid associated with index DataHandle.  
  ! grid( DataHandle )%ptr is DEALLOCATED (NULLIFIED)
  SUBROUTINE destroy_grid( DataHandle )
    INTEGER, INTENT(IN   ) :: DataHandle
    ! Local declarations
    INTEGER :: id, rc
    TYPE(ESMF_DELayout) :: taskLayout
    LOGICAL :: noneLeft

    IF ( ASSOCIATED( grid( DataHandle )%ptr ) ) THEN
      CALL ESMF_GridGet( grid( DataHandle )%ptr, delayout=taskLayout, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_GridGet', &
                       __FILE__ ,               &
                       ', line',                &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      ! I "know" I created this...  (not really, but ESMF cannot tell me!)
      CALL ESMF_DELayoutDestroy( taskLayout, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_DELayoutDestroy', &
                       __FILE__ ,                       &
                       ', line',                        &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      CALL ESMF_GridDestroy( grid( DataHandle )%ptr, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_GridDestroy', &
                       __FILE__ ,                   &
                       ', line',                    &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      DEALLOCATE( grid( DataHandle )%ptr )
      NULLIFY( grid( DataHandle )%ptr )
    ENDIF

  END SUBROUTINE destroy_grid


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


END MODULE module_ext_esmf


!$$$here...  use generic explicit interfaces?  if not, why not?  
 !$$$ remove duplication!
 SUBROUTINE ext_esmf_extract_data_real( data_esmf_real, Field,      &
                                      ips, ipe, jps, jpe, kps, kpe, &
                                      ims, ime, jms, jme, kms, kme )
   USE module_ext_esmf
   INTEGER,            INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
   INTEGER,            INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
   REAL(ESMF_KIND_R4), INTENT(IN   ) :: data_esmf_real( ips:ipe, jps:jpe )
   REAL,               INTENT(  OUT) :: Field( ims:ime, jms:jme, kms:kme )
   Field( ips:ipe, jps:jpe, kms ) = data_esmf_real( ips:ipe, jps:jpe )
 END SUBROUTINE ext_esmf_extract_data_real


 !$$$ remove duplication!
 SUBROUTINE ext_esmf_extract_data_int( esmf_data_int, Field,         &
                                       ips, ipe, jps, jpe, kps, kpe, &
                                       ims, ime, jms, jme, kms, kme )
   USE module_ext_esmf
   INTEGER,               INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
   INTEGER,               INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
   INTEGER(ESMF_KIND_I4), INTENT(IN   ) :: esmf_data_int( ips:ipe, jps:jpe )
   INTEGER,               INTENT(  OUT) :: Field( ims:ime, jms:jme, kms:kme )
   Field( ips:ipe, jps:jpe, kms ) = esmf_data_int( ips:ipe, jps:jpe )
 END SUBROUTINE ext_esmf_extract_data_int


 !$$$ remove duplication!
 SUBROUTINE ext_esmf_insert_data_real( Field, data_esmf_real,        &
                                       ips, ipe, jps, jpe, kps, kpe, &
                                       ims, ime, jms, jme, kms, kme )
   USE module_ext_esmf
   INTEGER,               INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
   INTEGER,               INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
   REAL,                  INTENT(IN   ) :: Field( ims:ime, jms:jme, kms:kme )
   REAL(ESMF_KIND_R4),    INTENT(  OUT) :: data_esmf_real( ips:ipe, jps:jpe )
   data_esmf_real( ips:ipe, jps:jpe ) = Field( ips:ipe, jps:jpe, kms )
 END SUBROUTINE ext_esmf_insert_data_real


 !$$$ remove duplication!
 SUBROUTINE ext_esmf_insert_data_int( Field, esmf_data_int,         &
                                      ips, ipe, jps, jpe, kps, kpe, &
                                      ims, ime, jms, jme, kms, kme )
   USE module_ext_esmf
   INTEGER,               INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
   INTEGER,               INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
   INTEGER,               INTENT(IN   ) :: Field( ims:ime, jms:jme, kms:kme )
   INTEGER(ESMF_KIND_I4), INTENT(  OUT) :: esmf_data_int( ips:ipe, jps:jpe )
   esmf_data_int( ips:ipe, jps:jpe ) = Field( ips:ipe, jps:jpe, kms )
 END SUBROUTINE ext_esmf_insert_data_int


!--------------

SUBROUTINE ext_esmf_ioinit( SysDepInfo, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  INTEGER Status
  CALL init_module_ext_esmf
  Status = 0 
END SUBROUTINE ext_esmf_ioinit

!--- open_for_read 
SUBROUTINE ext_esmf_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                    DataHandle , Status )
  USE module_ext_esmf
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_error_fatal('ext_esmf_open_for_read not supported yet')
  RETURN  
END SUBROUTINE ext_esmf_open_for_read


!--- inquire_opened
SUBROUTINE ext_esmf_inquire_opened ( DataHandle, FileName , FileStatus, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status

  Status = 0

  FileStatus = WRF_FILE_NOT_OPENED
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF      ( int_handle_in_use( DataHandle ) .AND. opened_for_read ( DataHandle ) ) THEN
      IF ( okay_to_read ( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_FOR_READ
      ELSE
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
      ENDIF
    ELSE IF ( int_handle_in_use( DataHandle ) .AND. opened_for_write ( DataHandle ) ) THEN
      IF ( okay_to_write ( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_FOR_WRITE
      ELSE
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
      ENDIF
    ENDIF
  ENDIF
  Status = 0
  
  RETURN
END SUBROUTINE ext_esmf_inquire_opened

!--- inquire_filename
SUBROUTINE ext_esmf_inquire_filename ( DataHandle, FileName , FileStatus, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER *80   SysDepInfo
  Status = 0
  FileStatus = WRF_FILE_NOT_OPENED
  IF ( int_valid_handle( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      IF ( opened_for_read ( DataHandle ) ) THEN
        IF ( okay_to_read( DataHandle ) ) THEN
           FileStatus = WRF_FILE_OPENED_FOR_READ
        ELSE
           FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
        ENDIF
      ELSE IF ( opened_for_write( DataHandle ) ) THEN
        IF ( okay_to_write( DataHandle ) ) THEN
           FileStatus = WRF_FILE_OPENED_FOR_WRITE
        ELSE
           FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
        ENDIF
      ELSE
        FileStatus = WRF_FILE_NOT_OPENED
      ENDIF
    ENDIF
  ENDIF
  ! need to cache file names before this routine will work properly
  CALL wrf_error_fatal( "ext_esmf_inquire_filename() not supported yet")
  Status = 0
END SUBROUTINE ext_esmf_inquire_filename

!--- sync
SUBROUTINE ext_esmf_iosync ( DataHandle, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  Status = 0
  RETURN
END SUBROUTINE ext_esmf_iosync

!--- close
SUBROUTINE ext_esmf_ioclose ( DataHandle, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER DataHandle, Status
  IF ( int_valid_handle (DataHandle) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL destroy_grid( DataHandle )
    ENDIF
  ENDIF
  Status = 0
  RETURN
END SUBROUTINE ext_esmf_ioclose

!--- ioexit
SUBROUTINE ext_esmf_ioexit( Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER :: i
  DO i = 1, int_num_handles
    CALL destroy_grid( i )
  ENDDO
!$$$ clean up import and export states here!!  
  CALL wrf_debug ( 5 , &
    'ext_esmf_ioexit:  DEBUG:  add code to clean up lingering ESMF objects here' )
  Status = 0
  RETURN  
END SUBROUTINE ext_esmf_ioexit

!--- get_next_time
SUBROUTINE ext_esmf_get_next_time ( DataHandle, DateStr, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("io_esmf.F90: ext_esmf_get_next_time: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("io_esmf.F90: ext_esmf_get_next_time: DataHandle not opened" )
  ENDIF
  CALL wrf_error_fatal( "ext_esmf_get_next_time() not supported yet")
  RETURN
END SUBROUTINE ext_esmf_get_next_time

!--- set_time
SUBROUTINE ext_esmf_set_time ( DataHandle, DateStr, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_error_fatal( "ext_esmf_set_time() not supported yet")
  RETURN
END SUBROUTINE ext_esmf_set_time

!--- get_var_info
SUBROUTINE ext_esmf_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                                   DomainStart , DomainEnd , WrfType, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: VarName
  integer               ,intent(out)    :: NDim
  character*(*)         ,intent(out)    :: MemoryOrder
  character*(*)         ,intent(out)    :: Stagger
  integer ,dimension(*) ,intent(out)    :: DomainStart, DomainEnd
  integer               ,intent(out)    :: WrfType
  integer               ,intent(out)    :: Status

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("io_esmf.F90: ext_esmf_get_var_info: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("io_esmf.F90: ext_esmf_get_var_info: DataHandle not opened" )
  ENDIF
  CALL wrf_error_fatal( "ext_esmf_get_var_info() not supported yet")
  RETURN
END SUBROUTINE ext_esmf_get_var_info

!--- get_next_var
SUBROUTINE ext_esmf_get_next_var ( DataHandle, VarName, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_esmf/io_esmf.F90: ext_esmf_get_next_var: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_esmf/io_esmf.F90: ext_esmf_get_next_var: DataHandle not opened" )
  ENDIF
  CALL wrf_error_fatal( "ext_esmf_get_next_var() not supported yet")
  RETURN
END SUBROUTINE ext_esmf_get_next_var

!--- get_dom_ti_real
SUBROUTINE ext_esmf_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Outcount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_error_fatal( "ext_esmf_get_dom_ti_real() not supported yet")
  RETURN
END SUBROUTINE ext_esmf_get_dom_ti_real 

!--- put_dom_ti_real
SUBROUTINE ext_esmf_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_error_fatal( "ext_esmf_put_dom_ti_real() not supported yet")
  RETURN
END SUBROUTINE ext_esmf_put_dom_ti_real 

!--- get_dom_ti_double
SUBROUTINE ext_esmf_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_ti_double not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_ti_double 

!--- put_dom_ti_double
SUBROUTINE ext_esmf_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_ti_double not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_ti_double 

!--- get_dom_ti_integer
SUBROUTINE ext_esmf_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_ti_integer not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_ti_integer 

!--- put_dom_ti_integer
SUBROUTINE ext_esmf_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  INTEGER ,       INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_ti_integer not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_ti_integer 

!--- get_dom_ti_logical
SUBROUTINE ext_esmf_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_ti_logical not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_ti_logical 

!--- put_dom_ti_logical
SUBROUTINE ext_esmf_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_ti_logical not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_ti_logical 

!--- get_dom_ti_char
SUBROUTINE ext_esmf_get_dom_ti_char ( DataHandle,Element,   Data,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_ti_char not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_ti_char 

!--- put_dom_ti_char
SUBROUTINE ext_esmf_put_dom_ti_char ( DataHandle, Element,  Data,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_ti_char not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_ti_char 

!--- get_dom_td_real
SUBROUTINE ext_esmf_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_td_real not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_real 

!--- put_dom_td_real
SUBROUTINE ext_esmf_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_td_real not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_real 

!--- get_dom_td_double
SUBROUTINE ext_esmf_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_td_double not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_double 

!--- put_dom_td_double
SUBROUTINE ext_esmf_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_td_double not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_double 

!--- get_dom_td_integer
SUBROUTINE ext_esmf_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_td_integer not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_integer 

!--- put_dom_td_integer
SUBROUTINE ext_esmf_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_td_integer not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_integer 

!--- get_dom_td_logical
SUBROUTINE ext_esmf_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_td_logical not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_logical 

!--- put_dom_td_logical
SUBROUTINE ext_esmf_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_td_logical not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_logical 

!--- get_dom_td_char
SUBROUTINE ext_esmf_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_dom_td_char not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_char 

!--- put_dom_td_char
SUBROUTINE ext_esmf_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_dom_td_char not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_char 

!--- get_var_ti_real
SUBROUTINE ext_esmf_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_ti_real not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_real 

!--- put_var_ti_real
SUBROUTINE ext_esmf_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_ti_real not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_real 

!--- get_var_ti_double
SUBROUTINE ext_esmf_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_ti_double not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_double 

!--- put_var_ti_double
SUBROUTINE ext_esmf_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_ti_double not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_double 

!--- get_var_ti_integer
SUBROUTINE ext_esmf_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_ti_integer not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_integer 

!--- put_var_ti_integer
SUBROUTINE ext_esmf_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_ti_integer not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_integer 

!--- get_var_ti_logical
SUBROUTINE ext_esmf_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_ti_logical not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_logical 

!--- put_var_ti_logical
SUBROUTINE ext_esmf_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_ti_logical not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_logical 

!--- get_var_ti_char
SUBROUTINE ext_esmf_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER locDataHandle, code
  CHARACTER*132 locElement, locVarName
  Status = 0
  CALL wrf_message('ext_esmf_get_var_ti_char not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_char 

!--- put_var_ti_char
SUBROUTINE ext_esmf_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
  INTEGER                 :: Count
  Status = 0
  CALL wrf_message('ext_esmf_put_var_ti_char not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_char 

!--- get_var_td_real
SUBROUTINE ext_esmf_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_td_real not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_td_real 

!--- put_var_td_real
SUBROUTINE ext_esmf_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_td_real not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_td_real 

!--- get_var_td_double
SUBROUTINE ext_esmf_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_td_double not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_td_double 

!--- put_var_td_double
SUBROUTINE ext_esmf_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_td_double not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_td_double 

!--- get_var_td_integer
SUBROUTINE ext_esmf_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_td_integer not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_td_integer 

!--- put_var_td_integer
SUBROUTINE ext_esmf_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_td_integer not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_td_integer 

!--- get_var_td_logical
SUBROUTINE ext_esmf_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_td_logical not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_td_logical 

!--- put_var_td_logical
SUBROUTINE ext_esmf_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_td_logical not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_td_logical 

!--- get_var_td_char
SUBROUTINE ext_esmf_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_get_var_td_char not supported yet')
  RETURN
END SUBROUTINE ext_esmf_get_var_td_char 

!--- put_var_td_char
SUBROUTINE ext_esmf_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_esmf_put_var_td_char not supported yet')
  RETURN
END SUBROUTINE ext_esmf_put_var_td_char 


