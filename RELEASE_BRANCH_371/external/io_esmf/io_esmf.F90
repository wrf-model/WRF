
MODULE module_ext_esmf

! 5.2.0r  USE ESMF_Mod
  USE ESMF
  USE module_esmf_extensions

  IMPLICIT NONE

  TYPE grid_ptr
    TYPE(ESMF_Grid), POINTER :: ptr
    ! use these for error-checking for now...
    INTEGER :: ide_save
    INTEGER :: jde_save
    INTEGER :: kde_save
    LOGICAL :: in_use
  END TYPE grid_ptr

!TODO:  encapsulate this state into a class...  
  INTEGER, PARAMETER :: int_num_handles = 99
  LOGICAL, DIMENSION(int_num_handles) :: okay_to_write, okay_to_read,       &
                                         opened_for_write, opened_for_read, &
                                         int_handle_in_use
  TYPE(grid_ptr) :: grid(int_num_handles)

  ! convenience...
  CHARACTER (256) :: msg

#include "wrf_io_flags.h"
#include "wrf_status_codes.h"

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
        WRITE( msg,* ) 'init_module_ext_esmf:  calling ioesmf_nullify_grid(',i,')'
        CALL wrf_debug ( 5, TRIM(msg) )
        CALL ioesmf_nullify_grid( i )
      ENDDO
      RETURN
    END SUBROUTINE init_module_ext_esmf


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
    CALL ESMF_VMGetCurrent(vm, rc=rc)
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
! 5.2.0r    CALL ESMF_VMAllReduce(vm, allSnd, allRcv, numprocs, ESMF_SUM, rc=rc )
    CALL ESMF_VMAllReduce(vm, allSnd, allRcv, numprocs, ESMF_REDUCE_SUM, rc=rc )
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



  ! Indexes for non-staggered variables come in at one-less than
  ! domain dimensions, but io_esmf is currently hacked to use full 
  ! domain spec, so adjust if not staggered.  
  !TODO:  remove this hackery once ESMF can support staggered 
  !TODO:  grids in regional models
  SUBROUTINE ioesmf_endfullhack( numdims, DomainEnd, PatchEnd, Stagger, &
                                 DomainEndFull, PatchEndFull )
    IMPLICIT NONE
    INTEGER,       INTENT(IN   ) :: numdims
    INTEGER,       INTENT(IN   ) :: DomainEnd(numdims)
    INTEGER,       INTENT(IN   ) :: PatchEnd(numdims)
    CHARACTER*(*), INTENT(IN   ) :: Stagger
    INTEGER,       INTENT(  OUT) :: DomainEndFull(numdims)
    INTEGER,       INTENT(  OUT) :: PatchEndFull(numdims)
    LOGICAL, EXTERNAL :: has_char
    DomainEndFull(1:numdims) = DomainEnd(1:numdims)
    IF ( .NOT. has_char( Stagger, 'x' ) ) DomainEndFull(1) = DomainEndFull(1) + 1
    IF ( .NOT. has_char( Stagger, 'y' ) ) DomainEndFull(2) = DomainEndFull(2) + 1
    PatchEndFull(1:numdims) = PatchEnd(1:numdims)
    IF ( .NOT. has_char( Stagger, 'x' ) ) THEN
      IF ( DomainEnd(1) == PatchEnd(1) ) PatchEndFull(1) = DomainEndFull(1)
    ENDIF
    IF ( .NOT. has_char( Stagger, 'y' ) ) THEN
      IF ( DomainEnd(2) == PatchEnd(2) ) PatchEndFull(2) = DomainEndFull(2)
    ENDIF
  END SUBROUTINE ioesmf_endfullhack


  ! Create the ESMF_Grid associated with index DataHandle.  
  !TODO:  Note that periodicity is not supported by this interface.  If 
  !TODO:  periodicity is needed, pass in via SysDepInfo in the call to 
  !TODO:  ext_esmf_ioinit().  
  !TODO:  Note that lat/lon coordinates are not supported by this interface 
  !TODO:  since general curvilinear coordindates (needed for map projections 
  !TODO:  used by WRF such as polar stereographic, mercator, lambert conformal)
  !TODO:  are not supported by ESMF as of ESMF 2.1.1.  Once they are supported, 
  !TODO:  add them via the "sieve" method used in ../io_mcel/.  
  SUBROUTINE ioesmf_create_grid( DataHandle, numdims,    &
                                 MemoryOrder, Stagger,   &
                                 DomainStart, DomainEnd, &
                                 MemoryStart, MemoryEnd, &
                                 PatchStart, PatchEnd )
    USE module_ext_esmf
    IMPLICIT NONE
    INTEGER,       INTENT(IN   ) :: DataHandle
    INTEGER,       INTENT(IN   ) :: numdims
    CHARACTER*(*), INTENT(IN   ) :: MemoryOrder            ! not used yet
    CHARACTER*(*), INTENT(IN   ) :: Stagger
    INTEGER,       INTENT(IN   ) :: DomainStart(numdims), DomainEnd(numdims)
    INTEGER,       INTENT(IN   ) :: MemoryStart(numdims), MemoryEnd(numdims)
    INTEGER,       INTENT(IN   ) :: PatchStart(numdims),  PatchEnd(numdims)
    INTEGER :: DomainEndFull(numdims)
    INTEGER :: PatchEndFull(numdims)

    WRITE( msg,* ) 'DEBUG ioesmf_create_grid:  begin, DataHandle = ', DataHandle
    CALL wrf_debug ( 5, TRIM(msg) )
    ! For now, blindly create a new grid if it does not already exist for 
    ! this DataHandle
!TODO:  Note that this approach will result in duplicate ESMF_Grids when 
!TODO:  io_esmf is used for input and output.  The first ESMF_Grid will 
!TODO:  be associated with the input handle and the second will be associated 
!TODO:  with the output handle.  Fix this if ESMF_Grids are expensive.  
    IF ( .NOT. grid( DataHandle )%in_use ) THEN
      IF ( ASSOCIATED( grid( DataHandle )%ptr ) ) THEN
        CALL wrf_error_fatal ( 'ASSERTION ERROR:  grid(',DataHandle,') should be NULL' )
      ENDIF
      IF ( numdims /= 2 ) THEN
        CALL wrf_error_fatal ( 'ERROR:  only 2D arrays supported so far with io_esmf' )
      ELSE
        WRITE( msg,* ) 'DEBUG ioesmf_create_grid:  creating grid(',DataHandle,')%ptr'
        CALL wrf_debug ( 5, TRIM(msg) )
        ALLOCATE( grid( DataHandle )%ptr )
        grid( DataHandle )%in_use = .TRUE.
        ! The non-staggered variables come in at one-less than
        ! domain dimensions, but io_esmf is currently hacked to use full 
        ! domain spec, so adjust if not staggered.  
        !TODO:  remove this hackery once ESMF can support staggered 
        !TODO:  grids in regional models
        CALL ioesmf_endfullhack( numdims, DomainEnd, PatchEnd, Stagger, &
                                 DomainEndFull, PatchEndFull )
!TODO:  at the moment this is hard-coded for 2D arrays
!TODO:  use MemoryOrder to set these properly!
!TODO:  also, set these once only
!TODO:  maybe even rip this out since it depends on a hack in input_wrf.F ...
        grid( DataHandle )%ide_save = DomainEndFull(1)
        grid( DataHandle )%jde_save = DomainEndFull(2)
        grid( DataHandle )%kde_save = 1
        WRITE( msg,* ) 'DEBUG ioesmf_create_grid:  DomainEndFull = ', DomainEndFull
        CALL wrf_debug ( 5, TRIM(msg) )
        WRITE( msg,* ) 'DEBUG ioesmf_create_grid:  PatchEndFull = ', PatchEndFull
        CALL wrf_debug ( 5, TRIM(msg) )
        CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid:  Calling ioesmf_create_grid_int()' )
        CALL ioesmf_create_grid_int( grid( DataHandle )%ptr,     &
                              numdims,                    &
                              DomainStart, DomainEndFull, &
!                              DomainStart, DomainEnd, &
                              MemoryStart, MemoryEnd,     &
!                              PatchStart, PatchEndFull )
                              PatchStart, PatchEnd )
        CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid:  back from ioesmf_create_grid_int()' )
        WRITE( msg,* ) 'DEBUG ioesmf_create_grid:  done creating grid(',DataHandle,')%ptr'
        CALL wrf_debug ( 5, TRIM(msg) )
      ENDIF
    ENDIF
    WRITE( msg,* ) 'DEBUG ioesmf_create_grid:  end'
    CALL wrf_debug ( 5, TRIM(msg) )

  END SUBROUTINE ioesmf_create_grid



  ! Create an ESMF_Grid that matches a WRF decomposition.  
  !TODO:  Note that periodicity is not supported by this interface.  If 
  !TODO:  periodicity is needed, pass in via SysDepInfo in the call to 
  !TODO:  ext_esmf_ioinit().  
  !TODO:  Note that lat/lon coordinates are not supported by this interface 
  !TODO:  since general curvilinear coordindates (needed for map projections 
  !TODO:  used by WRF such as polar stereographic, mercator, lambert conformal)
  !TODO:  are not supported by ESMF as of ESMF 2.1.1.  Once they are supported, 
  !TODO:  add them via the "sieve" method used in ../io_mcel/.  
  !TODO:  Note that DomainEnd and PatchEnd must currently include "extra" 
  !TODO:  points for non-periodic staggered arrays.  It may be possible to 
  !TODO:  remove this hackery once ESMF can support staggered 
  !TODO:  grids in regional models.  
  SUBROUTINE ioesmf_create_grid_int( esmfgrid, numdims,      &
                              DomainStart, DomainEnd, &
                              MemoryStart, MemoryEnd, &
                              PatchStart, PatchEnd )
    USE module_ext_esmf
    IMPLICIT NONE
    TYPE(ESMF_Grid), INTENT(INOUT) :: esmfgrid
    INTEGER,         INTENT(IN   ) :: numdims
    INTEGER,         INTENT(IN   ) :: DomainStart(numdims), DomainEnd(numdims)
    INTEGER,         INTENT(IN   ) :: MemoryStart(numdims), MemoryEnd(numdims)
    INTEGER,         INTENT(IN   ) :: PatchStart(numdims),  PatchEnd(numdims)
    ! Local declarations
    INTEGER :: numprocs     ! total number of tasks
    INTEGER, ALLOCATABLE :: ipatchStarts(:), jpatchStarts(:)
    INTEGER :: numprocsX    ! number of tasks in "i" dimension
    INTEGER :: numprocsY    ! number of tasks in "j" dimension
    INTEGER, ALLOCATABLE :: permuteTasks(:)
    INTEGER :: globalXcount ! staggered domain count in "i" dimension
    INTEGER :: globalYcount ! staggered domain count in "j" dimension
    INTEGER :: myXstart     ! task-local start in "i" dimension
    INTEGER :: myYstart     ! task-local start in "j" dimension
    INTEGER :: myXend       ! staggered task-local end in "i" dimension
    INTEGER :: myYend       ! staggered task-local end in "j" dimension
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
    INTEGER :: rc, debug_level
    INTEGER :: myXcount      ! task-local count in "i" dimension
    INTEGER :: myYcount      ! task-local count in "j" dimension
    INTEGER :: globalCellCounts(2)
    INTEGER :: numprocsXY(2)
    INTEGER :: myPE, i, j, pe, is, ie, js, je, is_min, js_min, ie_max, je_max
    INTEGER :: ips, ipe, jps, jpe, ids, ide, jds, jde
    TYPE(ESMF_VM) :: vm
    TYPE(ESMF_DELayout) :: taskLayout
    REAL(ESMF_KIND_R8), DIMENSION(:), POINTER :: coordX2d, coordY2d
    INTEGER, DIMENSION(3) :: ubnd, lbnd
    CHARACTER (32) :: gridname
    INTEGER, SAVE :: gridID = 0

      CALL get_wrf_debug_level( debug_level )

      CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid_int:  begin...' )
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  numdims = ',numdims
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  DomainStart = ',DomainStart(1:numdims)
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  DomainEnd = ',DomainEnd(1:numdims)
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  MemoryStart = ',MemoryStart(1:numdims)
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  MemoryEnd = ',MemoryEnd(1:numdims)
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  PatchStart = ',PatchStart(1:numdims)
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  PatchEnd = ',PatchEnd(1:numdims)
      CALL wrf_debug ( 5 , TRIM(msg) )
      ! First, determine number of tasks and number of tasks in each decomposed 
      ! dimension (ESMF 2.2.0 is restricted to simple task layouts)
      ! get current ESMF virtual machine and inquire...  
      CALL ESMF_VMGetCurrent(vm, rc=rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_VMGetCurrent', &
                       __FILE__ ,                    &
                       ', line',                     &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
!TODO:  Note (PET==MPI process) assumption here.  This is OK in ESMF 
!TODO:  2.2.0 but may change in a future ESMF release.  If so, we will 
!TODO:  need another way to do this.  May want to grab mpiCommunicator 
!TODO:  instead and ask it directly for number of MPI tasks.  Unless this 
!TODO:  is a serial run...
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
          numprocsY = numprocsY + 1
        ENDIF
        IF ( PatchStart(2) == jpatchStarts(pe) ) THEN
          numprocsX = numprocsX + 1
        ENDIF
      ENDDO
      DEALLOCATE( ipatchStarts, jpatchStarts )
WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  numprocsX = ',numprocsX
CALL wrf_debug ( 5 , TRIM(msg) )
WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  numprocsY = ',numprocsY
CALL wrf_debug ( 5 , TRIM(msg) )
      ! sanity check
      IF ( numprocs /= numprocsX*numprocsY ) THEN
        CALL wrf_error_fatal ( 'ASSERTION FAILED:  numprocs /= numprocsX*numprocsY' )
      ENDIF
      ! Next, create ESMF_DELayout
      numprocsXY = (/ numprocsX, numprocsY /)
!TODO:  1-to-1 DE to PET mapping is assumed below...  
      ALLOCATE( permuteTasks(0:numprocs-1) )
      pe = 0
      DO j = 0, numprocsY-1
        DO i = 0, numprocsX-1
! NOTE:  seems to work both ways...  
! (/ 0 2 1 3 /)
!        permuteTasks(pe) = (i*numprocsY) + j
! (/ 0 1 2 3 /)
        permuteTasks(pe) = pe
        pe = pe + 1
        ENDDO
      ENDDO
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  numprocsXY = ',numprocsXY
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG ioesmf_create_grid_int:  permuteTasks = ',permuteTasks
      CALL wrf_debug ( 5 , TRIM(msg) )
      CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid_int:  calling ESMF_DELayoutCreate' )
      taskLayout = ESMF_DELayoutCreate( vm, numprocsXY, petList=permuteTasks, rc=rc ) 
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_DELayoutCreate', &
                       __FILE__ ,                      &
                       ', line',                       &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid_int:  back from ESMF_DELayoutCreate' )
      DEALLOCATE( permuteTasks )

      CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid_int:  calling ESMF_DELayoutPrint 1' )
      IF ( 5 .LE. debug_level ) THEN
        CALL ESMF_DELayoutPrint( taskLayout, rc=rc )
      ENDIF
      CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid_int:  back from ESMF_DELayoutPrint 1' )

! Compute the dimensions for the ESMF grid, using WRF's non-staggered dimensions
! This is as of ESMF v3, JM 20080715

      ! the [ij][dp][se] bits are for convenience...  
      ids = DomainStart(1); ide = DomainEnd(1); 
      jds = DomainStart(2); jde = DomainEnd(2); 
      ips = PatchStart(1);  ipe = PatchEnd(1); 
      jps = PatchStart(2);  jpe = PatchEnd(2); 
      globalXcount = ide - ids  ! in other words, the number of points from ids to ide-1 inclusive
      globalYcount = jde - jds  ! in other words, the number of points from jds to jde-1 inclusive
      ! task-local numbers of points in patch for staggered arrays
      myXstart = ips
      myYstart = jps
      myXend = MIN(ipe,ide-1)
      myYend = MIN(jpe,jde-1)
      myXcount = myXend - myXstart + 1
      myYcount = myYend - myYstart + 1
      ! gather task-local information on all tasks since 
      ! ESMF_GridDistribute[Block] interface require global knowledge to set up 
      ! decompositions
      ! Recall that coordX and coordY are coordinates of *vertices*, not cell centers.  
      ! Thus they must be 1 bigger than the number of cells.  
      ALLOCATE( allXStart(0:numprocs-1),  allXCount(0:numprocs-1),  &
                allYStart(0:numprocs-1),  allYCount(0:numprocs-1),  &
                dimXCount(0:numprocsX-1), dimYCount(0:numprocsY-1), &
                coordX(globalXcount+1),   coordY(globalYcount+1) )
      CALL GatherIntegerScalars_ESMF(myXcount, myPE, numprocs, allXCount)
      CALL GatherIntegerScalars_ESMF(myXstart, myPE, numprocs, allXStart)
      CALL GatherIntegerScalars_ESMF(myYcount, myPE, numprocs, allYCount)
      CALL GatherIntegerScalars_ESMF(myYstart, myPE, numprocs, allYStart)

      !TODO:  ESMF 2.x does not support mercator, polar-stereographic, or 
      !TODO:  lambert-conformal projections.  Therefore, we're using fake 
      !TODO:  coordinates here.  This means that WRF will either have to 
      !TODO:  couple to models that run on the same coorindate such that 
      !TODO:  grid points are co-located or something else will have to 
      !TODO:  perform the inter-grid interpolation computations.  Replace 
      !TODO:  this once ESMF is upgraded to support the above map 
      !TODO:  projections (via general curvilinear coordinates).  
      CALL wrf_message( 'WARNING:  Using artificial coordinates for ESMF coupling.' )
      CALL wrf_message( 'WARNING:  ESMF coupling interpolation will be incorrect' )
      CALL wrf_message( 'WARNING:  unless grid points in the coupled components' )
      CALL wrf_message( 'WARNING:  are co-located.  This limitation will be removed' )
      CALL wrf_message( 'WARNING:  once ESMF coupling supports generalized' )
      CALL wrf_message( 'WARNING:  curvilinear coordintates needed to represent' )
      CALL wrf_message( 'WARNING:  common map projections used by WRF and other' )
      CALL wrf_message( 'WARNING:  regional models.' )
      ! Note that ESMF defines coordinates at *vertices*
      coordX(1) = 0.0
      DO i = 2, SIZE(coordX)
        coordX(i) = coordX(i-1) + 1.0
      ENDDO
      coordY(1) = 0.0
      DO j = 2, SIZE(coordY)
        coordY(j) = coordY(j-1) + 1.0
      ENDDO
      ! Create an ESMF_Grid
      ! For now we create only a 2D grid suitable for simple coupling of 2D 
      ! surface fields.  Later, create and subset one or more 3D grids.  
!TODO:  Pass staggering info into this routine once ESMF can support staggered 
!TODO:  grids.  For now, it is hard-coded for WRF-ARW.  
      gridID = gridID + 1
      WRITE ( gridname,'(a,i0)' ) 'WRF_grid_', gridID

CALL wrf_debug ( 5 , 'DEBUG WRF:  Calling ESMF_GridCreate' )
WRITE( msg,* ) 'DEBUG WRF:  SIZE(coordX) = ', SIZE(coordX)
CALL wrf_debug ( 5 , TRIM(msg) )
WRITE( msg,* ) 'DEBUG WRF:  SIZE(coordY) = ', SIZE(coordY)
CALL wrf_debug ( 5 , TRIM(msg) )
DO i = 1, SIZE(coordX)
  WRITE( msg,* ) 'DEBUG WRF:  coord1(',i,') = ', coordX(i)
  CALL wrf_debug ( 5 , TRIM(msg) )
ENDDO
DO j = 1, SIZE(coordY)
  WRITE( msg,* ) 'DEBUG WRF:  coord2(',j,') = ', coordY(j)
  CALL wrf_debug ( 5 , TRIM(msg) )
ENDDO
!WRITE( msg,* ) 'DEBUG WRF:  horzstagger = ', ESMF_GRID_HORZ_STAGGER_C_SW
!CALL wrf_debug ( 5 , TRIM(msg) )
WRITE( msg,* ) 'DEBUG WRF:  name = ', TRIM(gridname)
CALL wrf_debug ( 5 , TRIM(msg) )

      ! distribute the ESMF_Grid
      ! ignore repeated values
      is_min = MINVAL(allXStart)
      js_min = MINVAL(allYStart)
      i = 0
      j = 0
      WRITE( msg,* ) 'DEBUG:  is_min = ',is_min,'  allXStart = ',allXStart
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG:  js_min = ',js_min,'  allYStart = ',allYStart
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG:  allXCount = ',allXCount
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG:  allYCount = ',allYCount
      CALL wrf_debug ( 5 , TRIM(msg) )
      DO pe = 0, numprocs-1
        IF (allXStart(pe) == is_min) THEN
          IF (j >= numprocsY) THEN
            WRITE( msg,* ) 'ASSERTION FAILED in ESMF_GridCreate', &
                           __FILE__ ,                                   &
                           ', line',                                    &
                           __LINE__
            CALL wrf_error_fatal ( msg )
          ENDIF
      WRITE( msg,* ) 'DEBUG:  dimYCount(',j,') == allYCount(',pe,')'
      CALL wrf_debug ( 5 , TRIM(msg) )
          dimYCount(j) = allYCount(pe)
          j = j + 1
        ENDIF
        IF (allYStart(pe) == js_min) THEN
          IF (i >= numprocsX) THEN
            WRITE( msg,* ) 'ASSERTION FAILED in ESMF_GridCreate', &
                           __FILE__ ,                                   &
                           ', line',                                    &
                           __LINE__
            CALL wrf_error_fatal ( msg )
          ENDIF
      WRITE( msg,* ) 'DEBUG:  dimXCount(',i,') == allXCount(',pe,')'
      CALL wrf_debug ( 5 , TRIM(msg) )
          dimXCount(i) = allXCount(pe)
          i = i + 1
        ENDIF
      ENDDO
      WRITE( msg,* ) 'DEBUG:  i = ',i,'  dimXCount = ',dimXCount
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG:  j = ',j,'  dimYCount = ',dimYCount
      CALL wrf_debug ( 5 , TRIM(msg) )

#if 0
      esmfgrid = ESMF_GridCreateHorzXY(                     &
                   coord1=coordX, coord2=coordY,            &
                   horzstagger=ESMF_GRID_HORZ_STAGGER_C_SW, &
!TODO:  use this for 3D Grids once it is stable
!                  coordorder=ESMF_COORD_ORDER_XZY,         &
                   name=TRIM(gridname), rc=rc )
#else
! based on example in 3.1 ref man sec 23.2.5, Creating an Irregularly 
! Distributed Rectilinear Grid with a Non-Distributed Vertical Dimension
      !esmfgrid = ESMF_GridCreateShapeTile(  &
!write(0,*)'calling ESMF_GridCreateShapeTile for grid named ',trim(gridname)
!write(0,*)'calling ESMF_GridCreateShapeTile dimXCount ',dimXCount
!write(0,*)'calling ESMF_GridCreateShapeTile dimYCount ',dimYCount
! 5.2.0r      esmfgrid = ESMF_GridCreateShapeTile(  &
      esmfgrid = ESMF_GridCreate(  &
                 countsPerDEDim1=dimXCount , &
                 countsPerDEDim2=dimYcount , &
                 coordDep1=(/1/) , &
                 coordDep2=(/2/) , &
                 indexflag=ESMF_INDEX_GLOBAL, & ! use global indices
                 name=TRIM(gridname), &
                 rc = rc )

      CALL ESMF_GridAddCoord(esmfgrid, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, &
                 rc=rc)


      CALL ESMF_GridGetCoord(esmfgrid,coordDim=1,localDE=0, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, &
                 computationalLBound=lbnd,computationalUBound=ubnd, &
                 farrayptr=coordX2d, &
                 rc=rc)

      DO i=lbnd(1),ubnd(1)
        coordX2d(i) = (i-1)*1.0
      ENDDO
      CALL ESMF_GridGetCoord(esmfgrid,coordDim=2,localDE=0, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, &
                 computationalLBound=lbnd,computationalUBound=ubnd, &
                 farrayptr=coordY2d,                             &
                 rc=rc)
      DO i=lbnd(1),ubnd(1)
        coordY2d(i) = (i-1)*1.0
      ENDDO
                 
                 
#endif
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_GridCreate', &
                       __FILE__ ,                        &
                       ', line',                         &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
CALL wrf_debug ( 5 , 'DEBUG WRF:  back OK from ESMF_GridCreate' )

      CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid_int:  calling ESMF_DELayoutPrint 2' )
      IF ( 5 .LE. debug_level ) THEN
        CALL ESMF_DELayoutPrint( taskLayout, rc=rc )
      ENDIF
      CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid_int:  back from ESMF_DELayoutPrint 2' )

#if 0
      CALL ESMF_GridDistribute( esmfgrid,                  &
                                delayout=taskLayout,       &
                                countsPerDEDim1=dimXCount, &
                                countsPerDEDim2=dimYCount, &
                                rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_GridDistribute ', &
                       __FILE__ ,                       &
                       ', line ',                       &
                       __LINE__ ,                       &
                       ', error code = ',rc
        CALL wrf_error_fatal ( msg )
      ENDIF
#endif
CALL wrf_debug ( 5 , 'DEBUG WRF:  Calling ESMF_GridValidate()' )
      CALL ESMF_GridValidate( esmfgrid, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_GridValidate ',   &
                       __FILE__ ,                       &
                       ', line ',                       &
                       __LINE__ ,                       &
                       ', error code = ',rc
        CALL wrf_error_fatal ( msg )
      ENDIF

CALL wrf_debug ( 5 , 'DEBUG WRF:  back OK from ESMF_GridValidate()' )
      DEALLOCATE( allXStart, allXCount, allYStart, allYCount, &
                  dimXCount, dimYCount, coordX, coordY )

#if 0
      ! Print out the ESMF decomposition info for debug comparison with WRF 
      ! decomposition info.  
      ALLOCATE( cellCounts(0:numprocs-1,2), globalStarts(0:numprocs-1,2) )

      ! extract information about staggered grids for debugging
      CALL ESMF_GridGet( esmfgrid,                               &
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
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered     ipe = ',1+globalStarts(myPE,1) + cellCounts(myPE,1) - 1
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered i count = ',  cellCounts(myPE,1)
      CALL wrf_debug ( 5 , TRIM(msg) )
      CALL ESMF_GridGet( esmfgrid,                               &
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
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered     jpe = ',1+globalStarts(myPE,2) + cellCounts(myPE,2) - 1
      CALL wrf_debug ( 5 , TRIM(msg) )
      WRITE( msg,* ) 'DEBUG:  ESMF     staggered j count = ',  cellCounts(myPE,2)
      CALL wrf_debug ( 5 , TRIM(msg) )

      DEALLOCATE( cellCounts, globalStarts )

      CALL wrf_debug ( 100 , 'DEBUG ioesmf_create_grid_int:  print esmfgrid BEGIN...' )
      IF ( 100 .LE. debug_level ) THEN
        CALL ESMF_GridPrint( esmfgrid, rc=rc )
        IF ( rc /= ESMF_SUCCESS ) THEN
          WRITE( msg,* ) 'Error in ESMF_GridPrint', &
                         __FILE__ ,                        &
                         ', line',                         &
                         __LINE__
          CALL wrf_error_fatal ( msg )
        ENDIF
      ENDIF
      CALL wrf_debug ( 100 , 'DEBUG ioesmf_create_grid_int:  print esmfgrid END' )

#endif
      CALL wrf_debug ( 5 , 'DEBUG ioesmf_create_grid_int:  returning...' )

  END SUBROUTINE ioesmf_create_grid_int



  ! Destroy the ESMF_Grid associated with index DataHandle.  
  ! grid( DataHandle )%ptr is DEALLOCATED (NULLIFIED)
  SUBROUTINE ioesmf_destroy_grid( DataHandle )
    USE module_ext_esmf
    IMPLICIT NONE
    INTEGER, INTENT(IN   ) :: DataHandle
    ! Local declarations
    INTEGER :: id, rc
    TYPE(ESMF_DELayout) :: taskLayout
    LOGICAL :: noneLeft
    IF ( grid( DataHandle )%in_use ) THEN
#if 0
WRITE( msg,* ) 'DEBUG:  ioesmf_destroy_grid( ',DataHandle,' ) begin...'
CALL wrf_debug ( 5 , TRIM(msg) )
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
#endif
      CALL ESMF_GridDestroy( grid( DataHandle )%ptr, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        WRITE( msg,* ) 'Error in ESMF_GridDestroy', &
                       __FILE__ ,                   &
                       ', line',                    &
                       __LINE__
        CALL wrf_error_fatal ( msg )
      ENDIF
      DEALLOCATE( grid( DataHandle )%ptr )
      CALL ioesmf_nullify_grid( DataHandle )
WRITE( msg,* ) 'DEBUG:  ioesmf_destroy_grid( ',DataHandle,' ) end'
CALL wrf_debug ( 5 , TRIM(msg) )
    ENDIF

  END SUBROUTINE ioesmf_destroy_grid


  ! Nullify the grid_ptr associated with index DataHandle.  
  ! grid( DataHandle )%ptr must not be associated
  ! DataHandle must be in a valid range
  SUBROUTINE ioesmf_nullify_grid( DataHandle )
    USE module_ext_esmf
    IMPLICIT NONE
    INTEGER, INTENT(IN   ) :: DataHandle
    NULLIFY( grid( DataHandle )%ptr )
    grid( DataHandle )%in_use = .FALSE.
    grid( DataHandle )%ide_save = 0
    grid( DataHandle )%jde_save = 0
    grid( DataHandle )%kde_save = 0
  END SUBROUTINE ioesmf_nullify_grid


!TODO:  use generic explicit interfaces and remove duplication
!TODO:  use cpp to remove duplication
 SUBROUTINE ioesmf_extract_data_real( data_esmf_real, Field,      &
                                      ips, ipe, jps, jpe, kps, kpe, &
                                      ims, ime, jms, jme, kms, kme )
   USE module_ext_esmf
   IMPLICIT NONE
   INTEGER,            INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
   INTEGER,            INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
   REAL(ESMF_KIND_R4), INTENT(IN   ) :: data_esmf_real( ips:ipe, jps:jpe )
   REAL,               INTENT(  OUT) :: Field( ims:ime, jms:jme, kms:kme )
   Field( ips:ipe, jps:jpe, kms ) = data_esmf_real( ips:ipe, jps:jpe )
 END SUBROUTINE ioesmf_extract_data_real


!TODO:  use cpp to remove duplication
 SUBROUTINE ioesmf_extract_data_int( data_esmf_int, Field,         &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     ims, ime, jms, jme, kms, kme )
   USE module_ext_esmf
   IMPLICIT NONE
   INTEGER,               INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
   INTEGER,               INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
   INTEGER(ESMF_KIND_I4), INTENT(IN   ) :: data_esmf_int( ips:ipe, jps:jpe )
   INTEGER,               INTENT(  OUT) :: Field( ims:ime, jms:jme, kms:kme )
   Field( ips:ipe, jps:jpe, kms ) = data_esmf_int( ips:ipe, jps:jpe )
 END SUBROUTINE ioesmf_extract_data_int


!TODO:  use cpp to remove duplication
 SUBROUTINE ioesmf_insert_data_real( Field, data_esmf_real,        &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     ims, ime, jms, jme, kms, kme )
   USE module_ext_esmf
   IMPLICIT NONE
   INTEGER,               INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
   INTEGER,               INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
   REAL,                  INTENT(IN   ) :: Field( ims:ime, jms:jme, kms:kme )
   REAL(ESMF_KIND_R4),    INTENT(  OUT) :: data_esmf_real( ips:ipe, jps:jpe )
   !TODO:  Remove this hack once we no longer have to store non-staggered 
   !TODO:  arrays in space dimensioned for staggered arrays.  
   data_esmf_real = 0.0_ESMF_KIND_R4
   data_esmf_real( ips:ipe, jps:jpe ) = Field( ips:ipe, jps:jpe, kms )
 END SUBROUTINE ioesmf_insert_data_real


!TODO:  use cpp to remove duplication
 SUBROUTINE ioesmf_insert_data_int( Field, data_esmf_int,         &
                                    ips, ipe, jps, jpe, kps, kpe, &
                                    ims, ime, jms, jme, kms, kme )
   USE module_ext_esmf
   IMPLICIT NONE
   INTEGER,               INTENT(IN   ) :: ips, ipe, jps, jpe, kps, kpe
   INTEGER,               INTENT(IN   ) :: ims, ime, jms, jme, kms, kme
   INTEGER,               INTENT(IN   ) :: Field( ims:ime, jms:jme, kms:kme )
   INTEGER(ESMF_KIND_I4), INTENT(  OUT) :: data_esmf_int( ips:ipe, jps:jpe )
   !TODO:  Remove this hack once we no longer have to store non-staggered 
   !TODO:  arrays in space dimensioned for staggered arrays.  
   data_esmf_int = 0.0_ESMF_KIND_I4
   data_esmf_int( ips:ipe, jps:jpe ) = Field( ips:ipe, jps:jpe, kms )
 END SUBROUTINE ioesmf_insert_data_int


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
  CALL wrf_debug(1,'ext_esmf_open_for_read not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
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

  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_opened:  begin, DataHandle = ', DataHandle
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_opened:  int_valid_handle(',DataHandle,') = ', &
                 int_valid_handle( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_opened:  int_handle_in_use(',DataHandle,') = ', &
                 int_handle_in_use( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_opened:  opened_for_read(',DataHandle,') = ', &
                 opened_for_read( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_opened:  okay_to_read(',DataHandle,') = ', &
                 okay_to_read( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_opened:  opened_for_write(',DataHandle,') = ', &
                 opened_for_write( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_opened:  okay_to_write(',DataHandle,') = ', &
                 okay_to_write( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )

!TODO:  need to cache file name and match with FileName argument and return 
!TODO:  FileStatus = WRF_FILE_NOT_OPENED if they do not match

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
    WRITE( msg,* ) 'ERROR ext_esmf_inquire_opened:  file handle ',DataHandle,' is invalid'
    CALL wrf_error_fatal ( TRIM(msg) )
  ENDIF

  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_opened:  end, FileStatus = ', FileStatus
  CALL wrf_debug ( 5 , TRIM(msg) )

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

  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_filename:  begin, DataHandle = ', DataHandle
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_filename:  int_valid_handle(',DataHandle,') = ', &
                 int_valid_handle( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_filename:  int_handle_in_use(',DataHandle,') = ', &
                 int_handle_in_use( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_filename:  opened_for_read(',DataHandle,') = ', &
                 opened_for_read( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_filename:  okay_to_read(',DataHandle,') = ', &
                 okay_to_read( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_filename:  opened_for_write(',DataHandle,') = ', &
                 opened_for_write( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )
  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_filename:  okay_to_write(',DataHandle,') = ', &
                 okay_to_write( DataHandle )
  CALL wrf_debug ( 5 , TRIM(msg) )

!TODO:  need to cache file name and return via FileName argument

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
    WRITE( msg,* ) 'ERROR ext_esmf_inquire_filename:  file handle ',DataHandle,' is invalid'
    CALL wrf_error_fatal ( TRIM(msg) )
  ENDIF

  WRITE( msg,* ) 'DEBUG ext_esmf_inquire_filename:  end, FileStatus = ', FileStatus
  CALL wrf_debug ( 5 , TRIM(msg) )

  Status = 0
  RETURN
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
  ! locals
  TYPE state_ptr
    TYPE(ESMF_State), POINTER :: stateptr
  END TYPE state_ptr
  TYPE(state_ptr) :: states(2)
  TYPE(ESMF_State), POINTER :: state
  INTEGER :: numItems, numFields, i, istate
  TYPE(ESMF_StateItem_Flag), ALLOCATABLE :: itemTypes(:)
  TYPE(ESMF_Field) :: tmpField
  REAL, POINTER :: tmp_ptr(:,:)
  CHARACTER (len=ESMF_MAXSTR), ALLOCATABLE :: itemNames(:)
  CHARACTER (len=ESMF_MAXSTR) :: str
  INTEGER :: rc

! TODO:  The code below hangs with this error message:  
! TODO:  "ext_esmf_ioclose:  ESMF_FieldGetDataPointer( LANDMASK) failed"
! TODO:  Fix this so ESMF objects actually get destroyed to avoid memory 
! TODO:  leaks.  
  CALL wrf_debug( 5, 'ext_esmf_ioclose:  WARNING:  not destroying ESMF objects' )
#if 0
  !TODO:  Need to upgrade this to use nested ESMF_States if we want support 
  !TODO:  more than one auxin and one auxhist stream for ESMF.  
  IF ( int_valid_handle (DataHandle) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      ! Iterate through importState *and* exportState, find each ESMF_Field, 
      ! extract its data pointer and deallocate it, then destroy the 
      ! ESMF_Field.  
      CALL ESMF_ImportStateGetCurrent(states(1)%stateptr, rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal( 'ext_esmf_ioclose:  ESMF_ImportStateGetCurrent failed' )
      ENDIF
      CALL ESMF_ExportStateGetCurrent(states(2)%stateptr, rc)
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal( 'ext_esmf_ioclose:  ESMF_ExportStateGetCurrent failed' )
      ENDIF
      DO istate=1, 2
        state => states(istate)%stateptr   ! all this to avoid assignment (@#$%)
        ! Since there are no convenient iterators for ESMF_State (@#$%),
        ! write a lot of code...
        ! Figure out how many items are in the ESMF_State
        CALL ESMF_StateGet(state, itemCount=numItems, rc=rc)
        IF ( rc /= ESMF_SUCCESS) THEN
          CALL wrf_error_fatal ( 'ext_esmf_ioclose:  ESMF_StateGet(numItems) failed' )
        ENDIF
        ! allocate an array to hold the types of all items
        ALLOCATE( itemTypes(numItems) )
        ! allocate an array to hold the names of all items
        ALLOCATE( itemNames(numItems) )
        ! get the item types and names
!5.2.0r        CALL ESMF_StateGet(state, stateitemtypeList=itemTypes, &
        CALL ESMF_StateGet(state, itemtypeList=itemTypes, &
                           itemNameList=itemNames, rc=rc)
        IF ( rc /= ESMF_SUCCESS) THEN
          WRITE(str,*) 'ext_esmf_ioclose:  ESMF_StateGet itemTypes failed with rc = ', rc
          CALL wrf_error_fatal ( str )
        ENDIF
        ! count how many items are ESMF_Fields
        numFields = 0
        DO i=1,numItems
          IF ( itemTypes(i) == ESMF_STATEITEM_FIELD ) THEN
            numFields = numFields + 1
          ENDIF
        ENDDO
        IF ( numFields > 0) THEN
          ! finally, extract nested ESMF_Fields by name, if there are any
          ! (should be able to do this by index at least -- @#%$)
          DO i=1,numItems
            IF ( itemTypes(i) == ESMF_STATEITEM_FIELD ) THEN
              CALL ESMF_StateGetField( state, TRIM(itemNames(i)), &
                                       tmpField, rc=rc )
              IF ( rc /= ESMF_SUCCESS) THEN
                WRITE(str,*) 'ext_esmf_ioclose:  ESMF_StateGetField(',TRIM(itemNames(i)),') failed'
                CALL wrf_error_fatal ( str )
              ENDIF
              ! destroy pointer in field
              CALL ESMF_FieldGetDataPointer( tmpField, tmp_ptr, rc=rc )
              IF (rc /= ESMF_SUCCESS) THEN
                WRITE( str , * )                                   &
                  'ext_esmf_ioclose:  ESMF_FieldGetDataPointer( ', &
                  TRIM(itemNames(i)),') failed'
                CALL wrf_error_fatal ( TRIM(str) )
              ENDIF
              DEALLOCATE( tmp_ptr )
              ! destroy field
              CALL ESMF_FieldDestroy( tmpField, rc=rc )
              IF (rc /= ESMF_SUCCESS) THEN
                WRITE( str , * )                            &
                  'ext_esmf_ioclose:  ESMF_FieldDestroy( ', &
                  TRIM(itemNames(i)),') failed'
                CALL wrf_error_fatal ( TRIM(str) )
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        ! deallocate locals
        DEALLOCATE( itemTypes )
        DEALLOCATE( itemNames )
      ENDDO
      ! destroy ESMF_Grid associated with DataHandle
      CALL ioesmf_destroy_grid( DataHandle )
    ENDIF
  ENDIF
#endif
  Status = 0
  RETURN
END SUBROUTINE ext_esmf_ioclose

!--- ioexit
SUBROUTINE ext_esmf_ioexit( Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER :: i
  Status = 0
! TODO:  The code below causes ext_ncd_ioclose() to fail in the 
! TODO:  SST component for reasons as-yet unknown.  
! TODO:  Fix this so ESMF objects actually get destroyed to avoid memory 
! TODO:  leaks.  
  CALL wrf_debug( 5, 'ext_esmf_ioexit:  WARNING:  not destroying ESMF objects' )
#if 0
  DO i = 1, int_num_handles
    ! close any remaining open DataHandles
    CALL ext_esmf_ioclose ( i, Status )
    ! destroy ESMF_Grid for this DataHandle
    CALL ioesmf_destroy_grid( i )
  ENDDO
  CALL wrf_debug ( 5 , &
    'ext_esmf_ioexit:  DEBUG:  done cleaning up ESMF objects' )
#endif
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
  CALL wrf_debug(1, "ext_esmf_get_next_time() not supported yet")
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_next_time

!--- set_time
SUBROUTINE ext_esmf_set_time ( DataHandle, DateStr, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1, "ext_esmf_set_time() not supported yet")
  Status = WRF_WARN_NOTSUPPORTED
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
  CALL wrf_debug(1, "ext_esmf_get_var_info() not supported yet")
  Status = WRF_WARN_NOTSUPPORTED
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
  CALL wrf_debug(1, "ext_esmf_get_next_var() not supported yet")
  Status = WRF_WARN_NOTSUPPORTED
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
  CALL wrf_debug(1, "ext_esmf_get_dom_ti_real() not supported yet")
  Status = WRF_WARN_NOTSUPPORTED
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
  CALL wrf_debug(1, "ext_esmf_put_dom_ti_real() not supported yet")
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_dom_ti_real 

!--- get_dom_ti_double
SUBROUTINE ext_esmf_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_dom_ti_double not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_dom_ti_double 

!--- put_dom_ti_double
SUBROUTINE ext_esmf_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_dom_ti_double not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
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

  Status = 0
  IF      ( Element == 'WEST-EAST_GRID_DIMENSION' ) THEN
    Data(1) = grid( DataHandle )%ide_save
    Outcount = 1
  ELSE IF ( Element == 'SOUTH-NORTH_GRID_DIMENSION' ) THEN
    Data(1) = grid( DataHandle )%jde_save
    Outcount = 1
  ELSE IF ( Element == 'BOTTOM-TOP_GRID_DIMENSION' ) THEN
    Data(1) = grid( DataHandle )%kde_save
    Outcount = 1
  ELSE
    CALL wrf_debug(1,'ext_esmf_get_dom_ti_integer not fully supported yet')
    Status = WRF_WARN_NOTSUPPORTED
  ENDIF

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
  CALL wrf_debug(1,'ext_esmf_put_dom_ti_integer not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_dom_ti_integer 

!--- get_dom_ti_logical
SUBROUTINE ext_esmf_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_dom_ti_logical not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_dom_ti_logical 

!--- put_dom_ti_logical
SUBROUTINE ext_esmf_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_dom_ti_logical not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
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
  CALL wrf_debug(1,'ext_esmf_get_dom_ti_char not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
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
  CALL wrf_debug(1,'ext_esmf_put_dom_ti_char not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_dom_ti_char 

!--- get_dom_td_real
SUBROUTINE ext_esmf_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_dom_td_real not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_real 

!--- put_dom_td_real
SUBROUTINE ext_esmf_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_dom_td_real not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_real 

!--- get_dom_td_double
SUBROUTINE ext_esmf_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_dom_td_double not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_double 

!--- put_dom_td_double
SUBROUTINE ext_esmf_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_dom_td_double not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_double 

!--- get_dom_td_integer
SUBROUTINE ext_esmf_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_dom_td_integer not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_integer 

!--- put_dom_td_integer
SUBROUTINE ext_esmf_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_dom_td_integer not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_integer 

!--- get_dom_td_logical
SUBROUTINE ext_esmf_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_dom_td_logical not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_logical 

!--- put_dom_td_logical
SUBROUTINE ext_esmf_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_dom_td_logical not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_logical 

!--- get_dom_td_char
SUBROUTINE ext_esmf_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_dom_td_char not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_dom_td_char 

!--- put_dom_td_char
SUBROUTINE ext_esmf_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_dom_td_char not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_dom_td_char 

!--- get_var_ti_real
SUBROUTINE ext_esmf_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_ti_real not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_real 

!--- put_var_ti_real
SUBROUTINE ext_esmf_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_ti_real not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_real 

!--- get_var_ti_double
SUBROUTINE ext_esmf_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_ti_double not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_double 

!--- put_var_ti_double
SUBROUTINE ext_esmf_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_ti_double not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_double 

!--- get_var_ti_integer
SUBROUTINE ext_esmf_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_ti_integer not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_integer 

!--- put_var_ti_integer
SUBROUTINE ext_esmf_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_ti_integer not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_integer 

!--- get_var_ti_logical
SUBROUTINE ext_esmf_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_ti_logical not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_ti_logical 

!--- put_var_ti_logical
SUBROUTINE ext_esmf_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_ti_logical not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
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
  CALL wrf_debug(1,'ext_esmf_get_var_ti_char not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
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
  CALL wrf_debug(1,'ext_esmf_put_var_ti_char not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_ti_char 

!--- get_var_td_real
SUBROUTINE ext_esmf_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_td_real not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_td_real 

!--- put_var_td_real
SUBROUTINE ext_esmf_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_td_real not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_td_real 

!--- get_var_td_double
SUBROUTINE ext_esmf_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_td_double not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_td_double 

!--- put_var_td_double
SUBROUTINE ext_esmf_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_td_double not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_td_double 

!--- get_var_td_integer
SUBROUTINE ext_esmf_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_td_integer not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_td_integer 

!--- put_var_td_integer
SUBROUTINE ext_esmf_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_td_integer not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_td_integer 

!--- get_var_td_logical
SUBROUTINE ext_esmf_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_td_logical not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_td_logical 

!--- put_var_td_logical
SUBROUTINE ext_esmf_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_td_logical not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_td_logical 

!--- get_var_td_char
SUBROUTINE ext_esmf_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_get_var_td_char not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_get_var_td_char 

!--- put_var_td_char
SUBROUTINE ext_esmf_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
  USE module_ext_esmf
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug(1,'ext_esmf_put_var_td_char not supported yet')
  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_esmf_put_var_td_char 


