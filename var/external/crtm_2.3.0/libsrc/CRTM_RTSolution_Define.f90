!
! CRTM_RTSolution_Define
!
! Module defining the CRTM RTSolution structure and containing routines
! to manipulate it.
!
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, 13-May-2004
!                   paul.vandelst@noaa.gov
!

MODULE CRTM_RTSolution_Define


  ! ------------------
  ! Environment set up
  ! ------------------
  ! Intrinsic modules
  USE ISO_Fortran_Env      , ONLY: OUTPUT_UNIT
  ! Module use statements
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  USE SensorInfo_Parameters, ONLY: INVALID_SENSOR, &
                                   INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID
  USE CRTM_Parameters      , ONLY: STRLEN
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CRTM_RTSolution_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(+)
  PUBLIC :: OPERATOR(-)
  ! Public procedures
  PUBLIC :: CRTM_RTSolution_Associated
  PUBLIC :: CRTM_RTSolution_Destroy
  PUBLIC :: CRTM_RTSolution_Create
  PUBLIC :: CRTM_RTSolution_Zero
  PUBLIC :: CRTM_RTSolution_Inspect
  PUBLIC :: CRTM_RTSolution_DefineVersion
  PUBLIC :: CRTM_RTSolution_Compare
  PUBLIC :: CRTM_RTSolution_Statistics
  PUBLIC :: CRTM_RTSolution_InquireFile
  PUBLIC :: CRTM_RTSolution_ReadFile
  PUBLIC :: CRTM_RTSolution_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_RTSolution_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE CRTM_RTSolution_Add
  END INTERFACE OPERATOR(+)

  INTERFACE OPERATOR(-)
    MODULE PROCEDURE CRTM_RTSolution_Subtract
  END INTERFACE OPERATOR(-)

  INTERFACE OPERATOR(**)
    MODULE PROCEDURE CRTM_RTSolution_Exponent
  END INTERFACE OPERATOR(**)

  INTERFACE OPERATOR(/)
    MODULE PROCEDURE CRTM_RTSolution_Normalise
  END INTERFACE OPERATOR(/)

  INTERFACE SQRT
    MODULE PROCEDURE CRTM_RTSolution_Sqrt
  END INTERFACE SQRT

  INTERFACE CRTM_RTSolution_Inspect
    MODULE PROCEDURE Scalar_Inspect
    MODULE PROCEDURE Rank2_Inspect
  END INTERFACE CRTM_RTSolution_Inspect


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_RTSolution_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


  ! -------------------------------
  ! RTSolution data type definition
  ! -------------------------------
  !:tdoc+:
  TYPE :: CRTM_RTSolution_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimensions
    INTEGER :: n_Layers = 0  ! K
    ! Sensor information
    CHARACTER(STRLEN) :: Sensor_ID        = ''
    INTEGER           :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER           :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER           :: Sensor_Channel   = 0
    ! RT algorithm information
    CHARACTER(STRLEN) :: RT_Algorithm_Name = ''
    ! Internal variables. Users do not need to worry about these.
    LOGICAL :: Scattering_Flag = .TRUE.
    INTEGER :: n_Full_Streams  = 0
    INTEGER :: n_Stokes        = 0
    ! Forward radiative transfer intermediate results for a single channel
    !    These components are not defined when they are used as TL, AD
    !    and K variables
    REAL(fp) :: SSA_Max                 = ZERO  ! Max Single Scattering Albedo in the profile                      
    REAL(fp) :: SOD                     = ZERO  ! Scattering Optical Depth
    REAL(fp) :: Surface_Emissivity      = ZERO
    REAL(fp) :: Surface_Reflectivity    = ZERO
    REAL(fp) :: Up_Radiance             = ZERO
    REAL(fp) :: Down_Radiance           = ZERO
    REAL(fp) :: Down_Solar_Radiance     = ZERO
    REAL(fp) :: Surface_Planck_Radiance = ZERO
    REAL(fp) :: Total_Cloud_Cover       = ZERO  ! Only used for fractional clear/cloudy calculation
    REAL(fp) :: R_clear                 = ZERO  ! Only used for fractional clear/cloudy calculation
    REAL(fp) :: Tb_clear                = ZERO  ! Only used for fractional clear/cloudy calculation
    REAL(fp), ALLOCATABLE :: Upwelling_Overcast_Radiance(:)   ! K
    REAL(fp), ALLOCATABLE :: Upwelling_Radiance(:)   ! K
    REAL(fp), ALLOCATABLE :: Layer_Optical_Depth(:)  ! K
    REAL(fp), ALLOCATABLE :: Single_Scatter_Albedo(:)  ! K  
    ! Radiative transfer results for a single channel
    REAL(fp) :: Radiance               = ZERO
    REAL(fp) :: Brightness_Temperature = ZERO
    REAL(fp) :: Gamma                  = ZERO
  END TYPE CRTM_RTSolution_type
  !:tdoc-:


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_Associated
!
! PURPOSE:
!   Elemental function to test the status of the allocatable components
!   of a CRTM RTSolution object.
!
! CALLING SEQUENCE:
!   Status = CRTM_RTSolution_Associated( RTSolution )
!
! OBJECTS:
!   RTSolution:   RTSolution structure which is to have its member's
!                 status tested.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Scalar or any rank
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   Status:       The return value is a logical value indicating the
!                 status of the RTSolution members.
!                   .TRUE.  - if the array components are allocated.
!                   .FALSE. - if the array components are not allocated.
!                 UNITS:      N/A
!                 TYPE:       LOGICAL
!                 DIMENSION:  Same as input RTSolution argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Associated( RTSolution ) RESULT( Status )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: RTSolution
    LOGICAL :: Status
    Status = RTSolution%Is_Allocated
  END FUNCTION CRTM_RTSolution_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_Destroy
!
! PURPOSE:
!   Elemental subroutine to re-initialize CRTM RTSolution objects.
!
! CALLING SEQUENCE:
!   CALL CRTM_RTSolution_Destroy( RTSolution )
!
! OBJECTS:
!   RTSolution:   Re-initialized RTSolution structure.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Scalar OR any rank
!                 ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_RTSolution_Destroy( RTSolution )
    TYPE(CRTM_RTSolution_type), INTENT(OUT) :: RTSolution
    RTSolution%Is_Allocated = .FALSE.
    RTSolution%n_Layers = 0
  END SUBROUTINE CRTM_RTSolution_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_Create
!
! PURPOSE:
!   Elemental subroutine to create an instance of the CRTM RTSolution object.
!
! CALLING SEQUENCE:
!   CALL CRTM_RTSolution_Create( RTSolution, n_Layers )
!
! OBJECTS:
!   RTSolution:   RTSolution structure.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Scalar or any rank
!                 ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!   n_Layers:     Number of layers for which there is RTSolution data.
!                 Must be > 0.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Same as RTSolution object
!                 ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_RTSolution_Create( RTSolution, n_Layers )
    ! Arguments
    TYPE(CRTM_RTSolution_type), INTENT(OUT) :: RTSolution
    INTEGER,                    INTENT(IN)  :: n_Layers
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( RTSolution%Upwelling_Radiance(n_Layers), &
              RTSolution%Upwelling_Overcast_Radiance(n_Layers), &
              RTSolution%Layer_Optical_Depth(n_Layers), &
              RTSolution%Single_Scatter_Albedo(n_Layers), & 
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    RTSolution%n_Layers = n_Layers
    ! ...Arrays
    RTSolution%Upwelling_Radiance  = ZERO
    RTSolution%Upwelling_Overcast_Radiance  = ZERO
    RTSolution%Layer_Optical_Depth = ZERO
    RTSolution%Single_Scatter_Albedo = ZERO 

    ! Set allocation indicator
    RTSolution%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_RTSolution_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_Zero
!
! PURPOSE:
!   Elemental subroutine to zero out the data components
!   in a CRTM RTSolution object.
!
! CALLING SEQUENCE:
!   CALL CRTM_RTSolution_Zero( rts )
!
! OUTPUTS:
!   rts:          CRTM RTSolution structure in which the data components
!                 are to be zeroed out.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Scalar or any rank
!                 ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!   - The dimension components of the structure are *NOT* set to zero.
!   - The sensor infomration and RT algorithm components are
!     *NOT* reset in this routine.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_RTSolution_Zero( RTSolution )
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution

    ! Zero out the scalar data components
    RTSolution%SSA_Max                 = ZERO 
    RTSolution%SOD                     = ZERO
    RTSolution%Surface_Emissivity      = ZERO
    RTSolution%Surface_Reflectivity    = ZERO
    RTSolution%Up_Radiance             = ZERO
    RTSolution%Down_Radiance           = ZERO
    RTSolution%Down_Solar_Radiance     = ZERO
    RTSolution%Surface_Planck_Radiance = ZERO
    RTSolution%Total_Cloud_Cover       = ZERO
    RTSolution%R_clear                 = ZERO
    RTSolution%Tb_clear                = ZERO
    RTSolution%Radiance                = ZERO
    RTSolution%Brightness_Temperature  = ZERO
    RTSolution%Gamma                   = ZERO

    ! Zero out the array data components
    IF ( CRTM_RTSolution_Associated(RTSolution) ) THEN
      RTSolution%Upwelling_Radiance  = ZERO
      RTSolution%Upwelling_Overcast_Radiance  = ZERO
      RTSolution%Layer_Optical_Depth = ZERO
      RTSolution%Single_Scatter_Albedo = ZERO  
    END IF

  END SUBROUTINE CRTM_RTSolution_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_Inspect
!
! PURPOSE:
!   Subroutine to print the contents of a CRTM RTSolution object to stdout.
!
! CALLING SEQUENCE:
!   CALL CRTM_RTSolution_Inspect( RTSolution, Unit=unit )
!
! INPUTS:
!   RTSolution:  CRTM RTSolution object to display.
!                UNITS:      N/A
!                TYPE:       CRTM_RTSolution_type
!                DIMENSION:  Scalar or Rank-2 (n_channels x n_profiles)
!                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Unit:        Unit number for an already open file to which the output
!                will be written.
!                If the argument is specified and the file unit is not
!                connected, the output goes to stdout.
!                UNITS:      N/A
!                TYPE:       INTEGER
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Scalar_Inspect( RTSolution, Unit )
    ! Arguments
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: RTSolution
    INTEGER,          OPTIONAL, INTENT(IN) :: Unit
    ! Local variables
    INTEGER :: fid

    ! Setup
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF


    WRITE(fid,'(1x,"RTSolution OBJECT")')
    ! Display components
    WRITE(fid,'(3x,"Sensor Id                     : ",a )') TRIM(RTSolution%Sensor_ID)
    WRITE(fid,'(3x,"WMO Satellite Id              : ",i0)') RTSolution%WMO_Satellite_ID
    WRITE(fid,'(3x,"WMO Sensor Id                 : ",i0)') RTSolution%WMO_Sensor_ID
    WRITE(fid,'(3x,"Channel                       : ",i0)') RTSolution%Sensor_Channel
    WRITE(fid,'(3x,"RT Algorithm Name             : ",a )') RTSolution%RT_Algorithm_Name
    WRITE(fid,'(3x,"Scattering Optical Depth      : ",es13.6)') RTSolution%SOD
    WRITE(fid,'(3x,"Surface Emissivity            : ",es13.6)') RTSolution%Surface_Emissivity
    WRITE(fid,'(3x,"Surface Reflectivity          : ",es13.6)') RTSolution%Surface_Reflectivity
    WRITE(fid,'(3x,"Up Radiance                   : ",es13.6)') RTSolution%Up_Radiance
    WRITE(fid,'(3x,"Down Radiance                 : ",es13.6)') RTSolution%Down_Radiance
    WRITE(fid,'(3x,"Down Solar Radiance           : ",es13.6)') RTSolution%Down_Solar_Radiance
    WRITE(fid,'(3x,"Surface Planck Radiance       : ",es13.6)') RTSolution%Surface_Planck_Radiance
    WRITE(fid,'(3x,"Total cloud cover             : ",es13.6)') RTSolution%Total_Cloud_Cover
    WRITE(fid,'(3x,"Radiance (clear)              : ",es13.6)') RTSolution%R_clear
    WRITE(fid,'(3x,"Brightness Temperature (clear): ",es13.6)') RTSolution%Tb_clear
    WRITE(fid,'(3x,"Radiance                      : ",es13.6)') RTSolution%Radiance
    WRITE(fid,'(3x,"Brightness Temperature        : ",es13.6)') RTSolution%Brightness_Temperature
    WRITE(fid,'(3x,"Gamma                         : ",es13.6)') RTSolution%Gamma
    IF ( .NOT. CRTM_RTSolution_Associated(RTSolution) ) RETURN
    WRITE(fid,'(3x,"n_Layers : ",i0)') RTSolution%n_Layers
    WRITE(fid,'(3x,"Upwelling Overcast Radiance :")')
    WRITE(fid,'(5(1x,es13.6,:))') RTSolution%Upwelling_Overcast_Radiance
    WRITE(fid,'(3x,"Upwelling Radiance :")')
    WRITE(fid,'(5(1x,es13.6,:))') RTSolution%Upwelling_Radiance
    WRITE(fid,'(3x,"Layer Optical Depth :")')
    WRITE(fid,'(5(1x,es13.6,:))') RTSolution%Layer_Optical_Depth
  END SUBROUTINE Scalar_Inspect


  SUBROUTINE Rank2_Inspect( RTSolution, Unit )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: RTSolution(:,:)
    INTEGER,          OPTIONAL, INTENT(IN) :: Unit
    INTEGER :: fid
    INTEGER :: i, n_channels
    INTEGER :: j, n_profiles

    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF

    n_channels = SIZE(RTSolution,1)
    n_profiles = SIZE(RTSolution,2)
    DO j = 1, n_profiles
      DO i = 1, n_channels
        WRITE(fid, FMT='(1x,"PROFILE INDEX:",i0,", CHANNEL INDEX:",i0," - ")', ADVANCE='NO') j,i
        CALL Scalar_Inspect(RTSolution(i,j), Unit=Unit)
      END DO
    END DO
  END SUBROUTINE Rank2_Inspect



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_DefineVersion
!
! PURPOSE:
!   Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!   CALL CRTM_RTSolution_DefineVersion( Id )
!
! OUTPUTS:
!   Id:            Character string containing the version Id information
!                  for the module.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_RTSolution_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_RTSolution_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!   CRTM_RTSolution_Compare
!
! PURPOSE:
!   Elemental function to compare two CRTM_RTSolution objects to within
!   a user specified number of significant figures.
!
! CALLING SEQUENCE:
!   is_comparable = CRTM_RTSolution_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!   x, y:          Two CRTM RTSolution objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       CRTM_RTSolution_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   n_SigFig:      Number of significant figure to compare floating point
!                  components.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Conformable with inputs
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   is_comparable: Logical value indicating whether the inputs are
!                  comparable.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as inputs.
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: x, y
    INTEGER,          OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF

    ! Check the structure association status
    IF ( CRTM_RTSolution_Associated(x) .NEQV. CRTM_RTSolution_Associated(y) ) RETURN

    ! Check the sensor information
    IF ( (x%Sensor_ID        /= y%Sensor_ID       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) .OR. &
         (x%Sensor_Channel   /= y%Sensor_Channel  ) ) RETURN

    ! Check the RT algorithm name
    IF ( x%RT_Algorithm_Name /= y%RT_Algorithm_Name ) RETURN

    ! Check the scalar components
    IF ( .NOT. Compares_Within_Tolerance(x%SOD                    , y%SOD                    , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Surface_Emissivity     , y%Surface_Emissivity     , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Surface_Reflectivity   , y%Surface_Reflectivity   , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Up_Radiance            , y%Up_Radiance            , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Down_Radiance          , y%Down_Radiance          , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Down_Solar_Radiance    , y%Down_Solar_Radiance    , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Surface_Planck_Radiance, y%Surface_Planck_Radiance, n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Total_Cloud_Cover      , y%Total_Cloud_Cover      , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%R_clear                , y%R_clear                , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Tb_clear               , y%Tb_clear               , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Radiance               , y%Radiance               , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Brightness_Temperature , y%Brightness_Temperature , n) ) RETURN

    ! Check the array components
    IF ( CRTM_RTSolution_Associated(x) .AND. CRTM_RTSolution_Associated(y) ) THEN
      IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Upwelling_Overcast_Radiance, y%Upwelling_Overcast_Radiance, n))) .OR. &
           (.NOT. ALL(Compares_Within_Tolerance(x%Upwelling_Radiance         , y%Upwelling_Radiance         , n))) .OR. &
           (.NOT. ALL(Compares_Within_Tolerance(x%Layer_Optical_Depth        , y%Layer_Optical_Depth        , n))) ) RETURN
    END IF

    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_RTSolution_Compare


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_Statistics
!
! PURPOSE:
!   Function to compute the statistics of an array of CRTM RTSolution objects.
!
! CALLING SEQUENCE:
!   Error_Status = CRTM_RTSolution_Statistics( rts, rts_stats )
!
! INPUTS:
!   rts:          RTSolution object array for which statistics are required.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Rank-2 (n_channels x n_profiles)
!                 ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!   rts_stats:    Allocatable RTSolution object array containing the statistics
!                 for each channel.
!                   rts_stats(:,1) contains the profile average
!                   rts_stats(:,2) contains the profile standard deviation
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Rank-2 (n_channels x 2)
!                 ATTRIBUTES: INTENT(IN), ALLOCATABLE
!
! FUNCTION RESULT:
!   Error_Status: The return value is an integer defining the error status.
!                 The error codes are defined in the Message_Handler module.
!                 If == SUCCESS, the file write was successful
!                    == FAILURE, an unrecoverable error occurred.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_RTSolution_Statistics(rts, rts_stats) RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_RTSolution_type),              INTENT(IN)  :: rts(:,:)
    TYPE(CRTM_RTSolution_type), ALLOCATABLE, INTENT(OUT) :: rts_stats(:,:)
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_Statistics'
    ! Function variables
    CHARACTER(ML) :: err_msg
    CHARACTER(ML) :: alloc_msg
    INTEGER :: alloc_stat
    INTEGER :: n_channels, l
    INTEGER :: n_profiles, m
    REAL(fp) :: factor

    ! Setup
    err_stat = SUCCESS
    n_channels = SIZE(rts, DIM=1)
    n_profiles = SIZE(rts, DIM=2)
    factor = REAL(n_profiles,fp)


    ! Allocate the output stats object array
    ALLOCATE( rts_stats(n_channels, 2), &
              STAT = alloc_stat )
             !STAT = alloc_stat, ERRMSG = alloc_msg )
    IF ( alloc_stat /= 0 ) THEN
      err_msg = 'Error allocating output RTSolution structure - '//TRIM(alloc_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, err_msg, err_stat ); RETURN
    END IF
    rts_stats(:,1) = rts(:,1)
    rts_stats(:,2) = rts(:,1)
    CALL CRTM_RTSolution_Zero(rts_stats)


    ! Compute the average
    DO m = 1, n_profiles
      DO l = 1, n_channels
        rts_stats(l,1) = rts_stats(l,1) + rts(l,m)
      END DO
    END DO
    !rts_stats(:,1) = rts_stats(:,1)/factor
    DO l = 1, n_channels
      rts_stats(l,1) = rts_stats(l,1)/factor
    END DO

    ! Compute the standard deviation
    DO m = 1, n_profiles
      DO l = 1, n_channels
        rts_stats(l,2) = rts_stats(l,2) + (rts(l,m) - rts_stats(l,1))**2
      END DO
    END DO
    !rts_stats(:,2) = SQRT(rts_stats(:,2)/factor)
    DO l = 1, n_channels
      rts_stats(l,2) = SQRT(rts_stats(l,2)/factor)
    END DO

    ! Replace the algorithm identifier
    rts_stats(:,1)%RT_Algorithm_Name = 'Average'
    rts_stats(:,2)%RT_Algorithm_Name = 'Standard deviation'

  END FUNCTION CRTM_RTSolution_Statistics


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_InquireFile
!
! PURPOSE:
!   Function to inquire CRTM RTSolution object files.
!
! CALLING SEQUENCE:
!   Error_Status = CRTM_RTSolution_InquireFile( Filename               , &
!                                               n_Channels = n_Channels, &
!                                               n_Profiles = n_Profiles  )
!
! INPUTS:
!   Filename:       Character string specifying the name of a
!                   CRTM RTSolution data file to read.
!                   UNITS:      N/A
!                   TYPE:       CHARACTER(*)
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!   n_Channels:     The number of spectral channels for which there is
!                   data in the file.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!   n_Profiles:     The number of profiles in the data file.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!   Error_Status:   The return value is an integer defining the error status.
!                   The error codes are defined in the Message_Handler module.
!                   If == SUCCESS, the file inquire was successful
!                      == FAILURE, an unrecoverable error occurred.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_RTSolution_InquireFile( &
    Filename   , &  ! Input
    n_Channels , &  ! Optional output
    n_Profiles ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: l, m

    ! Set up
    err_stat = SUCCESS
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the number of channels,profiles
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) l, m
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Set the return arguments
    IF ( PRESENT(n_Channels) ) n_Channels = l
    IF ( PRESENT(n_Profiles) ) n_Profiles = m

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_RTSolution_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_ReadFile
!
! PURPOSE:
!   Function to read CRTM RTSolution object files.
!
! CALLING SEQUENCE:
!   Error_Status = CRTM_RTSolution_ReadFile( Filename                , &
!                                            RTSolution              , &
!                                            Quiet      = Quiet      , &
!                                            n_Channels = n_Channels , &
!                                            n_Profiles = n_Profiles , &
!
! INPUTS:
!   Filename:     Character string specifying the name of an
!                 RTSolution format data file to read.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!   RTSolution:   CRTM RTSolution object array containing the RTSolution
!                 data.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                 ATTRIBUTES: INTENT(OUT), ALLOCATABLE
!
! OPTIONAL INPUTS:
!   Quiet:        Set this logical argument to suppress INFORMATION
!                 messages being printed to stdout
!                 If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                    == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                 If not specified, default is .FALSE.
!                 UNITS:      N/A
!                 TYPE:       LOGICAL
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!   n_Channels:   The number of channels for which data was read.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!   n_Profiles:   The number of profiles for which data was read.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!
! FUNCTION RESULT:
!   Error_Status: The return value is an integer defining the error status.
!                 The error codes are defined in the Message_Handler module.
!                 If == SUCCESS, the file read was successful
!                    == FAILURE, an unrecoverable error occurred.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_RTSolution_ReadFile( &
    Filename   , &  ! Input
    RTSolution , &  ! Output
    Quiet      , &  ! Optional input
    n_Channels , &  ! Optional output
    n_Profiles , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                            INTENT(IN)  :: Filename
    TYPE(CRTM_RTSolution_type), ALLOCATABLE, INTENT(OUT) :: RTSolution(:,:)  ! L x M
    LOGICAL,          OPTIONAL,              INTENT(IN)  :: Quiet
    INTEGER,          OPTIONAL,              INTENT(OUT) :: n_Channels
    INTEGER,          OPTIONAL,              INTENT(OUT) :: n_Profiles
    LOGICAL,          OPTIONAL,              INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(ML) :: alloc_msg
    INTEGER :: io_stat
    INTEGER :: alloc_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: l, n_input_channels
    INTEGER :: m, n_input_profiles


    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_input_channels, n_input_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the return structure array
   !ALLOCATE(RTSolution(n_input_channels, n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
    ALLOCATE(RTSolution(n_input_channels, n_input_profiles), STAT=alloc_stat)
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating RTSolution array - '//TRIM(alloc_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Loop over all the profiles and channels
    Profile_Loop: DO m = 1, n_input_profiles
      Channel_Loop: DO l = 1, n_input_channels
        err_stat = Read_Record( fid, RTSolution(l,m) )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error reading RTSolution element (",i0,",",i0,") from ",a)' ) &
                 l, m, TRIM(Filename)
          CALL Read_Cleanup(); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Channels) ) n_Channels = n_input_channels
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_input_profiles


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of channels and profiles read from ",a,": ",i0,1x,i0)' ) &
             TRIM(Filename), n_input_channels, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      IF ( ALLOCATED(RTSolution) ) THEN
       !DEALLOCATE(RTSolution, STAT=alloc_stat, ERRMSG=alloc_msg)
        DEALLOCATE(RTSolution, STAT=alloc_stat)
        IF ( alloc_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deallocating RTSolution array during error cleanup - '//&
                TRIM(alloc_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION CRTM_RTSolution_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CRTM_RTSolution_WriteFile
!
! PURPOSE:
!   Function to write CRTM RTSolution object files.
!
! CALLING SEQUENCE:
!   Error_Status = CRTM_RTSolution_WriteFile( Filename     , &
!                                             RTSolution   , &
!                                             Quiet = Quiet  )
!
! INPUTS:
!   Filename:     Character string specifying the name of the
!                 RTSolution format data file to write.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN)
!
!   RTSolution:   CRTM RTSolution object array containing the RTSolution
!                 data.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Quiet:        Set this logical argument to suppress INFORMATION
!                 messages being printed to stdout
!                 If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                    == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                 If not specified, default is .FALSE.
!                 UNITS:      N/A
!                 TYPE:       LOGICAL
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status: The return value is an integer defining the error status.
!                 The error codes are defined in the Message_Handler module.
!                 If == SUCCESS, the file write was successful
!                    == FAILURE, an unrecoverable error occurred.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
! SIDE EFFECTS:
!   - If the output file already exists, it is overwritten.
!   - If an error occurs during *writing*, the output file is deleted before
!     returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_RTSolution_WriteFile( &
    Filename   , &  ! Input
    RTSolution , &  ! Input
    Quiet      , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),               INTENT(IN) :: Filename
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: RTSolution(:,:)
    LOGICAL,          OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,          OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: l, n_output_channels
    INTEGER :: m, n_output_profiles

    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug
    n_output_channels = SIZE(RTSolution,DIM=1)
    n_output_profiles = SIZE(RTSolution,DIM=2)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_output_channels, n_output_profiles
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions to '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the data
    Profile_Loop: DO m = 1, n_output_profiles
      Channel_Loop: DO l = 1, n_output_channels
        err_stat = Write_Record( fid, RTSolution(l,m) )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error writing RTSolution element (",i0,",",i0,") to ",a)' ) &
                 l, m, TRIM(Filename)
          CALL Write_Cleanup(); RETURN
        END IF
      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of channels and profiles written to ",a,": ",i0,1x,i0 )' ) &
             TRIM(Filename), n_output_channels, n_output_profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_RTSolution_WriteFile



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!   CRTM_RTSolution_Equal
!
! PURPOSE:
!   Elemental function to test the equality of two CRTM_RTSolution objects.
!   Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!   is_equal = CRTM_RTSolution_Equal( x, y )
!
!     or
!
!   IF ( x == y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:          Two CRTM RTSolution objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       CRTM_RTSolution_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_equal:      Logical value indicating whether the inputs are equal.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_RTSolution_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Setup
    is_equal = .FALSE.

    ! Check the structure association status
    IF ( CRTM_RTSolution_Associated(x) .NEQV. CRTM_RTSolution_Associated(y) ) RETURN

    ! Check scalars
    IF ( (x%n_Layers == y%n_Layers) .AND. &
         (x%Sensor_ID         == y%Sensor_ID        ) .AND. &
         (x%WMO_Satellite_ID  == y%WMO_Satellite_ID ) .AND. &
         (x%WMO_Sensor_ID     == y%WMO_Sensor_ID    ) .AND. &
         (x%Sensor_Channel    == y%Sensor_Channel   ) .AND. &
         (x%RT_Algorithm_Name == y%RT_Algorithm_Name) .AND. &
         (x%SOD                     .EqualTo. y%SOD                    ) .AND. &
         (x%Surface_Emissivity      .EqualTo. y%Surface_Emissivity     ) .AND. &
         (x%Surface_Reflectivity    .EqualTo. y%Surface_Reflectivity   ) .AND. &
         (x%Up_Radiance             .EqualTo. y%Up_Radiance            ) .AND. &
         (x%Down_Radiance           .EqualTo. y%Down_Radiance          ) .AND. &
         (x%Down_Solar_Radiance     .EqualTo. y%Down_Solar_Radiance    ) .AND. &
         (x%Surface_Planck_Radiance .EqualTo. y%Surface_Planck_Radiance) .AND. &
         (x%Total_Cloud_Cover       .EqualTo. y%Total_Cloud_Cover      ) .AND. &
         (x%R_clear                 .EqualTo. y%R_clear                ) .AND. &
         (x%Tb_clear                .EqualTo. y%Tb_clear               ) .AND. &
         (x%Radiance                .EqualTo. y%Radiance               ) .AND. &
         (x%Brightness_Temperature  .EqualTo. y%Brightness_Temperature ) ) &
      is_equal = .TRUE.

    ! Check arrays (which may or may not be allocated)
    IF ( CRTM_RTSolution_Associated(x) .AND. CRTM_RTSolution_Associated(y) ) THEN
      is_equal = is_equal .AND. &
                 ALL(x%Upwelling_Overcast_Radiance .EqualTo. y%Upwelling_Overcast_Radiance ) .AND. &
                 ALL(x%Upwelling_Radiance          .EqualTo. y%Upwelling_Radiance          ) .AND. &
                 ALL(x%Layer_Optical_Depth         .EqualTo. y%Layer_Optical_Depth         )
    END IF

  END FUNCTION CRTM_RTSolution_Equal


!--------------------------------------------------------------------------------
!
! NAME:
!   CRTM_RTSolution_Add
!
! PURPOSE:
!   Pure function to add two CRTM RTSolution objects.
!   Used in OPERATOR(+) interface block.
!
! CALLING SEQUENCE:
!   rtssum = CRTM_RTSolution_Add( rts1, rts2 )
!
!     or
!
!   rtssum = rts1 + rts2
!
!
! INPUTS:
!   rts1, rts2: The RTSolution objects to add.
!               UNITS:      N/A
!               TYPE:       CRTM_RTSolution_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!   rtssum:     RTSolution object containing the summed components.
!               UNITS:      N/A
!               TYPE:       CRTM_RTSolution_type
!               DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Add( rts1, rts2 ) RESULT( rtssum )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: rts1, rts2
    TYPE(CRTM_RTSolution_type) :: rtssum
    INTEGER :: k

    ! Check input
    ! ...If input structure association status different, do nothing
    IF ( CRTM_RTSolution_Associated(rts1) .NEQV. CRTM_RTSolution_Associated(rts2) ) RETURN
    ! ...If input structure for different sensors, do nothing
    IF ( (rts1%Sensor_ID         /= rts2%Sensor_ID        ) .AND. &
         (rts1%WMO_Satellite_ID  /= rts2%WMO_Satellite_ID ) .AND. &
         (rts1%WMO_Sensor_ID     /= rts2%WMO_Sensor_ID    ) .AND. &
         (rts1%Sensor_Channel    /= rts2%Sensor_Channel   ) ) RETURN

    ! Copy the first structure
    rtssum = rts1

    ! And add the second one's components to it
    ! ...Handle RT_Algorithm_Name
    rtssum%RT_Algorithm_Name = 'Addition'
    ! ...The scalar values
    rtssum%SOD                     = rtssum%SOD                     + rts2%SOD
    rtssum%Surface_Emissivity      = rtssum%Surface_Emissivity      + rts2%Surface_Emissivity
    rtssum%Surface_Reflectivity    = rtssum%Surface_Reflectivity    + rts2%Surface_Reflectivity
    rtssum%Up_Radiance             = rtssum%Up_Radiance             + rts2%Up_Radiance
    rtssum%Down_Radiance           = rtssum%Down_Radiance           + rts2%Down_Radiance
    rtssum%Down_Solar_Radiance     = rtssum%Down_Solar_Radiance     + rts2%Down_Solar_Radiance
    rtssum%Surface_Planck_Radiance = rtssum%Surface_Planck_Radiance + rts2%Surface_Planck_Radiance
    rtssum%Total_Cloud_Cover       = rtssum%Total_Cloud_Cover       + rts2%Total_Cloud_Cover
    rtssum%R_clear                 = rtssum%R_clear                 + rts2%R_clear
    rtssum%Tb_clear                = rtssum%Tb_clear                + rts2%Tb_clear
    rtssum%Radiance                = rtssum%Radiance                + rts2%Radiance
    rtssum%Brightness_Temperature  = rtssum%Brightness_Temperature  + rts2%Brightness_Temperature
    ! ...The arrays (which may or may not be allocated)
    IF ( CRTM_RTSolution_Associated(rts1) .AND. CRTM_RTSolution_Associated(rts2) ) THEN
      k = rts1%n_Layers
      rtssum%Upwelling_Overcast_Radiance(1:k)  = rtssum%Upwelling_Overcast_Radiance(1:k)  + &
                                                   rts2%Upwelling_Overcast_Radiance(1:k)

      rtssum%Upwelling_Radiance(1:k) = rtssum%Upwelling_Radiance(1:k) + &
                                         rts2%Upwelling_Radiance(1:k)

      rtssum%Layer_Optical_Depth(1:k) = rtssum%Layer_Optical_Depth(1:k) + &
                                          rts2%Layer_Optical_Depth(1:k)
    END IF

  END FUNCTION CRTM_RTSolution_Add


!--------------------------------------------------------------------------------
!
! NAME:
!   CRTM_RTSolution_Subtract
!
! PURPOSE:
!   Pure function to subtract two CRTM RTSolution objects.
!   Used in OPERATOR(-) interface block.
!
! CALLING SEQUENCE:
!   rtsdiff = CRTM_RTSolution_Subtract( rts1, rts2 )
!
!     or
!
!   rtsdiff = rts1 - rts2
!
!
! INPUTS:
!   rts1, rts2: The RTSolution objects to difference.
!               UNITS:      N/A
!               TYPE:       CRTM_RTSolution_type
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!   rtsdiff:    RTSolution object containing the differenced components.
!               UNITS:      N/A
!               TYPE:       CRTM_RTSolution_type
!               DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Subtract( rts1, rts2 ) RESULT( rtsdiff )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: rts1, rts2
    TYPE(CRTM_RTSolution_type) :: rtsdiff
    INTEGER :: k

    ! Check input
    ! ...If input structure association status different, do nothing
    IF ( CRTM_RTSolution_Associated(rts1) .NEQV. CRTM_RTSolution_Associated(rts2) ) RETURN
    ! ...If input structure for different sensors, do nothing
    IF ( (rts1%Sensor_ID         /= rts2%Sensor_ID        ) .AND. &
         (rts1%WMO_Satellite_ID  /= rts2%WMO_Satellite_ID ) .AND. &
         (rts1%WMO_Sensor_ID     /= rts2%WMO_Sensor_ID    ) .AND. &
         (rts1%Sensor_Channel    /= rts2%Sensor_Channel   ) ) RETURN

    ! Copy the first structure
    rtsdiff = rts1

    ! And subtract the second one's components from it
    ! ...Handle RT_Algorithm_Name
    rtsdiff%RT_Algorithm_Name = 'Subtraction'
    ! ...The scalar values
    rtsdiff%SOD                     = rtsdiff%SOD                     - rts2%SOD
    rtsdiff%Surface_Emissivity      = rtsdiff%Surface_Emissivity      - rts2%Surface_Emissivity
    rtsdiff%Surface_Reflectivity    = rtsdiff%Surface_Reflectivity    - rts2%Surface_Reflectivity
    rtsdiff%Up_Radiance             = rtsdiff%Up_Radiance             - rts2%Up_Radiance
    rtsdiff%Down_Radiance           = rtsdiff%Down_Radiance           - rts2%Down_Radiance
    rtsdiff%Down_Solar_Radiance     = rtsdiff%Down_Solar_Radiance     - rts2%Down_Solar_Radiance
    rtsdiff%Surface_Planck_Radiance = rtsdiff%Surface_Planck_Radiance - rts2%Surface_Planck_Radiance
    rtsdiff%Total_Cloud_Cover       = rtsdiff%Total_Cloud_Cover       - rts2%Total_Cloud_Cover
    rtsdiff%R_clear                 = rtsdiff%R_clear                 - rts2%R_clear
    rtsdiff%Tb_clear                = rtsdiff%Tb_clear                - rts2%Tb_clear
    rtsdiff%Radiance                = rtsdiff%Radiance                - rts2%Radiance
    rtsdiff%Brightness_Temperature  = rtsdiff%Brightness_Temperature  - rts2%Brightness_Temperature
    ! ...The arrays (which may or may not be allocated)
    IF ( CRTM_RTSolution_Associated(rts1) .AND. CRTM_RTSolution_Associated(rts2) ) THEN
      k = rts1%n_Layers
      rtsdiff%Upwelling_Overcast_Radiance(1:k)  = rtsdiff%Upwelling_Overcast_Radiance(1:k)  - &
                                                     rts2%Upwelling_Overcast_Radiance(1:k)

      rtsdiff%Upwelling_Radiance(1:k) = rtsdiff%Upwelling_Radiance(1:k) - &
                                           rts2%Upwelling_Radiance(1:k)

      rtsdiff%Layer_Optical_Depth(1:k) = rtsdiff%Layer_Optical_Depth(1:k) - &
                                            rts2%Layer_Optical_Depth(1:k)
    END IF

  END FUNCTION CRTM_RTSolution_Subtract


!--------------------------------------------------------------------------------
!
! NAME:
!   CRTM_RTSolution_Exponent
!
! PURPOSE:
!   Elemental utility function to raise the components of a CRTM RTSolution
!   object to the supplied integer power
!
!   Used to compute RTSolution statistics
!
! CALLING SEQUENCE:
!   rts_power = CRTM_RTSolution_Exponent( rts, power )
!
! OBJECTS:
!   rts:          RTSolution structure which is to have its members values
!                 raised to an integer power.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Scalar or any rank
!                 ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!   power:        Exponent power to be used.
!                 UNITS:      N/A
!                 TYPE:       INRTGER
!                 DIMENSION:  Same as input rts object
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   rts_power:    RTSolution structure containing the exponent result.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Same as input rts object
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Exponent( rts, power ) RESULT( rts_power )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: rts
    INTEGER                   , INTENT(IN) :: power
    TYPE(CRTM_RTSolution_type) :: rts_power
    INTEGER :: k

    ! Copy the structure
    rts_power = rts

    ! Raise the components to the supplied power
    ! ...Handle RT_Algorithm_Name
    rts_power%RT_Algorithm_Name = 'Exponent'
    ! ...The scalar values
    rts_power%SOD                     = (rts_power%SOD                    )**power
    rts_power%Surface_Emissivity      = (rts_power%Surface_Emissivity     )**power
    rts_power%Surface_Reflectivity    = (rts_power%Surface_Reflectivity   )**power
    rts_power%Up_Radiance             = (rts_power%Up_Radiance            )**power
    rts_power%Down_Radiance           = (rts_power%Down_Radiance          )**power
    rts_power%Down_Solar_Radiance     = (rts_power%Down_Solar_Radiance    )**power
    rts_power%Surface_Planck_Radiance = (rts_power%Surface_Planck_Radiance)**power
    rts_power%Total_Cloud_Cover       = (rts_power%Total_Cloud_Cover      )**power
    rts_power%R_clear                 = (rts_power%R_clear                )**power
    rts_power%Tb_clear                = (rts_power%Tb_clear               )**power
    rts_power%Radiance                = (rts_power%Radiance               )**power
    rts_power%Brightness_Temperature  = (rts_power%Brightness_Temperature )**power
    ! ...The arrays (which may or may not be allocated)
    IF ( CRTM_RTSolution_Associated(rts) ) THEN
      k = rts%n_Layers
      rts_power%Upwelling_Overcast_Radiance(1:k) = (rts_power%Upwelling_Overcast_Radiance(1:k))**power
      rts_power%Upwelling_Radiance(1:k)          = (rts_power%Upwelling_Radiance(1:k)         )**power
      rts_power%Layer_Optical_Depth(1:k)         = (rts_power%Layer_Optical_Depth(1:k)        )**power
    END IF

  END FUNCTION CRTM_RTSolution_Exponent


!--------------------------------------------------------------------------------
!
! NAME:
!   CRTM_RTSolution_Normalise
!
! PURPOSE:
!   Elemental utility function to normalise the components of
!   a CRTM RTSolution object.
!
!   Used to compute RTSolution statistics
!
! CALLING SEQUENCE:
!   rts_normal = CRTM_RTSolution_Normalise( rts, factor )
!
! OBJECTS:
!   rts:          RTSolution structure which is to have its members values
!                 normalised.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Scalar or any rank
!                 ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!   factor:       Normalisation factor to be used.
!                 UNITS:      N/A
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Same as input rts object
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   rts_normal:   RTSolution structure containing the normalised input
!                 object.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Same as input rts object
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Normalise( rts, factor ) RESULT( rts_normal )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: rts
    REAL(fp)                  , INTENT(IN) :: factor
    TYPE(CRTM_RTSolution_type) :: rts_normal
    INTEGER :: k

    ! Copy the structure
    rts_normal = rts

    ! Raise the components to the supplied normal
    ! ...Handle RT_Algorithm_Name
    rts_normal%RT_Algorithm_Name = 'Normalise'
    ! ...The scalar values
    rts_normal%SOD                     = rts_normal%SOD                    /factor
    rts_normal%Surface_Emissivity      = rts_normal%Surface_Emissivity     /factor
    rts_normal%Surface_Reflectivity    = rts_normal%Surface_Reflectivity   /factor
    rts_normal%Up_Radiance             = rts_normal%Up_Radiance            /factor
    rts_normal%Down_Radiance           = rts_normal%Down_Radiance          /factor
    rts_normal%Down_Solar_Radiance     = rts_normal%Down_Solar_Radiance    /factor
    rts_normal%Surface_Planck_Radiance = rts_normal%Surface_Planck_Radiance/factor
    rts_normal%Total_Cloud_Cover       = rts_normal%Total_Cloud_Cover      /factor
    rts_normal%R_clear                 = rts_normal%R_clear                /factor
    rts_normal%Tb_clear                = rts_normal%Tb_clear               /factor
    rts_normal%Radiance                = rts_normal%Radiance               /factor
    rts_normal%Brightness_Temperature  = rts_normal%Brightness_Temperature /factor
    ! ...The arrays (which may or may not be allocated)
    IF ( CRTM_RTSolution_Associated(rts) ) THEN
      k = rts%n_Layers
      rts_normal%Upwelling_Overcast_Radiance(1:k) = rts_normal%Upwelling_Overcast_Radiance(1:k)/factor
      rts_normal%Upwelling_Radiance(1:k)          = rts_normal%Upwelling_Radiance(1:k)         /factor
      rts_normal%Layer_Optical_Depth(1:k)         = rts_normal%Layer_Optical_Depth(1:k)        /factor
    END IF

  END FUNCTION CRTM_RTSolution_Normalise


!--------------------------------------------------------------------------------
!
! NAME:
!   CRTM_RTSolution_Sqrt
!
! PURPOSE:
!   Elemental utility function to compute the SQRT() of the components of
!   a CRTM RTSolution object.
!
!   Used to compute RTSolution statistics
!
! CALLING SEQUENCE:
!   rts_sqrt = CRTM_RTSolution_Sqrt( rts )
!
! OBJECTS:
!   rts:          RTSolution structure which is to have the square root
!                 taken of its member's vales.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Scalar or any rank
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   rts_sqrt:     RTSolution structure containing the square root of the
!                 input object.
!                 UNITS:      N/A
!                 TYPE:       CRTM_RTSolution_type
!                 DIMENSION:  Same as input rts object
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Sqrt( rts ) RESULT( rts_sqrt )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: rts
    TYPE(CRTM_RTSolution_type) :: rts_sqrt
    INTEGER :: k

    ! Copy the structure
    rts_sqrt = rts

    ! Raise the components to the supplied normal
    ! ...Handle RT_Algorithm_Name
    rts_sqrt%RT_Algorithm_Name = 'Square root'
    ! ...The scalar values
    rts_sqrt%SOD                     = SQRT(rts_sqrt%SOD                    )
    rts_sqrt%Surface_Emissivity      = SQRT(rts_sqrt%Surface_Emissivity     )
    rts_sqrt%Surface_Reflectivity    = SQRT(rts_sqrt%Surface_Reflectivity   )
    rts_sqrt%Up_Radiance             = SQRT(rts_sqrt%Up_Radiance            )
    rts_sqrt%Down_Radiance           = SQRT(rts_sqrt%Down_Radiance          )
    rts_sqrt%Down_Solar_Radiance     = SQRT(rts_sqrt%Down_Solar_Radiance    )
    rts_sqrt%Surface_Planck_Radiance = SQRT(rts_sqrt%Surface_Planck_Radiance)
    rts_sqrt%Total_Cloud_Cover       = SQRT(rts_sqrt%Total_Cloud_Cover      )
    rts_sqrt%R_clear                 = SQRT(rts_sqrt%R_clear                )
    rts_sqrt%Tb_clear                = SQRT(rts_sqrt%Tb_clear               )
    rts_sqrt%Radiance                = SQRT(rts_sqrt%Radiance               )
    rts_sqrt%Brightness_Temperature  = SQRT(rts_sqrt%Brightness_Temperature )
    ! ...The arrays (which may or may not be allocated)
    IF ( CRTM_RTSolution_Associated(rts) ) THEN
      k = rts%n_Layers
      rts_sqrt%Upwelling_Overcast_Radiance(1:k) = SQRT(rts_sqrt%Upwelling_Overcast_Radiance(1:k))
      rts_sqrt%Upwelling_Radiance(1:k)          = SQRT(rts_sqrt%Upwelling_Radiance(1:k)         )
      rts_sqrt%Layer_Optical_Depth(1:k)         = SQRT(rts_sqrt%Layer_Optical_Depth(1:k)        )
    END IF

  END FUNCTION CRTM_RTSolution_Sqrt


!
! NAME:
!   Read_Record
!
! PURPOSE:
!   Utility function to read a single RTSolution data record
!

  FUNCTION Read_Record( &
    fid, &  ! Input
    rts) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN)  :: fid
    TYPE(CRTM_RTSolution_type), INTENT(OUT) :: rts
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: n_layers

    ! Set up
    err_stat = SUCCESS


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the RTSolution structure if necessary
    IF ( n_layers > 0 ) THEN
      CALL CRTM_RTSolution_Create( rts, n_layers )
      IF ( .NOT. CRTM_RTSolution_Associated( rts ) ) THEN
        msg = 'Error creating output object.'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Read the sensor info
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      rts%Sensor_Id       , &
      rts%WMO_Satellite_Id, &
      rts%WMO_Sensor_Id   , &
      rts%Sensor_Channel
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading sensor information - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the RT algorithm name
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      rts%RT_Algorithm_Name
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading RT Algorithm Name'//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Read the forward radiative transfer intermediate results
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      rts%SOD                    , &
      rts%Surface_Emissivity     , &
      rts%Surface_Reflectivity   , &
      rts%Up_Radiance            , &
      rts%Down_Radiance          , &
      rts%Down_Solar_Radiance    , &
      rts%Surface_Planck_Radiance, &
      rts%Total_Cloud_Cover      , &
      rts%R_clear                , &
      rts%Tb_clear
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading scalar intermediate results - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF
    IF ( n_Layers > 0 ) THEN
      READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
        rts%Upwelling_Overcast_Radiance , &
        rts%Upwelling_Radiance, &
        rts%Layer_Optical_Depth
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading array intermediate results - '//TRIM(io_msg)
        CALL Read_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Read the radiative transfer results
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      rts%Radiance              , &
      rts%Brightness_Temperature
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading result data - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_RTSolution_Destroy( rts )
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!
! NAME:
!   Write_Record
!
! PURPOSE:
!   Function to write a single RTSolution data record
!

  FUNCTION Write_Record( &
    fid, &  ! Input
    rts) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER,                    INTENT(IN) :: fid
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: rts
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_RTSolution_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Set up
    err_stat = SUCCESS


    ! Write the data dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) rts%n_Layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the sensor info
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      rts%Sensor_Id       , &
      rts%WMO_Satellite_Id, &
      rts%WMO_Sensor_Id   , &
      rts%Sensor_Channel
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing sensor information - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the RT algorithm name
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      rts%RT_Algorithm_Name
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing RT Algorithm Name'//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the forward radiative transfer intermediate results
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      rts%SOD                    , &
      rts%Surface_Emissivity     , &
      rts%Surface_Reflectivity   , &
      rts%Up_Radiance            , &
      rts%Down_Radiance          , &
      rts%Down_Solar_Radiance    , &
      rts%Surface_Planck_Radiance, &
      rts%Total_Cloud_Cover      , &
      rts%R_clear                , &
      rts%Tb_clear
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing scalar intermediate results - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF
    IF ( rts%n_Layers > 0 ) THEN
      WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
        rts%Upwelling_Overcast_Radiance , &
        rts%Upwelling_Radiance, &
        rts%Layer_Optical_Depth
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing array intermediate results - '//TRIM(io_msg)
        CALL Write_Record_Cleanup(); RETURN
      END IF
    END IF


    ! Write the radiative transfer results
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      rts%Radiance              , &
      rts%Brightness_Temperature
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing result data - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Write_Record_Cleanup()
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= 0 ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_Record_Cleanup

  END FUNCTION Write_Record

END MODULE CRTM_RTSolution_Define
