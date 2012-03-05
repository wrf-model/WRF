!
! BeCoeff_Define
!
! Module defining the geomagnetic field object and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, paul.vandelst@noaa.gov
!                       10-Feb-2010
!
!       Based on:       Zeeman_Utility.f90
!                       Yong Han, NOAA/JCSDA, yong.han@noaa.gov
!                       07-Nov-2007: Created
!                       24-Nov-2009: Modified for CRTM
!
!

MODULE BeCoeff_Define


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: BeCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: BeCoeff_Associated
  PUBLIC :: BeCoeff_Destroy
  PUBLIC :: BeCoeff_Create
  PUBLIC :: BeCoeff_Inspect
  PUBLIC :: BeCoeff_ValidRelease
  PUBLIC :: BeCoeff_Info
  PUBLIC :: BeCoeff_DefineVersion
  

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE BeCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: BeCoeff_Define.f90 6621 2010-02-12 21:51:13Z paul.vandelst@noaa.gov $'
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: BeCOEFF_RELEASE = 1  ! This determines object and file formats.
  INTEGER, PARAMETER :: BeCOEFF_VERSION = 1  ! This is just the data version for the release.
  ! Magnetic field components
  INTEGER,      PARAMETER :: MAX_N_COMPONENTS = 3
  CHARACTER(*), PARAMETER :: COMPONENT_NAME(MAX_N_COMPONENTS) = &
    (/ 'East  ', 'North ', 'Zenith' /)
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  ! Dimension vector limits
  REAL(fp), PARAMETER :: MIN_LATITUDE  = -90.0_fp
  REAL(fp), PARAMETER :: MAX_LATITUDE  =  90.0_fp
  REAL(fp), PARAMETER :: MIN_LONGITUDE = ZERO
  REAL(fp), PARAMETER :: MAX_LONGITUDE = 360.0_fp
  ! Meggage string length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------------
  ! BeCoeff data type definition
  ! --------------------------
  !:tdoc+:
  TYPE :: BeCoeff_type
    ! Release and version information
    INTEGER :: Release = BeCOEFF_RELEASE
    INTEGER :: Version = BeCOEFF_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimension values
    INTEGER :: n_Components = MAX_N_COMPONENTS  ! I dimension. Cartesian directions, X, Y, and Z
    INTEGER :: n_Latitudes  = 0                 ! J dimension.
    INTEGER :: n_Longitudes = 0                 ! K dimension.
    ! Look-up-table lat/long dimension vector deltas
    REAL(fp) :: d_Latitude  = ZERO
    REAL(fp) :: d_Longitude = ZERO
    ! Geomagnetic filed look-up-table
    REAL(fp), ALLOCATABLE :: LUT(:,:,:)  ! I x J x K. Units are milliGauss
  END TYPE BeCoeff_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a BeCoeff object.
!
! CALLING SEQUENCE:
!       Status = BeCoeff_Associated( BeCoeff )
!
! OBJECTS:
!       BeCoeff: BeCoeff object which is to have its member's
!                status tested.
!                UNITS:      N/A
!                TYPE:       TYPE(BeCoeff_type)
!                DIMENSION:  Scalar or any rank
!                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the BeCoeff members.
!                .TRUE.  - if ANY of the BeCoeff allocatable or
!                          pointer members are in use.
!                .FALSE. - if ALL of the BeCoeff allocatable or
!                          pointer members are not in use.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input BeCoeff argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION BeCoeff_Associated( BeCoeff ) RESULT( Status )
    TYPE(BeCoeff_type), INTENT(IN) :: BeCoeff
    LOGICAL :: Status
    Status = BeCoeff%Is_Allocated
  END FUNCTION BeCoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize BeCoeff objects.
!
! CALLING SEQUENCE:
!       CALL BeCoeff_Destroy( BeCoeff )
!
! OBJECTS:
!       BeCoeff:      Re-initialized BeCoeff object.
!                     UNITS:      N/A
!                     TYPE:       TYPE(BeCoeff_type)
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE BeCoeff_Destroy( BeCoeff )
    TYPE(BeCoeff_type), INTENT(OUT) :: BeCoeff
    BeCoeff%Is_Allocated = .FALSE.
    BeCoeff%n_Components = MAX_N_COMPONENTS
    BeCoeff%n_Latitudes  = 0
    BeCoeff%n_Longitudes = 0
  END SUBROUTINE BeCoeff_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the BeCoeff object.
!
! CALLING SEQUENCE:
!       CALL BeCoeff_Create( BeCoeff, n_Latitudes, n_Longitudes )
!
! OBJECTS:
!       BeCoeff:      BeCoeff object.
!                     UNITS:      N/A
!                     TYPE:       TYPE(BeCoeff_type)
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Latitudes:  Number of regularly-spaced latitudes from 90S to 90N for
!                     which there is geomagnetic field data. Must be >= 3. 
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar or same as BeCoeff object
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Longitudes: Number of regularly-spaced longitudes from 0 to 360E for
!                     which there is geomagnetic field data. Must be >= 3. 
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar or same as BeCoeff object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE BeCoeff_Create( &
    BeCoeff     , &
    n_Latitudes , &
    n_Longitudes  )
    ! Arguments
    TYPE(BeCoeff_type), INTENT(OUT) :: BeCoeff
    INTEGER,            INTENT(IN)  :: n_Latitudes 
    INTEGER,            INTENT(IN)  :: n_Longitudes
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Latitudes < 3 .OR. n_Longitudes < 3 ) RETURN

    ! Perform the allocation
    ALLOCATE( BeCoeff%LUT( MAX_N_COMPONENTS, n_Latitudes, n_Longitudes ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    BeCoeff%n_Components = MAX_N_COMPONENTS  ! Unnecessary belts'n'braces
    BeCoeff%n_Latitudes  = n_Latitudes 
    BeCoeff%n_Longitudes = n_Longitudes
    ! ...Scalars
    BeCoeff%d_Latitude  = Delta_Latitude( n_Latitudes )
    BeCoeff%d_Longitude = Delta_Longitude( n_Longitudes )
    ! ...Arrays
    BeCoeff%LUT = ZERO
    
    ! Set allocationindicator
    BeCoeff%Is_Allocated = .TRUE.
    
  END SUBROUTINE BeCoeff_Create

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a BeCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL BeCoeff_Inspect( BeCoeff )
!
! INPUTS:
!       BeCoeff:       BeCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       TYPE(BeCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE BeCoeff_Inspect( BeCoeff )
    TYPE(BeCoeff_type), INTENT(IN) :: BeCoeff
    INTEGER :: i
    WRITE(*,'(1x,"BeCoeff OBJECT")')
    WRITE(*,'(3x,"n_Latitudes      :",1x,i0)') BeCoeff%n_Latitudes 
    WRITE(*,'(3x,"n_Longitudes     :",1x,i0)') BeCoeff%n_Longitudes
    WRITE(*,'(3x,"d_Latitude       :",1x,es13.6)') BeCoeff%d_Latitude
    WRITE(*,'(3x,"d_Longitude      :",1x,es13.6)') BeCoeff%d_Longitude
    IF ( .NOT. BeCoeff_Associated(BeCoeff) ) RETURN
    DO i = 1, BeCoeff%n_Components
      WRITE(*,'(3x,a," component :")') COMPONENT_NAME(i)
      WRITE(*,'(5(1x,es13.6,:))') BeCoeff%LUT(i,:,:)
    END DO
  END SUBROUTINE BeCoeff_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the BeCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = BeCoeff_ValidRelease( BeCoeff )
!
! INPUTS:
!       BeCoeff:       BeCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(BeCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION BeCoeff_ValidRelease( BeCoeff ) RESULT( IsValid )
    ! Arguments
    TYPE(BeCoeff_type), INTENT(IN) :: BeCoeff
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'BeCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( BeCoeff%Release < BeCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A BeCoeff data update is needed. ", &
                  &"BeCoeff release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  BeCoeff%Release, BeCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( BeCoeff%Release > BeCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A BeCoeff software update is needed. ", &
                  &"BeCoeff release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  BeCoeff%Release, BeCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION BeCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a BeCoeff object.
!
! CALLING SEQUENCE:
!       CALL BeCoeff_Info( BeCoeff, Info )
!
! INPUTS:
!       BeCoeff:       BeCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       TYPE(BeCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed BeCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE BeCoeff_Info( BeCoeff, Info )
    ! Arguments
    TYPE(BeCoeff_type), INTENT(IN)  :: BeCoeff
    CHARACTER(*),       INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
   WRITE( Long_String, &
          '(a,1x,"BeCoeff RELEASE.VERSION: ",i2,".",i2.2,2x,&
          &"N_LATITUDES=",i0,2x,&
          &"N_LONGITUDES=",i0 )' ) &
          ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
          BeCoeff%Release, BeCoeff%Version, &
          BeCoeff%n_Latitudes , &
          BeCoeff%n_Longitudes
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE BeCoeff_Info
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       BeCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL BeCoeff_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE BeCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE BeCoeff_DefineVersion


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
!       BeCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two BeCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = BeCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two BeCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(BeCoeff_type)
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION BeCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(BeCoeff_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
    
    ! Check the object association status
    IF ( (.NOT. BeCoeff_Associated(x)) .OR. &
         (.NOT. BeCoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (x%n_Components /= y%n_Components) .OR. &
         (x%n_Latitudes  /= y%n_Latitudes ) .OR. &
         (x%n_Longitudes /= y%n_Longitudes) ) RETURN
    ! ...Data
    IF ( (x%d_Latitude  .EqualTo. y%d_Latitude ) .AND. &
         (x%d_Longitude .EqualTo. y%d_Longitude) .AND. &
         ALL(x%LUT .EqualTo. y%LUT) ) is_equal = .TRUE.

  END FUNCTION BeCoeff_Equal


  !---------------------------
  ! General utility procedures
  !---------------------------
  
  ELEMENTAL FUNCTION Delta_Latitude( n ) RESULT( d_Latitude )
    INTEGER, INTENT(IN) :: n
    REAL(fp) :: d_Latitude
    d_Latitude = (MAX_LATITUDE-MIN_LATITUDE)/REAL(n-1,fp)
  END FUNCTION Delta_Latitude

  ELEMENTAL FUNCTION Delta_Longitude( n ) RESULT( d_Longitude )
    INTEGER, INTENT(IN) :: n
    REAL(fp) :: d_Longitude
    d_Longitude = (MAX_LONGITUDE-MIN_LONGITUDE)/REAL(n-1,fp)
  END FUNCTION Delta_Longitude

  

END MODULE BeCoeff_Define
