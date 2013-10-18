!
! MWwaterCoeff_Define
!
! Module defining the MWwaterCoeff object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 10-Nov-2011
!                       paul.vandelst@noaa.gov

MODULE MWwaterCoeff_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  USE FitCoeff_Define
  USE MWwaterLUT_Define
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: MWwaterCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: MWwaterCoeff_Associated
  PUBLIC :: MWwaterCoeff_Destroy
  PUBLIC :: MWwaterCoeff_Create
  PUBLIC :: MWwaterCoeff_Inspect
  PUBLIC :: MWwaterCoeff_ValidRelease
  PUBLIC :: MWwaterCoeff_Info
  PUBLIC :: MWwaterCoeff_DefineVersion
  PUBLIC :: MWwaterCoeff_ReadFile
  PUBLIC :: MWwaterCoeff_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE MWwaterCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: MWwaterCoeff_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: MWWATERCOEFF_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: MWWATERCOEFF_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Data indicators
  INTEGER, PARAMETER :: DATA_MISSING = 0
  INTEGER, PARAMETER :: DATA_PRESENT = 1
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL =  80 ! String length


  ! ---------------------------------
  ! MWwaterCoeff data type definition
  ! ---------------------------------
  !:tdoc+:
  TYPE :: MWwaterCoeff_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = MWWATERCOEFF_RELEASE
    INTEGER(Long) :: Version = MWWATERCOEFF_VERSION
    ! Derived type components
    TYPE(FitCoeff_1D_type) :: FCCoeff     ! Foam coverage          fitting coefficients
    TYPE(FitCoeff_1D_type) :: FRCoeff     ! Foam reflectivity      fitting coefficients
    TYPE(FitCoeff_3D_type) :: RCCoeff     ! Reflection correction  fitting coefficients
    TYPE(FitCoeff_3D_type) :: AZCoeff     ! Azimuth emissivity     fitting coefficients
    TYPE(FitCoeff_1D_type) :: SSCCoeff    ! Small-scale correction fitting coefficients
    TYPE(FitCoeff_3D_type) :: LSCCoeff    ! Large-scale correction fitting coefficients
    TYPE(MWwaterLUT_type)  :: LUT         ! Emissivity look-up table
  END TYPE MWwaterCoeff_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                           ## PUBLIC PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_Associated
!
! PURPOSE:
!       Pure function to test the status of the allocatable components
!       of the MWwaterCoeff structure.
!
! CALLING SEQUENCE:
!       Status = MWwaterCoeff_Associated( MWwaterCoeff )
!
! OBJECTS:
!       MWwaterCoeff:  Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       MWwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the components.
!                       .TRUE.  - if ALL of the MWwaterCoeff allocatable members
!                                 are in use.
!                       .FALSE. - if ANY of the MWwaterCoeff allocatable members
!                                 are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION MWwaterCoeff_Associated( self ) RESULT( Status )
    TYPE(MWwaterCoeff_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION MWwaterCoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_Destroy
!
! PURPOSE:
!       Pure subroutine to re-initialize MWwaterCoeff objects.
!
! CALLING SEQUENCE:
!       CALL MWwaterCoeff_Destroy( MWwaterCoeff )
!
! OBJECTS:
!       MWwaterCoeff: Re-initialized MWwaterCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       MWwaterCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE MWwaterCoeff_Destroy( self )
    TYPE(MWwaterCoeff_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE MWwaterCoeff_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_Create
!
! PURPOSE:
!       Pure subroutine to create a valid instance of an MWwaterCoeff object.
!
! CALLING SEQUENCE:
!       CALL MWwaterCoeff_Create( MWwaterCoeff )
!
! OBJECTS:
!       MWwaterCoeff:       MWwaterCoeff object structure.
!                           UNITS:      N/A
!                           TYPE:       MWwaterCoeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE MWwaterCoeff_Create( &
    self )  ! Output
    ! Arguments
    TYPE(MWwaterCoeff_type), INTENT(IN OUT) :: self
    ! Set allocation indicator
    self%Is_Allocated = .TRUE.
  END SUBROUTINE MWwaterCoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a MWwaterCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL MWwaterCoeff_Inspect( MWwaterCoeff )
!
! OBJECTS:
!       MWwaterCoeff:  MWwaterCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       MWwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWwaterCoeff_Inspect( self, pause )
    TYPE(MWwaterCoeff_type), INTENT(IN) :: self
    LOGICAL,       OPTIONAL, INTENT(IN) :: pause
    LOGICAL :: wait

    wait = .FALSE.
    IF ( PRESENT(pause) ) wait = pause

    WRITE(*,'(1x,"MWwaterCoeff OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version : ",i0,".",i0)') self%Release, self%Version
    IF ( .NOT. MWwaterCoeff_Associated(self) ) RETURN
    ! Derived types
    IF ( FitCoeff_Associated(self%FCCoeff ) ) THEN
      WRITE(*,'(1x,"Foam coverage cofficients, ")',ADVANCE='NO')
      CALL FitCoeff_Inspect(self%FCCoeff )
    END IF
    IF ( FitCoeff_Associated(self%FRCoeff ) ) THEN
      WRITE(*,'(1x,"Foam reflectivity cofficients, ")',ADVANCE='NO')
      CALL FitCoeff_Inspect(self%FRCoeff )
    END IF
    IF ( FitCoeff_Associated(self%RCCoeff) ) THEN
      WRITE(*,'(1x,"Reflection correction cofficients, ")',ADVANCE='NO')
      CALL FitCoeff_Inspect(self%RCCoeff)
    END IF
    IF ( FitCoeff_Associated(self%AZCoeff) ) THEN
      WRITE(*,'(1x,"Azimuth emissivity coefficients, ")',ADVANCE='NO')
      CALL FitCoeff_Inspect(self%AZCoeff)
    END IF
    IF ( FitCoeff_Associated(self%SSCCoeff) ) THEN
      WRITE(*,'(1x,"Small-scale correction coefficients, ")',ADVANCE='NO')
      CALL FitCoeff_Inspect(self%SSCCoeff)
    END IF
    IF ( FitCoeff_Associated(self%LSCCoeff) ) THEN
      WRITE(*,'(1x,"Large-scale correction coefficients, ")',ADVANCE='NO')
      CALL FitCoeff_Inspect(self%LSCCoeff)
    END IF
    IF ( MWwaterLUT_Associated(self%LUT) ) THEN
      WRITE(*,'(1x,"Emissivity look-up table, ")',ADVANCE='NO')
      CALL MWwaterLUT_Inspect(self%LUT,pause=pause)
    END IF
  END SUBROUTINE MWwaterCoeff_Inspect



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the MWwaterCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = MWwaterCoeff_ValidRelease( MWwaterCoeff )
!
! INPUTS:
!       MWwaterCoeff:  MWwaterCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       MWwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION MWwaterCoeff_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(MWwaterCoeff_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWwaterCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < MWWATERCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An MWwaterCoeff data update is needed. ", &
                  &"MWwaterCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, MWWATERCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > MWWATERCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An MWwaterCoeff software update is needed. ", &
                  &"MWwaterCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, MWWATERCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION MWwaterCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a MWwaterCoeff object.
!
! CALLING SEQUENCE:
!       CALL MWwaterCoeff_Info( MWwaterCoeff, Info )
!
! OBJECTS:
!       MWwaterCoeff:  MWwaterCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       MWwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the MWwaterCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWwaterCoeff_Info( self, Info )
    ! Arguments
    TYPE(MWwaterCoeff_type), INTENT(IN)  :: self
    CHARACTER(*),            INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"MWwaterCoeff RELEASE.VERSION: ",i0,".",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE MWwaterCoeff_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL MWwaterCoeff_DefineVersion( Id )
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

  SUBROUTINE MWwaterCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE MWwaterCoeff_DefineVersion


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_SetValue
!
! PURPOSE:
!       Subroutine to set the contents of a valid MWwaterCoeff object.
!
! CALLING SEQUENCE:
!       CALL MWwaterCoeff_SetValue( MWwaterCoeff, &
!                                   FCCoeff    = FCCoeff   , &
!                                   FRCoeff    = FRCoeff   , &
!                                   RCCoeff    = RCCoeff   , &
!                                   AZCoeff    = AZCoeff   , &
!                                   SSCCoeff   = SSCCoeff  , &
!                                   LSCCoeff   = LSCCoeff  , &
!                                   MWwaterLUT = MWwaterLUT  )
! OBJECTS:
!       MWwaterCoeff:  Valid, allocated MWwaterCoeff object for which
!                      values are to be set.
!                      UNITS:      N/A
!                      TYPE:       MWwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       FCCoeff:       Object containing the foam coverage fitting coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_1D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       FRCoeff:       Object containing the foam reflectivity fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_1D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       RCCoeff:       Object containing the reflection correction fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_3D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       AZCoeff:       Object containing the azimuth emissivity fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_3D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       SSCCoeff:      Object containing the small-scale correction fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_1D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       LSCCoeff:      Object containing the large-scale correction fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_3D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       MWwaterLUT:    Object containing the emissivity look-up table.
!                      UNITS:      N/A
!                      TYPE:       MWwaterLUT_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWwaterCoeff_SetValue( &
    self      , &  ! In/Output
    FCCoeff   , &  ! Optional input
    FRCoeff   , &  ! Optional input
    RCCoeff   , &  ! Optional input
    AZCoeff   , &  ! Optional input
    SSCCoeff  , &  ! Optional input
    LSCCoeff  , &  ! Optional input
    MWwaterLUT  )  ! Optional input
    ! Arguments
    TYPE(MWwaterCoeff_type)         , INTENT(IN OUT) :: self
    TYPE(FitCoeff_1D_type), OPTIONAL, INTENT(IN)     :: FCCoeff
    TYPE(FitCoeff_1D_type), OPTIONAL, INTENT(IN)     :: FRCoeff
    TYPE(FitCoeff_3D_type), OPTIONAL, INTENT(IN)     :: RCCoeff
    TYPE(FitCoeff_3D_type), OPTIONAL, INTENT(IN)     :: AZCoeff
    TYPE(FitCoeff_1D_type), OPTIONAL, INTENT(IN)     :: SSCCoeff
    TYPE(FitCoeff_3D_type), OPTIONAL, INTENT(IN)     :: LSCCoeff
    TYPE(MWwaterLUT_type) , OPTIONAL, INTENT(IN)     :: MWwaterLUT

    IF ( PRESENT(FCCoeff   ) ) self%FCCoeff  = FCCoeff
    IF ( PRESENT(FRCoeff   ) ) self%FRCoeff  = FRCoeff
    IF ( PRESENT(RCCoeff   ) ) self%RCCoeff  = RCCoeff
    IF ( PRESENT(AZCoeff   ) ) self%AZCoeff  = AZCoeff
    IF ( PRESENT(SSCCoeff  ) ) self%SSCCoeff = SSCCoeff
    IF ( PRESENT(LSCCoeff  ) ) self%LSCCoeff = LSCCoeff
    IF ( PRESENT(MWwaterLUT) ) self%LUT      = MWwaterLUT

  END SUBROUTINE MWwaterCoeff_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_GetValue
!
! PURPOSE:
!       Subroutine to get the contents of a valid MWwaterCoeff object.
!
! CALLING SEQUENCE:
!       CALL MWwaterCoeff_GetValue( MWwaterCoeff, &
!                                   FCCoeff    = FCCoeff   , &
!                                   FRCoeff    = FRCoeff   , &
!                                   RCCoeff    = RCCoeff   , &
!                                   AZCoeff    = AZCoeff   , &
!                                   SSCCoeff   = SSCCoeff  , &
!                                   LSCCoeff   = LSCCoeff  , &
!                                   MWwaterLUT = MWwaterLUT  )
!
! OBJECTS:
!       MWwaterCoeff:  Valid, allocated MWwaterCoeff object from which
!                      values are to be retrieved.
!                      UNITS:      N/A
!                      TYPE:       MWwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUTS:
!       FCCoeff:       Object containing the foam coverage fitting coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_1D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       FRCoeff:       Object containing the foam reflectivity fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_1D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCCoeff:       Object containing the reflection correction fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_3D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       AZCoeff:       Object containing the azimuth emissivity fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_3D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       SSCCoeff:      Object containing the small-scale correction fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_1D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       LSCCoeff:      Object containing the large-scale correction fitting
!                      coefficients.
!                      UNITS:      N/A
!                      TYPE:       FitCoeff_3D_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       MWwaterLUT:    Object containing the emissivity look-up table.
!                      UNITS:      N/A
!                      TYPE:       MWwaterLUT_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWwaterCoeff_GetValue( &
    self      , &  ! Input
    FCCoeff   , &  ! Optional output
    FRCoeff   , &  ! Optional output
    RCCoeff   , &  ! Optional output
    AZCoeff   , &  ! Optional output
    SSCCoeff  , &  ! Optional output
    LSCCoeff  , &  ! Optional output
    MWwaterLUT  )  ! Optional output
    ! Arguments
    TYPE(MWwaterCoeff_type)         , INTENT(IN)  :: self
    TYPE(FitCoeff_1D_type), OPTIONAL, INTENT(OUT) :: FCCoeff
    TYPE(FitCoeff_1D_type), OPTIONAL, INTENT(OUT) :: FRCoeff
    TYPE(FitCoeff_3D_type), OPTIONAL, INTENT(OUT) :: RCCoeff
    TYPE(FitCoeff_3D_type), OPTIONAL, INTENT(OUT) :: AZCoeff
    TYPE(FitCoeff_1D_type), OPTIONAL, INTENT(OUT) :: SSCCoeff
    TYPE(FitCoeff_3D_type), OPTIONAL, INTENT(OUT) :: LSCCoeff
    TYPE(MWwaterLUT_type) , OPTIONAL, INTENT(OUT) :: MWwaterLUT

    IF ( PRESENT(FCCoeff   ) ) FCCoeff    = self%FCCoeff
    IF ( PRESENT(FRCoeff   ) ) FRCoeff    = self%FRCoeff
    IF ( PRESENT(RCCoeff   ) ) RCCoeff    = self%RCCoeff
    IF ( PRESENT(AZCoeff   ) ) AZCoeff    = self%AZCoeff
    IF ( PRESENT(SSCCoeff  ) ) SSCCoeff   = self%SSCCoeff
    IF ( PRESENT(LSCCoeff  ) ) LSCCoeff   = self%LSCCoeff
    IF ( PRESENT(MWwaterLUT) ) MWwaterLUT = self%LUT

  END SUBROUTINE MWwaterCoeff_GetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_ReadFile
!
! PURPOSE:
!       Function to read MWwaterCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = MWwaterCoeff_ReadFile( &
!                        MWwaterCoeff       , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet   , &
!                        Title    = Title   , &
!                        History  = History , &
!                        Comment  = Comment   )
!
! OBJECTS:
!       MWwaterCoeff:   MWwaterCoeff object containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       MWwaterCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       MWwaterCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the MWwaterCoeff data is embedded within another file.
!                       If == .FALSE., File is closed upon function exit [DEFAULT].
!                          == .TRUE.,  File is NOT closed upon function exit
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Title:          Character string containing a succinct description
!                       of what is in the dataset.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string containing dataset creation
!                       history.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string containing any comments about
!                       the dataset.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file read was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION MWwaterCoeff_ReadFile( &
    MWwaterCoeff, &  ! Output
    Filename    , &  ! Input
    No_Close    , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     , &  ! Optional output
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(MWwaterCoeff_type), INTENT(OUT) :: MWwaterCoeff
    CHARACTER(*),            INTENT(IN)  :: Filename
    LOGICAL     ,  OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL     ,  OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL     ,  OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWwaterCoeff_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER(Long) :: lut_present
    TYPE(MWwaterCoeff_type) :: dummy

    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Check if the file is open.
    IF ( File_Open( Filename ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Read_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file if it exists
      IF ( File_Exists( Filename ) ) THEN
        err_stat = Open_Binary_File( Filename, fid )
        IF ( err_Stat /= SUCCESS ) THEN
          msg = 'Error opening '//TRIM(Filename)
          CALL Read_CleanUp(); RETURN
        END IF
      ELSE
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF


    ! Read and check the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%Release, &
      dummy%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. MWwaterCoeff_ValidRelease( dummy ) ) THEN
      msg = 'MWwaterCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Create instance of the structure
    CALL MWwaterCoeff_Create(MWwaterCoeff)
    ! ...Explicitly set the version
    MWwaterCoeff%Version = dummy%Version


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the coefficient data
    ! ...Read the foam coverage coefficients
    err_stat = FitCoeff_ReadFile( &
                 MWwaterCoeff%FCCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading foam coverage coefficient data.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the foam reflectivity coefficients
    err_stat = FitCoeff_ReadFile( &
                 MWwaterCoeff%FRCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading foam reflectivity coefficient data.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the reflection correction coefficients
    err_stat = FitCoeff_ReadFile( &
                 MWwaterCoeff%RCCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading reflection correction coefficient data'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the azimuth emissivity coefficients
    err_stat = FitCoeff_ReadFile( &
                 MWwaterCoeff%AZCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading azimuth emissivity coefficient data.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the small-scale correction coefficients
    err_stat = FitCoeff_ReadFile( &
                 MWwaterCoeff%SSCCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading small-scale correction coefficient data.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the large-scale correction coefficients
    err_stat = FitCoeff_ReadFile( &
                 MWwaterCoeff%LSCCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading large-scale correction coefficient data.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the emissivity look-up table if it's present
    ! ...Read the data indicator
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) lut_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading emissivity look-up table data indicator - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the LUT data
    IF ( lut_present == DATA_PRESENT ) THEN
      err_stat = MWwaterLUT_ReadFile( &
                   MWwaterCoeff%LUT, &
                   Filename, &
                   No_Close = .TRUE., &
                   Quiet    = Quiet , &
                   Debug    = Debug   )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading emissivity look-up table data.'
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL MWwaterCoeff_Info( MWwaterCoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL MWwaterCoeff_Destroy( MWwaterCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION MWwaterCoeff_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterCoeff_WriteFile
!
! PURPOSE:
!       Function to write MWwaterCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = MWwaterCoeff_WriteFile( &
!                        MWwaterCoeff       , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet   , &
!                        Title    = Title   , &
!                        History  = History , &
!                        Comment  = Comment   )
!
! OBJECTS:
!       MWwaterCoeff:   MWwaterCoeff object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       MWwaterCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       MWwaterCoeff format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the MWwaterCoeff data is to be embedded within another file.
!                       If == .FALSE., File is closed upon function exit [DEFAULT].
!                          == .TRUE.,  File is NOT closed upon function exit
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string containing a succinct description
!                       of what is in the dataset.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string containing dataset creation
!                       history.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string containing any comments about
!                       the dataset.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION MWwaterCoeff_WriteFile( &
    MWwaterCoeff, &  ! Input
    Filename    , &  ! Input
    No_Close    , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     , &  ! Optional input
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(MWwaterCoeff_type), INTENT(IN) :: MWwaterCoeff
    CHARACTER(*),            INTENT(IN) :: Filename
    LOGICAL     ,  OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL     ,  OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: History
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Comment
    LOGICAL     ,  OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWwaterCoeff_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER(Long) :: lut_present


    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF
    ! ...Check there is data to write
    IF ( .NOT. MWwaterCoeff_Associated( MWwaterCoeff ) ) THEN
      msg = 'MWwaterCoeff object is empty.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Check if the file is open.
    IF ( File_Open( FileName ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Write_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file for output
      err_stat = Open_Binary_File( Filename, fid, For_Output=.TRUE. )
      IF ( err_Stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_CleanUp(); RETURN
      END IF
    END IF


    ! Write the release and version
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MWwaterCoeff%Release, &
      MWwaterCoeff%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts_Binary_File( &
                 fid, &
                 Write_Module = MODULE_VERSION_ID, &
                 Title        = Title  , &
                 History      = History, &
                 Comment      = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the coefficient data
    ! ...Write the foam coverage coefficients
    err_stat = FitCoeff_WriteFile( &
                 MWwaterCoeff%FCCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing foam coverage coefficient data.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the foam reflectivity coefficients
    err_stat = FitCoeff_WriteFile( &
                 MWwaterCoeff%FRCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing foam reflectivity coefficient data.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the reflection correction coefficients
    err_stat = FitCoeff_WriteFile( &
                 MWwaterCoeff%RCCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing reflection correction coefficient data'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the azimuth emissivity coefficients
    err_stat = FitCoeff_WriteFile( &
                 MWwaterCoeff%AZCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing azimuth emissivity coefficient data.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the small-scale correction coefficients
    err_stat = FitCoeff_WriteFile( &
                 MWwaterCoeff%SSCCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing small-scale correction coefficient data.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the large-scale correction coefficients
    err_stat = FitCoeff_WriteFile( &
                 MWwaterCoeff%LSCCoeff, &
                 Filename, &
                 No_Close = .TRUE., &
                 Quiet    = Quiet , &
                 Debug    = Debug   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing large-scale correction coefficient data.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the emissivity look-up table if it's present
    IF ( MWwaterLUT_Associated( MWwaterCoeff%LUT ) ) THEN
      lut_present = DATA_PRESENT
    ELSE
      lut_present = DATA_MISSING
    END IF
    ! ...Write the data indicator
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) lut_present
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing emissivity look-up table data indicator - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the LUT data
    IF ( lut_present == DATA_PRESENT ) THEN
      err_stat = MWwaterLUT_WriteFile( &
                   MWwaterCoeff%LUT, &
                   Filename, &
                   No_Close = .TRUE., &
                   Quiet    = Quiet , &
                   Debug    = Debug   )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing emissivity look-up table data.'
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL MWwaterCoeff_Info( MWwaterCoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_Cleanup()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid, STATUS=WRITE_ERROR_STATUS, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_Cleanup

  END FUNCTION MWwaterCoeff_WriteFile


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       MWwaterCoeff_Equal
!
! PURPOSE:
!       Pure function to test the equality of two MWwaterCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = MWwaterCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two MWwaterCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       MWwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  PURE FUNCTION MWwaterCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(MWwaterCoeff_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( (.NOT. MWwaterCoeff_Associated(x)) .OR. &
         (.NOT. MWwaterCoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Structures
    IF ( FitCoeff_Associated( x%FCCoeff ) .NEQV. FitCoeff_Associated( x%FCCoeff ) ) RETURN
    IF ( FitCoeff_Associated( x%FCCoeff ) .AND.  FitCoeff_Associated( x%FCCoeff ) ) THEN
      IF ( .NOT. (x%FCCoeff == y%FCCoeff) ) RETURN
    END IF
    IF ( FitCoeff_Associated( x%FRCoeff ) .NEQV. FitCoeff_Associated( x%FRCoeff ) ) RETURN
    IF ( FitCoeff_Associated( x%FRCoeff ) .AND.  FitCoeff_Associated( x%FRCoeff ) ) THEN
      IF ( .NOT. (x%FRCoeff == y%FRCoeff) ) RETURN
    END IF
    IF ( FitCoeff_Associated( x%RCCoeff ) .NEQV. FitCoeff_Associated( x%RCCoeff ) ) RETURN
    IF ( FitCoeff_Associated( x%RCCoeff ) .AND.  FitCoeff_Associated( x%RCCoeff ) ) THEN
      IF ( .NOT. (x%RCCoeff == y%RCCoeff) ) RETURN
    END IF
    IF ( FitCoeff_Associated( x%AZCoeff ) .NEQV. FitCoeff_Associated( x%AZCoeff ) ) RETURN
    IF ( FitCoeff_Associated( x%AZCoeff ) .AND.  FitCoeff_Associated( x%AZCoeff ) ) THEN
      IF ( .NOT. (x%AZCoeff == y%AZCoeff) ) RETURN
    END IF
    IF ( FitCoeff_Associated( x%SSCCoeff ) .NEQV. FitCoeff_Associated( x%SSCCoeff ) ) RETURN
    IF ( FitCoeff_Associated( x%SSCCoeff ) .AND.  FitCoeff_Associated( x%SSCCoeff ) ) THEN
      IF ( .NOT. (x%SSCCoeff == y%SSCCoeff) ) RETURN
    END IF
    IF ( FitCoeff_Associated( x%LSCCoeff ) .NEQV. FitCoeff_Associated( x%LSCCoeff ) ) RETURN
    IF ( FitCoeff_Associated( x%LSCCoeff ) .AND.  FitCoeff_Associated( x%LSCCoeff ) ) THEN
      IF ( .NOT. (x%LSCCoeff == y%LSCCoeff) ) RETURN
    END IF
    IF ( MWwaterLUT_Associated( x%LUT ) .NEQV. MWwaterLUT_Associated( x%LUT ) ) RETURN
    IF ( MWwaterLUT_Associated( x%LUT ) .AND.  MWwaterLUT_Associated( x%LUT ) ) THEN
      IF ( .NOT. (x%LUT == y%LUT) ) RETURN
    END IF

    ! If we get here everything is equal
    is_equal = .TRUE.

  END FUNCTION MWwaterCoeff_Equal

END MODULE MWwaterCoeff_Define
