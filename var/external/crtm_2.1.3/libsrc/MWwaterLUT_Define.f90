!
! MWwaterLUT_Define
!
! Module defining the MWwaterLUT object containing the
! Look-Up Table (LUT) for the microWave (MW) sea surface emissivity
! model.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 10-Nov-2011
!                       paul.vandelst@noaa.gov
!

MODULE MWwaterLUT_Define

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
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: MWwaterLUT_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: MWwaterLUT_Associated
  PUBLIC :: MWwaterLUT_Destroy
  PUBLIC :: MWwaterLUT_Create
  PUBLIC :: MWwaterLUT_Inspect
  PUBLIC :: MWwaterLUT_ValidRelease
  PUBLIC :: MWwaterLUT_Info
  PUBLIC :: MWwaterLUT_DefineVersion
  PUBLIC :: MWwaterLUT_InquireFile
  PUBLIC :: MWwaterLUT_ReadFile
  PUBLIC :: MWwaterLUT_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE MWwaterLUT_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: MWwaterLUT_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: MWWATERLUT_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: MWWATERLUT_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL =  80 ! String length


  ! ----------------------------------
  ! MWwaterLUT data type definitions
  ! ----------------------------------
  !:tdoc+:
  TYPE :: MWwaterLUT_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = MWwaterLUT_RELEASE
    INTEGER(Long) :: Version = MWwaterLUT_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Angles       = 0   ! I1 dimension
    INTEGER(Long) :: n_Frequencies  = 0   ! I2 dimension
    INTEGER(Long) :: n_Temperatures = 0   ! I3 dimension
    INTEGER(Long) :: n_Wind_Speeds  = 0   ! I4 dimension
    ! Dimensional vectors
    REAL(Double),  ALLOCATABLE :: Angle(:)        ! I1
    REAL(Double),  ALLOCATABLE :: Frequency(:)    ! I2
    REAL(Double),  ALLOCATABLE :: Temperature(:)  ! I3
    REAL(Double),  ALLOCATABLE :: Wind_Speed(:)   ! I4
    ! Large-scale correction emissivity data
    REAL(Double),  ALLOCATABLE :: ev(:,:,:,:)  ! I1 x I2 x I3 x I4
    REAL(Double),  ALLOCATABLE :: eh(:,:,:,:)  ! I1 x I2 x I3 x I4
  END TYPE MWwaterLUT_type
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
!       MWwaterLUT_Associated
!
! PURPOSE:
!       Pure function to test the status of the allocatable components
!       of the MWwaterLUT structure.
!
! CALLING SEQUENCE:
!       Status = MWwaterLUT_Associated( MWwaterLUT )
!
! OBJECTS:
!       MWwaterLUT:    Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       MWwaterLUT_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the components.
!                       .TRUE.  - if ANY of the MWwaterLUT allocatable members
!                                 are in use.
!                       .FALSE. - if ALL of the MWwaterLUT allocatable members
!                                 are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION MWwaterLUT_Associated( self ) RESULT( Status )
    TYPE(MWwaterLUT_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION MWwaterLUT_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_Destroy
!
! PURPOSE:
!       Pure subroutine to re-initialize MWwaterLUT objects.
!
! CALLING SEQUENCE:
!       CALL MWwaterLUT_Destroy( MWwaterLUT )
!
! OBJECTS:
!       MWwaterLUT:    Re-initialized MWwaterLUT structure.
!                      UNITS:      N/A
!                      TYPE:       MWwaterLUT_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE MWwaterLUT_Destroy( self )
    TYPE(MWwaterLUT_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%n_Angles       = 0
    self%n_Frequencies  = 0
    self%n_Temperatures = 0
    self%n_Wind_Speeds  = 0
  END SUBROUTINE MWwaterLUT_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_Create
!
! PURPOSE:
!       Pure subroutine to create an instance of an MWwaterLUT object.
!
! CALLING SEQUENCE:
!       CALL MWwaterLUT_Create( MWwaterLUT    , &
!                               n_Angles      , &
!                               n_Frequencies , &
!                               n_Temperatures, &
!                               n_Wind_Speeds   )
!
! OBJECTS:
!       MWwaterLUT:         MWwaterLUT object structure.
!                           UNITS:      N/A
!                           TYPE:       MWwaterLUT_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Angles:           Number of zenith angles for which is are data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:      Number of spectral frequencies for which there are
!                           data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Temperatures:     Number of surface temperatures for which there are
!                           data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Wind_Speeds:      Number of surface wind speeds for which there are
!                           data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE MWwaterLUT_Create( &
    self          , &  ! Output
    n_Angles      , &  ! Input
    n_Frequencies , &  ! Input
    n_Temperatures, &  ! Input
    n_Wind_Speeds   )  ! Input
    ! Arguments
    TYPE(MWwaterLUT_type), INTENT(OUT) :: self
    INTEGER              , INTENT(IN)  :: n_Angles
    INTEGER              , INTENT(IN)  :: n_Frequencies
    INTEGER              , INTENT(IN)  :: n_Temperatures
    INTEGER              , INTENT(IN)  :: n_Wind_Speeds
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Angles       < 1 .OR. &
         n_Frequencies  < 1 .OR. &
         n_Temperatures < 1 .OR. &
         n_Wind_Speeds  < 1 ) RETURN


    ! Perform the allocation
    ALLOCATE( self%Angle( n_Angles ), &
              self%Frequency( n_Frequencies ), &
              self%Temperature( n_Temperatures ), &
              self%Wind_Speed( n_Wind_Speeds ), &
              self%ev( n_Angles, n_Frequencies, n_Temperatures, n_Wind_Speeds ), &
              self%eh( n_Angles, n_Frequencies, n_Temperatures, n_Wind_Speeds ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    self%n_Angles       = n_Angles
    self%n_Frequencies  = n_Frequencies
    self%n_Temperatures = n_Temperatures
    self%n_Wind_Speeds  = n_Wind_Speeds
    ! ...Arrays
    self%Angle       = ZERO
    self%Frequency   = ZERO
    self%Temperature = ZERO
    self%Wind_Speed  = ZERO
    self%ev          = ZERO
    self%eh          = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE MWwaterLUT_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a MWwaterLUT object to stdout.
!
! CALLING SEQUENCE:
!       CALL MWwaterLUT_Inspect( MWwaterLUT )
!
! OBJECTS:
!       MWwaterLUT:     MWwaterLUT object to display.
!                       UNITS:      N/A
!                       TYPE:       MWwaterLUT_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWwaterLUT_Inspect( self, pause )
    TYPE(MWwaterLUT_type), INTENT(IN) :: self
    LOGICAL,     OPTIONAL, INTENT(IN) :: pause
    LOGICAL :: wait
    INTEGER :: i2, i3, i4

    wait = .FALSE.
    IF ( PRESENT(pause) ) wait = pause

    WRITE(*,'(1x,"MWwaterLUT OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version : ",i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Angles        : ",i0)') self%n_Angles
    WRITE(*,'(3x,"n_Frequencies   : ",i0)') self%n_Frequencies
    WRITE(*,'(3x,"n_Temperatures  : ",i0)') self%n_Temperatures
    WRITE(*,'(3x,"n_Wind_Speeds   : ",i0)') self%n_Wind_Speeds
    IF ( .NOT. MWwaterLUT_Associated(self) ) RETURN
    ! Dimension arrays
    WRITE(*,'(3x,"Angle :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Angle
    WRITE(*,'(3x,"Frequency :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Frequency
    WRITE(*,'(3x,"Temperature :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Temperature
    WRITE(*,'(3x,"Wind_Speed :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Wind_Speed

    ! Emissivity arrays
    WRITE(*,'(/3x,"Emissivity(vertical polarisation) :")')
    IF ( wait ) THEN
      WRITE(*,FMT='(/1x,"Paused. Press <ENTER> to continue...")',ADVANCE='NO')
      READ(*,*)
    END IF

    DO i4 = 1, self%n_Wind_Speeds
      WRITE(*,'(5x,"WIND_SPEED  :",es13.6)') self%Wind_Speed(i4)
      DO i3 = 1, self%n_Temperatures
        WRITE(*,'(5x,"TEMPERATURE :",es13.6)') self%Temperature(i3)
        DO i2 = 1, self%n_Frequencies
          WRITE(*,'(5x,"FREQUENCY   :",es13.6)') self%Frequency(i2)
          WRITE(*,'(5(1x,es13.6,:))') self%ev(:,i2,i3,i4)
        END DO
      END DO
    END DO

    WRITE(*,'(/3x,"Emissivity(horizontal polarisation) :")')
    IF ( wait ) THEN
      WRITE(*,FMT='(/1x,"Paused. Press <ENTER> to continue...")',ADVANCE='NO')
      READ(*,*)
    END IF

    DO i4 = 1, self%n_Wind_Speeds
      WRITE(*,'(5x,"WIND_SPEED  :",es13.6)') self%Wind_Speed(i4)
      DO i3 = 1, self%n_Temperatures
        WRITE(*,'(5x,"TEMPERATURE :",es13.6)') self%Temperature(i3)
        DO i2 = 1, self%n_Frequencies
          WRITE(*,'(5x,"FREQUENCY   :",es13.6)') self%Frequency(i2)
          WRITE(*,'(5(1x,es13.6,:))') self%eh(:,i2,i3,i4)
        END DO
      END DO
    END DO
  END SUBROUTINE MWwaterLUT_Inspect



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_ValidRelease
!
! PURPOSE:
!       Function to check the MWwaterLUT Release value.
!
! CALLING SEQUENCE:
!       IsValid = MWwaterLUT_ValidRelease( MWwaterLUT )
!
! INPUTS:
!       MWwaterLUT:     MWwaterLUT object for which the Release component
!                       is to be checked.
!                       UNITS:      N/A
!                       TYPE:       MWwaterLUT_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:        Logical value defining the release validity.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION MWwaterLUT_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(MWwaterLUT_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWwaterLUT_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < MWWATERLUT_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An MWwaterLUT data update is needed. ", &
                  &"MWwaterLUT release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, MWWATERLUT_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > MWWATERLUT_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An MWwaterLUT software update is needed. ", &
                  &"MWwaterLUT release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, MWWATERLUT_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION MWwaterLUT_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a MWwaterLUT object.
!
! CALLING SEQUENCE:
!       CALL MWwaterLUT_Info( MWwaterLUT, Info )
!
! OBJECTS:
!       MWwaterLUT:      MWwaterLUT object about which info is required.
!                        UNITS:      N/A
!                        TYPE:       MWwaterLUT_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:            String containing version and dimension information
!                        about the MWwaterLUT object.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWwaterLUT_Info( self, Info )
    ! Arguments
    TYPE(MWwaterLUT_type), INTENT(IN)  :: self
    CHARACTER(*)         , INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"MWwaterLUT RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_ANGLES=",i0,2x,&
           &"N_FREQUENCIES=",i0,2x,&
           &"N_TEMPERATURES=",i0,2x,&
           &"N_WIND_SPEEDS=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%n_Angles      , &
           self%n_Frequencies , &
           self%n_Temperatures, &
           self%n_Wind_Speeds

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE MWwaterLUT_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL MWwaterLUT_DefineVersion( Id )
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

  SUBROUTINE MWwaterLUT_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE MWwaterLUT_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_InquireFile
!
! PURPOSE:
!       Function to inquire MWwaterLUT object files.
!
! CALLING SEQUENCE:
!       Error_Status = MWwaterLUT_InquireFile( &
!                        Filename                       , &
!                        n_Angles       = n_Angles      , &
!                        n_Frequencies  = n_Frequencies , &
!                        n_Temperatures = n_Temperatures, &
!                        n_Wind_Speeds  = n_Wind_Speeds , &
!                        Release        = Release       , &
!                        Version        = Version       , &
!                        Title          = Title         , &
!                        History        = History       , &
!                        Comment        = Comment         )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Angles:           Number of zenith angles for which is are data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Frequencies:      Number of spectral frequencies for which there are
!                           data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Temperatures:     Number of surface temperatures for which there are
!                           data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Wind_Speeds:      Number of surface wind speeds for which there are
!                           data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string containing a succinct description
!                           of what is in the dataset.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string containing dataset creation
!                           history.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string containing any comments about
!                           the dataset.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquire was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION MWwaterLUT_InquireFile( &
    Filename      , &  ! Input
    n_Angles      , &  ! Optional output
    n_Frequencies , &  ! Optional output
    n_Temperatures, &  ! Optional output
    n_Wind_Speeds , &  ! Optional output
    Release       , &  ! Optional output
    Version       , &  ! Optional output
    Title         , &  ! Optional output
    History       , &  ! Optional output
    Comment       ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Angles
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Temperatures
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Wind_Speeds
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWwaterLUT_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(MWwaterLUT_type) :: MWwaterLUT


    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MWwaterLUT%Release, &
      MWwaterLUT%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( .NOT. MWwaterLUT_ValidRelease( MWwaterLUT ) ) THEN
      msg = 'MWwaterLUT Release check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MWwaterLUT%n_Angles      , &
      MWwaterLUT%n_Frequencies , &
      MWwaterLUT%n_Temperatures, &
      MWwaterLUT%n_Wind_Speeds
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimension values from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    IF ( PRESENT(n_Angles      ) ) n_Angles       = MWwaterLUT%n_Angles
    IF ( PRESENT(n_Frequencies ) ) n_Frequencies  = MWwaterLUT%n_Frequencies
    IF ( PRESENT(n_Temperatures) ) n_Temperatures = MWwaterLUT%n_Temperatures
    IF ( PRESENT(n_Wind_Speeds ) ) n_Wind_Speeds  = MWwaterLUT%n_Wind_Speeds
    IF ( PRESENT(Release       ) ) Release        = MWwaterLUT%Release
    IF ( PRESENT(Version       ) ) Version        = MWwaterLUT%Version

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION MWwaterLUT_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_ReadFile
!
! PURPOSE:
!       Function to read MWwaterLUT object files.
!
! CALLING SEQUENCE:
!       Error_Status = MWwaterLUT_ReadFile( &
!                        MWwaterLUT         , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet   , &
!                        Title    = Title   , &
!                        History  = History , &
!                        Comment  = Comment   )
!
! OBJECTS:
!       MWwaterLUT:     MWwaterLUT object containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       MWwaterLUT_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       MWwaterLUT data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the MWwaterLUT data is embedded within another file.
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

  FUNCTION MWwaterLUT_ReadFile( &
    MWwaterLUT, &  ! Output
    Filename  , &  ! Input
    No_Close  , &  ! Optional input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional output
    History   , &  ! Optional output
    Comment   , &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(MWwaterLUT_type) , INTENT(OUT) :: MWwaterLUT
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL     , OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWwaterLUT_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(MWwaterLUT_type) :: dummy

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
    IF ( .NOT. MWwaterLUT_ValidRelease( dummy ) ) THEN
      msg = 'MWwaterLUT Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%n_Angles      , &
      dummy%n_Frequencies , &
      dummy%n_Temperatures, &
      dummy%n_Wind_Speeds
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading data dimensions - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL MWwaterLUT_Create( &
           MWwaterLUT       , &
           dummy%n_Angles      , &
           dummy%n_Frequencies , &
           dummy%n_Temperatures, &
           dummy%n_Wind_Speeds   )
    IF ( .NOT. MWwaterLUT_Associated( MWwaterLUT ) ) THEN
      msg = 'MWwaterLUT object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    MWwaterLUT%Version = dummy%Version


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
    ! ...Read the dimensional vectors
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MWwaterLUT%Angle      , &
      MWwaterLUT%Frequency  , &
      MWwaterLUT%Temperature, &
      MWwaterLUT%Wind_Speed
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensional vectors - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the emissivity data
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MWwaterLUT%ev, &
      MWwaterLUT%eh
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading emissivity data - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
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
      CALL MWwaterLUT_Info( MWwaterLUT, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL MWwaterLUT_Destroy( MWwaterLUT )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION MWwaterLUT_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWwaterLUT_WriteFile
!
! PURPOSE:
!       Function to write MWwaterLUT object files.
!
! CALLING SEQUENCE:
!       Error_Status = MWwaterLUT_WriteFile( &
!                        MWwaterLUT         , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       MWwaterLUT:     Object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       MWwaterLUT_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       MWwaterLUT format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the MWwaterLUT data is to be embedded within another file.
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

  FUNCTION MWwaterLUT_WriteFile( &
    MWwaterLUT, &  ! Input
    Filename  , &  ! Input
    No_Close  , &  ! Optional input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional input
    History   , &  ! Optional input
    Comment   , &  ! Optional input
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(MWwaterLUT_type) , INTENT(IN) :: MWwaterLUT
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL     , OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL     , OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL     , OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MWwaterLUT_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid


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
    IF ( .NOT. MWwaterLUT_Associated( MWwaterLUT ) ) THEN
      msg = 'MWwaterLUT object is empty.'
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
      MWwaterLUT%Release, &
      MWwaterLUT%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MWwaterLUT%n_Angles      , &
      MWwaterLUT%n_Frequencies , &
      MWwaterLUT%n_Temperatures, &
      MWwaterLUT%n_Wind_Speeds
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing data dimensions - '//TRIM(io_msg)
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
    ! ...Write the dimensional vectors
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MWwaterLUT%Angle      , &
      MWwaterLUT%Frequency  , &
      MWwaterLUT%Temperature, &
      MWwaterLUT%Wind_Speed
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensional vectors - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the emissivity data
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      MWwaterLUT%ev, &
      MWwaterLUT%eh
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing the emissivity data - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
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
       CALL MWwaterLUT_Info( MWwaterLUT, msg )
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

  END FUNCTION MWwaterLUT_WriteFile


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
!       MWwaterLUT_Equal
!
! PURPOSE:
!       Pure function to test the equality of two MWwaterLUT objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = MWwaterLUT_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two MWwaterLUT objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       MWwaterLUT_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!--------------------------------------------------------------------------------

  PURE FUNCTION MWwaterLUT_Equal( x, y ) RESULT( is_equal )
    TYPE(MWwaterLUT_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( (.NOT. MWwaterLUT_Associated(x)) .OR. &
         (.NOT. MWwaterLUT_Associated(y))      ) RETURN

   ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Angles       /= y%n_Angles       ) .OR. &
         (x%n_Frequencies  /= y%n_Frequencies  ) .OR. &
         (x%n_Temperatures /= y%n_Temperatures ) .OR. &
         (x%n_Wind_Speeds  /= y%n_Wind_Speeds  ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Angle       .EqualTo. y%Angle       ) .AND. &
         ALL(x%Frequency   .EqualTo. y%Frequency   ) .AND. &
         ALL(x%Temperature .EqualTo. y%Temperature ) .AND. &
         ALL(x%Wind_Speed  .EqualTo. y%Wind_Speed  ) .AND. &
         ALL(x%ev          .EqualTo. y%ev          ) .AND. &
         ALL(x%eh          .EqualTo. y%eh          ) ) &
      is_equal = .TRUE.

  END FUNCTION MWwaterLUT_Equal

END MODULE MWwaterLUT_Define
