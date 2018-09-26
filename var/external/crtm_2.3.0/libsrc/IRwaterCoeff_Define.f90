!
! IRwaterCoeff_Define
!
! Module defining the IRwaterCoeff object to hold coefficient
! data for the infrared water surface emissivity and reflectivity models.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Aug-2011
!                       paul.vandelst@noaa.gov
 
MODULE IRwaterCoeff_Define

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
  PUBLIC :: IRwaterCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: IRwaterCoeff_Associated
  PUBLIC :: IRwaterCoeff_Destroy
  PUBLIC :: IRwaterCoeff_Create
  PUBLIC :: IRwaterCoeff_Inspect
  PUBLIC :: IRwaterCoeff_ValidRelease
  PUBLIC :: IRwaterCoeff_Info
  PUBLIC :: IRwaterCoeff_DefineVersion
  PUBLIC :: IRwaterCoeff_InquireFile
  PUBLIC :: IRwaterCoeff_ReadFile
  PUBLIC :: IRwaterCoeff_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE IRwaterCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: IRwaterCoeff_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Current valid release and version
  INTEGER, PARAMETER :: IRWATERCOEFF_RELEASE = 3  ! This determines structure and file formats.
  INTEGER, PARAMETER :: IRWATERCOEFF_VERSION = 2  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Conversion constants
  REAL(fp), PARAMETER :: PI = 3.141592653589793238462643383279_fp
  REAL(fp), PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length


  ! ----------------------------------
  ! IRwaterCoeff data type definitions
  ! ----------------------------------
  !:tdoc+:
  TYPE :: IRwaterCoeff_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = IRWATERCOEFF_RELEASE
    INTEGER(Long) :: Version = IRWATERCOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Angles      = 0   ! I dimension
    INTEGER(Long) :: n_Frequencies = 0   ! L dimension
    INTEGER(Long) :: n_Wind_Speeds = 0   ! N dimension
    ! Dimensional vectors
    REAL(Double), ALLOCATABLE :: Angle(:)       ! I
    REAL(Double), ALLOCATABLE :: Frequency(:)   ! L
    REAL(Double), ALLOCATABLE :: Wind_Speed(:)  ! N
    ! Emissivity LUT data
    REAL(Double), ALLOCATABLE :: Emissivity(:,:,:)  ! I x L x N
    ! Transformed dimensional vectors
    REAL(Double), ALLOCATABLE :: Secant_Angle(:)  ! I
  END TYPE IRwaterCoeff_type
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
!       IRwaterCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the IRwaterCoeff structure.
!
! CALLING SEQUENCE:
!       Status = IRwaterCoeff_Associated( IRwaterCoeff )
!
! OBJECTS:
!       IRwaterCoeff:  Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       IRwaterCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the NLTE members.
!                       .TRUE.  - if ANY of the IRwaterCoeff allocatable members
!                                 are in use.
!                       .FALSE. - if ALL of the IRwaterCoeff allocatable members
!                                 are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION IRwaterCoeff_Associated( self ) RESULT( Status )
    TYPE(IRwaterCoeff_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION IRwaterCoeff_Associated

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize IRwaterCoeff objects.
!
! CALLING SEQUENCE:
!       CALL IRwaterCoeff_Destroy( IRwaterCoeff )
!
! OBJECTS:
!       IRwaterCoeff: Re-initialized IRwaterCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       IRwaterCoeff_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE IRwaterCoeff_Destroy( self )
    TYPE(IRwaterCoeff_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE IRwaterCoeff_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an IRwaterCoeff object.
!
! CALLING SEQUENCE:
!       CALL IRwaterCoeff_Create( IRwaterCoeff , &
!                                 n_Angles     , &
!                                 n_Frequencies, &
!                                 n_Wind_Speeds  )
!
! OBJECTS:
!       IRwaterCoeff:   IRwaterCoeff object structure.
!                       UNITS:      N/A
!                       TYPE:       IRwaterCoeff_type
!                       DIMENSION:  Scalar or any rank
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Angles:       Number of angles dimension.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with the IRwaterCoeff object
!                       ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:  Number of frequencies dimension.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with the IRwaterCoeff object
!                       ATTRIBUTES: INTENT(IN)
!
!       n_Wind_Speeds:  Number of wind speeds dimension.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with the IRwaterCoeff object
!                       ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE IRwaterCoeff_Create( &
    self         , &  ! Output
    n_Angles     , &  ! Input
    n_Frequencies, &  ! Input
    n_Wind_Speeds  )  ! Input
    ! Arguments
    TYPE(IRwaterCoeff_type), INTENT(OUT) :: self
    INTEGER                , INTENT(IN)  :: n_Angles             
    INTEGER                , INTENT(IN)  :: n_Frequencies        
    INTEGER                , INTENT(IN)  :: n_Wind_Speeds             
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Angles      < 1 .OR. &
         n_Frequencies < 1 .OR. &
         n_Wind_Speeds < 1 ) RETURN

   
    ! Perform the allocation
    ALLOCATE( self%Angle( n_Angles ), &
              self%Frequency( n_Frequencies ), &
              self%Wind_Speed( n_Wind_Speeds ), &
              self%Emissivity( n_Angles, n_Frequencies, n_Wind_Speeds ), &
              self%Secant_Angle( n_Angles ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    self%n_Angles      = n_Angles     
    self%n_Frequencies = n_Frequencies
    self%n_Wind_Speeds = n_Wind_Speeds
    ! ...Arrays
    self%Angle        = ZERO
    self%Frequency    = ZERO
    self%Wind_Speed   = ZERO
    self%Emissivity   = ZERO
    self%Secant_Angle = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE IRwaterCoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a IRwaterCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL IRwaterCoeff_Inspect( IRwaterCoeff )
!
! OBJECTS:
!       IRwaterCoeff:  IRwaterCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       IRwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRwaterCoeff_Inspect( self )
    TYPE(IRwaterCoeff_type), INTENT(IN) :: self
    INTEGER :: i2, i3
    WRITE(*,'(1x,"IRwaterCoeff OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version :",1x,i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Angles        :",1x,i0)') self%n_Angles     
    WRITE(*,'(3x,"n_Frequencies   :",1x,i0)') self%n_Frequencies
    WRITE(*,'(3x,"n_Wind_Speeds   :",1x,i0)') self%n_Wind_Speeds
    IF ( .NOT. IRwaterCoeff_Associated(self) ) RETURN
    ! Dimension arrays
    WRITE(*,'(3x,"Angle      :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Angle     
    WRITE(*,'(3x,"Frequency  :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Frequency 
    WRITE(*,'(3x,"Wind_Speed :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Wind_Speed
    ! Emissivity array
    WRITE(*,'(3x,"Emissivity :")')
    DO i3 = 1, self%n_Wind_Speeds
      WRITE(*,'(5x,"WIND_SPEED :",es13.6)') self%Wind_Speed(i3)
      DO i2 = 1, self%n_Frequencies
        WRITE(*,'(5x,"FREQUENCY  :",es13.6)') self%Frequency(i2)      
        WRITE(*,'(5(1x,es13.6,:))') self%Emissivity(:,i2,i3)
      END DO
    END DO
  END SUBROUTINE IRwaterCoeff_Inspect



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the IRwaterCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = IRwaterCoeff_ValidRelease( IRwaterCoeff )
!
! INPUTS:
!       IRwaterCoeff:  IRwaterCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       IRwaterCoeff_type
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

  FUNCTION IRwaterCoeff_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(IRwaterCoeff_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRwaterCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < IRWATERCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An IRwaterCoeff data update is needed. ", &
                  &"IRwaterCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, IRWATERCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > IRWATERCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An IRwaterCoeff software update is needed. ", &
                  &"IRwaterCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, IRWATERCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION IRwaterCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a IRwaterCoeff object.
!
! CALLING SEQUENCE:
!       CALL IRwaterCoeff_Info( IRwaterCoeff, Info )
!
! OBJECTS:
!       IRwaterCoeff:  IRwaterCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       IRwaterCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the IRwaterCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRwaterCoeff_Info( self, Info )
    ! Arguments
    TYPE(IRwaterCoeff_type), INTENT(IN)  :: self
    CHARACTER(*),            INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '( a,1x,"IRwaterCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
              &"N_ANGLES=",i3,2x,&
              &"N_FREQUENCIES=",i5,2x,&
              &"N_WIND_SPEEDS=",i3 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           self%n_Angles, &
           self%n_Frequencies, &
           self%n_Wind_Speeds
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE IRwaterCoeff_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL IRwaterCoeff_DefineVersion( Id )
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

  SUBROUTINE IRwaterCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE IRwaterCoeff_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire a IRwaterCoeff object container file.
!
! CALLING SEQUENCE:
!       Error_Status = IRwaterCoeff_InquireFile( &
!                        Filename                     , &
!                        n_Angles      = n_Angles     , &
!                        n_Frequencies = n_Frequencies, &
!                        n_Wind_Speeds = n_Wind_Speeds, &
!                        Release       = Release      , &
!                        Version       = Version      , &
!                        Title         = Title        , &
!                        History       = History      , &
!                        Comment       = Comment        )
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
!       n_Angles:           Number of angles for which there are
!                           emissivity data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Frequencies:      Number of spectral frequencies for which there are
!                           emissivity data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Wind_Speeds:      Number of wind speeds for which there are
!                           emissivity data.
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

  FUNCTION IRwaterCoeff_InquireFile( &
    Filename     , &  ! Input
    n_Angles     , &  ! Optional output
    n_Frequencies, &  ! Optional output
    n_Wind_Speeds, &  ! Optional output
    Release      , &  ! Optional output
    Version      , &  ! Optional output
    Title        , &  ! Optional output
    History      , &  ! Optional output
    Comment      ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Angles     
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Wind_Speeds
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRwaterCoeff_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(IRwaterCoeff_type) :: IRwaterCoeff

 
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
      IRwaterCoeff%Release, &
      IRwaterCoeff%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      IRwaterCoeff%n_Angles     , &
      IRwaterCoeff%n_Frequencies, &
      IRwaterCoeff%n_Wind_Speeds
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
    IF ( PRESENT(n_Angles     ) ) n_Angles      = IRwaterCoeff%n_Angles     
    IF ( PRESENT(n_Frequencies) ) n_Frequencies = IRwaterCoeff%n_Frequencies
    IF ( PRESENT(n_Wind_Speeds) ) n_Wind_Speeds = IRwaterCoeff%n_Wind_Speeds    
    IF ( PRESENT(Release      ) ) Release       = IRwaterCoeff%Release
    IF ( PRESENT(Version      ) ) Version       = IRwaterCoeff%Version
    
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
    
  END FUNCTION IRwaterCoeff_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_ReadFile
!
! PURPOSE:
!       Function to read IRwaterCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = IRwaterCoeff_ReadFile( &
!                        IRwaterCoeff       , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet   , &
!                        Title    = Title   , &
!                        History  = History , &
!                        Comment  = Comment   )
!
! OBJECTS:
!       IRwaterCoeff:   Object containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       IRwaterCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the IRwaterCoeff data is embedded within another file.
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

  FUNCTION IRwaterCoeff_ReadFile( &
    IRwaterCoeff, &  ! Output
    Filename    , &  ! Input
    No_Close    , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     , &  ! Optional output
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(IRwaterCoeff_type), INTENT(OUT) :: IRwaterCoeff
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRwaterCoeff_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(IRwaterCoeff_type) :: dummy
    

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
    IF ( .NOT. IRwaterCoeff_ValidRelease( dummy ) ) THEN
      msg = 'IRwaterCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%n_Angles     , &
      dummy%n_Frequencies, &
      dummy%n_Wind_Speeds
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimension values from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Create the return object
    CALL IRwaterCoeff_Create( &
           IRwaterCoeff       , &
           dummy%n_Angles     , &        
           dummy%n_Frequencies, &        
           dummy%n_Wind_Speeds  )
    IF ( .NOT. IRwaterCoeff_Associated( IRwaterCoeff ) ) THEN
      msg = 'IRwaterCoeff object creation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    IRwaterCoeff%Version = dummy%Version
    
    
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
      IRwaterCoeff%Angle     , &
      IRwaterCoeff%Frequency , &
      IRwaterCoeff%Wind_Speed
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensional vectors - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the emissivity data
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      IRwaterCoeff%Emissivity
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading emissivity data - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Compute the transformed dimensional vectors
    IRwaterCoeff%Secant_Angle = ONE/COS(DEGREES_TO_RADIANS*IRwaterCoeff%Angle)

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
       CALL IRwaterCoeff_Info( IRwaterCoeff, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
       END IF
       CALL IRwaterCoeff_Destroy( IRwaterCoeff )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION IRwaterCoeff_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRwaterCoeff_WriteFile
!
! PURPOSE:
!       Function to write IRwaterCoeff object container files.
!
! CALLING SEQUENCE:
!       Error_Status = IRwaterCoeff_WriteFile( &
!                        IRwaterCoeff       , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet   , &
!                        Title    = Title   , &
!                        History  = History , &
!                        Comment  = Comment   )
!
! OBJECTS:
!       IRwaterCoeff:   Object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       IRwaterCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the IRwaterCoeff data is to be embedded within another file.
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

  FUNCTION IRwaterCoeff_WriteFile( &
    IRwaterCoeff, &  ! Input
    Filename    , &  ! Input
    No_Close    , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     , &  ! Optional input
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(IRwaterCoeff_type), INTENT(IN) :: IRwaterCoeff
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRwaterCoeff_WriteFile'
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
    IF ( .NOT. IRwaterCoeff_Associated( IRwaterCoeff ) ) THEN
      msg = 'IRwaterCoeff object is empty.'
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
      IRwaterCoeff%Release, &
      IRwaterCoeff%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      IRwaterCoeff%n_Angles     , &
      IRwaterCoeff%n_Frequencies, &
      IRwaterCoeff%n_Wind_Speeds
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimension values to '//TRIM(Filename)//' - '//TRIM(io_msg)
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
      IRwaterCoeff%Angle     , &
      IRwaterCoeff%Frequency , &
      IRwaterCoeff%Wind_Speed
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensional vectors - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the emissivity data
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      IRwaterCoeff%Emissivity
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing emissivity data - '//TRIM(io_msg)
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
       CALL IRwaterCoeff_Info( IRwaterCoeff, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Write_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, STATUS=WRITE_ERROR_STATUS, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_CleanUp

  END FUNCTION IRwaterCoeff_WriteFile


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
!       IRwaterCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two IRwaterCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = IRwaterCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two IRwaterCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       IRwaterCoeff_type
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

  ELEMENTAL FUNCTION IRwaterCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(IRwaterCoeff_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. IRwaterCoeff_Associated(x)) .OR. &
         (.NOT. IRwaterCoeff_Associated(y))      ) RETURN


    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Angles      /= y%n_Angles      ) .OR. &
         (x%n_Frequencies /= y%n_Frequencies ) .OR. &
         (x%n_Wind_Speeds /= y%n_Wind_Speeds ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Angle      .EqualTo. y%Angle      ) .AND. &
         ALL(x%Frequency  .EqualTo. y%Frequency  ) .AND. &
         ALL(x%Wind_Speed .EqualTo. y%Wind_Speed ) .AND. &
         ALL(x%Emissivity .EqualTo. y%Emissivity ) ) &
      is_equal = .TRUE.
    
  END FUNCTION IRwaterCoeff_Equal

END MODULE IRwaterCoeff_Define
