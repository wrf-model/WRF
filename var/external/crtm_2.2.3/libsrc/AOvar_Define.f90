! 
! AOvar_Define
!
! Module defining the CRTM AtmOptics module internal
! variable object.
! 
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 01-Jul-2013
!                       paul.vandelst@noaa.gov
!                       

MODULE AOvar_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
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
  PUBLIC :: AOvar_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: AOvar_Associated
  PUBLIC :: AOvar_Destroy
  PUBLIC :: AOvar_Create
  PUBLIC :: AOvar_Inspect
  PUBLIC :: AOvar_ValidRelease
  PUBLIC :: AOvar_Info
  PUBLIC :: AOvar_DefineVersion
  PUBLIC :: AOvar_InquireFile
  PUBLIC :: AOvar_ReadFile
  PUBLIC :: AOvar_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE AOvar_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: AOvar_Define.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: AOVAR_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: AOVAR_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL =  80 ! String length

  
  ! ---------------------
  ! Structure definitions
  ! ---------------------
  ! The internal variable definition to hold information
  ! between FWD, TL, AD, and K-matrix calls
  TYPE :: AOvar_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER :: Release = AOVAR_RELEASE
    INTEGER :: Version = AOVAR_VERSION
    ! Dimensions
    INTEGER :: n_Layers = 0
    ! The total atmospheric transmittance
    REAL(fp) :: transmittance = ZERO
    ! The profile data
    REAL(fp), ALLOCATABLE :: optical_depth(:)
    REAL(fp), ALLOCATABLE :: bs(:)
    REAL(fp), ALLOCATABLE :: w(:)
  END TYPE AOvar_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                           ## PUBLIC PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

  ELEMENTAL FUNCTION AOvar_Associated( self ) RESULT( Status )
    TYPE(AOvar_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION AOvar_Associated

 
  ELEMENTAL SUBROUTINE AOvar_Destroy( self )
    TYPE(AOvar_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%n_Layers         = 0
  END SUBROUTINE AOvar_Destroy
  
  
  ELEMENTAL SUBROUTINE AOvar_Create( &
    self    , &  ! Output
    n_Layers  )  ! Input
    ! Arguments
    TYPE(AOvar_type), INTENT(OUT) :: self
    INTEGER         , INTENT(IN)  :: n_Layers                
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( self%optical_depth(n_Layers), &
              self%bs(n_Layers), &
              self%w(n_Layers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise dimensions only!
    self%n_Layers = n_Layers        

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.
  END SUBROUTINE AOvar_Create
  
  
  SUBROUTINE AOvar_Inspect( self)
    TYPE(AOvar_type), INTENT(IN) :: self
    WRITE(*,'(1x,"AOvar OBJECT")')

    ! Release/version info
    WRITE(*,'(3x,"Release.Version     :",1x,i0,".",i0)') self%Release, self%Version

    ! Dimensions
    WRITE(*,'(3x,"n_Layers            :",1x,i0)') self%n_Layers        
    IF ( .NOT. AOvar_Associated(self) ) RETURN

    ! Data
    WRITE(*,'(3x,"Total transmittance :",1x,es13.6)') self%transmittance
    WRITE(*,'(3x,"Optical depth (sigma) :")')
    WRITE(*,'(5(1x,es13.6,:))') self%optical_depth
    WRITE(*,'(3x,"Volume scattering coefficient (bs) :")')
    WRITE(*,'(5(1x,es13.6,:))') self%bs
    WRITE(*,'(3x,"Single scatter albedo (w) :")')
    WRITE(*,'(5(1x,es13.6,:))') self%w
  END SUBROUTINE AOvar_Inspect


  FUNCTION AOvar_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(AOvar_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AOvar_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.

    ! Check release is not too old
    IF ( self%Release < AOvar_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An AOvar data update is needed. ", &
                  &"AOvar release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, AOvar_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

    ! Check release is not too new
    IF ( self%Release > AOvar_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An AOvar software update is needed. ", &
                  &"AOvar release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, AOvar_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF
  END FUNCTION AOvar_ValidRelease


  SUBROUTINE AOvar_Info( self, Info )
    ! Arguments
    TYPE(AOvar_type), INTENT(IN)  :: self
    CHARACTER(*),     INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"AOvar RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_LAYERS=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%n_Layers      
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))
  END SUBROUTINE AOvar_Info


  SUBROUTINE AOvar_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE AOvar_DefineVersion


  FUNCTION AOvar_InquireFile( &
    Filename, &  ! Input
    n_Layers, &  ! Optional output  
    Release , &  ! Optional output
    Version , &  ! Optional output
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version        
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AOvar_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(AOvar_type) :: AOvar

 
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
      AOvar%Release, &
      AOvar%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( .NOT. AOvar_ValidRelease( AOvar ) ) THEN
      msg = 'AOvar Release check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%n_Layers      
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
    IF ( PRESENT(n_Layers) ) n_Layers = AOvar%n_Layers        
    IF ( PRESENT(Release ) ) Release  = AOvar%Release        
    IF ( PRESENT(Version ) ) Version  = AOvar%Version        
    
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
    
  END FUNCTION AOvar_InquireFile


  FUNCTION AOvar_ReadFile( &
    AOvar     , &  ! Output
    Filename  , &  ! Input
    No_Close  , &  ! Optional input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional output
    History   , &  ! Optional output
    Comment   , &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(AOvar_type),       INTENT(OUT) :: AOvar
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AOvar_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(AOvar_type) :: dummy
    
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
    IF ( .NOT. AOvar_ValidRelease( dummy ) ) THEN
      msg = 'AOvar Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) dummy%n_Layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading data dimensions - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL AOvar_Create( AOvar, dummy%n_Layers )
    IF ( .NOT. AOvar_Associated( AOvar ) ) THEN
      msg = 'AOvar object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    AOvar%Version = dummy%Version
        

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


    ! Read the data
    ! ...total transmittance
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%transmittance
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading total transmittance - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...optical depth
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%optical_depth
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading optical depth - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...volume scattering coefficient
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%bs
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading volume scattering coefficient - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...single scatter albedo
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%w
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading single scatter albedo - '//TRIM(io_msg)
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
       CALL AOvar_Info( AOvar, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
       END IF
       CALL AOvar_Destroy( AOvar )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION AOvar_ReadFile


  FUNCTION AOvar_WriteFile( &
    AOvar     , &  ! Input
    Filename  , &  ! Input
    No_Close  , &  ! Optional input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional input
    History   , &  ! Optional input
    Comment   , &  ! Optional input
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(AOvar_type),  INTENT(IN) :: AOvar
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AOvar_WriteFile'
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
    IF ( .NOT. AOvar_Associated( AOvar ) ) THEN
      msg = 'AOvar object is empty.'
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
      AOvar%Release, &
      AOvar%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%n_Layers
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


    ! Write the data
    ! ...total transmittance
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%transmittance
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing total transmittance - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...optical depth
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%optical_depth
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing optical depth - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...volume scattering coefficient
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%bs
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing volume scattering coefficient - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...single scatter albedo
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) AOvar%w
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing single scatter albedo - '//TRIM(io_msg)
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
       CALL AOvar_Info( AOvar, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Write_Cleanup()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_Cleanup

  END FUNCTION AOvar_WriteFile

  
!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

  ELEMENTAL FUNCTION AOvar_Equal( x, y ) RESULT( is_equal )
    TYPE(AOvar_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. AOvar_Associated(x)) .OR. &
         (.NOT. AOvar_Associated(y))      ) RETURN

   ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Layers /= y%n_Layers ) ) RETURN
    ! ...Data
    IF (    (x%transmittance .EqualTo. y%transmittance ) .AND. &
         ALL(x%optical_depth .EqualTo. y%optical_depth ) .AND. &
         ALL(x%bs            .EqualTo. y%bs            ) .AND. &
         ALL(x%w             .EqualTo. y%w             ) ) &
      is_equal = .TRUE.
  END FUNCTION AOvar_Equal

END MODULE AOvar_Define

