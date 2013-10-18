!
! AerosolCoeff_Binary_IO
!
! Module containing routines to read and write Binary format
! AerosolCoeff files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Jun-2004
!                       paul.vandelst@noaa.gov
!

MODULE AerosolCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE AerosolCoeff_Define, ONLY: AerosolCoeff_type        , &
                                 AerosolCoeff_Associated  , &
                                 AerosolCoeff_Destroy     , &
                                 AerosolCoeff_Create      , &
                                 AerosolCoeff_ValidRelease, &
                                 AerosolCoeff_Info        , &
                                 AerosolCoeff_Frequency
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: AerosolCoeff_Binary_InquireFile
  PUBLIC :: AerosolCoeff_Binary_ReadFile
  PUBLIC :: AerosolCoeff_Binary_WriteFile
  PUBLIC :: AerosolCoeff_Binary_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: AerosolCoeff_Binary_IO.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_Binary_InquireFile
!
! PURPOSE:
!       Function to inquire AerosolCoeff object Binary format files.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_Binary_InquireFile( &
!                        Filename, &
!                        n_Wavelengths    = n_Wavelengths   , &
!                        n_Radii          = n_Radii         , &
!                        n_Types          = n_Types         , &
!                        n_RH             = n_RH            , &
!                        n_Legendre_Terms = n_Legendre_Terms, &
!                        n_Phase_Elements = n_Phase_Elements, &
!                        Release          = Release         , &
!                        Version          = Version           )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          AerosolCoeff format data file.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Wavelengths:     The number of wavelengths in the look-up
!                          table (LUT). Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Radii:           The number of discrete effective radii for
!                          scatterers in the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Types:           The number of different aerosol types in
!                          the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_RH:              The number of relative humidity entries in
!                          the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:  The maximum number of Legendre polynomial
!                          terms in the LUT. Can be = 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Phase_Elements:  The maximum number of phase elements in the LUT.
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Release:           The coefficient file release number.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The coefficient file version number.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the Binary file inquiry was successful
!                             == FAILURE an error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION AerosolCoeff_Binary_InquireFile( &
    Filename        , &  ! Input
    n_Wavelengths   , &  ! Optional output
    n_Radii         , &  ! Optional output
    n_Types         , &  ! Optional output
    n_RH            , &  ! Optional output
    n_Legendre_Terms, &  ! Optional output
    n_Phase_Elements, &  ! Optional output
    Release         , &  ! Optional output
    Version         ) &  ! Optional Output
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Wavelengths       
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Radii             
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Types             
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_RH                
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Legendre_Terms    
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Phase_Elements    
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_Binary_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(AerosolCoeff_type) :: AerosolCoeff
    
    ! Setup
    err_stat = SUCCESS
    fid = -100
    ! Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the AerosolCoeff data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the release and version
    READ( fid,IOSTAT=io_stat ) AerosolCoeff%Release, AerosolCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) AerosolCoeff%n_Wavelengths   , &
                               AerosolCoeff%n_Radii         , &
                               AerosolCoeff%n_Types         , &
                               AerosolCoeff%n_RH            , &
                               AerosolCoeff%n_Legendre_Terms, &
                               AerosolCoeff%n_Phase_Elements
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    ! Close the file
    CLOSE( fid,IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    ! Set the return arguments
    IF ( PRESENT(n_Wavelengths   ) ) n_Wavelengths    = AerosolCoeff%n_Wavelengths
    IF ( PRESENT(n_Radii         ) ) n_Radii          = AerosolCoeff%n_Radii      
    IF ( PRESENT(n_Types         ) ) n_Types          = AerosolCoeff%n_Types
    IF ( PRESENT(n_RH            ) ) n_RH             = AerosolCoeff%n_RH      
    IF ( PRESENT(n_Legendre_Terms) ) n_Legendre_Terms = AerosolCoeff%n_Legendre_Terms
    IF ( PRESENT(n_Phase_Elements) ) n_Phase_Elements = AerosolCoeff%n_Phase_Elements
    IF ( PRESENT(Release         ) ) Release          = AerosolCoeff%Release     
    IF ( PRESENT(Version         ) ) Version          = AerosolCoeff%Version     

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION AerosolCoeff_Binary_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_Binary_ReadFile
!
! PURPOSE:
!       Function to read AerosolCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_Binary_ReadFile( &
!                        Filename     , &
!                        AerosolCoeff   , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       AerosolCoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AerosolCoeff:   AerosolCoeff object containing the aerosol coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AerosolCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
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

  FUNCTION AerosolCoeff_Binary_ReadFile( &
    Filename    , &  ! Input
    AerosolCoeff, &  ! Output
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(AerosolCoeff_type), INTENT(OUT) :: AerosolCoeff
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_ReadFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: strlen
    TYPE(AerosolCoeff_type) :: dummy

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF

    
    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read and check the release and version
    READ( fid,IOSTAT=io_stat ) dummy%Release, dummy%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. AerosolCoeff_ValidRelease( dummy ) ) THEN
      msg = 'AerosolCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    

    ! Read the aerosol coefficient data
    ! ...Read the dimensions
    READ( fid,IOSTAT=io_stat ) dummy%n_Wavelengths   , &
                               dummy%n_Radii         , &
                               dummy%n_Types         , &
                               dummy%n_RH            , &
                               dummy%n_Legendre_Terms, &
                               dummy%n_Phase_Elements
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL AerosolCoeff_Create( &
           AerosolCoeff, &
           dummy%n_Wavelengths   , &
           dummy%n_Radii         , &
           dummy%n_Types         , &
           dummy%n_RH            , &
           dummy%n_Legendre_Terms, &
           dummy%n_Phase_Elements  )
    IF ( .NOT. AerosolCoeff_Associated( AerosolCoeff ) ) THEN
      msg = 'AerosolCoeff object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the data source
    READ( fid,IOSTAT=io_stat ) strlen
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data source string length. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( strlen /= LEN(AerosolCoeff%Data_Source) ) THEN
      msg = 'Data source string length does not match structure definition'
      CALL Read_Cleanup(); RETURN
    END IF
    READ( fid,IOSTAT=io_stat ) AerosolCoeff%Data_Source
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data source string. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the type vector
    READ( fid,IOSTAT=io_stat ) AerosolCoeff%Type
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading type vector data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the type name vector
    READ( fid,IOSTAT=io_stat ) strlen
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading type name string length. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( strlen /= LEN(AerosolCoeff%Type_Name(1)) ) THEN
      msg = 'Type name string length does not match structure definition'
      CALL Read_Cleanup(); RETURN
    END IF
    READ( fid,IOSTAT=io_stat ) AerosolCoeff%Type_Name
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading type names. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the dimension vectors
    READ( fid,IOSTAT=io_stat ) AerosolCoeff%Wavelength, &
                               AerosolCoeff%Reff      , &
                               AerosolCoeff%RH
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimension vector data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the coefficient data
    READ( fid,IOSTAT=io_stat ) AerosolCoeff%ke    , &
                               AerosolCoeff%w     , &
                               AerosolCoeff%g     , &
                               AerosolCoeff%pcoeff
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading coefficient data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Assign the version number read in
    AerosolCoeff%Version = dummy%Version
    ! ...Compute the frequencies
    CALL AerosolCoeff_Frequency( AerosolCoeff )    


    ! Close the file
    CLOSE( fid,IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF

 
    ! Output an info message
    IF ( noisy ) THEN
      CALL AerosolCoeff_Info( AerosolCoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL AerosolCoeff_Destroy( AerosolCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION AerosolCoeff_Binary_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_Binary_WriteFile
!
! PURPOSE:
!       Function to write AerosolCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_Binary_WriteFile( &
!                        Filename, &
!                        AerosolCoeff, &
!                        Quiet = Quiet )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AerosolCoeff format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AerosolCoeff:   Object containing the aerosol coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AerosolCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
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
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AerosolCoeff_Binary_WriteFile( &
    Filename    , &  ! Input
    AerosolCoeff, &  ! Input
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(AerosolCoeff_type), INTENT(IN)  :: AerosolCoeff
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_WriteFile(Binary)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
 
    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Check the AerosolCoeff object
    ! ...Is there any data?
    IF ( .NOT. AerosolCoeff_Associated( AerosolCoeff ) ) THEN 
      msg = 'Input AerosolCoeff object is not allocated.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. AerosolCoeff_ValidRelease( AerosolCoeff ) ) THEN
      msg = 'AerosolCoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Open the file for writing
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the release and version
    WRITE( fid,IOSTAT=io_stat ) AerosolCoeff%Release, AerosolCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    
    
    ! Write the aerosol coefficient data
    ! ...Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) AerosolCoeff%n_Wavelengths   , &
                                AerosolCoeff%n_Radii         , &
                                AerosolCoeff%n_Types         , &
                                AerosolCoeff%n_RH            , &
                                AerosolCoeff%n_Legendre_Terms, &
                                AerosolCoeff%n_Phase_Elements
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the data source
    WRITE( fid,IOSTAT=io_stat ) LEN(AerosolCoeff%Data_Source)
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data source string length. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    WRITE( fid,IOSTAT=io_stat ) AerosolCoeff%Data_Source
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data source string. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the type vector
    WRITE( fid,IOSTAT=io_stat ) AerosolCoeff%Type
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing type vector data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the type name vector
    WRITE( fid,IOSTAT=io_stat ) LEN(AerosolCoeff%Type_Name(1))
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing type name string length. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    WRITE( fid,IOSTAT=io_stat ) AerosolCoeff%Type_Name
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing type names. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the dimension vectors
    WRITE( fid,IOSTAT=io_stat ) AerosolCoeff%Wavelength, &
                                AerosolCoeff%Reff      , &
                                AerosolCoeff%RH        
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dimension vector data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the coefficient data
    WRITE( fid,IOSTAT=io_stat ) AerosolCoeff%ke    , &
                                AerosolCoeff%w     , &
                                AerosolCoeff%g     , &
                                AerosolCoeff%pcoeff
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing coefficient data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    
    
    ! Close the file
    CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL AerosolCoeff_Info( AerosolCoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION AerosolCoeff_Binary_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_Binary_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_Binary_IOVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE AerosolCoeff_Binary_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE AerosolCoeff_Binary_IOVersion

END MODULE AerosolCoeff_Binary_IO
