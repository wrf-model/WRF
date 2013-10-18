!
! CloudCoeff_Binary_IO
!
! Module containing routines to read and write Binary format
! CloudCoeff files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CloudCoeff_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE File_Utility       , ONLY: File_Open, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CloudCoeff_Define  , ONLY: CloudCoeff_type        , &
                                 CloudCoeff_Associated  , &
                                 CloudCoeff_Destroy     , &
                                 CloudCoeff_Create      , &
                                 CloudCoeff_ValidRelease, &
                                 CloudCoeff_Info
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CloudCoeff_Binary_InquireFile
  PUBLIC :: CloudCoeff_Binary_ReadFile
  PUBLIC :: CloudCoeff_Binary_WriteFile
  PUBLIC :: CloudCoeff_Binary_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: CloudCoeff_Binary_IO.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_Binary_InquireFile
!
! PURPOSE:
!       Function to inquire CloudCoeff object Binary format files.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_Binary_InquireFile( &
!                        Filename, &
!                        n_MW_Frequencies = n_MW_Frequencies, &
!                        n_MW_Radii       = n_MW_Radii      , &
!                        n_IR_Frequencies = n_IR_Frequencies, &
!                        n_IR_Radii       = n_IR_Radii      , &
!                        n_Temperatures   = n_Temperatures  , &
!                        n_Densities      = n_Densities     , &
!                        n_Legendre_Terms = n_Legendre_Terms, &
!                        n_Phase_Elements = n_Phase_Elements, &
!                        Release          = Release         , &
!                        Version          = Version           )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          CloudCoeff format data file.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_MW_Frequencies:  The number of microwave frequencies in
!                          the look-up table (LUT)
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_MW_Radii:        The number of discrete effective radii 
!                          for MW scatterers in the LUT.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_IR_Frequencies:  The number of infrared frequencies in
!                          the LUT 
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_IR_Radii:        The number of discrete effective radii 
!                          for IR scatterers in the LUT.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Temperatures:    The number of discrete layer temperatures
!                          in the LUT. 
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Densities:       The number of fixed densities for snow, graupel,
!                          and hail/ice in the LUT. 
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Legendre_Terms:  The maximum number of Legendre polynomial
!                          terms in the LUT.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Phase_Elements:  The maximum number of phase elements in the LUT.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
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

  FUNCTION CloudCoeff_Binary_InquireFile( &
    Filename        , &  ! Input
    n_MW_Frequencies, &  ! Optional Output
    n_MW_Radii      , &  ! Optional Output
    n_IR_Frequencies, &  ! Optional Output
    n_IR_Radii      , &  ! Optional Output
    n_Temperatures  , &  ! Optional Output
    n_Densities     , &  ! Optional Output
    n_Legendre_Terms, &  ! Optional Output
    n_Phase_Elements, &  ! Optional Output
    Release         , &  ! Optional Output
    Version         ) &  ! Optional Output
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_MW_Frequencies
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_MW_Radii
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_IR_Frequencies
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_IR_Radii
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Temperatures
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Densities
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Legendre_Terms
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Phase_Elements
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_Binary_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(CloudCoeff_type) :: CloudCoeff
    
    ! Setup
    err_stat = SUCCESS
    fid = -100
    ! Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the CloudCoeff data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the release and version
    READ( fid,IOSTAT=io_stat ) CloudCoeff%Release, CloudCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    ! Read the dimensions
    READ( fid,IOSTAT=io_stat ) CloudCoeff%n_MW_Frequencies, &
                               CloudCoeff%n_MW_Radii      , &
                               CloudCoeff%n_IR_Frequencies, &
                               CloudCoeff%n_IR_Radii      , &
                               CloudCoeff%n_Temperatures  , &
                               CloudCoeff%n_Densities     , &
                               CloudCoeff%n_Legendre_Terms, &
                               CloudCoeff%n_Phase_Elements
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
    IF ( PRESENT(n_MW_Frequencies) ) n_MW_Frequencies = CloudCoeff%n_MW_Frequencies
    IF ( PRESENT(n_MW_Radii      ) ) n_MW_Radii       = CloudCoeff%n_MW_Radii      
    IF ( PRESENT(n_IR_Frequencies) ) n_IR_Frequencies = CloudCoeff%n_IR_Frequencies
    IF ( PRESENT(n_IR_Radii      ) ) n_IR_Radii       = CloudCoeff%n_IR_Radii      
    IF ( PRESENT(n_Temperatures  ) ) n_Temperatures   = CloudCoeff%n_Temperatures  
    IF ( PRESENT(n_Densities     ) ) n_Densities      = CloudCoeff%n_Densities     
    IF ( PRESENT(n_Legendre_Terms) ) n_Legendre_Terms = CloudCoeff%n_Legendre_Terms
    IF ( PRESENT(n_Phase_Elements) ) n_Phase_Elements = CloudCoeff%n_Phase_Elements
    IF ( PRESENT(Release         ) ) Release          = CloudCoeff%Release     
    IF ( PRESENT(Version         ) ) Version          = CloudCoeff%Version     

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

  END FUNCTION CloudCoeff_Binary_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_Binary_ReadFile
!
! PURPOSE:
!       Function to read CloudCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_Binary_ReadFile( &
!                        Filename     , &
!                        CloudCoeff   , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CloudCoeff format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       CloudCoeff:     CloudCoeff object containing the cloud coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CloudCoeff_type)
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

  FUNCTION CloudCoeff_Binary_ReadFile( &
    Filename  , &  ! Input
    CloudCoeff, &  ! Output
    Quiet     , &  ! Optional input
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),          INTENT(IN)  :: Filename
    TYPE(CloudCoeff_type), INTENT(OUT) :: CloudCoeff
    LOGICAL,     OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,     OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_ReadFile(Binary)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(CloudCoeff_type) :: dummy

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
    IF ( .NOT. CloudCoeff_ValidRelease( dummy ) ) THEN
      msg = 'CloudCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    

    ! Read the cloud coefficient data
    ! ...Read the dimensions
    READ( fid,IOSTAT=io_stat ) dummy%n_MW_Frequencies, &
                               dummy%n_MW_Radii      , &
                               dummy%n_IR_Frequencies, &
                               dummy%n_IR_Radii      , &
                               dummy%n_Temperatures  , &
                               dummy%n_Densities     , &
                               dummy%n_Legendre_Terms, &
                               dummy%n_Phase_Elements
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL CloudCoeff_Create( CloudCoeff, &
                            dummy%n_MW_Frequencies, &
                            dummy%n_MW_Radii      , &
                            dummy%n_IR_Frequencies, &
                            dummy%n_IR_Radii      , &
                            dummy%n_Temperatures  , &
                            dummy%n_Densities     , &
                            dummy%n_Legendre_Terms, &
                            dummy%n_Phase_Elements  )
    IF ( .NOT. CloudCoeff_Associated( CloudCoeff ) ) THEN
      msg = 'CloudCoeff object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the dimension vectors
    READ( fid,IOSTAT=io_stat ) CloudCoeff%Frequency_MW, &
                               CloudCoeff%Frequency_IR, &
                               CloudCoeff%Reff_MW     , &
                               CloudCoeff%Reff_IR     , &
                               CloudCoeff%Temperature , &
                               CloudCoeff%Density
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimension vector data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the microwave liquid phase data
    READ( fid,IOSTAT=io_stat ) CloudCoeff%ke_L_MW    , &
                               CloudCoeff%w_L_MW     , &
                               CloudCoeff%g_L_MW     , &
                               CloudCoeff%pcoeff_L_MW
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading microwave liquid phase data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the microwave solid phase data
    READ( fid,IOSTAT=io_stat ) CloudCoeff%ke_S_MW    , &
                               CloudCoeff%w_S_MW     , &
                               CloudCoeff%g_S_MW     , &
                               CloudCoeff%pcoeff_S_MW
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading microwave solid phase data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the infrared data
    READ( fid,IOSTAT=io_stat ) CloudCoeff%ke_IR    , &
                               CloudCoeff%w_IR     , &
                               CloudCoeff%g_IR     , &
                               CloudCoeff%pcoeff_IR
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading infrared data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Assign the version number read in
    CloudCoeff%Version = dummy%Version
    

    ! Close the file
    CLOSE( fid,IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF

 
    ! Output an info message
    IF ( noisy ) THEN
      CALL CloudCoeff_Info( CloudCoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL CloudCoeff_Destroy( CloudCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CloudCoeff_Binary_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_Binary_WriteFile
!
! PURPOSE:
!       Function to write CloudCoeff object files in Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_Binary_WriteFile( &
!                        Filename  , &
!                        CloudCoeff, &
!                        Quiet = Quiet )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       CloudCoeff format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       CloudCoeff:     Object containing the cloud coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CloudCoeff_type)
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

  FUNCTION CloudCoeff_Binary_WriteFile( &
    Filename  , &  ! Input
    CloudCoeff, &  ! Input
    Quiet     , &  ! Optional input
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),          INTENT(IN)  :: Filename
    TYPE(CloudCoeff_type), INTENT(IN)  :: CloudCoeff
    LOGICAL,     OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,     OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_WriteFile(Binary)'
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


    ! Check the CloudCoeff object
    ! ...Is there any data?
    IF ( .NOT. CloudCoeff_Associated( CloudCoeff ) ) THEN 
      msg = 'Input CloudCoeff object is not allocated.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. CloudCoeff_ValidRelease( CloudCoeff ) ) THEN
      msg = 'CloudCoeff Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Open the file for writing
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the release and version
    WRITE( fid,IOSTAT=io_stat ) CloudCoeff%Release, CloudCoeff%Version
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Release/Version. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    
    
    ! Write the cloud coefficient data
    ! ...Write the dimensions
    WRITE( fid,IOSTAT=io_stat ) CloudCoeff%n_MW_Frequencies, &
                                CloudCoeff%n_MW_Radii      , &
                                CloudCoeff%n_IR_Frequencies, &
                                CloudCoeff%n_IR_Radii      , &
                                CloudCoeff%n_Temperatures  , &
                                CloudCoeff%n_Densities     , &
                                CloudCoeff%n_Legendre_Terms, &
                                CloudCoeff%n_Phase_Elements
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the dimension vectors
    WRITE( fid,IOSTAT=io_stat ) CloudCoeff%Frequency_MW, &
                                CloudCoeff%Frequency_IR, &
                                CloudCoeff%Reff_MW     , &
                                CloudCoeff%Reff_IR     , &
                                CloudCoeff%Temperature , &
                                CloudCoeff%Density
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing dimension vector data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the microwave liquid phase data
    WRITE( fid,IOSTAT=io_stat ) CloudCoeff%ke_L_MW    , &
                                CloudCoeff%w_L_MW     , &
                                CloudCoeff%g_L_MW     , &
                                CloudCoeff%pcoeff_L_MW
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing microwave liquid phase data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the microwave solid phase data
    WRITE( fid,IOSTAT=io_stat ) CloudCoeff%ke_S_MW    , &
                                CloudCoeff%w_S_MW     , &
                                CloudCoeff%g_S_MW     , &
                                CloudCoeff%pcoeff_S_MW
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing microwave solid phase data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the infrared data
    WRITE( fid,IOSTAT=io_stat ) CloudCoeff%ke_IR    , &
                                CloudCoeff%w_IR     , &
                                CloudCoeff%g_IR     , &
                                CloudCoeff%pcoeff_IR
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing infrared data. IOSTAT = ",i0)' ) io_stat
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
      CALL CloudCoeff_Info( CloudCoeff, msg )
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

  END FUNCTION CloudCoeff_Binary_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_Binary_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CloudCoeff_Binary_IOVersion( Id )
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

  SUBROUTINE CloudCoeff_Binary_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CloudCoeff_Binary_IOVersion

END MODULE CloudCoeff_Binary_IO
