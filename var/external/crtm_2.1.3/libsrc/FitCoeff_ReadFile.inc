    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: release
    INTEGER :: version
    INTEGER :: n_dims
    INTEGER :: dims(FITCOEFF_MAX_N_DIMENSIONS)

    
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


    ! Read the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      release, &
      version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. ValidRelease( release ) ) THEN
      msg = 'FitCoeff Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimension data
    ! ...The number of dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      n_dims
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading number of dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check the value
    IF ( n_dims > SIZE(dummy%Dimensions) .OR. &
         n_dims > FITCOEFF_MAX_N_DIMENSIONS ) THEN
      WRITE( msg,'("Number of dimensions (",i0,") in ",a," is greater than allowed for datatype (",i0,")")' ) &
                 n_dims, TRIM(Filename), MIN(SIZE(dummy%Dimensions),FITCOEFF_MAX_N_DIMENSIONS)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...The dimension values
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dims(1:n_dims)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimension values from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL FitCoeff_Create( &
           FitCoeff, &
           dims(1:n_dims) )                  
    IF ( .NOT. FitCoeff_Associated( FitCoeff ) ) THEN
      msg = 'FitCoeff object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    FitCoeff%Version = version
        

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
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      FitCoeff%C      
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading coefficient data - '//TRIM(io_msg)
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
      CALL FitCoeff_Info( FitCoeff, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS
   
    SUBROUTINE Read_CleanUp()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL FitCoeff_Destroy( FitCoeff )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
