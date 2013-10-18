    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_SetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i

    ! Allocate structure if necessary    
    IF ( .NOT. FitCoeff_Associated(self) ) THEN
      CALL FitCoeff_Create( self, SHAPE(C) )
      IF ( .NOT. FitCoeff_Associated(self) ) THEN
        msg = 'Allocation of FitCoeff structure failed'
        CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
      END IF
    END IF
    
    
    ! Check dimensions are consistent
    DO i = 1, SIZE(self%Dimensions)
      IF ( self%Dimensions(i) /= SIZE(C,DIM=i) ) THEN
        WRITE( msg,'("Different dimension ",i0," size between ",&
                    &"structure (",i0,") and array (",i0,")")' ) &
                   i, self%Dimensions(i) /= SIZE(C,DIM=i)
        CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
        CALL FitCoeff_Destroy( self )
        RETURN
      END IF
    END DO


    ! Assign the coefficient data
    self%C = C


    ! Optional arguments
    ! ...Version
    IF ( PRESENT(Version) ) self%Version = Version
