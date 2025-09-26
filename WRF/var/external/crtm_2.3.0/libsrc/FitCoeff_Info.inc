    CHARACTER(*),           INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"FitCoeff RELEASE.VERSION : ",i0,".",i0,"; ", &
           &"DIMENSIONS=",10(1x,i0,:,",") )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           self%Dimensions    
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

