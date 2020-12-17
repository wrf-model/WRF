    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. FitCoeff_Associated(x)) .OR. &
         (.NOT. FitCoeff_Associated(y))      ) RETURN

   ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( ALL(x%Dimensions /= y%Dimensions ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%C .EqualTo. y%C) ) &
      is_equal = .TRUE.
