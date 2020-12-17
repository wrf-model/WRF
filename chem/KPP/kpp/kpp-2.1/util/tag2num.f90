! ****************************************************************
!                            
! tag2num - convert equation tags to kpp reaction number
!   Arguments :
!      id        - string with the equation tag
!
! ****************************************************************

ELEMENTAL INTEGER FUNCTION tag2num ( id )

  USE KPP_ROOT_Monitor, ONLY: EQN_TAGS

  CHARACTER(LEN=*), INTENT(IN) :: id
  INTEGER i

  tag2num = 0 ! mz_rs_20050115
  DO i = 1, SIZE(EQN_TAGS)
    IF (TRIM(EQN_TAGS(i)) == TRIM(id)) THEN
      tag2num = i ! mz_rs_20050115
      EXIT
    ENDIF
  END DO

END FUNCTION tag2num

! End of tag2num function
! ****************************************************************

