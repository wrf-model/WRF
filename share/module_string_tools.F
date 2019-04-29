MODULE module_string_tools

CONTAINS

    FUNCTION capitalize(str) RESULT(capStr)
!<DESCRIPTION>
!
! Returns a copy of 'str' in which all lower-case letters have been converted
! to upper-case letters.
!
!</DESCRIPTION>

        CHARACTER(LEN=*), INTENT(IN) :: str
        CHARACTER(LEN=LEN(str)) :: capStr

        INTEGER :: i
        INTEGER, PARAMETER :: offset = (IACHAR('a') - IACHAR('A'))


        DO i=1,LEN(str)
            IF ( ( IACHAR(str(i:i)) >= IACHAR('a') ) .AND. ( IACHAR(str(i:i)) <= IACHAR('z') ) ) THEN
               capStr(i:i) = ACHAR(IACHAR(str(i:i)) - offset)
            ELSE
               capStr(i:i) = str(i:i)
            END IF
        END Do

    END FUNCTION capitalize

END MODULE module_string_tools
