!
! String_Utility
!
! Module containing string utility routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!

MODULE String_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------
  ! Everything private by default
  PRIVATE
  ! Public procedures
  PUBLIC :: StrUpCase
  PUBLIC :: StrLowCase
  PUBLIC :: StrCompress
  PUBLIC :: StrClean


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE StrClean
    MODULE PROCEDURE StrClean_scalar
    MODULE PROCEDURE StrClean_rank1
  END INTERFACE StrClean


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: String_Utility.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! List of character for case conversion
  CHARACTER(*), PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
  CHARACTER(*), PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       StrUpCase
!
! PURPOSE:
!       Function to convert an input string to upper case.
!
! CALLING SEQUENCE:
!       Result = StrUpCase( String )
!
! INPUT ARGUMENTS:
!       String:  Character string to be converted to upper case.
!                UNITS:      N/A
!                TYPE:       CHARACTER(*)
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Result:  The input character string converted to upper case.
!                UNITS:      N/A
!                TYPE:       CHARACTER(LEN(String))
!                DIMENSION:  Scalar
!
! EXAMPLE:
!       string = 'this is a string'
!       WRITE( *, '( a )' ) StrUpCase( string )
!   THIS IS A STRING
!
! PROCEDURE:
!       Figure 3.5B, pg 80, "Upgrading to Fortran 90", by Cooper Redwine,
!       1995 Springer-Verlag, New York.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION StrUpCase( Input_String ) RESULT( Output_String )
    ! Arguments
    CHARACTER(*), INTENT(IN)     :: Input_String
    ! Function result
    CHARACTER(LEN(Input_String)) :: Output_String
    ! Local variables
    INTEGER :: i, n

    ! Copy input string
    Output_String = Input_String

    ! Convert case character by character
    DO i = 1, LEN(Output_String)
      n = INDEX(LOWER_CASE, Output_String(i:i))
      IF ( n /= 0 ) Output_String(i:i) = UPPER_CASE(n:n)
    END DO
  END FUNCTION StrUpCase


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       StrLowCase
!
! PURPOSE:
!       Function to convert an input string to lower case.
!
! CALLING SEQUENCE:
!       Result = StrLowCase( String )
!
! INPUT ARGUMENTS:
!       String: Character string to be converted to lower case.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Result:  The input character string converted to lower case.
!                UNITS:      N/A
!                TYPE:       CHARACTER( LEN(String) )
!                DIMENSION:  Scalar
!
! EXAMPLE:
!       string = 'THIS IS A STRING'
!       WRITE( *, '( a )' ) StrLowCase( string )
!   this is a string
!
! PROCEDURE:
!       Figure 3.5B, pg 80, "Upgrading to Fortran 90", by Cooper Redwine,
!       1995 Springer-Verlag, New York.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION StrLowCase( Input_String ) RESULT( Output_String )
    ! Argument
    CHARACTER(*), INTENT(IN)     :: Input_String
    ! Function result
    CHARACTER(LEN(Input_String)) :: Output_String
    ! Local variables
    INTEGER :: i, n

    ! Copy input string
    Output_String = Input_String

    ! Convert case character by character
    DO i = 1, LEN(Output_String)
      n = INDEX(UPPER_CASE, Output_String(i:i))
      IF ( n /= 0 ) Output_String(i:i) = LOWER_CASE(n:n)
    END DO
  END FUNCTION StrLowCase


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       StrCompress
!
! PURPOSE:
!       Subroutine to return a copy of an input string with all whitespace
!       (spaces and tabs) removed.
!
! CALLING SEQUENCE:
!       Result = StrCompress( String,  &  ! Input
!                             n = n    )  ! Optional Output
!
! INPUT ARGUMENTS:
!       String:         Character string to be compressed.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n:              Number of useful characters in output string
!                       after compression. From character n+1 -> LEN(Input_String)
!                       the output is padded with blanks.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Result:         Input string with all whitespace removed before the
!                       first non-whitespace character, and from in-between 
!                       non-whitespace characters.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(LEN(String))
!                       DIMENSION:  Scalar
!
! EXAMPLE:
!       Input_String = '  This is a string with spaces in it.'
!       Output_String = StrCompress( Input_String, n=n )
!       WRITE( *, '( a )' ) '>',Output_String( 1:n ),'<'
!   >Thisisastringwithspacesinit.<
!
!       or
!
!       WRITE( *, '( a )' ) '>',TRIM( Output_String ),'<'
!   >Thisisastringwithspacesinit.<
!
! PROCEDURE:
!       Definitions of a space and a tab character are made for the
!       ASCII collating sequence. Each single character of the input
!       string is checked against these definitions using the IACHAR()
!       intrinsic. If the input string character DOES NOT correspond 
!       to a space or tab, it is not copied to the output string.
!
!       Note that for input that ONLY has spaces or tabs BEFORE the first
!       useful character, the output of this function is the same as the
!       ADJUSTL() instrinsic.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION StrCompress( Input_String, n ) RESULT( Output_String )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Input_String
    INTEGER, OPTIONAL, INTENT(OUT) :: n
    ! Function result
    CHARACTER(LEN(Input_String)) :: Output_String
    ! Local parameters
    INTEGER, PARAMETER :: IACHAR_SPACE = 32
    INTEGER, PARAMETER :: IACHAR_TAB   = 9
    ! Local variables
    INTEGER :: i, j
    INTEGER :: IACHAR_Character

    ! Setup
    ! -----
    ! Initialise output string
    Output_String = ' '
    ! Initialise output string "useful" length counter
    j = 0

    ! Loop over string contents character by character
    ! ------------------------------------------------
    DO i = 1, LEN(Input_String)

      ! Convert the current character to its position
      ! in the ASCII collating sequence
      IACHAR_Character = IACHAR(Input_String(i:i))

      ! If the character is NOT a space ' ' or a tab '->|'
      ! copy it to the output string.
      IF ( IACHAR_Character /= IACHAR_SPACE .AND. &
           IACHAR_Character /= IACHAR_TAB         ) THEN
        j = j + 1
        Output_String(j:j) = Input_String(i:i)
      END IF

    END DO

    ! Save the non-whitespace count
    ! -----------------------------
    IF ( PRESENT(n) ) n = j

  END FUNCTION StrCompress



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       StrClean
!
! PURPOSE:
!       Subroutine to replace terminating NULL characters (ASCII 0, \0 in C)
!       in an input string with whitespace.
!
! CALLING SEQUENCE:
!       CALL StrClean( String )
!
! INPUT ARGUMENTS:
!       String:  On input, this argument contains the character string or
!                string array from which NULL characters are to be
!                removed.
!                UNITS:      N/A
!                TYPE:       CHARACTER(*)
!                DIMENSION:  Scalar or Rank-1
!                ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       String:  On output, this argument contains the character string or
!                string array from which the NULL characters have been
!                converted to whitespace.
!                UNITS:      N/A
!                TYPE:       CHARACTER(*)
!                DIMENSION:  Scalar or Rank-1
!                ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       The String argument has INTENT(IN OUT) and its contents are modified
!       as required to remove NULL Characters.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE StrClean_scalar( String )
    ! Arguments
    CHARACTER(*), INTENT(IN OUT) :: String
    ! Local parameters
    INTEGER, PARAMETER :: IACHAR_NULL = 0
    ! Local variables
    INTEGER :: i
    
    ! Search for null character
    Character_Loop: DO i = 1, LEN(String)
      IF ( IACHAR(String(i:i)) == IACHAR_NULL ) THEN
        String(i:LEN(String) ) = ' '
        EXIT Character_Loop
      END IF
    END DO Character_Loop
  END SUBROUTINE StrClean_scalar

  SUBROUTINE StrClean_rank1( String )
    ! Arguments
    CHARACTER(*), INTENT(IN OUT) :: String(:)
    ! Local variables
    INTEGER :: n
    DO n = 1, SIZE(String)
      CALL StrClean_scalar( String(n) )
    END DO
  END SUBROUTINE StrClean_rank1

END MODULE String_Utility
