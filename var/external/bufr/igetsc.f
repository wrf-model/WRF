      FUNCTION IGETSC(LUNIT)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    IGETSC
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2010-05-11
C
C ABSTRACT: THIS FUNCTION RETURNS ANY STATUS CODE THAT WAS INTERNALLY
C   SET WITHIN THE BUFR ARCHIVE LIBRARY SOFTWARE FOR A GIVEN LOGICAL
C   UNIT NUMBER
C
C PROGRAM HISTORY LOG:
C 2010-05-11  J. ATOR    -- ORIGINAL AUTHOR
C
C USAGE:    IGETSC (LUNIT)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     IGETSC   - INTEGER: STATUS CODE FOR LUNIT:
C                       0 = no problems noted with LUNIT
C                      -1 = unable to position LUNIT for appending,
C                           possibly due to an incomplete BUFR message
C                           at the end of the file
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     STATUS
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /STCODE/ ISCODES(NFILES)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Make sure the specified logical unit is connected to the library.

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

      IGETSC = ISCODES(LUN)

      RETURN
 900  CALL BORT('BUFRLIB: IGETSC - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
      END
