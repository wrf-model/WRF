      SUBROUTINE JSTCHR(STR,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    JSTCHR
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE LEFT-JUSTIFIES (I.E. REMOVES ALL LEADING
C   BLANKS FROM) A CHARACTER STRING.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN JSTIFY)
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT" (IN PARENT ROUTINE JSTIFY)
C 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE
C                           PORTABILITY TO OTHER PLATFORMS (JSTIFY WAS
C                           THEN REMOVED BECAUSE IT WAS JUST A DUMMY
C                           ROUTINE WITH ENTRIES)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C 2007-01-19  J. ATOR    -- RESTRUCTURED AND ADDED IRET ARGUMENT
C
C USAGE:    CALL JSTCHR (STR, IRET)
C   INPUT ARGUMENT LIST:
C     STR      - CHARACTER*(*): STRING
C
C   OUTPUT ARGUMENT LIST:
C     STR      - CHARACTER*(*): COPY OF INPUT STR WITH LEADING BLANKS
C                REMOVED
C     IRET     - INTEGER: RETURN CODE:
C                       0 = normal return
C                      -1 = input string was empty (i.e. all blanks)
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: ELEMDX   IGETFXY  SNTBBE
C                               Normally not called by any application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*(*) STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(STR.EQ.' ') THEN
         IRET = -1
      ELSE
         IRET = 0
         LSTR = LEN(STR)
         DO WHILE(STR(1:1).EQ.' ')
            STR = STR(2:LSTR)
         ENDDO
      ENDIF

      RETURN
      END
