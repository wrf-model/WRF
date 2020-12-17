      SUBROUTINE STATUS(LUNIT,LUN,IL,IM)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    STATUS
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE CHECKS WHETHER LOGICAL UNIT NUMBER LUNIT
C   (AND ITS ASSOCIATED BUFR FILE) IS CURRENTLY CONNECTED TO THE
C   BUFR ARCHIVE LIBRARY SOFTWARE.  IF SO, IT RETURNS THE I/O STREAM
C   INDEX (LUN) ASSOCIATED WITH THE LOGICAL UNIT NUMBER, THE LOGICAL
C   UNIT STATUS INDICATOR (IL), AND THE BUFR MESSAGE STATUS INDICATOR
C   (IM) FOR THAT I/O STREAM INDEX.  OTHERWISE, IT CHECKS WHETHER THERE
C   IS SPACE FOR A NEW I/O STREAM INDEX AND, IF SO, RETURNS THE NEXT
C   AVAILABLE I/O STREAM INDEX IN LUN IN ORDER TO DEFINE LUNIT (IL AND
C   IM ARE RETURNED AS ZERO, THEY ARE LATER DEFINED VIA CALLS TO BUFR
C   ARCHIVE LIBRARY SUBROUTINE WTSTAT IN THIS CASE).  IF THERE IS NO
C   SPACE FOR A NEW I/O STREAM INDEX, LUN IS RETURNED AS ZERO (AS WELL
C   AS IL AND IM) MEANING LUNIT COULD NOT BE CONNECTED TO THE BUFR
C   ARCHIVE LIBRARY SOFTWARE.  LUN IS USED TO IDENTIFY UP TO "NFILES"
C   UNIQUE BUFR FILES IN THE VARIOUS INTERNAL ARRAYS.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1996-12-11  J. WOOLLEN -- FIXED A LONG STANDING BUG WHICH OCCURS IN
C                           UNUSUAL SITUATIONS, VERY LOW IMPACT
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C
C USAGE:    CALL STATUS ( LUNIT, LUN, IL, IM )
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C
C   OUTPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX ASSOCIATED WITH LOGICAL UNIT
C                LUNIT
C                       0 = LUNIT is not currently connected to the
C                           BUFR Archive Library software and there is
C                           no space for a new I/O stream index
C     IL       - INTEGER: LOGICAL UNIT STATUS INDICATOR:
C                       0 = LUNIT is not currently connected to the
C                           BUFR Archive Library software or it was
C                           just connected in this call to STATUS
C                       1 = LUNIT is connected to the BUFR Archive
C                           Library software as an output file
C                      -1 = LUNIT is connected to the BUFR Archive
C                           Library software as an input file
C     IM       - INTEGER: INDICATOR AS TO WHETHER THERE IS A BUFR
C                MESSAGE CURRENTLY OPEN WITHIN MEMORY FOR THIS LUNIT:
C                       0 = no or LUNIT was just connected to the
C                           BUFR Archive Library software in this call
C                           to STATUS
C                       1 = yes
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT
C    THIS ROUTINE IS CALLED BY: CLOSBF   CLOSMG   COPYBF   COPYMG
C                               COPYSB   CPYMEM   DATEBF   DRFINI
C                               DUMPBF   DXDUMP   GETABDB  GETTAGPR
C                               GETVALNB IFBGET   IGETSC   INVMRG
C                               IUPVS01  LCMGDF   MESGBC   MINIMG
C                               MSGWRT   NMSUB    OPENBF   OPENMB
C                               OPENMG   POSAPX   RDMEMM   RDMEMS
C                               RDMGSB   READDX   READERME READLC
C                               READMG   READNS   READSB   REWNBF
C                               RTRCPT   STNDRD   UFBCNT   UFBCPY
C                               UFBCUP   UFBDMP   UFBEVN   UFBGET
C                               UFBIN3   UFBINT   UFBINX   UFBMMS
C                               UFBOVR   UFBPOS   UFBQCD   UFBQCP
C                               UFBREP   UFBRMS   UFBSEQ   UFBSTP
C                               UFBTAB   UFBTAM   UFDUMP   UPFTBV
C                               WRCMPS   WRDXTB   WRITLC   WRITSA
C                               WRITSB
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /STBFR/ IOLUN(NFILES),IOMSG(NFILES)

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(LUNIT.LE.0 .OR. LUNIT.GT.99) GOTO 900

C  CLEAR THE STATUS INDICATORS
C  ---------------------------

      LUN = 0
      IL  = 0
      IM  = 0

C  SEE IF UNIT IS ALREADY CONNECTED TO BUFR ARCHIVE LIBRARY SOFTWARE
C  -----------------------------------------------------------------

      DO I=1,NFILES
      IF(ABS(IOLUN(I)).EQ.LUNIT) LUN = I
      ENDDO

C  IF NOT, TRY TO DEFINE IT SO AS TO CONNECT IT TO BUFR ARCHIVE LIBRARY
C  SOFTWARE
C  --------------------------------------------------------------------

      IF(LUN.EQ.0) THEN
         DO I=1,NFILES
         IF(IOLUN(I).EQ.0) THEN

C  File space is available, return with LUN > 0, IL and IM remain 0
C  ----------------------------------------------------------------

            LUN = I
            GOTO 100
         ENDIF
         ENDDO

C  File space is NOT available, return with LUN, IL and IM all 0
C  -------------------------------------------------------------

         GOTO 100
      ENDIF

C  IF THE UNIT WAS ALREADY CONNECTED TO THE BUFR ARCHIVE LIBRARY
C   SOFTWARE PRIOR TO THIS CALL, RETURN STATUSES
C  -------------------------------------------------------------

      IL = SIGN(1,IOLUN(LUN))
      IM = IOMSG(LUN)

C  EXITS
C  ----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: STATUS - INPUT UNIT NUMBER (",I3,") '//
     . 'OUTSIDE LEGAL RANGE OF 1-99")') LUNIT
      CALL BORT(BORT_STR)
      END
