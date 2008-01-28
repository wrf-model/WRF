      FUNCTION INVTAG(NODE,LUN,INV1,INV2)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    INVTAG (docblock incomplete)
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS FUNCTION ....
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY) (INCOMPLETE); OUTPUTS MORE
C                           COMPLETE DIAGNOSTIC INFO WHEN UNUSUAL
C                           THINGS HAPPEN
C
C USAGE:    INVTAG (NODE, LUN, INV1, INV2)
C   INPUT ARGUMENT LIST:
C     NODE     - INTEGER: ....
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     INV1     - INTEGER: ....
C     INV2     - INTEGER: ....
C
C   OUTPUT ARGUMENT LIST:
C     INVTAG   - INTEGER: ....
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        None
C    THIS ROUTINE IS CALLED BY: UFBRP    UFBSEQ   UFBSP
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)
      COMMON /QUIET/  IPRT

      CHARACTER*10 TAG,TAGN
      CHARACTER*3  TYP
      REAL*8       VAL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      INVTAG = 0
      IF(NODE.EQ.0) GOTO 200
      TAGN = TAG(NODE)

C  SEARCH BETWEEN INV1 AND INV2
C  ----------------------------

10    DO INVTAG=INV1,INV2
      IF(TAG(INV(INVTAG,LUN)).EQ.TAGN) GOTO 100
      ENDDO

      INVTAG = 0

200   IF(IPRT.GE.2) THEN
      PRINT*
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
         PRINT*, 'BUFRLIB: INVTAG - INVTAG RETURNING WITH VALUE OF 0'
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT*
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
