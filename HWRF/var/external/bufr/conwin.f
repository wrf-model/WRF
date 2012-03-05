      SUBROUTINE CONWIN(LUN,INC1,INC2,NBMP)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CONWIN (docblock incomplete)
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE ....
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- IMPROVED MACHINE PORTABILITY
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
C                           HISTORY) (INCOMPLETE)
C
C USAGE:    CALL CONWIN (LUN, INC1, INC2, NBMP)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     INC1     - INTEGER: ....
C     INC2     - INTEGER: ....
C     NBMP     - INTEGER: ....
C
C   OUTPUT ARGUMENT LIST:
C     INC1     - INTEGER: ....
C     INC2     - INTEGER: ....
C
C REMARKS:
C    THIS ROUTINE CALLS:        GETWIN   INVCON   INVWIN   NEWWIN
C                               NXTWIN   USRTPL
C    THIS ROUTINE IS CALLED BY: UFBEVN   UFBIN3   UFBRW
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)
      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)

      REAL*8 VAL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  SPECIAL CASES
C  -------------

c  .... There are no condition nodes in string
      IF(NCON.EQ.0) THEN
         INC1 = 1
         INC2 = NVAL(LUN)
         GOTO 100
      ENDIF

c  .... BUMP node
      IF(INC1.GT.1 .AND. KONS(NCON).EQ.5) THEN
         CALL NXTWIN(LUN,INC1,INC2)
         GOTO 100
      ENDIF

C  EVALUATE CONDITIONS TO SEE IF ANY MORE CASES
C  --------------------------------------------

10    DO NC=1,NCON
      IF(KONS(NC).EQ.5) THEN
         INC1 = INVWIN(NODC(NC),LUN,INC1,NVAL(LUN))
         CALL USRTPL(LUN,INC1-1,NBMP)
         CALL NEWWIN(LUN,INC1,INC2)
      ELSE
15       CALL GETWIN(NODC(NC),LUN,INC1,INC2)
         IF(INC1.EQ.0 .AND. NC.EQ.1) GOTO 100
         IF(INC1.EQ.0              ) GOTO 10
         ICON = INVCON(NC,LUN,INC1,INC2)
         IF(ICON.EQ.0) GOTO 15
      ENDIF
      ENDDO

C  EXIT
C  ----

100   RETURN
      END
