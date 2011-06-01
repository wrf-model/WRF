      SUBROUTINE DRSTPL(INOD,LUN,INV1,INV2,INVN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DRSTPL (docblock incomplete)
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE ....
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT" (LATER REMOVED, UNKNOWN
C                           WHEN)
C 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY) (INCOMPLETE)
C
C USAGE:    CALL DRSTPL (INOD, LUN, INV1, INV2, INVN)
C   INPUT ARGUMENT LIST:
C     INOD     - INTEGER: ....
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     INV1     - INTEGER: ....
C     INV2     - INTEGER: ....
C     INVN     - INTEGER: ....
C
C   OUTPUT ARGUMENT LIST:
C     INVN     - INTEGER: ....
C
C REMARKS:
C    THIS ROUTINE CALLS:        INVWIN   NEWWIN   USRTPL
C    THIS ROUTINE IS CALLED BY: UFBRW
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

      CHARACTER*10 TAG
      CHARACTER*3  TYP

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

1     NODE = INOD
2     NODE = JMPB(NODE)
      IF(NODE.EQ.0) GOTO 100
      IF(TYP(NODE).EQ.'DRS' .OR. TYP(NODE).EQ.'DRB') THEN
         INVN = INVWIN(NODE,LUN,INV1,INV2)
         IF(INVN.GT.0) THEN
            CALL USRTPL(LUN,INVN,1)
            CALL NEWWIN(LUN,INV1,INV2)
            INVN = INVWIN(INOD,LUN,INVN,INV2)
            IF(INVN.GT.0) GOTO 100
            GOTO 1
         ENDIF
      ENDIF
      GOTO 2

C  EXIT
C  ----

100   RETURN
      END
