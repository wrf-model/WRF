      SUBROUTINE TABENT(LUN,NEMO,TAB,ITAB,IREP,IKNT,JUM0)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    TABENT
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE BUILDS AND STORES AN ENTRY FOR A TABLE B OR
C   TABLE D MNEMONIC (NEMO) WITHIN THE INTERNAL JUMP/LINK TABLE.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"; CORRECTED SOME MINOR ERRORS
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED HISTORY DOCUMENTATION; OUTPUTS
C                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY
C 2005-11-29  J. ATOR    -- ADDED SUPPORT FOR 207 AND 208 OPERATORS
C
C USAGE:    CALL TABENT (LUN, NEMO, TAB, ITAB, IREP, IKNT, JUM0)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     NEMO     - CHARACTER*8: TABLE B OR D MNEMONIC TO STORE IN JUMP/
C                LINK TABLE
C     TAB      - CHARACTER*1: INTERNAL BUFR TABLE ARRAY ('B' OR 'D') IN
C                WHICH NEMO IS DEFINED
C     ITAB     - INTEGER: POSITIONAL INDEX OF NEMO WITHIN TAB
C     IREP     - INTEGER: POSITIONAL INDEX WITHIN COMMON /REPTAB/
C                ARRAYS, FOR USE WHEN NEMO IS REPLICATED:
C                       0 = NEMO is not replicated
C     IKNT     - INTEGER: NUMBER OF REPLICATIONS, FOR USE WHEN NEMO IS
C                REPLICATED USING F=1 REGULAR (I.E., NON-DELAYED)
C                REPLICATION:
C                       0 = NEMO is not replicated using F=1 regular
C                           (i.e., non-delayed) replication
C     JUM0     - INTEGER: INDEX VALUE TO BE STORED FOR NEMO WITHIN
C                INTERNAL JUMP/LINK TABLE ARRAY JMPB(*)
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     INCTAB   NEMTBB
C    THIS ROUTINE IS CALLED BY: TABSUB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

C     Note that the values within the COMMON /REPTAB/ arrays were
C     initialized within subroutine BFRINI.

      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)

      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /TABCCC/ ICDW,ICSC,ICRV,INCW

      CHARACTER*128 BORT_STR
      CHARACTER*24  UNIT
      CHARACTER*10  TAG,RTAG
      CHARACTER*8   NEMO
      CHARACTER*3   TYP,TYPS,TYPT
      CHARACTER*1   REPS,TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  MAKE A JUMP/LINK TABLE ENTRY FOR A REPLICATOR
C  ---------------------------------------------

      IF(IREP.NE.0) THEN
         RTAG = REPS(IREP,1)//NEMO
         DO I=1,10
         IF(RTAG(I:I).EQ.' ') THEN
            RTAG(I:I) = REPS(IREP,2)
            CALL INCTAB(RTAG,TYPS(IREP,1),NODE)
            JUMP(NODE) = NODE+1
            JMPB(NODE) = JUM0
            LINK(NODE) = 0
            IBT (NODE) = LENS(IREP)
            IRF (NODE) = 0
            ISC (NODE) = 0
            IF(IREP.EQ.1) IRF(NODE) = IKNT
            JUM0 = NODE
            GOTO 1
         ENDIF
         ENDDO
         GOTO 900
      ENDIF

C  MAKE AN JUMP/LINK ENTRY FOR AN ELEMENT OR A SEQUENCE
C  ----------------------------------------------------

1     IF(TAB.EQ.'B') THEN
         CALL NEMTBB(LUN,ITAB,UNIT,ISCL,IREF,IBIT)
         IF(UNIT(1:5).EQ.'CCITT') THEN
            TYPT = 'CHR'
         ELSE
            TYPT = 'NUM'
         ENDIF
         CALL INCTAB(NEMO,TYPT,NODE)
         JUMP(NODE) = 0
         JMPB(NODE) = JUM0
         LINK(NODE) = 0
         IBT (NODE) = IBIT
         IRF (NODE) = IREF
         ISC (NODE) = ISCL
         IF(UNIT(1:4).EQ.'CODE') THEN
            TYPT = 'COD'
         ELSEIF(UNIT(1:4).EQ.'FLAG') THEN
            TYPT = 'FLG'
         ENDIF
         IF(TYPT.EQ.'NUM') THEN
            IBT(NODE) = IBT(NODE) + ICDW
            ISC(NODE) = ISC(NODE) + ICSC
            IRF(NODE) = IRF(NODE) * ICRV
         ELSEIF( (TYPT.EQ.'CHR') .AND. (INCW.GT.0) ) THEN
            IBT(NODE) = INCW * 8
         ENDIF
      ELSEIF(TAB.EQ.'D') THEN
         IF(IREP.EQ.0) THEN
            TYPT = 'SEQ'
         ELSE
            TYPT = TYPS(IREP,2)
         ENDIF
         CALL INCTAB(NEMO,TYPT,NODE)
         JUMP(NODE) = NODE+1
         JMPB(NODE) = JUM0
         LINK(NODE) = 0
         IBT (NODE) = 0
         IRF (NODE) = 0
         ISC (NODE) = 0
      ELSE
         GOTO 901
      ENDIF

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: TABENT - REPLICATOR ERROR FOR INPUT '//
     . 'MNEMONIC ",A,", RTAG IS ",A)') NEMO,RTAG
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: TABENT - UNDEFINED TAG (",A,") FOR '//
     . 'INPUT MNEMONIC ",A)') TAB,NEMO
      CALL BORT(BORT_STR)
      END
