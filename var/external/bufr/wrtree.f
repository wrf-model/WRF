      SUBROUTINE WRTREE(LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRTREE
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE CONVERTS USER NUMBERS INTO SCALED INTEGERS
C   AND PACKS THE USER ARRAY INTO THE SUBSET BUFFER.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- CORRECTED SOME MINOR ERRORS
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY); REPL. "IVAL(N)=ANINT(PKS(NODE))"
C                           WITH "IVAL(N)=NINT(PKS(NODE))" (FORMER
C                           CAUSED PROBLEMS ON SOME FOREIGN MACHINES)
C 2004-03-10  J. WOOLLEN -- CONVERTED PACKING FUNCTION 'PKS' TO REAL*8 
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2007-01-19  J. ATOR    -- PREVENT OVERFLOW OF CVAL FOR STRINGS LONGER
C                           THAN 8 CHARACTERS; USE FUNCTION IBFMS
C
C USAGE:    CALL WRTREE (LUN)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        IBFMS    PKB      PKC
C    THIS ROUTINE IS CALLED BY: WRITSA   WRITSB
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGLD4),MBYT(NFILES),
     .                MBAY(MXMSGLD4,NFILES)
      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)

      CHARACTER*10 TAG
      CHARACTER*8  CVAL
      CHARACTER*3  TYP
      DIMENSION    IVAL(MAXJL)
      EQUIVALENCE  (CVAL,RVAL)
      REAL*8       VAL,RVAL,PKS,TEN

      DATA         TEN  /10./

C-----------------------------------------------------------------------
      PKS(NODE) = VAL(N,LUN)*TEN**ISC(NODE)-IRF(NODE)
C-----------------------------------------------------------------------

C  CONVERT USER NUMBERS INTO SCALED INTEGERS
C  -----------------------------------------

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).EQ.1) THEN
         IVAL(N) = VAL(N,LUN)
      ELSEIF(TYP(NODE).EQ.'NUM') THEN
         IF(IBFMS(VAL(N,LUN)).EQ.0) THEN
            IVAL(N) = NINT(PKS(NODE))
         ELSE
            IVAL(N) = -1
         ENDIF
      ENDIF
      ENDDO

C  PACK THE USER ARRAY INTO THE SUBSET BUFFER
C  ------------------------------------------

      IBIT = 16

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).LT.3) THEN

C	 The value to be packed is numeric.

         CALL PKB(IVAL(N),IBT(NODE),IBAY,IBIT)
      ELSE

C	 The value to be packed is a character string.  If the string is
C	 longer than 8 characters, then only the first 8 will be packed
C	 by this routine, and a separate subsequent call to BUFR archive
C	 library subroutine WRITLC will be required to pack the
C	 remainder of the string.

         RVAL = VAL(N,LUN)
         NBT = MIN(8,IBT(NODE)/8)
         CALL PKC(CVAL,NBT,IBAY,IBIT)
      ENDIF
      ENDDO

      RETURN
      END
