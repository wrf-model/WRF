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
C 2009-08-03  J. WOOLLEN -- ADDED CAPABILITY TO COPY LONG STRINGS VIA
C                           UFBCPY USING FILE POINTER STORED IN NEW
C                           COMMON UFBCPL
C 2012-03-02  J. ATOR    -- USE IPKS TO HANDLE 2-03 OPERATOR CASES
C 2012-06-04  J. ATOR    -- ENSURE "MISSING" CHARACTER FIELDS ARE
C                           PROPERLY ENCODED WITH ALL BITS SET TO 1 
C
C USAGE:    CALL WRTREE (LUN)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        IBFMS    IPKM     PKB      PKC
C                               IPKS     READLC
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
      COMMON /USRINT/ NVAL(NFILES),INV(MAXSS,NFILES),VAL(MAXSS,NFILES)
      COMMON /UFBCPL/ LUNCPY(NFILES) 

      CHARACTER*120 LSTR
      CHARACTER*10  TAG
      CHARACTER*8   CVAL
      CHARACTER*3   TYP
      DIMENSION     IVAL(MAXSS)
      EQUIVALENCE   (CVAL,RVAL)
      REAL*8        VAL,RVAL

C-----------------------------------------------------------------------

C  CONVERT USER NUMBERS INTO SCALED INTEGERS
C  -----------------------------------------

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).EQ.1) THEN
         IVAL(N) = VAL(N,LUN)
      ELSEIF(TYP(NODE).EQ.'NUM') THEN
         IF(IBFMS(VAL(N,LUN)).EQ.0) THEN
            IVAL(N) = IPKS(VAL(N,LUN),NODE)
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

C	 The value to be packed is a character string.

         NCR=IBT(NODE)/8
         IF ( NCR.GT.8 .AND. LUNCPY(LUN).NE.0 ) THEN

C	    The string is longer than 8 characters and there was a
C           preceeding call to UFBCPY involving this output unit, so
C           read the long string with READLC and write it into the
C           output buffer using PKC.

            CALL READLC(LUNCPY(LUN),LSTR,TAG(NODE))
            CALL PKC(LSTR,NCR,IBAY,IBIT)
         ELSE
            RVAL = VAL(N,LUN)
            IF(IBFMS(RVAL).NE.0) THEN

C              The value is "missing", so set all bits to 1 before
C              packing the field as a character string.

               NUMCHR = MIN(NCR,LEN(LSTR)) 
               DO JJ = 1, NUMCHR 
                  CALL IPKM(LSTR(JJ:JJ),1,255)
               ENDDO
               CALL PKC(LSTR,NUMCHR,IBAY,IBIT)
            ELSE

C              The value is not "missing", so pack the equivalenced
C              character string.  Note that a maximum of 8 characters
C              will be packed here, so a separate subsequent call to
C              BUFR archive library subroutine WRITLC will be needed to
C              fully encode any string longer than 8 characters.

               CALL PKC(CVAL,NCR,IBAY,IBIT)
            ENDIF
         ENDIF

      ENDIF
      ENDDO

C  RESET UFBCPY FILE POINTER
C  -------------------------

      LUNCPY(LUN)=0
            
      RETURN
      END
