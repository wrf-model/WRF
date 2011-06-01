      SUBROUTINE RDTREE(LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDTREE
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE UNPACKS THE NEXT SUBSET FROM THE INTERNAL
C   UNCOMPRESSED MESSAGE BUFFER (ARRAY MBAY IN COMMON BLOCK /BITBUF/)
C   AND STORES THE UNPACKED SUBSET WITHIN THE INTERNAL ARRAY VAL(*,LUN)
C   IN COMMON BLOCK /USRINT/.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-
C                           LINING CODE WITH FPP DIRECTIVES
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2003-11-04  J. WOOLLEN -- FIXED A BUG WHICH COULD ONLY OCCUR WHEN
C                           THE LAST ELEMENT IN A SUBSET IS A CHARACTER
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY)
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2007-01-19  J. ATOR    -- PREVENT OVERFLOW OF CVAL FOR STRINGS LONGER
C                           THAN 8 CHARACTERS
C
C USAGE:    CALL RDTREE (LUN)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        RCSTPL   UPBB     UPC
C    THIS ROUTINE IS CALLED BY: READSB
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
      COMMON /USRBIT/ NBIT(MAXJL),MBIT(MAXJL)

      CHARACTER*10 TAG
      CHARACTER*8  CVAL
      CHARACTER*3  TYP
      DIMENSION    IVAL(MAXJL)
      EQUIVALENCE  (CVAL,RVAL)
      REAL*8       VAL,RVAL,UPS,TEN

      DATA TEN/10./

C-----------------------------------------------------------------------
C     Statement function to compute BUFR "missing value" for field
C     of length IBT(NODE)) bits (all bits "on"):

      MPS(NODE) = 2**(IBT(NODE))-1

C     Statement function to decode the unpacked BUFR value IVAL according
C     to the scale and reference values that are stored within index NODE
C     of the internal arrays ISC(*) and IRF(*):

      UPS(NODE) = (IVAL(N)+IRF(NODE))*TEN**(-ISC(NODE))
C-----------------------------------------------------------------------

C  CYCLE THROUGH A SUBSET SETTING UP THE TEMPLATE
C  ----------------------------------------------

      MBIT(1) = IBIT
      NBIT(1) = 0
      CALL RCSTPL(LUN)

C  UNPACK A SUBSET INTO THE USER ARRAY IVAL
C  ----------------------------------------

      DO N=1,NVAL(LUN)
      CALL UPBB(IVAL(N),NBIT(N),MBIT(N),MBAY(1,LUN))
      ENDDO

C  LOOP THROUGH EACH ELEMENT OF THE SUBSET, CONVERTING THE UNPACKED
C  VALUES TO THE PROPER TYPES
C  ----------------------------------------------------------------

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).EQ.1) THEN

C	 The unpacked value is a delayed descriptor replication factor.

         VAL(N,LUN) = IVAL(N)
      ELSEIF(ITP(NODE).EQ.2) THEN

C	 The unpacked value is a real.

         IF(IVAL(N).LT.MPS(NODE)) VAL(N,LUN) = UPS(NODE)
      ELSEIF(ITP(NODE).EQ.3) THEN

C	 The unpacked value is a character string.  If the string is
C	 longer than 8 characters, then only the first 8 will be
C	 unpacked by this routine, and a separate subsequent call to
C	 BUFR archive library subroutine READLC will be required to
C	 unpack the remainder of the string.

         CVAL = ' '
         KBIT = MBIT(N)
         NBT = MIN(8,NBIT(N)/8)
         CALL UPC(CVAL,NBT,MBAY(1,LUN),KBIT)
         VAL(N,LUN) = RVAL
      ENDIF
      ENDDO

      IBIT = NBIT(NVAL(LUN))+MBIT(NVAL(LUN))

      RETURN
      END
