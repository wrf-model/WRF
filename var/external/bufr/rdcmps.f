      SUBROUTINE RDCMPS(LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDCMPS
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2000-09-19
C
C ABSTRACT: THIS SUBROUTINE UNCOMPRESSES AND UNPACKS THE NEXT SUBSET
C   FROM THE INTERNAL COMPRESSED MESSAGE BUFFER (ARRAY MBAY IN COMMON
C   BLOCK /BITBUF/) AND STORES THE UNPACKED SUBSET WITHIN THE INTERNAL
C   ARRAY VAL(*,LUN) IN COMMON BLOCK /USRINT/.
C
C PROGRAM HISTORY LOG:
C 2000-09-19  J. WOOLLEN -- ORIGINAL AUTHOR
C 2002-05-14  J. WOOLLEN -- IMPROVED GENERALITY, PREVIOUSLY RDCMPS
C                           WOULD NOT RECOGNIZE COMPRESSED DELAYED
C                           REPLICATION AS A LEGITIMATE DATA STRUCTURE
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED HISTORY DOCUMENTATION
C 2004-08-18  J. ATOR    -- INITIALIZE CVAL TO EMPTY BEFORE CALLING UPC;
C                           CORRECT LOGIC FOR WHEN A CHARACTER VALUE IS
C                           THE SAME FOR ALL SUBSETS IN A MESSAGE;
C                           MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C
C USAGE:    CALL RDCMPS (LUN)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    THIS ROUTINE CALLS:        UPB      UPC      USRTPL
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
      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES),
     .                INODE(NFILES),IDATE(NFILES)
      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)

      CHARACTER*10 TAG
      CHARACTER*8  CREF,CVAL
      CHARACTER*3  TYP
      EQUIVALENCE  (CVAL,RVAL)
      REAL*8       VAL,RVAL,UPS,TEN

      DATA TEN/10/

C-----------------------------------------------------------------------
C     Statement function to compute BUFR "missing value" for field
C     of length LBIT bits (all bits "on"):

      LPS(LBIT) = MAX(2**(LBIT)-1,1)

C     Statement function to decode the encoded BUFR value IVAL according
C     to the scale and reference values that are stored within index NODE
C     of the internal arrays ISC(*) and IRF(*):

      UPS(NODE) = (IVAL+IRF(NODE))*TEN**(-ISC(NODE))
C-----------------------------------------------------------------------

C  SETUP THE SUBSET TEMPLATE
C  -------------------------

      CALL USRTPL(LUN,1,1)

C  UNCOMPRESS A SUBSET INTO THE VAL ARRAY ACCORDING TO TABLE B
C  -----------------------------------------------------------

      NSBS = NSUB(LUN)

C     Note that we are going to unpack the (NSBS)th subset from within
C     the current BUFR message.

      IBIT = MBYT(LUN)

C     Loop through each element of the subset.

      N = 0

1     DO N=N+1,NVAL(LUN)
      NODE = INV(N,LUN)
      NBIT = IBT(NODE)
      ITYP = ITP(NODE)

C     In each of the following code blocks, the "local reference value"
C     for the element is determined first, followed by the 6-bit value
C     which indicates how many bits are used to store the increment
C     (i.e. offset) from this "local reference value".  Then, we jump
C     ahead to where this increment is stored for this particular subset,
C     unpack it, and add it to the "local reference value" to determine
C     the final uncompressed value for this element from this subset.

C     Note that, if an element has the same final uncompressed value
C     for each subset in the message, then the encoding rules for BUFR
C     compression dictate that the "local reference value" will be equal
C     to this value, the 6-bit increment length indicator will have
C     a value of zero, and the actual increments themselves will be
C     omitted from the message.

      IF(ITYP.EQ.1.OR.ITYP.EQ.2) THEN

C        This is a numeric element.

         CALL UPB(LREF,NBIT,MBAY(1,LUN),IBIT)
         CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
         JBIT = IBIT + LINC*(NSBS-1)
         CALL UPB(NINC,LINC,MBAY(1,LUN),JBIT)
         IF(NINC.EQ.LPS(LINC)) NINC = LPS(NBIT)
         IVAL = LREF+NINC
         IF(ITYP.EQ.1) THEN
            CALL USRTPL(LUN,N,IVAL)
            GOTO 1
         ENDIF
         IF(IVAL.LT.LPS(NBIT)) VAL(N,LUN) = UPS(NODE)
         IBIT = IBIT + LINC*MSUB(LUN)
      ELSEIF(ITYP.EQ.3) THEN

C        This is a character element.

         CREF = ' '
         CALL UPC(CREF,NBIT/8,MBAY(1,LUN),IBIT)
         CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
         IF(LINC.EQ.0) THEN
            CVAL = CREF
         ELSE
            JBIT = IBIT + LINC*(NSBS-1)*8
            CVAL = ' '
            CALL UPC(CVAL,LINC,MBAY(1,LUN),JBIT)
         ENDIF
         VAL(N,LUN) = RVAL
         IBIT = IBIT + 8*LINC*MSUB(LUN)
      ENDIF
      ENDDO

      RETURN
      END
