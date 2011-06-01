      SUBROUTINE PKC(CHR,NCHR,IBAY,IBIT)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PKC
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE PACKS A CHARACTER STRING (CHR) CONTAINING
C   NCHR CHARACTERS INTO NCHR BYTES OF AN INTEGER ARRAY (IBAY),
C   STARTING WITH BIT (IBIT+1).  ON OUTPUT, IBIT IS UPDATED TO POINT TO
C   THE LAST BIT THAT WAS PACKED.  NOTE THAT THERE IS NO GUARANTEE THAT
C   THE NCHR CHARACTERS WILL BE ALIGNED ON BYTE BOUNDARIES WHEN PACKED
C   WITHIN IBAY.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  J. WOOLLEN -- BIG-ENDIAN/LITTLE-ENDIAN INDEPENDENT (WAS
C                           IN DECODER VERSION)
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY; CHANGED CALL FROM BORT TO BORT2
C 2004-08-18  J. ATOR    -- MODIFIED TO BE COMPATIBLE WITH WRITLC
C
C USAGE:    CALL PKC (CHR, NCHR, IBAY, IBIT)
C   INPUT ARGUMENT LIST:
C     CHR      - CHARACTER*(*): CHARACTER STRING TO BE PACKED
C     NCHR     - INTEGER: NUMBER OF BYTES OF IBAY WITHIN WHICH TO PACK
C                CHR (I.E., THE NUMBER OF CHARACTERS IN CHR)
C     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING BIT AFTER
C                WHICH TO START PACKING
C
C   OUTPUT ARGUMENT LIST:
C     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY NOW CONTAINING
C                PACKED CHR
C     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING LAST BIT
C                THAT WAS PACKED
C
C REMARKS:
C    THIS SUBROUTINE IS THE INVERSE OF BUFR ARCHIVE LIBRARY ROUTINE
C    UPC.
C
C    THIS ROUTINE CALLS:        IPKM     IREV     IUPM
C    THIS ROUTINE IS CALLED BY: CMSGINI  DXMINI   MSGINI   MSGWRT
C                               STNDRD   WRCMPS   WRITDX   WRITLC
C                               WRTREE
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      COMMON /CHARAC/ IASCII,IATOE(0:255),IETOA(0:255)
      COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

      CHARACTER*(*) CHR
      CHARACTER*1   CVAL(8)
      DIMENSION     IBAY(*),IVAL(2)
      EQUIVALENCE   (CVAL,IVAL)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      LB = IORD(NBYTW)

C     LB now points to the "low-order" (i.e. least significant) byte
C     within a machine word.

      IVAL(1) = 0
      NBIT = 8

      DO I=1,NCHR
      IF(I.LE.LEN(CHR)) THEN
         CVAL(LB) = CHR(I:I)
      ELSE
         CVAL(LB) = ' '
      ENDIF

C     If the machine is EBCDIC, then translate character CVAL(LB) from
C     EBCDIC to ASCII.

      IF(IASCII.EQ.0) CALL IPKM(CVAL(LB),1,IETOA(IUPM(CVAL(LB),8)))

      NWD  = IBIT/NBITW + 1
      NBT  = MOD(IBIT,NBITW)
      INT = ISHFT(IVAL(1),NBITW-NBIT)
      INT = ISHFT(INT,-NBT)
      MSK = ISHFT(  -1,NBITW-NBIT)
      MSK = ISHFT(MSK,-NBT)
      IBAY(NWD) = IREV(IOR(IAND(IREV(IBAY(NWD)),NOT(MSK)),INT))
      IF(NBT+NBIT.GT.NBITW) THEN

C        This character will not fit within the current word (i.e.
C        array member) of IBAY, because there are less than 8 bits of
C        space left.  Store as many bits as will fit within the current
C        word and then store the remaining bits within the next word.

         INT = ISHFT(IVAL(1),2*NBITW-(NBT+NBIT))
         MSK = ISHFT(  -1,2*NBITW-(NBT+NBIT))
         IBAY(NWD+1) = IREV(IOR(IAND(IREV(IBAY(NWD+1)),NOT(MSK)),INT))
      ENDIF
      IBIT = IBIT + NBIT
      ENDDO

C  EXITS
C  -----

      RETURN
      END
