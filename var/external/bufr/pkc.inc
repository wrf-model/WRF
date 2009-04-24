      SUBROUTINE PKC(CHR,NCHR,IBAY,IBIT)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    PKC
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE PACKS A CHARACTER STRING (CHR) CONTAINING
!   NCHR CHARACTERS INTO NCHR BYTES OF AN INTEGER ARRAY (IBAY),
!   STARTING WITH BIT (IBIT+1).  ON OUTPUT, IBIT IS UPDATED TO POINT TO
!   THE LAST BIT THAT WAS PACKED.  NOTE THAT THERE IS NO GUARANTEE THAT
!   THE NCHR CHARACTERS WILL BE ALIGNED ON BYTE BOUNDARIES WHEN PACKED
!   WITHIN IBAY.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  J. WOOLLEN -- BIG-ENDIAN/LITTLE-ENDIAN INDEPENDENT (WAS
!                           IN DECODER VERSION)
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
!                           ABNORMALLY; CHANGED CALL FROM BORT TO BORT2
! 2004-08-18  J. ATOR    -- MODIFIED TO BE COMPATIBLE WITH WRITLC
!
! USAGE:    CALL PKC (CHR, NCHR, IBAY, IBIT)
!   INPUT ARGUMENT LIST:
!     CHR      - CHARACTER*(*): CHARACTER STRING TO BE PACKED
!     NCHR     - INTEGER: NUMBER OF BYTES OF IBAY WITHIN WHICH TO PACK
!                CHR (I.E., THE NUMBER OF CHARACTERS IN CHR)
!     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING BIT AFTER
!                WHICH TO START PACKING
!
!   OUTPUT ARGUMENT LIST:
!     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY NOW CONTAINING
!                PACKED CHR
!     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING LAST BIT
!                THAT WAS PACKED
!
! REMARKS:
!    THIS SUBROUTINE IS THE INVERSE OF BUFR ARCHIVE LIBRARY ROUTINE
!    UPC.
!
!    THIS ROUTINE CALLS:        IPKM     IREV     IUPM
!    THIS ROUTINE IS CALLED BY: CMSGINI  DXMINI   MSGINI   MSGWRT
!                               STNDRD   WRCMPS   WRITDX   WRITLC
!                               WRTREE
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      COMMON /CHARAC/ IASCII,IATOE(0:255),IETOA(0:255)
      COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

      CHARACTER*(*) CHR
      CHARACTER*1   CVAL(8)
      DIMENSION     IBAY(*),IVAL(2)
      EQUIVALENCE   (CVAL,IVAL)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      LB = IORD(NBYTW)

!     LB now points to the "low-order" (i.e. least significant) byte
!     within a machine word.

      IVAL(1) = 0
      NBIT = 8

      DO I=1,NCHR
      IF(I.LE.LEN(CHR)) THEN
         CVAL(LB) = CHR(I:I)
      ELSE
         CVAL(LB) = ' '
      ENDIF

!     If the machine is EBCDIC, then translate character CVAL(LB) from
!     EBCDIC to ASCII.

      IF(IASCII.EQ.0) CALL IPKM(CVAL(LB),1,IETOA(IUPM(CVAL(LB),8)))

      NWD  = IBIT/NBITW + 1
      NBT  = MOD(IBIT,NBITW)
      INT = ISHFT(IVAL(1),NBITW-NBIT)
      INT = ISHFT(INT,-NBT)
      MSK = ISHFT(  -1,NBITW-NBIT)
      MSK = ISHFT(MSK,-NBT)
      IBAY(NWD) = IREV(IOR(IAND(IREV(IBAY(NWD)),NOT(MSK)),INT))
      IF(NBT+NBIT.GT.NBITW) THEN

!        This character will not fit within the current word (i.e.
!        array member) of IBAY, because there are less than 8 bits of
!        space left.  Store as many bits as will fit within the current
!        word and then store the remaining bits within the next word.

         INT = ISHFT(IVAL(1),2*NBITW-(NBT+NBIT))
         MSK = ISHFT(  -1,2*NBITW-(NBT+NBIT))
         IBAY(NWD+1) = IREV(IOR(IAND(IREV(IBAY(NWD+1)),NOT(MSK)),INT))
      ENDIF
      IBIT = IBIT + NBIT
      ENDDO

!  EXITS
!  -----

      RETURN
      END SUBROUTINE PKC
