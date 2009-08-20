      SUBROUTINE PKB(NVAL,NBITS,IBAY,IBIT)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    PKB
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE PACKS AN INTEGER VALUE (NVAL) INTO NBITS
!   BITS OF AN INTEGER ARRAY (IBAY), STARTING WITH BIT (IBIT+1).  ON
!   OUTPUT, IBIT IS UPDATED TO POINT TO THE LAST BIT THAT WAS PACKED.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  J. WOOLLEN -- BIG-ENDIAN/LITTLE-ENDIAN INDEPENDENT (WAS
!                           IN DECODER VERSION)
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION
!
! USAGE:    CALL PKB (NVAL, NBITS, IBAY, IBIT)
!   INPUT ARGUMENT LIST:
!     NVAL     - INTEGER: INTEGER TO BE PACKED
!     NBITS    - INTEGER: NUMBER OF BITS OF IBAY WITHIN WHICH TO PACK
!                NVAL
!     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY NOT YET CONTAINING
!                PACKED NVAL
!     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING BIT AFTER
!                WHICH TO START PACKING
!
!   OUTPUT ARGUMENT LIST:
!     IBAY     - INTEGER: *-WORD PACKED BINARY ARRAY NOW CONTAINING
!                PACKED NVAL
!     IBIT     - INTEGER: BIT POINTER WITHIN IBAY INDICATING LAST BIT
!                THAT WAS PACKED
!
! REMARKS:
!    THIS SUBROUTINE IS THE INVERSE OF BUFR ARCHIVE LIBRARY ROUTINE
!    UPB.
!
!    THIS ROUTINE CALLS:        IREV
!    THIS ROUTINE IS CALLED BY: CMSGINI  CNVED4   CPYUPD   DXMINI
!                               MSGINI   MSGUPD   MSGWRT   MVB
!                               OVRBS1   PAD      PADMSG   PKBS1
!                               STNDRD   SUBUPD   WRCMPS   WRITDX
!                               WRTREE
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      COMMON /HRDWRD/ NBYTW,NBITW,NREV,IORD(8)

      DIMENSION IBAY(*)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      NWD  = IBIT/NBITW + 1
      NBT  = MOD(IBIT,NBITW)
      IVAL = NVAL
      IF(ISHFT(IVAL,-NBITS).GT.0) IVAL = -1
      INT = ISHFT(IVAL,NBITW-NBITS)
      INT = ISHFT(INT,-NBT)
      MSK = ISHFT(  -1,NBITW-NBITS)
      MSK = ISHFT(MSK,-NBT)
      IBAY(NWD) = IREV(IOR(IAND(IREV(IBAY(NWD)),NOT(MSK)),INT))
      IF(NBT+NBITS.GT.NBITW) THEN

!        There are less than NBITS bits remaining within the current
!        word (i.e. array member) of IBAY, so store as many bits as
!        will fit within the current word and then store the remaining
!        bits within the next word.

         INT = ISHFT(IVAL,2*NBITW-(NBT+NBITS))
         MSK = ISHFT(  -1,2*NBITW-(NBT+NBITS))
         IBAY(NWD+1) = IREV(IOR(IAND(IREV(IBAY(NWD+1)),NOT(MSK)),INT))
      ENDIF

      IBIT = IBIT + NBITS

      RETURN
      END SUBROUTINE PKB
