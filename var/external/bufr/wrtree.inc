      SUBROUTINE WRTREE(LUN)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    WRTREE
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE CONVERTS USER NUMBERS INTO SCALED INTEGERS
!   AND PACKS THE USER ARRAY INTO THE SUBSET BUFFER.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1998-07-08  J. WOOLLEN -- CORRECTED SOME MINOR ERRORS
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           10,000 TO 20,000 BYTES
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
!                           INCREASED FROM 15000 TO 16000 (WAS IN
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
!                           WRF; ADDED DOCUMENTATION (INCLUDING
!                           HISTORY); REPL. "IVAL(N)=ANINT(PKS(NODE))"
!                           WITH "IVAL(N)=NINT(PKS(NODE))" (FORMER
!                           CAUSED PROBLEMS ON SOME FOREIGN MACHINES)
! 2004-03-10  J. WOOLLEN -- CONVERTED PACKING FUNCTION 'PKS' TO REAL*8 
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           20,000 TO 50,000 BYTES
! 2007-01-19  J. ATOR    -- PREVENT OVERFLOW OF CVAL FOR STRINGS LONGER
!                           THAN 8 CHARACTERS; USE FUNCTION IBFMS
!
! USAGE:    CALL WRTREE (LUN)
!   INPUT ARGUMENT LIST:
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!
! REMARKS:
!    THIS ROUTINE CALLS:        IBFMS    PKB      PKC
!    THIS ROUTINE IS CALLED BY: WRITSA   WRITSB
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGLD4),MBYT(NFILES), &
                      MBAY(MXMSGLD4,NFILES)
      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL), &
                      JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL), &
                      IBT(MAXJL),IRF(MAXJL),ISC(MAXJL), &
                      ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL), &
                      ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)

      CHARACTER*10 TAG
      CHARACTER*8  CVAL
      CHARACTER*3  TYP
      DIMENSION    IVAL(MAXJL)
      EQUIVALENCE  (CVAL,RVAL)
      REAL*8       VAL,RVAL,PKS,TEN

      DATA         TEN  /10./

!-----------------------------------------------------------------------
      PKS(NODE) = VAL(N,LUN)*TEN**ISC(NODE)-IRF(NODE)
!-----------------------------------------------------------------------

!  CONVERT USER NUMBERS INTO SCALED INTEGERS
!  -----------------------------------------

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

!  PACK THE USER ARRAY INTO THE SUBSET BUFFER
!  ------------------------------------------

      IBIT = 16

      DO N=1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).LT.3) THEN

!	 The value to be packed is numeric.

         CALL PKB(IVAL(N),IBT(NODE),IBAY,IBIT)
      ELSE

!	 The value to be packed is a character string.  If the string is
!	 longer than 8 characters, then only the first 8 will be packed
!	 by this routine, and a separate subsequent call to BUFR archive
!	 library subroutine WRITLC will be required to pack the
!	 remainder of the string.

         RVAL = VAL(N,LUN)
         NBT = MIN(8,IBT(NODE)/8)
         CALL PKC(CVAL,NBT,IBAY,IBIT)
      ENDIF
      ENDDO

      RETURN
      END SUBROUTINE WRTREE
