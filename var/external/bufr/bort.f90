      SUBROUTINE BORT(STR)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    BORT
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1998-07-08
!
! ABSTRACT: THIS SUBROUTINE PRINTS (TO STDOUT) A GIVEN ERROR STRING
!   AND THEN CALLS BUFR ARCHIVE LIBRARY SUBROUTINE BORT_EXIT TO ABORT
!   THE APPLICATION PROGRAM CALLING THE BUFR ARCHIVE LIBRARY SOFTWARE.
!   IT IS SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE BORT2, EXCEPT
!   BORT2 PRINTS TWO ERROR STRINGS. 
!
! PROGRAM HISTORY LOG:
! 1998-07-08  J. WOOLLEN -- ORIGINAL AUTHOR (REPLACED CRAY LIBRARY
!                           ROUTINE ABORT)
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION; REPLACED CALL TO
!                           INTRINSIC C ROUTINE "EXIT" WITH CALL TO
!                           BUFRLIB C ROUTINE "BORT_EXIT" WHICH ALWAYS
!                           RETURNS A NON-ZERO STATUS BACK TO EXECUTING
!                           SHELL SCRIPT
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION
!
! USAGE:    CALL BORT (STR)
!   INPUT ARGUMENT LIST:
!     STR      - CHARACTER*(*): ERROR MESSAGE TO BE PRINTED TO
!                STANDARD OUTPUT 
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT_EXIT
!    THIS ROUTINE IS CALLED BY: ADN30    CHEKSTAB CKTABA   CLOSMG
!                               CMPMSG   CMSGINI  CNVED4   COBFL
!                               COPYBF   COPYMG   COPYSB   CPYMEM
!                               CPYUPD   CRBMG    CWBMG    DATEBF
!                               DATELEN  DRFINI   DRSTPL   DUMPBF
!                               DXDUMP   DXMINI   GETWIN   GETTBH
!                               IDN30    IFBGET   INCTAB   INVMRG
!                               IPKM     IUPVS01  IUPVS1   IUPM
!                               JSTNUM   LSTJPB   LSTRPC   LSTRPS
!                               MAKESTAB MINIMG   MSGINI   MSGWRT
!                               MVB      NEMTBA   NEMTBAX  NEMTBB
!                               NEMTBD   NENUAA   NENUBD   NEVN
!                               NEWWIN   NMSUB    NVNWIN   NXTWIN
!                               OPENBF   OPENMB   OPENMG   OVRBS1
!                               PAD      PADMSG   PARUTG   PKBS1
!                               PKVS01   PKVS1    POSAPN   POSAPX
!                               RCSTPL   RDBFDX   RDMEMM   RDMEMS
!                               RDMGSB   RDMTBB   RDMTBD   READDX
!                               READERME READLC   READMG   READMM
!                               READNS   READSB   REWNBF   SNTBBE
!                               SNTBDE   STATUS   STDMSG   STNDRD
!                               TABENT   TABSUB   TRYBUMP  UFBCNT
!                               UFBCPY   UFBCUP   UFBDMP   UFBEVN
!                               UFBGET   UFBIN3   UFBINT   UFBINX
!                               UFBMEM   UFBMMS   UFBMNS   UFBOVR
!                               UFBPOS   UFBQCD   UFBQCP   UFBREP
!                               UFBRMS   UFBSEQ   UFBSTP   UFBTAB
!                               UFBTAM   UFDUMP   UPFTBV   UPTDD
!                               USRTPL   WRCMPS   WRDESC   WRDLEN
!                               WRITDX   WRITLC   WRITSA   WRITSB
!                               WTSTAT
!                               Normally not called by any application
!                               programs but it could be.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      CHARACTER*(*) STR
      PRINT*
      PRINT*,'**************BUFR ARCHIVE LIBRARY ABORT*****************'
      PRINT*,STR
      PRINT*,'**************BUFR ARCHIVE LIBRARY ABORT*****************'
      PRINT*
      CALL BORT_EXIT
      END SUBROUTINE BORT
