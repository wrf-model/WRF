      SUBROUTINE BORT(STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    BORT
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1998-07-08
C
C ABSTRACT: THIS SUBROUTINE PRINTS (TO STDOUT) A GIVEN ERROR STRING
C   AND THEN CALLS BUFR ARCHIVE LIBRARY SUBROUTINE BORT_EXIT TO ABORT
C   THE APPLICATION PROGRAM CALLING THE BUFR ARCHIVE LIBRARY SOFTWARE.
C   IT IS SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE BORT2, EXCEPT
C   BORT2 PRINTS TWO ERROR STRINGS. 
C
C PROGRAM HISTORY LOG:
C 1998-07-08  J. WOOLLEN -- ORIGINAL AUTHOR (REPLACED CRAY LIBRARY
C                           ROUTINE ABORT)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION; REPLACED CALL TO
C                           INTRINSIC C ROUTINE "EXIT" WITH CALL TO
C                           BUFRLIB C ROUTINE "BORT_EXIT" WHICH ALWAYS
C                           RETURNS A NON-ZERO STATUS BACK TO EXECUTING
C                           SHELL SCRIPT
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION
C
C USAGE:    CALL BORT (STR)
C   INPUT ARGUMENT LIST:
C     STR      - CHARACTER*(*): ERROR MESSAGE TO BE PRINTED TO
C                STANDARD OUTPUT 
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT_EXIT
C    THIS ROUTINE IS CALLED BY: ADN30    CHEKSTAB CKTABA   CLOSMG
C                               CMPMSG   CMSGINI  CNVED4   COBFL
C                               COPYBF   COPYMG   COPYSB   CPYMEM
C                               CPYUPD   CRBMG    CWBMG    DATEBF
C                               DATELEN  DRFINI   DRSTPL   DUMPBF
C                               DXDUMP   DXMINI   GETWIN   GETTBH
C                               IDN30    IFBGET   INCTAB   INVMRG
C                               IPKM     IUPVS01  IUPVS1   IUPM
C                               JSTNUM   LSTJPB   LSTRPC   LSTRPS
C                               MAKESTAB MINIMG   MSGINI   MSGWRT
C                               MVB      NEMTBA   NEMTBAX  NEMTBB
C                               NEMTBD   NENUAA   NENUBD   NEVN
C                               NEWWIN   NMSUB    NVNWIN   NXTWIN
C                               OPENBF   OPENMB   OPENMG   OVRBS1
C                               PAD      PADMSG   PARUTG   PKBS1
C                               PKVS01   PKVS1    POSAPN   POSAPX
C                               RCSTPL   RDBFDX   RDMEMM   RDMEMS
C                               RDMGSB   RDMTBB   RDMTBD   READDX
C                               READERME READLC   READMG   READMM
C                               READNS   READSB   REWNBF   SNTBBE
C                               SNTBDE   STATUS   STDMSG   STNDRD
C                               TABENT   TABSUB   TRYBUMP  UFBCNT
C                               UFBCPY   UFBCUP   UFBDMP   UFBEVN
C                               UFBGET   UFBIN3   UFBINT   UFBINX
C                               UFBMEM   UFBMMS   UFBMNS   UFBOVR
C                               UFBPOS   UFBQCD   UFBQCP   UFBREP
C                               UFBRMS   UFBSEQ   UFBSTP   UFBTAB
C                               UFBTAM   UFDUMP   UPFTBV   UPTDD
C                               USRTPL   WRCMPS   WRDESC   WRDLEN
C                               WRITDX   WRITLC   WRITSA   WRITSB
C                               WTSTAT
C                               Normally not called by any application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*(*) STR
      PRINT*
      PRINT*,'**************BUFR ARCHIVE LIBRARY ABORT*****************'
      PRINT*,STR
      PRINT*,'**************BUFR ARCHIVE LIBRARY ABORT*****************'
      PRINT*
      CALL BORT_EXIT
      END
