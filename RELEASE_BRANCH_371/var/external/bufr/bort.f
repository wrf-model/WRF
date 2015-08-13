      SUBROUTINE BORT(STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    BORT
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1998-07-08
C
C ABSTRACT: THIS SUBROUTINE WRITES (VIA BUFR ARCHIVE LIBRARY SUBROUTINE
C   ERRWRT) A GIVEN ERROR STRING AND THEN CALLS BUFR ARCHIVE LIBRARY
C   SUBROUTINE BORT_EXIT TO ABORT THE APPLICATION PROGRAM CALLING THE
C   BUFR ARCHIVE LIBRARY SOFTWARE. IT IS SIMILAR TO BUFR ARCHIVE LIBRARY
C   SUBROUTINE BORT2, EXCEPT BORT2 WRITES TWO ERROR STRINGS. 
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
C 2009-04-21  J. ATOR    -- USE ERRWRT
C
C USAGE:    CALL BORT (STR)
C   INPUT ARGUMENT LIST:
C     STR      - CHARACTER*(*): ERROR MESSAGE TO BE WRITTEN VIA
C                SUBROUTINE ERRWRT 
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT_EXIT ERRWRT
C    THIS ROUTINE IS CALLED BY: ADN30    ATRCPT   BVERS    CHEKSTAB
C                               CKTABA   CLOSMG   CMPMSG   CMSGINI
C                               CNVED4   COBFL    COPYBF   COPYMG
C                               COPYSB   CPDXMM   CPYMEM   CPYUPD
C                               CRBMG    CWBMG    DATEBF   DATELEN
C                               DRFINI   DRSTPL   DUMPBF   DXDUMP
C                               DXMINI   GETWIN   GETTBH   IDN30
C                               IFBGET   IGETNTBI IGETSC   IGETTDI
C                               INCTAB   INVMRG   IPKM     ISIZE
C                               IUPVS01  IUPM     JSTNUM   LCMGDF
C                               LSTJPB   MAKESTAB MINIMG   MSGINI
C                               MSGWRT   MVB      NEMTBA   NEMTBAX
C                               NEMTBB   NEMTBD   NENUBD   NEVN
C                               NEWWIN   NMSUB    NUMMTB   NVNWIN
C                               NXTWIN   OPENBF   OPENMB   OPENMG
C                               PAD      PADMSG   PARUTG   PKBS1
C                               PKVS01   POSAPX   RCSTPL   RDBFDX
C                               RDCMPS   RDMEMM   RDMEMS   RDMGSB
C                               RDMSGB   RDMSGW   RDMTBB   RDMTBD
C                               READDX   READERME READLC   READMG
C                               READNS   READSB   READS3   REWNBF
C                               RTRCPT   SNTBBE   SNTBDE   STATUS
C                               STBFDX   STDMSG   STNDRD   STNTBIA
C                               STRCPT   STSEQ    TABENT   TABSUB
C                               TRYBUMP  UFBCNT   UFBCPY   UFBCUP
C                               UFBDMP   UFBEVN   UFBGET   UFBIN3
C                               UFBINT   UFBINX   UFBMEM   UFBMEX
C                               UFBMMS   UFBMNS   UFBOVR   UFBPOS
C                               UFBQCD   UFBQCP   UFBREP   UFBRMS
C                               UFBSEQ   UFBSTP   UFBTAB   UFBTAM
C                               UFDUMP   UPDS3    UPFTBV   UPTDD
C                               USRTPL   WRCMPS   WRDESC   WRDLEN
C                               WRDXTB   WRITDX   WRITLC   WRITSA
C                               WRITSB   WTSTAT
C                               Normally not called by any application
C                               programs but it could be.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*(*) STR

      CALL ERRWRT(' ')
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(STR)
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(' ')

      CALL BORT_EXIT

      END
