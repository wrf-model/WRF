      SUBROUTINE BFRINI

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    BFRINI
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE IS CALLED ONLY ONE TIME (DURING THE FIRST
!   CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF) IN ORDER TO
!   INITIALIZE SOME GLOBAL VARIABLES AND ARRAYS WITHIN SEVERAL COMMON
!   BLOCKS.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
! 1998-07-08  J. WOOLLEN -- MODIFIED TO MAKE Y2K COMPLIANT
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           10,000 TO 20,000 BYTES
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
!                           INCREASED FROM 15000 TO 16000 (WAS IN
!                           VERIFICATION VERSION); INITIALIZES
!                           VARIABLE JSR AS ZERO IN NEW COMMON BLOCK
!                           /BUFRSR/ (WAS IN VERIFICATION VERSION);
!                           UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION
! 2004-08-18  J. ATOR    -- ADDED INITIALIZATION OF COMMON /MSGSTD/;
!                           MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           20,000 TO 50,000 BYTES
! 2005-11-29  J. ATOR    -- ADDED INITIALIZATION OF COMMON /MSGCMP/
!			    AND CALLS TO PKVS1 AND PKVS01
!
! USAGE:    CALL BFRINI
!
! REMARKS:
!    THIS ROUTINE CALLS:        IFXY     IPKM     PKVS01   PKVS1
!    THIS ROUTINE IS CALLED BY: OPENBF
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
      COMMON /MAXCMP/ MAXCMB,MAXROW,MAXCOL,NCMSGS,NCSUBS,NCBYTS
      COMMON /PADESC/ IBCT,IPD1,IPD2,IPD3,IPD4
      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)
      COMMON /STBFR / IOLUN(NFILES),IOMSG(NFILES)
      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES), &
                      MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2), &
                      IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES), &
                      TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES), &
                      TABD(MAXTBD,NFILES)
      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10), &
                      LD30(10),DXSTR(10)
!     COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL), &
!                     JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL), &
!                     IBT(MAXJL),IRF(MAXJL),ISC(MAXJL), &
!                     ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL), &
!                     ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /BUFRMG/ MSGLEN,MSGTXT(MXMSGLD4)
      COMMON /MRGCOM/ NRPL,NMRG,NAMB,NTOT
      COMMON /DATELN/ LENDAT
      COMMON /ACMODE/ IAC
      COMMON /BUFRSR/ JUNN,JILL,JIMM,JBIT,JBYT,JMSG,JSUB,KSUB,JNOD,JDAT, &
                      JSR(NFILES),JBAY(MXMSGLD4)
      COMMON /MSGSTD/ CSMF
      COMMON /MSGCMP/ CCMF


      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*56  DXSTR
!     CHARACTER*10  TAG
      CHARACTER*6   ADSN(5,2),DNDX(25,10)
      CHARACTER*3   TYPX(5,2),TYPS !,TYP
      CHARACTER*1   REPX(5,2),REPS
      CHARACTER*1   CSMF
      CHARACTER*1   CCMF
      DIMENSION     NDNDX(10),NLDXA(10),NLDXB(10),NLDXD(10),NLD30(10)
      DIMENSION     LENX(5)

      DATA ADSN   / '101000','360001','360002','360003','360004' , &
                    '101255','031002','031001','031001','031000' /
      DATA TYPX   /    'REP',   'DRP',   'DRP',   'DRS' ,  'DRB' , &
                       'SEQ',   'RPC',   'RPC',   'RPS' ,  'SEQ' /
      DATA REPX   /      '"',     '(',     '{',     '[' ,    '<' , &
                         '"',     ')',     '}',     ']' ,    '>' /
      DATA LENX   /       0 ,     16 ,      8 ,      8  ,     1  /

      DATA (DNDX(I,1),I=1,25)/ &
      '102000','031001','000001','000002', &
      '110000','031001','000010','000011','000012','000013','000015', &
                        '000016','000017','000018','000019','000020', &
      '107000','031001','000010','000011','000012','000013','101000', &
                        '031001','000030'/

      DATA (DNDX(I,2),I=1,15)/ &
      '103000','031001','000001','000002','000003', &
      '101000','031001','300004', &
      '105000','031001','300003','205064','101000','031001','000030'/

      DATA NDNDX /  25 ,  15 , 8*0 /
      DATA NLDXA /  35 ,  67 , 8*0 /
      DATA NLDXB /  80 , 112 , 8*0 /
      DATA NLDXD /  38 ,  70 , 8*0 /
      DATA NLD30 /   5 ,   6 , 8*0 /

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  INITIALIZE /BITBUF/
!  -------------------

      MAXBYT = 10000

!  INITIALIZE /MAXCMP/
!  -------------------

      MAXCMB = MAXBYT
      MAXROW = 0
      MAXCOL = 0
      NCMSGS = 0
      NCSUBS = 0
      NCBYTS = 0

!  INITIALIZE /PADESC/
!  -------------------

      IBCT = IFXY('063000')
      IPD1 = IFXY('102000')
      IPD2 = IFXY('031001')
      IPD3 = IFXY('206001')
      IPD4 = IFXY('063255')

!  INITIALIZE /STBFR/
!  ------------------

      DO I=1,NFILES
      IOLUN(I) = 0
      IOMSG(I) = 0
      ENDDO

!  INITIALIZE /REPTAB/
!  -------------------

      DO I=1,5
      LENS(I) = LENX(I)
      DO J=1,2
      IDNR(I,J) = IFXY(ADSN(I,J))
      TYPS(I,J) = TYPX(I,J)
      REPS(I,J) = REPX(I,J)
      ENDDO
      ENDDO

!  INITIALIZE /TABABD/ (INTERNAL ARRAYS HOLDING DICTIONARY TABLE)
!  --------------------------------------------------------------

!    NTBA(0) is the maximum number of entries w/i internal BUFR table A

      NTBA(0) = MAXTBA

!    NTBB(0) is the maximum number of entries w/i internal BUFR Table B

      NTBB(0) = MAXTBB

!    NTBD(0) is the maximum number of entries w/i internal BUFR Table D

      NTBD(0) = MAXTBD

!  INITIALIZE /DXTAB/
!  ------------------

      MAXDX = MAXBYT
!  .... IDXV is the version number of the local tables
      IDXV  = 1

      DO J=1,10
      LDXA(J)  = NLDXA(J)
      LDXB(J)  = NLDXB(J)
      LDXD(J)  = NLDXD(J)
      LD30(J)  = NLD30(J)
      DXSTR(J) = '      '
      NXSTR(J) = NDNDX(J)*2
      DO I=1,NDNDX(J)
      I1 = I*2-1
      CALL IPKM(DXSTR(J)(I1:I1),2,IFXY(DNDX(I,J)))
      ENDDO
      ENDDO

!  INITIALIZE /TABLES/
!  -------------------

      MAXTAB = MAXJL

!  INITIALIZE /BUFRMG/
!  -------------------

      MSGLEN = 0

!  INITIALIZE /MRGCOM/
!  -------------------

      NRPL = 0
      NMRG = 0
      NAMB = 0
      NTOT = 0

!  INITIALIZE /DATELN/
!  -------------------

      IF(LENDAT.NE.10) LENDAT = 8

!  INITIALIZE /ACMODE/
!  ------------------_

!  .... DK: What does this control??
      IAC = 0

!  INITIALIZE /BUFRSR/
!  -------------------

      DO I=1,NFILES
      JSR(I) = 0
      ENDDO

!  INITIALIZE /MSGSTD/
!  -------------------

      CSMF = 'N'

!  INITIALIZE /MSGCMP/
!  -------------------

      CCMF = 'N'

!  INITIALIZE /SECT01/
!  -------------------

      CALL PKVS1(-99,-99)

!  INITIALIZE /S01CM/
!  -------------------

      CALL PKVS01('INIT',-99)

      RETURN
      END SUBROUTINE BFRINI
