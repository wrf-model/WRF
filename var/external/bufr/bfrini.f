      SUBROUTINE BFRINI

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    BFRINI
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE IS CALLED ONLY ONE TIME (DURING THE FIRST
C   CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE OPENBF) IN ORDER TO
C   INITIALIZE SOME GLOBAL VARIABLES AND ARRAYS WITHIN SEVERAL COMMON
C   BLOCKS.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1998-07-08  J. WOOLLEN -- MODIFIED TO MAKE Y2K COMPLIANT
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); INITIALIZES
C                           VARIABLE JSR AS ZERO IN NEW COMMON BLOCK
C                           /BUFRSR/ (WAS IN VERIFICATION VERSION);
C                           UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION
C 2004-08-18  J. ATOR    -- ADDED INITIALIZATION OF COMMON /MSGSTD/;
C                           MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2005-11-29  J. ATOR    -- ADDED INITIALIZATION OF COMMON /MSGCMP/
C			    AND CALLS TO PKVS1 AND PKVS01
C 2009-03-23  J. ATOR    -- ADDED INITIALIZATION OF COMMON /DSCACH/,
C                           COMMON /MSTINF/ AND COMMON /TNKRCP/
C 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE
C                        -- ADDED INITIALIZATION OF COMMON BLOCKS
C                        -- /ENDORD/ AND /BUFRBMISS/
C
C USAGE:    CALL BFRINI
C
C REMARKS:
C    THIS ROUTINE CALLS:        IFXY     IPKM     PKVS01
C    THIS ROUTINE IS CALLED BY: OPENBF
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
      COMMON /MAXCMP/ MAXCMB,MAXROW,MAXCOL,NCMSGS,NCSUBS,NCBYTS
      COMMON /PADESC/ IBCT,IPD1,IPD2,IPD3,IPD4
      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)
      COMMON /STBFR / IOLUN(NFILES),IOMSG(NFILES)
      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES),
     .                MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2),
     .                IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES),
     .                TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES),
     .                TABD(MAXTBD,NFILES)
      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)
      COMMON /BTABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /DSCACH/ NCNEM,CNEM(MXCNEM),NDC(MXCNEM),
     .                IDCACH(MXCNEM,MAXNC)
      COMMON /BUFRMG/ MSGLEN,MSGTXT(MXMSGLD4)
      COMMON /MRGCOM/ NRPL,NMRG,NAMB,NTOT
      COMMON /DATELN/ LENDAT
      COMMON /ACMODE/ IAC
      COMMON /BUFRSR/ JUNN,JILL,JIMM,JBIT,JBYT,JMSG,JSUB,KSUB,JNOD,JDAT,
     .                JSR(NFILES),JBAY(MXMSGLD4)
      COMMON /MSGSTD/ CSMF
      COMMON /MSGCMP/ CCMF
      COMMON /TNKRCP/ ITRYR,ITRMO,ITRDY,ITRHR,ITRMI,CTRT
      COMMON /MSTINF/ LUN1,LUN2,LMTD,MTDIR
      COMMON /ENDORD/ IBLOCK,IORDBE(4),IORDLE(4)


      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*100 MTDIR
      CHARACTER*56  DXSTR
      CHARACTER*10  TAG
      CHARACTER*8   CNEM
      CHARACTER*6   ADSN(5,2),DNDX(25,10)
      CHARACTER*3   TYPX(5,2),TYPS,TYP
      CHARACTER*1   REPX(5,2),REPS
      CHARACTER*1   CSMF
      CHARACTER*1   CCMF
      CHARACTER*1   CTRT
      DIMENSION     NDNDX(10),NLDXA(10),NLDXB(10),NLDXD(10),NLD30(10)
      DIMENSION     LENX(5)

      DATA ADSN   / '101000','360001','360002','360003','360004' ,
     .              '101255','031002','031001','031001','031000' /
      DATA TYPX   /    'REP',   'DRP',   'DRP',   'DRS' ,  'DRB' ,
     .                 'SEQ',   'RPC',   'RPC',   'RPS' ,  'SEQ' /
      DATA REPX   /      '"',     '(',     '{',     '[' ,    '<' ,
     .                   '"',     ')',     '}',     ']' ,    '>' /
      DATA LENX   /       0 ,     16 ,      8 ,      8  ,     1  /

      DATA (DNDX(I,1),I=1,25)/
     .'102000','031001','000001','000002',
     .'110000','031001','000010','000011','000012','000013','000015',
     .                  '000016','000017','000018','000019','000020',
     .'107000','031001','000010','000011','000012','000013','101000',
     .                  '031001','000030'/

      DATA (DNDX(I,2),I=1,15)/
     .'103000','031001','000001','000002','000003',
     .'101000','031001','300004',
     .'105000','031001','300003','205064','101000','031001','000030'/

      DATA NDNDX /  25 ,  15 , 8*0 /
      DATA NLDXA /  35 ,  67 , 8*0 /
      DATA NLDXB /  80 , 112 , 8*0 /
      DATA NLDXD /  38 ,  70 , 8*0 /
      DATA NLD30 /   5 ,   6 , 8*0 /

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  INITIALIZE /ENDORD/ TO CONTROL OUTPUT BLOCKING -1=LE 0=NONE +1=BE
C  -----------------------------------------------------------------

      IBLOCK = 0

C  INITIALIZE /BUFRBMISS/
C  ----------------------

      BMISS = 10E10

C  INITIALIZE /BITBUF/
C  -------------------

      MAXBYT = 10000

C  INITIALIZE /MAXCMP/
C  -------------------

      MAXCMB = MAXBYT
      MAXROW = 0
      MAXCOL = 0
      NCMSGS = 0
      NCSUBS = 0
      NCBYTS = 0

C  INITIALIZE /PADESC/
C  -------------------

      IBCT = IFXY('063000')
      IPD1 = IFXY('102000')
      IPD2 = IFXY('031001')
      IPD3 = IFXY('206001')
      IPD4 = IFXY('063255')

C  INITIALIZE /STBFR/
C  ------------------

      DO I=1,NFILES
      IOLUN(I) = 0
      IOMSG(I) = 0
      ENDDO

C  INITIALIZE /REPTAB/
C  -------------------

      DO I=1,5
      LENS(I) = LENX(I)
      DO J=1,2
      IDNR(I,J) = IFXY(ADSN(I,J))
      TYPS(I,J) = TYPX(I,J)
      REPS(I,J) = REPX(I,J)
      ENDDO
      ENDDO

C  INITIALIZE /TABABD/ (INTERNAL ARRAYS HOLDING DICTIONARY TABLE)
C  --------------------------------------------------------------

C    NTBA(0) is the maximum number of entries w/i internal BUFR table A

      NTBA(0) = MAXTBA

C    NTBB(0) is the maximum number of entries w/i internal BUFR Table B

      NTBB(0) = MAXTBB

C    NTBD(0) is the maximum number of entries w/i internal BUFR Table D

      NTBD(0) = MAXTBD

C  INITIALIZE /DXTAB/
C  ------------------

      MAXDX = MAXBYT
c  .... IDXV is the version number of the local tables
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

C  INITIALIZE /BTABLES/
C  -------------------

      MAXTAB = MAXJL

C  INITIALIZE /BUFRMG/
C  -------------------

      MSGLEN = 0

C  INITIALIZE /MRGCOM/
C  -------------------

      NRPL = 0
      NMRG = 0
      NAMB = 0
      NTOT = 0

C  INITIALIZE /DATELN/
C  -------------------

      IF(LENDAT.NE.10) LENDAT = 8

C  INITIALIZE /ACMODE/
C  ------------------_

c  .... DK: What does this control??
      IAC = 0

C  INITIALIZE /BUFRSR/
C  -------------------

      DO I=1,NFILES
      JSR(I) = 0
      ENDDO

C  INITIALIZE /DSCACH/
C  -------------------

      NCNEM = 0

C  INITIALIZE /MSGSTD/
C  -------------------

      CSMF = 'N'

C  INITIALIZE /MSGCMP/
C  -------------------

      CCMF = 'N'

C  INITIALIZE /TNKRCP/
C  -------------------

      CTRT = 'N'

C  INITIALIZE /MSTINF/
C  -------------------

      MTDIR = '/nwprod/fix'
      LMTD = 11

      LUN1 = 98
      LUN2 = 99

C  INITIALIZE /S01CM/
C  -------------------

      CALL PKVS01('INIT',-99)

      RETURN
      END
