      SUBROUTINE DXINIT(LUN,IOI)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DXINIT
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE INITIALIZES THE MESSAGE CONTROL WORD
C   PARTITION ARRAYS (COMMON BLOCK /MSGCWD/) AND THE INTERNAL ARRAYS
C   (COMMON BLOCK /TABABD/) HOLDING THE DICTIONARY TABLE.  IT THEN
C   INITIALIZES THE TABLE WITH APRIORI TABLE B AND D ENTRIES
C   (OPTIONAL).
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION (INCLUDING HISTORY)
C
C USAGE:    CALL DXINIT (LUN, IOI)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     IOI      - INTEGER: SWITCH:
C                       0 = do not initialize the table with apriori
C                           Table B and D entries
C                    else = initialize the table with apriori Table B
C                           and D entries
C
C REMARKS:
C    THIS ROUTINE CALLS:        ADN30    IFXY     PKTDD
C    THIS ROUTINE IS CALLED BY: CPBFDX   OPENBF   RDBFDX   RDUSDX
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /PADESC/ IBCT,IPD1,IPD2,IPD3,IPD4
      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)
      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES),
     .                INODE(NFILES),IDATE(NFILES)
      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES),
     .                MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2),
     .                IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES),
     .                TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES),
     .                TABD(MAXTBD,NFILES)

      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*8   INIB(6,5),INID(5)
      CHARACTER*6   ADN30
      CHARACTER*3   TYPS
      CHARACTER*1   REPS

      DATA INIB   /'------','BYTCNT  ','BYTES  ','+0','+0','16',
     .             '------','BITPAD  ','NONE   ','+0','+0','1 ',
     .             '031000','DRF1BIT ','NUMERIC','+0','+0','1 ',
     .             '031001','DRF8BIT ','NUMERIC','+0','+0','8 ',
     .             '031002','DRF16BIT','NUMERIC','+0','+0','16'/
      DATA NINIB  /5/

      DATA INID   /'        ',
     .             'DRP16BIT',
     .             'DRP8BIT ',
     .             'DRPSTAK ',
     .             'DRP1BIT '/
      DATA NINID  /5/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CLEAR OUT A MESSAGE CONTROL WORD PARTITION ARRAYS
C  -------------------------------------------------

      NMSG(LUN)  = 0
      NSUB(LUN)  = 0
      MSUB(LUN)  = 0
      INODE(LUN) = 0
      IDATE(LUN) = 0

C  CLEAR OUT A TABLE PARTITION
C  ---------------------------

      NTBA(LUN) = 0
      DO I=1,NTBA(0)
      TABA(I,LUN) = ' '
      MTAB(I,LUN) = 0
      ENDDO

      NTBB(LUN) = 0
      DO I=1,NTBB(0)
      TABB(I,LUN) = ' '
      ENDDO

      NTBD(LUN) = 0
      DO I=1,NTBD(0)
      TABD(I,LUN) = ' '
c  .... This zeroes the counter in TABD array, IRET returns as 0 and
c       is not tested
      CALL PKTDD(I,LUN,0,IRET)
      ENDDO

      IF(IOI.EQ.0) GOTO 100

C  INITIALIZE TABLE WITH APRIORI TABLE B AND D ENTRIES
C  ---------------------------------------------------

      INIB(1,1) = ADN30(IBCT,6)
      INIB(1,2) = ADN30(IPD4,6)

      DO I=1,NINIB
      NTBB(LUN) = NTBB(LUN)+1
      IDNB(I,LUN) = IFXY(INIB(1,I))
      TABB(I,LUN)(  1:  6) = INIB(1,I)
      TABB(I,LUN)(  7: 70) = INIB(2,I)
      TABB(I,LUN)( 71: 94) = INIB(3,I)
      TABB(I,LUN)( 95: 98) = INIB(4,I)
      TABB(I,LUN)( 99:109) = INIB(5,I)
      TABB(I,LUN)(110:112) = INIB(6,I)
      ENDDO

      DO I=2,NINID
      N = NTBD(LUN)+1
      IDND(N,LUN) = IDNR(I,1)
      TABD(N,LUN)(1: 6) = ADN30(IDNR(I,1),6)
      TABD(N,LUN)(7:70) = INID(I)
c  .... DK: what if IRET = -1 ???
      CALL PKTDD(N,LUN,IDNR(1,1),IRET)
c  .... DK: what if IRET = -1 ???
      CALL PKTDD(N,LUN,IDNR(I,2),IRET)
      NTBD(LUN) = N
      ENDDO

C  EXIT
C  ----

100   RETURN
      END
