      SUBROUTINE CPBFDX(LUD,LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    CPBFDX
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE COPIES BUFR TABLE (DICTIONARY) MESSAGES
C   FROM ONE LOCATION TO ANOTHER WITHIN INTERNAL MEMORY (ARRAYS IN
C   COMMON BLOCKS /MSGCWD/ AND /TABABD/).
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
C USAGE:    CALL CPBFDX (LUD, LUN)
C   INPUT ARGUMENT LIST:
C     LUD      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C                FOR INPUT TABLE LOCATION
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C                FOR OUTPUT TABLE LOCATION
C
C REMARKS:
C    THIS ROUTINE CALLS:        DXINIT
C    THIS ROUTINE IS CALLED BY: READDX
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

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

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  INITIALIZE THE DICTIONARY TABLE PARTITION
C  -----------------------------------------

      CALL DXINIT(LUN,0)

C  COPY ONE TABLE PARTITION TO ANOTHER
C  -----------------------------------

c  .... Positional index for Table A mnem.
      INODE(LUN) = INODE(LUD)

c  .... Set the number of Table A entries
      NTBA(LUN) = NTBA(LUD)
c  .... Set the number of Table B entries
      NTBB(LUN) = NTBB(LUD)
c  .... Set the number of Table D entries
      NTBD(LUN) = NTBD(LUD)

c  .... Copy Table A entries
      DO I=1,NTBA(LUD)
c  .... Message type
      IDNA(I,LUN,1) = IDNA(I,LUD,1)
c  .... Message subtype
      IDNA(I,LUN,2) = IDNA(I,LUD,2)
c  .... Table A entries
      TABA(I,LUN) = TABA(I,LUD)
c  .... Pointer indices into internal tbl
      MTAB(I,LUN) = MTAB(I,LUD)
      ENDDO

c  .... Copy Table B entries
      DO I=1,NTBB(LUD)
c  .... Integer repr. of FXY descr.
      IDNB(I,LUN) = IDNB(I,LUD)
c  .... Table B entries
      TABB(I,LUN) = TABB(I,LUD)
      ENDDO

c  .... Copy Table D entries
      DO I=1,NTBD(LUD)
c  .... Integer repr. of FXY descr.
      IDND(I,LUN) = IDND(I,LUD)
c  .... Table B entries
      TABD(I,LUN) = TABD(I,LUD)
      ENDDO

      RETURN
      END
