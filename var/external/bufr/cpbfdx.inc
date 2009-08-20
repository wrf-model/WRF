      SUBROUTINE CPBFDX(LUD,LUN)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    CPBFDX
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE COPIES BUFR TABLE (DICTIONARY) MESSAGES
!   FROM ONE LOCATION TO ANOTHER WITHIN INTERNAL MEMORY (ARRAYS IN
!   COMMON BLOCKS /MSGCWD/ AND /TABABD/).
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
!                           DOCUMENTATION (INCLUDING HISTORY)
!
! USAGE:    CALL CPBFDX (LUD, LUN)
!   INPUT ARGUMENT LIST:
!     LUD      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!                FOR INPUT TABLE LOCATION
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!                FOR OUTPUT TABLE LOCATION
!
! REMARKS:
!    THIS ROUTINE CALLS:        DXINIT
!    THIS ROUTINE IS CALLED BY: READDX
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES), &
                      INODE(NFILES),IDATE(NFILES)
      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES), &
                      MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2), &
                      IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES), &
                      TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES), &
                      TABD(MAXTBD,NFILES)

      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  INITIALIZE THE DICTIONARY TABLE PARTITION
!  -----------------------------------------

      CALL DXINIT(LUN,0)

!  COPY ONE TABLE PARTITION TO ANOTHER
!  -----------------------------------

!  .... Positional index for Table A mnem.
      INODE(LUN) = INODE(LUD)

!  .... Set the number of Table A entries
      NTBA(LUN) = NTBA(LUD)
!  .... Set the number of Table B entries
      NTBB(LUN) = NTBB(LUD)
!  .... Set the number of Table D entries
      NTBD(LUN) = NTBD(LUD)

!  .... Copy Table A entries
      DO I=1,NTBA(LUD)
!  .... Message type
      IDNA(I,LUN,1) = IDNA(I,LUD,1)
!  .... Message subtype
      IDNA(I,LUN,2) = IDNA(I,LUD,2)
!  .... Table A entries
      TABA(I,LUN) = TABA(I,LUD)
!  .... Pointer indices into internal tbl
      MTAB(I,LUN) = MTAB(I,LUD)
      ENDDO

!  .... Copy Table B entries
      DO I=1,NTBB(LUD)
!  .... Integer repr. of FXY descr.
      IDNB(I,LUN) = IDNB(I,LUD)
!  .... Table B entries
      TABB(I,LUN) = TABB(I,LUD)
      ENDDO

!  .... Copy Table D entries
      DO I=1,NTBD(LUD)
!  .... Integer repr. of FXY descr.
      IDND(I,LUN) = IDND(I,LUD)
!  .... Table B entries
      TABD(I,LUN) = TABD(I,LUD)
      ENDDO

      RETURN
      END SUBROUTINE CPBFDX
