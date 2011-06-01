      SUBROUTINE NENUBD(NEMO,NUMB,LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NENUBD
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE CHECKS A MNEMONIC AND FXY VALUE PAIR THAT
C   WERE READ FROM A USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER
C   FORMAT, IN ORDER TO MAKE SURE THAT NEITHER VALUE HAS ALREADY BEEN
C   DEFINED WITHIN INTERNAL BUFR TABLE B OR D (IN COMMON BLOCK
C   /TABABD/) FOR THE GIVEN LUN.  IF EITHER VALUE HAS ALREADY BEEN
C   DEFINED FOR THIS LUN, THEN AN APPROPRIATE CALL IS MADE TO
C   BUFR ARCHIVE LIBRARY SUBROUTINE BORT.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN NENUCK)
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT" (IN PARENT ROUTINE NENUCK)
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI) (IN PARENT
C                           ROUTINE NENUCK)
C 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE
C                           PORTABILITY TO OTHER PLATFORMS (NENUCK WAS
C                           THEN REMOVED BECAUSE IT WAS JUST A DUMMY
C                           ROUTINE WITH ENTRIES)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C
C USAGE:    CALL NENUBD (NEMO, NUMB, LUN)
C   INPUT ARGUMENT LIST:
C     NEMO     - CHARACTER*8: MNEMONIC
C     NUMB     - CHARACTER*6: FXY VALUE ASSOCIATED WITH NEMO
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C REMARKS:
C    EXAMPLE SHOWING LAYOUT OF INTERNAL BUFR TABLE B (FROM A DBX DEBUG
C    SESSION USING "bufrtab.002", AND WHERE LUN = 1)
C
C   (dbx) print NTBB[1]
C   95
C
C   (dbx) print TABB[1,1]
C   0x1003c164 = "063000BYTCNT                                  ",
C                "                        BYTES                 ",
C                "  +0  +0         16                 "
C
C   (dbx) print TABB[2,1]
C   0x1003c1e4 = "063255BITPAD                                  ",
C                "                        NONE                  ",
C                "  +0  +0         1                  "
C
C   (dbx) print TABB[3,1]
C   0x1003c264 = "031000DRF1BIT                                 ",
C                "                        NUMERIC               ",
C                "  +0  +0         1                  "
C
C   (dbx) print TABB[8,1]
C   0x1003c4e4 = "001003WMOR     WMO REGION NUMBER              ",
C                "                        CODE TABLE            ",
C                "  +0  +0         3                  "
C
C   (dbx) print TABB[11,1]
C   0x1003c664 = "001194BUHD     BULLETIN HEADER                ",
C                "                        CCITT IA5             ",
C                "  +0  +0         64                 "
C
C   (dbx) print TABB[21,1]
C   0x1003cb64 = "004003DAYS     DAY                            ",
C                "                        DAY                   ",
C                "  +0  +0         6                  "
C
C   (dbx) print TABB[33,1]
C   0x1003d164 = "005002CLAT     LATITUDE (COARSE ACCURACY)     ",
C                "                        DEGREES               ",
C                "  +2  -0000"     15                 "
C
C   and so on, up through TABB[95,1] ( = TABB[NTBB[LUN],LUN] )
C
C
C
C    EXAMPLE SHOWING LAYOUT OF INTERNAL BUFR TABLE D (FROM A DBX DEBUG
C    SESSION USING "bufrtab.002", AND WHERE LUN = 1)
C
C   (dbx) print NTBD[1]
C   43
C
C   (dbx) &TABD[1,1]/14c
C   1008a364:  '3' '6' '0' '0' '0' '1' 'D' 'R' 'P' '1' '6' 'B' 'I' 'T'
C
C   (dbx) &TABD[2,1]/14c
C   1008a5bc:  '3' '6' '0' '0' '0' '2' 'D' 'R' 'P' '8' 'B' 'I' 'T' ' '
C
C   (dbx) &TABD[3,1]/14c
C   1008a814:  '3' '6' '0' '0' '0' '3' 'D' 'R' 'P' 'S' 'T' 'A' 'K' ' '
C
C   (dbx) &TABD[4,1]/14c
C   1008aa6c:  '3' '6' '0' '0' '0' '4' 'D' 'R' 'P' '1' 'B' 'I' 'T' ' '
C
C   (dbx) &TABD[5,1]/14c
C   1008acc4:  '3' '6' '3' '2' '1' '8' 'N' 'C' '0' '0' '2' '0' '0' '1'
C
C   (dbx) &TABD[6,1]/14c
C   1008af1c:  '3' '6' '3' '2' '1' '9' 'N' 'C' '0' '0' '2' '0' '0' '2'
C
C   (dbx) &TABD[24,1]/14c
C   1008d94c:  '3' '6' '1' '1' '3' '0' 'U' 'A' 'A' 'D' 'F' ' ' ' ' ' '
C
C   and so on, up through TABD[43,1] ( = TABD[NTBD[LUN],LUN] )
C
C
C    THIS ROUTINE CALLS:        BORT
C    THIS ROUTINE IS CALLED BY: RDBFDX   RDUSDX
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES),
     .                MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2),
     .                IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES),
     .                TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES),
     .                TABD(MAXTBD,NFILES)

      CHARACTER*600 TABD
      CHARACTER*128 BORT_STR
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*8   NEMO
      CHARACTER*6   NUMB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  CHECK TABLE B AND D
C  -------------------

      DO N=1,NTBB(LUN)
      IF(NUMB.EQ.TABB(N,LUN)(1: 6)) GOTO 900
      IF(NEMO.EQ.TABB(N,LUN)(7:14)) GOTO 901
      ENDDO

      DO N=1,NTBD(LUN)
      IF(NUMB.EQ.TABD(N,LUN)(1: 6)) GOTO 902
      IF(NEMO.EQ.TABD(N,LUN)(7:14)) GOTO 903
      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE B FXY VALUE (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE B MNEMONIC (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE D FXY VALUE (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE D MNEMONIC (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO
      CALL BORT(BORT_STR)
      END
