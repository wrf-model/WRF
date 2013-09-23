      SUBROUTINE RDUSDX(LUNDX,LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    RDUSDX
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE READS AND PARSES A FILE CONTAINING A USER-
C   SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT, AND THEN STORES
C   THIS INFORMATION INTO INTERNAL ARRAYS IN COMMON BLOCK /TABABD/ (SEE
C   REMARKS FOR CONTENTS OF INTERNAL ARRAYS).  THIS SUBROUTINE PERFORMS
C   A FUNCTION SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE RDBFDX,
C   EXECPT THAT RDBFDX READS THE BUFR TABLE DIRECTLY FROM MESSAGES AT
C   BEGINNING OF AN INPUT BUFR FILE.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1996-12-17  J. WOOLLEN -- FIXED FOR SOME MVS COMPILER'S TREATMENT OF
C                           INTERNAL READS (INCREASES PORTABILITY)
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"; CORRECTED SOME MINOR ERRORS
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY; CHANGED CALL FROM BORT TO BORT2
C 2006-04-14  D. KEYSER  -- ABORTS IF A USER-DEFINED MESSAGE TYPE "011"
C                           IS READ (EITHER DIRECTLY FROM A TABLE A
C                           MNEMONIC OR FROM THE "Y" VALUE OF A TABLE A
C                           FXY SEQUENCE DESCRIPTOR), MESSAGE TYPE
C                           "011" IS RESERVED FOR DICTIONARY MESSAGES
C                           (PREVIOUSLY WOULD STORE DATA WITH MESSAGE
C                           TYPE "011" BUT SUCH MESSAGES WOULD BE
C                           SKIPPED OVER WHEN READ)
C 2007-01-19  J. ATOR    -- MODIFIED IN RESPONSE TO NUMBCK CHANGES
C 2009-03-23  J. ATOR    -- INCREASE SIZE OF BORT_STR2; USE STNTBIA
C 2013-01-08  J. WHITING -- ADD ERR= OPTION TO READ STATEMENT
C
C USAGE:    CALL RDUSDX (LUNDX, LUN)
C   INPUT ARGUMENT LIST:
C     LUNDX    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR USER-
C                SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C   INPUT FILES:
C     UNIT "LUNDX" - USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER
C                    FORMAT
C
C REMARKS:
C    CONTENTS OF INTERNAL ARRAYS WRITTEN INTO COMMON BLOCK /TABABD/:
C
C     For Table A entries:
C        NTBA(LUN)     - INTEGER: Number of Table A entries (note that
C                        NTBA(0) contains the maximum number of such
C                        entries as set within subroutine BFRINI)
C        TABA(N,LUN)   - CHARACTER*128: Table A entries, where
C                        N=1,2,3,...,NTBA(LUN)
C        IDNA(N,LUN,1) - INTEGER: Message type corresponding to
C                        TABA(N,LUN)
C        IDNA(N,LUN,2) - INTEGER: Message subtype corresponding to
C                        TABA(N,LUN)
C
C     For Table B entries:
C        NTBB(LUN)     - INTEGER: Number of Table B entries (note that
C                        NTBB(0) contains the maximum number of such
C                        entries as set within subroutine BFRINI)
C        TABB(N,LUN)   - CHARACTER*128: Table B entries, where
C                        N=1,2,3,...,NTBB(LUN)
C        IDNB(N,LUN)   - INTEGER: Bit-wise representation of the FXY
C                        value corresponding to TABB(N,LUN)
C
C     For Table D entries:
C        NTBD(LUN)     - INTEGER: Number of Table D entries (note that
C                        NTBD(0) contains the maximum number of such
C                        entries as set within subroutine BFRINI)
C        TABD(N,LUN)   - CHARACTER*600: Table D entries, where
C                        N=1,2,3,...,NTBD(LUN)
C        IDND(N,LUN)   - INTEGER: Bit-wise representation of the FXY
C                        value corresponding to TABD(N,LUN)
C
C
C    THIS ROUTINE CALLS:        BORT2    DXINIT   ELEMDX   IGETNTBI
C                               MAKESTAB NEMOCK   NUMBCK   SEQSDX
C                               STNTBI   STNTBIA
C    THIS ROUTINE IS CALLED BY: CKTABA   READDX
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
      CHARACTER*128 BORT_STR1
      CHARACTER*156 BORT_STR2
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*80  CARD
      CHARACTER*8   NEMO
      CHARACTER*6   NUMB,NMB2

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  INITIALIZE THE DICTIONARY TABLE CONTROL WORD PARTITION ARRAYS
C  WITH APRIORI TABLE B AND D ENTRIES
C  --------------------------------------------------------------

      CALL DXINIT(LUN,1)
      REWIND LUNDX

C  READ USER CARDS UNTIL THERE ARE NO MORE
C  ---------------------------------------

1     READ(LUNDX,'(A80)',END=200,ERR=200) CARD

C  REREAD IF NOT A DEFINITION CARD
C  -------------------------------

c  .... This is a comment line
      IF(CARD(1: 1).EQ.       '*') GOTO 1
c  .... This is a separation line
      IF(CARD(3:10).EQ.'--------') GOTO 1
c  .... This is a blank line
      IF(CARD(3:10).EQ.'        ') GOTO 1
c  .... This is a header line
      IF(CARD(3:10).EQ.'MNEMONIC') GOTO 1
c  .... This is a header line
      IF(CARD(3:10).EQ.'TABLE  D') GOTO 1
c  .... This is a header line
      IF(CARD(3:10).EQ.'TABLE  B') GOTO 1

C  PARSE A DESCRIPTOR DEFINITION CARD
C  ----------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(21:21).EQ.'|') THEN

c  .... NEMO is the 8-character mnemonic name
         NEMO = CARD(3:10)
         IRET=NEMOCK(NEMO)
         IF(IRET.EQ.-1) GOTO 900
         IF(IRET.EQ.-2) GOTO 901

c  .... NUMB is the 6-character FXY value corresponding to NEMO
         NUMB = CARD(14:19)
         NMB2 = NUMB
         IF(NMB2(1:1).EQ.'A') NMB2(1:1) = '3'
         IRET=NUMBCK(NMB2)
         IF(IRET.EQ.-1) GOTO 902
         IF(IRET.EQ.-2) GOTO 903
         IF(IRET.EQ.-3) GOTO 904
         IF(IRET.EQ.-4) GOTO 905

C  TABLE A DESCRIPTOR FOUND
C  ------------------------

         IF(NUMB(1:1).EQ.'A') THEN
	   N = IGETNTBI ( LUN, 'A' )
	   CALL STNTBIA ( N, LUN, NUMB, NEMO, CARD(23:) )
	   IF ( IDNA(N,LUN,1) .EQ. 11 ) GOTO 906
c  .... Replace "A" with "3" so Table D descriptor will be found in
c  .... card as well (see below)
	   NUMB(1:1) = '3'
         ENDIF

C  TABLE B DESCRIPTOR FOUND
C  ------------------------

         IF(NUMB(1:1).EQ.'0') THEN
	   CALL STNTBI ( IGETNTBI(LUN,'B'), LUN, NUMB, NEMO, CARD(23:) )
           GOTO 1
         ENDIF

C  TABLE D DESCRIPTOR FOUND
C  ------------------------

         IF(NUMB(1:1).EQ.'3') THEN
	   CALL STNTBI ( IGETNTBI(LUN,'D'), LUN, NUMB, NEMO, CARD(23:) )
           GOTO 1
         ENDIF

c  .... First character of NUMB is not 'A', '0' or '3'
         GOTO 902

      ENDIF

C  PARSE A SEQUENCE DEFINITION CARD
C  --------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(19:19).NE.'|') THEN
         CALL SEQSDX(CARD,LUN)
         GOTO 1
      ENDIF

C  PARSE AN ELEMENT DEFINITION CARD
C  --------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(19:19).EQ.'|') THEN
         CALL ELEMDX(CARD,LUN)
         GOTO 1
      ENDIF

C  CAN'T FIGURE OUT WHAT KIND OF CARD IT IS
C  ----------------------------------------

      GOTO 907

C  NORMAL ENDING
C  -------------

200   CALL MAKESTAB

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"MNEMONIC ",A," IN USER DICTIONARY IS NOT'//
     . ' BETWEEN 1 AND 8 CHARACTERS")') NEMO
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"MNEMONIC ",A," IN USER DICTIONARY HAS '//
     . 'INVALID CHARACTERS")') NEMO
      CALL BORT2(BORT_STR1,BORT_STR2)
902   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '//
     . 'DICTIONARY HAS AN INVALID FIRST CHARACTER (F VALUE) - MUST BE'//
     . ' A, 0 OR 3")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
903   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '//
     . 'DICTIONARY HAS NON-NUMERIC VALUES IN CHARACTERS 2-6 (X AND Y '//
     . 'VALUES)")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
904   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '//
     . 'DICTIONARY HAS INVALID NUMBER IN CHARACTERS 2-3 (X VALUE) - '//
     . 'MUST BE BETWEEN 00 AND 63")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
905   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '//
     . 'DICTIONARY HAS INVALID NUMBER IN CHARACTERS 4-6 (Y VALUE) - '//
     . 'MUST BE BETWEEN 000 AND 255")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
906   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"USER-DEFINED MESSAGE TYPE ""011"" IS '//
     . 'RESERVED FOR DICTIONARY MESSAGES")')
      CALL BORT2(BORT_STR1,BORT_STR2)
907   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"THIS CARD HAS A BAD FORMAT - IT IS NOT '//
     . 'RECOGNIZED BY THIS SUBROUTINE")')
      CALL BORT2(BORT_STR1,BORT_STR2)

      END
