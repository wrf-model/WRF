      SUBROUTINE RDUSDX(LUNDX,LUN)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    RDUSDX
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE READS AND PARSES A FILE CONTAINING A USER-
!   SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT, AND THEN STORES
!   THIS INFORMATION INTO INTERNAL ARRAYS IN COMMON BLOCK /TABABD/ (SEE
!   REMARKS FOR CONTENTS OF INTERNAL ARRAYS).  THIS SUBROUTINE PERFORMS
!   A FUNCTION SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE RDBFDX,
!   EXECPT THAT RDBFDX READS THE BUFR TABLE DIRECTLY FROM MESSAGES AT
!   BEGINNING OF AN INPUT BUFR FILE.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
! 1996-12-17  J. WOOLLEN -- FIXED FOR SOME MVS COMPILER'S TREATMENT OF
!                           INTERNAL READS (INCREASES PORTABILITY)
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"; CORRECTED SOME MINOR ERRORS
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION; OUTPUTS MORE COMPLETE
!                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
!                           ABNORMALLY; CHANGED CALL FROM BORT TO BORT2
! 2006-04-14  D. KEYSER  -- ABORTS IF A USER-DEFINED MESSAGE TYPE "011"
!                           IS READ (EITHER DIRECTLY FROM A TABLE A
!                           MNEMONIC OR FROM THE "Y" VALUE OF A TABLE A
!                           FXY SEQUENCE DESCRIPTOR), MESSAGE TYPE
!                           "011" IS RESERVED FOR DICTIONARY MESSAGES
!                           (PREVIOUSLY WOULD STORE DATA WITH MESSAGE
!                           TYPE "011" BUT SUCH MESSAGES WOULD BE
!                           SKIPPED OVER WHEN READ)
! 2007-01-19  J. ATOR    -- MODIFIED IN RESPONSE TO NUMBCK CHANGES
!
! USAGE:    CALL RDUSDX (LUNDX, LUN)
!   INPUT ARGUMENT LIST:
!     LUNDX    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR USER-
!                SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!
!   INPUT FILES:
!     UNIT "LUNDX" - USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER
!                    FORMAT
!
! REMARKS:
!    CONTENTS OF INTERNAL ARRAYS WRITTEN INTO COMMON BLOCK /TABABD/:
!
!     For Table A entries:
!        NTBA(LUN)     - INTEGER: Number of Table A entries (note that
!                        NTBA(0) contains the maximum number of such
!                        entries as set within subroutine BFRINI)
!        TABA(N,LUN)   - CHARACTER*128: Table A entries, where
!                        N=1,2,3,...,NTBA(LUN)
!        IDNA(N,LUN,1) - INTEGER: Message type corresponding to
!                        TABA(N,LUN)
!        IDNA(N,LUN,2) - INTEGER: Message subtype corresponding to
!                        TABA(N,LUN)
!
!     For Table B entries:
!        NTBB(LUN)     - INTEGER: Number of Table B entries (note that
!                        NTBB(0) contains the maximum number of such
!                        entries as set within subroutine BFRINI)
!        TABB(N,LUN)   - CHARACTER*128: Table B entries, where
!                        N=1,2,3,...,NTBB(LUN)
!        IDNB(N,LUN)   - INTEGER: Bit-wise representation of the FXY
!                        value corresponding to TABB(N,LUN)
!
!     For Table D entries:
!        NTBD(LUN)     - INTEGER: Number of Table D entries (note that
!                        NTBD(0) contains the maximum number of such
!                        entries as set within subroutine BFRINI)
!        TABD(N,LUN)   - CHARACTER*600: Table D entries, where
!                        N=1,2,3,...,NTBD(LUN)
!        IDND(N,LUN)   - INTEGER: Bit-wise representation of the FXY
!                        value corresponding to TABD(N,LUN)
!
!
!    THIS ROUTINE CALLS:        BORT2    DIGIT    DXINIT   ELEMDX
!                               IFXY     MAKESTAB NEMOCK   NENUAA
!                               NENUBD   NUMBCK   SEQSDX
!    THIS ROUTINE IS CALLED BY: CKTABA   READDX
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES), &
                      MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2), &
                      IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES), &
                      TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES), &
                      TABD(MAXTBD,NFILES)

      CHARACTER*600 TABD
      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*80  CARD
      CHARACTER*8   NEMO
      CHARACTER*6   NUMB,NMB2

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  INITIALIZE THE DICTIONARY TABLE CONTROL WORD PARTITION ARRAYS
!   WITH APRIORI TABLE B AND D ENTRIES
!  --------------------------------------------------------------

      CALL DXINIT(LUN,1)
      REWIND LUNDX

!  READ USER CARDS UNTIL THERE ARE NO MORE
!  ---------------------------------------

1     READ(LUNDX,'(A80)',END=200) CARD

!  REREAD IF NOT A DEFINITION CARD
!  -------------------------------

!  .... This is a comment line
      IF(CARD(1: 1).EQ.       '*') GOTO 1
!  .... This is a separation line
      IF(CARD(3:10).EQ.'--------') GOTO 1
!  .... This is a blank line
      IF(CARD(3:10).EQ.'        ') GOTO 1
!  .... This is a header line
      IF(CARD(3:10).EQ.'MNEMONIC') GOTO 1
!  .... This is a header line
      IF(CARD(3:10).EQ.'TABLE  D') GOTO 1
!  .... This is a header line
      IF(CARD(3:10).EQ.'TABLE  B') GOTO 1

!  PARSE A DESCRIPTOR DEFINITION CARD
!  ----------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(21:21).EQ.'|') THEN

!  .... NEMO is the 8-character mnemonic name
         NEMO = CARD(3:10)
         IRET=NEMOCK(NEMO)
         IF(IRET.EQ.-1) GOTO 900
         IF(IRET.EQ.-2) GOTO 901

!  .... NEMB is the 6-character FXY value corresponding to NEMO
         NUMB = CARD(14:19)
         NMB2 = NUMB
         IF(NMB2(1:1).EQ.'A') NMB2(1:1) = '3'
         IRET=NUMBCK(NMB2)
         IF(IRET.EQ.-1) GOTO 902
         IF(IRET.EQ.-2) GOTO 903
         IF(IRET.EQ.-3) GOTO 904
         IF(IRET.EQ.-4) GOTO 905

!  TABLE A DESCRIPTOR FOUND
!  ------------------------

         IF(NUMB(1:1).EQ.'A') THEN

            N = NTBA(LUN)+1
            IF(N.GT.NTBA(0)) GOTO 906
            CALL NENUAA(NEMO,NUMB,LUN)
!  .... Y value from descriptor
            TABA(N,LUN)( 1: 3) = NUMB(4:6)
!  .... Mnemonic
            TABA(N,LUN)( 4:11) = NEMO
!  .... Description
            TABA(N,LUN)(13:67) = CARD(23:77)
            NTBA(LUN) = N

            IF(DIGIT(NEMO(3:8))) THEN
!  .... Message type & subtype obtained directly from Table A mnemonic
               READ(NEMO,'(2X,2I3)') MTYP,MSBT
               IF(NEMO(3:5).EQ.'011')  GOTO 907
               IDNA(N,LUN,1) = MTYP
               IDNA(N,LUN,2) = MSBT
            ELSE
!  .... Message type obtained from Y value of Table A seq. descriptor
               IF(NUMB(4:6).EQ.'011')  GOTO 907
               READ(NUMB(4:6),'(I3)') IDNA(N,LUN,1)
!  ........ Message subtype hardwired to ZERO
               IDNA(N,LUN,2) = 0
            ENDIF
!  .... Replace "A" with "3" so Table D descriptor will be found in
!  .... card as well (below)
            NUMB(1:1) = '3'

         ENDIF

!  TABLE B DESCRIPTOR FOUND
!  ------------------------

         IF(NUMB(1:1).EQ.'0') THEN

            N = NTBB(LUN)+1
            IF(N.GT.NTBB(0)) GOTO 908
            CALL NENUBD(NEMO,NUMB,LUN)
!  .... Integer representation of FXY descriptor
            IDNB(N,LUN) = IFXY(NUMB)
!  .... Character representation of FXY descriptor
            TABB(N,LUN)( 1: 6) = NUMB
!  .... Mnemonic
            TABB(N,LUN)( 7:14) = NEMO
!  .... Description
            TABB(N,LUN)(16:70) = CARD(23:77)
            NTBB(LUN) = N
            GOTO 1

         ENDIF

!  TABLE D DESCRIPTOR FOUND
!  ------------------------

         IF(NUMB(1:1).EQ.'3') THEN

            N = NTBD(LUN)+1
            IF(N.GT.NTBD(0)) GOTO 909
            CALL NENUBD(NEMO,NUMB,LUN)
!  .... Integer representation of FXY descriptor
            IDND(N,LUN) = IFXY(NUMB)
!  .... Character representation of FXY descriptor
            TABD(N,LUN)( 1: 6) = NUMB
!  .... Mnemonic
            TABD(N,LUN)( 7:14) = NEMO
!  .... Description
            TABD(N,LUN)(16:70) = CARD(23:77)
            NTBD(LUN) = N
            GOTO 1

         ENDIF

!  .... First character of NUMB is not 'A', '0' or '3'
         GOTO 902

      ENDIF

!  PARSE A SEQUENCE DEFINITION CARD
!  --------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(19:19).NE.'|') THEN
         CALL SEQSDX(CARD,LUN)
         GOTO 1
      ENDIF

!  PARSE AN ELEMENT DEFINITION CARD
!  --------------------------------

      IF(CARD(12:12).EQ.'|' .AND. CARD(19:19).EQ.'|') THEN
         CALL ELEMDX(CARD,LUN)
         GOTO 1
      ENDIF

!  CAN'T FIGURE OUT WHAT KIND OF CARD IT IS
!  ----------------------------------------

      GOTO 910

!  NORMAL ENDING
!  -------------

200   CALL MAKESTAB

!  EXITS
!  -----

      RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"MNEMONIC ",A," IN USER DICTIONARY IS NOT'// &
       ' BETWEEN 1 AND 8 CHARACTERS")') NEMO
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"MNEMONIC ",A," IN USER DICTIONARY HAS '// &
       'INVALID CHARACTERS")') NEMO
      CALL BORT2(BORT_STR1,BORT_STR2)
902   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
       'DICTIONARY HAS AN INVALID FIRST CHARACTER (F VALUE) - MUST BE'// &
       ' A, 0 OR 3")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
903   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
       'DICTIONARY HAS NON-NUMERIC VALUES IN CHARACTERS 2-6 (X AND Y '// &
       'VALUES)")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
904   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
       'DICTIONARY HAS INVALID NUMBER IN CHARACTERS 2-3 (X VALUE) - '// &
       'MUST BE BETWEEN 00 AND 63")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
905   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"DESCRIPTOR NUMBER ",A," IN USER '// &
       'DICTIONARY HAS INVALID NUMBER IN CHARACTERS 4-6 (Y VALUE) - '// &
       'MUST BE BETWEEN 000 AND 255")') NUMB
      CALL BORT2(BORT_STR1,BORT_STR2)
906   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF TABLE A ENTRIES IN USER '// &
       'DICTIONARY EXCEEDS THE LIMIT (",I4,")")') NTBA(0)
      CALL BORT2(BORT_STR1,BORT_STR2)
907   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"USER-DEFINED MESSAGE TYPE ""011"" IS '// &
       'RESERVED FOR DICTIONARY MESSAGES")')
      CALL BORT2(BORT_STR1,BORT_STR2)
908   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF TABLE B ENTRIES IN USER '// &
       'DICTIONARY EXCEEDS THE LIMIT (",I4,")")') NTBB(0)
      CALL BORT2(BORT_STR1,BORT_STR2)
909   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF TABLE D ENTRIES IN USER '// &
       'DICTIONARY EXCEEDS THE LIMIT (",I4,")")') NTBD(0)
      CALL BORT2(BORT_STR1,BORT_STR2)
910   WRITE(BORT_STR1,'("BUFRLIB: RDUSDX - CARD READ IN IS: ",A)') CARD
      WRITE(BORT_STR2,'(18X,"THIS CARD HAS A BAD FORMAT - IT IS NOT '// &
       'RECOGNIZED BY THIS SUBROUTINE")')
      CALL BORT2(BORT_STR1,BORT_STR2)
      END SUBROUTINE RDUSDX
