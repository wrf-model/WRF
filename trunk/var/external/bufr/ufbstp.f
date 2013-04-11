      SUBROUTINE UFBSTP(LUNIO,USR,I1,I2,IRET,STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    UFBSTP (docblock incomplete)
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1999-11-18
C
C ABSTRACT: THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR FROM
C   THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
C   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF
C   ABS(LUNIO) (I.E., IF ABS(LUNIO) POINTS TO A BUFR FILE THAT IS OPEN
C   FOR INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
C   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET.
C   THE DATA VALUES CORRESPOND TO MNEMONICS WHICH ARE
C   ....
C   IF UFBSTP IS READING VALUES, THEN EITHER BUFR ARCHIVE LIBRARY
C   SUBROUTINE READSB OR READNS MUST HAVE BEEN PREVIOUSLY
C   CALLED TO READ THE SUBSET FROM UNIT ABS(LUNIO) INTO INTERNAL
C   MEMORY.  IF IT IS WRITING VALUES, THEN EITHER BUFR ARCHIVE LIBRARY
C   SUBROUTINE OPENMG OR OPENMB MUST HAVE BEEN PREVIOUSLY CALLED TO
C   OPEN AND INITIALIZE A BUFR MESSAGE WITHIN MEMORY FOR THIS ABS(LUNIO).
C
C PROGRAM HISTORY LOG:
C 1999-11-18  J. WOOLLEN -- ORIGINAL AUTHOR
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY) (INCOMPLETE); OUTPUTS MORE
C                           COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY OR UNUSUAL THINGS
C                           HAPPEN; CHANGED CALL FROM BORT TO BORT2 IN
C                           SOME CASES
C 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST1 AND IFIRST2 FLAGS
C
C USAGE:    CALL UFBSTP (LUNIO, USR, I1, I2, IRET, STR)
C   INPUT ARGUMENT LIST:
C     LUNIO    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT
C                NUMBER FOR BUFR FILE
C                  - IF BUFR FILE OPEN FOR OUTPUT AND LUNIO IS LESS
C                    THAN ZERO, UFBSTP TREATS THE BUFR FILE AS THOUGH
C                    IT WERE OPEN FOR INPUT
C     USR      - ONLY IF BUFR FILE OPEN FOR OUTPUT:
C                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
C                   WRITTEN TO DATA SUBSET
C     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE
C                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER
C                MUST BE AT LEAST AS LARGE AS LATTER)
C     I2       - INTEGER:
C                  - IF BUFR FILE OPEN FOR INPUT:  LENGTH OF SECOND
C                    DIMENSION OF USR
C                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS
C                    OF DATA VALUES TO BE WRITTEN TO DATA SUBSET
C                    (MAXIMUM VALUE IS 255)
C     STR      - CHARACTER*(*): STRING ....
C
C   OUTPUT ARGUMENT LIST:
C     USR      - ONLY IF BUFR FILE OPEN FOR INPUT:
C                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
C                   READ FROM DATA SUBSET
C     IRET     - INTEGER:
C                  - IF BUFR FILE OPEN FOR INPUT: NUMBER OF "LEVELS" OF
C                    DATA VALUES READ FROM DATA SUBSET (MUST BE NO
C                    LARGER THAN I2)
C                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
C                    OF DATA VALUES WRITTEN TO DATA SUBSET (SHOULD BE
C                    SAME AS I2)
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     BORT2    STATUS   STRING
C                               UFBSP
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
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
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2
      REAL*8        USR(I1,I2),VAL

      DATA IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      IRET = 0

C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------

      LUNIT = ABS(LUNIO)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IM.EQ.0) GOTO 901
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 902

      IO = MIN(MAX(0,IL),1)
      IF(LUNIO.NE.LUNIT) IO = 0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
         PRINT*,'BUFRLIB: UFBSTP - THIRD ARGUMENT (INPUT) IS .LE. 0',
     .    ' -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
         PRINT*,'STR = ',STR
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
         ENDIF
         GOTO 100
      ELSEIF(I2.LE.0) THEN
         IF(IPRT.EQ.-1)  IFIRST1 = 1
         IF(IO.EQ.0 .OR. IFIRST1.EQ.0 .OR. IPRT.GE.1)  THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
            PRINT*,'BUFRLIB: UFBSTP - FOURTH ARGUMENT (INPUT) IS .LE. ',
     .       '0 -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
            PRINT*,'STR = ',STR
            IF(IPRT.EQ.0 .AND. IO.EQ.1)  PRINT 101
101   FORMAT('Note: Only the first occurrence of this WARNING message ',
     . 'is printed, there may be more.  To output'/6X,'ALL WARNING ',
     . 'messages, modify your application program to add ',
     . '"CALL OPENBF(0,''QUIET'',1)" prior'/6X,'to the first call to a',
     . ' BUFRLIB routine.')
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
            IFIRST1 = 1
         ENDIF
         GOTO 100
      ENDIF

C  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
C  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

C  PARSE OR RECALL THE INPUT STRING - READ/WRITE VALUES
C  ----------------------------------------------------

      CALL STRING(STR,LUN,I1,IO)

C  CALL THE MNEMONIC READER/WRITER
C  -------------------------------

      CALL UFBSP(LUN,USR,I1,I2,IO,IRET)

      IF(IO.EQ.1 .AND. IRET.NE.I2) GOTO 903

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               PRINT*,'BUFRLIB: UFBSTP - NO SPECIFIED VALUES READ IN',
     .          ' -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
               PRINT*,'STR = ',STR
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
            ENDIF
         ELSE
            IF(IPRT.EQ.-1)  IFIRST2 = 1
            IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               PRINT*,'BUFRLIB: UFBSTP - NO SPECIFIED VALUES WRITTEN ',
     .          'OUT -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
               PRINT*,'STR = ',STR,' MAY NOT BE IN THE BUFR TABLE(?)'
               IF(IPRT.EQ.0)  PRINT 101
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
               IFIRST2 = 1
            ENDIF
         ENDIF
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBSTP - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   CALL BORT('BUFRLIB: UFBSTP - A MESSAGE MUST BE OPEN IN BUFR '//
     . 'FILE, NONE ARE')
902   CALL BORT('BUFRLIB: UFBSTP - LOCATION OF INTERNAL TABLE FOR '//
     . 'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL '//
     . 'SUBSET ARRAY')
903   WRITE(BORT_STR1,'("BUFRLIB: UFBSTP - MNEMONIC STRING READ IN IS'//
     . ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '//
     . 'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,")'//
     . ' - INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
