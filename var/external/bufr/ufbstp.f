      SUBROUTINE UFBSTP(LUNIO,USR,I1,I2,IRET,STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    UFBSTP
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1999-11-18
C
C ABSTRACT: THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR FROM
C   THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
C   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF
C   ABS(LUNIO) (I.E., IF ABS(LUNIO) POINTS TO A BUFR FILE THAT IS OPEN
C   FOR INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
C   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET).
C   THE DATA VALUES CORRESPOND TO INTERNAL ARRAYS REPRESENTING PARSED
C   STRINGS OF MNEMONICS WHICH ARE EITHER:
C       1) PART OF A REGULAR (I.E., NON-DELAYED) REPLICATION SEQUENCE
C                OR
C       2) REPLICATED BY BEING DIRECTLY LISTED MORE THAN ONCE WITHIN AN
C          OVERALL SUBSET DEFINITION
C   SO IN THAT RESPECT IT IS VERY SIMILAR TO BUFR ARCHIVE LIBRARY
C   SUBROUTINE UFBREP.  HOWEVER, THERE IS AN IMPORTANT DIFFERENCE IN
C   HOW UFBSTP PROCESSES THE INPUT MNEMONIC STRING STR; FOR MORE DETAILS
C   SEE THE EXAMPLE IN THE DOCBLOCK FOR SUBROUTINE UFBREP.
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
C 2009-04-21  J. ATOR    -- USE ERRWRT
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
C     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B
C                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST
C                DIMENSION OF USR
C                  - IF BUFR FILE OPEN FOR INPUT: THERE ARE THREE
C                     "GENERIC" MNEMONICS NOT RELATED TO TABLE B,
C                     THESE RETURN THE FOLLOWING INFORMATION IN
C                     CORRESPONDING USR LOCATION:
C                     'NUL'  WHICH ALWAYS RETURNS BMISS ("MISSING")
C                     'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR
C                            MESSAGE (RECORD) NUMBER IN WHICH THIS
C                            SUBSET RESIDES
C                     'ISUB' WHICH ALWAYS RETURNS THE CURRENT SUBSET
C                            NUMBER OF THIS SUBSET WITHIN THE BUFR
C                            MESSAGE (RECORD) NUMBER 'IREC'
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
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     BORT2    ERRWRT   STATUS
C                               STRING   UFBSP
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
      COMMON /USRINT/ NVAL(NFILES),INV(MAXSS,NFILES),VAL(MAXSS,NFILES)
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2,ERRSTR
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
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBSTP - 3rd ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ELSEIF(I2.LE.0) THEN
         IF(IPRT.EQ.-1)  IFIRST1 = 1
         IF(IO.EQ.0 .OR. IFIRST1.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBSTP - 4th ARG. (INPUT) IS .LE. 0, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
            IF(IPRT.EQ.0 .AND. IO.EQ.1) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING ' //
     .   'message is printed, there may be more.  To output all ' //
     .   'such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add ' //
     .   '"CALL OPENBF(0,''QUIET'',1)" prior to the first call ' //
     .   'to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
            ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
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
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBSTP - NO SPECIFIED VALUES READ IN, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
            ENDIF
         ELSE
            IF(IPRT.EQ.-1)  IFIRST2 = 1
            IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBSTP - NO SPECIFIED VALUES WRITTEN OUT, ' //
     .   'SO RETURN WITH 5th ARG. (IRET) = 0; 6th ARG. (STR) ='
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT(STR)
      CALL ERRWRT('MAY NOT BE IN THE BUFR TABLE(?)')
               IF(IPRT.EQ.0) THEN
      ERRSTR = 'Note: Only the first occurrence of this WARNING ' //
     .   'message is printed, there may be more.  To output all ' //
     .   'such messages,'
      CALL ERRWRT(ERRSTR)
      ERRSTR = 'modify your application program to add ' //
     .   '"CALL OPENBF(0,''QUIET'',1)" prior to the first call ' //
     .   'to a BUFRLIB routine.'
      CALL ERRWRT(ERRSTR)
               ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
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
