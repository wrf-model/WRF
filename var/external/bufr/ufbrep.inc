      SUBROUTINE UFBREP(LUNIO,USR,I1,I2,IRET,STR)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    UFBREP
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR
!   FROM THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
!   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF
!   ABS(LUNIO) (I.E., IF ABS(LUNIO) POINTS TO A BUFR FILE THAT IS OPEN
!   FOR INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
!   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET.
!   THE DATA VALUES CORRESPOND TO MNEMONICS WHICH ARE PART OF A REGULAR
!   (I.E., NON-DELAYED) REPLICATION SEQUENCE OR FOR THOSE WHICH ARE
!   REPLICATED VIA BEING DIRECTLY LISTED MORE THAN ONCE WITHIN AN
!   OVERALL SUBSET DEFINITION RATHER THAN BY BEING INCLUDED WITHIN A
!   REPLICATION SEQUENCE.  IF UFBREP IS READING VALUES, THEN EITHER
!   BUFR ARCHIVE LIBRARY SUBROUTINE READSB OR READNS MUST HAVE
!   BEEN PREVIOUSLY CALLED TO READ THE SUBSET FROM UNIT ABS(LUNIO) INTO
!   INTERNAL MEMORY.  IF IT IS WRITING VALUES, THEN EITHER BUFR ARCHIVE
!   LIBRARY SUBROUTINE OPENMG OR OPENMB MUST HAVE BEEN PREVIOUSLY
!   CALLED TO OPEN AND INITIALIZE A BUFR MESSAGE WITHIN MEMORY FOR THIS
!   ABS(LUNIO).
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2003-05-19  J. WOOLLEN -- DISABLED THE PARSING SWITCH WHICH CONTROLS
!                           CHECKING FOR IN THE SAME REPLICATION GROUP,
!                           UFBREP DOES NOT NEED THIS CHECK, AND IT
!                           INTERFERES WITH WHAT UFBREP CAN DO
!                           OTHERWISE
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
!                           INCREASED FROM 15000 TO 16000 (WAS IN
!                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
!                           WRF; ADDED DOCUMENTATION (INCLUDING
!                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
!                           INFO WHEN ROUTINE TERMINATES ABNORMALLY OR
!                           UNUSUAL THINGS HAPPEN; CHANGED CALL FROM
!                           BORT TO BORT2 IN SOME CASES
! 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST1 AND IFIRST2 FLAGS
!
! USAGE:    CALL UFBREP (LUNIO, USR, I1, I2, IRET, STR)
!   INPUT ARGUMENT LIST:
!     LUNIO    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
!                FOR BUFR FILE
!                  - IF BUFR FILE OPEN FOR OUTPUT AND LUNIO IS LESS
!                    THAN ZERO, UFBREP TREATS THE BUFR FILE AS THOUGH
!                    IT WERE OPEN FOR INPUT
!     USR      - ONLY IF BUFR FILE OPEN FOR OUTPUT:
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
!                   WRITTEN TO DATA SUBSET
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR OR THE
!                NUMBER OF BLANK-SEPARATED MNEMONICS IN STR (FORMER
!                MUST BE AT LEAST AS LARGE AS LATTER)
!     I2       - INTEGER:
!                  - IF BUFR FILE OPEN FOR INPUT:  LENGTH OF SECOND
!                    DIMENSION OF USR
!                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
!                    OF DATA VALUES TO BE WRITTEN TO DATA SUBSET
!                    (MAXIMUM VALUE IS 255)
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST
!                DIMENSION OF USR
!                  - IF BUFR FILE OPEN FOR INPUT: THERE ARE THREE
!                     "GENERIC" MNEMONICS NOT RELATED TO TABLE B,
!                     THESE RETURN THE FOLLOWING INFORMATION IN
!                     CORRESPONDING USR LOCATION:
!                     'NUL'  WHICH ALWAYS RETURNS MISSING (10E10)
!                     'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR
!                            MESSAGE (RECORD) NUMBER IN WHICH THIS
!                            SUBSET RESIDES
!                     'ISUB' WHICH ALWAYS RETURNS THE CURRENT SUBSET
!                            NUMBER OF THIS SUBSET WITHIN THE BUFR
!                            MESSAGE (RECORD) NUMBER 'IREC'
!
!   OUTPUT ARGUMENT LIST:
!     USR      - ONLY IF BUFR FILE OPEN FOR INPUT:
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
!                   READ FROM DATA SUBSET
!     IRET     - INTEGER:
!                  - IF BUFR FILE OPEN FOR INPUT: NUMBER OF "LEVELS" OF
!                    DATA VALUES READ FROM DATA SUBSET (MUST BE NO
!                    LARGER THAN I2)
!                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
!                    OF DATA VALUES WRITTEN TO DATA SUBSET (SHOULD BE
!                    SAME AS I2)
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT     BORT2    STATUS   STRING
!                               UFBRP
!    THIS ROUTINE IS CALLED BY: None
!                               Normally called only by application
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
!     COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)
      COMMON /ACMODE/ IAC
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR1,BORT_STR2
      REAL*8        USR(I1,I2)!,VAL

      DATA IFIRST1/0/,IFIRST2/0/

      SAVE IFIRST1, IFIRST2

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      IRET = 0

!  CHECK THE FILE STATUS AND I-NODE
!  --------------------------------

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
         PRINT*,'BUFRLIB: UFBREP - THIRD ARGUMENT (INPUT) IS .LE. 0', &
          ' -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
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
            PRINT*,'BUFRLIB: UFBREP - FOURTH ARGUMENT (INPUT) IS .LE. ', &
             '0 -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
            PRINT*,'STR = ',STR
            IF(IPRT.EQ.0 .AND. IO.EQ.1)  PRINT 101
101   FORMAT('Note: Only the first occurrence of this WARNING message ', &
       'is printed, there may be more.  To output'/6X,'ALL WARNING ', &
       'messages, modify your application program to add ', &
       '"CALL OPENBF(0,''QUIET'',1)" prior'/6X,'to the first call to a', &
       ' BUFRLIB routine.')
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
            IFIRST1 = 1
         ENDIF
         GOTO 100
      ENDIF

!  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
!  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

!  PARSE OR RECALL THE INPUT STRING - READ/WRITE VALUES
!  ----------------------------------------------------

      IA2 = IAC
      IAC = 1
      CALL STRING(STR,LUN,I1,IO)

!  CALL THE MNEMONIC READER/WRITER
!  -------------------------------

      CALL UFBRP(LUN,USR,I1,I2,IO,IRET)
      IAC = IA2

      IF(IO.EQ.1 .AND. IRET.LT.I2) GOTO 903

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               PRINT*,'BUFRLIB: UFBREP - NO SPECIFIED VALUES READ IN', &
                ' -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
               PRINT*,'STR = ',STR
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
            ENDIF
         ELSE
            IF(IPRT.EQ.-1)  IFIRST2 = 1
            IF(IFIRST2.EQ.0 .OR. IPRT.GE.1)  THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               PRINT*,'BUFRLIB: UFBREP - NO SPECIFIED VALUES WRITTEN ', &
                'OUT -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
               PRINT*,'STR = ',STR,' MAY NOT BE IN THE BUFR TABLE(?)'
               IF(IPRT.EQ.0)  PRINT 101
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
               IFIRST2 = 1
            ENDIF
         ENDIF
      ENDIF

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFBREP - BUFR FILE IS CLOSED, IT MUST BE'// &
       ' OPEN')
901   CALL BORT('BUFRLIB: UFBREP - A MESSAGE MUST BE OPEN IN BUFR '// &
       'FILE, NONE ARE')
902   CALL BORT('BUFRLIB: UFBREP - LOCATION OF INTERNAL TABLE FOR '// &
       'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL '// &
       'SUBSET ARRAY')
903   WRITE(BORT_STR1,'("BUFRLIB: UFBREP - MNEMONIC STRING READ IN IS'// &
       ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
       'WRITTEN (",I3,") LESS THAN THE NUMBER REQUESTED (",I3,") - '// &
       'INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
      END SUBROUTINE UFBREP
