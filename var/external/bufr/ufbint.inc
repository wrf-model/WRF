      SUBROUTINE UFBINT(LUNIN,USR,I1,I2,IRET,STR)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    UFBINT
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR FROM
!   THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
!   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF
!   ABS(LUNIN) {I.E., IF ABS(LUNIN) POINTS TO A BUFR FILE THAT IS OPEN
!   FOR INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
!   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET}.
!   THE DATA VALUES CORRESPOND TO MNEMONICS WHICH ARE PART OF A
!   DELAYED-REPLICATION SEQUENCE, OR FOR WHICH THERE IS NO REPLICATION
!   AT ALL. IF UFBINT IS READING VALUES, THEN EITHER BUFR ARCHIVE
!   LIBRARY SUBROUTINE READSB OR READNS MUST HAVE BEEN PREVIOUSLY
!   CALLED TO READ THE SUBSET FROM UNIT ABS(LUNIN) INTO
!   INTERNAL MEMORY.  IF IT IS WRITING VALUES, THEN EITHER BUFR ARCHIVE
!   LIBRARY SUBROUTINE OPENMG OR OPENMB MUST HAVE BEEN PREVIOUSLY
!   CALLED TO OPEN AND INITIALIZE A BUFR MESSAGE WITHIN MEMORY FOR THIS
!   ABS(LUNIN).
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1996-11-25  J. WOOLLEN -- MODIFIED TO ADD A RETURN CODE WHEN
!                           MNEMONICS ARE NOT FOUND WHEN READING
! 1996-12-11  J. WOOLLEN -- REMOVED A HARD ABORT FOR USERS WHO TRY TO
!                           WRITE NON-EXISTING MNEMONICS
! 1996-12-17  J. WOOLLEN -- MODIFIED TO ALWAYS INITIALIZE "USR" ARRAY
!                           TO MISSING (10E10) WHEN BUFR FILE IS BEING
!                           READ
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"; IMPROVED MACHINE
!                           PORTABILITY
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
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
! USAGE:    CALL UFBINT (LUNIN, USR, I1, I2, IRET, STR)
!   INPUT ARGUMENT LIST:
!     LUNIN    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
!                FOR BUFR FILE
!                  - IF BUFR FILE OPEN FOR OUTPUT AND LUNIN IS LESS
!                    THAN ZERO, UFBINT TREATS THE BUFR FILE AS THOUGH
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
!     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B
!                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST
!                DIMENSION OF USR
!                  - IF BUFR FILE OPEN FOR INPUT: THIS CAN ALSO BE A
!                    SINGLE TABLE D (SEQUENCE) MNEMONIC WITH EITHER 8-
!                    OR 16-BIT DELAYED REPLICATION (SEE REMARKS 1)
!                  - IF BUFR FILE OPEN FOR INPUT: THERE ARE THREE
!                     "GENERIC" MNEMONICS NOT RELATED TO TABLE B OR D,
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
!    1) UFBINT CAN ALSO BE CALLED TO PROVIDE INFORMATION ABOUT A SINGLE
!       TABLE D (SEQUENCE) MNEMONIC WITH EITHER 8- OR 16-BIT DELAYED
!       REPLICATION IN A SUBSET WHEN THE BUFR FILE IS OPEN FOR INPUT.
!       THE MNEMONIC IN STR MUST APPEAR AS IT DOES IN THE BUFR TABLE,
!       I.E., BRACKETED BY "{" AND "}" OR "[" AND "]" FOR 8-BIT DELAYED
!       REPLICATION, OR BRACKETED BY "(" AND ")" FOR 16-BIT DELAYED
!       REPLICATION.  {NOTE: THIS WILL NOT WORK FOR SEQUENCES WITH
!       1-BIT DELAYED REPLICATION (BRACKETED BY "<" AND ">"), STANDARD
!       REPLICATION (BRACKETED BY "'s), OR NO REPLICATION (NO
!       BRACKETING SYMBOLS).}
!       
!       FOR EXAMPLE:
!
!       CALL UFBINT(LUNIN,PLEVL,1, 50,IRET,'{PLEVL}')
!
!       WILL RETURN WITH IRET EQUAL TO THE NUMBER OF OCCURRENCES OF THE
!       8-BIT DELAYED REPLICATION SEQUENCE PLEVL IN THE SUBSET AND WITH
!       (PLEVL(I),I=1,IRET) EQUAL TO THE NUMBER OF REPLICATIONS IN EACH
!       OCCURRENCE OF PLEVL IN THE SUBSET.  IF THERE ARE NO OCCURRENCES
!       OF PLEVL IN THE SUBSET, IRET IS RETURNED AS ZERO.
!
!    2) WHEN THE BUFR FILE IS OPEN FOR OUTPUT, UFBINT CAN BE USED TO
!       PRE-ALLOCATE SPACE FOR SOME OR ALL MNEMONICS WITHIN DELAYED
!       REPLICATION SEQUENCES.  A SUBSEQUENT CALL TO BUFR ARCHIVE
!       LIBRARY ROUTINE UFBREP OR UFBSEQ THEN ACTUALLY STORES THE
!       VALUES INTO THE BUFR FILES.  HERE ARE TWO EXAMPLES OF THIS:
!
!       EXAMPLE 1) PROBLEM: AN OUTPUT SUBSET "SEQNCE" IS LAID OUT AS
!          FOLLOWS IN A BUFR TABLE (NOTE 16 CHARACTERS HAVE BEEN
!          REMOVED FROM THE LAST COLUMN TO ALLOW THE TABLE TO FIT IN
!          THIS DOCBLOCK):
!
!       | SEQNCE   | {PLEVL}                                           |
!       | PLEVL    | WSPD WDIR TSIG PRLC TSIG PRLC TSIG PRLC           |
!
!              -- OR --
!
!       | SEQNCE   | {PLEVL}                                           |
!       | PLEVL    | WSPD WDIR "PSEQ"3                                 |
!       | PSEQ     | TSIG PRLC                                         |
!
!         IN THIS CASE THE APPLICATION PROGRAM MUST STORE VALUES WHICH
!         HAVE STANDARD REPLICATION NESTED INSIDE OF A DELAYED
!         REPLICATION SEQUENCE. FOR EXAMPLE,  ASSUME 50 LEVELS OF WIND
!         SPEED, WIND DIRECTION, OBSERVED PRESSURE, FIRST GUESS
!         PRESSURE AND ANALYZED PRESSURE ARE TO BE WRITTEN TO "SEQNCE".
!
!       THE FOLLOWING LOGIC WOULD ENCODE VALUES PROPERLY:
!.....................................................................
!              ....
!       REAL*8 DROBS(2,50)
!       REAL*8 SROBS(2,150)
!              ....
!       DO I=1,50
!         DROBS(1,I)     = Value of wind speed on level "I"
!         DROBS(2,I)     = Value of wind direction on level "I"
!         SROBS(1,I*3-2) = Value of observed pressure on level "I"
!         SROBS(2,I*3-2) = 25. ! Value in Code Table 0-08-021 (TSIG)
!                              !  for time sigificance (Nominal
!                              !  reporting time) for observed
!                              !  pressure on level "I"
!         SROBS(1,I*3-1) = Value of first guess pressure on level "I"
!         SROBS(2,I*3-1) = 27. ! Value in Code Table 0-08-021 (TSIG)
!                              !  for time sigificance (First guess)
!                              !  for first guess pressure on level "I"
!         SROBS(1,I*3) = Value of analyzed pressure on level "I"
!         SROBS(2,I*3) = 16.   ! Value in Code Table 0-08-021 (TSIG)
!                              !  for time sigificance (Analysis) for
!                              !  analyzed pressure on level "I"
!       ENDDO
!
!              ! The call to UFBINT here will not only store the 50
!              !  values of WSPD and WDIR into the BUFR subset, it
!              !  will also allocate the space to store three
!              !  replications of TSIG and PRLC on each of the 50
!              !  delayed-replication "levels"
!       CALL UFBINT(LUNIN,DROBS,2, 50,IRET,'WSPD WDIR')
!
!              ! The call to UFBREP here will actually store the 150
!              !  values of both TSIG and PRLC (three values for each
!              !  on 50 delayed-replication "levels")
!       CALL UFBREP(LUNIN,SROBS,2,150,IRET,'TSIG PRLC')
!              ....
!       STOP
!       END
!.....................................................................
!
!       A SIMILAR EXAMPLE COULD BE PROVIDED FOR READING VALUES WHICH
!       HAVE STANDARD REPLICATION NESTED WITHIN DELAYED REPLICATION,
!       FROM BUFR FILES OPEN FOR INPUT.  (NOT SHOWN HERE.)
!
!
!       EXAMPLE 2) PROBLEM: AN INPUT SUBSET, "REPT_IN", AND AN OUTPUT
!          SUBSET "REPT_OUT", ARE LAID OUT AS FOLLOWS IN A BUFR TABLE
!          (NOTE 16 CHARACTERS HAVE BEEN REMOVED FROM THE LAST COLUMN
!          TO ALLOW THE TABLE TO FIT IN THIS DOCBLOCK):
!
!       | REPT_IN  | YEAR MNTH DAYS HOUR MINU {PLEVL} CLAT CLON        |
!       | REPT_OUT | YEAR DOYR HOUR MINU {PLEVL} CLAT CLON             |
!       | PLEVL    | PRLC TMBD REHU WDIR WSPD
!
!         IN THIS CASE THE APPLICATION PROGRAM IS READING IN VALUES
!         FROM A BUFR FILE CONTAINING SUBSET "REPT_IN", CONVERTING
!         MONTH AND DAY TO DAY OF YEAR, AND THEN WRITING VALUES TO
!         SUBSET "REPT_OUT" IN ANOTHER BUFR FILE.  A CONVENIENT WAY TO
!         DO THIS IS TO CALL UFBSEQ TO READ IN AND WRITE OUT THE
!         VALUES, HOWEVER THIS IS COMPLICATED BY THE PRESENCE OF THE
!         DELAYED-RELICATION SEQUENCE "PLEVL" BECAUSE THE OUTPUT CALL
!         TO UFBSEQ DOES NOT KNOW A-PRIORI HOW MANY REPLICATIONS ARE
!         NEEDED TO STORE THE CONTENTS OF "PLEVL" (IT SETS THE NUMBER
!         TO ZERO BY DEFUALT).  A CALL TO UFBINT IS FIRST NEEDED TO
!         ALLOCATE THE SPACE AND DETERMINE THE NUMBER OF LEVELS NEEDED
!         TO STORE ALL VALUES IN "PLEVL".
!
!       THE FOLLOWING LOGIC WOULD PEFORM THE READ/WRITE PROPERLY:
!.....................................................................
!              ....
!       REAL*8 OBSI(2000),OBSO(1999),PLEVL(5,255),REPS_8
!       CHARACTER*8 SUBSET
!              ....
!
!       CALL DATELEN(10)
!
!         ! Open input BUFR file in LUBFI and open output BUFR file in
!         !  LUBFJ, both use the BUFR table in LINDX
!       CALL OPENBF(LUBFI,'IN', LINDX)
!       CALL OPENBF(LUBFJ,'OUT',LINDX)
!
!         ! Read through the BUFR messages in the input file
!       DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
!
!         ! Open message (for writing) in output file
!          CALL OPENMB(LUBFJ,'REPT_OUT',IDATE)
!
!         ! Read through the subsets in this input BUFR messages
!          DO WHILE(IREADSB(LUBFI).EQ.0)
!
!              ! This call to UFBSEQ will read in the entire contents
!              !  of subset "REPT_IN", storing them into array OBSI
!              !  (Note: On input, UFBSEQ knows how many replications
!                        of "PLEV" are present)
!             CALL UFBSEQ(LUBFI,OBSI,2000,1,IRET,'REPT_IN')
!
!              ! This call to UFBINT will return the number of
!              !  replications ("levels") in "PLEVL" for subset
!              !  "REPT_IN"" !  {see 1) above in REMARKS}
!             CALL UFBINT(LUBFI,REPS_8,1,1,IRET,'{PLEVL}')
!             IREPS = REPS_8
!
!             IYR = OBSI(1)
!             IMO = OBSI(2)
!             IDA = OBSI(3)
!             CALL xxxx(IYR, IMO, IDA, JDY) ! convert month and day
!                                           !  to day of year (JDY)
!             OBSO(1) = OBSI(1)
!             OBSO(2) = JDY
!             DO I = 3,1999
!                OBSO(I) = OBSI(1+1)
!             ENDDO
!
!             PLEVL = 10E10
!
!              ! The call to UFBINT here will allocate the space to
!              !  later allow UFBSEQ to store IREPS replications of
!              !  "PLEVL" into the output BUFR subset "REPT_OUT" (note
!              !  here it is simply storing missing values)
!             CALL UFBINT(LUBFJ,PLEVL,5,IREPS,IRET,
!     $        'PRLC TMBD REHU WDIR WSPD')
!
!              ! The call to UFBSEQ here will write out the entire
!              !  contents of subset "REPT_OUT", reading them from
!              !  array OBSO
!             CALL UFBSEQ(LUBFJ,OBSO,1999,1,IRET,'REPT_OUT')
!
!              ! Write the subset into the output BUFR message
!             CALL WRITSB(LUBFJ)
!          ENDDO
!
!              ! All done
!
!          STOP
!          END
!.....................................................................
!
!
!    THIS ROUTINE CALLS:        BORT     BORT2    STATUS   STRING
!                               TRYBUMP  UFBRW
!    THIS ROUTINE IS CALLED BY: UFBINX   UFBRMS
!                               Also called by application programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES), &
                      INODE(NFILES),IDATE(NFILES)
      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
!     COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)
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

      LUNIT = ABS(LUNIN)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IM.EQ.0) GOTO 901
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 902

      IO = MIN(MAX(0,IL),1)
      IF(LUNIT.NE.LUNIN) IO = 0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
         PRINT*,'BUFRLIB: UFBINT - THIRD ARGUMENT (INPUT) IS .LE. 0', &
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
            PRINT*,'BUFRLIB: UFBINT - FOURTH ARGUMENT (INPUT) IS .LE.', &
             ' 0 -  RETURN WITH FIFTH ARGUMENT (IRET) = 0'
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

!  PARSE OR RECALL THE INPUT STRING
!  --------------------------------

      CALL STRING(STR,LUN,I1,IO)

!  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
!  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

!  CALL THE MNEMONIC READER/WRITER
!  -------------------------------

      CALL UFBRW(LUN,USR,I1,I2,IO,IRET)

!  IF INCOMPLETE WRITE TRY TO INITIALIZE REPLICATION SEQUENCE OR RETURN
!  ---------------------------------------------------------------------

      IF(IO.EQ.1 .AND. IRET.NE.I2 .AND. IRET.GE.0) THEN
         CALL TRYBUMP(LUNIT,LUN,USR,I1,I2,IO,IRET)
         IF(IRET.NE.I2) GOTO 903
      ELSEIF(IRET.EQ.-1) THEN
         IRET = 0
      ENDIF

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
               PRINT*,'BUFRLIB: UFBINT - NO SPECIFIED VALUES READ IN', &
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
               PRINT*,'BUFRLIB: UFBINT - NO SPECIFIED VALUES WRITTEN ', &
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
900   CALL BORT('BUFRLIB: UFBINT - BUFR FILE IS CLOSED, IT MUST BE'// &
       ' OPEN')
901   CALL BORT('BUFRLIB: UFBINT - A MESSAGE MUST BE OPEN IN BUFR '// &
       'FILE, NONE ARE')
902   CALL BORT('BUFRLIB: UFBINT - LOCATION OF INTERNAL TABLE FOR '// &
       'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL '// &
       'SUBSET ARRAY')
903   WRITE(BORT_STR1,'("BUFRLIB: UFBINT - MNEMONIC STRING READ IN IS'// &
       ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '// &
       'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,")'// &
       ' - INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
      END SUBROUTINE UFBINT
