      SUBROUTINE UFBINT(LUNIN,USR,I1,I2,IRET,STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    UFBINT
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR FROM
C   THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
C   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF
C   ABS(LUNIN) (I.E., IF ABS(LUNIN) POINTS TO A BUFR FILE THAT IS OPEN
C   FOR INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
C   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET).
C   THE DATA VALUES CORRESPOND TO MNEMONICS WHICH ARE PART OF A
C   DELAYED-REPLICATION SEQUENCE, OR FOR WHICH THERE IS NO REPLICATION
C   AT ALL. IF UFBINT IS READING VALUES, THEN EITHER BUFR ARCHIVE
C   LIBRARY SUBROUTINE READSB OR READNS MUST HAVE BEEN PREVIOUSLY
C   CALLED TO READ THE SUBSET FROM UNIT ABS(LUNIN) INTO
C   INTERNAL MEMORY.  IF IT IS WRITING VALUES, THEN EITHER BUFR ARCHIVE
C   LIBRARY SUBROUTINE OPENMG OR OPENMB MUST HAVE BEEN PREVIOUSLY
C   CALLED TO OPEN AND INITIALIZE A BUFR MESSAGE WITHIN MEMORY FOR THIS
C   ABS(LUNIN).
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1996-11-25  J. WOOLLEN -- MODIFIED TO ADD A RETURN CODE WHEN
C                           MNEMONICS ARE NOT FOUND WHEN READING
C 1996-12-11  J. WOOLLEN -- REMOVED A HARD ABORT FOR USERS WHO TRY TO
C                           WRITE NON-EXISTING MNEMONICS
C 1996-12-17  J. WOOLLEN -- MODIFIED TO ALWAYS INITIALIZE "USR" ARRAY
C                           TO MISSING (10E10) WHEN BUFR FILE IS BEING
C                           READ
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"; IMPROVED MACHINE
C                           PORTABILITY
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
C                           INFO WHEN ROUTINE TERMINATES ABNORMALLY OR
C                           UNUSUAL THINGS HAPPEN; CHANGED CALL FROM
C                           BORT TO BORT2 IN SOME CASES
C 2004-08-18  J. ATOR    -- ADDED SAVE FOR IFIRST1 AND IFIRST2 FLAGS
C 2009-04-21  J. ATOR    -- USE ERRWRT
C
C USAGE:    CALL UFBINT (LUNIN, USR, I1, I2, IRET, STR)
C   INPUT ARGUMENT LIST:
C     LUNIN    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
C                FOR BUFR FILE
C                  - IF BUFR FILE OPEN FOR OUTPUT AND LUNIN IS LESS
C                    THAN ZERO, UFBINT TREATS THE BUFR FILE AS THOUGH
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
C                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
C                    OF DATA VALUES TO BE WRITTEN TO DATA SUBSET
C     STR      - CHARACTER*(*): STRING OF BLANK-SEPARATED TABLE B
C                MNEMONICS IN ONE-TO-ONE CORRESPONDENCE WITH FIRST
C                DIMENSION OF USR
C                  - IF BUFR FILE OPEN FOR INPUT: THIS CAN ALSO BE A
C                    SINGLE TABLE D (SEQUENCE) MNEMONIC WITH EITHER 8-
C                    OR 16-BIT DELAYED REPLICATION (SEE REMARKS 1)
C                  - IF BUFR FILE OPEN FOR INPUT: THERE ARE THREE
C                     "GENERIC" MNEMONICS NOT RELATED TO TABLE B OR D,
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
C    1) UFBINT CAN ALSO BE CALLED TO PROVIDE INFORMATION ABOUT A SINGLE
C       TABLE D (SEQUENCE) MNEMONIC WITH EITHER 8- OR 16-BIT DELAYED
C       REPLICATION IN A SUBSET WHEN THE BUFR FILE IS OPEN FOR INPUT.
C       THE MNEMONIC IN STR MUST APPEAR AS IT DOES IN THE BUFR TABLE,
C       I.E., BRACKETED BY "{" AND "}" OR "[" AND "]" FOR 8-BIT DELAYED
C       REPLICATION, OR BRACKETED BY "(" AND ")" FOR 16-BIT DELAYED
C       REPLICATION.  {NOTE: THIS WILL NOT WORK FOR SEQUENCES WITH
C       1-BIT DELAYED REPLICATION (BRACKETED BY "<" AND ">"), STANDARD
C       REPLICATION (BRACKETED BY "'s), OR NO REPLICATION (NO
C       BRACKETING SYMBOLS).}
C       
C       FOR EXAMPLE:
C
C       CALL UFBINT(LUNIN,PLEVL,1, 50,IRET,'{PLEVL}')
C
C       WILL RETURN WITH IRET EQUAL TO THE NUMBER OF OCCURRENCES OF THE
C       8-BIT DELAYED REPLICATION SEQUENCE PLEVL IN THE SUBSET AND WITH
C       (PLEVL(I),I=1,IRET) EQUAL TO THE NUMBER OF REPLICATIONS IN EACH
C       OCCURRENCE OF PLEVL IN THE SUBSET.  IF THERE ARE NO OCCURRENCES
C       OF PLEVL IN THE SUBSET, IRET IS RETURNED AS ZERO.
C
C    2) WHEN THE BUFR FILE IS OPEN FOR OUTPUT, UFBINT CAN BE USED TO
C       PRE-ALLOCATE SPACE FOR SOME OR ALL MNEMONICS WITHIN DELAYED
C       REPLICATION SEQUENCES.  A SUBSEQUENT CALL TO BUFR ARCHIVE
C       LIBRARY ROUTINE UFBREP OR UFBSEQ THEN ACTUALLY STORES THE
C       VALUES INTO THE BUFR FILES.  HERE ARE TWO EXAMPLES OF THIS:
C
C       EXAMPLE 1) PROBLEM: AN OUTPUT SUBSET "SEQNCE" IS LAID OUT AS
C          FOLLOWS IN A BUFR TABLE (NOTE 16 CHARACTERS HAVE BEEN
C          REMOVED FROM THE LAST COLUMN TO ALLOW THE TABLE TO FIT IN
C          THIS DOCBLOCK):
C
C       | SEQNCE   | {PLEVL}                                           |
C       | PLEVL    | WSPD WDIR TSIG PRLC TSIG PRLC TSIG PRLC           |
C
C              -- OR --
C
C       | SEQNCE   | {PLEVL}                                           |
C       | PLEVL    | WSPD WDIR "PSEQ"3                                 |
C       | PSEQ     | TSIG PRLC                                         |
C
C         IN THIS CASE THE APPLICATION PROGRAM MUST STORE VALUES WHICH
C         HAVE STANDARD REPLICATION NESTED INSIDE OF A DELAYED
C         REPLICATION SEQUENCE. FOR EXAMPLE,  ASSUME 50 LEVELS OF WIND
C         SPEED, WIND DIRECTION, OBSERVED PRESSURE, FIRST GUESS
c         PRESSURE AND ANALYZED PRESSURE ARE TO BE WRITTEN TO "SEQNCE".
C
C       THE FOLLOWING LOGIC WOULD ENCODE VALUES PROPERLY:
C.....................................................................
C              ....
C       REAL*8 DROBS(2,50)
C       REAL*8 SROBS(2,150)
C              ....
C       DO I=1,50
C         DROBS(1,I)     = Value of wind speed on level "I"
C         DROBS(2,I)     = Value of wind direction on level "I"
C         SROBS(1,I*3-2) = Value of observed pressure on level "I"
C         SROBS(2,I*3-2) = 25. ! Value in Code Table 0-08-021 (TSIG)
C                              !  for time sigificance (Nominal
C                              !  reporting time) for observed
C                              !  pressure on level "I"
C         SROBS(1,I*3-1) = Value of first guess pressure on level "I"
C         SROBS(2,I*3-1) = 27. ! Value in Code Table 0-08-021 (TSIG)
C                              !  for time sigificance (First guess)
C                              !  for first guess pressure on level "I"
C         SROBS(1,I*3) = Value of analyzed pressure on level "I"
C         SROBS(2,I*3) = 16.   ! Value in Code Table 0-08-021 (TSIG)
C                              !  for time sigificance (Analysis) for
C                              !  analyzed pressure on level "I"
C       ENDDO
C
C              ! The call to UFBINT here will not only store the 50
C              !  values of WSPD and WDIR into the BUFR subset, it
C              !  will also allocate the space to store three
C              !  replications of TSIG and PRLC on each of the 50
C              !  delayed-replication "levels"
C       CALL UFBINT(LUNIN,DROBS,2, 50,IRET,'WSPD WDIR')
C
C              ! The call to UFBREP here will actually store the 150
C              !  values of both TSIG and PRLC (three values for each
C              !  on 50 delayed-replication "levels")
C       CALL UFBREP(LUNIN,SROBS,2,150,IRET,'TSIG PRLC')
C              ....
C       STOP
C       END
C.....................................................................
C
C       A SIMILAR EXAMPLE COULD BE PROVIDED FOR READING VALUES WHICH
C       HAVE STANDARD REPLICATION NESTED WITHIN DELAYED REPLICATION,
C       FROM BUFR FILES OPEN FOR INPUT.  (NOT SHOWN HERE.)
C
C
C       EXAMPLE 2) PROBLEM: AN INPUT SUBSET, "REPT_IN", AND AN OUTPUT
C          SUBSET "REPT_OUT", ARE LAID OUT AS FOLLOWS IN A BUFR TABLE
C          (NOTE 16 CHARACTERS HAVE BEEN REMOVED FROM THE LAST COLUMN
C          TO ALLOW THE TABLE TO FIT IN THIS DOCBLOCK):
C
C       | REPT_IN  | YEAR MNTH DAYS HOUR MINU {PLEVL} CLAT CLON        |
C       | REPT_OUT | YEAR DOYR HOUR MINU {PLEVL} CLAT CLON             |
C       | PLEVL    | PRLC TMBD REHU WDIR WSPD
C
C         IN THIS CASE THE APPLICATION PROGRAM IS READING IN VALUES
C         FROM A BUFR FILE CONTAINING SUBSET "REPT_IN", CONVERTING
C         MONTH AND DAY TO DAY OF YEAR, AND THEN WRITING VALUES TO
C         SUBSET "REPT_OUT" IN ANOTHER BUFR FILE.  A CONVENIENT WAY TO
C         DO THIS IS TO CALL UFBSEQ TO READ IN AND WRITE OUT THE
C         VALUES, HOWEVER THIS IS COMPLICATED BY THE PRESENCE OF THE
C         DELAYED-RELICATION SEQUENCE "PLEVL" BECAUSE THE OUTPUT CALL
C         TO UFBSEQ DOES NOT KNOW A-PRIORI HOW MANY REPLICATIONS ARE
C         NEEDED TO STORE THE CONTENTS OF "PLEVL" (IT SETS THE NUMBER
C         TO ZERO BY DEFUALT).  A CALL TO UFBINT IS FIRST NEEDED TO
C         ALLOCATE THE SPACE AND DETERMINE THE NUMBER OF LEVELS NEEDED
C         TO STORE ALL VALUES IN "PLEVL".
C
C       THE FOLLOWING LOGIC WOULD PEFORM THE READ/WRITE PROPERLY:
C.....................................................................
C              ....
C       REAL*8 OBSI(2000),OBSO(1999),PLEVL(5,255),REPS_8
C       CHARACTER*8 SUBSET
C              ....
C
C       CALL DATELEN(10)
C
C         ! Open input BUFR file in LUBFI and open output BUFR file in
C         !  LUBFJ, both use the BUFR table in LINDX
C       CALL OPENBF(LUBFI,'IN', LINDX)
C       CALL OPENBF(LUBFJ,'OUT',LINDX)
C
C         ! Read through the BUFR messages in the input file
C       DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).GE.0)
C
C         ! Open message (for writing) in output file
C          CALL OPENMB(LUBFJ,'REPT_OUT',IDATE)
C
C         ! Read through the subsets in this input BUFR messages
C          DO WHILE(IREADSB(LUBFI).EQ.0)
C
C              ! This call to UFBSEQ will read in the entire contents
C              !  of subset "REPT_IN", storing them into array OBSI
C              !  (Note: On input, UFBSEQ knows how many replications
C                        of "PLEV" are present)
C             CALL UFBSEQ(LUBFI,OBSI,2000,1,IRET,'REPT_IN')
C
C              ! This call to UFBINT will return the number of
C              !  replications ("levels") in "PLEVL" for subset
C              !  "REPT_IN"" !  {see 1) above in REMARKS}
C             CALL UFBINT(LUBFI,REPS_8,1,1,IRET,'{PLEVL}')
C             IREPS = REPS_8
C
C             IYR = OBSI(1)
C             IMO = OBSI(2)
C             IDA = OBSI(3)
C             CALL xxxx(IYR, IMO, IDA, JDY) ! convert month and day
C                                           !  to day of year (JDY)
C             OBSO(1) = OBSI(1)
C             OBSO(2) = JDY
C             DO I = 3,1999
C                OBSO(I) = OBSI(1+1)
C             ENDDO
C
C             PLEVL = GETBMISS()
C
C              ! The call to UFBINT here will allocate the space to
C              !  later allow UFBSEQ to store IREPS replications of
C              !  "PLEVL" into the output BUFR subset "REPT_OUT" (note
C              !  here it is simply storing missing values)
C             CALL UFBINT(LUBFJ,PLEVL,5,IREPS,IRET,
C     $        'PRLC TMBD REHU WDIR WSPD')
C
C              ! The call to UFBSEQ here will write out the entire
C              !  contents of subset "REPT_OUT", reading them from
C              !  array OBSO
C             CALL UFBSEQ(LUBFJ,OBSO,1999,1,IRET,'REPT_OUT')
C
C              ! Write the subset into the output BUFR message
C             CALL WRITSB(LUBFJ)
C          ENDDO
C
C              ! All done
C
C          STOP
C          END
C.....................................................................
C
C
C    THIS ROUTINE CALLS:        BORT     BORT2    ERRWRT   STATUS
C                               STRING   TRYBUMP  UFBRW
C    THIS ROUTINE IS CALLED BY: UFBINX   UFBRMS
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES),
     .                INODE(NFILES),IDATE(NFILES)
      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
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

      LUNIT = ABS(LUNIN)
      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IM.EQ.0) GOTO 901
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 902

      IO = MIN(MAX(0,IL),1)
      IF(LUNIT.NE.LUNIN) IO = 0

      IF(I1.LE.0) THEN
         IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBINT - 3rd ARG. (INPUT) IS .LE. 0, ' //
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
      ERRSTR = 'BUFRLIB: UFBINT - 4th ARG. (INPUT) IS .LE. 0, ' //
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

C  PARSE OR RECALL THE INPUT STRING
C  --------------------------------

      CALL STRING(STR,LUN,I1,IO)

C  INITIALIZE USR ARRAY PRECEEDING AN INPUT OPERATION
C  --------------------------------------------------

      IF(IO.EQ.0) THEN
         DO J=1,I2
         DO I=1,I1
         USR(I,J) = BMISS
         ENDDO
         ENDDO
      ENDIF

C  CALL THE MNEMONIC READER/WRITER
C  -------------------------------

      CALL UFBRW(LUN,USR,I1,I2,IO,IRET)

C  IF INCOMPLETE WRITE TRY TO INITIALIZE REPLICATION SEQUENCE OR RETURN
C  ---------------------------------------------------------------------

      IF(IO.EQ.1 .AND. IRET.NE.I2 .AND. IRET.GE.0) THEN
         CALL TRYBUMP(LUNIT,LUN,USR,I1,I2,IO,IRET)
         IF(IRET.NE.I2) GOTO 903
      ELSEIF(IRET.EQ.-1) THEN
         IRET = 0
      ENDIF

      IF(IRET.EQ.0)  THEN
         IF(IO.EQ.0) THEN
            IF(IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: UFBINT - NO SPECIFIED VALUES READ IN, ' //
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
      ERRSTR = 'BUFRLIB: UFBINT - NO SPECIFIED VALUES WRITTEN OUT, ' //
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
900   CALL BORT('BUFRLIB: UFBINT - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   CALL BORT('BUFRLIB: UFBINT - A MESSAGE MUST BE OPEN IN BUFR '//
     . 'FILE, NONE ARE')
902   CALL BORT('BUFRLIB: UFBINT - LOCATION OF INTERNAL TABLE FOR '//
     . 'BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN INTERNAL '//
     . 'SUBSET ARRAY')
903   WRITE(BORT_STR1,'("BUFRLIB: UFBINT - MNEMONIC STRING READ IN IS'//
     . ': ",A)') STR
      WRITE(BORT_STR2,'(18X,"THE NUMBER OF ''LEVELS'' ACTUALLY '//
     . 'WRITTEN (",I3,") DOES NOT EQUAL THE NUMBER REQUESTED (",I3,")'//
     . ' - INCOMPLETE WRITE")')  IRET,I2
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
