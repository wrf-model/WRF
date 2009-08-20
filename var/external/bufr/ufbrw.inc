      SUBROUTINE UFBRW(LUN,USR,I1,I2,IO,IRET)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    UFBRW (docblock incomplete)
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE WRITES OR READS SPECIFIED VALUES TO OR FROM
!   THE CURRENT BUFR DATA SUBSET WITHIN INTERNAL ARRAYS, WITH THE
!   DIRECTION OF THE DATA TRANSFER DETERMINED BY THE CONTEXT OF IO
!   (I.E., IF IO INDICATES LUN POINTS TO A BUFR FILE THAT IS OPEN FOR
!   INPUT, THEN DATA VALUES ARE READ FROM THE INTERNAL DATA SUBSET;
!   OTHERWISE, DATA VALUES ARE WRITTEN TO THE INTERNAL DATA SUBSET.
!   THE DATA VALUES CORRESPOND TO INTERNAL ARRAYS REPRESENTING PARSED
!   STRINGS OF MNEMONICS WHICH ARE PART OF A DELAYED-REPLICATION
!   SEQUENCE, OR FOR WHICH THERE IS NO REPLICATION AT ALL.  THIS
!   ROUTINE SHOULD NEVER BE CALLED BY ANY APPLICATION PROGRAM
!   (APPLICATION PROGRAMS SHOULD ALWAYS CALL BUFR ARCHIVE LIBRARY
!   SUBROUTINE UFBINT TO PERFORM THESE FUNCTIONS).
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1996-12-11  J. WOOLLEN -- REMOVED A HARD ABORT FOR USERS WHO TRY TO
!                           WRITE NON-EXISTING MNEMONICS
! 1998-07-08  J. WOOLLEN -- IMPROVED MACHINE PORTABILITY
! 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-
!                           LINING CODE WITH FPP DIRECTIVES
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
!                           HISTORY) (INCOMPLETE)
! 2007-01-19  J. ATOR    -- USE FUNCTION IBFMS
!
! USAGE:    CALL UFBRW (LUN, USR, I1, I2, IO, IRET)
!   INPUT ARGUMENT LIST:
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!     USR      - ONLY IF BUFR FILE OPEN FOR OUTPUT:
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
!                   WRITTEN TO DATA SUBSET
!     I1       - INTEGER: LENGTH OF FIRST DIMENSION OF USR
!     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF USR
!     IO       - INTEGER: STATUS INDICATOR FOR BUFR FILE ASSOCIATED
!                WITH LUN:
!                       0 = input file
!                       1 = output file
!
!   OUTPUT ARGUMENT LIST:
!     USR      - ONLY IF BUFR FILE OPEN FOR INPUT:
!                   REAL*8: (I1,I2) STARTING ADDRESS OF DATA VALUES
!                   READ FROM DATA SUBSET
!     IRET     - INTEGER:
!                  - IF BUFR FILE OPEN FOR INPUT: NUMBER OF "LEVELS" OF
!                    DATA VALUES READ FROM DATA SUBSET (MUST BE NO
!                    LARGER THAN I2)
!                      -1 = ....
!                  - IF BUFR FILE OPEN FOR OUTPUT: NUMBER OF "LEVELS"
!                    OF DATA VALUES WRITTEN TO DATA SUBSET (SHOULD BE
!                    SAME AS I2)
!                      -1 = ....
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!
! REMARKS:
!    THIS ROUTINE CALLS:        CONWIN   DRSTPL   GETWIN   IBFMS
!                               INVWIN   LSTRPS   NEWWIN   NXTWIN
!    THIS ROUTINE IS CALLED BY: TRYBUMP  UFBINT
!                               Normally not called by any application
!                               programs (they should call UFBINT).
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL), &
                      JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL), &
                      IBT(MAXJL),IRF(MAXJL),ISC(MAXJL), &
                      ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL), &
                      ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)
      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
      COMMON /QUIET / IPRT

      CHARACTER*10 TAG
      CHARACTER*3  TYP
      REAL*8       USR(I1,I2),VAL

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      IRET = 0

!  LOOP OVER COND WINDOWS
!  ----------------------

      INC1 = 1
      INC2 = 1

1     CALL CONWIN(LUN,INC1,INC2,I2)
      IF(NNOD.EQ.0) THEN
         IRET = I2
         GOTO 100
      ELSEIF(INC1.EQ.0) THEN
         GOTO 100
      ELSE
         DO I=1,NNOD
         IF(NODS(I).GT.0) THEN
            INS2 = INC1
            CALL GETWIN(NODS(I),LUN,INS1,INS2)
            IF(INS1.EQ.0) GOTO 100
            GOTO 2
         ENDIF
         ENDDO
         IRET = -1
         GOTO 100
      ENDIF

!  LOOP OVER STORE NODES
!  ---------------------

2     IRET = IRET+1

      IF(IPRT.GE.2)  THEN
      PRINT*
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
         PRINT*,'BUFRLIB: UFBRW -  IRET:INS1:INS2:INC1:INC2 = ',IRET, &
          ':',INS1,':',INS2,':',INC1,':',INC2
         PRINT'(5A10)',(TAG(INV(I,LUN)),I=INS1,INS2)
      PRINT*,'+++++++++++++++++BUFR ARCHIVE LIBRARY++++++++++++++++++++'
      PRINT*
      ENDIF

!  WRITE USER VALUES
!  -----------------

      IF(IO.EQ.1 .AND. IRET.LE.I2) THEN
         DO I=1,NNOD
         IF(NODS(I).GT.0) THEN
            IF(IBFMS(USR(I,IRET)).EQ.0) THEN
               INVN = INVWIN(NODS(I),LUN,INS1,INS2)
               IF(INVN.EQ.0) THEN
                  CALL DRSTPL(NODS(I),LUN,INS1,INS2,INVN)
                  IF(INVN.EQ.0) THEN
                     IRET = 0
                     GOTO 100
                  ENDIF
                  CALL NEWWIN(LUN,INC1,INC2)
                  VAL(INVN,LUN) = USR(I,IRET)
               ELSEIF(LSTRPS(NODS(I),LUN).EQ.0) THEN
                  VAL(INVN,LUN) = USR(I,IRET)
               ELSEIF(IBFMS(VAL(INVN,LUN)).NE.0) THEN
                  VAL(INVN,LUN) = USR(I,IRET)
               ELSE
                  CALL DRSTPL(NODS(I),LUN,INS1,INS2,INVN)
                  IF(INVN.EQ.0) THEN
                     IRET = 0
                     GOTO 100
                  ENDIF
                  CALL NEWWIN(LUN,INC1,INC2)
                  VAL(INVN,LUN) = USR(I,IRET)
               ENDIF
            ENDIF
         ENDIF
         ENDDO
      ENDIF

!  READ USER VALUES
!  ----------------

      IF(IO.EQ.0 .AND. IRET.LE.I2) THEN
         DO I=1,NNOD
         USR(I,IRET) = BMISS
         IF(NODS(I).GT.0) THEN
            INVN = INVWIN(NODS(I),LUN,INS1,INS2)
            IF(INVN.GT.0) USR(I,IRET) = VAL(INVN,LUN)
         ENDIF
         ENDDO
      ENDIF

!  DECIDE WHAT TO DO NEXT
!  ----------------------

      IF(IO.EQ.1.AND.IRET.EQ.I2) GOTO 100
      CALL NXTWIN(LUN,INS1,INS2)
      IF(INS1.GT.0 .AND. INS1.LT.INC2) GOTO 2
      IF(NCON.GT.0) GOTO 1

!  EXIT
!  ----

100   RETURN
      END SUBROUTINE UFBRW
