      SUBROUTINE USRTPL(LUN,INVN,NBMP)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    USRTPL
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE STORES THE SUBSET TEMPLATE INTO INTERNAL
C   SUBSET ARRAYS IN COMMON BLOCK /USRINT/ FOR CASES OF NODE EXPANSION
C   (I.E. WHEN THE NODE IS EITHER A TABLE A MNEMONIC OR A DELAYED
C   REPLICATION FACTOR).
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
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
C                           HISTORY) (INCOMPLETE); OUTPUTS MORE
C                           COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY OR UNUSUAL THINGS
C                           HAPPEN; COMMENTED OUT HARDWIRE OF VTMP TO
C                           "BMISS" (10E10) WHEN IT IS > 10E9 (CAUSED
C                           PROBLEMS ON SOME FOREIGN MACHINES)
C 2009-03-31  J. WOOLLEN -- ADD DOCUMENTATION
C 2009-04-21  J. ATOR    -- USE ERRWRT
C
C USAGE:    CALL USRTPL (LUN, INVN, NBMP)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     INVN     - INTEGER: STARTING JUMP/LINK TABLE INDEX OF THE NODE
C                TO BE EXPANDED WITHIN THE SUBSET TEMPLATE
C     NBMP     - INTEGER: NUMBER OF TIMES BY WHICH INVN IS TO BE
C                EXPANDED (I.E. NUMBER OF REPLICATIONS OF NODE)
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     ERRWRT
C    THIS ROUTINE IS CALLED BY: DRFINI   DRSTPL   MSGUPD   OPENMB
C                               OPENMG   RDCMPS   TRYBUMP  UFBGET
C                               UFBTAB   UFBTAM   WRCMPS   WRITLC
C                               Normally not called by any application
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
      COMMON /BTABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /USRINT/ NVAL(NFILES),INV(MAXSS,NFILES),VAL(MAXSS,NFILES)
      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*10  TAG
      CHARACTER*3   TYP
      DIMENSION     ITMP(MAXJL)
      LOGICAL       DRP,DRS,DRB,DRX
      REAL*8        VAL,VTMP(MAXJL)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IF(IPRT.GE.2)  THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
         WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,I5,A,I5,A,A10)' )
     .      'BUFRLIB: USRTPL - LUN:INVN:NBMP:TAG(INODE(LUN)) = ',
     .      LUN, ':', INVN, ':', NBMP, ':', TAG(INODE(LUN))
         CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

      IF(NBMP.LE.0) THEN
         IF(IPRT.GE.1)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT('BUFRLIB: USRTPL - NBMP .LE. 0 - IMMEDIATE RETURN')
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         GOTO 100
      ENDIF

      DRP = .FALSE.
      DRS = .FALSE.
      DRX = .FALSE.

C  SET UP A NODE EXPANSION
C  -----------------------

      IF(INVN.EQ.1) THEN
c  .... case where node is a Table A mnemonic (nodi is positional index)
         NODI = INODE(LUN)
         INV(1,LUN) = NODI
         NVAL(LUN)  = 1
         IF(NBMP.NE.1) GOTO 900
      ELSEIF(INVN.GT.0 .AND. INVN.LE.NVAL(LUN)) THEN
c  .... case where node is (hopefully) a delayed replication factor
         NODI = INV(INVN,LUN)
         DRP  = TYP(NODI) .EQ. 'DRP'
         DRS  = TYP(NODI) .EQ. 'DRS'
         DRB  = TYP(NODI) .EQ. 'DRB'
         DRX  = DRP .OR. DRS .OR. DRB
         IVAL = VAL(INVN,LUN)
         JVAL = 2**IBT(NODI)-1
         VAL(INVN,LUN) = IVAL+NBMP
         IF(DRB.AND.NBMP.NE.1) GOTO 901
         IF(.NOT.DRX         ) GOTO 902
         IF(IVAL.LT.0.       ) GOTO 903
         IF(IVAL+NBMP.GT.JVAL) GOTO 904
      ELSE
         GOTO 905
      ENDIF

C  RECALL A PRE-FAB NODE EXPANSION SEGMENT
C  ---------------------------------------

      NEWN = 0
      N1 = ISEQ(NODI,1)
      N2 = ISEQ(NODI,2)

      IF(N1.EQ.0          ) GOTO 906
      IF(N2-N1+1.GT.MAXJL)  GOTO 907

      DO N=N1,N2
      NEWN = NEWN+1
      ITMP(NEWN) = JSEQ(N)
      VTMP(NEWN) = VALI(JSEQ(N))
      ENDDO

C  MOVE OLD NODES - STORE NEW ONES
C  -------------------------------

      IF(NVAL(LUN)+NEWN*NBMP.GT.MAXSS) GOTO 908

      DO J=NVAL(LUN),INVN+1,-1
      INV(J+NEWN*NBMP,LUN) = INV(J,LUN)
      VAL(J+NEWN*NBMP,LUN) = VAL(J,LUN)
      ENDDO

      IF(DRP.OR.DRS) VTMP(1) = NEWN
      KNVN = INVN

      DO I=1,NBMP
      DO J=1,NEWN
      KNVN = KNVN+1
      INV(KNVN,LUN) = ITMP(J)
      VAL(KNVN,LUN) = VTMP(J)
      ENDDO
      ENDDO

C  RESET POINTERS AND COUNTERS
C  ---------------------------

      NVAL(LUN) = NVAL(LUN) + NEWN*NBMP

      IF(IPRT.GE.2)  THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
         WRITE ( UNIT=ERRSTR, FMT='(A,A,A10,3(A,I5))' )
     .      'BUFRLIB: USRTPL - TAG(INV(INVN,LUN)):NEWN:NBMP:',
     .      'NVAL(LUN) = ', TAG(INV(INVN,LUN)), ':', NEWN, ':',
     .      NBMP, ':', NVAL(LUN)
         CALL ERRWRT(ERRSTR)
         DO I=1,NEWN
           WRITE ( UNIT=ERRSTR, FMT='(2(A,I5),A,A10)' )
     .      'For I = ', I, ', ITMP(I) = ', ITMP(I),
     .      ', TAG(ITMP(I)) = ', TAG(ITMP(I))
           CALL ERRWRT(ERRSTR)
         ENDDO
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

      IF(DRX) THEN
         NODE = NODI
         INVR = INVN
4        NODE = JMPB(NODE)
         IF(NODE.GT.0) THEN
            IF(ITP(NODE).EQ.0) THEN
               DO INVR=INVR-1,1,-1
               IF(INV(INVR,LUN).EQ.NODE) THEN
                  VAL(INVR,LUN) = VAL(INVR,LUN)+NEWN*NBMP
                  GOTO 4
               ENDIF
               ENDDO
               GOTO 909
            ELSE
               GOTO 4
            ENDIF
         ENDIF
      ENDIF

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: USRTPL - THIRD ARGUMENT (INPUT) = ",'//
     . 'I4,", MUST BE 1 WHEN SECOND ARGUMENT (INPUT) IS 1 (SUBSET '//
     . 'NODE) (",A,")")') NBMP,TAG(NODI)
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: USRTPL - THIRD ARGUMENT (INPUT) = ",'//
     . 'I4,", MUST BE 1 WHEN NODE IS DRB (1-BIT DELAYED REPL. FACTOR)'//
     . ' (",A,")")') NBMP,TAG(NODI)
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: USRTPL - NODE IS OF TYPE ",A," - IT '//
     . 'MUST BE EITHER A SUBSET OR DELAYED REPL. FACTOR (",A,")")')
     .  TYP(NODI),TAG(NODI)
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: USRTPL - REPLICATION FACTOR IS '//
     . 'NEGATIVE (=",I5,") (",A,")")') IVAL,TAG(NODI)
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: USRTPL - REPLICATION FACTOR OVERFLOW'//
     . ' (EXCEEDS MAXIMUM OF",I6," (",A,")")') JVAL,TAG(NODI)
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: USRTPL - INVENTORY INDEX {FIRST '//
     . 'ARGUMENT (INPUT)} OUT OF BOUNDS (=",I5,", RANGE IS 1 TO",I6,"'//
     . ') (",A,")")') INVN,NVAL(LUN),TAG(NODI)
      CALL BORT(BORT_STR)
906   WRITE(BORT_STR,'("BUFRLIB: USRTPL - UNSET EXPANSION SEGMENT (",'//
     . 'A,")")') TAG(NODI)
      CALL BORT(BORT_STR)
907   WRITE(BORT_STR,'("BUFRLIB: USRTPL - TEMPLATE ARRAY OVERFLOW, '//
     . 'EXCEEDS THE LIMIT (",I6,") (",A,")")') MAXJL,TAG(NODI)
      CALL BORT(BORT_STR)
908   WRITE(BORT_STR,'("BUFRLIB: USRTPL - INVENTORY OVERFLOW (",I6,")'//
     . ', EXCEEDS THE LIMIT (",I6,") (",A,")")')
     . NVAL(LUN)+NEWN*NBMP,MAXSS,TAG(NODI)
      CALL BORT(BORT_STR)
909   WRITE(BORT_STR,'("BUFRLIB: USRTPL - BAD BACKUP STRATEGY (",A,'//
     . '")")') TAG(NODI)
      CALL BORT(BORT_STR)
      END
