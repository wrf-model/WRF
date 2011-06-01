      SUBROUTINE PKTDD(ID,LUN,IDN,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    PKTDD
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE STORES INFORMATION ABOUT A "CHILD"
C   MNEMONIC WITHIN THE INTERNAL BUFR TABLE D ENTRY (IN COMMON BLOCK
C   /TABABD/) FOR A TABLE D SEQUENCE ("PARENT") MNEMONIC WHEN THE
C   "CHILD" MNEMONIC IS CONTAINED WITHIN THE SEQUENCE REPRESENTED BY
C   THE "PARENT" MNEMONIC (AS DETERMINED WITHIN BUFR ARCHIVE LIBRARY
C   SUBROUTINE SEQSDX).
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; ADDED MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN UNUSUAL THINGS HAPPEN
C
C USAGE:    CALL PKTDD (ID, LUN, IDN, IRET)
C   INPUT ARGUMENT LIST:
C     ID       - INTEGER: POSITIONAL INDEX OF PARENT MNEMONIC WITHIN
C                INTERNAL BUFR TABLE D ARRAY TABD(*,*)
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     IDN      - INTEGER: BIT-WISE REPRESENTATION OF FXY VALUE
C                CORRESPONDING TO CHILD MNEMONIC
C                       0 = delete all information about all child
C                           mnemonics from within TABD(ID,LUN)
C
C   OUTPUT ARGUMENT LIST:
C     IRET     - INTEGER: TOTAL NUMBER OF CHILD MNEMONICS STORED THUS
C                FAR (INCLUDING IDN) FOR THE PARENT MNEMONIC GIVEN BY
C                TABD(ID,LUN)
C                       0 = information was cleared from TABD(ID,LUN)
C                           because input IDN value was 0
C                      -1 = bad counter value or maximum number of
C                           child mnemonics already stored for this
C                           parent mnemonic
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C
C REMARKS:
C    THIS ROUTINE CALLS:        IPKM     IUPM
C    THIS ROUTINE IS CALLED BY: DXINIT   RDBFDX   SEQSDX
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
      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)
      COMMON /QUIET / IPRT

      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*56  DXSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LDD = LDXD(IDXV+1)+1

C     LDD points to the byte within TABD(ID,LUN) which contains (in
C     packed integer format) a count of the number of child mnemonics
C     stored thus far for this parent mnemonic.

C  ZERO THE COUNTER IF IDN IS ZERO
C  -------------------------------

      IF(IDN.EQ.0) THEN
         CALL IPKM(TABD(ID,LUN)(LDD:LDD),1,0)
         IRET = 0
         GOTO 100
      ENDIF

C  UPDATE THE STORED DESCRIPTOR COUNT FOR THIS TABLE D ENTRY
C  ---------------------------------------------------------

      ND = IUPM(TABD(ID,LUN)(LDD:LDD),8)

C     ND is the (unpacked) count of the number of child mnemonics
C     stored thus far for this parent mnemonic.

      IF(ND.LT.0 .OR. ND.EQ.MAXCD) THEN
      IF(IPRT.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
         IF(ND.LT.0) THEN
            PRINT*, 'BUFRLIB: PKTDD - BAD COUNTER VALUE (=',ND,
     .       ') - RETURN WITH IRET = -1'
         ELSE
            PRINT*, 'BUFRLIB: PKTDD - MAXIMUM NUMBER OF CHILD ',
     .       'MNEMONICS (MAXCD) ALREADY STORED FOR THIS PARENT ',
     .       'MNEMONIC - RETURN WITH IRET = -1'
         ENDIF
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
      ENDIF
         IRET = -1
         GOTO 100
      ELSE
         ND = ND+1
         CALL IPKM(TABD(ID,LUN)(LDD:LDD),1,ND)
         IRET = ND
      ENDIF

C  PACK AND STORE THE DESCRIPTOR
C  -----------------------------

      IDM = LDD+1 + (ND-1)*2

C     IDM points to the starting byte within TABD(ID,LUN) at which
C     the IDN value for this child mnemonic will be stored (as a
C     packed integer of width = 2 bytes).

      CALL IPKM(TABD(ID,LUN)(IDM:IDM),2,IDN)

C  EXIT
C  ----

100   RETURN
      END
