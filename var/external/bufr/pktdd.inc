      SUBROUTINE PKTDD(ID,LUN,IDN,IRET)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    PKTDD
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE STORES INFORMATION ABOUT A "CHILD"
!   MNEMONIC WITHIN THE INTERNAL BUFR TABLE D ENTRY (IN COMMON BLOCK
!   /TABABD/) FOR A TABLE D SEQUENCE ("PARENT") MNEMONIC WHEN THE
!   "CHILD" MNEMONIC IS CONTAINED WITHIN THE SEQUENCE REPRESENTED BY
!   THE "PARENT" MNEMONIC (AS DETERMINED WITHIN BUFR ARCHIVE LIBRARY
!   SUBROUTINE SEQSDX).
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION; ADDED MORE COMPLETE
!                           DIAGNOSTIC INFO WHEN UNUSUAL THINGS HAPPEN
!
! USAGE:    CALL PKTDD (ID, LUN, IDN, IRET)
!   INPUT ARGUMENT LIST:
!     ID       - INTEGER: POSITIONAL INDEX OF PARENT MNEMONIC WITHIN
!                INTERNAL BUFR TABLE D ARRAY TABD(*,*)
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!     IDN      - INTEGER: BIT-WISE REPRESENTATION OF FXY VALUE
!                CORRESPONDING TO CHILD MNEMONIC
!                       0 = delete all information about all child
!                           mnemonics from within TABD(ID,LUN)
!
!   OUTPUT ARGUMENT LIST:
!     IRET     - INTEGER: TOTAL NUMBER OF CHILD MNEMONICS STORED THUS
!                FAR (INCLUDING IDN) FOR THE PARENT MNEMONIC GIVEN BY
!                TABD(ID,LUN)
!                       0 = information was cleared from TABD(ID,LUN)
!                           because input IDN value was 0
!                      -1 = bad counter value or maximum number of
!                           child mnemonics already stored for this
!                           parent mnemonic
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!
! REMARKS:
!    THIS ROUTINE CALLS:        IPKM     IUPM
!    THIS ROUTINE IS CALLED BY: DXINIT   RDBFDX   SEQSDX
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
      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10), &
                      LD30(10),DXSTR(10)
      COMMON /QUIET / IPRT

      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*56  DXSTR

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      LDD = LDXD(IDXV+1)+1

!     LDD points to the byte within TABD(ID,LUN) which contains (in
!     packed integer format) a count of the number of child mnemonics
!     stored thus far for this parent mnemonic.

!  ZERO THE COUNTER IF IDN IS ZERO
!  -------------------------------

      IF(IDN.EQ.0) THEN
         CALL IPKM(TABD(ID,LUN)(LDD:LDD),1,0)
         IRET = 0
         GOTO 100
      ENDIF

!  UPDATE THE STORED DESCRIPTOR COUNT FOR THIS TABLE D ENTRY
!  ---------------------------------------------------------

      ND = IUPM(TABD(ID,LUN)(LDD:LDD),8)

!     ND is the (unpacked) count of the number of child mnemonics
!     stored thus far for this parent mnemonic.

      IF(ND.LT.0 .OR. ND.EQ.MAXCD) THEN
      IF(IPRT.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
         IF(ND.LT.0) THEN
            PRINT*, 'BUFRLIB: PKTDD - BAD COUNTER VALUE (=',ND, &
             ') - RETURN WITH IRET = -1'
         ELSE
            PRINT*, 'BUFRLIB: PKTDD - MAXIMUM NUMBER OF CHILD ', &
             'MNEMONICS (MAXCD) ALREADY STORED FOR THIS PARENT ', &
             'MNEMONIC - RETURN WITH IRET = -1'
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

!  PACK AND STORE THE DESCRIPTOR
!  -----------------------------

      IDM = LDD+1 + (ND-1)*2

!     IDM points to the starting byte within TABD(ID,LUN) at which
!     the IDN value for this child mnemonic will be stored (as a
!     packed integer of width = 2 bytes).

      CALL IPKM(TABD(ID,LUN)(IDM:IDM),2,IDN)

!  EXIT
!  ----

100   RETURN
      END SUBROUTINE PKTDD
