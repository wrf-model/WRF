      SUBROUTINE MSGUPD(LUNIT,LUN)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    MSGUPD
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE PACKS UP THE CURRENT SUBSET WITHIN MEMORY
!  (ARRAY IBAY IN COMMON BLOCK /BITBUF/) AND THEN TRIES TO ADD IT TO
!  THE BUFR MESSAGE THAT IS CURRENTLY OPEN WITHIN MEMORY FOR LUNIT
!  (ARRAY MBAY IN COMMON BLOCK /BITBUF/).  IF THE SUBSET WILL NOT FIT
!  INTO THE CURRENTLY OPEN MESSAGE, THEN THAT MESSAGE IS FLUSHED TO
!  LUNIT AND A NEW ONE IS CREATED IN ORDER TO HOLD THE CURRENT SUBSET.
!  IF THE SUBSET IS LARGER THAN AN EMPTY MESSAGE, THE SUBSET IS
!  DISCARDED AND A DIAGNOSTIC IS PRINTED.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"
! 1998-12-14  J. WOOLLEN -- NO LONGER CALLS BORT IF A SUBSET IS LARGER
!                           THAN A MESSAGE, JUST DISCARDS THE SUBSET
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           10,000 TO 20,000 BYTES
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           20,000 TO 50,000 BYTES
!
! USAGE:    CALL MSGUPD (LUNIT, LUN)
!   INPUT ARGUMENT LIST:
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT)
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!
! REMARKS:
!    THIS ROUTINE CALLS:        IUPB     MSGINI   MSGWRT   MVB
!                               PAD      PKB      USRTPL
!    THIS ROUTINE IS CALLED BY: WRITSA   WRITSB
!                               Normally not called by any application
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

      COMMON /MSGPTR/ NBY0,NBY1,NBY2,NBY3,NBY4,NBY5
      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES), &
                      INODE(NFILES),IDATE(NFILES)
      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGLD4),MBYT(NFILES), &
                      MBAY(MXMSGLD4,NFILES)
      COMMON /QUIET / IPRT

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!  PAD THE SUBSET BUFFER
!  ---------------------

      CALL PAD(IBAY,IBIT,IBYT,8)

!  SEE IF THE NEW SUBSET FITS
!  --------------------------

      IF(MBYT(LUN)+IBYT+8.GT.MAXBYT) THEN
!  .... NO it does not fit
         CALL MSGWRT(LUNIT,MBAY(1,LUN),MBYT(LUN))
         CALL MSGINI(LUN)
      ENDIF

      IF(MBYT(LUN)+IBYT+8.GT.MAXBYT) GOTO 900

!  SET A BYTE COUNT AND TRANSFER THE SUBSET BUFFER INTO THE MESSAGE
!  ----------------------------------------------------------------

      LBIT = 0
      CALL PKB(IBYT,16,IBAY,LBIT)

!     Note that we want to append the data for this subset to the end
!     of Section 4, but the value in MBYT(LUN) already includes the
!     length of Section 5 (i.e. 4 bytes).  Therefore, we need to begin
!     writing at the point 3 bytes prior to the byte currently pointed
!     to by MBYT(LUN).

      CALL MVB(IBAY,1,MBAY(1,LUN),MBYT(LUN)-3,IBYT)

!  UPDATE THE SUBSET AND BYTE COUNTERS
!  --------------------------------------

      MBYT(LUN)   = MBYT(LUN)   + IBYT
      NSUB(LUN)   = NSUB(LUN)   + 1

      LBIT = (NBY0+NBY1+NBY2+4)*8
      CALL PKB(NSUB(LUN),16,MBAY(1,LUN),LBIT)

      LBYT = NBY0+NBY1+NBY2+NBY3
      NBYT = IUPB(MBAY(1,LUN),LBYT+1,24)
      LBIT = LBYT*8
      CALL PKB(NBYT+IBYT,24,MBAY(1,LUN),LBIT)

!  RESET THE USER ARRAYS AND EXIT NORMALLY
!  ---------------------------------------

      CALL USRTPL(LUN,1,1)
      GOTO 100

!  ON ENCOUTERING OVERLARGE SUBSETS, EXIT GRACEFULLY (SUBSET DISCARDED)
!  --------------------------------------------------------------------

900   IF(IPRT.GE.0) THEN
      PRINT*
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*,'BUFRLIB: MSGUPD - SUBSET LONGER THAN ANY POSSIBLE ', &
       'MESSAGE {SUBSET LENGTH= ',MBYT(LUN)+IBYT+8,', MESSAGE LENGTH= ', &
       MAXBYT,'}'
      PRINT*,'>>>>>>>OVERLARGE SUBSET DISCARDED FROM FILE<<<<<<<<'
      PRINT*,'+++++++++++++++++++++++WARNING+++++++++++++++++++++++++'
      PRINT*
      ENDIF

!  EXIT
!  ----

100   RETURN
      END SUBROUTINE MSGUPD
