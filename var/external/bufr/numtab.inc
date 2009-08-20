      SUBROUTINE NUMTAB(LUN,IDN,NEMO,TAB,IRET)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    NUMTAB
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE FIRST SEARCHES FOR AN INTEGER IDN,
!   CONTAINING THE BIT-WISE REPRESENTATION OF A DESCRIPTOR (FXY) VALUE,
!   WITHIN THE INTERNAL BUFR REPLICATION ARRAYS IN COMMON BLOCK
!   /REPTAB/ TO SEE IF IDN IS A REPLICATION DESCRIPTOR OR A REPLICATION
!   FACTOR DESCRIPTOR.  IF THIS SEARCH IS UNSUCCESSFUL, IT SEACHES FOR
!   IDN WITHIN THE INTERNAL BUFR TABLE D AND B ARRAYS IN COMMON BLOCK
!   /TABABD/ TO SEE IF IDN IS A TABLE D OR TABLE B DESCRIPTOR.  IF THIS
!   SEARCH IS ALSO UNSUCCESSFUL, IT SEARCHES TO SEE IF IDN IS A TABLE C
!   OPERATOR DESCRIPTOR.  IF IDN IS FOUND IN ANY OF THESE SEARCHES,
!   THIS SUBROUTINE RETURNS THE CORRESPONDING MNEMONIC AND OTHER
!   INFORMATION FROM WITHIN EITHER THE INTERNAL ARRAYS FOR REPLICATION,
!   REPLICATION FACTOR, TABLE D OR TABLE B DESCRIPTORS; OR FROM THE
!   KNOWN VALUES FOR TABLE C DESCRIPTORS.  IF IDN IS NOT FOUND, IT
!   RETURNS WITH IRET=0.  THIS SUBROUTINE IS IDENTICAL TO BUFR ARCHIVE
!   LIBRARY SUBROUTINE NUMTBD EXCEPT NUMTBD SEARCHS FOR IDN WITHIN ONLY
!   THE INTERNAL TABLE D AND B ARRAYS.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2000-09-19  J. WOOLLEN -- ADDED CAPABILITY TO ENCODE AND DECODE DATA
!                           USING THE OPERATOR DESCRIPTORS (BUFR TABLE
!                           C) FOR CHANGING WIDTH AND CHANGING SCALE
! 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
!                           DOCUMENTATION; CORRECTED TYPO ("IDN" WAS
!                           SPECIFIED AS "ID" IN CALCULATION OF IRET
!                           FOR TAB='C')
! 2005-11-29  J. ATOR    -- ADDED SUPPORT FOR 207 AND 208 OPERATORS
!
! USAGE:    CALL NUMTAB (LUN, IDN, NEMO, TAB, IRET)
!   INPUT ARGUMENT LIST:
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!     IDN      - INTEGER: BIT-WISE REPRESENTATION OF DESCRIPTOR (FXY)
!                VALUE
!
!   OUTPUT ARGUMENT LIST:
!     NEMO     - CHARACTER*(*): MNEMONIC CORRESPONDING TO IDN
!     TAB      - CHARACTER*1: TYPE OF FXY VALUE THAT IS BIT-WISE
!                REPRESENTED BY IDN:
!                     'B' = BUFR Table B descriptor
!                     'C' = BUFR Table C descriptor
!                     'D' = BUFR Table D descriptor
!                     'R' = BUFR replication descriptor
!                     'F' = BUFR replication factor descriptor
!     IRET     - INTEGER: RETURN VALUE (SEE REMARKS)
!
!
! REMARKS:
!    THE INTERPRETATION OF THE RETURN VALUE IRET DEPENDS UPON THE
!    RETURN VALUE OF TAB AND THE INPUT VALUE IDN, AS FOLLOWS:
!
!    IF ( TAB = 'B' ) THEN
!       IRET = positional index of IDN within internal BUFR Table B
!              array
!    ELSE IF ( TAB = 'C') THEN
!    IRET = the X portion of the FXY value that is bit-wise represented
!           by IDN
!    ELSE IF ( TAB = 'D') THEN
!       IRET = positional index of IDN within internal BUFR Table D
!              array
!    ELSE IF ( TAB = 'R') THEN
!       IF ( the F portion of the FXY value that is bit-wise
!            represented by IDN ) = 1 THEN
!          ---> regular (non-delayed) replication
!          IRET = ( -1 * ( the Y portion of the FXY value that is bit-
!                        wise represented by IDN ) )
!          ---> where Y = the number of F=1 regular replications
!       ELSE
!          ---> delayed replication
!          IRET = positional index, I, of IDN within internal
!                 replication array IDNR(I,1), where I=2,5
!          ---> IRET = I = 2 --> 16-bit delayed replication descriptor
!          ---> IRET = I = 3 -->  8-bit delayed replication descriptor
!          ---> IRET = I = 4 -->  8-bit delayed replication descriptor
!                                (stack)
!          ---> IRET = I = 5 -->  1-bit delayed replication descriptor
!       END IF
!    ELSE IF ( TAB = 'F') THEN
!       IRET = positional index, I, of IDN within internal replication
!              array IDNR(I,2), where I=2,5
!       ---> IRET = I = 2 --> 16-bit replication factor descriptor
!       ---> IRET = I = 3 -->  8-bit replication factor descriptor
!       ---> IRET = I = 4 -->  8-bit replication factor descriptor
!                              (stack)
!       ---> IRET = I = 5 -->  1-bit replication factor descriptor
!    ELSE IF ( IRET = 0 ) THEN
!       IDN was not found in internal BUFR Table B or D, nor does it
!       represent a Table C operator descriptor, a replication
!       descriptor, or a replication factor descriptor
!    END IF
!
!
!    THIS ROUTINE CALLS:        ADN30
!    THIS ROUTINE IS CALLED BY: CKTABA   NEMTBD   SEQSDX   STNDRD
!                               UFBQCP
!                               Normally not called by any application
!                               programs.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE:  PORTABLE TO ALL PLATFORMS
!
!$$$

      INCLUDE 'bufrlib.prm'

!     Note that the values within the COMMON /REPTAB/ arrays were
!     initialized within subroutine BFRINI.

      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)

      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES), &
                      MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2), &
                      IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES), &
                      TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES), &
                      TABD(MAXTBD,NFILES)

      CHARACTER*(*) NEMO
      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*6   ADN30,CID
      CHARACTER*3   TYPS
      CHARACTER*1   REPS,TAB

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      NEMO = ' '
      IRET = 0
      TAB = ' '

!  LOOK FOR A REPLICATOR OR A REPLICATION FACTOR DESCRIPTOR
!  --------------------------------------------------------

      IF(IDN.GE.IDNR(1,1) .AND. IDN.LE.IDNR(1,2)) THEN

!        Note that the above test is checking whether IDN is the bit-
!        wise representation of a FXY (descriptor) value denoting F=1
!        regular (i.e. non-delayed) replication, since, as was
!        initialized within subroutine BFRINI,
!        IDNR(1,1) = IFXY('101000'), and IDNR(1,2) = IFXY('101255').

         TAB  = 'R'
         IRET = -MOD(IDN,256)
         GOTO 100
      ENDIF

      DO I=2,5
      IF(IDN.EQ.IDNR(I,1)) THEN
         TAB  = 'R'
         IRET = I
         GOTO 100
      ELSEIF(IDN.EQ.IDNR(I,2)) THEN
         TAB  = 'F'
         IRET = I
         GOTO 100
      ENDIF
      ENDDO

!  LOOK FOR IDN IN TABLE D
!  -----------------------

      DO I=1,NTBD(LUN)
      IF(IDN.EQ.IDND(I,LUN)) THEN
         NEMO = TABD(I,LUN)(7:14)
         TAB  = 'D'
         IRET = I
         GOTO 100
      ENDIF
      ENDDO

!  LOOK FOR IDN IN TABLE B
!  -----------------------

      DO I=1,NTBB(LUN)
      IF(IDN.EQ.IDNB(I,LUN)) THEN
         NEMO = TABB(I,LUN)(7:14)
         TAB  = 'B'
         IRET = I
         GOTO 100
      ENDIF
      ENDDO

!  LOOK FOR IDN IN TABLE C
!  -----------------------

      CID = ADN30(IDN,6)
      IF(CID(1:3).EQ.'201' .OR. CID(1:3).EQ.'202' .OR. &
         CID(1:3).EQ.'206' .OR. CID(1:3).EQ.'207' .OR. &
         CID(1:3).EQ.'208') THEN
         NEMO = CID(1:6)
         READ(NEMO,'(1X,I2)') IRET
         TAB  = 'C'
         GOTO 100
      ENDIF

!  EXIT
!  ----

100   RETURN
      END SUBROUTINE NUMTAB
