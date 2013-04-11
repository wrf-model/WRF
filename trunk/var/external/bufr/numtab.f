      SUBROUTINE NUMTAB(LUN,IDN,NEMO,TAB,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NUMTAB
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE FIRST SEARCHES FOR AN INTEGER IDN,
C   CONTAINING THE BIT-WISE REPRESENTATION OF A DESCRIPTOR (FXY) VALUE,
C   WITHIN THE INTERNAL BUFR REPLICATION ARRAYS IN COMMON BLOCK
C   /REPTAB/ TO SEE IF IDN IS A REPLICATION DESCRIPTOR OR A REPLICATION
C   FACTOR DESCRIPTOR.  IF THIS SEARCH IS UNSUCCESSFUL, IT SEACHES FOR
C   IDN WITHIN THE INTERNAL BUFR TABLE D AND B ARRAYS IN COMMON BLOCK
C   /TABABD/ TO SEE IF IDN IS A TABLE D OR TABLE B DESCRIPTOR.  IF THIS
C   SEARCH IS ALSO UNSUCCESSFUL, IT SEARCHES TO SEE IF IDN IS A TABLE C
C   OPERATOR DESCRIPTOR.  IF IDN IS FOUND IN ANY OF THESE SEARCHES,
C   THIS SUBROUTINE RETURNS THE CORRESPONDING MNEMONIC AND OTHER
C   INFORMATION FROM WITHIN EITHER THE INTERNAL ARRAYS FOR REPLICATION,
C   REPLICATION FACTOR, TABLE D OR TABLE B DESCRIPTORS; OR FROM THE
C   KNOWN VALUES FOR TABLE C DESCRIPTORS.  IF IDN IS NOT FOUND, IT
C   RETURNS WITH IRET=0.  THIS SUBROUTINE IS IDENTICAL TO BUFR ARCHIVE
C   LIBRARY SUBROUTINE NUMTBD EXCEPT NUMTBD SEARCHS FOR IDN WITHIN ONLY
C   THE INTERNAL TABLE D AND B ARRAYS.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2000-09-19  J. WOOLLEN -- ADDED CAPABILITY TO ENCODE AND DECODE DATA
C                           USING THE OPERATOR DESCRIPTORS (BUFR TABLE
C                           C) FOR CHANGING WIDTH AND CHANGING SCALE
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; CORRECTED TYPO ("IDN" WAS
C                           SPECIFIED AS "ID" IN CALCULATION OF IRET
C                           FOR TAB='C')
C 2005-11-29  J. ATOR    -- ADDED SUPPORT FOR 207 AND 208 OPERATORS
C
C USAGE:    CALL NUMTAB (LUN, IDN, NEMO, TAB, IRET)
C   INPUT ARGUMENT LIST:
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C     IDN      - INTEGER: BIT-WISE REPRESENTATION OF DESCRIPTOR (FXY)
C                VALUE
C
C   OUTPUT ARGUMENT LIST:
C     NEMO     - CHARACTER*(*): MNEMONIC CORRESPONDING TO IDN
C     TAB      - CHARACTER*1: TYPE OF FXY VALUE THAT IS BIT-WISE
C                REPRESENTED BY IDN:
C                     'B' = BUFR Table B descriptor
C                     'C' = BUFR Table C descriptor
C                     'D' = BUFR Table D descriptor
C                     'R' = BUFR replication descriptor
C                     'F' = BUFR replication factor descriptor
C     IRET     - INTEGER: RETURN VALUE (SEE REMARKS)
C
C
C REMARKS:
C    THE INTERPRETATION OF THE RETURN VALUE IRET DEPENDS UPON THE
C    RETURN VALUE OF TAB AND THE INPUT VALUE IDN, AS FOLLOWS:
C
C    IF ( TAB = 'B' ) THEN
C       IRET = positional index of IDN within internal BUFR Table B
C              array
C    ELSE IF ( TAB = 'C') THEN
C    IRET = the X portion of the FXY value that is bit-wise represented
C           by IDN
C    ELSE IF ( TAB = 'D') THEN
C       IRET = positional index of IDN within internal BUFR Table D
C              array
C    ELSE IF ( TAB = 'R') THEN
C       IF ( the F portion of the FXY value that is bit-wise
C            represented by IDN ) = 1 THEN
C          ---> regular (non-delayed) replication
C          IRET = ( -1 * ( the Y portion of the FXY value that is bit-
C                        wise represented by IDN ) )
C          ---> where Y = the number of F=1 regular replications
C       ELSE
C          ---> delayed replication
C          IRET = positional index, I, of IDN within internal
C                 replication array IDNR(I,1), where I=2,5
C          ---> IRET = I = 2 --> 16-bit delayed replication descriptor
C          ---> IRET = I = 3 -->  8-bit delayed replication descriptor
C          ---> IRET = I = 4 -->  8-bit delayed replication descriptor
C                                (stack)
C          ---> IRET = I = 5 -->  1-bit delayed replication descriptor
C       END IF
C    ELSE IF ( TAB = 'F') THEN
C       IRET = positional index, I, of IDN within internal replication
C              array IDNR(I,2), where I=2,5
C       ---> IRET = I = 2 --> 16-bit replication factor descriptor
C       ---> IRET = I = 3 -->  8-bit replication factor descriptor
C       ---> IRET = I = 4 -->  8-bit replication factor descriptor
C                              (stack)
C       ---> IRET = I = 5 -->  1-bit replication factor descriptor
C    ELSE IF ( IRET = 0 ) THEN
C       IDN was not found in internal BUFR Table B or D, nor does it
C       represent a Table C operator descriptor, a replication
C       descriptor, or a replication factor descriptor
C    END IF
C
C
C    THIS ROUTINE CALLS:        ADN30
C    THIS ROUTINE IS CALLED BY: CKTABA   NEMTBD   SEQSDX   STNDRD
C                               UFBQCP
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

C     Note that the values within the COMMON /REPTAB/ arrays were
C     initialized within subroutine BFRINI.

      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)

      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES),
     .                MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2),
     .                IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES),
     .                TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES),
     .                TABD(MAXTBD,NFILES)

      CHARACTER*(*) NEMO
      CHARACTER*600 TABD
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*6   ADN30,CID
      CHARACTER*3   TYPS
      CHARACTER*1   REPS,TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      NEMO = ' '
      IRET = 0
      TAB = ' '

C  LOOK FOR A REPLICATOR OR A REPLICATION FACTOR DESCRIPTOR
C  --------------------------------------------------------

      IF(IDN.GE.IDNR(1,1) .AND. IDN.LE.IDNR(1,2)) THEN

C        Note that the above test is checking whether IDN is the bit-
C        wise representation of a FXY (descriptor) value denoting F=1
C        regular (i.e. non-delayed) replication, since, as was
C        initialized within subroutine BFRINI,
C        IDNR(1,1) = IFXY('101000'), and IDNR(1,2) = IFXY('101255').

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

C  LOOK FOR IDN IN TABLE D
C  -----------------------

      DO I=1,NTBD(LUN)
      IF(IDN.EQ.IDND(I,LUN)) THEN
         NEMO = TABD(I,LUN)(7:14)
         TAB  = 'D'
         IRET = I
         GOTO 100
      ENDIF
      ENDDO

C  LOOK FOR IDN IN TABLE B
C  -----------------------

      DO I=1,NTBB(LUN)
      IF(IDN.EQ.IDNB(I,LUN)) THEN
         NEMO = TABB(I,LUN)(7:14)
         TAB  = 'B'
         IRET = I
         GOTO 100
      ENDIF
      ENDDO

C  LOOK FOR IDN IN TABLE C
C  -----------------------

      CID = ADN30(IDN,6)
      IF(CID(1:3).EQ.'201' .OR. CID(1:3).EQ.'202' .OR.
     .   CID(1:3).EQ.'206' .OR. CID(1:3).EQ.'207' .OR.
     .   CID(1:3).EQ.'208') THEN
         NEMO = CID(1:6)
         READ(NEMO,'(1X,I2)') IRET
         TAB  = 'C'
         GOTO 100
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
