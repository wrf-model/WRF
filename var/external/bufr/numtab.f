      SUBROUTINE NUMTAB(LUN,IDN,NEMO,TAB,IRET)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NUMTAB
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE FIRST SEARCHES FOR AN INTEGER IDN,
C   CONTAINING THE BIT-WISE REPRESENTATION OF A DESCRIPTOR (FXY) VALUE,
C   WITHIN THE INTERNAL BUFR REPLICATION ARRAYS IN COMMON BLOCK /REPTAB/
C   TO SEE IF IDN IS A REPLICATION DESCRIPTOR OR A REPLICATION FACTOR
C   DESCRIPTOR.  IF THIS SEARCH IS UNSUCCESSFUL, IT SEACHES FOR IDN
C   WITHIN THE INTERNAL BUFR TABLE D AND B ARRAYS TO SEE IF IDN IS A
C   TABLE D OR TABLE B DESCRIPTOR.  IF THIS SEARCH IS ALSO UNSUCCESSFUL,
C   IT SEARCHES TO SEE IF IDN IS A TABLE C OPERATOR DESCRIPTOR.  IF IDN
C   IS FOUND IN ANY OF THESE SEARCHES, THIS SUBROUTINE RETURNS THE
C   CORRESPONDING MNEMONIC AND OTHER INFORMATION FROM WITHIN EITHER THE
C   INTERNAL ARRAYS FOR REPLICATION, REPLICATION FACTOR, TABLE D OR
C   TABLE B DESCRIPTORS, OR ELSE FROM THE KNOWN VALUES FOR TABLE C
C   DESCRIPTORS.  IF IDN IS NOT FOUND, IT RETURNS WITH IRET=0.
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
C 2009-04-21  J. ATOR    -- USE NUMTBD
C 2010-03-19  J. ATOR    -- ADDED SUPPORT FOR 204 AND 205 OPERATORS
C 2012-03-02  J. ATOR    -- ADDED SUPPORT FOR 203 OPERATOR
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
C REMARKS:
C    THE INTERPRETATION OF THE RETURN VALUE IRET DEPENDS UPON THE
C    RETURN VALUE OF TAB AND THE INPUT VALUE IDN, AS FOLLOWS:
C
C    IF ( TAB = 'B' ) THEN
C       IRET = positional index of IDN within internal BUFR Table B
C              array
C    ELSE IF ( TAB = 'C') THEN
C       IRET = the X portion of the FXY value that is bit-wise
C              represented by IDN
C    ELSE IF ( TAB = 'D') THEN
C       IRET = positional index of IDN within internal BUFR Table D
C              array
C    ELSE IF ( TAB = 'R') THEN
C       IF ( IDN denoted regular (i.e. non-delayed) replication ) THEN
C          IRET = ((-1)*Y), where Y is the number of replications
C       ELSE ( i.e. delayed replication )
C          IRET = positional index (=I) of IDN within internal
C                 replication descriptor array IDNR(I,1), where:
C               IRET (=I) =2 --> 16-bit delayed replication descriptor
C               IRET (=I) =3 -->  8-bit delayed replication descriptor
C               IRET (=I) =4 -->  8-bit delayed replication descriptor
C                                 (stack)
C               IRET (=I) =5 -->  1-bit delayed replication descriptor
C       END IF
C    ELSE IF ( TAB = 'F') THEN
C       IRET = positional index (=I) of IDN within internal replication
C              factor array IDNR(I,2), where:
C            IRET (=I) =2 --> 16-bit replication factor
C            IRET (=I) =3 -->  8-bit replication factor
C            IRET (=I) =4 -->  8-bit replication factor
C                              (stack)
C            IRET (=I) =5 -->  1-bit replication factor
C    ELSE IF ( IRET = 0 ) THEN
C       IDN was not found in internal BUFR Table B or D, nor does it
C       represent a Table C operator descriptor, a replication
C       descriptor, or a replication factor descriptor
C    END IF
C
C
C    THIS ROUTINE CALLS:        ADN30    NUMTBD
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

      CHARACTER*(*) NEMO
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

C  LOOK FOR IDN IN TABLE B AND TABLE D
C  -----------------------------------

      CALL NUMTBD(LUN,IDN,NEMO,TAB,IRET)
      IF(IRET.NE.0) GOTO 100

C  LOOK FOR IDN IN TABLE C
C  -----------------------

      CID = ADN30(IDN,6)
      IF ( (CID(1:2).EQ.'20') .AND.
     .    ( LGE(CID(3:3),'1') .AND. LLE(CID(3:3),'8') ) ) THEN
         NEMO = CID(1:6)
         READ(NEMO,'(1X,I2)') IRET
         TAB  = 'C'
         GOTO 100
      ENDIF

C  EXIT
C  ----

100   RETURN
      END
