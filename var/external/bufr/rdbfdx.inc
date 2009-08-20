      SUBROUTINE RDBFDX(LUNIT,LUN)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    RDBFDX
!   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
!
! ABSTRACT: THIS SUBROUTINE READS BUFR TABLE (DICTIONARY) MESSAGES FROM
!   AN INPUT BUFR FILE AND COPIES THEM INTO INTERNAL MEMORY (ARRAYS IN
!   COMMON BLOCK /TABABD/).  IT IS ASSUMED THERE IS AT LEAST ONE
!   DICTIONARY MESSAGE AT THE BEGINNING OF THE FILE.  THIS SUBROUTINE
!   PERFORMS A FUNCTION SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE
!   RDUSDX, EXECPT THAT RDUSDX READS FROM AN EXTERNAL FILE CONTAINING A
!   USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT.  SEE
!   DOCBLOCK IN RDUSDX FOR A DESCRIPTION OF THE ARRAYS THAT ARE FILLED
!   IN COMMON BLOCK /TABABD/.
!
! PROGRAM HISTORY LOG:
! 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
! 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
!                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
! 1996-12-17  J. WOOLLEN -- FIXED FOR SOME MVS COMPILER'S TREATMENT OF
!                           INTERNAL READS (INCREASES PORTABILITY)
! 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
!                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
!                           ROUTINE "BORT"; CORRECTED SOME MINOR ERRORS
! 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
!                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
!                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
!                           BUFR FILES UNDER THE MPI)
! 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           10,000 TO 20,000 BYTES
! 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
!                           INTERDEPENDENCIES
! 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
!                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
!                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
!                           TERMINATES ABNORMALLY
! 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
!                           20,000 TO 50,000 BYTES
! 2005-11-29  J. ATOR    -- USE GETLENS, IUPBS01 AND RDMSGW
!
! USAGE:    CALL RDBFDX (LUNIT, LUN)
!   INPUT ARGUMENT LIST:
!     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
!     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
!                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT)
!
!   INPUT FILES:
!     UNIT "LUNIT" - BUFR FILE
!
! REMARKS:
!    THIS ROUTINE CALLS:        BORT     CAPIT    CHRTRN   CHRTRNA
!                               DIGIT    DXINIT   GETLENS  IDN30
!                               IFXY     IUPBS01  IUPM     MAKESTAB
!                               NENUAA   NENUBD   PKTDD    RDMSGW
!    THIS ROUTINE IS CALLED BY: READDX
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

      CHARACTER*600 TABD
      CHARACTER*128 BORT_STR
      CHARACTER*128 TABB,TABB1,TABB2
      CHARACTER*128 TABA
      CHARACTER*56  DXSTR
      CHARACTER*50  DXCMP
      CHARACTER*24  UNIT
      CHARACTER*8   NEMO
      CHARACTER*6   NUMB,CIDN
      CHARACTER*1   MOCT(MXMSGL)
      DIMENSION     MBAY(MXMSGLD4),LDXBD(10),LDXBE(10)

      EQUIVALENCE   (MBAY(1),MOCT(1))

      DATA LDXBD /38,70,8*0/
      DATA LDXBE /42,42,8*0/

!-----------------------------------------------------------------------
      JA(I) = IA+1+LDA*(I-1)
      JB(I) = IB+1+LDB*(I-1)
!-----------------------------------------------------------------------

!  INITIALIZE THE DICTIONARY TABLE CONTROL WORD PARTITION ARRAYS
!  -------------------------------------------------------------

      CALL DXINIT(LUN,0)
      REWIND LUNIT
      IDX = 0

!  CLEAR THE MESSAGE BUFFER
!  ------------------------

1     DO I=1,MXMSGLD4
        MBAY(I) = 0
      ENDDO

!  READ A MESSAGE
!  --------------

      CALL RDMSGW(LUNIT,MBAY,IER)
      IF(IER.EQ.-2) THEN
        GOTO 900
      ELSEIF(IER.NE.-1) THEN
!  .... IDX counts the number of dictionary messages read
        IDX = IDX+1
      ENDIF

!  IS THIS IS A BUFR DICTIONARY MESSAGE?
!  -------------------------------------

      IF(IUPBS01(MBAY,'MTYP').NE.11) THEN

!        NO, so assume that we have now read in all of the available
!        dictionary messages and that, therefore, it is safe to go
!        ahead and build a jump/link table (in COMMON /TABLES/) using
!        the information that we just read and stored within
!        COMMON /TABABD/.

         CALL MAKESTAB

!        Before returning, go ahead and reposition the file at the
!        end of the dictionary messages, so that the next read (in
!        READMG, etc.) will get the first message which contains
!        actual data.

         REWIND LUNIT

         DO NDX=1,IDX-1
           CALL RDMSGW(LUNIT,MBAY,IER)
           IF(IER.LT.0) GOTO 908
         ENDDO

         GOTO 100
      ENDIF

!  THIS IS A DICTIONARY MESSAGE, SO CONTINUE ONWARD
!  ------------------------------------------------

      IDXS = IUPBS01(MBAY,'MSBT')+1
      IF(IDXS.GT.IDXV+1) IDXS = IUPBS01(MBAY,'MTVL')+1
      IF(LDXA(IDXS).EQ.0) GOTO 901
      IF(LDXB(IDXS).EQ.0) GOTO 901
      IF(LDXD(IDXS).EQ.0) GOTO 901

      CALL GETLENS(MBAY,3,LEN0,LEN1,LEN2,LEN3,L4,L5)
      I3 = LEN0+LEN1+LEN2
      DXCMP = ' '
      CALL CHRTRN(DXCMP,MOCT(I3+8),NXSTR(IDXS))
      IF(DXCMP.NE.DXSTR(IDXS)) GOTO 902

!  SECTION 4 - READ DEFINITIONS FOR TABLES A, B AND D
!  --------------------------------------------------

      LDA  = LDXA (IDXS)
      LDB  = LDXB (IDXS)
      LDD  = LDXD (IDXS)
      LDBD = LDXBD(IDXS)
      LDBE = LDXBE(IDXS)
      L30  = LD30 (IDXS)

      IA = I3+LEN3+5
      LA = IUPM(MOCT(IA),8)
      IB = JA(LA+1)
      LB = IUPM(MOCT(IB),8)
      ID = JB(LB+1)
      LD = IUPM(MOCT(ID),8)

!  TABLE A
!  -------

      DO I=1,LA
      N = NTBA(LUN)+1
      IF(N.GT.NTBA(0)) GOTO 903
      CALL CHRTRNA(TABA(N,LUN),MOCT(JA(I)),LDA)
      NUMB = '   '//TABA(N,LUN)(1:3)
      NEMO = TABA(N,LUN)(4:11)
      CALL NENUAA(NEMO,NUMB,LUN)
      NTBA(LUN) = N

      IF(DIGIT(NEMO(3:8))) THEN
!  .... Message type and subtype obtained directly from Table A mnemo.
         READ(NEMO,'(2X,2I3)') MTYP,MSBT
         IDNA(N,LUN,1) = MTYP
         IDNA(N,LUN,2) = MSBT
      ELSE
!  .... Message type obtained from Y value of Table A sequence descr.
!       Message subtype hardwired to ZERO
         READ(NUMB(4:6),'(I3)') IDNA(N,LUN,1)
         IDNA(N,LUN,2) = 0
      ENDIF
      ENDDO

!  TABLE B
!  -------

      DO I=1,LB
      N = NTBB(LUN)+1
      IF(N.GT.NTBB(0)) GOTO 904
      CALL CHRTRNA(TABB1,MOCT(JB(I)     ),LDBD)
      CALL CHRTRNA(TABB2,MOCT(JB(I)+LDBD),LDBE)
      TABB(N,LUN) = TABB1(1:LDXBD(IDXV+1))//TABB2(1:LDXBE(IDXV+1))
      NUMB = TABB(N,LUN)(1:6)
      NEMO = TABB(N,LUN)(7:14)
      UNIT = TABB(N,LUN)(71:94)
      CALL CAPIT(UNIT)
      TABB(N,LUN)(71:94) = UNIT
      CALL NENUBD(NEMO,NUMB,LUN)
      IDNB(N,LUN) = IFXY(NUMB)
      NTBB(LUN) = N
      ENDDO

!  TABLE D
!  -------

      DO I=1,LD
      N = NTBD(LUN)+1
      IF(N.GT.NTBD(0)) GOTO 905
      CALL CHRTRNA(TABD(N,LUN),MOCT(ID+1),LDD)
      NUMB = TABD(N,LUN)(1:6)
      NEMO = TABD(N,LUN)(7:14)
      CALL NENUBD(NEMO,NUMB,LUN)
      IDND(N,LUN) = IFXY(NUMB)
      ND = IUPM(MOCT(ID+LDD+1),8)
      IF(ND.GT.MAXCD) GOTO 906
      DO J=1,ND
      NDD = ID+LDD+2 + (J-1)*L30
      CALL CHRTRNA(CIDN,MOCT(NDD),L30)
      IDN = IDN30(CIDN,L30)
      CALL PKTDD(N,LUN,IDN,IRET)
      IF(IRET.LT.0) GOTO 907
      ENDDO
      ID = ID+LDD+1 + ND*L30
      IF(IUPM(MOCT(ID+1),8).EQ.0) ID = ID+1
      NTBD(LUN) = N
      ENDDO

!  GOTO READ THE NEXT MESSAGE
!  --------------------------

      GOTO 1

!  EXITS
!  -----

100   RETURN
900   CALL BORT('BUFRLIB: RDBFDX - ERROR READING A BUFR DICTIONARY '// &
       'MESSAGE')
901   CALL BORT('BUFRLIB: RDBFDX - UNEXPECTED DICTIONARY MESSAGE '// &
       'SUBTYPE OR LOCAL VERSION NUMBER (E.G., L.V.N. HIGHER THAN '// &
       'KNOWN)')
902   CALL BORT('BUFRLIB: RDBFDX - UNEXPECTED DICTIONARY MESSAGE '// &
       'CONTENTS')
903   WRITE(BORT_STR,'("BUFRLIB: RDBFDX - NUMBER OF TABLE A ENTRIES '// &
       'IN BUFR TABLE EXCEEDS THE LIMIT (",I4,")")') NTBA(0)
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: RDBFDX - NUMBER OF TABLE B ENTRIES '// &
       'IN BUFR TABLE EXCEEDS THE LIMIT (",I4,")")') NTBB(0)
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: RDBFDX - NUMBER OF TABLE D ENTRIES '// &
       'IN BUFR TABLE EXCEEDS THE LIMIT (",I4,")")') NTBD(0)
      CALL BORT(BORT_STR)
906   WRITE(BORT_STR,'("BUFRLIB: RDBFDX - NUMBER OF DESCRIPTORS IN '// &
       'TABLE D ENTRY ",A," IN BUFR TABLE (",I4,") EXCEEDS THE LIMIT '// &
       ' (",I4,")")') NEMO,ND,MAXCD
      CALL BORT(BORT_STR)
907   CALL BORT('BUFRLIB: RDBFDX - BAD RETURN FROM BUFRLIB ROUTINE '// &
       'PKTDD, SEE PREVIOUS WARNING MESSAGE')
908   CALL BORT('BUFRLIB: RDBFDX - ERROR OR E-O-F POSITIONING READ TO'// &
       ' FIRST DATA MESSAGE AFTER DCTY MESSAGES (FILE CONTAINS ONLY '// &
       'DCTY MSGS?)')
      END SUBROUTINE RDBFDX
