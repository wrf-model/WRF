      SUBROUTINE W3FI75 (IBITL,ITYPE,ITOSS,FLD,IFLD,IBMAP,IBDSFL,
     &  NPTS,BDS11,IPFLD,PFLD,LEN,LENBDS,IBERR,PDS,IGDS)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FI75        GRIB PACK DATA AND FORM BDS OCTETS(1-11)
C   PRGMMR: FARLEY           ORG: NMC421      DATE:94-11-22
C
C ABSTRACT: THIS ROUTINE PACKS A GRIB FIELD AND FORMS OCTETS(1-11)
C   OF THE BINARY DATA SECTION (BDS).
C
C PROGRAM HISTORY LOG:
C   92-07-10  M. FARLEY   ORIGINAL AUTHOR
C   92-10-01  R.E.JONES   CORRECTION FOR FIELD OF CONSTANT DATA
C   92-10-16  R.E.JONES   GET RID OF ARRAYS FP AND INT
C   93-08-06  CAVANAUGH   ADDED ROUTINES FI7501, FI7502, FI7503
C                         TO ALLOW SECOND ORDER PACKING IN PDS.
C   93-07-21 STACKPOLE    ASSORTED REPAIRS TO GET 2ND DIFF PACK IN
C   93-10-28 CAVANAUGH    COMMENTED OUT NONOPERATIONAL PRINTS AND
C                         WRITE STATEMENTS
C   93-12-15  CAVANAUGH   CORRECTED LOCATION OF START OF FIRST ORDER
C                         VALUES AND START OF SECOND ORDER VALUES TO
C                         REFLECT A BYTE LOCATION IN THE BDS INSTEAD
C                         OF AN OFFSET IN SUBROUTINE FI7501.
C   94-01-27  CAVANAUGH   ADDED IGDS AS INPUT ARGUMENT TO THIS ROUTINE
C                         AND ADDED PDS AND IGDS ARRAYS TO THE CALL TO
C                         W3FI82 TO PROVIDE INFORMATION NEEDED FOR
C                         BOUSTROPHEDONIC PROCESSING.
C   94-05-25  CAVANAUGH   SUBROUTINE FI7503 HAS BEEN ADDED TO PROVIDE
C                         FOR ROW BY ROW OR COLUMN BY COLUMN SECOND
C                         ORDER PACKING.  THIS FEATURE CAN BE ACTIVATED
C                         BY SETTING IBDSFL(7) TO ZERO.
C   94-07-08  CAVANAUGH   COMMENTED OUT PRINT STATEMENTS USED FOR DEBUG
C   94-11-22  FARLEY      ENLARGED WORK ARRAYS TO HANDLE .5DEGREE GRIDS
C   95-06-01  R.E.JONES   CORRECTION FOR NUMBER OF UNUSED BITS AT END
C                         OF SECTION 4, IN BDS BYTE 4, BITS 5-8.
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C
C USAGE:    CALL W3FI75 (IBITL,ITYPE,ITOSS,FLD,IFLD,IBMAP,IBDSFL,
C    &              NPTS,BDS11,IPFLD,PFLD,LEN,LENBDS,IBERR,PDS,IGDS)
C   INPUT ARGUMENT LIST:
C     IBITL     - 0, COMPUTER COMPUTES PACKING LENGTH FROM POWER
C                    OF 2 THAT BEST FITS THE DATA.
C                 8, 12, ETC. COMPUTER RESCALES DATA TO FIT INTO
C                    SET NUMBER OF BITS.
C     ITYPE     - 0 = IF INPUT DATA IS FLOATING POINT (FLD)
C                 1 = IF INPUT DATA IS INTEGER (IFLD)
C     ITOSS     - 0 = NO BIT MAP IS INCLUDED (DON'T TOSS DATA)
C                 1 = TOSS NULL DATA ACCORDING TO IBMAP
C     FLD       - REAL ARRAY OF DATA TO BE PACKED IF ITYPE=0
C     IFLD      - INTEGER ARRAY TO BE PACKED IF ITYPE=1
C     IBMAP     - BIT MAP SUPPLIED FROM USER
C     IBDSFL    - INTEGER ARRAY CONTAINING TABLE 11 FLAG INFO
C                 BDS OCTET 4:
C                 (1) 0 = GRID POINT DATA
C                     1 = SPHERICAL HARMONIC COEFFICIENTS
C                 (2) 0 = SIMPLE PACKING
C                     1 = SECOND ORDER PACKING
C                 (3) 0 = ORIGINAL DATA WERE FLOATING POINT VALUES
C                     1 = ORIGINAL DATA WERE INTEGER VALUES
C                 (4) 0 = NO ADDITIONAL FLAGS AT OCTET 14
C                     1 = OCTET 14 CONTAINS FLAG BITS 5-12
C                 (5) 0 = RESERVED - ALWAYS SET TO 0
C                 (6) 0 = SINGLE DATUM AT EACH GRID POINT
C                     1 = MATRIX OF VALUES AT EACH GRID POINT
C                 (7) 0 = NO SECONDARY BIT MAPS
C                     1 = SECONDARY BIT MAPS PRESENT
C                 (8) 0 = SECOND ORDER VALUES HAVE CONSTANT WIDTH
C                     1 = SECOND ORDER VALUES HAVE DIFFERENT WIDTHS
C     NPTS      - NUMBER OF GRIDPOINTS IN ARRAY TO BE PACKED
C     IGDS      - ARRAY OF GDS INFORMATION
C
C   OUTPUT ARGUMENT LIST:
C     BDS11     - FIRST 11 OCTETS OF BDS
C     PFLD      - PACKED GRIB FIELD
C     LEN       - LENGTH OF PFLD
C     LENBDS    - LENGTH OF BDS
C     IBERR     - 1, ERROR CONVERTING IEEE F.P. NUMBER TO IBM370 F.P.
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
C
      REAL            FLD(*)
C     REAL            FWORK(260000)
C
C     FWORK CAN USE DYNAMIC ALLOCATION OF MEMORY ON CRAY
C
      REAL            FWORK(NPTS)
      REAL            RMIN,REFNCE
C
      INTEGER         IPFLD(*)
      INTEGER         IBDSFL(*)
      INTEGER         IBMAP(*)
      INTEGER         IFLD(*),IGDS(*)
C     INTEGER         IWORK(260000)
C
C     IWORK CAN USE DYNAMIC ALLOCATION OF MEMORY ON CRAY
C
      INTEGER         IWORK(NPTS)
C
      LOGICAL         CONST
C
      CHARACTER * 1   BDS11(11),PDS(*)
      CHARACTER * 1   PFLD(*)
      CHARACTER * 1   CIEXP(8)
      CHARACTER * 1   CIMANT(8)
C
      EQUIVALENCE     (IEXP,CIEXP(1))
      EQUIVALENCE     (IMANT,CIMANT(1))
C
C            1.0   PACK THE FIELD.
C
C            1.1   TOSS DATA IF BITMAP BEING USED,
C                  MOVING 'DATA' TO WORK AREA...
C
      CONST = .FALSE.
      IBERR = 0
      IW    = 0
C
      IF (ITOSS .EQ. 1) THEN
        IF (ITYPE .EQ. 0) THEN
          DO 110 IT=1,NPTS
            IF (IBMAP(IT) .EQ. 1) THEN
              IW = IW + 1
              FWORK(IW) = FLD(IT)
            ENDIF
  110     CONTINUE
          NPTS = IW
        ELSE IF (ITYPE .EQ. 1) THEN
          DO 111 IT=1,NPTS
            IF (IBMAP(IT) .EQ. 1) THEN
              IW = IW + 1
              IWORK(IW) = IFLD(IT)
            ENDIF
  111     CONTINUE
          NPTS = IW
        ENDIF
C
C             ELSE, JUST MOVE DATA TO WORK ARRAY
C
      ELSE IF (ITOSS .EQ. 0) THEN
        IF (ITYPE .EQ. 0) THEN
          DO 112 IT=1,NPTS
            FWORK(IT) = FLD(IT)
  112     CONTINUE
        ELSE IF (ITYPE .EQ. 1) THEN
          DO 113 IT=1,NPTS
            IWORK(IT) = IFLD(IT)
  113     CONTINUE
        ENDIF
      ENDIF
C
C            1.2   CONVERT DATA IF NEEDED PRIOR TO PACKING.
C                  (INTEGER TO F.P. OR F.P. TO INTEGER)
C     ITYPE = 0...FLOATING POINT DATA
C       IBITL = 0...PACK IN LEAST # BITS...CONVERT TO INTEGER
C     ITYPE = 1...INTEGER DATA
C       IBITL > 0...PACK IN FIXED # BITS...CONVERT TO FLOATING POINT
C
      IF (ITYPE .EQ. 0 .AND. IBITL .EQ. 0) THEN
        DO 120 IF=1,NPTS
          IWORK(IF) = NINT(FWORK(IF))
  120   CONTINUE
      ELSE IF (ITYPE .EQ. 1 .AND. IBITL .NE. 0) THEN
        DO 123 IF=1,NPTS
          FWORK(IF) = FLOAT(IWORK(IF))
  123   CONTINUE
      ENDIF
C
C            1.3   PACK THE DATA.
C
      IF (IBDSFL(2).NE.0) THEN
C                                    SECOND ORDER PACKING
C
C            PRINT*,'  DOING SECOND ORDER PACKING...'
          IF (IBITL.EQ.0) THEN
C
C             PRINT*,'    AND VARIABLE BIT PACKING'
C
C                           WORKING WITH INTEGER VALUES
C                           SINCE DOING VARIABLE BIT PACKING
C
              MAX  = IWORK(1)
              MIN  = IWORK(1)
              DO 300 I = 2, NPTS
                  IF (IWORK(I).LT.MIN) THEN
                      MIN  = IWORK(I)
                  ELSE IF (IWORK(I).GT.MAX) THEN
                      MAX  = IWORK(I)
                  END IF
  300         CONTINUE
C                           EXTRACT MINIMA
              DO 400 I = 1, NPTS
C                 IF (IWORK(I).LT.0) THEN
C                     PRINT *,'MINIMA 400',I,IWORK(I),NPTS
C                 END IF
                  IWORK(I)  = IWORK(I) - MIN
  400         CONTINUE
              REFNCE  = MIN
              IDIFF   = MAX - MIN
C             PRINT *,'REFERENCE VALUE',REFNCE
C
C             WRITE (6,FMT='(''  MINIMA REMOVED      = '',/,
C    &              10(3X,10I10,/))') (IWORK(I),I=1,6)
C             WRITE (6,FMT='(''  END OF ARRAY  = '',/,
C    &              10(3X,10I10,/))') (IWORK(I),I=NPTS-5,NPTS)
C
C                      FIND BIT WIDTH OF IDIFF
C
              CALL FI7505 (IDIFF,KWIDE)
C             PRINT*,'  BIT WIDTH FOR ORIGINAL DATA', KWIDE
              ISCAL2 = 0
C
C             MULTIPLICATIVE SCALE FACTOR SET TO 1
C             IN ANTICIPATION OF POSSIBLE USE IN GLAHN 2DN DIFF
C
              SCAL2 = 1.
C
          ELSE
C
C             PRINT*,'   AND FIXED BIT PACKING, IBITL = ', IBITL
C                               FIXED BIT PACKING
C                               - LENGTH OF FIELD IN IBITL
C                               - MUST BE REAL DATA
C                            FLOATING POINT INPUT
C
              RMAX  = FWORK(1)
              RMIN  = FWORK(1)
              DO 100 I = 2, NPTS
                  IF (FWORK(I).LT.RMIN) THEN
                      RMIN  = FWORK(I)
                  ELSE IF (FWORK(I).GT.RMAX) THEN
                      RMAX  = FWORK(I)
                  END IF
  100         CONTINUE
              REFNCE  = RMIN
C             PRINT *,'100 REFERENCE',REFNCE
C                             EXTRACT MINIMA
              DO 200 I = 1, NPTS
                  FWORK(I)  = FWORK(I) - RMIN
  200         CONTINUE
C             PRINT *,'REFERENCE VALUE',REFNCE
C             WRITE (6,FMT='(''  MINIMA REMOVED      = '',/,
C    &              10(3X,10F8.2,/))') (FWORK(I),I=1,6)
C             WRITE (6,FMT='(''  END OF ARRAY  = '',/,
C    &              10(3X,10F8.2,/))') (FWORK(I),I=NPTS-5,NPTS)
C                                FIND LARGEST DELTA
              IDELT  = NINT(RMAX - RMIN)
C                                DO BINARY SCALING
C                                   FIND OUT WHAT BINARY SCALE FACTOR
C                                       PERMITS CONTAINMENT OF
C                                       LARGEST DELTA
              CALL FI7505 (IDELT,IWIDE)
C
C                                   BINARY SCALING
C
              ISCAL2  = IWIDE - IBITL
C             PRINT *,'SCALING NEEDED TO FIT =',ISCAL2
C             PRINT*,'  RANGE OF  = ',IDELT
C
C                                EXPAND DATA WITH BINARY SCALING
C                                CONVERT TO INTEGER
              SCAL2  = 2.0**ISCAL2
              SCAL2  = 1./ SCAL2
              DO 600 I = 1, NPTS
                  IWORK(I)  = NINT(FWORK(I) * SCAL2)
  600         CONTINUE
              KWIDE = IBITL
          END IF
C
C  *****************************************************************
C
C           FOLLOWING IS FOR GLAHN SECOND DIFFERENCING
C           NOT STANDARD GRIB
C
C            TEST FOR SECOND DIFFERENCE PACKING
C            BASED OF SIZE OF PDS - SIZE IN FIRST 3 BYTES
C
          CALL GBYTE (PDS,IPDSIZ,0,24)
          IF (IPDSIZ.EQ.50) THEN
C             PRINT*,'  DO SECOND DIFFERENCE PACKING '
C
C                   GLAHN PACKING TO 2ND DIFFS
C
C             WRITE (6,FMT='(''  CALL TO W3FI82 WITH = '',/,
C    &                  10(3X,10I6,/))') (IWORK(I),I=1,NPTS)
C
               CALL W3FI82 (IWORK,FVAL1,FDIFF1,NPTS,PDS,IGDS)
C
C             PRINT *,'GLAHN',FVAL1,FDIFF1
C             WRITE (6,FMT='(''  OUT FROM W3FI82 WITH = '',/,
C    &                  10(3X,10I6,/))') (IWORK(I),I=1,NPTS)
C
C             MUST NOW RE-REMOVE THE MINIMUM VALUE
C             OF THE SECOND DIFFERENCES TO ASSURE
C             ALL POSITIVE NUMBERS FOR SECOND ORDER GRIB PACKING
C
C             ORIGINAL REFERENCE VALUE ADDED TO FIRST POINT
C             VALUE FROM THE 2ND DIFF PACKER TO BE ADDED
C             BACK IN WHEN THE 2ND DIFF VALUES ARE
C             RECONSTRUCTED BACK TO THE BASIC VALUES
C
C             ALSO, THE REFERENCE VALUE IS
C             POWER-OF-TWO SCALED TO MATCH
C             FVAL1.  ALL OF THIS SCALING
C             WILL BE REMOVED AFTER THE
C             GLAHN SECOND DIFFERENCING IS UNDONE.
C             THE SCALING FACTOR NEEDED TO DO THAT
C             IS SAVED IN THE PDS AS A SIGNED POSITIVE
C             TWO BYTE INTEGER
C
C             THE SCALING FOR THE 2ND DIF PACKED
C             VALUES IS PROPERLY SET TO ZERO
C
              FVAL1 = FVAL1 + REFNCE*SCAL2
C                                          FIRST TEST TO SEE IF
C                                          ON 32 OR 64 BIT COMPUTER
              CALL W3FI01(LW)
              IF (LW.EQ.4) THEN
                  CALL W3FI76 (FVAL1,IEXP,IMANT,32)
              ELSE
                  CALL W3FI76 (FVAL1,IEXP,IMANT,64)
              END IF
              CALL SBYTE (PDS,IEXP,320,8)
              CALL SBYTE (PDS,IMANT,328,24)
C
              IF (LW.EQ.4) THEN
                  CALL W3FI76 (FDIFF1,IEXP,IMANT,32)
              ELSE
                  CALL W3FI76 (FDIFF1,IEXP,IMANT,64)
              END IF
              CALL SBYTE (PDS,IEXP,352,8)
              CALL SBYTE (PDS,IMANT,360,24)
C
C             TURN ISCAL2 INTO SIGNED POSITIVE INTEGER
C             AND STORE IN TWO BYTES
C
              IF(ISCAL2.GE.0)  THEN
                CALL SBYTE (PDS,ISCAL2,384,16)
              ELSE
                CALL SBYTE (PDS,1,384,1)
                ISCAL2 = - ISCAL2
                CALL SBYTE( PDS,ISCAL2,385,15)
              ENDIF
C
              MAX  = IWORK(1)
              MIN  = IWORK(1)
              DO 700 I = 2, NPTS
                  IF (IWORK(I).LT.MIN) THEN
                      MIN  = IWORK(I)
                  ELSE IF (IWORK(I).GT.MAX) THEN
                      MAX  = IWORK(I)
                  END IF
  700         CONTINUE
C                           EXTRACT MINIMA
              DO 710 I = 1, NPTS
                  IWORK(I)  = IWORK(I) - MIN
  710         CONTINUE
              REFNCE  = MIN
C             PRINT *,'710 REFERENCE',REFNCE
              ISCAL2 = 0
C
C             AND RESET VALUE OF KWIDE - THE BIT WIDTH
C             FOR THE RANGE OF THE VALUES
C
              IDIFF = MAX - MIN
              CALL FI7505 (IDIFF,KWIDE)
C
C             PRINT*,'BIT WIDTH (KWIDE) OF 2ND DIFFS', KWIDE
C
C  **************************** END OF GLAHN PACKING  ************
          ELSE IF (IBDSFL(2).EQ.1.AND.IBDSFL(7).EQ.0) THEN
C                        HAVE SECOND ORDER PACKING WITH NO SECOND ORDER
C                        BIT MAP. ERGO ROW BY ROW - COL BY COL
              CALL FI7503 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
     *              LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE,IGDS)
              RETURN
          END IF
C         WRITE (6,FMT='(''  CALL TO FI7501 WITH = '',/,
C    &                  10(3X,10I6,/))') (IWORK(I),I=1,NPTS)
C         WRITE (6,FMT='(''  END OF ARRAY = '',/,
C    &                  10(3X,10I6,/))') (IWORK(I),I=NPTS-5,NPTS)
C         PRINT*,' REFNCE,ISCAL2, KWIDE AT CALL TO FI7501',
C    &             REFNCE, ISCAL2,KWIDE
C
C                         SECOND ORDER PACKING
C
          CALL FI7501 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
     *             LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE)
C
C              BDS COMPLETELY ASSEMBLED IN FI7501 FOR SECOND ORDER
C              PACKING.
C
      ELSE
C                                      SIMPLE PACKING
C
C                PRINT*,'  SIMPLE FIRST ORDER PACKING...'
          IF (IBITL.EQ.0) THEN
C                PRINT*,' WITH VARIABLE BIT LENGTH'
C
C                  WITH VARIABLE BIT LENGTH, ADJUSTED
C                  TO ACCOMMODATE LARGEST VALUE
C                  BINARY SCALING ALWAYS = 0
C
              CALL W3FI58(IWORK,NPTS,IWORK,PFLD,NBITS,LEN,KMIN)
              RMIN   = KMIN
              REFNCE  = RMIN
              ISCALE = 0
C             PRINT*,'  BIT LENGTH CAME OUT AT ...',NBITS
C
C           SET CONST .TRUE. IF ALL VALUES ARE THE SAME
C
              IF (LEN.EQ.0.AND.NBITS.EQ.0) CONST = .TRUE.
C
          ELSE
C           PRINT*,' FIXED BIT LENGTH, IBITL = ', IBITL
C
C             FIXED BIT LENGTH PACKING (VARIABLE PRECISION)
C             VALUES SCALED BY POWER OF 2 (ISCALE) TO
C             FIT LARGEST VALUE INTO GIVEN BIT LENGTH (IBITL)
C
              CALL W3FI59(FWORK,NPTS,IBITL,IWORK,PFLD,ISCALE,LEN,RMIN)
              REFNCE = RMIN
C             PRINT *,' SCALING NEEDED TO FIT IS ...', ISCALE
              NBITS = IBITL
C
C           SET CONST .TRUE. IF ALL VALUES ARE THE SAME
C
              IF (LEN.EQ.0) THEN
                  CONST = .TRUE.
                  NBITS = 0
              END IF
          END IF
C
C         COMPUTE LENGTH OF BDS IN OCTETS
C
          INUM  = NPTS * NBITS + 88
C         PRINT *,'NUMBER OF BITS BEFORE FILL ADDED',INUM
C
C                  NUMBER OF FILL BITS
          NFILL  = 0
          NLEFT  = MOD(INUM,16)
          IF (NLEFT.NE.0) THEN
              INUM  = INUM + 16 - NLEFT
              NFILL = 16 - NLEFT
          END IF
C         PRINT *,'NUMBER OF BITS AFTER FILL ADDED',INUM
C                  LENGTH OF BDS IN BYTES
          LENBDS = INUM / 8
C
C                2.0   FORM THE BINARY DATA SECTION (BDS).
C
C                 CONCANTENATE ALL FIELDS FOR BDS
C
C                               BYTES 1-3
          CALL SBYTE (BDS11,LENBDS,0,24)
C
C                               BYTE  4
C                                       FLAGS
          CALL SBYTE (BDS11,IBDSFL(1),24,1)
          CALL SBYTE (BDS11,IBDSFL(2),25,1)
          CALL SBYTE (BDS11,IBDSFL(3),26,1)
          CALL SBYTE (BDS11,IBDSFL(4),27,1)
C                                        NR OF FILL BITS
          CALL SBYTE (BDS11,NFILL,28,4)
C
C       FILL OCTETS 5-6 WITH THE SCALE FACTOR.
C
C                               BYTE  5-6
          IF (ISCALE.LT.0) THEN
              CALL SBYTE (BDS11,1,32,1)
              ISCALE  = - ISCALE
              CALL SBYTE (BDS11,ISCALE,33,15)
          ELSE
              CALL SBYTE (BDS11,ISCALE,32,16)
          END IF
C
C   FILL OCTET 7-10 WITH THE REFERENCE VALUE
C   CONVERT THE FLOATING POINT OF YOUR MACHINE TO IBM370 32 BIT
C   FLOATING POINT NUMBER
C
C                               BYTE  7-10
C                                        REFERENCE VALUE
C                                          FIRST TEST TO SEE IF
C                                          ON 32 OR 64 BIT COMPUTER
          CALL W3FI01(LW)
          IF (LW.EQ.4) THEN
              CALL W3FI76 (REFNCE,IEXP,IMANT,32)
          ELSE
              CALL W3FI76 (REFNCE,IEXP,IMANT,64)
          END IF
          CALL SBYTE (BDS11,IEXP,48,8)
          CALL SBYTE (BDS11,IMANT,56,24)
C
C
C                         FILL OCTET 11 WITH THE NUMBER OF BITS.
C
C                               BYTE  11
          CALL SBYTE (BDS11,NBITS,80,8)
      END IF
C
      RETURN
      END
      SUBROUTINE FI7501 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
     *           LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI7501      BDS SECOND ORDER PACKING
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-08-06
C
C ABSTRACT: PERFORM SECONDARY PACKING ON GRID POINT DATA,
C   GENERATING ALL BDS INFORMATION.
C
C PROGRAM HISTORY LOG:
C   93-08-06  CAVANAUGH
C   93-12-15  CAVANAUGH   CORRECTED LOCATION OF START OF FIRST ORDER
C                         VALUES AND START OF SECOND ORDER VALUES TO
C                         REFLECT A BYTE LOCATION IN THE BDS INSTEAD
C                         OF AN OFFSET.
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C
C USAGE:    CALL FI7501 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
C    *           LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE)
C   INPUT ARGUMENT LIST:
C     IWORK    - INTEGER SOURCE ARRAY
C     NPTS     - NUMBER OF POINTS IN IWORK
C     IBDSFL   - FLAGS
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IPFLD    - CONTAINS BDS FROM BYTE 12 ON
C     BDS11    - CONTAINS FIRST 11 BYTES FOR BDS
C     LEN      - NUMBER OF BYTES FROM 12 ON
C     LENBDS   - TOTAL LENGTH OF BDS
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
      CHARACTER*1     BDS11(*),PDS(*)
C
      REAL            REFNCE
C
      INTEGER         ISCAL2,KWIDE
      INTEGER         LENBDS
      INTEGER         IPFLD(*)
      INTEGER         LEN,KBDS(22)
      INTEGER         IWORK(*)
C                        OCTET NUMBER IN SECTION, FIRST ORDER PACKING
C     INTEGER         KBDS(12)
C                        FLAGS
      INTEGER         IBDSFL(*)
C                        EXTENDED FLAGS
C     INTEGER         KBDS(14)
C                        OCTET NUMBER FOR SECOND ORDER PACKING
C     INTEGER         KBDS(15)
C                        NUMBER OF FIRST ORDER VALUES
C     INTEGER         KBDS(17)
C                        NUMBER OF SECOND ORDER PACKED VALUES
C     INTEGER         KBDS(19)
C                        WIDTH OF SECOND ORDER PACKING
      INTEGER         ISOWID(50000)
C                        SECONDARY BIT MAP
      INTEGER         ISOBMP(8200)
C                        FIRST ORDER PACKED VALUES
      INTEGER         IFOVAL(50000)
C                        SECOND ORDER PACKED VALUES
      INTEGER         ISOVAL(100000)
C
C     INTEGER         KBDS(11)
C                        BIT WIDTH TABLE
      INTEGER         IBITS(31)
C
      DATA            IBITS/1,3,7,15,31,63,127,255,511,1023,
     *                      2047,4095,8191,16383,32767,65535,131072,
     *                      262143,524287,1048575,2097151,4194303,
     *                      8388607,16777215,33554431,67108863,
     *                      134217727,268435455,536870911,
     *                      1073741823,2147483647/
C  ----------------------------------
C                       INITIALIZE ARRAYS
      DO 100 I = 1, 50000
          ISOWID(I)  = 0
          IFOVAL(I)  = 0
  100 CONTINUE
C
      DO 101 I = 1, 8200
          ISOBMP(I)  = 0
  101 CONTINUE
      DO 102 I = 1, 100000
          ISOVAL(I)  = 0
  102 CONTINUE
C                      INITIALIZE POINTERS
C                            SECONDARY BIT WIDTH POINTER
      IWDPTR  = 0
C                            SECONDARY BIT MAP POINTER
      IBMP2P  = 0
C                            FIRST ORDER VALUE POINTER
      IFOPTR  = 0
C                            BYTE POINTER TO START OF 1ST ORDER VALUES
      KBDS(12)  = 0
C                            BYTE POINTER TO START OF 2ND ORDER VALUES
      KBDS(15)  = 0
C                            TO CONTAIN NUMBER OF FIRST ORDER VALUES
      KBDS(17)  = 0
C                            TO CONTAIN NUMBER OF SECOND ORDER VALUES
      KBDS(19)  = 0
C                            SECOND ORDER PACKED VALUE POINTER
      ISOPTR  = 0
C  =======================================================
C
C                         DATA IS IN IWORK
C
      KBDS(11)  = KWIDE
C
C       DATA PACKING
C
      ITER    = 0
      INEXT   = 1
      ISTART  = 1
C  -----------------------------------------------------------
      KOUNT = 0
C     DO 1 I = 1, NPTS, 10
C         PRINT *,I,(IWORK(K),K=I, I+9)
C   1 CONTINUE
 2000 CONTINUE
      ITER  = ITER + 1
C     PRINT *,'NEXT ITERATION STARTS AT',ISTART
       IF (ISTART.GT.NPTS) THEN
           GO TO 4000
       ELSE IF (ISTART.EQ.NPTS) THEN
           KPTS    = 1
           MXDIFF  = 0
           GO TO 2200
       END IF
C
C                     LOOK FOR REPITITIONS OF A SINGLE VALUE
       CALL FI7502 (IWORK,ISTART,NPTS,ISAME)
       IF (ISAME.GE.15) THEN
           KOUNT = KOUNT + 1
C          PRINT *,'FI7501 - FOUND IDENTICAL SET OF ',ISAME
           MXDIFF  = 0
           KPTS    = ISAME
       ELSE
C
C                     LOOK FOR SETS OF VALUES IN TREND SELECTED RANGE
           CALL FI7513 (IWORK,ISTART,NPTS,NMAX,NMIN,INRNGE)
C          PRINT *,'ISTART  ',ISTART,' INRNGE',INRNGE,NMAX,NMIN
           IEND  = ISTART + INRNGE - 1
C          DO 2199 NM = ISTART, IEND, 10
C              PRINT *,'  ',(IWORK(NM+JK),JK=0,9)
C2199      CONTINUE
           MXDIFF  = NMAX - NMIN
           KPTS    = INRNGE
       END IF
 2200 CONTINUE
C     PRINT *,'                 RANGE ',MXDIFF,' MAX',NMAX,' MIN',NMIN
C                 INCREMENT NUMBER OF FIRST ORDER VALUES
      KBDS(17)  = KBDS(17) + 1
C                 ENTER FIRST ORDER VALUE
      IF (MXDIFF.GT.0) THEN
          DO 2220 LK = 0, KPTS-1
              IWORK(ISTART+LK)  = IWORK(ISTART+LK) - NMIN
 2220     CONTINUE
          CALL SBYTE (IFOVAL,NMIN,IFOPTR,KBDS(11))
      ELSE
          CALL SBYTE (IFOVAL,IWORK(ISTART),IFOPTR,KBDS(11))
      END IF
      IFOPTR  = IFOPTR + KBDS(11)
C                  PROCESS SECOND ORDER BIT WIDTH
      IF (MXDIFF.GT.0) THEN
          DO 2330 KWIDE = 1, 31
              IF (MXDIFF.LE.IBITS(KWIDE)) THEN
                  GO TO 2331
              END IF
 2330     CONTINUE
 2331     CONTINUE
      ELSE
          KWIDE  = 0
      END IF
      CALL SBYTE (ISOWID,KWIDE,IWDPTR,8)
      IWDPTR  = IWDPTR + 8
C         PRINT *,KWIDE,' IFOVAL=',NMIN,IWORK(ISTART),KPTS
C               IF KWIDE NE 0, SAVE SECOND ORDER VALUE
      IF (KWIDE.GT.0) THEN
          CALL SBYTES (ISOVAL,IWORK(ISTART),ISOPTR,KWIDE,0,KPTS)
          ISOPTR  = ISOPTR + KPTS * KWIDE
          KBDS(19)  = KBDS(19) + KPTS
C         PRINT *,'            SECOND ORDER VALUES'
C         PRINT *,(IWORK(ISTART+I),I=0,KPTS-1)
      END IF
C                 ADD TO SECOND ORDER BITMAP
      CALL SBYTE (ISOBMP,1,IBMP2P,1)
      IBMP2P  = IBMP2P + KPTS
      ISTART  = ISTART + KPTS
      GO TO 2000
C  --------------------------------------------------------------
 4000 CONTINUE
C     PRINT *,'THERE WERE ',ITER,' SECOND ORDER GROUPS'
C     PRINT *,'THERE WERE ',KOUNT,' STRINGS OF CONSTANTS'
C                 CONCANTENATE ALL FIELDS FOR BDS
C
C                   REMAINDER GOES INTO IPFLD
      IPTR  = 0
C                               BYTES 12-13
C                                          VALUE FOR N1
C                                          LEAVE SPACE FOR THIS
      IPTR   = IPTR + 16
C                               BYTE 14
C                                          EXTENDED FLAGS
      CALL SBYTE (IPFLD,IBDSFL(5),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(6),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(7),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(8),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(9),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(10),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(11),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(12),IPTR,1)
      IPTR  = IPTR + 1
C                               BYTES 15-16
C                 SKIP OVER VALUE  FOR N2
      IPTR  = IPTR + 16
C                               BYTES 17-18
C                                     P1
      CALL SBYTE (IPFLD,KBDS(17),IPTR,16)
      IPTR  = IPTR + 16
C                               BYTES 19-20
C                                   P2
      CALL SBYTE (IPFLD,KBDS(19),IPTR,16)
      IPTR  = IPTR + 16
C                               BYTE 21 - RESERVED LOCATION
      CALL SBYTE (IPFLD,0,IPTR,8)
      IPTR  = IPTR + 8
C                               BYTES 22 - ?
C                                      WIDTHS OF SECOND ORDER PACKING
      IX    = (IWDPTR + 32) / 32
      CALL SBYTES (IPFLD,ISOWID,IPTR,32,0,IX)
      IPTR  = IPTR + IWDPTR
C                                      SECONDARY BIT MAP
      IJ    = (IBMP2P + 32) / 32
      CALL SBYTES (IPFLD,ISOBMP,IPTR,32,0,IJ)
      IPTR  = IPTR + IBMP2P
      IF (MOD(IPTR,8).NE.0) THEN
          IPTR  = IPTR + 8 - MOD(IPTR,8)
      END IF
C                                         DETERMINE LOCATION FOR START
C                                         OF FIRST ORDER PACKED VALUES
      KBDS(12)  = IPTR / 8 + 12
C                                        STORE LOCATION
      CALL SBYTE (IPFLD,KBDS(12),0,16)
C                                     MOVE IN FIRST ORDER PACKED VALUES
      IPASS   = (IFOPTR + 32) / 32
      CALL SBYTES (IPFLD,IFOVAL,IPTR,32,0,IPASS)
      IPTR  = IPTR + IFOPTR
      IF (MOD(IPTR,8).NE.0) THEN
          IPTR  = IPTR + 8 - MOD(IPTR,8)
      END IF
C     PRINT *,'IFOPTR =',IFOPTR,' ISOPTR =',ISOPTR
C                DETERMINE LOCATION FOR START
C                     OF SECOND ORDER VALUES
      KBDS(15)  = IPTR / 8 + 12
C                                   SAVE LOCATION OF SECOND ORDER VALUES
      CALL SBYTE (IPFLD,KBDS(15),24,16)
C                  MOVE IN SECOND ORDER PACKED VALUES
      IX    = (ISOPTR + 32) / 32
      CALL SBYTES (IPFLD,ISOVAL,IPTR,32,0,IX)
      IPTR  = IPTR + ISOPTR
      NLEFT  = MOD(IPTR+88,16)
      IF (NLEFT.NE.0) THEN
          NLEFT  = 16 - NLEFT
          IPTR   = IPTR + NLEFT
      END IF
C                                COMPUTE LENGTH OF DATA PORTION
      LEN     = IPTR / 8
C                                    COMPUTE LENGTH OF BDS
      LENBDS  = LEN + 11
C  -----------------------------------
C                               BYTES 1-3
C                                   THIS FUNCTION COMPLETED BELOW
C                                   WHEN LENGTH OF BDS IS KNOWN
      CALL SBYTE (BDS11,LENBDS,0,24)
C                               BYTE  4
      CALL SBYTE (BDS11,IBDSFL(1),24,1)
      CALL SBYTE (BDS11,IBDSFL(2),25,1)
      CALL SBYTE (BDS11,IBDSFL(3),26,1)
      CALL SBYTE (BDS11,IBDSFL(4),27,1)
C                              ENTER NUMBER OF FILL BITS
      CALL SBYTE (BDS11,NLEFT,28,4)
C                               BYTE  5-6
      IF (ISCAL2.LT.0) THEN
          CALL SBYTE (BDS11,1,32,1)
          ISCAL2 = - ISCAL2
      ELSE
          CALL SBYTE (BDS11,0,32,1)
      END IF
      CALL SBYTE (BDS11,ISCAL2,33,15)
C
C   FILL OCTET 7-10 WITH THE REFERENCE VALUE
C   CONVERT THE FLOATING POINT OF YOUR MACHINE TO IBM370 32 BIT
C   FLOATING POINT NUMBER
C                                        REFERENCE VALUE
C                                          FIRST TEST TO SEE IF
C                                          ON 32 OR 64 BIT COMPUTER
          CALL W3FI01(LW)
          IF (LW.EQ.4) THEN
              CALL W3FI76 (REFNCE,IEXP,IMANT,32)
          ELSE
              CALL W3FI76 (REFNCE,IEXP,IMANT,64)
          END IF
          CALL SBYTE (BDS11,IEXP,48,8)
          CALL SBYTE (BDS11,IMANT,56,24)
C
C                               BYTE  11
C
      CALL SBYTE (BDS11,KBDS(11),80,8)
C
      RETURN
      END
      SUBROUTINE FI7502 (IWORK,ISTART,NPTS,ISAME)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI7502      SECOND ORDER SAME VALUE COLLECTION
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-06-23
C
C ABSTRACT: COLLECT SEQUENTIAL SAME VALUES FOR PROCESSING
C   AS SECOND ORDER VALUE FOR GRIB MESSAGES.
C
C PROGRAM HISTORY LOG:
C   93-06-23  CAVANAUGH
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C
C USAGE:    CALL FI7502 (IWORK,ISTART,NPTS,ISAME)
C   INPUT ARGUMENT LIST:
C     IWORK    - ARRAY CONTAINING SOURCE DATA
C     ISTART   - STARTING LOCATION FOR THIS TEST
C     NPTS     - NUMBER OF POINTS IN IWORK
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     ISAME    - NUMBER OF SEQUENTIAL POINTS HAVING THE SAME VALUE
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
      INTEGER        IWORK(*)
      INTEGER        ISTART
      INTEGER        ISAME
      INTEGER        K
      INTEGER        NPTS
C  -------------------------------------------------------------
      ISAME  = 0
      DO 100 K = ISTART, NPTS
          IF (IWORK(K).NE.IWORK(ISTART)) THEN
              RETURN
          END IF
          ISAME  = ISAME + 1
  100 CONTINUE
      RETURN
      END
      SUBROUTINE FI7503 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
     *           LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE,IGDS)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI7501      ROW BY ROW, COL BY COL PACKING
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 94-05-20
C
C ABSTRACT: PERFORM ROW BY ROW OR COLUMN BY COLUMN PACKING
C   GENERATING ALL BDS INFORMATION.
C
C PROGRAM HISTORY LOG:
C   93-08-06  CAVANAUGH
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C
C USAGE:    CALL FI7503 (IWORK,IPFLD,NPTS,IBDSFL,BDS11,
C    *           LEN,LENBDS,PDS,REFNCE,ISCAL2,KWIDE,IGDS)
C   INPUT ARGUMENT LIST:
C     IWORK    - INTEGER SOURCE ARRAY
C     NPTS     - NUMBER OF POINTS IN IWORK
C     IBDSFL   - FLAGS
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IPFLD    - CONTAINS BDS FROM BYTE 12 ON
C     BDS11    - CONTAINS FIRST 11 BYTES FOR BDS
C     LEN      - NUMBER OF BYTES FROM 12 ON
C     LENBDS   - TOTAL LENGTH OF BDS
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
      CHARACTER*1     BDS11(*),PDS(*)
C
      REAL            REFNCE
C
      INTEGER         ISCAL2,KWIDE
      INTEGER         LENBDS
      INTEGER         IPFLD(*),IGDS(*)
      INTEGER         LEN,KBDS(22)
      INTEGER         IWORK(*)
C                        OCTET NUMBER IN SECTION, FIRST ORDER PACKING
C     INTEGER         KBDS(12)
C                        FLAGS
      INTEGER         IBDSFL(*)
C                        EXTENDED FLAGS
C     INTEGER         KBDS(14)
C                        OCTET NUMBER FOR SECOND ORDER PACKING
C     INTEGER         KBDS(15)
C                        NUMBER OF FIRST ORDER VALUES
C     INTEGER         KBDS(17)
C                        NUMBER OF SECOND ORDER PACKED VALUES
C     INTEGER         KBDS(19)
C                        WIDTH OF SECOND ORDER PACKING
      INTEGER         ISOWID(50000)
C                        SECONDARY BIT MAP
      INTEGER         ISOBMP(8200)
C                        FIRST ORDER PACKED VALUES
      INTEGER         IFOVAL(50000)
C                        SECOND ORDER PACKED VALUES
      INTEGER         ISOVAL(100000)
C
C     INTEGER         KBDS(11)
C  ----------------------------------
C                       INITIALIZE ARRAYS
      DO 100 I = 1, 50000
          ISOWID(I)  = 0
          IFOVAL(I)  = 0
  100 CONTINUE
C
      DO 101 I = 1, 8200
          ISOBMP(I)  = 0
  101 CONTINUE
      DO 102 I = 1, 100000
          ISOVAL(I)  = 0
  102 CONTINUE
C                      INITIALIZE POINTERS
C                            SECONDARY BIT WIDTH POINTER
      IWDPTR  = 0
C                            SECONDARY BIT MAP POINTER
      IBMP2P  = 0
C                            FIRST ORDER VALUE POINTER
      IFOPTR  = 0
C                            BYTE POINTER TO START OF 1ST ORDER VALUES
      KBDS(12)  = 0
C                            BYTE POINTER TO START OF 2ND ORDER VALUES
      KBDS(15)  = 0
C                            TO CONTAIN NUMBER OF FIRST ORDER VALUES
      KBDS(17)  = 0
C                            TO CONTAIN NUMBER OF SECOND ORDER VALUES
      KBDS(19)  = 0
C                            SECOND ORDER PACKED VALUE POINTER
      ISOPTR  = 0
C  =======================================================
C                         BUILD SECOND ORDER BIT MAP IN EITHER
C                         ROW BY ROW OR COL BY COL FORMAT
      IF (IAND(IGDS(13),32).NE.0) THEN
C                              COLUMN BY COLUMN
          KOUT  = IGDS(4)
          KIN   = IGDS(5)
C         PRINT *,'COLUMN BY COLUMN',KOUT,KIN
      ELSE
C                              ROW BY ROW
          KOUT  = IGDS(5)
          KIN   = IGDS(4)
C         PRINT *,'ROW BY ROW',KOUT,KIN
      END IF
      KBDS(17)  = KOUT
      KBDS(19)  = NPTS
C
C     DO 4100 J = 1, NPTS, 53
C         WRITE (6,4101) (IWORK(K),K=J,J+52)
 4101     FORMAT (1X,25I4)
C         PRINT *,' '
C4100 CONTINUE
C
C                             INITIALIZE BIT MAP POINTER
      IBMP2P = 0
C                             CONSTRUCT WORKING BIT MAP
      DO 2000 I = 1, KOUT
          DO 1000 J = 1, KIN
              IF (J.EQ.1) THEN
                  CALL SBYTE (ISOBMP,1,IBMP2P,1)
              ELSE
                  CALL SBYTE (ISOBMP,0,IBMP2P,1)
              END IF
              IBMP2P  = IBMP2P + 1
 1000     CONTINUE
 2000 CONTINUE
      LEN  = IBMP2P / 32 + 1
C     CALL BINARY(ISOBMP,LEN)
C
C                       PROCESS OUTER LOOP OF ROW BY ROW OR COL BY COL
C
      KPTR  = 1
      KBDS(11)  = KWIDE
      DO 6000 I = 1, KOUT
C                       IN CURRENT ROW OR COL
C                              FIND FIRST ORDER VALUE
          JPTR  = KPTR
          LOWEST  = IWORK(JPTR)
          DO 4000 J = 1, KIN
              IF (IWORK(JPTR).LT.LOWEST) THEN
                  LOWEST = IWORK(JPTR)
              END IF
              JPTR  = JPTR + 1
 4000     CONTINUE
C                            SAVE FIRST ORDER VALUE
          CALL SBYTE (IFOVAL,LOWEST,IFOPTR,KWIDE)
          IFOPTR  = IFOPTR + KWIDE
C         PRINT *,'FOVAL',I,LOWEST,KWIDE
C                            SUBTRACT FIRST ORDER VALUE FROM OTHER VALS
C                                         GETTING SECOND ORDER VALUES
          JPTR  = KPTR
          IBIG  = IWORK(JPTR) - LOWEST
          DO 4200 J = 1, KIN
              IWORK(JPTR)  = IWORK(JPTR) - LOWEST
              IF (IWORK(JPTR).GT.IBIG) THEN
                  IBIG  = IWORK(JPTR)
              END IF
              JPTR  = JPTR + 1
 4200     CONTINUE
C                            HOW MANY BITS TO CONTAIN LARGEST SECOND
C                                         ORDER VALUE IN SEGMENT
          CALL FI7505 (IBIG,NWIDE)
C                            SAVE BIT WIDTH
          CALL SBYTE (ISOWID,NWIDE,IWDPTR,8)
          IWDPTR  = IWDPTR + 8
C         PRINT *,I,'SOVAL',IBIG,' IN',NWIDE,' BITS'
C         WRITE (6,4101) (IWORK(K),K=KPTR,KPTR+52)
C                            SAVE SECOND ORDER VALUES OF THIS SEGMENT
          DO 5000 J = 0, KIN-1
              CALL SBYTE (ISOVAL,IWORK(KPTR+J),ISOPTR,NWIDE)
              ISOPTR  = ISOPTR + NWIDE
 5000     CONTINUE
          KPTR    = KPTR + KIN
 6000 CONTINUE
C  =======================================================
C                 CONCANTENATE ALL FIELDS FOR BDS
C
C                   REMAINDER GOES INTO IPFLD
      IPTR  = 0
C                               BYTES 12-13
C                                          VALUE FOR N1
C                                          LEAVE SPACE FOR THIS
      IPTR   = IPTR + 16
C                               BYTE 14
C                                          EXTENDED FLAGS
      CALL SBYTE (IPFLD,IBDSFL(5),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(6),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(7),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(8),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(9),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(10),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(11),IPTR,1)
      IPTR  = IPTR + 1
      CALL SBYTE (IPFLD,IBDSFL(12),IPTR,1)
      IPTR  = IPTR + 1
C                               BYTES 15-16
C                 SKIP OVER VALUE  FOR N2
      IPTR  = IPTR + 16
C                               BYTES 17-18
C                                     P1
      CALL SBYTE (IPFLD,KBDS(17),IPTR,16)
      IPTR  = IPTR + 16
C                               BYTES 19-20
C                                   P2
      CALL SBYTE (IPFLD,KBDS(19),IPTR,16)
      IPTR  = IPTR + 16
C                               BYTE 21 - RESERVED LOCATION
      CALL SBYTE (IPFLD,0,IPTR,8)
      IPTR  = IPTR + 8
C                               BYTES 22 - ?
C                                      WIDTHS OF SECOND ORDER PACKING
      IX    = (IWDPTR + 32) / 32
      CALL SBYTES (IPFLD,ISOWID,IPTR,32,0,IX)
      IPTR  = IPTR + IWDPTR
C     PRINT *,'ISOWID',IWDPTR,IX
C     CALL BINARY (ISOWID,IX)
C
C                     NO SECONDARY BIT MAP

C                                         DETERMINE LOCATION FOR START
C                                         OF FIRST ORDER PACKED VALUES
      KBDS(12)  = IPTR / 8 + 12
C                                        STORE LOCATION
      CALL SBYTE (IPFLD,KBDS(12),0,16)
C                                     MOVE IN FIRST ORDER PACKED VALUES
      IPASS   = (IFOPTR + 32) / 32
      CALL SBYTES (IPFLD,IFOVAL,IPTR,32,0,IPASS)
      IPTR  = IPTR + IFOPTR
C     PRINT *,'IFOVAL',IFOPTR,IPASS,KWIDE
C     CALL BINARY (IFOVAL,IPASS)
      IF (MOD(IPTR,8).NE.0) THEN
          IPTR  = IPTR + 8 - MOD(IPTR,8)
      END IF
C     PRINT *,'IFOPTR =',IFOPTR,' ISOPTR =',ISOPTR
C                DETERMINE LOCATION FOR START
C                     OF SECOND ORDER VALUES
      KBDS(15)  = IPTR / 8 + 12
C                                   SAVE LOCATION OF SECOND ORDER VALUES
      CALL SBYTE (IPFLD,KBDS(15),24,16)
C                  MOVE IN SECOND ORDER PACKED VALUES
      IX    = (ISOPTR + 32) / 32
      CALL SBYTES (IPFLD,ISOVAL,IPTR,32,0,IX)
      IPTR  = IPTR + ISOPTR
C     PRINT *,'ISOVAL',ISOPTR,IX
C     CALL BINARY (ISOVAL,IX)
      NLEFT  = MOD(IPTR+88,16)
      IF (NLEFT.NE.0) THEN
          NLEFT  = 16 - NLEFT
          IPTR   = IPTR + NLEFT
      END IF
C                                COMPUTE LENGTH OF DATA PORTION
      LEN     = IPTR / 8
C                                    COMPUTE LENGTH OF BDS
      LENBDS  = LEN + 11
C  -----------------------------------
C                               BYTES 1-3
C                                   THIS FUNCTION COMPLETED BELOW
C                                   WHEN LENGTH OF BDS IS KNOWN
      CALL SBYTE (BDS11,LENBDS,0,24)
C                               BYTE  4
      CALL SBYTE (BDS11,IBDSFL(1),24,1)
      CALL SBYTE (BDS11,IBDSFL(2),25,1)
      CALL SBYTE (BDS11,IBDSFL(3),26,1)
      CALL SBYTE (BDS11,IBDSFL(4),27,1)
C                              ENTER NUMBER OF FILL BITS
      CALL SBYTE (BDS11,NLEFT,28,4)
C                               BYTE  5-6
      IF (ISCAL2.LT.0) THEN
          CALL SBYTE (BDS11,1,32,1)
          ISCAL2 = - ISCAL2
      ELSE
          CALL SBYTE (BDS11,0,32,1)
      END IF
      CALL SBYTE (BDS11,ISCAL2,33,15)
C
C   FILL OCTET 7-10 WITH THE REFERENCE VALUE
C   CONVERT THE FLOATING POINT OF YOUR MACHINE TO IBM370 32 BIT
C   FLOATING POINT NUMBER
C                                        REFERENCE VALUE
C                                          FIRST TEST TO SEE IF
C                                          ON 32 OR 64 BIT COMPUTER
      CALL W3FI01(LW)
      IF (LW.EQ.4) THEN
          CALL W3FI76 (REFNCE,IEXP,IMANT,32)
      ELSE
          CALL W3FI76 (REFNCE,IEXP,IMANT,64)
      END IF
      CALL SBYTE (BDS11,IEXP,48,8)
      CALL SBYTE (BDS11,IMANT,56,24)
C
C                               BYTE  11
C
      CALL SBYTE (BDS11,KBDS(11),80,8)
C
      KLEN  = LENBDS / 4 + 1
C     PRINT *,'BDS11 LISTING',4,LENBDS
C     CALL BINARY (BDS11,4)
C     PRINT *,'IPFLD LISTING'
C     CALL BINARY (IPFLD,KLEN)
      RETURN
      END
      SUBROUTINE FI7505 (N,NBITS)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI7505      DETERMINE NUMBER OF BITS TO CONTAIN VALUE
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-06-23
C
C ABSTRACT: CALCULATE NUMBER OF BITS TO CONTAIN VALUE N, WITH A
C            MAXIMUM OF 32 BITS.
C
C PROGRAM HISTORY LOG:
C   93-06-23  CAVANAUGH
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C
C USAGE:    CALL FI7505 (N,NBITS)
C   INPUT ARGUMENT LIST:
C     N        - INTEGER VALUE
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     NBITS    - NUMBER OF BITS TO CONTAIN N
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
      INTEGER        N,NBITS
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
C
      DO 1000 NBITS = 1, 31
          IF (N.LE.IBITS(NBITS)) THEN
              RETURN
          END IF
 1000 CONTINUE
      RETURN
      END
      SUBROUTINE FI7513 (IWORK,ISTART,NPTS,MAX,MIN,INRNGE)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI7513      SELECT BLOCK OF DATA FOR PACKING
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 94-01-21
C
C ABSTRACT: SELECT A BLOCK OF DATA FOR PACKING
C
C PROGRAM HISTORY LOG:
C   94-01-21  CAVANAUGH
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C
C USAGE:    CALL FI7513 (IWORK,ISTART,NPTS,MAX,MIN,INRNGE)
C   INPUT ARGUMENT LIST:
C     *        - RETURN ADDRESS IF ENCOUNTER SET OF SAME VALUES
C     IWORK    -
C     ISTART   -
C     NPTS     -
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     MAX      -
C     MIN      -
C     INRNGE   -
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
      INTEGER        IWORK(*),NPTS,ISTART,INRNGE,INRNGA,INRNGB
      INTEGER        MAX,MIN,MXVAL,MAXB,MINB,MXVALB
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
C                        IDENTIFY NEXT BLOCK OF DATA FOR PACKING AND
C                           RETURN TO CALLER
C  ********************************************************************
      ISTRTA  = ISTART
C
C                     GET BLOCK A
      CALL FI7516 (IWORK,NPTS,INRNGA,ISTRTA,
     *                                  MAX,MIN,MXVAL,LWIDE)
C  ********************************************************************
C
      ISTRTB  = ISTRTA + INRNGA
 2000 CONTINUE
C                         IF HAVE PROCESSED ALL DATA, RETURN
      IF (ISTRTB.GT.NPTS) THEN
C                         NO MORE DATA TO LOOK AT
          INRNGE  = INRNGA
          RETURN
      END IF
C                     GET BLOCK B
      CALL FI7502 (IWORK,ISTRTB,NPTS,ISAME)
      IF (ISAME.GE.15) THEN
C         PRINT *,'BLOCK B HAS ALL IDENTICAL VALUES'
C         PRINT *,'BLOCK A HAS INRNGE =',INRNGA
C                     BLOCK B CONTAINS ALL IDENTICAL VALUES
          INRNGE  = INRNGA
C                     EXIT WITH BLOCK A
          RETURN
      END IF
C                     GET BLOCK B
C
      ISTRTB  = ISTRTA + INRNGA
      CALL FI7516 (IWORK,NPTS,INRNGB,ISTRTB,
     *                                  MAXB,MINB,MXVALB,LWIDEB)
C     PRINT *,'BLOCK A',INRNGA,' BLOCK B',INRNGB
C  ********************************************************************
C                     PERFORM TREND ANALYSIS TO DETERMINE
C                     IF DATA COLLECTION CAN BE IMPROVED
C
      KTRND  = LWIDE - LWIDEB
C     PRINT *,'TREND',LWIDE,LWIDEB
      IF (KTRND.LE.0) THEN
C         PRINT *,'BLOCK A - SMALLER, SHOULD EXTEND INTO BLOCK B'
          MXVAL   = IBITS(LWIDE)
C
C                     IF BLOCK A REQUIRES THE SAME OR FEWER BITS
C                             LOOK AHEAD
C                        AND GATHER THOSE DATA POINTS THAT CAN
C                        BE RETAINED IN BLOCK A
C                        BECAUSE THIS BLOCK OF DATA
C                            USES FEWER BITS
C
          CALL FI7518 (IRET,IWORK,NPTS,ISTRTA,INRNGA,INRNGB,
     *                          MAX,MIN,LWIDE,MXVAL)
          IF(IRET.EQ.1) GO TO 8000
C         PRINT *,'18 INRNGA IS NOW ',INRNGA
          IF (INRNGB.LT.20) THEN
              RETURN
          ELSE
              GO TO 2000
          END IF
      ELSE
C         PRINT *,'BLOCK A - LARGER, B SHOULD EXTEND BACK INTO A'
          MXVALB  = IBITS(LWIDEB)
C
C                     IF BLOCK B REQUIRES FEWER BITS
C                             LOOK BACK
C                            SHORTEN BLOCK A BECAUSE NEXT BLOCK OF DATA
C                            USES FEWER BITS
C
          CALL FI7517 (IRET,IWORK,NPTS,ISTRTB,INRNGA,
     *                               MAXB,MINB,LWIDEB,MXVALB)
          IF(IRET.EQ.1) GO TO 8000
C         PRINT *,'17 INRNGA IS NOW ',INRNGA
      END IF
C
C                           PACK UP BLOCK A
C                           UPDATA POINTERS
 8000 CONTINUE
      INRNGE  = INRNGA
C                           GET NEXT BLOCK A
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE FI7516 (IWORK,NPTS,INRNG,ISTART,MAX,MIN,MXVAL,LWIDTH)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI7516      SCAN NUMBER OF POINTS
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 94-01-21
C
C ABSTRACT: SCAN FORWARD FROM CURRENT POSITION. COLLECT POINTS AND
C           DETERMINE MAXIMUM AND MINIMUM VALUES AND THE NUMBER
C           OF POINTS THAT ARE INCLUDED. FORWARD SEARCH IS TERMINATED
C           BY ENCOUNTERING A SET OF IDENTICAL VALUES, BY REACHING
C           THE NUMBER OF POINTS SELECTED OR BY REACHING THE END
C           OF DATA.
C
C PROGRAM HISTORY LOG:
C   94-01-21  CAVANAUGH
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C
C USAGE:    CALL FI7516 (IWORK,NPTS,INRNG,ISTART,MAX,MIN,MXVAL,LWIDTH)
C   INPUT ARGUMENT LIST:
C     *        - RETURN ADDRESS IF ENCOUNTER SET OF SAME VALUES
C     IWORK    - DATA ARRAY
C     NPTS     - NUMBER OF POINTS IN DATA ARRAY
C     ISTART   - STARTING LOCATION IN DATA
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     INRNG    - NUMBER OF POINTS SELECTED
C     MAX      - MAXIMUM VALUE OF POINTS
C     MIN      - MINIMUM VALUE OF POINTS
C     MXVAL    - MAXIMUM VALUE THAT CAN BE CONTAINED IN LWIDTH BITS
C     LWIDTH   - NUMBER OF BITS TO CONTAIN MAX DIFF
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
      INTEGER        IWORK(*),NPTS,ISTART,INRNG,MAX,MIN,LWIDTH,MXVAL
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
C
      INRNG  = 1
      JQ        = ISTART + 19
      MAX       = IWORK(ISTART)
      MIN       = IWORK(ISTART)
      DO 1000 I = ISTART+1, JQ
          CALL FI7502 (IWORK,I,NPTS,ISAME)
          IF (ISAME.GE.15) THEN
              GO TO 5000
          END IF
          INRNG  = INRNG + 1
          IF (IWORK(I).GT.MAX) THEN
              MAX  = IWORK(I)
          ELSE IF (IWORK(I).LT.MIN) THEN
              MIN  = IWORK(I)
          END IF
 1000 CONTINUE
 5000 CONTINUE
      KRNG   = MAX - MIN
C
      DO 9000 LWIDTH = 1, 31
          IF (KRNG.LE.IBITS(LWIDTH)) THEN
C             PRINT *,'RETURNED',INRNG,' VALUES'
              RETURN
          END IF
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE FI7517 (IRET,IWORK,NPTS,ISTRTB,INRNGA,
     *                           MAXB,MINB,MXVALB,LWIDEB)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI7517      SCAN BACKWARD
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 94-01-21
C
C ABSTRACT: SCAN BACKWARDS UNTIL A VALUE EXCEEDS RANGE OF GROUP B
C           THIS MAY SHORTEN GROUP A
C
C PROGRAM HISTORY LOG:
C   94-01-21  CAVANAUGH
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C   98-06-17  IREDELL     REMOVED ALTERNATE RETURN
C
C USAGE:    CALL FI7517 (IRET,IWORK,NPTS,ISTRTB,INRNGA,
C    *                           MAXB,MINB,MXVALB,LWIDEB)
C   INPUT ARGUMENT LIST:
C     IWORK    -
C     ISTRTB   -
C     NPTS     -
C     INRNGA   -
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IRET     -
C     JLAST    -
C     MAXB     -
C     MINB     -
C     LWIDTH   - NUMBER OF BITS TO CONTAIN MAX DIFF
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
      INTEGER        IWORK(*),NPTS,ISTRTB,INRNGA
      INTEGER        MAXB,MINB,LWIDEB,MXVALB
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
      IRET=0
C     PRINT *,'          FI7517'
      NPOS  = ISTRTB - 1
      ITST  = 0
      KSET  = INRNGA
C
 1000 CONTINUE
C     PRINT *,'TRY NPOS',NPOS,IWORK(NPOS),MAXB,MINB
      ITST  = ITST + 1
      IF (ITST.LE.KSET) THEN
          IF (IWORK(NPOS).GT.MAXB) THEN
              IF ((IWORK(NPOS)-MINB).GT.MXVALB) THEN
C                 PRINT *,'WENT OUT OF RANGE AT',NPOS
                  IRET=1
                  RETURN
              ELSE
                  MAXB    = IWORK(NPOS)
              END IF
          ELSE IF (IWORK(NPOS).LT.MINB) THEN
              IF ((MAXB-IWORK(NPOS)).GT.MXVALB) THEN
C                 PRINT *,'WENT OUT OF RANGE AT',NPOS
                  IRET=1
                  RETURN
              ELSE
                  MINB    = IWORK(NPOS)
              END IF
          END IF
          INRNGA  = INRNGA - 1
          NPOS  = NPOS - 1
          GO TO 1000
      END IF
C  ----------------------------------------------------------------
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE FI7518 (IRET,IWORK,NPTS,ISTRTA,INRNGA,INRNGB,
     *                          MAXA,MINA,LWIDEA,MXVALA)
C $$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI7518      SCAN FORWARD
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 94-01-21
C
C ABSTRACT: SCAN FORWARD FROM START OF BLOCK B TOWARDS END OF BLOCK B
C           IF NEXT POINT UNDER TEST FORCES A LARGER MAXVALA THEN
C           TERMINATE INDICATING LAST POINT TESTED FOR INCLUSION
C           INTO BLOCK A.
C
C PROGRAM HISTORY LOG:
C   94-01-21  CAVANAUGH
C   95-10-31  IREDELL     REMOVED SAVES AND PRINTS
C   98-06-17  IREDELL     REMOVED ALTERNATE RETURN
C
C USAGE:    CALL FI7518 (IRET,IWORK,NPTS,ISTRTA,INRNGA,INRNGB,
C     *                          MAXA,MINA,LWIDEA,MXVALA)
C   INPUT ARGUMENT LIST:
C     IFLD     -
C     JSTART   -
C     NPTS     -
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IRET     -
C     JLAST    -
C     MAX      -
C     MIN      -
C     LWIDTH   - NUMBER OF BITS TO CONTAIN MAX DIFF
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: IBM VS FORTRAN 77, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916/256, Y-MP8/64, Y-MP EL92/256
C
C $$
      INTEGER        IWORK(*),NPTS,ISTRTA,INRNGA
      INTEGER        MAXA,MINA,LWIDEA,MXVALA
      INTEGER        IBITS(31)
C
      DATA           IBITS/1,3,7,15,31,63,127,255,511,1023,2047,
     *               4095,8191,16383,32767,65535,131071,262143,
     *               524287,1048575,2097151,4194303,8388607,
     *               16777215,33554431,67108863,134217727,268435455,
     *               536870911,1073741823,2147483647/
C  ----------------------------------------------------------------
      IRET=0
C     PRINT *,'          FI7518'
      NPOS  = ISTRTA + INRNGA
      ITST  = 0
C
 1000 CONTINUE
      ITST  = ITST + 1
      IF (ITST.LE.INRNGB) THEN
C         PRINT *,'TRY NPOS',NPOS,IWORK(NPOS),MAXA,MINA
          IF (IWORK(NPOS).GT.MAXA) THEN
              IF ((IWORK(NPOS)-MINA).GT.MXVALA) THEN
C                 PRINT *,'FI7518A -',ITST,' RANGE EXCEEDS MAX'
                  IRET=1
                  RETURN
              ELSE
                  MAXA    = IWORK(NPOS)
              END IF
          ELSE IF (IWORK(NPOS).LT.MINA) THEN
              IF ((MAXA-IWORK(NPOS)).GT.MXVALA) THEN
C                 PRINT *,'FI7518B -',ITST,' RANGE EXCEEDS MAX'
                  IRET=1
                  RETURN
              ELSE
                  MINA    = IWORK(NPOS)
              END IF
          END IF
          INRNGA  = INRNGA + 1
C         PRINT *,'               ',ITST,INRNGA
          NPOS  = NPOS +1
          GO TO 1000
      END IF
C  ----------------------------------------------------------------
 9000 CONTINUE
      RETURN
      END
