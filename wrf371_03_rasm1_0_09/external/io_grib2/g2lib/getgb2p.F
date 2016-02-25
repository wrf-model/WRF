C-----------------------------------------------------------------------
      SUBROUTINE GETGB2P(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &                   EXTRACT,K,GRIBM,LENG,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETGB2P        FINDS AND EXTRACTS A GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 94-04-01
C
C ABSTRACT: FIND AND EXTRACTS A GRIB MESSAGE FROM A FILE.
C   READ A GRIB INDEX FILE (OR OPTIONALLY THE GRIB FILE ITSELF)
C   TO GET THE INDEX BUFFER (I.E. TABLE OF CONTENTS) FOR THE GRIB FILE.
C   FIND IN THE INDEX BUFFER A REFERENCE TO THE GRIB FIELD REQUESTED.
C   THE GRIB FIELD REQUEST SPECIFIES THE NUMBER OF FIELDS TO SKIP
C   AND THE UNPACKED IDENTIFICATION SECTION, GRID DEFINITION TEMPLATE AND
C   PRODUCT DEFINTION SECTION PARAMETERS.  (A REQUESTED PARAMETER
C   OF -9999 MEANS TO ALLOW ANY VALUE OF THIS PARAMETER TO BE FOUND.)
C   IF THE REQUESTED GRIB FIELD IS FOUND, THEN IT IS READ FROM THE
C   GRIB FILE AND RETURNED. 
C   IF THE GRIB FIELD IS NOT FOUND, THEN THE RETURN CODE WILL BE NONZERO.
C
C PROGRAM HISTORY LOG:
C   94-04-01  IREDELL
C   95-10-31  IREDELL     MODULARIZED PORTIONS OF CODE INTO SUBPROGRAMS
C                         AND ALLOWED FOR UNSPECIFIED INDEX FILE
C 2002-01-11  GILBERT     MODIFIED FROM GETGB AND GETGBM TO WORK WITH GRIB2
C 2003-12-17  GILBERT     MODIFIED FROM GETGB2 TO RETURN PACKED GRIB2 MESSAGE.
C
C USAGE:    CALL GETGB2P(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
C    &                  EXTRACT,K,GRIBM,LENG,IRET)
C   INPUT ARGUMENTS:
C     LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE.
C                  FILE MUST BE OPENED WITH BAOPEN OR BAOPENR BEFORE CALLING 
C                  THIS ROUTINE.
C     LUGI         INTEGER UNIT OF THE UNBLOCKED GRIB INDEX FILE.
C                  IF NONZERO, FILE MUST BE OPENED WITH BAOPEN BAOPENR BEFORE 
C                  CALLING THIS ROUTINE.
C                  (=0 TO GET INDEX BUFFER FROM THE GRIB FILE)
C     J            INTEGER NUMBER OF FIELDS TO SKIP
C                  (=0 TO SEARCH FROM BEGINNING)
C     JDISC        GRIB2 DISCIPLINE NUMBER OF REQUESTED FIELD
C                  ( IF = -1, ACCEPT ANY DISCIPLINE)
C                  ( SEE CODE TABLE 0.0 )
C                  0 - Meteorological products
C                  1 - Hydrological products
C                  2 - Land surface products
C                  3 - Space products
C                  10 - Oceanographic products
C     JIDS()       INTEGER ARRAY OF VALUES IN THE IDENTIFICATION SECTION
C                  (=-9999 FOR WILDCARD)
C            JIDS(1)   = IDENTIFICATION OF ORIGINATING CENTRE
C                         ( SEE COMMON CODE TABLE C-1 )
C            JIDS(2)   = IDENTIFICATION OF ORIGINATING SUB-CENTRE
C            JIDS(3)   = GRIB MASTER TABLES VERSION NUMBER
C                         ( SEE CODE TABLE 1.0 )
C                       0 - Experimental
C                       1 - Initial operational version number
C            JIDS(4)   = GRIB LOCAL TABLES VERSION NUMBER
C                         ( SEE CODE TABLE 1.1 )
C                       0     - Local tables not used
C                       1-254 - Number of local tables version used
C            JIDS(5)   = SIGNIFICANCE OF REFERENCE TIME (CODE TABLE 1.2)
C                       0 - Analysis
C                       1 - Start of forecast
C                       2 - Verifying time of forecast
C                       3 - Observation time
C            JIDS(6)   = YEAR ( 4 DIGITS )
C            JIDS(7)   = MONTH
C            JIDS(8)   = DAY
C            JIDS(9)   = HOUR
C            JIDS(10)  = MINUTE
C            JIDS(11)  = SECOND
C            JIDS(12)  = PRODUCTION STATUS OF PROCESSED DATA
C                         ( SEE CODE TABLE 1.3 )
C                       0 - Operational products
C                       1 - Operational test products
C                       2 - Research products
C                       3 - Re-analysis products
C            JIDS(13)  = TYPE OF PROCESSED DATA ( SEE CODE TABLE 1.4 )
C                       0  - Analysis products
C                       1  - Forecast products
C                       2  - Analysis and forecast products
C                       3  - Control forecast products
C                       4  - Perturbed forecast products
C                       5  - Control and perturbed forecast products
C                       6  - Processed satellite observations
C                       7  - Processed radar observations
C     JPDTN        INTEGER PRODUCT DEFINITION TEMPLATE NUMBER (N)
C                  ( IF = -1, DON'T BOTHER MATCHING PDT - ACCEPT ANY )
C     JPDT()       INTEGER ARRAY OF VALUES DEFINING THE PRODUCT DEFINITION
C                  TEMPLATE 4.N OF THE FIELD FOR WHICH TO SEARCH
C                  (=-9999 FOR WILDCARD)
C     JGDTN        INTEGER GRID DEFINITION TEMPLATE NUMBER (M)
C                  ( IF = -1, DON'T BOTHER MATCHING GDT - ACCEPT ANY )
C     JGDT()       INTEGER ARRAY OF VALUES DEFINING THE GRID DEFINITION
C                  TEMPLATE 3.M OF THE FIELD FOR WHICH TO SEARCH
C                  (=-9999 FOR WILDCARD)
C     EXTRACT       LOGICAL VALUE INDICATING WHETHER TO RETURN A GRIB2 
C                   MESSAGE WITH JUST THE REQUESTED FIELD, OR THE ENTIRE
C                   GRIB2 MESSAGE CONTAINING THE REQUESTED FIELD.
C                  .TRUE. = RETURN GRIB2 MESSAGE CONTAINING ONLY THE REQUESTED
C                           FIELD.
C                  .FALSE. = RETURN ENTIRE GRIB2 MESSAGE CONTAINING THE
C                            REQUESTED FIELD.
C
C   OUTPUT ARGUMENTS:
C     K            INTEGER FIELD NUMBER RETURNED.
C     GRIBM         RETURNED GRIB MESSAGE.
C     LENG         LENGTH OF RETURNED GRIB MESSAGE IN BYTES.
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    96     ERROR READING INDEX FILE
C                    97     ERROR READING GRIB FILE
C                    99     REQUEST NOT FOUND
C
C SUBPROGRAMS CALLED:
C   GETG2I          READ INDEX FILE
C   GETG2IR         READ INDEX BUFFER FROM GRIB FILE
C   GETGB2S        SEARCH INDEX RECORDS
C   GETGB2RP        READ A PACKED GRIB RECORD
C   GF_FREE        FREES MEMORY USED BY GFLD  ( SEE REMARKS )
C
C REMARKS: SPECIFY AN INDEX FILE IF FEASIBLE TO INCREASE SPEED.
C   DO NOT ENGAGE THE SAME LOGICAL UNIT FROM MORE THAN ONE PROCESSOR.
C
C   Note that derived type gribfield contains pointers to many
C   arrays of data.  The memory for these arrays is allocated
C   when the values in the arrays are set, to help minimize
C   problems with array overloading.  Because of this users
C   are encouraged to free up this memory, when it is no longer
C   needed, by an explicit call to subroutine gf_free.
C   ( i.e.   CALL GF_FREE(GFLD) )
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE GRIB_MOD

      INTEGER,INTENT(IN) :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
      LOGICAL,INTENT(IN) :: EXTRACT
      INTEGER,INTENT(OUT) :: K,IRET,LENG
      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM

      TYPE(GRIBFIELD) :: GFLD

      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
      PARAMETER(MSK1=32000,MSK2=4000)

      SAVE CBUF,NLEN,NNUM
      DATA LUX/0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DECLARE INTERFACES (REQUIRED FOR CBUF POINTER)
      INTERFACE
         SUBROUTINE GETG2I(LUGI,CBUF,NLEN,NNUM,IRET)
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
            INTEGER,INTENT(IN) :: LUGI
            INTEGER,INTENT(OUT) :: NLEN,NNUM,IRET
         END SUBROUTINE GETG2I
         SUBROUTINE GETG2IR(LUGB,MSK1,MSK2,MNUM,CBUF,NLEN,NNUM,
     &                      NMESS,IRET)
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
            INTEGER,INTENT(IN) :: LUGB,MSK1,MSK2,MNUM
            INTEGER,INTENT(OUT) :: NLEN,NNUM,NMESS,IRET
         END SUBROUTINE GETG2IR
         SUBROUTINE GETGB2RP(LUGB,CINDEX,EXTRACT,GRIBM,LENG,IRET)
            INTEGER,INTENT(IN) :: LUGB
            CHARACTER(LEN=1),INTENT(IN) :: CINDEX(*)
            LOGICAL,INTENT(IN) :: EXTRACT
            INTEGER,INTENT(OUT) :: LENG,IRET
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM
         END SUBROUTINE GETGB2RP
      END INTERFACE

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE WHETHER INDEX BUFFER NEEDS TO BE INITIALIZED
      IRGI=0
      IF(LUGI.GT.0.AND.LUGI.NE.LUX) THEN
        CALL GETG2I(LUGI,CBUF,NLEN,NNUM,IRGI)
        LUX=LUGI
      ELSEIF(LUGI.LE.0.AND.LUGB.NE.LUX) THEN
        MSKP=0
        CALL GETG2IR(LUGB,MSK1,MSK2,MSKP,CBUF,NLEN,NNUM,NMESS,IRGI)
        LUX=LUGB
      ENDIF
      IF(IRGI.GT.1) THEN
        IRET=96
        LUX=0
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH INDEX BUFFER
      CALL GETGB2S(CBUF,NLEN,NNUM,J,-1,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &             JK,GFLD,LPOS,IRGS)
      IF(IRGS.NE.0) THEN
        IRET=99
        CALL GF_FREE(GFLD)
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EXTRACT GRIB MESSAGE FROM FILE
      CALL GETGB2RP(LUGB,CBUF(LPOS:),EXTRACT,GRIBM,LENG,IRET)
!      IF ( EXTRACT ) THEN
!         PRINT *,'NOT SUPPOSED TO BE HERE.'
!      ELSE
!         IPOS=(LPOS+3)*8
!         CALL G2LIB_GBYTE(CBUF,ISKIP,IPOS,32)     ! BYTES TO SKIP IN FILE
!         IPOS=IPOS+(32*8)
!         CALL G2LIB_GBYTE(CBUF,LENG,IPOS,32)      ! LENGTH OF GRIB MESSAGE
!         IF (.NOT. ASSOCIATED(GRIBM)) ALLOCATE(GRIBM(LENG))
!         CALL BAREAD(LUGB,ISKIP,LENG,LREAD,GRIBM)
!         IF ( LENG .NE. LREAD ) THEN
!            IRET=97
!            CALL GF_FREE(GFLD)
!            RETURN
!         ENDIF
!      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      K=JK
      CALL GF_FREE(GFLD)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
