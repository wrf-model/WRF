C-----------------------------------------------------------------------
      SUBROUTINE GETGB2(LUGB,LUGI,J,GUESS,JDISC,JIDS,JPDTN,JPDT,JGDTN,
     &                  JGDT,UNPACK,K,GFLD,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETGB2         FINDS AND UNPACKS A GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 94-04-01
C
C ABSTRACT: FIND AND UNPACK A GRIB MESSAGE.
C   READ A GRIB INDEX FILE (OR OPTIONALLY THE GRIB FILE ITSELF)
C   TO GET THE INDEX BUFFER (I.E. TABLE OF CONTENTS) FOR THE GRIB FILE.
C   FIND IN THE INDEX BUFFER A REFERENCE TO THE GRIB FIELD REQUESTED.
C   THE GRIB FIELD REQUEST SPECIFIES THE NUMBER OF FIELDS TO SKIP
C   AND THE UNPACKED IDENTIFICATION SECTION, GRID DEFINITION TEMPLATE AND
C   PRODUCT DEFINTION SECTION PARAMETERS.  (A REQUESTED PARAMETER
C   OF -9999 MEANS TO ALLOW ANY VALUE OF THIS PARAMETER TO BE FOUND.)
C   IF THE REQUESTED GRIB FIELD IS FOUND, THEN IT IS READ FROM THE
C   GRIB FILE AND UNPACKED.  ITS NUMBER IS RETURNED ALONG WITH
C   THE ASSOCIATED UNPACKED PARAMETERS.  THE BITMAP (IF ANY),
C   AND THE DATA VALUES ARE UNPACKED ONLY IF ARGUMENT "UNPACK" IS SET TO
C   TRUE.  IF THE GRIB FIELD IS NOT FOUND, THEN THE
C   RETURN CODE WILL BE NONZERO.
C
C   The decoded information for the selected GRIB field
C   is returned in a derived type variable, gfld.
C   Gfld is of type gribfield, which is defined
C   in module grib_mod, so users of this routine will need to include
C   the line "USE GRIB_MOD" in their calling routine.  Each component of the
C   gribfield type is described in the OUTPUT ARGUMENT LIST section below.
C
C PROGRAM HISTORY LOG:
C   94-04-01  IREDELL
C   95-10-31  IREDELL     MODULARIZED PORTIONS OF CODE INTO SUBPROGRAMS
C                         AND ALLOWED FOR UNSPECIFIED INDEX FILE
C 2002-01-11  GILBERT     MODIFIED FROM GETGB AND GETGBM TO WORK WITH GRIB2
C
C USAGE:    CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
C    &                  UNPACK,K,GFLD,IRET)
C   INPUT ARGUMENTS:
C     LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE.
C                  FILE MUST BE OPENED WITH BAOPEN OR BAOPENR BEFORE CALLING 
C                  THIS ROUTINE.
C     LUGI         INTEGER UNIT OF THE UNBLOCKED GRIB INDEX FILE.
C                  IF NONZERO, FILE MUST BE OPENED WITH BAOPEN BAOPENR BEFORE 
C                  CALLING THIS ROUTINE.
C                  >0 - READ INDEX FROM INDEX FILE LUGI, IF INDEX DOESN"T
C                       ALREADY EXIST.
C                  =0 - TO GET INDEX BUFFER FROM THE GRIB FILE, IF INDEX
C                       DOESN"T ALREADY EXIST.
C                  <0 - FORCE REREAD OF INDEX FROM INDEX FILE ABS(LUGI).
C                  =LUGB - FORCE REGENERATION OF INDEX FROM GRIB2 FILE LUGB.
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
C     UNPACK       LOGICAL VALUE INDICATING WHETHER TO UNPACK BITMAP/DATA
C                  .TRUE. = UNPACK BITMAP AND DATA VALUES
C                  .FALSE. = DO NOT UNPACK BITMAP AND DATA VALUES
C
C   OUTPUT ARGUMENTS:
C     K            INTEGER FIELD NUMBER UNPACKED
C     gfld - derived type gribfield ( defined in module grib_mod )
C            ( NOTE: See Remarks Section )
C        gfld%version = GRIB edition number ( currently 2 )
C        gfld%discipline = Message Discipline ( see Code Table 0.0 )
C        gfld%idsect() = Contains the entries in the Identification
C                        Section ( Section 1 )
C                        This element is actually a pointer to an array
C                        that holds the data.
C            gfld%idsect(1)  = Identification of originating Centre
C                                    ( see Common Code Table C-1 )
C                             7 - US National Weather Service
C            gfld%idsect(2)  = Identification of originating Sub-centre
C            gfld%idsect(3)  = GRIB Master Tables Version Number
C                                    ( see Code Table 1.0 )
C                             0 - Experimental
C                             1 - Initial operational version number
C            gfld%idsect(4)  = GRIB Local Tables Version Number
C                                    ( see Code Table 1.1 )
C                             0     - Local tables not used
C                             1-254 - Number of local tables version used
C            gfld%idsect(5)  = Significance of Reference Time (Code Table 1.2)
C                             0 - Analysis
C                             1 - Start of forecast
C                             2 - Verifying time of forecast
C                             3 - Observation time
C            gfld%idsect(6)  = Year ( 4 digits )
C            gfld%idsect(7)  = Month
C            gfld%idsect(8)  = Day
C            gfld%idsect(9)  = Hour
C            gfld%idsect(10)  = Minute
C            gfld%idsect(11)  = Second
C            gfld%idsect(12)  = Production status of processed data
C                                    ( see Code Table 1.3 )
C                              0 - Operational products
C                              1 - Operational test products
C                              2 - Research products
C                              3 - Re-analysis products
C            gfld%idsect(13)  = Type of processed data ( see Code Table 1.4 )
C                              0  - Analysis products
C                              1  - Forecast products
C                              2  - Analysis and forecast products
C                              3  - Control forecast products
C                              4  - Perturbed forecast products
C                              5  - Control and perturbed forecast products
C                              6  - Processed satellite observations
C                              7  - Processed radar observations
C        gfld%idsectlen = Number of elements in gfld%idsect().
C        gfld%local() = Pointer to character array containing contents
C                       of Local Section 2, if included
C        gfld%locallen = length of array gfld%local()
C        gfld%ifldnum = field number within GRIB message
C        gfld%griddef = Source of grid definition (see Code Table 3.0)
C                      0 - Specified in Code table 3.1
C                      1 - Predetermined grid Defined by originating centre
C        gfld%ngrdpts = Number of grid points in the defined grid.
C        gfld%numoct_opt = Number of octets needed for each
C                          additional grid points definition.
C                          Used to define number of
C                          points in each row ( or column ) for
C                          non-regular grids.
C                          = 0, if using regular grid.
C        gfld%interp_opt = Interpretation of list for optional points
C                          definition.  (Code Table 3.11)
C        gfld%igdtnum = Grid Definition Template Number (Code Table 3.1)
C        gfld%igdtmpl() = Contains the data values for the specified Grid
C                         Definition Template ( NN=gfld%igdtnum ).  Each
C                         element of this integer array contains an entry (in
C                         the order specified) of Grid Defintion Template 3.NN
C                         This element is actually a pointer to an array
C                         that holds the data.
C        gfld%igdtlen = Number of elements in gfld%igdtmpl().  i.e. number of
C                       entries in Grid Defintion Template 3.NN
C                       ( NN=gfld%igdtnum ).
C        gfld%list_opt() = (Used if gfld%numoct_opt .ne. 0)  This array
C                          contains the number of grid points contained in
C                          each row ( or column ).  (part of Section 3)
C                          This element is actually a pointer to an array
C                          that holds the data.  This pointer is nullified
C                          if gfld%numoct_opt=0.
C        gfld%num_opt = (Used if gfld%numoct_opt .ne. 0)  The number of entries
C                       in array ideflist.  i.e. number of rows ( or columns )
C                       for which optional grid points are defined.  This value
C                       is set to zero, if gfld%numoct_opt=0.
C        gfdl%ipdtnum = Product Definition Template Number (see Code Table 4.0)
C        gfld%ipdtmpl() = Contains the data values for the specified Product
C                         Definition Template ( N=gfdl%ipdtnum ).  Each element
C                         of this integer array contains an entry (in the
C                         order specified) of Product Defintion Template 4.N.
C                         This element is actually a pointer to an array
C                         that holds the data.
C        gfld%ipdtlen = Number of elements in gfld%ipdtmpl().  i.e. number of
C                       entries in Product Defintion Template 4.N
C                       ( N=gfdl%ipdtnum ).
C        gfld%coord_list() = Real array containing floating point values
C                            intended to document the vertical discretisation
C                            associated to model data on hybrid coordinate
C                            vertical levels.  (part of Section 4)
C                            This element is actually a pointer to an array
C                            that holds the data.
C        gfld%num_coord = number of values in array gfld%coord_list().
C        gfld%ndpts = Number of data points unpacked and returned.
C        gfld%idrtnum = Data Representation Template Number
C                       ( see Code Table 5.0)
C        gfld%idrtmpl() = Contains the data values for the specified Data
C                         Representation Template ( N=gfld%idrtnum ).  Each
C                         element of this integer array contains an entry
C                         (in the order specified) of Product Defintion
C                         Template 5.N.
C                         This element is actually a pointer to an array
C                         that holds the data.
C        gfld%idrtlen = Number of elements in gfld%idrtmpl().  i.e. number
C                       of entries in Data Representation Template 5.N
C                       ( N=gfld%idrtnum ).
C        gfld%unpacked = logical value indicating whether the bitmap and
C                        data values were unpacked.  If false,
C                        gfld%bmap and gfld%fld pointers are nullified.
C        gfld%expanded = Logical value indicating whether the data field
C                         was expanded to the grid in the case where a
C                         bit-map is present.  If true, the data points in
C                         gfld%fld match the grid points and zeros were
C                         inserted at grid points where data was bit-mapped
C                         out.  If false, the data values in gfld%fld were
C                         not expanded to the grid and are just a consecutive
C                         array of data points corresponding to each value of
C                         "1" in gfld%bmap.
C        gfld%ibmap = Bitmap indicator ( see Code Table 6.0 )
C                     0 = bitmap applies and is included in Section 6.
C                     1-253 = Predefined bitmap applies
C                     254 = Previously defined bitmap applies to this field
C                     255 = Bit map does not apply to this product.
C        gfld%bmap() = Logical*1 array containing decoded bitmap,
C                      if ibmap=0 or ibap=254.  Otherwise nullified.
C                      This element is actually a pointer to an array
C                      that holds the data.
C        gfld%fld() = Array of gfld%ndpts unpacked data points.
C                     This element is actually a pointer to an array
C                     that holds the data.
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    96     ERROR READING INDEX
C                    97     ERROR READING GRIB FILE
C                    99     REQUEST NOT FOUND
C                    OTHER  GF_GETFLD GRIB2 UNPACKER RETURN CODE
C
C SUBPROGRAMS CALLED:
C   GETIDX         GET INDEX
C   GETGB2S        SEARCH INDEX RECORDS
C   GETGB2R        READ AND UNPACK GRIB RECORD
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
      INTEGER,INTENT(IN) :: GUESS
      INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
      LOGICAL,INTENT(IN) :: UNPACK
      INTEGER,INTENT(OUT) :: K,IRET
      TYPE(GRIBFIELD),INTENT(OUT) :: GFLD

      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DECLARE INTERFACES (REQUIRED FOR CBUF POINTER)
      INTERFACE
         SUBROUTINE GETIDX(LUGB,LUGI,CBUF,NLEN,NNUM,IRGI)
           CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
           INTEGER,INTENT(IN) :: LUGB,LUGI
           INTEGER,INTENT(OUT) :: NLEN,NNUM,IRGI
         END SUBROUTINE GETIDX
      END INTERFACE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE WHETHER INDEX BUFFER NEEDS TO BE INITIALIZED
      IRGI=0
      CALL GETIDX(LUGB,LUGI,CBUF,NLEN,NNUM,IRGI)
      IF(IRGI.GT.1) THEN
        IRET=96
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH INDEX BUFFER
      CALL GETGB2S(CBUF,NLEN,NNUM,J,GUESS,JDISC,JIDS,JPDTN,JPDT,JGDTN,
     &             JGDT,JK,GFLD,LPOS,IRGS)
      IF(IRGS.NE.0) THEN
        IRET=99
        CALL GF_FREE(GFLD)
        RETURN
      ENDIF

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ LOCAL USE SECTION, IF AVAILABLE
      CALL GETGB2L(LUGB,CBUF(LPOS),GFLD,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ AND UNPACK GRIB RECORD
      IF (UNPACK) THEN
    !    NUMFLD=GFLD%IFLDNUM
    !    CALL GF_FREE(GFLD)
         CALL GETGB2R(LUGB,CBUF(LPOS),GFLD,IRET)
      ENDIF
      K=JK
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
