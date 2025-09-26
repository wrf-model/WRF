C-----------------------------------------------------------------------
      SUBROUTINE GETGB2S(CBUF,NLEN,NNUM,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,
     &                   JGDT,K,GFLD,LPOS,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETGB2S        FINDS A GRIB MESSAGE
C   PRGMMR: GILBERT          ORG: W/NP11     DATE: 02-01-15
C
C ABSTRACT: FIND A GRIB MESSAGE.
C   FIND IN THE INDEX FILE A REFERENCE TO THE GRIB FIELD REQUESTED.
C   THE GRIB FIELD REQUEST SPECIFIES THE NUMBER OF MESSAGES TO SKIP
C   AND THE UNPACKED IDENTIFICATION SECTION, GRID DEFINITION TEMPLATE AND 
C   PRODUCT DEFINTION SECTION PARAMETERS.  (A REQUESTED PARAMETER
C   OF -9999 MEANS TO ALLOW ANY VALUE OF THIS PARAMETER TO BE FOUND.)
C
C           EACH INDEX RECORD HAS THE FOLLOWING FORM:
C       BYTE 001 - 004: LENGTH OF INDEX RECORD
C       BYTE 005 - 008: BYTES TO SKIP IN DATA FILE BEFORE GRIB MESSAGE
C       BYTE 009 - 012: BYTES TO SKIP IN MESSAGE BEFORE LUS (LOCAL USE)
C                       SET = 0, IF NO LOCAL USE SECTION IN GRIB2 MESSAGE.
C       BYTE 013 - 016: BYTES TO SKIP IN MESSAGE BEFORE GDS
C       BYTE 017 - 020: BYTES TO SKIP IN MESSAGE BEFORE PDS
C       BYTE 021 - 024: BYTES TO SKIP IN MESSAGE BEFORE DRS
C       BYTE 025 - 028: BYTES TO SKIP IN MESSAGE BEFORE BMS
C       BYTE 029 - 032: BYTES TO SKIP IN MESSAGE BEFORE DATA SECTION
C       BYTE 033 - 040: BYTES TOTAL IN THE MESSAGE
C       BYTE 041 - 041: GRIB VERSION NUMBER ( CURRENTLY 2 )
C       BYTE 042 - 042: MESSAGE DISCIPLINE
C       BYTE 043 - 044: FIELD NUMBER WITHIN GRIB2 MESSAGE
C       BYTE 045 -  II: IDENTIFICATION SECTION (IDS)
C       BYTE II+1-  JJ: GRID DEFINITION SECTION (GDS)
C       BYTE JJ+1-  KK: PRODUCT DEFINITION SECTION (PDS)
C       BYTE KK+1-  LL: THE DATA REPRESENTATION SECTION (DRS)
C       BYTE LL+1-LL+6: FIRST 6 BYTES OF THE BIT MAP SECTION (BMS)
C
C   Most of the decoded information for the selected GRIB field
C   is returned in a derived type variable, gfld.  
C   Gfld is of type gribfield, which is defined
C   in module grib_mod, so users of this routine will need to include
C   the line "USE GRIB_MOD" in their calling routine.  Each component of the
C   gribfield type is described in the OUTPUT ARGUMENT LIST section below.
C   Only the unpacked bitmap and data field components are not set by this 
C   routine.
C
C PROGRAM HISTORY LOG:
C   95-10-31  IREDELL
C 2002-01-02  GILBERT   MODIFIED FROM GETG1S TO WORK WITH GRIB2
C 2011-06-24  VUONG BOI Initialize variable gfld%idsect and gfld%local
C
C USAGE:    CALL GETGB2S(CBUF,NLEN,NNUM,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,
C    &                   JGDT,K,GFLD,LPOS,IRET)
C   INPUT ARGUMENTS:
C     CBUF         CHARACTER*1 (NLEN) BUFFER CONTAINING INDEX DATA
C     NLEN         INTEGER TOTAL LENGTH OF ALL INDEX RECORDS
C     NNUM         INTEGER NUMBER OF INDEX RECORDS
C     J            INTEGER NUMBER OF MESSAGES TO SKIP
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
C                  ( IF = -1, DON'T BOTHER MATCHING PDT )
C     JPDT()       INTEGER ARRAY OF VALUES DEFINING THE PRODUCT DEFINITION 
C                  TEMPLATE 4.N OF THE FIELD FOR WHICH TO SEARCH
C                  (=-9999 FOR WILDCARD)
C     JGDTN        INTEGER GRID DEFINITION TEMPLATE NUMBER (M)
C                  ( IF = -1, DON'T BOTHER MATCHING GDT )
C     JGDT()       INTEGER ARRAY OF VALUES DEFINING THE GRID DEFINITION
C                  TEMPLATE 3.M OF THE FIELD FOR WHICH TO SEARCH
C                  (=-9999 FOR WILDCARD)
C   OUTPUT ARGUMENTS:
C     K            INTEGER MESSAGE NUMBER FOUND
C                  (CAN BE SAME AS J IN CALLING PROGRAM
C                  IN ORDER TO FACILITATE MULTIPLE SEARCHES)
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
C                        NOTE: This routine sets this component to .FALSE.
C        gfld%ibmap = Bitmap indicator ( see Code Table 6.0 )
C                     0 = bitmap applies and is included in Section 6.
C                     1-253 = Predefined bitmap applies
C                     254 = Previously defined bitmap applies to this field
C                     255 = Bit map does not apply to this product.
C        gfld%bmap() = Logical*1 array containing decoded bitmap,
C                      if ibmap=0 or ibap=254.  Otherwise nullified.
C                      This element is actually a pointer to an array
C                      that holds the data.
C                      NOTE: This component is not set by this routine.
C        gfld%fld() = Array of gfld%ndpts unpacked data points.
C                     This element is actually a pointer to an array
C                     that holds the data.
C                      NOTE: This component is not set by this routine.
C     LPOS         STARTING POSITION OF THE FOUND INDEX RECORD WITHIN
C                  THE COMPLETE INDEX BUFFER, CBUF.
C                  = 0, IF REQUEST NOT FOUND
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    1      REQUEST NOT FOUND
C
C REMARKS: 
C   THIS SUBPROGRAM IS INTENDED FOR PRIVATE USE BY GETGB2 ROUTINES ONLY.
C
C   Note that derived type gribfield contains pointers to many
C   arrays of data.  The memory for these arrays is allocated
C   when the values in the arrays are set, to help minimize
C   problems with array overloading.  Because of this users
C   are encouraged to free up this memory, when it is no longer
C   needed, by an explicit call to subroutine gf_free.
C   ( i.e.   CALL GF_FREE(GFLD) )
C
C SUBPROGRAMS CALLED:
C   GBYTE            UNPACK BYTES
C   GF_UNPACK1          UNPACK IDS
C   GF_UNPACK4          UNPACK PDS
C   GF_UNPACK3          UNPACK GDS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE GRIB_MOD

!      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
      CHARACTER(LEN=1),INTENT(IN) :: CBUF(NLEN)
      INTEGER,INTENT(IN) :: NLEN,NNUM,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
      INTEGER,INTENT(OUT) :: K,LPOS,IRET
      TYPE(GRIBFIELD),INTENT(OUT) :: GFLD

      INTEGER :: KGDS(5)
      LOGICAL :: MATCH1,MATCH3,MATCH4
!      INTEGER,POINTER,DIMENSION(:) :: KIDS,KPDT,KGDT
!      INTEGER,POINTER,DIMENSION(:) :: IDEF
!      REAL,POINTER,DIMENSION(:) :: COORD

      interface
         subroutine gf_unpack1(cgrib,lcgrib,iofst,ids,idslen,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,pointer,dimension(:) :: ids
            integer,intent(out) :: ierr,idslen
         end subroutine gf_unpack1
         subroutine gf_unpack3(cgrib,lcgrib,iofst,igds,igdstmpl,
     &                         mapgridlen,ideflist,idefnum,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,pointer,dimension(:) :: igdstmpl,ideflist
            integer,intent(out) :: igds(5)
            integer,intent(out) :: ierr,idefnum
         end subroutine gf_unpack3
         subroutine gf_unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl,
     &                      mappdslen,coordlist,numcoord,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            real,pointer,dimension(:) :: coordlist
            integer,pointer,dimension(:) :: ipdstmpl
            integer,intent(out) :: ipdsnum
            integer,intent(out) :: ierr,numcoord
         end subroutine gf_unpack4
         subroutine gf_unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,
     &                         idrstmpl,mapdrslen,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,intent(out) :: ndpts,idrsnum
            integer,pointer,dimension(:) :: idrstmpl
            integer,intent(out) :: ierr
         end subroutine gf_unpack5
      end interface
      
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INITIALIZE
      K=0
      LPOS=0
      IRET=1
      IPOS=0
      nullify(gfld%idsect,gfld%local)
      nullify(gfld%list_opt,gfld%igdtmpl,gfld%ipdtmpl)
      nullify(gfld%coord_list,gfld%idrtmpl,gfld%bmap,gfld%fld)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH FOR REQUEST
      DOWHILE(IRET.NE.0.AND.K.LT.NNUM)
        K=K+1
        CALL GBYTE(CBUF,INLEN,IPOS*8,4*8)    ! GET LENGTH OF CURRENT
                                              ! INDEX RECORD
        IF ( K.LE.J ) THEN           ! SKIP THIS INDEX
           IPOS=IPOS+INLEN
           CYCLE
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK IF GRIB2 DISCIPLINE IS A MATCH
        CALL GBYTE(CBUF,GFLD%DISCIPLINE,(IPOS+41)*8,1*8)
        IF ( (JDISC.NE.-1).AND.(JDISC.NE.GFLD%DISCIPLINE) ) THEN
           IPOS=IPOS+INLEN
           CYCLE
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK IF IDENTIFICATION SECTION IS A MATCH
        MATCH1=.FALSE.
        CALL GBYTE(CBUF,LSEC1,(IPOS+44)*8,4*8)  ! GET LENGTH OF IDS 
        IOF=0
        CALL GF_UNPACK1(CBUF(IPOS+45),LSEC1,IOF,GFLD%IDSECT,
     &                  GFLD%IDSECTLEN,ICND)
        IF ( ICND.EQ.0 ) THEN
           MATCH1=.TRUE.
           DO I=1,GFLD%IDSECTLEN
              IF ( (JIDS(I).NE.-9999).AND.
     &             (JIDS(I).NE.GFLD%IDSECT(I)) ) THEN
                 MATCH1=.FALSE.
                 EXIT
              ENDIF
           ENDDO
        ENDIF
        IF ( .NOT. MATCH1 ) THEN
           DEALLOCATE(GFLD%IDSECT)
           IPOS=IPOS+INLEN
           CYCLE
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK IF GRID DEFINITION TEMPLATE IS A MATCH
        JPOS=IPOS+44+LSEC1
        MATCH3=.FALSE.
        CALL GBYTE(CBUF,LSEC3,JPOS*8,4*8)  ! GET LENGTH OF GDS 
        IF ( JGDTN.EQ.-1 ) THEN
           MATCH3=.TRUE.
        ELSE
           CALL GBYTE(CBUF,NUMGDT,(JPOS+12)*8,2*8)  ! GET GDT TEMPLATE NO.
           IF ( JGDTN.EQ.NUMGDT ) THEN
              IOF=0
              CALL GF_UNPACK3(CBUF(JPOS+1),LSEC3,IOF,KGDS,GFLD%IGDTMPL,
     &                     GFLD%IGDTLEN,GFLD%LIST_OPT,GFLD%NUM_OPT,ICND)
              IF ( ICND.EQ.0 ) THEN
                 MATCH3=.TRUE.
                 DO I=1,GFLD%IGDTLEN
                    IF ( (JGDT(I).NE.-9999).AND.
     &                   (JGDT(I).NE.GFLD%IGDTMPL(I)) ) THEN
                       MATCH3=.FALSE.
                       EXIT
                    ENDIF
                 ENDDO
C                 WHERE ( JGDT(1:GFLD%IGDTLEN).NE.-9999 ) 
C     &              MATCH3=ALL(JGDT(1:GFLD%IGDTLEN).EQ.GFLD%IGDTMPL(1:GFLD%IGDTLEN))
              ENDIF
           ENDIF
        ENDIF
        IF ( .NOT. MATCH3 ) THEN
           IF (ASSOCIATED(GFLD%IGDTMPL)) DEALLOCATE(GFLD%IGDTMPL)
           IF (ASSOCIATED(GFLD%LIST_OPT)) DEALLOCATE(GFLD%LIST_OPT)
           IPOS=IPOS+INLEN
           CYCLE
        ELSE
           GFLD%GRIDDEF=KGDS(1)
           GFLD%NGRDPTS=KGDS(2)
           GFLD%NUMOCT_OPT=KGDS(3)
           GFLD%INTERP_OPT=KGDS(4)
           GFLD%IGDTNUM=KGDS(5)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK IF PRODUCT DEFINITION TEMPLATE IS A MATCH
        JPOS=JPOS+LSEC3
        MATCH4=.FALSE.
        CALL GBYTE(CBUF,LSEC4,JPOS*8,4*8)  ! GET LENGTH OF PDS 
        IF ( JPDTN.EQ.-1 ) THEN
           MATCH4=.TRUE.
        ELSE
           CALL GBYTE(CBUF,NUMPDT,(JPOS+7)*8,2*8)  ! GET PDT TEMPLATE NO.
           IF ( JPDTN.EQ.NUMPDT ) THEN
              IOF=0
              CALL GF_UNPACK4(CBUF(JPOS+1),LSEC4,IOF,GFLD%IPDTNUM,
     &                        GFLD%IPDTMPL,GFLD%IPDTLEN,
     &                        GFLD%COORD_LIST,GFLD%NUM_COORD,ICND)
              IF ( ICND.EQ.0 ) THEN
                 MATCH4=.TRUE.
                 DO I=1,GFLD%IPDTLEN
                    IF ( (JPDT(I).NE.-9999).AND.
     &                   (JPDT(I).NE.GFLD%IPDTMPL(I)) ) THEN
                       MATCH4=.FALSE.
                       EXIT
                    ENDIF
                 ENDDO
c                 WHERE ( JPDT.NE.-9999) 
c     &              MATCH4=ALL( JPDT(1:GFLD%IPDTLEN) .EQ. GFLD%IPDTMPL(1:GFLD%IPDTLEN) )
              ENDIF
           ENDIF
        ENDIF
        IF ( .NOT. MATCH4 ) THEN
           IF (ASSOCIATED(GFLD%IPDTMPL)) DEALLOCATE(GFLD%IPDTMPL)
           IF (ASSOCIATED(GFLD%COORD_LIST)) DEALLOCATE(GFLD%COORD_LIST)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  IF REQUEST IS FOUND
C  SET VALUES FOR DERIVED TYPE GFLD AND RETURN
        IF(MATCH1.AND.MATCH3.AND.MATCH4) THEN
           LPOS=IPOS+1
           CALL GBYTE(CBUF,GFLD%VERSION,(IPOS+40)*8,1*8)
           CALL GBYTE(CBUF,GFLD%IFLDNUM,(IPOS+42)*8,2*8)
           GFLD%UNPACKED=.FALSE.
           JPOS=IPOS+44+LSEC1
           IF ( JGDTN.EQ.-1 ) THEN     ! UNPACK GDS, IF NOT DONE BEFORE
              IOF=0
              CALL GF_UNPACK3(CBUF(JPOS+1),LSEC3,IOF,KGDS,GFLD%IGDTMPL,
     &                     GFLD%IGDTLEN,GFLD%LIST_OPT,GFLD%NUM_OPT,ICND)
              GFLD%GRIDDEF=KGDS(1)
              GFLD%NGRDPTS=KGDS(2)
              GFLD%NUMOCT_OPT=KGDS(3)
              GFLD%INTERP_OPT=KGDS(4)
              GFLD%IGDTNUM=KGDS(5)
           ENDIF
           JPOS=JPOS+LSEC3
           IF ( JPDTN.EQ.-1 ) THEN     ! UNPACK PDS, IF NOT DONE BEFORE
              IOF=0
              CALL GF_UNPACK4(CBUF(JPOS+1),LSEC4,IOF,GFLD%IPDTNUM,
     &                        GFLD%IPDTMPL,GFLD%IPDTLEN,
     &                        GFLD%COORD_LIST,GFLD%NUM_COORD,ICND)
           ENDIF
           JPOS=JPOS+LSEC4
           CALL GBYTE(CBUF,LSEC5,JPOS*8,4*8)  ! GET LENGTH OF DRS 
           IOF=0
           CALL GF_UNPACK5(CBUF(JPOS+1),LSEC5,IOF,GFLD%NDPTS,
     &                     GFLD%IDRTNUM,GFLD%IDRTMPL,
     &                     GFLD%IDRTLEN,ICND)
           JPOS=JPOS+LSEC5
           CALL GBYTE(CBUF,GFLD%IBMAP,(JPOS+5)*8,1*8)  ! GET IBMAP
           IRET=0
        ELSE      ! PDT DID NOT MATCH
           IPOS=IPOS+INLEN
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
