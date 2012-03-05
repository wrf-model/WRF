C-----------------------------------------------------------------------
      SUBROUTINE PUTGB2(LUGB,GFLD,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: PUTGB2         PACKS AND WRITES A GRIB2 MESSAGE
C   PRGMMR: GILBERT          ORG: W/NP11     DATE: 2002-04-22
C
C ABSTRACT: PACKS A SINGLE FIELD INTO A GRIB2 MESSAGE
C   AND WRITES OUT THAT MESSAGE TO THE FILE ASSOCIATED WITH UNIT LUGB.
C   NOTE THAT FILE/UNIT LUGB SHOULD BE OPENED WOTH A CALL TO 
C   SUBROUTINE BAOPENW BEFORE THIS ROUTINE IS CALLED.
C
C   The information to be packed into the GRIB field
C   is stored in a derived type variable, gfld.
C   Gfld is of type gribfield, which is defined
C   in module grib_mod, so users of this routine will need to include
C   the line "USE GRIB_MOD" in their calling routine.  Each component of the
C   gribfield type is described in the INPUT ARGUMENT LIST section below.
C
C PROGRAM HISTORY LOG:
C 2002-04-22  GILBERT  
C 2005-02-28  GILBERT   - Changed dimension of array cgrib to be a multiple
C                         of gfld%ngrdpts instead of gfld%ndpts.
C
C USAGE:    CALL PUTGB2(LUGB,GFLD,IRET)
C   INPUT ARGUMENTS:
C     LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE.
C                  FILE MUST BE OPENED WITH BAOPEN OR BAOPENW BEFORE CALLING 
C                  THIS ROUTINE.
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
C
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    2      MEMORY ALLOCATION ERROR
C                    10     No Section 1 info available
C                    11     No Grid Definition Template info available
C                    12     Missing some required data field info
C
C SUBPROGRAMS CALLED:
C   gribcreate     Start a new grib2 message
C   addlocal       Add local section to a GRIB2 message
C   addgrid        Add grid info to a GRIB2 message
C   addfield       Add data field to a GRIB2 message
C   gribend        End GRIB2 message
C
C REMARKS: 
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

      INTEGER,INTENT(IN) :: LUGB
      TYPE(GRIBFIELD),INTENT(IN) :: GFLD
      INTEGER,INTENT(OUT) :: IRET

      CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: CGRIB
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ALLOCATE ARRAY FOR GRIB2 FIELD
      lcgrib=gfld%ngrdpts*4
      allocate(cgrib(lcgrib),stat=is)
      if ( is.ne.0 ) then
         print *,'putgb2: cannot allocate memory. ',is
         iret=2
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CREATE NEW MESSAGE
      listsec0(1)=gfld%discipline
      listsec0(2)=gfld%version
      if ( associated(gfld%idsect) ) then
         call gribcreate(cgrib,lcgrib,listsec0,gfld%idsect,ierr)
         if (ierr.ne.0) then
            write(6,*) 'putgb2: ERROR creating new GRIB2 field = ',ierr
         endif
      else
         print *,'putgb2: No Section 1 info available. '
         iret=10
         deallocate(cgrib)
         return
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ADD LOCAL USE SECTION TO GRIB2 MESSAGE
      if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
         call addlocal(cgrib,lcgrib,gfld%local,gfld%locallen,ierr)
         if (ierr.ne.0) then
            write(6,*) 'putgb2: ERROR adding local info = ',ierr
         endif
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ADD GRID TO GRIB2 MESSAGE
      igds(1)=gfld%griddef
      igds(2)=gfld%ngrdpts
      igds(3)=gfld%numoct_opt
      igds(4)=gfld%interp_opt
      igds(5)=gfld%igdtnum
      if ( associated(gfld%igdtmpl) ) then
         call addgrid(cgrib,lcgrib,igds,gfld%igdtmpl,gfld%igdtlen,
     &                gfld%list_opt,gfld%num_opt,ierr)
         if (ierr.ne.0) then
            write(6,*) 'putgb2: ERROR adding grid info = ',ierr
         endif
      else
         print *,'putgb2: No GDT info available. '
         iret=11
         deallocate(cgrib)
         return
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ADD DATA FIELD TO GRIB2 MESSAGE
      if ( associated(gfld%ipdtmpl).AND.
     &     associated(gfld%idrtmpl).AND.
     &     associated(gfld%fld) ) then
         call addfield(cgrib,lcgrib,gfld%ipdtnum,gfld%ipdtmpl,
     &                 gfld%ipdtlen,gfld%coord_list,gfld%num_coord,
     &                 gfld%idrtnum,gfld%idrtmpl,gfld%idrtlen,
     &                 gfld%fld,gfld%ngrdpts,gfld%ibmap,gfld%bmap,
     &                 ierr)
         if (ierr.ne.0) then
            write(6,*) 'putgb2: ERROR adding data field = ',ierr
         endif
      else
         print *,'putgb2: Missing some field info. '
         iret=12
         deallocate(cgrib)
         return
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CLOSE GRIB2 MESSAGE AND WRITE TO FILE
      call gribend(cgrib,lcgrib,lengrib,ierr)
      call wryte(lugb,lengrib,cgrib)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      deallocate(cgrib)
      RETURN
      END
