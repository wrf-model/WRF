      module grib_mod
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MODULE:    grib_mod 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2002-01-23
!
! ABSTRACT: This Fortran Module contains the declaration
!   of derived type gribfield.
!   If variable gfld is declared of type gribfield 
!   ( i.e. TYPE(GRIBFIELD) :: GFLD ), it would have the following componenets:
!
!        gfld%version = GRIB edition number ( currently 2 )
!        gfld%discipline = Message Discipline ( see Code Table 0.0 )
!        gfld%idsect() = Contains the entries in the Identification
!                        Section ( Section 1 )
!                        This element is actually a pointer to an array
!                        that holds the data.
!            gfld%idsect(1)  = Identification of originating Centre
!                                    ( see Common Code Table C-1 )
!                             7 - US National Weather Service
!            gfld%idsect(2)  = Identification of originating Sub-centre
!            gfld%idsect(3)  = GRIB Master Tables Version Number
!                                    ( see Code Table 1.0 )
!                             0 - Experimental
!                             1 - Initial operational version number
!            gfld%idsect(4)  = GRIB Local Tables Version Number
!                                    ( see Code Table 1.1 )
!                             0     - Local tables not used
!                             1-254 - Number of local tables version used
!            gfld%idsect(5)  = Significance of Reference Time (Code Table 1.2)
!                             0 - Analysis
!                             1 - Start of forecast
!                             2 - Verifying time of forecast
!                             3 - Observation time
!            gfld%idsect(6)  = Year ( 4 digits )
!            gfld%idsect(7)  = Month
!            gfld%idsect(8)  = Day
!            gfld%idsect(9)  = Hour
!            gfld%idsect(10)  = Minute
!            gfld%idsect(11)  = Second
!            gfld%idsect(12)  = Production status of processed data
!                                    ( see Code Table 1.3 )
!                              0 - Operational products
!                              1 - Operational test products
!                              2 - Research products
!                              3 - Re-analysis products
!            gfld%idsect(13)  = Type of processed data ( see Code Table 1.4 )
!                              0  - Analysis products
!                              1  - Forecast products
!                              2  - Analysis and forecast products
!                              3  - Control forecast products
!                              4  - Perturbed forecast products
!                              5  - Control and perturbed forecast products
!                              6  - Processed satellite observations
!                              7  - Processed radar observations
!        gfld%idsectlen = Number of elements in gfld%idsect().
!        gfld%local() = Pointer to character array containing contents
!                       of Local Section 2, if included
!        gfld%locallen = length of array gfld%local()
!        gfld%ifldnum = field number within GRIB message
!        gfld%griddef = Source of grid definition (see Code Table 3.0)
!                      0 - Specified in Code table 3.1
!                      1 - Predetermined grid Defined by originating centre
!        gfld%ngrdpts = Number of grid points in the defined grid.
!        gfld%numoct_opt = Number of octets needed for each
!                          additional grid points definition.
!                          Used to define number of
!                          points in each row ( or column ) for
!                          non-regular grids.
!                          = 0, if using regular grid.
!        gfld%interp_opt = Interpretation of list for optional points
!                          definition.  (Code Table 3.11)
!        gfld%igdtnum = Grid Definition Template Number (Code Table 3.1)
!        gfld%igdtmpl() = Contains the data values for the specified Grid
!                         Definition Template ( NN=gfld%igdtnum ).  Each
!                         element of this integer array contains an entry (in
!                         the order specified) of Grid Defintion Template 3.NN
!                         This element is actually a pointer to an array
!                         that holds the data.
!        gfld%igdtlen = Number of elements in gfld%igdtmpl().  i.e. number of
!                       entries in Grid Defintion Template 3.NN
!                       ( NN=gfld%igdtnum ).
!        gfld%list_opt() = (Used if gfld%numoct_opt .ne. 0)  This array
!                          contains the number of grid points contained in
!                          each row ( or column ).  (part of Section 3)
!                          This element is actually a pointer to an array
!                          that holds the data.  This pointer is nullified
!                          if gfld%numoct_opt=0.
!        gfld%num_opt = (Used if gfld%numoct_opt .ne. 0)  The number of entries
!                       in array ideflist.  i.e. number of rows ( or columns )
!                       for which optional grid points are defined.  This value
!                       is set to zero, if gfld%numoct_opt=0.
!        gfdl%ipdtnum = Product Definition Template Number (see Code Table 4.0)
!        gfld%ipdtmpl() = Contains the data values for the specified Product
!                         Definition Template ( N=gfdl%ipdtnum ).  Each element
!                         of this integer array contains an entry (in the
!                         order specified) of Product Defintion Template 4.N.
!                         This element is actually a pointer to an array
!                         that holds the data.
!        gfld%ipdtlen = Number of elements in gfld%ipdtmpl().  i.e. number of
!                       entries in Product Defintion Template 4.N
!                       ( N=gfdl%ipdtnum ).
!        gfld%coord_list() = Real array containing floating point values
!                            intended to document the vertical discretisation
!                            associated to model data on hybrid coordinate
!                            vertical levels.  (part of Section 4)
!                            This element is actually a pointer to an array
!                            that holds the data.
!        gfld%num_coord = number of values in array gfld%coord_list().
!        gfld%ndpts = Number of data points unpacked and returned.
!        gfld%idrtnum = Data Representation Template Number
!                       ( see Code Table 5.0)
!        gfld%idrtmpl() = Contains the data values for the specified Data
!                         Representation Template ( N=gfld%idrtnum ).  Each
!                         element of this integer array contains an entry
!                         (in the order specified) of Product Defintion
!                         Template 5.N.
!                         This element is actually a pointer to an array
!                         that holds the data.
!        gfld%idrtlen = Number of elements in gfld%idrtmpl().  i.e. number
!                       of entries in Data Representation Template 5.N
!                       ( N=gfld%idrtnum ).
!        gfld%unpacked = logical value indicating whether the bitmap and
!                        data values were unpacked.  If false,
!                        gfld%bmap and gfld%fld pointers are nullified.
!        gfld%expanded = Logical value indicating whether the data field
!                         was expanded to the grid in the case where a
!                         bit-map is present.  If true, the data points in
!                         gfld%fld match the grid points and zeros were
!                         inserted at grid points where data was bit-mapped
!                         out.  If false, the data values in gfld%fld were
!                         not expanded to the grid and are just a consecutive
!                         array of data points corresponding to each value of
!                         "1" in gfld%bmap.
!        gfld%ibmap = Bitmap indicator ( see Code Table 6.0 )
!                     0 = bitmap applies and is included in Section 6.
!                     1-253 = Predefined bitmap applies
!                     254 = Previously defined bitmap applies to this field
!                     255 = Bit map does not apply to this product.
!        gfld%bmap() = Logical*1 array containing decoded bitmap,
!                      if ibmap=0 or ibap=254.  Otherwise nullified.
!                      This element is actually a pointer to an array
!                      that holds the data.
!        gfld%fld() = Array of gfld%ndpts unpacked data points.
!                     This element is actually a pointer to an array
!                     that holds the data.
!
!
! PROGRAM HISTORY LOG:
! 2002-01-23  Gilbert
!
! USAGE:    use grib_mod
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=12) :: G2_VERSION="g2lib-1.0.7"

      type gribfield
          integer :: version,discipline
          integer,pointer,dimension(:) :: idsect
          integer :: idsectlen
          character(len=1),pointer,dimension(:) :: local
          integer :: locallen
          integer :: ifldnum
          integer :: griddef,ngrdpts
          integer :: numoct_opt,interp_opt,num_opt
          integer,pointer,dimension(:) :: list_opt
          integer :: igdtnum,igdtlen
          integer,pointer,dimension(:) :: igdtmpl
          integer :: ipdtnum,ipdtlen
          integer,pointer,dimension(:) :: ipdtmpl
          integer :: num_coord
          real,pointer,dimension(:) :: coord_list
          integer :: ndpts,idrtnum,idrtlen
          integer,pointer,dimension(:) :: idrtmpl
          logical :: unpacked
          logical :: expanded
          integer :: ibmap
          logical*1,pointer,dimension(:) :: bmap
          real,pointer,dimension(:) :: fld
      end type gribfield

      end module
