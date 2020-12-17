PROGRAM test_g2lib
  implicit none

  integer , dimension(13)      :: listsec1
  integer , dimension(2)       :: listsec0
  integer                      :: ierr
  integer , parameter          :: lcgrib = 2000000
  character (lcgrib)           :: cgrib

  integer, dimension(5)                     :: igds
  integer, parameter                        :: igdstmplen = 25
  integer, dimension(igdstmplen)            :: igdstmpl
  integer, parameter                        :: idefnum = 0
  integer, dimension(idefnum)               :: ideflist

  integer                                   :: ipdsnum
  integer, parameter                        :: ipdstmplen = 15
  integer, dimension(ipdstmplen)            :: ipdstmpl
  integer                                   :: numcoord
  integer, dimension(1)                     :: coordlist
  integer                                   :: idrsnum
  integer, parameter                        :: idrstmplen = 7
  integer, dimension(idrstmplen)            :: idrstmpl
  integer                                   :: ibmap
  integer, dimension(1)                     :: bmap
  integer, parameter                        :: ngrdpts=20*25
  real                                      :: fld(ngrdpts)

  ! 
  ! Create the grib message
  !
  listsec0(1) = 0          ! Discipline (Table 0.0)
  listsec0(2) = 2          ! Grib edition number

  listsec1(1) = 255        ! Id of Originating Center (255 for missing)
  listsec1(2) = 255        ! Id of originating sub-center (255 for missing)
  listsec1(3) = 1          ! Master Table Version #
  listsec1(4) = 1          ! Local table version #
  listsec1(5) = 1          ! Significance of reference time, 1 indicates start of forecast

  listsec1(6) = 2005       ! Year of reference
  listsec1(7) = 09         ! Month of reference
  listsec1(8) = 30         ! Month of reference
  listsec1(9) = 0          ! Hour
  listsec1(10) = 0         ! Minute
  listsec1(11) = 0         ! Second
  listsec1(12) = 255       ! Production Status
  listsec1(13) = 1         ! Type of data (1 for forecast)

  call gribcreate(cgrib,lcgrib,listsec0,listsec1,ierr)
  if (ierr .ne. 0) then
     print *, 'gribcreate failed with ierr: ',ierr
  endif


  igds(1) = 0      ! Source of grid definition
  igds(2) = ngrdpts! Number of points in grid
  igds(3) = 0      ! 
  igds(4) = 0

  ! Here, setup the parameters that are common to all WRF projections

  igdstmpl(1) = 1       ! Shape of earth (1 for spherical with specified radius)
  igdstmpl(2) = 1       ! Scale factor for earth radius
  igdstmpl(3) = 6370*1000 ! Radius of earth
  igdstmpl(4) = 0       ! Scale factor for major axis
  igdstmpl(5) = 0       ! Major axis
  igdstmpl(6) = 0       ! Scale factor for minor axis
  igdstmpl(7) = 0       ! Minor axis
  igdstmpl(8) = 20      ! Number of points along x axis
  igdstmpl(9) = 25      ! Number of points along y axis


  ! This is the setup for lat/lon projection
  igds(5) = 0
  igdstmpl(10) = 0    ! Basic Angle of init projection (not important to us)
  igdstmpl(11) = 0    ! Subdivision of basic angle
  igdstmpl(12) = -20*1000000
  igdstmpl(13) = 20*1000000
  igdstmpl(14) = 128  ! Resolution and component flags
  igdstmpl(15) = 0*1000000
  igdstmpl(16) = 40*1000000
  
  ! Warning, the following assumes that dx and dy are valid at the equator.
  !    It is not clear in WRF where dx and dy are valid for latlon projections
  igdstmpl(17) = 12000 ! i-direction increment in micro degs
  igdstmpl(18) = 12000 ! j-direction increment in micro degs
  
  igdstmpl(19) = 64   ! Scanning mode

  call addgrid(cgrib,lcgrib,igds,igdstmpl,igdstmplen,ideflist,idefnum,ierr)
  if (ierr .ne. 0) then
     print *, 'addgrid failed with ierr: ',ierr
  endif


  ipdsnum = 0              ! Product definition template (0 for horiz grid)

  ipdstmpl(1) = 0          ! Parameter category
  ipdstmpl(2) = 1          ! Parameter number
  ipdstmpl(3) = 2          ! Type of generating process (2 for forecast)
  ipdstmpl(4) = 255        ! Background generating process id
  ipdstmpl(5) = 255        ! Analysis or forecast generating process id
  ipdstmpl(6) = 0          ! Data cutoff period (Hours)
  ipdstmpl(7) = 0          ! Data cutoff period (minutes)
  ipdstmpl(8) = 13         ! Time range indicator (13 for seconds)
  ipdstmpl(9) = 10800      ! Forecast time

  ipdstmpl(10) = 111       ! Type of first surface (111 for Eta level)
  ipdstmpl(11) = 0         ! Scale factor for 1st surface
  ipdstmpl(12) = 9965      ! First fixed surface
  ipdstmpl(13) = 255    ! Type of second fixed surface
  ipdstmpl(14) = 255 ! Scale factor for 2nd surface
  ipdstmpl(15) = 0
  numcoord = 0
  coordlist(1) = 0
  
  ! Set Data Representation templ (40 for JPEG2000, 41 for PNG)  
  idrsnum = 40
  idrstmpl(1) = 255       ! Reference value - ignored on input
  idrstmpl(2) = 0         ! Binary scale factor
  idrstmpl(3) = 3         ! Decimal scale factor 
  idrstmpl(4) = 0         ! number of bits for each data value - ignored on
  idrstmpl(5) = 0         ! Original field type - ignored on input
  idrstmpl(6) = 0         ! 0 for lossless compression
  idrstmpl(7) = 255       ! Desired compression ratio if idrstmpl(6) != 0
  ibmap = 255                ! Flag for bitmap
  
  fld = 1.2

  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, coordlist,      &
       numcoord, idrsnum, idrstmpl, idrstmplen, fld, ngrdpts, ibmap,          &
       bmap, ierr)
  
  if (ierr .ne. 0) then
     print *, 'addfield failed with ierr: ',ierr
  endif
  
  
end PROGRAM test_g2lib

