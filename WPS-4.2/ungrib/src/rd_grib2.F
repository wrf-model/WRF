*****************************************************************************!
! Subroutine RD_GRIB2                                                         !
!                                                                             !
! Purpose:                                                                    !
!    Read one record from the input GRIB2 file.  Based on the information in  !
!    the GRIB2 header and the user-defined Vtable, decide whether the field in!
!    the GRIB2 record is one to process or to skip.  If the field is one we   !
!    want to keep, extract the data from the GRIB2 record, and store the data !
!    in the ungrib memory structure.                                          !
!                                                                             !
! Argument list:                                                              !
!    Input:                                                                   !
!       junit   : "Unit Number" to open and read from.  Not really a Fortran  !
!                 unit number, since we do not do Fortran I/O for the GRIB2   !
!                 files.  Nor is it a UNIX File Descriptor returned from a C  !
!                 OPEN statement.  It is really just an array index to the    !
!                 array (IUARR) where the UNIX File Descriptor values are     !
!                 stored.                                                     !
!       gribflnm     : File name to open, if it is not already open.          !
!       debug_level  : Integer for various amounts of printout.               !
!       pmin         : Minimum pressure level (Pa) to process.                !
!                                                                             !
!    Output:                                                                  !
!                                                                             !
!       hdate        : The (up to)19-character date of the field to process.  !
!       grib_edition : Version of the gribfile (1 or 2)                       !
!       ireaderr     : Error flag: 0 - no error on read from GRIB2 file.      !
!                              1 - Hit the end of the GRIB2 file.             !
!                              2 - The file GRIBFLNM we tried to open does    !
!                                  not exist.                                 !
!                                                                             !
!                                                                             !
! Author: Paula McCaslin, NOAA/FSL,   Sept 2004                               !
! Code is based on code developed by Steve Gilbert NCEP & Kevin Manning NCAR  !
! Adapted for WPS: Jim Bresch, NCAR/MMM. Sept 2006                            !
!*****************************************************************************!
      
      SUBROUTINE rd_grib2(junit, gribflnm, hdate, 
     &  grib_edition, ireaderr, debug_level, pmin)

      use grib_mod
      use params
      use table          ! Included to define g2code
      use gridinfo       ! Included to define map%
      use storage_module ! Included sub put_storage
      use module_debug

      real, allocatable, dimension(:) :: hold_array
      parameter(msk1=32000,msk2=4000)
      character(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
      integer year, month, day, hour, minute, second, fcst
      character(len=*)  :: gribflnm
      character(len=*)  :: hdate
      character(len=8)  :: pabbrev
      character(len=20) :: labbrev
      character(len=80) :: tabbrev
      integer :: lskip, lgrib
      integer :: junit, itot, icount, iseek
      integer :: grib_edition
      integer :: i, j, ireaderr, ith , debug_level
      integer :: currlen
      logical :: unpack, expand
      type(gribfield) :: gfld
      ! For subroutine put_storage
      real :: level
      real :: scale_factor
      integer :: iplvl
      character (len=9) :: my_field
      character (len=8) :: tmp8
      ! For subroutine output
      integer , parameter :: maxlvl = 150
      real , dimension(maxlvl) :: plvl
      integer :: nlvl
      integer , dimension(maxlvl) :: level_array
      real :: glevel1, glevel2
      logical :: first = .true.
      real :: pmin

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET ARGUMENTS

      unpack=.true.
      expand=.true.
      hdate = '0000-00-00_00:00:00'
      ierr=0
      itot=0
      icount=0
      iseek=0
      lskip=0
      lgrib=0
      currlen=0
      ith=1
      scale_factor = 1e6
      call mprintf(.true.,DEBUG,"Begin rd_grib2", newline=.true.)

!/* IOS Return Codes from BACIO:  */
!/*  0    All was well                                   */
!/* -1    Tried to open read only _and_ write only       */
!/* -2    Tried to read and write in the same call       */
!/* -3    Internal failure in name processing            */
!/* -4    Failure in opening file                        */
!/* -5    Tried to read on a write-only file             */
!/* -6    Failed in read to find the 'start' location    */
!/* -7    Tried to write to a read only file             */
!/* -8    Failed in write to find the 'start' location   */
!/* -9    Error in close                                 */
!/* -10   Read or wrote fewer data than requested        */

!if ireaderr =1 we have hit the end of a file. 
!if ireaderr =2 we have hit the end of all the files. 
 

      ! Open a byte-addressable file.
      CALL BAOPENR(junit,gribflnm,IOS)
      first = .true.
      if (ios.eq.0) then 
      VERSION: do

         ! Search opend file for the next GRIB2 messege (record).
         call skgb(junit,iseek,msk1,lskip,lgrib)

         ! Check for EOF, or problem
         if (lgrib.eq.0) then
            exit 
         endif

         ! Check size, if needed allocate more memory.
         if (lgrib.gt.currlen) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lgrib),stat=is)
            !print *,'G2 allocate(cgrib(lgrib)) status: ',IS
            currlen=lgrib
         endif

         ! Read a given number of bytes from unblocked file.
         call baread(junit,lskip,lgrib,lengrib,cgrib)

         call mprintf ((lgrib.ne.lengrib),ERROR,
     &    "rd_grib2: IO Error. %i .ne. %i ", newline=.true.,
     &    i1=lgrib,i2=lengrib)

         iseek=lskip+lgrib
         icount=icount+1

         call mprintf (.true.,DEBUG,
     &     "G2 GRIB MESSAGE  %i starts at %i ", newline=.true.,
     &      i1=icount,i2=lskip+1)

         ! Unpack GRIB2 field
         call gb_info(cgrib,lengrib,listsec0,listsec1,
     &                numfields,numlocal,maxlocal,ierr)
         call mprintf((ierr.ne.0),ERROR,
     &     " ERROR querying GRIB2 message = %i",newline=.true.,i1=ierr)
         itot=itot+numfields

         grib_edition=listsec0(2)
         if (grib_edition.ne.2) then
              exit VERSION
         endif
         
         ! Additional print statments for developer.
!MGD         if ( debug_level .GT. 100 ) then
!MGD     print *,'G2 SECTION 0: ',(listsec0(j),j=1,3)
!MGD         print *,'G2 SECTION 1: ',(listsec1(j),j=1,13)
!MGD         print *,'G2 Contains ',numlocal,' Local Sections ',
!MGD     &           ' and ',numfields,' data fields.'
!MGD         endif


         ! ----
         ! Once per file fill in date, model and projection values.

         if (first) then 
           first = .false.

           ! Build the 19-character date string, based on GRIB2 header date
           ! and time information, including forecast time information:

           n=1
           call gf_getfld(cgrib,lengrib,n,.FALSE.,expand,gfld,ierr)


           year  =gfld%idsect(6)     !(FOUR-DIGIT) YEAR OF THE DATA
           month =gfld%idsect(7)     ! MONTH OF THE DATA
           day   =gfld%idsect(8)     ! DAY OF THE DATA
           hour  =gfld%idsect(9)     ! HOUR OF THE DATA
           minute=gfld%idsect(10)    ! MINUTE OF THE DATA
           second=gfld%idsect(11)    ! SECOND OF THE DATA

           fcst = 0

! Extract forecast time. Assume the first field's valid time is true for all fields.
! This doesn't have to be true, but ungrib is designed to decode one time-level at
! a time.

           if ( gfld%ipdtnum .ne. 8 ) then
	     if ( gfld%ipdtmpl(8) .eq. 1 ) then       ! time units are hours
	       fcst = gfld%ipdtmpl(9)
	     else if ( gfld%ipdtmpl(8) .eq. 0 ) then  ! minutes
	       fcst = gfld%ipdtmpl(9) / 60.
	     else if ( gfld%ipdtmpl(8) .eq. 2 ) then  ! days
	       fcst = gfld%ipdtmpl(9) * 24.
	     else
               call mprintf(.true.,ERROR,
     &           "Time unit in ipdtmpl(8), %i is not suported",
     &            newline=.true.,i1=gfld%ipdtmpl(8))
	     endif
	   else
!  pdt 4.8 data are time-averaged, accumulated, or min/max fields with the 
!  ending (valid) time provided. 
	     year  =gfld%ipdtmpl(16) 
	     month =gfld%ipdtmpl(17)
	     day   =gfld%ipdtmpl(18)
	     hour  =gfld%ipdtmpl(19)
	     minute=gfld%ipdtmpl(20) 
	     second=gfld%ipdtmpl(21) 
	     fcst = 0.
	   endif

           if ( gfld%idsect(5) .eq. 2 ) fcst = 0.
           ! Compute valid time.

           !print *, 'ymd',gfld%idsect(6),gfld%idsect(7),gfld%idsect(8)
           !print *, 'hhmm  ',gfld%idsect(9),gfld%idsect(10)
   
           call build_hdate(hdate,year,month,day,hour,minute,second)
           call mprintf(.true.,DEBUG,"G2 hdate = %s ", newline=.true.,
     &                  s1=hdate)
           call geth_newdate(hdate,hdate,3600*fcst)
           call mprintf(.true.,DEBUG,"G2 hdate (fcst?) = %s ",
     &                  newline=.true., s1=hdate)

           !--

           ! Indicator of the source (center) of the data.
           icenter = gfld%idsect(1)

           ! Indicator of model (or whatever) which generated the data.
           iprocess = gfld%ipdtmpl(5)


           if (icenter.eq.7) then
             if (iprocess.eq.81) then
               map%source = 'NCEP GFS Analysis'
             elseif (iprocess.eq.82) then
               map%source = 'NCEP GFS GDAS/FNL'
             elseif (iprocess.eq.83) then
               map%source = 'NCEP HRRR Model'
             elseif (iprocess.eq.84) then
               map%source = 'NCEP MESO NAM Model'
             elseif (iprocess.eq.89) then
               map%source = 'NCEP NMM '
             elseif (iprocess.eq.96) then
               map%source = 'NCEP GFS Model'
             elseif (iprocess.eq.86 .or. iprocess.eq.100) then
               map%source = 'NCEP RUC Model'    ! 60 km
             elseif (iprocess.eq.101) then
               map%source = 'NCEP RUC Model'    ! 40 km
             elseif (iprocess.eq.105) then
               if (year .gt. 2011) then
                 map%source = 'NCEP RAP Model'
               else
                 map%source = 'NCEP RUC Model'  ! 20 km
               endif
             elseif (iprocess.eq.107) then
               map%source = 'NCEP GEFS'
             elseif (iprocess.eq.109) then
               map%source = 'NCEP RTMA'
             elseif (iprocess.eq.140) then
               map%source = 'NCEP NARR'
             elseif (iprocess.eq.44) then
               map%source = 'NCEP SST Analysis'
             elseif (iprocess.eq.70) then
               map%source = 'GFDL Hurricane Model'
             elseif (iprocess.eq.80) then
               map%source = 'NCEP GFS Ensemble'
             elseif (iprocess.eq.107) then             ! renumbered as of 23 Feb 2010
               map%source = 'NCEP GFS Ensemble'
             elseif (iprocess.eq.111) then
               map%source = 'NCEP NMMB Model'
             elseif (iprocess.eq.112) then
               map%source = 'NCEP WRF-NMM Model'
             elseif (iprocess.eq.116) then
               map%source = 'NCEP WRF-ARW Model'
             elseif (iprocess.eq.129) then
               map%source = 'NCEP GODAS'
             elseif (iprocess.eq.197) then
               map%source = 'NCEP CDAS CFSV2'
             elseif (iprocess.eq.25) then
               map%source = 'NCEP SNOW COVER ANALYSIS'
             else
               map%source = 'unknown model from NCEP'
               call mprintf(.true.,STDOUT,
     &            "unknown model from NCEP %i ",newline=.true.,
     &            i1=iprocess)
               call mprintf(.true.,LOGFILE,
     &            "unknown model from NCEP %i ",newline=.true.,
     &            i1=iprocess)
             end if
           else if (icenter .eq. 57) then
             if (iprocess .eq. 87) then
               map%source = 'AFWA AGRMET'
             else
               map%source = 'AFWA'
             endif
           else if ( icenter .eq. 58 ) then
             map%source = 'US Navy FNOC'
           else if (icenter .eq. 59) then
             if (iprocess .eq. 125) then
               map%source = 'NOAA GSD Rapid Refresh Model'
             else if (iprocess .eq. 83) then
               map%source = 'NOAA GSD HRRR Model'
             else if (iprocess .eq. 105) then
               map%source = 'NOAA GSD'
             else 
               print *,'Unknown GSD source'
               stop
             endif
           else if (icenter .eq. 60) then
             map%source = 'NCAR'
           else if (icenter .eq. 98) then
             map%source = 'ECMWF'
           else if (icenter .eq. 34) then
             map%source = 'JMA'
           else if (icenter .eq. 74 .or. icenter .eq. 75 ) then
             map%source = 'UKMO'
           else
             map%source = 'unknown model and orig center'
           end if
           if (icenter.eq.7) then
             if (iprocess.eq.81 .or. iprocess.eq.82 .or.
     &           iprocess.eq.96) then
!  pmin should not be 15 or 40 hPa for gfs files. Use the next highest level.
               if (pmin .eq. 1500.) then
                 pmin = 1000.
               else if (pmin .eq. 4000.) then
                 pmin = 3000.
               endif
             endif
           endif

           call mprintf(.true.,DEBUG,"G2 source is = %s ",
     &                  newline=.true., s1=map%source)

           !--

           ! Store information about the grid containing the data.
           ! This stuff gets stored in the MAP variable, as defined in 
           ! module GRIDINFO.

           map%startloc = 'SWCORNER'
           map%grid_wind = .true.

           if (gfld%igdtnum.eq.0) then ! Lat/Lon grid aka Cylindrical Equidistant
              map%igrid = 0
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%dx = gfld%igdtmpl(17)
              map%dy = gfld%igdtmpl(18)
              map%lat1 = gfld%igdtmpl(12)
              map%lon1 = gfld%igdtmpl(13)
              write(tmp8,'(b8.8)') gfld%igdtmpl(14)
              if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
              map%r_earth = earth_radius (gfld%igdtmpl(1),
     &                         gfld%igdtmpl(2),gfld%igdtmpl(3))

! Fix for NCEP 1/12 degree grids (e.g. rtgsst)
              if (icenter .eq. 7 .and. map%dx .eq. 83000. .and. map%nx 
     &              .eq. 4320) then
                map%lat1 = 89958333.
                map%lon1 = 41667.
                map%dx = 83333.333 * sign(1.0,map%dx)
                map%dy = 83333.333 * sign(1.0,map%dy)
              endif

              if ((gfld%igdtmpl(10) .eq. 0).OR.
     &            (gfld%igdtmpl(10) .eq. 255)) THEN
          ! Scale lat/lon values to 0-180, default range is 1e6.
                map%lat1 = map%lat1/scale_factor
                map%lon1 = map%lon1/scale_factor
          ! Scale dx/dy values to degrees, default range is 1e6.
                map%dx = map%dx/scale_factor
                map%dy = map%dy/scale_factor
              else
          ! Basic angle and subdivisions are non-zero (not tested)
                map%lat1 = map%lat1 *
     &                     (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%lon1 = map%lon1 *
     &                     (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%dx = map%dx * 
     &                     (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%dy = map%dy * 
     &                     (gfld%igdtmpl(11)/gfld%igdtmpl(10))
               call mprintf(.true.,STDOUT,"WARNING - Basic angle option 
     &has not been tested, continuing anyway")
               call mprintf(.true.,LOGFILE,"WARNING - Basic angle option 
     & has not been tested, continuing anyway")
              endif


! The following is needed for NCEP GFS, 0.5 degree output. The j-scan is in the -y direction.
! In WPS, the sign of dy indicates the direction of the scan.
              write(tmp8,'(b8.8)') gfld%igdtmpl(19)
              read(tmp8,'(1x,i1)') jscan
              if ( jscan .eq. 0 .and. map%dy .gt. 0. ) then
                map%dy = -1. * map%dy
              endif
!             if ( map%lat1 .gt. gfld%igdtmpl(15) .and. 
!    &               map%dy .gt. 0. ) then
!               map%dy = -1. * map%dy
!               write(6,*) 'Resetting map%dy for iprocess = ',iprocess
!             endif

           elseif (gfld%igdtnum.eq.10) then ! Mercator Grid.
              map%igrid = 1
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%lov = 0.
              map%truelat1 = gfld%igdtmpl(13) / scale_factor
              map%truelat2 = 0.
              map%dx = gfld%igdtmpl(18) / scale_factor
              map%dy = gfld%igdtmpl(19) / scale_factor
              map%lat1 = gfld%igdtmpl(10) / scale_factor
              map%lon1 = gfld%igdtmpl(11) / scale_factor
              write(tmp8,'(b8.8)') gfld%igdtmpl(12)
              if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
              map%r_earth = earth_radius (gfld%igdtmpl(1),
     &                       gfld%igdtmpl(2),gfld%igdtmpl(3))

           elseif (gfld%igdtnum.eq.20) then ! Polar-Stereographic Grid.
              map%igrid = 5
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%lov = gfld%igdtmpl(14) / scale_factor
              map%truelat1 = 60.
              map%truelat2 = 91.
              map%dx = gfld%igdtmpl(15) / scale_factor
              map%dy = gfld%igdtmpl(16) / scale_factor
              map%lat1 = gfld%igdtmpl(10) / scale_factor
              map%lon1 = gfld%igdtmpl(11) / scale_factor
              write(tmp8,'(b8.8)') gfld%igdtmpl(12)
              if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
              map%r_earth = earth_radius (gfld%igdtmpl(1),
     &                       gfld%igdtmpl(2),gfld%igdtmpl(3))

           elseif (gfld%igdtnum.eq.30) then ! Lambert Conformal Grid
              map%igrid = 3
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%lov = gfld%igdtmpl(14) / scale_factor
              map%truelat1 = gfld%igdtmpl(19) / scale_factor
              map%truelat2 = gfld%igdtmpl(20) / scale_factor
              map%dx = gfld%igdtmpl(15) / scale_factor
              map%dy = gfld%igdtmpl(16) / scale_factor
              map%lat1 = gfld%igdtmpl(10) / scale_factor
              map%lon1 = gfld%igdtmpl(11) / scale_factor
              write(tmp8,'(b8.8)') gfld%igdtmpl(12)
              if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
              map%r_earth = earth_radius (gfld%igdtmpl(1),
     &                       gfld%igdtmpl(2),gfld%igdtmpl(3))

           elseif(gfld%igdtnum.eq.40) then ! Gaussian Grid (we will call it lat/lon)
              map%igrid = 4
              map%nx = gfld%igdtmpl(8)     ! Ni - # of points along a parallel
              map%ny = gfld%igdtmpl(9)     ! Nj - # of points along meridian
              map%dx = gfld%igdtmpl(17)    ! Di - i direction increment
              map%dy = gfld%igdtmpl(18)    ! N - # of parallels between pole and equator
              map%lat1 = gfld%igdtmpl(12)  ! La1 - lat of 1st grid point
              map%lon1 = gfld%igdtmpl(13)  ! Lo1 - lon of 1st grid point
              write(tmp8,'(b8.8)') gfld%igdtmpl(14)  ! resolution/component flag
              if (tmp8(5:5) .eq. '0') map%grid_wind = .false.
              map%r_earth = earth_radius (gfld%igdtmpl(1),
     &                       gfld%igdtmpl(2),gfld%igdtmpl(3))

              ! Scale dx/dy values to degrees, default range is 1e6.
              if (map%dx.gt.10000) then 
                 map%dx = map%dx/scale_factor
              endif
              if (map%dy.gt.10000) then 
                 map%dy = (map%dy/scale_factor)*(-1)
              endif

              ! Fix for zonal shift in CFSR data, following a similar fix 
              ! for global lat-lon data in rd_grib1.F
              if ( ABS(map%nx * map%dx - 360.0) < 1.0 ) then
                 if (ABS(map%dx - (360./real(map%nx))) > 0.00001) then
                    write(0,*) 'CFSR fix: recomputing delta-longitude'
                    map%dx = 360./real(map%nx)
                 endif
              endif

              ! Scale lat/lon values to 0-180, default range is 1e6.
              if (abs(map%lat1).ge.scale_factor) then
                 map%lat1 = map%lat1/scale_factor
              endif
              if (map%lon1.ge.scale_factor) then 
                 map%lon1 = map%lon1/scale_factor
              endif
              if ( debug_level .gt. 2 ) then
              call mprintf(.true.,DEBUG,
     &     "Gaussian Grid: Dx,Dy,lat,lon,nlats %f %f %f %f %i ",
     &     newline=.true.,f1=map%dx,f2=map%dy,f3=map%lat1,f4=map%lon1,
     &     i1=nint(map%dy))
              end if
           elseif (gfld%igdtnum.eq.32769) then ! Arakawa Non-E Staggered grid.
              map%igrid = 6               
              map%nx = gfld%igdtmpl(8)        
              map%ny = gfld%igdtmpl(9)        
              map%dx = gfld%igdtmpl(17)        
              map%dy = gfld%igdtmpl(18)       
              map%lat1 = gfld%igdtmpl(12)        
              map%lon1 = gfld%igdtmpl(13)        
              map%lat0 = gfld%igdtmpl(15)        
              map%lon0 = gfld%igdtmpl(16)        
              map%r_earth = earth_radius (gfld%igdtmpl(1),
     &                         gfld%igdtmpl(2),gfld%igdtmpl(3))
              if ((gfld%igdtmpl(10) .eq. 0).OR.
     &            (gfld%igdtmpl(10) .eq. 255)) THEN
                map%lat1 = map%lat1/scale_factor
                map%lon1 = map%lon1/scale_factor
                map%lat0 = map%lat0/scale_factor
                map%lon0 = map%lon0/scale_factor
                map%dx = map%dx/scale_factor/1.e3
                map%dy = map%dy/scale_factor/1.e3
              else
          ! Basic angle and subdivisions are non-zero (not tested)
                map%lat1 = map%lat1 *
     &                     (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%lon1 = map%lon1 *
     &                     (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%dx = map%dx * 
     &                     (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%dy = map%dy * 
     &                     (gfld%igdtmpl(11)/gfld%igdtmpl(10))
               call mprintf(.true.,STDOUT,"WARNING - Basic angle option 
     &has not been tested, continuing anyway")
               call mprintf(.true.,LOGFILE,"WARNING - Basic angle option 
     & has not been tested, continuing anyway")
              endif

           else
              call mprintf(.true.,STDOUT,"GRIB2 Unknown Projection: %i",
     &          newline=.true.,i1=gfld%igdtnum)
              call mprintf(.true.,STDOUT,
     &          "ungrib understands projections 0, 20, 30, and 40", 
     &          newline=.true.)
              call mprintf(.true.,LOGFILE,
     &          "GRIB2 Unknown Projection: %i",
     &          newline=.true.,i1=gfld%igdtnum)
              call mprintf(.true.,LOGFILE,
     &          "ungrib understands projections 0, 10, 20, 30, and 40", 
     &          newline=.true.)
       ! If the projection is not known, then it can't be processed by metgrid/plotfmt
                stop 'Stop in rd_grib2'
           endif

           call mprintf(.true.,DEBUG,"G2 igrid = %i ,  dx = %f ,  dy = %
     &f ", newline=.true., i1 = map%igrid, f1=map%dx, f2=map%dy)
         
           if (icenter.eq.7) then
             call ncep_grid_num (gfld%igdtnum)
           endif

           ! Deallocate arrays decoding GRIB2 record.
           call gf_free(gfld)

         endif   ! "first" if-block

         ! ----

         ! Continue to unpack GRIB2 field.
         NUM_FIELDS: do n = 1, numfields 
	   ! e.g. U and V would =2, otherwise its usually =1
           call gf_getfld(cgrib,lengrib,n,.FALSE.,expand,gfld,ierr)
           if (ierr.ne.0) then
             write(*,*) ' ERROR extracting field gf_getfld = ',ierr
             cycle
           endif

! The JMA GSM has two different grids in the same GRIB file, so we need
! to process the map info for each field separately. If any other centers do
! this, then processing will need to be added here, too.

            if (icenter .eq. 34 .and. gfld%igdtnum.eq.0) then
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%dx = gfld%igdtmpl(17)
              map%dy = gfld%igdtmpl(18)
              ! Scale dx/dy values to degrees, default range is 1e6.
              if (map%dx.gt.10000) then
                 map%dx = map%dx/scale_factor
              endif
              if (map%dy.gt.10000) then
                 map%dy = map%dy/scale_factor
              endif
              write(tmp8,'(b8.8)') gfld%igdtmpl(19)
              read(tmp8,'(1x,i1)') jscan
              write(0,*) 'gfld%igdtmpl(19) = ',gfld%igdtmpl(19),
     &   ' jscan = ',jscan
              if ( jscan .eq. 0 .and. map%dy .gt. 0. ) then
                map%dy = -1. * map%dy
              endif
            endif             ! JMA spectral

! ------------------------------------
         ! Additional print information for developer.
         if ( debug_level .GT. 1000 ) then
!MGD           print *
!MGD           print *,'G2 FIELD ',n
!MGD           if (n==1) then
!MGD            print *,'G2 SECTION 0: ',gfld%discipline,gfld%version
!MGD            print *,'G2 SECTION 1: ',(gfld%idsect(j),j=1,gfld%idsectlen)
!MGD           endif
!MGD           if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
!MGD              print *,'G2 SECTION 2: ',(gfld%local(j),j=1,gfld%locallen)
!MGD           endif
!MGD           print *,'G2 SECTION 3: ',gfld%griddef,gfld%ngrdpts,
!MGD     &                            gfld%numoct_opt,gfld%interp_opt,
!MGD     &                            gfld%igdtnum
!MGD           print *,'G2 GRID TEMPLATE 3.',gfld%igdtnum,': ',
!MGD     &            (gfld%igdtmpl(j),j=1,gfld%igdtlen)
!MGD           if ( gfld%num_opt .eq. 0 ) then
!MGD             print *,'G2 NO Section 3 List Defining No. of Data Points.'
!MGD           else
!MGD             print *,'G2 Section 3 Optional List: ',
!MGD     &                (gfld%list_opt(j),j=1,gfld%num_opt)
!MGD           endif
!MGD           print *,'G2 PRODUCT TEMPLATE 4.',gfld%ipdtnum,': ',
!MGD     &          (gfld%ipdtmpl(j),j=1,gfld%ipdtlen)

           pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),
     &                              gfld%ipdtmpl(2))
           !call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)
           !call prvtime(gfld%ipdtnum,gfld%ipdtmpl,listsec1,tabbrev)
!MGD            print *,'G2 TEXT: ',pabbrev,trim(labbrev)," ",trim(tabbrev)

!MGD           if ( gfld%num_coord .eq. 0 ) then
!MGD             print *,'G2 NO Optional Vertical Coordinate List.'
!MGD           else
!MGD             print *,'G2 Section 4 Optional Coordinates: ',
!MGD     &             (gfld%coord_list(j),j=1,gfld%num_coord)
!MGD           endif
           if ( gfld%ibmap .ne. 255 ) then
              call mprintf(.true.,DEBUG, 
     &             'G2 Num. of Data Points = %i with BIT-MAP %i', 
     &             newline=.true., i1=gfld%ndpts, i2=gfld%ibmap)
           else
              call mprintf(.true.,DEBUG, 
     &             'G2 Num. of Data Points = %i NO BIT-MAP', 
     &             newline=.true., i1=gfld%ndpts)
           endif
!MGD           print *,'G2 DRS TEMPLATE 5.',gfld%idrtnum,': ',
!MGD     &          (gfld%idrtmpl(j),j=1,gfld%idrtlen)
         endif ! Additional Print information 
! ------------------------------------

!         do i = 1, maxvar
!           write(6,'(a10,4i8)') namvar(i),(g2code(j,i),j=1,4)
!         enddo
!MGD      if (debug_level .gt. 50) then
!MGD          write(6,*) 'looking for ',gfld%discipline,gfld%ipdtmpl(1),
!MGD     &       gfld%ipdtmpl(2),gfld%ipdtmpl(10)
!MGD          endif
           call mprintf(.true.,DEBUG,"G2 Searching the g2code array (Vta
     &ble) for this grib field %i %i %i %i %i %i ", newline=.true.,
     & i1 = gfld%discipline, i2 = gfld%ipdtmpl(1),
     & i3 = gfld%ipdtmpl(2), i4 = gfld%ipdtmpl(10),
     & i5 = gfld%ipdtmpl(12), i6 = gfld%ipdtnum )


         ! Test this data record against list of desired variables 
         ! found in Vtable.
         ! ----
         MATCH_LOOP: do i=1,maxvar ! Max variables found in Vtable,
                                   ! maxvar is defined in table.mod

           if ( gfld%discipline .eq. g2code(1,i) .and.   !Discipline 
     &          gfld%ipdtmpl(1) .eq. g2code(2,i) .and.   !Category
     &          gfld%ipdtmpl(2) .eq. g2code(3,i) .and.   !Parameter
     &          gfld%ipdtmpl(10) .eq. g2code(4,i) .and.  !Elevation
     &          gfld%ipdtnum    .eq. g2code(5,i)) then   !Template

            call gf_free(gfld)
            call gf_getfld(cgrib,lengrib,n,.TRUE.,expand,gfld,ierr)
            pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),
     &                               gfld%ipdtmpl(2))

              !my_field (e.g. RH, TMP, similar to, but not the same as pabbrev)
              my_field=namvar(i) 

!MGD    if (debug_level .gt. 50) then
!MGD     write(6,*) 'namvar(i) = ',namvar(i),' pabbrev = ',pabbrev
!MGD     write(6,*) 'Parameter = ',gfld%ipdtmpl(2)
!MGD    endif
!  The following if-block is commented out since equivalent info can be obtained from g2print
!       if (debug_level .gt. 1000) then
!          fldmax=gfld%fld(1)
!          fldmin=gfld%fld(1)
!          sum=gfld%fld(1)
!          do j=2,gfld%ndpts
!            if (gfld%fld(j).gt.fldmax) fldmax=gfld%fld(j)
!            if (gfld%fld(j).lt.fldmin) fldmin=gfld%fld(j)
!            sum=sum+gfld%fld(j)
!          enddo ! gfld%ndpts
!          call mprintf(.true.,DEBUG,'G2 FIELD=%s MIN=%f AVG=%f MAX=%f',
!    &         newline=.true., s1=pabbrev, f1=fldmin, f2=sum/gfld%ndpts,
!    &         f3=fldmax)
!       endif

! need to match up soil levels with those requested.
! For the Vtable levels, -88 = all levels, -99 = missing. The units
! vary depending on the level code (e.g. 106 = cm, 103 = m).
! The grib2 standard allows scaling of the units, so make sure the soil level
! units are in cm (as used in the Vtable).
              if ( gfld%ipdtmpl(10) .eq. 106 ) then
                if ( ( gfld%ipdtmpl(14) .EQ. -1*(2**07-1) ) .AND.
!    &               ( gfld%ipdtmpl(15) .EQ. -1*(2**31-1) ) ) THEN ! Some compilers cannot
                                                                   ! handle the initial 2**31
                                                                   ! part of the computation, 
                                                                   ! which is an arithmetic 
                                                                   ! overflow on 32 bit signed ints
     &               ( gfld%ipdtmpl(15) .EQ. -2147483647  ) ) THEN
! special UM grib2
                   glevel1 = gfld%ipdtmpl(12)
                   glevel2 = gfld%ipdtmpl(11)
                else
                   glevel1 = 100. * gfld%ipdtmpl(12)*
     &                         (10.**(-1.*gfld%ipdtmpl(11)))
                   glevel2 = 100. * gfld%ipdtmpl(15)*
     &                         (10.**(-1.*gfld%ipdtmpl(14)))
                end if
                TMP8LOOP: do j = 1, maxvar
                  if ((g2code(4,j) .eq. 106) .and.
     &               (gfld%ipdtmpl(2) .eq. g2code(3,j)) .and.
     &               (glevel1 .eq. level1(j)) .and.
     &               ((glevel2 .eq. level2(j)) .or.
     &                                   (level2(j) .le. -88))) then
                    my_field = namvar(j)
                    exit TMP8LOOP
                  endif
                enddo TMP8LOOP
                if (j .gt. maxvar ) then
                  write(6,'(a,i6,a)') 'Subsoil level ',
     &               gfld%ipdtmpl(12), 
     &           ' in the GRIB2 file, was not found in the Vtable'
                  cycle MATCH_LOOP
                endif
!MGD         if (debug_level .gt. 50) write(6,*) 'my_field is now ',my_field
              endif

              ! Level (eg. 10000 mb)
              if(gfld%ipdtmpl(10).eq.100) then
                 ! Pressure level (range from 1000mb to 0mb)
                 level=gfld%ipdtmpl(12) *
     &                           (10. ** (-1. * gfld%ipdtmpl(11)))
		 if ( level .lt. pmin ) cycle MATCH_LOOP
              elseif((gfld%ipdtmpl(10).eq.105).or.
     &               (gfld%ipdtmpl(10).eq.118))then
                 ! Hybrid level (range from 1 to N)
                 level=gfld%ipdtmpl(12)
              elseif(gfld%ipdtmpl(10).eq.104) then
                 ! Sigma level (range from 10000 to 0)
                 level=gfld%ipdtmpl(12)
              elseif(gfld%ipdtmpl(10).eq.101) then
                 ! MSL
                 level=201300.
              elseif(gfld%ipdtmpl(10).eq.103) then
	         ! Height above ground (m)
		 if (gfld%ipdtmpl(12) .eq. 2. .or. 
     &               gfld%ipdtmpl(12) .eq. 1000. .or.     ! temp fix for hrrr maxref
     &               gfld%ipdtmpl(12) .eq. 10. ) then
                   level=200100.
		 else
                   cycle MATCH_LOOP
		 endif
              elseif((gfld%ipdtmpl(10).ge.206 .and.
     &               gfld%ipdtmpl(10).le.234) .or.
     &              (gfld%ipdtmpl(10).ge.242 .and.     
     &               gfld%ipdtmpl(10).le.254) .or.
     &              (gfld%ipdtmpl(10).eq.200) .or.
     &              (gfld%ipdtmpl(10).eq.10) ) then
                 ! NCEP cloud layers used for plotting
                   level=200100.
              elseif(gfld%ipdtmpl(10).eq.106.or.
     &               gfld%ipdtmpl(10).eq.1) then
                 ! Misc near ground/surface levels
                 level=200100.
              elseif(gfld%ipdtmpl(10).eq.6) then
                 ! Level of Max wind
                 level=200100.  
              elseif(gfld%ipdtmpl(10).eq.7) then
                 ! Tropopause
                 level=200100.
              else
                 ! If we are here then the Vtable contains a level code
                 ! which we cannot handle. Write an info message and skip it.
                 call mprintf(.true.,INFORM,"Rd_grib2 does not know abou
     &t level code %i (field = %s). Skipping this field. If you want thi
     &s level, rd_grib2.F must be modified", i1 = gfld%ipdtmpl(10),
     & s1 =  my_field )
                 cycle MATCH_LOOP
              endif
              iplvl = int(level)

              ! Store the unpacked 2D slab from the GRIB2 record
              allocate(hold_array(gfld%ngrdpts))
              do j=1,gfld%ngrdpts
                 hold_array(j)=gfld%fld(j)
              enddo

!   Some grids need to be reordered. Until we get an example, this is
!   a placeholder
!             call reorder_it (hold_array, map%nx, map%ny, map%dx, 
!    &                 map%dy, iorder)

              ! When we have reached this point, we have a data array ARRAY 
              ! which has some data we want to save, with field name FIELD 
              ! at pressure level LEVEL (Pa).  Dimensions of this data are 
              ! map%nx and map%ny.  Put this data into storage.

              !print *,'call put_storage',iplvl,my_field,hold_array(55),ith
              !e.g. call put_storage(200100, 'RH', my_field, 1, ith)
!             call mprintf(.true.,DEBUG,"Calling put_storage for
!    &level = %i , field = %s , g2level = %i ", newline=.true.,
!    & i1 = iplvl, s1 = my_field, i2 = gfld%ipdtmpl(12) )

              call put_storage(iplvl,my_field,
     &           reshape(hold_array(1:map%nx*map%ny),
     &           (/map%nx, map%ny/)), map%nx,map%ny)
              deallocate(hold_array)

              ! If Specific Humidity is present on hybrid levels AND 
              ! upper-air RH is missing, see if we can compute RH from 
              ! Specific Humidity.
              if (.not. is_there(iplvl, 'RH') .and.
     &            is_there(iplvl, 'SH') .and.
     &            is_there(iplvl, 'TT') .and.
     &            is_there(iplvl, 'P')) then
                  call g2_compute_rh_spechumd_upa(map%nx,map%ny,iplvl)
                 !call llstor_remove(iplvl, 'SH') !We are done with SH
              endif

              ! If Specific Humidity is present on hybrid levels AND 
              ! upper-air RH is missing, see if we can compute RH from 
              ! Specific Humidity - v2
              if (.not. is_there(iplvl, 'RH') .and.
     &            is_there(iplvl, 'SPECHUMD') .and.
     &            is_there(iplvl, 'THETA') .and.
     &            is_there(iplvl, 'TT')) then
                  call g2_compute_rh_spechumd_upa2(map%nx,map%ny,iplvl)
              endif

              ! If Temperature and Theta are present on hybrid levels AND
              ! upper-air PRESSURE is missing, see if we can compute PRESSURE from
              ! Temperature and Theta
              if (.not. is_there(iplvl, 'PRESSURE') .and.
     &            is_there(iplvl, 'THETA') .and.
     &            is_there(iplvl, 'TT')) then
                  call g2_compute_pressure_tth_upa(map%nx,map%ny,iplvl)
              endif

              ith=ith+1
              exit MATCH_LOOP

           endif ! Selected param.


         enddo MATCH_LOOP

         ! Deallocate arrays decoding GRIB2 record.
         call gf_free(gfld)

         enddo NUM_FIELDS


      enddo VERSION ! skgb


       if ( debug_level .gt. 100 ) then
         call mprintf (.true.,DEBUG,
     &   "G2 total number of fields found = %i ",newline=.true.,i1=itot)
       end if

       CALL BACLOSE(junit,IOS)

       nullify(gfld%local)            ! must be nullified before opening next file
       ireaderr=1
      else 
        call mprintf (.true.,DEBUG,"open status failed because %i ",
     &                newline=.true., i1=ios)
        hdate = '9999-99-99_99:99:99'
        ireaderr=2
      endif ! ireaderr check 

      END subroutine rd_grib2

!*****************************************************************************!
! Subroutine edition_num                                                      !
!                                                                             !
! Purpose:                                                                    !
!    Read one record from the input GRIB2 file.  Based on the information in  !
!    the GRIB2 header and the user-defined Vtable, decide whether the field in!
!    the GRIB2 record is one to process or to skip.  If the field is one we   !
!    want to keep, extract the data from the GRIB2 record, and pass the data  !
!    back to the calling routine.                                             !
!                                                                             !
! Argument list:                                                              !
!    Input:                                                                   !
!       JUNIT   : "Unit Number" to open and read from.  Not really a Fortran  !
!                 unit number, since we do not do Fortran I/O for the GRIB2   !
!                 files.  Nor is it a UNIX File Descriptor returned from a C  !
!                 OPEN statement.  It is really just an array index to the    !
!                 array (IUARR) where the UNIX File Descriptor values are     !
!                 stored.                                                     !
!       GRIB2FILE: File name to open, if it is not already open.              !
!                                                                             !
!    Output:                                                                  !
!       GRIB_EDITION: Set to 1 for GRIB and set to 2 for GRIB2                ! 
!       IERR     : Error flag: 0 - no error on read from GRIB2 file.          !
!                              1 - Hit the end of the GRIB2 file.             !
!                              2 - The file GRIBFLNM we tried to open does    !
!                                  not exist.                                 !
! Author: Paula McCaslin                                                      !
! NOAA/FSL                                                                    !
! Sept 2004                                                                   !
!*****************************************************************************!
      
      SUBROUTINE edition_num(junit, gribflnm, 
     &  grib_edition, ireaderr)

      use grib_mod
      use params
      use module_debug

      parameter(msk1=32000,msk2=4000)
      character(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
      character(len=*)  :: gribflnm
      integer :: lskip, lgrib
      integer :: junit
      integer :: grib_edition
      integer :: i, j, ireaderr
      integer :: currlen

      character(len=4) :: ctemp
      character(len=4),parameter :: grib='GRIB',c7777='7777'

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET ARGUMENTS

      itot=0
      icount=0
      iseek=0
      lskip=0
      lgrib=0
      currlen=0

!/* IOS Return Codes from BACIO:  */
!/*  0    All was well                                   */
!/* -1    Tried to open read only _and_ write only       */
!/* -2    Tried to read and write in the same call       */
!/* -3    Internal failure in name processing            */
!/* -4    Failure in opening file                        */
!/* -5    Tried to read on a write-only file             */
!/* -6    Failed in read to find the 'start' location    */
!/* -7    Tried to write to a read only file             */
!/* -8    Failed in write to find the 'start' location   */
!/* -9    Error in close                                 */
!/* -10   Read or wrote fewer data than requested        */

!if ireaderr =1 we have hit the end of a file. 
!if ireaderr =2 we have hit the end of all the files. 
!if ireaderr =3 beginning characters 'GRIB' not found

      ! Open a byte-addressable file.
      CALL BAOPENR(junit,gribflnm,IOS)
      if (ios.eq.0) then 

         ! Search opend file for the next GRIB2 messege (record).
         call skgb(junit,iseek,msk1,lskip,lgrib)

         ! Check for EOF, or problem
         call mprintf((lgrib.eq.0),ERROR,
     &     "Grib2 file or date problem, stopping in edition_num.",
     &     newline=.true.)
 
         ! Check size, if needed allocate more memory.
         if (lgrib.gt.currlen) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lgrib),stat=is)
            currlen=lgrib
         endif

         ! Read a given number of bytes from unblocked file.
         call baread(junit,lskip,lgrib,lengrib,cgrib)

         ! Check for beginning of GRIB message in the first 100 bytes
         istart=0
         do j=1,100
            ctemp=cgrib(j)//cgrib(j+1)//cgrib(j+2)//cgrib(j+3)
            if (ctemp.eq.grib ) then
              istart=j
              exit
            endif
         enddo
         if (istart.eq.0) then
            ireaderr=3
            print*, "The beginning 4 characters >GRIB< were not found."
         endif
   
         ! Unpack Section 0 - Indicator Section to extract GRIB edition field
         iofst=8*(istart+5)
         call gbyte(cgrib,discipline,iofst,8)     ! Discipline
         iofst=iofst+8
         call gbyte(cgrib,grib_edition,iofst,8)   ! GRIB edition number

!        print *, 'ungrib - grib edition num',  grib_edition
         CALL BACLOSE(junit,IOS)
         ireaderr=1
      else if (ios .eq. -4) then
        call mprintf(.true.,ERROR, 
     &    "edition_num: unable to open %s",newline=.true.,s1=gribflnm)
      else 
         print *,'edition_num: open status failed because',ios,gribflnm
         ireaderr=2
      endif ! ireaderr check 

      END subroutine edition_num

!*****************************************************************************!

      SUBROUTINE g2_compute_rh_spechumd_upa(ix, jx, iiplvl)
      ! Compute relative humidity from specific humidity in the upper air.
      use storage_module
      implicit none
      integer :: ix, jx
      integer :: iiplvl
      real :: lat1, lon1, dx, dy
      real, dimension(ix,jx) :: T, P, RH, Q
    
      real, parameter :: svp1=611.2
      real, parameter :: svp2=17.67
      real, parameter :: svp3=29.65
      real, parameter :: svpt0=273.15
      real, parameter :: eps = 0.622
    
      real startlat, startlon, deltalat, deltalon

      call get_storage(iiplvl, 'P', P, ix, jx)
      call get_storage(iiplvl, 'TT', T, ix, jx)
      call get_storage(iiplvl, 'SH', Q, ix, jx)
    
      rh=1.E2*(p*q/(q*(1.-eps)+eps))/(svp1*exp(svp2*(t-svpt0)/(T-svp3)))
     
      call put_storage(iiplvl, 'RH', rh, ix, jx)
    
      end subroutine g2_compute_rh_spechumd_upa

!*****************************************************************************!

      SUBROUTINE g2_compute_rh_spechumd_upa2(ix, jx, iiplvl)
      ! Compute relative humidity from specific humidity in the upper air.
      use storage_module
      implicit none
      integer :: ix, jx
      integer :: iiplvl
      real :: lat1, lon1, dx, dy
      real, dimension(ix,jx) :: T, TH, RH, Q, P
    
      real, parameter :: svp1=611.2
      real, parameter :: svp2=17.67
      real, parameter :: svp3=29.65
      real, parameter :: svpt0=273.15
      real, parameter :: eps = 0.622
    
      real startlat, startlon, deltalat, deltalon

      call get_storage(iiplvl, 'THETA', TH, ix, jx)
      call get_storage(iiplvl, 'TT', T, ix, jx)
      call get_storage(iiplvl, 'SPECHUMD', Q, ix, jx)
    
      p=1.e5*(t/th)**(1005/287.05)
     
      rh=1.E2*(p*q/(q*(1.-eps)+eps))/(svp1*exp(svp2*(t-svpt0)/(T-svp3)))
     
      call put_storage(iiplvl, 'RH', rh, ix, jx)
    
      end subroutine g2_compute_rh_spechumd_upa2

!*****************************************************************************!

      SUBROUTINE g2_compute_pressure_tth_upa(ix, jx, iiplvl)
      ! Compute relative humidity from specific humidity in the upper air.
      use storage_module
      implicit none
      integer :: ix, jx
      integer :: iiplvl
      real :: lat1, lon1, dx, dy
      real, dimension(ix,jx) :: T, TH, P
    
      real, parameter :: svp1=611.2
      real, parameter :: svp2=17.67
      real, parameter :: svp3=29.65
      real, parameter :: svpt0=273.15
      real, parameter :: eps = 0.622
    
      real startlat, startlon, deltalat, deltalon

      call get_storage(iiplvl, 'THETA', TH, ix, jx)
      call get_storage(iiplvl, 'TT', T, ix, jx)
    
      p=1.e5*(t/th)**(1005/287.05)
     
      call put_storage(iiplvl, 'PRESSURE', p, ix, jx)
    
      end subroutine g2_compute_pressure_tth_upa

!*****************************************************************************!

      subroutine ncep_grid_num (pnum)
!
!  Find the grib number for descriptive  labelling.
!  Grib2 doesn't have a grid-number entry, so we have to figure it out
!  from the parameters. 
!
      use gridinfo       ! Included to define map%
      integer :: pnum
      real, parameter :: eps = .01
      character (len=8) :: tmp8

!     write(6,*) 'begin ncep_grid_num'
!     write(6,*) 'dx = ',map%dx,' pnum = ',pnum,' nx = ',map%nx
      tmp8 = '        '
      if (pnum .eq. 30) then            ! lambert conformal
        if ( abs(map%dx - 12.19058) .lt. eps .and. map%nx .eq. 614) then
          write(tmp8,'("GRID 218")') 
        else if (abs(map%dx - 40.63525) .lt. eps 
     &     .and. map%nx .eq. 185) then
          write(tmp8,'("GRID 212")') 
        else if (abs(map%dx - 40.63525) .lt. eps 
     &     .and. map%nx .eq. 151) then
          write(tmp8,'("GRID 236")') 
        else if (abs(map%dx - 81.2705) .lt. eps 
     &     .and. map%nx .eq. 93) then
          write(tmp8,'("GRID 211")') 
        else if (abs (map%dx - 32.46341) .lt. eps 
     &     .and. map%nx .eq. 349) then
          write(tmp8,'("GRID 221")') 
        else if (abs(map%dx - 20.317625) .lt. eps 
     &     .and. map%nx .eq. 301) then
          write(tmp8,'("GRID 252")') 
        else if (abs(map%dx - 13.545087) .lt. eps 
     &     .and. map%nx .eq. 451) then
          write(tmp8,'("GRID 130")') 
        endif
      else if (pnum .eq. 20) then     ! polar stereographic
        if (abs(map%dx - 15.0) .lt. eps) then
          write(tmp8,'("GRID  88")') 
        endif
      else if (pnum .eq. 0) then      ! lat/lon
        if (abs(map%dx - 1.) .lt. eps .and. map%nx .eq. 360) then
          write(tmp8,'("GRID   3")') 
        else if (abs(map%dx - 0.5) .lt. eps .and. map%nx .eq. 720) then
          write(tmp8,'("GRID   4")') 
        endif
      endif
      map%source(25:32) = tmp8
!     write(6,*) 'map%source = ',map%source
      end subroutine ncep_grid_num
!*****************************************************************************!

      function earth_radius (icode, iscale, irad_m)
! Grib2 Code Table 3.2. Returns the spherical earth's radius in km.
      use module_debug
      real :: earth_radius
      integer :: icode
      integer :: iscale, irad_m
      if ( icode .eq. 0 ) then
        earth_radius = 6367470. * .001
      else if ( icode .eq. 1) then
        earth_radius = 0.001 * float(irad_m) / 10**iscale
      else if ( icode .eq. 6 ) then
        earth_radius = 6371229. * .001
      else if ( icode .eq. 8 ) then
        earth_radius = 6371200. * .001
      else
        call mprintf(.true.,ERROR,
     &    "unknown earth radius for code %i",newline=.true.,i1=icode)
      endif
      end function earth_radius
