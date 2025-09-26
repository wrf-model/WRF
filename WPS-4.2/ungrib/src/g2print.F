!*****************************************************************************!
  program g2print                                                             !
!                                                                             !
  use table
  use gridinfo
  use filelist
  implicit none
  interface
     subroutine parse_args(err, a1, h1, i1, l1, a2, h2, i2, l2,&
          a3, h3, i3, l3, hlast)
       integer :: err
       character(len=*) , optional :: a1, a2, a3
       character(len=*), optional :: h1, h2, h3
       integer , optional :: i1, i2, i3
       logical, optional :: l1, l2, l3
       character(len=*), optional :: hlast
     end subroutine parse_args
  end interface

  integer :: nunit1 = 12
  character(LEN=120) :: gribflnm 

  integer :: iprint

  integer , parameter :: maxlvl = 150

  real :: startlat, startlon, deltalat, deltalon
  real :: level
  character (LEN=9) ::  field
  character (LEN=3) ::  out_format

  logical :: readit

  integer, dimension(255) :: iuarr = 0

  character (LEN=19) :: HSTART, HEND, HDATE
  character(LEN=19) :: hsave  = '0000-00-00_00:00:00'
  integer :: itime
  integer :: ntimes
  integer :: interval
  integer :: ierr
  logical :: ordered_by_date
  integer :: debug_level
  integer :: grib_version
  integer :: vtable_columns

  character(len=30) :: hopt
  logical :: ivb = .FALSE.
  logical :: idb = .FALSE.

! -----------------

  gribflnm = '  '
  call parse_args(ierr, a1='v', l1=ivb, a2='V', l2=idb, hlast=gribflnm)
  if (ierr.ne.0) then
     call getarg(0, hopt)
     write(*,'(//,"Usage: ", A, " [-v] [-V] file",/)') trim(hopt)
     write(*,'("     -v   : Print more information about the GRIB records")')
     write(*,'("     -V   : Print way too much information about the GRIB&
          & records")')
     write(*,'("     file : GRIB file to read"//)')
     stop
  endif

! -----------------
! Determine GRIB Edition number
  grib_version=0
  call edition_num(nunit1, trim(gribflnm), grib_version, ierr)
  if (ierr.eq.3) STOP 'GRIB file problem' 


     debug_level = 0
     if (ivb) debug_level = 51
     if (idb) debug_level = 101
     write(6,*) 'reading from grib file = ',gribflnm

     LOOP1 : DO
        ! At the beginning of LOOP1, we are at a new time period.
        ! Clear the storage arrays and associated level information.

           ! If we need to read a new grib record, then read one.

              if (grib_version.ne.2) then 
!                write(6,*) 'calling r_grib1 with iunit ', nunit1
!                write(6,*) 'flnm = ',gribflnm
                 write(6,*) 'This is a Grib1 file. Please use g1print.\n'
                 stop
                 ! Read one record at a time from GRIB1 (and older Editions) 
!                call r_grib1(nunit1, gribflnm, level, field, &
!                     hdate, debug_level, ierr, iuarr, iprint)
              else 

                 ! Read one file of records from GRIB2.
                 if (debug_level .gt. 100) write(6,*) 'calling r_grib2'
                 call r_grib2(nunit1, gribflnm, hdate, &
                      grib_version, debug_level, ierr)

              endif

              if (ierr.eq.1) then 
                 ! We have hit the end of a file.  Exit LOOP1.
                 exit LOOP1
              endif

     enddo LOOP1

     if (grib_version.ne.2) then
        call c_close(iuarr(nunit1), iprint, ierr)
        iuarr(nunit1) = 0
     endif 

! And Now we are done:

   print*,' '
   print*,' '
   print*,'  Successful completion of g2print   '

contains
  subroutine sort_filedates
    implicit none

    integer :: n
    logical :: done
    if (nfiles > 1) then
       done = .FALSE.
       do while ( .not. done)
          done = .TRUE.
          do n = 1, nfiles-1
             if (filedates(n) > filedates(n+1)) then
                filedates(size(filedates)) = filedates(n)
                filedates(n) = filedates(n+1)
                filedates(n+1) = filedates(size(filedates))
                filedates(size(filedates)) = '0000-00-00 00:00:00.0000'
                done = .FALSE.
             endif
          enddo
       enddo
    endif
  end subroutine sort_filedates

end program g2print

!*****************************************************************************!
      
      SUBROUTINE r_grib2(junit, gribflnm, hdate,  &
        grib_edition, debug_level, ireaderr)

      use grib_mod
      use params
      use table          ! Included to define g2code
      use gridinfo       ! Included to define map%

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
      integer :: i, j, ireaderr, ith
      integer :: currlen
      logical :: unpack, expand
      type(gribfield) :: gfld
      real :: level
      real :: scale_factor
      integer :: iplvl, lvl2
      ! For subroutine output
      integer , parameter :: maxlvl = 150
      real , dimension(maxlvl) :: plvl
      integer :: nlvl
      integer , dimension(maxlvl) :: level_array
      logical :: verbose=.false.
      logical :: first = .true.
      integer :: debug_level
      character(len=4) :: tmp4
      character(len=40) :: string
      character(len=13) :: pstring = ',t50,":",i14)'
      character(len=15) :: rstring = ',t50,":",f14.5)'
      character(len=13) :: astring = ',t50,":",a14)'
      character(len=15) :: estring = ',t50,":",e14.5)'

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SET ARGUMENTS

      if (debug_level .gt. 50 ) then
        unpack=.true.
      else
        unpack=.false.
      endif
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
!     do j = 1,10
!       write(6,'("j = ",i4," level1 = ",i8," level2 = ",i8)') j, &
!         level1(j), level2(j)
!     enddo

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
 

      if ( debug_level .gt. 100 ) verbose = .true.
      if (verbose) write(6,*) 'begin r_grib2, flnm = ',gribflnm
      ! Open a byte-addressable file.
      CALL BAOPENR(junit,gribflnm,IOS)
      first = .true.
      if (verbose) write(6,*) 'back from baopenr, ios = ',ios
      if (ios.eq.0) then 
      VERSION: do

         ! Search opend file for the next GRIB2 messege (record).
      if (verbose) write(6,*) 'calling skgb'
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

         if (lgrib.ne.lengrib) then
            print *,'G2 r_grib2: IO Error.',lgrib,".ne.",lengrib
            stop 9 
         endif
         iseek=lskip+lgrib
         icount=icount+1

         if (verbose)  PRINT *,'G2 GRIB MESSAGE ',icount,' starts at',lskip+1

         ! Unpack GRIB2 field
         call gb_info(cgrib,lengrib,listsec0,listsec1, &
                      numfields,numlocal,maxlocal,ierr)
         if (ierr.ne.0) then
           write(*,*) ' ERROR querying GRIB2 message = ',ierr
           stop 10
         endif
         itot=itot+numfields

         grib_edition=listsec0(2)
         if (grib_edition.ne.2) then
              exit VERSION
         endif
         
         ! Additional print statments for developer.
         if (verbose) then
           print *,'G2 SECTION 0: ',(listsec0(j),j=1,3)
           print *,'G2 SECTION 1: ',(listsec1(j),j=1,13)
           print *,'G2 Contains ',numlocal,' Local Sections ', &
                 ' and ',numfields,' data fields.'
         endif

         ! ----
         ! Once per file fill in date, model and projection values.

         if (first) then 
           first = .false.

           ! Build the 19-character date string, based on GRIB2 header date
           ! and time information, including forecast time information:

           n=1
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           
          if (debug_level .gt. 100 ) then
          write(6,*) 'gfld%version = ',gfld%version
          if (gfld%discipline .eq. 0) then
            string = 'Meteorological products'
          else if (gfld%discipline .eq. 1) then
            string = 'Hydrological products'
          else if (gfld%discipline .eq. 2) then
            string = 'Land Surface products'
          else 
            string = 'See code table 0.0'
          endif
          write(6,*) 'Discipline = ',gfld%discipline,'   ',string
          write(6,*) 'gfld%idsect(1) = ',gfld%idsect(1)
          write(6,*) 'gfld%idsect(2) = ',gfld%idsect(2)
          write(6,*) 'gfld%idsect(3) = ',gfld%idsect(3)
          write(6,*) 'gfld%idsect(4) = ',gfld%idsect(4)
          write(6,*) 'gfld%idsect(5) = ',gfld%idsect(5)
          write(6,*) 'gfld%idsect(6) = ',gfld%idsect(6)
          write(6,*) 'gfld%idsect(7) = ',gfld%idsect(7)
          write(6,*) 'gfld%idsect(8) = ',gfld%idsect(8)
          write(6,*) 'gfld%idsect(9) = ',gfld%idsect(9)
          write(6,*) 'gfld%idsect(10) = ',gfld%idsect(10)
          write(6,*) 'gfld%idsect(11) = ',gfld%idsect(11)
          write(6,*) 'gfld%idsect(12) = ',gfld%idsect(12)
          write(6,*) 'gfld%idsect(13) = ',gfld%idsect(13)

          write(6,*) 'gfld%idsectlen = ',gfld%idsectlen
          write(6,*) 'gfld%locallen = ',gfld%locallen
          write(6,*) 'gfld%ifldnum = ',gfld%ifldnum
          write(6,*) 'gfld%ngrdpts = ',gfld%ngrdpts
          write(6,*) 'gfld%numoct_opt = ',gfld%numoct_opt
          write(6,*) 'gfld%interp_opt = ',gfld%interp_opt

          write(6,*) 'gfld%griddef = ',gfld%griddef
          if (gfld%igdtnum .eq. 0) then
            string = 'Lat/Lon cylindrical equidistant'
          else if (gfld%igdtnum .eq. 1) then
            string = 'Rotated Lat/Lon'
          else if (gfld%igdtnum .eq. 2) then
            string = 'Stretched Lat/Lon'
          else if (gfld%igdtnum .eq. 20) then
            string = 'Polar Stereographic'
          else if (gfld%igdtnum .eq. 30) then
            string = 'Lambert Conformal'
          else if (gfld%igdtnum .eq. 40) then
            string = 'Gaussian Lat/Lon'
          else if (gfld%igdtnum .eq. 50) then
            string = 'Spherical harmonic coefficients'
          else
            string = 'see code table 3.1'
          endif
          write(6,*) 'Grid Template number = ',gfld%igdtnum,'   ',string
          write(6,*) 'gfld%igdtlen = ',gfld%igdtlen
          do i = 1, gfld%igdtlen
            write(6,*) 'gfld%igdtmpl(',i,') = ',gfld%igdtmpl(i)
          enddo

          write(6,*) 'gfld%ipdtnum = ',gfld%ipdtnum
          write(6,*) 'gfld%ipdtlen = ',gfld%ipdtlen
          if ( gfld%ipdtnum .eq. 0 ) then
            do i = 1, gfld%ipdtlen
              write(6,*) 'gfld%ipdtmpl(',i,') = ',gfld%ipdtmpl(i)
            enddo
          endif
          write(6,*) 'gfld%num_coord = ',gfld%num_coord
          write(6,*) 'gfld%ndpts = ',gfld%ndpts
          write(6,*) 'gfld%idrtnum = ',gfld%idrtnum
          write(6,*) 'gfld%idrtlen = ',gfld%idrtlen
          write(6,*) 'gfld%expanded = ',gfld%expanded
          write(6,*) 'gfld%ibmap = ',gfld%ibmap
          endif

           year  =gfld%idsect(6)     !(FOUR-DIGIT) YEAR OF THE DATA
           month =gfld%idsect(7)     ! MONTH OF THE DATA
           day   =gfld%idsect(8)     ! DAY OF THE DATA
           hour  =gfld%idsect(9)     ! HOUR OF THE DATA
           minute=gfld%idsect(10)    ! MINUTE OF THE DATA
           second=gfld%idsect(11)    ! SECOND OF THE DATA

           fcst = 0

           ! Extract forecast time.
           if ( gfld%ipdtmpl(8) .eq. 1 ) then   ! time units are hours
             fcst = gfld%ipdtmpl(9)
           else if ( gfld%ipdtmpl(8) .eq. 0 ) then  ! minutes
             fcst = gfld%ipdtmpl(9) / 60.
           else if ( gfld%ipdtmpl(8) .eq. 2 ) then  ! days
             fcst = gfld%ipdtmpl(9) * 24.
           else
             fcst = 999
           endif

           ! Compute valid time. 

          if (verbose) then
            print *, 'ymd',gfld%idsect(6),gfld%idsect(7),gfld%idsect(8)
            print *, 'hhmm  ',gfld%idsect(9),gfld%idsect(10)
          endif
   
           call build_hdate(hdate,year,month,day,hour,minute,second)
           if (verbose) print *, 'G2 hdate = ',hdate
!          call geth_newdate(hdate,hdate,3600*fcst)   ! no need for this in print
!          print *, 'G2 hdate (fcst?) = ',hdate

           !--

           ! Indicator of the source (center) of the data.
           icenter = gfld%idsect(1)

           ! Indicator of model (or whatever) which generated the data.
           iprocess = gfld%ipdtmpl(5)


           if (icenter.eq.7) then
! Values obtained from http://www.nco.ncep.noaa.gov/pmb/docs/on388/tablea.html
! Note that NCEP recycles process numbers. This may cause labelling issues for
! ancient datasets.
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
             elseif (iprocess.eq.107) then          ! renumbered as of 23 Feb 2010
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
               write (6,*) 'unknown NCEP model, iprocess = ',iprocess
             end if
           else if (icenter .eq. 57) then
             if (iprocess .eq. 87) then
               map%source = 'AFWA AGRMET'
             else
               map%source = 'AFWA'
             endif
           else if (icenter .eq. 58) then
               map%source = 'US Navy FNOC'
           else if (icenter .eq. 98) then
             map%source = 'ECMWF'
           else if (icenter .eq. 34) then
             map%source = 'JMA'
           else if (icenter .eq. 74 .or. icenter .eq. 75 ) then
             map%source = 'UKMO'
           else
             map%source = 'unknown model and orig center'
           end if
           write (6,*) '     ',map%source

          if (debug_level .le. 50) then
          write(6,*) '---------------------------------------------------------------------------------------'
          write(6,*) ' rec Prod Cat Param  Lvl    Lvl      Lvl     Prod    Name            Time          Fcst'
          write(6,*) ' num Disc     num    code   one      two     Templ                                 hour'
          write(6,*) '---------------------------------------------------------------------------------------'
          endif


           !--

           ! Store information about the grid on which the data is. 
           ! This stuff gets stored in the MAP variable, as defined in 
           ! module GRIDINFO.

           map%startloc = 'SWCORNER'

           if (gfld%igdtnum.eq.0) then ! Lat/Lon grid aka Cylindrical Equidistant
              map%igrid = 0
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%dx = gfld%igdtmpl(17)
              map%dy = gfld%igdtmpl(18)
              map%lat1 = gfld%igdtmpl(12)
              map%lon1 = gfld%igdtmpl(13)

              if ((gfld%igdtmpl(10) .eq. 0).OR.   &
                  (gfld%igdtmpl(10) .eq. 255)) THEN
          ! Scale lat/lon values to 0-180, default range is 1e6.
                map%lat1 = map%lat1/scale_factor
                map%lon1 = map%lon1/scale_factor
          ! Scale dx/dy values to degrees, default range is 1e6.
                map%dx = map%dx/scale_factor
                map%dy = map%dy/scale_factor
              else
          ! Basic angle and subdivisions are non-zero (not tested)
                map%lat1 = map%lat1 *  &
                           (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%lon1 = map%lon1 *  &
                           (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%dx = map%dx *  &
                           (gfld%igdtmpl(11)/gfld%igdtmpl(10))
                map%dy = map%dy *  &
                           (gfld%igdtmpl(11)/gfld%igdtmpl(10))
              endif

           elseif (gfld%igdtnum.eq.30) then ! Lambert Conformal Grid
              map%igrid = 3
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%lov = gfld%igdtmpl(14)
              map%truelat1 = gfld%igdtmpl(19)
              map%truelat2 = gfld%igdtmpl(20)
              map%dx = gfld%igdtmpl(15)
              map%dy = gfld%igdtmpl(16)
              map%lat1 = gfld%igdtmpl(10)
              map%lon1 = gfld%igdtmpl(11)

           elseif(gfld%igdtnum.eq.40) then ! Gaussian Grid (we will call it lat/lon)
              map%igrid = 0
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%dx = gfld%igdtmpl(17)
              map%dy = gfld%igdtmpl(18) ! ?not in Grid Definition Template 3.40 doc
              map%lat1 = gfld%igdtmpl(12)
              map%lon1 = gfld%igdtmpl(13)

              ! Scale dx/dy values to degrees, default range is 1e6.
              if (map%dx.gt.10000) then 
                 map%dx = map%dx/scale_factor
              endif
              if (map%dy.gt.10000) then 
                 map%dy = (map%dy/scale_factor)*(-1)
              endif

              ! Scale lat/lon values to 0-180, default range is 1e6.
              if (abs(map%lat1).ge.scale_factor) then 
                 map%lat1 = map%lat1/scale_factor
              endif
              if (map%lon1.ge.scale_factor) then 
                 map%lon1 = map%lon1/scale_factor
              endif
           print *,'Gaussian Grid: Dx,Dy,lat,lon',map%dx,map%dy, &
             map%lat1,map%lon1

           elseif (gfld%igdtnum.eq.20) then ! Polar-Stereographic Grid.
              map%igrid = 5
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              !map%lov = gfld%igdtmpl(x) ! ?not in Grid Definition Template 3.20 doc
              map%truelat1 = 60.
              map%truelat2 = 91.
              !map%dx = gfld%igdtmpl(x)
              !map%dy = gfld%igdtmpl(x)
              map%lat1 = gfld%igdtmpl(10)
              map%lon1 = gfld%igdtmpl(11)

           else
              print*, 'GRIB2 Unknown Projection: ',gfld%igdtnum
              print*, 'see Code Table 3.1: Grid Definition Template No'
           endif
         
            call gf_free(gfld)
         endif

         ! ----

         ! Continue to unpack GRIB2 field.
         if (debug_level .gt. 100) write(6,*) 'numfields = ',numfields
         NUM_FIELDS: do n = 1, numfields 
! e.g. U and V would =2, otherwise its usually =1
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           if (ierr.ne.0) then
             write(*,*) ' ERROR extracting field gf_getfld = ',ierr
             cycle
           endif

! ------------------------------------
         ! Additional print information for developer.
         pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),  &
                                  gfld%ipdtmpl(2))
         if (debug_level .gt. 50 ) then
           print *
!          print *,'G2 FIELD ',n
           if (n==1) then
            write(*,'(/,"GRIB2 SECTION 0 - INDICATOR SECTION:")')
            write(*,'(5x,"Discipline"'//pstring) gfld%discipline
            write(*,'(5x,"GRIB Edition Number"'//pstring) gfld%version
            write(*,'(5x,"GRIB length"'//pstring) lengrib
            write(*,'(/,"GRIB2 SECTION 1 - IDENTIFICATION SECTION:")')
            write(*,'(5x,"Length of Section"'//pstring) gfld%idsectlen
            write(*,'(5x,"Originating Center ID"'//pstring) &
               gfld%idsect(1)
            write(*,'(5x,"Subcenter ID"'//pstring) gfld%idsect(2)
            write(*,'(5x,"GRIB Master Table Version"'//pstring) &
            gfld%idsect(3)
            write(*,'(5x,"GRIB Local Table Version"'//pstring) &
            gfld%idsect(4)
            write(*,'(5x,"Significance of Reference Time"'//pstring) &
            gfld%idsect(5)
            write(*,'(5x,"Year"'//pstring)  gfld%idsect(6)
            write(*,'(5x,"Month"'//pstring)  gfld%idsect(7)
            write(*,'(5x,"Day"'//pstring)  gfld%idsect(8)
            write(*,'(5x,"Hour"'//pstring)  gfld%idsect(9)
            write(*,'(5x,"Minute"'//pstring)  gfld%idsect(10)
            write(*,'(5x,"Second"'//pstring)  gfld%idsect(11)
            write(*,'(5x,"Production Status of data"'//pstring) &
              gfld%idsect(12)
            write(*,'(5x,"Type of processed data"'//pstring) &
              gfld%idsect(13)
!           print *,'G2 SECTION 1: ',(gfld%idsect(j),j=1,gfld%idsectlen)
           endif
           write(*,'(/,"GRIB2 SECTION 2 - LOCAL SECTION:")')
           write(*,'(5x,"Length of Section 2"'//pstring) gfld%locallen
           if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
           do j = 1, gfld%locallen
             write(*,'(5x,"Local value "'//astring) gfld%local(j)
           enddo
!             print *,'G2 SECTION 2: ',(gfld%local(j),j=1,gfld%locallen)
           endif
           write(*,'(/,"GRIB2 SECTION 3 - GRID DEFINITION SECTION:")')
!          write(*,'(5x,"Length of Section 3"'//pstring) gfld%unknown
           write(*,'(5x,"Source of grid definition"'&
           //pstring) gfld%griddef
           write(*,'(5x,"Number of grid points"'//pstring) gfld%ngrdpts
           write(*,'(5x,"Number of octets for addnl points"'//pstring) &
           gfld%numoct_opt
           write(*,'(5x,"Interpretation list"'//pstring) &
           gfld%interp_opt
           write(*,'(5x,"Grid Definition Template Number"'//pstring) &
           gfld%igdtnum
           if (gfld%igdtnum .eq. 0 .or. gfld%igdtnum .eq. 1 .or.  &
               gfld%igdtnum .eq. 2 .or. gfld%igdtnum .eq. 3 ) then
             if (gfld%igdtnum .eq. 0 ) then
               write(*,'(5x,"Lat/Lon or Cylindrical Equidistant Grid")')
             else if (gfld%igdtnum .eq. 1 ) then
               write(*,'(5x,"Rotated Lat/Lon or Cylind. Equi. Grid")')
             else if (gfld%igdtnum .eq. 2 ) then
               write(*,'(5x,"Stretched Lat/Lon or Cylind. Equi. Grid")')
             else if (gfld%igdtnum .eq. 3 ) then
               write(*,'(5x,"Stretched and Rotated Lat/Lon Grid")')
             endif
             write(*,'(10x,"Shape of the Earth"'//pstring) &
                gfld%igdtmpl(1)
             write(*,'(10x,"Scale factor of spher. Earth"'//pstring) &
                gfld%igdtmpl(2)
             write(*,'(10x,"Scaled value of spher. Earth"'//pstring) &
                gfld%igdtmpl(3)
             write(*,'(10x,"Scale factor of major axis"'//pstring) &
                gfld%igdtmpl(4)
             write(*,'(10x,"Scaled value of major axis"'//pstring) &
                gfld%igdtmpl(5)
             write(*,'(10x,"Scale factor of minor axis"'//pstring) &
                gfld%igdtmpl(6)
             write(*,'(10x,"Scaled value of minor axis"'//pstring) &
                gfld%igdtmpl(7)
             write(*,'(10x,"Ni - points along a parallel"'//pstring) &
                 gfld%igdtmpl(8)
             write(*,'(10x,"Nj - points along a meridian"'//pstring) &
                 gfld%igdtmpl(9)
             write(*,'(10x,"Basic angle of initial domain"'//pstring)&
                 gfld%igdtmpl(10)
             write(*,'(10x,"Subdivisions of basic angle"'//pstring) &
                 gfld%igdtmpl(11)
             write(*,'(10x,"La1"'//pstring)  gfld%igdtmpl(12)
             write(*,'(10x,"Lo1"'//pstring)  gfld%igdtmpl(13)
             write(*,'(10x,"Resolution and Component",t50,":",B14.8)')&
              gfld%igdtmpl(14)
             write(*,'(10x,"La2"'//pstring) gfld%igdtmpl(15)
             write(*,'(10x,"Lo2"'//pstring) gfld%igdtmpl(16)
             write(*,'(10x,"Di - i-dir increment"'//pstring) &
               gfld%igdtmpl(17)
             write(*,'(10x,"Dj - j-dir increment"'//pstring) &
               gfld%igdtmpl(18)
             write(*,'(10x,"Scanning mode"'//pstring) &
               gfld%igdtmpl(19)
             if (gfld%igdtnum .eq. 1) then
             write(*,'(10x,"Lat of southern pole of project"'//pstring)&
               gfld%igdtmpl(20)
             write(*,'(10x,"Lon of southern pole of project"'//pstring)&
               gfld%igdtmpl(21)
             write(*,'(10x,"Angle of rotation of projection"'//pstring)&
               gfld%igdtmpl(22)
             else if (gfld%igdtnum .eq. 2) then
             write(*,'(10x,"Lat of the pole of stretching "'//pstring)&
               gfld%igdtmpl(20)
             write(*,'(10x,"Lon of the pole of stretching "'//pstring)&
               gfld%igdtmpl(21)
             write(*,'(10x,"Stretching factor"'//pstring) &
               gfld%igdtmpl(22)
             else if (gfld%igdtnum .eq. 3) then
             write(*,'(10x,"Lat of southern pole of project"'//pstring)&
               gfld%igdtmpl(20)
             write(*,'(10x,"Lon of southern pole of project"'//pstring)&
               gfld%igdtmpl(21)
             write(*,'(10x,"Angle of rotation of projection"'//pstring)&
               gfld%igdtmpl(22)
             write(*,'(10x,"Lat of the pole of stretching "'//pstring)&
               gfld%igdtmpl(23)
             write(*,'(10x,"Lon of the pole of stretching "'//pstring)&
               gfld%igdtmpl(24)
             write(*,'(10x,"Stretching factor"'//pstring) &
               gfld%igdtmpl(25)
             endif
           else if (gfld%igdtnum .eq. 10) then
             write(*,'(5x,"Mercator Grid")')
           else if (gfld%igdtnum .eq. 20 .or. gfld%igdtnum .eq. 30) then
             if (gfld%igdtnum .eq. 20) then
               write(*,'(5x,"Polar Stereographic Grid")')
             else if (gfld%igdtnum .eq. 30) then
               write(*,'(5x,"Lambert Conformal Grid")')
             endif
             write(*,'(10x,"Shape of the Earth"'//pstring) &
                gfld%igdtmpl(1)
             write(*,'(10x,"Scale factor of spher. Earth"'//pstring) &
                gfld%igdtmpl(2)
             write(*,'(10x,"Scaled value of spher. Earth"'//pstring) &
                gfld%igdtmpl(3)
             write(*,'(10x,"Scale factor of major axis"'//pstring) &
                gfld%igdtmpl(4)
             write(*,'(10x,"Scaled value of major axis"'//pstring) &
                gfld%igdtmpl(5)
             write(*,'(10x,"Scale factor of minor axis"'//pstring) &
                gfld%igdtmpl(6)
             write(*,'(10x,"Scaled value of minor axis"'//pstring) &
                gfld%igdtmpl(7)
             write(*,'(10x,"Nx"'//pstring) gfld%igdtmpl(8)
             write(*,'(10x,"Ny"'//pstring) gfld%igdtmpl(9)
             write(*,'(10x,"La1"'//pstring) gfld%igdtmpl(10)
             write(*,'(10x,"Lo1"'//pstring) gfld%igdtmpl(11)
             write(*,'(10x,"Resolution and Component",t50,":",B14.8)')&
              gfld%igdtmpl(12)
             write(*,'(10x,"LaD"'//pstring) gfld%igdtmpl(13)
             write(*,'(10x,"LoV"'//pstring) gfld%igdtmpl(14)
             write(*,'(10x,"Dx"'//pstring) gfld%igdtmpl(15)
             write(*,'(10x,"Dy"'//pstring) gfld%igdtmpl(16)
             write(*,'(10x,"Projection Center Flag"'//pstring) &
               gfld%igdtmpl(17)
             write(*,'(10x,"Scanning mode"'//pstring) &
               gfld%igdtmpl(18)
             if (gfld%igdtnum .eq. 30) then
             write(*,'(10x,"Latin 1 "'//pstring) &
               gfld%igdtmpl(19)
             write(*,'(10x,"Latin 2 "'//pstring) &
               gfld%igdtmpl(20)
             write(*,'(10x,"Lat of southern pole of project"'//pstring)&
               gfld%igdtmpl(21)
             write(*,'(10x,"Lon of southern pole of project"'//pstring)&
               gfld%igdtmpl(22)
             endif
           else if (gfld%igdtnum .eq. 40 .or. gfld%igdtnum .eq. 41) then
             if (gfld%igdtnum .eq. 40) then
               write(*,'(5x,"Gaussian Lat/Lon Grid")')
             else if (gfld%igdtnum .eq. 41) then
               write(*,'(5x,"Rotated Gaussian Lat/Lon Grid")')
             else if (gfld%igdtnum .eq. 42) then
               write(*,'(5x,"Stretched Gaussian Lat/Lon Grid")')
             else if (gfld%igdtnum .eq. 43) then
               write(*,'(5x,"Stretched and Rotated Gaussian Lat/Lon ")')
             endif
           else
           do j = 1, gfld%igdtlen
             write(*,'(5x,"Grid Definition Template entry "'//pstring) &
             gfld%igdtmpl(j)
           enddo
           endif
!          print *,'G2 SECTION 3: ',gfld%griddef,gfld%ngrdpts, &
!                                 gfld%numoct_opt,gfld%interp_opt,  &
!                                 gfld%igdtnum
!          print *,'G2 GRID TEMPLATE 3.',gfld%igdtnum,': ',   &
!                 (gfld%igdtmpl(j),j=1,gfld%igdtlen)
           if ( gfld%num_opt .eq. 0 ) then
!            print *,'G2 NO Section 3 List Defining No. of Data Points.'
           else
             print *,'G2 Section 3 Optional List: ',     &
                      (gfld%list_opt(j),j=1,gfld%num_opt)
           endif
          write(*,'(/,"GRIB2 SECTION 4 - PRODUCT DEFINITION SECTION:")')
!          write(*,'(5x,"Length of Section 4"'//pstring) gfld%unknown
           write(*,'(5x,"Product Definition Template Number"'//pstring)&
            gfld%ipdtnum
            do j = 1, gfld%ipdtlen
              write(tmp4,'(i4)') j
              string = '(5x,"Template Entry  '//tmp4 // '"'
              write(*,string//pstring) gfld%ipdtmpl(j)
            enddo
!          print *,'G2 PRODUCT TEMPLATE 4.',gfld%ipdtnum,': ',   &
!               (gfld%ipdtmpl(j),j=1,gfld%ipdtlen)

           !call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)
           !call prvtime(gfld%ipdtnum,gfld%ipdtmpl,listsec1,tabbrev)
           write(*,'(5x,"Product Abbreviated Name",t50,":",a14)')&
           pabbrev
!           print *,'G2 TEXT: ',pabbrev,trim(labbrev)," ",trim(tabbrev)

           if ( gfld%num_coord .eq. 0 ) then
!            print *,'G2 NO Optional Vertical Coordinate List.'
           else
             print *,'G2 Section 4 Optional Coordinates: ',   &
                   (gfld%coord_list(j),j=1,gfld%num_coord)
           endif
!          if ( gfld%ibmap .ne. 255 ) then
!             print *,'G2 Num. of Data Points = ',gfld%ndpts,   &
!                  '    with BIT-MAP ',gfld%ibmap
!          else
!             print *,'G2 Num. of Data Points = ',gfld%ndpts,  &
!                     '    NO BIT-MAP '
!          endif
         write(*,'(/,"GRIB2 SECTION 5 - DATA REPRESENTATION SECTION:")')
         write(*,'(5x,"Data Representation Template Number"'//pstring)&
            gfld%idrtnum
            do j = 1, gfld%idrtlen
              write(tmp4,'(i4)') j
              string = '(5x,"Template Entry  '//tmp4 // '"'
              write(*,string//pstring) gfld%idrtmpl(j)
            enddo
!          print *,'G2 DRS TEMPLATE 5.',gfld%idrtnum,': ',  &
!               (gfld%idrtmpl(j),j=1,gfld%idrtlen)

!      if ( gfld%ipdtnum .eq. 0 ) then
!        if (gfld%ipdtmpl(1) .eq. 0 ) then
!          write(6,*) 'Temperature'
!        else if (gfld%ipdtmpl(1) .eq. 1 ) then
!          write(6,*) 'Moisture'
!        else if (gfld%ipdtmpl(1) .eq. 2 ) then
!          write(6,*) 'Momentum'
!        else if (gfld%ipdtmpl(1) .eq. 3 ) then
!         write(6,*) 'Mass'
!       endif
!      endif

         write(*,'(/,"GRIB2 SECTION 6 - BIT-MAP SECTION:")')
         write(*,'(5x,"Bit-map indicator"'//pstring) &
           gfld%ibmap

           fldmax=gfld%fld(1)
           fldmin=gfld%fld(1)
           sum=gfld%fld(1)
           do j=2,gfld%ndpts
             if (gfld%fld(j).gt.fldmax) fldmax=gfld%fld(j)
             if (gfld%fld(j).lt.fldmin) fldmin=gfld%fld(j)
             sum=sum+gfld%fld(j)
           enddo ! gfld%ndpts

         write(*,'(/,"GRIB2 SECTION 7 - DATA SECTION:")')

	 if ( fldmax .lt. -1.e10 ) then
           write(*,'(5x,"Minimum Data Value "'//estring)&
            fldmin
	 else
           write(*,'(5x,"Minimum Data Value "'//rstring)&
            fldmin
	 endif
	 if ( fldmax .gt. 1.e10 ) then
           write(*,'(5x,"Maximum Data Value "'//estring)&
            fldmax
	 else
           write(*,'(5x,"Maximum Data Value "'//rstring)&
            fldmax
	 endif
!          print *,'G2 Data Values:'
!          write(*,fmt='("G2 MIN=",f21.8," AVE=",f21.8,    &
!               " MAX=",f21.8)') fldmin,sum/gfld%ndpts,fldmax
         if (debug_level .gt. 100 ) then
            print*, 'gfld%fld = ',gfld%fld
!           do j=1,gfld%ndpts
!              write(*,*) j, gfld%fld(j)
!           enddo
         endif
         endif ! Additional Print information 
! ------------------------------------
         if ( debug_level .le. 50) then
           if(gfld%ipdtmpl(10).eq.100) then    ! pressure level
             level=gfld%ipdtmpl(12) *  &
                     (10. ** (-1. * gfld%ipdtmpl(11)))
           else if(gfld%ipdtmpl(10).eq.101 .or.&    ! sea level, sfc, or trop
                gfld%ipdtmpl(10).eq.1 .or. gfld%ipdtmpl(10).eq.7) then 
             level = 0
           else if(gfld%ipdtmpl(10).eq.106) then  ! below ground sfc is in cm in Vtable
             level= 100. * gfld%ipdtmpl(12)*(10.**(-1.*gfld%ipdtmpl(11)))
           else
             level=gfld%ipdtmpl(12) * 10.** (-1.*gfld%ipdtmpl(11))
           endif
           if (gfld%ipdtmpl(13) .eq. 255) then
             lvl2 = 0
           else if(gfld%ipdtmpl(10).eq.106) then   ! below ground sfc is in cm in Vtable
             lvl2 = 100. * gfld%ipdtmpl(15) * 10.** (-1.*gfld%ipdtmpl(14))
           else
             lvl2 = gfld%ipdtmpl(15) * 10.** (-1.*gfld%ipdtmpl(14))
           endif
!           Account for multiple forecast hours in one file
           if (gfld%ipdtnum.eq.0 .or. gfld%ipdtnum.eq.1 .or. gfld%ipdtnum.eq. 8) then
              ! Product Definition Template 4.0, 4.1, 4.8
              ! Extract forecast time.
             if ( gfld%ipdtmpl(8) .eq. 1 ) then   ! time units are hours
               fcst = gfld%ipdtmpl(9)
             else if ( gfld%ipdtmpl(8) .eq. 0 ) then  ! minutes
               fcst = gfld%ipdtmpl(9) / 60.
             else if ( gfld%ipdtmpl(8) .eq. 2 ) then  ! days
               fcst = gfld%ipdtmpl(9) * 24.
             else
               fcst = 999
             endif
           endif

           ! Non-standard Product Definition Templates need to be reported
           string = '                      '
           if ( gfld%ipdtnum .eq. 8 ) then
             string = '  PDT4.8'
           else if ( gfld%ipdtnum .eq. 1 ) then
             string = '  PDT4.1'
           endif
           write(6,987) itot,gfld%discipline,gfld%ipdtmpl(1),  &
             gfld%ipdtmpl(2),gfld%ipdtmpl(10),int(level),&
             lvl2,gfld%ipdtnum,pabbrev,hdate,fcst,string
  987     format(2i4,i5,i4,i8,i8,i8,i8,3x,a10,a20,i5.2,a10)

         endif

         ! Deallocate arrays decoding GRIB2 record.
         call gf_free(gfld)

         enddo NUM_FIELDS

      enddo VERSION ! skgb

       if (debug_level .gt. 50) &
          print *, 'G2 total number of fields found = ',itot

        CALL BACLOSE(junit,IOS)

        ireaderr=1
      else 
        print *,'open status failed because',ios
        hdate = '9999-99-99_99:99:99'
        ireaderr=2
      endif ! ireaderr check 

      END subroutine r_grib2

!*****************************************************************************!
! Subroutine edition_num                                                         !
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
      
      SUBROUTINE edition_num(junit, gribflnm, grib_edition, ireaderr)

      use grib_mod
      use params

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

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SET ARGUMENTS

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

!     write(6,*) 'junit = ',junit,' gribflnm = ',gribflnm

      ! Open a byte-addressable file.
      CALL BAOPENR(junit,gribflnm,IOS)      ! from w3lib
!     write(6,*) 'ios = ',ios
      if (ios.eq.0) then 

         ! Search opend file for the next GRIB2 messege (record).
         call skgb(junit,iseek,msk1,lskip,lgrib)

         ! Check for EOF, or problem
         if (lgrib.eq.0) then
            write(*,'("\n\tThere is a problem with the input file.")')
            write(*,'("\tPerhaps it is not a Grib2 file?\n")')
            STOP "Grib2 file or date problem, stopping in edition_num."
         endif
 
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

         print *, 'ungrib - grib edition num',  grib_edition
         CALL BACLOSE(junit,IOS)
         ireaderr=1
      else if (ios .eq. -4) then
        print *,'edition_num: unable to open ',gribflnm
        stop 'edition_num'
      else 
         print *,'edition_num: open status failed because',ios,gribflnm
         ireaderr=2
      endif ! ireaderr check 

      END subroutine edition_num
subroutine parse_args(err, a1, h1, i1, l1, a2, h2, i2, l2, a3, h3, i3, l3, &
     hlast)
  integer :: err
  character(len=*) , optional :: a1, a2, a3
  character(len=*), optional :: h1, h2, h3
  integer , optional :: i1, i2, i3
  logical, optional :: l1, l2, l3
  character(len=*), optional :: hlast

  character(len=100) :: hold
  integer :: ioff = 0

  if (present(hlast)) then
     ioff = -1
  endif

  err = 0

  narg = iargc()
  numarg = narg + ioff

  i = 1
  LOOP : do while ( i <= numarg)

     ierr = 1
     if (present(i1)) then
        call checkiarg(i, a1, i1, ierr)
     elseif (present(h1)) then
        call checkharg(i, a1, h1, ierr)
     elseif (present(l1)) then
        call checklarg(i, a1, l1, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     if (present(i2)) then
        call checkiarg(i, a2, i2, ierr)
     elseif (present(h2)) then
        call checkharg(i, a2, h2, ierr)
     elseif (present(l2)) then
        call checklarg(i, a2, l2, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     if (present(i3)) then
        call checkiarg(i, a3, i3, ierr)
     elseif (present(h3)) then
        call checkharg(i, a3, h3, ierr)
     elseif (present(l3)) then
        call checklarg(i, a3, l3, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     err = 1
     call getarg(1, hold)
     write(*, '("arg = ", A)') trim(hold)

     exit LOOP

  enddo LOOP

  if (present(hlast)) then
     if (narg.eq.0) then
        err = 1
     else
        call getarg(narg, hlast)
     endif
  endif

contains
  subroutine checkiarg(c, a, i, ierr)
    integer :: c
    character(len=*) :: a
    integer :: i

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)

    if ('-'//a.eq.trim(hold)) then
       c = c + 1
       call getarg(c, hold)
       read(hold, *) i
       c = c + 1
       ierr = 0
    elseif ('-'//a .eq. hold(1:len_trim(a)+1)) then
       hold = hold(len_trim(a)+2: len(hold))
       read(hold, *) i
       c = c + 1
       ierr = 0
    endif

  end subroutine checkiarg
  subroutine checkharg(c, a, h, ierr)
    integer :: c
    character(len=*) :: a
    character(len=*) :: h

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)

    if ('-'//a.eq.trim(hold)) then
       c = c + 1
       call getarg(c, hold)
       h = trim(hold)
       c = c + 1
       ierr = 0
    elseif ('-'//a .eq. hold(1:len_trim(a)+1)) then
       hold = hold(len_trim(a)+2: len(hold))
       h = trim(hold)
       c = c + 1
       ierr = 0
    endif

  end subroutine checkharg

  subroutine checklarg(c, a, l, ierr)
    integer :: c
    character(len=*) :: a
    logical :: l

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)
    if ('-'//a.eq.trim(hold)) then
       l = .TRUE.
       c = c + 1
       ierr = 0
    endif

  end subroutine checklarg

end subroutine parse_args

