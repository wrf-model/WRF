subroutine da_read_obs_bufrairs(obstype,iv,infile)
   !--------------------------------------------------------
   !  Purpose: read in NCEP bufr eos AIRS/AMSUA/HSB 1b data 
   !            to innovation structure
   !
   !   METHOD: use F90 sequantial data structure to avoid read file twice
   !            so that da_scan_bufrairs is not necessary any more.
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !                  and deallocate sequential data structure
   !
   !  HISTORY: 2006/01/03 - Creation            Zhiquan Liu
   !           2008/03/01 - VISNIR Cloud Cover  Tom Auligne
   !           2008/04/01 - Warmest FoV         Tom Auligne
   !           2008/07/15 - BUFR format change  Hui-Chuan Lin
   !            NCEP msg type NC021250 (center FOV data) discontinued after Aug 2007
   !            replaced by NC021249 (every FOV data)
   !
   !------------------------------------------------------------------------------




 implicit none

  character(9)      ,  intent (in)  :: obstype
  character(100)    ,  intent (in)  :: infile
  type (iv_type)    ,intent (inout) :: iv

#ifdef BUFR

! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: N_AIRSCHAN = 281  !- 281 subset ch out of 2378 ch for AIRS
  integer(i_kind),parameter :: N_AMSUCHAN =  15  
  integer(i_kind),parameter :: N_HSBCHAN  =   4
  integer(i_kind),parameter :: N_MAXCHAN  = 350
  integer(i_kind),parameter :: maxinfo    =  12

! BUFR format for AQUASPOT (SPITSEQN)
  integer(i_kind),parameter :: N_AQUASPOT_LIST = 25
  type aquaspot_list
     sequence
     real(r_double) :: said   ! Satellite identifier
     real(r_double) :: orbn   ! Orbit number
     real(r_double) :: slnm   ! Scan line number 
     real(r_double) :: mjfc   ! Major frame count
     real(r_double) :: selv   ! Height of station
     real(r_double) :: soza   ! Solar zenith angle
     real(r_double) :: solazi ! Solar azimuth angle
     real(r_double) :: intms(2,9) ! SATELLITE inSTRUMENT TEMPERATURES
  end type aquaspot_list
  real(r_double), dimension(1:N_AQUASPOT_LIST) :: aquaspot_list_array


! BUFR format for AIRSSPOT (SITPSEQN)
  integer(i_kind),parameter :: N_AIRSSPOT_LIST = 12
  type airsspot_list
     sequence
     real(r_double) :: siid  ! Satellite instruments
     real(r_double) :: year
     real(r_double) :: mnth
     real(r_double) :: days
     real(r_double) :: hour
     real(r_double) :: minu
     real(r_double) :: seco
     real(r_double) :: clath ! Latitude (high accuracy)
     real(r_double) :: clonh ! Longitude (high accuracy)
     real(r_double) :: saza  ! Satellite zenith angle 
     real(r_double) :: bearaz ! Bearing or azimuth
     real(r_double) :: fovn  ! Field of view number
  end type airsspot_list
  real(r_double), dimension(1:N_AIRSSPOT_LIST) :: airsspot_list_array


! BUFR format for AIRSCHAN (SCBTSEQN)
  integer(i_kind),parameter :: N_AIRSCHAN_LIST = 4
  type airschan_list
     sequence
     real(r_double) :: chnm    ! Channel number
     real(r_double) :: logrcw  ! Log-10 of (Temperature-radiance central wavenumber
     real(r_double) :: acqf    ! Channel quality flags for ATOVS
     real(r_double) :: tmbrst  ! Brightness temperature
  end type airschan_list
  real(r_double), dimension(1:N_AIRSCHAN_LIST,1:N_MAXCHAN) :: airschan_list_array
  
! BUFR talble file sequencial number
  character(len=512)  :: table_file


! Variables for BUFR IO    
  type(aquaspot_list) :: aquaspot
  type(airsspot_list) :: airsspot
  type(airschan_list) :: airschan(N_MAXCHAN)
  
  real(r_kind)      :: step, start
  real(r_kind)      :: airdata(N_AIRSCHAN+maxinfo)
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=8)  :: spotname
  character(len=8)  :: channame
  integer(i_kind)   :: nchan,nchanr
  integer(i_kind)   :: iret


! Work variables for time
  integer(i_kind)   :: idate, ifgat
  integer(i_kind)   :: idate5(6)
  character(len=10) :: date
  integer(i_kind)   :: nmind
  integer(i_kind)   :: iy, im, idd, ihh
  real*8            :: obs_time


! Other work variables
  integer(i_kind)  :: nreal
  integer(i_kind)  :: k, iobsout
  integer(i_kind),dimension(19)::icount
  real(r_kind)     :: rlat, rlon, dx, dy, dx1, dy1, sstx, dlon, dlat
  real(r_kind)     :: sza, timedif, pred, crit1
  integer(i_kind)  :: klat1, klon1, klatp1, klonp1
  integer(i_kind)  :: ifov,size
  integer(i_kind)  :: num_eos_file, num_eos_global, num_eos_local, num_eos_kept, num_eos_thinned
  integer(i_kind)  :: inst,platform_id,satellite_id,sensor_id
  logical          :: iflag,outside,outside_all
  integer(i_kind)  :: i, l, n, error, airs_table_unit
  integer          :: iost, lnbufr
  real(r_kind),allocatable,dimension(:,:):: airdata_all
  real(kind=8)     :: tocc(1,1)
  integer          ::num_bufr(7),numbufr,ibufr
  character(20)    ::filename
  integer(i_kind), allocatable :: ptotal(:)

! Set standard parameters
  real(r_kind)     :: POinT001 =   0.001_r_kind
  real(r_kind)     :: POinT01  =   0.01_r_kind
  real(r_kind)     :: TEN      =  10.0_r_kind
  real(r_kind)     :: R45      =  45.0_r_kind
  real(r_kind)     :: R60      =  60.0_r_kind
  real(r_kind)     :: R90      =  90.0_r_kind
  real(r_kind)     :: R180     = 180.0_r_kind
  real(r_kind)     :: R360     = 360.0_r_kind

! Thinning variables
  integer(i_kind) itt,itx,iobs,iout,size_tmp, j
  real(r_kind) crit,dist
  real(r_kind) dlon_earth,dlat_earth
  logical luse
  real , allocatable :: in(:), out(:)
  logical :: found, head_found

  logical           :: airs, eos_amsua, hsb, airstab
  type(info_type)            :: info
  type(model_loc_type)       :: loc
  type (datalink_type), pointer   :: head, p, current, prev

  if (trace_use) call da_trace_entry("da_read_obs_bufrairs")

!  0.0  Initialize variables
!-----------------------------------
  platform_id  = 9   ! eos series
  satellite_id = 2   ! eos-2
  nreal  = maxinfo
  allocate(ptotal(0:num_fgat_time))
  ptotal(0:num_fgat_time) = 0
  airs=      obstype == 'airs     '
  eos_amsua= obstype == 'eos_amsua'
  hsb=       obstype == 'hsb      '

  icount=0
  if(airs)then
     sensor_id = 11
     step   = 1.1_r_kind
     start = -48.9_r_kind
     senname = 'AIRS'
     nchan  = N_AIRSCHAN
     nchanr = N_AIRSCHAN
  else if(eos_amsua)then
     sensor_id = 3
     step   = three + one/three
     start  = -48.33_r_kind
     senname = 'AMSU'
     nchan  = N_AMSUCHAN
     nchanr = N_AMSUCHAN
  else if(hsb)then
     sensor_id = 12
     step   = 1.1_r_kind
     start  = -48.95_r_kind
     senname = 'HSB'
     nchan  = N_HSBCHAN
     nchanr = N_HSBCHAN+1
  end if
  spotname = trim(senname)//'SPOT'
  channame = trim(senname)//'CHAN'
 
      do inst = 1, rtminit_nsensor
        if (    platform_id  == rtminit_platform(inst) &
          .and. satellite_id == rtminit_satid(inst)    &
          .and. sensor_id    == rtminit_sensor(inst)    ) then
            exit
        end if
      end do

      if ( inst == rtminit_nsensor .and.           &
           platform_id  /= rtminit_platform(inst)  &
          .or. satellite_id /= rtminit_satid(inst) &
          .or. sensor_id /= rtminit_sensor(inst)  ) then
         if (trace_use) call da_trace_exit("da_read_obs_bufrairs")
         return
      end if

  num_eos_file    = 0  ! number of obs in file
  num_eos_global  = 0  ! number of obs within whole domain
  num_eos_local   = 0  ! number of obs within tile
  num_eos_kept    = 0  ! number of obs kept for innovation computation
  num_eos_thinned = 0  ! number of obs rejected by thinning
  size = 0     !
  iobs = 0     ! for thinning, argument is inout

!    1.0  Open BUFR table and BUFR file
!--------------------------------------------------------------
  table_file = 'gmao_airs_bufr.tbl'      ! make table file name
  inquire(file=table_file,exist=airstab)
  if (airstab) then
      if (print_detail_rad) then
         write(unit=message(1),fmt=*) &
            'Reading BUFR Table A file: ',trim(table_file)
         call da_message(message(1:1))
      end if
      call da_get_unit(airs_table_unit)
      open(unit=airs_table_unit,file=table_file,iostat = iost)
      if (iost /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open file "//table_file/))
      end if
  end if

! Open BUFR file
!  call da_get_unit(lnbufr)

   num_bufr(:)=0
   numbufr=0
   if (num_fgat_time>1) then
      do i=1,7
         call da_get_unit(lnbufr)
         write(filename,fmt='(A,2I1,A)') trim(infile),0,i,'.bufr'
         open(unit   = lnbufr, FILE   = trim(filename),iostat =  iost, form = 'unformatted', STATUS = 'OLD')
         if (iost == 0) then
            numbufr=numbufr+1
	    num_bufr(numbufr)=i
         else
            close (lnbufr)
         end if
         call da_free_unit(lnbufr)
      end do
   else
     numbufr=1
   end if

   if (numbufr==0) numbufr=1

bufrfile:  do ibufr=1,numbufr   
   if (num_fgat_time==1) then
      filename=trim(infile)//'.bufr'
   else
      if ((numbufr ==1) .and. (num_bufr(ibufr) == 0)) then
         filename=trim(infile)//'.bufr'
      else
         write(filename,fmt='(A,2I1,A)') trim(infile),0,num_bufr(ibufr),'.bufr'   
      end if
   end if

  lnbufr=97

  open(unit=lnbufr,file=trim(filename),form='unformatted',iostat = iost, status = 'old')
  if (iost /= 0) then
     call da_warning(__FILE__,__LINE__, &
        (/"Cannot open file "//infile/))
     close(lnbufr)
     if (airstab) then
        close(airs_table_unit)
        call da_free_unit(airs_table_unit)
     end if
     if (trace_use) call da_trace_exit("da_read_obs_bufrairs")
     return
  end if
  if ( airstab ) then
     call openbf(lnbufr,'IN',airs_table_unit)
  else
     call openbf(lnbufr,'IN',lnbufr)
  end if
  call datelen(10)


!   2.0  Read header
!---------------------------
  call readmg(lnbufr,subset,idate,iret)

  iy = 0
  im = 0
  idd = 0
  ihh = 0
  if( iret /= 0 ) goto 1000     ! no data?

  write(unit=date,fmt='( i10)') idate
  read(unit=date,fmt='(i4,3i2)') iy,im,idd,ihh
  write(unit=stdout,fmt='(a,4i4,2x,a)') &
      'Bufr file date is ',iy,im,idd,ihh,trim(infile)

!   3.0 Loop over observations
!----------------------------
     if ( ibufr == 1 ) then
        allocate ( head )
      ! allocate ( head % tb (1:nchan) )
        nullify  ( head % next )
        p => head
     endif  

  loop_obspoints: do

!   3.1 Read headder
!-------------------------------
     call readsb(lnbufr,iret)

     if( iret /=0 )then
        call readmg(lnbufr,subset,idate,iret)
        if( iret /= 0 ) exit loop_obspoints     ! end of file
        cycle loop_obspoints
     end if

     num_eos_file = num_eos_file + 1

!   3.2 Read AQUASPOT (SPITSEQN)
!------------------------
     call ufbseq(lnbufr,aquaspot_list_array,N_AQUASPOT_LIST,1,iret,'SPITSEQN')
     aquaspot = aquaspot_list( aquaspot_list_array(1), &
                               aquaspot_list_array(2), &
                               aquaspot_list_array(3), &
                               aquaspot_list_array(4), &
                               aquaspot_list_array(5), &
                               aquaspot_list_array(6), &
                               aquaspot_list_array(7), &
                               RESHAPE(aquaspot_list_array(8:25), (/2,9/)) )

!   3.3 Read AIRSSPOT or AMSUSPOT or HSBSPOT
!-------------------------------------------------
     if ( trim(senname) == 'AIRS' ) then
        call ufbseq(lnbufr,airsspot_list_array,N_AIRSSPOT_LIST,1,iret,'SITPSEQN')
     else
        call ufbseq(lnbufr,airsspot_list_array,N_AIRSSPOT_LIST,1,iret,spotname)
     end if
     airsspot = airsspot_list( airsspot_list_array(1), &
                               airsspot_list_array(2), &
                               airsspot_list_array(3), &
                               airsspot_list_array(4), &
                               airsspot_list_array(5), &
                               airsspot_list_array(6), &
                               airsspot_list_array(7), &
                               airsspot_list_array(8), &
                               airsspot_list_array(9), &
                               airsspot_list_array(10), &
                               airsspot_list_array(11), &
                               airsspot_list_array(12) )

!   3.4 Read AIRSCHAN or AMSUCHAN or HSBCHAN
!-------------------------------------------
     if ( trim(senname) == 'AIRS' ) then
        call ufbseq(lnbufr,airschan_list_array,N_AIRSCHAN_LIST,N_MAXCHAN,iret,'SCBTSEQN')
     else
        call ufbseq(lnbufr,airschan_list_array,N_AIRSCHAN_LIST,N_MAXCHAN,iret,channame)
     end if
     do l = 1 , N_MAXCHAN
        airschan(l) = airschan_list( airschan_list_array(1,l), &
                                     airschan_list_array(2,l), & 
                                     airschan_list_array(3,l), & 
                                     airschan_list_array(4,l)  )
     end do

     if (iret /= nchanr) then
        write(unit=message(1),fmt=*) &
            'Cannot read ', channame, &
            ' bufr data:', &
            iret, ' ch data is read instead of', nchanr
        call da_warning(__FILE__,__LINE__,message(1:1))
        cycle loop_obspoints
     end if

!   3.5 Read Cloud Cover from AIRS/VISNIR
!-------------------------------------------
     call ufbint(lnbufr,tocc,1,1,iret,'TOCC')
     
!   4.0  Check observing position (lat/lon)
!      QC1:  juge if data is in the domain, 
!            read next record if not
!------------------------------------------
     if( abs(airsspot%clath) > R90  .or. &
          abs(airsspot%clonh) > R360 .or. &
          (abs(airsspot%clath) == R90 .and. airsspot%clonh /= ZERO) )then
        cycle loop_obspoints
     end if

!    Retrieve observing position
     if(airsspot%clonh >= R360) then
        airsspot%clonh = airsspot%clonh - R360
!     else if(airsspot%clonh < ZERO) then
!        airsspot%clonh = airsspot%clonh + R360
     end if

        info%lat  = airsspot%clath
        info%lon  = airsspot%clonh 
        call da_llxy (info, loc, outside, outside_all )

        if ( outside_all ) cycle loop_obspoints
	
!  4.1  Check obs time
!-------------------------------------
     idate5(1) = airsspot%year ! year
     idate5(2) = airsspot%mnth ! month
     idate5(3) = airsspot%days ! day
     idate5(4) = airsspot%hour ! hour
     idate5(5) = airsspot%minu ! minute
     idate5(6) = airsspot%seco ! second

     if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
          idate5(2) <    1 .or. idate5(2) >   12 .or. &
          idate5(3) <    1 .or. idate5(3) >   31 .or. &
          idate5(4) <    0 .or. idate5(4) >   24 .or. &
          idate5(5) <    0 .or. idate5(5) >   60 .or. &
          idate5(6) <    0 .or. idate5(6) >   60 ) then
        cycle loop_obspoints
     end if

!  QC3: time consistency check with the background date

      if (idate5(1) .LE. 99) then
        if (idate5(1) .LT. 78) then
          idate5(1) = idate5(1) + 2000
        else
          idate5(1) = idate5(1) + 1900
        end if
      end if

      call da_get_julian_time(idate5(1),idate5(2),idate5(3),idate5(4),idate5(5),obs_time)

      if ( obs_time < time_slots(0) .or.  &
           obs_time >= time_slots(num_fgat_time) ) cycle

!  3.2.1   determine FGAT index ifgat
!
       do ifgat=1,num_fgat_time
           if ( obs_time >= time_slots(ifgat-1) .and.  &
                obs_time  < time_slots(ifgat) ) exit
       end do

       num_eos_global = num_eos_global + 1
       ptotal(ifgat) = ptotal(ifgat) + 1

       if (outside) cycle loop_obspoints

       num_eos_local = num_eos_local + 1

       write(unit=info%date_char, &
         fmt='(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
         idate5(1), '-', idate5(2), '-', idate5(3), '_', idate5(4), &
         ':', idate5(5), ':', idate5(6)

       info%elv = 0.0  !aquaspot%selv

!  4.2  Check observational info
!-------------------------------------------------------
     if( airsspot%fovn <    0.0_r_kind .or. airsspot%fovn > 100.0_r_kind .or. &
          airsspot%saza < -360.0_r_kind .or. airsspot%saza > 360.0_r_kind .or. &
          aquaspot%soza < -180.0_r_kind .or. aquaspot%soza > 180.0_r_kind )then
         write(unit=message(1),fmt=*) &
            'Cannot read ', channame, ' bufr data:', &
            ' strange obs info(fov,saza,soza):', &
            airsspot%fovn, airsspot%saza, aquaspot%soza
        call da_warning(__FILE__,__LINE__,message(1:1))
        cycle loop_obspoints
     end if

!    Retrieve observing info
     ifov = int( airsspot%fovn + POinT001 )
     sza  = abs(airsspot%saza)
!     if( ((airs .or. hsb) .and. ifov <= 45) .or. &
!          ( eos_amsua     .and. ifov <= 15) )then
!        sza = - sza
!     end if

!     airdata(6) = (start + float(ifov-1)*step)  ! look angle (deg)
!     airdata(9) = ZERO                          ! surface height
!     airdata(10)= POinT001                      ! land sea mask

!  4.1 Make Thinning
!  Map obs to thinning grid
!-------------------------------------------------------------------
       if (thinning) then
          dlat_earth = info%lat
          dlon_earth = info%lon
          if (dlon_earth<zero) dlon_earth = dlon_earth+r360
          if (dlon_earth>=r360) dlon_earth = dlon_earth-r360
          dlat_earth = dlat_earth*deg2rad
          dlon_earth = dlon_earth*deg2rad
          crit = 1.0 ! 0.01_r_kind+terrain + timedif !+ 10.0_r_kind*float(iskip)
          if (airs_warmest_fov) &
             crit = 1E10 * exp(-(airschan(129)%tmbrst-220.0)/2)   ! warmest bt for window channel (10.36 micron)
          call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,luse)
          if (.not. luse) then
             num_eos_thinned = num_eos_thinned + 1
             cycle loop_obspoints
          end if
       end if


!   4.3 Retrieve Tb
!-----------------------
     iflag = .false.
  
     do l=1,nchan
        airdata(l+nreal) = airschan(l)%tmbrst            ! brightness temperature
        if( airdata(l+nreal) > 0.0_r_kind .and. airdata(l+nreal) < 500.0_r_kind )then
           iflag = .true.
        else
           airdata(l+nreal) = missing_r
        end if
     end do

     if ( .not. iflag )then
        write(unit=message(1),fmt=*) &
          'Error in reading ', channame, ' bufr data:', &
          ' all tb data is missing'
        call da_warning(__FILE__,__LINE__,message(1:1))
        num_eos_local = num_eos_local - 1
        cycle loop_obspoints
     end if

     num_eos_kept = num_eos_kept + 1

!  4.0   assign information to sequential radiance structure
!--------------------------------------------------------------------------
   allocate ( p % tb_inv (1:nchan) )
   p%info             = info
   p%loc              = loc
   p%landsea_mask     = POinT001
   p%scanline         = int(aquaspot%slnm + POinT001)
   p%scanpos          = ifov
   p%satzen           = sza
   p%satazi           = (start + float(ifov-1)*step)  ! look angle (deg) ! airsspot%bearaz
   p%solzen           = aquaspot%soza
   p%solazi           = aquaspot%solazi
   p%tb_inv(1:nchan)  = airdata(nreal+1:nreal+nchan)
   p%sensor_index     = inst
   p%ifgat            = ifgat

   size = size + 1
   allocate ( p%next, stat=error)   ! add next data
   if (error /= 0 ) then
      call da_error(__FILE__,__LINE__, &
          (/"Cannot allocate radiance structure"/))
   end if

   p => p%next
   nullify (p%next)

  end do loop_obspoints

  call closbf(lnbufr)
  close(lnbufr)

end do bufrfile

   if (thinning .and. num_eos_global > 0) then

#ifdef DM_PARALLEL
         
      ! Get minimum crit and associated processor index.
      j = 0
      do ifgat = 1, num_fgat_time
         do n = 1, iv%num_inst
            j = j + thinning_grid(n,ifgat)%itxmax
         end do
      end do
         
      allocate ( in  (j) )
      allocate ( out (j) ) 
               
      j = 0
      do ifgat = 1, num_fgat_time
         do n = 1, iv%num_inst
            do i = 1, thinning_grid(n,ifgat)%itxmax
               j = j + 1
               in(j) = thinning_grid(n,ifgat)%score_crit(i)
            end do
         end do             
      end do
      call mpi_reduce(in, out, j, true_mpi_real, mpi_min, root, comm, ierr)

      call wrf_dm_bcast_real (out, j)

      j = 0
      do ifgat = 1, num_fgat_time
         do n = 1, iv%num_inst
            do i = 1, thinning_grid(n,ifgat)%itxmax
               j = j + 1
               if ( ABS(out(j)-thinning_grid(n,ifgat)%score_crit(i)) > 1.0E-10 ) thinning_grid(n,ifgat)%ibest_obs(i)  = 0
            end do
         end do
      end do

      deallocate( in  )
      deallocate( out )

#endif

      ! Delete the nodes which being thinning out
      if ( size > 0 ) then
         p => head
         prev => head
         head_found = .false.
         size_tmp = size
         do j = 1, size_tmp
            n = p%sensor_index
            ifgat = p%ifgat
            found = .false.

            do i = 1, thinning_grid(n,ifgat)%itxmax
               if ( thinning_grid(n,ifgat)%ibest_obs(i) == j .and. thinning_grid(n,ifgat)%score_crit(i) < 9.99e6_r_kind ) then
                  found = .true.
                  exit
               endif
            end do

            ! free current data
            if ( .not. found ) then
               current => p
               p => p%next
               if ( head_found ) then
                  prev%next => p
               else
                  head => p
                  prev => p
               endif
               deallocate ( current % tb_inv )
               deallocate ( current )
               num_eos_thinned = num_eos_thinned + 1
               num_eos_kept = num_eos_kept - 1
               size = size - 1
               continue
            endif

            if ( found .and. head_found ) then
               prev => p
               p => p%next
               continue
            endif

            if ( found .and. .not. head_found ) then
               head_found = .true.
               head => p
               prev => p
               p => p%next
            endif
         end do
      endif

   endif  ! End of thinning

   iv%total_rad_pixel   = iv%total_rad_pixel + size
   iv%total_rad_channel = iv%total_rad_channel + size*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + size
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_eos_global
   iv%instid(inst)%info%nlocal = size
   
   do ifgat = 1, num_fgat_time
      ptotal(ifgat) = ptotal(ifgat) + ptotal(ifgat-1)
      iv%info(radiance)%ptotal(ifgat) = iv%info(radiance)%ptotal(ifgat) + ptotal(ifgat)
   end do
   
   write(unit=stdout,fmt='(a)') '   num_eos_file num_eos_global  num_eos_local   num_eos_kept num_eos_thinned'
   write(unit=stdout,fmt='(5(5x,i10))') num_eos_file,num_eos_global, num_eos_local,num_eos_kept,num_eos_thinned

!  5.0 allocate innovation radiance structure
!----------------------------------------------------------------
!  do i = 1, iv%num_inst
   if ( size > 0 ) then
      iv%instid(inst)%num_rad = size
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
   end if
!  end do

!   6.0 assign sequential structure to innovation structure
!-------------------------------------------------------------

  n = 0
  p => head

  call da_allocate_rad_iv (inst, nchan, iv)

  do i = 1, size
!   inst = p%sensor_index
   n = n + 1

   call da_initialize_rad_iv (inst, n, iv, p)
   iv%instid(inst)%rain_flag(n) = tocc(1,1) ! Temporary dumping of AIRS/VISNIR cloud cover
   
   current => p
   p => p%next

! free current data
   deallocate ( current % tb_inv )
   deallocate ( current )

 end do

 deallocate (p)
 deallocate (ptotal)

1000 continue
   call closbf(lnbufr)
   close(lnbufr)
!   call da_free_unit(lnbufr)
   if (airstab) then
      close(airs_table_unit)
      call da_free_unit(airs_table_unit)
   end if

   if (trace_use) call da_trace_exit("da_read_obs_bufrairs")
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with a BUFR library"/))
#endif

end subroutine da_read_obs_bufrairs
