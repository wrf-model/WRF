subroutine da_read_obs_bufriasi (obstype,iv,infile)	 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_iasi                  read bufr format iasi data

  implicit none


  character(9)      ,  intent (in)  :: obstype
  character(100)    ,  intent (in)  :: infile
  type (iv_type)    ,intent (inout) :: iv
#ifdef BUFR  
  real(kind=8)    ::  obs_time  
  type (datalink_type), pointer  :: head, p, current, prev
  type(info_type)                :: info
  type(model_loc_type)           :: loc 
  type(model_loc_type)           :: loc_fov  

! Subroutine constants
  real(r_kind)                   :: ten      =  10.0_r_kind
  real(r_kind)                   :: r90      =  90.0_r_kind
  real(r_kind)                   :: r360     = 360.0_r_kind

! Number of channels for sensors in BUFR
  integer(i_kind),parameter      :: nchan = 616        !--- 616 subset ch out of 8078 ch for AIRS
  integer(i_kind),parameter      :: n_totchan  = 616
  integer(i_kind),parameter      :: maxinfo    =  33
  integer(i_kind)                :: inst,platform_id,satellite_id,sensor_id
  integer(i_kind), allocatable   :: ptotal(:), nread(:)
  real(r_kind)                   :: crit
  integer(i_kind)                :: ifgat, iout, iobs
  logical                        :: outside, outside_all, iuse

! BUFR functions
  integer(i_kind)   :: ireadsb,ireadmg

! Variables for BUFR IO    
  real(r_double),dimension(5)           :: linele
  real(r_double),dimension(14)          :: allspot
  real(r_double),dimension(2,n_totchan) :: allchan
  real(r_double),dimension(3,10)        :: cscale
  real(r_double),dimension(6)           :: cloud_frac
  integer           :: numbufr,ibufr  
  logical           :: found, head_found 
  real(r_kind)      :: step, start,step_adjust
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  integer(i_kind)   :: jstart
  integer(i_kind)   :: iret
  character(10)     :: date

! Work variables for time
  integer(i_kind)   :: idate, im, iy, idd, ihh
  integer(i_kind)   :: idate5(6)

! Other work variables
  real(r_kind)                          :: piece
  real(r_kind)                          :: dlon_earth,dlat_earth
  real(r_kind)                          :: lza, lzaest,sat_height_ratio
  real(r_kind)                          :: sat_zenang
  real(r_kind)                          :: radi
  real(r_kind),dimension(10)            :: sscale
  real(r_kind),dimension(n_totchan)     :: temperature
  real(r_kind),allocatable,dimension(:) :: data_all
  real(r_kind)                          :: Effective_Temperature

  logical          :: iasi
  integer(i_kind)  :: nreal, ksatid
  integer(i_kind)  :: ifov, iscn
  integer(i_kind)  :: num_iasi_file
  integer(i_kind)  :: num_iasi_local, num_iasi_global, num_iasi_used, num_iasi_thinned 
  integer(i_kind)  :: num_iasi_used_tmp  
  integer(i_kind)  :: i, j, l, iskip, ifovn, bad_line
  integer(i_kind)  :: itx, k, nele, itt, n
  integer(i_kind)  :: iexponent
  integer(i_kind)  :: num_bufr(7)
  integer          :: iost, lnbufr 
  character(20)    :: filename  
  real,allocatable :: in(:), out(:)

! Set standard parameters
  integer(i_kind),parameter :: ichan=-999  ! fov-based surface code is not channel specific for iasi 
  real(r_kind),parameter    :: expansion=one         ! exansion factor for fov-based location code.                                                 ! use one for ir sensors.
  real(r_kind),parameter    :: tbmin  = 50._r_kind
  real(r_kind),parameter    :: tbmax  = 550._r_kind
  real(r_kind),parameter    :: earth_radius = 6371000._r_kind
  

   if (trace_use) call da_trace_entry("da_read_obs_bufriasi")
!  0.0  Initialize variables
!-----------------------------------
  platform_id  = 10   ! metop series
  sensor_id = 16 !iasi
  nreal  = maxinfo
  iasi=      obstype == 'iasi'
  bad_line=-1
  step  = 3.334
  start = -48.33
  step_adjust = 0.625_r_kind
  senname = 'IASI'
  allspotlist= &
   'SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ SOZA SOLAZI SAID'
  num_bufr(:)=0
  numbufr=0
  allocate(nread(1:rtminit_nsensor))
  allocate(ptotal(0:num_fgat_time))
  nread(1:rtminit_nsensor) = 0
  ptotal(0:num_fgat_time) = 0
  iobs = 0                 ! for thinning, argument is inout  
  num_iasi_file    = 0
  num_iasi_local   = 0
  num_iasi_global  = 0
  num_iasi_used    = 0
  num_iasi_thinned = 0  
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
   lnbufr = 95
   open(unit=lnbufr,file=trim(filename),form='unformatted', &
      iostat = iost, status = 'old')
   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open file "//infile/))
      if (trace_use) call da_trace_exit("da_read_obs_bufriasi")
      return
   end if

! Open BUFR table
   call openbf(lnbufr,'IN',lnbufr) 
   call datelen(10)
   call readmg(lnbufr,subset,idate,iret)
   iy=0
   im=0
   idd=0
   ihh=0
   write(unit=date,fmt='( i10)') idate
   read(unit=date,fmt='(i4,3i2)') iy,im,idd,ihh
   write(unit=stdout,fmt='(a,4i4,2x,a)') &
      'Bufr file date is ',iy,im,idd,ihh,trim(infile)

   ! Loop to read bufr file and assign information to a sequential structure
   !-------------------------------------------------------------------------
! Allocate arrays to hold data
  nele=nreal+nchan
  allocate(data_all(nele))
   if ( ibufr == 1 ) then
      allocate (head)
      nullify  ( head % next )
      p => head
   end if
  

! Big loop to read data file

  do while(ireadmg(lnbufr,subset,idate)>=0)  

     read_loop: do while (ireadsb(lnbufr)==0)
         num_iasi_file = num_iasi_file + 1	 

!    Read IASI FOV information
         call ufbint(lnbufr,linele,5,1,iret,'FOVN SLNM QGFQ MJFC SELV')				
         if ( linele(3) /= zero) then
	         cycle read_loop  
	     end if
         if ( bad_line == nint(linele(2))) then
!        zenith angle/scan spot mismatch, reject entire line
           cycle read_loop
         else
           bad_line = -1
         end if
          call ufbint(lnbufr,allspot,14,1,iret,allspotlist)
         if(iret /= 1) cycle read_loop

         ksatid = nint(allspot(14))
         ! SAID 3 is metop-b (metop-1)
         ! SAID 4 is metop-a (metop-2)
         ! SAID 5 is metop-c (metop-3)
         if ( ( ksatid > 5) .or. ( ksatid < 3) ) then 
            write(unit=message(1),fmt='(A,I6)') 'Unknown platform: ', ksatid
            call da_warning(__FILE__,__LINE__,message(1:1))
         end if
         if ( ksatid == 3 ) then
             satellite_id = 1
         else if ( ksatid == 4 ) then
             satellite_id = 2
         else if ( ksatid == 5 ) then
             satellite_id = 3
         end if

!    Check observing position
         info%lat  =  allspot(8)  ! latitude
         info%lon  =  allspot(9)  ! longitude)
         if( abs(info%lat) > r90  .or. abs(info%lon) > r360 .or. &
         (abs(info%lat) == r90 .and. info%lon /= zero) )then
         write(unit=stdout,fmt=*) &
         'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
               ' STRANGE OBS POINT (LAT,LON):', info%lat, info%lon
            cycle read_loop
         end if		 
	 		 
         call da_llxy (info, loc, outside, outside_all)	
         if (outside_all) cycle
	     inst = 0	 
         do i = 1, rtminit_nsensor
            if (platform_id  == rtminit_platform(i) &
               .and. satellite_id == rtminit_satid(i)    &
               .and. sensor_id    == rtminit_sensor(i)) then
               inst = i
               exit
            end if
         end do	 
         if (inst == 0) cycle read_loop		 
		 
!    Check obs time
         idate5(1) = nint(allspot(2)) ! year
         idate5(2) = nint(allspot(3)) ! month
         idate5(3) = nint(allspot(4)) ! day
         idate5(4) = nint(allspot(5)) ! hour
         idate5(5) = nint(allspot(6)) ! minute
         idate5(6) = nint(allspot(7)) ! second		
		
         if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
             idate5(2) < 1    .or. idate5(2) >   12 .or. &
             idate5(3) < 1    .or. idate5(3) >   31 .or. &
             idate5(4) <0     .or. idate5(4) >   24 .or. &
             idate5(5) <0     .or. idate5(5) >   60 )then

            write(6,*)'READ_IASI:  ### ERROR IN READING ', 'IASI', ' BUFR DATA:', &
                ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
             cycle read_loop
         end if
         call da_get_julian_time(idate5(1),idate5(2),idate5(3),idate5(4),idate5(5),obs_time)		
         if ( obs_time < time_slots(0) .or.  &
           obs_time >= time_slots(num_fgat_time) ) cycle read_loop
         do ifgat=1,num_fgat_time
            if ( obs_time >= time_slots(ifgat-1) .and.  &
                obs_time  < time_slots(ifgat) ) exit
         end do	
         num_iasi_global = num_iasi_global + 1
         ptotal(ifgat) = ptotal(ifgat) + 1  

!    Observational info
         sat_zenang  = allspot(10)            ! satellite zenith angle
         ifov = nint(linele(1))               ! field of view

!    IASI fov ranges from 1 to 120.   Current angle dependent bias
!    correction has a maximum of 90 scan positions.   Geometry
!    of IASI scan allows us to remap 1-120 to 1-60.   Variable
!    ifovn below contains the remapped IASI fov.  This value is
!    passed on to and used in setuprad
         ifovn = (ifov-1)/2 + 1
         iscn = nint(linele(2))               ! scan line

!    Check field of view (FOVN) and satellite zenith angle (SAZA)
         if( ifov <= 0 .or. ifov > 120 .or. sat_zenang > 90._r_kind ) then
         write(unit=stdout,fmt=*) &
         'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                ' STRANGE OBS INFO(FOVN,SLNM,SAZA):', ifov, iscn, allspot(10)				
           cycle read_loop
         end if
         if ( ifov <= 60 ) sat_zenang = -sat_zenang

!    Compare IASI satellite scan angle and zenith angle
         piece = -step_adjust
         if ( mod(ifovn,2) == 1) piece = step_adjust
         lza = ((start + float((ifov-1)/4)*step) + piece)*deg2rad
         sat_height_ratio = (earth_radius + linele(5))/earth_radius
         lzaest = asin(sat_height_ratio*sin(lza))*rad2deg
         if (abs(sat_zenang - lzaest) > one) then
		 bad_line = iscn
          write(unit=stdout,fmt=*) 'abs(sat_zenang - lzaest) > one',abs(sat_zenang - lzaest)		   		   	   
           cycle read_loop
         end if
!   Clear Amount  (percent clear)
         call ufbrep(lnbufr,cloud_frac,1,6,iret,'FCPH')
         if (outside) cycle ! No good for this PE		
         num_iasi_local = num_iasi_local + 1
		 
         write(unit=info%date_char, &
         fmt='(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
         idate5(1), '-', idate5(2), '-', idate5(3), '_', idate5(4), &
         ':', idate5(5), ':', idate5(6)
         info%elv = 0.0  !aquaspot%selv		   
		 
         !  Make Thinning
         !  Map obs to thinning grid
         !-------------------------------------------------------------------
         if (thinning) then
            dlat_earth = info%lat
            dlon_earth = info%lon
            if (dlon_earth<zero) dlon_earth = dlon_earth+r360
            if (dlon_earth>=r360) dlon_earth = dlon_earth-r360
            dlat_earth = dlat_earth*deg2rad
            dlon_earth = dlon_earth*deg2rad
            crit = 1. 
            call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
            if (.not. iuse) then
               num_iasi_thinned=num_iasi_thinned+1
               cycle
            end if
         end if		
         call ufbrep(lnbufr,cscale,3,10,iret,'STCH ENCH CHSF')
         if(iret /= 10) then
           write(unit=stdout,fmt=*)  'READ_IASI  read scale error ',iret		   
           cycle read_loop
         end if

! The scaling factors are as follows, cscale(1) is the start channel number,
!                                     cscale(2) is the end channel number,
!                                     cscale(3) is the exponent scaling factor
! In our case (616 channels) there are 10 groups of cscale (dimension :: cscale(3,10))
!  The units are W/m2..... you need to convert to mW/m2.... (subtract 5 from cscale(3)
         do i=1,10  ! convert exponent scale factor to int and change units
             iexponent = -(nint(cscale(3,i)) - 5)
             sscale(i)=ten**iexponent
         end do

!    Read IASI channel number(CHNM) and radiance (SCRA)

         call ufbint(lnbufr,allchan,2,n_totchan,iret,'SCRA CHNM')
         if( iret /= n_totchan)then
              write(unit=stdout,fmt=*) &
			  'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                iret, ' CH DATA IS READ INSTEAD OF ',n_totchan				
           cycle read_loop
         end if
         num_iasi_used = num_iasi_used + 1		 
         nread(inst) = nread(inst) + 1								
         iskip = 0
         jstart=1
         do i=1,n_totchan
!     check that channel number is within reason
            if (( allchan(1,i) > zero .and. allchan(1,i) < 99999._r_kind) .and. &  ! radiance bounds
               (allchan(2,i) < 8462._r_kind .and. allchan(2,i) > zero )) then     ! chan # bounds
!         radiance to BT calculation
              radi = allchan(1,i)
              scaleloop: do j=jstart,10
                 if(allchan(2,i) >= cscale(1,j) .and. allchan(2,i) <= cscale(2,j))then
                    radi = allchan(1,i)*sscale(j)
                    jstart=j
                    exit scaleloop
                 end if
              end do scaleloop
              if (rtm_option == rtm_option_crtm) then
#ifdef CRTM
                 call CRTM_Planck_Temperature(inst,i,radi,temperature(i))
#endif
              else if (rtm_option == rtm_option_rttov) then
#ifdef RTTOV
                 Effective_Temperature = LOG( ( coefs(inst)%coef%planck1(i) / radi ) + 1 )
                 Effective_Temperature = coefs(inst)%coef%planck2(i)/Effective_Temperature
                 temperature(i) = ( Effective_Temperature - coefs(inst)%coef%ff_bco(i) ) / &
                                  coefs(inst)%coef%ff_bcs(i)
#endif
              end if
              if(temperature(i) < tbmin .or. temperature(i) > tbmax ) then
                 temperature(i) = min(tbmax,max(zero,temperature(i)))

              end if
            else           ! error with channel number or radiance
!             write(6,*)'READ_IASI:  iasi chan error',i,allchan(1,i), allchan(2,i)
              temperature(i) = min(tbmax,max(zero,temperature(i)))

            end if
         end do
         if(iskip > 0)write(6,*) ' READ_IASI : iskip > 0 ',iskip
!         if( iskip >= 10 )cycle read_loop 		
         data_all(5) = sat_zenang             ! satellite zenith angle (deg)
         data_all(6) = allspot(11)            ! satellite azimuth angle (deg)
         data_all(9) = allspot(12)            ! solar zenith angle (deg)
         data_all(10)= allspot(13)            ! solar azimuth angle (deg)
         do l=1,nchan
            data_all(l+nreal) = temperature(l)   ! brightness temerature
         end do
				
!  4.0   assign information to sequential radiance structure
!--------------------------------------------------------------------------
         allocate ( p % tb_inv (1:nchan) )
         p%info             = info
         p%loc              = loc
         p%landsea_mask     = 1
         p%scanpos          = ifovn
         p%satzen           = data_all(5)
         p%satazi           = data_all(6)  ! look angle (deg) ! airsspot%bearaz
         p%solzen           = data_all(9)
         p%solazi           = data_all(10)
         p%tb_inv(1:nchan)  = data_all(nreal+1:nreal+nchan)
         p%sensor_index     = inst
         p%ifgat            = ifgat		

         allocate (p%next)   ! add next data
         p => p%next
         nullify (p%next) 		
     end do read_loop
  end do
  call closbf(lnbufr)

  !Deallocate temporary array for next bufrfile do loop
  deallocate(data_all)
end do bufrfile

   if (thinning .and. num_iasi_global > 0 ) then

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
               if ( ABS(out(j)-thinning_grid(n,ifgat)%score_crit(i)) > 1.0E-10 ) thinning_grid(n,ifgat)%ibest_obs(i) = 0
            end do
         end do
      end do

      deallocate( in  )
      deallocate( out )

#endif

      ! Delete the nodes which being thinning out
      p => head
      prev => head
      head_found = .false.
      num_iasi_used_tmp = num_iasi_used
      do j = 1, num_iasi_used_tmp
         n = p%sensor_index
         ifgat = p%ifgat
         found = .false.

         do i = 1, thinning_grid(n,ifgat)%itxmax
            if ( thinning_grid(n,ifgat)%ibest_obs(i) == j .and. thinning_grid(n,ifgat)%score_crit(i) < 9.99e6_r_kind ) then
               found = .true.
               exit
            end if
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
            end if
            deallocate ( current % tb_inv )
            deallocate ( current )
            num_iasi_thinned = num_iasi_thinned + 1
            num_iasi_used = num_iasi_used - 1
            nread(n) = nread(n) - 1
            continue
         end if

         if ( found .and. head_found ) then
            prev => p
            p => p%next
            continue
         end if

         if ( found .and. .not. head_found ) then
            head_found = .true.
            head => p
            prev => p
            p => p%next
         end if

      end do

   end if  ! End of thinning

   iv%total_rad_pixel   = iv%total_rad_pixel + num_iasi_used
   iv%total_rad_channel = iv%total_rad_channel + num_iasi_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_iasi_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_iasi_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=message(1),fmt='(a)') '   num_iasi_file num_iasi_global  num_iasi_local   num_iasi_used num_iasi_thinned'
   write(unit=message(2),fmt='(5(6x,i10))') num_iasi_file, num_iasi_global, num_iasi_local, num_iasi_used, num_iasi_thinned
   call da_message(message(1:2))

   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------  
 

   do i = 1, iv%num_inst 
    
      if (nread(i) < 1) cycle
      iv%instid(i)%num_rad  = nread(i)
      iv%instid(i)%info%nlocal = nread(i)
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         i, iv%instid(i)%rttovid_string, iv%instid(i)%num_rad
      call da_allocate_rad_iv (i, nchan, iv)
	  
   end do

   !  6.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   nread(1:rtminit_nsensor) = 0
   p => head

   do n = 1, num_iasi_used
      i = p%sensor_index 
      nread(i) = nread(i) + 1 
  
      call da_initialize_rad_iv (i, nread(i), iv, p)
  
      current => p
      p => p%next
      ! free current data
      deallocate ( current % tb_inv )
      deallocate ( current )
   end do
   deallocate ( p )
   deallocate (nread)
   deallocate (ptotal)

   call closbf(lnbufr)
   close(lnbufr)

!   call da_free_unit(lnbufr)

   if (trace_use) call da_trace_exit("da_read_obs_bufriasi")
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with a BUFR library"/))
#endif  


end subroutine da_read_obs_bufriasi
