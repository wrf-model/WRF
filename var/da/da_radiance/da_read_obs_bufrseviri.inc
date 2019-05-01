subroutine da_read_obs_bufrseviri (obstype,iv,infile)	 

! subprogram:    read_seviri                  read bufr format seviri data
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
   !  HISTORY: 2013/03/26 - Creation            Hongli Wang 
   !
   !------------------------------------------------------------------------------

  implicit none

  real(r_kind)     :: POinT001 =   0.001_r_kind
  real(r_kind)     :: POinT01  =   0.01_r_kind
  real(r_kind)     :: TEN      =  10.0_r_kind
  real(r_kind)     :: R45      =  45.0_r_kind
  real(r_kind)     :: R60      =  60.0_r_kind
  real(r_kind)     :: R90      =  90.0_r_kind
  real(r_kind)     :: R180     = 180.0_r_kind
  real(r_kind)     :: R360     = 360.0_r_kind  

  character(9)      ,  intent (in)  :: obstype
  type (iv_type)    ,intent (inout) :: iv
#ifdef BUFR  
  real(kind=8)    ::  obs_time  
  type (datalink_type), pointer  :: head, p, current, prev
  type(info_type)                :: info
  type(model_loc_type)           :: loc 
  type(model_loc_type)           :: loc_fov  

!!! for seviri
  character(80),  intent (in) :: infile

  character(8)       :: subset,subcsr,subasr
  character(80)      :: hdrsevi                             ! seviri header

  integer(i_kind)    :: nchanl,ilath,ilonh,ilzah,iszah,irec 
  integer(i_kind)    :: nhdr,nchn,ncld,nbrst !,idate,lnbufr
  integer(i_kind)    :: ireadmg,ireadsb,iret

  real(r_double),allocatable,dimension(:)   :: hdr         
  real(r_double),allocatable,dimension(:,:) :: datasev1,datasev2   

  logical            :: clrsky,allsky,allchnmiss
  real               :: rclrsky
  integer :: kidsat 
  integer(i_kind)    :: idate5(6)
  integer (i_kind), allocatable :: ptotal(:), nread(:) 
  integer(i_kind)    :: idate, im, iy, idd, ihh
!!! end for seviri

  ! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: nchan = 8       
  integer(i_kind),parameter :: n_totchan  = 12 
  integer(i_kind),parameter :: maxinfo    =  33
  integer(i_kind)   :: inst,platform_id,satellite_id,sensor_id
  real(r_kind)      :: crit
  integer(i_kind)   :: ifgat, iout, iobs
  logical           :: outside, outside_all, iuse,outside_fov

  integer           :: numbufr,ibufr,jj  
  logical :: found, head_found 
  real(r_kind)      :: step, start,step_adjust
  character(len=4)  :: senname
  integer(i_kind)   :: size,size_tmp
  character(10)     :: date
  real(r_kind)      :: sstime, tdiff, t4dv
  integer(i_kind)   :: nmind

  ! Other work variables
  real(r_kind)     :: clr_amt,piece
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth,dlon_earth_deg,dlat_earth_deg
  real(r_kind)     :: lza, lzaest,sat_height_ratio
  real(r_kind)     :: pred, crit1, dist1
  real(r_kind)     :: sat_zenang
  real(r_kind)     :: radi
  real(r_kind)     :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr
  real(r_kind),dimension(0:4) :: rlndsea
  real(r_kind),dimension(0:3) :: sfcpct
  real(r_kind),dimension(0:3) :: ts
  real(r_kind),dimension(10) :: sscale
  real(r_kind),dimension(n_totchan) :: temperature
  real(r_kind),allocatable,dimension(:):: data_all
  real(r_kind) disterr,disterrmax,rlon00,rlat00

  logical          :: assim,valid
  logical          :: seviri 
  logical          :: data_on_edges,luse
  integer(i_kind)  :: nreal, ityp,ityp2, isflg
  integer(i_kind)  :: ifov, instr, ioff, ilat, ilon, sensorindex
  integer(i_kind)  :: num_seviri_file
  integer(i_kind)  :: num_seviri_local, num_seviri_global, num_seviri_used, num_seviri_thinned 
  integer(i_kind)  :: num_seviri_used_tmp  
  integer(i_kind)  :: i, j, l, iskip, ifovn, bad_line

  integer(i_kind)  :: itx, k, nele, itt, n
  integer(i_kind)  :: iexponent
  integer(i_kind)  :: idomsfc
  integer(i_kind)  :: ntest
  integer(i_kind)  :: error_status
  integer(i_kind)  :: num_bufr(7)

  integer          :: iost, lnbufr 
  character(20)    ::filename  
  real, allocatable :: in(:), out(:)

  ! Set standard parameters
  integer(i_kind),parameter:: ichan=-999  ! fov-based surface code is not channel specific for seviri 
  real(r_kind),parameter:: expansion=one  ! exansion factor for fov-based location code. 
  real(r_kind),parameter:: tbmin  = 50._r_kind
  real(r_kind),parameter:: tbmax  = 550._r_kind
  real(r_kind),parameter:: earth_radius = 6371000._r_kind

  ilath=8                      ! the position of latitude in the header
  ilonh=9                      ! the position of longitude in the header
  ilzah=10                     ! satellite zenith angle
  iszah=11                     ! solar zenith angle
  subcsr='NC021043'            ! sub message
  subasr='NC021042'            ! sub message

   if(trace_use) call da_trace_entry("da_read_obs_bufrseviri")

  !  0.0  Initialize variables
  !-----------------------------------
  sensor_id = 21 !seviri
  disterrmax=zero
  ntest=0
  nreal  = maxinfo
  seviri= obstype == 'seviri'

  bad_line=-1
  step  = 1.0
  start = 0.0
  step_adjust = 1.0_r_kind
  senname = 'SEVIRI'
  num_bufr(:)=0
  numbufr=0
  allocate(nread(1:rtminit_nsensor))
  allocate(ptotal(0:num_fgat_time))
  nread(1:rtminit_nsensor) = 0
  ptotal(0:num_fgat_time) = 0
  iobs = 0                 ! for thinning, argument is inout  
  num_seviri_file    = 0
  num_seviri_local   = 0
  num_seviri_global  = 0
  num_seviri_used    = 0
  num_seviri_thinned = 0  

  ! 1.0 Assign file names and prepare to read bufr files 
  !-------------------------------------------------------------------------

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

   ! dont change, WRFDA uses specified units to read radiance data
   lnbufr = 92 
   open(unit=lnbufr,file=trim(filename),form='unformatted', &
      iostat = iost, status = 'old' ) !,convert='little_endian')
   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open file "//infile/))
      if(trace_use) call da_trace_exit("da_read_obs_bufrsevri")
      return
   end if

   ! Open BUFR table
   call openbf(lnbufr,'IN',lnbufr) 
   call datelen(10)
   call readmg(lnbufr,subset,idate,iret)

   ! Check the data set
  if( iret/=0) then
     write(message(1),fmt='(A)')'SKIP PROCESSING OF SEVIRI FILE'
     write(message(2),fmt='(A,I2,A)')'infile = ', lnbufr, infile
     call da_warning(__FILE__,__LINE__,message(1:2))
     if(trace_use) call da_trace_exit("da_read_obs_bufrseviri")
     return 
  endif

  clrsky=.false.
  allsky=.false.
  if(subset == subcsr) then
     clrsky=.true.
     write(message(1),fmt='(A)')'READ SEVIRI FILE'
     write(message(2),fmt='(A,L1,2A)')'clrsky= ', clrsky,' subset= ', subset
     call da_message(message(1:2))
  elseif(subset == subasr) then
     allsky=.true.
     write(message(1),fmt='(A)')'READ SEVIRI FILE'
     write(message(2),fmt='(A,L1,2A)')'allsky= ', allsky,' subset= ', subset
     call da_message(message(1:2))
  else
     write(message(1),fmt='(A)')'SKIP PROCESSING OF UNKNOWN SEVIRI FILE'
     write(message(2),fmt='(A,I2,3A)')'infile=', lnbufr, infile,' subset=', subset
     call da_warning(__FILE__,__LINE__,message(1:2))
     if(trace_use) call da_trace_exit("da_read_obs_bufrseviri")
     return 
  endif

  ! Set BUFR string based on seviri data set
  if (clrsky) then
     hdrsevi='SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA SOZA'
     nhdr=11
     nchn=12
     ncld=nchn
     nbrst=nchn
  else if (allsky) then
     hdrsevi='SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH'
     nhdr=9
     nchn=11
     ncld=2
     nbrst=nchn*6                ! channel dependent: all, clear, cloudy, low,
                                 !middle and high clouds
  endif
  allocate(datasev1(1,ncld))     ! not channel dependent
  allocate(datasev2(1,nbrst))    ! channel dependent: all, clear, cloudy, low,
                                 !middle and high clouds
  allocate(hdr(nhdr))



   iy=0
   im=0
   idd=0
   ihh=0

   sensorindex=1  
   write(unit=date,fmt='( i10)') idate
   read(unit=date,fmt='(i4,3i2)') iy,im,idd,ihh
   write(unit=stdout,fmt='(a,4i4,2x,a)') &
      'Bufr file date is ',iy,im,idd,ihh,trim(infile)

   ! 2.0 Loop to read bufr file and assign information to a sequential structure
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
         num_seviri_file = num_seviri_file + 1	 

    ! Read SEVIRI information
         call ufbint(lnbufr,hdr,nhdr,1,iret,hdrsevi)				

        kidsat = nint(hdr(1))
        ! SAID 55 is meteosat-8  or msg-1
        ! SAID 56 is meteosat-9  or msg-2
        ! SAID 57 is meteosat-10 or msg-3
        if ( ( kidsat > 57) .or. ( kidsat < 55) ) then 
           write(unit=message(1),fmt='(A,I6)') 'Unknown platform: ', kidsat
           call da_warning(__FILE__,__LINE__,message(1:1))
        end if
        platform_id  = 12  ! MSG - Meteosat Second Generation
        if ( kidsat == 55 ) then
            satellite_id = 1
        else if ( kidsat == 56 ) then
            satellite_id = 2
        else if ( kidsat == 57 ) then
            satellite_id = 3
        end if

        if (clrsky) then     ! asr bufr has no sza
        ! remove the obs whose satellite zenith angles larger than 65 degree
           if ( hdr(ilzah) > 65.0 ) cycle read_loop
        end if

        call ufbint(lnbufr,datasev1,1,ncld,iret,'NCLDMNT')

        if(iret /= 1) cycle read_loop
        do n=1,ncld
           if(datasev1(1,n)>= 0.0 .and. datasev1(1,n) <= 100.0 ) then
              rclrsky=datasev1(1,n)
               ! first QC filter out data with less clear sky fraction
               if ( rclrsky < 70.0 ) cycle read_loop
               !if ( rclrsky < 90.0 ) cycle read_loop
           end if
        end do

        call ufbrep(lnbufr,datasev2,1,nbrst,iret,'TMBRST')
        
        ! Check if data of channel 1-11 are missing

        allchnmiss=.true.
        do n=1,11
           if(datasev2(1,n)<500.)  then
              allchnmiss=.false.
           end if
        end do
        if(allchnmiss) cycle read_loop

        ! Check observing position
         info%lat  =  hdr(ilath)           ! latitude
         info%lon  =  hdr(ilonh)           ! longitude
         if( abs(info%lat) > R90  .or. abs(info%lon) > R360 .or. &
         (abs(info%lat) == R90 .and. info%lon /= ZERO) )then
         write(unit=stdout,fmt=*) &
         'READ_SEVIRI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
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
		 
        ! Check obs time
         idate5(1) = nint(hdr(2)) ! year
         idate5(2) = nint(hdr(3)) ! month
         idate5(3) = nint(hdr(4)) ! day
         idate5(4) = nint(hdr(5)) ! hour
         idate5(5) = nint(hdr(6)) ! minute
         idate5(6) = nint(hdr(7)) ! second		
		
         if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
             idate5(2) < 1    .or. idate5(2) >   12 .or. &
             idate5(3) < 1    .or. idate5(3) >   31 .or. &
             idate5(4) < 0    .or. idate5(4) >   24 .or. &
             idate5(5) < 0    .or. idate5(5) >   60 ) then

            write(message(1),fmt='(A)')'ERROR IN READING SEVIRI BUFR DATA'
            write(message(2),fmt='(A,5I8)')'STRANGE OBS TIME (YMDHM):', idate5(1:5)
            call da_warning(__FILE__,__LINE__,message(1:2))
            cycle read_loop
         end if

         call da_get_julian_time(idate5(1),idate5(2),idate5(3),idate5(4),idate5(5),obs_time)		
         if ( obs_time < time_slots(0) .or.  &
           obs_time >= time_slots(num_fgat_time) ) cycle read_loop
         do ifgat=1,num_fgat_time
            if ( obs_time >= time_slots(ifgat-1) .and.  &
                obs_time  < time_slots(ifgat) ) exit
         end do	
         num_seviri_global = num_seviri_global + 1
         ptotal(ifgat) = ptotal(ifgat) + 1  

         if (outside) cycle ! No good for this PE		
         num_seviri_local = num_seviri_local + 1
		 
         write(unit=info%date_char, &
         fmt='(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
         idate5(1), '-', idate5(2), '-', idate5(3), '_', idate5(4), &
         ':', idate5(5), ':', idate5(6)
         info%elv = 0.0  !aquaspot%selv		   
		 
         ! 3.0  Make Thinning
         ! Map obs to thinning grid
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
               num_seviri_thinned=num_seviri_thinned+1
               cycle
            end if
         end if		

         ! data SEVIRI channel number(CHNM) and radiance (SCRA)

         num_seviri_used = num_seviri_used + 1		 
         nread(inst) = nread(inst) + 1								
         iskip = 0
         do i=1,n_totchan
            ! check that tb is within reasonal bound
            if ( datasev2(1,i) < tbmin .or. datasev2(1,i) > tbmax ) then
               temperature(i) = missing_r
            else 
               temperature(i) = datasev2(1,i)
            end if
         end do

         if(iskip > 0)write(6,*) ' READ_SEVIRI : iskip > 0 ',iskip

         do l=1,nchan
            data_all(l+nreal) = temperature(l+3)   ! brightness temerature
         end do
				
    ! 4.0 assign information to sequential radiance structure
    !--------------------------------------------------------------------------
    !        iscan = nint(hdr(ilzah))+1.001_r_kind 
         allocate ( p % tb_inv (1:nchan ))
         p%info             = info
         p%loc              = loc
         p%landsea_mask     = 1
         p%scanpos          = nint(hdr(ilzah))+1.001_r_kind 
         p%satzen           = hdr(ilzah)               ! satellite zenith angle (deg)
         p%satazi           = 0.0                      ! satellite azimuth angle (deg) 
         p%solzen           = 0.0                      ! solar zenith angle (deg)  
         p%solazi           = 0.0                      ! solar azimuth angle (deg) 
         p%tb_inv(1:nchan)  = data_all(nreal+1:nreal+nchan)
         p%sensor_index     = inst
         p%ifgat            = ifgat		

         allocate (p%next)   ! add next data
         p => p%next
         nullify (p%next) 		
     end do read_loop
  end do
  call closbf(lnbufr)

  !Deallocate temporary arrays for next bufrfile do loop
  deallocate(datasev1)
  deallocate(datasev2)
  deallocate(hdr)
  deallocate(data_all)
end do bufrfile

   if (thinning .and. num_seviri_global > 0 ) then

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
      num_seviri_used_tmp = num_seviri_used
      do j = 1, num_seviri_used_tmp
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
            num_seviri_thinned = num_seviri_thinned + 1
            num_seviri_used = num_seviri_used - 1
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

   iv%total_rad_pixel   = iv%total_rad_pixel + num_seviri_used
   iv%total_rad_channel = iv%total_rad_channel + num_seviri_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_seviri_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_seviri_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') '   num_seviri_file num_seviri_global  num_seviri_local   num_seviri_used num_seviri_thinned'
   write(stdout,'(5(8x,i10))') num_seviri_file, num_seviri_global, num_seviri_local, num_seviri_used, num_seviri_thinned

  

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

   do n = 1, num_seviri_used
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

   call da_free_unit(lnbufr)

   if(trace_use) call da_trace_exit("da_read_obs_bufrseviri")
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with a BUFR library"/))
#endif  


end subroutine da_read_obs_bufrseviri
