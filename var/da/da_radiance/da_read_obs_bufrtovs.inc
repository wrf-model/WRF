subroutine da_read_obs_bufrtovs (obstype,iv, infile)

   !---------------------------------------------------------------------------
   !  Purpose: read in NCEP bufr tovs 1b data to innovation structure
   !
   !   METHOD: use F90 sequential data structure to avoid reading file twice  
   !            so that da_scan_bufrtovs is not necessary any more.
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !               and deallocate sequential data structure
   !   File History:
   !      Peiming Dong Added NPP atms, 2012/4/17
   !---------------------------------------------------------------------------

   implicit none

   character(5)      ,  intent (in)    :: obstype
   character(20)     ,  intent (in)    :: infile
   type (iv_type)    ,  intent (inout) :: iv

#ifdef BUFR

   integer          :: iost
   integer(i_kind), allocatable :: nread(:)

   integer(i_kind),parameter:: n1bhdr=15
   integer(i_kind),parameter:: n2bhdr=2
   integer(i_kind),parameter:: maxinfo=12
   integer(i_kind),parameter:: maxchanl=100

   logical hirs2,hirs3,hirs4,msu,amsua,amsub,mhs,atms
   logical outside, outside_all, iuse
   integer :: inst

   character(10) date
   character(8) subset,subfgn
   character(80) hdr1b
   character(80) hdr2b
   integer(i_kind) ihh,i,j,k,ifov,idd,ireadmg,ireadsb
   integer(i_kind) iret,idate,im,iy,nchan
   integer :: num_bufr(7), numbufr, ibufr
   character(20) :: filename

   ! thinning variables
   integer(i_kind) itt,itx,iobs,iout
   real(r_kind) terrain,timedif,crit,dist
   real(r_kind) dlon_earth,dlat_earth

   real(r_kind) tbmin,tbmax, tbbad
   real(r_kind) panglr,rato
   ! real(r_kind) rmask
   real(r_kind) step,start

   real(r_double),dimension(maxinfo+maxchanl):: data1b8
   real(r_double),dimension(n1bhdr):: bfr1bhdr
   real(r_double),dimension(n2bhdr):: bfr2bhdr

   ! Instrument triplet, follow the convension of RTTOV 
   integer   :: platform_id, satellite_id, sensor_id

   ! pixel information
   integer   ::  year,month,day,hour,minute,second  ! observation time
   real*8    ::  obs_time
   ! real      ::  rlat, rlon                         !  lat/lon in degrees   for Anfovs
   real      ::  satzen, satazi, solzen ,solazi       !  scan angles for Anfovs
   integer   ::  landsea_mask
   real      ::  srf_height
   ! channels' bright temperature
   real , allocatable ::   tb_inv(:)                    !  bright temperatures
   !  end type bright_temperature

   type (datalink_type), pointer    :: head, p, current, prev

   integer                        ::  ifgat
   type(info_type)                ::  info
   type(model_loc_type)           ::  loc

   data hdr1b /'SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA HOLS LSQL SOLAZI'/
   data hdr2b /'CLATH CLONH'/
   !  data hdr1b /'FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA HOLS LSQL SLNM BEARAZ'/

   data tbmin,tbmax,tbbad / 50.0_r_kind, 550.0_r_kind, -9.99e11_r_kind /
   integer :: num_tovs_local, num_tovs_file, num_tovs_global, num_tovs_selected
   integer :: num_tovs_thinned, num_tovs_used, num_tovs_used_tmp
   integer :: lnbufr
   integer :: n
   integer(i_kind), allocatable :: ptotal(:)
   real , allocatable :: in(:), out(:)
   logical :: found, head_found

   call da_trace_entry("da_read_obs_bufrtovs")

   ! Initialize variables

   nchan = 22
   allocate(nread(1:rtminit_nsensor))
   allocate(ptotal(0:num_fgat_time))
   nread(1:rtminit_nsensor) = 0
   ptotal(0:num_fgat_time) = 0

   ! Set various variables depending on type of data to be read

   ! platform_id  = 1                 !! for NOAA series
   ! platform_id  = 10                !! for METOP series

   hirs2=     obstype == 'hirs2'
   hirs3=     obstype == 'hirs3'
   hirs4=     obstype == 'hirs4'
   msu=       obstype == 'msu  '
   amsua=     obstype == 'amsua'
   amsub=     obstype == 'amsub'
   mhs=       obstype == 'mhs  '
   atms=      obstype == 'atms '

   if (hirs2) then
      sensor_id    =  0
      step   = 1.80_r_kind
      start  = -49.5_r_kind
      nchan=nchan_hirs2
      subfgn='NC021021'
      rato=1.1363987_r_kind
   else if (hirs3) then 
      sensor_id    =  0
      step   = 1.80_r_kind
      start  = -49.5_r_kind
      nchan=nchan_hirs3
      subfgn='NC021025'
   else if (hirs4) then 
      sensor_id    =  0
      step   = 1.80_r_kind
      start  = -49.5_r_kind
      nchan=nchan_hirs4
      subfgn='NC021028'
   else if (mhs) then 
      sensor_id    =  15
      step   = 10.0_r_kind/9.0_r_kind
      start  = -445.0_r_kind/9.0_r_kind
      nchan=nchan_mhs
      subfgn='NC021027'
   else if (msu) then
      sensor_id    =  1
      step   = 9.474_r_kind
      start  = -47.37_r_kind
      nchan=nchan_msu
      subfgn='NC021022'
      rato=1.1363987_r_kind
   else if (amsua) then
      sensor_id    =  3
      step   = three + one/three
      start  = -48.33_r_kind
      nchan=nchan_amsua
      subfgn='NC021023'
   else if (amsub)  then
      sensor_id    =  4
      step   = 1.1_r_kind
      start  = -48.95_r_kind
      nchan=nchan_amsub
      subfgn='NC021024'
   else if (atms) then
      sensor_id    =  19
      step   = 1.11_r_kind
      start  = -52.725_r_kind
      nchan=22
      subfgn='NC021203'
      hdr1b='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA HMSL LSQL SOLAZI'
   end if

   allocate (tb_inv(nchan))

   num_tovs_file     = 0    ! number of obs in file
   num_tovs_global   = 0    ! number of obs within whole domain
   num_tovs_local    = 0    ! number of obs within tile
   num_tovs_thinned  = 0    ! number of obs rejected by thinning
   num_tovs_used     = 0    ! number of obs entered into innovation computation
   num_tovs_selected = 0    ! number of obs limited for debuging
   iobs = 0                 ! for thinning, argument is inout

   ! 0.0  Open unit to satellite bufr file and read file header
   !--------------------------------------------------------------

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

!  We want to use specific unit number for bufr data, so we can control the endian format in environment. 
   lnbufr = 99

   open(unit=lnbufr,file=trim(filename),form='unformatted', &
      iostat = iost, status = 'old')
   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open file "//infile/))
      call da_trace_exit("da_read_obs_bufrtovs")
      return
   end if

   call openbf(lnbufr,'IN',lnbufr)
   call datelen(10)
   call readmg(lnbufr,subset,idate,iret)
   if (subset /= subfgn) then
      call closbf(lnbufr)
      close(lnbufr)
      message(1)='The file title does not match the data subset'
      write(unit=message(2),fmt=*) &
         'infile=', lnbufr, infile,' subset=', subset, ' subfgn=',subfgn
      call da_error(__FILE__,__LINE__,message(1:2))
   end if

   iy=0
   im=0
   idd=0
   ihh=0
   write(unit=date,fmt='( i10)') idate
   read(unit=date,fmt='(i4,3i2)') iy,im,idd,ihh
   write(unit=stdout,fmt=*) &
      'Bufr file date is ',iy,im,idd,ihh,infile

   ! Loop to read bufr file and assign information to a sequential structure
   !-------------------------------------------------------------------------

   if ( ibufr == 1 ) then
      allocate (head)
      !  allocate ( head % tb_inv (1:nchan) )
      nullify  ( head % next )
      p => head
   endif

   if (tovs_start > 1) then
      write (unit=stdout,fmt='(A,I6)') "   Skipping tovs obs before", tovs_start
   end if


   obs: do while (ireadmg(lnbufr,subset,idate)==0 .and. subset==subfgn)
      do while (ireadsb(lnbufr)==0)

         ! 1.0     Read header record and data record
         call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)
         call ufbint(lnbufr,bfr2bhdr,n2bhdr,1,iret,hdr2b)
         call ufbrep(lnbufr,data1b8,1,nchan,iret,'TMBR')
         ! call ufbrep(lnbufr,data1b8,1,1,iret,'BEARAZ')

         ! check if observation outside range

         num_tovs_file = num_tovs_file + 1

         ! 2.0     Extract observation location and other required information
         !     QC1:  judge if data is in the domain, read next record if not
         !------------------------------------------------------------------------
         ! rlat = bfr1bhdr(bufr_lat)
         ! rlon = bfr1bhdr(bufr_lat)
         ! if (rlon < 0.0) rlon = rlon+360.0

         if(abs(bfr2bhdr(1)) <= 90. .and. abs(bfr2bhdr(2)) <= 360.)then
            info%lat = bfr2bhdr(1)
            info%lon = bfr2bhdr(2)
         elseif(abs(bfr1bhdr(9)) <= 90. .and. abs(bfr1bhdr(10)) <= 360.)then
            info%lat = bfr1bhdr(9)
            info%lon = bfr1bhdr(10)
         endif

         call da_llxy (info, loc, outside, outside_all)

         if (outside_all) cycle

         !  3.0     Extract other information
         !------------------------------------------------------
         !  3.1     Extract satellite id and scan position. 
   
         if ( nint(bfr1bhdr(bufr_satellite_id)) >= 200 .and. nint(bfr1bhdr(bufr_satellite_id)) <= 204 ) then
            platform_id = 1
            satellite_id = nint(bfr1bhdr(bufr_satellite_id))-192
         else if ( nint(bfr1bhdr(bufr_satellite_id)) >= 205 .and. nint(bfr1bhdr(bufr_satellite_id)) <= 209 ) then
            platform_id = 1
            satellite_id = nint(bfr1bhdr(bufr_satellite_id))-191
         else if ( nint(bfr1bhdr(bufr_satellite_id)) == 223 ) then ! noaa-19
            platform_id = 1
            satellite_id = nint(bfr1bhdr(bufr_satellite_id))-204
         else if ( nint(bfr1bhdr(bufr_satellite_id)) >= 3 .and. nint(bfr1bhdr(bufr_satellite_id)) <= 5 ) then
            platform_id = 10
            satellite_id = nint(bfr1bhdr(bufr_satellite_id))-2
         else if ( nint(bfr1bhdr(bufr_satellite_id)) == 708 ) then ! tiros-n
            platform_id = 19
            satellite_id = 0
         else if ( nint(bfr1bhdr(bufr_satellite_id)) == 224 ) then ! npp
            platform_id = 17
            satellite_id = 0
         else if ( nint(bfr1bhdr(bufr_satellite_id)) >= 701 .and. nint(bfr1bhdr(bufr_satellite_id)) <= 707 ) then
            platform_id = 1
            satellite_id = nint(bfr1bhdr(bufr_satellite_id))-700
         end if
         ifov = nint(bfr1bhdr(bufr_ifov))    


         !  QC2:  limb pixel rejected (not implemented)

         !  3.2     Extract date information.
    
         year   = bfr1bhdr(bufr_year)   
         month  = bfr1bhdr(bufr_month)  
         day    = bfr1bhdr(bufr_day)    
         hour   = bfr1bhdr(bufr_hour)   
         minute = bfr1bhdr(bufr_minute) 
         second = bfr1bhdr(bufr_second) 
         
         write(unit=info%date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
            year, '-', month, '-', day, '_', hour, ':', minute, ':', second

         !  QC3: time consistency check with the background date

         if (year <= 99) then
            if (year < 78) then
               year = year + 2000
            else
               year = year + 1900
            end if
         end if

         call da_get_julian_time(year,month,day,hour,minute,obs_time)

         if (obs_time < time_slots(0) .or.  &
            obs_time >= time_slots(num_fgat_time)) cycle

         ! 3.2.1 determine FGAT index ifgat
   
         do ifgat=1,num_fgat_time
            if (obs_time >= time_slots(ifgat-1) .and.  &
                obs_time  < time_slots(ifgat)) exit
         end do

         ! 3.3 Find wrfvar instrument index from RTTOV instrument triplet
         !     go to next data if id is not in the lists

         inst = 0
         do i = 1, rtminit_nsensor
            if (platform_id  == rtminit_platform(i) &
               .and. satellite_id == rtminit_satid(i)    &
               .and. sensor_id    == rtminit_sensor(i)) then
               inst = i
               exit
            end if
         end do
         if (inst == 0) cycle

         ! 3.4 extract satellite and solar angle
   
         panglr=(start+float(ifov-1)*step)*deg2rad
         if (hirs2 .or. msu) then
            satzen = asin(rato*sin(panglr))*rad2deg
            satzen = abs(satzen)
         else
            satzen = bfr1bhdr(bufr_satzen) !*deg2rad   ! local zenith angle
            satzen = abs(satzen)
 
            ! if (amsua .and. ifov .le. 15) satzen=-satzen
            ! if (amsub .and. ifov .le. 45) satzen=-satzen
            ! if (hirs3 .and. ifov .le. 28) satzen=-satzen
         end if
         if ( satzen > 65.0 ) cycle   ! CRTM has a satzen > 65.0 check
         satazi = panglr*rad2deg            ! look angle
         ! if (satazi<0.0) satazi = satazi+360.0
         solzen = bfr1bhdr(bufr_solzen)              ! solar zenith angle
         solazi = bfr1bhdr(bufr_solazi)              !RTTOV9_3

         num_tovs_global = num_tovs_global + 1
         ptotal(ifgat) = ptotal(ifgat) + 1

         if (num_tovs_global < tovs_start) then
            cycle
         end if

         if (num_tovs_global > tovs_end) then
            write (unit=stdout,fmt='(A,I6)') "   Skipping radiance obs after", tovs_end
            exit obs
         end if

         num_tovs_selected = num_tovs_selected + 1
 
         if (num_tovs_selected > max_tovs_input) then
            write(unit=message(1),fmt='(A,I10,A)') &
               "Max number of tovs",max_tovs_input," reached"
            call da_warning(__FILE__,__LINE__,message(1:1))
            num_tovs_selected = num_tovs_selected-1
            num_tovs_global   = num_tovs_global-1
            ptotal(ifgat) = ptotal(ifgat) - 1
            exit obs
         end if

         if (outside) cycle ! No good for this PE
         num_tovs_local = num_tovs_local + 1

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
            timedif = 0.0 !2.0_r_kind*abs(tdiff)        ! range:  0 to 6
            terrain = 0.01_r_kind*abs(bfr1bhdr(13))
            crit = 1.0 !0.01_r_kind+terrain + timedif !+ 10.0_r_kind*float(iskip)
            call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
            if (.not. iuse) then
               num_tovs_thinned=num_tovs_thinned+1
               cycle
            end if
         end if

         num_tovs_used = num_tovs_used + 1
         nread(inst) = nread(inst) + 1

         ! 3.5 extract surface information
         srf_height = bfr1bhdr(bufr_station_height)          ! station height
         if (srf_height < 8888.0 .AND. srf_height > -416.0) then
         else
            srf_height = 0.0
         endif  

         landsea_mask = nint(bfr1bhdr(bufr_landsea_mask))  ! 0:land ; 1:sea (same as RTTOV)
         ! rmask=one                          ! land
         ! if (nint(bfr1bhdr(bufr_landsea_mask))==1) rmask=0.0_r_kind   ! reverse the land/sea mask in bufr
         ! landsea_mask = rmask+.001_r_kind             ! land sea mask

         info%elv = srf_height

         ! 3.6 extract channel bright temperature
   
         tb_inv(1:nchan) = data1b8(1:nchan)
         do k = 1, nchan
            if ( tb_inv(k) < tbmin .or. tb_inv(k) > tbmax) &
               tb_inv(k) = missing_r
         end do
         if ( all(tb_inv<0.0) ) then
            num_tovs_local = num_tovs_local -1
            num_tovs_used = num_tovs_used - 1
            nread(inst) = nread(inst) - 1
            cycle
         end if

         !  4.0   assign information to sequential radiance structure
         !--------------------------------------------------------------------------
         allocate (p % tb_inv (1:nchan))
         p%info             = info
         p%loc              = loc
         p%landsea_mask     = landsea_mask
         p%scanpos          = ifov
         p%satzen           = satzen
         p%satazi           = satazi
         p%solzen           = solzen
         p%tb_inv(1:nchan)  = tb_inv(1:nchan)
         p%sensor_index     = inst
         p%ifgat            = ifgat
!RTTOV9_3
         p%solazi           = solazi
 !end of RTTOV9_3
         allocate (p%next)   ! add next data

         p => p%next
         nullify (p%next)
      end do
   end do obs

   call closbf(lnbufr)
   close(lnbufr)

end do bufrfile

   if (thinning .and. num_tovs_global > 0 ) then

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
      p => head
      prev => head
      head_found = .false.
      num_tovs_used_tmp = num_tovs_used
      do j = 1, num_tovs_used_tmp
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
            num_tovs_thinned = num_tovs_thinned + 1
            num_tovs_used = num_tovs_used - 1
            nread(n) = nread(n) - 1
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

   endif  ! End of thinning

   iv%total_rad_pixel   = iv%total_rad_pixel   + num_tovs_used
   iv%total_rad_channel = iv%total_rad_channel + num_tovs_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_tovs_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_tovs_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1) 
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i) 
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'num_tovs_file num_tovs_global num_tovs_local num_tovs_used num_tovs_thinned'
   write(unit=stdout,fmt='(5i10)') num_tovs_file,num_tovs_global, num_tovs_local,num_tovs_used,num_tovs_thinned

   deallocate(tb_inv)  

   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------  
   
   do i = 1, iv%num_inst
      if (nread(i) < 1) cycle
      iv%instid(i)%num_rad = nread(i)
      iv%instid(i)%info%nlocal = nread(i)
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         i, iv%instid(i)%rttovid_string, iv%instid(i)%num_rad

      call da_allocate_rad_iv(i,nchan,iv)

   end do
   
   !  6.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   nread(1:rtminit_nsensor) = 0 
   p => head
   ! do while ( associated(p) )

   do n = 1, num_tovs_used
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

   ! check if sequential structure has been freed
   !
   ! p => head
   ! do i = 1, num_rad_selected
   !    write (unit=stdout,fmt=*)  i, p%tb_inv(1:nchan)
   !    p => p%next
   ! end do

   call da_trace_exit("da_read_obs_bufrtovs")
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with a BUFR library"/)) 
#endif
  

end subroutine da_read_obs_bufrtovs


