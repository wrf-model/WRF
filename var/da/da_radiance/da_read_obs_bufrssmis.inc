subroutine da_read_obs_bufrssmis (obstype,iv,infile)

   !---------------------------------------------------------------------------
   !  Purpose: read in NCEP bufr SSM/IS data to innovation structure
   !
   !   METHOD: use F90 sequential data structure to avoid reading file twice  
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !               and deallocate sequential data structure
   !---------------------------------------------------------------------------



   use da_control

   implicit none

   character(5) ,     intent (in)    :: obstype    ! ssmis
   character(20),     intent (in)    :: infile     ! ssmis.bufr
   type (iv_type),    intent (inout) :: iv

#ifdef BUFR

   integer(i_kind), parameter :: bufsat_dmsp16 = 249  ! DMSP16 BUFR identifier
   integer(i_kind), parameter :: bufsat_dmsp17 = 285  ! DMSP17 BUFR identifier
   integer(i_kind), parameter :: bufsat_dmsp18 = 286  ! DMSP18 BUFR identifier
   integer(i_kind), parameter :: n1bhdr = 15
   integer(i_kind), parameter :: maxchanl = 24
   real(r_kind),    parameter :: tbmin = 70.0_r_kind
   real(r_kind),    parameter :: tbmax = 320.0_r_kind

   character(80) :: hdr1b
   !data hdr1b /'SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SLNM ORBN SELV SURF RAINF'/
   data hdr1b /'SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SLNM ORBN SELV SFLG RFLAG'/
   character(10) :: date
   character(8)  :: subset, subfgn
   character(20) :: filename

   logical :: outside, outside_all

   integer(i_kind) :: iost, inst, lnbufr, ifgat
   integer(i_kind) :: num_bufr(7),numbufr,ibufr
   integer(i_kind) :: ihh, i, j, n, k, slnm, ifov, idd, ireadmg, ireadsb
   integer(i_kind) :: iret, idate, im, iy
   integer(i_kind) :: jc, incangl, bch, landsea_mask, rain_flag
   integer(i_kind) :: platform_id, satellite_id, sensor_id, nchan, num_ssmis_file
   integer(i_kind) :: num_ssmis_local, num_ssmis_global, num_ssmis_used, num_ssmis_thinned
   integer(i_kind) :: num_ssmis_used_tmp

   real(r_double), dimension(2,maxchanl) :: bufrtbb
   real(r_double), dimension(n1bhdr)     :: bfr1bhdr

   ! pixel information
   integer(i_kind)   ::  year,month,day,hour,minute,second  ! observation time
   real(kind=8)    ::  obs_time
   real(r_double), allocatable ::  tb_inv(:)          !  bright temperatures

   type (datalink_type), pointer  :: head, p, current, prev
   type(info_type)                :: info
   type(model_loc_type)           :: loc

   ! thinning variables
   integer(i_kind) :: itt,itx,iobs,iout
   real(r_kind)    :: terrain,timedif,crit,dist
   real(r_kind)    :: dlon_earth,dlat_earth
   logical         :: iuse
   real, allocatable :: in(:), out(:)
   logical           :: found, head_found

   integer(i_kind), allocatable :: ptotal(:), nread(:)

   call da_trace_entry("da_read_obs_bufrssmis")

   allocate(nread(1:rtminit_nsensor))
   allocate(ptotal(0:num_fgat_time))
   nread(1:rtminit_nsensor) = 0
   ptotal(0:num_fgat_time) = 0

   platform_id  = 2                 ! for DMSP series
   sensor_id    = 10                ! for SSMIS
   nchan        = nchan_ssmis

   allocate (tb_inv(nchan))
   num_ssmis_file    = 0
   num_ssmis_local   = 0
   num_ssmis_global  = 0
   num_ssmis_used    = 0
   num_ssmis_thinned = 0
   iobs = 0                 ! for thinning, argument is inout

   ! 0.0  Open unit to satellite bufr file and read file header
   !--------------------------------------------------------------

!   call da_get_unit(lnbufr)

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

   lnbufr=98
   open(unit=lnbufr,file=trim(filename),form='unformatted', &
      iostat = iost, status = 'old')
   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open file "//infile/))
      call da_trace_exit("da_read_obs_bufrssmis")
      return
   end if

   call openbf(lnbufr,'IN',lnbufr)
   call datelen(10)
   call readmg(lnbufr,subset,idate,iret)

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
      nullify  ( head % next )
      p => head
   end if

! Set various variables depending on type of data to be read

   !subfgn = 'NC003003'
   subfgn = 'NC021201'
   incangl = 53.2_r_kind

   subset_loop: do while (ireadmg(lnbufr,subset,idate)==0)

      read_loop: do while (ireadsb(lnbufr)==0 .and. subset==subfgn)

         num_ssmis_file = num_ssmis_file + 1
         ! 1.0     Read header record and data record

         call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)
         call ufbrep(lnbufr,bufrtbb,2,maxchanl,iret,"CHNM TMBR" )

         ! check if observation outside range

         ! 2.0     Extract observation location and other required information
         !     QC1:  judge if data is in the domain, read next record if not
         !------------------------------------------------------------------------

         info%lat  =  bfr1bhdr(bufr_lat)
         info%lon  =  bfr1bhdr(bufr_lon)
         call da_llxy (info, loc, outside, outside_all)


         if (outside_all) cycle

         !  3.0     Extract other information

         info%elv  = 0.0
         landsea_mask = nint(bfr1bhdr(bufr_landsea_mask))   ! ssmis surface flag
                                                            ! 0:land, 2:near coast, 3:ice,
                                                            ! 4:possible ice, 5:ocean, 6:coast
         ! RTTOV surftype: 0:land, 1:sea, 2:sea ice
         if ( landsea_mask == 5 ) then
            landsea_mask = 1
         else if ( landsea_mask == 2 .or. landsea_mask == 6 ) then
            landsea_mask = 0
         else if ( landsea_mask == 3 .or. landsea_mask == 4 ) then
            landsea_mask = 2
         end if
         rain_flag = nint(bfr1bhdr(15))    ! 0:no rain, 1:rain

         !------------------------------------------------------
         !  3.1     Extract satellite id and scan position. 
   
         if (nint(bfr1bhdr(bufr_satellite_id)) == bufsat_dmsp16) then
            satellite_id = 16
         else if (nint(bfr1bhdr(bufr_satellite_id)) == bufsat_dmsp17) then
            satellite_id = 17
         else if (nint(bfr1bhdr(bufr_satellite_id)) == bufsat_dmsp18) then
            satellite_id = 18
         end if

         ! 3.3 Find wrfvar instrument index from RTTOV instrument triplet
         !     go to next data if id is not in the lists

         inst = 0
         do i = 1, rtminit_nsensor
            if (platform_id  == rtminit_platform(i)      &
               .and. satellite_id == rtminit_satid(i)    &
               .and. sensor_id    == rtminit_sensor(i)) then
               inst = i
               exit
            end if
         end do
         if (inst == 0) cycle read_loop

         !  3.1     Extract scan number and scan position. 

         slnm = nint(bfr1bhdr(11))
         ifov = nint(bfr1bhdr(bufr_ifov))

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
            obs_time >= time_slots(num_fgat_time)) cycle read_loop

         ! 3.2.1 determine FGAT index ifgat
   
         do ifgat=1,num_fgat_time
            if (obs_time >= time_slots(ifgat-1) .and.  &
                obs_time  < time_slots(ifgat)) exit
         end do

         num_ssmis_global = num_ssmis_global + 1
         ptotal(ifgat) = ptotal(ifgat) + 1

         if (outside) cycle ! No good for this PE

         num_ssmis_local = num_ssmis_local + 1

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
            timedif = 0. !2.0_r_kind*abs(tdiff)        ! range:  0 to 6
            terrain = 0.01_r_kind*abs(bfr1bhdr(13))
            crit = 1. !0.01_r_kind+terrain + timedif !+ 10._r_kind*float(iskip)
            call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
            if (.not. iuse) then
               num_ssmis_thinned=num_ssmis_thinned+1
               cycle
            end if
         end if

         num_ssmis_used = num_ssmis_used + 1
         nread(inst) = nread(inst) + 1

         if (num_ssmis_used > max_ssmis_input) then
            write(unit=message(1),fmt='(A,I10,A)') &
               "Max number of ssmis",max_ssmis_input," reached"
            call da_warning(__FILE__,__LINE__,message(1:1))
            num_ssmis_used = num_ssmis_used - 1
            exit read_loop
         end if

         ! 3.4 extract satellite and solar angle
   
         ! 3.5 extract surface information

         ! 3.6 extract channel bright temperature
   
         tb_inv(1:nchan) = missing_r

         do jc = 1, nchan
            bch = nint(bufrtbb(1,jc))     !ch index from bufr
            tb_inv(jc) = bufrtbb(2,jc)
            if (tb_inv(jc) < tbmin .or. tb_inv(jc) > tbmax .or. bch /= jc) then
                tb_inv(jc) = missing_r
            end if
         end do

         if ( maxval(tb_inv(:)) > missing_r ) then

            !  4.0   assign information to sequential radiance structure
            !--------------------------------------------------------------------------
            allocate (p % tb_inv (1:nchan))
            p%info                = info
            p%loc                 = loc
            p%landsea_mask        = landsea_mask
            p%scanline            = slnm
            p%scanpos             = ifov
            p%satzen              = incangl
            p%satazi              = 0.0     ! dummy value
            p%solzen              = 0.0     ! dummy value
            p%tb_inv(1:nchan)     = tb_inv(1:nchan)
            p%sensor_index        = inst
            p%ifgat               = ifgat
            p%rain_flag           = rain_flag

            allocate (p%next)   ! add next data
            p => p%next
            nullify (p%next)

         else

            num_ssmis_local = num_ssmis_local - 1
            num_ssmis_used  = num_ssmis_used - 1

         end if

      end do read_loop

   end do subset_loop

  call closbf(lnbufr)
  close(lnbufr)

end do bufrfile

   if (thinning .and. num_ssmis_global > 0 ) then

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
      num_ssmis_used_tmp = num_ssmis_used
      do j = 1, num_ssmis_used_tmp
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
            num_ssmis_thinned = num_ssmis_thinned + 1
            num_ssmis_used = num_ssmis_used - 1
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

   iv%total_rad_pixel   = iv%total_rad_pixel + num_ssmis_used
   iv%total_rad_channel = iv%total_rad_channel + num_ssmis_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_ssmis_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_ssmis_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'num_ssmis_file, num_ssmis_global, num_ssmis_local, num_ssmis_used, num_ssmis_thinned'
   write(stdout,*) num_ssmis_file, num_ssmis_global, num_ssmis_local, num_ssmis_used, num_ssmis_thinned

   deallocate(tb_inv)  

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

   do n = 1, num_ssmis_used
      i = p%sensor_index
      nread(i) = nread(i) + 1
      call da_initialize_rad_iv (i, nread(i), iv, p)

      iv%instid(i)%rain_flag(n) = p%rain_flag

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

   call da_trace_exit("da_read_obs_bufrssmis")
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with a BUFR library"/))
#endif
 

end subroutine da_read_obs_bufrssmis

