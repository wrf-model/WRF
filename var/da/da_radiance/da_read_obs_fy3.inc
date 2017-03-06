subroutine da_read_obs_fy3 (obstype,iv, infile)

   !---------------------------------------------------------------------------
   !  Purpose: read in fy3 1b data to innovation structure
   !
   !   Dong peiming 2012/03/09
   !   METHOD: use F90 sequential data structure to avoid reading file twice  
   !            so that da_scan_bufrtovs is not necessary any more.
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !               and deallocate sequential data structure
   !---------------------------------------------------------------------------

   implicit none

   character(5)      ,  intent (in)    :: obstype
   character(20)     ,  intent (in)    :: infile
   type (iv_type)    ,  intent (inout) :: iv

!
TYPE type_rad_FY3
    INTEGER :: yyyy,mn,dd,hh,mm,ss
    INTEGER :: iscanline,iscanpos
    REAL*4  :: rlat,rlon !lat/lon in degrees for Anfovs
    INTEGER :: isurf_height, isurf_type !height/type for Anfovs
    REAL*4  :: satzen,satazi,solzen,solazi !scan angles for Anfovs
    REAL*4  :: tbb(20) !bright temperatures
!   REAL*4  :: btemps(20)
    INTEGER :: iavhrr(13),ihirsflag
    INTEGER :: iprepro(5) ! values from pre-processing
    REAL*4  :: clfra ! Cloud cover (<1.0)
    REAL*4  :: ts ! Skin temperature
    REAL*4  :: tctop ! Cloud top temperature
END TYPE type_rad_FY3

  TYPE (type_rad_FY3)   :: rad
  integer :: iscan,nscan
!
   integer          :: iost
   integer(i_kind), allocatable :: nread(:)

!dongpm   logical hirs2,hirs3,hirs4,msu,amsua,amsub,mhs
   logical mwts,mwhs
   logical outside, outside_all, iuse
   integer :: inst

   integer(i_kind) i,j,k,ifov
   integer(i_kind) nchan
   integer :: num_bufr(7), numbufr, ibufr
   character(20) :: filename

   ! thinning variables
   integer(i_kind) itt,itx,iobs,iout
   real(r_kind) terrain,timedif,crit,dist
   real(r_kind) dlon_earth,dlat_earth

   real(r_kind) tbmin,tbmax, tbbad
   ! real(r_kind) rmask

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

   data tbmin,tbmax,tbbad / 50.0_r_kind, 550.0_r_kind, -9.99e11_r_kind /
   integer :: num_tovs_local, num_tovs_file, num_tovs_global, num_tovs_selected
   integer :: num_tovs_thinned, num_tovs_used, num_tovs_used_tmp
   integer :: lnbufr
   integer :: n
   integer(i_kind), allocatable :: ptotal(:)
   real , allocatable :: in(:), out(:)
   logical :: found, head_found

   if (trace_use) call da_trace_entry("da_read_obs_fy3")

   ! Initialize variables

   nchan = 20
   allocate(nread(1:rtminit_nsensor))
   allocate(ptotal(0:num_fgat_time))
   nread(1:rtminit_nsensor) = 0
   ptotal(0:num_fgat_time) = 0

   ! Set various variables depending on type of data to be read

   ! platform_id  = 1                 !! for NOAA series
   ! platform_id  = 10                !! for METOP series

!dongpm   hirs2=     obstype == 'hirs2'
!dongpm   hirs3=     obstype == 'hirs3'
!dongpm   hirs4=     obstype == 'hirs4'
!dongpm   msu=       obstype == 'msu  '
!dongpm   amsua=     obstype == 'amsua'
!dongpm   amsub=     obstype == 'amsub'
!dongpm   mhs=       obstype == 'mhs  '
          mwts=      obstype == 'mwts '
          mwhs=      obstype == 'mwhs '

!dongpm   if (hirs2) then
!dongpm      sensor_id    =  0
!dongpm      step   = 1.80_r_kind
!dongpm      start  = -49.5_r_kind
!dongpm      nchan=nchan_hirs2
!dongpm      subfgn='NC021021'
!dongpm      rato=1.1363987_r_kind
!dongpm   else if (hirs3) then 
!dongpm      sensor_id    =  0
!dongpm      step   = 1.80_r_kind
!dongpm      start  = -49.5_r_kind
!dongpm      nchan=nchan_hirs3
!dongpm      subfgn='NC021025'
!dongpm   else if (hirs4) then 
!dongpm      sensor_id    =  0
!dongpm      step   = 1.80_r_kind
!dongpm      start  = -49.5_r_kind
!dongpm      nchan=nchan_hirs4
!dongpm      subfgn='NC021028'
!dongpm   else if (mhs) then 
!dongpm      sensor_id    =  15
!dongpm      step   = 10.0_r_kind/9.0_r_kind
!dongpm      start  = -445.0_r_kind/9.0_r_kind
!dongpm      nchan=nchan_mhs
!dongpm      subfgn='NC021027'
!dongpm   else if (msu) then
!dongpm      sensor_id    =  1
!dongpm      step   = 9.474_r_kind
!dongpm      start  = -47.37_r_kind
!dongpm      nchan=nchan_msu
!dongpm      subfgn='NC021022'
!dongpm      rato=1.1363987_r_kind
!dongpm   else if (amsua) then
!dongpm      sensor_id    =  3
!dongpm      step   = three + one/three
!dongpm      start  = -48.33_r_kind
!dongpm      nchan=nchan_amsua
!dongpm      subfgn='NC021023'
!dongpm   else if (amsub)  then
!dongpm      sensor_id    =  4
!dongpm      step   = 1.1_r_kind
!dongpm      start  = -48.95_r_kind
!dongpm      nchan=nchan_amsub
!dongpm      subfgn='NC021024'
!dongpm   end if
          if (mwts) then
           sensor_id    =  40
           nchan=4
           nscan=15
          else if(mwhs) then
           sensor_id    =  41
           nchan=5
           nscan=98
          endif

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
         write(filename,fmt='(A,2I1,A)') trim(infile),0,i,'.dat'
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
      filename=trim(infile)//'.dat'
   else
      if ((numbufr ==1) .and. (num_bufr(ibufr) == 0)) then
         filename=trim(infile)//'.dat'
      else
         write(filename,fmt='(A,2I1,A)') trim(infile),0,num_bufr(ibufr),'.dat'   
      end if
   end if

!  We want to use specific unit number for bufr data, so we can control the endian format in environment. 
   lnbufr = 99

   open(unit=lnbufr,file=trim(filename),form='unformatted', &
      iostat = iost, status = 'old')
   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open file "//filename/))
      if (trace_use) call da_trace_exit("da_read_obs_fy3")
      return
   end if


   if ( ibufr == 1 ) then
      allocate (head)
      !  allocate ( head % tb_inv (1:nchan) )
      nullify  ( head % next )
      p => head
   endif

   if (tovs_start > 1) then
      write (unit=stdout,fmt='(A,I6)') "   Skipping tovs obs before", tovs_start
   end if

   obs: do while (.true.)
        do iscan=1,nscan

         ! 1.0     Read fy3 data
         read(lnbufr,end=1000) rad

         num_tovs_file = num_tovs_file + 1

         ! 2.0     Extract observation location and other required information
         !     QC1:  judge if data is in the domain, read next record if not
         !------------------------------------------------------------------------
         ! rlat = bfr1bhdr(bufr_lat)
         ! rlon = bfr1bhdr(bufr_lat)
         ! if (rlon < 0.0) rlon = rlon+360.0

         info%lat  =  rad%rlat

         info%lon  =  rad%rlon
         call da_llxy (info, loc, outside, outside_all)

         if (outside_all) cycle

         !  3.0     Extract other information
         !------------------------------------------------------
         !  3.1     Extract satellite id and scan position. 
   
         platform_id = 23
         if(infile(5:5)=='a') then
            satellite_id = 1
         elseif(infile(5:5)=='b') then
            satellite_id = 2
         else
            call da_warning(__FILE__,__LINE__,(/"Can not assimilate data from this instrument"/))
            if (trace_use) call da_trace_exit("da_read_obs_fy3")
            return
         endif
         ifov = rad%iscanpos    

         !  QC2:  limb pixel rejected (not implemented)

         !  3.2     Extract date information.
    
         year   = rad%yyyy   
         month  = rad%mn  
         day    = rad%dd    
         hour   = rad%hh   
         minute = rad%mm 
         second = rad%ss 
!dongpm for test
!          year   = 2008
!          month  = 8
!          day    = 5
!          hour   = 18
!          minute = 0
!          second = 0     
         
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
   
            satzen = rad%satzen !*deg2rad   ! local zenith angle
            satzen = abs(satzen)
 
            ! if (amsua .and. ifov .le. 15) satzen=-satzen
            ! if (amsub .and. ifov .le. 45) satzen=-satzen
            ! if (hirs3 .and. ifov .le. 28) satzen=-satzen
!dongpm         if ( satzen > 65.0 ) cycle   ! CRTM has a satzen > 65.0 check
         satazi = rad%satazi*0.01           ! look angle
         ! if (satazi<0.0) satazi = satazi+360.0
         solzen = rad%solzen*0.01              ! solar zenith angle
         solazi = rad%solazi*0.01              !RTTOV9_3

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
!dongpm            terrain = 0.01_r_kind*abs(bfr1bhdr(13))
            terrain = 0.01_r_kind*abs(rad%satzen)
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
         srf_height = rad%isurf_height          ! station height
         if (srf_height < 8888.0 .AND. srf_height > -416.0) then
         else
            srf_height = 0.0
         endif  

!dongpm         landsea_mask = rad%isurf_type  ! 0:land ; 1:sea (same as RTTOV)
!fy3 isurf_type is just reversed as RTTOV
         if(rad%isurf_type .eq. 0) then   ! sea
           landsea_mask = 1
         elseif(rad%isurf_type .eq. 1) then   !coast 
           landsea_mask = 0
         elseif(rad%isurf_type .eq. 2) then   !land
           landsea_mask = 0
         else
           landsea_mask = rad%isurf_type
           write(unit=message(1),fmt='(A,I6)') 'Unknown surface type: ', landsea_mask
           call da_warning(__FILE__,__LINE__,message(1:1))
         endif
         ! rmask=one                          ! land
         ! if (nint(bfr1bhdr(bufr_landsea_mask))==1) rmask=0.0_r_kind   ! reverse the land/sea mask in bufr
         ! landsea_mask = rmask+.001_r_kind             ! land sea mask

         info%elv = srf_height

         ! 3.6 extract channel bright temperature
   
         tb_inv(1:nchan) = rad%tbb(1:nchan)
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
1000  continue
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

   if (trace_use) call da_trace_exit("da_read_obs_fy3")

  

end subroutine da_read_obs_fy3


