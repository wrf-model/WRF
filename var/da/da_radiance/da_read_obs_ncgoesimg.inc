subroutine  da_read_obs_ncgoesimg (iv,infile)
!----------------------------------------------------
! Purpose:  read netcdf format goes imager GVAR data
!           and convert to brightness temperature
!
! Method: Yang et al., 2017: Impact of assimilating GOES imager clear-sky radiance 
!         with a rapid refresh assimilation system for convection-permitting forecast
!         over Mexico. J. Geophys. Res. Atmos., 122, 5472–5490
!----------------------------------------------------
   implicit none

   character(100),   intent (in) :: infile
   type (iv_type),intent (inout) :: iv

   real(kind=8)                  ::  obs_time

   type(datalink_type), pointer  :: head, p, current, prev
   type(info_type)               :: info
   type(model_loc_type)          :: loc

! Number of channels for sensors in NETCDF

   integer(i_kind),parameter    :: nchan = 4
   integer(i_kind)              :: inst,platform_id,satellite_id,sensor_id
   integer(i_kind), allocatable :: ptotal(:)
   real(r_kind)                 :: crit
   integer(i_kind)              :: ifgat, iout, iobs
   logical                      :: outside, outside_all, iuse

! Variables for netcdf IO
   logical                      :: found, head_found, isfile
   integer(i_kind)              :: iret,ireadsb,ireadmg,irec,isub,next
   character(10)                :: date

! Work variables for time
   integer(i_kind)              :: idate, im, iy, idd, ihh
   integer(i_kind)              :: idate5(6)

! Other work variables

   real(r_kind)     :: dlon_earth,dlat_earth,dlon_earth_deg,dlat_earth_deg
   real(r_kind)     :: timedif, pred, crit1, dist1
   real(r_kind),allocatable  :: data_all(:)

   real,allocatable          :: sat_zen(:,:)
   real,allocatable          :: lon(:,:),lat(:,:)

   real,allocatable          :: ch_tmp(:,:,:)
   integer                   :: bands
   integer                   :: crdate,crtime,dd

   real,allocatable          :: bt(:,:,:)
   integer(i_kind)           :: doy,mlen(12),mday(12),mon,mm

   integer(i_kind)  :: iscan
   integer(i_kind)  :: num_goesimg_file
   integer(i_kind)  :: num_goesimg_local, num_goesimg_global, num_goesimg_used, num_goesimg_thinned
   integer(i_kind)  :: num_goesimg_used_tmp
   integer(i_kind)  :: i, j, l,nchannel,ichan, iskip, ngoes
   integer(i_kind)  :: itx, k, nele, itt, n
   integer(i_kind)  :: error_status
   character(20)    ::filename
   real, allocatable :: in(:), out(:)

! Set standard parameters
   integer          :: start(1:20)       ! Array of dimension starts.

! use start for ir sensors.
   character(len=512)    :: input_file
   integer               :: length,ndims     ! Filename length.
   integer               :: rcode,io_status,ii,jj           ! NETCDF return code.
   integer               :: cdfid,varid,ivtype              ! NETCDF file IDs.
   character(len=20)     :: var2d(6)
   integer, dimension(3) :: dims ,dimids
   integer               :: natts

!------------------convert GVAR to BT-----------------------------
   real             :: tff
   real,parameter   :: c1 = 1.191066e-05, c2 = 1.438833
   real             :: m(4),b(4),va(4),vb(4),aa(4),ab(4), &
                       ba(4),bb(4),ra(4),rb(4)
   real             :: va_14(4),vb_14(4),aa_14(4),ab_14(4), &
                       ba_14(4),bb_14(4),ra_14(4),rb_14(4)
   real             :: va_13(4),vb_13(4),aa_13(4),ab_13(4), &
                       ba_13(4),bb_13(4),ra_13(4),rb_13(4)
   real             :: va_15(4),vb_15(4),aa_15(4),ab_15(4), &
                       ba_15(4),bb_15(4),ra_15(4),rb_15(4)
   data  m  /  227.3889,   38.8383,     5.2285,     5.5297/
   data  b  /   68.2167,   29.1287,    15.6854,    16.5892/

!-------------------goes-14 calibration coefficient----------------------------------
   data va_14 / 2577.3518, 1519.3488,  933.98541,  752.88143/
   data vb_14 / 2577.3518, 1518.5610,  934.19579,  752.82392/
   data aa_14  /-1.5565294,-3.9656363,-0.50944128,-0.16549136/
   data ab_14  /-1.5565294,-3.9615475,-0.51352435,-0.16396181/
   data ba_14  / 1.0027731, 1.0133248,  1.0029188,  1.0001953/
   data bb_14  / 1.0027731, 1.0135737,  1.0027813,  1.0002291/
   data ra_14  /-4.0683469e-07,-7.5834376e-06,-3.3213255e-06,9.0998038e-07/
   data rb_14  /-4.0683469e-07,-7.9139638e-06,-2.9825801e-06,8.1000947e-07/
!-------------------goes-13 calibration coefficient----------------------------------
   data va_13  / 2561.7421, 1522.5182, 937.23449, 751.92589/
   data vb_13  / 2561.7421, 1521.6645, 937.27498, 751.92589/
   data aa_13  /-1.4755462, -4.1556932, -0.52227011, -0.16048355/
   data ab_13  /-1.4755462, -4.1411143, -0.51783545, -0.16048355/
   data ba_13  / 1.0025686, 1.0142082, 1.0023802, 1.0006854/
   data bb_13  / 1.0028656, 1.0142255, 1.0023789, 1.0006854/
   data ra_13  /-5.8203946e-07, -8.0255086e-06, -2.0798856e-06, -3.9399190e-07/
   data rb_13  /-5.8203946e-07, -8.0755893e-06, -2.1027609e-06, -3.9399190e-07/
!-------------------goes-15 calibration coefficient----------------------------------
   data va_15  / 2562.7905, 1521.1988, 935.89417, 753.72229/
   data vb_15  / 2562.7905, 1521.5277, 935.78158, 753.93403/
   data aa_15  /-1.5903939, -3.9614910, -0.51798741, -0.16676232/
   data ab_15  /-1.5903939, -3.9580619, -0.51371643, -0.17147513/
   data ba_15  / 1.0026700, 1.0132094, 1.0025141, 1.0002673/
   data bb_15  / 1.0026700, 1.0130975, 1.0025320, 1.0001237/
   data ra_15  /-3.1926330e-07, -7.4310172e-06, -2.3893260e-06, 7.3287547e-07/
   data rb_15  /-3.1926330e-07, -7.3039584e-06, -2.4517009e-06, 1.1424349e-06/

   data  mlen /31,28,31,30,31,30,31,31,30,31,30,31/ ! days of each month

   if (trace_use) call da_trace_entry("da_read_obs_ncgoesimg")

!  0.0  Initialize variables
!-----------------------------------
   platform_id  = 4   ! GOES series
   sensor_id    = 22  ! imager
   ifgat        = 1
   nchannel     = 4   ! # of channels

   if(infile(6:7)=='13') then
      satellite_id = 13
   elseif(infile(6:7)=='14') then
      satellite_id = 14
   elseif(infile(6:7)=='15') then
      satellite_id = 15
   else
      write(*,*) 'goes satellite ', satellite_id, ' is not supported'
      return
   endif

! determine if sensor triplet is in the sensor list 
!-----------------------------------------------------
   inst = 0
   do ngoes = 1, rtminit_nsensor
      if (platform_id  == rtminit_platform(ngoes) &
         .and. sensor_id == rtminit_sensor(ngoes) &
         .and. satellite_id == rtminit_satid(ngoes)) then
         inst = ngoes
      else
         cycle
      end if
   end do
   if (inst == 0) then
      write(unit=message(1),fmt='(A,I2,A)') " goes-",satellite_id,"-imager is not in sensor list"
      call da_warning(__FILE__,__LINE__, message(1:1))
      return
   end if

! assign corresponding calibration coeffs
!-------------------------------------------
   if (satellite_id==13) then  !only use the first detector's parameters
      va=va_13
      aa=aa_13
      ba=ba_13
      ra=ra_13
   end if
   if (satellite_id==14) then
      va=va_14
      aa=aa_14
      ba=ba_14
      ra=ra_14
   end if
   if (satellite_id==15) then
      va=va_15
      aa=aa_15
      ba=ba_15
      ra=ra_15
   end if 

   allocate(ptotal(0:num_fgat_time))
   ptotal(0:num_fgat_time) = 0
   iobs = 0                 ! for thinning, argument is inout

! 1.0 Read NC files: sepeate file for different channels
!-------------------------------------------------------

! variables to read from NC file
!--------------------------------
      var2d(1)='imageDate'
      var2d(2)='imageTime'
      var2d(3)='lat'
      var2d(4)='lon'
      var2d(5)='bands'
      var2d(6)='data'

! determine file existant and open file
!----------------------------------------
      write(input_file,fmt='(A,A,I2.2,A)') trim(infile),'-',01,'.nc'
      write(*,*) "Reading dimension of ", trim(input_file)
      inquire(file=trim(input_file), exist=isfile)
      if ( isfile ) then
         length = len_trim(input_file)
         rcode = nf_open( input_file(1:length), NF_NOWRITE, cdfid)
      else
         write(unit=message(1),fmt='(A,I2,A)') " goes-",satellite_id,"-imager-01.nc not exist"
         call da_warning(__FILE__,__LINE__, message(1:1))
         return
      end if

         io_status = nf_inq_varid(cdfid, trim(var2d(6)), varid)
         rcode     = nf_inq_var  (cdfid, varid, trim(var2d(6)), ivtype, ndims, dimids, natts)

         do i = 1, ndims
            rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
         end do

         rcode = nf_close(cdfid)

         write(*,*) "ndims dims for lat lon ",ndims, dims

         allocate(  bt(nchan,dims(1), dims(2)))
         allocate(       lat(dims(1), dims(2)))
         allocate(       lon(dims(1), dims(2)))
         allocate(   sat_zen(dims(1), dims(2)))
         allocate(  ch_tmp(dims(1), dims(2), dims(3)) )

   ncfile_loop:  do ichan=1,nchannel  ! Channel loop
!-------------------------------------------------------

      num_goesimg_file    = 0
      num_goesimg_local   = 0
      num_goesimg_global  = 0
      num_goesimg_used    = 0
      num_goesimg_thinned = 0

! determine file existant and open file
!----------------------------------------
      write(input_file,fmt='(A,A,I2.2,A)') trim(infile),'-',ichan,'.nc'
      write(*,*) "Reading ", trim(input_file)
      inquire(file=trim(input_file), exist=isfile)
      if ( isfile ) then
         length = len_trim(input_file)
         rcode = nf_open( input_file(1:length), NF_NOWRITE, cdfid)
      end if

         do n=1,4  ! read the first 4 variables
        !----------------------------------------
            start     = 1
            io_status = nf_inq_varid(cdfid, trim(var2d(n)), varid)
            if (io_status /= 0 ) then
               write(unit=message(1),fmt='(a,a,a)') "Variable ",trim(var2d(n)), " does not exist"
               call da_warning(__FILE__,__LINE__, message(1:1))
               cycle
            endif
            rcode = nf_inq_var( cdfid, varid, trim(var2d(n)), ivtype, ndims, dimids, natts )

            do i = 1, ndims
               rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
            end do

            select case(trim(var2d(n)))
            case ('imageDate')
               rcode = nf_get_vara_int( cdfid, varid, start, dims, crdate)
            case ('imageTime')
               rcode = nf_get_vara_int( cdfid, varid, start, dims, crtime)
            case ('lat')
               rcode = nf_get_vara_double( cdfid, varid, start, dims, lat)
            case ('lon')
               rcode = nf_get_vara_double( cdfid, varid, start, dims, lon)
            end select
         end do  ! end reading of the first 4 variables
        !--------------------------------------------------

        ! determine ccyymmddhh
        !---------------------------
         idate5(1)= int(crdate/1000)
         dd = mod(crdate,1000)
         if ( mod(idate5(1),4) == 0 ) then
            mlen(2) = 29
            if ( MOD(idate5(1),100) == 0 ) then
               mlen(2) = 28
            endif
            if ( MOD(idate5(1),400) == 0 ) then
               mlen(2) = 29
            endif
         endif
         mm = 0
         do mon=1,12
            mday(mon) = mm
            mm = mm + mlen(mon)
         end do

         do mon= 2,12
            if (dd.gt.mday(mon-1).and.dd.le.mday(mon))then
               idate5(2) = mon-1
               idate5(3) = dd - mday(mon-1)
            end if
         end do

         idate5(4)=int(crtime/10000)
         idate5(5)=int(crtime/100)-int(crtime/10000)*100
         idate5(6)=crtime-int(crtime/100)*100

!------------- calculate satelize zenith angle -----------------------
         sat_zen=missing_r
         do jj=1,dims(2)
         do ii=1,dims(1)
            call da_get_satzen(lat(ii,jj),lon(ii,jj),satellite_id,sat_zen(ii,jj))
            if(sat_zen(ii,jj) > 75.0) sat_zen(ii,jj)=missing_r
         end do
         end do

!---------------------------------------------------------------------------
      do n=5,6  ! read bands/data

         io_status = nf_inq_varid(cdfid, trim(var2d(n)), varid)
         if (io_status /= 0 ) then
            write(unit=message(1),fmt='(a,a,a)') "Variable ",trim(var2d(n)), " does not exist"
            call da_warning(__FILE__,__LINE__, message(1:1))
            cycle
         endif
         rcode = nf_inq_var( cdfid, varid, trim(var2d(n)), ivtype, ndims, dimids, natts )
         dims = 0

         do i = 1, ndims
            rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
         end do

         select case(trim(var2d(n)))
         case ('data')
            rcode = nf_get_vara_double( cdfid, varid, start, dims, ch_tmp)
         case ('bands')
            rcode = nf_get_vara_int2( cdfid, varid, start, dims, bands)
         end select
      end do

      if (bands==6) bands=5  ! crtm coeffs index?

!------------------------convert to BT-----------------------

      do jj=1,dims(2)
      do ii=1,dims(1)
         ch_tmp(ii,jj,1) = ch_tmp(ii,jj,1)/32.0  !16 bit to 10 bit
         tff  = (ch_tmp(ii,jj,1)-b(bands-1))/m(bands-1)

         if ( (tff .gt. 0.0) .and. (lat(ii,jj).lt.100.0) ) then
            tff = c2*va(bands-1)/alog(1+c1*va(bands-1)**3/tff)
            tff = aa(bands-1)+ba(bands-1)*tff+ra(bands-1)*tff**2
            bt(ichan,ii,jj) = tff
         else 
            bt(ichan,ii,jj) = missing_r
         end if
      end do
      end do

      rcode = nf_close(cdfid)

   end do ncfile_loop ! end channel loop
!---------------------------------

   allocate (head)
   nullify  (head % next )
   p => head

   allocate(data_all(nchan))

! 2.0 loop to read data file
!----------------------------------------------
   do jj=1,dims(2)
   do ii=1,dims(1)

         num_goesimg_file = num_goesimg_file + 1
         info%lon  =  lon(ii,jj)  ! longitude
         info%lat  =  lat(ii,jj)  ! latitude
         call da_llxy (info, loc, outside, outside_all)
         if (outside_all) cycle
         if (inst == 0) cycle
         if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
             idate5(2) < 1    .or. idate5(2) >   12 .or. &
             idate5(3) < 1    .or. idate5(3) >   31 .or. &
             idate5(4) <0     .or. idate5(4) >   24 .or. &
             idate5(5) <0     .or. idate5(5) >   60 )then
            write(6,*)'READ_goeimg:  ### ERROR IN READING ', 'goeimg', ' BUFR DATA:', &
                ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
            exit
         end if
         call da_get_julian_time(idate5(1),idate5(2),idate5(3),idate5(4),idate5(5),obs_time)
         if ( obs_time < time_slots(0) .or.  &
            obs_time >= time_slots(num_fgat_time) ) cycle
         do ifgat=1,num_fgat_time
            if ( obs_time >= time_slots(ifgat-1) .and.  &
                  obs_time  < time_slots(ifgat) ) exit
         end do
         num_goesimg_global = num_goesimg_global + 1
         ptotal(ifgat) = ptotal(ifgat) + 1
         if (outside)  cycle
         num_goesimg_local = num_goesimg_local + 1
         write(unit=info%date_char, &
            fmt='(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
            idate5(1), '-', idate5(2), '-', idate5(3), '_', idate5(4), &
            ':', idate5(5), ':', idate5(6)
            info%elv = 0.0  !aquaspot%selv

! 3.0 Map obs to thinning grid
!-------------------------------------------------------------------
         if (thinning) then
            dlat_earth = info%lat
            dlon_earth = info%lon
            if (dlon_earth<zero) dlon_earth = dlon_earth+r360
            if (dlon_earth>=r360) dlon_earth = dlon_earth-r360
            dlat_earth = dlat_earth*deg2rad
            dlon_earth = dlon_earth*deg2rad
            timedif = 0.
            crit = 1.
            call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
            if (.not. iuse) then
               num_goesimg_thinned=num_goesimg_thinned+1
               cycle
            end if
         end if

         do l=1,nchan
            data_all(l)=bt(l,ii,jj)
         end do

         num_goesimg_used = num_goesimg_used + 1

!  4.0   assign information to sequential radiance structure
!--------------------------------------------------------------------------
         allocate ( p % tb_inv (1:nchan) )
         p%info                  = info
         p%loc                   = loc
         p%landsea_mask          = 1  ! ???
         p%scanpos               = ii ! ??? "scan" position
         p%satzen                = sat_zen(ii,jj)
         p%solzen                = 0.0
         p%tb_inv(1:nchan)       = data_all(1:nchan)
         p%sensor_index          = inst
         p%ifgat                 = ifgat
         allocate (p%next)   ! add next data
         p => p%next
         nullify (p%next)

   end do
   end do

   deallocate(data_all) ! Deallocate data arrays
   deallocate(bt)
   deallocate(sat_zen)
   deallocate(lat)
   deallocate(lon)
   deallocate(ch_tmp)

!------------------------------------------------------

   if (thinning .and. num_goesimg_global > 0 ) then
#ifdef DM_PARALLEL

      ! Get minimum crit and associated processor index.
      j = 0
      do ifgat = 1, num_fgat_time
            j = j + thinning_grid(inst,ifgat)%itxmax
      end do


      allocate ( in  (j) )
      allocate ( out (j) )
      j = 0
      do ifgat = 1, num_fgat_time
            do i = 1, thinning_grid(inst,ifgat)%itxmax
               j = j + 1
               in(j) = thinning_grid(inst,ifgat)%score_crit(i)
            end do
      end do

      call mpi_reduce(in, out, j, true_mpi_real, mpi_min, root, comm, ierr)

      call wrf_dm_bcast_real (out, j)

      j = 0
      do ifgat = 1, num_fgat_time
            do i = 1, thinning_grid(inst,ifgat)%itxmax
               j = j + 1
               if ( ABS(out(j)-thinning_grid(inst,ifgat)%score_crit(i)) > 1.0E-10 ) thinning_grid(inst,ifgat)%ibest_obs(i) = 0
            end do
      end do
      deallocate( in  )
      deallocate( out )

#endif
      ! Delete the nodes which being thinning out
      p => head
      prev => head
      head_found = .false.
      num_goesimg_used_tmp = num_goesimg_used

      do j = 1, num_goesimg_used_tmp
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
      !      deallocate ( current % cloud_flag )
            deallocate ( current )
            num_goesimg_thinned = num_goesimg_thinned + 1
            num_goesimg_used = num_goesimg_used - 1
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
!stop
   iv%total_rad_pixel   = iv%total_rad_pixel + num_goesimg_used
   iv%total_rad_channel = iv%total_rad_channel + num_goesimg_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_goesimg_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_goesimg_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'num_goesimg_file, num_goesimg_global, num_goesimg_local, num_goesimg_used, num_goesimg_thinned'
   write(stdout,*) num_goesimg_file, num_goesimg_global, num_goesimg_local, num_goesimg_used, num_goesimg_thinned


   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------


   if (num_goesimg_used > 0) then
      iv%instid(inst)%num_rad  = num_goesimg_used
      iv%instid(inst)%info%nlocal = num_goesimg_used
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      call da_allocate_rad_iv (inst, nchan, iv)
   end if

   !  6.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   p => head
   do n = 1, num_goesimg_used
      i = p%sensor_index
      call da_initialize_rad_iv (i, n, iv, p)
      current => p
      p => p%next

      ! free current data
      deallocate ( current % tb_inv )
!      deallocate ( current % cloud_flag )
      deallocate ( current )
   end do
   deallocate ( p )
   deallocate (ptotal)

   if (trace_use) call da_trace_exit("da_read_obs_ncgoesimg")

end subroutine da_read_obs_ncgoesimg
