subroutine da_read_obs_bufrgpsro_eph (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Read NCEP GPSRO BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------
   !   METHOD: use F90 sequantial data structure to avoid reading the file twice
   !            1. read gpsro data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !                  and deallocate sequential data structure
   !
   !  HISTORY: 2009/12/17  - F90 sequantial structure  Peng Xiu
   !           Nov. 2015   - added the option for the assimilation of
   !                         gps excess phase which requires the input data
   !                         as profiles
   !
   !----------------------------------------------------------------------------

   use da_control

   implicit none

   type (iv_type),             intent(inout) :: iv

#ifdef BUFR

   real,    parameter   :: r8bfms = 9.0D08  ! BUFR missing value threshold
   integer, parameter   :: maxlevs = 500
   integer              :: iunit, iost, idate, iret, nlev1, nlev2,k,i, nlev_bad
   integer              :: num_report, num_outside_all, num_outside_time
   integer              :: iyear,imonth,iday,ihour,imin
   integer              :: ntotal, nlocal, nlev, ref_qc, iobs
   real*8               :: obs_time
   real*8               :: hdr(12)
   real                 :: ntotal_ifgat(0:num_fgat_time)
   real*8               :: rdata1(25,maxlevs), rdata2(25,maxlevs)
   real                 :: height, ref_data, ref_error
   real                 :: azim
   character(len=8)     :: subset
   character(len=80)    :: hdstr
   logical              :: outside, outside_all
   type(info_type)      :: info !for input to llxy
   type(model_loc_type) :: loc  !for output from llxy
   character(len=5)     :: id
   character(len=19)    :: date_char
   character(len=12)    :: platform_name
   integer              :: ifgat, kk
   integer              :: err_opt   ! 1: WRF-Var/obsproc, 2: GSI
   real                 :: erh90, erh0, err90, err0
   integer(i_kind)      :: ireadns
   type datalink_gpsro  !for gpsro data reading
       type (info_type)         :: info
       type (model_loc_type)    :: loc
       type (gpsref_type)       :: gpsref
       integer                  :: ifgat
       real                     :: rfict   !to be used for gpseph
       real, allocatable        :: azim(:) !to be used for gpseph
       real, allocatable        :: lat(:)  !to be used for gpseph
       real, allocatable        :: lon(:)  !to be used for gpseph
       integer                  :: obs_global_index
       type(datalink_gpsro), pointer :: next
   end type datalink_gpsro
   type(datalink_gpsro),pointer  :: head=>null(), plink=>null()
   type(ob_in_mean_h)   :: pseudo_ob
   integer              :: iprof
   integer              :: obs_index
   integer              :: lowest_level
   integer,parameter    :: nfile_max = 8
   integer              :: nfile, ifile, max_lev
   character(80)        :: fname(nfile_max)
   character(80)        :: infile_prefix, fname_tmp
   logical              :: fexist
   logical              :: treat_as_profile

   if (trace_use) call da_trace_entry("da_read_obs_bufrgpsro_eph")

   treat_as_profile = .false.
   platform_name    = 'FM-116 GPSRF'
   if ( use_gpsephobs ) then
      treat_as_profile = .true.
      platform_name    = 'FM-118 GPSEP'
   end if

   max_lev = 1
   infile_prefix = 'gpsro'
   iprof  = 0
   nlocal = 0
   ntotal = 0
   num_report       = 0
   num_outside_all  = 0
   num_outside_time = 0
   ntotal_ifgat(0:num_fgat_time)=0

   ! checking for input files
   nfile    = 0  !initialize
   fname(:) = '' !initialize
   ! first check if gpsro.bufr is available
   fname_tmp = trim(infile_prefix)//'.bufr'
   inquire (file=fname_tmp, exist=fexist)
   if ( fexist ) then
      nfile = 1
      fname(nfile)  = fname_tmp
   else
      ! check if gpsro-0x.bufr is available for multiple input files
      ! here 0x is the input file sequence number
      ! do not confuse it with fgat time slot index
      do i = 1, nfile_max
         write(fname_tmp,fmt='(A,A,I2.2,A)') trim(infile_prefix),'-',i,'.bufr'
         inquire (file=fname_tmp, exist=fexist)
         if ( fexist ) then
            nfile = nfile + 1
            fname(nfile)  = fname_tmp
         else
            exit
         end if
      end do
   end if

   if ( nfile == 0 ) then
      call da_warning(__FILE__,__LINE__, &
         (/"No valid NCEP BUFR gpsro.bufr or gpsro-01.bufr file found."/))
      if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro_eph")
      return
   end if

bufrfile:  do ifile = 1, nfile

   ! Use specified unit number, so we can control its endian format in environment.
   iunit = 96
   call closbf(iunit)
   open(unit=iunit, file=trim(fname(ifile)), &
      iostat=iost, form='unformatted', status='OLD')
   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iost," opening PREPBUFR obs file "//trim(fname(ifile))
      call da_warning(__FILE__,__LINE__,message(1:1))
      if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro_eph")
      return
   end if

   !--------------------------------
   ! open bufr file then check date
   !--------------------------------
   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readmg(iunit,subset,idate,iret)
   if ( iret /= 0 ) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iret," reading GPSRO BUFR obs file "//trim(fname(ifile))
      call da_warning(__FILE__,__LINE__,message(1:1))
      call closbf(iunit)
      close(iunit)
      if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro_eph")
      return
   end if
   write(unit=message(1),fmt='(a,i10)') 'GPSRO BUFR file date is: ', idate
   call da_message(message(1:1))
   rewind(iunit)

   hdstr = 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU CLATH CLONH'

   reports: do while ( ireadns(iunit,subset,idate) == 0 )

      num_report = num_report + 1

      call ufbint(iunit,hdr,12,1,iret,hdstr)

      iyear  = int(hdr(1))
      imonth = int(hdr(2))
      iday   = int(hdr(3))
      ihour  = int(hdr(4))
      imin   = int(hdr(5))

      write(id, '(i3.3,i2.2)') int(hdr(8)), int(hdr(9)) ! construct id using SAID and PTID
      write(date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
         iyear, '-', imonth, '-', iday, '_', ihour, ':', imin, ':', 0

      ! check date
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if (obs_time < time_slots(0) .or.  &
          obs_time >= time_slots(num_fgat_time)) then
         num_outside_time = num_outside_time + 1
         if ( print_detail_obs ) then
            write(unit=stderr,fmt='(a,1x,i4.4,4i2.2,a)')  &
               info%id(1:5),iyear,imonth,iday,ihour,imin, '  -> outside_time'
         end if
         cycle reports
      end if

      ! determine FGAT index ifgat
      do ifgat=1,num_fgat_time
         if (obs_time >= time_slots(ifgat-1) .and.  &
             obs_time  < time_slots(ifgat)) exit
      end do

      if ( hdr(6) < 100.0 ) then   ! check percentage of confidence PCCF
         cycle reports
      end if

      call ufbseq(iunit,rdata1,25,maxlevs,nlev1,'ROSEQ1')  ! RAOC PROFILE LOCATIONS SEQUENCE
      call ufbseq(iunit,rdata2,25,maxlevs,nlev2,'ROSEQ3')  ! RAOC HEIGHT/REFRACTIVITY SEQUENCE

      if ( nlev1 /= nlev2 ) then
         cycle reports
      end if

      if ( treat_as_profile ) then
         !only one lat/lon (perigee point at occultation point)
         !for each report that contains multiple levels of data
         info%lat  = hdr(11)
         info%lon  = hdr(12)
         ! gpsro.bufr contains missing longitude, occasionally
         if ( info%lat > r8bfms .or. info%lon > r8bfms ) then
            cycle reports
         end if
         ! check loc
         info%lat = max(info%lat, -89.95)
         info%lat = min(info%lat,  89.95)
         call da_llxy(info, loc, outside, outside_all)
         if ( outside_all ) then
            num_outside_all = num_outside_all + 1
            if ( print_detail_obs ) then
               write(unit=stderr,fmt='(a,2(1x,f8.3),a)')  &
                  id(1:5), info%lat, info%lon, '  -> outside_domain'
            end if
            cycle reports
         end if

         ! check all levels
         nlev_bad = 0
         do k = 1, nlev1
            !info%lat  = rdata1(1,k)
            !info%lon  = rdata1(2,k)
            !azim      = rdata1(3,k)
            !height    = rdata2(1,k)
            !ref_data  = rdata2(2,k)
            if ( abs(rdata1(1,k)) > 90.0 .or. abs(rdata1(2,k)) > 180.0 .or. &
                 abs(rdata1(3,k)) > 180.0 .or. rdata2(1,k) > r8bfms .or. &
                 rdata2(2,k) > r8bfms ) then
               nlev_bad = nlev_bad + 1
            end if
         end do
         if ( nlev_bad == nlev1 ) cycle reports

         ntotal = ntotal + 1
         ntotal_ifgat(ifgat)=ntotal_ifgat(ifgat)+1
         if ( use_gpsephobs .and. gpseph_loadbalance ) then
            if ( myproc /= MOD((ntotal-1), num_procs) ) cycle reports
         else
            if ( outside ) cycle reports
         end if

         nlocal = nlocal + 1

         if (.not. associated(head)) then
             nullify ( head )
             allocate ( head )
             nullify ( head%next )
             plink => head
         else
             allocate ( plink%next )
             plink => plink%next
             nullify ( plink%next )
         end if

         write(info%name, '(a,i6.6,a,a)') 'NCEP_GPSRO_', nlocal, '_', date_char

         nlev = nlev1
         max_lev = max(max_lev, nlev)

         plink%info%name      = info%name
         plink%info%platform  = platform_name
         plink%info%id        = id
         plink%info%date_char = date_char
         plink%info%levels    = nlev
         plink%info%elv       = 0.0            ! not used
         plink%info%lat       = info%lat
         plink%info%lon       = info%lon
         plink%loc%x          = loc%x
         plink%loc%y          = loc%y
         plink%loc%i          = loc%i
         plink%loc%j          = loc%j
         plink%loc%dx         = loc%dx
         plink%loc%dxm        = loc%dxm
         plink%loc%dy         = loc%dy
         plink%loc%dym        = loc%dym

         plink%obs_global_index = ntotal

         plink%ifgat = ifgat
         plink%rfict = hdr(7)*0.001 !ELRC: EARTH LOC RADIUS OF CURVATURE

         allocate (plink%gpsref%h  (1:nlev))
         allocate (plink%gpsref%ref(1:nlev))
         if ( use_gpsephobs ) then
            allocate (plink%azim(1:nlev))
            allocate (plink%lat(1:nlev))
            allocate (plink%lon(1:nlev))
         end if
      end if !treat_as_profile

      iprof = iprof + 1

      lev_loop: do k = 1, nlev1

         info%lat  = rdata1(1,k)
         info%lon  = rdata1(2,k)
         azim      = rdata1(3,k)
         height    = rdata2(1,k)
         ref_data  = rdata2(2,k)
         ref_qc    = 0                ! initialized to be good

         if ( .not. treat_as_profile ) then

            ! gpsro.bufr contains missing longitude, occasionally
            if ( info%lat > r8bfms .or. info%lon > r8bfms ) then
               cycle lev_loop
            end if

            ! check for missing data
            if ( height > r8bfms .or. ref_data > r8bfms ) then
               cycle lev_loop
            end if

            ! check loc
            info%lat = max(info%lat, -89.95)
            info%lat = min(info%lat,  89.95)
            call da_llxy(info, loc, outside, outside_all)
            if ( outside_all ) then
               num_outside_all = num_outside_all + 1
               if ( print_detail_obs ) then
                  write(unit=stderr,fmt='(a,2(1x,f8.3),a)')  &
                     id(1:5), info%lat, info%lon, '  -> outside_domain'
               end if
               cycle lev_loop
            end if
            ntotal = ntotal + 1
            ntotal_ifgat(ifgat)=ntotal_ifgat(ifgat)+1
            if ( outside ) then
               cycle lev_loop
            end if
            nlocal = nlocal + 1
         end if ! not profile

         if ( treat_as_profile ) then
            plink%azim(k) = azim
            plink%lat(k)  = missing_r
            plink%lon(k)  = missing_r
            plink%gpsref%h(k)         = missing_r
            plink%gpsref%ref(k)%inv   = missing_r
            plink%gpsref%ref(k)%qc    = missing_data
            plink%gpsref%ref(k)%error = xmiss
            if ( use_gpsephobs ) then
               if ( info%lat < r8bfms .and. info%lon < r8bfms .and. &
                    azim < r8bfms ) then
                  plink%azim(k) = azim
                  if ( gpsro_drift == 0 ) then
                     plink%lat(k)  = plink%info%lat
                     plink%lon(k)  = plink%info%lon
                  else
                     plink%lat(k)  = info%lat
                     plink%lon(k)  = info%lon
                  end if
               end if
            end if
            if ( height < r8bfms .and. ref_data < r8bfms ) then
               plink%gpsref%h(k)         = height
               plink%gpsref%ref(k)%inv   = ref_data
               plink%gpsref%ref(k)%qc    = ref_qc
               plink%gpsref%ref(k)%error = ref_error
            end if
         end if !treat_as_profile

         if ( ref_data < r8bfms ) then ! non-missing data
            ref_error = ref_data * 0.01
         end if

         if ( height < r8bfms .and. info%lat < r8bfms ) then ! non-missing height

            ! check height, only keep data below certain height (default is 30km)
            if ( height > top_km_gpsro*1000.0 .or. &
                 height < bot_km_gpsro*1000.0 ) then
               ref_qc = -77
            end if

            err_opt = 1
            ! observation errors  WRF-Var/obsproc
            if ( err_opt == 1 ) then
               if ( height >= 12000.0 ) then
                  ref_error = ref_error * 0.3
               else
                  erh90 = (0.3-1.5)*(height-12000.0)/(12000.0-0.0) + 0.3
                  if ( height >= 5500.0 ) then
                     erh0 = (0.3-1.3)*(height-12000.0)/(12000.0-5500.0) + 0.3
                  else if ( height >= 2500.0) then
                     erh0 = (1.3-2.5)*(height-5500.0)/(5500.0-2500.0) + 1.3
                  else
                     erh0 = 2.5
                  end if
                  err90 = ref_error * erh90
                  err0  = ref_error * erh0
                  ref_error = err90 - (1.0-abs(info%lat)/90.0)*(err90-err0)
               end if
            end if

            ! observation errors  GSI_Q1FY09,  Kuo et al. 2003
            if ( err_opt == 2 ) then
               if ( (info%lat >= -30.0) .and. (info%lat <= 30.0) ) then   ! tropics
                  if ( (height >= 7000.0) .and. (height <= 31000.0) ) then
                     ref_error = ref_error*(0.1125+(1.25e-5*height))
                  else if ( height > 31000.0 ) then
                     ref_error = ref_error*0.5
                  else if ( height < 7000.0  ) then
                     ref_error = ref_error*(3.0-(4.0e-4*height))
                  else
                     write(unit=message(1),fmt='(a,f8.1,a,f8.2)') 'unable to process with height = ', &
                        height, ' at lat = ', info%lat
                     call da_error(__FILE__,__LINE__,message(1:1))
                  end if
               else   ! mid-latitudes
                  if ( (height >= 5000.0) .and. (height <= 25000.0) ) then
                     ref_error = ref_error*0.3
                  else if ( (height >= 25000.0) .and. (height <= 31000.0) ) then
                     ref_error = ref_error*(-3.45+(1.5e-4*height))
                  else if ( height > 31000.0 ) then
                     ref_error = ref_error*1.2
                  else if ( height < 5000.0 ) then
                     ref_error = ref_error*(0.75-(9.0e-5*height))
                  else
                     write(unit=message(1),fmt='(a,f8.1,a,f8.2)') 'unable to process with height = ', &
                        height, ' at lat = ', info%lat
                     call da_error(__FILE__,__LINE__,message(1:1))
                  end if
               end if
            end if

         end if ! non-missing height

         !write(info%name, '(a,i6.6,a,a)') 'NCEP_GPSRO_', nlocal, '_', date_char
         write(info%name, '(a,i6.6,a,a)') 'NCEP_GPSRO_', iprof, '_', date_char

         if ( print_detail_obs ) then
            write(unit=stdout,fmt='(a,1x,a,1x,i4.4,4i2.2,2f8.2,f8.1,f8.2,i3,f9.5)')  &
               info%name,id(1:5),iyear,imonth,iday,ihour,imin, &
               info%lat,info%lon,height,ref_data,ref_qc,ref_error
         end if

         if ( .not. treat_as_profile ) then
            if (.not. associated(head)) then
                nullify ( head )
                allocate ( head )
                nullify ( head%next )
                plink => head
            else
                allocate ( plink%next )
                plink => plink%next
                nullify ( plink%next )
            end if

            plink%info%name      = info%name
            plink%info%platform  = platform_name
            plink%info%id        = id
            plink%info%date_char = date_char
            plink%info%levels    = 1              ! each level is treated as separate obs
            plink%info%elv       = 0.0            ! not used
            plink%info%lat       = info%lat
            plink%info%lon       = info%lon
            plink%loc%x          = loc%x
            plink%loc%y          = loc%y
            plink%loc%i          = loc%i
            plink%loc%j          = loc%j
            plink%loc%dx         = loc%dx
            plink%loc%dxm        = loc%dxm
            plink%loc%dy         = loc%dy
            plink%loc%dym        = loc%dym

            plink%obs_global_index = ntotal

            ! 1 level
            allocate (plink%gpsref%h  (1:1))
            allocate (plink%gpsref%ref(1:1))
            plink%gpsref%h(1)         = height
            plink%gpsref%ref(1)%inv   = ref_data
            plink%gpsref%ref(1)%qc    = ref_qc
            plink%gpsref%ref(1)%error = ref_error
            plink%ifgat=ifgat
         end if !not treat_as_profile

      end do lev_loop
   end do reports

   call closbf(iunit)
   close(iunit)

end do bufrfile

   ! assign data in either iv%gpsref or iv%gpseph
   if ( use_gpsephobs ) then
      obs_index = gpseph
      !re-set mex_lev for pseudo_ob on model mean height levels
      max_lev = kde-kds+1
      allocate (pseudo_ob%ref(kds:kde))
      allocate (pseudo_ob%lat(kds:kde))
      allocate (pseudo_ob%lon(kds:kde))
      allocate (pseudo_ob%azim(kds:kde))
   else
      obs_index = gpsref
   end if

   iv%info(obs_index)%max_lev = max_lev
   iv%info(obs_index)%ntotal  = ntotal
   iv%info(obs_index)%nlocal  = nlocal

   !allocate iv%info

   if (iv%info(obs_index)%nlocal > 0) then
      allocate (iv%info(obs_index)%name(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%platform(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%id(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%date_char(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%levels(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%lat(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%lon(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%elv(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%pstar(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%slp(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%pw(iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%x  (kms:kme,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%y  (kms:kme,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%i  (kms:kme,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%j  (kms:kme,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%dx (kms:kme,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%dxm(kms:kme,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%dy (kms:kme,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%dym(kms:kme,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%k  (iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%dz (iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%dzm(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%zk (iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%proc_domain(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%thinned(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
      allocate (iv%info(obs_index)%obs_global_index(iv%info(obs_index)%nlocal))
      iv%info(obs_index)%proc_domain(:,:)  = .false.
      iv%info(obs_index)%thinned(:,:)      = .false.
      iv%info(obs_index)%zk(:,:)           = missing_r
   end if

   if ( use_gpsephobs ) then
      if (iv%info(gpseph)%nlocal > 0) allocate(iv%gpseph(1:iv%info(gpseph)%nlocal))
   else
      if (iv%info(gpsref)%nlocal > 0) allocate(iv%gpsref(1:iv%info(gpsref)%nlocal))
   end if

   ! sort obs and assign to iv structure

   iobs=0
   do kk = 1, num_fgat_time
      plink => head
      iv%info(obs_index)%ptotal(kk)=0

      reports2: do i = 1, iv%info(obs_index)%nlocal

         if ( plink%ifgat /= kk ) then  ! not in current time slot
            plink => plink%next
            cycle reports2
         else

            iobs=iobs+1

            iv%info(obs_index)%name(iobs)      = plink%info%name
            iv%info(obs_index)%platform(iobs)  = plink%info%platform
            iv%info(obs_index)%id(iobs)        = plink%info%id
            iv%info(obs_index)%date_char(iobs) = plink%info%date_char
            iv%info(obs_index)%levels(iobs)    = plink%info%levels
            iv%info(obs_index)%elv(iobs)       = plink%info%elv            ! not used

            iv%info(obs_index)%lat(:,iobs)     = plink%info%lat
            iv%info(obs_index)%lon(:,iobs)     = plink%info%lon
            iv%info(obs_index)%x(:,iobs)       = plink%loc%x
            iv%info(obs_index)%y(:,iobs)       = plink%loc%y
            iv%info(obs_index)%i(:,iobs)       = plink%loc%i
            iv%info(obs_index)%j(:,iobs)       = plink%loc%j
            iv%info(obs_index)%dx(:,iobs)      = plink%loc%dx
            iv%info(obs_index)%dxm(:,iobs)     = plink%loc%dxm
            iv%info(obs_index)%dy(:,iobs)      = plink%loc%dy
            iv%info(obs_index)%dym(:,iobs)     = plink%loc%dym

            iv%info(obs_index)%slp(iobs)%inv   = missing_r
            iv%info(obs_index)%slp(iobs)%qc    = missing_data
            iv%info(obs_index)%slp(iobs)%error = xmiss
            iv%info(obs_index)%pw(iobs)%inv    = missing_r
            iv%info(obs_index)%pw(iobs)%qc     = missing_data
            iv%info(obs_index)%pw(iobs)%error  = xmiss

            iv%info(obs_index)%obs_global_index(iobs) = plink%obs_global_index

            if ( use_gpsephobs ) then
               nlev = plink%info%levels
               !create pseudo_ob on grid mean altitude for gpseph
               call da_gpseph_create_ob(nlev, plink%gpsref%h(1:nlev),       &
                                              plink%gpsref%ref(1:nlev)%inv, &
                                              plink%lat(1:nlev),            &
                                              plink%lon(1:nlev),            &
                                              plink%azim(1:nlev),           &
                                              pseudo_ob, lowest_level)
               if ( lowest_level < 0 ) then
                  ! no valid levels found for creating ob
                  iv%info(gpseph)%levels(iobs) = 1
                  iv%gpseph(iobs)%level1 = 0
                  iv%gpseph(iobs)%level2 = 0
                  iv%gpseph(iobs)%rfict  = plink%rfict
                  !hcl tmp hack: still allocate for 1 level
                  allocate (iv%gpseph(iobs)%h  (1))
                  allocate (iv%gpseph(iobs)%ref(1))
                  allocate (iv%gpseph(iobs)%eph(1))
                  allocate (iv%gpseph(iobs)%lat(1))
                  allocate (iv%gpseph(iobs)%lon(1))
                  allocate (iv%gpseph(iobs)%azim(1))
                  iv%gpseph(iobs)%h(1)         = global_h_mean(1)*1000.0 !km to m
                  iv%gpseph(iobs)%lat(1)       = missing_r
                  iv%gpseph(iobs)%lon(1)       = missing_r
                  iv%gpseph(iobs)%azim(1)      = missing_r
                  iv%gpseph(iobs)%ref(1)%inv   = missing_r
                  iv%gpseph(iobs)%ref(1)%qc    = missing_data
                  iv%gpseph(iobs)%ref(1)%error = xmiss
                  iv%gpseph(iobs)%eph(1)%inv   = missing_r
                  iv%gpseph(iobs)%eph(1)%qc    = missing_data
                  iv%gpseph(iobs)%eph(1)%error = xmiss
               else
                  iv%gpseph(iobs)%level1 = lowest_level+1
                  iv%gpseph(iobs)%level2 = kde-1
                  iv%info(gpseph)%levels(iobs) = max_lev !it is kde-kds+1 for gpseph
                  iv%gpseph(iobs)%rfict  = plink%rfict

                  allocate (iv%gpseph(iobs)%h  (kds:kde))
                  allocate (iv%gpseph(iobs)%ref(kds:kde))
                  allocate (iv%gpseph(iobs)%eph(kds:kde))
                  ! iv%gpseph includes additional azim/lat/lon info of each level
                  allocate (iv%gpseph(iobs)%lat(kds:kde))
                  allocate (iv%gpseph(iobs)%lon(kds:kde))
                  allocate (iv%gpseph(iobs)%azim(kds:kde))
                  do k = kts, kte
                     iv%gpseph(iobs)%h(k)         = global_h_mean(k)*1000.0 !km to m
                     iv%gpseph(iobs)%lat(k)       = pseudo_ob%lat(k)
                     iv%gpseph(iobs)%lon(k)       = pseudo_ob%lon(k)
                     iv%gpseph(iobs)%azim(k)      = pseudo_ob%azim(k)
                     iv%gpseph(iobs)%ref(k)%inv   = pseudo_ob%ref(k)
                     iv%gpseph(iobs)%ref(k)%qc    = missing_data !plink%gpsref%ref(i)%qc
                     iv%gpseph(iobs)%ref(k)%error = xmiss        !plink%gpsref%ref(i)%error
                     iv%gpseph(iobs)%eph(k)%inv   = missing_r
                     iv%gpseph(iobs)%eph(k)%qc    = missing_data
                     iv%gpseph(iobs)%eph(k)%error = xmiss
                  end do
               end if

            else !not use_gpsephobs

               nlev = plink%info%levels
               allocate (iv%gpsref(iobs)%h  (1:nlev))
               allocate (iv%gpsref(iobs)%ref(1:nlev))
               allocate (iv%gpsref(iobs)%p  (1:nlev))
               allocate (iv%gpsref(iobs)%t  (1:nlev))
               allocate (iv%gpsref(iobs)%q  (1:nlev))
               do k = 1, nlev
                  iv%gpsref(iobs)%h(k)         = plink%gpsref%h(k)
                  iv%gpsref(iobs)%ref(k)%inv   = plink%gpsref%ref(k)%inv
                  iv%gpsref(iobs)%ref(k)%qc    = plink%gpsref%ref(k)%qc
                  iv%gpsref(iobs)%ref(k)%error = plink%gpsref%ref(k)%error

                  iv%gpsref(iobs)%p(k)%inv     = missing_r
                  iv%gpsref(iobs)%p(k)%qc      = missing_data
                  iv%gpsref(iobs)%p(k)%error   = xmiss
                  iv%gpsref(iobs)%t(k)%inv     = missing_r
                  iv%gpsref(iobs)%t(k)%qc      = missing_data
                  iv%gpsref(iobs)%t(k)%error   = xmiss
                  iv%gpsref(iobs)%q(k)%inv     = missing_r
                  iv%gpsref(iobs)%q(k)%qc      = missing_data
                  iv%gpsref(iobs)%q(k)%error   = xmiss
               end do

            end if !use_gpsephobs
         end if ! if current time slot
         plink => plink%next
      end do reports2

      ntotal_ifgat(kk)=ntotal_ifgat(kk)+ntotal_ifgat(kk-1)
      iv%info(obs_index)%ptotal(kk)=ntotal_ifgat(kk)
      iv%info(obs_index)%plocal(kk)=iobs
      ! thinning is not applied to GPSRO BUFR, simply make a copy from total number
      iv%info(obs_index)%thin_ptotal(kk)=ntotal_ifgat(kk)
      iv%info(obs_index)%thin_plocal(kk)=iobs
   end do

   write(unit=message(1),fmt='(A,3(1x,i7))') &
      'da_read_obs_bufrgpsro_eph: num_report, num_outside_all, num_outside_time: ', &
      num_report, num_outside_all, num_outside_time
   call da_message(message(1:1))

   if ( iobs /= iv%info(obs_index)%nlocal ) then
      call da_error(__FILE__,__LINE__,(/"numbers mismatch between scanning and reading NCEP GSPRO BUFR file"/))
   end if

   if ( use_gpsephobs ) then
      deallocate (pseudo_ob%ref)
      deallocate (pseudo_ob%lat)
      deallocate (pseudo_ob%lon)
      deallocate (pseudo_ob%azim)
   end if

   ! Release the linked list
   plink => head

   DO WHILE ( ASSOCIATED (plink) )
      head => plink%next
      deallocate (plink%gpsref%h)
      deallocate (plink%gpsref%ref)
      if ( use_gpsephobs ) then
         deallocate (plink%azim)
         deallocate (plink%lat)
         deallocate (plink%lon)
      end if
      deallocate ( plink )
      plink => head
   ENDDO

   NULLIFY (head)

   if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro_eph")
#else
   call da_error(__FILE__,__LINE__,(/"must compile with BUFR library"/))
#endif

end subroutine da_read_obs_bufrgpsro_eph
