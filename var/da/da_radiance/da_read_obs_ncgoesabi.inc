subroutine  da_read_obs_ncgoesabi (iv, satellite_id)

   implicit none

   type (iv_type),intent (inout) :: iv
   integer, intent(in)           :: satellite_id ! 16 or 17

   type(datalink_type), pointer :: head, p, current, prev, p_fgat
   type(info_type)              :: info
   type(model_loc_type)         :: loc
   integer(i_kind), allocatable :: ptotal(:)
   real(r_kind)                 :: crit
   integer(i_kind)              :: iout, iobs, i_dummy(1)
   logical                      :: outside, outside_all, iuse, first_chan
   logical                      :: found, head_found

   !! ABI Fixed Grid Variables
   integer                      :: ny, nx
   integer                      :: yoff_fd, xoff_fd

   !! Earth location info
   real(r_kind)                 :: req, rpol, pph, nam
!!!   real                         :: lat_sat, lon_sat ! Assume fixed values in da_get_satzen
   real, allocatable            :: lat(:,:), lon(:,:), satzen(:,:)

   !! Masks for data reduction
   logical, allocatable         :: &
      earthmask(:,:), &
      zenmask(:,:), &
      fgatmask(:,:), &
      allmask_local(:,:), &
      allmask_global(:,:), &
      patchmask(:,:), &
      domainmask(:,:), &
      radmask(:,:), &
      thinmask(:,:)

   integer(kind=1), allocatable :: view_choice(:,:,:,:)
   logical                      :: use_view_choice, best_view

   !! Brightness Temperature (K)
   real, allocatable            :: bt(:,:)

!!!   !! Temporary, only used for data I/O
!!!   character(len=100)           :: prefix=""
!!!   character(len=10)            :: nxthin
!!!   logical                      :: print_grid = .false., &
!!!                                   print_view = .false., &
!!!                                   print_bt = .false.

   !! Iterates
   integer                      :: ichan, ifile, iview, ifgat, ipass, ioff, &
                                   jchan, jfile, jview, &
                                   n, i, j, ix, iy

   !! Thinning Variables, need to replace with applicable WRFDA ones
   integer, parameter           :: xthin=1
   integer, parameter           :: ythin=1

   !! Satellite variables
   integer(i_kind),parameter    :: nchan = 10
   integer(i_kind),parameter    :: nscan = 22
   integer, parameter           :: platform_id  = 4   ! GOES series
   integer, parameter           :: sensor_id    = 44  ! ABI
   integer, parameter           :: channel_list(nchan) = (/7,8,9,10,11,12,13,14,15,16/) !List of all available channels
   integer, parameter           :: nviews=4
   integer(i_kind)              :: inst

   character(len=14), parameter :: INST_PREFIX  = 'OR_ABI-L1b-Rad'


   !! File reading variables
   character(len=1000)          :: fname, fname_short, command
   character(len=50)            :: list_file, count_file
   integer                      :: file_unit

   type date_type
      integer                   :: yr, mt, dy, hr, mn, sc
   end type

   type viewinfo
      logical                          :: select
      integer                          :: nfiles
      character(len=1000)              :: fpath
      character(len=200), allocatable  :: filename(:)
      integer, allocatable             :: filechan(:)
      type(date_type), allocatable     :: filedate(:)
      logical, allocatable             :: file_fgat_match(:,:)
      real*8, allocatable              :: fgat_time_diff(:,:) ! seconds
      real*8, allocatable              :: min_time_diff(:,:) ! seconds
      integer, allocatable             :: nfiles_used(:) 
      integer                          :: ny, nx, yoff, xoff
      real, allocatable                :: &
         lat(:,:), lon(:,:), satzen(:,:)
      logical, allocatable             :: &
         earthmask(:,:), zenmask(:,:), &
         patchmask(:,:), domainmask(:,:)
      character(len=2)                 :: name_short
      character(len=10)                :: name
   end type viewinfo

   type(viewinfo), target, allocatable :: view_att(:)
   type(viewinfo), pointer             :: this_view

   integer                             :: first_file, tot_files_used, npass
   integer                             :: ncid, varid

   !! WRFDA channel and satellite_id select
   !! These should be inputs to the subroutine or global variables in WRFDA
   !Could populate using .info file.  Would reduce number of files to read...
   integer, dimension(10)              :: channel_select = (/7, 8, 9, 10, 11, 12, 13, 14, 15, 16/)

!!!   ! Global WRFDA obs timing info
   character(len=19)                   :: fgat_times_c(num_fgat_time)
   real(r_kind)                        :: fgat_times_r(num_fgat_time)
!!!   real(r_kind)                        :: dt_fgat(2) ! (seconds)

   ! Local Obs date/time variables
!!!   real(r_kind)                        :: j2000, j2000_fgat(num_fgat_time)
   real(r_kind)                        :: obs_time
   integer(i_kind)                     :: yr, mt, dy, hr, mn, sc, jdy
   real(r_kind)                        :: timbdy(2)

   ! Other work variables
   real(r_kind)     :: dlon_earth,dlat_earth,dlon_earth_deg,dlat_earth_deg
   real(r_kind)     :: ngoes
   integer(i_kind)  :: num_goesabi_local, num_goesabi_global, num_goesabi_used, &
                 num_goesabi_used_tmp, num_goesabi_thinned
   integer(i_kind)  :: itx, itt
   real, allocatable :: in(:), out(:)

   if (trace_use) call da_trace_entry("da_read_obs_ncgoesabi")

! determine if satellite_id is supported
!-----------------------------------------------------
   if(satellite_id .ne. 16 .and. &
      satellite_id .ne. 17) then
      write(unit=stdout,fmt='(A,I2.2,A)') 'goes satellite ', satellite_id, ' is not supported for abi instrument'
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
      write(unit=message(1),fmt='(A,I2,A)') " goes-",satellite_id,"-abi is not in sensor list"
      call da_warning(__FILE__,__LINE__, message(1:1))
      return
   end if

   allocate(ptotal(0:num_fgat_time))
   ptotal(0:num_fgat_time) = 0
   iobs = 0                 ! for thinning, argument is inout


! 1.0 Read (grid), parse, and select NC files: identify files for channels, views, times, overlap w/ patch/domain
!----------------------------------------------------------------------------------------------------------
! 2.0 Read (BT) NC files: grab radiance data and convert to BT (K)
!----------------------------------------------------------------------------------------------------------
!
! JJG:  NEED TO ADD A MORE COMPLETE DESCRIPTION HERE
!

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Initialize ABI L1B reading
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   do ifgat=1,num_fgat_time
      if (num_fgat_time.eq.1 .or. (ifgat.gt.1 .and. ifgat.lt.num_fgat_time)) then
         fgat_times_r(ifgat) = &
            (time_slots(ifgat) + time_slots(ifgat-1)) / 2.D0 !minutes
      else if (ifgat .eq. 1) then !First time slot is dt/2 (da_get_time_slots)
         fgat_times_r(ifgat) = &
            time_slots(ifgat-1) !minutes
      else !Last time slot is dt/2 (da_get_time_slots)
         fgat_times_r(ifgat) = &
            time_slots(ifgat) !minutes
      end if

      call da_get_cal_time(fgat_times_r(ifgat),yr,mt,dy,hr,mn,sc)
      fgat_times_r(ifgat) = fgat_times_r(ifgat) * 60.D0 !seconds

      write(unit=fgat_times_c(ifgat), &
         fmt='(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')  &
         yr, '-', mt, '-', dy, '_', hr, ':', mn, ':', sc

   end do

!!!   !! UPDATE THIS FOR VARIABLES AVAILABLE IN WRFDA
!!!   !! Establish fgat j2000day for later comparisons
!!!   fgat_time = analysis_date(1:19)
!!!   read(fgat_time(1:4),fmt='(I4)') yr
!!!   read(fgat_time(6:7),fmt='(I2.2)') mt
!!!   read(fgat_time(9:10),fmt='(I2.2)') dy
!!!   read(fgat_time(12:13),fmt='(I2.2)') hr
!!!   read(fgat_time(15:16),fmt='(I2.2)') mn
!!!   read(fgat_time(18:19),fmt='(I2.2)') sc
!!!   call cal2j2000day(j2000_fgat(1),yr,mt,dy,hr,mn,sc)
!
!!!   if ( var4d ) then
!!!      dt_fgat(1) = - real(var4d_bin, 8)
!!!      dt_fgat(2) =   real(var4d_bin, 8)
!
!!!      do ifgat = 2, num_fgat_time
!!!         if (ifgat .lt. num_fgat_time) then
!!!            j2000 = j2000_fgat(ifgat-1) + dt_fgat(2) / 86400.D0
!!!            call j2000day2cal(j2000,yr,mt,dy,hr,mn,sc)
!!!         end if
!
!!!         write(unit=fgat_times(ifgat), &
!!!            fmt='(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')  &
!!!            yr, '-', mt, '-', dy, '_', hr, ':', mn, ':', sc
!
!!!         call cal2j2000day(j2000_fgat(ifgat),yr,mt,dy,hr,mn,sc)
!!!      end do
!!!   else
!!!      fgat_time = time_window_min(1:19)
!!!      read(fgat_time(1:4),fmt='(I4)') yr
!!!      read(fgat_time(6:7),fmt='(I2.2)') mt
!!!      read(fgat_time(9:10),fmt='(I2.2)') dy
!!!      read(fgat_time(12:13),fmt='(I2.2)') hr
!!!      read(fgat_time(15:16),fmt='(I2.2)') mn
!!!      read(fgat_time(18:19),fmt='(I2.2)') sc
!!!      call cal2j2000day(j2000,yr,mt,dy,hr,mn,sc)
!!!      dt_fgat(1) = (j2000 - j2000_fgat(1)) * 86400. * 2.
!
!!!      fgat_time = time_window_max(1:19)
!!!      read(fgat_time(1:4),fmt='(I4)') yr
!!!      read(fgat_time(6:7),fmt='(I2.2)') mt
!!!      read(fgat_time(9:10),fmt='(I2.2)') dy
!!!      read(fgat_time(12:13),fmt='(I2.2)') hr
!!!      read(fgat_time(15:16),fmt='(I2.2)') mn
!!!      read(fgat_time(18:19),fmt='(I2.2)') sc
!!!      call cal2j2000day(j2000,yr,mt,dy,hr,mn,sc)
!!!      dt_fgat(2) = (j2000 - j2000_fgat(2)) * 86400. * 2.
!
!!!   end if
!!!   write(unit=stdout, fmt='(A)') 'num_fgat, j2000_fgat = '
!!!   write(unit=stdout, fmt='(F18.1)') num_fgat_time, j2000_fgat

   allocate(view_att(nviews))
   view_att(:)%select = .true.  ! Need to set this according to namelist entries
   view_att(1)%name_short = 'F'
   view_att(2)%name_short = 'C'
   view_att(3)%name_short = 'M1'
   view_att(4)%name_short = 'M2'

   view_att(1)%name = 'Full Disk'
   view_att(2)%name = 'CONUS'
   view_att(3)%name = 'MESO1'
   view_att(4)%name = 'MESO2'

   view_att(1)%fpath = './goes-fd/'
   view_att(2)%fpath = './goes-conus/'
   view_att(3)%fpath = './goes-meso/'
   view_att(4)%fpath = './goes-meso/'

   !! Initialize local obs structures
   allocate (head)
   nullify  (head % next )
   p => head

   num_goesabi_local   = 0
   num_goesabi_global  = 0
   num_goesabi_used    = 0
   num_goesabi_thinned = 0


   !! Take 2 passes over the data:
   !! + 1st pass: determine which views should be used for each fgat and each channel across observed domain
   !! + 2nd pass: read radiance values and convert to BT

   npass = 1
   if (nviews.gt.1 .and. view_att(1)%select) npass = 2
   tot_files_used = 0

   do ipass = 1, npass
      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt='(A,I0,A,I2.2,A)') &
         'Starting pass ',ipass,&
         ' of GOES-',satellite_id,' data processing'

   !! Loop over the available views for this instrument (ABI)
   do iview = 1, nviews
      this_view => view_att(iview)
      if ( .not.this_view%select ) cycle

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! Collect files available for this view
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (ipass .eq. 1) then
         write(unit=stdout,fmt='(5A)') 'Searching for GOES ', trim(this_view%name) ,' files in ', trim(this_view%fpath),'...'

         ! Query fpath for files that begin with INST_PREFIX, name, and for this satellite_id
         fname = trim(INST_PREFIX)//trim(this_view%name_short)
         list_file = 'INST'//trim(this_view%name_short)
         count_file = 'num_INST'//trim(this_view%name_short)

         call da_get_unit(file_unit)

         if (rootproc) then
            write(command,fmt='(5A,I2.2,2A)')&
                    "find ",trim(this_view%fpath), &
                    " -type f -name '",trim(fname), &
                    "*G",satellite_id, &
                    "*' -printf '%P\n' > ",trim(list_file)
            call execute_command_line (trim(command))

            write(command,fmt='(4A)') "cat ",trim(list_file)," | wc -l > ",trim(count_file)
            call execute_command_line (trim(command))

            open(unit=file_unit,file=trim(count_file))
            read(file_unit,*) this_view%nfiles
            close(file_unit)
            i_dummy = this_view%nfiles
         end if
#ifdef DM_PARALLEL
         call mpi_barrier(comm, ierr)
!!!         call wrf_dm_bcast_integer(i_dummy, 1)
         call mpi_bcast ( i_dummy, 1, mpi_integer, root, comm, ierr )
         this_view%nfiles = i_dummy(1)
#endif
         if (this_view%nfiles .lt. 1) then
            if (iview .eq. 1) then
               npass = 1
            end if
            this_view%select = .false.
            cycle
         end if

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !! Allocate/init components for this_view
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         allocate(this_view%filename(this_view%nfiles))
         allocate(this_view%filechan(this_view%nfiles))
         allocate(this_view%filedate(this_view%nfiles))
         allocate(this_view%file_fgat_match(this_view%nfiles,num_fgat_time))
         allocate(this_view%fgat_time_diff(this_view%nfiles,num_fgat_time))
         allocate(this_view%min_time_diff(nchan,num_fgat_time))
         allocate(this_view%nfiles_used(num_fgat_time))

!!!         this_view%file_fgat_match = .true.
         this_view%file_fgat_match = .false.
!!!         this_view%fgat_time_diff = max(dt_fgat(1),dt_fgat(2))
         do ifgat=1,num_fgat_time
            this_view%fgat_time_diff(:,ifgat) = &
               (time_slots(ifgat) - time_slots(ifgat-1)) * 60.D0 !seconds

            this_view%min_time_diff(:,ifgat) = &
               (time_slots(ifgat) - time_slots(ifgat-1)) * 60.D0 / 2.D0 !seconds
         end do

!!!         this_view%min_time_diff = max(dt_fgat(1),dt_fgat(2)) / 2.D0

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !! Determine which of the files will be used based on user-definitions:
         !! + fgat window length
         !! + channels used
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         ! Read the file names for this view
         open(unit=file_unit,file=trim(list_file))
         read(file_unit, fmt='(A)') (this_view%filename(ifile), ifile=1,this_view%nfiles)
         close(file_unit)

         call da_free_unit(file_unit)

         do ifile = 1, this_view%nfiles

            ioff = 0
            if (iview.eq.3 .or. iview.eq.4) ioff=1

            ioff = ioff+19
            fname = trim(this_view%filename(ifile))
            read(fname(1+ioff:2+ioff),fmt='(I2.2)') this_view%filechan(ifile)

!!!            !! The channel could instead be read from band_id in each file, but
!!!            !!  opening/closing files for all channels is time consuming
!!!            ierr=nf_open(trim(this_view%fpath)//trim(fname),nf_nowrite,ncid)
!!!            ierr=nf_inq_varid(ncid,'band_id',varid)
!!!            ierr=nf_get_var_int(ncid,varid,this_view%filechan(ifile))
!!!            ierr=nf_close(ncid)

            ! Check if channel is selected
            if ( .not.any(this_view%filechan(ifile) .eq. channel_select) .or. &
                 .not.any(this_view%filechan(ifile) .eq. channel_list) ) then
!!!               ierr=nf_close(ncid)
!!!               this_view%file_fgat_match(ifile,:) = .false.
               cycle
            end if

            !! Determine central date of this file for obs binning
            !obs START time
            ioff = ioff + 8
            read(fname(1+ioff:4+ioff),fmt='(I4.4)') yr
            read(fname(5+ioff:7+ioff),fmt='(I3.3)') jdy
            read(fname(8+ioff:9+ioff),fmt='(I2.2)') hr
            read(fname(10+ioff:11+ioff),fmt='(I2.2)') mn
            read(fname(12+ioff:13+ioff),fmt='(I2.2)') sc
            call jday2cal(jdy, yr, mt, dy)
!!!            call cal2j2000day(timbdy(1),yr,mt,dy,hr,mn,sc)
            call da_get_julian_time(yr,mt,dy,hr,mn,timbdy(1))

            !obs END time
            ioff = ioff + 16
            read(fname(1+ioff:4+ioff),fmt='(I4.4)') yr
            read(fname(5+ioff:7+ioff),fmt='(I3.3)') jdy
            read(fname(8+ioff:9+ioff),fmt='(I2.2)') hr
            read(fname(10+ioff:11+ioff),fmt='(I2.2)') mn
            read(fname(12+ioff:13+ioff),fmt='(I2.2)') sc
            call jday2cal(jdy, yr, mt, dy)
!!!            call cal2j2000day(timbdy(2),yr,mt,dy,hr,mn,sc)
            call da_get_julian_time(yr,mt,dy,hr,mn,timbdy(2))

!!!            j2000=(timbdy(1) + timbdy(2)) / 2.D0
            obs_time=(timbdy(1) + timbdy(2)) / 2.D0
            obs_time = obs_time + real(sc,8)/60.D0

!! The time it takes to read time_bounds from each file is not insignificant.  Above method is much faster.
!            !! Determine central date of this file for obs binning
!!!            ierr=nf_open(trim(this_view%fpath)//trim(fname),nf_nowrite,ncid)
!!!            ierr=nf_inq_varid(ncid,'time_bounds',varid)
!!!            ierr=nf_get_var_double(ncid,varid,timbdy)
!!!            ierr=nf_close(ncid)
!!!            j2000=(timbdy(1) + timbdy(2)) / 2.D0 /86400.D0


!!!            call j2000day2cal(j2000,yr,mt,dy,hr,mn,sc)

            call da_get_cal_time(obs_time,yr,mt,dy,hr,mn,sc)
            obs_time = obs_time * 60.D0

            this_view%filedate(ifile)%yr = yr
            this_view%filedate(ifile)%mt = mt
            this_view%filedate(ifile)%dy = dy
            this_view%filedate(ifile)%hr = hr
            this_view%filedate(ifile)%mn = mn
            this_view%filedate(ifile)%sc = sc

!!!            ! Compare this file j2000day to all fgat window j2000day's
!!!            do ifgat = 1, num_fgat_time
!!!               this_view%fgat_time_diff(ifile,ifgat) = 86400.D0 * (j2000 - j2000_fgat(ifgat))
!
!!!               write(unit=stdout, fmt='(F18.1)') this_view%fgat_time_diff(ifile,ifgat)
!
!!!               if ( this_view%fgat_time_diff(ifile,ifgat) .lt. dt_fgat(1)/2. .or. &
!!!                    this_view%fgat_time_diff(ifile,ifgat) .gt. dt_fgat(2)/2. ) then
!
!!!                  this_view%file_fgat_match(ifile,ifgat) = .false.
!!!                  cycle
!!!               end if
!!!               call get_ichan(this_view%filechan(ifile), channel_list, nchan, ichan)
!
!!!               ! Determine minimum time difference between this obs bin and available files for this view
!!!               if ( abs(this_view%fgat_time_diff(ifile, ifgat)) .ge. this_view%min_time_diff(ichan, ifgat) ) then
!!!                  this_view%file_fgat_match(ifile,ifgat) = .false.
!!!                  exit
!!!               else
!!!                  this_view%min_time_diff(ichan, ifgat) = abs(this_view%fgat_time_diff(ifile, ifgat))
!!!               end if
!!!            end do

!!! Eliminates need for j2000_fgat(ifgat), j2000day2cal, cal2j2000day, uses internal WRFDA timing subroutines + da_get_cal_time
!!! Using julian time precise to seconds due to MESO

            if ( obs_time < time_slots(0) * 60.D0 .or.  &
               obs_time >= time_slots(num_fgat_time) * 60.D0 ) then
               cycle
            end if

            do ifgat=1,num_fgat_time
               this_view%file_fgat_match(ifile,ifgat) = &
                  ( obs_time >= time_slots(ifgat-1) * 60.D0 .and.  &
                     obs_time  < time_slots(ifgat) * 60.D0 )
               if (this_view%file_fgat_match(ifile,ifgat)) exit
            end do

            this_view%fgat_time_diff(ifile,ifgat) = &
                abs( obs_time - fgat_times_r(ifgat) )

            call get_ichan(this_view%filechan(ifile), channel_list, nchan, ichan)
            if ( abs(this_view%fgat_time_diff(ifile, ifgat)) .ge. &
                 this_view%min_time_diff(ichan, ifgat) ) then
               this_view%file_fgat_match(ifile,ifgat) = .false.
            else
               this_view%min_time_diff(ichan, ifgat) = abs(this_view%fgat_time_diff(ifile, ifgat))
            end if

            if (count(this_view%file_fgat_match(ifile,:)) .gt. 1) then
               print*, 'WARNING: More than one bin was selected for ',trim(fname) 
               print*, 'num_bin_per_file = ',count(this_view%file_fgat_match(ifile,:))
               print*, 'obs_time = ',obs_time
               print*, 'Ignoring this file for reading.'
               this_view%file_fgat_match(ifile,:) = .false.
               cycle
            end if
         end do
      end if


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! Access netcdf channel/band files across all fgat windows
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      this_view%nfiles_used = 0

      do ifgat = 1, num_fgat_time
         if (count(this_view%file_fgat_match(:, ifgat)) .lt. 1) then
            cycle
         end if

         write(unit=stdout,fmt='(A,I0,A)') &
            'Processing GOES-',satellite_id,' ABI data for:'
         write(unit=stdout,fmt='(2A)') &
            '          ',fgat_times_c(ifgat)

!!         if ( ipass .eq. 1 .and. (npass.gt.1 .or. count(this_view%file_fgat_match(:, ifgat)).gt.1) ) then
         if ( ipass .eq. 1 .and. count(this_view%file_fgat_match(:, ifgat)).gt.1 ) then

            ! Select a single file for this view, channel, and fgat
            do ifile = 1, this_view%nfiles
               if ( .not. this_view%file_fgat_match(ifile,ifgat) ) cycle
               call get_ichan(this_view%filechan(ifile), channel_list, nchan, ichan)
               if ( abs(this_view%fgat_time_diff(ifile, ifgat)) .gt. this_view%min_time_diff(ichan, ifgat) ) then
                  this_view%file_fgat_match(ifile,ifgat) = .false.
               end if
            end do
         end if

         do ifile = 1, this_view%nfiles
            if ( .not. this_view%file_fgat_match(ifile,ifgat) ) cycle
            first_file = ifile
            exit
         end do

         fname_short = trim(this_view%filename(first_file))
         fname = trim(this_view%fpath)//trim(fname_short)

write(stdout,fmt=*) 'TEST1'

         if ( ipass.eq.1 .and. sum(this_view%nfiles_used(:)).eq.0 ) then
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !! Get ABI metadata (first pass for FD, CONUS, MESO)
            ! Only ny and nx need to be read for all views, but this is a cheap subroutine
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            write(unit=stdout,fmt='(2A)') &
               '          Reading metadata for ',trim(this_view%name)

            call get_abil1b_metadata( &
               fname, this_view%ny, this_view%nx, req, rpol, pph, nam)! , lat_sat, lon_sat )

write(stdout,fmt=*) 'TEST2'
write(stdout,fmt=*) this_view%ny, this_view%nx, req, rpol, pph, nam

            if ( iview.eq.1 .and. ipass.lt.npass ) then
               allocate(view_choice(this_view%ny,this_view%nx,nchan,num_fgat_time))
               view_choice = 0

write(stdout,fmt=*) size(view_choice)
write(stdout,fmt=*) sizeof(view_choice)

            end if
         end if
         ny = this_view%ny
         nx = this_view%nx
write(stdout,fmt=*) 'TEST3'


         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !! Generate grid if
         !!  + CONUS or FD and first matching fgat
         !!  + MESO and any fgat (extent changes in time)
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if (  ( any(iview.eq.(/1,2/)) .and. sum(this_view%nfiles_used(:)).eq.0 ) &
            .or. any(iview.eq.(/3,4/)) ) then

            if (allocated(lat)) then
               deallocate(lat)
               deallocate(lon)
               deallocate(satzen)
               deallocate(earthmask)
               deallocate(zenmask)
            end if

            !! Allocate spatial information
            allocate(lat(ny,nx))
            allocate(lon(ny,nx))
            allocate(satzen(ny,nx))
            allocate(earthmask(ny,nx))
            allocate(zenmask(ny,nx))
            allocate(patchmask(ny,nx))
            allocate(domainmask(ny,nx))

write(stdout,fmt=*) 'TEST4'

write(stdout,fmt=*) sizeof(lat), size(lat)
write(stdout,fmt=*) sizeof(lon), size(lon)
write(stdout,fmt=*) sizeof(satzen), size(satzen)

write(stdout,fmt=*) 'TEST5'

            if ( ipass.eq.2 .and. iview .eq. 1 ) then
               ! Restore FD grid from memory
               lat = this_view%lat
               lon = this_view%lon
               satzen = this_view%satzen
               earthmask = this_view%earthmask
               zenmask = this_view%zenmask
               patchmask = this_view%patchmask
               domainmask = this_view%domainmask

               deallocate(this_view%lat)
               deallocate(this_view%lon)
               deallocate(this_view%satzen)
               deallocate(this_view%earthmask)
               deallocate(this_view%zenmask)
               deallocate(this_view%patchmask)
               deallocate(this_view%domainmask)

               this_view%yoff = yoff_fd
               this_view%xoff = xoff_fd
            else
write(stdout,fmt=*) 'TEST6'

               ! Read grid from file
               write(unit=stdout,fmt='(2A)') &
                  '          Reading grid info for ',trim(this_view%name)
write(stdout,fmt=*) 'TEST7'

               call get_abil1b_grid( fname, ny, nx, &
                                        req, rpol, pph, nam, satellite_id, &
                                        lat, lon, satzen, earthmask, zenmask, &
                                        this_view%yoff, this_view%xoff )
write(stdout,fmt=*) 'TEST8'

               !! With domain/patch pruning was here, view_choice can be allocated smaller
               patchmask = .false.
               domainmask = .false.
               do ix = 1, nx
if (mod(ix,100).eq.0) write(stdout,fmt=*) 'TEST9', ix

                  do iy = 1, ny
                     if (earthmask(iy,ix)) then
                        info%lon  =  lon(iy,ix)  ! longitude
                        info%lat  =  lat(iy,ix)  ! latitude
                        call da_llxy (info, loc, outside, outside_all)
                        patchmask(iy,ix) = outside
                        domainmask(iy,ix) = outside_all
                     end if
                  end do
               end do
            end if
write(stdout,fmt=*) 'TEST10'

            if ( iview.eq.1 ) then
               yoff_fd = this_view%yoff
               xoff_fd = this_view%xoff
               this_view%yoff = 1
               this_view%xoff = 1
            else
               this_view%yoff = this_view%yoff - yoff_fd
               this_view%xoff = this_view%xoff - xoff_fd
            end if
write(stdout,fmt=*) 'TEST11'

!!!!               print*,'yoff = ',this_view%yoff
!!!!               print*,'xoff = ',this_view%xoff
!!!!!!! START GRID WRITE
!!!            if ( ipass .eq. 1 .and. print_grid .and. iview.eq.1) then
!!!               write(prefix,fmt='(3A)') &
!!!                 'GRID_VIEW',trim(this_view%name_short),'_'
!!!               if (iview .gt. 2) prefix = trim(prefix)// &
!!!                    fgat_times_c(ifgat)//'_'
!
!!!               open(unit=31, &
!!!                    file=trim(prefix)//'lat.dat', &
!!!                    status='replace')
!!!               open(unit=32, &
!!!                    file=trim(prefix)//'lon.dat', &
!!!                    status='replace')
!!!               open(unit=33, &
!!!                    file=trim(prefix)//'satzen.dat', &
!!!                    status='replace')
!
!!!               write(nxthin,fmt='(I0)') nx/xthin+1
!!!               do iy=1, ny, ythin
!!!                  write(31,fmt='('//trim(nxthin)//'F15.6)') lat    (iy, 1:nx:xthin)
!!!                  write(32,fmt='('//trim(nxthin)//'F15.6)') lon    (iy, 1:nx:xthin)
!!!                  write(33,fmt='('//trim(nxthin)//'F15.6)') satzen (iy, 1:nx:xthin)
!!!               end do
!!!               close(31)
!!!               close(32)
!!!               close(33)
!!!            end if
!!!!!!! END GRID WRITE
         end if

write(stdout,fmt=*) 'TEST12'

         ! Loop over channels
         ! This loop over channels could be parallelized, if needed for time savings

         ChannelLoop: do ichan = 1, nchan

            ifile = 0
            do jfile = 1, this_view%nfiles
               if ( .not. this_view%file_fgat_match(jfile,ifgat) ) cycle
               call get_ichan(this_view%filechan(jfile), channel_list, nchan, jchan)
               if ( ichan .eq. jchan ) then
                  ifile = jfile
                  exit
               end if
            end do
            if ( ifile .eq. 0 ) cycle

            this_view%nfiles_used(ifgat) = this_view%nfiles_used(ifgat) + 1

            use_view_choice = ( sum(view_att(1)%nfiles_used(:)).gt.0 )

            VIEW_SELECT: &
            if ( ipass.lt.npass .and. use_view_choice ) then
               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               !! Determine which view has the closest observed 
               !!  time to fgat for this channel
               !!  Note: this only needs to be done for a single channel, 
               !!    unless individual channel files are missing at fgat.
               !!    Solution where file view availability differs by channel used here.
               !!  (only available when FD data present for one of the fgat times)
               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               if ( iview.eq.1 ) then
                  view_choice(:,:, ichan, ifgat) = 1
               else
                  best_view = .true.
!                  do jview = 1, iview-1 !This assumes MESO1 and MESO2 are in identical locations
                  do jview = 1, min(iview-1,2) !This assumes MESO1 and MESO2 do not overlap
                     best_view = best_view .and. &
                        this_view%min_time_diff(ichan, ifgat) .lt. &
                           view_att(jview)%min_time_diff(ichan, ifgat)
                  end do
                  if ( best_view ) &
                     view_choice(this_view%yoff:ny+this_view%yoff-1, &
                                 this_view%xoff:nx+this_view%xoff-1, &
                                 ichan, ifgat) = iview
               end if

!!!!!!! START VIEW_CHOICE WRITE
!!!               if ( this_view%nfiles_used(ifgat).eq.1 .and. print_view  .and. iview.eq.nviews) then
!!!                  write(unit=stdout,fmt='(A)') &
!!!                     '          printing view_choice'
!!!
!!!                  write(prefix,fmt='(5A)') &
!!!                       'VIEW',trim(this_view%name_short),'_', &
!!!                       fgat_times_c(ifgat),'_'
!!!
!!!                  open(unit=30,file=trim(prefix)//&
!!!                       'CHOICE.dat',status='replace')
!!!                  write(nxthin,fmt='(I0)') view_att(1)%nx
!!!
!!!                  do iy=1, view_att(1)%ny
!!!                     write(30,fmt='('//trim(nxthin)//'I3)') view_choice (iy, 1:view_att(1)%nx, ichan, ifgat)
!!!                  end do
!!!
!!!                  close(30)
!!!               end if
!!!!! END VIEW_CHOICE WRITE
            else
               if (inst == 0) cycle

               fname_short = trim(this_view%filename(ifile))
               fname = trim(this_view%fpath)//trim(fname_short)

               allocate(allmask_local(ny,nx))
               allocate(allmask_global(ny,nx))

               !!Utilizing these masks to eliminate data:
               !! + earthmask
               !! + zenmask
               !! + fgatmask [only if npass > 1]
               !! + model domain mask
               !! + patch mask
               !! + thinning

               allmask_global = (earthmask .and. zenmask .and. domainmask)
               allmask_local = (earthmask .and. zenmask .and. patchmask)

               ! Only use locations where this view is nearest to this fgat time
               ! - only available when FD data present for any fgat time
               if (  use_view_choice ) then
                  allocate(fgatmask(ny,nx))
                  fgatmask = ( view_choice(this_view%yoff:ny+this_view%yoff-1, &
                                             this_view%xoff:nx+this_view%xoff-1, &
                                             ichan, ifgat) .eq. iview )

                  if ( count(fgatmask) .eq. 0 ) then
                     deallocate(fgatmask)
                     deallocate(allmask_local)
                     deallocate(allmask_global)
                     cycle
                  end if
                  allmask_local = (allmask_local .and. fgatmask)
                  allmask_global = (allmask_global .and. fgatmask)
               end if

               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               !! Read radiance and convert to brightness temp.
               !! once per permutation of 
               !! + INST VIEW (FD, CONUS, MESOx2)
               !! + fgat
               !! + channel/band
               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               write(unit=stdout,fmt='(2A)') &
                  '          Reading radiances: ',trim(fname_short)

!!print*,this_view%fgat_time_diff(ifile,ifgat)

               ! Allocate this bt
               allocate(bt(ny,nx))
               allocate(radmask(ny,nx))

!               ! This reads in bt only for the local patch,
!               !  reduces read time, but would mess up global count below
!               call get_abil1b_bt( fname, ny, nx, allmask_local, &
!                                      bt )

               ! This reads in bt for whole domain, 
               !  creates valid plocal check at end of this subroutine, 
               !  could read only on rootproc and distribute data,
               !  or switch to round-robin reading+distribution across channels
               call get_abil1b_bt( fname, ny, nx, allmask_global, radmask, &
                                      bt )

               allmask_local  = (allmask_local .and. radmask)
               allmask_global = (allmask_global .and. radmask)
               deallocate(radmask)

!!!!!!! START BT WRITE
!!!               if ( ipass .eq. npass .and. print_bt ) then
!!!                  write(prefix,fmt='(A,I2.2,5A)') &
!!!                    'BT_C',this_view%filechan(ifile), &
!!!                       '_VIEW',trim(this_view%name_short),'_', &
!!!                       fgat_times_c(ifgat),'_'
!!!
!!!                  open(unit=30,file=trim(prefix)//'bt.dat',&
!!!                       status='replace')
!!!                  write(nxthin,fmt='(I0)') nx/xthin+1
!!!
!!!                  do iy=1, ny, ythin
!!!                     write(30,fmt='('//trim(nxthin)//'F15.6)') bt(iy, 1:nx:xthin)
!!!                  end do
!!!
!!!                  close(30)
!!!               end if
!!!!! END BT WRITE

               !! Write bt, lat, lon, and satzen to datalink structures

               first_chan = (this_view%nfiles_used(ifgat).eq.1)
               if (first_chan) then
                  p_fgat => p
                  allocate(thinmask(ny,nx))
                  thinmask = .false.

                  yr = this_view%filedate(ifile)%yr
                  mt = this_view%filedate(ifile)%mt
                  dy = this_view%filedate(ifile)%dy
                  hr = this_view%filedate(ifile)%hr
                  mn = this_view%filedate(ifile)%mn
                  sc = this_view%filedate(ifile)%sc
               else
                  p => p_fgat
               end if

               do iy = 1, ny
               do ix = 1, nx

                  if (.not. allmask_global(iy,ix)) cycle

                  if (first_chan) then
                     info%lon  =  lon(iy,ix)  ! longitude
                     info%lat  =  lat(iy,ix)  ! latitude
                     call da_llxy (info, loc, outside)

                     ptotal(ifgat) = ptotal(ifgat) + 1
                     num_goesabi_global = num_goesabi_global + 1
                  end if

                  if (.not. allmask_local(iy,ix)) cycle

                  if (first_chan) &
                     num_goesabi_local = num_goesabi_local + 1

                  if (thinning) then
                     if (first_chan) then
                        dlat_earth = info%lat
                        dlon_earth = info%lon
                        if (dlon_earth<zero) dlon_earth = dlon_earth+r360
                        if (dlon_earth>=r360) dlon_earth = dlon_earth-r360
                        dlat_earth = dlat_earth*deg2rad
                        dlon_earth = dlon_earth*deg2rad
                        crit = 1.
                        call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
                        if (.not. iuse) then
                           num_goesabi_thinned=num_goesabi_thinned+1
                           thinmask(iy,ix) = .true.
                           cycle
                        end if
                     else
                        if (thinmask(iy,ix)) cycle
                     end if
                  end if

                  if (first_chan) then
                     num_goesabi_used = num_goesabi_used + 1

                     write(unit=info%date_char, &
                        fmt='(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')  &
                        yr, '-', mt, '-', dy, '_', hr, ':', mn, ':', sc
                     info%elv = 0.0  !aquaspot%selv
                     allocate ( p % tb_inv (1:nchan) )

                     p%info                  = info
                     p%loc                   = loc
                     p%landsea_mask          = 1  ! ???
                     if (use_view_choice) then
                        p%scanpos               = &
                           (iy + this_view%yoff-1 - 1) * (nscan+1) / view_att(1)%ny  
                           ! ??? "scan" position (IS THIS CORRECT?)
                     else
                        p%scanpos               = &
                           (iy + this_view%yoff-1 - 1) * (nscan+1) / 5423
                           ! ??? "scan" position (IS THIS CORRECT?)
                     end if

                     p%satzen                = satzen(iy,ix)
                     p%solzen                = 0.0
                     p%sensor_index          = inst
                     p%ifgat                 = ifgat
                  end if

                  ! Transfer BT from all files
                  p%tb_inv(ichan)         = bt(iy,ix)

                  if (first_chan) &
                     allocate (p%next)   ! add next data

                  p => p%next

                  if (first_chan) &
                     nullify (p%next)
               end do
               end do

               deallocate(bt)
               deallocate(allmask_local)
               deallocate(allmask_global)

               if ( use_view_choice ) &
                  deallocate(fgatmask)

            end if VIEW_SELECT

         end do ChannelLoop

         if (this_view%nfiles_used(ifgat).ge.1) &
            deallocate(thinmask)

      end do ! end fgat loop

      if (sum(this_view%nfiles_used) .gt. 0) then
         if (iview.eq.1) then
            ! Store FD grid in memory
            if(ipass.eq.1) then
               allocate(this_view%lat(this_view%ny,this_view%nx))
               allocate(this_view%lon(this_view%ny,this_view%nx))
               allocate(this_view%satzen(this_view%ny,this_view%nx))
               allocate(this_view%earthmask(this_view%ny,this_view%nx))
               allocate(this_view%zenmask(this_view%ny,this_view%nx))
               allocate(this_view%patchmask(this_view%ny,this_view%nx))
               allocate(this_view%domainmask(this_view%ny,this_view%nx))

               this_view%lat = lat
               this_view%lon = lon
               this_view%satzen = satzen
               this_view%earthmask = earthmask
               this_view%zenmask = zenmask
               this_view%patchmask = patchmask
               this_view%domainmask = domainmask
            end if
         end if

         ! Deallocate static data
         deallocate(lat)
         deallocate(lon)
         deallocate(satzen)
         deallocate(earthmask)
         deallocate(zenmask)
      end if

      tot_files_used = tot_files_used + sum(view_att(iview)%nfiles_used)

   end do ! end view loop
   
      if (tot_files_used .lt. 1) then
         write(unit=message(1),fmt='(A,I2,2A)') "No L1B data found for GOES-",satellite_id," using prefix ",INST_PREFIX
         call da_warning(__FILE__,__LINE__, message(1:1))
         return
      end if
   end do ! end pass loop

   if (allocated(view_choice)) deallocate(view_choice)

   do iview = 1, nviews
      if ( .not.view_att(iview)%select ) cycle
      deallocate(view_att(iview)%filename)
      deallocate(view_att(iview)%filechan)
      deallocate(view_att(iview)%filedate)
      deallocate(view_att(iview)%file_fgat_match)
      deallocate(view_att(iview)%fgat_time_diff)
      deallocate(view_att(iview)%min_time_diff)
      deallocate(view_att(iview)%nfiles_used)
   end do
   deallocate(view_att)

!------------------------------------------------------
   ! NOTE: Remainder of this subroutine copied from da_read_obs_ncgoesimg.inc

   if (thinning .and. num_goesabi_global > 0 ) then
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
               if ( ABS(out(j)-thinning_grid(inst,ifgat)%score_crit(i)) > 1.0D-10 ) thinning_grid(inst,ifgat)%ibest_obs(i) = 0
            end do
      end do
      deallocate( in  )
      deallocate( out )

#endif
      ! Delete the nodes which being thinning out
      p => head
      prev => head
      head_found = .false.
      num_goesabi_used_tmp = num_goesabi_used

      do j = 1, num_goesabi_used_tmp
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
            num_goesabi_thinned = num_goesabi_thinned + 1
            num_goesabi_used = num_goesabi_used - 1
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
   iv%total_rad_pixel   = iv%total_rad_pixel + num_goesabi_used
   iv%total_rad_channel = iv%total_rad_channel + num_goesabi_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_goesabi_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_goesabi_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'num_goesabi_global, num_goesabi_local, num_goesabi_used, num_goesabi_thinned'
   write(unit=stdout,fmt=*) num_goesabi_global, num_goesabi_local, num_goesabi_used, num_goesabi_thinned


   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------


   if (num_goesabi_used > 0) then
      iv%instid(inst)%num_rad  = num_goesabi_used
      iv%instid(inst)%info%nlocal = num_goesabi_used
      write(unit=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      call da_allocate_rad_iv (inst, nchan, iv)
   end if

   !  6.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   p => head
   do n = 1, num_goesabi_used
      i = p%sensor_index
      call da_initialize_rad_iv (i, n, iv, p)
      current => p
      p => p%next

      ! free current data
      deallocate ( current % tb_inv )
!!!      deallocate ( current % cloud_flag )
      deallocate ( current )
   end do
   deallocate ( p )
   deallocate (ptotal)

#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif

   if (trace_use) call da_trace_exit("da_read_obs_ncgoesimg")

end subroutine da_read_obs_ncgoesabi


subroutine get_ichan(channel, channel_list, nchan, ichan) !result(ichan)

   implicit none

   integer, intent(in)  :: channel, nchan
   integer, intent(in)  :: channel_list(nchan)
   integer, intent(out) :: ichan
   integer :: i

   ichan = 0
   do i = 1, nchan
      if (channel .eq. channel_list(i)) then
         ichan = i
         exit
      end if
   end do

end subroutine get_ichan



subroutine get_abil1b_metadata( filename, &
                                   ydim, xdim, req, rpol, pph, nam) !, lat_sat, lon_sat )

   implicit none

   character(*), intent(in)  :: filename

   integer, intent(out)      :: ydim, xdim
   real(r_kind), intent(out) :: req, rpol, pph, nam
!!!   real, intent(out)         :: lat_sat, lon_sat

   integer                  :: ierr, ncid, varid, dimid
   real(r_kind), parameter  :: pi=3.1415926535898D0
write(stdout,fmt=*) 'TEST13'

   ierr=nf_open(trim(filename),nf_nowrite,ncid)
   call handle_err('Error opening file',ierr)
write(stdout,fmt=*) 'TEST14'

   !! Determine ABI satellite parameters (optional outputs)
   ierr=nf_inq_dimid(ncid,'y',dimid)
   ierr=nf_inq_dimlen(ncid,dimid,ydim)
   ierr=nf_inq_dimid(ncid,'x',dimid)
   ierr=nf_inq_dimlen(ncid,dimid,xdim)
write(stdout,fmt=*) 'TEST15'

   ierr=nf_inq_varid(ncid,'goes_imager_projection',varid)
   ierr=nf_get_att_double(ncid,varid,'semi_major_axis',req)
   ierr=nf_get_att_double(ncid,varid,'semi_minor_axis',rpol)
   ierr=nf_get_att_double(ncid,varid,'perspective_point_height',pph)
   ierr=nf_get_att_double(ncid,varid,'longitude_of_projection_origin',nam)
   nam=nam*pi/180
write(stdout,fmt=*) 'TEST16'

!!!   ierr=nf_inq_varid(ncid,'nominal_satellite_subpoint_lat',varid)
!!!   ierr=nf_get_var_real(ncid,varid,lat_sat)
!!!   ierr=nf_inq_varid(ncid,'nominal_satellite_subpoint_lon',varid)
!!!   ierr=nf_get_var_real(ncid,varid,lon_sat)

#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif
write(stdout,fmt=*) 'TEST17'

   ierr=nf_close(ncid)
   call handle_err('Error closing file',ierr)
write(stdout,fmt=*) 'TEST18'


end subroutine get_abil1b_metadata

subroutine get_abil1b_grid( filename, ydim, xdim, req, rpol, pph, nam, satellite_id, &
                               lat, lon, satzen, earthmask, zenmask, &
                               yoff, xoff )

   implicit none

   character(*), intent(in) :: filename
   integer, intent(in)      :: ydim, xdim, satellite_id
   real(r_kind), intent(in) :: req, rpol, pph, nam
!!!   real, intent(in)         :: lon_sat

   real, intent(out)        :: lat(ydim,xdim), lon(ydim,xdim), satzen(ydim,xdim)
   logical, intent(out)     :: earthmask(ydim, xdim), zenmask(ydim, xdim)
   integer, intent(out)     :: yoff, xoff

   real                     :: yy(ydim), xx(xdim)
   integer                  :: ierr, ncid, varid
   integer                  :: iy, ix
   real                     :: slp, itp
   real(r_kind)             :: hh

   real                     :: alat, alon ! , alon_sat
   real                     :: theta, theta1, theta2, r1

   real, parameter         :: rre=6371.004*1e3
   real(r_kind), parameter :: pi=3.1415926535898D0
   real, parameter         :: satzen_limit=75.0
   real, parameter         :: fillv=-999.000
write(stdout,fmt=*) 'TEST19'

   ierr=nf_open(trim(filename),nf_nowrite,ncid)
   call handle_err('Error opening file',ierr)
write(stdout,fmt=*) 'TEST20'

   ierr=nf_inq_varid(ncid,'y',varid)
write(stdout,fmt=*) 'TEST21'

   ierr=nf_get_var_real(ncid,varid,yy)
write(stdout,fmt=*) 'TEST22'

   ierr=nf_get_att_real(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_real(ncid,varid,'add_offset',itp)
   yy = yy*slp+itp 
   yoff = floor(itp/slp)
write(stdout,fmt=*) 'TEST23'

   ierr=nf_inq_varid(ncid,'x',varid)
write(stdout,fmt=*) 'TEST24'

   ierr=nf_get_var_real(ncid,varid,xx)
write(stdout,fmt=*) 'TEST25'

   ierr=nf_get_att_real(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_real(ncid,varid,'add_offset',itp)
   xx = xx*slp+itp
   xoff = floor(itp/slp)

write(stdout,fmt=*) 'TEST26'

   earthmask=.false.
write(stdout,fmt=*) 'TEST27'

   zenmask=.false.
write(stdout,fmt=*) 'TEST28'

!!!   alon_sat=lon_sat*pi/180.D0

   hh=pph+req
write(stdout,fmt=*) 'TEST29'

write(stdout,fmt=*) hh,pph,req,rpol,nam,slp,itp,xoff,yoff
write(stdout,fmt=*) 'TEST30'

   lat = fillv
write(stdout,fmt=*) 'TEST31'

   lon = fillv
write(stdout,fmt=*) 'TEST32'

   satzen = fillv
write(stdout,fmt=*) 'TEST33'
   do ix=1,xdim
if (mod(ix,100).eq.0) write(stdout,fmt=*) 'TEST34', ix

      do iy=1,ydim
         call get_abil1b_latlon(yy(iy),xx(ix),req,rpol,hh,nam,lat(iy,ix),lon(iy,ix))

         if (isnan(lat(iy,ix)) .OR. isnan(lon(iy,ix))) then
            lat(iy,ix) = fillv
            lon(iy,ix) = fillv
            cycle
         end if

         call da_get_satzen(lat(iy,ix),lon(iy,ix),satellite_id,satzen(iy,ix))

         if (isnan(satzen(iy,ix))) then
            lat(iy,ix) = fillv
            lon(iy,ix) = fillv
            satzen(iy,ix) = fillv
            cycle
         end if
         earthmask(iy,ix)=.true.

         if (satzen(iy,ix).gt.satzen_limit) then
            cycle
         end if
         zenmask(iy,ix)=.true.
      end do
   end do

write(stdout,fmt=*) 'TEST35'

#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif
write(stdout,fmt=*) 'TEST36'

   ierr=nf_close(ncid)
   call handle_err('Error closing file',ierr)

write(stdout,fmt=*) 'TEST37'

end subroutine get_abil1b_grid

subroutine get_abil1b_bt( filename, ydim, xdim, allmask, radmask, &
                             bt )
   implicit none

   character(*), intent(in)      :: filename
   integer, intent(in)           :: ydim, xdim
   logical, intent(in)           :: allmask(ydim, xdim)
   logical, intent(out)          :: radmask(ydim, xdim)
   real, intent(out)             :: bt(ydim, xdim)

   real             :: rad(ydim, xdim)
   integer(kind=1)  :: DQF(ydim, xdim)
   integer          :: ierr, ncid, varid
   integer          :: iy, ix
   integer          :: ystart, yend, xstart, xend, nykeep, nxkeep
   real             :: slp, itp
   real             :: bc1, bc2, fk1, fk2
   real, parameter  :: fillv=-999.000

   bt = fillv
   radmask = .false.
!   if (.true.) then
      !! Attempt to save rad reading time by selecting a subset of netcdf var
      ystart = ydim
      yend = 1
      do iy = 1, ydim
         if ( any(allmask(iy,:)) ) then
            ystart = iy
            exit
         end if
      end do
      do iy = ydim, 1, -1
         if ( any(allmask(iy,:)) ) then
            yend = iy
            exit
         end if
      end do

      xstart = xdim
      xend = 1
      do ix = 1, xdim
         if ( any(allmask(:,ix)) ) then
            xstart = ix
            exit
         end if
      end do
      do ix = xdim, 1, -1
         if ( any(allmask(:,ix)) ) then
            xend = ix
            exit
         end if
      end do
      nykeep = yend - ystart + 1
      nxkeep = xend - xstart + 1

      if (nykeep.gt.0 .and. nxkeep.gt.0) then
         ierr=nf_open(trim(filename),nf_nowrite,ncid)
         call handle_err('Error opening file',ierr)

         ierr=nf_inq_varid(ncid,'Rad',varid)
         ierr=nf_get_vara_real(ncid,varid,(/ystart,xstart/),(/nykeep,nxkeep/), &
                                  rad(ystart:yend,xstart:xend) )

         ierr=nf_inq_varid(ncid,'DQF',varid)
         ierr=nf_get_vara_int(ncid,varid,(/ystart,xstart/),(/nykeep,nxkeep/), &
                                  DQF(ystart:yend,xstart:xend) )
      else
         return
      end if
!   else
!      ystart = 1
!      yend   = ydim
!      xstart = 1
!      xend   = xdim
!
!      ierr=nf_open(trim(filename),nf_nowrite,ncid)
!      ierr=nf_inq_varid(ncid,'Rad',varid)
!
!      ierr=nf_get_var_real(ncid,varid,rad)
!
!      ierr=nf_inq_varid(ncid,'DQF',varid)
!      ierr=nf_get_var_real(ncid,varid,DQF)
!   end if

   ierr=nf_get_att_real(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_real(ncid,varid,'add_offset',itp)
   rad=rad*slp+itp

   ierr=nf_inq_varid(ncid,'planck_bc1',varid)
   ierr=nf_get_var_real(ncid,varid,bc1)
   ierr=nf_inq_varid(ncid,'planck_bc2',varid)
   ierr=nf_get_var_real(ncid,varid,bc2)
   ierr=nf_inq_varid(ncid,'planck_fk1',varid)
   ierr=nf_get_var_real(ncid,varid,fk1)
   ierr=nf_inq_varid(ncid,'planck_fk2',varid)
   ierr=nf_get_var_real(ncid,varid,fk2)

   do ix=xstart, xend
      do iy=ystart, yend
         if ( allmask(iy,ix) ) then
            if ( rad(iy,ix).ge.0.0 .and. any(DQF(iy,ix).eq.(/0,1/)) ) then
               bt(iy,ix)=(fk2/(alog((fk1/rad(iy,ix))+1.))-bc1)/bc2
               radmask(iy,ix) = .true.
            end if
         end if
      end do
   end do

#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif

   ierr=nf_close(ncid)
   call handle_err('Error closing file',ierr)


end subroutine get_abil1b_bt


subroutine get_abil1b_latlon(yy,xx,req,rpol,hh,nam,lat,lon)
implicit none
   real::yy,xx,lat,lon,lat1,lon1
   real::aa,bb,cc,rs,sx,sy,sz
   real*8::req,rpol,hh,nam
   real*8,parameter::pi=3.1415926535898D0

   aa=sin(xx)**2+cos(xx)**2*(cos(yy)**2+req**2/rpol**2*sin(yy)**2)
   bb=-2.D0*hh*cos(xx)*cos(yy)
   cc=hh**2-req**2
   rs=(-bb-sqrt(bb**2-4.D0*aa*cc))/(2.D0*aa)
   sx=rs*cos(xx)*cos(yy)
   sy=-rs*sin(xx)
   sz=rs*cos(xx)*sin(yy)

   lat1=atan(req**2/rpol**2*sz/sqrt((hh-sx)**2+sy**2))
   lon1=nam-atan(sy/(hh-sx))

   lat=lat1*180.D0/pi
   lon=lon1*180.D0/pi

!	print*,hh,rpol,hh
!	print*,aa,bb,cc,rs
!	print*,aa,bb,cc,rs,sx,sy,sz
!	print*,'aaa',lat,lon
!	pause
end subroutine get_abil1b_latlon


!subroutine j2000day2cal(j2000,yr,mt,dy,hr,mn,sc)
!  ! Converts J2000 epoch day to Gregorian calender date
!  ! source: David G. Simpson, NASA Goddard, Accessed April 2018
!  !         https://caps.gsfc.nasa.gov/simpson/software.html
!
!  implicit none
!
!  real(r_kind), intent(in) :: j2000
!  integer, intent(out) :: yr,mt,dy,hr,mn,sc
!
!  real(r_kind) :: ju, j0, F
!  integer      :: yr0, sc0
!  INTEGER :: A, B, C, D, E, Z, ALPHA   ! intermediate variables
!  real(r_kind) :: dd
!  real(r_kind), parameter :: jd_j2000=2451545.0
!
!  !! First convert J2000 to Julian date
!  ju=j2000+jd_j2000
!
!  ju = ju + 0.5D0 
!  Z = INT(ju)
!  F = ju - Z
!
!  !! Gregorian date test (can probably assume this is a Gregorian date)
!  IF (Z .LT. 2299161) THEN
!     A = Z
!  ELSE
!     ALPHA = INT((Z-1867216.25D0)/36524.25D0)
!     A = Z + 1 + ALPHA - ALPHA/4
!  END IF
!
!  B = A + 1524
!  C = INT((B-122.1D0)/365.25D0)
!  D = INT(365.25D0*C)
!  E = INT((B-D)/30.6001D0)
!
!  IF (E .LT. 14) THEN
!     mt = E - 1
!  ELSE
!     mt = E - 13
!  END IF
!
!  IF (mt .GT. 2) THEN
!     yr = C - 4716
!  ELSE
!     yr = C - 4715
!  END IF
!
!  dd = B - D - INT(30.6001D0*E) + F
!
!  dy = floor(dd)
!
!  !! Remainder for hr, mn, sc.
!  dd = dd - real(dy,8)
!
!  sc0 = nint(dd*86400.)
!  hr  = sc0 / 3600
!  sc0 = sc0 - hr*3600
!  mn  = sc0 / 60
!  sc  = sc0 - mn*60
!
!end subroutine j2000day2cal
!
!subroutine cal2j2000day(j2000,yr,mt,dy,hr,mn,sc)
!  ! Converts Gregorian calender date to J2000 epoch day
!  ! source: David G. Simpson, NASA Goddard, Accessed April 2018
!  !         https://caps.gsfc.nasa.gov/simpson/software.html
!  ! Alternative: http://aa.usno.navy.mil/faq/docs/JD_Formula.php
!
!   implicit none
!
!   real(r_kind), intent(out) :: j2000
!   integer, intent(inout) :: yr,mt,dy,hr,mn,sc
!
!   real(r_kind) :: ju
!   INTEGER :: A, B
!   real(r_kind), parameter :: jd_j2000=2451545.0 
!
!
!   IF (mt .LE. 2) THEN
!      yr = yr - 1
!      mt = mt + 12
!   END IF
!
!  !! Gregorian date test (assuming this is a Gregorian date)
!!   IF (GREGORIAN_FLAG) THEN           ! Gregorian calendar
!      A = yr/100
!      B = 2 - A + A/4
!!   ELSE                               ! Julian calendar
!!      B = 0
!!   END IF
!
!   ju = real( INT(365.25D0*(yr+4716)) &
!              + INT(30.6001D0*(mt+1)) + B + dy,8) &
!              - 1524.5D0
!
!   ju = ju + (real(hr,8) &
!               + ( real(mn,8)  &
!               + real(sc,8) / 60.0) / 60.0 ) / 24.0
!
!   j2000 = ju-jd_j2000
!
!end subroutine cal2j2000day

subroutine jday2cal(jdy, yr, mt, dy)

   implicit none

   integer, intent(in)  :: jdy, yr
   integer, intent(out) :: mt, dy

   integer :: d_in_m(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
   integer :: imonth, tot_days


   if ( mod(yr,4).eq.0 .and. .not.(mod(yr,100).eq.0 .and. .not.mod(yr,400).eq.0) ) d_in_m(2) = 29

   tot_days = 0
   do imonth = 1, 12
      tot_days = tot_days + d_in_m(imonth)
      if (tot_days .ge. jdy) then
         mt = imonth
         dy = jdy - ( tot_days - d_in_m(imonth) )
         exit
      end if
   end do

end subroutine jday2cal

subroutine da_get_cal_time(jmod,yr,mt,dy,hr,mn,sc)
  ! Converts modified Julian time (in minutes) to Gregorian calender date
  ! Modified from this code: David G. Simpson, NASA Goddard, Accessed April 2018
  !         https://caps.gsfc.nasa.gov/simpson/software.html

  implicit none

  real(r_kind), intent(in) :: jmod
  integer, intent(out) :: yr,mt,dy,hr,mn
  integer, intent(out), optional :: sc

  real(r_kind) :: ju, j0, F
  integer      :: yr0, sc0
  INTEGER :: A, B, C, D, E, Z, ALPHA   ! intermediate variables
  real(r_kind) :: dd

  ! J2000 Reference time: 2000 Jan 01 00:12:00
  real(r_kind), parameter :: jd_j2000 = 2451545.0

  ! This MJD Reference time: 1978 Jan 01 00:00:00 (see da_get_julian_time)
  real(r_kind), parameter :: jd_jmod = jd_j2000 - 8035.5

  ! Convert to days
  ju = jmod / 1440.D0

  !! Convert reference MJD to actual Julian time
  ju = ju+jd_jmod
  ju = ju + 0.5D0 
  Z = INT(ju)
  F = ju - Z

  !! Gregorian date test (can probably assume this is a Gregorian date)
  IF (Z .LT. 2299161) THEN
     A = Z
  ELSE
     ALPHA = INT((Z-1867216.25D0)/36524.25D0)
     A = Z + 1 + ALPHA - ALPHA/4
  END IF

  B = A + 1524
  C = INT((B-122.1D0)/365.25D0)
  D = INT(365.25D0*C)
  E = INT((B-D)/30.6001D0)

  IF (E .LT. 14) THEN
     mt = E - 1
  ELSE
     mt = E - 13
  END IF

  IF (mt .GT. 2) THEN
     yr = C - 4716
  ELSE
     yr = C - 4715
  END IF

  dd = B - D - INT(30.6001D0*E) + F

  dy = floor(dd)

  !! Remainder for hr, mn, sc.
  dd = dd - real(dy,8)

  sc0 = nint(dd*86400.)
  hr  = sc0 / 3600
  sc0 = sc0 - hr*3600
  mn  = sc0 / 60
  if (present(sc)) sc  = sc0 - mn*60

end subroutine da_get_cal_time

subroutine handle_err(rmarker,nf_status)

   implicit none

   integer, intent(in) :: nf_status
   character*(*), intent(in)        :: rmarker
   if (nf_status .ne. nf_noerr) then
      write(*,*)  'NetCDF error : ',rmarker
      write(*,*)  '  ',nf_strerror(nf_status)
      stop 
   endif    
end subroutine handle_err

