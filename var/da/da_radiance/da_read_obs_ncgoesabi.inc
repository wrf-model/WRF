subroutine  da_read_obs_ncgoesabi (iv, satellite_id)

   implicit none

! 1.0 Read locs, parse, and select NC files: identify files for channels, views, times, overlap w/ patch/domain
!----------------------------------------------------------------------------------------------------------
! 2.0 Read (BT) NC files: grab radiance data and convert to BT (K)
!----------------------------------------------------------------------------------------------------------
!
! JJG:  NEED TO ADD A MORE COMPLETE DESCRIPTION HERE
!

   !These libraries must be linked: netcdf, mpi

   !!These externally defined variables/routines are used herein:
   ! cpp: DM_PARALLEL
   ! PARALLELIZATION: ntasks_x, ntasks_y, num_procs, myproc, comm, ierr, true_mpi_real
   ! RADIANCE OPERATOR: rtminit_nsensor, rtminit_platform, rtminit_sensor, rtminit_satid
   ! THINNING: thinning_grid
   ! GENERAL OBS: num_fgat_time, time_slots
   ! WRFDA types: iv_type, datalink_type, info_type, model_loc_type
   ! WRFDA subs: da_get_satzen, da_llxy, da_get_unit, da_free_unit, da_get_julian_time
   !             da_trace_entry, da_trace_exit
   ! precisions: r_kind, i_kind

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
   integer                      :: ny_global, nx_global
   integer                      :: yoff_fd, xoff_fd
   ! For MPI parallelization
   integer                      :: nrad_mask, nrad_buf
   integer                      :: ny_local, nx_local
   integer                      :: ys_local, xs_local

   !! Earth location info
   real, allocatable            :: yy(:), xx(:)
   real(r_kind)                 :: req, rpol, pph, nam
!!!   real                         :: lat_sat, lon_sat ! Assume fixed values in da_get_satzen
   real, allocatable, target    :: buf_real(:,:)
   integer, allocatable, target :: buf_int(:,:)


   ! Temporary data fields for assigning radiance locations to local patches
   type data_field
      real, pointer :: local_r(:,:)
      real, pointer :: mask_r(:)
      real, pointer :: remote_r(:)
      integer, pointer :: local_i(:,:)
      integer, pointer :: mask_i(:)
      integer, pointer :: remote_i(:)
   end type data_field

   type(data_field)             :: lat_f, lon_f, satzen_f, &
                                   modj_f, modi_f, &
                                   obsj_f, obsi_f

   ! Masks for data reduction
   logical, allocatable         :: &
      allmask_patch(:,:) , &
      allmask_local(:,:) , &
      earthmask(:,:), zenmask(:,:), domainmask(:,:), &
      thinmask(:,:)

   logical, allocatable :: view_mask(:,:,:,:,:)

   logical                      :: use_view_mask, best_view


   ! Brightness Temperature (K)
   real, allocatable            :: bt_patch(:,:)

   !! Iterates
   integer                      :: ichan, ifile, iview, ifgat, ipass, ioff, &
                                   jchan, jfile, jview, &
                                   n, i, j, iy, ix, iyl, ixl, iproc

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
   end type date_type

   ! Linked list type for radiance location information
   type viewlist
      real    :: lat, lon, satzen
      integer :: iy, ix
      type(viewlist), pointer :: next
      integer :: i
   end type viewlist

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
      integer                          :: ny_global, nx_global, yoff_fd, xoff_fd
      integer                          :: ny_local, nx_local
      integer                          :: ys_local, xs_local
      integer                          :: ys_patch, xs_patch
      integer                          :: ye_patch, xe_patch
      integer                          :: ys_patch_fd, xs_patch_fd
      integer                          :: ye_patch_fd, xe_patch_fd
      integer                          :: nrad_on_patch, nrad_on_domain
      logical, allocatable             :: patchmask(:,:)
      type(viewlist), pointer          :: head
      type(viewlist), pointer          :: current
      character(len=2)                 :: name_short
      character(len=10)                :: name
      logical                          :: moving
   end type viewinfo

   type(viewinfo), target, allocatable :: view_att(:)
   type(viewinfo), pointer             :: this_view
   type(viewlist), pointer             :: this_obslist

   integer                             :: first_file, tot_files_used, npass
   integer                             :: ncid, varid

   !! WRFDA channel and satellite_id select
   !! These should be inputs to the subroutine or global variables in WRFDA
   !Could populate using .info file.  Would reduce number of files to read...
   integer, dimension(10)              :: channel_select = (/7, 8, 9, 10, 11, 12, 13, 14, 15, 16/)

   ! Global WRFDA obs timing info
   character(len=19)                   :: fgat_times_c(num_fgat_time)
   real(r_kind)                        :: fgat_times_r(num_fgat_time)

   ! Local Obs date/time variables
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

   view_att(1)%moving = .false.
   view_att(2)%moving = .false.
   view_att(3)%moving = .true.
   view_att(4)%moving = .true.

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
   use_view_mask = .false.

   do ipass = 1, npass
      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt='(A,I0,A,I2.2,A)') &
         'Starting pass ',ipass,&
         ' of GOES-',satellite_id,' data processing'

   !! Loop over the available views for this instrument (ABI)
   do iview = 1, nviews
      this_view => view_att(iview)

      !Initialize linked list for obs in this view
      if (ipass .eq. 1) then
         allocate(view_att(iview)%head)
         view_att(iview)%head%i = 0
      end if
      ! Associate this_obslist
      this_obslist => view_att(iview)%head

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

         this_view%file_fgat_match = .false.
         do ifgat=1,num_fgat_time
            this_view%fgat_time_diff(:,ifgat) = &
               (time_slots(ifgat) - time_slots(ifgat-1)) * 60.D0 !seconds

            this_view%min_time_diff(:,ifgat) = &
               (time_slots(ifgat) - time_slots(ifgat-1)) * 60.D0 / 2.D0 !seconds
         end do

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
            call da_get_julian_time(yr,mt,dy,hr,mn,timbdy(1))

            !obs END time
            ioff = ioff + 16
            read(fname(1+ioff:4+ioff),fmt='(I4.4)') yr
            read(fname(5+ioff:7+ioff),fmt='(I3.3)') jdy
            read(fname(8+ioff:9+ioff),fmt='(I2.2)') hr
            read(fname(10+ioff:11+ioff),fmt='(I2.2)') mn
            read(fname(12+ioff:13+ioff),fmt='(I2.2)') sc
            call jday2cal(jdy, yr, mt, dy)
            call da_get_julian_time(yr,mt,dy,hr,mn,timbdy(2))

            obs_time=(timbdy(1) + timbdy(2)) / 2.D0
            obs_time = obs_time + real(sc,8)/60.D0

!! The time it takes to read time_bounds from each file is not insignificant.  Above method is much faster.
!            !! Determine central date of this file for obs binning
!!!            ierr=nf_open(trim(this_view%fpath)//trim(fname),nf_nowrite,ncid)
!!!            ierr=nf_inq_varid(ncid,'time_bounds',varid)
!!!            ierr=nf_get_var_double(ncid,varid,timbdy)
!!!            ierr=nf_close(ncid)
!!!            j2000=(timbdy(1) + timbdy(2)) / 2.D0 /86400.D0

            call da_get_cal_time(obs_time,yr,mt,dy,hr,mn,sc)
            obs_time = obs_time * 60.D0

            this_view%filedate(ifile)%yr = yr
            this_view%filedate(ifile)%mt = mt
            this_view%filedate(ifile)%dy = dy
            this_view%filedate(ifile)%hr = hr
            this_view%filedate(ifile)%mn = mn
            this_view%filedate(ifile)%sc = sc

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
            ! Only ny_global and nx_global need to be read for all views, but this is a cheap subroutine
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            write(unit=stdout,fmt='(2A)') &
               '          Reading abi metadata for ',trim(this_view%name)

            call get_abil1b_metadata( &
               fname, this_view%ny_global, this_view%nx_global, req, rpol, pph, nam)! , lat_sat, lon_sat )

write(stdout,fmt=*) 'TEST2'
write(stdout,fmt=*) this_view%ny_global, this_view%nx_global, req, rpol, pph, nam

#ifdef DM_PARALLEL
            ! Split the global ABI grid for this view into local segments
            call split_grid( this_view%ny_global, this_view%nx_global , &
                             this_view%ny_local,  this_view%nx_local  , &
                             this_view%ys_local,  this_view%xs_local  , &
                             (iview.eq.1) )

write(stdout,fmt=*) 'ntasks_y, ntasks_x, num_procs, myproc = ', ntasks_y, ntasks_x, num_procs, myproc


write(stdout,fmt=*) 'TEST3'

#else
            ! When mpi parallelism is not available, assign global values to local variables
            this_view%ny_local = this_view%ny_global
            this_view%nx_local = this_view%nx_global
            this_view%ys_local = 1
            this_view%xs_local = 1
#endif

write(stdout,fmt=*) 'ny_local, nx_local, ys_local, xs_local = ', &
                     this_view%ny_local, this_view%nx_local, this_view%ys_local, this_view%xs_local

write(stdout,fmt=*) 'TEST4'

         end if

         ! Recall global dims for this_view
         ny_global = this_view%ny_global
         nx_global = this_view%nx_global

write(stdout,fmt=*) 'TEST5'

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !! Generate grid locations if
         !!  + CONUS or FD and first matching fgat
         !!  + MESO and any fgat (extent changes in time)
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if (  ( .not.this_view%moving .and. sum(this_view%nfiles_used(:)).eq.0 ) &
                 .or. this_view%moving ) then

!            if ( ipass.eq.2 .and. iview .eq. 1 ) then
!               ! Restore FD attributes from memory
!               this_view%yoff_fd = yoff_fd
!               this_view%xoff_fd = xoff_fd
!            end if

            if ( ipass.eq.1 .or. this_view%moving ) then
write(stdout,fmt=*) 'TEST6'

               ! Read grid from file, convert to lat, lon, satzen
               write(unit=stdout,fmt='(2A)') &
                  '          Reading abi grid info for ',trim(this_view%name)


               !==================================================
               ! Establish GOES metadata for this view and ifgat
               !==================================================
               allocate( yy(ny_global) )
               allocate( xx(nx_global) )
               call get_abil1b_grid1( fname, &
                                      ny_global, nx_global, &
                                      yy, xx, &
                                      this_view%yoff_fd, this_view%xoff_fd )

               if ( iview.eq.1 ) then
                  yoff_fd = this_view%yoff_fd
                  xoff_fd = this_view%xoff_fd
                  this_view%yoff_fd = 1
                  this_view%xoff_fd = 1
               else
                  this_view%yoff_fd = this_view%yoff_fd - yoff_fd
                  this_view%xoff_fd = this_view%xoff_fd - xoff_fd
!                  this_view%yoff_fd = this_view%yoff_fd - yoff_fd + 1
!                  this_view%xoff_fd = this_view%xoff_fd - xoff_fd + 1
               end if

write(stdout,fmt=*) 'TEST8'

!Currently load balancing is a good start, but not very effective (see split_grid)
!Is there some way to subdivide the grid into smaller pieces and process in a round-robin fashion? Would need to send messages between processors to keep track of subsections that were already processed.  Fields would need to be more flexible than simple 2d/1d arrays, possibly linked lists.

!This allocation and subroutine are entirely independent for specified local segments
!BEGIN PARALLEL SECTION

               !==========================================
               ! Establish fields for local subset of 
               !  radiance locations in this view
               !==========================================
               ! Recall local dims for this_view
               ny_local = this_view%ny_local
               nx_local = this_view%nx_local
               ys_local = this_view%ys_local
               xs_local = this_view%xs_local

               !! Allocate local obs spatial fields for this view
               allocate(lat_f%local_r(ny_local,nx_local))
               allocate(lon_f%local_r(ny_local,nx_local))
               allocate(satzen_f%local_r(ny_local,nx_local))
               allocate(obsj_f%local_i(ny_local,nx_local))
               allocate(obsi_f%local_i(ny_local,nx_local))

               !! Allocate local mask fields for this view
               allocate(earthmask(ny_local,nx_local))
               allocate(zenmask(ny_local,nx_local))

               !! Allocate local model-specific fields
               allocate(modj_f%local_i(ny_local,nx_local))
               allocate(modi_f%local_i(ny_local,nx_local))
               allocate(domainmask(ny_local,nx_local))

               do iy = 1, ny_local
                  obsj_f%local_i(iy,:) = iy
               end do
               do ix = 1, nx_local
                  obsi_f%local_i(:,ix) = ix
               end do

               call get_abil1b_grid2( ny_global, nx_global, ny_local, nx_local, ys_local, xs_local, &
                                      yy, xx, req, rpol, pph, nam, satellite_id, &
                                      lat_f%local_r, lon_f%local_r, satzen_f%local_r, &
                                      earthmask, zenmask, &
                                      modj_f%local_i, modi_f%local_i, domainmask)


write(stdout,fmt=*) 'TEST8'


               !==========================================================
               ! Reduce local locations using all available masks
               !==========================================================

write(stdout,fmt=*) 'TEST9'

               allocate(allmask_local(ny_local,nx_local))
               allmask_local = &
                   (earthmask .and. zenmask .and. domainmask)

               nrad_mask = count( allmask_local )

               allocate( lat_f%mask_r    (nrad_mask) )
               allocate( lon_f%mask_r    (nrad_mask) )
               allocate( satzen_f%mask_r (nrad_mask) )
               allocate( modj_f%mask_i   (nrad_mask) )
               allocate( modi_f%mask_i   (nrad_mask) )
               allocate( obsj_f%mask_i   (nrad_mask) )
               allocate( obsi_f%mask_i   (nrad_mask) )

               lat_f%mask_r    = pack( lat_f%local_r    , allmask_local )
               lon_f%mask_r    = pack( lon_f%local_r    , allmask_local )
               satzen_f%mask_r = pack( satzen_f%local_r , allmask_local )
               modj_f%mask_i   = pack( modj_f%local_i   , allmask_local )
               modi_f%mask_i   = pack( modi_f%local_i   , allmask_local )
               obsj_f%mask_i   = pack( obsj_f%local_i   , allmask_local )
               obsi_f%mask_i   = pack( obsi_f%local_i   , allmask_local )

               deallocate( lat_f%local_r, lon_f%local_r, satzen_f%local_r )
               deallocate( modj_f%local_i, modi_f%local_i )
               deallocate( allmask_local, earthmask, zenmask )
               deallocate( obsj_f%local_i, obsi_f%local_i, domainmask )

!END PARALLEL SECTION


               deallocate( yy, xx )


               !=======================================================
               ! Reduce all masked locations (local and remote)
               !  to linked list within this WRF patch (this_obslist)
               !=======================================================

               ! Destroy this_obslist if it was previously populated
               if (this_obslist%i .gt. 0) then
                  n = this_obslist%i
                  this_obslist => view_att(iview)%head%next
                  do i = 1, n
                     view_att(iview)%current => this_obslist
                     this_obslist => this_obslist%next

                     ! free current data
                     deallocate ( view_att(iview)%current )
                  end do
                  ! Reassociate this_obslist
                  this_obslist => view_att(iview)%head
               end if

               ! Setup global patch mask for this view
               allocate(this_view%patchmask(ny_global,nx_global))
               this_view%patchmask = .false.
               this_view%nrad_on_domain = 0

write(stdout,fmt=*) 'TEST10'

               do iproc = 0, num_procs-1
                  nrad_buf = nrad_mask
#ifdef DM_PARALLEL
                  call mpi_bcast(nrad_buf, 1, mpi_integer, iproc, comm, ierr )
#endif
#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif
                  if (nrad_buf .eq. 0) cycle

write(stdout,fmt=*) 'TEST14', nrad_buf

                  !BCAST REMOTE MASKED FIELDS FOR PROCESSING
                  allocate( buf_real( nrad_buf, 3 ) )
                  allocate( buf_int(  nrad_buf, 4 ) )

                  if (iproc .eq. myproc) then
                     buf_real(:,1) = lat_f%mask_r
                     buf_real(:,2) = lon_f%mask_r
                     buf_real(:,3) = satzen_f%mask_r
                     buf_int (:,1) = modj_f%mask_i
                     buf_int (:,2) = modi_f%mask_i
                     buf_int (:,3) = obsj_f%mask_i
                     buf_int (:,4) = obsi_f%mask_i
                  else
                     buf_real = missing_r
                     buf_int = missing
                  end if
#ifdef DM_PARALLEL
                  call mpi_bcast(buf_real, nrad_buf * 3, true_mpi_real, iproc, comm, ierr )
                  call mpi_bcast(buf_int,  nrad_buf * 4, mpi_integer, iproc, comm, ierr )
#endif

write(stdout,fmt=*) 'TEST11', iproc

                  lat_f%remote_r    => buf_real(:,1)
                  lon_f%remote_r    => buf_real(:,2)
                  satzen_f%remote_r => buf_real(:,3)
                  modj_f%remote_i   => buf_int(:,1)
                  modi_f%remote_i   => buf_int(:,2)
                  obsj_f%remote_i   => buf_int(:,3)
                  obsi_f%remote_i   => buf_int(:,4)

                  this_view%nrad_on_domain = this_view%nrad_on_domain + nrad_buf
                  do n = 1, nrad_buf
#ifdef DM_PARALLEL
                     loc%j = modj_f%remote_i(n)
                     loc%i = modi_f%remote_i(n)
                     call da_llxy (info, loc, outside, patch_test_only = .true.)
                     if (outside) cycle
#endif
                     iy = obsj_f%remote_i(n)
                     ix = obsi_f%remote_i(n)
                     this_view%patchmask(iy,ix) = .true.

                     allocate(this_obslist%next)
                     i = this_obslist%i
                     this_obslist => this_obslist%next
                     this_obslist%i = i + 1
                     this_obslist%lat = lat_f%remote_r(n)
                     this_obslist%lon = lon_f%remote_r(n)
                     this_obslist%satzen = satzen_f%remote_r(n)
                     this_obslist%iy = iy
                     this_obslist%ix = ix

write(stdout,fmt=*) 'TEST18', iy, iproc, this_obslist%i

                  end do

                  deallocate( buf_real, buf_int)

write(stdout,fmt=*) 'TEST19', iproc, this_obslist%i

               end do

               deallocate( lat_f%mask_r, lon_f%mask_r, satzen_f%mask_r, modj_f%mask_i, modi_f%mask_i, obsj_f%mask_i, obsi_f%mask_i)

               this_view%nrad_on_patch = this_obslist%i

               if ( this_view%nrad_on_patch.gt.0 ) then

                  ! Determine ys & ye for this patch
                  this_view%ys_patch = ny_global
                  this_view%ye_patch = 1
                  do iy = 1, ny_global
                     if ( any(this_view%patchmask(iy,:)) ) then
                        this_view%ys_patch = iy
                        this_view%ys_patch_fd = iy+this_view%yoff_fd-1  ! offset to FD grid
                        exit
                     end if
                  end do
                  do iy = ny_global, 1, -1
                     if ( any(this_view%patchmask(iy,:)) ) then
                        this_view%ye_patch = iy
                        this_view%ye_patch_fd = iy+this_view%yoff_fd-1  ! offset to FD grid
                        exit
                     end if
                  end do

                  ! Determine xs & xe for this patch
                  this_view%xs_patch = nx_global
                  this_view%xe_patch = 1
                  do ix = 1, nx_global
                     if ( any(this_view%patchmask(:,ix)) ) then
                        this_view%xs_patch = ix
                        this_view%xs_patch_fd = ix+this_view%xoff_fd-1  ! offset to FD grid
                        exit
                     end if
                  end do
                  do ix = nx_global, 1, -1
                     if ( any(this_view%patchmask(:,ix)) ) then
                        this_view%xe_patch = ix
                        this_view%xe_patch_fd = ix+this_view%xoff_fd-1  ! offset to FD grid
                        exit
                     end if
                  end do

               end if
            end if

write(stdout,fmt=*) 'TEST24'

         end if

         PatchMatch: if (this_view%nrad_on_patch .gt. 0) then
write(stdout,fmt=*) 'TEST26'

            if ( iview.eq.1 .and. ipass.lt.npass .and. sum(this_view%nfiles_used(:)).eq.0 ) then
               allocate(view_mask(&
                   this_view%ys_patch_fd:this_view%ye_patch_fd, &
                   this_view%xs_patch_fd:this_view%xe_patch_fd, &
                   nchan, num_fgat_time, nviews))

               view_mask = .false.
               use_view_mask = .true.

write(stdout,fmt=*) size(view_mask)
write(stdout,fmt=*) sizeof(view_mask)
            end if

write(stdout,fmt=*) 'TEST27'

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

!               use_view_mask = ( sum(view_att(1)%nfiles_used(:)).gt.0 )

               VIEW_SELECT: &
               if ( ipass.lt.npass .and. use_view_mask ) then
                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  !! Determine which view has the closest observed 
                  !!  time to fgat for this channel
                  !!  Note: this only needs to be done for a single channel, 
                  !!    unless individual channel files are missing at fgat.
                  !!    Solution where file view availability differs by channel used here.
                  !!  (only available when FD data present for one of the fgat times)
                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  if ( iview.eq.1 ) then
                     view_mask(:,:, ichan, ifgat, iview) = .true.
                  else
                     best_view = .true.
!                     do jview = 1, iview-1 !This assumes MESO1 and MESO2 are in identical locations
                     do jview = 1, min(iview-1,2) !This assumes MESO1 and MESO2 do not overlap
                        best_view = best_view .and. &
                           this_view%min_time_diff(ichan, ifgat) .lt. &
                              view_att(jview)%min_time_diff(ichan, ifgat)
                     end do
                     
                     if ( best_view ) then

                        view_mask(this_view%ys_patch_fd:this_view%ye_patch_fd, &
                                    this_view%ys_patch_fd:this_view%ye_patch_fd, &
                                    ichan, ifgat, iview) = .true.

!                        do jview = 1, iview-1 !This assumes MESO1 and MESO2 are in identical locations
                        do jview = 1, min(iview-1,2) !This assumes MESO1 and MESO2 do not overlap

                           view_mask(this_view%ys_patch_fd:this_view%ye_patch_fd, &
                                       this_view%ys_patch_fd:this_view%ye_patch_fd, &
                                       ichan, ifgat, jview) = .false.
                        end do
                     end if

                  end if

               else
                  if (inst == 0) cycle

                  fname_short = trim(this_view%filename(ifile))
                  fname = trim(this_view%fpath)//trim(fname_short)


                  !!Utilizing these masks to eliminate data:
                  !! + earthmask
                  !! + zenmask
                  !! + view_mask [only if npass > 1]
                  !! + model domain mask
                  !! + patch mask
                  !! + thinning

                  allocate(allmask_patch(ny_global,nx_global))
                  allmask_patch = this_view%patchmask

                  ! Only use locations where this view is nearest to this fgat time
                  ! - only available when FD data present for any fgat time
                  if (  use_view_mask ) then
                     if ( count(view_mask(this_view%ys_patch_fd:this_view%ye_patch_fd, &
                                    this_view%ys_patch_fd:this_view%ye_patch_fd, &
                                    ichan, ifgat, iview)) .eq. 0 ) then
                        deallocate(allmask_patch)
                        cycle
                     end if

                     allmask_patch(this_view%ys_patch:this_view%ye_patch , &
                                   this_view%ys_patch:this_view%ye_patch ) = ( &
                        allmask_patch(this_view%ys_patch:this_view%ye_patch , &
                                      this_view%ys_patch:this_view%ye_patch ) &
                        .and. &
                        view_mask(this_view%ys_patch_fd:this_view%ye_patch_fd, &
                                    this_view%ys_patch_fd:this_view%ye_patch_fd, &
                                    ichan, ifgat, iview) )
                  end if

                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  !! Read radiance and convert to brightness temp.
                  !! once per permutation of 
                  !! + INST VIEW (FD, CONUS, MESOx2)
                  !! + fgat
                  !! + channel/band
                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  write(unit=stdout,fmt='(2A)') &
                     '          Reading abi radiances: ',trim(fname_short)

                  ! Allocate this patch bt
                  allocate(bt_patch(this_view%ys_patch:this_view%ye_patch, &
                              this_view%xs_patch:this_view%xe_patch))

                  ! This reads in bt only for the local patch,
                  !  reduces read time, but would mess up global count below
                  call get_abil1b_bt( fname, &
                                      ny_global, nx_global, &
                                      this_view%ys_patch, this_view%ye_patch, &
                                      this_view%xs_patch, this_view%xe_patch, &
                                      allmask_patch, bt_patch )


                  !! Write bt, lat, lon, and satzen to datalink structures

                  first_chan = (this_view%nfiles_used(ifgat).eq.1)
                  if (first_chan) then
                     p_fgat => p

                     yr = this_view%filedate(ifile)%yr
                     mt = this_view%filedate(ifile)%mt
                     dy = this_view%filedate(ifile)%dy
                     hr = this_view%filedate(ifile)%hr
                     mn = this_view%filedate(ifile)%mn
                     sc = this_view%filedate(ifile)%sc
                     num_goesabi_global = num_goesabi_global + this_view%nrad_on_domain
                     ptotal(ifgat) = ptotal(ifgat) + this_view%nrad_on_domain

                     allocate(thinmask(this_view%ys_patch:this_view%ye_patch, &
                                       this_view%xs_patch:this_view%xe_patch))
                     thinmask = .false.
                  else
                     p => p_fgat
                  end if

                  this_obslist => view_att(iview)%head

                  do n = 1, this_view%nrad_on_patch

                     this_obslist => this_obslist%next

                     iy = this_obslist%iy
                     ix = this_obslist%ix

                     if (.not. allmask_patch(iy,ix)) cycle

                     if (first_chan) then
                        info%lat = this_obslist%lat ! latitude
                        info%lon = this_obslist%lon ! longitude
                        num_goesabi_local = num_goesabi_local + 1
                     end if

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
                        if (use_view_mask) then
                           p%scanpos               = &
                              (iy + this_view%yoff_fd-1 - 1) * (nscan+1) / view_att(1)%ny_global
                              ! ??? "scan" position (IS THIS CORRECT?)
                        else
                           p%scanpos               = &
                              (iy + this_view%yoff_fd-1 - 1) * (nscan+1) / 5423
                              ! ??? "scan" position (IS THIS CORRECT?)
                        end if
                        p%satzen                = this_obslist%satzen
                        p%solzen                = 0.0
                        p%sensor_index          = inst
                        p%ifgat                 = ifgat
                     end if

                     ! Transfer BT from all files
                     p%tb_inv(ichan)         = bt_patch(iy,ix)

                     if (first_chan) &
                        allocate (p%next)   ! add next data

                     p => p%next

                     if (first_chan) &
                        nullify (p%next)

                  end do
                  deallocate( bt_patch, allmask_patch )
               end if VIEW_SELECT
            end do ChannelLoop

            if (allocated(thinmask)) deallocate(thinmask)

         end if PatchMatch

!#ifdef DM_PARALLEL
!   call mpi_barrier(comm, ierr)
!#endif
      end do ! end fgat loop

      if (this_view%moving .or. ipass.eq.npass) then
         ! Deallocate static data
         if (allocated(this_view%patchmask)) deallocate(this_view%patchmask)
      end if

      if (ipass.eq.npass) then
         if (this_obslist%i .gt. 0) then
            ! Destroy this_obslist and head
            n = this_obslist%i
            this_obslist => view_att(iview)%head%next
            do i = 1, n
               view_att(iview)%current => this_obslist
               this_obslist => this_obslist%next

               ! free current data
               deallocate ( view_att(iview)%current )
            end do
            deallocate(view_att(iview)%head)
         end if
      end if

      tot_files_used = tot_files_used + sum(view_att(iview)%nfiles_used)

   end do ! end view loop
   
      if (tot_files_used .lt. 1) then
         write(unit=message(1),fmt='(A,I2,2A)') "No L1B data found for GOES-",satellite_id," using prefix ",INST_PREFIX
         call da_warning(__FILE__,__LINE__, message(1:1))
         return
      end if
   end do ! end pass loop

   if (allocated(view_mask)) deallocate(view_mask)

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_ichan(channel, channel_list, nchan, ichan) !result(ichan)

   implicit none

   integer, intent(in)  :: channel, nchan
   integer, intent(in)  :: channel_list(nchan)
   integer, intent(out) :: ichan
   integer :: i

   if (trace_use) call da_trace_entry("get_ichan")

   ichan = 0
   do i = 1, nchan
      if (channel .eq. channel_list(i)) then
         ichan = i
         exit
      end if
   end do

   if (trace_use) call da_trace_exit("get_ichan")

end subroutine get_ichan

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_metadata( filename, &
                                   ydim, xdim, req, rpol, pph, nam) !, lat_sat, lon_sat )

   implicit none

   character(*), intent(in)  :: filename

   integer, intent(out)      :: ydim, xdim
   real(r_kind), intent(out) :: req, rpol, pph, nam
!!!   real, intent(out)         :: lat_sat, lon_sat

   integer                  :: ierr, ncid, varid, dimid
   real(r_kind), parameter  :: pi=3.1415926535898D0

   if (trace_use) call da_trace_entry("get_abil1b_metadata")

   ierr=nf_open(trim(filename),nf_nowrite,ncid)
   call handle_err('Error opening file',ierr)

   !! Determine ABI satellite parameters (optional outputs)
   ierr=nf_inq_dimid(ncid,'y',dimid)
   ierr=nf_inq_dimlen(ncid,dimid,ydim)
   ierr=nf_inq_dimid(ncid,'x',dimid)
   ierr=nf_inq_dimlen(ncid,dimid,xdim)

   ierr=nf_inq_varid(ncid,'goes_imager_projection',varid)
   ierr=nf_get_att_double(ncid,varid,'semi_major_axis',req)
   ierr=nf_get_att_double(ncid,varid,'semi_minor_axis',rpol)
   ierr=nf_get_att_double(ncid,varid,'perspective_point_height',pph)
   ierr=nf_get_att_double(ncid,varid,'longitude_of_projection_origin',nam)
   nam=nam*pi/180

!!!   ierr=nf_inq_varid(ncid,'nominal_satellite_subpoint_lat',varid)
!!!   ierr=nf_get_var_double(ncid,varid,lat_sat)
!!!   ierr=nf_inq_varid(ncid,'nominal_satellite_subpoint_lon',varid)
!!!   ierr=nf_get_var_double(ncid,varid,lon_sat)

   ierr=nf_close(ncid)
   call handle_err('Error closing file',ierr)

   if (trace_use) call da_trace_exit("get_abil1b_metadata")

end subroutine get_abil1b_metadata

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_grid1( filename, &
                             ny, nx, &
                             xx, yy, &
                             yoff, xoff )

   implicit none

   character(*), intent(in)  :: filename
   integer,      intent(in)  :: ny, nx
   integer,      intent(out) :: yy(ny), xx(nx)
   integer,      intent(out) :: yoff, xoff

   integer                  :: ierr, ncid, varid
   real                     :: slp, itp

   if (trace_use) call da_trace_entry("get_abil1b_grid1")

   ierr=nf_open(trim(filename),nf_nowrite,ncid)
   call handle_err('Error opening file',ierr)

   ierr=nf_inq_varid(ncid,'y',varid)

   ierr=nf_get_var_double(ncid,varid,yy)

   ierr=nf_get_att_double(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_double(ncid,varid,'add_offset',itp)
   yy = yy*slp+itp 
   yoff = floor(itp/slp)

   ierr=nf_inq_varid(ncid,'x',varid)

   ierr=nf_get_var_double(ncid,varid,xx)

   ierr=nf_get_att_double(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_double(ncid,varid,'add_offset',itp)
   xx = xx*slp+itp
   xoff = floor(itp/slp)

   ierr=nf_close(ncid)
   call handle_err('Error closing file',ierr)

   if (trace_use) call da_trace_exit("get_abil1b_grid1")

end subroutine get_abil1b_grid1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_grid2( ny, nx, nyl, nxl, ys, xs, &
                             yy, xx, req, rpol, pph, nam, satellite_id, &
                             lat, lon, satzen, &
                             earthmask, zenmask, &
                             modj, modi, domainmask )

   implicit none

   integer,           intent(in)  :: ny, nx, nyl, nxl, ys, xs
   real,              intent(in)  :: yy(ny), xx(nx)
   real(r_kind),      intent(in)  :: req, rpol, pph, nam
   integer,           intent(in)  :: satellite_id

   ! GOES-ABI fields
   real,              intent(out) :: lat(nyl,nxl), lon(nyl,nxl), satzen(nyl,nxl)
   logical,           intent(out) :: earthmask(nyl,nxl), zenmask(nyl,nxl)

   ! Model-specific fields
   integer, optional, intent(out) :: modj(nyl,nxl), modi(nyl,nxl)
   logical, optional, intent(out) :: domainmask(nyl,nxl)

   ! Internal Variables
   type(info_type)          :: info
   type(model_loc_type)     :: loc
   logical                  :: outside_all, dummy_bool

   integer                  :: iy, ix, iyl, ixl
   real(r_kind)             :: hh
   real, parameter          :: satzen_limit=75.0

   if (trace_use) call da_trace_entry("get_abil1b_grid2")

   hh=pph+req

   lat = missing_r
   lon = missing_r
   satzen = missing_r

   earthmask=.false.
   zenmask=.false.

   do ixl = 1, nxl
      ix = ixl + xs - 1
      do iyl = 1, nyl
         iy = iyl + ys - 1

         call get_abil1b_latlon(yy(iy),xx(ix),req,rpol,hh,nam,lat(iyl,ixl),lon(iyl,ixl))

         if( lat(iyl,ixl).eq.missing_r .OR. lon(iyl,ixl).eq.missing_r ) cycle

         earthmask(iyl,ixl)=.true.

         call da_get_satzen(lat(iyl,ixl),lon(iyl,ixl),satellite_id,satzen(iyl,ixl))

         if (satzen(iyl,ixl).gt.satzen_limit) then
            satzen(iyl,ixl) = missing_r
            cycle
         end if
         zenmask(iyl,ixl)=.true.
      end do
   end do

   !Populate domainmask, modi, modj with model coordinate utility
   if ( present(modj) .and. present(modi) .and. present(domainmask) ) then
      modj = missing
      modi = missing
      domainmask = .false.
      do ixl = 1, nxl
         do iyl = 1, nyl
            if (earthmask(iyl,ixl)) then
               info%lon  =  lon(iyl,ixl)  ! longitude
               info%lat  =  lat(iyl,ixl)  ! latitude
               call da_llxy (info, loc, dummy_bool, outside_all)
               if (.not. outside_all) then
                  modj(iyl,ixl) = loc%j
                  modi(iyl,ixl) = loc%i
                  domainmask(iyl,ixl) = .true.
               end if
            end if
         end do
      end do
   end if

   if (trace_use) call da_trace_exit("get_abil1b_grid2")

end subroutine get_abil1b_grid2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_bt( filename, ny, nx, ys, ye, xs, xe, &
                             radmask, bt )
   implicit none

   character(*), intent(in)      :: filename

   !Size of full data set
   integer, intent(in)           :: ny, nx

   !Starting and stopping indices of this view desired (not equivalent to Full Disk indices)
   integer, intent(in)           :: ys, ye, xs, xe 

   logical, intent(inout)        :: radmask(ny, nx)
   real, intent(out)             :: bt(ys:ye, xs:xe)

   real             :: rad(ys:ye, xs:xe)
   integer(kind=1)  :: DQF(ys:ye, xs:xe)
   integer          :: ierr, ncid, varid
   integer          :: iy, ix
   integer          :: nykeep, nxkeep
   real             :: slp, itp
   real             :: bc1, bc2, fk1, fk2

   if (trace_use) call da_trace_entry("get_abil1b_bt")

   bt = missing_r

   !! Save rad reading time by selecting a subset of netcdf var
   nykeep = ye - ys + 1
   nxkeep = xe - xs + 1

   if (nykeep.gt.0 .and. nxkeep.gt.0) then
      ierr=nf_open(trim(filename),nf_nowrite,ncid)
      call handle_err('Error opening file',ierr)

      ierr=nf_inq_varid(ncid,'Rad',varid)
      ierr=nf_get_vara_double(ncid,varid,(/ys,xs/),(/nykeep,nxkeep/), &
                               rad(ys:ye,xs:xe) )

      ierr=nf_inq_varid(ncid,'DQF',varid)
      ierr=nf_get_vara_int(ncid,varid,(/ys,xs/),(/nykeep,nxkeep/), &
                               DQF(ys:ye,xs:xe) )
   else
      return
   end if

   ierr=nf_get_att_double(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_double(ncid,varid,'add_offset',itp)
   rad=rad*slp+itp

   ierr=nf_inq_varid(ncid,'planck_bc1',varid)
   ierr=nf_get_var_double(ncid,varid,bc1)
   ierr=nf_inq_varid(ncid,'planck_bc2',varid)
   ierr=nf_get_var_double(ncid,varid,bc2)
   ierr=nf_inq_varid(ncid,'planck_fk1',varid)
   ierr=nf_get_var_double(ncid,varid,fk1)
   ierr=nf_inq_varid(ncid,'planck_fk2',varid)
   ierr=nf_get_var_double(ncid,varid,fk2)

   do ix=xs, xe
      do iy=ys, ye
         if ( radmask(iy,ix) ) then
            if( rad(iy,ix).ge.0.0 .and. any(DQF(iy,ix).eq.(/0,1/)) ) then
               bt(iy,ix)=(fk2/(alog((fk1/rad(iy,ix))+1.))-bc1)/bc2
            else
               radmask(iy,ix) = .true.
            end if
         end if
      end do
   end do

!#ifdef DM_PARALLEL
!   call mpi_barrier(comm, ierr)
!#endif

   ierr=nf_close(ncid)
   call handle_err('Error closing file',ierr)

   if (trace_use) call da_trace_exit("get_abil1b_bt")

end subroutine get_abil1b_bt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_latlon(yy,xx,req,rpol,hh,nam,lat,lon)

   implicit none

   real, intent(in)    :: yy, xx
   real, intent(in)    :: req, rpol, hh, nam
   real, intent(inout) :: lat,lon

   real   :: lat1,lon1
   real   :: aa,bb,cc,rs,sx,sy,sz
   real   :: radicand

   if (trace_use) call da_trace_entry("get_abil1b_latlon")

   aa=sin(xx)**2+cos(xx)**2*(cos(yy)**2+req**2/rpol**2*sin(yy)**2)
   bb=-2.D0*hh*cos(xx)*cos(yy)
   cc=hh**2-req**2

   radicand = bb**2 - 4.D0*aa*cc
   if (radicand .lt. 0.) return

   rs=(-bb-sqrt(radicand))/(2.D0*aa)
   sx=rs*cos(xx)*cos(yy)
   sy=-rs*sin(xx)
   sz=rs*cos(xx)*sin(yy)

   lat1=atan(req**2/rpol**2*sz/sqrt((hh-sx)**2+sy**2))
   lon1=nam-atan(sy/(hh-sx))

   lat=lat1*180.D0/pi
   lon=lon1*180.D0/pi

   if (trace_use) call da_trace_exit("get_abil1b_latlon")

end subroutine get_abil1b_latlon

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine split_grid( ny_global, nx_global, &
                       ny_local, nx_local, &
                       ys_local, xs_local, &
                       redist )

   implicit none

   integer, intent(in)  :: ny_global, nx_global
   logical, intent(in)  :: redist
   integer, intent(out) :: ny_local, nx_local, &
                           ys_local, xs_local

   integer, target   :: ny_grid(ntasks_y), ys_grid(ntasks_y)  !, ye_grid(ntasks_y)
   integer, target   :: nx_grid(ntasks_x), xs_grid(ntasks_x)  !, xe_grid(ntasks_x)
   integer, pointer  :: ngrid(:), sgrid(:)

   integer           :: mm, i, j, ii, iproc, igrid, ntasks, nglobal, fact

   do igrid = 1, 2
      if (igrid.eq.1) then
         ngrid => ny_grid
         sgrid => ys_grid
         ntasks = ntasks_y
         nglobal = ny_global
      else if (igrid.eq.2) then
         ngrid => nx_grid
         sgrid => xs_grid
         ntasks = ntasks_x
         nglobal = nx_global
      end if

      ngrid = nglobal / ntasks
      mm = mod( nglobal , ntasks )
      do j = 1, ntasks
         ngrid(j) = ngrid(j) + 1
         mm = mm - 1
         if (mm .eq. 0) exit
      end do

      if (redist) then
         fact = 4
         !Redistribute grid from middle to edges to balance load 
         !  of calls to da_llxy in get_abil1b_grid
         do i = 1, 2
            if (mod(ntasks,2).eq.1) then
               ii = ntasks/2+1
               mm = ngrid(ii) / fact
               mm = mm/2
               ngrid(ii)   = ngrid(ii)   - 2*mm
               ngrid(ii-1) = ngrid(ii-1) + mm
               ngrid(ii+1) = ngrid(ii+1) + mm
            else
               ii = ntasks/2
            end if
            do j = ntasks/2, 2, -1
               mm = ngrid(j) / fact
               ngrid(j)   = ngrid(j)   - mm
               ngrid(j-1) = ngrid(j-1) + mm
            end do
            do j = ii+1, ntasks-1
               mm = ngrid(j) / fact
               ngrid(j)   = ngrid(j)   - mm
               ngrid(j+1) = ngrid(j+1) + mm
            end do
         end do
      end if

      sgrid(1) = 1
      do j = 1, ntasks
!         if (j .eq. 1) egrid(1) = ngrid(1) !NOT NECESSARY
         if (j .lt. ntasks) then
            sgrid(j+1) = sgrid(j) + ngrid(j)
!            egrid(j+1) = egrid(j) + ngrid(j+1) !NOT NECESSARY
         end if
      end do
   end do

   j = myproc / ntasks_x + 1
   ny_local = ny_grid(j)
   ys_local = ys_grid(j)

   i = mod(myproc, ntasks_x) + 1
   nx_local = nx_grid(i)
   xs_local = xs_grid(i)

write(stdout,fmt=*) 'TEST57', myproc, j, i

end subroutine split_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

