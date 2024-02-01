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
   ! WRFDA subs: da_llxy, da_get_julian_time,
   !             da_get_unit, da_free_unit,
   !             da_get_sat_angles(_1d), da_get_solar_angles(_1d)
   !             da_trace_entry, da_trace_exit,
   ! precisions: r_double, i_kind

   type (iv_type),intent (inout) :: iv
   integer, intent(in)           :: satellite_id ! 16 or 17

   type(datalink_type), pointer :: head, p, current, prev, p_fgat
   type(info_type)              :: info
   type(model_loc_type)         :: loc
   integer(i_kind), allocatable :: ptotal(:)
   integer(i_kind)              :: nthinned
   real(r_double)               :: crit
   integer(i_kind)              :: iout, iobs, i_dummy(1)
   logical                      :: outside, outside_all, iuse, first_chan
   logical                      :: found, head_found

   !! ABI Fixed Grid Variables
   integer                      :: ny_global, nx_global
   integer                      :: yoff_fd, xoff_fd
   ! For MPI parallelization
   integer                      :: nbuf, nrad_local, nrad_mask, buf_i, buf_f
   integer, allocatable         :: nbufs(:), displs(:)
   integer                      :: ny_local, nx_local

   !! Earth location info
   real, allocatable            :: yy_abi(:), xx_abi(:)
   real, allocatable            :: yy_1d(:), xx_1d(:)
   real, allocatable            :: iy_1d(:), ix_1d(:)
   real, allocatable            :: solzen_1d(:), solazi_1d(:)

   real(r_double)               :: req, rpol, pph, nam
!!!   real                         :: lat_sat, lon_sat ! Assume fixed values in da_get_sat_angles
   real, allocatable, target    :: buf_real(:,:)
   integer, allocatable, target :: buf_int(:,:)
   type(model_loc_type), allocatable, target :: buf_loc(:)
   type(info_type), allocatable :: info_1d(:)


   ! Masks for data reduction
   logical :: earthmask, zenmask
   logical, allocatable         :: &
      earthmask_1d(:)    , &
      zenmask_1d(:)    , &
      domainmask_1d(:)    , &
      patchmask_1d(:)    , &
      dummybool_2d(:,:)    , &
      allmask_p(:,:) , &
      readmask_p(:,:) , &
      thinmask(:,:)

   logical, allocatable :: view_mask(:,:,:,:,:)

   logical                      :: use_view_mask, best_view


   ! Brightness Temperature (K)
   real, allocatable            :: bt_p(:,:,:), rad_p(:,:,:), terrain_hgt(:,:)
   real                         :: bc1, bc2, fk1, fk2

   !! Iterates
   integer                      :: ichan, ifile, iview, ifgat, ipass, ioff, &
                                   jchan, jfile, jview, icount, io_stat, &
                                   n, i, j, iy, ix, jy, jx, iyl, ixl, iyfd, ixfd, iproc, subgrid, &
                                   isup, jsup, ixsup, iysup
   INTEGER :: cstat, estat
   CHARACTER(LEN=100) :: cmsg
   logical :: exists

   !! Satellite variables
   integer(i_kind),parameter    :: nchan = 10
   integer(i_kind),parameter    :: nscan = 22
   integer, parameter           :: platform_id  = 4   ! GOES series
   integer, parameter           :: sensor_id    = 44  ! ABI
   integer, parameter           :: channel_list(nchan) = (/7,8,9,10,11,12,13,14,15,16/) !List of all available channels
!   integer, parameter           :: channel_index(channel_list(1):channel_list(nchan)) = (/1,2,3,4,5,6,7,8,9,10/) !List of all available channels

   integer, parameter           :: nviews = 4
   integer(i_kind)              :: inst
   character(len=14), parameter :: INST_PREFIX  = 'OR_ABI-L1b-Rad'

   !! File reading variables
   character(len=1000)          :: fname, command
   character(len=50)            :: list_file
   integer                      :: file_unit

   type date_type
      integer                   :: yr, mt, dy, hr, mn, sc, jdy
      real(r_double)            :: obs_time
   end type date_type

!   ! Linked list type for radiance location information
!   type viewnode
!      real    :: lat, lon, satzen, satazi
!      integer :: iy, ix
!      type(model_loc_type)    :: loc
!      type(viewnode), pointer :: next
!      integer :: i
!   end type viewnode

   type field_r
      real, pointer :: local(:)
      real, pointer :: domain(:)
      real, pointer :: patch(:)
   end type field_r
   type field_i
      integer, pointer :: local(:)
      integer, pointer :: domain(:)
      integer, pointer :: patch(:)
   end type field_i
   type field_loc
      type(model_loc_type), pointer :: local(:)
      type(model_loc_type), pointer :: domain(:)
      type(model_loc_type), pointer :: patch(:)
   end type field_loc

   type viewinfo
      logical                          :: select
      integer                          :: nfiles
      character(len=1000)              :: fpath
      character(len=200), allocatable  :: filename(:)
      integer, allocatable             :: filechan(:)
      type(date_type), allocatable     :: filedate(:)
      logical, allocatable             :: file_fgat_match(:,:)
      real*8, allocatable              :: fgat_time_abs_diff(:,:) ! seconds
      real*8, allocatable              :: min_time_diff(:,:) ! seconds
      integer, allocatable             :: nfiles_used(:)
      logical                          :: meta_initialized = .false.
      logical                          :: grid_initialized = .false.
      integer                          :: ny_global, nx_global, yoff_fd, xoff_fd
      integer                          :: ys_local, xs_local
      integer                          :: ye_local, xe_local
      integer, allocatable             :: ny_grid(:), nx_grid(:)
      integer, allocatable             :: ys_grid(:), xs_grid(:)
      integer                          :: ys_p, xs_p
      integer                          :: ye_p, xe_p
      integer                          :: ys_p_fd, xs_p_fd
      integer                          :: ye_p_fd, xe_p_fd
      integer                          :: nrad_on_patch, nrad_on_domain
      integer                          :: nrad_on_patch_cldqc, nrad_on_domain_cldqc
      logical, allocatable             :: patchmask(:,:,:)
!      type(viewnode), pointer          :: head
!      type(viewnode), pointer          :: current

      type(field_r)   :: lat_1d, lon_1d, satzen_1d, satazi_1d
      type(field_i)   :: iy_1d, ix_1d
      type(field_loc) :: loc_1d

      character(len=2)                 :: name_short
      character(len=10)                :: name
      logical                          :: moving
   end type viewinfo

   type(viewinfo), target, allocatable :: view_att(:)
   type(viewinfo), pointer             :: this_view

   integer                             :: first_file, tot_files_used, npass
   integer                             :: ncid, varid

   !! WRFDA channel and satellite_id select
   !! These should be inputs to the subroutine or global variables in WRFDA
   !Could populate using .info file.  Would reduce number of files to read...
!   integer, dimension(10)              :: channel_select = (/7, 8, 9, 10, 11, 12, 13, 14, 15, 16/)

   ! Global WRFDA obs timing info
   character(len=19)                   :: fgat_times_c(num_fgat_time)
   real(r_double)                      :: fgat_times_r(num_fgat_time)

   ! Local Obs date/time variables
   real(r_double)                      :: obs_time
   integer(i_kind)                     :: yr, mt, dy, hr, mn, sc, jdy
   real(r_double)                      :: timbdy(2)

   ! Other work variables
   real(r_double)   :: dlon_earth,dlat_earth,dlon_earth_deg,dlat_earth_deg
   real(r_double)   :: ngoes
   integer(i_kind)  :: num_goesabi_local, num_goesabi_global, &
                 num_goesabi_used, num_goesabi_used_fgat(num_fgat_time), &
                 num_goesabi_used_tmp, num_goesabi_thinned
   integer(i_kind)  :: itx, itt
   real, allocatable :: in(:), out(:)

   !Cloud QC variables
   integer :: tbuf, nkeep, ikeep
   integer :: abi_halo_width  ! Must be ≥ 0
   integer :: superob_width
   real    :: mu10, mu14, sigma10, sigma14, pearson, temp_max
   real    :: mu, sigma
   real, allocatable :: tb_temp(:,:)
   logical :: cldqc
   character(18) :: terr_fname

   integer :: TEMPIR_ifile
   real    :: TEMPIR_min_time_diff, TEMPIR_time_abs_diff
   real, parameter :: TEMPIR_delay_minutes = 15.0

   if (trace_use) call da_trace_entry("da_read_obs_ncgoesabi")

! determine if satellite_id is supported
!-----------------------------------------------------
   if(satellite_id .ne. 16 .and. &
      satellite_id .ne. 17) then
      write(unit=stdout,fmt='(A,I2.2,A)') 'goes satellite ', satellite_id, ' is not supported for abi instrument'
      return
   endif

   write(terr_fname,'(A,I2.2,A)') 'OR_ABI-TERR_G',satellite_id,'.nc'

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
      write(unit=message(1),fmt='(A,I2.2,A)') " goes-",satellite_id,"-abi is not in sensor list"
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
   ! (default) All views are used (algorithm figures out which views have files present)
   ! Could set this according to namelist entries
   view_att(:) % select = .true.
   view_att(1) % name_short = 'F'
   view_att(2) % name_short = 'C'
   view_att(3) % name_short = 'M1'
   view_att(4) % name_short = 'M2'

   view_att(1) % name = 'Full Disk'
   view_att(2) % name = 'CONUS'
   view_att(3) % name = 'MESO1'
   view_att(4) % name = 'MESO2'

   write(view_att(1) % fpath,'(A,I2.2,A)') "./goes-",satellite_id,"-fdisk*/"
   write(view_att(2) % fpath,'(A,I2.2,A)') "./goes-",satellite_id,"-conus*/"
   write(view_att(3) % fpath,'(A,I2.2,A)') "./goes-",satellite_id,"-meso*/"
   write(view_att(4) % fpath,'(A,I2.2,A)') "./goes-",satellite_id,"-meso*/"

   ! (default) Full Disk and CONUS are fixed while MESO 1 & 2 can move within an assimilation window
   view_att(1) % moving = .false.
   view_att(2) % moving = .false.
   view_att(3) % moving = .true.
   view_att(4) % moving = .true.

!   ! Full Disk, CONUS, and MESO 1 & 2 are fixed within an assimilation window (e.g., 3D-Var)
!   view_att(1) % moving = .false.
!   view_att(2) % moving = .false.
!   view_att(3) % moving = .false.
!   view_att(4) % moving = .false.

   !! Initialize local obs structures
   allocate (head)
   nullify  (head % next )
   p => head

   num_goesabi_local     = 0
   num_goesabi_global    = 0
   num_goesabi_used_fgat = 0
   num_goesabi_thinned   = 0

   abi_halo_width = abi_superob_halfwidth
   if ( use_clddet_zz ) then
      abi_halo_width = abi_halo_width + 10
   end if

   superob_width = 2*abi_superob_halfwidth+1

   tot_files_used = 0
   use_view_mask = .false.

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Collect files available for all views
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   PrepViews: do iview = 1, nviews
      this_view => view_att(iview)

      if ( .not.this_view % select ) cycle PrepViews

      ! Query fpath for files that match L1B naming conventions for this_view and satellite_id
      fname = trim(INST_PREFIX)//trim(this_view % name_short)
      write(list_file,'(A,I2.2,2A)') &
         'file_list_GOES-',satellite_id,'-ABI_',trim(this_view % name_short)

      call da_get_unit(file_unit)

      if (rootproc) then
         inquire(file=trim(list_file), exist=exists)
         if ( .not.exists ) then
            ! Create list_file containing all files for this_view
            write(unit=stdout,fmt='(5A)') 'Searching for GOES ', trim(this_view % name) ,' files in ', trim(this_view % fpath),'...'

            write(command,fmt='(5A,I2.2,2A)')&
                    "find ",trim(this_view % fpath), &
                    " \( -type l -o -type f \) -name '",trim(fname), &
                    "*G",satellite_id, &
                    "*' > ",trim(list_file)
!                    "*' -printf '%P\n' > ",trim(list_file)

            write(stdout,fmt='(A)') 'WARNING find requires substantial memory. It is recommended to issue'
            write(stdout,fmt='(A)') 'WARNING  the following from the command line before running WRFDA:'
            write(stdout,fmt='(A)') adjustl(trim(command))
            cmsg = ""
            call execute_command_line ( adjustl(trim(command)), &
               WAIT=.true., EXITSTAT=estat, CMDSTAT=cstat, CMDMSG=cmsg )
            write(stdout,*) 'estat: ', estat
            write(stdout,*) 'cstat: ', cstat
            write(stdout,*) 'cmsg:  ', cmsg
         end if
         write(unit=stdout,fmt='(5A)') 'Using GOES ', trim(this_view % name) ,' files listed in ', trim(list_file)

         icount = 0
         io_stat = -1
         do while (io_stat .ne. 0)
            open(unit=file_unit,file=trim(list_file), iostat = io_stat)
            icount = icount + 1
            if (icount .gt. 10000) exit
         end do

         this_view % nfiles = 0
         do
            read(file_unit, fmt=*, iostat = io_stat)
            if ( io_stat .ne. 0 ) exit
            this_view % nfiles = this_view % nfiles + 1
         end do
         close(file_unit)

         i_dummy = this_view % nfiles
      end if
#ifdef DM_PARALLEL
      call mpi_barrier(comm, ierr)
      call mpi_bcast ( i_dummy(1), 1, mpi_integer, root, comm, ierr )
      this_view % nfiles = i_dummy(1)
#endif
      if (this_view % nfiles .lt. 1) then
         this_view % select = .false.
         cycle PrepViews
      end if

      allocate(this_view % filename(this_view % nfiles))

      ! Read the file names for this view
      open(unit=file_unit,file=trim(list_file))
      read(file_unit, fmt='(A)') (this_view % filename(ifile), ifile=1,this_view % nfiles)
      close(file_unit)

      call da_free_unit(file_unit)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! Allocate/init components for this_view
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      allocate(this_view % filechan(this_view % nfiles))
      allocate(this_view % filedate(this_view % nfiles))
      allocate(this_view % file_fgat_match(this_view % nfiles,num_fgat_time))
      allocate(this_view % fgat_time_abs_diff(this_view % nfiles,num_fgat_time))
      allocate(this_view % min_time_diff(nchan,num_fgat_time))
      allocate(this_view % nfiles_used(num_fgat_time))

      this_view % file_fgat_match = .false.
      do ifgat=1,num_fgat_time
         this_view % fgat_time_abs_diff(:,ifgat) = &
            abs(time_slots(ifgat) - time_slots(ifgat-1)) * 60.D0 !seconds

         this_view % min_time_diff(:,ifgat) = &
            abs(time_slots(ifgat) - time_slots(ifgat-1)) * 60.D0 / 2.D0 !seconds
      end do

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! Determine which of the files will be used based on user-definitions:
      !! + fgat window length
      !! + channels used
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      do ifile = 1, this_view % nfiles

         !Grab the filename (without path) using INST_PREFIX
         fname = trim(this_view % filename(ifile))
         ioff = index(fname, trim(INST_PREFIX))
!!         this_view % filepath(ifile) = fname(1:ioff-1)
         fname = trim(fname(ioff:len(adjustl(trim(fname)))))
!!         this_view % filename(ifile) = trim(fname)

         ioff = 0
         if (iview.eq.3 .or. iview.eq.4) ioff=1
         ioff = ioff+19
         read(fname(1+ioff:2+ioff),fmt='(I2.2)') this_view % filechan(ifile)

!!!         !! The channel could instead be read from band_id in each file, but
!!!         !!  opening/closing files for all channels is time consuming
!!!         ierr=nf_open(trim(this_view % fpath)//trim(fname),nf_nowrite,ncid)
!!!         ierr=nf_inq_varid(ncid,'band_id',varid)
!!!         ierr=nf_get_var_int(ncid,varid,this_view % filechan(ifile))
!!!         ierr=nf_close(ncid)

         ! Check if channel is selected
!            if ( .not.any(this_view % filechan(ifile) .eq. channel_select) .or. &
         if ( .not.any(this_view % filechan(ifile) .eq. channel_list) ) then
!!!               ierr=nf_close(ncid)
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
         obs_time = real(sc,8)/60.D0 / 2.D0

         call jday2cal(jdy, yr, mt, dy)
         call da_get_julian_time(yr,mt,dy,hr,mn,timbdy(1))

         this_view % filedate(ifile) % jdy = jdy

         !obs END time
         ioff = ioff + 16
         read(fname(1+ioff:4+ioff),fmt='(I4.4)') yr
         read(fname(5+ioff:7+ioff),fmt='(I3.3)') jdy
         read(fname(8+ioff:9+ioff),fmt='(I2.2)') hr
         read(fname(10+ioff:11+ioff),fmt='(I2.2)') mn
         read(fname(12+ioff:13+ioff),fmt='(I2.2)') sc
         obs_time = obs_time + real(sc,8)/60.D0 / 2.D0

         call jday2cal(jdy, yr, mt, dy)
         call da_get_julian_time(yr,mt,dy,hr,mn,timbdy(2))

         obs_time = obs_time + (timbdy(1) + timbdy(2)) / 2.D0

!! The time it takes to read time_bounds from each file is not insignificant.  Above method is much faster.
!         !! Determine central date of this file for obs binning
!!!         ierr=nf_open(trim(this_view % fpath)//trim(fname),nf_nowrite,ncid)
!!!         ierr=nf_inq_varid(ncid,'time_bounds',varid)
!!!         ierr=nf_get_var_double(ncid,varid,timbdy)
!!!         ierr=nf_close(ncid)
!!!         j2000=(timbdy(1) + timbdy(2)) / 2.D0 /86400.D0

         call da_get_cal_time(obs_time,yr,mt,dy,hr,mn,sc)
         obs_time = obs_time * 60.D0

         this_view % filedate(ifile) % yr = yr
         this_view % filedate(ifile) % mt = mt
         this_view % filedate(ifile) % dy = dy
         this_view % filedate(ifile) % hr = hr
         this_view % filedate(ifile) % mn = mn
         this_view % filedate(ifile) % sc = sc
         this_view % filedate(ifile) % obs_time = obs_time


!JJG: Note that this test being limited by time_slots prevents the use of data before/after the first/last time of the window even if the observations outside the window were recorded at times nearer to those bounds than data contained within the window.
         if ( obs_time < time_slots(0) * 60.D0 .or.  &
            obs_time >= time_slots(num_fgat_time) * 60.D0 ) then
            cycle
         end if

         do ifgat=1,num_fgat_time
            this_view % file_fgat_match(ifile,ifgat) = &
               ( obs_time >= time_slots(ifgat-1) * 60.D0 .and.  &
                  obs_time  < time_slots(ifgat) * 60.D0 )
            if (this_view % file_fgat_match(ifile,ifgat)) exit
         end do

         this_view % fgat_time_abs_diff(ifile,ifgat) = &
             abs( obs_time - fgat_times_r(ifgat) )

         call get_ichan(this_view % filechan(ifile), channel_list, nchan, ichan)
         if ( this_view % fgat_time_abs_diff(ifile, ifgat) .ge. &
              this_view % min_time_diff(ichan, ifgat) ) then
            this_view % file_fgat_match(ifile,ifgat) = .false.
         else
            this_view % min_time_diff(ichan, ifgat) = this_view % fgat_time_abs_diff(ifile, ifgat)
         end if

         if (count(this_view % file_fgat_match(ifile,:)) .gt. 1) then
            print*, 'WARNING: More than one bin was selected for ',trim(fname)
            print*, 'num_bin_per_file = ',count(this_view % file_fgat_match(ifile,:))
            print*, 'obs_time = ',obs_time
            print*, 'Ignoring this file for reading.'
            this_view % file_fgat_match(ifile,:) = .false.
            cycle
         end if
      end do

      do ifgat = 1, num_fgat_time
         ! Select a single file for this view, channel, and fgat using min_time_diff
         if ( count(this_view % file_fgat_match(:, ifgat)).gt.1 ) then
            do ifile = 1, this_view % nfiles
               if ( .not. this_view % file_fgat_match(ifile,ifgat) ) cycle
               call get_ichan(this_view % filechan(ifile), channel_list, nchan, ichan)
               if ( this_view % fgat_time_abs_diff(ifile, ifgat) .gt. &
                    this_view % min_time_diff(ichan, ifgat) ) then
                  this_view % file_fgat_match(ifile,ifgat) = .false.
               end if
            end do
         end if
      end do
   end do PrepViews

   !! If Full Disk is selected, take 2 passes over the data:
   !! + 1st pass: (A) Determine portions of each view corresponding to this patch
   !!                 for each fgat and each channel across observed domain
   !!             (B) Eliminate portions of broader views (Full Disk and CONUS) that
   !!                 can be replaced by narrower views (CONUS and MESO) with times
   !!                 closer to fgat time
   !! + 2nd pass: read radiance values, convert to BT, calculate quantities for online cloud detection QC
   !!
   !! Otherwise only take one pass, and duplicated data cannot be removed from CONUS/MESO1/MESO2

   npass = 1
   if (count(view_att(:) % select).gt.1 .and. view_att(1) % select) npass = 2

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Process data for views w/ nfiles > 1
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   do ipass = 1, npass
      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt='(A,I0,A,I2.2,A)') &
         'Starting pass ',ipass,&
         ' of GOES-',satellite_id,' ABI data processing'

   !! Loop over the available views for this instrument (ABI)
   do iview = 1, nviews
      this_view => view_att(iview)

      if ( .not.this_view % select ) cycle

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! Access netcdf channel/band files across all fgat windows
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      this_view % nfiles_used = 0

      fgat_loop: do ifgat = 1, num_fgat_time
         if (count(this_view % file_fgat_match(:, ifgat)) .lt. 1) then
            cycle fgat_loop
         end if

         first_file = 0
         do ifile = 1, this_view % nfiles
            if ( .not. this_view % file_fgat_match(ifile,ifgat) ) cycle
            first_file = ifile
            exit
         end do
         if (first_file .eq. 0) cycle fgat_loop

         if ( sum(this_view % nfiles_used(:)).eq.0) &
            write(unit=stdout,fmt='(2A)') &
               'Processing data for view: ', trim(this_view % name)
         write(unit=stdout,fmt='(2A)') &
            '          fgat time: ',fgat_times_c(ifgat)

         yr = this_view % filedate(first_file) % yr
         mt = this_view % filedate(first_file) % mt
         dy = this_view % filedate(first_file) % dy
         hr = this_view % filedate(first_file) % hr
         mn = this_view % filedate(first_file) % mn
         sc = this_view % filedate(first_file) % sc
         write(unit=stdout, &
            fmt='(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')  &
            '          data time: ',yr, '-', mt, '-', dy, '_', hr, ':', mn, ':', sc

         fname = trim(this_view % filename(first_file))

         if ( .not.this_view % meta_initialized ) then
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !! Get ABI metadata (first pass for FD, CONUS, MESO)
            ! Only ny_global and nx_global need to be read for all views, but this is a cheap subroutine
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            write(unit=stdout,fmt='(A)') &
               '          Reading abi metadata...'

            this_view % meta_initialized = .true.

            call get_abil1b_metadata( &
               fname, this_view % ny_global, this_view % nx_global, &
               req, rpol, pph, nam)! , lat_sat, lon_sat )

#ifdef DM_PARALLEL
            ! Split the global ABI grid for this view into local segments
            allocate ( this_view % ny_grid ( num_procs ) )
            allocate ( this_view % nx_grid ( num_procs ) )
            allocate ( this_view % ys_grid ( num_procs ) )
            allocate ( this_view % xs_grid ( num_procs ) )

            call split_grid( this_view % ny_global, this_view % nx_global , &
                             this_view % ny_grid,  this_view % nx_grid    , &
                             this_view % ys_grid,  this_view % xs_grid    )
#else
            ! When mpi parallelism is not available, assign global values to local variables
            this_view % ny_grid = this_view % ny_global
            this_view % nx_grid = this_view % nx_global
            this_view % ys_grid = 1
            this_view % xs_grid = 1
#endif
         end if

         ! Recall global dims for this_view
         ny_global = this_view % ny_global
         nx_global = this_view % nx_global

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !! Generate grid locations if
         !!  + CONUS or FD and first matching fgat
         !!  + MESO and any fgat (extent changes in time)
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         DoGridGen: if (  this_view % moving .or. .not.this_view % grid_initialized ) then

            ! Read grid from file, convert to lat, lon, satzen, satazi
            write(unit=stdout,fmt='(2A)') &
               '          Establishing abi grid info...'

            this_view % grid_initialized = .true.

            !========================================================
            ! Establish GOES metadata for this view and ifgat
            ! (constant acros fgat's, except for this_view % moving)
            !========================================================
            allocate( yy_abi (ny_global) )
            allocate( xx_abi (nx_global) )
            call get_abil1b_grid1( fname, &
                                   ny_global, nx_global, &
                                   yy_abi, xx_abi, &
                                   this_view % yoff_fd, this_view % xoff_fd )

            if ( iview.eq.1 ) then
               yoff_fd = this_view % yoff_fd
               xoff_fd = this_view % xoff_fd
               this_view % yoff_fd = 1
               this_view % xoff_fd = 1
            else
               this_view % yoff_fd = this_view % yoff_fd - yoff_fd + 1
               this_view % xoff_fd = this_view % xoff_fd - xoff_fd + 1
            end if

            !===========================================================
            ! Create a local array subset of observation location
            !  quantities across processors.
            !===========================================================
            nrad_local = ny_global * nx_global / (num_procs-1)
            allocate( yy_1d (nrad_local) )
            allocate( xx_1d (nrad_local) )
            allocate( iy_1d (nrad_local) )
            allocate( ix_1d (nrad_local) )

            n = 0 ; icount = 0

!JJG: Not convinced that these subgrids are needed.  Might be able to loop over global X/Y instead. This solution may be overly complex.  mod test for load balancing is still needed!
            ! This loop over subgrids and the selective logic
            !  below for myproc balances the processor loads
            !  when some imager pixels are off-earth or outside
            !  zenith-angle limits (Full Disk and CONUS)
            do subgrid = 1, num_procs
               ! Recall local dims for this_view
               ny_local = this_view % ny_grid(subgrid)
               nx_local = this_view % nx_grid(subgrid)
               this_view % ys_local = this_view % ys_grid(subgrid)
               this_view % xs_local = this_view % xs_grid(subgrid)

               do ixl = 1, nx_local
                  do iyl = 1, ny_local
                     iy = iyl + this_view % ys_local - 1
                     ix = ixl + this_view % xs_local - 1
                     if ( mod( iy-abi_superob_halfwidth-1, superob_width ) == 0  .and. &
                          mod( ix-abi_superob_halfwidth-1, superob_width ) == 0 ) then
                        !This mod test produces balanced loads between processors
                        if ( mod( n, num_procs ) .eq. myproc ) then
                           icount = icount + 1

                           yy_1d ( icount ) = yy_abi( iy )
                           xx_1d ( icount ) = xx_abi( ix )
                           iy_1d ( icount ) = iy
                           ix_1d ( icount ) = ix
                        end if
                        n = n + 1
                     end if
                  end do
               end do
            end do

!            !This may work as a simplified replacement for the code above, not sure if loads will be balanced
!            do ix = 1, nx_global
!               do iy = 1, ny_global
!                  !This mod test produces balanced loads between processors
!                  if ( mod( n, num_procs ) .eq. myproc ) then
!                     icount = icount + 1
!                     yy_1d ( icount ) = yy_abi( iy )
!                     xx_1d ( icount ) = xx_abi( ix )
!                     iy_1d ( icount ) = iy
!                     ix_1d ( icount ) = ix
!                  end if
!                  n = n + 1
!               end do
!            end do

            nrad_local = icount

            deallocate( yy_abi, xx_abi )

            allocate( earthmask_1d                  (1:nrad_local) )
            allocate( zenmask_1d                    (1:nrad_local) )
            allocate( this_view % lat_1d % local    (1:nrad_local) )
            allocate( this_view % lon_1d % local    (1:nrad_local) )
            allocate( this_view % satzen_1d % local (1:nrad_local) )
            allocate( this_view % satazi_1d % local (1:nrad_local) )
            allocate( this_view % iy_1d % local     (1:nrad_local) )
            allocate( this_view % ix_1d % local     (1:nrad_local) )

            ! Assign values for iy, ix, lat, lon, satzen, satazi
            this_view % iy_1d % local = iy_1d (1:nrad_local)
            this_view % ix_1d % local = ix_1d (1:nrad_local)
            deallocate( iy_1d )
            deallocate( ix_1d )

            write(unit=stdout,fmt='(3A,I0)') &
               '          ',trim(this_view % name),' locations processed on this core: ', nrad_local

            if (nrad_local .gt. 0) &
               call get_abil1b_grid2_1d( yy_1d(1:nrad_local), xx_1d(1:nrad_local), &
                                         req, rpol, pph, nam, satellite_id, &
                                         this_view % lat_1d % local, &
                                         this_view % lon_1d % local, &
                                         this_view % satzen_1d % local, &
                                         this_view % satazi_1d % local, &
                                         earthmask_1d, zenmask_1d )

            ! Reduce values for iy, ix, lat, lon, satzen, satazi
            ! using earth and zenith masks
            nrad_mask = count ( earthmask_1d .and. zenmask_1d )
            this_view % lat_1d % local(1:nrad_mask) = &
                     pack(this_view % lat_1d % local    , earthmask_1d .and. zenmask_1d )
            this_view % lon_1d % local(1:nrad_mask) = &
                     pack(this_view % lon_1d % local    , earthmask_1d .and. zenmask_1d )
            this_view % satzen_1d % local(1:nrad_mask) = &
                     pack(this_view % satzen_1d % local , earthmask_1d .and. zenmask_1d )
            this_view % satazi_1d % local(1:nrad_mask) = &
                     pack(this_view % satazi_1d % local , earthmask_1d .and. zenmask_1d )
            this_view % iy_1d % local(1:nrad_mask) = &
                     pack(this_view % iy_1d % local     , earthmask_1d .and. zenmask_1d )
            this_view % ix_1d % local(1:nrad_mask) = &
                     pack(this_view % ix_1d % local     , earthmask_1d .and. zenmask_1d )

            nrad_local = nrad_mask

            deallocate( earthmask_1d )
            deallocate( zenmask_1d )
            deallocate( yy_1d, xx_1d )

            ! Populate loc x, y and determine in/outside domain
            allocate ( this_view % loc_1d % local (nrad_local) )
            allocate ( domainmask_1d              (nrad_local) )
            allocate ( dummybool_2d               (nrad_local,2) )
            allocate ( info_1d                    (nrad_local) )
            info_1d (:) % lat = this_view % lat_1d % local ( 1:nrad_local )
            info_1d (:) % lon = this_view % lon_1d % local ( 1:nrad_local )
            call da_llxy_1d ( info_1d, this_view % loc_1d % local(:), &
                              dummybool_2d(:,1), dummybool_2d(:,2) )
            domainmask_1d = .not.dummybool_2d(:,2)
            deallocate( dummybool_2d )
            deallocate( info_1d )
            nrad_mask = count( domainmask_1d )

#ifdef DM_PARALLEL
            call mpi_barrier(comm, ierr)
#endif
            ! COMMUNICATE 1D FIELDS FROM REMOTE PROCS TO LOCAL BUFFER
            !  Note: these comms are a minor bottleneck, which will be
            !  more noticeable for 4D-Var when MESO1/2 is processed
            !  at multiple fgat's
            !  Potential Solutions
            !  SOLUTION 1: mpi_allgatherv (let's mpi figure out the most efficient way to distribute the data to all processes)
            !  SOLUTION 2: round-robin mpi_bcast (may be less resource intensive with smaller communication chunks)

!            ! BEGIN SOLUTION 1
!!            !PACK UP DOMAIN DATA FROM THIS PROCESSOR
!!            this_view % lat_1d % local    (1:nrad_mask) = &
!!               pack(this_view % lat_1d % local    (1:nrad_local),   domainmask_1d )
!!            this_view % lon_1d % local    (1:nrad_mask) = &
!!               pack(this_view % lon_1d % local    (1:nrad_local),   domainmask_1d )
!!            this_view % satzen_1d % local (1:nrad_mask) = &
!!               pack(this_view % satzen_1d % local (1:nrad_local),   domainmask_1d )
!!            this_view % satazi_1d % local (1:nrad_mask) = &
!!               pack(this_view % satazi_1d % local (1:nrad_local),   domainmask_1d )
!!            this_view % iy_1d % local     (1:nrad_mask) = &
!!               pack(this_view % iy_1d % local     (1:nrad_local),   domainmask_1d )
!!            this_view % ix_1d % local     (1:nrad_mask) = &
!!               pack(this_view % ix_1d % local     (1:nrad_local),   domainmask_1d )
!!            this_view % loc_1d % local    (1:nrad_mask) % y = &
!!               pack(this_view % loc_1d % local    (1:nrad_local) % y, domainmask_1d )
!!            this_view % loc_1d % local    (1:nrad_mask) % x = &
!!               pack(this_view % loc_1d % local    (1:nrad_local) % x, domainmask_1d )
!
!            !ALLOCATE COMMUNICATION BUFFERS
!            allocate ( nbufs ( num_procs ) )
!            allocate ( displs ( num_procs ) )
!#ifdef DM_PARALLEL
!            call mpi_allgather ( nrad_mask, 1, mpi_integer, nbufs, 1, mpi_integer, comm, ierr )
!#else
!            nbufs = nrad_mask
!#endif
!
!            displs = 0
!            do iproc = 1, num_procs - 1
!               displs(iproc+1) = displs(iproc) + nbufs(iproc)
!            end do
!
!            this_view % nrad_on_domain = sum( nbufs )
!
!            allocate( buf_real( this_view % nrad_on_domain, 4 ) )
!            allocate( buf_int ( this_view % nrad_on_domain, 2 ) )
!            allocate( buf_loc ( this_view % nrad_on_domain ) )
!
!            buf_real  = missing_r
!            buf_int   = missing
!            buf_loc%y = missing_r
!            buf_loc%x = missing_r
!
!            !PACK UP DOMAIN DATA FROM THIS PROCESSOR
!            buf_i = displs(iproc+1) + 1
!            buf_f = buf_i + nrad_mask - 1
!            buf_real( buf_i:buf_f, 1 ) = &
!               pack(this_view % lat_1d % local    (1:nrad_local),     domainmask_1d )
!            buf_real( buf_i:buf_f, 2 ) = &
!               pack(this_view % lon_1d % local    (1:nrad_local),     domainmask_1d )
!            buf_real( buf_i:buf_f, 3 ) = &
!               pack(this_view % satzen_1d % local (1:nrad_local),     domainmask_1d )
!            buf_real( buf_i:buf_f, 4 ) = &
!               pack(this_view % satazi_1d % local (1:nrad_local),     domainmask_1d )
!            buf_int ( buf_i:buf_f, 1 ) = &
!               pack(this_view % iy_1d % local     (1:nrad_local),     domainmask_1d )
!            buf_int ( buf_i:buf_f, 2 ) = &
!               pack(this_view % ix_1d % local     (1:nrad_local),     domainmask_1d )
!            buf_loc ( buf_i:buf_f ) % y = &
!               pack(this_view % loc_1d % local    (1:nrad_local) % y, domainmask_1d )
!            buf_loc ( buf_i:buf_f ) % x = &
!               pack(this_view % loc_1d % local    (1:nrad_local) % x, domainmask_1d )
!
!#ifdef DM_PARALLEL
!             !PERFORM COMMS
!
!            ! NOTE: MPI_IN_PLACE can only be used when comm is an intracommunicator
!            call mpi_allgatherv ( &
!               MPI_IN_PLACE, 0, true_mpi_real, buf_real(:,1), nbufs, displs, true_mpi_real, comm, ierr )
!            call mpi_allgatherv ( &
!               MPI_IN_PLACE, 0, true_mpi_real, buf_real(:,2), nbufs, displs, true_mpi_real, comm, ierr )
!            call mpi_allgatherv ( &
!               MPI_IN_PLACE, 0, true_mpi_real, buf_real(:,3), nbufs, displs, true_mpi_real, comm, ierr )
!            call mpi_allgatherv ( &
!               MPI_IN_PLACE, 0, true_mpi_real, buf_real(:,4), nbufs, displs, true_mpi_real, comm, ierr )
!            call mpi_allgatherv ( &
!               MPI_IN_PLACE, 0, mpi_integer,   buf_int(:,1),  nbufs, displs, mpi_integer,   comm, ierr )
!            call mpi_allgatherv ( &
!               MPI_IN_PLACE, 0, mpi_integer,   buf_int(:,2),  nbufs, displs, mpi_integer,   comm, ierr )
!            call mpi_allgatherv ( &
!               MPI_IN_PLACE, 0, true_mpi_real, buf_loc(:)%y,  nbufs, displs, true_mpi_real, comm, ierr )
!            call mpi_allgatherv ( &
!               MPI_IN_PLACE, 0, true_mpi_real, buf_loc(:)%x,  nbufs, displs, true_mpi_real, comm, ierr )
!
!!            call mpi_allgatherv ( &
!!               this_view % lat_1d % local    (1:nrad_mask), nrad_mask, true_mpi_real, &
!!               buf_real(:,1),  nbufs, displs,  true_mpi_real, comm, ierr )
!!            call mpi_allgatherv ( &
!!               this_view % lon_1d % local    (1:nrad_mask), nrad_mask, true_mpi_real, &
!!               buf_real(:,2),  nbufs, displs,  true_mpi_real, comm, ierr )
!!            call mpi_allgatherv ( &
!!               this_view % satzen_1d % local (1:nrad_mask), nrad_mask, true_mpi_real, &
!!               buf_real(:,3),  nbufs, displs,  true_mpi_real, comm, ierr )
!!            call mpi_allgatherv ( &
!!               this_view % satazi_1d % local (1:nrad_mask), nrad_mask, true_mpi_real, &
!!               buf_real(:,4),  nbufs, displs,  true_mpi_real, comm, ierr )
!!            call mpi_allgatherv ( &
!!               this_view % iy_1d % local     (1:nrad_mask), nrad_mask, mpi_integer,   &
!!               buf_int(:,1),  nbufs, displs,  mpi_integer,   comm, ierr )
!!            call mpi_allgatherv ( &
!!               this_view % ix_1d % local     (1:nrad_mask), nrad_mask, mpi_integer,   &
!!               buf_int(:,2),  nbufs, displs,  mpi_integer,   comm, ierr )
!!            call mpi_allgatherv ( &
!!               this_view % loc_1d % local    (1:nrad_mask) % y, nrad_mask, true_mpi_real, &
!!               buf_loc(:)%y,  nbufs, displs,  true_mpi_real, comm, ierr )
!!            call mpi_allgatherv ( &
!!               this_view % loc_1d % local    (1:nrad_mask) % x, nrad_mask, true_mpi_real, &
!!               buf_loc(:)%x,  nbufs, displs,  true_mpi_real, comm, ierr )
!!#else
!!            buf_real( :, 1 ) = this_view % lat_1d % local    (1:nrad_mask)
!!            buf_real( :, 2 ) = this_view % lon_1d % local    (1:nrad_mask)
!!            buf_real( :, 3 ) = this_view % satzen_1d % local (1:nrad_mask)
!!            buf_real( :, 4 ) = this_view % satazi_1d % local (1:nrad_mask)
!!            buf_int ( :, 1 ) = this_view % iy_1d % local     (1:nrad_mask)
!!            buf_int ( :, 2 ) = this_view % ix_1d % local     (1:nrad_mask)
!!            buf_loc ( : ) % y = this_view % loc_1d % local   (1:nrad_mask) % y
!!            buf_loc ( : ) % x = this_view % loc_1d % local   (1:nrad_mask) % x
!#endif
!            deallocate ( nbufs, displs )
!            ! END SOLUTION 1

            ! BEGIN SOLUTION 2
            !ALLOCATE COMMUNICATION BUFFERS
#ifdef DM_PARALLEL
            call mpi_allreduce( nrad_mask, nbuf, 1, mpi_integer, mpi_sum, comm, ierr )
#else
            nbuf = nrad_mask
#endif
            allocate( buf_real( nbuf, 4 ) )
            allocate( buf_int ( nbuf, 2 ) )
            allocate( buf_loc ( nbuf ) )

            this_view % nrad_on_domain = nbuf

            buf_f = 0
            ProcLoop: do iproc = 0, num_procs-1
               nbuf = nrad_mask
#ifdef DM_PARALLEL
               call mpi_bcast(nbuf, 1, mpi_integer, iproc, comm, ierr )
#endif
               if (nbuf .eq. 0) cycle ProcLoop
               buf_i = buf_f + 1
               buf_f = buf_i + nbuf - 1

               if (iproc .eq. myproc) then
                  !PACK UP DATA FROM THIS PROCESSOR
                  buf_real( buf_i:buf_f, 1 ) = &
                     pack(this_view % lat_1d % local    (1:nrad_local),   domainmask_1d )
                  buf_real( buf_i:buf_f, 2 ) = &
                     pack(this_view % lon_1d % local    (1:nrad_local),   domainmask_1d )
                  buf_real( buf_i:buf_f, 3 ) = &
                     pack(this_view % satzen_1d % local (1:nrad_local),   domainmask_1d )
                  buf_real( buf_i:buf_f, 4 ) = &
                     pack(this_view % satazi_1d % local (1:nrad_local),   domainmask_1d )
                  buf_int ( buf_i:buf_f, 1 ) = &
                     pack(this_view % iy_1d % local     (1:nrad_local),   domainmask_1d )
                  buf_int ( buf_i:buf_f, 2 ) = &
                     pack(this_view % ix_1d % local     (1:nrad_local),   domainmask_1d )

                  buf_loc ( buf_i:buf_f ) % y = &
                     pack(this_view % loc_1d % local    (1:nrad_local) % y, domainmask_1d )
                  buf_loc ( buf_i:buf_f ) % x = &
                     pack(this_view % loc_1d % local    (1:nrad_local) % x, domainmask_1d )
               else
                  buf_real(buf_i:buf_f,:)  = missing_r
                  buf_int(buf_i:buf_f,:)   = missing
!                  buf_loc(buf_i:buf_f)%y   = missing_r
!                  buf_loc(buf_i:buf_f)%x   = missing_r
               end if
#ifdef DM_PARALLEL
               !PERFORM COMMS
               call mpi_bcast(buf_real(buf_i:buf_f,:), nbuf * 4, true_mpi_real, iproc, comm, ierr )
               call mpi_bcast(buf_int (buf_i:buf_f,:), nbuf * 2, mpi_integer,   iproc, comm, ierr )

               !Only x & y components of loc need to be communicated
               call mpi_bcast( buf_loc(buf_i:buf_f)%y, nbuf, true_mpi_real, iproc, comm, ierr )
               call mpi_bcast( buf_loc(buf_i:buf_f)%x, nbuf, true_mpi_real, iproc, comm, ierr )
#endif
            end do ProcLoop
            ! END SOLUTION 2

            deallocate ( this_view % lat_1d % local    )
            deallocate ( this_view % lon_1d % local    )
            deallocate ( this_view % satzen_1d % local )
            deallocate ( this_view % satazi_1d % local )
            deallocate ( this_view % iy_1d % local     )
            deallocate ( this_view % ix_1d % local     )
            deallocate ( this_view % loc_1d % local    )
            deallocate ( domainmask_1d )

            ! ASSOCIATE REMOTE POINTERS WITH BUFFERS CONTAINING DOMAIN-WIDE OBS
            this_view % lat_1d % domain    => buf_real(:,1)
            this_view % lon_1d % domain    => buf_real(:,2)
            this_view % satzen_1d % domain => buf_real(:,3)
            this_view % satazi_1d % domain => buf_real(:,4)
            this_view % iy_1d % domain     => buf_int (:,1)
            this_view % ix_1d % domain     => buf_int (:,2)
            this_view % loc_1d % domain    => buf_loc (:)
            write(unit=stdout,fmt='(3A,I0)') &
               '          ',trim(this_view % name),' locations within domain: ', this_view % nrad_on_domain

            ! Populate remainder of loc and determine in/outside patch
            allocate ( patchmask_1d (this_view % nrad_on_domain) )
            allocate ( dummybool_2d (this_view % nrad_on_domain,1) )
            call da_llxy_1d ( locs = buf_loc, outside = dummybool_2d(:,1), do_xy = .false. )
            patchmask_1d = .not.dummybool_2d(:,1)
            deallocate( dummybool_2d )
            this_view % nrad_on_patch = count(patchmask_1d)
            write(unit=stdout,fmt='(3A,I0)') &
               '          ',trim(this_view % name),' locations within this subdomain: ', this_view % nrad_on_patch

            if ( this_view % nrad_on_patch .gt. 0 ) then
               if ( allocated ( this_view % patchmask ) ) then
                  deallocate ( this_view % patchmask )
                  deallocate ( this_view % lat_1d % patch    )
                  deallocate ( this_view % lon_1d % patch    )
                  deallocate ( this_view % satzen_1d % patch )
                  deallocate ( this_view % satazi_1d % patch )
                  deallocate ( this_view % iy_1d % patch     )
                  deallocate ( this_view % ix_1d % patch     )
                  deallocate ( this_view % loc_1d % patch    )
               end if
               allocate( this_view % lat_1d % patch    (this_view % nrad_on_patch) )
               allocate( this_view % lon_1d % patch    (this_view % nrad_on_patch) )
               allocate( this_view % satzen_1d % patch (this_view % nrad_on_patch) )
               allocate( this_view % satazi_1d % patch (this_view % nrad_on_patch) )
               allocate( this_view % iy_1d % patch     (this_view % nrad_on_patch) )
               allocate( this_view % ix_1d % patch     (this_view % nrad_on_patch) )
               allocate( this_view % loc_1d % patch    (this_view % nrad_on_patch) )

               this_view % lat_1d % patch    = &
                  pack( this_view % lat_1d % domain,    patchmask_1d )
               this_view % lon_1d % patch    = &
                  pack( this_view % lon_1d % domain,    patchmask_1d )
               this_view % satzen_1d % patch = &
                  pack( this_view % satzen_1d % domain, patchmask_1d )
               this_view % satazi_1d % patch = &
                  pack( this_view % satazi_1d % domain, patchmask_1d )
               this_view % iy_1d % patch     = &
                  pack( this_view % iy_1d % domain,     patchmask_1d )
               this_view % ix_1d % patch     = &
                  pack( this_view % ix_1d % domain,     patchmask_1d )
               this_view % loc_1d % patch    = &
                  pack( this_view % loc_1d % domain,    patchmask_1d )

               ! Determine grid extents for this patch on this_view and on Full Disk
               this_view % ys_p = minval(this_view % iy_1d % patch)
               this_view % ye_p = maxval(this_view % iy_1d % patch)
               this_view % xs_p = minval(this_view % ix_1d % patch)
               this_view % xe_p = maxval(this_view % ix_1d % patch)
               this_view % ys_p_fd = this_view % ys_p + this_view % yoff_fd - 1
               this_view % ye_p_fd = this_view % ye_p + this_view % yoff_fd - 1
               this_view % xs_p_fd = this_view % xs_p + this_view % xoff_fd - 1
               this_view % xe_p_fd = this_view % xe_p + this_view % xoff_fd - 1

!               write(stdout,*) 'ABI grid extents for this view:'
!               write(stdout,'(A,4I10)') 'ys_p, ye_p, xs_p, xe_p            ',this_view % ys_p, this_view % ye_p, this_view % xs_p, this_view % xe_p
!               write(stdout,*) 'ABI grid extents for Full Disk:'
!               write(stdout,'(A,4I10)') 'ys_p_fd, ye_p_fd, xs_p_fd, xe_p_fd',this_view % ys_p_fd, this_view % ye_p_fd, this_view % xs_p_fd, this_view % xe_p_fd

               ! Setup ZZ clddet extents
               this_view % ys_local = max(this_view % ys_p - abi_halo_width, 1)
               this_view % ye_local = min(this_view % ye_p + abi_halo_width, ny_global)
               this_view % xs_local = max(this_view % xs_p - abi_halo_width, 1)
               this_view % xe_local = min(this_view % xe_p + abi_halo_width, nx_global)

               ! Setup patch mask for this view, including ZZ clddet buffer
               allocate( this_view % patchmask( &
                   this_view % ys_local:this_view % ye_local, &
                   this_view % xs_local:this_view % xe_local, 2 ) )

               this_view % patchmask = .false.
               do n = 1, this_view % nrad_on_patch
                  iy = this_view % iy_1d % patch (n)
                  ix = this_view % ix_1d % patch (n)

                  cldqc = .true.
                  do jy = iy - abi_halo_width, iy + abi_halo_width
                  do jx = ix - abi_halo_width, ix + abi_halo_width
                     if ( &
                          jy.ge.1 .and. jy.le.ny_global &
                    .and. jx.ge.1 .and. jx.le.nx_global &
                           ) then
                        this_view % patchmask ( jy, jx, 2 ) = .true.
                     else
                        cldqc = .false.
                     end if
                  end do
                  end do
                  this_view % patchmask ( iy, ix, 1 ) = cldqc
               end do
               this_view % nrad_on_patch_cldqc = count( this_view % patchmask (:,:,1) )
            else
               this_view % nrad_on_patch_cldqc = 0
            end if
!            write(unit=stdout,fmt='(3A,I0)') &
!               '          ',trim(this_view % name),' locations within this subdomain eligible for ZZ clddet: ', this_view % nrad_on_patch_cldqc


            !FREE UP POINTERS AND BUFFERS
            nullify ( this_view % lat_1d % domain    )
            nullify ( this_view % lon_1d % domain    )
            nullify ( this_view % satzen_1d % domain )
            nullify ( this_view % satazi_1d % domain )
            nullify ( this_view % iy_1d % domain     )
            nullify ( this_view % ix_1d % domain     )
            nullify ( this_view % loc_1d % domain    )
            deallocate ( buf_real, buf_int, buf_loc )
            deallocate ( patchmask_1d )

#ifdef DM_PARALLEL
            call mpi_allreduce( this_view % nrad_on_patch_cldqc, &
                                this_view % nrad_on_domain_cldqc, &
                                1, mpi_integer, mpi_sum, comm, ierr )
            call mpi_barrier(comm, ierr)
#else
            this_view % nrad_on_domain_cldqc = this_view % nrad_on_patch_cldqc
#endif
         end if DoGridGen

         if ( iview.eq.1 .and. ipass.lt.npass .and. &
              sum(this_view % nfiles_used(:)).eq.0 ) then
            if ( this_view % nrad_on_patch_cldqc .gt. 0 ) then
               allocate( view_mask( &
                   this_view % ys_p_fd-2:this_view % ye_p_fd+2, &
                   this_view % xs_p_fd-2:this_view % xe_p_fd+2, &
                   nviews, nchan, num_fgat_time ) )
               view_mask = .false.
            end if
            use_view_mask = .true.
         end if

!         if ( (ipass.lt.npass .and. iview.eq.1) .or. .not.use_view_mask ) then
!            num_goesabi_global = num_goesabi_global + this_view % nrad_on_domain_cldqc
!            !ptotal(ifgat) = ptotal(ifgat) + this_view % nrad_on_domain_cldqc
!         end if

         PatchMatch: if (this_view % nrad_on_patch_cldqc .gt. 0) then

            ! Loop over channels; each process reads radiance data only for its subdomain
            ChannelLoop: do ichan = 1, nchan
               ifile = 0
               do jfile = 1, this_view % nfiles
                  if ( .not. this_view % file_fgat_match(jfile,ifgat) ) cycle
                  call get_ichan(this_view % filechan(jfile), channel_list, nchan, jchan)
                  if ( ichan .eq. jchan ) then
                     ifile = jfile
                     exit
                  end if
               end do
               if ( ifile .eq. 0 ) cycle ChannelLoop

               this_view % nfiles_used(ifgat) = this_view % nfiles_used(ifgat) + 1

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
                     do n = 1, this_view % nrad_on_patch
                        iy = this_view % iy_1d % patch (n)
                        ix = this_view % ix_1d % patch (n)
                        iyfd = iy + this_view % yoff_fd-1
                        ixfd = ix + this_view % xoff_fd-1
                        view_mask( iyfd, ixfd, iview, ichan, ifgat) = &
                           this_view % patchmask ( iy, ix, 1 )
                     end do
                  else
                     best_view = .true.
!                     do jview = 1, iview-1 !This assumes MESO1 and MESO2 are in identical locations
                     do jview = 1, min(iview-1,2) !This assumes MESO1 and MESO2 do not overlap
                        best_view = best_view .and. &
                           this_view % min_time_diff(ichan, ifgat) .lt. &
                              view_att(jview) % min_time_diff(ichan, ifgat)
                     end do
                     if ( best_view ) then
                        do n = 1, this_view % nrad_on_patch
                           iy = this_view % iy_1d % patch (n)
                           ix = this_view % ix_1d % patch (n)
                           if ( this_view % patchmask ( iy, ix, 1 ) ) then
                              iyfd = iy + this_view % yoff_fd-1
                              ixfd = ix + this_view % xoff_fd-1

                              view_mask( iyfd, ixfd, iview, ichan, ifgat) = .true.

                              !This assumes MESO1 and MESO2 do not overlap
                              view_mask( iyfd, ixfd, 1:min(iview-1,2), ichan, ifgat) = .false.

!                              !This assumes MESO1 and MESO2 are in identical locations
!                              view_mask( iyfd, ixfd, 1:iview-1, ichan, ifgat) = .false.
                           end if
                        end do
                     end if
                  end if

               else
                  !!Utilizing these masks to eliminate data:
                  !! + earthmask
                  !! + zenmask
                  !! + view_mask [only if npass > 1]
                  !! + model domain mask
                  !! + patch mask
                  !! + thinning

                  allocate( allmask_p( &
                     this_view % ys_local:this_view % ye_local, &
                     this_view % xs_local:this_view % xe_local ) )
                  allmask_p = this_view % patchmask ( &
                     this_view % ys_local:this_view % ye_local, &
                     this_view % xs_local:this_view % xe_local, 1 )

                  allocate( readmask_p( &
                     this_view % ys_local:this_view % ye_local, &
                     this_view % xs_local:this_view % xe_local ) )
                  readmask_p = this_view % patchmask ( &
                     this_view % ys_local:this_view % ye_local, &
                     this_view % xs_local:this_view % xe_local, 2 )

                  ! Only use locations where this view is nearest to this fgat time
                  ! - only available when FD data present for any fgat time
                  if (  use_view_mask ) then
                     if ( .not.any( &
                          view_mask ( this_view % ys_p_fd:this_view % ye_p_fd, &
                                      this_view % xs_p_fd:this_view % xe_p_fd, &
                                      iview, ichan, ifgat ) &
                                      ) ) then
                        deallocate(allmask_p, readmask_p)
                        write(unit=stdout,fmt='(3A,I0)') &
                           '          ZERO pixels selected for ',trim(this_view % name),' on band ', channel_list(ichan)
                        this_view % nfiles_used(ifgat) = this_view % nfiles_used(ifgat) - 1
                        cycle ChannelLoop
                     end if
                     do n = 1, this_view % nrad_on_patch
                        iy = this_view % iy_1d % patch (n)
                        ix = this_view % ix_1d % patch (n)
                        iyfd = iy + this_view % yoff_fd-1
                        ixfd = ix + this_view % xoff_fd-1

                        allmask_p( iy, ix ) = &
                           ( allmask_p( iy, ix ) .and. view_mask( iyfd, ixfd, iview, ichan, ifgat) )

                        readmask_p( iy, ix ) = &
                           ( readmask_p( iy, ix ) .and. view_mask( iyfd, ixfd, iview, ichan, ifgat) )
                     end do
                  end if

                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  !! Read radiance and convert to brightness temp.
                  !! once per permutation of
                  !! + INST VIEW (FD, CONUS, MESOx2)
                  !! + fgat
                  !! + channel/band
                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  write(unit=stdout,fmt='(A,I0,A,I0)') &
                     '          Reading ', count(readmask_p), ' abi radiances for band ',channel_list(ichan)
                  if ( use_clddet_zz) write(unit=stdout,fmt='(A,I0)') &
                     '          which includes the cloud detection halo'
                  TEMPIR_ifile = -1
                  if ( use_clddet_zz .and. channel_list(ichan).eq.14 ) then
                     ! Require earlier file to be withn 1/2 of TEMPIR_delay_minutes
                     TEMPIR_min_time_diff = TEMPIR_delay_minutes
!write(unit=stdout,fmt='(A,F14.2)') &
!                     '    ref_time (min): ', this_view % filedate(ifile) % obs_time / 60.D0 - TEMPIR_delay_minutes
                     do jfile = 1, this_view % nfiles
                        if ( this_view % filechan(jfile) .ne. channel_list(ichan) .or. &
                             jfile .eq. ifile ) cycle

                        TEMPIR_time_abs_diff = &
                            abs( this_view % filedate(jfile) % obs_time / 60.D0 - &
                                (this_view % filedate(ifile) % obs_time / 60.D0 - TEMPIR_delay_minutes) )

                        if ( TEMPIR_time_abs_diff .lt. TEMPIR_min_time_diff ) then
                           TEMPIR_ifile = jfile
                           TEMPIR_min_time_diff = TEMPIR_time_abs_diff
                        end if
                     end do
                     if ( TEMPIR_min_time_diff .gt. 0.5 * TEMPIR_delay_minutes ) then
!                        write(unit=stdout,fmt='(A,F7.2,A)') &
!                           '          TEMPIR: minimum time difference is too large - ',TEMPIR_min_time_diff,' minutes'
                        TEMPIR_ifile = -1
!                     else
!                        write(unit=stdout,fmt='(A,F7.2,A)') &
!                           '          TEMPIR: minimum time difference is accetable - ',TEMPIR_min_time_diff,' minutes'
                     end if
                  end if

                  ! Allocate and read bt for this patch and current time
                  if ( TEMPIR_ifile.gt.0 ) then
                     allocate( rad_p ( &
                        this_view % ys_local:this_view % ye_local, &
                        this_view % xs_local:this_view % xe_local, 2 ) )

                     allocate( bt_p ( &
                        this_view % ys_local:this_view % ye_local, &
                        this_view % xs_local:this_view % xe_local, 2 ) )
                  else
                     allocate( rad_p ( &
                        this_view % ys_local:this_view % ye_local, &
                        this_view % xs_local:this_view % xe_local, 1 ) )

                     allocate( bt_p ( &
                        this_view % ys_local:this_view % ye_local, &
                        this_view % xs_local:this_view % xe_local, 1 ) )
                  end if

                  fname = trim(this_view % filename(ifile))
                  call get_abil1b_rad( fname, &
                                       this_view % ys_local, this_view % ye_local, &
                                       this_view % xs_local, this_view % xe_local, &
                                       readmask_p, inst, ichan, &
                                       rad_p(:,:,1), bc1, bc2, fk1, fk2 )

                  bt_p = missing_r
                  where (readmask_p)
                     bt_p(:,:,1) = rad2bt(rad_p(:,:,1), bc1, bc2, fk1, fk2)
                  end where

                  !JJG: It is possible for readmask_p to differ across channels.
                  ! readmask_p needs to be incorporated, but presently causes error between channel reading
                  ! when lining up channels to identical members of linked p list.
                  ! Fixing this will require moving away from linked list including the readmask_p quality
                  ! flag in the datalink_type.
                  ! Presently readmask_p is used internally within get_abil1b_rad to set rad_p=missing_r (works fine)
                  !allmask_p = (allmask_p .and. readmask_p)
                  if ( TEMPIR_ifile.gt.0 ) then
                     fname = trim(this_view % filename(TEMPIR_ifile))
                     call get_abil1b_rad( fname, &
                                          this_view % ys_local, this_view % ye_local, &
                                          this_view % xs_local, this_view % xe_local, &
                                          readmask_p, inst, ichan, &
                                          rad_p(:,:,2), bc1, bc2, fk1, fk2 )

                     where (readmask_p)
                        bt_p(:,:,2) = rad2bt(rad_p(:,:,2), bc1, bc2, fk1, fk2)
                     end where

                     yr = this_view % filedate(TEMPIR_ifile) % yr
                     mt = this_view % filedate(TEMPIR_ifile) % mt
                     dy = this_view % filedate(TEMPIR_ifile) % dy
                     hr = this_view % filedate(TEMPIR_ifile) % hr
                     mn = this_view % filedate(TEMPIR_ifile) % mn
                     sc = this_view % filedate(TEMPIR_ifile) % sc
!                     write(unit=stdout, &
!                        fmt='(A,I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')  &
!                        '          TEMPIR time: ',yr, '-', mt, '-', dy, '_', hr, ':', mn, ':', sc
                  end if

                  first_chan = (this_view % nfiles_used(ifgat).eq.1)

                  !! Write bt, lat, lon, satzen, satazi, solzen, solazi to datalink structures
                  if (first_chan) then
                     p_fgat => p

                     yr = this_view % filedate(ifile) % yr
                     mt = this_view % filedate(ifile) % mt
                     dy = this_view % filedate(ifile) % dy
                     hr = this_view % filedate(ifile) % hr
                     mn = this_view % filedate(ifile) % mn
                     sc = this_view % filedate(ifile) % sc

                     allocate( solzen_1d (this_view % nrad_on_patch) )
                     allocate( solazi_1d (this_view % nrad_on_patch) )

                     call da_get_solar_angles_1d ( yr, mt, dy, hr, mn, sc, &
                        this_view % lat_1d % patch, this_view % lon_1d % patch, &
                        solzen_1d, solazi_1d )

                     if ( use_clddet_zz .and. &
                          abi_halo_width-abi_superob_halfwidth.ge.1) then
                           ! Allocate terrain_hgt using local indices for this view
                           allocate( terrain_hgt ( &
                                             this_view % ys_local:this_view % ye_local, &
                                             this_view % xs_local:this_view % xe_local ) )

                           ! Read terrain file using Full Disk global indices
                           write(*,*)  'DEBUG da_read_obs_ncgoesabi, ys_local, ye_local, yoff_fd-1: ', &
                              this_view % ys_local, this_view % ye_local, this_view % yoff_fd-1
                           write(*,*)  'DEBUG da_read_obs_ncgoesabi, xs_local, xe_local, xoff_fd-1: ', &
                              this_view % xs_local, this_view % xe_local, this_view % xoff_fd-1

                           call get_abil1b_terr( terr_fname, &
                                             this_view % ys_local + this_view % yoff_fd - 1, &
                                             this_view % ye_local + this_view % yoff_fd - 1, &
                                             this_view % xs_local + this_view % xoff_fd - 1, &
                                             this_view % xe_local + this_view % xoff_fd - 1, &
                                             terrain_hgt )

                     end if

                     allocate(thinmask(this_view % ys_p:this_view % ye_p, &
                                       this_view % xs_p:this_view % xe_p))
                     thinmask = .false.
                  else
                     p => p_fgat
                  end if

                  PixelLoop: do n = 1, this_view % nrad_on_patch
                     iy = this_view % iy_1d % patch (n)
                     ix = this_view % ix_1d % patch (n)

                     if (.not. allmask_p( iy, ix )) cycle PixelLoop

                     if (first_chan) then
                        info % lat = this_view % lat_1d % patch (n) ! latitude
                        info % lon = this_view % lon_1d % patch (n) ! longitude
                        num_goesabi_local = num_goesabi_local + 1
                     end if

                     if (thinning) then
                        if (first_chan) then
                           dlat_earth = info % lat
                           dlon_earth = info % lon
                           if (dlon_earth<zero) dlon_earth = dlon_earth+r360
                           if (dlon_earth>=r360) dlon_earth = dlon_earth-r360
                           dlat_earth = dlat_earth * deg2rad
                           dlon_earth = dlon_earth * deg2rad
                           crit = 1.
                           call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
                           if (.not. iuse) then
                              num_goesabi_thinned=num_goesabi_thinned+1
                              thinmask( iy, ix ) = .true.
                              cycle PixelLoop
                           end if
                        else
                           if (thinmask( iy, ix )) cycle PixelLoop
                        end if
                     end if

                     if (first_chan) then
                        num_goesabi_used_fgat(ifgat) = num_goesabi_used_fgat(ifgat) + 1

                        allocate ( p % tb_inv (1:nchan) )
                        allocate ( p % rad_obs (1:nchan) )
                        p % tb_inv                = missing_r
                        p % rad_obs               = missing_r

                        write(unit=info % date_char, &
                           fmt='(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')  &
                           yr, '-', mt, '-', dy, '_', hr, ':', mn, ':', sc
                        if ( allocated(terrain_hgt) ) then
                           info % elv = terrain_hgt( iy, ix )
                        else
                           info % elv = 0.0
                        end if
                        p % info                  = info
                        p % loc                   = this_view % loc_1d % patch (n)

                        p % landsea_mask          = 1  ! ???
                        if (use_view_mask) then
                           p % scanpos            = &
                              ( iy + this_view % yoff_fd-1 - 1) * (nscan+1) / view_att(1) % ny_global
                              ! ??? "scan" position (IS THIS CORRECT? NECESSARY? iFOV?)
                        else
                           p % scanpos            = &
                              ( iy + this_view % yoff_fd-1 - 1) * (nscan+1) / 5424
                              ! ??? "scan" position (IS THIS CORRECT? NECESSARY? iFOV?)
                        end if
                        p % satzen                = this_view % satzen_1d % patch (n)
                        p % satazi                = this_view % satazi_1d % patch (n)
                        p % solzen                = solzen_1d (n)
                        p % solazi                = solazi_1d (n)
                        if ( p % solzen < 0. ) p % solzen = 150.
                        p % sensor_index          = inst
                        p % ifgat                 = ifgat
                     end if

                     ! Super-ob the radiance, then convert to BT for this channel
                     tbuf = abi_superob_halfwidth
                     if (abi_halo_width.ge.tbuf .and. tbuf.gt.0) then
                        ! require that nkeep >= superob_width to filter out bad data
                        nkeep = count ( rad_p ( iy-tbuf:iy+tbuf, ix-tbuf:ix+tbuf, 1 ) .gt. 0.0 )
                        if (nkeep .ge. superob_width) then
                           p % rad_obs(ichan) = sum( pack( &
                                                   rad_p ( iy-tbuf:iy+tbuf, ix-tbuf:ix+tbuf, 1 ), &
                                                   rad_p ( iy-tbuf:iy+tbuf, ix-tbuf:ix+tbuf, 1 ) .gt. 0.0 ) ) &
                                                   / real(nkeep,r_double)
                        end if
                     else
                        ! Extract single pixel BT and radiance value for this channel
                        p % rad_obs(ichan)    = rad_p( iy, ix, 1 )
                     end if
                     if (p % rad_obs(ichan) .gt. 0.0) then
                        p % tb_inv(ichan) = rad2bt(p % rad_obs(ichan), bc1, bc2, fk1, fk2 )
                     end if

                     ! Preprocessing for Cloud Mask (da_qc_goesabi.inc) including
                     !  extracting Tb values from cloud QC buffer
                     if (.not. allocated(p % superob)) then
                        allocate( p % superob(superob_width,superob_width) )
                     end if

                     ! Loops over superob pixels
                     do jsup = 1, superob_width
                     do isup = 1, superob_width
                        iysup = iy + jsup-1-abi_superob_halfwidth
                        ixsup = ix + isup-1-abi_superob_halfwidth
                        if (first_chan) then
                           allocate ( p % superob(isup,jsup) % tb_obs  (1:nchan,1) )
                           allocate ( p % superob(isup,jsup) % cld_qc(1) )
                           allocate ( p % superob(isup,jsup) % cld_qc(1) % tb_stddev_3x3(nchan) )
                        end if
                        p % superob(isup,jsup) % tb_obs(ichan,1) = bt_p( iysup, ixsup, 1 )

                        tbuf = 1
                        if (abi_halo_width-abi_superob_halfwidth.ge.tbuf .and. &
                            bt_p( iysup, ixsup, 1 ).gt.0.0) then
                           nkeep = count ( bt_p ( iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf, 1 ) .gt. 0.0 )
                           if (nkeep .gt. 0) then
                              allocate( tb_temp ( nkeep, 1 ) )
                              tb_temp(:,1) = pack( bt_p ( iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf, 1 ), &
                                                   bt_p ( iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf, 1 ) .gt. 0.0)
                              mu  = sum( tb_temp(:,1) ) / real(nkeep,r_double)
                              sigma = sqrt( sum( (tb_temp(:,1) - mu)**2 ) / real(nkeep,r_double) )
                              deallocate( tb_temp )

                              p % superob(isup,jsup) % cld_qc(1) % tb_stddev_3x3(ichan) = sigma
                           else
                              p % superob(isup,jsup) % cld_qc(1) % tb_stddev_3x3(ichan) = missing_r
                           end if
                           if (channel_list(ichan).eq.14) then

                              if ( allocated(terrain_hgt) ) then
                                 ! Determine sigma_z of terrain height across these pixels
                                 p % superob(isup,jsup) % cld_qc(1) % terr_hgt = terrain_hgt( iysup, ixsup )
                                 nkeep = count ( terrain_hgt ( iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf ) .gt. missing_r )
                                 if (nkeep .gt. 0) then
                                    allocate( tb_temp ( nkeep, 1 ) )
                                    tb_temp(:,1) = pack( terrain_hgt ( iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf ), &
                                                  terrain_hgt ( iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf ) .gt. missing_r)
                                    mu = sum( tb_temp(:,1) ) / real(nkeep,r_double)
                                    sigma = sqrt( sum( (tb_temp(:,1) - mu)**2 ) / real(nkeep,r_double) )
                                    deallocate( tb_temp )

                                    ! Values for RTCT cloud QC
                                    ! - channel 14 and sigma_z (std. dev. of terrain height in km)
                                    !   w/ landmask and lapse rate of 7 K km^-1

                                    temp_max = 0.
                                    do jy = iysup-tbuf, iysup+tbuf
                                    do jx = ixsup-tbuf, ixsup+tbuf
                                       if ( bt_p( jy, jx, 1) .gt. 0. ) &
                                          temp_max = max(temp_max,bt_p( jy, jx, 1 ) )
                                    end do
                                    end do

                                    if (temp_max .gt. missing_r) then
                                       ! Store RTCT
                                       p % superob(isup,jsup) % cld_qc(1) % RTCT = temp_max - bt_p( iysup, ixsup, 1 ) - &
                                                           3.0_r_double * 0.007_r_double * sigma
                                    else
                                       p % superob(isup,jsup) % cld_qc(1) % RTCT = missing_r
                                    end if
                                 else
                                    p % superob(isup,jsup) % cld_qc(1) % RTCT = missing_r
                                 end if
                              else
                                 p % superob(isup,jsup) % cld_qc(1) % RTCT = missing_r
                                 p % superob(isup,jsup) % cld_qc(1) % terr_hgt = missing_r
                              end if

                           end if
                        else
                           p % superob(isup,jsup) % cld_qc(1) % tb_stddev_3x3(ichan) = missing_r
                           if (channel_list(ichan).eq.14) then
                              p % superob(isup,jsup) % cld_qc(1) % RTCT = missing_r
                              p % superob(isup,jsup) % cld_qc(1) % terr_hgt = missing_r
                           end if
                        end if

                        ! Values for RFMFT cloud QC
                        ! - channels 14 and 15
                        tbuf = 10
                        if (abi_halo_width-abi_superob_halfwidth.ge.tbuf .and. &
                            bt_p( iysup, ixsup, 1 ).gt.0.0) then
                           if (channel_list(ichan).eq.14) then
                              p % superob(isup,jsup) % cld_qc(1) % RFMFT_ij = -1

                              !Determine Neighboring Warm Center (NWC) for this pixel
                              temp_max = 0.0
                              do jy = iysup-tbuf, iysup+tbuf
                              do jx = ixsup-tbuf, ixsup+tbuf
                                 if ( bt_p( jy, jx, 1 ) .gt. temp_max ) then
                                    temp_max = bt_p( jy, jx, 1 )
                                    p % superob(isup,jsup) % cld_qc(1) % RFMFT_ij(1) = jy
                                    p % superob(isup,jsup) % cld_qc(1) % RFMFT_ij(2) = jx
                                 end if
                              end do
                              end do
                              p % superob(isup,jsup) % cld_qc(1) % RFMFT = &
                                          bt_p( iysup, ixsup, 1 ) - temp_max
                           end if
                           if (channel_list(ichan).eq.15 .and. &
                               all(p % superob(isup,jsup) % cld_qc(1) % RFMFT_ij.gt.0)) then

                              temp_max = bt_p ( p % superob(isup,jsup) % cld_qc(1) % RFMFT_ij(1), &
                                                p % superob(isup,jsup) % cld_qc(1) % RFMFT_ij(2), 1 )

                              p % superob(isup,jsup) % cld_qc(1) % RFMFT = abs( &
                                 p % superob(isup,jsup) % cld_qc(1) % RFMFT + &
                                 temp_max  - bt_p( iysup, ixsup, 1 ) )

                           end if
                        else
                           if ( any( channel_list(ichan).eq.(/14,15/) ) ) then

                               p % superob(isup,jsup) % cld_qc(1) % RFMFT = missing_r

                               p % superob(isup,jsup) % cld_qc(1) % RFMFT_ij = -1

                           end if
                        end if

                        ! Values for CIRH2O cloud QC
                        !  - channels 10 and 14 for Pearson correlation coefficient of CIRH2O test
                        tbuf = 2
                        if (abi_halo_width-abi_superob_halfwidth.ge.tbuf .and. &
                            bt_p( iysup, ixsup, 1 ).gt.0.0) then

                           if (channel_list(ichan).eq.10) then

                              allocate( p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi ( &
                                 iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf, 2 ) )

                              p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi(:,:,1) = &
                                 bt_p( iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf, 1 )

                           end if
                           if (channel_list(ichan).eq.14 .and. &
                               size(p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi).gt.1) then

                              p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi(:,:,2) = &
                                 bt_p( iysup-tbuf:iysup+tbuf, ixsup-tbuf:ixsup+tbuf, 1 )
                              nkeep = 0
                              do jy = iysup-tbuf, iysup+tbuf
                              do jx = ixsup-tbuf, ixsup+tbuf
                                 if ( all(p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi( jy, jx, : ) .gt. missing_r) ) nkeep = nkeep + 1
                              end do
                              end do
                              allocate( tb_temp ( nkeep, 2 ) )
                              ikeep = 0
                              do jy = iysup-tbuf, iysup+tbuf
                              do jx = ixsup-tbuf, ixsup+tbuf
                                 if ( all(p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi( jy, jx, : ) .gt. missing_r) ) then
                                    ikeep = ikeep + 1
                                    tb_temp(ikeep,1) = &
                                       p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi( jy, jx, 1 )
                                    tb_temp(ikeep,2) = &
                                       p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi( jy, jx, 2 )
                                 end if
                              end do
                              end do

                              mu10 = sum( tb_temp(:,1) ) / real(nkeep,r_double)
                              sigma10 = sqrt( sum( (tb_temp(:,1) - mu10)**2 ) &
                                           / real(nkeep,r_double) )

                              mu14 = sum( tb_temp(:,2) ) / real(nkeep,r_double)
                              sigma14 = sqrt( sum( (tb_temp(:,2) - mu14)**2 ) / &
                                           real(nkeep,r_double) )

                              pearson = sum((tb_temp(:,1) - mu10) * (tb_temp(:,2) - mu14)) / &
                                           real(nkeep,r_double) / ( sigma10 * sigma14 )

                              deallocate( tb_temp )
                              deallocate( p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi )
                              !allocate( p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi (1,1,1) )

                              p % superob(isup,jsup) % cld_qc(1) % CIRH2O = pearson

                           end if
                        else
                           if ( any( channel_list(ichan).eq.(/10,14/) ) ) then

                              if ( allocated( p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi ) ) &
                                 deallocate( p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi)

                              !allocate( p % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi (1,1,1))

                              p % superob(isup,jsup) % cld_qc(1) % CIRH2O = missing_r

                           end if
                        end if

                        ! Values for TEMPIR cloud QC
                        ! - channel 14
                        if ( use_clddet_zz .and. (channel_list(ichan).eq.14) ) then

                           p % superob(isup,jsup) % cld_qc(1) % TEMPIR = missing_r

                           if ( TEMPIR_ifile.gt.0 .and. &
                                bt_p( iysup, ixsup, 1 ).gt.0.0 .and. &
                                bt_p( iysup, ixsup, 2 ).gt.0.0 ) then
                              if ( bt_p( iysup, ixsup, 2 ).lt.330. ) &
                                 p % superob(isup,jsup) % cld_qc(1) % TEMPIR = &
                                    bt_p( iysup, ixsup, 2 ) - bt_p( iysup, ixsup, 1 )
                           end if

                        end if
                     end do ! isup
                     end do ! jsup

                     if (first_chan) &
                        allocate (p % next)   ! add next data

                     p => p % next

                     if (first_chan) &
                        nullify (p % next)

                  end do PixelLoop
                  if ( allocated(bt_p)        ) deallocate ( bt_p )
                  if ( allocated(rad_p)       ) deallocate ( rad_p )
                  if ( allocated(solzen_1d)   ) deallocate ( solzen_1d )
                  if ( allocated(solazi_1d)   ) deallocate ( solazi_1d )
                  if ( allocated(allmask_p)   ) deallocate ( allmask_p )
                  if ( allocated(readmask_p)  ) deallocate ( readmask_p )
               end if VIEW_SELECT
            end do ChannelLoop
            if ( allocated(terrain_hgt) ) deallocate ( terrain_hgt )
            if ( allocated(thinmask)    ) deallocate ( thinmask )
         else
            write(unit=stdout,fmt='(A)') &
               '          No pixels to read within this subdomain. Waiting for other processors...'
         end if PatchMatch

#ifdef DM_PARALLEL
         call mpi_barrier(comm, ierr)
#endif

      end do fgat_loop ! end fgat loop

      if ( (this_view % moving .or. ipass.eq.npass) .and. this_view%nrad_on_patch.gt.0 ) then
         ! Deallocate location info
         deallocate ( this_view % patchmask )
         deallocate ( this_view % lat_1d % patch    )
         deallocate ( this_view % lon_1d % patch    )
         deallocate ( this_view % satzen_1d % patch )
         deallocate ( this_view % satazi_1d % patch )
         deallocate ( this_view % iy_1d % patch     )
         deallocate ( this_view % ix_1d % patch     )
         deallocate ( this_view % loc_1d % patch    )
      end if

      if (ipass .eq. 2) tot_files_used = tot_files_used + sum(view_att(iview) % nfiles_used)

   end do ! end view loop

   end do ! end pass loop

   if ( allocated(view_mask) ) deallocate(view_mask)

   do iview = 1, nviews
      if ( .not.view_att(iview) % select ) cycle
      this_view => view_att(iview)
      deallocate ( this_view % filename )
      deallocate ( this_view % filechan )
      deallocate ( this_view % filedate )
      deallocate ( this_view % file_fgat_match )
      deallocate ( this_view % fgat_time_abs_diff )
      deallocate ( this_view % min_time_diff )
      deallocate ( this_view % nfiles_used )
      if ( allocated( this_view % ny_grid ) ) deallocate ( this_view % ny_grid )
      if ( allocated( this_view % nx_grid ) ) deallocate ( this_view % nx_grid )
      if ( allocated( this_view % ys_grid ) ) deallocate ( this_view % ys_grid )
      if ( allocated( this_view % xs_grid ) ) deallocate ( this_view % xs_grid )
   end do
   deallocate(view_att)

   if (tot_files_used .lt. 1) then
      write(unit=message(1),fmt=*) "Either no L1B data found or no matching fgat windows for GOES-",satellite_id," ABI using prefix ",INST_PREFIX, " for this process rank.  This subdomain may have an unacceptable zenith angle or fall entirely outside the GOES viewing extent."

!      write(unit=message(1),fmt='(A)') "Either no L1B data found or no matching"
!      write(unit=message(2),fmt='(A,I2,A)') "fgat windows for GOES-",satellite_id," ABI using"
!      write(unit=message(3),fmt='(3A)') "prefix ",INST_PREFIX, " for this process rank."
!      write(unit=message(4),fmt='(A)') "This subdomain may have an unacceptable zenith "
!      write(unit=message(5),fmt='(A)') "angle or fall entirely outside the GOES viewing"
!      write(unit=message(6),fmt='(A)') "extent."

      call da_warning(__FILE__,__LINE__, message(1:1))
   end if

#ifdef DM_PARALLEL
   call mpi_allreduce( num_goesabi_local, &
                       num_goesabi_global, &
                       1, mpi_integer, mpi_sum, comm, ierr )
#else
   num_goesabi_global = num_goesabi_local
#endif

!------------------------------------------------------
   ! NOTE: Remainder of this subroutine modified from da_read_obs_ncgoesimg.inc

   if (thinning .and. num_goesabi_global > 0 ) then
#ifdef DM_PARALLEL

      ! Get minimum crit and associated processor index.
      j = 0
      do ifgat = 1, num_fgat_time
            j = j + thinning_grid(inst,ifgat) % itxmax
      end do


      allocate ( in  (j) )
      allocate ( out (j) )
      j = 0
      do ifgat = 1, num_fgat_time
            do i = 1, thinning_grid(inst,ifgat) % itxmax
               j = j + 1
               in(j) = thinning_grid(inst,ifgat) % score_crit(i)
            end do
      end do

      call mpi_reduce(in, out, j, true_mpi_real, mpi_min, root, comm, ierr)

      call wrf_dm_bcast_real (out, j)

      j = 0
      do ifgat = 1, num_fgat_time
            do i = 1, thinning_grid(inst,ifgat) % itxmax
               j = j + 1
               if ( ABS(out(j)-thinning_grid(inst,ifgat) % score_crit(i)) > 1.0D-10 ) thinning_grid(inst,ifgat) % ibest_obs(i) = 0
            end do
      end do
      deallocate( in  )
      deallocate( out )

#endif
      ! Delete the nodes being thinned out
      p => head
      prev => head
      head_found = .false.
      num_goesabi_used_tmp = sum(num_goesabi_used_fgat)

      do j = 1, num_goesabi_used_tmp
         n = p % sensor_index
         ifgat = p % ifgat
         found = .false.

         do i = 1, thinning_grid(n,ifgat) % itxmax
            if ( thinning_grid(n,ifgat) % ibest_obs(i) == j .and. thinning_grid(n,ifgat) % score_crit(i) < 9.99e6_r_double ) then
               found = .true.
               exit
            end if
         end do

         ! free current data
         if ( .not. found ) then
            current => p
            p => p % next
            if ( head_found ) then
               prev % next => p
            else
               head => p
               prev => p
            end if
            deallocate ( current % tb_inv )
            deallocate ( current % rad_obs )
            if ( allocated( current % superob ) ) then
               do jsup = 1, superob_width
               do isup = 1, superob_width
                  deallocate ( current % superob(isup,jsup) % tb_obs )
                  if ( allocated ( current % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi ) ) &
                     deallocate ( current % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi )
                  deallocate ( current % superob(isup,jsup) % cld_qc(1) % tb_stddev_3x3 )
                  deallocate ( current % superob(isup,jsup) % cld_qc )
               end do
               end do
               deallocate ( current % superob )
            end if
            deallocate ( current )
            num_goesabi_thinned = num_goesabi_thinned + 1
            num_goesabi_used_fgat(ifgat) = num_goesabi_used_fgat(ifgat) - 1
            continue
         end if

         if ( found .and. head_found ) then
            prev => p
            p => p % next
            continue
         end if
         if ( found .and. .not. head_found ) then
            head_found = .true.
            head => p
            prev => p
            p => p % next
         end if

      end do

   end if  ! End of thinning
!stop
   num_goesabi_used       = sum(num_goesabi_used_fgat)
   iv % total_rad_pixel   = iv % total_rad_pixel + num_goesabi_used
   iv % total_rad_channel = iv % total_rad_channel + num_goesabi_used*nchan

   iv % info(radiance) % nlocal = iv % info(radiance) % nlocal + num_goesabi_used
   iv % info(radiance) % ntotal = iv % info(radiance) % ntotal + num_goesabi_global

   do i = 1, num_fgat_time
#ifdef DM_PARALLEL
      call mpi_allreduce( num_goesabi_used_fgat(i), &
                          ptotal(i), &
                          1, mpi_integer, mpi_sum, comm, ierr )
#else
      ptotal(i) = num_goesabi_used_fgat(i)
#endif
   end do

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv % info(radiance) % ptotal(i) = iv % info(radiance) % ptotal(i) + ptotal(i)
   end do

#ifdef DM_PARALLEL
   call mpi_allreduce( num_goesabi_thinned, &
                       nthinned, &
                       1, mpi_integer, mpi_sum, comm, ierr )
#else
   nthinned = num_goesabi_thinned
#endif

   if ( iv % info(radiance) % ptotal(num_fgat_time) /= (iv % info(radiance) % ntotal - nthinned) ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal - nthinned:",iv % info(radiance) % ntotal - nthinned," is different from the sum of ptotal:", iv % info(radiance) % ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'num_goesabi_global, num_goesabi_thinned_global, num_goesabi_used_global'
   write(unit=stdout,fmt=*) num_goesabi_global, nthinned, ptotal(num_fgat_time)

   write(unit=stdout,fmt='(a)') 'num_goesabi_local, num_goesabi_thinned, num_goesabi_used'
   write(unit=stdout,fmt=*) num_goesabi_local, num_goesabi_thinned, num_goesabi_used

   !  5.0 allocate innovation radiance structure
   !----------------------------------------------------------------


   if (num_goesabi_used > 0) then
      iv % instid(inst) % num_rad  = num_goesabi_used
      iv % instid(inst) % info % nlocal = num_goesabi_used
      write(unit=stdout,FMT='(a,i3,2x,a,3x,i10)') &
        'Allocating space for radiance innov structure', &
         inst, iv % instid(inst) % rttovid_string, iv % instid(inst) % num_rad
      call da_allocate_rad_iv (inst, nchan, iv)
   end if

   !  6.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   p => head
   do n = 1, num_goesabi_used
      i = p % sensor_index
      call da_initialize_rad_iv (i, n, iv, p)
      current => p
      p => p % next

      ! free current data
      deallocate ( current % tb_inv )
      deallocate ( current % rad_obs )
      if ( allocated ( current % superob ) ) then
         do jsup = 1, superob_width
         do isup = 1, superob_width
            deallocate ( current % superob(isup,jsup) % tb_obs )
            if ( allocated ( current % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi ) ) &
               deallocate ( current % superob(isup,jsup) % cld_qc(1) % CIRH2O_abi )
            deallocate ( current % superob(isup,jsup) % cld_qc(1) % tb_stddev_3x3 )
            deallocate ( current % superob(isup,jsup) % cld_qc )
         end do
         end do
         deallocate ( current % superob )
      end if
      deallocate ( current )
   end do
   deallocate ( p )
   deallocate (ptotal)

#ifdef DM_PARALLEL
   call mpi_barrier(comm, ierr)
#endif

   if (trace_use) call da_trace_exit("da_read_obs_ncgoesabi")

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

   character(*), intent(in)    :: filename
   integer, intent(out)        :: ydim, xdim
   real(r_double), intent(out) :: req, rpol, pph, nam
!!!   real, intent(out)         :: lat_sat, lon_sat

   integer                  :: ierr, ncid, varid, dimid

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
   nam = nam * deg2rad

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
                             yy_abi, xx_abi, &
                             yoff, xoff )

   implicit none

   character(*), intent(in)  :: filename
   integer,      intent(in)  :: ny, nx
   real,         intent(out) :: yy_abi(ny), xx_abi(nx)
   integer,      intent(out) :: yoff, xoff

   integer                  :: ierr, ncid, varid
   real                     :: slp, itp

   if (trace_use) call da_trace_entry("get_abil1b_grid1")

   ierr=nf_open(trim(filename),nf_nowrite,ncid)
   call handle_err('Error opening file',ierr)

   ierr=nf_inq_varid(ncid,'y',varid)

   ierr=nf_get_var_double(ncid,varid,yy_abi)

   ierr=nf_get_att_double(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_double(ncid,varid,'add_offset',itp)
   yy_abi = yy_abi*slp+itp
   yoff = floor(itp/slp)

   ierr=nf_inq_varid(ncid,'x',varid)

   ierr=nf_get_var_double(ncid,varid,xx_abi)

   ierr=nf_get_att_double(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_double(ncid,varid,'add_offset',itp)
   xx_abi = xx_abi*slp+itp
   xoff = floor(itp/slp)

   ierr=nf_close(ncid)
   call handle_err('Error closing file',ierr)

   if (trace_use) call da_trace_exit("get_abil1b_grid1")

end subroutine get_abil1b_grid1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_grid2_1d( yy_abi, xx_abi, req, rpol, pph, nam, satellite_id, &
                             lat, lon, satzen, satazi, &
                             earthmask, zenmask )

   implicit none

   real,              intent(in)  :: yy_abi(:), xx_abi(:)
   real(r_double),    intent(in)  :: req, rpol, pph, nam
   integer,           intent(in)  :: satellite_id

   ! GOES-ABI fields
   real,              intent(out) :: lat(:), lon(:)
   real,              intent(out) :: satzen(:), satazi(:)
   logical,           intent(out) :: earthmask(:), zenmask(:)

   ! Internal Variables
   type(info_type)          :: info
   logical                  :: outside_all, dummy_bool

   integer                  :: iy, ix, n
   real(r_double)           :: hh
   real, parameter          :: satzen_limit=75.0

   if (trace_use) call da_trace_entry("get_abil1b_grid2_1d")

   lat = missing_r
   lon = missing_r
   satzen = missing_r
   satazi = missing_r
   earthmask=.true.
   zenmask=.true.

   hh=pph+req

   call get_abil1b_latlon_1d ( yy_abi, xx_abi, lat, lon, req, rpol, hh, nam )

   where( lat.eq.missing_r .OR. lon.eq.missing_r .OR. &
          isnan(lat)       .OR. isnan(lon) )
      earthmask = .false.
      lat       = missing_r
      lon       = missing_r
   end where

   call da_get_sat_angles_1d( lat, lon, satellite_id, satzen, satazi )

   where ( isnan(satzen) .or. satzen.gt.satzen_limit .or. satzen.eq.missing_r )
      satzen  = missing_r
      zenmask = .false.
   end where

   if (trace_use) call da_trace_exit("get_abil1b_grid2_1d")

end subroutine get_abil1b_grid2_1d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_rad( filename, ys, ye, xs, xe, radmask, inst, ichan, &
                           radout, bc1, bc2, fk1, fk2 )
   implicit none

   character(*), intent(in)      :: filename

   !Size of full data set

   !Starting and stopping indices of this view desired (not equivalent to Full Disk indices)
   integer, intent(in)           :: ys, ye, xs, xe
   integer, intent(in)           :: inst, ichan

   logical, intent(inout)        :: radmask( ys:ye, xs:xe )
   real, intent(out)             :: radout( ys:ye, xs:xe )
   real, intent(out)             :: bc1, bc2, fk1, fk2

   real             :: rad(xs:xe, ys:ye)
   integer          :: DQF(xs:xe, ys:ye)

   integer          :: ierr, ncid, varid
   integer          :: iy, ix
   integer          :: nykeep, nxkeep
   real             :: slp, itp

   if (trace_use) call da_trace_entry("get_abil1b_rad")

   rad = missing_r

   !! Save rad reading time by selecting a subset of netcdf var
   nykeep = ye - ys + 1
   nxkeep = xe - xs + 1

   if (nykeep.le.0 .or. nxkeep.le.0) then
      radmask = .false.
      return
   end if

   ierr=nf_open(trim(filename),nf_nowrite,ncid)

   call handle_err('Error opening file',ierr)

   ierr=nf_inq_varid( ncid, 'Rad', varid )
   ierr=nf_get_vara_double ( ncid, varid, (/xs,ys/), (/nxkeep,nykeep/), rad )
   ierr=nf_get_att_double(ncid,varid,'scale_factor',slp)
   ierr=nf_get_att_double(ncid,varid,'add_offset',itp)
   rad=rad*slp+itp

   ierr=nf_inq_varid ( ncid, 'DQF', varid )
   ierr=nf_get_vara_int ( ncid, varid, (/xs,ys/), (/nxkeep,nykeep/), DQF )

   ierr=nf_inq_varid( ncid, 'planck_bc1', varid )
   ierr=nf_get_var_double( ncid, varid, bc1 )
   ierr=nf_inq_varid( ncid, 'planck_bc2', varid )
   ierr=nf_get_var_double( ncid, varid, bc2 )
   ierr=nf_inq_varid( ncid, 'planck_fk1', varid )
   ierr=nf_get_var_double( ncid, varid, fk1 )
   ierr=nf_inq_varid( ncid, 'planck_fk2', varid )
   ierr=nf_get_var_double( ncid, varid, fk2 )

   radmask = ( radmask .and. (transpose(DQF).eq.0 .or. transpose(DQF).eq.1) )
   radmask = ( radmask .and. transpose(rad).gt.0.0 )

   radout = missing_r
   where ( radmask )
      radout = transpose(rad)
   end where

   ierr=nf_close(ncid)
   call handle_err('Error closing file',ierr)

   if (trace_use) call da_trace_exit("get_abil1b_rad")

end subroutine get_abil1b_rad

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

elemental function rad2bt( rad, bc1, bc2, fk1, fk2 ) result(bt)
   implicit none

   real, intent(in) :: rad
   real, intent(in) :: bc1, bc2, fk1, fk2

   real :: bt

   bt = ( fk2 / ( log(( fk1 / rad ) + 1.0) ) - bc1 ) / bc2

end function rad2bt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

elemental function bt2rad( bt, bc1, bc2, fk1, fk2 ) result(rad)
   implicit none

   real, intent(in) :: bt
   real, intent(in) :: bc1, bc2, fk1, fk2

   real :: rad

   rad = fk1 / ( exp( fk2 / (bc1 + bc2 * bt)) - 1.0 )

end function bt2rad

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_terr( filename, ys, ye, xs, xe, terr )
   implicit none

   character(*), intent(in)      :: filename

   !Size of full data set

   !Starting and stopping indices of this view desired (not equivalent to Full Disk indices)
   integer, intent(in)           :: ys, ye, xs, xe
   real, intent(out)             :: terr( ys:ye, xs:xe ) ! unit = meters

   real             :: terr_trans( xs:xe, ys:ye ) ! unit = meters
   integer          :: ncid, varid
   integer          :: nykeep, nxkeep
   real             :: terr_miss

   if (trace_use) call da_trace_entry("get_abil1b_terr")

   terr = missing_r

   !! Save rad reading time by selecting a subset of netcdf var
   nykeep = ye - ys + 1
   nxkeep = xe - xs + 1

   if (nykeep.le.0 .or. nxkeep.le.0) then
      return
   end if

   call handle_err ( 'Error opening file', &
                     nf_open(trim(filename),nf_nowrite,ncid) )
   call handle_err ( 'Error getting terr ID', &
                     nf_inq_varid( ncid, 'terr', varid ) )

   write(*,*)  'DEBUG get_abil1b_terr, xs, ys, xs+nxkeep, ys+nykeep: ',xs,ys,xs+nxkeep,ys+nykeep

   call handle_err ( 'Error reading terrain height', &
                     nf_get_vara_double ( ncid, varid, (/xs,ys/), (/nxkeep,nykeep/), terr_trans ) )
   terr = transpose(terr_trans)

   call handle_err ( 'Error with _FillValue', &
                     nf_get_att_double(ncid, varid, '_FillValue', terr_miss) )

   where ( terr .le. terr_miss ) &
      terr = missing_r

   call handle_err('Error closing file', &
                   nf_close(ncid) )

   if (trace_use) call da_trace_exit("get_abil1b_terr")

end subroutine get_abil1b_terr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_latlon_1d( yy_abi, xx_abi, lat, lon, req, rpol, hh, nam )

   implicit none

   real, intent(in)    :: yy_abi(:), xx_abi(:)
   real, intent(in)    :: req, rpol, hh, nam
   real, intent(inout) :: lat(:), lon(:)

   real, allocatable   :: lat1(:), lon1(:)
   real, allocatable   :: aa(:), bb(:), cc(:), rs(:), sx(:), sy(:), sz(:)
   real, allocatable   :: radicand(:)
   integer             :: n

   if (trace_use) call da_trace_entry("get_abil1b_latlon_1d")

   n = size(yy_abi)

   allocate ( lat1( n ) )
   allocate ( lon1( n ) )
   allocate ( aa( n ) )
   allocate ( bb( n ) )
   allocate ( cc( n ) )
   allocate ( rs( n ) )
   allocate ( sx( n ) )
   allocate ( sy( n ) )
   allocate ( sz( n ) )
   allocate ( radicand( n ) )

   aa = sin( xx_abi )**2 + cos( xx_abi )**2 * ( cos( yy_abi )**2 + req**2/rpol**2 * sin( yy_abi )**2 )

   bb = -2.D0 * hh * cos( xx_abi ) * cos( yy_abi )

   cc = hh**2-req**2

   radicand = bb ** 2 - 4.D0 * aa * cc

   where ( radicand .ge. 0. )
      rs   = ( -bb - sqrt( radicand ) ) / ( 2.D0 * aa )
      sx   =  rs * cos( xx_abi ) * cos( yy_abi )
      sy   = -rs * sin( xx_abi )
      sz   =  rs * cos( xx_abi ) * sin( yy_abi )

      lat1 = atan( req**2 / rpol**2 * sz / sqrt( ( hh - sx )**2 + sy**2) )
      lon1 = nam - atan( sy / ( hh - sx ) )

      lat  = lat1 / deg2rad
      lon  = lon1 / deg2rad
   end where

   deallocate ( lat1, lon1, aa, bb, cc, rs, sx, sy, sz, radicand )

   if (trace_use) call da_trace_exit("get_abil1b_latlon_1d")

end subroutine get_abil1b_latlon_1d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_abil1b_latlon( yy_abi, xx_abi, lat, lon, req, rpol, hh, nam )

   implicit none

   real, intent(in)    :: yy_abi, xx_abi
   real, intent(in)    :: req, rpol, hh, nam
   real, intent(inout) :: lat,lon

   real   :: lat1,lon1
   real   :: aa,bb,cc,rs,sx,sy,sz
   real   :: radicand

   if (trace_use) call da_trace_entry("get_abil1b_latlon")

   aa = sin( xx_abi )**2 + cos( xx_abi )**2 * ( cos( yy_abi )**2 + req**2/rpol**2 * sin( yy_abi )**2)
   bb = -2.D0*hh * cos( xx_abi ) * cos( yy_abi )
   cc = hh**2 - req**2

   radicand = bb **2 - 4.D0 * aa * cc
   if (radicand .lt. 0.) return

   rs   = ( -bb - sqrt( radicand ) )/(2.D0 * aa)
   sx   =  rs * cos( xx_abi ) * cos( yy_abi )
   sy   = -rs * sin( xx_abi )
   sz   =  rs * cos( xx_abi ) * sin( yy_abi )

   lat1 = atan( req**2/rpol**2 * sz / sqrt( ( hh - sx )**2 + sy**2) )
   lon1 = nam-atan( sy / ( hh - sx ) )

   lat  = lat1 / deg2rad
   lon  = lon1 / deg2rad

   if (trace_use) call da_trace_exit("get_abil1b_latlon")

end subroutine get_abil1b_latlon

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef DM_PARALLEL
subroutine split_grid( ny_global, nx_global, &
                       ny_grid, nx_grid, &
                       ys_grid, xs_grid )
   implicit none

   integer, intent(in)  :: ny_global, nx_global
   integer, intent(out) :: ny_grid(num_procs), nx_grid(num_procs), &
                           ys_grid(num_procs), xs_grid(num_procs)

   integer, target   :: ny_vec(ntasks_y), ys_vec(ntasks_y)  !, ye_vec(ntasks_y)
   integer, target   :: nx_vec(ntasks_x), xs_vec(ntasks_x)  !, xe_vec(ntasks_x)
   integer, pointer  :: nvec(:), svec(:)

   integer           :: mm, i, j, ii, iproc, igrid, ntasks, nglobal, fact

   do igrid = 1, 2
      if (igrid.eq.1) then
         nvec => ny_vec
         svec => ys_vec
         ntasks = ntasks_y
         nglobal = ny_global
      else if (igrid.eq.2) then
         nvec => nx_vec
         svec => xs_vec
         ntasks = ntasks_x
         nglobal = nx_global
      end if

      nvec = nglobal / ntasks
      mm = mod( nglobal , ntasks )
      do j = 1, ntasks
         if ( mm .eq. 0 ) exit
         nvec(j) = nvec(j) + 1
         mm = mm - 1
      end do

      svec(1) = 1
      do j = 1, ntasks
         if (j .lt. ntasks) then
            svec(j+1) = svec(j) + nvec(j)
         end if
      end do
   end do

   iproc = 0
   do j = 1, ntasks_y
   do i = 1, ntasks_x
      iproc = iproc + 1
      ny_grid(iproc) = ny_vec(j)
      ys_grid(iproc) = ys_vec(j)
      nx_grid(iproc) = nx_vec(i)
      xs_grid(iproc) = xs_vec(i)
   end do
   end do

end subroutine split_grid
#endif

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

  real(r_double), intent(in)     :: jmod
  integer, intent(out)           :: yr,mt,dy,hr,mn
  integer, intent(out), optional :: sc

  real(r_double) :: ju, j0, F
  integer      :: yr0, sc0
  INTEGER :: A, B, C, D, E, Z, ALPHA   ! intermediate variables
  real(r_double) :: dd

  ! Conversion to Julian day from MJD reference time: 1978 Jan 01 00:12:00 (see da_get_julian_time)
  real(r_double), parameter :: jd_jmod = 2443510.0

  ! Convert to days
  ju = jmod / 1440.D0

  !! Convert reference MJD to actual Julian time
  ju = ju+jd_jmod
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

