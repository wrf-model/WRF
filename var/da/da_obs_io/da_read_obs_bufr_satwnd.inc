subroutine da_read_obs_bufr_satwnd(filename, iv, ob)

implicit none

character(len=*), intent(in)    :: filename
type (iv_type),   intent(inout) :: iv
type (y_type),    intent(inout) :: ob

integer, parameter :: i_kind   = selected_int_kind(8)
integer, parameter :: r_double = selected_real_kind(15) ! double precision
integer, parameter :: r_kind   = selected_real_kind(15) ! double precision

integer(i_kind), parameter :: missing_i = -999
real(r_kind),    parameter :: missing_r = -999.0

integer(i_kind), parameter :: nstring   = 50
integer(i_kind), parameter :: nmeta     = 3  ! number of metadata
integer(i_kind), parameter :: nvars     = 2  ! number of obs variables

character(len=nstring), dimension(nmeta) :: name_meta = &
   (/                        &
      'air_pressure       ', &
      'latitude           ', &
      'longitude          '  &
   /)

character(len=nstring), dimension(nvars) :: name_vars = &
   (/                        &
      'eastward_wind      ', &
      'northward_wind     '  &
   /)

integer(i_kind) :: idx_p = 1
integer(i_kind) :: idx_lat = 2
integer(i_kind) :: idx_lon = 3
integer(i_kind) :: idx_u = 1
integer(i_kind) :: idx_v = 2

type datalink_satwnd
   character(len=nstring)    :: stid       ! station identifier
   character(len=nstring)    :: name       ! instrument name
   character(len=nstring)    :: date_char  ! ccyy-mm-dd_hh:mm:ss
   integer(i_kind)           :: satid      ! satellite identifier
   integer(i_kind)           :: rptype     ! prepbufr report type
   integer(i_kind)           :: ifgat      ! index of time slot
   integer(i_kind)           :: itg        ! index of thinning grid
   integer(i_kind)           :: qm         ! quality marker
   real(r_double)            :: tdiff      ! time difference between obs_time and analysis_time
   real(r_kind)              :: err        ! ob error
   real(r_kind)              :: lat        ! latitude in degree
   real(r_kind)              :: lon        ! longitude in degree
   real(r_kind)              :: prs        ! pressure
   real(r_kind)              :: landsea    ! land sea qualifier 0:land, 1:sea, 2:coast
   real(r_kind)              :: satzen     ! satellite zenith angle in degree
   real(r_kind)              :: wspd
   real(r_kind)              :: wdir
   real(r_kind)              :: uwind
   real(r_kind)              :: vwind
   real(r_kind)              :: cvwd       ! coefficient of variation
   real(r_kind)              :: pccf1      ! percent confidence, Quality Index without forecast (qifn)
   real(r_kind)              :: pccf2      ! percent confidence, Estimated Error (EE) in m/s converted to percent confidence

   type (datalink_satwnd), pointer :: next ! pointer to next data
end type datalink_satwnd

type(datalink_satwnd), pointer :: rhead=>null(), rlink=>null()

real(r_kind), parameter  :: r8bfms = 9.0E08  ! threshold to check for BUFR missing value
real(r_kind), parameter  :: pi = acos(-1.0)

integer(i_kind), parameter :: ntime = 6      ! number of data to read in timestr
integer(i_kind), parameter :: ninfo = 5      ! number of data to read in infostr
integer(i_kind), parameter :: nlalo = 2      ! number of data to read in lalostr
integer(i_kind), parameter :: ndata = 3      ! number of data to read in datastr
integer(i_kind), parameter :: nqc1  = 2      ! number of data to read in qc1str
integer(i_kind), parameter :: nqc2  = 2      ! number of data to read in qc2str

character(len=80) :: timestr, infostr, lalostr, datastr, qc1str, qc2str

real(r_double), dimension(ntime)  :: timedat
real(r_double), dimension(ninfo)  :: infodat
real(r_double), dimension(nlalo)  :: lalodat
real(r_double), dimension(ndata)  :: wdata
real(r_double), dimension(nqc1,2) :: qc1dat
real(r_double), dimension(nqc2,4) :: qc2dat

integer(i_kind), parameter :: nmsgtyp = 10  ! number of message types to process
! message types that are not processed into prepbufr
character(len=8), dimension(nmsgtyp) :: message_types = &
   (/ 'NC005030',  &
      'NC005031',  &
      'NC005032',  &
      'NC005034',  &
      'NC005039',  &
      'NC005080',  &
      'NC005081',  &  ! not tested
      'NC005090',  &
      'NC005091',  &
      'NC005072'   &
   /)
character(len=8)  :: subset

integer(i_kind) :: iunit, iost, iret
integer(i_kind) :: k, kx, ilev, itype, ivar
integer(i_kind) :: idate
integer(i_kind) :: num_report_infile
integer(i_kind) :: ireadmg, ireadsb

integer(i_kind) :: iyear, imonth, iday, ihour, imin, isec
real(r_double)  :: obs_time, analysis_time

logical      :: outside, outside_all
logical      :: use_errtable
real(r_kind) :: coef
real(r_kind) :: pob
real(r_kind) :: xlat, xlon, prs
real(r_kind) :: angearth
type(info_type) :: xinfo
type(model_loc_type) :: xloc1

integer,                  allocatable :: itmp(:)
character(len=nstring),   allocatable :: str_stid(:)
character(len=nstring),   allocatable :: str_name(:)
character(len=nstring),   allocatable :: date_char(:)     ! date_time string in wrf format
type(model_loc_type),     allocatable :: xloc(:)
logical,                  allocatable :: inside_domain(:)
logical,                  allocatable :: inside_patch(:)
integer(i_kind),          allocatable :: iwindow(:)
integer(i_kind),          allocatable :: rptype(:)
real(r_kind),    allocatable :: tdiff_avg(:)
real(r_kind),    allocatable :: num_val(:)
real(r_kind),    allocatable :: num_err(:)
real(r_kind),    allocatable :: num_meta(:)
real(r_kind),    allocatable :: sdata_val(:,:)
real(r_kind),    allocatable :: sdata_err(:,:)
real(r_kind),    allocatable :: smeta(:,:)
character(len=14) :: cdate14       ! ccyymmddhhnnss used in da_advance_time
character(len=10) :: adate10       ! ccyymmddhh     used in da_advance_time
character(len=14) :: tdiff_char

integer(i_kind) :: i, ii, kk, n, isize
integer(i_kind) :: num_outside_domain, num_outside_time
integer(i_kind) :: num_qc_fail
integer(i_kind) :: ntotal, nlocal, max_lev, nlev, nlevel
integer(i_kind) :: itotal, ilocal, ifgat
integer(i_kind) :: ntotal_ifgat(0:num_fgat_time)
logical         :: fexist

real(r_kind) :: err_val
real(r_kind) :: uob, vob
real(r_kind) :: u_grid, v_grid

! for thinning
real(r_kind)    :: dlat_earth, dlon_earth, crit
integer(i_kind) :: nobs, icnt, num_date
integer(i_kind) :: iobs, itt, itx, iout
integer(i_kind) :: num_thinned, num_keep
logical :: iuse
real(r_kind)    :: xval
real(r_kind)    :: c1, c2
! for superobbing in vertical
real(r_kind)    :: zk
integer(i_kind) :: nplev
real(r_kind), allocatable :: plev(:)

continue ! end of declaration

inquire(file=trim(filename), exist=fexist)
if ( .not. fexist ) then
   write(unit=message(1),fmt='(a)') trim(filename)//' does not exist.'
   call da_warning(__FILE__,__LINE__,message(1:1))
   return
end if

if ( thin_conv_opt(polaramv) == thin_single ) then
   thin_conv_opt(polaramv) = thin_multi
   write(unit=message(1),fmt='(a,2i5)') 'thin_conv_opt(polaramv)=1 is not implemented, resetting to thin_conv_opt(polaramv)=2'
   call da_message(message(1:1))
end if

timestr = 'YEAR MNTH DAYS HOUR MINU SECO'
infostr = 'SAID LSQL SAZA OGCE SWCM'
lalostr = 'CLATH CLONH'
datastr = 'PRLC WDIR WSPD'
qc1str  = 'TCOV CVWD'
qc2str  = 'GNAPS PCCF'

! oetab(300,33,6) processed in da_setup_obs_structures_bufr.inc
use_errtable = allocated(oetab)

num_report_infile  = 0

allocate (itmp(1)) ! for broadcasting nobs

! only rootproc reads from file
! thinning check is also done on rootproc only
if ( rootproc ) then

   write(unit=message(1),fmt='(a)') 'Reading from '//trim(filename)
   call da_message(message(1:1))

   ! open bufr file
   call da_get_unit(iunit)
   open (unit=iunit, file=trim(filename), &
         iostat=iost, form='unformatted', status='old')
   if (iost /= 0) then
      write(unit=message(1),fmt='(a,i5,a)') &
         "Error",iost," opening BUFR obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      return
   end if

   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readmg(iunit,subset,idate,iret)

   if ( iret /= 0 ) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iret," reading BUFR obs file "//trim(filename)
      call closbf(iunit)
      close(iunit)
      call da_free_unit(iunit)
      call da_warning(__FILE__,__LINE__,message(1:1))
      return
   end if
   rewind(iunit)

   write(unit=message(1),fmt='(a,i10)') 'da_read_obs_bufr_satwnd: '//trim(filename)//' file date is: ', idate
   call da_message(message(1:1))

   read (analysis_date,'(i4,4(1x,i2))') iyear, imonth, iday, ihour, imin
   call da_get_julian_time (iyear,imonth,iday,ihour,imin,analysis_time)
   adate10 = analysis_date(1:4)//analysis_date(6:7)//analysis_date(9:10)//analysis_date(12:13)

   ! initialize counters
   num_outside_domain = 0
   num_outside_time = 0
   num_qc_fail = 0
   num_thinned = 0
   num_keep = 0

   if ( thin_conv_opt(polaramv) > no_thin ) then
      call cleangrids_conv(polaramv)
      iobs = 0 ! initialize an intent(inout) variable used by map2grids_conv
      ! itxmax is calculated in subroutine make3grids called in da_setup_obs_structures_bufr
      ! itxmax is horizontal thinning grids when thin_conv_opt/=thin_superob_hv
      ! itxmax is horizontal*vertical thinning grids when thin_conv_opt==thin_superob_hv
      write(unit=message(1),fmt='(a,i8)') &
        'da_read_obs_bufr_satwnd: number of thinning grids = ', thinning_grid_conv(polaramv)%itxmax
      call da_message(message(1:1))
      if ( thin_conv_opt(polaramv) == thin_superob_hv ) then
         !to-do: nplev and plev should be set in make3grids and stored in thinning_grid_conv structure
         nplev = int(1200/thin_mesh_vert_conv(polaramv))
         allocate (plev(nplev))
         do k = 1, nplev
            plev(k) = 1200.0 - (k-1)*thin_mesh_vert_conv(polaramv)
         end do
      end if
   end if ! thinning

   if ( .not. associated(rhead) ) then
      nullify ( rhead )
      allocate ( rhead )
      nullify ( rhead%next )
   end if

   if ( .not. associated(rlink) ) then
      rlink => rhead
   else
      allocate ( rlink%next )
      rlink => rlink%next
      nullify ( rlink%next )
   end if

   msg_loop: do while (ireadmg(iunit,subset,idate)==0)
!print*,subset
      if ( ufo_vars_getindex(message_types, subset) <= 0 ) then
         ! skip types other than GOES-16/17, AVHRR (METOP/NOAA), VIIRS (NPP/NOAA-20) AMVs
         ! that are included in prepbufr
         cycle msg_loop
      end if
      read_loop_subset: do while (ireadsb(iunit)==0)

         num_report_infile = num_report_infile + 1

         call fill_datalink(rlink, missing_r, missing_i)

         call ufbint(iunit,timedat,ntime,1,iret,timestr)

         iyear  = nint(timedat(1))
         imonth = nint(timedat(2))
         iday   = nint(timedat(3))
         ihour  = nint(timedat(4))
         imin   = nint(timedat(5))
         isec   = min(59, nint(timedat(6))) ! raw BUFR data that has SECO = 60.0 SECOND
                                            ! that was probably rounded from 59.x seconds
                                            ! reset isec to 59 rather than advancing one minute
         write(unit=rlink%date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
            iyear, '-', imonth, '-', iday, '_', ihour, ':', imin, ':', isec
         call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
         if ( obs_time < time_slots(0) .or.  &
              obs_time >= time_slots(num_fgat_time) ) then
            num_outside_time = num_outside_time + 1
            cycle read_loop_subset
         end if
         ! determine fgat index
         do ifgat = 1, num_fgat_time
            if ( obs_time >= time_slots(ifgat-1) .and.  &
                 obs_time  < time_slots(ifgat) ) then
                exit
            end if
         end do
         rlink%ifgat = ifgat
         if ( num_fgat_time > 1 ) then
            rlink%tdiff = obs_time + float(isec)/60.0 - &
               (time_slots(0) + (ifgat-1)*(time_slots(num_fgat_time)-time_slots(0))/float(num_fgat_time-1))
         else
            rlink%tdiff = obs_time + float(isec)/60.0 - analysis_time
         end if
         rlink%tdiff = rlink%tdiff/60.0 ! minute to hour

         if ( subset == 'NC005080' .or. &
              subset == 'NC005090' ) then
            call ufbint(iunit,lalodat,nlalo,1,iret,'CLAT CLON')
         else
            call ufbint(iunit,lalodat,nlalo,1,iret,lalostr)  ! CLATH CLONH
         end if

         xlat = lalodat(1)
         xlon = lalodat(2)
         if ( abs(xlat) > 90.0 .or. abs(xlon) > 360.0 ) cycle read_loop_subset
         if ( xlon > 180.0 ) xlon = xlon - 360.0
         xinfo%lat = xlat
         xinfo%lon = xlon
         xinfo%lat = max(xinfo%lat, -89.995)
         xinfo%lat = min(xinfo%lat,  89.995)
         call da_llxy(xinfo, xloc1, outside, outside_all)
         if ( outside_all ) then
            num_outside_domain = num_outside_domain + 1
            cycle read_loop_subset
         end if
         rlink % lat = xlat
         rlink % lon = xlon

         call ufbint(iunit,infodat,ninfo,1,iret,infostr)  ! SAID LSQL SAZA OGCE SWCM
         call ufbint(iunit,wdata,ndata,1,iret,datastr)    ! PRLC WDIR WSPD
         call ufbrep(iunit,qc1dat,nqc1,2,iret,qc1str)     ! 2 replications TCOV CVWD
         call ufbrep(iunit,qc2dat,nqc2,4,iret,qc2str)     ! 4 replications GNAPS PCCF

         if ( wdata(1) > r8bfms ) cycle read_loop_subset  ! missing pressure

         rlink % satid  = nint(infodat(1))  ! SAID satellite identifier
         ! construct rptype and stid from subset and satid
         call set_rptype_satwnd(subset, rlink%satid, rlink%rptype, rlink%stid, rlink%name)

         if ( wdata(1) < r8bfms ) rlink % prs  = wdata(1)   ! PRLC pressure in Pa
         if ( wdata(2) < r8bfms ) rlink % wdir = wdata(2)
         if ( wdata(3) < r8bfms ) rlink % wspd = wdata(3)

         if ( infodat(4) < r8bfms ) rlink % landsea = infodat(4)  ! LSQL land sea qualifier 0:land, 1:sea, 2:coast
         if ( infodat(5) < r8bfms ) rlink % satzen  = infodat(5)  ! SAZA satellite zenith angle (degree)

         if ( qc1dat(2,1) < r8bfms ) rlink % cvwd  = qc1dat(2,1)
         if ( qc2dat(2,2) < r8bfms ) rlink % pccf1 = qc2dat(2,2)
         if ( qc2dat(2,4) < r8bfms ) rlink % pccf2 = qc2dat(2,4)

         call filter_obs_satwnd(rlink%qm, rlink%rptype, rlink%prs, &  ! set rlink%qm
            rlink%satzen, rlink%landsea, rlink%pccf1, rlink%pccf2, rlink%cvwd, rlink%wspd)

         if ( rlink%qm > qmarker_retain ) then
            num_qc_fail = num_qc_fail + 1
            cycle read_loop_subset
         end if

         if ( thin_conv_opt(polaramv) > no_thin ) then
            !crit = abs(tdiff(ii))
            crit = 1.0
            dlat_earth = rlink%lat
            dlon_earth = rlink%lon
            if ( dlon_earth < 0.0 ) dlon_earth = dlon_earth + 360.0
            if ( dlon_earth >= 360.0 ) dlon_earth = dlon_earth - 360.0
            dlat_earth = dlat_earth * deg2rad
            dlon_earth = dlon_earth * deg2rad
            if ( thin_conv_opt(polaramv) /= thin_superob_hv ) then
               call map2grids_conv(polaramv,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
            else
               zk = get_zk(nplev, plev, rlink%prs*0.01) ! 0.01 for Pa to hPa
               call map2grids_conv(polaramv,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse,zk=zk,thin_3d=.true.,nlev=nplev)
            end if
            if ( thin_conv_opt(polaramv) /= thin_superob .and. &
                 thin_conv_opt(polaramv) /= thin_superob_hv ) then
               if ( .not. iuse ) then
                  num_thinned = num_thinned + 1
                  if ( print_detail_obs ) then
                     write(unit=message(1),fmt='(a,2(1x,f8.3),1x,f8.3,a)')  &
                        rlink%stid, rlink%lat, rlink%lon, rlink%prs*0.01, '  -> thinned'
                  end if
                  cycle read_loop_subset
               end if
            end if ! thin_conv_opt /= thin_superob, thin_superob_hv
            rlink % itg = itx
         end if ! thin_conv_opt(polaramv) > nothin

         num_keep = num_keep + 1

         if ( rlink%wdir >= 0.0 .and. rlink%wdir <= 360.0 .and. &
              rlink%wspd < r8bfms ) then
            angearth = rlink%wdir * pi / 180.0
            rlink%uwind = -1.0 * rlink%wspd * sin(angearth)
            rlink%vwind = -1.0 * rlink%wspd * cos(angearth)
         end if

         if ( use_errtable ) then
            kx = rlink % rptype
            pob = rlink % prs * 0.01 ! Pa to hPa
            do ilev = 1, 32
               if ( pob >= oetab(kx,ilev+1,1) .and. pob <= oetab(kx,ilev,1) ) then
                  coef = (pob-oetab(kx,ilev,1))/(oetab(kx,ilev,1)-oetab(kx,ilev+1,1))
                  rlink % err = (1.0+coef)*oetab(kx,ilev,4)-coef*oetab(kx,ilev+1,4) !uv
                  exit
               end if
            end do
            ! in case errors are not set for some report types, use uv_error_val from namelist
            if ( rlink % err > 100.0 ) rlink % err = uv_error_val(polaramv)  ! m/s
         else
            rlink % err = uv_error_val(polaramv)  ! m/s
         end if

         allocate ( rlink%next )
         rlink => rlink%next
         nullify ( rlink%next )

      end do read_loop_subset ! ireadsb
   end do msg_loop ! ireadmg

   if ( thin_conv_opt(polaramv) == thin_superob_hv ) then
      deallocate (plev)
   end if

   call closbf(iunit)
   close(iunit)
   call da_free_unit(iunit)

   write(message(1),'(a,a,a,i10)') 'da_read_obs_bufr_satwnd: num_report_infile ', trim(filename), ' : ', num_report_infile
   call da_message(message(1:1))
   if ( thin_conv_opt(polaramv) == thin_superob .or. &
        thin_conv_opt(polaramv) == thin_superob_hv ) then
      write(unit=message(1),fmt='(a, 5i8)') &
        'da_read_obs_bufr_satwnd: num_outside_domain, num_outside_time, num_qc_fail, num_superob = ', &
         num_outside_domain, num_outside_time, num_qc_fail, iobs
      call da_message(message(1:1))
   else
      write(unit=message(1),fmt='(a, 5i8)') &
        'da_read_obs_bufr_satwnd: num_outside_domain, num_outside_time, num_qc_fail, num_thinned = ', &
         num_outside_domain, num_outside_time, num_qc_fail, num_thinned
      call da_message(message(1:1))
   end if
   if ( thin_conv_opt(polaramv) == no_thin .or. &
        thin_conv_opt(polaramv) == thin_multi ) then
      nobs = num_keep
   else
      nobs = iobs
   end if

   itmp(1) = nobs
end if ! rootproc

call wrf_dm_bcast_integer(itmp, 1)
nobs = itmp(1)
deallocate (itmp)

!print*, 'nobs = ', nobs
if ( nobs <= 0 ) then
   write(unit=message(1),fmt='(a)') ' No valid obs found'
   call da_warning(__FILE__,__LINE__,message(1:1))
   return
end if

! superobbing is done on rootproc
if ( rootproc) then

   allocate (sdata_val(nobs, nvars))
   allocate (sdata_err(nobs, nvars))
   allocate (smeta(nobs, nmeta))
   allocate (iwindow(nobs))
   allocate (date_char(nobs))
   allocate (str_stid(nobs))
   allocate (str_name(nobs))

   if ( thin_conv_opt(polaramv) == no_thin .or. &
        thin_conv_opt(polaramv) == thin_multi ) then
      allocate (rptype(nobs))
      icnt = 0
      rlink => rhead
      do while ( associated(rlink) )
         icnt = icnt + 1
         sdata_val(icnt,idx_u) = rlink%uwind
         sdata_val(icnt,idx_v) = rlink%vwind
         sdata_err(icnt,idx_u) = rlink%err
         sdata_err(icnt,idx_v) = rlink%err
         smeta(icnt,idx_p) = rlink%prs
         smeta(icnt,idx_lat) = rlink%lat
         smeta(icnt,idx_lon) = rlink%lon
         iwindow(icnt) = rlink%ifgat
         rptype(icnt) = rlink%rptype
         date_char(icnt) = rlink%date_char
         str_stid(icnt) = rlink%stid
         str_name(icnt) = rlink%name
         rlink => rlink%next
      end do
   else if ( thin_conv_opt(polaramv) == thin_superob .or. &
             thin_conv_opt(polaramv) == thin_superob_hv ) then
      sdata_val = missing_r  ! initialize
      sdata_err = missing_r  ! initialize
      smeta = missing_r  ! initialize
      allocate (tdiff_avg(nobs))
      tdiff_avg = 0.0
      iwindow = -1
      iobs = 0 ! re-use this counter
      allocate (num_val(nvars))
      allocate (num_err(nvars))
      allocate (num_meta(nmeta))
      ! loop over thinning grids
      itx_loop: do i = 1, thinning_grid_conv(polaramv)%itxmax
         if ( thinning_grid_conv(polaramv)%icount(i) <= 0 ) cycle itx_loop
         ! increment the counter for the thinning grid that has obs
         iobs = iobs + 1
         ! reset counter for obs within each thinning grid
         icnt = 0
         num_date = 0
         num_val(:) = 0
         num_err(:) = 0
         num_meta(:) = 0
         ! loop over nobs to process the obs within the thinning grid
         rlink => rhead
         nobs_loop_1: do while ( associated(rlink) )
            if ( rlink%itg /= i ) then
               rlink => rlink%next
               cycle nobs_loop_1
            end if
            icnt = icnt + 1
            if ( icnt == 1 ) then ! first ob in the thinning grid
              ifgat = rlink%ifgat
              str_stid(iobs) = rlink%stid
              str_name(iobs) = rlink%name
            end if
            ! only average obs in the same time window
            if ( rlink%ifgat /= ifgat ) then
               rlink => rlink%next
               cycle nobs_loop_1
            end if
            ! average dates
            num_date = num_date + 1
            c1 = 1.0/num_date
            c2 = (num_date-1)*c1
            tdiff_avg(iobs) = c2*tdiff_avg(iobs) + rlink%tdiff*c1
            do ivar = 1, nvars
               if ( trim(name_vars(ivar)) == 'eastward_wind' ) then
                  xval = rlink%uwind
               else if ( trim(name_vars(ivar)) == 'northward_wind' ) then
                  xval = rlink%vwind
               end if
               if ( abs(xval-missing_r) > 0.1 ) then
                  num_val(ivar) = num_val(ivar) + 1
                  c1 = 1.0/num_val(ivar)
                  c2 = (num_val(ivar)-1)*c1
                  sdata_val(iobs, ivar) = c2*sdata_val(iobs, ivar) + xval*c1
               end if
               xval = rlink%err
               if ( abs(xval-missing_r) > 0.1 ) then
                  num_err(ivar) = num_err(ivar) + 1
                  c1 = 1.0/num_err(ivar)
                  c2 = (num_err(ivar)-1)*c1
                  sdata_err(iobs, ivar) = c2*sdata_err(iobs, ivar) + xval*c1
               end if
            end do
            do ivar = 1, nmeta
               if ( trim(name_meta(ivar)) == 'air_pressure' ) then
                  xval = rlink%prs
               else if ( trim(name_meta(ivar)) == 'latitude' ) then
                  xval = rlink%lat
               else if ( trim(name_meta(ivar)) == 'longitude' ) then
                  xval = rlink%lon
               end if
               if ( abs(xval-missing_r) > 0.1 ) then
                  num_meta(ivar) = num_meta(ivar) + 1
                  c1 = 1.0/num_meta(ivar)
                  c2 = (num_meta(ivar)-1)*c1
                  smeta(iobs, ivar) = c2*smeta(iobs, ivar) + xval*c1
               end if
            end do
            if ( icnt == thinning_grid_conv(polaramv)%icount(i) ) then
               rlink => null()
               exit nobs_loop_1
            end if
            rlink => rlink%next
         end do nobs_loop_1
         iwindow(iobs) = ifgat
         tdiff_char = ''
         write(tdiff_char,'(i4,a1)') int(tdiff_avg(iobs)*60.0), 'm'
         call da_advance_time(adate10, trim(adjustl(tdiff_char)), cdate14)
         date_char(iobs) = cdate14(1:4)//'-'//cdate14(5:6)//'-'//cdate14(7:8)// &
                           '_'//cdate14(9:10)//':'//cdate14(11:12)//':'//cdate14(13:14)
      end do itx_loop
      deallocate (num_meta)
      deallocate (num_err)
      deallocate (num_val)
      deallocate (tdiff_avg)
   end if ! thin_conv_opt(polaramv)

   ! done with rlink
   ! release the linked list
   rlink => rhead
   do while ( associated(rlink) )
      rhead => rlink%next
      if ( associated (rlink) ) deallocate (rlink)
      rlink => rhead
   end do
   nullify (rhead)

end if ! rootproc

if ( .not. allocated(sdata_val) ) then
   allocate (sdata_val(nobs, nvars))
end if
isize = nobs*nvars
call wrf_dm_bcast_real(sdata_val, isize)
if ( .not. allocated(sdata_err) ) then
   allocate (sdata_err(nobs, nvars))
end if
call wrf_dm_bcast_real(sdata_err, isize)
if ( .not. allocated(smeta) ) then
   allocate (smeta(nobs, nmeta))
end if
isize = nobs*nmeta
call wrf_dm_bcast_real(smeta, isize)
isize = nobs
if ( .not. allocated(iwindow) ) then
   allocate (iwindow(nobs))
end if
call wrf_dm_bcast_integer(iwindow, isize)
if ( thin_conv_opt(polaramv) == no_thin .or. &
     thin_conv_opt(polaramv) == thin_multi ) then
   if ( .not. allocated(rptype) ) then
      allocate (rptype(nobs))
   end if
   call wrf_dm_bcast_integer(rptype, isize)
end if
if ( .not. allocated(str_stid) ) then
   allocate (str_stid(nobs))
end if
if ( .not. allocated(str_name) ) then
   allocate (str_name(nobs))
end if
if ( .not. allocated(date_char) ) then
   allocate (date_char(nobs))
end if
do i = 1, nobs
   call wrf_dm_bcast_string(str_stid(i), nstring)
   call wrf_dm_bcast_string(str_name(i), nstring)
   call wrf_dm_bcast_string(date_char(i), nstring)
end do

allocate (xloc(nobs))
allocate (inside_domain(nobs))
allocate (inside_patch(nobs))
inside_domain(:) = .false.
inside_patch(:)  = .false.

nlocal = 0
ntotal = 0
ntotal_ifgat(0:num_fgat_time) = 0

check_loop_2: do ii = 1, nobs

   ! check loc
   if ( abs(smeta(ii,idx_lat)) > 90.0 .or. abs(smeta(ii,idx_lon)) > 360.0 ) then
      cycle check_loop_2
   end if
   if ( smeta(ii,idx_lon) > 180.0 ) smeta(ii,idx_lon) = smeta(ii,idx_lon) - 360.0
   xinfo%lat = smeta(ii,idx_lat)
   xinfo%lon = smeta(ii,idx_lon)
   xinfo%lat = max(xinfo%lat, -89.995)
   xinfo%lat = min(xinfo%lat,  89.995)
   call da_llxy(xinfo, xloc(ii), outside, outside_all)
   if ( outside_all ) then
      cycle check_loop_2
   end if
   inside_domain(ii) = .true.
   ifgat = iwindow(ii)
   if ( ifgat < 0 ) cycle check_loop_2
   ntotal = ntotal + 1
   ntotal_ifgat(ifgat) = ntotal_ifgat(ifgat) + 1
   if ( outside ) then
      cycle check_loop_2
   end if
   inside_patch(ii) = .true.
   nlocal = nlocal + 1

end do check_loop_2

! transfer data arrays to iv structures

iv%info(polaramv)%ntotal = ntotal
iv%info(polaramv)%nlocal = nlocal
max_lev = 1
iv%info(polaramv)%max_lev = max_lev

!allocate iv and iv%info
if ( nlocal > 0 ) then
   allocate (iv%polaramv(1:nlocal))
   allocate (iv%info(polaramv)%name(nlocal))
   allocate (iv%info(polaramv)%platform(nlocal))
   allocate (iv%info(polaramv)%id(nlocal))
   allocate (iv%info(polaramv)%date_char(nlocal))
   allocate (iv%info(polaramv)%levels(nlocal))
   allocate (iv%info(polaramv)%lat(max_lev,nlocal))
   allocate (iv%info(polaramv)%lon(max_lev,nlocal))
   allocate (iv%info(polaramv)%elv(nlocal))
   allocate (iv%info(polaramv)%pstar(nlocal))
   allocate (iv%info(polaramv)%slp(nlocal))
   allocate (iv%info(polaramv)%pw(nlocal))
   allocate (iv%info(polaramv)%x  (kms:kme,nlocal))
   allocate (iv%info(polaramv)%y  (kms:kme,nlocal))
   allocate (iv%info(polaramv)%i  (kms:kme,nlocal))
   allocate (iv%info(polaramv)%j  (kms:kme,nlocal))
   allocate (iv%info(polaramv)%dx (kms:kme,nlocal))
   allocate (iv%info(polaramv)%dxm(kms:kme,nlocal))
   allocate (iv%info(polaramv)%dy (kms:kme,nlocal))
   allocate (iv%info(polaramv)%dym(kms:kme,nlocal))
   allocate (iv%info(polaramv)%k  (max_lev,nlocal))
   allocate (iv%info(polaramv)%dz (max_lev,nlocal))
   allocate (iv%info(polaramv)%dzm(max_lev,nlocal))
   allocate (iv%info(polaramv)%zk (max_lev,nlocal))
   allocate (iv%info(polaramv)%proc_domain(max_lev,nlocal))
   allocate (iv%info(polaramv)%thinned(max_lev,nlocal))
   allocate (iv%info(polaramv)%obs_global_index(nlocal))
   iv%info(polaramv)%elv(:)            = missing_r
   iv%info(polaramv)%proc_domain(:,:)  = .false.
   iv%info(polaramv)%thinned(:,:)      = .false.
   iv%info(polaramv)%zk(:,:)           = missing_r
   iv%info(polaramv)%slp(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
   iv%info(polaramv)%pw(:)  = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)

   nlev = 1
   do ilocal = 1, nlocal
      allocate (iv%polaramv(ilocal)%p(1:nlev))
      allocate (iv%polaramv(ilocal)%u(1:nlev))
      allocate (iv%polaramv(ilocal)%v(1:nlev))
      iv%polaramv(ilocal)%p(:) = missing_r
      iv%polaramv(ilocal)%u(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
      iv%polaramv(ilocal)%v(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
   end do
end if

ilocal = 0
itotal = 0

do kk = 1, num_fgat_time

   iv%info(polaramv)%ptotal(kk)=0

   nobs_loop_2: do ii=1, nobs

      if ( .not. inside_domain(ii) ) cycle nobs_loop_2
      itotal = itotal + 1

      if ( .not. inside_patch(ii)  ) cycle nobs_loop_2

      if ( iwindow(ii) /= kk ) then
         cycle nobs_loop_2
      else
         ilocal = ilocal+1
         iv%info(polaramv)%platform(ilocal) = 'FM-88 SATWND'
         if ( thin_conv_opt(polaramv) == thin_superob .or. &
              thin_conv_opt(polaramv) == thin_superob_hv ) then
            iv%info(polaramv)%name(ilocal) = 'satwnd'
            iv%info(polaramv)%id(ilocal)   = 'SUPEROB'
         else
            write(iv%info(polaramv)%name(ilocal), '(a,a,i3)') &
               trim(str_name(ii)),' ',rptype(ii)
            iv%info(polaramv)%id(ilocal)        = str_stid(ii)
         end if
         iv%info(polaramv)%date_char(ilocal) = date_char(ii)
         iv%info(polaramv)%levels(ilocal)    = nlev  ! = 1
         iv%info(polaramv)%lat(:,ilocal)     = smeta(ii,idx_lat)
         iv%info(polaramv)%lon(:,ilocal)     = smeta(ii,idx_lon)

         iv%info(polaramv)%x(:,ilocal)       = xloc(ii)%x
         iv%info(polaramv)%y(:,ilocal)       = xloc(ii)%y
         iv%info(polaramv)%i(:,ilocal)       = xloc(ii)%i
         iv%info(polaramv)%j(:,ilocal)       = xloc(ii)%j
         iv%info(polaramv)%dx(:,ilocal)      = xloc(ii)%dx
         iv%info(polaramv)%dxm(:,ilocal)     = xloc(ii)%dxm
         iv%info(polaramv)%dy(:,ilocal)      = xloc(ii)%dy
         iv%info(polaramv)%dym(:,ilocal)     = xloc(ii)%dym

         iv%info(polaramv)%obs_global_index(ilocal) = itotal

         iv%polaramv(ilocal)%p(1) = smeta(ii,idx_p)

         uob  = sdata_val(ii,idx_u)
         if ( abs(uob - missing_r) > 1.0 ) then
            iv%polaramv(ilocal)%u(1)%inv = uob
            iv%polaramv(ilocal)%u(1)%qc  = 0
            err_val = sdata_err(ii,idx_u)
            if ( err_val > 0.0 ) then
               iv%polaramv(ilocal)%u(1)%error = err_val
            end if
         end if
         vob  = sdata_val(ii,idx_v)
         if ( abs(vob - missing_r) > 1.0 ) then
            iv%polaramv(ilocal)%v(1)%inv = vob
            iv%polaramv(ilocal)%v(1)%qc  = 0
            err_val = sdata_err(ii,idx_v)
            if ( err_val > 0.0 ) then
               iv%polaramv(ilocal)%v(1)%error = err_val
            end if
         end if

         if ( uv_error_opt(polaramv) == error_opt_nml ) then
            iv%polaramv(ilocal)%u(1)%error = uv_error_val(polaramv)
            iv%polaramv(ilocal)%v(1)%error = uv_error_val(polaramv)
         end if

         ! u/v-earth to u/v-grid
         if ( abs(uob - missing_r) > 1.0 .and. &
              abs(vob - missing_r) > 1.0 ) then
            call da_earth_2_model_wind ( uob, vob, u_grid, v_grid, &
                                         iv%info(polaramv)%lon(1,ilocal) )
            iv%polaramv(ilocal)%u(1)%inv = u_grid
            iv%polaramv(ilocal)%v(1)%inv = v_grid
         end if

      end if ! inside time window
   end do nobs_loop_2

   ntotal_ifgat(kk) = ntotal_ifgat(kk) + ntotal_ifgat(kk-1)
   iv%info(polaramv)%ptotal(kk) = ntotal_ifgat(kk)
   iv%info(polaramv)%plocal(kk) = ilocal
end do ! num_fgat_time

deallocate (inside_patch)
deallocate (inside_domain)
deallocate (xloc)
if ( thin_conv_opt(polaramv) == no_thin .or. &
     thin_conv_opt(polaramv) == thin_multi ) then
   deallocate (rptype)
end if
deallocate (iwindow)
deallocate (smeta)
deallocate (sdata_err)
deallocate (sdata_val)
deallocate (str_stid)
deallocate (str_name)

! transfer iv to a smaller ob structure

ob % nlocal(polaramv) = iv%info(polaramv)%nlocal
ob % ntotal(polaramv) = iv%info(polaramv)%ntotal
if ( nlocal > 0) then
   allocate (ob % polaramv(1:nlocal))
   do n = 1, nlocal
      nlevel = iv%info(polaramv)%levels(n)
      allocate (ob % polaramv(n)%u(1:nlevel))
      allocate (ob % polaramv(n)%v(1:nlevel))
      do k = 1, nlevel
         ob % polaramv(n) % u(k) = iv % polaramv(n) % u(k) % inv
         ob % polaramv(n) % v(k) = iv % polaramv(n) % v(k) % inv
      end do
   end do
end if

contains

!--------------------------------------------------------------

subroutine filter_obs_satwnd(qm, rptype, prs, satzen, landsea, pccf1, pccf2, cvwd, wspd)

! refer to GSI/read_satwnd.f90

implicit none

integer(i_kind), intent(out) :: qm
integer(i_kind), intent(in)  :: rptype
real(r_kind),    intent(in)  :: prs
real(r_kind),    intent(in)  :: satzen
real(r_kind),    intent(in)  :: landsea
real(r_kind),    intent(in)  :: pccf1
real(r_kind),    intent(in)  :: pccf2
real(r_kind),    intent(in)  :: cvwd
real(r_kind),    intent(in)  :: wspd

real(r_kind) :: experr_norm
logical :: EC_AMV_QC = .true.
integer(i_kind) :: iland = 0

qm = 2  ! neutral or not checked

if ( satzen > 68.0_r_kind ) qm = 15

if ( rptype == 260 ) then
   if ( pccf1 < 85.0_r_kind ) qm = 15
end if

if ( rptype == 240 .or. &
     rptype == 245 .or. &
     rptype == 246 .or. &
     rptype == 247 .or. &
     rptype == 251 ) then

   if ( pccf1 < 80.0_r_kind .or. pccf1 > 100.0_r_kind ) qm = 15 ! reject data with low QI

   if ( prs < 12500.0_r_kind ) qm = 15 ! reject data above 125hPa

   experr_norm = 10.0_r_kind - 0.1_r_kind * pccf2
   if ( wspd > 0.1_r_kind ) then
      experr_norm = experr_norm / wspd
   else
      experr_norm = 100.0_r_kind
   end if
   if ( experr_norm > 0.9_r_kind ) qm = 15 ! reject data with estimated error/spd>0.9

   if ( rptype == 240 .or. &
        rptype == 245 .or. &
        rptype == 246 .or. &
        rptype == 251 ) then
      if ( cvwd < 0.04_r_kind ) qm = 15
      if ( cvwd > 0.50_r_kind ) qm = 15
   end if

   if ( EC_AMV_QC ) then
      if ( pccf1 < 90_r_kind .or. pccf1 > 100.0_r_kind ) qm = 15 ! stricter QI
      if ( prs < 15000.0_r_kind) qm = 15 ! all high level
      if ( rptype == 251 .and. prs < 70000.0_r_kind ) qm = 15  ! VIS
      if ( rptype == 246 .and. prs > 30000.0_r_kind ) qm = 15  ! WVCA
      if ( nint(landsea) == iland .and. prs > 85000.0_r_kind) qm = 15  ! low over land
   end if
end if  ! rptype 240, 245, 246, 247, 251

end subroutine filter_obs_satwnd

!--------------------------------------------------------------

subroutine fill_datalink (datalink, rfill, ifill)

implicit none

type (datalink_satwnd), intent(inout) :: datalink
real(r_kind),           intent(in)    :: rfill    ! fill value in real
integer(i_kind),        intent(in)    :: ifill    ! fill value in integer

integer(i_kind) :: i

datalink % lat      = rfill
datalink % lon      = rfill
datalink % satid    = ifill
datalink % itg      = ifill
datalink % ifgat    = ifill
datalink % landsea  = rfill
datalink % satzen   = rfill
datalink % prs      = rfill
datalink % wdir     = rfill
datalink % wspd     = rfill
datalink % cvwd     = rfill
datalink % uwind    = rfill
datalink % vwind    = rfill
datalink % pccf1    = rfill
datalink % pccf2    = rfill
datalink % err      = rfill
datalink % qm       = ifill

end subroutine fill_datalink

!--------------------------------------------------------------

subroutine set_rptype_satwnd(subset, satid, rptype, stid, name)

implicit none
character(len=*), intent(in)  :: subset  ! bufr message subset
integer(i_kind),  intent(in)  :: satid   ! satellite id
integer(i_kind),  intent(out) :: rptype  ! report type
character(len=nstring), intent(out) :: stid    ! station id
character(len=nstring), intent(out) :: name    ! instrument name

character(len=3) :: csatid

rptype = -1
stid   = ''
write(csatid, '(i3.3)') satid

select case ( trim(subset) )
case ( 'NC005030' )  ! GOES IR LW
   rptype = 245
   stid = 'IR'//csatid
   name = 'GOES IR LW'
case ( 'NC005039' )  ! GOES IR SW
   rptype = 240
   stid = 'IR'//csatid
   name = 'GOES IR SW'
case ( 'NC005032' )  ! GOES VIS
   rptype = 251
   stid = 'VI'//csatid
   name = 'GOES VIS'
case ( 'NC005034' )  ! GOES WV cloud top
   rptype = 246
   stid = 'WV'//csatid
   name = 'GOES WV cloud top'
case ( 'NC005031' )  ! GOES WV clear sky/deep layer
   rptype = 247
   stid = 'WV'//csatid
   name = 'GOES WV clear sky/depp layer'
case ( 'NC005080', 'NC005081' )  ! AVHRR (METOP-a/b/c, NOAA-15/18/19)
   rptype = 244
   stid = 'IR'//csatid
   name = 'AVHRR'
case ( 'NC005090', 'NC005091' )  ! VIIRS (NPP, NOAA-20)
   rptype = 260
   stid = 'IR'//csatid
   name = 'VIIRS'
case ( 'NC005072' )  ! LEOGEO (non-specific mixture of geostationary and low earth orbiting satellites)
   rptype = 255
   stid = 'IR'//csatid  ! satid=854
   name = 'LEOGEO'
end select

end subroutine set_rptype_satwnd

!--------------------------------------------------------------

integer function ufo_vars_getindex(vars, varname)
implicit none
character(len=*), intent(in) :: vars(:)
character(len=*), intent(in) :: varname

integer :: ivar

ufo_vars_getindex = -1

do ivar = 1, size(vars)
  if (trim(vars(ivar)) == trim(varname)) then
    ufo_vars_getindex = ivar
    exit
  endif
enddo

end function ufo_vars_getindex

!--------------------------------------------------------------

real function get_zk (nlev, levels, val)

implicit none
integer, intent(in)  :: nlev
real,    intent(in)  :: levels(nlev)
real,    intent(in)  :: val
integer              :: k
integer              :: level_index
logical              :: decreasing

if ( levels(nlev) < levels(1) ) then
   decreasing = .true.
else
   decreasing = .false.
end if

level_index = 0 ! initialize

if ( decreasing ) then
   if ( val >= levels(1) ) then
      level_index = 1
   else if ( val < levels(nlev-1) .and. val >= levels(nlev) ) then
      level_index = nlev
   else
      do k = 2, nlev-1
         if ( val >= levels(k) ) then
            level_index = k
            exit
         end if
      end do
   end if
else
   if ( val <= levels(1) ) then
      level_index = 1
   else if ( val > levels(nlev-1) .and. val <= levels(nlev) ) then
      level_index = nlev
   else
      do k = 2, nlev-1
         if ( val <= levels(k) ) then
            level_index = k
            exit
         end if
      end do
   end if
end if ! decreasing or not

get_zk = real(level_index) + &
         (val-levels(level_index))/(levels(level_index+1)-levels(level_index))

end function get_zk

end subroutine da_read_obs_bufr_satwnd
