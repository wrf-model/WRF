program da_rad_diags
!
! Author: Hui-Chuan Lin
!
! program to read multiple-time of radiance innovation files and write out in
! netcdf format for ncl time-series plotting
!
! input files: (1)  namelist.da_rad_diags
!                   &record1
!                    nproc = 16   (the proc numbers used when inv files were written out)
!                    instid = 'dmsp-16-ssmis'   (inst names, can be more than one instid)
!                    file_prefix = "inv"
!                    start_date = '2005082000'
!                    end_date   = '2005083018'
!                    cycle_period  = 6
!                   /
!              (2) inv_* or oma_* from wrfvar

   implicit none

#include "netcdf.inc"
!
! namelist variables
!
   namelist /record1/ nproc, instid, file_prefix, start_date, end_date, cycle_period
           ! nproc: number of processsors used when writing out inv files
           ! instid, eg dmsp-16-ssmis
           ! file_prefix, inv or oma
           ! start_date, end_date, eg 2006100100, 2006102800
           ! cycle_period (hours) between dates, eg 6 or 12
   integer, parameter                     :: maxnum = 20, maxlvl = 100
   integer                                :: nml_unit = 87
   integer                                :: nproc, nlev, ilev, ich
   integer                                :: cycle_period, nlev_rtm, nlev_mdl
   character(len=20), dimension(maxnum)   :: instid
   character(len=6)                       :: file_prefix
   character(len=10)                      :: start_date, end_date
!
! netcdf variables
!
   character(len=200)                     :: ncname
   integer                                :: ncid, dimid, varid
   integer, dimension(3)                  :: ishape, istart, icount
!
   logical                                :: amsr2
   logical                                :: isfile, prf_found, jac_found
   integer, parameter                     :: datelen1 = 10
   integer, parameter                     :: datelen2 = 19
   real*4, parameter                      :: missing_r = -888888.00
   character(len=20)                      :: rtm_option    ! CRTM or RTTOV
   character(len=250)                     :: buf, inst
   character(len=7)                       :: numbuf
   character(len=datelen1)                :: valid_date
   integer                                :: ninst, iinst, iproc, ipixel, ifirst
   integer                                :: ios, i, n, ips, ipe, nerr, itime, itmp
   integer                                :: ntime, nchan, total_npixel
   integer, dimension(:), allocatable     :: ichan, npixel, iunit, scanpos, isflg
   integer, dimension(:), allocatable     :: landsea_mask, soiltyp, vegtyp
   real*4,  dimension(:), allocatable     :: lat, lon, elv, elev
   real*4,  dimension(:), allocatable     :: ret_clw
   real*4,  dimension(:), allocatable     :: satzen, satazi, t2m, mr2m, u10, v10, ps, ts
   real*4,  dimension(:), allocatable     :: smois, tslb, snowh, vegfra, clwp
   integer, dimension(:,:), allocatable   :: tb_qc
   real*4,  dimension(:,:), allocatable   :: tb_obs, tb_bak, tb_inv, tb_oma, tb_err, ems, ems_jac
   real*4,  dimension(:,:), allocatable   :: prf_pfull, prf_phalf, prf_t, prf_q, prf_water
   real*4,  dimension(:,:), allocatable   :: prf_ice, prf_rain, prf_snow, prf_grau, prf_hail
   real*4,  dimension(:,:), allocatable   :: prf_water_reff, prf_ice_reff, prf_rain_reff
   real*4,  dimension(:,:), allocatable   :: prf_snow_reff, prf_grau_reff, prf_hail_reff
   real*4,  dimension(:,:), allocatable   :: rtm_prf_p, rtm_prf_t, rtm_prf_q
   real*4,  dimension(:,:), allocatable   :: mdl_prf_p, mdl_prf_t, mdl_prf_q, mdl_prf_qcw, mdl_prf_qrn
   real*4,  dimension(:,:,:), allocatable :: prf_t_jac, prf_q_jac, prf_der_trans, prf_trans_jac, prf_trans, prf_lod_jac, prf_lod
   real*4,  dimension(:,:,:), allocatable :: prf_water_jac, prf_ice_jac, prf_rain_jac, prf_snow_jac, prf_grau_jac, prf_hail_jac
   real*4,  dimension(:,:,:), allocatable :: prf_water_reff_jac, prf_ice_reff_jac, prf_rain_reff_jac
   real*4,  dimension(:,:,:), allocatable :: prf_snow_reff_jac, prf_grau_reff_jac, prf_hail_reff_jac
   character(len=200), dimension(:), allocatable      :: inpname
   character(len=datelen1), dimension(:), allocatable :: datestr1          ! date string
   character(len=datelen2), dimension(:), allocatable :: datestr2          ! date string
!
! initialize some variables

   instid   = ''
   ninst    = 0
   file_prefix = 'inv'
   ilev = 0
   nlev = 0
   nlev_rtm = 0
   nlev_mdl = 0
   prf_found = .false.
   jac_found = .false.
   rtm_option = 'unknown'
!
! read namelist
!
   open(unit=nml_unit, file='namelist.da_rad_diags', status='old', form='formatted')
   read(unit=nml_unit, nml=record1, iostat=ios)
   write(0,nml=record1)
   if ( ios /= 0 ) then
      write(0,*) 'Error reading namelist record1'
      stop
   end if
!
! find out how many instruments to process
!
   do i = 1, maxnum
      if ( len_trim(instid(i)) /= 0 ) then
         ninst = ninst + 1
      end if
   end do
!
! find out how many dates to process
!
   ntime = 0
   valid_date = start_date
   do while ( valid_date <= end_date )
      ntime = ntime + 1
      call advance_cymdh(valid_date, cycle_period, valid_date)
   end do
   write(0,*) 'ntime = ', ntime

   allocate ( datestr1(ntime) )
   valid_date = start_date
   datestr1(1) = start_date
   do i = 2, ntime
      call advance_cymdh(datestr1(i-1), cycle_period, datestr1(i))
   end do

ntime_loop: do itime = 1, ntime

   write(0,*) '=================='
   write(0,*) trim(datestr1(itime))

   ninst_loop: do iinst = 1, ninst

      write(0,*) '------------------'
      write(0,*) trim(instid(iinst))

      amsr2 = index(instid(iinst),'amsr2') > 0

      nerr = 0
      total_npixel = 0
      ips = 0
      ipe = 0

      allocate ( npixel(0:nproc-1) )
      allocate ( iunit(0:nproc-1) )
      allocate ( inpname(0:nproc-1) )

      nproc_loop_1: do iproc = 0, nproc - 1   ! loop for first getting number of pixels from each proc

         write(unit=inpname(iproc), fmt='(a,i4.4)')  &
            trim(datestr1(itime))//'/'//trim(adjustl(file_prefix))//'_'//trim(instid(iinst))//'.', iproc
         iunit(iproc) = 101 + iproc
         inquire(file=trim(inpname(iproc)), exist=isfile)
         if ( .not. isfile ) Then
            write(0,*) 'Error opening innovation radiance file ', trim(inpname(iproc))
            nerr = nerr + 1
            if ( nerr == nproc ) then
               write(0,*) 'found no vaild files for ', trim(instid(iinst))
               deallocate ( npixel )
               deallocate ( iunit )
               deallocate ( inpname )
               cycle ninst_loop
            end if
            cycle nproc_loop_1
         end if
 
         open(unit=iunit(iproc),file=trim(inpname(iproc)),form='formatted',iostat=ios)
         read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf   ! first read in one line
 
         inst = buf(1:(index(buf,'number-of-pixels')-2))   ! retrieve inst name
         !
         ! retrieve number of pixels
         !
         numbuf = buf((index(buf,'channel-number')-8):(index(buf,'channel-number')-2))
         read(numbuf,'(i7)') npixel(iproc)

         total_npixel = total_npixel + npixel(iproc)

         itmp = 0
         do while ( ios == 0 .and. itmp < 2 )
            read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf
            if ( index(buf,'INFO :') > 0 ) then
               itmp = itmp + 1
            else
               if ( index(buf,'EMS_JACOBIAN') > 0 ) then
                  jac_found = .true.
               end if
               if ( index(buf,'level fullp(mb)') > 0 ) then
                  prf_found = .true.
                  rtm_option = 'CRTM'
               end if
               if ( index(buf,'RTM_level pres(mb)') > 0 ) then
                  prf_found = .true.
                  rtm_option = 'RTTOV'
               end if
            end if
            if ( rtm_option /= 'unknown' ) exit
         end do

      end do nproc_loop_1

      write(0,*) 'total_npixel = ', total_npixel

      ifirst = 1
      nproc_loop_2: do iproc = 0, nproc - 1

         inquire(file=trim(inpname(iproc)), exist=isfile)
         if ( .not. isfile ) cycle nproc_loop_2
         rewind(iunit(iproc))
         read(unit=iunit(iproc),fmt='(a)') buf
         !
         ! retrieve number of channels
         !
         numbuf = buf((index(buf,'index-of-channels')-6):(index(buf,'index-of-channels')-2))
         read(numbuf,'(i5)') nchan

         if ( .not. allocated(ichan) ) allocate (   ichan(1:nchan) )

         read(unit=iunit(iproc),fmt='(10i5)',iostat=ios) ichan
         read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf      ! pixel-info line
         read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf      ! xb-surf-info line

         if ( ifirst == 1 ) then

            allocate (     datestr2(1:total_npixel) )
            allocate (      scanpos(1:total_npixel) )
            allocate ( landsea_mask(1:total_npixel) )
            allocate (          elv(1:total_npixel) )
            allocate (          lat(1:total_npixel) )
            allocate (          lon(1:total_npixel) )
            allocate (       satzen(1:total_npixel) )
            allocate (       satazi(1:total_npixel) )
            allocate (      ret_clw(1:total_npixel) ) !obs retrieved clw
            allocate (          t2m(1:total_npixel) )
            allocate (         mr2m(1:total_npixel) )
            allocate (          u10(1:total_npixel) )
            allocate (          v10(1:total_npixel) )
            allocate (           ps(1:total_npixel) )
            allocate (           ts(1:total_npixel) )
            allocate (        smois(1:total_npixel) )
            allocate (         tslb(1:total_npixel) )
            allocate (        snowh(1:total_npixel) )
            allocate (        isflg(1:total_npixel) )
            allocate (      soiltyp(1:total_npixel) )
            allocate (       vegtyp(1:total_npixel) )
            allocate (       vegfra(1:total_npixel) )
            allocate (         elev(1:total_npixel) )
            allocate (         clwp(1:total_npixel) ) !model/guess clwp
            allocate ( tb_obs(1:nchan,1:total_npixel) )
            allocate ( tb_bak(1:nchan,1:total_npixel) )
            allocate ( tb_inv(1:nchan,1:total_npixel) )
            allocate ( tb_oma(1:nchan,1:total_npixel) )
            allocate ( tb_err(1:nchan,1:total_npixel) )
            allocate (  tb_qc(1:nchan,1:total_npixel) )
            allocate (    ems(1:nchan,1:total_npixel) )
            if ( jac_found ) then
               allocate ( ems_jac(1:nchan,1:total_npixel) )
            end if
            if ( prf_found .and. (rtm_option == 'CRTM') ) then
               allocate ( prf_pfull(1:maxlvl,1:total_npixel) )
               allocate ( prf_phalf(1:maxlvl,1:total_npixel) )
               allocate ( prf_t(1:maxlvl,1:total_npixel) )
               allocate ( prf_q(1:maxlvl,1:total_npixel) )
               allocate ( prf_water(1:maxlvl,1:total_npixel) )
               allocate ( prf_ice(1:maxlvl,1:total_npixel) )
               allocate ( prf_rain(1:maxlvl,1:total_npixel) )
               allocate ( prf_snow(1:maxlvl,1:total_npixel) )
               allocate ( prf_grau(1:maxlvl,1:total_npixel) )
               allocate ( prf_hail(1:maxlvl,1:total_npixel) )
               allocate ( prf_water_reff(1:maxlvl,1:total_npixel) )
               allocate ( prf_ice_reff(1:maxlvl,1:total_npixel) )
               allocate ( prf_rain_reff(1:maxlvl,1:total_npixel) )
               allocate ( prf_snow_reff(1:maxlvl,1:total_npixel) )
               allocate ( prf_grau_reff(1:maxlvl,1:total_npixel) )
               allocate ( prf_hail_reff(1:maxlvl,1:total_npixel) )
               if ( jac_found ) then
                  allocate ( prf_t_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_q_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_der_trans(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_trans_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_trans(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_lod_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_lod(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_water_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_ice_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_rain_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_snow_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_grau_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_hail_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_water_reff_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_ice_reff_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_rain_reff_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_snow_reff_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_grau_reff_jac(1:maxlvl,1:nchan,1:total_npixel) )
                  allocate ( prf_hail_reff_jac(1:maxlvl,1:nchan,1:total_npixel) )
               end if
            end if
            if ( prf_found .and. (rtm_option == 'RTTOV') ) then
               allocate ( rtm_prf_p(1:maxlvl,1:total_npixel) )
               allocate ( rtm_prf_t(1:maxlvl,1:total_npixel) )
               allocate ( rtm_prf_q(1:maxlvl,1:total_npixel) )  ! in ppmv
               allocate ( mdl_prf_p(1:maxlvl,1:total_npixel) )
               allocate ( mdl_prf_t(1:maxlvl,1:total_npixel) )
               allocate ( mdl_prf_q(1:maxlvl,1:total_npixel) )  ! in g/kg
               allocate ( mdl_prf_qcw(1:maxlvl,1:total_npixel) )
               allocate ( mdl_prf_qrn(1:maxlvl,1:total_npixel) )
               ! initialize
               rtm_prf_p = missing_r
               rtm_prf_t = missing_r
               rtm_prf_q = missing_r
               mdl_prf_p = missing_r
               mdl_prf_t = missing_r
               mdl_prf_q = missing_r
               mdl_prf_qcw = missing_r
               mdl_prf_qrn = missing_r
            end if
            ! initialize
            tb_obs = missing_r
            tb_bak = missing_r
            tb_inv = missing_r
            tb_oma = missing_r
            tb_err = missing_r
            ncname = 'diags_'//trim(instid(iinst))//"_"//datestr1(itime)//'.nc'
            ios = NF_CREATE(trim(ncname), NF_CLOBBER, ncid)  ! NF_CLOBBER specifies the default behavior of
                                                             ! overwritting any existing dataset with the 
                                                             ! same file name
            if ( ios /= 0 ) then
               write(0,*) 'Error creating netcdf file: ', ncname
               stop
            end if
             
            ifirst = 0

         end if   ! end of ifirst if
!
!        decide the index of pixels to store data in
!
         ips = ipe + 1
         ipe = ipe + npixel(iproc)
         write(0,*) 'Processing pixels ', ips, ' to ', ipe

         npixel_loop: do ipixel = ips, ipe

            read(unit=iunit(iproc),fmt='(7x,i7,2x,a19,i6,i3,f6.0,4f8.2,f8.3)',iostat=ios)  &
               n, datestr2(ipixel), scanpos(ipixel), landsea_mask(ipixel), elv(ipixel), &
               lat(ipixel), lon(ipixel), satzen(ipixel), satazi(ipixel), ret_clw(ipixel)

            read(unit=iunit(iproc),fmt='(14x,9f10.2,3i3,3f10.2)',iostat=ios)  &
               t2m(ipixel), mr2m(ipixel), u10(ipixel), v10(ipixel), ps(ipixel), ts(ipixel), &
               smois(ipixel), tslb(ipixel), snowh(ipixel), isflg(ipixel), soiltyp(ipixel),  &
               vegtyp(ipixel), vegfra(ipixel), elev(ipixel), clwp(ipixel)
            read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf           ! OBS
            read(unit=iunit(iproc),fmt='(10f11.2)',iostat=ios) tb_obs(:,ipixel)
            read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf           ! BAK
            read(unit=iunit(iproc),fmt='(10f11.2)',iostat=ios) tb_bak(:,ipixel)
            read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf           ! IVBC
            read(unit=iunit(iproc),fmt='(10f11.2)',iostat=ios) tb_inv(:,ipixel)
            read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf           ! OMA or EMS
            if ( buf(1:3) == "OMA" ) then
               read(unit=iunit(iproc),fmt='(10f11.2)',iostat=ios) tb_oma(:,ipixel)
               read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf        ! OMA or EMS
            end if
            read(unit=iunit(iproc),fmt='(10f11.2)',iostat=ios) ems(:,ipixel)
            read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf           ! EMS_JACOBIAN or ERR
            if ( buf(1:12) == "EMS_JACOBIAN" ) then
               read(unit=iunit(iproc),fmt='(10f10.3)',iostat=ios) ems_jac(:,ipixel)
               read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf        ! OMA or EMS
            end if
            read(unit=iunit(iproc),fmt='(10f11.2)',iostat=ios) tb_err(:,ipixel)
            read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf           ! QC
            read(unit=iunit(iproc),fmt='(10i11)',iostat=ios  ) tb_qc(:,ipixel)
            read(unit=iunit(iproc),fmt='(a)',iostat=ios) buf
            if ( buf(1:4) == "INFO" ) then
               backspace(iunit(iproc))
               cycle npixel_loop
            else
               if ( index(buf(1:6),'level') > 0 ) then    ! CRTM profiles are available
                  ilev = 0
                  do while ( ios == 0 )
                     ilev = ilev + 1
                     read(unit=iunit(iproc),fmt='(3x,2f10.2,f8.2,13f8.3)',iostat=ios)        &
                        prf_pfull(ilev,ipixel), prf_phalf(ilev,ipixel), prf_t(ilev,ipixel),  &
                        prf_q(ilev,ipixel), prf_water(ilev,ipixel), prf_ice(ilev,ipixel),    &
                        prf_rain(ilev,ipixel), prf_snow(ilev,ipixel), prf_grau(ilev,ipixel), &
                        prf_hail(ilev,ipixel),prf_water_reff(ilev,ipixel),                   &
                        prf_ice_reff(ilev,ipixel), prf_rain_reff(ilev,ipixel),               &
                        prf_snow_reff(ilev,ipixel), prf_grau_reff(ilev,ipixel),              &
                        prf_hail_reff(ilev,ipixel)
                  end do
                  nlev = ilev - 1
                  ! backspace(iunit(iproc))
               else if ( index(buf, 'RTM_level') > 0 ) then  ! RTTOV profiles are available
                  ! first, RTTOV levels
                  ilev = 0
                  do while ( ios == 0 )
                     ilev = ilev + 1
                     read(unit=iunit(iproc),fmt='(3x,f10.2,f8.2,e11.4)',iostat=ios)        &
                        rtm_prf_p(ilev,ipixel), rtm_prf_t(ilev,ipixel),                    &
                        rtm_prf_q(ilev,ipixel)  ! q in ppmv
                  end do
                  nlev_rtm = ilev - 1
                  ! second, Model levels
                  ios  = 0
                  ilev = 0
                  do while ( ios == 0 )
                     ilev = ilev + 1
                     read(unit=iunit(iproc),fmt='(3x,f10.2,f8.2,3e11.4)',iostat=ios)        &
                        mdl_prf_p(ilev,ipixel), mdl_prf_t(ilev,ipixel),                     &
                        mdl_prf_q(ilev,ipixel), mdl_prf_qcw(ilev,ipixel),                   &
                        mdl_prf_qrn(ilev,ipixel)
                  end do
                  nlev_mdl = ilev - 1
               end if
    
               if ( jac_found ) then
                  ios = 0
                  do while ( ios == 0 )
                     ! ilev = ilev + 1
                     read(unit=iunit(iproc),fmt='(i5,i3,10x,19f14.7)',iostat=ios)               &
                        ich, ilev, prf_t_jac(ilev,ich,ipixel), prf_q_jac(ilev,ich,ipixel),     & 
                        prf_der_trans(ilev,ich,ipixel),                                        & 
                        prf_trans_jac(ilev,ich,ipixel), prf_trans(ilev,ich,ipixel),       & 
                        prf_lod_jac(ilev,ich,ipixel), prf_lod(ilev,ich,ipixel),           & 
                        prf_water_jac(ilev,ich,ipixel), prf_ice_jac(ilev,ich,ipixel),          &
                        prf_rain_jac(ilev,ich,ipixel), prf_snow_jac(ilev,ich,ipixel),          &
                        prf_grau_jac(ilev,ich,ipixel), prf_hail_jac(ilev,ich,ipixel),          &
                        prf_water_reff_jac(ilev,ich,ipixel), prf_ice_reff_jac(ilev,ich,ipixel), &
                        prf_rain_reff_jac(ilev,ich,ipixel), prf_snow_reff_jac(ilev,ich,ipixel), &
                        prf_grau_reff_jac(ilev,ich,ipixel), prf_hail_reff_jac(ilev,ich,ipixel)
                     nlev = max(nlev,ilev)
                  end do
                  backspace(iunit(iproc))
               else
                  backspace(iunit(iproc))
               end if
            end if 

         end do npixel_loop

         close(iunit(iproc))

      end do nproc_loop_2

      write(0,*) 'Writing out data in netCDF format...'
      !
      ! define dimensions
      !
      ios = NF_DEF_DIM(ncid, 'nchan', nchan, dimid)
      ios = NF_DEF_DIM(ncid, 'npixel', total_npixel, dimid)
      ios = NF_DEF_DIM(ncid, 'DateStrLen', datelen2, dimid)
      if ( prf_found .or. jac_found ) then
         if ( rtm_option == 'RTTOV' ) then
            nlev = max(nlev_rtm,nlev_mdl)
         end if
         ios = NF_DEF_DIM(ncid, 'nlev', nlev, dimid)
      end if
      !
      ! output global attributes
      !
      if ( trim(rtm_option) == 'unknown' ) then
         if ( maxval(mr2m) < -8000.0 ) then
            rtm_option = 'CRTM'
         else
            rtm_option = 'RTTOV'
         end if
      end if
      ios = NF_PUT_ATT_TEXT (ncid, NF_GLOBAL, 'rtm_option', 20, rtm_option)
      !
      ! define variables
      !
      ! define 2-D array for date string
      !
      ios = NF_INQ_DIMID(ncid, 'DateStrLen', dimid)
      ishape(1) = dimid
      ios = NF_INQ_DIMID(ncid, 'npixel', dimid)
      ishape(2) = dimid
      ios = NF_DEF_VAR(ncid, 'date',   NF_CHAR,  2, ishape(1:2), varid)
      !
      ! define 2-D array with dimensions nchan * total_npixel
      !
      ios = NF_INQ_DIMID(ncid, 'nchan', dimid)
      ishape(1) = dimid
      ios = NF_INQ_DIMID(ncid, 'npixel', dimid)
      ishape(2) = dimid
      ios = NF_DEF_VAR(ncid, 'tb_obs', NF_FLOAT, 2, ishape(1:2), varid)
      ios = NF_PUT_ATT_REAL(ncid, varid, 'missing_value', NF_FLOAT, 1, missing_r)
      ios = NF_DEF_VAR(ncid, 'tb_bak', NF_FLOAT, 2, ishape(1:2), varid)
      ios = NF_PUT_ATT_REAL(ncid, varid, 'missing_value', NF_FLOAT, 1, missing_r)
      ios = NF_DEF_VAR(ncid, 'tb_inv', NF_FLOAT, 2, ishape(1:2), varid)
      ios = NF_PUT_ATT_REAL(ncid, varid, 'missing_value', NF_FLOAT, 1, missing_r)
      ios = NF_DEF_VAR(ncid, 'tb_oma', NF_FLOAT, 2, ishape(1:2), varid)
      ios = NF_PUT_ATT_REAL(ncid, varid, 'missing_value', NF_FLOAT, 1, missing_r)
      ios = NF_DEF_VAR(ncid, 'ems',    NF_FLOAT, 2, ishape(1:2), varid)
      if ( jac_found ) then
         ios = NF_DEF_VAR(ncid, 'ems_jac',NF_FLOAT, 2, ishape(1:2), varid)
      end if
      ios = NF_DEF_VAR(ncid, 'tb_err', NF_FLOAT, 2, ishape(1:2), varid)
      ios = NF_DEF_VAR(ncid, 'tb_qc',  NF_INT,   2, ishape(1:2), varid)
      !
      ! define 2-D array with dimensions nlev * total_npixel
      !
      if ( prf_found ) then
         ios = NF_INQ_DIMID(ncid, 'nlev', dimid)
         ishape(1) = dimid
         ios = NF_INQ_DIMID(ncid, 'npixel', dimid)
         ishape(2) = dimid
         if ( rtm_option == 'CRTM' ) then
            ios = NF_DEF_VAR(ncid, 'prf_pfull', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_phalf', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_t',     NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_q',     NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_water', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_ice',   NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_rain',  NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_snow',  NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_grau',  NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_hail',  NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_water_reff', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_ice_reff',   NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_rain_reff',  NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_snow_reff',  NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_grau_reff',  NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'prf_hail_reff',  NF_FLOAT, 2, ishape(1:2), varid)
         else if ( rtm_option == 'RTTOV' ) then
            ios = NF_DEF_VAR(ncid, 'rtm_prf_p', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'rtm_prf_t', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'rtm_prf_q', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'mdl_prf_p', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'mdl_prf_t', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'mdl_prf_q', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'mdl_prf_qcw', NF_FLOAT, 2, ishape(1:2), varid)
            ios = NF_DEF_VAR(ncid, 'mdl_prf_qrn', NF_FLOAT, 2, ishape(1:2), varid)
         end if
      end if
      !
      ! define 3-D array with dimensions nlev * nchan * total_npixel
      !
      if ( jac_found ) then
         ios = NF_INQ_DIMID(ncid, 'nlev', dimid)
         ishape(1) = dimid
         ios = NF_INQ_DIMID(ncid, 'nchan', dimid)
         ishape(2) = dimid
         ios = NF_INQ_DIMID(ncid, 'npixel', dimid)
         ishape(3) = dimid
         ios = NF_DEF_VAR(ncid, 'prf_t_jac',     NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_q_jac',     NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_der_trans',     NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_trans_jac', NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_trans', NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_lod_jac', NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_lod', NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_water_jac', NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_ice_jac',   NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_rain_jac',  NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_snow_jac',  NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_grau_jac',  NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_hail_jac',  NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_water_reff_jac', NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_ice_reff_jac',   NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_rain_reff_jac',  NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_snow_reff_jac',  NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_grau_reff_jac',  NF_FLOAT, 3, ishape, varid)
         ios = NF_DEF_VAR(ncid, 'prf_hail_reff_jac',  NF_FLOAT, 3, ishape, varid)
      end if
      !
      ! define 1-D array with dimension nchan
      !
      ios = NF_INQ_DIMID(ncid, 'nchan', dimid)
      ishape(1) = dimid
      ios = NF_DEF_VAR(ncid, 'ichan',  NF_INT,   1, ishape(1), varid)   ! channel index
      !
      ! define 1-D array with dimension total_npixel
      !
      ios = NF_INQ_DIMID(ncid, 'npixel', dimid)
      ishape(1) = dimid
      ios = NF_DEF_VAR(ncid, 'scanpos',      NF_INT,   1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'landsea_mask', NF_INT,   1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'elv',          NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'lat',          NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'lon',          NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'satzen',       NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'satazi',       NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 't2m',          NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'mr2m',         NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'u10',          NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'v10',          NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'ps',           NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'ts',           NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'tslb',         NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'smois',        NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'snowh',        NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'isflg',        NF_INT,   1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'soiltyp',      NF_INT,   1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'vegtyp',       NF_INT,   1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'vegfra',       NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'elev',         NF_FLOAT, 1, ishape(1), varid)
      ios = NF_DEF_VAR(ncid, 'clwp',         NF_FLOAT, 1, ishape(1), varid)
      if ( amsr2 ) then
         ios = NF_DEF_VAR(ncid, 'ret_clw',   NF_FLOAT, 1, ishape(1), varid)
      end if

      ios = NF_ENDDEF(ncid)
      !
      ! output date string
      istart(1) = 1
      istart(2) = 1
      icount(1) = datelen2
      icount(2) = total_npixel
      ios = NF_INQ_VARID (ncid, 'date', varid)
      ios = NF_PUT_VARA_TEXT(ncid, varid, istart, icount, datestr2)
      !
      ! output 2-D array with dimensions nchan * total_npixel
      !
      istart(1) = 1
      istart(2) = 1
      icount(1) = nchan
      icount(2) = total_npixel
      ios = NF_INQ_VARID (ncid, 'tb_obs', varid)
      ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), tb_obs)
      ios = NF_INQ_VARID (ncid, 'tb_bak', varid)
      ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), tb_bak)
      ios = NF_INQ_VARID (ncid, 'tb_inv', varid)
      ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), tb_inv)
      ios = NF_INQ_VARID (ncid, 'tb_oma', varid)
      ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), tb_oma)
      ios = NF_INQ_VARID (ncid, 'ems', varid)
      ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), ems)
      if ( jac_found ) then
         ios = NF_INQ_VARID (ncid, 'ems_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), ems_jac)
      end if
      ios = NF_INQ_VARID (ncid, 'tb_err', varid)
      ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), tb_err)
      ios = NF_INQ_VARID (ncid, 'tb_qc', varid)
      ios = NF_PUT_VARA_INT(ncid,  varid, istart(1:2), icount(1:2), tb_qc)
      !
      ! output 2-D array with dimensions nlev * total_npixel
      !
      if ( prf_found ) then
         istart(1) = 1
         istart(2) = 1
         icount(1) = nlev
         icount(2) = total_npixel
         if ( rtm_option == 'CRTM' ) then
            ios = NF_INQ_VARID (ncid, 'prf_pfull', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_pfull(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_phalf', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_phalf(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_t', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_t(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_q', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_q(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_water', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_water(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_ice', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_ice(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_rain', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_rain(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_snow', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_snow(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_grau', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_grau(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_hail', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_hail(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_water_reff', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_water_reff(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_ice_reff', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_ice_reff(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_rain_reff', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_rain_reff(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_snow_reff', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_snow_reff(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_grau_reff', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_grau_reff(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'prf_hail_reff', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), prf_hail_reff(1:nlev,:))
         else if ( rtm_option == 'RTTOV' ) then
            ios = NF_INQ_VARID (ncid, 'rtm_prf_p', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), rtm_prf_p(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'rtm_prf_t', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), rtm_prf_t(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'rtm_prf_q', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), rtm_prf_q(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'mdl_prf_p', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), mdl_prf_p(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'mdl_prf_t', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), mdl_prf_t(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'mdl_prf_q', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), mdl_prf_q(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'mdl_prf_qcw', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), mdl_prf_qcw(1:nlev,:))
            ios = NF_INQ_VARID (ncid, 'mdl_prf_qrn', varid)
            ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:2), icount(1:2), mdl_prf_qrn(1:nlev,:))
         end if
      end if
      !
      ! output 3-D array with dimensions nlev * nchan * total_npixel
      !
      if ( jac_found ) then
         istart(1) = 1
         istart(2) = 1
         istart(3) = 1
         icount(1) = nlev
         icount(2) = nchan
         icount(3) = total_npixel
         ios = NF_INQ_VARID (ncid, 'prf_t_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_t_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_q_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_q_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_der_trans', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_der_trans(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_trans_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_trans_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_trans', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_trans(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_lod_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_lod_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_lod', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_lod(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_water_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_water_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_ice_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_ice_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_rain_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_rain_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_snow_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_snow_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_grau_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_grau_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_hail_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_hail_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_water_reff_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_water_reff_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_ice_reff_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_ice_reff_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_rain_reff_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_rain_reff_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_snow_reff_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_snow_reff_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_grau_reff_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_grau_reff_jac(1:nlev,:,:))
         ios = NF_INQ_VARID (ncid, 'prf_hail_reff_jac', varid)
         ios = NF_PUT_VARA_REAL(ncid, varid, istart(1:3), icount(1:3), prf_hail_reff_jac(1:nlev,:,:))
      end if
      !
      ! output 1-D array with dimension nchan
      !
      istart(1) = 1
      icount(1) = nchan
      ios = NF_INQ_VARID (ncid, 'ichan', varid)
      ios = NF_PUT_VARA_INT(ncid,  varid, istart(1), icount(1), ichan)
      !
      ! output 1-D array with dimension total_npixel
      !
      istart(2) = 1
      icount(2) = total_npixel
      ios = NF_INQ_VARID (ncid, 'scanpos', varid)
      ios = NF_PUT_VARA_INT(ncid,  varid, istart(2), icount(2), scanpos)
      ios = NF_INQ_VARID (ncid, 'landsea_mask', varid)
      ios = NF_PUT_VARA_INT(ncid,  varid, istart(2), icount(2), landsea_mask)
      ios = NF_INQ_VARID (ncid, 'elv', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), elv)
      ios = NF_INQ_VARID (ncid, 'lat', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), lat)
      ios = NF_INQ_VARID (ncid, 'lon', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), lon)
      ios = NF_INQ_VARID (ncid, 'satzen', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), satzen)
      ios = NF_INQ_VARID (ncid, 'satazi', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), satazi)
      ios = NF_INQ_VARID (ncid, 't2m', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), t2m)
      ios = NF_INQ_VARID (ncid, 'mr2m', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), mr2m)
      ios = NF_INQ_VARID (ncid, 'u10', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), u10)
      ios = NF_INQ_VARID (ncid, 'v10', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), v10)
      ios = NF_INQ_VARID (ncid, 'ps', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), ps)
      ios = NF_INQ_VARID (ncid, 'ts', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), ts)
      ios = NF_INQ_VARID (ncid, 'smois', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), smois)
      ios = NF_INQ_VARID (ncid, 'tslb', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), tslb)
      ios = NF_INQ_VARID (ncid, 'snowh', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), snowh)
      ios = NF_INQ_VARID (ncid, 'isflg', varid)
      ios = NF_PUT_VARA_INT(ncid,  varid, istart(2), icount(2), isflg)
      ios = NF_INQ_VARID (ncid, 'soiltyp', varid)
      ios = NF_PUT_VARA_INT(ncid,  varid, istart(2), icount(2), soiltyp)
      ios = NF_INQ_VARID (ncid, 'vegtyp', varid)
      ios = NF_PUT_VARA_INT(ncid,  varid, istart(2), icount(2), vegtyp)
      ios = NF_INQ_VARID (ncid, 'vegfra', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), vegfra)
      ios = NF_INQ_VARID (ncid, 'elev', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), elev)
      ios = NF_INQ_VARID (ncid, 'clwp', varid)
      ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), clwp)
      if ( amsr2 ) then
         ios = NF_INQ_VARID (ncid, 'ret_clw', varid)
         ios = NF_PUT_VARA_REAL(ncid,  varid, istart(2), icount(2), ret_clw)
      end if
      !
      ios = NF_CLOSE(ncid)

      deallocate ( npixel )
      deallocate ( iunit )
      deallocate ( inpname )

      deallocate ( ichan )
      deallocate ( datestr2 )
      deallocate ( scanpos )
      deallocate ( landsea_mask )
      deallocate ( elv )
      deallocate ( lat )
      deallocate ( lon )
      deallocate ( satzen )
      deallocate ( satazi )
      deallocate ( t2m )
      deallocate ( mr2m )
      deallocate ( u10 )
      deallocate ( v10 )
      deallocate ( ps )
      deallocate ( ts )
      deallocate ( smois )
      deallocate ( tslb )
      deallocate ( snowh )
      deallocate ( isflg )
      deallocate ( soiltyp )
      deallocate ( vegtyp )
      deallocate ( vegfra )
      deallocate ( elev )
      deallocate ( clwp )
      deallocate ( ret_clw )
      deallocate ( tb_obs )
      deallocate ( tb_bak )
      deallocate ( tb_inv )
      deallocate ( tb_oma )
      deallocate ( ems )
      if ( jac_found ) deallocate ( ems_jac )
      deallocate ( tb_err )
      deallocate ( tb_qc )
      if ( prf_found .and. (rtm_option == 'CRTM') ) then
         deallocate ( prf_pfull )
         deallocate ( prf_phalf )
         deallocate ( prf_t )
         deallocate ( prf_q )
         deallocate ( prf_water )
         deallocate ( prf_ice )
         deallocate ( prf_rain )
         deallocate ( prf_snow )
         deallocate ( prf_grau )
         deallocate ( prf_hail )
         deallocate ( prf_water_reff )
         deallocate ( prf_ice_reff )
         deallocate ( prf_rain_reff )
         deallocate ( prf_snow_reff )
         deallocate ( prf_grau_reff )
         deallocate ( prf_hail_reff )
         if ( jac_found ) then
            deallocate ( prf_t_jac )
            deallocate ( prf_q_jac )
            deallocate ( prf_der_trans )
            deallocate ( prf_trans_jac )
            deallocate ( prf_trans )
            deallocate ( prf_lod_jac )
            deallocate ( prf_lod )
            deallocate ( prf_water_jac )
            deallocate ( prf_ice_jac )
            deallocate ( prf_rain_jac )
            deallocate ( prf_snow_jac )
            deallocate ( prf_grau_jac )
            deallocate ( prf_hail_jac )
            deallocate ( prf_water_reff_jac )
            deallocate ( prf_ice_reff_jac )
            deallocate ( prf_rain_reff_jac )
            deallocate ( prf_snow_reff_jac )
            deallocate ( prf_grau_reff_jac )
            deallocate ( prf_hail_reff_jac )
         end if
      end if
      if ( prf_found .and. (rtm_option == 'RTTOV') ) then
         deallocate ( rtm_prf_p )
         deallocate ( rtm_prf_t )
         deallocate ( rtm_prf_q )
         deallocate ( mdl_prf_p )
         deallocate ( mdl_prf_t )
         deallocate ( mdl_prf_q )
         deallocate ( mdl_prf_qcw )
         deallocate ( mdl_prf_qrn )
      end if

   end do ninst_loop

end do ntime_loop

deallocate ( datestr1 )

end program da_rad_diags

subroutine advance_cymdh(currentdate,dh,newdate)
   
   implicit none       
      
   character(len=10), intent(in)  :: currentdate
   integer,           intent(in)  :: dh
   character(len=10), intent(out) :: newdate
   
   integer :: ccyy, mm, dd, hh

   read(currentdate(1:10), fmt='(i4, 3i2)')  ccyy, mm, dd, hh
   hh = hh + dh
   do while (hh < 0)
      hh = hh + 24
      call change_date ( ccyy, mm, dd, -1 )
   end do  
   do while (hh > 23)                     
      hh = hh - 24                        
      call change_date ( ccyy, mm, dd, 1 )
   end do
   write(newdate,'(i4.4,3(i2.2))') ccyy, mm, dd, hh
   
end subroutine advance_cymdh

subroutine change_date( ccyy, mm, dd, delta )

      implicit none

      integer, intent(inout) :: ccyy, mm, dd
      integer, intent(in)    :: delta
      integer, dimension(12) :: mmday

      mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
      mmday(2) = 28
      if (mod(ccyy,4) == 0) then
         mmday(2) = 29
         if ( mod(ccyy,100) == 0) then
            mmday(2) = 28
         endif
         if(mod(ccyy,400) == 0) then
            mmday(2) = 29
         end if
      endif
      dd = dd + delta
      if(dd == 0) then
         mm = mm - 1
         if(mm == 0) then
            mm = 12
            ccyy = ccyy - 1
         endif
         dd = mmday(mm)
      elseif ( dd .gt. mmday(mm) ) then
         dd = 1
         mm = mm + 1
         if(mm > 12 ) then
            mm = 1
            ccyy = ccyy + 1
         end if
      end if

end subroutine change_date
