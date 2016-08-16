module module_tracker
  implicit none
  private
#if ( HWRF == 1 )
  public :: ncep_tracker_center, ncep_tracker_init, update_tracker_post_move

  real, parameter :: invE=0.36787944117 ! 1/e

  ! Copied from tracker:
  real,parameter :: searchrad_6=250.0 ! km - ignore data more than this far from domain center
  real,parameter :: searchrad_7=200.0 ! km - ignore data more than this far from domain center
  integer, parameter :: maxtp=11 ! number of tracker parameters
  real, parameter :: uverrmax = 225.0  ! For use in get_uv_guess
  real, parameter :: ecircum = 40030.2  ! Earth's circumference
  ! (km) using erad=6371.e3
  real, parameter :: rads_vmag=120.0 ! max search radius for wind minimum
  real, parameter :: err_reg_init=300.0 ! max err at initial time (km)
  real, parameter :: err_reg_max=225.0 ! max err at other times (km)

  real, parameter :: errpmax=485.0 ! max stddev of track parameters
  real, parameter :: errpgro=1.25 ! stddev multiplier

  real, parameter :: max_wind_search_radius=searchrad_7 ! max radius for vmax search
  real, parameter :: min_mlsp_search_radius=searchrad_7 ! max radius for pmin search

  ! Also used:
  real, parameter :: km2nmi = 0.539957, kn2mps=0.514444, mps2kn=1./kn2mps, pi180=0.01745329251
contains

  !----------------------------------------------------------------------------------
  ! These two simple routines return an N, S, E or W for the
  ! hemisphere of a latitude or longitude.  They are copied from
  ! module_HIFREQ to avoid a relatively pointless compiler dependency.

  character(1) function get_lat_ns(lat)
    ! This could be written simply as merge('N','S',lat>=0) if WRF allowed F95
    implicit none ; real lat
    if(lat>=0) then
       get_lat_ns='N'
    else
       get_lat_ns='S'
    endif
  end function get_lat_ns
  character(1) function get_lon_ew(lon)
    ! This could be written simply as merge('E','W',lon>=0) if WRF allowed F95
    implicit none ; real lon
    if(lon>=0) then
       get_lon_ew='E'
    else
       get_lon_ew='W'
    endif
  end function get_lon_ew

  subroutine ncep_tracker_init(grid)
    ! Initialize tracker variables in the grid structure.
    use module_domain, only: domain
    implicit none
    type(domain), intent(inout) :: grid
    call wrf_message('ncep_tracker_init')
    grid%track_stderr_m1=-99.9
    grid%track_stderr_m2=-99.9
    grid%track_stderr_m3=-99.9
    grid%track_n_old=0
    grid%track_old_lon=0
    grid%track_old_lat=0
    grid%track_old_ntsd=0

    grid%tracker_angle=0
    grid%tracker_fixlon=-999.0
    grid%tracker_fixlat=-999.0
    grid%tracker_ifix=-99
    grid%tracker_jfix=-99
    grid%tracker_havefix=.false.
    grid%tracker_gave_up=.false.
    grid%tracker_pmin=-99999.
    grid%tracker_vmax=-99.
    grid%tracker_rmw=-99.

    grid%track_have_guess=.false.
    grid%track_guess_lat=-999.0
    grid%track_guess_lon=-999.0
  end subroutine ncep_tracker_init

  subroutine ncep_tracker_center(grid)
    ! Top-level entry to the inline ncep tracker.  Finds the center of
    ! the storm in the specified grid and updates the grid variables.
    ! Will do nothing and return immediately if
    ! grid%tracker_gave_up=.true.

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    character*255 :: message

    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE

    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

    call ntc_impl(grid,                &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )
  end subroutine ncep_tracker_center

  subroutine ntc_impl(grid, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    ! This is the main entry point to the tracker.  It is most similar
    ! to the function "tracker" in the GFDL/NCEP vortex tracker.

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_sum_real
#endif
    implicit none
    logical, external :: wrf_dm_on_monitor
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    real :: dxdymean, sum
    integer :: i,j, iweights,ip


    integer :: iguess, jguess ! first guess location
    real :: latguess, longuess ! same, but in lat & lon

    integer :: iuvguess, juvguess ! "second guess" location using everything except wind maxima
    real :: srsq
    integer :: ifinal,jfinal
    real :: latfinal,lonfinal
    integer :: ierr
    integer :: icen(maxtp), jcen(maxtp) ! center locations for each parameter
    real :: loncen(maxtp), latcen(maxtp) ! lat, lon locations in degrees
    logical :: calcparm(maxtp) ! do we have a valid center location for this parameter?
    real :: max_wind,min_pres ! for ATCF output
    real :: rcen(maxtp) ! center value (max wind, min mslp, etc.)
    character*255 :: message
    logical :: north_hemi ! true = northern hemisphere
    logical :: have_guess ! first guess is available
    real :: guessdist,guessdeg ! first guess distance to nearest point on grid
    real :: latnear, lonnear ! nearest point in grid to first guess

    ! icen,jcen: Same meaning as clon, clat in tracker, but uses i and
    ! j indexes of the center instead of lat/lon.  Tracker comment:
    !            Holds the coordinates for the center positions for
    !            all storms at all times for all parameters.
    !            (max_#_storms, max_fcst_times, max_#_parms).
    !            For the third position (max_#_parms), here they are:
    !             1: Relative vorticity at 850 mb
    !             2: Relative vorticity at 700 mb
    !             3: Vector wind magnitude at 850 mb
    !             4: NOT CURRENTLY USED
    !             5: Vector wind magnitude at 700 mb
    !             6: NOT CURRENTLY USED
    !             7: Geopotential height at 850 mb
    !             8: Geopotential height at 700 mb
    !             9: Mean Sea Level Pressure
    !            10: Vector wind magnitude at 10 m
    !            11: Relative vorticity at 10 m

    call wrf_message('ncep_tracker_center')

    ! Initialize center information to invalid values for all centers:
    icen=-99
    jcen=-99
    latcen=9e9
    loncen=9e9
    rcen=9e9
    calcparm=.false.
    if(grid%vortex_tracker==6) then
       srsq=searchrad_6*searchrad_6*1e6
    else
       srsq=searchrad_7*searchrad_7*1e6
    endif

    ! Get the first guess from the prior nest motion timestep:
    have_guess=grid%track_have_guess
    if(have_guess) then
       ! We have a first guess center.  We have to translate it to gridpoint space.
       longuess=grid%track_guess_lon
       latguess=grid%track_guess_lat
       call get_nearest_lonlat(grid,iguess,jguess,ierr,longuess,latguess, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe,     lonnear, latnear)
       if(ierr==0) then
          call calcdist(longuess,latguess, lonnear,latnear, guessdist,guessdeg)
          if(guessdist*1e3>3*grid%dy) then
108          format('WARNING: guess lon=',F0.3,',lat=',F0.3, &
                  ' too far (',F0.3,'km) from nearest point lon=',F0.3,',lat=',F0.3, &
                  '.  Will use domain center as first guess.')
             write(message,108) grid%track_guess_lon,grid%track_guess_lat, &
                  guessdist,lonnear,latnear
             call wrf_message(message)
             have_guess=.false. ! indicate that the first guess is unusable
          else
             latguess=latnear
             longuess=lonnear
          endif
       else
          have_guess=.false. ! indicate that the first guess is unusable.
109       format('WARNING: guess lon=',F0.3,',lat=',F0.3, &
                  ' does not exist in this domain.  Will use domain center as first guess.')
          write(message,109) grid%track_guess_lon,grid%track_guess_lat
          call wrf_message(message)
       endif
   endif

   ! If we could not get the first guess from the prior nest motion
   ! timestep, then use the default first guess: the domain center.
    if(grid%vortex_tracker==6 .or. .not.have_guess) then
       ! vt=6: hard coded first-guess center is domain center:
       ! vt=7: first guess comes from prior timestep
       !     Initial first guess is domain center.
       !     Backup first guess is domain center if first guess is unusable.
       iguess=ide/2
       jguess=jde/2
       if(grid%vortex_tracker==7) then
          call wrf_message('Using domain center as first guess since no valid first guess is available.')
       endif
       call get_lonlat(grid,iguess,jguess,longuess,latguess,ierr, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe)
       if(ierr/=0) then
          call wrf_error_fatal("ERROR: center of domain is not inside the domain")
       endif
       have_guess=.true.
    endif

    if(.not.have_guess) then
       call wrf_error_fatal("INTERNAL ERROR: No first guess is available (should never happen).")
    endif

    north_hemi = latguess>0.0

    ! Get the mean V-to-H point-to-point distance:
    sum=0
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          sum=sum+grid%dx_nmm(i,j)
       enddo
    enddo
#ifdef DM_PARALLEL
    sum=wrf_dm_sum_real(sum)
#endif
    dxdymean=0.5*(grid%dy_nmm + sum/( (ide-ids) * (jde-jds) ))/1000.0
33  format ('dxdymean=',F0.3,' dx=',F0.3,' dy=',F0.3,' sum=',F0.3,' count=',I0)
    !write(0,33) dxdymean,grid%dx_nmm((ips+ipe)/2,(jps+jpe)/2),grid%dy_nmm, &
    !     sum,(ide-ids) * (jde-jds)

    ! Find the centers of all fields except the wind minima:
    call find_center(grid,grid%p850rv,grid%sp850rv,srsq, &
         icen(1),jcen(1),rcen(1),calcparm(1),loncen(1),latcen(1),dxdymean,'zeta', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE, north_hemi=north_hemi)
    call find_center(grid,grid%p700rv,grid%sp700rv,srsq, &
         icen(2),jcen(2),rcen(2),calcparm(2),loncen(2),latcen(2),dxdymean,'zeta', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE, north_hemi=north_hemi)
    call find_center(grid,grid%p850z,grid%sp850z,srsq, &
         icen(7),jcen(7),rcen(7),calcparm(7),loncen(7),latcen(7),dxdymean,'hgt', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    call find_center(grid,grid%p700z,grid%sp700z,srsq, &
         icen(8),jcen(8),rcen(8),calcparm(8),loncen(8),latcen(8),dxdymean,'hgt', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    call find_center(grid,grid%membrane_mslp,grid%smslp,srsq, &
         icen(9),jcen(9),rcen(9),calcparm(9),loncen(9),latcen(9),dxdymean,'slp', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    call find_center(grid,grid%m10rv,grid%sm10rv,srsq, &
         icen(11),jcen(11),rcen(11),calcparm(11),loncen(11),latcen(11),dxdymean,'zeta', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE, north_hemi=north_hemi)

    ! Get a guess center location for the wind minimum searches:
    call get_uv_guess(grid,icen,jcen,loncen,latcen,calcparm, &
         iguess,jguess,longuess,latguess,iuvguess,juvguess, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    ! Find wind minima.  Requires a first guess center:
    windmin: if(grid%vortex_tracker==6) then
       call find_center(grid,grid%p850wind,grid%sp850wind,srsq, &
            icen(3),jcen(3),rcen(3),calcparm(3),loncen(3),latcen(3),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
       call find_center(grid,grid%p700wind,grid%sp700wind,srsq, &
            icen(5),jcen(5),rcen(5),calcparm(5),loncen(5),latcen(5),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
       call find_center(grid,grid%m10wind,grid%sm10wind,srsq, &
            icen(10),jcen(10),rcen(10),calcparm(10),loncen(10),latcen(10),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
    else
       call get_uv_center(grid,grid%p850wind, &
            icen(3),jcen(3),rcen(3),calcparm(3),loncen(3),latcen(3),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
       call get_uv_center(grid,grid%p700wind, &
            icen(5),jcen(5),rcen(5),calcparm(5),loncen(5),latcen(5),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
       call get_uv_center(grid,grid%m10wind, &
            icen(10),jcen(10),rcen(10),calcparm(10),loncen(10),latcen(10),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
    endif windmin

    ! Get a final guess center location:
    call fixcenter(grid,icen,jcen,calcparm,loncen,latcen, &
         iguess,jguess,longuess,latguess, &
         ifinal,jfinal,lonfinal,latfinal, &
         north_hemi, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)

    grid%tracker_fixes=0
    do ip=1,maxtp
       if(calcparm(ip)) then
300       format('Parameter ',I0,': i=',I0,' j=',I0,' lon=',F0.2,' lat=',F0.2)
          !write(0,300) ip,icen(ip),jcen(ip),loncen(ip),latcen(ip)
          if(icen(ip)>=ips .and. icen(ip)<=ipe &
               .and. jcen(ip)>=jps .and. jcen(ip)<=jpe) then
             grid%tracker_fixes(icen(ip),jcen(ip))=ip
          endif
       else
301       format('Parameter ',I0,' invalid')
          !write(0,301) ip
       endif
    enddo

    if(iguess>=ips .and. iguess<=ipe .and. jguess>=jps .and. jguess<=jpe) then
       grid%tracker_fixes(iguess,jguess)=-1
201    format('First guess: i=',I0,' j=',I0,' lon=',F0.2,' lat=',F0.2)
       !write(0,201) iguess,jguess,longuess,latguess
    endif

    if(iuvguess>=ips .and. iuvguess<=ipe .and. juvguess>=jps .and. juvguess<=jpe) then
       grid%tracker_fixes(iuvguess,juvguess)=-2
202    format('UV guess: i=',I0,' j=',I0)
       !write(0,202) iguess,jguess
    endif

1000 format('Back with final lat/lon at i=',I0,' j=',I0,' lon=',F0.3,' lat=',F0.3)
    !write(0,1000) ifinal,jfinal,lonfinal,latfinal

    if(ifinal>=ips .and. ifinal<=ipe .and. jfinal>=jps .and. jfinal<=jpe) then
       grid%tracker_fixes(ifinal,jfinal)=-3
203    format('Final fix: i=',I0,' j=',I0,' lon=',F0.2,' lat=',F0.2)
       !write(0,201) ifinal,jfinal,lonfinal,latfinal
    endif

    call get_tracker_distsq(grid, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    call get_wind_pres_intensity(grid, &
         grid%tracker_pmin,grid%tracker_vmax,grid%tracker_rmw, &
         max_wind_search_radius, min_mlsp_search_radius, &
         lonfinal,latfinal, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

#ifdef DM_PARALLEL
    if(wrf_dm_on_monitor()) then
#endif
       call output_partial_atcfunix(grid, &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE)
#ifdef DM_PARALLEL
    endif
#endif

!     if(grid%vortex_tracker==7) then
!        call get_first_ges(grid,iguess,jguess,longuess,latguess, &
!             IDS,IDE,JDS,JDE,KDS,KDE, &
!             IMS,IME,JMS,JME,KMS,KME, &
!             IPS,IPE,JPS,JPE,KPS,KPE)
       
!        call store_old_fixes(grid, &
!             IDS,IDE,JDS,JDE,KDS,KDE, &
!             IMS,IME,JMS,JME,KMS,KME, &
!             IPS,IPE,JPS,JPE,KPS,KPE)

!        ! Store the first guess:
!        grid%track_have_guess=.true.
!        grid%track_guess_lat=latguess
!        grid%track_guess_lon=longuess
! 3011   format('First guess: lon=',F0.3,' lat=',F0.3)
!        write(message,3011) grid%track_guess_lon,grid%track_guess_lat
!        call wrf_debug(1,message)
!     endif

  end subroutine ntc_impl

  subroutine get_first_ges(grid,  &
         iguess,jguess,longuess,latguess, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    ! This replicates the functionality of the tracker get_first_ges
    ! routine, whose purpose is to analyze the storm and guess where
    ! it will be at the next nest motion timestep.  It does that using
    ! two different methods, similar to the GFDL/NCEP Tracker's
    ! methods:
    !
    !  1. Use the present, and past few, fix locations and extrapolate
    !  to the next location.
    !  
    !  2. Calculate the mean motion and extrapolate to get the
    !  location at the next nest motion timestep.
    !
    ! The average of the two results is used.

#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_maxval_real
#endif
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    use module_wrf_error, only: wrf_at_debug_level
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer, intent(out) :: iguess,jguess
    real, intent(out) :: longuess,latguess

    character*255 message
    integer :: iold, inew, jold, jnew
    integer :: ifix,jfix,jrot,irot,ierr, pinky,brain, n, tsum, ntsd_plus_1, i, told
    real :: motion_grideast, motion_gridnorth, fixdx
    real :: dxeast,dynorth, xeast, ynorth
    real :: dxrot, dyrot, tracker_dt, xsum, ysum, ytsum, xtsum, xxsum, yysum, ttsum
    real :: mx, my, bx, by ! x=mx*t+bx ; y=my*t+by
    real :: xrot,yrot
    logical :: have_motion_guess, have_line_guess

    have_motion_guess=.false.
    have_line_guess=.false.

    if(grid%tracker_havefix) then
       ifix=grid%tracker_ifix
       jfix=grid%tracker_jfix

       call mean_motion(grid, motion_grideast, motion_gridnorth, &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE)

#ifdef DM_PARALLEL
       fixdx=0
       if(ifix>=ips .and. ifix<=ipe .and. jfix>=jps .and. jfix<=jpe) then
          fixdx=grid%dx_nmm(ifix,jfix)
       endif
       pinky=2 ; brain=308
       call wrf_dm_maxval_real(fixdx,pinky,brain)
#else
       fixdx=grid%dx_nmm(ifix,jfix)
#endif

       ! Rotated east and north motion in gridpoints per second, on the combined H+V grid:
       tracker_dt=grid%dt*grid%nphs*grid%ntrack
       dxeast = motion_grideast * tracker_dt / fixdx
       dynorth = motion_gridnorth * tracker_dt / grid%dy_nmm

       ! Combine the H & V coordinate systems and rotate 45 degrees.
       ! This puts the H points on a rectangular grid.  Add storm motion
       ! to the rotated coordinates and round to nearest H point
       xeast=ifix*2
       ynorth=jfix
       if(mod(jfix,2)==0) xeast=xeast+1
       jrot=nint((xeast+ynorth)/2 + (dxeast+dynorth)/2)
       irot=nint((ynorth-xeast)/2 + ((jde-1)/2) + (dynorth-dxeast)/2)

       ! Translate back to usual E grid H points
       iguess=irot-jrot+((jde-1)/2)
       jguess=irot+jrot-((jde-1)/2)
       if(mod(jguess,2)==0) then
          iguess=(iguess-1)/2
       else
          iguess=iguess/2
       endif

       ! This last step should not be necessary but done just in case:
       have_motion_guess = .not.(iguess<ide/4 .or. iguess>ide*3/4 .or. jguess<jde/4 .or. jguess>jde*3/4)

       !print *,'got have_motion_guess=',have_motion_guess
    endif

    if(.not.have_motion_guess) then
       ! Could not find the storm, so give the domain center as the
       ! next first guess location.
       iguess=ide/2
       jguess=jde/2
       !print *,'cannot find storm, so using domain center for motion guess'
    endif

    if(grid%track_n_old>0) then
       !print *,'line guess: have old'
       n=1
       call to_rot45_grid(grid%tracker_ifix,grid%tracker_jfix,jde,xrot,yrot)
       xsum=xrot
       ysum=yrot
       tsum=grid%ntsd
       xtsum=xsum*tsum
       xxsum=xsum*xsum
       yysum=ysum*ysum
       ytsum=ysum*tsum
       ttsum=tsum*tsum
       
       do i=1,grid%track_n_old
          call get_nearest_lonlat(grid,iold,jold,ierr, &
               grid%track_old_lon(i),grid%track_old_lat(i), &
               ids,ide, jds,jde, kds,kde, &
               ims,ime, jms,jme, kms,kme, &
               ips,ipe, jps,jpe, kps,kpe)
          if(ierr==0) then
             !print *,'insert: i=',iold,' j=',jold,' lon=',grid%track_old_lon(i),' lat=',grid%track_old_lat(i),' t=',grid%track_old_ntsd(i)
             call to_rot45_grid(iold,jold,jde,xrot,yrot)
             n=n+1
             xsum=xsum+xrot
             ysum=ysum+yrot
             told=grid%track_old_ntsd(i)
             tsum=tsum+told
             xtsum=xtsum+xrot*told
             xxsum=xxsum+xrot*xrot
             ytsum=ytsum+yrot*told
             yysum=xxsum+yrot*yrot
             ttsum=ttsum+told*told
          endif
       enddo
       !print *,'line guess: n=',n

       if(n>1) then
          ntsd_plus_1 = grid%ntsd + grid%ntrack*grid%nphs
          mx=(xtsum-(xsum*tsum)/real(n))/(ttsum-(tsum*tsum)/real(n))
          my=(ytsum-(ysum*tsum)/real(n))/(ttsum-(tsum*tsum)/real(n))
          bx=(xsum-mx*tsum)/real(n)
          by=(ysum-my*tsum)/real(n)
          !print *,'mx=',mx,' my=',my,' bx=',bx,' by=',by,' t+1=',ntsd_plus_1
          xrot=nint(mx*ntsd_plus_1+bx)
          yrot=nint(my*ntsd_plus_1+by)
          call from_rot45_grid(inew,jnew,jde,xrot,yrot)
          !print *,'inew=',inew,' jnew=',jnew,' xrot=',xrot,' yrot=',yrot
          have_line_guess=.not.(inew<ide/4 .or. inew>ide*3/4 &
               .or. jnew<jde/4 .or. jnew>jde*3/4)
       else
          have_line_guess=.false.
       endif
    endif

    print_locs: if(wrf_at_debug_level(2)) then
       call get_lonlat(grid,iguess,jguess,longuess,latguess,ierr, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe)
       if(ierr==0) then
          if(have_motion_guess) then
3088         format('Motion Guess: lon=',F0.3,' lat=',F0.3)
             write(message,3088) longuess,latguess
             call wrf_debug(2,message)
          else
3089         format('Motion Guess failed; use domain center: lon=',F0.3,' lat=',F0.3)
             write(message,3089) longuess,latguess
             call wrf_debug(2,message)
          endif
       else
3090      format('Motion guess ierr=',I0)
          write(message,3090) ierr
          call wrf_debug(2,message)
       endif
       if(have_line_guess) then
          call get_lonlat(grid,inew,jnew,longuess,latguess,ierr, &
               ids,ide, jds,jde, kds,kde, &
               ims,ime, jms,jme, kms,kme, &
               ips,ipe, jps,jpe, kps,kpe)
          if(ierr==0) then
3091         format('Line guess: lon=',F0.3,' lat=',F0.3)
             write(message,3091) longuess,latguess
             call wrf_debug(2,message)
          else
3092         format('Line guess ierr=',I0)
             write(message,3092) ierr
             call wrf_debug(2,message)
          endif
       endif
    endif print_locs

    if(have_line_guess) then
       if(have_motion_guess) then
          call wrf_debug(1,'get_first_ges: have MOTION and LINE guesses')
          iguess=(iguess+inew)/2
          jguess=(jguess+jnew)/2
       else
          call wrf_debug(1,'get_first_ges: have LINE guess only')
          iguess=inew
          jguess=jnew
       endif
    elseif(have_motion_guess) then
       call wrf_debug(1,'get_first_ges: have MOTION guess only')
    else
       call wrf_debug(1,'get_first_ges: have no guesses; will use domain center')
    endif

    ! Now get lats & lons:
    call get_lonlat(grid,iguess,jguess,longuess,latguess,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)
    if(ierr/=0) then
       ! Should never get here due to max/min check before.
       call wrf_error_fatal("ERROR: domain is not inside the domain in get_first_ges (!?)")
    endif

38  format('First guess: i=',I0,' j=',I0,' lat=',F8.3,' lon=',F8.3)
    write(message,38) iguess,jguess,latguess,longuess
    call wrf_message(message)
  end subroutine get_first_ges

  subroutine store_old_fixes(grid,  &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    ! This stores old fix locations for later use in the get_first_ges
    ! routine's line of best fit.
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer i
    if(grid%tracker_havefix) then
       !print *,'in store old, have fix'
       if(grid%track_n_old>0) then
          !print *,'in store old, shifting old'
          do i=1,grid%num_old_fixes-1
             grid%track_old_lon(i+1)=grid%track_old_lon(i)
             grid%track_old_lat(i+1)=grid%track_old_lat(i)
             grid%track_old_ntsd(i+1)=grid%track_old_ntsd(i)
          enddo
       endif
       grid%track_old_lon(1)=grid%tracker_fixlon
       grid%track_old_lat(1)=grid%tracker_fixlat
       grid%track_old_ntsd(1)=grid%ntsd
       grid%track_n_old=min(grid%num_old_fixes,grid%track_n_old+1)
       !print *,'in store old, now have ',grid%track_n_old
    endif
  end subroutine store_old_fixes

  subroutine to_rot45_grid(i,j,jde,x,y)
    implicit none
    integer, intent(in) :: i,j,jde
    real, intent(inout) :: x,y
    real :: a,b
    a=i*2
    b=j
    if(mod(j,2)==0) a=a+1
    x=(a+b)/2
    y=(b-a)/2+((jde-1)/2)
  end subroutine to_rot45_grid

  subroutine from_rot45_grid(i,j,jde,x,y)
    implicit none
    integer, intent(inout) :: i,j
    integer, intent(in) :: jde
    real, intent(in) :: x,y
    i=x-y+((jde-1)/2)
    j=x+y-((jde-1)/2)
    if(mod(j,2)==0) then
       i=(i-1)/2
    else
       i=i/2
    endif
  end subroutine from_rot45_grid

  subroutine get_nearest_lonlat(grid,iloc,jloc,ierr,lon,lat, &
               ids,ide, jds,jde, kds,kde, &
               ims,ime, jms,jme, kms,kme, &
               ips,ipe, jps,jpe, kps,kpe, &
               lonnear, latnear)
    ! Finds the nearest point in the domain to the specified lon,lat
    ! location.
#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_minloc_real
#endif
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer, intent(out) :: iloc,jloc,ierr
    real, intent(in) :: lon,lat
    real :: dx,dy,d,dmin, zdummy, latmin,lonmin
    integer :: i,j,imin,jmin
    real, intent(out), optional :: latnear, lonnear

    zdummy=42
    dmin=9e9
    imin=-99
    jmin=-99
    latmin=9e9
    lonmin=9e9
    ierr=0
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          dy=abs(lat-grid%hlat(i,j))
          dx=abs(mod(3600.+180.+(lon-grid%hlon(i,j)),360.)-180.)
          d=dx*dx+dy*dy
          if(d<dmin) then
             dmin=d
             imin=i
             jmin=j
             latmin=grid%hlat(i,j)
             lonmin=grid%hlon(i,j)
          endif
       enddo
    enddo

#ifdef DM_PARALLEL
    call wrf_dm_minloc_real(dmin,latmin,lonmin,zdummy,imin,jmin)
#endif
    if(imin<0 .or. jmin<0) then
       ierr=-99
    else
       iloc=imin ; jloc=jmin
    endif
    if(present(latnear)) latnear=latmin
    if(present(lonnear)) lonnear=lonmin
  end subroutine get_nearest_lonlat

  subroutine output_partial_atcfunix(grid, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         ITS,ITE,JTS,JTE,KTS,KTE)
    ! This outputs to a format that can be easily converted to ATCF,
    ! using units used by ATCF.
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE
    character*255 message
313 format(F11.2,", ",                                  &
           "W10 = ",F7.3," kn, PMIN = ",F8.3," mbar, ", &
           "LAT = ",F6.3,A1,", LON = ",F7.3,A1,", ",    &
           "RMW = ",F7.3," nmi")
    write(grid%outatcf_lun,313) grid%dt*grid%ntsd,                 &
         grid%tracker_vmax*mps2kn,grid%tracker_pmin/100.,          &
         abs(grid%tracker_fixlat),get_lat_ns(grid%tracker_fixlat), &
         abs(grid%tracker_fixlon),get_lon_ew(grid%tracker_fixlon), &
         grid%tracker_rmw*km2nmi
    ! write(message,313) grid%dt*grid%ntsd,                 &
    !      grid%tracker_vmax*mps2kn,grid%tracker_pmin/100.,          &
    !      abs(grid%tracker_fixlat),get_lat_ns(grid%tracker_fixlat), &
    !      abs(grid%tracker_fixlon),get_lon_ew(grid%tracker_fixlon), &
    !      grid%tracker_rmw*km2nmi
    ! call wrf_message(message)
  end subroutine output_partial_atcfunix

  subroutine get_wind_pres_intensity(grid, &
       min_mslp,max_wind,rmw, &
       max_wind_search_radius, min_mlsp_search_radius, clon,clat, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       ITS,ITE,JTS,JTE,KTS,KTE)
    ! This determines the maximum wind, RMW and minimum mslp in the domain.
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_maxval_real,wrf_dm_minval_real
#endif
    implicit none
    type(domain), intent(inout) :: grid
    real, intent(out) :: min_mslp,max_wind,rmw
    real, intent(in) :: max_wind_search_radius, min_mlsp_search_radius,clon,clat
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE

    real :: localextreme,globalextreme, sdistsq,windsq
    real :: globallat,globallon,degrees
    integer :: locali,localj,globali,globalj,ierr,i,j

    ! Get the MSLP minimum location and determine if what we found is
    ! still a storm:
    localextreme=9e9
    locali=-1
    localj=-1
    sdistsq=min_mlsp_search_radius*min_mlsp_search_radius*1e6
    do j=jts,min(jte,jde-1)
       do i=its,min(ite,ide-1)
          if(grid%membrane_mslp(i,j)<localextreme .and. &
               grid%tracker_distsq(i,j)<sdistsq) then
             localextreme=grid%membrane_mslp(i,j)
             locali=i
             localj=j
          endif
       enddo
    enddo

    globalextreme=localextreme
    globali=locali
    globalj=localj
#ifdef DM_PARALLEL
    call wrf_dm_minval_real(globalextreme,globali,globalj)
#endif
    min_mslp=globalextreme
    if(globali<0 .or. globalj<0) then
       call wrf_message("WARNING: No membrane mslp values found that were less than 9*10^9.")
       min_mslp=-999
    endif

    ! Get the wind maximum location.  Note that we're using the square
    ! of the wind until after the loop to avoid the sqrt() call.
    localextreme=-9e9
    locali=-1
    localj=-1
    sdistsq=max_wind_search_radius*max_wind_search_radius*1e6
    do j=jts,min(jte,jde-1)
       do i=its,min(ite,ide-1)
          if(grid%tracker_distsq(i,j)<sdistsq) then
             windsq=grid%u10(i,j)*grid%u10(i,j) + &
                    grid%v10(i,j)*grid%v10(i,j)
             if(windsq>localextreme) then
                localextreme=windsq
                locali=i
                localj=j
             endif
          endif
       enddo
    enddo
    if(localextreme>0) localextreme=sqrt(localextreme)

    globalextreme=localextreme
    globali=locali
    globalj=localj
#ifdef DM_PARALLEL
    call wrf_dm_maxval_real(globalextreme,globali,globalj)
#endif

    call get_lonlat(grid,globali,globalj,globallon,globallat,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte)
    if(ierr/=0) then
       call wrf_message("WARNING: Unable to find location of wind maximum.")
       rmw=-99
    else
       call calcdist(clon,clat,globallon,globallat,rmw,degrees)
    end if

    ! Get the guess location for the next time:
    max_wind=globalextreme
    if(globali<0 .or. globalj<0) then
       call wrf_message("WARNING: No wind values found that were greater than -9*10^9.")
       min_mslp=-999
    endif

  end subroutine get_wind_pres_intensity

  subroutine mean_motion(grid,motion_grideast,motion_gridnorth, &
       ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme, &
       its,ite, jts,jte, kts,kte)
    ! This calculates the mean motion of the storm by calculating the
    ! average wind vector at 850, 700 and 500 mbars.

#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_sum_real8, wrf_dm_sum_integer
#endif
    use module_wrf_error
    USE MODULE_DOMAIN, ONLY : domain, domain_clock_get
    implicit none
    integer, intent(in) :: &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte
    type(domain), intent(in) :: grid
    real, intent(out) :: motion_grideast,motion_gridnorth
    integer :: count,i,j,ierr
    real :: distsq, dist
    real*8 :: e,n

    e=0 ; n=0 ; count=0 ! east sum, north sum, count

    dist = min(grid%tracker_edge_dist, max(50e3, 3e3*grid%tracker_rmw))
    distsq = dist * dist

    ! print *,'motion search radius (m) = ',dist
    ! print *,'  considered edge dist = ',grid%tracker_edge_dist
    ! print *,'  considered 3e3*rmw = ',3e3*grid%tracker_rmw
    ! print *,'  considered 50e3.'

    do j=jts,min(jte,jde-1)
       do i=its,min(ite,ide-1)
          if(grid%tracker_distsq(i,j)<distsq) then
             count = count + 3
             e = e + (grid%p500u(i,j) + grid%p700u(i,j) + grid%p850u(i,j))
             n = n + (grid%p500v(i,j) + grid%p700v(i,j) + grid%p850v(i,j))
          endif
       enddo
    enddo

#ifdef DM_PARALLEL
    e=wrf_dm_sum_real8(e)
    n=wrf_dm_sum_real8(n)
    count=wrf_dm_sum_integer(count)
#endif

    motion_grideast=e/count
    motion_gridnorth=n/count

    ! print *,'e=',e,' n=',n,' count=',count
    ! print *,'motion: east=',motion_grideast,' north=',motion_gridnorth
  end subroutine mean_motion

  subroutine fixcenter(grid,icen,jcen,calcparm,loncen,latcen, &
       iguess,jguess,longuess,latguess, &
       ifinal,jfinal,lonfinal,latfinal, &
       north_hemi, &
       ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme, &
       ips,ipe, jps,jpe, kps,kpe)
    ! This is the same as "fixcenter" in gettrk_main.  Original comment:
    !
    ! ABSTRACT: This subroutine loops through the different parameters
    !           for the input storm number (ist) and calculates the 
    !           center position of the storm by taking an average of
    !           the center positions obtained for those parameters.
    !           First we check to see which parameters are within a 
    !           max error range (errmax), and we discard those that are
    !           not within that range.  Of the remaining parms, we get 
    !           a mean position, and then we re-calculate the position
    !           by giving more weight to those estimates that are closer
    !           to this mean first-guess position estimate.

    ! Arguments: Input:
    ! grid - the grid being processed
    ! icen,jcen - arrays of center gridpoint locations
    ! calcperm - array of center validity flags (true = center is valid)
    ! loncen,latcen - center geographic locations
    ! iguess,jguess - first guess gridpoint location
    ! longuess,latguess - first guess geographic location

    ! Arguments: Output:
    ! ifinal,jfinal - final center gridpoint location
    ! lonfinal,latfinal - final center geographic location

    ! Arguments: Optional input:
    ! north_hemi - true = northern hemisphere, false=south

    use module_wrf_error
    USE MODULE_DOMAIN, ONLY : domain, domain_clock_get
    implicit none
    integer, intent(in) :: &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe
    type(domain), intent(inout) :: grid
    integer, intent(in) :: icen(maxtp), jcen(maxtp)
    real, intent(in) :: loncen(maxtp), latcen(maxtp)
    logical, intent(inout) :: calcparm(maxtp)

    integer, intent(in) :: iguess,jguess
    real, intent(in) :: latguess,longuess

    integer, intent(inout) :: ifinal,jfinal
    real, intent(inout) :: lonfinal,latfinal

    logical, intent(in), optional :: north_hemi

    character*255 :: message
    real :: errdist(maxtp),avgerr,errmax,errinit,xavg_stderr
    real :: dist,degrees, total
    real :: minutes,hours,trkerr_avg, dist_from_mean(maxtp),wsum
    integer :: ip,itot4next,iclose,count,ifound,ierr
    integer(kind=8) :: isum,jsum
    real :: irsum,jrsum,errtmp,devia,wtpos
    real :: xmn_dist_from_mean, stderr_close
    logical use4next(maxtp)

    ! Determine forecast hour:
    call domain_clock_get(grid,minutesSinceSimulationStart=minutes)
    hours=minutes/60.

    ! Decide maximum values for distance and std. dev.:
    if(hours<0.5) then
       errmax=err_reg_init
       errinit=err_reg_init
    else
       errmax=err_reg_max
       errinit=err_reg_max
    endif

    if(hours>4.) then
       xavg_stderr = ( grid%track_stderr_m1 + &
            grid%track_stderr_m2 + grid%track_stderr_m3 ) / 3.0
    elseif(hours>3.) then
       xavg_stderr = ( grid%track_stderr_m1 + grid%track_stderr_m2 ) / 2.0
    elseif(hours>2.) then
       xavg_stderr = grid%track_stderr_m1
    endif

    if(hours>2.) then
       errtmp = 3.0*xavg_stderr*errpgro
       errmax = max(errtmp,errinit)
       errtmp = errpmax
       errmax = min(errmax,errtmp)
    endif

    ! Initialize loop variables:
    errdist=0.0
    use4next=.false.
    trkerr_avg=0
    itot4next=0
    iclose=0
    isum=0
    jsum=0
    ifound=0

    !write(0,*) 'errpmax=',errpmax
    !write(0,*) 'errmax=',errmax

500 format('Parm ip=',I0,' dist=',F0.3)
501 format('  too far, but discard')
    do ip=1,maxtp
       if(ip==4 .or. ip==6) then
          calcparm(ip)=.false.
          cycle
       elseif(calcparm(ip)) then
          ifound=ifound+1
          call calcdist(longuess,latguess,loncen(ip),latcen(ip),dist,degrees)
          errdist(ip)=dist
          !write(0,500) ip,dist
          if(dist<=errpmax) then
             if(ip==3 .or. ip==5 .or. ip==10) then
                use4next(ip)=.false.
                !write(0,'(A)') '  within range but discard: errpmax'
             else
                !write(0,'(A)') '  within range and keep: errpmax'
                use4next(ip)=.true.
                trkerr_avg=trkerr_avg+dist
                itot4next=itot4next+1
             endif
          endif
          if(dist<=errmax) then
502          format('  apply i=',I0,' j=',I0)
             !write(0,502) icen(ip),jcen(ip)
             iclose=iclose+1
             isum=isum+icen(ip)
             jsum=jsum+jcen(ip)
503          format(' added things isum=',I0,' jsum=',I0,' iclose=',I0)
             !write(0,503) isum,jsum,iclose
          else
             !write(0,*) '  discard; too far: errmax'
             calcparm(ip)=.false.
          endif
       endif
    enddo

    if(ifound<=0) then
       call wrf_message('The tracker could not find the centers for any parameters.  Thus,')
       call wrf_message('a center position could not be obtained for this storm.')
       goto 999
    endif

    if(iclose<=0) then
200    format('No storms are within errmax=',F0.1,'km of the parameters')
       !write(message,200) errmax
       call wrf_message(message)
       goto 999
    endif

    ifinal=real(isum)/real(iclose)
    jfinal=real(jsum)/real(iclose)

504 format(' calculated ifinal, jfinal: ifinal=',I0,' jfinal=',I0,' isum=',I0,' jsum=',I0,' iclose=',I0)
    !write(0,504) ifinal,jfinal,isum,jsum,iclose

    call get_lonlat(grid,ifinal,jfinal,lonfinal,latfinal,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)
    if(ierr/=0) then
       call wrf_message('Gave up on finding the storm location due to error in get_lonlat (1).')
       goto 999
    endif

    count=0
    dist_from_mean=0.0
    total=0.0
    do ip=1,maxtp
       if(calcparm(ip)) then
          call calcdist(lonfinal,latfinal,loncen(ip),latcen(ip),dist,degrees)
          dist_from_mean(ip)=dist
          total=total+dist
          count=count+1
       endif
    enddo
    xmn_dist_from_mean=total/real(count)

    do ip=1,maxtp
       if(calcparm(ip)) then
          total=total+(xmn_dist_from_mean-dist_from_mean(ip))**2
       endif
    enddo
    if(count<2) then
       stderr_close=0.0
    else
       stderr_close=max(1.0,sqrt(1./(count-1) * total))
    endif

    if(calcparm(1) .or. calcparm(2) .or. calcparm(7) .or. &
         calcparm(8) .or. calcparm(9) .or. calcparm(11)) then
       continue
    else
       ! Message copied straight from tracker:
       call wrf_message('In fixcenter, STOPPING PROCESSING for this storm.  The reason is that')
       call wrf_message('none of the fix locations for parms z850, z700, zeta 850, zeta 700')
       call wrf_message('MSLP or sfc zeta were within a reasonable distance of the guess location.')
       goto 999
    endif

    ! Recalculate the final center location using weights
    if(stderr_close<5.0) then
       ! Old code forced a minimum of 5.0 stddev
       stderr_close=5.0
    endif
    irsum=0
    jrsum=0
    wsum=0
    do ip=1,maxtp
       if(calcparm(ip)) then
          devia=max(1.0,dist_from_mean(ip)/stderr_close)
          wtpos=exp(-devia/3.)
          irsum=icen(ip)*wtpos+irsum
          jrsum=jcen(ip)*wtpos+jrsum
          wsum=wtpos+wsum
1100      format(' Adding parm: devia=',F0.3,' wtpos=',F0.3,' irsum=',F0.3,' jrsum=',F0.3,' wsum=',F0.3)
          !write(0,1100) devia,wtpos,irsum,jrsum,wsum
       endif
    enddo
    ifinal=nint(real(irsum)/real(wsum))
    jfinal=nint(real(jrsum)/real(wsum))
    call get_lonlat(grid,ifinal,jfinal,lonfinal,latfinal,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)
    if(ierr/=0) then
       call wrf_message('Gave up on finding the storm location due to error in get_lonlat (2).')
       goto 999
    endif

    ! Store the lat/lon location:
    grid%tracker_fixlon=lonfinal
    grid%tracker_fixlat=latfinal
    grid%tracker_ifix=ifinal
    grid%tracker_jfix=jfinal
    grid%tracker_havefix=.true.

1000 format('Stored lat/lon at i=',I0,' j=',I0,' lon=',F0.3,' lat=',F0.3)
    !write(0,1000) ifinal,jfinal,lonfinal,latfinal
    

    if(nint(hours) > grid%track_last_hour ) then
       ! It is time to recalculate the std. dev. of the track:
       count=0
       dist_from_mean=0.0
       total=0.0
       do ip=1,maxtp
          if(calcparm(ip)) then
             call calcdist(lonfinal,latfinal,loncen(ip),loncen(ip),dist,degrees)
             dist_from_mean(ip)=dist
             total=total+dist
             count=count+1
          endif
       enddo
       xmn_dist_from_mean=total/real(count)

       do ip=1,maxtp
          if(calcparm(ip)) then
             total=total+(xmn_dist_from_mean-dist_from_mean(ip))**2
          endif
       enddo
       if(count<2) then
          stderr_close=0.0
       else
          stderr_close=max(1.0,sqrt(1./(count-1) * total))
       endif

       grid%track_stderr_m3=grid%track_stderr_m2
       grid%track_stderr_m2=grid%track_stderr_m1
       grid%track_stderr_m1=stderr_close
       grid%track_last_hour=nint(hours)
    endif

    !write(0,*) 'got to return'
    return

    ! We jump here if we're giving up on finding the center
999 continue
    ! Use domain center as storm location
    grid%tracker_ifix=ide/2
    grid%tracker_jfix=jde/2
    grid%tracker_havefix=.false.
    grid%tracker_gave_up=.true.
    call get_lonlat(grid,ifinal,jfinal,lonfinal,latfinal,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)
    if(ierr/=0) then
       call wrf_error_fatal('Center of domain is not in domain (!?)')
       goto 999
    endif

    grid%tracker_fixlon=-999.0
    grid%tracker_fixlat=-999.0
    
  end subroutine fixcenter

  subroutine get_uv_guess(grid,icen,jcen,loncen,latcen,calcparm, &
       iguess,jguess,longuess,latguess,iout,jout, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       ITS,ITE,JTS,JTE,KTS,KTE)
    ! This is a rewrite of the gettrk_main.f get_uv_guess.  Original comment:
    ! ABSTRACT: The purpose of this subroutine is to get a modified 
    !           first guess lat/lon position before searching for the 
    !           minimum in the wind field.  The reason for doing this is
    !           to better refine the guess and avoid picking up a wind
    !           wind minimum far away from the center.  So, use the 
    !           first guess position (and give it strong weighting), and
    !           then also use the  fix positions for the current time
    !           (give the vorticity centers stronger weighting as well),
    !           and then take the average of these positions.

    ! Arguments: Input:
    !  grid - grid being searched
    !  icen,jcen - tracker parameter center gridpoints
    !  loncen,latcen - tracker parameter centers' geographic locations
    !  calcparm - is each center valid?
    !  iguess, jguess - first guess gridpoint location
    !  longuess,latguess - first guess geographic location

    ! Arguments: Output:
    !  iout,jout - uv guess center location

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_sum_real
#endif
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE

    integer, intent(in) :: icen(maxtp), jcen(maxtp)
    real, intent(in) :: loncen(maxtp), latcen(maxtp)
    logical, intent(in) :: calcparm(maxtp)

    integer, intent(in) :: iguess,jguess
    real, intent(in) :: latguess,longuess

    integer, intent(inout) :: iout,jout
    real :: degrees,dist
    integer :: ip,ict
    integer(kind=8) :: isum,jsum

    ict=2
    isum=2*iguess
    jsum=2*jguess

    ! Get a guess storm center location for searching for the wind centers:
    do ip=1,maxtp
       if ((ip > 2 .and. ip < 7) .or. ip == 10) then
          cycle   ! because 3-6 are for 850 & 700 u & v and 10 is 
          ! for surface wind magnitude.
       elseif(calcparm(ip)) then
          call calcdist (longuess,latguess,loncen(ip),latcen(ip),dist,degrees)
          if(dist<uverrmax) then
             if(ip==1 .or. ip==2 .or. ip==11) then
                isum=isum+2*icen(ip)
                jsum=jsum+2*jcen(ip)
                ict=ict+2
             else
                isum=isum+icen(ip)
                jsum=jsum+jcen(ip)
                ict=ict+1
             endif
          endif
       endif
    enddo

    iout=nint(real(isum)/real(ict))
    jout=nint(real(jsum)/real(ict))
  end subroutine get_uv_guess

  subroutine get_uv_center(grid,orig, &
       iout,jout,rout,calcparm,lonout,latout, &
       dxdymean,cparm, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE, &
       iuvguess,juvguess) 

    use module_wrf_error
#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_minval_real, wrf_dm_maxval_real, wrf_dm_sum_real
#endif
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    use module_relax
    implicit none

    integer, intent(in) :: iuvguess,juvguess
    type(domain), intent(inout) :: grid
    character*(*), intent(in) :: cparm
    real, intent(in) :: dxdymean
    real, intent(inout) :: rout
    integer, intent(inout) :: iout,jout
    logical, intent(inout) :: calcparm
    real, intent(inout) :: latout,lonout
    real, intent(in) :: orig(ims:ime,jms:jme)

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    integer :: icen,jcen, i,j, istart,istop, jstart,jstop, ierr
    real :: rcen, srsq

    ! Restrict the search area.  By default, we search everywhere except the boundary:
    istart=max(ids+1,ips)
    istop=min(ide-2,ipe)
    jstart=max(jds+2,jps)
    jstop=min(jde-3,jpe)

    ! If the guess location is given, then further restrict the search area:
    istart=max(istart,iuvguess-nint(rads_vmag/(2.*dxdymean)))
    istop=min(istop,iuvguess+nint(rads_vmag/(2.*dxdymean)))
    jstart=max(jstart,juvguess-nint(rads_vmag/(2.*dxdymean)))
    jstop=min(jstop,juvguess+nint(rads_vmag/(2.*dxdymean)))
    
    srsq=rads_vmag*rads_vmag*1e6

    icen=-99
    jcen=-99
    rcen=9e9
    do j=jstart,jstop
       do i=istart,istop
          if(orig(i,j)<rcen .and. grid%distsq(i,j)<srsq) then
             rcen=orig(i,j)
             icen=i
             jcen=j
          endif
       enddo
    enddo

#ifdef DM_PARALLEL
       call wrf_dm_minval_real(rcen,icen,jcen)
       !write(0,*) 'global',icen,jcen,rcen
#endif

    ! Return result:
    resultif: if(icen==-99 .or. jcen==-99) then
       ! No center found.
       calcparm=.false.
       !write(0,*) 'no center found'
    else
       iout=icen
       jout=jcen
       rout=rcen
       calcparm=.true.
       call get_lonlat(grid,iout,jout,lonout,latout,ierr, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe) 
       if(ierr/=0) then
          !write(0,*) 'bad lonlat'
          calcparm=.false.
          return
       endif
       !write(0,*) 'center found; lon=',lonout,' lat=',latout
    endif resultif
  end subroutine get_uv_center

  subroutine find_center(grid,orig,smooth,srsq, &
       iout,jout,rout,calcparm,lonout,latout, &
       dxdymean,cparm, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE, &
       iuvguess,juvguess,north_hemi)
    ! This routine replaces the gettrk_main functions find_maxmin and
    ! get_uv_center.

    ! Finds the minimum or maximum value of the smoothed version
    ! (smooth) of the given field (orig).  If a center cannot be
    ! found, sets calcparm=.false., otherwise places the longitude in
    ! lonout and latitude in latout, gridpoint location in (iout,jout)

    ! Mandatory arguments:

    ! grid - grid to search
    ! orig - field to search
    ! smooth - smoothed version of the field (smoothed via relax4e)
    ! iout,jout - center location
    ! rout - center value (min MSLP, min wind, max or min zeta, etc.)
    ! calcparm - true if a center was found, false otherwise
    ! lonout,latout - geographic location of the center
    ! dxdymean - mean H-to-V gridpoint distance of the entire domain
    ! cparm - which type of field: zeta, hgt, wind, slp
    ! srsq - square of the maximum radius from domain center to search
    ! ids, ..., kpe - grid, memory and patch dimensions

    ! Optional arguments:

    ! iuvguess,juvguess - first guess center location to restrict search
    ! to a subset of the grid.
    ! north_hemi - we're in the northern hemisphere: true or false?

    use module_wrf_error
#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_minval_real, wrf_dm_maxval_real, wrf_dm_sum_real
#endif
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    use module_relax
    implicit none

    integer, intent(in), optional :: iuvguess,juvguess
    type(domain), intent(inout) :: grid
    character*(*), intent(in) :: cparm
    real, intent(in) :: dxdymean, srsq
    real, intent(inout) :: rout
    integer, intent(inout) :: iout,jout
    logical, intent(inout) :: calcparm
    real, intent(inout) :: latout,lonout
    real, intent(in) :: orig(ims:ime,jms:jme)
    real, intent(out) :: smooth(ims:ime,jms:jme)
    character*255 :: message
    logical, optional :: north_hemi

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    integer :: icen,jcen,i,j,ismooth,ierr
    real :: rcen, here, sum, mean, cendist, heredist

    integer :: istart,istop, jstart,jstop,itemp

    logical :: findmin

    ! Emulate the tracker's barnes analysis with a 1/e iterative smoother:
    grid%relaxmask=.false.
    do j=max(jds+2,jps),min(jde-3,jpe)
       do i=max(ids+2,ips),min(ide-3,ipe)
          grid%relaxmask(i,j)=.true.
       enddo
    enddo
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          grid%relaxwork(i,j)=orig(i,j)
       enddo
    enddo

    ! Decide how many smoother iterations to do based on the parameter
    ! and grid spacing:
    if(trim(cparm)=='wind') then
       itemp=nint(1.2*111./(dxdymean*sqrt(2.)))
       ismooth=min(30,max(2,itemp))
       !write(0,*) 'wind itemp=',itemp,' ismooth=',ismooth,' dxdymean=',dxdymean
    elseif(grid%vortex_tracker==6) then
       itemp=nint(0.3*111./(dxdymean*sqrt(2.)))
       ismooth=min(15,max(1,itemp))
       !write(0,*) 'non-wind itemp=',itemp,' ismooth=',ismooth,' dxdymean=',dxdymean
    else
       itemp=nint(150./(dxdymean*sqrt(2.)))
       ismooth=min(50,max(2,itemp))
    endif

    ! Restrict the search area.  By default, we search everywhere except the boundary:
    istart=max(ids+1,ips)
    istop=min(ide-2,ipe)
    jstart=max(jds+2,jps)
    jstop=min(jde-3,jpe)

    ! If the guess location is given, then further restrict the search area:
    if(present(iuvguess)) then
       istart=max(istart,iuvguess-nint(rads_vmag/(2.*dxdymean)))
       istop=min(istop,iuvguess+nint(rads_vmag/(2.*dxdymean)))
    endif
    if(present(juvguess)) then
       jstart=max(jstart,juvguess-nint(rads_vmag/(2.*dxdymean)))
       jstop=min(jstop,juvguess+nint(rads_vmag/(2.*dxdymean)))
    endif

    ! Call the smoother:
    !write(0,*) 'SMOOTH: ',ismooth
    call relax4e(grid,real(0.59539032480831),ismooth,0, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    
    ! Copy the smoothed data back in:
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          smooth(i,j)=grid%relaxwork(i,j)
       enddo
    enddo
       
    ! Figure out whether we're finding a min or max:
    ifmin: if(trim(cparm)=='zeta') then
       if(.not.present(north_hemi)) then
          call wrf_error_fatal('When calling module_tracker find_center for zeta, you must specify the hemisphere parameter.')
       endif
       findmin=.not.north_hemi
    elseif(trim(cparm)=='hgt') then
       findmin=.true.
    elseif(trim(cparm)=='slp') then
       findmin=.true.
    elseif(trim(cparm)=='wind') then
       findmin=.true.
    else
100    format('Invalid parameter cparm="',A,'" in module_tracker find_center')
       !write(message,100) trim(cparm)
       call wrf_error_fatal(message)
    endif ifmin

3011 format('ips=',I0,' ipe=',I0,' istart=',I0,' istop=',I0)
3012 format('jps=',I0,' jpe=',I0,' jstart=',I0,' jstop=',I0)
    !write(0,3011) ips,ipe,istart,istop
    !write(0,3012) jps,jpe,jstart,jstop

    ! Find the extremum:
    icen=-99
    jcen=-99
    findminmax: if(findmin) then ! Find a minimum
       rcen=9e9
       do j=jstart,jstop
          do i=istart,istop
             if(grid%relaxwork(i,j)<rcen .and. grid%distsq(i,j)<srsq) then
                rcen=grid%relaxwork(i,j)
                icen=i
                jcen=j
             endif
          enddo
       enddo
3013   format(A,' minval i=',I0,' j=',I0,' r=',F0.3)
       !write(0,3013) 'local',icen,jcen,rcen
#ifdef DM_PARALLEL
       call wrf_dm_minval_real(rcen,icen,jcen)
       !write(0,3013) 'global',icen,jcen,rcen
#endif
    else ! Find a maximum
3014   format(A,' maxval i=',I0,' j=',I0,' r=',F0.3)
       rcen=-9e9
       do j=jstart,jstop
          do i=istart,istop
             if(grid%relaxwork(i,j)>rcen .and. grid%distsq(i,j)<srsq) then
                rcen=grid%relaxwork(i,j)
                icen=i
                jcen=j
             endif
          enddo
       enddo
       !write(0,3014) 'local',icen,jcen,rcen
#ifdef DM_PARALLEL
       call wrf_dm_maxval_real(rcen,icen,jcen)
       !write(0,3014) 'global',icen,jcen,rcen
#endif
    endif findminmax

    ! Return result:
    resultif: if(icen==-99 .or. jcen==-99) then
       ! No center found.
       calcparm=.false.
       !write(0,*) 'no center found'
    else
       iout=icen
       jout=jcen
       rout=rcen
       calcparm=.true.
       call get_lonlat(grid,iout,jout,lonout,latout,ierr, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe) 
       if(ierr/=0) then
          !write(0,*) 'bad lonlat'
          calcparm=.false.
          return
       endif
       !write(0,*) 'center found; lon=',lonout,' lat=',latout
    endif resultif
  end subroutine find_center

  subroutine get_tracker_distsq(grid, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         ITS,ITE,JTS,JTE,KTS,KTE)
    ! This computes approximate distances in km from the tracker
    ! center of the various points in the domain.  It uses the same
    ! computation as used for distsq: the calculation is done in
    ! gridpoint space, approximating the domain as flat.
    ! Point-to-point distances come from grid%dx_nmm and grid%dy_nmm.
    ! This routine also determines the distance from the tracker
    ! center location to the nearest point in the domain edge.
#ifdef DM_PARALLEL
    use module_dm, only: wrf_dm_minval_real
#endif
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    character*255 message
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE
    integer i,j,cx,cy,ierr
    integer wilbur,harvey ! filler variables for a function call
    real xfar,yfar,far,xshift,max_edge_distsq,clatr,clonr
    real ylat1,ylat2,xlon1,xlon2,mindistsq

    cx=grid%tracker_ifix
    cy=grid%tracker_jfix

    call get_lonlat(grid,cx,cy,clonr,clatr,ierr, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         ITS,ITE,JTS,JTE,KTS,KTE)
    if(ierr/=0) then
       call wrf_error_fatal('Tracker fix location is not inside domain.')
    end if

    do j=jts,min(jte,jde-1)
       if(mod(j,2)==1) then
          xshift=1.
       else
          xshift=-1.
       endif
       do i=its,min(ite,ide-1)
          xfar=(i-cx)*grid%dx_nmm(i,j)*2
          yfar=(j-cy)*grid%dy_nmm
          if(mod(cy-j,2) /= 0) then
             xfar=xfar + grid%dx_nmm(i,j)*xshift
          endif
          far = xfar*xfar + yfar*yfar
          GRID%tracker_distsq(i,j)=far
       enddo
    enddo

    ! Determine angle.  Note that this is mathematical angle, not
    ! compass angle, and is in geographic lat/lon, not rotated
    ! lat/lon.  (Geographic East=0, geographic North=pi/2.)
    xlon1=clonr ; ylat1=clatr
    call clean_lon_lat(xlon1,ylat1)
    xlon1=xlon1*pi180
    ylat1=ylat1*pi180
    do j=jts,min(jte,jde-1)
       do i=its,min(ite,ide-1)
          xlon2=grid%glon(i,j)
          ylat2=grid%glat(i,j)
          call clean_lon_lat(xlon2,ylat2)
          xlon2=xlon2*pi180
          ylat2=ylat2*pi180
          grid%tracker_angle(i,j)=atan2(xlon2-xlon1,ylat2-ylat1)
       enddo
    enddo

    ! Determine the distance between the center location and the
    ! domain edge.
    mindistsq=9e19
    if(jts==jds) then
       mindistsq=min(mindistsq,minval(grid%tracker_distsq(its:min(ite,ide-1),jds)))
    endif
    if(jte==jde) then
       mindistsq=min(mindistsq,minval(grid%tracker_distsq(its:min(ite,ide-1),jde-1)))
    endif
    if(its==ids) then
       mindistsq=min(mindistsq,minval(grid%tracker_distsq(ids,jts:min(jte,jde-1))))
    endif
    if(ite==ide) then
       mindistsq=min(mindistsq,minval(grid%tracker_distsq(ide-1,jts:min(jte,jde-1))))
    endif
#ifdef DM_PARALLEL
    wilbur=1
    harvey=2
    call wrf_dm_minval_real(mindistsq,wilbur,harvey)
#endif

    grid%tracker_edge_dist=sqrt(mindistsq)

17  format('Min distance from lon=',F9.3,', lat=',F9.3,' to center is ',F19.3)
    !print *,'Min distance from edge to center is ',grid%tracker_edge_dist
    write(message,17) clonr, clatr, grid%tracker_edge_dist
    call wrf_message(message)
  end subroutine get_tracker_distsq

  subroutine clean_lon_lat(xlon1,ylat1)
    real, intent(inout) :: xlon1,ylat1
    ! This modifies a (lat,lon) pair so that the longitude fits
    ! between [-180,180] and the latitude between [-90,90], taking
    ! into account spherical geometry.
    ! NOTE: inputs and outputs are in degrees
    xlon1=(mod(xlon1+3600.+180.,360.)-180.)
    ylat1=(mod(ylat1+3600.+180.,360.)-180.)
    if(ylat1>90.) then
       ylat1=180.-ylat1
       xlon1=mod(xlon1+360.,360.)-180.
    elseif(ylat1<-90.) then
       ylat1=-180. - ylat1
       xlon1=mod(xlon1+360.,360.)-180.
    endif
  end subroutine clean_lon_lat

  subroutine calcdist(rlonb,rlatb,rlonc,rlatc,xdist,degrees)
    ! Copied from gettrk_main.f
    !
    !     ABSTRACT: This subroutine computes the distance between two 
    !               lat/lon points by using spherical coordinates to 
    !               calculate the great circle distance between the points.
    !                       Figure out the angle (a) between pt.B and pt.C,
    !             N. Pole   then figure out how much of a % of a great 
    !               x       circle distance that angle represents.
    !              / \
    !            b/   \     cos(a) = (cos b)(cos c) + (sin b)(sin c)(cos A)
    !            /     \                                             .
    !        pt./<--A-->\c     NOTE: The latitude arguments passed to the
    !        B /         \           subr are the actual lat vals, but in
    !                     \          the calculation we use 90-lat.
    !               a      \                                      .
    !                       \pt.  NOTE: You may get strange results if you:
    !                         C    (1) use positive values for SH lats AND
    !                              you try computing distances across the 
    !                              equator, or (2) use lon values of 0 to
    !                              -180 for WH lons AND you try computing
    !                              distances across the 180E meridian.
    !    
    !     NOTE: In the diagram above, (a) is the angle between pt. B and
    !     pt. C (with pt. x as the vertex), and (A) is the difference in
    !     longitude (in degrees, absolute value) between pt. B and pt. C.
    !
    !     !!! NOTE !!! -- THE PARAMETER ecircum IS DEFINED (AS OF THE 
    !     ORIGINAL WRITING OF THIS SYSTEM) IN KM, NOT M, SO BE AWARE THAT
    !     THE DISTANCE RETURNED FROM THIS SUBROUTINE IS ALSO IN KM.
    !
    implicit none

    real, intent(inout) :: degrees
    real, intent(out) :: xdist
    real, intent(in) :: rlonb,rlatb,rlonc,rlatc
    real, parameter :: dtr = 0.0174532925199433
    real :: distlatb,distlatc,pole,difflon,cosanga,circ_fract
    !
    if (rlatb < 0.0 .or. rlatc < 0.0) then
       pole = -90.
    else
       pole = 90.
    endif
    !
    distlatb = (pole - rlatb) * dtr
    distlatc = (pole - rlatc) * dtr
    difflon  = abs( (rlonb - rlonc)*dtr )
    !
    cosanga = ( cos(distlatb) * cos(distlatc) + &
         sin(distlatb) * sin(distlatc) * cos(difflon))

    !     This next check of cosanga is needed since I have had ACOS crash
    !     when calculating the distance between 2 identical points (should
    !     = 0), but the input for ACOS was just slightly over 1
    !     (e.g., 1.00000000007), due to (I'm guessing) rounding errors.

    if (cosanga > 1.0) then
       cosanga = 1.0
    endif

    degrees    = acos(cosanga) / dtr
    circ_fract = degrees / 360.
    xdist      = circ_fract * ecircum
    !
    !     NOTE: whether this subroutine returns the value of the distance
    !           in km or m depends on the scale of the parameter ecircum. 
    !           At the original writing of this subroutine (7/97), ecircum
    !           was given in km.
    !
    return
  end subroutine calcdist

  ! subroutine get_lonlat(grid,iguess,jguess,longuess,latguess, &
  !       ids,ide, jds,jde, kds,kde, &
  !       ims,ime, jms,jme, kms,kme, &
  !       ips,ipe, jps,jpe, kps,kpe)
  !   ! Returns the latitude (latguess) and longitude (longuess) of the
  !   ! specified location (iguess,jguess) in the specified grid.
  !   USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
  !   USE MODULE_DM, ONLY: wrf_dm_at_ij_real
  !   implicit none
  !   integer, intent(in) :: &
  !       ids,ide, jds,jde, kds,kde, &
  !       ims,ime, jms,jme, kms,kme, &
  !       ips,ipe, jps,jpe, kps,kpe
  !   type(domain), intent(inout) :: grid
  !   integer, intent(in) :: iguess,jguess
  !   real, intent(inout) :: longuess,latguess

  !   call wrf_dm_at_ij_real(grid,iguess,jguess,ims,ime, jms,jme, &
  !        longuess,grid%hlon, &
  !       val2=latguess,field2=grid%hlat)
  ! end subroutine get_lonlat


  subroutine get_lonlat(grid,iguess,jguess,longuess,latguess,ierr, &
       ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme, &
       ips,ipe, jps,jpe, kps,kpe)
    ! Returns the latitude (latguess) and longitude (longuess) of the
    ! specified location (iguess,jguess) in the specified grid.
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    USE MODULE_DM, ONLY: wrf_dm_maxloc_real
    implicit none
    integer, intent(in) :: &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe
    integer, intent(out) :: ierr
    type(domain), intent(inout) :: grid
    integer, intent(in) :: iguess,jguess
    real, intent(inout) :: longuess,latguess
    real :: weight,zjunk
    integer :: itemp,jtemp

    ierr=0
    zjunk=1
    if(iguess>=ips .and. iguess<=ipe .and. jguess>=jps .and. jguess<=jpe) then
       weight=1
       longuess=grid%hlon(iguess,jguess)
       latguess=grid%hlat(iguess,jguess)
       itemp=iguess
       jtemp=jguess
    else
       weight=0
       longuess=-999.9
       latguess=-999.9
       itemp=-99
       jtemp=-99
    endif

#ifdef DM_PARALLEL
    call wrf_dm_maxloc_real(weight,latguess,longuess,zjunk,itemp,jtemp)
#endif

    if(itemp==-99 .and. jtemp==-99) then
       ierr=95
    endif
  end subroutine get_lonlat

  subroutine update_tracker_post_move(grid)
    ! This updates the tracker i/j fix location and square of the
    ! distance to the tracker center after a nest move.
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    type(domain), intent(inout) :: grid
    integer :: ierr, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE

    ! Get the grid bounds:
    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

    ! Get the i/j center location from the fix location:
    ierr=0
    call get_nearest_lonlat(grid,grid%tracker_ifix,grid%tracker_jfix, &
         ierr,grid%tracker_fixlon,grid%tracker_fixlat, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    ! Get the square of the approximate distance to the tracker center
    ! at all points:
    if(ierr==0) &
         call get_tracker_distsq(grid, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
  end subroutine update_tracker_post_move
#endif
end module module_tracker

#if (HWRF == 1)
subroutine nmm_med_tracker_post_move(grid)
  ! This updates the tracker i/j fix location and square of the
  ! distance to the tracker center after a nest move.
  use module_tracker, only: update_tracker_post_move
  use module_domain, only : domain
  type(domain), intent(inout) :: grid
  call update_tracker_post_move(grid)
end subroutine nmm_med_tracker_post_move
#endif
