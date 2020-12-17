      MODULE tuv_subs

      use params_mod, only : dp

      IMPLICIT none

      private
      public :: tuv_radfld, sundis, calc_zenith

      integer :: nstr = 1        ! stream count

      CONTAINS

      SUBROUTINE tuv_radfld( nlambda_start, cld_od_opt, cldfrac, nlyr, nwave, &
                             zenith, z, albedo, &
                             aircol, o2col, o3col, so2col, no2col, &
                             tauaer300, tauaer400, tauaer600, tauaer999, &
                             waer300, waer400, waer600, waer999, &
                             gaer300, gaer400, gaer600, gaer999, &
                             dtaer, omaer, gaer, dtcld, omcld, gcld, &
                             has_aer_ra_feedback, &
                             qll, dobsi, o3_xs, no2_xs, o2_xs, &
                             so2_xs, wmin, wc, tlev, srb_o2_xs, radfld, efld, &
                             e_dir, e_dn, e_up, &
#ifdef SW_DEBUG
                             dir_fld, dwn_fld, up_fld, dt_cld, tuv_diags )
#else
                             dir_fld, dwn_fld, up_fld, dt_cld )
#endif
!-----------------------------------------------------------------------------
!     ... calculate the radiation field
!-----------------------------------------------------------------------------
  
      use srb,       only : la_srb
      use rad_trans, only : rtlink

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in)  :: nlambda_start
      integer, intent(in)  :: nlyr
      integer, intent(in)  :: nwave
      integer, intent(in)  :: cld_od_opt
      real, intent(in)  :: zenith
      real, intent(in)  :: dobsi
      real, intent(in)  :: wmin
      real, intent(in)  :: z(:)
      real, intent(in)  :: albedo(:)
      real, intent(in)  :: aircol(:)
      real, intent(in)  :: o2col(:)
      real, intent(in)  :: o3col(:)
      real, intent(in)  :: so2col(:)
      real, intent(in)  :: no2col(:)
      real, intent(in)  :: tauaer300(:)
      real, intent(in)  :: tauaer400(:)
      real, intent(in)  :: tauaer600(:)
      real, intent(in)  :: tauaer999(:)
      real, intent(in)  :: waer300(:)
      real, intent(in)  :: waer400(:)
      real, intent(in)  :: waer600(:)
      real, intent(in)  :: waer999(:)
      real, intent(in)  :: gaer300(:)
      real, intent(in)  :: gaer400(:)
      real, intent(in)  :: gaer600(:)
      real, intent(in)  :: gaer999(:)
      real, intent(in)  :: qll(:)
      real, intent(in)  :: wc(:)
      real, intent(in)  :: tlev(:)
      real, intent(in)  :: cldfrac(:)
      real, intent(in)  :: o2_xs(:)
      real, intent(in)  :: so2_xs(:)
      real, intent(in)  :: o3_xs(:,:)
      real, intent(in)  :: no2_xs(:,:)
      real, intent(out) :: srb_o2_xs(:,:)
      real, intent(out) :: radfld(:,:)
      real, intent(out) :: efld(:,:)
      real, intent(inout)  :: dir_fld(:,:), dwn_fld(:,:), up_fld(:,:)
      real, intent(inout)  :: e_dir(:,:), e_dn(:,:), e_up(:,:)
      real, intent(inout)  :: dt_cld(:)
      real, intent(inout)  :: dtaer(:,:), omaer(:,:), gaer(:,:)
      real, intent(inout)  :: dtcld(:,:), omcld(:,:), gcld(:,:)
      logical, intent(in)  :: has_aer_ra_feedback
#ifdef SW_DEBUG
      logical, intent(in) :: tuv_diags
#endif

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      integer :: k, n
      integer :: wn
      integer :: n_radlev, n_radlevp1
      integer :: nid(0:nlyr)
      real :: dtrl(nlyr,nwave)
      real :: dto2(nlyr,nwave)
      real :: dto3(nlyr,nwave)
      real :: dtso2(nlyr,nwave)
      real :: dtno2(nlyr,nwave)
!     real :: dtcld(nlyr,nwave)
!     real :: dtaer(nlyr,nwave)
      real :: dtsnw(nlyr,nwave)

!     real :: omcld(nlyr,nwave)
!     real :: gcld(nlyr,nwave)
!     real :: omaer(nlyr,nwave)
!     real :: gaer(nlyr,nwave)
      real :: omsnw(nlyr,nwave)
      real :: gsnw(nlyr,nwave)

      real :: edir(nlyr+1)
      real :: edn(nlyr+1)
      real :: eup(nlyr+1)
      real :: fdir(nlyr+1)
      real :: fdn(nlyr+1)
      real :: fup(nlyr+1)

      real :: vcol(nlyr)
      real :: scol(nlyr)

      real :: dsdh(0:nlyr,nlyr)

      n_radlev = size( radfld,dim=2 )
      n_radlevp1 = n_radlev + 1

      do wn = 1,nwave
        omcld(:,wn) = 0.
        omaer(:,wn) = 0.
        omsnw(:,wn) = 0.
        gcld(:,wn)  = 0.
        gaer(:,wn)  = 0.
        gsnw(:,wn)  = 0.
        dtcld(:,wn) = 0.
        dtaer(:,wn) = 0.
        dtsnw(:,wn) = 0.
      end do

      call odrl( wc, aircol, dtrl )
      call seto2( o2col, o2_xs, dto2 )
      call odo3( o3col, o3_xs, dto3, dobsi )
      call setso2( so2col, so2_xs, dtso2 )
      call setno2( no2col, no2_xs, dtno2 )
!-------------------------------------------------------------
! aerosol optical depths
!-------------------------------------------------------------
      if( has_aer_ra_feedback ) then
        call setaer( nlambda_start, wc, tauaer300, tauaer400, &
                     tauaer600, tauaer999, waer300, &
                     waer400, waer600, waer999,     &
                     gaer300, gaer400, gaer600,     &
                     gaer999, dtaer, omaer, gaer )
      endif
!-------------------------------------------------------------
! cloud optical depths (cloud water units = g/m3)
!-------------------------------------------------------------
      call setcld( nlambda_start, cld_od_opt, z, qll, cldfrac, &
                   dtcld, omcld, gcld )
      dt_cld(:n_radlev) = dtcld(2:n_radlevp1,1)

      call sphers( nlyr, z, zenith, dsdh, nid )

#ifdef SW_DEBUG
      if( tuv_diags ) then
        open(unit=33,file='WRF-TUV.dbg.out')
        write(33,*) 'tuv_radfld: tuv_diags'
        write(33,'(''nlyr = '',i4)') nlyr
        write(33,'(''dsdh(1,1) = '',1p,g15.7)') dsdh(1,1)
        write(33,*) 'dsdh(nlyr,:)'
        do n = 1,nlyr,5
          write(33,'(1p,5g15.7)') dsdh(nlyr,n:min(n+4,nlyr))
        end do
        close(33)
      endif
#endif

      call airmas( nlyr, dsdh, nid, aircol, vcol, scol )
      call la_srb( nlyr, z, tlev, wmin, &
                   vcol, scol, o2_xs, dto2, srb_o2_xs )

      do wn = nlambda_start,nwave
        call rtlink( &
           nstr, nlyr+1, nlyr, nwave, &
           wn, albedo(wn), zenith, &
           dsdh, nid, &
           dtrl,  &
           dto3,  &
           dto2, &
           dtso2, &
           dtno2,  &
           dtcld, omcld, gcld, &
           dtaer, omaer, gaer, &
           dtsnw, omsnw, gsnw, &
#ifdef SW_DEBUG
           edir, edn, eup, fdir, fdn, fup, tuv_diags )
#else
           edir, edn, eup, fdir, fdn, fup )
#endif
!       radfld(wn,1:nlyr-1)  = fdir(2:nlyr) + fdn(2:nlyr) + fup(2:nlyr)
!       efld(1:nlyr-1,wn)    = edir(2:nlyr) + edn(2:nlyr) + eup(2:nlyr)
!       dir_fld(1:nlyr-1,wn) = fdir(2:nlyr)
!       dwn_fld(1:nlyr-1,wn) = fdn(2:nlyr)
!       up_fld(1:nlyr-1,wn)  = fup(2:nlyr)
!       e_dir(1:nlyr-1,wn)   = edir(2:nlyr)
!       e_dn(1:nlyr-1,wn)    = edn(2:nlyr)
!       e_up(1:nlyr-1,wn)    = eup(2:nlyr)
        radfld(wn,1:n_radlev) = fdir(2:n_radlevp1) + fdn(2:n_radlevp1) + fup(2:n_radlevp1)
        efld(1:n_radlev,wn)    = edir(2:n_radlevp1) + edn(2:n_radlevp1) + eup(2:n_radlevp1)
        dir_fld(1:n_radlev,wn) = fdir(2:n_radlevp1)
        dwn_fld(1:n_radlev,wn) = fdn(2:n_radlevp1)
        up_fld(1:n_radlev,wn)  = fup(2:n_radlevp1)
        e_dir(1:n_radlev,wn)   = edir(2:n_radlevp1)
        e_dn(1:n_radlev,wn)    = edn(2:n_radlevp1)
        e_up(1:n_radlev,wn)    = eup(2:n_radlevp1)
      end do

      END SUBROUTINE tuv_radfld

      SUBROUTINE odrl( wc, aircol, dtrl )
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Compute Rayleigh optical depths as a function of altitude and wavelength =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  C       - REAL, number of air molecules per cm^2 at each specified    (O)=*
!=            altitude layer                                                 =*
!=  DTRL    - REAL, Rayleigh optical depth at each specified altitude     (O)=*
!=            and each specified wavelength                                  =*
!-----------------------------------------------------------------------------*

!-----------------------------------------------------------------------------*
!     ...dummy arguments
!-----------------------------------------------------------------------------*
      REAL,    intent(in)  :: aircol(:)
      REAL,    intent(in)  :: wc(:)
      REAL,    intent(out) :: dtrl(:,:)

!-----------------------------------------------------------------------------*
!     ...local variables
!-----------------------------------------------------------------------------*
      INTEGER :: nwave, nlyr
      INTEGER :: wn
      REAL    :: srayl, wmicrn, xx 
      
      nwave = size( wc )
      nlyr  = size( aircol )
!-----------------------------------------------------------------------------*
! compute Rayleigh cross sections and depths:
!-----------------------------------------------------------------------------*
      DO wn = 1,nwave
!-----------------------------------------------------------------------------*
! Rayleigh scattering cross section from WMO 1985 (originally from
! Nicolet, M., On the molecular scattering in the terrestrial atmosphere:
! An empirical formula for its calculation in the homoshpere, Planet.
! Space Sci., 32, 1467-1468, 1984.
!-----------------------------------------------------------------------------*
        wmicrn =  wc(wn)*1.E-3
        IF( wmicrn <= 0.55 ) THEN
          xx = 3.6772 + 0.389*wmicrn + 0.09426/wmicrn
        ELSE
          xx = 4.04
        ENDIF
        srayl = 4.02e-28/(wmicrn)**xx
!-----------------------------------------------------------------------------*
! alternate (older) expression from
! Frohlich and Shaw, Appl.Opt. v.11, p.1773 (1980).
!-----------------------------------------------------------------------------*
        dtrl(:nlyr,wn) = aircol(:nlyr)*srayl
      END DO

      END SUBROUTINE odrl

      SUBROUTINE odo3( o3col, o3xs, dto3, dobsi )
!-----------------------------------------------------------------------------
!=  NAME:  Optical Depths of O3
!=  PURPOSE:
!=  Compute ozone optical depths as a function of altitude and wavelength
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  O3XS   - REAL, molecular absoprtion cross section (cm^2) of O3 at     (I)
!=           each specified wavelength and altitude
!=  C      - REAL, ozone vertical column increments, molec cm-2, for each (I)
!=           layer
!=  DTO3   - REAL, optical depth due to ozone absorption at each          (O)
!=           specified altitude at each specified wavelength
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      REAL, intent(in)    :: dobsi
      REAL, intent(in)    :: o3col(:)
      REAL, intent(in)    :: o3xs(:,:)
      REAL, intent(inout) :: dto3(:,:)

      INTEGER :: nlyr, nwave
      INTEGER :: wn
      REAL    :: dob_at_grnd, scale_fac

      nwave = size(o3xs,dim=1)
      nlyr  = size(o3col)

      if( dobsi == 0. ) then
!-----------------------------------------------------------------------------
!  no scaling
!-----------------------------------------------------------------------------
        DO wn = 1,nwave
          dto3(:nlyr,wn) = o3col(:nlyr) * o3xs(wn,:nlyr)
        END DO
      else
!-----------------------------------------------------------------------------
!  scale model o3 column to dobsi
!-----------------------------------------------------------------------------
        dob_at_grnd = sum( o3col(:nlyr) )/2.687e16
        scale_fac   = dobsi/dob_at_grnd
        DO wn = 1,nwave
          dto3(:nlyr,wn) = scale_fac * o3col(:nlyr) * o3xs(wn,:nlyr)
        END DO
      endif

      END SUBROUTINE odo3

      SUBROUTINE seto2( o2col, o2xs1, dto2 )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Set up an altitude profile of air molecules.  Subroutine includes a
!=  shape-conserving scaling method that allows scaling of the entire
!=  profile to a given sea-level pressure.
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  CZ      - REAL, number of air molecules per cm^2 at each specified    (O)
!=            altitude layer
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      REAL, intent(in)  :: o2col(:)
      REAL, intent(in)  :: o2xs1(:)
      REAL, intent(out) :: dto2(:,:)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      INTEGER :: nlyr, nwave
      INTEGER :: wn

      nwave = size(o2xs1)
      nlyr  = size(o2col)

!-----------------------------------------------------------------------------
!  Assumes that O2 = 20.95 % of air density.  If desire different O2 
!  profile (e.g. for upper atmosphere) then can load it here.
!-----------------------------------------------------------------------------
      DO wn = 1,nwave
        dto2(:nlyr,wn) = o2col(:nlyr) * o2xs1(wn)
      ENDDO

      END SUBROUTINE seto2

      SUBROUTINE setso2( colso2, so2_xs, dtso2 )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Set up an altitude profile of SO2 molecules, and corresponding absorption
!=  optical depths.  Subroutine includes a shape-conserving scaling method
!=  that allows scaling of the entire profile to a given overhead SO2
!=  column amount.
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  SO2_XS - REAL, molecular absoprtion cross section (cm^2) of O2 at     (I)
!=           each specified wavelength
!=  DTSO2  - REAL, optical depth due to SO2 absorption at each            (O)
!=           specified altitude at each specified wavelength
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      REAL,    intent(in)  :: colso2(:)
      REAL,    intent(in)  :: so2_xs(:)
      REAL,    intent(out) :: dtso2(:,:)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      integer :: nwave, nlyr
      integer :: wn

      nwave = size( so2_xs )
      nlyr  = size( colso2 )

      DO wn = 1,nwave
        dtso2(:nlyr,wn) = colso2(:nlyr)*so2_xs(wn)
      END DO

      END SUBROUTINE setso2

      SUBROUTINE setno2( colno2, no2_xs, dtno2 )
!-----------------------------------------------------------------------------
!=  NAME:  Optical Depths of no2
!=  PURPOSE:
!=  Compute no2 optical depths as a function of altitude and wavelength
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  NO2_XS - REAL, molecular absoprtion cross section (cm^2) of no2 at    (I)
!=           each specified wavelength and altitude
!=  COLNO2 - REAL, no2 vertical column increments, molec cm-2, for each   (I)
!=           layer
!=  DTNO2  - REAL, optical depth due to no2 absorption at each            (O)
!=           specified altitude at each specified wavelength
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      REAL, intent(in)    :: colno2(:)
      REAL, intent(in)    :: no2_xs(:,:)
      REAL, intent(inout) :: dtno2(:,:)

      INTEGER :: nlyr, nwave
      INTEGER :: wn

      nwave = size(no2_xs,dim=1)
      nlyr  = size(colno2)

      DO wn = 1,nwave
        dtno2(:nlyr,wn) = colno2(:nlyr) * no2_xs(wn,:nlyr)
      END DO

      END SUBROUTINE setno2

      subroutine setaer( nlambda_start, wc, tauaer300, tauaer400, &
                         tauaer600, tauaer999,               &
                         waer300, waer400, waer600, waer999, &
                         gaer300, gaer400, gaer600, gaer999, &
                         dtaer, omaer, gaer )
!----------------------------------------------------------------------
! The routine is based on aerosol treatment in module_ra_rrtmg_sw.F
! INPUT: 
! nzlev: number of specified altitude levels in the working grid
! z: specified altitude working grid   
! Aerosol optical properties at 300, 400, 600 and 999 nm. 
!   tauaer300, tauaer400, tauaer600, tauaer999: Layer AODs
!   waer300, waer400, waer600, waer999: Layer SSAs
!   gaer300, gaer400, gaer600, gaer999: Layer asymmetry parameters

! OUTPUT:
! dtaer: Layer AOD at FTUV wavelengths
! omaer: Layer SSA at FTUV wavelengths
! gaer : Layer asymmetry parameters at FTUV wavelengths
!------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nlambda_start
      real, intent(in)  :: wc(:)
      real, intent(in)  :: tauaer300(:), tauaer400(:),    &
                           tauaer600(:), tauaer999(:)
      real, intent(in)  :: waer300(:), waer400(:),        &
                           waer600(:), waer999(:)
      real, intent(in)  :: gaer300(:), gaer400(:),        &
                           gaer600(:), gaer999(:)
      real, intent(out) :: dtaer(:,:), omaer(:,:), gaer(:,:)

!-----------------------------------------------------------------------------
! Local Variables
!-----------------------------------------------------------------------------
      real, parameter :: thresh = 1.e-9
      integer     :: k, wn, nlyr, nwave
      real        :: ang, slope, wfac

      nlyr =  size(dtaer,dim=1)
      nwave = size(dtaer,dim=2)

wave_loop: &
      do wn = nlambda_start,nwave
        wfac = wc(wn)*1.e-3 - .6
        do k = 1,nlyr-1
!-----------------------------------------------------------------------------
! use angstrom exponent to calculate aerosol optical depth; wc is in nm.  
!-----------------------------------------------------------------------------
          if( tauaer300(k) > thresh .and. tauaer999(k) > thresh ) then
            ang = log(tauaer300(k)/tauaer999(k))/log(0.999/0.3)
            dtaer(k,wn) = tauaer400(k)*(0.4/(wc(wn)*1.e-3))**ang
!-----------------------------------------------------------------------------
! ssa - use linear interpolation/extrapolation
!-----------------------------------------------------------------------------
            slope = 5.*(waer600(k) - waer400(k))
            omaer(k,wn) = slope*wfac + waer600(k)
            omaer(k,wn) = max( .4,min( 1.,omaer(k,wn) ) )
!-----------------------------------------------------------------------------
! asymmetry parameter - use linear interpolation/extrapolation
!-----------------------------------------------------------------------------
            slope = 5.*(gaer600(k) - gaer400(k))
            gaer(k,wn) = slope*wfac + gaer600(k)
            gaer(k,wn) = max( .5,min( 1.,gaer(k,wn) ) )
          endif
        end do
      end do wave_loop

      end subroutine setaer

      subroutine setcld( nlambda_start, cld_od_opt, z, xlwc, cldfrac, &
                         dtcld, omcld, gcld )
!-----------------------------------------------------------------------------
!= PURPOSE:
!= Set up cloud optical depth, single albedo and g
!-----------------------------------------------------------------------------
!= PARAMETERS:
!= PARAMETERS:
!= NZ - INTEGER, number of specified altitude levels in the working (I)
!= grid
!= Z - real(dp), specified altitude working grid (km) (I)
!= XLWC Cloud water content g/M3 (I)
!=
!= dtcld - cloud optical depth
!= omcld - cloud droplet single albedo
!= gcld  - g
!-----------------------------------------------------------------------------
!
! VERTICAL DOMAIN is from bottom(1) to TOP (TOP=nz)
! CCM from top(1) to bottom(nz)
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! ... Dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nlambda_start
      integer, intent(in) :: cld_od_opt
      real, intent(in)  :: z(:)
      real, intent(in)  :: xlwc(:)
      real, intent(in)  :: cldfrac(:)
      real, intent(inout) :: dtcld(:,:)
      real, intent(inout) :: omcld(:,:)
      real, intent(inout) :: gcld(:,:)
!-----------------------------------------------------------------------------
! ... Local variables
!-----------------------------------------------------------------------------
      real, parameter :: km2m = 1.e3        ! kilometer to meter
      real, parameter :: wden = 1.e6        ! g/m3 (1 m3 water = 1e6 g water)
      real, parameter :: re = 10.0 * 1.e-6  ! assuming cloud drop radius = 10 um to M
      real, parameter :: fac = 1./(wden*re)

      integer  :: astat
      integer  :: wn
      integer  :: nlyr, nwave
      real, allocatable :: wrk(:), layer_cldfrac(:)

      nlyr  = size(dtcld,dim=1)
      nwave = size(dtcld,dim=2)

      allocate( wrk(nlyr),layer_cldfrac(nlyr),stat=astat )
      if( astat /= 0 ) then
        call wrf_error_fatal( 'setcld: failed to allocate wrk' )
      endif

!-----------------------------------------------------------------------------
! ... calculate optical depth
!-----------------------------------------------------------------------------
      wrk(1:nlyr-1) = (z(2:nlyr) - z(1:nlyr-1))*km2m   !  (km -> m)
      wrk(1:nlyr-1) = 1.5 * .5*(xlwc(1:nlyr-1) + xlwc(2:nlyr))*wrk(1:nlyr-1)*fac
      wrk(1:nlyr-1) = max( wrk(1:nlyr-1),0. )
      if( cld_od_opt == 2 ) then
        layer_cldfrac(1:nlyr-1) = .5*(cldfrac(1:nlyr-1) + cldfrac(2:nlyr))
        wrk(1:nlyr-1) = wrk(1:nlyr-1)*layer_cldfrac(1:nlyr-1)*sqrt( layer_cldfrac(1:nlyr-1) )
      endif
!----------------------------------------------------
! ....calculate cloud optical depth T
! following Liao et al. JGR, 104, 23697, 1999
!----------------------------------------------------
      if( any( wrk(1:nlyr-1) > 0. ) ) then
        do wn = nlambda_start,nwave
          dtcld(1:nlyr-1,wn) = wrk(1:nlyr-1)
          omcld(1:nlyr-1,wn) = .9999
          gcld (1:nlyr-1,wn) = .85
        end do
      endif

      if( allocated( wrk ) ) then
        deallocate( wrk )
      endif
      if( allocated( layer_cldfrac ) ) then
        deallocate( layer_cldfrac )
      endif

      end subroutine setcld

      REAL FUNCTION sundis( julday )
!-----------------------------------------------------------------------------
! purpose:
! calculate earth-sun distance variation for a given date. based on
! fourier coefficients originally from: spencer, j.w., 1971, fourier
! series representation of the position of the sun, search, 2:172
!-----------------------------------------------------------------------------
! parameters:
! idate - integer, specification of the date, from yymmdd (i)
! esrm2 - real(dp), variation of the earth-sun distance (o)
! esrm2 = (average e/s dist)^2 / (e/s dist on day idate)^2
!-----------------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: julday

!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      real(dp), parameter :: pi = 3.1415926_dp

      real(dp) :: dayn, thet0
      real(dp) :: sinth, costh, sin2th, cos2th

!-----------------------------------------------------------------------------
! ... parse date to find day number (julian day)
!-----------------------------------------------------------------------------
      dayn = real(julday - 1,kind=dp) + .5_dp
!-----------------------------------------------------------------------------
! ... define angular day number and compute esrm2:
!-----------------------------------------------------------------------------
      thet0 = 2._dp*pi*dayn/365._dp
!-----------------------------------------------------------------------------
! ... calculate sin(2*thet0), cos(2*thet0) from
! addition theorems for trig functions for better
! performance; the computation of sin2th, cos2th
! is about 5-6 times faster than the evaluation
! of the intrinsic functions sin and cos
!-----------------------------------------------------------------------------
      sinth  = sin( thet0 )
      costh  = cos( thet0 )
      sin2th = 2._dp*sinth*costh
      cos2th = costh*costh - sinth*sinth
      sundis  = real( 1.000110_dp + .034221_dp*costh + .001280_dp*sinth &
                                  + .000719_dp*cos2th + .000077_dp*sin2th )

      END FUNCTION sundis

      subroutine calc_zenith( lat, long, julday, gmt, zenith, &
                              its, ite, jts, jte, &
                              ims, ime, jms, jme )
!-------------------------------------------------------------------
! this subroutine calculates solar zenith and azimuth angles for a
! part  time and location.  must specify:
! input:
! lat - latitude in decimal degrees
! long - longitude in decimal degrees
! gmt  - greenwich mean time - decimal military eg.
! 22.75 = 45 min after ten pm gmt
! output:
! zenith
! azimuth
! .. Scalar Arguments ..
!-------------------------------------------------------------------
        integer, intent(in)  :: julday
        integer, intent(in)  :: its,ite
        integer, intent(in)  :: jts,jte
        integer, intent(in)  :: ims,ime
        integer, intent(in)  :: jms,jme
        real(dp), intent(in) :: gmt
        real, intent(in)     :: lat(ims:ime,jms:jme)
        real, intent(in)     :: long(ims:ime,jms:jme)
        real, intent(out)    :: zenith(ims:ime,jms:jme)

!-------------------------------------------------------------------
! .. Local variables
!-------------------------------------------------------------------
      real(dp), parameter :: d2r = 3.1415926_dp/180.0_dp
      real(dp), parameter :: r2d = 1.0_dp/d2r

      integer  :: i, j
      real(dp) :: caz, csz, cw, d, ec, epsi, eqt, eyt, feqt, feqt1, &
          feqt2, feqt3, feqt4, feqt5, feqt6, feqt7, lbgmt, lzgmt, ml, pepsi, &
          pi, ra, raz, rdecl, reqt, rlt, rml, rra, ssw, sw, tab, w, wr, &
          yt, zpt, zr

      d = real(julday,dp) + gmt/24.0_dp
!-------------------------------------------------------------------
! calc geom mean longitude
!-------------------------------------------------------------------
      ml  = 279.2801988_dp + d*(.9856473354_dp + 2.267E-13_dp*d)
      rml = ml*d2r
!-------------------------------------------------------------------
! calc equation of time in sec
! w = mean long of perigee
! e = eccentricity
! epsi = mean obliquity of ecliptic
!-------------------------------------------------------------------
      w = 282.4932328_dp + d*(4.70684E-5_dp + 3.39E-13_dp*d)
      wr = w*d2r
      ec   = 1.6720041E-2_dp - d*(1.1444E-9_dp + 9.4E-17_dp*d)
      epsi = 23.44266511_dp - d*(3.5626E-7_dp + 1.23E-15_dp*d)
      pepsi = epsi*d2r
      yt = (tan(pepsi/2.0_dp))**2
      cw = cos(wr)
      sw = sin(wr)
      ssw = sin(2.0_dp*wr)
      eyt = 2._dp*ec*yt
      feqt1 = -sin(rml)*cw*(eyt + 2._dp*ec)
      feqt2 = cos(rml)*sw*(2._dp*ec - eyt)
      feqt3 = sin(2._dp*rml)*(yt - (5._dp*ec**2/4._dp)*(cw**2 - sw**2))
      feqt4 = cos(2._dp*rml)*(5._dp*ec**2*ssw/4._dp)
      feqt5 = sin(3._dp*rml)*(eyt*cw)
      feqt6 = -cos(3._dp*rml)*(eyt*sw)
      feqt7 = -sin(4._dp*rml)*(.5_dp*yt**2)
      feqt  = feqt1 + feqt2 + feqt3 + feqt4 + feqt5 + feqt6 + feqt7
      eqt   = feqt*13751.0_dp

!-------------------------------------------------------------------
! convert eq of time from sec to deg
!-------------------------------------------------------------------
      reqt = eqt/240._dp
!-------------------------------------------------------------------
! calc right ascension in rads
!-------------------------------------------------------------------
      ra  = ml - reqt
      rra = ra*d2r
!-------------------------------------------------------------------
! calc declination in rads, deg
!-------------------------------------------------------------------
      tab = 0.43360_dp*sin(rra)
      rdecl = atan(tab)
      do j = jts,jte
        do i = its,ite
!-------------------------------------------------------------------
! calc local hour angle
!-------------------------------------------------------------------
          lbgmt = 12.0_dp - eqt/3600._dp + real(long(i,j),dp)*24._dp/360._dp
          lzgmt = 15.0_dp*(gmt - lbgmt)
          zpt   = lzgmt*d2r
          rlt   = real(lat(i,j),dp)*d2r
          csz   = sin(rlt)*sin(rdecl) + cos(rlt)*cos(rdecl)*cos(zpt)
          csz   = min( 1._dp,csz )
          zr    = acos(csz)
          zenith(i,j) = real( zr/d2r,4 )
        end do
      end do

      end subroutine calc_zenith

      SUBROUTINE sphers( nlyr, z, zen, dsdh, nid )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Calculate slant path over vertical depth ds/dh in spherical geometry.
!=  Calculation is based on:  A.Dahlback, and K.Stamnes, A new spheric model
!=  for computing the radiation field available for photolysis and heating
!=  at twilight, Planet.Space Sci., v39, n5, pp. 671-683, 1991 (Appendix B)
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  NZ      - INTEGER, number of specified altitude levels in the working (I)
!=            grid
!=  Z       - REAL, specified altitude working grid (km)                  (I)
!=  ZEN     - REAL, solar zenith angle (degrees)                          (I)
!=  DSDH    - REAL, slant path of direct beam through each layer crossed  (O)
!=            when travelling from the top of the atmosphere to layer i;
!=            DSDH(i,j), i = 0..NZ-1, j = 1..NZ-1
!=  NID     - INTEGER, number of layers crossed by the direct beam when   (O)
!=            travelling from the top of the atmosphere to layer i;
!=            NID(i), i = 0..NZ-1
!-----------------------------------------------------------------------------
!  This program is free software;  you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by the
!= Free Software Foundation;  either version 2 of the license, or (at your
!= option) any later version.
!= The TUV package is distributed in the hope that it will be useful, but
!= WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTIBI-
!= LITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
!= License for more details.
!= To obtain a copy of the GNU General Public License, write to:
!= Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!-----------------------------------------------------------------------------
!= To contact the authors, please mail to:
!= Sasha Madronich, NCAR/ACD, P.O.Box 3000, Boulder, CO, 80307-3000, USA
!= send email to:  sasha@ucar.edu
!-----------------------------------------------------------------------------

      use params_mod, only : pi, radius

      IMPLICIT NONE

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      INTEGER, intent(in) :: nlyr
      REAL,    intent(in) :: zen
      REAL,    intent(in) :: z(:)

      INTEGER, intent(inout) :: nid(0:nlyr)
      REAL,    intent(inout) :: dsdh(0:nlyr,nlyr)


!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      REAL, parameter ::  dr = pi/180.

      INTEGER :: j, jm1, k
      INTEGER :: id
      REAL    :: re
      REAL    :: zd(0:nlyr)
      REAL    :: ds_dh(1:nlyr)
      REAL(8) :: zenrad, rpsinz, rj, rjp1, dsj, dhj, ga, gb, sm

      zenrad = REAL( zen*dr,8 )
!-----------------------------------------------------------------------------
! include the elevation above sea level to the radius of the earth:
!-----------------------------------------------------------------------------
      re = radius + z(1)

!-----------------------------------------------------------------------------
! from bottom-up to top-down
! note zd is the elevation above earth surface:
!-----------------------------------------------------------------------------
      zd(0:nlyr) = z(nlyr+1:1:-1) - z(1)

!-----------------------------------------------------------------------------
! initialize nid
!-----------------------------------------------------------------------------
      nid(0:nlyr) = 0

!-----------------------------------------------------------------------------
! calculate ds/dh of every layer
!-----------------------------------------------------------------------------
layer_loop : &
      DO k = 0, nlyr
        ds_dh(:) = 0.
        rpsinz = real(re + zd(k),8) * SIN(zenrad)
!       IF( zen > 90.0 .AND. rpsinz < real(re,8) ) THEN
        IF( zen <= 90.0 .or. rpsinz >= real(re,8) ) THEN
!-----------------------------------------------------------------------------
! Find index of layer in which the screening height lies
!-----------------------------------------------------------------------------
          id = k 
          IF( zen > 90.0 ) THEN
            DO j = 1,nlyr
              IF( rpsinz < real(zd(j-1) + re,8) .AND. &
                  rpsinz >= real(zd(j) + re,8) ) then
                id = j
              ENDIF
            END DO
          END IF
 
          DO j = 1, id
            jm1 = j - 1
!           IF( j == id .AND. id == k .AND. zen > 90.0 ) then
            IF( j /= id .or. k /= id .or. zen <= 90.0 ) then
              sm = 1.0_8
            ELSE
              sm = -1.0_8
            ENDIF
            rj   = real(re + zd(jm1),8)
            rjp1 = real(re + zd(j),8)
            dhj  = zd(jm1) - zd(j)
 
            ga = max( rj*rj - rpsinz*rpsinz,0.0_8 )
            gb = max( rjp1*rjp1 - rpsinz*rpsinz,0.0_8 )

            IF( id > k .AND. j == id ) THEN
              dsj = SQRT( ga )
            ELSE
              dsj = SQRT( ga ) - sm*SQRT( gb )
            END IF
            ds_dh(j) = real( dsj/dhj,4 )
          END DO
          nid(k) = id
        ELSE
          nid(k) = -1
        ENDIF
        dsdh(k,:) = ds_dh(:)

      END DO layer_loop

      END SUBROUTINE sphers

      SUBROUTINE airmas( nlyr, dsdh, nid, cz, vcol, scol )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Calculate vertical and slant air columns, in spherical geometry, as a
!=  function of altitude.
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  NZ      - INTEGER, number of specified altitude levels in the working (I)
!=            grid
!=  DSDH    - REAL, slant path of direct beam through each layer crossed  (O)
!=            when travelling from the top of the atmosphere to layer i;
!=            DSDH(i,j), i = 0..NZ-1, j = 1..NZ-1
!=  NID     - INTEGER, number of layers crossed by the direct beam when   (O)
!=            travelling from the top of the atmosphere to layer i;
!=            NID(i), i = 0..NZ-1
!=  VCOL    - REAL, output, vertical air column, molec cm-2, above level iz
!=  SCOL    - REAL, output, slant air column in direction of sun, above iz
!=            also in molec cm-2
!-----------------------------------------------------------------------------

      use params_mod, only : largest

      IMPLICIT NONE

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      INTEGER, intent(in) :: nlyr
      INTEGER, intent(in) :: nid(0:nlyr)
      REAL,    intent(in) :: dsdh(0:nlyr,nlyr)
      REAL,    intent(in) :: cz(nlyr)

      REAL, intent(inout) :: vcol(nlyr), scol(nlyr)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      INTEGER :: lyr, j, nlev, nlevi
      REAL    :: sum, vsum

!-----------------------------------------------------------------------------
! calculate vertical and slant column from each level: work downward
!-----------------------------------------------------------------------------
      nlev = nlyr + 1
      vsum = 0.
      DO lyr = 1, nlyr
        nlevi = nlev - lyr
        vsum = vsum + cz(nlevi)
        vcol(nlevi) = vsum
        sum = 0.
        IF( nid(lyr) < 0 ) THEN
          sum = largest
        ELSE
!-----------------------------------------------------------------------------
! single pass layers:
!-----------------------------------------------------------------------------
          DO j = 1, MIN(nid(lyr), lyr)
            sum = sum + cz(nlev-j)*dsdh(lyr,j)
          END DO
!-----------------------------------------------------------------------------
! double pass layers:
!-----------------------------------------------------------------------------
           DO j = MIN(nid(lyr),lyr)+1, nid(lyr)
             sum = sum + 2.*cz(nlev-j)*dsdh(lyr,j)
           END DO
        ENDIF
        scol(nlevi) = sum
      END DO

      END SUBROUTINE airmas

      END MODULE tuv_subs
