!
! NESDIS_LandEM_Module
!
! Module containing the NESDIS microwave land emissivity model
!
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, QSS Group Inc., 01-Jun-2005
!                       Banghua.Yan@noaa.gov
!                       Fuzhong Weng, NOAA/NESDIS/ORA,
!                       Fuzhong.Weng@noaa.gov
!

MODULE NESDIS_LandEM_Module
 
  ! -----------------
  ! Enviroment set up
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE Fundamental_Constants, ONLY: C_2
  USE NESDIS_SnowEM_Parameters
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Procedures
  PUBLIC  :: NESDIS_LandEM
  ! Parameters
  PUBLIC :: ZERO
  PUBLIC :: POINT1
  PUBLIC :: POINT5
  PUBLIC :: ONE
  PUBLIC :: TWO
  PUBLIC :: THREE
  PUBLIC :: FOUR
  PUBLIC :: PI
  PUBLIC :: EMISSH_DEFAULT
  PUBLIC :: EMISSV_DEFAULT
  
  PUBLIC :: ONE_TENTH
  PUBLIC :: HALF
  
  ! -----------------
  ! Module parameters
  ! -----------------
  REAL(fp), PARAMETER :: ZERO   = 0.0_fp
  REAL(fp), PARAMETER :: POINT1 = 0.1_fp
  REAL(fp), PARAMETER :: POINT5 = 0.5_fp
  REAL(fp), PARAMETER :: ONE    = 1.0_fp
  REAL(fp), PARAMETER :: TWO    = 2.0_fp
  REAL(fp), PARAMETER :: THREE  = 3.0_fp
  REAL(fp), PARAMETER :: FOUR   = 4.0_fp
  REAL(fp), PARAMETER :: PI     = 3.141592653589793238462643_fp
  REAL(fp), PARAMETER :: TWOPI  = TWO*PI
  REAL(fp), PARAMETER :: EMISSH_DEFAULT = 0.25_fp
  REAL(fp), PARAMETER :: EMISSV_DEFAULT = 0.30_fp

  REAL(fp), PARAMETER :: ONE_TENTH = POINT1
  REAL(fp), PARAMETER :: HALF      = POINT5
  
  
CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       NESDIS_LandEM
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over land conditions.
!
! REFERENCES:
!       Weng, F., B. Yan, and N. Grody, 2001: "A microwave land emissivity model",
!         J. Geophys. Res., 106, 20, 115-20, 123
!
! CALLING SEQUENCE:
!       CALL NESDIS_LandEM(Angle,                 &   ! Input
!                          Frequency,             &   ! Input
!                          Soil_Moisture_Content, &   ! Input
!                          Vegetation_Fraction,   &   ! Input
!                          Soil_Temperature,      &   ! Input
!                          Land_Temperature,      &   ! Input
!                          Snow_Depth,            &   ! Input
!                          Emissivity_H,          &   ! Output
!                          Emissivity_V)              ! Output
!
! INPUT ARGUMENTS:
!         Angle:                   The angle values in degree.
!                                  UNITS:      Degrees
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Rank-1, (I)
!
!         Frequency                Frequency User defines
!                                  This is the "I" dimension
!                                  UNITS:      GHz
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!
!         Soil_Moisture_Content:   The volumetric water content of the soil (0:1).
!                                  UNITS:      N/A
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!
!         Vegetation_Fraction:     The vegetation fraction of the surface (0:1).
!                                  UNITS:      N/A
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!
!         Soil_Temperature:        The soil temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!
!         Land_Temperature:        The land surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!
!         Snow_Depth:              The snow depth.
!                                  UNITS:      mm
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!
! OUTPUT ARGUMENTS:
!         Emissivity_H:            The surface emissivity at a horizontal
!                                  polarization.
!                                  UNITS:      N/A
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!
!         Emissivity_V:            The surface emissivity at a vertical polarization.
!                                  UNITS:      N/A
!                                  TYPE:       REAL(fp)
!                                  DIMENSION:  Scalar
!
!
! INTERNAL ARGUMENTS:
!       theta       -  local zenith angle in radian
!       rhob        -  bulk volume density of the soil (1.18-1.12)
!       rhos        -  density of the solids (2.65 g.cm^3 for solid soil material)
!       sand        -  sand fraction (sand + clay = 1.0)
!       clay        -  clay fraction
!       lai         -  leaf area index (eg. lai = 4.0 for corn leaves)
!       sigma       -  surface roughness formed between medium 1 and 2,
!                      expressed as the standard deviation of roughtness height (mm)
!       leaf_thick  --  leaf thickness (mm)
!       rad         -  radius of dense medium scatterers (mm)
!       va          -  fraction volume of dense medium scatterers(0.0 - 1.0)
!       ep          -  dielectric constant of ice or sand particles, complex value
!                               (e.g, 3.0+i0.0)
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, QSS Group Inc., 16-May-2005
!                       Banghua.Yan@noaa.gov
!                       Fuzhong Weng, NOAA/NESDIS/ORA,
!                       Fuzhong.Weng@noaa.gov
!
!------------------------------------------------------------------------------------------------------------

  SUBROUTINE NESDIS_LandEM(Angle,                 &   ! Input
                           Frequency,             &   ! Input
                           Soil_Moisture_Content, &   ! Input
                           Vegetation_Fraction,   &   ! Input
                           Soil_Temperature,      &   ! Input
                           t_skin,                &   ! Input
                           Lai,                   &   ! Input
                           Soil_Type,             &   ! Input
                           Vegetation_Type,       &   ! Input
                           Snow_Depth,            &   ! Input
                           Emissivity_H,          &   ! Output
                           Emissivity_V)              ! Output
    ! Arguments
    REAL(fp), intent(in) :: Angle
    REAL(fp), intent(in) :: Frequency
    REAL(fp), intent(in) :: Soil_Moisture_Content
    REAL(fp), intent(in) :: Vegetation_Fraction
    REAL(fp), intent(in) :: Soil_Temperature
    REAL(fp), intent(in) :: t_skin
    REAL(fp), intent(in) :: Lai
    INTEGER,  intent(in) :: Soil_Type
    INTEGER,  intent(in) :: Vegetation_Type
    REAL(fp), intent(in) :: Snow_Depth
    REAL(fp), intent(out):: Emissivity_V,Emissivity_H
    ! Local parameters
    REAL(fp), PARAMETER :: snow_depth_c     = 10.0_fp
    REAL(fp), PARAMETER :: tsoilc_undersnow = 280.0_fp
    REAL(fp), PARAMETER :: rhos = 2.65_fp
    REAL(fp)            :: sand, clay, rhob
    REAL(fp), PARAMETER, dimension(0:9) :: frac_sand = (/ 0.80_fp,     &
                          0.92_fp, 0.10_fp, 0.20_fp, 0.51_fp, 0.50_fp, &
                          0.35_fp, 0.60_fp, 0.42_fp,  0.92_fp  /)
    REAL(fp), PARAMETER, dimension(0:9) :: frac_clay = (/ 0.20_fp,     &
                          0.06_fp, 0.34_fp, 0.63_fp, 0.14_fp, 0.43_fp, &
                          0.34_fp, 0.28_fp, 0.085_fp, 0.06_fp /)
    REAL(fp), PARAMETER, dimension(0:9) :: rhob_soil = (/ 1.48_fp,     &
                          1.68_fp, 1.27_fp, 1.21_fp, 1.48_fp, 1.31_fp, &
                          1.32_fp, 1.40_fp, 1.54_fp, 1.68_fp /)
! Specific Density
    REAL(fp), PARAMETER, dimension(0:13) :: veg_rho  = (/ 0.33_fp,     &
                          0.40_fp, 0.40_fp, 0.40_fp, 0.40_fp, 0.40_fp, &
                          0.25_fp, 0.25_fp, 0.40_fp, 0.40_fp, 0.40_fp, &
                          0.40_fp, 0.33_fp, 0.33_fp            /)
! MGE
    REAL(fp), PARAMETER, dimension(0:13) :: veg_mge  = (/ 0.50_fp,     &
                          0.45_fp, 0.45_fp, 0.45_fp, 0.40_fp, 0.40_fp, &
                          0.30_fp, 0.35_fp, 0.30_fp, 0.30_fp, 0.40_fp, &
                          0.30_fp, 0.50_fp, 0.40_fp            /)
! LAI
    REAL(fp), PARAMETER, dimension(0:13) :: lai_min  = (/ 0.52_fp,     &
                          3.08_fp, 1.85_fp, 2.80_fp, 5.00_fp, 1.00_fp, &
                          0.50_fp, 0.52_fp, 0.60_fp, 0.50_fp, 0.60_fp, &
                          0.10_fp, 1.56_fp, 0.01_fp            /)
    REAL(fp), PARAMETER, dimension(0:13) :: lai_max  = (/ 2.90_fp,     &
                          6.48_fp, 3.31_fp, 5.50_fp, 6.40_fp, 5.16_fp, &
                          3.66_fp, 2.90_fp, 2.60_fp, 3.66_fp, 2.60_fp, &
                          0.75_fp, 5.68_fp, 0.01_fp            /)
! Leaf_thickness
    REAL(fp), PARAMETER, dimension(0:13) :: leaf_th  = (/ 0.07_fp,     &
                          0.18_fp, 0.18_fp, 0.18_fp, 0.18_fp, 0.18_fp, &
                          0.12_fp, 0.12_fp, 0.12_fp, 0.12_fp, 0.12_fp, &
                          0.12_fp, 0.15_fp, 0.12_fp            /)
    ! Local variables
    REAL(fp) :: mv,veg_frac,theta,theta_i,theta_t,mu,r21_h,r21_v,r23_h,r23_v,  &
                t21_v,t21_h,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,mge, &
                    leaf_thick,rad,sigma,va,ep_real,ep_imag
    REAL(fp) :: t_soil
    REAL(fp) :: rhoveg, vlai
    REAL(fp) :: local_snow_depth
    COMPLEX(fp) :: esoil, eveg, esnow, eair
    LOGICAL :: SnowEM_Physical_Model

    eair = CMPLX(ONE,-ZERO,fp)
    theta = Angle*PI/180.0_fp

    ! By default use the 
    ! Assign local variable
    mv               = Soil_Moisture_Content
    veg_frac         = Vegetation_Fraction
    t_soil           = Soil_Temperature
    sand = frac_sand(Soil_Type)
    clay = frac_clay(Soil_Type )
    rhob = rhob_soil(Soil_Type )
    local_snow_depth = Snow_Depth

    ! Check soil/skin temperature
    if ( (t_soil <= 100.0_fp .OR.  t_soil >= 350.0_fp) .AND. &
         (t_skin >= 100.0_fp .AND. t_skin <= 350.0_fp) ) t_soil = t_skin

    ! Check soil moisture content range
    mv = MAX(MIN(mv,ONE),ZERO)

    ! Surface type based on snow depth
    IF (local_snow_depth > POINT1) THEN

      ! O.k.; we're going to compute snow emissivities....
      
      ! By default use the physical model for snow
      SnowEM_Physical_Model = .TRUE.
      if (local_snow_depth > snow_depth_c) SnowEM_Physical_Model = .FALSE.

      ! Compute the snow emissivity
      IF ( SnowEM_Physical_Model ) THEN

        ep_real = 3.2_fp
        ep_imag = -0.0005_fp
        sigma = ONE

        ! For deep snow, the performance of the model is poor
        local_snow_depth = MIN(local_snow_depth,1000.0_fp)

        ! The fraction volume of dense medium
        ! scatterers must be in the range (0-1)
        va = 0.4_fp + 0.0004_fp*local_snow_depth
        va = MAX(MIN(va,ONE),ZERO)

        ! Limit for snow grain size
        rad = MIN((POINT5 + 0.005_fp*local_snow_depth),ONE)

        ! Limit for soil temperature
        t_soil = MIN(t_soil,tsoilc_undersnow)

        CALL Snow_Diel(Frequency, ep_real, ep_imag, rad, va, esnow)
        CALL Soil_Diel(Frequency, t_soil, mv, rhob, rhos, sand, clay, esoil)

        theta_i = ASIN(REAL(SIN(theta)*SQRT(eair)/SQRT(esnow),fp))

        CALL Reflectance(esnow, eair, theta_i,  theta, r21_v, r21_h)
        CALL Transmittance(esnow, eair, theta_i, theta, t21_v, t21_h)

        mu      = COS(theta_i)
        theta_t = ASIN(REAL(SIN(theta_i)*SQRT(esnow)/SQRT(esoil),fp))

        CALL Reflectance(esnow, esoil, theta_i, theta_t, r23_v, r23_h)
        CALL Roughness_Reflectance(Frequency, sigma, r23_v, r23_h)
        CALL Snow_Optic(Frequency,rad,local_snow_depth,va,ep_real, ep_imag,gv,gh,&
                        ssalb_v,ssalb_h,tau_v,tau_h)
        CALL Two_Stream_Solution(mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v, &
                                 r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,Emissivity_V,Emissivity_H, &
                               frequency, t_soil, t_skin)
      ELSE
        ! Use the empirical method 
        CALL SnowEM_Default(Frequency,t_skin, local_snow_depth,Emissivity_V,Emissivity_H)
      END IF

    ELSE

      ! No snow, so we're going to compute canopy emissivities....
      
      ! Limit for vegetation fraction
      veg_frac = MAX(MIN(veg_frac,ONE),ZERO)

!     lai = THREE*veg_frac + POINT5
!     mge = POINT5*veg_frac
!     leaf_thick = 0.07_fp
      mu  = COS(theta)
      sigma = POINT5
    
      vlai = Lai*veg_frac
      mge = veg_mge(Vegetation_Type)
      rhoveg = veg_rho(Vegetation_Type)
      leaf_thick = leaf_th(Vegetation_Type)

      r21_h    = ZERO
      r21_v    = ZERO
      t21_h    = ONE
      t21_v    = ONE

      CALL Soil_Diel(Frequency, t_soil, mv, rhob, rhos, sand, clay, esoil)
      theta_t = ASIN(REAL(SIN(theta)*SQRT(eair)/SQRT(esoil),fp))
      CALL Reflectance(eair, esoil, theta, theta_t, r23_v, r23_h)
      CALL Roughness_Reflectance(Frequency, sigma, r23_v, r23_h)
      CALL Canopy_Diel(Frequency, mge, eveg, rhoveg)
      CALL Canopy_Optic(vlai,Frequency,theta,eveg,leaf_thick,gv,gh,ssalb_v,ssalb_h,tau_v,tau_h)
      CALL Two_Stream_Solution(mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v, &
                               r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,Emissivity_V,Emissivity_H, &
                               frequency, t_soil, t_skin)
    END IF

  END SUBROUTINE NESDIS_LandEM



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################



subroutine SnowEM_Default(frequency,ts, depth,Emissivity_V,Emissivity_H)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .
!   prgmmr:  Banghua Yan and Fuzhong Weng               org: nesdis              date: 2005-12-01
!
! abstract: preliminary estimate of snow emissivity using  surface temperature and snow depth
!
! input argument list:
!
!      ts         -  surface temperature
!      frequency   -  frequency (ghz)
!
! output argument list:
!
!      Emissivity_V         -  snow emissivty at V-POL
!      Emissivity_H         -  snow emissivty at H-POL
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  ! Arguments
  REAL(fp) :: frequency,ts, depth,Emissivity_V,Emissivity_H
  ! Local parameters  
  INTEGER , PARAMETER :: new = 7
  INTEGER , PARAMETER :: NFRESH_SHALLOW_SNOW = 1
  INTEGER , PARAMETER :: NPOWDER_SNOW        = 2
  INTEGER , PARAMETER :: NWET_SNOW           = 3
  INTEGER , PARAMETER :: NDEEP_SNOW          = 4
  REAL(fp), PARAMETER :: twet    = 270.0_fp
  REAL(fp), PARAMETER :: tcrust  = 235.0_fp
  REAL(fp), PARAMETER :: depth_s =  50.0_fp
  REAL(fp), PARAMETER :: depth_c = 100.0_fp
  ! Local variables
  INTEGER :: ich,basic_snow_type
  REAL(fp), DIMENSION(new) :: ev, eh, freq
  REAL(fp) :: df, df0


  freq = FREQUENCY_AMSRE(1:new)
  
  ! Determine the snow type based on temperatures
  basic_snow_type = NFRESH_SHALLOW_SNOW
  if (ts >= twet .and. depth <= depth_s) then
    basic_snow_type = NWET_SNOW
  else
    if (depth <= depth_s) then
      basic_snow_type = NFRESH_SHALLOW_SNOW
    else
      basic_snow_type = NPOWDER_SNOW
    endif
  endif
  if (ts <= tcrust .and. depth >= depth_c) basic_snow_type = NDEEP_SNOW

  ! Assign the emissivity spectrum
  SELECT CASE (basic_snow_type)
    CASE (NFRESH_SHALLOW_SNOW)
      ev = GRASS_AFTER_SNOW_EV_AMSRE(1:new)
      eh = GRASS_AFTER_SNOW_EH_AMSRE(1:new)
    CASE (NPOWDER_SNOW)
      ev = POWDER_SNOW_EV_AMSRE(1:new)
      eh = POWDER_SNOW_EH_AMSRE(1:new)
    CASE (NWET_SNOW)
      ev = WET_SNOW_EV_AMSRE(1:new)
      eh = WET_SNOW_EH_AMSRE(1:new)
    CASE (NDEEP_SNOW)
      ev = DEEP_SNOW_EV_AMSRE(1:new)
      eh = DEEP_SNOW_EH_AMSRE(1:new)
  END SELECT

  ! Handle possible extrapolation
  if (frequency <= freq(1)) then
    Emissivity_H = eh(1)
    Emissivity_V = ev(1)
    return
  end if
  if (frequency >= freq(new)) then
    Emissivity_H = eh(new)
    Emissivity_V = ev(new)
    return
  end if

  ! Interpolate emissivity at a certain frequency
  Channel_loop: do ich=2,new
    if (frequency <= freq(ich)) then
      df  = frequency-freq(ich-1)
      df0 = freq(ich)-freq(ich-1)
      Emissivity_H = eh(ich-1) + (df*(eh(ich)-eh(ich-1))/df0)
      Emissivity_V = ev(ich-1) + (df*(ev(ich)-ev(ich-1))/df0)
      exit Channel_loop
    end if
  end do Channel_loop

end subroutine SnowEM_Default


subroutine Canopy_Optic(vlai,frequency,theta,esv,d,gv,gh,&
                        ssalb_v,ssalb_h,tau_v, tau_h)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    canopy_optic compute optic parameters for canopy
!
!   prgmmr:  Fuzhong Weng and Banghua Yan                org: nesdis              date: 2000-11-28
!
! abstract: compute optic parameters for canopy
!
! program history log:
!
! input argument list:
!
!      lai         -  leaf area index
!      frequency   -  frequency (ghz)
!      theta       -  incident angle
!      esv         -  leaf dielectric constant
!      d           -  leaf thickness (mm)
!
! output argument list:
!
!      gv           -  asymmetry factor for v pol
!      gh           -  asymmetry factor for h pol
!      ssalb_v      -  single scattering albedo at v. polarization
!      ssalb_h      -  single scattering albedo at h. polarization
!      tau_v        -  optical depth at v. polarization
!      tau_h        -  optical depth at h. polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  REAL(fp) :: frequency,theta,d,vlai,ssalb_v,ssalb_h,tau_v,tau_h,gv, gh, mu
  COMPLEX(fp) :: ix,k0,kz0,kz1,rhc,rvc,esv,expval1,factt,factrvc,factrhc
  REAL(fp) :: rh,rv,th,tv
  REAL(fp), PARAMETER :: threshold = 0.999_fp

  mu = COS(theta)
  ix = CMPLX(ZERO, ONE, fp)

  k0  = CMPLX(TWOPI*frequency/300.0_fp, ZERO, fp)   ! 1/mm
  kz0 = k0*mu
  kz1 = k0*SQRT(esv - SIN(theta)**2)

  rhc = (kz0 - kz1)/(kz0 + kz1)
  rvc = (esv*kz0 - kz1)/(esv*kz0 + kz1)

  expval1 = EXP(-TWO*ix*kz1*d)
  factrvc = ONE-rvc**2*expval1
  factrhc = ONE-rhc**2*expval1
  factt   = FOUR*kz0*kz1*EXP(ix*(kz0-kz1)*d)

  rv = ABS(rvc*(ONE - expval1)/factrvc)**2
  rh = ABS(rhc*(ONE - expval1)/factrhc)**2

  th = ABS(factt/((kz1+kz0)**2*factrhc))**2
  tv = ABS(esv*factt/((kz1+esv*kz0)**2*factrvc))**2

  gv = POINT5
  gh = POINT5

  tau_v = POINT5*vlai*(TWO-tv-th)
  tau_h = tau_v

  ssalb_v = MIN((rv+rh)/(TWO-tv-th),threshold)
  ssalb_h = ssalb_v

end subroutine Canopy_Optic


subroutine Snow_Optic(frequency,a,h,f,ep_real,ep_imag,gv,gh, ssalb_v,ssalb_h,tau_v,tau_h)

!-------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    landem      comput optic parameters for snow
!
!   prgmmr: Fuzhong Weng and Banghua Yan                 org: nesdis              date: 2000-11-28
!
! abstract: compute optic parameters for snow
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      frequency    -  frequency (ghz)
!      ep_real      -  real part of dielectric constant of particles
!      ep_imag      -  imaginary part of dielectric constant of particles
!      a            -  particle radiu (mm)
!      h            -  snow depth(mm)
!      f            -  fraction volume of snow (0.0 - 1.0)
!
! output argument list:
!
!       ssalb       -  single scattering albedo
!       tau         -  optical depth
!       g           -  asymmetry factor
!
!   important internal variables:
!
!       ks          -  scattering coeffcient (/mm)
!       ka          -  absorption coeffient (/mm)
!       kp          -  eigenvalue of two-stream approximation
!       y           -  = yr+iyi
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  REAL(fp) :: yr,yi,ep_real,ep_imag
  REAL(fp) :: frequency,a,h,f,ssalb_v,ssalb_h,tau_v,tau_h,gv,gh,k
  REAL(fp) :: ks1,ks2,ks3,ks,kr1,kr2,kr3,kr,ki1,ki2,ki3,ki
  REAL(fp) :: fact1,fact2,fact3,fact4,fact5

  k = TWOPI/(300._fp/frequency)

  yr = (ep_real - ONE)/(ep_real + TWO)
  yi = -ep_imag/(ep_real + TWO)

  fact1 = (ONE+TWO*f)**2
  fact2 = ONE-f*yr
  fact3 = (ONE-f)**4
  fact4 = f*(k*a)**3
  fact5 = ONE+TWO*f*yr

  ks1 = k*SQRT(fact2/fact5)
  ks2 = fact4*fact3/fact1
  ks3 = (yr/fact2)**2
  ks = ks1*ks2*ks3

  kr1 = fact5/fact2
  kr2 = TWO*ks2
  kr3 = TWO*yi*yr/(fact2**3)
  kr = k*SQRT(kr1+kr2*kr3)

  ki1 = THREE*f*yi/fact2**2
  ki2 = kr2
  ki3 = ks3
  ki  = k**2/(TWO*kr)*(ki1+ki2*ki3)

  gv = POINT5
  gh = POINT5

  ssalb_v = MIN(ks/ki, 0.999_fp)
  ssalb_h = ssalb_v
  tau_v = TWO*ki*h
  tau_h = tau_v

end subroutine Snow_Optic


subroutine Soil_Diel(freq,t_soil,vmc,rhob,rhos,sand,clay,esm)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Soil_Diel   calculate the dielectric properties of soil
!
!   prgmmr: Fuzhong Weng and Banghua Yan                 org: nesdis              date: 2000-11-28
!
! abstract: compute the dilectric constant of the bare soil
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      frequency    -  frequency (ghz)
!      t_soil       -  soil temperature
!      vmc          -  volumetric moisture content (demensionless)
!      rhob         -  bulk volume density of the soil (1.18-1.12)
!      rhos         -  density of the solids (2.65 g.cm^3 for
!                       solid soil material)
!      sand         -  sand fraction (sand + clay = 1.0)
!      clay         -  clay fraction
!
! output argument list:
!
!      esm          -  dielectric constant for bare soil
!
! important internal variables:
!
!      esof         -  the permittivity of free space
!      eswo         -  static dieletric constant
!      tauw         -  relaxation time of water
!      s            -  salinity
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  REAL(fp) :: f,tauw,freq,t_soil,vmc,rhob,rhos,sand,clay
  REAL(fp) :: alpha,beta,ess,rhoef,t,eswi,eswo
  REAL(fp) :: esof
  COMPLEX(fp) :: esm,esw,es1,es2

  alpha = 0.65_fp
  beta  = 1.09_fp - 0.11_fp*sand + 0.18_fp*clay
  ess = (1.01_fp + 0.44_fp*rhos)**2 - 0.062_fp
  rhoef = -1.645_fp + 1.939_fp*rhob - 0.020213_fp*sand + 0.01594_fp*clay
  t = t_soil - 273.0_fp
  f = freq*1.0e9_fp

  ! the permittivity at the high frequency limit
  eswi = 5.5_fp

  ! the permittivity of free space (esof)
  esof = 8.854e-12_fp

  ! static dieletric constant (eswo)
  eswo = 87.134_fp+(-1.949e-1_fp+(-1.276e-2_fp+2.491e-4_fp*t)*t)*t
  tauw = 1.1109e-10_fp+(-3.824e-12_fp+(6.938e-14_fp-5.096e-16_fp*t)*t)*t

  if (vmc > ZERO) then
     es1 = CMPLX(eswi, -rhoef*(rhos-rhob)/(TWOPI*f*esof*rhos*vmc), fp)
  else
     es1 = CMPLX(eswi, ZERO, fp)
  endif

  es2 = CMPLX(eswo-eswi, ZERO, fp)/CMPLX(ONE, f*tauw, fp)
  esw = es1 + es2
  esm = ONE + (ess**alpha - ONE)*rhob/rhos + vmc**beta*esw**alpha - vmc
  esm = esm**(ONE/alpha)

  if(AIMAG(esm) >= ZERO) esm = CMPLX(REAL(esm,fp),-0.0001_fp, fp)

end subroutine Soil_Diel


subroutine Snow_Diel(frequency,ep_real,ep_imag,rad,frac,ep_eff)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Snow_Diel   compute dielectric constant of snow
!
!   prgmmr: Fuzhong Weng and Banghua Yan                 org: nesdis              date: 2000-11-28
!
! abstract: compute dielectric constant of snow
!
!
! program history log:
!
! input argument list:
!
!       frequency   -  frequency (ghz)
!       ep_real     -  real part of dielectric constant of particle
!       ep_imag     -  imaginary part of dielectric constant of particle
!       rad         -  particle radiu (mm)
!       frac        -  fraction volume of snow (0.0 - 1.0)
!
! output argument list:
!
!       ep_eff      -  dielectric constant of the dense medium
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!  Copyright (C) 2005 Fuzhong Weng and Banghua Yan
!
!  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
!  General Public License as published by the Free Software Foundation; either version 2 of the License,
!  or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
!  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
!  License for more details.
!
!  You should have received a copy of the GNU General Public License along with this program; if not, write
!  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!----------------------------------------------------------------------------------

  REAL(fp) :: ep_imag,ep_real
  REAL(fp) :: frequency,rad,frac,k0,yr,yi
  COMPLEX(fp) :: y,ep_r,ep_i,ep_eff,fracy

  k0 = TWOPI/(300.0_fp/frequency)

  yr = (ep_real - ONE)/(ep_real + TWO)
  yi = ep_imag/(ep_real + TWO)

  y = CMPLX(yr, yi, fp)
  fracy=frac*y

  ep_r = (ONE + TWO*fracy)/(ONE - fracy)
  ep_i = TWO*fracy*y*(k0*rad)**3*(ONE-frac)**4/((ONE-fracy)**2*(ONE+TWO*frac)**2)
  ep_eff = ep_r - CMPLX(ZERO,ONE,fp)*ep_i

  if (AIMAG(ep_eff) >= ZERO) ep_eff = CMPLX(REAL(ep_eff), -0.0001_fp, fp)

end subroutine Snow_Diel


subroutine Canopy_Diel(frequency,mg,esv,rhoveg)

!----------------------------------------------------------------------------------
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   canopy_diel compute the dielectric constant of the vegetation canopy
!
!   prgmmr:  Fuzhong Weng and Banghua Yan                org: nesdis              date: 2000-11-28
!
! abstract: compute the dielectric constant of the vegetation canopy geomatrical optics approximation
!
!           for vegetation canopy work for horizontal leaves
!
! program history log:
!
! input argument list:
!
!      frequency    -  frequency (ghz)
!      mg           -  gravimetric water content
!
! output argument list:
!
!      esv          -  dielectric constant of leaves
!
! remarks:
!
! references:
!
!     ulaby and el-rayer, 1987: microwave dielectric spectrum of vegetation part ii,
!           dual-dispersion model, ieee trans geosci. remote sensing, 25, 550-557
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  REAL(fp) :: frequency,  mg, en, vf, vb
  REAL(fp) :: rhoveg, vmv
  COMPLEX(fp) :: esv, xx

  vmv = mg*rhoveg/( ONE - mg*(ONE-rhoveg) )
  en = 1.7_fp + (3.2_fp + 6.5_fp*vmv)*vmv

  vf = vmv*(0.82_fp*vmv + 0.166_fp)
  vb = 31.4_fp*vmv*vmv/( ONE + 59.5_fp*vmv*vmv)

  xx = CMPLX(ZERO,ONE,fp)

  esv = en + vf*(4.9_fp + 75.0_fp/(ONE + xx*frequency/18.0_fp)-xx*(18.0_fp/frequency)) + &
        vb*(2.9_fp + 55.0_fp/(ONE + SQRT(xx*frequency/0.18_fp)))

  if (AIMAG(esv) >= ZERO) esv = CMPLX(REAL(esv), -0.0001_fp, fp)

end subroutine Canopy_Diel


subroutine Reflectance(em1, em2, theta_i, theta_t, rv, rh)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Reflectance compute the surface reflectivity
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the surface reflectivety using fresnel equations
!    for a rough surface having a standard deviation of height of sigma
!
! program history log:
!
! input argument list:
!      theta_i      -  incident angle (degree)
!      theta_t      -  transmitted angle (degree)
!      em1          -  dielectric constant of the medium 1
!      em2          -  dielectric constant of the medium 2
!
! output argument list:
!
!      rv           -  reflectivity at vertical polarization
!      rh           -  reflectivity at horizontal polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  REAL(fp) :: theta_i, theta_t
  REAL(fp) :: rh, rv,cos_i,cos_t
  COMPLEX(fp) :: em1, em2, m1, m2, angle_i, angle_t

  ! compute the refractive index ratio between medium 2 and 1
  ! using dielectric constant (n = SQRT(e))
  cos_i = COS(theta_i)
  cos_t = COS(theta_t)

  angle_i = CMPLX(cos_i, ZERO, fp)
  angle_t = CMPLX(cos_t, ZERO, fp)

  m1 = SQRT(em1)
  m2 = SQRT(em2)

  rv = (ABS((m1*angle_t-m2*angle_i)/(m1*angle_t+m2*angle_i)))**2
  rh = (ABS((m1*angle_i-m2*angle_t)/(m1*angle_i+m2*angle_t)))**2

end subroutine Reflectance


subroutine Transmittance(em1,em2,theta_i,theta_t,tv,th)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Transmittance    calculate Transmittance
!
!   prgmmr:  Banghua Yan and Fuzhong Weng               org: nesdis              date: 2000-11-28
!
! abstract: compute Transmittance
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      theta_i      -  incident angle (degree)
!      theta_t      -  transmitted angle (degree)
!      em1          -  dielectric constant of the medium 1
!      em2          -  dielectric constant of the medium 2
!
! output argument list:
!
!      tv           -  transmisivity at vertical polarization
!      th           -  transmisivity at horizontal polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  REAL(fp) :: theta_i, theta_t
  REAL(fp) :: th, tv, rr, cos_i,cos_t
  COMPLEX(fp) :: em1, em2, m1, m2, angle_i, angle_t

  ! compute the refractive index ratio between medium 2 and 1
  ! using dielectric constant (n = SQRT(e))
  cos_i = COS(theta_i)
  cos_t = COS(theta_t)

  angle_i = CMPLX(cos_i, ZERO, fp)
  angle_t = CMPLX(cos_t, ZERO, fp)

  m1 = SQRT(em1)
  m2 = SQRT(em2)

  rr = ABS(m2/m1)*cos_t/cos_i
  tv = rr*(ABS(TWO*m1*angle_i/(m1*angle_t + m2*angle_i)))**2
  th = rr*(ABS(TWO*m1*angle_i/(m1*angle_i + m2*angle_t)))**2

end subroutine Transmittance


subroutine Roughness_Reflectance(frequency,sigma,rv,rh)

!-------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rought_reflectance    calculate surface relectivity
!
!   prgmmr: Banghua Yan and Fuzhong Weng                 org: nesdis              date: 2000-11-28
!
! abstract: compute the surface reflectivety for a rough surface having a standard devoation of height of sigma
!
!
! program history log:
!
! input argument list:
!
!      frequency    -  frequency (ghz)
!
!      theta        -  local zenith angle (degree) (currently, not used here)
!
!      sigma        -  standard deviation of rough surface height
!
!                      smooth surface:0.38, medium: 1.10, rough:2.15 cm
!
!    internal variables
!
!
! output argument list:
!
!      rv            -  reflectivity at vertical polarization
!      rh            -  reflectivity at horizontal polarization
!
!
!   important internal variables:
!
!      k0           -  a propagation constant or wavenumber in a free space
!
! remarks:
!
! references:
!
!   wang, j. and b. j. choudhury, 1992: passive microwave radiation from soil: examples...
!    passive microwave remote sensing of .. ed. b. j. choudhury, etal vsp.
!    also wang and choudhury (1982)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!-------------------------------------------------------------------------------------------------------------

  REAL(fp) :: frequency
  REAL(fp) :: q, rh, rv, rh_s, rv_s, sigma

  rh_s = 0.3_fp*rh
  rv_s = 0.3_fp*rv
  q = 0.35_fp*(ONE - EXP(-0.60_fp*frequency*sigma**TWO))
  rh = rh_s + q*(rv_s-rh_s)
  rv = rv_s + q*(rh_s-rv_s)

end subroutine Roughness_Reflectance


subroutine Two_Stream_Solution(mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v, &
      r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,esv,esh,frequency,t_soil,t_skin)

!-------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    two_stream_solution
!
!   prgmmr: Banghua Yan and Fuzhong Weng                 org: nesdis              date: 2000-11-28
!
! abstract: two stream solution
!         Updated with the more accurate formula of total upwelling radiance emanating from the surface.
!
! REFERENCES:
!       Weng, F., B. Yan, and N. Grody, 2001: "A microwave land emissivity model", J. Geophys. Res., 106,
!                                             20, 115-20, 123
!   version: beta
!
! program history log:
!
! input argument list:
!
!      b            -  scattering layer temperature (k)         (gdas)   (not used here)
!      mu           -  cos(theta)
!      gv           -  asymmetry factor for v pol
!      gh           -  asymmetry factor for h pol
!      ssalb_v      -  single scattering albedo at v. polarization
!      ssalb_h      -  single scattering albedo at h. polarization
!      tau_v        -  optical depth at v. polarization
!      tau_h        -  optical depth at h. polarization
!      r12_v        -  reflectivity at vertical polarization   (not used here)
!      r12_h        -  reflectivity at horizontal polarization (not used here)
!      r21_v        -  reflectivity at vertical polarization
!      r21_h        -  reflectivity at horizontal polarization
!      r23_v        -  reflectivity at vertical polarization
!      r23_h        -  reflectivity at horizontal polarization
!      t21_v        -  transmisivity at vertical polarization
!      t21_h        -  transmisivity at horizontal polarization
!      t12_v        -  transmisivity at vertical polarization   (not used here)
!      t12_h        -  transmisivity at horizontal polarization (not used here)
!      Frequency    -  frequency
!      t_soil       -  soil temperature
!      t_skin       -  land surface temperature
!
! output argument list:
!
!       esh         -  emissivity for horizontal polarization
!       esv         -  emissivity for vertical polarization
!
! Local variables:
!       gsect0, gsect1_h, gsect1_v, gsect2_h, gsect2_v
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!-------------------------------------------------------------------------------------------------------------

  REAL(fp) :: mu, gv, gh, ssalb_h, ssalb_v, tau_h,tau_v,                 &
              r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, esv, esh
  REAL(fp) :: alfa_v, alfa_h, kk_h, kk_v, gamma_h, gamma_v, beta_v, beta_h
  REAL(fp) :: fact1,fact2
  REAL(fp) :: frequency, t_soil, t_skin
  REAL(fp) :: gsect0, gsect1_h, gsect1_v, gsect2_h, gsect2_v

  alfa_h  = SQRT((ONE - ssalb_h)/(ONE - gh*ssalb_h))
  kk_h    = SQRT((ONE - ssalb_h)*(ONE -  gh*ssalb_h))/mu
  beta_h  = (ONE - alfa_h)/(ONE + alfa_h)
  gamma_h = (beta_h -r23_h)/(ONE-beta_h*r23_h)

  alfa_v  = SQRT((ONE-ssalb_v)/(ONE - gv*ssalb_v))
  kk_v    = SQRT((ONE-ssalb_v)*(ONE - gv*ssalb_v))/mu
  beta_v  = (ONE - alfa_v)/(ONE + alfa_v)
  gamma_v = (beta_v -r23_v)/(ONE-beta_v*r23_v)

  fact1=gamma_h*EXP(-TWO*kk_h*tau_h)
  fact2=gamma_v*EXP(-TWO*kk_v*tau_v)

  gsect0  =(EXP(C_2*frequency/t_skin) -ONE)/(EXP(C_2*frequency/t_soil) -ONE)

  gsect1_h=(ONE-r23_h)*(gsect0-ONE)
  gsect2_h=((ONE-beta_h*beta_h)/(ONE-beta_h*r23_h))*EXP(-kk_h*tau_h)

  gsect1_v=(ONE-r23_v)*(gsect0-ONE)
  gsect2_v=((ONE-beta_v*beta_v)/(ONE-beta_v*r23_v))*EXP(-kk_h*tau_v)

  esh  = t21_h*((ONE - beta_h)*(ONE + fact1)+gsect1_h*gsect2_h) /(ONE-beta_h*r21_h-(beta_h-r21_h)*fact1)
  esv  = t21_v*((ONE - beta_v)*(ONE + fact2)+gsect1_v*gsect2_v) /(ONE-beta_v*r21_v-(beta_v-r21_v)*fact2)

  if (esh < EMISSH_DEFAULT) esh = EMISSH_DEFAULT
  if (esv < EMISSV_DEFAULT) esv = EMISSV_DEFAULT

  if (esh > ONE) esh = ONE
  if (esv > ONE) esv = ONE

end subroutine Two_Stream_Solution

END MODULE NESDIS_LandEM_Module
