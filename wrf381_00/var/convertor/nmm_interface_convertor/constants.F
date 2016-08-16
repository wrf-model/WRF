module constants
!$$$   module documentation block
!                .      .    .                                       .
! module:    constants
!   prgmmr: treadon          org: np23                date: 2003-09-25
!
! abstract:  This module contains the definition of various constants
!            used in the gsi code
!
! program history log:
!   2003-09-25  treadon - original code
!   2004-03-02  treadon - allow global and regional constants to differ
!   2004-06-16  treadon - update documentation
!   2004-10-28  treadon - replace parameter tiny=1.e-12 with tiny_r_kind
!                         and tiny_single
!   2004-11-16  treadon - add huge_single, huge_r_kind parameters
!   2005-01-27  cucurull - add ione
!   2005-08-24  derber   - move cg_term to constants from qcmod
!   2006-03-07  treadon  - add rd_over_cp_mass
!   2006-05-18  treadon  - add huge_i_kind
!   2006-06-06       su  - add var-qc wgtlim, change value to 0.25 (ECMWF)
!   2006-07-28  derber   - add r1000
!   2007-03-20  rancic   - add r3600
!
! Subroutines Included:
!   sub init_constants  - compute derived constants, set regional/global constants
!
! Variable Definitions:
!   see below
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,r_kind,i_kind
  implicit none

! Declare constants
  integer(i_kind) izero,ione
  real(r_kind) rearth,grav,omega,rd,rv,cp,cv,cvap,cliq
  real(r_kind) csol,hvap,hfus,psat,t0c,ttp,jcal,cp_mass,cg_term
  real(r_kind) fv,deg2rad,rad2deg,pi,tiny_r_kind,huge_r_kind,huge_i_kind
  real(r_kind) ozcon,rozcon,tpwcon,rd_over_g,rd_over_cp,g_over_rd
  real(r_kind) amsua_clw_d1,amsua_clw_d2,constoz,zero,one,two,four
  real(r_kind) one_tenth,quarter,three,five,rd_over_cp_mass
  real(r_kind) rearth_equator,stndrd_atmos_ps,r1000
  real(r_kind) r3600    
  real(r_kind) semi_major_axis,semi_minor_axis,n_a,n_b
  real(r_kind) eccentricity,grav_polar,grav_ratio
  real(r_kind) grav_equator,earth_omega,grav_constant
  real(r_kind) flattening,eccentricity_linear,somigliana
  real(r_kind) dldt,dldti,hsub,psatk,tmix,xa,xai,xb,xbi
  real(r_kind) eps,epsm1,omeps,wgtlim
  real(r_kind) elocp,cpr,el2orc,cclimit,climit,epsq
  real(r_kind) pcpeff0,pcpeff1,pcpeff2,pcpeff3,rcp,c0,delta
  real(r_kind) h1000,factor1,factor2,rhcbot,rhctop,dx_max,dx_min,dx_inv
  real(r_kind) h300,half,cmr,cws,ke2,row,rrow
  real(r_single) zero_single,tiny_single,huge_single


! Define constants common to global and regional applications
!           name     value                  description                     units
!           ----     -----                  -----------                     -----
  parameter(rearth_equator= 6.37813662e6_r_kind) ! equatorial earth radius (m)
  parameter(omega  = 7.2921e-5_r_kind)  !  angular velocity of earth       (1/s)
  parameter(cp     = 1.0046e+3_r_kind)  !  specific heat of air @pressure  (J/kg/K)
  parameter(cvap   = 1.8460e+3_r_kind)  !  specific heat of h2o vapor      (J/kg/K)
  parameter(csol   = 2.1060e+3_r_kind)  !  specific heat of solid h2o (ice)(J/kg/K)
  parameter(hvap   = 2.5000e+6_r_kind)  !  latent heat of h2o condensation (J/kg)
  parameter(hfus   = 3.3358e+5_r_kind)  !  latent heat of h2o fusion       (J/kg)
  parameter(psat   = 6.1078e+2_r_kind)  !  pressure at h2o triple point    (Pa)
  parameter(t0c    = 2.7315e+2_r_kind)  !  temperature at zero celsius     (K)
  parameter(ttp    = 2.7316e+2_r_kind)  !  temperature at h2o triple point (K)
  parameter(jcal   = 4.1855e+0_r_kind)  !  joules per calorie              ()
  parameter(stndrd_atmos_ps = 1013.25e2_r_kind) ! 1976 US standard atmosphere ps   (Pa)

! Numeric constants
  parameter(izero  = 0)
  parameter(ione   = 1)
  parameter(zero_single = 0.0_r_single)
  parameter(zero   = 0.0_r_kind)
  parameter(one_tenth  = 0.10_r_kind)
  parameter(quarter= 0.25_r_kind)
  parameter(one    = 1.0_r_kind)
  parameter(two    = 2.0_r_kind)
  parameter(three  = 3.0_r_kind)
  parameter(four   = 4.0_r_kind)
  parameter(five   = 5.0_r_kind)
  parameter(r1000  = 1000.0_r_kind)
  parameter(r3600  = 3600.0_r_kind)

! Constants for gps refractivity
  parameter(n_a=77.6_r_kind) !K/mb
  parameter(n_b=3.73e+5_r_kind) !K^2/mb

! Parameters below from WGS-84 model software inside GPS receivers.
  parameter(semi_major_axis = 6378.1370e3_r_kind)    !                     (m)
  parameter(semi_minor_axis = 6356.7523142e3_r_kind) !                     (m)
  parameter(grav_polar = 9.8321849378_r_kind)        !                     (m/s2)
  parameter(grav_equator = 9.7803253359_r_kind)      !                     (m/s2) 
  parameter(earth_omega = 7.292115e-5_r_kind)        !                     (rad/s)
  parameter(grav_constant = 3.986004418e14_r_kind)   !                     (m3/s2)

! Derived geophysical constants
  parameter(flattening = (semi_major_axis-semi_minor_axis)/semi_major_axis)!() 
  parameter(somigliana = &
       (semi_minor_axis/semi_major_axis) * (grav_polar/grav_equator) - one)!()
  parameter(grav_ratio = (earth_omega*earth_omega * &
       semi_major_axis*semi_major_axis * semi_minor_axis) / grav_constant) !()

! Derived thermodynamic constants
  parameter ( dldti = cvap-csol )
  parameter ( hsub = hvap+hfus )
  parameter ( psatk = psat*0.001_r_kind )
  parameter ( tmix = ttp-20._r_kind )
  parameter ( elocp = hvap/cp )
  parameter ( rcp  = one/cp )

! Constants used in GFS moist physics
  parameter ( h300 = 300._r_kind )
  parameter ( half = 0.5_r_kind )
  parameter ( cclimit = 0.001_r_kind )
  parameter ( climit = 1.e-20_r_kind)
  parameter ( epsq = 2.e-12_r_kind )
  parameter ( h1000 = 1000.0_r_kind)
  parameter ( rhcbot=0.85_r_kind )
  parameter ( rhctop=0.85_r_kind )
  parameter ( dx_max=-8.8818363_r_kind )
  parameter ( dx_min=-5.2574954_r_kind )
  parameter ( dx_inv=one/(dx_max-dx_min) )
  parameter ( c0=0.002_r_kind )
  parameter ( delta=0.6077338_r_kind )
  parameter ( pcpeff0=1.591_r_kind )
  parameter ( pcpeff1=-0.639_r_kind )
  parameter ( pcpeff2=0.0953_r_kind )
  parameter ( pcpeff3=-0.00496_r_kind )
  parameter ( cmr = one/0.0003_r_kind )
  parameter ( cws = 0.025_r_kind )
  parameter ( ke2 = 0.00002_r_kind )
  parameter ( row = 1000._r_kind )
  parameter ( rrow = one/row )

! Constant used to process ozone
  parameter ( constoz = 604229.0_r_kind)

! Constants used in cloud liquid water correction for AMSU-A
! brightness temperatures
  parameter ( amsua_clw_d1 = 0.754_r_kind )
  parameter ( amsua_clw_d2 = -2.265_r_kind )

! Constants used for variational qc
  parameter ( wgtlim = 0.25_r_kind)  ! Cutoff weight for concluding that obs has been
                                     ! rejected by nonlinear qc. This limit is arbitrary
                                     ! and DOES NOT affect nonlinear qc. It only affects
                                     ! the printout which "counts" the number of obs that
                                     ! "fail" nonlinear qc.  Observations counted as failing
                                     ! nonlinear qc are still assimilated.  Their weight
                                     ! relative to other observations is reduced. Changing
                                     ! wgtlim does not alter the analysis, only
                                     ! the nonlinear qc data "count"

contains
  subroutine init_constants_derived
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_constants_derived          set derived constants
!     prgmmr:    treadon          org: np23           date: 2004-12-02
!
! abstract:  This routine sets derived constants
!
! program history log:
!   2004-12-02  treadon
!   2005-03-03  treadon - add implicit none
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none
    logical regional
    real(r_kind) reradius,g,r_d,r_v,cliq_wrf

!   Trigonometric constants
    pi      = acos(-one)
    deg2rad = pi/180.0_r_kind
    rad2deg = one/deg2rad
    cg_term = (sqrt(two*pi))/two                  ! constant for variational qc
    tiny_r_kind = tiny(zero)
    huge_r_kind = huge(zero)
    tiny_single = tiny(zero_single)
    huge_single = huge(zero_single)
    huge_i_kind = huge(izero)

!   Geophysical parameters used in conversion of geopotential to
!   geometric height
    eccentricity_linear = sqrt(semi_major_axis**2 - semi_minor_axis**2)
    eccentricity = eccentricity_linear / semi_major_axis

    return
  end subroutine init_constants_derived

  subroutine init_constants(regional)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_constants       set regional or global constants
!     prgmmr:    treadon          org: np23           date: 2004-03-02
!
! abstract:  This routine sets constants specific to regional or global
!            applications of the gsi
!
! program history log:
!   2004-03-02  treadon
!   2004-06-16  treadon, documentation
!   2004-10-28  treadon - use intrinsic TINY function to set value
!                         for smallest machine representable positive
!                         number
!   2004-12-03  treadon - move derived constants to init_constants_derived
!   2005-03-03  treadon - add implicit none
!
!   input argument list:
!     regional - if .true., set regional gsi constants;
!                otherwise (.false.), use global constants
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none
    logical regional
    real(r_kind) reradius,g,r_d,r_v,cliq_wrf

!   Define regional constants here
    if (regional) then

!      Name given to WRF constants
       reradius = one/6370.e03_r_kind
       g        = 9.81_r_kind
       r_d      = 287.04_r_kind
       r_v      = 461.6_r_kind
       cliq_wrf = 4190.0_r_kind
       cp_mass  = 1004.67_r_kind

!      Transfer WRF constants into unified GSI constants
       rearth = one/reradius
       grav   = g
       rd     = r_d
       rv     = r_v
       cv     = cp-r_d
       cliq   = cliq_wrf
       rd_over_cp_mass = rd / cp_mass

!   Define global constants here
    else
       rearth = 6.3712e+6_r_kind
       grav   = 9.80665e+0_r_kind
       rd     = 2.8705e+2_r_kind
       rv     = 4.6150e+2_r_kind
       cv     = 7.1760e+2_r_kind
       cliq   = 4.1855e+3_r_kind
       cp_mass= zero
       rd_over_cp_mass = zero
    endif


!   Now define derived constants which depend on constants
!   which differ between global and regional applications.

!   Constants related to ozone assimilation
    ozcon = grav*21.4e-9_r_kind
    rozcon= one/ozcon

!   Constant used in vertical integral for precipitable water
    tpwcon = 100.0_r_kind/grav

!   Derived atmospheric constants
    fv         = rv/rd-one    ! used in virtual temperature equation 
    dldt       = cvap-cliq
    xa         = -(dldt/rv)
    xai        = -(dldti/rv)
    xb         = xa+hvap/(rv*ttp)
    xbi        = xai+hsub/(rv*ttp)
    eps        = rd/rv
    epsm1      = rd/rv-one
    omeps      = one-eps
    factor1    = (cvap-cliq)/rv
    factor2    = hvap/rv-factor1*t0c
    cpr        = cp*rd
    el2orc     = hvap*hvap/(rv*cp)
    rd_over_g  = rd/grav
    rd_over_cp = rd/cp
    g_over_rd  = grav/rd

    return
  end subroutine init_constants

end module constants
