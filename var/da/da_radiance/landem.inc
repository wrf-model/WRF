subroutine landem(theta,freq,mv,veg_frac,veg_tp,soil_tp, &
     t_soil,t_skin,snow_depth,esh,esv)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    landem      noaa/nesdis emissivity model over land/ice
!
!   prgmmr: Fuzhong Weng     org: nesdis              date: 2000-11-28
!           Banghua Yan
!
! abstract: noaa/nesdis emissivity model to compute microwave emissivity over
!       various land surface conditions (land/snow)
!
!    reference: weng, f, b. yan, and n. grody, 2001: 
!      "A microwave land emissivity model", J. Geophys. Res., 106,
!       20, 115-20, 123
!
!   version: beta 
!
! program history log:
!   1999-07-01  weng
!   2000-01-01  yan - add canopy scattering
!   2000-11-28  weng,yan - parameterize model input and incorporate into NCEP ssi
!   2004-08-04  treadon - add only on use declarations; add intent in/out
!   2004-09-22  todling - replaced zsqrt by general interface sqrt
!
! input argument list:
!
!       theta       -  local zenith angle (0 - 60.0)
!       freq        -  frequency in ghz ( 0 - 90.0) 
!       mv          -  volumetric moisture content in soil (0.0 - 1.0) (gdas)
!       veg_frac    -  vegetation fraction (0 - 1.0)                   (gdas)
!       veg_tp      -  vegetation type               (gdas, not used)
!                       1: broadleave evergreen trees
!                       2: broadleave deciduous trees
!                       3: broad & needle mixed forest
!                       4: needleleave evergreen trees
!                       5: needleleave deciduous trees
!                       6: broadleave tree with groundcover (savana)
!                       7: groundcover only (perenial groundcover)
!                       8: broadleave shrubs with perenial groundcover
!                       9: broadleave shrubs with bare soil
!                       10: dwarf trees & shrubs with bare soil
!                       11: bare soil'
!                       12: cultivations (use paramater 7)
!                       13: glacial             
!
!       soil_tp     -  soil type                 (gdas, not used)
!                       1: loamy sand (coarse)
!                       2: silty clayloam (medium)
!                       3: light clay (fine)
!                       4: sand loam (coarse-medium)
!                       5: sandy clay (coarse-fine)
!                       6: clay loam (medium-fine)
!                       7: sandy clay loam (coarse-med-fine)
!                       8: loam (organic)
!                       9: ice (use loamy sand property)
!
!       t_soil      -  soil temperature (k)                 (gdas)
!       t_skin      -  scattering layer temperature (k)     (gdas)
!       snow_depth  -  scatter medium depth (mm)            (gdas)
!
!
! output argument list:
!
!       esh         -  emissivity for horizontal polarization
!       esv         -  emissivity for vertical polarization
!
! important internal variables:
!
!       rhob        -  bulk volume density of the soil (1.18-1.12)
!       rhos        -  density of the solids (2.65 g.cm^3 for solid soil material)
!       sand        -  sand fraction (sand + clay = 1.0)
!       clay        -  clay fraction 
!       lai         -  leaf area index (eg. lai = 4.0 for corn leaves)
!       sigma       -  surface roughness formed between medium 1 and 2, 
!                      expressed as he standard deviation of roughtness height (mm)
!       leaf_thick  --  leaf thickness (mm)
!       rad         -  radius of dense medium scatterers (mm)
!       va          -  fraction volume of dense medium scatterers(0.0 - 1.0)
!       ep          -  dielectric constant of ice or sand particles, complex value
!                               (e.g, 3.0+i0.0)
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind
!  use constants, only: zero,one_tenth,half,one,three
  implicit none

! Declare passed variables
  real(r_kind),intent(in):: theta,freq,mv,veg_frac,veg_tp,soil_tp,&
       t_soil,t_skin,snow_depth
  real(r_kind),intent(out):: esh,esv
  
! Declare local parameters
  real(r_kind),parameter:: rhob=1.18_r_kind
  real(r_kind),parameter:: rhos = 2.65_r_kind
  real(r_kind),parameter:: sand = 0.8_r_kind
  real(r_kind),parameter:: clay = 0.2_r_kind

! Declare local variables
  real(r_kind) b,theta_i,theta_t,mu,r12_h,r12_v,r21_h,r21_v,r23_h,r23_v, &
      t21_v,t21_h,t12_v,t12_h,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,mge,&
      lai,leaf_thick,rad,sigma,va,ep_real,ep_imag
  complex(r_kind) esoil, eveg, esnow, eair

  eair = dcmplx(one,-zero)
! if (snow_depth .gt.zero) then 
  if (snow_depth .gt.one_tenth) then  
                              ! ice dielectric constant 
     ep_real = 3.2_r_kind
     ep_imag = -0.0005_r_kind
     sigma = one
     va = 0.4_r_kind + 0.0004_r_kind*snow_depth
     rad = one + 0.005_r_kind*snow_depth
     call snow_diel(freq, ep_real, ep_imag, rad, va, esnow)
     call soil_diel(freq, t_soil, mv, rhob, rhos, sand, clay, esoil)
!    theta_t = dasin(dreal(sin(theta)*sqrt(eair)/sqrt(esnow)))
!    call reflectance(eair, esnow, theta, theta_t, r12_v, r12_h)
!    call transmitance(eair, esnow, theta, theta_t, t12_v, t12_h)
     theta_i = asin(real(sin(theta)*sqrt(eair)/sqrt(esnow)))
     call reflectance(esnow, eair, theta_i,  theta, r21_v, r21_h)
     call transmitance(esnow, eair, theta_i, theta, t21_v, t21_h)
     mu  = cos(theta_i)
     theta_t = asin(real(sin(theta_i)*sqrt(esnow)/sqrt(esoil)))
     call reflectance(esnow, esoil, theta_i, theta_t, r23_v, r23_h)
     call rough_reflectance(freq, theta_i, sigma, r23_v, r23_h)
     call snow_optic(freq,rad,snow_depth,va,ep_real, ep_imag,gv,gh,& 
            ssalb_v,ssalb_h,tau_v,tau_h)
     call two_stream_solution(t_skin,mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,r12_h, &
       r12_v,r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,t12_v,t12_h,esv,esh)
  else 
     sigma = half
     lai = three*veg_frac + half
     mge = half*veg_frac
     leaf_thick = 0.07_r_kind
     mu  = cos(theta)
!    r12_h    = zero
!    r12_v    = zero
     r21_h    = zero
     r21_v    = zero
     t21_h    = one
     t21_v    = one
!    t12_v    = one
!    t12_h    = one
     call soil_diel(freq, t_soil, mv, rhob, rhos, sand, clay, esoil)
     theta_t = asin(real(sin(theta)*sqrt(eair)/sqrt(esoil)))
     call reflectance(eair, esoil, theta, theta_t, r23_v, r23_h)
     call rough_reflectance(freq, theta, sigma, r23_v, r23_h)
     call canopy_diel(freq, mge, eveg)
     call canopy_optic(lai,freq,theta,eveg,leaf_thick,gv,gh,ssalb_v,ssalb_h,tau_v,tau_h)
     call two_stream_solution(t_skin,mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,&
          r12_h,r12_v,r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,t12_v,t12_h,esv,esh)
  endif
  return
end subroutine landem

subroutine canopy_optic(lai,frequency,theta,esv,d,gv,gh,&
     ssalb_v,ssalb_h,tau_v, tau_h)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    canopy_optic compute optic parameters for canopy
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute optic parameters for canopy
!
! program history log:
!   2004-09-22  todling - replaced zsqrt by general interface sqrt; same for zexp/exp
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
!$$$

!  use kinds, only: r_kind 
!  use constants, only: zero, half, one, two, four, pi
  implicit none
  real(r_kind) threshold
  real(r_kind) frequency,theta,d,lai,ssalb_v,ssalb_h,tau_v,tau_h,gv, gh, mu
  complex(r_kind) ix,k0,kz0,kz1,rhc,rvc,esv,expval1,factt,factrvc,factrhc
  real(r_kind) rh,rvert,th,tv,av,ah,astar,rstar
  threshold=0.999_r_kind
  mu = cos(theta)
  ix  = dcmplx(zero,one)
  k0  = dcmplx(two*pi*frequency/300.0_r_kind, zero)   ! 1/mm
  kz0 = k0*mu
  kz1 = k0*sqrt(esv - sin(theta)**2)
  rhc = (kz0 - kz1)/(kz0 + kz1)
  rvc = (esv*kz0 - kz1)/(esv*kz0 + kz1)  
  expval1=exp(-two*ix*kz1*d)
  factrvc=one-rvc**2*expval1
  factrhc=one-rhc**2*expval1
  factt=four*kz0*kz1*exp(ix*(kz0-kz1)*d)
  rvert = abs(rvc*(one - expval1)/factrvc)**2
  rh = abs(rhc*(one - expval1)/factrhc)**2
  th = abs(factt/((kz1+kz0)**2*factrhc))**2
  tv = abs(esv*factt/((kz1+esv*kz0)**2*factrvc))**2
! av = one - rvert - tv
! ah = one - rh - th
! astar = av + ah
! rstar = rvert + rh
! randomly oriented leaves
  gv = half
  gh = half
! tau_v = half*lai*(astar + rstar)
  tau_v = half*lai*(two-tv-th)
  tau_h = tau_v
! tau_h = half*lai*(astar + rstar)
! ssalb_v = rstar/ (astar + rstar)
  ssalb_v = min((rvert+rh)/ (two-tv-th),threshold)
  ssalb_h = ssalb_v
! ssalb_h = rstar/ (astar + rstar)
  return
end subroutine canopy_optic

subroutine snow_optic(frequency,a,h,f,ep_real,ep_imag,gv,gh,&
     ssalb_v,ssalb_h,tau_v,tau_h)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    landem      comput optic parameters for snow
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
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
!$$$

!  use kinds, only: r_kind
!  use constants, only: half, one, two, three, pi
  implicit none
  real(r_kind) yr,yi,ep_real,ep_imag
  real(r_kind) frequency,a,h,f,ssalb_v,ssalb_h,tau_v,tau_h,gv,gh,k
  real(r_kind) ks1,ks2,ks3,ks,kr1,kr2,kr3,kr,ki1,ki2,ki3,ki,ka
  real(r_kind) fact1,fact2,fact3,fact4,fact5
  k = two*pi/(300._r_kind/frequency)
  yr  = (ep_real - one)/(ep_real + two)
  yi =  - ep_imag/(ep_real + two)
  fact1 = (one+two*f)**2
  fact2 = one-f*yr
  fact3 = (one-f)**4
  fact4 = f*(k*a)**3
  fact5 = one+two*f*yr
  ks1 = k*sqrt(fact2/fact5)
  ks2 = fact4*fact3/fact1
  ks3 = (yr/fact2)**2
  ks = ks1*ks2*ks3  
  kr1 = fact5/fact2
! kr2 = two*fact4*fact3/fact1
  kr2 = two*ks2
  kr3 = two*yi*yr/(fact2**3)
  kr = k*sqrt(kr1+kr2*kr3)
  ki1 = three*f*yi/fact2**2
! ki2 = two*fact4*fact3/fact1
  ki2 = kr2
! ki3 = (yr/fact2)**2
  ki3 = ks3
  ki  = k**2/(two*kr)*(ki1+ki2*ki3)
! ka = ki - ks
  gv = half
  gh = half
  ssalb_v = ks / ki
  if(ssalb_v .gt. 0.999_r_kind) ssalb_v = 0.999_r_kind
  ssalb_h = ssalb_v
  tau_v = two*ki*h
  tau_h = tau_v
  return 
end subroutine snow_optic
subroutine soil_diel(freq,t_soil,vmc,rhob,rhos,sand,clay,esm)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    soil_diel   calculate the dielectric properties of soil
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
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
!$$$

!  use kinds, only: r_kind
!  use constants, only: zero, one, two, pi
  implicit none
  real(r_kind) esof
  real(r_kind)    f,a,b,tauw,freq,t_soil,vmc,rhob,rhos,sand,clay
  real(r_kind)    s,alpha,beta,ess,rhoef,t,eswi,eswo
  complex(r_kind) ix,esm,esw,es1,es2
!
! 
! ix  = dcmplx(zero, one)
! s = zero
  alpha = 0.65_r_kind
  beta  = 1.09_r_kind - 0.11_r_kind*sand + 0.18_r_kind*clay
  ess = (1.01_r_kind + 0.44_r_kind*rhos)**2 - 0.062_r_kind                              !a2
  rhoef = -1.645_r_kind + 1.939_r_kind*rhob - 0.020213_r_kind*sand + 0.01594_r_kind*clay       !a4
  t = t_soil - t_landem
  f = freq*1.0e9_r_kind
! the permittivity at the high frequency limit
  eswi = 5.5_r_kind
! the permittivity of free space (esof)
  esof = 8.854e-12_r_kind
! static dieletric constant (eswo)
  eswo = 87.134_r_kind+(-1.949e-1_r_kind+(-1.276e-2_r_kind+2.491e-4_r_kind*t)*t)*t
! a = one+(1.613e-5_r_kind*t-3.656e-3_r_kind+(3.210e-5_r_kind-4.232e-7_r_kind*s)*s)*s
! eswo = eswo*a
! relaxation time of water (tauw)
  tauw = 1.1109e-10_r_kind+(-3.824e-12_r_kind+(6.938e-14_r_kind-5.096e-16_r_kind*t)*t)*t
! b = one+(2.282e-5_r_kind*t-7.638e-4_r_kind+(-7.760e-6_r_kind+1.105e-8_r_kind*s)*s)*s
! tauw = tauw*b
  if (vmc .gt. zero) then
     es1 = dcmplx(eswi, - rhoef *(rhos - rhob)/(two*pi*f*esof*rhos*vmc)) 
  else
     es1 = dcmplx(eswi, zero)
  endif
  es2 = dcmplx(eswo - eswi,zero)/dcmplx(one, f*tauw)
  esw = es1 + es2
  esm = one + (ess**alpha - one)*rhob/rhos + vmc**beta*esw**alpha - vmc       
  esm = esm**(one/alpha)
  if(imag(esm) .ge.zero) esm = dcmplx(real(esm), -0.0001_r_kind)
  return
end subroutine soil_diel
!        
subroutine snow_diel(frequency,ep_real,ep_imag,rad,frac,ep_eff)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    snow_diel   compute dielectric constant of snow
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
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
!$$$

!  use kinds, only: r_kind
!  use constants, only: zero, one, two, pi
  implicit none
  real(r_kind) ep_imag,ep_real
  real(r_kind) frequency,rad,frac,k0,yr,yi
  complex(r_kind)  y,ep_r,ep_i,ep_eff,fracy
  k0 = two*pi/(300.0_r_kind/frequency)
  yr  = (ep_real - one)/(ep_real + two)
  yi =   ep_imag/(ep_real + two)
  y = dcmplx(yr, yi)
  fracy=frac*y
  ep_r = (one + two*fracy)/(one - fracy)
  ep_i = two*fracy*y*(k0*rad)**3*(one-frac)**4/((one-fracy)**2*(one+two*frac)**2)
  ep_eff = ep_r - dcmplx(zero,one)*ep_i
  if (imag(ep_eff).ge.zero) ep_eff = dcmplx(real(ep_eff), -0.0001_r_kind)
  return 
end subroutine snow_diel

subroutine canopy_diel(frequency,mg,esv)

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   canopy_diel compute the dielectric constant of the vegetation canopy
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the dielectric constant of the vegetation canopy
!     geomatrical optics approximation for vegetation canopy
!     work for horizontal leaves
!
! program history log:
!   2004-09-22  todling - replaced zsqrt by general interface sqrt
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
!$$$

!  use kinds, only: r_kind
!  use constants, only: zero, one
  implicit none
  real(r_kind)  frequency,  mg, en, vf, vb
  complex(r_kind)  esv, xx
  en = 1.7_r_kind - (0.74_r_kind - 6.16_r_kind*mg)*mg
  vf = mg*(0.55_r_kind*mg - 0.076_r_kind)
  vb = 4.64_r_kind*mg*mg/( one + 7.36_r_kind*mg*mg)
  xx = dcmplx(zero,one)
  esv = en + vf*(4.9_r_kind + 75.0_r_kind/(one + xx*frequency/18.0_r_kind)-xx*(18.0_r_kind/frequency)) + &
       vb*(2.9_r_kind + 55.0_r_kind/(one + sqrt(xx*frequency/0.18_r_kind)))
  if (imag(esv).ge.zero) esv = dcmplx(real(esv), -0.0001_r_kind)
  return
end subroutine canopy_diel
subroutine reflectance(em1, em2, theta_i, theta_t, rvert, rh)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    reflectance compute the surface reflectivity
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the surface reflectivety using fresnel equations 
!    for a rough surface having a standard deviation of height of sigma  
!
! program history log:
!   2004-09-22  todling - replaced zsqrt by general interface sqrt; same for zabs/abs
!
! input argument list:
!      theta_i      -  incident angle (degree)
!      theta_t      -  transmitted angle (degree)
!      em1          -  dielectric constant of the medium 1
!      em2          -  dielectric constant of the medium 2
!
! output argument list:
!
!      rvert        -  reflectivity at vertical polarization
!      rh           -  reflectivity at horizontal polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind
!  use constants, only: zero
  implicit none
  real(r_kind) theta_i, theta_t
  real(r_kind) rh, rvert,cos_i,cos_t
  complex(r_kind) em1, em2, m1, m2, angle_i, angle_t
! compute the refractive index ratio between medium 2 and 1
! using dielectric constant (n = sqrt(e))
  cos_i=cos(theta_i)
  cos_t=cos(theta_t)
  angle_i = dcmplx(cos_i, zero)
  angle_t = dcmplx(cos_t, zero)
  m1 = sqrt(em1)
  m2 = sqrt(em2)
  rvert = (abs((m1*angle_t-m2*angle_i)/(m1*angle_t+m2*angle_i)))**2
  rh = (abs((m1*angle_i-m2*angle_t)/(m1*angle_i+m2*angle_t)))**2
  return 
end subroutine reflectance

subroutine transmitance(em1,em2,theta_i,theta_t,tv,th)

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    transmitance    calculate transmitance
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute transmitance 
!
! program history log:
!   2004-09-22  todling - replaced zsqrt by general interface sqrt; same for zabs/abs
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
!$$$

!  use kinds, only: r_kind
!  use constants, only: zero, two
  implicit none
  real(r_kind) theta_i, theta_t
!
  real(r_kind) th, tv, rr, cos_i,cos_t
!
  complex(r_kind) em1, em2, m1, m2, angle_i, angle_t
! compute the refractive index ratio between medium 2 and 1
! using dielectric constant (n = sqrt(e))
  cos_i=cos(theta_i)
  cos_t=cos(theta_t)
  angle_i = dcmplx(cos_i, zero)
  angle_t = dcmplx(cos_t, zero)
  m1 = sqrt(em1)
  m2 = sqrt(em2)
  rr = abs(m2/m1)*cos_t/cos_i
  tv =  rr*(abs(two*m1*angle_i/(m1*angle_t + m2*angle_i)))**2
  th =  rr*(abs(two*m1*angle_i/(m1*angle_i + m2*angle_t)))**2
  return
end subroutine transmitance

subroutine rough_reflectance(frequency,theta,sigma,rvert,rh)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rought_reflectance calculate surface relectivity      
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the surface reflectivety for a rough surface 
!    having a standard devoation of height of sigma  
!
!
! program history log:
!
! input argument list:
!
!      frequency    -  frequency (ghz)
!      theta        -  local zenith angle (degree)
!      sigma        -  standard deviation of rough surface height 
!                      smooth surface:0.38, medium: 1.10, rough:2.15 cm
!
!    internal variables
!
!
! output argument list:
!
!      rvert         -  reflectivity at vertical polarization
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
!$$$

!  use kinds, only: r_kind
!  use constants, only: one, two
  implicit none
  real(r_kind) theta, frequency
! real(r_kind) p, q, sigma, rh, rvert, rh_s, rv_s
  real(r_kind) p, q, rh, rvert, rh_s, rv_s, sigma
! rh_s = rh
! rv_s = rvert
  rh_s = 0.3_r_kind*rh
  rv_s = 0.3_r_kind*rvert
! p = 0.3_r_kind
  q = 0.35_r_kind*(one - exp(-0.60_r_kind*frequency*sigma**two))  
! rh = (q*rv_s + (one - q)*rh_s)*p
! rv = (q*rh_s + (one - q)*rv_s)*p
  rh = rh_s + q*(rv_s-rh_s)
  rvert = rv_s + q*(rh_s-rv_s)
  return 
end subroutine rough_reflectance

subroutine two_stream_solution(b,mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,r12_h, & 
     r12_v,r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,t12_v,t12_h,esv,esh)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    two_stream_solution
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: two stream solution
!
!   version: beta 
!
! program history log:
!
! input argument list:
!
!      b            -  scattering layer temperature (k)                  (gdas)
!      mu           -  cos(theta)
!      gv           -  asymmetry factor for v pol
!      gh           -  asymmetry factor for h pol
!      ssalb_v      -  single scattering albedo at v. polarization
!      ssalb_h      -  single scattering albedo at h. polarization
!      tau_v        -  optical depth at v. polarization           
!      tau_h        -  optical depth at h. polarization
!      r12_v        -  reflectivity at vertical polarization
!      r12_h        -  reflectivity at horizontal polarization
!      r21_v        -  reflectivity at vertical polarization
!      r21_h        -  reflectivity at horizontal polarization
!      r23_v        -  reflectivity at vertical polarization
!      r23_h        -  reflectivity at horizontal polarization
!      t21_v        -  transmisivity at vertical polarization
!      t21_h        -  transmisivity at horizontal polarization
!      t12_v        -  transmisivity at vertical polarization
!      t12_h        -  transmisivity at horizontal polarization
!
! output argument list:
!
!       esh         -  emissivity for horizontal polarization
!       esv         -  emissivity for vertical polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind
!  use constants, only: one, two
  implicit none
  real(r_kind) b, mu, gv, gh, ssalb_h, ssalb_v, tau_h,tau_v,r12_h, &
       r12_v, r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, t12_v, t12_h, esv, esh
! real(r_kind) esh0, esh1, esh2, esv0, esv1, esv2
! real(r_kind) esh1, esv1
  real(r_kind) alfa_v, alfa_h, kk_h, kk_v, gamma_h, gamma_v, beta_v, beta_h
  real(r_kind) fact1,fact2
  alfa_h = sqrt((one - ssalb_h)/(one - gh*ssalb_h))
  kk_h = sqrt ((one - ssalb_h)*(one -  gh*ssalb_h))/mu
  beta_h = (one - alfa_h)/(one + alfa_h)
  gamma_h = (beta_h -r23_h)/(one-beta_h*r23_h)
  alfa_v = sqrt((one-ssalb_v)/(one - gv*ssalb_v))
  kk_v = sqrt ((one-ssalb_v)*(one - gv*ssalb_v))/mu
  beta_v = (one - alfa_v)/(one + alfa_v)
  gamma_v = (beta_v -r23_v)/(one-beta_v*r23_v)
! esh0 = i0/b*r12_h
! esh0 = zero
! esh1 = (one - beta_h)*(one + gamma_h*exp(-two*kk_h*tau_h))
! esh1 = esh1/((one-beta_h*r21_h)-(beta_h-r21_h)*gamma_h*exp(-two*kk_h*tau_h))
! esh2 = i0/b*t12_h*(beta_h-gamma_h*exp(-two*kk_h*tau_h))
! esh2 = esh2 /((one-beta_h*r21_h)-(beta_h-r21_h)*gamma_h*exp(-two*kk_h*tau_h))
! esh2 = zero
! esv0 = i0/b*r12_v
! esv0 = zero
! esv1 = (one-beta_v)*(one+gamma_v*exp(-two*kk_v*tau_v))
! esv1 = esv1/((one-beta_v*r21_v)-(beta_v-r21_v)*gamma_v*exp(-two*kk_v*tau_v))
! esv2 = i0/b*t12_v*(beta_v - gamma_v*exp(-two*kk_v*tau_v))
! esv2 = esv2 /((one-beta_v*r21_v)-(beta_v-r21_v)*beta_v*exp(-two*kk_v*tau_v))
! esv2 = zero
! esh  = esh0 + t21_h*(esh1 + esh2)
! esv  = esv0 + t21_v*(esv1 + esv2)
  fact1=gamma_h*exp(-two*kk_h*tau_h)
  fact2=gamma_v*exp(-two*kk_v*tau_v)
  esh  = t21_h*(one - beta_h)*(one + fact1) &
       /(one-beta_h*r21_h-(beta_h-r21_h)*fact1)
  esv  = t21_v*(one - beta_v)*(one + fact2) &
       /(one-beta_v*r21_v-(beta_v-r21_v)*fact2)
  return
end subroutine two_stream_solution
