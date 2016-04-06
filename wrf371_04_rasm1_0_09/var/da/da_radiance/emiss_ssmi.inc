subroutine emiss_ssmi(ntype_index,theta,frequency,mv,veg_frac, veg_tp, soil_tp, &
     t_soil, t_skin, snow_depth, tbb, esh, esv)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: iceem_amsua  noaa/nesdis SSM/I emissivity model over snow/ice
!
!   prgmmr: Banghua Yan      org: nesdis              date: 2004-02-12
!           Fuzhong Weng
!
! abstract:
!    Please refer to the following paper for details
!    Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, submitted to TGRS
!
!   version: 4
!
! program history log:
!   2004-01-01  yan,b -  implement the algorithm for snow/ice emissivity
!   2004-02-01  yan,b -  modify the code for SSI
!   2004-07-23  okamoto - modify the code for GSI
!   2004-09-22  todling - using general interface sqrt instead of zsqrt
!
! input argument list:
!      ntype_index    -  surface type
!              1 : ocean
!              2 : sea ice
!              3 : snow over land
!              4 : non-snow land
!      theta       -  local zenith angle in radian
!      frequency   -  frequency in GHz
!      veg_frac    -  Vegetation fraction (0 - 1.0)   (GDAS)
!      veg_tp      -  Vegetation type              (GDAS, not used)
!                        1: Broadleave Evergreen Trees
!                        2: Broadleave Deciduous Trees
!                        3: Broad & Needle Mixed Forest
!                        4: Needleleave Evergreen Trees
!                        5: Needleleave Deciduous Trees
!                        6: Broadleave Tree with Groundcover (Savana)
!                        7: Groundcover Only (Perenial Groundcover)
!                        8: Broadleave Shrubs with Perenial Groundcover
!                        9: Broadleave Shrubs with Bare Soil
!                        10: Dwarf Trees & Shrubs with Bare Soil
!                        11: Bare Soil'
!                        12: Cultivations (use paramater 7)
!                        13: Glacial
!      soil_tp     -  Soil type        (GDAS, not used)
!                        1: Loamy Sand (coarse)
!                        2: Silty Clayloam (medium)
!                        3: Light Clay (fine)
!                        4: Sand Loam (coarse-medium)
!                        5: Sandy Clay (coarse-fine)
!                        6: Clay Loam (medium-fine)
!                        7: Sandy Clay loam (coarse-med-fine)
!                        8: Loam (organic)
!                        9: Ice (use loamy sand property)
!      t_soil      -  soil temperature (K)    (GDAS)
!      t_skin      -  scattering layer temperature (K)   (GDAS)
!      mv          -  volumetric moisture content in soil (0.0 - 1.0) (GDAS)
!      mg          -  gravimetric water content (0.0 - 1.0) (GDAS,not used)
!      snow_depth  -  scatter medium depth (mm?)        (GDAS)
!      tbb[1] ~ tbb[7]: brightness temperature at four SSM/I
!                 tbb[1] :  at 19.35 GHz  v-polarization
!                 tbb[2] :  at 19.35 GHz  h-polarization
!                 tbb[3] :  at 22.235 GHz v-polarization
!                 tbb[4] :  at 37 GHz     v-polarization
!                 tbb[5] :  at 37 GHz     h-polarization
!                 tbb[6] :  at 85 GHz     v-polarization
!                 tbb[7] :  at 85 GHz     h-polarization
!       When tbb[ ]  = -999.9, it means a missing value (no available data)
!
! output argument list:
!   em_vector : esv, esh
!      esv    : emissivity at vertical polarization
!      esh    : emissivity at horizontal polarization
!
! important internal variables:
!
!   rhob    -  bulk volume density of the soil (1.18-1.12)
!   rhos    -  density of the solids (2.65 g.cm^3 for solid soil material)
!   sand    -  sand fraction (sand + clay = 1.0)
!   clay    -  clay fraction
!   lai     -  leaf area index (eg. lai = 4.0 for corn leaves)
!   sigma   -  surface roughness formed between medium 1 and 2,
!                  expressed as he standard deviation of roughtness height (mm)
!   leaf_thick  -  leaf thickness (mm)
!   rad     -  radius of dense medium scatterers (mm)
!   va      -  fraction volume of dense medium scatterers(0.0-1.0)
!   slnt    -  salinity (per throusand)
!   ep      -  dielectric constant of ice or sand particles, complex value
!                               (e.g, 3.0+i0.0)
!
! remarks:
!
!  Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
!  use constants, only: zero, half, one
  implicit none
  
  real(r_kind):: rhob,rhos,sand,clay
  integer(i_kind)      :: nw,nwv,nwh
  parameter(rhob=1.18_r_kind,rhos=2.65_r_kind,sand=0.8_r_kind,clay=0.2_r_kind)
  parameter(nw=7,nwv=4,nwh=3)
  
  real(r_kind) theta,frequency,mv, mg, mge, veg_frac, veg_tp, soil_tp, &
       t_soil, t_skin, snow_depth, em_vector(2)
  real(r_kind) tbb(nw),tv(nwv),th(nwh)
  real(r_kind) t_scat, b, theta_i,theta_t, mu,  &
       r12_h, r12_v, r21_h, r21_v, r23_h, r23_v,t21_v, t21_h, t12_v, t12_h, &
       gv, gh, ssalb_h,ssalb_v,tau_h,tau_v, esh, esv,      &
       lai, leaf_thick, slnty, rad, sigma, va
  real(r_kind) ev_default,eh_default
  complex(r_kind) ep, esoil, eveg, eair, esand
  integer(i_kind) ntype_index
  
  data ev_default/0.9_r_kind/
  data eh_default/0.88_r_kind/
  
  esh  =  eh_default  ;  esv  =  ev_default
  eair = dcmplx(one, -one)
  t_scat = t_skin
  b = t_scat
!        write(UNIT=stdout,FMT='(a,i3,a,7f8.2)') 'emiss_ssmi: type=',ntype_index,' tbb=',tbb
  if (ntype_index .ne. 4) then
     tv(1) = tbb(1);  tv(2) = tbb(3);  tv(3) = tbb(4);  tv(4) = tbb(6)
     th(1) = tbb(2); th(2) = tbb(5);  th(3) = tbb(7)
     call ossmem(ntype_index, theta,frequency,t_skin,tv,th,em_vector)
     esh = em_vector(1) ;  esv = em_vector(2)
  else
     sigma = half
     lai = 3.0_r_kind*veg_frac + half
     mge = half*veg_frac
     leaf_thick = 0.07_r_kind
     slnty = 3.0_r_kind
     mu  = cos(theta)
     r12_h = zero;  r12_v = zero;  r21_h = zero; r21_v = zero
     t21_h = one; t21_v = one; t12_v = one; t12_h = one
     theta_i  = theta    ! in radian
     call soil_diel(frequency, t_soil, mv, rhob, rhos, sand, clay, esoil)
     theta_t = asin(real(sin(theta_i)*sqrt(eair)/sqrt(esoil)))
     call reflectance(eair, esoil, theta_i, theta_t, r23_v, r23_h)
     call rough_reflectance(frequency, theta_i, sigma, r23_v, r23_h)
     call canopy_diel(frequency,mge, eveg)
     call canopy_optic(lai, frequency, theta_i, eveg, leaf_thick, &
          gv, gh, ssalb_v, ssalb_h, tau_v, tau_h)
     call two_stream_solution( b, mu, gv, gh, ssalb_h,  ssalb_v,tau_h, tau_v, &
          r12_h, r12_v, r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, t12_v, t12_h,  &
          esv, esh)
  end if
  
  if(esh.gt.one) esh = one
  if(esh.lt.0.35_r_kind) esh = 0.35_r_kind
  if(esv.gt.one) esv = one
  if(esv.lt.0.35_r_kind) esv = 0.35_r_kind
  
  return
end subroutine emiss_ssmi
