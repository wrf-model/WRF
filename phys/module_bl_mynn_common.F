!====================================================================

 module module_bl_mynn_common

!------------------------------------------
!Define Model-specific constants/parameters.
!This module will be used at the initialization stage
!where all model-specific constants are read and saved into
!memory. This module is then used again in the MYNN-EDMF. All
!MYNN-specific constants are declared globally in the main
!module (module_bl_mynn) further below:
!------------------------------------------
!
! The following 5-6 lines are the only lines in this file that are not
! universal for all dycores... Any ideas how to universalize it?
! For MPAS:
! use mpas_kind_types,only: kind_phys => RKIND
! For CCPP:
!  use machine,  only : kind_phys
! For WRF
  use module_gfs_machine,  only : kind_phys

!WRF CONSTANTS
  use module_model_constants, only:         &
    & karman, g, p1000mb,                   &
    & cp, r_d, r_v, rcp, xlv, xlf, xls,     &
    & svp1, svp2, svp3, p608, ep_2, rvovrd, &                                                                         
    & cpv, cliq, cice, svpt0

 implicit none
 save
! save :: cp, cpv, cice, cliq, p608, karman, rcp, & !taken directly from module_model_constants
!         r_d, r_v, xls, xlv, xlf, rvovrd, ep_2,  & !taken directly from module_model_constants
!         p1000mb, svp1, svp2, svp3,              & !taken directly from module_model_constants
!         grav, t0c,                              & !renamed from module_model_constants
!         zero, half, one, two, onethird,         & !set here
!         twothirds, tref, tkmin, tice,           & !set here
!         ep_3, gtr, rk, tv0, tv1, xlscp, xlvcp,  & !derived here
!         g_inv                                     !derived here

! To be specified from dycore
! real:: cp           != 7.*r_d/2. (J/kg/K)
! real:: cpv          != 4.*r_v    (J/kg/K) Spec heat H2O gas
! real:: cice         != 2106.     (J/kg/K) Spec heat H2O ice
! real:: cliq         != 4190.     (J/kg/K) Spec heat H2O liq
! real:: p608         != R_v/R_d-1.
! real:: ep_2         != R_d/R_v
!! real:: grav         != accel due to gravity
! real:: karman       != von Karman constant
!! real:: t0c          != temperature of water at freezing, 273.15 K
! real:: rcp          != r_d/cp
! real:: r_d          != 287.  (J/kg/K) gas const dry air
! real:: r_v          != 461.6 (J/kg/K) gas const water
! real:: xlf          != 0.35E6 (J/kg) fusion at 0 C
! real:: xlv          != 2.50E6 (J/kg) vaporization at 0 C
! real:: xls          != 2.85E6 (J/kg) sublimation
! real:: rvovrd       != r_v/r_d != 1.608

! Specified locally
 real,parameter:: zero   = 0.0
 real,parameter:: half   = 0.5
 real,parameter:: one    = 1.0
 real,parameter:: two    = 2.0
 real,parameter:: onethird  = 1./3.
 real,parameter:: twothirds = 2./3.
 real,parameter:: tref  = 300.0   ! reference temperature (K)
 real,parameter:: TKmin = 253.0   ! for total water conversion, Tripoli and Cotton (1981)
! real,parameter:: p1000mb=100000.0
! real,parameter:: svp1  = 0.6112 !(kPa)
! real,parameter:: svp2  = 17.67  !(dimensionless)
! real,parameter:: svp3  = 29.65  !(K)
 real,parameter:: tice  = 240.0  !-33 (C), temp at saturation w.r.t. ice
 real,parameter:: grav  = g
 real,parameter:: t0c   = svpt0        != 273.15

! To be derived in the init routine
 real,parameter:: ep_3   = 1.-ep_2 != 0.378
 real,parameter:: gtr    = grav/tref
 real,parameter:: rk     = cp/r_d
 real,parameter:: tv0    =  p608*tref
 real,parameter:: tv1    = (1.+p608)*tref
 real,parameter:: xlscp  = (xlv+xlf)/cp
 real,parameter:: xlvcp  = xlv/cp
 real,parameter:: g_inv  = 1./grav

! grav   = g
! t0c    = svpt0        != 273.15
! ep_3   = 1.-ep_2      != 0.378                                                                                   
! gtr    = grav/tref
! rk     = cp/r_d
! tv0    = p608*tref
! tv1    = (1.+p608)*tref
! xlscp  = (xlv+xlf)/cp
! xlvcp  = xlv/cp
! g_inv  = 1./grav


 end module module_bl_mynn_common
