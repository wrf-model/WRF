!WRF:MODEL_LAYER:PHYSICS
!
! translated from NN f77 to F90 and put into WRF by Mariusz Pagowski
! NOAA/GSD & CIRA/CSU, Feb 2008
! changes to original code:
! 1. code is 1D (in z)
! 2. no advection of TKE, covariances and variances 
! 3. Cranck-Nicholson replaced with the implicit scheme
! 4. removed terrain dependent grid since input in WRF in actual
!    distances in z[m]
! 5. cosmetic changes to adhere to WRF standard (remove common blocks, 
!            intent etc)
!-------------------------------------------------------------------
!Modifications implemented by Joseph Olson and Jaymes Kenyon NOAA/GSD/MDB - CU/CIRES
!
! Departures from original MYNN (Nakanish & Niino 2009)
! 1. Addition of BouLac mixing length in the free atmosphere.
! 2. Changed the turbulent mixing length to be integrated from the
!    surface to the top of the BL + a transition layer depth.
! v3.4.1:    Option to use Kitamura/Canuto modification which removes 
!            the critical Richardson number and negative TKE (default).
!            Hybrid PBL height diagnostic, which blends a theta-v-based
!            definition in neutral/convective BL and a TKE-based definition
!            in stable conditions.
!            TKE budget output option (bl_mynn_tkebudget)
! v3.5.0:    TKE advection option (bl_mynn_tkeadvect)
! v3.5.1:    Fog deposition related changes.
! v3.6.0:    Removed fog deposition from the calculation of tendencies
!            Added mixing of qc, qi, qni
!            Added output for wstar, delta, TKE_PBL, & KPBL for correct 
!                   coupling to shcu schemes  
! v3.8.0:    Added subgrid scale cloud output for coupling to radiation
!            schemes (activated by setting icloud_bl =1 in phys namelist).
!            Added WRF_DEBUG prints (at level 3000)
!            Added Tripoli and Cotton (1981) correction.
!            Added namelist option bl_mynn_cloudmix to test effect of mixing
!                cloud species (default = 1: on). 
!            Added mass-flux option (bl_mynn_edmf, = 1 for DMP mass-flux, 0: off).
!                Related options: 
!                 bl_mynn_edmf_mom = 1 : activate momentum transport in MF scheme
!                 bl_mynn_edmf_tke = 1 : activate TKE transport in MF scheme
!            Added mixing length option (bl_mynn_mixlength, see notes below)
!            Added more sophisticated saturation checks, following Thompson scheme
!            Added new cloud PDF option (bl_mynn_cloudpdf = 2) from Chaboureau
!                and Bechtold (2002, JAS, with mods) 
!            Added capability to mix chemical species when env variable
!                WRF_CHEM = 1, thanks to Wayne Angevine.
!            Added scale-aware mixing length, following Junshi Ito's work
!                Ito et al. (2015, BLM).
! v3.9.0    Improvement to the mass-flux scheme (dynamic number of plumes,
!                better plume/cloud depth, significant speed up, better cloud
!                fraction). 
!            Added Stochastic Parameter Perturbation (SPP) implementation.
!            Many miscellaneous tweaks to the mixing lengths and stratus
!                component of the subgrid clouds.
! v.4.0      Removed or added alternatives to WRF-specific functions/modules
!                for the sake of portability to other models.
!                the sake of portability to other models.
!            Further refinement of mass-flux scheme from SCM experiments with
!                Wayne Angevine: switch to linear entrainment and back to
!                Simpson and Wiggert-type w-equation.
!            Addition of TKE production due to radiation cooling at top of 
!                clouds (proto-version); not activated by default.
!            Some code rewrites to move if-thens out of loops in an attempt to
!                improve computational efficiency.
!            New tridiagonal solver, which is supposedly 14% faster and more
!                conservative. Impact seems very small.
!            Many miscellaneous tweaks to the mixing lengths and stratus
!                component of the subgrid-scale (SGS) clouds.
! v4.1       Big improvements in downward SW radiation due to revision of subgrid clouds
!                - better cloud fraction and subgrid scale mixing ratios.
!                - may experience a small cool bias during the daytime now that high 
!                  SW-down bias is greatly reduced...
!            Some tweaks to increase the turbulent mixing during the daytime for
!                bl_mynn_mixlength option 2 to alleviate cool bias (very small impact).
!            Improved ensemble spread from changes to SPP in MYNN
!                - now perturbing eddy diffusivity and eddy viscosity directly
!                - now perturbing background rh (in SGS cloud calc only)
!                - now perturbing entrainment rates in mass-flux scheme
!            Added IF checks (within IFDEFS) to protect mixchem code from being used
!                when HRRR smoke is used (no impact on regular non-wrf chem use)
!            Important bug fix for wrf chem when transporting chemical species in MF scheme
!            Removed 2nd mass-flux scheme (no only bl_mynn_edmf = 1, no option 2)
!            Removed unused stochastic code for mass-flux scheme
!            Changed mass-flux scheme to be integrated on interface levels instead of
!                mass levels - impact is small
!            Added option to mix 2nd moments in MYNN as opposed to the scalar_pblmix option.
!                - activated with bl_mynn_mixscalars = 1; this sets scalar_pblmix = 0
!                - added tridagonal solver used in scalar_pblmix option to duplicate tendencies
!                - this alone changes the interface call considerably from v4.0.
!            Slight revision to TKE production due to radiation cooling at top of clouds
!            Added the non-Guassian buoyancy flux function of Bechtold and Siebesma (1998, JAS).
!                - improves TKE in SGS clouds
!            Added heating due to dissipation of TKE (small impact, maybe + 0.1 C daytime PBL temp)
!            Misc changes made for FV3/MPAS compatibility
! v4.2       A series of small tweaks to help reduce a cold bias in the PBL:
!                - slight increase in diffusion in convective conditions
!                - relaxed criteria for mass-flux activation/strength
!                - added capability to cycle TKE for continuity in hourly updating HRRR
!                - added effects of compensational environmental subsidence in mass-flux scheme,
!                  which resulted in tweaks to detrainment rates.
!            Bug fix for diagnostic-decay of SGS clouds - noticed by Greg Thompson. This has
!                a very small, but primarily  positive, impact on SW-down biases.
!            Tweak to calculation of KPBL - urged by Laura Fowler - to make more intuitive.
!            Tweak to temperature range of blending for saturation check (water to ice). This
!                slightly reduces excessive SGS clouds in polar region. No impact warm clouds. 
!            Added namelist option bl_mynn_output (0 or 1) to suppress or activate the
!                allocation and output of 10 3D variables. Most people will want this
!                set to 0 (default) to save memory and disk space.
!            Added new array qi_bl as opposed to using qc_bl for both SGS qc and qi. This
!                gives us more control of the magnitudes which can be confounded by using
!                a single array. As a results, many subroutines needed to be modified,
!                especially mym_condensation.
!            Added the blending of the stratus component of the SGS clouds to the mass-flux
!                clouds to account for situations where stratus and cumulus may exist in the
!                grid cell.
!            Misc small-impact bugfixes:
!                1) dz was incorrectly indexed in mym_condensation
!                2) configurations with icloud_bl = 0 were using uninitialized arrays
!
!            Many of these changes are now documented in Olson et al. (2019,
!                NOAA Technical Memorandum)
!
! For more explanation of some configuration options, see "JOE's mods" below:
!-------------------------------------------------------------------

MODULE module_bl_mynn

!==================================================================
!FV3 CONSTANTS
!       use physcons, only : cp     => con_cp,              &
!      &                     g      => con_g,               &
!      &                     r_d    => con_rd,              &
!      &                     r_v    => con_rv,              &
!      &                     cpv    => con_cvap,            &
!      &                     cliq   => con_cliq,            &
!      &                     Cice   => con_csol,            &
!      &                     rcp    => con_rocp,            &
!      &                     XLV    => con_hvap,            &
!      &                     XLF    => con_hfus,            &
!      &                     EP_1   => con_fvirt,           &
!      &                     EP_2   => con_eps
!
!  IMPLICIT NONE
!
!   REAL    , PARAMETER :: karman       = 0.4
!   REAL    , PARAMETER :: XLS          = 2.85E6
!   REAL    , PARAMETER :: p1000mb      = 100000.
!   REAL    , PARAMETER :: rvovrd       = r_v/r_d
!   REAL    , PARAMETER :: SVP1         = 0.6112
!   REAL    , PARAMETER :: SVP2         = 17.67
!   REAL    , PARAMETER :: SVP3         = 29.65
!   REAL    , PARAMETER :: SVPT0        = 273.15
!
!   INTEGER , PARAMETER :: param_first_scalar = 1, &
!       &                  p_qc = 2, &
!       &                  p_qr = 0, &
!       &                  p_qi = 2, &
!       &                  p_qs = 0, &
!       &                  p_qg = 0, &
!       &                  p_qnc= 0, &
!       &                  p_qni= 0
!
!END FV3 CONSTANTS
!====================================================================
!WRF CONSTANTS
  USE module_model_constants, only: &
       &karman, g, p1000mb, &
       &cp, r_d, r_v, rcp, xlv, xlf, xls, &
       &svp1, svp2, svp3, svpt0, ep_1, ep_2, rvovrd, &
       &cpv, cliq, cice

  USE module_state_description, only: param_first_scalar, &
       &p_qc, p_qr, p_qi, p_qs, p_qg, p_qnc, p_qni 

  IMPLICIT NONE

!END WRF CONSTANTS
!===================================================================
! From here on, these are used for any model
! The parameters below depend on stability functions of module_sf_mynn.
  REAL, PARAMETER :: cphm_st=5.0, cphm_unst=16.0, &
                     cphh_st=5.0, cphh_unst=16.0

  REAL, PARAMETER :: xlvcp=xlv/cp, xlscp=(xlv+xlf)/cp, ev=xlv, rd=r_d, &
       &rk=cp/rd, svp11=svp1*1.e3, p608=ep_1, ep_3=1.-ep_2

  REAL, PARAMETER :: tref=300.0     ! reference temperature (K)
  REAL, PARAMETER :: TKmin=253.0    ! for total water conversion, Tripoli and Cotton (1981)
  REAL, PARAMETER :: tv0=p608*tref, tv1=(1.+p608)*tref, gtr=g/tref

! Closure constants
  REAL, PARAMETER :: &
       &vk  = karman, &
       &pr  =  0.74,  &
       &g1  =  0.235, &  ! NN2009 = 0.235
       &b1  = 24.0, &
       &b2  = 15.0, &    ! CKmod     NN2009
       &c2  =  0.729, &  ! 0.729, & !0.75, &
       &c3  =  0.340, &  ! 0.340, & !0.352, &
       &c4  =  0.0, &
       &c5  =  0.2, &
       &a1  = b1*( 1.0-3.0*g1 )/6.0, &
!       &c1  = g1 -1.0/( 3.0*a1*b1**(1.0/3.0) ), &
       &c1  = g1 -1.0/( 3.0*a1*2.88449914061481660), &
       &a2  = a1*( g1-c1 )/( g1*pr ), &
       &g2  = b2/b1*( 1.0-c3 ) +2.0*a1/b1*( 3.0-2.0*c2 )

  REAL, PARAMETER :: &
       &cc2 =  1.0-c2, &
       &cc3 =  1.0-c3, &
       &e1c =  3.0*a2*b2*cc3, &
       &e2c =  9.0*a1*a2*cc2, &
       &e3c =  9.0*a2*a2*cc2*( 1.0-c5 ), &
       &e4c = 12.0*a1*a2*cc2, &
       &e5c =  6.0*a1*a1

! Constants for min tke in elt integration (qmin), max z/L in els (zmax), 
! and factor for eddy viscosity for TKE (Kq = Sqfac*Km):
  REAL, PARAMETER :: qmin=0.0, zmax=1.0, Sqfac=3.0
! Note that the following mixing-length constants are now specified in mym_length
!      &cns=3.5, alp1=0.23, alp2=0.3, alp3=3.0, alp4=10.0, alp5=0.4

! Constants for gravitational settling
!  REAL, PARAMETER :: gno=1.e6/(1.e8)**(2./3.), gpw=5./3., qcgmin=1.e-8
  REAL, PARAMETER :: gno=1.0  !original value seems too agressive: 4.64158883361278196
  REAL, PARAMETER :: gpw=5./3., qcgmin=1.e-8, qkemin=1.e-12

! Constants for cloud PDF (mym_condensation)
  REAL, PARAMETER :: rr2=0.7071068, rrp=0.3989423

! 'parameters' for Poisson distribution (EDMF scheme)
  REAL, PARAMETER  :: zero = 0.0, half = 0.5, one = 1.0, two = 2.0, &
                      onethird = 1./3., twothirds = 2./3.

  !Use Canuto/Kitamura mod (remove Ric and negative TKE) (1:yes, 0:no)
  !For more info, see Canuto et al. (2008 JAS) and Kitamura (Journal of the 
  !Meteorological Society of Japan, Vol. 88, No. 5, pp. 857-864, 2010).
  !Note that this change required further modification of other parameters
  !above (c2, c3). If you want to remove this option, set c2 and c3 constants 
  !(above) back to NN2009 values (see commented out lines next to the
  !parameters above). This only removes the negative TKE problem
  !but does not necessarily improve performance - neutral impact.
  REAL, PARAMETER :: CKmod=1.

  !Use Ito et al. (2015, BLM) scale-aware (0: no, 1: yes). Note that this also has impacts
  !on the cloud PDF and mass-flux scheme, using Honnert et al. (2011) similarity function
  !for TKE in the upper PBL/cloud layer.
  REAL, PARAMETER :: scaleaware=1.

  !Temporary switch to deactivate the mixing of chemical species (already done when WRF_CHEM = 1)
  INTEGER, PARAMETER :: bl_mynn_mixchem = 0

  !Adding top-down diffusion driven by cloud-top radiative cooling
  INTEGER, PARAMETER :: bl_mynn_topdown = 1

  !Option to activate heating due to dissipation of TKE (to activate, set to 1.0)
  REAL, PARAMETER :: dheat_opt = 1.

  !Option to activate environmental subsidence in mass-flux scheme
  LOGICAL, PARAMETER :: env_subs = .true.

  !option to print out more stuff for debugging purposes
  LOGICAL, PARAMETER :: debug_code = .false.

! JAYMES-
! Constants used for empirical calculations of saturation
! vapor pressures (in function "esat") and saturation mixing ratios
! (in function "qsat"), reproduced from module_mp_thompson.F, 
! v3.6 
  REAL, PARAMETER:: J0= .611583699E03
  REAL, PARAMETER:: J1= .444606896E02
  REAL, PARAMETER:: J2= .143177157E01
  REAL, PARAMETER:: J3= .264224321E-1
  REAL, PARAMETER:: J4= .299291081E-3
  REAL, PARAMETER:: J5= .203154182E-5
  REAL, PARAMETER:: J6= .702620698E-8
  REAL, PARAMETER:: J7= .379534310E-11
  REAL, PARAMETER:: J8=-.321582393E-13

  REAL, PARAMETER:: K0= .609868993E03
  REAL, PARAMETER:: K1= .499320233E02
  REAL, PARAMETER:: K2= .184672631E01
  REAL, PARAMETER:: K3= .402737184E-1
  REAL, PARAMETER:: K4= .565392987E-3
  REAL, PARAMETER:: K5= .521693933E-5
  REAL, PARAMETER:: K6= .307839583E-7
  REAL, PARAMETER:: K7= .105785160E-9
  REAL, PARAMETER:: K8= .161444444E-12
! end-

!JOE & JAYMES'S mods
!
! Mixing Length Options 
!   specifed through namelist:  bl_mynn_mixlength
!   added:  16 Apr 2015
!
! 0: Uses original MYNN mixing length formulation (except elt is calculated from 
!    a 10-km vertical integration).  No scale-awareness is applied to the master
!    mixing length (el), regardless of "scaleaware" setting. 
!
! 1 (*DEFAULT*): Instead of (0), uses BouLac mixing length in free atmosphere.  
!    This helps remove excessively large mixing in unstable layers aloft.  Scale-
!    awareness in dx is available via the "scaleaware" setting.  As of Apr 2015, 
!    this mixing length formulation option is used in the ESRL RAP/HRRR configuration.
!
! 2: As in (1), but elb is lengthened using separate cloud mixing length functions 
!    for statically stable and unstable regimes.  This elb adjustment is only 
!    possible for nonzero cloud fractions, such that cloud-free cells are treated 
!    as in (1), but BouLac calculation is used more sparingly - when elb > 500 m. 
!    This is to reduce the computational expense that comes with the BouLac calculation.
!    Also, This option is  scale-aware in dx if "scaleaware" = 1. (Following Ito et al. 2015). 
!
!JOE & JAYMES- end



  INTEGER :: mynn_level

  CHARACTER*128 :: mynn_message

  INTEGER, PARAMETER :: kdebug=27

CONTAINS

! **********************************************************************
! *   An improved Mellor-Yamada turbulence closure model               *
! *                                                                    *
! *                                   Aug/2005  M. Nakanishi (N.D.A)   *
! *                        Modified:  Dec/2005  M. Nakanishi (N.D.A)   *
! *                                             naka@nda.ac.jp         *
! *                                                                    *
! *   Contents:                                                        *
! *     1. mym_initialize  (to be called once initially)               *
! *        gives the closure constants and initializes the turbulent   *
! *        quantities.                                                 *
! *    (2) mym_level2      (called in the other subroutines)           *
! *        calculates the stability functions at Level 2.              *
! *    (3) mym_length      (called in the other subroutines)           *
! *        calculates the master length scale.                         *
! *     4. mym_turbulence                                              *
! *        calculates the vertical diffusivity coefficients and the    *
! *        production terms for the turbulent quantities.              *
! *     5. mym_predict                                                 *
! *        predicts the turbulent quantities at the next step.         *
! *     6. mym_condensation                                            *
! *        determines the liquid water content and the cloud fraction  *
! *        diagnostically.                                             *
! *                                                                    *
! *             call mym_initialize                                    *
! *                  |                                                 *
! *                  |<----------------+                               *
! *                  |                 |                               *
! *             call mym_condensation  |                               *
! *             call mym_turbulence    |                               *
! *             call mym_predict       |                               *
! *                  |                 |                               *
! *                  |-----------------+                               *
! *                  |                                                 *
! *                 end                                                *
! *                                                                    *
! *   Variables worthy of special mention:                             *
! *     tref   : Reference temperature                                 *
! *     thl    : Liquid water potential temperature                    *
! *     qw     : Total water (water vapor+liquid water) content        *
! *     ql     : Liquid water content                                  *
! *     vt, vq : Functions for computing the buoyancy flux             *
! *                                                                    *
! *     If the water contents are unnecessary, e.g., in the case of    *
! *     ocean models, thl is the potential temperature and qw, ql, vt  *
! *     and vq are all zero.                                           *
! *                                                                    *
! *   Grid arrangement:                                                *
! *             k+1 +---------+                                        *
! *                 |         |     i = 1 - nx                         *
! *             (k) |    *    |     j = 1 - ny                         *
! *                 |         |     k = 1 - nz                         *
! *              k  +---------+                                        *
! *                 i   (i)  i+1                                       *
! *                                                                    *
! *     All the predicted variables are defined at the center (*) of   *
! *     the grid boxes. The diffusivity coefficients are, however,     *
! *     defined on the walls of the grid boxes.                        *
! *     # Upper boundary values are given at k=nz.                     *
! *                                                                    *
! *   References:                                                      *
! *     1. Nakanishi, M., 2001:                                        *
! *        Boundary-Layer Meteor., 99, 349-378.                        *
! *     2. Nakanishi, M. and H. Niino, 2004:                           *
! *        Boundary-Layer Meteor., 112, 1-31.                          *
! *     3. Nakanishi, M. and H. Niino, 2006:                           *
! *        Boundary-Layer Meteor., (in press).                         *
! *     4. Nakanishi, M. and H. Niino, 2009:                           *
! *        Jour. Meteor. Soc. Japan, 87, 895-912.                      *
! **********************************************************************
!
!     SUBROUTINE  mym_initialize:
!
!     Input variables:
!       iniflag         : <>0; turbulent quantities will be initialized
!                         = 0; turbulent quantities have been already
!                              given, i.e., they will not be initialized
!       nx, ny, nz      : Dimension sizes of the
!                         x, y and z directions, respectively
!       tref            : Reference temperature                      (K)
!       dz(nz)        : Vertical grid spacings                     (m)
!                         # dz(nz)=dz(nz-1)
!       zw(nz+1)        : Heights of the walls of the grid boxes     (m)
!                         # zw(1)=0.0 and zw(k)=zw(k-1)+dz(k-1)
!       h(nx,ny)        : G^(1/2) in the terrain-following coordinate
!                         # h=1-zg/zt, where zg is the height of the
!                           terrain and zt the top of the model domain
!       pi0(nx,my,nz) : Exner function at zw*h+zg             (J/kg K)
!                         defined by c_p*( p_basic/1000hPa )^kappa
!                         This is usually computed by integrating
!                         d(pi0)/dz = -h*g/tref.
!       rmo(nx,ny)      : Inverse of the Obukhov length         (m^(-1))
!       flt, flq(nx,ny) : Turbulent fluxes of sensible and latent heat,
!                         respectively, e.g., flt=-u_*Theta_* (K m/s)
!! flt - liquid water potential temperature surface flux
!! flq - total water flux surface flux
!       ust(nx,ny)      : Friction velocity                        (m/s)
!       pmz(nx,ny)      : phi_m-zeta at z1*h+z0, where z1 (=0.5*dz(1))
!                         is the first grid point above the surafce, z0
!                         the roughness length and zeta=(z1*h+z0)*rmo
!       phh(nx,ny)      : phi_h at z1*h+z0
!       u, v(nx,nz,ny): Components of the horizontal wind        (m/s)
!       thl(nx,nz,ny)  : Liquid water potential temperature
!                                                                    (K)
!       qw(nx,nz,ny)  : Total water content Q_w                (kg/kg)
!
!     Output variables:
!       ql(nx,nz,ny)  : Liquid water content                   (kg/kg)
!       v?(nx,nz,ny)  : Functions for computing the buoyancy flux
!       qke(nx,nz,ny) : Twice the turbulent kinetic energy q^2
!                                                              (m^2/s^2)
!       tsq(nx,nz,ny) : Variance of Theta_l                      (K^2)
!       qsq(nx,nz,ny) : Variance of Q_w
!       cov(nx,nz,ny) : Covariance of Theta_l and Q_w              (K)
!       el(nx,nz,ny)  : Master length scale L                      (m)
!                         defined on the walls of the grid boxes
!
!     Work arrays:        see subroutine mym_level2
!       pd?(nx,nz,ny) : Half of the production terms at Level 2
!                         defined on the walls of the grid boxes
!       qkw(nx,nz,ny) : q on the walls of the grid boxes         (m/s)
!
!     # As to dtl, ...gh, see subroutine mym_turbulence.
!
!-------------------------------------------------------------------
  SUBROUTINE  mym_initialize (                                & 
       &            kts,kte,                                  &
       &            dz, zw,                                   &
       &            u, v, thl, qw,                            &
!       &            ust, rmo, pmz, phh, flt, flq,             &
       &            zi, theta, sh,                            &
       &            ust, rmo, el,                             &
       &            Qke, Tsq, Qsq, Cov, Psig_bl, cldfra_bl1D, &
       &            bl_mynn_mixlength,                        &
       &            edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf,    &
       &            INITIALIZE_QKE,                           &
       &            spp_pbl,rstoch_col)
!
!-------------------------------------------------------------------
    
    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: bl_mynn_mixlength,bl_mynn_edmf
    LOGICAL, INTENT(IN)   :: INITIALIZE_QKE
!    REAL, INTENT(IN)   :: ust, rmo, pmz, phh, flt, flq
    REAL, INTENT(IN)   :: ust, rmo, Psig_bl
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,cldfra_bl1D,&
                                          edmf_w1,edmf_a1,edmf_qc1
    REAL, DIMENSION(kts:kte), INTENT(out) :: tsq,qsq,cov
    REAL, DIMENSION(kts:kte), INTENT(inout) :: el,qke

    REAL, DIMENSION(kts:kte) :: &
         &ql,pdk,pdt,pdq,pdc,dtl,dqw,dtv,&
         &gm,gh,sm,sh,qkw,vt,vq
    INTEGER :: k,l,lmax
    REAL :: phm,vkz,elq,elv,b1l,b2l,pmz=1.,phh=1.,flt=0.,flq=0.,tmpq
    REAL :: zi
    REAL, DIMENSION(kts:kte) :: theta

    REAL, DIMENSION(kts:kte) :: rstoch_col
    INTEGER ::spp_pbl

!   **  At first ql, vt and vq are set to zero.  **
    DO k = kts,kte
       ql(k) = 0.0
       vt(k) = 0.0
       vq(k) = 0.0
    END DO
!
    CALL mym_level2 ( kts,kte,&
         &            dz,  &
         &            u, v, thl, qw, &
         &            ql, vt, vq, &
         &            dtl, dqw, dtv, gm, gh, sm, sh )
!
!   **  Preliminary setting  **

    el (kts) = 0.0
    IF (INITIALIZE_QKE) THEN
       !qke(kts) = ust**2 * ( b1*pmz )**(2.0/3.0)
       qke(kts) = 1.5 * ust**2 * ( b1*pmz )**(2.0/3.0)
       DO k = kts+1,kte
          !qke(k) = 0.0
          !linearly taper off towards top of pbl
          qke(k)=qke(kts)*MAX((ust*700. - zw(k))/(MAX(ust,0.01)*700.), 0.01)
       ENDDO
    ENDIF
!
    phm      = phh*b2 / ( b1*pmz )**(1.0/3.0)
    tsq(kts) = phm*( flt/ust )**2
    qsq(kts) = phm*( flq/ust )**2
    cov(kts) = phm*( flt/ust )*( flq/ust )
!
    DO k = kts+1,kte
       vkz = vk*zw(k)
       el (k) = vkz/( 1.0 + vkz/100.0 )
!       qke(k) = 0.0
!
       tsq(k) = 0.0
       qsq(k) = 0.0
       cov(k) = 0.0
    END DO
!
!   **  Initialization with an iterative manner          **
!   **  lmax is the iteration count. This is arbitrary.  **
    lmax = 5
!
    DO l = 1,lmax
!
       CALL mym_length (                     &
            &            kts,kte,            &
            &            dz, zw,             &
            &            rmo, flt, flq,      &
            &            vt, vq,             &
            &            u, v, qke,          &
            &            dtv,                &
            &            el,                 &
            &            zi,theta,           &
            &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength,&
            &            edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf)
!
       DO k = kts+1,kte
          elq = el(k)*qkw(k)
          pdk(k) = elq*( sm(k)*gm (k)+&
               &sh(k)*gh (k) )
          pdt(k) = elq*  sh(k)*dtl(k)**2
          pdq(k) = elq*  sh(k)*dqw(k)**2
          pdc(k) = elq*  sh(k)*dtl(k)*dqw(k)
       END DO
!
!   **  Strictly, vkz*h(i,j) -> vk*( 0.5*dz(1)*h(i,j)+z0 )  **
       vkz = vk*0.5*dz(kts)
       elv = 0.5*( el(kts+1)+el(kts) ) /  vkz
       IF (INITIALIZE_QKE)THEN 
          !qke(kts) = ust**2 * ( b1*pmz*elv    )**(2.0/3.0)
          qke(kts) = 1.0 * MAX(ust,0.02)**2 * ( b1*pmz*elv    )**(2.0/3.0) 
       ENDIF

       phm      = phh*b2 / ( b1*pmz/elv**2 )**(1.0/3.0)
       tsq(kts) = phm*( flt/ust )**2
       qsq(kts) = phm*( flq/ust )**2
       cov(kts) = phm*( flt/ust )*( flq/ust )

       DO k = kts+1,kte-1
          b1l = b1*0.25*( el(k+1)+el(k) )
          !tmpq=MAX(b1l*( pdk(k+1)+pdk(k) ),qkemin)
          !add MIN to limit unreasonable QKE
          tmpq=MIN(MAX(b1l*( pdk(k+1)+pdk(k) ),qkemin),125.)
!          PRINT *,'tmpqqqqq',tmpq,pdk(k+1),pdk(k)
          IF (INITIALIZE_QKE)THEN
             qke(k) = tmpq**0.666666666
          ENDIF

          IF ( qke(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*( b1l/b1 ) / SQRT( qke(k) )
          END IF

          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO

    END DO

!!    qke(kts)=qke(kts+1)
!!    tsq(kts)=tsq(kts+1)
!!    qsq(kts)=qsq(kts+1)
!!    cov(kts)=cov(kts+1)

    IF (INITIALIZE_QKE)THEN
       qke(kts)=0.5*(qke(kts)+qke(kts+1))
       qke(kte)=qke(kte-1)
    ENDIF
    tsq(kte)=tsq(kte-1)
    qsq(kte)=qsq(kte-1)
    cov(kte)=cov(kte-1)

!
!    RETURN

  END SUBROUTINE mym_initialize
  
!
! ==================================================================
!     SUBROUTINE  mym_level2:
!
!     Input variables:    see subroutine mym_initialize
!
!     Output variables:
!       dtl(nx,nz,ny) : Vertical gradient of Theta_l             (K/m)
!       dqw(nx,nz,ny) : Vertical gradient of Q_w
!       dtv(nx,nz,ny) : Vertical gradient of Theta_V             (K/m)
!       gm (nx,nz,ny) : G_M divided by L^2/q^2                (s^(-2))
!       gh (nx,nz,ny) : G_H divided by L^2/q^2                (s^(-2))
!       sm (nx,nz,ny) : Stability function for momentum, at Level 2
!       sh (nx,nz,ny) : Stability function for heat, at Level 2
!
!       These are defined on the walls of the grid boxes.
!
  SUBROUTINE  mym_level2 (kts,kte,&
       &            dz, &
       &            u, v, thl, qw, &
       &            ql, vt, vq, &
       &            dtl, dqw, dtv, gm, gh, sm, sh )
!
!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,ql,vt,vq

    REAL, DIMENSION(kts:kte), INTENT(out) :: &
         &dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: rfc,f1,f2,rf1,rf2,smc,shc,&
         &ri1,ri2,ri3,ri4,duz,dtz,dqz,vtt,vqq,dtq,dzk,afk,abk,ri,rf

    REAL ::   a2den

!    ev  = 2.5e6
!    tv0 = 0.61*tref
!    tv1 = 1.61*tref
!    gtr = 9.81/tref
!
    rfc = g1/( g1+g2 )
    f1  = b1*( g1-c1 ) +3.0*a2*( 1.0    -c2 )*( 1.0-c5 ) &
    &                   +2.0*a1*( 3.0-2.0*c2 )
    f2  = b1*( g1+g2 ) -3.0*a1*( 1.0    -c2 )
    rf1 = b1*( g1-c1 )/f1
    rf2 = b1*  g1     /f2
    smc = a1 /a2*  f1/f2
    shc = 3.0*a2*( g1+g2 )
!
    ri1 = 0.5/smc
    ri2 = rf1*smc
    ri3 = 4.0*rf2*smc -2.0*ri2
    ri4 = ri2**2
!
    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz =   duz                    /dzk**2
       dtz = ( thl(k)-thl(k-1) )/( dzk )
       dqz = ( qw(k)-qw(k-1) )/( dzk )
!
       vtt =  1.0 +vt(k)*abk +vt(k-1)*afk  ! Beta-theta in NN09, Eq. 39
       vqq =  tv0 +vq(k)*abk +vq(k-1)*afk  ! Beta-q
       dtq =  vtt*dtz +vqq*dqz
!
       dtl(k) =  dtz
       dqw(k) =  dqz
       dtv(k) =  dtq
!?      dtv(i,j,k) =  dtz +tv0*dqz
!?   :              +( ev/pi0(i,j,k)-tv1 )
!?   :              *( ql(i,j,k)-ql(i,j,k-1) )/( dzk*h(i,j) )
!
       gm (k) =  duz
       gh (k) = -dtq*gtr
!
!   **  Gradient Richardson number  **
       ri = -gh(k)/MAX( duz, 1.0e-10 )

    !a2den is needed for the Canuto/Kitamura mod
    IF (CKmod .eq. 1) THEN
       a2den = 1. + MAX(ri,0.0)
    ELSE
       a2den = 1. + 0.0
    ENDIF

       rfc = g1/( g1+g2 )
       f1  = b1*( g1-c1 ) +3.0*(a2/a2den)*( 1.0    -c2 )*( 1.0-c5 ) &
    &                     +2.0*a1*( 3.0-2.0*c2 )
       f2  = b1*( g1+g2 ) -3.0*a1*( 1.0    -c2 )
       rf1 = b1*( g1-c1 )/f1
       rf2 = b1*  g1     /f2
       smc = a1 /(a2/a2den)*  f1/f2
       shc = 3.0*(a2/a2den)*( g1+g2 )

       ri1 = 0.5/smc
       ri2 = rf1*smc
       ri3 = 4.0*rf2*smc -2.0*ri2
       ri4 = ri2**2

!   **  Flux Richardson number  **
       rf = MIN( ri1*( ri+ri2-SQRT(ri**2-ri3*ri+ri4) ), rfc )
!
       sh (k) = shc*( rfc-rf )/( 1.0-rf )
       sm (k) = smc*( rf1-rf )/( rf2-rf ) * sh(k)
    END DO
!
!    RETURN

#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mym_level2

! ==================================================================
!     SUBROUTINE  mym_length:
!
!     Input variables:    see subroutine mym_initialize
!
!     Output variables:   see subroutine mym_initialize
!
!     Work arrays:
!       elt(nx,ny)      : Length scale depending on the PBL depth    (m)
!       vsc(nx,ny)      : Velocity scale q_c                       (m/s)
!                         at first, used for computing elt
!
!     NOTE: the mixing lengths are meant to be calculated at the full-
!           sigmal levels (or interfaces beween the model layers).
!
  SUBROUTINE  mym_length (                     & 
    &            kts,kte,                      &
    &            dz, zw,                       &
    &            rmo, flt, flq,                &
    &            vt, vq,                       &
    &            u1, v1, qke,                  &
    &            dtv,                          &
    &            el,                           &
    &            zi,theta,                     &
    &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength,&
    &            edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf)
    
!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    INTEGER, INTENT(IN)   :: bl_mynn_mixlength,bl_mynn_edmf
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl
    REAL, DIMENSION(kts:kte), INTENT(IN)   :: u1,v1,qke,vt,vq,cldfra_bl1D,&
                                          edmf_w1,edmf_a1,edmf_qc1
    REAL, DIMENSION(kts:kte), INTENT(out)  :: qkw, el
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dtv

    REAL :: elt,vsc

    REAL, DIMENSION(kts:kte), INTENT(IN) :: theta
    REAL, DIMENSION(kts:kte) :: qtke,elBLmin,elBLavg,thetaw
    REAL :: wt,wt2,zi,zi2,h1,h2,hs,elBLmin0,elBLavg0,cldavg

    ! THE FOLLOWING CONSTANTS ARE IMPORTANT FOR REGULATING THE
    ! MIXING LENGTHS:
    REAL :: cns,   &   ! for surface layer (els) in stable conditions
            alp1,  &   ! for turbulent length scale (elt)
            alp2,  &   ! for buoyancy length scale (elb)
            alp3,  &   ! for buoyancy enhancement factor of elb
            alp4,  &   ! for surface layer (els) in unstable conditions
            alp5,  &   ! for BouLac mixing length or above PBLH
            alp6       ! for mass-flux/

    !THE FOLLOWING LIMITS DO NOT DIRECTLY AFFECT THE ACTUAL PBLH.
    !THEY ONLY IMPOSE LIMITS ON THE CALCULATION OF THE MIXING LENGTH 
    !SCALES SO THAT THE BOULAC MIXING LENGTH (IN FREE ATMOS) DOES
    !NOT ENCROACH UPON THE BOUNDARY LAYER MIXING LENGTH (els, elb & elt).
    REAL, PARAMETER :: minzi = 300.  !min mixed-layer height
    REAL, PARAMETER :: maxdz = 750.  !max (half) transition layer depth
                                     !=0.3*2500 m PBLH, so the transition
                                     !layer stops growing for PBLHs > 2.5 km.
    REAL, PARAMETER :: mindz = 300.  !300  !min (half) transition layer depth

    !SURFACE LAYER LENGTH SCALE MODS TO REDUCE IMPACT IN UPPER BOUNDARY LAYER
    REAL, PARAMETER :: ZSLH = 100. ! Max height correlated to surface conditions (m)
    REAL, PARAMETER :: CSL = 2.    ! CSL = constant of proportionality to L O(1)
    REAL :: z_m


    INTEGER :: i,j,k
    REAL :: afk,abk,zwk,zwk1,dzk,qdz,vflx,bv,tau_cloud,elb,els,els1,elf, &
            & el_stab,el_unstab,el_mf,el_stab_mf,elb_mf,PBLH_PLUS_ENT,   &
            & Uonset,Ugrid,el_les

!    tv0 = 0.61*tref
!    gtr = 9.81/tref

    SELECT CASE(bl_mynn_mixlength)

      CASE (0) ! ORIGINAL MYNN MIXING LENGTH

        cns  = 2.7
        alp1 = 0.23
        alp2 = 1.0
        alp3 = 5.0
        alp4 = 100.
        alp5 = 0.4

        ! Impose limits on the height integration for elt and the transition layer depth
        zi2  = MIN(10000.,zw(kte-2))  !originally integrated to model top, not just 10 km.
        h1=MAX(0.3*zi2,mindz)
        h1=MIN(h1,maxdz)         ! 1/2 transition layer depth
        h2=h1/2.0                ! 1/4 transition layer depth

        qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))
        DO k = kts+1,kte
           afk = dz(k)/( dz(k)+dz(k-1) )
           abk = 1.0 -afk
           qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
        END DO

        elt = 1.0e-5
        vsc = 1.0e-5        

        !   **  Strictly, zwk*h(i,j) -> ( zwk*h(i,j)+z0 )  **
        k = kts+1
        zwk = zw(k)
        DO WHILE (zwk .LE. zi2+h1)
           dzk = 0.5*( dz(k)+dz(k-1) )
           qdz = MAX( qkw(k)-qmin, 0.03 )*dzk
           elt = elt +qdz*zwk
           vsc = vsc +qdz
           k   = k+1
           zwk = zw(k)
        END DO

        elt =  alp1*elt/vsc
        vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
        vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(1.0/3.0)

        !   **  Strictly, el(i,j,1) is not zero.  **
        el(kts) = 0.0
        zwk1    = zw(kts+1)

        DO k = kts+1,kte
           zwk = zw(k)              !full-sigma levels

           !   **  Length scale limited by the buoyancy effect  **
           IF ( dtv(k) .GT. 0.0 ) THEN
              bv  = SQRT( gtr*dtv(k) )
              elb = alp2*qkw(k) / bv &
                  &       *( 1.0 + alp3/alp2*&
                  &SQRT( vsc/( bv*elt ) ) )
              elf = alp2 * qkw(k)/bv

           ELSE
              elb = 1.0e10
              elf = elb
           ENDIF

           z_m = MAX(0.,zwk - 4.)

           !   **  Length scale in the surface layer  **
           IF ( rmo .GT. 0.0 ) THEN
              els  = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
              els1 = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax ))
           ELSE
              els  =  vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
              els1 =  vk*z_m*( 1.0 - alp4* zwk*rmo )**0.2
           END IF

           !   ** HARMONC AVERGING OF MIXING LENGTH SCALES:
           !       el(k) =      MIN(elb/( elb/elt+elb/els+1.0 ),elf)
           !       el(k) =      elb/( elb/elt+elb/els+1.0 )

           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5

           el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)

        END DO

      CASE (1) !OPERATIONAL FORM OF MIXING LENGTH

        cns  = 2.3
        alp1 = 0.23
        alp2 = 0.65
        alp3 = 3.0
        alp4 = 20.
        alp5 = 0.4

        ! Impose limits on the height integration for elt and the transition layer depth
        zi2=MAX(zi,minzi)
        h1=MAX(0.3*zi2,mindz)
        h1=MIN(h1,maxdz)         ! 1/2 transition layer depth
        h2=h1/2.0                ! 1/4 transition layer depth

        qtke(kts)=MAX(qke(kts)/2.,0.01) !tke at full sigma levels
        thetaw(kts)=theta(kts)          !theta at full-sigma levels
        qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))

        DO k = kts+1,kte
           afk = dz(k)/( dz(k)+dz(k-1) )
           abk = 1.0 -afk
           qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
           qtke(k) = (qkw(k)**2.)/2.    ! q -> TKE
           thetaw(k)= theta(k)*abk + theta(k-1)*afk
        END DO

        elt = 1.0e-5
        vsc = 1.0e-5

        !   **  Strictly, zwk*h(i,j) -> ( zwk*h(i,j)+z0 )  **
        k = kts+1
        zwk = zw(k)
        DO WHILE (zwk .LE. zi2+h1)
           dzk = 0.5*( dz(k)+dz(k-1) )
           qdz = MAX( qkw(k)-qmin, 0.03 )*dzk
           elt = elt +qdz*zwk
           vsc = vsc +qdz
           k   = k+1
           zwk = zw(k)
        END DO

        elt =  alp1*elt/vsc
        vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
        vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(1.0/3.0)

        !   **  Strictly, el(i,j,1) is not zero.  **
        el(kts) = 0.0
        zwk1    = zw(kts+1)              !full-sigma levels

        ! COMPUTE BouLac mixing length
        CALL boulac_length(kts,kte,zw,dz,qtke,thetaw,elBLmin,elBLavg)

        DO k = kts+1,kte
           zwk = zw(k)              !full-sigma levels

           !   **  Length scale limited by the buoyancy effect  **
           IF ( dtv(k) .GT. 0.0 ) THEN
              bv  = SQRT( gtr*dtv(k) ) 
              elb = alp2*qkw(k) / bv &                ! formulation,
                  &       *( 1.0 + alp3/alp2*&       ! except keep
                  &SQRT( vsc/( bv*elt ) ) )          ! elb bounded by
              elb = MIN(elb, zwk)                     ! zwk
              elf = alp2 * qkw(k)/bv
           ELSE
              elb = 1.0e10
              elf = elb
           ENDIF

           z_m = MAX(0.,zwk - 4.)

           !   **  Length scale in the surface layer  **
           IF ( rmo .GT. 0.0 ) THEN
              els  = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
              els1 = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax ))
           ELSE
              els  =  vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
              els1 =  vk*z_m*( 1.0 - alp4* zwk*rmo )**0.2
           END IF

           !   ** NOW BLEND THE MIXING LENGTH SCALES:
           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5

           !add blending to use BouLac mixing length in free atmos;
           !defined relative to the PBLH (zi) + transition layer (h1)
           el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
           el(k) = el(k)*(1.-wt) + alp5*elBLmin(k)*wt

           ! include scale-awareness, except for original MYNN
           el(k) = el(k)*Psig_bl

         END DO

      CASE (2) !Experimental mixing length formulation

        Uonset = 2.5 + dz(kts)*0.1
        Ugrid  = sqrt(u1(kts)**2 + v1(kts)**2)
        cns  = 3.5 * (1.0 - MIN(MAX(Ugrid - Uonset, 0.0)/10.0, 1.0))
        alp1 = 0.23
        alp2 = 0.30
        alp3 = 2.0
        alp4 = 20.  !10.
        alp5 = alp2 !like alp2, but for free atmosphere
        alp6 = 50.0 !used for MF mixing length

        ! Impose limits on the height integration for elt and the transition layer depth
        !zi2=MAX(zi,minzi)
        zi2=MAX(zi,    100.)
        h1=MAX(0.3*zi2,mindz)
        h1=MIN(h1,maxdz)         ! 1/2 transition layer depth
        h2=h1*0.5                ! 1/4 transition layer depth

        qtke(kts)=MAX(0.5*qke(kts),0.01) !tke at full sigma levels
        qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))

        DO k = kts+1,kte
           afk = dz(k)/( dz(k)+dz(k-1) )
           abk = 1.0 -afk
           qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
           qtke(k) = 0.5*qkw(k)  ! qkw -> TKE
        END DO

        elt = 1.0e-5
        vsc = 1.0e-5

        !   **  Strictly, zwk*h(i,j) -> ( zwk*h(i,j)+z0 )  **
        PBLH_PLUS_ENT = MAX(zi+h1, 100.)
        k = kts+1
        zwk = zw(k)
        DO WHILE (zwk .LE. PBLH_PLUS_ENT)
           dzk = 0.5*( dz(k)+dz(k-1) )
           qdz = MAX( qkw(k)-qmin, 0.03 )*dzk  !consider reducing 0.3
           elt = elt +qdz*zwk
           vsc = vsc +qdz
           k   = k+1
           zwk = zw(k)
        END DO

        elt =  MAX(alp1*elt/vsc, 10.)
        vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
        vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(0.33333)

        !   **  Strictly, el(i,j,1) is not zero.  **
        el(kts) = 0.0
        zwk1    = zw(kts+1)

        DO k = kts+1,kte
           zwk = zw(k)              !full-sigma levels
           cldavg = 0.5*(cldfra_bl1D(k-1)+cldfra_bl1D(k))

           !   **  Length scale limited by the buoyancy effect  **
           IF ( dtv(k) .GT. 0.0 ) THEN
              bv  = SQRT( gtr*dtv(k) )
              !elb_mf = alp2*qkw(k) / bv  &
              elb_mf = MAX(alp2*qkw(k),  &
!                  &MAX(1.-0.5*cldavg,0.0)**0.5 * alp6*edmf_a1(k)*edmf_w1(k)) / bv  &
                  & alp6*edmf_a1(k)*edmf_w1(k)) / bv  &
                  &  *( 1.0 + alp3*SQRT( vsc/( bv*elt ) ) )
              elb = MIN(alp5*qkw(k)/bv, zwk)
              elf = elb/(1. + (elb/600.))  !bound free-atmos mixing length to < 600 m.
              !IF (zwk > zi .AND. elf > 400.) THEN
              !   ! COMPUTE BouLac mixing length
              !   !CALL boulac_length0(k,kts,kte,zw,dz,qtke,thetaw,elBLmin0,elBLavg0)
              !   !elf = alp5*elBLavg0
              !   elf = MIN(MAX(50.*SQRT(qtke(k)), 400.), zwk)
              !ENDIF

           ELSE
              ! use version in development for RAP/HRRR 2016
              ! JAYMES-
              ! tau_cloud is an eddy turnover timescale;
              ! see Teixeira and Cheinet (2004), Eq. 1, and
              ! Cheinet and Teixeira (2003), Eq. 7.  The
              ! coefficient 0.5 is tuneable. Expression in
              ! denominator is identical to vsc (a convective
              ! velocity scale), except that elt is relpaced
              ! by zi, and zero is replaced by 1.0e-4 to
              ! prevent division by zero.
              tau_cloud = MIN(MAX(0.5*zi/((gtr*zi*MAX(flt,1.0e-4))**(0.3333)),50.),150.)
              !minimize influence of surface heat flux on tau far away from the PBLH.
              wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
              tau_cloud = tau_cloud*(1.-wt) + 50.*wt

              elb = MIN(tau_cloud*SQRT(MIN(qtke(k),30.)), zwk)
              elf = elb
              elb_mf = elb
         END IF

         z_m = MAX(0.,zwk - 4.)

         !   **  Length scale in the surface layer  **
         IF ( rmo .GT. 0.0 ) THEN
            els  = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
            els1 = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax ))
         ELSE
            els  =  vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
            els1 =  vk*z_m*( 1.0 - alp4* zwk*rmo )**0.2
         END IF

         !   ** NOW BLEND THE MIXING LENGTH SCALES:
         wt=.5*TANH((zwk - (zi2+h1))/h2) + .5

         ! "el_unstab" = blended els-elt
         el_unstab = els/(1. + (els1/elt))
         el(k) = MIN(el_unstab, elb_mf)
         el(k) = el(k)*(1.-wt) + elf*wt

         ! include scale-awareness. For now, use simple asymptotic kz -> 12 m.
         el_les= MIN(els/(1. + (els1/12.)), elb_mf)
         el(k) = el(k)*Psig_bl + (1.-Psig_bl)*el_les

       END DO

    END SELECT


#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mym_length

! ==================================================================
  SUBROUTINE boulac_length0(k,kts,kte,zw,dz,qtke,theta,lb1,lb2)
!
!    NOTE: This subroutine was taken from the BouLac scheme in WRF-ARW
!          and modified for integration into the MYNN PBL scheme.
!          WHILE loops were added to reduce the computational expense.
!          This subroutine computes the length scales up and down
!          and then computes the min, average of the up/down
!          length scales, and also considers the distance to the
!          surface.
!
!      dlu = the distance a parcel can be lifted upwards give a finite
!            amount of TKE.
!      dld = the distance a parcel can be displaced downwards given a
!            finite amount of TKE.
!      lb1 = the minimum of the length up and length down
!      lb2 = the average of the length up and length down
!-------------------------------------------------------------------

     INTEGER, INTENT(IN) :: k,kts,kte
     REAL, DIMENSION(kts:kte), INTENT(IN) :: qtke,dz,theta
     REAL, INTENT(OUT) :: lb1,lb2
     REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw

     !LOCAL VARS
     INTEGER :: izz, found
     REAL :: dlu,dld
     REAL :: dzt, zup, beta, zup_inf, bbb, tl, zdo, zdo_sup, zzz


     !----------------------------------
     ! FIND DISTANCE UPWARD             
     !----------------------------------
     zup=0.
     dlu=zw(kte+1)-zw(k)-dz(k)/2.
     zzz=0.
     zup_inf=0.
     beta=g/theta(k)           !Buoyancy coefficient

     !print*,"FINDING Dup, k=",k," zw=",zw(k)

     if (k .lt. kte) then      !cant integrate upwards from highest level
        found = 0
        izz=k
        DO WHILE (found .EQ. 0)

           if (izz .lt. kte) then
              dzt=dz(izz)                    ! layer depth above
              zup=zup-beta*theta(k)*dzt     ! initial PE the parcel has at k
              !print*,"  ",k,izz,theta(izz),dz(izz)
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt/2. ! PE gained by lifting a parcel to izz+1
              zzz=zzz+dzt                   ! depth of layer k to izz+1
              !print*,"  PE=",zup," TKE=",qtke(k)," z=",zw(izz)
              if (qtke(k).lt.zup .and. qtke(k).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    !fractional distance up into the layer where TKE becomes < PE
                    tl=(-beta*(theta(izz)-theta(k)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(k)))**2. + &
                      &       2.*bbb*beta*(qtke(k)-zup_inf))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(k))then
                       tl=(qtke(k)-zup_inf)/(beta*(theta(izz)-theta(k)))
                    else
                       tl=0.
                    endif
                 endif
                 dlu=zzz-dzt+tl
                 !print*,"  FOUND Dup:",dlu," z=",zw(izz)," tl=",tl
                 found =1
              endif
              zup_inf=zup
              izz=izz+1
           ELSE
              found = 1
           ENDIF

        ENDDO

     endif

     !----------------------------------
     ! FIND DISTANCE DOWN               
     !----------------------------------
     zdo=0.
     zdo_sup=0.
     dld=zw(k)
     zzz=0.

     !print*,"FINDING Ddown, k=",k," zwk=",zw(k)
     if (k .gt. kts) then  !cant integrate downwards from lowest level

        found = 0
        izz=k
        DO WHILE (found .EQ. 0)

           if (izz .gt. kts) then
              dzt=dz(izz-1)
              zdo=zdo+beta*theta(k)*dzt
              !print*,"  ",k,izz,theta(izz),dz(izz-1)
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              !print*,"  PE=",zdo," TKE=",qtke(k)," z=",zw(izz)
              if (qtke(k).lt.zdo .and. qtke(k).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(k))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(k)))**2. + &
                      &       2.*bbb*beta*(qtke(k)-zdo_sup))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(k)) then
                       tl=(qtke(k)-zdo_sup)/(beta*(theta(izz)-theta(k)))
                    else
                       tl=0.
                    endif
                 endif
                 dld=zzz-dzt+tl
                 !print*,"  FOUND Ddown:",dld," z=",zw(izz)," tl=",tl
                 found = 1
              endif
              zdo_sup=zdo
              izz=izz-1
           ELSE
              found = 1
           ENDIF
        ENDDO

     endif

     !----------------------------------
     ! GET MINIMUM (OR AVERAGE)         
     !----------------------------------
     !The surface layer length scale can exceed z for large z/L,
     !so keep maximum distance down > z.
     dld = min(dld,zw(k+1))!not used in PBL anyway, only free atmos
     lb1 = min(dlu,dld)     !minimum
     !JOE-fight floating point errors
     dlu=MAX(0.1,MIN(dlu,1000.))
     dld=MAX(0.1,MIN(dld,1000.))
     lb2 = sqrt(dlu*dld)    !average - biased towards smallest
     !lb2 = 0.5*(dlu+dld)   !average

     if (k .eq. kte) then
        lb1 = 0.
        lb2 = 0.
     endif
     !print*,"IN MYNN-BouLac",k,lb1
     !print*,"IN MYNN-BouLac",k,dld,dlu

  END SUBROUTINE boulac_length0

! ==================================================================
  SUBROUTINE boulac_length(kts,kte,zw,dz,qtke,theta,lb1,lb2)
!
!    NOTE: This subroutine was taken from the BouLac scheme in WRF-ARW
!          and modified for integration into the MYNN PBL scheme.
!          WHILE loops were added to reduce the computational expense.
!          This subroutine computes the length scales up and down
!          and then computes the min, average of the up/down
!          length scales, and also considers the distance to the
!          surface.
!
!      dlu = the distance a parcel can be lifted upwards give a finite 
!            amount of TKE.
!      dld = the distance a parcel can be displaced downwards given a
!            finite amount of TKE.
!      lb1 = the minimum of the length up and length down
!      lb2 = the average of the length up and length down
!-------------------------------------------------------------------

     INTEGER, INTENT(IN) :: kts,kte
     REAL, DIMENSION(kts:kte), INTENT(IN) :: qtke,dz,theta
     REAL, DIMENSION(kts:kte), INTENT(OUT) :: lb1,lb2
     REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw

     !LOCAL VARS
     INTEGER :: iz, izz, found
     REAL, DIMENSION(kts:kte) :: dlu,dld
     REAL, PARAMETER :: Lmax=2000.  !soft limit
     REAL :: dzt, zup, beta, zup_inf, bbb, tl, zdo, zdo_sup, zzz

     !print*,"IN MYNN-BouLac",kts, kte

     do iz=kts,kte

        !----------------------------------
        ! FIND DISTANCE UPWARD
        !----------------------------------
        zup=0.
        dlu(iz)=zw(kte+1)-zw(iz)-dz(iz)/2.
        zzz=0.
        zup_inf=0.
        beta=g/theta(iz)           !Buoyancy coefficient

        !print*,"FINDING Dup, k=",iz," zw=",zw(iz)

        if (iz .lt. kte) then      !cant integrate upwards from highest level

          found = 0
          izz=iz       
          DO WHILE (found .EQ. 0) 

            if (izz .lt. kte) then
              dzt=dz(izz)                    ! layer depth above 
              zup=zup-beta*theta(iz)*dzt     ! initial PE the parcel has at iz
              !print*,"  ",iz,izz,theta(izz),dz(izz)
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt/2. ! PE gained by lifting a parcel to izz+1
              zzz=zzz+dzt                   ! depth of layer iz to izz+1
              !print*,"  PE=",zup," TKE=",qtke(iz)," z=",zw(izz)
              if (qtke(iz).lt.zup .and. qtke(iz).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    !fractional distance up into the layer where TKE becomes < PE
                    tl=(-beta*(theta(izz)-theta(iz)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2. + &
                      &       2.*bbb*beta*(qtke(iz)-zup_inf))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(iz))then
                       tl=(qtke(iz)-zup_inf)/(beta*(theta(izz)-theta(iz)))
                    else
                       tl=0.
                    endif
                 endif            
                 dlu(iz)=zzz-dzt+tl
                 !print*,"  FOUND Dup:",dlu(iz)," z=",zw(izz)," tl=",tl
                 found =1
              endif
              zup_inf=zup
              izz=izz+1
             ELSE
              found = 1
            ENDIF

          ENDDO

        endif
                   
        !----------------------------------
        ! FIND DISTANCE DOWN
        !----------------------------------
        zdo=0.
        zdo_sup=0.
        dld(iz)=zw(iz)
        zzz=0.

        !print*,"FINDING Ddown, k=",iz," zwk=",zw(iz)
        if (iz .gt. kts) then  !cant integrate downwards from lowest level

          found = 0
          izz=iz       
          DO WHILE (found .EQ. 0) 

            if (izz .gt. kts) then
              dzt=dz(izz-1)
              zdo=zdo+beta*theta(iz)*dzt
              !print*,"  ",iz,izz,theta(izz),dz(izz-1)
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt/2.
              zzz=zzz+dzt
              !print*,"  PE=",zdo," TKE=",qtke(iz)," z=",zw(izz)
              if (qtke(iz).lt.zdo .and. qtke(iz).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(iz))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2. + &
                      &       2.*bbb*beta*(qtke(iz)-zdo_sup))))/bbb/beta
                 else
                    if (theta(izz) .ne. theta(iz)) then
                       tl=(qtke(iz)-zdo_sup)/(beta*(theta(izz)-theta(iz)))
                    else
                       tl=0.
                    endif
                 endif            
                 dld(iz)=zzz-dzt+tl
                 !print*,"  FOUND Ddown:",dld(iz)," z=",zw(izz)," tl=",tl
                 found = 1
              endif
              zdo_sup=zdo
              izz=izz-1
            ELSE
              found = 1
            ENDIF
          ENDDO

        endif

        !----------------------------------
        ! GET MINIMUM (OR AVERAGE)
        !----------------------------------
        !The surface layer length scale can exceed z for large z/L,
        !so keep maximum distance down > z.
        dld(iz) = min(dld(iz),zw(iz+1))!not used in PBL anyway, only free atmos
        lb1(iz) = min(dlu(iz),dld(iz))     !minimum
        !JOE-fight floating point errors
        dlu(iz)=MAX(0.1,MIN(dlu(iz),1000.))
        dld(iz)=MAX(0.1,MIN(dld(iz),1000.))
        lb2(iz) = sqrt(dlu(iz)*dld(iz))    !average - biased towards smallest
        !lb2(iz) = 0.5*(dlu(iz)+dld(iz))   !average

        !Apply soft limit (only impacts very large lb; lb=100 by 5%, lb=500 by 20%).
        lb1(iz) = lb1(iz)/(1. + (lb1(iz)/Lmax))
        lb2(iz) = lb2(iz)/(1. + (lb2(iz)/Lmax))
 
        if (iz .eq. kte) then
           lb1(kte) = lb1(kte-1)
           lb2(kte) = lb2(kte-1)
        endif
        !print*,"IN MYNN-BouLac",kts, kte,lb1(iz)
        !print*,"IN MYNN-BouLac",iz,dld(iz),dlu(iz)

     ENDDO
                   
  END SUBROUTINE boulac_length
!
! ==================================================================
!     SUBROUTINE  mym_turbulence:
!
!     Input variables:    see subroutine mym_initialize
!       levflag         : <>3;  Level 2.5
!                         = 3;  Level 3
!
!     # ql, vt, vq, qke, tsq, qsq and cov are changed to input variables.
!
!     Output variables:   see subroutine mym_initialize
!       dfm(nx,nz,ny) : Diffusivity coefficient for momentum,
!                         divided by dz (not dz*h(i,j))            (m/s)
!       dfh(nx,nz,ny) : Diffusivity coefficient for heat,
!                         divided by dz (not dz*h(i,j))            (m/s)
!       dfq(nx,nz,ny) : Diffusivity coefficient for q^2,
!                         divided by dz (not dz*h(i,j))            (m/s)
!       tcd(nx,nz,ny)   : Countergradient diffusion term for Theta_l
!                                                                  (K/s)
!       qcd(nx,nz,ny)   : Countergradient diffusion term for Q_w
!                                                              (kg/kg s)
!       pd?(nx,nz,ny) : Half of the production terms
!
!       Only tcd and qcd are defined at the center of the grid boxes
!
!     # DO NOT forget that tcd and qcd are added on the right-hand side
!       of the equations for Theta_l and Q_w, respectively.
!
!     Work arrays:        see subroutine mym_initialize and level2
!
!     # dtl, dqw, dtv, gm and gh are allowed to share storage units with
!       dfm, dfh, dfq, tcd and qcd, respectively, for saving memory.
!
  SUBROUTINE  mym_turbulence (                                &
    &            kts,kte,                                     &
    &            levflag,                                     &
    &            dz, zw,                                      &
    &            u, v, thl, ql, qw,                           &
    &            qke, tsq, qsq, cov,                          &
    &            vt, vq,                                      &
    &            rmo, flt, flq,                               &
    &            zi,theta,                                    &
    &            sh,                                          &
    &            El,                                          &
    &            Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, &
    &		 qWT1D,qSHEAR1D,qBUOY1D,qDISS1D,              &
    &            bl_mynn_tkebudget,                           &
    &            Psig_bl,Psig_shcu,cldfra_bl1D,bl_mynn_mixlength,&
    &            edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf,       &
    &            TKEprodTD,                                   &
    &            spp_pbl,rstoch_col)

!-------------------------------------------------------------------
!
    INTEGER, INTENT(IN)   :: kts,kte

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    INTEGER, INTENT(IN)   :: levflag,bl_mynn_mixlength,bl_mynn_edmf
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl,Psig_shcu
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,& 
         &ql,vt,vq,qke,tsq,qsq,cov,cldfra_bl1D,edmf_w1,edmf_a1,edmf_qc1,&
         &TKEprodTD

    REAL, DIMENSION(kts:kte), INTENT(out) :: dfm,dfh,dfq,&
         &pdk,pdt,pdq,pdc,tcd,qcd,el

    REAL, DIMENSION(kts:kte), INTENT(inout) :: &
         qWT1D,qSHEAR1D,qBUOY1D,qDISS1D
    REAL :: q3sq_old,dlsq1,qWTP_old,qWTP_new
    REAL :: dudz,dvdz,dTdz,&
            upwp,vpwp,Tpwp
    INTEGER, INTENT(in) :: bl_mynn_tkebudget

    REAL, DIMENSION(kts:kte) :: qkw,dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k
!    REAL :: cc2,cc3,e1c,e2c,e3c,e4c,e5c
    REAL :: e6c,dzk,afk,abk,vtt,vqq,&
         &cw25,clow,cupp,gamt,gamq,smd,gamv,elq,elh

    REAL :: zi, cldavg
    REAL, DIMENSION(kts:kte), INTENT(in) :: theta

    REAL ::  a2den, duz, ri, HLmod  !JOE-Canuto/Kitamura mod
!JOE-stability criteria for cw
    REAL:: auh,aum,adh,adm,aeh,aem,Req,Rsl,Rsl2
!JOE-end

    DOUBLE PRECISION  q2sq, t2sq, r2sq, c2sq, elsq, gmel, ghel
    DOUBLE PRECISION  q3sq, t3sq, r3sq, c3sq, dlsq, qdiv
    DOUBLE PRECISION  e1, e2, e3, e4, enum, eden, wden

!   Stochastic
    INTEGER,  INTENT(IN)                          ::    spp_pbl
    REAL, DIMENSION(KTS:KTE)                      ::    rstoch_col
    REAL :: prlimit


!
!    tv0 = 0.61*tref
!    gtr = 9.81/tref
!
!    cc2 =  1.0-c2
!    cc3 =  1.0-c3
!    e1c =  3.0*a2*b2*cc3
!    e2c =  9.0*a1*a2*cc2
!    e3c =  9.0*a2*a2*cc2*( 1.0-c5 )
!    e4c = 12.0*a1*a2*cc2
!    e5c =  6.0*a1*a1
!

    CALL mym_level2 (kts,kte,&
    &            dz, &
    &            u, v, thl, qw, &
    &            ql, vt, vq, &
    &            dtl, dqw, dtv, gm, gh, sm, sh )
!
    CALL mym_length (                           &
    &            kts,kte,                       &
    &            dz, zw,                        &
    &            rmo, flt, flq,                 &
    &            vt, vq,                        &
    &            u, v, qke,                     &
    &            dtv,                           &
    &            el,                            &
    &            zi,theta,                      &
    &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength, &
    &            edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf )
!

    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       elsq = el (k)**2
       q2sq = b1*elsq*( sm(k)*gm(k)+sh(k)*gh(k) )
       q3sq = qkw(k)**2

!JOE-Canuto/Kitamura mod
       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz =   duz                    /dzk**2
       !   **  Gradient Richardson number  **
       ri = -gh(k)/MAX( duz, 1.0e-10 )
       IF (CKmod .eq. 1) THEN
          a2den = 1. + MAX(ri,0.0)
       ELSE
          a2den = 1. + 0.0
       ENDIF
!JOE-end
!
!  Modified: Dec/22/2005, from here, (dlsq -> elsq)
       gmel = gm (k)*elsq
       ghel = gh (k)*elsq
!  Modified: Dec/22/2005, up to here

       ! Level 2.0 debug prints
       IF ( debug_code ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
           print*,"MYNN; mym_turbulence2.0; sh=",sh(k)," k=",k
           print*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           print*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           print*," qke=",qke(k)," el=",el(k)," ri=",ri
           print*," PBLH=",zi," u=",u(k)," v=",v(k)
         ENDIF
       ENDIF

!JOE-Apply Helfand & Labraga stability check for all Ric
!      when CKmod == 1. (currently not forced below)
       IF (CKmod .eq. 1) THEN
          HLmod = q2sq -1.
       ELSE
          HLmod = q3sq
       ENDIF

!     **  Since qkw is set to more than 0.0, q3sq > 0.0.  **

!JOE-test new stability criteria in level 2.5 (as well as level 3) - little/no impact
!     **  Limitation on q, instead of L/q  **
          dlsq =  elsq
          IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)
!JOE-end

       IF ( q3sq .LT. q2sq ) THEN
       !IF ( HLmod .LT. q2sq ) THEN
          !Apply Helfand & Labraga mod
          qdiv = SQRT( q3sq/q2sq )   !HL89: (1-alfa)
          sm(k) = sm(k) * qdiv
          sh(k) = sh(k) * qdiv
!
          !JOE-Canuto/Kitamura mod
          !e1   = q3sq - e1c*ghel * qdiv**2
          !e2   = q3sq - e2c*ghel * qdiv**2
          !e3   = e1   + e3c*ghel * qdiv**2
          !e4   = e1   - e4c*ghel * qdiv**2
          e1   = q3sq - e1c*ghel/a2den * qdiv**2
          e2   = q3sq - e2c*ghel/a2den * qdiv**2
          e3   = e1   + e3c*ghel/(a2den**2) * qdiv**2
          e4   = e1   - e4c*ghel/a2den * qdiv**2
          eden = e2*e4 + e3*e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
       ELSE
          !JOE-Canuto/Kitamura mod
          !e1   = q3sq - e1c*ghel
          !e2   = q3sq - e2c*ghel
          !e3   = e1   + e3c*ghel
          !e4   = e1   - e4c*ghel
          e1   = q3sq - e1c*ghel/a2den
          e2   = q3sq - e2c*ghel/a2den
          e3   = e1   + e3c*ghel/(a2den**2)
          e4   = e1   - e4c*ghel/a2den
          eden = e2*e4 + e3*e5c*gmel
          eden = MAX( eden, 1.0d-20 )

          qdiv = 1.0
          sm(k) = q3sq*a1*( e3-3.0*c1*e4       )/eden
          !JOE-Canuto/Kitamura mod
          !sh(k) = q3sq*a2*( e2+3.0*c1*e5c*gmel )/eden
          sh(k) = q3sq*(a2/a2den)*( e2+3.0*c1*e5c*gmel )/eden
       END IF !end Helfand & Labraga check

       !JOE: Level 2.5 debug prints
       ! HL88 , lev2.5 criteria from eqs. 3.17, 3.19, & 3.20
       IF ( debug_code ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0 .OR. &
           sh(k) > 0.76*b2 .or. (sm(k)**2*gm(k) .gt. .44**2)) THEN
           print*,"MYNN; mym_turbulence2.5; sh=",sh(k)," k=",k
           print*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           print*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           print*," qke=",qke(k)," el=",el(k)," ri=",ri
           print*," PBLH=",zi," u=",u(k)," v=",v(k)
         ENDIF
       ENDIF

!   **  Level 3 : start  **
       IF ( levflag .EQ. 3 ) THEN
          t2sq = qdiv*b2*elsq*sh(k)*dtl(k)**2
          r2sq = qdiv*b2*elsq*sh(k)*dqw(k)**2
          c2sq = qdiv*b2*elsq*sh(k)*dtl(k)*dqw(k)
          t3sq = MAX( tsq(k)*abk+tsq(k-1)*afk, 0.0 )
          r3sq = MAX( qsq(k)*abk+qsq(k-1)*afk, 0.0 )
          c3sq =      cov(k)*abk+cov(k-1)*afk

!  Modified: Dec/22/2005, from here
          c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )
!
          vtt  = 1.0 +vt(k)*abk +vt(k-1)*afk
          vqq  = tv0 +vq(k)*abk +vq(k-1)*afk
          t2sq = vtt*t2sq +vqq*c2sq
          r2sq = vtt*c2sq +vqq*r2sq
          c2sq = MAX( vtt*t2sq+vqq*r2sq, 0.0d0 )
          t3sq = vtt*t3sq +vqq*c3sq
          r3sq = vtt*c3sq +vqq*r3sq
          c3sq = MAX( vtt*t3sq+vqq*r3sq, 0.0d0 )
!
          cw25 = e1*( e2 + 3.0*c1*e5c*gmel*qdiv**2 )/( 3.0*eden )
!
!     **  Limitation on q, instead of L/q  **
          dlsq =  elsq
          IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)
!
!     **  Limitation on c3sq (0.12 =< cw =< 0.76) **
          !JOE: use Janjic's (2001; p 13-17) methodology (eqs 4.11-414 and 5.7-5.10)
          ! to calculate an exact limit for c3sq:
          auh = 27.*a1*((a2/a2den)**2)*b2*(g/tref)**2
          aum = 54.*(a1**2)*(a2/a2den)*b2*c1*(g/tref)
          adh = 9.*a1*((a2/a2den)**2)*(12.*a1 + 3.*b2)*(g/tref)**2
          adm = 18.*(a1**2)*(a2/a2den)*(b2 - 3.*(a2/a2den))*(g/tref)

          aeh = (9.*a1*((a2/a2den)**2)*b1 +9.*a1*((a2/a2den)**2)* &
                (12.*a1 + 3.*b2))*(g/tref)
          aem = 3.*a1*(a2/a2den)*b1*(3.*(a2/a2den) + 3.*b2*c1 + &
                (18.*a1*c1 - b2)) + &
                (18.)*(a1**2)*(a2/a2den)*(b2 - 3.*(a2/a2den))

          Req = -aeh/aem
          Rsl = (auh + aum*Req)/(3.*adh + 3.*adm*Req)
          !For now, use default values, since tests showed little/no sensitivity
          Rsl = .12             !lower limit
          Rsl2= 1.0 - 2.*Rsl    !upper limit
          !IF (k==2)print*,"Dynamic limit RSL=",Rsl
          !IF (Rsl < 0.10 .OR. Rsl > 0.18) THEN
          !   print*,'--- ERROR: MYNN: Dynamic Cw '// &
          !        'limit exceeds reasonable limits'
          !   print*," MYNN: Dynamic Cw limit needs attention=",Rsl
          !ENDIF

          !JOE-Canuto/Kitamura mod
          !e2   = q3sq - e2c*ghel * qdiv**2
          !e3   = q3sq + e3c*ghel * qdiv**2
          !e4   = q3sq - e4c*ghel * qdiv**2
          e2   = q3sq - e2c*ghel/a2den * qdiv**2
          e3   = q3sq + e3c*ghel/(a2den**2) * qdiv**2
          e4   = q3sq - e4c*ghel/a2den * qdiv**2
          eden = e2*e4  + e3 *e5c*gmel * qdiv**2

          !JOE-Canuto/Kitamura mod
          !wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
          !     &        *( e2*e4c - e3c*e5c*gmel * qdiv**2 )
          wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
               &        *( e2*e4c/a2den - e3c*e5c*gmel/(a2den**2) * qdiv**2 )

          IF ( wden .NE. 0.0 ) THEN
             !JOE: test dynamic limits
             !clow = q3sq*( 0.12-cw25 )*eden/wden
             !cupp = q3sq*( 0.76-cw25 )*eden/wden
             clow = q3sq*( Rsl -cw25 )*eden/wden
             cupp = q3sq*( Rsl2-cw25 )*eden/wden
!
             IF ( wden .GT. 0.0 ) THEN
                c3sq  = MIN( MAX( c3sq, c2sq+clow ), c2sq+cupp )
             ELSE
                c3sq  = MAX( MIN( c3sq, c2sq+clow ), c2sq+cupp )
             END IF
          END IF
!
          e1   = e2 + e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
!  Modified: Dec/22/2005, up to here

          !JOE-Canuto/Kitamura mod
          !e6c  = 3.0*a2*cc3*gtr * dlsq/elsq
          e6c  = 3.0*(a2/a2den)*cc3*gtr * dlsq/elsq

          !============================
          !     **  for Gamma_theta  **
          !!          enum = qdiv*e6c*( t3sq-t2sq )
          IF ( t2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ENDIF
          gamt =-e1  *enum    /eden

          !============================
          !     **  for Gamma_q  **
          !!          enum = qdiv*e6c*( r3sq-r2sq )
          IF ( r2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ENDIF
          gamq =-e1  *enum    /eden

          !============================
          !     **  for Sm' and Sh'd(Theta_V)/dz  **
          !!          enum = qdiv*e6c*( c3sq-c2sq )
          enum = MAX( qdiv*e6c*( c3sq-c2sq ), 0.0d0)

          !JOE-Canuto/Kitamura mod
          !smd  = dlsq*enum*gtr/eden * qdiv**2 * (e3c+e4c)*a1/a2
          smd  = dlsq*enum*gtr/eden * qdiv**2 * (e3c/(a2den**2) + &
               & e4c/a2den)*a1/(a2/a2den)

          gamv = e1  *enum*gtr/eden
          sm(k) = sm(k) +smd

          !============================
          !     **  For elh (see below), qdiv at Level 3 is reset to 1.0.  **
          qdiv = 1.0

          ! Level 3 debug prints
          IF ( debug_code ) THEN
            IF (sh(k)<-0.3 .OR. sm(k)<-0.3 .OR. &
              qke(k) < -0.1 .or. ABS(smd) .gt. 2.0) THEN
              print*," MYNN; mym_turbulence3.0; sh=",sh(k)," k=",k
              print*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
              print*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
              print*," qke=",qke(k)," el=",el(k)," ri=",ri
              print*," PBLH=",zi," u=",u(k)," v=",v(k)
            ENDIF
          ENDIF

!   **  Level 3 : end  **

       ELSE
!     **  At Level 2.5, qdiv is not reset.  **
          gamt = 0.0
          gamq = 0.0
          gamv = 0.0
       END IF
!
!      Add stochastic perturbation of prandtl number limit
       if (spp_pbl==1) then
          prlimit = MIN(MAX(1.,2.5 + 5.0*rstoch_col(k)), 10.)
          IF(sm(k) > sh(k)*Prlimit) THEN
             sm(k) = sh(k)*Prlimit
          ENDIF
       ENDIF
!
!      Add min background stability function (diffusivity) within model levels
!      with active plumes and low cloud fractions.
       cldavg = 0.5*(cldfra_bl1D(k-1) + cldfra_bl1D(k))
       IF (edmf_a1(k) > 0.001 .OR. cldavg > 0.02) THEN
           cldavg = 0.5*(cldfra_bl1D(k-1) + cldfra_bl1D(k))
           !sm(k) = MAX(sm(k), MAX(1.0 - 2.0*cldavg, 0.0)**0.33 * 0.03 * &                           
           !  &     MIN(10.*edmf_a1(k)*edmf_w1(k),1.0) )
           !sh(k) = MAX(sh(k), MAX(1.0 - 2.0*cldavg, 0.0)**0.33 * 0.03 * &                           
           !  &     MIN(10.*edmf_a1(k)*edmf_w1(k),1.0) )

           ! for mass-flux columns
           sm(k) = MAX(sm(k), 0.03*MIN(10.*edmf_a1(k)*edmf_w1(k),1.0) )
           sh(k) = MAX(sh(k), 0.03*MIN(10.*edmf_a1(k)*edmf_w1(k),1.0) )
           ! for clouds
           sm(k) = MAX(sm(k), 0.03*MIN(cldavg,1.0) )
           sh(k) = MAX(sh(k), 0.03*MIN(cldavg,1.0) )

       ENDIF
!
       elq = el(k)*qkw(k)
       elh = elq*qdiv

       ! Production of TKE (pdk), T-variance (pdt),
       ! q-variance (pdq), and covariance (pdc)
       pdk(k) = elq*( sm(k)*gm(k) &
            &                    +sh(k)*gh(k)+gamv ) + & ! JAYMES TKE
            &   TKEprodTD(k)                             ! JOE-top-down
       pdt(k) = elh*( sh(k)*dtl(k)+gamt )*dtl(k)
       pdq(k) = elh*( sh(k)*dqw(k)+gamq )*dqw(k)
       pdc(k) = elh*( sh(k)*dtl(k)+gamt )&
            &*dqw(k)*0.5 &
                  &+elh*( sh(k)*dqw(k)+gamq )*dtl(k)*0.5

       ! Contergradient terms
       tcd(k) = elq*gamt
       qcd(k) = elq*gamq

       ! Eddy Diffusivity/Viscosity divided by dz
       dfm(k) = elq*sm(k) / dzk
       dfh(k) = elq*sh(k) / dzk
!  Modified: Dec/22/2005, from here
!   **  In sub.mym_predict, dfq for the TKE and scalar variance **
!   **  are set to 3.0*dfm and 1.0*dfm, respectively. (Sqfac)   **
       dfq(k) =     dfm(k)
!  Modified: Dec/22/2005, up to here

   IF ( bl_mynn_tkebudget == 1) THEN
       !TKE BUDGET
       dudz = ( u(k)-u(k-1) )/dzk
       dvdz = ( v(k)-v(k-1) )/dzk
       dTdz = ( thl(k)-thl(k-1) )/dzk

       upwp = -elq*sm(k)*dudz
       vpwp = -elq*sm(k)*dvdz
       Tpwp = -elq*sh(k)*dTdz
       Tpwp = SIGN(MAX(ABS(Tpwp),1.E-6),Tpwp)

       IF ( k .EQ. kts+1 ) THEN
          qWT1D(kts)=0.
          q3sq_old =0.
          qWTP_old =0.
          !**  Limitation on q, instead of L/q  **
          dlsq1 = MAX(el(kts)**2,1.0)
          IF ( q3sq_old/dlsq1 .LT. -gh(k) ) q3sq_old = -dlsq1*gh(k)
       ENDIF

       !!!Vertical Transport Term
       qWTP_new = elq*Sqfac*sm(k)*(q3sq - q3sq_old)/dzk
       qWT1D(k) = 0.5*(qWTP_new - qWTP_old)/dzk
       qWTP_old = elq*Sqfac*sm(k)*(q3sq - q3sq_old)/dzk
       q3sq_old = q3sq

       !!!Shear Term
       !!!qSHEAR1D(k)=-(upwp*dudz + vpwp*dvdz)
       qSHEAR1D(k) = elq*sm(k)*gm(k)

       !!!Buoyancy Term    
       !!!qBUOY1D(k)=g*Tpwp/thl(k)
       !qBUOY1D(k)= elq*(sh(k)*gh(k) + gamv)
       qBUOY1D(k) = elq*(sh(k)*(-dTdz*g/thl(k)) + gamv)

       !!!Dissipation Term
       qDISS1D(k) = (q3sq**(3./2.))/(b1*MAX(el(k),1.))
    ENDIF

    END DO
!

    dfm(kts) = 0.0
    dfh(kts) = 0.0
    dfq(kts) = 0.0
    tcd(kts) = 0.0
    qcd(kts) = 0.0

    tcd(kte) = 0.0
    qcd(kte) = 0.0

!
    DO k = kts,kte-1
       dzk = dz(k)
       tcd(k) = ( tcd(k+1)-tcd(k) )/( dzk )
       qcd(k) = ( qcd(k+1)-qcd(k) )/( dzk )
    END DO
!

   IF ( bl_mynn_tkebudget == 1) THEN
      !JOE-TKE BUDGET
      qWT1D(kts)=0.
      qSHEAR1D(kts)=qSHEAR1D(kts+1)
      qBUOY1D(kts)=qBUOY1D(kts+1)
      qDISS1D(kts)=qDISS1D(kts+1)
   ENDIF

    if (spp_pbl==1) then
       DO k = kts,kte
          dfm(k)= dfm(k) + dfm(k)* rstoch_col(k) * 1.5 * MAX(exp(-MAX(zw(k)-8000.,0.0)/2000.),0.001)
          dfh(k)= dfh(k) + dfh(k)* rstoch_col(k) * 1.5 * MAX(exp(-MAX(zw(k)-8000.,0.0)/2000.),0.001)
       END DO
    endif

!    RETURN
#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mym_turbulence

! ==================================================================
!     SUBROUTINE  mym_predict:
!
!     Input variables:    see subroutine mym_initialize and turbulence
!       qke(nx,nz,ny) : qke at (n)th time level
!       tsq, ...cov     : ditto
!
!     Output variables:
!       qke(nx,nz,ny) : qke at (n+1)th time level
!       tsq, ...cov     : ditto
!
!     Work arrays:
!       qkw(nx,nz,ny)   : q at the center of the grid boxes        (m/s)
!       bp (nx,nz,ny)   : = 1/2*F,     see below
!       rp (nx,nz,ny)   : = P-1/2*F*Q, see below
!
!     # The equation for a turbulent quantity Q can be expressed as
!          dQ/dt + Ah + Av = Dh + Dv + P - F*Q,                      (1)
!       where A is the advection, D the diffusion, P the production,
!       F*Q the dissipation and h and v denote horizontal and vertical,
!       respectively. If Q is q^2, F is 2q/B_1L.
!       Using the Crank-Nicholson scheme for Av, Dv and F*Q, a finite
!       difference equation is written as
!          Q{n+1} - Q{n} = dt  *( Dh{n}   - Ah{n}   + P{n} )
!                        + dt/2*( Dv{n}   - Av{n}   - F*Q{n}   )
!                        + dt/2*( Dv{n+1} - Av{n+1} - F*Q{n+1} ),    (2)
!       where n denotes the time level.
!       When the advection and diffusion terms are discretized as
!          dt/2*( Dv - Av ) = a(k)Q(k+1) - b(k)Q(k) + c(k)Q(k-1),    (3)
!       Eq.(2) can be rewritten as
!          - a(k)Q(k+1) + [ 1 + b(k) + dt/2*F ]Q(k) - c(k)Q(k-1)
!                 = Q{n} + dt  *( Dh{n}   - Ah{n}   + P{n} )
!                        + dt/2*( Dv{n}   - Av{n}   - F*Q{n}   ),    (4)
!       where Q on the left-hand side is at (n+1)th time level.
!
!       In this subroutine, a(k), b(k) and c(k) are obtained from
!       subprogram coefvu and are passed to subprogram tinteg via
!       common. 1/2*F and P-1/2*F*Q are stored in bp and rp,
!       respectively. Subprogram tinteg solves Eq.(4).
!
!       Modify this subroutine according to your numerical integration
!       scheme (program).
!
!-------------------------------------------------------------------
  SUBROUTINE  mym_predict (kts,kte,&
       &            levflag,  &
       &            delt,&
       &            dz, &
       &            ust, flt, flq, pmz, phh, &
       &            el, dfq, &
       &            pdk, pdt, pdq, pdc,&
       &            qke, tsq, qsq, cov, &
       &            s_aw,s_awqke,bl_mynn_edmf_tke &
       &)

!-------------------------------------------------------------------
    INTEGER, INTENT(IN) :: kts,kte    

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    INTEGER, INTENT(IN) :: levflag
    INTEGER, INTENT(IN) :: bl_mynn_edmf_tke
    REAL, INTENT(IN)    :: delt
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz, dfq,el
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: pdk, pdt, pdq, pdc
    REAL, INTENT(IN)    ::  flt, flq, ust, pmz, phh
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: qke,tsq, qsq, cov
! WA 8/3/15
    REAL, DIMENSION(kts:kte+1), INTENT(INOUT) :: s_awqke,s_aw

    INTEGER :: k
    REAL, DIMENSION(kts:kte) :: qkw, bp, rp, df3q
    REAL :: vkz,pdk1,phm,pdt1,pdq1,pdc1,b1l,b2l,onoff
    REAL, DIMENSION(kts:kte) :: dtz
    REAL, DIMENSION(kts:kte) :: a,b,c,d,x


    ! REGULATE THE MOMENTUM MIXING FROM THE MASS-FLUX SCHEME (on or off)
    IF (bl_mynn_edmf_tke == 0) THEN
       onoff=0.0
    ELSE
       onoff=1.0
    ENDIF

!   **  Strictly, vkz*h(i,j) -> vk*( 0.5*dz(1)*h(i,j)+z0 )  **
    vkz = vk*0.5*dz(kts)
!
!   **  dfq for the TKE is 3.0*dfm.  **
!
    DO k = kts,kte
!!       qke(k) = MAX(qke(k), 0.0)
       qkw(k) = SQRT( MAX( qke(k), 0.0 ) )
       df3q(k)=Sqfac*dfq(k)
       dtz(k)=delt/dz(k)
    END DO
!
    pdk1 = 2.0*ust**3*pmz/( vkz )
    phm  = 2.0/ust   *phh/( vkz )
    pdt1 = phm*flt**2
    pdq1 = phm*flq**2
    pdc1 = phm*flt*flq
!
!   **  pdk(i,j,1)+pdk(i,j,2) corresponds to pdk1.  **
    pdk(kts) = pdk1 -pdk(kts+1)

!!    pdt(kts) = pdt1 -pdt(kts+1)
!!    pdq(kts) = pdq1 -pdq(kts+1)
!!    pdc(kts) = pdc1 -pdc(kts+1)
    pdt(kts) = pdt(kts+1)
    pdq(kts) = pdq(kts+1)
    pdc(kts) = pdc(kts+1)
!
!   **  Prediction of twice the turbulent kinetic energy  **
!!    DO k = kts+1,kte-1
    DO k = kts,kte-1
       b1l = b1*0.5*( el(k+1)+el(k) )
       bp(k) = 2.*qkw(k) / b1l
       rp(k) = pdk(k+1) + pdk(k)
    END DO

!!    a(1)=0.
!!    b(1)=1.
!!    c(1)=-1.
!!    d(1)=0.

! Since df3q(kts)=0.0, a(1)=0.0 and b(1)=1.+dtz(k)*df3q(k+1)+bp(k)*delt.
    DO k=kts,kte-1
!       a(k-kts+1)=-dtz(k)*df3q(k)
!       b(k-kts+1)=1.+dtz(k)*(df3q(k)+df3q(k+1))+bp(k)*delt
!       c(k-kts+1)=-dtz(k)*df3q(k+1)
!       d(k-kts+1)=rp(k)*delt + qke(k)
! WA 8/3/15 add EDMF contribution
       a(k-kts+1)=-dtz(k)*df3q(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(k-kts+1)=1. + dtz(k)*(df3q(k)+df3q(k+1)) &
                     + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff + bp(k)*delt
       c(k-kts+1)=-dtz(k)*df3q(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(k-kts+1)=rp(k)*delt + qke(k) + dtz(k)*(s_awqke(k)-s_awqke(k+1))*onoff
    ENDDO

!!    DO k=kts+1,kte-1
!!       a(k-kts+1)=-dtz(k)*df3q(k)
!!       b(k-kts+1)=1.+dtz(k)*(df3q(k)+df3q(k+1))
!!       c(k-kts+1)=-dtz(k)*df3q(k+1)
!!       d(k-kts+1)=rp(k)*delt + qke(k) - qke(k)*bp(k)*delt
!!    ENDDO

    a(kte)=-1. !0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=0.

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)

    DO k=kts,kte
!       qke(k)=max(d(k-kts+1), 1.e-4)
       qke(k)=max(x(k), 1.e-4)
    ENDDO
      

    IF ( levflag .EQ. 3 ) THEN
!
!  Modified: Dec/22/2005, from here
!   **  dfq for the scalar variance is 1.0*dfm.  **
!       CALL coefvu ( dfq, 1.0 ) make change here 
!  Modified: Dec/22/2005, up to here
!
!   **  Prediction of the temperature variance  **
!!       DO k = kts+1,kte-1
       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdt(k+1) + pdt(k) 
       END DO
       
!zero gradient for tsq at bottom and top
       
!!       a(1)=0.
!!       b(1)=1.
!!       c(1)=-1.
!!       d(1)=0.

! Since dfq(kts)=0.0, a(1)=0.0 and b(1)=1.+dtz(k)*dfq(k+1)+bp(k)*delt.
       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + tsq(k)
       ENDDO

!!       DO k=kts+1,kte-1
!!          a(k-kts+1)=-dtz(k)*dfq(k)
!!          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))
!!          c(k-kts+1)=-dtz(k)*dfq(k+1)
!!          d(k-kts+1)=rp(k)*delt + tsq(k) - tsq(k)*bp(k)*delt
!!       ENDDO

       a(kte)=-1. !0.
       b(kte)=1.
       c(kte)=0.
       d(kte)=0.

!       CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)
       
       DO k=kts,kte
!          tsq(k)=d(k-kts+1)
           tsq(k)=x(k)
       ENDDO
       
!   **  Prediction of the moisture variance  **
!!       DO k = kts+1,kte-1
       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdq(k+1) +pdq(k) 
       END DO
       
!zero gradient for qsq at bottom and top
       
!!       a(1)=0.
!!       b(1)=1.
!!       c(1)=-1.
!!       d(1)=0.

! Since dfq(kts)=0.0, a(1)=0.0 and b(1)=1.+dtz(k)*dfq(k+1)+bp(k)*delt.
       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + qsq(k)
       ENDDO

!!       DO k=kts+1,kte-1
!!          a(k-kts+1)=-dtz(k)*dfq(k)
!!          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))
!!          c(k-kts+1)=-dtz(k)*dfq(k+1)
!!          d(k-kts+1)=rp(k)*delt + qsq(k) -qsq(k)*bp(k)*delt
!!       ENDDO

       a(kte)=-1. !0.
       b(kte)=1.
       c(kte)=0.
       d(kte)=0.
       
!       CALL tridiag(kte,a,b,c,d)
       CALL tridiag2(kte,a,b,c,d,x)

       DO k=kts,kte
!          qsq(k)=d(k-kts+1)
           qsq(k)=x(k)
       ENDDO
       
!   **  Prediction of the temperature-moisture covariance  **
!!       DO k = kts+1,kte-1
       DO k = kts,kte-1
          b2l = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdc(k+1) + pdc(k) 
       END DO
       
!zero gradient for tqcov at bottom and top
       
!!       a(1)=0.
!!       b(1)=1.
!!       c(1)=-1.
!!       d(1)=0.

! Since dfq(kts)=0.0, a(1)=0.0 and b(1)=1.+dtz(k)*dfq(k+1)+bp(k)*delt.
       DO k=kts,kte-1
          a(k-kts+1)=-dtz(k)*dfq(k)
          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          c(k-kts+1)=-dtz(k)*dfq(k+1)
          d(k-kts+1)=rp(k)*delt + cov(k)
       ENDDO

!!       DO k=kts+1,kte-1
!!          a(k-kts+1)=-dtz(k)*dfq(k)
!!          b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))
!!          c(k-kts+1)=-dtz(k)*dfq(k+1)
!!          d(k-kts+1)=rp(k)*delt + cov(k) - cov(k)*bp(k)*delt
!!       ENDDO

       a(kte)=-1. !0.
       b(kte)=1.
       c(kte)=0.
       d(kte)=0.

!       CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)
       
       DO k=kts,kte
!          cov(k)=d(k-kts+1)
          cov(k)=x(k)
       ENDDO
       
    ELSE
!!       DO k = kts+1,kte-1
       DO k = kts,kte-1
          IF ( qkw(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*0.25*( el(k+1)+el(k) )/qkw(k)
          END IF
!
          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO
       
!!       tsq(kts)=tsq(kts+1)
!!       qsq(kts)=qsq(kts+1)
!!       cov(kts)=cov(kts+1)

       tsq(kte)=tsq(kte-1)
       qsq(kte)=qsq(kte-1)
       cov(kte)=cov(kte-1)
      
    END IF

#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mym_predict
  
! ==================================================================
!     SUBROUTINE  mym_condensation:
!
!     Input variables:    see subroutine mym_initialize and turbulence
!       exner(nz)    : Perturbation of the Exner function    (J/kg K)
!                         defined on the walls of the grid boxes
!                         This is usually computed by integrating
!                         d(pi)/dz = h*g*tv/tref**2
!                         from the upper boundary, where tv is the
!                         virtual potential temperature minus tref.
!
!     Output variables:   see subroutine mym_initialize
!       cld(nx,nz,ny)   : Cloud fraction
!
!     Work arrays:
!       qmq(nx,nz,ny)   : Q_w-Q_{sl}, where Q_{sl} is the saturation
!                         specific humidity at T=Tl
!       alp(nx,nz,ny)   : Functions in the condensation process
!       bet(nx,nz,ny)   : ditto
!       sgm(nx,nz,ny)   : Combined standard deviation sigma_s
!                         multiplied by 2/alp
!
!     # qmq, alp, bet and sgm are allowed to share storage units with
!       any four of other work arrays for saving memory.
!
!     # Results are sensitive particularly to values of cp and rd.
!       Set these values to those adopted by you.
!
!-------------------------------------------------------------------
  SUBROUTINE  mym_condensation (kts,kte,  &
    &            dx, dz, zw,              &
    &            thl, qw, qv, qc, qi,     &
    &            p,exner,                 &
    &            tsq, qsq, cov,           &
    &            Sh, el, bl_mynn_cloudpdf,&
    &            qc_bl1D, qi_bl1D,        &
    &            cldfra_bl1D,             &
    &            PBLH1,HFX1,              &
    &            Vt, Vq, th, sgm, rmo,    &
    &            spp_pbl,rstoch_col       )

!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte, bl_mynn_cloudpdf

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    REAL, INTENT(IN)      :: dx,PBLH1,HFX1,rmo
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw
    REAL, DIMENSION(kts:kte), INTENT(IN) :: p,exner,thl,qw,qv,qc,qi, &
         &tsq, qsq, cov, th

    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: vt,vq,sgm

    REAL, DIMENSION(kts:kte) :: qmq,alp,a,bet,b,ql,q1,RH
    REAL, DIMENSION(kts:kte), INTENT(OUT) :: qc_bl1D,qi_bl1D, &
                                             cldfra_bl1D
    DOUBLE PRECISION :: t3sq, r3sq, c3sq

    REAL :: qsl,esat,qsat,tlk,qsat_tl,dqsl,cld0,q1k,eq1,qll,&
         &q2p,pt,rac,qt,t,xl,rsl,cpm,cdhdz,Fng,qww,alpha,beta,bb,&
         &ls_min,ls,wt,cld_factor,fac_damp,liq_frac,ql_ice,ql_water,&
         &low_weight
    INTEGER :: i,j,k

    REAL :: erf

    !JOE: NEW VARIABLES FOR ALTERNATE SIGMA
    REAL::dth,dtl,dqw,dzk,els
    REAL, DIMENSION(kts:kte), INTENT(IN) :: Sh,el

    !JOE: variables for BL clouds
    REAL::zagl,damp,PBLH2,ql_limit
    REAL            :: lfac

    !JAYMES:  variables for tropopause-height estimation
    REAL            :: theta1, theta2, ht1, ht2
    INTEGER         :: k_tropo

!   Stochastic
    INTEGER,  INTENT(IN)                          ::    spp_pbl
    REAL, DIMENSION(KTS:KTE)                      ::    rstoch_col
    REAL :: qw_pert

! First, obtain an estimate for the tropopause height (k), using the method employed in the
! Thompson subgrid-cloud scheme.  This height will be a consideration later when determining 
! the "final" subgrid-cloud properties.
! JAYMES:  added 3 Nov 2016, adapted from G. Thompson

    DO k = kte-3, kts, -1
       theta1 = th(k)
       theta2 = th(k+2)
       ht1 = 44307.692 * (1.0 - (p(k)/101325.)**0.190)
       ht2 = 44307.692 * (1.0 - (p(k+2)/101325.)**0.190)
       if ( (((theta2-theta1)/(ht2-ht1)) .lt. 10./1500. ) .AND.       &
     &                       (ht1.lt.19000.) .and. (ht1.gt.4000.) ) then 
          goto 86
       endif
    ENDDO
 86   continue
    k_tropo = MAX(kts+2, k+2)

    zagl = 0.

    SELECT CASE(bl_mynn_cloudpdf)

      CASE (0) ! ORIGINAL MYNN PARTIAL-CONDENSATION SCHEME

        DO k = kts,kte-1
           t  = th(k)*exner(k)

!x      if ( ct .gt. 0.0 ) then
!       a  =  17.27
!       b  = 237.3
!x      else
!x        a  =  21.87
!x        b  = 265.5
!x      end if
!
!   **  3.8 = 0.622*6.11 (hPa)  **

           !SATURATED VAPOR PRESSURE
           esat = esat_blend(t)
           !SATURATED SPECIFIC HUMIDITY
           !qsl=ep_2*esat/(p(k)-ep_3*esat)
           qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))
           !dqw/dT: Clausius-Clapeyron
           dqsl = qsl*ep_2*ev/( rd*t**2 )

           alp(k) = 1.0/( 1.0+dqsl*xlvcp )
           bet(k) = dqsl*exner(k)

           !Sommeria and Deardorff (1977) scheme, as implemented
           !in Nakanishi and Niino (2009), Appendix B
           t3sq = MAX( tsq(k), 0.0 )
           r3sq = MAX( qsq(k), 0.0 )
           c3sq =      cov(k)
           c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )
           r3sq = r3sq +bet(k)**2*t3sq -2.0*bet(k)*c3sq
           !DEFICIT/EXCESS WATER CONTENT
           qmq(k) = qw(k) -qsl
           !ORIGINAL STANDARD DEVIATION
           sgm(k) = SQRT( MAX( r3sq, 1.0d-10 ))
           !NORMALIZED DEPARTURE FROM SATURATION
           q1(k)   = qmq(k) / sgm(k)
           !CLOUD FRACTION. rr2 = 1/SQRT(2) = 0.707
           cldfra_bl1D(k) = 0.5*( 1.0+erf( q1(k)*rr2 ) )

           eq1  = rrp*EXP( -0.5*q1k*q1k )
           qll  = MAX( cldfra_bl1D(k)*q1k + eq1, 0.0 )
           !ESTIMATED LIQUID WATER CONTENT (UNNORMALIZED)
           ql(k) = alp(k)*sgm(k)*qll
           !LIMIT SPECIES TO TEMPERATURE RANGES
           liq_frac = min(1.0, max(0.0,(t-240.0)/29.0))
           qc_bl1D(k) = liq_frac*ql(k)
           qi_bl1D(k) = (1.0 - liq_frac)*ql(k)

           if(cldfra_bl1D(k)>0.01 .and. qc_bl1D(k)<1.E-6)qc_bl1D(k)=1.E-6
           if(cldfra_bl1D(k)>0.01 .and. qi_bl1D(k)<1.E-8)qi_bl1D(k)=1.E-8

           !Now estimate the buiyancy flux functions
           q2p = xlvcp/exner(k)
           pt = thl(k) +q2p*ql(k) ! potential temp

           !qt is a THETA-V CONVERSION FOR TOTAL WATER (i.e., THETA-V = qt*THETA)
           qt   = 1.0 +p608*qw(k) -(1.+p608)*(qc_bl1D(k)+qi_bl1D(k))*cldfra_bl1D(k)
           rac  = alp(k)*( cldfra_bl1D(K)-qll*eq1 )*( q2p*qt-(1.+p608)*pt )

           !BUOYANCY FACTORS: wherever vt and vq are used, there is a
           !"+1" and "+tv0", respectively, so these are subtracted out here.
           !vt is unitless and vq has units of K.
           vt(k) =      qt-1.0 -rac*bet(k)
           vq(k) = p608*pt-tv0 +rac

        END DO

      CASE (1, -1) !ALTERNATIVE FORM (Nakanishi & Niino 2004 BLM, eq. B6, and
                       !Kuwano-Yoshida et al. 2010 QJRMS, eq. 7):
        DO k = kts,kte-1
           t  = th(k)*exner(k)
           !SATURATED VAPOR PRESSURE
           esat = esat_blend(t)
           !SATURATED SPECIFIC HUMIDITY
           !qsl=ep_2*esat/(p(k)-ep_3*esat)
           qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))
           !dqw/dT: Clausius-Clapeyron
           dqsl = qsl*ep_2*ev/( rd*t**2 )

           alp(k) = 1.0/( 1.0+dqsl*xlvcp )
           bet(k) = dqsl*exner(k)

           if (k .eq. kts) then 
             dzk = 0.5*dz(k)
           else
             dzk = dz(k)
           end if
           dth = 0.5*(thl(k+1)+thl(k)) - 0.5*(thl(k)+thl(MAX(k-1,kts)))
           dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))
           sgm(k) = SQRT( MAX( (alp(k)**2 * MAX(el(k)**2,0.1) * &
                             b2 * MAX(Sh(k),0.03))/4. * &
                      (dqw/dzk - bet(k)*(dth/dzk ))**2 , 1.0e-10) )
           qmq(k) = qw(k) -qsl
           q1(k)   = qmq(k) / sgm(k)
           cldfra_bl1D(K) = 0.5*( 1.0+erf( q1(k)*rr2 ) )

           !now compute estimated lwc for PBL scheme's use 
           !qll IS THE NORMALIZED LIQUID WATER CONTENT (Sommeria and
           !Deardorff (1977, eq 29a). rrp = 1/(sqrt(2*pi)) = 0.3989
           q1k  = q1(k)
           eq1  = rrp*EXP( -0.5*q1k*q1k )
           qll  = MAX( cldfra_bl1D(K)*q1k + eq1, 0.0 )
           !ESTIMATED LIQUID WATER CONTENT (UNNORMALIZED)
           ql (k) = alp(k)*sgm(k)*qll
           liq_frac = min(1.0, max(0.0,(t-240.0)/29.0))
           qc_bl1D(k) = liq_frac*ql(k)
           qi_bl1D(k) = (1.0 - liq_frac)*ql(k)

           if(cldfra_bl1D(k)>0.01 .and. qc_bl1D(k)<1.E-6)qc_bl1D(k)=1.E-6
           if(cldfra_bl1D(k)>0.01 .and. qi_bl1D(k)<1.E-8)qi_bl1D(k)=1.E-8

           !Now estimate the buiyancy flux functions
           q2p = xlvcp/exner(k)
           pt = thl(k) +q2p*ql(k) ! potential temp

           !qt is a THETA-V CONVERSION FOR TOTAL WATER (i.e., THETA-V = qt*THETA)
           qt   = 1.0 +p608*qw(k) -(1.+p608)*(qc_bl1D(k)+qi_bl1D(k))*cldfra_bl1D(k)
           rac  = alp(k)*( cldfra_bl1D(K)-qll*eq1 )*( q2p*qt-(1.+p608)*pt )

           !BUOYANCY FACTORS: wherever vt and vq are used, there is a
           !"+1" and "+tv0", respectively, so these are subtracted out here.
           !vt is unitless and vq has units of K.
           vt(k) =      qt-1.0 -rac*bet(k)
           vq(k) = p608*pt-tv0 +rac

        END DO

      CASE (2, -2)
        !Diagnostic statistical scheme of Chaboureau and Bechtold (2002), JAS
        !JAYMES- this added 27 Apr 2015
        PBLH2=MAX(10.,PBLH1)
        zagl = 0.
        DO k = kts,kte-1
           t  = th(k)*exner(k)
           !SATURATED VAPOR PRESSURE
           esat = esat_blend(t)
           !SATURATED SPECIFIC HUMIDITY
           !qsl=ep_2*esat/(p(k)-ep_3*esat)
           qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))
           !dqw/dT: Clausius-Clapeyron
           dqsl = qsl*ep_2*ev/( rd*t**2 )
           !RH (0 to 1.0)
           RH(k)=MAX(MIN(1.0,qw(k)/MAX(1.E-8,qsl)),0.001)

           alp(k) = 1.0/( 1.0+dqsl*xlvcp )
           bet(k) = dqsl*exner(k)

           xl = xl_blend(t)                    ! obtain latent heat
           tlk = thl(k)*(p(k)/p1000mb)**rcp    ! recover liquid temp (tl) from thl
           qsat_tl = qsat_blend(tlk,p(k))      ! get saturation water vapor mixing ratio
                                               !   at tl and p
           rsl = xl*qsat_tl / (r_v*tlk**2)     ! slope of C-C curve at t = tl
                                               ! CB02, Eqn. 4
           cpm = cp + qw(k)*cpv                ! CB02, sec. 2, para. 1
           a(k) = 1./(1. + xl*rsl/cpm)         ! CB02 variable "a"
           !SPP
           qw_pert = qw(k) + qw(k)*0.5*rstoch_col(k)*real(spp_pbl)
           !qmq(k) = a(k) * (qw(k) - qsat_tl) ! saturation deficit/excess;
                                               !   the numerator of Q1
           qmq(k) = a(k) * (qw_pert - qsat_tl)
           b(k) = a(k)*rsl                     ! CB02 variable "b"
           dtl =    0.5*(thl(k+1)*(p(k+1)/p1000mb)**rcp + tlk) &
               & - 0.5*(tlk + thl(MAX(k-1,kts))*(p(MAX(k-1,kts))/p1000mb)**rcp)
           dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))

           if (k .eq. kts) then
             dzk = 0.5*dz(k)
           else
             dzk = dz(k)
           end if

           cdhdz = dtl/dzk + (g/cpm)*(1.+qw(k))  ! expression below Eq. 9
                                                 ! in CB02
           zagl = zagl + dz(k)
           !Use analog to surface layer length scale to make the cloud mixing length scale
           !become less than z in stable conditions.
           els  = zagl !save for more testing:  /(1.0 + 1.0*MIN( 0.5*dz(1)*MAX(rmo,0.0), 1. ))

           !ls_min = 300. + MIN(3.*MAX(HFX1,0.),300.)
           ls_min = 300. + MIN(2.*MAX(HFX1,0.),150.)
           ls_min = MIN(MAX(els,25.),ls_min) ! Let this be the minimum possible length scale:
           if (zagl > PBLH1+2000.) ls_min = MAX(ls_min + 0.5*(PBLH1+2000.-zagl),300.)
                                        !   25 m < ls_min(=zagl) < 300 m
           lfac=MIN(4.25+dx/4000.,6.)   ! A dx-dependent multiplier for the master length scale:
                                        !   lfac(750 m) = 4.4
                                        !   lfac(3 km)  = 5.0
                                        !   lfac(13 km) = 6.0
           ls = MAX(MIN(lfac*el(k),600.),ls_min)  ! Bounded:  ls_min < ls < 600 m
                   ! Note: CB02 use 900 m as a constant free-atmosphere length scale. 

                   ! Above 300 m AGL, ls_min remains 300 m.  For dx = 3 km, the 
                   ! MYNN master length scale (el) must exceed 60 m before ls
                   ! becomes responsive to el, otherwise ls = ls_min = 300 m.

           sgm(k) = MAX(1.e-10, 0.225*ls*SQRT(MAX(0., & ! Eq. 9 in CB02:
                   & (a(k)*dqw/dzk)**2              & ! < 1st term in brackets,
                   & -2*a(k)*b(k)*cdhdz*dqw/dzk     & ! < 2nd term,
                   & +b(k)**2 * cdhdz**2)))           ! < 3rd term
                   ! CB02 use a multiplier of 0.2, but 0.225 is chosen
                   ! based on tests

           q1(k) = qmq(k) / sgm(k)  ! Q1, the normalized saturation
           cldfra_bl1D(K) = MAX(0., MIN(1., 0.5+0.36*ATAN(1.55*q1(k)))) ! Eq. 7 in CB02

        END DO

        ! JAYMES- this option added 8 May 2015
        ! The cloud water formulations are taken from CB02, Eq. 8.
        ! "fng" represents the non-Gaussian contribution to the liquid
        ! water flux; these formulations are from Cuijpers and Bechtold
        ! (1995), Eq. 7.  CB95 also draws from Bechtold et al. 1995,
        ! hereafter BCMT95
        zagl = 0.
        DO k = kts,kte-1
           t    = th(k)*exner(k)
           q1k  = q1(k)
           zagl = zagl + dz(k)

           !CLOUD WATER AND ICE
           IF (q1k < 0.) THEN        !unstaurated
              ql_water = sgm(k)*EXP(1.2*q1k-1)
!              ql_ice   = sgm(k)*EXP(0.9*q1k-2.6)
              !Reduce ice mixing ratios in the upper troposphere
              low_weight = MIN(MAX(p(k)-40000.0, 0.0),40000.0)/40000.0
              ql_ice   = low_weight * sgm(k)*EXP(1.1*q1k-1.6) &  !low-lev
                  + (1.-low_weight) * sgm(k)*EXP(1.1*q1k-2.8)!upper-lev
           ELSE IF (q1k > 2.) THEN   !supersaturated
              ql_water = sgm(k)*q1k
              ql_ice = MIN(80.*qv(k),0.1)*sgm(k)*q1k
           ELSE                      !slightly saturated (0 > q1 < 2)
              ql_water = sgm(k)*(EXP(-1.) + 0.66*q1k + 0.086*q1k**2)
              ql_ice = MIN(80.*qv(k),0.1)*sgm(k)*(EXP(-1.) + 0.66*q1k + 0.086*q1k**2)
           ENDIF

           !In saturated grid cells, use average of current estimate and prev time step
           IF ( qc(k) > 1.e-7 ) ql_water = 0.5 * ( ql_water + qc(k) )
           IF ( qi(k) > 1.e-9 ) ql_ice = 0.5 * ( ql_ice + qi(k) )

           IF (cldfra_bl1D(K) < 0.005) THEN
              ql_ice   = 0.0
              ql_water = 0.0
           ENDIF

           !PHASE PARTITIONING:  Make some inferences about the relative amounts of subgrid cloud water vs. ice
           !based on collocated explicit clouds.  Otherise, use a simple temperature-dependent partitioning.
           IF ( qc(k) + qi(k) > 0.0 ) THEN ! explicit condensate exists, so attempt to retain its phase partitioning
              IF ( qi(k) == 0.0 ) THEN       ! explicit contains no ice; assume subgrid liquid
                liq_frac = 1.0  
              ELSE IF ( qc(k) == 0.0 ) THEN  ! explicit contains no liquid; assume subgrid ice
                liq_frac = 0.0
              ELSE IF ( (qc(k) >= 1.E-10) .AND. (qi(k) >= 1.E-10) ) THEN  ! explicit contains mixed phase of workably 
                                                                          ! large amounts; assume subgrid follows 
                                                                          ! same partioning
                liq_frac = qc(k) / ( qc(k) + qi(k) )
              ELSE 
                liq_frac = MIN(1.0, MAX(0.0, (t-238.)/31.)) ! explicit contains mixed phase, but at least one 
                                                                   ! species is very small, so make a temperature-
                                                                   ! depedent guess
              ENDIF
           ELSE                          ! no explicit condensate, so make a temperature-dependent guess
             liq_frac = MIN(1.0, MAX(0.0, (t-238.)/31.))
           ENDIF

           qc_bl1D(k) = liq_frac*ql_water       ! apply liq_frac to ql_water and ql_ice
           qi_bl1D(k) = (1.0-liq_frac)*ql_ice

           !Above tropopause:  eliminate subgrid clouds from CB scheme
           if (k .ge. k_tropo-1) then
              cldfra_bl1D(K) = 0.
              qc_bl1D(k)  = 0.
              qi_bl1D(k)  = 0.
           endif
       
           !Buoyancy-flux-related calculations follow...
           ! "Fng" represents the non-Gaussian transport factor
           ! (non-dimensional) from Bechtold et al. 1995 
           ! (hereafter BCMT95), section 3(c).  Their suggested 
           ! forms for Fng (from their Eq. 20) are:
           !IF (q1k < -2.) THEN
           !  Fng = 2.-q1k
           !ELSE IF (q1k > 0.) THEN
           !  Fng = 1.
           !ELSE
           !  Fng = 1.-1.5*q1k
           !ENDIF
           ! For purposes of the buoyancy flux in stratus, we will use Fng = 1
           !Fng = 1.
           Q1(k)=MAX(Q1(k),-5.0)
           IF (Q1(k) .GE. 1.0) THEN
              Fng = 1.0
           ELSEIF (Q1(k) .GE. -1.7 .AND. Q1(k) < 1.0) THEN
              Fng = EXP(-0.4*(Q1(k)-1.0))
           ELSEIF (Q1(k) .GE. -2.5 .AND. Q1(k) .LT. -1.7) THEN
              Fng = 3.0 + EXP(-3.8*(Q1(k)+1.7))
           ELSE
              Fng = MIN(23.9 + EXP(-1.6*(Q1(k)+2.5)), 60.)
           ENDIF
           Fng = MIN(Fng, 20.)

           xl    = xl_blend(t)
           bb = b(k)*t/th(k) ! bb is "b" in BCMT95.  Their "b" differs from 
                             ! "b" in CB02 (i.e., b(k) above) by a factor 
                             ! of T/theta.  Strictly, b(k) above is formulated in
                             ! terms of sat. mixing ratio, but bb in BCMT95 is
                             ! cast in terms of sat. specific humidity.  The
                             ! conversion is neglected here. 
           qww   = 1.+0.61*qw(k)
           alpha = 0.61*th(k)
           beta  = (th(k)/t)*(xl/cp) - 1.61*th(k)
           vt(k) = qww   - MIN(cldfra_bl1D(K),0.5)*beta*bb*Fng   - 1.
           vq(k) = alpha + MIN(cldfra_bl1D(K),0.5)*beta*a(k)*Fng - tv0
           ! vt and vq correspond to beta-theta and beta-q, respectively,  
           ! in NN09, Eq. B8.  They also correspond to the bracketed
           ! expressions in BCMT95, Eq. 15, since (s*ql/sigma^2) = cldfra*Fng
           ! The "-1" and "-tv0" terms are included for consistency with 
           ! the legacy vt and vq formulations (above).

           ! dampen the amplification factor (cld_factor) with height in order
           ! to limit excessively large cloud fractions aloft
           fac_damp = 1. -MIN(MAX( zagl-(PBLH2+1000.),0.0)/ &
                              MAX((zw(k_tropo)-(PBLH2+1000.)),500.), 1.)
           !cld_factor = 1.0 + fac_damp*MAX(0.0, ( RH(k) - 0.5 ) / 0.51 )**3.3
           cld_factor = 1.0 + fac_damp*MAX(0.0, ( RH(k) - 0.75 ) / 0.26 )**1.9
           cldfra_bl1D(K) = MIN( 1., cld_factor*cldfra_bl1D(K) )

         END DO

      END SELECT !end cloudPDF option

      !FOR TESTING PURPOSES ONLY, ISOLATE ON THE MASS-CLOUDS.
      IF (bl_mynn_cloudpdf .LT. 0) THEN
         DO k = kts,kte-1
            cldfra_bl1D(k) = 0.0
            qc_bl1D(k) = 0.0
            qi_bl1D(k) = 0.0
         END DO
      ENDIF
!
      ql(kte) = ql(kte-1)
      vt(kte) = vt(kte-1)
      vq(kte) = vq(kte-1)
      qc_bl1D(kte)=0.
      qi_bl1D(kte)=0.
      cldfra_bl1D(kte)=0.

    RETURN

#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mym_condensation

! ==================================================================
  SUBROUTINE mynn_tendencies(kts,kte,      &
       &levflag,grav_settling,             &
       &delt,dz,rho,                       &
       &u,v,th,tk,qv,qc,qi,qnc,qni,        &
       &p,exner,                           &
       &thl,sqv,sqc,sqi,sqw,               &
       &qnwfa,qnifa,                       &
       &ust,flt,flq,flqv,flqc,wspd,qcg,    &
       &uoce,voce,                         &
       &tsq,qsq,cov,                       &
       &tcd,qcd,                           &
       &dfm,dfh,dfq,                       &
       &Du,Dv,Dth,Dqv,Dqc,Dqi,Dqnc,Dqni,   &
       &Dqnwfa,Dqnifa,                     &
       &vdfg1,diss_heat,                   &
       &s_aw,s_awthl,s_awqt,s_awqv,s_awqc, &
       &s_awu,s_awv,                       &
       &s_awqnc,s_awqni,                   &
       &s_awqnwfa,s_awqnifa,               &
       &sub_thl,sub_sqv,                   &
       &sub_u,sub_v,                       &
       &det_thl,det_sqv,det_sqc,           &
       &det_u,det_v,                       &
       &FLAG_QC,FLAG_QI,FLAG_QNC,FLAG_QNI, &
       &FLAG_QNWFA,FLAG_QNIFA,             &
       &cldfra_bl1d,                       &
       &bl_mynn_cloudmix,                  &
       &bl_mynn_mixqt,                     &
       &bl_mynn_edmf,                      &
       &bl_mynn_edmf_mom,                  &
       &bl_mynn_mixscalars                )

!-------------------------------------------------------------------
    INTEGER, INTENT(in) :: kts,kte

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    INTEGER, INTENT(in) :: grav_settling,levflag
    INTEGER, INTENT(in) :: bl_mynn_cloudmix,bl_mynn_mixqt,&
                           bl_mynn_edmf,bl_mynn_edmf_mom, &
                           bl_mynn_mixscalars
    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC,&
                           FLAG_QNWFA,FLAG_QNIFA

!! grav_settling = 1 or 2 for gravitational settling of droplets
!! grav_settling = 0 otherwise
! thl - liquid water potential temperature
! qw - total water
! dfm,dfh,dfq - as above
! flt - surface flux of thl
! flq - surface flux of qw

! mass-flux plumes
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: s_aw,s_awthl,s_awqt,&
         &s_awqnc,s_awqni,s_awqv,s_awqc,s_awu,s_awv,s_awqnwfa,s_awqnifa
! tendencies from mass-flux environmental subsidence and detrainment
    REAL, DIMENSION(kts:kte), INTENT(in) :: sub_thl,sub_sqv,  &
         &sub_u,sub_v,det_thl,det_sqv,det_sqc,det_u,det_v
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,th,tk,qv,qc,qi,qni,qnc,&
         &rho,p,exner,dfq,dz,tsq,qsq,cov,tcd,qcd,cldfra_bl1d,diss_heat
    REAL, DIMENSION(kts:kte), INTENT(inout) :: thl,sqw,sqv,sqc,sqi,&
         &qnwfa,qnifa,dfm,dfh
    REAL, DIMENSION(kts:kte), INTENT(inout) :: du,dv,dth,dqv,dqc,dqi,&
         &dqni,dqnc,dqnwfa,dqnifa
    REAL, INTENT(IN) :: delt,ust,flt,flq,flqv,flqc,wspd,uoce,voce,qcg

!    REAL, INTENT(IN) :: delt,ust,flt,flq,qcg,&
!         &gradu_top,gradv_top,gradth_top,gradqv_top

!local vars

    REAL, DIMENSION(kts:kte) :: dtz,vt,vq,dfhc,dfmc !Kh for clouds (Pr < 2)
    REAL, DIMENSION(kts:kte) :: sqv2,sqc2,sqi2,sqw2,qni2,qnc2, & !AFTER MIXING
                                qnwfa2,qnifa2
    REAL, DIMENSION(kts:kte) :: zfac,plumeKh
    REAL, DIMENSION(kts:kte) :: a,b,c,d,x
    REAL, DIMENSION(kts:kte+1) :: rhoz, & !rho on model interface
          &         khdz, kmdz
    REAL :: rhs,gfluxm,gfluxp,dztop,maxdfh,mindfh,maxcf,maxKh,zw
    REAL :: grav_settling2,vdfg1    !Katata-fogdes
    REAL :: t,esat,qsl,onoff,kh,km,dzk
    INTEGER :: k,kk

    !Activate nonlocal mixing from the mass-flux scheme for
    !scalars (0.0 = no; 1.0 = yes)
    REAL, PARAMETER :: nonloc = 0.0

    dztop=.5*(dz(kte)+dz(kte-1))

    ! REGULATE THE MOMENTUM MIXING FROM THE MASS-FLUX SCHEME (on or off)
    ! Note that s_awu and s_awv already come in as 0.0 if bl_mynn_edmf_mom == 0, so
    ! we only need to zero-out the MF term
    IF (bl_mynn_edmf_mom == 0) THEN
       onoff=0.0
    ELSE
       onoff=1.0
    ENDIF

    !Prepare "constants" for diffusion equation.
    !khdz = rho*Kh/dz
    dtz(kts)=delt/dz(kts)
    kh=dfh(kts)*dz(kts)
    km=dfm(kts)*dz(kts)
    rhoz(kts)=rho(kts)
    khdz(kts)=rhoz(kts)*kh/dz(kts)
    kmdz(kts)=rhoz(kts)*km/dz(kts)
    DO k=kts+1,kte
       dtz(k)=delt/dz(k)
       rhoz(k)=(rho(k)*dz(k-1) + rho(k-1)*dz(k))/(dz(k-1)+dz(k))

       dzk = 0.5  *( dz(k)+dz(k-1) )
       kh  = dfh(k)*dzk
       km  = dfm(k)*dzk
       khdz(k)= rhoz(k)*kh/dzk
       kmdz(k)= rhoz(k)*km/dzk
    ENDDO
    rhoz(kte+1)=rho(kte)
    kh=dfh(kte)*dz(kte)
    km=dfm(kte)*dz(kte)
    khdz(kte+1)=rhoz(kte+1)*kh/dz(kte)
    kmdz(kte+1)=rhoz(kte+1)*km/dz(kte)

!!============================================
!! u
!!============================================

    k=kts

    a(1)=0.
    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
    c(1)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
    d(1)=u(k) + dtz(k)*uoce*ust**2/wspd - dtz(k)*s_awu(k+1)*onoff + &
         sub_u(k)*delt + det_u(k)*delt

!JOE - tend test
!    a(k)=0.
!    b(k)=1.+dtz(k)*dfm(k+1)    - 0.5*dtz(k)*s_aw(k+1)*onoff
!    c(k)=-dtz(k)*dfm(k+1)      - 0.5*dtz(k)*s_aw(k+1)*onoff
!    d(k)=u(k)*(1.-ust**2/wspd*dtz(k)) + &
!         dtz(k)*uoce*ust**2/wspd - dtz(k)*s_awu(k+1)*onoff

    DO k=kts+1,kte-1
       a(k)=   - dtz(k)*dfm(k)            + 0.5*dtz(k)*s_aw(k)*onoff
       b(k)=1. + dtz(k)*(dfm(k)+dfm(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff
       c(k)=   - dtz(k)*dfm(k+1)          - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(k)=u(k) + dtz(k)*(s_awu(k)-s_awu(k+1))*onoff + &
            sub_u(k)*delt + det_u(k)*delt
    ENDDO

!! no flux at the top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=0.

!! specified gradient at the top 
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=gradu_top*dztop

!! prescribed value
    a(kte)=0
    b(kte)=1.
    c(kte)=0.
    d(kte)=u(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)

    DO k=kts,kte
!       du(k)=(d(k-kts+1)-u(k))/delt
       du(k)=(x(k)-u(k))/delt
    ENDDO

!!============================================
!! v
!!============================================

    k=kts

    a(1)=0.
    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
    c(1)=   - dtz(k)*dfm(k+1)               - 0.5*dtz(k)*s_aw(k+1)*onoff
!!    d(1)=v(k)
    d(1)=v(k) + dtz(k)*voce*ust**2/wspd - dtz(k)*s_awv(k+1)*onoff + &
          sub_v(k)*delt + det_v(k)*delt

!JOE - tend test
!    a(k)=0.
!    b(k)=1.+dtz(k)*dfm(k+1)  - 0.5*dtz(k)*s_aw(k+1)*onoff
!    c(k)=  -dtz(k)*dfm(k+1)  - 0.5*dtz(k)*s_aw(k+1)*onoff
!    d(k)=v(k)*(1.-ust**2/wspd*dtz(k)) + &
!         dtz(k)*voce*ust**2/wspd - dtz(k)*s_awv(k+1)*onoff

    DO k=kts+1,kte-1
       a(k)=   - dtz(k)*dfm(k)            + 0.5*dtz(k)*s_aw(k)*onoff
       b(k)=1. + dtz(k)*(dfm(k)+dfm(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff
       c(k)=   - dtz(k)*dfm(k+1)          - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(k)=v(k) + dtz(k)*(s_awv(k)-s_awv(k+1))*onoff + &
            sub_v(k)*delt + det_v(k)*delt
    ENDDO

!! no flux at the top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=0.

!! specified gradient at the top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=gradv_top*dztop

!! prescribed value
    a(kte)=0
    b(kte)=1.
    c(kte)=0.
    d(kte)=v(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)

    DO k=kts,kte
!       dv(k)=(d(k-kts+1)-v(k))/delt
       dv(k)=(x(k)-v(k))/delt
    ENDDO

!!============================================
!! thl tendency
!! NOTE: currently, gravitational settling is removed
!!============================================
    k=kts

!    a(k)=0.
!    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    c(k)=  -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    d(k)=thl(k) + dtz(k)*flt + tcd(k)*delt &
!        & -dtz(k)*s_awthl(kts+1) + diss_heat(k)*delt*dheat_opt + &
!        & sub_thl(k)*delt + det_thl(k)*delt
!
!    DO k=kts+1,kte-1
!       a(k)=  -dtz(k)*dfh(k)            + 0.5*dtz(k)*s_aw(k)
!       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
!       c(k)=  -dtz(k)*dfh(k+1)          - 0.5*dtz(k)*s_aw(k+1)
!       d(k)=thl(k) + tcd(k)*delt + dtz(k)*(s_awthl(k)-s_awthl(k+1)) &
!           &       + diss_heat(k)*delt*dheat_opt + &
!           &         sub_thl(k)*delt + det_thl(k)*delt
!    ENDDO

!rho-weighted:                                                                                                           
    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k) - 0.5*dtz(k)*s_aw(k+1)
    c(k)=  -dtz(k)*khdz(k+1)/rho(k)           - 0.5*dtz(k)*s_aw(k+1)
    d(k)=thl(k)  + dtz(k)*flt + tcd(k)*delt - dtz(k)*s_awthl(k+1) + &
       & diss_heat(k)*delt*dheat_opt + sub_thl(k)*delt + det_thl(k)*delt

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)     + 0.5*dtz(k)*s_aw(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k) + &
           &    0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)
       d(k)=thl(k) + tcd(k)*delt + dtz(k)*(s_awthl(k)-s_awthl(k+1)) + &
          &       + diss_heat(k)*delt*dheat_opt + &
          &         sub_thl(k)*delt + det_thl(k)*delt
    ENDDO

!! no flux at the top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=0.

!! specified gradient at the top
!assume gradthl_top=gradth_top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=gradth_top*dztop

!! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=thl(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,x)
    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
       !thl(k)=d(k-kts+1)
       thl(k)=x(k)
    ENDDO

IF (bl_mynn_mixqt > 0) THEN
 !============================================
 ! MIX total water (sqw = sqc + sqv + sqi)
 ! NOTE: no total water tendency is output; instead, we must calculate
 !       the saturation specific humidity and then 
 !       subtract out the moisture excess (sqc & sqi)
 !============================================

    k=kts

!    a(k)=0.
!    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    c(k)=  -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    !rhs= qcd(k) !+ (gfluxp - gfluxm)/dz(k)&
!    d(k)=sqw(k) + dtz(k)*flq + qcd(k)*delt - dtz(k)*s_awqt(k+1)
!
!    DO k=kts+1,kte-1
!       a(k)=  -dtz(k)*dfh(k)            + 0.5*dtz(k)*s_aw(k)
!       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
!       c(k)=  -dtz(k)*dfh(k+1)          - 0.5*dtz(k)*s_aw(k+1)
!       d(k)=sqw(k) + qcd(k)*delt + dtz(k)*(s_awqt(k)-s_awqt(k+1))
!    ENDDO

!rho-weighted:
    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k) - 0.5*dtz(k)*s_aw(k+1)
    c(k)=  -dtz(k)*khdz(k+1)/rho(k)           - 0.5*dtz(k)*s_aw(k+1)
    d(k)=sqw(k)  + dtz(k)*flq + qcd(k)*delt - dtz(k)*s_awqt(k+1)

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)     + 0.5*dtz(k)*s_aw(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k) + &
           &    0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)
       d(k)=sqw(k) + qcd(k)*delt + dtz(k)*(s_awqt(k)-s_awqt(k+1))
    ENDDO

!! no flux at the top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=0.
!! specified gradient at the top
!assume gradqw_top=gradqv_top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=gradqv_top*dztop
!! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqw(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,sqw2)
    CALL tridiag3(kte,a,b,c,d,sqw2)

!    DO k=kts,kte
!       sqw2(k)=d(k-kts+1)
!    ENDDO
ELSE
    sqw2=sqw
ENDIF

IF (bl_mynn_mixqt == 0) THEN
!============================================
! cloud water ( sqc ). If mixing total water (bl_mynn_mixqt > 0),
! then sqc will be backed out of saturation check (below).
!============================================
  IF (bl_mynn_cloudmix > 0 .AND. FLAG_QC) THEN

    k=kts

!    a(k)=0.
!    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    c(k)=  -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    d(k)=sqc(k) + dtz(k)*flqc + qcd(k)*delt - &
!         dtz(k)*s_awqc(k+1)  + det_sqc(k)*delt
!
!    DO k=kts+1,kte-1
!       a(k)=  -dtz(k)*dfh(k)            + 0.5*dtz(k)*s_aw(k)
!       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
!       c(k)=  -dtz(k)*dfh(k+1)          - 0.5*dtz(k)*s_aw(k+1)
!       d(k)=sqc(k) + qcd(k)*delt + dtz(k)*(s_awqc(k)-s_awqc(k+1)) + &
!            det_sqc(k)*delt
!    ENDDO

!rho-weighted:
    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k) - 0.5*dtz(k)*s_aw(k+1)
    c(k)=  -dtz(k)*khdz(k+1)/rho(k)           - 0.5*dtz(k)*s_aw(k+1)
    d(k)=sqc(k)  + dtz(k)*flqc + qcd(k)*delt - dtz(k)*s_awqc(k+1) + &
       & det_sqc(k)*delt

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)     + 0.5*dtz(k)*s_aw(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k) + &
           &    0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)
       d(k)=sqc(k) + qcd(k)*delt + dtz(k)*(s_awqc(k)-s_awqc(k+1)) + &
          & det_sqc(k)*delt
    ENDDO

! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqc(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,sqc2)
    CALL tridiag3(kte,a,b,c,d,sqc2)

!    DO k=kts,kte
!       sqc2(k)=d(k-kts+1)
!    ENDDO
  ELSE
    !If not mixing clouds, set "updated" array equal to original array
    sqc2=sqc
  ENDIF
ENDIF

IF (bl_mynn_mixqt == 0) THEN
  !============================================
  ! MIX WATER VAPOR ONLY ( sqv ). If mixing total water (bl_mynn_mixqt > 0),
  ! then sqv will be backed out of saturation check (below).
  !============================================

    k=kts

!    a(k)=0.
!    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    c(k)=  -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    d(k)=sqv(k) + dtz(k)*flqv + qcd(k)*delt - dtz(k)*s_awqv(k+1) + &
!       & sub_sqv(k)*delt + det_sqv(k)*delt
!
!    DO k=kts+1,kte-1
!       a(k)=  -dtz(k)*dfh(k)            + 0.5*dtz(k)*s_aw(k)
!       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
!       c(k)=  -dtz(k)*dfh(k+1)          - 0.5*dtz(k)*s_aw(k+1)
!       d(k)=sqv(k) + qcd(k)*delt + dtz(k)*(s_awqv(k)-s_awqv(k+1)) + &
!          & sub_sqv(k)*delt + det_sqv(k)*delt
!    ENDDO

!rho-weighted:
    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k) - 0.5*dtz(k)*s_aw(k+1)
    c(k)=  -dtz(k)*khdz(k+1)/rho(k)           - 0.5*dtz(k)*s_aw(k+1)
    d(k)=sqv(k)  + dtz(k)*flqv + qcd(k)*delt - dtz(k)*s_awqv(k+1) + &
       & sub_sqv(k)*delt + det_sqv(k)*delt

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)     + 0.5*dtz(k)*s_aw(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k) + &
           &    0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)
       d(k)=sqv(k) + qcd(k)*delt + dtz(k)*(s_awqv(k)-s_awqv(k+1)) + &
          & sub_sqv(k)*delt + det_sqv(k)*delt
    ENDDO

! no flux at the top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=0.

! specified gradient at the top
! assume gradqw_top=gradqv_top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=gradqv_top*dztop

! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqv(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,sqv2)
    CALL tridiag3(kte,a,b,c,d,sqv2)

!    DO k=kts,kte
!       sqv2(k)=d(k-kts+1)
!    ENDDO
ELSE
    sqv2=sqv
ENDIF

!============================================
! MIX CLOUD ICE ( sqi )                      
!============================================
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QI) THEN

    k=kts

!    a(k)=0.
!    b(k)=1.+dtz(k)*dfh(k+1)
!    c(k)=  -dtz(k)*dfh(k+1)
!    d(k)=sqi(k) !+ qcd(k)*delt !should we have qcd for ice?
!
!    DO k=kts+1,kte-1
!       a(k)=  -dtz(k)*dfh(k)
!       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1))
!       c(k)=  -dtz(k)*dfh(k+1)
!       d(k)=sqi(k) !+ qcd(k)*delt
!    ENDDO

!rho-weighted:
    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k)
    c(k)=  -dtz(k)*khdz(k+1)/rho(k)
    d(k)=sqi(k)

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k)
       c(k)=  -dtz(k)*khdz(k+1)/rho(k)
       d(k)=sqi(k)
    ENDDO

!! no flux at the top
!    a(kte)=-1.       
!    b(kte)=1.        
!    c(kte)=0.        
!    d(kte)=0.        

!! specified gradient at the top
!assume gradqw_top=gradqv_top
!    a(kte)=-1.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=gradqv_top*dztop

!! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqi(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,sqi2)
    CALL tridiag3(kte,a,b,c,d,sqi2)

!    DO k=kts,kte
!       sqi2(k)=d(k-kts+1)
!    ENDDO
ELSE
   sqi2=sqi
ENDIF

!!============================================
!! cloud ice number concentration (qni)
!!============================================
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNI .AND. &
      bl_mynn_mixscalars > 0) THEN

    k=kts

    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    d(k)=qni(k)  - dtz(k)*s_awqni(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)     + 0.5*dtz(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k) + &
           &    0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
       d(k)=qni(k) + dtz(k)*(s_awqni(k)-s_awqni(k+1))*nonloc
    ENDDO

!! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qni(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,x)
    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
       !qni2(k)=d(k-kts+1)
       qni2(k)=x(k)
    ENDDO

ELSE
    qni2=qni
ENDIF

!!============================================
!! cloud water number concentration (qnc)     
!! include non-local transport                
!!============================================
  IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNC .AND. &
      bl_mynn_mixscalars > 0) THEN

    k=kts

    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)/rho(k)           - 0.5*dtz(k)*s_aw(k+1)*nonloc
    d(k)=qnc(k)  - dtz(k)*s_awqnc(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)     + 0.5*dtz(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))/rho(k) + &
           &    0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
       d(k)=qnc(k) + dtz(k)*(s_awqnc(k)-s_awqnc(k+1))*nonloc
    ENDDO

!! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnc(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,x)
    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
       !qnc2(k)=d(k-kts+1)
       qnc2(k)=x(k)
    ENDDO

ELSE
    qnc2=qnc
ENDIF

!============================================
! Water-friendly aerosols ( qnwfa ).
!============================================
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNWFA .AND. &
      bl_mynn_mixscalars > 0) THEN

    k=kts

    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))/rho(k) - &
           &    0.5*dtz(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    d(k)=qnwfa(k)  - dtz(k)*s_awqnwfa(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)     + 0.5*dtz(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))/rho(k) + &
           &    0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
       d(k)=qnwfa(k) + dtz(k)*(s_awqnwfa(k)-s_awqnwfa(k+1))*nonloc
    ENDDO

! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnwfa(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,x)
    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
       !qnwfa2(k)=d(k)
       qnwfa2(k)=x(k)
    ENDDO

ELSE
    !If not mixing aerosols, set "updated" array equal to original array
    qnwfa2=qnwfa
ENDIF

!============================================
! Ice-friendly aerosols ( qnifa ).
!============================================
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNIFA .AND. &
      bl_mynn_mixscalars > 0) THEN

   k=kts

    a(k)=  -dtz(k)*khdz(k)/rho(k)
    b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))/rho(k) - &
           &    0.5*dtz(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
    d(k)=qnifa(k)  - dtz(k)*s_awqnifa(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)/rho(k)     + 0.5*dtz(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))/rho(k) + &
           &    0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)/rho(k) - 0.5*dtz(k)*s_aw(k+1)*nonloc
       d(k)=qnifa(k) + dtz(k)*(s_awqnifa(k)-s_awqnifa(k+1))*nonloc
    ENDDO

! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnifa(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,x)
    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
       !qnifa2(k)=d(k-kts+1)
       qnifa2(k)=x(k)
    ENDDO

ELSE
    !If not mixing aerosols, set "updated" array equal to original array
    qnifa2=qnifa
ENDIF


!!============================================
!! Compute tendencies and convert to mixing ratios for WRF.
!! Note that the momentum tendencies are calculated above.
!!============================================

    IF (bl_mynn_mixqt > 0) THEN 
      DO k=kts,kte
         t  = th(k)*exner(k)
         !SATURATED VAPOR PRESSURE
         esat=esat_blend(t)
         !SATURATED SPECIFIC HUMIDITY
         !qsl=ep_2*esat/(p(k)-ep_3*esat)
         qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))

         !IF (qsl >= sqw2(k)) THEN !unsaturated
         !   sqv2(k) = MAX(0.0,sqw2(k))
         !   sqi2(k) = MAX(0.0,sqi2(k))
         !   sqc2(k) = MAX(0.0,sqw2(k) - sqv2(k) - sqi2(k))
         !ELSE                     !saturated
            IF (FLAG_QI) THEN
              !sqv2(k) = qsl
              sqi2(k) = MAX(0., sqi2(k))
              sqc2(k) = MAX(0., sqw2(k) - sqi2(k) - qsl)      !updated cloud water
              sqv2(k) = MAX(0., sqw2(k) - sqc2(k) - sqi2(k))  !updated water vapor
            ELSE
              !sqv2(k) = qsl
              sqi2(k) = 0.0
              sqc2(k) = MAX(0., sqw2(k) - qsl)         !updated cloud water
              sqv2(k) = MAX(0., sqw2(k) - sqc2(k))     ! updated water vapor
            ENDIF
         !ENDIF
      ENDDO
    ENDIF

    !=====================
    ! WATER VAPOR TENDENCY
    !=====================
    DO k=kts,kte
       Dqv(k)=(sqv2(k)/(1.-sqv2(k)) - qv(k))/delt
       !IF(-Dqv(k) > qv(k)) Dqv(k)=-qv(k)
    ENDDO

    IF (bl_mynn_cloudmix > 0) THEN
      !=====================
      ! CLOUD WATER TENDENCY
      !=====================
      !qc fog settling tendency is now computed in module_bl_fogdes.F, so
      !sqc should only be changed by eddy diffusion or mass-flux.
      !print*,"FLAG_QC:",FLAG_QC
      IF (FLAG_QC) THEN
         DO k=kts,kte
            Dqc(k)=(sqc2(k)/(1.-sqv2(k)) - qc(k))/delt
            IF(Dqc(k)*delt + qc(k) < 0.) THEN
              !print*,'  neg qc:',qsl,sqw2(k),sqi2(k),sqc2(k),qc(k),tk(k)
              Dqc(k)=-qc(k)/delt 
            ENDIF
         ENDDO
      ELSE
         DO k=kts,kte
           Dqc(k) = 0.
         ENDDO
      ENDIF

      !===================
      ! CLOUD WATER NUM CONC TENDENCY
      !===================
      IF (FLAG_QNC .AND. bl_mynn_mixscalars > 0) THEN
         DO k=kts,kte
           !IF(sqc2(k)>1.e-9)qnc2(k)=MAX(qnc2(k),1.e6)
           Dqnc(k) = (qnc2(k)-qnc(k))/delt
           !IF(Dqnc(k)*delt + qnc(k) < 0.)Dqnc(k)=-qnc(k)/delt
         ENDDO 
      ELSE
         DO k=kts,kte
           Dqnc(k) = 0.
         ENDDO
      ENDIF

      !===================
      ! CLOUD ICE TENDENCY
      !===================
      IF (FLAG_QI) THEN
         DO k=kts,kte
           Dqi(k)=(sqi2(k)/(1.-sqv2(k)) - qi(k))/delt
           IF(Dqi(k)*delt + qi(k) < 0.) THEN
           !   !print*,' neg qi;',qsl,sqw2(k),sqi2(k),sqc2(k),qi(k),tk(k)
              Dqi(k)=-qi(k)/delt
           ENDIF
         ENDDO
      ELSE
         DO k=kts,kte
           Dqi(k) = 0.
         ENDDO
      ENDIF

      !===================
      ! CLOUD ICE NUM CONC TENDENCY
      !===================
      IF (FLAG_QNI .AND. bl_mynn_mixscalars > 0) THEN
         DO k=kts,kte
           Dqni(k)=(qni2(k)-qni(k))/delt
           !IF(Dqni(k)*delt + qni(k) < 0.)Dqni(k)=-qni(k)/delt
         ENDDO
      ELSE
         DO k=kts,kte
           Dqni(k)=0.
         ENDDO
      ENDIF
    ELSE !-MIX CLOUD SPECIES?
      !CLOUDS ARE NOT NIXED (when bl_mynn_cloudmix == 0)
      DO k=kts,kte
         Dqc(k)=0.
         Dqnc(k)=0.
         Dqi(k)=0.
         Dqni(k)=0.
      ENDDO
    ENDIF

    !===================
    ! THETA TENDENCY
    !===================
    IF (FLAG_QI) THEN
      DO k=kts,kte
         Dth(k)=(thl(k) + xlvcp/exner(k)*sqc(k) &
           &            + xlscp/exner(k)*sqi(k) &
           &            - th(k))/delt
         !Use form from Tripoli and Cotton (1981) with their
         !suggested min temperature to improve accuracy:
         !Dth(k)=(thl(k)*(1.+ xlvcp/MAX(tk(k),TKmin)*sqc(k)  &
         !  &               + xlscp/MAX(tk(k),TKmin)*sqi(k)) &
         !  &               - th(k))/delt
      ENDDO
    ELSE
      DO k=kts,kte
         Dth(k)=(thl(k)+xlvcp/exner(k)*sqc(k) - th(k))/delt
         !Use form from Tripoli and Cotton (1981) with their
         !suggested min temperature to improve accuracy.
         !Dth(k)=(thl(k)*(1.+ xlvcp/MAX(tk(k),TKmin)*sqc(k))  &
         !&               - th(k))/delt
      ENDDO
    ENDIF

    !===================
    ! AEROSOL TENDENCIES
    !===================
    IF (FLAG_QNWFA .AND. FLAG_QNIFA .AND. &
        bl_mynn_mixscalars > 0) THEN
       DO k=kts,kte
          !=====================
          ! WATER-friendly aerosols
          !=====================
          Dqnwfa(k)=(qnwfa2(k) - qnwfa(k))/delt
          !=====================
          ! Ice-friendly aerosols
          !=====================
          Dqnifa(k)=(qnifa2(k) - qnifa(k))/delt
       ENDDO
    ELSE
       DO k=kts,kte
          Dqnwfa(k)=0.
          Dqnifa(k)=0.
       ENDDO
    ENDIF

#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mynn_tendencies

! ==================================================================
#if (WRF_CHEM == 1)
  SUBROUTINE mynn_mix_chem(kts,kte,       &
       levflag,grav_settling,             &
       delt,dz,                           &
       nchem, kdvel, ndvel, num_vert_mix, &
       chem1, vd1,                        &
       qnc,qni,                           &
       p,exner,                           &
       thl,sqv,sqc,sqi,sqw,               &
       ust,flt,flq,flqv,flqc,wspd,qcg,    &
       uoce,voce,                         &
       tsq,qsq,cov,                       &
       tcd,qcd,                           &
       dfm,dfh,dfq,                       &
       s_aw,                              &
       s_awchem,                          &
       bl_mynn_cloudmix)

!-------------------------------------------------------------------
    INTEGER, INTENT(in) :: kts,kte
    INTEGER, INTENT(in) :: grav_settling,levflag
    INTEGER, INTENT(in) :: bl_mynn_cloudmix

    REAL, DIMENSION(kts:kte), INTENT(IN) :: qni,qnc,&
         &p,exner,dfm,dfh,dfq,dz,tsq,qsq,cov,tcd,qcd
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: thl,sqw,sqv,sqc,sqi
    REAL, INTENT(IN) :: delt,ust,flt,flq,flqv,flqc,wspd,uoce,voce,qcg
    INTEGER, INTENT(IN   )   ::   nchem, kdvel, ndvel, num_vert_mix
    REAL, DIMENSION( kts:kte+1), INTENT(IN) :: s_aw
    REAL, DIMENSION( kts:kte, nchem ), INTENT(INOUT) :: chem1
    REAL, DIMENSION( kts:kte+1,nchem), INTENT(IN) :: s_awchem
    REAL, DIMENSION( ndvel ), INTENT(INOUT) :: vd1

!local vars

    REAL, DIMENSION(kts:kte) :: dtz,vt,vq
    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d
    REAL :: rhs,gfluxm,gfluxp,dztop
    REAL :: t,esl,qsl
    INTEGER :: k,kk
    INTEGER :: ic  ! Chemical array loop index
    REAL, DIMENSION( kts:kte, nchem ) :: chem_new

    dztop=.5*(dz(kte)+dz(kte-1))

    DO k=kts,kte
       dtz(k)=delt/dz(k)
    ENDDO

  !============================================
  ! Patterned after mixing of water vapor in mynn_tendencies.
  !============================================

    DO ic = 1,nchem
       k=kts

       a(1)=0.
       b(1)=1.+dtz(k)*dfh(k+1)  - 0.5*dtz(k)*s_aw(k+1)
       c(1)=-dtz(k)*dfh(k+1)    - 0.5*dtz(k)*s_aw(k+1)
       d(1)=chem1(k,ic) + dtz(k) * -vd1(ic)*chem1(1,ic) - dtz(k)*s_awchem(k+1,ic)

       DO k=kts+1,kte-1
          a(k)=-dtz(k)*dfh(k)      + 0.5*dtz(k)*s_aw(k)
          b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) +  0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
          c(k)=-dtz(k)*dfh(k+1)    - 0.5*dtz(k)*s_aw(k+1)
          ! d(kk)=chem1(k,ic) + qcd(k)*delt
          d(k)=chem1(k,ic) + rhs*delt + dtz(k)*(s_awchem(k,ic)-s_awchem(k+1,ic))
       ENDDO

      ! prescribed value at top
       a(kte)=0.
       b(kte)=1.
       c(kte)=0.
       d(kte)=chem1(kte,ic)

       CALL tridiag(kte,a,b,c,d)

       DO k=kts,kte
          chem_new(k,ic)=d(k-kts+1)
       ENDDO
    ENDDO

  END SUBROUTINE mynn_mix_chem
#endif

! ==================================================================
  SUBROUTINE retrieve_exchange_coeffs(kts,kte,&
       &dfm,dfh,dz,K_m,K_h)

!-------------------------------------------------------------------

    INTEGER , INTENT(in) :: kts,kte

    REAL, DIMENSION(KtS:KtE), INTENT(in) :: dz,dfm,dfh

    REAL, DIMENSION(KtS:KtE), INTENT(out) :: K_m, K_h


    INTEGER :: k
    REAL :: dzk

    K_m(kts)=0.
    K_h(kts)=0.

    DO k=kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       K_m(k)=dfm(k)*dzk
       K_h(k)=dfh(k)*dzk
    ENDDO

  END SUBROUTINE retrieve_exchange_coeffs

! ==================================================================
  SUBROUTINE tridiag(n,a,b,c,d)

!! to solve system of linear eqs on tridiagonal matrix n times n
!! after Peaceman and Rachford, 1955
!! a,b,c,d - are vectors of order n 
!! a,b,c - are coefficients on the LHS
!! d - is initially RHS on the output becomes a solution vector
    
!-------------------------------------------------------------------

    INTEGER, INTENT(in):: n
    REAL, DIMENSION(n), INTENT(in) :: a,b
    REAL, DIMENSION(n), INTENT(inout) :: c,d
    
    INTEGER :: i
    REAL :: p
    REAL, DIMENSION(n) :: q
    
    c(n)=0.
    q(1)=-c(1)/b(1)
    d(1)=d(1)/b(1)
    
    DO i=2,n
       p=1./(b(i)+a(i)*q(i-1))
       q(i)=-c(i)*p
       d(i)=(d(i)-a(i)*d(i-1))*p
    ENDDO
    
    DO i=n-1,1,-1
       d(i)=d(i)+q(i)*d(i+1)
    ENDDO

  END SUBROUTINE tridiag

! ==================================================================
      subroutine tridiag2(n,a,b,c,d,x)
      implicit none
!      a - sub-diagonal (means it is the diagonal below the main diagonal)
!      b - the main diagonal
!      c - sup-diagonal (means it is the diagonal above the main diagonal)
!      d - right part
!      x - the answer
!      n - number of unknowns (levels)

        integer,intent(in) :: n
        real, dimension(n),intent(in) :: a,b,c,d
        real ,dimension(n),intent(out) :: x
        real ,dimension(n) :: cp,dp
        real :: m
        integer :: i

        ! initialize c-prime and d-prime
        cp(1) = c(1)/b(1)
        dp(1) = d(1)/b(1)
        ! solve for vectors c-prime and d-prime
        do i = 2,n
           m = b(i)-cp(i-1)*a(i)
           cp(i) = c(i)/m
           dp(i) = (d(i)-dp(i-1)*a(i))/m
        enddo
        ! initialize x
        x(n) = dp(n)
        ! solve for x from the vectors c-prime and d-prime
        do i = n-1, 1, -1
           x(i) = dp(i)-cp(i)*x(i+1)
        end do

    end subroutine tridiag2
! ==================================================================
       subroutine tridiag3(kte,a,b,c,d,x)

!ccccccccccccccccccccccccccccccc                                                                   
! Aim: Inversion and resolution of a tridiagonal matrix                                            
!          A X = D                                                                                 
! Input:                                                                                           
!  a(*) lower diagonal (Ai,i-1)                                                                  
!  b(*) principal diagonal (Ai,i)                                                                
!  c(*) upper diagonal (Ai,i+1)                                                                  
!  d                                                                                               
! Output                                                                                           
!  x     results                                                                                   
!ccccccccccccccccccccccccccccccc                                                                   

       implicit none
        integer,intent(in)   :: kte
        integer, parameter   :: kts=1
        real, dimension(kte) :: a,b,c,d
        real ,dimension(kte),intent(out) :: x
        integer :: in

!       integer kms,kme,kts,kte,in
!       real a(kms:kme,3),c(kms:kme),x(kms:kme)

        do in=kte-1,kts,-1
         d(in)=d(in)-c(in)*d(in+1)/b(in+1)
         b(in)=b(in)-c(in)*a(in+1)/b(in+1)
        enddo

        do in=kts+1,kte
         d(in)=d(in)-a(in)*d(in-1)/b(in-1)
        enddo

        do in=kts,kte
         x(in)=d(in)/b(in)
        enddo

        return
        end subroutine tridiag3
! ==================================================================
  SUBROUTINE mynn_bl_driver(            &
       &initflag,restart,cycling,       &
       &grav_settling,                  &
       &delt,dz,dx,znt,                 &
       &u,v,w,th,qv,qc,qi,qnc,qni,      &
       &qnwfa,qnifa,                    &
       &p,exner,rho,T3D,                &
       &xland,ts,qsfc,qcg,ps,           &
       &ust,ch,hfx,qfx,rmol,wspd,       &
       &uoce,voce,                      & !ocean current
       &vdfg,                           & !Katata-added for fog dep
       &Qke, & !TKE_PBL,                            &
       &qke_adv,bl_mynn_tkeadvect,      & !ACF for QKE advection
#if (WRF_CHEM == 1)
       chem3d, vd3d, nchem,             & ! WA 7/29/15 For WRF-Chem
       kdvel, ndvel, num_vert_mix,      &
#endif
       &Tsq,Qsq,Cov,                    &
       &RUBLTEN,RVBLTEN,RTHBLTEN,       &
       &RQVBLTEN,RQCBLTEN,RQIBLTEN,     &
       &RQNCBLTEN,RQNIBLTEN,            &
       &RQNWFABLTEN,RQNIFABLTEN,        &
       &exch_h,exch_m,                  &
       &Pblh,kpbl,                      & 
       &el_pbl,                         &
       &dqke,qWT,qSHEAR,qBUOY,qDISS,    & !JOE-TKE BUDGET
       &wstar,delta,                    & !JOE-added for grims
       &bl_mynn_tkebudget,              &
       &bl_mynn_cloudpdf,Sh3D,          &
       &bl_mynn_mixlength,              &
       &icloud_bl,qc_bl,qi_bl,cldfra_bl,&
       &bl_mynn_edmf,                   &
       &bl_mynn_edmf_mom,bl_mynn_edmf_tke, &
       &bl_mynn_mixscalars,             &
       &bl_mynn_output,                 &
       &bl_mynn_cloudmix,bl_mynn_mixqt, &
       &edmf_a,edmf_w,edmf_qt,          &
       &edmf_thl,edmf_ent,edmf_qc,      &
       &sub_thl3D,sub_sqv3D,            &
       &det_thl3D,det_sqv3D,            &
       &nupdraft,maxMF,ktop_plume,      &
       &spp_pbl,pattern_spp_pbl,        &
       &RTHRATEN,                       &
       &FLAG_QC,FLAG_QI,FLAG_QNC,       &
       &FLAG_QNI,FLAG_QNWFA,FLAG_QNIFA  &
       &,IDS,IDE,JDS,JDE,KDS,KDE        &
       &,IMS,IME,JMS,JME,KMS,KME        &
       &,ITS,ITE,JTS,JTE,KTS,KTE)
    
!-------------------------------------------------------------------

    INTEGER, INTENT(in) :: initflag
    !INPUT NAMELIST OPTIONS:
    LOGICAL, INTENT(IN) :: restart,cycling
    INTEGER, INTENT(in) :: grav_settling
    INTEGER, INTENT(in) :: bl_mynn_tkebudget
    INTEGER, INTENT(in) :: bl_mynn_cloudpdf
    INTEGER, INTENT(in) :: bl_mynn_mixlength
    INTEGER, INTENT(in) :: bl_mynn_edmf
    LOGICAL, INTENT(IN) :: bl_mynn_tkeadvect
    INTEGER, INTENT(in) :: bl_mynn_edmf_mom
    INTEGER, INTENT(in) :: bl_mynn_edmf_tke
    INTEGER, INTENT(in) :: bl_mynn_mixscalars
    INTEGER, INTENT(in) :: bl_mynn_output
    INTEGER, INTENT(in) :: bl_mynn_cloudmix
    INTEGER, INTENT(in) :: bl_mynn_mixqt
    INTEGER, INTENT(in) :: icloud_bl

    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC,&
                           FLAG_QNWFA,FLAG_QNIFA
    
    INTEGER,INTENT(IN) :: &
         & IDS,IDE,JDS,JDE,KDS,KDE &
         &,IMS,IME,JMS,JME,KMS,KME &
         &,ITS,ITE,JTS,JTE,KTS,KTE

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

! initflag > 0  for TRUE
! else        for FALSE
!       levflag         : <>3;  Level 2.5
!                         = 3;  Level 3
! grav_settling = 1 when gravitational settling accounted for
! grav_settling = 0 when gravitational settling NOT accounted for
    
    REAL, INTENT(in) :: delt
!WRF
    REAL, INTENT(in) :: dx
!END WRF
!FV3
!     REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(in) :: dx
!END FV3
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(in) :: dz,&
         &u,v,w,th,qv,p,exner,rho,T3D
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(in)::&
         &qc,qi,qni,qnc,qnwfa,qnifa
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(in) :: xland,ust,&
         &ch,rmol,ts,qsfc,qcg,ps,hfx,qfx,wspd,uoce,voce,vdfg,znt

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &Qke,Tsq,Qsq,Cov, &
         !&tke_pbl, & !JOE-added for coupling (TKE_PBL = QKE/2)
         &qke_adv    !ACF for QKE advection

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,&
         &RQIBLTEN,RQNIBLTEN,RTHRATEN,RQNCBLTEN, &
         &RQNWFABLTEN,RQNIFABLTEN

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &exch_h,exch_m

   REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(inout) :: &
         & edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc, &
         & sub_thl3D,sub_sqv3D,det_thl3D,det_sqv3D

    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(inout) :: &
         &Pblh,wstar,delta  !JOE-added for GRIMS

    REAL, DIMENSION(IMS:IME,JMS:JME) :: &
         &Psig_bl,Psig_shcu

    INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: & 
         &KPBL,nupdraft,ktop_plume

    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(OUT) :: &
         &maxmf

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &el_pbl

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &qWT,qSHEAR,qBUOY,qDISS,dqke
    ! 3D budget arrays are not allocated when bl_mynn_tkebudget == 0.
    ! 1D (local) budget arrays are used for passing between subroutines.
    REAL, DIMENSION(KTS:KTE) :: qWT1,qSHEAR1,qBUOY1,qDISS1,dqke1,diss_heat

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: Sh3D

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &qc_bl,qi_bl,cldfra_bl
    REAL, DIMENSION(KTS:KTE) :: qc_bl1D,qi_bl1D,cldfra_bl1D,&
                         qc_bl1D_old,qi_bl1D_old,cldfra_bl1D_old

! WA 7/29/15 Mix chemical arrays
#if (WRF_CHEM == 1)
    INTEGER, INTENT(IN   ) ::   nchem, kdvel, ndvel, num_vert_mix
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme, nchem ), INTENT(INOUT), OPTIONAL :: chem3d
    REAL,    DIMENSION( ims:ime, kdvel, jms:jme, ndvel ), INTENT(IN), OPTIONAL :: vd3d
    REAL,    DIMENSION( kts:kte, nchem ) :: chem1
    REAL,    DIMENSION( kts:kte+1, nchem ) :: s_awchem1
    REAL,    DIMENSION( ndvel ) :: vd1
    INTEGER ic
#endif

!local vars
    INTEGER :: ITF,JTF,KTF, IMD,JMD
    INTEGER :: i,j,k
    REAL, DIMENSION(KTS:KTE) :: thl,thvl,tl,sqv,sqc,sqi,sqw,&
         &El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, &
         &Vt, Vq, sgm, thlsg

    REAL, DIMENSION(KTS:KTE) :: thetav,sh,u1,v1,w1,p1,ex1,dz1,th1,tk1,rho1,&
           & qke1,tsq1,qsq1,cov1,qv1,qi1,qc1,du1,dv1,dth1,dqv1,dqc1,dqi1, &
           & k_m1,k_h1,qni1,dqni1,qnc1,dqnc1,qnwfa1,qnifa1,dqnwfa1,dqnifa1 

!JOE: mass-flux variables
    REAL, DIMENSION(KTS:KTE) :: dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf
    REAL, DIMENSION(KTS:KTE) :: edmf_a1,edmf_w1,edmf_qt1,edmf_thl1,&
                                edmf_ent1,edmf_qc1
    REAL, DIMENSION(KTS:KTE) :: sub_thl,sub_sqv,sub_u,sub_v, &
                        det_thl,det_sqv,det_sqc,det_u,det_v
    REAL,DIMENSION(KTS:KTE+1) :: s_aw1,s_awthl1,s_awqt1,&
                  s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqke1,&
                  s_awqnc1,s_awqni1,s_awqnwfa1,s_awqnifa1

    REAL, DIMENSION(KTS:KTE+1) :: zw
    REAL :: cpm,sqcg,flt,flq,flqv,flqc,pmz,phh,exnerg,zet,&
          & afk,abk,ts_decay, qc_bl2, qi_bl2,             &
          & th_sfc,ztop_plume,sqc9,sqi9

!JOE-add GRIMS parameters & variables
   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   REAL :: govrth, sflux, bfx0, wstar3, wm2, wm3, delb
!JOE-end GRIMS
!JOE-top-down diffusion
   REAL, DIMENSION(ITS:ITE,JTS:JTE) :: maxKHtopdown
   REAL,DIMENSION(KTS:KTE) :: KHtopdown,zfac,wscalek2,&
                             zfacent,TKEprodTD
   REAL :: bfxpbl,dthvx,tmp1,temps,templ,zl1,wstar3_2
   real :: ent_eff,radsum,radflux,we,rcldb,rvls,&
           minrad,zminrad
   real, parameter :: pfac =2.0, zfmin = 0.01, phifac=8.0
   integer :: kk,kminrad
   logical :: cloudflg
!JOE-end top down

    INTEGER, SAVE :: levflag

    LOGICAL :: INITIALIZE_QKE

! Stochastic fields 
     INTEGER,  INTENT(IN)                                               ::spp_pbl
     REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN),OPTIONAL  ::pattern_spp_pbl
     REAL, DIMENSION(KTS:KTE)                         ::    rstoch_col


    IF ( debug_code ) THEN
       print*,'in MYNN driver; at beginning'
    ENDIF

!***  Begin debugging
    IMD=(IMS+IME)/2
    JMD=(JMS+JME)/2
!***  End debugging 

!WRF
    JTF=MIN0(JTE,JDE-1)
    ITF=MIN0(ITE,IDE-1)
    KTF=MIN0(KTE,KDE-1)
!FV3
!    JTF=JTE
!    ITF=ITE
!    KTF=KTE

    levflag=mynn_level

    IF (bl_mynn_edmf > 0) THEN
      ! setup random seed
      !call init_random_seed

      IF (bl_mynn_output > 0) THEN !research mode
         edmf_a(its:ite,kts:kte,jts:jte)=0.
         edmf_w(its:ite,kts:kte,jts:jte)=0.
         edmf_qt(its:ite,kts:kte,jts:jte)=0.
         edmf_thl(its:ite,kts:kte,jts:jte)=0.
         edmf_ent(its:ite,kts:kte,jts:jte)=0.
         edmf_qc(its:ite,kts:kte,jts:jte)=0.
         sub_thl3D(its:ite,kts:kte,jts:jte)=0.
         sub_sqv3D(its:ite,kts:kte,jts:jte)=0.
         det_thl3D(its:ite,kts:kte,jts:jte)=0.
         det_sqv3D(its:ite,kts:kte,jts:jte)=0.
      ENDIF
      ktop_plume(its:ite,jts:jte)=0   !int
      nupdraft(its:ite,jts:jte)=0     !int
      maxmf(its:ite,jts:jte)=0.
    ENDIF
    maxKHtopdown(its:ite,jts:jte)=0.

    IF (initflag > 0) THEN

       !Test to see if we want to initialize qke
       IF ( (restart .or. cycling)) THEN
          IF (MAXVAL(QKE(its:ite,kts,jts:jte)) < 0.0002) THEN
             INITIALIZE_QKE = .TRUE.
             !print*,"QKE is too small, must initialize"
          ELSE
             INITIALIZE_QKE = .FALSE.
             !print*,"Using background QKE, will not initialize"
          ENDIF
       ELSE ! not cycling or restarting:
          INITIALIZE_QKE = .TRUE.
          !print*,"not restart nor cycling, must initialize QKE"
       ENDIF
 
       Sh3D(its:ite,kts:kte,jts:jte)=0.
       el_pbl(its:ite,kts:kte,jts:jte)=0.
       tsq(its:ite,kts:kte,jts:jte)=0.
       qsq(its:ite,kts:kte,jts:jte)=0.
       cov(its:ite,kts:kte,jts:jte)=0.
       dqc1(kts:kte)=0.0
       dqi1(kts:kte)=0.0
       dqni1(kts:kte)=0.0
       dqnc1(kts:kte)=0.0
       dqnwfa1(kts:kte)=0.0
       dqnifa1(kts:kte)=0.0
       qc_bl1D(kts:kte)=0.0
       qi_bl1D(kts:kte)=0.0
       cldfra_bl1D(kts:kte)=0.0
       qc_bl1D_old(kts:kte)=0.0
       cldfra_bl1D_old(kts:kte)=0.0
       edmf_a1(kts:kte)=0.0
       edmf_w1(kts:kte)=0.0
       edmf_qc1(kts:kte)=0.0
       sgm(kts:kte)=0.0
       vt(kts:kte)=0.0
       vq(kts:kte)=0.0

       DO j=JTS,JTF
          DO k=KTS,KTE
             DO i=ITS,ITF
                exch_m(i,k,j)=0.
                exch_h(i,k,j)=0.
            ENDDO
         ENDDO
       ENDDO

       IF ( bl_mynn_tkebudget == 1) THEN
         DO j=JTS,JTF
            DO k=KTS,KTE
               DO i=ITS,ITF
                  qWT(i,k,j)=0.
                  qSHEAR(i,k,j)=0.
                  qBUOY(i,k,j)=0.
                  qDISS(i,k,j)=0.
                  dqke(i,k,j)=0.
               ENDDO
            ENDDO
         ENDDO
       ENDIF

       DO j=JTS,JTF
          DO i=ITS,ITF
             DO k=KTS,KTE !KTF
                dz1(k)=dz(i,k,j)
                u1(k) = u(i,k,j)
                v1(k) = v(i,k,j)
                w1(k) = w(i,k,j)
                th1(k)=th(i,k,j)
                tk1(k)=T3D(i,k,j)
                rho1(k)=rho(i,k,j)
                sqc(k)=qc(i,k,j)/(1.+qv(i,k,j))
                sqv(k)=qv(i,k,j)/(1.+qv(i,k,j))
                thetav(k)=th(i,k,j)*(1.+0.61*sqv(k))
                IF (icloud_bl > 0) THEN
                   CLDFRA_BL1D(k)=CLDFRA_BL(i,k,j)
                   QC_BL1D(k)=QC_BL(i,k,j)
                   QI_BL1D(k)=QI_BL(i,k,j)
                ENDIF
                IF (PRESENT(qi) .AND. FLAG_QI ) THEN
                   sqi(k)=qi(i,k,j)/(1.+qv(i,k,j))
                   sqw(k)=sqv(k)+sqc(k)+sqi(k)
                   thl(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc(k) &
                       &           - xlscp/exner(i,k,j)*sqi(k)
                   !Use form from Tripoli and Cotton (1981) with their
                   !suggested min temperature to improve accuracy.
                   !thl(k)=th(i,k,j)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k) &
                   !    &               - xlscp/MAX(tk1(k),TKmin)*sqi(k))
                   !COMPUTE THL USING SGS CLOUDS FOR PBLH DIAG
                   IF(sqc(k)<1e-6 .and. sqi(k)<1e-8 .and. CLDFRA_BL1D(k)>0.001)THEN
                      sqc9=QC_BL1D(k)*CLDFRA_BL1D(k)
                      sqi9=QI_BL1D(k)*CLDFRA_BL1D(k)
                   ELSE
                      sqc9=sqc(k)
                      sqi9=sqi(k)
                   ENDIF
                   thlsg(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc9 &
                         &           - xlscp/exner(i,k,j)*sqi9
                ELSE
                   sqi(k)=0.0
                   sqw(k)=sqv(k)+sqc(k)
                   thl(k)=th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                   !Use form from Tripoli and Cotton (1981) with their 
                   !suggested min temperature to improve accuracy.      
                   !thl(k)=th(i,k,j)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k))
                   !COMPUTE THL USING SGS CLOUDS FOR PBLH DIAG
                   IF(sqc(k)<1e-6 .and. CLDFRA_BL1D(k)>0.001)THEN
		      sqc9=QC_BL1D(k)*CLDFRA_BL1D(k)
                      sqi9=0.0
                   ELSE
                      sqc9=sqc(k)
                      sqi9=0.0
                   ENDIF
                   thlsg(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc9 &
                         &           - xlscp/exner(i,k,j)*sqi9
                ENDIF
                thvl(k)=thlsg(k)*(1.+0.61*sqv(k))

                IF (k==kts) THEN
                   zw(k)=0.
                ELSE
                   zw(k)=zw(k-1)+dz(i,k-1,j)
                ENDIF
                IF (INITIALIZE_QKE) THEN
                   !Initialize tke for initial PBLH calc only - using 
                   !simple PBLH form of Koracin and Berkowicz (1988, BLM)
                   !to linearly taper off tke towards top of PBL.
                   qke1(k)=5.*ust(i,j) * MAX((ust(i,j)*700. - zw(k))/(MAX(ust(i,j),0.01)*700.), 0.01)
                ELSE
                   qke1(k)=qke(i,k,j)
                ENDIF
                el(k)=el_pbl(i,k,j)
                sh(k)=Sh3D(i,k,j)
                tsq1(k)=tsq(i,k,j)
                qsq1(k)=qsq(i,k,j)
                cov1(k)=cov(i,k,j)
                if (spp_pbl==1) then
                    rstoch_col(k)=pattern_spp_pbl(i,k,j)
                else
                    rstoch_col(k)=0.0
                endif

             ENDDO

             zw(kte+1)=zw(kte)+dz(i,kte,j)

!             CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
             CALL GET_PBLH(KTS,KTE,PBLH(i,j),thvl,&
               &  Qke1,zw,dz1,xland(i,j),KPBL(i,j))
             
             IF (scaleaware > 0.) THEN
                CALL SCALE_AWARE(dx,PBLH(i,j),Psig_bl(i,j),Psig_shcu(i,j))
             ELSE
                Psig_bl(i,j)=1.0
                Psig_shcu(i,j)=1.0
             ENDIF

             CALL mym_initialize (             & 
                  &kts,kte,                    &
                  &dz1, zw, u1, v1, thl, sqv,  &
                  &PBLH(i,j), th1, sh,         &
                  &ust(i,j), rmol(i,j),        &
                  &el, Qke1, Tsq1, Qsq1, Cov1, &
                  &Psig_bl(i,j), cldfra_bl1D,  &
                  &bl_mynn_mixlength,          &
                  &edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf,&
                  &INITIALIZE_QKE,             &
                  &spp_pbl,rstoch_col )

             !UPDATE 3D VARIABLES
             DO k=KTS,KTE !KTF
                el_pbl(i,k,j)=el(k)
                sh3d(i,k,j)=sh(k)
                qke(i,k,j)=qke1(k)
                tsq(i,k,j)=tsq1(k)
                qsq(i,k,j)=qsq1(k)
                cov(i,k,j)=cov1(k)
                !ACF,JOE- initialize qke_adv array if using advection
                IF (bl_mynn_tkeadvect) THEN
                   qke_adv(i,k,j)=qke1(k)
                ENDIF
             ENDDO

!***  Begin debugging
!             k=kdebug
!             IF(I==IMD .AND. J==JMD)THEN
!               PRINT*,"MYNN DRIVER INIT: k=",1," sh=",sh(k)
!               PRINT*," sqw=",sqw(k)," thl=",thl(k)," k_m=",exch_m(i,k,j)
!               PRINT*," xland=",xland(i,j)," rmol=",rmol(i,j)," ust=",ust(i,j)
!               PRINT*," qke=",qke(i,k,j)," el=",el_pbl(i,k,j)," tsq=",Tsq(i,k,j)
!               PRINT*," PBLH=",PBLH(i,j)," u=",u(i,k,j)," v=",v(i,k,j)
!             ENDIF
!***  End debugging

          ENDDO
       ENDDO

    ENDIF ! end initflag

    !ACF- copy qke_adv array into qke if using advection
    IF (bl_mynn_tkeadvect) THEN
       qke=qke_adv
    ENDIF

    DO j=JTS,JTF
       DO i=ITS,ITF
          DO k=KTS,KTE !KTF
            !JOE-TKE BUDGET
             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j)=qke(i,k,j)
             END IF
             IF (icloud_bl > 0) THEN
                CLDFRA_BL1D(k)=CLDFRA_BL(i,k,j)
                QC_BL1D(k)=QC_BL(i,k,j)
                QI_BL1D(k)=QI_BL(i,k,j)
                cldfra_bl1D_old(k)=cldfra_bl(i,k,j)
                qc_bl1D_old(k)=qc_bl(i,k,j)
                qi_bl1D_old(k)=qi_bl(i,k,j)
             ENDIF
             dz1(k)= dz(i,k,j)
             u1(k) = u(i,k,j)
             v1(k) = v(i,k,j)
             w1(k) = w(i,k,j)
             th1(k)= th(i,k,j)
             tk1(k)=T3D(i,k,j)
             rho1(k)=rho(i,k,j)
             qv1(k)= qv(i,k,j)
             qc1(k)= qc(i,k,j)
             sqv(k)= qv(i,k,j)/(1.+qv(i,k,j))
             sqc(k)= qc(i,k,j)/(1.+qv(i,k,j))
             dqc1(k)=0.0
             dqi1(k)=0.0
             dqni1(k)=0.0
             dqnc1(k)=0.0
             dqnwfa1(k)=0.0
             dqnifa1(k)=0.0
             IF(PRESENT(qi) .AND. FLAG_QI)THEN
                qi1(k)= qi(i,k,j)
                sqi(k)= qi(i,k,j)/(1.+qv(i,k,j))
                sqw(k)= sqv(k)+sqc(k)+sqi(k)
                thl(k)= th(i,k,j) - xlvcp/exner(i,k,j)*sqc(k) &
                     &            - xlscp/exner(i,k,j)*sqi(k)
                !Use form from Tripoli and Cotton (1981) with their
                !suggested min temperature to improve accuracy.    
                !thl(k)=th(i,k,j)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k) &
                !    &               - xlscp/MAX(tk1(k),TKmin)*sqi(k))
                !COMPUTE THL USING SGS CLOUDS FOR PBLH DIAG
                IF(sqc(k)<1e-6 .and. sqi(k)<1e-8 .and. CLDFRA_BL1D(k)>0.001)THEN
                   sqc9=QC_BL1D(k)*CLDFRA_BL1D(k)
                   sqi9=QI_BL1D(k)*CLDFRA_BL1D(k)
                ELSE
                   sqc9=sqc(k)
                   sqi9=sqi(k)
                ENDIF
                thlsg(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc9 &
                      &           - xlscp/exner(i,k,j)*sqi9
             ELSE
                qi1(k)=0.0
                sqi(k)=0.0
                sqw(k)= sqv(k)+sqc(k)
                thl(k)= th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                !Use form from Tripoli and Cotton (1981) with their
                !suggested min temperature to improve accuracy.    
                !thl(k)=th(i,k,j)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k))
                !COMPUTE THL USING SGS CLOUDS FOR PBLH DIAG
                IF(sqc(k)<1e-6 .and. CLDFRA_BL1D(k)>0.001)THEN
                   sqc9=QC_BL1D(k)*CLDFRA_BL1D(k)
                   sqi9=QI_BL1D(k)*CLDFRA_BL1D(k)
                ELSE
                   sqc9=sqc(k)
                   sqi9=0.0
                ENDIF
                thlsg(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc9 &
                      &           - xlscp/exner(i,k,j)*sqi9 
            ENDIF
            thetav(k)=th(i,k,j)*(1.+0.608*sqv(k))
            thvl(k)=thlsg(k)*(1.+0.61*sqv(k))

             IF (PRESENT(qni) .AND. FLAG_QNI ) THEN
                qni1(k)=qni(i,k,j)
             ELSE
                qni1(k)=0.0
             ENDIF
             IF (PRESENT(qnc) .AND. FLAG_QNC ) THEN
                qnc1(k)=qnc(i,k,j)
             ELSE
                qnc1(k)=0.0
             ENDIF
             IF (PRESENT(qnwfa) .AND. FLAG_QNWFA ) THEN
                qnwfa1(k)=qnwfa(i,k,j)
             ELSE
                qnwfa1(k)=0.0
             ENDIF
             IF (PRESENT(qnifa) .AND. FLAG_QNIFA ) THEN
                qnifa1(k)=qnifa(i,k,j)
             ELSE
                qnifa1(k)=0.0
             ENDIF
             p1(k) = p(i,k,j)
             ex1(k)= exner(i,k,j)
             el(k) = el_pbl(i,k,j)
             qke1(k)=qke(i,k,j)
             sh(k) = sh3d(i,k,j)
             tsq1(k)=tsq(i,k,j)
             qsq1(k)=qsq(i,k,j)
             cov1(k)=cov(i,k,j)
             if (spp_pbl==1) then
                rstoch_col(k)=pattern_spp_pbl(i,k,j)
             else
                rstoch_col(k)=0.0
             endif


             !edmf
             edmf_a1(k)=0.0
             edmf_w1(k)=0.0
             edmf_qc1(k)=0.0
             s_aw1(k)=0.
             s_awthl1(k)=0.
             s_awqt1(k)=0.
             s_awqv1(k)=0.
             s_awqc1(k)=0.
             s_awu1(k)=0.
             s_awv1(k)=0.
             s_awqke1(k)=0.
             s_awqnc1(k)=0.
             s_awqni1(k)=0.
             s_awqnwfa1(k)=0.
             s_awqnifa1(k)=0.
             sub_thl(k)=0.
             sub_sqv(k)=0.
             sub_u(k)=0.
             sub_v(k)=0.
             det_thl(k)=0.
             det_sqv(k)=0.
             det_sqc(k)=0.
             det_u(k)=0.
             det_v(k)=0.

#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
      IF (PRESENT(chem3d) .AND. PRESENT(vd3d)) THEN
             ! WA 7/29/15 Set up chemical arrays
             DO ic = 1,nchem
                chem1(k,ic) = chem3d(i,k,j,ic)
                s_awchem1(k,ic)=0.
             ENDDO
             DO ic = 1,ndvel
                IF (k == KTS) THEN
                   vd1(ic) = vd3d(i,1,j,ic)
                ENDIF
             ENDDO
      ELSE
             DO ic = 1,nchem
                chem1(k,ic) = 0.
                s_awchem1(k,ic)=0.
             ENDDO
             DO ic = 1,ndvel
                IF (k == KTS) THEN
                   vd1(ic) = 0.
                ENDIF
             ENDDO
      ENDIF
    ENDIF
#endif

             IF (k==kts) THEN
                zw(k)=0.
             ELSE
                zw(k)=zw(k-1)+dz(i,k-1,j)
             ENDIF
          ENDDO ! end k

          zw(kte+1)=zw(kte)+dz(i,kte,j)
          !EDMF
          s_aw1(kte+1)=0.
          s_awthl1(kte+1)=0.
          s_awqt1(kte+1)=0.
          s_awqv1(kte+1)=0.
          s_awqc1(kte+1)=0.
          s_awu1(kte+1)=0.
          s_awv1(kte+1)=0.
          s_awqke1(kte+1)=0.
          s_awqnc1(kte+1)=0.
          s_awqni1(kte+1)=0.
          s_awqnwfa1(kte+1)=0.
          s_awqnifa1(kte+1)=0.
#if (WRF_CHEM == 1)
          DO ic = 1,nchem
             s_awchem1(kte+1,ic)=0.
          ENDDO
#endif

!          CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
          CALL GET_PBLH(KTS,KTE,PBLH(i,j),thvl,&
          & Qke1,zw,dz1,xland(i,j),KPBL(i,j))

          IF (scaleaware > 0.) THEN
             CALL SCALE_AWARE(dx,PBLH(i,j),Psig_bl(i,j),Psig_shcu(i,j))
          ELSE
             Psig_bl(i,j)=1.0
             Psig_shcu(i,j)=1.0
          ENDIF

          sqcg= 0.0   !JOE, it was: qcg(i,j)/(1.+qcg(i,j))
          cpm=cp*(1.+0.84*qv(i,kts,j))
          exnerg=(ps(i,j)/p1000mb)**rcp

          !-----------------------------------------------------
          !ORIGINAL CODE
          !flt = hfx(i,j)/( rho(i,kts,j)*cpm ) &
          ! +xlvcp*ch(i,j)*(sqc(kts)/exner(i,kts,j) -sqcg/exnerg)
          !flq = qfx(i,j)/  rho(i,kts,j)       &
          !    -ch(i,j)*(sqc(kts)   -sqcg )
          !-----------------------------------------------------
          ! Katata-added - The deposition velocity of cloud (fog)
          ! water is used instead of CH.
          flt = hfx(i,j)/( rho(i,kts,j)*cpm ) &
            & +xlvcp*vdfg(i,j)*(sqc(kts)/exner(i,kts,j)- sqcg/exnerg)
          flq = qfx(i,j)/  rho(i,kts,j)       &
            & -vdfg(i,j)*(sqc(kts) - sqcg )
!JOE-test- should this be after the call to mym_condensation?-using old vt & vq
!same as original form
!         flt = flt + xlvcp*ch(i,j)*(sqc(kts)/exner(i,kts,j) -sqcg/exnerg)
          flqv = qfx(i,j)/rho(i,kts,j)
          flqc = -vdfg(i,j)*(sqc(kts) - sqcg )
          th_sfc = ts(i,j)/ex1(kts)

          zet = 0.5*dz(i,kts,j)*rmol(i,j)
          if ( zet >= 0.0 ) then
            pmz = 1.0 + (cphm_st-1.0) * zet
            phh = 1.0 +  cphh_st      * zet
          else
            pmz = 1.0/    (1.0-cphm_unst*zet)**0.25 - zet
            phh = 1.0/SQRT(1.0-cphh_unst*zet)
          end if

          !-- Estimate wstar & delta for GRIMS shallow-cu-------
          govrth = g/th1(kts)
          sflux = hfx(i,j)/rho(i,kts,j)/cpm + &
                  qfx(i,j)/rho(i,kts,j)*ep_1*th1(kts)
          bfx0 = max(sflux,0.)
          wstar3     = (govrth*bfx0*pblh(i,j))
          wstar(i,j) = wstar3**h1
          wm3        = wstar3 + 5.*ust(i,j)**3.
          wm2        = wm3**h2
          delb       = govrth*d3*pblh(i,j)
          delta(i,j) = min(d1*pblh(i,j) + d2*wm2/delb, 100.)
          !-- End GRIMS-----------------------------------------

          CALL  mym_condensation ( kts,kte,      &
               &dx,dz1,zw,thl,sqw,sqv,sqc,sqi,   &
               &p1,ex1,tsq1,qsq1,cov1,           &
               &Sh,el,bl_mynn_cloudpdf,          &
               &qc_bl1D,qi_bl1D,cldfra_bl1D,     &
               &PBLH(i,j),HFX(i,j),              &
               &Vt, Vq, th1, sgm, rmol(i,j),     &
               &spp_pbl, rstoch_col              )

          !ADD TKE source driven by cloud top cooling
          IF (bl_mynn_topdown.eq.1)then
             cloudflg=.false.
             minrad=100.
             kminrad=kpbl(i,j)
             zminrad=PBLH(i,j)
             KHtopdown(kts:kte)=0.0
             TKEprodTD(kts:kte)=0.0
             maxKHtopdown(i,j)=0.0
             !CHECK FOR STRATOCUMULUS-TOPPED BOUNDARY LAYERS
             DO kk = MAX(1,kpbl(i,j)-2),kpbl(i,j)+3
                if(sqc(kk).gt. 1.e-6 .OR. sqi(kk).gt. 1.e-6 .OR. &
                   cldfra_bl1D(kk).gt.0.5) then
                   cloudflg=.true.
                endif
                if(rthraten(i,kk,j) < minrad)then
                   minrad=rthraten(i,kk,j)
                   kminrad=kk
                   zminrad=zw(kk) + 0.5*dz1(kk)
                endif
             ENDDO
             IF (MAX(kminrad,kpbl(i,j)) < 2)cloudflg = .false.
             IF (cloudflg) THEN
                zl1 = dz1(kts)
                k = MAX(kpbl(i,j)-1, kminrad-1)
                !Best estimate of height of TKE source (top of downdrafts):
                !zminrad = 0.5*pblh(i,j) + 0.5*zminrad

                templ=thl(k)*ex1(k)
                !rvls is ws at full level
                rvls=100.*6.112*EXP(17.67*(templ-273.16)/(templ-29.65))*(ep_2/p1(k+1))
                temps=templ + (sqw(k)-rvls)/(cp/xlv  +  ep_2*xlv*rvls/(rd*templ**2))
                rvls=100.*6.112*EXP(17.67*(temps-273.15)/(temps-29.65))*(ep_2/p1(k+1))
                rcldb=max(sqw(k)-rvls,0.)

                !entrainment efficiency
                dthvx     = (thl(k+2) + th1(k+2)*ep_1*sqw(k+2)) &
                          - (thl(k)   + th1(k)  *ep_1*sqw(k))
                dthvx     = max(dthvx,0.1)
                tmp1      = xlvcp * rcldb/(ex1(k)*dthvx)
                !Originally from Nichols and Turton (1986), where a2 = 60, but lowered
                !here to 8, as in Grenier and Bretherton (2001).
                ent_eff   = 0.2 + 0.2*8.*tmp1

                radsum=0.
                DO kk = MAX(1,kpbl(i,j)-3),kpbl(i,j)+3
                   radflux=rthraten(i,kk,j)*ex1(kk)         !converts theta/s to temp/s
                   radflux=radflux*cp/g*(p1(kk)-p1(kk+1)) ! converts temp/s to W/m^2
                   if (radflux < 0.0 ) radsum=abs(radflux)+radsum
                ENDDO

                !More strict limits over land to reduce stable-layer mixouts
                if ((xland(i,j)-1.5).GE.0)THEN ! WATER
                   radsum=MIN(radsum,120.0)
                   bfx0 = max(radsum/rho1(k)/cp,0.)
                else                           ! LAND
                   radsum=MIN(0.25*radsum,30.0)!practically turn off over land
                   bfx0 = max(radsum/rho1(k)/cp - max(sflux,0.0),0.)
                endif

                !entrainment from PBL top thermals
                wm3    = g/thetav(k)*bfx0*MIN(pblh(i,j),1500.) ! this is wstar3(i)
                wm2    = wm2 + wm3**h2
                bfxpbl = - ent_eff * bfx0
                dthvx  = max(thetav(k+1)-thetav(k),0.1)
                we     = max(bfxpbl/dthvx,-sqrt(wm3**h2))

                DO kk = kts,kpbl(i,j)+3
                   !Analytic vertical profile
                   zfac(kk) = min(max((1.-(zw(kk+1)-zl1)/(zminrad-zl1)),zfmin),1.)
                   zfacent(kk) = 10.*MAX((zminrad-zw(kk+1))/zminrad,0.0)*(1.-zfac(kk))**3

                   !Calculate an eddy diffusivity profile (not used at the moment)
                   wscalek2(kk) = (phifac*karman*wm3*(zfac(kk)))**h1
                   !Modify shape of KH to be similar to Lock et al (2000): use pfac = 3.0
                   KHtopdown(kk) = wscalek2(kk)*karman*(zminrad-zw(kk+1))*(1.-zfac(kk))**3 !pfac
                   KHtopdown(kk) = MAX(KHtopdown(kk),0.0)
                   !Do not include xkzm at kpbl-1 since it changes entrainment
                   !if (kk.eq.kpbl(i,j)-1 .and. cloudflg .and. we.lt.0.0) then
                   !   KHtopdown(kk) = 0.0
                   !endif
                   
                   !Calculate TKE production = 2(g/TH)(w'TH'), where w'TH' = A(TH/g)wstar^3/PBLH,
                   !A = ent_eff, and wstar is associated with the radiative cooling at top of PBL.
                   !An analytic profile controls the magnitude of this TKE prod in the vertical. 
                   TKEprodTD(kk)=2.*ent_eff*wm3/MAX(pblh(i,j),100.)*zfacent(kk)
                   TKEprodTD(kk)= MAX(TKEprodTD(kk),0.0)
                ENDDO
             ENDIF !end cloud check
             maxKHtopdown(i,j)=MAXVAL(KHtopdown(:))
          ELSE
             maxKHtopdown(i,j)=0.0
             KHtopdown(kts:kte) = 0.0
             TKEprodTD(kts:kte)=0.0
          ENDIF !end top-down check

          IF (bl_mynn_edmf > 0) THEN
            !PRINT*,"Calling DMP Mass-Flux: i= ",i," j=",j
            CALL DMP_mf(                          &
               &kts,kte,delt,zw,dz1,p1,           &
               &bl_mynn_edmf_mom,                 &
               &bl_mynn_edmf_tke,                 &
               &bl_mynn_mixscalars,               &
               &u1,v1,w1,th1,thl,thetav,tk1,      &
               &sqw,sqv,sqc,qke1,                 &
               &qnc1,qni1,qnwfa1,qnifa1,          &
               &ex1,Vt,Vq,sgm,                    &
               &ust(i,j),flt,flq,flqv,flqc,       &
               &PBLH(i,j),KPBL(i,j),DX,           &
               &xland(i,j),th_sfc,                &
            ! now outputs - tendencies
            ! &,dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf &
            ! outputs - updraft properties
               & edmf_a1,edmf_w1,edmf_qt1,        &
               & edmf_thl1,edmf_ent1,edmf_qc1,    &
            ! for the solver
               & s_aw1,s_awthl1,s_awqt1,          &
               & s_awqv1,s_awqc1,                 &
               & s_awu1,s_awv1,s_awqke1,          &
               & s_awqnc1,s_awqni1,               &
               & s_awqnwfa1,s_awqnifa1,           &
               & sub_thl,sub_sqv,                 &
               & sub_u,sub_v,                     &
               & det_thl,det_sqv,det_sqc,         &
               & det_u,det_v,                     &
#if (WRF_CHEM == 1)
               & nchem,chem1,s_awchem1,           &
#endif
               & qc_bl1D,cldfra_bl1D,             &
               & qc_bl1D_old,cldfra_bl1D_old,     &
               & FLAG_QC,FLAG_QI,                 &
               & FLAG_QNC,FLAG_QNI,               &
               & FLAG_QNWFA,FLAG_QNIFA,           &
               & Psig_shcu(i,j),                  &
               & nupdraft(i,j),ktop_plume(i,j),   &
               & maxmf(i,j),ztop_plume,           &
               & spp_pbl,rstoch_col               &
            )

          ENDIF

          CALL mym_turbulence (                  & 
               &kts,kte,levflag,                 &
               &dz1, zw, u1, v1, thl, sqc, sqw,  &
               &qke1, tsq1, qsq1, cov1,          &
               &vt, vq,                          &
               &rmol(i,j), flt, flq,             &
               &PBLH(i,j),th1,                   &
               &Sh,el,                           &
               &Dfm,Dfh,Dfq,                     &
               &Tcd,Qcd,Pdk,                     &
               &Pdt,Pdq,Pdc,                     &
               &qWT1,qSHEAR1,qBUOY1,qDISS1,      &
               &bl_mynn_tkebudget,               &
               &Psig_bl(i,j),Psig_shcu(i,j),     &     
               &cldfra_bl1D,bl_mynn_mixlength,   &
               &edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf,   &
               &TKEprodTD,                       &
               &spp_pbl,rstoch_col)

          CALL mym_predict (kts,kte,levflag,     &
               &delt, dz1,                       &
               &ust(i,j), flt, flq, pmz, phh,    &
               &el, dfq, pdk, pdt, pdq, pdc,     &
               &Qke1, Tsq1, Qsq1, Cov1,          &
               &s_aw1, s_awqke1, bl_mynn_edmf_tke)

          DO k=kts,kte-1
             ! Set max dissipative heating rate close to 0.1 K per hour (=0.000027...)
             diss_heat(k) = MIN(MAX(twothirds*(qke1(k)**1.5)/(b1*MAX(0.5*(el(k)+el(k+1)),1.))/cp, 0.0),0.00003)
          ENDDO
          diss_heat(kte) = 0.

          CALL mynn_tendencies(kts,kte,          &
               &levflag,grav_settling,           &
               &delt, dz1, rho1,                 &
               &u1, v1, th1, tk1, qv1,           &
               &qc1, qi1, qnc1, qni1,            &
               &p1, ex1, thl, sqv, sqc, sqi, sqw,&
               &qnwfa1, qnifa1,                  &
               &ust(i,j),flt,flq,flqv,flqc,      &
               &wspd(i,j),qcg(i,j),              &
               &uoce(i,j),voce(i,j),             &
               &tsq1, qsq1, cov1,                &
               &tcd, qcd,                        &
               &dfm, dfh, dfq,                   &
               &Du1, Dv1, Dth1, Dqv1,            &
               &Dqc1, Dqi1, Dqnc1, Dqni1,        &
               &Dqnwfa1, Dqnifa1,                &
               &vdfg(i,j), diss_heat,            &
               ! mass flux components
               &s_aw1,s_awthl1,s_awqt1,          &
               &s_awqv1,s_awqc1,s_awu1,s_awv1,   &
               &s_awqnc1,s_awqni1,               &
               &s_awqnwfa1,s_awqnifa1,           &
               &sub_thl,sub_sqv,                 &
               &sub_u,sub_v,                     &
               &det_thl,det_sqv,det_sqc,         &
               &det_u,det_v,                     &
               &FLAG_QC,FLAG_QI,FLAG_QNC,        &
               &FLAG_QNI,FLAG_QNWFA,FLAG_QNIFA,  &
               &cldfra_bl1d,                     &
               &bl_mynn_cloudmix,                &
               &bl_mynn_mixqt,                   &
               &bl_mynn_edmf,                    &
               &bl_mynn_edmf_mom,                &
               &bl_mynn_mixscalars             )

#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
          CALL mynn_mix_chem(kts,kte,           &
               levflag,grav_settling,           &
               delt, dz1,                       &
               nchem, kdvel, ndvel, num_vert_mix, &
               chem1, vd1,                      &
               qnc1,qni1,                       &
               p1, ex1, thl, sqv, sqc, sqi, sqw,&
               ust(i,j),flt,flq,flqv,flqc,      &
               wspd(i,j),qcg(i,j),              &
               uoce(i,j),voce(i,j),             &
               tsq1, qsq1, cov1,                &
               tcd, qcd,                        &
               &dfm, dfh, dfq,                  &
               ! mass flux components
               & s_aw1,                         &
               & s_awchem1,                     &
               &bl_mynn_cloudmix)
    ENDIF
#endif

 
          CALL retrieve_exchange_coeffs(kts,kte,&
               &dfm, dfh, dz1, K_m1, K_h1)

          !UPDATE 3D ARRAYS
          DO k=KTS,KTE !KTF
             exch_m(i,k,j)=K_m1(k)
             exch_h(i,k,j)=K_h1(k)
             RUBLTEN(i,k,j)=du1(k)
             RVBLTEN(i,k,j)=dv1(k)
             RTHBLTEN(i,k,j)=dth1(k)
             RQVBLTEN(i,k,j)=dqv1(k)
             IF(bl_mynn_cloudmix > 0)THEN
               IF (PRESENT(qc) .AND. FLAG_QC) RQCBLTEN(i,k,j)=dqc1(k)
               IF (PRESENT(qi) .AND. FLAG_QI) RQIBLTEN(i,k,j)=dqi1(k)
             ELSE
               IF (PRESENT(qc) .AND. FLAG_QC) RQCBLTEN(i,k,j)=0.
               IF (PRESENT(qi) .AND. FLAG_QI) RQIBLTEN(i,k,j)=0.
             ENDIF
             IF(bl_mynn_cloudmix > 0 .AND. bl_mynn_mixscalars > 0)THEN
               IF (PRESENT(qnc) .AND. FLAG_QNC) RQNCBLTEN(i,k,j)=dqnc1(k)
               IF (PRESENT(qni) .AND. FLAG_QNI) RQNIBLTEN(i,k,j)=dqni1(k)
               IF (PRESENT(qnwfa) .AND. FLAG_QNWFA) RQNWFABLTEN(i,k,j)=dqnwfa1(k)
               IF (PRESENT(qnifa) .AND. FLAG_QNIFA) RQNIFABLTEN(i,k,j)=dqnifa1(k)
             ELSE
               IF (PRESENT(qnc) .AND. FLAG_QNC) RQNCBLTEN(i,k,j)=0.
               IF (PRESENT(qni) .AND. FLAG_QNI) RQNIBLTEN(i,k,j)=0.
               IF (PRESENT(qnwfa) .AND. FLAG_QNWFA) RQNWFABLTEN(i,k,j)=0.
               IF (PRESENT(qnifa) .AND. FLAG_QNIFA) RQNIFABLTEN(i,k,j)=0.
             ENDIF

             IF(icloud_bl > 0)THEN
               !DIAGNOSTIC-DECAY FOR SUBGRID-SCALE CLOUDS
               IF (CLDFRA_BL1D(k) < cldfra_bl1D_old(k)) THEN
                  !DECAY TIMESCALE FOR CALM CONDITION IS THE EDDY TURNOVER
                  !TIMESCALE, BUT FOR WINDY CONDITIONS, IT IS THE ADVECTIVE 
                  !TIMESCALE. USE THE MINIMUM OF THE TWO.
                  ts_decay = MIN( 1800., 3.*dx/MAX(SQRT(u1(k)**2 + v1(k)**2),1.0) )
                  cldfra_bl(i,k,j)= MAX(cldfra_bl1D(k),cldfra_bl1D_old(k)-(0.25*delt/ts_decay))
                  ! qc_bl2 and qi_bl2 are decay rates 
                  qc_bl2          = MAX(qc_bl1D(k),qc_bl1D_old(k))
                  qc_bl2          = MAX(qc_bl2,1.0E-5)
                  qi_bl2          = MAX(qi_bl1D(k),qi_bl1D_old(k))
                  qi_bl2          = MAX(qi_bl2,1.0E-6)
                  qc_bl(i,k,j)    = MAX(qc_bl1D(k),qc_bl1D_old(k)-(MIN(qc_bl2,1.0E-4) * delt/ts_decay))
                  qi_bl(i,k,j)    = MAX(qi_bl1D(k),qi_bl1D_old(k)-(MIN(qi_bl2,1.0E-5) * delt/ts_decay))
                  IF (cldfra_bl(i,k,j) < 0.005 .OR. &
                     (qc_bl(i,k,j) + qi_bl(i,k,j)) < 1E-9) THEN
                     CLDFRA_BL(i,k,j)= 0.
                     QC_BL(i,k,j)    = 0.
                     QI_BL(i,k,j)    = 0.
                  ENDIF
               ELSE
                  qc_bl(i,k,j)=qc_bl1D(k)
                  qi_bl(i,k,j)=qi_bl1D(k)
                  cldfra_bl(i,k,j)=cldfra_bl1D(k)
               ENDIF
             ENDIF

             el_pbl(i,k,j)=el(k)
             qke(i,k,j)=qke1(k)
             tsq(i,k,j)=tsq1(k)
             qsq(i,k,j)=qsq1(k)
             cov(i,k,j)=cov1(k)
             sh3d(i,k,j)=sh(k)

          ENDDO !end-k

          IF ( bl_mynn_tkebudget == 1) THEN
             DO k = kts,kte
                dqke(i,k,j)  = (qke1(k)-dqke(i,k,j))*0.5  !qke->tke
                qWT(i,k,j)   = qWT1(k)*delt
                qSHEAR(i,k,j)= qSHEAR1(k)*delt
                qBUOY(i,k,j) = qBUOY1(k)*delt
                qDISS(i,k,j) = qDISS1(k)*delt
             ENDDO
          ENDIF

          !update updraft properties
          IF (bl_mynn_output > 0) THEN !research mode == 1
             DO k = kts,kte
                edmf_a(i,k,j)=edmf_a1(k)
                edmf_w(i,k,j)=edmf_w1(k)
                edmf_qt(i,k,j)=edmf_qt1(k)
                edmf_thl(i,k,j)=edmf_thl1(k)
                edmf_ent(i,k,j)=edmf_ent1(k)
                edmf_qc(i,k,j)=edmf_qc1(k)
                sub_thl3D(i,k,j)=sub_thl(k)
                sub_sqv3D(i,k,j)=sub_sqv(k)
                det_thl3D(i,k,j)=det_thl(k)
                det_sqv3D(i,k,j)=det_sqv(k)
             ENDDO
          ENDIF

          !***  Begin debug prints
          IF ( debug_code ) THEN
             DO k = kts,kte
               IF ( sh(k) < 0. .OR. sh(k)> 200.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," sh=",sh(k)
               IF ( qke(i,k,j) < -1. .OR. qke(i,k,j)> 200.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," qke=",qke(i,k,j)
               IF ( el_pbl(i,k,j) < 0. .OR. el_pbl(i,k,j)> 2000.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," el_pbl=",el_pbl(i,k,j)
               IF ( ABS(vt(k)) > 0.8 )print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vt=",vt(k)
               IF ( ABS(vq(k)) > 6000.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vq=",vq(k) 
               IF ( exch_m(i,k,j) < 0. .OR. exch_m(i,k,j)> 2000.)print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," exxch_m=",exch_m(i,k,j)
               IF ( vdfg(i,j) < 0. .OR. vdfg(i,j)>5. )print*,&
                  "SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vdfg=",vdfg(i,j)
               IF ( ABS(QFX(i,j))>.001)print*,&
                  "SUSPICIOUS VALUES AT: i,j=",i,j," QFX=",QFX(i,j)
               IF ( ABS(HFX(i,j))>1000.)print*,&
                  "SUSPICIOUS VALUES AT: i,j=",i,j," HFX=",HFX(i,j)
               IF (icloud_bl > 0) then
                  IF( cldfra_bl(i,k,j) < 0.0 .OR. cldfra_bl(i,k,j)> 1.)THEN
                  PRINT*,"SUSPICIOUS VALUES: CLDFRA_BL=",cldfra_bl(i,k,j)," qc_bl=",QC_BL(i,k,j)
                  ENDIF
               ENDIF

               !IF (I==IMD .AND. J==JMD) THEN
               !   PRINT*,"MYNN DRIVER END: k=",k," sh=",sh(k)
               !   PRINT*," sqw=",sqw(k)," thl=",thl(k)," exch_m=",exch_m(i,k,j)
               !   PRINT*," xland=",xland(i,j)," rmol=",rmol(i,j)," ust=",ust(i,j)
               !   PRINT*," qke=",qke(i,k,j)," el=",el_pbl(i,k,j)," tsq=",tsq(i,k,j)
               !   PRINT*," PBLH=",PBLH(i,j)," u=",u(i,k,j)," v=",v(i,k,j)
               !   PRINT*," vq=",vq(k)," vt=",vt(k)," vdfg=",vdfg(i,j)
               !ENDIF
             ENDDO !end-k
          ENDIF
          !***  End debug prints

          !JOE-add tke_pbl for coupling w/shallow-cu schemes (TKE_PBL = QKE/2.)
          !    TKE_PBL is defined on interfaces, while QKE is at middle of layer.
          !tke_pbl(i,kts,j) = 0.5*MAX(qke(i,kts,j),1.0e-10)
          !DO k = kts+1,kte
          !   afk = dz1(k)/( dz1(k)+dz1(k-1) )
          !   abk = 1.0 -afk
          !   tke_pbl(i,k,j) = 0.5*MAX(qke(i,k,j)*abk+qke(i,k-1,j)*afk,1.0e-3)
          !ENDDO

       ENDDO
    ENDDO

!ACF copy qke into qke_adv if using advection
    IF (bl_mynn_tkeadvect) THEN
       qke_adv=qke
    ENDIF
!ACF-end

#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mynn_bl_driver

! ==================================================================
  SUBROUTINE mynn_bl_init_driver(                   &
       &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,          &
       &RQCBLTEN,RQIBLTEN & !,RQNIBLTEN,RQNCBLTEN   &
       &,QKE,                                       &
       &EXCH_H                                      &
       !&,icloud_bl,qc_bl,cldfra_bl                 &
       &,RESTART,ALLOWED_TO_READ,LEVEL              &
       &,IDS,IDE,JDS,JDE,KDS,KDE                    &
       &,IMS,IME,JMS,JME,KMS,KME                    &
       &,ITS,ITE,JTS,JTE,KTS,KTE)

    !---------------------------------------------------------------
    LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
    INTEGER,INTENT(IN) :: LEVEL !,icloud_bl

    INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
         &                IMS,IME,JMS,JME,KMS,KME,                    &
         &                ITS,ITE,JTS,JTE,KTS,KTE
    
    
    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: &
         &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,                 &
         &RQCBLTEN,RQIBLTEN,& !RQNIBLTEN,RQNCBLTEN       &
         &QKE,EXCH_H

!    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: &
!         &qc_bl,cldfra_bl

    INTEGER :: I,J,K,ITF,JTF,KTF
    
    JTF=MIN0(JTE,JDE-1)
    KTF=MIN0(KTE,KDE-1)
    ITF=MIN0(ITE,IDE-1)
    
    IF(.NOT.RESTART)THEN
       DO J=JTS,JTF
          DO K=KTS,KTF
             DO I=ITS,ITF
                RUBLTEN(i,k,j)=0.
                RVBLTEN(i,k,j)=0.
                RTHBLTEN(i,k,j)=0.
                RQVBLTEN(i,k,j)=0.
                if( p_qc >= param_first_scalar ) RQCBLTEN(i,k,j)=0.
                if( p_qi >= param_first_scalar ) RQIBLTEN(i,k,j)=0.
                !if( p_qnc >= param_first_scalar ) RQNCBLTEN(i,k,j)=0.
                !if( p_qni >= param_first_scalar ) RQNIBLTEN(i,k,j)=0.
                !QKE(i,k,j)=0.
                EXCH_H(i,k,j)=0.
!                if(icloud_bl > 0) qc_bl(i,k,j)=0.
!                if(icloud_bl > 0) cldfra_bl(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF

    mynn_level=level

  END SUBROUTINE mynn_bl_init_driver

! ==================================================================

  SUBROUTINE GET_PBLH(KTS,KTE,zi,thetav1D,qke1D,zw1D,dz1D,landsea,kzi)

    !---------------------------------------------------------------
    !             NOTES ON THE PBLH FORMULATION
    !
    !The 1.5-theta-increase method defines PBL heights as the level at 
    !which the potential temperature first exceeds the minimum potential 
    !temperature within the boundary layer by 1.5 K. When applied to 
    !observed temperatures, this method has been shown to produce PBL-
    !height estimates that are unbiased relative to profiler-based 
    !estimates (Nielsen-Gammon et al. 2008). However, their study did not
    !include LLJs. Banta and Pichugina (2008) show that a TKE-based 
    !threshold is a good estimate of the PBL height in LLJs. Therefore,
    !a hybrid definition is implemented that uses both methods, weighting
    !the TKE-method more during stable conditions (PBLH < 400 m).
    !A variable tke threshold (TKEeps) is used since no hard-wired
    !value could be found to work best in all conditions.
    !---------------------------------------------------------------

    INTEGER,INTENT(IN) :: KTS,KTE

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    REAL, INTENT(OUT) :: zi
    REAL, INTENT(IN) :: landsea
    REAL, DIMENSION(KTS:KTE), INTENT(IN) :: thetav1D, qke1D, dz1D
    REAL, DIMENSION(KTS:KTE+1), INTENT(IN) :: zw1D
    !LOCAL VARS
    REAL ::  PBLH_TKE,qtke,qtkem1,wt,maxqke,TKEeps,minthv
    REAL :: delt_thv   !delta theta-v; dependent on land/sea point
    REAL, PARAMETER :: sbl_lim  = 200. !upper limit of stable BL height (m).
    REAL, PARAMETER :: sbl_damp = 400. !transition length for blending (m).
    INTEGER :: I,J,K,kthv,ktke,kzi

    !Initialize KPBL (kzi)
    kzi = 2

    !FIND MIN THETAV IN THE LOWEST 200 M AGL
    k = kts+1
    kthv = 1
    minthv = 9.E9
    DO WHILE (zw1D(k) .LE. 200.)
    !DO k=kts+1,kte-1
       IF (minthv > thetav1D(k)) then
           minthv = thetav1D(k)
           kthv = k
       ENDIF
       k = k+1
       !IF (zw1D(k) .GT. sbl_lim) exit
    ENDDO

    !FIND THETAV-BASED PBLH (BEST FOR DAYTIME).
    zi=0.
    k = kthv+1
    IF((landsea-1.5).GE.0)THEN
        ! WATER
        delt_thv = 1.0
    ELSE
        ! LAND
        delt_thv = 1.25
    ENDIF

    zi=0.
    k = kthv+1
!    DO WHILE (zi .EQ. 0.) 
    DO k=kts+1,kte-1
       IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
          zi = zw1D(k) - dz1D(k-1)* &
             & MIN((thetav1D(k)-(minthv + delt_thv))/ &
             & MAX(thetav1D(k)-thetav1D(k-1),1E-6),1.0)
       ENDIF
       !k = k+1
       IF (k .EQ. kte-1) zi = zw1D(kts+1) !EXIT SAFEGUARD
       IF (zi .NE. 0.0) exit
    ENDDO
    !print*,"IN GET_PBLH:",thsfc,zi

    !FOR STABLE BOUNDARY LAYERS, USE TKE METHOD TO COMPLEMENT THE
    !THETAV-BASED DEFINITION (WHEN THE THETA-V BASED PBLH IS BELOW ~0.5 KM).
    !THE TANH WEIGHTING FUNCTION WILL MAKE THE TKE-BASED DEFINITION NEGLIGIBLE 
    !WHEN THE THETA-V-BASED DEFINITION IS ABOVE ~1 KM.
    ktke = 1
    maxqke = MAX(Qke1D(kts),0.)
    !Use 5% of tke max (Kosovic and Curry, 2000; JAS)
    !TKEeps = maxtke/20. = maxqke/40.
    TKEeps = maxqke/40.
    TKEeps = MAX(TKEeps,0.02) !0.025) 
    PBLH_TKE=0.

    k = ktke+1
!    DO WHILE (PBLH_TKE .EQ. 0.) 
    DO k=kts+1,kte-1
       !QKE CAN BE NEGATIVE (IF CKmod == 0)... MAKE TKE NON-NEGATIVE.
       qtke  =MAX(Qke1D(k)/2.,0.)      ! maximum TKE
       qtkem1=MAX(Qke1D(k-1)/2.,0.)
       IF (qtke .LE. TKEeps) THEN
           PBLH_TKE = zw1D(k) - dz1D(k-1)* &
             & MIN((TKEeps-qtke)/MAX(qtkem1-qtke, 1E-6), 1.0)
           !IN CASE OF NEAR ZERO TKE, SET PBLH = LOWEST LEVEL.
           PBLH_TKE = MAX(PBLH_TKE,zw1D(kts+1))
           !print *,"PBLH_TKE:",i,j,PBLH_TKE, Qke1D(k)/2., zw1D(kts+1)
       ENDIF
       !k = k+1
       IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1) !EXIT SAFEGUARD
       IF (PBLH_TKE .NE. 0.) exit
    ENDDO

    !With TKE advection turned on, the TKE-based PBLH can be very large 
    !in grid points with convective precipitation (> 8 km!),
    !so an artificial limit is imposed to not let PBLH_TKE exceed the
    !theta_v-based PBL height +/- 350 m.
    !This has no impact on 98-99% of the domain, but is the simplest patch
    !that adequately addresses these extremely large PBLHs.
    PBLH_TKE = MIN(PBLH_TKE,zi+350.)
    PBLH_TKE = MAX(PBLH_TKE,MAX(zi-350.,10.))

    wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
    IF (maxqke <= 0.05) THEN
       !Cold pool situation - default to theta_v-based def
    ELSE
       !BLEND THE TWO PBLH TYPES HERE: 
       zi=PBLH_TKE*(1.-wt) + zi*wt
    ENDIF

    !Compute KPBL (kzi)
    DO k=kts+1,kte-1
       IF ( zw1D(k) >= zi) THEN
          kzi = k-1
          exit
       ENDIF
    ENDDO

#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE GET_PBLH
  
! ==================================================================
! Dynamic Multi-Plume (DMP) Mass-Flux Scheme
!
! Much thanks to Kay Suslj of NASA-JPL for contributing the original version
! of this mass-flux scheme. Considerable changes have been made from it's
! original form. Some additions include:
!  1) scale-aware tapering as dx -> 0
!  2) transport of TKE (extra namelist option)
!  3) Chaboureau-Bechtold cloud fraction & coupling to radiation (when icloud_bl > 0)
!  4) some extra limits for numerical stability
! This scheme remains under development, so consider it experimental code. 
!
  SUBROUTINE DMP_mf(                       &
                 & kts,kte,dt,zw,dz,p,      &
                 & momentum_opt,            &
                 & tke_opt,                 &
                 & scalar_opt,              &
                 & u,v,w,th,thl,thv,tk,     &
                 & qt,qv,qc,qke,            &
                 qnc,qni,qnwfa,qnifa,       &
                 & exner,vt,vq,sgm,         &
                 & ust,flt,flq,flqv,flqc,   &
                 & pblh,kpbl,DX,landsea,ts, &
            ! outputs - updraft properties   
                 & edmf_a,edmf_w,           &
                 & edmf_qt,edmf_thl,        &
                 & edmf_ent,edmf_qc,        &
            ! outputs - variables needed for solver 
                 & s_aw,s_awthl,s_awqt,     &
                 & s_awqv,s_awqc,           &
                 & s_awu,s_awv,s_awqke,     &
                 & s_awqnc,s_awqni,         &
                 & s_awqnwfa,s_awqnifa,     &
                 & sub_thl,sub_sqv,         &
                 & sub_u,sub_v,             &
                 & det_thl,det_sqv,det_sqc, &
                 & det_u,det_v,             &
#if (WRF_CHEM == 1)
                 & nchem,chem,s_awchem,     &
#endif
            ! in/outputs - subgrid scale clouds
                 & qc_bl1d,cldfra_bl1d,         &
                 & qc_bl1D_old,cldfra_bl1D_old, &
            ! inputs - flags for moist arrays
                 & F_QC,F_QI,               &
                 F_QNC,F_QNI,               &
                 & F_QNWFA,F_QNIFA,         &
                 & Psig_shcu,               &
            ! output info
                 &nup2,ktop,maxmf,ztop,     &
            ! unputs for stochastic perturbations
                 &spp_pbl,rstoch_col) 

  ! inputs:
     INTEGER, INTENT(IN) :: KTS,KTE,KPBL,momentum_opt,tke_opt,scalar_opt

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

! Stochastic 
     INTEGER,  INTENT(IN)          :: spp_pbl
     REAL, DIMENSION(KTS:KTE)      :: rstoch_col

     REAL,DIMENSION(KTS:KTE), INTENT(IN) :: U,V,W,TH,THL,TK,QT,QV,QC,&
                      exner,dz,THV,P,qke,qnc,qni,qnwfa,qnifa
     REAL,DIMENSION(KTS:KTE+1), INTENT(IN) :: ZW  !height at full-sigma
     REAL, INTENT(IN) :: DT,UST,FLT,FLQ,FLQV,FLQC,PBLH,&
                         DX,Psig_shcu,landsea,ts
     LOGICAL, OPTIONAL :: F_QC,F_QI,F_QNC,F_QNI,F_QNWFA,F_QNIFA

  ! outputs - updraft properties
     REAL,DIMENSION(KTS:KTE), INTENT(OUT) :: edmf_a,edmf_w,        &
                      & edmf_qt,edmf_thl, edmf_ent,edmf_qc
     !add one local edmf variable:
     REAL,DIMENSION(KTS:KTE) :: edmf_th
  ! output
     INTEGER, INTENT(OUT) :: nup2,ktop
     REAL, INTENT(OUT) :: maxmf,ztop
  ! outputs - variables needed for solver
     REAL,DIMENSION(KTS:KTE+1) :: s_aw,      & !sum ai*wis_awphi
                               s_awthl,      & !sum ai*wi*phii
                                s_awqt,      &
                                s_awqv,      &
                                s_awqc,      &
                               s_awqnc,      &
                               s_awqni,      &
                             s_awqnwfa,      &
                             s_awqnifa,      &
                                 s_awu,      &
                                 s_awv,      &
                               s_awqke, s_aw2

     REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: qc_bl1d,cldfra_bl1d, &
                                       qc_bl1d_old,cldfra_bl1d_old

    INTEGER, PARAMETER :: NUP=10, debug_mf=0

  !------------- local variables -------------------
  ! updraft properties defined on interfaces (k=1 is the top of the
  ! first model layer
     REAL,DIMENSION(KTS:KTE+1,1:NUP) :: UPW,UPTHL,UPQT,UPQC,UPQV, &
                                        UPA,UPU,UPV,UPTHV,UPQKE,UPQNC, &
                                        UPQNI,UPQNWFA,UPQNIFA
  ! entrainment variables
     REAL,DIMENSION(KTS:KTE,1:NUP) :: ENT,ENTf
     INTEGER,DIMENSION(KTS:KTE,1:NUP) :: ENTi
  ! internal variables
     INTEGER :: K,I,k50
     REAL :: fltv,wstar,qstar,thstar,sigmaW,sigmaQT,sigmaTH,z0,    &
             pwmin,pwmax,wmin,wmax,wlv,Psig_w,maxw,maxqc,wpbl
     REAL :: B,QTn,THLn,THVn,QCn,Un,Vn,QKEn,QNCn,QNIn,QNWFAn,QNIFAn, &
             Wn2,Wn,EntEXP,EntW,BCOEFF,THVkm1,THVk,Pk

  ! w parameters
     REAL,PARAMETER :: &
          &Wa=2./3., &
          &Wb=0.002,&
          &Wc=1.5 
        
  ! Lateral entrainment parameters ( L0=100 and ENT0=0.1) were taken from
  ! Suselj et al (2013, jas). Note that Suselj et al (2014,waf) use L0=200 and ENT0=0.2.
     REAL,PARAMETER :: &
         & L0=100.,&
         & ENT0=0.1

  ! Implement ideas from Neggers (2016, JAMES):
     REAL, PARAMETER :: Atot = 0.10 ! Maximum total fractional area of all updrafts
     REAL, PARAMETER :: lmax = 1000.! diameter of largest plume
     REAL, PARAMETER :: dl   = 100. ! diff size of each plume - the differential multiplied by the integrand
     REAL, PARAMETER :: dcut = 1.2  ! max diameter of plume to parameterize relative to dx (km)
     REAL ::  d            != -2.3 to -1.7  ;=-1.9 in Neggers paper; power law exponent for number density (N=Cl^d).
          ! Note that changing d to -2.0 makes each size plume equally contribute to the total coverage of all plumes.
          ! Note that changing d to -1.7 doubles the area coverage of the largest plumes relative to the smallest plumes.
     REAL :: cn,c,l,n,an2,hux,maxwidth,wspd_pbl,cloud_base,width_flx

#if (WRF_CHEM == 1)
     INTEGER, INTENT(IN) :: nchem
     REAL,DIMENSION(kts:kte, nchem) :: chem
     REAL,DIMENSION(kts:kte+1, nchem) :: s_awchem
     REAL,DIMENSION(nchem) :: chemn
     REAL,DIMENSION(KTS:KTE+1,1:NUP, nchem) :: UPCHEM
     INTEGER :: ic
     REAL,DIMENSION(KTS:KTE+1, nchem) :: edmf_chem
#endif

  !JOE: add declaration of ERF
   REAL :: ERF

   LOGICAL :: superadiabatic

  ! VARIABLES FOR CHABOUREAU-BECHTOLD CLOUD FRACTION
   REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: vt, vq, sgm
   REAL :: sigq,xl,tlk,qsat_tl,rsl,cpm,a,qmq,mf_cf,Q1,diffqt,&
           Fng,qww,alpha,beta,bb,f,pt,t,q2p,b9,satvp,rhgrid, &
           Ac_mf,Ac_strat,qc_mf

  ! Variables for plume interpolation/saturation check
   REAL,DIMENSION(KTS:KTE) :: exneri,dzi
   REAL ::  THp, QTp, QCp, QCs, esat, qsl

   ! WA TEST 11/9/15 for consistent reduction of updraft params
   REAL :: csigma,acfac

   !JOE- plume overshoot
   INTEGER :: overshoot
   REAL :: bvf, Frz, dzp

   !Flux limiter: not let mass-flux of heat between k=1&2 exceed (fluxportion)*(surface heat flux).
   !This limiter makes adjustments to the entire column.
   REAL :: adjustment, flx1
   REAL, PARAMETER :: fluxportion=0.75 ! set liberally, so has minimal impact. 0.5 starts to have a noticeable impact
                                       ! over land (decrease maxMF by 10-20%), but no impact over water.

   !Subsidence
   REAL,DIMENSION(KTS:KTE) :: sub_thl,sub_sqv,sub_u,sub_v,    &  !tendencies due to subsidence
                      det_thl,det_sqv,det_sqc,det_u,det_v,    &  !tendencied due to detrainment
                 envm_a,envm_w,envm_thl,envm_sqv,envm_sqc,    &
                                       envm_u,envm_v  !environmental variables defined at middle of layer
   REAL,DIMENSION(KTS:KTE+1) ::  envi_a,envi_w        !environmental variables defined at model interface
   REAL :: temp,sublim,qc_ent,qv_ent,qt_ent,thl_ent,detrate,  &
           detrateUV,oow,exc_fac,aratio,detturb,qc_grid
   REAL, PARAMETER :: Cdet = 1./45.
   !parameter "Csub" determines the propotion of upward vertical velocity that contributes to
   !environmenatal subsidence. Some portion is expected to be compensated by downdrafts instead of
   !gentle environmental subsidence. 1.0 assumes all upward vertical velocity in the mass-flux scheme
   !is compensated by "gentle" environmental subsidence. 
   REAL, PARAMETER :: Csub=0.25

! check the inputs
!     print *,'dt',dt
!     print *,'dz',dz
!     print *,'u',u
!     print *,'v',v
!     print *,'thl',thl
!     print *,'qt',qt
!     print *,'ust',ust
!     print *,'flt',flt
!     print *,'flq',flq
!     print *,'pblh',pblh

! Initialize individual updraft properties
  UPW=0.
  UPTHL=0.
  UPTHV=0.
  UPQT=0.
  UPA=0.
  UPU=0.
  UPV=0.
  UPQC=0.
  UPQV=0.
  UPQKE=0.
  UPQNC=0.
  UPQNI=0.
  UPQNWFA=0.
  UPQNIFA=0.
#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
      UPCHEM(KTS:KTE+1,1:NUP,1:nchem)=0.0
    ENDIF
#endif
  ENT=0.001
! Initialize mean updraft properties
  edmf_a  =0.
  edmf_w  =0.
  edmf_qt =0.
  edmf_thl=0.
  edmf_ent=0.
  edmf_qc =0.
#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
      edmf_chem(kts:kte+1,1:nchem) = 0.0
    ENDIF
#endif
! Initialize the variables needed for implicit solver
  s_aw=0.
  s_awthl=0.
  s_awqt=0.
  s_awqv=0.
  s_awqc=0.
  s_awu=0.
  s_awv=0.
  s_awqke=0.
  s_awqnc=0.
  s_awqni=0.
  s_awqnwfa=0.
  s_awqnifa=0.
#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
      s_awchem(kts:kte+1,1:nchem) = 0.0
    ENDIF
#endif
! Initialize explicit tendencies for subsidence & detrainment
  sub_thl = 0.
  sub_sqv = 0.
  sub_u = 0.
  sub_v = 0.
  det_thl = 0.
  det_sqv = 0.
  det_sqc = 0.
  det_u = 0.
  det_v = 0.

  ! Taper off MF scheme when significant resolved-scale motions
  ! are present This function needs to be asymetric...
  k      = 1
  maxw   = 0.0
  cloud_base  = 9000.0
!  DO WHILE (ZW(k) < pblh + 500.)
  DO k=1,kte-1
     IF(ZW(k) > pblh + 500.) exit

     wpbl = w(k)
     IF(w(k) < 0.)wpbl = 2.*w(k)
     maxw = MAX(maxw,ABS(wpbl))

     !Find highest k-level below 50m AGL
     IF(ZW(k)<=50.)k50=k

     !Search for cloud base
     IF(qc(k)>1E-5 .AND. cloud_base == 9000.0)THEN
       cloud_base = 0.5*(ZW(k)+ZW(k+1))
     ENDIF

     !k = k + 1
  ENDDO
  !print*," maxw before manipulation=", maxw
  maxw = MAX(0.,maxw - 1.0)     ! do nothing for small w (< 1 m/s), but
  Psig_w = MAX(0.0, 1.0 - maxw) ! linearly taper off for w > 1.0 m/s
  Psig_w = MIN(Psig_w, Psig_shcu)
  !print*," maxw=", maxw," Psig_w=",Psig_w," Psig_shcu=",Psig_shcu

  fltv = flt + svp1*flq
  !PRINT*," fltv=",fltv," zi=",pblh 

  !Completely shut off MF scheme for strong resolved-scale vertical velocities.
  IF(Psig_w == 0.0 .and. fltv > 0.0) fltv = -1.*fltv

! if surface buoyancy is positive we do integration, otherwise not, and make sure that 
! PBLH > twice the height of the surface layer (set at z0 = 50m)
! Also, ensure that it is at least slightly superadiabatic up through 50 m
      superadiabatic = .false.
  IF((landsea-1.5).GE.0)THEN
     hux = -0.002   ! WATER  ! dT/dz must be < - 0.2 K per 100 m.
  ELSE
     hux = -0.005  ! LAND    ! dT/dz must be < - 0.5 K per 100 m.
  ENDIF
  DO k=1,MAX(1,k50-1) !use "-1" because k50 used interface heights (zw). 
    IF (k == 1) then
      IF ((th(k)-ts)/(0.5*dz(k)) < hux) THEN
        superadiabatic = .true.
      ELSE
        superadiabatic = .false.
        exit
      ENDIF
    ELSE
      IF ((th(k)-th(k-1))/(0.5*(dz(k)+dz(k-1))) < hux) THEN
        superadiabatic = .true.
      ELSE
        superadiabatic = .false.
        exit
      ENDIF
    ENDIF
  ENDDO

  ! Determine the numer of updrafts/plumes in the grid column:
  ! Some of these criteria may be a little redundant but useful for bullet-proofing.
  !   (1) largest plume = 1.0 * dx.
  !   (2) Apply a scale-break, assuming no plumes with diameter larger than PBLH can exist.
  !   (3) max plume size beneath clouds deck approx = 0.5 * cloud_base.
  !   (4) add wspd-dependent limit, when plume model breaks down. (hurricanes)
  !   (5) land-only limit to reduce plume sizes in weakly forced conditions
  ! Criteria (1)
    NUP2 = max(1,min(NUP,INT(dx*dcut/dl)))
  !Criteria (2)
    maxwidth = 1.2*PBLH 
  ! Criteria (3)
    maxwidth = MIN(maxwidth,0.75*cloud_base)
  ! Criteria (4)
    wspd_pbl=SQRT(MAX(u(kts)**2 + v(kts)**2, 0.01))
    !Note: area fraction (acfac) is modified below
  ! Criteria (5)
    IF((landsea-1.5).LT.0)THEN
      width_flx = MAX(MIN(1000.*(0.6*tanh((flt - 0.050)/0.03) + .5),1000.), 0.)
      maxwidth = MIN(maxwidth,width_flx)
    ENDIF
  ! Convert maxwidth to number of plumes
    NUP2 = MIN(MAX(INT((maxwidth - MOD(maxwidth,100.))/100), 0), NUP2)

  !Initialize values:
  ktop = 0
  ztop = 0.0
  maxmf= 0.0

  IF ( fltv > 0.002 .AND. NUP2 .GE. 1 .AND. superadiabatic) then
    !PRINT*," Conditions met to run mass-flux scheme",fltv,pblh

    ! Find coef C for number size density N
    cn = 0.
    d=-1.9  !set d to value suggested by Neggers 2015 (JAMES).
    !d=-1.9 + .2*tanh((fltv - 0.05)/0.15) 
    do I=1,NUP !NUP2
       IF(I > NUP2) exit
       l  = dl*I                            ! diameter of plume
       cn = cn + l**d * (l*l)/(dx*dx) * dl  ! sum fractional area of each plume
    enddo
    C = Atot/cn   !Normalize C according to the defined total fraction (Atot)

    ! Find the portion of the total fraction (Atot) of each plume size:
    An2 = 0.
    do I=1,NUP !NUP2
       IF(I > NUP2) exit
       l  = dl*I                            ! diameter of plume
       N = C*l**d                           ! number density of plume n
       UPA(1,I) = N*l*l/(dx*dx) * dl        ! fractional area of plume n
       ! Make updraft area (UPA) a function of the buoyancy flux
!       acfac = .5*tanh((fltv - 0.03)/0.09) + .5
!       acfac = .5*tanh((fltv - 0.02)/0.09) + .5 
       acfac = .5*tanh((fltv - 0.01)/0.09) + .5

       !add a windspeed-dependent adjustment to acfac that tapers off
       !the mass-flux scheme linearly above sfc wind speeds of 20 m/s:
       acfac = acfac*(1. - MIN(MAX(wspd_pbl - 20.0, 0.0), 10.0)/10.) 

       UPA(1,I)=UPA(1,I)*acfac
       An2 = An2 + UPA(1,I)                 ! total fractional area of all plumes
       !print*," plume size=",l,"; area=",UPA(1,I),"; total=",An2
    end do

    ! set initial conditions for updrafts
    z0=50.
    pwmin=0.1       ! was 0.5
    pwmax=0.4       ! was 3.0

    wstar=max(1.E-2,(g/thv(1)*fltv*pblh)**(1./3.))
    qstar=max(flq,1.0E-5)/wstar
    thstar=flt/wstar

    IF((landsea-1.5).GE.0)THEN
       csigma = 1.34   ! WATER
    ELSE
       csigma = 1.34   ! LAND
    ENDIF

    IF (env_subs) THEN
       exc_fac = 0.0
    ELSE
       exc_fac = 0.58
    ENDIF

    !Note: sigmaW is typically about 0.5*wstar
    sigmaW =1.34*wstar*(z0/pblh)**(1./3.)*(1 - 0.8*z0/pblh)
    sigmaQT=csigma*qstar*(z0/pblh)**(-1./3.)
    sigmaTH=csigma*thstar*(z0/pblh)**(-1./3.)

    !Note: Given the pwmin & pwmax set above, these max/mins are
    !      rarely exceeded. 
    wmin=MIN(sigmaW*pwmin,0.05)
    wmax=MIN(sigmaW*pwmax,0.4)

    !recompute acfac for plume excess
    acfac = .5*tanh((fltv - 0.03)/0.07) + .5

    !SPECIFY SURFACE UPDRAFT PROPERTIES AT MODEL INTERFACE BETWEEN K = 1 & 2
    DO I=1,NUP !NUP2
       IF(I > NUP2) exit
       wlv=wmin+(wmax-wmin)/NUP2*(i-1)

       !SURFACE UPDRAFT VERTICAL VELOCITY
       UPW(1,I)=wmin + REAL(i)/REAL(NUP)*(wmax-wmin)
       !IF (UPW(1,I) > 0.5*ZW(2)/dt) UPW(1,I) = 0.5*ZW(2)/dt

       UPU(1,I)=(U(KTS)*DZ(KTS+1)+U(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPV(1,I)=(V(KTS)*DZ(KTS+1)+V(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQC(1,I)=0
       !UPQC(1,I)=(QC(KTS)*DZ(KTS+1)+QC(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQT(1,I)=(QT(KTS)*DZ(KTS+1)+QT(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))&
           &     +exc_fac*UPW(1,I)*sigmaQT/sigmaW       
       UPTHV(1,I)=(THV(KTS)*DZ(KTS+1)+THV(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1)) &
           &     +exc_fac*UPW(1,I)*sigmaTH/sigmaW
!was       UPTHL(1,I)= UPTHV(1,I)/(1.+svp1*UPQT(1,I))  !assume no saturated parcel at surface
       UPTHL(1,I)=(THL(KTS)*DZ(KTS+1)+THL(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1)) &
           &     +exc_fac*UPW(1,I)*sigmaTH/sigmaW
       UPQKE(1,I)=(QKE(KTS)*DZ(KTS+1)+QKE(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNC(1,I)=(QNC(KTS)*DZ(KTS+1)+QNC(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNI(1,I)=(QNI(KTS)*DZ(KTS+1)+QNI(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNWFA(1,I)=(QNWFA(KTS)*DZ(KTS+1)+QNWFA(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNIFA(1,I)=(QNIFA(KTS)*DZ(KTS+1)+QNIFA(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
    ENDDO

#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
      DO I=1,NUP !NUP2
        IF(I > NUP2) exit
        do ic = 1,nchem
          UPCHEM(1,I,ic)=(CHEM(KTS,ic)*DZ(KTS+1)+CHEM(KTS+1,ic)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
        enddo
      ENDDO
    ENDIF
#endif

    !Initialize environmental variables which can be modified by detrainment
    DO k=kts,kte
       envm_thl(k)=THL(k)
       envm_sqv(k)=QV(k)
       envm_sqc(k)=QC(k)
       envm_u(k)=U(k)
       envm_v(k)=V(k)
    ENDDO

  !QCn = 0.
  ! do integration  updraft
    DO I=1,NUP !NUP2
       IF(I > NUP2) exit
       QCn = 0.
       overshoot = 0
       l  = dl*I                            ! diameter of plume
       DO k=KTS+1,KTE-1
          !w-dependency for entrainment a la Tian and Kuang (2016)
          !ENT(k,i) = 0.35/(MIN(MAX(UPW(K-1,I),0.75),1.9)*l)
          wmin = 0.3 + l*0.0005 !* MAX(pblh-ZW(k+1), 0.0)/pblh
          ENT(k,i) = 0.31/(MIN(MAX(UPW(K-1,I),wmin),1.9)*l)
          !Entrainment from Negggers (2015, JAMES)
          !ENT(k,i) = 0.02*l**-0.35 - 0.0009
          !Minimum background entrainment 
          ENT(k,i) = max(ENT(k,i),0.0003)
          !ENT(k,i) = max(ENT(k,i),0.05/ZW(k))  !not needed for Tian and Kuang
          !JOE - increase entrainment for plumes extending very high.
          IF(ZW(k) >= MIN(pblh+1500., 4000.))THEN
            ENT(k,i)=ENT(k,i) + (ZW(k)-MIN(pblh+1500.,4000.))*5.0E-6
          ENDIF

          !SPP
          ENT(k,i) = ENT(k,i) * (1.0 - rstoch_col(k))

          ENT(k,i) = min(ENT(k,i),0.9/(ZW(k+1)-ZW(k)))

          ! Linear entrainment:
          EntExp= ENT(K,I)*(ZW(k+1)-ZW(k))
          QTn =UPQT(k-1,I) *(1.-EntExp) + QT(k)*EntExp
          THLn=UPTHL(k-1,I)*(1.-EntExp) + THL(k)*EntExp
          Un  =UPU(k-1,I)  *(1.-EntExp) + U(k)*EntExp
          Vn  =UPV(k-1,I)  *(1.-EntExp) + V(k)*EntExp
          QKEn=UPQKE(k-1,I)*(1.-EntExp) + QKE(k)*EntExp
          QNCn=UPQNC(k-1,I)*(1.-EntExp) + QNC(k)*EntExp
          QNIn=UPQNI(k-1,I)*(1.-EntExp) + QNI(k)*EntExp
          QNWFAn=UPQNWFA(k-1,I)*(1.-EntExp) + QNWFA(k)*EntExp
          QNIFAn=UPQNIFA(k-1,I)*(1.-EntExp) + QNIFA(k)*EntExp

          !capture the updated qc, qt & thl modified by entranment alone,
          !since they will be modified later if condensation occurs.
          qc_ent  = QCn
          qt_ent  = QTn
          thl_ent = THLn

          ! Exponential Entrainment:
          !EntExp= exp(-ENT(K,I)*(ZW(k)-ZW(k-1)))
          !QTn =QT(K) *(1-EntExp)+UPQT(K-1,I)*EntExp
          !THLn=THL(K)*(1-EntExp)+UPTHL(K-1,I)*EntExp
          !Un  =U(K)  *(1-EntExp)+UPU(K-1,I)*EntExp
          !Vn  =V(K)  *(1-EntExp)+UPV(K-1,I)*EntExp
          !QKEn=QKE(k)*(1-EntExp)+UPQKE(K-1,I)*EntExp

#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
          do ic = 1,nchem
             ! Exponential Entrainment:
             !chemn(ic) = chem(k,ic)*(1-EntExp)+UPCHEM(K-1,I,ic)*EntExp
             ! Linear entrainment:
             chemn(ic)=UPCHEM(k-1,i,ic)*(1.-EntExp) + chem(k,ic)*EntExp
         enddo
    ENDIF
#endif

          ! Define pressure at model interface
          Pk    =(P(k)*DZ(k+1)+P(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
          ! Compute plume properties thvn and qcn
          call condensation_edmf(QTn,THLn,Pk,ZW(k+1),THVn,QCn)

          ! Define environment THV at the model interface levels
          THVk  =(THV(k)*DZ(k+1)+THV(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
          THVkm1=(THV(k-1)*DZ(k)+THV(k)*DZ(k-1))/(DZ(k-1)+DZ(k))

!          B=g*(0.5*(THVn+UPTHV(k-1,I))/THV(k-1) - 1.0)
          B=g*(THVn/THVk - 1.0)
          IF(B>0.)THEN
            BCOEFF = 0.15        !w typically stays < 2.5, so doesnt hit the limits nearly as much
          ELSE
            BCOEFF = 0.2 !0.33
          ENDIF

          ! Original StEM with exponential entrainment
          !EntW=exp(-2.*(Wb+Wc*ENT(K,I))*(ZW(k)-ZW(k-1)))
          !Wn2=UPW(K-1,I)**2*EntW + (1.-EntW)*0.5*Wa*B/(Wb+Wc*ENT(K,I))
          ! Original StEM with linear entrainment
          !Wn2=UPW(K-1,I)**2*(1.-EntExp) + EntExp*0.5*Wa*B/(Wb+Wc*ENT(K,I))
          !Wn2=MAX(Wn2,0.0)
          !WA: TEMF form
!          IF (B>0.0 .AND. UPW(K-1,I) < 0.2 ) THEN
          IF (UPW(K-1,I) < 0.2 ) THEN
             Wn = UPW(K-1,I) + (-2. * ENT(K,I) * UPW(K-1,I) + BCOEFF*B / MAX(UPW(K-1,I),0.2)) * MIN(ZW(k)-ZW(k-1), 250.)
          ELSE
             Wn = UPW(K-1,I) + (-2. * ENT(K,I) * UPW(K-1,I) + BCOEFF*B / UPW(K-1,I)) * MIN(ZW(k)-ZW(k-1), 250.)
          ENDIF
          !Do not allow a parcel to accelerate more than 1.25 m/s over 200 m.
          !Add max increase of 2.0 m/s for coarse vertical resolution.
          IF(Wn > UPW(K-1,I) + MIN(1.25*(ZW(k)-ZW(k-1))/200., 2.0) ) THEN
             Wn = UPW(K-1,I) + MIN(1.25*(ZW(k)-ZW(k-1))/200., 2.0)
          ENDIF
          !Add symmetrical max decrease in w
          IF(Wn < UPW(K-1,I) - MIN(1.25*(ZW(k)-ZW(k-1))/200., 2.0) ) THEN
             Wn = UPW(K-1,I) - MIN(1.25*(ZW(k)-ZW(k-1))/200., 2.0)
          ENDIF
          Wn = MIN(MAX(Wn,0.0), 3.0)
          !Check to make sure that the plume made it up at least one level.
          !if it failed, then set nup2=0 and exit the mass-flux portion.
          IF (k==kts+1 .AND. Wn == 0.) THEN
             NUP2=0
             exit
          ENDIF

          IF (debug_mf == 1) THEN
            IF (Wn .GE. 3.0) THEN
              ! surface values
              print *," **** SUSPICIOUSLY LARGE W:"
              print *,' QCn:',QCn,' ENT=',ENT(k,i),' Nup2=',Nup2
              print *,'pblh:',pblh,' Wn:',Wn,' UPW(k-1)=',UPW(K-1,I)
              print *,'K=',k,' B=',B,' dz=',ZW(k)-ZW(k-1)
            ENDIF
          ENDIF

          !Allow strongly forced plumes to overshoot if KE is sufficient
          !IF (fltv > 0.05 .AND. Wn <= 0 .AND. overshoot == 0) THEN
          IF (Wn <= 0.0 .AND. overshoot == 0) THEN
             overshoot = 1
             IF ( THVk-THVkm1 .GT. 0.0 ) THEN
                bvf = SQRT( gtr*(THVk-THVkm1)/dz(k) )
                !vertical Froude number
                Frz = UPW(K-1,I)/(bvf*dz(k))
                !IF ( Frz >= 0.5 ) Wn =  MIN(Frz,1.0)*UPW(K-1,I)
                dzp = dz(k)*MAX(MIN(Frz,1.0),0.0) ! portion of highest layer the plume penetrates
             ENDIF
          !ELSEIF (fltv > 0.05 .AND. overshoot == 1) THEN
          ELSE
             dzp = dz(k)
          !   !Do not let overshooting parcel go more than 1 layer up
          !   Wn = 0.0
          ENDIF
          !print*,"k=",k," dzp=",dzp

          !Limit very tall plumes
!          Wn2=Wn2*EXP(-MAX(ZW(k)-(pblh+2000.),0.0)/1000.)
!          IF(ZW(k) >= pblh+3000.)Wn2=0.
          Wn=Wn*EXP(-MAX(ZW(k+1)-MIN(pblh+2000.,3500.),0.0)/1000.)

          !JOE- minimize the plume penetratration in stratocu-topped PBL
   !       IF (fltv < 0.06) THEN
   !          IF(ZW(k+1) >= pblh-200. .AND. qc(k) > 1e-5 .AND. I > 4) Wn=0.
   !       ENDIF

          !Modify environment variables (representative of the model layer - envm*)
          !following the updraft dynamical detrainment of Asai and Kasahara (1967, JAS).
          !Reminder: w is limited to be non-negative (above)
          aratio   = MIN(UPA(K-1,I)/(1.-UPA(K-1,I)), 0.5) !limit should never get hit
          detturb  = 0.00008
          oow      = -0.060/MAX(1.0,(0.5*(Wn+UPW(K-1,I))))   !coef for dynamical detrainment rate
          detrate  = MIN(MAX(oow*(Wn-UPW(K-1,I))/dz(k), detturb), .0002) ! dynamical detrainment rate (m^-1)
          detrateUV= MIN(MAX(oow*(Wn-UPW(K-1,I))/dz(k), detturb), .0001) ! dynamical detrainment rate (m^-1) 
          envm_thl(k)=envm_thl(k) + (0.5*(thl_ent + UPTHL(K-1,I)) - thl(k))*detrate*aratio*MIN(dzp,300.)
          qv_ent = 0.5*(MAX(qt_ent-qc_ent,0.) + MAX(UPQT(K-1,I)-UPQC(K-1,I),0.))
          envm_sqv(k)=envm_sqv(k) + (qv_ent-QV(K))*detrate*aratio*MIN(dzp,300.)
          IF (UPQC(K-1,I) > 1E-8) THEN
             IF (QC(K) > 1E-6) THEN
                qc_grid = QC(K)
             ELSE
                qc_grid = cldfra_bl1d(k)*qc_bl1d(K)
             ENDIF
             envm_sqc(k)=envm_sqc(k) + MAX(UPA(K-1,I)*0.5*(QCn + UPQC(K-1,I)) - qc_grid, 0.0)*detrate*aratio*MIN(dzp,300.)
          ENDIF
          envm_u(k)  =envm_u(k)   + (0.5*(Un + UPU(K-1,I)) - U(K))*detrateUV*aratio*MIN(dzp,300.)
          envm_v(k)  =envm_v(k)   + (0.5*(Vn + UPV(K-1,I)) - V(K))*detrateUV*aratio*MIN(dzp,300.)

          IF (Wn > 0.) THEN
             !Update plume variables at current k index
             UPW(K,I)=Wn  !Wn !sqrt(Wn2)
             UPTHV(K,I)=THVn
             UPTHL(K,I)=THLn
             UPQT(K,I)=QTn
             UPQC(K,I)=QCn
             UPU(K,I)=Un
             UPV(K,I)=Vn
             UPQKE(K,I)=QKEn
             UPQNC(K,I)=QNCn
             UPQNI(K,I)=QNIn
             UPQNWFA(K,I)=QNWFAn
             UPQNIFA(K,I)=QNIFAn
             UPA(K,I)=UPA(K-1,I)
#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
             do ic = 1,nchem
                UPCHEM(k,I,ic) = chemn(ic)
             enddo
    ENDIF
#endif
             ktop = MAX(ktop,k)
          ELSE
             exit  !exit k-loop
          END IF
       ENDDO
       IF (debug_mf == 1) THEN
          IF (MAXVAL(UPW(:,I)) > 10.0 .OR. MINVAL(UPA(:,I)) < 0.0 .OR. &
              MAXVAL(UPA(:,I)) > Atot .OR. NUP2 > 10) THEN
             ! surface values
             print *,'flq:',flq,' fltv:',fltv,' Nup2=',Nup2
             print *,'pblh:',pblh,' wstar:',wstar,' ktop=',ktop
             print *,'sigmaW=',sigmaW,' sigmaTH=',sigmaTH,' sigmaQT=',sigmaQT
             ! means
             print *,'u:',u
             print *,'v:',v
             print *,'thl:',thl
             print *,'UPA:',UPA(:,I)
             print *,'UPW:',UPW(:,I)
             print *,'UPTHL:',UPTHL(:,I)
             print *,'UPQT:',UPQT(:,I)
             print *,'ENT:',ENT(:,I)
          ENDIF
       ENDIF
    ENDDO
  ELSE
    !At least one of the conditions was not met for activating the MF scheme.
    NUP2=0. 
  END IF !end criteria for mass-flux scheme

  ktop=MIN(ktop,KTE-1)  !  Just to be safe...
  IF (ktop == 0) THEN
     ztop = 0.0
  ELSE
     ztop=zw(ktop)
  ENDIF

  IF(nup2 > 0) THEN

    !Calculate the fluxes for each variable
    !All s_aw* variable are == 0 at k=1
    DO k=KTS,KTE
      IF(k > KTOP) exit
      DO i=1,NUP !NUP2
        IF(I > NUP2) exit
        s_aw(k+1)   = s_aw(k+1)    + UPA(K,i)*UPW(K,i)*Psig_w
        s_awthl(k+1)= s_awthl(k+1) + UPA(K,i)*UPW(K,i)*UPTHL(K,i)*Psig_w
        s_awqt(k+1) = s_awqt(k+1)  + UPA(K,i)*UPW(K,i)*UPQT(K,i)*Psig_w
        s_awqc(k+1) = s_awqc(k+1)  + UPA(K,i)*UPW(K,i)*UPQC(K,i)*Psig_w
        IF (momentum_opt > 0) THEN
          s_awu(k+1)  = s_awu(k+1)   + UPA(K,i)*UPW(K,i)*UPU(K,i)*Psig_w
          s_awv(k+1)  = s_awv(k+1)   + UPA(K,i)*UPW(K,i)*UPV(K,i)*Psig_w
        ENDIF
        IF (tke_opt > 0) THEN
          s_awqke(k+1)= s_awqke(k+1) + UPA(K,i)*UPW(K,i)*UPQKE(K,i)*Psig_w
        ENDIF
      ENDDO
      s_awqv(k+1) = s_awqt(k+1)  - s_awqc(k+1)
    ENDDO
#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
      DO k=KTS,KTE
        IF(k > KTOP) exit
        DO i=1,NUP !NUP2
          IF(I > NUP2) exit
          do ic = 1,nchem
            s_awchem(k+1,ic) = s_awchem(k+1,ic) + UPA(K,i)*UPW(K,i)*UPCHEM(K,i,ic)*Psig_w
          enddo
        ENDDO
      ENDDO
    ENDIF
#endif

    IF (scalar_opt > 0) THEN
      DO k=KTS,KTE
        IF(k > KTOP) exit
        DO I=1,NUP !NUP2
          IF (I > NUP2) exit
          s_awqnc(k+1)= s_awqnc(K+1) + UPA(K,i)*UPW(K,i)*UPQNC(K,i)*Psig_w
          s_awqni(k+1)= s_awqni(K+1) + UPA(K,i)*UPW(K,i)*UPQNI(K,i)*Psig_w
          s_awqnwfa(k+1)= s_awqnwfa(K+1) + UPA(K,i)*UPW(K,i)*UPQNWFA(K,i)*Psig_w
          s_awqnifa(k+1)= s_awqnifa(K+1) + UPA(K,i)*UPW(K,i)*UPQNIFA(K,i)*Psig_w
        ENDDO
      ENDDO
    ENDIF

    !Flux limiter: Check ratio of heat flux at top of first model layer
    !and at the surface. Make sure estimated flux out of the top of the
    !layer is < fluxportion*surface_heat_flux
    IF (s_aw(kts+1) /= 0.) THEN
       dzi(kts) = 0.5*(DZ(kts)+DZ(kts+1)) !dz centered at model interface
       flx1   = MAX(s_aw(kts+1)*(TH(kts)-TH(kts+1))/dzi(kts),1.0e-5)
    ELSE
       flx1 = 0.0
       !print*,"ERROR: s_aw(kts+1) == 0, NUP=",NUP," NUP2=",NUP2,&
       !       " superadiabatic=",superadiabatic," KTOP=",KTOP
    ENDIF
    adjustment=1.0
    !Print*,"Flux limiter in MYNN-EDMF, adjustment=",fluxportion*flt/dz(kts)/flx1
    !Print*,"flt/dz=",flt/dz(kts)," flx1=",flx1," s_aw(kts+1)=",s_aw(kts+1)
    IF (flx1 > fluxportion*flt/dz(kts) .AND. flx1>0.0) THEN
       adjustment= fluxportion*flt/dz(kts)/flx1
       s_aw   = s_aw*adjustment
       s_awthl= s_awthl*adjustment
       s_awqt = s_awqt*adjustment
       s_awqc = s_awqc*adjustment
       s_awqv = s_awqv*adjustment
       s_awqnc= s_awqnc*adjustment
       s_awqni= s_awqni*adjustment
       s_awqnwfa= s_awqnwfa*adjustment
       s_awqnifa= s_awqnifa*adjustment
       IF (momentum_opt > 0) THEN
          s_awu  = s_awu*adjustment
          s_awv  = s_awv*adjustment
       ENDIF
       IF (tke_opt > 0) THEN
          s_awqke= s_awqke*adjustment
       ENDIF
#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
       s_awchem = s_awchem*adjustment
    ENDIF
#endif
       UPA = UPA*adjustment
    ENDIF
    !Print*,"adjustment=",adjustment," fluxportion=",fluxportion," flt=",flt

    !Calculate mean updraft properties for output:
    !all edmf_* variables at k=1 correspond to the interface at top of first model layer
    DO k=KTS,KTE-1
      IF(k > KTOP) exit
      DO I=1,NUP !NUP2
        IF(I > NUP2) exit
        edmf_a(K)  =edmf_a(K)  +UPA(K,i)
        edmf_w(K)  =edmf_w(K)  +UPA(K,i)*UPW(K,i)
        edmf_qt(K) =edmf_qt(K) +UPA(K,i)*UPQT(K,i)
        edmf_thl(K)=edmf_thl(K)+UPA(K,i)*UPTHL(K,i)
        edmf_ent(K)=edmf_ent(K)+UPA(K,i)*ENT(K,i)
        edmf_qc(K) =edmf_qc(K) +UPA(K,i)*UPQC(K,i)
#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
        do ic = 1,nchem
          edmf_chem(k,ic) = edmf_chem(k,ic) + UPA(K,I)*UPCHEM(k,i,ic)
        enddo
    ENDIF
#endif
      ENDDO

      !Note that only edmf_a is multiplied by Psig_w. This takes care of the
      !scale-awareness of the subsidence below:
      IF (edmf_a(k)>0.) THEN
        edmf_w(k)=edmf_w(k)/edmf_a(k)
        edmf_qt(k)=edmf_qt(k)/edmf_a(k)
        edmf_thl(k)=edmf_thl(k)/edmf_a(k)
        edmf_ent(k)=edmf_ent(k)/edmf_a(k)
        edmf_qc(k)=edmf_qc(k)/edmf_a(k)
#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
        do ic = 1,nchem
          edmf_chem(k,ic) = edmf_chem(k,ic)/edmf_a(k)
        enddo
    ENDIF
#endif
        edmf_a(k)=edmf_a(k)*Psig_w
        !FIND MAXIMUM MASS-FLUX IN THE COLUMN:
        IF(edmf_a(k)*edmf_w(k) > maxmf) maxmf = edmf_a(k)*edmf_w(k)
      ENDIF
    ENDDO

    !Calculate the effects environmental subsidence.
     !All envi_*variables are valid at the interfaces, like the edmf_* variables
    IF (env_subs) THEN
       DO k=KTS+1,KTE-1
          !First, smooth the profiles of w & a, since sharp vertical gradients
          !in plume variables are not likely extended to env variables
          !Note1: w is treated as negative further below
          !Note2: both w & a will be transformed into env variables further below
          envi_w(k) = onethird*(edmf_w(K-1)+edmf_w(K)+edmf_w(K+1))
          envi_a(k) = onethird*(edmf_a(k-1)+edmf_a(k)+edmf_a(k+1))*adjustment
       ENDDO
       !define env variables at k=1 (top of first model layer)
       envi_w(kts) = edmf_w(kts)
       envi_a(kts) = edmf_a(kts)
       !define env variables at k=kte
       envi_w(kte) = 0.0
       envi_a(kte) = edmf_a(kte)
       !define env variables at k=kte+1
       envi_w(kte+1) = 0.0
       envi_a(kte+1) = edmf_a(kte)
       !Add limiter for very long time steps (i.e. dt > 300 s)
       !Note that this is not a robust check - only for violations in
       !   the first model level.
       IF (envi_w(kts) > 0.9*DZ(kts)/dt) THEN
          sublim = 0.9*DZ(kts)/dt/envi_w(kts)
       ELSE
          sublim = 1.0
       ENDIF
       !Transform w & a into env variables
       DO k=KTS,KTE
          temp=envi_a(k)
          envi_a(k)=1.0-temp
          envi_w(k)=csub*sublim*envi_w(k)*temp/(1.-temp)
       ENDDO
       !calculate tendencies from subsidence and detrainment valid at the middle of
       !each model layer
       dzi(kts)    = 0.5*(DZ(kts)+DZ(kts+1))
       sub_thl(kts)=0.5*envi_w(kts)*envi_a(kts)*(thl(kts+1)-thl(kts))/dzi(kts)
       sub_sqv(kts)=0.5*envi_w(kts)*envi_a(kts)*(qv(kts+1)-qv(kts))/dzi(kts)
       DO k=KTS+1,KTE-1
          dzi(k)    = 0.5*(DZ(k)+DZ(k+1))
          sub_thl(k)=0.5*(envi_w(k)+envi_w(k-1))*0.5*(envi_a(k)+envi_a(k-1)) * &
                      (thl(k+1)-thl(k))/dzi(k)
          sub_sqv(k)=0.5*(envi_w(k)+envi_w(k-1))*0.5*(envi_a(k)+envi_a(k-1)) * &
                      (qv(k+1)-qv(k))/dzi(k) 
       ENDDO

       DO k=KTS,KTE-1
          det_thl(k)=Cdet*(envm_thl(k)-thl(k))*envi_a(k)*Psig_w
          det_sqv(k)=Cdet*(envm_sqv(k)-qv(k))*envi_a(k)*Psig_w
          det_sqc(k)=Cdet*(envm_sqc(k)-qc(k))*envi_a(k)*Psig_w
       ENDDO
       IF (momentum_opt > 0) THEN
         sub_u(kts)=0.5*envi_w(kts)*envi_a(kts)*(u(kts+1)-u(kts))/dzi(kts)
         sub_v(kts)=0.5*envi_w(kts)*envi_a(kts)*(v(kts+1)-v(kts))/dzi(kts)
         DO k=KTS+1,KTE-1
            sub_u(k)=0.5*(envi_w(k)+envi_w(k-1))*0.5*(envi_a(k)+envi_a(k-1)) * &
                      (u(k+1)-u(k))/dzi(k)
            sub_v(k)=0.5*(envi_w(k)+envi_w(k-1))*0.5*(envi_a(k)+envi_a(k-1)) * &
                      (v(k+1)-v(k))/dzi(k)
         ENDDO

         DO k=KTS,KTE-1
           det_u(k) = Cdet*(envm_u(k)-u(k))*envi_a(k)*Psig_w
           det_v(k) = Cdet*(envm_v(k)-v(k))*envi_a(k)*Psig_w
         ENDDO
       ENDIF
    ENDIF !end subsidence/env detranment

    !First, compute exner, plume theta, and dz centered at interface
    !Here, k=1 is the top of the first model layer. These values do not 
    !need to be defined at k=kte (unused level).
    DO K=KTS,KTE-1
       exneri(k) = (exner(k)*DZ(k+1)+exner(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
       edmf_th(k)= edmf_thl(k) + xlvcp/exneri(k)*edmf_qc(K)
       dzi(k)    = 0.5*(DZ(k)+DZ(k+1))
    ENDDO

!JOE: ADD CLDFRA_bl1d, qc_bl1d. Note that they have already been defined in
!     mym_condensation. Here, a shallow-cu component is added, but no cumulus
!     clouds can be added at k=1 (start loop at k=2).  
    DO K=KTS+1,KTE-2
        IF(k > KTOP) exit
        IF(0.5*(edmf_qc(k)+edmf_qc(k-1))>0.0)THEN

            satvp = 3.80*exp(17.27*(th(k)-273.)/ &
                   (th(k)-36.))/(.01*p(k))
            rhgrid = max(.01,MIN( 1., qv(k) /satvp))

            !then interpolate plume thl, th, and qt to mass levels
            THp = (edmf_th(k)*dzi(k-1)+edmf_th(k-1)*dzi(k))/(dzi(k-1)+dzi(k))
            QTp = (edmf_qt(k)*dzi(k-1)+edmf_qt(k-1)*dzi(k))/(dzi(k-1)+dzi(k))
            !convert TH to T
            t = THp*exner(k)
            !SATURATED VAPOR PRESSURE
            esat = esat_blend(t)
            !SATURATED SPECIFIC HUMIDITY
            qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat)) 

            !condensed liquid in the plume on mass levels
            IF (edmf_qc(k)>0.0 .AND. edmf_qc(k-1)>0.0)THEN
              QCp = 0.5*(edmf_qc(k)+edmf_qc(k-1))
            ELSE
              QCp = MAX(0.0, QTp-qsl)
            ENDIF

            !COMPUTE CLDFRA & QC_BL FROM MASS-FLUX SCHEME and recompute vt & vq

            xl = xl_blend(tk(k))                ! obtain blended heat capacity 
            tlk = thl(k)*(p(k)/p1000mb)**rcp    ! recover liquid temp (tl) from thl
            qsat_tl = qsat_blend(tlk,p(k))      ! get saturation water vapor mixing ratio
                                                !   at tl and p
            rsl = xl*qsat_tl / (r_v*tlk**2)     ! slope of C-C curve at t = tl
                                                ! CB02, Eqn. 4
            cpm = cp + qt(k)*cpv                ! CB02, sec. 2, para. 1
            a   = 1./(1. + xl*rsl/cpm)          ! CB02 variable "a"
            b9  = a*rsl                         ! CB02 variable "b" 

            q2p  = xlvcp/exner(k)
            pt = thl(k) +q2p*QCp*0.5*(edmf_a(k)+edmf_a(k-1)) ! potential temp (env + plume)
            bb = b9*tk(k)/pt ! bb is "b9" in BCMT95.  Their "b9" differs from
                           ! "b9" in CB02 by a factor
                           ! of T/theta.  Strictly, b9 above is formulated in
                           ! terms of sat. mixing ratio, but bb in BCMT95 is
                           ! cast in terms of sat. specific humidity.  The
                           ! conversion is neglected here.
            qww   = 1.+0.61*qt(k)
            alpha = 0.61*pt
            t     = TH(k)*exner(k)
            beta  = pt*xl/(t*cp) - 1.61*pt
            !Buoyancy flux terms have been moved to the end of this section...

            !Now calculate convective component of the cloud fraction:
            if (a > 0.0) then
               f = MIN(1.0/a, 4.0)              ! f is vertical profile scaling function (CB2005)
            else
               f = 1.0
            endif
            sigq = 9.E-3 * 0.5*(edmf_a(k)+edmf_a(k-1)) * &
               &           0.5*(edmf_w(k)+edmf_w(k-1)) * f       ! convective component of sigma (CB2005)
            !sigq = MAX(sigq, 1.0E-4)
            sigq = SQRT(sigq**2 + sgm(k)**2)    ! combined conv + stratus components

            qmq = a * (qt(k) - qsat_tl)           ! saturation deficit/excess;
                                                !   the numerator of Q1
            mf_cf = min(max(0.5 + 0.36 * atan(1.55*(qmq/sigq)),0.01),0.6)
            IF ( debug_code ) THEN
               print*,"In MYNN, StEM edmf"
               print*,"  CB: env qt=",qt(k)," qsat=",qsat_tl
               print*,"      satdef=",QTp - qsat_tl
               print*,"  CB: sigq=",sigq," qmq=",qmq," tlk=",tlk
               print*,"  CB: mf_cf=",mf_cf," cldfra_bl=",cldfra_bl1d(k)," edmf_a=",edmf_a(k)
            ENDIF

            ! Update cloud fractions and specific humidities in grid cells
            ! where the mass-flux scheme is active. Now, we also use the
            ! stratus component of the SGS clouds as well. The stratus cloud 
            ! fractions (Ac_strat) are reduced slightly to give way to the 
            ! mass-flux SGS cloud fractions (Ac_mf).
            IF (cldfra_bl1d(k) < 0.5) THEN
               IF (mf_cf > 0.5*(edmf_a(k)+edmf_a(k-1))) THEN
                  !cldfra_bl1d(k) = mf_cf
                  !qc_bl1d(k) = QCp*0.5*(edmf_a(k)+edmf_a(k-1))/mf_cf
                  Ac_mf      = mf_cf
                  Ac_strat   = cldfra_bl1d(k)*(1.0-mf_cf)
                  cldfra_bl1d(k) = Ac_mf + Ac_strat
                  !dillute Qc from updraft area to larger cloud area
                  qc_mf      = QCp*0.5*(edmf_a(k)+edmf_a(k-1))/mf_cf
                  !The mixing ratios from the stratus component are not well
                  !estimated in shallow-cumulus regimes. Ensure stratus clouds 
                  !have mixing ratio similar to cumulus
                  QCs        = MIN(MAX(qc_bl1d(k), 0.5*qc_mf), 5E-4)
                  qc_bl1d(k) = (qc_mf*Ac_mf + QCs*Ac_strat)/cldfra_bl1d(k)
               ELSE
                  !cldfra_bl1d(k)=0.5*(edmf_a(k)+edmf_a(k-1))
                  !qc_bl1d(k) = QCp
                  Ac_mf      = 0.5*(edmf_a(k)+edmf_a(k-1))
                  Ac_strat   = cldfra_bl1d(k)*(1.0-Ac_mf)
                  cldfra_bl1d(k)=Ac_mf + Ac_strat
                  qc_mf      = QCp
                  !Ensure stratus clouds have mixing ratio similar to cumulus
                  QCs        = MIN(MAX(qc_bl1d(k), 0.5*qc_mf), 5E-4)
                  qc_bl1d(k) = (QCp*Ac_mf + QCs*Ac_strat)/cldfra_bl1d(k)
               ENDIF
            ELSE
               Ac_mf = mf_cf
            ENDIF

            !Now recalculate the terms for the buoyancy flux for mass-flux clouds:
            !See mym_condensation for details on these formulations.  The
            !cloud-fraction bounding was added to improve cloud retention,
            !following RAP and HRRR testing.
            !Fng = 2.05 ! the non-Gaussian transport factor (assumed constant)
            !Use Bechtold and Siebesma (1998) piecewise estimation of Fng:
            Q1 = qmq/MAX(sigq,1E-10)
            Q1=MAX(Q1,-5.0)
            IF (Q1 .GE. 1.0) THEN
               Fng = 1.0
            ELSEIF (Q1 .GE. -1.7 .AND. Q1 < 1.0) THEN
               Fng = EXP(-0.4*(Q1-1.0))
            ELSEIF (Q1 .GE. -2.5 .AND. Q1 .LT. -1.7) THEN
               Fng = 3.0 + EXP(-3.8*(Q1+1.7))
            ELSE
               Fng = MIN(23.9 + EXP(-1.6*(Q1+2.5)), 60.)
            ENDIF

            vt(k) = qww   - MIN(0.40,Ac_mf)*beta*bb*Fng - 1.
            vq(k) = alpha + MIN(0.40,Ac_mf)*beta*a*Fng  - tv0
         ENDIF

      ENDDO

    ENDIF  !end nup2 > 0

    !modify output (negative: dry plume, positive: moist plume)
    IF (ktop > 0) THEN
      maxqc = maxval(edmf_qc(1:ktop)) 
      IF ( maxqc < 1.E-8) maxmf = -1.0*maxmf
    ENDIF

!
! debugging   
!
IF (edmf_w(1) > 4.0) THEN 
! surface values
    print *,'flq:',flq,' fltv:',fltv
    print *,'pblh:',pblh,' wstar:',wstar
    print *,'sigmaW=',sigmaW,' sigmaTH=',sigmaTH,' sigmaQT=',sigmaQT
! means
!   print *,'u:',u
!   print *,'v:',v  
!   print *,'thl:',thl
!   print *,'thv:',thv
!   print *,'qt:',qt
!   print *,'p:',p
 
! updrafts
! DO I=1,NUP2
!   print *,'up:A',i
!   print *,UPA(:,i)
!   print *,'up:W',i
!   print*,UPW(:,i)
!   print *,'up:thv',i
!   print *,UPTHV(:,i)
!   print *,'up:thl',i 
!   print *,UPTHL(:,i)
!   print *,'up:qt',i
!   print *,UPQT(:,i)
!   print *,'up:tQC',i
!   print *,UPQC(:,i)
!   print *,'up:ent',i
!   print *,ENT(:,i)   
! ENDDO
 
! mean updrafts
   print *,' edmf_a',edmf_a(1:14)
   print *,' edmf_w',edmf_w(1:14)
   print *,' edmf_qt:',edmf_qt(1:14)
   print *,' edmf_thl:',edmf_thl(1:14)
 
ENDIF !END Debugging


#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

END SUBROUTINE DMP_MF
!=================================================================

subroutine condensation_edmf(QT,THL,P,zagl,THV,QC)
!
! zero or one condensation for edmf: calculates THV and QC
!
real,intent(in)   :: QT,THL,P,zagl
real,intent(out)  :: THV
real,intent(inout):: QC

integer :: niter,i
real :: diff,exn,t,th,qs,qcold

! constants used from module_model_constants.F
! p1000mb
! rcp ... Rd/cp
! xlv ... latent heat for water (2.5e6)
! cp
! rvord .. rv/rd  (1.6) 

! number of iterations
  niter=50
! minimum difference (usually converges in < 8 iterations with diff = 2e-5)
  diff=2.e-5

  EXN=(P/p1000mb)**rcp
  !QC=0.  !better first guess QC is incoming from lower level, do not set to zero
  do i=1,NITER
     T=EXN*THL + xlvcp*QC        
     QS=qsat_blend(T,P)
     QCOLD=QC
     QC=0.5*QC + 0.5*MAX((QT-QS),0.)
     if (abs(QC-QCOLD)<Diff) exit
  enddo

  T=EXN*THL + xlvcp*QC
  QS=qsat_blend(T,P)
  QC=max(QT-QS,0.)

  !Do not allow saturation below 100 m
  if(zagl < 100.)QC=0.

  !THV=(THL+xlv/cp*QC).*(1+(1-rvovrd)*(QT-QC)-QC);
  THV=(THL+xlvcp*QC)*(1.+QT*(rvovrd-1.)-rvovrd*QC)

!  IF (QC > 0.0) THEN
!    PRINT*,"EDMF SAT, p:",p," iterations:",i
!    PRINT*," T=",T," THL=",THL," THV=",THV
!    PRINT*," QS=",QS," QT=",QT," QC=",QC,"ratio=",qc/qs
!  ENDIF

  !THIS BASICALLY GIVE THE SAME RESULT AS THE PREVIOUS LINE
  !TH = THL + xlv/cp/EXN*QC
  !THV= TH*(1. + 0.608*QT)

  !print *,'t,p,qt,qs,qc'
  !print *,t,p,qt,qs,qc 


end subroutine condensation_edmf

!===============================================================

SUBROUTINE SCALE_AWARE(dx,PBL1,Psig_bl,Psig_shcu)

    !---------------------------------------------------------------
    !             NOTES ON SCALE-AWARE FORMULATION
    !
    !JOE: add scale-aware factor (Psig) here, taken from Honnert et al. (2011,
    !     JAS) and/or from Hyeyum Hailey Shin and Song-You Hong (2013, JAS)
    !
    ! Psig_bl tapers local mixing
    ! Psig_shcu tapers nonlocal mixing

    REAL,INTENT(IN) :: dx,PBL1
    REAL, INTENT(OUT) :: Psig_bl,Psig_shcu
    REAL :: dxdh

    Psig_bl=1.0
    Psig_shcu=1.0
    dxdh=MAX(2.5*dx,10.)/MIN(PBL1,3000.)
    ! Honnert et al. 2011, TKE in PBL  *** original form used until 201605
    !Psig_bl= ((dxdh**2) + 0.07*(dxdh**0.667))/((dxdh**2) + &
    !         (3./21.)*(dxdh**0.67) + (3./42.))
    ! Honnert et al. 2011, TKE in entrainment layer
    !Psig_bl= ((dxdh**2) + (4./21.)*(dxdh**0.667))/((dxdh**2) + &
     !        (3./20.)*(dxdh**0.67) + (7./21.))
    ! New form to preseve parameterized mixing - only down 5% at dx = 750 m
     Psig_bl= ((dxdh**2) + 0.106*(dxdh**0.667))/((dxdh**2) +0.066*(dxdh**0.667) + 0.071)

    !assume a 500 m cloud depth for shallow-cu clods
    dxdh=MAX(2.5*dx,10.)/MIN(PBL1+500.,3500.)
    ! Honnert et al. 2011, TKE in entrainment layer *** original form used until 201605
    !Psig_shcu= ((dxdh**2) + (4./21.)*(dxdh**0.667))/((dxdh**2) + &
    !         (3./20.)*(dxdh**0.67) + (7./21.))

    ! Honnert et al. 2011, TKE in cumulus
    !Psig(i)= ((dxdh**2) + 1.67*(dxdh**1.4))/((dxdh**2) +1.66*(dxdh**1.4) +
    !0.2)

    ! Honnert et al. 2011, w'q' in PBL
    !Psig(i)= 0.5 + 0.5*((dxdh**2) + 0.03*(dxdh**1.4) -
    !(4./13.))/((dxdh**2) + 0.03*(dxdh**1.4) + (4./13.))
    ! Honnert et al. 2011, w'q' in cumulus
    !Psig(i)= ((dxdh**2) - 0.07*(dxdh**1.4))/((dxdh**2) -0.07*(dxdh**1.4) +
    !0.02)

    ! Honnert et al. 2011, q'q' in PBL
    !Psig(i)= 0.5 + 0.5*((dxdh**2) + 0.25*(dxdh**0.667) -0.73)/((dxdh**2)
    !-0.03*(dxdh**0.667) + 0.73)
    ! Honnert et al. 2011, q'q' in cumulus
    !Psig(i)= ((dxdh**2) - 0.34*(dxdh**1.4))/((dxdh**2) - 0.35*(dxdh**1.4)
    !+ 0.37)

    ! Hyeyum Hailey Shin and Song-You Hong 2013, TKE in PBL (same as Honnert's above)
    !Psig_shcu= ((dxdh**2) + 0.070*(dxdh**0.667))/((dxdh**2)
    !+0.142*(dxdh**0.667) + 0.071)
    ! Hyeyum Hailey Shin and Song-You Hong 2013, TKE in entrainment zone  *** switch to this form 201605
    Psig_shcu= ((dxdh**2) + 0.145*(dxdh**0.667))/((dxdh**2) +0.172*(dxdh**0.667) + 0.170)

    ! Hyeyum Hailey Shin and Song-You Hong 2013, w'theta' in PBL
    !Psig(i)= 0.5 + 0.5*((dxdh**2) -0.098)/((dxdh**2) + 0.106) 
    ! Hyeyum Hailey Shin and Song-You Hong 2013, w'theta' in entrainment zone
    !Psig(i)= 0.5 + 0.5*((dxdh**2) - 0.112*(dxdh**0.25) -0.071)/((dxdh**2)
    !+ 0.054*(dxdh**0.25) + 0.10)

    !print*,"in scale_aware; dx, dxdh, Psig(i)=",dx,dxdh,Psig(i)
    !If(Psig_bl(i) < 0.0 .OR. Psig(i) > 1.)print*,"dx, dxdh, Psig(i)=",dx,dxdh,Psig_bl(i) 
    If(Psig_bl > 1.0) Psig_bl=1.0
    If(Psig_bl < 0.0) Psig_bl=0.0

    If(Psig_shcu > 1.0) Psig_shcu=1.0
    If(Psig_shcu < 0.0) Psig_shcu=0.0

  END SUBROUTINE SCALE_AWARE

! =====================================================================

  FUNCTION esat_blend(t) 
! JAYMES- added 22 Apr 2015
! 
! This calculates saturation vapor pressure.  Separate ice and liquid functions 
! are used (identical to those in module_mp_thompson.F, v3.6).  Then, the 
! final returned value is a temperature-dependant "blend".  Because the final 
! value is "phase-aware", this formulation may be preferred for use throughout 
! the module (replacing "svp").

      IMPLICIT NONE
      
      REAL, INTENT(IN):: t
      REAL :: esat_blend,XC,ESL,ESI,chi

      XC=MAX(-80.,t-273.16)

! For 253 < t < 273.16 K, the vapor pressures are "blended" as a function of temperature, 
! using the approach of Chaboureau and Bechtold (2002), JAS, p. 2363.  The resulting 
! values are returned from the function.
      IF (t .GE. 273.16) THEN
          esat_blend = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8))))))) 
      ELSE IF (t .LE. 253.) THEN
          esat_blend = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
      ELSE
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          chi  = (273.16-t)/20.16
          esat_blend = (1.-chi)*ESL  + chi*ESI
      END IF

  END FUNCTION esat_blend

! ====================================================================

  FUNCTION qsat_blend(t, P, waterice)
! JAYMES- this function extends function "esat" and returns a "blended"
! saturation mixing ratio.

      IMPLICIT NONE

      REAL, INTENT(IN):: t, P
      CHARACTER(LEN=1), OPTIONAL, INTENT(IN) :: waterice
      CHARACTER(LEN=1) :: wrt
      REAL :: qsat_blend,XC,ESL,ESI,RSLF,RSIF,chi

      IF ( .NOT. PRESENT(waterice) ) THEN 
          wrt = 'b'
      ELSE
          wrt = waterice
      ENDIF

      XC=MAX(-80.,t-273.16)

      IF ((t .GE. 273.16) .OR. (wrt .EQ. 'w')) THEN
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8))))))) 
          qsat_blend = 0.622*ESL/(P-ESL) 
      ELSE IF (t .LE. 253.) THEN
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          qsat_blend = 0.622*ESI/(P-ESI)
      ELSE
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          RSLF = 0.622*ESL/(P-ESL)
          RSIF = 0.622*ESI/(P-ESI)
          chi  = (273.16-t)/20.16
          qsat_blend = (1.-chi)*RSLF + chi*RSIF
      END IF

  END FUNCTION qsat_blend

! ===================================================================

  FUNCTION xl_blend(t)
! JAYMES- this function interpolates the latent heats of vaporization and
! sublimation into a single, temperature-dependant, "blended" value, following
! Chaboureau and Bechtold (2002), Appendix.

      IMPLICIT NONE

      REAL, INTENT(IN):: t
      REAL :: xl_blend,xlvt,xlst,chi

      IF (t .GE. 273.16) THEN
          xl_blend = xlv + (cpv-cliq)*(t-273.16)  !vaporization/condensation
      ELSE IF (t .LE. 253.) THEN
          xl_blend = xls + (cpv-cice)*(t-273.16)  !sublimation/deposition
      ELSE
          xlvt = xlv + (cpv-cliq)*(t-273.16)  !vaporization/condensation
          xlst = xls + (cpv-cice)*(t-273.16)  !sublimation/deposition
          chi  = (273.16-t)/20.16
          xl_blend = (1.-chi)*xlvt + chi*xlst     !blended
      END IF

  END FUNCTION xl_blend

! ===================================================================
! ===================================================================
! ===================================================================

END MODULE module_bl_mynn
