#define MYNN_DBG_LVL 3000
!WRF:MODEL_LAYER:PHYSICS
!
! translated from NN f77 to F90 and put into WRF by Mariusz Pagowski
! NOAA/GSD & CIRA/CSU, Feb 2008
! changes to original code:
! 1. code is 1d (in z)
! 2. no advection of TKE, covariances and variances 
! 3. Cranck-Nicholson replaced with the implicit scheme
! 4. removed terrain dependent grid since input in WRF in actual
!    distances in z[m]
! 5. cosmetic changes to adhere to WRF standard (remove common blocks, 
!            intent etc)
!-------------------------------------------------------------------
!Modifications implemented by Joseph Olson NOAA/GSD/AMB - CU/CIRES
! (approved by Mikio Nakanishi or under consideration or do not
! significantly alter the general behavior of the MYNN as documented.):
!
! 1. Addition of BouLac mixing length in the free atmosphere.
! 2. Changed the turbulent mixing length to be integrated from the
!    surface to the top of the BL + a transition layer depth.
! 3. v3.4.1: Option to use Kitamura/Canuto modification which removes 
!            the critical Richardson number and negative TKE (default).
!            Hybrid PBL height diagnostic, which blends a theta-v-based
!            definition in neutral/convective BL and a TKE-based definition
!            in stable conditions.
!            TKE budget output option (bl_mynn_tkebudget)
! 4. v3.5.0: TKE advection option (bl_mynn_tkeadvect)
! 5. v3.5.1: Fog deposition related changes.
! 6. v3.6.0: Removed fog deposition from the calculation of tendencies
!            Added mixing of qc, qi, qni
!            Added output for wstar, delta, TKE_PBL, & KPBL for correct 
!                   coupling to shcu schemes  
! 7. v3.8.0: Added subgrid scale cloud output for coupling to radiation
!            schemes (activated by setting icloud_bl =1 in phys namelist).
!            Added WRF_DEBUG prints (at level 3000)
!            Added Tripoli and Cotton (1981) correction.
!            Added namelist option bl_mynn_cloudmix to test effect of mixing
!                cloud species (default = 1: on). 
!            Added mass-flux option (bl_mynn_edmf, = 1 for StEM, 2 for TEMF).
!                This option is off by default (=0).
!                Related (hidden) options: 
!                 bl_mynn_edmf_mom = 1 : activate momentum transport in MF scheme
!                 bl_mynn_edmf_tke = 1 : activate TKE transport in MF scheme
!                 bl_mynn_edmf_part= 1 : activate areal partitioning of ED & MF
!            Added mixing length option (bl_mynn_mixlength, see notes below)
!            Added more sophisticated saturation checks, following Thompson scheme
!            Added new cloud PDF option (bl_mynn_cloudpdf = 2) from Chaboureau
!                and Bechtold (2002, JAS, with mods) 
!            Added capability to mix chemical species when env variable
!                WRF_CHEM = 1, thanks to Wayne Angevine.
!            Added scale-aware mixing length, following Junshi Ito's work
!                Ito et al. (2015, BLM).
!
! For changes 1, 3, and 6, see "JOE's mods" below:
!-------------------------------------------------------------------

MODULE module_bl_mynn

  USE module_model_constants, only: &
       &karman, g, p1000mb, &
       &cp, r_d, r_v, rcp, xlv, xlf, xls, &
       &svp1, svp2, svp3, svpt0, ep_1, ep_2, rvovrd, &
       &cpv, cliq, cice

  USE module_state_description, only: param_first_scalar, &
       &p_qc, p_qr, p_qi, p_qs, p_qg, p_qnc, p_qni

  USE module_wrf_error
!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------

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

! 'parameters' for Poisson distribution (StEM EDMF scheme)
  REAL, PARAMETER  :: zero = 0.0, half = 0.5, one = 1.0, two = 2.0

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
! *     thl     : Liquid water potential temperature               *
! *     qw     : Total water (water vapor+liquid water) content        *
! *     ql     : Liquid water content                                  *
! *     vt, vq : Functions for computing the buoyancy flux             *
! *                                                                    *
! *     If the water contents are unnecessary, e.g., in the case of    *
! *     ocean models, thl is the potential temperature and qw, ql, vt   *
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
! *     # Upper boundary values are given at k=nz.                   *
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
  SUBROUTINE  mym_initialize ( &
       &            kts,kte,&
       &            dz, zw,  &
       &            u, v, thl, qw, &
!       &            ust, rmo, pmz, phh, flt, flq,&
       &            zi, theta, sh,&
       &            ust, rmo, el,&
       &            Qke, Tsq, Qsq, Cov, Psig_bl, cldfra_bl1D, &
       &            bl_mynn_mixlength, &
       &            edmf_a1,edmf_qc1,bl_mynn_edmf)
!
!-------------------------------------------------------------------
    
    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: bl_mynn_mixlength,bl_mynn_edmf
!    REAL, INTENT(IN)   :: ust, rmo, pmz, phh, flt, flq
    REAL, INTENT(IN)   :: ust, rmo, Psig_bl
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,cldfra_bl1D,&
                                            edmf_a1,edmf_qc1
    REAL, DIMENSION(kts:kte), INTENT(out) :: tsq,qsq,cov
    REAL, DIMENSION(kts:kte), INTENT(inout) :: el,qke

    REAL, DIMENSION(kts:kte) :: &
         &ql,pdk,pdt,pdq,pdc,dtl,dqw,dtv,&
         &gm,gh,sm,sh,qkw,vt,vq
    INTEGER :: k,l,lmax
    REAL :: phm,vkz,elq,elv,b1l,b2l,pmz=1.,phh=1.,flt=0.,flq=0.,tmpq
    REAL :: zi
    REAL, DIMENSION(kts:kte) :: theta

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
    qke(kts) = ust**2 * ( b1*pmz )**(2.0/3.0)
!
    phm      = phh*b2 / ( b1*pmz )**(1.0/3.0)
    tsq(kts) = phm*( flt/ust )**2
    qsq(kts) = phm*( flq/ust )**2
    cov(kts) = phm*( flt/ust )*( flq/ust )
!
    DO k = kts+1,kte
       vkz = vk*zw(k)
       el (k) = vkz/( 1.0 + vkz/100.0 )
       qke(k) = 0.0
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
       CALL mym_length ( & 
            &            kts,kte,&
            &            dz, zw, &
            &            rmo, flt, flq, &
            &            vt, vq, &
            &            qke, &
            &            dtv, &
            &            el, &
            &            zi,theta,&
            &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength,&
            &            edmf_a1,edmf_qc1,bl_mynn_edmf)
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
!
       elv = 0.5*( el(kts+1)+el(kts) ) /  vkz 
       qke(kts) = ust**2 * ( b1*pmz*elv    )**(2.0/3.0)
!
       phm      = phh*b2 / ( b1*pmz/elv**2 )**(1.0/3.0)
       tsq(kts) = phm*( flt/ust )**2
       qsq(kts) = phm*( flq/ust )**2
       cov(kts) = phm*( flt/ust )*( flq/ust )
!
       DO k = kts+1,kte-1
          b1l = b1*0.25*( el(k+1)+el(k) )
          tmpq=MAX(b1l*( pdk(k+1)+pdk(k) ),qkemin)
!          PRINT *,'tmpqqqqq',tmpq,pdk(k+1),pdk(k)
          qke(k) = tmpq**(2.0/3.0)

!
          IF ( qke(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*( b1l/b1 ) / SQRT( qke(k) )
          END IF
!
          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO

!
    END DO

!!    qke(kts)=qke(kts+1)
!!    tsq(kts)=tsq(kts+1)
!!    qsq(kts)=qsq(kts+1)
!!    cov(kts)=cov(kts+1)

    qke(kte)=qke(kte-1)
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
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,ql,vt,vq

    REAL, DIMENSION(kts:kte), INTENT(out) :: &
         &dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: rfc,f1,f2,rf1,rf2,smc,shc,&
         &ri1,ri2,ri3,ri4,duz,dtz,dqz,vtt,vqq,dtq,dzk,afk,abk,ri,rf

!JOE-Canuto/Kitamura mod
    REAL ::   a2den
!JOE-end

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

!JOE-Canuto/Kitamura mod
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
!JOE-end

!   **  Flux Richardson number  **
       rf = MIN( ri1*( ri+ri2-SQRT(ri**2-ri3*ri+ri4) ), rfc )
!
       sh (k) = shc*( rfc-rf )/( 1.0-rf )
       sm (k) = smc*( rf1-rf )/( rf2-rf ) * sh(k)
    END DO
!
    RETURN

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
  SUBROUTINE  mym_length ( & 
    &            kts,kte,&
    &            dz, zw, &
    &            rmo, flt, flq, &
    &            vt, vq, &
    &            qke, &
    &            dtv, &
    &            el, &
    &            zi,theta,&       !JOE-BouLac mod
    &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength,&
    &            edmf_a1,edmf_qc1,bl_mynn_edmf)
    
!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: bl_mynn_mixlength,bl_mynn_edmf
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl
    REAL, DIMENSION(kts:kte), INTENT(IN)   :: qke,vt,vq,cldfra_bl1D,&
                                              edmf_a1,edmf_qc1
    REAL, DIMENSION(kts:kte), INTENT(out)  :: qkw, el
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dtv

    REAL :: elt,vsc

    REAL, DIMENSION(kts:kte), INTENT(IN) :: theta
    REAL, DIMENSION(kts:kte) :: qtke,elBLmin,elBLavg,thetaw
    REAL :: wt,wt2,zi,zi2,h1,h2,hs,elBLmin0,elBLavg0

    ! THE FOLLOWING CONSTANTS ARE IMPORTANT FOR REGULATING THE
    ! MIXING LENGTHS:
    REAL :: cns,   &   ! for surface layer (els) in stable conditions
            alp1,  &   ! for turbulent length scale (elt)
            alp2,  &   ! for buoyancy length scale (elb)
            alp3,  &   ! for buoyancy enhancement factor of elb
            alp4,  &   ! for surface layer (els) in unstable conditions
            alp5       ! for BouLac mixing length

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
    REAL :: afk,abk,zwk,zwk1,dzk,qdz,vflx,bv,tau_cloud,elb,elb_cloud,els,els1,elf, &
            & el_stab,el_unstab,el_mf,el_stab_mf,elb_mf


    IF ( bl_mynn_mixlength .EQ. 0 ) THEN
       cns  = 2.7
       alp1 = 0.23
       alp2 = 1.0
       alp3 = 5.0
       alp4 = 100.
       alp5 = 0.4
    ELSEIF ( bl_mynn_mixlength .EQ. 1 ) THEN
       cns  = 2.3
       alp1 = 0.23
       alp2 = 0.6
       alp3 = 3.0
       alp4 = 20.
       alp5 = 0.4
    ELSEIF ( bl_mynn_mixlength .GE. 2 ) THEN
       cns  = 2.7
       alp1 = 0.23
       alp2 = 0.3
       alp3 = 3.0
       alp4 = 10.
       alp5 = 0.4
    ENDIF

!    tv0 = 0.61*tref
!    gtr = 9.81/tref
!
!JOE-added to impose limits on the height integration for elt as well 
!    as the transition layer depth
    IF ( bl_mynn_mixlength .EQ. 0 ) THEN
       zi2=10000.  !originally integrated to model top, not just 10 km.
    ELSE
       zi2=MAX(zi,minzi)
    ENDIF
    h1=MAX(0.3*zi2,mindz)
    h1=MIN(h1,maxdz)         ! 1/2 transition layer depth
    h2=h1/2.0                ! 1/4 transition layer depth

    qtke(kts)=MAX(qke(kts)/2.,0.01) !tke at full sigma levels
    thetaw(kts)=theta(kts)          !theta at full-sigma levels
!JOE-end
    qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))

    DO k = kts+1,kte
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
       qtke(k) = (qkw(k)**2.)/2.    ! q -> TKE
       thetaw(k)= theta(k)*abk + theta(k-1)*afk
    END DO
!
    elt = 1.0e-5
    vsc = 1.0e-5
!
!   **  Strictly, zwk*h(i,j) -> ( zwk*h(i,j)+z0 )  **
!JOE-Lt mod: only integrate to top of PBL (+ transition/entrainment
!   layer), since TKE aloft is not relevant. Make WHILE loop, so it
!   exits after looping through the boundary layer.
!
     k = kts+1
     zwk = zw(k)
     DO WHILE (zwk .LE. (zi2+h1))
       dzk = 0.5*( dz(k)+dz(k-1) )
       qdz = MAX( qkw(k)-qmin, 0.03 )*dzk
             elt = elt +qdz*zwk
             vsc = vsc +qdz
       k   = k+1
       zwk = zw(k)
    END DO
!
    elt =  alp1*elt/vsc
    vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
    vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**(1.0/3.0)
!
!   **  Strictly, el(i,j,1) is not zero.  **
    el(kts) = 0.0
!
    IF ( bl_mynn_mixlength .EQ. 1 ) THEN
       ! COMPUTE BouLac mixing length
       CALL boulac_length(kts,kte,zw,dz,qtke,thetaw,elBLmin,elBLavg)
    ENDIF

    DO k = kts+1,kte
       zwk = zw(k)              !full-sigma levels
       IF (k .EQ. kts+1) zwk1=zwk 

       !   **  Length scale limited by the buoyancy effect  **

       IF ( dtv(k) .GT. 0.0 ) THEN
         bv  = SQRT( gtr*dtv(k) )

         IF ( bl_mynn_mixlength .EQ. 0 ) THEN ! use default elb formulation
           elb = alp2*qkw(k) / bv &     
                &       *( 1.0 + alp3/alp2*&
                &SQRT( vsc/( bv*elt ) ) )
           elf = alp2 * qkw(k)/bv

         ELSE IF ( bl_mynn_mixlength .EQ. 1 ) THEN ! use default elb
           elb = alp2*qkw(k) / bv &                ! formulation,
                &       *( 1.0 + alp3/alp2*&       ! except keep
                &SQRT( vsc/( bv*elt ) ) )          ! elb bounded by
           elb = MIN(elb, zwk)                     ! zwk
           elf = alp2 * qkw(k)/bv                  

         ELSE IF ( bl_mynn_mixlength .GE. 2 ) THEN ! use new elb formulation
           elb_mf = alp2*qkw(k) / bv &
                &       *( 1.0 + alp3/alp2*&
                &SQRT( vsc/( bv*elt ) ) )
           tau_cloud = MIN(MAX(0.5*zi/((gtr*zi*MAX(flt,1.0e-4))**(1.0/3.0)), 100.),300.)
           !minimize influence of surface heat flux on tau far away from the PBLH.
           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
           tau_cloud = tau_cloud*(1.-wt) + 100.*wt        
           elb_cloud = MIN(tau_cloud*SQRT(MIN(qtke(k),16.)), zwk)
           elb = MIN(alp2*qkw(k)/bv, zwk)
           elf = elb
           IF (zwk > zi .AND. elf > 500.) THEN
             ! COMPUTE BouLac mixing length
             CALL boulac_length0(k,kts,kte,zw,dz,qtke,thetaw,elBLmin0,elBLavg0)
             elf = alp5*elBLavg0
           ENDIF
           elb = elb*(1.-cldfra_bl1D(k)) + elb_cloud*cldfra_bl1D(k)
           elf = elf*(1.-cldfra_bl1D(k)) + elb_cloud*cldfra_bl1D(k)

         END IF 

       ELSE

         IF ( bl_mynn_mixlength .LE. 1 ) THEN ! use default elb formulation
           elb = 1.0e10
           elf = elb

         ELSE IF ( bl_mynn_mixlength .GE. 2 ) THEN
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

           tau_cloud = MIN(MAX(0.5*zi/((gtr*zi*MAX(flt,1.0e-4))**(1.0/3.0)),200.),300.)
           !minimize influence of surface heat flux on tau far away from the PBLH.
           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
           tau_cloud = tau_cloud*(1.-wt) + 100.*wt
           elb_cloud = MIN(tau_cloud*SQRT(MIN(qtke(k),20.)), zwk)
           elb = elb_cloud
           elf = elb_cloud
           elb_mf = elb
           IF (zwk > zi .AND. elf > 500.) THEN
             ! COMPUTE BouLac mixing length for dry conditions in free atmosphere
             CALL boulac_length0(k,kts,kte,zw,dz,qtke,thetaw,elBLmin0,elBLavg0)
             elf = alp5*elBLavg0*(1.-cldfra_bl1D(k)) + elb_cloud*cldfra_bl1D(k)
           END IF
           elf = elf*(1.-cldfra_bl1D(k)) + elb_cloud*cldfra_bl1D(k)

         END IF
       END IF

       z_m = MAX(0.,zwk - 4.)

!   **  Length scale in the surface layer  **
       IF ( rmo .GT. 0.0 ) THEN
         IF ( bl_mynn_mixlength .LE. 1 ) THEN
           els  = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
           els1 = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax ))
         ELSE
           els  = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
           els1 = vk*z_m/(1.0+cns*MIN( zwk1*rmo, zmax ))
         ENDIF
       ELSE
          els  =  vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
          els1 =  vk*z_m*( 1.0 - alp4* zwk*rmo )**0.2
       END IF

!
!   ** HARMONC AVERGING OF MIXING LENGTH SCALES: 
!       el(k) =      MIN(elb/( elb/elt+elb/els+1.0 ),elf)
!       el(k) =      elb/( elb/elt+elb/els+1.0 )

       wt=.5*TANH((zwk - (zi2+h1))/h2) + .5

       IF ( bl_mynn_mixlength .EQ. 0 ) THEN
          el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)

       ELSE IF ( bl_mynn_mixlength .EQ. 1 ) THEN 
          !add blending to use BouLac mixing length in free atmos;
          !defined relative to the PBLH (zi) + transition layer (h1)
          el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
          el(k) = el(k)*(1.-wt) + alp5*elBLmin(k)*wt

       !JAYMES- el_stab & el_unstab blending begin ( bl_mynn_mixlength opt)
       ELSE IF ( bl_mynn_mixlength .GE. 2 ) THEN
          hs = MAX(MIN(0.2*zi,200.),50.) ! bounded:  50 m < hs < 200 m
          !z_m = MAX(0.,zwk - zwk1)
          wt2 = 1.-(MIN(z_m,hs)/hs)

          el_stab = els*wt2 + elb*(1.-wt2)
          el_stab_mf = els*wt2 + elb_mf*(1.-wt2)

          el_unstab = els/(1. + (els1/elt))

          IF (bl_mynn_edmf > 0 .AND. edmf_a1(kts)>0.0) THEN
             !Force unstable mixing length to be used in the lower PBL and
             !blended unstable-mf length scale in the upper PBL when the mass-flux
             !scheme is active.
              !wt2=.5*TANH((zwk - (0.5*zi))/(0.25*zi)) + .5
              !el(k) = el_unstab*wt2 + MIN(el_stab_mf,el_unstab)*(1.-wt2)
              el(k) = MIN(el_stab_mf,el_unstab)
          ELSE
              el(k) = MIN(el_stab,el_unstab)
          ENDIF
          el(k) = el(k)*(1.-wt) + elf*wt
       END IF

       IF ( bl_mynn_mixlength .GE. 1 ) THEN ! include scale-awareness, except for original MYNN
         el(k) = el(k)*Psig_bl
       END IF

       IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
         IF (el(k) > 1200.) THEN
           WRITE ( mynn_message , FMT='(A,F7.0,I5,5F7.0)' ) &
           ' MYNN; mym_length; LARGE el,k,elb_cloud,elb,elt,elf,tau:'&
                                , el(k),k,elb_cloud,elb,elt,elf,tau_cloud
           CALL wrf_debug ( 0 , mynn_message )
         ENDIF
       ENDIF

    END DO
!
    RETURN

  END SUBROUTINE mym_length

!JOE- BouLac Code Start -

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
!JOE-END BOULAC CODE

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
  SUBROUTINE  mym_turbulence (& 
    &            kts,kte,&
    &            levflag, &
    &            dz, zw, &
    &            u, v, thl, ql, qw, &
    &            qke, tsq, qsq, cov, &
    &            vt, vq,&
    &            rmo, flt, flq, &
    &            zi,theta,&
    &            sh,&
    &            El,&
    &            Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc &
    &		 ,qWT1D,qSHEAR1D,qBUOY1D,qDISS1D &
    &            ,bl_mynn_tkebudget &
    &            ,Psig_bl,Psig_shcu,cldfra_bl1D,bl_mynn_mixlength,&
    &            edmf_a1,edmf_qc1,bl_mynn_edmf)

!-------------------------------------------------------------------
!
    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: levflag,bl_mynn_mixlength,bl_mynn_edmf
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl,Psig_shcu
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,& 
         &ql,vt,vq,qke,tsq,qsq,cov,cldfra_bl1D,edmf_a1,edmf_qc1

    REAL, DIMENSION(kts:kte), INTENT(out) :: dfm,dfh,dfq,&
         &pdk,pdt,pdq,pdc,tcd,qcd,el

!JOE-TKE BUDGET
    REAL, DIMENSION(kts:kte), INTENT(inout) :: &
         qWT1D,qSHEAR1D,qBUOY1D,qDISS1D
    REAL :: q3sq_old,dlsq1,qWTP_old,qWTP_new
    REAL :: dudz,dvdz,dTdz,&
            upwp,vpwp,Tpwp
    INTEGER, INTENT(in) :: bl_mynn_tkebudget
!JOE-end

    REAL, DIMENSION(kts:kte) :: qkw,dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k
!    REAL :: cc2,cc3,e1c,e2c,e3c,e4c,e5c
    REAL :: e6c,dzk,afk,abk,vtt,vqq,&
         &cw25,clow,cupp,gamt,gamq,smd,gamv,elq,elh

    REAL :: zi
    REAL, DIMENSION(kts:kte), INTENT(in) :: theta

    REAL ::  a2den, duz, ri, HLmod  !JOE-Canuto/Kitamura mod
!JOE-stability criteria for cw
    REAL:: auh,aum,adh,adm,aeh,aem,Req,Rsl,Rsl2
!JOE-end

    DOUBLE PRECISION  q2sq, t2sq, r2sq, c2sq, elsq, gmel, ghel
    DOUBLE PRECISION  q3sq, t3sq, r3sq, c3sq, dlsq, qdiv
    DOUBLE PRECISION  e1, e2, e3, e4, enum, eden, wden


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
    CALL mym_length ( & 
    &            kts,kte, &
    &            dz, zw, &
    &            rmo, flt, flq, &
    &            vt, vq, &
    &            qke, &
    &            dtv, &
    &            el, &
    &            zi,theta,&
    &            qkw,Psig_bl,cldfra_bl1D,bl_mynn_mixlength, &
    &            edmf_a1,edmf_qc1,bl_mynn_edmf)
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
       IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
           WRITE ( mynn_message , FMT='(A,F8.1,A,I6)' ) &
           " MYNN; mym_turbulence2.0; sh=",sh(k)," k=",k
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " qke=",qke(k)," el=",el(k)," ri=",ri
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " PBLH=",zi," u=",u(k)," v=",v(k)
           CALL wrf_debug ( 0 , mynn_message )
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

       !JOE-TEST: try forcing some small ED mixing within the MF plume
       !IF (bl_mynn_edmf > 0 .AND. edmf_a1(k)>0.01) THEN
       !   sh(k) = MAX(sh(k),0.05)
       !   sm(k) = MAX(sm(k),0.05)
       !ENDIF

       !JOE: Level 2.5 debug prints
       ! HL88 , lev2.5 criteria from eqs. 3.17, 3.19, & 3.20
       IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0 .OR. &
           sh(k) > 0.76*b2 .or. (sm(k)**2*gm(k) .gt. .44**2)) THEN
           WRITE ( mynn_message , FMT='(A,F8.1,A,I6)' ) &
           " MYNN; mym_turbulence2.5; sh=",sh(k)," k=",k
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " qke=",qke(k)," el=",el(k)," ri=",ri
           CALL wrf_debug ( 0 , mynn_message )
           WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
           " PBLH=",zi," u=",u(k)," v=",v(k)
           CALL wrf_debug ( 0 , mynn_message )
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
          !   wrf_err_message = '--- ERROR: MYNN: Dynamic Cw '// &
          !        'limit exceeds reasonable limits'
          !   CALL wrf_message ( wrf_err_message )
          !   WRITE ( mynn_message , FMT='(A,F8.3)' ) &
          !   " MYNN: Dynamic Cw limit needs attention=",Rsl
          !   CALL wrf_debug ( 0 , mynn_message )
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
          IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
            IF (sh(k)<-0.3 .OR. sm(k)<-0.3 .OR. &
              qke(k) < -0.1 .or. ABS(smd) .gt. 2.0) THEN
              WRITE ( mynn_message , FMT='(A,F8.1,A,I6)' ) &
              " MYNN; mym_turbulence3.0; sh=",sh(k)," k=",k
              CALL wrf_debug ( 0 , mynn_message )
              WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
              " gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
              CALL wrf_debug ( 0 , mynn_message )
              WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
              " q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
              CALL wrf_debug ( 0 , mynn_message )
              WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
              " qke=",qke(k)," el=",el(k)," ri=",ri
              CALL wrf_debug ( 0 , mynn_message )
              WRITE ( mynn_message , FMT='(A,F6.1,A,F6.1,A,F6.1)' ) &
              " PBLH=",zi," u=",u(k)," v=",v(k)
              CALL wrf_debug ( 0 , mynn_message )
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
       elq = el(k)*qkw(k)
       elh = elq*qdiv

       ! Production of TKE (pdk), T-variance (pdt),
       ! q-variance (pdq), and covariance (pdc)
       pdk(k) = elq*( sm(k)*gm(k) &
            &                    +sh(k)*gh(k)+gamv ) ! JAYMES TKE
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

    RETURN

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
    INTEGER, INTENT(IN) :: levflag
    INTEGER, INTENT(IN) :: bl_mynn_edmf_tke
    REAL, INTENT(IN)    :: delt
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz, dfq,el
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: pdk, pdt, pdq, pdc
    REAL, INTENT(IN)    ::  flt, flq, ust, pmz, phh
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: qke,tsq, qsq, cov
! WA 8/3/15
    REAL, DIMENSION(kts:kte+1), INTENT(INOUT) :: s_awqke,s_aw

    INTEGER :: k,nz
    REAL, DIMENSION(kts:kte) :: qkw, bp, rp, df3q
    REAL :: vkz,pdk1,phm,pdt1,pdq1,pdc1,b1l,b2l,onoff
    REAL, DIMENSION(kts:kte) :: dtz
    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    nz=kte-kts+1

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

    a(nz)=-1. !0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=0.

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte   
       qke(k)=d(k-kts+1)
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

       a(nz)=-1. !0.
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          tsq(k)=d(k-kts+1)
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

       a(nz)=-1. !0.
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          qsq(k)=d(k-kts+1)
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

       a(nz)=-1. !0.
       b(nz)=1.
       c(nz)=0.
       d(nz)=0.
       
       CALL tridiag(nz,a,b,c,d)
       
       DO k=kts,kte
          cov(k)=d(k-kts+1)
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
    &            dx, dz,                  &
    &            thl, qw,                 &
    &            p,exner,                 &
    &            tsq, qsq, cov,           &
    &            Sh, el, bl_mynn_cloudpdf,& !JOE - cloud PDF testing
    &            qc_bl1D, cldfra_bl1D,    & !JOE - subgrid BL clouds
    &            PBLH1,HFX1,              & !JOE - for subgrid BL clouds
    &            edmf_qc1,                &
    &            Vt, Vq, th)

!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte, bl_mynn_cloudpdf
    REAL, INTENT(IN)      :: dx,PBLH1,HFX1
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
    REAL, DIMENSION(kts:kte), INTENT(IN) :: p,exner, thl, qw, &
         &tsq, qsq, cov, edmf_qc1, th

    REAL, DIMENSION(kts:kte), INTENT(OUT) :: vt,vq

    REAL, DIMENSION(kts:kte) :: qmq,alp,a,bet,b,sgm,ql,q1,cld,RH
    REAL, DIMENSION(kts:kte), INTENT(OUT) :: qc_bl1D,cldfra_bl1D
    DOUBLE PRECISION :: t3sq, r3sq, c3sq

! WA TEST 8/6/15 save incoming qc and cldfra (from EDMF?)
 !   REAL, DIMENSION(kts:kte) :: qc_prev,cldfra_prev
!

    REAL :: p2a,qsl,esat,qsat,tlk,qsat_tl,dqsl,cld0,q1k,eq1,qll,&
         &q2p,pt,rac,qt,t,xl,rsl,cpm,cdhdz,fng,qww,alpha,beta,bb,ls,wt
    INTEGER :: i,j,k

    REAL :: erf

    !JOE: NEW VARIABLES FOR ALTERNATE SIGMA
    REAL::dth,dtl,dqw,dzk
    REAL, DIMENSION(kts:kte), INTENT(IN) :: Sh,el

    !JOE: variables for BL clouds
    REAL::zagl,cld9,damp,edown,RHcrit,RHmean,RHsum,RHnum,Hshcu,PBLH2
    REAL, PARAMETER :: Hfac = 3.0     !cloud depth factor for HFX (m^3/W)
    REAL, PARAMETER :: HFXmin = 50.0  !min W/m^2 for BL clouds
    REAL            :: RH_00L, RH_00O, phi_dz, lfac
    REAL, PARAMETER :: cdz = 2.0
    REAL, PARAMETER :: mdz = 1.5

! WA TEST 8/6/15 save incoming qc and cldfra (from EDMF?)
 !   qc_prev = qc_bl1D
 !   cldfra_prev = cldfra_bl1D

    zagl = 0.
! Note: kte needs to be larger than kts, i.e., kte >= kts+1.
    DO k = kts,kte-1
       p2a = exner(k)
       t  = th(k)*p2a

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
       qsl=ep_2*esat/(p(k)-ep_3*esat)
       !dqw/dT: Clausius-Clapeyron
       dqsl = qsl*ep_2*ev/( rd*t**2 )
       !RH (0 to 1.0)
       RH(k)=MAX(MIN(1.0,qw(k)/MAX(1.E-8,qsl)),0.001)

       alp(k) = 1.0/( 1.0+dqsl*xlvcp )
       bet(k) = dqsl*p2a

       IF (bl_mynn_cloudpdf == 0) THEN
          !Sommeria and Deardorff (1977) scheme, as implemented
          !in Nakanishi and Niino (2009), Appendix B
          t3sq = MAX( tsq(k), 0.0 )
          r3sq = MAX( qsq(k), 0.0 )
          c3sq =      cov(k)
          c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )
          r3sq = r3sq +bet(k)**2*t3sq -2.0*bet(k)*c3sq
          !DEFICIT/EXCESS WATER CONTENT
          qmq(k) = qw(k) -qsl
          !ORIGINAL STANDARD DEVIATION: limit e-6 produces ~10% more BL clouds
          !than e-10
          sgm(k) = SQRT( MAX( r3sq, 1.0d-10 ))
          !NORMALIZED DEPARTURE FROM SATURATION
          q1(k)   = qmq(k) / sgm(k)
          !CLOUD FRACTION. rr2 = 1/SQRT(2) = 0.707
          cld(k) = 0.5*( 1.0+erf( q1(k)*rr2 ) )

       ELSE IF (bl_mynn_cloudpdf == 1) THEN
          !ALTERNATIVE FORM (Nakanishi & Niino 2004 BLM, eq. B6, and 
          !Kuwano-Yoshida et al. 2010 QJRMS, eq. 7):
          if (k .eq. kts) then 
             dzk = 0.5*dz(k)
          else
             dzk = 0.5*( dz(k) + dz(k-1) )
          end if
          dth = 0.5*(thl(k+1)+thl(k)) - 0.5*(thl(k)+thl(MAX(k-1,kts)))
          dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))
          sgm(k) = SQRT( MAX( (alp(k)**2 * MAX(el(k)**2,0.1) * &
                             b2 * MAX(Sh(k),0.03))/4. * &
                      (dqw/dzk - bet(k)*(dth/dzk ))**2 , 1.0e-10) )
          qmq(k) = qw(k) -qsl
          q1(k)   = qmq(k) / sgm(k)
          cld(k) = 0.5*( 1.0+erf( q1(k)*rr2 ) )

       ELSE IF (bl_mynn_cloudpdf >= 2) THEN
          !Diagnostic statistical scheme of Chaboureau and Bechtold (2002), JAS
          !JAYMES- this added 27 Apr 2015
          xl = xl_blend(t)                    ! obtain latent heat

          tlk = thl(k)*(p(k)/p1000mb)**rcp    ! recover liquid temp (tl) from thl

          qsat_tl = qsat_blend(tlk,p(k))      ! get saturation water vapor mixing ratio
                                              !   at tl and p

          rsl = xl*qsat_tl / (r_v*tlk**2)     ! slope of C-C curve at t = tl
                                              ! CB02, Eqn. 4
 
          cpm = cp + qw(k)*cpv                ! CB02, sec. 2, para. 1
     
          a(k) = 1./(1. + xl*rsl/cpm)         ! CB02 variable "a"

          qmq(k) = a(k) * (qw(k) - qsat_tl) ! saturation deficit/excess;
                                              !   the numerator of Q1

          b(k) = a(k)*rsl                     ! CB02 variable "b"

          dtl =    0.5*(thl(k+1)*(p(k+1)/p1000mb)**rcp + tlk) &
               & - 0.5*(tlk + thl(MAX(k-1,kts))*(p(MAX(k-1,kts))/p1000mb)**rcp)

          dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))

          if (k .eq. kts) then
             dzk = 0.5*dz(k)
          else
             dzk = 0.5*( dz(k) + dz(k-1) )
          end if

          cdhdz = dtl/dzk + (g/cpm)*(1.+qw(k))  ! expression below Eq. 9
                                                ! in CB02

          zagl = zagl + dz(k)
          ls = MIN(MAX(zagl,25.),100.) !let this be the minimum possible ls
          !ls = MIN(el(k),300.)
          lfac=MIN(2.25+dx/4000.,4.)
          ls = MAX(MIN(lfac*el(k),500.),ls)
                  ! CB02 use 900 m as a (constant) free-atmosphere length scale. 
                  ! The form above was selected based on HRRR tests.

          sgm(k) = MAX(1.e-10, 0.2*ls*SQRT(MAX(0., & ! Eq. 9 in CB02:
                  & (a(k)*dqw/dzk)**2              & ! < 1st term in brackets,
                  & -2*a(k)*b(k)*cdhdz*dqw/dzk     & ! < 2nd term,
                  & +b(k)**2 * cdhdz**2)))           ! < 3rd term
                  ! Per CB02, 0.2 is chosen as a constant.

          q1(k) = qmq(k) / sgm(k)  ! Q1, the normalized saturation

          cld(k) = MAX(0., MIN(1., 0.5+0.36*ATAN(1.55*q1(k)))) ! Eq. 7 in CB02

       ENDIF
    END DO

    zagl = 0.
    RHsum=0.
    RHnum=0.
    RHmean=0.1 !initialize with small value for small PBLH cases
    damp =0
    PBLH2=MAX(10.,PBLH1)

    DO k = kts,kte-1

      IF (edmf_qc1(k) > 1.e-9) THEN
         !RETAIN CLDFRA & QC_BL FROM MASS-FLUX SCHEME, but compute vt & vq
         q1k = -0.7
         fng = -1.5*q1k

         xl    = xl_blend(t)
         q2p  = xlvcp/exner(k)
         pt = thl(k) +q2p*edmf_qc1(k) ! potential temp
         bb = b(k)*t/pt ! bb is "b" in BCMT95.  Their "b" differs from
                        ! "b" in CB02 (i.e., b(k) above) by a factor
                        ! of T/theta.  Strictly, b(k) above is formulated in
                        ! terms of sat. mixing ratio, but bb in BCMT95 is
                        ! cast in terms of sat. specific humidity.  The
                        ! conversion is neglected here.
         qww   = 1.+0.61*qw(k)
         alpha = 0.61*pt
         p2a   = exner(k)
         t     = thl(k)*p2a
         beta  = pt*xl/(t*cp) - 1.61*pt

         vt(k) = (1.-cldfra_bl1D(k))*qww   + cldfra_bl1D(k)*(qww   - beta*bb  *(1.+fng)) - 1.
         vq(k) = (1.-cldfra_bl1D(k))*alpha + cldfra_bl1D(k)*(alpha + beta*a(k)*(1.+fng)) - tv0
         ! These equations were derived by Jaymes, using BCMT95, Eq. B5,
         ! in order to recast the BC02/BCMT95 buoyancy flux in terms of
         ! vt and vq (i.e., beta-theta and beta-q in NN09, Eq. B8).
         ! The "-1" and "-tv0" terms are included for consistency with
         ! the legacy vt and vq formulations (above).

      ELSE

         q1k  = q1(k)
         zagl = zagl + dz(k)
         !COMPUTE MEAN RH IN PBL (NOT PRESSURE WEIGHTED).
         IF (zagl < PBLH2 .AND. PBLH2 > 400.) THEN
            RHsum=RHsum+RH(k)
            RHnum=RHnum+1.0
            RHmean=RHsum/RHnum
         ENDIF
!         IF (cld(k) < 0. .OR. cld(k) > 1.) THEN
!            PRINT*,"MYM_CONDENSATION, k=",k," cld=",cld(k)
!            PRINT*," r3sq=",r3sq," t3sq=",t3sq," c3sq=",c3sq
!         ENDIF
!         q1=0.
!         cld(k)=0.

         IF ( bl_mynn_cloudpdf <= 1 ) THEN
       
              RHcrit = 1. - 0.35*(1.0 - (MAX(250.- MAX(HFX1,HFXmin),0.0)/200.)**2)
              if(HFX1 > HFXmin)then
                 cld9=MIN(MAX(0., (rh(k)-RHcrit)/(1.1-RHcrit)), 1.)**2
              else
                 cld9=0.0
              endif
       
              edown=PBLH2*.1
              !Vary BL cloud depth (Hshcu) by mean RH in PBL and HFX 
              !(somewhat following results from Zhang and Klein (2013, JAS))
              Hshcu=200. + (RHmean+0.5)**1.5*MAX(HFX1,0.)*Hfac
              if(zagl < PBLH2-edown)then
                 damp=MIN(1.0,exp(-ABS(((PBLH2-edown)-zagl)/edown)))
              elseif(zagl >= PBLH2-edown .AND. zagl < PBLH2+Hshcu)then
                 damp=1.
              elseif (zagl >= PBLH2+Hshcu)then
                 damp=MIN(1.0,exp(-ABS((zagl-(PBLH2+Hshcu))/500.)))
              endif
       !       cldfra_bl1D(k)=cld9*damp
             cldfra_bl1D(k)=cld(k) ! JAYMES: use this form to retain the Sommeria-Deardorff value
       
              !use alternate cloud fraction to estimate qc for use in BL clouds-radiation
              eq1  = rrp*EXP( -0.5*q1k*q1k )
              qll  = MAX( cldfra_bl1D(k)*q1k + eq1, 0.0 )
              !ESTIMATED LIQUID WATER CONTENT (UNNORMALIZED)
              ql (k) = alp(k)*sgm(k)*qll
              if(cldfra_bl1D(k)>0.01 .and. ql(k)<1.E-6)ql(k)=1.E-6
       !         qc_bl1D(k)=ql(k)*damp
             qc_bl1D(k)=ql(k) ! JAYMES: use this form to retain the Sommeria-Deardorff value
       
              !now recompute estimated lwc for PBL scheme's use
              !qll IS THE NORMALIZED LIQUID WATER CONTENT (Sommeria and
              !Deardorff (1977, eq 29a). rrp = 1/(sqrt(2*pi)) = 0.3989
              eq1  = rrp*EXP( -0.5*q1k*q1k )
              qll  = MAX( cld(k)*q1k + eq1, 0.0 )
              !ESTIMATED LIQUID WATER CONTENT (UNNORMALIZED)
              ql (k) = alp(k)*sgm(k)*qll
       
              q2p = xlvcp/exner(k)
              pt = thl(k) +q2p*ql(k) ! potential temp
       
              !qt is a THETA-V CONVERSION FOR TOTAL WATER (i.e., THETA-V = qt*THETA)
              qt   = 1.0 +p608*qw(k) -(1.+p608)*ql(k)
              rac  = alp(k)*( cld(k)-qll*eq1 )*( q2p*qt-(1.+p608)*pt )
       
              !BUOYANCY FACTORS: wherever vt and vq are used, there is a
              !"+1" and "+tv0", respectively, so these are subtracted out here.
              !vt is unitless and vq has units of K.
              vt(k) =      qt-1.0 -rac*bet(k)
              vq(k) = p608*pt-tv0 +rac
       
         ELSE IF ( bl_mynn_cloudpdf == 2 ) THEN
         ! JAYMES- this option added 8 May 2015
         ! The cloud water formulations are taken from CB02, Eq. 8.
         ! "fng" represents the non-Gaussian contribution to the liquid
         ! water flux; these formulations are from Cuijpers and Bechtold
         ! (1995), Eq. 7.  CB95 also draws from Bechtold et al. 1995,
         ! hereafter BCMT95
              IF (q1k < 0.) THEN                 
                ql (k) = sgm(k)*EXP(1.2*q1k-1)
              ELSE IF (q1k > 2.) THEN
                ql (k) = sgm(k)*q1k
              ELSE
                ql (k) = sgm(k)*(EXP(-1.) + 0.66*q1k + 0.086*q1k**2)
              ENDIF
       
              !buoyancy-flux-related calculations follow
              IF (q1k < -2.) THEN
                fng = 1.-q1k
              ELSE IF (q1k > 0.) THEN
                fng = 0.
              ELSE
                fng = -1.5*q1k
              ENDIF
       
              xl    = xl_blend(t)
              q2p  = xlvcp/exner(k)
              pt = thl(k) +q2p*ql(k) ! potential temp
              bb = b(k)*t/pt ! bb is "b" in BCMT95.  Their "b" differs from 
                             ! "b" in CB02 (i.e., b(k) above) by a factor 
                             ! of T/theta.  Strictly, b(k) above is formulated in
                             ! terms of sat. mixing ratio, but bb in BCMT95 is
                             ! cast in terms of sat. specific humidity.  The
                             ! conversion is neglected here. 
              qww   = 1.+0.61*qw(k)
              alpha = 0.61*pt
              p2a   = exner(k)
              t     = thl(k)*p2a
              beta  = pt*xl/(t*cp) - 1.61*pt
       
              vt(k) = (1.-cld(k))*qww   + cld(k)*(qww   - beta*bb  *(1.+fng)) - 1.
              vq(k) = (1.-cld(k))*alpha + cld(k)*(alpha + beta*a(k)*(1.+fng)) - tv0
              ! These equations were derived by Jaymes, using BCMT95, Eq. B5, 
              ! in order to recast the BC02/BCMT95 buoyancy flux in terms of
              ! vt and vq (i.e., beta-theta and beta-q in NN09, Eq. B8).  
              ! The "-1" and "-tv0" terms are included for consistency with 
              ! the legacy vt and vq formulations (above).
       
              !return a cloud condensate and cloud fraction for icloud_bl option:
              cldfra_bl1D(k) = cld(k)
              qc_bl1D(k) = ql(k)
       
         ENDIF !end cloudPDF option

       ENDIF !end MF check

       !To avoid FPE in radiation driver, when these two quantities are multiplied by eachother,
       ! add limit to qc_bl and cldfra_bl:
       IF (QC_BL1D(k) < 1E-6 .AND. ABS(CLDFRA_BL1D(k)) > 0.01) QC_BL1D(k)= 1E-6
       IF (CLDFRA_BL1D(k) < 1E-2)THEN
          CLDFRA_BL1D(k)=0.
          QC_BL1D(k)=0.
       ENDIF

    END DO
!

    cld(kte) = cld(kte-1)
    ql(kte) = ql(kte-1)
    vt(kte) = vt(kte-1)
    vq(kte) = vq(kte-1)
    qc_bl1D(kte)=0.
    cldfra_bl1D(kte)=0.

    RETURN

  END SUBROUTINE mym_condensation

! ==================================================================
  SUBROUTINE mynn_tendencies(kts,kte,      &
       &levflag,grav_settling,             &
       &delt,dz,                           &
       &u,v,th,tk,qv,qc,qi,qni,qnc,        &
       &p,exner,                           &
       &thl,sqv,sqc,sqi,sqw,               &
       &ust,flt,flq,flqv,flqc,wspd,qcg,    &
       &uoce,voce,                         &
       &tsq,qsq,cov,                       &
       &tcd,qcd,                           &
       &dfm,dfh,dfq,                       &
       &Du,Dv,Dth,Dqv,Dqc,Dqi,Dqni,        &!Dqnc,   &
       &vdfg1,                             &
       &s_aw,s_awthl,s_awqt,s_awqv,s_awqc, &
       &s_awu,s_awv,                       &
       &FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC, &
       &cldfra_bl1d,                       &
       &bl_mynn_cloudmix,                  &
       &bl_mynn_mixqt,                     &
       &bl_mynn_edmf,                      &
       &bl_mynn_edmf_mom                  )

!-------------------------------------------------------------------
    INTEGER, INTENT(in) :: kts,kte
    INTEGER, INTENT(in) :: grav_settling,levflag
    INTEGER, INTENT(in) :: bl_mynn_cloudmix,bl_mynn_mixqt,&
                           bl_mynn_edmf,bl_mynn_edmf_mom
    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC

!! grav_settling = 1 or 2 for gravitational settling of droplets
!! grav_settling = 0 otherwise
! thl - liquid water potential temperature
! qw - total water
! dfm,dfh,dfq - as above
! flt - surface flux of thl
! flq - surface flux of qw

    REAL,DIMENSION(kts:kte+1), INTENT(in) :: s_aw,s_awthl,s_awqt,&
                             s_awqv,s_awqc,s_awu,s_awv
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,th,tk,qv,qc,qi,qni,qnc,&
         &p,exner,dfq,dz,tsq,qsq,cov,tcd,qcd,cldfra_bl1d
    REAL, DIMENSION(kts:kte), INTENT(inout) :: thl,sqw,sqv,sqc,sqi,&
         &dfm,dfh
    REAL, DIMENSION(kts:kte), INTENT(inout) :: du,dv,dth,dqv,dqc,dqi,&
         &dqni !,dqnc
    REAL, INTENT(IN) :: delt,ust,flt,flq,flqv,flqc,wspd,uoce,voce,qcg

!    REAL, INTENT(IN) :: delt,ust,flt,flq,qcg,&
!         &gradu_top,gradv_top,gradth_top,gradqv_top

!local vars

    REAL, DIMENSION(kts:kte) :: dtz,vt,vq,dfhc,dfmc !Kh for clouds (Pr < 2)
    REAL, DIMENSION(kts:kte) :: sqv2,sqc2,sqi2,sqw2,qni2 !,qnc2 !AFTER MIXING

    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    REAL :: rhs,gfluxm,gfluxp,dztop,maxdfh,mindfh
    REAL :: grav_settling2,vdfg1    !Katata-fogdes
    REAL :: t,esat,qsl,onoff
    INTEGER :: k,kk,nz

    nz=kte-kts+1

    dztop=.5*(dz(kte)+dz(kte-1))

    ! REGULATE THE MOMENTUM MIXING FROM THE MASS-FLUX SCHEME (on or off)
    IF (bl_mynn_edmf_mom == 0) THEN
       onoff=0.0
    ELSE
       onoff=1.0
    ENDIF
    ! Note that s_awu and s_awv already come in as 0.0 if bl_mynn_edmf_mom == 0, so
    ! we only need to zero-out the MF diffusivity term
    maxdfh=maxval(dfh(1:15))
    mindfh=maxdfh*0.01
    DO k=kts,kte
       dtz(k)=delt/dz(k)
       IF (dfm(k) > dfh(k)) THEN 
         !in stable regime only, limit Prandtl number to < 2 within clouds
         IF (qc(k) > 1.e-6 .OR. &
             qi(k) > 1.e-6 .OR. &
             cldfra_bl1D(k) > 0.05 ) THEN
             dfh(k)= MAX(dfh(k),dfm(k)*0.5)
         ENDIF
       ENDIF
       !add small minimum Km & Kh in MF updrafts for edmf2
       IF(bl_mynn_edmf==2 .AND. k > 1 .AND. s_aw(k)>0.0) THEN
           dfh(k)=MAX(mindfh,dfh(k))
           dfm(k)=MAX(mindfh,dfm(k))
       ENDIF
    ENDDO

!!============================================
!! u
!!============================================

    k=kts

    a(1)=0.
    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
    c(1)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
    d(1)=u(k) + dtz(k)*uoce*ust**2/wspd - dtz(k)*s_awu(k+1)*onoff

!!    a(1)=0.
!!    b(1)=1.+dtz(k)*dfm(k+1)
!!    c(1)=-dtz(k)*dfm(k+1)
!!    d(1)=u(k)*(1.-ust**2/wspd*dtz(k))

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(kk)=1. + dtz(k)*(dfm(k)+dfm(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff
       c(kk)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(kk)=u(k) + dtz(k)*(s_awu(k)-s_awu(k+1))*onoff
    ENDDO

!! no flux at the top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=0.

!! specified gradient at the top 
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=gradu_top*dztop

!! prescribed value
    a(nz)=0
    b(nz)=1.
    c(nz)=0.
    d(nz)=u(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       du(k)=(d(k-kts+1)-u(k))/delt
    ENDDO

!!============================================
!! v
!!============================================

    k=kts

    a(1)=0.
    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
    c(1)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
!    d(1)=v(k)
    d(1)=v(k) + dtz(k)*voce*ust**2/wspd - dtz(k)*s_awv(k+1)*onoff

!!    a(1)=0.
!!    b(1)=1.+dtz(k)*dfm(k+1)
!!    c(1)=-dtz(k)*dfm(k+1)
!!    d(1)=v(k)*(1.-ust**2/wspd*dtz(k))

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k) + 0.5*dtz(k)*s_aw(k)*onoff
       b(kk)=1. + dtz(k)*(dfm(k)+dfm(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff
       c(kk)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
       d(kk)=v(k) + dtz(k)*(s_awv(k)-s_awv(k+1))*onoff
    ENDDO

!! no flux at the top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=0.

!! specified gradient at the top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=gradv_top*dztop

!! prescribed value
    a(nz)=0
    b(nz)=1.
    c(nz)=0.
    d(nz)=v(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       dv(k)=(d(k-kts+1)-v(k))/delt
    ENDDO

!!============================================
!! thl 
!! NOTE: currently, gravitational settling is removed
!!============================================
    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(1)=-dtz(k)*(dfh(k+1) + 0.5*s_aw(k+1))

    rhs= tcd(k)

    d(1)=thl(k) + dtz(k)*flt + rhs*delt -dtz(1)*s_awthl(kts+1)

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*(dfh(k) - 0.5*s_aw(k))
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1) + 0.5*(s_aw(k)-s_aw(k+1)))
       c(kk)=-dtz(k)*(dfh(k+1) + 0.5*s_aw(k+1))

       rhs= tcd(k)

       d(kk)=thl(k) + rhs*delt + dtz(k)*(s_awthl(k)-s_awthl(k+1))
    ENDDO

!! no flux at the top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=0.

!! specified gradient at the top
!assume gradthl_top=gradth_top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=gradth_top*dztop

!! prescribed value
    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=thl(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       thl(k)=d(k-kts+1)
    ENDDO

IF (bl_mynn_mixqt > 0) THEN
 !============================================
 ! MIX total water (sqw = sqc + sqv + sqi)
 ! NOTE: no total water tendency is output; instead, we must calculate
 !       the saturation specific humidity and then 
 !       subtract out the moisture excess (sqc & sqi)
 !============================================

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)  - 0.5*dtz(k)*s_aw(k+1)
    c(1)=  -dtz(k)*(dfh(k+1) + 0.5*s_aw(k+1))

    rhs= qcd(k) !+ (gfluxp - gfluxm)/dz(k)& 

    d(1)=sqw(k) + dtz(k)*flq + rhs*delt - dtz(k)*s_awqt(k+1)

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*(dfh(k) - 0.5*s_aw(k))
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1) + 0.5*(s_aw(k)-s_aw(k+1)))
       c(kk)=-dtz(k)*(dfh(k+1) + 0.5*s_aw(k+1))

       rhs= qcd(k)

       d(kk)=sqw(k) + rhs*delt + dtz(k)*(s_awqt(k)-s_awqt(k+1))
    ENDDO

!! no flux at the top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=0.
!! specified gradient at the top
!assume gradqw_top=gradqv_top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=gradqv_top*dztop
!! prescribed value
    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqw(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqw2(k)=d(k-kts+1)
    ENDDO
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

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(1)=-dtz(k)*dfh(k+1)   - 0.5*dtz(k)*s_aw(k+1)

    rhs   =  qcd(k)
    d(1)=sqc(k) + dtz(k)*flqc + rhs*delt -dtz(k)*s_awqc(k+1)

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k) + 0.5*dtz(k)*s_aw(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(kk)=-dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)

       rhs = qcd(k)
       d(kk)=sqc(k) + rhs*delt + dtz(k)*(s_awqc(k)-s_awqc(k+1))
    ENDDO

! prescribed value
    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqc(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqc2(k)=d(k-kts+1)
    ENDDO
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

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
    c(1)=-dtz(k)*dfh(k+1)   - 0.5*dtz(k)*s_aw(k+1)
    d(1)=sqv(k) + dtz(k)*flqv + qcd(k)*delt - dtz(k)*s_awqv(k+1)  !note: using qt, not qv...

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k) + 0.5*dtz(k)*s_aw(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
       c(kk)=-dtz(k)*dfh(k+1)  - 0.5*dtz(k)*s_aw(k+1)
       d(kk)=sqv(k) + qcd(k)*delt + dtz(k)*(s_awqv(k)-s_awqv(k+1))
    ENDDO

! no flux at the top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=0.

! specified gradient at the top
! assume gradqw_top=gradqv_top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=gradqv_top*dztop

! prescribed value
    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqv(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqv2(k)=d(k-kts+1)
    ENDDO
ELSE
    sqv2=sqv
ENDIF

!============================================
! MIX CLOUD ICE ( sqi )                      
!============================================
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QI) THEN

    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    d(1)=sqi(k) !+ qcd(k)*delt !should we have qcd for ice???

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1))
       c(kk)=-dtz(k)*dfh(k+1)
       d(kk)=sqi(k) !+ qcd(k)*delt
    ENDDO

!! no flux at the top
!    a(nz)=-1.       
!    b(nz)=1.        
!    c(nz)=0.        
!    d(nz)=0.        

!! specified gradient at the top
!assume gradqw_top=gradqv_top
!    a(nz)=-1.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=gradqv_top*dztop

!! prescribed value
    a(nz)=0.
    b(nz)=1.
    c(nz)=0.
    d(nz)=sqi(kte)

    CALL tridiag(nz,a,b,c,d)

    DO k=kts,kte
       sqi2(k)=d(k-kts+1)
    ENDDO
ELSE
   sqi2=sqi
ENDIF

!!============================================
!! cloud ice number concentration (qni)
!!============================================
! diasbled this since scalar_pblmix option can be invoked instead
!IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNI) THEN
!
!    k=kts
!
!    a(1)=0.
!    b(1)=1.+dtz(k)*dfh(k+1)
!    c(1)=-dtz(k)*dfh(k+1)
!
!    rhs = qcd(k)
!
!    d(1)=qni(k) !+ dtz(k)*flqc + rhs*delt
!
!    DO k=kts+1,kte-1
!       kk=k-kts+1
!       a(kk)=-dtz(k)*dfh(k)
!       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1))
!       c(kk)=-dtz(k)*dfh(k+1)
!
!       rhs = qcd(k)
!       d(kk)=qni(k) !+ rhs*delt
!
!    ENDDO
!
!! prescribed value
!    a(nz)=0.
!    b(nz)=1.
!    c(nz)=0.
!    d(nz)=qni(kte)
!
!    CALL tridiag(nz,a,b,c,d)
!
!    DO k=kts,kte
!       qni2(k)=d(k-kts+1)
!    ENDDO
!ELSE
    qni2=qni
!ENDIF

!!============================================
!! Compute tendencies and convert to mixing ratios for WRF.
!! Note that the momentum tendencies are calculated above.
!!============================================

    DO k=kts,kte

       IF (bl_mynn_mixqt > 0) THEN
         t  = th(k)*exner(k)
         !SATURATED VAPOR PRESSURE
         esat=esat_blend(t)
         !SATURATED SPECIFIC HUMIDITY
         qsl=ep_2*esat/(p(k)-ep_3*esat)

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
       ENDIF

       !=====================
       ! WATER VAPOR TENDENCY
       !=====================
       Dqv(k)=(sqv2(k)/(1.-sqv2(k)) - qv(k))/delt
       !IF(-Dqv(k) > qv(k)) Dqv(k)=-qv(k)

       !=====================
       ! CLOUD WATER TENDENCY
       !=====================
       !qc fog settling tendency is now computed in module_bl_fogdes.F, so
       !sqc should only be changed by eddy diffusion or mass-flux.
       !print*,"FLAG_QC:",FLAG_QC
       IF (bl_mynn_cloudmix > 0 .AND. FLAG_QC) THEN
         Dqc(k)=(sqc2(k)/(1.-sqc2(k)) - qc(k))/delt
         IF(Dqc(k)*delt + qc(k) < 0.) THEN
           !WRITE ( mynn_message , FMT='(A,5(F8.6,A1),F6.1)' ) &
           !' MYNN; neg qc: ',qsl,' ',sqw2(k),' ',sqi2(k),' ',sqc2(k),' ',qc(k),' ',tk(k)
           !CALL wrf_debug ( 0 , mynn_message )
           Dqc(k)=-qc(k)/delt 
         ENDIF

         !REMOVED MIXING OF QNC - PERFORMED IN THE SCALAR_PBLMIX OPTION
         !IF (FLAG_QNC) THEN
         !  IF(sqc2(k)>1.e-9)qnc2(k)=MAX(qnc2(k),1.e6)
         !  Dqnc(k) = (qnc2(k)-qnc(k))/delt
         !  IF(Dqnc(k)*delt + qnc(k) < 0.)Dqnc(k)=-qnc(k)/delt 
         !ELSE
         !  Dqnc(k) = 0.
         !ENDIF
       ELSE
         Dqc(k)=0.
         !Dqnc(k)=0.
       ENDIF

       !===================
       ! CLOUD ICE TENDENCY
       !===================
       IF (bl_mynn_cloudmix > 0 .AND. FLAG_QI) THEN
         Dqi(k)=(sqi2(k)/(1.-sqi2(k)) - qi(k))/delt
         IF(Dqi(k)*delt + qi(k) < 0.) THEN
           !WRITE ( mynn_message , FMT='(A,5(F8.6,A1),F6.1)' ) &
           !' MYNN; neg qi; ',qsl,' ',sqw2(k),' ',sqi2(k),' ',sqc2(k),' ',qi(k),' ',tk(k)
           !CALL wrf_debug ( 0 , mynn_message )
           Dqi(k)=-qi(k)/delt
         ENDIF

         !REMOVED MIXING OF QNI - PERFORMED IN THE SCALAR_PBLMIX OPTION
         !SET qni2 = qni above, so all tendencies are zero
         IF (FLAG_QNI) THEN
           Dqni(k)=(qni2(k)-qni(k))/delt
           IF(Dqni(k)*delt + qni(k) < 0.)Dqni(k)=-qni(k)/delt
         ELSE
           Dqni(k)=0.
         ENDIF
       ELSE
         Dqi(k)=0.
         Dqni(k)=0.
       ENDIF

       !===================
       ! THETA TENDENCY
       !===================
       IF (FLAG_QI) THEN
         Dth(k)=(thl(k) + xlvcp/exner(k)*sqc(k) &
           &            + xlscp/exner(k)*sqi(k) &
           &            - th(k))/delt
         !Use form from Tripoli and Cotton (1981) with their
         !suggested min temperature to improve accuracy:
         !Dth(k)=(thl(k)*(1.+ xlvcp/MAX(tk(k),TKmin)*sqc2(k)  &
         !  &               + xlscp/MAX(tk(k),TKmin)*sqi2(k)) &
         !  &               - th(k))/delt
       ELSE
         Dth(k)=(thl(k)+xlvcp/exner(k)*sqc2(k) - th(k))/delt
         !Use form from Tripoli and Cotton (1981) with their
         !suggested min temperature to improve accuracy.
         !Dth(k)=(thl(k)*(1.+ xlvcp/MAX(tk(k),TKmin)*sqc2(k))  &
         !&               - th(k))/delt
       ENDIF

    ENDDO

  END SUBROUTINE mynn_tendencies

! ==================================================================
#if (WRF_CHEM == 1)
  SUBROUTINE mynn_mix_chem(kts,kte,      &
       levflag,grav_settling,             &
       delt,dz,                           &
       nchem, kdvel, ndvel, num_vert_mix, &
       chem1, vd1,                        &
       qni,qnc,        &
       p,exner,                           &
       thl,sqv,sqc,sqi,sqw,               &
       ust,flt,flq,flqv,flqc,wspd,qcg,    &
       uoce,voce,                         &
       tsq,qsq,cov,                       &
       tcd,qcd,                           &
       dfm,dfh,dfq,                       &
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
    REAL, DIMENSION( kts:kte, nchem ), INTENT(INOUT) :: chem1
    REAL, DIMENSION( kts:kte+1,nchem), INTENT(IN) :: s_awchem
    REAL, DIMENSION( ndvel ), INTENT(INOUT) :: vd1

!local vars

    REAL, DIMENSION(kts:kte) :: dtz,vt,vq
    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d
    REAL :: rhs,gfluxm,gfluxp,dztop
    REAL :: t,esl,qsl
    INTEGER :: k,kk,nz
    INTEGER :: ic  ! Chemical array loop index
    REAL, DIMENSION( kts:kte, nchem ) :: chem_new

    nz=kte-kts+1

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
       b(1)=1.+dtz(k)*dfh(k+1)
       c(1)=-dtz(k)*dfh(k+1)
       ! d(1)=sqv(k) + dtz(k)*flqv + qcd(k)*delt
       d(1)=chem1(k,ic) + dtz(k) * -vd1(ic)*chem1(1,ic)

       DO k=kts+1,kte-1
          kk=k-kts+1
          a(kk)=-dtz(k)*dfh(k)
          b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1))
          c(kk)=-dtz(k)*dfh(k+1)
          ! d(kk)=chem1(k,ic) + qcd(k)*delt
          d(kk)=chem1(k,ic) + rhs*delt + dtz(k)*(s_awchem(k,ic)-s_awchem(k+1,ic))
       ENDDO

      ! prescribed value at top
       a(nz)=0.
       b(nz)=1.
       c(nz)=0.
       d(nz)=chem1(kte,ic)

       CALL tridiag(nz,a,b,c,d)

       DO k=kts,kte
          chem_new(k,ic)=d(k-kts+1)
       ENDDO
    ENDDO

  END SUBROUTINE mynn_mix_chem
#endif

! ==================================================================
  SUBROUTINE retrieve_exchange_coeffs(kts,kte,&
       &dfm,dfh,dfq,dz,&
       &K_m,K_h,K_q)

!-------------------------------------------------------------------

    INTEGER , INTENT(in) :: kts,kte

    REAL, DIMENSION(KtS:KtE), INTENT(in) :: dz,dfm,dfh,dfq

    REAL, DIMENSION(KtS:KtE), INTENT(out) :: &
         &K_m, K_h, K_q


    INTEGER :: k
    REAL :: dzk

    K_m(kts)=0.
    K_h(kts)=0.
    K_q(kts)=0.

    DO k=kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       K_m(k)=dfm(k)*dzk
       K_h(k)=dfh(k)*dzk
       K_q(k)=Sqfac*dfq(k)*dzk
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
  SUBROUTINE mynn_bl_driver( & 
       &initflag,grav_settling,         &
       &delt,dz,dx,znt,                 &
       &u,v,w,th,qv,qc,qi,qni,qnc,      &
       &p,exner,rho,T3D,                &
       &xland,ts,qsfc,qcg,ps,           &
       &ust,ch,hfx,qfx,rmol,wspd,       &
       &uoce,voce,                      & !ocean current
       &vdfg,                           & !Katata-added for fog dep
       &Qke,tke_pbl,                    &
       &qke_adv,bl_mynn_tkeadvect,      & !ACF for QKE advection
#if (WRF_CHEM == 1)
       chem3d, vd3d, nchem,             & ! WA 7/29/15 For WRF-Chem
       kdvel, ndvel, num_vert_mix,      &
#endif
       &Tsq,Qsq,Cov,                    &
       &RUBLTEN,RVBLTEN,RTHBLTEN,       &
       &RQVBLTEN,RQCBLTEN,RQIBLTEN,     &
       &RQNIBLTEN,                      & !RQNCBLTEN, &
       &exch_h,exch_m,                  &
       &Pblh,kpbl,                      & 
       &el_pbl,                         &
       &dqke,qWT,qSHEAR,qBUOY,qDISS,    & !JOE-TKE BUDGET
       &wstar,delta,                    & !JOE-added for grims
       &bl_mynn_tkebudget,              & !JOE-TKE BUDGET
       &bl_mynn_cloudpdf,Sh3D,          & !JOE-cloudPDF testing
       &bl_mynn_mixlength,              & !JAYMES- mixing length options
       &icloud_bl,qc_bl,cldfra_bl,      & !JOE-subgrid bl clouds
       &bl_mynn_edmf,                   & !JOE- edmf
       &bl_mynn_edmf_mom,bl_mynn_edmf_tke, &
       &bl_mynn_edmf_part,              & !JOE- edmf
       &bl_mynn_cloudmix,bl_mynn_mixqt, & !JOE- cloud mixing methods
       &edmf_a,edmf_w,edmf_qt,          &
       &edmf_thl,edmf_ent,edmf_qc,      &
      &FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC &
       &,IDS,IDE,JDS,JDE,KDS,KDE        &
       &,IMS,IME,JMS,JME,KMS,KME        &
       &,ITS,ITE,JTS,JTE,KTS,KTE)
    
!-------------------------------------------------------------------

    INTEGER, INTENT(in) :: initflag
    !INPUT NAMELIST OPTIONS:
    INTEGER, INTENT(in) :: grav_settling
    INTEGER, INTENT(in) :: bl_mynn_tkebudget
    INTEGER, INTENT(in) :: bl_mynn_cloudpdf
    INTEGER, INTENT(in) :: bl_mynn_mixlength
    INTEGER, INTENT(in) :: bl_mynn_edmf
    LOGICAL, INTENT(IN) :: bl_mynn_tkeadvect
    INTEGER, INTENT(in) :: bl_mynn_edmf_mom
    INTEGER, INTENT(in) :: bl_mynn_edmf_tke
    INTEGER, INTENT(in) :: bl_mynn_edmf_part
    INTEGER, INTENT(in) :: bl_mynn_cloudmix
    INTEGER, INTENT(in) :: bl_mynn_mixqt
    INTEGER, INTENT(in) :: icloud_bl

    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC
    
    INTEGER,INTENT(IN) :: &
         & IDS,IDE,JDS,JDE,KDS,KDE &
         &,IMS,IME,JMS,JME,KMS,KME &
         &,ITS,ITE,JTS,JTE,KTS,KTE
    

! initflag > 0  for TRUE
! else        for FALSE
!       levflag         : <>3;  Level 2.5
!                         = 3;  Level 3
! grav_settling = 1 when gravitational settling accounted for
! grav_settling = 0 when gravitational settling NOT accounted for
    
    REAL, INTENT(in) :: delt,dx
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(in) :: dz,&
         &u,v,w,th,qv,p,exner,rho,T3D
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(in)::&
         &qc,qi,qni,qnc
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(in) :: xland,ust,&
         &ch,rmol,ts,qsfc,qcg,ps,hfx,qfx, wspd,uoce,voce, vdfg,znt

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &Qke,Tsq,Qsq,Cov, &
         &tke_pbl, & !JOE-added for coupling (TKE_PBL = QKE/2)
         &qke_adv    !ACF for QKE advection

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,&
         &RQIBLTEN,RQNIBLTEN !,RQNCBLTEN

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &exch_h,exch_m

   REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), OPTIONAL, INTENT(inout) :: &
         & edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc

    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(inout) :: &
         &Pblh,wstar,delta  !JOE-added for GRIMS

    REAL, DIMENSION(IMS:IME,JMS:JME) :: &
         &Psig_bl,Psig_shcu

    INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: & 
         &KPBL
    
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &el_pbl

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &qWT,qSHEAR,qBUOY,qDISS,dqke
    ! 3D budget arrays are not allocated when bl_mynn_tkebudget == 0.
    ! 1D (local) budget arrays are used for passing between subroutines.
    REAL, DIMENSION(KTS:KTE) :: qWT1,qSHEAR1,qBUOY1,qDISS1,dqke1

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: K_q,Sh3D

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &qc_bl,cldfra_bl
    REAL, DIMENSION(KTS:KTE) :: qc_bl1D,cldfra_bl1D

! WA 7/29/15 Mix chemical arrays
#if (WRF_CHEM == 1)
    INTEGER, INTENT(IN   )   ::   nchem, kdvel, ndvel, num_vert_mix
    REAL,    DIMENSION( ims:ime, kts:kte, jms:jme, nchem ), INTENT(INOUT) :: chem3d
    REAL,    DIMENSION( ims:ime, kdvel, jms:jme, ndvel ), INTENT(IN) :: vd3d
    REAL,    DIMENSION( kts:kte, nchem ) :: chem1
    REAL,    DIMENSION( kts:kte+1, nchem ) :: s_awchem1
    REAL,    DIMENSION( ndvel ) :: vd1
    INTEGER ic
#endif

!local vars
    INTEGER :: ITF,JTF,KTF, IMD,JMD
    INTEGER :: i,j,k
    REAL, DIMENSION(KTS:KTE) :: thl,tl,sqv,sqc,sqi,sqw,&
         &El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, Vt, Vq

    REAL, DIMENSION(KTS:KTE) :: thetav,sh,u1,v1,w1,p1,ex1,dz1,th1,tk1,rho1,&
           & qke1,tsq1,qsq1,cov1,qv1,qi1,qc1,du1,dv1,dth1,dqv1,dqc1,dqi1, &
           & k_m1,k_h1,k_q1,qni1,dqni1,qnc1 !,dqnc1

!JOE: mass-flux variables
    REAL, DIMENSION(KTS:KTE) :: dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf
    REAL, DIMENSION(KTS:KTE) :: edmf_a1,edmf_w1,edmf_qt1,edmf_thl1,&
                                edmf_ent1,edmf_qc1
    REAL,DIMENSION(KTS:KTE+1) :: s_aw1,s_awthl1,s_awqt1,&
                  s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqke1

    REAL, DIMENSION(KTS:KTE+1) :: zw
    REAL :: cpm,sqcg,flt,flq,flqv,flqc,pmz,phh,exnerg,zet,& 
              &afk,abk

!JOE-add GRIMS parameters & variables
   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   REAL :: govrth, sflux, bfx0, wstar3, wm2, wm3, delb
!JOE-end GRIMS
    INTEGER, SAVE :: levflag


    IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
        WRITE ( mynn_message , FMT='(A)' ) &
           'in MYNN driver; at beginning'
        CALL wrf_debug ( 0 , mynn_message )
    ENDIF

!***  Begin debugging
    IMD=(IMS+IME)/2
    JMD=(JMS+JME)/2
!***  End debugging 

    JTF=MIN0(JTE,JDE-1)
    ITF=MIN0(ITE,IDE-1)
    KTF=MIN0(KTE,KDE-1)

    levflag=mynn_level

    IF (bl_mynn_edmf > 0) THEN
      ! setup random seed
      call init_random_seed

      edmf_a(its:ite,kts:kte,jts:jte)=0.
      edmf_w(its:ite,kts:kte,jts:jte)=0.
      edmf_qt(its:ite,kts:kte,jts:jte)=0.
      edmf_thl(its:ite,kts:kte,jts:jte)=0.
      edmf_ent(its:ite,kts:kte,jts:jte)=0.
      edmf_qc(its:ite,kts:kte,jts:jte)=0.
    ENDIF

    IF (initflag > 0) THEN
 
       Sh3D(its:ite,kts:kte,jts:jte)=0.
       el_pbl(its:ite,kts:kte,jts:jte)=0.
       tsq(its:ite,kts:kte,jts:jte)=0.
       qsq(its:ite,kts:kte,jts:jte)=0.
       cov(its:ite,kts:kte,jts:jte)=0.
       dqc1(kts:kte)=0.0
       dqi1(kts:kte)=0.0
       dqni1(kts:kte)=0.0
       !dqnc1(kts:kte)=0.0
       qc_bl1D(kts:kte)=0.0
       cldfra_bl1D(kts:kte)=0.0
       edmf_a1(kts:kte)=0.0
       edmf_qc1(kts:kte)=0.0

       DO j=JTS,JTF
          DO i=ITS,ITF
             DO k=KTS,KTF
                dz1(k)=dz(i,k,j)
                u1(k) = u(i,k,j)
                v1(k) = v(i,k,j)
                w1(k) = w(i,k,j)
                th1(k)=th(i,k,j)
                tk1(k)=T3D(i,k,j)
                rho1(k)=rho(i,k,j)
                sqc(k)=qc(i,k,j)/(1.+qc(i,k,j))
                sqv(k)=qv(i,k,j)/(1.+qv(i,k,j))
                thetav(k)=th(i,k,j)*(1.+0.61*sqv(k))
                IF (PRESENT(qi) .AND. FLAG_QI ) THEN
                   sqi(k)=qi(i,k,j)/(1.+qi(i,k,j))
                   sqw(k)=sqv(k)+sqc(k)+sqi(k)
                   thl(k)=th(i,k,j)- xlvcp/exner(i,k,j)*sqc(k) &
                       &           - xlscp/exner(i,k,j)*sqi(k)
                   !Use form from Tripoli and Cotton (1981) with their
                   !suggested min temperature to improve accuracy.
                   !thl(k)=th(i,k,j)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k) &
                   !    &               - xlscp/MAX(tk1(k),TKmin)*sqi(k))
                ELSE
                   sqi(k)=0.0
                   sqw(k)=sqv(k)+sqc(k)
                   thl(k)=th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                   !Use form from Tripoli and Cotton (1981) with their 
                   !suggested min temperature to improve accuracy.      
                   !thl(k)=th(i,k,j)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k))
                ENDIF

                IF (k==kts) THEN
                   zw(k)=0.
                ELSE
                   zw(k)=zw(k-1)+dz(i,k-1,j)
                ENDIF

                exch_m(i,k,j)=0.
                exch_h(i,k,j)=0.
                K_q(i,k,j)=0.
                qke(i,k,j)=0.1-MIN(zw(k)*0.001, 0.0) !for initial PBLH calc only
                qke1(k)=qke(i,k,j)
                el(k)=el_pbl(i,k,j)
                sh(k)=Sh3D(i,k,j)
                tsq1(k)=tsq(i,k,j)
                qsq1(k)=qsq(i,k,j)
                cov1(k)=cov(i,k,j)

                IF ( bl_mynn_tkebudget == 1) THEN
                   !TKE BUDGET VARIABLES
                   qWT(i,k,j)=0.
                   qSHEAR(i,k,j)=0.
                   qBUOY(i,k,j)=0.
                   qDISS(i,k,j)=0.
                   dqke(i,k,j)=0.
                ENDIF
             ENDDO

             zw(kte+1)=zw(kte)+dz(i,kte,j)

             CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
               &  Qke1,zw,dz1,xland(i,j),KPBL(i,j))
             
             IF (scaleaware > 0.) THEN
                CALL SCALE_AWARE(dx,PBLH(i,j),Psig_bl(i,j),Psig_shcu(i,j))
             ELSE
                Psig_bl(i,j)=1.0
                Psig_shcu(i,j)=1.0
             ENDIF

             CALL mym_initialize ( & 
                  &kts,kte,    &
                  &dz1, zw, u1, v1, thl, sqv,  &
                  &PBLH(i,j), th1, sh,         &
                  &ust(i,j), rmol(i,j),        &
                  &el, Qke1, Tsq1, Qsq1, Cov1, &
                  &Psig_bl(i,j), cldfra_bl1D,  &
                  &bl_mynn_mixlength,          &
                  &edmf_a1,edmf_qc1,bl_mynn_edmf )

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
          DO k=KTS,KTF
             !JOE-TKE BUDGET
             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j)=qke(i,k,j)
             END IF
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
             sqc(k)= qc(i,k,j)/(1.+qc(i,k,j))
             dqc1(k)=0.0
             dqi1(k)=0.0
             dqni1(k)=0.0
             !dqnc1(k)=0.0
             IF(PRESENT(qi) .AND. FLAG_QI)THEN
                qi1(k)= qi(i,k,j)
                sqi(k)= qi(i,k,j)/(1.+qi(i,k,j))
                sqw(k)= sqv(k)+sqc(k)+sqi(k)
                thl(k)= th(i,k,j) - xlvcp/exner(i,k,j)*sqc(k) &
                     &            - xlscp/exner(i,k,j)*sqi(k)
                !Use form from Tripoli and Cotton (1981) with their
                !suggested min temperature to improve accuracy.    
                !thl(k)=th(i,k,j)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k) &
                !    &               - xlscp/MAX(tk1(k),TKmin)*sqi(k))
             ELSE
                qi1(k)=0.0
                sqi(k)=0.0
                sqw(k)= sqv(k)+sqc(k)
                thl(k)= th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
                !Use form from Tripoli and Cotton (1981) with their
                !suggested min temperature to improve accuracy.    
                !thl(k)=th(i,k,j)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k))
             ENDIF

             IF (PRESENT(qni) .AND. FLAG_QNI ) THEN
                qni1(k)=qni(i,k,j)
                !print*,"MYNN: Flag_qni=",FLAG_QNI,qni(i,k,j)
             ELSE
                qni1(k)=0.0
             ENDIF
             IF (PRESENT(qnc) .AND. FLAG_QNC ) THEN
                qnc1(k)=qnc(i,k,j)
                !print*,"MYNN: Flag_qnc=",FLAG_QNC,qnc(i,k,j)
             ELSE
                qnc1(k)=0.0
             ENDIF
             thetav(k)=th(i,k,j)*(1.+0.608*sqv(k))
             p1(k) = p(i,k,j)
             ex1(k)= exner(i,k,j)
             el(k) = el_pbl(i,k,j)
             qke1(k)=qke(i,k,j)
             sh(k) = sh3d(i,k,j)
             tsq1(k)=tsq(i,k,j)
             qsq1(k)=qsq(i,k,j)
             cov1(k)=cov(i,k,j)

             !edmf
             edmf_a1(k)=0.0
             edmf_qc1(k)=0.0
             s_aw1(k)=0.
             s_awthl1(k)=0.
             s_awqt1(k)=0.
             s_awqv1(k)=0.
             s_awqc1(k)=0.
             s_awu1(k)=0.
             s_awv1(k)=0.
             s_awqke1(k)=0.

#if (WRF_CHEM == 1)
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
#endif

             IF (k==kts) THEN
                zw(k)=0.
             ELSE
                zw(k)=zw(k-1)+dz(i,k-1,j)
             ENDIF
          ENDDO

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
#if (WRF_CHEM == 1)
          DO ic = 1,nchem
            s_awchem1(kte+1,ic)=0.
          ENDDO
#endif

          CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
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
          flqv = qfx(i,j)/rho(i,kts,j)
          flqc = -vdfg(i,j)*(sqc(kts) - sqcg )

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

          IF (bl_mynn_edmf == 1) THEN
            !PRINT*,"Calling StEM Mass-Flux: i= ",i," j=",j
            CALL StEM_mf(& 
               &kts,kte,delt,zw,p1,               &
               &bl_mynn_edmf_mom,                 &
               &bl_mynn_edmf_tke,                 &
               &u1,v1,w1,th1,thl,thetav,tk1,      &
               &sqw,sqv,sqc,qke1,                 &
               &ust(i,j),flt,flq,flqv,flqc,       &
               &PBLH(i,j),DX,xland(i,j),          &
            ! now outputs - tendencies
            ! &,dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf &
            ! outputs - updraft properties
               & edmf_a1,edmf_w1,edmf_qt1,        &
               & edmf_thl1,edmf_ent1,edmf_qc1,    &
            ! for the solver
               & s_aw1,s_awthl1,s_awqt1,          &
               & s_awqv1,s_awqc1,s_awu1,s_awv1,   &
               & s_awqke1,                        &
#if (WRF_CHEM == 1)
               & nchem,chem1,s_awchem1,           &
#endif
               & qc_bl1D,cldfra_bl1D,             &
               & FLAG_QI,FLAG_QC,                 &
               & Psig_shcu(i,j)                   &
            )

          ELSEIF (bl_mynn_edmf == 2) THEN
            CALL temf_mf( &
               &kts,kte,delt,zw,p1,ex1,           &
               &u1,v1,w1,th1,thl,thetav,          &
               &sqw,sqv,sqc,qke1,                 &
               &ust(i,j),flt,flq,flqv,flqc,       &
               &hfx(i,j),qfx(i,j),ts(i,j),        &
               &pblh(i,j),rho1,dfh,dx,znt(i,j),ep_2,   &
            ! outputs - updraft properties
               & edmf_a1,edmf_w1,edmf_qt1,        &
               & edmf_thl1,edmf_ent1,edmf_qc1,    &
            ! for the solver
               & s_aw1,s_awthl1,s_awqt1,          &
               & s_awqv1,s_awqc1,                 &
               & s_awu1,s_awv1,s_awqke1,          &
#if (WRF_CHEM == 1)
               & nchem,chem1,s_awchem1,           &
#endif
               & qc_bl1D,cldfra_bl1D              &
               &,FLAG_QI,FLAG_QC                  &
               &,Psig_shcu(i,j)                   &
               &,i,j,ids,ide,jds,jde              &
             )
          ENDIF


          CALL  mym_condensation ( kts,kte,      &
               &dx,dz1,thl,sqw,p1,ex1,           &
               &tsq1, qsq1, cov1,                &
               &Sh,el,bl_mynn_cloudpdf,          & !JOE-cloud PDF testing (Kuwano-Yoshida et al. 2010)
               &qc_bl1D,cldfra_bl1D,             & !JOE-subgrid BL clouds
               &PBLH(i,j),HFX(i,j),              & !JOE-subgrid BL clouds
               &edmf_qc1,                        &
               &Vt, Vq, th1)

          CALL mym_turbulence ( &
               &kts,kte,levflag, &
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
               &edmf_a1,edmf_qc1,bl_mynn_edmf)


!          IF (bl_mynn_edmf > 0) THEN
!            !DEBUG
!             DO k=kts,kte
!               IF (s_aw1(k)<0. .OR. s_aw1(k)>0.5) THEN
!                  PRINT*,"After Mass-Flux: i= ",i," j=",j," k=",k
!                  PRINT*," s_aw1=",s_aw1(k)," s_awthl1=",s_awthl1(k)," s_awqt1=",s_awqt1(k)
!                  PRINT*," s_awu1=",s_awu1(k)," s_awv1=",s_awu1(k)
!               ENDIF
!             ENDDO
!          ENDIF

          IF (bl_mynn_edmf_part > 0 .AND. bl_mynn_edmf > 0) THEN
             !Partition the fluxes from each component (ed & mf).
             !Assume overlap of 50%: Reduce eddy diffusivities by 50% of the estimated
             !area fraction of mass-flux scheme's updraft.
             DO k=kts,kte
                dfm(k)=dfm(k) * (1. - 0.5*edmf_a1(k))
                dfh(k)=dfh(k) * (1. - 0.5*edmf_a1(k))
                dfq(k)=dfq(k) * (1. - 0.5*edmf_a1(k))
             ENDDO
          ENDIF

          CALL mym_predict (kts,kte,levflag,     &
               &delt, dz1,                       &
               &ust(i,j), flt, flq, pmz, phh,    &
               &el, dfq, pdk, pdt, pdq, pdc,     &
               &Qke1, Tsq1, Qsq1, Cov1,          &
               &s_aw1, s_awqke1, bl_mynn_edmf_tke)

          CALL mynn_tendencies(kts,kte,          &
               &levflag,grav_settling,           &
               &delt, dz1,                       &
               &u1, v1, th1, tk1, qv1, qc1, qi1, &
               &qni1,qnc1,                       &
               &p1, ex1, thl, sqv, sqc, sqi, sqw,&
               &ust(i,j),flt,flq,flqv,flqc,      &
               &wspd(i,j),qcg(i,j),              &
               &uoce(i,j),voce(i,j),             &
               &tsq1, qsq1, cov1,                &
               &tcd, qcd,                        &
               &dfm, dfh, dfq,                   &
               &Du1, Dv1, Dth1, Dqv1,            &
               &Dqc1, Dqi1, Dqni1,               & !Dqnc1,        &
               &vdfg(i,j),                       & !JOE/Katata- fog deposition
               ! mass flux components
               &s_aw1,s_awthl1,s_awqt1,          &
               &s_awqv1,s_awqc1,s_awu1,s_awv1,   &
               &FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC,&
               &cldfra_bl1d,                     &
               &bl_mynn_cloudmix,                &
               &bl_mynn_mixqt,                   &
               &bl_mynn_edmf,                    &
               &bl_mynn_edmf_mom)

#if (WRF_CHEM == 1)
    IF (bl_mynn_mixchem == 1) THEN
          CALL mynn_mix_chem(kts,kte,          &
               levflag,grav_settling,           &
               delt, dz1,                       &
               nchem, kdvel, ndvel, num_vert_mix, &
               chem1, vd1,                      &
               qni1,qnc1,                       &
               p1, ex1, thl, sqv, sqc, sqi, sqw,&
               ust(i,j),flt,flq,flqv,flqc,      &
               wspd(i,j),qcg(i,j),              &
               uoce(i,j),voce(i,j),             &
               tsq1, qsq1, cov1,                &
               tcd, qcd,                        &
               &dfm, dfh, dfq,                  &
               ! mass flux components
               & s_awchem1,                      &
               &bl_mynn_cloudmix)
    ENDIF
#endif

!
! add mass flux tendencies and calculate the new variables. 
! Now done implicitly in the mynn_tendencies subroutine
!           do k=kts,kte
!             du1(k)=du1(k)+du1mf(k)
!             dv1(k)=dv1(k)+dv1mf(k)
!             dth1(k)=dth1(k)+dth1mf(k)
!             dqv1(k)=dqv1(k)+dqv1mf(k)
! that is supposed to be done by bl_fogdes
!             dqc1(k)=dqc1(k)+dqc1mf(k) 
!           enddo
 

          CALL retrieve_exchange_coeffs(kts,kte,&
               &dfm, dfh, dfq, dz1,&
               &K_m1, K_h1, K_q1)

          !UPDATE 3D ARRAYS
          DO k=KTS,KTF
             exch_m(i,k,j)=K_m1(k)
             exch_h(i,k,j)=K_h1(k)
             K_q(i,k,j)=K_q1(k)
             RUBLTEN(i,k,j)=du1(k)
             RVBLTEN(i,k,j)=dv1(k)
             RTHBLTEN(i,k,j)=dth1(k)
             RQVBLTEN(i,k,j)=dqv1(k)
             IF(bl_mynn_cloudmix > 0)THEN
               IF (PRESENT(qc) .AND. FLAG_QC) RQCBLTEN(i,k,j)=dqc1(k)
               IF (PRESENT(qi) .AND. FLAG_QI) RQIBLTEN(i,k,j)=dqi1(k)
               !IF (PRESENT(qnc) .AND. FLAG_QNC) RQNCBLTEN(i,k,j)=dqnc1(k)
               IF (PRESENT(qni) .AND. FLAG_QNI) RQNIBLTEN(i,k,j)=dqni1(k)
             ELSE
               IF (PRESENT(qc) .AND. FLAG_QC) RQCBLTEN(i,k,j)=0.
               IF (PRESENT(qi) .AND. FLAG_QI) RQIBLTEN(i,k,j)=0.
               !IF (PRESENT(qnc) .AND. FLAG_QNC) RQNCBLTEN(i,k,j)=0.
               IF (PRESENT(qni) .AND. FLAG_QNI) RQNIBLTEN(i,k,j)=0.
             ENDIF
             IF(icloud_bl > 0)THEN
               !make BL clouds scale aware - may already be done in mym_condensation
               qc_bl(i,k,j)=qc_bl1D(k) !*Psig_shcu(i,j)
               cldfra_bl(i,k,j)=cldfra_bl1D(k) !*Psig_shcu(i,j)
             ENDIF
             el_pbl(i,k,j)=el(k)
             qke(i,k,j)=qke1(k)
             tsq(i,k,j)=tsq1(k)
             qsq(i,k,j)=qsq1(k)
             cov(i,k,j)=cov1(k)
             sh3d(i,k,j)=sh(k)

             IF ( bl_mynn_tkebudget == 1) THEN
                dqke(i,k,j)  = (qke1(k)-dqke(i,k,j))*0.5  !qke->tke
                qWT(i,k,j)   = qWT1(k)*delt
                qSHEAR(i,k,j)= qSHEAR1(k)*delt
                qBUOY(i,k,j) = qBUOY1(k)*delt
                qDISS(i,k,j) = qDISS1(k)*delt
             ENDIF

             !update updraft properties
             IF (bl_mynn_edmf > 0) THEN
               edmf_a(i,k,j)=edmf_a1(k)
               edmf_w(i,k,j)=edmf_w1(k)
               edmf_qt(i,k,j)=edmf_qt1(k)
               edmf_thl(i,k,j)=edmf_thl1(k)
               edmf_ent(i,k,j)=edmf_ent1(k)
               edmf_qc(i,k,j)=edmf_qc1(k)
               !Reapply checks on cldfra_bl and qc_bl to avoid FPEs in radiation driver
               ! when these two quantities are multiplied by eachother (they may have changed 
               ! in the MF scheme:
               IF (icloud_bl > 0) THEN
                 IF (QC_BL(i,k,j) < 1E-6 .AND. ABS(CLDFRA_BL(i,k,j)) > 0.1)QC_BL(i,k,j)= 1E-6
                 IF (CLDFRA_BL(i,k,j) < 1E-2)CLDFRA_BL(i,k,j)= 0.
               ENDIF
             ENDIF

             !***  Begin debug prints
             IF ( wrf_at_debug_level(3000) ) THEN
               IF ( sh(k) < 0. .OR. sh(k)> 200.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," sh=",sh(k)
               IF ( qke(i,k,j) < -1. .OR. qke(i,k,j)> 200.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," qke=",qke(i,k,j)
               IF ( el_pbl(i,k,j) < 0. .OR. el_pbl(i,k,j)> 2000.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," el_pbl=",el_pbl(i,k,j)
               IF ( ABS(vt(k)) > 0.8 )print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vt=",vt(k)
               IF ( ABS(vq(k)) > 6000.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vq=",vq(k) 
               IF ( exch_m(i,k,j) < 0. .OR. exch_m(i,k,j)> 2000.)print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," exxch_m=",exch_m(i,k,j)
               IF ( vdfg(i,j) < 0. .OR. vdfg(i,j)>5. )print*,"**SUSPICIOUS VALUES AT: i,j,k=",i,j,k," vdfg=",vdfg(i,j)
               IF ( ABS(QFX(i,j))>.001)print*, "**SUSPICIOUS VALUES AT: i,j=",i,j," QFX=",QFX(i,j) 
               IF ( ABS(HFX(i,j))>1000.)print*, "**SUSPICIOUS VALUES AT: i,j=",i,j," HFX=",HFX(i,j)
               IF (icloud_bl > 0) then
                  IF( cldfra_bl(i,k,j) < 0.0 .OR. cldfra_bl(i,k,j)> 1.)THEN
                  PRINT*,"**SUSPICIOUS VALUES: CLDFRA_BL=",cldfra_bl(i,k,j)," qc_bl=",QC_BL(i,k,j)
                  ENDIF
               ENDIF
             ENDIF
             !***  End debug prints
          ENDDO

          !JOE-add tke_pbl for coupling w/shallow-cu schemes (TKE_PBL = QKE/2.)
          !    TKE_PBL is defined on interfaces, while QKE is at middle of layer.
          tke_pbl(i,kts,j) = 0.5*MAX(qke(i,kts,j),1.0e-10)
          DO k = kts+1,kte
             afk = dz1(k)/( dz1(k)+dz1(k-1) )
             abk = 1.0 -afk
             tke_pbl(i,k,j) = 0.5*MAX(qke(i,k,j)*abk+qke(i,k-1,j)*afk,1.0e-3)
          ENDDO

!***  Begin debugging
!          IF(I==IMD .AND. J==JMD)THEN
!             k=kdebug
!             PRINT*,"MYNN DRIVER END: k=",1," sh=",sh(k)
!             PRINT*," sqw=",sqw(k)," thl=",thl(k)," k_m=",k_m(i,k,j)
!             PRINT*," xland=",xland(i,j)," rmol=",rmol(i,j)," ust=",ust(i,j)
!             PRINT*," qke=",qke(i,k,j)," el=",el_pbl(i,k,j)," tsq=",tsq(i,k,j)
!             PRINT*," PBLH=",PBLH(i,j)," u=",u(i,k,j)," v=",v(i,k,j)
!             PRINT*," vq=",vq(k)," vt=",vt(k)," vdfg=",vdfg(i,j)
!          ENDIF
!***  End debugging

       ENDDO
    ENDDO

!ACF copy qke into qke_adv if using advection
    IF (bl_mynn_tkeadvect) THEN
       qke_adv=qke
    ENDIF
!ACF-end
    
  END SUBROUTINE mynn_bl_driver

! ==================================================================
  SUBROUTINE mynn_bl_init_driver(                   &
       &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,          &
       &RQCBLTEN,RQIBLTEN & !,RQNIBLTEN,RQNCBLTEN       &
       &,QKE,TKE_PBL,EXCH_H                         &
!       &,icloud_bl,qc_bl,cldfra_bl                 & !JOE-subgrid bl clouds 
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
         &QKE,TKE_PBL,EXCH_H

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
                TKE_PBL(i,k,j)=0.
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
    REAL, INTENT(OUT) :: zi
    REAL, INTENT(IN) :: landsea
    REAL, DIMENSION(KTS:KTE), INTENT(IN) :: thetav1D, qke1D, dz1D
    REAL, DIMENSION(KTS:KTE+1), INTENT(IN) :: zw1D
    !LOCAL VARS
    REAL ::  PBLH_TKE,qtke,qtkem1,wt,maxqke,TKEeps,minthv
    REAL :: delt_thv   !delta theta-v; dependent on land/sea point
    REAL, PARAMETER :: sbl_lim  = 400. !200. !upper limit of stable BL height (m).
    REAL, PARAMETER :: sbl_damp = 800.!400. !transition length for blending (m).
    INTEGER :: I,J,K,kthv,ktke,kzi,kzi2

    !ADD KPBL (kzi) for coupling to some Cu schemes, initialize at k=2                                  
    !kzi2 is the TKE-based part of the hybrid KPBL                                                      
    kzi = 2
    kzi2= 2

    !FIND MAX TKE AND MIN THETAV IN THE LOWEST 500 M
    k = kts+1
    kthv = 1
    ktke = 1
    maxqke = 0.
    minthv = 9.E9
    DO WHILE (zw1D(k) .LE. sbl_lim)
       qtke  =MAX(Qke1D(k),0.)   ! maximum QKE
       IF (maxqke < qtke) then
           maxqke = qtke
           ktke = k
       ENDIF
       IF (minthv > thetav1D(k)) then
           minthv = thetav1D(k)
           kthv = k
       ENDIF
       k = k+1
    ENDDO

    !Use 5% of tke max (Kosovic and Curry, 2000; JAS)
    !TKEeps = maxtke/20. = maxqke/40.
    TKEeps = maxqke/40.
    TKEeps = MAX(TKEeps,0.02) !0.010) !0.025)

    !FIND THETAV-BASED PBLH (BEST FOR DAYTIME).
    zi=0.
    k = kthv+1
    IF((landsea-1.5).GE.0)THEN
        ! WATER
        delt_thv = 0.75
    ELSE
        ! LAND
        delt_thv = 1.25
    ENDIF

    zi=0.
    k = kthv+1
    DO WHILE (zi .EQ. 0.) 
       IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
          kzi = MAX(k-1,1)
          zi = zw1D(k) - dz1D(k-1)* &
             & MIN((thetav1D(k)-(minthv + delt_thv))/ &
             & MAX(thetav1D(k)-thetav1D(k-1),1E-6),1.0)
       ENDIF
       k = k+1
       IF (k .EQ. kte-1) zi = zw1D(kts+1) !EXIT SAFEGUARD
    ENDDO
    !print*,"IN GET_PBLH:",thsfc,zi

    !FOR STABLE BOUNDARY LAYERS, USE TKE METHOD TO COMPLEMENT THE
    !THETAV-BASED DEFINITION (WHEN THE THETA-V BASED PBLH IS BELOW ~0.5 KM).
    !THE TANH WEIGHTING FUNCTION WILL MAKE THE TKE-BASED DEFINITION NEGLIGIBLE 
    !WHEN THE THETA-V-BASED DEFINITION IS ABOVE ~1 KM.

    PBLH_TKE=0.
    k = ktke+1
    DO WHILE (PBLH_TKE .EQ. 0.) 
       !QKE CAN BE NEGATIVE (IF CKmod == 0)... MAKE TKE NON-NEGATIVE.
       qtke  =MAX(Qke1D(k)/2.,0.)      ! maximum TKE
       qtkem1=MAX(Qke1D(k-1)/2.,0.)
       IF (qtke .LE. TKEeps) THEN
           kzi2 = MAX(k-1,1)
           PBLH_TKE = zw1D(k) - dz1D(k-1)* &
             & MIN((TKEeps-qtke)/MAX(qtkem1-qtke, 1E-6), 1.0)
           !IN CASE OF NEAR ZERO TKE, SET PBLH = LOWEST LEVEL.
           PBLH_TKE = MAX(PBLH_TKE,zw1D(kts+1))
           !print *,"PBLH_TKE:",i,j,PBLH_TKE, Qke1D(k)/2., zw1D(kts+1)
       ENDIF
       k = k+1
       IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1) !EXIT SAFEGUARD
    ENDDO

    !With TKE advection turned on, the TKE-based PBLH can be very large 
    !in grid points with convective precipitation (> 8 km!),
    !so an artificial limit is imposed to not let PBLH_TKE xceed 4km.
    !This has no impact on 98-99% of the domain, but is the simplest patch
    !that adequately addresses these extremely large PBLHs.
    !PBLH_TKE = MIN(PBLH_TKE,4000.)
    PBLH_TKE = MIN(PBLH_TKE,zi+500.)
    PBLH_TKE = MAX(PBLH_TKE,MAX(zi-500.,10.))

    wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
    IF (maxqke <= 0.05) THEN
       !Cold pool situation - default to theta_v-based def
    ELSE
       !BLEND THE TWO PBLH TYPES HERE: 
       zi=PBLH_TKE*(1.-wt) + zi*wt
    ENDIF

    !ADD KPBL (kzi) for coupling to some Cu schemes
     kzi = MAX(INT(kzi2*(1.-wt) + kzi*wt),1)

  END SUBROUTINE GET_PBLH
  
! ==================================================================
! Much thanks to Kay Suslj of NASA-JPL for contributing the original version
! of this mass-flux scheme. Considerable changes have been made from it's
! original form. Some additions include:
!  1) scale-aware tapering as dx -> 0
!  2) transport of TKE (extra namelist option)
!  3) Chaboureau-Bechtold cloud fraction & coupling to radiation (when icloud_bl > 0)
!  4) some extra limits for numerical stability
! This scheme remains under development, so consider it experimental code. 
!
  SUBROUTINE StEM_mf( & 
                 & kts,kte,dt,zw,p,         &
                 & momentum_opt,            &
                 & tke_opt,                 &
                 & u,v,w,th,thl,thv,tk,     &
                 & qt,qv,qc,qke,            &
                 & ust,flt,flq,flqv,flqc,   &
                 & pblh,DX,landsea,         &
            ! outputs - tendencies
            !  &dth,dqv,dqc,du,dv,&
            ! outputs - updraft properties   
                 & edmf_a,edmf_w,           &
                 & edmf_qt,edmf_thl,        &
                 & edmf_ent,edmf_qc,        &
            ! outputs - variables needed for solver 
                 & s_aw,s_awthl,s_awqt,     &
                 & s_awqv,s_awqc,           &
                 & s_awu,s_awv,s_awqke,     &
#if (WRF_CHEM == 1)
                 & nchem,chem,s_awchem,      &
#endif
            ! in/outputs - subgrid scale clouds
                 & qc_bl1d,cldfra_bl1d,     &
            ! inputs - flags for moist arrays
                 &F_QC,F_QI,                &
                 &Psig_shcu) 

  ! inputs:
     INTEGER, INTENT(IN) :: KTS,KTE,momentum_opt,tke_opt
     REAL,DIMENSION(KTS:KTE), INTENT(IN) :: U,V,W,TH,THL,TK,QT,QV,QC,THV,P,qke
     REAL,DIMENSION(KTS:KTE+1), INTENT(IN) :: ZW  !height at full-sigma
     REAL, INTENT(IN) :: DT,UST,FLT,FLQ,FLQV,FLQC,PBLH,DX,Psig_shcu,landsea
     LOGICAL, OPTIONAL :: F_QC,F_QI

  ! outputs - tendencies
  !   REAL,DIMENSION(KTS:KTE), INTENT(OUT) :: DTH,DQV,DQC,DU,DV
  ! outputs - updraft properties
     REAL,DIMENSION(KTS:KTE), INTENT(OUT) :: edmf_a,edmf_w,        &
                 & edmf_qt,edmf_thl, edmf_ent,edmf_qc

  ! outputs - variables needed for solver
     REAL,DIMENSION(KTS:KTE+1) :: s_aw,      & !sum ai*wis_awphi
                               s_awthl,      & !sum ai*wi*phii
                                s_awqt,      &
                                s_awqv,      &
                                s_awqc,      &
                                 s_awu,      &
                                 s_awv,      &
                               s_awqke

     REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: qc_bl1d,cldfra_bl1d

    INTEGER, PARAMETER :: NUP=10
  ! local variables
  ! updraft properties
     REAL,DIMENSION(KTS:KTE+1,1:NUP) :: UPW,UPTHL,UPQT,UPQC,UPQV,  &
                                        UPA,UPU,UPV,UPTHV,UPQKE
  ! entrainment variables
     REAL,DIMENSION(KTS:KTE,1:NUP) :: ENT,ENTf
     INTEGER,DIMENSION(KTS:KTE,1:NUP) :: ENTi
  ! internal variables
     INTEGER :: K,I,k50
     REAL :: fltv,wstar,qstar,thstar,sigmaW,sigmaQT,sigmaTH,z0,    &
             pwmin,pwmax,wmin,wmax,wlv,wtv,Psig_w,maxw
     REAL :: B,QTn,THLn,THVn,QCn,Un,Vn,QKEn,Wn2,EntEXP,EntW

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

#if (WRF_CHEM == 1)
     INTEGER, INTENT(IN) :: nchem
     REAL,DIMENSION(kts:kte+1, nchem) :: chem
     REAL,DIMENSION(kts:kte+1, nchem) :: s_awchem
     REAL,DIMENSION(nchem) :: chemn
     REAL,DIMENSION(KTS:KTE+1,1:NUP, nchem) :: UPCHEM
     INTEGER :: ic
     REAL,DIMENSION(KTS:KTE+1, nchem) :: edmf_chem
#endif

  !JOE: add declaration of ERF
   REAL :: ERF

  !JOE: add option to switch cloud fraction calculations:
   INTEGER, PARAMETER :: cldfra_opt = 0  ! 0: Chaboureau and Bechtold (2005, JGR)
                                         ! 1: Xu & Randall (1994)
  !JOE:add for cldfra
   REAL :: xcldfra, UMF_new, dcf, ktop_dcf, kbcon_dcf
  ! PARAMETERS FOR RANDALL AND XU (1996) CLOUD FRACTION
   REAL, PARAMETER  ::  coef_p = 0.25, coef_gamm = 0.49, coef_alph = 100.
   REAL :: satvp,rhgrid,h2oliq
   LOGICAL :: superadiabatic

  ! VARIABLES FOR CHABOUREAU-BECHTOLD
   REAL :: sigq,xl,tlk,qsat_tl,rsl,cpm,a,qmq,mf_cf,diffqt

   ! WA TEST 11/9/15 for consistent reduction of updraft params
   REAL :: csigma,flt2,flq2,fltv2,acfac

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
  ENT=0.

  !taper off MF scheme when significant resolved-scale motions are present
  !This function needs to be asymetric...
  k      = 1
  maxw   = 0.0
  DO WHILE (ZW(k) < pblh + 500.)
     maxw = MAX(maxw,ABS(W(k)))
     !JOE-find highest k-level below 50m AGL                                                                                    
     IF(ZW(k)<=50.)k50=k
     k = k+1
  ENDDO
  !print*," maxw before manipulation=", maxw
  maxw = MAX(0.,maxw - 0.5)         ! do nothing for small w, but
  Psig_w = MAX(0.0, 1.0 - maxw/0.5) ! linearly taper off for w > 0.5 m/s
  Psig_w = MIN(Psig_w, Psig_shcu)
  !print*," maxw=", maxw," Psig_w=",Psig_w," Psig_shcu=",Psig_shcu

  flt2 = flt !*0.0 !reserve some for ED or set to zero to remove excess
  flq2 = flq !*0.0 !reserve some for ED or set to zero to remove excess
  fltv = flt + svp1*flq
  fltv2 = flt2 + svp1*flq2
  !PRINT*," fltv=",fltv," zi=",pblh 

  !Completely shut off MF scheme for strong resolved-scale vertical velocities.
  IF(Psig_w == 0.0 .and. fltv > 0.0) fltv = -1.*fltv 

! if surface buoyancy is positive we do integration otherwise not, and make sure that 
! PBLH > twice the height of the surface layer (set at z0 = 50m)
! Also, ensure that it is at least slightly superadiabatic up through 50 m
  superadiabatic = .false.
  DO k=1,MAX(1,k50-1)
    IF (th(k+1)-th(k) < 0.) THEN
       superadiabatic = .true.
    ELSE
       superadiabatic = .false.
       exit
    ENDIF
  ENDDO

  IF ( fltv > 0.002 .AND. PBLH > 100. .AND. superadiabatic) then
    !PRINT*," Conditions met to run mass-flux scheme",fltv,pblh
    ! get entrainment coefficient
    ! get dz/L0
    ENTf(kts:kte,1:Nup)=0.1
    ENTi(kts:kte,1:Nup)=0.1
    do i=1,Nup
      do k=kts+1,kte
        ENTf(k,i)=(ZW(k)-ZW(k-1))/L0    ! input into Poisson
        ENTf(k,i)=MIN(ENTf(k,i),9.9) !JOE: test avoiding FPE
        ENTf(k,i)=MAX(ENTf(k,i),0.05) !JOE: test avoiding FPE
      enddo
    enddo
    ! get Poisson P(dz/L0)
    call Poisson(1,Nup,kts+1,kte,ENTf,ENTi)
    ! entrainent: Ent=Ent0/dz*P(dz/L0)             
    do i=1,Nup   
      do k=kts+1,kte
        ENT(k,i)=real(ENTi(k,i))*Ent0/(ZW(k)-ZW(k-1))  !eq (13) in Suselj et al. (2013, jas)
        ! WA TEST 11/12/15 Add some small deterministic background entrainment
        ENT(k,i) = max(ENT(k,i),0.0002)
        ! JOE - increase updraft entrainment near surface
        ENT(k,i) = max(ENT(k,i),0.3/ZW(k))
      enddo
    enddo

!    print *,'Entrainment:',ENT   
       
    ! set initial conditions for updrafts
    z0=50.
    pwmin=0.5
    pwmax=2.0 !1.5  ! was 3.0

    wstar=max(1.E-2,(g/thv(1)*fltv*pblh)**(1./3.))
    qstar=flq2/wstar
    thstar=flt2/wstar

    IF((landsea-1.5).GE.0)THEN
       csigma = 1.34   ! WATER
    ELSE
       csigma = 1.34   ! LAND
    ENDIF
    sigmaW =1.34*wstar*(z0/pblh)**(1./3.)*(1 - 0.8*z0/pblh)
    sigmaQT=csigma*qstar*(z0/pblh)**(-1./3.)
    sigmaTH=csigma*thstar*(z0/pblh)**(-1./3.)

    wmin=sigmaW*pwmin
    wmax=sigmaW*pwmax

    !SPECIFY SURFACE UPDRAFT PROPERTIES
    DO I=1,NUP
       wlv=wmin+(wmax-wmin)/NUP*(i-1)
       wtv=wmin+(wmax-wmin)/NUP*i

       !SURFACE UPDRAFT VERTICAL VELOCITY
       UPW(1,I)=0.5*(wlv+wtv)
       !IF (UPW(1,I) > 0.5*ZW(2)/dt) UPW(1,I) = 0.5*ZW(2)/dt

       !SURFACE UPDRAFT AREA
       !UPA(1,I)=0.5*ERF(wtv/(sqrt(2.)*sigmaW)) - 0.5*ERF(wlv/(sqrt(2.)*sigmaW))
       !UPA(1,I)=0.4*ERF(wtv/(sqrt(2.)*sigmaW)) - 0.4*ERF(wlv/(sqrt(2.)*sigmaW))  !19.6
       !UPA(1,I)=0.3*ERF(wtv/(sqrt(2.)*sigmaW)) - 0.3*ERF(wlv/(sqrt(2.)*sigmaW))  !14.6
       !UPA(1,I)=0.25*ERF(wtv/(sqrt(2.)*sigmaW)) - 0.25*ERF(wlv/(sqrt(2.)*sigmaW))  !12.0
       UPA(1,I)=0.2*ERF(wtv/(sqrt(2.)*sigmaW)) - 0.2*ERF(wlv/(sqrt(2.)*sigmaW))   !9.6
       !Make updraft area a function of the buoyancy flux
       acfac = .5*tanh((fltv - 0.05)/0.2) + .5
       UPA(1,I)=UPA(1,I)*acfac

       UPU(1,I)=U(1)
       UPV(1,I)=V(1)
       UPQC(1,I)=0
       !UPQT(1,I) =QT(1) +0.58*UPW(1,I)*sigmaQT/sigmaW       
       !UPTHV(1,I)=THV(1)+0.58*UPW(1,I)*sigmaTH/sigmaW
       !Alternatively, initialize parcel over lowest 50m
       UPQT(1,I) = 0.
       UPTHL(1,I)= 0.
       k50=1 !for now, keep at lowest model layer...
       DO k=1,k50
         UPQT(1,I) = UPQT(1,I) +QT(k) +0.58*UPW(1,I)*sigmaQT/sigmaW !*EXP(-ZW(k)/100.)
         UPTHV(1,I)= UPTHV(1,I)+THV(k)+0.58*UPW(1,I)*sigmaTH/sigmaW !*EXP(-ZW(k)/100.)
         !UPQT(1,I) = UPQT(1,I) +QT(k) +1.*UPW(1,I)*sigmaQT/sigmaW
         !UPTHV(1,I)= UPTHV(1,I)+THV(k)+1.*UPW(1,I)*sigmaTH/sigmaW
       ENDDO
       UPQT(1,I) = UPQT(1,I)/REAL(k50)
       UPTHV(1,I)= UPTHV(1,I)/REAL(k50)
       UPTHL(1,I)= UPTHV(1,I)/(1.+svp1*UPQT(1,I))
       UPQKE(1,I)= QKE(1)
#if (WRF_CHEM == 1)
       do ic = 1,nchem
          UPCHEM(1,I,ic)= CHEM(1,ic)
       enddo
#endif

!       !DEBUG
!       IF (UPA(1,I)<0. .OR. UPA(1,I)>0.5 .OR. wstar<0. .OR. wstar>4.0 .OR. &
!           ABS(thstar)> 5. .OR. sigmaW>1.5) THEN
!          PRINT*,"IN Mass-Flux: UPA(1,i)=",UPA(1,i)
!          PRINT*," wstar=",wstar," qstar=",qstar
!          PRINT*," thstar=",thstar," sigmaW=",sigmaW
!       ENDIF
    ENDDO

  !QCn = 0.
  ! do integration  updraft
    DO I=1,NUP
       QCn = 0.
       DO k=KTS+1,KTE

          !JOE - increase background entrainment within clouds
          !if(QCn > 1.e-8)ENT(k,i) = max(ENT(k,i),0.0004)
          !JOE - use constant ent in dry plumes in the PBL
          IF(QCn < 1.e-8 .AND. ZW(k) < pblh) ENT(k,i) = max(0.0009,0.3/ZW(k))
          !JOE - add some w-dependency for entrainment
          IF(UPW(K-1,I)>0.01) ENT(k,i) = max(ENT(k,i), 0.23/(UPW(K-1,I)*230.))
          !JOE - increase entrainment for plumes extending too high.
          IF(ZW(k) >= pblh+2000.) ENT(k,i) =ENT(k,i) + (ZW(k)-(pblh+2000.)) * 2.0E-6

          EntExp=exp(-ENT(K,I)*(ZW(k)-ZW(k-1)))

          QTn =QT(K) *(1-EntExp)+UPQT(K-1,I)*EntExp
          THLn=THL(K)*(1-EntExp)+UPTHL(K-1,I)*EntExp
          Un  =U(K)  *(1-EntExp)+UPU(K-1,I)*EntExp
          Vn  =V(K)  *(1-EntExp)+UPV(K-1,I)*EntExp
          QKEn=QKE(k)*(1-EntExp)+UPQKE(K-1,I)*EntExp

#if (WRF_CHEM == 1)
          do ic = 1,nchem
             chemn(ic) = chem(k,ic)*(1-EntExp)+UPCHEM(K-1,I,ic)*EntExp
         enddo
#endif

          ! get thvn,qcn
          call condensation_edmf(QTn,THLn,(P(K)+P(K-1))/2.,ZW(k),THVn,QCn)

          B=g*(0.5*(THVn+UPTHV(k-1,I))/THV(k-1) - 1.0)

          EntW=exp(-2.*(Wb+Wc*ENT(K,I))*(ZW(k)-ZW(k-1)))
          Wn2=UPW(K-1,I)**2*EntW + (1.-EntW)*0.5*Wa*B/(Wb+Wc*ENT(K,I))

          Wn2=Wn2*EXP(-MAX(ZW(k)-(pblh+3000.),0.0)/1000.)
          IF(ZW(k) >= pblh+4000.)Wn2=0.

          IF (Wn2 > 0.) THEN
             UPW(K,I)=sqrt(Wn2)
             !IF (UPW(K,I) > 0.5*(ZW(K)-ZW(K-1))/dt) UPW(K,I) = 0.5*(ZW(K)-ZW(K-1))/dt
             UPTHV(K,I)=THVn
             UPTHL(K,I)=THLn
             UPQT(K,I)=QTn
             UPQC(K,I)=QCn
             UPU(K,I)=Un
             UPV(K,I)=Vn
             UPQKE(K,I)=QKEn
             UPA(K,I)=UPA(K-1,I)
#if (WRF_CHEM == 1)
             do ic = 1,nchem
                UPCHEM(k,ic,I) = chemn(ic)
             enddo
#endif
          ELSE
             exit
          END IF
       ENDDO
    ENDDO  

  END IF 
!        
!  get updraft properties, for saving
!
  edmf_a  =0.
  edmf_w  =0.
  edmf_qt =0.
  edmf_thl=0.
  edmf_ent=0.
  edmf_qc =0.
#if (WRF_CHEM == 1)
  edmf_chem(kts:kte+1,1:nchem) = 0.0
#endif

  ! writing updraft properties in their variable
  ! all variables, except Areas are now multipled by the area  
  ! to confirm with WRF grid setup we do not save the first and the last row   

  DO k=KTS,KTE-1
    DO I=1,NUP
       edmf_a(K)=edmf_a(K)+UPA(K+1,I)      !should it really be K+1????
       edmf_w(K)=edmf_w(K)+UPA(K+1,I)*UPW(K+1,I)
       edmf_qt(K)=edmf_qt(K)+UPA(K+1,I)*UPQT(K+1,I)
       edmf_thl(K)=edmf_thl(K)+UPA(K+1,I)*UPTHL(K+1,I)
       edmf_ent(K)=edmf_ent(K)+UPA(K+1,I)*ENT(K+1,I)
       edmf_qc(K)=edmf_qc(K)+UPA(K+1,I)*UPQC(K+1,I)
#if (WRF_CHEM == 1)
       do ic = 1,nchem
          edmf_chem(k,ic) = edmf_chem(k,ic) + UPA(K+1,I)*UPCHEM(k,ic,I)
       enddo
#endif
    ENDDO 

    IF (edmf_a(k)>0.) THEN
       edmf_w(k)=edmf_w(k)/edmf_a(k)
       edmf_qt(k)=edmf_qt(k)/edmf_a(k)
       edmf_thl(k)=edmf_thl(k)/edmf_a(k)
       edmf_ent(k)=edmf_ent(k)/edmf_a(k)
       edmf_qc(k)=edmf_qc(k)/edmf_a(k)
#if (WRF_CHEM == 1)
       do ic = 1,nchem
          edmf_chem(k,ic) = edmf_chem(k,ic)/edmf_a(k)
       enddo
#endif
       edmf_a(k)=edmf_a(k)*Psig_w
    ENDIF
  ENDDO

  !
  ! computing variables needed for implicit solver
  !
  s_aw=0.
  s_awthl=0.
  s_awqt=0.
  s_awqv=0.
  s_awqc=0.
  s_awu=0.
  s_awv=0.
  s_awqke=0.
#if (WRF_CHEM == 1)
  s_awchem(kts:kte+1,1:nchem) = 0.0
#endif

  DO k=KTS,KTE
     DO I=1,NUP
       s_aw(k)   = s_aw(K)    + UPA(K,I)*UPW(K,I)*Psig_w 
       s_awthl(k)= s_awthl(K) + UPA(K,i)*UPW(K,I)*UPTHL(K,I)*Psig_w
       s_awqt(k) = s_awqt(K)  + UPA(K,i)*UPW(K,I)*UPQT(K,I)*Psig_w
       s_awqc(k) = s_awqc(K)  + UPA(K,i)*UPW(K,I)*UPQC(K,I)*Psig_w
       IF (momentum_opt > 0) THEN
         s_awu(k)  = s_awu(K)   + UPA(K,i)*UPW(K,I)*UPU(K,I)*Psig_w
         s_awv(k)  = s_awv(K)   + UPA(K,i)*UPW(K,I)*UPV(K,I)*Psig_w
       ENDIF
       IF (tke_opt > 0) THEN
         s_awqke(k)= s_awqke(K) + UPA(K,i)*UPW(K,I)*UPQKE(K,I)*Psig_w
       ENDIF
#if (WRF_CHEM == 1)
       do ic = 1,nchem
          s_awchem(k,ic) = s_awchem(k,ic) + UPA(K,i)*UPW(K,I)*UPCHEM(K,I,ic)*Psig_w 
       enddo
#endif
     ENDDO
     s_awqv(k) = s_awqt(k)  - s_awqc(k)
  ENDDO

!JOE: ADD CLDFRA_bl1d, qc_bl1d. Note that they have already been defined above.
!     Here, a shallow-cu component is added (or max is used).  
   DO K=KTS,KTE
!     qc_bl1d(k)=0.
!     cldfra_bl1d(k)=0.

      IF (cldfra_opt == 0) THEN
         IF(edmf_qc(k)>0.0)THEN
            !Chaboureau and Bechtold (2005, JGR)
            diffqt=edmf_qt(k)-qt(k)
            if(ABS(diffqt) < 1.0E-6)diffqt = 1.0E-6
            !sigq = MAX(edmf_a(k)*(1.-edmf_a(k))*diffqt**2 , 1.0e-8)
            sigq = MAX(5.*edmf_a(k)*diffqt**2 , 1.0e-8)
            sigq = sqrt(sigq)

            xl = xl_blend(tk(k))                ! obtain blended heat capacity
            tlk = thl(k)*(p(k)/p1000mb)**rcp    ! recover liquid temp (tl) from thl
            qsat_tl = qsat_blend(tlk,p(k))      ! get saturation water vapor mixing ratio
                                                !   at tl and p
            rsl = xl*qsat_tl / (r_v*tlk**2)     ! slope of C-C curve at t = tl
                                                ! CB02, Eqn. 4
            cpm = cp + qt(k)*cpv                ! CB02, sec. 2, para. 1
            a   = 1./(1. + xl*rsl/cpm)          ! CB02 variable "a"
            qmq = a * (qt(k) - qsat_tl)         ! saturation deficit/excess;
                                                !   the numerator of Q1
            mf_cf = min(max(0.5 + 0.36 * atan(1.55*(qmq/sigq)),0.01),1.0)
            !IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
            !   print*,"In MYNN, StEM edmf"
            !   print*,"  CB: qt=",qt(k)," qsat=",qsat_tl," satdef=",qt(k) - qsat_tl
            !   print*,"  CB: sigq=",sigq," qmq=",qmq," tlk=",tlk
            !   print*,"  CB: mf_cf=",mf_cf," cldfra_bl=",cldfra_bl1d(k)," edmf_a=",edmf_a(k)
            !ENDIF
            IF (mf_cf > edmf_a(k)) THEN
               cldfra_bl1d(k) = mf_cf
               qc_bl1d(k) = edmf_qc(k)*edmf_a(k)/mf_cf
            ELSE
               cldfra_bl1d(k)=edmf_a(k) !MAX(mf_cf,edmf_a(k))
               qc_bl1d(k) = edmf_qc(k)  !MAX(qc_bl1d(k), edmf_qc(k))
            ENDIF
         ENDIF
      ELSEIF(cldfra_opt == 1) THEN
         !Randall and Xu
         qc_bl1d(k) = MAX(qc_bl1d(k), edmf_qc(k))
         if(F_qc .and. .not. F_qi)then
           satvp = 3.80*exp(17.27*(th(k)-273.)/ &
                   (th(k)-36.))/(.01*p(k))
           rhgrid = max(.1,MIN( .95, qv(k) /satvp))
           h2oliq=1000.*qc_bl1d(k)
           satvp=1000.*satvp
           cldfra_bl1d(k)=(1.-exp(-coef_alph*h2oliq/&
                   ((1.-rhgrid)*satvp)**coef_gamm))*(rhgrid**coef_p)
           cldfra_bl1d(k)=max(0.0,MIN(1.,cldfra_bl1d(k)))
         elseif(F_qc .and. F_qi)then
           satvp = 3.80*exp(17.27*(th(k)-273.)/ &
                   (th(k)-36.))/(.01*p(k))
           rhgrid = max(.1,MIN( .95, qv(k) /satvp))
           h2oliq=1000.*qc_bl1d(k)
           satvp=1000.*satvp
           cldfra_bl1d(k)=(1.-exp(-coef_alph*h2oliq/&
                    ((1.-rhgrid)*satvp)**coef_gamm))*(rhgrid**coef_p)
           cldfra_bl1d(k)=max(0.0,MIN(1.,cldfra_bl1d(k)))
         endif
      ENDIF
   ENDDO

       
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
! DO I=1,NUP   
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

! initialization of deltas
!  DO k=kts,kte 
!    dth(k)=0.
!    dqv(k)=0.
!    dqc(k)=0.
!    du(k)=0.
!    dv(k)=0.
!  ENDDO        

END SUBROUTINE StEM_MF
!=================================================================
subroutine Poisson(istart,iend,jstart,jend,mu,POI)

  integer, intent(in) :: istart,iend,jstart,jend 
  real,dimension(istart:iend,jstart:jend),intent(in) :: MU
  integer, dimension(istart:iend,jstart:jend), intent(out) :: POI
  integer :: i,j
  !
  ! do this only once
  ! call init_random_seed

  do i=istart,iend
    do j=jstart,jend
      call   random_Poisson(mu(i,j),.true.,POI(i,j))
    enddo
  enddo

end subroutine Poisson
!=================================================================  
subroutine init_random_seed()
   !JOE: PGI had problem! use iso_fortran_env, only: int64
   !JOE: PGI had problem! use ifport, only: getpid 
   implicit none
   integer, allocatable :: seed(:)
   integer :: i, n, un, istat, dt(8), pid
   !JOE: PGI had problem! integer(int64) :: t
   integer :: t

   call random_seed(size = n)
   allocate(seed(n))

   ! First try if the OS provides a random number generator
   !JOE: PGI had problem! open(newunit=un, file="/dev/urandom", access="stream", &
   un=191
   open(unit=un, file="/dev/urandom", access="stream", &
   form="unformatted", action="read", status="old", iostat=istat)

   if (istat == 0) then
      read(un) seed
      close(un)
   else
      ! Fallback to XOR:ing the current time and pid. The PID is
      ! useful in case one launches multiple instances of the same
      ! program in parallel.
      call system_clock(t)
      if (t == 0) then
         call date_and_time(values=dt)
         !t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
         !   + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
         !   + dt(3) * 24_int64 * 60 * 60 * 1000 &
         !   + dt(5) * 60 * 60 * 1000 &
         !   + dt(6) * 60 * 1000 + dt(7) * 1000 &
         !   + dt(8)
         t = dt(6) * 60 &  ! only return seconds for smaller t
           + dt(7)
      end if

      !JOE: PGI had problem!pid = getpid()
      ! for distributed memory jobs we need to fix this
      !pid=1
      pid = 666 + MOD(t,10)  !JOE: doesn't work for PG compilers: getpid()
 
      t = ieor(t, int(pid, kind(t)))
      do i = 1, n
         seed(i) = lcg(t)
      end do
   end if
   call random_seed(put=seed)

  contains

  ! Pseudo-random number generator (PRNG) 
  ! This simple PRNG might not be good enough for real work, but is
  ! sufficient for seeding a better PRNG.
  function lcg(s)

   integer :: lcg
   !JOE: PGI had problem! integer(int64) :: s
   integer :: s

   if (s == 0) then
      !s = 104729
      s = 1047
   else
      !s = mod(s, 4294967296_int64)
      s = mod(s, 71)
   end if
   !s = mod(s * 279470273_int64, 4294967291_int64)
   s = mod(s * 23, 17)
   !lcg = int(mod(s, int(huge(0), int64)), kind(0))
   lcg = int(mod(s, int(s/3.5)))

  end function lcg

  end subroutine init_random_seed


subroutine random_Poisson(mu,first,ival) 
!**********************************************************************
!     Translated to Fortran 90 by Alan Miller from:  RANLIB
!
!     Library of Fortran Routines for Random Number Generation
!
!                    Compiled and Written by:
!
!                         Barry W. Brown
!                          James Lovato
!
!             Department of Biomathematics, Box 237
!             The University of Texas, M.D. Anderson Cancer Center
!             1515 Holcombe Boulevard
!             Houston, TX      77030
!
! Generates a single random deviate from a Poisson distribution with mean mu.
! Scalar Arguments:
	REAL, INTENT(IN)    :: mu  !The mean of the Poisson distribution from which
                                   !a random deviate is to be generated.
	LOGICAL, INTENT(IN) :: first
        INTEGER             :: ival

!     TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
!     COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL
!     SEPARATION OF CASES A AND B
!
!     .. Local Scalars ..
!JOE: since many of these scalars conflict with globally declared closure constants (above),
!     need to change XX to XX_s
!	REAL          :: b1, b2, c, c0, c1, c2, c3, del, difmuk, e, fk, fx, fy, g,  &
!                    omega, px, py, t, u, v, x, xx
	REAL          :: b1_s, b2_s, c, c0, c1_s, c2_s, c3_s, del, difmuk, e, fk, fx, fy, g_s,  &
                    omega, px, py, t, u, v, x, xx
	REAL, SAVE    :: s, d, p, q, p0
        INTEGER       :: j, k, kflag
	LOGICAL, SAVE :: full_init
        INTEGER, SAVE :: l, m
!     ..
!     .. Local Arrays ..
	REAL, SAVE    :: pp(35)
!     ..
!     .. Data statements ..
!JOE: since many of these scalars conflict with globally declared closure constants (above),
!     need to change XX to XX_s
!	REAL, PARAMETER :: a0 = -.5, a1 = .3333333, a2 = -.2500068, a3 = .2000118,  &
	REAL, PARAMETER :: a0 = -.5, a1_s = .3333333, a2_s = -.2500068, a3 = .2000118,  &
                           a4 = -.1661269, a5 = .1421878, a6 = -0.1384794,  &
                           a7 = .1250060

	REAL, PARAMETER :: fact(10) = (/ 1., 1., 2., 6., 24., 120., 720., 5040.,  &
                 40320., 362880. /)

!JOE: difmuk,fk,u errors - undefined
   difmuk = 0.
   fk = 1.0
   u = 0.

!     ..
!     .. Executable Statements ..
   IF (mu > 10.0) THEN
!     C A S E  A. (RECALCULATION OF S, D, L IF MU HAS CHANGED)

      IF (first) THEN
         s = SQRT(mu)
         d = 6.0*mu*mu

!             THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL
!             PROBABILITIES FK WHENEVER K >= M(MU). L=IFIX(MU-1.1484)
!             IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .

         l = mu - 1.1484
         full_init = .false.
      END IF

!     STEP N. NORMAL SAMPLE - random_normal() FOR STANDARD NORMAL DEVIATE
      g_s = mu + s*random_normal()
      IF (g_s > 0.0) THEN
         ival = g_s

 	 !     STEP I. IMMEDIATE ACCEPTANCE IF ival IS LARGE ENOUGH
         IF (ival>=l) RETURN

	 !     STEP S. SQUEEZE ACCEPTANCE - SAMPLE U
		fk = ival
		difmuk = mu - fk
		CALL RANDOM_NUMBER(u)
		IF (d*u >= difmuk*difmuk*difmuk) RETURN
      END IF

      !     STEP P. PREPARATIONS FOR STEPS Q AND H.
      !             (RECALCULATIONS OF PARAMETERS IF NECESSARY)
      !             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7.
      !             THE QUANTITIES B1_S, B2_S, C3_S, C2_S, C1_S, C0 ARE FOR THE HERMITE
      !             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.
      !             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION.

      IF (.NOT. full_init) THEN
         omega = .3989423/s
		b1_s = .4166667E-1/mu
		b2_s = .3*b1_s*b1_s
		c3_s = .1428571*b1_s*b2_s
		c2_s = b2_s - 15.*c3_s
		c1_s = b1_s - 6.*b2_s + 45.*c3_s
		c0 = 1. - b1_s + 3.*b2_s - 15.*c3_s
		c = .1069/mu
		full_init = .true.
      END IF

	  IF (g_s < 0.0) GO TO 50

	!             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)

	  kflag = 0
	  GO TO 70

	!     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)

	  40 IF (fy-u*fy <= py*EXP(px-fx)) RETURN

	!     STEP E. EXPONENTIAL SAMPLE - random_exponential() FOR STANDARD EXPONENTIAL
	!             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'
	!             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)

	  50 e = random_exponential()
	  CALL RANDOM_NUMBER(u)
	  u = u + u - one
	  t = 1.8 + SIGN(e, u)
	  IF (t <= (-.6744)) GO TO 50
	  ival = mu + s*t
	  fk = ival
	  difmuk = mu - fk

	!             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)

	  kflag = 1
	  GO TO 70

	!     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)

	  60 IF (c*ABS(u) > py*EXP(px+e) - fy*EXP(fx+e)) GO TO 50
	  RETURN

	!     STEP F. 'SUBROUTINE' F. CALCULATION OF PX, PY, FX, FY.
	!             CASE ival < 10 USES FACTORIALS FROM TABLE FACT

	  70 IF (ival>=10) GO TO 80
	  px = -mu
!JOE: had error " Subscript #1 of FACT has value -858993459"; shouldn't be < 1.
         !py = mu**ival/fact(ival+1)
	  py = mu**ival/fact(MAX(ival+1,1))
	  GO TO 110

	!             CASE ival >= 10 USES POLYNOMIAL APPROXIMATION
	!             A0-A7 FOR ACCURACY WHEN ADVISABLE
	!             .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)

	  80 del = .8333333E-1/fk
	  del = del - 4.8*del*del*del
	  v = difmuk/fk
	  IF (ABS(v)>0.25) THEN
		px = fk*LOG(one + v) - difmuk - del
	  ELSE
		px = fk*v*v* (((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2_s)*v+a1_s)*v+a0) - del
	  END IF
	  py = .3989423/SQRT(fk)
	  110 x = (half - difmuk)/s
	  xx = x*x
	  fx = -half*xx
	  fy = omega* (((c3_s*xx + c2_s)*xx + c1_s)*xx + c0)
	  IF (kflag <= 0) GO TO 40
	  GO TO 60

	!---------------------------------------------------------------------------
	!     C A S E  B.    mu < 10
	!     START NEW TABLE AND CALCULATE P0 IF NECESSARY
      ELSE

	  IF (first) THEN
		m = MAX(1, INT(mu))
		l = 0
                !print*,"mu=",mu
                !print*," mu=",mu," p=",EXP(-mu)
		p = EXP(-mu)
		q = p
		p0 = p
	  END IF

	!     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD

	  DO
		CALL RANDOM_NUMBER(u)
		ival = 0
		IF (u <= p0) RETURN

	!     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE
	!             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES
	!             (0.458=PP(9) FOR MU=10)

		IF (l == 0) GO TO 150
		j = 1
		IF (u > 0.458) j = MIN(l, m)
		DO k = j, l
		  IF (u <= pp(k)) GO TO 180
		END DO
		IF (l == 35) CYCLE

	!     STEP C. CREATION OF NEW POISSON PROBABILITIES P
	!             AND THEIR CUMULATIVES Q=PP(K)

		150 l = l + 1
		DO k = l, 35
		  p = p*mu / k
		  q = q + p
		  pp(k) = q
		  IF (u <= q) GO TO 170
		END DO
		l = 35
	  END DO

	  170 l = k
	  180 ival = k
	  RETURN
	END IF

	RETURN
	END subroutine random_Poisson

!==================================================================

	FUNCTION random_normal() RESULT(fn_val)

	! Adapted from the following Fortran 77 code
	!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
	!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
	!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

	!  The function random_normal() returns a normally distributed pseudo-random
	!  number with zero mean and unit variance.

	!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
	!  and J.F. Monahan augmented with quadratic bounding curves.

	REAL :: fn_val

	!     Local variables
	REAL     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,           &
				r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

	!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

	DO
	  CALL RANDOM_NUMBER(u)
	  CALL RANDOM_NUMBER(v)
	  v = 1.7156 * (v - half)

	!     Evaluate the quadratic form
	  x = u - s
	  y = ABS(v) - t
	  q = x**2 + y*(a*y - b*x)

	!     Accept P if inside inner ellipse
	  IF (q < r1) EXIT
	!     Reject P if outside outer ellipse
	  IF (q > r2) CYCLE
	!     Reject P if outside acceptance region
	  IF (v**2 < -4.0*LOG(u)*u**2) EXIT
	END DO

	!     Return ratio of P's coordinates as the normal deviate
	fn_val = v/u
	RETURN

	END FUNCTION random_normal

!===============================================================

	FUNCTION random_exponential() RESULT(fn_val)

	! Adapted from Fortran 77 code from the book:
	!     Dagpunar, J. 'Principles of random variate generation'
	!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

	! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
	! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
	! TO EXP(-random_exponential), USING INVERSION.

	REAL  :: fn_val

	!     Local variable
	REAL  :: r

	DO
	  CALL RANDOM_NUMBER(r)
	  IF (r > zero) EXIT
	END DO

	fn_val = -LOG(r)
	RETURN

	END FUNCTION random_exponential

!===============================================================

subroutine condensation_edmf(QT,THL,P,zagl,THV,QC)
!
! zero or one condensation for edmf: calculates THV and QC
!
real,intent(in) :: QT,THL,P,zagl
real,intent(out):: THV,QC

integer :: niter,i
real :: diff,exn,t,qs,qcold

! constants used from module_model_constants.F
! p1000mb
! rcp ... Rd/cp
! xlv ... latent heat for water (2.5e6)
! cp
! rvord .. rv/rd  (1.6) 


! number of iterations
  niter=50
! minimum difference
  diff=1.e-4

  EXN=(P/p1000mb)**rcp
  QC=0.

  do i=1,NITER
     T=EXN*THL+xlv/cp*QC
     QS=qsat_blend(T,P)
     QCOLD=QC
     QC=max(0.5*QC + 0.5*(QT-QS),0.)
     if (abs(QC-QCOLD)<Diff) exit
  enddo

  T=exn*THL+xlv/cp*QC
  QS=qsat_blend(T,P)
  QC=max(QT-QS,0.)

  !Do not allow saturation below 100 m
  if(zagl < 100.)QC=0.

  !THV=(THL+xlv/cp*QC).*(1+(1-rvovrd)*(QT-QC)-QC);
  THV=(THL+xlv/cp*QC)*(1.+QT*(rvovrd-1.)-rvovrd*QC)

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
    dxdh=MAX(dx,10.)/MIN(PBL1,3000.)
    ! Honnert et al. 2011, TKE in PBL  *** original form used until 201605
    !Psig_bl= ((dxdh**2) + 0.07*(dxdh**0.667))/((dxdh**2) + &
    !         (3./21.)*(dxdh**0.67) + (3./42.))
    ! Honnert et al. 2011, TKE in entrainment layer
    !Psig_bl= ((dxdh**2) + (4./21.)*(dxdh**0.667))/((dxdh**2) + &
     !        (3./20.)*(dxdh**0.67) + (7./21.))
    ! New form to preseve parameterized mixing - only down 5% at dx = 750 m
     Psig_bl= ((dxdh**2) + 0.106*(dxdh**0.667))/((dxdh**2) +0.066*(dxdh**0.667) + 0.071)

    !assume a 500 m cloud depth for shallow-cu clods
    dxdh=MAX(dx,10.)/MIN(PBL1+500.,3500.)
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

    !print*,"in shcu_gf; dx, dxdh, Psig(i)=",dx,dxdh,Psig(i)
    !If(Psig(i) < 0.000001 .OR. Psig(i) > 0.99999999)print*,"in shcu_gf;
    !dx, dxdh, Psig(i)=",dx,dxdh,Psig(i) 
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

  FUNCTION qsat_blend(t, P)
! JAYMES- this function extends function "esat" and returns a "blended"
! saturation mixing ratio.

      IMPLICIT NONE

      REAL, INTENT(IN):: t, P
      REAL :: qsat_blend,XC,ESL,ESI,RSLF,RSIF,chi

      XC=MAX(-80.,t-273.16)

      IF (t .GE. 273.16) THEN
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
! This is the mass flux part of the TEMF scheme from module_bl_temf.F,
! adapted for the MYNN context by Wayne Angevine June 2015.
! Variable strategy:  TEMF external variables that have semantically
! comfortable counterparts in the MYNN-EDMF context have been changed to
! use those names.  Otherwise the TEMF variable names have been kept but
! redefined as local variables.  Only "moist" vars are used, whether
! updraft condenses or not.  Some former local vars are replaced with
! externals.
!
! (Partial) list of conversions:
!    wupd_temfx -> moist_w
!    thup_temfx -> moist_thl
!    qtup_temfx -> moist_qt
!    qlup_temfx -> moist_qc
!    cf3d_temfx -> cldfra_bl1d
!    au -> moist_a

  SUBROUTINE temf_mf( & 
                 & kts,kte,dt,zw,p,pi1d,     &
                 & u,v,w,th,thl,thv,qt,qv,qc,&
                 & qke,ust,flt,flq,flqv,flqc,&
                 & hfx,qfx,tsk,              &
                 & pblh,rho,dfh,dx,znt,ep_2, &
            ! outputs - updraft properties
                 & edmf_a,edmf_w,edmf_qt,    &
                 & edmf_thl,edmf_ent,edmf_qc,&
!                 & dry_a,moist_a,           &
!                 & dry_w,moist_w,           &
!                 & dry_qt,moist_qt,         &
!                 & dry_thl,moist_thl,       &
!                 & dry_ent,moist_ent,       &
!                 & moist_qc,                &
            ! outputs - variables needed for solver
                 & s_aw,s_awthl,s_awqt,      &
                 & s_awqv,s_awqc,            &
                 & s_awu,s_awv,s_awqke,      &
#if (WRF_CHEM == 1)
                 & nchem,chem,s_awchem,      &
#endif
            ! in/outputs - subgrid scale clouds
                 & qc_bl1d,cldfra_bl1d,      &
            ! inputs - flags for moist arrays
                 &F_QC,F_QI,psig,            &
                 &ii,jj,ids,ide,jds,jde)

  ! inputs:
     INTEGER, INTENT(IN) :: kts,kte,ii,jj,ids,ide,jds,jde
     REAL,DIMENSION(kts:kte), INTENT(IN) :: u,v,w,th,thl,qt,qv,qc,thv,p,pi1d
     REAL,DIMENSION(kts:kte), INTENT(IN) :: qke
     REAL,DIMENSION(kts:kte+1), INTENT(IN) :: zw  !height at full-sigma
     REAL,DIMENSION(kts:kte), INTENT(IN) :: rho  !density
     REAL,DIMENSION(kts:kte), INTENT(IN) :: dfh  !diffusivity for heat
     REAL, INTENT(IN) :: dt,ust,flt,flq,flqv,flqc,hfx,qfx,tsk,pblh,dx,znt,ep_2,psig
     LOGICAL, OPTIONAL :: f_qc,f_qi

  ! outputs - updraft properties
     REAL,DIMENSION(kts:kte), INTENT(OUT) :: &
                 & edmf_a,edmf_w,edmf_qt,    &
                 & edmf_thl,edmf_ent,edmf_qc

  ! outputs - variables needed for solver
     REAL,DIMENSION(kts:kte+1) :: s_aw,      & !sum ai*wis_awphi
                               s_awthl,      & !sum ai*wi*phii
                                s_awqt,      &
                                s_awqv,      &
                                s_awqc,      &
                                 s_awu,      &
                                 s_awv,      &
                                 s_awqke
#if (WRF_CHEM == 1)
     INTEGER, INTENT(IN) :: nchem
     REAL,DIMENSION(kts:kte+1, nchem) :: chem
     REAL,DIMENSION(kts:kte+1, nchem) :: s_awchem
     INTEGER :: ic
#endif

     REAL,DIMENSION(kts:kte), INTENT(INOUT) :: qc_bl1d,cldfra_bl1d

! Local variables
!
! EDMF constants
   real, parameter :: CM = 0.03      ! Proportionality constant for subcloud MF
   real, parameter :: Cdelt = 0.006  ! Prefactor for detrainment rate
   real, parameter :: Cw = 0.5       ! Prefactor for surface wUPD
   real, parameter :: Cc = 3.0       ! Prefactor for convective length scale
   real, parameter :: lasymp = 200.0 ! Asymptotic length scale WA 11/20/09
   real, parameter :: hmax = 4000.0  ! Max hd,hct WA 11/20/09
   integer, parameter :: Nupd = 8    ! Number of updrafts
!
   integer :: i, k, kt, nu   ! Loop variable
   integer::  h0idx
   real::  h0
   real::  wstr, ang, wm
   real, dimension( Nupd) ::  hd,lcl,hct,ht
   real::  convection_TKE_surface_src, sfcFTE
   real::  sfcTHVF
   real::  z0t
   integer, dimension( Nupd) ::  hdidx,lclidx,hctidx,htidx
   integer::  hmax_idx
   integer::  tval
   real, dimension( kts:kte) :: zm, zt, dzm, dzt
   real, dimension( kts:kte) :: thetal, qtot
   real, dimension( kts:kte) :: u_temf, v_temf
   real, dimension( kts:kte) :: rv, rl, rt
   real, dimension( kts:kte) :: chi_poisson, gam
   real, dimension( kts:kte) :: dthdz
   real, dimension( kts:kte) :: lepsmin
   real, dimension( kts:kte) :: thetav
   real, dimension( kts:kte) :: dmoist_qtdz
   real, dimension( kts:kte) :: B, Bmoist
   real, dimension( kts:kte, Nupd) :: epsmf, deltmf, dMdz
   real, dimension( kts:kte, Nupd) :: UUPD, VUPD
   real, dimension( kts:kte, Nupd) :: thetavUPD, qlUPD, TEUPD
   real, dimension( kts:kte, Nupd) :: thetavUPDmoist, wUPD_dry
   real, dimension( kts:kte, Nupd) :: dthUPDdz, dwUPDdz
   real, dimension( kts:kte, Nupd) :: dwUPDmoistdz
   real, dimension( kts:kte, Nupd) :: dUUPDdz, dVUPDdz, dTEUPDdz
   real, dimension( kts:kte, Nupd) :: TUPD, rstUPD, rUPD, rlUPD, qstUPD
   real, dimension( kts:kte, Nupd) :: MUPD, wUPD, qtUPD, thlUPD, qcUPD
   real, dimension( kts:kte, Nupd) :: aUPD, cldfraUPD
   real, dimension( kts:kte) :: N2, S, Ri, beta, ftau, fth, ratio
   real, dimension( kts:kte) :: TKE, TE2
   real, dimension( kts:kte) :: ustrtilde, linv, leps
   real, dimension( kts:kte) :: km, kh
   real, dimension( kts:kte) :: Fz, QFK, uwk, vwk
   real, dimension( kts:kte) :: km_conv, kh_conv, lconv
   real, dimension( kts:kte) :: alpha2, beta2   ! For thetav flux calculation
   real, dimension( kts:kte) :: THVF, buoy_src, srcs
   real, dimension( kts:kte) :: beta1 ! For saturation humidity calculations
   real, dimension( kts:kte) :: MFCth
   real Cepsmf    ! Prefactor for entrainment rate
   real red_fact  ! for reducing MF components
   real, dimension( kts:kte) :: edmf_u, edmf_v, edmf_qke ! Same format as registry vars, but not passed out
   integer:: bdy_dist,taper_dist
   real:: taper

#if (WRF_CHEM == 1)
   real,dimension( kts:kte+1, nchem, Nupd) :: chemUPD, dchemUPDdz
   real,dimension( kts:kte+1, nchem)       :: edmf_chem
#endif

   ! Used to be TEMF external variables, now local
   real, dimension( kts:kte, Nupd) :: &
             shf_temfx, qf_temfx, uw_temfx, vw_temfx , &
             mf_temfx
   real, dimension( Nupd) :: hd_temfx, lcl_temfx, hct_temfx, cfm_temfx
   logical is_convective
   ! Vars for cloud fraction calculation
   real, dimension( kts:kte) :: sigq, qst, satdef
   real :: sigq2, rst, cldfra_sum, psig_w, maxw

!----------------------------------------------------------------------
! Grid staggering:  Matlab version has mass and turbulence levels.
! WRF has full levels (with w) and half levels (u,v,theta,q*).  Both
! sets of levels use the same indices (kts:kte).  See pbl_driver or
! WRF Physics doc for (a few) details.
! So *mass levels correspond to half levels.*
! WRF full levels are ignored, we define our own turbulence levels
! in order to put the first one below the first half level.
! Another difference is that
! the Matlab version (and the Mauritsen et al. paper) consider the
! first mass level to be at z0 (effectively the surface).  WRF considers
! the first half level to be above the effective surface.  The first half
! level, at k=1, has nonzero values of u,v for example.  Here we convert
! all incoming variables to internal ones with the correct indexing
! in order to make the code consistent with the Matlab version.  We
! already had to do this for thetal and qt anyway, so the only additional
! overhead is for u and v.
! I use suffixes m for mass and t for turbulence as in Matlab for things
! like indices.
! Note that zsrf is the terrain height ASL, from Registry variable ht.
! Translations (Matlab to WRF):
!     dzt -> calculated below
!     dzm -> not supplied, calculated below
!     k -> karman
!     z0 -> znt
!     z0t -> not in WRF, calculated below
!     zt -> calculated below
!     zm -> zw but NOTE zm(1) is now z0 (znt) and zm(2) is zw(1)
!
! Other notes:
! - I have often used 1 instead of kts below, because the scheme demands
!   to know where the surface is.  It won't work if kts .NE. 1.

      IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
           WRITE ( mynn_message , FMT='(A)' ) &
           ' MYNN; in TEMF_MF, beginning'
           CALL wrf_debug ( 0 , mynn_message )
      ENDIF

      !JOE-initialize s_aw* variables
      s_aw   = 0.
      s_awthl= 0.
      s_awqt = 0.
      s_awqv = 0.
      s_awqc = 0.
      s_awu  = 0.
      s_awv  = 0.
      s_awqke= 0.
      edmf_a = 0.
      edmf_w = 0.
      edmf_qt= 0. !qt
      edmf_thl=0. !thl
      edmf_ent=0.
      edmf_qc= 0. !qc
      edmf_u=0.
      edmf_v=0.
      edmf_qke=0.
 
      z0t = znt

      do k = kts,kte
         rv(k) = qv(k) / (1.-qv(k))   ! Water vapor
         rl(k) = qc(k) / (1.-qc(k))   ! Liquid water
         rt(k) = qt(k)                ! Total water (without ice)
         thetal(k) = thl(k)
         qtot(k) = qt(k)
         thetav(k) = thv(k)
      end do

      do k = kts,kte
         u_temf(k) = u(k)
         v_temf(k) = v(k)
      end do

      !taper off MF scheme when significant resolved-scale motions are present
      !This function needs to be asymetric...
      k      = 1
      maxw   = 0.0
      DO WHILE (ZW(k) < pblh + 500.)
         maxw = MAX(maxw,ABS(W(k)))
         k = k+1
      ENDDO
      maxw = MAX(0.,maxw - 0.5)         ! do nothing for small w, but
      Psig_w = MAX(0.0, 1.0 - maxw/0.5) ! linearly taper off for w > 0.5 m/s
      Psig_w = MIN(Psig_w, Psig)
      !print*," maxw=", maxw," Psig_w=",Psig_w," Psig_shcu=",Psig_shcu

      ! Get delta height at half (mass) levels
      zm(1) = znt
      dzt(1) = zw(2) - zm(1)
      ! Get height and delta at turbulence levels
      zt(1) = (zw(2) - znt) / 2.
      do kt = kts+1,kte
         zm(kt) = zw(kt) ! Convert indexing from WRF to TEMF
         zt(kt) = (zm(kt) + zw(kt+1)) / 2.
         dzm(kt) = zt(kt) - zt(kt-1)
         dzt(kt) = zw(kt+1) - zw(kt)
      end do
      dzm(1) = dzm(2)

      !print *,"In TEMF_MF zw = ", zw
      !print *,"zm = ", zm
      !print *,"zt = ", zt
      !print *,"dzm = ", dzm
      !print *,"dzt = ", dzt

      ! Gradients at first level
      dthdz(1) = (thetal(2)-thetal(1)) / (zt(1) * log10(zm(2)/z0t))

      !print *,"In TEMF_MF dthdz(1),thetal(2,1),tsk,zt(1),zm(2),z0t = ", &
      !             dthdz(1),thetal(2),thetal(1),tsk,zt(1),zm(2),z0t       

      ! Surface thetaV flux from Stull p.147
      sfcTHVF = hfx/(rho(1)*cp) * (1.+0.608*(qv(1)+qc(1))) + 0.608*thetav(1)*qfx

      ! WA use hd_temf to calculate w* instead of finding h0 here????
      ! Watch initialization!
      h0idx = 1
      h0 = zm(1)

      lepsmin(kts) = 0.

      ! WA 2/11/13 find index just above hmax for use below
      hmax_idx = kte-1

      do k = kts+1,kte-1
         lepsmin(k) = 0.

         ! Mean gradients
         dthdz(k) = (thetal(k+1) - thetal(k)) / dzt(k)

         ! Find h0 (should eventually be interpolated for smoothness)
         if (thetav(k) > thetav(1) .AND. h0idx .EQ. 1) then
         ! WA 9/28/11 limit h0 as for hd and hct
            if (zm(k) < hmax) then
               h0idx = k
               h0 = zm(k)
            else
               h0idx = k
               h0 = hmax
            end if
         end if
         ! WA 2/11/13 find index just above hmax for use below
         if (zm(k) > hmax) then
            hmax_idx = min(hmax_idx,k)
         end if
      end do

      ! Gradients at top level

      dthdz(kte) = dthdz(kte-1)

      if ( hfx > 0.) then
         wstr = (g * h0 / thetav(2) * hfx/(rho(1)*cp) ) ** (1./3.)
         bdy_dist = min( min((ii-ids),(ide-ii)) , min((jj-jds),(jde-jj)) )
         taper_dist = 5
         ! JSK - linearly taper w-star near lateral boundaries (within 5 grid columns)
         if (bdy_dist .LE. taper_dist) then
            taper = max(0., min( 1., real(bdy_dist) / real(taper_dist) ) )
            wstr  = wstr * taper
         end if
      else
         wstr = 0.
      end if

      !print *,"In TEMF_MF wstr,hfx,dthdz(1:2),h0 = ", wstr,hfx,dthdz(1),dthdz(2),h0
      IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
         WRITE ( mynn_message , FMT='(A,F5.1,F6.1,F5.1,F5.1,F5.1)' ) &
         ' MYNN; in TEMF_MF: wstr,hfx,dtdz1,dtdz2,h0:', wstr,hfx,dthdz(1),dthdz(2),h0
         CALL wrf_debug ( 0 , mynn_message )
      ENDIF

      ! Set flag convective or not for use below
      is_convective = wstr > 0. .AND. dthdz(1)<0. .AND. dthdz(2)<0.  
      ! WA 12/16/09 require two levels of negative (unstable) gradient

      !*** Mass flux block starts here ***
      ! WA WFIP 11/13/15 allow multiple updrafts, deterministic for now

      if ( is_convective) then

         !print *,"In TEMF_MF is_convective, wstr = ", wstr
         IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
            WRITE ( mynn_message , FMT='(A)' ) &
            ' MYNN; in TEMF_MF: inconvective branch'
            CALL wrf_debug ( 0 , mynn_message )
         ENDIF

         !Cepsmf = 2. / max(200.,h0)
         Cepsmf = 1.0 / max(200.,h0) ! WA TEST reduce entrainment 
         ! Cepsmf = max(Cepsmf,0.002)
         ! Cepsmf = max(Cepsmf,0.0015)  ! WA TEST reduce max entrainment
         Cepsmf = max(Cepsmf,0.0005)  ! WA TEST reduce min entrainment

         do nu = 1,Nupd
            do k = kts,kte
               ! Calculate lateral entrainment fraction for subcloud layer
               ! epsilon and delta are defined on mass grid (half levels)
               ! epsmf(k,nu) = Cepsmf * (1+0.2*(floor(nu - Nupd/2.))) ! WA for three updrafts
               ! epsmf(k,nu) = Cepsmf * (1+0.05*(floor(nu - Nupd/2.))) ! WA for ten updrafts
               ! epsmf(k,nu) = Cepsmf * (1+0.0625*(floor(nu - Nupd/2.))) ! WA for eight updrafts
               ! epsmf(k,nu) = Cepsmf * (1+0.03*(floor(nu - Nupd/2.))) ! WA for eight updrafts, less spread
               epsmf(k,nu) = Cepsmf * (1+0.25*(nu-1)) ! WA for eight updrafts, much more eps for some plumes, per Neggers 2015 fig. 15 
            end do
            !print *,"In TEMF_MF Cepsmf, epsmf = ", Cepsmf, epsmf(1,:)
            !IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
            !   WRITE ( mynn_message , FMT='(A,F8.4)' ) &
            !   ' MYNN; in TEMF_MF, Cepsmf, epsmf(1:13,nu)=', Cepsmf
            !   CALL wrf_debug ( 0 , mynn_message )
            !   print*,"    epsmf(1:13,nu)=",epsmf(1:13,nu)
            !ENDIF

            ! Initialize updraft
            thlUPD(1,nu) = thetal(1) + Cw*wstr
            qtUPD(1,nu) = qtot(1) + 0.0*qfx/wstr
            rUPD(1,nu) = qtUPD(1,nu) / (1. - qtUPD(1,nu))
            wUPD(1,nu) = Cw * wstr
            wUPD_dry(1,nu) = Cw * wstr
            UUPD(1,nu) = u_temf(1)
            VUPD(1,nu) = v_temf(1)
            thetavUPD(1,nu) = thlUPD(1,nu) * (1. + 0.608*qtUPD(1,nu))  ! WA Assumes no liquid
            thetavUPDmoist(1,nu) = thetavUPD(1,nu)
            TEUPD(1,nu) = qke(1) + g / thetav(1) * sfcTHVF
            qlUPD(1,nu) = qc(1)  ! WA allow environment liquid
            TUPD(1,nu) = thlUPD(1,nu) * pi1d(1)
            !rstUPD(1,nu) = rsat_temf(p(1),TUPD(1,nu),ep_2)
            rstUPD(1,nu) = qsat_blend(TUPD(1,nu),p(1)) ! get saturation water vapor mixing ratio at tl and p
            rlUPD(1,nu) = 0.
#if (WRF_CHEM == 1)
            do ic = 1,nchem
               chemUPD(1,ic,nu) = chem(1,ic)
            enddo
#endif

            ! Calculate updraft parameters counting up
            do k = 2,kte
               ! WA 2/11/13 use hmax index to prevent oddness high up
               if ( k < hmax_idx) then
                  dthUPDdz(k-1,nu) = -epsmf(k,nu) * (thlUPD(k-1,nu) - thetal(k-1))
                  thlUPD(k,nu) = thlUPD(k-1,nu) + dthUPDdz(k-1,nu) * dzm(k-1)
                  dmoist_qtdz(k-1) = -epsmf(k,nu) * (qtUPD(k-1,nu) - qtot(k-1))
                  qtUPD(k,nu) = qtUPD(k-1,nu) + dmoist_qtdz(k-1) * dzm(k-1)
                  thetavUPD(k,nu) = thlUPD(k,nu) * (1. + 0.608*qtUPD(k,nu))  ! WA Assumes no liquid
                  B(k-1) = g * (thetavUPD(k,nu) - thetav(k)) / thetav(k)
                  if ( wUPD_dry(k-1,nu) < 1e-15 ) then
                     wUPD_dry(k,nu) = 0.
                  else
                     dwUPDdz(k-1,nu) = -2. *epsmf(k,nu)*wUPD_dry(k-1,nu) + 0.33*B(k-1)/wUPD_dry(k-1,nu)
                     wUPD_dry(k,nu) = wUPD_dry(k-1,nu) + dwUPDdz(k-1,nu) * dzm(k-1)
                  end if
                  dUUPDdz(k-1,nu) = -epsmf(k,nu) * (UUPD(k-1,nu) - u_temf(k-1))
                  UUPD(k,nu) = UUPD(k-1,nu) + dUUPDdz(k-1,nu) * dzm(k-1)
                  dVUPDdz(k-1,nu) = -epsmf(k,nu) * (VUPD(k-1,nu) - v_temf(k-1))
                  VUPD(k,nu) = VUPD(k-1,nu) + dVUPDdz(k-1,nu) * dzm(k-1)
                  dTEUPDdz(k-1,nu) = -epsmf(k,nu) * (TEUPD(k-1,nu) - qke(k-1))
                  TEUPD(k,nu) = TEUPD(k-1,nu) + dTEUPDdz(k-1,nu) * dzm(k-1)
                  ! Alternative updraft velocity based on moist thetav
                  ! Need thetavUPDmoist, qlUPD
                  rUPD(k,nu) = qtUPD(k,nu) / (1. - qtUPD(k,nu))
                  ! WA Updraft temperature assuming no liquid
                  TUPD(k,nu) = thlUPD(k,nu) * pi1d(k)
                  ! Updraft saturation mixing ratio
                  !rstUPD(k,nu) = rsat_temf(p(k-1),TUPD(k,nu),ep_2)
                  rstUPD(k,nu) = qsat_blend(TUPD(k,nu),p(k-1))
                  ! Correct to actual temperature (Sommeria & Deardorff 1977)
                  beta1(k) = 0.622 * (xlv/(r_d*TUPD(k,nu))) * (xlv/(cp*TUPD(k,nu)))
                  rstUPD(k,nu) = rstUPD(k,nu) * (1.0+beta1(k)*rUPD(k,nu)) / (1.0+beta1(k)*rstUPD(k,nu))
                  qstUPD(k,nu) = rstUPD(k,nu) / (1. + rstUPD(k,nu))
                  if (rUPD(k,nu) > rstUPD(k,nu)) then
                     rlUPD(k,nu) = rUPD(k,nu) - rstUPD(k,nu)
                     qlUPD(k,nu) = rlUPD(k,nu) / (1. + rlUPD(k,nu))
                     thetavUPDmoist(k,nu) = (thlUPD(k,nu) + ((xlv/cp)*qlUPD(k,nu)/pi1d(k))) * &
                                        (1. + 0.608*qstUPD(k,nu) - qlUPD(k,nu))
                  else
                     rlUPD(k,nu) = 0.
                     qlUPD(k,nu) = qc(k-1)   ! WA 4/6/10 allow environment liquid
                     thetavUPDmoist(k,nu) = thlUPD(k,nu) * (1. + 0.608*qtUPD(k,nu))
                  end if
                  Bmoist(k-1) = g * (thetavUPDmoist(k,nu) - thetav(k)) / thetav(k)
                  if ( wUPD(k-1,nu) < 1e-15 ) then
                     wUPD(k,nu) = 0.
                  else
                     dwUPDmoistdz(k-1,nu) = -2. *epsmf(k,nu)*wUPD(k-1,nu) + 0.33*Bmoist(k-1)/wUPD(k-1,nu)
                     wUPD(k,nu) = wUPD(k-1,nu) + dwUPDmoistdz(k-1,nu) * dzm(k-1)
                  end if
#if (WRF_CHEM == 1)
                  do ic = 1,nchem
                     dchemUPDdz(k-1,ic,nu) = -epsmf(k,nu) * (chemUPD(k-1,ic,nu) - chem(k-1,ic))
                     chemUPD(k,ic,nu) = chemUPD(k-1,ic,nu) + dchemUPDdz(k-1,ic,nu) * dzm(k-1)
                  enddo
#endif
               else ! above hmax
                  thlUPD(k,nu) = thetal(k)
                  qtUPD(k,nu) = qtot(k)
                  wUPD_dry(k,nu) = 0.
                  UUPD(k,nu) = u_temf(k)
                  VUPD(k,nu) = v_temf(k)
                  TEUPD(k,nu) = qke(k)
                  qlUPD(k,nu) = qc(k-1)
                  wUPD(k,nu) = 0.
#if (WRF_CHEM == 1)
                  do ic = 1,nchem
                     chemUPD(k,ic,nu) = chem(k-1,ic)
                  enddo
#endif
               end if

               IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
                 IF ( ABS(wUPD(k,nu))>10. ) THEN
                   WRITE ( mynn_message , FMT='(A,2I3)' ) &
                   ' MYNN, in TEMF_MF, huge w at (nu,k):', nu,k
                   CALL wrf_debug ( 0 , mynn_message )
                   print *,"     thlUPD(1:k,nu) = ", thlUPD(1:k,nu)
                   print *,"     wUPD(1:k,nu)   = ", wUPD(1:k,nu)
                   print *,"     Bmoist(1:k-1)  = ", Bmoist(1:k-1)
                   print *,"     epsmf(1:k,nu)  = ", epsmf(1:k,nu)
                 ENDIF
               ENDIF

            ENDDO !end-k

            ! Find hd based on wUPD
            if (wUPD_dry(1,nu) == 0.) then
               hdidx(nu) = 1
            else
               hdidx(nu) = kte  ! In case wUPD <= 0 not found
               do k = 2,kte
                  if (wUPD_dry(k,nu) <= 0. .OR. zm(k) > hmax) then
                     hdidx(nu) = k
                     ! goto 100   ! FORTRAN made me do it!
                     exit
                  end if
               end do
            end if
 100                   hd(nu) = zm(hdidx(nu))

            ! Find LCL, hct, and ht
            lclidx(nu) = kte   ! In case LCL not found
            do k = kts,kte
               if ( k < hmax_idx .AND. rUPD(k,nu) > rstUPD(k,nu)) then
                  lclidx(nu) = k
                  ! goto 200
                  exit
               end if
            end do
 200                   lcl(nu) = zm(lclidx(nu))

            if (hd(nu) > lcl(nu)) then   ! Forced cloud (at least) occurs
               ! Find hct based on wUPDmoist
               if (wUPD(1,nu) == 0.) then
                  hctidx(nu) = 1
               else
                  hctidx(nu) = kte  ! In case wUPD <= 0 not found
                  do k = 2,kte
                     if (wUPD(k,nu) <= 0. .OR. zm(k) > hmax) then
                        hctidx(nu) = k
                        ! goto 300   ! FORTRAN made me do it!
                        exit
                     end if
                  end do
               end if
 300                         hct(nu) = zm(hctidx(nu))
               if (hctidx(nu) <= hdidx(nu)+1) then   ! No active cloud
                  hct(nu) = hd(nu)
                  hctidx(nu) = hdidx(nu)
               else
               end if
            else   ! No cloud
               hct(nu) = hd(nu)
               hctidx(nu) = hdidx(nu)
            end if
            ht(nu) = max(hd(nu),hct(nu))
            htidx(nu) = max(hdidx(nu),hctidx(nu))

            ! Now truncate updraft at ht with taper
            do k = 1,kte
               if (zm(k) < 0.9*ht(nu)) then  ! Below taper region
                   tval = 1
               else if (zm(k) >= 0.9*ht(nu) .AND. zm(k) <= 1.0*ht(nu)) then
                  ! Within taper region
                  tval = 1. - ((zm(k) - 0.9*ht(nu)) / (1.0*ht(nu) - 0.9*ht(nu)))
               else  ! Above taper region
                  tval = 0.
               end if
               thlUPD(k,nu) = tval * thlUPD(k,nu) + (1-tval)*thetal(k)
               thetavUPD(k,nu) = tval * thetavUPD(k,nu) + (1-tval)*thetav(k)
               qtUPD(k,nu) = tval * qtUPD(k,nu) + (1-tval) * qtot(k)
               if (k > 1) then
                  qlUPD(k,nu) = tval * qlUPD(k,nu) + (1-tval) * qc(k-1)
               end if
               UUPD(k,nu) = tval * UUPD(k,nu) + (1-tval) * u_temf(k)
               VUPD(k,nu) = tval * VUPD(k,nu) + (1-tval) * v_temf(k)
               TEUPD(k,nu) = tval * TEUPD(k,nu) + (1-tval) * qke(k)
               if (zm(k) > ht(nu)) then  ! WA this is just for cleanliness
                  wUPD(k,nu) = 0.
                  dwUPDmoistdz(k,nu) = 0.
                  wUPD_dry(k,nu) = 0.
                  dwUPDdz(k,nu) = 0.
               end if
#if (WRF_CHEM == 1)
               do ic = 1,nchem
                  chemUPD(k,ic,nu) = tval * chemUPD(k,ic,nu) + (1-tval) * chem(k,ic)
               enddo
#endif
            end do

            ! Calculate lateral detrainment rate for cloud layer
            ! WA 8/5/15 constant detrainment
            ! deltmf(1,nu) = Cepsmf
            ! do k = 2,kte-1
            !    deltmf(k,nu) = deltmf(k-1,nu)
            ! end do
            ! deltmf(kte,nu) = Cepsmf
            deltmf(:,nu) = epsmf(:,nu)  ! WA TEST delt = eps everywhere 

            ! Calculate mass flux (defined on turbulence levels)
            mf_temfx(1,nu) = CM * wstr / Nupd
            ! WA 3/2/16 limit max MF for stability
            ! WA reduce the constant for improved numerical stability?
            mf_temfx(1,nu) = min(mf_temfx(1,nu),0.2/Nupd)
            do kt = 2,kte-1
               dMdz(kt,nu) = (epsmf(kt,nu) - deltmf(kt,nu)) * mf_temfx(kt-1,nu) * dzt(kt)
               mf_temfx(kt,nu) = mf_temfx(kt-1,nu) + dMdz(kt,nu)
               ! WA TEST 6/14/16 don't allow <0
               mf_temfx(kt,nu) = max(mf_temfx(kt,nu),0.)
               IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
                  IF ( mf_temfx(kt,nu)>=0.2/NUPD ) THEN
                     WRITE ( mynn_message , FMT='(A,2I3)' ) &
                     ' MYNN, in TEMF_MF, huge MF at (nu,k):', nu,kt
                     CALL wrf_debug ( 0 , mynn_message )
                     print *,"     mf_temfx(1:kt,nu)  = ", mf_temfx(1:kt,nu)
                  ENDIF
               ENDIF
            end do
            mf_temfx(kte,nu) = 0.

            ! Calculate cloud fraction (on mass levels)
            ! WA eventually replace this with the same saturation calculation
            ! used in the MYNN code above for consistency.
            ! WA TEST 6/14/16 make sure aUPD(1) is reasonable
            aUPD(1,nu) = 0.06/Nupd
            do k = 2,kte
               ! WA TEST 6/14/16 increase epsilon in test
               !if (wUPD(k-1,nu) >= 1.0e-15 .AND. wUPD(k,nu) >= 1.0e-15) then
               if (wUPD(k-1,nu) >= 1.0e-5 .AND. wUPD(k,nu) >= 1.0e-5) then
                  aUPD(k,nu) = ((mf_temfx(k-1,nu)+mf_temfx(k,nu))/2.0) / &
                         ((wUPD(k-1,nu)+wUPD(k,nu))/2.0)  ! WA average before divide, is that best?
               else
                  aUPD(k,nu) = 0.0
               end if
               sigq2 = aUPD(k,nu) * (qtUPD(k,nu)-qtot(k))
               if (sigq2 > 0.0) then
                  sigq(k) = sqrt(sigq2)
               else
                  sigq(k) = 0.0
               end if
               !rst = rsat_temf(p(k-1),th(k-1)*pi1d(k-1),ep_2)
               rst = qsat_blend(th(k-1)*pi1d(k-1),p(k-1))
               qst(k) = rst / (1. + rst)
               satdef(k) = qtot(k) - qst(k)
               if (satdef(k) <= 0.0) then
                  if (sigq(k) > 1.0e-15) then
                     cldfraUPD(k,nu) = max(0.5 + 0.36 * atan(1.55*(satdef(k)/sigq(k))),0.0) / Nupd
                  else
                     cldfraUPD(k,nu) = 0.0
                  end if
               else
                  cldfraUPD(k,nu) = 1.0 / Nupd
               end if
               if (zm(k) < lcl(nu)) then
                  cldfraUPD(k,nu) = 0.0
               end if
            end do

         end do  ! loop over nu updrafts

         ! Add updraft areas into edmf_a, etc.
         ! Add cloud fractions into cldfra_bl1d
         !cldfra_bl1d(1) = 0.0
         cfm_temfx = 0.0
         do k = 2,kte
            !cldfra_bl1d(k) = 0.0
            cldfra_sum = 0.0
            edmf_a(k) = 0.0
            edmf_w(k) = 0.0
            edmf_thl(k) = 0.0
            edmf_qt(k) = 0.0
            edmf_qc(k) = 0.0
            edmf_u(k) = 0.0
            edmf_v(k) = 0.0
            edmf_qke(k) = 0.0
            edmf_ent(k) = 0.0
#if (WRF_CHEM == 1)
            do ic = 1,nchem
               edmf_chem(k,ic) = 0.0
            enddo
#endif
            do nu = 1,Nupd
               ! WA TEST 6/14/16 no contribution from very small updrafts
               if (aUPD(k,nu)>1e-5) then
               edmf_a(k) = edmf_a(k) + aUPD(k,nu)
               edmf_w(k) = edmf_w(k) + aUPD(k,nu)*wUPD(k,nu)
               edmf_thl(k) = edmf_thl(k) + aUPD(k,nu)*thlUPD(k,nu)
            ! print *,"k,nu,aUPD,thlUPD,edmf_thl = ", k,nu,aUPD(k,nu),thlUPD(k,nu),edmf_thl(k)
               edmf_qt(k) = edmf_qt(k) + aUPD(k,nu)*qtUPD(k,nu)
               edmf_qc(k) = edmf_qc(k) + aUPD(k,nu)*qlUPD(k,nu)
               edmf_u(k) = edmf_u(k) + aUPD(k,nu)*UUPD(k,nu)
               edmf_v(k) = edmf_v(k) + aUPD(k,nu)*VUPD(k,nu)
               edmf_qke(k) = edmf_qke(k) + aUPD(k,nu)*TEUPD(k,nu)
               edmf_ent(k) = edmf_ent(k) + aUPD(k,nu)*epsmf(k,nu)
               !cldfra_bl1d(k) = cldfra_bl1d(k) + cldfraUPD(k,nu)
               cldfra_sum = cldfra_sum + cldfraUPD(k,nu)
#if (WRF_CHEM == 1)
               do ic = 1,nchem
                  edmf_chem(k,ic) = edmf_chem(k,ic) + aUPD(k,nu)*chemUPD(k,ic,nu)
               enddo
#endif
               endif
            end do

            IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
            !   print *,"In TEMF_MF edmf_w = ", edmf_w(1:10)
            !   print *,"In TEMF_MF edmf_a = ", edmf_a(1:10)
            !   print *,"In TEMF_MF edmf_thl = ", edmf_thl(1:10)
            !   print *,"In TEMF_MF aUPD(2,:) = ", aUPD(2,:)
            !   print *,"In TEMF_MF wUPD(2,:) = ", wUPD(2,:)
            !   print *,"In TEMF_MF thlUPD(2,:) = ", thlUPD(2,:)
            ENDIF

            ! WA TEST 6/14/16 don't divide by very small updrafts
            !if (edmf_a(k)>0.) then
            if (edmf_a(k)>1.e-3) then
               edmf_w(k)=edmf_w(k)/edmf_a(k)
               edmf_qt(k)=edmf_qt(k)/edmf_a(k)
               edmf_thl(k)=edmf_thl(k)/edmf_a(k)
               edmf_ent(k)=edmf_ent(k)/edmf_a(k)
               edmf_qc(k)=edmf_qc(k)/edmf_a(k)
               edmf_u(k)=edmf_u(k)/edmf_a(k)
               edmf_v(k)=edmf_v(k)/edmf_a(k)
               edmf_qke(k)=edmf_qke(k)/edmf_a(k)
#if (WRF_CHEM == 1)
               do ic = 1,nchem
                  edmf_chem(k,ic) = edmf_chem(k,ic)/edmf_a(k)
               enddo
#endif

               if (edmf_qc(k) > 0.0) then
                 IF (cldfra_sum > edmf_a(k)) THEN
                   cldfra_bl1d(k) = cldfra_sum
                   qc_bl1d(k) = edmf_qc(k)*edmf_a(k)/cldfra_sum
                 ELSE
                   cldfra_bl1d(k)=edmf_a(k)
                   qc_bl1d(k) = edmf_qc(k)
                 ENDIF
               endif
            endif

            ! Put max value so far into cfm
            if (zt(k) <= hmax) then
               cfm_temfx = max(cldfra_bl1d(k),cfm_temfx)
            end if
         end do

         !cldfra_bl1d(kte) = 0.0

         ! Computing variables needed for solver

         do k=kts,kte  ! do these in loop above
            ! WA TEST 6/14/16 don't use very small updrafts to be consistent
            ! with block above
            if (edmf_a(k)>1e-3) then
            s_aw(k)   = edmf_a(k)*edmf_w(k)*psig_w
            s_awthl(k)= edmf_a(k)*edmf_w(k)*edmf_thl(k)*psig_w
            s_awqt(k) = edmf_a(k)*edmf_w(k)*edmf_qt(k)*psig_w 
            s_awqc(k) = edmf_a(k)*edmf_w(k)*edmf_qc(k)*psig_w
            s_awqv(k) = s_awqt(k) - s_awqc(k)
            s_awu(k)  = edmf_a(k)*edmf_w(k)*edmf_u(k)*psig_w
            s_awv(k)  = edmf_a(k)*edmf_w(k)*edmf_v(k)*psig_w
            s_awqke(k) = edmf_a(k)*edmf_w(k)*edmf_qke(k)*psig_w
#if (WRF_CHEM == 1)
            do ic = 1,nchem
               s_awchem(k,ic) = edmf_w(k)*edmf_chem(k,ic)*psig_w
            enddo
#endif
            endif
            !now reduce diagnostic output array by psig
             edmf_a(k)=edmf_a(k)*psig_w 
         enddo

      ! end if   ! is_convective
      ! Mass flux block ends here
      else
         edmf_a = 0.
         edmf_w = 0.
         edmf_qt = 0.
         edmf_thl = 0.
         edmf_ent = 0.
         edmf_u = 0.
         edmf_v = 0.
         edmf_qke = 0.
         s_aw   = 0.
         s_awthl= 0.
         s_awqt = 0.
         s_awqv = 0.
         s_awqc = 0.
         s_awu  = 0.
         s_awv  = 0.
         s_awqke= 0.
         edmf_qc(1) = qc(1)
         !qc_bl1d(1) = qc(1)
         do k = kts+1,kte-1
            edmf_qc(k) = qc(k-1)
            !qc_bl1d(k) = qc(k-1)
         end do
#if (WRF_CHEM == 1)
         do ic = 1,nchem
            s_awchem(:,ic) = 0.
         enddo
#endif
      end if
      !edmf_qc(kte) = qc(kte)
      !qc_bl1d(kte) = qc(kte)

      !IF ( wrf_at_debug_level(MYNN_DBG_LVL) ) THEN
      !   print *,"After TEMF_MF, s_aw = ", s_aw(1:5)
      !   print *,"After TEMF_MF, s_awthl = ", s_awthl(1:5)
      !   print *,"After TEMF_MF, s_awqt = ", s_awqt(1:5)
      !   print *,"After TEMF_MF, s_awqc = ", s_awqc(1:5)
      !   print *,"After TEMF_MF, s_awqv = ", s_awqv(1:5)
      !   print *,"After TEMF_MF, s_awu = ", s_awu(1:5)
      !   print *,"After TEMF_MF, s_awv = ", s_awv(1:5)
      !   print *,"After TEMF_MF, s_awqke = ", s_awqke(1:5)
      !ENDIF

END SUBROUTINE temf_mf

!--------------------------------------------------------------------
!
   real function rsat_temf(p,T,ep2)

!  Calculates the saturation mixing ratio with respect to liquid water
!  Arguments are pressure (Pa) and absolute temperature (K)
!  Uses the formula from the ARM intercomparison setup.
!  Converted from Matlab by WA 7/28/08

implicit none
real p, T, ep2
real temp, x
real, parameter :: c0 = 0.6105851e+3
real, parameter :: c1 = 0.4440316e+2
real, parameter :: c2 = 0.1430341e+1
real, parameter :: c3 = 0.2641412e-1
real, parameter :: c4 = 0.2995057e-3
real, parameter :: c5 = 0.2031998e-5
real, parameter :: c6 = 0.6936113e-8
real, parameter :: c7 = 0.2564861e-11
real, parameter :: c8 = -0.3704404e-13

temp = T - 273.15

x =c0+temp*(c1+temp*(c2+temp*(c3+temp*(c4+temp*(c5+temp*(c6+temp*(c7+temp*c8)))))))
rsat_temf = ep2*x/(p-x)

return
end function rsat_temf

!=================================================================

END MODULE module_bl_mynn
