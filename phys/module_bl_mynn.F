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
!(approved by Mikio Nakanishi or under consideration):
! 1. Addition of BouLac mixing length in the free atmosphere.
! 2. Changed the turbulent mixing length to be integrated from the
!    surface to the top of the BL + a transition layer depth.
! 3. v3.4.1: Option to use Kitamura/Canuto modification which removes 
!    the critical Richardson number and negative TKE (default).
! 4. v3.4.1: Hybrid PBL height diagnostic, which blends a theta-v-based
!    definition in neutral/convective BL and a TKE-based definition
!    in stable conditions.
! 5. v3.4.1: TKE budget output option (bl_mynn_tkebudget)
! 6. v3.5.0: TKE advection option (bl_mynn_tkeadvect)
! 7. v3.5.1: Fog deposition related changes.
!
! For changes 1 and 3, see "JOE's mods" below:
!-------------------------------------------------------------------

MODULE module_bl_mynn

  USE module_model_constants, only: &
       &karman, g, p1000mb, &
       &cp, r_d, rcp, xlv, &
       &svp1, svp2, svp3, svpt0, ep_1, ep_2

!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------

! The parameters below depend on stability functions of module_sf_mynn.
  REAL, PARAMETER :: cphm_st=5.0, cphm_unst=16.0, &
                     cphh_st=5.0, cphh_unst=16.0

  REAL, PARAMETER :: xlvcp=xlv/cp, ev=xlv, rd=r_d, rk=cp/rd, &
       &svp11=svp1*1.e3, p608=ep_1, ep_3=1.-ep_2

  REAL, PARAMETER :: tref=300.0    ! reference temperature (K)
  REAL, PARAMETER :: tv0=p608*tref, tv1=(1.+p608)*tref, gtr=g/tref

! Closure constants
  REAL, PARAMETER :: &
       &vk  = karman, &
       &pr  =  0.74, &
       &g1  =  0.235, &
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

! Constants for length scale (alps & cns) and TKE diffusion (Sqfac)
! Original (Nakanishi and Niino 2009) (for CKmod=0.):
!  REAL, PARAMETER :: qmin=0.0, zmax=1.0, cns=2.7, & 
!           &alp1=0.23, alp2=1.0, alp3=5.0, alp4=100.0, &
!           &alp5=0.40, Sqfac=3.0
! Modified for Rapid Refresh/HRRR (and for CKmod=1.):
  REAL, PARAMETER :: qmin=0.0, zmax=1.0, cns=2.1, &
            &alp1=0.23, alp2=0.60, alp3=3.0, alp4=10.0, &
            &alp5=0.40, Sqfac=2.0

! Constants for gravitational settling
!  REAL, PARAMETER :: gno=1.e6/(1.e8)**(2./3.), gpw=5./3., qcgmin=1.e-8
  REAL, PARAMETER :: gno=1.0  !original value seems too agressive: 4.64158883361278196
  REAL, PARAMETER :: gpw=5./3., qcgmin=1.e-8, qkemin=1.e-12
!  REAL, PARAMETER :: pblh_ref=1500.

! Constants for cloud PDF (mym_condensation)
  REAL, PARAMETER :: rr2=0.7071068, rrp=0.3989423

!JOE's mods
  !Use Canuto/Kitamura mod (remove Ric and negative TKE) (1:yes, 0:no)
  !For more info, see Canuto et al. (2008 JAS) and Kitamura (Journal of the 
  !Meteorological Society of Japan, Vol. 88, No. 5, pp. 857-864, 2010).
  !Note that this change required further modification of other parameters
  !above (c2, c3, alp2, and Sqfac). If you want to remove this option, set these
  !parameters back to NN2009 values (see commented out lines next to the
  !parameters above). This only removes the negative TKE problem
  !but does not necessarily improve performance - neutral impact.
  REAL, PARAMETER :: CKmod=1.

  !Use BouLac mixing length in free atmosphere (1:yes, 0:no)
  !This helps remove excessively large mixing in unstable layers aloft.
  REAL, PARAMETER :: BLmod=1.
!JOE-end

  INTEGER :: mynn_level

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
!       mx, my          : Maximum numbers of grid boxes
!                         in the x and y directions, respectively
!       nx, ny, nz      : Numbers of the actual grid boxes
!                         in the x, y and z directions, respectively
!       tref            : Reference temperature                      (K)
!       dz(nz)        : Vertical grid spacings                     (m)
!                         # dz(nz)=dz(nz-1)
!       zw(nz+1)        : Heights of the walls of the grid boxes     (m)
!                         # zw(1)=0.0 and zw(k)=zw(k-1)+dz(k-1)
!       h(mx,ny)        : G^(1/2) in the terrain-following coordinate
!                         # h=1-zg/zt, where zg is the height of the
!                           terrain and zt the top of the model domain
!       pi0(mx,my,nz) : Exner function at zw*h+zg             (J/kg K)
!                         defined by c_p*( p_basic/1000hPa )^kappa
!                         This is usually computed by integrating
!                         d(pi0)/dz = -h*g/tref.
!       rmo(mx,ny)      : Inverse of the Obukhov length         (m^(-1))
!       flt, flq(mx,ny) : Turbulent fluxes of sensible and latent heat,
!                         respectively, e.g., flt=-u_*Theta_* (K m/s)
!! flt - liquid water potential temperature surface flux
!! flq - total water flux surface flux
!       ust(mx,ny)      : Friction velocity                        (m/s)
!       pmz(mx,ny)      : phi_m-zeta at z1*h+z0, where z1 (=0.5*dz(1))
!                         is the first grid point above the surafce, z0
!                         the roughness length and zeta=(z1*h+z0)*rmo
!       phh(mx,ny)      : phi_h at z1*h+z0
!       u, v(mx,my,nz): Components of the horizontal wind        (m/s)
!       thl(mx,my,nz)  : Liquid water potential temperature
!                                                                    (K)
!       qw(mx,my,nz)  : Total water content Q_w                (kg/kg)
!
!     Output variables:
!       ql(mx,my,nz)  : Liquid water content                   (kg/kg)
!       v?(mx,my,nz)  : Functions for computing the buoyancy flux
!       qke(mx,my,nz) : Twice the turbulent kinetic energy q^2
!                                                              (m^2/s^2)
!       tsq(mx,my,nz) : Variance of Theta_l                      (K^2)
!       qsq(mx,my,nz) : Variance of Q_w
!       cov(mx,my,nz) : Covariance of Theta_l and Q_w              (K)
!       el(mx,my,nz)  : Master length scale L                      (m)
!                         defined on the walls of the grid boxes
!       bsh             : no longer used
!       via common      : Closure constants
!
!     Work arrays:        see subroutine mym_level2
!       pd?(mx,my,nz) : Half of the production terms at Level 2
!                         defined on the walls of the grid boxes
!       qkw(mx,my,nz) : q on the walls of the grid boxes         (m/s)
!
!     # As to dtl, ...gh, see subroutine mym_turbulence.
!
!-------------------------------------------------------------------
  SUBROUTINE  mym_initialize ( kts,kte,&
       &            dz, zw,  &
       &            u, v, thl, qw, &
!       &            ust, rmo, pmz, phh, flt, flq,&
!JOE-BouLac/PBLH mod
       &        zi,theta,&
       &        sh,&
!JOE-end
       &            ust, rmo, el,&
       &            Qke, Tsq, Qsq, Cov)
!
!-------------------------------------------------------------------
    
    INTEGER, INTENT(IN)   :: kts,kte
!    REAL, INTENT(IN)   :: ust, rmo, pmz, phh, flt, flq
    REAL, INTENT(IN)   :: ust, rmo
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw

    REAL, DIMENSION(kts:kte), INTENT(out) :: tsq,qsq,cov
    REAL, DIMENSION(kts:kte), INTENT(inout) :: el,qke

    REAL, DIMENSION(kts:kte) :: &
         &ql,pdk,pdt,pdq,pdc,dtl,dqw,dtv,&
         &gm,gh,sm,sh,qkw,vt,vq
    INTEGER :: k,l,lmax
    REAL :: phm,vkz,elq,elv,b1l,b2l,pmz=1.,phh=1.,flt=0.,flq=0.,tmpq
!JOE-BouLac and PBLH mod
    REAL :: zi
    REAL, DIMENSION(kts:kte) :: theta
!JOE-end


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
       CALL mym_length ( kts,kte,&
            &            dz, zw, &
            &            rmo, flt, flq, &
            &            vt, vq, &
            &            qke, &
            &            dtv, &
            &            el, &
!JOE-added for BouLac/PBHL
            &            zi,theta,&
!JOE-end
            &            qkw)
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
!       dtl(mx,my,nz) : Vertical gradient of Theta_l             (K/m)
!       dqw(mx,my,nz) : Vertical gradient of Q_w
!       dtv(mx,my,nz) : Vertical gradient of Theta_V             (K/m)
!       gm (mx,my,nz) : G_M divided by L^2/q^2                (s^(-2))
!       gh (mx,my,nz) : G_H divided by L^2/q^2                (s^(-2))
!       sm (mx,my,nz) : Stability function for momentum, at Level 2
!       sh (mx,my,nz) : Stability function for heat, at Level 2
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
       vtt =  1.0 +vt(k)*abk +vt(k-1)*afk
       vqq =  tv0 +vq(k)*abk +vq(k-1)*afk
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
!       elt(mx,ny)      : Length scale depending on the PBL depth    (m)
!       vsc(mx,ny)      : Velocity scale q_c                       (m/s)
!                         at first, used for computing elt
!
!     NOTE: the mixing lengths are meant to be calculated at the full-
!           sigmal levels (or interfaces beween the model layers).
!
  SUBROUTINE  mym_length ( kts,kte,&
    &            dz, zw, &
    &            rmo, flt, flq, &
    &            vt, vq, &
    &            qke, &
    &            dtv, &
    &            el, &
    &            zi,theta,&       !JOE-BouLac mod
    &            qkw)
    
!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq
    REAL, DIMENSION(kts:kte), INTENT(IN)   :: qke,vt,vq

    REAL, DIMENSION(kts:kte), INTENT(out)  :: qkw, el
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dtv

    REAL :: elt,vsc
!JOE-added for BouLac ML
    REAL, DIMENSION(kts:kte), INTENT(IN) :: theta
    REAL, DIMENSION(kts:kte) :: qtke,elBLmin,elBLavg,thetaw
    REAL :: wt,zi,zi2,h1,h2

    !THE FOLLOWING LIMITS DO NOT DIRECTLY AFFECT THE ACTUAL PBLH.
    !THEY ONLY IMPOSE LIMITS ON THE CALCULATION OF THE MIXING LENGTH 
    !SCALES SO THAT THE BOULAC MIXING LENGTH (IN FREE ATMOS) DOES
    !NOT ENCROACH UPON THE BOUNDARY LAYER MIXING LENGTH (els, elb & elt).
    REAL, PARAMETER :: minzi = 300.  !min mixed-layer height
    REAL, PARAMETER :: maxdz = 750.  !max (half) transition layer depth
                                     !=0.3*2500 m PBLH, so the transition
                                     !layer stops growing for PBLHs > 2.5 km.
    REAL, PARAMETER :: mindz = 300.  !min (half) transition layer depth

    !SURFACE LAYER LENGTH SCALE MODS TO REDUCE IMPACT IN UPPER BOUNDARY LAYER
    REAL, PARAMETER :: ZSLH = 100. ! Max height correlated to surface conditions (m)
    REAL, PARAMETER :: CSL = 2.    ! CSL = constant of proportionality to L O(1)
    REAL :: z_m, alp4b

!Joe-end

    INTEGER :: i,j,k
    REAL :: afk,abk,zwk,dzk,qdz,vflx,bv,elb,els,elf

!    tv0 = 0.61*tref
!    gtr = 9.81/tref
!
!JOE-added to impose limits on the height integration for elt as well 
!    as the transition layer depth
    IF ( BLmod .EQ. 0. ) THEN
       zi2=5000.  !originally integrated to model top, not just 5000 m.
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

!JOE- BouLac Start 
       qtke(k) = (qkw(k)**2.)/2.    ! q -> TKE
       thetaw(k)= theta(k)*abk + theta(k-1)*afk
!JOE- BouLac End

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
     DO WHILE (zwk .LE. MIN((zi2+h1), 3000.)) !JOE: 20130523 reduce too high diffusivity over mts 
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
!JOE- BouLac Start
    IF ( BLmod .GT. 0. ) THEN
       ! COMPUTE BouLac mixing length
       CALL boulac_length(kts,kte,zw,dz,qtke,thetaw,elBLmin,elBLavg)
    ENDIF
!JOE- BouLac END

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
       END IF
!
       z_m = MAX(ZSLH,CSL*zwk*rmo)

!   **  Length scale in the surface layer  **
       IF ( rmo .GT. 0.0 ) THEN
       !   IF ( zwk <= z_m ) THEN  ! use original cns
             els = vk*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
             !els = vk*zwk/(1.0+cns*MIN( 0.5*zw(kts+1)*rmo, zmax ))
       !   ELSE
       !      !blend to neutral values (kz) above z_m
       !      els = vk*z_m/(1.0+cns*MIN( zwk*rmo, zmax )) + vk*(zwk - z_m)
       !   ENDIF
       ELSE
          !orig: els =  vk*zwk*( 1.0 - alp4* zwk*rmo )**0.2
          !start blending to original alp4 [~O(z)] above 500 m
          wt=.5*TANH((zwk - 1000.)/500.) + .5
          alp4b =alp4*(1.-wt) + 100.*wt
          els =  vk*zwk*( 1.0 - alp4b* zwk*rmo )**0.2
       END IF
!
!   ** HARMONC AVERGING OF MIXING LENGTH SCALES: 
!       el(k) =      MIN(elb/( elb/elt+elb/els+1.0 ),elf)
!       el(k) =      elb/( elb/elt+elb/els+1.0 )
!JOE- BouLac Start
       IF ( BLmod .EQ. 0. ) THEN
          el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
       ELSE
          !add blending to use BouLac mixing length in free atmos;
          !defined relative to the PBLH (zi) + transition layer (h1)
          el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
          wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
          el(k) = el(k)*(1.-wt) + alp5*elBLmin(k)*wt
       ENDIF
!JOE- BouLac End

       !IF (el(k) > 1000.) THEN
       !   print*,"SUSPICIOUSLY LARGE Lm:",el(k),k
       !ENDIF
    END DO
!
    RETURN

  END SUBROUTINE mym_length

!JOE- BouLac Code Start -
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
!       dfm(mx,my,nz) : Diffusivity coefficient for momentum,
!                         divided by dz (not dz*h(i,j))            (m/s)
!       dfh(mx,my,nz) : Diffusivity coefficient for heat,
!                         divided by dz (not dz*h(i,j))            (m/s)
!       dfq(mx,my,nz) : Diffusivity coefficient for q^2,
!                         divided by dz (not dz*h(i,j))            (m/s)
!       tcd(mx,my,nz)   : Countergradient diffusion term for Theta_l
!                                                                  (K/s)
!       qcd(mx,my,nz)   : Countergradient diffusion term for Q_w
!                                                              (kg/kg s)
!       pd?(mx,my,nz) : Half of the production terms
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
  SUBROUTINE  mym_turbulence ( kts,kte,&
    &            levflag, &
    &            dz, zw, &
    &            u, v, thl, ql, qw, &
    &            qke, tsq, qsq, cov, &
    &            vt, vq,&
    &            rmo, flt, flq, &
!JOE-BouLac/PBLH test
    &            zi,theta,&
    &            sh,&
!JOE-end
    &            El,&
    &            Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc &
!JOE-TKE BUDGET
    &		 ,qWT1D,qSHEAR1D,qBUOY1D,qDISS1D &
    &            ,bl_mynn_tkebudget &
!JOE-end
    &)

!-------------------------------------------------------------------
!
    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: levflag
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq   
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,& 
         &ql,vt,vq,qke,tsq,qsq,cov

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

!JOE-added for BouLac/PBLH test
    REAL :: zi
    REAL, DIMENSION(kts:kte), INTENT(in) :: theta
!JOE-end

    REAL ::  a2den, duz, ri, HLmod  !JOE-Canuto/Kitamura mod

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
    CALL mym_length (kts,kte, &
    &            dz, zw, &
    &            rmo, flt, flq, &
    &            vt, vq, &
    &            qke, &
    &            dtv, &
    &            el, &
    &            zi,theta,&  !JOE-hybrid PBLH
    &            qkw)
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
!
!JOE-add prints
       IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
          PRINT*,"MYM_TURBULENCE2.0: k=",k," sh=",sh(k)
          PRINT*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
          PRINT*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
          PRINT*," qke=",qke(k)," el=",el(k)," ri=",ri
          PRINT*," PBLH=",zi," u=",u(k)," v=",v(k)
       ENDIF
!JOE-Apply Helfand & Labraga stability check for all Ric
!    when CKmod == 1.
        IF (CKmod .eq. 1) THEN
          HLmod = q2sq -1.
       ELSE
          HLmod = q3sq
       ENDIF
!     **  Since qkw is set to more than 0.0, q3sq > 0.0.  **
       IF ( q3sq .LT. q2sq ) THEN
!       IF ( HLmod .LT. q2sq ) THEN
!JOE-END
          qdiv = SQRT( q3sq/q2sq )   !HH89: (1-alfa)
          sm(k) = sm(k) * qdiv
          sh(k) = sh(k) * qdiv
!
!JOE-Canuto/Kitamura mod
!          e1   = q3sq - e1c*ghel * qdiv**2
!          e2   = q3sq - e2c*ghel * qdiv**2
!          e3   = e1   + e3c*ghel * qdiv**2
!          e4   = e1   - e4c*ghel * qdiv**2
          e1   = q3sq - e1c*ghel/a2den * qdiv**2
          e2   = q3sq - e2c*ghel/a2den * qdiv**2
          e3   = e1   + e3c*ghel/(a2den**2) * qdiv**2
          e4   = e1   - e4c*ghel/a2den * qdiv**2
!JOE-end
          eden = e2*e4 + e3*e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
       ELSE
!JOE-Canuto/Kitamura mod
!          e1   = q3sq - e1c*ghel
!          e2   = q3sq - e2c*ghel
!          e3   = e1   + e3c*ghel
!          e4   = e1   - e4c*ghel
          e1   = q3sq - e1c*ghel/a2den
          e2   = q3sq - e2c*ghel/a2den
          e3   = e1   + e3c*ghel/(a2den**2)
          e4   = e1   - e4c*ghel/a2den
!JOE-end
          eden = e2*e4 + e3*e5c*gmel
          eden = MAX( eden, 1.0d-20 )
!
          qdiv = 1.0
          sm(k) = q3sq*a1*( e3-3.0*c1*e4       )/eden
!JOE-Canuto/Kitamura mod
!          sh(k) = q3sq*a2*( e2+3.0*c1*e5c*gmel )/eden
          sh(k) = q3sq*(a2/a2den)*( e2+3.0*c1*e5c*gmel )/eden
!JOE-end
       END IF
!
!      HL88 , lev2.5 criteria from eqs. 3.17, 3.19, & 3.20                                                                                                                                                           
       IF (sh(k)<0.0 .OR. sm(k)<0.0 .OR. &
           sh(k) > 0.76*b2 .or. (sm(k)**2*gm(k) .gt. .44**2)) THEN
          PRINT*,"MYM_TURBULENCE2.5: k=",k," sh=",sh(k)
          PRINT*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
          PRINT*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
          PRINT*," qke=",qke(k)," el=",el(k)," ri=",ri
          PRINT*," PBLH=",zi," u=",u(k)," v=",v(k)
       ENDIF

!   **  Level 3 : start  **
       IF ( levflag .EQ. 3 ) THEN
          t2sq = qdiv*b2*elsq*sh(k)*dtl(k)**2
          r2sq = qdiv*b2*elsq*sh(k)*dqw(k)**2
          c2sq = qdiv*b2*elsq*sh(k)*dtl(k)*dqw(k)
          t3sq = MAX( tsq(k)*abk+tsq(k-1)*afk, 0.0 )
          r3sq = MAX( qsq(k)*abk+qsq(k-1)*afk, 0.0 )
          c3sq =      cov(k)*abk+cov(k-1)*afk
!
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
!JOE-Canuto/Kitamura mod
!          e2   = q3sq - e2c*ghel * qdiv**2
!          e3   = q3sq + e3c*ghel * qdiv**2
!          e4   = q3sq - e4c*ghel * qdiv**2
          e2   = q3sq - e2c*ghel/a2den * qdiv**2
          e3   = q3sq + e3c*ghel/(a2den**2) * qdiv**2
          e4   = q3sq - e4c*ghel/a2den * qdiv**2
!JOE-end
          eden = e2*e4  + e3 *e5c*gmel * qdiv**2
!
!JOE-Canuto/Kitamura mod
!          wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
!               &        *( e2*e4c - e3c*e5c*gmel * qdiv**2 )
          wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
               &        *( e2*e4c/a2den - e3c*e5c*gmel/(a2den**2) * qdiv**2 )
!JOE-end
!
          IF ( wden .NE. 0.0 ) THEN
             clow = q3sq*( 0.12-cw25 )*eden/wden
             cupp = q3sq*( 0.76-cw25 )*eden/wden
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
!
!JOE-Canuto/Kitamura mod
!          e6c  = 3.0*a2*cc3*gtr * dlsq/elsq
          e6c  = 3.0*(a2/a2den)*cc3*gtr * dlsq/elsq
!JOE-end
!
!     **  for Gamma_theta  **
!!          enum = qdiv*e6c*( t3sq-t2sq )
          IF ( t2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( t3sq-t2sq ), 0.0d0 )
          ENDIF

          gamt =-e1  *enum    /eden
!
!     **  for Gamma_q  **
!!          enum = qdiv*e6c*( r3sq-r2sq )
          IF ( r2sq .GE. 0.0 ) THEN
             enum = MAX( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ELSE
             enum = MIN( qdiv*e6c*( r3sq-r2sq ), 0.0d0 )
          ENDIF

          gamq =-e1  *enum    /eden
!
!     **  for Sm' and Sh'd(Theta_V)/dz  **
!!          enum = qdiv*e6c*( c3sq-c2sq )
          enum = MAX( qdiv*e6c*( c3sq-c2sq ), 0.0d0)

!JOE-Canuto/Kitamura mod
!          smd  = dlsq*enum*gtr/eden * qdiv**2 * (e3c+e4c)*a1/a2
          smd  = dlsq*enum*gtr/eden * qdiv**2 * (e3c/(a2den**2) + &
               & e4c/a2den)*a1/(a2/a2den)
!JOE-end
          gamv = e1  *enum*gtr/eden
!
          sm(k) = sm(k) +smd
!
!     **  For elh (see below), qdiv at Level 3 is reset to 1.0.  **
          qdiv = 1.0
!   **  Level 3 : end  **
!
          IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
            PRINT*,"MYM_TURBULENCE3.0: k=",k," sh=",sh(k)
            PRINT*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
            PRINT*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
            PRINT*," qke=",qke(k)," el=",el(k)," ri=",ri
            PRINT*," PBLH=",zi," u=",u(k)," v=",v(k)
          ENDIF

       ELSE
!     **  At Level 2.5, qdiv is not reset.  **
          gamt = 0.0
          gamq = 0.0
          gamv = 0.0
       END IF
!
       elq = el(k)*qkw(k)
       elh = elq*qdiv
!
       pdk(k) = elq*( sm(k)*gm(k) &
            &                    +sh(k)*gh(k)+gamv )
       pdt(k) = elh*( sh(k)*dtl(k)+gamt )*dtl(k)
       pdq(k) = elh*( sh(k)*dqw(k)+gamq )*dqw(k)
       pdc(k) = elh*( sh(k)*dtl(k)+gamt )&
            &*dqw(k)*0.5 &
                  &+elh*( sh(k)*dqw(k)+gamq )*dtl(k)*0.5
!
       tcd(k) = elq*gamt
       qcd(k) = elq*gamq
!
       dfm(k) = elq*sm (k) / dzk
       dfh(k) = elq*sh (k) / dzk
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
!       qke(mx,my,nz) : qke at (n)th time level
!       tsq, ...cov     : ditto
!
!     Output variables:
!       qke(mx,my,nz) : qke at (n+1)th time level
!       tsq, ...cov     : ditto
!
!     Work arrays:
!       qkw(mx,my,nz)   : q at the center of the grid boxes        (m/s)
!       bp (mx,my,nz)   : = 1/2*F,     see below
!       rp (mx,my,nz)   : = P-1/2*F*Q, see below
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
       &            qke, tsq, qsq, cov &
       &)

!-------------------------------------------------------------------
    INTEGER, INTENT(IN)   :: kts,kte    
    INTEGER, INTENT(IN) :: levflag
    REAL, INTENT(IN) :: delt
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz, dfq,el
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: pdk, pdt, pdq, pdc
    REAL, INTENT(IN) ::  flt, flq, ust, pmz, phh
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: qke,tsq, qsq, cov

    INTEGER :: k,nz
    REAL, DIMENSION(kts:kte) :: qkw, bp, rp, df3q
    REAL :: vkz,pdk1,phm,pdt1,pdq1,pdc1,b1l,b2l
    REAL, DIMENSION(kts:kte) :: dtz
    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    nz=kte-kts+1

!   **  Strictly, vkz*h(i,j) -> vk*( 0.5*dz(1)*h(i,j)+z0 )  **
    vkz = vk*0.5*dz(kts)
!
!  Modified: Dec/22/2005, from here
!   **  dfq for the TKE is 3.0*dfm.  **
!    CALL coefvu ( dfq, 3.0 ) ! make change here
!  Modified: Dec/22/2005, up to here
!
    DO k = kts,kte
!!       qke(k) = MAX(qke(k), 0.0)
       qkw(k) = SQRT( MAX( qke(k), 0.0 ) )
       !df3q(k)=3.*dfq(k)
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
       a(k-kts+1)=-dtz(k)*df3q(k)
       b(k-kts+1)=1.+dtz(k)*(df3q(k)+df3q(k+1))+bp(k)*delt
       c(k-kts+1)=-dtz(k)*df3q(k+1)
       d(k-kts+1)=rp(k)*delt + qke(k)
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
!       cld(mx,my,nz)   : Cloud fraction
!
!     Work arrays:
!       qmq(mx,my,nz)   : Q_w-Q_{sl}, where Q_{sl} is the saturation
!                         specific humidity at T=Tl
!       alp(mx,my,nz)   : Functions in the condensation process
!       bet(mx,my,nz)   : ditto
!       sgm(mx,my,nz)   : Combined standard deviation sigma_s
!                         multiplied by 2/alp
!
!     # qmq, alp, bet and sgm are allowed to share storage units with
!       any four of other work arrays for saving memory.
!
!     # Results are sensitive particularly to values of cp and rd.
!       Set these values to those adopted by you.
!
!-------------------------------------------------------------------
  SUBROUTINE  mym_condensation (kts,kte, &
    &            dz, &
    &            thl, qw, &
    &            p,exner, &
    &            tsq, qsq, cov, &
    &            Sh, el, bl_mynn_cloudpdf,&  !JOE - cloud PDF testing
    &            Vt, Vq)

!-------------------------------------------------------------------
    INTEGER, INTENT(IN)   :: kts,kte, bl_mynn_cloudpdf

    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
    REAL, DIMENSION(kts:kte), INTENT(IN) :: p,exner, thl, qw, &
         &tsq, qsq, cov

    REAL, DIMENSION(kts:kte), INTENT(OUT) :: vt,vq

    REAL, DIMENSION(kts:kte) :: qmq,alp,bet,sgm,ql,cld

    DOUBLE PRECISION :: t3sq, r3sq, c3sq
!

    REAL :: p2a,t,esl,qsl,dqsl,q1,cld0,eq1,qll,&
         &q2p,pt,rac,qt
    INTEGER :: i,j,k

    REAL :: erf

    !JOE: NEW VARIABLES FOR ALTERNATE SIGMA
    REAL::dth,dqw,dzk
    REAL, DIMENSION(kts:kte), INTENT(IN) :: Sh,el

! Note: kte needs to be larger than kts, i.e., kte >= kts+1.

    DO k = kts,kte-1
       p2a = exner(k)
       t  = thl(k)*p2a 

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
       esl=svp11*EXP(svp2*(t-svpt0)/(t-svp3))
       !SATURATED SPECIFIC HUMIDITY
       qsl=ep_2*esl/(p(k)-ep_3*esl)
       !dqw/dT: Clausius-Clapeyron
       dqsl = qsl*ep_2*ev/( rd*t**2 )
       !DEFICIT/EXCESS WATER CONTENT
       qmq(k) = qw(k) -qsl

       alp(k) = 1.0/( 1.0+dqsl*xlvcp )
       bet(k) = dqsl*p2a
!
       t3sq = MAX( tsq(k), 0.0 )
       r3sq = MAX( qsq(k), 0.0 )
       c3sq =      cov(k)
       c3sq = SIGN( MIN( ABS(c3sq), SQRT(t3sq*r3sq) ), c3sq )
!
       r3sq = r3sq +bet(k)**2*t3sq -2.0*bet(k)*c3sq
       IF (bl_mynn_cloudpdf == 0) THEN
          !ORIGINAL STANDARD DEVIATION: limit e-6 produces ~10% more BL clouds than e-10
          sgm(k) = SQRT( MAX( r3sq, 1.0d-10 ))
       ELSE
          !ALTERNATIVE FORM (Nakanishi & Niino 2004 BLM, eq. B6, and 
          ! Kuwano-Yoshida et al. 2010 QJRMS, eq. 7):
          if (k .eq. kts) then 
             dzk = 0.5*dz(k)
          else
             dzk = 0.5*( dz(k) + dz(k-1) )
          end if
          dth = 0.5*(thl(k+1)+thl(k)) - 0.5*(thl(k)+thl(MAX(k-1,kts)))
          dqw = 0.5*(qw(k+1) + qw(k)) - 0.5*(qw(k) + qw(MAX(k-1,kts)))
          sgm(k) = SQRT( MAX( (alp(k)**2 * MAX(el(k)**2,1.) * &
                             b2 * MAX(Sh(k),0.03))/4. * &
                      (dqw/dzk - bet(k)*(dth/dzk ))**2 , 1.0e-10) )
       ENDIF
    END DO
!
    DO k = kts,kte-1
       !NORMALIZED DEPARTURE FROM SATURATION
       q1   = qmq(k) / sgm(k)
       !CLOUD FRACTION. rr2 = 1/SQRT(2) = 0.707
       cld(k) = 0.5*( 1.0+erf( q1*rr2 ) )
!       IF (cld(k) < 0. .OR. cld(k) > 1.) THEN
!          PRINT*,"MYM_CONDENSATION, k=",k," cld=",cld(k)
!          PRINT*," r3sq=",r3sq," t3sq=",t3sq," c3sq=",c3sq
!       ENDIF
!       q1=0.
!       cld(k)=0.

       !qll IS THE NORMALIZED LIQUID WATER CONTENT (Sommeria and
       !Deardorff (1977, eq 29a). rrp = 1/(sqrt(2*pi)) = 0.3989
       eq1  = rrp*EXP( -0.5*q1*q1 )
       qll  = MAX( cld(k)*q1 + eq1, 0.0 )
       !ESTIMATED LIQUID WATER CONTENT (UNNORMALIZED)
       ql (k) = alp(k)*sgm(k)*qll
!
       q2p  = xlvcp/exner(k) 
       !POTENTIAL TEMPERATURE
       pt   = thl(k) +q2p*ql(k)
       !qt is a THETA-V CONVERSION FOR TOTAL WATER (i.e., THETA-V = qt*THETA)
       qt   = 1.0 +p608*qw(k) -(1.+p608)*ql(k)
       rac  = alp(k)*( cld(k)-qll*eq1 )*( q2p*qt-(1.+p608)*pt )

       !BUOYANCY FACTORS: wherever vt and vq are used, there is a
       !"+1" and "+tv0", respectively, so these are subtracted out here.
       !vt is unitless and vq has units of K.
       vt (k) =      qt-1.0 -rac*bet(k)
       vq (k) = p608*pt-tv0 +rac
    END DO
!

    cld(kte) = cld(kte-1)
    ql(kte) = ql(kte-1)
    vt(kte) = vt(kte-1)
    vq(kte) = vq(kte-1)

    RETURN

  END SUBROUTINE mym_condensation

! ==================================================================
  SUBROUTINE mynn_tendencies(kts,kte,&
       &levflag,grav_settling,&
       &delt,&
       &dz,&
       &u,v,th,qv,qc,p,exner,&
       &thl,sqv,sqc,sqw,&
       &ust,flt,flq,wspd,qcg,&
       &uoce,voce,&
       &tsq,qsq,cov,&
       &tcd,qcd,&
       &dfm,dfh,dfq,&
       &Du,Dv,Dth,Dqv,Dqc&
       &,vdfg1&               !Katata/JOE-fogdes
       &)

!-------------------------------------------------------------------
    INTEGER, INTENT(in) :: kts,kte
    INTEGER, INTENT(in) :: grav_settling,levflag

!! grav_settling = 1 or 2 for gravitational settling of droplets
!! grav_settling = 0 otherwise
! thl - liquid water potential temperature
! qw - total water
! dfm,dfh,dfq - as above
! flt - surface flux of thl
! flq - surface flux of qw

    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,th,qv,qc,p,exner,&
         &dfm,dfh,dfq,dz,tsq,qsq,cov,tcd,qcd
    REAL, DIMENSION(kts:kte), INTENT(inout) :: thl,sqw,sqv,sqc
    REAL, DIMENSION(kts:kte), INTENT(out) :: du,dv,dth,dqv,dqc
    REAL, INTENT(IN) :: delt,ust,flt,flq,wspd,uoce,voce,qcg

!    REAL, INTENT(IN) :: delt,ust,flt,flq,qcg,&
!         &gradu_top,gradv_top,gradth_top,gradqv_top

!local vars

    REAL, DIMENSION(kts:kte) :: dtz,vt,vq

    REAL, DIMENSION(1:kte-kts+1) :: a,b,c,d

    REAL :: rhs,gfluxm,gfluxp,dztop

    REAL :: grav_settling2,vdfg1    !Katata-fogdes

    INTEGER :: k,kk,nz

    nz=kte-kts+1

    dztop=.5*(dz(kte)+dz(kte-1))

    DO k=kts,kte
       dtz(k)=delt/dz(k)
    ENDDO

!!============================================
!! u
!!============================================
   
    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*(dfm(k+1)+ust**2/wspd)
    c(1)=-dtz(k)*dfm(k+1)
    d(1)=u(k)+dtz(k)*uoce*ust**2/wspd

!!    a(1)=0.
!!    b(1)=1.+dtz(k)*dfm(k+1)
!!    c(1)=-dtz(k)*dfm(k+1)
!!    d(1)=u(k)*(1.-ust**2/wspd*dtz(k))
    
    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k)
       b(kk)=1.+dtz(k)*(dfm(k)+dfm(k+1))
       c(kk)=-dtz(k)*dfm(k+1)
       d(kk)=u(k)
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
    b(1)=1.+dtz(k)*(dfm(k+1)+ust**2/wspd)
    c(1)=-dtz(k)*dfm(k+1)
    d(1)=v(k)+dtz(k)*voce*ust**2/wspd

!!    a(1)=0.
!!    b(1)=1.+dtz(k)*dfm(k+1)
!!    c(1)=-dtz(k)*dfm(k+1)
!!    d(1)=v(k)*(1.-ust**2/wspd*dtz(k))

    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfm(k)
       b(kk)=1.+dtz(k)*(dfm(k)+dfm(k+1))
       c(kk)=-dtz(k)*dfm(k+1)
       d(kk)=v(k)
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
!!============================================
    k=kts

    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    
!Katata - added
    grav_settling2 = MIN(REAL(grav_settling),1.)
!Katata - end

! if qcg not used then assume constant flux in the surface layer
!JOE-remove original code
!    IF (qcg < qcgmin) THEN
!       IF (sqc(k) > qcgmin) THEN
!          gfluxm=grav_settling2*gno*sqc(k)**gpw
!       ELSE
!          gfluxm=0.
!       ENDIF
!    ELSE
!       gfluxm=grav_settling2*gno*(qcg/(1.+qcg))**gpw
!    ENDIF
!and replace with vdfg1 is computed in module_sf_fogdes.F.
    IF (sqc(k) > qcgmin) THEN
       !gfluxm=grav_settling2*gno*sqc(k)**gpw
       gfluxm=grav_settling2*sqc(k)*vdfg1
    ELSE
       gfluxm=0.
    ENDIF
!JOE-end

    IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
       gfluxp=grav_settling2*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
    ELSE
       gfluxp=0.
    ENDIF

    rhs=-xlvcp/exner(k)&
         &*( &
         (gfluxp - gfluxm)/dz(k)&
          & ) + tcd(k)

    d(1)=thl(k)+dtz(k)*flt+rhs*delt
    
    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) 
       c(kk)=-dtz(k)*dfh(k+1)

       IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
          gfluxp=grav_settling2*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
       ELSE
          gfluxp=0.
       ENDIF
       
       IF (.5*(sqc(k-1)+sqc(k)) > qcgmin) THEN
          gfluxm=grav_settling2*gno*(.5*(sqc(k-1)+sqc(k)))**gpw
       ELSE
          gfluxm=0.
       ENDIF

       rhs=-xlvcp/exner(k)&
            &*( &
            &(gfluxp - gfluxm)/dz(k)&
            & ) + tcd(k)
       d(kk)=thl(k)+rhs*delt
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

!!============================================
!! total water (sqw = sqc + sqv)
!!============================================

    k=kts
  
    a(1)=0.
    b(1)=1.+dtz(k)*dfh(k+1)
    c(1)=-dtz(k)*dfh(k+1)
    
!JOE: replace orig code with fogdep
!    IF (qcg < qcgmin) THEN
!       IF (sqc(k) > qcgmin) THEN
!          gfluxm=grav_settling2*gno*sqc(k)**gpw
!       ELSE
!          gfluxm=0.
!       ENDIF
!    ELSE
!       gfluxm=grav_settling2*gno*(qcg/(1.+qcg))**gpw
!    ENDIF
!and replace with fogdes code + remove use of qcg:
    IF (sqc(k) > qcgmin) THEN
       !gfluxm=grav_settling2*gno*(.5*(sqc(k)+sqc(k)))**gpw
       gfluxm=grav_settling2*sqc(k)*vdfg1
    ELSE
       gfluxm=0.
    ENDIF
!JOE-end
    
    IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
       gfluxp=grav_settling2*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
    ELSE
       gfluxp=0.
    ENDIF

    rhs=&
         &( &
         &(gfluxp - gfluxm)/dz(k)& 
        & ) + qcd(k)

    d(1)=sqw(k)+dtz(k)*flq+rhs*delt
    
    DO k=kts+1,kte-1
       kk=k-kts+1
       a(kk)=-dtz(k)*dfh(k)
       b(kk)=1.+dtz(k)*(dfh(k)+dfh(k+1)) 
       c(kk)=-dtz(k)*dfh(k+1)

       IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
          gfluxp=grav_settling2*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
       ELSE
          gfluxp=0.
       ENDIF

       IF (.5*(sqc(k-1)+sqc(k)) > qcgmin) THEN
          gfluxm=grav_settling2*gno*(.5*(sqc(k-1)+sqc(k)))**gpw
       ELSE
          gfluxm=0.
       ENDIF

       rhs=&
            &( &
            &(gfluxp - gfluxm)/dz(k)&
            & ) + qcd(k)
       d(kk)=sqw(k) + rhs*delt
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
       sqw(k)=d(k-kts+1)
    ENDDO

!!============================================
!! cloud water (sqc) - gravitational settling only
!!============================================

    k=kts

!JOE: replace with fogdes code + remove use of qcg:
    IF (sqc(k) > qcgmin) THEN
       !gfluxm=grav_settling2*gno*(.5*(sqc(k)+sqc(k)))**gpw
       gfluxm=grav_settling2*sqc(k)*vdfg1
    ELSE
       gfluxm=0.
    ENDIF
!JOE-end

    IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
       gfluxp=grav_settling2*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
    ELSE
       gfluxp=0.
    ENDIF

    dqc(k)=(gfluxp - gfluxm)/dz(k)*delt

    DO k=kts+1,kte-1

       IF (.5*(sqc(k+1)+sqc(k)) > qcgmin) THEN
          gfluxp=grav_settling2*gno*(.5*(sqc(k+1)+sqc(k)))**gpw
       ELSE
          gfluxp=0.
       ENDIF

       IF (.5*(sqc(k-1)+sqc(k)) > qcgmin) THEN
          gfluxm=grav_settling2*gno*(.5*(sqc(k-1)+sqc(k)))**gpw
       ELSE
          gfluxm=0.
       ENDIF

       dqc(k)=(gfluxp - gfluxm)/dz(k)*delt

    ENDDO

    dqc(kte)=0.

    DO k=kts,kte
       sqc(k)=sqc(k) + dqc(k)
    ENDDO

!!============================================
!! convert to mixing ratios for wrf
!!============================================

    DO k=kts,kte
       !sqw(k)=d(k-kts+1)
       sqv(k)=sqw(k)-sqc(k)
       Dqv(k)=(sqv(k)/(1.-sqv(k))-qv(k))/delt
       !qc tendency is now computed in module_bl_fogdes.F
       !Dqc(k)=(sqc(k)/(1.-sqc(k))-qc(k))/delt
       Dqc(k)=0.
       Dth(k)=(thl(k)+xlvcp/exner(k)*sqc(k)-th(k))/delt
    ENDDO

  END SUBROUTINE mynn_tendencies

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
       K_q(k)=dfq(k)*dzk
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
  SUBROUTINE mynn_bl_driver(&
       &initflag,&
       &grav_settling,&
       &delt,&
       &dz,&
       &u,v,th,qv,qc,&
       &p,exner,rho,&
       &xland,ts,qsfc,qcg,ps,&
       &ust,ch,hfx,qfx,rmol,wspd,&
       &uoce,voce,&
       &vdfg,&                          !Katata-added for fog dep
       &Qke,&
       &qke_adv,bl_mynn_tkeadvect,&     !ACF for QKE advection
       &Tsq,Qsq,Cov,&
       &Du,Dv,Dth,&
       &Dqv,Dqc,&
!       &K_m,K_h,K_q&
       &K_h,k_m,&
       &Pblh&
       &,el_pbl&
       &,dqke,qWT,qSHEAR,qBUOY,qDISS    & !JOE-TKE BUDGET
       &,bl_mynn_tkebudget              & !JOE-TKE BUDGET
       &,bl_mynn_cloudpdf,Sh3D          & !JOE-cloudPDF testing
       &,IDS,IDE,JDS,JDE,KDS,KDE        &
       &,IMS,IME,JMS,JME,KMS,KME        &
       &,ITS,ITE,JTS,JTE,KTS,KTE)
    
!-------------------------------------------------------------------

    INTEGER, INTENT(in) :: initflag
    !INPUT NAMELIST OPTIONS:
    INTEGER, INTENT(in) :: grav_settling
    INTEGER, INTENT(in) :: bl_mynn_tkebudget
    INTEGER, INTENT(in) :: bl_mynn_cloudpdf
    LOGICAL, INTENT(IN) :: bl_mynn_tkeadvect
    
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
    
    REAL, INTENT(in) :: delt
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(in) :: dz,&
         &u,v,th,qv,qc,p,exner,rho 
    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(in) :: xland,ust,&
!         &ch,rmol,ts,qsfc,qcg,ps,hfx,qfx, wspd,uoce,voce
!Katata-added for extra in-output
         &ch,rmol,ts,qsfc,qcg,ps,hfx,qfx, wspd,uoce,voce, vdfg
!Katata-end

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &Qke,Tsq,Qsq,Cov, &
!ACF for QKE advection
         &qke_adv
!ACF-end

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &Du,Dv,Dth,Dqv,Dqc

    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &K_h,K_m

    REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(inout) :: &
         &Pblh
    
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(inout) :: &
         &el_pbl

!JOE-TKE BUDGET
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(out) :: &
         &qWT,qSHEAR,qBUOY,qDISS,dqke
    ! 3D budget arrays are not allocated when bl_mynn_tkebudget == 0.
    ! 1D (local) budget arrays are used for passing between subroutines.
    REAL, DIMENSION(KTS:KTE) :: qWT1,qSHEAR1,qBUOY1,qDISS1,dqke1
!JOE-end
    REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: K_q,Sh3D

!local vars
    INTEGER :: ITF,JTF,KTF, IMD,JMD
    INTEGER :: i,j,k
    REAL, DIMENSION(KTS:KTE) :: thl,sqv,sqc,sqw,&
         &El, Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, Vt, Vq

    REAL, DIMENSION(KTS:KTE) :: thetav,sh,u1,v1,p1,ex1,dz1,th1,qke1, &
           & tsq1,qsq1,cov1,qv1,qc1,du1,dv1,dth1,dqv1,dqc1, &
           & k_m1,k_h1,k_q1

    REAL, DIMENSION(KTS:KTE+1) :: zw
    
    REAL :: cpm,sqcg,flt,flq,pmz,phh,exnerg,zet
    
    INTEGER, SAVE :: levflag

!***  Begin debugging
    IMD=(IMS+IME)/2
    JMD=(JMS+JME)/2
!***  End debugging 

    JTF=MIN0(JTE,JDE-1)
    ITF=MIN0(ITE,IDE-1)
    KTF=MIN0(KTE,KDE-1)

    levflag=mynn_level

    IF (initflag > 0) THEN
 
       Sh3D(its:ite,kts:kte,jts:jte)=0.
       el_pbl(its:ite,kts:kte,jts:jte)=0.
       tsq(its:ite,kts:kte,jts:jte)=0.
       qsq(its:ite,kts:kte,jts:jte)=0.
       cov(its:ite,kts:kte,jts:jte)=0.

       DO j=JTS,JTF
          DO i=ITS,ITF
             DO k=KTS,KTF
                dz1(k)=dz(i,k,j)
                u1(k) = u(i,k,j)
                v1(k) = v(i,k,j)
                sqc(k)=qc(i,k,j)/(1.+qc(i,k,j))
                sqv(k)=qv(i,k,j)/(1.+qv(i,k,j))
                sqw(k)=sqv(k)+sqc(k)
                th1(k)=th(i,k,j)
                thetav(k)=th(i,k,j)*(1.+0.61*sqv(k))
                thl(k)=th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)

                IF (k==kts) THEN
                   zw(k)=0.
                ELSE
                   zw(k)=zw(k-1)+dz(i,k-1,j)
                ENDIF

                k_m(i,k,j)=0.
                k_h(i,k,j)=0.
                k_q(i,k,j)=0.
                qke(i,k,j)=0.1-MIN(zw(k)*0.001, 0.0)
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
               &  Qke1,zw,dz1,xland(i,j))

             CALL mym_initialize ( kts,kte,&
                  &dz1, zw, u1, v1, thl, sqv,&
                  &PBLH(i,j),th1,&                      !JOE-BouLac mod
                  &sh,&                                 !JOE-cloudPDF mod
                  &ust(i,j), rmol(i,j),&
                  &el, Qke1, Tsq1, Qsq1, Cov1)

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
!ACF,JOE-end                
             ENDDO

!***  Begin debugging
!             k=kdebug
!             IF(I==IMD .AND. J==JMD)THEN
!               PRINT*,"MYNN DRIVER INIT: k=",1," sh=",sh(k)
!               PRINT*," sqw=",sqw(k)," thl=",thl(k)," k_m=",k_m(i,k,j)
!               PRINT*," xland=",xland(i,j)," rmol=",rmol(i,j)," ust=",ust(i,j)
!               PRINT*," qke=",qke(i,k,j)," el=",el_pbl(i,k,j)," tsq=",Tsq(i,k,j)
!               PRINT*," PBLH=",PBLH(i,j)," u=",u(i,k,j)," v=",v(i,k,j)
!             ENDIF
!***  End debugging

          ENDDO
       ENDDO

    ENDIF ! end initflag

!ACF copy qke_adv array into qke if using advection
    IF (bl_mynn_tkeadvect) THEN
       qke=qke_adv
    ENDIF
!ACF-end

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
             th1(k)= th(i,k,j)
             qv1(k)= qv(i,k,j)
             qc1(k)= qc(i,k,j)
             sqv(k)= qv(i,k,j)/(1.+qv(i,k,j))
             sqc(k)= qc(i,k,j)/(1.+qc(i,k,j))
             sqw(k)= sqv(k)+sqc(k)
             thl(k)= th(i,k,j)-xlvcp/exner(i,k,j)*sqc(k)
             thetav(k)=th(i,k,j)*(1.+0.608*sqv(k))
             p1(k) = p(i,k,j)
             ex1(k)= exner(i,k,j)
             el(k) = el_pbl(i,k,j)
             qke1(k)=qke(i,k,j)
             sh(k) = sh3d(i,k,j)
             tsq1(k)=tsq(i,k,j)
             qsq1(k)=qsq(i,k,j)
             cov1(k)=cov(i,k,j)

             IF (k==kts) THEN
                zw(k)=0.
             ELSE
                zw(k)=zw(k-1)+dz(i,k-1,j)
             ENDIF
          ENDDO

          zw(kte+1)=zw(kte)+dz(i,kte,j)          
          
          CALL GET_PBLH(KTS,KTE,PBLH(i,j),thetav,&
          & Qke1,zw,dz1,xland(i,j))
          
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

          zet = 0.5*dz(i,kts,j)*rmol(i,j)
          if ( zet >= 0.0 ) then
            pmz = 1.0 + (cphm_st-1.0) * zet
            phh = 1.0 +  cphh_st      * zet
          else
            pmz = 1.0/    (1.0-cphm_unst*zet)**0.25 - zet
            phh = 1.0/SQRT(1.0-cphh_unst*zet)
          end if
!!!!!

          CALL  mym_condensation ( kts,kte,&
               &dz1,thl,sqw,p1,ex1, &
               &tsq1, qsq1, cov1, &
               &Sh,el,bl_mynn_cloudpdf, & !JOE-added for cloud PDF testing (from Kuwano-Yoshida et al. 2010)
               &Vt, Vq)

          CALL mym_turbulence ( kts,kte,levflag, &
               &dz1, zw, u1, v1, thl, sqc, sqw, &
               &qke1, tsq1, qsq1, cov1, &
               &vt, vq,&
               &rmol(i,j), flt, flq, &
               &PBLH(i,j),th1,&                  !JOE-BouLac mod
               &Sh,&                             !JOE-cloudPDF mod
               &el,&
               &Dfm,Dfh,Dfq, &
               &Tcd,Qcd,Pdk, &
               &Pdt,Pdq,Pdc &
               &,qWT1,qSHEAR1,qBUOY1,qDISS1  &   !JOE-TKE BUDGET
               &,bl_mynn_tkebudget           &   !JOE-TKE BUDGET
               &)

          CALL mym_predict (kts,kte,levflag,  &
               &delt, dz1, &
               &ust(i,j), flt, flq, pmz, phh, &
               &el, dfq, pdk, pdt, pdq, pdc, &
               &Qke1, Tsq1, Qsq1, Cov1)

          CALL mynn_tendencies(kts,kte,&
               &levflag,grav_settling,&
               &delt, dz1,&
               &u1, v1, th1, qv1, qc1,&
               &p1, ex1, thl, sqv, sqc, sqw,&
               &ust(i,j),flt,flq,wspd(i,j),qcg(i,j),&
               &uoce(i,j),voce(i,j),&
               &tsq1, qsq1, cov1,&
               &tcd, qcd, &
               &dfm, dfh, dfq,&
               &Du1, Dv1, Dth1, Dqv1, Dqc1&
               &,vdfg(i,j)&                 !JOE/Katata- fog deposition
               &)

          CALL retrieve_exchange_coeffs(kts,kte,&
               &dfm, dfh, dfq, dz1,&
               &K_m1, K_h1, K_q1)

          !UPDATE 3D ARRAYS
          DO k=KTS,KTF
             K_m(i,k,j)=K_m1(k)
             K_h(i,k,j)=K_h1(k)
             K_q(i,k,j)=K_q1(k)
             du(i,k,j)=du1(k)
             dv(i,k,j)=dv1(k)
             dth(i,k,j)=dth1(k)
             dqv(i,k,j)=dqv1(k)
             dqc(i,k,j)=dqc1(k)
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
             !***  Begin debugging
!             IF ( sh(k) < 0. .OR. sh(k)> 200. .OR. &
!                & qke(i,k,j) < -5. .OR. qke(i,k,j)> 200. .OR. &
!                & el_pbl(i,k,j) < 0. .OR. el_pbl(i,k,j)> 2000. .OR. &
!                & ABS(vt(k)) > 0.8 .OR. ABS(vq(k)) > 1100. .OR. &
!                & k_m(i,k,j) < 0. .OR. k_m(i,k,j)> 2000. .OR. &
!                & vdfg(i,j) < 0. .OR. vdfg(i,j)>5. ) THEN
!                  PRINT*,"SUSPICIOUS VALUES AT: k=",k," sh=",sh(k)
!                  PRINT*," sqw=",sqw(k)," thl=",thl(k)," k_m=",k_m(i,k,j)
!                  PRINT*," xland=",xland(i,j)," rmol=",rmol(i,j)," ust=",ust(i,j)
!                  PRINT*," qke=",qke(i,k,j)," el=",el_pbl(i,k,j)," tsq=",tsq(i,k,j)
!                  PRINT*," PBLH=",PBLH(i,j)," u=",u(i,k,j)," v=",v(i,k,j)
!                  PRINT*," vq=",vq(k)," vt=",vt(k)," vdfg=",vdfg(i,j)
!             ENDIF
             !***  End debugging
          ENDDO
!JOE-end addition

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
  SUBROUTINE mynn_bl_init_driver(&
       &Du,Dv,Dth,&
       &Dqv,Dqc&
       &,RESTART,ALLOWED_TO_READ,LEVEL&
       &,IDS,IDE,JDS,JDE,KDS,KDE                    &
       &,IMS,IME,JMS,JME,KMS,KME                    &
       &,ITS,ITE,JTS,JTE,KTS,KTE)

    !---------------------------------------------------------------
    LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
    INTEGER,INTENT(IN) :: LEVEL

    INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
         &                IMS,IME,JMS,JME,KMS,KME,                    &
         &                ITS,ITE,JTS,JTE,KTS,KTE
    
    
    REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: &
         &Du,Dv,Dth,Dqv,Dqc

    INTEGER :: I,J,K,ITF,JTF,KTF
    
    JTF=MIN0(JTE,JDE-1)
    KTF=MIN0(KTE,KDE-1)
    ITF=MIN0(ITE,IDE-1)
    
    IF(.NOT.RESTART)THEN
       DO J=JTS,JTF
          DO K=KTS,KTF
             DO I=ITS,ITF
                Du(i,k,j)=0.
                Dv(i,k,j)=0.
                Dth(i,k,j)=0.
                Dqv(i,k,j)=0.
                Dqc(i,k,j)=0.
             ENDDO
          ENDDO
       ENDDO
    ENDIF

    mynn_level=level

  END SUBROUTINE mynn_bl_init_driver

! ==================================================================

  SUBROUTINE GET_PBLH(KTS,KTE,zi,thetav1D,qke1D,zw1D,dz1D,landsea)

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
    REAL, PARAMETER :: sbl_lim  = 200. !typical scale of stable BL (m).
    REAL, PARAMETER :: sbl_damp = 400. !transition length for blending (m).
    INTEGER :: I,J,K,kthv,ktke

    !FIND MAX TKE AND MIN THETAV IN THE LOWEST 500 M
    k = kts+1
    kthv = 1
    ktke = 1
    maxqke = 0.
    minthv = 9.E9
    DO WHILE (zw1D(k) .LE. 500.)
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
    !TKEeps = maxtke/20. = maxqke/40.
    TKEeps = maxqke/40. 
    TKEeps = MAX(TKEeps,0.025)

    !FIND THETAV-BASED PBLH (BEST FOR DAYTIME).
    zi=0.
    k = kthv+1
    IF((landsea-1.5).GE.0)THEN                                            
        ! WATER
        delt_thv = 0.75
    ELSE         
        ! LAND     
        delt_thv = 1.5  
    ENDIF

    zi=0.
    k = kthv+1
    DO WHILE (zi .EQ. 0.) 
       IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
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
    !so an artificial limit is imposed to not let PBLH_TKE exceed 4km.
    !This has no impact on 98-99% of the domain, but is the simplest patch
    !that adequately addresses these extremely large PBLHs.
    PBLH_TKE = MIN(PBLH_TKE,4000.)

    !BLEND THE TWO PBLH TYPES HERE: 
    wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
    zi=PBLH_TKE*(1.-wt) + zi*wt

  END SUBROUTINE GET_PBLH
  
! ==================================================================

END MODULE module_bl_mynn
