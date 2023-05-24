!>\file module_bl_mynn.F90
!! This file contains the entity of MYNN-EDMF PBL scheme.
! **********************************************************************
! *   An improved Mellor-Yamada turbulence closure model               *
! *                                                                    *
! *      Original author: M. Nakanishi (N.D.A), naka@nda.ac.jp         *
! *      Translated into F90 and implemented in WRF-ARW by:            *
! *                       Mariusz Pagowski (NOAA-GSL)                  *
! *      Subsequently developed by:                                    *
! *                 Joseph Olson, Jaymes Kenyon (NOAA/GSL),            *
! *                 Wayne Angevine (NOAA/CSL), Kay Suselj (NASA/JPL),  *
! *                 Franciano Puhales (UFSM), Laura Fowler (NCAR),     *
! *                 Elynn Wu (UCSD), and Jordan Schnell (NOAA/GSL)     *
! *                                                                    *
! *   Contents:                                                        *
! *                                                                    *
! *   mynn_bl_driver - main subroutine which calls all other routines  *
! *   --------------                                                   *
! *     1. mym_initialize  (to be called once initially)               *
! *        gives the closure constants and initializes the turbulent   *
! *        quantities.                                                 *
! *     2. get_pblh                                                    *
! *        Calculates the boundary layer height                        *
! *     3. scale_aware                                                 *
! *        Calculates scale-adaptive tapering functions                *
! *     4. mym_condensation                                            *
! *        determines the liquid water content and the cloud fraction  *
! *        diagnostically.                                             *
! *     5. dmp_mf                                                      *
! *        Calls the (nonlocal) mass-flux component                    *
! *     6. ddmf_jpl                                                    *
! *        Calls the downdraft mass-flux component                     *
! *    (-) mym_level2      (called in the other subroutines)           *
! *        calculates the stability functions at Level 2.              *
! *    (-) mym_length      (called in the other subroutines)           *
! *        calculates the master length scale.                         *
! *     7. mym_turbulence                                              *
! *        calculates the vertical diffusivity coefficients and the    *
! *        production terms for the turbulent quantities.              *
! *     8. mym_predict                                                 *
! *        predicts the turbulent quantities at the next step.         *
! *                                                                    *
! *             call mym_initialize                                    *
! *                  |                                                 *
! *                  |<----------------+                               *
! *                  |                 |                               *
! *             call get_pblh          |                               *
! *             call scale_aware       |                               *
! *             call mym_condensation  |                               *
! *             call dmp_mf            |                               *
! *             call ddmf_jpl          |                               *
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
! *     qke    : 2 * TKE                                               *
! *     el     : mixing length                                         *
! *                                                                    *
! *     If the water contents are unnecessary, e.g., in the case of    *
! *     ocean models, thl is the potential temperature and qw, ql, vt  *
! *     and vq are all zero.                                           *
! *                                                                    *
! *   Grid arrangement:                                                *
! *             k+1 +---------+                                        *
! *                 |         |     i = 1 - nx                         *
! *             (k) |    *    |     k = 1 - nz                         *
! *                 |         |                                        *
! *              k  +---------+                                        *
! *                 i   (i)  i+1                                       *
! *                                                                    *
! *     All the predicted variables are defined at the center (*) of   *
! *     the grid boxes. The diffusivity coefficients and two of their  *
! *     components (el and stability functions sh & sm) are, however,  *
! *     defined on the walls of the grid boxes.                        *
! *     # Upper boundary values are given at k=nz.                     *
! *                                                                    *
! *   References:                                                      *
! *     1. Nakanishi, M., 2001:                                        *
! *        Boundary-Layer Meteor., 99, 349-378.                        *
! *     2. Nakanishi, M. and H. Niino, 2004:                           *
! *        Boundary-Layer Meteor., 112, 1-31.                          *
! *     3. Nakanishi, M. and H. Niino, 2006:                           *
! *        Boundary-Layer Meteor., 119, 397-407.                       *
! *     4. Nakanishi, M. and H. Niino, 2009:                           *
! *        Jour. Meteor. Soc. Japan, 87, 895-912.                      *
! *     5. Olson J. and coauthors, 2019: A description of the          *
! *        MYNN-EDMF scheme and coupling to other components in        *
! *        WRF-ARW. NOAA Tech. Memo. OAR GSD, 61, 37 pp.,              *
! *        https://doi.org/10.25923/n9wm-be49.                         * 
! *     6. Puhales, Franciano S. and coauthors, 2020: Turbulent        *
! *        Kinetic Energy Budget for MYNN-EDMF PBL Scheme in WRF model.*
! *        Universidade Federal de Santa Maria Technical Note. 9 pp.   *
! **********************************************************************
! ==================================================================
! Notes on original implementation into WRF-ARW
! changes to original code:
! 1. code is 1D (in z)
! 2. option to advect TKE, but not the covariances and variances
! 3. Cranck-Nicholson replaced with the implicit scheme
! 4. removed terrain-dependent grid since input in WRF in actual
!    distances in z[m]
! 5. cosmetic changes to adhere to WRF standard (remove common blocks,
!            intent etc)
!-------------------------------------------------------------------
! Further modifications post-implementation
!
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
! v4.5 / CCPP
!            This version includes many modifications that proved valuable in the global
!            framework and removes some key lingering bugs in the mixing of chemical species.
!            TKE Budget output fixed (Puhales, 2020-12)
!            New option for stability function: (Puhales, 2020-12)
!                bl_mynn_stfunc = 0 (original, Kansas-type function, Paulson, 1970 )
!                bl_mynn_stfunc = 1 (expanded range, same as used for Jimenez et al (MWR)
!                see the Technical Note for this implementation (small impact).
!            Improved conservation of momentum and higher-order moments.
!            Important bug fixes for mixing of chemical species.
!            Addition of pressure-gradient effects on updraft momentum transport.
!            Addition of bl_mynn_closure option = 2.5, 2.6, or 3.0
!            Addition of higher-order moments for sigma when using 
!                bl_mynn_cloudpdf = 2 (Chab-Becht).
!            Removed WRF_CHEM dependencies.
!            Many miscellaneous tweaks.
!
! Many of these changes are now documented in references listed above.
!====================================================================

MODULE module_bl_mynn

  use module_bl_mynn_common,only: &
        cp        , cpv       , cliq       , cice      , &
        p608      , ep_2      , ep_3       , gtr       , &
        grav      , g_inv     , karman     , p1000mb   , &
        rcp       , r_d       , r_v        , rk        , &
        rvovrd    , svp1      , svp2       , svp3      , &
        xlf       , xlv       , xls        , xlscp     , &
        xlvcp     , tv0       , tv1        , tref      , &
        zero      , half      , one        , two       , &
        onethird  , twothirds , tkmin      , t0c       , &
        tice      , kind_phys


  IMPLICIT NONE

!===================================================================
! From here on, these are MYNN-specific parameters:
! The parameters below depend on stability functions of module_sf_mynn.
  REAL, PARAMETER :: cphm_st=5.0, cphm_unst=16.0, &
                     cphh_st=5.0, cphh_unst=16.0

! Closure constants
  REAL, PARAMETER ::  &
       &pr  =  0.74,  &
       &g1  =  0.235, &  ! NN2009 = 0.235
       &b1  = 24.0,   &
       &b2  = 15.0,   &  ! CKmod     NN2009
       &c2  =  0.729, &  ! 0.729, & !0.75, &
       &c3  =  0.340, &  ! 0.340, & !0.352, &
       &c4  =  0.0,   &
       &c5  =  0.2,   &
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
!      &cns=3.5, alp1=0.23, alp2=0.3, alp3=3.0, alp4=10.0, alp5=0.2

  REAL, PARAMETER :: gpw=5./3., qcgmin=1.e-8, qkemin=1.e-12
  REAL, PARAMETER :: tliq = 269. !all hydrometeors are liquid when T > tliq

! Constants for cloud PDF (mym_condensation)
  REAL, PARAMETER :: rr2=0.7071068, rrp=0.3989423

  !>Use Canuto/Kitamura mod (remove Ric and negative TKE) (1:yes, 0:no)
  !!For more info, see Canuto et al. (2008 JAS) and Kitamura (Journal of the 
  !!Meteorological Society of Japan, Vol. 88, No. 5, pp. 857-864, 2010).
  !!Note that this change required further modification of other parameters
  !!above (c2, c3). If you want to remove this option, set c2 and c3 constants 
  !!(above) back to NN2009 values (see commented out lines next to the
  !!parameters above). This only removes the negative TKE problem
  !!but does not necessarily improve performance - neutral impact.
  REAL, PARAMETER :: CKmod=1.

  !>Use Ito et al. (2015, BLM) scale-aware (0: no, 1: yes). Note that this also has impacts
  !!on the cloud PDF and mass-flux scheme, using Honnert et al. (2011) similarity function
  !!for TKE in the upper PBL/cloud layer.
  REAL, PARAMETER :: scaleaware=1.

  !>Of the following the options, use one OR the other, not both.
  !>Adding top-down diffusion driven by cloud-top radiative cooling
  INTEGER, PARAMETER :: bl_mynn_topdown = 0
  !>Option to activate downdrafts, from Elynn Wu (0: deactive, 1: active)
  INTEGER, PARAMETER :: bl_mynn_edmf_dd = 0

  !>Option to activate heating due to dissipation of TKE (to activate, set to 1.0)
  INTEGER, PARAMETER :: dheat_opt = 1

  !Option to activate environmental subsidence in mass-flux scheme
  LOGICAL, PARAMETER :: env_subs = .false.

  !Option to switch flux-profile relationship for surface (from Puhales et al. 2020)
  !0: use original Dyer-Hicks, 1: use Cheng-Brustaert and Blended COARE
  INTEGER, PARAMETER :: bl_mynn_stfunc = 1

  !option to print out more stuff for debugging purposes
  LOGICAL, PARAMETER :: debug_code = .false.
  INTEGER, PARAMETER :: idbg = 23 !specific i-point to write out

! JAYMES-
!> Constants used for empirical calculations of saturation
!! vapor pressures (in function "esat") and saturation mixing ratios
!! (in function "qsat"), reproduced from module_mp_thompson.F, 
!! v3.6 
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

  ! Used in WRF-ARW module_physics_init.F
  INTEGER :: mynn_level


CONTAINS

! ==================================================================
!>\ingroup gsd_mynn_edmf
!! This subroutine is the GSD MYNN-EDNF PBL driver routine,which
!! encompassed the majority of the subroutines that comprise the 
!! procedures that ultimately solve for tendencies of 
!! \f$U, V, \theta, q_v, q_c, and q_i\f$.
!!\section gen_mynn_bl_driver GSD mynn_bl_driver General Algorithm
!> @{
  SUBROUTINE mynn_bl_driver(            &
       &initflag,restart,cycling,       &
       &delt,dz,dx,znt,                 &
       &u,v,w,th,sqv3d,sqc3d,sqi3d,     &
       &qnc,qni,                        &
       &qnwfa,qnifa,qnbca,ozone,        &
       &p,exner,rho,t3d,                &
       &xland,ts,qsfc,ps,               &
       &ust,ch,hfx,qfx,rmol,wspd,       &
       &uoce,voce,                      & !ocean current
       &qke,qke_adv,                    &
       &sh3d,sm3d,                      &
       &nchem,kdvel,ndvel,              & !smoke/chem variables
       &chem3d,vdep,                    &
       &frp,emis_ant_no,                &
       &mix_chem,enh_mix,               & !note: these arrays/flags are still under development
       &rrfs_sd,smoke_dbg,              & !end smoke/chem variables
       &tsq,qsq,cov,                    &
       &rublten,rvblten,rthblten,       &
       &rqvblten,rqcblten,rqiblten,     &
       &rqncblten,rqniblten,            &
       &rqnwfablten,rqnifablten,        &
       &rqnbcablten,dozone,             &
       &exch_h,exch_m,                  &
       &pblh,kpbl,                      & 
       &el_pbl,                         &
       &dqke,qwt,qshear,qbuoy,qdiss,    &
       &qc_bl,qi_bl,cldfra_bl,          &
       &bl_mynn_tkeadvect,              &
       &tke_budget,                     &
       &bl_mynn_cloudpdf,               &
       &bl_mynn_mixlength,              &
       &icloud_bl,                      &
       &closure,                        &
       &bl_mynn_edmf,                   &
       &bl_mynn_edmf_mom,               &
       &bl_mynn_edmf_tke,               &
       &bl_mynn_mixscalars,             &
       &bl_mynn_output,                 &
       &bl_mynn_cloudmix,bl_mynn_mixqt, &
       &edmf_a,edmf_w,edmf_qt,          &
       &edmf_thl,edmf_ent,edmf_qc,      &
       &sub_thl3D,sub_sqv3D,            &
       &det_thl3D,det_sqv3D,            &
       &nupdraft,maxMF,ktop_plume,      &
       &spp_pbl,pattern_spp_pbl,        &
       &rthraten,                       &
       &FLAG_QC,FLAG_QI,FLAG_QNC,       &
       &FLAG_QNI,FLAG_QNWFA,FLAG_QNIFA, &
       &FLAG_QNBCA,                     &
       &IDS,IDE,JDS,JDE,KDS,KDE,        &
       &IMS,IME,JMS,JME,KMS,KME,        &
       &ITS,ITE,JTS,JTE,KTS,KTE         )
    
!-------------------------------------------------------------------

    INTEGER, INTENT(in) :: initflag
    !INPUT NAMELIST OPTIONS:
    LOGICAL, INTENT(in) :: restart,cycling
    INTEGER, INTENT(in) :: tke_budget
    INTEGER, INTENT(in) :: bl_mynn_cloudpdf
    INTEGER, INTENT(in) :: bl_mynn_mixlength
    INTEGER, INTENT(in) :: bl_mynn_edmf
    LOGICAL, INTENT(in) :: bl_mynn_tkeadvect
    INTEGER, INTENT(in) :: bl_mynn_edmf_mom
    INTEGER, INTENT(in) :: bl_mynn_edmf_tke
    INTEGER, INTENT(in) :: bl_mynn_mixscalars
    INTEGER, INTENT(in) :: bl_mynn_output
    INTEGER, INTENT(in) :: bl_mynn_cloudmix
    INTEGER, INTENT(in) :: bl_mynn_mixqt
    INTEGER, INTENT(in) :: icloud_bl
    REAL,    INTENT(in) :: closure

    LOGICAL, INTENT(in) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC,&
                           FLAG_QNWFA,FLAG_QNIFA,FLAG_QNBCA

    LOGICAL, INTENT(IN) :: mix_chem,enh_mix,rrfs_sd,smoke_dbg

    INTEGER, INTENT(in) :: &
         & IDS,IDE,JDS,JDE,KDS,KDE &
         &,IMS,IME,JMS,JME,KMS,KME &
         &,ITS,ITE,JTS,JTE,KTS,KTE

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

! initflag > 0  for TRUE
! else        for FALSE
!       closure       : <= 2.5;  Level 2.5
!                  2.5< and <3;  Level 2.6
!                        =   3;  Level 3
    
    REAL, INTENT(in) :: delt
    REAL, DIMENSION(IMS:IME), INTENT(in) :: dx
    REAL, DIMENSION(IMS:IME,KMS:KME), INTENT(in) :: dz,      &
         &u,v,w,th,sqv3D,p,exner,rho,t3d
    REAL, DIMENSION(IMS:IME,KMS:KME), OPTIONAL, INTENT(in):: &
         &sqc3D,sqi3D,qni,qnc,qnwfa,qnifa,qnbca
    REAL, DIMENSION(IMS:IME,KMS:KME), OPTIONAL, INTENT(in):: ozone
    REAL, DIMENSION(IMS:IME), INTENT(in) :: xland,ust,       &
         &ch,ts,qsfc,ps,hfx,qfx,wspd,uoce,voce,znt

    REAL, DIMENSION(IMS:IME,KMS:KME), INTENT(inout) ::       &
         &qke,tsq,qsq,cov,qke_adv

    REAL, DIMENSION(IMS:IME,KMS:KME), INTENT(inout) ::       &
         &rublten,rvblten,rthblten,rqvblten,rqcblten,        &
         &rqiblten,rqniblten,rqncblten,                      &
         &rqnwfablten,rqnifablten,rqnbcablten
    REAL, DIMENSION(IMS:IME,KMS:KME), INTENT(inout) :: dozone

    REAL, DIMENSION(IMS:IME,KMS:KME), INTENT(in)    :: rthraten

    REAL, DIMENSION(IMS:IME,KMS:KME), INTENT(out)   ::       &
         &exch_h,exch_m

   !These 10 arrays are only allocated when bl_mynn_output > 0
   REAL, DIMENSION(IMS:IME,KMS:KME), OPTIONAL, INTENT(inout) :: &
         & edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc,  &
         & sub_thl3D,sub_sqv3D,det_thl3D,det_sqv3D

!   REAL, DIMENSION(IMS:IME,KMS:KME)   :: &
!         & edmf_a_dd,edmf_w_dd,edmf_qt_dd,edmf_thl_dd,edmf_ent_dd,edmf_qc_dd

    REAL, DIMENSION(IMS:IME), INTENT(inout) :: pblh,rmol

    REAL, DIMENSION(IMS:IME) :: psig_bl,psig_shcu

    INTEGER,DIMENSION(IMS:IME),INTENT(INOUT) ::             &
         &kpbl,nupdraft,ktop_plume

    REAL, DIMENSION(IMS:IME), INTENT(OUT) ::                &
         &maxmf

    REAL, DIMENSION(IMS:IME,KMS:KME), INTENT(inout) ::      &
         &el_pbl

    REAL, DIMENSION(IMS:IME,KMS:KME), optional, INTENT(out) :: &
         &qwt,qshear,qbuoy,qdiss,dqke
    ! 3D budget arrays are not allocated when tke_budget == 0
    ! 1D (local) budget arrays are used for passing between subroutines.
    REAL, DIMENSION(kts:kte) :: qwt1,qshear1,qbuoy1,qdiss1, &
         &dqke1,diss_heat

    REAL, DIMENSION(IMS:IME,KMS:KME), intent(out) :: Sh3D,Sm3D

    REAL, DIMENSION(IMS:IME,KMS:KME), INTENT(inout) ::      &
         &qc_bl,qi_bl,cldfra_bl
    REAL, DIMENSION(KTS:KTE) :: qc_bl1D,qi_bl1D,cldfra_bl1D,&
                    qc_bl1D_old,qi_bl1D_old,cldfra_bl1D_old

! smoke/chemical arrays
    INTEGER, INTENT(IN   ) ::   nchem, kdvel, ndvel
    REAL,    DIMENSION(ims:ime, kms:kme, nchem), INTENT(INOUT), optional :: chem3d
    REAL,    DIMENSION(ims:ime, ndvel),   INTENT(IN),   optional :: vdep
    REAL,    DIMENSION(ims:ime),     INTENT(IN),    optional :: frp,EMIS_ANT_NO
    !local
    REAL,    DIMENSION(kts:kte  ,nchem) :: chem1
    REAL,    DIMENSION(kts:kte+1,nchem) :: s_awchem1
    REAL,    DIMENSION(ndvel)           :: vd1
    INTEGER :: ic

!local vars
    INTEGER :: ITF,JTF,KTF, IMD,JMD
    INTEGER :: i,j,k,kproblem
    REAL, DIMENSION(KTS:KTE) :: thl,thvl,tl,qv1,qc1,qi1,sqw,&
         &el, dfm, dfh, dfq, tcd, qcd, pdk, pdt, pdq, pdc,  &
         &vt, vq, sgm, thlsg, sqwsg
    REAL, DIMENSION(KTS:KTE) :: thetav,sh,sm,u1,v1,w1,p1,   &
         &ex1,dz1,th1,tk1,rho1,qke1,tsq1,qsq1,cov1,         &
         &sqv,sqi,sqc,du1,dv1,dth1,dqv1,dqc1,dqi1,ozone1,   &
         &k_m1,k_h1,qni1,dqni1,qnc1,dqnc1,qnwfa1,qnifa1,    &
         &qnbca1,dqnwfa1,dqnifa1,dqnbca1,dozone1

    !mass-flux variables
    REAL, DIMENSION(KTS:KTE) :: dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf
    REAL, DIMENSION(KTS:KTE) :: edmf_a1,edmf_w1,edmf_qt1,   &
         &edmf_thl1,edmf_ent1,edmf_qc1
    REAL, DIMENSION(KTS:KTE) :: edmf_a_dd1,edmf_w_dd1,      &
         &edmf_qt_dd1,edmf_thl_dd1,                         &
         &edmf_ent_dd1,edmf_qc_dd1
    REAL, DIMENSION(KTS:KTE) :: sub_thl,sub_sqv,sub_u,sub_v,&
                        det_thl,det_sqv,det_sqc,det_u,det_v
    REAL,DIMENSION(KTS:KTE+1) :: s_aw1,s_awthl1,s_awqt1,    &
                  s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqke1,   &
                  s_awqnc1,s_awqni1,s_awqnwfa1,s_awqnifa1,  &
                  s_awqnbca1
    REAL,DIMENSION(KTS:KTE+1) :: sd_aw1,sd_awthl1,sd_awqt1, &
                  sd_awqv1,sd_awqc1,sd_awu1,sd_awv1,sd_awqke1

    REAL, DIMENSION(KTS:KTE+1) :: zw
    REAL :: cpm,sqcg,flt,fltv,flq,flqv,flqc,pmz,phh,exnerg,zet,phi_m,&
          & afk,abk,ts_decay, qc_bl2, qi_bl2,                        &
          & th_sfc,ztop_plume,sqc9,sqi9,wsp

    !top-down diffusion
    REAL, DIMENSION(ITS:ITE) :: maxKHtopdown
    REAL, DIMENSION(KTS:KTE) :: KHtopdown,TKEprodTD

    LOGICAL :: INITIALIZE_QKE,problem

    ! Stochastic fields 
    INTEGER,  INTENT(IN)                                     ::spp_pbl
    REAL, DIMENSION( ims:ime, kms:kme), INTENT(IN),OPTIONAL  ::pattern_spp_pbl
    REAL, DIMENSION(KTS:KTE)                                 ::rstoch_col

    ! Substepping TKE
    INTEGER :: nsub
    real    :: delt2


    if (debug_code) then !check incoming values
      do i=its,ite
        problem = .false.
        do k=kts,kte
          wsp  = sqrt(u(i,k)**2 + v(i,k)**2)
          if (abs(hfx(i)) > 1200. .or. abs(qfx(i)) > 0.001 .or.         &
              wsp > 200. .or. t3d(i,k) > 360. .or. t3d(i,k) < 160. .or. &
              sqv3d(i,k)< 0.0 .or. sqc3d(i,k)< 0.0 ) then
             kproblem = k
             problem = .true.
             print*,"Incoming problem at: i=",i," k=1"
             print*," QFX=",qfx(i)," HFX=",hfx(i)
             print*," wsp=",wsp," T=",t3d(i,k)
             print*," qv=",sqv3d(i,k)," qc=",sqc3d(i,k)
             print*," u*=",ust(i)," wspd=",wspd(i)
             print*," xland=",xland(i)," ts=",ts(i)
             print*," z/L=",0.5*dz(i,1)*rmol(i)," ps=",ps(i)
             print*," znt=",znt(i)," dx=",dx(i)
          endif
        enddo
        if (problem) then
          print*,"===tk:",t3d(i,max(kproblem-3,1):min(kproblem+3,kte))
          print*,"===qv:",sqv3d(i,max(kproblem-3,1):min(kproblem+3,kte))
          print*,"===qc:",sqc3d(i,max(kproblem-3,1):min(kproblem+3,kte))
          print*,"===qi:",sqi3d(i,max(kproblem-3,1):min(kproblem+3,kte))
          print*,"====u:",u(i,max(kproblem-3,1):min(kproblem+3,kte))
          print*,"====v:",v(i,max(kproblem-3,1):min(kproblem+3,kte))
        endif
      enddo
    endif

!***  Begin debugging
    IMD=(IMS+IME)/2
    JMD=(JMS+JME)/2
!***  End debugging 

    JTF=JTE
    ITF=ITE
    KTF=KTE

    IF (bl_mynn_output > 0) THEN !research mode
       edmf_a(its:ite,kts:kte)=0.
       edmf_w(its:ite,kts:kte)=0.
       edmf_qt(its:ite,kts:kte)=0.
       edmf_thl(its:ite,kts:kte)=0.
       edmf_ent(its:ite,kts:kte)=0.
       edmf_qc(its:ite,kts:kte)=0.
       sub_thl3D(its:ite,kts:kte)=0.
       sub_sqv3D(its:ite,kts:kte)=0.
       det_thl3D(its:ite,kts:kte)=0.
       det_sqv3D(its:ite,kts:kte)=0.

       !edmf_a_dd(its:ite,kts:kte)=0.
       !edmf_w_dd(its:ite,kts:kte)=0.
       !edmf_qt_dd(its:ite,kts:kte)=0.
       !edmf_thl_dd(its:ite,kts:kte)=0.
       !edmf_ent_dd(its:ite,kts:kte)=0.
       !edmf_qc_dd(its:ite,kts:kte)=0.
    ENDIF
    ktop_plume(its:ite)=0   !int
    nupdraft(its:ite)=0     !int
    maxmf(its:ite)=0.
    maxKHtopdown(its:ite)=0.

    ! DH* CHECK HOW MUCH OF THIS INIT IF-BLOCK IS ACTUALLY NEEDED FOR RESTARTS
!> - Within the MYNN-EDMF, there is a dependecy check for the first time step,
!! If true, a three-dimensional initialization loop is entered. Within this loop,
!! several arrays are initialized and k-oriented (vertical) subroutines are called 
!! at every i and j point, corresponding to the x- and y- directions, respectively.  
    IF (initflag > 0 .and. .not.restart) THEN

       !Test to see if we want to initialize qke
       IF ( (restart .or. cycling)) THEN
          IF (MAXVAL(QKE(its:ite,kts)) < 0.0002) THEN
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
 
       if (.not.restart .or. .not.cycling) THEN
         Sh3D(its:ite,kts:kte)=0.
         Sm3D(its:ite,kts:kte)=0.
         el_pbl(its:ite,kts:kte)=0.
         tsq(its:ite,kts:kte)=0.
         qsq(its:ite,kts:kte)=0.
         cov(its:ite,kts:kte)=0.
         cldfra_bl(its:ite,kts:kte)=0.
         qc_bl(its:ite,kts:kte)=0.
         qke(its:ite,kts:kte)=0.
       else
         qc_bl1D(kts:kte)=0.0
         qi_bl1D(kts:kte)=0.0
         cldfra_bl1D(kts:kte)=0.0
       end if
       dqc1(kts:kte)=0.0
       dqi1(kts:kte)=0.0
       dqni1(kts:kte)=0.0
       dqnc1(kts:kte)=0.0
       dqnwfa1(kts:kte)=0.0
       dqnifa1(kts:kte)=0.0
       dqnbca1(kts:kte)=0.0
       dozone1(kts:kte)=0.0
       qc_bl1D_old(kts:kte)=0.0
       cldfra_bl1D_old(kts:kte)=0.0
       edmf_a1(kts:kte)=0.0
       edmf_w1(kts:kte)=0.0
       edmf_qc1(kts:kte)=0.0
       edmf_a_dd1(kts:kte)=0.0
       edmf_w_dd1(kts:kte)=0.0
       edmf_qc_dd1(kts:kte)=0.0
       sgm(kts:kte)=0.0
       vt(kts:kte)=0.0
       vq(kts:kte)=0.0

       DO k=KTS,KTE
          DO i=ITS,ITF
             exch_m(i,k)=0.
             exch_h(i,k)=0.
          ENDDO
       ENDDO

       IF (tke_budget .eq. 1) THEN
          DO k=KTS,KTE
             DO i=ITS,ITF
                qWT(i,k)=0.
                qSHEAR(i,k)=0.
                qBUOY(i,k)=0.
                qDISS(i,k)=0.
                dqke(i,k)=0.
             ENDDO
          ENDDO
       ENDIF

       DO i=ITS,ITF
          DO k=KTS,KTE !KTF
                dz1(k)=dz(i,k)
                u1(k) = u(i,k)
                v1(k) = v(i,k)
                w1(k) = w(i,k)
                th1(k)=th(i,k)
                tk1(k)=T3D(i,k)
                ex1(k)=exner(i,k)
                rho1(k)=rho(i,k)
                sqc(k)=sqc3D(i,k) !/(1.+qv(i,k))
                sqv(k)=sqv3D(i,k) !/(1.+qv(i,k))
                thetav(k)=th(i,k)*(1.+0.608*sqv(k))
                IF (icloud_bl > 0) THEN
                   CLDFRA_BL1D(k)=CLDFRA_BL(i,k)
                   QC_BL1D(k)=QC_BL(i,k)
                   QI_BL1D(k)=QI_BL(i,k)
                ENDIF
                IF (FLAG_QI ) THEN
                   sqi(k)=sqi3D(i,k) !/(1.+qv(i,k))
                   sqw(k)=sqv(k)+sqc(k)+sqi(k)
                   thl(k)=th1(k) - xlvcp/ex1(k)*sqc(k) &
                       &         - xlscp/ex1(k)*sqi(k)
                   !Use form from Tripoli and Cotton (1981) with their
                   !suggested min temperature to improve accuracy.
                   !thl(k)=th(i,k)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k) &
                   !    &               - xlscp/MAX(tk1(k),TKmin)*sqi(k))
                   !COMPUTE THL USING SGS CLOUDS FOR PBLH DIAG
                   IF(sqc(k)<1e-6 .and. sqi(k)<1e-8 .and. CLDFRA_BL1D(k)>0.001)THEN
                      sqc9=QC_BL1D(k)*CLDFRA_BL1D(k)
                      sqi9=QI_BL1D(k)*CLDFRA_BL1D(k)
                   ELSE
                      sqc9=sqc(k)
                      sqi9=sqi(k)
                   ENDIF
                   thlsg(k)=th1(k) - xlvcp/ex1(k)*sqc9 &
                         &         - xlscp/ex1(k)*sqi9
                   sqwsg(k)=sqv(k)+sqc9+sqi9
                ELSE
                   sqi(k)=0.0
                   sqw(k)=sqv(k)+sqc(k)
                   thl(k)=th1(k)-xlvcp/ex1(k)*sqc(k)
                   !Use form from Tripoli and Cotton (1981) with their 
                   !suggested min temperature to improve accuracy.      
                   !thl(k)=th(i,k)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k))
                   !COMPUTE THL USING SGS CLOUDS FOR PBLH DIAG
                   IF(sqc(k)<1e-6 .and. CLDFRA_BL1D(k)>0.001)THEN
                            sqc9=QC_BL1D(k)*CLDFRA_BL1D(k)
                      sqi9=0.0
                   ELSE
                      sqc9=sqc(k)
                      sqi9=0.0
                   ENDIF
                   thlsg(k)=th1(k) - xlvcp/ex1(k)*sqc9 &
                         &         - xlscp/ex1(k)*sqi9
                   sqwsg(k)=sqv(k)+sqc9+sqi9
                ENDIF
                thvl(k)=thlsg(k)*(1.+0.61*sqv(k))

                IF (k==kts) THEN
                   zw(k)=0.
                ELSE
                   zw(k)=zw(k-1)+dz(i,k-1)
                ENDIF
                IF (INITIALIZE_QKE) THEN
                   !Initialize tke for initial PBLH calc only - using 
                   !simple PBLH form of Koracin and Berkowicz (1988, BLM)
                   !to linearly taper off tke towards top of PBL.
                   qke1(k)=5.*ust(i) * MAX((ust(i)*700. - zw(k))/(MAX(ust(i),0.01)*700.), 0.01)
                ELSE
                   qke1(k)=qke(i,k)
                ENDIF
                el(k)=el_pbl(i,k)
                sh(k)=Sh3D(i,k)
                sm(k)=Sm3D(i,k)
                tsq1(k)=tsq(i,k)
                qsq1(k)=qsq(i,k)
                cov1(k)=cov(i,k)
                if (spp_pbl==1) then
                    rstoch_col(k)=pattern_spp_pbl(i,k)
                else
                    rstoch_col(k)=0.0
                endif

             ENDDO

             zw(kte+1)=zw(kte)+dz(i,kte)

!>  - Call get_pblh() to calculate hybrid (\f$\theta_{vli}-TKE\f$) PBL height.
             CALL GET_PBLH(KTS,KTE,PBLH(i),thetav,&
               &  Qke1,zw,dz1,xland(i),KPBL(i))
             
!>  - Call scale_aware() to calculate similarity functions for scale-adaptive control
!! (\f$P_{\sigma-PBL}\f$ and \f$P_{\sigma-shcu}\f$).
             IF (scaleaware > 0.) THEN
                CALL SCALE_AWARE(dx(i),PBLH(i),Psig_bl(i),Psig_shcu(i))
             ELSE
                Psig_bl(i)=1.0
                Psig_shcu(i)=1.0
             ENDIF

             ! DH* CHECK IF WE CAN DO WITHOUT CALLING THIS ROUTINE FOR RESTARTS
!>  - Call mym_initialize() to initializes the mixing length, TKE, \f$\theta^{'2}\f$,
!! \f$q^{'2}\f$, and \f$\theta^{'}q^{'}\f$. These variables are calculated after 
!! obtaining prerequisite variables by calling the following subroutines from 
!! within mym_initialize(): mym_level2() and mym_length().
             CALL mym_initialize (                & 
                  &kts,kte,xland(i),              &
                  &dz1, dx(i), zw,                &
                  &u1, v1, thl, sqv,              &
                  &thlsg, sqwsg,                  &
                  &PBLH(i), th1, thetav, sh, sm,  &
                  &ust(i), rmol(i),               &
                  &el, Qke1, Tsq1, Qsq1, Cov1,    &
                  &Psig_bl(i), cldfra_bl1D,       &
                  &bl_mynn_mixlength,             &
                  &edmf_w1,edmf_a1,               &
                  &INITIALIZE_QKE,                &
                  &spp_pbl,rstoch_col )

             IF (.not.restart) THEN
                !UPDATE 3D VARIABLES
                DO k=KTS,KTE !KTF
                   el_pbl(i,k)=el(k)
                   sh3d(i,k)=sh(k)
                   sm3d(i,k)=sm(k)
                   qke(i,k)=qke1(k)
                   tsq(i,k)=tsq1(k)
                   qsq(i,k)=qsq1(k)
                   cov(i,k)=cov1(k)
                ENDDO
                !initialize qke_adv array if using advection
                IF (bl_mynn_tkeadvect) THEN
                   DO k=KTS,KTE
                      qke_adv(i,k)=qke1(k)
                   ENDDO
                ENDIF
             ENDIF

!***  Begin debugging
!             IF(I==IMD .AND. J==JMD)THEN
!               PRINT*,"MYNN DRIVER INIT: k=",1," sh=",sh(k)
!               PRINT*," sqw=",sqw(k)," thl=",thl(k)," k_m=",exch_m(i,k)
!               PRINT*," xland=",xland(i)," rmol=",rmol(i)," ust=",ust(i)
!               PRINT*," qke=",qke(i,k)," el=",el_pbl(i,k)," tsq=",Tsq(i,k)
!               PRINT*," PBLH=",PBLH(i)," u=",u(i,k)," v=",v(i,k)
!             ENDIF
!***  End debugging

       ENDDO !end i-loop

    ENDIF ! end initflag

!> - After initializing all required variables, the regular procedures 
!! performed at every time step are ready for execution.
    !ACF- copy qke_adv array into qke if using advection
    IF (bl_mynn_tkeadvect) THEN
       qke=qke_adv
    ENDIF

    DO i=ITS,ITF
       DO k=KTS,KTE !KTF
            !JOE-TKE BUDGET
             IF (tke_budget .eq. 1) THEN
                dqke(i,k)=qke(i,k)
             END IF
             IF (icloud_bl > 0) THEN
                CLDFRA_BL1D(k)=CLDFRA_BL(i,k)
                QC_BL1D(k)=QC_BL(i,k)
                QI_BL1D(k)=QI_BL(i,k)
                cldfra_bl1D_old(k)=cldfra_bl(i,k)
                qc_bl1D_old(k)=qc_bl(i,k)
                qi_bl1D_old(k)=qi_bl(i,k)
             else
                CLDFRA_BL1D(k)=0.0
                QC_BL1D(k)=0.0
                QI_BL1D(k)=0.0
                cldfra_bl1D_old(k)=0.0
                qc_bl1D_old(k)=0.0
                qi_bl1D_old(k)=0.0
             ENDIF
             dz1(k)= dz(i,k)
             u1(k) = u(i,k)
             v1(k) = v(i,k)
             w1(k) = w(i,k)
             th1(k)= th(i,k)
             tk1(k)=T3D(i,k)
             p1(k) = p(i,k)
             ex1(k)= exner(i,k)
             rho1(k)=rho(i,k)
             sqv(k)= sqv3D(i,k) !/(1.+qv(i,k))
             sqc(k)= sqc3D(i,k) !/(1.+qv(i,k))
             qv1(k)= sqv(k)/(1.-sqv(k))
             qc1(k)= sqc(k)/(1.-sqv(k))
             dqc1(k)=0.0
             dqi1(k)=0.0
             dqni1(k)=0.0
             dqnc1(k)=0.0
             dqnwfa1(k)=0.0
             dqnifa1(k)=0.0
             dqnbca1(k)=0.0
             dozone1(k)=0.0
             IF(FLAG_QI)THEN
                sqi(k)= sqi3D(i,k) !/(1.+qv(i,k))
                qi1(k)= sqi(k)/(1.-sqv(k))
                sqw(k)= sqv(k)+sqc(k)+sqi(k)
                thl(k)= th1(k) - xlvcp/ex1(k)*sqc(k) &
                     &         - xlscp/ex1(k)*sqi(k)
                !Use form from Tripoli and Cotton (1981) with their
                !suggested min temperature to improve accuracy.    
                !thl(k)=th(i,k)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k) &
                !    &               - xlscp/MAX(tk1(k),TKmin)*sqi(k))
                !COMPUTE THL USING SGS CLOUDS FOR PBLH DIAG
                IF(sqc(k)<1e-6 .and. sqi(k)<1e-8 .and. CLDFRA_BL1D(k)>0.001)THEN
                   sqc9=QC_BL1D(k)*CLDFRA_BL1D(k)
                   sqi9=QI_BL1D(k)*CLDFRA_BL1D(k)
                ELSE
                   sqc9=sqc(k)
                   sqi9=sqi(k)
                ENDIF
                thlsg(k)=th1(k) - xlvcp/ex1(k)*sqc9 &
                      &         - xlscp/ex1(k)*sqi9
                sqwsg(k)=sqv(k)+sqc9+sqi9
             ELSE
                qi1(k)=0.0
                sqi(k)=0.0
                sqw(k)= sqv(k)+sqc(k)
                thl(k)= th1(k)-xlvcp/ex1(k)*sqc(k)
                !Use form from Tripoli and Cotton (1981) with their
                !suggested min temperature to improve accuracy.    
                !thl(k)=th(i,k)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc(k))
                !COMPUTE THL USING SGS CLOUDS FOR PBLH DIAG
                IF(sqc(k)<1e-6 .and. CLDFRA_BL1D(k)>0.001)THEN
                   sqc9=QC_BL1D(k)*CLDFRA_BL1D(k)
                   sqi9=QI_BL1D(k)*CLDFRA_BL1D(k)
                ELSE
                   sqc9=sqc(k)
                   sqi9=0.0
                ENDIF
                thlsg(k)=th1(k) - xlvcp/ex1(k)*sqc9 &
                      &         - xlscp/ex1(k)*sqi9 
            ENDIF
            thetav(k)=th1(k)*(1.+0.608*sqv(k))
            thvl(k)  =thlsg(k) *(1.+0.608*sqv(k))

             IF (FLAG_QNI ) THEN
                qni1(k)=qni(i,k)
             ELSE
                qni1(k)=0.0
             ENDIF
             IF (FLAG_QNC ) THEN
                qnc1(k)=qnc(i,k)
             ELSE
                qnc1(k)=0.0
             ENDIF
             IF (FLAG_QNWFA ) THEN
                qnwfa1(k)=qnwfa(i,k)
             ELSE
                qnwfa1(k)=0.0
             ENDIF
             IF (FLAG_QNIFA ) THEN
                qnifa1(k)=qnifa(i,k)
             ELSE
                qnifa1(k)=0.0
             ENDIF
             IF (FLAG_QNBCA .and. PRESENT(qnbca)) THEN
                qnbca1(k)=qnbca(i,k)
             ELSE
                qnbca1(k)=0.0
             ENDIF
             IF (PRESENT(ozone)) THEN
                ozone1(k)=ozone(i,k)
             ELSE
                ozone1(k)=0.0
             ENDIF
             el(k) = el_pbl(i,k)
             qke1(k)=qke(i,k)
             sh(k)  =sh3d(i,k)
             sm(k)  =sm3d(i,k)
             tsq1(k)=tsq(i,k)
             qsq1(k)=qsq(i,k)
             cov1(k)=cov(i,k)
             if (spp_pbl==1) then
                rstoch_col(k)=pattern_spp_pbl(i,k)
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
             s_awqnbca1(k)=0.
             ![EWDD]
             edmf_a_dd1(k)=0.0
             edmf_w_dd1(k)=0.0
             edmf_qc_dd1(k)=0.0
             sd_aw1(k)=0.
             sd_awthl1(k)=0.
             sd_awqt1(k)=0.
             sd_awqv1(k)=0.
             sd_awqc1(k)=0.
             sd_awu1(k)=0.
             sd_awv1(k)=0.
             sd_awqke1(k)=0.
             sub_thl(k)=0.
             sub_sqv(k)=0.
             sub_u(k)=0.
             sub_v(k)=0.
             det_thl(k)=0.
             det_sqv(k)=0.
             det_sqc(k)=0.
             det_u(k)=0.
             det_v(k)=0.

             IF (k==kts) THEN
                zw(k)=0.
             ELSE
                zw(k)=zw(k-1)+dz(i,k-1)
             ENDIF
          ENDDO ! end k

          !initialize smoke/chem arrays (if used):
             if  ( mix_chem ) then
                do ic = 1,ndvel
                   vd1(ic) = vdep(i,ic) ! dry deposition velocity
                enddo
                do k = kts,kte
                   do ic = 1,nchem
                      chem1(k,ic) = chem3d(i,k,ic)
                      s_awchem1(k,ic)=0.
                   enddo
                enddo
             else
                do ic = 1,ndvel
                   vd1(ic) = 0. ! dry deposition velocity
                enddo
                do k = kts,kte
                   do ic = 1,nchem
                      chem1(k,ic) = 0.
                      s_awchem1(k,ic)=0.
                   enddo
                enddo
             endif

          zw(kte+1)=zw(kte)+dz(i,kte)
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
          s_awqnbca1(kte+1)=0.
          sd_aw1(kte+1)=0.
          sd_awthl1(kte+1)=0.
          sd_awqt1(kte+1)=0.
          sd_awqv1(kte+1)=0.
          sd_awqc1(kte+1)=0.
          sd_awu1(kte+1)=0.
          sd_awv1(kte+1)=0.
          sd_awqke1(kte+1)=0.
          IF ( mix_chem ) THEN
             DO ic = 1,nchem
                s_awchem1(kte+1,ic)=0.
             ENDDO
          ENDIF

!>  - Call get_pblh() to calculate the hybrid \f$\theta_{vli}-TKE\f$
!! PBL height diagnostic.
          CALL GET_PBLH(KTS,KTE,PBLH(i),thetav,&
          & Qke1,zw,dz1,xland(i),KPBL(i))

!>  - Call scale_aware() to calculate the similarity functions,
!! \f$P_{\sigma-PBL}\f$ and \f$P_{\sigma-shcu}\f$, to control 
!! the scale-adaptive behaviour for the local and nonlocal 
!! components, respectively.
          IF (scaleaware > 0.) THEN
             CALL SCALE_AWARE(dx(i),PBLH(i),Psig_bl(i),Psig_shcu(i))
          ELSE
             Psig_bl(i)=1.0
             Psig_shcu(i)=1.0
          ENDIF

          sqcg= 0.0   !ill-defined variable; qcg has been removed
          cpm=cp*(1.+0.84*qv1(kts))
          exnerg=(ps(i)/p1000mb)**rcp

          !-----------------------------------------------------
          !ORIGINAL CODE
          !flt = hfx(i)/( rho(i,kts)*cpm ) &
          ! +xlvcp*ch(i)*(sqc(kts)/exner(i,kts) -sqcg/exnerg)
          !flq = qfx(i)/  rho(i,kts)       &
          !    -ch(i)*(sqc(kts)   -sqcg )
          !-----------------------------------------------------
          flqv   = qfx(i)/rho1(kts)
          flqc   = 0.0 !currently no sea-spray fluxes, fog settling hangled elsewhere
          th_sfc = ts(i)/ex1(kts)

          ! TURBULENT FLUX FOR TKE BOUNDARY CONDITIONS
          flq =flqv+flqc                  !! LATENT
          flt =hfx(i)/(rho1(kts)*cpm )-xlvcp*flqc/ex1(kts)  !! Temperature flux
          fltv=flt + flqv*p608*th_sfc     !! Virtual temperature flux

          ! Update 1/L using updated sfc heat flux and friction velocity
          rmol(i) = -karman*gtr*fltv/max(ust(i)**3,1.0e-6)
          zet = 0.5*dz(i,kts)*rmol(i)
          zet = MAX(zet, -20.)
          zet = MIN(zet,  20.)
          !if(i.eq.idbg)print*,"updated z/L=",zet
          if (bl_mynn_stfunc == 0) then
             !Original Kansas-type stability functions
             if ( zet >= 0.0 ) then
                pmz = 1.0 + (cphm_st-1.0) * zet
                phh = 1.0 +  cphh_st      * zet
             else
                pmz = 1.0/    (1.0-cphm_unst*zet)**0.25 - zet
                phh = 1.0/SQRT(1.0-cphh_unst*zet)
             end if
          else
             !Updated stability functions (Puhales, 2020)
             phi_m = phim(zet)
             pmz   = phi_m - zet
             phh   = phih(zet)
          end if

!>  - Call mym_condensation() to calculate the nonconvective component
!! of the subgrid cloud fraction and mixing ratio as well as the functions
!! used to calculate the buoyancy flux. Different cloud PDFs can be
!! selected by use of the namelist parameter \p bl_mynn_cloudpdf.

          CALL  mym_condensation ( kts,kte,      &
               &dx(i),dz1,zw,xland(i),           &
               &thl,sqw,sqv,sqc,sqi,             &
               &p1,ex1,tsq1,qsq1,cov1,           &
               &Sh,el,bl_mynn_cloudpdf,          &
               &qc_bl1D,qi_bl1D,cldfra_bl1D,     &
               &PBLH(i),HFX(i),                  &
               &Vt, Vq, th1, sgm, rmol(i),       &
               &spp_pbl, rstoch_col              )

!>  - Add TKE source driven by cloud top cooling
!!  Calculate the buoyancy production of TKE from cloud-top cooling when
!! \p bl_mynn_topdown =1.
          IF (bl_mynn_topdown.eq.1)then
             CALL topdown_cloudrad(kts,kte,dz1,zw,          &
                &xland(i),kpbl(i),PBLH(i),                  &
                &sqc,sqi,sqw,thl,th1,ex1,p1,rho1,thetav,    &
                &cldfra_bl1D,rthraten(i,:),                 &
                &maxKHtopdown(i),KHtopdown,TKEprodTD        )
          ELSE
             maxKHtopdown(i)  = 0.0
             KHtopdown(kts:kte) = 0.0
             TKEprodTD(kts:kte) = 0.0
          ENDIF

          IF (bl_mynn_edmf > 0) THEN
            !PRINT*,"Calling DMP Mass-Flux: i= ",i
            CALL DMP_mf(                          &
               &kts,kte,delt,zw,dz1,p1,rho1,      &
               &bl_mynn_edmf_mom,                 &
               &bl_mynn_edmf_tke,                 &
               &bl_mynn_mixscalars,               &
               &u1,v1,w1,th1,thl,thetav,tk1,      &
               &sqw,sqv,sqc,qke1,                 &
               &qnc1,qni1,qnwfa1,qnifa1,qnbca1,   &
               &ex1,Vt,Vq,sgm,                    &
               &ust(i),flt,fltv,flq,flqv,         &
               &PBLH(i),KPBL(i),DX(i),            &
               &xland(i),th_sfc,                  &
            ! now outputs - tendencies
            ! &,dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf  &
            ! outputs - updraft properties
               & edmf_a1,edmf_w1,edmf_qt1,        &
               & edmf_thl1,edmf_ent1,edmf_qc1,    &
            ! for the solver
               & s_aw1,s_awthl1,s_awqt1,          &
               & s_awqv1,s_awqc1,                 &
               & s_awu1,s_awv1,s_awqke1,          &
               & s_awqnc1,s_awqni1,               &
               & s_awqnwfa1,s_awqnifa1,s_awqnbca1,&
               & sub_thl,sub_sqv,                 &
               & sub_u,sub_v,                     &
               & det_thl,det_sqv,det_sqc,         &
               & det_u,det_v,                     &
            ! chem/smoke mixing
               & nchem,chem1,s_awchem1,           &
               & mix_chem,                        &
               & qc_bl1D,cldfra_bl1D,             &
               & qc_bl1D_old,cldfra_bl1D_old,     &
               & FLAG_QC,FLAG_QI,                 &
               & FLAG_QNC,FLAG_QNI,               &
               & FLAG_QNWFA,FLAG_QNIFA,FLAG_QNBCA,&
               & Psig_shcu(i),                    &
               & nupdraft(i),ktop_plume(i),       &
               & maxmf(i),ztop_plume,             &
               & spp_pbl,rstoch_col               )
          ENDIF

          IF (bl_mynn_edmf_dd == 1) THEN
            CALL DDMF_JPL(kts,kte,delt,zw,dz1,p1, &
              &u1,v1,th1,thl,thetav,tk1,          &
              sqw,sqv,sqc,rho1,ex1,               &
              &ust(i),flt,flq,                    &
              &PBLH(i),KPBL(i),                   &
              &edmf_a_dd1,edmf_w_dd1,edmf_qt_dd1, &
              &edmf_thl_dd1,edmf_ent_dd1,         &
              &edmf_qc_dd1,                       &
              &sd_aw1,sd_awthl1,sd_awqt1,         &
              &sd_awqv1,sd_awqc1,sd_awu1,sd_awv1, &
              &sd_awqke1,                         &
              &qc_bl1d,cldfra_bl1d,               &
              &rthraten(i,:)                      )
          ENDIF

          !Capability to substep the eddy-diffusivity portion
          !do nsub = 1,2
          delt2 = delt !*0.5    !only works if topdown=0

          CALL mym_turbulence (                  & 
               &kts,kte,xland(i),closure,        &
               &dz1, DX(i), zw,                  &
               &u1, v1, thl, thetav, sqc, sqw,   &
               &thlsg, sqwsg,                    &
               &qke1, tsq1, qsq1, cov1,          &
               &vt, vq,                          &
               &rmol(i), flt, flq,               &
               &PBLH(i),th1,                     &
               &Sh,Sm,el,                        &
               &Dfm,Dfh,Dfq,                     &
               &Tcd,Qcd,Pdk,                     &
               &Pdt,Pdq,Pdc,                     &
               &qWT1,qSHEAR1,qBUOY1,qDISS1,      &
               &tke_budget,                      &
               &Psig_bl(i),Psig_shcu(i),         &
               &cldfra_bl1D,bl_mynn_mixlength,   &
               &edmf_w1,edmf_a1,                 &
               &TKEprodTD,                       &
               &spp_pbl,rstoch_col)

!>  - Call mym_predict() to solve TKE and 
!! \f$\theta^{'2}, q^{'2}, and \theta^{'}q^{'}\f$
!! for the following time step.
          CALL mym_predict (kts,kte,closure,     &
               &delt2, dz1,                      &
               &ust(i), flt, flq, pmz, phh,      &
               &el, dfq, rho1, pdk, pdt, pdq, pdc,&
               &Qke1, Tsq1, Qsq1, Cov1,          &
               &s_aw1, s_awqke1, bl_mynn_edmf_tke,&
               &qWT1, qDISS1,tke_budget          ) !! TKE budget  (Puhales, 2020)

          if (dheat_opt > 0) then
             DO k=kts,kte-1
                ! Set max dissipative heating rate to 7.2 K per hour
                diss_heat(k) = MIN(MAX(1.0*(qke1(k)**1.5)/(b1*MAX(0.5*(el(k)+el(k+1)),1.))/cp, 0.0),0.002)
                ! Limit heating above 100 mb:
                diss_heat(k) = diss_heat(k) * exp(-10000./MAX(p1(k),1.)) 
             ENDDO
             diss_heat(kte) = 0.
          else
             diss_heat(1:kte) = 0.
          endif

!>  - Call mynn_tendencies() to solve for tendencies of 
!! \f$U, V, \theta, q_{v}, q_{c}, and q_{i}\f$.
          CALL mynn_tendencies(kts,kte,i,        &
               &delt, dz1, rho1,                 &
               &u1, v1, th1, tk1, qv1,           &
               &qc1, qi1, qnc1, qni1,            &
               &ps(i), p1, ex1, thl,             &
               &sqv, sqc, sqi, sqw,              &
               &qnwfa1, qnifa1, qnbca1, ozone1,  &
               &ust(i),flt,flq,flqv,flqc,        &
               &wspd(i),uoce(i),voce(i),         &
               &tsq1, qsq1, cov1,                &
               &tcd, qcd,                        &
               &dfm, dfh, dfq,                   &
               &Du1, Dv1, Dth1, Dqv1,            &
               &Dqc1, Dqi1, Dqnc1, Dqni1,        &
               &Dqnwfa1, Dqnifa1, Dqnbca1,       &
               &Dozone1,                         &
               &diss_heat,                       &
               ! mass flux components
               &s_aw1,s_awthl1,s_awqt1,          &
               &s_awqv1,s_awqc1,s_awu1,s_awv1,   &
               &s_awqnc1,s_awqni1,               &
               &s_awqnwfa1,s_awqnifa1,s_awqnbca1,&
               &sd_aw1,sd_awthl1,sd_awqt1,       &
               &sd_awqv1,sd_awqc1,               &
               sd_awu1,sd_awv1,                  &
               &sub_thl,sub_sqv,                 &
               &sub_u,sub_v,                     &
               &det_thl,det_sqv,det_sqc,         &
               &det_u,det_v,                     &
               &FLAG_QC,FLAG_QI,FLAG_QNC,        &
               &FLAG_QNI,FLAG_QNWFA,FLAG_QNIFA,  &
               &FLAG_QNBCA,                      &
               &cldfra_bl1d,                     &
               &bl_mynn_cloudmix,                &
               &bl_mynn_mixqt,                   &
               &bl_mynn_edmf,                    &
               &bl_mynn_edmf_mom,                &
               &bl_mynn_mixscalars               )


          IF ( mix_chem ) THEN
            IF ( rrfs_sd ) THEN 
             CALL mynn_mix_chem(kts,kte,i,       &
                  &delt, dz1, pblh(i),           &
                  &nchem, kdvel, ndvel,          &
                  &chem1, vd1,                   &
                  &rho1,flt,                     &
                  &tcd, qcd,                     &
                  &dfh,                          &
                  &s_aw1,s_awchem1,              &
                  &emis_ant_no(i),               &
                  &frp(i), rrfs_sd,              &
                  &enh_mix, smoke_dbg            )
             ELSE
              CALL mynn_mix_chem(kts,kte,i,       &
                   &delt, dz1, pblh(i),           &
                   &nchem, kdvel, ndvel,          &
                   &chem1, vd1,                   &
                   &rho1,flt,                     &
                   &tcd, qcd,                     &
                   &dfh,                          &
                   &s_aw1,s_awchem1,              &
                   &zero,                         &
                   &zero, rrfs_sd,                &
                   &enh_mix, smoke_dbg            )
             ENDIF
             DO ic = 1,nchem
                DO k = kts,kte
                   chem3d(i,k,ic) = max(1.e-12, chem1(k,ic))
                ENDDO
             ENDDO
          ENDIF
 
          CALL retrieve_exchange_coeffs(kts,kte,&
               &dfm, dfh, dz1, K_m1, K_h1)

          !UPDATE 3D ARRAYS
          do k=kts,kte
             exch_m(i,k)=K_m1(k)
             exch_h(i,k)=K_h1(k)
             rublten(i,k)=du1(k)
             rvblten(i,k)=dv1(k)
             rthblten(i,k)=dth1(k)
             rqvblten(i,k)=dqv1(k)
             if (bl_mynn_cloudmix > 0) then
               if (present(sqc3D) .and. flag_qc) rqcblten(i,k)=dqc1(k)
               if (present(sqi3D) .and. flag_qi) rqiblten(i,k)=dqi1(k)
             else
               if (present(sqc3D) .and. flag_qc) rqcblten(i,k)=0.
               if (present(sqi3D) .and. flag_qi) rqiblten(i,k)=0.
             endif
             if (bl_mynn_cloudmix > 0 .and. bl_mynn_mixscalars > 0) then
               if (present(qnc) .and. flag_qnc) rqncblten(i,k)=dqnc1(k)
               if (present(qni) .and. flag_qni) rqniblten(i,k)=dqni1(k)
               if (present(qnwfa) .and. flag_qnwfa) rqnwfablten(i,k)=dqnwfa1(k)
               if (present(qnifa) .and. flag_qnifa) rqnifablten(i,k)=dqnifa1(k)
               if (present(qnbca) .and. flag_qnbca) rqnbcablten(i,k)=dqnbca1(k)
             else
               if (present(qnc) .and. flag_qnc) rqncblten(i,k)=0.
               if (present(qni) .and. flag_qni) rqniblten(i,k)=0.
               if (present(qnwfa) .and. flag_qnwfa) rqnwfablten(i,k)=0.
               if (present(qnifa) .and. flag_qnifa) rqnifablten(i,k)=0.
               if (present(qnbca) .and. flag_qnbca) rqnbcablten(i,k)=0.
             endif
             dozone(i,k)=dozone1(k)

             if (icloud_bl > 0) then
                qc_bl(i,k)=qc_bl1D(k)
                qi_bl(i,k)=qi_bl1D(k)
                cldfra_bl(i,k)=cldfra_bl1D(k)
             endif

             el_pbl(i,k)=el(k)
             qke(i,k)=qke1(k)
             tsq(i,k)=tsq1(k)
             qsq(i,k)=qsq1(k)
             cov(i,k)=cov1(k)
             sh3d(i,k)=sh(k)
             sm3d(i,k)=sm(k)
          enddo !end-k

          if (tke_budget .eq. 1) then
             !! TKE budget is now given in m**2/s**-3 (Puhales, 2020)
             !! Lower boundary condtions (using similarity relationships such as the prognostic equation for Qke)
             k=kts
             qSHEAR1(k)=4.*(ust(i)**3*phi_m/(karman*dz(i,k)))-qSHEAR1(k+1) !! staggered
             qBUOY1(k)=4.*(-ust(i)**3*zet/(karman*dz(i,k)))-qBUOY1(k+1) !! staggered
             !! unstaggering SHEAR and BUOY and trasfering all TKE budget to 3D array               
             do k = kts,kte-1
                qSHEAR(i,k)=0.5*(qSHEAR1(k)+qSHEAR1(k+1)) !!! unstaggering in z
                qBUOY(i,k)=0.5*(qBUOY1(k)+qBUOY1(k+1)) !!! unstaggering in z
                qWT(i,k)=qWT1(k)
                qDISS(i,k)=qDISS1(k)
                dqke(i,k)=(qke1(k)-dqke(i,k))*0.5/delt
             enddo
             !! Upper boundary conditions               
             k=kte
             qSHEAR(i,k)=0.
             qBUOY(i,k)=0.
             qWT(i,k)=0.
             qDISS(i,k)=0.
             dqke(i,k)=0.
          endif

          !update updraft/downdraft properties
          if (bl_mynn_output > 0) THEN !research mode == 1
             if (bl_mynn_edmf > 0) THEN
                DO k = kts,kte
                   edmf_a(i,k)=edmf_a1(k)
                   edmf_w(i,k)=edmf_w1(k)
                   edmf_qt(i,k)=edmf_qt1(k)
                   edmf_thl(i,k)=edmf_thl1(k)
                   edmf_ent(i,k)=edmf_ent1(k)
                   edmf_qc(i,k)=edmf_qc1(k)
                   sub_thl3D(i,k)=sub_thl(k)
                   sub_sqv3D(i,k)=sub_sqv(k)
                   det_thl3D(i,k)=det_thl(k)
                   det_sqv3D(i,k)=det_sqv(k)
                ENDDO
             endif
!             if (bl_mynn_edmf_dd > 0) THEN
!                DO k = kts,kte
!                   edmf_a_dd(i,k)=edmf_a_dd1(k)
!                   edmf_w_dd(i,k)=edmf_w_dd1(k)
!                   edmf_qt_dd(i,k)=edmf_qt_dd1(k)
!                   edmf_thl_dd(i,k)=edmf_thl_dd1(k)
!                   edmf_ent_dd(i,k)=edmf_ent_dd1(k)
!                   edmf_qc_dd(i,k)=edmf_qc_dd1(k)
!                ENDDO
!             ENDIF
          ENDIF

          !***  Begin debug prints
          IF ( debug_code .and. (i .eq. idbg)) THEN
             IF ( ABS(QFX(i))>.001)print*,&
                "SUSPICIOUS VALUES AT: i=",i," QFX=",QFX(i)
             IF ( ABS(HFX(i))>1100.)print*,&
                "SUSPICIOUS VALUES AT: i=",i," HFX=",HFX(i)
             DO k = kts,kte
               IF ( sh(k) < 0. .OR. sh(k)> 200.)print*,&
                  "SUSPICIOUS VALUES AT: i,k=",i,k," sh=",sh(k)
               IF ( ABS(vt(k)) > 2.0 )print*,&
                  "SUSPICIOUS VALUES AT: i,k=",i,k," vt=",vt(k)
               IF ( ABS(vq(k)) > 7000.)print*,&
                  "SUSPICIOUS VALUES AT: i,k=",i,k," vq=",vq(k)
               IF ( qke(i,k) < -1. .OR. qke(i,k)> 200.)print*,&
                  "SUSPICIOUS VALUES AT: i,k=",i,k," qke=",qke(i,k)
               IF ( el_pbl(i,k) < 0. .OR. el_pbl(i,k)> 1500.)print*,&
                  "SUSPICIOUS VALUES AT: i,k=",i,k," el_pbl=",el_pbl(i,k)
               IF ( exch_m(i,k) < 0. .OR. exch_m(i,k)> 2000.)print*,&
                  "SUSPICIOUS VALUES AT: i,k=",i,k," exxch_m=",exch_m(i,k)
               IF (icloud_bl > 0) then
                  IF( cldfra_bl(i,k) < 0.0 .OR. cldfra_bl(i,k)> 1.)THEN
                  PRINT*,"SUSPICIOUS VALUES: CLDFRA_BL=",cldfra_bl(i,k)," qc_bl=",QC_BL(i,k)
                  ENDIF
               ENDIF

               !IF (I==IMD .AND. J==JMD) THEN
               !   PRINT*,"MYNN DRIVER END: k=",k," sh=",sh(k)
               !   PRINT*," sqw=",sqw(k)," thl=",thl(k)," exch_m=",exch_m(i,k)
               !   PRINT*," xland=",xland(i)," rmol=",rmol(i)," ust=",ust(i)
               !   PRINT*," qke=",qke(i,k)," el=",el_pbl(i,k)," tsq=",tsq(i,k)
               !   PRINT*," PBLH=",PBLH(i)," u=",u(i,k)," v=",v(i,k)
               !   PRINT*," vq=",vq(k)," vt=",vt(k)
               !ENDIF
             ENDDO !end-k
          ENDIF
          !***  End debug prints

          !JOE-add tke_pbl for coupling w/shallow-cu schemes (TKE_PBL = QKE/2.)
          !    TKE_PBL is defined on interfaces, while QKE is at middle of layer.
          !tke_pbl(i,kts) = 0.5*MAX(qke(i,kts),1.0e-10)
          !DO k = kts+1,kte
          !   afk = dz1(k)/( dz1(k)+dz1(k-1) )
          !   abk = 1.0 -afk
          !   tke_pbl(i,k) = 0.5*MAX(qke(i,k)*abk+qke(i,k-1)*afk,1.0e-3)
          !ENDDO

    ENDDO !end i-loop

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
!> @}

!=======================================================================
!     SUBROUTINE  mym_initialize:
!
!     Input variables:
!       iniflag         : <>0; turbulent quantities will be initialized
!                         = 0; turbulent quantities have been already
!                              given, i.e., they will not be initialized
!       nx, nz          : Dimension sizes of the
!                         x and z directions, respectively
!       tref            : Reference temperature                      (K)
!       dz(nz)          : Vertical grid spacings                     (m)
!                         # dz(nz)=dz(nz-1)
!       zw(nz+1)        : Heights of the walls of the grid boxes     (m)
!                         # zw(1)=0.0 and zw(k)=zw(k-1)+dz(k-1)
!       exner(nx,nz)    : Exner function at zw*h+zg             (J/kg K)
!                         defined by c_p*( p_basic/1000hPa )^kappa
!                         This is usually computed by integrating
!                         d(pi0)/dz = -h*g/tref.
!       rmo(nx)         : Inverse of the Obukhov length         (m^(-1))
!       flt, flq(nx)    : Turbulent fluxes of potential temperature and
!                         total water, respectively:
!                                    flt=-u_*Theta_*             (K m/s)
!                                    flq=-u_*qw_*            (kg/kg m/s)
!       ust(nx)         : Friction velocity                        (m/s)
!       pmz(nx)         : phi_m-zeta at z1*h+z0, where z1 (=0.5*dz(1))
!                         is the first grid point above the surafce, z0
!                         the roughness length and zeta=(z1*h+z0)*rmo
!       phh(nx)         : phi_h at z1*h+z0
!       u, v(nx,nz)     : Components of the horizontal wind        (m/s)
!       thl(nx,nz)      : Liquid water potential temperature
!                                                                    (K)
!       qw(nx,nz)       : Total water content Q_w                (kg/kg)
!
!     Output variables:
!       ql(nx,nz)       : Liquid water content                   (kg/kg)
!       vt, vq(nx,nz)   : Functions for computing the buoyancy flux
!       qke(nx,nz)      : Twice the turbulent kinetic energy q^2
!                                                              (m^2/s^2)
!       tsq(nx,nz)      : Variance of Theta_l                      (K^2)
!       qsq(nx,nz)      : Variance of Q_w
!       cov(nx,nz)      : Covariance of Theta_l and Q_w              (K)
!       el(nx,nz)       : Master length scale L                      (m)
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

!>\ingroup gsd_mynn_edmf
!! This subroutine initializes the mixing length, TKE, \f$\theta^{'2}\f$,
!! \f$q^{'2}\f$, and \f$\theta^{'}q^{'}\f$.
!!\section gen_mym_ini GSD MYNN-EDMF mym_initialize General Algorithm 
!> @{
  SUBROUTINE  mym_initialize (                                & 
       &            kts,kte,xland,                            &
       &            dz, dx, zw,                               &
       &            u, v, thl, qw,                            &
       &            thlsg, qwsg,                              &
!       &            ust, rmo, pmz, phh, flt, flq,             &
       &            zi, theta, thetav, sh, sm,                &
       &            ust, rmo, el,                             &
       &            Qke, Tsq, Qsq, Cov, Psig_bl, cldfra_bl1D, &
       &            bl_mynn_mixlength,                        &
       &            edmf_w1,edmf_a1,                          &
       &            INITIALIZE_QKE,                           &
       &            spp_pbl,rstoch_col)
!
!-------------------------------------------------------------------
    
    INTEGER, INTENT(IN)   :: kts,kte
    INTEGER, INTENT(IN)   :: bl_mynn_mixlength
    LOGICAL, INTENT(IN)   :: INITIALIZE_QKE
!    REAL, INTENT(IN)   :: ust, rmo, pmz, phh, flt, flq
    REAL, INTENT(IN)   :: ust, rmo, Psig_bl, dx, xland
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,cldfra_bl1D,&
                                          edmf_w1,edmf_a1
    REAL, DIMENSION(kts:kte), INTENT(out) :: tsq,qsq,cov
    REAL, DIMENSION(kts:kte), INTENT(inout) :: el,qke
    REAL, DIMENSION(kts:kte) :: &
         &ql,pdk,pdt,pdq,pdc,dtl,dqw,dtv,&
         &gm,gh,sm,sh,qkw,vt,vq
    INTEGER :: k,l,lmax
    REAL :: phm,vkz,elq,elv,b1l,b2l,pmz=1.,phh=1.,flt=0.,flq=0.,tmpq
    REAL :: zi
    REAL, DIMENSION(kts:kte) :: theta,thetav,thlsg,qwsg

    REAL, DIMENSION(kts:kte) :: rstoch_col
    INTEGER ::spp_pbl

!> - At first ql, vt and vq are set to zero.
    DO k = kts,kte
       ql(k) = 0.0
       vt(k) = 0.0
       vq(k) = 0.0
    END DO
!
!> - Call mym_level2() to calculate the stability functions at level 2.
    CALL mym_level2 ( kts,kte,                      &
         &            dz,                           &
         &            u, v, thl, thetav, qw,        &
         &            thlsg, qwsg,                  &
         &            ql, vt, vq,                   &
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
       vkz = karman*zw(k)
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
!> - call mym_length() to calculate the master length scale.
       CALL mym_length (                          &
            &            kts,kte,xland,           &
            &            dz, dx, zw,              &
            &            rmo, flt, flq,           &
            &            vt, vq,                  &
            &            u, v, qke,               &
            &            dtv,                     &
            &            el,                      &
            &            zi,theta,                &
            &            qkw,Psig_bl,cldfra_bl1D, &
            &            bl_mynn_mixlength,       &
            &            edmf_w1,edmf_a1          )
!
       DO k = kts+1,kte
          elq = el(k)*qkw(k)
          pdk(k) = elq*( sm(k)*gm(k) + &
               &         sh(k)*gh(k) )
          pdt(k) = elq*  sh(k)*dtl(k)**2
          pdq(k) = elq*  sh(k)*dqw(k)**2
          pdc(k) = elq*  sh(k)*dtl(k)*dqw(k)
       END DO
!
!   **  Strictly, vkz*h(i,j) -> karman*( 0.5*dz(1)*h(i,j)+z0 )  **
       vkz = karman*0.5*dz(kts)
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
             qke(k) = tmpq**twothirds
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
!> @}
  
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

!>\ingroup gsd_mynn_edmf
!! This subroutine calculates the level 2, non-dimensional wind shear
!! \f$G_M\f$ and vertical temperature gradient \f$G_H\f$ as well as 
!! the level 2 stability funcitons \f$S_h\f$ and \f$S_m\f$.
!!\param kts    horizontal dimension
!!\param kte    vertical dimension
!!\param dz     vertical grid spacings (\f$m\f$)
!!\param u      west-east component of the horizontal wind (\f$m s^{-1}\f$)
!!\param v      south-north component of the horizontal wind (\f$m s^{-1}\f$)
!!\param thl    liquid water potential temperature
!!\param qw     total water content \f$Q_w\f$
!!\param ql     liquid water content (\f$kg kg^{-1}\f$)
!!\param vt
!!\param vq
!!\param dtl     vertical gradient of \f$\theta_l\f$ (\f$K m^{-1}\f$)
!!\param dqw     vertical gradient of \f$Q_w\f$
!!\param dtv     vertical gradient of \f$\theta_V\f$ (\f$K m^{-1}\f$)
!!\param gm      \f$G_M\f$ divided by \f$L^{2}/q^{2}\f$ (\f$s^{-2}\f$)
!!\param gh      \f$G_H\f$ divided by \f$L^{2}/q^{2}\f$ (\f$s^{-2}\f$)
!!\param sm      stability function for momentum, at Level 2
!!\param sh      stability function for heat, at Level 2
!!\section gen_mym_level2 GSD MYNN-EDMF mym_level2 General Algorithm
!! @ {
  SUBROUTINE  mym_level2 (kts,kte,                &
       &            dz,                           &
       &            u, v, thl, thetav, qw,        &
       &            thlsg, qwsg,                  &
       &            ql, vt, vq,                   &
       &            dtl, dqw, dtv, gm, gh, sm, sh )
!
!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,qw,ql,vt,vq,&
                                            thetav,thlsg,qwsg
    REAL, DIMENSION(kts:kte), INTENT(out) :: &
         &dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k

    REAL :: rfc,f1,f2,rf1,rf2,smc,shc,&
         &ri1,ri2,ri3,ri4,duz,dtz,dqz,vtt,vqq,dtq,dzk,afk,abk,ri,rf

    REAL ::   a2fac

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
       !Alternatively, use SGS clouds for thl
       !dtz = ( thlsg(k)-thlsg(k-1) )/( dzk )
       dqz = ( qw(k)-qw(k-1) )/( dzk )
       !Alternatively, use SGS clouds for qw
       !dqz = ( qwsg(k)-qwsg(k-1) )/( dzk )
!
       vtt =  1.0 +vt(k)*abk +vt(k-1)*afk  ! Beta-theta in NN09, Eq. 39
       vqq =  tv0 +vq(k)*abk +vq(k-1)*afk  ! Beta-q
       dtq =  vtt*dtz +vqq*dqz
       !Alternatively, use theta-v without the SGS clouds
       !dtq = ( thetav(k)-thetav(k-1) )/( dzk )
!
       dtl(k) =  dtz
       dqw(k) =  dqz
       dtv(k) =  dtq
!?      dtv(i,j,k) =  dtz +tv0*dqz
!?   :              +( xlv/pi0(i,j,k)-tv1 )
!?   :              *( ql(i,j,k)-ql(i,j,k-1) )/( dzk*h(i,j) )
!
       gm (k) =  duz
       gh (k) = -dtq*gtr
!
!   **  Gradient Richardson number  **
       ri = -gh(k)/MAX( duz, 1.0e-10 )

    !a2fac is needed for the Canuto/Kitamura mod
    IF (CKmod .eq. 1) THEN
       a2fac = 1./(1. + MAX(ri,0.0))
    ELSE
       a2fac = 1.
    ENDIF

       rfc = g1/( g1+g2 )
       f1  = b1*( g1-c1 ) +3.0*a2*a2fac *( 1.0    -c2 )*( 1.0-c5 ) &
    &                     +2.0*a1*( 3.0-2.0*c2 )
       f2  = b1*( g1+g2 ) -3.0*a1*( 1.0    -c2 )
       rf1 = b1*( g1-c1 )/f1
       rf2 = b1*  g1     /f2
       smc = a1 /(a2*a2fac)*  f1/f2
       shc = 3.0*(a2*a2fac)*( g1+g2 )

       ri1 = 0.5/smc
       ri2 = rf1*smc
       ri3 = 4.0*rf2*smc -2.0*ri2
       ri4 = ri2**2

!   **  Flux Richardson number  **
       rf = MIN( ri1*( ri + ri2-SQRT(ri**2 - ri3*ri + ri4) ), rfc )
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
!! @}

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
!>\ingroup gsd_mynn_edmf
!! This subroutine calculates the mixing lengths.
  SUBROUTINE  mym_length (                     & 
    &            kts,kte,xland,                &
    &            dz, dx, zw,                   &
    &            rmo, flt, flq,                &
    &            vt, vq,                       &
    &            u1, v1, qke,                  &
    &            dtv,                          &
    &            el,                           &
    &            zi, theta, qkw,               &
    &            Psig_bl, cldfra_bl1D,         &
    &            bl_mynn_mixlength,            &
    &            edmf_w1,edmf_a1               )
    
!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    INTEGER, INTENT(IN)   :: bl_mynn_mixlength
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl,dx,xland
    REAL, DIMENSION(kts:kte), INTENT(IN)   :: u1,v1,qke,vt,vq,cldfra_bl1D,&
                                          edmf_w1,edmf_a1
    REAL, DIMENSION(kts:kte), INTENT(out)  :: qkw, el
    REAL, DIMENSION(kts:kte), INTENT(in)   :: dtv

    REAL :: elt,vsc

    REAL, DIMENSION(kts:kte), INTENT(IN) :: theta
    REAL, DIMENSION(kts:kte) :: qtke,elBLmin,elBLavg,thetaw
    REAL :: wt,wt2,zi,zi2,h1,h2,hs,elBLmin0,elBLavg0,cldavg

    ! THE FOLLOWING CONSTANTS ARE IMPORTANT FOR REGULATING THE
    ! MIXING LENGTHS:
    REAL :: cns,   &   !< for surface layer (els) in stable conditions
            alp1,  &   !< for turbulent length scale (elt)
            alp2,  &   !< for buoyancy length scale (elb)
            alp3,  &   !< for buoyancy enhancement factor of elb
            alp4,  &   !< for surface layer (els) in unstable conditions
            alp5,  &   !< for BouLac mixing length or above PBLH
            alp6       !< for mass-flux/

    !THE FOLLOWING LIMITS DO NOT DIRECTLY AFFECT THE ACTUAL PBLH.
    !THEY ONLY IMPOSE LIMITS ON THE CALCULATION OF THE MIXING LENGTH 
    !SCALES SO THAT THE BOULAC MIXING LENGTH (IN FREE ATMOS) DOES
    !NOT ENCROACH UPON THE BOUNDARY LAYER MIXING LENGTH (els, elb & elt).
    REAL, PARAMETER :: minzi = 300.  !< min mixed-layer height
    REAL, PARAMETER :: maxdz = 750.  !< max (half) transition layer depth
                                     !! =0.3*2500 m PBLH, so the transition
                                     !! layer stops growing for PBLHs > 2.5 km.
    REAL, PARAMETER :: mindz = 300.  !< 300  !min (half) transition layer depth

    !SURFACE LAYER LENGTH SCALE MODS TO REDUCE IMPACT IN UPPER BOUNDARY LAYER
    REAL, PARAMETER :: ZSLH = 100. !< Max height correlated to surface conditions (m)
    REAL, PARAMETER :: CSL = 2.    !< CSL = constant of proportionality to L O(1)


    INTEGER :: i,j,k
    REAL :: afk,abk,zwk,zwk1,dzk,qdz,vflx,bv,tau_cloud,wstar,elb,els, &
            & elf,el_stab,el_mf,el_stab_mf,elb_mf,                    &
            & PBLH_PLUS_ENT,Uonset,Ugrid,wt_u,el_les
    REAL, PARAMETER :: ctau = 1000. !constant for tau_cloud

!    tv0 = 0.61*tref
!    gtr = 9.81/tref

    SELECT CASE(bl_mynn_mixlength)

      CASE (0) ! ORIGINAL MYNN MIXING LENGTH + BouLac

        cns  = 2.7
        alp1 = 0.23
        alp2 = 1.0
        alp3 = 5.0
        alp4 = 100.
        alp5 = 0.3

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

        !   **  Strictly, el(i,k=1) is not zero.  **
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

           !   **  Length scale in the surface layer  **
           IF ( rmo .GT. 0.0 ) THEN
              els  = karman*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
           ELSE
              els  =  karman*zwk*( 1.0 - alp4* zwk*rmo )**0.2
           END IF

           !   ** HARMONC AVERGING OF MIXING LENGTH SCALES:
           !       el(k) =      MIN(elb/( elb/elt+elb/els+1.0 ),elf)
           !       el(k) =      elb/( elb/elt+elb/els+1.0 )

           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5

           el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)

        END DO

      CASE (1) !NONLOCAL (using BouLac) FORM OF MIXING LENGTH

        ugrid = sqrt(u1(kts)**2 + v1(kts)**2)
        uonset= 15. 
        wt_u = (1.0 - min(max(ugrid - uonset, 0.0)/30.0, 0.5)) 
        cns  = 3.5
        alp1 = 0.22 !was 0.21
        alp2 = 0.25 !was 0.3
        alp3 = 2.0 * wt_u !taper off bouyancy enhancement in shear-driven pbls
        alp4 = 5.0
        alp5 = 0.3
        alp6 = 50.

        ! Impose limits on the height integration for elt and the transition layer depth
        zi2=MAX(zi,200.) !minzi)
        h1=MAX(0.3*zi2,200.)
        h1=MIN(h1,500.)          ! 1/2 transition layer depth
        h2=h1/2.0                ! 1/4 transition layer depth

        qtke(kts)=MAX(0.5*qke(kts), 0.01) !tke at full sigma levels
        thetaw(kts)=theta(kts)            !theta at full-sigma levels
        qkw(kts) = SQRT(MAX(qke(kts),1.0e-10))

        DO k = kts+1,kte
           afk = dz(k)/( dz(k)+dz(k-1) )
           abk = 1.0 -afk
           qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
           qtke(k) = 0.5*(qkw(k)**2)     ! q -> TKE
           thetaw(k)= theta(k)*abk + theta(k-1)*afk
        END DO

        elt = 1.0e-5
        vsc = 1.0e-5

        !   **  Strictly, zwk*h(i,j) -> ( zwk*h(i,j)+z0 )  **
        k = kts+1
        zwk = zw(k)
        DO WHILE (zwk .LE. zi2+h1)
           dzk = 0.5*( dz(k)+dz(k-1) )
           qdz = min(max( qkw(k)-qmin, 0.03 ), 30.0)*dzk
           elt = elt +qdz*zwk
           vsc = vsc +qdz
           k   = k+1
           zwk = zw(k)
        END DO

        elt = MIN( MAX( alp1*elt/vsc, 10.), 400.)
        !avoid use of buoyancy flux functions which are ill-defined at the surface
        !vflx = ( vt(kts)+1.0 )*flt + ( vq(kts)+tv0 )*flq
        vflx = flt
        vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**onethird

        !   **  Strictly, el(i,j,1) is not zero.  **
        el(kts) = 0.0
        zwk1    = zw(kts+1)              !full-sigma levels

        ! COMPUTE BouLac mixing length
        CALL boulac_length(kts,kte,zw,dz,qtke,thetaw,elBLmin,elBLavg)

        DO k = kts+1,kte
           zwk = zw(k)              !full-sigma levels

           !   **  Length scale limited by the buoyancy effect  **
           IF ( dtv(k) .GT. 0.0 ) THEN
              bv  = max( sqrt( gtr*dtv(k) ), 0.001)
              elb = MAX(alp2*qkw(k),                          &
                  &    alp6*edmf_a1(k-1)*edmf_w1(k-1)) / bv   &
                  &  *( 1.0 + alp3*SQRT( vsc/(bv*elt) ) )
              elb = MIN(elb, zwk)
              elf = 0.65 * qkw(k)/bv
              elBLavg(k) = MAX(elBLavg(k), alp6*edmf_a1(k-1)*edmf_w1(k-1)/bv)
           ELSE
              elb = 1.0e10
              elf = elb
           ENDIF

           !   **  Length scale in the surface layer  **
           IF ( rmo .GT. 0.0 ) THEN
              els  = karman*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
           ELSE
              els  =  karman*zwk*( 1.0 - alp4* zwk*rmo )**0.2
           END IF

           !   ** NOW BLEND THE MIXING LENGTH SCALES:
           wt=.5*TANH((zwk - (zi2+h1))/h2) + .5

           !add blending to use BouLac mixing length in free atmos;
           !defined relative to the PBLH (zi) + transition layer (h1)
           !el(k) = MIN(elb/( elb/elt+elb/els+1.0 ),elf)
           !try squared-blending
           el(k) = SQRT( els**2/(1. + (els**2/elt**2) +(els**2/elb**2)))
           el(k) = MIN (el(k), elf)
           el(k) = el(k)*(1.-wt) + alp5*elBLavg(k)*wt

           ! include scale-awareness, except for original MYNN
           el(k) = el(k)*Psig_bl

         END DO

      CASE (2) !Local (mostly) mixing length formulation

        Uonset = 3.5 + dz(kts)*0.1
        Ugrid  = sqrt(u1(kts)**2 + v1(kts)**2)
        cns  = 3.5 !JOE-test  * (1.0 - MIN(MAX(Ugrid - Uonset, 0.0)/10.0, 1.0))
        alp1 = 0.22 !0.21
        alp2 = 0.25 !0.30
        alp3 = 2.0  !1.5
        alp4 = 5.0
        alp5 = alp2 !like alp2, but for free atmosphere
        alp6 = 50.0 !used for MF mixing length

        ! Impose limits on the height integration for elt and the transition layer depth
        !zi2=MAX(zi,minzi)
        zi2=MAX(zi,    200.)
        !h1=MAX(0.3*zi2,mindz)
        !h1=MIN(h1,maxdz)         ! 1/2 transition layer depth
        h1=MAX(0.3*zi2,200.)
        h1=MIN(h1,500.)
        h2=h1*0.5                ! 1/4 transition layer depth

        qtke(kts)=MAX(0.5*qke(kts),0.01) !tke at full sigma levels
        qkw(kts) = SQRT(MAX(qke(kts),1.0e-4))

        DO k = kts+1,kte
           afk = dz(k)/( dz(k)+dz(k-1) )
           abk = 1.0 -afk
           qkw(k) = SQRT(MAX(qke(k)*abk+qke(k-1)*afk,1.0e-3))
           qtke(k) = 0.5*qkw(k)**2  ! qkw -> TKE
        END DO

        elt = 1.0e-5
        vsc = 1.0e-5

        !   **  Strictly, zwk*h(i,j) -> ( zwk*h(i,j)+z0 )  **
        PBLH_PLUS_ENT = MAX(zi+h1, 100.)
        k = kts+1
        zwk = zw(k)
        DO WHILE (zwk .LE. PBLH_PLUS_ENT)
           dzk = 0.5*( dz(k)+dz(k-1) )
           qdz = min(max( qkw(k)-qmin, 0.03 ), 30.0)*dzk
           elt = elt +qdz*zwk
           vsc = vsc +qdz
           k   = k+1
           zwk = zw(k)
        END DO

        elt = MIN( MAX(alp1*elt/vsc, 10.), 400.)
        !avoid use of buoyancy flux functions which are ill-defined at the surface
        !vflx = ( vt(kts)+1.0 )*flt +( vq(kts)+tv0 )*flq
        vflx = flt
        vsc = ( gtr*elt*MAX( vflx, 0.0 ) )**onethird

        !   **  Strictly, el(i,j,1) is not zero.  **
        el(kts) = 0.0
        zwk1    = zw(kts+1)

        DO k = kts+1,kte
           zwk = zw(k)              !full-sigma levels
           dzk = 0.5*( dz(k)+dz(k-1) )
           cldavg = 0.5*(cldfra_bl1D(k-1)+cldfra_bl1D(k))

           !   **  Length scale limited by the buoyancy effect  **
           IF ( dtv(k) .GT. 0.0 ) THEN
              !impose min value on bv
              bv  = MAX( SQRT( gtr*dtv(k) ), 0.001)  
              !elb_mf = alp2*qkw(k) / bv  &
              elb_mf = MAX(alp2*qkw(k),                    &
                  &    alp6*edmf_a1(k-1)*edmf_w1(k-1)) / bv    &
                  &  *( 1.0 + alp3*SQRT( vsc/( bv*elt ) ) )
              elb = MIN(MAX(alp5*qkw(k), alp6*edmf_a1(k)*edmf_w1(k))/bv, zwk)

              !tau_cloud = MIN(MAX(0.5*zi/((gtr*zi*MAX(vflx,1.0e-4))**onethird),30.),150.)
              wstar = 1.25*(gtr*zi*MAX(vflx,1.0e-4))**onethird
              tau_cloud = MIN(MAX(ctau * wstar/grav, 30.), 150.)
              !minimize influence of surface heat flux on tau far away from the PBLH.
              wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
              tau_cloud = tau_cloud*(1.-wt) + 50.*wt
              elf = MIN(MAX(tau_cloud*SQRT(MIN(qtke(k),40.)), &
                  &         alp6*edmf_a1(k)*edmf_w1(k)/bv), zwk)

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
              !tau_cloud = MIN(MAX(0.5*zi/((gtr*zi*MAX(vflx,1.0e-4))**onethird),50.),150.)
              wstar = 1.25*(gtr*zi*MAX(vflx,1.0e-4))**onethird
              tau_cloud = MIN(MAX(ctau * wstar/grav, 50.), 200.)
              !minimize influence of surface heat flux on tau far away from the PBLH.
              wt=.5*TANH((zwk - (zi2+h1))/h2) + .5
              !tau_cloud = tau_cloud*(1.-wt) + 50.*wt
              tau_cloud = tau_cloud*(1.-wt) + MAX(100.,dzk*0.25)*wt

              elb = MIN(tau_cloud*SQRT(MIN(qtke(k),40.)), zwk)
              !elf = elb
              elf = elb !/(1. + (elb/800.))  !bound free-atmos mixing length to < 800 m.
              elb_mf = elb
         END IF
         elf    = elf/(1. + (elf/800.))  !bound free-atmos mixing length to < 800 m.
         elb_mf = MAX(elb_mf, 0.01) !to avoid divide-by-zero below

         !   **  Length scale in the surface layer  **
         IF ( rmo .GT. 0.0 ) THEN
            els  = karman*zwk/(1.0+cns*MIN( zwk*rmo, zmax ))
         ELSE
            els  =  karman*zwk*( 1.0 - alp4* zwk*rmo )**0.2
         END IF

         !   ** NOW BLEND THE MIXING LENGTH SCALES:
         wt=.5*TANH((zwk - (zi2+h1))/h2) + .5

         !try squared-blending
         el(k) = SQRT( els**2/(1. + (els**2/elt**2) +(els**2/elb_mf**2)))
         el(k) = el(k)*(1.-wt) + elf*wt

         ! include scale-awareness. For now, use simple asymptotic kz -> 12 m (should be ~dz).
         el_les= MIN(els/(1. + (els/12.)), elb_mf)
         el(k) = el(k)*Psig_bl + (1.-Psig_bl)*el_les

       END DO

    END SELECT


#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mym_length

! ==================================================================
!>\ingroup gsd_mynn_edmf
!! This subroutine was taken from the BouLac scheme in WRF-ARW and modified for
!! integration into the MYNN PBL scheme. WHILE loops were added to reduce the
!! computational expense. This subroutine computes the length scales up and down
!! and then computes the min, average of the up/down length scales, and also
!! considers the distance to the surface.
!\param dlu  the distance a parcel can be lifted upwards give a finite
!  amount of TKE.
!\param dld  the distance a parcel can be displaced downwards given a
!  finite amount of TKE.
!\param lb1  the minimum of the length up and length down
!\param lb2  the average of the length up and length down
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
     dlu=zw(kte+1)-zw(k)-dz(k)*0.5
     zzz=0.
     zup_inf=0.
     beta=gtr           !Buoyancy coefficient (g/tref)

     !print*,"FINDING Dup, k=",k," zw=",zw(k)

     if (k .lt. kte) then      !cant integrate upwards from highest level
        found = 0
        izz=k
        DO WHILE (found .EQ. 0)

           if (izz .lt. kte) then
              dzt=dz(izz)                   ! layer depth above
              zup=zup-beta*theta(k)*dzt     ! initial PE the parcel has at k
              !print*,"  ",k,izz,theta(izz),dz(izz)
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt*0.5 ! PE gained by lifting a parcel to izz+1
              zzz=zzz+dzt                   ! depth of layer k to izz+1
              !print*,"  PE=",zup," TKE=",qtke(k)," z=",zw(izz)
              if (qtke(k).lt.zup .and. qtke(k).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    !fractional distance up into the layer where TKE becomes < PE
                    tl=(-beta*(theta(izz)-theta(k)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(k)))**2 + &
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
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt*0.5
              zzz=zzz+dzt
              !print*,"  PE=",zdo," TKE=",qtke(k)," z=",zw(izz)
              if (qtke(k).lt.zdo .and. qtke(k).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(k))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(k)))**2 + &
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
!>\ingroup gsd_mynn_edmf
!! This subroutine was taken from the BouLac scheme in WRF-ARW
!! and modified for integration into the MYNN PBL scheme.
!! WHILE loops were added to reduce the computational expense.
!! This subroutine computes the length scales up and down
!! and then computes the min, average of the up/down
!! length scales, and also considers the distance to the
!! surface.
  SUBROUTINE boulac_length(kts,kte,zw,dz,qtke,theta,lb1,lb2)
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
        dlu(iz)=zw(kte+1)-zw(iz)-dz(iz)*0.5
        zzz=0.
        zup_inf=0.
        beta=gtr           !Buoyancy coefficient (g/tref)

        !print*,"FINDING Dup, k=",iz," zw=",zw(iz)

        if (iz .lt. kte) then      !cant integrate upwards from highest level

          found = 0
          izz=iz
          DO WHILE (found .EQ. 0)

            if (izz .lt. kte) then
              dzt=dz(izz)                    ! layer depth above
              zup=zup-beta*theta(iz)*dzt     ! initial PE the parcel has at iz
              !print*,"  ",iz,izz,theta(izz),dz(izz)
              zup=zup+beta*(theta(izz+1)+theta(izz))*dzt*0.5 ! PE gained by lifting a parcel to izz+1
              zzz=zzz+dzt                   ! depth of layer iz to izz+1
              !print*,"  PE=",zup," TKE=",qtke(iz)," z=",zw(izz)
              if (qtke(iz).lt.zup .and. qtke(iz).ge.zup_inf) then
                 bbb=(theta(izz+1)-theta(izz))/dzt
                 if (bbb .ne. 0.) then
                    !fractional distance up into the layer where TKE becomes < PE
                    tl=(-beta*(theta(izz)-theta(iz)) + &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2 + &
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
              zdo=zdo-beta*(theta(izz-1)+theta(izz))*dzt*0.5
              zzz=zzz+dzt
              !print*,"  PE=",zdo," TKE=",qtke(iz)," z=",zw(izz)
              if (qtke(iz).lt.zdo .and. qtke(iz).ge.zdo_sup) then
                 bbb=(theta(izz)-theta(izz-1))/dzt
                 if (bbb .ne. 0.) then
                    tl=(beta*(theta(izz)-theta(iz))+ &
                      & sqrt( max(0.,(beta*(theta(izz)-theta(iz)))**2 + &
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
!       closure        : closure level (2.5, 2.6, or 3.0)
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
!>\ingroup gsd_mynn_edmf
!! This subroutine calculates the vertical diffusivity coefficients and the 
!! production terms for the turbulent quantities.      
!>\section gen_mym_turbulence GSD mym_turbulence General Algorithm
!! Two subroutines mym_level2() and mym_length() are called within this
!!subrouine to collect variable to carry out successive calculations:
!! - mym_level2() calculates the level 2 nondimensional wind shear \f$G_M\f$
!! and vertical temperature gradient \f$G_H\f$ as well as the level 2 stability
!! functions \f$S_h\f$ and \f$S_m\f$.
!! - mym_length() calculates the mixing lengths.
!! - The stability criteria from Helfand and Labraga (1989) are applied.
!! - The stability functions for level 2.5 or level 3.0 are calculated.
!! - If level 3.0 is used, counter-gradient terms are calculated.
!! - Production terms of TKE,\f$\theta^{'2}\f$,\f$q^{'2}\f$, and \f$\theta^{'}q^{'}\f$
!! are calculated.
!! - Eddy diffusivity \f$K_h\f$ and eddy viscosity \f$K_m\f$ are calculated.
!! - TKE budget terms are calculated (if the namelist parameter \p tke_budget 
!! is set to True)
  SUBROUTINE  mym_turbulence (                                &
    &            kts,kte,                                     &
    &            xland,closure,                               &
    &            dz, dx, zw,                                  &
    &            u, v, thl, thetav, ql, qw,                   &
    &            thlsg, qwsg,                                 &
    &            qke, tsq, qsq, cov,                          &
    &            vt, vq,                                      &
    &            rmo, flt, flq,                               &
    &            zi,theta,                                    &
    &            sh, sm,                                      &
    &            El,                                          &
    &            Dfm, Dfh, Dfq, Tcd, Qcd, Pdk, Pdt, Pdq, Pdc, &
    &		 qWT1D,qSHEAR1D,qBUOY1D,qDISS1D,              &
    &            tke_budget,                                  &
    &            Psig_bl,Psig_shcu,cldfra_bl1D,               &
    &            bl_mynn_mixlength,                           &
    &            edmf_w1,edmf_a1,                             &
    &            TKEprodTD,                                   &
    &            spp_pbl,rstoch_col)

!-------------------------------------------------------------------
!
    INTEGER, INTENT(IN)   :: kts,kte

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    INTEGER, INTENT(IN)   :: bl_mynn_mixlength,tke_budget
    REAL, INTENT(IN)      :: closure
    REAL, DIMENSION(kts:kte), INTENT(in) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: zw
    REAL, INTENT(in) :: rmo,flt,flq,Psig_bl,Psig_shcu,dx,xland,zi
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,thl,thetav,qw,& 
         &ql,vt,vq,qke,tsq,qsq,cov,cldfra_bl1D,edmf_w1,edmf_a1,&
         &TKEprodTD,thlsg,qwsg

    REAL, DIMENSION(kts:kte), INTENT(out) :: dfm,dfh,dfq,&
         &pdk,pdt,pdq,pdc,tcd,qcd,el

    REAL, DIMENSION(kts:kte), INTENT(inout) :: &
         qWT1D,qSHEAR1D,qBUOY1D,qDISS1D
    REAL :: q3sq_old,dlsq1,qWTP_old,qWTP_new
    REAL :: dudz,dvdz,dTdz,&
            upwp,vpwp,Tpwp

    REAL, DIMENSION(kts:kte) :: qkw,dtl,dqw,dtv,gm,gh,sm,sh

    INTEGER :: k
!    REAL :: cc2,cc3,e1c,e2c,e3c,e4c,e5c
    REAL :: e6c,dzk,afk,abk,vtt,vqq,&
         &cw25,clow,cupp,gamt,gamq,smd,gamv,elq,elh

    REAL :: cldavg
    REAL, DIMENSION(kts:kte), INTENT(in) :: theta

    REAL ::  a2fac, duz, ri !JOE-Canuto/Kitamura mod

    REAL:: auh,aum,adh,adm,aeh,aem,Req,Rsl,Rsl2,&
           gmelq,sm20,sh20,sm25max,sh25max,sm25min,sh25min,&
           sm_pbl,sh_pbl,zi2,wt,slht,wtpr

    DOUBLE PRECISION  q2sq, t2sq, r2sq, c2sq, elsq, gmel, ghel
    DOUBLE PRECISION  q3sq, t3sq, r3sq, c3sq, dlsq, qdiv
    DOUBLE PRECISION  e1, e2, e3, e4, enum, eden, wden

!   Stochastic
    INTEGER,  INTENT(IN)                          ::    spp_pbl
    REAL, DIMENSION(KTS:KTE)                      ::    rstoch_col
    REAL :: Prnum, Prlim
    REAL, PARAMETER :: Prlimit = 5.0


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

    CALL mym_level2 (kts,kte,                   &
    &            dz,                            &
    &            u, v, thl, thetav, qw,         &
    &            thlsg, qwsg,                   &
    &            ql, vt, vq,                    &
    &            dtl, dqw, dtv, gm, gh, sm, sh  )
!
    CALL mym_length (                           &
    &            kts,kte,xland,                 &
    &            dz, dx, zw,                    &
    &            rmo, flt, flq,                 &
    &            vt, vq,                        &
    &            u, v, qke,                     &
    &            dtv,                           &
    &            el,                            &
    &            zi,theta,                      &
    &            qkw,Psig_bl,cldfra_bl1D,       &
    &            bl_mynn_mixlength,             &
    &            edmf_w1,edmf_a1                )
!

    DO k = kts+1,kte
       dzk = 0.5  *( dz(k)+dz(k-1) )
       afk = dz(k)/( dz(k)+dz(k-1) )
       abk = 1.0 -afk
       elsq = el (k)**2
       q3sq = qkw(k)**2
       q2sq = b1*elsq*( sm(k)*gm(k)+sh(k)*gh(k) )

       sh20 = MAX(sh(k), 1e-5)
       sm20 = MAX(sm(k), 1e-5)
       sh(k)= MAX(sh(k), 1e-5)

       !Canuto/Kitamura mod
       duz = ( u(k)-u(k-1) )**2 +( v(k)-v(k-1) )**2
       duz =   duz                    /dzk**2
       !   **  Gradient Richardson number  **
       ri = -gh(k)/MAX( duz, 1.0e-10 )
       IF (CKmod .eq. 1) THEN
          a2fac = 1./(1. + MAX(ri,0.0))
       ELSE
          a2fac = 1.
       ENDIF
       !end Canuto/Kitamura mod

       !level 2.0 Prandtl number
       !Prnum = MIN(sm20/sh20, 4.0)
       !The form of Zilitinkevich et al. (2006) but modified
       !half-way towards Esau and Grachev (2007, Wind Eng)
       !Prnum = MIN(0.76 + 3.0*MAX(ri,0.0), Prlimit)
       Prnum = MIN(0.76 + 4.0*MAX(ri,0.0), Prlimit)
       !Prnum = MIN(0.76 + 5.0*MAX(ri,0.0), Prlimit)
!
!  Modified: Dec/22/2005, from here, (dlsq -> elsq)
       gmel = gm (k)*elsq
       ghel = gh (k)*elsq
!  Modified: Dec/22/2005, up to here

       ! Level 2.0 debug prints
       IF ( debug_code ) THEN
         IF (sh(k)<0.0 .OR. sm(k)<0.0) THEN
           print*,"MYNN; mym_turbulence 2.0; sh=",sh(k)," k=",k
           print*," gm=",gm(k)," gh=",gh(k)," sm=",sm(k)
           print*," q2sq=",q2sq," q3sq=",q3sq," q3/q2=",q3sq/q2sq
           print*," qke=",qke(k)," el=",el(k)," ri=",ri
           print*," PBLH=",zi," u=",u(k)," v=",v(k)
         ENDIF
       ENDIF

!     **  Since qkw is set to more than 0.0, q3sq > 0.0.  **

!     new stability criteria in level 2.5 (as well as level 3) - little/no impact
!     **  Limitation on q, instead of L/q  **
       dlsq =  elsq
       IF ( q3sq/dlsq .LT. -gh(k) ) q3sq = -dlsq*gh(k)

       IF ( q3sq .LT. q2sq ) THEN
          !Apply Helfand & Labraga mod
          qdiv = SQRT( q3sq/q2sq )   !HL89: (1-alfa)
!
          !Use level 2.5 stability functions
          !e1   = q3sq - e1c*ghel*a2fac
          !e2   = q3sq - e2c*ghel*a2fac
          !e3   = e1   + e3c*ghel*a2fac**2
          !e4   = e1   - e4c*ghel*a2fac
          !eden = e2*e4 + e3*e5c*gmel
          !eden = MAX( eden, 1.0d-20 )
          !sm(k) = q3sq*a1*( e3-3.0*c1*e4       )/eden
          !!JOE-Canuto/Kitamura mod
          !!sh(k) = q3sq*a2*( e2+3.0*c1*e5c*gmel )/eden
          !sh(k) = q3sq*(a2*a2fac)*( e2+3.0*c1*e5c*gmel )/eden
          !sm(k) = Prnum*sh(k)
          !sm(k) = sm(k) * qdiv

          !Use level 2.0 functions as in original MYNN
          sh(k) = sh(k) * qdiv
          sm(k) = sm(k) * qdiv
        !  !sm_pbl = sm(k) * qdiv
        !
        !  !Or, use the simple Pr relationship
        !  sm(k) = Prnum*sh(k)
        !
        !  !or blend them:
        !  zi2   = MAX(zi, 300.)
        !  wt    =.5*TANH((zw(k) - zi2)/200.) + .5
        !  sm(k) = sm_pbl*(1.-wt) + sm(k)*wt

          !Recalculate terms for later use
          !JOE-Canuto/Kitamura mod
          !e1   = q3sq - e1c*ghel * qdiv**2
          !e2   = q3sq - e2c*ghel * qdiv**2
          !e3   = e1   + e3c*ghel * qdiv**2
          !e4   = e1   - e4c*ghel * qdiv**2
          e1   = q3sq - e1c*ghel*a2fac * qdiv**2
          e2   = q3sq - e2c*ghel*a2fac * qdiv**2
          e3   = e1   + e3c*ghel*a2fac**2 * qdiv**2
          e4   = e1   - e4c*ghel*a2fac * qdiv**2
          eden = e2*e4 + e3*e5c*gmel * qdiv**2
          eden = MAX( eden, 1.0d-20 )
          !!JOE-Canuto/Kitamura mod
          !!sh(k) = q3sq*a2*( e2+3.0*c1*e5c*gmel )/eden  - retro 5
          !sh(k) = q3sq*(a2*a2fac)*( e2+3.0*c1*e5c*gmel )/eden
          !sm(k) = Prnum*sh(k)
       ELSE
          !JOE-Canuto/Kitamura mod
          !e1   = q3sq - e1c*ghel
          !e2   = q3sq - e2c*ghel
          !e3   = e1   + e3c*ghel
          !e4   = e1   - e4c*ghel
          e1   = q3sq - e1c*ghel*a2fac
          e2   = q3sq - e2c*ghel*a2fac
          e3   = e1   + e3c*ghel*a2fac**2
          e4   = e1   - e4c*ghel*a2fac
          eden = e2*e4 + e3*e5c*gmel
          eden = MAX( eden, 1.0d-20 )

          qdiv = 1.0
          !Use level 2.5 stability functions
          sm(k) = q3sq*a1*( e3-3.0*c1*e4       )/eden
        !  sm_pbl = q3sq*a1*( e3-3.0*c1*e4       )/eden
          !!JOE-Canuto/Kitamura mod
          !!sh(k) = q3sq*a2*( e2+3.0*c1*e5c*gmel )/eden
          sh(k) = q3sq*(a2*a2fac)*( e2+3.0*c1*e5c*gmel )/eden
        !  sm(k) = Prnum*sh(k)

        !  !or blend them:
        !  zi2   = MAX(zi, 300.)
        !  wt    = .5*TANH((zw(k) - zi2)/200.) + .5
        !  sm(k) = sm_pbl*(1.-wt) + sm(k)*wt
       END IF !end Helfand & Labraga check

       !Impose broad limits on Sh and Sm:
       gmelq    = MAX(gmel/q3sq, 1e-8)
       sm25max  = 4.  !MIN(sm20*3.0, SQRT(.1936/gmelq))
       sh25max  = 4.  !MIN(sh20*3.0, 0.76*b2)
       sm25min  = 0.0 !MAX(sm20*0.1, 1e-6)
       sh25min  = 0.0 !MAX(sh20*0.1, 1e-6)

       !JOE: Level 2.5 debug prints
       ! HL88 , lev2.5 criteria from eqs. 3.17, 3.19, & 3.20
       IF ( debug_code ) THEN
         IF ((sh(k)<sh25min .OR. sm(k)<sm25min .OR. &
              sh(k)>sh25max .OR. sm(k)>sm25max) ) THEN
           print*,"In mym_turbulence 2.5: k=",k
           print*," sm=",sm(k)," sh=",sh(k)
           print*," ri=",ri," Pr=",sm(k)/MAX(sh(k),1e-8)
           print*," gm=",gm(k)," gh=",gh(k)
           print*," q2sq=",q2sq," q3sq=",q3sq, q3sq/q2sq
           print*," qke=",qke(k)," el=",el(k)
           print*," PBLH=",zi," u=",u(k)," v=",v(k)
           print*," SMnum=",q3sq*a1*( e3-3.0*c1*e4)," SMdenom=",eden
           print*," SHnum=",q3sq*(a2*a2fac)*( e2+3.0*c1*e5c*gmel ),&
                  " SHdenom=",eden
         ENDIF
       ENDIF

       !Enforce constraints for level 2.5 functions
       IF ( sh(k) > sh25max ) sh(k) = sh25max
       IF ( sh(k) < sh25min ) sh(k) = sh25min
       !IF ( sm(k) > sm25max ) sm(k) = sm25max
       !IF ( sm(k) < sm25min ) sm(k) = sm25min
       !sm(k) = Prnum*sh(k)

       !surface layer PR
       !slht  = zi*0.1
       !wtpr  = min( max( (slht - zw(k))/slht, 0.0), 1.0) ! 1 at z=0, 0 above sfc layer
       !Prlim = 1.0*wtpr + (1.0 - wtpr)*Prlimit
       !Prlim = 2.0*wtpr + (1.0 - wtpr)*Prlimit
       !sm(k) = MIN(sm(k), Prlim*Sh(k))
       !Pending more testing, keep same Pr limit in sfc layer
       sm(k) = MIN(sm(k), Prlimit*Sh(k))

!   **  Level 3 : start  **
       IF ( closure .GE. 3.0 ) THEN
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
          ! Use Janjic's (2001; p 13-17) methodology (eqs 4.11-414 and 5.7-5.10)
          ! to calculate an exact limit for c3sq:
          auh = 27.*a1*((a2*a2fac)**2)*b2*(gtr)**2
          aum = 54.*(a1**2)*(a2*a2fac)*b2*c1*(gtr)
          adh = 9.*a1*((a2*a2fac)**2)*(12.*a1 + 3.*b2)*(gtr)**2
          adm = 18.*(a1**2)*(a2*a2fac)*(b2 - 3.*(a2*a2fac))*(gtr)

          aeh = (9.*a1*((a2*a2fac)**2)*b1 +9.*a1*((a2*a2fac)**2)* &
                (12.*a1 + 3.*b2))*(gtr)
          aem = 3.*a1*(a2*a2fac)*b1*(3.*(a2*a2fac) + 3.*b2*c1 + &
                (18.*a1*c1 - b2)) + &
                (18.)*(a1**2)*(a2*a2fac)*(b2 - 3.*(a2*a2fac))

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
          e2   = q3sq - e2c*ghel*a2fac * qdiv**2
          e3   = q3sq + e3c*ghel*a2fac**2 * qdiv**2
          e4   = q3sq - e4c*ghel*a2fac * qdiv**2
          eden = e2*e4  + e3 *e5c*gmel * qdiv**2

          !JOE-Canuto/Kitamura mod
          !wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
          !     &        *( e2*e4c - e3c*e5c*gmel * qdiv**2 )
          wden = cc3*gtr**2 * dlsq**2/elsq * qdiv**2 &
               &        *( e2*e4c*a2fac - e3c*e5c*gmel*a2fac**2 * qdiv**2 )

          IF ( wden .NE. 0.0 ) THEN
             !JOE: test dynamic limits
             clow = q3sq*( 0.12-cw25 )*eden/wden
             cupp = q3sq*( 0.76-cw25 )*eden/wden
             !clow = q3sq*( Rsl -cw25 )*eden/wden
             !cupp = q3sq*( Rsl2-cw25 )*eden/wden
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
          e6c  = 3.0*(a2*a2fac)*cc3*gtr * dlsq/elsq

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
          smd  = dlsq*enum*gtr/eden * qdiv**2 * (e3c*a2fac**2 + &
               & e4c*a2fac)*a1/(a2*a2fac)

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
!      Add min background stability function (diffusivity) within model levels
!      with active plumes and clouds.
       cldavg = 0.5*(cldfra_bl1D(k-1) + cldfra_bl1D(k))
       IF (edmf_a1(k) > 0.001 .OR. cldavg > 0.02) THEN
           ! for mass-flux columns
           sm(k) = MAX(sm(k), 0.03*MIN(10.*edmf_a1(k)*edmf_w1(k),1.0) )
           sh(k) = MAX(sh(k), 0.03*MIN(10.*edmf_a1(k)*edmf_w1(k),1.0) )
           ! for clouds
           sm(k) = MAX(sm(k), 0.05*MIN(cldavg,1.0) )
           sh(k) = MAX(sh(k), 0.05*MIN(cldavg,1.0) )
       ENDIF
!
       elq = el(k)*qkw(k)
       elh = elq*qdiv

       ! Production of TKE (pdk), T-variance (pdt),
       ! q-variance (pdq), and covariance (pdc)
       pdk(k) = elq*( sm(k)*gm(k)                &
            &        +sh(k)*gh(k)+gamv ) +       &
            &   TKEprodTD(k)
       pdt(k) = elh*( sh(k)*dtl(k)+gamt )*dtl(k)
       pdq(k) = elh*( sh(k)*dqw(k)+gamq )*dqw(k)
       pdc(k) = elh*( sh(k)*dtl(k)+gamt )        &
            &   *dqw(k)*0.5                      &
            & + elh*( sh(k)*dqw(k)+gamq )*dtl(k)*0.5

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

   IF (tke_budget .eq. 1) THEN
       !TKE BUDGET
!       dudz = ( u(k)-u(k-1) )/dzk
!       dvdz = ( v(k)-v(k-1) )/dzk
!       dTdz = ( thl(k)-thl(k-1) )/dzk

!       upwp = -elq*sm(k)*dudz
!       vpwp = -elq*sm(k)*dvdz
!       Tpwp = -elq*sh(k)*dTdz
!       Tpwp = SIGN(MAX(ABS(Tpwp),1.E-6),Tpwp)

       
!!  TKE budget  (Puhales, 2020, WRF 4.2.1)  << EOB   

       !!!Shear Term
       !!!qSHEAR1D(k)=-(upwp*dudz + vpwp*dvdz)
       qSHEAR1D(k) = elq*sm(k)*gm(k) !staggered

       !!!Buoyancy Term    
       !!!qBUOY1D(k)=grav*Tpwp/thl(k)
       !qBUOY1D(k)= elq*(sh(k)*gh(k) + gamv)
       !qBUOY1D(k) = elq*(sh(k)*(-dTdz*grav/thl(k)) + gamv) !! ORIGINAL CODE
       
       !! Buoyncy term takes the TKEprodTD(k) production now
       qBUOY1D(k) = elq*(sh(k)*gh(k)+gamv)+TKEprodTD(k) !staggered

       !!!Dissipation Term (now it evaluated on mym_predict)
       !qDISS1D(k) = (q3sq**(3./2.))/(b1*MAX(el(k),1.)) !! ORIGINAL CODE
       
       !! >> EOB
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
!>\ingroup gsd_mynn_edmf
!! This subroutine predicts the turbulent quantities at the next step.
  SUBROUTINE  mym_predict (kts,kte,                                     &
       &            closure,                                            &
       &            delt,                                               &
       &            dz,                                                 &
       &            ust, flt, flq, pmz, phh,                            &
       &            el, dfq, rho,                                       &
       &            pdk, pdt, pdq, pdc,                                 &
       &            qke, tsq, qsq, cov,                                 &
       &            s_aw,s_awqke,bl_mynn_edmf_tke,                      &
       &            qWT1D, qDISS1D,tke_budget)  !! TKE budget  (Puhales, 2020)

!-------------------------------------------------------------------
    INTEGER, INTENT(IN) :: kts,kte    

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    REAL, INTENT(IN)    :: closure
    INTEGER, INTENT(IN) :: bl_mynn_edmf_tke, tke_budget
    REAL, INTENT(IN)    :: delt
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz, dfq, el, rho
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: pdk, pdt, pdq, pdc
    REAL, INTENT(IN)    ::  flt, flq, ust, pmz, phh
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: qke,tsq, qsq, cov
! WA 8/3/15
    REAL, DIMENSION(kts:kte+1), INTENT(INOUT) :: s_awqke,s_aw
    
    !!  TKE budget  (Puhales, 2020, WRF 4.2.1)  << EOB 
    REAL, DIMENSION(kts:kte), INTENT(OUT) :: qWT1D, qDISS1D  
    REAL, DIMENSION(kts:kte) :: tke_up,dzinv  
    !! >> EOB
    
    INTEGER :: k
    REAL, DIMENSION(kts:kte) :: qkw, bp, rp, df3q
    REAL :: vkz,pdk1,phm,pdt1,pdq1,pdc1,b1l,b2l,onoff
    REAL, DIMENSION(kts:kte) :: dtz
    REAL, DIMENSION(kts:kte) :: a,b,c,d,x

    REAL, DIMENSION(kts:kte) :: rhoinv
    REAL, DIMENSION(kts:kte+1) :: rhoz,kqdz,kmdz

    ! REGULATE THE MOMENTUM MIXING FROM THE MASS-FLUX SCHEME (on or off)
    IF (bl_mynn_edmf_tke == 0) THEN
       onoff=0.0
    ELSE
       onoff=1.0
    ENDIF

!   **  Strictly, vkz*h(i,j) -> karman*( 0.5*dz(1)*h(i,j)+z0 )  **
    vkz = karman*0.5*dz(kts)
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
!JOE-add conservation + stability criteria
    !Prepare "constants" for diffusion equation.
    !khdz = rho*Kh/dz = rho*dfh
    rhoz(kts)  =rho(kts)
    rhoinv(kts)=1./rho(kts)
    kqdz(kts)  =rhoz(kts)*df3q(kts)
    kmdz(kts)  =rhoz(kts)*dfq(kts)
    DO k=kts+1,kte
       rhoz(k)  =(rho(k)*dz(k-1) + rho(k-1)*dz(k))/(dz(k-1)+dz(k))
       rhoz(k)  =  MAX(rhoz(k),1E-4)
       rhoinv(k)=1./MAX(rho(k),1E-4)
       kqdz(k)  = rhoz(k)*df3q(k) ! for TKE
       kmdz(k)  = rhoz(k)*dfq(k)  ! for T'2, q'2, and T'q'
    ENDDO
    rhoz(kte+1)=rhoz(kte)
    kqdz(kte+1)=rhoz(kte+1)*df3q(kte)
    kmdz(kte+1)=rhoz(kte+1)*dfq(kte)

    !stability criteria for mf
    DO k=kts+1,kte-1
       kqdz(k) = MAX(kqdz(k),  0.5* s_aw(k))
       kqdz(k) = MAX(kqdz(k), -0.5*(s_aw(k)-s_aw(k+1)))
       kmdz(k) = MAX(kmdz(k),  0.5* s_aw(k))
       kmdz(k) = MAX(kmdz(k), -0.5*(s_aw(k)-s_aw(k+1)))
    ENDDO
!JOE-end conservation mods

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
!       a(k)=   - dtz(k)*df3q(k) + 0.5*dtz(k)*s_aw(k)*onoff
!       b(k)=1. + dtz(k)*(df3q(k)+df3q(k+1)) &
!               + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))*onoff + bp(k)*delt
!       c(k)=   - dtz(k)*df3q(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
!       d(k)=rp(k)*delt + qke(k) + dtz(k)*(s_awqke(k)-s_awqke(k+1))*onoff
!JOE 8/22/20 improve conservation
       a(k)=   - dtz(k)*kqdz(k)*rhoinv(k)                       &
           &   + 0.5*dtz(k)*rhoinv(k)*s_aw(k)*onoff
       b(k)=1. + dtz(k)*(kqdz(k)+kqdz(k+1))*rhoinv(k)           &
           &   + 0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))*onoff &
           &   + bp(k)*delt
       c(k)=   - dtz(k)*kqdz(k+1)*rhoinv(k)                     &
           &   - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*onoff
       d(k)=rp(k)*delt + qke(k)                                 &
           &   + dtz(k)*rhoinv(k)*(s_awqke(k)-s_awqke(k+1))*onoff
    ENDDO

!!    DO k=kts+1,kte-1
!!       a(k-kts+1)=-dtz(k)*df3q(k)
!!       b(k-kts+1)=1.+dtz(k)*(df3q(k)+df3q(k+1))
!!       c(k-kts+1)=-dtz(k)*df3q(k+1)
!!       d(k-kts+1)=rp(k)*delt + qke(k) - qke(k)*bp(k)*delt
!!    ENDDO

!! "no flux at top"
!    a(kte)=-1. !0.
!    b(kte)=1.
!    c(kte)=0.
!    d(kte)=0.
!! "prescribed value"
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qke(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)

    DO k=kts,kte
!       qke(k)=max(d(k-kts+1), 1.e-4)
       qke(k)=max(x(k), 1.e-4)
       qke(k)=min(qke(k), 150.)
    ENDDO
      
   
!!  TKE budget  (Puhales, 2020, WRF 4.2.1)  << EOB 
    IF (tke_budget .eq. 1) THEN
       !! TKE Vertical transport << EOBvt
        tke_up=0.5*qke
        dzinv=1./dz
        k=kts
        qWT1D(k)=dzinv(k)*(                                    &
            &  (kqdz(k+1)*(tke_up(k+1)-tke_up(k))-kqdz(k)*tke_up(k)) &
            &  + 0.5*rhoinv(k)*(s_aw(k+1)*tke_up(k+1)          &
            &  +      (s_aw(k+1)-s_aw(k))*tke_up(k)            &
            &  +      (s_awqke(k)-s_awqke(k+1)))*onoff) !unstaggered
        DO k=kts+1,kte-1
            qWT1D(k)=dzinv(k)*(                                &
            & (kqdz(k+1)*(tke_up(k+1)-tke_up(k))-kqdz(k)*(tke_up(k)-tke_up(k-1))) &
            &  + 0.5*rhoinv(k)*(s_aw(k+1)*tke_up(k+1)          &
            &  +      (s_aw(k+1)-s_aw(k))*tke_up(k)            &
            &  -                  s_aw(k)*tke_up(k-1)          &
            &  +      (s_awqke(k)-s_awqke(k+1)))*onoff) !unstaggered
        ENDDO
        k=kte
        qWT1D(k)=dzinv(k)*(-kqdz(k)*(tke_up(k)-tke_up(k-1)) &
            &  + 0.5*rhoinv(k)*(-s_aw(k)*tke_up(k)-s_aw(k)*tke_up(k-1)+s_awqke(k))*onoff) !unstaggared
        !!  >> EOBvt
        qDISS1D=bp*tke_up !! TKE dissipation rate !unstaggered
    END IF
!! >> EOB 
   
    IF ( closure > 2.5 ) THEN

       !   **  Prediction of the moisture variance  **
       DO k = kts,kte-1
          b2l   = b2*0.5*( el(k+1)+el(k) )
          bp(k) = 2.*qkw(k) / b2l
          rp(k) = pdq(k+1) + pdq(k)
       END DO

       !zero gradient for qsq at bottom and top
       !a(1)=0.
       !b(1)=1.
       !c(1)=-1.
       !d(1)=0.

       ! Since dfq(kts)=0.0, a(1)=0.0 and b(1)=1.+dtz(k)*dfq(k+1)+bp(k)*delt.
       DO k=kts,kte-1
          a(k)=   - dtz(k)*kmdz(k)*rhoinv(k)
          b(k)=1. + dtz(k)*(kmdz(k)+kmdz(k+1))*rhoinv(k) + bp(k)*delt
          c(k)=   - dtz(k)*kmdz(k+1)*rhoinv(k)
          d(k)=rp(k)*delt + qsq(k)
       ENDDO

       a(kte)=-1. !0.
       b(kte)=1.
       c(kte)=0.
       d(kte)=0.

!       CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)
       
       DO k=kts,kte
          !qsq(k)=d(k-kts+1)
          qsq(k)=MAX(x(k),1e-17)
       ENDDO
    ELSE
       !level 2.5 - use level 2 diagnostic
       DO k = kts,kte-1
          IF ( qkw(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*0.25*( el(k+1)+el(k) )/qkw(k)
          END IF
          qsq(k) = b2l*( pdq(k+1)+pdq(k) )
       END DO
       qsq(kte)=qsq(kte-1)
    END IF
!!!!!!!!!!!!!!!!!!!!!!end level 2.6   

    IF ( closure .GE. 3.0 ) THEN
!
!   **  dfq for the scalar variance is 1.0*dfm.  **
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
          !a(k-kts+1)=-dtz(k)*dfq(k)
          !b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          !c(k-kts+1)=-dtz(k)*dfq(k+1)
          !d(k-kts+1)=rp(k)*delt + tsq(k)
!JOE 8/22/20 improve conservation
          a(k)=   - dtz(k)*kmdz(k)*rhoinv(k)
          b(k)=1. + dtz(k)*(kmdz(k)+kmdz(k+1))*rhoinv(k) + bp(k)*delt
          c(k)=   - dtz(k)*kmdz(k+1)*rhoinv(k)
          d(k)=rp(k)*delt + tsq(k)
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
          !a(k-kts+1)=-dtz(k)*dfq(k)
          !b(k-kts+1)=1.+dtz(k)*(dfq(k)+dfq(k+1))+bp(k)*delt
          !c(k-kts+1)=-dtz(k)*dfq(k+1)
          !d(k-kts+1)=rp(k)*delt + cov(k)
!JOE 8/22/20 improve conservation
          a(k)=   - dtz(k)*kmdz(k)*rhoinv(k)
          b(k)=1. + dtz(k)*(kmdz(k)+kmdz(k+1))*rhoinv(k) + bp(k)*delt
          c(k)=   - dtz(k)*kmdz(k+1)*rhoinv(k)
          d(k)=rp(k)*delt + cov(k)
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

       !Not level 3 - default to level 2 diagnostic
       DO k = kts,kte-1
          IF ( qkw(k) .LE. 0.0 ) THEN
             b2l = 0.0
          ELSE
             b2l = b2*0.25*( el(k+1)+el(k) )/qkw(k)
          END IF
!
          tsq(k) = b2l*( pdt(k+1)+pdt(k) )
          cov(k) = b2l*( pdc(k+1)+pdc(k) )
       END DO
       
       tsq(kte)=tsq(kte-1)
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
!     Work arrays/variables:
!       qmq             : Q_w-Q_{sl}, where Q_{sl} is the saturation
!                         specific humidity at T=Tl
!       alp(nx,nz,ny)   : Functions in the condensation process
!       bet(nx,nz,ny)   : ditto
!       sgm(nx,nz,ny)   : Combined standard deviation sigma_s
!                         multiplied by 2/alp
!
!     # qmq, alp, bet and sgm are allowed to share storage units with
!       any four of other work arrays for saving memory.
!
!     # Results are sensitive particularly to values of cp and r_d.
!       Set these values to those adopted by you.
!
!-------------------------------------------------------------------
!>\ingroup gsd_mynn_edmf 
!! This subroutine calculates the nonconvective component of the 
!! subgrid cloud fraction and mixing ratio as well as the functions used to 
!! calculate the buoyancy flux. Different cloud PDFs can be selected by
!! use of the namelist parameter \p bl_mynn_cloudpdf .
  SUBROUTINE  mym_condensation (kts,kte,   &
    &            dx, dz, zw, xland,        &
    &            thl, qw, qv, qc, qi,      &
    &            p,exner,                  &
    &            tsq, qsq, cov,            &
    &            Sh, el, bl_mynn_cloudpdf, &
    &            qc_bl1D, qi_bl1D,         &
    &            cldfra_bl1D,              &
    &            PBLH1,HFX1,               &
    &            Vt, Vq, th, sgm, rmo,     &
    &            spp_pbl,rstoch_col        )

!-------------------------------------------------------------------

    INTEGER, INTENT(IN)   :: kts,kte, bl_mynn_cloudpdf

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    REAL, INTENT(IN)      :: dx,PBLH1,HFX1,rmo,xland
    REAL, DIMENSION(kts:kte), INTENT(IN) :: dz
    REAL, DIMENSION(kts:kte+1), INTENT(IN) :: zw
    REAL, DIMENSION(kts:kte), INTENT(IN) :: p,exner,thl,qw,qv,qc,qi, &
         &tsq, qsq, cov, th

    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: vt,vq,sgm

    REAL, DIMENSION(kts:kte) :: alp,a,bet,b,ql,q1,RH
    REAL, DIMENSION(kts:kte), INTENT(OUT) :: qc_bl1D,qi_bl1D, &
                                             cldfra_bl1D
    DOUBLE PRECISION :: t3sq, r3sq, c3sq

    REAL :: qsl,esat,qsat,dqsl,cld0,q1k,qlk,eq1,qll,&
         &q2p,pt,rac,qt,t,xl,rsl,cpm,Fng,qww,alpha,beta,bb,&
         &ls,wt,cld_factor,fac_damp,liq_frac,ql_ice,ql_water,&
         &qmq,qsat_tk
    INTEGER :: i,j,k

    REAL :: erf

    !VARIABLES FOR ALTERNATIVE SIGMA
    REAL::dth,dtl,dqw,dzk,els
    REAL, DIMENSION(kts:kte), INTENT(IN) :: Sh,el

    !variables for SGS BL clouds
    REAL            :: zagl,damp,PBLH2
    REAL            :: cfmax

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
           dqsl = qsl*ep_2*xlv/( r_d*t**2 )

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
           qmq  = qw(k) -qsl
           !ORIGINAL STANDARD DEVIATION
           sgm(k) = SQRT( MAX( r3sq, 1.0d-10 ))
           !NORMALIZED DEPARTURE FROM SATURATION
           q1(k)   = qmq / sgm(k)
           !CLOUD FRACTION. rr2 = 1/SQRT(2) = 0.707
           cldfra_bl1D(k) = 0.5*( 1.0+erf( q1(k)*rr2 ) )

           q1k  = q1(k)
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

           !Now estimate the buoyancy flux functions
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
           dqsl = qsl*ep_2*xlv/( r_d*t**2 )

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
           qmq   = qw(k) -qsl
           q1(k) = qmq / sgm(k)
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

           !Now estimate the buoyancy flux functions
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
        !but with use of higher-order moments to estimate sigma
        PBLH2=MAX(10.,PBLH1)
        zagl = 0.
        DO k = kts,kte-1
           zagl = zagl + dz(k)
           t  = th(k)*exner(k)

           xl = xl_blend(t)                  ! obtain latent heat
           qsat_tk = qsat_blend(t,  p(k))    ! saturation water vapor mixing ratio at tk and p
           rh(k)=MAX(MIN(1.0,qw(k)/MAX(1.E-8,qsat_tk)),0.001)

           !dqw/dT: Clausius-Clapeyron
           dqsl = qsat_tk*ep_2*xlv/( r_d*t**2 )
           alp(k) = 1.0/( 1.0+dqsl*xlvcp )
           bet(k) = dqsl*exner(k)
 
           rsl = xl*qsat_tk / (r_v*t**2)     ! slope of C-C curve at t (=abs temperature)
                                             ! CB02, Eqn. 4
           cpm = cp + qw(k)*cpv              ! CB02, sec. 2, para. 1
           a(k) = 1./(1. + xl*rsl/cpm)       ! CB02 variable "a"
           b(k) = a(k)*rsl                   ! CB02 variable "b"

           !SPP
           qw_pert = qw(k) + qw(k)*0.5*rstoch_col(k)*real(spp_pbl)

           !This form of qmq (the numerator of Q1) no longer uses the a(k) factor
           qmq    = qw_pert - qsat_tk          ! saturation deficit/excess;

           !Use the form of Eq. (6) in Chaboureau and Bechtold (2002)
           !except neglect all but the first term for sig_r
           r3sq = MAX( qsq(k), 0.0 )
           !Calculate sigma using higher-order moments:
           sgm(k) = SQRT( r3sq )
           !Set limits on sigma relative to saturation water vapor
           sgm(k) = MIN( sgm(k), qsat_tk*0.666 ) !500 )
           sgm(k) = MAX( sgm(k), qsat_tk*0.035 ) !Note: 0.02 results in SWDOWN similar
                                                 !to the first-order version of sigma
           q1(k) = qmq  / sgm(k)  ! Q1, the normalized saturation
           q1k   = q1(k)          ! backup Q1 for later modification

           ! Specify cloud fraction
           !Original C-B cloud fraction, allows cloud fractions out to q1 = -3.5
           !cldfra_bl1D(K) = max(0., min(1., 0.5+0.36*atan(1.55*q1(k)))) ! Eq. 7 in CB02
           !Waynes LES fit  - over-diffuse, when limits removed from vt & vq & fng
           !cldfra_bl1D(K) = max(0., min(1., 0.5+0.36*atan(1.2*(q1(k)+0.4))))
           !Best compromise: Improves marine stratus without adding much cold bias.
           cldfra_bl1D(K) = max(0., min(1., 0.5+0.36*atan(1.8*(q1(k)+0.2))))

           ! Specify hydrometeors
           ! JAYMES- this option added 8 May 2015
           ! The cloud water formulations are taken from CB02, Eq. 8.
           IF (q1k < 0.) THEN        !unsaturated
              ql_water = sgm(k)*EXP(1.2*q1k-1)
              ql_ice   = sgm(k)*EXP(1.2*q1k-1.)
           ELSE IF (q1k > 2.) THEN   !supersaturated
              ql_water = sgm(k)*q1k
              ql_ice   = sgm(k)*q1k
           ELSE                      !slightly saturated (0 > q1 < 2)
              ql_water = sgm(k)*(EXP(-1.) + 0.66*q1k + 0.086*q1k**2)
              ql_ice   = sgm(k)*(EXP(-1.) + 0.66*q1k + 0.086*q1k**2)
           ENDIF

           !In saturated grid cells, use average of SGS and resolved values
           if ( qc(k) > 1.e-6 ) ql_water = 0.5 * ( ql_water + qc(k) ) 
           !since ql_ice is actually the total frozen condensate (snow+ice),
           !do not average with grid-scale ice alone 
           !if ( qi(k) > 1.e-9 ) ql_ice = 0.5 * ( ql_ice + qi(k) )

           if (cldfra_bl1D(k) < 0.01) then
              ql_ice   = 0.0
              ql_water = 0.0
              cldfra_bl1D(k) = 0.0
           endif

           !PHASE PARTITIONING: currently commented out since we are moving towards prognostic sgs clouds
           !Make some inferences about the relative amounts of
           !subgrid cloud water vs. ice based on collocated explicit clouds.  Otherise, 
           !use a simple temperature-dependent partitioning.
           ! IF ( qc(k) + qi(k) > 0.0 ) THEN ! explicit condensate exists, retain its phase partitioning
           !    IF ( qi(k) == 0.0 ) THEN       ! explicit contains no ice; assume subgrid liquid
           !      liq_frac = 1.0
           !    ELSE IF ( qc(k) == 0.0 ) THEN  ! explicit contains no liquid; assume subgrid ice
           !      liq_frac = 0.0
           !    ELSE IF ( (qc(k) >= 1.E-10) .AND. (qi(k) >= 1.E-10) ) THEN  ! explicit contains mixed phase of workably 
           !                                                                ! large amounts; assume subgrid follows 
           !                                                               ! same partioning
           !      liq_frac = qc(k) / ( qc(k) + qi(k) )
           !    ELSE
           !      liq_frac = MIN(1.0, MAX(0.0, (t-tice)/(t0c-tice))) ! explicit contains mixed phase, but at least one 
           !                                                         ! species is very small, so make a temperature-
           !                                                         ! depedent guess
           !    ENDIF
           ! ELSE                          ! no explicit condensate, so make a temperature-dependent guess
             liq_frac = MIN(1.0, MAX(0.0, (t-tice)/(tliq-tice)))
           ! ENDIF

           qc_bl1D(k) = liq_frac*ql_water       ! apply liq_frac to ql_water and ql_ice
           qi_bl1D(k) = (1.0-liq_frac)*ql_ice

           !Above tropopause:  eliminate subgrid clouds from CB scheme
           if (k .ge. k_tropo-1) then
              cldfra_bl1D(K) = 0.
              qc_bl1D(k)     = 0.
              qi_bl1D(k)     = 0.
           endif

           !Buoyancy-flux-related calculations follow...
           !limiting Q1 to avoid too much diffusion in cloud layers
           q1k=max(Q1(k),-2.0)

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
           ! Use the form of "Fng" from Bechtold and Siebesma (1998, JAS)
           IF (q1k .GE. 1.0) THEN
              Fng = 1.0
           ELSEIF (q1k .GE. -1.7 .AND. q1k .LT. 1.0) THEN
              Fng = EXP(-0.4*(q1k-1.0))
           ELSEIF (q1k .GE. -2.5 .AND. q1k .LT. -1.7) THEN
              Fng = 3.0 + EXP(-3.8*(q1k+1.7))
           ELSE
              Fng = MIN(23.9 + EXP(-1.6*(q1k+2.5)), 60.)
           ENDIF

           cfmax= min(cldfra_bl1D(k), 0.5)
           bb = b(k)*t/th(k) ! bb is "b" in BCMT95.  Their "b" differs from 
                             ! "b" in CB02 (i.e., b(k) above) by a factor 
                             ! of T/theta.  Strictly, b(k) above is formulated in
                             ! terms of sat. mixing ratio, but bb in BCMT95 is
                             ! cast in terms of sat. specific humidity.  The
                             ! conversion is neglected here. 
           qww   = 1.+0.61*qw(k)
           alpha = 0.61*th(k)
           beta  = (th(k)/t)*(xl/cp) - 1.61*th(k)
           vt(k) = qww   - cfmax*beta*bb*Fng   - 1.
           vq(k) = alpha + cfmax*beta*a(k)*Fng - tv0
           ! vt and vq correspond to beta-theta and beta-q, respectively,  
           ! in NN09, Eq. B8.  They also correspond to the bracketed
           ! expressions in BCMT95, Eq. 15, since (s*ql/sigma^2) = cldfra*Fng
           ! The "-1" and "-tv0" terms are included for consistency with 
           ! the legacy vt and vq formulations (above).

           ! dampen amplification factor where need be
           fac_damp = min(zagl * 0.0025, 1.0)
           !cld_factor = 1.0 + fac_damp*MAX(0.0, ( RH(k) - 0.75 ) / 0.26 )**1.9 !HRRRv4
           !cld_factor = 1.0 + fac_damp*min((max(0.0, ( RH(k) - 0.92 )) / 0.25 )**2, 0.3)
           cld_factor = 1.0 + fac_damp*min((max(0.0, ( RH(k) - 0.92 )) / 0.145)**2, 0.4)
           cldfra_bl1D(K) = MIN( 1., cld_factor*cldfra_bl1D(K) )
        enddo

      END SELECT !end cloudPDF option

      !For testing purposes only, option for isolating on the mass-flux clouds.
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
!>\ingroup gsd_mynn_edmf
!! This subroutine solves for tendencies of U, V, \f$\theta\f$, qv,
!! qc, and qi
  SUBROUTINE mynn_tendencies(kts,kte,i,    &
       &delt,dz,rho,                       &
       &u,v,th,tk,qv,qc,qi,qnc,qni,        &
       &psfc,p,exner,                      &
       &thl,sqv,sqc,sqi,sqw,               &
       &qnwfa,qnifa,qnbca,ozone,           &
       &ust,flt,flq,flqv,flqc,wspd,        &
       &uoce,voce,                         &
       &tsq,qsq,cov,                       &
       &tcd,qcd,                           &
       &dfm,dfh,dfq,                       &
       &Du,Dv,Dth,Dqv,Dqc,Dqi,Dqnc,Dqni,   &
       &Dqnwfa,Dqnifa,Dqnbca,Dozone,       &
       &diss_heat,                         &
       &s_aw,s_awthl,s_awqt,s_awqv,s_awqc, &
       &s_awu,s_awv,                       &
       &s_awqnc,s_awqni,                   &
       &s_awqnwfa,s_awqnifa,s_awqnbca,     &
       &sd_aw,sd_awthl,sd_awqt,sd_awqv,    &
       &sd_awqc,sd_awu,sd_awv,             &
       &sub_thl,sub_sqv,                   &
       &sub_u,sub_v,                       &
       &det_thl,det_sqv,det_sqc,           &
       &det_u,det_v,                       &
       &FLAG_QC,FLAG_QI,FLAG_QNC,FLAG_QNI, &
       &FLAG_QNWFA,FLAG_QNIFA,FLAG_QNBCA,  &
       &cldfra_bl1d,                       &
       &bl_mynn_cloudmix,                  &
       &bl_mynn_mixqt,                     &
       &bl_mynn_edmf,                      &
       &bl_mynn_edmf_mom,                  &
       &bl_mynn_mixscalars                 )

!-------------------------------------------------------------------
    INTEGER, INTENT(in) :: kts,kte,i

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

    INTEGER, INTENT(in) :: bl_mynn_cloudmix,bl_mynn_mixqt,&
                           bl_mynn_edmf,bl_mynn_edmf_mom, &
                           bl_mynn_mixscalars
    LOGICAL, INTENT(IN) :: FLAG_QI,FLAG_QNI,FLAG_QC,FLAG_QNC,&
                           FLAG_QNWFA,FLAG_QNIFA,FLAG_QNBCA

! thl - liquid water potential temperature
! qw - total water
! dfm,dfh,dfq - diffusivities i.e., dfh(k) = elq*sh(k) / dzk
! flt - surface flux of thl
! flq - surface flux of qw

! mass-flux plumes
    REAL, DIMENSION(kts:kte+1), INTENT(in) :: s_aw,s_awthl,s_awqt,&
         &s_awqnc,s_awqni,s_awqv,s_awqc,s_awu,s_awv,              &
         &s_awqnwfa,s_awqnifa,s_awqnbca,                          &
         &sd_aw,sd_awthl,sd_awqt,sd_awqv,sd_awqc,sd_awu,sd_awv
! tendencies from mass-flux environmental subsidence and detrainment
    REAL, DIMENSION(kts:kte), INTENT(in) :: sub_thl,sub_sqv,  &
         &sub_u,sub_v,det_thl,det_sqv,det_sqc,det_u,det_v
    REAL, DIMENSION(kts:kte), INTENT(in) :: u,v,th,tk,qv,qc,qi,qni,qnc,&
         &rho,p,exner,dfq,dz,tsq,qsq,cov,tcd,qcd,cldfra_bl1d,diss_heat
    REAL, DIMENSION(kts:kte), INTENT(inout) :: thl,sqw,sqv,sqc,sqi,&
         &qnwfa,qnifa,qnbca,ozone,dfm,dfh
    REAL, DIMENSION(kts:kte), INTENT(inout) :: du,dv,dth,dqv,dqc,dqi,&
         &dqni,dqnc,dqnwfa,dqnifa,dqnbca,dozone
    REAL, INTENT(IN) :: delt,ust,flt,flq,flqv,flqc,wspd,uoce,voce,&
         &psfc
    !debugging
    REAL ::wsp,wsp2,tk2,th2
    LOGICAL :: problem
    integer :: kproblem

!    REAL, INTENT(IN) :: gradu_top,gradv_top,gradth_top,gradqv_top

!local vars

    REAL, DIMENSION(kts:kte) :: dtz,dfhc,dfmc,delp
    REAL, DIMENSION(kts:kte) :: sqv2,sqc2,sqi2,sqw2,qni2,qnc2, & !AFTER MIXING
                                qnwfa2,qnifa2,qnbca2,ozone2
    REAL, DIMENSION(kts:kte) :: zfac,plumeKh,rhoinv
    REAL, DIMENSION(kts:kte) :: a,b,c,d,x
    REAL, DIMENSION(kts:kte+1) :: rhoz, & !rho on model interface
          &         khdz, kmdz
    REAL :: rhs,gfluxm,gfluxp,dztop,maxdfh,mindfh,maxcf,maxKh,zw
    REAL :: t,esat,qsl,onoff,kh,km,dzk,rhosfc
    REAL :: ustdrag,ustdiff,qvflux
    REAL :: th_new,portion_qc,portion_qi,condensate,qsat
    INTEGER :: k,kk

    !Activate nonlocal mixing from the mass-flux scheme for
    !number concentrations and aerosols (0.0 = no; 1.0 = yes)
    REAL, PARAMETER :: nonloc = 1.0

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
    !khdz = rho*Kh/dz = rho*dfh
    rhosfc     = psfc/(R_d*(tk(kts)+p608*qv(kts)))
    dtz(kts)   =delt/dz(kts)
    rhoz(kts)  =rho(kts)
    rhoinv(kts)=1./rho(kts)
    khdz(kts)  =rhoz(kts)*dfh(kts)
    kmdz(kts)  =rhoz(kts)*dfm(kts)
    delp(kts)  = psfc - (p(kts+1)*dz(kts) + p(kts)*dz(kts+1))/(dz(kts)+dz(kts+1))
    DO k=kts+1,kte
       dtz(k)   =delt/dz(k)
       rhoz(k)  =(rho(k)*dz(k-1) + rho(k-1)*dz(k))/(dz(k-1)+dz(k))
       rhoz(k)  =  MAX(rhoz(k),1E-4)
       rhoinv(k)=1./MAX(rho(k),1E-4)
       dzk      = 0.5  *( dz(k)+dz(k-1) )
       khdz(k)  = rhoz(k)*dfh(k)
       kmdz(k)  = rhoz(k)*dfm(k)
    ENDDO
    DO k=kts+1,kte-1
       delp(k)  = (p(k)*dz(k-1) + p(k-1)*dz(k))/(dz(k)+dz(k-1)) - &
                  (p(k+1)*dz(k) + p(k)*dz(k+1))/(dz(k)+dz(k+1))
    ENDDO
    delp(kte)  =delp(kte-1)
    rhoz(kte+1)=rhoz(kte)
    khdz(kte+1)=rhoz(kte+1)*dfh(kte)
    kmdz(kte+1)=rhoz(kte+1)*dfm(kte)

    !stability criteria for mf
    DO k=kts+1,kte-1
       khdz(k) = MAX(khdz(k),  0.5*s_aw(k))
       khdz(k) = MAX(khdz(k), -0.5*(s_aw(k)-s_aw(k+1)))
       kmdz(k) = MAX(kmdz(k),  0.5*s_aw(k))
       kmdz(k) = MAX(kmdz(k), -0.5*(s_aw(k)-s_aw(k+1)))
    ENDDO

    ustdrag = MIN(ust*ust,0.99)/wspd  ! limit at ~ 20 m/s
    ustdiff = MIN(ust*ust,0.01)/wspd  ! limit at ~ 2 m/s
    dth(kts:kte) = 0.0  ! must initialize for moisture_check routine

!!============================================
!! u
!!============================================

    k=kts

!original approach (drag in b-vector):
!    a(1)=0.
!    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
!    c(1)=-dtz(k)*dfm(k+1) - 0.5*dtz(k)*s_aw(k+1)*onoff
!    d(1)=u(k) + dtz(k)*uoce*ust**2/wspd - dtz(k)*s_awu(k+1)*onoff + &
!         sub_u(k)*delt + det_u(k)*delt

!rho-weighted (drag in b-vector):
    a(k)=  -dtz(k)*kmdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(kmdz(k+1)+rhosfc*ust**2/wspd)*rhoinv(k) &
           & - 0.5*dtz(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
    c(k)=  -dtz(k)*kmdz(k+1)*rhoinv(k) &
           & - 0.5*dtz(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
    d(k)=u(k)  + dtz(k)*uoce*ust**2/wspd - dtz(k)*s_awu(k+1)*onoff - &
       & dtz(k)*rhoinv(k)*sd_awu(k+1)*onoff + sub_u(k)*delt + det_u(k)*delt

!rho-weighted with drag term moved out of b-array
!    a(k)=  -dtz(k)*kmdz(k)*rhoinv(k)
!    b(k)=1.+dtz(k)*(kmdz(k+1))*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
!    c(k)=  -dtz(k)*kmdz(k+1)*rhoinv(k)   - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
!    d(k)=u(k)*(1.-ust**2/wspd*dtz(k)*rhosfc/rho(k)) + dtz(k)*uoce*ust**2/wspd - &
!    !!!d(k)=u(k)*(1.-ust**2/wspd*dtz(k)) + dtz(k)*uoce*ust**2/wspd - &
!      &  dtz(k)*rhoinv(k)*s_awu(k+1)*onoff - dtz(k)*rhoinv(k)*sd_awu(k+1)*onoff + sub_u(k)*delt + det_u(k)*delt

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*kmdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k)*onoff + 0.5*dtz(k)*rhoinv(k)*sd_aw(k)*onoff 
       b(k)=1.+dtz(k)*(kmdz(k)+kmdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))*onoff + 0.5*dtz(k)*rhoinv(k)*(sd_aw(k)-sd_aw(k+1))*onoff
       c(k)=  -dtz(k)*kmdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
       d(k)=u(k) + dtz(k)*rhoinv(k)*(s_awu(k)-s_awu(k+1))*onoff + dtz(k)*rhoinv(k)*(sd_awu(k)-sd_awu(k+1))*onoff + &
           &    sub_u(k)*delt + det_u(k)*delt
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
!    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
!       du(k)=(d(k-kts+1)-u(k))/delt
       du(k)=(x(k)-u(k))/delt
    ENDDO

!!============================================
!! v
!!============================================

    k=kts

!original approach (drag in b-vector):
!    a(1)=0.
!    b(1)=1. + dtz(k)*(dfm(k+1)+ust**2/wspd) - 0.5*dtz(k)*s_aw(k+1)*onoff
!    c(1)=   - dtz(k)*dfm(k+1)               - 0.5*dtz(k)*s_aw(k+1)*onoff
!    d(1)=v(k) + dtz(k)*voce*ust**2/wspd - dtz(k)*s_awv(k+1)*onoff + &
!          sub_v(k)*delt + det_v(k)*delt

!rho-weighted (drag in b-vector):
    a(k)=  -dtz(k)*kmdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(kmdz(k+1) + rhosfc*ust**2/wspd)*rhoinv(k) &
        &  - 0.5*dtz(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
    c(k)=  -dtz(k)*kmdz(k+1)*rhoinv(k) - 0.5*dtz(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
    d(k)=v(k)  + dtz(k)*voce*ust**2/wspd - dtz(k)*s_awv(k+1)*onoff - dtz(k)*rhoinv(k)*sd_awv(k+1)*onoff + &
       & sub_v(k)*delt + det_v(k)*delt

!rho-weighted with drag	term moved out of b-array
!    a(k)=  -dtz(k)*kmdz(k)*rhoinv(k)
!    b(k)=1.+dtz(k)*(kmdz(k+1))*rhoinv(k)  - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
!    c(k)=  -dtz(k)*kmdz(k+1)*rhoinv(k)    - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff
!    d(k)=v(k)*(1.-ust**2/wspd*dtz(k)*rhosfc/rho(k)) + dtz(k)*voce*ust**2/wspd - &
!    !!!d(k)=v(k)*(1.-ust**2/wspd*dtz(k)) + dtz(k)*voce*ust**2/wspd - &
!      &  dtz(k)*rhoinv(k)*s_awv(k+1)*onoff - dtz(k)*rhoinv(k)*sd_awv(k+1)*onoff + sub_v(k)*delt + det_v(k)*delt

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*kmdz(k)*rhoinv(k)   + 0.5*dtz(k)*rhoinv(k)*s_aw(k)*onoff + 0.5*dtz(k)*rhoinv(k)*sd_aw(k)*onoff
       b(k)=1.+dtz(k)*(kmdz(k)+kmdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))*onoff + 0.5*dtz(k)*rhoinv(k)*(sd_aw(k)-sd_aw(k+1))*onoff
       c(k)=  -dtz(k)*kmdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*onoff - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)*onoff 
       d(k)=v(k) + dtz(k)*rhoinv(k)*(s_awv(k)-s_awv(k+1))*onoff + dtz(k)*rhoinv(k)*(sd_awv(k)-sd_awv(k+1))*onoff + &
           &    sub_v(k)*delt + det_v(k)*delt
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
!    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
!       dv(k)=(d(k-kts+1)-v(k))/delt
       dv(k)=(x(k)-v(k))/delt
    ENDDO

!!============================================
!! thl tendency
!!============================================
    k=kts

!    a(k)=0.
!    b(k)=1.+dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    c(k)=  -dtz(k)*dfh(k+1) - 0.5*dtz(k)*s_aw(k+1)
!    d(k)=thl(k) + dtz(k)*flt + tcd(k)*delt &
!        & -dtz(k)*s_awthl(kts+1) + diss_heat(k)*delt + &
!        & sub_thl(k)*delt + det_thl(k)*delt
!
!    DO k=kts+1,kte-1
!       a(k)=  -dtz(k)*dfh(k)            + 0.5*dtz(k)*s_aw(k)
!       b(k)=1.+dtz(k)*(dfh(k)+dfh(k+1)) + 0.5*dtz(k)*(s_aw(k)-s_aw(k+1))
!       c(k)=  -dtz(k)*dfh(k+1)          - 0.5*dtz(k)*s_aw(k+1)
!       d(k)=thl(k) + tcd(k)*delt + dtz(k)*(s_awthl(k)-s_awthl(k+1)) &
!           &       + diss_heat(k)*delt + &
!           &         sub_thl(k)*delt + det_thl(k)*delt
!    ENDDO

!rho-weighted: rhosfc*X*rhoinv(k)
    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)           - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
    d(k)=thl(k)  + dtz(k)*rhosfc*flt*rhoinv(k) + tcd(k)*delt &
       & - dtz(k)*rhoinv(k)*s_awthl(k+1) -dtz(k)*rhoinv(k)*sd_awthl(k+1) + &
       & diss_heat(k)*delt + sub_thl(k)*delt + det_thl(k)*delt

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k) + 0.5*dtz(k)*rhoinv(k)*sd_aw(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k) + &
           &   0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1)) + 0.5*dtz(k)*rhoinv(k)*(sd_aw(k)-sd_aw(k+1))
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
       d(k)=thl(k) + tcd(k)*delt + &
          & dtz(k)*rhoinv(k)*(s_awthl(k)-s_awthl(k+1)) + dtz(k)*rhoinv(k)*(sd_awthl(k)-sd_awthl(k+1)) + &
          &       diss_heat(k)*delt + &
          &       sub_thl(k)*delt + det_thl(k)*delt
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
    CALL tridiag2(kte,a,b,c,d,x)
!    CALL tridiag3(kte,a,b,c,d,x)

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
    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)           - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
    d(k)=sqw(k)  + dtz(k)*rhosfc*flq*rhoinv(k) + qcd(k)*delt - dtz(k)*rhoinv(k)*s_awqt(k+1) - dtz(k)*rhoinv(k)*sd_awqt(k+1)

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k) + 0.5*dtz(k)*rhoinv(k)*sd_aw(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1)) + 0.5*dtz(k)*rhoinv(k)*(sd_aw(k)-sd_aw(k+1))
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
       d(k)=sqw(k) + qcd(k)*delt + dtz(k)*rhoinv(k)*(s_awqt(k)-s_awqt(k+1)) + dtz(k)*rhoinv(k)*(sd_awqt(k)-sd_awqt(k+1))
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
    CALL tridiag2(kte,a,b,c,d,sqw2)
!    CALL tridiag3(kte,a,b,c,d,sqw2)

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
    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)           - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
    d(k)=sqc(k)  + dtz(k)*rhosfc*flqc*rhoinv(k) + qcd(k)*delt &
       &  - dtz(k)*rhoinv(k)*s_awqc(k+1) - dtz(k)*rhoinv(k)*sd_awqc(k+1) + &
       &  det_sqc(k)*delt

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k) + 0.5*dtz(k)*rhoinv(k)*sd_aw(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1)) + 0.5*dtz(k)*rhoinv(k)*(sd_aw(k)-sd_aw(k+1))
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
       d(k)=sqc(k) + qcd(k)*delt + dtz(k)*rhoinv(k)*(s_awqc(k)-s_awqc(k+1)) + dtz(k)*rhoinv(k)*(sd_awqc(k)-sd_awqc(k+1)) + &
          & det_sqc(k)*delt
    ENDDO

! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=sqc(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,sqc2)
!    CALL tridiag3(kte,a,b,c,d,sqc2)

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

    !limit unreasonably large negative fluxes:
    qvflux = flqv
    if (qvflux < 0.0) then
       !do not allow specified surface flux to reduce qv below 1e-8 kg/kg
       qvflux = max(qvflux, (min(0.9*sqv(kts) - 1e-8, 0.0)/dtz(kts)))
    endif

!rho-weighted:  rhosfc*X*rhoinv(k)
    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)           - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
    d(k)=sqv(k)  + dtz(k)*rhosfc*qvflux*rhoinv(k) + qcd(k)*delt &
       &  - dtz(k)*rhoinv(k)*s_awqv(k+1) - dtz(k)*rhoinv(k)*sd_awqv(k+1) + &
       & sub_sqv(k)*delt + det_sqv(k)*delt

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k) + 0.5*dtz(k)*rhoinv(k)*sd_aw(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1)) + 0.5*dtz(k)*rhoinv(k)*(sd_aw(k)-sd_aw(k+1))
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1) - 0.5*dtz(k)*rhoinv(k)*sd_aw(k+1)
       d(k)=sqv(k) + qcd(k)*delt + dtz(k)*rhoinv(k)*(s_awqv(k)-s_awqv(k+1)) + dtz(k)*rhoinv(k)*(sd_awqv(k)-sd_awqv(k+1)) + &
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
    CALL tridiag2(kte,a,b,c,d,sqv2)
!    CALL tridiag3(kte,a,b,c,d,sqv2)

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
    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k)
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)
    d(k)=sqi(k)

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k)
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)
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
    CALL tridiag2(kte,a,b,c,d,sqi2)
!    CALL tridiag3(kte,a,b,c,d,sqi2)

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

    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    d(k)=qni(k)  - dtz(k)*rhoinv(k)*s_awqni(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
       d(k)=qni(k) + dtz(k)*rhoinv(k)*(s_awqni(k)-s_awqni(k+1))*nonloc
    ENDDO

!! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qni(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)
!    CALL tridiag3(kte,a,b,c,d,x)

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

    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)           - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    d(k)=qnc(k)  - dtz(k)*rhoinv(k)*s_awqnc(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
       d(k)=qnc(k) + dtz(k)*rhoinv(k)*(s_awqnc(k)-s_awqnc(k+1))*nonloc
    ENDDO

!! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnc(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)
!    CALL tridiag3(kte,a,b,c,d,x)

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

    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))*rhoinv(k) - &
           &    0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    d(k)=qnwfa(k)  - dtz(k)*rhoinv(k)*s_awqnwfa(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
       d(k)=qnwfa(k) + dtz(k)*rhoinv(k)*(s_awqnwfa(k)-s_awqnwfa(k+1))*nonloc
    ENDDO

! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnwfa(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)
!    CALL tridiag3(kte,a,b,c,d,x)

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

    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))*rhoinv(k) - &
           &    0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    d(k)=qnifa(k)  - dtz(k)*rhoinv(k)*s_awqnifa(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
       d(k)=qnifa(k) + dtz(k)*rhoinv(k)*(s_awqnifa(k)-s_awqnifa(k+1))*nonloc
    ENDDO

! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnifa(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)
!    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
       !qnifa2(k)=d(k-kts+1)
       qnifa2(k)=x(k)
    ENDDO

ELSE
    !If not mixing aerosols, set "updated" array equal to original array
    qnifa2=qnifa
ENDIF

!============================================
! Black-carbon aerosols ( qnbca ).           
!============================================
IF (bl_mynn_cloudmix > 0 .AND. FLAG_QNBCA .AND. &
      bl_mynn_mixscalars > 0) THEN

   k=kts

    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))*rhoinv(k) - &
           &    0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
    d(k)=qnbca(k)  - dtz(k)*rhoinv(k)*s_awqnbca(k+1)*nonloc

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k)*nonloc
       b(k)=1.+dtz(k)*(khdz(k) + khdz(k+1))*rhoinv(k) + &
           &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))*nonloc
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)*nonloc
       d(k)=qnbca(k) + dtz(k)*rhoinv(k)*(s_awqnbca(k)-s_awqnbca(k+1))*nonloc
    ENDDO

! prescribed value
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=qnbca(kte)

!    CALL tridiag(kte,a,b,c,d)
!    CALL tridiag2(kte,a,b,c,d,x)
    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
       !qnbca2(k)=d(k-kts+1)
       qnbca2(k)=x(k)
    ENDDO

ELSE
    !If not mixing aerosols, set "updated" array equal to original array
    qnbca2=qnbca
ENDIF

!============================================
! Ozone - local mixing only
!============================================

    k=kts

!rho-weighted:
    a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
    b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k)
    c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)
    d(k)=ozone(k)

    DO k=kts+1,kte-1
       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
       b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k)
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)
       d(k)=ozone(k)
    ENDDO

! prescribed value                                                                                                           
    a(kte)=0.
    b(kte)=1.
    c(kte)=0.
    d(kte)=ozone(kte)

!    CALL tridiag(kte,a,b,c,d)
    CALL tridiag2(kte,a,b,c,d,x)
!    CALL tridiag3(kte,a,b,c,d,x)

    DO k=kts,kte
       !ozone2(k)=d(k-kts+1)
       dozone(k)=(x(k)-ozone(k))/delt
    ENDDO

!!============================================
!! Compute tendencies and convert to mixing ratios for WRF.
!! Note that the momentum tendencies are calculated above.
!!============================================

   IF (bl_mynn_mixqt > 0) THEN 
      DO k=kts,kte
         !compute updated theta using updated thl and old condensate
         th_new = thl(k) + xlvcp/exner(k)*sqc(k) &
           &             + xlscp/exner(k)*sqi(k)

         t  = th_new*exner(k)
         qsat = qsat_blend(t,p(k)) 
         !SATURATED VAPOR PRESSURE
         !esat=esat_blend(t)
         !SATURATED SPECIFIC HUMIDITY
         !qsl=ep_2*esat/(p(k)-ep_3*esat)
         !qsl=ep_2*esat/max(1.e-4,(p(k)-ep_3*esat))

         IF (sqc(k) > 0.0 .or. sqi(k) > 0.0) THEN !initially saturated
            sqv2(k) = MIN(sqw2(k),qsat)
            portion_qc = sqc(k)/(sqc(k) + sqi(k))
            portion_qi = sqi(k)/(sqc(k) + sqi(k))
            condensate = MAX(sqw2(k) - qsat, 0.0)
            sqc2(k) = condensate*portion_qc
            sqi2(k) = condensate*portion_qi
         ELSE                     ! initially unsaturated -----
            sqv2(k) = sqw2(k)     ! let microphys decide what to do
            sqi2(k) = 0.0         ! if sqw2 > qsat 
            sqc2(k) = 0.0
         ENDIF
         !dqv(k) = (sqv2(k) - sqv(k))/delt
         !dqc(k) = (sqc2(k) - sqc(k))/delt
         !dqi(k) = (sqi2(k) - sqi(k))/delt
      ENDDO
   ENDIF


    !=====================
    ! WATER VAPOR TENDENCY
    !=====================
    DO k=kts,kte
       Dqv(k)=(sqv2(k)/(1.-sqv2(k)) - qv(k))/delt
       !if (sqv2(k) < 0.0)print*,"neg qv:",sqv2(k),k
    ENDDO

    IF (bl_mynn_cloudmix > 0) THEN
      !=====================
      ! CLOUD WATER TENDENCY
      !=====================
      !print*,"FLAG_QC:",FLAG_QC
      IF (FLAG_QC) THEN
         DO k=kts,kte
            Dqc(k)=(sqc2(k)/(1.-sqv2(k)) - qc(k))/delt
            !if (sqc2(k) < 0.0)print*,"neg qc:",sqc2(k),k
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
           !if (sqi2(k) < 0.0)print*,"neg qi:",sqi2(k),k
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

    !ensure non-negative moist species
    CALL moisture_check(kte, delt, delp, exner,  &
                        sqv2, sqc2, sqi2, thl,   &
                        dqv, dqc, dqi, dth )

    !=====================
    ! OZONE TENDENCY CHECK
    !=====================
    DO k=kts,kte
       IF(Dozone(k)*delt + ozone(k) < 0.) THEN
         Dozone(k)=-ozone(k)*0.99/delt
       ENDIF
    ENDDO

    !===================
    ! THETA TENDENCY
    !===================
    IF (FLAG_QI) THEN
      DO k=kts,kte
         Dth(k)=(thl(k) + xlvcp/exner(k)*sqc2(k) &
           &            + xlscp/exner(k)*sqi2(k) &
           &            - th(k))/delt
         !Use form from Tripoli and Cotton (1981) with their
         !suggested min temperature to improve accuracy:
         !Dth(k)=(thl(k)*(1.+ xlvcp/MAX(tk(k),TKmin)*sqc(k)  &
         !  &               + xlscp/MAX(tk(k),TKmin)*sqi(k)) &
         !  &               - th(k))/delt
      ENDDO
    ELSE
      DO k=kts,kte
         Dth(k)=(thl(k)+xlvcp/exner(k)*sqc2(k) - th(k))/delt
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
          !=====================
          ! Black-carbon aerosols
          !=====================
          Dqnbca(k)=(qnbca2(k) - qnbca(k))/delt
       ENDDO
    ELSE
       DO k=kts,kte
          Dqnwfa(k)=0.
          Dqnifa(k)=0.
          Dqnbca(k)=0.
       ENDDO
    ENDIF

    !ensure non-negative moist species
    !note: if called down here, dth needs to be updated, but
    !      if called before the theta-tendency calculation, do not compute dth
    !CALL moisture_check(kte, delt, delp, exner,     &
    !                    sqv, sqc, sqi, thl,         &
    !                    dqv, dqc, dqi, dth )

    if (debug_code) then
       problem = .false.
       do k=kts,kte
          wsp  = sqrt(u(k)**2 + v(k)**2)
          wsp2 = sqrt((u(k)+du(k)*delt)**2 + (v(k)+du(k)*delt)**2)
          th2  = th(k) + Dth(k)*delt
          tk2  = th2*exner(k)
          if (wsp2 > 200. .or. tk2 > 360. .or. tk2 < 160.) then
             problem = .true.
             print*,"Outgoing problem at: i=",i," k=",k
             print*," incoming wsp=",wsp," outgoing wsp=",wsp2
             print*," incoming T=",th(k)*exner(k),"outgoing T:",tk2
             print*," du=",du(k)*delt," dv=",dv(k)*delt," dth=",dth(k)*delt
             print*," km=",kmdz(k)*dz(k)," kh=",khdz(k)*dz(k)
             print*," u*=",ust," wspd=",wspd,"rhosfc=",rhosfc
             print*," LH=",flq*rhosfc*1004.," HFX=",flt*rhosfc*1004.
             print*," drag term=",ust**2/wspd*dtz(k)*rhosfc/rho(kts)
             kproblem = k
          endif
       enddo
       if (problem) then
          print*,"==thl:",thl(max(kproblem-3,1):min(kproblem+3,kte))
          print*,"===qv:",sqv2(max(kproblem-3,1):min(kproblem+3,kte))
          print*,"===qc:",sqc2(max(kproblem-3,1):min(kproblem+3,kte))
          print*,"===qi:",sqi2(max(kproblem-3,1):min(kproblem+3,kte))
          print*,"====u:",u(max(kproblem-3,1):min(kproblem+3,kte))
          print*,"====v:",v(max(kproblem-3,1):min(kproblem+3,kte))
       endif
    endif

#ifdef HARDCODE_VERTICAL
# undef kts
# undef kte
#endif

  END SUBROUTINE mynn_tendencies

! ==================================================================
  SUBROUTINE moisture_check(kte, delt, dp, exner, &
                            qv, qc, qi, th,       &
                            dqv, dqc, dqi, dth )

  ! This subroutine was adopted from the CAM-UW ShCu scheme and 
  ! adapted for use here.
  !
  ! If qc < qcmin, qi < qimin, or qv < qvmin happens in any layer,
  ! force them to be larger than minimum value by (1) condensating 
  ! water vapor into liquid or ice, and (2) by transporting water vapor 
  ! from the very lower layer.
  ! 
  ! We then update the final state variables and tendencies associated
  ! with this correction. If any condensation happens, update theta too.
  ! Note that (qv,qc,qi,th) are the final state variables after
  ! applying corresponding input tendencies and corrective tendencies.

    implicit none
    integer,  intent(in)     :: kte
    real, intent(in)         :: delt
    real, dimension(kte), intent(in)     :: dp, exner
    real, dimension(kte), intent(inout)  :: qv, qc, qi, th
    real, dimension(kte), intent(inout)  :: dqv, dqc, dqi, dth
    integer   k
    real ::  dqc2, dqi2, dqv2, sum, aa, dum
    real, parameter :: qvmin = 1e-20,   &
                       qcmin = 0.0,     &
                       qimin = 0.0

    do k = kte, 1, -1  ! From the top to the surface
       dqc2 = max(0.0, qcmin-qc(k)) !qc deficit (>=0)
       dqi2 = max(0.0, qimin-qi(k)) !qi deficit (>=0)

       !fix tendencies
       dqc(k) = dqc(k) +  dqc2/delt
       dqi(k) = dqi(k) +  dqi2/delt
       dqv(k) = dqv(k) - (dqc2+dqi2)/delt
       dth(k) = dth(k) + xlvcp/exner(k)*(dqc2/delt) + &
                         xlscp/exner(k)*(dqi2/delt)
       !update species
       qc(k)  = qc(k)  +  dqc2
       qi(k)  = qi(k)  +  dqi2
       qv(k)  = qv(k)  -  dqc2 - dqi2
       th(k)  = th(k)  +  xlvcp/exner(k)*dqc2 + &
                          xlscp/exner(k)*dqi2

       !then fix qv
       dqv2   = max(0.0, qvmin-qv(k)) !qv deficit (>=0)
       dqv(k) = dqv(k) + dqv2/delt
       qv(k)  = qv(k)  + dqv2
       if( k .ne. 1 ) then
           qv(k-1)   = qv(k-1)  - dqv2*dp(k)/dp(k-1)
           dqv(k-1)  = dqv(k-1) - dqv2*dp(k)/dp(k-1)/delt
       endif
       qv(k) = max(qv(k),qvmin)
       qc(k) = max(qc(k),qcmin)
       qi(k) = max(qi(k),qimin)
    end do
    ! Extra moisture used to satisfy 'qv(1)>=qvmin' is proportionally
    ! extracted from all the layers that has 'qv > 2*qvmin'. This fully
    ! preserves column moisture.
    if( dqv2 .gt. 1.e-20 ) then
        sum = 0.0
        do k = 1, kte
           if( qv(k) .gt. 2.0*qvmin ) sum = sum + qv(k)*dp(k)
        enddo
        aa = dqv2*dp(1)/max(1.e-20,sum)
        if( aa .lt. 0.5 ) then
            do k = 1, kte
               if( qv(k) .gt. 2.0*qvmin ) then
                   dum    = aa*qv(k)
                   qv(k)  = qv(k) - dum
                   dqv(k) = dqv(k) - dum/delt
               endif
            enddo
        else
        ! For testing purposes only (not yet found in any output):
        !    write(*,*) 'Full moisture conservation is impossible'
        endif
    endif

    return

  END SUBROUTINE moisture_check

! ==================================================================

  SUBROUTINE mynn_mix_chem(kts,kte,i,     &
       delt,dz,pblh,                      &
       nchem, kdvel, ndvel,               &
       chem1, vd1,                        &
       rho,                               &
       flt, tcd, qcd,                     &
       dfh,                               &
       s_aw, s_awchem,                    &
       emis_ant_no, frp, rrfs_sd,         &
       enh_mix, smoke_dbg                 )

!-------------------------------------------------------------------
    INTEGER, INTENT(in) :: kts,kte,i
    REAL, DIMENSION(kts:kte), INTENT(IN)    :: dfh,dz,tcd,qcd
    REAL, DIMENSION(kts:kte), INTENT(INOUT) :: rho
    REAL, INTENT(IN)    :: delt,flt,pblh
    INTEGER, INTENT(IN) :: nchem, kdvel, ndvel
    REAL, DIMENSION( kts:kte+1), INTENT(IN) :: s_aw
    REAL, DIMENSION( kts:kte, nchem ), INTENT(INOUT) :: chem1
    REAL, DIMENSION( kts:kte+1,nchem), INTENT(IN) :: s_awchem
    REAL, DIMENSION( ndvel ), INTENT(IN) :: vd1
    REAL, INTENT(IN) :: emis_ant_no,frp
    LOGICAL, INTENT(IN) :: rrfs_sd,enh_mix,smoke_dbg
!local vars

    REAL, DIMENSION(kts:kte)     :: dtz
    REAL, DIMENSION(kts:kte) :: a,b,c,d,x
    REAL :: rhs,dztop
    REAL :: t,dzk
    REAL :: hght 
    REAL :: khdz_old, khdz_back
    INTEGER :: k,kk,kmaxfire                         ! JLS 12/21/21
    INTEGER :: ic  ! Chemical array loop index
    
    INTEGER, SAVE :: icall

    REAL, DIMENSION(kts:kte) :: rhoinv
    REAL, DIMENSION(kts:kte+1) :: rhoz,khdz
    REAL, PARAMETER :: NO_threshold    = 10.0     ! For anthropogenic sources
    REAL, PARAMETER :: frp_threshold   = 10.0     ! RAR 02/11/22: I increased the frp threshold to enhance mixing over big fires
    REAL, PARAMETER :: pblh_threshold  = 100.0

    dztop=.5*(dz(kte)+dz(kte-1))

    DO k=kts,kte
       dtz(k)=delt/dz(k)
    ENDDO

    !Prepare "constants" for diffusion equation.
    !khdz = rho*Kh/dz = rho*dfh
    rhoz(kts)  =rho(kts)
    rhoinv(kts)=1./rho(kts)
    khdz(kts)  =rhoz(kts)*dfh(kts)

    DO k=kts+1,kte
       rhoz(k)  =(rho(k)*dz(k-1) + rho(k-1)*dz(k))/(dz(k-1)+dz(k))
       rhoz(k)  =  MAX(rhoz(k),1E-4)
       rhoinv(k)=1./MAX(rho(k),1E-4)
       dzk      = 0.5  *( dz(k)+dz(k-1) )
       khdz(k)  = rhoz(k)*dfh(k)
    ENDDO
    rhoz(kte+1)=rhoz(kte)
    khdz(kte+1)=rhoz(kte+1)*dfh(kte)

    !stability criteria for mf
    DO k=kts+1,kte-1
       khdz(k) = MAX(khdz(k),  0.5*s_aw(k))
       khdz(k) = MAX(khdz(k), -0.5*(s_aw(k)-s_aw(k+1)))
    ENDDO

    !Enhanced mixing over fires
    IF ( rrfs_sd .and. enh_mix ) THEN
       DO k=kts+1,kte-1
          khdz_old  = khdz(k)
          khdz_back = pblh * 0.15 / dz(k)
          !Modify based on anthropogenic emissions of NO and FRP
          IF ( pblh < pblh_threshold ) THEN
             IF ( emis_ant_no > NO_threshold ) THEN
                khdz(k) = MAX(1.1*khdz(k),sqrt((emis_ant_no / NO_threshold)) / dz(k) * rhoz(k)) ! JLS 12/21/21
!                khdz(k) = MAX(khdz(k),khdz_back)
             ENDIF
             IF ( frp > frp_threshold ) THEN
                kmaxfire = ceiling(log(frp))
                khdz(k) = MAX(1.1*khdz(k), (1. - k/(kmaxfire*2.)) * ((log(frp))**2.- 2.*log(frp)) / dz(k)*rhoz(k)) ! JLS 12/21/21
!                khdz(k) = MAX(khdz(k),khdz_back)
             ENDIF
          ENDIF
       ENDDO
    ENDIF

  !============================================
  ! Patterned after mixing of water vapor in mynn_tendencies.
  !============================================

    DO ic = 1,nchem
       k=kts

       a(k)=  -dtz(k)*khdz(k)*rhoinv(k)
       b(k)=1.+dtz(k)*(khdz(k+1)+khdz(k))*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)
       c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k)           - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)
       d(k)=chem1(k,ic) & !dtz(k)*flt  !neglecting surface sources 
            & - dtz(k)*vd1(ic)*chem1(k,ic) &
            & - dtz(k)*rhoinv(k)*s_awchem(k+1,ic)

       DO k=kts+1,kte-1
          a(k)=  -dtz(k)*khdz(k)*rhoinv(k)     + 0.5*dtz(k)*rhoinv(k)*s_aw(k)
          b(k)=1.+dtz(k)*(khdz(k)+khdz(k+1))*rhoinv(k) + &
             &    0.5*dtz(k)*rhoinv(k)*(s_aw(k)-s_aw(k+1))
          c(k)=  -dtz(k)*khdz(k+1)*rhoinv(k) - 0.5*dtz(k)*rhoinv(k)*s_aw(k+1)
          d(k)=chem1(k,ic) + dtz(k)*rhoinv(k)*(s_awchem(k,ic)-s_awchem(k+1,ic))
       ENDDO

      ! prescribed value at top
       a(kte)=0.
       b(kte)=1.
       c(kte)=0.
       d(kte)=chem1(kte,ic)

       CALL tridiag3(kte,a,b,c,d,x)

       DO k=kts,kte
          chem1(k,ic)=x(k)
       ENDDO
    ENDDO

  END SUBROUTINE mynn_mix_chem

! ==================================================================
!>\ingroup gsd_mynn_edmf
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
!>\ingroup gsd_mynn_edmf
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
!>\ingroup gsd_mynn_edmf
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
!>\ingroup gsd_mynn_edmf
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
!>\ingroup gsd_mynn_edmf
!! This subroutine calculates hybrid diagnotic boundary-layer height (PBLH).
!!
!! NOTES ON THE PBLH FORMULATION: The 1.5-theta-increase method defines
!!PBL heights as the level at.
!!which the potential temperature first exceeds the minimum potential.
!!temperature within the boundary layer by 1.5 K. When applied to.
!!observed temperatures, this method has been shown to produce PBL-
!!height estimates that are unbiased relative to profiler-based.
!!estimates (Nielsen-Gammon et al. 2008 \cite Nielsen_Gammon_2008). 
!! However, their study did not
!!include LLJs. Banta and Pichugina (2008) \cite Pichugina_2008  show that a TKE-based.
!!threshold is a good estimate of the PBL height in LLJs. Therefore,
!!a hybrid definition is implemented that uses both methods, weighting
!!the TKE-method more during stable conditions (PBLH < 400 m).
!!A variable tke threshold (TKEeps) is used since no hard-wired
!!value could be found to work best in all conditions.
!>\section gen_get_pblh  GSD get_pblh General Algorithm
!> @{
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

    !> - FIND MIN THETAV IN THE LOWEST 200 M AGL
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

    !> - FIND THETAV-BASED PBLH (BEST FOR DAYTIME).
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

    !> - FOR STABLE BOUNDARY LAYERS, USE TKE METHOD TO COMPLEMENT THE
    !! THETAV-BASED DEFINITION (WHEN THE THETA-V BASED PBLH IS BELOW ~0.5 KM).
    !!THE TANH WEIGHTING FUNCTION WILL MAKE THE TKE-BASED DEFINITION NEGLIGIBLE 
    !!WHEN THE THETA-V-BASED DEFINITION IS ABOVE ~1 KM.
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
           !print *,"PBLH_TKE:",i,PBLH_TKE, Qke1D(k)/2., zw1D(kts+1)
       ENDIF
       !k = k+1
       IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1) !EXIT SAFEGUARD
       IF (PBLH_TKE .NE. 0.) exit
    ENDDO

    !> - With TKE advection turned on, the TKE-based PBLH can be very large 
    !! in grid points with convective precipitation (> 8 km!),
    !! so an artificial limit is imposed to not let PBLH_TKE exceed the
    !!theta_v-based PBL height +/- 350 m.
    !!This has no impact on 98-99% of the domain, but is the simplest patch
    !!that adequately addresses these extremely large PBLHs.
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
!> @}
  
! ==================================================================
!>\ingroup gsd_mynn_edmf
!! This subroutine is the Dynamic Multi-Plume (DMP) Mass-Flux Scheme.
!! 
!! dmp_mf() calculates the nonlocal turbulent transport from the dynamic
!! multiplume mass-flux scheme as well as the shallow-cumulus component of 
!! the subgrid clouds. Note that this mass-flux scheme is called when the
!! namelist paramter \p bl_mynn_edmf is set to 1 (recommended).
!!
!! Much thanks to Kay Suslj of NASA-JPL for contributing the original version
!! of this mass-flux scheme. Considerable changes have been made from it's
!! original form. Some additions include:
!!  -# scale-aware tapering as dx -> 0
!!  -# transport of TKE (extra namelist option)
!!  -# Chaboureau-Bechtold cloud fraction & coupling to radiation (when icloud_bl > 0)
!!  -# some extra limits for numerical stability
!!
!! This scheme remains under development, so consider it experimental code. 
!!
  SUBROUTINE DMP_mf(                            &
                 & kts,kte,dt,zw,dz,p,rho,      &
                 & momentum_opt,                &
                 & tke_opt,                     &
                 & scalar_opt,                  &
                 & u,v,w,th,thl,thv,tk,         &
                 & qt,qv,qc,qke,                &
                 & qnc,qni,qnwfa,qnifa,qnbca,   &
                 & exner,vt,vq,sgm,             &
                 & ust,flt,fltv,flq,flqv,       &
                 & pblh,kpbl,dx,landsea,ts,     &
            ! outputs - updraft properties   
                 & edmf_a,edmf_w,               &
                 & edmf_qt,edmf_thl,            &
                 & edmf_ent,edmf_qc,            &
            ! outputs - variables needed for solver 
                 & s_aw,s_awthl,s_awqt,         &
                 & s_awqv,s_awqc,               &
                 & s_awu,s_awv,s_awqke,         &
                 & s_awqnc,s_awqni,             &
                 & s_awqnwfa,s_awqnifa,         &
                 & s_awqnbca,                   &
                 & sub_thl,sub_sqv,             &
                 & sub_u,sub_v,                 &
                 & det_thl,det_sqv,det_sqc,     &
                 & det_u,det_v,                 &
            ! chem/smoke
                 & nchem,chem1,s_awchem,        &
                 & mix_chem,                    &
            ! in/outputs - subgrid scale clouds
                 & qc_bl1d,cldfra_bl1d,         &
                 & qc_bl1D_old,cldfra_bl1D_old, &
            ! inputs - flags for moist arrays
                 & F_QC,F_QI,                   &
                 & F_QNC,F_QNI,                 &
                 & F_QNWFA,F_QNIFA,F_QNBCA,     &
                 & Psig_shcu,                   &
            ! output info
                 &nup2,ktop,maxmf,ztop,         &
            ! unputs for stochastic perturbations
                 &spp_pbl,rstoch_col            ) 

  ! inputs:
     INTEGER, INTENT(IN) :: KTS,KTE,KPBL,momentum_opt,tke_opt,scalar_opt

#ifdef HARDCODE_VERTICAL
# define kts 1
# define kte HARDCODE_VERTICAL
#endif

! Stochastic 
     INTEGER,  INTENT(IN)          :: spp_pbl
     REAL, DIMENSION(KTS:KTE)      :: rstoch_col

     REAL,DIMENSION(KTS:KTE), INTENT(IN) ::                            &
                   u,v,w,th,thl,tk,qt,qv,qc,                           &
                   exner,dz,THV,P,rho,qke,qnc,qni,qnwfa,qnifa,qnbca
     REAL,DIMENSION(KTS:KTE+1), INTENT(IN) :: zw    !height at full-sigma
     REAL, INTENT(IN) :: dt,ust,flt,fltv,flq,flqv,pblh,                &
                         dx,psig_shcu,landsea,ts
     LOGICAL, OPTIONAL :: f_qc,f_qi,f_qnc,f_qni,                       &
                   f_qnwfa,f_qnifa,f_qnbca

  ! outputs - updraft properties
     REAL,DIMENSION(KTS:KTE), INTENT(OUT) :: edmf_a,edmf_w,            &
                      & edmf_qt,edmf_thl,edmf_ent,edmf_qc
     !add one local edmf variable:
     REAL,DIMENSION(KTS:KTE) :: edmf_th
  ! output
     INTEGER, INTENT(OUT) :: nup2,ktop
     REAL, INTENT(OUT) :: maxmf,ztop
  ! outputs - variables needed for solver - sum ai*rho*wis_awphi
     REAL,DIMENSION(KTS:KTE+1) :: s_aw,s_awthl,s_awqt,                 &
                         s_awqv,s_awqc,s_awqnc,s_awqni,                &
                         s_awqnwfa,s_awqnifa,s_awqnbca,                &
                         s_awu,s_awv,s_awqke,s_aw2

     REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: qc_bl1d,cldfra_bl1d,    &
                                       qc_bl1d_old,cldfra_bl1d_old

    INTEGER, PARAMETER :: nup=10, debug_mf=0

  !------------- local variables -------------------
  ! updraft properties defined on interfaces (k=1 is the top of the
  ! first model layer
     REAL,DIMENSION(KTS:KTE+1,1:NUP) :: UPW,UPTHL,UPQT,UPQC,UPQV,      &
                                        UPA,UPU,UPV,UPTHV,UPQKE,UPQNC, &
                                        UPQNI,UPQNWFA,UPQNIFA,UPQNBCA
  ! entrainment variables
     REAL,DIMENSION(KTS:KTE,1:NUP) :: ENT,ENTf
     INTEGER,DIMENSION(KTS:KTE,1:NUP) :: ENTi
  ! internal variables
     INTEGER :: K,I,k50
     REAL :: fltv2,wstar,qstar,thstar,sigmaW,sigmaQT,sigmaTH,z0,       &
             pwmin,pwmax,wmin,wmax,wlv,Psig_w,maxw,maxqc,wpbl
     REAL :: B,QTn,THLn,THVn,QCn,Un,Vn,QKEn,QNCn,QNIn,                 &
             QNWFAn,QNIFAn,QNBCAn,                                     &
             Wn2,Wn,EntEXP,EntEXM,EntW,BCOEFF,THVkm1,THVk,Pk,rho_int

  ! w parameters
     REAL,PARAMETER :: &
          &Wa=2./3.,   &
          &Wb=0.002,   &
          &Wc=1.5 
        
  ! Lateral entrainment parameters ( L0=100 and ENT0=0.1) were taken from
  ! Suselj et al (2013, jas). Note that Suselj et al (2014,waf) use L0=200 and ENT0=0.2.
     REAL,PARAMETER :: &
         & L0=100.,    &
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

  ! chem/smoke
     INTEGER, INTENT(IN) :: nchem
     REAL,DIMENSION(:, :) :: chem1
     REAL,DIMENSION(kts:kte+1, nchem) :: s_awchem
     REAL,DIMENSION(nchem) :: chemn
     REAL,DIMENSION(KTS:KTE+1,1:NUP, nchem) :: UPCHEM
     INTEGER :: ic
     REAL,DIMENSION(KTS:KTE+1, nchem) :: edmf_chem
     LOGICAL, INTENT(IN) :: mix_chem

  !JOE: add declaration of ERF
   REAL :: ERF

   LOGICAL :: superadiabatic

  ! VARIABLES FOR CHABOUREAU-BECHTOLD CLOUD FRACTION
   REAL,DIMENSION(KTS:KTE), INTENT(INOUT) :: vt, vq, sgm
   REAL :: sigq,xl,rsl,cpm,a,qmq,mf_cf,Aup,Q1,diffqt,qsat_tk,&
           Fng,qww,alpha,beta,bb,f,pt,t,q2p,b9,satvp,rhgrid, &
           Ac_mf,Ac_strat,qc_mf
   REAL, PARAMETER :: cf_thresh = 0.5 ! only overwrite stratus CF less than this value

  ! Variables for plume interpolation/saturation check
   REAL,DIMENSION(KTS:KTE) :: exneri,dzi
   REAL :: THp, QTp, QCp, QCs, esat, qsl
   REAL :: csigma,acfac,ac_wsp,ac_cld

   !plume overshoot
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
           detrateUV,oow,exc_fac,aratio,detturb,qc_grid,qc_sgs,&
           qc_plume,exc_heat,exc_moist,tk_int
   REAL, PARAMETER :: Cdet   = 1./45.
   REAL, PARAMETER :: dzpmax = 300. !limit dz used in detrainment - can be excessing in thick layers
   !parameter "Csub" determines the propotion of upward vertical velocity that contributes to
   !environmenatal subsidence. Some portion is expected to be compensated by downdrafts instead of
   !gentle environmental subsidence. 1.0 assumes all upward vertical velocity in the mass-flux scheme
   !is compensated by "gentle" environmental subsidence. 
   REAL, PARAMETER :: Csub=0.25

   !Factor for the pressure gradient effects on momentum transport
   REAL, PARAMETER :: pgfac = 0.00  ! Zhang and Wu showed 0.4 is more appropriate for lower troposphere
   REAL :: Uk,Ukm1,Vk,Vkm1,dxsa

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
  UPQNBCA=0.
  IF ( mix_chem ) THEN
     UPCHEM(KTS:KTE+1,1:NUP,1:nchem)=0.0
  ENDIF

  ENT=0.001
! Initialize mean updraft properties
  edmf_a  =0.
  edmf_w  =0.
  edmf_qt =0.
  edmf_thl=0.
  edmf_ent=0.
  edmf_qc =0.
  IF ( mix_chem ) THEN
     edmf_chem(kts:kte+1,1:nchem) = 0.0
  ENDIF

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
  s_awqnbca=0.
  IF ( mix_chem ) THEN
     s_awchem(kts:kte+1,1:nchem) = 0.0
  ENDIF

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
     IF(zw(k) > pblh + 500.) exit

     wpbl = w(k)
     IF(w(k) < 0.)wpbl = 2.*w(k)
     maxw = MAX(maxw,ABS(wpbl))

     !Find highest k-level below 50m AGL
     IF(ZW(k)<=50.)k50=k

     !Search for cloud base
     qc_sgs = MAX(qc(k), qc_bl1d(k)*cldfra_bl1d(k))
     IF(qc_sgs> 1E-5 .AND. cloud_base == 9000.0)THEN
       cloud_base = 0.5*(ZW(k)+ZW(k+1))
     ENDIF

     !k = k + 1
  ENDDO
  !print*," maxw before manipulation=", maxw
  maxw = MAX(0.,maxw - 1.0)     ! do nothing for small w (< 1 m/s), but
  Psig_w = MAX(0.0, 1.0 - maxw) ! linearly taper off for w > 1.0 m/s
  Psig_w = MIN(Psig_w, Psig_shcu)
  !print*," maxw=", maxw," Psig_w=",Psig_w," Psig_shcu=",Psig_shcu

  !Completely shut off MF scheme for strong resolved-scale vertical velocities.
  fltv2 = fltv
  IF(Psig_w == 0.0 .and. fltv > 0.0) fltv2 = -1.*fltv

  ! If surface buoyancy is positive we do integration, otherwise no.
  ! Also, ensure that it is at least slightly superadiabatic up through 50 m
  superadiabatic = .false.
  IF((landsea-1.5).GE.0)THEN
     hux = -0.001   ! WATER  ! dT/dz must be < - 0.1 K per 100 m.
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
  !   (5) limit to reduce max plume sizes in weakly forced conditions. This is only
  !       meant to "soften" the activation of the mass-flux scheme.
  ! Criteria (1)
    NUP2 = max(1,min(NUP,INT(dx*dcut/dl)))
  !Criteria (2)
    maxwidth = 1.1*PBLH 
  ! Criteria (3)
    maxwidth = MIN(maxwidth,0.5*cloud_base)
  ! Criteria (4)
    wspd_pbl=SQRT(MAX(u(kts)**2 + v(kts)**2, 0.01))
    !Note: area fraction (acfac) is modified below
  ! Criteria (5) - only a function of flt (not fltv)
    if ((landsea-1.5).LT.0) then  !land
      !width_flx = MAX(MIN(1000.*(0.6*tanh((flt - 0.050)/0.03) + .5),1000.), 0.)
      width_flx = MAX(MIN(1000.*(0.6*tanh((flt - 0.040)/0.03) + .5),1000.), 0.) 
    else                          !water
      width_flx = MAX(MIN(1000.*(0.6*tanh((flt - 0.003)/0.01) + .5),1000.), 0.)
    endif
    maxwidth = MIN(maxwidth,width_flx)
  ! Convert maxwidth to number of plumes
    NUP2 = MIN(MAX(INT((maxwidth - MOD(maxwidth,100.))/100), 0), NUP2)

  !Initialize values for 2d output fields:
  ktop = 0
  ztop = 0.0
  maxmf= 0.0

  IF ( fltv2 > 0.002 .AND. NUP2 .GE. 1 .AND. superadiabatic) then
    !PRINT*," Conditions met to run mass-flux scheme",fltv2,pblh

    ! Find coef C for number size density N
    cn = 0.
    d=-1.9  !set d to value suggested by Neggers 2015 (JAMES).
    !d=-1.9 + .2*tanh((fltv2 - 0.05)/0.15) 
    do I=1,NUP !NUP2
       IF(I > NUP2) exit
       l  = dl*I                            ! diameter of plume
       cn = cn + l**d * (l*l)/(dx*dx) * dl  ! sum fractional area of each plume
    enddo
    C = Atot/cn   !Normalize C according to the defined total fraction (Atot)

    ! Make updraft area (UPA) a function of the buoyancy flux
    if ((landsea-1.5).LT.0) then  !land
       !acfac = .5*tanh((fltv2 - 0.03)/0.09) + .5
       !acfac = .5*tanh((fltv2 - 0.02)/0.09) + .5
       acfac = .5*tanh((fltv2 - 0.02)/0.05) + .5
    else                          !water
       acfac = .5*tanh((fltv2 - 0.01)/0.03) + .5
    endif
    !add a windspeed-dependent adjustment to acfac that tapers off
    !the mass-flux scheme linearly above sfc wind speeds of 20 m/s:
    ac_wsp = 1.0 - min(max(wspd_pbl - 20.0, 0.0), 10.0)/10.0
    !reduce area fraction beneath cloud bases < 1200 m AGL
    ac_cld = min(cloud_base/1200., 1.0)
    acfac  = acfac * min(ac_wsp, ac_cld)

    ! Find the portion of the total fraction (Atot) of each plume size:
    An2 = 0.
    do I=1,NUP !NUP2
       IF(I > NUP2) exit
       l  = dl*I                            ! diameter of plume
       N  = C*l**d                          ! number density of plume n
       UPA(1,I) = N*l*l/(dx*dx) * dl        ! fractional area of plume n

       UPA(1,I) = UPA(1,I)*acfac
       An2 = An2 + UPA(1,I)                 ! total fractional area of all plumes
       !print*," plume size=",l,"; area=",UPA(1,I),"; total=",An2
    end do

    ! set initial conditions for updrafts
    z0=50.
    pwmin=0.1       ! was 0.5
    pwmax=0.4       ! was 3.0

    wstar=max(1.E-2,(gtr*fltv2*pblh)**(onethird))
    qstar=max(flq,1.0E-5)/wstar
    thstar=flt/wstar

    IF((landsea-1.5).GE.0)THEN
       csigma = 1.34   ! WATER
    ELSE
       csigma = 1.34   ! LAND
    ENDIF

    if (env_subs) then
       exc_fac = 0.0
    else
       if ((landsea-1.5).GE.0) then
         !water: increase factor to compensate for decreased pwmin/pwmax
         exc_fac = 0.58*4.0*min(cloud_base/1000., 1.0)
       else
         !land: no need to increase factor - already sufficiently large superadiabatic layers
         exc_fac = 0.58
       endif
    endif

    !Note: sigmaW is typically about 0.5*wstar
    sigmaW =csigma*wstar*(z0/pblh)**(onethird)*(1 - 0.8*z0/pblh)
    sigmaQT=csigma*qstar*(z0/pblh)**(onethird)
    sigmaTH=csigma*thstar*(z0/pblh)**(onethird)

    !Note: Given the pwmin & pwmax set above, these max/mins are
    !      rarely exceeded. 
    wmin=MIN(sigmaW*pwmin,0.1)
    wmax=MIN(sigmaW*pwmax,0.5)

    !SPECIFY SURFACE UPDRAFT PROPERTIES AT MODEL INTERFACE BETWEEN K = 1 & 2
    DO I=1,NUP !NUP2
       IF(I > NUP2) exit
       wlv=wmin+(wmax-wmin)/NUP2*(i-1)

       !SURFACE UPDRAFT VERTICAL VELOCITY
       UPW(1,I)=wmin + REAL(i)/REAL(NUP)*(wmax-wmin)
       !IF (UPW(1,I) > 0.5*ZW(2)/dt) UPW(1,I) = 0.5*ZW(2)/dt

       UPU(1,I)=(U(KTS)*DZ(KTS+1)+U(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPV(1,I)=(V(KTS)*DZ(KTS+1)+V(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQC(1,I)=0.0
       !UPQC(1,I)=(QC(KTS)*DZ(KTS+1)+QC(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))

       exc_heat = exc_fac*UPW(1,I)*sigmaTH/sigmaW
       UPTHV(1,I)=(THV(KTS)*DZ(KTS+1)+THV(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1)) &
           &     + exc_heat
!was       UPTHL(1,I)= UPTHV(1,I)/(1.+svp1*UPQT(1,I))  !assume no saturated parcel at surface
       UPTHL(1,I)=(THL(KTS)*DZ(KTS+1)+THL(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1)) &
           &     + exc_heat

       !calculate exc_moist by use of surface fluxes
       exc_moist=exc_fac*UPW(1,I)*sigmaQT/sigmaW
       !calculate exc_moist by conserving rh:
!       tk_int  =(tk(kts)*dz(kts+1)+tk(kts+1)*dz(kts))/(dz(kts+1)+dz(kts))
!       pk      =(p(kts)*dz(kts+1)+p(kts+1)*dz(kts))/(dz(kts+1)+dz(kts))
!       qtk     =(qt(kts)*dz(kts+1)+qt(kts+1)*dz(kts))/(dz(kts)+dz(kts+1))
!       qsat_tk = qsat_blend(tk_int,  pk)    ! saturation water vapor mixing ratio at tk and p
!       rhgrid  =MAX(MIN(1.0,qtk/MAX(1.E-8,qsat_tk)),0.001)
!       tk_int  = tk_int + exc_heat
!       qsat_tk = qsat_blend(tk_int,  pk) 
!       exc_moist= max(rhgrid*qsat_tk - qtk, 0.0)
       UPQT(1,I)=(QT(KTS)*DZ(KTS+1)+QT(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))&
            &     +exc_moist

       UPQKE(1,I)=(QKE(KTS)*DZ(KTS+1)+QKE(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNC(1,I)=(QNC(KTS)*DZ(KTS+1)+QNC(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNI(1,I)=(QNI(KTS)*DZ(KTS+1)+QNI(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNWFA(1,I)=(QNWFA(KTS)*DZ(KTS+1)+QNWFA(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNIFA(1,I)=(QNIFA(KTS)*DZ(KTS+1)+QNIFA(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
       UPQNBCA(1,I)=(QNBCA(KTS)*DZ(KTS+1)+QNBCA(KTS+1)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
    ENDDO

    IF ( mix_chem ) THEN
      DO I=1,NUP !NUP2
        IF(I > NUP2) exit
        do ic = 1,nchem
          UPCHEM(1,I,ic)=(chem1(KTS,ic)*DZ(KTS+1)+chem1(KTS+1,ic)*DZ(KTS))/(DZ(KTS)+DZ(KTS+1))
        enddo
      ENDDO
    ENDIF

    !Initialize environmental variables which can be modified by detrainment
    DO k=kts,kte
       envm_thl(k)=THL(k)
       envm_sqv(k)=QV(k)
       envm_sqc(k)=QC(k)
       envm_u(k)=U(k)
       envm_v(k)=V(k)
    ENDDO

    !dxsa is scale-adaptive factor governing the pressure-gradient term of the momentum transport
    dxsa = 1. - MIN(MAX((12000.0-dx)/(12000.0-3000.0), 0.), 1.)

    ! do integration  updraft
    DO I=1,NUP !NUP2
       IF(I > NUP2) exit
       QCn = 0.
       overshoot = 0
       l  = dl*I                            ! diameter of plume
       DO k=KTS+1,KTE-1
          !Entrainment from Tian and Kuang (2016)
          !ENT(k,i) = 0.35/(MIN(MAX(UPW(K-1,I),0.75),1.9)*l)
          wmin = 0.3 + l*0.0005 !* MAX(pblh-ZW(k+1), 0.0)/pblh
          ENT(k,i) = 0.33/(MIN(MAX(UPW(K-1,I),wmin),0.9)*l)

          !Entrainment from Negggers (2015, JAMES)
          !ENT(k,i) = 0.02*l**-0.35 - 0.0009
          !ENT(k,i) = 0.04*l**-0.50 - 0.0009   !more plume diversity
          !ENT(k,i) = 0.04*l**-0.495 - 0.0009  !"neg1+"

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

          ! Define environment U & V at the model interface levels
          Uk  =(U(k)*DZ(k+1)+U(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
          Ukm1=(U(k-1)*DZ(k)+U(k)*DZ(k-1))/(DZ(k-1)+DZ(k))
          Vk  =(V(k)*DZ(k+1)+V(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
          Vkm1=(V(k-1)*DZ(k)+V(k)*DZ(k-1))/(DZ(k-1)+DZ(k))

          ! Linear entrainment:
          EntExp= ENT(K,I)*(ZW(k+1)-ZW(k))
          EntExm= EntExp*0.3333    !reduce entrainment for momentum
          QTn =UPQT(k-1,I) *(1.-EntExp) + QT(k)*EntExp
          THLn=UPTHL(k-1,I)*(1.-EntExp) + THL(k)*EntExp
          Un  =UPU(k-1,I)  *(1.-EntExm) + U(k)*EntExm + dxsa*pgfac*(Uk - Ukm1)
          Vn  =UPV(k-1,I)  *(1.-EntExm) + V(k)*EntExm + dxsa*pgfac*(Vk - Vkm1)
          QKEn=UPQKE(k-1,I)*(1.-EntExp) + QKE(k)*EntExp
          QNCn=UPQNC(k-1,I)*(1.-EntExp) + QNC(k)*EntExp
          QNIn=UPQNI(k-1,I)*(1.-EntExp) + QNI(k)*EntExp
          QNWFAn=UPQNWFA(k-1,I)*(1.-EntExp) + QNWFA(k)*EntExp
          QNIFAn=UPQNIFA(k-1,I)*(1.-EntExp) + QNIFA(k)*EntExp
          QNBCAn=UPQNBCA(k-1,I)*(1.-EntExp) + QNBCA(k)*EntExp

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

          if ( mix_chem ) then
            do ic = 1,nchem
              ! Exponential Entrainment:
              !chemn(ic) = chem(k,ic)*(1-EntExp)+UPCHEM(K-1,I,ic)*EntExp
              ! Linear entrainment:
              chemn(ic)=UPCHEM(k-1,i,ic)*(1.-EntExp) + chem1(k,ic)*EntExp
            enddo
          endif

          ! Define pressure at model interface
          Pk    =(P(k)*DZ(k+1)+P(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
          ! Compute plume properties thvn and qcn
          call condensation_edmf(QTn,THLn,Pk,ZW(k+1),THVn,QCn)

          ! Define environment THV at the model interface levels
          THVk  =(THV(k)*DZ(k+1)+THV(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
          THVkm1=(THV(k-1)*DZ(k)+THV(k)*DZ(k-1))/(DZ(k-1)+DZ(k))

!          B=g*(0.5*(THVn+UPTHV(k-1,I))/THV(k-1) - 1.0)
          B=grav*(THVn/THVk - 1.0)
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
          IF (Wn <= 0.0 .AND. overshoot == 0) THEN
             overshoot = 1
             IF ( THVk-THVkm1 .GT. 0.0 ) THEN
                bvf = SQRT( gtr*(THVk-THVkm1)/dz(k) )
                !vertical Froude number
                Frz = UPW(K-1,I)/(bvf*dz(k))
                !IF ( Frz >= 0.5 ) Wn =  MIN(Frz,1.0)*UPW(K-1,I)
                dzp = dz(k)*MAX(MIN(Frz,1.0),0.0) ! portion of highest layer the plume penetrates
             ENDIF
          ELSE
             dzp = dz(k)
          ENDIF

          !Limit very tall plumes
          Wn=Wn*EXP(-MAX(ZW(k+1)-MIN(pblh+2000.,3500.),0.0)/1000.)

          !JOE- minimize the plume penetratration in stratocu-topped PBL
   !       IF (fltv2 < 0.06) THEN
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
          envm_thl(k)=envm_thl(k) + (0.5*(thl_ent + UPTHL(K-1,I)) - thl(k))*detrate*aratio*MIN(dzp,dzpmax)
          qv_ent = 0.5*(MAX(qt_ent-qc_ent,0.) + MAX(UPQT(K-1,I)-UPQC(K-1,I),0.))
          envm_sqv(k)=envm_sqv(k) + (qv_ent-QV(K))*detrate*aratio*MIN(dzp,dzpmax)
          IF (UPQC(K-1,I) > 1E-8) THEN
             IF (QC(K) > 1E-6) THEN
                qc_grid = QC(K)
             ELSE
                qc_grid = cldfra_bl1d(k)*qc_bl1d(K)
             ENDIF
             envm_sqc(k)=envm_sqc(k) + MAX(UPA(K-1,I)*0.5*(QCn + UPQC(K-1,I)) - qc_grid, 0.0)*detrate*aratio*MIN(dzp,dzpmax)
          ENDIF
          envm_u(k)  =envm_u(k)   + (0.5*(Un + UPU(K-1,I)) - U(K))*detrateUV*aratio*MIN(dzp,dzpmax)
          envm_v(k)  =envm_v(k)   + (0.5*(Vn + UPV(K-1,I)) - V(K))*detrateUV*aratio*MIN(dzp,dzpmax)

          IF (Wn > 0.) THEN
             !Update plume variables at current k index
             UPW(K,I)=Wn  !sqrt(Wn2)
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
             UPQNBCA(K,I)=QNBCAn
             UPA(K,I)=UPA(K-1,I)
             IF ( mix_chem ) THEN
               do ic = 1,nchem
                 UPCHEM(k,I,ic) = chemn(ic)
               enddo
             ENDIF
             ktop = MAX(ktop,k)
          ELSE
             exit  !exit k-loop
          END IF
       ENDDO
       IF (debug_mf == 1) THEN
          IF (MAXVAL(UPW(:,I)) > 10.0 .OR. MINVAL(UPA(:,I)) < 0.0 .OR. &
              MAXVAL(UPA(:,I)) > Atot .OR. NUP2 > 10) THEN
             ! surface values
             print *,'flq:',flq,' fltv:',fltv2,' Nup2=',Nup2
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
    DO i=1,NUP !NUP2
      IF(I > NUP2) exit
      DO k=KTS,KTE-1
        IF(k > ktop) exit
        rho_int     = (rho(k)*DZ(k+1)+rho(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
        s_aw(k+1)   = s_aw(k+1)    + rho_int*UPA(K,i)*UPW(K,i)*Psig_w
        s_awthl(k+1)= s_awthl(k+1) + rho_int*UPA(K,i)*UPW(K,i)*UPTHL(K,i)*Psig_w
        s_awqt(k+1) = s_awqt(k+1)  + rho_int*UPA(K,i)*UPW(K,i)*UPQT(K,i)*Psig_w
        !to conform to grid mean properties, move qc to qv in grid mean
        !saturated layers, so total water fluxes are preserved but 
        !negative qc fluxes in unsaturated layers is reduced.
        IF (qc(k) > 1e-12 .OR. qc(k+1) > 1e-12) then
          qc_plume = UPQC(K,i)
        ELSE
          qc_plume = 0.0
        ENDIF
        s_awqc(k+1) = s_awqc(k+1)  + rho_int*UPA(K,i)*UPW(K,i)*qc_plume*Psig_w
        IF (momentum_opt > 0) THEN
          s_awu(k+1)  = s_awu(k+1)   + rho_int*UPA(K,i)*UPW(K,i)*UPU(K,i)*Psig_w
          s_awv(k+1)  = s_awv(k+1)   + rho_int*UPA(K,i)*UPW(K,i)*UPV(K,i)*Psig_w
        ENDIF
        IF (tke_opt > 0) THEN
          s_awqke(k+1)= s_awqke(k+1) + rho_int*UPA(K,i)*UPW(K,i)*UPQKE(K,i)*Psig_w
        ENDIF
        s_awqv(k+1) = s_awqt(k+1)  - s_awqc(k+1)
      ENDDO
    ENDDO

    IF ( mix_chem ) THEN
      DO k=KTS,KTE
        IF(k > KTOP) exit
        rho_int     = (rho(k)*DZ(k+1)+rho(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
        DO i=1,NUP !NUP2
          IF(I > NUP2) exit
          do ic = 1,nchem
            s_awchem(k+1,ic) = s_awchem(k+1,ic) + rho_int*UPA(K,i)*UPW(K,i)*UPCHEM(K,i,ic)*Psig_w
          enddo
        ENDDO
      ENDDO
    ENDIF

    IF (scalar_opt > 0) THEN
      DO k=KTS,KTE
        IF(k > KTOP) exit
        rho_int     = (rho(k)*DZ(k+1)+rho(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
        DO I=1,NUP !NUP2
          IF (I > NUP2) exit
          s_awqnc(k+1)= s_awqnc(K+1) + rho_int*UPA(K,i)*UPW(K,i)*UPQNC(K,i)*Psig_w
          s_awqni(k+1)= s_awqni(K+1) + rho_int*UPA(K,i)*UPW(K,i)*UPQNI(K,i)*Psig_w
          s_awqnwfa(k+1)= s_awqnwfa(K+1) + rho_int*UPA(K,i)*UPW(K,i)*UPQNWFA(K,i)*Psig_w
          s_awqnifa(k+1)= s_awqnifa(K+1) + rho_int*UPA(K,i)*UPW(K,i)*UPQNIFA(K,i)*Psig_w
          s_awqnbca(k+1)= s_awqnbca(K+1) + rho_int*UPA(K,i)*UPW(K,i)*UPQNBCA(K,i)*Psig_w
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
       s_awqnbca= s_awqnbca*adjustment
       IF (momentum_opt > 0) THEN
          s_awu  = s_awu*adjustment
          s_awv  = s_awv*adjustment
       ENDIF
       IF (tke_opt > 0) THEN
          s_awqke= s_awqke*adjustment
       ENDIF
       IF ( mix_chem ) THEN
          s_awchem = s_awchem*adjustment
       ENDIF
       UPA = UPA*adjustment
    ENDIF
    !Print*,"adjustment=",adjustment," fluxportion=",fluxportion," flt=",flt

    !Calculate mean updraft properties for output:
    !all edmf_* variables at k=1 correspond to the interface at top of first model layer
    DO k=KTS,KTE-1
      IF(k > KTOP) exit
      rho_int     = (rho(k)*DZ(k+1)+rho(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
      DO I=1,NUP !NUP2
        IF(I > NUP2) exit
        edmf_a(K)  =edmf_a(K)  +UPA(K,i)
        edmf_w(K)  =edmf_w(K)  +rho_int*UPA(K,i)*UPW(K,i)
        edmf_qt(K) =edmf_qt(K) +rho_int*UPA(K,i)*UPQT(K,i)
        edmf_thl(K)=edmf_thl(K)+rho_int*UPA(K,i)*UPTHL(K,i)
        edmf_ent(K)=edmf_ent(K)+rho_int*UPA(K,i)*ENT(K,i)
        edmf_qc(K) =edmf_qc(K) +rho_int*UPA(K,i)*UPQC(K,i)
      ENDDO

      !Note that only edmf_a is multiplied by Psig_w. This takes care of the
      !scale-awareness of the subsidence below:
      IF (edmf_a(k)>0.) THEN
        edmf_w(k)=edmf_w(k)/edmf_a(k)
        edmf_qt(k)=edmf_qt(k)/edmf_a(k)
        edmf_thl(k)=edmf_thl(k)/edmf_a(k)
        edmf_ent(k)=edmf_ent(k)/edmf_a(k)
        edmf_qc(k)=edmf_qc(k)/edmf_a(k)
        edmf_a(k)=edmf_a(k)*Psig_w

        !FIND MAXIMUM MASS-FLUX IN THE COLUMN:
        IF(edmf_a(k)*edmf_w(k) > maxmf) maxmf = edmf_a(k)*edmf_w(k)
      ENDIF
    ENDDO ! end k

    !smoke/chem
    IF ( mix_chem ) THEN
      DO k=kts,kte-1
        IF(k > KTOP) exit
        rho_int     = (rho(k)*dz(k+1)+rho(k+1)*dz(k))/(dz(k+1)+dz(k))
        DO I=1,NUP !NUP2
          IF(I > NUP2) exit
          do ic = 1,nchem
            edmf_chem(k,ic) = edmf_chem(k,ic) + rho_int*UPA(K,I)*UPCHEM(k,i,ic)
          enddo
        ENDDO

        IF (edmf_a(k)>0.) THEN
          do ic = 1,nchem
            edmf_chem(k,ic) = edmf_chem(k,ic)/edmf_a(k)
          enddo
        ENDIF
      ENDDO ! end k
    ENDIF

    !Calculate the effects environmental subsidence.
    !All envi_*variables are valid at the interfaces, like the edmf_* variables
    IF (env_subs) THEN
       DO k=kts+1,kte-1
          !First, smooth the profiles of w & a, since sharp vertical gradients
          !in plume variables are not likely extended to env variables
          !Note1: w is treated as negative further below
          !Note2: both w & a will be transformed into env variables further below
          envi_w(k) = onethird*(edmf_w(k-1)+edmf_w(k)+edmf_w(k+1))
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
       DO k=kts,kte
          temp=envi_a(k)
          envi_a(k)=1.0-temp
          envi_w(k)=csub*sublim*envi_w(k)*temp/(1.-temp)
       ENDDO
       !calculate tendencies from subsidence and detrainment valid at the middle of
       !each model layer. The lowest model layer uses an assumes w=0 at the surface.
       dzi(kts)    = 0.5*(dz(kts)+dz(kts+1))
       rho_int     = (rho(kts)*dz(kts+1)+rho(kts+1)*dz(kts))/(dz(kts+1)+dz(kts))
       sub_thl(kts)= 0.5*envi_w(kts)*envi_a(kts)*                               &
                     (rho(kts+1)*thl(kts+1)-rho(kts)*thl(kts))/dzi(kts)/rho_int
       sub_sqv(kts)= 0.5*envi_w(kts)*envi_a(kts)*                               &
                     (rho(kts+1)*qv(kts+1)-rho(kts)*qv(kts))/dzi(kts)/rho_int
       DO k=kts+1,kte-1
          dzi(k)    = 0.5*(dz(k)+dz(k+1))
          rho_int   = (rho(k)*dz(k+1)+rho(k+1)*dz(k))/(dz(k+1)+dz(k))
          sub_thl(k)= 0.5*(envi_w(k)+envi_w(k-1))*0.5*(envi_a(k)+envi_a(k-1)) * &
                      (rho(k+1)*thl(k+1)-rho(k)*thl(k))/dzi(k)/rho_int
          sub_sqv(k)= 0.5*(envi_w(k)+envi_w(k-1))*0.5*(envi_a(k)+envi_a(k-1)) * &
                      (rho(k+1)*qv(k+1)-rho(k)*qv(k))/dzi(k)/rho_int
       ENDDO

       DO k=KTS,KTE-1
          det_thl(k)=Cdet*(envm_thl(k)-thl(k))*envi_a(k)*Psig_w
          det_sqv(k)=Cdet*(envm_sqv(k)-qv(k))*envi_a(k)*Psig_w
          det_sqc(k)=Cdet*(envm_sqc(k)-qc(k))*envi_a(k)*Psig_w
       ENDDO

       IF (momentum_opt > 0) THEN
         rho_int     = (rho(kts)*dz(kts+1)+rho(kts+1)*dz(kts))/(dz(kts+1)+dz(kts))
         sub_u(kts)=0.5*envi_w(kts)*envi_a(kts)*                               &
                    (rho(kts+1)*u(kts+1)-rho(kts)*u(kts))/dzi(kts)/rho_int
         sub_v(kts)=0.5*envi_w(kts)*envi_a(kts)*                               &
                    (rho(kts+1)*v(kts+1)-rho(kts)*v(kts))/dzi(kts)/rho_int
         DO k=kts+1,kte-1
            rho_int   = (rho(k)*dz(k+1)+rho(k+1)*dz(k))/(dz(k+1)+dz(k))
            sub_u(k)=0.5*(envi_w(k)+envi_w(k-1))*0.5*(envi_a(k)+envi_a(k-1)) * &
                      (rho(k+1)*u(k+1)-rho(k)*u(k))/dzi(k)/rho_int
            sub_v(k)=0.5*(envi_w(k)+envi_w(k-1))*0.5*(envi_a(k)+envi_a(k-1)) * &
                      (rho(k+1)*v(k+1)-rho(k)*v(k))/dzi(k)/rho_int
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
            !interpolate plume quantities to mass levels
            Aup = (edmf_a(k)*dzi(k-1)+edmf_a(k-1)*dzi(k))/(dzi(k-1)+dzi(k))
            THp = (edmf_th(k)*dzi(k-1)+edmf_th(k-1)*dzi(k))/(dzi(k-1)+dzi(k))
            QTp = (edmf_qt(k)*dzi(k-1)+edmf_qt(k-1)*dzi(k))/(dzi(k-1)+dzi(k))
            !convert TH to T
!            t = THp*exner(k)
            !SATURATED VAPOR PRESSURE
            esat = esat_blend(tk(k))
            !SATURATED SPECIFIC HUMIDITY
            qsl=ep_2*esat/max(1.e-7,(p(k)-ep_3*esat)) 

            !condensed liquid in the plume on mass levels
            IF (edmf_qc(k)>0.0 .AND. edmf_qc(k-1)>0.0)THEN
              QCp = (edmf_qc(k)*dzi(k-1)+edmf_qc(k-1)*dzi(k))/(dzi(k-1)+dzi(k))
            ELSE
              QCp = MAX(edmf_qc(k),edmf_qc(k-1))
            ENDIF

            !COMPUTE CLDFRA & QC_BL FROM MASS-FLUX SCHEME and recompute vt & vq
            xl = xl_blend(tk(k))                ! obtain blended heat capacity
            qsat_tk = qsat_blend(tk(k),p(k))    ! get saturation water vapor mixing ratio
                                                !   at t and p
            rsl = xl*qsat_tk / (r_v*tk(k)**2)   ! slope of C-C curve at t (abs temp)
                                                ! CB02, Eqn. 4
            cpm = cp + qt(k)*cpv                ! CB02, sec. 2, para. 1
            a   = 1./(1. + xl*rsl/cpm)          ! CB02 variable "a"
            b9  = a*rsl                         ! CB02 variable "b" 

            q2p  = xlvcp/exner(k)
            pt = thl(k) +q2p*QCp*Aup ! potential temp (env + plume)
            bb = b9*tk(k)/pt ! bb is "b9" in BCMT95.  Their "b9" differs from
                           ! "b9" in CB02 by a factor
                           ! of T/theta.  Strictly, b9 above is formulated in
                           ! terms of sat. mixing ratio, but bb in BCMT95 is
                           ! cast in terms of sat. specific humidity.  The
                           ! conversion is neglected here.
            qww   = 1.+0.61*qt(k)
            alpha = 0.61*pt
            beta  = pt*xl/(tk(k)*cp) - 1.61*pt
            !Buoyancy flux terms have been moved to the end of this section...

            !Now calculate convective component of the cloud fraction:
            if (a > 0.0) then
               f = MIN(1.0/a, 4.0)              ! f is vertical profile scaling function (CB2005)
            else
               f = 1.0
            endif

            !CB form:
            !sigq = 3.5E-3 * Aup * 0.5*(edmf_w(k)+edmf_w(k-1)) * f  ! convective component of sigma (CB2005)
            !sigq = SQRT(sigq**2 + sgm(k)**2)    ! combined conv + stratus components
            !Per S.DeRoode 2009?
            !sigq = 4. * Aup * (QTp - qt(k))
            sigq = 10. * Aup * (QTp - qt(k)) 
            !constrain sigq wrt saturation:
            sigq = max(sigq, qsat_tk*0.02 )
            sigq = min(sigq, qsat_tk*0.25 )

            qmq = a * (qt(k) - qsat_tk)           ! saturation deficit/excess;
            Q1  = qmq/sigq                        !   the numerator of Q1

            if ((landsea-1.5).GE.0) then      ! WATER
               !modified form from LES
               !mf_cf = min(max(0.5 + 0.36 * atan(1.20*(Q1+0.2)),0.01),0.6)
               !Original CB
               mf_cf = min(max(0.5 + 0.36 * atan(1.55*Q1),0.01),0.6)
               mf_cf = max(mf_cf, 1.2 * Aup)
               mf_cf = min(mf_cf, 5.0 * Aup)
            else                              ! LAND
               !LES form
               !mf_cf = min(max(0.5 + 0.36 * atan(1.20*(Q1+0.4)),0.01),0.6)
               !Original CB
               mf_cf = min(max(0.5 + 0.36 * atan(1.55*Q1),0.01),0.6)
               mf_cf = max(mf_cf, 1.75 * Aup)
               mf_cf = min(mf_cf, 5.0  * Aup)
            endif

            ! WA TEST 4/15/22 use fit to Aup rather than CB
            !IF (Aup > 0.1) THEN
            !   mf_cf = 2.5 * Aup
            !ELSE
            !   mf_cf = 1.8 * Aup
            !ENDIF

            !IF ( debug_code ) THEN
            !   print*,"In MYNN, StEM edmf"
            !   print*,"  CB: env qt=",qt(k)," qsat=",qsat_tk
            !   print*,"  k=",k," satdef=",QTp - qsat_tk," sgm=",sgm(k)
            !   print*,"  CB: sigq=",sigq," qmq=",qmq," tk=",tk(k)
            !   print*,"  CB: mf_cf=",mf_cf," cldfra_bl=",cldfra_bl1d(k)," edmf_a=",edmf_a(k)
            !ENDIF

            ! Update cloud fractions and specific humidities in grid cells
            ! where the mass-flux scheme is active. The specific humidities
            ! are converted to grid means (not in-cloud quantities).

            if ((landsea-1.5).GE.0) then     ! water
               !don't overwrite stratus CF & qc_bl - degrades marine stratus
               if (cldfra_bl1d(k) < cf_thresh) then
                  if (QCp * Aup > 5e-5) then
                     qc_bl1d(k) = 1.86 * (QCp * Aup) - 2.2e-5
                  else
                     qc_bl1d(k) = 1.18 * (QCp * Aup)
                  endif
                  if (mf_cf .ge. Aup) then
                    qc_bl1d(k) = qc_bl1d(k) / mf_cf
                  endif
                  cldfra_bl1d(k) = mf_cf
                  Ac_mf          = mf_cf
               endif
            else                             ! land
               if (QCp * Aup > 5e-5) then
                  qc_bl1d(k) = 1.86 * (QCp * Aup) - 2.2e-5
               else
                  qc_bl1d(k) = 1.18 * (QCp * Aup)
               endif
               if (mf_cf .ge. Aup) then
                  qc_bl1d(k) = qc_bl1d(k) / mf_cf
               endif
               cldfra_bl1d(k) = mf_cf
               Ac_mf          = mf_cf
            endif

            !Now recalculate the terms for the buoyancy flux for mass-flux clouds:
            !See mym_condensation for details on these formulations.
            !Use Bechtold and Siebesma (1998) piecewise estimation of Fng with 
            !limits ,since they really should be recalculated after all the other changes...:
            !Only overwrite vt & vq in non-stratus condition
            if (cldfra_bl1d(k) < cf_thresh) then
               !if ((landsea-1.5).GE.0) then      ! WATER
                  Q1=max(Q1,-2.25)
               !else
               !   Q1=max(Q1,-2.0)
               !endif

               if (Q1 .ge. 1.0) then
                  Fng = 1.0
               elseif (Q1 .ge. -1.7 .and. Q1 .lt. 1.0) then
                  Fng = EXP(-0.4*(Q1-1.0))
               elseif (Q1 .ge. -2.5 .and. Q1 .lt. -1.7) then
                  Fng = 3.0 + EXP(-3.8*(Q1+1.7))
               else
                  Fng = min(23.9 + EXP(-1.6*(Q1+2.5)), 60.)
               endif

               !link the buoyancy flux function to active clouds only (c*Aup):
               vt(k) = qww   - (1.5*Aup)*beta*bb*Fng - 1.
               vq(k) = alpha + (1.5*Aup)*beta*a*Fng  - tv0
            endif
         endif
      enddo !k-loop

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
    print *,'flq:',flq,' fltv:',fltv2
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
!>\ingroup gsd_mynn_edmf
!! This subroutine 
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
! rvord .. r_v/r_d  (1.6) 

! number of iterations
  niter=50
! minimum difference (usually converges in < 8 iterations with diff = 2e-5)
  diff=1.e-6

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

subroutine condensation_edmf_r(QT,THL,P,zagl,THV,QC)
!                                                                                                
! zero or one condensation for edmf: calculates THL and QC                                       
! similar to condensation_edmf but with different inputs                                         
!                                                                                                
real,intent(in)   :: QT,THV,P,zagl
real,intent(out)  :: THL, QC

integer :: niter,i
real :: diff,exn,t,th,qs,qcold

! number of iterations                                                                           
  niter=50
! minimum difference                                                                             
  diff=2.e-5

  EXN=(P/p1000mb)**rcp
  ! assume first that th = thv                                                                   
  T = THV*EXN
  !QS = qsat_blend(T,P)                                                                          
  !QC = QS - QT                                                                                  

  QC=0.

  do i=1,NITER
     QCOLD = QC
     T = EXN*THV/(1.+QT*(rvovrd-1.)-rvovrd*QC)
     QS=qsat_blend(T,P)
     QC= MAX((QT-QS),0.)
     if (abs(QC-QCOLD)<Diff) exit
  enddo
  THL = (T - xlv/cp*QC)/EXN

end subroutine condensation_edmf_r

!===============================================================
! ===================================================================
! This is the downdraft mass flux scheme - analogus to edmf_JPL but  
! flipped updraft to downdraft. This scheme is currently only tested 
! for Stratocumulus cloud conditions. For a detailed desctiption of the
! model, see paper.

SUBROUTINE DDMF_JPL(kts,kte,dt,zw,dz,p,              &
              &u,v,th,thl,thv,tk,qt,qv,qc,           &
              &rho,exner,                            &
              &ust,wthl,wqt,pblh,kpbl,               &
              &edmf_a_dd,edmf_w_dd, edmf_qt_dd,      &
              &edmf_thl_dd,edmf_ent_dd,edmf_qc_dd,   &
              &sd_aw,sd_awthl,sd_awqt,               &
              &sd_awqv,sd_awqc,sd_awu,sd_awv,        &
              &sd_awqke,                             &
              &qc_bl1d,cldfra_bl1d,                  &
              &rthraten                              )

        INTEGER, INTENT(IN) :: KTS,KTE,KPBL
        REAL,DIMENSION(KTS:KTE), INTENT(IN) :: U,V,TH,THL,TK,QT,QV,QC,&
            THV,P,rho,exner,rthraten,dz
        ! zw .. heights of the downdraft levels (edges of boxes)
        REAL,DIMENSION(KTS:KTE+1), INTENT(IN) :: ZW
        REAL, INTENT(IN) :: DT,UST,WTHL,WQT,PBLH

  ! outputs - downdraft properties
        REAL,DIMENSION(KTS:KTE), INTENT(OUT) :: edmf_a_dd,edmf_w_dd,   &
                      & edmf_qt_dd,edmf_thl_dd, edmf_ent_dd,edmf_qc_dd

  ! outputs - variables needed for solver (sd_aw - sum ai*wi, sd_awphi - sum ai*wi*phii)
        REAL,DIMENSION(KTS:KTE+1) :: sd_aw, sd_awthl, sd_awqt, sd_awu, &
                            sd_awv, sd_awqc, sd_awqv, sd_awqke, sd_aw2

        REAL,DIMENSION(KTS:KTE), INTENT(IN) :: qc_bl1d, cldfra_bl1d

        INTEGER, PARAMETER :: NDOWN=5, debug_mf=0 !fixing number of plumes to 5
  ! draw downdraft starting height randomly between cloud base and cloud top
        INTEGER, DIMENSION(1:NDOWN) :: DD_initK
        REAL   , DIMENSION(1:NDOWN) :: randNum
  ! downdraft properties
        REAL,DIMENSION(KTS:KTE+1,1:NDOWN) :: DOWNW,DOWNTHL,DOWNQT,&
                    DOWNQC,DOWNA,DOWNU,DOWNV,DOWNTHV

  ! entrainment variables
        REAl,DIMENSION(KTS+1:KTE+1,1:NDOWN) :: ENT,ENTf
        INTEGER,DIMENSION(KTS+1:KTE+1,1:NDOWN) :: ENTi

  ! internal variables
        INTEGER :: K,I,ki, kminrad, qlTop, p700_ind, qlBase
        REAL :: wthv,wstar,qstar,thstar,sigmaW,sigmaQT,sigmaTH,z0, &
            pwmin,pwmax,wmin,wmax,wlv,wtv,went,mindownw
        REAL :: B,QTn,THLn,THVn,QCn,Un,Vn,QKEn,Wn2,Wn,THVk,Pk, &
                EntEXP,EntW, Beta_dm, EntExp_M, rho_int
        REAL :: jump_thetav, jump_qt, jump_thetal, &
                refTHL, refTHV, refQT
  ! DD specific internal variables
        REAL :: minrad,zminrad, radflux, F0, wst_rad, wst_dd
        logical :: cloudflg

        REAL :: sigq,xl,rsl,cpm,a,mf_cf,diffqt,&
               Fng,qww,alpha,beta,bb,f,pt,t,q2p,b9,satvp,rhgrid

  ! w parameters
        REAL,PARAMETER :: &
            &Wa=1., &
            &Wb=1.5,&
            &Z00=100.,&
            &BCOEFF=0.2
  ! entrainment parameters
        REAL,PARAMETER :: &
        & L0=80,&
        & ENT0=0.2

   pwmin=-3. ! drawing from the negative tail -3sigma to -1sigma
   pwmax=-1.

  ! initialize downdraft properties
   DOWNW=0.
   DOWNTHL=0.
   DOWNTHV=0.
   DOWNQT=0.
   DOWNA=0.
   DOWNU=0.
   DOWNV=0.
   DOWNQC=0.
   ENT=0.
   DD_initK=0

   edmf_a_dd  =0.
   edmf_w_dd  =0.
   edmf_qt_dd =0.
   edmf_thl_dd=0.
   edmf_ent_dd=0.
   edmf_qc_dd =0.

   sd_aw=0.
   sd_awthl=0.
   sd_awqt=0.
   sd_awqv=0.
   sd_awqc=0.
   sd_awu=0.
   sd_awv=0.
   sd_awqke=0.

  ! FIRST, CHECK FOR STRATOCUMULUS-TOPPED BOUNDARY LAYERS
   cloudflg=.false.
   minrad=100.
   kminrad=kpbl
   zminrad=PBLH
   qlTop = 1 !initialize at 0
   qlBase = 1
   wthv=wthl+svp1*wqt
   do k = MAX(3,kpbl-2),kpbl+3
      if (qc(k).gt. 1.e-6 .AND. cldfra_bl1D(k).gt.0.5) then
          cloudflg=.true. ! found Sc cloud
          qlTop = k       ! index for Sc cloud top
      endif
   enddo

   do k = qlTop, kts, -1
      if (qc(k) .gt. 1E-6) then
         qlBase = k ! index for Sc cloud base
      endif
   enddo
   qlBase = (qlTop+qlBase)/2 ! changed base to half way through the cloud

!   call init_random_seed_1()
!   call RANDOM_NUMBER(randNum)
   do i=1,NDOWN
      ! downdraft starts somewhere between cloud base to cloud top
      ! the probability is equally distributed
      DD_initK(i) = qlTop ! nint(randNum(i)*REAL(qlTop-qlBase)) + qlBase
   enddo

   ! LOOP RADFLUX
   F0 = 0.
   do k = 1, qlTop ! Snippet from YSU, YSU loops until qlTop - 1
      radflux = rthraten(k) * exner(k) ! Converts theta/s to temperature/s
      radflux = radflux * cp / grav * ( p(k) - p(k+1) ) ! Converts K/s to W/m^2
      if ( radflux < 0.0 ) F0 = abs(radflux) + F0
   enddo
   F0 = max(F0, 1.0)
   !found Sc cloud and cloud not at surface, trigger downdraft
   if (cloudflg) then

!      !get entrainent coefficient
!      do i=1,NDOWN
!         do k=kts+1,kte
!            ENTf(k,i)=(ZW(k+1)-ZW(k))/L0
!         enddo
!      enddo
!
!      ! get Poisson P(dz/L0)
!      call Poisson(1,NDOWN,kts+1,kte,ENTf,ENTi)


      ! entrainent: Ent=Ent0/dz*P(dz/L0)
      do i=1,NDOWN
         do k=kts+1,kte
!            ENT(k,i)=real(ENTi(k,i))*Ent0/(ZW(k+1)-ZW(k))
            ENT(k,i) = 0.002
            ENT(k,i) = min(ENT(k,i),0.9/(ZW(k+1)-ZW(k)))
         enddo
      enddo

      !!![EW: INVJUMP] find 700mb height then subtract trpospheric lapse rate!!!
      p700_ind = MINLOC(ABS(p-70000),1)!p1D is 70000
      jump_thetav = thv(p700_ind) - thv(1) - (thv(p700_ind)-thv(qlTop+3))/(ZW(p700_ind)-ZW(qlTop+3))*(ZW(p700_ind)-ZW(qlTop))
      jump_qt = qc(p700_ind) + qv(p700_ind) - qc(1) - qv(1)
      jump_thetal = thl(p700_ind) - thl(1) - (thl(p700_ind)-thl(qlTop+3))/(ZW(p700_ind)-ZW(qlTop+3))*(ZW(p700_ind)-ZW(qlTop))

      refTHL = thl(qlTop) !sum(thl(1:qlTop)) / (qlTop) ! avg over BL for now or just at qlTop
      refTHV = thv(qlTop) !sum(thv(1:qlTop)) / (qlTop)
      refQT  = qt(qlTop)  !sum(qt(1:qlTop))  / (qlTop)

      ! wstar_rad, following Lock and MacVean (1999a)
      wst_rad = ( grav * zw(qlTop) * F0 / (refTHL * rho(qlTop) * cp) ) ** (0.333)
      wst_rad = max(wst_rad, 0.1)
      wstar   = max(0.,(grav/thv(1)*wthv*pblh)**(onethird))
      went    = thv(1) / ( grav * jump_thetav * zw(qlTop) ) * &
                (0.15 * (wstar**3 + 5*ust**3) + 0.35 * wst_rad**3 )
      qstar  = abs(went*jump_qt/wst_rad)
      thstar = F0/rho(qlTop)/cp/wst_rad - went*jump_thetav/wst_rad
      !wstar_dd = mixrad + surface wst
      wst_dd = (0.15 * (wstar**3 + 5*ust**3) + 0.35 * wst_rad**3 ) ** (0.333)

      print*,"qstar=",qstar," thstar=",thstar," wst_dd=",wst_dd
      print*,"F0=",F0," wst_rad=",wst_rad," jump_thv=",jump_thetav
      print*,"entrainment velocity=",went

      sigmaW  = 0.2*wst_dd  ! 0.8*wst_dd !wst_rad tuning parameter ! 0.5 was good
      sigmaQT = 40  * qstar ! 50 was good
      sigmaTH = 1.0 * thstar! 0.5 was good

      wmin=sigmaW*pwmin
      wmax=sigmaW*pwmax
      !print*,"sigw=",sigmaW," wmin=",wmin," wmax=",wmax

      do I=1,NDOWN !downdraft now starts at different height
         ki = DD_initK(I)

         wlv=wmin+(wmax-wmin)/REAL(NDOWN)*(i-1)
         wtv=wmin+(wmax-wmin)/REAL(NDOWN)*i

         !DOWNW(ki,I)=0.5*(wlv+wtv)
         DOWNW(ki,I)=wlv
         !DOWNA(ki,I)=0.5*ERF(wtv/(sqrt(2.)*sigmaW))-0.5*ERF(wlv/(sqrt(2.)*sigmaW))
         DOWNA(ki,I)=.1/REAL(NDOWN)
         DOWNU(ki,I)=(u(ki-1)*DZ(ki) + u(ki)*DZ(ki-1)) /(DZ(ki)+DZ(ki-1))
         DOWNV(ki,I)=(v(ki-1)*DZ(ki) + v(ki)*DZ(ki-1)) /(DZ(ki)+DZ(ki-1))

         !reference now depends on where dd starts
!         refTHL = 0.5 * (thl(ki) + thl(ki-1))
!         refTHV = 0.5 * (thv(ki) + thv(ki-1))
!         refQT  = 0.5 * (qt(ki)  + qt(ki-1) )

         refTHL = (thl(ki-1)*DZ(ki) + thl(ki)*DZ(ki-1)) /(DZ(ki)+DZ(ki-1))
         refTHV = (thv(ki-1)*DZ(ki) + thv(ki)*DZ(ki-1)) /(DZ(ki)+DZ(ki-1))
         refQT  = (qt(ki-1)*DZ(ki)  + qt(ki)*DZ(ki-1))  /(DZ(ki)+DZ(ki-1))

         !DOWNQC(ki,I) = 0.0
         DOWNQC(ki,I) = (qc(ki-1)*DZ(ki) + qc(ki)*DZ(ki-1)) /(DZ(ki)+DZ(ki-1))
         DOWNQT(ki,I) = refQT  !+ 0.5  *DOWNW(ki,I)*sigmaQT/sigmaW
         DOWNTHV(ki,I)= refTHV + 0.01 *DOWNW(ki,I)*sigmaTH/sigmaW
         DOWNTHL(ki,I)= refTHL + 0.01 *DOWNW(ki,I)*sigmaTH/sigmaW

         !input :: QT,THV,P,zagl,  output :: THL, QC
!         Pk  =(P(ki-1)*DZ(ki)+P(ki)*DZ(ki-1))/(DZ(ki)+DZ(ki-1))
!         call condensation_edmf_r(DOWNQT(ki,I),   &
!              &        DOWNTHL(ki,I),Pk,ZW(ki),   &
!              &     DOWNTHV(ki,I),DOWNQC(ki,I)    )

      enddo


      !print*, " Begin integration of downdrafts:"
      DO I=1,NDOWN
         !print *, "Plume # =", I,"======================="
         DO k=DD_initK(I)-1,KTS+1,-1
            !starting at the first interface level below cloud top
            !EntExp=exp(-ENT(K,I)*dz(k))
            !EntExp_M=exp(-ENT(K,I)/3.*dz(k))
            EntExp  =ENT(K,I)*dz(k)
            EntExp_M=ENT(K,I)*0.333*dz(k)

            QTn =DOWNQT(k+1,I) *(1.-EntExp) + QT(k)*EntExp
            THLn=DOWNTHL(k+1,I)*(1.-EntExp) + THL(k)*EntExp
            Un  =DOWNU(k+1,I)  *(1.-EntExp) + U(k)*EntExp_M
            Vn  =DOWNV(k+1,I)  *(1.-EntExp) + V(k)*EntExp_M
            !QKEn=DOWNQKE(k-1,I)*(1.-EntExp) + QKE(k)*EntExp

!            QTn =DOWNQT(K+1,I) +(QT(K) -DOWNQT(K+1,I)) *(1.-EntExp)
!            THLn=DOWNTHL(K+1,I)+(THL(K)-DOWNTHL(K+1,I))*(1.-EntExp)
!            Un  =DOWNU(K+1,I)  +(U(K)  -DOWNU(K+1,I))*(1.-EntExp_M)
!            Vn  =DOWNV(K+1,I)  +(V(K)  -DOWNV(K+1,I))*(1.-EntExp_M)

            ! given new p & z, solve for thvn & qcn
            Pk  =(P(k-1)*DZ(k)+P(k)*DZ(k-1))/(DZ(k)+DZ(k-1))
            call condensation_edmf(QTn,THLn,Pk,ZW(k),THVn,QCn)
!            B=grav*(0.5*(THVn+DOWNTHV(k+1,I))/THV(k)-1.)
            THVk  =(THV(k-1)*DZ(k)+THV(k)*DZ(k-1))/(DZ(k)+DZ(k-1))
            B=grav*(THVn/THVk - 1.0)
!            Beta_dm = 2*Wb*ENT(K,I) + 0.5/(ZW(k)-dz(k)) * &
!                 &    max(1. - exp((ZW(k) -dz(k))/Z00 - 1. ) , 0.)
!            EntW=exp(-Beta_dm * dz(k))
            EntW=EntExp
!            if (Beta_dm >0) then
!               Wn2=DOWNW(K+1,I)**2*EntW - Wa*B/Beta_dm * (1. - EntW)
!            else
!               Wn2=DOWNW(K+1,I)**2      - 2.*Wa*B*dz(k)
!            end if

            mindownw = MIN(DOWNW(K+1,I),-0.2)
            Wn = DOWNW(K+1,I) + (-2.*ENT(K,I)*DOWNW(K+1,I) - &
                    BCOEFF*B/mindownw)*MIN(dz(k), 250.)

            !Do not allow a parcel to accelerate more than 1.25 m/s over 200 m.
            !Add max increase of 2.0 m/s for coarse vertical resolution.
            IF (Wn < DOWNW(K+1,I) - MIN(1.25*dz(k)/200., 2.0))THEN
                Wn = DOWNW(K+1,I) - MIN(1.25*dz(k)/200., 2.0)
            ENDIF
            !Add symmetrical max decrease in w
            IF (Wn > DOWNW(K+1,I) + MIN(1.25*dz(k)/200., 2.0))THEN
                Wn = DOWNW(K+1,I) + MIN(1.25*dz(k)/200., 2.0)
            ENDIF
            Wn = MAX(MIN(Wn,0.0), -3.0)

            !print *, "  k       =",      k,      " z    =", ZW(k)
            !print *, "  entw    =",ENT(K,I),     " Bouy =", B
            !print *, "  downthv =",   THVn,      " thvk =", thvk
            !print *, "  downthl =",   THLn,      " thl  =", thl(k)
            !print *, "  downqt  =",   QTn ,      " qt   =", qt(k)
            !print *, "  downw+1 =",DOWNW(K+1,I), " Wn2  =", Wn

            IF (Wn .lt. 0.) THEN !terminate when velocity is too small
               DOWNW(K,I)  = Wn !-sqrt(Wn2)
               DOWNTHV(K,I)= THVn
               DOWNTHL(K,I)= THLn
               DOWNQT(K,I) = QTn
               DOWNQC(K,I) = QCn
               DOWNU(K,I)  = Un
               DOWNV(K,I)  = Vn
               DOWNA(K,I)  = DOWNA(K+1,I)
            ELSE
               !plumes must go at least 2 levels
               if (DD_initK(I) - K .lt. 2) then
                  DOWNW(:,I)  = 0.0
                  DOWNTHV(:,I)= 0.0
                  DOWNTHL(:,I)= 0.0
                  DOWNQT(:,I) = 0.0
                  DOWNQC(:,I) = 0.0
                  DOWNU(:,I)  = 0.0
                  DOWNV(:,I)  = 0.0
               endif
               exit
            ENDIF
         ENDDO
      ENDDO
   endif ! end cloud flag

   DOWNW(1,:) = 0. !make sure downdraft does not go to the surface
   DOWNA(1,:) = 0.

   ! Combine both moist and dry plume, write as one averaged plume
   ! Even though downdraft starts at different height, average all up to qlTop
   DO k=qlTop,KTS,-1
      DO I=1,NDOWN
         IF (I > NDOWN) exit
         edmf_a_dd(K)  =edmf_a_dd(K)  +DOWNA(K-1,I)
         edmf_w_dd(K)  =edmf_w_dd(K)  +DOWNA(K-1,I)*DOWNW(K-1,I)
         edmf_qt_dd(K) =edmf_qt_dd(K) +DOWNA(K-1,I)*DOWNQT(K-1,I)
         edmf_thl_dd(K)=edmf_thl_dd(K)+DOWNA(K-1,I)*DOWNTHL(K-1,I)
         edmf_ent_dd(K)=edmf_ent_dd(K)+DOWNA(K-1,I)*ENT(K-1,I)
         edmf_qc_dd(K) =edmf_qc_dd(K) +DOWNA(K-1,I)*DOWNQC(K-1,I)
      ENDDO

      IF (edmf_a_dd(k) >0.) THEN
          edmf_w_dd(k)  =edmf_w_dd(k)  /edmf_a_dd(k)
          edmf_qt_dd(k) =edmf_qt_dd(k) /edmf_a_dd(k)
          edmf_thl_dd(k)=edmf_thl_dd(k)/edmf_a_dd(k)
          edmf_ent_dd(k)=edmf_ent_dd(k)/edmf_a_dd(k)
          edmf_qc_dd(k) =edmf_qc_dd(k) /edmf_a_dd(k)
      ENDIF
   ENDDO

   !
   ! computing variables needed for solver
   !

   DO k=KTS,qlTop
      rho_int = (rho(k)*DZ(k+1)+rho(k+1)*DZ(k))/(DZ(k+1)+DZ(k))
      DO I=1,NDOWN
         sd_aw(k)   =sd_aw(k)   +rho_int*DOWNA(k,i)*DOWNW(k,i)
         sd_awthl(k)=sd_awthl(k)+rho_int*DOWNA(k,i)*DOWNW(k,i)*DOWNTHL(k,i)
         sd_awqt(k) =sd_awqt(k) +rho_int*DOWNA(k,i)*DOWNW(k,i)*DOWNQT(k,i)
         sd_awqc(k) =sd_awqc(k) +rho_int*DOWNA(k,i)*DOWNW(k,i)*DOWNQC(k,i)
         sd_awu(k)  =sd_awu(k)  +rho_int*DOWNA(k,i)*DOWNW(k,i)*DOWNU(k,i)
         sd_awv(k)  =sd_awv(k)  +rho_int*DOWNA(k,i)*DOWNW(k,i)*DOWNV(k,i)
      ENDDO
      sd_awqv(k) = sd_awqt(k)  - sd_awqc(k)
   ENDDO

END SUBROUTINE DDMF_JPL
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
!>\ingroup gsd_mynn_edmf
!! \author JAYMES- added 22 Apr 2015
!! This function calculates saturation vapor pressure.  Separate ice and liquid functions
!! are used (identical to those in module_mp_thompson.F, v3.6). Then, the
!! final returned value is a temperature-dependant "blend". Because the final
!! value is "phase-aware", this formulation may be preferred for use throughout
!! the module (replacing "svp").
  FUNCTION esat_blend(t) 

      IMPLICIT NONE
      
      REAL, INTENT(IN):: t
      REAL :: esat_blend,XC,ESL,ESI,chi

      XC=MAX(-80.,t - t0c) !note t0c = 273.15, tice is set in module mynn_common

! For 253 < t < 273.16 K, the vapor pressures are "blended" as a function of temperature, 
! using the approach of Chaboureau and Bechtold (2002), JAS, p. 2363.  The resulting 
! values are returned from the function.
      IF (t .GE. t0c) THEN
          esat_blend = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8))))))) 
      ELSE IF (t .LE. tice) THEN
          esat_blend = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
      ELSE
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          chi  = (t0c - t)/(t0c - tice)
          esat_blend = (1.-chi)*ESL  + chi*ESI
      END IF

  END FUNCTION esat_blend

! ====================================================================

!>\ingroup gsd_mynn_edmf
!! This function extends function "esat" and returns a "blended"
!! saturation mixing ratio.
!!\author JAYMES
  FUNCTION qsat_blend(t, P, waterice)

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

      XC=MAX(-80.,t - t0c)

      IF ((t .GE. t0c) .OR. (wrt .EQ. 'w')) THEN
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8))))))) 
          qsat_blend = 0.622*ESL/max(P-ESL, 1e-5) 
!      ELSE IF (t .LE. 253.) THEN
      ELSE IF (t .LE. tice) THEN
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          qsat_blend = 0.622*ESI/max(P-ESI, 1e-5)
      ELSE
          ESL  = J0+XC*(J1+XC*(J2+XC*(J3+XC*(J4+XC*(J5+XC*(J6+XC*(J7+XC*J8)))))))
          ESI  = K0+XC*(K1+XC*(K2+XC*(K3+XC*(K4+XC*(K5+XC*(K6+XC*(K7+XC*K8)))))))
          RSLF = 0.622*ESL/max(P-ESL, 1e-5)
          RSIF = 0.622*ESI/max(P-ESI, 1e-5)
!          chi  = (273.16-t)/20.16
          chi  = (t0c - t)/(t0c - tice) 
         qsat_blend = (1.-chi)*RSLF + chi*RSIF
      END IF

  END FUNCTION qsat_blend

! ===================================================================

!>\ingroup gsd_mynn_edmf
!! This function interpolates the latent heats of vaporization and sublimation into
!! a single, temperature-dependent, "blended" value, following 
!! Chaboureau and Bechtold (2002) \cite Chaboureau_2002, Appendix.
!!\author JAYMES
  FUNCTION xl_blend(t)

      IMPLICIT NONE

      REAL, INTENT(IN):: t
      REAL :: xl_blend,xlvt,xlst,chi
      !note: t0c = 273.15, tice is set in mynn_common

      IF (t .GE. t0c) THEN
          xl_blend = xlv + (cpv-cliq)*(t-t0c)  !vaporization/condensation
      ELSE IF (t .LE. tice) THEN
          xl_blend = xls + (cpv-cice)*(t-t0c)  !sublimation/deposition
      ELSE
          xlvt = xlv + (cpv-cliq)*(t-t0c)  !vaporization/condensation
          xlst = xls + (cpv-cice)*(t-t0c)  !sublimation/deposition
!          chi  = (273.16-t)/20.16
          chi  = (t0c - t)/(t0c - tice)
          xl_blend = (1.-chi)*xlvt + chi*xlst     !blended
      END IF

  END FUNCTION xl_blend

! ===================================================================

  FUNCTION phim(zet)
     ! New stability function parameters for momentum (Puhales, 2020, WRF 4.2.1)
     ! The forms in unstable conditions (z/L < 0) use Grachev et al. (2000), which are a blend of 
     ! the classical (Kansas) forms (i.e., Paulson 1970, Dyer and Hicks 1970), valid for weakly 
     ! unstable conditions (-1 < z/L < 0). The stability functions for stable conditions use an
     ! updated form taken from Cheng and Brutsaert (2005), which extends the validity into very
     ! stable conditions [z/L ~ O(10)].
      IMPLICIT NONE

      REAL, INTENT(IN):: zet
      REAL :: dummy_0,dummy_1,dummy_11,dummy_2,dummy_22,dummy_3,dummy_33,dummy_4,dummy_44,dummy_psi
      REAL, PARAMETER :: am_st=6.1, bm_st=2.5, rbm_st=1./bm_st
      REAL, PARAMETER :: ah_st=5.3, bh_st=1.1, rbh_st=1./bh_st
      REAL, PARAMETER :: am_unst=10., ah_unst=34.
      REAL :: phi_m,phim

      if ( zet >= 0.0 ) then
         dummy_0=1+zet**bm_st
         dummy_1=zet+dummy_0**(rbm_st)
         dummy_11=1+dummy_0**(rbm_st-1)*zet**(bm_st-1)
         dummy_2=(-am_st/dummy_1)*dummy_11
         phi_m = 1-zet*dummy_2
      else
         dummy_0 = (1.0-cphm_unst*zet)**0.25
         phi_m = 1./dummy_0
         dummy_psi = 2.*log(0.5*(1.+dummy_0))+log(0.5*(1.+dummy_0**2))-2.*atan(dummy_0)+1.570796

         dummy_0=(1.-am_unst*zet)          ! parentesis arg
         dummy_1=dummy_0**0.333333         ! y
         dummy_11=-0.33333*am_unst*dummy_0**(-0.6666667) ! dy/dzet
         dummy_2 = 0.33333*(dummy_1**2.+dummy_1+1.)      ! f
         dummy_22 = 0.3333*dummy_11*(2.*dummy_1+1.)      ! df/dzet
         dummy_3 = 0.57735*(2.*dummy_1+1.) ! g
         dummy_33 = 1.1547*dummy_11        ! dg/dzet
         dummy_4 = 1.5*log(dummy_2)-1.73205*atan(dummy_3)+1.813799364 !psic
         dummy_44 = (1.5/dummy_2)*dummy_22-1.73205*dummy_33/(1.+dummy_3**2)! dpsic/dzet

         dummy_0 = zet**2
         dummy_1 = 1./(1.+dummy_0) ! denon
         dummy_11 = 2.*zet         ! denon/dzet
         dummy_2 = ((1-phi_m)/zet+dummy_11*dummy_4+dummy_0*dummy_44)*dummy_1
         dummy_22 = -dummy_11*(dummy_psi+dummy_0*dummy_4)*dummy_1**2

         phi_m = 1.-zet*(dummy_2+dummy_22)
      end if

      !phim = phi_m - zet
      phim = phi_m

  END FUNCTION phim
! ===================================================================

  FUNCTION phih(zet)
    ! New stability function parameters for heat (Puhales, 2020, WRF 4.2.1)
    ! The forms in unstable conditions (z/L < 0) use Grachev et al. (2000), which are a blend of
    ! the classical (Kansas) forms (i.e., Paulson 1970, Dyer and Hicks 1970), valid for weakly
    ! unstable conditions (-1 < z/L < 0). The stability functions for stable conditions use an
    ! updated form taken from Cheng and Brutsaert (2005), which extends the validity into very
    ! stable conditions [z/L ~ O(10)].
      IMPLICIT NONE

      REAL, INTENT(IN):: zet
      REAL :: dummy_0,dummy_1,dummy_11,dummy_2,dummy_22,dummy_3,dummy_33,dummy_4,dummy_44,dummy_psi
      REAL, PARAMETER :: am_st=6.1, bm_st=2.5, rbm_st=1./bm_st
      REAL, PARAMETER :: ah_st=5.3, bh_st=1.1, rbh_st=1./bh_st
      REAL, PARAMETER :: am_unst=10., ah_unst=34.
      REAL :: phh,phih

      if ( zet >= 0.0 ) then
         dummy_0=1+zet**bh_st
         dummy_1=zet+dummy_0**(rbh_st)
         dummy_11=1+dummy_0**(rbh_st-1)*zet**(bh_st-1)
         dummy_2=(-ah_st/dummy_1)*dummy_11
         phih = 1-zet*dummy_2
      else
         dummy_0 = (1.0-cphh_unst*zet)**0.5
         phh = 1./dummy_0
         dummy_psi = 2.*log(0.5*(1.+dummy_0))

         dummy_0=(1.-ah_unst*zet)          ! parentesis arg
         dummy_1=dummy_0**0.333333         ! y
         dummy_11=-0.33333*ah_unst*dummy_0**(-0.6666667) ! dy/dzet
         dummy_2 = 0.33333*(dummy_1**2.+dummy_1+1.)      ! f
         dummy_22 = 0.3333*dummy_11*(2.*dummy_1+1.)      ! df/dzet
         dummy_3 = 0.57735*(2.*dummy_1+1.) ! g
         dummy_33 = 1.1547*dummy_11        ! dg/dzet
         dummy_4 = 1.5*log(dummy_2)-1.73205*atan(dummy_3)+1.813799364 !psic
         dummy_44 = (1.5/dummy_2)*dummy_22-1.73205*dummy_33/(1.+dummy_3**2)! dpsic/dzet

         dummy_0 = zet**2
         dummy_1 = 1./(1.+dummy_0)         ! denon
         dummy_11 = 2.*zet                 ! ddenon/dzet
         dummy_2 = ((1-phh)/zet+dummy_11*dummy_4+dummy_0*dummy_44)*dummy_1
         dummy_22 = -dummy_11*(dummy_psi+dummy_0*dummy_4)*dummy_1**2

         phih = 1.-zet*(dummy_2+dummy_22)
      end if

END FUNCTION phih
! ==================================================================
 SUBROUTINE topdown_cloudrad(kts,kte,dz1,zw,xland,kpbl,PBLH,  &
               &sqc,sqi,sqw,thl,th1,ex1,p1,rho1,thetav,       &
               &cldfra_bl1D,rthraten,                         &
               &maxKHtopdown,KHtopdown,TKEprodTD              )

    !input
    integer, intent(in) :: kte,kts
    real, dimension(kts:kte), intent(in) :: dz1,sqc,sqi,sqw,&
          thl,th1,ex1,p1,rho1,thetav,cldfra_bl1D,rthraten
    real, dimension(kts:kte+1), intent(in) :: zw
    real, intent(in) :: pblh,xland
    integer,intent(in) :: kpbl
    !output
    real, intent(out) :: maxKHtopdown
    real, dimension(kts:kte), intent(out) :: KHtopdown,TKEprodTD
    !local
    real, dimension(kts:kte) :: zfac,wscalek2,zfacent
    real :: bfx0,sflux,wm2,wm3,h1,h2,bfxpbl,dthvx,tmp1
    real :: temps,templ,zl1,wstar3_2
    real :: ent_eff,radsum,radflux,we,rcldb,rvls,minrad,zminrad
    real, parameter :: pfac =2.0, zfmin = 0.01, phifac=8.0
    integer :: k,kk,kminrad
    logical :: cloudflg

    cloudflg=.false.
    minrad=100.
    kminrad=kpbl
    zminrad=PBLH
    KHtopdown(kts:kte)=0.0
    TKEprodTD(kts:kte)=0.0
    maxKHtopdown=0.0

    !CHECK FOR STRATOCUMULUS-TOPPED BOUNDARY LAYERS
    DO kk = MAX(1,kpbl-2),kpbl+3
       if (sqc(kk).gt. 1.e-6 .OR. sqi(kk).gt. 1.e-6 .OR. &
           cldfra_bl1D(kk).gt.0.5) then
          cloudflg=.true.
       endif
       if (rthraten(kk) < minrad)then
          minrad=rthraten(kk)
          kminrad=kk
          zminrad=zw(kk) + 0.5*dz1(kk)
       endif
    ENDDO

    IF (MAX(kminrad,kpbl) < 2)cloudflg = .false.
    IF (cloudflg) THEN
       zl1 = dz1(kts)
       k = MAX(kpbl-1, kminrad-1)
       !Best estimate of height of TKE source (top of downdrafts):
       !zminrad = 0.5*pblh(i) + 0.5*zminrad

       templ=thl(k)*ex1(k)
       !rvls is ws at full level
       rvls=100.*6.112*EXP(17.67*(templ-273.16)/(templ-29.65))*(ep_2/p1(k+1))
       temps=templ + (sqw(k)-rvls)/(cp/xlv  +  ep_2*xlv*rvls/(r_d*templ**2))
       rvls=100.*6.112*EXP(17.67*(temps-273.15)/(temps-29.65))*(ep_2/p1(k+1))
       rcldb=max(sqw(k)-rvls,0.)

       !entrainment efficiency
       dthvx     = (thl(k+2) + th1(k+2)*p608*sqw(k+2)) &
                 - (thl(k)   + th1(k)  *p608*sqw(k))
       dthvx     = max(dthvx,0.1)
       tmp1      = xlvcp * rcldb/(ex1(k)*dthvx)
       !Originally from Nichols and Turton (1986), where a2 = 60, but lowered
       !here to 8, as in Grenier and Bretherton (2001).
       ent_eff   = 0.2 + 0.2*8.*tmp1

       radsum=0.
       DO kk = MAX(1,kpbl-3),kpbl+3
          radflux=rthraten(kk)*ex1(kk)         !converts theta/s to temp/s
          radflux=radflux*cp/grav*(p1(kk)-p1(kk+1)) ! converts temp/s to W/m^2
          if (radflux < 0.0 ) radsum=abs(radflux)+radsum
       ENDDO

       !More strict limits over land to reduce stable-layer mixouts
       if ((xland-1.5).GE.0)THEN      ! WATER
          radsum=MIN(radsum,90.0)
          bfx0 = max(radsum/rho1(k)/cp,0.)
       else                           ! LAND
          radsum=MIN(0.25*radsum,30.0)!practically turn off over land
          bfx0 = max(radsum/rho1(k)/cp - max(sflux,0.0),0.)
       endif

       !entrainment from PBL top thermals
       wm3    = grav/thetav(k)*bfx0*MIN(pblh,1500.) ! this is wstar3(i)
       wm2    = wm2 + wm3**h2
       bfxpbl = - ent_eff * bfx0
       dthvx  = max(thetav(k+1)-thetav(k),0.1)
       we     = max(bfxpbl/dthvx,-sqrt(wm3**h2))

       DO kk = kts,kpbl+3
          !Analytic vertical profile
          zfac(kk) = min(max((1.-(zw(kk+1)-zl1)/(zminrad-zl1)),zfmin),1.)
          zfacent(kk) = 10.*MAX((zminrad-zw(kk+1))/zminrad,0.0)*(1.-zfac(kk))**3

          !Calculate an eddy diffusivity profile (not used at the moment)
          wscalek2(kk) = (phifac*karman*wm3*(zfac(kk)))**h1
          !Modify shape of Kh to be similar to Lock et al (2000): use pfac = 3.0
          KHtopdown(kk) = wscalek2(kk)*karman*(zminrad-zw(kk+1))*(1.-zfac(kk))**3 !pfac
          KHtopdown(kk) = MAX(KHtopdown(kk),0.0)

          !Calculate TKE production = 2(g/TH)(w'TH'), where w'TH' = A(TH/g)wstar^3/PBLH,
          !A = ent_eff, and wstar is associated with the radiative cooling at top of PBL.
          !An analytic profile controls the magnitude of this TKE prod in the vertical.
          TKEprodTD(kk)=2.*ent_eff*wm3/MAX(pblh,100.)*zfacent(kk)
          TKEprodTD(kk)= MAX(TKEprodTD(kk),0.0)
       ENDDO
    ENDIF !end cloud check
    maxKHtopdown=MAXVAL(KHtopdown(:))

 END SUBROUTINE topdown_cloudrad
! ==================================================================
! ===================================================================
! ===================================================================

END MODULE module_bl_mynn
