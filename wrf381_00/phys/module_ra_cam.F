MODULE module_ra_cam
  use module_ra_cam_support
  use module_cam_support, only: endrun

  implicit none
! 
! A. Slingo's data for cloud particle radiative properties (from 'A GCM
! Parameterization for the Shortwave Properties of Water Clouds' JAS
! vol. 46 may 1989 pp 1419-1427)
! 
   real(r8) abarl(4)         ! A coefficient for extinction optical depth
   real(r8) bbarl(4)         ! B coefficient for extinction optical depth
   real(r8) cbarl(4)         ! C coefficient for single scat albedo
   real(r8) dbarl(4)         ! D coefficient for single  scat albedo
   real(r8) ebarl(4)         ! E coefficient for asymmetry parameter
   real(r8) fbarl(4)         ! F coefficient for asymmetry parameter

   save abarl, bbarl, cbarl, dbarl, ebarl, fbarl

   data abarl/ 2.817e-02, 2.682e-02,2.264e-02,1.281e-02/
   data bbarl/ 1.305    , 1.346    ,1.454    ,1.641    /
   data cbarl/-5.62e-08 ,-6.94e-06 ,4.64e-04 ,0.201    /
   data dbarl/ 1.63e-07 , 2.35e-05 ,1.24e-03 ,7.56e-03 /
   data ebarl/ 0.829    , 0.794    ,0.754    ,0.826    /
   data fbarl/ 2.482e-03, 4.226e-03,6.560e-03,4.353e-03/

#if 0
! moved and changed to local variables into radcswmx for thread-safety, JM 20100217
   real(r8) abarli           ! A coefficient for current spectral band
   real(r8) bbarli           ! B coefficient for current spectral band
   real(r8) cbarli           ! C coefficient for current spectral band
   real(r8) dbarli           ! D coefficient for current spectral band
   real(r8) ebarli           ! E coefficient for current spectral band
   real(r8) fbarli           ! F coefficient for current spectral band
#endif
! 
! Caution... A. Slingo recommends no less than 4.0 micro-meters nor
! greater than 20 micro-meters
! 
! ice water coefficients (Ebert and Curry,1992, JGR, 97, 3831-3836)
! 
   real(r8) abari(4)         ! a coefficient for extinction optical depth
   real(r8) bbari(4)         ! b coefficient for extinction optical depth
   real(r8) cbari(4)         ! c coefficient for single scat albedo
   real(r8) dbari(4)         ! d coefficient for single scat albedo
   real(r8) ebari(4)         ! e coefficient for asymmetry parameter
   real(r8) fbari(4)         ! f coefficient for asymmetry parameter

   save abari, bbari, cbari, dbari, ebari, fbari

   data abari/ 3.448e-03, 3.448e-03,3.448e-03,3.448e-03/
   data bbari/ 2.431    , 2.431    ,2.431    ,2.431    /
   data cbari/ 1.00e-05 , 1.10e-04 ,1.861e-02,.46658   /
   data dbari/ 0.0      , 1.405e-05,8.328e-04,2.05e-05 /
   data ebari/ 0.7661   , 0.7730   ,0.794    ,0.9595   /
   data fbari/ 5.851e-04, 5.665e-04,7.267e-04,1.076e-04/

#if 0
! moved and changed to local variables into radcswmx for thread-safety, JM 20100217
   real(r8) abarii           ! A coefficient for current spectral band
   real(r8) bbarii           ! B coefficient for current spectral band
   real(r8) cbarii           ! C coefficient for current spectral band
   real(r8) dbarii           ! D coefficient for current spectral band
   real(r8) ebarii           ! E coefficient for current spectral band
   real(r8) fbarii           ! F coefficient for current spectral band
#endif
! 
   real(r8) delta            ! Pressure (in atm) for stratos. h2o limit
   real(r8) o2mmr            ! O2 mass mixing ratio:

   save delta, o2mmr

!
! UPDATE TO H2O NEAR-IR: Delta optimized for Hitran 2K and CKD 2.4
!
   data delta / 0.0014257179260883 /
!
! END UPDATE
!
   data o2mmr / .23143 /

! Next series depends on spectral interval
! 
   real(r8) frcsol(nspint)   ! Fraction of solar flux in spectral interval
   real(r8) wavmin(nspint)   ! Min wavelength (micro-meters) of interval
   real(r8) wavmax(nspint)   ! Max wavelength (micro-meters) of interval
   real(r8) raytau(nspint)   ! Rayleigh scattering optical depth
   real(r8) abh2o(nspint)    ! Absorption coefficiant for h2o (cm2/g)
   real(r8) abo3 (nspint)    ! Absorption coefficiant for o3  (cm2/g)
   real(r8) abco2(nspint)    ! Absorption coefficiant for co2 (cm2/g)
   real(r8) abo2 (nspint)    ! Absorption coefficiant for o2  (cm2/g)
   real(r8) ph2o(nspint)     ! Weight of h2o in spectral interval
   real(r8) pco2(nspint)     ! Weight of co2 in spectral interval
   real(r8) po2 (nspint)     ! Weight of o2  in spectral interval
   real(r8) nirwgt(nspint)   ! Spectral Weights to simulate Nimbus-7 filter
   save frcsol ,wavmin ,wavmax ,raytau ,abh2o ,abo3 , &
        abco2  ,abo2   ,ph2o   ,pco2   ,po2   ,nirwgt

   data frcsol / .001488, .001389, .001290, .001686, .002877, &
                 .003869, .026336, .360739, .065392, .526861, &
                 .526861, .526861, .526861, .526861, .526861, &
                 .526861, .006239, .001834, .001834/
! 
! weight for 0.64 - 0.7 microns  appropriate to clear skies over oceans
! 
   data nirwgt /  0.0,   0.0,   0.0,      0.0,   0.0, &
                  0.0,   0.0,   0.0, 0.320518,   1.0,  1.0, &
                  1.0,   1.0,   1.0,      1.0,   1.0, &
                  1.0,   1.0,   1.0 /

   data wavmin / .200,  .245,  .265,  .275,  .285, &
                 .295,  .305,  .350,  .640,  .700,  .701, &
                 .701,  .701,  .701,  .702,  .702, &
                 2.630, 4.160, 4.160/

   data wavmax / .245,  .265,  .275,  .285,  .295, &
                 .305,  .350,  .640,  .700, 5.000, 5.000, &
                 5.000, 5.000, 5.000, 5.000, 5.000, &
                 2.860, 4.550, 4.550/

!
! UPDATE TO H2O NEAR-IR: Rayleigh scattering optimized for Hitran 2K & CKD 2.4
!
   real(r8) v_raytau_35
   real(r8) v_raytau_64
   real(r8) v_abo3_35
   real(r8) v_abo3_64
   parameter( &
        v_raytau_35 = 0.155208, &
        v_raytau_64 = 0.0392, &
        v_abo3_35 = 2.4058030e+01, &  
        v_abo3_64 = 2.210e+01 &
        )

   data raytau / 4.020, 2.180, 1.700, 1.450, 1.250, &
                  1.085, 0.730, v_raytau_35, v_raytau_64, &
                  0.02899756, 0.01356763, 0.00537341, &
                  0.00228515, 0.00105028, 0.00046631, &
                  0.00025734, &
                 .0001, .0001, .0001/
!
! END UPDATE
!

! 
! Absorption coefficients
! 
!
! UPDATE TO H2O NEAR-IR: abh2o optimized for Hitran 2K and CKD 2.4
!
   data abh2o /    .000,     .000,    .000,    .000,    .000, &
                   .000,     .000,    .000,    .000,    &
                   0.00256608,  0.06310504,   0.42287445, 2.45397941, &
                  11.20070807, 47.66091389, 240.19010243, &
                   .000,    .000,    .000/
!
! END UPDATE
!

   data abo3  /5.370e+04, 13.080e+04,  9.292e+04, 4.530e+04, 1.616e+04, &
               4.441e+03,  1.775e+02, v_abo3_35, v_abo3_64,      .000, &
               .000,   .000    ,   .000   ,   .000   ,      .000, &
               .000,   .000    ,   .000   ,   .000    /

   data abco2  /   .000,     .000,    .000,    .000,    .000, &
                   .000,     .000,    .000,    .000,    .000, &
                   .000,     .000,    .000,    .000,    .000, &
                   .000,     .094,    .196,   1.963/

   data abo2  /    .000,     .000,    .000,    .000,    .000, &
                   .000,     .000,    .000,1.11e-05,6.69e-05, &
                   .000,     .000,    .000,    .000,    .000, &  
                   .000,     .000,    .000,    .000/
! 
! Spectral interval weights
! 
   data ph2o  /    .000,     .000,    .000,    .000,    .000, &
        .000,     .000,    .000,    .000,    .505,     &
        .210,     .120,    .070,    .048,    .029,     &
        .018,     .000,    .000,    .000/

   data pco2  /    .000,     .000,    .000,    .000,    .000, &
        .000,     .000,    .000,    .000,    .000,     &
        .000,     .000,    .000,    .000,    .000,     &
        .000,    1.000,    .640,    .360/

   data po2   /    .000,     .000,    .000,    .000,    .000, &
        .000,     .000,    .000,   1.000,   1.000,     &
        .000,     .000,    .000,    .000,    .000,     &
        .000,     .000,    .000,    .000/

   real(r8) amo                 ! Molecular weight of ozone (g/mol)
   save     amo

   data amo   /  48.0000   /

contains
subroutine camrad(RTHRATENLW,RTHRATENSW,                           &
                     dolw,dosw,                                    &
                     SWUPT,SWUPTC,SWDNT,SWDNTC,                    &
                     LWUPT,LWUPTC,LWDNT,LWDNTC,                    &
                     SWUPB,SWUPBC,SWDNB,SWDNBC,                    &
                     LWUPB,LWUPBC,LWDNB,LWDNBC,                    &
                     swcf,lwcf,olr,cemiss,taucldc,taucldi,coszr,   &
                     GSW,GLW,XLAT,XLONG,                           &
                     ALBEDO,t_phy,TSK,EMISS,                       &
                     QV3D,QC3D,QR3D,QI3D,QS3D,QG3D,                &
                     ALSWVISDIR,ALSWVISDIF,                        & !ssib  
                     ALSWNIRDIR,ALSWNIRDIF,                        & !ssib 
                     SWVISDIR,SWVISDIF,                            & !ssib 
                     SWNIRDIR,SWNIRDIF,                            & !ssib 
                     sf_surface_physics,                           & !ssib 
                     SWDDIR,SWDDIF,SWDDNI,                         & ! amontornes-bcodina (2014-04-20)
                     F_QV,F_QC,F_QR,F_QI,F_QS,F_QG,                &
                     f_ice_phy,f_rain_phy,                         &
                     p_phy,p8w,z,pi_phy,rho_phy,dz8w,               &
                     CLDFRA,XLAND,XICE,SNOW,                        &
                     ozmixm,pin0,levsiz,num_months,                 &
                     m_psp,m_psn,aerosolcp,aerosolcn,m_hybi0,       &
                     cam_abs_dim1, cam_abs_dim2,                    &
                     paerlev,naer_c,                                &
                     GMT,JULDAY,JULIAN,YR,DT,XTIME,DECLIN,SOLCON,         &
                     RADT,DEGRAD,n_cldadv,                                  &
                     abstot_3d, absnxt_3d, emstot_3d,              &
                     doabsems,                                     &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     coszen                                        )

!ccc To use CLWRF time-varying trace gases
   USE MODULE_RA_CLWRF_SUPPORT, ONLY : read_CAMgases
   USE module_wrf_error
   USE module_state_description, ONLY : SSIBSCHEME, CLMSCHEME          !ssib & clm

!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------

   INTEGER,    INTENT(IN   ) ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte
   LOGICAL,    INTENT(IN   ) ::        F_QV,F_QC,F_QR,F_QI,F_QS,F_QG
   LOGICAL,    INTENT(INout) ::        doabsems
   LOGICAL,    INTENT(IN   ) ::        dolw,dosw

   INTEGER,    INTENT(IN  )  ::        n_cldadv
   INTEGER,    INTENT(IN  )  ::        JULDAY
   REAL,       INTENT(IN  )  ::        JULIAN
   INTEGER,    INTENT(IN  )  ::        YR
   REAL,       INTENT(IN  )  ::        DT
   INTEGER,      INTENT(IN   )    ::   levsiz, num_months
   INTEGER,      INTENT(IN   )    ::   paerlev, naer_c
   INTEGER,      INTENT(IN   )    ::   cam_abs_dim1, cam_abs_dim2


   REAL, INTENT(IN    )      ::        RADT,DEGRAD,             &
                                       XTIME,DECLIN,SOLCON,GMT
!
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN    ) ::                                   P_PHY, &
                                                           P8W, &
                                                             Z, &
                                                            pi_PHY, &
                                                           rho_PHY, &
                                                              dz8w, &
                                                             T_PHY, &
                                                            QV3D, &
                                                            QC3D, &
                                                            QR3D, &
                                                            QI3D, &
                                                            QS3D, &
                                                            QG3D, &
                                                        CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATENLW, &
                                                        RTHRATENSW
!
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                           XLAND, &
                                                           XICE, &
                                                           SNOW, &
                                                           EMISS, &
                                                             TSK, &
                                                             ALBEDO

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months ),      &
          INTENT(IN   ) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), INTENT(IN )  ::                   PIN0

   REAL,  DIMENSION(ims:ime,jms:jme), INTENT(IN )  ::      m_psp,m_psn
   REAL,  DIMENSION(paerlev), intent(in)             ::      m_hybi0
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, naer_c ),      &
          INTENT(IN   ) ::                    aerosolcp, aerosolcn

!
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                   GSW, GLW

!---------SSiB variables (fds 06/2010)----------------
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN)     ::                            ALSWVISDIR, &
                                                      ALSWVISDIF, &
                                                      ALSWNIRDIR, &
                                                      ALSWNIRDIF

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(OUT)    ::                              SWVISDIR, &
                                                        SWVISDIF, &
                                                        SWNIRDIR, &
                                                        SWNIRDIF, &
							                            SWDDIR,   &
                                                        SWDDNI,   &
                                                        SWDDIF

   INTEGER, INTENT(IN) :: sf_surface_physics
!--------------------------------------
! saving arrays for doabsems reduction of radiation calcs

   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim2 , jms:jme ),           &
         INTENT(INOUT)  ::                                  abstot_3d
   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim1 , jms:jme ),           &
         INTENT(INOUT)  ::                                  absnxt_3d
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),           &
         INTENT(INOUT)  ::                                  emstot_3d


! Added outputs of total and clearsky fluxes etc
! Note that k=1 refers to the half level below the model lowest level (Sfc)
!           k=kme refers to the half level above the model highest level (TOA)
!
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                &
!         INTENT(INOUT)  ::                                  swup, &
!                                                       swupclear, &
!                                                            swdn, &
!                                                       swdnclear, &
!                                                            lwup, &
!                                                       lwupclear, &
!                                                            lwdn, &
!                                                       lwdnclear

   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                    SWUPT,SWUPTC,SWDNT,SWDNTC,                    &
                    LWUPT,LWUPTC,LWDNT,LWDNTC,                    &
                    SWUPB,SWUPBC,SWDNB,SWDNBC,                    &
                    LWUPB,LWUPBC,LWDNB,LWDNBC

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                  swcf, &
                                                            lwcf, &
                                                             olr, &
                                                            coszr    
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme )                 , &
         INTENT(OUT   )  ::                               cemiss, &        ! cloud emissivity for isccp
                                                         taucldc, &        ! cloud water optical depth for isccp
                                                         taucldi           ! cloud ice optical depth for isccp
!
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         INTENT(IN   ) ::                                            &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY

  real, dimension(ims:ime,jms:jme), optional, intent(in) :: coszen

! LOCAL VARIABLES
 
   INTEGER :: lchnk, ncol, pcols, pver, pverp, pverr, pverrp
   INTEGER :: pcnst, pnats, ppcnst, i, j, k, ii, kk, kk1, m, n
   integer :: begchunk, endchunk
   integer :: nyrm, nyrp
   real(r8) doymodel, doydatam, doydatap, deltat, fact1, fact2

   REAL :: XT24, TLOCTM, HRANG, XXLAT, oldXT24
 
   real(r8), DIMENSION( 1:ite-its+1 ) :: coszrs, landfrac, landm, snowh, icefrac, lwups
   real(r8), DIMENSION( 1:ite-its+1 ) :: asdir, asdif, aldir, aldif, ps
   real(r8), DIMENSION( 1:ite-its+1, 1:kte-kts+1 ) :: cld, pmid, lnpmid, pdel, zm, t
   real(r8), DIMENSION( 1:ite-its+1, 1:kte-kts+2 ) ::  pint, lnpint
   real(r8), DIMENSION( 1:ite-its+1, 1:kte-kts+1, n_cldadv) :: q
!   real(r8), DIMENSION( 1:kte-kts+1 ) :: hypm       ! reference pressures at midpoints
!   real(r8), DIMENSION( 1:kte-kts+2 ) :: hypi       ! reference pressures at interfaces
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: cicewp      ! in-cloud cloud ice water path
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: cliqwp      ! in-cloud cloud liquid water path
    real(r8), dimension(  1:ite-its+1, 0:kte-kts+1 ) :: tauxcl      ! cloud water optical depth
    real(r8), dimension(  1:ite-its+1, 0:kte-kts+1 ) :: tauxci      ! cloud ice optical depth
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: emis        ! cloud emissivity
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: rel         ! effective drop radius (microns)
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: rei         ! ice effective drop size (microns)
    real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 ) :: pmxrgn      ! Maximum values of pressure for each
    integer , dimension(  1:ite-its+1 ) :: nmxrgn               ! Number of maximally overlapped regions

   real(r8), dimension(  1:ite-its+1 ) :: fsns          ! Surface absorbed solar flux
   real(r8), dimension(  1:ite-its+1 ) :: fsnt          ! Net column abs solar flux at model top
   real(r8), dimension(  1:ite-its+1 ) :: flns          ! Srf longwave cooling (up-down) flux
   real(r8), dimension(  1:ite-its+1 ) :: flnt          ! Net outgoing lw flux at model top
! Added outputs of total and clearsky fluxes etc
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsup        ! Upward total sky solar
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsupc       ! Upward clear sky solar
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsdn        ! Downward total sky solar
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsdnc       ! Downward clear sky solar
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsdndir     ! Direct Downward total sky solar amontornes-bcodina (2014-04-20)
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsdncdir    ! Direct Downward clear sky solar amontornes-bcodina (2014-04-20)
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsdndif     ! Diffuse Downward total sky solar amontornes-bcodina (2014-04-20)
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fsdncdif    ! Diffuse Downward clear sky solar amontornes-bcodina (2014-04-20)
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: flup        ! Upward total sky longwave
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: flupc       ! Upward clear sky longwave
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fldn        ! Downward total sky longwave
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+2 )  :: fldnc       ! Downward clear sky longwave
   real(r8), dimension(  1:ite-its+1 ) :: swcftoa                 ! Top of the atmosphere solar cloud forcing
   real(r8), dimension(  1:ite-its+1 ) :: lwcftoa                 ! Top of the atmosphere longwave cloud forcing
   real(r8), dimension(  1:ite-its+1 ) :: olrtoa                  ! Top of the atmosphere outgoing longwave 
!
   real(r8), dimension(  1:ite-its+1 ) :: sols          ! Downward solar rad onto surface (sw direct)
   real(r8), dimension(  1:ite-its+1 ) :: soll          ! Downward solar rad onto surface (lw direct)
   real(r8), dimension(  1:ite-its+1 ) :: solsd         ! Downward solar rad onto surface (sw diffuse)
   real(r8), dimension(  1:ite-its+1 ) :: solld         ! Downward solar rad onto surface (lw diffuse)
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: qrs      ! Solar heating rate
   real(r8), dimension(  1:ite-its+1 ) :: fsds          ! Flux Shortwave Downwelling Surface
   real(r8), dimension(  1:ite-its+1 ) :: fsdsdir       ! Flux Shortwave Direct Downwelling Surface
   real(r8), dimension(  1:ite-its+1 ) :: fsdsdif       ! Flux Shortwave Diffuse Downwelling Surface
   real(r8), dimension(  1:ite-its+1, 1:kte-kts+1 ) :: qrl      ! Longwave cooling rate
   real(r8), dimension(  1:ite-its+1 ) :: flwds          ! Surface down longwave flux
   real(r8), dimension(  1:ite-its+1, levsiz, num_months ) :: ozmixmj        ! monthly ozone mixing ratio
   real(r8), dimension(  1:ite-its+1, levsiz ) :: ozmix          ! ozone mixing ratio (time interpolated)
   real(r8), dimension(levsiz)         :: pin            ! ozone pressure level
   real(r8), dimension(1:ite-its+1)    :: m_psjp,m_psjn          ! MATCH surface pressure
   real(r8), dimension(  1:ite-its+1, paerlev, naer_c ) :: aerosoljp        ! monthly aerosol concentrations
   real(r8), dimension(  1:ite-its+1, paerlev, naer_c ) :: aerosoljn        ! monthly aerosol concentrations
   real(r8), dimension(paerlev)                           :: m_hybi
   real(r8), dimension(1:ite-its+1 )          :: clat           ! latitude in radians for columns
   real(r8), dimension(its:ite,kts:kte+1,kts:kte+1) :: abstot ! Total absorptivity
   real(r8), dimension(its:ite,kts:kte,4)           :: absnxt ! Total nearest layer absorptivity
   real(r8), dimension(its:ite,kts:kte+1)           :: emstot ! Total emissivity
   CHARACTER(LEN=256) :: msgstr

!ccc
#ifdef CLWRFGHG
! CLWRF-UC June.09 
   REAL(r8)                                         :: co2vmr, n2ovmr, ch4vmr, f11vmr, f12vmr
#endif
   LOGICAL, EXTERNAL                                :: wrf_dm_on_monitor
   CHARACTER(LEN=256)                               :: message

!ccc

#if !defined(MAC_KLUDGE)
   lchnk = 1
   begchunk = ims
   endchunk = ime
   ncol = ite - its + 1
   pcols= ite - its + 1
   pver = kte - kts + 1
   pverp= pver + 1
   pverr = kte - kts + 1
   pverrp= pverr + 1
! number of advected constituents and non-advected constituents (including water vapor)
   ppcnst = n_cldadv
! number of non-advected constituents
   pnats = 0
   pcnst = ppcnst-pnats

! check the # species defined for the input climatology and naer

!  if(naer_c.ne.naer) then
!            WRITE( wrf_err_message , * ) 'naer_c ne naer ', naer_c, naer
   if(naer_c.ne.naer_all) then
             WRITE( wrf_err_message , * ) 'naer_c-1 ne naer_all ', naer_c, naer_all
             CALL wrf_error_fatal ( wrf_err_message )
   endif 

! update CO2 volume mixing ratio (co2vmr)
  
! determine time interpolation factors, check sanity
! of interpolation factors to within 32-bit roundoff
! assume that day of year is 1 for all input data
!
!ccc
#ifdef CLWRFGHG
! CLWRF-UC June.09  (Copy from share/wrf_tsin.F)

   CALL read_CAMgases(yr,julian,"CAM",co2vmr,n2ovmr,ch4vmr,f11vmr,f12vmr)

   IF ( wrf_dm_on_monitor() ) THEN
     WRITE(message,*)'write 1 CAM-CLWRF interpolated values______ year:',yr,' julian day:',julian
     call wrf_debug( 100, message)
     WRITE(message,*)'  CAM-CLWRF co2vmr: ',co2vmr,' n2ovmr:',n2ovmr,' ch4vmr:',ch4vmr,' cfc11:'&
       ,f11vmr,' cfc12:',f12vmr
     call wrf_debug( 100, message)
   ENDIF

#else
!ccc
   nyrm     = yr - yrdata(1) + 1
   nyrp     = nyrm + 1
   doymodel = yr*365.    + julian
   doydatam = yrdata(nyrm)*365. + 1.
   doydatap = yrdata(nyrp)*365. + 1.
   deltat   = doydatap - doydatam
   fact1    = (doydatap - doymodel)/deltat
   fact2    = (doymodel - doydatam)/deltat
   co2vmr = (co2(nyrm)*fact1 + co2(nyrp)*fact2)*1.e-06
!ccc
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE(message,*)'CAM interpolated values_____ year:',yr,' julian day:',julian
     call wrf_debug( 100, message)
     WRITE(message,*)'CAM co2vmr: ',co2vmr,' n2ovmr:',n2ovmr,' ch4vmr:',ch4vmr,' cfc11:',f11vmr,&
       ' cfc12:',f12vmr
     call wrf_debug( 100, message)
   ENDIF
#endif
!ccc

   co2mmr=co2vmr*mwco2/mwdry
!
!===================================================
! Radiation computations
!===================================================

      do k=1,levsiz
      pin(k)=pin0(k)
      enddo

      do k=1,paerlev
      m_hybi(k)=m_hybi0(k)
      enddo

! check for uninitialized arrays
      if(abstot_3d(its,kts,kts,jts) .eq. 0.0 .and. .not.doabsems .and. dolw)then
        CALL wrf_debug(0, 'camrad lw: CAUTION: re-calculating abstot, absnxt, emstot on restart')
        doabsems = .true.
      endif

   do j =jts,jte

!
! Cosine solar zenith angle for current time step
!

!  call zenith (calday, clat, clon, coszrs, ncol)

      if (present(coszen)) then
         do i=its,ite
            ii=i-its+1
            clat(ii)=XLAT(I,J)*DEGRAD
            coszrs(ii)=coszen(i,j)
         enddo
      else
         do i = its,ite
            ii = i - its + 1
            ! XT24 is the fractional part of simulation days plus half of RADT expressed in 
            ! units of minutes
            ! JULIAN is in days
            ! RADT is in minutes
            XT24=MOD(XTIME+RADT*0.5,1440.)
            TLOCTM=GMT+XT24/60.+XLONG(I,J)/15.
            HRANG=15.*(TLOCTM-12.)*DEGRAD
            XXLAT=XLAT(I,J)*DEGRAD
            clat(ii)=xxlat
            coszrs(II)=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)
         enddo
      end if

! moist variables

      do k = kts,kte
      kk = kte - k + kts 
      do i = its,ite
      ii = i - its + 1
!    convert to specific humidity
      q(ii,kk,1) = max(1.e-10,qv3d(i,k,j)/(1.+qv3d(i,k,j)))
     IF ( F_QI .and. F_QC .and. F_QS ) THEN
      q(ii,kk,ixcldliq) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j)))
      q(ii,kk,ixcldice) = max(0.,(qi3d(i,k,j)+qs3d(i,k,j))/(1.+qv3d(i,k,j)))
     ELSE IF ( F_QC .and. F_QR ) THEN
! Warm rain or simple ice
      q(ii,kk,ixcldliq) = 0.
      q(ii,kk,ixcldice) = 0.
      if(t_phy(i,k,j).gt.273.15)q(ii,kk,ixcldliq) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j)))
      if(t_phy(i,k,j).le.273.15)q(ii,kk,ixcldice) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j)))
     ELSE IF ( F_QC .and. F_QS ) THEN
! For Ferrier (note that currently Ferrier has QI, so this section will not be used)
      q(ii,kk,ixcldice) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j))*f_ice_phy(i,k,j))
      q(ii,kk,ixcldliq) = max(0.,qc3d(i,k,j)/(1.+qv3d(i,k,j))*(1.-f_ice_phy(i,k,j))*(1.-f_rain_phy(i,k,j)))
     ELSE
      q(ii,kk,ixcldliq) = 0.
      q(ii,kk,ixcldice) = 0.
     ENDIF
      cld(ii,kk) = CLDFRA(I,K,J)
      enddo
      enddo

      do i = its,ite
      ii = i - its + 1
      landfrac(ii) = 2.-XLAND(I,J)
      landm(ii) = landfrac(ii)
      snowh(ii) = 0.001*SNOW(I,J)
      icefrac(ii) = XICE(I,J)
      enddo

      do m=1,num_months-1
      do k=1,levsiz
      do i = its,ite
      ii = i - its + 1
      ozmixmj(ii,k,m) = ozmixm(i,k,j,m+1)
      enddo
      enddo
      enddo

      do i = its,ite
      ii = i - its + 1
      m_psjp(ii) = m_psp(i,j)
      m_psjn(ii) = m_psn(i,j)
      enddo

      do n=1,naer_c
      do k=1,paerlev
      do i = its,ite
      ii = i - its + 1
      aerosoljp(ii,k,n) = aerosolcp(i,k,j,n)
      aerosoljn(ii,k,n) = aerosolcn(i,k,j,n)
      enddo
      enddo
      enddo

!
! Complete radiation calculations
!
      do i = its,ite
      ii = i - its + 1
      lwups(ii) = stebol*EMISS(I,J)*TSK(I,J)**4
      enddo

      do k = kts,kte+1
      kk = kte - k + kts + 1
      do i = its,ite
      ii = i - its + 1
      pint(ii,kk) = p8w(i,k,j)
      if(k.eq.kts)ps(ii)=pint(ii,kk)
      lnpint(ii,kk) = log(pint(ii,kk))
      enddo
      enddo

      if(.not.doabsems .and. dolw)then
!      do kk = kts,kte+1
      do kk = 1,cam_abs_dim2
        do kk1 = kts,kte+1
          do i = its,ite
            abstot(i,kk1,kk) = abstot_3d(i,kk1,kk,j)
          enddo
        enddo
      enddo
!      do kk = 1,4
      do kk = 1,cam_abs_dim1
        do kk1 = kts,kte
          do i = its,ite
            absnxt(i,kk1,kk) = absnxt_3d(i,kk1,kk,j)
          enddo
        enddo
      enddo
      do kk = kts,kte+1
          do i = its,ite
            emstot(i,kk) = emstot_3d(i,kk,j)
          enddo
      enddo
      endif

      do k = kts,kte
      kk = kte - k + kts 
      do i = its,ite
      ii = i - its + 1
      pmid(ii,kk) = p_phy(i,k,j)
      lnpmid(ii,kk) = log(pmid(ii,kk))
      lnpint(ii,kk) = log(pint(ii,kk))
      pdel(ii,kk) = pint(ii,kk+1) - pint(ii,kk)
      t(ii,kk) = t_phy(i,k,j)
      zm(ii,kk) = z(i,k,j)
      enddo
      enddo


! Compute cloud water/ice paths and optical properties for input to radiation

      call param_cldoptics_calc(ncol, pcols, pver, pverp, pverr, pverrp, ppcnst, q, cld, landfrac, landm,icefrac, &
                                pdel, t, ps, pmid, pint, cicewp, cliqwp, emis, rel, rei, pmxrgn, nmxrgn, snowh)

!-----fds (06/2010)----------------------------
   SELECT CASE(sf_surface_physics)
   CASE (SSIBSCHEME)
   if (xtime .gt. 1.0) then
!      call wrf_message("using SSiB albedoes for land points")
      do i = its,ite
         ii = i - its + 1
         if (xland(i,j).lt.1.5) then   !land points only
           asdir(ii) = ALSWVISDIR(i,j) ! SSiB visdir albedo
           asdif(ii) = ALSWVISDIF(i,j) ! SSiB visdif albedo
           aldir(ii) = ALSWNIRDIR(i,j) ! SSiB nirdir albedo
           aldif(ii) = ALSWNIRDIF(i,j) ! SSiB nirdif albedo
         else
           asdir(ii) = albedo(i,j)
           asdif(ii) = albedo(i,j)
           aldir(ii) = albedo(i,j)
           aldif(ii) = albedo(i,j)
         endif
      enddo
   else
      do i = its,ite
         ii = i - its + 1
         asdir(ii) = albedo(i,j)
         asdif(ii) = albedo(i,j)
         aldir(ii) = albedo(i,j)
         aldif(ii) = albedo(i,j)
      enddo
   endif
   CASE (CLMSCHEME)
   if (xtime .gt. 1.0) then
      do i = its,ite
         ii = i - its + 1
         if (xland(i,j).lt.1.5) then   !land points only
           asdir(ii) = ALSWVISDIR(i,j) ! CLM visdir albedo
           asdif(ii) = ALSWVISDIF(i,j) ! CLM visdif albedo
           aldir(ii) = ALSWNIRDIR(i,j) ! CLM nirdir albedo
           aldif(ii) = ALSWNIRDIF(i,j) ! CLM nirdif albedo
         else
           asdir(ii) = albedo(i,j)
           asdif(ii) = albedo(i,j)
           aldir(ii) = albedo(i,j)
           aldif(ii) = albedo(i,j)
         endif
      enddo
   else
      do i = its,ite
         ii = i - its + 1
         asdir(ii) = albedo(i,j)
         asdif(ii) = albedo(i,j)
         aldir(ii) = albedo(i,j)
         aldif(ii) = albedo(i,j)
      enddo
   endif

   CASE DEFAULT
      do i = its,ite
      ii = i - its + 1
! use same albedo for direct and diffuse
! change this when separate values are provided
      asdir(ii) = albedo(i,j)
      asdif(ii) = albedo(i,j)
      aldir(ii) = albedo(i,j)
      aldif(ii) = albedo(i,j)
      enddo
   END SELECT
!-----------------------------------------------

! WRF allocate space here (not needed if oznini is called)
!  allocate (ozmix(pcols,levsiz,begchunk:endchunk)) ! This line from oznini.F90

      call radctl (j,lchnk, ncol, pcols, pver, pverp, pverr, pverrp, ppcnst, pcnst, lwups, emis, pmid,             &
                   pint, lnpmid, lnpint, pdel, t, q,   &
                   cld, cicewp, cliqwp, tauxcl, tauxci, coszrs, clat, asdir, asdif,               &
                   aldir, aldif, solcon, GMT,JULDAY,JULIAN,DT,XTIME,   &
                   pin, ozmixmj, ozmix, levsiz, num_months,  & 
                   m_psjp,m_psjn, aerosoljp, aerosoljn,  m_hybi, paerlev, naer_c, pmxrgn, nmxrgn, &
                   dolw, dosw, doabsems, abstot, absnxt, emstot, &
                   fsup, fsupc, fsdn, fsdnc,            &
                   fsdndir ,fsdncdir,fsdndif ,fsdncdif, & ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes
                   flup, flupc, fldn, fldnc, swcftoa, lwcftoa, olrtoa,  &
                   fsns, fsnt    ,flns    ,flnt    , &
                   qrs, qrl, flwds, rel, rei,                       &
                   sols, soll, solsd, solld,                  &
!ccc
#ifdef CLWRFGHG
!CLWRF-UC June.09
                   n2ovmr, ch4vmr, f11vmr, f12vmr  , &
#endif
!ccc
                   landfrac, zm, fsds, fsdsdir, fsdsdif) ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes

      do k = kts,kte
      kk = kte - k + kts 
      do i = its,ite
      ii = i - its + 1
      if(dolw)RTHRATENLW(I,K,J) = 1.e4*qrl(ii,kk)/(cpair*pi_phy(i,k,j))
      if(dosw)RTHRATENSW(I,K,J) = 1.e4*qrs(ii,kk)/(cpair*pi_phy(i,k,j))
      cemiss(i,k,j)     = emis(ii,kk)
      taucldc(i,k,j)    = tauxcl(ii,kk)
      taucldi(i,k,j)    = tauxci(ii,kk)
      enddo
      enddo

      if(doabsems .and. dolw)then
!      do kk = kts,kte+1
      do kk = 1,cam_abs_dim2
        do kk1 = kts,kte+1
          do i = its,ite
            abstot_3d(i,kk1,kk,j) = abstot(i,kk1,kk)
          enddo
        enddo
      enddo
!      do kk = 1,4
      do kk = 1,cam_abs_dim1
        do kk1 = kts,kte
          do i = its,ite
            absnxt_3d(i,kk1,kk,j) = absnxt(i,kk1,kk)
          enddo
        enddo
      enddo
      do kk = kts,kte+1
          do i = its,ite
            emstot_3d(i,kk,j) = emstot(i,kk)
          enddo
      enddo
      endif

      IF(PRESENT(SWUPT))THEN
      if(dosw)then
! Added shortwave and longwave upward/downward total and clear sky fluxes
      do k = kts,kte+1
      kk = kte +1 - k + kts
      do i = its,ite
      ii = i - its + 1
!      swup(i,k,j)      = fsup(ii,kk)
!      swupclear(i,k,j) = fsupc(ii,kk)
!      swdn(i,k,j)      = fsdn(ii,kk)
!      swdnclear(i,k,j) = fsdnc(ii,kk)
       if(k.eq.kte+1)then
         swupt(i,j)     = fsup(ii,kk)
         swuptc(i,j)    = fsupc(ii,kk)
         swdnt(i,j)     = fsdn(ii,kk)
         swdntc(i,j)    = fsdnc(ii,kk)
       endif
       if(k.eq.kts)then
         swupb(i,j)     = fsup(ii,kk)
         swupbc(i,j)    = fsupc(ii,kk)
         swdnb(i,j)     = fsdn(ii,kk)
         swdnbc(i,j)    = fsdnc(ii,kk)
       endif
!            if(i.eq.30.and.j.eq.30) then
!            print 1234, 'short ', i,ii,k,kk,fsup(ii,kk),fsupc(ii,kk),fsdn(ii,kk),fsdnc(ii,kk)
!            1234 format (a6,4i4,4f10.3)
!            endif
     enddo
      enddo
      endif
      if(dolw)then
! Added shortwave and longwave upward/downward total and clear sky fluxes
      do k = kts,kte+1
      kk = kte +1 - k + kts
      do i = its,ite
      ii = i - its + 1
!      lwup(i,k,j)      = flup(ii,kk)
!      lwupclear(i,k,j) = flupc(ii,kk)
!      lwdn(i,k,j)      = fldn(ii,kk)
!      lwdnclear(i,k,j) = fldnc(ii,kk)
       if(k.eq.kte+1)then
         lwupt(i,j)     = flup(ii,kk)
         lwuptc(i,j)    = flupc(ii,kk)
         lwdnt(i,j)     = fldn(ii,kk)
         lwdntc(i,j)    = fldnc(ii,kk)
       endif
       if(k.eq.kts)then
         lwupb(i,j)     = flup(ii,kk)
         lwupbc(i,j)    = flupc(ii,kk)
         lwdnb(i,j)     = fldn(ii,kk)
         lwdnbc(i,j)    = fldnc(ii,kk)
       endif
!            if(i.eq.30.and.j.eq.30) then
!            print 1234, 'long  ', i,ii,k,kk,flup(ii,kk),flupc(ii,kk),fldn(ii,kk),fldnc(ii,kk)
!            1234 format (a6,4i4,4f10.3)
!            endif
      enddo
      enddo
      endif
      ENDIF

      do i = its,ite
      ii = i - its + 1
! Added shortwave and longwave cloud forcing at TOA and surface
      if(dolw)then
        GLW(I,J) = flwds(ii)
        lwcf(i,j) = lwcftoa(ii)
        olr(i,j)  = olrtoa(ii)
      endif
      if(dosw)then
        GSW(I,J) = fsns(ii)
        swcf(i,j) = swcftoa(ii)
        coszr(i,j) = coszrs(ii)
        SWDDIR(i,j)= fsdsdir(ii)               ! amontornes-bcodina (2014-04-20)
        SWDDNI(i,j)= fsdsdir(ii)/coszrs(ii)    ! amontornes-bcodina (2014-04-20)
        SWDDIF(i,j)= fsdsdif(ii)               ! amontornes-bcodina (2014-04-20)
      endif
      enddo
!-------fds (06/2010)---------
   SELECT CASE(sf_surface_physics)
   CASE (SSIBSCHEME)
!   call wrf_message("CAM using ssib albedo2")
      if(dosw)then
      do i = its,ite
        ii = i - its + 1
        SWVISDIR(I,J) = sols(ii)  !SSiB 
        SWVISDIF(I,J) = solsd(ii) !SSiB 
        SWNIRDIR(I,J) = soll(ii)  !SSiB 
        SWNIRDIF(I,J) = solld(ii) !SSiB 
      enddo
      endif
  CASE (CLMSCHEME)
      if(dosw)then
      do i = its,ite
        ii = i - its + 1
        SWVISDIR(I,J) = sols(ii)  !CLM
        SWVISDIF(I,J) = solsd(ii) !CLM
        SWNIRDIR(I,J) = soll(ii)  !CLM
        SWNIRDIF(I,J) = solld(ii) !CLM
      enddo
      endif

   END SELECT
!-----------------------------

    enddo    ! j-loop

#endif

end subroutine camrad
!====================================================================
   SUBROUTINE camradinit(                                           &
                         R_D,R_V,CP,G,STBOLT,EP_2,shalf,pptop,               &
                         ozmixm,pin,levsiz,XLAT,num_months,         &
                         m_psp,m_psn,m_hybi,aerosolcp,aerosolcn,    &
                         paerlev,naer_c,                            &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )

   USE module_wrf_error
   USE module_state_description
   !USE module_configure

!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,  &
                                     ims, ime, jms, jme, kms, kme,  &
                                     its, ite, jts, jte, kts, kte
   REAL, intent(in)               :: pptop
   REAL, INTENT(IN)               :: R_D,R_V,CP,G,STBOLT,EP_2

   REAL,     DIMENSION( kms:kme )  :: shalf

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months
   INTEGER,      INTENT(IN   )    ::   paerlev, naer_c

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  :: XLAT

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months ),      &
          INTENT(INOUT   ) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), INTENT(INOUT )  ::                   PIN
   REAL,  DIMENSION(ims:ime, jms:jme), INTENT(INOUT )  ::                  m_psp,m_psn
   REAL,  DIMENSION(paerlev), INTENT(INOUT )  ::               m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, naer_c ),      &
          INTENT(INOUT) ::                             aerosolcp,aerosolcn

   REAL(r8)    :: pstd
   REAL(r8)    :: rh2o, cpair

! These were made allocatable 20090612 to save static memory allocation. JM
   IF ( .NOT. ALLOCATED( ksul   ) ) ALLOCATE( ksul( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( wsul   ) ) ALLOCATE( wsul( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( gsul   ) ) ALLOCATE( gsul( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( ksslt  ) ) ALLOCATE( ksslt( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( wsslt  ) ) ALLOCATE( wsslt( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( gsslt  ) ) ALLOCATE( gsslt( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( kcphil ) ) ALLOCATE( kcphil( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( wcphil ) ) ALLOCATE( wcphil( nrh, nspint ) )
   IF ( .NOT. ALLOCATED( gcphil ) ) ALLOCATE( gcphil( nrh, nspint ) )

   IF ( .NOT. ALLOCATED(ah2onw  ) ) ALLOCATE( ah2onw(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(eh2onw  ) ) ALLOCATE( eh2onw(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(ah2ow   ) ) ALLOCATE( ah2ow(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(cn_ah2ow) ) ALLOCATE( cn_ah2ow(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(cn_eh2ow) ) ALLOCATE( cn_eh2ow(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(ln_ah2ow) ) ALLOCATE( ln_ah2ow(n_p, n_tp, n_u, n_te, n_rh) )
   IF ( .NOT. ALLOCATED(ln_eh2ow) ) ALLOCATE( ln_eh2ow(n_p, n_tp, n_u, n_te, n_rh) )

#if !defined(MAC_KLUDGE)
   ozncyc = .true.
   indirect = .true.
   ixcldliq = 2
   ixcldice = 3
#if (NMM_CORE != 1)
! aerosol array is not in the NMM Registry 
!   since CAM radiation not available to NMM (yet)
!   so this is blocked out to enable CAM compilation with NMM
   idxSUL = P_SUL
   idxSSLT = P_SSLT
   idxDUSTfirst = P_DUST1
   idxOCPHO = P_OCPHO
   idxCARBONfirst = P_OCPHO
   idxBCPHO = P_BCPHO
   idxOCPHI = P_OCPHI
   idxBCPHI = P_BCPHI
   idxBG = P_BG
   idxVOLC = P_VOLC
#endif

   pstd = 101325.0
! from physconst module
   mwdry = 28.966            ! molecular weight dry air ~ kg/kmole (shr_const_mwdair)
   mwco2 =  44.              ! molecular weight co2
   mwh2o = 18.016            ! molecular weight water vapor (shr_const_mwwv)
   mwch4 =  16.              ! molecular weight ch4
   mwn2o =  44.              ! molecular weight n2o
   mwf11 = 136.              ! molecular weight cfc11
   mwf12 = 120.              ! molecular weight cfc12
   cappa = R_D/CP
   rair = R_D
   tmelt = 273.16            ! freezing T of fresh water ~ K 
   r_universal = 6.02214e26 * STBOLT   ! Universal gas constant ~ J/K/kmole
   latvap = 2.501e6          ! latent heat of evaporation ~ J/kg
   latice = 3.336e5          ! latent heat of fusion ~ J/kg
   zvir = R_V/R_D - 1.
   rh2o = R_V
   cpair = CP
!
   epsqs = EP_2

   CALL radini(G, CP, EP_2, STBOLT, pstd*10.0 )
   CALL esinti(epsqs  ,latvap  ,latice  ,rh2o    ,cpair   ,tmelt   )
   CALL oznini(ozmixm,pin,levsiz,num_months,XLAT,                   &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)                   
   CALL aerosol_init(m_psp,m_psn,m_hybi,aerosolcp,aerosolcn,paerlev,naer_c,shalf,pptop,    &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)

#endif

   END SUBROUTINE camradinit
#if !defined(MAC_KLUDGE)


subroutine oznint(julday,julian,dt,gmt,xtime,ozmixmj,ozmix,levsiz,num_months,pcols)

      IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months,pcols

   REAL(r8),  DIMENSION( pcols, levsiz, num_months ),      &
          INTENT(IN   ) ::                                  ozmixmj 

   REAL, INTENT(IN    )      ::        XTIME,GMT
   INTEGER, INTENT(IN )      ::        JULDAY
   REAL,    INTENT(IN )      ::        JULIAN
   REAL,    INTENT(IN )      ::        DT

   REAL(r8),  DIMENSION( pcols, levsiz ),      &
          INTENT(OUT  ) ::                                  ozmix
   !Local
   REAL(r8)  :: intJULIAN
   integer   :: np1,np,nm,m,k,i
   integer   :: IJUL
   integer, dimension(12) ::  date_oz
   data date_oz/16, 45, 75, 105, 136, 166, 197, 228, 258, 289, 319, 350/
   real(r8) :: cdayozp, cdayozm
   real(r8) :: fact1, fact2
   logical  :: finddate
   CHARACTER(LEN=256) :: msgstr

   ! JULIAN starts from 0.0 at 0Z on 1 Jan.
   intJULIAN = JULIAN + 1.0_r8    ! offset by one day
! jan 1st 00z is julian=1.0 here
   IJUL=INT(intJULIAN)
!  Note that following will drift. 
!    Need to use actual month/day info to compute julian.
   intJULIAN=intJULIAN-FLOAT(IJUL)
   IJUL=MOD(IJUL,365)
   IF(IJUL.EQ.0)IJUL=365
   intJULIAN=intJULIAN+IJUL
   np1=1
   finddate=.false.
!   do m=1,num_months
   do m=1,12
   if(date_oz(m).gt.intjulian.and..not.finddate) then
     np1=m
     finddate=.true.
   endif
   enddo
   cdayozp=date_oz(np1)
   if(np1.gt.1) then
   cdayozm=date_oz(np1-1)
   np=np1
   nm=np-1
   else
   cdayozm=date_oz(12)
   np=np1
   nm=12
   endif
   call getfactors(ozncyc,np1, cdayozm, cdayozp,intjulian, &
                    fact1, fact2) 

!
! Time interpolation.
!
      do k=1,levsiz
         do i=1,pcols
            ozmix(i,k) = ozmixmj(i,k,nm)*fact1 + ozmixmj(i,k,np)*fact2
         end do
      end do

END subroutine oznint


subroutine get_aerosol(c, julday, julian, dt, gmt, xtime, m_psp, m_psn, aerosoljp, &
  aerosoljn, m_hybi, paerlev, naer_c, pint, pcols, pver, pverp, pverr, pverrp, AEROSOLt, scale)
!------------------------------------------------------------------
!
!  Input:
!     time at which aerosol mmrs are needed (get_curr_calday())
!     chunk index
!     CAM's vertical grid (pint)
!
!  Output:
!     values for Aerosol Mass Mixing Ratios at specified time
!     on vertical grid specified by CAM (AEROSOLt)
!
!  Method:
!     first determine which indexs of aerosols are the bounding data sets
!     interpolate both onto vertical grid aerm(),aerp().
!     from those two, interpolate in time.
!
!------------------------------------------------------------------

!  use volcanicmass, only: get_volcanic_mass
!  use timeinterp, only: getfactors
!
! aerosol fields interpolated to current time step
!   on pressure levels of this time step.
! these should be made read-only for other modules
! Is allocation done correctly here?
!
   integer, intent(in) :: c                   ! Chunk Id.
   integer, intent(in) :: paerlev, naer_c, pcols, pver, pverp, pverr, pverrp
   real(r8), intent(in) :: pint(pcols,pverp)  ! midpoint pres.
   real(r8), intent(in) :: scale(naer_all)    ! scale each aerosol by this amount
   REAL, INTENT(IN    )      ::        XTIME,GMT
   INTEGER, INTENT(IN )      ::        JULDAY
   REAL, INTENT(IN    )      ::        JULIAN
   REAL, INTENT(IN    )      ::        DT
   real(r8), intent(in   )      ::        m_psp(pcols),m_psn(pcols)  ! Match surface pressure
   real(r8), intent(in   )   ::        aerosoljp(pcols,paerlev,naer_c) 
   real(r8), intent(in   )   ::        aerosoljn(pcols,paerlev,naer_c) 
   real(r8), intent(in   )   ::        m_hybi(paerlev)

   real(r8), intent(out) :: AEROSOLt(pcols, pver, naer_all) ! aerosols
!
! Local workspace
!
   real(r8) caldayloc                     ! calendar day of current timestep
   real(r8) fact1, fact2                  ! time interpolation factors

  integer :: nm = 1                ! index to prv month in array. init to 1 and toggle between 1 and 2
  integer :: np = 2                ! index to nxt month in array. init to 2 and toggle between 1 and 2
  integer :: mo_nxt = bigint       ! index to nxt month in file
  integer :: mo_prv                       ! index to previous month

  real(r8) :: cdaym = inf          ! calendar day of prv month
  real(r8) :: cdayp = inf          ! calendar day of next month
  real(r8) :: Mid(12)              ! Days into year for mid month date
  data Mid/16.5, 46.0, 75.5, 106.0, 136.5, 167.0, 197.5, 228.5, 259.0, 289.5, 320.0, 350.5 /

   integer i, k, j                        ! spatial indices
   integer m                              ! constituent index
   integer lats(pcols),lons(pcols)        ! latitude and longitudes of column
   integer ncol                           ! number of columns
   INTEGER IJUL
   REAL(r8) intJULIAN

   real(r8) speciesmin(naer)              ! minimal value for each species
!
! values before current time step "the minus month"
! aerosolm(pcols,pver) is value of preceeding month's aerosol mmr
! aerosolp(pcols,pver) is value of next month's aerosol mmr
!  (think minus and plus or values to left and right of point to be interpolated)
!
   real(r8) AEROSOLm(pcols,pver,naer) ! aerosol mmr from MATCH in column at previous (minus) month
!
! values beyond (or at) current time step "the plus month"
!
   real(r8) AEROSOLp(pcols,pver,naer) ! aerosol mmr from MATCH in column at next (plus) month
   CHARACTER(LEN=256) :: msgstr

   ! JULIAN starts from 0.0 at 0Z on 1 Jan.
   intJULIAN = JULIAN + 1.0_r8    ! offset by one day
! jan 1st 00z is julian=1.0 here
   IJUL=INT(intJULIAN)
!  Note that following will drift. 
!    Need to use actual month/day info to compute julian.
   intJULIAN=intJULIAN-FLOAT(IJUL)
   IJUL=MOD(IJUL,365)
   IF(IJUL.EQ.0)IJUL=365
   caldayloc=intJULIAN+IJUL

   if (caldayloc < Mid(1)) then
      mo_prv = 12
      mo_nxt =  1
   else if (caldayloc >= Mid(12)) then
      mo_prv = 12
      mo_nxt =  1
   else
      do i = 2 , 12
         if (caldayloc < Mid(i)) then
            mo_prv = i-1
            mo_nxt = i
            exit
         end if
      end do
   end if
!
! Set initial calendar day values
!
   cdaym = Mid(mo_prv)
   cdayp = Mid(mo_nxt)

!
! Determine time interpolation factors.  1st arg says we are cycling 1 year of data
!
   call getfactors (.true., mo_nxt, cdaym, cdayp, caldayloc, &
                    fact1, fact2)
!
! interpolate (prv and nxt month) bounding datasets onto cam vertical grid.
! compute mass mixing ratios on CAMS's pressure coordinate
!  for both the "minus" and "plus" months
!
!  ncol = get_ncols_p(c)
   ncol = pcols

!  call vert_interpolate (M_ps_cam_col(1,c,nm), pint, nm, AEROSOLm, ncol, c)
!  call vert_interpolate (M_ps_cam_col(1,c,np), pint, np, AEROSOLp, ncol, c)

   call vert_interpolate (m_psp, aerosoljp, m_hybi, paerlev, naer_c, pint, nm, AEROSOLm, pcols, pver, pverp, ncol, c)
   call vert_interpolate (m_psn, aerosoljn, m_hybi, paerlev, naer_c, pint, np, AEROSOLp, pcols, pver, pverp, ncol, c)

!
! Time interpolate.
!
   do m=1,naer
      do k=1,pver
         do i=1,ncol
            AEROSOLt(i,k,m) = AEROSOLm(i,k,m)*fact1 + AEROSOLp(i,k,m)*fact2
         end do
      end do
   end do

!  do i=1,ncol
!     Match_ps_chunk(i,c) = m_ps(i,nm)*fact1 + m_ps(i,np)*fact2
!  end do
!
! get background aerosol (tuning) field
!
   call background (c, ncol, pint, pcols, pverr, pverrp, AEROSOLt(:, :, idxBG))

!
! find volcanic aerosol masses
!
! if (strat_volcanic) then
!   call get_volcanic_mass(c, AEROSOLt(:,:,idxVOLC))
! else
    AEROSOLt(:,:,idxVOLC) = 0._r8
! endif

!
! exit if mmr is negative (we have previously set
!  cumulative mass to be a decreasing function.)
!
   speciesmin(:) = 0. ! speciesmin(m) = 0 is minimum mmr for each species

   do m=1,naer
      do k=1,pver
         do i=1,ncol
            if (AEROSOLt(i, k, m) < speciesmin(m)) then
               write(6,*) 'AEROSOL_INTERPOLATE: negative mass mixing ratio, exiting'
               write(6,*) 'm, column, pver',m, i, k ,AEROSOLt(i, k, m)
!ccc
#ifdef CLWRFGHG
!CLWRF-UC July.09
               print *,'naer:',naer,' pver:',pver,' ncol:',ncol
               PRINT *,'ERROR -- error -- ERROR -- error -- ERROR -- error'
               CALL wrf_error_fatal('CLWRF-module_ra_cam. : AEROSOLt=NaN')
#endif
!ccc
               call endrun ()
            end if
         end do
      end do
   end do
!
! scale any AEROSOLS as required
!
   call scale_aerosols (AEROSOLt, pcols, pver, ncol, c, scale)

   return
end subroutine get_aerosol


subroutine aerosol_indirect(ncol,lchnk,pcols,pver,ppcnst,landfrac,pmid,t,qm1,cld,zm,rel)
!--------------------------------------------------------------
! Compute effect of sulfate on effective liquid water radius
!  Method of Martin et. al.
!--------------------------------------------------------------

! use constituents, only: ppcnst, cnst_get_ind
! use history, only: outfld

!#include "comctl.h"

  integer, intent(in) :: ncol                  ! number of atmospheric columns
  integer, intent(in) :: lchnk                 ! chunk identifier
  integer, intent(in) :: pcols,pver,ppcnst

  real(r8), intent(in) :: landfrac(pcols)      ! land fraction
  real(r8), intent(in) :: pmid(pcols,pver)     ! Model level pressures
  real(r8), intent(in) :: t(pcols,pver)        ! Model level temperatures
  real(r8), intent(in) :: qm1(pcols,pver,ppcnst) ! Specific humidity and tracers
  real(r8), intent(in) :: cld(pcols,pver)      ! Fractional cloud cover
  real(r8), intent(in) :: zm(pcols,pver)       ! Height of midpoints (above surface)
  real(r8), intent(in) :: rel(pcols,pver)      ! liquid effective drop size (microns)
!
! local variables
!
  real(r8) locrhoair(pcols,pver)  ! dry air density            [kg/m^3 ]
  real(r8) lwcwat(pcols,pver)     ! in-cloud liquid water path [kg/m^3 ]
  real(r8) sulfmix(pcols,pver)    ! sulfate mass mixing ratio  [kg/kg  ]
  real(r8) so4mass(pcols,pver)    ! sulfate mass concentration [g/cm^3 ]
  real(r8) Aso4(pcols,pver)       ! sulfate # concentration    [#/cm^3 ]
  real(r8) Ntot(pcols,pver)       ! ccn # concentration        [#/cm^3 ]
  real(r8) relmod(pcols,pver)     ! effective radius           [microns]

  real(r8) wrel(pcols,pver)       ! weighted effective radius    [microns]
  real(r8) wlwc(pcols,pver)       ! weighted liq. water content  [kg/m^3 ]
  real(r8) cldfrq(pcols,pver)     ! frequency of occurance of...
!                                  ! clouds (cld => 0.01)         [fraction]
  real(r8) locPi                  ! my piece of the pi
  real(r8) Rdryair                ! gas constant of dry air   [J/deg/kg]
  real(r8) rhowat                 ! density of water          [kg/m^3  ]
  real(r8) Acoef                  ! m->A conversion factor; assumes
!                                  ! Dbar=0.10, sigma=2.0      [g^-1    ]
  real(r8) rekappa                ! kappa in evaluation of re(lmod)
  real(r8) recoef                 ! temp. coeficient for calc of re(lmod)
  real(r8) reexp                  ! 1.0/3.0
  real(r8) Ntotb                  ! temp var to hold below cloud ccn
! -- Parameters for background CDNC (from `ambient' non-sulfate aerosols)...
  real(r8) Cmarn                  ! Coef for CDNC_marine         [cm^-3]
  real(r8) Cland                  ! Coef for CDNC_land           [cm^-3]
  real(r8) Hmarn                  ! Scale height for CDNC_marine [m]
  real(r8) Hland                  ! Scale height for CDNC_land   [m]
  parameter ( Cmarn = 50.0, Cland = 100.0 )
  parameter ( Hmarn = 1000.0, Hland = 2000.0 )
  real(r8) bgaer                  ! temp var to hold background CDNC

  integer i,k     ! loop indices
!
! Statement functions
!
  logical land    ! is this a column over land?
  land(i) = nint(landfrac(i)).gt.0.5_r8

  if (indirect) then

!   call endrun ('AEROSOL_INDIRECT:  indirect effect is obsolete')

!   ramping is not yet resolved so sulfmix is 0.
    sulfmix(1:ncol,1:pver) = 0._r8

    locPi = 3.141592654
    Rdryair = 287.04
    rhowat = 1000.0
    Acoef = 1.2930E14
    recoef = 3.0/(4.0*locPi*rhowat)
    reexp = 1.0/3.0

!   call cnst_get_ind('CLDLIQ', ixcldliq)
    do k=pver,1,-1
      do i = 1,ncol
        locrhoair(i,k) = pmid(i,k)/( Rdryair*t(i,k) )
        lwcwat(i,k) = ( qm1(i,k,ixcldliq)/max(0.01_r8,cld(i,k)) )* &
                      locrhoair(i,k)
!          NOTE: 0.001 converts kg/m3 -> g/cm3
        so4mass(i,k) = sulfmix(i,k)*locrhoair(i,k)*0.001
        Aso4(i,k) = so4mass(i,k)*Acoef

        if (Aso4(i,k) <= 280.0) then
           Aso4(i,k) = max(36.0_r8,Aso4(i,k))
           Ntot(i,k) = -1.15E-3*Aso4(i,k)**2 + 0.963*Aso4(i,k)+5.30
           rekappa = 0.80
        else
           Aso4(i,k) = min(1500.0_r8,Aso4(i,k))
           Ntot(i,k) = -2.10E-4*Aso4(i,k)**2 + 0.568*Aso4(i,k)-27.9
           rekappa = 0.67
        end if
        if (land(i)) then ! Account for local background aerosol;
           bgaer = Cland*exp(-(zm(i,k)/Hland))
           Ntot(i,k) = max(bgaer,Ntot(i,k))
        else
           bgaer = Cmarn*exp(-(zm(i,k)/Hmarn))
           Ntot(i,k) = max(bgaer,Ntot(i,k))
        end if

        if (k == pver) then
           Ntotb = Ntot(i,k)
        else
           Ntotb = Ntot(i,k+1)
        end if

        relmod(i,k) = (( (recoef*lwcwat(i,k))/(rekappa*Ntotb))**reexp)*10000.0
        relmod(i,k) = max(4.0_r8,relmod(i,k))
        relmod(i,k) = min(20.0_r8,relmod(i,k))
        if (cld(i,k) >= 0.01) then
           cldfrq(i,k) = 1.0
        else
           cldfrq(i,k) = 0.0
        end if
        wrel(i,k) = relmod(i,k)*cldfrq(i,k)
        wlwc(i,k) = lwcwat(i,k)*cldfrq(i,k)
      end do
    end do
!   call outfld('MSO4    ',so4mass,pcols,lchnk)
!   call outfld('LWC     ',lwcwat ,pcols,lchnk)
!   call outfld('CLDFRQ  ',cldfrq ,pcols,lchnk)
!   call outfld('WREL    ',wrel   ,pcols,lchnk)
!   call outfld('WLWC    ',wlwc   ,pcols,lchnk)
!   write(6,*)'WARNING: indirect calculation has no effects'
  else
    do k = 1, pver
      do i = 1, ncol
        relmod(i,k) = rel(i,k)
      end do
    end do
  endif

! call outfld('REL     ',relmod ,pcols,lchnk)

  return
end subroutine aerosol_indirect


      subroutine aer_trn(aer_mpp, aer_trn_ttl, pcols, plev, plevp )
!
!     Purpose: Compute strat. aerosol transmissions needed in absorptivity/
!              emissivity calculations
!              aer_trn() is called by radclw() when doabsems is .true.
!
!     use shr_kind_mod, only: r8 => shr_kind_r8
!     use pmgrid
!     use ppgrid
!     use prescribed_aerosols, only: strat_volcanic
      implicit none

!     Input arguments
!
!       [kg m-2] Volcanics path above kth interface level
!
      integer, intent(in)         :: pcols, plev, plevp
      real(r8), intent(in) :: aer_mpp(pcols,plevp)

!     Output arguments
!
!       [fraction] Total volcanic transmission between interfaces k1 and k2
!
      real(r8), intent(out) ::  aer_trn_ttl(pcols,plevp,plevp,bnd_nbr_LW)

!-------------------------------------------------------------------------
!     Local variables

      integer bnd_idx           ! LW band index
      integer i                 ! lon index
      integer k1                ! lev index
      integer k2                ! lev index
      real(r8) aer_pth_dlt      ! [kg m-2] Volcanics path between interface
                                !          levels k1 and k2
      real(r8) odap_aer_ttl     ! [fraction] Total path absorption optical
                                !            depth

!-------------------------------------------------------------------------

      if (strat_volcanic) then
        do bnd_idx=1,bnd_nbr_LW
           do i=1,pcols
              aer_trn_ttl(i,1,1,bnd_idx)=1.0
           end do
           do k1=2,plevp
              do i=1,pcols
                 aer_trn_ttl(i,k1,k1,bnd_idx)=1.0

                 aer_pth_dlt  = abs(aer_mpp(i,k1) - aer_mpp(i,1))
                 odap_aer_ttl = abs_cff_mss_aer(bnd_idx) * aer_pth_dlt

                 aer_trn_ttl(i,1,k1,bnd_idx) = exp(-1.66 * odap_aer_ttl)
              end do
           end do

           do k1=2,plev
              do k2=k1+1,plevp
                 do i=1,pcols
                    aer_trn_ttl(i,k1,k2,bnd_idx) = &
                         aer_trn_ttl(i,1,k2,bnd_idx) / &
                         aer_trn_ttl(i,1,k1,bnd_idx)
                 end do
              end do
           end do

           do k1=2,plevp
              do k2=1,k1-1
                 do i=1,pcols
                    aer_trn_ttl(i,k1,k2,bnd_idx)=aer_trn_ttl(i,k2,k1,bnd_idx)
                 end do
              end do
           end do
        end do
      else
        aer_trn_ttl = 1.0
      endif

      return
      end subroutine aer_trn

      subroutine aer_pth(aer_mass, aer_mpp, ncol, pcols, plev, plevp)
!------------------------------------------------------
!     Purpose: convert mass per layer to cumulative mass from Top
!------------------------------------------------------
!     use shr_kind_mod, only: r8 => shr_kind_r8
!     use ppgrid
!     use pmgrid
      implicit none
!#include "crdcon.h"

!     Parameters
!     Input
      integer, intent(in)        :: pcols, plev, plevp
      real(r8), intent(in):: aer_mass(pcols,plev)  ! Rad level aerosol mass mixing ratio
      integer, intent(in):: ncol
!
!     Output
      real(r8), intent(out):: aer_mpp(pcols,plevp) ! [kg m-2] Volcanics path above kth interface
!
!     Local
      integer i      ! Column index
      integer k      ! Level index
!------------------------------------------------------
!------------------------------------------------------

      aer_mpp(1:ncol,1) =  0._r8
      do k=2,plevp
          aer_mpp(1:ncol,k) = aer_mpp(1:ncol,k-1) + aer_mass(1:ncol,k-1)
      enddo
!
      return
      end subroutine aer_pth

subroutine radctl(j, lchnk   ,ncol    , pcols, pver, pverp, pverr, pverrp, ppcnst, pcnst,  &
                  lwups   ,emis    ,          &
                  pmid    ,pint    ,pmln    ,piln    ,pdel    ,t       , &
!                 qm1     ,cld     ,cicewp  ,cliqwp  ,coszrs,  clat, &
                  qm1     ,cld     ,cicewp  ,cliqwp  ,tauxcl, tauxci, coszrs,  clat, &
                  asdir   ,asdif   ,aldir   ,aldif   ,solcon, GMT,JULDAY,JULIAN,DT,XTIME,  &
                  pin, ozmixmj, ozmix, levsiz, num_months,      &
                  m_psp, m_psn,  aerosoljp, aerosoljn, m_hybi, paerlev, naer_c, pmxrgn  , &
                  nmxrgn  ,                   &
                  dolw, dosw, doabsems, abstot, absnxt, emstot, &
                  fsup    ,fsupc   ,fsdn    ,fsdnc   , &
                  fsdndir ,fsdncdir,fsdndif ,fsdncdif, & ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes
                  flup    ,flupc   ,fldn    ,fldnc   , &
                  swcf    ,lwcf    ,flut    ,          &
                  fsns    ,fsnt    ,flns    ,flnt    , &
                  qrs     ,qrl     ,flwds   ,rel     ,rei     , &
                  sols    ,soll    ,solsd   ,solld   , &
!ccc
#ifdef CLWRFGHG
!CLWRF-UC June.09
                  n2ovmr, ch4vmr, f11vmr, f12vmr     , &
#endif
!ccc
                  landfrac,zm      ,fsds, fsdsdir,fsdsdif     ) ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Driver for radiation computation.
! 
! Method: 
! Radiation uses cgs units, so conversions must be done from
! model fields to radiation fields.
!
! Author: CCM1,  CMS Contact: J. Truesdale
! 
!-----------------------------------------------------------------------
!  use shr_kind_mod, only: r8 => shr_kind_r8
!  use ppgrid
!  use pspect
!  use commap
!  use history, only: outfld
!  use constituents, only: ppcnst, cnst_get_ind
!  use prescribed_aerosols, only: get_aerosol, naer_all, aerosol_diagnostics, &
!     aerosol_indirect, get_rf_scales, get_int_scales, radforce, idxVOLC
!  use physics_types, only: physics_state
!  use wv_saturation, only: aqsat
!  use chemistry,    only: trace_gas
!  use physconst, only: cpair, epsilo
!  use aer_optics, only: idxVIS
!  use aerosol_intr, only: set_aerosol_from_prognostics


   implicit none

!
! Input arguments
!
   integer, intent(in) :: lchnk,j                 ! chunk identifier
   integer, intent(in) :: ncol                  ! number of atmospheric columns
   integer, intent(in) :: levsiz                ! number of ozone data levels
   integer, intent(in) :: num_months            ! 12 months
   integer, intent(in) :: paerlev,naer_c          ! aerosol vertical level and # species
   integer, intent(in) :: pcols, pver, pverp, pverr, pverrp, ppcnst, pcnst
   logical, intent(in) :: dolw,dosw,doabsems


   integer nspint            ! Num of spctrl intervals across solar spectrum
   integer naer_groups       ! Num of aerosol groups for optical diagnostics
   parameter ( nspint = 19 )
   parameter ( naer_groups = 7 )    ! current groupings are sul, sslt, all carbons, all dust, background, and all aerosols


   real(r8), intent(in) :: lwups(pcols)         ! Longwave up flux at surface
   real(r8), intent(in) :: emis(pcols,pver)     ! Cloud emissivity
   real(r8), intent(in) :: pmid(pcols,pver)     ! Model level pressures
   real(r8), intent(in) :: pint(pcols,pverp)    ! Model interface pressures
   real(r8), intent(in) :: pmln(pcols,pver)     ! Natural log of pmid
   real(r8), intent(in) :: rel(pcols,pver)      ! liquid effective drop size (microns)
   real(r8), intent(in) :: rei(pcols,pver)      ! ice effective drop size (microns)
   real(r8), intent(in) :: piln(pcols,pverp)    ! Natural log of pint
   real(r8), intent(in) :: pdel(pcols,pverp)    ! Pressure difference across layer 
   real(r8), intent(in) :: t(pcols,pver)        ! Model level temperatures
   real(r8), intent(in) :: qm1(pcols,pver,ppcnst) ! Specific humidity and tracers
   real(r8), intent(in) :: cld(pcols,pver)      ! Fractional cloud cover
   real(r8), intent(in) :: cicewp(pcols,pver)   ! in-cloud cloud ice water path
   real(r8), intent(in) :: cliqwp(pcols,pver)   ! in-cloud cloud liquid water path
   real(r8), intent(inout) :: tauxcl(pcols,0:pver) ! cloud water optical depth
   real(r8), intent(inout) :: tauxci(pcols,0:pver) ! cloud ice optical depth
   real(r8), intent(in) :: coszrs(pcols)        ! Cosine solar zenith angle
   real(r8), intent(in) :: clat(pcols)          ! latitude in radians for columns 
   real(r8), intent(in) :: asdir(pcols)         ! albedo shortwave direct
   real(r8), intent(in) :: asdif(pcols)         ! albedo shortwave diffuse
   real(r8), intent(in) :: aldir(pcols)         ! albedo longwave direct
   real(r8), intent(in) :: aldif(pcols)         ! albedo longwave diffuse
   real(r8), intent(in) :: landfrac(pcols)      ! land fraction
   real(r8), intent(in) :: zm(pcols,pver)       ! Height of midpoints (above surface)
   real(r8), intent(in) :: pin(levsiz)          ! Pressure levels of ozone data
   real(r8), intent(in) :: ozmixmj(pcols,levsiz,num_months)  ! monthly ozone mixing ratio
   real(r8), intent(inout) :: ozmix(pcols,levsiz)  ! Ozone data
   real, intent(in) :: solcon               ! solar constant with eccentricity factor
   REAL, INTENT(IN    )      ::        XTIME,GMT              
   INTEGER, INTENT(IN )      ::        JULDAY
   REAL,    INTENT(IN )      ::        JULIAN
   REAL,    INTENT(IN )      ::        DT
   real(r8), intent(in)     :: m_psp(pcols),m_psn(pcols)       ! MATCH surface pressure
   real(r8), intent(in)     :: aerosoljp(pcols,paerlev,naer_c)   ! aerosol concentrations
   real(r8), intent(in)     :: aerosoljn(pcols,paerlev,naer_c)   ! aerosol concentrations
   real(r8), intent(in)     :: m_hybi(paerlev)
!  type(physics_state), intent(in) :: state     
   real(r8), intent(inout) :: pmxrgn(pcols,pverp) ! Maximum values of pmid for each
!    maximally overlapped region.
!    0->pmxrgn(i,1) is range of pmid for
!    1st region, pmxrgn(i,1)->pmxrgn(i,2) for
!    2nd region, etc
   integer, intent(inout) :: nmxrgn(pcols)     ! Number of maximally overlapped regions

    real(r8) :: pmxrgnrf(pcols,pverp)             ! temporary copy of pmxrgn
    integer  :: nmxrgnrf(pcols)     ! temporary copy of nmxrgn

!
! Output solar arguments
!
   real(r8), intent(out) :: fsns(pcols)          ! Surface absorbed solar flux
   real(r8), intent(out) :: fsnt(pcols)          ! Net column abs solar flux at model top
   real(r8), intent(out) :: flns(pcols)          ! Srf longwave cooling (up-down) flux
   real(r8), intent(out) :: flnt(pcols)          ! Net outgoing lw flux at model top
   real(r8), intent(out) :: sols(pcols)          ! Downward solar rad onto surface (sw direct)
   real(r8), intent(out) :: soll(pcols)          ! Downward solar rad onto surface (lw direct)
   real(r8), intent(out) :: solsd(pcols)         ! Downward solar rad onto surface (sw diffuse)
   real(r8), intent(out) :: solld(pcols)         ! Downward solar rad onto surface (lw diffuse)
   real(r8), intent(out) :: qrs(pcols,pver)      ! Solar heating rate
   real(r8), intent(out) :: fsds(pcols)          ! Flux Shortwave Downwelling Surface
   real(r8), intent(out) :: fsdsdir(pcols)       ! Flux Shortwave Direct Downwelling Surface (amontornes-bcodina 2014-04-20)
   real(r8), intent(out) :: fsdsdif(pcols)       ! Flux Shortwave Diffuse Downwelling Surface (amontornes-bcodina 2014-04-20)
! Added outputs of total and clearsky fluxes etc
   real(r8), intent(out) :: fsup(pcols,pverp)    ! Upward total sky solar
   real(r8), intent(out) :: fsupc(pcols,pverp)   ! Upward clear sky solar
   real(r8), intent(out) :: fsdn(pcols,pverp)    ! Downward total sky solar
   real(r8), intent(out) :: fsdnc(pcols,pverp)   ! Downward clear sky solar
   real(r8), intent(out) :: fsdndir(pcols,pverp) ! Downward Direct total sky solar (amontornes-bcodina 2014-04-20)
   real(r8), intent(out) :: fsdncdir(pcols,pverp)! Downward Direct clear sky solar (amontornes-bcodina 2014-04-20)
   real(r8), intent(out) :: fsdndif(pcols,pverp) ! Downward Diffuse total sky solar (amontornes-bcodina 2014-04-20)
   real(r8), intent(out) :: fsdncdif(pcols,pverp)! Downward Diffuse clear sky solar (amontornes-bcodina 2014-04-20)
   real(r8), intent(out) :: flup(pcols,pverp)    ! Upward total sky longwave
   real(r8), intent(out) :: flupc(pcols,pverp)   ! Upward clear sky longwave
   real(r8), intent(out) :: fldn(pcols,pverp)    ! Downward total sky longwave
   real(r8), intent(out) :: fldnc(pcols,pverp)   ! Downward clear sky longwave
   real(r8), intent(out) :: swcf(pcols)          ! Top of the atmosphere solar cloud forcing
   real(r8), intent(out) :: lwcf(pcols)          ! Top of the atmosphere longwave cloud forcing
   real(r8), intent(out) :: flut(pcols)          ! Top of the atmosphere outgoing longwave
!
! Output longwave arguments
!
   real(r8), intent(out) :: qrl(pcols,pver)      ! Longwave cooling rate
   real(r8), intent(out) :: flwds(pcols)         ! Surface down longwave flux

   real(r8), intent(inout) :: abstot(pcols,pverp,pverp) ! Total absorptivity
   real(r8), intent(inout) :: absnxt(pcols,pver,4)      ! Total nearest layer absorptivity
   real(r8), intent(inout) :: emstot(pcols,pverp)     ! Total emissivity


!
!---------------------------Local variables-----------------------------
!
   integer i, k              ! index

   integer :: in2o, ich4, if11, if12 ! indexes of gases in constituent array

   real(r8) solin(pcols)         ! Solar incident flux
!  real(r8) fsds(pcols)          ! Flux Shortwave Downwelling Surface
   real(r8) fsntoa(pcols)        ! Net solar flux at TOA
   real(r8) fsntoac(pcols)       ! Clear sky net solar flux at TOA
   real(r8) fsnirt(pcols)        ! Near-IR flux absorbed at toa
   real(r8) fsnrtc(pcols)        ! Clear sky near-IR flux absorbed at toa
   real(r8) fsnirtsq(pcols)      ! Near-IR flux absorbed at toa >= 0.7 microns
   real(r8) fsntc(pcols)         ! Clear sky total column abs solar flux
   real(r8) fsnsc(pcols)         ! Clear sky surface abs solar flux
   real(r8) fsdsc(pcols)         ! Clear sky surface downwelling solar flux
   real(r8) fsdscdir(pcols)      ! Clear sky surface direct downwelling solar flux (amontornes-bcodina 2014-04-20)
   real(r8) fsdscdif(pcols)      ! Clear sky surface diffuse downwelling solar flux (amontornes-bcodina 2014-04-20)
!  real(r8) flut(pcols)          ! Upward flux at top of model
!  real(r8) lwcf(pcols)          ! longwave cloud forcing
!  real(r8) swcf(pcols)          ! shortwave cloud forcing
   real(r8) flutc(pcols)         ! Upward Clear Sky flux at top of model
   real(r8) flntc(pcols)         ! Clear sky lw flux at model top
   real(r8) flnsc(pcols)         ! Clear sky lw flux at srf (up-down)
   real(r8) ftem(pcols,pver)     ! temporary array for outfld

   real(r8) pbr(pcols,pverr)     ! Model mid-level pressures (dynes/cm2)
   real(r8) pnm(pcols,pverrp)    ! Model interface pressures (dynes/cm2)
   real(r8) o3vmr(pcols,pverr)   ! Ozone volume mixing ratio
   real(r8) o3mmr(pcols,pverr)   ! Ozone mass mixing ratio
   real(r8) eccf                 ! Earth/sun distance factor
   real(r8) n2o(pcols,pver)      ! nitrous oxide mass mixing ratio
   real(r8) ch4(pcols,pver)      ! methane mass mixing ratio
   real(r8) cfc11(pcols,pver)    ! cfc11 mass mixing ratio
   real(r8) cfc12(pcols,pver)    ! cfc12 mass mixing ratio
   real(r8) rh(pcols,pverr)      ! level relative humidity (fraction)
   real(r8) lwupcgs(pcols)       ! Upward longwave flux in cgs units

   real(r8) esat(pcols,pverr)    ! saturation vapor pressure
   real(r8) qsat(pcols,pverr)    ! saturation specific humidity

   real(r8) :: frc_day(pcols) ! = 1 for daylight, =0 for night colums
   real(r8) :: aertau(pcols,nspint,naer_groups) ! Aerosol column optical depth
   real(r8) :: aerssa(pcols,nspint,naer_groups) ! Aerosol column averaged single scattering albedo
   real(r8) :: aerasm(pcols,nspint,naer_groups) ! Aerosol column averaged asymmetry parameter
   real(r8) :: aerfwd(pcols,nspint,naer_groups) ! Aerosol column averaged forward scattering

   real(r8) aerosol(pcols, pver, naer_all) ! aerosol mass mixing ratios
   real(r8) scales(naer_all)               ! scaling factors for aerosols

!ccc
#ifdef CLWRFGHG
!CLWRF-UC July.09
   real(r8), INTENT(IN)        ::    n2ovmr, ch4vmr, f11vmr, f12vmr
#endif
   LOGICAL, EXTERNAL                                :: wrf_dm_on_monitor
   CHARACTER (LEN=256)         ::    message
!ccc

!
! Interpolate ozone volume mixing ratio to model levels
!
! WRF: added pin, levsiz, ozmix here
   call oznint(julday,julian,dt,gmt,xtime,ozmixmj,ozmix,levsiz,num_months,pcols)

   call radozn(lchnk   ,ncol    &
              ,pcols, pver &
              ,pmid    ,pin, levsiz, ozmix, o3vmr   )

!  call outfld('O3VMR   ',o3vmr ,pcols, lchnk)

!
! Set chunk dependent radiation input
!
   call radinp(lchnk   ,ncol    ,pcols, pver, pverp,      &
               pmid    ,pint    ,o3vmr   , pbr     ,&
               pnm     ,eccf    ,o3mmr   )

!
! Solar radiation computation
!
   if (dosw) then

!
! calculate heating with aerosols
!
      call aqsat(t, pmid, esat, qsat, pcols, &
                 ncol, pver, 1, pver)

      ! calculate relative humidity
!     rh(1:ncol,1:pver) = q(1:ncol,1:pver,1) / qsat(1:ncol,1:pver) * &
!        ((1.0 - epsilo) * qsat(1:ncol,1:pver) + epsilo) / &
!        ((1.0 - epsilo) * q(1:ncol,1:pver,1) + epsilo)
      rh(1:ncol,1:pver) = qm1(1:ncol,1:pver,1) / qsat(1:ncol,1:pver) * &
         ((1.0 - epsilo) * qsat(1:ncol,1:pver) + epsilo) / &
         ((1.0 - epsilo) * qm1(1:ncol,1:pver,1) + epsilo)

      if (radforce) then

         pmxrgnrf = pmxrgn
         nmxrgnrf = nmxrgn

         call get_rf_scales(scales)

         call get_aerosol(lchnk, julday, julian, dt, gmt, xtime, m_psp, m_psn, aerosoljp, &
           aerosoljn, m_hybi, paerlev, naer, pint, pcols, pver, pverp, pverr, pverrp, aerosol, scales)

         ! overwrite with prognostics aerosols

!   no feedback from prognostic aerosols 
!        call set_aerosol_from_prognostics (ncol, q, aerosol)

         call aerosol_indirect(ncol,lchnk,pcols,pver,ppcnst,landfrac,pmid,t,qm1,cld,zm,rel)
   
!        call t_startf('radcswmx_rf')
         call radcswmx(j, lchnk   ,ncol ,pcols, pver, pverp,         &
                    pnm     ,pbr     ,qm1     ,rh      ,o3mmr   , &
                    aerosol ,cld     ,cicewp  ,cliqwp  ,rel     , &
!                   rei     ,eccf    ,coszrs  ,scon    ,solin   ,solcon , &
                    rei     ,tauxcl  ,tauxci  ,eccf    ,coszrs  ,scon    ,solin   ,solcon , &
                    asdir   ,asdif   ,aldir   ,aldif   ,nmxrgnrf, &
                    pmxrgnrf,qrs     ,fsnt    ,fsntc   ,fsntoa  , &
                    fsntoac ,fsnirt  ,fsnrtc  ,fsnirtsq,fsns    , &
                    fsnsc   ,fsdsc   ,fsds    ,sols    ,soll    , &
                    solsd   ,solld   ,frc_day ,                   &
                    fsup    ,fsupc   ,fsdn    ,fsdnc   ,          &
                    fsdndir ,fsdncdir,fsdndif ,fsdncdif,          & ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes profile
                    fsdsdir ,fsdsdif ,fsdscdir,fsdscdif,          & ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes sfc
                    aertau  ,aerssa  ,aerasm  ,aerfwd             )
!        call t_stopf('radcswmx_rf')

!
! Convert units of shortwave fields needed by rest of model from CGS to MKS
!

            do i = 1, ncol
            solin(i) = solin(i)*1.e-3
            fsnt(i)  = fsnt(i) *1.e-3
            fsns(i)  = fsns(i) *1.e-3
            fsntc(i) = fsntc(i)*1.e-3
            fsnsc(i) = fsnsc(i)*1.e-3
            end do
         ftem(:ncol,:pver) = qrs(:ncol,:pver)/cpair
!
! Dump shortwave radiation information to history tape buffer (diagnostics)
!
!        call outfld('QRS_RF  ',ftem  ,pcols,lchnk)
!        call outfld('FSNT_RF ',fsnt  ,pcols,lchnk)
!        call outfld('FSNS_RF ',fsns  ,pcols,lchnk)
!        call outfld('FSNTC_RF',fsntc ,pcols,lchnk)
!        call outfld('FSNSC_RF',fsnsc ,pcols,lchnk)
 
      endif ! if (radforce)

      call get_int_scales(scales)

      call get_aerosol(lchnk, julday, julian, dt, gmt, xtime, m_psp, m_psn, aerosoljp, aerosoljn, &
             m_hybi, paerlev, naer, pint, pcols, pver, pverp, pverr, pverrp, aerosol, scales)

      ! overwrite with prognostics aerosols
!     call set_aerosol_from_prognostics (ncol, q, aerosol)

      call aerosol_indirect(ncol,lchnk,pcols,pver,ppcnst,landfrac,pmid,t,qm1,cld,zm,rel)
!     call t_startf('radcswmx')

      call radcswmx(j, lchnk   ,ncol    ,pcols, pver, pverp,         &
                    pnm     ,pbr     ,qm1     ,rh      ,o3mmr   , &
                    aerosol ,cld     ,cicewp  ,cliqwp  ,rel     , &
!                   rei     ,eccf    ,coszrs  ,scon    ,solin   ,solcon , &
                    rei     ,tauxcl  ,tauxci  ,eccf    ,coszrs  ,scon    ,solin   ,solcon , &
                    asdir   ,asdif   ,aldir   ,aldif   ,nmxrgn  , &
                    pmxrgn  ,qrs     ,fsnt    ,fsntc   ,fsntoa  , &
                    fsntoac ,fsnirt  ,fsnrtc  ,fsnirtsq,fsns    , &
                    fsnsc   ,fsdsc   ,fsds    ,sols    ,soll    , &
                    solsd   ,solld   ,frc_day ,                   &
                    fsup    ,fsupc   ,fsdn    ,fsdnc   ,          &
                    fsdndir ,fsdncdir,fsdndif ,fsdncdif,          & ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes profiles
                    fsdsdir ,fsdsdif ,fsdscdir,fsdscdif,          & ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes sfc
                    aertau  ,aerssa  ,aerasm  ,aerfwd             )
!     call t_stopf('radcswmx')

! -- tls ---------------------------------------------------------------2
!
! Convert units of shortwave fields needed by rest of model from CGS to MKS
!
      do i=1,ncol
         solin(i) = solin(i)*1.e-3
         fsds(i)  = fsds(i)*1.e-3
         fsnirt(i)= fsnirt(i)*1.e-3
         fsnrtc(i)= fsnrtc(i)*1.e-3
         fsnirtsq(i)= fsnirtsq(i)*1.e-3
         fsnt(i)  = fsnt(i) *1.e-3
         fsns(i)  = fsns(i) *1.e-3
         fsntc(i) = fsntc(i)*1.e-3
         fsnsc(i) = fsnsc(i)*1.e-3
         fsdsc(i) = fsdsc(i)*1.e-3
         fsntoa(i)=fsntoa(i)*1.e-3
         fsntoac(i)=fsntoac(i)*1.e-3
         swcf(i)  = fsntoa(i) - fsntoac(i)

         fsdsdir(i)  = fsdsdir(i)*1.e-3  ! amontornes-bcodina (2014-04-20)
         fsdsdif(i)  = fsdsdif(i)*1.e-3  ! amontornes-bcodina (2014-04-20)
         fsdscdir(i) = fsdscdir(i)*1.e-3 ! amontornes-bcodina (2014-04-20)
         fsdscdif(i) = fsdscdif(i)*1.e-3 ! amontornes-bcodina (2014-04-20)
      end do
      ftem(:ncol,:pver) = qrs(:ncol,:pver)/cpair

! Added upward/downward total and clear sky fluxes
         do k = 1, pverp
            do i = 1, ncol
            fsup(i,k)  = fsup(i,k)*1.e-3
            fsupc(i,k) = fsupc(i,k)*1.e-3
            fsdn(i,k)  = fsdn(i,k)*1.e-3
            fsdnc(i,k) = fsdnc(i,k)*1.e-3
            end do
         end do

!
! Dump shortwave radiation information to history tape buffer (diagnostics)
!

!     call outfld('frc_day ', frc_day, pcols, lchnk)
!     call outfld('SULOD_v ', aertau(:,idxVIS,1) ,pcols,lchnk)
!     call outfld('SSLTOD_v', aertau(:,idxVIS,2) ,pcols,lchnk)
!     call outfld('CAROD_v ', aertau(:,idxVIS,3) ,pcols,lchnk)
!     call outfld('DUSTOD_v', aertau(:,idxVIS,4) ,pcols,lchnk)
!     call outfld('BGOD_v  ', aertau(:,idxVIS,5) ,pcols,lchnk)
!     call outfld('VOLCOD_v', aertau(:,idxVIS,6) ,pcols,lchnk)
!     call outfld('AEROD_v ', aertau(:,idxVIS,7) ,pcols,lchnk)
!     call outfld('AERSSA_v', aerssa(:,idxVIS,7) ,pcols,lchnk)
!     call outfld('AERASM_v', aerasm(:,idxVIS,7) ,pcols,lchnk)
!     call outfld('AERFWD_v', aerfwd(:,idxVIS,7) ,pcols,lchnk)
!     call aerosol_diagnostics (lchnk, ncol, pdel, aerosol)

!     call outfld('QRS     ',ftem  ,pcols,lchnk)
!     call outfld('SOLIN   ',solin ,pcols,lchnk)
!     call outfld('FSDS    ',fsds  ,pcols,lchnk)
!     call outfld('FSNIRTOA',fsnirt,pcols,lchnk)
!     call outfld('FSNRTOAC',fsnrtc,pcols,lchnk)
!     call outfld('FSNRTOAS',fsnirtsq,pcols,lchnk)
!     call outfld('FSNT    ',fsnt  ,pcols,lchnk)
!     call outfld('FSNS    ',fsns  ,pcols,lchnk)
!     call outfld('FSNTC   ',fsntc ,pcols,lchnk)
!     call outfld('FSNSC   ',fsnsc ,pcols,lchnk)
!     call outfld('FSDSC   ',fsdsc ,pcols,lchnk)
!     call outfld('FSNTOA  ',fsntoa,pcols,lchnk)
!     call outfld('FSNTOAC ',fsntoac,pcols,lchnk)
!     call outfld('SOLS    ',sols  ,pcols,lchnk)
!     call outfld('SOLL    ',soll  ,pcols,lchnk)
!     call outfld('SOLSD   ',solsd ,pcols,lchnk)
!     call outfld('SOLLD   ',solld ,pcols,lchnk)

   end if
!
! Longwave radiation computation
!
   if (dolw) then

      call get_int_scales(scales)

      call get_aerosol(lchnk, julday, julian, dt, gmt, xtime, m_psp, m_psn, aerosoljp, aerosoljn, &
             m_hybi, paerlev, naer, pint, pcols, pver, pverp, pverr, pverrp, aerosol, scales)

!
! Convert upward longwave flux units to CGS
!
      do i=1,ncol
!        lwupcgs(i) = lwup(i)*1000.
         lwupcgs(i) = lwups(i)
      end do
!
! Do longwave computation. If not implementing greenhouse gas code then
! first specify trace gas mixing ratios. If greenhouse gas code then:
!  o ixtrcg   => indx of advected n2o tracer
!  o ixtrcg+1 => indx of advected ch4 tracer
!  o ixtrcg+2 => indx of advected cfc11 tracer
!  o ixtrcg+3 => indx of advected cfc12 tracer
!
      if (trace_gas) then
!        call cnst_get_ind('N2O'  , in2o)
!        call cnst_get_ind('CH4'  , ich4)
!        call cnst_get_ind('CFC11', if11)
!        call cnst_get_ind('CFC12', if12)
!        call t_startf("radclwmx")
         call radclwmx(lchnk   ,ncol    ,pcols, pver, pverp ,        & 
                       lwupcgs ,t       ,qm1(1,1,1)       ,o3vmr ,   &
                       pbr     ,pnm     ,pmln    ,piln    ,          &
                       qm1(1,1,in2o)    ,qm1(1,1,ich4)    ,          &
                       qm1(1,1,if11)    ,qm1(1,1,if12)    ,          &
                       cld     ,emis    ,pmxrgn  ,nmxrgn  ,qrl     , &
                       doabsems, abstot, absnxt, emstot,             &
                       flns    ,flnt    ,flnsc   ,flntc   ,flwds   , &
                       flut    ,flutc   ,                            &
                       flup    ,flupc   ,fldn    ,fldnc   ,          &
                       aerosol(:,:,idxVOLC))
!        call t_stopf("radclwmx")
      else
!ccc
#ifdef CLWRFGHG
         IF ( wrf_dm_on_monitor() ) THEN
           WRITE(message,*)'CLWRF-CAM_call-trcmix n2ovmr:',n2ovmr,' ch4vmr:',ch4vmr,' f11vmr:',f11vmr,' f12vmr:',f12vmr
           CALL wrf_debug(1, message)
         END IF
         call trcmix_clwrf(lchnk   ,ncol    ,pcols, pver,  &
                     pmid    ,clat, n2ovmr, ch4vmr, f11vmr, f12vmr, n2o,   &
                     ch4, cfc11, cfc12 )

#else
         call trcmix(lchnk   ,ncol    ,pcols, pver,  &
                     pmid    ,clat, n2o     ,ch4     ,                     &
                     cfc11   ,cfc12   )
#endif
         IF ( wrf_dm_on_monitor() ) THEN
           WRITE(message,*)'CLWRF post_trcmix_values. n2o:', n2o(pcols/2,pver/2), ' ch4:',      &
             ch4(pcols/2,pver/2),' cfc11:', cfc11(pcols/2,pver/2),' cfc12:', cfc12(pcols/2,pver/2) 
           CALL wrf_debug(1, message)
         END IF 
!ccc
!        call t_startf("radclwmx")
         call radclwmx(lchnk     ,ncol    ,pcols, pver, pverp ,        &
                       lwupcgs   ,t       ,qm1(1,1,1)       ,o3vmr ,   &
                       pbr       ,pnm     ,pmln    ,piln    ,          &
                       n2o       ,ch4     ,cfc11   ,cfc12   ,          &
                       cld       ,emis    ,pmxrgn  ,nmxrgn  ,qrl     , &
                       doabsems, abstot, absnxt, emstot,             &
                       flns      ,flnt    ,flnsc   ,flntc   ,flwds   , &
                       flut      ,flutc   ,                            &
                       flup      ,flupc   ,fldn    ,fldnc   ,          &
                       aerosol(:,:,idxVOLC))
!        call t_stopf("radclwmx")
      endif
!
! Convert units of longwave fields needed by rest of model from CGS to MKS
!
      do i=1,ncol
         flnt(i)  = flnt(i)*1.e-3
         flut(i)  = flut(i)*1.e-3
         flutc(i) = flutc(i)*1.e-3
         flns(i)  = flns(i)*1.e-3
         flntc(i) = flntc(i)*1.e-3
         flnsc(i) = flnsc(i)*1.e-3
         flwds(i) = flwds(i)*1.e-3
         lwcf(i)  = flutc(i) - flut(i)
      end do

! Added upward/downward total and clear sky fluxes
         do k = 1, pverp
            do i = 1, ncol
            flup(i,k)  = flup(i,k)*1.e-3
            flupc(i,k) = flupc(i,k)*1.e-3
            fldn(i,k)  = fldn(i,k)*1.e-3
            fldnc(i,k) = fldnc(i,k)*1.e-3
            end do
         end do
!
! Dump longwave radiation information to history tape buffer (diagnostics)
!
!     call outfld('QRL     ',qrl(:ncol,:)/cpair,ncol,lchnk)
!     call outfld('FLNT    ',flnt  ,pcols,lchnk)
!     call outfld('FLUT    ',flut  ,pcols,lchnk)
!     call outfld('FLUTC   ',flutc ,pcols,lchnk)
!     call outfld('FLNTC   ',flntc ,pcols,lchnk)
!     call outfld('FLNS    ',flns  ,pcols,lchnk)
!     call outfld('FLNSC   ',flnsc ,pcols,lchnk)
!     call outfld('LWCF    ',lwcf  ,pcols,lchnk)
!     call outfld('SWCF    ',swcf  ,pcols,lchnk)
!
   end if
!
   return
end subroutine radctl
  subroutine param_cldoptics_calc(ncol, pcols, pver, pverp, pverr, pverrp, ppcnst, &
                                  q, cldn, landfrac, landm,icefrac, &
        pdel,  t, ps, pmid, pint, cicewp, cliqwp, emis, rel, rei, pmxrgn, nmxrgn, snowh )
!
! Compute (liquid+ice) water path and cloud water/ice diagnostics
! *** soon this code will compute liquid and ice paths from input liquid and ice mixing ratios
! 
! **** mixes interface and physics code temporarily
!-----------------------------------------------------------------------
!   use physics_types, only: physics_state
!   use history,       only: outfld
!   use pkg_cldoptics, only: cldefr, cldems, cldovrlap, cldclw

    implicit none

! Arguments
    integer, intent(in) :: ncol, pcols, pver, pverp, pverr, pverrp, ppcnst
    real(r8), intent(in)  :: q(pcols,pver,ppcnst)     ! moisture arrays
    real(r8), intent(in)  :: cldn(pcols,pver)        ! new cloud fraction
    real(r8), intent(in)  :: pdel(pcols,pver)        ! pressure thickness
    real(r8), intent(in)  :: t(pcols,pver)           ! temperature
    real(r8), intent(in)  :: pmid(pcols,pver)        ! pressure 
    real(r8), intent(in)  :: pint(pcols,pverp)       ! pressure 
    real(r8), intent(in)  :: ps(pcols)               ! surface pressure 
    real(r8), intent(in)  :: landfrac(pcols)         ! Land fraction
    real(r8), intent(in)  :: icefrac(pcols)          ! Ice fraction
    real(r8), intent(in)  :: landm(pcols)            ! Land fraction ramped
    real(r8), intent(in) :: snowh(pcols)         ! Snow depth over land, water equivalent (m)

!!$    real(r8), intent(out) :: cwp   (pcols,pver)      ! in-cloud cloud (total) water path
    real(r8), intent(out) :: cicewp(pcols,pver)      ! in-cloud cloud ice water path
    real(r8), intent(out) :: cliqwp(pcols,pver)      ! in-cloud cloud liquid water path
    real(r8), intent(out) :: emis  (pcols,pver)      ! cloud emissivity
    real(r8), intent(out) :: rel   (pcols,pver)      ! effective drop radius (microns)
    real(r8), intent(out) :: rei   (pcols,pver)      ! ice effective drop size (microns)
    real(r8), intent(out) :: pmxrgn(pcols,pver+1)    ! Maximum values of pressure for each
    integer , intent(out) :: nmxrgn(pcols)           ! Number of maximally overlapped regions

! Local variables
    real(r8) :: cwp   (pcols,pver)      ! in-cloud cloud (total) water path
!!$    real(r8) :: cicewp(pcols,pver)      ! in-cloud cloud ice water path
!!$    real(r8) :: cliqwp(pcols,pver)      ! in-cloud cloud liquid water path
    real(r8) :: effcld(pcols,pver)                   ! effective cloud=cld*emis
    real(r8) :: gicewp(pcols,pver)                   ! grid-box cloud ice water path
    real(r8) :: gliqwp(pcols,pver)                   ! grid-box cloud liquid water path
    real(r8) :: gwp   (pcols,pver)                   ! grid-box cloud (total) water path
    real(r8) :: hl     (pcols)                       ! Liquid water scale height
    real(r8) :: tgicewp(pcols)                       ! Vertically integrated ice water path
    real(r8) :: tgliqwp(pcols)                       ! Vertically integrated liquid water path
    real(r8) :: tgwp   (pcols)                       ! Vertically integrated (total) cloud water path
    real(r8) :: tpw    (pcols)                       ! total precipitable water
    real(r8) :: clwpold(pcols,pver)                  ! Presribed cloud liq. h2o path
    real(r8) :: ficemr (pcols,pver)                  ! Ice fraction from ice and liquid mixing ratios

    real(r8) :: rgrav                ! inverse gravitational acceleration

    integer :: i,k                                   ! loop indexes
    integer :: lchnk

!-----------------------------------------------------------------------

! Compute liquid and ice water paths
    tgicewp(:ncol) = 0.
    tgliqwp(:ncol) = 0.
    do k=1,pver
       do i = 1,ncol
          gicewp(i,k) = q(i,k,ixcldice)*pdel(i,k)/gravmks*1000.0  ! Grid box ice water path.
          gliqwp(i,k) = q(i,k,ixcldliq)*pdel(i,k)/gravmks*1000.0  ! Grid box liquid water path.
!!$          gwp   (i,k) = gicewp(i,k) + gliqwp(i,k)
          cicewp(i,k) = gicewp(i,k) / max(0.01_r8,cldn(i,k))                 ! In-cloud ice water path.
          cliqwp(i,k) = gliqwp(i,k) / max(0.01_r8,cldn(i,k))                 ! In-cloud liquid water path.
!!$          cwp   (i,k) = gwp   (i,k) / max(0.01_r8,cldn(i,k))
          ficemr(i,k) = q(i,k,ixcldice) /                 &
               max(1.e-10_r8,(q(i,k,ixcldice)+q(i,k,ixcldliq)))
          
          tgicewp(i)  = tgicewp(i) + gicewp(i,k)
          tgliqwp(i)  = tgliqwp(i) + gliqwp(i,k)
       end do
    end do
    tgwp(:ncol) = tgicewp(:ncol) + tgliqwp(:ncol)
    gwp(:ncol,:pver) = gicewp(:ncol,:pver) + gliqwp(:ncol,:pver) 
    cwp(:ncol,:pver) = cicewp(:ncol,:pver) + cliqwp(:ncol,:pver) 

! Compute total preciptable water in column (in mm)
    tpw(:ncol) = 0.0
    rgrav = 1.0/gravmks
    do k=1,pver
       do i=1,ncol
          tpw(i) = tpw(i) + pdel(i,k)*q(i,k,1)*rgrav
       end do
    end do

! Diagnostic liquid water path (old specified form)
!   call cldclw(lchnk, ncol, pcols, pver, pverp, state%zi, clwpold, tpw, hl)

! Cloud water and ice particle sizes
    call cldefr(lchnk, ncol, pcols, pver, pverp, landfrac, t, rel, rei, ps, pmid, landm, icefrac, snowh)

! Cloud emissivity.
    call cldems(lchnk, ncol, pcols, pver, pverp, cwp, ficemr, rei, emis)

! Effective cloud cover
    do k=1,pver
       do i=1,ncol
          effcld(i,k) = cldn(i,k)*emis(i,k)
       end do
    end do

! Determine parameters for maximum/random overlap
    call cldovrlap(lchnk, ncol, pcols, pver, pverp, pint, cldn, nmxrgn, pmxrgn)

!   call outfld('GCLDLWP' ,gwp    , pcols,lchnk)
!   call outfld('TGCLDCWP',tgwp   , pcols,lchnk)
!   call outfld('TGCLDLWP',tgliqwp, pcols,lchnk)
!   call outfld('TGCLDIWP',tgicewp, pcols,lchnk)
!   call outfld('ICLDLWP' ,cwp    , pcols,lchnk)
!   call outfld('SETLWP'  ,clwpold, pcols,lchnk)
!   call outfld('EFFCLD'  ,effcld , pcols,lchnk)
!   call outfld('LWSH'    ,hl     , pcols,lchnk)

  end subroutine param_cldoptics_calc

subroutine radabs(lchnk   ,ncol    ,pcols, pver, pverp,   &
   pbr    ,pnm     ,co2em    ,co2eml  ,tplnka  , &
   s2c    ,tcg     ,w        ,h2otr   ,plco2   , &
   plh2o  ,co2t    ,tint     ,tlayr   ,plol    , &
   plos   ,pmln    ,piln     ,ucfc11  ,ucfc12  , &
   un2o0  ,un2o1   ,uch4     ,uco211  ,uco212  , &
   uco213 ,uco221  ,uco222   ,uco223  ,uptype  , &
   bn2o0  ,bn2o1   ,bch4    ,abplnk1  ,abplnk2 , &
   abstot ,absnxt  ,plh2ob  ,wb       , &
   aer_mpp ,aer_trn_ttl)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Compute absorptivities for h2o, co2, o3, ch4, n2o, cfc11 and cfc12
! 
! Method: 
! h2o  ....  Uses nonisothermal emissivity method for water vapor from
!            Ramanathan, V. and  P.Downey, 1986: A Nonisothermal
!            Emissivity and Absorptivity Formulation for Water Vapor
!            Journal of Geophysical Research, vol. 91., D8, pp 8649-8666
!
!            Implementation updated by Collins, Hackney, and Edwards (2001)
!               using line-by-line calculations based upon Hitran 1996 and
!               CKD 2.1 for absorptivity and emissivity
!
!            Implementation updated by Collins, Lee-Taylor, and Edwards (2003)
!               using line-by-line calculations based upon Hitran 2000 and
!               CKD 2.4 for absorptivity and emissivity
!
! co2  ....  Uses absorptance parameterization of the 15 micro-meter
!            (500 - 800 cm-1) band system of Carbon Dioxide, from
!            Kiehl, J.T. and B.P.Briegleb, 1991: A New Parameterization
!            of the Absorptance Due to the 15 micro-meter Band System
!            of Carbon Dioxide Jouranl of Geophysical Research,
!            vol. 96., D5, pp 9013-9019.
!            Parameterizations for the 9.4 and 10.4 mircon bands of CO2
!            are also included.
!
! o3   ....  Uses absorptance parameterization of the 9.6 micro-meter
!            band system of ozone, from Ramanathan, V. and R.Dickinson,
!            1979: The Role of stratospheric ozone in the zonal and
!            seasonal radiative energy balance of the earth-troposphere
!            system. Journal of the Atmospheric Sciences, Vol. 36,
!            pp 1084-1104
!
! ch4  ....  Uses a broad band model for the 7.7 micron band of methane.
!
! n20  ....  Uses a broad band model for the 7.8, 8.6 and 17.0 micron
!            bands of nitrous oxide
!
! cfc11 ...  Uses a quasi-linear model for the 9.2, 10.7, 11.8 and 12.5
!            micron bands of CFC11
!
! cfc12 ...  Uses a quasi-linear model for the 8.6, 9.1, 10.8 and 11.2
!            micron bands of CFC12
!
!
! Computes individual absorptivities for non-adjacent layers, accounting
! for band overlap, and sums to obtain the total; then, computes the
! nearest layer contribution.
! 
! Author: W. Collins (H2O absorptivity) and J. Kiehl
! 
!-----------------------------------------------------------------------
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: lchnk                       ! chunk identifier
   integer, intent(in) :: ncol                        ! number of atmospheric columns
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: pbr(pcols,pver)            ! Prssr at mid-levels (dynes/cm2)
   real(r8), intent(in) :: pnm(pcols,pverp)           ! Prssr at interfaces (dynes/cm2)
   real(r8), intent(in) :: co2em(pcols,pverp)         ! Co2 emissivity function
   real(r8), intent(in) :: co2eml(pcols,pver)         ! Co2 emissivity function
   real(r8), intent(in) :: tplnka(pcols,pverp)        ! Planck fnctn level temperature
   real(r8), intent(in) :: s2c(pcols,pverp)           ! H2o continuum path length
   real(r8), intent(in) :: tcg(pcols,pverp)           ! H2o-mass-wgted temp. (Curtis-Godson approx.)
   real(r8), intent(in) :: w(pcols,pverp)             ! H2o prs wghted path
   real(r8), intent(in) :: h2otr(pcols,pverp)         ! H2o trnsmssn fnct for o3 overlap
   real(r8), intent(in) :: plco2(pcols,pverp)         ! Co2 prs wghted path length
   real(r8), intent(in) :: plh2o(pcols,pverp)         ! H2o prs wfhted path length
   real(r8), intent(in) :: co2t(pcols,pverp)          ! Tmp and prs wghted path length
   real(r8), intent(in) :: tint(pcols,pverp)          ! Interface temperatures
   real(r8), intent(in) :: tlayr(pcols,pverp)         ! K-1 level temperatures
   real(r8), intent(in) :: plol(pcols,pverp)          ! Ozone prs wghted path length
   real(r8), intent(in) :: plos(pcols,pverp)          ! Ozone path length
   real(r8), intent(in) :: pmln(pcols,pver)           ! Ln(pmidm1)
   real(r8), intent(in) :: piln(pcols,pverp)          ! Ln(pintm1)
   real(r8), intent(in) :: plh2ob(nbands,pcols,pverp) ! Pressure weighted h2o path with 
                                                      !    Hulst-Curtis-Godson temp. factor 
                                                      !    for H2O bands 
   real(r8), intent(in) :: wb(nbands,pcols,pverp)     ! H2o path length with 
                                                      !    Hulst-Curtis-Godson temp. factor 
                                                      !    for H2O bands 

   real(r8), intent(in) :: aer_mpp(pcols,pverp) ! STRAER path above kth interface level
   real(r8), intent(in) :: aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) ! aer trn.


!
! Trace gas variables
!
   real(r8), intent(in) :: ucfc11(pcols,pverp)        ! CFC11 path length
   real(r8), intent(in) :: ucfc12(pcols,pverp)        ! CFC12 path length
   real(r8), intent(in) :: un2o0(pcols,pverp)         ! N2O path length
   real(r8), intent(in) :: un2o1(pcols,pverp)         ! N2O path length (hot band)
   real(r8), intent(in) :: uch4(pcols,pverp)          ! CH4 path length
   real(r8), intent(in) :: uco211(pcols,pverp)        ! CO2 9.4 micron band path length
   real(r8), intent(in) :: uco212(pcols,pverp)        ! CO2 9.4 micron band path length
   real(r8), intent(in) :: uco213(pcols,pverp)        ! CO2 9.4 micron band path length
   real(r8), intent(in) :: uco221(pcols,pverp)        ! CO2 10.4 micron band path length
   real(r8), intent(in) :: uco222(pcols,pverp)        ! CO2 10.4 micron band path length
   real(r8), intent(in) :: uco223(pcols,pverp)        ! CO2 10.4 micron band path length
   real(r8), intent(in) :: uptype(pcols,pverp)        ! continuum path length
   real(r8), intent(in) :: bn2o0(pcols,pverp)         ! pressure factor for n2o
   real(r8), intent(in) :: bn2o1(pcols,pverp)         ! pressure factor for n2o
   real(r8), intent(in) :: bch4(pcols,pverp)          ! pressure factor for ch4
   real(r8), intent(in) :: abplnk1(14,pcols,pverp)    ! non-nearest layer Planck factor
   real(r8), intent(in) :: abplnk2(14,pcols,pverp)    ! nearest layer factor
!
! Output arguments
!
   real(r8), intent(out) :: abstot(pcols,pverp,pverp) ! Total absorptivity
   real(r8), intent(out) :: absnxt(pcols,pver,4)      ! Total nearest layer absorptivity
!
!---------------------------Local variables-----------------------------
!
   integer i                   ! Longitude index
   integer k                   ! Level index
   integer k1                  ! Level index
   integer k2                  ! Level index
   integer kn                  ! Nearest level index
   integer wvl                 ! Wavelength index

   real(r8) abstrc(pcols)              ! total trace gas absorptivity
   real(r8) bplnk(14,pcols,4)          ! Planck functions for sub-divided layers
   real(r8) pnew(pcols)        ! Effective pressure for H2O vapor linewidth
   real(r8) pnewb(nbands)      ! Effective pressure for h2o linewidth w/
                               !    Hulst-Curtis-Godson correction for
                               !    each band
   real(r8) u(pcols)           ! Pressure weighted H2O path length
   real(r8) ub(nbands)         ! Pressure weighted H2O path length with
                               !    Hulst-Curtis-Godson correction for
                               !    each band
   real(r8) tbar(pcols,4)      ! Mean layer temperature
   real(r8) emm(pcols,4)       ! Mean co2 emissivity
   real(r8) o3emm(pcols,4)     ! Mean o3 emissivity
   real(r8) o3bndi             ! Ozone band parameter
   real(r8) temh2o(pcols,4)    ! Mean layer temperature equivalent to tbar
   real(r8) k21                ! Exponential coefficient used to calculate
!                              !  rotation band transmissvty in the 650-800
!                              !  cm-1 region (tr1)
   real(r8) k22                ! Exponential coefficient used to calculate
!                              !  rotation band transmissvty in the 500-650
!                              !  cm-1 region (tr2)
   real(r8) uc1(pcols)         ! H2o continuum pathlength in 500-800 cm-1
   real(r8) to3h2o(pcols)      ! H2o trnsmsn for overlap with o3
   real(r8) pi                 ! For co2 absorptivity computation
   real(r8) sqti(pcols)        ! Used to store sqrt of mean temperature
   real(r8) et                 ! Co2 hot band factor
   real(r8) et2                ! Co2 hot band factor squared
   real(r8) et4                ! Co2 hot band factor to fourth power
   real(r8) omet               ! Co2 stimulated emission term
   real(r8) f1co2              ! Co2 central band factor
   real(r8) f2co2(pcols)       ! Co2 weak band factor
   real(r8) f3co2(pcols)       ! Co2 weak band factor
   real(r8) t1co2(pcols)       ! Overlap factr weak bands on strong band
   real(r8) sqwp               ! Sqrt of co2 pathlength
   real(r8) f1sqwp(pcols)      ! Main co2 band factor
   real(r8) oneme              ! Co2 stimulated emission term
   real(r8) alphat             ! Part of the co2 stimulated emission term
   real(r8) wco2               ! Constants used to define co2 pathlength
   real(r8) posqt              ! Effective pressure for co2 line width
   real(r8) u7(pcols)          ! Co2 hot band path length
   real(r8) u8                 ! Co2 hot band path length
   real(r8) u9                 ! Co2 hot band path length
   real(r8) u13                ! Co2 hot band path length
   real(r8) rbeta7(pcols)      ! Inverse of co2 hot band line width par
   real(r8) rbeta8             ! Inverse of co2 hot band line width par
   real(r8) rbeta9             ! Inverse of co2 hot band line width par
   real(r8) rbeta13            ! Inverse of co2 hot band line width par
   real(r8) tpatha             ! For absorptivity computation
   real(r8) abso(pcols,4)      ! Absorptivity for various gases/bands
   real(r8) dtx(pcols)         ! Planck temperature minus 250 K
   real(r8) dty(pcols)         ! Path temperature minus 250 K
   real(r8) term7(pcols,2)     ! Kl_inf(i) in eq(r8) of table A3a of R&D
   real(r8) term8(pcols,2)     ! Delta kl_inf(i) in eq(r8)
   real(r8) tr1                ! Eqn(6) in table A2 of R&D for 650-800
   real(r8) tr10(pcols)        ! Eqn (6) times eq(4) in table A2
!                              !  of R&D for 500-650 cm-1 region
   real(r8) tr2                ! Eqn(6) in table A2 of R&D for 500-650
   real(r8) tr5                ! Eqn(4) in table A2 of R&D for 650-800
   real(r8) tr6                ! Eqn(4) in table A2 of R&D for 500-650
   real(r8) tr9(pcols)         ! Equation (6) times eq(4) in table A2
!                              !  of R&D for 650-800 cm-1 region
   real(r8) sqrtu(pcols)       ! Sqrt of pressure weighted h20 pathlength
   real(r8) fwk(pcols)         ! Equation(33) in R&D far wing correction
   real(r8) fwku(pcols)        ! GU term in eqs(1) and (6) in table A2
   real(r8) to3co2(pcols)      ! P weighted temp in ozone band model
   real(r8) dpnm(pcols)        ! Pressure difference between two levels
   real(r8) pnmsq(pcols,pverp) ! Pressure squared
   real(r8) dw(pcols)          ! Amount of h2o between two levels
   real(r8) uinpl(pcols,4)     ! Nearest layer subdivision factor
   real(r8) winpl(pcols,4)     ! Nearest layer subdivision factor
   real(r8) zinpl(pcols,4)     ! Nearest layer subdivision factor
   real(r8) pinpl(pcols,4)     ! Nearest layer subdivision factor
   real(r8) dplh2o(pcols)      ! Difference in press weighted h2o amount
   real(r8) r293               ! 1/293
   real(r8) r250               ! 1/250
   real(r8) r3205              ! Line width factor for o3 (see R&Di)
   real(r8) r300               ! 1/300
   real(r8) rsslp              ! Reciprocal of sea level pressure
   real(r8) r2sslp             ! 1/2 of rsslp
   real(r8) ds2c               ! Y in eq(7) in table A2 of R&D
   real(r8)  dplos             ! Ozone pathlength eq(A2) in R&Di
   real(r8) dplol              ! Presure weighted ozone pathlength
   real(r8) tlocal             ! Local interface temperature
   real(r8) beta               ! Ozone mean line parameter eq(A3) in R&Di
!                               (includes Voigt line correction factor)
   real(r8) rphat              ! Effective pressure for ozone beta
   real(r8) tcrfac             ! Ozone temperature factor table 1 R&Di
   real(r8) tmp1               ! Ozone band factor see eq(A1) in R&Di
   real(r8) u1                 ! Effective ozone pathlength eq(A2) in R&Di
   real(r8) realnu             ! 1/beta factor in ozone band model eq(A1)
   real(r8) tmp2               ! Ozone band factor see eq(A1) in R&Di
   real(r8) u2                 ! Effective ozone pathlength eq(A2) in R&Di
   real(r8) rsqti              ! Reciprocal of sqrt of path temperature
   real(r8) tpath              ! Path temperature used in co2 band model
   real(r8) tmp3               ! Weak band factor see K&B
   real(r8) rdpnmsq            ! Reciprocal of difference in press^2
   real(r8) rdpnm              ! Reciprocal of difference in press
   real(r8) p1                 ! Mean pressure factor
   real(r8) p2                 ! Mean pressure factor
   real(r8) dtym10             ! T - 260 used in eq(9) and (10) table A3a
   real(r8) dplco2             ! Co2 path length
   real(r8) te                 ! A_0 T factor in ozone model table 1 of R&Di
   real(r8) denom              ! Denominator in eq(r8) of table A3a of R&D
   real(r8) th2o(pcols)        ! transmission due to H2O
   real(r8) tco2(pcols)        ! transmission due to CO2
   real(r8) to3(pcols)         ! transmission due to O3
!
! Transmission terms for various spectral intervals:
!
   real(r8) trab2(pcols)       ! H2o   500 -  800 cm-1
   real(r8) absbnd             ! Proportional to co2 band absorptance
   real(r8) dbvtit(pcols,pverp)! Intrfc drvtv plnck fnctn for o3
   real(r8) dbvtly(pcols,pver) ! Level drvtv plnck fnctn for o3
!
! Variables for Collins/Hackney/Edwards (C/H/E) & 
!       Collins/Lee-Taylor/Edwards (C/LT/E) H2O parameterization

!
! Notation:
! U   = integral (P/P_0 dW)  eq. 15 in Ramanathan/Downey 1986
! P   = atmospheric pressure
! P_0 = reference atmospheric pressure
! W   = precipitable water path
! T_e = emission temperature
! T_p = path temperature
! RH  = path relative humidity
!
   real(r8) fa               ! asymptotic value of abs. as U->infinity
   real(r8) a_star           ! normalized absorptivity for non-window
   real(r8) l_star           ! interpolated line transmission
   real(r8) c_star           ! interpolated continuum transmission

   real(r8) te1              ! emission temperature
   real(r8) te2              ! te^2
   real(r8) te3              ! te^3
   real(r8) te4              ! te^4
   real(r8) te5              ! te^5

   real(r8) log_u            ! log base 10 of U 
   real(r8) log_uc           ! log base 10 of H2O continuum path
   real(r8) log_p            ! log base 10 of P
   real(r8) t_p              ! T_p
   real(r8) t_e              ! T_e (offset by T_p)

   integer iu                ! index for log10(U)
   integer iu1               ! iu + 1
   integer iuc               ! index for log10(H2O continuum path)
   integer iuc1              ! iuc + 1
   integer ip                ! index for log10(P)
   integer ip1               ! ip + 1
   integer itp               ! index for T_p
   integer itp1              ! itp + 1
   integer ite               ! index for T_e
   integer ite1              ! ite + 1
   integer irh               ! index for RH
   integer irh1              ! irh + 1

   real(r8) dvar             ! normalized variation in T_p/T_e/P/U
   real(r8) uvar             ! U * diffusivity factor
   real(r8) uscl             ! factor for lineary scaling as U->0

   real(r8) wu               ! weight for U
   real(r8) wu1              ! 1 - wu
   real(r8) wuc              ! weight for H2O continuum path
   real(r8) wuc1             ! 1 - wuc
   real(r8) wp               ! weight for P
   real(r8) wp1              ! 1 - wp
   real(r8) wtp              ! weight for T_p
   real(r8) wtp1             ! 1 - wtp
   real(r8) wte              ! weight for T_e
   real(r8) wte1             ! 1 - wte
   real(r8) wrh              ! weight for RH
   real(r8) wrh1             ! 1 - wrh

   real(r8) w_0_0_           ! weight for Tp/Te combination
   real(r8) w_0_1_           ! weight for Tp/Te combination
   real(r8) w_1_0_           ! weight for Tp/Te combination
   real(r8) w_1_1_           ! weight for Tp/Te combination

   real(r8) w_0_00           ! weight for Tp/Te/RH combination
   real(r8) w_0_01           ! weight for Tp/Te/RH combination
   real(r8) w_0_10           ! weight for Tp/Te/RH combination
   real(r8) w_0_11           ! weight for Tp/Te/RH combination
   real(r8) w_1_00           ! weight for Tp/Te/RH combination
   real(r8) w_1_01           ! weight for Tp/Te/RH combination
   real(r8) w_1_10           ! weight for Tp/Te/RH combination
   real(r8) w_1_11           ! weight for Tp/Te/RH combination

   real(r8) w00_00           ! weight for P/Tp/Te/RH combination
   real(r8) w00_01           ! weight for P/Tp/Te/RH combination
   real(r8) w00_10           ! weight for P/Tp/Te/RH combination
   real(r8) w00_11           ! weight for P/Tp/Te/RH combination
   real(r8) w01_00           ! weight for P/Tp/Te/RH combination
   real(r8) w01_01           ! weight for P/Tp/Te/RH combination
   real(r8) w01_10           ! weight for P/Tp/Te/RH combination
   real(r8) w01_11           ! weight for P/Tp/Te/RH combination
   real(r8) w10_00           ! weight for P/Tp/Te/RH combination
   real(r8) w10_01           ! weight for P/Tp/Te/RH combination
   real(r8) w10_10           ! weight for P/Tp/Te/RH combination
   real(r8) w10_11           ! weight for P/Tp/Te/RH combination
   real(r8) w11_00           ! weight for P/Tp/Te/RH combination
   real(r8) w11_01           ! weight for P/Tp/Te/RH combination
   real(r8) w11_10           ! weight for P/Tp/Te/RH combination
   real(r8) w11_11           ! weight for P/Tp/Te/RH combination

   integer ib                ! spectral interval:
                             !   1 = 0-800 cm^-1 and 1200-2200 cm^-1
                             !   2 = 800-1200 cm^-1


   real(r8) pch2o            ! H2O continuum path
   real(r8) fch2o            ! temp. factor for continuum
   real(r8) uch2o            ! U corresponding to H2O cont. path (window)

   real(r8) fdif             ! secant(zenith angle) for diffusivity approx.

   real(r8) sslp_mks         ! Sea-level pressure in MKS units
   real(r8) esx              ! saturation vapor pressure returned by vqsatd
   real(r8) qsx              ! saturation mixing ratio returned by vqsatd
   real(r8) pnew_mks         ! pnew in MKS units
   real(r8) q_path           ! effective specific humidity along path
   real(r8) rh_path          ! effective relative humidity along path
   real(r8) omeps            ! 1 - epsilo

   integer  iest             ! index in estblh2o

      integer bnd_idx        ! LW band index
      real(r8) aer_pth_dlt   ! [kg m-2] STRAER path between interface levels k1 and k2
      real(r8) aer_pth_ngh(pcols)
                             ! [kg m-2] STRAER path between neighboring layers
      real(r8) odap_aer_ttl  ! [fraction] Total path absorption optical depth
      real(r8) aer_trn_ngh(pcols,bnd_nbr_LW) 
                             ! [fraction] Total transmission between 
                             !            nearest neighbor sub-levels
!
!--------------------------Statement function---------------------------
!
   real(r8) dbvt,t             ! Planck fnctn tmp derivative for o3
!
   dbvt(t)=(-2.8911366682e-4+(2.3771251896e-6+1.1305188929e-10*t)*t)/ &
      (1.0+(-6.1364820707e-3+1.5550319767e-5*t)*t)
!
!
!-----------------------------------------------------------------------
!
! Initialize
!
   do k2=1,ntoplw-1
      do k1=1,ntoplw-1
         abstot(:,k1,k2) = inf    ! set unused portions for lf95 restart write
      end do
   end do
   do k2=1,4
      do k1=1,ntoplw-1
         absnxt(:,k1,k2) = inf    ! set unused portions for lf95 restart write
      end do
   end do

   do k=ntoplw,pverp
      abstot(:,k,k) = inf         ! set unused portions for lf95 restart write
   end do

   do k=ntoplw,pver
      do i=1,ncol
         dbvtly(i,k) = dbvt(tlayr(i,k+1))
         dbvtit(i,k) = dbvt(tint(i,k))
      end do
   end do
   do i=1,ncol
      dbvtit(i,pverp) = dbvt(tint(i,pverp))
   end do
!
   r293    = 1./293.
   r250    = 1./250.
   r3205   = 1./.3205
   r300    = 1./300.
   rsslp   = 1./sslp
   r2sslp  = 1./(2.*sslp)
!
!Constants for computing U corresponding to H2O cont. path
!
   fdif       = 1.66
   sslp_mks   = sslp / 10.0
   omeps      = 1.0 - epsilo
!
! Non-adjacent layer absorptivity:
!
! abso(i,1)     0 -  800 cm-1   h2o rotation band
! abso(i,1)  1200 - 2200 cm-1   h2o vibration-rotation band
! abso(i,2)   800 - 1200 cm-1   h2o window
!
! Separation between rotation and vibration-rotation dropped, so
!                only 2 slots needed for H2O absorptivity
!
! 500-800 cm^-1 H2o continuum/line overlap already included
!                in abso(i,1).  This used to be in abso(i,4)
!
! abso(i,3)   o3  9.6 micrometer band (nu3 and nu1 bands)
! abso(i,4)   co2 15  micrometer band system
!
   do k=ntoplw,pverp
      do i=1,ncol
         pnmsq(i,k) = pnm(i,k)**2
         dtx(i) = tplnka(i,k) - 250.
      end do
   end do
!
! Non-nearest layer level loops
!
   do k1=pverp,ntoplw,-1
      do k2=pverp,ntoplw,-1
         if (k1 == k2) cycle
         do i=1,ncol
            dplh2o(i) = plh2o(i,k1) - plh2o(i,k2)
            u(i)      = abs(dplh2o(i))
            sqrtu(i)  = sqrt(u(i))
            ds2c      = abs(s2c(i,k1) - s2c(i,k2))
            dw(i)     = abs(w(i,k1) - w(i,k2))
            uc1(i)    = (ds2c + 1.7e-3*u(i))*(1. +  2.*ds2c)/(1. + 15.*ds2c)
            pch2o     = ds2c
            pnew(i)   = u(i)/dw(i)
            pnew_mks  = pnew(i) * sslp_mks
!
! Changed effective path temperature to std. Curtis-Godson form
!
            tpatha = abs(tcg(i,k1) - tcg(i,k2))/dw(i)
            t_p = min(max(tpatha, min_tp_h2o), max_tp_h2o)
            iest = floor(t_p) - min_tp_h2o
            esx = estblh2o(iest) + (estblh2o(iest+1)-estblh2o(iest)) * &
                 (t_p - min_tp_h2o - iest)
            qsx = epsilo * esx / (pnew_mks - omeps * esx)
!
! Compute effective RH along path
!
            q_path = dw(i) / abs(pnm(i,k1) - pnm(i,k2)) / rga
!
! Calculate effective u, pnew for each band using
!        Hulst-Curtis-Godson approximation:
! Formulae: Goody and Yung, Atmospheric Radiation: Theoretical Basis, 
!           2nd edition, Oxford University Press, 1989.
! Effective H2O path (w)
!      eq. 6.24, p. 228
! Effective H2O path pressure (pnew = u/w):
!      eq. 6.29, p. 228
!
            ub(1) = abs(plh2ob(1,i,k1) - plh2ob(1,i,k2)) / psi(t_p,1)
            ub(2) = abs(plh2ob(2,i,k1) - plh2ob(2,i,k2)) / psi(t_p,2)
            
            pnewb(1) = ub(1) / abs(wb(1,i,k1) - wb(1,i,k2)) * phi(t_p,1)
            pnewb(2) = ub(2) / abs(wb(2,i,k1) - wb(2,i,k2)) * phi(t_p,2)

            dtx(i)      = tplnka(i,k2) - 250.
            dty(i)      = tpatha       - 250.

            fwk(i)  = fwcoef + fwc1/(1. + fwc2*u(i))
            fwku(i) = fwk(i)*u(i)
!
! Define variables for C/H/E (now C/LT/E) fit
!
! abso(i,1)     0 -  800 cm-1   h2o rotation band
! abso(i,1)  1200 - 2200 cm-1   h2o vibration-rotation band
! abso(i,2)   800 - 1200 cm-1   h2o window
!
! Separation between rotation and vibration-rotation dropped, so
!                only 2 slots needed for H2O absorptivity
!
! Notation:
! U   = integral (P/P_0 dW)  
! P   = atmospheric pressure
! P_0 = reference atmospheric pressure
! W   = precipitable water path
! T_e = emission temperature
! T_p = path temperature
! RH  = path relative humidity
!
!
! Terms for asymptotic value of emissivity
!
            te1  = tplnka(i,k2)
            te2  = te1 * te1
            te3  = te2 * te1
            te4  = te3 * te1
            te5  = te4 * te1

!
!  Band-independent indices for lines and continuum tables
!
            dvar = (t_p - min_tp_h2o) / dtp_h2o
            itp = min(max(int(aint(dvar,r8)) + 1, 1), n_tp - 1)
            itp1 = itp + 1
            wtp = dvar - floor(dvar)
            wtp1 = 1.0 - wtp
            
            t_e = min(max(tplnka(i,k2)-t_p, min_te_h2o), max_te_h2o)
            dvar = (t_e - min_te_h2o) / dte_h2o
            ite = min(max(int(aint(dvar,r8)) + 1, 1), n_te - 1)
            ite1 = ite + 1
            wte = dvar - floor(dvar)
            wte1 = 1.0 - wte
            
            rh_path = min(max(q_path / qsx, min_rh_h2o), max_rh_h2o)
            dvar = (rh_path - min_rh_h2o) / drh_h2o
            irh = min(max(int(aint(dvar,r8)) + 1, 1), n_rh - 1)
            irh1 = irh + 1
            wrh = dvar - floor(dvar)
            wrh1 = 1.0 - wrh

            w_0_0_ = wtp  * wte
            w_0_1_ = wtp  * wte1
            w_1_0_ = wtp1 * wte 
            w_1_1_ = wtp1 * wte1
            
            w_0_00 = w_0_0_ * wrh
            w_0_01 = w_0_0_ * wrh1
            w_0_10 = w_0_1_ * wrh
            w_0_11 = w_0_1_ * wrh1
            w_1_00 = w_1_0_ * wrh
            w_1_01 = w_1_0_ * wrh1
            w_1_10 = w_1_1_ * wrh
            w_1_11 = w_1_1_ * wrh1

!
! H2O Continuum path for 0-800 and 1200-2200 cm^-1
!
!    Assume foreign continuum dominates total H2O continuum in these bands
!    per Clough et al, JGR, v. 97, no. D14 (Oct 20, 1992), p. 15776
!    Then the effective H2O path is just 
!         U_c = integral[ f(P) dW ]
!    where 
!           W = water-vapor mass and 
!        f(P) = dependence of foreign continuum on pressure 
!             = P / sslp
!    Then 
!         U_c = U (the same effective H2O path as for lines)
!
!
! Continuum terms for 800-1200 cm^-1
!
!    Assume self continuum dominates total H2O continuum for this band
!    per Clough et al, JGR, v. 97, no. D14 (Oct 20, 1992), p. 15776
!    Then the effective H2O self-continuum path is 
!         U_c = integral[ h(e,T) dW ]                        (*eq. 1*)
!    where 
!           W = water-vapor mass and 
!           e = partial pressure of H2O along path
!           T = temperature along path
!      h(e,T) = dependence of foreign continuum on e,T
!             = e / sslp * f(T)
!
!    Replacing
!           e =~ q * P / epsilo
!           q = mixing ratio of H2O
!     epsilo = 0.622
!
!    and using the definition
!           U = integral [ (P / sslp) dW ]
!             = (P / sslp) W                                 (homogeneous path)
!
!    the effective path length for the self continuum is
!         U_c = (q / epsilo) f(T) U                         (*eq. 2*)
!
!    Once values of T, U, and q have been calculated for the inhomogeneous
!        path, this sets U_c for the corresponding
!        homogeneous atmosphere.  However, this need not equal the
!        value of U_c' defined by eq. 1 for the actual inhomogeneous atmosphere
!        under consideration.
!
!    Solution: hold T and q constant, solve for U' that gives U_c' by
!        inverting eq. (2):
!
!        U' = (U_c * epsilo) / (q * f(T))
!
            fch2o = fh2oself(t_p) 
            uch2o = (pch2o * epsilo) / (q_path * fch2o)

!
! Band-dependent indices for non-window
!
            ib = 1

            uvar = ub(ib) * fdif
            log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
            dvar = (log_u - min_lu_h2o) / dlu_h2o
            iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
            iu1 = iu + 1
            wu = dvar - floor(dvar)
            wu1 = 1.0 - wu
            
            log_p  = min(log10(max(pnewb(ib), min_p_h2o)), max_lp_h2o)
            dvar = (log_p - min_lp_h2o) / dlp_h2o
            ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
            ip1 = ip + 1
            wp = dvar - floor(dvar)
            wp1 = 1.0 - wp
         
            w00_00 = wp  * w_0_00 
            w00_01 = wp  * w_0_01 
            w00_10 = wp  * w_0_10 
            w00_11 = wp  * w_0_11 
            w01_00 = wp  * w_1_00 
            w01_01 = wp  * w_1_01 
            w01_10 = wp  * w_1_10 
            w01_11 = wp  * w_1_11 
            w10_00 = wp1 * w_0_00 
            w10_01 = wp1 * w_0_01 
            w10_10 = wp1 * w_0_10 
            w10_11 = wp1 * w_0_11 
            w11_00 = wp1 * w_1_00 
            w11_01 = wp1 * w_1_01 
            w11_10 = wp1 * w_1_10 
            w11_11 = wp1 * w_1_11 
!
! Asymptotic value of absorptivity as U->infinity
!
            fa = fat(1,ib) + &
                 fat(2,ib) * te1 + &
                 fat(3,ib) * te2 + &
                 fat(4,ib) * te3 + &
                 fat(5,ib) * te4 + &
                 fat(6,ib) * te5

            a_star = &
                 ah2onw(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
                 ah2onw(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
                 ah2onw(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
                 ah2onw(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
                 ah2onw(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
                 ah2onw(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
                 ah2onw(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
                 ah2onw(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
                 ah2onw(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
                 ah2onw(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
                 ah2onw(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
                 ah2onw(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
                 ah2onw(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
                 ah2onw(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
                 ah2onw(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
                 ah2onw(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
                 ah2onw(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
                 ah2onw(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
                 ah2onw(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
                 ah2onw(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
                 ah2onw(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
                 ah2onw(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
                 ah2onw(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
                 ah2onw(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
                 ah2onw(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
                 ah2onw(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu 
            abso(i,ib) = min(max(fa * (1.0 - (1.0 - a_star) * &
                                 aer_trn_ttl(i,k1,k2,ib)), &
                             0.0_r8), 1.0_r8)
!
! Invoke linear limit for scaling wrt u below min_u_h2o
!
            if (uvar < min_u_h2o) then
               uscl = uvar / min_u_h2o
               abso(i,ib) = abso(i,ib) * uscl
            endif
                         
!
! Band-dependent indices for window
!
            ib = 2

            uvar = ub(ib) * fdif
            log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
            dvar = (log_u - min_lu_h2o) / dlu_h2o
            iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
            iu1 = iu + 1
            wu = dvar - floor(dvar)
            wu1 = 1.0 - wu
            
            log_p  = min(log10(max(pnewb(ib), min_p_h2o)), max_lp_h2o)
            dvar = (log_p - min_lp_h2o) / dlp_h2o
            ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
            ip1 = ip + 1
            wp = dvar - floor(dvar)
            wp1 = 1.0 - wp
         
            w00_00 = wp  * w_0_00 
            w00_01 = wp  * w_0_01 
            w00_10 = wp  * w_0_10 
            w00_11 = wp  * w_0_11 
            w01_00 = wp  * w_1_00 
            w01_01 = wp  * w_1_01 
            w01_10 = wp  * w_1_10 
            w01_11 = wp  * w_1_11 
            w10_00 = wp1 * w_0_00 
            w10_01 = wp1 * w_0_01 
            w10_10 = wp1 * w_0_10 
            w10_11 = wp1 * w_0_11 
            w11_00 = wp1 * w_1_00 
            w11_01 = wp1 * w_1_01 
            w11_10 = wp1 * w_1_10 
            w11_11 = wp1 * w_1_11 

            log_uc  = min(log10(max(uch2o * fdif, min_u_h2o)), max_lu_h2o)
            dvar = (log_uc - min_lu_h2o) / dlu_h2o
            iuc = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
            iuc1 = iuc + 1
            wuc = dvar - floor(dvar)
            wuc1 = 1.0 - wuc
!
! Asymptotic value of absorptivity as U->infinity
!
            fa = fat(1,ib) + &
                 fat(2,ib) * te1 + &
                 fat(3,ib) * te2 + &
                 fat(4,ib) * te3 + &
                 fat(5,ib) * te4 + &
                 fat(6,ib) * te5

            l_star = &
                 ln_ah2ow(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
                 ln_ah2ow(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
                 ln_ah2ow(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
                 ln_ah2ow(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
                 ln_ah2ow(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
                 ln_ah2ow(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
                 ln_ah2ow(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
                 ln_ah2ow(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
                 ln_ah2ow(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
                 ln_ah2ow(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
                 ln_ah2ow(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
                 ln_ah2ow(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
                 ln_ah2ow(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
                 ln_ah2ow(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
                 ln_ah2ow(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
                 ln_ah2ow(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
                 ln_ah2ow(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
                 ln_ah2ow(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
                 ln_ah2ow(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
                 ln_ah2ow(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
                 ln_ah2ow(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
                 ln_ah2ow(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
                 ln_ah2ow(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
                 ln_ah2ow(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
                 ln_ah2ow(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
                 ln_ah2ow(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
                 ln_ah2ow(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
                 ln_ah2ow(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
                 ln_ah2ow(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
                 ln_ah2ow(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
                 ln_ah2ow(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
                 ln_ah2ow(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu 

            c_star = &
                 cn_ah2ow(ip , itp , iuc , ite , irh ) * w11_11 * wuc1 + &
                 cn_ah2ow(ip , itp , iuc , ite , irh1) * w11_10 * wuc1 + &
                 cn_ah2ow(ip , itp , iuc , ite1, irh ) * w11_01 * wuc1 + &
                 cn_ah2ow(ip , itp , iuc , ite1, irh1) * w11_00 * wuc1 + &
                 cn_ah2ow(ip , itp , iuc1, ite , irh ) * w11_11 * wuc  + &
                 cn_ah2ow(ip , itp , iuc1, ite , irh1) * w11_10 * wuc  + &
                 cn_ah2ow(ip , itp , iuc1, ite1, irh ) * w11_01 * wuc  + &
                 cn_ah2ow(ip , itp , iuc1, ite1, irh1) * w11_00 * wuc  + &
                 cn_ah2ow(ip , itp1, iuc , ite , irh ) * w10_11 * wuc1 + &
                 cn_ah2ow(ip , itp1, iuc , ite , irh1) * w10_10 * wuc1 + &
                 cn_ah2ow(ip , itp1, iuc , ite1, irh ) * w10_01 * wuc1 + &
                 cn_ah2ow(ip , itp1, iuc , ite1, irh1) * w10_00 * wuc1 + &
                 cn_ah2ow(ip , itp1, iuc1, ite , irh ) * w10_11 * wuc  + &
                 cn_ah2ow(ip , itp1, iuc1, ite , irh1) * w10_10 * wuc  + &
                 cn_ah2ow(ip , itp1, iuc1, ite1, irh ) * w10_01 * wuc  + &
                 cn_ah2ow(ip , itp1, iuc1, ite1, irh1) * w10_00 * wuc  + &
                 cn_ah2ow(ip1, itp , iuc , ite , irh ) * w01_11 * wuc1 + &
                 cn_ah2ow(ip1, itp , iuc , ite , irh1) * w01_10 * wuc1 + &
                 cn_ah2ow(ip1, itp , iuc , ite1, irh ) * w01_01 * wuc1 + &
                 cn_ah2ow(ip1, itp , iuc , ite1, irh1) * w01_00 * wuc1 + &
                 cn_ah2ow(ip1, itp , iuc1, ite , irh ) * w01_11 * wuc  + &
                 cn_ah2ow(ip1, itp , iuc1, ite , irh1) * w01_10 * wuc  + &
                 cn_ah2ow(ip1, itp , iuc1, ite1, irh ) * w01_01 * wuc  + &
                 cn_ah2ow(ip1, itp , iuc1, ite1, irh1) * w01_00 * wuc  + &
                 cn_ah2ow(ip1, itp1, iuc , ite , irh ) * w00_11 * wuc1 + &
                 cn_ah2ow(ip1, itp1, iuc , ite , irh1) * w00_10 * wuc1 + &
                 cn_ah2ow(ip1, itp1, iuc , ite1, irh ) * w00_01 * wuc1 + &
                 cn_ah2ow(ip1, itp1, iuc , ite1, irh1) * w00_00 * wuc1 + &
                 cn_ah2ow(ip1, itp1, iuc1, ite , irh ) * w00_11 * wuc  + &
                 cn_ah2ow(ip1, itp1, iuc1, ite , irh1) * w00_10 * wuc  + &
                 cn_ah2ow(ip1, itp1, iuc1, ite1, irh ) * w00_01 * wuc  + &
                 cn_ah2ow(ip1, itp1, iuc1, ite1, irh1) * w00_00 * wuc 
            abso(i,ib) = min(max(fa * (1.0 - l_star * c_star * &
                                 aer_trn_ttl(i,k1,k2,ib)), &
                             0.0_r8), 1.0_r8) 
!
! Invoke linear limit for scaling wrt u below min_u_h2o
!
            if (uvar < min_u_h2o) then
               uscl = uvar / min_u_h2o
               abso(i,ib) = abso(i,ib) * uscl
            endif

         end do
!
! Line transmission in 800-1000 and 1000-1200 cm-1 intervals
!
         do i=1,ncol
            term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*(1. + c16*dty(i))
            term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*(1. + c17*dty(i))
            term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*(1. + c26*dty(i))
            term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*(1. + c27*dty(i))
         end do
!
! 500 -  800 cm-1   h2o rotation band overlap with co2
!
         do i=1,ncol
            k21    = term7(i,1) + term8(i,1)/ &
               (1. + (c30 + c31*(dty(i)-10.)*(dty(i)-10.))*sqrtu(i))
            k22    = term7(i,2) + term8(i,2)/ &
               (1. + (c28 + c29*(dty(i)-10.))*sqrtu(i))
            tr1    = exp(-(k21*(sqrtu(i) + fc1*fwku(i))))
            tr2    = exp(-(k22*(sqrtu(i) + fc1*fwku(i))))
            tr1=tr1*aer_trn_ttl(i,k1,k2,idx_LW_0650_0800) 
!                                          ! H2O line+STRAER trn 650--800 cm-1
            tr2=tr2*aer_trn_ttl(i,k1,k2,idx_LW_0500_0650)
!                                          ! H2O line+STRAER trn 500--650 cm-1
            tr5    = exp(-((coefh(1,3) + coefh(2,3)*dtx(i))*uc1(i)))
            tr6    = exp(-((coefh(1,4) + coefh(2,4)*dtx(i))*uc1(i)))
            tr9(i)   = tr1*tr5
            tr10(i)  = tr2*tr6
            th2o(i) = tr10(i)
            trab2(i) = 0.65*tr9(i) + 0.35*tr10(i)
         end do
         if (k2 < k1) then
            do i=1,ncol
               to3h2o(i) = h2otr(i,k1)/h2otr(i,k2)
            end do
         else
            do i=1,ncol
               to3h2o(i) = h2otr(i,k2)/h2otr(i,k1)
            end do
         end if
!
! abso(i,3)   o3  9.6 micrometer band (nu3 and nu1 bands)
!
         do i=1,ncol
            dpnm(i)  = pnm(i,k1) - pnm(i,k2)
            to3co2(i) = (pnm(i,k1)*co2t(i,k1) - pnm(i,k2)*co2t(i,k2))/dpnm(i)
            te       = (to3co2(i)*r293)**.7
            dplos    = plos(i,k1) - plos(i,k2)
            dplol    = plol(i,k1) - plol(i,k2)
            u1       = 18.29*abs(dplos)/te
            u2       = .5649*abs(dplos)/te
            rphat    = dplol/dplos
            tlocal   = tint(i,k2)
            tcrfac   = sqrt(tlocal*r250)*te
            beta     = r3205*(rphat + dpfo3*tcrfac)
            realnu   = te/beta
            tmp1     = u1/sqrt(4. + u1*(1. + realnu))
            tmp2     = u2/sqrt(4. + u2*(1. + realnu))
            o3bndi    = 74.*te*log(1. + tmp1 + tmp2)
            abso(i,3) = o3bndi*to3h2o(i)*dbvtit(i,k2)
            to3(i)   = 1.0/(1. + 0.1*tmp1 + 0.1*tmp2)
         end do
!
! abso(i,4)      co2 15  micrometer band system
!
         do i=1,ncol
            sqwp      = sqrt(abs(plco2(i,k1) - plco2(i,k2)))
            et        = exp(-480./to3co2(i))
            sqti(i)   = sqrt(to3co2(i))
            rsqti     = 1./sqti(i)
            et2       = et*et
            et4       = et2*et2
            omet      = 1. - 1.5*et2
            f1co2     = 899.70*omet*(1. + 1.94774*et + 4.73486*et2)*rsqti
            f1sqwp(i) = f1co2*sqwp
            t1co2(i)  = 1./(1. + (245.18*omet*sqwp*rsqti))
            oneme     = 1. - et2
            alphat    = oneme**3*rsqti
            pi        = abs(dpnm(i))
            wco2      =  2.5221*co2vmr*pi*rga
            u7(i)     =  4.9411e4*alphat*et2*wco2
            u8        =  3.9744e4*alphat*et4*wco2
            u9        =  1.0447e5*alphat*et4*et2*wco2
            u13       = 2.8388e3*alphat*et4*wco2
            tpath     = to3co2(i)
            tlocal    = tint(i,k2)
            tcrfac    = sqrt(tlocal*r250*tpath*r300)
            posqt     = ((pnm(i,k2) + pnm(i,k1))*r2sslp + dpfco2*tcrfac)*rsqti
            rbeta7(i) = 1./(5.3228*posqt)
            rbeta8    = 1./(10.6576*posqt)
            rbeta9    = rbeta7(i)
            rbeta13   = rbeta9
            f2co2(i)  = (u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i)))) + &
               (u8   /sqrt(4. + u8*(1. + rbeta8))) + &
               (u9   /sqrt(4. + u9*(1. + rbeta9)))
            f3co2(i)  = u13/sqrt(4. + u13*(1. + rbeta13))
         end do
         if (k2 >= k1) then
            do i=1,ncol
               sqti(i) = sqrt(tlayr(i,k2))
            end do
         end if
!
         do i=1,ncol
            tmp1      = log(1. + f1sqwp(i))
            tmp2      = log(1. + f2co2(i))
            tmp3      = log(1. + f3co2(i))
            absbnd    = (tmp1 + 2.*t1co2(i)*tmp2 + 2.*tmp3)*sqti(i)
            abso(i,4) = trab2(i)*co2em(i,k2)*absbnd
            tco2(i)   = 1./(1.0+10.0*(u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i)))))
         end do
!
! Calculate absorptivity due to trace gases, abstrc
!
         call trcab( lchnk   ,ncol    ,pcols, pverp,                   &
            k1      ,k2      ,ucfc11  ,ucfc12  ,un2o0   , &
            un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
            uco221  ,uco222  ,uco223  ,bn2o0   ,bn2o1   , &
            bch4    ,to3co2  ,pnm     ,dw      ,pnew    , &
            s2c     ,uptype  ,u       ,abplnk1 ,tco2    , &
            th2o    ,to3     ,abstrc  , &
            aer_trn_ttl)
!
! Sum total absorptivity
!
         do i=1,ncol
            abstot(i,k1,k2) = abso(i,1) + abso(i,2) + &
               abso(i,3) + abso(i,4) + abstrc(i)
         end do
      end do ! do k2 = 
   end do ! do k1 = 
!
! Adjacent layer absorptivity:
!
! abso(i,1)     0 -  800 cm-1   h2o rotation band
! abso(i,1)  1200 - 2200 cm-1   h2o vibration-rotation band
! abso(i,2)   800 - 1200 cm-1   h2o window
!
! Separation between rotation and vibration-rotation dropped, so
!                only 2 slots needed for H2O absorptivity
!
! 500-800 cm^-1 H2o continuum/line overlap already included
!                in abso(i,1).  This used to be in abso(i,4)
!
! abso(i,3)   o3  9.6 micrometer band (nu3 and nu1 bands)
! abso(i,4)   co2 15  micrometer band system
!
! Nearest layer level loop
!
   do k2=pver,ntoplw,-1
      do i=1,ncol
         tbar(i,1)   = 0.5*(tint(i,k2+1) + tlayr(i,k2+1))
         emm(i,1)    = 0.5*(co2em(i,k2+1) + co2eml(i,k2))
         tbar(i,2)   = 0.5*(tlayr(i,k2+1) + tint(i,k2))
         emm(i,2)    = 0.5*(co2em(i,k2) + co2eml(i,k2))
         tbar(i,3)   = 0.5*(tbar(i,2) + tbar(i,1))
         emm(i,3)    = emm(i,1)
         tbar(i,4)   = tbar(i,3)
         emm(i,4)    = emm(i,2)
         o3emm(i,1)  = 0.5*(dbvtit(i,k2+1) + dbvtly(i,k2))
         o3emm(i,2)  = 0.5*(dbvtit(i,k2) + dbvtly(i,k2))
         o3emm(i,3)  = o3emm(i,1)
         o3emm(i,4)  = o3emm(i,2)
         temh2o(i,1) = tbar(i,1)
         temh2o(i,2) = tbar(i,2)
         temh2o(i,3) = tbar(i,1)
         temh2o(i,4) = tbar(i,2)
         dpnm(i)     = pnm(i,k2+1) - pnm(i,k2)
      end do
!
!  Weighted Planck functions for trace gases
!
      do wvl = 1,14
         do i = 1,ncol
            bplnk(wvl,i,1) = 0.5*(abplnk1(wvl,i,k2+1) + abplnk2(wvl,i,k2))
            bplnk(wvl,i,2) = 0.5*(abplnk1(wvl,i,k2) + abplnk2(wvl,i,k2))
            bplnk(wvl,i,3) = bplnk(wvl,i,1)
            bplnk(wvl,i,4) = bplnk(wvl,i,2)
         end do
      end do
      
      do i=1,ncol
         rdpnmsq    = 1./(pnmsq(i,k2+1) - pnmsq(i,k2))
         rdpnm      = 1./dpnm(i)
         p1         = .5*(pbr(i,k2) + pnm(i,k2+1))
         p2         = .5*(pbr(i,k2) + pnm(i,k2  ))
         uinpl(i,1) =  (pnmsq(i,k2+1) - p1**2)*rdpnmsq
         uinpl(i,2) = -(pnmsq(i,k2  ) - p2**2)*rdpnmsq
         uinpl(i,3) = -(pnmsq(i,k2  ) - p1**2)*rdpnmsq
         uinpl(i,4) =  (pnmsq(i,k2+1) - p2**2)*rdpnmsq
         winpl(i,1) = (.5*( pnm(i,k2+1) - pbr(i,k2)))*rdpnm
         winpl(i,2) = (.5*(-pnm(i,k2  ) + pbr(i,k2)))*rdpnm
         winpl(i,3) = (.5*( pnm(i,k2+1) + pbr(i,k2)) - pnm(i,k2  ))*rdpnm
         winpl(i,4) = (.5*(-pnm(i,k2  ) - pbr(i,k2)) + pnm(i,k2+1))*rdpnm
         tmp1       = 1./(piln(i,k2+1) - piln(i,k2))
         tmp2       = piln(i,k2+1) - pmln(i,k2)
         tmp3       = piln(i,k2  ) - pmln(i,k2)
         zinpl(i,1) = (.5*tmp2          )*tmp1
         zinpl(i,2) = (        - .5*tmp3)*tmp1
         zinpl(i,3) = (.5*tmp2 -    tmp3)*tmp1
         zinpl(i,4) = (   tmp2 - .5*tmp3)*tmp1
         pinpl(i,1) = 0.5*(p1 + pnm(i,k2+1))
         pinpl(i,2) = 0.5*(p2 + pnm(i,k2  ))
         pinpl(i,3) = 0.5*(p1 + pnm(i,k2  ))
         pinpl(i,4) = 0.5*(p2 + pnm(i,k2+1))
         if(strat_volcanic) then
           aer_pth_ngh(i) = abs(aer_mpp(i,k2)-aer_mpp(i,k2+1))
         endif
      end do
      do kn=1,4
         do i=1,ncol
            u(i)     = uinpl(i,kn)*abs(plh2o(i,k2) - plh2o(i,k2+1))
            sqrtu(i) = sqrt(u(i))
            dw(i)    = abs(w(i,k2) - w(i,k2+1))
            pnew(i)  = u(i)/(winpl(i,kn)*dw(i))
            pnew_mks  = pnew(i) * sslp_mks
            t_p = min(max(tbar(i,kn), min_tp_h2o), max_tp_h2o)
            iest = floor(t_p) - min_tp_h2o
            esx = estblh2o(iest) + (estblh2o(iest+1)-estblh2o(iest)) * &
                 (t_p - min_tp_h2o - iest)
            qsx = epsilo * esx / (pnew_mks - omeps * esx)
            q_path = dw(i) / ABS(dpnm(i)) / rga
            
            ds2c     = abs(s2c(i,k2) - s2c(i,k2+1))
            uc1(i)   = uinpl(i,kn)*ds2c
            pch2o    = uc1(i)
            uc1(i)   = (uc1(i) + 1.7e-3*u(i))*(1. +  2.*uc1(i))/(1. + 15.*uc1(i))
            dtx(i)      = temh2o(i,kn) - 250.
            dty(i)      = tbar(i,kn) - 250.
            
            fwk(i)    = fwcoef + fwc1/(1. + fwc2*u(i))
            fwku(i)   = fwk(i)*u(i)

            if(strat_volcanic) then
              aer_pth_dlt=uinpl(i,kn)*aer_pth_ngh(i)
  
              do bnd_idx=1,bnd_nbr_LW
                 odap_aer_ttl=abs_cff_mss_aer(bnd_idx) * aer_pth_dlt 
                 aer_trn_ngh(i,bnd_idx)=exp(-fdif * odap_aer_ttl)
              end do
            else
              aer_trn_ngh(i,:) = 1.0
            endif

!
! Define variables for C/H/E (now C/LT/E) fit
!
! abso(i,1)     0 -  800 cm-1   h2o rotation band
! abso(i,1)  1200 - 2200 cm-1   h2o vibration-rotation band
! abso(i,2)   800 - 1200 cm-1   h2o window
!
! Separation between rotation and vibration-rotation dropped, so
!                only 2 slots needed for H2O absorptivity
!
! Notation:
! U   = integral (P/P_0 dW)  
! P   = atmospheric pressure
! P_0 = reference atmospheric pressure
! W   = precipitable water path
! T_e = emission temperature
! T_p = path temperature
! RH  = path relative humidity
!
!
! Terms for asymptotic value of emissivity
!
            te1  = temh2o(i,kn)
            te2  = te1 * te1
            te3  = te2 * te1
            te4  = te3 * te1
            te5  = te4 * te1

!
! Indices for lines and continuum tables 
! Note: because we are dealing with the nearest layer,
!       the Hulst-Curtis-Godson corrections
!       for inhomogeneous paths are not applied.
!
            uvar = u(i)*fdif
            log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
            dvar = (log_u - min_lu_h2o) / dlu_h2o
            iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
            iu1 = iu + 1
            wu = dvar - floor(dvar)
            wu1 = 1.0 - wu
            
            log_p  = min(log10(max(pnew(i), min_p_h2o)), max_lp_h2o)
            dvar = (log_p - min_lp_h2o) / dlp_h2o
            ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
            ip1 = ip + 1
            wp = dvar - floor(dvar)
            wp1 = 1.0 - wp
            
            dvar = (t_p - min_tp_h2o) / dtp_h2o
            itp = min(max(int(aint(dvar,r8)) + 1, 1), n_tp - 1)
            itp1 = itp + 1
            wtp = dvar - floor(dvar)
            wtp1 = 1.0 - wtp
            
            t_e = min(max(temh2o(i,kn)-t_p,min_te_h2o),max_te_h2o)
            dvar = (t_e - min_te_h2o) / dte_h2o
            ite = min(max(int(aint(dvar,r8)) + 1, 1), n_te - 1)
            ite1 = ite + 1
            wte = dvar - floor(dvar)
            wte1 = 1.0 - wte
            
            rh_path = min(max(q_path / qsx, min_rh_h2o), max_rh_h2o)
            dvar = (rh_path - min_rh_h2o) / drh_h2o
            irh = min(max(int(aint(dvar,r8)) + 1, 1), n_rh - 1)
            irh1 = irh + 1
            wrh = dvar - floor(dvar)
            wrh1 = 1.0 - wrh
            
            w_0_0_ = wtp  * wte
            w_0_1_ = wtp  * wte1
            w_1_0_ = wtp1 * wte 
            w_1_1_ = wtp1 * wte1
            
            w_0_00 = w_0_0_ * wrh
            w_0_01 = w_0_0_ * wrh1
            w_0_10 = w_0_1_ * wrh
            w_0_11 = w_0_1_ * wrh1
            w_1_00 = w_1_0_ * wrh
            w_1_01 = w_1_0_ * wrh1
            w_1_10 = w_1_1_ * wrh
            w_1_11 = w_1_1_ * wrh1
            
            w00_00 = wp  * w_0_00 
            w00_01 = wp  * w_0_01 
            w00_10 = wp  * w_0_10 
            w00_11 = wp  * w_0_11 
            w01_00 = wp  * w_1_00 
            w01_01 = wp  * w_1_01 
            w01_10 = wp  * w_1_10 
            w01_11 = wp  * w_1_11 
            w10_00 = wp1 * w_0_00 
            w10_01 = wp1 * w_0_01 
            w10_10 = wp1 * w_0_10 
            w10_11 = wp1 * w_0_11 
            w11_00 = wp1 * w_1_00 
            w11_01 = wp1 * w_1_01 
            w11_10 = wp1 * w_1_10 
            w11_11 = wp1 * w_1_11 

!
! Non-window absorptivity
!
            ib = 1
            
            fa = fat(1,ib) + &
                 fat(2,ib) * te1 + &
                 fat(3,ib) * te2 + &
                 fat(4,ib) * te3 + &
                 fat(5,ib) * te4 + &
                 fat(6,ib) * te5
            
            a_star = &
                 ah2onw(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
                 ah2onw(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
                 ah2onw(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
                 ah2onw(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
                 ah2onw(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
                 ah2onw(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
                 ah2onw(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
                 ah2onw(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
                 ah2onw(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
                 ah2onw(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
                 ah2onw(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
                 ah2onw(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
                 ah2onw(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
                 ah2onw(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
                 ah2onw(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
                 ah2onw(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
                 ah2onw(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
                 ah2onw(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
                 ah2onw(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
                 ah2onw(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
                 ah2onw(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
                 ah2onw(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
                 ah2onw(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
                 ah2onw(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
                 ah2onw(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
                 ah2onw(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
                 ah2onw(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
                 ah2onw(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu
            
            abso(i,ib) = min(max(fa * (1.0 - (1.0 - a_star) * &
                                 aer_trn_ngh(i,ib)), &
                             0.0_r8), 1.0_r8)

!
! Invoke linear limit for scaling wrt u below min_u_h2o
!
            if (uvar < min_u_h2o) then
               uscl = uvar / min_u_h2o
               abso(i,ib) = abso(i,ib) * uscl
            endif
            
!
! Window absorptivity
!
            ib = 2
            
            fa = fat(1,ib) + &
                 fat(2,ib) * te1 + &
                 fat(3,ib) * te2 + &
                 fat(4,ib) * te3 + &
                 fat(5,ib) * te4 + &
                 fat(6,ib) * te5
            
            a_star = &
                 ah2ow(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
                 ah2ow(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
                 ah2ow(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
                 ah2ow(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
                 ah2ow(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
                 ah2ow(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
                 ah2ow(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
                 ah2ow(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
                 ah2ow(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
                 ah2ow(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
                 ah2ow(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
                 ah2ow(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
                 ah2ow(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
                 ah2ow(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
                 ah2ow(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
                 ah2ow(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
                 ah2ow(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
                 ah2ow(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
                 ah2ow(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
                 ah2ow(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
                 ah2ow(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
                 ah2ow(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
                 ah2ow(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
                 ah2ow(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
                 ah2ow(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
                 ah2ow(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
                 ah2ow(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
                 ah2ow(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
                 ah2ow(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
                 ah2ow(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
                 ah2ow(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
                 ah2ow(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu
            
            abso(i,ib) = min(max(fa * (1.0 - (1.0 - a_star) * &
                                 aer_trn_ngh(i,ib)), &
                             0.0_r8), 1.0_r8)

!
! Invoke linear limit for scaling wrt u below min_u_h2o
!
            if (uvar < min_u_h2o) then
               uscl = uvar / min_u_h2o
               abso(i,ib) = abso(i,ib) * uscl
            endif
            
         end do
!
! Line transmission in 800-1000 and 1000-1200 cm-1 intervals
!
         do i=1,ncol
            term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*(1. + c16*dty(i))
            term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*(1. + c17*dty(i))
            term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*(1. + c26*dty(i))
            term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*(1. + c27*dty(i))
         end do
!
! 500 -  800 cm-1   h2o rotation band overlap with co2
!
         do i=1,ncol
            dtym10     = dty(i) - 10.
            denom      = 1. + (c30 + c31*dtym10*dtym10)*sqrtu(i)
            k21        = term7(i,1) + term8(i,1)/denom
            denom      = 1. + (c28 + c29*dtym10       )*sqrtu(i)
            k22        = term7(i,2) + term8(i,2)/denom
            tr1     = exp(-(k21*(sqrtu(i) + fc1*fwku(i))))
            tr2     = exp(-(k22*(sqrtu(i) + fc1*fwku(i))))
            tr1=tr1*aer_trn_ngh(i,idx_LW_0650_0800) 
!                                         ! H2O line+STRAER trn 650--800 cm-1
            tr2=tr2*aer_trn_ngh(i,idx_LW_0500_0650) 
!                                         ! H2O line+STRAER trn 500--650 cm-1
            tr5     = exp(-((coefh(1,3) + coefh(2,3)*dtx(i))*uc1(i)))
            tr6     = exp(-((coefh(1,4) + coefh(2,4)*dtx(i))*uc1(i)))
            tr9(i)  = tr1*tr5
            tr10(i) = tr2*tr6
            trab2(i)= 0.65*tr9(i) + 0.35*tr10(i)
            th2o(i) = tr10(i)
         end do
!
! abso(i,3)  o3  9.6 micrometer (nu3 and nu1 bands)
!
         do i=1,ncol
            te        = (tbar(i,kn)*r293)**.7
            dplos     = abs(plos(i,k2+1) - plos(i,k2))
            u1        = zinpl(i,kn)*18.29*dplos/te
            u2        = zinpl(i,kn)*.5649*dplos/te
            tlocal    = tbar(i,kn)
            tcrfac    = sqrt(tlocal*r250)*te
            beta      = r3205*(pinpl(i,kn)*rsslp + dpfo3*tcrfac)
            realnu    = te/beta
            tmp1      = u1/sqrt(4. + u1*(1. + realnu))
            tmp2      = u2/sqrt(4. + u2*(1. + realnu))
            o3bndi    = 74.*te*log(1. + tmp1 + tmp2)
            abso(i,3) = o3bndi*o3emm(i,kn)*(h2otr(i,k2+1)/h2otr(i,k2))
            to3(i)    = 1.0/(1. + 0.1*tmp1 + 0.1*tmp2)
         end do
!
! abso(i,4)   co2 15  micrometer band system
!
         do i=1,ncol
            dplco2   = plco2(i,k2+1) - plco2(i,k2)
            sqwp     = sqrt(uinpl(i,kn)*dplco2)
            et       = exp(-480./tbar(i,kn))
            sqti(i)  = sqrt(tbar(i,kn))
            rsqti    = 1./sqti(i)
            et2      = et*et
            et4      = et2*et2
            omet     = (1. - 1.5*et2)
            f1co2    = 899.70*omet*(1. + 1.94774*et + 4.73486*et2)*rsqti
            f1sqwp(i)= f1co2*sqwp
            t1co2(i) = 1./(1. + (245.18*omet*sqwp*rsqti))
            oneme    = 1. - et2
            alphat   = oneme**3*rsqti
            pi       = abs(dpnm(i))*winpl(i,kn)
            wco2     = 2.5221*co2vmr*pi*rga
            u7(i)    = 4.9411e4*alphat*et2*wco2
            u8       = 3.9744e4*alphat*et4*wco2
            u9       = 1.0447e5*alphat*et4*et2*wco2
            u13      = 2.8388e3*alphat*et4*wco2
            tpath    = tbar(i,kn)
            tlocal   = tbar(i,kn)
            tcrfac   = sqrt((tlocal*r250)*(tpath*r300))
            posqt    = (pinpl(i,kn)*rsslp + dpfco2*tcrfac)*rsqti
            rbeta7(i)= 1./(5.3228*posqt)
            rbeta8   = 1./(10.6576*posqt)
            rbeta9   = rbeta7(i)
            rbeta13  = rbeta9
            f2co2(i) = u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i))) + &
                 u8   /sqrt(4. + u8*(1. + rbeta8)) + &
                 u9   /sqrt(4. + u9*(1. + rbeta9))
            f3co2(i) = u13/sqrt(4. + u13*(1. + rbeta13))
            tmp1     = log(1. + f1sqwp(i))
            tmp2     = log(1. + f2co2(i))
            tmp3     = log(1. + f3co2(i))
            absbnd   = (tmp1 + 2.*t1co2(i)*tmp2 + 2.*tmp3)*sqti(i)
            abso(i,4)= trab2(i)*emm(i,kn)*absbnd
            tco2(i)  = 1.0/(1.0+ 10.0*u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i))))
         end do ! do i =
!
! Calculate trace gas absorptivity for nearest layer, abstrc
!
         call trcabn(lchnk   ,ncol    ,pcols, pverp,                   &
              k2      ,kn      ,ucfc11  ,ucfc12  ,un2o0   , &
              un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
              uco221  ,uco222  ,uco223  ,tbar    ,bplnk   , &
              winpl   ,pinpl   ,tco2    ,th2o    ,to3     , &
              uptype  ,dw      ,s2c     ,u       ,pnew    , &
              abstrc  ,uinpl   , &
              aer_trn_ngh)
!
! Total next layer absorptivity:
!
         do i=1,ncol
            absnxt(i,k2,kn) = abso(i,1) + abso(i,2) + &
                 abso(i,3) + abso(i,4) + abstrc(i)
         end do
      end do ! do kn =
   end do ! do k2 =

   return
end subroutine radabs



subroutine radems(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  s2c     ,tcg     ,w       ,tplnke  ,plh2o   , &
                  pnm     ,plco2   ,tint    ,tint4   ,tlayr   , &
                  tlayr4  ,plol    ,plos    ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,uch4    ,uco211 ,uco212   , &
                  uco213  ,uco221  ,uco222  ,uco223  ,uptype  , &
                  bn2o0   ,bn2o1   ,bch4    ,co2em   ,co2eml  , &
                  co2t    ,h2otr   ,abplnk1 ,abplnk2 ,emstot  , &
                  plh2ob  ,wb      , &
                  aer_trn_ttl)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Compute emissivity for H2O, CO2, O3, CH4, N2O, CFC11 and CFC12
! 
! Method: 
! H2O  ....  Uses nonisothermal emissivity method for water vapor from
!            Ramanathan, V. and  P.Downey, 1986: A Nonisothermal
!            Emissivity and Absorptivity Formulation for Water Vapor
!            Jouranl of Geophysical Research, vol. 91., D8, pp 8649-8666
!
!            Implementation updated by Collins,Hackney, and Edwards 2001
!               using line-by-line calculations based upon Hitran 1996 and
!               CKD 2.1 for absorptivity and emissivity
!
!            Implementation updated by Collins, Lee-Taylor, and Edwards (2003)
!               using line-by-line calculations based upon Hitran 2000 and
!               CKD 2.4 for absorptivity and emissivity
!
! CO2  ....  Uses absorptance parameterization of the 15 micro-meter
!            (500 - 800 cm-1) band system of Carbon Dioxide, from
!            Kiehl, J.T. and B.P.Briegleb, 1991: A New Parameterization
!            of the Absorptance Due to the 15 micro-meter Band System
!            of Carbon Dioxide Jouranl of Geophysical Research,
!            vol. 96., D5, pp 9013-9019. Also includes the effects
!            of the 9.4 and 10.4 micron bands of CO2.
!
! O3   ....  Uses absorptance parameterization of the 9.6 micro-meter
!            band system of ozone, from Ramanathan, V. and R. Dickinson,
!            1979: The Role of stratospheric ozone in the zonal and
!            seasonal radiative energy balance of the earth-troposphere
!            system. Journal of the Atmospheric Sciences, Vol. 36,
!            pp 1084-1104
!
! ch4  ....  Uses a broad band model for the 7.7 micron band of methane.
!
! n20  ....  Uses a broad band model for the 7.8, 8.6 and 17.0 micron
!            bands of nitrous oxide
!
! cfc11 ...  Uses a quasi-linear model for the 9.2, 10.7, 11.8 and 12.5
!            micron bands of CFC11
!
! cfc12 ...  Uses a quasi-linear model for the 8.6, 9.1, 10.8 and 11.2
!            micron bands of CFC12
!
!
! Computes individual emissivities, accounting for band overlap, and
! sums to obtain the total.
!
! Author: W. Collins (H2O emissivity) and J. Kiehl
! 
!-----------------------------------------------------------------------
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: lchnk                    ! chunk identifier
   integer, intent(in) :: ncol                     ! number of atmospheric columns
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: s2c(pcols,pverp)        ! H2o continuum path length
   real(r8), intent(in) :: tcg(pcols,pverp)        ! H2o-mass-wgted temp. (Curtis-Godson approx.)
   real(r8), intent(in) :: w(pcols,pverp)          ! H2o path length
   real(r8), intent(in) :: tplnke(pcols)           ! Layer planck temperature
   real(r8), intent(in) :: plh2o(pcols,pverp)      ! H2o prs wghted path length
   real(r8), intent(in) :: pnm(pcols,pverp)        ! Model interface pressure
   real(r8), intent(in) :: plco2(pcols,pverp)      ! Prs wghted path of co2
   real(r8), intent(in) :: tint(pcols,pverp)       ! Model interface temperatures
   real(r8), intent(in) :: tint4(pcols,pverp)      ! Tint to the 4th power
   real(r8), intent(in) :: tlayr(pcols,pverp)      ! K-1 model layer temperature
   real(r8), intent(in) :: tlayr4(pcols,pverp)     ! Tlayr to the 4th power
   real(r8), intent(in) :: plol(pcols,pverp)       ! Pressure wghtd ozone path
   real(r8), intent(in) :: plos(pcols,pverp)       ! Ozone path
   real(r8), intent(in) :: plh2ob(nbands,pcols,pverp) ! Pressure weighted h2o path with 
                                                      !    Hulst-Curtis-Godson temp. factor 
                                                      !    for H2O bands 
   real(r8), intent(in) :: wb(nbands,pcols,pverp)     ! H2o path length with 
                                                      !    Hulst-Curtis-Godson temp. factor 
                                                      !    for H2O bands 

   real(r8), intent(in) :: aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) 
!                               ! [fraction] Total strat. aerosol
!                               ! transmission between interfaces k1 and k2  

!
! Trace gas variables
!
   real(r8), intent(in) :: ucfc11(pcols,pverp)     ! CFC11 path length
   real(r8), intent(in) :: ucfc12(pcols,pverp)     ! CFC12 path length
   real(r8), intent(in) :: un2o0(pcols,pverp)      ! N2O path length
   real(r8), intent(in) :: un2o1(pcols,pverp)      ! N2O path length (hot band)
   real(r8), intent(in) :: uch4(pcols,pverp)       ! CH4 path length
   real(r8), intent(in) :: uco211(pcols,pverp)     ! CO2 9.4 micron band path length
   real(r8), intent(in) :: uco212(pcols,pverp)     ! CO2 9.4 micron band path length
   real(r8), intent(in) :: uco213(pcols,pverp)     ! CO2 9.4 micron band path length
   real(r8), intent(in) :: uco221(pcols,pverp)     ! CO2 10.4 micron band path length
   real(r8), intent(in) :: uco222(pcols,pverp)     ! CO2 10.4 micron band path length
   real(r8), intent(in) :: uco223(pcols,pverp)     ! CO2 10.4 micron band path length
   real(r8), intent(in) :: bn2o0(pcols,pverp)      ! pressure factor for n2o
   real(r8), intent(in) :: bn2o1(pcols,pverp)      ! pressure factor for n2o
   real(r8), intent(in) :: bch4(pcols,pverp)       ! pressure factor for ch4
   real(r8), intent(in) :: uptype(pcols,pverp)     ! p-type continuum path length
!
! Output arguments
!
   real(r8), intent(out) :: emstot(pcols,pverp)     ! Total emissivity
   real(r8), intent(out) :: co2em(pcols,pverp)      ! Layer co2 normalzd plnck funct drvtv
   real(r8), intent(out) :: co2eml(pcols,pver)      ! Intrfc co2 normalzd plnck func drvtv
   real(r8), intent(out) :: co2t(pcols,pverp)       ! Tmp and prs weighted path length
   real(r8), intent(out) :: h2otr(pcols,pverp)      ! H2o transmission over o3 band
   real(r8), intent(out) :: abplnk1(14,pcols,pverp) ! non-nearest layer Plack factor
   real(r8), intent(out) :: abplnk2(14,pcols,pverp) ! nearest layer factor

!
!---------------------------Local variables-----------------------------
!
   integer i                    ! Longitude index
   integer k                    ! Level index]
   integer k1                   ! Level index
!
! Local variables for H2O:
!
   real(r8) h2oems(pcols,pverp)     ! H2o emissivity
   real(r8) tpathe                  ! Used to compute h2o emissivity
   real(r8) dtx(pcols)              ! Planck temperature minus 250 K
   real(r8) dty(pcols)              ! Path temperature minus 250 K
!
! The 500-800 cm^-1 emission in emis(i,4) has been combined
!              into the 0-800 cm^-1 emission in emis(i,1)
!
   real(r8) emis(pcols,2)           ! H2O emissivity 
!
!
!
   real(r8) term7(pcols,2)          ! Kl_inf(i) in eq(r8) of table A3a of R&D
   real(r8) term8(pcols,2)          ! Delta kl_inf(i) in eq(r8)
   real(r8) tr1(pcols)              ! Equation(6) in table A2 for 650-800
   real(r8) tr2(pcols)              ! Equation(6) in table A2 for 500-650
   real(r8) tr3(pcols)              ! Equation(4) in table A2 for 650-800
   real(r8) tr4(pcols)              ! Equation(4),table A2 of R&D for 500-650
   real(r8) tr7(pcols)              ! Equation (6) times eq(4) in table A2
!                                      of R&D for 650-800 cm-1 region
   real(r8) tr8(pcols)              ! Equation (6) times eq(4) in table A2
!                                      of R&D for 500-650 cm-1 region
   real(r8) k21(pcols)              ! Exponential coefficient used to calc
!                                     rot band transmissivity in the 650-800
!                                     cm-1 region (tr1)
   real(r8) k22(pcols)              ! Exponential coefficient used to calc
!                                     rot band transmissivity in the 500-650
!                                     cm-1 region (tr2)
   real(r8) u(pcols)                ! Pressure weighted H2O path length
   real(r8) ub(nbands)              ! Pressure weighted H2O path length with
                                    !  Hulst-Curtis-Godson correction for
                                    !  each band
   real(r8) pnew                    ! Effective pressure for h2o linewidth
   real(r8) pnewb(nbands)           ! Effective pressure for h2o linewidth w/
                                    !  Hulst-Curtis-Godson correction for
                                    !  each band
   real(r8) uc1(pcols)              ! H2o continuum pathlength 500-800 cm-1
   real(r8) fwk                     ! Equation(33) in R&D far wing correction
   real(r8) troco2(pcols,pverp)     ! H2o overlap factor for co2 absorption
   real(r8) emplnk(14,pcols)        ! emissivity Planck factor
   real(r8) emstrc(pcols,pverp)     ! total trace gas emissivity
!
! Local variables for CO2:
!
   real(r8) co2ems(pcols,pverp)      ! Co2 emissivity
   real(r8) co2plk(pcols)            ! Used to compute co2 emissivity
   real(r8) sum(pcols)               ! Used to calculate path temperature
   real(r8) t1i                      ! Co2 hot band temperature factor
   real(r8) sqti                     ! Sqrt of temperature
   real(r8) pi                       ! Pressure used in co2 mean line width
   real(r8) et                       ! Co2 hot band factor
   real(r8) et2                      ! Co2 hot band factor
   real(r8) et4                      ! Co2 hot band factor
   real(r8) omet                     ! Co2 stimulated emission term
   real(r8) ex                       ! Part of co2 planck function
   real(r8) f1co2                    ! Co2 weak band factor
   real(r8) f2co2                    ! Co2 weak band factor
   real(r8) f3co2                    ! Co2 weak band factor
   real(r8) t1co2                    ! Overlap factor weak bands strong band
   real(r8) sqwp                     ! Sqrt of co2 pathlength
   real(r8) f1sqwp                   ! Main co2 band factor
   real(r8) oneme                    ! Co2 stimulated emission term
   real(r8) alphat                   ! Part of the co2 stimulated emiss term
   real(r8) wco2                     ! Consts used to define co2 pathlength
   real(r8) posqt                    ! Effective pressure for co2 line width
   real(r8) rbeta7                   ! Inverse of co2 hot band line width par
   real(r8) rbeta8                   ! Inverse of co2 hot band line width par
   real(r8) rbeta9                   ! Inverse of co2 hot band line width par
   real(r8) rbeta13                  ! Inverse of co2 hot band line width par
   real(r8) tpath                    ! Path temp used in co2 band model
   real(r8) tmp1                     ! Co2 band factor
   real(r8) tmp2                     ! Co2 band factor
   real(r8) tmp3                     ! Co2 band factor
   real(r8) tlayr5                   ! Temperature factor in co2 Planck func
   real(r8) rsqti                    ! Reciprocal of sqrt of temperature
   real(r8) exm1sq                   ! Part of co2 Planck function
   real(r8) u7                       ! Absorber amt for various co2 band systems
   real(r8) u8                       ! Absorber amt for various co2 band systems
   real(r8) u9                       ! Absorber amt for various co2 band systems
   real(r8) u13                      ! Absorber amt for various co2 band systems
   real(r8) r250                     ! Inverse 250K
   real(r8) r300                     ! Inverse 300K
   real(r8) rsslp                    ! Inverse standard sea-level pressure
!
! Local variables for O3:
!
   real(r8) o3ems(pcols,pverp)       ! Ozone emissivity
   real(r8) dbvtt(pcols)             ! Tmp drvtv of planck fctn for tplnke
   real(r8) dbvt,fo3,t,ux,vx
   real(r8) te                       ! Temperature factor
   real(r8) u1                       ! Path length factor
   real(r8) u2                       ! Path length factor
   real(r8) phat                     ! Effecitive path length pressure
   real(r8) tlocal                   ! Local planck function temperature
   real(r8) tcrfac                   ! Scaled temperature factor
   real(r8) beta                     ! Absorption funct factor voigt effect
   real(r8) realnu                   ! Absorption function factor
   real(r8) o3bndi                   ! Band absorption factor
!
! Transmission terms for various spectral intervals:
!
   real(r8) absbnd                   ! Proportional to co2 band absorptance
   real(r8) tco2(pcols)              ! co2 overlap factor
   real(r8) th2o(pcols)              ! h2o overlap factor
   real(r8) to3(pcols)               ! o3 overlap factor
!
! Variables for new H2O parameterization
!
! Notation:
! U   = integral (P/P_0 dW)  eq. 15 in Ramanathan/Downey 1986
! P   = atmospheric pressure
! P_0 = reference atmospheric pressure
! W   = precipitable water path
! T_e = emission temperature
! T_p = path temperature
! RH  = path relative humidity
!
   real(r8) fe               ! asymptotic value of emis. as U->infinity
   real(r8) e_star           ! normalized non-window emissivity
   real(r8) l_star           ! interpolated line transmission
   real(r8) c_star           ! interpolated continuum transmission

   real(r8) te1              ! emission temperature
   real(r8) te2              ! te^2
   real(r8) te3              ! te^3
   real(r8) te4              ! te^4
   real(r8) te5              ! te^5

   real(r8) log_u            ! log base 10 of U 
   real(r8) log_uc           ! log base 10 of H2O continuum path
   real(r8) log_p            ! log base 10 of P
   real(r8) t_p              ! T_p
   real(r8) t_e              ! T_e (offset by T_p)

   integer iu                ! index for log10(U)
   integer iu1               ! iu + 1
   integer iuc               ! index for log10(H2O continuum path)
   integer iuc1              ! iuc + 1
   integer ip                ! index for log10(P)
   integer ip1               ! ip + 1
   integer itp               ! index for T_p
   integer itp1              ! itp + 1
   integer ite               ! index for T_e
   integer ite1              ! ite + 1
   integer irh               ! index for RH
   integer irh1              ! irh + 1

   real(r8) dvar             ! normalized variation in T_p/T_e/P/U
   real(r8) uvar             ! U * diffusivity factor
   real(r8) uscl             ! factor for lineary scaling as U->0

   real(r8) wu               ! weight for U
   real(r8) wu1              ! 1 - wu
   real(r8) wuc              ! weight for H2O continuum path
   real(r8) wuc1             ! 1 - wuc
   real(r8) wp               ! weight for P
   real(r8) wp1              ! 1 - wp
   real(r8) wtp              ! weight for T_p
   real(r8) wtp1             ! 1 - wtp
   real(r8) wte              ! weight for T_e
   real(r8) wte1             ! 1 - wte
   real(r8) wrh              ! weight for RH
   real(r8) wrh1             ! 1 - wrh

   real(r8) w_0_0_           ! weight for Tp/Te combination
   real(r8) w_0_1_           ! weight for Tp/Te combination
   real(r8) w_1_0_           ! weight for Tp/Te combination
   real(r8) w_1_1_           ! weight for Tp/Te combination

   real(r8) w_0_00           ! weight for Tp/Te/RH combination
   real(r8) w_0_01           ! weight for Tp/Te/RH combination
   real(r8) w_0_10           ! weight for Tp/Te/RH combination
   real(r8) w_0_11           ! weight for Tp/Te/RH combination
   real(r8) w_1_00           ! weight for Tp/Te/RH combination
   real(r8) w_1_01           ! weight for Tp/Te/RH combination
   real(r8) w_1_10           ! weight for Tp/Te/RH combination
   real(r8) w_1_11           ! weight for Tp/Te/RH combination

   real(r8) w00_00           ! weight for P/Tp/Te/RH combination
   real(r8) w00_01           ! weight for P/Tp/Te/RH combination
   real(r8) w00_10           ! weight for P/Tp/Te/RH combination
   real(r8) w00_11           ! weight for P/Tp/Te/RH combination
   real(r8) w01_00           ! weight for P/Tp/Te/RH combination
   real(r8) w01_01           ! weight for P/Tp/Te/RH combination
   real(r8) w01_10           ! weight for P/Tp/Te/RH combination
   real(r8) w01_11           ! weight for P/Tp/Te/RH combination
   real(r8) w10_00           ! weight for P/Tp/Te/RH combination
   real(r8) w10_01           ! weight for P/Tp/Te/RH combination
   real(r8) w10_10           ! weight for P/Tp/Te/RH combination
   real(r8) w10_11           ! weight for P/Tp/Te/RH combination
   real(r8) w11_00           ! weight for P/Tp/Te/RH combination
   real(r8) w11_01           ! weight for P/Tp/Te/RH combination
   real(r8) w11_10           ! weight for P/Tp/Te/RH combination
   real(r8) w11_11           ! weight for P/Tp/Te/RH combination

   integer ib                ! spectral interval:
                             !   1 = 0-800 cm^-1 and 1200-2200 cm^-1
                             !   2 = 800-1200 cm^-1

   real(r8) pch2o            ! H2O continuum path
   real(r8) fch2o            ! temp. factor for continuum
   real(r8) uch2o            ! U corresponding to H2O cont. path (window)

   real(r8) fdif             ! secant(zenith angle) for diffusivity approx.

   real(r8) sslp_mks         ! Sea-level pressure in MKS units
   real(r8) esx              ! saturation vapor pressure returned by vqsatd
   real(r8) qsx              ! saturation mixing ratio returned by vqsatd
   real(r8) pnew_mks         ! pnew in MKS units
   real(r8) q_path           ! effective specific humidity along path
   real(r8) rh_path          ! effective relative humidity along path
   real(r8) omeps            ! 1 - epsilo

   integer  iest             ! index in estblh2o

!
!---------------------------Statement functions-------------------------
!
! Derivative of planck function at 9.6 micro-meter wavelength, and
! an absorption function factor:
!
!
   dbvt(t)=(-2.8911366682e-4+(2.3771251896e-6+1.1305188929e-10*t)*t)/ &
           (1.0+(-6.1364820707e-3+1.5550319767e-5*t)*t)
!
   fo3(ux,vx)=ux/sqrt(4.+ux*(1.+vx))
!
!
!
!-----------------------------------------------------------------------
!
! Initialize
!
   r250  = 1./250.
   r300  = 1./300.
   rsslp = 1./sslp
!
! Constants for computing U corresponding to H2O cont. path
!
   fdif       = 1.66
   sslp_mks   = sslp / 10.0
   omeps      = 1.0 - epsilo
!
! Planck function for co2
!
   do i=1,ncol
      ex             = exp(960./tplnke(i))
      co2plk(i)      = 5.e8/((tplnke(i)**4)*(ex - 1.))
      co2t(i,ntoplw) = tplnke(i)
      sum(i)         = co2t(i,ntoplw)*pnm(i,ntoplw)
   end do
   k = ntoplw
   do k1=pverp,ntoplw+1,-1
      k = k + 1
      do i=1,ncol
         sum(i)         = sum(i) + tlayr(i,k)*(pnm(i,k)-pnm(i,k-1))
         ex             = exp(960./tlayr(i,k1))
         tlayr5         = tlayr(i,k1)*tlayr4(i,k1)
         co2eml(i,k1-1) = 1.2e11*ex/(tlayr5*(ex - 1.)**2)
         co2t(i,k)      = sum(i)/pnm(i,k)
      end do
   end do
!
! Initialize planck function derivative for O3
!
   do i=1,ncol
      dbvtt(i) = dbvt(tplnke(i))
   end do
!
! Calculate trace gas Planck functions
!
   call trcplk(lchnk   ,ncol    ,pcols, pver, pverp,         &
               tint    ,tlayr   ,tplnke  ,emplnk  ,abplnk1 , &
               abplnk2 )
!
! Interface loop
!
   do k1=ntoplw,pverp
!
! H2O emissivity
!
! emis(i,1)     0 -  800 cm-1   h2o rotation band
! emis(i,1)  1200 - 2200 cm-1   h2o vibration-rotation band
! emis(i,2)   800 - 1200 cm-1   h2o window
!
! Separation between rotation and vibration-rotation dropped, so
!                only 2 slots needed for H2O emissivity
!
!      emis(i,3)   = 0.0
!
! For the p type continuum
!
      do i=1,ncol
         u(i)        = plh2o(i,k1)
         pnew        = u(i)/w(i,k1)
         pnew_mks    = pnew * sslp_mks
!
! Apply scaling factor for 500-800 continuum
!
         uc1(i)      = (s2c(i,k1) + 1.7e-3*plh2o(i,k1))*(1. + 2.*s2c(i,k1))/ &
                       (1. + 15.*s2c(i,k1))
         pch2o       = s2c(i,k1)
!
! Changed effective path temperature to std. Curtis-Godson form
!
         tpathe   = tcg(i,k1)/w(i,k1)
         t_p = min(max(tpathe, min_tp_h2o), max_tp_h2o)
         iest = floor(t_p) - min_tp_h2o
         esx = estblh2o(iest) + (estblh2o(iest+1)-estblh2o(iest)) * &
               (t_p - min_tp_h2o - iest)
         qsx = epsilo * esx / (pnew_mks - omeps * esx)
!
! Compute effective RH along path
!
         q_path = w(i,k1) / pnm(i,k1) / rga
!
! Calculate effective u, pnew for each band using
!        Hulst-Curtis-Godson approximation:
! Formulae: Goody and Yung, Atmospheric Radiation: Theoretical Basis, 
!           2nd edition, Oxford University Press, 1989.
! Effective H2O path (w)
!      eq. 6.24, p. 228
! Effective H2O path pressure (pnew = u/w):
!      eq. 6.29, p. 228
!
         ub(1) = plh2ob(1,i,k1) / psi(t_p,1)
         ub(2) = plh2ob(2,i,k1) / psi(t_p,2)

         pnewb(1) = ub(1) / wb(1,i,k1) * phi(t_p,1)
         pnewb(2) = ub(2) / wb(2,i,k1) * phi(t_p,2)
!
!
!
         dtx(i) = tplnke(i) - 250.
         dty(i) = tpathe - 250.
!
! Define variables for C/H/E (now C/LT/E) fit
!
! emis(i,1)     0 -  800 cm-1   h2o rotation band
! emis(i,1)  1200 - 2200 cm-1   h2o vibration-rotation band
! emis(i,2)   800 - 1200 cm-1   h2o window
!
! Separation between rotation and vibration-rotation dropped, so
!                only 2 slots needed for H2O emissivity
!
! emis(i,3)   = 0.0
!
! Notation:
! U   = integral (P/P_0 dW)  
! P   = atmospheric pressure
! P_0 = reference atmospheric pressure
! W   = precipitable water path
! T_e = emission temperature
! T_p = path temperature
! RH  = path relative humidity
!
! Terms for asymptotic value of emissivity
!
         te1  = tplnke(i)
         te2  = te1 * te1
         te3  = te2 * te1
         te4  = te3 * te1
         te5  = te4 * te1
!
! Band-independent indices for lines and continuum tables
!
         dvar = (t_p - min_tp_h2o) / dtp_h2o
         itp = min(max(int(aint(dvar,r8)) + 1, 1), n_tp - 1)
         itp1 = itp + 1
         wtp = dvar - floor(dvar)
         wtp1 = 1.0 - wtp

         t_e = min(max(tplnke(i) - t_p, min_te_h2o), max_te_h2o)
         dvar = (t_e - min_te_h2o) / dte_h2o
         ite = min(max(int(aint(dvar,r8)) + 1, 1), n_te - 1)
         ite1 = ite + 1
         wte = dvar - floor(dvar)
         wte1 = 1.0 - wte

         rh_path = min(max(q_path / qsx, min_rh_h2o), max_rh_h2o)
         dvar = (rh_path - min_rh_h2o) / drh_h2o
         irh = min(max(int(aint(dvar,r8)) + 1, 1), n_rh - 1)
         irh1 = irh + 1
         wrh = dvar - floor(dvar)
         wrh1 = 1.0 - wrh

         w_0_0_ = wtp  * wte
         w_0_1_ = wtp  * wte1
         w_1_0_ = wtp1 * wte 
         w_1_1_ = wtp1 * wte1

         w_0_00 = w_0_0_ * wrh
         w_0_01 = w_0_0_ * wrh1
         w_0_10 = w_0_1_ * wrh
         w_0_11 = w_0_1_ * wrh1
         w_1_00 = w_1_0_ * wrh
         w_1_01 = w_1_0_ * wrh1
         w_1_10 = w_1_1_ * wrh
         w_1_11 = w_1_1_ * wrh1
!
! H2O Continuum path for 0-800 and 1200-2200 cm^-1
!
!    Assume foreign continuum dominates total H2O continuum in these bands
!    per Clough et al, JGR, v. 97, no. D14 (Oct 20, 1992), p. 15776
!    Then the effective H2O path is just 
!         U_c = integral[ f(P) dW ]
!    where 
!           W = water-vapor mass and 
!        f(P) = dependence of foreign continuum on pressure 
!             = P / sslp
!    Then 
!         U_c = U (the same effective H2O path as for lines)
!
!
! Continuum terms for 800-1200 cm^-1
!
!    Assume self continuum dominates total H2O continuum for this band
!    per Clough et al, JGR, v. 97, no. D14 (Oct 20, 1992), p. 15776
!    Then the effective H2O self-continuum path is 
!         U_c = integral[ h(e,T) dW ]                        (*eq. 1*)
!    where 
!           W = water-vapor mass and 
!           e = partial pressure of H2O along path
!           T = temperature along path
!      h(e,T) = dependence of foreign continuum on e,T
!             = e / sslp * f(T)
!
!    Replacing
!           e =~ q * P / epsilo
!           q = mixing ratio of H2O
!     epsilo = 0.622
!
!    and using the definition
!           U = integral [ (P / sslp) dW ]
!             = (P / sslp) W                                 (homogeneous path)
!
!    the effective path length for the self continuum is
!         U_c = (q / epsilo) f(T) U                         (*eq. 2*)
!
!    Once values of T, U, and q have been calculated for the inhomogeneous
!        path, this sets U_c for the corresponding
!        homogeneous atmosphere.  However, this need not equal the
!        value of U_c' defined by eq. 1 for the actual inhomogeneous atmosphere
!        under consideration.
!
!    Solution: hold T and q constant, solve for U' that gives U_c' by
!        inverting eq. (2):
!
!        U' = (U_c * epsilo) / (q * f(T))
!
         fch2o = fh2oself(t_p)
         uch2o = (pch2o * epsilo) / (q_path * fch2o)

!
! Band-dependent indices for non-window
!
         ib = 1

         uvar = ub(ib) * fdif
         log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
         dvar = (log_u - min_lu_h2o) / dlu_h2o
         iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
         iu1 = iu + 1
         wu = dvar - floor(dvar)
         wu1 = 1.0 - wu
         
         log_p  = min(log10(max(pnewb(ib), min_p_h2o)), max_lp_h2o)
         dvar = (log_p - min_lp_h2o) / dlp_h2o
         ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
         ip1 = ip + 1
         wp = dvar - floor(dvar)
         wp1 = 1.0 - wp

         w00_00 = wp  * w_0_00 
         w00_01 = wp  * w_0_01 
         w00_10 = wp  * w_0_10 
         w00_11 = wp  * w_0_11 
         w01_00 = wp  * w_1_00 
         w01_01 = wp  * w_1_01 
         w01_10 = wp  * w_1_10 
         w01_11 = wp  * w_1_11 
         w10_00 = wp1 * w_0_00 
         w10_01 = wp1 * w_0_01 
         w10_10 = wp1 * w_0_10 
         w10_11 = wp1 * w_0_11 
         w11_00 = wp1 * w_1_00 
         w11_01 = wp1 * w_1_01 
         w11_10 = wp1 * w_1_10 
         w11_11 = wp1 * w_1_11 

!
! Asymptotic value of emissivity as U->infinity
!
         fe = fet(1,ib) + &
              fet(2,ib) * te1 + &
              fet(3,ib) * te2 + &
              fet(4,ib) * te3 + &
              fet(5,ib) * te4 + &
              fet(6,ib) * te5

         e_star = &
              eh2onw(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
              eh2onw(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
              eh2onw(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
              eh2onw(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
              eh2onw(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
              eh2onw(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
              eh2onw(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
              eh2onw(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
              eh2onw(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
              eh2onw(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
              eh2onw(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
              eh2onw(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
              eh2onw(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
              eh2onw(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
              eh2onw(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
              eh2onw(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
              eh2onw(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
              eh2onw(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
              eh2onw(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
              eh2onw(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
              eh2onw(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
              eh2onw(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
              eh2onw(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
              eh2onw(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
              eh2onw(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
              eh2onw(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
              eh2onw(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
              eh2onw(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
              eh2onw(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
              eh2onw(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
              eh2onw(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
              eh2onw(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu 
         emis(i,ib) = min(max(fe * (1.0 - (1.0 - e_star) * &
                              aer_trn_ttl(i,k1,1,ib)), &
                          0.0_r8), 1.0_r8)
!
! Invoke linear limit for scaling wrt u below min_u_h2o
!
         if (uvar < min_u_h2o) then
            uscl = uvar / min_u_h2o
            emis(i,ib) = emis(i,ib) * uscl
         endif

                      

!
! Band-dependent indices for window
!
         ib = 2

         uvar = ub(ib) * fdif
         log_u  = min(log10(max(uvar, min_u_h2o)), max_lu_h2o)
         dvar = (log_u - min_lu_h2o) / dlu_h2o
         iu = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
         iu1 = iu + 1
         wu = dvar - floor(dvar)
         wu1 = 1.0 - wu
         
         log_p  = min(log10(max(pnewb(ib), min_p_h2o)), max_lp_h2o)
         dvar = (log_p - min_lp_h2o) / dlp_h2o
         ip = min(max(int(aint(dvar,r8)) + 1, 1), n_p - 1)
         ip1 = ip + 1
         wp = dvar - floor(dvar)
         wp1 = 1.0 - wp

         w00_00 = wp  * w_0_00 
         w00_01 = wp  * w_0_01 
         w00_10 = wp  * w_0_10 
         w00_11 = wp  * w_0_11 
         w01_00 = wp  * w_1_00 
         w01_01 = wp  * w_1_01 
         w01_10 = wp  * w_1_10 
         w01_11 = wp  * w_1_11 
         w10_00 = wp1 * w_0_00 
         w10_01 = wp1 * w_0_01 
         w10_10 = wp1 * w_0_10 
         w10_11 = wp1 * w_0_11 
         w11_00 = wp1 * w_1_00 
         w11_01 = wp1 * w_1_01 
         w11_10 = wp1 * w_1_10 
         w11_11 = wp1 * w_1_11 

         log_uc  = min(log10(max(uch2o * fdif, min_u_h2o)), max_lu_h2o)
         dvar = (log_uc - min_lu_h2o) / dlu_h2o
         iuc = min(max(int(aint(dvar,r8)) + 1, 1), n_u - 1)
         iuc1 = iuc + 1
         wuc = dvar - floor(dvar)
         wuc1 = 1.0 - wuc
!
! Asymptotic value of emissivity as U->infinity
!
         fe = fet(1,ib) + &
              fet(2,ib) * te1 + &
              fet(3,ib) * te2 + &
              fet(4,ib) * te3 + &
              fet(5,ib) * te4 + &
              fet(6,ib) * te5

         l_star = &
              ln_eh2ow(ip , itp , iu , ite , irh ) * w11_11 * wu1 + &
              ln_eh2ow(ip , itp , iu , ite , irh1) * w11_10 * wu1 + &
              ln_eh2ow(ip , itp , iu , ite1, irh ) * w11_01 * wu1 + &
              ln_eh2ow(ip , itp , iu , ite1, irh1) * w11_00 * wu1 + &
              ln_eh2ow(ip , itp , iu1, ite , irh ) * w11_11 * wu  + &
              ln_eh2ow(ip , itp , iu1, ite , irh1) * w11_10 * wu  + &
              ln_eh2ow(ip , itp , iu1, ite1, irh ) * w11_01 * wu  + &
              ln_eh2ow(ip , itp , iu1, ite1, irh1) * w11_00 * wu  + &
              ln_eh2ow(ip , itp1, iu , ite , irh ) * w10_11 * wu1 + &
              ln_eh2ow(ip , itp1, iu , ite , irh1) * w10_10 * wu1 + &
              ln_eh2ow(ip , itp1, iu , ite1, irh ) * w10_01 * wu1 + &
              ln_eh2ow(ip , itp1, iu , ite1, irh1) * w10_00 * wu1 + &
              ln_eh2ow(ip , itp1, iu1, ite , irh ) * w10_11 * wu  + &
              ln_eh2ow(ip , itp1, iu1, ite , irh1) * w10_10 * wu  + &
              ln_eh2ow(ip , itp1, iu1, ite1, irh ) * w10_01 * wu  + &
              ln_eh2ow(ip , itp1, iu1, ite1, irh1) * w10_00 * wu  + &
              ln_eh2ow(ip1, itp , iu , ite , irh ) * w01_11 * wu1 + &
              ln_eh2ow(ip1, itp , iu , ite , irh1) * w01_10 * wu1 + &
              ln_eh2ow(ip1, itp , iu , ite1, irh ) * w01_01 * wu1 + &
              ln_eh2ow(ip1, itp , iu , ite1, irh1) * w01_00 * wu1 + &
              ln_eh2ow(ip1, itp , iu1, ite , irh ) * w01_11 * wu  + &
              ln_eh2ow(ip1, itp , iu1, ite , irh1) * w01_10 * wu  + &
              ln_eh2ow(ip1, itp , iu1, ite1, irh ) * w01_01 * wu  + &
              ln_eh2ow(ip1, itp , iu1, ite1, irh1) * w01_00 * wu  + &
              ln_eh2ow(ip1, itp1, iu , ite , irh ) * w00_11 * wu1 + &
              ln_eh2ow(ip1, itp1, iu , ite , irh1) * w00_10 * wu1 + &
              ln_eh2ow(ip1, itp1, iu , ite1, irh ) * w00_01 * wu1 + &
              ln_eh2ow(ip1, itp1, iu , ite1, irh1) * w00_00 * wu1 + &
              ln_eh2ow(ip1, itp1, iu1, ite , irh ) * w00_11 * wu  + &
              ln_eh2ow(ip1, itp1, iu1, ite , irh1) * w00_10 * wu  + &
              ln_eh2ow(ip1, itp1, iu1, ite1, irh ) * w00_01 * wu  + &
              ln_eh2ow(ip1, itp1, iu1, ite1, irh1) * w00_00 * wu 

         c_star = &
              cn_eh2ow(ip , itp , iuc , ite , irh ) * w11_11 * wuc1 + &
              cn_eh2ow(ip , itp , iuc , ite , irh1) * w11_10 * wuc1 + &
              cn_eh2ow(ip , itp , iuc , ite1, irh ) * w11_01 * wuc1 + &
              cn_eh2ow(ip , itp , iuc , ite1, irh1) * w11_00 * wuc1 + &
              cn_eh2ow(ip , itp , iuc1, ite , irh ) * w11_11 * wuc  + &
              cn_eh2ow(ip , itp , iuc1, ite , irh1) * w11_10 * wuc  + &
              cn_eh2ow(ip , itp , iuc1, ite1, irh ) * w11_01 * wuc  + &
              cn_eh2ow(ip , itp , iuc1, ite1, irh1) * w11_00 * wuc  + &
              cn_eh2ow(ip , itp1, iuc , ite , irh ) * w10_11 * wuc1 + &
              cn_eh2ow(ip , itp1, iuc , ite , irh1) * w10_10 * wuc1 + &
              cn_eh2ow(ip , itp1, iuc , ite1, irh ) * w10_01 * wuc1 + &
              cn_eh2ow(ip , itp1, iuc , ite1, irh1) * w10_00 * wuc1 + &
              cn_eh2ow(ip , itp1, iuc1, ite , irh ) * w10_11 * wuc  + &
              cn_eh2ow(ip , itp1, iuc1, ite , irh1) * w10_10 * wuc  + &
              cn_eh2ow(ip , itp1, iuc1, ite1, irh ) * w10_01 * wuc  + &
              cn_eh2ow(ip , itp1, iuc1, ite1, irh1) * w10_00 * wuc  + &
              cn_eh2ow(ip1, itp , iuc , ite , irh ) * w01_11 * wuc1 + &
              cn_eh2ow(ip1, itp , iuc , ite , irh1) * w01_10 * wuc1 + &
              cn_eh2ow(ip1, itp , iuc , ite1, irh ) * w01_01 * wuc1 + &
              cn_eh2ow(ip1, itp , iuc , ite1, irh1) * w01_00 * wuc1 + &
              cn_eh2ow(ip1, itp , iuc1, ite , irh ) * w01_11 * wuc  + &
              cn_eh2ow(ip1, itp , iuc1, ite , irh1) * w01_10 * wuc  + &
              cn_eh2ow(ip1, itp , iuc1, ite1, irh ) * w01_01 * wuc  + &
              cn_eh2ow(ip1, itp , iuc1, ite1, irh1) * w01_00 * wuc  + &
              cn_eh2ow(ip1, itp1, iuc , ite , irh ) * w00_11 * wuc1 + &
              cn_eh2ow(ip1, itp1, iuc , ite , irh1) * w00_10 * wuc1 + &
              cn_eh2ow(ip1, itp1, iuc , ite1, irh ) * w00_01 * wuc1 + &
              cn_eh2ow(ip1, itp1, iuc , ite1, irh1) * w00_00 * wuc1 + &
              cn_eh2ow(ip1, itp1, iuc1, ite , irh ) * w00_11 * wuc  + &
              cn_eh2ow(ip1, itp1, iuc1, ite , irh1) * w00_10 * wuc  + &
              cn_eh2ow(ip1, itp1, iuc1, ite1, irh ) * w00_01 * wuc  + &
              cn_eh2ow(ip1, itp1, iuc1, ite1, irh1) * w00_00 * wuc 
         emis(i,ib) = min(max(fe * (1.0 - l_star * c_star * &
                              aer_trn_ttl(i,k1,1,ib)), &
                          0.0_r8), 1.0_r8) 
!
! Invoke linear limit for scaling wrt u below min_u_h2o
!
         if (uvar < min_u_h2o) then
            uscl = uvar / min_u_h2o
            emis(i,ib) = emis(i,ib) * uscl
         endif

                      
!
! Compute total emissivity for H2O
!
         h2oems(i,k1) = emis(i,1)+emis(i,2)

      end do
!
!
!

      do i=1,ncol
         term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*(1.+c16*dty(i))
         term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*(1.+c17*dty(i))
         term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*(1.+c26*dty(i))
         term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*(1.+c27*dty(i))
      end do
      do i=1,ncol
!
! 500 -  800 cm-1   rotation band overlap with co2
!
         k21(i) = term7(i,1) + term8(i,1)/ &
                 (1. + (c30 + c31*(dty(i)-10.)*(dty(i)-10.))*sqrt(u(i)))
         k22(i) = term7(i,2) + term8(i,2)/ &
                 (1. + (c28 + c29*(dty(i)-10.))*sqrt(u(i)))
         fwk    = fwcoef + fwc1/(1.+fwc2*u(i))
         tr1(i) = exp(-(k21(i)*(sqrt(u(i)) + fc1*fwk*u(i))))
         tr2(i) = exp(-(k22(i)*(sqrt(u(i)) + fc1*fwk*u(i))))
         tr1(i)=tr1(i)*aer_trn_ttl(i,k1,1,idx_LW_0650_0800) 
!                                            ! H2O line+aer trn 650--800 cm-1
         tr2(i)=tr2(i)*aer_trn_ttl(i,k1,1,idx_LW_0500_0650) 
!                                            ! H2O line+aer trn 500--650 cm-1
         tr3(i) = exp(-((coefh(1,1) + coefh(2,1)*dtx(i))*uc1(i)))
         tr4(i) = exp(-((coefh(1,2) + coefh(2,2)*dtx(i))*uc1(i)))
         tr7(i) = tr1(i)*tr3(i)
         tr8(i) = tr2(i)*tr4(i)
         troco2(i,k1) = 0.65*tr7(i) + 0.35*tr8(i)
         th2o(i) = tr8(i)
      end do
!
! CO2 emissivity for 15 micron band system
!
      do i=1,ncol
         t1i    = exp(-480./co2t(i,k1))
         sqti   = sqrt(co2t(i,k1))
         rsqti  = 1./sqti
         et     = t1i
         et2    = et*et
         et4    = et2*et2
         omet   = 1. - 1.5*et2
         f1co2  = 899.70*omet*(1. + 1.94774*et + 4.73486*et2)*rsqti
         sqwp   = sqrt(plco2(i,k1))
         f1sqwp = f1co2*sqwp
         t1co2  = 1./(1. + 245.18*omet*sqwp*rsqti)
         oneme  = 1. - et2
         alphat = oneme**3*rsqti
         wco2   = 2.5221*co2vmr*pnm(i,k1)*rga
         u7     = 4.9411e4*alphat*et2*wco2
         u8     = 3.9744e4*alphat*et4*wco2
         u9     = 1.0447e5*alphat*et4*et2*wco2
         u13    = 2.8388e3*alphat*et4*wco2
!
         tpath  = co2t(i,k1)
         tlocal = tplnke(i)
         tcrfac = sqrt((tlocal*r250)*(tpath*r300))
         pi     = pnm(i,k1)*rsslp + 2.*dpfco2*tcrfac
         posqt  = pi/(2.*sqti)
         rbeta7 =  1./( 5.3288*posqt)
         rbeta8 = 1./ (10.6576*posqt)
         rbeta9 = rbeta7
         rbeta13= rbeta9
         f2co2  = (u7/sqrt(4. + u7*(1. + rbeta7))) + &
                  (u8/sqrt(4. + u8*(1. + rbeta8))) + &
                  (u9/sqrt(4. + u9*(1. + rbeta9)))
         f3co2  = u13/sqrt(4. + u13*(1. + rbeta13))
         tmp1   = log(1. + f1sqwp)
         tmp2   = log(1. +  f2co2)
         tmp3   = log(1. +  f3co2)
         absbnd = (tmp1 + 2.*t1co2*tmp2 + 2.*tmp3)*sqti
         tco2(i)=1.0/(1.0+10.0*(u7/sqrt(4. + u7*(1. + rbeta7))))
         co2ems(i,k1)  = troco2(i,k1)*absbnd*co2plk(i)
         ex     = exp(960./tint(i,k1))
         exm1sq = (ex - 1.)**2
         co2em(i,k1) = 1.2e11*ex/(tint(i,k1)*tint4(i,k1)*exm1sq)
      end do
!
! O3 emissivity
!
      do i=1,ncol
         h2otr(i,k1) = exp(-12.*s2c(i,k1))
          h2otr(i,k1)=h2otr(i,k1)*aer_trn_ttl(i,k1,1,idx_LW_1000_1200)
         te          = (co2t(i,k1)/293.)**.7
         u1          = 18.29*plos(i,k1)/te
         u2          = .5649*plos(i,k1)/te
         phat        = plos(i,k1)/plol(i,k1)
         tlocal      = tplnke(i)
         tcrfac      = sqrt(tlocal*r250)*te
         beta        = (1./.3205)*((1./phat) + (dpfo3*tcrfac))
         realnu      = (1./beta)*te
         o3bndi      = 74.*te*(tplnke(i)/375.)*log(1. + fo3(u1,realnu) + fo3(u2,realnu))
         o3ems(i,k1) = dbvtt(i)*h2otr(i,k1)*o3bndi
         to3(i)=1.0/(1. + 0.1*fo3(u1,realnu) + 0.1*fo3(u2,realnu))
      end do
!
!   Calculate trace gas emissivities
!
      call trcems(lchnk   ,ncol    ,pcols, pverp,               &
                  k1      ,co2t    ,pnm     ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,bn2o0   ,bn2o1   ,uch4    , &
                  bch4    ,uco211  ,uco212  ,uco213  ,uco221  , &
                  uco222  ,uco223  ,uptype  ,w       ,s2c     , &
                  u       ,emplnk  ,th2o    ,tco2    ,to3     , &
                  emstrc  , &
                  aer_trn_ttl)
!
! Total emissivity:
!
      do i=1,ncol
         emstot(i,k1) = h2oems(i,k1) + co2ems(i,k1) + o3ems(i,k1)  &
                        + emstrc(i,k1)
      end do
   end do ! End of interface loop

   return
end subroutine radems

subroutine radtpl(lchnk   ,ncol    ,pcols, pver, pverp,                 &
                  tnm     ,lwupcgs ,qnm     ,pnm     ,plco2   ,plh2o   , &
                  tplnka  ,s2c     ,tcg     ,w       ,tplnke  , &
                  tint    ,tint4   ,tlayr   ,tlayr4  ,pmln    , &
                  piln    ,plh2ob  ,wb      )
!--------------------------------------------------------------------
!
! Purpose:
! Compute temperatures and path lengths for longwave radiation
!
! Method:
! <Describe the algorithm(s) used in the routine.>
! <Also include any applicable external references.>
!
! Author: CCM1
!
!--------------------------------------------------------------------

!------------------------------Arguments-----------------------------
!
! Input arguments
!
   integer, intent(in) :: lchnk                 ! chunk identifier
   integer, intent(in) :: ncol                  ! number of atmospheric columns
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: tnm(pcols,pver)      ! Model level temperatures
   real(r8), intent(in) :: lwupcgs(pcols)       ! Surface longwave up flux
   real(r8), intent(in) :: qnm(pcols,pver)      ! Model level specific humidity
   real(r8), intent(in) :: pnm(pcols,pverp)     ! Pressure at model interfaces (dynes/cm2)
   real(r8), intent(in) :: pmln(pcols,pver)     ! Ln(pmidm1)
   real(r8), intent(in) :: piln(pcols,pverp)    ! Ln(pintm1)
!
! Output arguments
!
   real(r8), intent(out) :: plco2(pcols,pverp)   ! Pressure weighted co2 path
   real(r8), intent(out) :: plh2o(pcols,pverp)   ! Pressure weighted h2o path
   real(r8), intent(out) :: tplnka(pcols,pverp)  ! Level temperature from interface temperatures
   real(r8), intent(out) :: s2c(pcols,pverp)     ! H2o continuum path length
   real(r8), intent(out) :: tcg(pcols,pverp)     ! H2o-mass-wgted temp. (Curtis-Godson approx.)
   real(r8), intent(out) :: w(pcols,pverp)       ! H2o path length
   real(r8), intent(out) :: tplnke(pcols)        ! Equal to tplnka
   real(r8), intent(out) :: tint(pcols,pverp)    ! Layer interface temperature
   real(r8), intent(out) :: tint4(pcols,pverp)   ! Tint to the 4th power
   real(r8), intent(out) :: tlayr(pcols,pverp)   ! K-1 level temperature
   real(r8), intent(out) :: tlayr4(pcols,pverp)  ! Tlayr to the 4th power
   real(r8), intent(out) :: plh2ob(nbands,pcols,pverp)! Pressure weighted h2o path with 
                                                      !    Hulst-Curtis-Godson temp. factor 
                                                      !    for H2O bands 
   real(r8), intent(out) :: wb(nbands,pcols,pverp)    ! H2o path length with 
                                                      !    Hulst-Curtis-Godson temp. factor 
                                                      !    for H2O bands 

!
!---------------------------Local variables--------------------------
!
   integer i                 ! Longitude index
   integer k                 ! Level index
   integer kp1               ! Level index + 1

   real(r8) repsil               ! Inver ratio mol weight h2o to dry air
   real(r8) dy                   ! Thickness of layer for tmp interp
   real(r8) dpnm                 ! Pressure thickness of layer
   real(r8) dpnmsq               ! Prs squared difference across layer
   real(r8) dw                   ! Increment in H2O path length
   real(r8) dplh2o               ! Increment in plh2o
   real(r8) cpwpl                ! Const in co2 mix ratio to path length conversn

!--------------------------------------------------------------------
!
   repsil = 1./epsilo
!
! Compute co2 and h2o paths
!
   cpwpl = amco2/amd * 0.5/(gravit*p0)
   do i=1,ncol
      plh2o(i,ntoplw)  = rgsslp*qnm(i,ntoplw)*pnm(i,ntoplw)*pnm(i,ntoplw)
      plco2(i,ntoplw)  = co2vmr*cpwpl*pnm(i,ntoplw)*pnm(i,ntoplw)
   end do
   do k=ntoplw,pver
      do i=1,ncol
         plh2o(i,k+1)  = plh2o(i,k) + rgsslp* &
                         (pnm(i,k+1)**2 - pnm(i,k)**2)*qnm(i,k)
         plco2(i,k+1)  = co2vmr*cpwpl*pnm(i,k+1)**2
      end do
   end do
!
! Set the top and bottom intermediate level temperatures,
! top level planck temperature and top layer temp**4.
!
! Tint is lower interface temperature
! (not available for bottom layer, so use ground temperature)
!
   do i=1,ncol
      tint4(i,pverp)   = lwupcgs(i)/stebol
      tint(i,pverp)    = sqrt(sqrt(tint4(i,pverp)))
      tplnka(i,ntoplw) = tnm(i,ntoplw)
      tint(i,ntoplw)   = tplnka(i,ntoplw)
      tlayr4(i,ntoplw) = tplnka(i,ntoplw)**4
      tint4(i,ntoplw)  = tlayr4(i,ntoplw)
   end do
!
! Intermediate level temperatures are computed using temperature
! at the full level below less dy*delta t,between the full level
!
   do k=ntoplw+1,pver
      do i=1,ncol
         dy = (piln(i,k) - pmln(i,k))/(pmln(i,k-1) - pmln(i,k))
         tint(i,k)  = tnm(i,k) - dy*(tnm(i,k)-tnm(i,k-1))
         tint4(i,k) = tint(i,k)**4
      end do
   end do
!
! Now set the layer temp=full level temperatures and establish a
! planck temperature for absorption (tplnka) which is the average
! the intermediate level temperatures.  Note that tplnka is not
! equal to the full level temperatures.
!
   do k=ntoplw+1,pverp
      do i=1,ncol
         tlayr(i,k)  = tnm(i,k-1)
         tlayr4(i,k) = tlayr(i,k)**4
         tplnka(i,k) = .5*(tint(i,k) + tint(i,k-1))
      end do
   end do
!
! Calculate tplank for emissivity calculation.
! Assume isothermal tplnke i.e. all levels=ttop.
!
   do i=1,ncol
      tplnke(i)       = tplnka(i,ntoplw)
      tlayr(i,ntoplw) = tint(i,ntoplw)
   end do
!
! Now compute h2o path fields:
!
   do i=1,ncol
!
! Changed effective path temperature to std. Curtis-Godson form
!
      tcg(i,ntoplw) = rga*qnm(i,ntoplw)*pnm(i,ntoplw)*tnm(i,ntoplw)
      w(i,ntoplw)   = sslp * (plh2o(i,ntoplw)*2.) / pnm(i,ntoplw)
!
! Hulst-Curtis-Godson scaling for H2O path
!
      wb(1,i,ntoplw) = w(i,ntoplw) * phi(tnm(i,ntoplw),1)
      wb(2,i,ntoplw) = w(i,ntoplw) * phi(tnm(i,ntoplw),2)
!
! Hulst-Curtis-Godson scaling for effective pressure along H2O path
!
      plh2ob(1,i,ntoplw) = plh2o(i,ntoplw) * psi(tnm(i,ntoplw),1)
      plh2ob(2,i,ntoplw) = plh2o(i,ntoplw) * psi(tnm(i,ntoplw),2)

      s2c(i,ntoplw) = plh2o(i,ntoplw)*fh2oself(tnm(i,ntoplw))*qnm(i,ntoplw)*repsil
   end do

   do k=ntoplw,pver
      do i=1,ncol
         dpnm       = pnm(i,k+1) - pnm(i,k)
         dpnmsq     = pnm(i,k+1)**2 - pnm(i,k)**2
         dw         = rga*qnm(i,k)*dpnm
         kp1        = k+1
         w(i,kp1)   = w(i,k) + dw
!
! Hulst-Curtis-Godson scaling for H2O path
!
         wb(1,i,kp1) = wb(1,i,k) + dw * phi(tnm(i,k),1)
         wb(2,i,kp1) = wb(2,i,k) + dw * phi(tnm(i,k),2)
!
! Hulst-Curtis-Godson scaling for effective pressure along H2O path
!
         dplh2o = plh2o(i,kp1) - plh2o(i,k)

         plh2ob(1,i,kp1) = plh2ob(1,i,k) + dplh2o * psi(tnm(i,k),1)
         plh2ob(2,i,kp1) = plh2ob(2,i,k) + dplh2o * psi(tnm(i,k),2)
!
! Changed effective path temperature to std. Curtis-Godson form
!
         tcg(i,kp1) = tcg(i,k) + dw*tnm(i,k)
         s2c(i,kp1) = s2c(i,k) + rgsslp*dpnmsq*qnm(i,k)* &
                      fh2oself(tnm(i,k))*qnm(i,k)*repsil
      end do
   end do
!
   return
end subroutine radtpl


subroutine radclwmx(lchnk   ,ncol    ,pcols, pver, pverp,         &
                    lwupcgs ,tnm     ,qnm     ,o3vmr   , &
                    pmid    ,pint    ,pmln    ,piln    ,          &
                             n2o     ,ch4     ,cfc11   ,cfc12   , &
                    cld     ,emis    ,pmxrgn  ,nmxrgn  ,qrl     , &
                    doabsems, abstot, absnxt, emstot,             &
                    flns    ,flnt    ,flnsc   ,flntc   ,flwds   , &
                    flut    ,flutc   , &
                    flup    ,flupc   ,fldn    ,fldnc   ,          &
                    aer_mass)
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Compute longwave radiation heating rates and boundary fluxes
! 
! Method: 
! Uses broad band absorptivity/emissivity method to compute clear sky;
! assumes randomly overlapped clouds with variable cloud emissivity to
! include effects of clouds.
!
! Computes clear sky absorptivity/emissivity at lower frequency (in
! general) than the model radiation frequency; uses previously computed
! and stored values for efficiency
!
! Note: This subroutine contains vertical indexing which proceeds
!       from bottom to top rather than the top to bottom indexing
!       used in the rest of the model.
! 
! Author: B. Collins
! 
!-----------------------------------------------------------------------
!  use shr_kind_mod, only: r8 => shr_kind_r8
!  use ppgrid
!  use radae, only: nbands, radems, radabs, radtpl, abstot_3d, absnxt_3d, emstot_3d
!  use volcrad

   implicit none

   integer pverp2,pverp3,pverp4
!  parameter (pverp2=pver+2,pverp3=pver+3,pverp4=pver+4)

   real(r8) cldmin
   parameter (cldmin = 1.0d-80)
!------------------------------Commons----------------------------------
!-----------------------------------------------------------------------
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: lchnk                 ! chunk identifier
   integer, intent(in) :: pcols, pver, pverp
   integer, intent(in) :: ncol                  ! number of atmospheric columns
!    maximally overlapped region.
!    0->pmxrgn(i,1) is range of pmid for
!    1st region, pmxrgn(i,1)->pmxrgn(i,2) for
!    2nd region, etc
   integer, intent(in) :: nmxrgn(pcols)         ! Number of maximally overlapped regions
   logical, intent(in) :: doabsems

   real(r8), intent(in) :: pmxrgn(pcols,pverp)  ! Maximum values of pmid for each
   real(r8), intent(in) :: lwupcgs(pcols)       ! Longwave up flux in CGS units
!
! Input arguments which are only passed to other routines
!
   real(r8), intent(in) :: tnm(pcols,pver)      ! Level temperature
   real(r8), intent(in) :: qnm(pcols,pver)      ! Level moisture field
   real(r8), intent(in) :: o3vmr(pcols,pver)    ! ozone volume mixing ratio
   real(r8), intent(in) :: pmid(pcols,pver)     ! Level pressure
   real(r8), intent(in) :: pint(pcols,pverp)    ! Model interface pressure
   real(r8), intent(in) :: pmln(pcols,pver)     ! Ln(pmid)
   real(r8), intent(in) :: piln(pcols,pverp)    ! Ln(pint)
   real(r8), intent(in) :: n2o(pcols,pver)      ! nitrous oxide mass mixing ratio
   real(r8), intent(in) :: ch4(pcols,pver)      ! methane mass mixing ratio
   real(r8), intent(in) :: cfc11(pcols,pver)    ! cfc11 mass mixing ratio
   real(r8), intent(in) :: cfc12(pcols,pver)    ! cfc12 mass mixing ratio
   real(r8), intent(in) :: cld(pcols,pver)      ! Cloud cover
   real(r8), intent(in) :: emis(pcols,pver)     ! Cloud emissivity
   real(r8), intent(in) :: aer_mass(pcols,pver) ! STRAER mass in layer

!
! Output arguments
!
   real(r8), intent(out) :: qrl(pcols,pver)      ! Longwave heating rate
   real(r8), intent(out) :: flns(pcols)          ! Surface cooling flux
   real(r8), intent(out) :: flnt(pcols)          ! Net outgoing flux
   real(r8), intent(out) :: flut(pcols)          ! Upward flux at top of model
   real(r8), intent(out) :: flnsc(pcols)         ! Clear sky surface cooing
   real(r8), intent(out) :: flntc(pcols)         ! Net clear sky outgoing flux
   real(r8), intent(out) :: flutc(pcols)         ! Upward clear-sky flux at top of model
   real(r8), intent(out) :: flwds(pcols)         ! Down longwave flux at surface
! Added downward/upward total and clear sky fluxes
   real(r8), intent(out) :: flup(pcols,pverp)      ! Total sky upward longwave flux 
   real(r8), intent(out) :: flupc(pcols,pverp)     ! Clear sky upward longwave flux 
   real(r8), intent(out) :: fldn(pcols,pverp)      ! Total sky downward longwave flux 
   real(r8), intent(out) :: fldnc(pcols,pverp)     ! Clear sky downward longwave flux
!
   real(r8), intent(inout) :: abstot(pcols,pverp,pverp) ! Total absorptivity
   real(r8), intent(inout) :: absnxt(pcols,pver,4)      ! Total nearest layer absorptivity
   real(r8), intent(inout) :: emstot(pcols,pverp)     ! Total emissivity

!---------------------------Local variables-----------------------------
!
   integer i                 ! Longitude index
   integer ilon              ! Longitude index
   integer ii                ! Longitude index
   integer iimx              ! Longitude index (max overlap)
   integer k                 ! Level index
   integer k1                ! Level index
   integer k2                ! Level index
   integer k3                ! Level index
   integer km                ! Level index
   integer km1               ! Level index
   integer km3               ! Level index
   integer km4               ! Level index
   integer irgn              ! Index for max-overlap regions
   integer l                 ! Index for clouds to overlap
   integer l1                ! Index for clouds to overlap
   integer n                 ! Counter

!
   real(r8) :: plco2(pcols,pverp)   ! Path length co2
   real(r8) :: plh2o(pcols,pverp)   ! Path length h2o
   real(r8) tmp(pcols)           ! Temporary workspace
   real(r8) tmp2(pcols)          ! Temporary workspace
   real(r8) absbt(pcols)         ! Downward emission at model top
   real(r8) plol(pcols,pverp)    ! O3 pressure wghted path length
   real(r8) plos(pcols,pverp)    ! O3 path length
   real(r8) aer_mpp(pcols,pverp) ! STRAER path above kth interface level
   real(r8) co2em(pcols,pverp)   ! Layer co2 normalized planck funct. derivative
   real(r8) co2eml(pcols,pver)   ! Interface co2 normalized planck funct. deriv.
   real(r8) delt(pcols)          ! Diff t**4 mid layer to top interface
   real(r8) delt1(pcols)         ! Diff t**4 lower intrfc to mid layer
   real(r8) bk1(pcols)           ! Absrptvty for vertical quadrature
   real(r8) bk2(pcols)           ! Absrptvty for vertical quadrature
   real(r8) cldp(pcols,pverp)    ! Cloud cover with extra layer
   real(r8) ful(pcols,pverp)     ! Total upwards longwave flux
   real(r8) fsul(pcols,pverp)    ! Clear sky upwards longwave flux
   real(r8) fdl(pcols,pverp)     ! Total downwards longwave flux
   real(r8) fsdl(pcols,pverp)    ! Clear sky downwards longwv flux
   real(r8) fclb4(pcols,-1:pver)    ! Sig t**4 for cld bottom interfc
   real(r8) fclt4(pcols,0:pver)    ! Sig t**4 for cloud top interfc
   real(r8) s(pcols,pverp,pverp) ! Flx integral sum
   real(r8) tplnka(pcols,pverp)  ! Planck fnctn temperature
   real(r8) s2c(pcols,pverp)     ! H2o cont amount
   real(r8) tcg(pcols,pverp)     ! H2o-mass-wgted temp. (Curtis-Godson approx.)
   real(r8) w(pcols,pverp)       ! H2o path
   real(r8) tplnke(pcols)        ! Planck fnctn temperature
   real(r8) h2otr(pcols,pverp)   ! H2o trnmsn for o3 overlap
   real(r8) co2t(pcols,pverp)    ! Prs wghted temperature path
   real(r8) tint(pcols,pverp)    ! Interface temperature
   real(r8) tint4(pcols,pverp)   ! Interface temperature**4
   real(r8) tlayr(pcols,pverp)   ! Level temperature
   real(r8) tlayr4(pcols,pverp)  ! Level temperature**4
   real(r8) plh2ob(nbands,pcols,pverp)! Pressure weighted h2o path with 
                                      !    Hulst-Curtis-Godson temp. factor 
                                      !    for H2O bands 
   real(r8) wb(nbands,pcols,pverp)    ! H2o path length with 
                                      !    Hulst-Curtis-Godson temp. factor 
                                      !    for H2O bands 

   real(r8) cld0                 ! previous cloud amt (for max overlap)
   real(r8) cld1                 ! next cloud amt (for max overlap)
   real(r8) emx(0:pverp)         ! Emissivity factors (max overlap)
   real(r8) emx0                 ! Emissivity factors for BCs (max overlap)
   real(r8) trans                ! 1 - emis
   real(r8) asort(pver)          ! 1 - cloud amounts to be sorted for max ovrlp.
   real(r8) atmp                 ! Temporary storage for sort when nxs = 2
   real(r8) maxcld(pcols)        ! Maximum cloud at any layer

   integer indx(pcols)       ! index vector of gathered array values
!!$   integer indxmx(pcols+1,pverp)! index vector of gathered array values
   integer indxmx(pcols,pverp)! index vector of gathered array values
!    (max overlap)
   integer nrgn(pcols)       ! Number of max overlap regions at longitude
   integer npts              ! number of values satisfying some criterion
   integer ncolmx(pverp)     ! number of columns with clds in region
   integer kx1(pcols,pverp)  ! Level index for top of max-overlap region
   integer kx2(pcols,0:pverp)! Level index for bottom of max-overlap region
   integer kxs(0:pverp,pcols,pverp)! Level indices for cld layers sorted by cld()
!    in descending order
   integer nxs(pcols,pverp)  ! Number of cloudy layers between kx1 and kx2
   integer nxsk              ! Number of cloudy layers between (kx1/kx2)&k
   integer ksort(0:pverp)    ! Level indices of cloud amounts to be sorted
!    for max ovrlp. calculation
   integer ktmp              ! Temporary storage for sort when nxs = 2

!  real aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) ! [fraction] Total
  real(r8) aer_trn_ttl(pcols,pverp,pverp,bnd_nbr_LW) ! [fraction] Total
!                               ! transmission between interfaces k1 and k2  
!
! Pointer variables to 3d structures
!
!  real(r8), pointer :: abstot(:,:,:)
!  real(r8), pointer :: absnxt(:,:,:)
!  real(r8), pointer :: emstot(:,:)

!
! Trace gas variables
!
   real(r8) ucfc11(pcols,pverp)  ! CFC11 path length
   real(r8) ucfc12(pcols,pverp)  ! CFC12 path length
   real(r8) un2o0(pcols,pverp)   ! N2O path length
   real(r8) un2o1(pcols,pverp)   ! N2O path length (hot band)
   real(r8) uch4(pcols,pverp)    ! CH4 path length
   real(r8) uco211(pcols,pverp)  ! CO2 9.4 micron band path length
   real(r8) uco212(pcols,pverp)  ! CO2 9.4 micron band path length
   real(r8) uco213(pcols,pverp)  ! CO2 9.4 micron band path length
   real(r8) uco221(pcols,pverp)  ! CO2 10.4 micron band path length
   real(r8) uco222(pcols,pverp)  ! CO2 10.4 micron band path length
   real(r8) uco223(pcols,pverp)  ! CO2 10.4 micron band path length
   real(r8) bn2o0(pcols,pverp)   ! pressure factor for n2o
   real(r8) bn2o1(pcols,pverp)   ! pressure factor for n2o
   real(r8) bch4(pcols,pverp)    ! pressure factor for ch4
   real(r8) uptype(pcols,pverp)  ! p-type continuum path length
   real(r8) abplnk1(14,pcols,pverp)  ! non-nearest layer Plack factor
   real(r8) abplnk2(14,pcols,pverp)  ! nearest layer factor
!
!
!-----------------------------------------------------------------------
!
!
   pverp2=pver+2
   pverp3=pver+3
   pverp4=pver+4
!
! Set pointer variables
!
!  abstot => abstot_3d(:,:,:,lchnk)
!  absnxt => absnxt_3d(:,:,:,lchnk)
!  emstot => emstot_3d(:,:,lchnk)
!
! accumulate mass path from top of atmosphere
!
  call aer_pth(aer_mass, aer_mpp, ncol, pcols, pver, pverp)

!
! Calculate some temperatures needed to derive absorptivity and
! emissivity, as well as some h2o path lengths
!
   call radtpl(lchnk   ,ncol    ,pcols, pver, pverp,                  &
               tnm     ,lwupcgs ,qnm     ,pint    ,plco2   ,plh2o   , &
               tplnka  ,s2c     ,tcg     ,w       ,tplnke  , &
               tint    ,tint4   ,tlayr   ,tlayr4  ,pmln    , &
               piln    ,plh2ob  ,wb      )
   if (doabsems) then
!
! Compute ozone path lengths at frequency of a/e calculation.
!
      call radoz2(lchnk, ncol, pcols, pver, pverp, o3vmr   ,pint    ,plol    ,plos, ntoplw    )
!
! Compute trace gas path lengths
!
      call trcpth(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  tnm     ,pint    ,cfc11   ,cfc12   ,n2o     , &
                  ch4     ,qnm     ,ucfc11  ,ucfc12  ,un2o0   , &
                  un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , &
                  uco221  ,uco222  ,uco223  ,bn2o0   ,bn2o1   , &
                  bch4    ,uptype  )

!     Compute transmission through STRAER absorption continuum
      call aer_trn(aer_mpp, aer_trn_ttl, pcols, pver, pverp)

!
!
! Compute total emissivity:
!
      call radems(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  s2c     ,tcg     ,w       ,tplnke  ,plh2o   , &
                  pint    ,plco2   ,tint    ,tint4   ,tlayr   , &
                  tlayr4  ,plol    ,plos    ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,uch4    ,uco211  ,uco212  , &
                  uco213  ,uco221  ,uco222  ,uco223  ,uptype  , &
                  bn2o0   ,bn2o1   ,bch4    ,co2em   ,co2eml  , &
                  co2t    ,h2otr   ,abplnk1 ,abplnk2 ,emstot  , &
                  plh2ob  ,wb      , &
                  aer_trn_ttl)
!
! Compute total absorptivity:
!
      call radabs(lchnk   ,ncol    ,pcols, pver, pverp,         &
                  pmid    ,pint    ,co2em   ,co2eml  ,tplnka  , &
                  s2c     ,tcg     ,w       ,h2otr   ,plco2   , &
                  plh2o   ,co2t    ,tint    ,tlayr   ,plol    , &
                  plos    ,pmln    ,piln    ,ucfc11  ,ucfc12  , &
                  un2o0   ,un2o1   ,uch4    ,uco211  ,uco212  , &
                  uco213  ,uco221  ,uco222  ,uco223  ,uptype  , &
                  bn2o0   ,bn2o1   ,bch4    ,abplnk1 ,abplnk2 , &
                  abstot  ,absnxt  ,plh2ob  ,wb      , &
                  aer_mpp ,aer_trn_ttl)
   end if
!
! Compute sums used in integrals (all longitude points)
!
! Definition of bk1 & bk2 depends on finite differencing.  for
! trapezoidal rule bk1=bk2. trapezoidal rule applied for nonadjacent
! layers only.
!
! delt=t**4 in layer above current sigma level km.
! delt1=t**4 in layer below current sigma level km.
!
   do i=1,ncol
      delt(i) = tint4(i,pver) - tlayr4(i,pverp)
      delt1(i) = tlayr4(i,pverp) - tint4(i,pverp)
      s(i,pverp,pverp) = stebol*(delt1(i)*absnxt(i,pver,1) + delt (i)*absnxt(i,pver,4))
      s(i,pver,pverp)  = stebol*(delt (i)*absnxt(i,pver,2) + delt1(i)*absnxt(i,pver,3))
   end do
   do k=ntoplw,pver-1
      do i=1,ncol
         bk2(i) = (abstot(i,k,pver) + abstot(i,k,pverp))*0.5
         bk1(i) = bk2(i)
         s(i,k,pverp) = stebol*(bk2(i)*delt(i) + bk1(i)*delt1(i))
      end do
   end do
!
! All k, km>1
!
   do km=pver,ntoplw+1,-1
      do i=1,ncol
         delt(i)  = tint4(i,km-1) - tlayr4(i,km)
         delt1(i) = tlayr4(i,km) - tint4(i,km)
      end do
      do k=pverp,ntoplw,-1
         if (k == km) then
            do i=1,ncol
               bk2(i) = absnxt(i,km-1,4)
               bk1(i) = absnxt(i,km-1,1)
            end do
         else if (k == km-1) then
            do i=1,ncol
               bk2(i) = absnxt(i,km-1,2)
               bk1(i) = absnxt(i,km-1,3)
            end do
         else
            do i=1,ncol
               bk2(i) = (abstot(i,k,km-1) + abstot(i,k,km))*0.5
               bk1(i) = bk2(i)
            end do
         end if
         do i=1,ncol
            s(i,k,km) = s(i,k,km+1) + stebol*(bk2(i)*delt(i) + bk1(i)*delt1(i))
         end do
      end do
   end do
!
! Computation of clear sky fluxes always set first level of fsul
!
   do i=1,ncol
      fsul(i,pverp) = lwupcgs(i)
   end do
!
! Downward clear sky fluxes store intermediate quantities in down flux
! Initialize fluxes to clear sky values.
!
   do i=1,ncol
      tmp(i) = fsul(i,pverp) - stebol*tint4(i,pverp)
      fsul(i,ntoplw) = fsul(i,pverp) - abstot(i,ntoplw,pverp)*tmp(i) + s(i,ntoplw,ntoplw+1)
      fsdl(i,ntoplw) = stebol*(tplnke(i)**4)*emstot(i,ntoplw)
   end do
!
! fsdl(i,pverp) assumes isothermal layer
!
   do k=ntoplw+1,pver
      do i=1,ncol
         fsul(i,k) = fsul(i,pverp) - abstot(i,k,pverp)*tmp(i) + s(i,k,k+1)
         fsdl(i,k) = stebol*(tplnke(i)**4)*emstot(i,k) - (s(i,k,ntoplw+1) - s(i,k,k+1))
      end do
   end do
!
! Store the downward emission from level 1 = total gas emission * sigma
! t**4.  fsdl does not yet include all terms
!
   do i=1,ncol
      absbt(i) = stebol*(tplnke(i)**4)*emstot(i,pverp)
      fsdl(i,pverp) = absbt(i) - s(i,pverp,ntoplw+1)
   end do
!
!----------------------------------------------------------------------
! Modifications for clouds -- max/random overlap assumption
!
! The column is divided into sets of adjacent layers, called regions,
!   in which the clouds are maximally overlapped.  The clouds are
!   randomly overlapped between different regions.  The number of
!   regions in a column is set by nmxrgn, and the range of pressures
!   included in each region is set by pmxrgn.  The max/random overlap
!   can be written in terms of the solutions of random overlap with
!   cloud amounts = 1.  The random overlap assumption is equivalent to
!   setting the flux boundary conditions (BCs) at the edges of each region
!   equal to the mean all-sky flux at those boundaries.  Since the
!   emissivity array for propogating BCs is only computed for the
!   TOA BC, the flux BCs elsewhere in the atmosphere have to be formulated
!   in terms of solutions to the random overlap equations.  This is done
!   by writing the flux BCs as the sum of a clear-sky flux and emission
!   from a cloud outside the region weighted by an emissivity.  This
!   emissivity is determined from the location of the cloud and the
!   flux BC.
!
! Copy cloud amounts to buffer with extra layer (needed for overlap logic)
!
   cldp(:ncol,ntoplw:pver) = cld(:ncol,ntoplw:pver)
   cldp(:ncol,pverp) = 0.0
!
!
! Select only those locations where there are no clouds
!    (maximum cloud fraction <= 1.e-3 treated as clear)
!    Set all-sky fluxes to clear-sky values.
!
   maxcld(1:ncol) = maxval(cldp(1:ncol,ntoplw:pver),dim=2)

   npts = 0
   do i=1,ncol
      if (maxcld(i) < cldmin) then
         npts = npts + 1
         indx(npts) = i
      end if
   end do

   do ii = 1, npts
      i = indx(ii)
      do k = ntoplw, pverp
         fdl(i,k) = fsdl(i,k)
         ful(i,k) = fsul(i,k)
      end do
   end do
!
! Select only those locations where there are clouds
!
   npts = 0
   do i=1,ncol
      if (maxcld(i) >= cldmin) then
         npts = npts + 1
         indx(npts) = i
      end if
   end do

!
! Initialize all-sky fluxes. fdl(i,1) & ful(i,pverp) are boundary conditions
!
   do ii = 1, npts
      i = indx(ii)
      fdl(i,ntoplw) = fsdl(i,ntoplw)
      fdl(i,pverp)  = 0.0
      ful(i,ntoplw) = 0.0
      ful(i,pverp)  = fsul(i,pverp)
      do k = ntoplw+1, pver
         fdl(i,k) = 0.0
         ful(i,k) = 0.0
      end do
!
! Initialize Planck emission from layer boundaries
!
      do k = ntoplw, pver
         fclt4(i,k-1) = stebol*tint4(i,k)
         fclb4(i,k-1) = stebol*tint4(i,k+1)
      enddo
      fclb4(i,ntoplw-2) =  stebol*tint4(i,ntoplw)
      fclt4(i,pver)     = stebol*tint4(i,pverp)
!
! Initialize indices for layers to be max-overlapped
!
      do irgn = 0, nmxrgn(i)
         kx2(i,irgn) = ntoplw-1
      end do
      nrgn(i) = 0
   end do

!----------------------------------------------------------------------
! INDEX CALCULATIONS FOR MAX OVERLAP

   do ii = 1, npts
      ilon = indx(ii)

!
! Outermost loop over regions (sets of adjacent layers) to be max overlapped
!
      do irgn = 1, nmxrgn(ilon)
!
! Calculate min/max layer indices inside region.
!
         n = 0
         if (kx2(ilon,irgn-1) < pver) then
            nrgn(ilon) = irgn
            k1 = kx2(ilon,irgn-1)+1
            kx1(ilon,irgn) = k1
            kx2(ilon,irgn) = 0
            do k2 = pver, k1, -1
               if (pmid(ilon,k2) <= pmxrgn(ilon,irgn)) then
                  kx2(ilon,irgn) = k2
                  exit
               end if
            end do
!
! Identify columns with clouds in the given region.
!
            do k = k1, k2
               if (cldp(ilon,k) >= cldmin) then
                  n = n+1
                  indxmx(n,irgn) = ilon
                  exit
               endif
            end do
         endif
         ncolmx(irgn) = n
!
! Dummy value for handling clear-sky regions
!
!!$         indxmx(ncolmx(irgn)+1,irgn) = ncol+1
!
! Outer loop over columns with clouds in the max-overlap region
!
         do iimx = 1, ncolmx(irgn)
            i = indxmx(iimx,irgn)
!
! Sort cloud areas and corresponding level indices.
!
            n = 0
            do k = kx1(i,irgn),kx2(i,irgn)
               if (cldp(i,k) >= cldmin) then
                  n = n+1
                  ksort(n) = k
!
! We need indices for clouds in order of largest to smallest, so
!    sort 1-cld in ascending order
!
                  asort(n) = 1.0-cldp(i,k)
               end if
            end do
            nxs(i,irgn) = n
!
! If nxs(i,irgn) eq 1, no need to sort.
! If nxs(i,irgn) eq 2, sort by swapping if necessary
! If nxs(i,irgn) ge 3, sort using local sort routine
!
            if (nxs(i,irgn) == 2) then
               if (asort(2) < asort(1)) then
                  ktmp = ksort(1)
                  ksort(1) = ksort(2)
                  ksort(2) = ktmp

                  atmp = asort(1)
                  asort(1) = asort(2)
                  asort(2) = atmp
               endif
            else if (nxs(i,irgn) >= 3) then
               call sortarray(nxs(i,irgn),asort,ksort(1:))
            endif

            do l = 1, nxs(i,irgn)
               kxs(l,i,irgn) = ksort(l)
            end do
!
! End loop over longitude i for fluxes
!
         end do
!
! End loop over regions irgn for max-overlap
!
      end do
!
!----------------------------------------------------------------------
! DOWNWARD FLUXES:
! Outermost loop over regions (sets of adjacent layers) to be max overlapped
!
      do irgn = 1, nmxrgn(ilon)
!
! Compute clear-sky fluxes for regions without clouds
!
         iimx = 1
         if (ilon < indxmx(iimx,irgn) .and. irgn <= nrgn(ilon)) then
!
! Calculate emissivity so that downward flux at upper boundary of region
!    can be cast in form of solution for downward flux from cloud above
!    that boundary.  Then solutions for fluxes at other levels take form of
!    random overlap expressions.  Try to locate "cloud" as close as possible
!    to TOA such that the "cloud" pseudo-emissivity is between 0 and 1.
!
            k1 = kx1(ilon,irgn)
            do km1 = ntoplw-2, k1-2
               km4 = km1+3
               k2 = k1
               k3 = k2+1
               tmp(ilon) = s(ilon,k2,min(k3,pverp))*min(1,pverp2-k3)
               emx0 = (fdl(ilon,k1)-fsdl(ilon,k1))/ &
                      ((fclb4(ilon,km1)-s(ilon,k2,km4)+tmp(ilon))- fsdl(ilon,k1))
               if (emx0 >= 0.0 .and. emx0 <= 1.0) exit
            end do
            km1 = min(km1,k1-2)
            do k2 = kx1(ilon,irgn)+1, kx2(ilon,irgn)+1
               k3 = k2+1
               tmp(ilon) = s(ilon,k2,min(k3,pverp))*min(1,pverp2-k3)
               fdl(ilon,k2) = (1.0-emx0)*fsdl(ilon,k2) + &
                               emx0*(fclb4(ilon,km1)-s(ilon,k2,km4)+tmp(ilon))
            end do
         else if (ilon==indxmx(iimx,irgn) .and. iimx<=ncolmx(irgn)) then
            iimx = iimx+1
         end if
!
! Outer loop over columns with clouds in the max-overlap region
!
         do iimx = 1, ncolmx(irgn)
            i = indxmx(iimx,irgn)

!
! Calculate emissivity so that downward flux at upper boundary of region
!    can be cast in form of solution for downward flux from cloud above that
!    boundary.  Then solutions for fluxes at other levels take form of
!    random overlap expressions.  Try to locate "cloud" as close as possible
!    to TOA such that the "cloud" pseudo-emissivity is between 0 and 1.
!
            k1 = kx1(i,irgn)
            do km1 = ntoplw-2,k1-2
               km4 = km1+3
               k2 = k1
               k3 = k2 + 1
               tmp(i) = s(i,k2,min(k3,pverp))*min(1,pverp2-k3)
               tmp2(i) = s(i,k2,min(km4,pverp))*min(1,pverp2-km4)
               emx0 = (fdl(i,k1)-fsdl(i,k1))/((fclb4(i,km1)-tmp2(i)+tmp(i))-fsdl(i,k1))
               if (emx0 >= 0.0 .and. emx0 <= 1.0) exit
            end do
            km1 = min(km1,k1-2)
            ksort(0) = km1 + 1
!
! Loop to calculate fluxes at level k
!
            nxsk = 0
            do k = kx1(i,irgn), kx2(i,irgn)
!
! Identify clouds (largest to smallest area) between kx1 and k
!    Since nxsk will increase with increasing k up to nxs(i,irgn), once
!    nxsk == nxs(i,irgn) then use the list constructed for previous k
!
               if (nxsk < nxs(i,irgn)) then
                  nxsk = 0
                  do l = 1, nxs(i,irgn)
                     k1 = kxs(l,i,irgn)
                     if (k >= k1) then
                        nxsk = nxsk + 1
                        ksort(nxsk) = k1
                     endif
                  end do
               endif
!
! Dummy value of index to insure computation of cloud amt is valid for l=nxsk+1
!
               ksort(nxsk+1) = pverp
!
! Initialize iterated emissivity factors
!
               do l = 1, nxsk
                  emx(l) = emis(i,ksort(l))
               end do
!
! Initialize iterated emissivity factor for bnd. condition at upper interface
!
               emx(0) = emx0
!
! Initialize previous cloud amounts
!
               cld0 = 1.0
!
! Indices for flux calculations
!
               k2 = k+1
               k3 = k2+1
               tmp(i) = s(i,k2,min(k3,pverp))*min(1,pverp2-k3)
!
! Loop over number of cloud levels inside region (biggest to smallest cld area)
!
               do l = 1, nxsk+1
!
! Calculate downward fluxes
!
                  cld1 = cldp(i,ksort(l))*min(1,nxsk+1-l)
                  if (cld0 /= cld1) then
                     fdl(i,k2) = fdl(i,k2)+(cld0-cld1)*fsdl(i,k2)
                     do l1 = 0, l - 1
                        km1 = ksort(l1)-1
                        km4 = km1+3
                        tmp2(i) = s(i,k2,min(km4,pverp))* min(1,pverp2-km4)
                        fdl(i,k2) = fdl(i,k2)+(cld0-cld1)*emx(l1)*(fclb4(i,km1)-tmp2(i)+tmp(i)- &
                                    fsdl(i,k2))
                     end do
                  endif
                  cld0 = cld1
!
! Multiply emissivity factors by current cloud transmissivity
!
                  if (l <= nxsk) then
                     k1 = ksort(l)
                     trans = 1.0-emis(i,k1)
!
! Ideally the upper bound on l1 would be l-1, but the sort routine
!    scrambles the order of layers with identical cloud amounts
!
                     do l1 = 0, nxsk
                        if (ksort(l1) < k1) then
                           emx(l1) = emx(l1)*trans
                        endif
                     end do
                  end if
!
! End loop over number l of cloud levels
!
               end do
!
! End loop over level k for fluxes
!
            end do
!
! End loop over longitude i for fluxes
!
         end do
!
! End loop over regions irgn for max-overlap
!
      end do

!
!----------------------------------------------------------------------
! UPWARD FLUXES:
! Outermost loop over regions (sets of adjacent layers) to be max overlapped
!
      do irgn = nmxrgn(ilon), 1, -1
!
! Compute clear-sky fluxes for regions without clouds
!
         iimx = 1
         if (ilon < indxmx(iimx,irgn) .and. irgn <= nrgn(ilon)) then
!
! Calculate emissivity so that upward flux at lower boundary of region
!    can be cast in form of solution for upward flux from cloud below that
!    boundary.  Then solutions for fluxes at other levels take form of
!    random overlap expressions.  Try to locate "cloud" as close as possible
!    to surface such that the "cloud" pseudo-emissivity is between 0 and 1.
! Include allowance for surface emissivity (both numerator and denominator
!    equal 1)
!
            k1 = kx2(ilon,irgn)+1
            if (k1 < pverp) then
               do km1 = pver-1,kx2(ilon,irgn),-1
                  km3 = km1+2
                  k2 = k1
                  k3 = k2+1
                  tmp(ilon) = s(ilon,k2,min(km3,pverp))* min(1,pverp2-km3)
                  emx0 = (ful(ilon,k1)-fsul(ilon,k1))/ &
                         ((fclt4(ilon,km1)+s(ilon,k2,k3)-tmp(ilon))- fsul(ilon,k1))
                  if (emx0 >= 0.0 .and. emx0 <= 1.0) exit
               end do
               km1 = max(km1,kx2(ilon,irgn))
            else
               km1 = k1-1
               km3 = km1+2
               emx0 = 1.0
            endif

            do k2 = kx1(ilon,irgn), kx2(ilon,irgn)
               k3 = k2+1
!
! If km3 == pver+2, one of the s integrals = 0 (integration limits both = p_s)
!
               tmp(ilon) = s(ilon,k2,min(km3,pverp))* min(1,pverp2-km3)
               ful(ilon,k2) =(1.0-emx0)*fsul(ilon,k2) + emx0* &
                             (fclt4(ilon,km1)+s(ilon,k2,k3)-tmp(ilon))
            end do
         else if (ilon==indxmx(iimx,irgn) .and. iimx<=ncolmx(irgn)) then
            iimx = iimx+1
         end if
!
! Outer loop over columns with clouds in the max-overlap region
!
         do iimx = 1, ncolmx(irgn)
            i = indxmx(iimx,irgn)

!
! Calculate emissivity so that upward flux at lower boundary of region
!    can be cast in form of solution for upward flux from cloud at that
!    boundary.  Then solutions for fluxes at other levels take form of
!    random overlap expressions.  Try to locate "cloud" as close as possible
!    to surface such that the "cloud" pseudo-emissivity is between 0 and 1.
! Include allowance for surface emissivity (both numerator and denominator
!    equal 1)
!
            k1 = kx2(i,irgn)+1
            if (k1 < pverp) then
               do km1 = pver-1,kx2(i,irgn),-1
                  km3 = km1+2
                  k2 = k1
                  k3 = k2+1
                  tmp(i) = s(i,k2,min(km3,pverp))*min(1,pverp2-km3)
                  emx0 = (ful(i,k1)-fsul(i,k1))/((fclt4(i,km1)+s(i,k2,k3)-tmp(i))-fsul(i,k1))
                  if (emx0 >= 0.0 .and. emx0 <= 1.0) exit
               end do
               km1 = max(km1,kx2(i,irgn))
            else
               emx0 = 1.0
               km1 = k1-1
            endif
            ksort(0) = km1 + 1

!
! Loop to calculate fluxes at level k
!
            nxsk = 0
            do k = kx2(i,irgn), kx1(i,irgn), -1
!
! Identify clouds (largest to smallest area) between k and kx2
!    Since nxsk will increase with decreasing k up to nxs(i,irgn), once
!    nxsk == nxs(i,irgn) then use the list constructed for previous k
!
               if (nxsk < nxs(i,irgn)) then
                  nxsk = 0
                  do l = 1, nxs(i,irgn)
                     k1 = kxs(l,i,irgn)
                     if (k <= k1) then
                        nxsk = nxsk + 1
                        ksort(nxsk) = k1
                     endif
                  end do
               endif
!
! Dummy value of index to insure computation of cloud amt is valid for l=nxsk+1
!
               ksort(nxsk+1) = pverp
!
! Initialize iterated emissivity factors
!
               do l = 1, nxsk
                  emx(l) = emis(i,ksort(l))
               end do
!
! Initialize iterated emissivity factor for bnd. condition at lower interface
!
               emx(0) = emx0
!
! Initialize previous cloud amounts
!
               cld0 = 1.0
!
! Indices for flux calculations
!
               k2 = k
               k3 = k2+1
!
! Loop over number of cloud levels inside region (biggest to smallest cld area)
!
               do l = 1, nxsk+1
!
! Calculate upward fluxes
!
                  cld1 = cldp(i,ksort(l))*min(1,nxsk+1-l)
                  if (cld0 /= cld1) then
                     ful(i,k2) = ful(i,k2)+(cld0-cld1)*fsul(i,k2)
                     do l1 = 0, l - 1
                        km1 = ksort(l1)-1
                        km3 = km1+2
!
! If km3 == pver+2, one of the s integrals = 0 (integration limits both = p_s)
!
                        tmp(i) = s(i,k2,min(km3,pverp))* min(1,pverp2-km3)
                        ful(i,k2) = ful(i,k2)+(cld0-cld1)*emx(l1)* &
                                   (fclt4(i,km1)+s(i,k2,k3)-tmp(i)- fsul(i,k2))
                     end do
                  endif
                  cld0 = cld1
!
! Multiply emissivity factors by current cloud transmissivity
!
                  if (l <= nxsk) then
                     k1 = ksort(l)
                     trans = 1.0-emis(i,k1)
!
! Ideally the upper bound on l1 would be l-1, but the sort routine
!    scrambles the order of layers with identical cloud amounts
!
                     do l1 = 0, nxsk
                        if (ksort(l1) > k1) then
                           emx(l1) = emx(l1)*trans
                        endif
                     end do
                  end if
!
! End loop over number l of cloud levels
!
               end do
!
! End loop over level k for fluxes
!
            end do
!
! End loop over longitude i for fluxes
!
         end do
!
! End loop over regions irgn for max-overlap
!
      end do
!
! End outermost longitude loop
!
   end do
!
! End cloud modification loops
!
!----------------------------------------------------------------------
! All longitudes: store history tape quantities
!
   do i=1,ncol
      flwds(i) = fdl (i,pverp )
      flns(i)  = ful (i,pverp ) - fdl (i,pverp )
      flnsc(i) = fsul(i,pverp ) - fsdl(i,pverp )
      flnt(i)  = ful (i,ntoplw) - fdl (i,ntoplw)
      flntc(i) = fsul(i,ntoplw) - fsdl(i,ntoplw)
      flut(i)  = ful (i,ntoplw)
      flutc(i) = fsul(i,ntoplw)
   end do
!
! Computation of longwave heating (J/kg/s)
!
   do k=ntoplw,pver
      do i=1,ncol
         qrl(i,k) = (ful(i,k) - fdl(i,k) - ful(i,k+1) + fdl(i,k+1))* &
                     1.E-4*gravit/((pint(i,k) - pint(i,k+1)))
      end do
   end do
! Return 0 above solution domain
   if ( ntoplw > 1 )then
      qrl(:ncol,:ntoplw-1) = 0.
   end if

! Added downward/upward total and clear sky fluxes
!
   do k=ntoplw,pverp
      do i=1,ncol
        flup(i,k)  = ful(i,k)
        flupc(i,k) = fsul(i,k)
        fldn(i,k)  = fdl(i,k)
        fldnc(i,k) = fsdl(i,k)
      end do
   end do
! Return 0 above solution domain
   if ( ntoplw > 1 )then
      flup(:ncol,:ntoplw-1) = 0.
      flupc(:ncol,:ntoplw-1) = 0.
      fldn(:ncol,:ntoplw-1) = 0.
      fldnc(:ncol,:ntoplw-1) = 0.
   end if
!
   return
end subroutine radclwmx

subroutine radcswmx(jj, lchnk   ,ncol    ,pcols, pver, pverp,         &
                    pint    ,pmid    ,h2ommr  ,rh      ,o3mmr   , &
                    aermmr  ,cld     ,cicewp  ,cliqwp  ,rel     , &
!                   rei     ,eccf    ,coszrs  ,scon    ,solin   ,solcon,  &
                    rei     ,tauxcl  ,tauxci  ,eccf    ,coszrs  ,scon    ,solin   ,solcon,  &
                    asdir   ,asdif   ,aldir   ,aldif   ,nmxrgn  , &
                    pmxrgn  ,qrs     ,fsnt    ,fsntc   ,fsntoa  , &
                    fsntoac ,fsnirtoa,fsnrtoac,fsnrtoaq,fsns    , &
                    fsnsc   ,fsdsc   ,fsds    ,sols    ,soll    , &
                    solsd   ,solld   ,frc_day ,                   &
                    fsup    ,fsupc   ,fsdn    ,fsdnc   ,          &
                    fsdndir ,fsdncdir,fsdndif ,fsdncdif,          & ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes profiles
                    fsdsdir ,fsdsdif ,fsdscdir,fsdscdif,          & ! amontornes-bcodina (2014-04-20) Dir/Dif fluxes sfc
                    aertau  ,aerssa  ,aerasm  ,aerfwd             )
!-----------------------------------------------------------------------
! 
! Purpose: 
! Solar radiation code
! 
! Method: 
! Basic method is Delta-Eddington as described in:
! 
! Briegleb, Bruce P., 1992: Delta-Eddington
! Approximation for Solar Radiation in the NCAR Community Climate Model,
! Journal of Geophysical Research, Vol 97, D7, pp7603-7612).
! 
! Five changes to the basic method described above are:
! (1) addition of sulfate aerosols (Kiehl and Briegleb, 1993)
! (2) the distinction between liquid and ice particle clouds 
! (Kiehl et al, 1996);
! (3) provision for calculating TOA fluxes with spectral response to
! match Nimbus-7 visible/near-IR radiometers (Collins, 1998);
! (4) max-random overlap (Collins, 2001)
! (5) The near-IR absorption by H2O was updated in 2003 by Collins, 
!     Lee-Taylor, and Edwards for consistency with the new line data in
!     Hitran 2000 and the H2O continuum version CKD 2.4.  Modifications
!     were optimized by reducing RMS errors in heating rates relative
!     to a series of benchmark calculations for the 5 standard AFGL 
!     atmospheres.  The benchmarks were performed using DISORT2 combined
!     with GENLN3.  The near-IR scattering optical depths for Rayleigh
!     scattering were also adjusted, as well as the correction for
!     stratospheric heating by H2O.
!
! The treatment of maximum-random overlap is described in the
! comment block "INDEX CALCULATIONS FOR MAX OVERLAP".
! 
! Divides solar spectrum into 19 intervals from 0.2-5.0 micro-meters.
! solar flux fractions specified for each interval. allows for
! seasonally and diurnally varying solar input.  Includes molecular,
! cloud, aerosol, and surface scattering, along with h2o,o3,co2,o2,cloud, 
! and surface absorption. Computes delta-eddington reflections and
! transmissions assuming homogeneously mixed layers. Adds the layers 
! assuming scattering between layers to be isotropic, and distinguishes 
! direct solar beam from scattered radiation.
! 
! Longitude loops are broken into 1 or 2 sections, so that only daylight
! (i.e. coszrs > 0) computations are done.
! 
! Note that an extra layer above the model top layer is added.
! 
! cgs units are used.
! 
! Special diagnostic calculation of the clear sky surface and total column
! absorbed flux is also done for cloud forcing diagnostics.
! 
!-----------------------------------------------------------------------
!  use shr_kind_mod, only: r8 => shr_kind_r8
!  use ppgrid
!  use ghg_surfvals, only: co2mmr
!  use prescribed_aerosols, only: idxBG, idxSUL, idxSSLT, idxOCPHO, idxBCPHO, idxOCPHI, idxBCPHI, &
!    idxDUSTfirst, numDUST, idxVOLC, naer_all
!  use aer_optics, only: nrh, ndstsz, ksul, wsul, gsul, &
!    ksslt, wsslt, gsslt, kcphil, wcphil, gcphil, kcphob, wcphob, gcphob, &
!    kcb, wcb, gcb, kdst, wdst, gdst, kbg, wbg, gbg, kvolc, wvolc, gvolc
!  use abortutils, only: endrun

   implicit none

   integer nspint            ! Num of spctrl intervals across solar spectrum
   integer naer_groups       ! Num of aerosol groups for optical diagnostics

   parameter ( nspint = 19 )
   parameter ( naer_groups = 7 )    ! current groupings are sul, sslt, all carbons, all dust, and all aerosols
!-----------------------Constants for new band (640-700 nm)-------------

!-------------Parameters for accelerating max-random solution-------------
! 
! The solution time scales like prod(j:1->N) (1 + n_j) where 
! N   = number of max-overlap regions (nmxrgn)
! n_j = number of unique cloud amounts in region j
! 
! Therefore the solution cost can be reduced by decreasing n_j.
! cldmin reduces n_j by treating cloud amounts < cldmin as clear sky.
! cldeps reduces n_j by treating cloud amounts identical to log(1/cldeps)
! decimal places as identical
! 
! areamin reduces the cost by dropping configurations that occupy
! a surface area < areamin of the model grid box.  The surface area
! for a configuration C(j,k_j), where j is the region number and k_j is the
! index for a unique cloud amount (in descending order from biggest to
! smallest clouds) in region j, is
! 
! A = prod(j:1->N) [C(j,k_j) - C(j,k_j+1)]
! 
! where C(j,0) = 1.0 and C(j,n_j+1) = 0.0.
! 
! nconfgmax reduces the cost and improves load balancing by setting an upper
! bound on the number of cloud configurations in the solution.  If the number
! of configurations exceeds nconfgmax, the nconfgmax configurations with the
! largest area are retained, and the fluxes are normalized by the total area
! of these nconfgmax configurations.  For the current max/random overlap 
! assumption (see subroutine cldovrlap), 30 levels, and cloud-amount 
! parameterization, the mean and RMS number of configurations are 
! both roughly 5.  nconfgmax has been set to the mean+2*RMS number, or 15.
! 
! Minimum cloud amount (as a fraction of the grid-box area) to 
! distinguish from clear sky
! 
   real(r8) cldmin
   parameter (cldmin = 1.0e-80_r8)
! 
! Minimimum horizontal area (as a fraction of the grid-box area) to retain 
! for a unique cloud configuration in the max-random solution
! 
   real(r8) areamin
   parameter (areamin = 0.01_r8)
! 
! Decimal precision of cloud amount (0 -> preserve full resolution;
! 10^-n -> preserve n digits of cloud amount)
! 
   real(r8) cldeps
   parameter (cldeps = 0.0_r8)
! 
! Maximum number of configurations to include in solution
! 
   integer nconfgmax
   parameter (nconfgmax = 15)
!------------------------------Commons----------------------------------
! 
! Input arguments
! 
   integer, intent(in) :: lchnk,jj             ! chunk identifier
   integer, intent(in) :: pcols, pver, pverp
   integer, intent(in) :: ncol              ! number of atmospheric columns

   real(r8), intent(in) :: pmid(pcols,pver) ! Level pressure
   real(r8), intent(in) :: pint(pcols,pverp) ! Interface pressure
   real(r8), intent(in) :: h2ommr(pcols,pver) ! Specific humidity (h2o mass mix ratio)
   real(r8), intent(in) :: o3mmr(pcols,pver) ! Ozone mass mixing ratio
   real(r8), intent(in) :: aermmr(pcols,pver,naer_all) ! Aerosol mass mixing ratio
   real(r8), intent(in) :: rh(pcols,pver)   ! Relative humidity (fraction)
! 
   real(r8), intent(in) :: cld(pcols,pver)  ! Fractional cloud cover
   real(r8), intent(in) :: cicewp(pcols,pver) ! in-cloud cloud ice water path
   real(r8), intent(in) :: cliqwp(pcols,pver) ! in-cloud cloud liquid water path
   real(r8), intent(in) :: rel(pcols,pver)  ! Liquid effective drop size (microns)
   real(r8), intent(in) :: rei(pcols,pver)  ! Ice effective drop size (microns)
! 
   real(r8), intent(in) :: eccf             ! Eccentricity factor (1./earth-sun dist^2)
   real, intent(in) :: solcon           ! solar constant with eccentricity factor
   real(r8), intent(in) :: coszrs(pcols)    ! Cosine solar zenith angle
   real(r8), intent(in) :: asdir(pcols)     ! 0.2-0.7 micro-meter srfc alb: direct rad
   real(r8), intent(in) :: aldir(pcols)     ! 0.7-5.0 micro-meter srfc alb: direct rad
   real(r8), intent(in) :: asdif(pcols)     ! 0.2-0.7 micro-meter srfc alb: diffuse rad
   real(r8), intent(in) :: aldif(pcols)     ! 0.7-5.0 micro-meter srfc alb: diffuse rad

   real(r8), intent(in) :: scon             ! solar constant 
! 
! IN/OUT arguments
! 
   real(r8), intent(inout) :: pmxrgn(pcols,pverp) ! Maximum values of pressure for each
!                                                 !    maximally overlapped region. 
!                                                 !    0->pmxrgn(i,1) is range of pressure for
!                                                 !    1st region,pmxrgn(i,1)->pmxrgn(i,2) for
!                                                 !    2nd region, etc
   integer, intent(inout) ::  nmxrgn(pcols)    ! Number of maximally overlapped regions
! 
! Output arguments
! 

   real(r8), intent(out) :: solin(pcols)     ! Incident solar flux
   real(r8), intent(out) :: qrs(pcols,pver)  ! Solar heating rate
   real(r8), intent(out) :: fsns(pcols)      ! Surface absorbed solar flux
   real(r8), intent(out) :: fsnt(pcols)      ! Total column absorbed solar flux
   real(r8), intent(out) :: fsntoa(pcols)    ! Net solar flux at TOA
   real(r8), intent(out) :: fsds(pcols)      ! Flux shortwave downwelling surface
! 
   real(r8), intent(out) :: fsnsc(pcols)     ! Clear sky surface absorbed solar flux
   real(r8), intent(out) :: fsdsc(pcols)     ! Clear sky surface downwelling solar flux

   real(r8), intent(out) :: fsdscdir(pcols)  ! Clear sky surface direct downwelling solar flux (amontornes-bcodina 2014-04-20)
   real(r8), intent(out) :: fsdscdif(pcols)  ! Clear sky surface diffuse downwelling solar flux (amontornes-bcodina 2014-04-20)
   real(r8), intent(out) :: fsdsdir(pcols)   ! Clear sky surface direct downwelling solar flux (amontornes-bcodina 2014-04-20)
   real(r8), intent(out) :: fsdsdif(pcols)   ! Clear sky surface diffuse downwelling solar flux (amontornes-bcodina 2014-04-20)

   real(r8), intent(out) :: fsntc(pcols)     ! Clear sky total column absorbed solar flx
   real(r8), intent(out) :: fsntoac(pcols)   ! Clear sky net solar flx at TOA
   real(r8), intent(out) :: sols(pcols)      ! Direct solar rad on surface (< 0.7)
   real(r8), intent(out) :: soll(pcols)      ! Direct solar rad on surface (>= 0.7)
   real(r8), intent(out) :: solsd(pcols)     ! Diffuse solar rad on surface (< 0.7)
   real(r8), intent(out) :: solld(pcols)     ! Diffuse solar rad on surface (>= 0.7)
   real(r8), intent(out) :: fsnirtoa(pcols)  ! Near-IR flux absorbed at toa
   real(r8), intent(out) :: fsnrtoac(pcols)  ! Clear sky near-IR flux absorbed at toa
   real(r8), intent(out) :: fsnrtoaq(pcols)  ! Net near-IR flux at toa >= 0.7 microns
   real(r8), intent(out) :: tauxcl(pcols,0:pver) ! water cloud extinction optical depth
   real(r8), intent(out) :: tauxci(pcols,0:pver) ! ice cloud extinction optical depth

! Added downward/upward total and clear sky fluxes
   real(r8), intent(out) :: fsup(pcols,pverp)      ! Total sky upward solar flux (spectrally summed)
   real(r8), intent(out) :: fsupc(pcols,pverp)     ! Clear sky upward solar flux (spectrally summed)
   real(r8), intent(out) :: fsdn(pcols,pverp)      ! Total sky downward solar flux (spectrally summed)
   real(r8), intent(out) :: fsdnc(pcols,pverp)     ! Clear sky downward solar flux (spectrally summed)
   real(r8), intent(out) :: fsdndir(pcols,pverp)   ! Total sky direct downward solar flux (spectrally summed) amontornes-bcodina (2014-04-20)
   real(r8), intent(out) :: fsdncdir(pcols,pverp)  ! Clear sky direct downward solar flux (spectrally summed) amontornes-bcodina (2014-04-20)
   real(r8), intent(out) :: fsdndif(pcols,pverp)   ! Total sky diffuse downward solar flux (spectrally summed) amontornes-bcodina (2014-04-20)
   real(r8), intent(out) :: fsdncdif(pcols,pverp)  ! Clear sky diffuse downward solar flux (spectrally summed) amontornes-bcodina (2014-04-20)
!
   real(r8) , intent(out) :: frc_day(pcols) ! = 1 for daylight, =0 for night columns
   real(r8) :: aertau(pcols,nspint,naer_groups) ! Aerosol column optical depth
   real(r8) :: aerssa(pcols,nspint,naer_groups) ! Aerosol column averaged single scattering albedo
   real(r8) :: aerasm(pcols,nspint,naer_groups) ! Aerosol column averaged asymmetry parameter
   real(r8) :: aerfwd(pcols,nspint,naer_groups) ! Aerosol column averaged forward scattering
!  real(r8), intent(out) :: aertau(pcols,nspint,naer_groups) ! Aerosol column optical depth
!  real(r8), intent(out) :: aerssa(pcols,nspint,naer_groups) ! Aerosol column averaged single scattering albedo
!  real(r8), intent(out) :: aerasm(pcols,nspint,naer_groups) ! Aerosol column averaged asymmetry parameter
!  real(r8), intent(out) :: aerfwd(pcols,nspint,naer_groups) ! Aerosol column averaged forward scattering
! 
!---------------------------Local variables-----------------------------
! 
! Max/random overlap variables
! 
   real(r8) asort(pverp)     ! 1 - cloud amounts to be sorted for max ovrlp.
   real(r8) atmp             ! Temporary storage for sort when nxs = 2
   real(r8) cld0             ! 1 - (cld amt) used to make wstr, cstr, nstr
   real(r8) totwgt           ! Total of xwgts = total fractional area of 
!   grid-box covered by cloud configurations
!   included in solution to fluxes

   real(r8) wgtv(nconfgmax)  ! Weights for fluxes
!   1st index is configuration number
   real(r8) wstr(pverp,pverp) ! area weighting factors for streams
!   1st index is for stream #, 
!   2nd index is for region #

   real(r8) xexpt            ! solar direct beam trans. for layer above
   real(r8) xrdnd            ! diffuse reflectivity for layer above
   real(r8) xrupd            ! diffuse reflectivity for layer below
   real(r8) xrups            ! direct-beam reflectivity for layer below
   real(r8) xtdnt            ! total trans for layers above

   real(r8) xwgt             ! product of cloud amounts

   real(r8) yexpt            ! solar direct beam trans. for layer above
   real(r8) yrdnd            ! diffuse reflectivity for layer above
   real(r8) yrupd            ! diffuse reflectivity for layer below
   real(r8) ytdnd            ! dif-beam transmission for layers above
   real(r8) ytupd            ! dif-beam transmission for layers below

   real(r8) zexpt            ! solar direct beam trans. for layer above
   real(r8) zrdnd            ! diffuse reflectivity for layer above
   real(r8) zrupd            ! diffuse reflectivity for layer below
   real(r8) zrups            ! direct-beam reflectivity for layer below
   real(r8) ztdnt            ! total trans for layers above

   logical new_term          ! Flag for configurations to include in fluxes
   logical region_found      ! flag for identifying regions

   integer ccon(0:pverp,nconfgmax)                                
! flags for presence of clouds
!   1st index is for level # (including 
!    layer above top of model and at surface)
!   2nd index is for configuration #
   integer cstr(0:pverp,pverp)                                
! flags for presence of clouds
!   1st index is for level # (including 
!    layer above top of model and at surface)
!   2nd index is for stream #
   integer icond(0:pverp,nconfgmax)
! Indices for copying rad. properties from
!     one identical downward cld config.
!     to another in adding method (step 2)
!   1st index is for interface # (including 
!     layer above top of model and at surface)
!   2nd index is for configuration # range
   integer iconu(0:pverp,nconfgmax)
! Indices for copying rad. properties from
!     one identical upward configuration
!     to another in adding method (step 2)
!   1st index is for interface # (including 
!     layer above top of model and at surface)
!   2nd index is for configuration # range
   integer iconfig           ! Counter for random-ovrlap configurations
   integer irgn              ! Index for max-overlap regions
   integer is0               ! Lower end of stream index range
   integer is1               ! Upper end of stream index range
   integer isn               ! Stream index
   integer istr(pverp+1)     ! index for stream #s during flux calculation
   integer istrtd(0:pverp,0:nconfgmax+1)
! indices into icond 
!   1st index is for interface # (including 
!     layer above top of model and at surface)
!   2nd index is for configuration # range
   integer istrtu(0:pverp,0:nconfgmax+1)
! indices into iconu 
!   1st index is for interface # (including 
!     layer above top of model and at surface)
!   2nd index is for configuration # range
   integer j                 ! Configuration index
   integer k1                ! Level index
   integer k2                ! Level index
   integer ksort(pverp)      ! Level indices of cloud amounts to be sorted
   integer ktmp              ! Temporary storage for sort when nxs = 2
   integer kx1(0:pverp)      ! Level index for top of max-overlap region
   integer kx2(0:pverp)      ! Level index for bottom of max-overlap region
   integer l                 ! Index 
   integer l0                ! Index
   integer mrgn              ! Counter for nrgn
   integer mstr              ! Counter for nstr
   integer n0                ! Number of configurations with ccon(k,:)==0
   integer n1                ! Number of configurations with ccon(k,:)==1
   integer nconfig           ! Number of random-ovrlap configurations
   integer nconfigm          ! Value of config before testing for areamin,
!    nconfgmax
   integer npasses           ! number of passes over the indexing loop
   integer nrgn              ! Number of max overlap regions at current 
!    longitude
   integer nstr(pverp)       ! Number of unique cloud configurations
!   ("streams") in a max-overlapped region
!   1st index is for region #
   integer nuniq             ! # of unique cloud configurations
   integer nuniqd(0:pverp)   ! # of unique cloud configurations: TOA 
!   to level k
   integer nuniqu(0:pverp)   ! # of unique cloud configurations: surface
!   to level k 
   integer nxs               ! Number of cloudy layers between k1 and k2 
   integer ptr0(nconfgmax)   ! Indices of configurations with ccon(k,:)==0
   integer ptr1(nconfgmax)   ! Indices of configurations with ccon(k,:)==1
   integer ptrc(nconfgmax)   ! Pointer for configurations sorted by wgtv
!  integer findvalue         ! Function for finding kth smallest element
!   in a vector
!  external findvalue

! 
! Other
! 
   integer ns                ! Spectral loop index
   integer i                 ! Longitude loop index
   integer k                 ! Level loop index
   integer km1               ! k - 1
   integer kp1               ! k + 1
   integer n                 ! Loop index for daylight
   integer ndayc             ! Number of daylight columns
   integer idayc(pcols)      ! Daytime column indices
   integer indxsl            ! Index for cloud particle properties
   integer ksz               ! dust size bin index
   integer krh               ! relative humidity bin index
   integer kaer              ! aerosol group index
   real(r8) wrh              ! weight for linear interpolation between lut points
   real(r8) :: rhtrunc       ! rh, truncated for the purposes of extrapolating
                             ! aerosol optical properties 
   real(r8) albdir(pcols,nspint) ! Current spc intrvl srf alb to direct rad
   real(r8) albdif(pcols,nspint) ! Current spc intrvl srf alb to diffuse rad
! 
   real(r8) wgtint           ! Weight for specific spectral interval

! 
! Diagnostic and accumulation arrays; note that sfltot, fswup, and
! fswdn are not used in the computation,but are retained for future use.
! 
   real(r8) solflx           ! Solar flux in current interval
   real(r8) sfltot           ! Spectrally summed total solar flux
   real(r8) totfld(0:pver)   ! Spectrally summed flux divergence
   real(r8) fswup(0:pverp)   ! Spectrally summed up flux
   real(r8) fswdn(0:pverp)   ! Spectrally summed down flux
   real(r8) fswupc(0:pverp)  ! Spectrally summed up clear sky flux
   real(r8) fswdnc(0:pverp)  ! Spectrally summed down clear sky flux
   real(r8) fswdndir(0:pverp) ! Spectrally summed direct flux in all sky    (amontornes-bcodina 2014-04-20)
   real(r8) fswdncdir(0:pverp)! Spectrally summed direct flux in clear sky  (amontornes-bcodina 2014-04-20)
! 
! Cloud radiative property arrays
! 
!  real(r8) tauxcl(pcols,0:pver) ! water cloud extinction optical depth
!  real(r8) tauxci(pcols,0:pver) ! ice cloud extinction optical depth
   real(r8) wcl(pcols,0:pver) ! liquid cloud single scattering albedo
   real(r8) gcl(pcols,0:pver) ! liquid cloud asymmetry parameter
   real(r8) fcl(pcols,0:pver) ! liquid cloud forward scattered fraction
   real(r8) wci(pcols,0:pver) ! ice cloud single scattering albedo
   real(r8) gci(pcols,0:pver) ! ice cloud asymmetry parameter
   real(r8) fci(pcols,0:pver) ! ice cloud forward scattered fraction
!
! Aerosol mass paths by species
!
  real(r8) usul(pcols,pver)   ! sulfate (SO4)
  real(r8) ubg(pcols,pver)    ! background aerosol
  real(r8) usslt(pcols,pver)  ! sea-salt (SSLT)
  real(r8) ucphil(pcols,pver) ! hydrophilic organic carbon (OCPHI)
  real(r8) ucphob(pcols,pver) ! hydrophobic organic carbon (OCPHO)
  real(r8) ucb(pcols,pver)    ! black carbon (BCPHI + BCPHO)
  real(r8) uvolc(pcols,pver) ! volcanic mass
  real(r8) udst(ndstsz,pcols,pver) ! dust

!
! local variables used for the external mixing of aerosol species
!
  real(r8) tau_sul             ! optical depth, sulfate
  real(r8) tau_bg              ! optical depth, background aerosol
  real(r8) tau_sslt            ! optical depth, sea-salt
  real(r8) tau_cphil           ! optical depth, hydrophilic carbon
  real(r8) tau_cphob           ! optical depth, hydrophobic carbon
  real(r8) tau_cb              ! optical depth, black carbon
  real(r8) tau_volc            ! optical depth, volcanic
  real(r8) tau_dst(ndstsz)     ! optical depth, dust, by size category
  real(r8) tau_dst_tot         ! optical depth, total dust
  real(r8) tau_tot             ! optical depth, total aerosol

  real(r8) tau_w_sul           ! optical depth * single scattering albedo, sulfate
  real(r8) tau_w_bg            ! optical depth * single scattering albedo, background aerosol
  real(r8) tau_w_sslt          ! optical depth * single scattering albedo, sea-salt
  real(r8) tau_w_cphil         ! optical depth * single scattering albedo, hydrophilic carbon
  real(r8) tau_w_cphob         ! optical depth * single scattering albedo, hydrophobic carbon
  real(r8) tau_w_cb            ! optical depth * single scattering albedo, black carbon
  real(r8) tau_w_volc          ! optical depth * single scattering albedo, volcanic
  real(r8) tau_w_dst(ndstsz)   ! optical depth * single scattering albedo, dust, by size
  real(r8) tau_w_dst_tot       ! optical depth * single scattering albedo, total dust
  real(r8) tau_w_tot           ! optical depth * single scattering albedo, total aerosol

  real(r8) tau_w_g_sul         ! optical depth * single scattering albedo * asymmetry parameter, sulfate
  real(r8) tau_w_g_bg          ! optical depth * single scattering albedo * asymmetry parameter, background aerosol
  real(r8) tau_w_g_sslt        ! optical depth * single scattering albedo * asymmetry parameter, sea-salt
  real(r8) tau_w_g_cphil       ! optical depth * single scattering albedo * asymmetry parameter, hydrophilic carbon
  real(r8) tau_w_g_cphob       ! optical depth * single scattering albedo * asymmetry parameter, hydrophobic carbon
  real(r8) tau_w_g_cb          ! optical depth * single scattering albedo * asymmetry parameter, black carbon
  real(r8) tau_w_g_volc        ! optical depth * single scattering albedo * asymmetry parameter, volcanic
  real(r8) tau_w_g_dst(ndstsz) ! optical depth * single scattering albedo * asymmetry parameter, dust, by size
  real(r8) tau_w_g_dst_tot     ! optical depth * single scattering albedo * asymmetry parameter, total dust
  real(r8) tau_w_g_tot         ! optical depth * single scattering albedo * asymmetry parameter, total aerosol

  real(r8) f_sul               ! forward scattering fraction, sulfate
  real(r8) f_bg                ! forward scattering fraction, background aerosol
  real(r8) f_sslt              ! forward scattering fraction, sea-salt
  real(r8) f_cphil             ! forward scattering fraction, hydrophilic carbon
  real(r8) f_cphob             ! forward scattering fraction, hydrophobic carbon
  real(r8) f_cb                ! forward scattering fraction, black carbon
  real(r8) f_volc              ! forward scattering fraction, volcanic
  real(r8) f_dst(ndstsz)       ! forward scattering fraction, dust, by size
  real(r8) f_dst_tot           ! forward scattering fraction, total dust
  real(r8) f_tot               ! forward scattering fraction, total aerosol

  real(r8) tau_w_f_sul         ! optical depth * forward scattering fraction * single scattering albedo, sulfate
  real(r8) tau_w_f_bg          ! optical depth * forward scattering fraction * single scattering albedo, background
  real(r8) tau_w_f_sslt        ! optical depth * forward scattering fraction * single scattering albedo, sea-salt
  real(r8) tau_w_f_cphil       ! optical depth * forward scattering fraction * single scattering albedo, hydrophilic C
  real(r8) tau_w_f_cphob       ! optical depth * forward scattering fraction * single scattering albedo, hydrophobic C
  real(r8) tau_w_f_cb          ! optical depth * forward scattering fraction * single scattering albedo, black C
  real(r8) tau_w_f_volc        ! optical depth * forward scattering fraction * single scattering albedo, volcanic
  real(r8) tau_w_f_dst(ndstsz) ! optical depth * forward scattering fraction * single scattering albedo, dust, by size
  real(r8) tau_w_f_dst_tot     ! optical depth * forward scattering fraction * single scattering albedo, total dust
  real(r8) tau_w_f_tot         ! optical depth * forward scattering fraction * single scattering albedo, total aerosol
  real(r8) w_dst_tot           ! single scattering albedo, total dust
  real(r8) w_tot               ! single scattering albedo, total aerosol
  real(r8) g_dst_tot           ! asymmetry parameter, total dust
  real(r8) g_tot               ! asymmetry parameter, total aerosol
  real(r8) ksuli               ! specific extinction interpolated between rh look-up-table points, sulfate
  real(r8) ksslti              ! specific extinction interpolated between rh look-up-table points, sea-salt
  real(r8) kcphili             ! specific extinction interpolated between rh look-up-table points, hydrophilic carbon
  real(r8) wsuli               ! single scattering albedo interpolated between rh look-up-table points, sulfate
  real(r8) wsslti              ! single scattering albedo interpolated between rh look-up-table points, sea-salt
  real(r8) wcphili             ! single scattering albedo interpolated between rh look-up-table points, hydrophilic carbon
  real(r8) gsuli               ! asymmetry parameter interpolated between rh look-up-table points, sulfate
  real(r8) gsslti              ! asymmetry parameter interpolated between rh look-up-table points, sea-salt
  real(r8) gcphili             ! asymmetry parameter interpolated between rh look-up-table points, hydrophilic carbon
! 
! Aerosol radiative property arrays
! 
   real(r8) tauxar(pcols,0:pver) ! aerosol extinction optical depth
   real(r8) wa(pcols,0:pver) ! aerosol single scattering albedo
   real(r8) ga(pcols,0:pver) ! aerosol assymetry parameter
   real(r8) fa(pcols,0:pver) ! aerosol forward scattered fraction

! 
! Various arrays and other constants:
! 
   real(r8) pflx(pcols,0:pverp) ! Interface press, including extra layer
   real(r8) zenfac(pcols)    ! Square root of cos solar zenith angle
   real(r8) sqrco2           ! Square root of the co2 mass mixg ratio
   real(r8) tmp1             ! Temporary constant array
   real(r8) tmp2             ! Temporary constant array
   real(r8) pdel             ! Pressure difference across layer
   real(r8) path             ! Mass path of layer
   real(r8) ptop             ! Lower interface pressure of extra layer
   real(r8) ptho2            ! Used to compute mass path of o2
   real(r8) ptho3            ! Used to compute mass path of o3
   real(r8) pthco2           ! Used to compute mass path of co2
   real(r8) pthh2o           ! Used to compute mass path of h2o
   real(r8) h2ostr           ! Inverse sq. root h2o mass mixing ratio
   real(r8) wavmid(nspint)   ! Spectral interval middle wavelength
   real(r8) trayoslp         ! Rayleigh optical depth/standard pressure
   real(r8) tmp1l            ! Temporary constant array
   real(r8) tmp2l            ! Temporary constant array
   real(r8) tmp3l            ! Temporary constant array
   real(r8) tmp1i            ! Temporary constant array
   real(r8) tmp2i            ! Temporary constant array
   real(r8) tmp3i            ! Temporary constant array
   real(r8) rdenom           ! Multiple scattering term
   real(r8) rdirexp          ! layer direct ref times exp transmission
   real(r8) tdnmexp          ! total transmission - exp transmission
   real(r8) psf(nspint)      ! Frac of solar flux in spect interval
! 
! Layer absorber amounts; note that 0 refers to the extra layer added
! above the top model layer
! 
   real(r8) uh2o(pcols,0:pver) ! Layer absorber amount of h2o
   real(r8) uo3(pcols,0:pver) ! Layer absorber amount of  o3
   real(r8) uco2(pcols,0:pver) ! Layer absorber amount of co2
   real(r8) uo2(pcols,0:pver) ! Layer absorber amount of  o2
   real(r8) uaer(pcols,0:pver) ! Layer aerosol amount 
! 
! Total column absorber amounts:
! 
   real(r8) uth2o(pcols)     ! Total column  absorber amount of  h2o
   real(r8) uto3(pcols)      ! Total column  absorber amount of  o3
   real(r8) utco2(pcols)     ! Total column  absorber amount of  co2
   real(r8) uto2(pcols)      ! Total column  absorber amount of  o2
! 
! These arrays are defined for pver model layers; 0 refers to the extra
! layer on top:
! 
   real(r8) rdir(nspint,pcols,0:pver) ! Layer reflectivity to direct rad
   real(r8) rdif(nspint,pcols,0:pver) ! Layer reflectivity to diffuse rad
   real(r8) tdir(nspint,pcols,0:pver) ! Layer transmission to direct rad
   real(r8) tdif(nspint,pcols,0:pver) ! Layer transmission to diffuse rad
   real(r8) explay(nspint,pcols,0:pver) ! Solar beam exp trans. for layer

   real(r8) rdirc(nspint,pcols,0:pver) ! Clear Layer reflec. to direct rad
   real(r8) rdifc(nspint,pcols,0:pver) ! Clear Layer reflec. to diffuse rad
   real(r8) tdirc(nspint,pcols,0:pver) ! Clear Layer trans. to direct rad
   real(r8) tdifc(nspint,pcols,0:pver) ! Clear Layer trans. to diffuse rad
   real(r8) explayc(nspint,pcols,0:pver) ! Solar beam exp trans. clear layer

   real(r8) flxdiv           ! Flux divergence for layer
! 
! 
! Radiative Properties:
! 
! There are 1 classes of properties:
! (1. All-sky bulk properties
! (2. Clear-sky properties
! 
! The first set of properties are generated during step 2 of the solution.
! 
! These arrays are defined at model interfaces; in 1st index (for level #),
! 0 is the top of the extra layer above the model top, and
! pverp is the earth surface.  2nd index is for cloud configuration
! defined over a whole column.
! 
   real(r8) exptdn(0:pverp,nconfgmax) ! Sol. beam trans from layers above
   real(r8) rdndif(0:pverp,nconfgmax) ! Ref to dif rad for layers above
   real(r8) rupdif(0:pverp,nconfgmax) ! Ref to dif rad for layers below
   real(r8) rupdir(0:pverp,nconfgmax) ! Ref to dir rad for layers below
   real(r8) tdntot(0:pverp,nconfgmax) ! Total trans for layers above
! 
! Bulk properties used during the clear-sky calculation.
! 
   real(r8) exptdnc(0:pverp) ! clr: Sol. beam trans from layers above
   real(r8) rdndifc(0:pverp) ! clr: Ref to dif rad for layers above
   real(r8) rupdifc(0:pverp) ! clr: Ref to dif rad for layers below
   real(r8) rupdirc(0:pverp) ! clr: Ref to dir rad for layers below
   real(r8) tdntotc(0:pverp) ! clr: Total trans for layers above

   real(r8) fluxup(0:pverp)  ! Up   flux at model interface
   real(r8) fluxdn(0:pverp)  ! Down flux at model interface
   real(r8) fluxdndir(0:pverp)  ! Direct Down flux at model interface (amontornes-bcodina 2014-04-20)
   real(r8) wexptdn          ! Direct solar beam trans. to surface

! moved to here from the module storage above, because these have to be thread-private.  JM 20100217
   real(r8) abarli           ! A coefficient for current spectral band
   real(r8) bbarli           ! B coefficient for current spectral band
   real(r8) cbarli           ! C coefficient for current spectral band
   real(r8) dbarli           ! D coefficient for current spectral band
   real(r8) ebarli           ! E coefficient for current spectral band
   real(r8) fbarli           ! F coefficient for current spectral band

   real(r8) abarii           ! A coefficient for current spectral band
   real(r8) bbarii           ! B coefficient for current spectral band
   real(r8) cbarii           ! C coefficient for current spectral band
   real(r8) dbarii           ! D coefficient for current spectral band
   real(r8) ebarii           ! E coefficient for current spectral band
   real(r8) fbarii           ! F coefficient for current spectral band
! JM 20100217

! 
!-----------------------------------------------------------------------
! START OF CALCULATION
!-----------------------------------------------------------------------
! 
!  write (6, '(a, x, i3)') 'radcswmx : chunk identifier', lchnk

   do i=1, ncol
! 
! Initialize output fields:
! 
      fsds(i)     = 0.0_r8

      fsnirtoa(i) = 0.0_r8
      fsnrtoac(i) = 0.0_r8
      fsnrtoaq(i) = 0.0_r8

      fsns(i)     = 0.0_r8
      fsnsc(i)    = 0.0_r8
      fsdsc(i)    = 0.0_r8

      fsdscdir(i) = 0.0_r8 ! amontornes-bcodina (2014-04-20)
      fsdscdif(i) = 0.0_r8 ! amontornes-bcodina (2014-04-20)
      fsdsdir(i)  = 0.0_r8 ! amontornes-bcodina (2014-04-20)
      fsdsdif(i)  = 0.0_r8 ! amontornes-bcodina (2014-04-20)

      fsnt(i)     = 0.0_r8
      fsntc(i)    = 0.0_r8
      fsntoa(i)   = 0.0_r8
      fsntoac(i)  = 0.0_r8

      solin(i)    = 0.0_r8

      sols(i)     = 0.0_r8
      soll(i)     = 0.0_r8
      solsd(i)    = 0.0_r8
      solld(i)    = 0.0_r8

! initialize added downward/upward total and clear sky fluxes

         do k=1,pverp
            fsup(i,k)  = 0.0_r8
            fsupc(i,k) = 0.0_r8
            fsdn(i,k)  = 0.0_r8
            fsdnc(i,k) = 0.0_r8
            fsdndir(i,k)  = 0.0_r8 ! amontornes-bcodina (2014-04-20)
            fsdncdir(i,k) = 0.0_r8 ! amontornes-bcodina (2014-04-20)
            fsdndif(i,k)  = 0.0_r8 ! amontornes-bcodina (2014-04-20)
            fsdncdif(i,k) = 0.0_r8 ! amontornes-bcodina (2014-04-20)
            tauxcl(i,k-1) = 0.0_r8
            tauxci(i,k-1) = 0.0_r8
         end do

      do k=1, pver
         qrs(i,k) = 0.0_r8
      end do

      ! initialize aerosol diagnostic fields to 0.0 
      ! Average can be obtained by dividing <aerod>/<frc_day>
      do kaer = 1, naer_groups
         do ns = 1, nspint
            frc_day(i) = 0.0_r8
            aertau(i,ns,kaer) = 0.0_r8
            aerssa(i,ns,kaer) = 0.0_r8
            aerasm(i,ns,kaer) = 0.0_r8
            aerfwd(i,ns,kaer) = 0.0_r8
         end do
      end do

   end do
! 
! Compute starting, ending daytime loop indices:
!  *** Note this logic assumes day and night points are contiguous so
!  *** will not work in general with chunked data structure.
! 
   ndayc = 0
   do i=1,ncol
      if (coszrs(i) > 0.0_r8) then
         ndayc = ndayc + 1
         idayc(ndayc) = i
      end if
   end do
! 
! If night everywhere, return:
! 
   if (ndayc == 0) return
! 
! Perform other initializations
! 
   tmp1   = 0.5_r8/(gravit*sslp)
   tmp2   = delta/gravit
   sqrco2 = sqrt(co2mmr)

   do n=1,ndayc
      i=idayc(n)
! 
! Define solar incident radiation and interface pressures:
! 
!        solin(i)  = scon*eccf*coszrs(i)
!WRF use SOLCON (MKS) calculated outside
         solin(i)  = solcon*coszrs(i)*1000.
         pflx(i,0) = 0._r8
         do k=1,pverp
            pflx(i,k) = pint(i,k)
         end do
! 
! Compute optical paths:
! 
         ptop      = pflx(i,1)
         ptho2     = o2mmr * ptop / gravit
         ptho3     = o3mmr(i,1) * ptop / gravit
         pthco2    = sqrco2 * (ptop / gravit)
         h2ostr    = sqrt( 1._r8 / h2ommr(i,1) )
         zenfac(i) = sqrt(coszrs(i))
         pthh2o    = ptop**2*tmp1 + (ptop*rga)* &
                    (h2ostr*zenfac(i)*delta)
         uh2o(i,0) = h2ommr(i,1)*pthh2o
         uco2(i,0) = zenfac(i)*pthco2
         uo2 (i,0) = zenfac(i)*ptho2
         uo3 (i,0) = ptho3
         uaer(i,0) = 0.0_r8
         do k=1,pver
            pdel      = pflx(i,k+1) - pflx(i,k)
            path      = pdel / gravit
            ptho2     = o2mmr * path
            ptho3     = o3mmr(i,k) * path
            pthco2    = sqrco2 * path
            h2ostr    = sqrt(1.0_r8/h2ommr(i,k))
            pthh2o    = (pflx(i,k+1)**2 - pflx(i,k)**2)*tmp1 + pdel*h2ostr*zenfac(i)*tmp2
            uh2o(i,k) = h2ommr(i,k)*pthh2o
            uco2(i,k) = zenfac(i)*pthco2
            uo2 (i,k) = zenfac(i)*ptho2
            uo3 (i,k) = ptho3
            usul(i,k) = aermmr(i,k,idxSUL) * path 
            ubg(i,k) = aermmr(i,k,idxBG) * path 
            usslt(i,k) = aermmr(i,k,idxSSLT) * path
            if (usslt(i,k) .lt. 0.0) then  ! usslt is sometimes small and negative, will be fixed
              usslt(i,k) = 0.0
            end if
            ucphil(i,k) = aermmr(i,k,idxOCPHI) * path
            ucphob(i,k) = aermmr(i,k,idxOCPHO) * path
            ucb(i,k) = ( aermmr(i,k,idxBCPHO) + aermmr(i,k,idxBCPHI) ) * path
            uvolc(i,k) =  aermmr(i,k,idxVOLC)
            do ksz = 1, ndstsz
              udst(ksz,i,k) = aermmr(i,k,idxDUSTfirst-1+ksz) * path
            end do
         end do
! 
! Compute column absorber amounts for the clear sky computation:
! 
         uth2o(i) = 0.0_r8
         uto3(i)  = 0.0_r8
         utco2(i) = 0.0_r8
         uto2(i)  = 0.0_r8

         do k=1,pver
            uth2o(i) = uth2o(i) + uh2o(i,k)
            uto3(i)  = uto3(i)  + uo3(i,k)
            utco2(i) = utco2(i) + uco2(i,k)
            uto2(i)  = uto2(i)  + uo2(i,k)
         end do
! 
! Set cloud properties for top (0) layer; so long as tauxcl is zero,
! there is no cloud above top of model; the other cloud properties
! are arbitrary:
! 
         tauxcl(i,0)  = 0._r8
         wcl(i,0)     = 0.999999_r8
         gcl(i,0)     = 0.85_r8
         fcl(i,0)     = 0.725_r8
         tauxci(i,0)  = 0._r8
         wci(i,0)     = 0.999999_r8
         gci(i,0)     = 0.85_r8
         fci(i,0)     = 0.725_r8
! 
! Aerosol 
! 
         tauxar(i,0)  = 0._r8
         wa(i,0)      = 0.925_r8
         ga(i,0)      = 0.850_r8
         fa(i,0)      = 0.7225_r8
! 
! End  do n=1,ndayc
! 
   end do
! 
! Begin spectral loop
! 
   do ns=1,nspint
! 
! Set index for cloud particle properties based on the wavelength,
! according to A. Slingo (1989) equations 1-3:
! Use index 1 (0.25 to 0.69 micrometers) for visible
! Use index 2 (0.69 - 1.19 micrometers) for near-infrared
! Use index 3 (1.19 to 2.38 micrometers) for near-infrared
! Use index 4 (2.38 to 4.00 micrometers) for near-infrared
! 
! Note that the minimum wavelength is encoded (with .001, .002, .003)
! in order to specify the index appropriate for the near-infrared
! cloud absorption properties
! 
      if(wavmax(ns) <= 0.7_r8) then
         indxsl = 1
      else if(wavmin(ns) == 0.700_r8) then
         indxsl = 2
      else if(wavmin(ns) == 0.701_r8) then
         indxsl = 3
      else if(wavmin(ns) == 0.702_r8 .or. wavmin(ns) > 2.38_r8) then
         indxsl = 4
      end if
! 
! Set cloud extinction optical depth, single scatter albedo,
! asymmetry parameter, and forward scattered fraction:
! 
      abarli = abarl(indxsl)
      bbarli = bbarl(indxsl)
      cbarli = cbarl(indxsl)
      dbarli = dbarl(indxsl)
      ebarli = ebarl(indxsl)
      fbarli = fbarl(indxsl)
! 
      abarii = abari(indxsl)
      bbarii = bbari(indxsl)
      cbarii = cbari(indxsl)
      dbarii = dbari(indxsl)
      ebarii = ebari(indxsl)
      fbarii = fbari(indxsl)
! 
! adjustfraction within spectral interval to allow for the possibility of
! sub-divisions within a particular interval:
! 
      psf(ns) = 1.0_r8
      if(ph2o(ns)/=0._r8) psf(ns) = psf(ns)*ph2o(ns)
      if(pco2(ns)/=0._r8) psf(ns) = psf(ns)*pco2(ns)
      if(po2 (ns)/=0._r8) psf(ns) = psf(ns)*po2 (ns)

      do n=1,ndayc
         i=idayc(n)

         frc_day(i) = 1.0_r8
         do kaer = 1, naer_groups
            aertau(i,ns,kaer) = 0.0
            aerssa(i,ns,kaer) = 0.0
            aerasm(i,ns,kaer) = 0.0
            aerfwd(i,ns,kaer) = 0.0
         end do

            do k=1,pver
! 
! liquid
! 
               tmp1l = abarli + bbarli/rel(i,k)
               tmp2l = 1._r8 - cbarli - dbarli*rel(i,k)
               tmp3l = fbarli*rel(i,k)
! 
! ice
! 
               tmp1i = abarii + bbarii/rei(i,k)
               tmp2i = 1._r8 - cbarii - dbarii*rei(i,k)
               tmp3i = fbarii*rei(i,k)

               if (cld(i,k) >= cldmin .and. cld(i,k) >= cldeps) then
                  tauxcl(i,k) = cliqwp(i,k)*tmp1l
                  tauxci(i,k) = cicewp(i,k)*tmp1i
               else
                  tauxcl(i,k) = 0.0
                  tauxci(i,k) = 0.0
               endif
! 
! Do not let single scatter albedo be 1.  Delta-eddington solution
! for non-conservative case has different analytic form from solution
! for conservative case, and raddedmx is written for non-conservative case.
! 
               wcl(i,k) = min(tmp2l,.999999_r8)
               gcl(i,k) = ebarli + tmp3l
               fcl(i,k) = gcl(i,k)*gcl(i,k)
! 
               wci(i,k) = min(tmp2i,.999999_r8)
               gci(i,k) = ebarii + tmp3i
               fci(i,k) = gci(i,k)*gci(i,k)
! 
! Set aerosol properties
! Conversion factor to adjust aerosol extinction (m2/g)
! 
               rhtrunc = rh(i,k)
               rhtrunc = min(rh(i,k),1._r8)
!              if(rhtrunc.lt.0._r8) call endrun ('RADCSWMX')
               krh = min(floor( rhtrunc * nrh ) + 1, nrh - 1)
               wrh = rhtrunc * nrh - krh

               ! linear interpolation of optical properties between rh table points
               ksuli = ksul(krh + 1, ns) * (wrh + 1) - ksul(krh, ns) * wrh
               ksslti = ksslt(krh + 1, ns) * (wrh + 1) - ksslt(krh, ns) * wrh
               kcphili = kcphil(krh + 1, ns) * (wrh + 1) - kcphil(krh, ns) * wrh
               wsuli = wsul(krh + 1, ns) * (wrh + 1) - wsul(krh, ns) * wrh
               wsslti = wsslt(krh + 1, ns) * (wrh + 1) - wsslt(krh, ns) * wrh
               wcphili = wcphil(krh + 1, ns) * (wrh + 1) - wcphil(krh, ns) * wrh
               gsuli = gsul(krh + 1, ns) * (wrh + 1) - gsul(krh, ns) * wrh
               gsslti = gsslt(krh + 1, ns) * (wrh + 1) - gsslt(krh, ns) * wrh
               gcphili = gcphil(krh + 1, ns) * (wrh + 1) - gcphil(krh, ns) * wrh

               tau_sul = 1.e4 * ksuli * usul(i,k)
               tau_sslt = 1.e4 * ksslti * usslt(i,k)
               tau_cphil = 1.e4 * kcphili * ucphil(i,k)
               tau_cphob = 1.e4 * kcphob(ns) * ucphob(i,k)
               tau_cb = 1.e4 * kcb(ns) * ucb(i,k)
               tau_volc = 1.e3 * kvolc(ns) * uvolc(i,k)
               tau_dst(:) = 1.e4 * kdst(:,ns) * udst(:,i,k)
               tau_bg = 1.e4 * kbg(ns) * ubg(i,k)

               tau_w_sul = tau_sul * wsuli
               tau_w_sslt = tau_sslt * wsslti
               tau_w_cphil = tau_cphil * wcphili
               tau_w_cphob = tau_cphob * wcphob(ns)
               tau_w_cb = tau_cb * wcb(ns)
               tau_w_volc = tau_volc * wvolc(ns)
               tau_w_dst(:) = tau_dst(:) * wdst(:,ns)
               tau_w_bg = tau_bg * wbg(ns)

               tau_w_g_sul = tau_w_sul * gsuli
               tau_w_g_sslt = tau_w_sslt * gsslti
               tau_w_g_cphil = tau_w_cphil * gcphili
               tau_w_g_cphob = tau_w_cphob * gcphob(ns)
               tau_w_g_cb = tau_w_cb * gcb(ns)
               tau_w_g_volc = tau_w_volc * gvolc(ns)
               tau_w_g_dst(:) = tau_w_dst(:) * gdst(:,ns)
               tau_w_g_bg = tau_w_bg * gbg(ns)

               f_sul = gsuli * gsuli
               f_sslt = gsslti * gsslti
               f_cphil = gcphili * gcphili
               f_cphob = gcphob(ns) * gcphob(ns)
               f_cb = gcb(ns) * gcb(ns)
               f_volc = gvolc(ns) * gvolc(ns)
               f_dst(:) = gdst(:,ns) * gdst(:,ns)
               f_bg = gbg(ns) * gbg(ns)

               tau_w_f_sul = tau_w_sul * f_sul
               tau_w_f_bg = tau_w_bg * f_bg
               tau_w_f_sslt = tau_w_sslt * f_sslt
               tau_w_f_cphil = tau_w_cphil * f_cphil
               tau_w_f_cphob = tau_w_cphob * f_cphob
               tau_w_f_cb = tau_w_cb * f_cb
               tau_w_f_volc = tau_w_volc * f_volc
               tau_w_f_dst(:) = tau_w_dst(:) * f_dst(:)
!
! mix dust aerosol size bins
!   w_dst_tot, g_dst_tot, w_dst_tot are currently not used anywhere
!   but calculate them anyway for future use
!
               tau_dst_tot = sum(tau_dst)
               tau_w_dst_tot = sum(tau_w_dst)
               tau_w_g_dst_tot = sum(tau_w_g_dst)
               tau_w_f_dst_tot = sum(tau_w_f_dst)

               if (tau_dst_tot .gt. 0.0) then
                 w_dst_tot = tau_w_dst_tot / tau_dst_tot
               else
                 w_dst_tot = 0.0
               endif

               if (tau_w_dst_tot .gt. 0.0) then
                 g_dst_tot = tau_w_g_dst_tot / tau_w_dst_tot
                 f_dst_tot = tau_w_f_dst_tot / tau_w_dst_tot
               else
                 g_dst_tot = 0.0
                 f_dst_tot = 0.0
               endif
!
! mix aerosols
!
               tau_tot     = tau_sul + tau_sslt &
                           + tau_cphil + tau_cphob + tau_cb + tau_dst_tot
               tau_tot     = tau_tot + tau_bg + tau_volc

               tau_w_tot   = tau_w_sul + tau_w_sslt &
                           + tau_w_cphil + tau_w_cphob + tau_w_cb + tau_w_dst_tot
               tau_w_tot   = tau_w_tot + tau_w_bg + tau_w_volc

               tau_w_g_tot = tau_w_g_sul + tau_w_g_sslt &
                           + tau_w_g_cphil + tau_w_g_cphob + tau_w_g_cb + tau_w_g_dst_tot
               tau_w_g_tot = tau_w_g_tot + tau_w_g_bg + tau_w_g_volc

               tau_w_f_tot = tau_w_f_sul + tau_w_f_sslt &
                           + tau_w_f_cphil + tau_w_f_cphob + tau_w_f_cb + tau_w_f_dst_tot
               tau_w_f_tot = tau_w_f_tot + tau_w_f_bg  + tau_w_f_volc

               if (tau_tot .gt. 0.0) then
                 w_tot = tau_w_tot / tau_tot
               else
                 w_tot = 0.0
               endif

               if (tau_w_tot .gt. 0.0) then
                 g_tot = tau_w_g_tot / tau_w_tot
                 f_tot = tau_w_f_tot / tau_w_tot
               else
                 g_tot = 0.0
                 f_tot = 0.0
               endif

               tauxar(i,k) = tau_tot
               wa(i,k)     = min(w_tot, 0.999999_r8)
               if (g_tot.gt.1._r8) write(6,*) "g_tot > 1"
               if (g_tot.lt.-1._r8) write(6,*) "g_tot < -1"
!              if (g_tot.gt.1._r8) call endrun ('RADCSWMX')
!              if (g_tot.lt.-1._r8) call endrun ('RADCSWMX')
               ga(i,k)     = g_tot
               if (f_tot.gt.1._r8) write(6,*)"f_tot > 1"
               if (f_tot.lt.0._r8) write(6,*)"f_tot < 0"
!              if (f_tot.gt.1._r8) call endrun ('RADCSWMX')
!              if (f_tot.lt.0._r8) call endrun ('RADCSWMX')
               fa(i,k)     = f_tot

               aertau(i,ns,1) = aertau(i,ns,1) + tau_sul
               aertau(i,ns,2) = aertau(i,ns,2) + tau_sslt
               aertau(i,ns,3) = aertau(i,ns,3) + tau_cphil + tau_cphob + tau_cb
               aertau(i,ns,4) = aertau(i,ns,4) + tau_dst_tot
               aertau(i,ns,5) = aertau(i,ns,5) + tau_bg
               aertau(i,ns,6) = aertau(i,ns,6) + tau_volc
               aertau(i,ns,7) = aertau(i,ns,7) + tau_tot

               aerssa(i,ns,1) = aerssa(i,ns,1) + tau_w_sul
               aerssa(i,ns,2) = aerssa(i,ns,2) + tau_w_sslt
               aerssa(i,ns,3) = aerssa(i,ns,3) + tau_w_cphil + tau_w_cphob + tau_w_cb
               aerssa(i,ns,4) = aerssa(i,ns,4) + tau_w_dst_tot
               aerssa(i,ns,5) = aerssa(i,ns,5) + tau_w_bg
               aerssa(i,ns,6) = aerssa(i,ns,6) + tau_w_volc
               aerssa(i,ns,7) = aerssa(i,ns,7) + tau_w_tot

               aerasm(i,ns,1) = aerasm(i,ns,1) + tau_w_g_sul
               aerasm(i,ns,2) = aerasm(i,ns,2) + tau_w_g_sslt
               aerasm(i,ns,3) = aerasm(i,ns,3) + tau_w_g_cphil + tau_w_g_cphob + tau_w_g_cb
               aerasm(i,ns,4) = aerasm(i,ns,4) + tau_w_g_dst_tot
               aerasm(i,ns,5) = aerasm(i,ns,5) + tau_w_g_bg
               aerasm(i,ns,6) = aerasm(i,ns,6) + tau_w_g_volc
               aerasm(i,ns,7) = aerasm(i,ns,7) + tau_w_g_tot

               aerfwd(i,ns,1) = aerfwd(i,ns,1) + tau_w_f_sul
               aerfwd(i,ns,2) = aerfwd(i,ns,2) + tau_w_f_sslt
               aerfwd(i,ns,3) = aerfwd(i,ns,3) + tau_w_f_cphil + tau_w_f_cphob + tau_w_f_cb
               aerfwd(i,ns,4) = aerfwd(i,ns,4) + tau_w_f_dst_tot
               aerfwd(i,ns,5) = aerfwd(i,ns,5) + tau_w_f_bg
               aerfwd(i,ns,6) = aerfwd(i,ns,6) + tau_w_f_volc
               aerfwd(i,ns,7) = aerfwd(i,ns,7) + tau_w_f_tot

! 
! End do k=1,pver
! 
            end do

            ! normalize aerosol optical diagnostic fields
            do kaer = 1, naer_groups

               if (aerssa(i,ns,kaer) .gt. 0.0) then   ! aerssa currently holds product of tau and ssa
                  aerasm(i,ns,kaer) = aerasm(i,ns,kaer) / aerssa(i,ns,kaer)
                  aerfwd(i,ns,kaer) = aerfwd(i,ns,kaer) / aerssa(i,ns,kaer)
               else
                  aerasm(i,ns,kaer) = 0.0_r8
                  aerfwd(i,ns,kaer) = 0.0_r8
               end if

               if (aertau(i,ns,kaer) .gt. 0.0) then
                  aerssa(i,ns,kaer) = aerssa(i,ns,kaer) / aertau(i,ns,kaer)
               else
                  aerssa(i,ns,kaer) = 0.0_r8
               end if

            end do


! 
! End do n=1,ndayc
! 
      end do

! 
! Set reflectivities for surface based on mid-point wavelength
! 
      wavmid(ns) = 0.5_r8*(wavmin(ns) + wavmax(ns))
! 
! Wavelength less  than 0.7 micro-meter
! 
      if (wavmid(ns) < 0.7_r8 ) then
         do n=1,ndayc
            i=idayc(n)
               albdir(i,ns) = asdir(i)
               albdif(i,ns) = asdif(i)
         end do
! 
! Wavelength greater than 0.7 micro-meter
! 
      else
         do n=1,ndayc
            i=idayc(n)
               albdir(i,ns) = aldir(i)
               albdif(i,ns) = aldif(i)
         end do
      end if
      trayoslp = raytau(ns)/sslp
! 
! Layer input properties now completely specified; compute the
! delta-Eddington solution reflectivities and transmissivities
! for each layer
! 
      call raddedmx(pver, pverp, pcols, coszrs   ,ndayc    ,idayc   , &
              abh2o(ns),abo3(ns) ,abco2(ns),abo2(ns) , &
              uh2o     ,uo3      ,uco2     ,uo2      , &
              trayoslp ,pflx     ,ns       , &
              tauxcl   ,wcl      ,gcl      ,fcl      , &
              tauxci   ,wci      ,gci      ,fci      , &
              tauxar   ,wa       ,ga       ,fa       , &
              rdir     ,rdif     ,tdir     ,tdif     ,explay  , &
              rdirc    ,rdifc    ,tdirc    ,tdifc    ,explayc )
! 
! End spectral loop
! 
   end do
! 
!----------------------------------------------------------------------
! 
! Solution for max/random cloud overlap.  
! 
! Steps:
! (1. delta-Eddington solution for each layer (called above)
! 
! (2. The adding method is used to
! compute the reflectivity and transmissivity to direct and diffuse
! radiation from the top and bottom of the atmosphere for each
! cloud configuration.  This calculation is based upon the
! max-random overlap assumption.
! 
! (3. to solve for the fluxes, combine the
! bulk properties of the atmosphere above/below the region.
! 
! Index calculations for steps 2-3 are performed outside spectral
! loop to avoid redundant calculations.  Index calculations (with
! application of areamin & nconfgmax conditions) are performed 
! first to identify the minimum subset of terms for the configurations 
! satisfying the areamin & nconfgmax conditions. This minimum set is 
! used to identify the corresponding minimum subset of terms in 
! steps 2 and 3.
! 

   do n=1,ndayc
      i=idayc(n)

!----------------------------------------------------------------------
! INDEX CALCULATIONS FOR MAX OVERLAP
! 
! The column is divided into sets of adjacent layers, called regions, 
! in which the clouds are maximally overlapped.  The clouds are
! randomly overlapped between different regions.  The number of
! regions in a column is set by nmxrgn, and the range of pressures
! included in each region is set by pmxrgn.  
! 
! The following calculations determine the number of unique cloud 
! configurations (assuming maximum overlap), called "streams",
! within each region. Each stream consists of a vector of binary
! clouds (either 0 or 100% cloud cover).  Over the depth of the region, 
! each stream requires a separate calculation of radiative properties. These
! properties are generated using the adding method from
! the radiative properties for each layer calculated by raddedmx.
! 
! The upward and downward-propagating streams are treated
! separately.
! 
! We will refer to a particular configuration of binary clouds
! within a single max-overlapped region as a "stream".  We will 
! refer to a particular arrangement of binary clouds over the entire column
! as a "configuration".
! 
! This section of the code generates the following information:
! (1. nrgn    : the true number of max-overlap regions (need not = nmxrgn)
! (2. nstr    : the number of streams in a region (>=1)
! (3. cstr    : flags for presence of clouds at each layer in each stream
! (4. wstr    : the fractional horizontal area of a grid box covered
! by each stream
! (5. kx1,2   : level indices for top/bottom of each region
! 
! The max-overlap calculation proceeds in 3 stages:
! (1. compute layer radiative properties in raddedmx.
! (2. combine these properties between layers 
! (3. combine properties to compute fluxes at each interface.  
! 
! Most of the indexing information calculated here is used in steps 2-3
! after the call to raddedmx.
! 
! Initialize indices for layers to be max-overlapped
! 
! Loop to handle fix in totwgt=0. For original overlap config 
! from npasses = 0.
! 
         npasses = 0
         do
            do irgn = 0, nmxrgn(i)
               kx2(irgn) = 0
            end do
            mrgn = 0
! 
! Outermost loop over regions (sets of adjacent layers) to be max overlapped
! 
            do irgn = 1, nmxrgn(i)
! 
! Calculate min/max layer indices inside region.  
! 
               region_found = .false.
               if (kx2(irgn-1) < pver) then
                  k1 = kx2(irgn-1)+1
                  kx1(irgn) = k1
                  kx2(irgn) = k1-1
                  do k2 = pver, k1, -1
                     if (pmid(i,k2) <= pmxrgn(i,irgn)) then
                        kx2(irgn) = k2
                        mrgn = mrgn+1
                        region_found = .true.
                        exit
                     end if
                  end do
               else
                  exit
               endif

               if (region_found) then
! 
! Sort cloud areas and corresponding level indices.  
! 
                  nxs = 0
                  if (cldeps > 0) then 
                     do k = k1,k2
                        if (cld(i,k) >= cldmin .and. cld(i,k) >= cldeps) then
                           nxs = nxs+1
                           ksort(nxs) = k
! 
! We need indices for clouds in order of largest to smallest, so
! sort 1-cld in ascending order
! 
                           asort(nxs) = 1.0_r8-(floor(cld(i,k)/cldeps)*cldeps)
                        end if
                     end do
                  else
                     do k = k1,k2
                        if (cld(i,k) >= cldmin) then
                           nxs = nxs+1
                           ksort(nxs) = k
! 
! We need indices for clouds in order of largest to smallest, so
! sort 1-cld in ascending order
! 
                           asort(nxs) = 1.0_r8-cld(i,k)
                        end if
                     end do
                  endif
! 
! If nxs eq 1, no need to sort. 
! If nxs eq 2, sort by swapping if necessary
! If nxs ge 3, sort using local sort routine
! 
                  if (nxs == 2) then
                     if (asort(2) < asort(1)) then
                        ktmp = ksort(1)
                        ksort(1) = ksort(2)
                        ksort(2) = ktmp

                        atmp = asort(1)
                        asort(1) = asort(2)
                        asort(2) = atmp
                     endif
                  else if (nxs >= 3) then
                     call sortarray(nxs,asort,ksort)
                  endif
! 
! Construct wstr, cstr, nstr for this region
! 
                  cstr(k1:k2,1:nxs+1) = 0
                  mstr = 1
                  cld0 = 0.0_r8
                  do l = 1, nxs
                     if (asort(l) /= cld0) then
                        wstr(mstr,mrgn) = asort(l) - cld0
                        cld0 = asort(l)
                        mstr = mstr + 1
                     endif
                     cstr(ksort(l),mstr:nxs+1) = 1
                  end do
                  nstr(mrgn) = mstr
                  wstr(mstr,mrgn) = 1.0_r8 - cld0
! 
! End test of region_found = true
! 
               endif
! 
! End loop over regions irgn for max-overlap
! 
            end do
            nrgn = mrgn
! 
! Finish construction of cstr for additional top layer
! 
            cstr(0,1:nstr(1)) = 0
! 
! INDEX COMPUTATIONS FOR STEP 2-3
! This section of the code generates the following information:
! (1. totwgt     step 3     total frac. area of configurations satisfying
! areamin & nconfgmax criteria
! (2. wgtv       step 3     frac. area of configurations 
! (3. ccon       step 2     binary flag for clouds in each configuration
! (4. nconfig    steps 2-3  number of configurations
! (5. nuniqu/d   step 2     Number of unique cloud configurations for
! up/downwelling rad. between surface/TOA
! and level k
! (6. istrtu/d   step 2     Indices into iconu/d
! (7. iconu/d    step 2     Cloud configurations which are identical
! for up/downwelling rad. between surface/TOA
! and level k
! 
! Number of configurations (all permutations of streams in each region)
! 
            nconfigm = product(nstr(1: nrgn))
! 
! Construction of totwgt, wgtv, ccon, nconfig
! 
            istr(1: nrgn) = 1
            nconfig = 0
            totwgt = 0.0_r8
            new_term = .true.
            do iconfig = 1, nconfigm
               xwgt = 1.0_r8
               do mrgn = 1,  nrgn
                  xwgt = xwgt * wstr(istr(mrgn),mrgn)
               end do
               if (xwgt >= areamin) then
                  nconfig = nconfig + 1
                  if (nconfig <= nconfgmax) then
                     j = nconfig
                     ptrc(nconfig) = nconfig
                  else
                     nconfig = nconfgmax
                     if (new_term) then
                        j = findvalue(1,nconfig,wgtv,ptrc)
                     endif
                     if (wgtv(j) < xwgt) then
                        totwgt = totwgt - wgtv(j)
                        new_term = .true.
                     else
                        new_term = .false.
                     endif
                  endif
                  if (new_term) then
                     wgtv(j) = xwgt
                     totwgt = totwgt + xwgt
                     do mrgn = 1, nrgn
                        ccon(kx1(mrgn):kx2(mrgn),j) = cstr(kx1(mrgn):kx2(mrgn),istr(mrgn))
                     end do
                  endif
               endif

               mrgn =  nrgn
               istr(mrgn) = istr(mrgn) + 1
               do while (istr(mrgn) > nstr(mrgn) .and. mrgn > 1)
                  istr(mrgn) = 1
                  mrgn = mrgn - 1
                  istr(mrgn) = istr(mrgn) + 1
               end do
! 
! End do iconfig = 1, nconfigm
! 
            end do
! 
! If totwgt = 0 implement maximum overlap and make another pass
! if totwgt = 0 on this second pass then terminate.
! 
            if (totwgt > 0.) then
               exit
            else
               npasses = npasses + 1
               if (npasses >= 2 ) then
                  write(6,*)'RADCSWMX: Maximum overlap of column ','failed'
                  call endrun
               endif
               nmxrgn(i)=1
               pmxrgn(i,1)=1.0e30
            end if
!
! End npasses = 0, do
!
         end do
! 
! 
! Finish construction of ccon
! 
         ccon(0,:) = 0
         ccon(pverp,:) = 0
! 
! Construction of nuniqu/d, istrtu/d, iconu/d using binary tree 
! 
         nuniqd(0) = 1
         nuniqu(pverp) = 1

         istrtd(0,1) = 1
         istrtu(pverp,1) = 1

         do j = 1, nconfig
            icond(0,j)=j
            iconu(pverp,j)=j
         end do

         istrtd(0,2) = nconfig+1
         istrtu(pverp,2) = nconfig+1

         do k = 1, pverp
            km1 = k-1
            nuniq = 0
            istrtd(k,1) = 1
            do l0 = 1, nuniqd(km1)
               is0 = istrtd(km1,l0)
               is1 = istrtd(km1,l0+1)-1
               n0 = 0
               n1 = 0
               do isn = is0, is1
                  j = icond(km1,isn)
                  if (ccon(k,j) == 0) then
                     n0 = n0 + 1
                     ptr0(n0) = j
                  endif
                  if (ccon(k,j) == 1) then
                     n1 = n1 + 1
                     ptr1(n1) = j
                  endif
               end do
               if (n0 > 0) then
                  nuniq = nuniq + 1
                  istrtd(k,nuniq+1) = istrtd(k,nuniq)+n0
                  icond(k,istrtd(k,nuniq):istrtd(k,nuniq+1)-1) =  ptr0(1:n0)
               endif
               if (n1 > 0) then
                  nuniq = nuniq + 1
                  istrtd(k,nuniq+1) = istrtd(k,nuniq)+n1
                  icond(k,istrtd(k,nuniq):istrtd(k,nuniq+1)-1) =  ptr1(1:n1)
               endif
            end do
            nuniqd(k) = nuniq
         end do

         do k = pver, 0, -1
            kp1 = k+1
            nuniq = 0
            istrtu(k,1) = 1
            do l0 = 1, nuniqu(kp1)
               is0 = istrtu(kp1,l0)
               is1 = istrtu(kp1,l0+1)-1
               n0 = 0
               n1 = 0
               do isn = is0, is1
                  j = iconu(kp1,isn)
                  if (ccon(k,j) == 0) then
                     n0 = n0 + 1
                     ptr0(n0) = j
                  endif
                  if (ccon(k,j) == 1) then
                     n1 = n1 + 1
                     ptr1(n1) = j
                  endif
               end do
               if (n0 > 0) then
                  nuniq = nuniq + 1
                  istrtu(k,nuniq+1) = istrtu(k,nuniq)+n0
                  iconu(k,istrtu(k,nuniq):istrtu(k,nuniq+1)-1) =  ptr0(1:n0)
               endif
               if (n1 > 0) then
                  nuniq = nuniq + 1
                  istrtu(k,nuniq+1) = istrtu(k,nuniq)+n1
                  iconu(k,istrtu(k,nuniq):istrtu(k,nuniq+1)-1) = ptr1(1:n1)
               endif
            end do
            nuniqu(k) = nuniq
         end do
! 
!----------------------------------------------------------------------
! End of index calculations
!----------------------------------------------------------------------


!----------------------------------------------------------------------
! Start of flux calculations
!----------------------------------------------------------------------
! 
! Initialize spectrally integrated totals:
! 
         do k=0,pver
            totfld(k) = 0.0_r8
            fswup (k) = 0.0_r8
            fswdn (k) = 0.0_r8
            fswupc (k) = 0.0_r8
            fswdnc (k) = 0.0_r8
            fswdndir(k) = 0.0_r8 ! amontornes-bcodina (2014-04-20)
            fswdncdir(k)= 0.0_r8 ! amontornes-bcodina (2014-04-20)
         end do

         sfltot        = 0.0_r8
         fswup (pverp) = 0.0_r8
         fswdn (pverp) = 0.0_r8
         fswupc (pverp) = 0.0_r8
         fswdnc (pverp) = 0.0_r8
         fswdndir(pverp) = 0.0_r8 ! amontornes-bcodina (2014-04-20)
         fswdncdir(pverp)= 0.0_r8 ! amontornes-bcodina (2014-04-20)

! 
! Start spectral interval
! 
         do ns = 1,nspint
            wgtint = nirwgt(ns)
!----------------------------------------------------------------------
! STEP 2
! 
! 
! Apply adding method to solve for radiative properties
! 
! First initialize the bulk properties at TOA
! 
            rdndif(0,1:nconfig) = 0.0_r8
            exptdn(0,1:nconfig) = 1.0_r8
            tdntot(0,1:nconfig) = 1.0_r8
! 
! Solve for properties involving downward propagation of radiation.
! The bulk properties are:
! 
! (1. exptdn   Sol. beam dwn. trans from layers above
! (2. rdndif   Ref to dif rad for layers above
! (3. tdntot   Total trans for layers above
! 
            do k = 1, pverp
               km1 = k - 1
               do l0 = 1, nuniqd(km1)
                  is0 = istrtd(km1,l0)
                  is1 = istrtd(km1,l0+1)-1

                  j = icond(km1,is0)

                  xexpt   = exptdn(km1,j)
                  xrdnd   = rdndif(km1,j)
                  tdnmexp = tdntot(km1,j) - xexpt

                  if (ccon(km1,j) == 1) then
! 
! If cloud in layer, use cloudy layer radiative properties
! 
                     ytdnd = tdif(ns,i,km1)
                     yrdnd = rdif(ns,i,km1)

                     rdenom  = 1._r8/(1._r8-yrdnd*xrdnd)
                     rdirexp = rdir(ns,i,km1)*xexpt

                     zexpt = xexpt * explay(ns,i,km1)
                     zrdnd = yrdnd + xrdnd*(ytdnd**2)*rdenom
                     ztdnt = xexpt*tdir(ns,i,km1) + ytdnd*(tdnmexp + xrdnd*rdirexp)*rdenom
                  else
! 
! If clear layer, use clear-sky layer radiative properties
! 
                     ytdnd = tdifc(ns,i,km1)
                     yrdnd = rdifc(ns,i,km1)

                     rdenom  = 1._r8/(1._r8-yrdnd*xrdnd)
                     rdirexp = rdirc(ns,i,km1)*xexpt

                     zexpt = xexpt * explayc(ns,i,km1)
                     zrdnd = yrdnd + xrdnd*(ytdnd**2)*rdenom
                     ztdnt = xexpt*tdirc(ns,i,km1) + ytdnd* &
                                            (tdnmexp + xrdnd*rdirexp)*rdenom
                  endif

! 
! If 2 or more configurations share identical properties at a given level k,
! the properties (at level k) are computed once and copied to 
! all the configurations for efficiency.
! 
                  do isn = is0, is1
                     j = icond(km1,isn)
                     exptdn(k,j) = zexpt
                     rdndif(k,j) = zrdnd
                     tdntot(k,j) = ztdnt
                  end do
! 
! end do l0 = 1, nuniqd(k)
! 
               end do
! 
! end do k = 1, pverp
! 
            end do
! 
! Solve for properties involving upward propagation of radiation.
! The bulk properties are:
! 
! (1. rupdif   Ref to dif rad for layers below
! (2. rupdir   Ref to dir rad for layers below
! 
! Specify surface boundary conditions (surface albedos)
! 
            rupdir(pverp,1:nconfig) = albdir(i,ns)
            rupdif(pverp,1:nconfig) = albdif(i,ns)

            do k = pver, 0, -1
               do l0 = 1, nuniqu(k)
                  is0 = istrtu(k,l0)
                  is1 = istrtu(k,l0+1)-1

                  j = iconu(k,is0)

                  xrupd = rupdif(k+1,j)
                  xrups = rupdir(k+1,j)

                  if (ccon(k,j) == 1) then
! 
! If cloud in layer, use cloudy layer radiative properties
! 
                     yexpt = explay(ns,i,k)
                     yrupd = rdif(ns,i,k)
                     ytupd = tdif(ns,i,k)

                     rdenom  = 1._r8/( 1._r8 - yrupd*xrupd)
                     tdnmexp = (tdir(ns,i,k)-yexpt)
                     rdirexp = xrups*yexpt

                     zrupd = yrupd + xrupd*(ytupd**2)*rdenom
                     zrups = rdir(ns,i,k) + ytupd*(rdirexp + xrupd*tdnmexp)*rdenom
                  else
! 
! If clear layer, use clear-sky layer radiative properties
! 
                     yexpt = explayc(ns,i,k)
                     yrupd = rdifc(ns,i,k)
                     ytupd = tdifc(ns,i,k)

                     rdenom  = 1._r8/( 1._r8 - yrupd*xrupd)
                     tdnmexp = (tdirc(ns,i,k)-yexpt)
                     rdirexp = xrups*yexpt

                     zrupd = yrupd + xrupd*(ytupd**2)*rdenom
                     zrups = rdirc(ns,i,k) + ytupd*(rdirexp + xrupd*tdnmexp)*rdenom
                  endif

! 
! If 2 or more configurations share identical properties at a given level k,
! the properties (at level k) are computed once and copied to 
! all the configurations for efficiency.
! 
                  do isn = is0, is1
                     j = iconu(k,isn)
                     rupdif(k,j) = zrupd
                     rupdir(k,j) = zrups
                  end do
! 
! end do l0 = 1, nuniqu(k)
! 
               end do
! 
! end do k = pver,0,-1
! 
            end do
! 
!----------------------------------------------------------------------
! 
! STEP 3
! 
! Compute up and down fluxes for each interface k.  This requires
! adding up the contributions from all possible permutations
! of streams in all max-overlap regions, weighted by the
! product of the fractional areas of the streams in each region
! (the random overlap assumption).  The adding principle has been
! used in step 2 to combine the bulk radiative properties 
! above and below the interface.
! 
            do k = 0,pverp
! 
! Initialize the fluxes
! 
               fluxup(k)=0.0_r8
               fluxdn(k)=0.0_r8
               fluxdndir(k) = 0.0_r8 ! amontornes-bcodina (2014-04-20)

               do iconfig = 1, nconfig
                  xwgt = wgtv(iconfig)
                  xexpt = exptdn(k,iconfig)
                  xtdnt = tdntot(k,iconfig)
                  xrdnd = rdndif(k,iconfig)
                  xrupd = rupdif(k,iconfig)
                  xrups = rupdir(k,iconfig)
! 
! Flux computation
! 
                  rdenom = 1._r8/(1._r8 - xrdnd * xrupd)

                  fluxup(k) = fluxup(k) + xwgt *  &
                              ((xexpt * xrups + (xtdnt - xexpt) * xrupd) * rdenom)
                  fluxdn(k) = fluxdn(k) + xwgt *  &
                              (xexpt + (xtdnt - xexpt + xexpt * xrups * xrdnd) * rdenom)
                  fluxdndir(k) = fluxdndir(k) + xwgt * xexpt ! Beer's Law amontornes-bcodina (2014-04-20)
                  
! 
! End do iconfig = 1, nconfig
! 
               end do
! 
! Normalize by total area covered by cloud configurations included
! in solution
! 
               fluxup(k)=fluxup(k) / totwgt
               fluxdn(k)=fluxdn(k) / totwgt           
               fluxdndir(k)=fluxdndir(k) / totwgt    ! amontornes-bcodina (2014-04-20)
! 
! End do k = 0,pverp
! 
            end do
! 
! Initialize the direct-beam flux at surface
! 
            wexptdn = 0.0_r8

            do iconfig = 1, nconfig
               wexptdn =  wexptdn + wgtv(iconfig) * exptdn(pverp,iconfig)
            end do

            wexptdn = wexptdn / totwgt
! 
! Monochromatic computation completed; accumulate in totals
! 
            solflx   = solin(i)*frcsol(ns)*psf(ns)
            fsnt(i)  = fsnt(i) + solflx*(fluxdn(1) - fluxup(1))
            fsntoa(i)= fsntoa(i) + solflx*(fluxdn(0) - fluxup(0))
            fsns(i)  = fsns(i) + solflx*(fluxdn(pverp)-fluxup(pverp))
            sfltot   = sfltot + solflx
            fswup(0) = fswup(0) + solflx*fluxup(0)
            fswdn(0) = fswdn(0) + solflx*fluxdn(0)
            fswdndir(0) = fswdndir(0) + solflx*fluxdndir(0)  ! amontornes-bcodina (2014-04-20)
! 
! Down spectral fluxes need to be in mks; thus the .001 conversion factors
! 
            if (wavmid(ns) < 0.7_r8) then
               sols(i)  = sols(i) + wexptdn*solflx*0.001_r8
               solsd(i) = solsd(i)+(fluxdn(pverp)-wexptdn)*solflx*0.001_r8
            else
               soll(i)  = soll(i) + wexptdn*solflx*0.001_r8
               solld(i) = solld(i)+(fluxdn(pverp)-wexptdn)*solflx*0.001_r8
               fsnrtoaq(i) = fsnrtoaq(i) + solflx*(fluxdn(0) - fluxup(0))
            end if
            fsnirtoa(i) = fsnirtoa(i) + wgtint*solflx*(fluxdn(0) - fluxup(0))

            do k=0,pver
! 
! Compute flux divergence in each layer using the interface up and down
! fluxes:
! 
               kp1 = k+1
               flxdiv = (fluxdn(k  ) - fluxdn(kp1)) + (fluxup(kp1) - fluxup(k  ))
               totfld(k)  = totfld(k)  + solflx*flxdiv
               fswdn(kp1) = fswdn(kp1) + solflx*fluxdn(kp1)
               fswup(kp1) = fswup(kp1) + solflx*fluxup(kp1)
               fswdndir(kp1) = fswdndir(kp1) + solflx*fluxdndir(kp1)  ! amontornes-bcodina (2014-04-20)
            end do
! 
! Perform clear-sky calculation
! 
            exptdnc(0) =   1.0_r8
            rdndifc(0) =   0.0_r8
            tdntotc(0) =   1.0_r8
            rupdirc(pverp) = albdir(i,ns)
            rupdifc(pverp) = albdif(i,ns)

            do k = 1, pverp
               km1 = k - 1
               xexpt = exptdnc(km1)
               xrdnd = rdndifc(km1)
               yrdnd = rdifc(ns,i,km1)
               ytdnd = tdifc(ns,i,km1)

               exptdnc(k) = xexpt*explayc(ns,i,km1)

               rdenom  = 1._r8/(1._r8 - yrdnd*xrdnd)
               rdirexp = rdirc(ns,i,km1)*xexpt
               tdnmexp = tdntotc(km1) - xexpt

               tdntotc(k) = xexpt*tdirc(ns,i,km1) + ytdnd*(tdnmexp + xrdnd*rdirexp)* &
                                rdenom
               rdndifc(k) = yrdnd + xrdnd*(ytdnd**2)*rdenom
            end do

            do k=pver,0,-1
               xrupd = rupdifc(k+1)
               yexpt = explayc(ns,i,k)
               yrupd = rdifc(ns,i,k)
               ytupd = tdifc(ns,i,k)

               rdenom = 1._r8/( 1._r8 - yrupd*xrupd)

               rupdirc(k) = rdirc(ns,i,k) + ytupd*(rupdirc(k+1)*yexpt + &
                            xrupd*(tdirc(ns,i,k)-yexpt))*rdenom
               rupdifc(k) = yrupd + xrupd*ytupd**2*rdenom
            end do

            do k=0,1
               rdenom    = 1._r8/(1._r8 - rdndifc(k)*rupdifc(k))
               fluxup(k) = (exptdnc(k)*rupdirc(k) + (tdntotc(k)-exptdnc(k))*rupdifc(k))* &
                           rdenom
               fluxdn(k) = exptdnc(k) + &
                           (tdntotc(k) - exptdnc(k) + exptdnc(k)*rupdirc(k)*rdndifc(k))* &
                           rdenom
               fswupc(k) = fswupc(k) + solflx*fluxup(k)
               fswdnc(k) = fswdnc(k) + solflx*fluxdn(k)
               fswdncdir(k) = fswdncdir(k) + solflx*exptdnc(k) ! Beer's Law amontornes-bcodina (2014-04-20)
            end do
!           k = pverp
            do k=2,pverp
            rdenom      = 1._r8/(1._r8 - rdndifc(k)*rupdifc(k))
            fluxup(k)   = (exptdnc(k)*rupdirc(k) + (tdntotc(k)-exptdnc(k))*rupdifc(k))* &
                           rdenom
            fluxdn(k)   = exptdnc(k) + (tdntotc(k) - exptdnc(k) + &
                          exptdnc(k)*rupdirc(k)*rdndifc(k))*rdenom
            fswupc(k)   = fswupc(k) + solflx*fluxup(k)
            fswdnc(k)   = fswdnc(k) + solflx*fluxdn(k)
            fswdncdir(k) = fswdncdir(k) + solflx*exptdnc(k) ! Beer's Law amontornes-bcodina (2014-04-20)
            end do

            fsntc(i)    = fsntc(i)+solflx*(fluxdn(1)-fluxup(1))
            fsntoac(i)  = fsntoac(i)+solflx*(fluxdn(0)-fluxup(0))
            fsnsc(i)    = fsnsc(i)+solflx*(fluxdn(pverp)-fluxup(pverp))
            fsdsc(i)    = fsdsc(i)+solflx*(fluxdn(pverp))
            fsnrtoac(i) = fsnrtoac(i)+wgtint*solflx*(fluxdn(0)-fluxup(0))
! 
! End of clear sky calculation
! 

! 
! End of spectral interval loop
! 
         end do
! 
! Compute solar heating rate (J/kg/s)
! 
         do k=1,pver
            qrs(i,k) = -1.E-4*gravit*totfld(k)/(pint(i,k) - pint(i,k+1))
         end do

! Added downward/upward total and clear sky fluxes

         do k=1,pverp
            fsup(i,k)  = fswup(k)
            fsupc(i,k) = fswupc(k)
            fsdn(i,k)  = fswdn(k)
            fsdnc(i,k) = fswdnc(k)
            fsdndir(i,k)  = fswdndir(k)            ! amontornes-bcodina (2014-04-20)
            fsdncdir(i,k) = fswdncdir(k)           ! amontornes-bcodina (2014-04-20)
            fsdndif(i,k)  = fswdn(k)-fswdndir(k)   ! amontornes-bcodina (2014-04-20)
            fsdncdif(i,k) = fswdnc(k)-fswdncdir(k) ! amontornes-bcodina (2014-04-20)
         end do

! 
! Set the downwelling flux at the surface 
! 
         fsds(i) = fswdn(pverp)
! amontornes-bcodina (2014-04-20) :: Save surface direct/difuse fluxes
      fsdscdir(i) = fsdncdir(i,pverp) ! amontornes-bcodina (2014-04-20)
      fsdscdif(i) = fsdncdif(i,pverp) ! amontornes-bcodina (2014-04-20)
      fsdsdir(i)  = fsdndir(i,pverp)  ! amontornes-bcodina (2014-04-20)
      fsdsdif(i)  = fsdndif(i,pverp)  ! amontornes-bcodina (2014-04-20)
! 
! End do n=1,ndayc
! 
   end do

!  write (6, '(a, x, i3)') 'radcswmx : exiting, chunk identifier', lchnk

   return
end subroutine radcswmx

subroutine raddedmx(pver, pverp, pcols, coszrs  ,ndayc   ,idayc   ,abh2o   , &
                    abo3    ,abco2   ,abo2    ,uh2o    ,uo3     , &
                    uco2    ,uo2     ,trayoslp,pflx    ,ns      , &
                    tauxcl  ,wcl     ,gcl     ,fcl     ,tauxci  , &
                    wci     ,gci     ,fci     ,tauxar  ,wa      , &
                    ga      ,fa      ,rdir    ,rdif    ,tdir    , &
                    tdif    ,explay  ,rdirc   ,rdifc   ,tdirc   , &
                    tdifc   ,explayc )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Computes layer reflectivities and transmissivities, from the top down
! to the surface using the delta-Eddington solutions for each layer
! 
! Method: 
! For more details , see Briegleb, Bruce P., 1992: Delta-Eddington
! Approximation for Solar Radiation in the NCAR Community Climate Model,
! Journal of Geophysical Research, Vol 97, D7, pp7603-7612).
!
! Modified for maximum/random cloud overlap by Bill Collins and John
!    Truesdale
! 
! Author: Bill Collins
! 
!-----------------------------------------------------------------------
!  use shr_kind_mod, only: r8 => shr_kind_r8
!  use ppgrid

   implicit none

   integer nspint           ! Num of spctrl intervals across solar spectrum

   parameter ( nspint = 19 )
!
! Minimum total transmission below which no layer computation are done:
!
   real(r8) trmin                ! Minimum total transmission allowed
   real(r8) wray                 ! Rayleigh single scatter albedo
   real(r8) gray                 ! Rayleigh asymetry parameter
   real(r8) fray                 ! Rayleigh forward scattered fraction

   parameter (trmin = 1.e-3)
   parameter (wray = 0.999999)
   parameter (gray = 0.0)
   parameter (fray = 0.1)
!
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: pver, pverp, pcols
   real(r8), intent(in) :: coszrs(pcols)        ! Cosine zenith angle
   real(r8), intent(in) :: trayoslp             ! Tray/sslp
   real(r8), intent(in) :: pflx(pcols,0:pverp)  ! Interface pressure
   real(r8), intent(in) :: abh2o                ! Absorption coefficiant for h2o
   real(r8), intent(in) :: abo3                 ! Absorption coefficiant for o3
   real(r8), intent(in) :: abco2                ! Absorption coefficiant for co2
   real(r8), intent(in) :: abo2                 ! Absorption coefficiant for o2
   real(r8), intent(in) :: uh2o(pcols,0:pver)   ! Layer absorber amount of h2o
   real(r8), intent(in) :: uo3(pcols,0:pver)    ! Layer absorber amount of  o3
   real(r8), intent(in) :: uco2(pcols,0:pver)   ! Layer absorber amount of co2
   real(r8), intent(in) :: uo2(pcols,0:pver)    ! Layer absorber amount of  o2
   real(r8), intent(in) :: tauxcl(pcols,0:pver) ! Cloud extinction optical depth (liquid)
   real(r8), intent(in) :: wcl(pcols,0:pver)    ! Cloud single scattering albedo (liquid)
   real(r8), intent(in) :: gcl(pcols,0:pver)    ! Cloud asymmetry parameter (liquid)
   real(r8), intent(in) :: fcl(pcols,0:pver)    ! Cloud forward scattered fraction (liquid)
   real(r8), intent(in) :: tauxci(pcols,0:pver) ! Cloud extinction optical depth (ice)
   real(r8), intent(in) :: wci(pcols,0:pver)    ! Cloud single scattering albedo (ice)
   real(r8), intent(in) :: gci(pcols,0:pver)    ! Cloud asymmetry parameter (ice)
   real(r8), intent(in) :: fci(pcols,0:pver)    ! Cloud forward scattered fraction (ice)
   real(r8), intent(in) :: tauxar(pcols,0:pver) ! Aerosol extinction optical depth
   real(r8), intent(in) :: wa(pcols,0:pver)     ! Aerosol single scattering albedo
   real(r8), intent(in) :: ga(pcols,0:pver)     ! Aerosol asymmetry parameter
   real(r8), intent(in) :: fa(pcols,0:pver)     ! Aerosol forward scattered fraction

   integer, intent(in) :: ndayc                 ! Number of daylight columns
   integer, intent(in) :: idayc(pcols)          ! Daylight column indices
   integer, intent(in) :: ns                    ! Index of spectral interval
!
! Input/Output arguments
!
! Following variables are defined for each layer; 0 refers to extra
! layer above top of model:
!
   real(r8), intent(inout) :: rdir(nspint,pcols,0:pver)   ! Layer reflectivity to direct rad
   real(r8), intent(inout) :: rdif(nspint,pcols,0:pver)   ! Layer reflectivity to diffuse rad
   real(r8), intent(inout) :: tdir(nspint,pcols,0:pver)   ! Layer transmission to direct rad
   real(r8), intent(inout) :: tdif(nspint,pcols,0:pver)   ! Layer transmission to diffuse rad
   real(r8), intent(inout) :: explay(nspint,pcols,0:pver) ! Solar beam exp transm for layer
!
! Corresponding quantities for clear-skies
!
   real(r8), intent(inout) :: rdirc(nspint,pcols,0:pver)  ! Clear layer reflec. to direct rad
   real(r8), intent(inout) :: rdifc(nspint,pcols,0:pver)  ! Clear layer reflec. to diffuse rad
   real(r8), intent(inout) :: tdirc(nspint,pcols,0:pver)  ! Clear layer trans. to direct rad
   real(r8), intent(inout) :: tdifc(nspint,pcols,0:pver)  ! Clear layer trans. to diffuse rad
   real(r8), intent(inout) :: explayc(nspint,pcols,0:pver)! Solar beam exp transm clear layer
!
!---------------------------Local variables-----------------------------
!
   integer i                 ! Column indices
   integer k                 ! Level index
   integer nn                ! Index of column loops (max=ndayc)

   real(r8) taugab(pcols)        ! Layer total gas absorption optical depth
   real(r8) tauray(pcols)        ! Layer rayleigh optical depth
   real(r8) taucsc               ! Layer cloud scattering optical depth
   real(r8) tautot               ! Total layer optical depth
   real(r8) wtot                 ! Total layer single scatter albedo
   real(r8) gtot                 ! Total layer asymmetry parameter
   real(r8) ftot                 ! Total layer forward scatter fraction
   real(r8) wtau                 !  rayleigh layer scattering optical depth
   real(r8) wt                   !  layer total single scattering albedo
   real(r8) ts                   !  layer scaled extinction optical depth
   real(r8) ws                   !  layer scaled single scattering albedo
   real(r8) gs                   !  layer scaled asymmetry parameter
!
!---------------------------Statement functions-------------------------
!
! Statement functions and other local variables
!
   real(r8) alpha                ! Term in direct reflect and transmissivity
   real(r8) gamma                ! Term in direct reflect and transmissivity
   real(r8) el                   ! Term in alpha,gamma,n,u
   real(r8) taus                 ! Scaled extinction optical depth
   real(r8) omgs                 ! Scaled single particle scattering albedo
   real(r8) asys                 ! Scaled asymmetry parameter
   real(r8) u                    ! Term in diffuse reflect and
!    transmissivity
   real(r8) n                    ! Term in diffuse reflect and
!    transmissivity
   real(r8) lm                   ! Temporary for el
   real(r8) ne                   ! Temporary for n
   real(r8) w                    ! Dummy argument for statement function
   real(r8) uu                   ! Dummy argument for statement function
   real(r8) g                    ! Dummy argument for statement function
   real(r8) e                    ! Dummy argument for statement function
   real(r8) f                    ! Dummy argument for statement function
   real(r8) t                    ! Dummy argument for statement function
   real(r8) et                   ! Dummy argument for statement function
!
! Intermediate terms for delta-eddington solution
!
   real(r8) alp                  ! Temporary for alpha
   real(r8) gam                  ! Temporary for gamma
   real(r8) ue                   ! Temporary for u
   real(r8) arg                  ! Exponential argument
   real(r8) extins               ! Extinction
   real(r8) amg                  ! Alp - gam
   real(r8) apg                  ! Alp + gam
!
   alpha(w,uu,g,e) = .75_r8*w*uu*((1._r8 + g*(1._r8-w))/(1._r8 - e*e*uu*uu))
   gamma(w,uu,g,e) = .50_r8*w*((3._r8*g*(1._r8-w)*uu*uu + 1._r8)/(1._r8-e*e*uu*uu))
   el(w,g)         = sqrt(3._r8*(1._r8-w)*(1._r8 - w*g))
   taus(w,f,t)     = (1._r8 - w*f)*t
   omgs(w,f)       = (1._r8 - f)*w/(1._r8 - w*f)
   asys(g,f)       = (g - f)/(1._r8 - f)
   u(w,g,e)        = 1.5_r8*(1._r8 - w*g)/e
   n(uu,et)        = ((uu+1._r8)*(uu+1._r8)/et ) - ((uu-1._r8)*(uu-1._r8)*et)
!
!-----------------------------------------------------------------------
!
! Compute layer radiative properties
!
! Compute radiative properties (reflectivity and transmissivity for
!    direct and diffuse radiation incident from above, under clear
!    and cloudy conditions) and transmission of direct radiation
!    (under clear and cloudy conditions) for each layer.
!
   do k=0,pver
      do nn=1,ndayc
         i=idayc(nn)
            tauray(i) = trayoslp*(pflx(i,k+1)-pflx(i,k))
            taugab(i) = abh2o*uh2o(i,k) + abo3*uo3(i,k) + abco2*uco2(i,k) + abo2*uo2(i,k)
            tautot = tauxcl(i,k) + tauxci(i,k) + tauray(i) + taugab(i) + tauxar(i,k)
            taucsc = tauxcl(i,k)*wcl(i,k) + tauxci(i,k)*wci(i,k) + tauxar(i,k)*wa(i,k)
            wtau   = wray*tauray(i)
            wt     = wtau + taucsc
            wtot   = wt/tautot
            gtot   = (wtau*gray + gcl(i,k)*wcl(i,k)*tauxcl(i,k) &
                     + gci(i,k)*wci(i,k)*tauxci(i,k) + ga(i,k) *wa(i,k) *tauxar(i,k))/wt
            ftot   = (wtau*fray + fcl(i,k)*wcl(i,k)*tauxcl(i,k) &
                     + fci(i,k)*wci(i,k)*tauxci(i,k) + fa(i,k) *wa(i,k) *tauxar(i,k))/wt
            ts   = taus(wtot,ftot,tautot)
            ws   = omgs(wtot,ftot)
            gs   = asys(gtot,ftot)
            lm   = el(ws,gs)
            alp  = alpha(ws,coszrs(i),gs,lm)
            gam  = gamma(ws,coszrs(i),gs,lm)
            ue   = u(ws,gs,lm)
!
!     Limit argument of exponential to 25, in case lm very large:
!
            arg  = min(lm*ts,25._r8)
            extins = exp(-arg)
            ne = n(ue,extins)
            rdif(ns,i,k) = (ue+1._r8)*(ue-1._r8)*(1._r8/extins - extins)/ne
            tdif(ns,i,k)   =   4._r8*ue/ne
!
!     Limit argument of exponential to 25, in case coszrs is very small:
!
            arg       = min(ts/coszrs(i),25._r8)
            explay(ns,i,k) = exp(-arg)
            apg = alp + gam
            amg = alp - gam
            rdir(ns,i,k) = amg*(tdif(ns,i,k)*explay(ns,i,k)-1._r8) + apg*rdif(ns,i,k)
            tdir(ns,i,k) = apg*tdif(ns,i,k) + (amg*rdif(ns,i,k)-(apg-1._r8))*explay(ns,i,k)
!
!     Under rare conditions, reflectivies and transmissivities can be
!     negative; zero out any negative values
!
            rdir(ns,i,k) = max(rdir(ns,i,k),0.0_r8)
            tdir(ns,i,k) = max(tdir(ns,i,k),0.0_r8)
            rdif(ns,i,k) = max(rdif(ns,i,k),0.0_r8)
            tdif(ns,i,k) = max(tdif(ns,i,k),0.0_r8)
!
!     Clear-sky calculation
!
            if (tauxcl(i,k) == 0.0_r8 .and. tauxci(i,k) == 0.0_r8) then

               rdirc(ns,i,k) = rdir(ns,i,k)
               tdirc(ns,i,k) = tdir(ns,i,k)
               rdifc(ns,i,k) = rdif(ns,i,k)
               tdifc(ns,i,k) = tdif(ns,i,k)
               explayc(ns,i,k) = explay(ns,i,k)
            else
               tautot = tauray(i) + taugab(i) + tauxar(i,k)
               taucsc = tauxar(i,k)*wa(i,k)
!
! wtau already computed for all-sky
!
               wt     = wtau + taucsc
               wtot   = wt/tautot
               gtot   = (wtau*gray + ga(i,k)*wa(i,k)*tauxar(i,k))/wt
               ftot   = (wtau*fray + fa(i,k)*wa(i,k)*tauxar(i,k))/wt
               ts   = taus(wtot,ftot,tautot)
               ws   = omgs(wtot,ftot)
               gs   = asys(gtot,ftot)
               lm   = el(ws,gs)
               alp  = alpha(ws,coszrs(i),gs,lm)
               gam  = gamma(ws,coszrs(i),gs,lm)
               ue   = u(ws,gs,lm)
!
!     Limit argument of exponential to 25, in case lm very large:
!
               arg  = min(lm*ts,25._r8)
               extins = exp(-arg)
               ne = n(ue,extins)
               rdifc(ns,i,k) = (ue+1._r8)*(ue-1._r8)*(1._r8/extins - extins)/ne
               tdifc(ns,i,k)   =   4._r8*ue/ne
!
!     Limit argument of exponential to 25, in case coszrs is very small:
!
               arg       = min(ts/coszrs(i),25._r8)
               explayc(ns,i,k) = exp(-arg)
               apg = alp + gam
               amg = alp - gam
               rdirc(ns,i,k) = amg*(tdifc(ns,i,k)*explayc(ns,i,k)-1._r8)+ &
                               apg*rdifc(ns,i,k)
               tdirc(ns,i,k) = apg*tdifc(ns,i,k) + (amg*rdifc(ns,i,k) - (apg-1._r8))* &
                               explayc(ns,i,k)
!
!     Under rare conditions, reflectivies and transmissivities can be
!     negative; zero out any negative values
!
               rdirc(ns,i,k) = max(rdirc(ns,i,k),0.0_r8)
               tdirc(ns,i,k) = max(tdirc(ns,i,k),0.0_r8)
               rdifc(ns,i,k) = max(rdifc(ns,i,k),0.0_r8)
               tdifc(ns,i,k) = max(tdifc(ns,i,k),0.0_r8)
            end if
         end do
   end do

   return
end subroutine raddedmx

subroutine radinp(lchnk   ,ncol    , pcols, pver, pverp,     &
                  pmid    ,pint    ,o3vmr   , pmidrd  ,&
                  pintrd  ,eccf    ,o3mmr   )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Set latitude and time dependent arrays for input to solar
! and longwave radiation.
! Convert model pressures to cgs, and compute ozone mixing ratio, needed for
! the solar radiation.
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: CCM1, CMS Contact J. Kiehl
! 
!-----------------------------------------------------------------------
!  use shr_kind_mod, only: r8 => shr_kind_r8
!  use ppgrid
!  use time_manager, only: get_curr_calday

   implicit none

!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: lchnk                ! chunk identifier
   integer, intent(in) :: pcols, pver, pverp
   integer, intent(in) :: ncol                 ! number of atmospheric columns

   real(r8), intent(in) :: pmid(pcols,pver)    ! Pressure at model mid-levels (pascals)
   real(r8), intent(in) :: pint(pcols,pverp)   ! Pressure at model interfaces (pascals)
   real(r8), intent(in) :: o3vmr(pcols,pver)   ! ozone volume mixing ratio
!
! Output arguments
!
   real(r8), intent(out) :: pmidrd(pcols,pver)  ! Pressure at mid-levels (dynes/cm*2)
   real(r8), intent(out) :: pintrd(pcols,pverp) ! Pressure at interfaces (dynes/cm*2)
   real(r8), intent(out) :: eccf                ! Earth-sun distance factor
   real(r8), intent(out) :: o3mmr(pcols,pver)   ! Ozone mass mixing ratio

!
!---------------------------Local variables-----------------------------
!
   integer i                ! Longitude loop index
   integer k                ! Vertical loop index

   real(r8) :: calday           ! current calendar day
   real(r8) vmmr                ! Ozone volume mixing ratio
   real(r8) delta               ! Solar declination angle

!
!-----------------------------------------------------------------------
!
!  calday = get_curr_calday()
   eccf = 1. ! declared intent(out) so fill a value (not used in WRF)
!  call shr_orb_decl (calday  ,eccen     ,mvelpp  ,lambm0  ,obliqr  , &
!                     delta   ,eccf)

!
! Convert pressure from pascals to dynes/cm2
!
   do k=1,pver
      do i=1,ncol
         pmidrd(i,k) = pmid(i,k)*10.0
         pintrd(i,k) = pint(i,k)*10.0
      end do
   end do
   do i=1,ncol
      pintrd(i,pverp) = pint(i,pverp)*10.0
   end do
!
! Convert ozone volume mixing ratio to mass mixing ratio:
!
   vmmr = amo/amd
   do k=1,pver
      do i=1,ncol
         o3mmr(i,k) = vmmr*o3vmr(i,k)
      end do
   end do
!
   return
end subroutine radinp
subroutine radoz2(lchnk   ,ncol    ,pcols, pver, pverp, o3vmr   ,pint    ,plol    ,plos, ntoplw    )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Computes the path length integrals to the model interfaces given the
! ozone volume mixing ratio
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: CCM1, CMS Contact J. Kiehl
! 
!-----------------------------------------------------------------------
!  use shr_kind_mod, only: r8 => shr_kind_r8
!  use ppgrid
!  use comozp

   implicit none
!------------------------------Input arguments--------------------------
!
   integer, intent(in) :: lchnk                ! chunk identifier
   integer, intent(in) :: ncol                 ! number of atmospheric columns
   integer, intent(in) :: pcols, pver, pverp

   real(r8), intent(in) :: o3vmr(pcols,pver)   ! ozone volume mixing ratio
   real(r8), intent(in) :: pint(pcols,pverp)   ! Model interface pressures

   integer, intent(in) :: ntoplw               ! topmost level/layer longwave is solved for

!
!----------------------------Output arguments---------------------------
!
   real(r8), intent(out) :: plol(pcols,pverp)   ! Ozone prs weighted path length (cm)
   real(r8), intent(out) :: plos(pcols,pverp)   ! Ozone path length (cm)

!
!---------------------------Local workspace-----------------------------
!
   integer i                ! longitude index
   integer k                ! level index
!
!-----------------------------------------------------------------------
!
! Evaluate the ozone path length integrals to interfaces;
! factors of .1 and .01 to convert pressures from cgs to mks:
!
   do i=1,ncol
      plos(i,ntoplw) = 0.1 *cplos*o3vmr(i,ntoplw)*pint(i,ntoplw)
      plol(i,ntoplw) = 0.01*cplol*o3vmr(i,ntoplw)*pint(i,ntoplw)*pint(i,ntoplw)
   end do
   do k=ntoplw+1,pverp
      do i=1,ncol
         plos(i,k) = plos(i,k-1) + 0.1*cplos*o3vmr(i,k-1)*(pint(i,k) - pint(i,k-1))
         plol(i,k) = plol(i,k-1) + 0.01*cplol*o3vmr(i,k-1)* &
                    (pint(i,k)*pint(i,k) - pint(i,k-1)*pint(i,k-1))
      end do
   end do
!
   return
end subroutine radoz2


subroutine radozn (lchnk, ncol, pcols, pver,pmid, pin, levsiz, ozmix, o3vmr)
!----------------------------------------------------------------------- 
! 
! Purpose: Interpolate ozone from current time-interpolated values to model levels
! 
! Method: Use pressure values to determine interpolation levels
! 
! Author: Bruce Briegleb
! 
!--------------------------------------------------------------------------
!  use shr_kind_mod, only: r8 => shr_kind_r8
!  use ppgrid
!  use phys_grid,     only: get_lat_all_p, get_lon_all_p
!  use comozp
!  use abortutils, only: endrun
!--------------------------------------------------------------------------
   implicit none
!--------------------------------------------------------------------------
!
! Arguments
!
   integer, intent(in) :: lchnk               ! chunk identifier
   integer, intent(in) :: pcols, pver
   integer, intent(in) :: ncol                ! number of atmospheric columns
   integer, intent(in) :: levsiz              ! number of ozone layers

   real(r8), intent(in) :: pmid(pcols,pver)   ! level pressures (mks)
   real(r8), intent(in) :: pin(levsiz)        ! ozone data level pressures (mks)
   real(r8), intent(in) :: ozmix(pcols,levsiz) ! ozone mixing ratio

   real(r8), intent(out) :: o3vmr(pcols,pver) ! ozone volume mixing ratio
!
! local storage
!
   integer i                   ! longitude index
   integer k, kk, kkstart      ! level indices
   integer kupper(pcols)       ! Level indices for interpolation
   integer kount               ! Counter
   integer lats(pcols)         ! latitude indices
   integer lons(pcols)         ! latitude indices

   real(r8) dpu                ! upper level pressure difference
   real(r8) dpl                ! lower level pressure difference
!
! Initialize latitude indices
!
!  call get_lat_all_p(lchnk, ncol, lats)
!  call get_lon_all_p(lchnk, ncol, lons)
!
! Initialize index array
!
   do i=1,ncol
      kupper(i) = 1
   end do

   do k=1,pver
!
! Top level we need to start looking is the top level for the previous k
! for all longitude points
!
      kkstart = levsiz
      do i=1,ncol
         kkstart = min0(kkstart,kupper(i))
      end do
      kount = 0
!
! Store level indices for interpolation
!
      do kk=kkstart,levsiz-1
         do i=1,ncol
            if (pin(kk).lt.pmid(i,k) .and. pmid(i,k).le.pin(kk+1)) then
               kupper(i) = kk
               kount = kount + 1
            end if
         end do
!
! If all indices for this level have been found, do the interpolation and
! go to the next level
!
         if (kount.eq.ncol) then
            do i=1,ncol
               dpu = pmid(i,k) - pin(kupper(i))
               dpl = pin(kupper(i)+1) - pmid(i,k)
               o3vmr(i,k) = (ozmix(i,kupper(i))*dpl + &
                             ozmix(i,kupper(i)+1)*dpu)/(dpl + dpu)
            end do
            goto 35
         end if
      end do
!
! If we've fallen through the kk=1,levsiz-1 loop, we cannot interpolate and
! must extrapolate from the bottom or top ozone data level for at least some
! of the longitude points.
!
      do i=1,ncol
         if (pmid(i,k) .lt. pin(1)) then
            o3vmr(i,k) = ozmix(i,1)*pmid(i,k)/pin(1)
         else if (pmid(i,k) .gt. pin(levsiz)) then
            o3vmr(i,k) = ozmix(i,levsiz)
         else
            dpu = pmid(i,k) - pin(kupper(i))
            dpl = pin(kupper(i)+1) - pmid(i,k)
            o3vmr(i,k) = (ozmix(i,kupper(i))*dpl + &
                          ozmix(i,kupper(i)+1)*dpu)/(dpl + dpu)
         end if
      end do

      if (kount.gt.ncol) then
         call endrun ('RADOZN: Bad ozone data: non-monotonicity suspected')
      end if
35    continue
   end do

   return
end subroutine radozn


#endif

end MODULE module_ra_cam
