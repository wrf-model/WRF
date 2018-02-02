!WRF:MODEL_MP:PHYSICS
!
!-- "Modified" fer_hires microphysics - 11 July 2016 version
!
!-- Updates based on NAM changes in 2011:
! (a) Expanded rain lookup tables from 0.45 mm to 1 mm mean diameter.
! (b) Allow cloud ice to fall (fall speeds based on 50 micron mean diameters).
! (c) Cloud water autoconversion to rain follows Liu et al. (JAS, 2006)
! (d) Fix to MY_GROWTH by multiplying estimates by 1.e-3
! (e) Added integer function GET_INDEXR
! (f) Added warning messages when unusual conditions occur, screened for
!     5 different types of problems, such as (1) condensate in the
!     stratosphere, (2) temperature = NaN, (3) water supersaturation at
!     <180K, (4) too many iterations (>10) in the condensation function,
!     and (5) too many iterations (>10) in the deposition function.  
!
!-- Updates based on jan19 2014 changes in the NMMB:
!
! (1) Ice nucleation: Fletcher (1962) replaces Meyers et al. (1992)
! (2) Cloud ice is a simple function of the number concentration from (1), and it
!     is no longer a fractional function of the large ice.  Thus, the FLARGE &
!     FSMALL parameters are no longer used.
! (3) T_ICE_init=-12 deg C provides a slight delay in the initial onset of ice.
! (4) NLImax is a function of rime factor (RF) and temperature.  
!    a) For RF>10, NLImax=1.e3.  Mean ice diameters can exceed the 1 mm maximum
!       size in the tables so that NLICE=NLImax=1.e3.
!    b) Otherwise, NLImax is 10 L-1 at 0C and decreasing to 5 L-1 at <=-40C.  
!       NLICE>NLImax at the maximum ice diameter of 1 mm.
! (5) Can turn off ice processes by setting T_ICE & T_ICE_init to be < -100 deg C
! (6) Modified the homogeneous freezing of cloud water when T<T_ICE.
! (7) Reduce the fall speeds of rimed ice by making VEL_INC a function of 
!     VrimeF**1.5 and not VrimeF**2.
! (8) RHgrd=0.98 (98% RH) for the onset of condensation, to match what's been
!     tested for many months in the NAMX.  Made obsolete with change in (13).
! (9) Rime factor is *never* adjusted when NLICE>NLImax.  
! (10) Ice deposition does not change the rime factor (RF) when RF>=10 & T>T_ICE.
! (11) Limit GAMMAS to <=1.5 (air resistance impact on ice fall speeds)
! (12) NSImax is maximum # conc of ice crystals.  At cold temperature NSImax is
!      calculated based on assuming 10% of total ice content is due to cloud ice.
!
!-- Further modifications starting on 23 July 2015
! (13) RHgrd is passed in as an input argument so that it can vary for different
!      domains (RHgrd=0.98 for 12-km parent, 1.0 for 3-km nests)
! (14) Use the old "PRAUT" cloud water autoconversion *threshold* (QAUT0)

!-- Further modifications starting on 28 July 2015
! (15) Added calculations for radar reflectivity and number concentrations of
!      rain (Nrain) and precipitating ice (Nsnow).
! (16) Removed double counting of air resistance term for riming onto ice (PIACW)
! (17) The maximum rime factor (RFmx) is now a function of MASSI(INDEXS), accounting
!      for the increase in unrimed ice particle densities as values of INDEXS
!      decrease from the maximum upper limit of 1000 microns to the lower limit of
!      50 microns, coinciding with the assumed size of cloud ice; see lines 1128-1134.
! (18) A new closure is used for updating the rime factor, which is described in
!      detail near lines 1643-1682.  The revised code is near lines 1683-1718.
! (19) Restructured the two-pass algorithm to be more robust, removed the HAIL
!      & LARGE_RF logical variables so that NLICE>NLImax can occur.
! (20) Increased nsimax (see !aug27 below)
! (21) Modified the rain sedimentation (see two !aug27 blocks below)
! (22) NInuclei is the lower of Fletcher (1962), Cooper (1986), or NSImax. 
! (23) NLImax is no longer used or enforced. Instead, INDEXS=MDImax when RF>20,
!      else INDEXS is a function of temperature. Look for !sep10 comment.
! (24) An override was inserted for (18), such that the rime density is not diluted
!      diluted when RF>20. Look for !sep10 comment.
! (25) Radar reflectivity calculations were changes to reduce radar bright bands,
!      limit enhanced, mixed-phase reflectivity to RF>=20. Look for !sep10 comments.
! (26) NLICE is not to exceed NSI_max (250 L^-1) when RF<20.  Look for !sep16 comments.
! Commented out! (28) Increase hail fall speeds using Thompson et al. (2008).  Look for !sep22 comments.
! (29) Modify NLImax, INDEXS for RF>=20. Look for !sep22 comments.
! (30) Check on NSmICE, Vci based on whether FLIMASS<1.  Look for !sep22a comments.
! Revised in (34)! (31) Introduced RFlag logical, which if =T enforces a lower limit of drop sizes not 
!      to go below INDEXRmin and N0r is adjusted.  Look for !nov25 comments (corrections,
!      refinements to sep25 & nov18 versions, includes an additional fix in nov25-fix).
!      Also set INDEXRmin=500 rather than 250 microns.
!-----------------------------------------------------------------------------
!--- The following changes now refer to dates when those were made in 2016.
!-----------------------------------------------------------------------------
! (32) Convective (RF>=20, Ng~10 L^-1, RHOg~500 kg m^-3), transition (RF=10, Ng~25 L^-1,
!      RHOg~300 kg m^-3), & stratiform (RF<2) profiles are blended based on RF. !mar08
! (33) Fixed bug in Biggs' freezing, put back in collisional drop freezing.  !mar03
! (34) Changes in (31) are revised so that INDEXRmin at and below 0C level is 
!      based on a rain rate equal to the snowfall rate above the 0C level.  !mar03
! (35) Increase radar reflectivity when RF>10 and RQSnew > 2.5 g m^-3. !mar12
! (36) !mar10 combines all elements of (32)-(35) together.
! (37) Bug fixes for the changes in (34) and the RFLAG variable  !apr18
! (38) Revised Schumann-Ludlam limit.  !apr18
! (39) Simplified PCOND (cloud cond/evap) calculation   !apr21
! (40) Slight change in calculating RF.   !apr22
! (41) Reduce RF values for calculating mean sizes of snow, graupel, sleet/hail  !apr22a
! (42) Increase reflectivity from large, wet, high rime factor ice (graupel) by
!      assuming |Kw|**2/|Ki|**2 = 0.224 (Smith, 1984, JCAM).
! (43) Major restructuring of code to allow N0r to vary from N0r0   !may11
! (44) More major restructuring of code to use fixed XLS, XLV, XLF   !may12
! (45) Increased VEL_INC ~ VrimeF**2, put the enhanced graupel/hail fall speeds
!      from Thompson into the code but only in limited circumstances, restructured
!      and streamlined the INDEXS calculation, removed the upper limit for
!      for the vapor mixing ratio is at water saturation when calculating ice
!      deposition, and N0r is gradually increased for conditions supporting
!      drizzle when rain contents decrease below 0.25 g/m**3.   !may17
! (46) The may11 code changes that increase N0r0 when rain contents exceed 1 g m^-3
!      have been removed, limit the number of iterations calculating final rain
!      parameters, remove the revised N0r calculation for reflectivity. All of
!      the changes following those made in the may10 code.  !may20
! (47) Reduce the assumed # concentration of hail/sleet when RF>10 from 5 L^-1 to
!      1 L^-1, and also reduce it for graupel when RF>5 from 10 L^-1 to 5 L^-1.
!      This is being done to try and make greater use of the Thompson graupel/hail 
!      fallspeeds by having INDEXS==MDImax.
! (48) Increased NCW from 200e6 to 300e6 for a more delayed onset of drizzle, 
!      simplified drizzle algorithm to reduce/eliminate N0r bulls eyes and to allow
!      for supercooled drizzle, and set limits for 8.e6 <= N0r (m^-4) <= 1.e9  !may31
! (49) Further restructuring of code to better define STRAT, DRZL logicals,
!      add these rain flags to mprates arrays   !jun01
! (50) Increase in reflectivity due to wet ice was commented out.
! (51) Fixed minor bug to update INDEXR2 in the "rain_pass: do" loop.   !jun13
! (52) Final changes to Nsnow for boosting reflectivities from ice for 
!      mass contents exceeding 5 g m^-3.  !jun16
! (53) Cosmetic changes only that do not affect the calculations. Removed old, unused 
!      diagnostic arrays. Updated comments.
!      
!-----------------------------------------------------------------------------
!

MODULE module_mp_fer_hires

!-----------------------------------------------------------------------
!-- The following changes were made on 24 July 2006.
!   (1) All known version 2.1 dependencies were removed from the 
!       operational WRF NMM model code (search for "!HWRF")
!   (2) Incorporated code changes from the GFDL model (search for "!GFDL")
!-----------------------------------------------------------------------
!
      REAL,PRIVATE,SAVE ::  ABFR, CBFR, CIACW, CIACR, C_N0r0,           &
     &  C_NR, CRAIN, &    
     &  ARAUT, BRAUT, CN0r0, CN0r_DMRmin, CN0r_DMRmax, CRACW, ESW0,     &
     &  RFmax, RQR_DRmin, RQR_DRmax, RR_DRmin, RR_DR1, RR_DR2,          &
     &  RR_DR3, RR_DR4, RR_DR5, RR_DRmax, BETA6, PI_E, RFmx1, ARcw,     & !jul31
     &  RH_NgC, RH_NgT, RQhail, AVhail, BVhail, QAUT0                !mar08 !may17
!
      INTEGER,PRIVATE,PARAMETER :: INDEXRstrmax=500      !mar03, stratiform maximum
      INTEGER, PRIVATE,PARAMETER :: MY_T1=1, MY_T2=35
      REAL,PRIVATE,DIMENSION(MY_T1:MY_T2),SAVE :: MY_GROWTH
!
      REAL, PRIVATE,PARAMETER :: DMImin=.05e-3, DMImax=1.e-3,           &
     &      DelDMI=1.e-6,XMImin=1.e6*DMImin
      REAL, PUBLIC,PARAMETER :: XMImax=1.e6*DMImax, XMIexp=.0536
      INTEGER, PUBLIC,PARAMETER :: MDImin=XMImin, MDImax=XMImax
      REAL, PRIVATE,DIMENSION(MDImin:MDImax) ::                         &
     &      ACCRI,VSNOWI,VENTI1,VENTI2
      REAL, PUBLIC,DIMENSION(MDImin:MDImax) :: SDENS    !-- For RRTM
!
      REAL, PRIVATE,PARAMETER :: DMRmin=.05e-3, DMRmax=1.0e-3,           &
     &      DelDMR=1.e-6, XMRmin=1.e6*DMRmin, XMRmax=1.e6*DMRmax
      INTEGER, PUBLIC,PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax
!
      REAL, PRIVATE,DIMENSION(MDRmin:MDRmax)::                           &
     &      ACCRR,MASSR,RRATE,VRAIN,VENTR1,VENTR2
!
      INTEGER, PRIVATE,PARAMETER :: Nrime=40
      REAL, DIMENSION(2:9,0:Nrime),PRIVATE,SAVE :: VEL_RF
!
      INTEGER,PARAMETER :: NX=7501
      REAL, PARAMETER :: XMIN=180.0,XMAX=330.0
      REAL, DIMENSION(NX),PRIVATE,SAVE :: TBPVS,TBPVS0
      REAL, PRIVATE,SAVE :: C1XPVS0,C2XPVS0,C1XPVS,C2XPVS
!
      REAL, PRIVATE,PARAMETER ::                                        &
!--- Physical constants follow:
     &   CP=1004.6, EPSQ=1.E-12, GRAV=9.806, RHOL=1000., RD=287.04      &
     &  ,RV=461.5, T0C=273.15, XLV=2.5E6, XLF=3.3358e+5                 &
     &  ,pi=3.141592653589793                                           &
!--- Derived physical constants follow:
     &  ,EPS=RD/RV, EPS1=RV/RD-1., EPSQ1=1.001*EPSQ                     &
     &  ,RCP=1./CP, RCPRV=RCP/RV, RGRAV=1./GRAV, RRHOL=1./RHOL          &
     &  ,XLS=XLV+XLF, XLV1=XLV/CP, XLF1=XLF/CP, XLS1=XLS/CP             &
     &  ,XLV2=XLV*XLV/RV, XLS2=XLS*XLS/RV                               &
     &  ,XLV3=XLV*XLV*RCPRV, XLS3=XLS*XLS*RCPRV                         &
!--- Constants specific to the parameterization follow:
!--- CLIMIT/CLIMIT1 are lower limits for treating accumulated precipitation
     &  ,CLIMIT=10.*EPSQ, CLIMIT1=-CLIMIT                               &
     &  ,C1=1./3.                                                       &
     &  ,DMR1=.1E-3, DMR2=.2E-3, DMR3=.32E-3, DMR4=0.45E-3              &
     &  ,DMR5=0.67E-3                                                   &
     &  ,XMR1=1.e6*DMR1, XMR2=1.e6*DMR2, XMR3=1.e6*DMR3                 &
     &  ,XMR4=1.e6*DMR4, XMR5=1.e6*DMR5, RQRmix=0.05E-3, RQSmix=1.E-3   & !jul28 !apr27
     &  ,Cdry=1.634e13, Cwet=1./.224       !jul28   !apr27
      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3, MDR4=XMR4  &
     &  , MDR5=XMR5

!-- Debug 20120111
LOGICAL, SAVE :: WARN1=.TRUE.,WARN2=.TRUE.,WARN3=.TRUE.,WARN5=.TRUE.
REAL, SAVE :: Pwarn=75.E2, QTwarn=1.E-3
INTEGER, PARAMETER :: MAX_ITERATIONS=10

!
! ======================================================================
!--- Important tunable parameters that are exported to other modules
!GFDL * RHgrd - generic reference to the threshold relative humidity for 
!GFDL           the onset of condensation
!GFDL (new) * RHgrd_in  - "RHgrd" for the inner domain
!GFDL (new) * RHgrd_out - "RHgrd" for the outer domain
!HWRF 6/11/2010 mod - use lower RHgrd_out for p < 850 hPa
!  * T_ICE - temperature (C) threshold at which all remaining liquid water
!            is glaciated to ice
!  * T_ICE_init - maximum temperature (C) at which ice nucleation occurs
!
!-- To turn off ice processes, set T_ICE & T_ICE_init to <= -100. (i.e., -100 C)
!
!  * NLImax - maximum number concentrations (m**-3) of large ice (snow/graupel/sleet) 
!  * NLImin - minimum number concentrations (m**-3) of large ice (snow/graupel/sleet) 
!  * NSImax - maximum number concentrations (m**-3) of small ice crystals
!  * N0r0 - assumed intercept (m**-4) of rain drops if drop diameters are between 0.2 and 1.0 mm
!  * N0rmin - minimum intercept (m**-4) for rain drops 
!  * NCW - number concentrations of cloud droplets (m**-3)
!  * PRINT_diag - for extended model diagnostics (code currently commented out)
! ======================================================================
      REAL, PUBLIC,PARAMETER ::                                         &
     &  RHgrd_in=1.                                                     &  !GFDL
     & ,RHgrd_out=0.975                                                 &  !GFDL
     & ,P_RHgrd_out=850.E2                                              &  !HWRF 6/11/2010
     & ,T_ICE=-40.                                                      &
     & ,T_ICEK=T0C+T_ICE                                                &
     & ,T_ICE_init=-12.                                                 &
     & ,NSI_max=250.E3                                                  &
     & ,NLImin=1.E3                                                     &
     & ,N0r0=8.E6                                                       &
     & ,N0rmin=1.E4                                                     &
!!2-09-2012     & ,NCW=60.E6                                                       &  !GFDL
!! based on Aligo's email,NCW is changed to 250E6
     & ,NCW=250.E6                                                         !GFDL
!HWRF     & ,NCW=100.E6                                                 &
      LOGICAL, PARAMETER :: PRINT_diag=.FALSE.  !GFDL
!--- Other public variables passed to other routines:
      REAL, PUBLIC,DIMENSION(MDImin:MDImax) :: MASSI
!
!
      CONTAINS

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE FER_HIRES (itimestep,DT,DX,DY,GID,RAINNC,RAINNCV,    & !GID
     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,qv,qt,   & !gopal's doing
     &                      LOWLYR,SR,                                &
     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,         &
     &                      QC,QR,QI,                                 &
     &                      ids,ide, jds,jde, kds,kde,                &
     &                      ims,ime, jms,jme, kms,kme,                &
     &                      its,ite, jts,jte, kts,kte                 )
!HWRF      SUBROUTINE ETAMP_NEW (itimestep,DT,DX,DY,                         &
!HWRF     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,qv,qc,     &
!HWRF     &                      LOWLYR,SR,                                  &
!HWRF     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           &
!HWRF     &                      mp_restart_state,tbpvs_state,tbpvs0_state,  &
!HWRF     &                      RAINNC,RAINNCV,                             &
!HWRF     &                      ids,ide, jds,jde, kds,kde,		        &
!HWRF     &                      ims,ime, jms,jme, kms,kme,		        &
!HWRF     &                      its,ite, jts,jte, kts,kte )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,ITIMESTEP,GID  ! GID gopal's doing

      REAL, INTENT(IN) 	    :: DT,DX,DY
      REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      dz8w,p_phy,pi_phy,rho_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      th_phy,qv,qt,qc,qr,qi
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::    &
     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)           ::     &
     &                                                   RAINNC,RAINNCV
      REAL, INTENT(OUT),    DIMENSION(ims:ime,jms:jme):: SR
!
!HWRF      REAL,DIMENSION(*),INTENT(INOUT) :: MP_RESTART_STATE
!
!HWRF      REAL,DIMENSION(nx),INTENT(INOUT) :: TBPVS_STATE,TBPVS0_STATE
!
      INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: LOWLYR

!-----------------------------------------------------------------------
!     LOCAL VARS
!-----------------------------------------------------------------------

!     SOME VARS WILL BE USED FOR DATA ASSIMILATION (DON'T NEED THEM NOW). 
!     THEY ARE TREATED AS LOCAL VARS, BUT WILL BECOME STATE VARS IN THE 
!     FUTURE. SO, WE DECLARED THEM AS MEMORY SIZES FOR THE FUTURE USE

!     TLATGS_PHY,TRAIN_PHY,APREC,PREC,ACPREC,SR are not directly related 
!     the microphysics scheme. Instead, they will be used by Eta precip 
!     assimilation.

      REAL,  DIMENSION( ims:ime, kms:kme, jms:jme ) ::                  &
     &       TLATGS_PHY,TRAIN_PHY
      REAL,  DIMENSION(ims:ime,jms:jme):: APREC,PREC,ACPREC
      REAL,  DIMENSION(its:ite, kts:kte, jts:jte):: t_phy

      INTEGER :: I,J,K,KFLIP
      REAL :: WC
!
!-----------------------------------------------------------------------
!**********************************************************************
!-----------------------------------------------------------------------
!
!HWRF      MY_GROWTH(MY_T1:MY_T2)=MP_RESTART_STATE(MY_T1:MY_T2)
!HWRF!
!HWRF      C1XPVS0=MP_RESTART_STATE(MY_T2+1)
!HWRF      C2XPVS0=MP_RESTART_STATE(MY_T2+2)
!HWRF      C1XPVS =MP_RESTART_STATE(MY_T2+3)
!HWRF      C2XPVS =MP_RESTART_STATE(MY_T2+4)
!HWRF      CIACW  =MP_RESTART_STATE(MY_T2+5)
!HWRF      CIACR  =MP_RESTART_STATE(MY_T2+6)
!HWRF      CRACW  =MP_RESTART_STATE(MY_T2+7)
!HWRF      CRAUT  =MP_RESTART_STATE(MY_T2+8)
!HWRF!
!HWRF      TBPVS(1:NX) =TBPVS_STATE(1:NX)
!HWRF      TBPVS0(1:NX)=TBPVS0_STATE(1:NX)
!
!----------
!2015-03-30, recalculate some constants which may depend on phy time step
          CALL MY_GROWTH_RATES (DT)

!--- CIACW is used in calculating riming rates
!      The assumed effective collection efficiency of cloud water rimed onto
!      ice is =0.5 below:
!
        CIACW=DT*0.25*PI_E*0.5*(1.E5)**C1
!
!--- CIACR is used in calculating freezing of rain colliding with large ice
!      The assumed collection efficiency is 1.0
!
        CIACR=PI_E*DT
!
!--- CRACW is used in calculating collection of cloud water by rain (an
!      assumed collection efficiency of 1.0)
!
        CRACW=DT*0.25*PI_E*1.0
!
!-- See comments in subroutine etanewhr_init starting with variable RDIS=
!
        BRAUT=DT*1.1E10*BETA6/NCW

!       write(*,*)'dt=',dt
!       write(*,*)'pi_e=',pi_e
!       write(*,*)'ciacw=',ciacw 
!       write(*,*)'ciacr=',ciacr 
!       write(*,*)'cracw=',cracw 
!       write(*,*)'araut=',araut
!       write(*,*)'braut=',braut
!! END OF adding, 2015-03-30
!-----------

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
        t_phy(i,k,j) = th_phy(i,k,j)*pi_phy(i,k,j)
        qv(i,k,j)=qv(i,k,j)/(1.+qv(i,k,j)) !Convert to specific humidity
      ENDDO
      ENDDO
      ENDDO

! initial data assimilation vars (will need to delete this part in the future)

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
	 TLATGS_PHY (i,k,j)=0.
	 TRAIN_PHY  (i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,jte
      DO i = its,ite
         ACPREC(i,j)=0.
         APREC (i,j)=0.
         PREC  (i,j)=0.
         SR    (i,j)=0.
      ENDDO
      ENDDO

!-- 6/11/2010: Update QT, F_ice, F_rain arrays
      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
         QT(I,K,J)=QC(I,K,J)+QR(I,K,J)+QI(I,K,J)
         IF (QI(I,K,J) <= EPSQ) THEN
            F_ICE_PHY(I,K,J)=0.
            F_RIMEF_PHY(I,K,J)=1.
            IF (T_PHY(I,K,J) < T_ICEK) F_ICE_PHY(I,K,J)=1.
         ELSE
            F_ICE_PHY(I,K,J)=MAX( 0., MIN(1., QI(I,K,J)/QT(I,K,J) ) )
         ENDIF
         IF (QR(I,K,J) <= EPSQ) THEN
            F_RAIN_PHY(I,K,J)=0.
         ELSE
            F_RAIN_PHY(I,K,J)=QR(I,K,J)/(QR(I,K,J)+QC(I,K,J))
         ENDIF
      ENDDO
      ENDDO
      ENDDO

!-----------------------------------------------------------------------

      CALL EGCP01DRV(GID,DT,LOWLYR,                                     &
     &               APREC,PREC,ACPREC,SR,                              &
     &               dz8w,rho_phy,qt,t_phy,qv,F_ICE_PHY,P_PHY,          &
     &               F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY,TRAIN_PHY,       &
     &               ids,ide, jds,jde, kds,kde,		                &
     &               ims,ime, jms,jme, kms,kme,		                &
     &               its,ite, jts,jte, kts,kte		          )
!-----------------------------------------------------------------------

     DO j = jts,jte
        DO k = kts,kte
	DO i = its,ite
  	   th_phy(i,k,j) = t_phy(i,k,j)/pi_phy(i,k,j)
           qv(i,k,j)=qv(i,k,j)/(1.-qv(i,k,j))  !Convert to mixing ratio
           WC=qt(I,K,J)
           QI(I,K,J)=0.
           QR(I,K,J)=0.
           QC(I,K,J)=0.
           IF(F_ICE_PHY(I,K,J)>=1.)THEN
             QI(I,K,J)=WC
           ELSEIF(F_ICE_PHY(I,K,J)<=0.)THEN
             QC(I,K,J)=WC
           ELSE
             QI(I,K,J)=F_ICE_PHY(I,K,J)*WC
             QC(I,K,J)=WC-QI(I,K,J)
           ENDIF
!
           IF(QC(I,K,J)>0..AND.F_RAIN_PHY(I,K,J)>0.)THEN
             IF(F_RAIN_PHY(I,K,J).GE.1.)THEN
               QR(I,K,J)=QC(I,K,J)
               QC(I,K,J)=0.
             ELSE
               QR(I,K,J)=F_RAIN_PHY(I,K,J)*QC(I,K,J)
               QC(I,K,J)=QC(I,K,J)-QR(I,K,J)
             ENDIF
          endif
	ENDDO
        ENDDO
     ENDDO
! 
! update rain (from m to mm)

       DO j=jts,jte
       DO i=its,ite
          RAINNC(i,j)=APREC(i,j)*1000.+RAINNC(i,j)
          RAINNCV(i,j)=APREC(i,j)*1000.
       ENDDO
       ENDDO
!
!HWRF     MP_RESTART_STATE(MY_T1:MY_T2)=MY_GROWTH(MY_T1:MY_T2)
!HWRF     MP_RESTART_STATE(MY_T2+1)=C1XPVS0
!HWRF     MP_RESTART_STATE(MY_T2+2)=C2XPVS0
!HWRF     MP_RESTART_STATE(MY_T2+3)=C1XPVS
!HWRF     MP_RESTART_STATE(MY_T2+4)=C2XPVS
!HWRF     MP_RESTART_STATE(MY_T2+5)=CIACW
!HWRF     MP_RESTART_STATE(MY_T2+6)=CIACR
!HWRF     MP_RESTART_STATE(MY_T2+7)=CRACW
!HWRF     MP_RESTART_STATE(MY_T2+8)=CRAUT
!HWRF!
!HWRF     TBPVS_STATE(1:NX) =TBPVS(1:NX)
!HWRF     TBPVS0_STATE(1:NX)=TBPVS0(1:NX)

!-----------------------------------------------------------------------

  END SUBROUTINE FER_HIRES

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! NOTE: The only differences between FER_HIRES and FER_HIRES_ADVECT
! is that the QT, and F_* are all local variables in the advected
! version, and QRIMEF is only in the advected version.  The innards
! are all the same.
      SUBROUTINE FER_HIRES_ADVECT (itimestep,DT,DX,DY,GID,RAINNC,RAINNCV,    & !GID
     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,qv,   & !gopal's doing
     &                      LOWLYR,SR,                                &
     &                      QC,QR,QI,QRIMEF,                          &
     &                      ids,ide, jds,jde, kds,kde,                &
     &                      ims,ime, jms,jme, kms,kme,                &
     &                      its,ite, jts,jte, kts,kte                 )
!HWRF      SUBROUTINE ETAMP_NEW (itimestep,DT,DX,DY,                         &
!HWRF     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,qv,qc,     &
!HWRF     &                      LOWLYR,SR,                                  &
!HWRF     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           &
!HWRF     &                      mp_restart_state,tbpvs_state,tbpvs0_state,  &
!HWRF     &                      RAINNC,RAINNCV,                             &
!HWRF     &                      ids,ide, jds,jde, kds,kde,		        &
!HWRF     &                      ims,ime, jms,jme, kms,kme,		        &
!HWRF     &                      its,ite, jts,jte, kts,kte )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,ITIMESTEP,GID  ! GID gopal's doing

      REAL, INTENT(IN) 	    :: DT,DX,DY
      REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      dz8w,p_phy,pi_phy,rho_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      th_phy,qv,qc,qr,qi,qrimef
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)           ::     &
     &                                                   RAINNC,RAINNCV
      REAL, INTENT(OUT),    DIMENSION(ims:ime,jms:jme):: SR
!
!HWRF      REAL,DIMENSION(*),INTENT(INOUT) :: MP_RESTART_STATE
!
!HWRF      REAL,DIMENSION(nx),INTENT(INOUT) :: TBPVS_STATE,TBPVS0_STATE
!
      INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: LOWLYR

!-----------------------------------------------------------------------
!     LOCAL VARS
!-----------------------------------------------------------------------

      REAL,                 DIMENSION(ims:ime, kms:kme, jms:jme ) ::    &
     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, QT

!     SOME VARS WILL BE USED FOR DATA ASSIMILATION (DON'T NEED THEM NOW). 
!     THEY ARE TREATED AS LOCAL VARS, BUT WILL BECOME STATE VARS IN THE 
!     FUTURE. SO, WE DECLARED THEM AS MEMORY SIZES FOR THE FUTURE USE
!     TLATGS_PHY,TRAIN_PHY,APREC,PREC,ACPREC,SR are not directly related 
!     the microphysics scheme. Instead, they will be used by Eta precip 
!     assimilation.

      REAL,  DIMENSION( ims:ime, kms:kme, jms:jme ) ::                  &
     &       TLATGS_PHY,TRAIN_PHY
      REAL,  DIMENSION(ims:ime,jms:jme):: APREC,PREC,ACPREC
      REAL,  DIMENSION(its:ite, kts:kte, jts:jte):: t_phy

      INTEGER :: I,J,K,KFLIP
      REAL :: WC
!
!-----------------------------------------------------------------------
!**********************************************************************
!-----------------------------------------------------------------------
!
!HWRF      MY_GROWTH(MY_T1:MY_T2)=MP_RESTART_STATE(MY_T1:MY_T2)
!HWRF!
!HWRF      C1XPVS0=MP_RESTART_STATE(MY_T2+1)
!HWRF      C2XPVS0=MP_RESTART_STATE(MY_T2+2)
!HWRF      C1XPVS =MP_RESTART_STATE(MY_T2+3)
!HWRF      C2XPVS =MP_RESTART_STATE(MY_T2+4)
!HWRF      CIACW  =MP_RESTART_STATE(MY_T2+5)
!HWRF      CIACR  =MP_RESTART_STATE(MY_T2+6)
!HWRF      CRACW  =MP_RESTART_STATE(MY_T2+7)
!HWRF      CRAUT  =MP_RESTART_STATE(MY_T2+8)
!HWRF!
!HWRF      TBPVS(1:NX) =TBPVS_STATE(1:NX)
!HWRF      TBPVS0(1:NX)=TBPVS0_STATE(1:NX)
!
!----------
!2015-03-30, recalculate some constants which may depend on phy time step
          CALL MY_GROWTH_RATES (DT)

!--- CIACW is used in calculating riming rates
!      The assumed effective collection efficiency of cloud water rimed onto
!      ice is =0.5 below:
!
        CIACW=DT*0.25*PI_E*0.5*(1.E5)**C1
!
!--- CIACR is used in calculating freezing of rain colliding with large ice
!      The assumed collection efficiency is 1.0
!
        CIACR=PI_E*DT
!
!--- CRACW is used in calculating collection of cloud water by rain (an
!      assumed collection efficiency of 1.0)
!
        CRACW=DT*0.25*PI_E*1.0
!
!-- See comments in subroutine etanewhr_init starting with variable RDIS=
!
        BRAUT=DT*1.1E10*BETA6/NCW

!       write(*,*)'dt=',dt
!       write(*,*)'pi_e=',pi_e
!       write(*,*)'ciacw=',ciacw 
!       write(*,*)'ciacr=',ciacr 
!       write(*,*)'cracw=',cracw 
!       write(*,*)'araut=',araut
!       write(*,*)'braut=',braut
!! END OF adding, 2015-03-30
!-----------

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
        t_phy(i,k,j) = th_phy(i,k,j)*pi_phy(i,k,j)
        qv(i,k,j)=qv(i,k,j)/(1.+qv(i,k,j)) !Convert to specific humidity
      ENDDO
      ENDDO
      ENDDO

! initial data assimilation vars (will need to delete this part in the future)

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
	 TLATGS_PHY (i,k,j)=0.
	 TRAIN_PHY  (i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,jte
      DO i = its,ite
         ACPREC(i,j)=0.
         APREC (i,j)=0.
         PREC  (i,j)=0.
         SR    (i,j)=0.
      ENDDO
      ENDDO

!-- 6/11/2010: Update QT, F_ice, F_rain arrays

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
         QT(I,K,J)=QC(I,K,J)+QR(I,K,J)+QI(I,K,J)
         IF (QI(I,K,J) <= EPSQ) THEN
            F_ICE_PHY(I,K,J)=0.
            F_RIMEF_PHY(I,K,J)=1.
            IF (T_PHY(I,K,J) < T_ICEK) F_ICE_PHY(I,K,J)=1.
         ELSE
            F_ICE_PHY(I,K,J)=MAX( 0., MIN(1., QI(I,K,J)/QT(I,K,J) ) )
            F_RIMEF_PHY(I,K,J)=QRIMEF(I,K,J)/QI(I,K,J)
         ENDIF
         IF (QR(I,K,J) <= EPSQ) THEN
            F_RAIN_PHY(I,K,J)=0.
         ELSE
            F_RAIN_PHY(I,K,J)=QR(I,K,J)/(QR(I,K,J)+QC(I,K,J))
         ENDIF
      ENDDO
      ENDDO
      ENDDO

!-----------------------------------------------------------------------

      CALL EGCP01DRV(GID,DT,LOWLYR,                                     &
     &               APREC,PREC,ACPREC,SR,                              &
     &               dz8w,rho_phy,qt,t_phy,qv,F_ICE_PHY,P_PHY,          &
     &               F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY,TRAIN_PHY,       &
     &               ids,ide, jds,jde, kds,kde,		                &
     &               ims,ime, jms,jme, kms,kme,		                &
     &               its,ite, jts,jte, kts,kte		          )
!-----------------------------------------------------------------------

     DO j = jts,jte
        DO k = kts,kte
	DO i = its,ite
  	   th_phy(i,k,j) = t_phy(i,k,j)/pi_phy(i,k,j)
           qv(i,k,j)=qv(i,k,j)/(1.-qv(i,k,j))  !Convert to mixing ratio
           WC=qt(I,K,J)
           QI(I,K,J)=0.
           QR(I,K,J)=0.
           QC(I,K,J)=0.
           IF(F_ICE_PHY(I,K,J)>=1.)THEN
             QI(I,K,J)=WC
           ELSEIF(F_ICE_PHY(I,K,J)<=0.)THEN
             QC(I,K,J)=WC
           ELSE
             QI(I,K,J)=F_ICE_PHY(I,K,J)*WC
             QC(I,K,J)=WC-QI(I,K,J)
           ENDIF
!
           IF(QC(I,K,J)>0..AND.F_RAIN_PHY(I,K,J)>0.)THEN
             IF(F_RAIN_PHY(I,K,J).GE.1.)THEN
               QR(I,K,J)=QC(I,K,J)
               QC(I,K,J)=0.
             ELSE
               QR(I,K,J)=F_RAIN_PHY(I,K,J)*QC(I,K,J)
               QC(I,K,J)=QC(I,K,J)-QR(I,K,J)
             ENDIF
          endif
          QRIMEF(I,K,J)=QI(I,K,J)*F_RIMEF_PHY(I,K,J)
	ENDDO
        ENDDO
     ENDDO
! 
! update rain (from m to mm)

       DO j=jts,jte
       DO i=its,ite
          RAINNC(i,j)=APREC(i,j)*1000.+RAINNC(i,j)
          RAINNCV(i,j)=APREC(i,j)*1000.
       ENDDO
       ENDDO
!
!HWRF     MP_RESTART_STATE(MY_T1:MY_T2)=MY_GROWTH(MY_T1:MY_T2)
!HWRF     MP_RESTART_STATE(MY_T2+1)=C1XPVS0
!HWRF     MP_RESTART_STATE(MY_T2+2)=C2XPVS0
!HWRF     MP_RESTART_STATE(MY_T2+3)=C1XPVS
!HWRF     MP_RESTART_STATE(MY_T2+4)=C2XPVS
!HWRF     MP_RESTART_STATE(MY_T2+5)=CIACW
!HWRF     MP_RESTART_STATE(MY_T2+6)=CIACR
!HWRF     MP_RESTART_STATE(MY_T2+7)=CRACW
!HWRF     MP_RESTART_STATE(MY_T2+8)=CRAUT
!HWRF!
!HWRF     TBPVS_STATE(1:NX) =TBPVS(1:NX)
!HWRF     TBPVS0_STATE(1:NX)=TBPVS0(1:NX)

!-----------------------------------------------------------------------

  END SUBROUTINE FER_HIRES_ADVECT

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      SUBROUTINE EGCP01DRV(GID,                            & !GID gopal's doing
     &  DTPH,LOWLYR,APREC,PREC,ACPREC,SR,                  &
     &  dz8w,RHO_PHY,CWM_PHY,T_PHY,Q_PHY,F_ICE_PHY,P_PHY,  &
     &  F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY,TRAIN_PHY,       &
     &  ids,ide, jds,jde, kds,kde,                         &
     &  ims,ime, jms,jme, kms,kme,                         &
     &  its,ite, jts,jte, kts,kte)
!-----------------------------------------------------------------------
! DTPH           Physics time step (s)
! CWM_PHY (qt)   Mixing ratio of the total condensate. kg/kg
! Q_PHY          Mixing ratio of water vapor. kg/kg
! F_RAIN_PHY     Fraction of rain. 
! F_ICE_PHY      Fraction of ice.
! F_RIMEF_PHY    Mass ratio of rimed ice (rime factor).
!
!TLATGS_PHY,TRAIN_PHY,APREC,PREC,ACPREC,SR are not directly related the
!micrphysics sechme. Instead, they will be used by Eta precip assimilation.
!
!-----------------------------------------------------------------------
!--- Variables APREC,PREC,ACPREC,SR are calculated for precip assimilation
!    and/or ZHAO's scheme in Eta and are not required by this microphysics 
!    scheme itself.  
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
!     VARIABLES PASSED IN/OUT
      INTEGER,INTENT(IN ) :: ids,ide, jds,jde, kds,kde                  &
     &                      ,ims,ime, jms,jme, kms,kme                  &
     &                      ,its,ite, jts,jte, kts,kte
      INTEGER,INTENT(IN ) :: GID     ! grid%id gopal's doing
      REAL,INTENT(IN) :: DTPH
      INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: LOWLYR
      REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT) ::                  &
     &                         APREC,PREC,ACPREC,SR
      REAL,DIMENSION( its:ite, kts:kte, jts:jte ),INTENT(INOUT) :: t_phy
      REAL,DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN) ::         &
     &                                             dz8w,P_PHY,RHO_PHY
      REAL,DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(INOUT) ::      &
     &   CWM_PHY, F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY           &
     &   ,Q_PHY,TRAIN_PHY
!
!-----------------------------------------------------------------------
!LOCAL VARIABLES
!-----------------------------------------------------------------------
!
!HWRF - Below are directives in the operational code that have been removed,
!       where "TEMP_DEX" has been replaced with "I,J,L" and "TEMP_DIMS" has
!       been replaced with "its:ite,jts:jte,kts:kte"
!HWRF#define CACHE_FRIENDLY_MP_ETANEW
!HWRF#ifdef CACHE_FRIENDLY_MP_ETANEW
!HWRF#  define TEMP_DIMS  kts:kte,its:ite,jts:jte
!HWRF#  define TEMP_DEX   L,I,J
!HWRF#else
!HWRF#  define TEMP_DIMS  its:ite,jts:jte,kts:kte
!HWRF#  define TEMP_DEX   I,J,L
!HWRF#endif
!HWRF!
      INTEGER :: LSFC,I,J,I_index,J_index,L,K,KFLIP
!HWRF      REAL,DIMENSION(TEMP_DIMS) :: CWM,T,Q,TRAIN,TLATGS,P
      REAL,DIMENSION(its:ite,jts:jte,kts:kte) ::                        &
     &   CWM,T,Q,TRAIN,TLATGS,P
      REAL,DIMENSION(kts:kte,its:ite,jts:jte) :: F_ice,F_rain,F_RimeF       
      INTEGER,DIMENSION(its:ite,jts:jte) :: LMH
      REAL :: TC,WC,QI,QR,QW,Fice,Frain,DUM,ASNOW,ARAIN
      REAL,DIMENSION(kts:kte) :: P_col,Q_col,T_col,QV_col,WC_col,       &
     & RimeF_col,QI_col,QR_col,QW_col, THICK_col, RHC_col, DPCOL,       &
     & pcond1d,pidep1d,                                                 &
     & piacw1d,piacwi1d,piacwr1d,piacr1d,picnd1d,pievp1d,pimlt1d,       &
     & praut1d,pracw1d,prevp1d,pisub1d,pevap1d, DBZ_col,NR_col,NS_col,  &
     & vsnow1d,vrain11d,vrain21d,vci1d,NSmICE1d,INDEXS1d,INDEXR1d,      & !jul28
     & RFlag1d   !jun01
      REAL,DIMENSION(2) :: PRECtot,PRECmax
!-----------------------------------------------------------------------
!
        DO J=JTS,JTE    
        DO I=ITS,ITE  
           LMH(I,J) = KTE-LOWLYR(I,J)+1
        ENDDO
        ENDDO


        DO 98  J=JTS,JTE    
        DO 98  I=ITS,ITE  
           DO L=KTS,KTE
             KFLIP=KTE+1-L
             CWM(I,J,L)=CWM_PHY(I,KFLIP,J)
             T(I,J,L)=T_PHY(I,KFLIP,J)
             Q(I,J,L)=Q_PHY(I,KFLIP,J)
             P(I,J,L)=P_PHY(I,KFLIP,J)
             TLATGS(I,J,L)=TLATGS_PHY(I,KFLIP,J)
             TRAIN(I,J,L)=TRAIN_PHY(I,KFLIP,J)
             F_ice(L,I,J)=F_ice_PHY(I,KFLIP,J)
             F_rain(L,I,J)=F_rain_PHY(I,KFLIP,J)
             F_RimeF(L,I,J)=F_RimeF_PHY(I,KFLIP,J)
           ENDDO
98      CONTINUE
     
       DO 100 J=JTS,JTE    
        DO 100 I=ITS,ITE  
          LSFC=LMH(I,J)                      ! "L" of surface
!
          DO K=KTS,KTE
            KFLIP=KTE+1-K
            DPCOL(K)=RHO_PHY(I,KFLIP,J)*GRAV*dz8w(I,KFLIP,J)
          ENDDO
!   
   !
   !--- Initialize column data (1D arrays)
   !
          IF (CWM(I,J,1) .LE. EPSQ) CWM(I,J,1)=EPSQ
          F_ice(1,I,J)=1.
          F_rain(1,I,J)=0.
          F_RimeF(1,I,J)=1.
          DO L=1,LSFC
      !
      !--- Pressure (Pa) = (Psfc-Ptop)*(ETA/ETA_sfc)+Ptop
      !
            P_col(L)=P(I,J,L)
      !
      !--- Layer thickness = RHO*DZ = -DP/G = (Psfc-Ptop)*D_ETA/(G*ETA_sfc)
      !
            THICK_col(L)=DPCOL(L)*RGRAV
            T_col(L)=T(I,J,L)
            TC=T_col(L)-T0C
            QV_col(L)=max(EPSQ, Q(I,J,L))
            IF (CWM(I,J,L) .LE. EPSQ1) THEN
              WC_col(L)=0.
              IF (TC .LT. T_ICE) THEN
                F_ice(L,I,J)=1.
              ELSE
                F_ice(L,I,J)=0.
              ENDIF
              F_rain(L,I,J)=0.
              F_RimeF(L,I,J)=1.
            ELSE
              WC_col(L)=CWM(I,J,L)

!-- Debug 20120111:  TC==TC will fail if NaN
IF (WC_col(L)>QTwarn .AND. P_col(L)<Pwarn .AND. TC==TC) THEN
   WRITE(0,*) 'WARN4: >1 g/kg condensate in stratosphere; I,J,L,TC,P,QT=',   &
              I,J,L,TC,.01*P_col(L),1000.*WC_col(L)
   QTwarn=MAX(WC_col(L),10.*QTwarn)
   Pwarn=MIN(P_col(L),0.5*Pwarn)
ENDIF
!-- TC/=TC will pass if TC is NaN
IF (WARN5 .AND. TC/=TC) THEN
   WRITE(0,*) 'WARN5: NaN temperature; I,J,L,P=',I,J,L,.01*P_col(L)
   WARN5=.FALSE.
ENDIF

            ENDIF
            IF (T_ICE<=-100.) F_ice(L,I,J)=0.  !-- For no ice runs
      !
      !--- Determine composition of condensate in terms of 
      !      cloud water, ice, & rain
      !
            WC=WC_col(L)
            QI=0.
            QR=0.
            QW=0.
            Fice=F_ice(L,I,J)
            Frain=F_rain(L,I,J)
            IF (Fice .GE. 1.) THEN
              QI=WC
            ELSE IF (Fice .LE. 0.) THEN
              QW=WC
            ELSE
              QI=Fice*WC
              QW=WC-QI
            ENDIF
            IF (QW.GT.0. .AND. Frain.GT.0.) THEN
              IF (Frain .GE. 1.) THEN
                QR=QW
                QW=0.
              ELSE
                QR=Frain*QW
                QW=QW-QR
              ENDIF
            ENDIF
            IF (QI .LE. 0.) F_RimeF(L,I,J)=1.
            RimeF_col(L)=F_RimeF(L,I,J)
            QI_col(L)=QI
            QR_col(L)=QR
            QW_col(L)=QW
!GFDL => New.  Added RHC_col to allow for height- and grid-dependent values for
!GFDL          the relative humidity threshold for condensation ("RHgrd")
!6/11/2010 mod - Use lower RHgrd_out threshold for < 850 hPa
!------------------------------------------------------------
            IF(GID .EQ. 1 .AND. P_col(L)<P_RHgrd_out) THEN  ! gopal's doing based on GFDL
              RHC_col(L)=RHgrd_out        
            ELSE
              RHC_col(L)=RHgrd_in       
            ENDIF
!------------------------------------------------------------
          ENDDO
!
!#######################################################################
   !
   !--- Perform the microphysical calculations in this column
   !
          I_index=I
          J_index=J

       CALL EGCP01COLUMN ( ARAIN, ASNOW, DTPH, RHC_col,                 &
     & I_index, J_index, LSFC,                                          &
     & P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col,         &
     & THICK_col, WC_col, KTS,KTE,              pcond1d,pidep1d,        &
     & piacw1d,piacwi1d,piacwr1d,piacr1d,picnd1d,pievp1d,pimlt1d,       &
     & praut1d,pracw1d,prevp1d,pisub1d,pevap1d, DBZ_col,NR_col,NS_col,  &
     & vsnow1d,vrain11d,vrain21d,vci1d,NSmICE1d,INDEXS1d,INDEXR1d,      & !jul28
     & RFlag1d)   !jun01

   !
!#######################################################################
!
   !
   !--- Update storage arrays
   !
          DO L=1,LSFC
            TRAIN(I,J,L)=(T_col(L)-T(I,J,L))/DTPH
            TLATGS(I,J,L)=T_col(L)-T(I,J,L)
            T(I,J,L)=T_col(L)
            Q(I,J,L)=QV_col(L)
            CWM(I,J,L)=WC_col(L)
      !
      !--- REAL*4 array storage
      !
            IF (QI_col(L) .LE. EPSQ) THEN
              F_ice(L,I,J)=0.
              IF (T_col(L) .LT. T_ICEK) F_ice(L,I,J)=1.
              F_RimeF(L,I,J)=1.
            ELSE
              F_ice(L,I,J)=MAX( 0., MIN(1., QI_col(L)/WC_col(L)) )
              F_RimeF(L,I,J)=MAX(1., RimeF_col(L))
            ENDIF
            IF (QR_col(L) .LE. EPSQ) THEN
              DUM=0
            ELSE
              DUM=QR_col(L)/(QR_col(L)+QW_col(L))
            ENDIF
            F_rain(L,I,J)=DUM
      !
          ENDDO
   !
   !--- Update accumulated precipitation statistics
   !
   !--- Surface precipitation statistics; SR is fraction of surface 
   !    precipitation (if >0) associated with snow
   !
        APREC(I,J)=(ARAIN+ASNOW)*RRHOL       ! Accumulated surface precip (depth in m)  !<--- Ying
        PREC(I,J)=PREC(I,J)+APREC(I,J)
        ACPREC(I,J)=ACPREC(I,J)+APREC(I,J)
        IF(APREC(I,J) .LT. 1.E-8) THEN
          SR(I,J)=0.
        ELSE
          SR(I,J)=RRHOL*ASNOW/APREC(I,J)
        ENDIF
!   !
!   !--- Debug statistics 
!   !
!        IF (PRINT_diag) THEN
!          PRECtot(1)=PRECtot(1)+ARAIN
!          PRECtot(2)=PRECtot(2)+ASNOW
!          PRECmax(1)=MAX(PRECmax(1), ARAIN)
!          PRECmax(2)=MAX(PRECmax(2), ASNOW)
!        ENDIF


!#######################################################################
!#######################################################################
!
100   CONTINUE                          ! End "I" & "J" loops
        DO 101 J=JTS,JTE    
        DO 101 I=ITS,ITE  
           DO L=KTS,KTE
              KFLIP=KTE+1-L
             CWM_PHY(I,KFLIP,J)=CWM(I,J,L)
             T_PHY(I,KFLIP,J)=T(I,J,L)
             Q_PHY(I,KFLIP,J)=Q(I,J,L)
             TLATGS_PHY(I,KFLIP,J)=TLATGS(I,J,L)
             TRAIN_PHY(I,KFLIP,J)=TRAIN(I,J,L)
             F_ice_PHY(I,KFLIP,J)=F_ice(L,I,J)
             F_rain_PHY(I,KFLIP,J)=F_rain(L,I,J)
             F_RimeF_PHY(I,KFLIP,J)=F_RimeF(L,I,J)
           ENDDO
101     CONTINUE
!
      END SUBROUTINE EGCP01DRV
!
!-----------------------------------------------------------------------
!
!###############################################################################
! ***** VERSION OF MICROPHYSICS DESIGNED FOR HIGHER RESOLUTION MESO ETA MODEL
!       (1) Represents sedimentation by preserving a portion of the precipitation
!           through top-down integration from cloud-top.  Modified procedure to
!           Zhao and Carr (1997).
!       (2) Microphysical equations are modified to be less sensitive to time
!           steps by use of Clausius-Clapeyron equation to account for changes in
!           saturation mixing ratios in response to latent heating/cooling.  
!       (3) Prevent spurious temperature oscillations across 0C due to 
!           microphysics.
!       (4) Uses lookup tables for: calculating two different ventilation 
!           coefficients in condensation and deposition processes; accretion of
!           cloud water by precipitation; precipitation mass; precipitation rate
!           (and mass-weighted precipitation fall speeds).
!       (5) Assumes temperature-dependent variation in mean diameter of large ice
!           (Houze et al., 1979; Ryan et al., 1996).
!        -> 8/22/01: This relationship has been extended to colder temperatures
!           to parameterize smaller large-ice particles down to mean sizes of MDImin,
!           which is 50 microns reached at -55.9C.
!       (6) Attempts to differentiate growth of large and small ice, mainly for
!           improved transition from thin cirrus to thick, precipitating ice
!           anvils.
!       (7) Top-down integration also attempts to treat mixed-phase processes,
!           allowing a mixture of ice and water.  Based on numerous observational
!           studies, ice growth is based on nucleation at cloud top &
!           subsequent growth by vapor deposition and riming as the ice particles 
!           fall through the cloud.  There are two modes of ice nucleation
!           following Meyers et al. (JAM, 1992):
!            a) Deposition & condensation freezing nucleation - eq. (2.4) when
!               air is supersaturated w/r/t ice
!            b) Contact freezing nucleation - eq. (2.6) in presence of cloud water
!       (8) Depositional growth of newly nucleated ice is calculated for large time
!           steps using Fig. 8 of Miller and Young (JAS, 1979), at 1 deg intervals
!           using their ice crystal masses calculated after 600 s of growth in water
!           saturated conditions.  The growth rates are normalized by time step
!           assuming 3D growth with time**1.5 following eq. (6.3) in Young (1993).
!       (9) Ice precipitation rates can increase due to increase in response to
!           cloud water riming due to (a) increased density & mass of the rimed
!           ice, and (b) increased fall speeds of rimed ice.
!###############################################################################
!###############################################################################
!
      SUBROUTINE EGCP01COLUMN ( ARAIN, ASNOW, DTPH, RHC_col,             &
     & I_index, J_index, LSFC,                                           &
     & P_col, QI_col, QR_col, Q_col, QW_col, RimeF_col, T_col,           &
     & THICK_col, WC_col ,KTS,KTE,pcond1d,pidep1d,                       &
     & piacw1d,piacwi1d,piacwr1d,piacr1d,picnd1d,pievp1d,pimlt1d,        &
     & praut1d,pracw1d,prevp1d,pisub1d,pevap1d, DBZ_col,NR_col,NS_col,   &
     & vsnow1d,vrain11d,vrain21d,vci1d,NSmICE1d,INDEXS1d,INDEXR1d,       &  !jul28
     & RFlag1d)   !jun01
!
!###############################################################################
!###############################################################################
!
!-------------------------------------------------------------------------------
!----- NOTE:  Code is currently set up w/o threading!  
!-------------------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:  Grid-scale microphysical processes - condensation & precipitation
!   PRGRMMR: Ferrier         ORG: W/NP22     DATE: 08-2001
!   PRGRMMR: Jin  (Modification for WRF structure)
!-------------------------------------------------------------------------------
! ABSTRACT:
!   * Merges original GSCOND & PRECPD subroutines.   
!   * Code has been substantially streamlined and restructured.
!   * Exchange between water vapor & small cloud condensate is calculated using
!     the original Asai (1965, J. Japan) algorithm.  See also references to
!     Yau and Austin (1979, JAS), Rutledge and Hobbs (1983, JAS), and Tao et al.
!     (1989, MWR).  This algorithm replaces the Sundqvist et al. (1989, MWR)
!     parameterization.  
!-------------------------------------------------------------------------------
!     
! USAGE: 
!   * CALL EGCP01COLUMN_hr FROM SUBROUTINE EGCP01DRV
!
! INPUT ARGUMENT LIST:
!   DTPH       - physics time step (s)
!   I_index    - I index
!   J_index    - J index
!   LSFC       - Eta level of level above surface, ground
!   P_col      - vertical column of model pressure (Pa)
!   QI_col     - vertical column of model ice mixing ratio (kg/kg)
!   QR_col     - vertical column of model rain ratio (kg/kg)
!   Q_col      - vertical column of model water vapor specific humidity (kg/kg)
!   QW_col     - vertical column of model cloud water mixing ratio (kg/kg)
!   RimeF_col  - vertical column of rime factor for ice in model (ratio, defined below)
!   T_col      - vertical column of model temperature (deg K)
!   THICK_col  - vertical column of model mass thickness (density*height increment)
!   WC_col     - vertical column of model mixing ratio of total condensate (kg/kg)
!   RHC_col    - vertical column of threshold relative humidity for onset of condensation (ratio)   !GFDL
!   
!
! OUTPUT ARGUMENT LIST: 
!   ARAIN      - accumulated rainfall at the surface (kg)
!   ASNOW      - accumulated snowfall at the surface (kg)
!   Q_col      - vertical column of model water vapor specific humidity (kg/kg)
!   WC_col     - vertical column of model mixing ratio of total condensate (kg/kg)
!   QW_col     - vertical column of model cloud water mixing ratio (kg/kg)
!   QI_col     - vertical column of model ice mixing ratio (kg/kg)
!   QR_col     - vertical column of model rain ratio (kg/kg)
!   RimeF_col  - vertical column of rime factor for ice in model (ratio, defined below)
!   T_col      - vertical column of model temperature (deg K)
!   DBZ_col    - vertical column of radar reflectivity (dBZ)
!   NR_col     - vertical column of rain number concentration (m^-3)
!   NS_col     - vertical column of snow number concentration (m^-3)
!     
! OUTPUT FILES:
!     NONE
!     
! Subprograms & Functions called:
!   * Real Function CONDENSE  - cloud water condensation
!   * Real Function DEPOSIT   - ice deposition (not sublimation)
!   * Integer Function GET_INDEXR  - estimate the mean size of raindrops (microns)
!
! UNIQUE: NONE
!  
! LIBRARY: NONE
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!------------------------------------------------------------------------- 
!--------------- Arrays & constants in argument list --------------------- 
!------------------------------------------------------------------------- 
!
      IMPLICIT NONE
!    
      INTEGER,INTENT(IN) :: KTS,KTE,I_index, J_index, LSFC
      REAL,INTENT(IN)    :: DTPH
      REAL,INTENT(INOUT) ::  ARAIN, ASNOW
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: P_col,QI_col,QR_col,RHC_col  &
     & ,Q_col ,QW_col, RimeF_col, T_col, THICK_col,WC_col,pcond1d       &
     & ,pidep1d,piacw1d,piacwi1d,piacwr1d,piacr1d,picnd1d,pievp1d       &
     & ,pimlt1d,praut1d,pracw1d,prevp1d,pisub1d,pevap1d,DBZ_col,NR_col  &
     & ,NS_col,vsnow1d,vrain11d,vrain21d,vci1d,NSmICE1d,INDEXS1d        &  !jun01
     & ,INDEXR1d,RFlag1d    !jun01
!
!--------------------------------------------------------------------------------
!--- The following arrays are integral calculations based on the mean 
!    snow/graupel diameters, which vary from 50 microns to 1000 microns 
!    (1 mm) at 1-micron intervals and assume exponential size distributions.
!    The values are normalized and require being multipled by the number 
!    concentration of large ice (NLICE).
!---------------------------------------
!    - VENTI1 - integrated quantity associated w/ ventilation effects 
!               (capacitance only) for calculating vapor deposition onto ice
!    - VENTI2 - integrated quantity associated w/ ventilation effects 
!               (with fall speed) for calculating vapor deposition onto ice
!    - ACCRI  - integrated quantity associated w/ cloud water collection by ice
!    - MASSI  - integrated quantity associated w/ ice mass 
!    - VSNOWI - mass-weighted fall speed of snow (large ice), used to calculate 
!               precipitation rates
!    - VEL_RF - velocity increase of rimed particles as functions of crude
!               particle size categories (at 0.1 mm intervals of mean ice particle
!               sizes) and rime factor (different values of Rime Factor of 1.1**N, 
!               where N=0 to Nrime).
!--------------------------------------------------------------------------------
!--- The following arrays are integral calculations based on the mean 
!    rain diameters, which vary from 50 microns to 1000 microns 
!    (1 mm) at 1-micron intervals and assume exponential size distributions.
!    The values are normalized and require being multiplied by the rain intercept
!    (N0r).
!---------------------------------------
!    - VENTR1 - integrated quantity associated w/ ventilation effects 
!               (capacitance only) for calculating evaporation from rain
!    - VENTR2 - integrated quantity associated w/ ventilation effects 
!               (with fall speed) for calculating evaporation from rain
!    - ACCRR  - integrated quantity associated w/ cloud water collection by rain
!    - MASSR  - integrated quantity associated w/ rain
!    - VRAIN  - mass-weighted fall speed of rain, used to calculate 
!               precipitation rates
!    - RRATE  - precipitation rates, which should also be equal to RHO*QR*VRAIN
!
!------------------------------------------------------------------------- 
!------- Key parameters, local variables, & important comments ---------
!-----------------------------------------------------------------------
!
!--- TOLER => Tolerance or precision for accumulated precipitation 
!
      REAL, PARAMETER :: TOLER=5.E-7, C2=1./6., RHO0=1.194,             &
                         Xratio=.025, Zmin=0.01, DBZmin=-20.
!
!--- If BLEND=1:
!      precipitation (large) ice amounts are estimated at each level as a 
!      blend of ice falling from the grid point above and the precip ice
!      present at the start of the time step (see TOT_ICE below).
!--- If BLEND=0:
!      precipitation (large) ice amounts are estimated to be the precip
!      ice present at the start of the time step.
!
!--- Extended to include sedimentation of rain on 2/5/01 
!
      REAL, PARAMETER :: BLEND=1.
!
!--- This variable is for debugging purposes (if .true.)
!
      LOGICAL, PARAMETER :: PRINT_diag=.TRUE.
!
!-----------------------------------------------------------------------
!--- Local variables
!-----------------------------------------------------------------------
!
      REAL :: EMAIRI, N0r, NLICE, NSmICE, NInuclei, Nrain, Nsnow, Nmix
      LOGICAL :: CLEAR, ICE_logical, DBG_logical, RAIN_logical,         &
                 STRAT, DRZL
      INTEGER :: INDEX_MY,INDEXR,INDEXR1,INDEXR2,INDEXS,IPASS,ITDX,IXRF,&
     &           IXS,LBEF,L,INDEXRmin,INDEXS0C,IDR        !mar03  !may20
!
!
      REAL :: ABI,ABW,AIEVP,ARAINnew,ASNOWnew,BLDTRH,BUDGET,            &
     &        CREVP,DELI,DELR,DELT,DELV,DELW,DENOMF,                    &
     &        DENOMI,DENOMW,DENOMWI,DIDEP,                              &
     &        DIEVP,DIFFUS,DLI,DTRHO,DUM,DUM1,DUM2,DUM3,                &
     &        DWV0,DWVI,DWVR,DYNVIS,ESI,ESW,FIR,FLIMASS,                &
     &        FWR,FWS,GAMMAR,GAMMAS,                                    &
     &        PCOND,PIACR,PIACW,PIACWI,PIACWR,PICND,PIDEP,PIDEP_max,    &
     &        PIEVP,PILOSS,PIMLT,PINIT,PP,PRACW,PRAUT,PREVP,PRLOSS,     &
     &        QI,QInew,QLICE,QR,QRnew,QSI,QSIgrd,QSInew,QSW,QSW0,       &
     &        QSWgrd,QSWnew,QT,QTICE,QTnew,QTRAIN,Q,QW,QWnew,Rcw,       &
     &        RFACTOR,RFmx,RFrime,RHO,RIMEF,RIMEF1,RQR,RR,RRHO,SFACTOR, &
     &        TC,TCC,TFACTOR,THERM_COND,THICK,TK,TK2,TNEW,              &
     &        TOT_ICE,TOT_ICEnew,TOT_RAIN,TOT_RAINnew,                  &
     &        VEL_INC,VENTR,VENTIL,VENTIS,VRAIN1,VRAIN2,VRIMEF,VSNOW,   &
     &        VSNOW1,WC,WCnew,WSgrd,WS,WSnew,WV,WVnew,                  &
     &        XLI,XLIMASS,XRF, RHgrd,                                   &
     &        NSImax,QRdum,QSmICE,QLgIce,RQLICE,VCI,TIMLT,              &
     &        RQSnew,RQRnew,Zrain,Zsnow,Ztot,RHOX0C,RFnew,PSDEP,DELS     !mar03  !apr22
      REAL, SAVE :: Revised_LICE=1.e-3    !-- kg/m**3
!
!#######################################################################
!########################## Begin Execution ############################
!#######################################################################
!
!
      ARAIN=0.                ! Accumulated rainfall into grid box from above (kg/m**2)
      VRAIN1=0.               ! Rain fall speeds into grib box from above (m/s)
      VSNOW1=0.               ! Ice fall speeds into grib box from above (m/s)
      ASNOW=0.                ! Accumulated snowfall into grid box from above (kg/m**2)
      INDEXS0C=MDImin         ! Mean snow/graupel diameter just above (<0C) freezing level (height)
      RHOX0C=22.5             ! Estimated ice density at 0C (kg m^-3)  !mar03
      TIMLT=0.                ! Total ice melting in a layer (drizzle detection)
      STRAT=.FALSE.           ! Stratiform rain DSD below melting level    !may11
      DRZL=.FALSE.            ! Drizzle DSD below melting level    !may23
!
!-----------------------------------------------------------------------
!------------ Loop from top (L=1) to surface (L=LSFC) ------------------
!-----------------------------------------------------------------------
!
big_loop: DO L=1,LSFC
        pcond1d(L)=0.
        pidep1d(L)=0.
        piacw1d(L)=0.
        piacwi1d(L)=0.
        piacwr1d(L)=0.
        piacr1d(L)=0.
        picnd1d(L)=0.
        pievp1d(L)=0.
        pimlt1d(L)=0.
        praut1d(L)=0.
        pracw1d(L)=0.
        prevp1d(L)=0.
        pisub1d(L)=0.
        pevap1d(L)=0.
        vsnow1d(L)=0.
        vrain11d(L)=0.
        vrain21d(L)=0.
        vci1d(L)=0.
        NSmICE1d(L)=0.
        DBZ_col(L)=DBZmin
        NR_col(L)=0.
        NS_col(L)=0.
        INDEXR1d(L)=0.
        INDEXS1d(L)=0.
        RFlag1d(L)=0.   !jun01
!
!--- Skip this level and go to the next lower level if no condensate 
!      and very low specific humidities
!
!--- Check if any rain is falling into layer from above
!
        IF (ARAIN .GT. CLIMIT) THEN
          CLEAR=.FALSE.
          VRAIN1=0.
        ELSE
          CLEAR=.TRUE.
          ARAIN=0.
        ENDIF
!
!--- Check if any ice is falling into layer from above
!
!--- NOTE that "SNOW" in variable names is often synonomous with 
!    large, precipitation ice particles
!
        IF (ASNOW .GT. CLIMIT) THEN
          CLEAR=.FALSE.
          VSNOW1=0.
        ELSE
          ASNOW=0.
        ENDIF
!
!-----------------------------------------------------------------------
!------------ Proceed with cloud microphysics calculations -------------
!-----------------------------------------------------------------------
!
        TK=T_col(L)         ! Temperature (deg K)
        TC=TK-T0C           ! Temperature (deg C)
        PP=P_col(L)         ! Pressure (Pa)
        Q=Q_col(L)          ! Specific humidity of water vapor (kg/kg)
        WV=Q/(1.-Q)         ! Water vapor mixing ratio (kg/kg)
        WC=WC_col(L)        ! Grid-scale mixing ratio of total condensate (water or ice; kg/kg)
        RHgrd=RHC_col(L)    ! Threshold relative humidity for the onset of condensation
!
!-----------------------------------------------------------------------
!--- Moisture variables below are mixing ratios & not specifc humidities
!-----------------------------------------------------------------------
!    
!--- This check is to determine grid-scale saturation when no condensate is present
!    
        ESW=MIN(1000.*FPVS0(TK),0.99*PP) ! Saturation vapor pressure w/r/t water
        QSW=EPS*ESW/(PP-ESW)             ! Saturation mixing ratio w/r/t water
        WS=QSW                           ! General saturation mixing ratio (water/ice)
        QSI=QSW                          ! Saturation mixing ratio w/r/t ice
        IF (TC .LT. 0.) THEN
          ESI=MIN(1000.*FPVS(TK),0.99*PP)  ! Saturation vapor pressure w/r/t ice
          QSI=EPS*ESI/(PP-ESI)           ! Saturation mixing ratio w/r/t water
          WS=QSI                         ! General saturation mixing ratio (water/ice)
        ENDIF
!
!--- Effective grid-scale Saturation mixing ratios
!
        QSWgrd=RHgrd*QSW
        QSIgrd=RHgrd*QSI
        WSgrd=RHgrd*WS
!
!--- Check if air is subsaturated and w/o condensate
!
        IF (WV.GT.WSgrd .OR. WC.GT.EPSQ) CLEAR=.FALSE.
!
!-----------------------------------------------------------------------
!-- Loop to the end if in clear, subsaturated air free of condensate ---
!-----------------------------------------------------------------------
!
        IF (CLEAR) THEN
          STRAT=.FALSE.     !- Reset stratiform rain flag
          DRZL=.FALSE.      !- Reset drizzle flag
          INDEXRmin=MDRmin  !- Reset INDEXRmin
          TIMLT=0.          !- Reset accumulated ice melting
          CYCLE big_loop
        ENDIF
!
!-----------------------------------------------------------------------
!--------- Initialize RHO, THICK & microphysical processes -------------
!-----------------------------------------------------------------------
!
!
!--- Virtual temperature, TV=T*(1./EPS-1)*Q, Q is specific humidity;
!    (see pp. 63-65 in Fleagle & Businger, 1963)
!
          RHO=PP/(RD*TK*(1.+EPS1*Q)) ! Air density (kg/m**3)
          RRHO=1./RHO                ! Reciprocal of air density
          DTRHO=DTPH*RHO             ! Time step * air density
          BLDTRH=BLEND*DTRHO         ! Blend parameter * time step * air density
          THICK=THICK_col(L)         ! Layer thickness = RHO*DZ = -DP/G = (Psfc-Ptop)*D_ETA/(G*ETA_sfc)
!
          ARAINnew=0.                ! Updated accumulated rainfall
          ASNOWnew=0.                ! Updated accumulated snowfall
          QI=QI_col(L)               ! Ice mixing ratio
          QInew=0.                   ! Updated ice mixing ratio
          QR=QR_col(L)               ! Rain mixing ratio
          QRnew=0.                   ! Updated rain ratio
          QW=QW_col(L)               ! Cloud water mixing ratio
          QWnew=0.                   ! Updated cloud water ratio
!
          PCOND=0.                   ! Condensation (>0) or evaporation (<0) of cloud water (kg/kg)
          PIDEP=0.                   ! Deposition (>0) or sublimation (<0) of ice crystals (kg/kg)
          PINIT=0.                   ! Ice initiation (part of PIDEP calculation, kg/kg)
          PIACW=0.                   ! Cloud water collection (riming) by precipitation ice (kg/kg; >0)
          PIACWI=0.                  ! Growth of precip ice by riming (kg/kg; >0)
          PIACWR=0.                  ! Shedding of accreted cloud water to form rain (kg/kg; >0)
          PIACR=0.                   ! Freezing of rain onto large ice at supercooled temps (kg/kg; >0)
          PICND=0.                   ! Condensation (>0) onto wet, melting ice (kg/kg)
          PIEVP=0.                   ! Evaporation (<0) from wet, melting ice (kg/kg)
          PIMLT=0.                   ! Melting ice (kg/kg; >0)
          PRAUT=0.                   ! Cloud water autoconversion to rain (kg/kg; >0)
          PRACW=0.                   ! Cloud water collection (accretion) by rain (kg/kg; >0)
          PREVP=0.                   ! Rain evaporation (kg/kg; <0)
          NSmICE=0.                  ! Cloud ice number concentration (m^-3)
          Nrain=0.                   ! Rain number concentration (m^-3)   !jul28 begin
          Nsnow=0.                   ! "Snow" number concentration (m^-3)
          RQRnew=0.                  ! Final rain content (kg/m**3)
          RQSnew=0.                  ! Final "snow" content (kg/m**3)
          Zrain=0.                   ! Radar reflectivity from rain (mm**6 m-3)
          Zsnow=0.                   ! Radar reflectivity from snow (mm**6 m-3)
          Ztot=0.                    ! Radar reflectivity from rain+snow (mm**6 m-3)
          INDEXR=MDRmin              ! Mean diameter of rain (microns)
          INDEXR1=INDEXR             ! 1st updated mean diameter of rain (microns)
          INDEXR2=INDEXR             ! 2nd updated mean diameter of rain (microns)
          N0r=0.                     ! 1st estimate for rain intercept (m^-4)
          DUM1=MIN(0.,TC)
          DUM=XMImax*EXP(XMIexp*DUM1)
          INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )   ! 1st estimate for mean diameter of snow (microns)
          VCI=0.                     ! Cloud ice fall speeds (m/s)
          VSNOW=0.                   ! "Snow" (snow/graupel/sleet/hail) fall speeds (m/s)
          VRAIN2=0.                  ! Rain fall speeds out of bottom of grid box (m/s)
          RimeF1=1.                  ! Rime Factor (ratio, >=1, defined below)
!
!--- Double check input hydrometeor mixing ratios
!
!          DUM=WC-(QI+QW+QR)
!          DUM1=ABS(DUM)
!          DUM2=TOLER*MIN(WC, QI+QW+QR)
!          IF (DUM1 .GT. DUM2) THEN
!            WRITE(0,"(/2(a,i4),a,i2)") '{@ i=',I_index,' j=',J_index,
!     &                                 ' L=',L
!            WRITE(0,"(4(a12,g11.4,1x))") 
!     & '{@ TCold=',TC,'P=',.01*PP,'DIFF=',DUM,'WCold=',WC,
!     & '{@ QIold=',QI,'QWold=',QW,'QRold=',QR
!          ENDIF
!
!***********************************************************************
!*********** MAIN MICROPHYSICS CALCULATIONS NOW FOLLOW! ****************
!***********************************************************************
!
!--- Calculate a few variables, which are used more than once below
!
!--- Latent heat of vaporization as a function of temperature from
!      Bolton (1980, JAS)
!
          TK2=1./(TK*TK)             ! 1./TK**2
!
!--- Basic thermodynamic quantities
!      * DYNVIS - dynamic viscosity  [ kg/(m*s) ]
!      * THERM_COND - thermal conductivity  [ J/(m*s*K) ]
!      * DIFFUS - diffusivity of water vapor  [ m**2/s ]
!
          TFACTOR=SQRT(TK*TK*TK)/(TK+120.)
          DYNVIS=1.496E-6*TFACTOR
          THERM_COND=2.116E-3*TFACTOR
          DIFFUS=8.794E-5*TK**1.81/PP
!
!--- Air resistance term for the fall speed of ice following the
!      basic research by Heymsfield, Kajikawa, others 
!
          GAMMAS=MIN(1.5, (1.E5/PP)**C1)    !-- limited to 1.5x
!
!--- Air resistance for rain fall speed (Beard, 1985, JAS, p.470)
!
          GAMMAR=(RHO0/RHO)**.4
!
!----------------------------------------------------------------------
!-------------  IMPORTANT MICROPHYSICS DECISION TREE  -----------------
!----------------------------------------------------------------------
!
!--- Determine if conditions supporting ice are present
!
          IF (TC.LT.0. .OR. QI.GT. EPSQ .OR. ASNOW.GT.CLIMIT) THEN
            ICE_logical=.TRUE.
          ELSE
            ICE_logical=.FALSE.
            QLICE=0.
            QTICE=0.
          ENDIF
          IF (T_ICE <= -100.) THEN
            ICE_logical=.FALSE.
            QLICE=0.
            QTICE=0.
          ENDIF
!
!--- Determine if rain is present
!
          RAIN_logical=.FALSE.
          IF (ARAIN.GT.CLIMIT .OR. QR.GT.EPSQ) RAIN_logical=.TRUE.
!
ice_test: IF (ICE_logical) THEN
!
!--- IMPORTANT:  Estimate time-averaged properties.
!
!---
!  ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!  * QSmICE  - estimated mixing ratio for small cloud ice
!---
!  * TOT_ICE - total mass (small & large) ice before microphysics,
!              which is the sum of the total mass of large ice in the 
!              current layer and the input flux of ice from above
!  * PILOSS  - greatest loss (<0) of total (small & large) ice by
!              sublimation, removing all of the ice falling from above
!              and the ice within the layer
!  * RimeF1  - Rime Factor, which is the mass ratio of total (unrimed & rimed) 
!              ice mass to the unrimed ice mass (>=1)
!  * VrimeF  - the velocity increase due to rime factor or melting (ratio, >=1)
!  * VSNOW   - Fall speed of rimed snow w/ air resistance correction
!  * VCI     - Fall speed of 50-micron ice crystals w/ air resistance correction
!  * EMAIRI  - equivalent mass of air associated layer and with fall of snow into layer
!  * XLIMASS - used for debugging, associated with calculating large ice mixing ratio
!  * FLIMASS - mass fraction of large ice
!  * QTICE   - time-averaged mixing ratio of total ice
!  * QLICE   - time-averaged mixing ratio of large ice
!  * NLICE   - time-averaged number concentration of large ice
!  * NSmICE  - number concentration of small ice crystals at current level
!  * QSmICE  - mixing ratio of small ice crystals at current level
!---
!--- Assumed number fraction of large ice particles to total (large & small) 
!    ice particles, which is based on a general impression of the literature.
!
            NInuclei=0.
            NSmICE=0.
            QSmICE=0. 
            Rcw=0.
            IF (TC<0.) THEN
!
!--- Max # conc of small ice crystals based on 10% of total ice content
!    or the parameter NSI_max
!
              NSImax=MAX(NSI_max, 0.1*RHO*QI/MASSI(MDImin) )  !aug27
!
!-- Specify Fletcher, Cooper, Meyers, etc. here for ice nuclei concentrations
!   Cooper (1986):   NInuclei=MIN(5.*EXP(-0.304*TC), NSImax)
!   Fletcher (1962): NInuclei=MIN(0.01*EXP(-0.6*TC), NSImax)
!
!aug28: The formulas below mean that Fletcher is used for >-21C and Cooper at colder
!       temperatures. In areas of high ice contents near the tops of deep convection,
!       the number concentrations will be determined by the lower value of the "FQi" 
!       contribution to NSImax or Cooper.
!
              NInuclei=MIN(0.01*EXP(-0.6*TC), NSImax)         !aug28 - Fletcher (1962)
              NInuclei=MIN(5.*EXP(-0.304*TC), NInuclei)       !aug28 - Cooper (1984)
              IF (QI>EPSQ) THEN
                DUM=RRHO*MASSI(MDImin)
                NSmICE=MIN(NInuclei, QI/DUM)
                QSmICE=NSmICE*DUM
              ENDIF         ! End IF (QI>EPSQ)
            ENDIF            ! End IF (TC<0.)
  init_ice: IF (QI<=EPSQ .AND. ASNOW<=CLIMIT) THEN
              TOT_ICE=0.
              PILOSS=0.
              RimeF1=1.
              VrimeF=1.
              VEL_INC=GAMMAS
              VSNOW=0.
              VSNOW1=0.
              VCI=0.
              EMAIRI=THICK
              XLIMASS=RimeF1*MASSI(INDEXS)
              FLIMASS=1.
              QLICE=0.
              RQLICE=0.
              QTICE=0.
              NLICE=0.
            ELSE  init_ice
   !
   !--- For T<0C mean particle size follows Houze et al. (JAS, 1979, p. 160), 
   !    converted from Fig. 5 plot of LAMDAs.  Similar set of relationships 
   !    also shown in Fig. 8 of Ryan (BAMS, 1996, p. 66).
   !
!
!sep10 - Start of changes described in (23) at top of code.
!
              TOT_ICE=THICK*QI+BLEND*ASNOW
              PILOSS=-TOT_ICE/THICK
              QLgICE=MAX(0., QI-QSmICE)         !-- 1st-guess estimate of large ice
              VCI=GAMMAS*VSNOWI(MDImin)
!
!-- Need to save this original value before two_pass iteration
!
              LBEF=MAX(1,L-1)
              RimeF1=(RimeF_col(L)*THICK*QLgICE                         &
     &                +RimeF_col(LBEF)*BLEND*ASNOW)/TOT_ICE
!
!mar08 see (32); !apr22a see (41) start - Estimate mean diameter (INDEXS in microns)
              IF (RimeF1>2.) THEN
                DUM3=RH_NgC*(RHO*QLgICE)**C1         !- convective mean diameter
                DUM2=RH_NgT*(RHO*QLgICE)**C1         !- transition mean diameter
                IF (RimeF1>=10.) THEN
                  DUM=DUM3
                ELSE IF (RimeF1>=5.) THEN
                  DUM=0.2*(RimeF1-5.)                !- Blend at 5<=RF<10
                  DUM=DUM3*DUM+DUM2*(1.-DUM)
                ELSE
                  DUM1=REAL(INDEXS)                  !- stratiform mean diameter
                  DUM=0.33333*(RimeF1-2.)            !- Blend at 2<RF<5
                  DUM=DUM2*DUM+DUM1*(1.-DUM)
                ENDIF
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
              ENDIF
!may11 - begin
              EMAIRI=THICK+BLDTRH*VSNOW1
              QLICE=(THICK*QLgICE+BLEND*ASNOW)/EMAIRI   !- Estimate of large ice
!may17 - start; now calculated before the DO IPASS iteration
              RQLICE=RHO*QLICE
              QTICE=QLICE+QSmICE
              FLIMASS=QLICE/QTICE
!may17 end
!may11 - end
!mar08 !apr22a end
!
!===========================================
    two_pass: DO IPASS=1,2
!===========================================
!
!-- Prevent rime factor (RimeF1) from exceeding a maximum value, RFmx, in which 
!   the ice has an equivalent density near that of pure ice
!
                DUM=1.E-6*REAL(INDEXS)          !- Mean diameter in m
                RFmx=RFmx1*DUM*DUM*DUM/MASSI(INDEXS)
                RimeF1=MIN(RimeF1, RFmx)
!
    vel_rime:   IF (RimeF1<=1.) THEN
                  RimeF1=1.
                  VrimeF=1.
                ELSE   vel_rime
  !--- Prevent rime factor (RimeF1) from exceeding a maximum value (RFmax)
                  RimeF1=MIN(RimeF1, RFmax)
                  IXS=MAX(2, MIN(INDEXS/100, 9))
                  XRF=10.492*ALOG(RimeF1)
                  IXRF=MAX(0, MIN(INT(XRF), Nrime))
                  IF (IXRF .GE. Nrime) THEN
                    VrimeF=VEL_RF(IXS,Nrime)
                  ELSE
                    VrimeF=VEL_RF(IXS,IXRF)+(XRF-FLOAT(IXRF))*            &
       &                   (VEL_RF(IXS,IXRF+1)-VEL_RF(IXS,IXRF))
                  ENDIF
                  VrimeF=MAX(1., VrimeF)
                ENDIF   vel_rime
!
!may17 - start
                VEL_INC=GAMMAS*VrimeF           !- Normal rimed ice fall speeds
                VSNOW=VEL_INC*VSNOWI(INDEXS)
                IF (RimeF1>=5. .AND. INDEXS==MDImax .AND. RQLICE>RQhail) THEN
!- Additional increase using Thompson graupel/hail fall speeds
                  DUM=GAMMAS*AVhail*RQLICE**BVhail
                  IF (DUM>VSNOW) THEN
                    VSNOW=DUM
                    VEL_INC=VSNOW/VSNOWI(INDEXS)
                  ENDIF
                ENDIF
                XLIMASS=RimeF1*MASSI(INDEXS)
                NLICE=RQLICE/XLIMASS
!
!sep16 - End of change described in (26) at top of code.
!
!===========================================
                IF (IPASS>=2 .OR.                                       &
                    (NLICE>=NLImin .AND. NLICE<=NSI_max)) EXIT two_pass
!may17 - end
!===========================================
!
!--- Force NLICE to be between NLImin and NSI_max when IPASS=1
!
!                IF (PRINT_diag .AND. RQLICE>Revised_LICE) DUM2=NLICE     !-- For debugging (see DUM2 below)
                NLICE=MAX(NLImin, MIN(NSI_max, NLICE) )
!sep16 - End of changes
!
                XLI=RQLICE/(NLICE*RimeF1)         !- Mean mass of unrimed ice
new_size:       IF (XLI<=MASSI(MDImin) ) THEN
                  INDEXS=MDImin
                ELSE IF (XLI<=MASSI(450) ) THEN   new_size
                  DLI=9.5885E5*XLI**.42066         ! DLI in microns
                  INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
                ELSE IF (XLI<MASSI(MDImax) ) THEN   new_size
                  DLI=3.9751E6*XLI**.49870         ! DLI in microns
                  INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
                ELSE  new_size
                  INDEXS=MDImax
!                  IF (PRINT_diag .AND. RQLICE>Revised_LICE) THEN
!                    WRITE(0,"(5(a12,g11.4,1x))") '{$ RimeF1=',RimeF1,   &
!     &                ' RHO*QLICE=',RQLICE,' TC=',TC,' NLICE=',NLICE,   &
!     &                ' NLICEold=',DUM2
!                    Revised_LICE=1.2*RQLICE
!                  ENDIF
                ENDIF    new_size
!===========================================
              ENDDO      two_pass
!===========================================
            ENDIF        init_ice
          ENDIF          ice_test
!
!mar03 !may11 !jun01 - start; see (34) above
          IF(TC<=0.) THEN
            STRAT=.FALSE.
            INDEXRmin=MDRmin
            TIMLT=0.
            INDEXS0C=INDEXS
            RHOX0C=22.5*MAX(1.,MIN(RimeF1,40.))     !- Estimated ice density at 0C (kg m^-3)
          ELSE   ! TC>0.
            IF(.NOT.RAIN_logical) THEN
              STRAT=.FALSE.     !- Reset STRAT
              INDEXRmin=MDRmin  !- Reset INDEXRmin
              IF(.NOT.ICE_logical) TIMLT=0.
            ELSE
!- STRAT=T for stratiform rain
              IF(TIMLT>EPSQ .AND. RHOX0C<=225.) THEN
                STRAT=.TRUE.
              ELSE
                STRAT=.FALSE.     !- Reset STRAT
                INDEXRmin=MDRmin  !- Reset INDEXRmin
              ENDIF
              IF(STRAT .AND. INDEXRmin<=MDRmin) THEN
                INDEXRmin=INDEXS0C*(0.001*RHOX0C)**C1
                INDEXRmin=MAX(MDRmin, MIN(INDEXRmin, INDEXRstrmax) )
              ENDIF
            ENDIF
          ENDIF
!
          IF(STRAT .OR. TIMLT>EPSQ) THEN
            DRZL=.FALSE.
          ELSE
!- DRZL=T for drizzle (no melted ice falling from above)
            DRZL=.TRUE.    !mar30
          ENDIF
!jun01 - end
!
!----------------------------------------------------------------------
!--------------- Calculate individual processes -----------------------
!----------------------------------------------------------------------
!
!--- Cloud water autoconversion to rain (PRAUT) and collection of cloud 
!    water by precipitation ice (PIACW)
!    
          IF (QW.GT.EPSQ .AND. TC.GE.T_ICE) THEN
!-- The old autoconversion threshold returns
            DUM2=RHO*QW
            IF (DUM2>QAUT0) THEN
!-- July 2010 version follows Liu & Daum (JAS, 2004) and Liu et al. (JAS, 2006)
              DUM2=DUM2*DUM2
              DUM=BRAUT*DUM2*QW
              DUM1=ARAUT*DUM2
              PRAUT=MIN(QW, DUM*(1.-EXP(-DUM1*DUM1)) )
            ENDIF
            IF (QLICE .GT. EPSQ) THEN
      !
      !--- Collection of cloud water by large ice particles ("snow")
      !    PIACWI=PIACW for riming, PIACWI=0 for shedding
      !
              FWS=MIN(1., CIACW*VEL_INC*NLICE*ACCRI(INDEXS) ) !jul28 (16)
              PIACW=FWS*QW
              IF (TC<0.) THEN
                PIACWI=PIACW            !- Large ice riming
                Rcw=ARcw*DUM2**C1       !- Cloud droplet radius in microns
              ENDIF
            ENDIF           ! End IF (QLICE .GT. EPSQ)
          ENDIF             ! End IF (QW.GT.EPSQ .AND. TC.GE.T_ICE)
!
!----------------------------------------------------------------------
!--- Calculate homogeneous freezing of cloud water (PIACW, PIACWI) and 
!    ice deposition (PIDEP), which also includes ice initiation (PINIT)
!
ice_only: IF (TC.LT.T_ICE .AND. (WV.GT.QSWgrd .OR. QW.GT.EPSQ)) THEN
   !
   !--- Adjust to ice saturation at T<T_ICE (-40C) if saturated w/r/t water
   !    or if cloud water is present (homogeneous glaciation).
   !    
            PIACW=QW
            PIACWI=PIACW
            Rcw=0.                             ! Homogeneous freezing of cloud water adds to
                                               ! cloud ice, do not use to update RF for large ice
            DUM1=TK+XLF1*PIACW                 ! Updated (dummy) temperature (deg K)
            DUM2=WV                            ! Updated (dummy) water vapor mixing ratio
            DUM=MIN(1000.*FPVS(DUM1),0.99*PP)  ! Updated (dummy) saturation vapor pressure w/r/t ice
            DUM=RHgrd*EPS*DUM/(PP-DUM)         ! Updated (dummy) saturation mixing ratio w/r/t ice

!-- Debug 20120111
IF (WARN1 .AND. DUM1<XMIN) THEN
   WRITE(0,*) 'WARN1: Water saturation T<180K; I,J,L,TC,P=',   &
              I_index,J_index,L,DUM1-T0C,.01*PP
   WARN1=.FALSE.
ENDIF

!-- Adjust to ice saturation if homogeneous freezing occurs
            IF (DUM2>DUM)                                               &
     &          PIDEP=DEPOSIT(PP,DUM1,DUM2,RHgrd,I_index,J_index,L)

            DWVi=0.    ! Used only for debugging
   !
          ELSE IF (TC<0.) THEN  ice_only
   !
   !--- These quantities are handy for ice deposition/sublimation
   !    PIDEP_max - max deposition or minimum sublimation to ice saturation
   !
            DENOMI=1.+XLS3*QSI*TK2
!            DWVi=MIN(WV,QSW)-QSIgrd    !may17
            DWVi=WV-QSIgrd          !may17   !-- Speed up ice deposition
            PIDEP_max=MAX(PILOSS, DWVi/DENOMI)
            DIDEP=0.     !-- Vapor deposition/sublimation onto existing ice
            IF (QTICE .GT. 0.) THEN
      !
      !--- Calculate ice deposition/sublimation
      !      * SFACTOR - [VEL_INC**.5]*[Schmidt**(1./3.)]*[(RHO/DYNVIS)**.5],
      !        where Schmidt (Schmidt Number) =DYNVIS/(RHO*DIFFUS)
      !      * Units: SFACTOR - s**.5/m ;  ABI - m**2/s ;  NLICE - m**-3 ;
      !               VENTIL, VENTIS - m**-2 ;  VENTI1 - m ;  
      !               VENTI2 - m**2/s**.5 ; DIDEP - unitless
      !
              ABI=1./(RHO*XLS2*QSI*TK2/THERM_COND+1./DIFFUS)
      !
      !--- VENTIL - Number concentration * ventilation factors for large ice
      !--- VENTIS - Number concentration * ventilation factors for small ice
      !
      !--- Variation in the number concentration of ice with time is not
      !      accounted for in these calculations (could be in the future).
      !
              DUM=(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
              SFACTOR=SQRT(GAMMAS)*DUM              !-- GAMMAS (RF=1 for cloud ice)
              VENTIS=(VENTI1(MDImin)+SFACTOR*VENTI2(MDImin))*NSmICE
              SFACTOR=SQRT(VEL_INC)*DUM
              VENTIL=(VENTI1(INDEXS)+SFACTOR*VENTI2(INDEXS))*NLICE
              DIDEP=ABI*(VENTIL+VENTIS)*DTPH
            ENDIF   !-- IF (QTICE .GT. 0.) THEN
      !
      !--- WARNING: the Meyers et al. stuff is not used!
      !--- Two modes of ice nucleation from Meyers et al. (JAM, 1992):
      !      (1) Deposition & condensation freezing nucleation - eq. (2.4),
      !          requires ice supersaturation
      !      (2) Contact freezing nucleation - eq. (2.6), requires presence
      !          of cloud water
      !
      !--- Ice crystal growth during the physics time step is calculated using
      !    Miller & Young (1979, JAS) and is represented by MY_GROWTH(INDEX_MY),
      !    where INDEX_MY is tabulated air temperatures between -1 and -35 C.
      !    The original Miller & Young (MY) calculations only went down to -30C,
      !    so a fixed value is assumed at temperatures colder than -30C.
      !
      !--- Sensitivity test using:
      !    - Fletcher (1962) for ice initiation
      !    - Can occur only when there is water supersaturation and T<-12C.
      !    - Maximum cloud ice number concentration of 100 per liter
      !
            IF (WV>QSWgrd .AND. TC<T_ICE_init .AND. NSmICE<NInuclei) THEN
              INDEX_MY=MAX(MY_T1, MIN( INT(.5-TC), MY_T2 ) )
      !-- Only initiate small ice to get to NInuclei number concentrations
              DUM=MAX(0., NInuclei-NSmICE)
              !PINIT=MAX(0., DUM*MY_GROWTH_NMM(INDEX_MY)*RRHO)
              PINIT=MAX(0., DUM*MY_GROWTH(INDEX_MY)*RRHO)
            ENDIF
      !
      !--- Calculate PIDEP, but also account for limited water vapor supply
      !
            IF (DWVi>0.) THEN
              PIDEP=MIN(DWVI*DIDEP+PINIT, PIDEP_max)
            ELSE IF (DWVi<0.) THEN
              PIDEP=MAX(DWVi*DIDEP, PIDEP_max)
            ENDIF
   !
          ENDIF  ice_only
!
!------------------------------------------------------------------------
!--- Cloud water condensation
!
          IF (TC>=T_ICE .AND. (QW>EPSQ .OR. WV>QSWgrd)) THEN
!apr21 - start; see (39) above
            DUM1=QW-PIACWI
            DUM2=TK+XLS1*PIDEP+XLF1*PIACWI
            DUM3=WV-PIDEP
            PCOND=CONDENSE(PP,DUM1,DUM2,DUM3,RHgrd,I_index,J_index,L)
!apr21 - end; see (39) above
          ENDIF
!
!--- Limit freezing of accreted rime to prevent temperature oscillations,
!    a crude Schumann-Ludlam limit (p. 209 of Young, 1993). 
!
          TCC=TC+XLV1*PCOND+XLS1*PIDEP+XLF1*PIACWI
          IF (TCC>0.) THEN
            PIACWI=0.
            Rcw=0.              !apr18  see (38)
            PIDEP=0.            !apr18  see (38)
            TCC=TC+XLV1*PCOND   !apr18  see (38)
          ENDIF
!
          QSW0=0.
          IF (TC.GT.0. .AND. TCC.GT.0. .AND. ICE_logical) THEN
   !
   !--- Calculate melting and evaporation/condensation
   !      * Units: SFACTOR - s**.5/m ;  ABI - m**2/s ;  NLICE - m**-3 ;
   !               VENTIL - m**-2 ;  VENTI1 - m ;  
   !               VENTI2 - m**2/s**.5 ; CIEVP - /s
   !
            SFACTOR=SQRT(VEL_INC)*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
            VENTIL=NLICE*(VENTI1(INDEXS)+SFACTOR*VENTI2(INDEXS))
            AIEVP=VENTIL*DIFFUS*DTPH
            IF (AIEVP .LT. Xratio) THEN
              DIEVP=AIEVP
            ELSE
              DIEVP=1.-EXP(-AIEVP)
            ENDIF
            QSW0=EPS*ESW0/(PP-ESW0)
            DWV0=MIN(WV,QSW)-QSW0
            DUM=QW+PCOND
            IF (WV.LT.QSW .AND. DUM.LE.EPSQ) THEN
   !
   !--- Evaporation from melting snow (sink of snow) or shedding
   !    of water condensed onto melting snow (source of rain)
   !
              DUM=DWV0*DIEVP
              PIEVP=MAX( MIN(0., DUM), PILOSS)
              PICND=MAX(0., DUM)
            ENDIF            ! End IF (WV.LT.QSW .AND. DUM.LE.EPSQ)
            PIMLT=THERM_COND*TCC*VENTIL*RRHO*DTPH/XLF
   !
   !--- Limit melting to prevent temperature oscillations across 0C
   !
            DUM1=MAX( 0., (TCC+XLV1*PIEVP)/XLF1 )
            PIMLT=MIN(PIMLT, DUM1)
   !
   !--- Limit loss of snow by melting (>0) and evaporation
   !
            DUM=PIEVP-PIMLT
            IF (DUM .LT. PILOSS) THEN
              DUM1=PILOSS/DUM
              PIMLT=PIMLT*DUM1
              PIEVP=PIEVP*DUM1
            ENDIF           ! End IF (DUM .GT. QTICE)
          ENDIF             ! End IF (TC.GT.0. .AND. TCC.GT.0. .AND. ICE_logical) 
!
!--- IMPORTANT:  Estimate time-averaged properties.
!
!  * TOT_RAIN - total mass of rain before microphysics, which is the sum of
!               the total mass of rain in the current layer and the input 
!               flux of rain from above
!  * VRAIN1   - fall speed of rain into grid from above
!  * VRAIN2   - fall speed of rain out of grid box to the level below
!  * QTRAIN   - time-averaged mixing ratio of rain (kg/kg)
!  * PRLOSS   - greatest loss (<0) of rain, removing all rain falling from
!               above and the rain within the layer
!  * RQR      - rain content (kg/m**3)
!  * INDEXR   - mean size of rain drops to the nearest 1 micron in size
!  * N0r      - intercept of rain size distribution (typically 10**6 m**-4)
!
          TOT_RAIN=0.
          QTRAIN=0.
          PRLOSS=0.
          RQR=0.
do_rain:  IF (RAIN_logical) THEN
            TOT_RAIN=THICK*QR+BLEND*ARAIN   !aug27 begin mods
            PRLOSS=-TOT_RAIN/THICK
!may11 - start
            QTRAIN=TOT_RAIN/(THICK+BLDTRH*VRAIN1) !- Rain mixing ratio
            RQR=RHO*QTRAIN                        !- Rain content
            IF(STRAT .AND. RQR>1.E-3) STRAT=.FALSE.    !may31
            IF(DRZL .AND. PIMLT>EPSQ) DRZL=.FALSE.     !jun01
!
DSD1:       IF (RQR<=RQR_DRmin) THEN
!-- Extremely light rain
              INDEXR=MDRmin
              N0r=RQR/MASSR(INDEXR)
            ELSE IF (DRZL) THEN  DSD1
!-- Drizzle - gradually increase N0r for rain contents <0.5 g m^-3   !may31
              DUM=MAX(1.0, 0.5E-3/RQR)
              N0r=MIN(1.E9, N0r0*DUM*SQRT(DUM) )   !- 8.e6 <= N0r <= 1.e9
              DUM1=RQR/(PI*RHOL*N0r)
              DUM=1.E6*SQRT(SQRT(DUM1))
              INDEXR=MAX(XMRmin, MIN(DUM, XMRmax) )
              N0r=RQR/MASSR(INDEXR)
!may20 - start
            ELSE IF (RQR>=RQR_DRmax) THEN  DSD1
!-- Extremely heavy rain
              INDEXR=MDRmax
              N0r=RQR/MASSR(INDEXR)
            ELSE  DSD1
!-- Light to heavy rain
              N0r=N0r0
              DUM=CN0r0*SQRT(SQRT(RQR))
              INDEXR=MAX(XMRmin, MIN(DUM, XMRmax) )
              IF (STRAT .AND. INDEXR<INDEXRmin) THEN
!-- Stratiform rain
                INDEXR=INDEXRmin
                N0r=RQR/MASSR(INDEXR)
              ENDIF
            ENDIF  DSD1
!may20 - end
            VRAIN2=GAMMAR*VRAIN(INDEXR)    !-- VRAIN2 is used below
!may11 - end
!
            IF (TC .LT. T_ICE) THEN
              PIACR=-PRLOSS
            ELSE
              DWVr=WV-PCOND-QSW
              DUM=QW+PCOND
              IF (DWVr.LT.0. .AND. DUM.LE.EPSQ) THEN
      !
      !--- Rain evaporation
      !
      !    * RFACTOR - [GAMMAR**.5]*[Schmidt**(1./3.)]*[(RHO/DYNVIS)**.5],
      !        where Schmidt (Schmidt Number) =DYNVIS/(RHO*DIFFUS)
      !
      !    * Units: RFACTOR - s**.5/m ;  ABW - m**2/s ;  VENTR - m**-2 ;  
      !             N0r - m**-4 ;  VENTR1 - m**2 ;  VENTR2 - m**3/s**.5 ;
      !             CREVP - unitless
      !
                RFACTOR=SQRT(GAMMAR)*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
                ABW=1./(RHO*XLV2*QSW*TK2/THERM_COND+1./DIFFUS)
      !
      !--- Note that VENTR1, VENTR2 lookup tables do not include the 
      !      1/Davg multiplier as in the ice tables
      !
                VENTR=N0r*(VENTR1(INDEXR)+RFACTOR*VENTR2(INDEXR))
                CREVP=ABW*VENTR*DTPH
                PREVP=MAX(DWVr*CREVP, PRLOSS)
              ELSE IF (QW .GT. EPSQ) THEN
                FWR=CRACW*GAMMAR*N0r*ACCRR(INDEXR)
                PRACW=MIN(1.,FWR)*QW
              ENDIF           ! End IF (DWVr.LT.0. .AND. DUM.LE.EPSQ)
      !
              IF (ICE_logical .AND. TC<0. .AND. TCC<0.) THEN
         !
         !--- Biggs (1953) heteorogeneous freezing (e.g., Lin et al., 1983)
         !   - Rescaled mean drop diameter from microns (INDEXR) to mm (DUM) to prevent underflow
         !
                DUM=.001*FLOAT(INDEXR)
                DUM=(EXP(ABFR*TC)-1.)*DUM*DUM*DUM*DUM*DUM*DUM*DUM
                PIACR=MIN(CBFR*N0r*RRHO*DUM, QTRAIN)
!- Start changes listed above in (33) - no collisional freezing of rain
                FIR=0.

                 IF (QLICE .GT. EPSQ) THEN
             !
             !--- Freezing of rain by collisions w/ large ice
             !
                   DUM=GAMMAR*VRAIN(INDEXR)
                   DUM1=DUM-VSNOW
             !
             !--- DUM2 - Difference in spectral fall speeds of rain and
             !      large ice, parameterized following eq. (48) on p. 112 of 
             !      Murakami (J. Meteor. Soc. Japan, 1990)
             !
                   DUM2=SQRT(DUM1*DUM1+.04*DUM*VSNOW)
                   DUM1=5.E-12*INDEXR*INDEXR+2.E-12*INDEXR*INDEXS        &
      &                 +.5E-12*INDEXS*INDEXS
                   FIR=MIN(1., CIACR*NLICE*DUM1*DUM2)
             !
             !--- Future?  Should COLLECTION BY SMALL ICE BE INCLUDED???
             !
                   PIACR=MIN(PIACR+FIR*QTRAIN, QTRAIN)
                 ENDIF        ! End IF (QLICE .GT. EPSQ)
                DUM=PREVP-PIACR
                If (DUM .LT. PRLOSS) THEN
                  DUM1=PRLOSS/DUM
                  PREVP=DUM1*PREVP
                  PIACR=DUM1*PIACR
                ENDIF        ! End If (DUM .LT. PRLOSS)
              ENDIF          ! End IF (TC.LT.0. .AND. TCC.LT.0.)
            ENDIF            ! End IF (TC .LT. T_ICE)
          ENDIF  do_rain     ! End IF (RAIN_logical) 
!
          INDEXR1=INDEXR
!
!----------------------------------------------------------------------
!---------------------- Main Budget Equations -------------------------
!----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!--- Update fields, determine characteristics for next lower layer ----
!-----------------------------------------------------------------------
!
!--- Carefully limit sinks of cloud water
!
          DUM1=PIACW+PRAUT+PRACW-MIN(0.,PCOND)
          IF (DUM1 .GT. QW) THEN
            DUM=QW/DUM1
            PIACW=DUM*PIACW
            PIACWI=DUM*PIACWI
            PRAUT=DUM*PRAUT
            PRACW=DUM*PRACW
            IF (PCOND .LT. 0.) PCOND=DUM*PCOND
          ENDIF
          PIACWR=PIACW-PIACWI          ! TC >= 0C
!
!--- QWnew - updated cloud water mixing ratio
!
          DELW=PCOND-PIACW-PRAUT-PRACW
          QWnew=QW+DELW
          IF (QWnew .LE. EPSQ) QWnew=0.
          IF (QW.GT.0. .AND. QWnew.NE.0.) THEN
            DUM=QWnew/QW
            IF (DUM .LT. TOLER) QWnew=0.
          ENDIF
!
!--- Update temperature and water vapor mixing ratios
!
          DELT= XLV1*(PCOND+PIEVP+PICND+PREVP)                          &
     &         +XLS1*PIDEP+XLF1*(PIACWI+PIACR-PIMLT)
          Tnew=TK+DELT
!
          DELV=-PCOND-PIDEP-PIEVP-PICND-PREVP
          WVnew=WV+DELV
!
!--- Update ice mixing ratios
!
!---
!  * TOT_ICEnew - total mass (small & large) ice after microphysics,
!                 which is the sum of the total mass of ice in the 
!                 layer and the flux of ice out of the grid box below
!  * RimeF      - Rime Factor, which is the mass ratio of total (unrimed & 
!                 rimed) ice mass to the unrimed ice mass (>=1)
!  * QInew      - updated mixing ratio of total (large & small) ice in layer
!  * QLICEnew=FLIMASS*QInew, an estimate of the updated large ice mixing ratio
!
!  -> TOT_ICEnew=QInew*THICK+QLICEnew*BLDTRH*VSNOW+(QInew-QLICEnew)*BLDTRH*VCI
!               =QInew*THICK+QInew*FLIMASS*BLDTRH*VSNOW+QInew*(1.-FLIMASS)*BLDTRH*VCI
!               =QInew*THICK+QInew*BLDTRH*(FLIMASS*VSNOW+(1.-FLIMASS)*VCI)
!               =QInew*(THICK+BLDTRH*(FLIMASS*VSNOW+(1.-FLIMASS)*VCI))
!  -> Rearranging this equation gives:
!     QInew=TOT_ICEnew/(THICK+BLDTRH*(FLIMASS*VSNOW+(1.-FLIMASS)*VCI))
!
!  * ASNOWnew   - updated accumulation of snow and cloud ice at bottom of grid cell
!      -> ASNOWnew=QInew*BLDTRH*(FLIMASS+VSNOW+(1.-FLIMASS)*VCI)
!---
!
          DELI=0.
          RimeF=1.
          IF (ICE_logical) THEN
            DELI=PIDEP+PIEVP+PIACWI+PIACR-PIMLT
            TOT_ICEnew=TOT_ICE+THICK*DELI
            IF (TOT_ICE.GT.0. .AND. TOT_ICEnew.NE.0.) THEN
              DUM=TOT_ICEnew/TOT_ICE
              IF (DUM .LT. TOLER) TOT_ICEnew=0.
            ENDIF
            IF (TOT_ICEnew .LE. CLIMIT) THEN
              TOT_ICEnew=0.
              RimeF=1.
              QInew=0.
              ASNOWnew=0.
            ELSE
!
!--- Update rime factor if appropriate
!
!apr22 - start; see (40) above
              IF (PINIT<=EPSQ) THEN
                PSDEP=MAX(0., PIDEP)
              ELSE
!-- Assume vapor deposition is onto cloud ice if PINT>0
                PSDEP=0.
              ENDIF
              DELS=PIACWI+PIACR+PSDEP
              IF (DELS<=EPSQ .OR. Tnew>=T0C) THEN
                RimeF=RimeF1
              ELSE
!
!-----------------------------------------------------------------------
! RimeF is the new, updated rime factor; RimeF1 is the existing rime factor
!
! From near line 1115:
!    RimeF1=(RimeF_col(L)*THICK*QLgICE+RimeF_col(LBEF)*BLEND*ASNOW)/TOT_ICE
!    RimeF1*TOT_ICE=RimeF_col(L)*THICK*QLgICE+RimeF_col(LBEF)*BLEND*ASNOW
!
! TOT_ICEnew=TOT_ICE+THICK*DELI
! TOT_ICEnew=TOT_ICE+THICK*(PIDEP+PIEVP+PIACWI+PIACR-PIMLT)
!
! But the following processes do not affect the rime factor (ice density):
!    1) PIEVP, evaporation from melting ice 
!    2) PIDEP<0, sublimation of ice
!    3) PIMLT, melting of ice because it is shed to rain
!
! So the final version is
!    TOT_ICEnew=TOT_ICE+THICK*DELS, 
!    DELS=PSDEP+PIACWI+PIACR,
!    PSDEP=MAX(0., PIDEP) only if PINIT<=EPSQ
!
!-----------------------------------------------------------------------
! Rime factor values associated with different processes.
!-----------------------------------------------------------------------
! 1) RF=1 for growth by vapor deposition.
! 2) RFmx, the RF associated with an ice density of 900 kg m^-3, the assumed
!    properties for the freezing of rain to ice (PIACR).
! 3) RFrime, the RF associated with the density of rimed ice, the assumed
!    properties for the riming of cloud water onto ice (PIACWI).
!
! The rime factor (RFnew) associated with the various microphysical processes is:
!   RFnew=(1.*PSDEP+RFrime*PIACWI+RFmx*PIACR)/DELS,
!
! The updated rime factor (RimeF) becomes:
!   RimeF*TOT_ICEnew=RimeF1*TOT_ICE+THICK*DELS*RFnew,
!   RimeF=(RimeF1*TOT_ICE+THICK*DELS*RFnew)/TOT_ICEnew
!-----------------------------------------------------------------------
!
!-- Calculate density of rimed ice following Heymsfield and Pflaum (1985), where
!      Rime density = 300.*(-Rcw*Vimpact/Tsfc)**0.44, 
!   in which Rcw is the mean diameter of the cloud droplets, Vimpact=VSNOW, and 
!   Tsfc (surface temperature of the particle, deg C) is approximated by TC.
!
!   Here the calculations are extended to temperatures as warm as -0.5C, whereas
!   the original study only looked at ice particles whose surface temperature were
!   colder than -5C.  Rime densities will vary from 170 to 900 kg m^-3 
!   (Straka, 2009 textbook; p. 308).
!
                DUM=1.E-6*REAL(INDEXS)              !- Mean diameter in m
                DUM1=DUM*DUM*DUM/MASSI(INDEXS)      !- Used to estimate ice densities
                RFmx=RFmx1*DUM1                     !- Rime factor for density of pure ice (PIACR)
                IF (PIACWI>0. .AND. Rcw>0.) THEN
                  DUM=-Rcw*VSNOW/MIN(-0.5,TC)
                  DUM=MIN(12.14, MAX(0.275, DUM) )
                  DUM=300.*DUM**0.44                !- Initial rime density
                  DUM=MIN(900., MAX(170., DUM) )    !- Final rime density, kg m^-3
                  RFrime=PI*DUM*DUM1                !- Rime factor for the density of rimed ice
                ELSE
                  RFrime=1.                         !- Homogeneous freezing of cloud water does not 
                                                    !- modify RF, contributes to cloud ice
                ENDIF
!
                RFnew=(PSDEP+RFrime*PIACWI+RFmx*PIACR)/DELS
                DUM2=TOT_ICE+THICK*DELS
                DUM3=RimeF1*TOT_ICE+RFnew*THICK*DELS
                IF (DUM2<=CLIMIT) THEN
                  RimeF=RimeF1
                ELSE
                  RimeF=MIN(RFmx, MAX(1., DUM3/DUM2) )
                ENDIF
!apr22 - end; see (40) above
              ENDIF       ! End IF (DUM.LE.EPSQ .AND. PIDEP.LE.EPSQ)
!
              DUM1=BLDTRH*(FLIMASS*VSNOW+(1.-FLIMASS)*VCI)
              QInew=TOT_ICEnew/(THICK+DUM1)
              IF (QInew .LE. EPSQ) QInew=0.
              IF (QI.GT.0. .AND. QInew.NE.0.) THEN
                DUM=QInew/QI
                IF (DUM .LT. TOLER) QInew=0.
              ENDIF
              ASNOWnew=QInew*DUM1
              IF (ASNOW.GT.0. .AND. ASNOWnew.NE.0.) THEN
                DUM=ASNOWnew/ASNOW
                IF (DUM .LT. TOLER) ASNOWnew=0.
              ENDIF
              RQSnew=FLIMASS*RHO*QInew
              IF (RQSnew>0.) THEN    !jul28 begin
!mar03 - Use mean diameter for snow/graupel (INDEXS) from above
                Nsnow=RQSnew/(RimeF*MASSI(INDEXS))
                IF (RQSnew>0.0025 .AND. RimeF>5.) THEN
!mar12 - Blend to Nsnow=1.e3 for 2.5 g m^-3 < RQSnew <= 5 g m^-3 when RF>5
                  IF (RQSnew<=0.005) THEN
                    DUM=MIN(1., 400.*RQSnew-1.)       !- Blend at 2.5 < RQSnew (g m^-3) < 5
                    Nsnow=1.E3*DUM+Nsnow*(1.-DUM)     !- Final, blended Nsnow
                  ELSE
!-- When RF>5, this branch will produce 56.1, 62.2, & 69.1 dBZ reflectivities 
!   when RQSnew reaches 5, 7.5, and 10 g m^-3 (respectively) due to
!   Nsnow being reduced to 1, 0.55, and 0.2 L^-1 (respectively). A 60 dBZ is reached 
!   when RQSnew=6.6 g m^-3 & Nsnow=0.712 L^-1 (solving a quadratic eqn).
                    DUM=180.*(RQSnew-0.005)           !- Steadily decrease Nsnow at > 5 g m^-3
                    Nsnow=1.E3*(1.-MIN(DUM,0.8))      !- from 1 L^-1 down to 0.2 L^-1 at >=10 g m^-3
                  ENDIF
                ENDIF
                Zsnow=Cdry*RQSnew*RQSnew/Nsnow
              ENDIF   !jul28 end
            ENDIF         ! End IF (TOT_ICEnew .LE. CLIMIT)
          ENDIF           ! End IF (ICE_logical)
!
!--- Update rain mixing ratios
!
!---
! * TOT_RAINnew - total mass of rain after microphysics
!                 current layer and the input flux of ice from above
! * VRAIN2      - time-averaged fall speed of rain in grid and below 
! * QRdum       - first-guess estimate (dummy) rain mixing ratio in layer
!                 (uses rain fall speed from grid box above, VRAIN1)
! * QRnew       - updated rain mixing ratio in layer
!      -> TOT_RAINnew=QRnew*(THICK+BLDTRH*VRAIN2)
!  * ARAINnew  - updated accumulation of rain at bottom of grid cell
!---
!
          TIMLT=TIMLT+PIMLT*THICK     !may31
          DELR=PRAUT+PRACW+PIACWR-PIACR+PIMLT+PREVP+PICND
          TOT_RAINnew=TOT_RAIN+THICK*DELR
          IF (TOT_RAIN.GT.0. .AND. TOT_RAINnew.NE.0.) THEN
            DUM=TOT_RAINnew/TOT_RAIN
            IF (DUM .LT. TOLER) TOT_RAINnew=0.
          ENDIF
update_rn: IF (TOT_RAINnew .LE. CLIMIT) THEN
            TOT_RAINnew=0.
            VRAIN2=0.
            QRnew=0.
            ARAINnew=0.
          ELSE  update_rn
!may11 - start
            QRnew=TOT_RAINnew/(THICK+BLDTRH*VRAIN2)
            RQRnew=RHO*QRnew
!may20 - start - made slight changes to speed up algorithm, plus remove
!      block that increases N0r for heavy rain.
            IF(STRAT .AND. RQRnew>1.E-3) STRAT=.FALSE.  !may31
            IF(DRZL .AND. TIMLT>EPSQ) DRZL=.FALSE.      !jun01
!
!-- Iterate for consistent QRnew, RQRnew, N0r, INDEXR2, VRAIN2
!
            INDEXR2=INDEXR1
rain_pass:  DO IPASS=1,3
DSD2:         IF (RQRnew<=RQR_DRmin) THEN
!-- Extremely light rain
                INDEXR=MDRmin
                N0r=RQRnew/MASSR(INDEXR)
              ELSE IF (DRZL) THEN  DSD2
!-- Drizzle - gradually increase N0r for rain contents <0.5 g m^-3   !may31
                DUM=MAX(1.0, 0.5E-3/RQRnew)
                N0r=MIN(1.E9, N0r0*DUM*SQRT(DUM) )   !- 8.e6 <= N0r <= 1.e9
                DUM1=RQRnew/(PI*RHOL*N0r)
                DUM=1.E6*SQRT(SQRT(DUM1))
                INDEXR=MAX(XMRmin, MIN(DUM, XMRmax) )
                N0r=RQRnew/MASSR(INDEXR)
              ELSE IF (RQRnew>=RQR_DRmax) THEN
!-- Extremely heavy rain
                INDEXR=MDRmax
                N0r=RQRnew/MASSR(INDEXR)
              ELSE DSD2
!-- Light to heavy rain
                N0r=N0r0
                DUM=CN0r0*SQRT(SQRT(RQRnew))
                INDEXR=MAX(XMRmin, MIN(DUM, XMRmax) )
                IF (STRAT .AND. INDEXR<INDEXRmin) THEN
!-- Stratiform rain
                  INDEXR=INDEXRmin
                  N0r=RQRnew/MASSR(INDEXR)
                ENDIF
              ENDIF  DSD2
              VRAIN2=GAMMAR*VRAIN(INDEXR)
              QRnew=TOT_RAINnew/(THICK+BLDTRH*VRAIN2)
              RQRnew=RHO*QRnew
              IDR=ABS(INDEXR-INDEXR2)
              INDEXR2=INDEXR   !jun13
              IF(IDR<=5) EXIT  rain_pass    !- Agreement within 5 microns
!may20 - end
            ENDDO  rain_pass
!may11 - end
            IF (QRnew .LE. EPSQ) QRnew=0.
            IF (QR.GT.0. .AND. QRnew.NE.0.) THEN
              DUM=QRnew/QR
              IF (DUM .LT. TOLER) QRnew=0.
            ENDIF
            ARAINnew=BLDTRH*VRAIN2*QRnew
            IF (ARAIN.GT.0. .AND. ARAINnew.NE.0.) THEN
              DUM=ARAINnew/ARAIN
              IF (DUM .LT. TOLER) ARAINnew=0.
            ENDIF
            RQRnew=RHO*QRnew
!may11 - start
            IF (RQRnew>EPSQ) THEN
!may20 - remove may11 change in N0r calculation
              Nrain=N0r*1.E-6*REAL(INDEXR2)
              Zrain=Crain*RQRnew*RQRnew/Nrain   !-- Rain reflectivity
            ENDIF
!may11 - end
          ENDIF  update_rn
!
          WCnew=QWnew+QRnew+QInew
!
!----------------------------------------------------------------------
!-------------- Begin debugging & verification ------------------------
!----------------------------------------------------------------------
!
!--- QT, QTnew - total water (vapor & condensate) before & after microphysics, resp.
!
          QT=THICK*(WV+WC)+ARAIN+ASNOW
          QTnew=THICK*(WVnew+WCnew)+ARAINnew+ASNOWnew
          BUDGET=QT-QTnew
!
!--- Additional check on budget preservation, accounting for truncation effects
!
          DBG_logical=.FALSE.
          DUM=ABS(BUDGET)
          IF (DUM .GT. TOLER) THEN
            DUM=DUM/MIN(QT, QTnew)
            IF (DUM .GT. TOLER) DBG_logical=.TRUE.
          ENDIF
!
          IF (PRINT_diag) THEN
            ESW=MIN(1000.*FPVS0(Tnew),0.99*PP)
            QSWnew=(RHgrd+0.001)*EPS*ESW/(PP-ESW)
            IF( (QWnew>EPSQ .OR. QRnew>EPSQ .OR. WVnew>QSWnew)          &
     &         .AND. TC<T_ICE) DBG_logical=.TRUE.
          ENDIF
!
          IF ((WVnew.LT.EPSQ .OR. DBG_logical) .AND. PRINT_diag) THEN
   !
            WRITE(0,"(/2(a,i4),2(a,i2))") '{} i=',I_index,' j=',J_index,&
     &                                    ' L=',L,' LSFC=',LSFC
   !
            ESW=MIN(1000.*FPVS0(Tnew),0.99*PP)
            QSWnew=EPS*ESW/(PP-ESW)
            IF (TC.LT.0. .OR. Tnew .LT. 0.) THEN
              ESI=MIN(1000.*FPVS(Tnew),0.99*PP)
              QSInew=EPS*ESI/(PP-ESI)
            ELSE
              QSI=QSW
              QSInew=QSWnew
            ENDIF
            WSnew=QSInew
            WRITE(0,"(4(a12,g11.4,1x))")                                   &
     & '{} TCold=',TC,'TCnew=',Tnew-T0C,'P=',.01*PP,'RHO=',RHO,            &
     & '{} THICK=',THICK,'RHold=',WV/WS,'RHnew=',WVnew/WSnew,              &
     &   'RHgrd=',RHgrd,                                                   &
     & '{} RHWold=',WV/QSW,'RHWnew=',WVnew/QSWnew,'RHIold=',WV/QSI,        &
     &   'RHInew=',WVnew/QSInew,                                           &
     & '{} QSWold=',QSW,'QSWnew=',QSWnew,'QSIold=',QSI,'QSInew=',QSInew,   &
     & '{} WSold=',WS,'WSnew=',WSnew,'WVold=',WV,'WVnew=',WVnew,           &
     & '{} WCold=',WC,'WCnew=',WCnew,'QWold=',QW,'QWnew=',QWnew,           &
     & '{} QIold=',QI,'QInew=',QInew,'QRold=',QR,'QRnew=',QRnew,           &
     & '{} ARAINold=',ARAIN,'ARAINnew=',ARAINnew,'ASNOWold=',ASNOW,        &
     &   'ASNOWnew=',ASNOWnew,                                             &
     & '{} TOT_RAIN=',TOT_RAIN,'TOT_RAINnew=',TOT_RAINnew,                 &
     &   'TOT_ICE=',TOT_ICE,'TOT_ICEnew=',TOT_ICEnew,                      &
     & '{} BUDGET=',BUDGET,'QTold=',QT,'QTnew=',QTnew                       
   !
            WRITE(0,"(4(a12,g11.4,1x))")                                   &
     & '{} DELT=',DELT,'DELV=',DELV,'DELW=',DELW,'DELI=',DELI,             &
     & '{} DELR=',DELR,'PCOND=',PCOND,'PIDEP=',PIDEP,'PIEVP=',PIEVP,       &
     & '{} PICND=',PICND,'PREVP=',PREVP,'PRAUT=',PRAUT,'PRACW=',PRACW,     &
     & '{} PIACW=',PIACW,'PIACWI=',PIACWI,'PIACWR=',PIACWR,'PIMLT=',       &
     &    PIMLT,                                                           &
     & '{} PIACR=',PIACR                                                    
   !
            WRITE(0,"(4(a13,L2))")                                         &
     & '{} ICE_logical=',ICE_logical,'RAIN_logical=',RAIN_logical,         &
     &    'STRAT=',STRAT,'DRZL=',DRZL
   !
            IF (ICE_logical) WRITE(0,"(4(a12,g11.4,1x))")                  &
     & '{} RimeF1=',RimeF1,'GAMMAS=',GAMMAS,'VrimeF=',VrimeF,              &
     &   'VSNOW=',VSNOW,                                                   &
     & '{} QSmICE=',QSmICE,'XLIMASS=',XLIMASS,'RQLICE=',RQLICE,            &
     &   'QTICE=',QTICE,                                                   &
     & '{} NLICE=',NLICE,'NSmICE=',NSmICE,'PILOSS=',PILOSS,                &
     &   'EMAIRI=',EMAIRI,                                                 &
     & '{} RimeF=',RimeF,'VCI=',VCI,'INDEXS=',FLOAT(INDEXS),               &
     &    'FLIMASS=',FLIMASS,                                              &
     & '{} Nsnow=',Nsnow,'Zsnow=',Zsnow,'TIMLT=',TIMLT,'RHOX0C=',RHOX0C,   &
     & '{} INDEXS0C=',FLOAT(INDEXS0C)
   !
            IF (RAIN_logical) WRITE(0,"(4(a12,g11.4,1x))")                 &
     & '{} INDEXR1=',FLOAT(INDEXR1),'INDEXR=',FLOAT(INDEXR),               &
     &   'GAMMAR=',GAMMAR,'N0r=',N0r,                                      &
     & '{} VRAIN1=',VRAIN1,'VRAIN2=',VRAIN2,'QTRAIN=',QTRAIN,'RQR=',RQR,   &
     & '{} PRLOSS=',PRLOSS,'VOLR1=',THICK+BLDTRH*VRAIN1,                   &
     &   'VOLR2=',THICK+BLDTRH*VRAIN2,'INDEXR2=',INDEXR2,                  &
     & '{} Nrain=',Nrain,'Zrain=',Zrain
   !
            IF (PRACW .GT. 0.) WRITE(0,"(a12,g11.4,1x)") '{} FWR=',FWR
   !
            IF (PIACR .GT. 0.) WRITE(0,"(a12,g11.4,1x)") '{} FIR=',FIR
   !
            DUM=PIMLT+PICND-PREVP-PIEVP
            IF (DUM.GT.0. .or. DWVi.NE.0.)                                 &
     &        WRITE(0,"(4(a12,g11.4,1x))")                                 &
     & '{} TFACTOR=',TFACTOR,'DYNVIS=',DYNVIS,                             &
     &   'THERM_CON=',THERM_COND,'DIFFUS=',DIFFUS
   !
            IF (PREVP .LT. 0.) WRITE(0,"(4(a12,g11.4,1x))")                &
     & '{} RFACTOR=',RFACTOR,'ABW=',ABW,'VENTR=',VENTR,'CREVP=',CREVP,     &
     & '{} DWVr=',DWVr,'DENOMW=',DENOMW
   !
            IF (PIDEP.NE.0. .AND. DWVi.NE.0.)                              &
     &        WRITE(0,"(4(a12,g11.4,1x))")                                 &
     & '{} DWVi=',DWVi,'DENOMI=',DENOMI,'PIDEP_max=',PIDEP_max,            &
     &   'SFACTOR=',SFACTOR,                                               &
     & '{} ABI=',ABI,'VENTIL=',VENTIL,'VENTIL1=',VENTI1(INDEXS),           &
     &   'VENTIL2=',SFACTOR*VENTI2(INDEXS),                                &
     & '{} VENTIS=',VENTIS,'DIDEP=',DIDEP
   !
            IF (PIDEP.GT.0. .AND. PCOND.NE.0.)                             &
     &        WRITE(0,"(4(a12,g11.4,1x))")                                 &
     & '{} DENOMW=',DENOMW,'DENOMWI=',DENOMWI,'DENOMF=',DENOMF,            &
     &    'DUM2=',PCOND-PIACW
   !
            IF (FWS .GT. 0.) WRITE(0,"(4(a12,g11.4,1x))")                  &
     & '{} FWS=',FWS                     
   !
            DUM=PIMLT+PICND-PIEVP
            IF (DUM.GT. 0.) WRITE(0,"(4(a12,g11.4,1x))")                   &
     & '{} SFACTOR=',SFACTOR,'VENTIL=',VENTIL,'VENTIL1=',VENTI1(INDEXS),   &
     &   'VENTIL2=',SFACTOR*VENTI2(INDEXS),                                &
     & '{} AIEVP=',AIEVP,'DIEVP=',DIEVP,'QSW0=',QSW0,'DWV0=',DWV0       
   !
          ENDIF
!
!----------------------------------------------------------------------
!------------------------- Update arrays ------------------------------
!----------------------------------------------------------------------
!
          T_col(L)=Tnew                           ! Update temperature
          Q_col(L)=max(EPSQ, WVnew/(1.+WVnew))    ! Update specific humidity
          WC_col(L)=max(EPSQ, WCnew)              ! Update total condensate mixing ratio
          QI_col(L)=max(EPSQ, QInew)              ! Update ice mixing ratio
          QR_col(L)=max(EPSQ, QRnew)              ! Update rain mixing ratio
          QW_col(L)=max(EPSQ, QWnew)              ! Update cloud water mixing ratio
          RimeF_col(L)=RimeF                      ! Update rime factor
          NR_col(L)=Nrain                         ! Update rain number concentration   !jul28 begin
          NS_col(L)=Nsnow                         ! Update snow number concentration
!aligo          IF (RQRnew>RQRmix .AND. RQSnew>RQSmix .and. RimeF>10.) THEN
!            Zsnow=Zsnow*Cwet     !apr27
!aligo          ENDIF
          Ztot=MAX(Ztot, Zrain+Zsnow)
          Ztot=Zrain+Zsnow
          IF (Ztot>Zmin) DBZ_col(L)=10.*ALOG10(Ztot)     ! Update radar reflectivity
          ASNOW=ASNOWnew                          ! Update accumulated snow
          VSNOW1=VSNOW                            ! Update total ice fall speed
          ARAIN=ARAINnew                          ! Update accumulated rain
          VRAIN1=VRAIN2                           ! Update rain fall speed
!
!-- Assign microphysical processes and fall speeds to 1D array
!
          if(pcond.gt.0)then
            pcond1d(L)=pcond
          elseif(pcond.lt.0)then
            pevap1d(L)=pcond
          endif
          if(pidep.gt.0)then
            pidep1d(L)=pidep
          elseif(pidep.lt.0)then
            pisub1d(L)=pidep
          endif
          piacw1d(L)=piacw
          piacwi1d(L)=piacwi
          piacwr1d(L)=piacwr
          piacr1d(L)=piacr
          picnd1d(L)=picnd
          pievp1d(L)=pievp
          pimlt1d(L)=pimlt
          praut1d(L)=praut
          pracw1d(L)=pracw
          prevp1d(L)=prevp
          if (qinew>EPSQ) then
            vsnow1d(L)=vsnow
!sep22a - Start changes
            if (FLIMASS<1.) then 
              vci1d(L)=vci
              NSmICE1d(L)=NSmICE
            endif
!sep22a - End changes
            INDEXS1d(L)=FLOAT(INDEXS)
          endif
          if (qrnew>EPSQ) then
            vrain11d(L)=vrain1
            vrain21d(L)=vrain2
            INDEXR1d(L)=FLOAT(INDEXR2)
!jun01 - start
            IDR=0
            IF(STRAT) IDR=1
            IF(DRZL) IDR=IDR+2
            RFlag1d(L)=IDR
!jun01 - end
          endif
!
!#######################################################################
!
      ENDDO  big_loop
!
!#######################################################################
!
!-----------------------------------------------------------------------
!--------------------------- Return to GSMDRIVE -----------------------
!-----------------------------------------------------------------------
!
        CONTAINS
!#######################################################################
!--------- Produces accurate calculation of cloud condensation ---------
!#######################################################################
!
      REAL FUNCTION CONDENSE (PP,QW,TK,WV,RHgrd,I,J,L)
!
!---------------------------------------------------------------------------------
!------   The Asai (1965) algorithm takes into consideration the release of ------
!------   latent heat in increasing the temperature & in increasing the     ------
!------   saturation mixing ratio (following the Clausius-Clapeyron eqn.).  ------
!---------------------------------------------------------------------------------
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: HIGH_PRES=Selected_Real_Kind(15)
      REAL (KIND=HIGH_PRES), PARAMETER ::                               &
     & RHLIMIT=.001, RHLIMIT1=-RHLIMIT
      REAL (KIND=HIGH_PRES) :: COND, SSAT, WCdum
!
      REAL,INTENT(IN) :: QW,PP,WV,TK,RHgrd
      REAL WVdum,Tdum,DWV,WS,ESW

integer,INTENT(IN) :: I,J,L     !-- Debug 20120111
integer :: niter
real :: DWVinp,SSATinp

!
!-----------------------------------------------------------------------
!
      Tdum=TK
      WVdum=WV
      WCdum=QW
      ESW=MIN(1000.*FPVS0(Tdum),0.99*PP)        ! Saturation vapor press w/r/t water
      WS=RHgrd*EPS*ESW/(PP-ESW)                 ! Saturation mixing ratio w/r/t water
      DWV=WVdum-WS                              ! Deficit grid-scale water vapor mixing ratio
      SSAT=DWV/WS                               ! Supersaturation ratio
      CONDENSE=0.

DWVinp=DWV     !-- Debug 20120111
SSATinp=SSAT

      DO NITER=1,MAX_ITERATIONS
        COND=DWV/(1.+XLV3*WS/(Tdum*Tdum))       ! Asai (1965, J. Japan)
        COND=MAX(COND, -WCdum)                  ! Limit cloud water evaporation
        Tdum=Tdum+XLV1*COND                     ! Updated temperature
        WVdum=WVdum-COND                        ! Updated water vapor mixing ratio
        WCdum=WCdum+COND                        ! Updated cloud water mixing ratio
        CONDENSE=CONDENSE+COND                  ! Total cloud water condensation
        ESW=MIN(1000.*FPVS0(Tdum),0.99*PP)      ! Updated saturation vapor press w/r/t water
        WS=RHgrd*EPS*ESW/(PP-ESW)               ! Updated saturation mixing ratio w/r/t water
        DWV=WVdum-WS                            ! Deficit grid-scale water vapor mixing ratio
        SSAT=DWV/WS                             ! Grid-scale supersaturation ratio
        IF (SSAT>=RHLIMIT1 .AND. SSAT<=RHLIMIT) EXIT   !-- Exit if SSAT is near 0
        IF (SSAT<RHLIMIT1 .AND. WCdum<=EPSQ) EXIT      !-- Exit if SSAT<0 & no cloud water
      ENDDO

!-- Debug 20120111
IF (NITER>MAX_ITERATIONS) THEN
!-- Too many iterations - indicates possible numerical instability
   IF (WARN3) THEN
      write(0,*) 'WARN3: Too many iterations in function CONDENSE; ', &
         ' I,J,L,TC,SSAT,QW,DWV=',I,J,L,TK-T0C,SSATinp,1000.*QW,DWVinp
      WARN3=.FALSE.
   ENDIF
   SSAT=0.
   CONDENSE=DWVinp
ENDIF

!
      END FUNCTION CONDENSE
!
!#######################################################################
!---------------- Calculate ice deposition at T<T_ICE ------------------
!#######################################################################
!
      REAL FUNCTION DEPOSIT (PP,Tdum,WVdum,RHgrd,I,J,L)   !-- Debug 20120111
!
!--- Also uses the Asai (1965) algorithm, but uses a different target
!      vapor pressure for the adjustment
!
      IMPLICIT NONE      
!
      INTEGER, PARAMETER :: HIGH_PRES=Selected_Real_Kind(15)
      REAL (KIND=HIGH_PRES), PARAMETER :: RHLIMIT=.001,                 &
     & RHLIMIT1=-RHLIMIT
      REAL (KIND=HIGH_PRES) :: DEP, SSAT
!    
      real,INTENT(IN) ::  PP,RHgrd
      real,INTENT(INOUT) ::  WVdum,Tdum
      real ESI,WS,DWV

integer,INTENT(IN) :: I,J,L   !-- Debug 20120111
integer :: niter
real :: Tinp,DWVinp,SSATinp

!
!-----------------------------------------------------------------------
!
      ESI=MIN(1000.*FPVS(Tdum),0.99*PP)         ! Saturation vapor press w/r/t ice
      WS=RHgrd*EPS*ESI/(PP-ESI)                 ! Saturation mixing ratio
      DWV=WVdum-WS                              ! Deficit grid-scale water vapor mixing ratio
      SSAT=DWV/WS                               ! Supersaturation ratio
      DEPOSIT=0.

Tinp=Tdum     !-- Debug 20120111
DWVinp=DWV
SSATinp=SSAT

      DO NITER=1,MAX_ITERATIONS
        DEP=DWV/(1.+XLS3*WS/(Tdum*Tdum))        ! Asai (1965, J. Japan)
        Tdum=Tdum+XLS1*DEP                      ! Updated temperature
        WVdum=WVdum-DEP                         ! Updated ice mixing ratio
        DEPOSIT=DEPOSIT+DEP                     ! Total ice deposition
        ESI=MIN(1000.*FPVS(Tdum),0.99*PP)       ! Updated saturation vapor press w/r/t ice
        WS=RHgrd*EPS*ESI/(PP-ESI)               ! Updated saturation mixing ratio w/r/t ice
        DWV=WVdum-WS                            ! Deficit grid-scale water vapor mixing ratio
        SSAT=DWV/WS                             ! Grid-scale supersaturation ratio
        IF (SSAT>=RHLIMIT1 .AND. SSAT<=RHLIMIT) EXIT   !-- Exit if SSAT is near 0
      ENDDO

!-- Debug 20120111
IF (NITER>MAX_ITERATIONS) THEN
!-- Too many iterations - indicates possible numerical instability
   IF (WARN2) THEN
      write(0,*) 'WARN2: Too many iterations in function DEPOSIT; ', &
         ' I,J,L,TC,SSAT,DWV=',I,J,L,Tinp-T0C,SSATinp,DWVinp
      WARN2=.FALSE.
   ENDIF
   SSAT=0.
   DEPOSIT=DWVinp
ENDIF

!
      END FUNCTION DEPOSIT
!
!#######################################################################
!--- Used to calculate the mean size of rain drops (INDEXR) in microns
!#######################################################################
!
!-- NOTE: this function is not used in this version.
!
      INTEGER FUNCTION GET_INDEXR(RR)
      IMPLICIT NONE
      REAL, INTENT(IN) :: RR
      IF (RR .LE. RR_DRmin) THEN
!
!--- Assume fixed mean diameter of rain (0.05 mm) for low rain rates, 
!      instead vary N0r with rain rate
!
        GET_INDEXR=MDRmin
      ELSE IF (RR .LE. RR_DR1) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
!      for mean diameters (Dr) between 0.05 and 0.10 mm:
!      V(Dr)=5.6023e4*Dr**1.136, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*5.6023e4*Dr**(4+1.136) = 1.408e15*Dr**5.136,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.123e-3*RR**.1947 -> Dr (microns) = 1.123e3*RR**.1947
!
        GET_INDEXR=INT( 1.123E3*RR**.1947 + .5 )
        GET_INDEXR=MAX( MDRmin, MIN(GET_INDEXR, MDR1) )
      ELSE IF (RR .LE. RR_DR2) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
!      for mean diameters (Dr) between 0.10 and 0.20 mm:
!      V(Dr)=1.0867e4*Dr**.958, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*1.0867e4*Dr**(4+.958) = 2.731e14*Dr**4.958,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.225e-3*RR**.2017 -> Dr (microns) = 1.225e3*RR**.2017
!
        GET_INDEXR=INT( 1.225E3*RR**.2017 + .5 )
        GET_INDEXR=MAX( MDR1, MIN(GET_INDEXR, MDR2) )
      ELSE IF (RR .LE. RR_DR3) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
!      for mean diameters (Dr) between 0.20 and 0.32 mm:
!      V(Dr)=2831.*Dr**.80, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*2831.*Dr**(4+.80) = 7.115e13*Dr**4.80, 
!        RR in kg/(m**2*s)
!      Dr (m) = 1.3006e-3*RR**.2083 -> Dr (microns) = 1.3006e3*RR**.2083
!
        GET_INDEXR=INT( 1.3006E3*RR**.2083 + .5 )
        GET_INDEXR=MAX( MDR2, MIN(GET_INDEXR, MDR3) )
      ELSE IF (RR .LE. RR_DR4) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables
!      for mean diameters (Dr) between 0.32 and 0.45 mm:
!      V(Dr)=963.0*Dr**.666, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*963.0*Dr**(4+.666) = 2.4205e13*Dr**4.666,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.354e-3*RR**.2143 -> Dr (microns) = 1.354e3*RR**.2143
!
        GET_INDEXR=INT( 1.354E3*RR**.2143 + .5 )
        GET_INDEXR=MAX( MDR3, MIN(GET_INDEXR, MDR4) )
      ELSE IF (RR .LE. RR_DR5) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables
!      for mean diameters (Dr) between 0.45 and 0.675 mm:
!      V(Dr)=309.0*Dr**.5185, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*309.0*Dr**(4+.5185) = 7.766e12*Dr**4.5185,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.404e-3*RR**.2213 -> Dr (microns) = 1.404e3*RR**.2213
!
        GET_INDEXR=INT( 1.404E3*RR**.2213 + .5 )
        GET_INDEXR=MAX( MDR4, MIN(GET_INDEXR, MDR5) )
      ELSE IF (RR .LE. RR_DRmax) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables
!      for mean diameters (Dr) between 0.675 and 1.0 mm:
!      V(Dr)=85.81Dr**.343, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*85.81*Dr**(4+.343) = 2.1566e12*Dr**4.343,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.4457e-3*RR**.2303 -> Dr (microns) = 1.4457e3*RR**.2303
!
        GET_INDEXR=INT( 1.4457E3*RR**.2303 + .5 )
        GET_INDEXR=MAX( MDR5, MIN(GET_INDEXR, MDRmax) )
      ELSE 
!
!--- Assume fixed mean diameter of rain (1.0 mm) for high rain rates, 
!      instead vary N0r with rain rate
!
        GET_INDEXR=MDRmax
      ENDIF              ! End IF (RR .LE. RR_DRmin) etc. 
!
      END FUNCTION GET_INDEXR
!
      END SUBROUTINE EGCP01COLUMN

!#######################################################################
!------- Initialize constants & lookup tables for microphysics ---------
!#######################################################################
!

! SH 0211/2002

!-----------------------------------------------------------------------
      SUBROUTINE fer_hires_init (GSMDT,DT,DELX,DELY,LOWLYR,restart,         &
!HWRF     &   MP_RESTART_STATE,TBPVS_STATE,TBPVS0_STATE,                     &
     &   ALLOWED_TO_READ,                                               &
     &   IDS,IDE,JDS,JDE,KDS,KDE,                                       &
     &   IMS,IME,JMS,JME,KMS,KME,                                       &
     &   ITS,ITE,JTS,JTE,KTS,KTE,                                       &
     &   F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY)
!-----------------------------------------------------------------------
!-------------------------------------------------------------------------------
!---  SUBPROGRAM DOCUMENTATION BLOCK
!   PRGRMMR: Ferrier         ORG: W/NP22     DATE: February 2001
!            Jin             ORG: W/NP22     DATE: January 2002 
!        (Modification for WRF structure)
!                                               
!-------------------------------------------------------------------------------
! ABSTRACT:
!   * Reads various microphysical lookup tables used in COLUMN_MICRO
!   * Lookup tables were created "offline" and are read in during execution
!   * Creates lookup tables for saturation vapor pressure w/r/t water & ice
!-------------------------------------------------------------------------------
!     
! USAGE: CALL fer_hires_init FROM SUBROUTINE GSMDRIVE AT MODEL START TIME
!
!   INPUT ARGUMENT LIST:
!       DTPH - physics time step (s)
!  
!   OUTPUT ARGUMENT LIST: 
!     NONE
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBROUTINES:
!     MY_GROWTH_RATES - lookup table for growth of nucleated ice
!     GPVS            - lookup tables for saturation vapor pressure (water, ice)
!
!   UNIQUE: NONE
!  
!   LIBRARY: NONE
!  
!   COMMON BLOCKS:
!     CMICRO_CONS - constants used in GSMCOLUMN
!     CMY600       - lookup table for growth of ice crystals in 
!                    water saturated conditions (Miller & Young, 1979)
!     IVENT_TABLES - lookup tables for ventilation effects of ice
!     IACCR_TABLES - lookup tables for accretion rates of ice
!     IMASS_TABLES - lookup tables for mass content of ice
!     IRATE_TABLES - lookup tables for precipitation rates of ice
!     IRIME_TABLES - lookup tables for increase in fall speed of rimed ice
!     MAPOT        - Need lat/lon grid resolution
!     RVENT_TABLES - lookup tables for ventilation effects of rain
!     RACCR_TABLES - lookup tables for accretion rates of rain
!     RMASS_TABLES - lookup tables for mass content of rain
!     RVELR_TABLES - lookup tables for fall speeds of rain
!     RRATE_TABLES - lookup tables for precipitation rates of rain
!   
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!------------------------------------------------------------------------- 
!-------------- Parameters & arrays for lookup tables -------------------- 
!------------------------------------------------------------------------- 
!
!--- Common block of constants used in column microphysics
!
!WRF
!     real DLMD,DPHD
!WRF
!
!-----------------------------------------------------------------------
!--- Parameters & data statement for local calculations
!-----------------------------------------------------------------------
!
      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3
!
!     VARIABLES PASSED IN
      integer,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     & 
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE       
!WRF
       INTEGER, DIMENSION(ims:ime,jms:jme),INTENT(INOUT) :: LOWLYR
!
      real, INTENT(IN) ::  DELX,DELY
!HWRF      real,DIMENSION(*), INTENT(INOUT) :: MP_RESTART_STATE
!HWRF      real,DIMENSION(NX), INTENT(INOUT) :: TBPVS_STATE,TBPVS0_STATE
      real,DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(OUT),OPTIONAL :: &
     &  F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
      real,INTENT(IN) :: DT,GSMDT
      LOGICAL,INTENT(IN) :: allowed_to_read,restart
!
!-----------------------------------------------------------------------
!     LOCAL VARIABLES
!-----------------------------------------------------------------------
      REAL :: BBFR,DTPH,DX,Thour_print,RDIS
      INTEGER :: I,IM,J,L,K,JTF,KTF,ITF
      INTEGER :: etampnew_unit1
      LOGICAL :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      CHARACTER*80 errmess
!
!-----------------------------------------------------------------------
!
      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)
!
      DO J=JTS,JTF
      DO I=ITS,ITF
        LOWLYR(I,J)=1
      ENDDO
      ENDDO
!    
      IF(.NOT.RESTART .AND. ALLOWED_TO_READ .AND. present(F_ICE_PHY)) THEN    !HWRF
        CALL wrf_debug(1,'WARNING: F_ICE_PHY,F_RAIN_PHY AND F_RIMEF_PHY IS REINITIALIZED')   !HWRF
        DO J = jts,jte
        DO K = kts,kte
        DO I= its,ite
          F_ICE_PHY(i,k,j)=0.
          F_RAIN_PHY(i,k,j)=0.
          F_RIMEF_PHY(i,k,j)=1.
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!    
!-----------------------------------------------------------------------
      IF(ALLOWED_TO_READ)THEN
!-----------------------------------------------------------------------
!
        DX=SQRT((DELX)**2+(DELY)**2)/1000.    ! Model resolution at equator (km)  !GFDL
        DX=MIN(100., MAX(5., DX) )
!
!-- Relative humidity threshold for the onset of grid-scale condensation
!!-- 9/1/01:  Assume the following functional dependence for 5 - 100 km resolution:
!!       RHgrd=0.90 for dx=100 km, 0.98 for dx=5 km, where
!        RHgrd=0.90+.08*((100.-DX)/95.)**.5
!
        DTPH=MAX(GSMDT*60.,DT)
        DTPH=NINT(DTPH/DT)*DT
!
!--- Create lookup tables for saturation vapor pressure w/r/t water & ice
!
        CALL GPVS
!
!--- Read in various lookup tables
!
        IF ( wrf_dm_on_monitor() ) THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              etampnew_unit1 = i
              GOTO 2061
            ENDIF
          ENDDO
          etampnew_unit1 = -1
 2061     CONTINUE
        ENDIF
!
        CALL wrf_dm_bcast_bytes ( etampnew_unit1 , IWORDSIZE )
!
        IF ( etampnew_unit1 < 0 ) THEN
          CALL wrf_error_fatal ( 'module_mp_fer_hires: fer_hires_init: Can not find unused fortran unit to read in lookup table.' )
        ENDIF
!
        IF ( wrf_dm_on_monitor() ) THEN
!!was     OPEN (UNIT=1,FILE="eta_micro_lookup.dat",FORM="UNFORMATTED")
!-- Use the new, expanded tables for rain (maximum mean diameter of 1 mm rather than 0.45 mm)
!old      OPEN(UNIT=etampnew_unit1,FILE="ETAMPNEW_DATA",                  &
          print*,'open ETAMPNEW_DATA.expanded_rain, in fer_hires' 
          OPEN(UNIT=etampnew_unit1,FILE="ETAMPNEW_DATA.expanded_rain",  &
     &        FORM="UNFORMATTED",STATUS="OLD",ERR=9061)
!
          READ(etampnew_unit1) VENTR1
          READ(etampnew_unit1) VENTR2
          READ(etampnew_unit1) ACCRR
          READ(etampnew_unit1) MASSR
          READ(etampnew_unit1) VRAIN
          READ(etampnew_unit1) RRATE
          READ(etampnew_unit1) VENTI1
          READ(etampnew_unit1) VENTI2
          READ(etampnew_unit1) ACCRI
          READ(etampnew_unit1) MASSI
          READ(etampnew_unit1) VSNOWI
          READ(etampnew_unit1) VEL_RF
!        read(etampnew_unit1) my_growth    ! Applicable only for DTPH=180 s
          CLOSE (etampnew_unit1)
        ENDIF
!
        CALL wrf_dm_bcast_bytes ( VENTR1 , size ( VENTR1 ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( VENTR2 , size ( VENTR2 ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( ACCRR , size ( ACCRR ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( MASSR , size ( MASSR ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( VRAIN , size ( VRAIN ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( RRATE , size ( RRATE ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( VENTI1 , size ( VENTI1 ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( VENTI2 , size ( VENTI2 ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( ACCRI , size ( ACCRI ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( MASSI , size ( MASSI ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( VSNOWI , size ( VSNOWI ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( VEL_RF , size ( VEL_RF ) * RWORDSIZE )
!
!--- Calculates coefficients for growth rates of ice nucleated in water
!    saturated conditions, scaled by physics time step (lookup table)
!
        CALL MY_GROWTH_RATES (DTPH)
!
        PI_E=ACOS(-1.)
!
!--- Constants associated with Biggs (1953) freezing of rain, as parameterized
!    following Lin et al. (JCAM, 1983) & Reisner et al. (1998, QJRMS).
!
        ABFR=-0.66
        BBFR=100.
        CBFR=20.*PI_E*PI_E*BBFR*RHOL*1.E-21*DTPH   !mar03 - Bug fix in (33); include DTPH
!
!--- CIACW is used in calculating riming rates
!      The assumed effective collection efficiency of cloud water rimed onto
!      ice is =0.5 below:
!
        CIACW=0.5*DTPH*0.25*PI_E  !jul28 (16)
!
!--- CIACR is used in calculating freezing of rain colliding with large ice
!      The assumed collection efficiency is 1.0
!
        CIACR=PI_E*DTPH
!
!--- Based on rain lookup tables for mean diameters from 0.05 to 0.45 mm
!    * Four different functional relationships of mean drop diameter as 
!      a function of rain rate (RR), derived based on simple fits to 
!      mass-weighted fall speeds of rain as functions of mean diameter
!      from the lookup tables.  
!
        RR_DRmin=N0r0*RRATE(MDRmin)     ! RR for mean drop diameter of .05 mm
        RR_DR1=N0r0*RRATE(MDR1)         ! RR for mean drop diameter of .10 mm
        RR_DR2=N0r0*RRATE(MDR2)         ! RR for mean drop diameter of .20 mm
        RR_DR3=N0r0*RRATE(MDR3)         ! RR for mean drop diameter of .32 mm
        RR_DR4=N0r0*RRATE(MDR4)         ! RR for mean drop diameter of .45 mm
        RR_DR5=N0r0*RRATE(MDR5)         ! RR for mean drop diameter of .675 mm
        RR_DRmax=N0r0*RRATE(MDRmax)     ! RR for mean drop diameter of 1.0 mm
!
        RQR_DRmin=N0r0*MASSR(MDRmin)    ! Rain content for mean drop diameter of .05 mm
        RQR_DRmax=N0r0*MASSR(MDRmax)    ! Rain content for mean drop diameter of 1.0 mm
        C_NR=1./(PI*RHOL)   !jul28
        Crain=720.E18*C_NR*C_NR    !jul28
        C_N0r0=PI_E*RHOL*N0r0
        CN0r0=1.E6/SQRT(SQRT(C_N0r0))
        CN0r_DMRmin=1./(PI_E*RHOL*DMRmin*DMRmin*DMRmin*DMRmin)
        CN0r_DMRmax=1./(PI_E*RHOL*DMRmax*DMRmax*DMRmax*DMRmax)
!
!--- CRACW is used in calculating collection of cloud water by rain (an
!      assumed collection efficiency of 1.0)
!
        CRACW=DTPH*0.25*PI_E*1.0
!
        ESW0=1000.*FPVS0(T0C)     ! Saturation vapor pressure at 0C
        RFmax=1.1**Nrime          ! Maximum rime factor allowed
        RFmx1=PI*900.             ! Density near that of pure ice, 900 kg m^-3
!
!------------------------------------------------------------------------
!--------------- Constants passed through argument list -----------------
!------------------------------------------------------------------------
!
!--- Important parameters for self collection (autoconversion) of 
!    cloud water to rain. 
!
!-- Relative dispersion == standard deviation of droplet spectrum / mean radius
!   (see pp 1542-1543, Liu & Daum, JAS, 2004)
        RDIS=0.5  !-- relative dispersion of droplet spectrum
        BETA6=( (1.+3.*RDIS*RDIS)*(1.+4.*RDIS*RDIS)*(1.+5.*RDIS*RDIS)/  &
     &         ((1.+RDIS*RDIS)*(1.+2.*RDIS*RDIS) ) )
!-- Kappa=1.1e10 g^-2 cm^3 s^-1 after eq. (8b) on p.1105 of Liu et al. (JAS, 2006)
!   => More extensive units conversion than can be described here to go from
!      eq. (13) in Liu et al. (JAS, 2006) to what's programmed below.  Note that
!      the units used throughout the paper are in cgs units!
!
        ARAUT=1.03e19/(NCW*SQRT(NCW))
        BRAUT=DTPH*1.1E10*BETA6/NCW
!
!--- QAUT0 is the *OLD* threshold cloud content for autoconversion to rain 
!      needed for droplets to reach a diameter of 20 microns (following
!      Manton and Cotton, 1977; Banta and Hanson, 1987, JCAM).  It's no longer
!      used in this version, but the value is passed into radiation in case
!      a ball park estimate is needed.
!--- QAUT0=1.2567, 0.8378, or 0.4189 g/m**3 for droplet number concentrations
!      of 300, 200, and 100 cm**-3, respectively
!
        QAUT0=PI*RHOL*NCW*(20.E-6)**3/6.     !-- legacy
!
!--- For calculating cloud droplet radius in microns, Rcw
!
        ARcw=1.E6*(0.75/(PI*NCW*RHOL))**C1
!
!may20 - start
!
!- An explanation for the following settings assumed for "hail" or frozen drops (RF>10):
!        RH_NgC=PI*500.*5.E3
!  RHOg=500 kg m^-3, Ng=5.e3 m^-3 => "hail" content >7.85 g m^-3 for INDEXS=MDImax
!- or -
!        RH_NgC=PI*500.*1.E3
!  RHOg=500 kg m^-3, Ng=1.e3 m^-3 => "hail" content >1.57 g m^-3 for INDEXS=MDImax
!
        RH_NgC=PI*500.*1.E3                !- RHOg=500 kg m^-3, Ng=1.e3 m^-3
        RQhail=RH_NgC*(1.E-3)**3           !- Threshold "hail" content  ! becomes 1.57 g m^-3
        Bvhail=0.82*C1                     !- Exponent for Thompson graupel
        Avhail=1353.*(1./RH_NgC)**Bvhail   !- 1353 ~ constant for Thompson graupel
        RH_NgC=1.E6*(1./RH_NgC)**C1        !mar08 (convection, convert to microns)
!
!- An explanation for the following settings assumed for graupel (RF>5):
!        RH_NgT=PI*300.*10.E3
!  RHOg=300 kg m^-3, Ng=10.e3 m^-3 => "graupel" content must exceed 9.43 g m^-3 for INDEXS=MDImax
!- or -
!        RH_NgT=PI*300.*5.E3
!  RHOg=300 kg m^-3, Ng=5.e3 m^-3 => "graupel" content must exceed 4.71 g m^-3 for INDEXS=MDImax
!
        RH_NgT=PI*300.*5.E3                !- RHOg=300 kg m^-3, Ng=5.e3 m^-3
        RH_NgT=1.E6*(1./RH_NgT)**C1        !mar08 (transition, convert to microns)
!may20 - end
!
!--- For calculating snow optical depths by considering bulk density of
!      snow based on emails from Q. Fu (6/27-28/01), where optical 
!      depth (T) = 1.5*SWP/(Reff*DENS), SWP is snow water path, Reff 
!      is effective radius, and DENS is the bulk density of snow.
!
!    SWP (kg/m**2)=(1.E-3 kg/g)*SWPrad, SWPrad in g/m**2 used in radiation
!    T = 1.5*1.E3*SWPrad/(Reff*DENS)
!  
!    See derivation for MASSI(INDEXS), note equal to RHO*QSNOW/NSNOW
!
!      SDENS=1.5e3/DENS, DENS=MASSI(INDEXS)/[PI_E*(1.E-6*INDEXS)**3]
!
        DO I=MDImin,MDImax
          SDENS(I)=PI_E*1.5E-15*FLOAT(I*I*I)/MASSI(I)
        ENDDO
!
        Thour_print=-DTPH/3600.


      ENDIF  ! Allowed_to_read

      RETURN
!
!-----------------------------------------------------------------------
!
9061 CONTINUE
      WRITE( errmess , '(A,I4)' )                                        &
       'module_mp_hwrf: error opening ETAMPNEW_DATA on unit '          &
     &, etampnew_unit1
      CALL wrf_error_fatal(errmess)
!
!-----------------------------------------------------------------------
      END SUBROUTINE fer_hires_init
!
      SUBROUTINE MY_GROWTH_RATES (DTPH)
!
!--- Below are tabulated values for the predicted mass of ice crystals
!    after 600 s of growth in water saturated conditions, based on 
!    calculations from Miller and Young (JAS, 1979).  These values are
!    crudely estimated from tabulated curves at 600 s from Fig. 6.9 of
!    Young (1993).  Values at temperatures colder than -27C were 
!    assumed to be invariant with temperature.  
!
!--- Used to normalize Miller & Young (1979) calculations of ice growth
!    over large time steps using their tabulated values at 600 s.
!    Assumes 3D growth with time**1.5 following eq. (6.3) in Young (1993).
!
      IMPLICIT NONE
!
      REAL,INTENT(IN) :: DTPH
!
      REAL  DT_ICE
      REAL,DIMENSION(35) :: MY_600
!WRF
!
!-----------------------------------------------------------------------
      DATA MY_600 /                                                     &
     & 5.5e-8, 1.4E-7, 2.8E-7, 6.E-7, 3.3E-6,                           & 
     & 2.E-6, 9.E-7, 8.8E-7, 8.2E-7, 9.4e-7,                            & 
     & 1.2E-6, 1.85E-6, 5.5E-6, 1.5E-5, 1.7E-5,                         & 
     & 1.5E-5, 1.E-5, 3.4E-6, 1.85E-6, 1.35E-6,                         & 
     & 1.05E-6, 1.E-6, 9.5E-7, 9.0E-7, 9.5E-7,                          & 
     & 9.5E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7,                              & 
     & 9.E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7 /        ! -31 to -35 deg C
!
!-----------------------------------------------------------------------
!
      DT_ICE=(DTPH/600.)**1.5
      MY_GROWTH=DT_ICE*MY_600*1.E-3    !-- 20090714: Convert from g to kg
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE MY_GROWTH_RATES
!
!-----------------------------------------------------------------------
!---------  Old GFS saturation vapor pressure lookup tables  -----------
!-----------------------------------------------------------------------
!
      SUBROUTINE GPVS
!     ******************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GPVS        COMPUTE SATURATION VAPOR PRESSURE TABLE
!   AUTHOR: N PHILLIPS       W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE TABLE AS A FUNCTION OF
!   TEMPERATURE FOR THE TABLE LOOKUP FUNCTION FPVS.
!   EXACT SATURATION VAPOR PRESSURES ARE CALCULATED IN SUBPROGRAM FPVSX.
!   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH
!   OF 7501 FOR TEMPERATURES RANGING FROM 180.0 TO 330.0 KELVIN.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL
!   94-12-30  IREDELL             EXPAND TABLE
!   96-02-19  HONG                ICE EFFECT
!   01-11-29  JIN                 MODIFIED FOR WRF
!
! USAGE:  CALL GPVS
!
! SUBPROGRAMS CALLED:
!   (FPVSX)  - INLINABLE FUNCTION TO COMPUTE SATURATION VAPOR PRESSURE
!
! COMMON BLOCKS:
!   COMPVS   - SCALING PARAMETERS AND TABLE FOR FUNCTION FPVS.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      IMPLICIT NONE
      real :: X,XINC,T
      integer :: JX
!----------------------------------------------------------------------
      XINC=(XMAX-XMIN)/(NX-1)
      C1XPVS=1.-XMIN/XINC
      C2XPVS=1./XINC
      C1XPVS0=1.-XMIN/XINC
      C2XPVS0=1./XINC
!
      DO JX=1,NX
        X=XMIN+(JX-1)*XINC
        T=X
        TBPVS(JX)=FPVSX(T)
        TBPVS0(JX)=FPVSX0(T)
      ENDDO
! 
      END SUBROUTINE GPVS
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
                     REAL   FUNCTION FPVS(T)
!-----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    FPVS        COMPUTE SATURATION VAPOR PRESSURE
!   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.
!   A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE
!   COMPUTED IN GPVS. SEE DOCUMENTATION FOR FPVSX FOR DETAILS.
!   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.
!   THE INTERPOLATION ACCURACY IS ALMOST 6 DECIMAL PLACES.
!   ON THE CRAY, FPVS IS ABOUT 4 TIMES FASTER THAN EXACT CALCULATION.
!   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
!   94-12-30  IREDELL             EXPAND TABLE
!   96-02-19  HONG                ICE EFFECT
!   01-11-29  JIN                 MODIFIED FOR WRF
!
! USAGE:   PVS=FPVS(T)
!
!   INPUT ARGUMENT LIST:
!     T        - REAL TEMPERATURE IN KELVIN
!
!   OUTPUT ARGUMENT LIST:
!     FPVS     - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!$$$
      IMPLICIT NONE
      real,INTENT(IN) :: T
      real XJ
      integer :: JX
!-----------------------------------------------------------------------
      IF (T>=XMIN .AND. T<=XMAX) THEN
        XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
        JX=MIN(XJ,NX-1.)
        FPVS=TBPVS(JX)+(XJ-JX)*(TBPVS(JX+1)-TBPVS(JX))
      ELSE IF (T>XMAX) THEN
!-- Magnus Tetens formula for water saturation (Murray, 1967)
!   (saturation vapor pressure in kPa)
        FPVS=0.61078*exp(17.2694*(T-273.16)/(T-35.86))
      ELSE 
!-- Magnus Tetens formula for ice saturation(Murray, 1967)
!   (saturation vapor pressure in kPa)
        FPVS=0.61078*exp(21.8746*(T-273.16)/(T-7.66))
      ENDIF
!
      END FUNCTION FPVS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                     REAL FUNCTION FPVS0(T)
!-----------------------------------------------------------------------
      IMPLICIT NONE
      real,INTENT(IN) :: T
      real :: XJ1
      integer :: JX1
!-----------------------------------------------------------------------
      IF (T>=XMIN .AND. T<=XMAX) THEN
        XJ1=MIN(MAX(C1XPVS0+C2XPVS0*T,1.),FLOAT(NX))
        JX1=MIN(XJ1,NX-1.)
        FPVS0=TBPVS0(JX1)+(XJ1-JX1)*(TBPVS0(JX1+1)-TBPVS0(JX1))
      ELSE
!-- Magnus Tetens formula for water saturation (Murray, 1967)
!   (saturation vapor pressure in kPa)
        FPVS0=0.61078*exp(17.2694*(T-273.16)/(T-35.86))
      ENDIF
!
      END FUNCTION FPVS0
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
                    REAL FUNCTION FPVSX(T)
!-----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    FPVSX       COMPUTE SATURATION VAPOR PRESSURE
!   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: EXACTLY COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE.
!   THE WATER MODEL ASSUMES A PERFECT GAS, CONSTANT SPECIFIC HEATS
!   FOR GAS AND LIQUID, AND NEGLECTS THE VOLUME OF THE LIQUID.
!   THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT
!   OF CONDENSATION WITH TEMPERATURE.  THE ICE OPTION IS NOT INCLUDED.
!   THE CLAUSIUS-CLAPEYRON EQUATION IS INTEGRATED FROM THE TRIPLE POINT
!   TO GET THE FORMULA
!       PVS=PSATK*(TR**XA)*EXP(XB*(1.-TR))
!   WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS
!   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
!   94-12-30  IREDELL             EXACT COMPUTATION
!   96-02-19  HONG                ICE EFFECT 
!   01-11-29  JIN                 MODIFIED FOR WRF
!
! USAGE:   PVS=FPVSX(T)
! REFERENCE:   EMANUEL(1994),116-117
!
!   INPUT ARGUMENT LIST:
!     T        - REAL TEMPERATURE IN KELVIN
!
!   OUTPUT ARGUMENT LIST:
!     FPVSX    - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!$$$
      IMPLICIT NONE
!-----------------------------------------------------------------------
       real, parameter :: TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2   &
      ,         CLIQ=4.1855E+3,CVAP= 1.8460E+3                          &
      ,         CICE=2.1060E+3,HSUB=2.8340E+6
!
      real, parameter :: PSATK=PSAT*1.E-3
      real, parameter :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
      real, parameter :: DLDTI=CVAP-CICE                                &
      ,                  XAI=-DLDTI/RV,XBI=XAI+HSUB/(RV*TTP)
      real T,TR
!-----------------------------------------------------------------------
      TR=TTP/T
!
      IF(T.GE.TTP)THEN
        FPVSX=PSATK*(TR**XA)*EXP(XB*(1.-TR))
      ELSE
        FPVSX=PSATK*(TR**XAI)*EXP(XBI*(1.-TR))
      ENDIF
! 
      END FUNCTION FPVSX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                 REAL   FUNCTION FPVSX0(T)
!-----------------------------------------------------------------------
      IMPLICIT NONE
      real,parameter :: TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2     &
      ,         CLIQ=4.1855E+3,CVAP=1.8460E+3                           &
      ,         CICE=2.1060E+3,HSUB=2.8340E+6
      real,PARAMETER :: PSATK=PSAT*1.E-3
      real,PARAMETER :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
      real,PARAMETER :: DLDTI=CVAP-CICE                                 &
      ,                 XAI=-DLDT/RV,XBI=XA+HSUB/(RV*TTP)
      real :: T,TR
!-----------------------------------------------------------------------
      TR=TTP/T
      FPVSX0=PSATK*(TR**XA)*EXP(XB*(1.-TR))
!
      END FUNCTION FPVSX0
!
      END MODULE module_mp_fer_hires
