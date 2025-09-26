     MODULE MODULE_BL_MFSHCONVPBL




USE MODULE_MODEL_CONSTANTS

REAL,PARAMETER :: XG      = 9.80665

REAL,PARAMETER :: XP00= 1.E5               ! Reference pressure
!
!
REAL,PARAMETER :: XMD= 28.9644E-3
REAL,PARAMETER :: XMV= 18.0153E-3            ! Molar mass of dry air and molar mass of vapor
REAL,PARAMETER :: XRD=R_D
REAL,PARAMETER :: XRV=R_V        ! Gaz constant for dry air, gaz constant for vapor
REAL,PARAMETER :: XCPD=7.* XRD /2.
REAL,PARAMETER :: XCPV=4.* XRV          ! Cpd (dry air), Cpv (vapor)
REAL,PARAMETER :: XCL= 4.218E+3
REAL,PARAMETER :: XCI= 2.106E+3      ! Cl (liquid), Ci (ice)
REAL,PARAMETER :: XTT= 273.16        ! Triple point temperature
REAL,PARAMETER :: XLVTT=2.5008E+6              ! Vaporization heat constant
REAL,PARAMETER :: XLSTT=2.8345E+6   ! Sublimation heat constant
                               !  temperature
REAL,PARAMETER :: XGAMW = (XCL - XCPV) / XRV! Constants for saturation vapor
REAL,PARAMETER :: XBETAW= (XLVTT/XRV) + (XGAMW * XTT)
!The use of intrinsics in an initialization expressions is a F2003 feature.
!For backward compatibility, hard coded Log(644.11) & Log(XTT) here
!REAL,PARAMETER :: XALPW= LOG(611.14) + (XBETAW /XTT) + (XGAMW *LOG(XTT))
REAL,PARAMETER :: LOG_611_14 = 6.415326
REAL,PARAMETER :: LOG_XTT    = 5.610058
REAL,PARAMETER :: XALPW= LOG_611_14 + (XBETAW /XTT) + (XGAMW *LOG_XTT)
                                !  pressure  function
REAL,PARAMETER :: XGAMI  = (XCI - XCPV) / XRV
REAL,PARAMETER :: XBETAI = (XLSTT/XRV) + (XGAMI * XTT)
!REAL,PARAMETER :: XALPI  = LOG(611.14) + (XBETAI /XTT) + (XGAMI *LOG(XTT))
REAL,PARAMETER :: XALPI  = LOG_611_14 + (XBETAI /XTT) + (XGAMI *LOG_XTT)
REAL,PARAMETER :: XLINI = 0.32


REAL, PARAMETER :: XALP_PERT   = 0.3  ! coefficient for the perturbation of
                   ! theta_l and r_t at the first level of
                   ! the updraft
REAL, PARAMETER ::XABUO       = 1.   ! coefficient of the buoyancy term in the w_up equation
REAL, PARAMETER ::XBENTR      = 1.   ! coefficient of the entrainment term in thew_up equation
REAL, PARAMETER ::XBDETR      = 0.   ! coefficient of the detrainment term in thew_up equation


REAL, PARAMETER ::XCMF        = 0.065! coefficient for the mass flux at the firstlevel 0.065
                                ! of the updraft (closure) XCMF        = 0.065
REAL, PARAMETER ::XENTR_DRY   = 0.55 ! coefficient for entrainment in dry part XENTR_DRY   = 0.55
REAL, PARAMETER ::XDETR_DRY   = 10.  ! coefficient for detrainment in dry part XDETR_DRY   = 10.
REAL, PARAMETER ::XDETR_LUP   = 1.0   !  coefficient for detrainment in dry part XDETR_LUP   = 1.

REAL, PARAMETER ::XENTR_MF    = 0.035! entrainment constant (m/Pa) = 0.2 (m)  XENTR_MF    = 0.035
REAL, PARAMETER ::XCRAD_MF    = 50.  ! cloud radius in cloudy part
REAL, PARAMETER ::XKCF_MF     = 2.75 ! coefficient for cloud fraction
REAL, PARAMETER ::XKRC_MF     = 1.   ! coefficient for convective rc
REAL, PARAMETER ::XTAUSIGMF   = 600.
REAL, PARAMETER ::XPRES_UV    = 0.5  ! coefficient for pressure term in wind mixing
!
REAL, PARAMETER ::XFRAC_UP_MAX= 0.33 ! maximum Updraft fraction


!

     CONTAINS


      SUBROUTINE MFSHCONVPBL (DT,STEPBL,HT,DZ           &
                    ,RHO,PMID,PINT,TH,EXNER             &
                    ,QV, QC, U, V                       &
                    ,HFX, QFX, TKE                      &
                    ,RUBLTEN,RVBLTEN,RTHBLTEN           &
                    ,RQVBLTEN,RQCBLTEN                  &
                    ,IDS,IDE,JDS,JDE,KDS,KDE            &
                    ,IMS,IME,JMS,JME,KMS,KME            &
                    ,ITS,ITE,JTS,JTE,KTS,KTE,KRR        &
                    ,MASSFLUX_EDKF, ENTR_EDKF, DETR_EDKF & 
                    ,THL_UP, THV_UP, RT_UP, RV_UP       &
                    ,RC_UP, U_UP, V_UP, FRAC_UP, RC_MF  &
                    ,WTHV,PLM_BL89 )                   
 
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: KRR

      INTEGER,INTENT(IN) :: STEPBL

      REAL,INTENT(IN) :: DT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HT, HFX, QFX
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ          &
     &                                                     ,EXNER       &
     &                                                     ,PMID,PINT   &
     &                                                     ,QV,QC,RHO   &
     &                                                     ,TH,U,V,TKE   
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RQCBLTEN,RQVBLTEN       &
     &                                        ,RTHBLTEN                                &
     &                                        ,RUBLTEN,RVBLTEN        
  
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),OPTIONAL,INTENT(OUT) ::              &
     &                                         MASSFLUX_EDKF, ENTR_EDKF, DETR_EDKF &
     &                                        ,THL_UP, THV_UP, RT_UP, RV_UP        &
     &                                        ,RC_UP, U_UP, V_UP, FRAC_UP

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),OPTIONAL,INTENT(INOUT) ::            &
     &                                         RC_MF

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) ::   WTHV
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) ::   PLM_BL89

!
!Local declaration

INTEGER   :: KRRL      ! number of liquid water var.
INTEGER   :: KRRI      ! number of ice water var.
LOGICAL   :: OMIXUV    ! True if mixing of momentum

REAL   :: PIMPL_MF     ! degre of implicitness
REAL   ::  PTSTEP   ! Dynamical timestep 
REAL   ::  PTSTEP_MET! Timestep for meteorological variables                        

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)   :: PZZ       !  Height at the flux point

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)   :: PZZM      !  Height at the mass point

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)   :: PDZZ      ! depth between mass levels 
 
REAL, DIMENSION(ITS:ITE,JTS:JTE)   ::  PSFTH,PSFRV
                                            ! normal surface fluxes of theta,rv
!
!    prognostic variables at t- deltat
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PPABSM      ! Pressure at mass point
!REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PPABSF    ! Pressure at flux point
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PEXNM       ! Exner function at t-dt
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PRHODREF    ! dry density of the
                                                     ! reference state
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PRHODJ    ! dry density of the

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PTKEM       ! TKE
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE) ::  PUM,PVM     ! momentum

!   thermodynamical variables which are transformed in conservative var.
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     ::  PTHM       ! pot. temp. = PTHLM in turb.f90
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE,KRR) ::  PRM        ! water species 

REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     ::  PRUS,PRVS,PRTHS 
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE,KRR) ::  PRRS

! For diagnostic output
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PEMF, PENTR, PDETR
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PTHL_UP, PRT_UP, PRV_UP, PRC_UP
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PU_UP, PV_UP, PTHV_UP, PFRAC_UP                   
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PRC_MF
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: WTHV_MF
REAL, DIMENSION(ITS:ITE,JTS:JTE,KTS:KTE)     :: PLM_MF

INTEGER :: I,J,K  ! loop variables
      
! Transform WRF Variable to input of mass flux scheme

DO J=JTS,JTE
   DO K=KTS,KTE
      DO I=ITS,ITE
        IF (K==KTS) PZZ(I,J,K)=0.
        PEMF(I,J,K)=0.
        PENTR(I,J,K)=0.
        PDETR(I,J,K)=0.
        PTHL_UP(I,J,K)=0.
        PTHV_UP(I,J,K)=0.
        PRT_UP(I,J,K)=0.
        PRV_UP(I,J,K)=0.
        PRC_UP(I,J,K)=0.
        PU_UP(I,J,K)=0.
        PV_UP(I,J,K)=0.
        PFRAC_UP(I,J,K)=0.
        WTHV_MF(I,J,K)=0.
        PTHM(I,J,K)=TH(I,K,J)
        PTKEM(I,J,K)=TKE(I,K,J)
        PRM(I,J,K,1)=QV(I,K,J)-RC_MF(I,K,J)
        PRM(I,J,K,2)=RC_MF(I,K,J)
        PUM(I,J,K)=U(I,K,J)
        PVM(I,J,K)=V(I,K,J)
        PRHODREF(I,J,K)=RHO(I,K,J)/(1.+QV(I,K,J))
        PEXNM(I,J,K)=EXNER(I,K,J)
        PPABSM(I,J,K)=PMID(I,K,J)
        IF (K/=KTE) THEN
            PZZ(I,J,K+1)=PZZ(I,J,K)+DZ(I,K,J)
            PZZM(I,J,K)=0.5*(PZZ(I,J,K+1)+PZZ(I,J,K)) ! z at mass point
        ELSE
            PZZM(I,J,K)=PZZ(I,J,K)+0.5*DZ(I,K-1,J) ! z at mass point
        ENDIF
        IF (K==KTS) THEN
           PDZZ(I,J,K)=2*(PZZM(I,J,K))      
        ELSE
           PDZZ(I,J,K)=PZZM(I,J,K)-PZZM(I,J,K-1)      
        ENDIF
     
        PRHODJ(I,J,K)=PRHODREF(I,J,K)*DZ(I,K,J)
  

      ENDDO
   ENDDO
ENDDO

! fill the kte+1 level
PTHM(:,:,KTE)=PTHM(:,:,KTE-1)
PTKEM(:,:,KTE)=PTKEM(:,:,KTE-1)
PRM(:,:,KTE,1)=PRM(:,:,KTE-1,1)
PRM(:,:,KTE,2)=PRM(:,:,KTE-1,2)
PUM(:,:,KTE)=PUM(:,:,KTE-1)
PVM(:,:,KTE)=PVM(:,:,KTE-1)
PRHODREF(:,:,KTE)=PRHODREF(:,:,KTE-1)
PEXNM(:,:,KTE)=PEXNM(:,:,KTE-1)
PPABSM(:,:,KTE)=PPABSM(:,:,KTE-1)
PRHODJ(:,:,KTE)=PRHODJ(:,:,KTE-1)

PSFTH(:,:)=HFX(ITS:ITE,JTS:JTE)/(PRHODREF(:,:,KTS)*XCPD)
PSFRV(:,:)=QFX(ITS:ITE,JTS:JTE)/(PRHODREF(:,:,KTS))



! Assign some variables

OMIXUV=.FALSE.
KRRL=1 !Qc is managed
KRRI=0 !Qi not
PIMPL_MF=0.
PTSTEP=DT*STEPBL
PTSTEP_MET=PTSTEP

      CALL MFSHCONVPBL_CORE(KRR,KRRL,KRRI,                            &
                OMIXUV,                                               &
                PIMPL_MF,PTSTEP,PTSTEP_MET,                           &
                PDZZ, PZZ,                                            &
                PRHODJ, PRHODREF,                                     &
                PPABSM, PEXNM,                                        &
                PSFTH,PSFRV,                                          &
                PTHM,PRM,PUM,PVM,PTKEM,                               &
                PRTHS,PRRS,PRUS,PRVS,PEMF, PENTR, PDETR,              &
                PTHL_UP, PRT_UP, PRV_UP, PRC_UP,                      &
                PU_UP, PV_UP, PTHV_UP, PFRAC_UP, PRC_MF, WTHV_MF,PLM_MF  )


DO J=JTS,JTE
   DO K=KTS,KTE
      DO I=ITS,ITE
         RQCBLTEN(I,K,J)=PRRS(I,J,K,2)       
         RQVBLTEN(I,K,J)=PRRS(I,J,K,1)       
         RTHBLTEN(I,K,J)=PRTHS(I,J,K)                
         RUBLTEN(I,K,J)=PRUS(I,J,K)
         RVBLTEN(I,K,J)=PRVS(I,J,K)
         WTHV(I,K,J)=WTHV_MF(I,J,K)
         PLM_BL89(I,K,J)=PLM_MF(I,J,K)
      ENDDO
   ENDDO
ENDDO

      IF ( PRESENT(MASSFLUX_EDKF) ) THEN
         DO J=JTS,JTE
            DO K=KTS,KTE
               DO I=ITS,ITE
                  MASSFLUX_EDKF(I,K,J)=PEMF(I,J,K)
                  ENTR_EDKF(I,K,J)=PENTR(I,J,K)
                  DETR_EDKF(I,K,J)=PDETR(I,J,K)
                  THL_UP(I,K,J)=PTHL_UP(I,J,K)
                  THV_UP(I,K,J)=PTHV_UP(I,J,K)
                  RT_UP(I,K,J)=PRT_UP(I,J,K)
                  RV_UP(I,K,J)=PRV_UP(I,J,K)
                  RC_UP(I,K,J)=PRC_UP(I,J,K)
                  U_UP(I,K,J)=PU_UP(I,J,K)
                  V_UP(I,K,J)=PV_UP(I,J,K)
                  FRAC_UP(I,K,J)=PFRAC_UP(I,J,K)
                  RC_MF(I,K,J)=PRC_MF(I,J,K)
               ENDDO
            ENDDO
         ENDDO
      ENDIF


END SUBROUTINE MFSHCONVPBL

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   WRAPPER from WRF to MASS FLUX SCHEME 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
  
      SUBROUTINE MFSHCONVPBL_CORE(KRR,KRRL,KRRI,                            &
                OMIXUV,                       &
                PIMPL_MF,PTSTEP,PTSTEP_MET,                 &
                PDZZ, PZZ,                                            &
                PRHODJ, PRHODREF,                                     &
                PPABSM, PEXNM,                                        &
                PSFTH,PSFRV,                                          &
                PTHM,PRM,PUM,PVM,PTKEM,                               &
                PRTHS,PRRS,PRUS,PRVS,  PEMF, PENTR, PDETR,            &
                PTHL_UP, PRT_UP, PRV_UP, PRC_UP,                      &
                PU_UP, PV_UP, PTHV_UP, PFRAC_UP,  PRC_MF,             &
                PFLXZTHVMF,PLM       )
!!
!!****  *MFSHCONVPBL_CORE* - Interfacing routine
!!

!! --------------------------------------------------------------------------
!


IMPLICIT NONE

INTEGER,                INTENT(IN)   :: KRR          ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL         ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI         ! number of ice water var.
LOGICAL,                INTENT(IN)   :: OMIXUV    ! True if mixing of momentum
             
REAL,                   INTENT(IN)   :: PIMPL_MF     ! degre of implicitness
REAL,                 INTENT(IN)     ::  PTSTEP   ! Dynamical timestep 
REAL,                 INTENT(IN)     ::  PTSTEP_MET! Timestep for meteorological variables                        
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PZZ         ! Height of flux point
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PDZZ        ! Metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PRHODJ      ! dry density * Grid size
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PRHODREF    ! dry density of the
                                                     ! reference state
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PPABSM      ! Pressure at time t-1
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PEXNM       ! Exner function at t-dt

REAL, DIMENSION(:,:),   INTENT(IN)   ::  PSFTH,PSFRV ! normal surface fluxes of theta and Rv 
REAL, DIMENSION(:,:,:), INTENT(IN)   ::  PTHM        ! Theta at t-dt
REAL, DIMENSION(:,:,:,:), INTENT(IN) ::  PRM         ! water var. at t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PUM,PVM     ! wind components at t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTKEM       ! tke at t-dt


REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PRUS,PRVS,PRTHS ! Meso-NH sources
REAL, DIMENSION(:,:,:,:), INTENT(OUT) ::  PRRS 

REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PEMF, PENTR, PDETR
REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PTHL_UP, PRT_UP, PRV_UP, PRC_UP 
REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PU_UP, PV_UP, PTHV_UP, PFRAC_UP
REAL, DIMENSION(:,:,:), INTENT(INOUT) ::  PRC_MF 
REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PFLXZTHVMF 
REAL, DIMENSION(:,:,:), INTENT(OUT) ::  PLM
  

!
!                     0.2  Declaration of local variables
!
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::     &
          ZEXN,ZCPH,                                      &
          PRV,PRL,PTH,                                    &
          ZTM,                                            &  ! Temperature at t-dt
          ZLVOCPEXN,                                      &  !
          ZCF_MF,                                         & 
          ZLSOCPEXN,                                      &  !
          ZAMOIST,                                        &  !
          ZATHETA,                                        &  !
          ZTHLM,                                          &  !
          ZRTM,                                           &  !
          ZTHVM,ZTHVREF,ZUMM,ZVMM,                        &  !
          ZRI_UP,ZW_UP,                                   & ! 
          ZEMF_O_RHODREF,                                 & ! entrainment/detrainment
          ZTHLDT,ZRTDT,ZUDT,ZVDT,                         & ! tendencies
          ZFLXZTHMF,ZFLXZRMF,ZFLXZUMF,ZFLXZVMF   ! fluxes


INTEGER,DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2))  :: IKLCL,IKETL,IKCTL 
                    
INTEGER :: IKU, IKB, IKE  
INTEGER :: JI,JJ,JK,JSV                          ! Loop counters
INTEGER :: IRESP    ! error code


!------------------------------------------------------------------------

!!! 1. Initialisation

! Internal Domain
IKU=SIZE(PTHM,3)
IKB=1              ! Modif WRF JP
IKE=IKU-1


! number of scalar var

  ZUMM=PUM   !Modif WRF JP
  ZVMM=PVM   !Modif WRF JP

! Thermodynamics functions

  CALL COMPUTE_FUNCTION_THERMO_MF( KRR,KRRL,KRRI,                   &
                                   PTHM,PRM,PEXNM,PPABSM,           &
                                   ZTM,ZLVOCPEXN,ZLSOCPEXN,         &
                                   ZAMOIST,ZATHETA                  )



! Conservative variables at t-dt

  CALL THL_RT_FROM_TH_R_MF( KRR,KRRL,KRRI,                         &
                                  PTHM, PRM, ZLVOCPEXN, ZLSOCPEXN,       &
                                  ZTHLM, ZRTM                            )



! Virtual potential temperature at t-dt

ZTHVM(:,:,:) = PTHM(:,:,:)*((1.+XRV / XRD *PRM(:,:,:,1))/(1.+ZRTM(:,:,:))) 


       ZTHVREF=XG/ZTHVM
       CALL BL89(PZZ,PDZZ,ZTHVREF,ZTHLM,KRR, &
                 PRM,PTKEM,PLM)

!!! 2. Compute updraft


      CALL UPDRAFT_SOPE (KRR,KRRL,KRRI,OMIXUV,                     &
                         PZZ,PDZZ,PSFTH,PSFRV,PPABSM,PRHODREF,     &
                         PTKEM,PTHM,PRM,ZTHLM,ZRTM,ZUMM,ZVMM,      &
                         PTHL_UP,PRT_UP,PRV_UP,PU_UP,PV_UP,        &
                         PRC_UP,ZRI_UP,PTHV_UP,ZW_UP,PFRAC_UP,PEMF,&
                         PDETR,PENTR,IKLCL,IKETL,IKCTL )

!!! 3. Compute fluxes of conservative variables and their divergence = tendency

       ZEMF_O_RHODREF=PEMF/PRHODREF
       CALL MF_TURB(OMIXUV, PIMPL_MF, PTSTEP,PTSTEP_MET,       &
                PDZZ, PRHODJ, ZTHLM,ZTHVM,ZRTM,ZUMM,ZVMM,           &
                ZTHLDT,ZRTDT,ZUDT,ZVDT,                            &
                ZEMF_O_RHODREF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP,&
                ZFLXZTHMF,PFLXZTHVMF,ZFLXZRMF,ZFLXZUMF,ZFLXZVMF   )

               
!!! 5. Compute diagnostic convective cloud fraction and content
! ! !  ONLY liquid cloud implemented (yet)

      CALL COMPUTE_MF_CLOUD(KRRL,ZTHLM,PRC_UP,PFRAC_UP,PDZZ,IKLCL,  &
                            PRC_MF,ZCF_MF                      )


!!! 6. Compute tendency terms for pronostic variables

ZEXN(:,:,:)=(PPABSM(:,:,:)/XP00) ** (XRD/XCPD)
!


PRV(:,:,:)=PRM(:,:,:,1)-PRC_MF(:,:,:)
PRL(:,:,:)=PRC_MF(:,:,:)
      !       2.1 Cph
ZCPH(:,:,:)=XCPD+ XCPV * PRV(:,:,:)+ XCL * PRL(:,:,:) 
      
PTH(:,:,:)=(ZTHLM(:,:,:)+ZTHLDT(:,:,:))+(XLVTT/(ZCPH*ZEXN(:,:,:))*PRL(:,:,:))          

PRTHS(:,:,:)  = ZTHLDT(:,:,:)
PRTHS(:,:,:)  = (PTH(:,:,:)-PTHM(:,:,:))/PTSTEP_MET
PRRS(:,:,:,2) = (PRC_MF-PRM(:,:,:,2))/PTSTEP_MET
PRRS(:,:,:,1) = ZRTDT(:,:,:)-PRRS(:,:,:,2)

PRTHS(:,:,:)  = ZTHLDT(:,:,:)
PRRS(:,:,:,1) = ZRTDT(:,:,:)
PRRS(:,:,:,2) = 0
PRUS(:,:,:)   = ZUDT(:,:,:)
PRVS(:,:,:)   = ZVDT(:,:,:) 
  

END SUBROUTINE MFSHCONVPBL_CORE 


!     ###################################################################
      SUBROUTINE COMPUTE_BL89_ML(PDZZ2D, &
             PTKEM2D,PG_O_THVREF2D,PVPT,KK,OUPORDN,PLWORK)
!     ###################################################################
!!
!!     COMPUTE_BL89_ML routine to:
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!              ------------
!USE MODD_CST
!
!
IMPLICIT NONE
!
!          0.1 arguments
!
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDZZ2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PTKEM2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PG_O_THVREF2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PVPT
INTEGER,                INTENT(IN)  :: KK
LOGICAL,                INTENT(IN)  :: OUPORDN
REAL, DIMENSION(:),     INTENT(OUT) :: PLWORK

!          0.2 Local variable
!
REAL, DIMENSION(SIZE(PTKEM2D,1)) :: ZLWORK1,ZLWORK2 ! Temporary mixing length
REAL, DIMENSION(SIZE(PTKEM2D,1)) :: ZINTE,ZPOTE     ! TKE and potential energy
                                                    ! between 2 levels
INTEGER :: IKB,IKE
!
REAL, DIMENSION(SIZE(PTKEM2D,1),SIZE(PTKEM2D,2)) :: ZDELTVPT,ZHLVPT                                
                      !Virtual Potential Temp at Half level and DeltaThv between
                      !2 levels
REAL, DIMENSION(SIZE(PTKEM2D,1)) :: ZTH! Potential Temp 

INTEGER :: IIJU,IKU             !Internal Domain
INTEGER :: J1D                  !horizontal loop counter
INTEGER :: JKK                  !loop counters
INTEGER :: JRR                  !moist loop counter
INTEGER :: JIJK                 !loop counters 
REAL    :: ZTEST,ZTEST0,ZTESTM  !test for vectorization
!-------------------------------------------------------------------------------------
!
!*       1.    INITIALISATION
!              --------------
IIJU=SIZE(PTKEM2D,1)
!
IKB = 1 !Modif WRF JP
IKE = SIZE(PTKEM2D,2)-1 !Modif WRF JP
IKU = SIZE(PTKEM2D,2)

ZDELTVPT(:,2:IKU)=PVPT(:,2:IKU)-PVPT(:,1:IKU-1)
ZDELTVPT(:,1)=0.

! to prevent division by zero
WHERE (ABS(ZDELTVPT(:,:))<1.E-10)
  ZDELTVPT(:,:)=1.E-10
END WHERE
!
ZHLVPT(:,2:IKU)= 0.5 * ( PVPT(:,2:IKU)+PVPT(:,1:IKU-1) )
ZHLVPT(:,1)    = PVPT(:,1)

!
!
!
!*       2.    CALCULATION OF THE UPWARD MIXING LENGTH
!              ---------------------------------------
!

IF (OUPORDN.EQV..TRUE.) THEN 
 ZINTE(:)=PTKEM2D(:,KK)
 PLWORK=0.
 ZLWORK1=0.
 ZLWORK2=0.
 ZTESTM=1.
 ZTH(:)=PVPT(:,KK)
 DO JKK=KK+1,IKE
    IF(ZTESTM > 0.) THEN
      ZTESTM=0
      DO J1D=1,IIJU
        ZTEST0=0.5+SIGN(0.5,ZINTE(J1D))
        ZPOTE(J1D) = ZTEST0*(PG_O_THVREF2D(J1D,KK)      *      &
            (ZHLVPT(J1D,JKK) - ZTH(J1D)))  * PDZZ2D(J1D,JKK) !particle keeps its temperature
        ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
        ZTESTM=ZTESTM+ZTEST0
        ZLWORK1(J1D)=PDZZ2D(J1D,JKK)
        !ZLWORK2 jump of the last reached level
        ZLWORK2(J1D)=        ( - PG_O_THVREF2D(J1D,KK) *                     &
            (  PVPT(J1D,JKK-1) - ZTH(J1D) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF2D(J1D,KK) * (PVPT(J1D,JKK-1) - ZTH(J1D)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF2D(J1D,KK)                        &
                 * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ))    ) /             &
        ( PG_O_THVREF2D(J1D,KK) * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ) 
      !
        PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D))
        ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
      END DO 
    ENDIF
  END DO 
ENDIF
!!
!*       2.    CALCULATION OF THE DOWNWARD MIXING LENGTH
!              ---------------------------------------
!

IF (OUPORDN.EQV..FALSE.) THEN 
 ZINTE(:)=PTKEM2D(:,KK)
 PLWORK=0.
 ZLWORK1=0.
 ZLWORK2=0.
 ZTESTM=1.
 ZTH(:)=PVPT(:,KK)
 DO JKK=KK,IKB,-1 
    IF(ZTESTM > 0.) THEN
      ZTESTM=0
      DO J1D=1,IIJU
        ZTEST0=0.5+SIGN(0.5,ZINTE(J1D))
        ZPOTE(J1D) = -ZTEST0*(PG_O_THVREF2D(J1D,KK)      *      &
            (ZHLVPT(J1D,JKK) - ZTH(J1D)))  * PDZZ2D(J1D,JKK) !particle keeps its temperature
        ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
        ZTESTM=ZTESTM+ZTEST0
        ZLWORK1(J1D)=PDZZ2D(J1D,JKK)
        ZLWORK2(J1D)=        ( + PG_O_THVREF2D(J1D,KK) *                     &
            (  PVPT(J1D,JKK) - ZTH(J1D) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF2D(J1D,KK) * (PVPT(J1D,JKK) - ZTH(J1D)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF2D(J1D,KK)                        &
                 * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ))    ) /             &
        ( PG_O_THVREF2D(J1D,KK) * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ) 
      !
        PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D)) 
        ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
      END DO 
    ENDIF
  END DO 
ENDIF
  
END SUBROUTINE COMPUTE_BL89_ML
!

!
!         #############################################################
          SUBROUTINE COMPUTE_ENTR_DETR(OTEST,OTESTLCL,&
                            HFRAC_ICE,PFRAC_ICE,KK,PPABSM,PZZ,PDZZ,&
                            PTHVM,PTHLM,PRTM,PW_UP2,&
                            PTHL_UP,PRT_UP,PLUP,&
                            PENTR,PDETR,PBUO_INTEG)
!         #############################################################

!!
!!***COMPUTE_ENTR_DETR* - calculates caracteristics of the updraft or downdraft
!!                       using model of the EDMF scheme 
!!
!!
!! --------------------------------------------------------------------------
!



IMPLICIT NONE
!
!                         
!*                    1.1  Declaration of Arguments
!
!

LOGICAL,DIMENSION(:),INTENT(INOUT)  :: OTEST ! test to see if updraft is running
LOGICAL,DIMENSION(:),INTENT(INOUT)  :: OTESTLCL !test of condensation 
CHARACTER*1                         :: HFRAC_ICE ! frac_ice can be compute using
                                              ! Temperature (T) or prescribed
                                              ! (Y)
REAL, DIMENSION(:), INTENT(INOUT)   :: PFRAC_ICE ! if frac_ice is prescribed
INTEGER,            INTENT(IN)      :: KK  ! level where E and D are computed     
!
!    prognostic variables at t- deltat
!
REAL, DIMENSION(:,:),   INTENT(IN) ::  PPABSM      ! Pressure at time t-1
REAL, DIMENSION(:,:),   INTENT(IN) ::  PZZ       !  Height at the flux point
REAL, DIMENSION(:,:),   INTENT(IN) ::  PDZZ       !  metrics coefficient
REAL, DIMENSION(:,:),   INTENT(IN) ::  PTHVM      ! ThetaV environment 

!
!   thermodynamical variables which are transformed in conservative var.
!
REAL, DIMENSION(:),   INTENT(IN)     ::  PTHLM     ! Thetal
REAL, DIMENSION(:),   INTENT(IN)     ::  PRTM      ! total mixing ratio 
REAL, DIMENSION(:,:), INTENT(INOUT)  ::  PW_UP2    ! Vertical velocity^2
REAL, DIMENSION(:),   INTENT(IN)     ::  PTHL_UP,PRT_UP  ! updraft properties
REAL, DIMENSION(:),   INTENT(IN)     ::  PLUP      ! LUP compute from the ground
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PENTR     ! Mass flux entrainment of the updraft
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PDETR     ! Mass flux detrainment of the updraft
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PBUO_INTEG! Integral Buoyancy
!
!
!                       1.2  Declaration of local variables
!
!

! Variables for cloudy part

REAL, DIMENSION(SIZE(PTHLM))   :: ZKIC           ! fraction of env. mass in the muxtures
REAL, DIMENSION(SIZE(PTHLM))   :: ZEPSI,ZDELTA   ! factor entrainment detrainment
REAL, DIMENSION(SIZE(PTHLM))   :: ZEPSI_CLOUD    ! factor entrainment detrainment
REAL, DIMENSION(SIZE(PTHLM))   :: ZCOEFFMF_CLOUD ! factor for compputing entr. detr.

REAL, DIMENSION(SIZE(PTHLM))   :: ZMIXTHL,ZMIXRT ! Thetal and rt in the mixtures
!
REAL, DIMENSION(SIZE(PTHLM))   :: ZTHMIX,ZTHVMIX ! Theta and Thetav  of mixtures
REAL, DIMENSION(SIZE(PTHLM))   :: ZRVMIX,ZRCMIX,ZRIMIX ! mixing ratios in mixtures

REAL, DIMENSION(SIZE(PTHLM))   :: ZTHMIX_F2     ! Theta and Thetav  of mixtures
REAL, DIMENSION(SIZE(PTHLM))   :: ZRVMIX_F2,ZRCMIX_F2,ZRIMIX_F2 ! mixing ratios in mixtures

REAL, DIMENSION(SIZE(PTHLM))   :: ZTHV_UP       ! thvup at mass point kk


REAL, DIMENSION(SIZE(PTHLM))   :: ZTHVMIX_1,ZTHVMIX_2 ! Theta and Thetav  of mixtures


! Variables for dry part

REAL, DIMENSION(SIZE(PTHLM))   :: ZBUO_INTEG,&         ! Temporary integral Buoyancy
                                  ZDZ_HALF,&           ! half-DeltaZ between 2 flux points
                                  ZDZ_STOP,&           ! Exact Height of the LCL 
                                  ZTHV_MINUS_HALF,&    ! Thv at flux point(kk)  
                                  ZTHV_PLUS_HALF,&     ! Thv at flux point(kk+1)
                                  ZCOEFF_MINUS_HALF,&  ! Variation of Thv between mass points kk-1 and kk
                                  ZCOEFF_PLUS_HALF,&   ! Variation of Thv between mass points kk and kk+1
                                  ZCOTHVU_MINUS_HALF,& ! Variation of Thvup between flux point kk and mass point kk
                                  ZCOTHVU_PLUS_HALF,&  ! Variation of Thvup between mass point kk and flux point kk+1    
                                  ZW2_HALF             ! w**2 at mass point KK  

REAL, DIMENSION(SIZE(PTHLM))   :: ZCOPRE_MINUS_HALF,&  ! Variation of pressure between mass points kk-1 and kk
                                  ZCOPRE_PLUS_HALF,&   ! Variation of pressure between mass points kk and kk+1
                                  ZPRE_MINUS_HALF,&    ! pressure at flux point kk
                                  ZPRE_PLUS_HALF,&     ! pressure at flux point kk+1
                                  ZTHV_UP_F1,&         ! thv_up at flux point kk
                                  ZTHV_UP_F2           ! thv_up at flux point kk+1
REAL, DIMENSION(SIZE(PTHLM))   :: ZCOEFF_QSAT,&        ! variation of Qsat at the transition between dry part and cloudy part
                                  ZRC_ORD,&            ! 
                                  ZPART_DRY            ! part of dry part at the transition level
!
REAL, DIMENSION(SIZE(PTHLM))   :: ZQVSAT ! QV at saturation

REAL, DIMENSION(SIZE(PTHLM))   :: PT ! temperature to compute saturation vapour mixing ratio 

REAL, DIMENSION(SIZE(PTHVM,1),SIZE(PTHVM,2))   ::ZG_O_THVREF
!
LOGICAL, DIMENSION(SIZE(OTEST,1)) :: GTEST_LOCAL_LCL,& ! true if LCL found between flux point KK and mass point KK  
                                     GTEST_LOCAL_LCL2  ! true if LCL found between mass point KK and flux point KK+1
!
REAL     :: ZRDORV       ! RD/RV
REAL     :: ZRVORD       ! RV/RD
INTEGER  :: ILON, ITEST, IKB         !


!----------------------------------------------------------------------------------
                        
!                1.3 Initialisation
!                ------------------
  IKB=1 ! modif WRF JP    

  
  ZRDORV   = XRD / XRV   !=0.622
  ZRVORD   = XRV / XRD   !=1.607
  ZG_O_THVREF=XG/PTHVM
  
  ZCOEFF_QSAT=0.
  ZRC_ORD=0.
  ZPART_DRY=1.
  GTEST_LOCAL_LCL=.FALSE.
  ZDZ_HALF(:) = (PZZ(:,KK+1)-PZZ(:,KK))/2.
  ZDZ_STOP(:) = ZDZ_HALF(:)
 
  ZKIC(:)=0.1  ! starting value for critical mixed fraction for CLoudy Part


!                Computation of KIC
!                ---------------------

!        2.1    Compute critical mixed fraction by estimating unknown  
!               T^mix r_c^mix and r_i^mix from thl^mix and r_t^mix
!               We determine the zero crossing of the linear curve
!               evaluating the derivative using ZMIXF=0.1.
!               -----------------------------------------------------

  ZMIXTHL(:) = ZKIC(:) * PTHLM(:)+(1. - ZKIC(:))*PTHL_UP(:)
  ZMIXRT(:)  = ZKIC(:) * PRTM(:)+(1. - ZKIC(:))*PRT_UP(:)

! MIXTURE FOR CLOUDY PART
  !  Compute pressure at flux level KK and at flux Level KK+1  


IF (KK==IKB) THEN !MODIF WRF JP
  ZCOPRE_MINUS_HALF(:) = 0. !MODIF WRF JP
ELSE!MODIF WRF JP
  ZCOPRE_MINUS_HALF(:) = ((PPABSM(:,KK)-PPABSM(:,KK-1))/PDZZ(:,KK))
ENDIF!MODIF WRF JP   
  ZCOPRE_PLUS_HALF(:) = ((PPABSM(:,KK+1)-PPABSM(:,KK))/PDZZ(:,KK+1))
  
IF (KK==IKB) THEN !MODIF WRF JP   
  ZPRE_MINUS_HALF(:)= PPABSM(:,KK)
ELSE!MODIF WRF JP
  ZPRE_MINUS_HALF(:)= ZCOPRE_MINUS_HALF*0.5*(PZZ(:,KK)-PZZ(:,KK-1))+PPABSM(:,KK-1)
ENDIF!MODIF WRF JP   
  ZPRE_PLUS_HALF(:) = ZCOPRE_PLUS_HALF*0.5*(PZZ(:,KK+1)-PZZ(:,KK))+PPABSM(:,KK)  


  !  Compute non cons. var. of mixture at the mass level
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               PPABSM(:,KK),ZMIXTHL,ZMIXRT,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX)
             

  ! Compute theta_v of mixture at mass level KK for KF90        
  ZTHVMIX_1(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+ZMIXRT(:))

  !  Compute non cons. var. of mixture at the flux level KK+1
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               ZPRE_PLUS_HALF,ZMIXTHL,ZMIXRT,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX)
             

  ! compute theta_v of mixture at the flux level KK+1 for KF90       
  ZTHVMIX_2(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+ZMIXRT(:))


!        2.1    Compute critical mixed fraction by estimating unknown  
!               T^mix r_c^mix and r_i^mix from thl^mix and r_t^mix
!               We determine the zero crossing of the linear curve
!               evaluating the derivative using ZMIXF=0.1.
!               -----------------------------------------------------


! THV_UP FOR DRY PART
  ! Compute theta_v of updraft at flux level KK                 
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               ZPRE_MINUS_HALF,PTHL_UP,PRT_UP,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX)
  ZTHV_UP_F1(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+PRT_UP(:))
 
  ! Compute theta_v of updraft at mass level KK                 
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               PPABSM(:,KK),PTHL_UP,PRT_UP,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX)            
  ZTHV_UP(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+PRT_UP(:))

  ! Compute theta_v of updraft at flux level KK+1                   
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,&
               ZPRE_PLUS_HALF,PTHL_UP,PRT_UP,&
               ZTHMIX_F2,ZRVMIX_F2,ZRCMIX_F2,ZRIMIX_F2)
  ZTHV_UP_F2(:) = ZTHMIX_F2(:)*(1.+ZRVORD*ZRVMIX_F2(:))/(1.+PRT_UP(:))
 
   
!
!*   2.2     Compute final values for entr. and detr. 
!            ----------------------------------------
!
! Dry PART  

 ! Computation of integral entrainment and detrainment between flux level KK
 ! and mass level KK 

  WHERE ((ZRCMIX(:)>0.).AND.(.NOT.OTESTLCL)) 
! If rc is found between flux level KK and mass level KK
! a part of dry entrainment/detrainment is defined
! the exact height of LCL is also determined
     ZCOEFF_QSAT(:) = (ZRCMIX_F2(:) - ZRCMIX(:))/ ZDZ_HALF(:)
     ZRC_ORD(:) = ZRCMIX(:) - ZCOEFF_QSAT(:) * ZDZ_HALF(:)
     ZDZ_STOP = (- ZRC_ORD(:)/ZCOEFF_QSAT(:))     
     ZPART_DRY(:) = ZDZ_STOP / (PZZ(:,KK+1)-PZZ(:,KK))
     GTEST_LOCAL_LCL(:)=.TRUE.
  
  ENDWHERE

IF (KK==IKB) THEN !MODIF WRF JP   
  ZCOEFF_MINUS_HALF = 0.
ELSE!MODIF WRF JP
  ZCOEFF_MINUS_HALF = ((PTHVM(:,KK)-PTHVM(:,KK-1))/PDZZ(:,KK))
ENDIF!MODIF WRF JP   

  ZCOEFF_PLUS_HALF  = ((PTHVM(:,KK+1)-PTHVM(:,KK))/PDZZ(:,KK+1))
  
  ZCOTHVU_MINUS_HALF = (ZTHV_UP(:)-ZTHV_UP_F1(:))/ZDZ_HALF(:)
  ZCOTHVU_PLUS_HALF  = (ZTHV_UP_F2(:)-ZTHV_UP(:))/ZDZ_HALF(:)

IF (KK==IKB) THEN !MODIF WRF JP   
  ZTHV_MINUS_HALF = PTHVM(:,KK)
ELSE!MODIF WRF JP
  ZTHV_MINUS_HALF = ZCOEFF_MINUS_HALF*0.5*(PZZ(:,KK)-PZZ(:,KK-1))+PTHVM(:,KK-1)
ENDIF!MODIF WRF JP   

  ZTHV_PLUS_HALF  = ZCOEFF_PLUS_HALF*0.5*(PZZ(:,KK+1)-PZZ(:,KK))+ ZTHV_MINUS_HALF  ! BUG JP 16082010
 
  ! Integral Buoyancy between flux level KK and mass level KK 
          
  PBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_HALF(:)*&
              (0.5*( ZCOTHVU_MINUS_HALF - ZCOEFF_MINUS_HALF)*ZDZ_HALF(:) &
                - ZTHV_MINUS_HALF + ZTHV_UP_F1(:) ) 


 
  WHERE ((OTEST).AND.(.NOT.OTESTLCL))
     PENTR=0.
     PDETR=0.

 
     ZBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_STOP(:)*&
                (0.5 * (  - ZCOEFF_MINUS_HALF)* ZDZ_STOP(:) &
                  - ZTHV_MINUS_HALF + ZTHV_UP_F1(:) ) 
           
  
     WHERE (ZBUO_INTEG(:)>=0.)
         PENTR = 0.5/(XABUO-XBENTR*XENTR_DRY)*&
                 LOG(1.+ (2.*(XABUO-XBENTR*XENTR_DRY)/PW_UP2(:,KK))* &
                 ZBUO_INTEG)
         PDETR = 0.
    
         ZW2_HALF = PW_UP2(:,KK) +  2*(XABUO-XBENTR*XENTR_DRY)*(ZBUO_INTEG)
     ELSEWHERE
         PENTR = 0.
         PDETR = 0.5/(XABUO)*&
                 LOG(1.+ (2.*(XABUO)/PW_UP2(:,KK))* &
                 MAX(0.,-ZBUO_INTEG))

         ZW2_HALF = PW_UP2(:,KK) +  2*(XABUO)*(ZBUO_INTEG)
     ENDWHERE
     
 ENDWHERE

 
 ZDZ_STOP(:) = ZDZ_HALF(:)
  
! total Integral Buoyancy between flux level KK and flux level KK+1 

 PBUO_INTEG = PBUO_INTEG + ZG_O_THVREF(:,KK)*ZDZ_HALF(:)*&
                (0.5*(ZCOTHVU_PLUS_HALF - ZCOEFF_PLUS_HALF)* ZDZ_HALF(:) - & 
                PTHVM(:,KK) + ZTHV_UP(:) ) 
              
               
 WHERE ((((ZRCMIX_F2(:)>0.).AND.(ZRCMIX(:)<=0.)).AND.(.NOT.OTESTLCL)).AND.(.NOT.GTEST_LOCAL_LCL(:)))
 ! If rc is found between mass level KK and flux level KK+1
 ! a part of dry entrainment is defined
 ! the exact height of LCL is also determined
     PT(:)=ZTHMIX_F2(:)*((PPABSM(:,KK+1)/XP00)**(XRD/XCPD)) 
     ZQVSAT(:)=EXP( XALPW - XBETAW/PT(:) - XGAMW*LOG(PT(:))  )
     ZQVSAT(:)=XRD/XRV*ZQVSAT(:)/PPABSM(:,KK+1)   &
                   / (1.+(XRD/XRV-1.)*ZQVSAT(:)/PPABSM(:,KK+1))   
     ZCOEFF_QSAT(:) = (PRT_UP(:) - ZQVSAT(:) - &
                ZRCMIX(:))/ (0.5* (PZZ(:,KK+2)-PZZ(:,KK+1)))
     ZRC_ORD(:) = ZRCMIX_F2(:) - ZCOEFF_QSAT(:) * ZDZ_HALF(:)
     ZDZ_STOP = (- ZRC_ORD(:)/ZCOEFF_QSAT(:))     
     ZPART_DRY(:) = 0.5+ZDZ_STOP  / (PZZ(:,KK+1)-PZZ(:,KK))
     GTEST_LOCAL_LCL2(:)=.TRUE.     
 ENDWHERE


 WHERE (((OTEST).AND.(.NOT.OTESTLCL)).AND.(.NOT.GTEST_LOCAL_LCL(:)))
   
     ZBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_STOP(:)*&
                (0.5*( - ZCOEFF_PLUS_HALF)* ZDZ_STOP(:)&
                - PTHVM(:,KK) + ZTHV_UP(:) )

             

     WHERE (ZW2_HALF>0.)
        WHERE (ZBUO_INTEG(:)>=0.)
           PENTR = PENTR + 0.5/(XABUO-XBENTR*XENTR_DRY)* &
                LOG(1.+ (2.*(XABUO-XBENTR*XENTR_DRY)/ZW2_HALF(:)) * ZBUO_INTEG)
          
           PDETR = PDETR
              
        ELSEWHERE
          PENTR = PENTR
          
          PDETR = PDETR + 0.5/(XABUO)* &
                LOG(1.+ (2.*(XABUO)/ZW2_HALF(:)) * &
                MAX(-ZBUO_INTEG,0.))
        ENDWHERE     
     ELSEWHERE
        ! if w**2<0 the updraft is stopped 
           OTEST=.FALSE.
           PENTR = PENTR 
           PDETR = PDETR 
     ENDWHERE
              
 ENDWHERE
 PENTR = XENTR_DRY*PENTR/(PZZ(:,KK+1)-PZZ(:,KK))    
 PDETR = XDETR_DRY*PDETR/(PZZ(:,KK+1)-PZZ(:,KK))
 PDETR = MAX(ZPART_DRY(:)*XDETR_LUP/(PLUP-0.5*(PZZ(:,KK)+PZZ(:,KK+1))),PDETR)


  

! compute final value of critical mixed fraction using theta_v
! of mixture, grid-scale and updraft in cloud

 WHERE ((OTEST).AND.(OTESTLCL))
     ZKIC(:) = MAX(0.,ZTHV_UP(:)-PTHVM(:,KK))*ZKIC(:) /  &  
                 (ZTHV_UP(:)-ZTHVMIX_1(:)+1.E-10)
                       
     ZKIC(:) = MAX(0., MIN(1., ZKIC(:)))
    
     ZEPSI(:) = ZKIC(:) **2.
     ZDELTA(:) = (1.-ZKIC(:))**2.
     ZEPSI_CLOUD=MIN(ZDELTA,ZEPSI)
     ZCOEFFMF_CLOUD(:)=XENTR_MF * XG / XCRAD_MF         
     PENTR(:) = ZCOEFFMF_CLOUD(:)*ZEPSI_CLOUD(:)
     PDETR(:) = ZCOEFFMF_CLOUD(:)*ZDELTA(:)
 ENDWHERE
 
! compute final value of critical mixed fraction using theta_v
! of mixture, grid-scale and updraft in cloud


 WHERE (((OTEST).AND.(.NOT.(OTESTLCL))).AND.((GTEST_LOCAL_LCL(:).OR.GTEST_LOCAL_LCL2(:))))
     ZKIC(:) = MAX(0.,ZTHV_UP_F2(:)-ZTHV_PLUS_HALF)*ZKIC(:) /  &  
                       (ZTHV_UP_F2(:)-ZTHVMIX_2(:)+1.E-10)                      
     ZKIC(:) = MAX(0., MIN(1., ZKIC(:)))
     ZEPSI(:) = ZKIC(:) **2.
     ZDELTA(:) = (1.-ZKIC(:))**2.
     ZEPSI_CLOUD=MIN(ZDELTA,ZEPSI)
     ZCOEFFMF_CLOUD(:)=XENTR_MF * XG / XCRAD_MF     
     PENTR(:) = PENTR+(1.-ZPART_DRY(:))*ZCOEFFMF_CLOUD(:)*ZEPSI_CLOUD(:)
     PDETR(:) = PDETR+(1.-ZPART_DRY(:))*ZCOEFFMF_CLOUD(:)*ZDELTA(:)
 ENDWHERE
 


END SUBROUTINE COMPUTE_ENTR_DETR  

                                       
!     #################################################################
      SUBROUTINE COMPUTE_FUNCTION_THERMO_MF( KRR,KRRL,KRRI,                  &
                                       PTH, PR, PEXN, PPABS,                 &
                                       PT,PLVOCPEXN,PLSOCPEXN,               &
                                       PAMOIST,PATHETA                       )
!     #################################################################
!
!!
!!****  *COMPUTE_FUNCTION_THERMO_MF* - 
!!
!!
!! --------------------------------------------------------------------------
!       

!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
INTEGER,                INTENT(IN)   :: KRR           ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL          ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI          ! number of ice water var.

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PTH      ! theta
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PR       ! water species
REAL, DIMENSION(:,:,:)  , INTENT(IN) :: PPABS,PEXN    ! pressure, Exner funct.

REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PT      ! temperature
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PLVOCPEXN,PLSOCPEXN     ! L/(cp*Pi)

REAL, DIMENSION(:,:,:), INTENT(OUT)  ::  PAMOIST,PATHETA
!
!-------------------------------------------------------------------------------
! 
!*       0.2   Declarations of local variables
!
REAL                :: ZEPS         ! XMV / XMD
REAL, DIMENSION(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)) ::     &
          ZCP,                        &  ! Cp 
          ZE,                         &  ! Saturation mixing ratio
          ZDEDT,                      &  ! Saturation mixing ratio derivative
          ZFRAC_ICE,                  &  ! Ice fraction
          ZAMOIST_W,                  &  ! Coefficients for s = f (Thetal,Rnp)
          ZATHETA_W,                  &  !
          ZAMOIST_I,                  &  !
          ZATHETA_I                      !

INTEGER             :: JRR
!
!-------------------------------------------------------------------------------
!
  ZEPS = XMV / XMD

  PLVOCPEXN(:,:,:) = 0.
  PLSOCPEXN(:,:,:) = 0.
  PAMOIST(:,:,:) = 0.
  PATHETA(:,:,:) = 0.
  ZFRAC_ICE(:,:,:) = 0.0
  ZAMOIST_W(:,:,:) = 0.0
  ZATHETA_W(:,:,:) = 0.0
  ZAMOIST_I(:,:,:) = 0.0
  ZATHETA_I(:,:,:) = 0.0
!
!*       Cph
!
ZCP=XCPD

IF (KRR > 0) ZCP(:,:,:) = ZCP(:,:,:) + XCPV * PR(:,:,:,1)

DO JRR = 2,1+KRRL  ! loop on the liquid components  
   ZCP(:,:,:)  = ZCP(:,:,:) + XCL * PR(:,:,:,JRR)
END DO

DO JRR = 2+KRRL,1+KRRL+KRRI ! loop on the solid components   
  ZCP(:,:,:)  = ZCP(:,:,:)  + XCI * PR(:,:,:,JRR)
END DO

!*      Temperature
!
PT(:,:,:) =  PTH(:,:,:) * PEXN(:,:,:)
!
!
!! Liquid water
!
IF ( KRRL >= 1 ) THEN 
!
!*       Lv/Cph 
!
  PLVOCPEXN(:,:,:) = (XLVTT + (XCPV-XCL) *  (PT(:,:,:)-XTT) ) / ZCP(:,:,:)
!
!*      Saturation vapor pressure with respect to water
!
  ZE(:,:,:) =  EXP( XALPW - XBETAW/PT(:,:,:) - XGAMW*ALOG( PT(:,:,:) ) )
!
!*      Saturation  mixing ratio with respect to water
!
  ZE(:,:,:) =  ZE(:,:,:) * ZEPS / ( PPABS(:,:,:) - ZE(:,:,:) )
!
!*      Compute the saturation mixing ratio derivative (rvs')
!
  ZDEDT(:,:,:) = ( XBETAW / PT(:,:,:)  - XGAMW ) / PT(:,:,:)   &
                 * ZE(:,:,:) * ( 1. + ZE(:,:,:) / ZEPS )
!
!*      Compute Amoist
!
  ZAMOIST_W(:,:,:)=  0.5 / ( 1.0 + ZDEDT(:,:,:) * PLVOCPEXN(:,:,:) )
!
!*      Compute Atheta
!
  ZATHETA_W(:,:,:)= ZAMOIST_W(:,:,:) * PEXN(:,:,:) *                       &
        ( ( ZE(:,:,:) - PR(:,:,:,1) ) * PLVOCPEXN(:,:,:) /                  &
          ( 1. + ZDEDT(:,:,:) * PLVOCPEXN(:,:,:) )           *              &
          (                                                                 &
           ZE(:,:,:) * (1. + ZE(:,:,:)/ZEPS)                                &
                        * ( -2.*XBETAW/PT(:,:,:) + XGAMW ) / PT(:,:,:)**2   &
          +ZDEDT(:,:,:) * (1. + 2. * ZE(:,:,:)/ZEPS)                        &
                        * ( XBETAW/PT(:,:,:) - XGAMW ) / PT(:,:,:)          &
          )                                                                 &
         - ZDEDT(:,:,:)                                                     &
        )

!
!! Solid water
!
  IF ( KRRI >= 1 ) THEN 

!
!*       Fraction of ice
!
    WHERE(PR(:,:,:,2)+PR(:,:,:,4)>0.0)
      ZFRAC_ICE(:,:,:) = PR(:,:,:,4) / ( PR(:,:,:,2)+PR(:,:,:,4) )
    END WHERE
!
!*       Ls/Cph 
!
    PLSOCPEXN(:,:,:) = (XLSTT + (XCPV-XCI) *  (PT(:,:,:)-XTT) ) / ZCP(:,:,:)
!
!*      Saturation vapor pressure with respect to ice
!
    ZE(:,:,:) =  EXP( XALPI - XBETAI/PT(:,:,:) - XGAMI*ALOG( PT(:,:,:) ) )
!
!*      Saturation  mixing ratio with respect to ice
!
    ZE(:,:,:) =  ZE(:,:,:) * ZEPS / ( PPABS(:,:,:) - ZE(:,:,:) )
!
!*      Compute the saturation mixing ratio derivative (rvs')
!
    ZDEDT(:,:,:) = ( XBETAI / PT(:,:,:)  - XGAMI ) / PT(:,:,:)   &
                   * ZE(:,:,:) * ( 1. + ZE(:,:,:) / ZEPS )
!
!*      Compute Amoist
!
    ZAMOIST_I(:,:,:)=  0.5 / ( 1.0 + ZDEDT(:,:,:) * PLSOCPEXN(:,:,:) )
!
!*      Compute Atheta
!
    ZATHETA_I(:,:,:)= ZAMOIST_I(:,:,:) * PEXN(:,:,:) *                     &
        ( ( ZE(:,:,:) - PR(:,:,:,1) ) * PLSOCPEXN(:,:,:) /                  &
          ( 1. + ZDEDT(:,:,:) * PLSOCPEXN(:,:,:) )           *              &
          (                                                                 &
           ZE(:,:,:) * (1. + ZE(:,:,:)/ZEPS)                                &
                        * ( -2.*XBETAI/PT(:,:,:) + XGAMI ) / PT(:,:,:)**2   &
          +ZDEDT(:,:,:) * (1. + 2. * ZE(:,:,:)/ZEPS)                        &
                        * ( XBETAI/PT(:,:,:) - XGAMI ) / PT(:,:,:)          &
          )                                                                 &
         - ZDEDT(:,:,:)                                                     &
        )


  ENDIF

  PAMOIST(:,:,:) = (1.0-ZFRAC_ICE(:,:,:))*ZAMOIST_W(:,:,:) &
                         +ZFRAC_ICE(:,:,:) *ZAMOIST_I(:,:,:)
  PATHETA(:,:,:) = (1.0-ZFRAC_ICE(:,:,:))*ZATHETA_W(:,:,:) &
                         +ZFRAC_ICE(:,:,:) *ZATHETA_I(:,:,:)

!
!*      Lv/Cph/Exner and  Ls/Cph/Exner
!
  PLVOCPEXN(:,:,:) = PLVOCPEXN(:,:,:) / PEXN(:,:,:)
  PLSOCPEXN(:,:,:) = PLSOCPEXN(:,:,:) / PEXN(:,:,:)
!
ENDIF

END SUBROUTINE COMPUTE_FUNCTION_THERMO_MF
!
!     #################################################################
      SUBROUTINE COMPUTE_UPDRAFT(OMIXUV,PZZ,PDZZ,KK,              &
                                 PSFTH,PSFRV,                     &
                                 PPABSM,PRHODREF,PUM,PVM, PTKEM,  &
                                 PTHM,PRVM,PRCM,PRIM,PTHLM,PRTM,  &
                                 PTHL_UP,PRT_UP,                  &
                                 PRV_UP,PRC_UP,PRI_UP,PTHV_UP,    &
                                 PW_UP,PU_UP, PV_UP,              &
                                 PFRAC_UP,PEMF,PDETR,PENTR,       &
                                 KKLCL,KKETL,KKCTL)
!     #################################################################
!!
!!****  *COMPUTE_UPDRAFT* - calculates caracteristics of the updraft 
!!                         
!! --------------------------------------------------------------------------



IMPLICIT NONE

!*                    1.1  Declaration of Arguments
!
!
!
LOGICAL,                INTENT(IN) :: OMIXUV    ! True if mixing of momentum
REAL, DIMENSION(:,:), INTENT(IN)   :: PZZ       !  Height at the flux point
REAL, DIMENSION(:,:), INTENT(IN)   :: PDZZ      !  Metrics coefficient
 
INTEGER,              INTENT(IN)   ::  KK        
REAL, DIMENSION(:),   INTENT(IN)   ::  PSFTH,PSFRV
! normal surface fluxes of theta,rv,(u,v) parallel to the orography
!
REAL, DIMENSION(:,:),   INTENT(IN) ::  PPABSM     ! Pressure at t-dt
REAL, DIMENSION(:,:),   INTENT(IN) ::  PRHODREF   ! dry density of the
                                                  ! reference state
REAL, DIMENSION(:,:),   INTENT(IN) ::  PUM        ! u mean wind
REAL, DIMENSION(:,:),   INTENT(IN) ::  PVM        ! v mean wind
REAL, DIMENSION(:,:),   INTENT(IN) ::  PTKEM      ! TKE at t-dt
!
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTHM           ! liquid pot. temp. at t-dt
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PRVM,PRCM,PRIM ! vapor mixing ratio at t-dt
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTHLM,PRTM     ! cons. var. at t-dt


REAL, DIMENSION(:,:),   INTENT(OUT)  ::  PTHL_UP,PRT_UP   ! updraft properties
REAL, DIMENSION(:,:),   INTENT(OUT)  ::  PU_UP, PV_UP     ! updraft wind components
REAL, DIMENSION(:,:),   INTENT(OUT)  ::  PRV_UP,PRC_UP, & ! updraft rv, rc
                                         PRI_UP,PTHV_UP,& ! updraft ri, THv
                                         PW_UP,PFRAC_UP   ! updraft w, fraction

                                         
REAL, DIMENSION(:,:),   INTENT(OUT)  ::  PEMF,PDETR,PENTR ! Mass_flux,
                                                          ! detrainment,entrainment
INTEGER, DIMENSION(:),  INTENT(OUT)  ::  KKLCL,KKETL,KKCTL! LCL, ETL, CTL                                     
!                       1.2  Declaration of local variables
!
!
! Mean environment variables at t-dt at flux point
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) ::    &
                        ZTHM_F,ZRVM_F,ZRCM_F,ZRIM_F   ! Theta,rv,rl,ri of
                                                      ! updraft environnement
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) ::    &
                        ZRTM_F, ZTHLM_F, ZTKEM_F,&    ! rt, thetal,TKE,pressure,
                        ZUM_F,ZVM_F,ZRHO_F,      &    ! density,momentum
                        ZPRES_F,ZTHVM_F,ZTHVM,   &    ! interpolated at the flux point
                        ZG_O_THVREF,             &    ! g*ThetaV ref
                        ZW_UP2                        ! w**2  of the updraft

                        
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) ::  &
                        ZTH_UP,                  &    ! updraft THETA 
                        ZFRAC_ICE                     ! Ice fraction

REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) ::  ZCOEF  ! diminution coefficient for too high clouds 
                        
REAL, DIMENSION(SIZE(PSFTH,1) )            ::  ZWTHVSURF  ! Surface w'thetav'
CHARACTER(LEN=1)                           ::  YFRAC_ICE  ! Ice Fraction
                                                          ! precribed or computed 

REAL  :: ZRDORV       ! RD/RV
REAL  :: ZRVORD       ! RV/RD


REAL, DIMENSION(SIZE(PTHM,1)) :: ZMIX1,ZMIX2,ZMIX3
REAL, DIMENSION(SIZE(PTHM,1)) :: ZBUO_INTEG           ! Integrated Buoyancy 

REAL, DIMENSION(SIZE(PTHM,1)) :: ZLUP         ! Upward Mixing length from the ground
REAL, DIMENSION(SIZE(PTHM,1)) :: ZDEPTH       ! Deepness limit for cloud

INTEGER  :: IKB,IKE            ! index value for the Beginning and the End
                               ! of the physical domain for the mass points
INTEGER  :: IKU           ! array size in k
INTEGER  :: JK,JI,JSV          ! loop counters

LOGICAL, DIMENSION(SIZE(PTHM,1)) ::  GTEST,GTESTLCL,GTESTETL
                               ! Test if the ascent continue, if LCL or ETL is reached
LOGICAL                          ::  GLMIX 
                               ! To choose upward or downward mixing length
LOGICAL, DIMENSION(SIZE(PTHM,1))              :: GWORK1
LOGICAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2)) :: GWORK2

INTEGER  :: ITEST
REAL  :: ZTMAX,ZRMAX  ! control value
! Thresholds for the  perturbation of
! theta_l and r_t at the first level of the updraft
ZTMAX=2.0
ZRMAX=1.E-3
!------------------------------------------------------------------------

!                     INITIALISATION

!                 Local variables, internal domain
! Internal Domain
IKU=SIZE(PTHM,2)
IKB=1        ! modif WRF JP
IKE=IKU-1      ! modif WRF JP

! Initialisation of intersesting Level :LCL,ETL,CTL
KKLCL(:)=IKE
KKETL(:)=IKE
KKCTL(:)=IKE

! 
! Initialisation
PRV_UP(:,:)=0.
PRC_UP(:,:)=0.

PW_UP(:,:)=0.
PEMF(:,:)=0.
PDETR(:,:)=0.
PENTR(:,:)=0.
ZTH_UP(:,:)=0.
PFRAC_UP(:,:)=0.
PTHV_UP(:,:)=0.

!no ice cloud coded yet 
PRI_UP(:,:)=0.
ZFRAC_ICE(:,:)=0.
YFRAC_ICE='T'

ZBUO_INTEG=0.
! Initialisation of the constants   
ZRDORV   = XRD / XRV   !=0.622
ZRVORD   = (XRV / XRD) 


! Initialisation of environment variables at t-dt

! variables at flux level


ZTHM_F (:,IKB+1:IKU) = 0.5*(PTHM(:,IKB:IKU-1)+PTHM(:,IKB+1:IKU))
ZTHLM_F(:,IKB+1:IKU) = 0.5*(PTHLM(:,IKB:IKU-1)+PTHLM(:,IKB+1:IKU))
ZRTM_F (:,IKB+1:IKU) = 0.5*(PRTM(:,IKB:IKU-1)+PRTM(:,IKB+1:IKU))
ZTKEM_F(:,IKB+1:IKU) = 0.5*(PTKEM(:,IKB:IKU-1)+PTKEM(:,IKB+1:IKU))
ZPRES_F(:,IKB+1:IKU) = 0.5*(PPABSM(:,IKB:IKU-1)+PPABSM(:,IKB+1:IKU))
ZRHO_F (:,IKB+1:IKU) = 0.5*(PRHODREF(:,IKB:IKU-1)+PRHODREF(:,IKB+1:IKU))
ZRVM_F (:,IKB+1:IKU) = 0.5*(PRVM(:,IKB:IKU-1)+PRVM(:,IKB+1:IKU))
ZUM_F  (:,IKB+1:IKU) = 0.5*(PUM(:,IKB:IKU-1)+PUM(:,IKB+1:IKU))
ZVM_F  (:,IKB+1:IKU) = 0.5*(PVM(:,IKB:IKU-1)+PVM(:,IKB+1:IKU))



ZTHM_F (:,IKB) = PTHM(:,IKB) 
ZTHLM_F(:,IKB) = PTHLM(:,IKB)
ZRTM_F (:,IKB) = PRTM (:,IKB)
ZTKEM_F(:,IKB) = PTKEM(:,IKB)
ZPRES_F(:,IKB) = PPABSM(:,IKB)
ZRHO_F(:,IKB)  = PRHODREF(:,IKB)
ZRVM_F(:,IKB)  = PRVM(:,IKB)
ZUM_F(:,IKB)   = PUM(:,IKB)
ZVM_F(:,IKB)   = PVM(:,IKB)


! thetav at mass and flux levels 
ZTHVM_F(:,:)=ZTHM_F(:,:)*((1.+ZRVORD*ZRVM_F(:,:))/(1.+ZRTM_F(:,:)))
ZTHVM(:,:)=PTHM(:,:)*((1.+ZRVORD*PRVM(:,:))/(1.+PRTM(:,:)))


!                     
!          Initialisation of updraft characteristics 
PTHL_UP(:,:)=ZTHLM_F(:,:)
PRT_UP(:,:)=ZRTM_F(:,:)
ZW_UP2(:,:)=0.
PTHV_UP(:,:)=ZTHVM_F(:,:)
PU_UP(:,:)=ZUM_F(:,:)
PV_UP(:,:)=ZVM_F(:,:)


! Computation or initialisation of updraft characteristics at the KK level
! thetal_up,rt_up,thetaV_up, w-squared,Buoyancy term and mass flux (PEMF)

! 03/2009
!PTHL_UP(:,KK)= ZTHLM_F(:,KK)+(PSFTH(:)/SQRT(ZTKEM_F(:,KK)))*XALP_PERT
!PRT_UP(:,KK) = ZRTM_F(:,KK)+(PSFRV(:)/SQRT(ZTKEM_F(:,KK)))*XALP_PERT
PTHL_UP(:,KK)= ZTHLM_F(:,KK)+MAX(0.,MIN(ZTMAX,(PSFTH(:)/SQRT(ZTKEM_F(:,KK)))*XALP_PERT))
PRT_UP(:,KK) = ZRTM_F(:,KK)+MAX(0.,MIN(ZRMAX,(PSFRV(:)/SQRT(ZTKEM_F(:,KK)))*XALP_PERT)) 



ZW_UP2(:,KK) = MAX(0.0001,(2./3.)*ZTKEM_F(:,KK))


! Computation of non conservative variable for the KK level of the updraft
! (all or nothing ajustement)
CALL TH_R_FROM_THL_RT_2D(YFRAC_ICE,ZFRAC_ICE(:,KK:KK),ZPRES_F(:,KK:KK), &
             PTHL_UP(:,KK:KK),PRT_UP(:,KK:KK),ZTH_UP(:,KK:KK), &
             PRV_UP(:,KK:KK),PRC_UP(:,KK:KK),PRI_UP(:,KK:KK))

! compute updraft thevav and buoyancy term at KK level             
PTHV_UP(:,KK) = ZTH_UP(:,KK)*((1+ZRVORD*PRV_UP(:,KK))/(1+PRT_UP(:,KK))) 
                                                            
! Closure assumption for mass flux at KK level                                                  
ZG_O_THVREF=XG/ZTHVM_F

! compute L_up
GLMIX=.TRUE.
ZTKEM_F(:,KK)=0.

CALL COMPUTE_BL89_ML(PDZZ,ZTKEM_F,ZG_O_THVREF,ZTHVM_F,KK,GLMIX,ZLUP)
ZLUP(:)=MAX(ZLUP(:),1.E-10)

! Compute Buoyancy flux at the ground
ZWTHVSURF(:) = (ZTHVM_F(:,IKB)/ZTHM_F(:,IKB))*PSFTH(:)+     &
                (0.61*ZTHM_F(:,IKB))*PSFRV(:)

! Mass flux at KK level (updraft triggered if PSFTH>0.)
WHERE (ZWTHVSURF(:)>0.)
  PEMF(:,KK) = XCMF * ZRHO_F(:,KK) * ((ZG_O_THVREF(:,KK))*ZWTHVSURF*ZLUP)**(1./3.)
  PFRAC_UP(:,KK)=MIN(PEMF(:,KK)/(SQRT(ZW_UP2(:,KK))*ZRHO_F(:,KK)),XFRAC_UP_MAX)
  ZW_UP2(:,KK)=(PEMF(:,KK)/(PFRAC_UP(:,KK)*ZRHO_F(:,KK)))**2
  GTEST(:)=.TRUE.
ELSEWHERE
  PEMF(:,KK) =0.
  GTEST(:)=.FALSE.
ENDWHERE


!--------------------------------------------------------------------------

!                        3. Iteration
!                           ---------
!
! If GTEST = T the updraft starts from the KK level and stops when GTEST becomes F
!
!
GTESTLCL(:)=.FALSE.
GTESTETL(:)=.FALSE.

!       Loop on vertical level

DO JK=KK,IKE-1
! IF the updraft top is reached for all column, stop the loop on levels
ITEST=COUNT(GTEST)
IF (ITEST==0) CYCLE
!       Computation of entrainment and detrainment with KF90
!       parameterization in clouds and LR01 in subcloud layer


! to find the LCL (check if JK is LCL or not)

  WHERE(GTEST)
  WHERE ((PRC_UP(:,JK)>0.).AND.(.NOT.(GTESTLCL)))
      KKLCL(:) = JK           
      GTESTLCL(:)=.TRUE.
  ENDWHERE
  ENDWHERE


! COMPUTE PENTR and PDETR at mass level JK

 CALL COMPUTE_ENTR_DETR(GTEST,GTESTLCL,YFRAC_ICE,ZFRAC_ICE(:,JK),JK,&
                       PPABSM(:,:),PZZ(:,:),PDZZ(:,:),ZTHVM(:,:),&
                       PTHLM(:,JK),PRTM(:,JK),ZW_UP2(:,:),   &
                       PTHL_UP(:,JK),PRT_UP(:,JK),ZLUP(:), &
                       PENTR(:,JK),PDETR(:,JK),ZBUO_INTEG)
  

  IF (JK==KK) THEN
       PDETR(:,JK)=0.
  ENDIF   

 
!       Computation of updraft characteristics at level JK+1
  WHERE(GTEST)
    ZMIX1(:)=0.5*(PZZ(:,JK+1)-PZZ(:,JK))*(PENTR(:,JK)-PDETR(:,JK))
    PEMF(:,JK+1)=PEMF(:,JK)*EXP(2*ZMIX1(:))
  ENDWHERE
  
  

! stop the updraft if MF becomes negative
  WHERE (GTEST.AND.(PEMF(:,JK+1)<=0.))
    PEMF(:,JK+1)=0.
    GTEST(:)=.FALSE.
    KKCTL(:) = JK+1         
  ENDWHERE


! If the updraft did not stop, compute cons updraft characteritics at jk+1
  WHERE(GTEST)     
    ZMIX2(:) = (PZZ(:,JK+1)-PZZ(:,JK))*PENTR(:,JK) !&
    ZMIX3(:) = (PZZ(:,JK+1)-PZZ(:,JK))*PDETR(:,JK) !&                   
                
    PTHL_UP(:,JK+1) = (PTHL_UP(:,JK)*(1.-0.5*ZMIX2(:)) + PTHLM(:,JK)*ZMIX2(:)) &
                          /(1.+0.5*ZMIX2(:))   
    PRT_UP(:,JK+1) = (PRT_UP (:,JK)*(1.-0.5*ZMIX2(:)) + PRTM(:,JK)*ZMIX2(:))   &
                          /(1.+0.5*ZMIX2(:))
  ENDWHERE
  

  IF(OMIXUV) THEN
    WHERE(GTEST) 
      PU_UP(:,JK+1) = (PU_UP (:,JK)*(1-0.5*ZMIX2(:)) + PUM(:,JK)*ZMIX2(:)+ &
                        0.5*XPRES_UV*(PZZ(:,JK+1)-PZZ(:,JK))*&
                        ((PUM(:,JK+1)-PUM(:,JK))/PDZZ(:,JK+1)+&
                         (PUM(:,JK)-PUM(:,JK-1))/PDZZ(:,JK))        )   &
                        /(1+0.5*ZMIX2(:))
      PV_UP(:,JK+1) = (PV_UP (:,JK)*(1-0.5*ZMIX2(:)) + PVM(:,JK)*ZMIX2(:)+ &
                        0.5*XPRES_UV*(PZZ(:,JK+1)-PZZ(:,JK))*&
                        ((PVM(:,JK+1)-PVM(:,JK))/PDZZ(:,JK+1)+&
                         (PVM(:,JK)-PVM(:,JK-1))/PDZZ(:,JK))    )   &
                        /(1+0.5*ZMIX2(:))
    ENDWHERE
  ENDIF
  
! Compute non cons. var. at level JK+1
  CALL TH_R_FROM_THL_RT_2D(YFRAC_ICE,ZFRAC_ICE(:,JK+1:JK+1),ZPRES_F(:,JK+1:JK+1), &
          PTHL_UP(:,JK+1:JK+1),PRT_UP(:,JK+1:JK+1),ZTH_UP(:,JK+1:JK+1), &
          PRV_UP(:,JK+1:JK+1),PRC_UP(:,JK+1:JK+1),PRI_UP(:,JK+1:JK+1))
  

! Compute the updraft theta_v, buoyancy and w**2 for level JK+1   
  WHERE(GTEST)
      PTHV_UP(:,JK+1) = ZTH_UP(:,JK+1)*((1+ZRVORD*PRV_UP(:,JK+1))/(1+PRT_UP(:,JK+1)))
   WHERE (.NOT.(GTESTLCL)) 
      WHERE (ZBUO_INTEG(:)>0.)
        ZW_UP2(:,JK+1)  = ZW_UP2(:,JK) + 2.*(XABUO-XBENTR*XENTR_DRY)* ZBUO_INTEG(:)              
      ENDWHERE
      WHERE (ZBUO_INTEG(:)<=0.)
        ZW_UP2(:,JK+1)  = ZW_UP2(:,JK) + 2.*XABUO* ZBUO_INTEG(:)
      ENDWHERE      
   ENDWHERE      
   WHERE (GTESTLCL)
      ZW_UP2(:,JK+1)  = ZW_UP2(:,JK)*(1.-(XBDETR*ZMIX3(:)+XBENTR*ZMIX2(:)))&
            /(1.+(XBDETR*ZMIX3(:)+XBENTR*ZMIX2(:))) &
            +2.*(XABUO)*ZBUO_INTEG/(1.+(XBDETR*ZMIX3(:)+XBENTR*ZMIX2(:)))
   ENDWHERE
 ENDWHERE


! Test if the updraft has reach the ETL
  GTESTETL(:)=.FALSE.
  WHERE (GTEST.AND.(ZBUO_INTEG(:)<=0.))
      KKETL(:) = JK+1           
      GTESTETL(:)=.TRUE.
  ENDWHERE

! Test is we have reached the top of the updraft
  WHERE (GTEST.AND.((ZW_UP2(:,JK+1)<=0.).OR.(PEMF(:,JK+1)<=0.)))
      ZW_UP2(:,JK+1)=0.
      PEMF(:,JK+1)=0.
      GTEST(:)=.FALSE.
      PTHL_UP(:,JK+1)=ZTHLM_F(:,JK+1)
      PRT_UP(:,JK+1)=ZRTM_F(:,JK+1)
      PRC_UP(:,JK+1)=0.
      PRV_UP(:,JK+1)=0.
      PTHV_UP(:,JK+1)=ZTHVM_F(:,JK+1)
      PFRAC_UP(:,JK+1)=0.
      KKCTL(:)=JK+1
  ENDWHERE
 
! compute frac_up at JK+1
  WHERE (GTEST)
      PFRAC_UP(:,JK+1)=PEMF(:,JK+1)/(SQRT(ZW_UP2(:,JK+1))*ZRHO_F(:,JK+1))
  ENDWHERE

! Updraft fraction must be smaller than XFRAC_UP_MAX
  WHERE (GTEST)
      PFRAC_UP(:,JK+1)=MIN(XFRAC_UP_MAX,PFRAC_UP(:,JK+1))
  ENDWHERE


! When cloudy and non-buoyant, updraft fraction must decrease
  
  WHERE ((GTEST.AND.GTESTETL).AND.GTESTLCL)
    PFRAC_UP(:,JK+1)=MIN(PFRAC_UP(:,JK+1),PFRAC_UP(:,JK))
  ENDWHERE

! Mass flux is updated with the new updraft fraction
  
  PEMF(:,JK+1)=PFRAC_UP(:,JK+1)*SQRT(ZW_UP2(:,JK+1))*ZRHO_F(:,JK+1)

ENDDO


PW_UP(:,:)=SQRT(ZW_UP2(:,:))

PEMF(:,KK) =0.
PEMF(:,IKU)=0. ! modif WRF JP
PFRAC_UP(:,IKU)=0.


DO JI=1,SIZE(PTHM,1) 
   ZDEPTH(JI) = (PZZ(JI,KKCTL(JI)) -  PZZ(JI,KKLCL(JI)) )
END DO

GWORK1(:)= (GTESTLCL(:) .AND. (ZDEPTH(:) > 3000.) )
GWORK2(:,:) = SPREAD( GWORK1(:), DIM=2, NCOPIES=IKU )
ZCOEF(:,:) = SPREAD( (1.-(ZDEPTH(:)-3000.)/1000.), DIM=2, NCOPIES=IKU)
ZCOEF=MIN(MAX(ZCOEF,0.),1.)

WHERE (GWORK2) 
   PEMF(:,:)     = PEMF(:,:)     * ZCOEF(:,:)
   PFRAC_UP(:,:) = PFRAC_UP(:,:) * ZCOEF(:,:)
ENDWHERE


END SUBROUTINE COMPUTE_UPDRAFT


                
!     #################################################################
      SUBROUTINE MF_TURB(OMIXUV, PIMPL, PTSTEP,                       &
                PTSTEP_MET,  PDZZ,                          &
                PRHODJ,                                               &
                PTHLM,PTHVM,PRTM,PUM,PVM,                             &
                PTHLDT,PRTDT,PUDT,PVDT,                               &
                PEMF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP,              &
                PFLXZTHMF,PFLXZTHVMF,PFLXZRMF,PFLXZUMF,PFLXZVMF       )

!     #################################################################
!
!
!!****  *MF_TURB* - computes the MF_turbulent source terms for the prognostic
!!                  variables. 
!!

!! --------------------------------------------------------------------------

!
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
LOGICAL,                INTENT(IN)   :: OMIXUV      ! True if mixing of momentum
REAL,                   INTENT(IN)   :: PIMPL       ! degree of implicitness
REAL,                 INTENT(IN)     ::  PTSTEP   ! Dynamical timestep 
REAL,                 INTENT(IN)     ::  PTSTEP_MET! Timestep for meteorological variables                        
!
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PDZZ        ! metric coefficients

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PRHODJ      ! dry density * Grid size

!   Conservative var. at t-dt
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PTHLM        ! conservative pot. temp.
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PRTM         ! water var.  where 
!  Virtual potential temperature at t-dt
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PTHVM 
!  Momentum at t-dt
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PUM
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PVM
!
! Tendencies of conservative variables
REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PTHLDT

REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PRTDT 
! Tendencies of momentum
REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PUDT
REAL, DIMENSION(:,:,:),   INTENT(OUT) ::  PVDT
! Tendencies of scalar variables


! Updraft characteritics
REAL, DIMENSION(:,:,:), INTENT(IN)   ::  PEMF,PTHL_UP,PTHV_UP,PRT_UP,PU_UP,PV_UP
! Fluxes
REAL, DIMENSION(:,:,:), INTENT(OUT)  ::  PFLXZTHMF,PFLXZTHVMF,PFLXZRMF,PFLXZUMF,PFLXZVMF

!
!
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!

REAL, DIMENSION(SIZE(PTHLM,1),SIZE(PTHLM,2),SIZE(PTHLM,3)) :: ZVARS

!
!----------------------------------------------------------------------------
!
!*      1.PRELIMINARIES
!         -------------
!
!

!
PFLXZTHMF = 0.
PFLXZRMF = 0.
PFLXZTHVMF = 0.
PFLXZUMF = 0.
PFLXZVMF = 0.
PTHLDT = 0.
PRTDT = 0.
PUDT = 0.
PVDT = 0.

!
!----------------------------------------------------------------------------
!
!*      2. COMPUTE THE MEAN FLUX OF CONSERVATIVE VARIABLES at time t-dt
!          (equation (3) of Soares et al)
!          + THE MEAN FLUX OF THETA_V (buoyancy flux)
!          -----------------------------------------------
!   ( Resulting fluxes are in flux level (w-point) as PEMF and PTHL_UP )
!

PFLXZTHMF(:,:,:) = PEMF(:,:,:)*(PTHL_UP(:,:,:)-MZM(PTHLM(:,:,:)))

PFLXZRMF(:,:,:) =  PEMF(:,:,:)*(PRT_UP(:,:,:)-MZM(PRTM(:,:,:)))

PFLXZTHVMF(:,:,:) = PEMF(:,:,:)*(PTHV_UP(:,:,:)-MZM(PTHVM(:,:,:)))

PFLXZTHVMF(:,:,:) = 9.81/PTHVM(:,:,:)* PEMF(:,:,:)*(PTHV_UP(:,:,:)-MZM(PTHVM(:,:,:))) !JP

IF (OMIXUV) THEN
  PFLXZUMF(:,:,:) =  PEMF(:,:,:)*(PU_UP(:,:,:)-MZM(PUM(:,:,:)))
  PFLXZVMF(:,:,:) =  PEMF(:,:,:)*(PV_UP(:,:,:)-MZM(PVM(:,:,:)))
ENDIF
!
!
!----------------------------------------------------------------------------
!
!*      3. COMPUTE TENDENCIES OF CONSERVATIVE VARIABLES (or treated as such...)
!          (implicit formulation)
!          --------------------------------------------
!

!
!
! 3.1 Compute the tendency for the conservative potential temperature
!     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
!
CALL TRIDIAG_MASSFLUX(PTHLM,PFLXZTHMF,-PEMF,PTSTEP_MET,PIMPL,  &
                      PDZZ,PRHODJ,ZVARS )
! compute new flux
PFLXZTHMF(:,:,:) = PEMF(:,:,:)*(PTHL_UP(:,:,:)-MZM(ZVARS(:,:,:)))

!!! compute THL tendency
!
PTHLDT(:,:,:)= (ZVARS(:,:,:)-PTHLM(:,:,:))/PTSTEP_MET

!
! 3.2 Compute the tendency for the conservative mixing ratio
!
CALL TRIDIAG_MASSFLUX(PRTM(:,:,:),PFLXZRMF,-PEMF,PTSTEP_MET,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )
! compute new flux
PFLXZRMF(:,:,:) =  PEMF(:,:,:)*(PRT_UP(:,:,:)-MZM(ZVARS(:,:,:)))

!!! compute RT tendency
PRTDT(:,:,:) = (ZVARS(:,:,:)-PRTM(:,:,:))/PTSTEP_MET
!
! 3.3 Compute the tendency for the (non conservative but treated as it)  mixing ratio
!
!CALL TRIDIAG_MASSFLUX(PTHVM(:,:,:),PFLXZTHVMF,-PEMF,PTSTEP,PIMPL,  &
!                                PDZZ,PRHODJ,ZVARS )
! compute new flux
!PFLXZTHVMF(:,:,:) =  PEMF(:,:,:)*(PTHV_UP(:,:,:)-MZM(ZVARS(:,:,:)))



IF (OMIXUV) THEN
!
! 3.3 Compute the tendency for the (non conservative but treated as it) zonal momentum
!     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
!

 CALL TRIDIAG_MASSFLUX(PUM,PFLXZUMF,-PEMF,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )
! compute new flux
PFLXZUMF(:,:,:) = PEMF(:,:,:)*(PU_UP(:,:,:)-MZM(ZVARS(:,:,:)))

! compute U tendency
  PUDT(:,:,:)= (ZVARS(:,:,:)-PUM(:,:,:))/PTSTEP_MET

!
!
! 3.4 Compute the tendency for the (non conservative but treated as it for the time beiing) 
!                                  meridian momentum
!     (PDZZ and flux in w-point and PRHODJ is mass point, result in mass point)
!
  CALL TRIDIAG_MASSFLUX(PVM,PFLXZVMF,-PEMF,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,ZVARS )
! compute new flux
PFLXZVMF(:,:,:) = PEMF(:,:,:)*(PV_UP(:,:,:)-MZM(ZVARS(:,:,:)))

! compute V tendency
  PVDT(:,:,:)= (ZVARS(:,:,:)-PVM(:,:,:))/PTSTEP_MET

ENDIF



!
END SUBROUTINE MF_TURB   

FUNCTION MZM(PA)  RESULT(PMZM)


!
IMPLICIT NONE
!
!*       0.1   Declarations of argument and result
!              ------------------------------------
!

REAL, DIMENSION(:,:,:), INTENT(IN)                :: PA     ! variable at mass localization
REAL, DIMENSION(SIZE(PA,1),SIZE(PA,2),SIZE(PA,3)) :: PMZM   ! result at flux localization

!        0.2   Declarations of local variables
!              -------------------------------
!

INTEGER :: JK             ! Loop index in z direction
INTEGER :: IKU            ! upper bound in z direction of PA
!
IKU = SIZE(PA,3)

!DO JK=2,IKU MODIF WRF JP
DO JK=2,IKU
   PMZM(:,:,JK)=0.5*(PA(:,:,JK)+PA(:,:,JK-1))
ENDDO

PMZM(:,:,1)=PA(:,:,2)


END FUNCTION MZM


!
!
!     #################################################################
      SUBROUTINE TH_R_FROM_THL_RT_1D(HFRAC_ICE,PFRAC_ICE,PP,             &
                                  PTHL, PRT, PTH, PRV, PRL, PRI       )
!     #################################################################
!
!
!!****  *TH_R_FROM_THL_RT_1D* - computes the non-conservative variables
!!                          from conservative variables
!!

!!
!! --------------------------------------------------------------------------
!       

!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
CHARACTER*1           , INTENT(IN) :: HFRAC_ICE
REAL, DIMENSION(:), INTENT(INOUT) :: PFRAC_ICE
REAL, DIMENSION(:), INTENT(IN) :: PP          ! Pressure
REAL, DIMENSION(:), INTENT(IN) :: PTHL    ! thetal (or thetal tendency) to
                                             ! transform into th (or tendency)
REAL, DIMENSION(:),INTENT(IN)  :: PRT    ! Total mixing ratios (or tendency) to
                                             ! transform into rv,rc and ri
                                             ! (or its tendency)
REAL, DIMENSION(:), INTENT(OUT):: PTH    ! th (or th_l tendency)
REAL, DIMENSION(:), INTENT(OUT):: PRV    ! vapor mixing ratio (or its tendency)
REAL, DIMENSION(:), INTENT(OUT):: PRL    ! vapor mixing ratio (or its tendency)
REAL, DIMENSION(:), INTENT(OUT):: PRI    ! vapor mixing ratio (or its tendency)

!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER                       :: II
! Loop control
REAL                          :: ZCOEF
REAL, DIMENSION(SIZE(PP,1))   :: ZEXN,ZFOES,ZQVSAT
REAL, DIMENSION(SIZE(PTHL,1)) :: ZRVSAT,ZCPH,ZRLTEMP  
REAL, DIMENSION(SIZE(PTHL,1)) :: ZT,ZLOVCPEXN,ZLOSCPEXN
!----------------------------------------------------------------------------
!
!*      1 Initialisation 
!         --------------
!
!
ZCOEF = 0.8
PRL(:)=0.
PRI(:)=0.
ZRLTEMP(:)=0.
PRV(:)=PRT(:)
PTH(:)=PTHL(:)
ZEXN(:)=(PP(:)/XP00) ** (XRD/XCPD)
!
!       2 Iteration
!         ---------

    DO II=1,20
      ZT(:)=PTH(:)*ZEXN(:)

      WHERE (ZT(:) > 273.15)
! warm cloud
      ZFOES(:) = EXP( XALPW - XBETAW/ZT(:) - XGAMW*LOG(ZT(:))  )
      ZQVSAT(:) = XRD/XRV*ZFOES(:)/PP(:)   &
                   / (1.+(XRD/XRV-1.)*ZFOES(:)/PP(:))

      ZRVSAT(:) = (1-ZCOEF)*ZQVSAT(:)*(1+PRT(:))+(ZCOEF)*PRV(:)
!      CALL COMPUTE_FRAC_ICE_1D(HFRAC_ICE,PFRAC_ICE,PP,ZT) 
      PFRAC_ICE(:)=0.
      ZRLTEMP(:)=MAX(0.,PRV(:)-ZRVSAT(:))
      PRV(:)=PRV(:)-ZRLTEMP(:)
      PRL(:)=PRL(:)+PRI(:)+ZRLTEMP(:)
      PRI(:)    = PFRAC_ICE(:)    * (PRL(:))
      PRL(:)    = (1-PFRAC_ICE(:))* (PRT(:) - PRV(:))
      !       2.1 Cph
      ZCPH(:)=XCPD+ XCPV * PRV(:)+ XCL * PRL(:) + XCI * PRI(:)
      
      !       2.2 L/Cp/EXN
      ZLOVCPEXN(:) = (XLVTT + (XCPV-XCL) * (ZT(:)-XTT)) &
                        /(ZCPH*ZEXN(:))
      ZLOSCPEXN(:) = (XLSTT + (XCPV-XCI) * (ZT(:)-XTT)) &
                        /(ZCPH*ZEXN(:))
      PTH(:)=PTHL(:)+ZLOVCPEXN*PRL(:)+ZLOSCPEXN(:)*PRI(:)          

      ELSEWHERE
             ! cold shallow cloud not yet coded 
             ! probleme also for the highest level of fire case
        PRL(:)=0.
        PRI(:)=0.
        PRV(:)=PRT(:)
        PTH(:)=PTHL(:)
      ENDWHERE 
    ENDDO
   
END SUBROUTINE TH_R_FROM_THL_RT_1D

              
!     #################################################################
      SUBROUTINE TH_R_FROM_THL_RT_2D(HFRAC_ICE,PFRAC_ICE,PP,             &
                                  PTHL, PRT, PTH, PRV, PRL, PRI       )
!     #################################################################
!
!
!!****  *TH_R_FROM_THL_RT_2D* - computes the non-conservative variables
!!                          from conservative variables
!!
!!
!!
!! --------------------------------------------------------------------------


!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
CHARACTER*1         , INTENT(IN) :: HFRAC_ICE
REAL, DIMENSION(:,:), INTENT(INOUT) :: PFRAC_ICE
REAL, DIMENSION(:,:), INTENT(IN) :: PP          ! Pressure
REAL, DIMENSION(:,:), INTENT(IN) :: PTHL    ! thetal (or thetal tendency) to
                                             ! transform into th (or tendency)
REAL, DIMENSION(:,:),INTENT(IN)  :: PRT    ! Total mixing ratios (or tendency) to
                                             ! transform into rv,rc and ri
                                             ! (or its tendency)
REAL, DIMENSION(:,:), INTENT(OUT):: PTH    ! th (or th_l tendency)
REAL, DIMENSION(:,:), INTENT(OUT):: PRV    ! vapor mixing ratio (or its tendency)
REAL, DIMENSION(:,:), INTENT(OUT):: PRL    ! vapor mixing ratio (or its tendency)
REAL, DIMENSION(:,:), INTENT(OUT):: PRI    ! vapor mixing ratio (or its tendency)

!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER                :: II
! Loop control
REAL                                         :: ZCOEF
REAL, DIMENSION(SIZE(PP,1),SIZE(PP,2))       :: ZEXN,ZFOES,ZQVSAT
REAL, DIMENSION(SIZE(PTHL,1),SIZE(PTHL,2))   :: ZRVSAT,ZCPH,ZRLTEMP  
REAL, DIMENSION(SIZE(PTHL,1),SIZE(PTHL,2))   :: ZT,ZLOVCPEXN,ZLOSCPEXN
!----------------------------------------------------------------------------
!
!*      1 Initialisation 
!         --------------
!
!
ZCOEF = 0.8
PRL(:,:)=0.
PRI(:,:)=0.
ZRLTEMP(:,:)=0.
PRV(:,:)=PRT(:,:)
PTH(:,:)=PTHL(:,:)
ZEXN(:,:)=(PP(:,:)/XP00) ** (XRD/XCPD)
!
!       2 Iteration
!         ---------

    DO II=1,20
      ZT(:,:)=PTH(:,:)*ZEXN(:,:)
      WHERE (ZT(:,:) > 273.15)
! warm cloud

      ZFOES(:,:) = EXP( XALPW - XBETAW/ZT(:,:) - XGAMW*LOG(ZT(:,:))  )
      ZQVSAT(:,:) = XRD/XRV*ZFOES(:,:)/PP(:,:)   &
                   / (1.+(XRD/XRV-1.)*ZFOES(:,:)/PP(:,:))
      ZRVSAT(:,:) = (1-ZCOEF)*ZQVSAT(:,:)*(1+PRT(:,:))+(ZCOEF)*PRV(:,:)

!      CALL COMPUTE_FRAC_ICE_2D(HFRAC_ICE,PFRAC_ICE,PP,ZT) 
      PFRAC_ICE(:,:) = 0.
      ZRLTEMP(:,:)=MAX(0.,PRV(:,:)-ZRVSAT(:,:))
      PRV(:,:)=PRV(:,:)-ZRLTEMP(:,:)
      PRL(:,:)=PRL(:,:)+PRI(:,:)+ZRLTEMP(:,:)
      PRI(:,:)    = PFRAC_ICE(:,:)    * (PRL(:,:))
      PRL(:,:)    = (1-PFRAC_ICE(:,:))* (PRT(:,:) - PRV(:,:))
      !       2.1 Cph
        ZCPH(:,:)=XCPD+ XCPV * PRV(:,:)+ XCL * PRL(:,:) + XCI * PRI(:,:)
      
        !       2.2 L/Cp/EXN
        ZLOVCPEXN(:,:) = (XLVTT + (XCPV-XCL) * (ZT(:,:)-XTT)) &
                        /(ZCPH*ZEXN(:,:))
        ZLOSCPEXN(:,:) = (XLSTT + (XCPV-XCI) * (ZT(:,:)-XTT)) &
                        /(ZCPH*ZEXN(:,:))
        PTH(:,:)=PTHL(:,:)+ZLOVCPEXN*PRL(:,:)+ZLOSCPEXN(:,:)*PRI(:,:)          
      ELSEWHERE
        ! cold shallow cloud not yet coded 
        ! probleme also for the highest level of fire case
        PRL(:,:)=0.
        PRI(:,:)=0.
        PRV(:,:)=PRT(:,:)
        PTH(:,:)=PTHL(:,:)
       ENDWHERE   
    ENDDO
   
END SUBROUTINE TH_R_FROM_THL_RT_2D


                                       
!     #################################################################
      SUBROUTINE THL_RT_FROM_TH_R_MF( KRR,KRRL,KRRI,                  &
                                       PTH, PR, PLVOCPEXN, PLSOCPEXN, &
                                       PTHL, PRT                      )
!     #################################################################
!
!!
!!****  *THL_RT_FROM_TH_R* - computes the conservative variables THL and RT
!!                           from TH and the non precipitating water species
!!
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
!USE MODD_CST
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
INTEGER,                INTENT(IN)   :: KRR           ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL          ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI          ! number of ice water var.

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PTH      ! theta
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PR       ! water species
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PLVOCPEXN, PLSOCPEXN      ! L/(cp*Pi)

REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PTHL     ! th_l
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PRT      ! total non precip. water
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!

!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
!

IF ( KRRL >= 1 ) THEN
  IF ( KRRI >= 1 ) THEN 
    ! Rnp 
    PRT(:,:,:)  = PR(:,:,:,1)  + PR(:,:,:,2)  + PR(:,:,:,4)
    ! Theta_l 
    PTHL(:,:,:)  = PTH(:,:,:)  - PLVOCPEXN(:,:,:) * PR(:,:,:,2) &
                               - PLSOCPEXN(:,:,:) * PR(:,:,:,4)
  ELSE
    ! Rnp
    PRT(:,:,:)  = PR(:,:,:,1)  + PR(:,:,:,2) 
    ! Theta_l
    PTHL(:,:,:) = PTH(:,:,:)  - PLVOCPEXN(:,:,:) * PR(:,:,:,2)
  END IF
ELSE
    ! Rnp = rv
PRT(:,:,:)  = PR(:,:,:,1)
    ! Theta_l = Theta
PTHL(:,:,:) = PTH(:,:,:)
END IF

END SUBROUTINE THL_RT_FROM_TH_R_MF


!      #################################################
       SUBROUTINE TRIDIAG_MASSFLUX(PVARM,PF,PDFDT,PTSTEP,PIMPL,  &
                                 PDZZ,PRHODJ,PVARP             )
!      #################################################
!
!
!!****   *TRIDIAG_MASSFLUX* - routine to solve a time implicit scheme
!!
!!
!! ---------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
!
!
IMPLICIT NONE
!
!
!*       0.1 declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PVARM   ! variable at t-1      at mass point
REAL, DIMENSION(:,:,:), INTENT(IN) :: PF      ! flux in dT/dt=-dF/dz at flux point
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDFDT   ! dF/dT                at flux point
REAL,                   INTENT(IN) :: PTSTEP  ! Double time step
REAL,                   INTENT(IN) :: PIMPL   ! implicit weight
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZZ    ! Dz                   at flux point
REAL, DIMENSION(:,:,:), INTENT(IN) :: PRHODJ  ! (dry rho)*J          at mass point
!
REAL, DIMENSION(:,:,:), INTENT(OUT):: PVARP   ! variable at t+1      at mass point
!
!
!*       0.2 declarations of local variables
!
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2),SIZE(PVARM,3))  :: ZRHODJ_DFDT_O_DZ
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2),SIZE(PVARM,3))  :: ZMZM_RHODJ
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2),SIZE(PVARM,3))  :: ZA, ZB, ZC
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2),SIZE(PVARM,3))  :: ZY ,ZGAM 
                                         ! RHS of the equation, 3D work array
REAL, DIMENSION(SIZE(PVARM,1),SIZE(PVARM,2))                :: ZBET
                                         ! 2D work array
INTEGER                              :: JK            ! loop counter
INTEGER                              :: IKB,IKE       ! inner vertical limits
!
! ---------------------------------------------------------------------------
!                                              
!*      1.  Preliminaries
!           -------------
!
IKB=1
IKE=SIZE(PVARM,3)-1 
!
ZMZM_RHODJ  = MZM(PRHODJ)
ZRHODJ_DFDT_O_DZ = ZMZM_RHODJ*PDFDT/PDZZ
!
ZA=0.
ZB=0.
ZC=0.
ZY=0.
!
!
!*      2.  COMPUTE THE RIGHT HAND SIDE
!           ---------------------------
!
ZY(:,:,IKB) = PRHODJ(:,:,IKB)*PVARM(:,:,IKB)/PTSTEP             &
    - ZMZM_RHODJ(:,:,IKB+1) * PF(:,:,IKB+1)/PDZZ(:,:,IKB+1)     &
    + ZMZM_RHODJ(:,:,IKB  ) * PF(:,:,IKB  )/PDZZ(:,:,IKB  )     &
    + ZRHODJ_DFDT_O_DZ(:,:,IKB+1) * 0.5*PIMPL * PVARM(:,:,IKB+1)    &
    + ZRHODJ_DFDT_O_DZ(:,:,IKB+1) * 0.5*PIMPL * PVARM(:,:,IKB  )
!
DO JK=IKB+1,IKE-1
  ZY(:,:,JK) = PRHODJ(:,:,JK)*PVARM(:,:,JK)/PTSTEP          &
    - ZMZM_RHODJ(:,:,JK+1) * PF(:,:,JK+1)/PDZZ(:,:,JK+1)    &
    + ZMZM_RHODJ(:,:,JK  ) * PF(:,:,JK  )/PDZZ(:,:,JK  )    &
    + ZRHODJ_DFDT_O_DZ(:,:,JK+1) * 0.5*PIMPL * PVARM(:,:,JK+1)  &
    + ZRHODJ_DFDT_O_DZ(:,:,JK+1) * 0.5*PIMPL * PVARM(:,:,JK  )  &
    - ZRHODJ_DFDT_O_DZ(:,:,JK  ) * 0.5*PIMPL * PVARM(:,:,JK  )  &
    - ZRHODJ_DFDT_O_DZ(:,:,JK  ) * 0.5*PIMPL * PVARM(:,:,JK-1)
END DO
! 
ZY(:,:,IKE) = PRHODJ(:,:,IKE)*PVARM(:,:,IKE)/PTSTEP         &
    - ZMZM_RHODJ(:,:,IKE+1) * PF(:,:,IKE+1)/PDZZ(:,:,IKE+1) &
    + ZMZM_RHODJ(:,:,IKE  ) * PF(:,:,IKE  )/PDZZ(:,:,IKE  ) &
    - ZRHODJ_DFDT_O_DZ(:,:,IKE ) * 0.5*PIMPL * PVARM(:,:,IKE  ) &
    - ZRHODJ_DFDT_O_DZ(:,:,IKE ) * 0.5*PIMPL * PVARM(:,:,IKE-1)
!
!
!*       3.  INVERSION OF THE TRIDIAGONAL SYSTEM
!            -----------------------------------
!
IF ( PIMPL > 1.E-10 ) THEN
!
!*       3.1 arrays A, B, C
!            --------------
!
  ZB(:,:,IKB) =   PRHODJ(:,:,IKB)/PTSTEP                   &
                + ZRHODJ_DFDT_O_DZ(:,:,IKB+1) * 0.5*PIMPL
  ZC(:,:,IKB) =   ZRHODJ_DFDT_O_DZ(:,:,IKB+1) * 0.5*PIMPL

  DO JK=IKB+1,IKE-1
    ZA(:,:,JK) = - ZRHODJ_DFDT_O_DZ(:,:,JK  ) * 0.5*PIMPL
    ZB(:,:,JK) =   PRHODJ(:,:,JK)/PTSTEP                   &
                 + ZRHODJ_DFDT_O_DZ(:,:,JK+1) * 0.5*PIMPL &
                 - ZRHODJ_DFDT_O_DZ(:,:,JK  ) * 0.5*PIMPL
    ZC(:,:,JK) =   ZRHODJ_DFDT_O_DZ(:,:,JK+1) * 0.5*PIMPL
  END DO

  ZA(:,:,IKE) = - ZRHODJ_DFDT_O_DZ(:,:,IKE  ) * 0.5*PIMPL
  ZB(:,:,IKE) =   PRHODJ(:,:,IKE)/PTSTEP                   &
                - ZRHODJ_DFDT_O_DZ(:,:,IKE  ) * 0.5*PIMPL
!
!*       3.2 going up
!            --------
!
  ZBET(:,:) = ZB(:,:,IKB)  ! bet = b(ikb)
  PVARP(:,:,IKB) = ZY(:,:,IKB) / ZBET(:,:)

  !
  DO JK = IKB+1,IKE-1
    ZGAM(:,:,JK) = ZC(:,:,JK-1) / ZBET(:,:)  
                                                    ! gam(k) = c(k-1) / bet
    ZBET(:,:)    = ZB(:,:,JK) - ZA(:,:,JK) * ZGAM(:,:,JK)
                                                    ! bet = b(k) - a(k)* gam(k)  
    PVARP(:,:,JK)= ( ZY(:,:,JK) - ZA(:,:,JK) * PVARP(:,:,JK-1) ) / ZBET(:,:)
                                        ! res(k) = (y(k) -a(k)*res(k-1))/ bet 
  END DO 
  ! special treatment for the last level
  ZGAM(:,:,IKE) = ZC(:,:,IKE-1) / ZBET(:,:) 
                                                    ! gam(k) = c(k-1) / bet
  ZBET(:,:)     = ZB(:,:,IKE) - ZA(:,:,IKE) * ZGAM(:,:,IKE)
                                                    ! bet = b(k) - a(k)* gam(k)  
  PVARP(:,:,IKE)= ( ZY(:,:,IKE) - ZA(:,:,IKE) * PVARP(:,:,IKE-1) ) / ZBET(:,:)
                                       ! res(k) = (y(k) -a(k)*res(k-1))/ bet 
!
!*       3.3 going down
!            ----------
!
  DO JK = IKE-1,IKB,-1
    PVARP(:,:,JK) = PVARP(:,:,JK) - ZGAM(:,:,JK+1) * PVARP(:,:,JK+1)
  END DO
!
!
ELSE
!!! EXPLICIT FORMULATION
! 
  PVARP(:,:,IKB:IKE) = ZY(:,:,IKB:IKE) * PTSTEP / PRHODJ(:,:,IKB:IKE)
!
END IF 
!
!
!*       4.  FILL THE UPPER AND LOWER EXTERNAL VALUES
!            ----------------------------------------
!
!PVARP(:,:,IKB-1)=PVARP(:,:,IKB) MODIF WRF JP
PVARP(:,:,IKE+1)=PVARP(:,:,IKE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIDIAG_MASSFLUX

!     
!     #################################################################
      SUBROUTINE UPDRAFT_SOPE(KRR,KRRL,KRRI,OMIXUV,                   & 
                         PZZ,PDZZ,PSFTH,PSFRV,PPABSM,PRHODREF,        &
                         PTKEM,PTHM,PRM,PTHLM,PRTM,PUM,PVM,           &
                         PTHL_UP,PRT_UP,PRV_UP,PU_UP,PV_UP,           &
                         PRC_UP,PRI_UP,PTHV_UP,PW_UP,PFRAC_UP,PEMF,   &
                         PDETR,PENTR,KKLCL,KKETL,KKCTL )
!     #################################################################
!!
!!****  *UPDRAFT_SOPE* - Interfacing routine
!!
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!

IMPLICIT NONE

!*                    1.1  Declaration of Arguments
!
!
INTEGER,                INTENT(IN)   :: KRR       ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL      ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI      ! number of ice water var.
LOGICAL,                INTENT(IN)   :: OMIXUV    ! True if mixing of momentum

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PZZ       !  Height at the flux point

REAL, DIMENSION(:,:,:), INTENT(IN)   :: PDZZ      ! depth between mass levels 
 
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PSFTH,PSFRV
                                            ! normal surface fluxes of theta,rv
!
!    prognostic variables at t- deltat
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PPABSM      ! Pressure at time t-1
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PRHODREF    ! dry density of the
                                                     ! reference state
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTKEM       ! TKE
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PUM,PVM     ! momentum

!
!   thermodynamical variables which are transformed in conservative var.
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PTHM       ! pot. temp. = PTHLM in turb.f90
REAL, DIMENSION(:,:,:,:), INTENT(IN)   ::  PRM        ! water species 
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PTHLM,PRTM !cons. var.  
REAL, DIMENSION(:,:,:),   INTENT(OUT)  ::  PTHL_UP,PRT_UP  ! updraft properties
REAL, DIMENSION(:,:,:),   INTENT(OUT)  ::  PRV_UP,PRC_UP,PRI_UP,&!Thl,Rt,Rv,Rc,Ri
                                           PW_UP,PFRAC_UP,PEMF, &!w,Updraft Fraction, Mass Flux
                                           PDETR,PENTR,PTHV_UP, &!entrainment, detrainment, ThV
                                           PU_UP, PV_UP          !updraft wind component
                                           
INTEGER, DIMENSION(:,:),  INTENT(OUT)  ::  KKLCL,KKETL,KKCTL     !index for LCL,ETL,CTL       
!
!
!
!                       1.2  Declaration of local variables

INTEGER :: IKB

!
! Variables to transform 3D fields in 2D fields
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZPABSM,ZRHODREF
       
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZZZ,ZDZZ

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZTHLM,ZRTM,&
                                                              ZTHM,ZTKEM,&
                                                              ZUM,ZVM
                                                              
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZRVM,ZRCM,ZRIM

REAL, DIMENSION(SIZE(PSFTH,1)*SIZE(PSFTH,2))    ::  ZSFTH,ZSFRV

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3))  :: ZTHL_UP,ZRT_UP,&
                                      ZRV_UP,ZRC_UP, ZRI_UP, &
                                      ZW_UP,ZU_UP,ZV_UP,ZTHV_UP,           &
                                      ZFRAC_UP,ZEMF_UP,ZENTR_UP,ZDETR_UP

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2))    :: ZFRAC_GRID
INTEGER, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2)) :: JKETL,JKCTL,JKLCL

INTEGER :: IIU,IJU,IKU! Limit of the grid
INTEGER :: J1D        ! horizontal loop counter
INTEGER :: JK,JKK,JSV ! loop counters
INTEGER :: JRR        ! moist loop counter
REAL    :: ZRVORD     ! Rv/Rd (=1/0.622 cf glossaire)

!------------------------------------------------------------------------

!                     2. INITIALISATION

!                     2.1 Local variables, internal domain

IIU=SIZE(PTKEM,1)
IJU=SIZE(PTKEM,2)
!
IKB = 1              ! Modif WRF JP
IKU = SIZE(PTKEM,3)

ZRVORD = XRV / XRD


DO JK=IKB,IKU
  ZZZ    (:,JK) = RESHAPE(PZZ    (:,:,JK),(/ IIU*IJU /) )
  ZDZZ   (:,JK) = RESHAPE(PDZZ    (:,:,JK),(/ IIU*IJU /) )  
  ZTHM   (:,JK) = RESHAPE(PTHM   (:,:,JK),(/ IIU*IJU /) )
  ZTKEM  (:,JK) = RESHAPE(PTKEM  (:,:,JK),(/ IIU*IJU /) )
  ZPABSM (:,JK) = RESHAPE(PPABSM (:,:,JK),(/ IIU*IJU /) )
  ZRHODREF(:,JK) = RESHAPE(PRHODREF(:,:,JK),(/ IIU*IJU /) )  
  ZRVM   (:,JK) = RESHAPE(PRM    (:,:,JK,1),(/ IIU*IJU /) ) 
  ZTHLM  (:,JK) = RESHAPE(PTHLM   (:,:,JK),(/ IIU*IJU /) )
  ZRTM   (:,JK) = RESHAPE(PRTM    (:,:,JK),(/ IIU*IJU /) )
  ZUM    (:,JK) = RESHAPE(PUM    (:,:,JK),(/ IIU*IJU /) )
  ZVM    (:,JK) = RESHAPE(PVM    (:,:,JK),(/ IIU*IJU /) )
END DO

IF (KRRL>1) THEN
DO JK=1,IKU
  ZRCM   (:,JK) = RESHAPE(PRM    (:,:,JK,2),(/ IIU*IJU /) ) 
END DO
ELSE
  ZRCM   (:,:) =0.
ENDIF
IF (KRRI>1) THEN
DO JK=1,IKU
  ZRIM   (:,JK) = RESHAPE(PRM    (:,:,JK,4),(/ IIU*IJU /) ) 
END DO
ELSE
  ZRIM   (:,:) =0.
ENDIF

ZSFTH(:)=RESHAPE(PSFTH(:,:),(/ IIU*IJU /) )
ZSFRV(:)=RESHAPE(PSFRV(:,:),(/ IIU*IJU /) )

!Updraft begins at level 1 (Modif WRF)
JK=IKB 

!        6.2 compute properties of the updraft
CALL COMPUTE_UPDRAFT(OMIXUV,ZZZ,ZDZZ,JK,      &
                     ZSFTH,ZSFRV,ZPABSM,ZRHODREF,ZUM,ZVM,ZTKEM, &
                     ZTHM,ZRVM,ZRCM,ZRIM,ZTHLM,ZRTM,       &
                     ZTHL_UP,ZRT_UP,ZRV_UP,ZRC_UP,ZRI_UP,&
                     ZTHV_UP,ZW_UP,ZU_UP,ZV_UP, &
                     ZFRAC_UP,ZEMF_UP,&
                     ZDETR_UP,ZENTR_UP,&
                     JKLCL,JKETL,JKCTL)


PTHL_UP(:,:,:)= RESHAPE(ZTHL_UP(:,:), (/ IIU,IJU,IKU /))
PRT_UP(:,:,:)=RESHAPE(ZRT_UP(:,:), (/ IIU,IJU,IKU /) )
PRV_UP(:,:,:)=RESHAPE(ZRV_UP(:,:), (/ IIU,IJU,IKU /) )
PRC_UP(:,:,:)=RESHAPE(ZRC_UP(:,:), (/ IIU,IJU,IKU /) )
PRI_UP(:,:,:)=RESHAPE(ZRI_UP(:,:), (/ IIU,IJU,IKU /) )
PW_UP(:,:,:)=RESHAPE(ZW_UP(:,:), (/ IIU,IJU,IKU /) )
PU_UP(:,:,:)=RESHAPE(ZU_UP(:,:), (/ IIU,IJU,IKU /) )
PV_UP(:,:,:)=RESHAPE(ZV_UP(:,:), (/ IIU,IJU,IKU /) )
PEMF(:,:,:)=RESHAPE(ZEMF_UP(:,:), (/ IIU,IJU,IKU /) )
PDETR(:,:,:)=RESHAPE(ZDETR_UP(:,:), (/ IIU,IJU,IKU /) )
PENTR(:,:,:)=RESHAPE(ZENTR_UP(:,:), (/ IIU,IJU,IKU /) )
PTHV_UP(:,:,:)=RESHAPE(ZTHV_UP(:,:), (/ IIU,IJU,IKU /) )
KKETL(:,:)=RESHAPE(JKETL(:),(/ IIU,IJU/) )
KKCTL(:,:)=RESHAPE(JKCTL(:),(/ IIU,IJU/) )
KKLCL(:,:)=RESHAPE(JKLCL(:),(/ IIU,IJU/) )
PFRAC_UP(:,:,:)=RESHAPE(ZFRAC_UP(:,:),(/ IIU,IJU,IKU /) )

END SUBROUTINE UPDRAFT_SOPE

!     #################################################################
      SUBROUTINE COMPUTE_MF_CLOUD(KRRL, PTHLM,PRC_UP, PFRAC_UP, PDZZ, KKLCL,&
                                  PRC_MF,PCF_MF                     )
!     #################################################################
!!
!!****  *COMPUTE_MF_CLOUD* - 
!!       compute diagnostic subgrid cumulus cloud caracteristics
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to compute the cloud fraction and 
!!      the mean cloud content associated with clouds described by the 
!!      mass flux scheme
!!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!     REFERENCE
!!     ---------
!!
!!
!!     AUTHOR
!!     ------
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!


IMPLICIT NONE

!*                    1.1  Declaration of Arguments
!
!
!
INTEGER,                  INTENT(IN)   ::  KRRL         ! number of liquid water var.
                                                        ! scheme
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PTHLM ! updraft characteritics

REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PRC_UP ! updraft characteritics
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PFRAC_UP          ! Updraft Fraction

REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PDZZ
INTEGER, DIMENSION(:,:),  INTENT(IN)   ::  KKLCL          ! index of updraft condensation level
REAL, DIMENSION(:,:,:),   INTENT(OUT)  ::  PRC_MF, PCF_MF ! cloud content and
                                                          ! cloud fraction for MF scheme

!
!                       1.2  Declaration of local variables
!
REAL, DIMENSION(SIZE(PTHLM,1),SIZE(PTHLM,2),SIZE(PTHLM,3)) ::  ZFLXZ
INTEGER  :: IKU, IKB, IKE, JI,JJ,JK
!------------------------------------------------------------------------

!                     1. INITIALISATION

!                     2.1 Internal domain

! Internal Domain
IKU=SIZE(PTHLM,3)
IKB=1
IKE=IKU-1

PCF_MF = 0.
PRC_MF = 0.


!
!                    Direct cloud scheme
!        This scheme may be activated only if the selected updraft model
!        gives the updraft fraction as an output
!        

! attention, les variables de l'updraft sont en niveaux flux
! ils faut les passer aux niveaux masse pour calculer PRC_MF et PCF_MF
DO JI=1,SIZE(PCF_MF,1)
DO JJ=1,SIZE(PCF_MF,2)

DO JK=KKLCL(JI,JJ),IKE

PCF_MF(JI,JJ,JK ) = XKCF_MF *0.5* (       &
  &    PFRAC_UP(JI,JJ,JK) +  PFRAC_UP(JI,JJ,JK+1) )
PRC_MF(JI,JJ,JK)  = 0.5* XKCF_MF * ( PFRAC_UP(JI,JJ,JK)*PRC_UP(JI,JJ,JK)  &
                         + PFRAC_UP(JI,JJ,JK+1)*PRC_UP(JI,JJ,JK+1) )


END DO
END DO
END DO


END SUBROUTINE COMPUTE_MF_CLOUD


!     #################################################################
      SUBROUTINE mfshconvpblinit(massflux_EDKF, entr_EDKF, detr_EDKF & 
                                     ,thl_up, thv_up, rt_up       &
                                     ,rv_up, rc_up, u_up, v_up    &
                                     ,frac_up,RESTART,ALLOWED_TO_READ,     &
     &                      IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE                 )
!     #################################################################

      IMPLICIT NONE

      LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT),OPTIONAL ::             &
     &                                        MASSFLUX_EDKF, ENTR_EDKF, DETR_EDKF &
     &                                       ,THL_UP, THV_UP, RT_UP, RV_UP        &
     &                                       ,RC_UP, U_UP, V_UP, FRAC_UP 

      INTEGER :: I,J,K,ITF,JTF,KTF

!---------------------------------------------
      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)

!     IF(.NOT.RESTART)THEN
      IF( PRESENT (MASSFLUX_EDKF) ) THEN
        DO J=JTS,JTF
        DO K=KTS,KTF
        DO I=ITS,ITF
          MASSFLUX_EDKF(I,K,J)=0.
          ENTR_EDKF(I,K,J)=0.
          DETR_EDKF(I,K,J)=0.
          THL_UP(I,K,J)=0.
          THV_UP(I,K,J)=0.
          RT_UP(I,K,J)=0.
          RV_UP(I,K,J)=0.
          RC_UP(I,K,J)=0.
          U_UP(I,K,J)=0.
          V_UP(I,K,J)=0.
          FRAC_UP(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF

END SUBROUTINE mfshconvpblinit


!     #########################################################
      SUBROUTINE BL89(PZZ,PDZZ,PTHVREF,PTHLM,KRR, &
                 PRM,PTKEM,PLM)
!     #########################################################
!
!!****  *BL89* -
!!
!!    PURPOSE
!!    -------
!!    This routine computes the mixing length from Bougeault-Lacarrere 89
!!    formula.
!!
!!**  METHOD
!!    ------
!!
!*       0.1   Declaration of arguments
!              ------------------------
!
INTEGER,                INTENT(IN)   :: KRR

REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PZZ
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PDZZ
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PTHVREF
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTKEM       ! TKE
!   thermodynamical variables PTHLM=Theta at the begining
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTHLM     ! conservative pot. temp.
REAL, DIMENSION(:,:,:,:), INTENT(IN) ::  PRM       ! water var.                 
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PLM       ! Mixing length

!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER :: IKB,IKE

REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZRTM
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) ::ZVPT  ! Virtual Potential Temp at half levels
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2)) ::  ZLWORK
!           ! downwards then upwards vertical displacement,
REAL, DIMENSION(SIZE(PTKEM,1)*SIZE(PTKEM,2),SIZE(PTKEM,3)) :: ZZZ,ZDZZ,&
                                                              ZG_O_THVREF, &
                                                              ZTHM,ZTKEM,ZLM,&
                                                              ZLMUP,ZLMDN,&
                                                              ZLMTEST
!           ! input and output arrays packed according one horizontal coord.
REAL, DIMENSION(SIZE(PRM,1)*SIZE(PRM,2),SIZE(PRM,3),SIZE(PRM,4)) :: ZRM
!           ! input array packed according one horizontal coord.
REAL, DIMENSION(SIZE(PRM,1)*SIZE(PRM,2),SIZE(PRM,3)) :: ZSUM ! to replace SUM function

REAL   :: ZPOTE,ZLWORK1,ZLWORK2

INTEGER :: IIU,IJU,IKU,IICE
INTEGER :: J1D        ! horizontal loop counter
INTEGER :: JK,JKK     ! loop counters
INTEGER :: JRR        ! moist loop counter
REAL    :: ZRVORD     ! Rv/Rd (=1/0.622 cf glossaire)
LOGICAL :: GUPORDN    !Test for computation of upward or downward mixing length
REAL    :: Z2SQRT2
!-------------------------------------------------------------------------------
!
Z2SQRT2=2.*SQRT(2.)
IIU=SIZE(PTKEM,1)
IJU=SIZE(PTKEM,2)
!
IKB =  1
IKE = SIZE(PTKEM,3)-1
IKU = SIZE(PTKEM,3)
ZRVORD = XRV / XRD
!-------------------------------------------------------------------------------
!
!*       1.    pack the horizontal dimensions into one
!              ---------------------------------------
!
DO JK=1,IKU
  ZZZ    (:,JK)   = RESHAPE(PZZ    (:,:,JK),(/ IIU*IJU /) )
  ZDZZ   (:,JK)   = RESHAPE(PDZZ   (:,:,JK),(/ IIU*IJU /) )
  ZTHM   (:,JK)   = RESHAPE(PTHLM  (:,:,JK),(/ IIU*IJU /) )
  ZTKEM  (:,JK)   = RESHAPE(PTKEM  (:,:,JK),(/ IIU*IJU /) )
  ZG_O_THVREF(:,JK)   = RESHAPE(XG/PTHVREF(:,:,JK),(/ IIU*IJU /) )
  DO JRR=1,KRR
    ZRM  (:,JK,JRR) = RESHAPE(PRM    (:,:,JK,JRR),(/ IIU*IJU /) )
  END DO
END DO
!-------------------------------------------------------------------------------
!
!*       2.    Virtual potential temperature on the model grid
!              -----------------------------------------------
!
IF(KRR /= 0) THEN
  ZSUM(:,:) = 0.
  DO JRR=1,KRR
    ZSUM(:,:) = ZSUM(:,:)+ZRM(:,:,JRR)
  ENDDO
  ZVPT(:,1:)=ZTHM(:,:) * ( 1. + ZRVORD*ZRM(:,:,1) )  &
                           / ( 1. + ZSUM(:,:) )
ELSE
  ZVPT(:,1:)=ZTHM(:,:)
END IF
!
!-------------------------------------------------------------------------------
!
!*       3.  loop on model levels
!            --------------------
!
DO JK=IKB,IKE
!-------------------------------------------------------------------------------
!
!*       4.  mixing length for a downwards displacement
!            ------------------------------------------
   GUPORDN=.FALSE.
   CALL COMPUTE_BL89_ML(ZDZZ,ZTKEM,ZG_O_THVREF,ZVPT,JK,GUPORDN,ZLWORK)



!-------------------------------------------------------------------------------
!
!*       5.  intermediate storage of the final mixing length
!            -----------------------------------------------
!
   ZLMDN(:,JK)=MIN(ZLWORK(:),0.5*(ZZZ(:,JK)+ZZZ(:,JK+1))-ZZZ(:,IKB))
!
!-------------------------------------------------------------------------------
!
!*       6.  mixing length for an upwards displacement
!            -----------------------------------------
 
   GUPORDN=.TRUE.
   CALL COMPUTE_BL89_ML(ZDZZ,ZTKEM,ZG_O_THVREF,ZVPT,JK,GUPORDN,ZLWORK)

   ZLMUP(:,JK)=ZLWORK(:)

!-------------------------------------------------------------------------------
! 
  DO J1D=1,IIU*IJU
    ZLWORK1=MAX(ZLMDN(J1D,JK),1.E-10)
    ZLWORK2=MAX(ZLMUP(J1D,JK),1.E-10)
    ZPOTE = ZLWORK1 / ZLWORK2
    ZLWORK2=1.d0 + ZPOTE**(2./3.)
    ZLM(J1D,JK) = Z2SQRT2*ZLWORK1/(ZLWORK2*SQRT(ZLWORK2))
  END DO

ZLM(:,JK)=( 0.5* (MAX(ZLMDN(:,JK),1.E-10)**(-2./3.)+MAX(ZLMUP(:,JK),1.E-10)**(-2./3.)) )**(-1.5)
ZLM(:,JK)=MAX(ZLM(:,JK),XLINI)

!*       8.  end of the loop on the vertical levels
!            --------------------------------------
!
END DO
!
!-------------------------------------------------------------------------------
!
!*       9.  boundaries
!            ----------
!
ZLM(:,IKE)  =ZLM(:,IKE-1)
ZLM(:,IKE+1)=ZLM(:,IKE-1)
ZLMUP(:,IKE)  =ZLMUP(:,IKE-1)
ZLMUP(:,IKE+1)=ZLMUP(:,IKE-1)
ZLMDN(:,IKE)  =ZLMDN(:,IKE-1)
ZLMDN(:,IKE+1)=ZLMDN(:,IKE-1)
!
  DO JK=1,IKU
    PLM  (:,:,JK)   = RESHAPE(ZLM  (:,JK), (/ IIU,IJU /) )
  END DO

END SUBROUTINE BL89
                        
END  MODULE MODULE_BL_MFSHCONVPBL

