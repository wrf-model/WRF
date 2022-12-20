!WRF:MODEL_MP:PHYSICS
!
! To improve cloud and precipitation forecast we developed new cloud prediction
! scheme and we implemented it in WRF model. Fractional cloud cover, cloud liquid
! water, cloud ice and cloud snow are explicitly predicted by adding three
! prognostic equations for fractional cloud cover, cloud mixing ratio and snow per
! cloud fraction to the model. Sedimentation of ice and snow is also included in
! parameterization. Precipitation of rain and snow are determined from cloud
! fields. Clouds predicted like this can be used also in radiation
! parameterization.
! Thermodynamic wet bulb temperature will be used for describing clouds because it
! is constant during water phase changes. By using this temperature moist static
! energy of model grid box and cloudy part inside the grid box is the same and
! principle of energy conservation is satisfied.
! A complete description is now found in Ristic I., Kordic I., 2018:
! Cloud parameterization and cloud prediction scheme in the Eta numerical
! weather model. NWCM - Serbian Academy of Sciences and Arts.
! Integration of the model for test cases indicate that new cloud prediction
! scheme improved forecast compared to the original model. New fractional cloud
! cover formula showed good results in practice, since the fractional cloud cover,
! predicted in this way, was much closer to the real cloud cover values.
! Significant progress has been made in stratiform precipitation forecast.
! Positive impact on convection scheme is also noticed.
!
! Author: Ivan Ristic, WEATHER2, ivanr@weather2.rs
! Last modified: 23 October 2020
!
MODULE module_mp_ivanr_micro
!
      CONTAINS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE IVANRM_NEW (itimestep,DT,                              &
     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,qv,qt,     &
     &                      SR,                                         &
     &                      QC,QS,QR,                                   &
     &                      xland,                                      &
     &                      CLD,T0,P0,Q0,F_SNOW,                        &
     &                      RAINNC,RAINNCV,                             &
     &                      SNOWNC,SNOWNCV,                             &
     &                      ids,ide, jds,jde, kds,kde,		        &
     &                      ims,ime, jms,jme, kms,kme,		        &
     &                      its,ite, jts,jte, kts,kte )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,ITIMESTEP
      REAL, INTENT(IN) 	    :: DT
      REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      dz8w,rho_phy,p_phy,pi_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme)::      &
     &                      th_phy,qv,qt
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::    &
     &                      QC,QS,QR
      REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: XLAND
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::    &
     &                      CLD,T0,P0,Q0,F_SNOW
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)           ::     &
     &                      RAINNC,RAINNCV,SNOWNC,SNOWNCV
      REAL, INTENT(OUT),    DIMENSION(ims:ime,jms:jme):: SR
!-----------------------------------------------------------------------
!     LOCAL VARS
!-----------------------------------------------------------------------
      REAL,DIMENSION(its:ite, kts:kte, jts:jte):: T_PHY,Q_PHY
      REAL,DIMENSION(kts:kte) ::                                        &
     &  P_col, QV_col, T_col, THICK_col, WC_col,                        &
     &  CLD_col, T0_col, P0_col, Q0_col, F_SNOW_col
      REAL :: ASNOW,ARAIN
      REAL :: SEAMASK
      INTEGER :: I,J,K,KFLIP
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
        T_PHY(i,k,j)=th_phy(i,k,j)*pi_phy(i,k,j)
        Q_PHY(i,k,j)=qv(i,k,j)/(1.+qv(i,k,j)) !Convert to specific humidity
      ENDDO
      ENDDO
      ENDDO

      IF (itimestep .eq. 1) THEN
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
          CLD(i,k,j)=0.
          T0(i,k,j)=T_PHY(i,k,j)
          P0(i,k,j)=P_PHY(i,k,j)
          Q0(i,k,j)=Q_PHY(i,k,j)
          F_SNOW(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
        QT(I,K,J)=QC(I,K,J)+QR(I,K,J)+QS(I,K,J)
      ENDDO
      ENDDO
      ENDDO

      DO 100 J = JTS,JTE
      DO 100 I = ITS,ITE
!
      DO K = KTS,KTE
        KFLIP=KTE+KTS-K
        P_col(K)=P_PHY(I,KFLIP,J)
        THICK_col(K)=RHO_PHY(I,KFLIP,J)*dz8w(I,KFLIP,J)
        T_col(K)=T_PHY(I,KFLIP,J)
        QV_col(K)=Q_PHY(I,KFLIP,J)
        WC_col(K)=QT(I,KFLIP,J)
        CLD_col(K)=CLD(I,KFLIP,J)
        T0_col(K)=T0(I,KFLIP,J)
        P0_col(K)=P0(I,KFLIP,J)
        Q0_col(K)=Q0(I,KFLIP,J)
        F_SNOW_col(K)=F_SNOW(I,KFLIP,J)
      ENDDO
!
      SEAMASK=XLAND(I,J)-1.
!
!#######################################################################
   !
   !--- Perform the microphysical calculations in this column
   !
       CALL IVANRMCOLUMN ( ARAIN, ASNOW, DT, KTS, KTE,                  &
     & P_col, QV_col, T_col,                                            &
     & THICK_col, WC_col,                                               &
     &  CLD_col,T0_col,P0_col,Q0_col,F_SNOW_col,SEAMASK )
   !
!#######################################################################
!
   !
   !--- Update storage arrays
   !
      DO K = KTS,KTE
        KFLIP=KTE+KTS-K
        T_PHY(I,KFLIP,J)=T_col(K)
        Q_PHY(I,KFLIP,J)=QV_col(K)
        QT(I,KFLIP,J)=WC_col(K)
        CLD(I,KFLIP,J)=CLD_col(K)
        T0(I,KFLIP,J)=T0_col(K)
        P0(I,KFLIP,J)=P0_col(K)
        Q0(I,KFLIP,J)=Q0_col(K)
        F_SNOW(I,KFLIP,J)=F_SNOW_col(K)
      ENDDO
   !
   !--- Update accumulated precipitation statistics
   !
   !--- Surface precipitation statistics; SR is fraction of surface 
   !    precipitation (if >0) associated with snow
   !
      RAINNCV(I,J)=ARAIN+ASNOW
      RAINNC(I,J)=RAINNC(I,J)+RAINNCV(I,J)
      IF(RAINNCV(I,J) .LT. 1.E-8) THEN
        SR(I,J)=0.
      ELSE
        SR(I,J)=ASNOW/RAINNCV(I,J)
      ENDIF
      SNOWNCV(I,J)=ASNOW
      SNOWNC(I,J)=SNOWNC(I,J)+SNOWNCV(I,J)
!#######################################################################
!#######################################################################
!
100   CONTINUE                          ! End "I" & "J" loops

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
        th_phy(i,k,j)=T_PHY(i,k,j)/pi_phy(i,k,j)
        qv(i,k,j)=Q_PHY(i,k,j)/(1.-Q_PHY(i,k,j))  !Convert to mixing ratio
        QC(I,K,J)=QT(I,K,J)
        QS(I,K,J)=0.
        QR(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------

  END SUBROUTINE IVANRM_NEW

!-----------------------------------------------------------------------
      SUBROUTINE IVANRMCOLUMN ( ARAIN, ASNOW, DTPH, KTS, KTE,           &
     & P_col,QV_col,T_col,                                              &
     & THICK_col, WC_col,                                               &
     &  CLD_col,T0_col,P0_col,Q0_col,F_SNOW_col,SEAMASK )
!
!###############################################################################
!###############################################################################
!
!-------------------------------------------------------------------------
!--------------- Arrays & constants in argument list ---------------------
!-------------------------------------------------------------------------
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN) :: KTS, KTE
      REAL,INTENT(INOUT) ::  ARAIN, ASNOW
      REAL,INTENT(IN)    :: DTPH
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) ::                          &
     &  P_col, QV_col, T_col, THICK_col, WC_col,                        &
     &  CLD_col, T0_col, P0_col, Q0_col, F_SNOW_col
      REAL, INTENT(IN) :: SEAMASK
!-----------------------------------------------------------------------
      REAL,PARAMETER :: A2=17.2693882,A3=273.16,A4=35.86                &
     &, PQ0=379.90516,TRESH=.95                                         &
     &, CP=1004.6,ELWV=2.50E6,ELIV=2.834E6,ROW=1.E3,G=9.8               &
     &, EPSQ=1.E-12,DLDT=2274.0,TM10=263.16                             &
     &, R=287.04,RV=461.5,EPS1=RV/R-1.                                  &
     &, CPR=CP*R,RCPR=1./(CPR),ELIW=ELIV-ELWV,CH2O=4.2E6                &
     &, ARCP=A2*(A3-A4)/CP,RCP=1./CP,PQ0C=PQ0*TRESH,RROG=1./(ROW*G)     &
     &, RROW=1./ROW,US=1.,EPS=0.622,CCLIMIT=1.0E-3,C0=0.15,             &
     &  CWS=0.025,EK=100.

      DOUBLE PRECISION DD(0:4),sol,P,DIS
!
!-----------------------------------------------------------------------
!--- Local variables
!-----------------------------------------------------------------------
!
      INTEGER :: L,ITCNT
!
      REAL :: RDTPH,TWODT,RTWODT,CLIMIT,FINV,PRECRL1,PRECSL1,      &
     &        PRECIL1,TKL,QKL,CWMKL,PKL,TMTO,FCLOUD,QV,WV,WC,FSNOW,     &
     &        ELV,FIW,QCi,QW,TMT0i,TG1,FWI,THICK,RTHICK,CPDR,PRECAK,    &
     &        ULL,U00KL,QC,RQKL_1,RQKL_C,RQKL_0,TKL_0,QKL_0,PKL_0,      &
     &        FCI,TT,QQ,PP,WW,AC,AB,CCRKL,RQKL,DRQ0,CONE0,CCRKL_0,CR,   &
     &        AA2,EXPF,WMINK,PRECRL,PRECSL,PRECIL,PRAUT,PSAUT,PRACW,    &
     &        PSACI,ERR,ERS,PSM,PSM1,PSM2,PPR,PPS,CONDE,RCONDE,RHO,     &
     &        DIFFUS,PRECRK,PRECSK,PRECIK,PID,VICE,AA1,CS,AMAXCM,QCw,   &
     &        VENTR,VENTS,EVPR,EVPS,CRS,ERK,factor,AMAXPS,VSNOW,RP,TGS, &
     &        FO,TGUESS,DTG,AI,BI,TMT0,QC_0,FLUNI,CWMK,FI,F1,           &
     &        PSFC,U00,COEF,AB_0,CCRKL_1
!
!#######################################################################
!########################## Begin Execution ############################
!#######################################################################
!
!
      RDTPH=1./DTPH
      TWODT=DTPH
      RTWODT=1./TWODT
      CLIMIT=EPSQ
      FINV=1.
      PRECRL1=0.
      PRECSL1=0.
      PRECIL1=0.

      ARAIN=0.                ! Accumulated rainfall into grid box from above (kg/m**2)
      ASNOW=0.                ! Accumulated snowfall into grid box from above (kg/m**2)

      PSFC=P_col(KTE)+0.5*G*THICK_col(KTE)
!
!-----------------------------------------------------------------------
!------------ Loop from top (L=KTS) to surface (L=KTE) -----------------
!-----------------------------------------------------------------------
!
      DO  L=KTS,KTE
        IF (QV_col(L).LE.EPSQ)QV_col(L)=EPSQ
        IF (WC_col(L).LE.EPSQ)WC_col(L)=0.
      END DO

      DO 90  L=KTS,KTE
 
      TKL=T_col(L)
      TMT0=TKL-273.16
      PKL=P_col(L)
      QKL=QV_col(L)
      CWMKL=WC_col(L)
      FSNOW=F_SNOW_col(L)
      IF(CWMKL.LE.CLIMIT)FSNOW=0.
      FCLOUD=0.
      FWI=0.
 
      TKL_0=T0_col(L)
      QKL_0=Q0_col(L)
      PKL_0=P0_col(L)
!-----------------------------------------------------------------------
!--------------CIRRUS CLOUD ICE SUPERSATURATION-------------------------
!-----------------------------------------------------------------------
      IF(PKL.LT.35000..AND.TMT0.LT.-38.)THEN
        FCI=1.25
      ELSE
        FCI=1.
      ENDIF
!-----------------------------------------------------------------------
!-----ULL, U00----------------------------------------------------------
!-----------------------------------------------------------------------
      IF(L.GE.KTE-10.AND.L.LE.KTE)THEN
        ULL=0.1*FLOAT(L-KTE+10)
      ELSE
        ULL=0.
      ENDIF

      U00=(1.-SEAMASK)*0.75+SEAMASK*0.80
!-----------------------------------------------------------------------
!-----QCi---------------------------------------------------------------
!-----------------------------------------------------------------------
      TT=TKL
      QQ=QKL
      PP=PKL
      ELV=ELIV
      FIW=1.
      CALL FSLOPE
      QCi=QW
      TMT0i=TG1-273.16
!-----------------------------------------------------------------------
!-----ICE-WATER ID NUMBER IW--------------------------------------------
!-----------------------------------------------------------------------
      IF(TMT0i.LT.-15.)THEN
        FIW=1.
      ELSEIF(TMT0i.GE.0.)THEN
        FIW=0.
      ELSE
        FIW=1.
        U00KL=U00+ULL*(0.95-U00)*FINV
        FI=QKL-U00KL*QSAT(PKL,TKL,FIW,FCI)
        IF(PRECIL1.LE.0..OR.FI.LE.0.)FIW=0.
      ENDIF
 
      FWI=1.-FIW
!-----------------------------------------------------------------------
!-----SNOW IS NOT PART OF CLOUD WATER CLOUDS----------------------------
!-----------------------------------------------------------------------
      THICK=THICK_col(L)
      RTHICK=1./THICK
      CPDR=FSNOW*CWMKL*(1.-FIW)
      PRECAK=CPDR*THICK
      CWMKL=CWMKL-CPDR
      FSNOW=FSNOW*FIW
!-----------------------------------------------------------------------
!-----ELV---------------------------------------------------------------
!-----------------------------------------------------------------------
      ELV=(1.-FIW)*ELWV+FIW*ELIV
!-----------------------------------------------------------------------
!-----QC, QC_0----------------------------------------------------------
!-----------------------------------------------------------------------
      TT=TKL
      QQ=QKL
      PP=PKL
      CALL FSLOPE
      QC=QW
      TMT0=TG1-273.16
 
      TT=TKL_0
      QQ=QKL_0
      PP=PKL_0
      CALL FSLOPE
      QC_0=QW
!-----------------------------------------------------------------------
!-----U00KL-------------------------------------------------------------
!-----------------------------------------------------------------------
      U00KL=U00+ULL*(0.95-U00)*FINV
 
      TT=TKL
      QQ=QKL
      PP=PKL/U00KL
      CALL FSLOPE
      U00KL=QW/QC
!-----------------------------------------------------------------------
!-----RQKL_0, RQKL_1----------------------------------------------------
!-----------------------------------------------------------------------
      RQKL_1=QKL/QC
      RQKL_C=CWMKL/QC
      RQKL_0=AMAX1(AMIN1(QKL_0/QC_0,RQKL_1),U00KL)
      RQKL_C=AMIN1(RQKL_C,AMAX1(0.,RQKL_1+RQKL_C-U00KL))
      RQKL_1=AMAX1(RQKL_1,U00KL)
!-----------------------------------------------------------------------
!-----AB_0--------------------------------------------------------------
!-----------------------------------------------------------------------
      AB_0=(RQKL_1+RQKL_C-U00KL)/US
!-----------------------------------------------------------------------
!-----COEF--------------------------------------------------------------
!-----------------------------------------------------------------------
      COEF=2./US
!-----------------------------------------------------------------------
!-----CCRKL_1-----------------------------------------------------------
!-----------------------------------------------------------------------
      AC=US-RQKL_1
      AB=AB_0+COEF*RQKL_C
 
      IF(AB.LE.0.)THEN
        CCRKL_1=0.
      ELSEIF(AC.LE.0.)THEN
        CCRKL_1=1.
      ELSE
        DD(0)=-RQKL_1*COEF*RQKL_C
        DD(2)=US*AB_0
        DD(1)=-DD(0)-DD(2)+AC*(1.+AB)
        IF(DD(2).NE.0.D+0)THEN
          P=5.D-1*DD(1)/DD(2)
          DIS=P**2-DD(0)/DD(2)
          sol=-P+SQRT(DIS)
        ELSE
          sol=-DD(0)/DD(1)
        ENDIF
        CCRKL_1=sol
      ENDIF
!-----------------------------------------------------------------------
!-----DRQ0--------------------------------------------------------------
!-----------------------------------------------------------------------
      DRQ0=CCRKL_1*(RQKL_1-RQKL_0)
      RQKL_0=RQKL_1-DRQ0
      DRQ0=RQKL_1-RQKL_0+RQKL_C
!-----------------------------------------------------------------------
!-----CCRKL, CONE0------------------------------------------------------
!-----------------------------------------------------------------------
      AC=US-RQKL_0
      AB=CCRKL_1*AB_0+COEF*DRQ0
 
      IF(CCRKL_1.LE.0.)THEN
        CCRKL=0.
        RQKL=U00KL
      ELSEIF(AC.LE.0.)THEN
        CCRKL=1.
        RQKL=US
      ELSE
        DD(1)=AC*(CCRKL_1+AB)-US*AB*(1.-CCRKL_1)
        IF(DD(1).LT.0.D+0)THEN
          DRQ0=RQKL_1-RQKL_0
          AB=CCRKL_1*AB_0+COEF*DRQ0
        ELSE
          RQKL_C=0.
        ENDIF
        DD(0)=(AC-DRQ0-US)*COEF*RQKL_C
        DD(2)=DRQ0*CCRKL_1+(CCRKL_1*US+DRQ0)*AB
        DD(1)=-DD(0)-DD(2)+DD(1)
        IF(DD(2).NE.0.D+0)THEN
          P=5.D-1*DD(1)/DD(2)
          DIS=P**2-DD(0)/DD(2)
          sol=-P+SQRT(DIS)
        ELSE
          sol=-DD(0)/DD(1)
        ENDIF
        CCRKL=sol*CCRKL_1
        RQKL=RQKL_0+(1.-sol)*DRQ0
      ENDIF
 
      CONE0=(QKL/QC-RQKL)*QC
 
      IF(CONE0+CWMKL.LE.CLIMIT.OR.CCRKL.LE.0.)THEN
        CCRKL=0.
        RQKL=U00KL
        CONE0=-CWMKL
        FCLOUD=1.
        FWI=1.
      ENDIF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     ACCUMULATE LATENT HEATING DUE TO GRID-SCALE PRECIP/EVAP.
!     SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE
!     IS CALLED.  THIS PERIOD IS THE PHYSICS TIMESTEP.
!
      WC_col(L)=CONE0+CWMKL
      CLD_col(L)=CCRKL
      T_col(L)=ELV*RCP*CONE0+TKL
      QV_col(L)=-CONE0+QKL
!-----------------------------------------------------------------------
!-------------------SAVE T, P, Q FOR THIS STEP--------------------------
!-----------------------------------------------------------------------
      T0_col(L)=T_col(L)
      P0_col(L)=P_col(L)
      Q0_col(L)=QV_col(L)
!-----------------------------------------------------------------------
!----------CHOOSE THE POINTS WHERE PRECIPITATION CAN BE PRODUCED--------
!-----------------------------------------------------------------------
      CR=0.4*RTWODT
      AA2=1.0*RTWODT
      EXPF=EXP(0.025*TMT0)
      WMINK=0.1E-3*EXPF

      PRECRL=0.
      PRECSL=0.
      PRECIL=0.
      PRAUT=0.
      PSAUT=0.
      PRACW=0.
      PSACI=0.
      ERR  =0.
      ERS  =0.
      PSM  =0.
      PSM1 =0.
      PSM2 =0.
      PPR  =0.
      PPS  =0.
      CPDR =0.

      TT=T_col(L)
      QQ=QV_col(L)
      WW=WC_col(L)

      CONDE=THICK_col(L)*TWODT
      RCONDE=1./CONDE

      PP=PKL
      RHO=PP/(R*TT*(1.+0.608*QQ))
      DIFFUS=8.794E-5*TT**1.81/PP

      THICK=THICK_col(L)
      RTHICK=1./THICK
      CPDR=FSNOW*WW
      PRECAK=CPDR*THICK+PRECAK
      WW=WW-CPDR
      FSNOW=0.

      PRECRK=AMAX1(0.,PRECRL1)
      PRECSK=AMAX1(0.,PRECSL1)+PRECAK
      PRECIK=AMAX1(0.,PRECIL1)
      IF(WW.LT.CLIMIT.AND.(PRECRK+PRECSK+PRECIK).EQ.0.)THEN
        PID=0.
      ELSE
        PID=1.
      ENDIF
!-----------------------------------------------------------------------
!-----SEDIMENTATION OF ICE----------------------------------------------
!-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        IF(FWI.EQ.0..AND.PP+25.E2.LT.PSFC)THEN
          PRECIK=AMAX1(0.,PRECIL1)+WW*THICK
          RHO=PP/(R*TT*(1.+0.608*QQ))
          VICE=0.15*(PP/300.E2)**(-0.178)*(TT/233.)**(-0.394)
          AA1=RHO*VICE*RTHICK
          FLUNI=AA1*TWODT/(1.+AA1*TWODT)
          WW=(1.-FLUNI)*PRECIK*RTHICK
          PRECIL=FLUNI*PRECIK
        ELSE
          PRECSL1=PRECSL1+PRECIL1
          PRECIL1=0.
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
!------------------PRECIPITATION PRODUCTION RATES-----------------------
!------------------AUTO-CONVERT RATES-----------------------------------
!-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        CWMK=AMAX1(0.,WW-CLIMIT)
        IF(FWI.EQ.0.)THEN
          EXPF=EXP(0.025*TMT0)
          IF(TT.LE.250.16)THEN
            AA1=1.E-3*EXPF
          ELSE
            AA1=0.167E-3*(1.+0.5*SQRT(AMAX1(0.,268.-TT)))
          ENDIF
          PSAUT=AA1*AMAX1(0.,CWMK-WMINK)
          CPDR=-PSAUT*TWODT
          IF(-CPDR.GE.CWMK)THEN
            CPDR=-CWMK
            PSAUT=-CPDR*RTWODT
          ENDIF
        ELSE
          AMAXCM=AMAX1(0.,CWMK-WMINK)
          PRAUT=C0*AMAXCM*AMAXCM
          CPDR=-PRAUT*TWODT
          IF(-CPDR.GE.CWMK)THEN
            CPDR=-CWMK
            PRAUT=-CPDR*RTWODT
          ENDIF
        ENDIF
        PPR=PRAUT*CONDE
        PPS=PSAUT*CONDE
      ENDIF

      IF(PID.EQ.1.)THEN
        WW=CPDR+WW
        PRECRL=PRECRL1+PPR
        PRECSL=PRECSL1+PRECAK+PPS
      ENDIF
!-----------------------------------------------------------------------
!-----------------------ACCRETIONS--------------------------------------
!-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        CWMK=WW
        PRECRK=AMAX1(0.,PRECRL1)
        PRECSK=AMAX1(0.,PRECSL1)
        IF(FWI.EQ.0.)THEN
          EXPF=EXP(0.025*TMT0)
          CS=AA2*EXPF
          PSACI=CS*AMAX1(0.,CWMK)*PRECSK
          CPDR=-PSACI*TWODT
          IF(-CPDR.GE.CWMK)THEN
            CPDR=-CWMK
            PSACI=-CPDR*RTWODT
          ENDIF
        ELSE
          PSM2=CWS*CR*AMAX1(0.,CWMK)*PRECSK
          PRACW=CR*AMAX1(0.,CWMK)*(PRECRK+PRECSK)
          CPDR=-PRACW*TWODT
          IF(-CPDR.GE.CWMK)THEN
            CPDR=-CWMK
            PRACW=-CPDR*RTWODT
          ENDIF
        ENDIF
        PPR=PRACW*CONDE
        PPS=PSACI*CONDE
      ENDIF

      IF(PID.EQ.1.)THEN
        WW=CPDR+WW
        PRECRL=PRECRL+PPR
        PRECSL=PRECSL+PPS
      ENDIF
!-----------------------------------------------------------------------
!-----EVAPORATION/CONDENSATION OF PRECIPITATION-------------------------
!***** ERR & ERS POSITIVE--EVAPORATION
!***** ERR & ERS NEGTIVE---CONDENSATION
!-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        PRECRK=AMAX1(0.,PRECRL)
        PRECSK=AMAX1(0.,PRECSL)
!-----------------------------------------------------------------------
! INCREASE THE EVAPORATION/CONDENSATION FOR STRONG/LIGHT PREC
!-----------------------------------------------------------------------
        ELV=ELWV
        FIW=0.
        CALL FSLOPE
        QCw=QW

        ELV=ELIV
        FIW=1.
        CALL FSLOPE
        QCi=QW
        TMT0i=TG1-273.16

        VENTR=EK*SQRT(1.*SQRT(RHO)*PRECRK*RTWODT)
        VENTS=EK*SQRT(4.*SQRT(RHO)*PRECSK*RTWODT)

        VENTR=VENTR*AMAX1(1.,AMIN1(2.,VENTR))
        VENTS=VENTS*AMAX1(1.,AMIN1(2.,VENTS))

        EVPR=DIFFUS*VENTR
        EVPS=DIFFUS*VENTS

        ERR=EVPR*AMAX1(0.,QCw-QQ)
        ERS=EVPS*AMAX1(0.,QCi-QQ)

        IF(TMT0i.GT.0.)THEN
          CRS=EVPS*AMIN1(0.,QCi-QQ)
        ELSE
          CRS=0.
        ENDIF

        ERK=AMAX1(0.,U00KL*QC-QQ)*RTWODT-CRS

        IF(ERR+ERS.GT.ERK)THEN
          factor=ERK/(ERR+ERS)
          ERR=ERR*factor
          ERS=ERS*factor
        ENDIF

        ERS=ERS+CRS

        PPR=-ERR*CONDE
        PPS=-ERS*CONDE

        IF(-PPR.GE.PRECRK)THEN
          PPR=-PRECRK
          ERR=-PPR*RCONDE
        ENDIF

        IF(-PPS.GE.PRECSK)THEN
          PPS=-PRECSK
          ERS=-PPS*RCONDE
        ENDIF

      ENDIF

      IF(PID.EQ.1.)THEN
        PRECRL=PRECRL+PPR
        PRECSL=PRECSL+PPS
      ENDIF
!-----------------------------------------------------------------------
!--------------------MELTING OF THE SNOW--------------------------------
!-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        CWMK=WW
        AMAXPS=AMAX1(0.,PRECSL)

        IF(TMT0i.GT.0..AND.AMAXPS.GT.0.)THEN
          PSM1=EVPS*CP/ELIW*TMT0i
          PSM=PSM1+PSM2
        ELSE
          PSM1=0.
          PSM2=0.
          PSM=0.
        ENDIF

        PPR=PSM*CONDE
        PPS=-PSM*CONDE

        IF(-PPS.GE.AMAXPS)THEN
          PPS=-AMAXPS
          PPR=AMAXPS
          PSM1=-PPS*RCONDE
          PSM2=0.
          PSM=PSM1
        ENDIF

      ENDIF

      IF(PID.EQ.1.)THEN
        PRECRL=PRECRL+PPR
        PRECSL=PRECSL+PPS
      ENDIF
!-----------------------------------------------------------------------
!---------------UPDATE T AND Q------------------------------------------
!-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        TT=-RCP*(ELWV*ERR+ELIV*ERS+ELIW*PSM1)*TWODT+TT
        QQ=(ERR+ERS)*TWODT+QQ
      ENDIF
!-----------------------------------------------------------------------
!-----SEDIMENTATION OF SNOW---------------------------------------------
!-----------------------------------------------------------------------
      IF(PID.EQ.1.)THEN
        IF(FWI.EQ.0..AND.PP+25.E2.LT.PSFC)THEN
          PRECSK=AMAX1(0.,PRECSL)
          RHO=PP/(R*TT*(1.+0.608*QQ))
          VSNOW=1.0*EXP(0.025*TMT0)
          AA1=RHO*VSNOW*RTHICK
          FLUNI=AA1*TWODT/(1.+AA1*TWODT)
          CPDR=(1.-FLUNI)*PRECSK*RTHICK
          WW=CPDR+WW
          PRECSL=FLUNI*PRECSK
          IF(CPDR.GT.0.)FSNOW=CPDR/WW
        ENDIF
      ENDIF
 
      PRECRL1=PRECRL
      PRECSL1=PRECSL
      PRECIL1=PRECIL
!
!     ACCUMULATE LATENT HEATING DUE TO GRID-SCALE PRECIP/EVAP.
!     SCALE BY THE RECIPROCAL OF THE PERIOD AT WHICH THIS ROUTINE
!     IS CALLED.  THIS PERIOD IS THE PHYSICS TIMESTEP.
!
      T_col(L)=TT
      QV_col(L)=QQ
      WC_col(L)=WW
      F_SNOW_col(L)=FSNOW
 
   90 CONTINUE
!-----------------------------------------------------------------------
!-------------------THE PRECIPITATION ON SFC----------------------------
!-----------------------------------------------------------------------
      ARAIN=PRECRL1
      ASNOW=PRECSL1
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                     RETURN
      CONTAINS
      SUBROUTINE FSLOPE
      RP=TT+RCP*ELV*QQ
      TGS=TT
      QW=QSAT(PP,TGS,FIW,FCI)
      IF(FCLOUD*FWI*FIW*TT.GT.A3)QW=AMIN1(QQ,QW)
      FO=TGS+RCP*ELV*QW-RP
      TG1=TGS-.5*FO
      TGUESS=TGS
      ITCNT=0
   10 QW=QSAT(PP,TG1,FIW,FCI)
      IF(FCLOUD*FWI*FIW*TT.GT.A3)QW=AMIN1(QQ,QW)
      F1=TG1+RCP*ELV*QW-RP
      IF(ABS(F1).LT..001.OR.ABS(F1-FO).LT.1.E-10.OR.ITCNT.GT.30)GOTO 25
      ITCNT=ITCNT+1
      DTG=F1*(TG1-TGUESS)/(F1-FO)
      TGUESS=TG1
      FO=F1
      TG1=TG1-DTG
      GOTO 10
   25 CONTINUE
      END SUBROUTINE FSLOPE
      END SUBROUTINE IVANRMCOLUMN
!-----------------------------------------------------------------------
      REAL FUNCTION QSAT(PP,TT,FIW,FCI)
!-----------------------------------------------------------------------
      REAL, PARAMETER :: PQ0=379.90516,A2=17.2693882,A3=273.16,A4=35.86
!-----------------------------------------------------------------------
!-----AI, BI------------------------------------------------------------
!-----------------------------------------------------------------------
      TMT0=TT-A3

      IF(TMT0.LT.-20.)THEN
        AI=0.007225
        BI=0.9674
      ELSEIF(TMT0.LT.0.)THEN
        AI=0.008855
        BI=1.
      ELSE
        AI=0.
        BI=1.
        IF(FIW.EQ.1.)TMT0=0.
      ENDIF

      AI=FIW*AI*FCI
      BI=1.-FIW+FIW*BI*FCI

      QSAT=PQ0/PP*EXP(A2*TMT0/(TT-A4))*(BI+AI*TMT0)
!-----------------------------------------------------------------------
      RETURN
      END
!-----------------------------------------------------------------------
!--------------------------- Return to GSMDRIVE ------------------------
!-----------------------------------------------------------------------
      END MODULE module_mp_ivanr_micro
