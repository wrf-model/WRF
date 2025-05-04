!-----------------------------------------------------------------------
!
!WRF:MODEL_LAYER:PHYSICS
!
! To improve cloud and precipitation forecast we developed new convective
! scheme and we implemented it in WRF model. Convective clouds have always
! been a great challenge for meteorologists, among other things, due to
! the inability to describe processes of cloud formation, development and
! dissipation in a satisfactory manner. Applying parameterization in to
! the models has lead to simpler form of equations that could be used in
! practice, and thus different types of convective schemes in numerical
! weather prediction models appeared. Proposed convective scheme is based
! on basic elements that affect convection such as convective available
! potential energy (CAPE), vertical velocity at the base of the cloud, the
! amount of ice in the cloud and important assumptions. The scheme is
! conceived as a wet vertical turbulent diffusion and a logical
! continuation of dry vertical planetary boundary layer (PBL) turbulent
! diffusion. The scheme determines the vertical levels in the model where
! the convective cloud begins and ends. Integrated in the model this
! scheme showed good results in practice.
! A complete description is now found in Ristic I., Kordic I., April 2022:
! Convective velocity scale and its application in convective parametrization
!
! Author: Ivan Ristic, WEATHER2, ivanr@weather2.rs
! Last modified: 23 December 2021
!
!-----------------------------------------------------------------------
!
      MODULE MODULE_CU_RISTIC
!
!-----------------------------------------------------------------------
!
      USE MODULE_MODEL_CONSTANTS
!
!-----------------------------------------------------------------------
!
CONTAINS
!
!-----------------------------------------------------------------------
      SUBROUTINE RISTICDRV(                                             &
     &                  IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE                         &
     &                 ,USTAR                                           &
     &                 ,W,QC                                            &
     &                 ,DT,ITIMESTEP,STEPCU                             &
     &                 ,RAINCV,PRATEC,CUTOP,CUBOT                       &
     &                 ,TH,T,QV                                         &
     &                 ,PMID,PI,RHO,DZ8W                                &
     &                 ,CP,R,ELWV,ELIV,G                                &
     &                 ,CU_ACT_FLAG                                     &
     &                 ,RTHCUTEN,RQVCUTEN                               &
     &                                                                  )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     & 
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: USTAR
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: W,QC
!
      INTEGER,INTENT(IN) :: ITIMESTEP,STEPCU
!
      REAL,INTENT(IN) :: CP,DT,ELIV,ELWV,G,R
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ8W        &
     &                                                     ,PI          &
     &                                                     ,PMID,QV     &
     &                                                     ,RHO,T,TH
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                           &
     &    ,OPTIONAL                                                     &
     &    ,INTENT(INOUT) ::                        RQVCUTEN,RTHCUTEN
! 
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) ::                  &
           PRATEC,RAINCV
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CUBOT,CUTOP
!
      LOGICAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CU_ACT_FLAG
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: LBOT,LTOP
!
      REAL :: DTCNVC,PCPCOL
! 
      REAL,DIMENSION(KTS:KTE) :: DPCOL,DQDT,DTDT,PCOL,QCOL,TCOL
      REAL,DIMENSION(KTS:KTE) :: WCOL,CWMCOL
!
      INTEGER :: I,J,K,KFLIP

!***  Begin debugging convection
      REAL :: DELQ,DELT,PLYR
      INTEGER :: IMD,JMD
      LOGICAL :: PRINT_DIAG
!***  End debugging convection
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!***  PREPARE TO CALL RISTIC CONVECTION SCHEME
!
!-----------------------------------------------------------------------

!***  Begin debugging convection
      IMD=(IMS+IME)/2
      JMD=(JMS+JME)/2
      PRINT_DIAG=.FALSE.
!***  End debugging convection

!
        DO J=JTS,JTE
        DO I=ITS,ITE
          CU_ACT_FLAG(I,J)=.TRUE.
        ENDDO
        ENDDO
!
        DTCNVC=DT*STEPCU
!
        DO J=JTS,JTE  
        DO I=ITS,ITE
!
          DO K=KTS,KTE
            DQDT(K)=0.
            DTDT(K)=0.
          ENDDO
!
          PCPCOL=0.
          RAINCV(I,J)=0.
          PRATEC(I,J)=0.
!
!***  FILL 1-D VERTICAL ARRAYS 
!***  AND FLIP DIRECTION SINCE RISTIC SCHEME 
!***  COUNTS DOWNWARD FROM THE DOMAIN'S TOP
!
          DO K=KTS,KTE
            KFLIP=KTE+1-K
!
!***  CONVERT FROM MIXING RATIO TO SPECIFIC HUMIDITY
!
            QCOL(K)=MAX(EPSQ,QV(I,KFLIP,J)/(1.+QV(I,KFLIP,J)))
            TCOL(K)=T(I,KFLIP,J)
            PCOL(K)=PMID(I,KFLIP,J)
            DPCOL(K)=RHO(I,KFLIP,J)*G*DZ8W(I,KFLIP,J)
            WCOL(K)=0.5*(W(I,KFLIP,J)+W(I,KFLIP+1,J))*DT/200.
            CWMCOL(K)=QC(I,KFLIP,J)
          ENDDO
!-----------------------------------------------------------------------
!***
!***  CALL CONVECTION
!***
!-----------------------------------------------------------------------
!***  Begin debugging convection
!         PRINT_DIAG=.FALSE.
!         IF(I==IMD.AND.J==JMD)PRINT_DIAG=.TRUE.
!***  End debugging convection
!-----------------------------------------------------------------------
          CALL RISTIC(ITIMESTEP,I,J,DTCNVC                              &
     &            ,DPCOL,PCOL,QCOL,TCOL                                 &
     &            ,USTAR(I,J)                                           &
     &            ,WCOL,CWMCOL                                          &
     &            ,DQDT,DTDT,PCPCOL,LBOT,LTOP                           &
     &            ,CP,R,ELWV,ELIV,G                                     &
     &            ,PRINT_DIAG                                           &
     &            ,IDS,IDE,JDS,JDE,KDS,KDE                              &
     &            ,IMS,IME,JMS,JME,KMS,KME                              &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
! 
!***  COMPUTE HEATING AND MOISTENING TENDENCIES
!
          IF ( PRESENT( RTHCUTEN ) .AND. PRESENT( RQVCUTEN )) THEN
            DO K=KTS,KTE
              KFLIP=KTE+1-K
              RTHCUTEN(I,K,J)=DTDT(KFLIP)/PI(I,K,J)
!
!***  CONVERT FROM SPECIFIC HUMIDTY BACK TO MIXING RATIO
!
              RQVCUTEN(I,K,J)=DQDT(KFLIP)/(1.-QCOL(KFLIP))**2
            ENDDO
          ENDIF
!
!***  ALL UNITS IN RISTIC SCHEME ARE MKS, THUS CONVERT PRECIP FROM METERS
!***  TO MILLIMETERS PER STEP FOR OUTPUT.
!
          RAINCV(I,J)=PCPCOL*1.E3/STEPCU
          PRATEC(I,J)=PCPCOL*1.E3/(STEPCU * DT)
!
!***  CONVECTIVE CLOUD TOP AND BOTTOM FROM THIS CALL
!
          CUTOP(I,J)=REAL(KTE+1-LTOP)
          CUBOT(I,J)=REAL(KTE+1-LBOT)
!
!-----------------------------------------------------------------------
!
        ENDDO
        ENDDO
!
      END SUBROUTINE RISTICDRV
!-----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------------------------------------------
                          SUBROUTINE RISTIC                             &
!-----------------------------------------------------------------------
     & (ITIMESTEP,I,J,DTCNVC                                            &
     & ,DPRS,PRSMID,Q,T                                                 &
     & ,USTAR                                                           &
     & ,W,CWM                                                           &
     & ,DQDT,DTDT,PCPCOL,LBOT,LTOP                                      &
     & ,CP,R,ELWV,ELIV,G                                                &
     & ,PRINT_DIAG                                                      &
     & ,IDS,IDE,JDS,JDE,KDS,KDE                                         &
     & ,IMS,IME,JMS,JME,KMS,KME                                         &
     & ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE                     &
                           ,I,J,ITIMESTEP
!
      INTEGER,INTENT(OUT) :: LBOT,LTOP
!
      REAL,INTENT(IN) :: CP,DTCNVC,ELIV,ELWV,G,R
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: DPRS,PRSMID,Q,T
      REAL,INTENT(IN) :: USTAR
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: W,CWM
!
      REAL,INTENT(INOUT) :: PCPCOL
!
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: DQDT,DTDT
!
!-----------------------------------------------------------------------
!***  DEFINE LOCAL VARIABLES
!-----------------------------------------------------------------------
!                                                            
      REAL,DIMENSION(KTS:KTE+1) :: TH,QH,APEL,TP,QP,FLUX,TN,QN,C2
!
      REAL,DIMENSION(KTS:KTE) :: CPCP,DZ,DIFT,DIFQ
!
!***  Begin debugging convection
      LOGICAL :: PRINT_DIAG
!***  End debugging convection
!
!-----------------------------------------------------------------------
!***
!***  LOCAL SCALARS
!***
      REAL :: CAPA,RCP,DTPH,RDTPH,TWODT,RTWODT                          &
     &            ,CAPE,WLCL,FVIJE,FCLOUD,FLUXB,CFAC,RHOL,TTHBT,QBT     &
     &            ,ZLO,TKL,QKL,CWMKL,PKL,APE,TBT,WKL,THICK,RTHICK       &
     &            ,RHO,ZINT,ZMID,DZKL                                   &
     &            ,TT,QQ,PP,QC,QW,TC,TG1,QWAT,TVC,TVE,FLUNI,ZLCL,FRC1   &
     &            ,FPCP,C0,CPDR,ZTOP,FACW,FOUT,FFUP                     &
     &            ,RP,TGS,FO,TGUESS,F1,DTG,RROW,PRECRL,SUMQ,SUMT
!
      INTEGER :: L,LMHK,LMHP,LMHM,LBTK,LTPK,ITCNT,NSTEP,ITER
!
      REAL,PARAMETER :: ROW=1.E3
!-----------------------------------------------------------------------
      PCPCOL=0.
      LBOT=0
      LTOP=KTE
!-----------------------------------------------------------------------
!-----PREPARATORY CALCULATIONS------------------------------------------
!-----------------------------------------------------------------------
      CAPA=R/CP
      RCP=1./CP
      RROW=1./ROW
      DTPH=DTCNVC
      RDTPH=1./DTPH
      TWODT=DTPH
      RTWODT=1./TWODT
!-----------------------------------------------------------------------
!-----START OF CONVECTION-----------------------------------------------
!-----------------------------------------------------------------------
      LMHK=KTE
      LMHP=LMHK+1
      LMHM=LMHK-1
!
      LBTK=LMHK
      LTPK=LMHK
!
      TP=0.
      QP=0.
      TH=0.
      QH=0.
      TN=0.
      QN=0.
      C2=0.
      TTHBT=0.
      QBT=0.
      FLUX=0.
      CPCP=0.
      CAPE=0.
      WLCL=0.
      FVIJE=0.
      FCLOUD=0.
      ZLO=0.
!
      FLUXB=1000.
      C2(LMHP)=0.03*USTAR
      RHOL=1.
!
      DO 90 L=LMHK,1,-1
!
      TKL=T(L)
      QKL=Q(L)
      CWMKL=CWM(L)
      PKL=PRSMID(L)
      APE=(1.E5/PKL)**CAPA
      TBT=TTHBT/APE
      WKL=W(L)
!
      THICK=DPRS(L)/G
      RTHICK=1./THICK
!
      RHO=PKL/(R*TKL*(1.+0.608*QKL))
      CFAC=RHO/RHOL
      RHOL=RHO
      ZINT=ZLO+THICK/RHO
      DZKL=ZINT-ZLO
      ZMID=0.5*(ZLO+ZINT)
      ZLO=ZINT
!
      DZ(L)=DZKL
      TH(L)=(TKL-RCP*ELWV*CWMKL)*APE
      QH(L)=QKL+CWMKL
      APEL(L)=APE
!-----------------------------------------------------------------------
!-----QC----------------------------------------------------------------
!-----------------------------------------------------------------------
      TT=TBT
      QQ=QBT
      PP=PKL
      CALL FSLOPE
      QC=QW
      TC=TG1
      QWAT=QBT-QC
      IF(QWAT.LE.0.)THEN
        QC=QBT
        TC=TBT
        QWAT=0.
      ENDIF
      TVC=TC*(1.+0.608*QC)/(1.+QWAT)
      TVE=TKL*(1.+0.608*QKL)/(1.+CWMKL)
      CAPE=CAPE+(TVC-TVE)/TVE*G*DZKL
      IF(TVC.GT.TVE)FVIJE=1.
      FLUNI=0.5+SIGN(0.5,T(L)-T(L-1))
!-----------------------------------------------------------------------
!-----CLOUD BOTTOM------------------------------------------------------
!-----------------------------------------------------------------------
      IF(FCLOUD.EQ.0.)THEN
        IF(2*L.LT.LMHK)THEN
          GOTO 100
        ELSEIF(QWAT.LE.0..OR.CAPE.LE.0..AND.WLCL.LE.0.)THEN
          FLUX=0.
          FVIJE=0.
          FLUXB=1000.
          ZLCL=ZMID
          LBTK=L
          CAPE=C2(LMHP)*C2(LMHP)+4.64*AMAX1(0.,WKL)**(1./3.)/TVE*G*DZKL
          WLCL=WKL
        ELSE
          IF(CAPE.LE.0.)GOTO 100
          FCLOUD=1.
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
!-----PRECIPITATION-----------------------------------------------------
!-----------------------------------------------------------------------
      IF(TC.GE.268.16)THEN
        FRC1=0.
      ELSEIF(TC.LE.248.16)THEN
        FRC1=1.
      ELSE
        FRC1=(268.16-TC)/(268.16-248.16)
      ENDIF
      IF(FCLOUD*CAPE.LE.0.)THEN
        IF(FCLOUD*0.35.GT.FLUNI*USTAR)THEN
          FLUX(L+1)=0.
          CPCP(L+1)=CPCP(L+1)/FPCP
        ENDIF
        FPCP=FCLOUD
      ELSE
        C0=0.03*(0.1+0.9*FRC1)/SQRT(CAPE)
        FPCP=1.-EXP(-C0*DZKL)
      ENDIF
      CPDR=QWAT*FPCP
      QBT=QBT-CPDR
      TTHBT=TTHBT+RCP*ELWV*CPDR*APE
      CPCP(L)=FLUX(L+1)*CPDR*FRC1
!-----------------------------------------------------------------------
!-----CLOUD TOP---------------------------------------------------------
!-----------------------------------------------------------------------
      IF(FCLOUD.EQ.1.)THEN
        IF(CAPE.LE.0..OR.QWAT.LE.0.)THEN
          FLUX(L)=0.
          LTPK=L
          ZTOP=ZMID
          GOTO 95
        ENDIF
      ELSEIF(CAPE.LE.0.)THEN
        GOTO 100
      ENDIF
!-----------------------------------------------------------------------
!-----FLUX--------------------------------------------------------------
!-----------------------------------------------------------------------
      C2(L)=(C2(LMHP)*CAPE)**(1./3.)
      FLUX(L)=RHO*C2(L)*TWODT
      FLUXB=AMIN1(FLUXB,DZKL/SQRT(CAPE)*RTWODT)
!-----------------------------------------------------------------------
!-----UPDRAFT ENTRAINMENT-----------------------------------------------
!-----------------------------------------------------------------------
      FACW=(FLUX(L)-FLUX(L+1))/FLUX(L)
      FOUT=AMIN1(1.,0.5*(1.-FACW)*(3.*CFAC-1.))
      FFUP=1.-FOUT
      TTHBT=TTHBT*FOUT+TH(L)*FFUP
      QBT=QBT*FOUT+QH(L)*FFUP
      TP(L)=TTHBT
      QP(L)=QBT
!-----------------------------------------------------------------------
   90 CONTINUE
!-----------------------------------------------------------------------
   95 CONTINUE
!-----------------------------------------------------------------------
!-----NO CONVECTION-----------------------------------------------------
!-----------------------------------------------------------------------
      CPCP=CPCP*FVIJE
!-----------------------------------------------------------------------
!-----PRECIPITATION-----------------------------------------------------
!-----------------------------------------------------------------------
      DIFT=0.
      DIFQ=0.
      PRECRL=0.
      NSTEP=NINT(1./FLUXB+0.5)
      FLUXB=1./FLOAT(NSTEP)
      DO ITER=1,NSTEP
      DO L=LTPK,LMHK
      THICK=DPRS(L)/G
      RTHICK=1./THICK
      TN(L)=TH(L)+FLUXB*RTHICK*(RCP*ELWV*CPCP(L)*APEL(L)                &
     &               +FLUX(L+1)*(TP(L+1)-TH(L  ))                       &
     &               -FLUX(L  )*(TP(L  )-TH(L-1)))
      QN(L)=QH(L)+FLUXB*RTHICK*(-CPCP(L)                                &
     &               +FLUX(L+1)*(QP(L+1)-QH(L  ))                       &
     &               -FLUX(L  )*(QP(L  )-QH(L-1)))
      PRECRL=PRECRL+FLUXB*CPCP(L)
      ENDDO
      TH=TN
      QH=QN
      ENDDO
!-----------------------------------------------------------------------
!-------------------THE PRECIPITATION ON SFC----------------------------
!-----------------------------------------------------------------------
      SUMQ=0.
      SUMT=0.
      DO L=LTPK,LMHK
        DIFT(L)=TH(L)/APEL(L)+CWM(L)*RCP*ELWV-T(L)
        DIFQ(L)=QH(L)-CWM(L)-Q(L)
        THICK=DPRS(L)/G
        SUMQ=SUMQ+DIFQ(L)*THICK
        SUMT=SUMT+DIFT(L)*THICK*APEL(L)*CP/ELWV
      ENDDO
      IF(PRINT_DIAG)write(6,*) PRECRL,SUMT,SUMQ,FLUXB
!
!--------------SAVE CLOUD TOP AND BOTTOM FOR RADIATION------------------
!
      LTOP=LTPK
      LBOT=LBTK
      PCPCOL=PRECRL*RROW
      DQDT=DIFQ*RTWODT
      DTDT=DIFT*RTWODT
!-----------------------------------------------------------------------
  100 CONTINUE
      RETURN
      CONTAINS
      SUBROUTINE FSLOPE
      IF(L.EQ.LMHK)THEN
        TG1=TT
        QW=QQ
        GOTO 25
      ENDIF
      RP=TT+RCP*ELWV*QQ
      TGS=TT
      QW=QSAT(PP,TGS,0.,0.)
      FO=TGS+RCP*ELWV*QW-RP
      TG1=AMAX1(TGS/2.,TGS-.5*FO)
      TGUESS=TGS
      ITCNT=0
   10 QW=QSAT(PP,TG1,0.,0.)
      F1=TG1+RCP*ELWV*QW-RP
      IF(ABS(F1).LT..001.OR.ABS(F1-FO).LT.1.E-10.OR.ITCNT.GT.30)GOTO 25
      ITCNT=ITCNT+1
      DTG=F1*(TG1-TGUESS)/(F1-FO)
      TGUESS=TG1
      FO=F1
      TG1=TG1-DTG
      GOTO 10
   25 CONTINUE
      END SUBROUTINE FSLOPE
!-----------------------------------------------------------------------
      END SUBROUTINE RISTIC
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
      SUBROUTINE RISTICINIT(RTHCUTEN,RQVCUTEN                           &
     &                  ,RESTART                                        &
     &                  ,ALLOWED_TO_READ                                &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: RESTART,ALLOWED_TO_READ
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) ::            &
     &                                              RTHCUTEN            &
     &                                             ,RQVCUTEN
!
      INTEGER :: I,J,K,ITF,JTF,KTF
!-----------------------------------------------------------------------

      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)
! 
      IF(.NOT.RESTART)THEN
        DO J=JTS,JTF
        DO K=KTS,KTF
        DO I=ITS,ITF
          RTHCUTEN(I,K,J)=0.
          RQVCUTEN(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
      END SUBROUTINE RISTICINIT
!-----------------------------------------------------------------------
!
      END MODULE MODULE_CU_RISTIC
!
!-----------------------------------------------------------------------
