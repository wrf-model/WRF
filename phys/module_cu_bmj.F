!-----------------------------------------------------------------------
!
!WRF:MODEL_LAYER:PHYSICS
!
!-----------------------------------------------------------------------
!
      MODULE MODULE_CU_BMJ
!
!-----------------------------------------------------------------------
      USE MODULE_MODEL_CONSTANTS
!-----------------------------------------------------------------------
!
      REAL,PARAMETER ::                                                 &
     &                  DSPC=-3000.                                     &
     &                 ,DTTOP=0.,EFIFC=5.0,EFIMN=0.20,EFMNT=0.70        & 
     &                 ,ELIWV=2.683E6,ENPLO=20000.,ENPUP=15000.         &
     &                 ,EPSDN=1.05,EPSDT=0.                             &
     &                 ,EPSNTP=.0001,EPSNTT=.0001,EPSPR=1.E-7           &
     &                 ,EPSUP=1.00                                      &
     &                 ,FR=1.00,FSL=0.85,FSS=0.85,GAM=0.5,PEPS=1./2.5   &
     &                 ,FUP=0.,FCC=5.00,CRMN=0.14,CRMX=85.0             &
     &                 ,PBM=13000.,PFRZ=15000.,PNO=1000.                &
     &                 ,PONE=2500.,PQM=20000.                           &
     &                 ,PSH=20000.,PSHU=45000.                          &
     &                 ,RENDP=1./(ENPLO-ENPUP)                          &
     &                 ,RHLSC=0.00,RHHSC=1.10                           &
     &                 ,ROW=1.E3                                        &
     &                 ,STABDF=0.90,STABDS=0.90                         &
     &                 ,STABS=1.0,STRESH=1.10                           &
     &                 ,DTSHAL=-1.0,TREL=2400.
!
      REAL,PARAMETER :: DTtrigr=-0.0                                    &
                       ,DTPtrigr=DTtrigr*PONE      !<-- Average parcel virtual temperature deficit over depth PONE.
                                                   !<-- NOTE: CAPEtrigr is scaled by the cloud base temperature (see below)
!
      REAL,PARAMETER :: DSPBFL=-3875.*FR                                &
     &                 ,DSP0FL=-5875.*FR                                &
     &                 ,DSPTFL=-1875.*FR                                &
     &                 ,DSPBFS=-3875.                                   &
     &                 ,DSP0FS=-5875.                                   &
     &                 ,DSPTFS=-1875.
!
      REAL,PARAMETER :: PL=2500.,PLQ=70000.,PH=105000.                  &
     &                 ,THL=210.,THH=365.,THHQ=325.
!
      INTEGER,PARAMETER :: ITB=76,JTB=134,ITBQ=152,JTBQ=440
!
      INTEGER,PARAMETER :: ITREFI_MAX=3
!
!***  ARRAYS FOR LOOKUP TABLES
!
      REAL,DIMENSION(ITB),PRIVATE,SAVE :: STHE,THE0
      REAL,DIMENSION(JTB),PRIVATE,SAVE :: QS0,SQS
      REAL,DIMENSION(ITBQ),PRIVATE,SAVE :: STHEQ,THE0Q
      REAL,DIMENSION(ITB,JTB),PRIVATE,SAVE :: PTBL
      REAL,DIMENSION(JTB,ITB),PRIVATE,SAVE :: TTBL
      REAL,DIMENSION(JTBQ,ITBQ),PRIVATE,SAVE :: TTBLQ

!***  SHARE COPIES FOR MODULE_BL_MYJPBL
!
      REAL,DIMENSION(JTB) :: QS0_EXP,SQS_EXP
      REAL,DIMENSION(ITB,JTB) :: PTBL_EXP
!
      REAL,PARAMETER :: RDP=(ITB-1.)/(PH-PL),RDPQ=(ITBQ-1.)/(PH-PLQ)  &
     &                 ,RDQ=ITB-1,RDTH=(JTB-1.)/(THH-THL)             &
     &                 ,RDTHE=JTB-1.,RDTHEQ=JTBQ-1.                   &
     &                 ,RSFCP=1./101300.
!
      REAL,PARAMETER :: AVGEFI=(EFIMN+1.)*0.5
!
!-----------------------------------------------------------------------
!
CONTAINS
!
!-----------------------------------------------------------------------
      SUBROUTINE BMJDRV(                                                &
     &                  IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE                         &
     &                 ,DT,ITIMESTEP,STEPCU,CCLDFRA,CONVCLD             &
     &                 ,RAINCV,PRATEC,CUTOP,CUBOT,KPBL                  &
     &                 ,TH,T,QV,QCCONV,QICONV,BMJ_RAD_FEEDBACK          &
     &                 ,PINT,PMID,PI,RHO,DZ8W                           &
     &                 ,CP,R,ELWV,ELIV,G,TFRZ,D608                      &
     &                 ,CLDEFI,LOWLYR,XLAND,CU_ACT_FLAG                 &
                      ! optional
     &                 ,RTHCUTEN,RQVCUTEN                               &
     &                                                                  )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     & 
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: ITIMESTEP,STEPCU
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: KPBL,LOWLYR
!
      REAL,INTENT(IN) :: CP,DT,ELIV,ELWV,G,R,TFRZ,D608
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: XLAND
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ8W        &
     &                                                     ,PI,PINT     &
     &                                                     ,PMID,QV     &
     &                                                     ,RHO,T,TH
! 
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CCLDFRA  &
                                                              ,QCCONV   &
                                                              ,QICONV
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                           &
     &    ,OPTIONAL                                                     &
     &    ,INTENT(INOUT) ::                        RQVCUTEN,RTHCUTEN
! 
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CLDEFI,RAINCV,   &
           PRATEC,CONVCLD
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CUBOT,CUTOP
!
      LOGICAL,INTENT(IN) :: bmj_rad_feedback
      LOGICAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CU_ACT_FLAG

!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: LBOT,LPBL,LTOP
!
      REAL :: DTCNVC,LANDMASK,PCPCOL,PSFC,PTOP
      REAL :: PAVG,PWCOL,DQCOL,DQCOLMIN
      REAL :: CUMX,QCIS,RRP,PRRT,MCOL,MPVPR,FACTL
      INTEGER :: BBOT,TTOP
! 
      REAL,DIMENSION(KTS:KTE) :: DPCOL,DQDT,DTDT,PCOL,QCOL,TCOL
      REAL,DIMENSION(KTS:KTE) :: PVPR,JPR
!
      INTEGER :: I,J,K,KFLIP,LMH

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
!***  PREPARE TO CALL BMJ CONVECTION SCHEME
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
            JPR(K)=0.
            PVPR(K)=0.
            QCCONV(I,K,J)=0.
            QICONV(I,K,J)=0.
            CCLDFRA(I,K,J)=0.
          ENDDO
!
          DQCOL=0.
          PWCOL=0.
          PCPCOL=0.
          DQCOLMIN=0.
          RAINCV(I,J)=0.
          PRATEC(I,J)=0.
          CONVCLD(I,J)=0.
          PSFC=PINT(I,LOWLYR(I,J),J)
          PTOP=PINT(I,KTE+1,J)      ! KTE+1=KME
!
!***  CONVERT TO BMJ LAND MASK (1.0 FOR SEA; 0.0 FOR LAND)
!
          LANDMASK=XLAND(I,J)-1.
!
!***  FILL 1-D VERTICAL ARRAYS 
!***  AND FLIP DIRECTION SINCE BMJ SCHEME 
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
!           DPCOL(K)=PINT(I,KFLIP,J)-PINT(I,KFLIP+1,J)
            DPCOL(K)=RHO(I,KFLIP,J)*G*DZ8W(I,KFLIP,J)
          ENDDO
!
!***  LOWEST LAYER ABOVE GROUND MUST ALSO BE FLIPPED
!
          LMH=KTE+1-LOWLYR(I,J)
          LPBL=KTE+1-KPBL(I,J)
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
          CALL BMJ(ITIMESTEP,I,J,DTCNVC,LMH,LANDMASK,CLDEFI(I,J)        &
     &            ,DPCOL,PCOL,QCOL,TCOL,PSFC,PTOP                       &
     &            ,DQDT,DTDT,PCPCOL,LBOT,LTOP,LPBL                      &
     &            ,PWCOL,DQCOL,DQCOLMIN                                 &
     &            ,CP,R,ELWV,ELIV,G,TFRZ,D608                           &   
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
!***  ALL UNITS IN BMJ SCHEME ARE MKS, THUS CONVERT PRECIP FROM METERS
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
       IF ( bmj_rad_feedback ) THEN
!
           IF (DQCOL.GT.DQCOLMIN) THEN
!
!***  CONVECTIVE CLOUD FRACTION: BASED ON SLINGO (1987) WITH A POISSON 
!***  VERTICAL PROFILE. PLEASE NOTE THAT THE BMJ PRECIPITATION RATE
!***  (PRATEC) HAS TO BE CONVERTED FROM MMS-1 TO MMDAY-1.
!
             TTOP=0
             BBOT=0
             PAVG=0.
             CUMX=0.
             MPVPR=0.
             FACTL=0.
!
             PRRT=(PRATEC(I,J)*86400.0)/CRMN
             RRP=0.8/(LOG(CRMX/CRMN))
             IF (PRRT<CRMX/CRMN) THEN
               CUMX=RRP*LOG(PRRT)
             ELSE
               CUMX=0.8
             ENDIF
!
!***  COMPUTE THE CONVECTIVE CLOUD FRACTION (CCLDFRA) AT EACH MODEL LEVEL.
!***  FOR N>=17 USE THE STERLING APPROXIMATION AS FOR N=17 IT GIVES A RELATIVE
!***  ERROR OF ~9.4x10E-8.
!
             TTOP=NINT(CUTOP(I,J))
             BBOT=NINT(CUBOT(I,J))
             PAVG=1./(PEPS*3.)**2
             DO K=KTS,KTE
              IF (K.GE.BBOT.AND.K.LE.TTOP) THEN
                JPR(K)=(1./PEPS)*((PMID(I,K,J)-PMID(I,TTOP,J))/(PMID(I,BBOT,J)-PMID(I,TTOP,J)))
              ELSE
                JPR(K)=0.0
              ENDIF
             ENDDO
!
             DO K=KTS,KTE
               PVPR(K)=0.
             ENDDO
             IF (JPR(BBOT).LT.17) THEN
              DO K=BBOT,TTOP
                PVPR(K)=(PAVG)**(JPR(K))/GAMMA(JPR(K)+1.)
              ENDDO 
             ELSE
              DO K=BBOT,TTOP
                FACTL=JPR(K)*LOG(JPR(K))-JPR(K)+1./2.*LOG(2.*JPR(K)*ACOS(-1.))+ &
                     LOG(1.+1./(12.*JPR(K))+1./(288.*JPR(K)**2))
                PVPR(K)=EXP((JPR(K))*LOG(1.*PAVG)-FACTL)
              ENDDO
             ENDIF
             MPVPR=MAXVAL(PVPR)
             DO K=BBOT,TTOP
               PVPR(K)=PVPR(K)/MPVPR
             ENDDO
             DO K=KTS,KTE
               CCLDFRA(I,K,J)=CUMX*PVPR(K)
             ENDDO
!
!***  COMPUTE THE CONVECTIVE CLOUD CONDENSATES (QCCONV,QICONV). PLEASE NOTE THAT
!***  THE EQUATION FOR QCIS IS VALID FOR PRRTs IN THE RANGE 10**(-7) TO 10**3.
!
             QCIS=0.
             MCOL=0.
             DO K=CUBOT(I,J),CUTOP(I,J)
               KFLIP=KTE+1-K
               MCOL=RHO(I,K,J)*DZ8W(I,K,J)*QCOL(KFLIP)*CCLDFRA(I,K,J)+MCOL
             ENDDO
             CONVCLD(I,J)=PWCOL**GAM*(DQCOL-DQCOLMIN)**(1.-GAM)
             QCIS=CONVCLD(I,J)/MCOL
             DO K=KTS,KTE
              KFLIP=KTE+1-K
              IF (TCOL(KFLIP)>=TFRZ) THEN
                QICONV(I,K,J)=0.
                QCCONV(I,K,J)=(QCIS*QCOL(KFLIP)*CCLDFRA(I,K,J))/(1.-QCOL(KFLIP))
              ELSE
                QICONV(I,K,J)=(QCIS*QCOL(KFLIP)*CCLDFRA(I,K,J))/(1.-QCOL(KFLIP))
                QCCONV(I,K,J)=0.
              ENDIF
             ENDDO
!
          ENDIF
!
       ENDIF
!
!-----------------------------------------------------------------------
!***  Begin debugging convection
          IF(PRINT_DIAG)THEN
            DELT=0.
            DELQ=0.
            PLYR=0.
            IF(LBOT>0.AND.LTOP<LBOT)THEN
              DO K=LTOP,LBOT
                PLYR=PLYR+DPCOL(K)
                DELQ=DELQ+DPCOL(K)*DTCNVC*ABS(DQDT(K))
                DELT=DELT+DPCOL(K)*DTCNVC*ABS(DTDT(K))
              ENDDO
              DELQ=DELQ/PLYR
              DELT=DELT/PLYR
            ENDIF
!
            WRITE(6,"(2a,2i4,3e12.4,f7.2,4i3)") &
                 '{cu3 i,j,PCPCOL,DTavg,DQavg,PLYR,'  &
                 ,'LBOT,LTOP,CUBOT,CUTOP = '  &
                 ,i,j, PCPCOL,DELT,1000.*DELQ,.01*PLYR  &
                 ,LBOT,LTOP,NINT(CUBOT(I,J)),NINT(CUTOP(I,J))
!
            IF(PLYR> 0.)THEN
              DO K=LBOT,LTOP,-1
                KFLIP=KTE+1-K
                WRITE(6,"(a,i3,2e12.4,f7.2)") '{cu3a KFLIP,DT,DQ,DP = ' &
                     ,KFLIP,DTCNVC*DTDT(K),1000.*DTCNVC*DQDT(K),.01*DPCOL(K)
              ENDDO
            ENDIF
          ENDIF
!***  End debugging convection
!-----------------------------------------------------------------------
!
        ENDDO
        ENDDO
!
      END SUBROUTINE BMJDRV
!-----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------------------------------------------
                          SUBROUTINE BMJ                                &
!-----------------------------------------------------------------------
     & (ITIMESTEP,I,J,DTCNVC,LMH,SM,CLDEFI                              &
     & ,DPRS,PRSMID,Q,T,PSFC,PT                                         &
     & ,DQDT,DTDT,PCPCOL,LBOT,LTOP,LPBL                                 &
     & ,PWCOL,DQCOL,DQCOLMIN                                            &
     & ,CP,R,ELWV,ELIV,G,TFRZ,D608                                      &
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
      INTEGER,INTENT(IN) :: LMH,LPBL
!
      INTEGER,INTENT(OUT) :: LBOT,LTOP
!
      REAL,INTENT(IN) :: CP,D608,DTCNVC,ELIV,ELWV,G,PSFC,PT,R,SM,TFRZ
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: DPRS,PRSMID,Q,T
!
      REAL,INTENT(INOUT) :: CLDEFI,PCPCOL,PWCOL,DQCOL,DQCOLMIN
!
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: DQDT,DTDT
!
!-----------------------------------------------------------------------
!***  DEFINE LOCAL VARIABLES
!-----------------------------------------------------------------------
!                                                            
      REAL,DIMENSION(KTS:KTE) :: APEK,APESK,EL,FPK                      &
                                ,PK,PSK,QK,QREFK,QSATK                  &
                                ,THERK,THEVRF,THSK                      &
                                ,THVMOD,THVREF,TK,TREFK
      REAL,DIMENSION(KTS:KTE) :: APE,DIFQ,DIFT,THEE,THES,TREF
!
      REAL,DIMENSION(KTS:KTE) :: CPE,CPEcnv,DTV,DTVcnv,THEScnv    !<-- CPE for shallow convection buoyancy check (24 Aug 2006)
!
      LOGICAL :: DEEP,SHALLOW
!
!***  Begin debugging convection
      LOGICAL :: PRINT_DIAG
!***  End debugging convection
!
!-----------------------------------------------------------------------
!***
!***  LOCAL SCALARS
!***
!-----------------------------------------------------------------------
      REAL :: APEKL,APEKXX,APEKXY,APES,APESTS                           &
     &            ,AVRGT,AVRGTL,BQ,BQK,BQS00K,BQS10K                    &
     &            ,CAPA,CUP,DEN,DENTPY,DEPMIN,DEPTH                     &
     &            ,DEPWL,DHDT,DIFQL,DIFTL,DP,DPKL,DPLO,DPMIX,DPOT       &
     &            ,DPUP,DQREF,DRHDP,DRHEAT,DSP                          &
     &            ,DSP0,DSP0K,DSPB,DSPBK,DSPT,DSPTK                     &
     &            ,DSQ,DST,DSTQ,DTHEM,DTDP,EFI                          &
     &            ,FEFI,FFUP,FPRS,FPTK,HCORR                            &
     &            ,OTSUM,P,P00K,P01K,P10K,P11K                          &
     &            ,PART1,PART2,PART3,PBOT,PBOTFC,PBTK                   &
     &            ,PK0,PKB,PKL,PKT,PKXXXX,PKXXXY                        &
     &            ,PLMH,PELEVFC,PBTmx,plo,POTSUM,PP1,PPK,PRECK          &
     &            ,PRWD,PDQD,PRESK,PSP,PSUM,PTHRS,PTOP,PTPK,PUP         &
     &            ,QBT,QKL,QNEW,QOTSUM,QQ1,QQK,QRFKL                    &
     &            ,QRFTP,QSP,QSUM,QUP,RDP0T                             &
     &            ,RDPSUM,RDTCNVC,RHH,RHL,RHMAX,ROTSUM,RTBAR,RHAVG      &
     &            ,SM1,SMIX,SQ,SQK,SQS00K,SQS10K,STABDL,SUMDE,SUMDP     &
     &            ,SUMDT,TAUK,TAUKSC,TCORR,THBT,THERKX,THERKY           &
     &            ,THESP,THSKL,THTPK,THVMKL,TKL,TNEW                    &
     &            ,TQ,TQK,TREFKX,TRFKL,trmlo,trmup,TSKL,tsp,TTH         &
     &            ,TTHK,TUP                                             &
     &            ,CAPEcnv,PSPcnv,THBTcnv,CAPEtrigr,CAPE                &
     &            ,TLEV2,QSAT1,QSAT2,RHSHmax
!
      INTEGER :: IQ,IQTB,IT,ITER,ITREFI,ITTB,ITTBK,KB,KNUMH,KNUML       &
     &          ,L,L0,L0M1,LB,LBM1,LCOR,LPT1                            &
     &          ,LQM,LSHU,LTP1,LTP2,LTSH, LBOTcnv,LTOPcnv,LMID
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: DSPBSL=DSPBFL*FSL,DSP0SL=DSP0FL*FSL             &
     &                 ,DSPTSL=DSPTFL*FSL                               &
     &                 ,DSPBSS=DSPBFS*FSS,DSP0SS=DSP0FS*FSS             &
     &                 ,DSPTSS=DSPTFS*FSS
!
      REAL,PARAMETER :: ELEVFC=0.6,STEFI=1.
!
      REAL,PARAMETER :: SLOPBL=(DSPBFL-DSPBSL)/(1.-EFIMN)               &
     &                 ,SLOP0L=(DSP0FL-DSP0SL)/(1.-EFIMN)               &
     &                 ,SLOPTL=(DSPTFL-DSPTSL)/(1.-EFIMN)               &
     &                 ,SLOPBS=(DSPBFS-DSPBSS)/(1.-EFIMN)               &
     &                 ,SLOP0S=(DSP0FS-DSP0SS)/(1.-EFIMN)               &
     &                 ,SLOPTS=(DSPTFS-DSPTSS)/(1.-EFIMN)               &
     &                 ,SLOPST=(STABDF-STABDS)/(1.-EFIMN)               &
     &                 ,SLOPE=(1.-EFMNT)/(1.-EFIMN)
!
      REAL :: A23M4L,CPRLG,ELOCP,RCP,QWAT
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      CAPA=R/CP
      CPRLG=CP/(ROW*G*ELWV)
      ELOCP=ELIWV/CP
      RCP=1./CP
      A23M4L=A2*(A3-A4)*ELWV
      RDTCNVC=1./DTCNVC
      DEPMIN=PSH*PSFC*RSFCP
!
      DEEP=.FALSE.
      SHALLOW=.FALSE.
!
      DSP0=0.
      DSPB=0.
      DSPT=0.
!-----------------------------------------------------------------------
      TAUK=DTCNVC/TREL
      TAUKSC=DTCNVC/(1.0*TREL) 
!-----------------------------------------------------------------------
!-----------------------------PREPARATIONS------------------------------
!-----------------------------------------------------------------------
      LBOT=LMH
      DQCOL=0.
      PWCOL=0.
      PCPCOL=0.
      DQCOLMIN=0.
      TREF(KTS)=T(KTS)
!
      DO L=KTS,LMH
        APESTS=PRSMID(L)
        APE(L)=(1.E5/APESTS)**CAPA
        CPEcnv(L)=0.
        DTVcnv(L)=0.
        THEScnv(L)=0.
      ENDDO
!
!-----------------------------------------------------------------------
!----------------SEARCH FOR MAXIMUM BUOYANCY LEVEL----------------------
!-----------------------------------------------------------------------
!
      PLMH=PRSMID(LMH)
      PELEVFC=PLMH*ELEVFC
      PBTmx=PRSMID(LMH)-PONE
      CAPEcnv=0.
      PSPcnv=0.
      THBTcnv=0.
      LBOTcnv=LBOT
      LTOPcnv=LBOT
!
!-----------------------------------------------------------------------
!----------------TRIAL MAXIMUM BUOYANCY LEVEL VARIABLES-----------------
!-----------------------------------------------------------------------
!
      max_buoy_loop: DO KB=LMH,1,-1
!
!-----------------------------------------------------------------------
!
        PKL=PRSMID(KB)
!       IF (PKL<PELEVFC .OR. T(KB)<=TFRZ) EXIT
        IF (PKL<PELEVFC) EXIT
        LBOT=LMH
        LTOP=LMH
!
!-----------------------------------------------------------------------
!***  SEARCH OVER A SCALED DEPTH IN FINDING THE PARCEL
!***  WITH THE MAX THETA-E 
!-----------------------------------------------------------------------
!
        QBT=Q(KB)
        THBT=T(KB)*APE(KB)
        TTH=(THBT-THL)*RDTH
        QQ1=TTH-AINT(TTH)
        ITTB=INT(TTH)+1
!----------------KEEPING INDICES WITHIN THE TABLE-----------------------
        IF(ITTB<1)THEN
          ITTB=1
          QQ1=0.
        ELSE IF(ITTB>=JTB)THEN
          ITTB=JTB-1
          QQ1=0.
        ENDIF
!--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
        ITTBK=ITTB
        BQS00K=QS0(ITTBK)
        SQS00K=SQS(ITTBK)
        BQS10K=QS0(ITTBK+1)
        SQS10K=SQS(ITTBK+1)
!--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
        BQ=(BQS10K-BQS00K)*QQ1+BQS00K
        SQ=(SQS10K-SQS00K)*QQ1+SQS00K
        TQ=(QBT-BQ)/SQ*RDQ
        PP1=TQ-AINT(TQ)
        IQTB=INT(TQ)+1
!----------------KEEPING INDICES WITHIN THE TABLE-----------------------
        IF(IQTB<1)THEN
          IQTB=1
          PP1=0.
        ELSE IF(IQTB>=ITB)THEN
          IQTB=ITB-1
          PP1=0.
        ENDIF
!--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
        IQ=IQTB
        IT=ITTB
        P00K=PTBL(IQ  ,IT  )
        P10K=PTBL(IQ+1,IT  )
        P01K=PTBL(IQ  ,IT+1)
        P11K=PTBL(IQ+1,IT+1)
!
!--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
!
        PSP=P00K+(P10K-P00K)*PP1+(P01K-P00K)*QQ1                        &
     &          +(P00K-P10K-P01K+P11K)*PP1*QQ1
        APES=(1.E5/PSP)**CAPA
        THESP=THBT*EXP(ELOCP*QBT*APES/THBT)
!
!-----------------------------------------------------------------------
!-----------CHOOSE CLOUD BASE AS MODEL LEVEL JUST BELOW PSP-------------
!-----------------------------------------------------------------------
!
        DO L=KTS,LMH-1
          P=PRSMID(L)
          IF(P<PSP.AND.P>=PQM)LBOT=L+1
        ENDDO
!***
!*** WARNING: LBOT MUST NOT BE > LMH-1 IN SHALLOW CONVECTION
!*** MAKE SURE CLOUD BASE IS AT LEAST PONE ABOVE THE SURFACE
!***
        PBOT=PRSMID(LBOT)
        IF(PBOT>=PBTmx.OR.LBOT>=LMH)THEN
          DO L=KTS,LMH-1
            P=PRSMID(L)
            IF(P<PBTmx)LBOT=L
          ENDDO
          PBOT=PRSMID(LBOT)
        ENDIF 
!
!-----------------------------------------------------------------------
!----------------CLOUD TOP COMPUTATION----------------------------------
!-----------------------------------------------------------------------
!
        LTOP=LBOT
        PTOP=PBOT
        DO L=LMH,KTS,-1
          THES(L)=THESP
        ENDDO
!
!-----------------------------------------------------------------------
!### BEGIN: ###########  WARNING  ###########  WARNING  ###########
!-----------------------------------------------------------------------
!
!### IMPORTANT: THIS "DO KB=LMH,1,-1" loop must be broken up into two
!    separate loops in order for entrainment as programmed below to work
!    properly.  
!
!---------------  ENTRAINMENT DURING PARCEL ASCENT  --------------------
!
!        DO L=LMH,KB,-1
!          THES(L)=THESP
!        ENDDO
!
!        DO L=KTS,LMH
!          THEE(L)=THES(L)
!        ENDDO
!!
!        FEFI=(CLDEFI-EFIMN)*SLOPE+EFMNT
!        FFUP=FUP/(FEFI*FEFI)
!!
!        IF(PBOT>ENPLO)THEN
!          FPRS=1.
!        ELSEIF(PBOT>ENPUP)THEN
!          FPRS=(PBOT-ENPUP)*RENDP
!        ELSE
!          FPRS=0.
!        ENDIF
!!
!        FFUP=FFUP*FPRS*FPRS*0.5
!        DPUP=DPRS(KB)
!!
!        DO L=KB-1,KTS,-1
!          DPLO=DPUP
!          DPUP=DPRS(L)
!!
!          THES(L)=((-FFUP*DPLO+1.)*THES(L+1)                           &
!     &            +(THEE(L)*DPUP+THEE(L+1)*DPLO)*FFUP)                 &
!     &           /(FFUP*DPUP+1.)
!      ENDDO
!
!-----------------------------------------------------------------------
!### END: ###########  WARNING  ###########  WARNING  ###########
!-----------------------------------------------------------------------
!!
!-----------------------------------------------------------------------
!!***  COMPUTE PARCEL TEMPERATURE ALONG THE ASCENT TRAJECTORY
!!***  SCALING PRESSURE & TT TABLE INDEX.
!-----------------------------------------------------------------------
!!
!!
!       DO L=LMH,KTS,-1
!!
!         PRESK=PRSMID(L)
!!
!         IF(PRESK<PLQ)THEN
!           CALL TTBLEX(ITB,JTB,PL,PRESK,RDP,RDTHE                      &
!     &                ,STHE,THE0,THES(L),TTBL,TREF(L))
!         ELSE
!           CALL TTBLEX(ITBQ,JTBQ,PLQ,PRESK,RDPQ,RDTHEQ                 &
!     &                ,STHEQ,THE0Q,THES(L),TTBLQ,TREF(L))
!         ENDIF
!!
!       ENDDO
!!
!!-----------------------------------------------------------------------
!!----------------BUOYANCY CHECK-----------------------------------------
!!-----------------------------------------------------------------------
!!
!       DO L=LMH,KTS,-1
!         IF(TREF(L)>T(L)-DTTOP)LTOP=L
!       ENDDO
!!
!!***  CLOUD TOP PRESSURE
!!
!       PTOP=PRSMID(LTOP)
!
!------------------FIRST ENTROPY CHECK----------------------------------
!
        DO L=KTS,LMH
          CPE(L)=0.
          DTV(L)=0.
        ENDDO
!-----------------------------------------------------------------------
!       lbot_ltop: IF(LBOT>LTOP)THEN
!-----------------------------------------------------------------------
!-- Begin: Buoyancy check including deep convection (24 Aug 2006) 
!-----------------------------------------------------------------------
          DENTPY=0.
          L=KB
          PLO=PRSMID(L)
          TRMLO=0.
          CAPEtrigr=DTPtrigr/T(LBOT)
!
!--- Below cloud base
!
          IF(KB>LBOT) THEN
            DO L=KB-1,LBOT+1,-1
              PUP=PRSMID(L)
              TUP=THBT/APE(L)
              DP=PLO-PUP
              TRMUP=(TUP*(QBT*0.608+1.)                                 &
     &            -T(L)*(Q(L)*0.608+1.))*0.5                            &
     &             /(T(L)*(Q(L)*0.608+1.))
              DTV(L)=TRMLO+TRMUP
              DENTPY=DTV(L)*DP+DENTPY
              CPE(L)=DENTPY
              IF (DENTPY < CAPEtrigr) GO TO 170
              PLO=PUP
              TRMLO=TRMUP
            ENDDO
          ELSE
            L=LBOT+1
            PLO=PRSMID(L)
            TUP=THBT/APE(L)
            TRMLO=(TUP*(QBT*0.608+1.)                                   &
     &            -T(L)*(Q(L)*0.608+1.))*0.5                            &
     &             /(T(L)*(Q(L)*0.608+1.))
          ENDIF  ! IF(KB>LBOT) THEN
!
!--- At cloud base
!
          L=LBOT
          PUP=PSP
          TUP=THBT/APES
          TSP=(T(L+1)-T(L))/(PLO-PBOT)                                  &
     &       *(PUP-PBOT)+T(L)
          QSP=(Q(L+1)-Q(L))/(PLO-PBOT)                                  &
     &       *(PUP-PBOT)+Q(L)
          DP=PLO-PUP
          TRMUP=(TUP*(QBT*0.608+1.)                                     &
     &          -TSP*(QSP*0.608+1.))*0.5                                &
     &         /(TSP*(QSP*0.608+1.))
          DTV(L)=TRMLO+TRMUP
          DENTPY=DTV(L)*DP+DENTPY
          CPE(L)=DENTPY
          DTV(L)=DTV(L)*DP
          PLO=PUP
          TRMLO=TRMUP
          PUP=PRSMID(L)
!
!--- Calculate updraft temperature along moist adiabat (TUP)
!
          IF(PUP<PLQ)THEN
            CALL TTBLEX(ITB,JTB,PL,PUP,RDP,RDTHE                        &
     &                 ,STHE,THE0,THES(L),TTBL,TUP)
          ELSE
            CALL TTBLEX(ITBQ,JTBQ,PLQ,PUP,RDPQ,RDTHEQ                   &
     &                 ,STHEQ,THE0Q,THES(L),TTBLQ,TUP)
          ENDIF
!
          QUP=PQ0/PUP*EXP(A2*(TUP-A3)/(TUP-A4))
          QWAT=QBT-QUP  !-- Water loading effects, reversible adiabat
          DP=PLO-PUP
          TRMUP=(TUP*(QUP*0.608+1.-QWAT)                                &
     &          -T(L)*(Q(L)*0.608+1.))*0.5                              &
     &         /(T(L)*(Q(L)*0.608+1.))
          DENTPY=(TRMLO+TRMUP)*DP+DENTPY
          CPE(L)=DENTPY
          DTV(L)=(DTV(L)+(TRMLO+TRMUP)*DP)/(PRSMID(LBOT+1)-PRSMID(LBOT))
!
          IF (DENTPY < CAPEtrigr) GO TO 170
!
          PLO=PUP
          TRMLO=TRMUP
!
!-----------------------------------------------------------------------
!--- In cloud above cloud base
!-----------------------------------------------------------------------
!
          DO L=LBOT-1,KTS,-1
            PUP=PRSMID(L)
!
!--- Calculate updraft temperature along moist adiabat (TUP)
!
            IF(PUP<PLQ)THEN
              CALL TTBLEX(ITB,JTB,PL,PUP,RDP,RDTHE                      &
       &                 ,STHE,THE0,THES(L),TTBL,TUP)
            ELSE
              CALL TTBLEX(ITBQ,JTBQ,PLQ,PUP,RDPQ,RDTHEQ                 &
       &                 ,STHEQ,THE0Q,THES(L),TTBLQ,TUP)
            ENDIF
!
            QUP=PQ0/PUP*EXP(A2*(TUP-A3)/(TUP-A4))
            QWAT=QBT-QUP  !-- Water loading effects, reversible adiabat
            DP=PLO-PUP
            TRMUP=(TUP*(QUP*0.608+1.-QWAT)                              &
     &            -T(L)*(Q(L)*0.608+1.))*0.5                            &
     &           /(T(L)*(Q(L)*0.608+1.))
            DTV(L)=TRMLO+TRMUP
            DENTPY=DTV(L)*DP+DENTPY
            CPE(L)=DENTPY
!
            IF (DENTPY < CAPEtrigr) GO TO 170
!
            PLO=PUP
            TRMLO=TRMUP
          ENDDO
!
!-----------------------------------------------------------------------
!
170       LTP1=KB
          CAPE=0.
!
!-----------------------------------------------------------------------
!--- Cloud top level (LTOP) is located where CAPE is a maximum
!--- Exit cloud-top search if CAPE < CAPEtrigr
!-----------------------------------------------------------------------
!
          DO L=KB,KTS,-1
            IF (CPE(L) < CAPEtrigr) THEN
              EXIT
            ELSE IF (CPE(L) > CAPE) THEN
              LTP1=L
              CAPE=CPE(L)
            ENDIF
          ENDDO      !-- End DO L=KB,KTS,-1
!
          LTOP=MIN(LTP1,LBOT)
! 
!-----------------------------------------------------------------------
!--------------- CHECK FOR MAXIMUM INSTABILITY  ------------------------
!-----------------------------------------------------------------------
          IF(CAPE > CAPEcnv) THEN
            CAPEcnv=CAPE
            PSPcnv=PSP
            THBTcnv=THBT
            LBOTcnv=LBOT
            LTOPcnv=LTOP
            DO L=LMH,KTS,-1
              CPEcnv(L)=CPE(L)
              DTVcnv(L)=DTV(L)
              THEScnv(L)=THES(L)
            ENDDO
          ENDIF    ! End IF(CAPE > CAPEcnv) THEN
!
!       ENDIF lbot_ltop
!
!-----------------------------------------------------------------------
!
      ENDDO max_buoy_loop
!
!-----------------------------------------------------------------------
!------------------------  MAXIMUM INSTABILITY  ------------------------
!-----------------------------------------------------------------------
!
      IF(CAPEcnv > 0.) THEN
        PSP=PSPcnv
        THBT=THBTcnv
        LBOT=LBOTcnv
        LTOP=LTOPcnv
        PBOT=PRSMID(LBOT)
        PTOP=PRSMID(LTOP)
!
        DO L=LMH,KTS,-1
          CPE(L)=CPEcnv(L)
          DTV(L)=DTVcnv(L)
          THES(L)=THEScnv(L)
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!-----  Quick exit if cloud is too thin or no CAPE is present  ---------
!-----------------------------------------------------------------------
!
      IF(PTOP>PBOT-PNO.OR.LTOP>LBOT-2.OR.CAPEcnv<=0.)THEN
        LBOT=0
        LTOP=KTE
        PBOT=PRSMID(LMH)
        PTOP=PBOT
        CLDEFI=AVGEFI*SM+STEFI*(1.-SM)
!       CLDEFI=(EFIMN-0.2)*SM+(1.+0.2)*(1.-SM)
        GO TO 800
      ENDIF
!
!***  DEPTH OF CLOUD REQUIRED TO MAKE THE POINT A DEEP CONVECTION POINT
!***  IS A SCALED VALUE OF PSFC.
!
      DEPTH=PBOT-PTOP
!
      IF(DEPTH>=DEPMIN) THEN
        DEEP=.TRUE.
      ELSE
        SHALLOW=.TRUE.
!       CLDEFI=(EFIMN-0.1)*SM+(1.+0.1)*(1.-SM)
        GO TO 600
      ENDIF
!
!-----------------------------------------------------------------------
!DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
!DCDCDCDCDCDCDCDCDCDCDC    DEEP CONVECTION   DCDCDCDCDCDCDCDCDCDCDCDCDCD
!DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
!-----------------------------------------------------------------------
!
  300 CONTINUE
!
      LB =LBOT
      EFI=CLDEFI
!-----------------------------------------------------------------------
!--------------INITIALIZE VARIABLES IN THE CONVECTIVE COLUMN------------
!-----------------------------------------------------------------------
!***
!***  ONE SHOULD NOTE THAT THE VALUES ASSIGNED TO THE ARRAY TREFK
!***  IN THE FOLLOWING LOOP ARE REALLY ONLY RELEVANT IN ANCHORING THE
!***  REFERENCE TEMPERATURE PROFILE AT LEVEL LB.  WHEN BUILDING THE
!***  REFERENCE PROFILE FROM CLOUD BASE, THEN ASSIGNING THE
!***  AMBIENT TEMPERATURE TO TREFK IS ACCEPTABLE.  HOWEVER, WHEN
!***  BUILDING THE REFERENCE PROFILE FROM SOME OTHER LEVEL (SUCH AS
!***  ONE LEVEL ABOVE THE GROUND), THEN TREFK SHOULD BE FILLED WITH
!***  THE TEMPERATURES IN TREF(L) WHICH ARE THE TEMPERATURES OF
!***  THE MOIST ADIABAT THROUGH CLOUD BASE.  BY THE TIME THE LINE 
!***  NUMBERED 450 HAS BEEN REACHED, TREFK ACTUALLY DOES HOLD THE
!***  REFERENCE TEMPERATURE PROFILE.
!***
      DO L=KTS,LMH
        DIFT  (L)=0.
        DIFQ  (L)=0.
        TKL      =T(L)
        TK    (L)=TKL
        TREFK (L)=TKL
        QKL      =Q(L)
        QK    (L)=QKL
        QREFK (L)=QKL
        PKL      =PRSMID(L)
        PK    (L)=PKL
        PSK   (L)=PKL
        APEKL    =APE(L)
        APEK  (L)=APEKL
!
!--- Calculate temperature along moist adiabat (TREF)
!
        IF(PKL<PLQ)THEN
          CALL TTBLEX(ITB,JTB,PL,PKL,RDP,RDTHE                          &
     &               ,STHE,THE0,THES(L),TTBL,TREF(L))
        ELSE
          CALL TTBLEX(ITBQ,JTBQ,PLQ,PKL,RDPQ,RDTHEQ                     &
     &               ,STHEQ,THE0Q,THES(L),TTBLQ,TREF(L))
        ENDIF
        THERK (L)=TREF(L)*APEKL
      ENDDO
!
!------------DEEP CONVECTION REFERENCE TEMPERATURE PROFILE------------
!
      LTP1=LTOP+1
      LBM1=LB-1
      PKB=PK(LB)
      PKT=PK(LTOP)
      STABDL=(EFI-EFIMN)*SLOPST+STABDS
!
!------------TEMPERATURE REFERENCE PROFILE BELOW FREEZING LEVEL-------
!
      EL(LB) = ELWV    
      L0=LB
      PK0=PK(LB)
      TREFKX=TREFK(LB)
      THERKX=THERK(LB)
      APEKXX=APEK(LB)
      THERKY=THERK(LBM1)
      APEKXY=APEK(LBM1)
!
      DO L=LBM1,LTOP,-1
        IF(T(L+1)<TFRZ)GO TO 430
        TREFKX=((THERKY-THERKX)*STABDL                                  &
     &          +TREFKX*APEKXX)/APEKXY
        TREFK(L)=TREFKX
        EL(L)=ELWV
        APEKXX=APEKXY
        THERKX=THERKY
        APEKXY=APEK(L-1)
        THERKY=THERK(L-1)
        L0=L
        PK0=PK(L0)
      ENDDO
!
!--------------FREEZING LEVEL AT OR ABOVE THE CLOUD TOP-----------------
!
      GO TO 450
!
!--------------TEMPERATURE REFERENCE PROFILE ABOVE FREEZING LEVEL-------
!
  430 L0M1=L0-1
      RDP0T=1./(PK0-PKT)
      DTHEM=THERK(L0)-TREFK(L0)*APEK(L0)
!
      DO L=LTOP,L0M1
        TREFK(L)=(THERK(L)-(PK(L)-PKT)*DTHEM*RDP0T)/APEK(L)
        EL(L)=ELWV !ELIV
      ENDDO
!
!-----------------------------------------------------------------------
!--------------DEEP CONVECTION REFERENCE HUMIDITY PROFILE---------------
!-----------------------------------------------------------------------
!
!***  DEPWL IS THE PRESSURE DIFFERENCE BETWEEN CLOUD BASE AND
!***  THE FREEZING LEVEL
!
  450 CONTINUE
      DEPWL=PKB-PK0
      DEPTH=PFRZ*PSFC*RSFCP
      SM1=1.-SM
      PBOTFC=1.
!
!-------------FIRST ADJUSTMENT OF TEMPERATURE PROFILE-------------------
!!
!      SUMDT=0.
!      SUMDP=0.
!!
!      DO L=LTOP,LB
!        SUMDT=(TK(L)-TREFK(L))*DPRS(L)+SUMDT
!        SUMDP=SUMDP+DPRS(L)
!      ENDDO
!!
!      TCORR=SUMDT/SUMDP
!!
!      DO L=LTOP,LB
!        TREFK(L)=TREFK(L)+TCORR
!      ENDDO
!!
!-----------------------------------------------------------------------
!--------------- ITERATION LOOP FOR CLOUD EFFICIENCY -------------------
!-----------------------------------------------------------------------
!
      cloud_efficiency : DO ITREFI=1,ITREFI_MAX  
!
!-----------------------------------------------------------------------
        DSPBK=((EFI-EFIMN)*SLOPBS+DSPBSS*PBOTFC)*SM                     &
     &       +((EFI-EFIMN)*SLOPBL+DSPBSL*PBOTFC)*SM1
        DSP0K=((EFI-EFIMN)*SLOP0S+DSP0SS*PBOTFC)*SM                     &
     &       +((EFI-EFIMN)*SLOP0L+DSP0SL*PBOTFC)*SM1
        DSPTK=((EFI-EFIMN)*SLOPTS+DSPTSS*PBOTFC)*SM                     &
     &       +((EFI-EFIMN)*SLOPTL+DSPTSL*PBOTFC)*SM1
!
!-----------------------------------------------------------------------
!
        DO L=LTOP,LB
!
!***
!***  SATURATION PRESSURE DIFFERENCE
!***
          IF(DEPWL>=DEPTH)THEN
            IF(L<L0)THEN
              DSP=((PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)
            ELSE
              DSP=((PKB-PK(L))*DSP0K+(PK(L)-PK0)*DSPBK)/(PKB-PK0)
            ENDIF
          ELSE
            DSP=DSP0K
            IF(L<L0)THEN
              DSP=((PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)
            ENDIF
          ENDIF
!***
!***  HUMIDITY PROFILE
!***
          PSK(L)=PK(L)+DSP
          APESK(L)=(1.E5/PSK(L))**CAPA

          IF(PK(L)>PQM)THEN
            THSK(L)=TREFK(L)*APEK(L)
            QREFK(L)=PQ0/PSK(L)*EXP(A2*(THSK(L)-A3*APESK(L))            &
     &                                /(THSK(L)-A4*APESK(L)))
          ELSE
            QREFK(L)=QK(L)
          ENDIF
!
        ENDDO
!-----------------------------------------------------------------------
!***
!***  ENTHALPY CONSERVATION INTEGRAL
!***
!-----------------------------------------------------------------------
        enthalpy_conservation : DO ITER=1,2
!
          SUMDE=0.
          SUMDP=0.
          DHDT =0.
!
          DO L=LTOP,LB
            SUMDE=((TK(L)-TREFK(L))*CP+(QK(L)-QREFK(L))*EL(L))*DPRS(L)  &
     &            +SUMDE
            DHDT=(QREFK(L)*A23M4L/((TREFK(L)*APEK(L)/APESK(L))-A4)**2+CP)*DPRS(L) &
     &            +DHDT
            SUMDP=SUMDP+DPRS(L)
          ENDDO
!
          HCORR=SUMDE/(SUMDP-DPRS(LTOP))
          DHDT=DHDT/(SUMDP-DPRS(LTOP))
          LCOR=LTOP+1
!***
!***  FIND LQM
!***
          LQM=1
          DO L=KTS,LB
            IF(PK(L)<=PQM)LQM=L
          ENDDO
!***
!***  ABOVE LQM CORRECT TEMPERATURE ONLY
!***
          IF(LCOR<=LQM)THEN
            DO L=LCOR,LQM
              TREFK(L)=TREFK(L)+HCORR*RCP
            ENDDO
            LCOR=LQM+1
          ENDIF
!***
!***  BELOW LQM CORRECT BOTH TEMPERATURE AND MOISTURE
!***
          DO L=LCOR,LB
            TREFK(L)=HCORR/DHDT+TREFK(L)
            THSKL=TREFK(L)*APEK(L)
            QREFK(L)=PQ0/PSK(L)*EXP(A2*(THSKL-A3*APESK(L))              &
     &                                /(THSKL-A4*APESK(L)))
          ENDDO
!
        ENDDO  enthalpy_conservation
!-----------------------------------------------------------------------
!
!***  HEATING, MOISTENING, PRECIPITATION
!
!-----------------------------------------------------------------------
        AVRGT=0.
        PRECK=0.
        PDQD=0.
        PRWD=0.
        DSQ=0.
        DST=0.
!
        DO L=LTOP,LB
          TKL=TK(L)
          DIFTL=(TREFK(L)-TKL  )*TAUK
          DIFQL=(QREFK(L)-QK(L))*TAUK
          AVRGTL=(TKL+TKL+DIFTL)
          DPOT=DPRS(L)/AVRGTL
          DST=DIFTL*DPOT+DST
          DSQ=DIFQL*EL(L)*DPOT+DSQ
          AVRGT=AVRGTL*DPRS(L)+AVRGT
          PRECK=DIFTL*DPRS(L)+PRECK
          PDQD=(QK(L)-QREFK(L))*DPRS(L)+PDQD
          PRWD=QK(L)*DPRS(L)+PRWD
          DIFT(L)=DIFTL
          DIFQ(L)=DIFQL
        ENDDO
!
        DST=(DST+DST)*CP
        DSQ=DSQ+DSQ
        DENTPY=DST+DSQ
        AVRGT=AVRGT/(SUMDP+SUMDP)
!
!        DRHEAT=PRECK*CP/AVRGT
        DRHEAT=(PRECK*SM+MAX(1.E-7,PRECK)*(1.-SM))*CP/AVRGT !As in Eta!
        DRHEAT=MAX(DRHEAT,1.E-20)
        EFI=EFIFC*DENTPY/DRHEAT
!-----------------------------------------------------------------------
        EFI=MIN(EFI,1.)
        EFI=MAX(EFI,EFIMN)
!-----------------------------------------------------------------------
!
      ENDDO  cloud_efficiency
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!---------------------- DEEP CONVECTION --------------------------------
!-----------------------------------------------------------------------
!
      IF(DENTPY>=EPSNTP.AND.PRECK>EPSPR)THEN
!
        CLDEFI=EFI
        FEFI=EFMNT+SLOPE*(EFI-EFIMN)
        FEFI=(DENTPY-EPSNTP)*FEFI/DENTPY
        PRECK=PRECK*FEFI
!
!***  UPDATE PRECIPITATION AND TENDENCIES OF TEMPERATURE AND MOISTURE
!
        CUP=PRECK*CPRLG
        PCPCOL=CUP
        DQCOL=PDQD/G
        PWCOL=PRWD/G
        DQCOLMIN=(CRMN*TREL)/(FEFI*86400.)
!
        DO L=LTOP,LB
          DTDT(L)=DIFT(L)*FEFI*RDTCNVC
          DQDT(L)=DIFQ(L)*FEFI*RDTCNVC
        ENDDO
!
      ELSE
!
!-----------------------------------------------------------------------
!***  REDUCE THE CLOUD TOP
!-----------------------------------------------------------------------
!
!        LTOP=LTOP+3
!        PTOP=PRSMID(LTOP)
!        DEPMIN=PSH*PSFC*RSFCP
!        DEPTH=PBOT-PTOP
!***
!***  ITERATE DEEP CONVECTION PROCEDURE IF NEEDED
!***
!        IF(DEPTH>=DEPMIN)THEN
!          GO TO 300
!        ENDIF
!
!        CLDEFI=AVGEFI
         CLDEFI=EFIMN*SM+STEFI*(1.-SM)
!        CLDEFI=(EFIMN-0.1)*SM+(1.+0.1)*(1.-SM)
!***
!***  SEARCH FOR SHALLOW CLOUD TOP
!***
!        LTSH=LBOT
!        LBM1=LBOT-1
!        PBTK=PK(LBOT)
!        DEPMIN=PSH*PSFC*RSFCP
!        PTPK=PBTK-DEPMIN
        PTPK=MAX(PSHU, PK(LBOT)-DEPMIN)
!***
!***  CLOUD TOP IS THE LEVEL JUST BELOW PBTK-PSH or JUST BELOW PSHU
!***
        DO L=KTS,LMH
          IF(PK(L)<=PTPK)LTOP=L+1
        ENDDO
!
!        PTPK=PK(LTOP)
!!***
!!***  HIGHEST LEVEL ALLOWED IS LEVEL JUST BELOW PSHU
!!***
!        IF(PTPK<=PSHU)THEN
!!
!          DO L=KTS,LMH
!            IF(PK(L)<=PSHU)LSHU=L+1
!          ENDDO
!!
!          LTOP=LSHU
!          PTPK=PK(LTOP)
!        ENDIF
!
!        if(ltop>=lbot)then
!!!!!!     lbot=0
!          ltop=lmh
!!!!!!     pbot=pk(lbot)
!          ptop=pk(ltop)
!          pbot=ptop
!          go to 600
!        endif
!
!        LTP1=LTOP+1
!        LTP2=LTOP+2
!!
!        DO L=LTOP,LBOT
!          QSATK(L)=PQ0/PK(L)*EXP(A2*(TK(L)-A3)/(TK(L)-A4))
!        ENDDO
!!
!        RHH=QK(LTOP)/QSATK(LTOP)
!        RHMAX=0.
!        LTSH=LTOP
!!
!        DO L=LTP1,LBM1
!          RHL=QK(L)/QSATK(L)
!          DRHDP=(RHH-RHL)/(PK(L-1)-PK(L))
!!
!          IF(DRHDP>RHMAX)THEN
!            LTSH=L-1
!            RHMAX=DRHDP
!          ENDIF
!!
!          RHH=RHL
!        ENDDO
!
!-----------------------------------------------------------------------
!-- Make shallow cloud top a function of virtual temperature excess (DTV)
!-----------------------------------------------------------------------
!
        LTP1=LBOT
        DO L=LBOT-1,LTOP,-1
          IF (DTV(L) > 0.) THEN
            LTP1=L
          ELSE
            EXIT
          ENDIF
        ENDDO
        LTOP=MIN(LTP1,LBOT)
!***
!***  CLOUD MUST BE AT LEAST TWO LAYERS THICK
!***
!        IF(LBOT-LTOP<2)LTOP=LBOT-2  (eliminate this criterion)
!
!-- End: Buoyancy check (24 Aug 2006)
!
        PTOP=PK(LTOP)
        SHALLOW=.TRUE.
        DEEP=.FALSE.
!
      ENDIF
!DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
!DCDCDCDCDCDCDC          END OF DEEP CONVECTION            DCDCDCDCDCDCD
!DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  600 CONTINUE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!----------------GATHER SHALLOW CONVECTION POINTS-----------------------
!
!      IF(PTOP<=PBOT-PNO.AND.LTOP<=LBOT-2)THEN
!         DEPMIN=PSH*PSFC*RSFCP
!!
!!        if(lpbl<lbot)lbot=lpbl
!!        if(lbot>lmh-1)lbot=lmh-1
!!        pbot=prsmid(lbot)
!!
!         IF(PTOP+1.>=PBOT-DEPMIN)SHALLOW=.TRUE.
!      ELSE
!         LBOT=0
!         LTOP=KTE
!      ENDIF
!
!***********************************************************************
!-----------------------------------------------------------------------
!***  Begin debugging convection
      IF(PRINT_DIAG)THEN
        WRITE(6,"(a,2i3,L2,3e12.4)")  &
             '{cu2a lbot,ltop,shallow,pbot,ptop,depmin = ' &
             ,lbot,ltop,shallow,pbot,ptop,depmin
      ENDIF
!***  End debugging convection
!-----------------------------------------------------------------------
!
      IF(.NOT.SHALLOW)GO TO 800
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!SCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
!SCSCSCSCSCSCSC         SHALLOW CONVECTION          CSCSCSCSCSCSCSCSCSCS
!SCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
!-----------------------------------------------------------------------
      DO L=KTS,LMH
        TKL      =T(L)
        TK   (L) =TKL
        TREFK(L) =TKL
        QKL      =Q(L)
        QK   (L) =QKL
        QREFK(L) =QKL
        PKL      =PRSMID(L)
        PK   (L) =PKL
        QSATK(L) =PQ0/PK(L)*EXP(A2*(TK(L)-A3)/(TK(L)-A4))
        APEKL    =APE(L)
        APEK (L) =APEKL
        THVMKL   =TKL*APEKL*(QKL*D608+1.)
        THVREF(L)=THVMKL
!
!        IF(TKL>=TFRZ)THEN
          EL(L)=ELWV
!        ELSE
!          EL(L)=ELIV
!        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!-- Begin: Raise cloud top if avg RH>RHSHmax and CAPE>0
!   RHSHmax=RH at cloud base associated with a DSP of PONE
!-----------------------------------------------------------------------
!
      TLEV2=T(LBOT)*((PK(LBOT)-PONE)/PK(LBOT))**CAPA
      QSAT1=PQ0/PK(LBOT)*EXP(A2*(T(LBOT)-A3)/(TK(LBOT)-A4))
      QSAT2=PQ0/(PK(LBOT)-PONE)*EXP(A2*(TLEV2-A3)/(TLEV2-A4))
      RHSHmax=QSAT2/QSAT1
      SUMDP=0.
      RHAVG=0.
!
      DO L=LBOT,LTOP,-1
        RHAVG=RHAVG+DPRS(L)*QK(L)/QSATK(L)
        SUMDP=SUMDP+DPRS(L)
      ENDDO
!
      IF (RHAVG/SUMDP > RHSHmax) THEN
        LTSH=LTOP
        DO L=LTOP-1,KTS,-1
          RHAVG=RHAVG+DPRS(L)*QK(L)/QSATK(L)
          SUMDP=SUMDP+DPRS(L)
          IF (CPE(L) > 0.) THEN
            LTSH=L
          ELSE
            EXIT
          ENDIF
          IF (RHAVG/SUMDP <= RHSHmax) EXIT
          IF (PK(L) <= PSHU) EXIT
        ENDDO
        LTOP=LTSH
      ENDIF
!
!-- End: Raise cloud top if avg RH>RHSHmax and CAPE>0
!
!---------------------------SHALLOW CLOUD TOP---------------------------
      LBM1=LBOT-1
      PTPK=PTOP
      LTP1=LTOP-1
      DEPTH=PBOT-PTOP
!-----------------------------------------------------------------------
!***  Begin debugging convection
      IF(PRINT_DIAG)THEN
        WRITE(6,"(a,4e12.4)") '{cu2b PBOT,PTOP,DEPTH,DEPMIN= ' &
             ,PBOT,PTOP,DEPTH,DEPMIN
      ENDIF
!***  End debugging convection
!-----------------------------------------------------------------------
!
!BSF      IF(DEPTH<DEPMIN)THEN
!BSF        GO TO 800
!BSF      ENDIF
!-----------------------------------------------------------------------
      IF(PTOP>PBOT-PNO.OR.LTOP>LBOT-2)THEN
        LBOT=0
!!!     LTOP=LBOT
        LTOP=KTE
        PTOP=PBOT
        GO TO 800
      ENDIF
!
!--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX AT TOP-------
!
      THTPK=T(LTP1)*APE(LTP1)
!
      TTHK=(THTPK-THL)*RDTH
      QQK =TTHK-AINT(TTHK)
      IT  =INT(TTHK)+1
!
      IF(IT<1)THEN
        IT=1
        QQK=0.
      ENDIF
!
      IF(IT>=JTB)THEN
        IT=JTB-1
        QQK=0.
      ENDIF
!
!--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY AT TOP--------
!
      BQS00K=QS0(IT)
      SQS00K=SQS(IT)
      BQS10K=QS0(IT+1)
      SQS10K=SQS(IT+1)
!
!--------------SCALING SPEC. HUMIDITY & TABLE INDEX AT TOP--------------
!
      BQK=(BQS10K-BQS00K)*QQK+BQS00K
      SQK=(SQS10K-SQS00K)*QQK+SQS00K
!
!     TQK=(Q(LTOP)-BQK)/SQK*RDQ
      TQK=(Q(LTP1)-BQK)/SQK*RDQ
!
      PPK=TQK-AINT(TQK)
      IQ =INT(TQK)+1
!
      IF(IQ<1)THEN
        IQ=1
        PPK=0.
      ENDIF
!
      IF(IQ>=ITB)THEN
        IQ=ITB-1
        PPK=0.
      ENDIF
!
!----------------CLOUD TOP SATURATION POINT PRESSURE--------------------
!
      PART1=(PTBL(IQ+1,IT)-PTBL(IQ,IT))*PPK
      PART2=(PTBL(IQ,IT+1)-PTBL(IQ,IT))*QQK
      PART3=(PTBL(IQ  ,IT  )-PTBL(IQ+1,IT  )                            &
     &      -PTBL(IQ  ,IT+1)+PTBL(IQ+1,IT+1))*PPK*QQK
      PTPK=PTBL(IQ,IT)+PART1+PART2+PART3
!-----------------------------------------------------------------------
      DPMIX=PTPK-PSP
      IF(ABS(DPMIX).LT.3000.)DPMIX=-3000.
!
!----------------TEMPERATURE PROFILE SLOPE------------------------------
!
      SMIX=(THTPK-THBT)/DPMIX*STABS
!
      TREFKX=TREFK(LBOT+1)
      PKXXXX=PK(LBOT+1)
      PKXXXY=PK(LBOT)
      APEKXX=APEK(LBOT+1)
      APEKXY=APEK(LBOT)
!
      LMID=.5*(LBOT+LTOP)
!
      DO L=LBOT,LTOP,-1
        TREFKX=((PKXXXY-PKXXXX)*SMIX                                    &
     &          +TREFKX*APEKXX)/APEKXY
        TREFK(L)=TREFKX
        IF(L<=LMID) TREFK(L)=MAX(TREFK(L), TK(L)+DTSHAL)
        APEKXX=APEKXY
        PKXXXX=PKXXXY
        APEKXY=APEK(L-1)
        PKXXXY=PK(L-1)
      ENDDO
!
!----------------TEMPERATURE REFERENCE PROFILE CORRECTION---------------
!
      SUMDT=0.
      SUMDP=0.
!
      DO L=LTOP,LBOT
        SUMDT=(TK(L)-TREFK(L))*DPRS(L)+SUMDT
        SUMDP=SUMDP+DPRS(L)
      ENDDO
!
      RDPSUM=1./SUMDP
      FPK(LBOT)=TREFK(LBOT)
!
      TCORR=SUMDT*RDPSUM
!
      DO L=LTOP,LBOT
        TRFKL   =TREFK(L)+TCORR
        TREFK(L)=TRFKL
        FPK  (L)=TRFKL
      ENDDO
!
!----------------HUMIDITY PROFILE EQUATIONS-----------------------------
!
      PSUM  =0.
      QSUM  =0.
      POTSUM=0.
      QOTSUM=0.
      OTSUM =0.
      DST   =0.
      FPTK  =FPK(LTOP)
!
      DO L=LTOP,LBOT
        DPKL  =FPK(L)-FPTK
        PSUM  =DPKL *DPRS(L)+PSUM
        QSUM  =QK(L)*DPRS(L)+QSUM
        RTBAR =2./(TREFK(L)+TK(L))
        OTSUM =DPRS(L)*RTBAR+OTSUM
        POTSUM=DPKL    *RTBAR*DPRS(L)+POTSUM
        QOTSUM=QK(L)   *RTBAR*DPRS(L)+QOTSUM
        DST   =(TREFK(L)-TK(L))*RTBAR*DPRS(L)/EL(L)+DST
      ENDDO
!
      PSUM  =PSUM*RDPSUM
      QSUM  =QSUM*RDPSUM
      ROTSUM=1./OTSUM
      POTSUM=POTSUM*ROTSUM
      QOTSUM=QOTSUM*ROTSUM
      DST   =DST*ROTSUM*CP
!
!-----------------------------------------------------------------------
!***  Begin debugging convection
      IF(PRINT_DIAG)THEN
        WRITE(6,"(a,5e12.4)") '{cu2c DST,PSUM,QSUM,POTSUM,QOTSUM = ' &
             ,DST,PSUM,QSUM,POTSUM,QOTSUM
      ENDIF
!***  End debugging convection
!-----------------------------------------------------------------------
!
!----------------ENSURE POSITIVE ENTROPY CHANGE-------------------------
!
      IF(DST>0.)THEN
!        dstq=dst*epsup
        LBOT=0
!!!!    LTOP=LBOT
        LTOP=KTE
        PTOP=PBOT
        GO TO 800
      ELSE
        DSTQ=DST*EPSDN
      ENDIF
!
!----------------CHECK FOR ISOTHERMAL ATMOSPHERE------------------------
!
      DEN=POTSUM-PSUM
!
      IF(-DEN/PSUM<5.E-5)THEN
        LBOT=0
!!!!    LTOP=LBOT
        LTOP=KTE
        PTOP=PBOT
        GO TO 800
!
!----------------SLOPE OF THE REFERENCE HUMIDITY PROFILE----------------
!
      ELSE
        DQREF=(QOTSUM-DSTQ-QSUM)/DEN
      ENDIF
!
!-------------- HUMIDITY DOES NOT INCREASE WITH HEIGHT------------------
!
      IF(DQREF<0.)THEN
        LBOT=0
!!!!    LTOP=LBOT
        LTOP=KTE
        PTOP=PBOT
        GO TO 800
      ENDIF
!
!----------------HUMIDITY AT THE CLOUD TOP------------------------------
!
      QRFTP=QSUM-DQREF*PSUM
!
!----------------HUMIDITY PROFILE---------------------------------------
!
      DO L=LTOP,LBOT
        QRFKL=(FPK(L)-FPTK)*DQREF+QRFTP
!
!***  TOO DRY CLOUDS NOT ALLOWED
!
        TNEW=(TREFK(L)-TK(L))*TAUKSC+TK(L)
        QSATK(L)=PQ0/PK(L)*EXP(A2*(TNEW-A3)/(TNEW-A4))
        QNEW=(QRFKL-QK(L))*TAUKSC+QK(L)
!
        IF(QNEW<QSATK(L)*RHLSC)THEN
          LBOT=0
!!!!      LTOP=LBOT
          LTOP=KTE
          PTOP=PBOT
          GO TO 800
        ENDIF
!
!-------------TOO MOIST CLOUDS NOT ALLOWED------------------------------
!
        IF(QNEW>QSATK(L)*RHHSC)THEN
          LBOT=0
!!!!      LTOP=LBOT
          LTOP=KTE
          PTOP=PBOT
          GO TO 800
        ENDIF

!
        THVREF(L)=TREFK(L)*APEK(L)*(QRFKL*D608+1.)
        QREFK(L)=QRFKL
      ENDDO
!
!------------------ ELIMINATE CLOUDS WITH BOTTOMS TOO DRY --------------
!!
!      qnew=(qrefk(lbot)-qk(lbot))*tauksc+qk(lbot)
!!
!      if(qnew<qk(lbot+1)*stresh)then  !!?? stresh too large!!
!        lbot=0
!!!!!!   ltop=lbot
!        ltop=kte
!        ptop=pbot
!        go to 800
!      endif
!!
!-------------- ELIMINATE IMPOSSIBLE SLOPES (BETTS,DTHETA/DQ)------------
!
      DO L=LTOP,LBOT
        DTDP=(THVREF(L-1)-THVREF(L))/(PRSMID(L)-PRSMID(L-1))
!
        IF(DTDP<EPSDT)THEN
          LBOT=0
!!!!!     LTOP=LBOT
          LTOP=KTE
          PTOP=PBOT
          GO TO 800
        ENDIF
!
      ENDDO
!-----------------------------------------------------------------------
!--------------RELAXATION TOWARD REFERENCE PROFILES---------------------
!-----------------------------------------------------------------------
!
      DO L=LTOP,LBOT
        DTDT(L)=(TREFK(L)-TK(L))*TAUKSC*RDTCNVC
        DQDT(L)=(QREFK(L)-QK(L))*TAUKSC*RDTCNVC
      ENDDO
!
!-----------------------------------------------------------------------
!***  Begin debugging convection
      IF(PRINT_DIAG)THEN
        DO L=LBOT,LTOP,-1
          WRITE(6,"(a,i3,4e12.4)") '{cu2 KFLIP,DT,DTDT,DQ,DQDT = ' &
               ,KTE+1-L,TREFK(L)-TK(L),DTDT(L),QREFK(L)-QK(L),DQDT(L)
        ENDDO
      ENDIF
!***  End debugging convection
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!SCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
!SCSCSCSCSCSCSC         END OF SHALLOW CONVECTION        SCSCSCSCSCSCSCS
!SCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
!-----------------------------------------------------------------------
  800 CONTINUE
!-----------------------------------------------------------------------
      END SUBROUTINE BMJ
!-----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------------------------------------------
                           SUBROUTINE TTBLEX                            &
     & (ITBX,JTBX,PLX,PRSMID,RDPX,RDTHEX,STHE                           &
     & ,THE0,THESP,TTBL,TREF)
!-----------------------------------------------------------------------
!     ******************************************************************
!     *                                                                *
!     *           EXTRACT TEMPERATURE OF THE MOIST ADIABAT FROM        *
!     *                      THE APPROPRIATE TTBL                      *
!     *                                                                *
!     ******************************************************************
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: ITBX,JTBX
!
      REAL,INTENT(IN) :: PLX,PRSMID,RDPX,RDTHEX,THESP
!
      REAL,DIMENSION(ITBX),INTENT(IN) :: STHE,THE0
!
      REAL,DIMENSION(JTBX,ITBX),INTENT(IN) :: TTBL
!
      REAL,INTENT(OUT) :: TREF
!-----------------------------------------------------------------------
      REAL :: BTHE00K,BTHE10K,BTHK,PK,PP,QQ,STHE00K,STHE10K,STHK        &
     &       ,T00K,T01K,T10K,T11K,TPK,TTHK
!
      INTEGER :: IPTB,ITHTB
!-----------------------------------------------------------------------
!----------------SCALING PRESSURE & TT TABLE INDEX----------------------
!-----------------------------------------------------------------------
      PK=PRSMID
      TPK=(PK-PLX)*RDPX
      QQ=TPK-AINT(TPK)
      IPTB=INT(TPK)+1
!----------------KEEPING INDICES WITHIN THE TABLE-----------------------
      IF(IPTB<1)THEN
        IPTB=1
        QQ=0.
      ENDIF
!
      IF(IPTB>=ITBX)THEN
        IPTB=ITBX-1
        QQ=0.
      ENDIF
!----------------BASE AND SCALING FACTOR FOR THETAE---------------------
      BTHE00K=THE0(IPTB)
      STHE00K=STHE(IPTB)
      BTHE10K=THE0(IPTB+1)
      STHE10K=STHE(IPTB+1)
!----------------SCALING THE & TT TABLE INDEX---------------------------
      BTHK=(BTHE10K-BTHE00K)*QQ+BTHE00K
      STHK=(STHE10K-STHE00K)*QQ+STHE00K
      TTHK=(THESP-BTHK)/STHK*RDTHEX
      PP=TTHK-AINT(TTHK)
      ITHTB=INT(TTHK)+1
!----------------KEEPING INDICES WITHIN THE TABLE-----------------------
      IF(ITHTB<1)THEN
        ITHTB=1
        PP=0.
      ENDIF
!
      IF(ITHTB>=JTBX)THEN
        ITHTB=JTBX-1
        PP=0.
      ENDIF
!----------------TEMPERATURE AT FOUR SURROUNDING TT TABLE PTS.----------
      T00K=TTBL(ITHTB,IPTB)
      T10K=TTBL(ITHTB+1,IPTB)
      T01K=TTBL(ITHTB,IPTB+1)
      T11K=TTBL(ITHTB+1,IPTB+1)
!-----------------------------------------------------------------------
!----------------PARCEL TEMPERATURE-------------------------------------
!-----------------------------------------------------------------------
      TREF=(T00K+(T10K-T00K)*PP+(T01K-T00K)*QQ                          &
     &    +(T00K-T10K-T01K+T11K)*PP*QQ)
!-----------------------------------------------------------------------
      END SUBROUTINE TTBLEX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE BMJINIT(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN            &
     &                  ,CLDEFI,LOWLYR,CP,RD,RESTART                    &
                        ,ALLOWED_TO_READ                                &
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
      REAL,INTENT(IN) :: CP,RD
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) ::            &
     &                                              RTHCUTEN            &
     &                                             ,RQVCUTEN            &
     &                                             ,RQCCUTEN            &
     &                                             ,RQRCUTEN
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CLDEFI

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: LOWLYR
!
      REAL,PARAMETER :: EPS=1.E-9
!
      REAL, DIMENSION(JTB) :: APP,APT,AQP,AQT,PNEW,POLD,QSNEW,QSOLD     &
     &                       ,THENEW,THEOLD,TNEW,TOLD,Y2P,Y2T
!
      REAL,DIMENSION(JTBQ) :: APTQ,AQTQ,THENEWQ,THEOLDQ                 &
     &                       ,TNEWQ,TOLDQ,Y2TQ
!
      INTEGER :: I,J,K,ITF,JTF,KTF
      INTEGER :: KTH,KTHM,KTHM1,KP,KPM,KPM1
!
      REAL :: APE,DP,DQS,DTH,DTHE,P,QS,QS0K,SQSK,STHEK                  &
     &       ,TH,THE0K,DENOM,ELOCP
!-----------------------------------------------------------------------

      ELOCP=ELIWV/CP
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
          RQCCUTEN(I,K,J)=0.
          RQRCUTEN(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
!
        DO J=JTS,JTF
        DO I=ITS,ITF
          CLDEFI(I,J)=AVGEFI
        ENDDO
        ENDDO
      ENDIF
!
!***  FOR NOW, ASSUME SIGMA MODE FOR LOWEST MODEL LAYER
!
      DO J=JTS,JTF
      DO I=ITS,ITF
        LOWLYR(I,J)=1
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!----------------COARSE LOOK-UP TABLE FOR SATURATION POINT--------------
!-----------------------------------------------------------------------
!
      KTHM=JTB
      KPM=ITB
      KTHM1=KTHM-1
      KPM1=KPM-1
!
      DTH=(THH-THL)/REAL(KTHM-1)
      DP =(PH -PL )/REAL(KPM -1)
!
      TH=THL-DTH
!-----------------------------------------------------------------------
      DO 100 KTH=1,KTHM
!
      TH=TH+DTH
      P=PL-DP
!
      DO KP=1,KPM
        P=P+DP
        APE=(100000./P)**(RD/CP)
        DENOM=TH-A4*APE
        IF (DENOM>EPS) THEN
           QSOLD(KP)=PQ0/P*EXP(A2*(TH-A3*APE)/DENOM)
        ELSE
           QSOLD(KP)=0.
        ENDIF
        POLD(KP)=P
      ENDDO
!
      QS0K=QSOLD(1)
      SQSK=QSOLD(KPM)-QSOLD(1)
      QSOLD(1  )=0.
      QSOLD(KPM)=1.
!
      DO KP=2,KPM1
        QSOLD(KP)=(QSOLD(KP)-QS0K)/SQSK
        IF((QSOLD(KP)-QSOLD(KP-1)).LT.EPS)QSOLD(KP)=QSOLD(KP-1)+EPS
      ENDDO
!
      QS0(KTH)=QS0K
      QS0_EXP(KTH)=QS0K
      SQS(KTH)=SQSK
      SQS_EXP(KTH)=SQSK
!-----------------------------------------------------------------------
      QSNEW(1  )=0.
      QSNEW(KPM)=1.
      DQS=1./REAL(KPM-1)
!
      DO KP=2,KPM1
        QSNEW(KP)=QSNEW(KP-1)+DQS
      ENDDO
!
      Y2P(1   )=0.
      Y2P(KPM )=0.
!
      CALL SPLINE(JTB,KPM,QSOLD,POLD,Y2P,KPM,QSNEW,PNEW,APP,AQP)
!
      DO KP=1,KPM
        PTBL(KP,KTH)=PNEW(KP)
        PTBL_EXP(KP,KTH)=PNEW(KP)
      ENDDO
!-----------------------------------------------------------------------
  100 CONTINUE
!-----------------------------------------------------------------------
!------------COARSE LOOK-UP TABLE FOR T(P) FROM CONSTANT THE------------
!-----------------------------------------------------------------------
      P=PL-DP
!
      DO 200 KP=1,KPM
!
      P=P+DP
      TH=THL-DTH
!
      DO KTH=1,KTHM
        TH=TH+DTH
        APE=(1.E5/P)**(RD/CP)
        DENOM=TH-A4*APE
        IF (DENOM>EPS) THEN
           QS=PQ0/P*EXP(A2*(TH-A3*APE)/DENOM)
        ELSE
           QS=0.
        ENDIF
!        QS=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
        TOLD(KTH)=TH/APE
        THEOLD(KTH)=TH*EXP(ELOCP*QS/TOLD(KTH))
      ENDDO
!
      THE0K=THEOLD(1)
      STHEK=THEOLD(KTHM)-THEOLD(1)
      THEOLD(1   )=0.
      THEOLD(KTHM)=1.
!
      DO KTH=2,KTHM1
        THEOLD(KTH)=(THEOLD(KTH)-THE0K)/STHEK
        IF((THEOLD(KTH)-THEOLD(KTH-1)).LT.EPS)                          &
     &      THEOLD(KTH)=THEOLD(KTH-1)  +  EPS
      ENDDO
!
      THE0(KP)=THE0K
      STHE(KP)=STHEK
!-----------------------------------------------------------------------
      THENEW(1  )=0.
      THENEW(KTHM)=1.
      DTHE=1./REAL(KTHM-1)
!
      DO KTH=2,KTHM1
        THENEW(KTH)=THENEW(KTH-1)+DTHE
      ENDDO
!
      Y2T(1   )=0.
      Y2T(KTHM)=0.
!
      CALL SPLINE(JTB,KTHM,THEOLD,TOLD,Y2T,KTHM,THENEW,TNEW,APT,AQT)
!
      DO KTH=1,KTHM
        TTBL(KTH,KP)=TNEW(KTH)
      ENDDO
!-----------------------------------------------------------------------
  200 CONTINUE
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!---------------FINE LOOK-UP TABLE FOR SATURATION POINT-----------------
!-----------------------------------------------------------------------
      KTHM=JTBQ
      KPM=ITBQ
      KTHM1=KTHM-1
      KPM1=KPM-1
!
      DTH=(THHQ-THL)/REAL(KTHM-1)
      DP=(PH-PLQ)/REAL(KPM-1)
!
      TH=THL-DTH
      P=PLQ-DP
!-----------------------------------------------------------------------
!---------------FINE LOOK-UP TABLE FOR T(P) FROM CONSTANT THE-----------
!-----------------------------------------------------------------------
      DO 300 KP=1,KPM
!
      P=P+DP
      TH=THL-DTH
!
      DO KTH=1,KTHM
        TH=TH+DTH
        APE=(1.E5/P)**(RD/CP)
        DENOM=TH-A4*APE
        IF (DENOM>EPS) THEN
           QS=PQ0/P*EXP(A2*(TH-A3*APE)/DENOM)
        ELSE
           QS=0.
        ENDIF
!        QS=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
        TOLDQ(KTH)=TH/APE
        THEOLDQ(KTH)=TH*EXP(ELOCP*QS/TOLDQ(KTH))
      ENDDO
!
      THE0K=THEOLDQ(1)
      STHEK=THEOLDQ(KTHM)-THEOLDQ(1)
      THEOLDQ(1   )=0.
      THEOLDQ(KTHM)=1.
!
      DO KTH=2,KTHM1
        THEOLDQ(KTH)=(THEOLDQ(KTH)-THE0K)/STHEK
        IF((THEOLDQ(KTH)-THEOLDQ(KTH-1))<EPS)                           &
     &      THEOLDQ(KTH)=THEOLDQ(KTH-1)+EPS
      ENDDO
!
      THE0Q(KP)=THE0K
      STHEQ(KP)=STHEK
!-----------------------------------------------------------------------
      THENEWQ(1  )=0.
      THENEWQ(KTHM)=1.
      DTHE=1./REAL(KTHM-1)
!
      DO KTH=2,KTHM1
        THENEWQ(KTH)=THENEWQ(KTH-1)+DTHE
      ENDDO
!
      Y2TQ(1   )=0.
      Y2TQ(KTHM)=0.
!
      CALL SPLINE(JTBQ,KTHM,THEOLDQ,TOLDQ,Y2TQ,KTHM                     &
     &           ,THENEWQ,TNEWQ,APTQ,AQTQ)
!
      DO KTH=1,KTHM
        TTBLQ(KTH,KP)=TNEWQ(KTH)
      ENDDO
!-----------------------------------------------------------------------
  300 CONTINUE
!-----------------------------------------------------------------------
      END SUBROUTINE BMJINIT
!-----------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!-----------------------------------------------------------------------
      SUBROUTINE SPLINE(JTBX,NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)
!   ********************************************************************
!   *                                                                  *
!   *  THIS IS A ONE-DIMENSIONAL CUBIC SPLINE FITTING ROUTINE          *
!   *  PROGRAMED FOR A SMALL SCALAR MACHINE.                           *
!   *                                                                  *
!   *  PROGRAMER Z. JANJIC                                             *
!   *                                                                  *
!   *  NOLD - NUMBER OF GIVEN VALUES OF THE FUNCTION.  MUST BE GE 3.   *
!   *  XOLD - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE       *
!   *         FUNCTION ARE GIVEN.  MUST BE IN ASCENDING ORDER.         *
!   *  YOLD - THE GIVEN VALUES OF THE FUNCTION AT THE POINTS XOLD.     *
!   *  Y2   - THE SECOND DERIVATIVES AT THE POINTS XOLD.  IF NATURAL   *
!   *         SPLINE IS FITTED Y2(1)=0. AND Y2(NOLD)=0. MUST BE        *
!   *         SPECIFIED.                                               *
!   *  NNEW - NUMBER OF VALUES OF THE FUNCTION TO BE CALCULATED.       *
!   *  XNEW - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE       *
!   *         FUNCTION ARE CALCULATED.  XNEW(K) MUST BE GE XOLD(1)     *
!   *         AND LE XOLD(NOLD).                                       *
!   *  YNEW - THE VALUES OF THE FUNCTION TO BE CALCULATED.             *
!   *  P, Q - AUXILIARY VECTORS OF THE LENGTH NOLD-2.                  *
!   *                                                                  *
!   ********************************************************************
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: JTBX,NNEW,NOLD
      REAL,DIMENSION(JTBX),INTENT(IN) :: XNEW,XOLD,YOLD
      REAL,DIMENSION(JTBX),INTENT(INOUT) :: P,Q,Y2
      REAL,DIMENSION(JTBX),INTENT(OUT) :: YNEW
!
      INTEGER :: K,K1,K2,KOLD,NOLDM1
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                   &
     &       ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1
!-----------------------------------------------------------------------
      NOLDM1=NOLD-1
!
      DXL=XOLD(2)-XOLD(1)
      DXR=XOLD(3)-XOLD(2)
      DYDXL=(YOLD(2)-YOLD(1))/DXL
      DYDXR=(YOLD(3)-YOLD(2))/DXR
      RTDXC=0.5/(DXL+DXR)
!
      P(1)= RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))
      Q(1)=-RTDXC*DXR
!
      IF(NOLD==3)GO TO 150
!-----------------------------------------------------------------------
      K=3
!
  100 DXL=DXR
      DYDXL=DYDXR
      DXR=XOLD(K+1)-XOLD(K)
      DYDXR=(YOLD(K+1)-YOLD(K))/DXR
      DXC=DXL+DXR
      DEN=1./(DXL*Q(K-2)+DXC+DXC)
!
      P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
      Q(K-1)=-DEN*DXR
!
      K=K+1
      IF(K<NOLD)GO TO 100
!-----------------------------------------------------------------------
  150 K=NOLDM1
!
  200 Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)
!
      K=K-1
      IF(K>1)GO TO 200
!-----------------------------------------------------------------------
      K1=1
!
  300 XK=XNEW(K1)
!
      DO 400 K2=2,NOLD
!
      IF(XOLD(K2)>XK)THEN
        KOLD=K2-1
        GO TO 450
      ENDIF
!
  400 CONTINUE
!
      YNEW(K1)=YOLD(NOLD)
      GO TO 600
!
  450 IF(K1==1)GO TO 500
      IF(K==KOLD)GO TO 550
!
  500 K=KOLD
!
      Y2K=Y2(K)
      Y2KP1=Y2(K+1)
      DX=XOLD(K+1)-XOLD(K)
      RDX=1./DX
!
      AK=.1666667*RDX*(Y2KP1-Y2K)
      BK=0.5*Y2K
      CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)
!
  550 X=XK-XOLD(K)
      XSQ=X*X
!
      YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)
!
  600 K1=K1+1
      IF(K1<=NNEW)GO TO 300
!-----------------------------------------------------------------------
      END SUBROUTINE SPLINE
!-----------------------------------------------------------------------
!
      END MODULE MODULE_CU_BMJ
!
!-----------------------------------------------------------------------
