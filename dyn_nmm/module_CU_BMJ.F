!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_cu_bmj
      use module_mpp
!
      LOGICAL :: UNIS=.TRUE.,UNIL=.FALSE.
!
!jw  removed EPSQ=1.E-12 from the following parameter statement
!jw  because it's also defined in module_NMM_CONSTANTS and was
!jw  generating a compiler error
      REAL,PARAMETER :: A2=17.2693882,A3=273.16,A4=35.86              &
                       ,DSPC=-3000.                                   &
                       ,DTTOP=0.,EFIFC=5.0,EFIMN=.20,EFMNT=.70        &
                       ,ELIVW=2.72E6                                  &
                       ,EPSDN=1.05,EPSDT=0.,EPSNTP=1.E-4              &
                       ,EPSP=1.E-7,EPSUP=1.0                          &
                       ,FCC=.90,FSL=.850,FSS=.85                      &
                       ,PBM=13000.,PFRZ=15000.,PNO=1000.              &
                       ,PONE=2500.,PQM=20000.,PQ0=379.90516           &
                       ,PSH=20000.,PSHU=45000.                        &
                       ,RHLSC=0.50,RHHSC=0.99                         &
                       ,RHF=0.10,ROW=1.E3                             &
                       ,STABD=.90,STABFC=1.0                          &
                       ,STABS=1.0,STRESH=1.10                         &
                       ,T1=274.16,TREL=2400.
!
      REAL,PARAMETER :: DSPBFL=-3875.,DSP0FL=-5875.,DSPTFL=-1875.     &
                       ,DSPBFS=-3875.,DSP0FS=-5875.,DSPTFS=-1875.
!
      REAL,PARAMETER :: PL=2500.,PLQ=70000.,PH=105000.                &
                       ,THL=210.,THH=365.,THHQ=325.
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
!
      REAL,PARAMETER :: RDP=(ITB-1.)/(PH-PL),RDPQ=(ITBQ-1.)/(PH-PLQ)  &
                       ,RDQ=ITB-1,RDTH=(JTB-1.)/(THH-THL)             &
                       ,RDTHE=JTB-1.,RDTHEQ=JTBQ-1.
!
CONTAINS
!
!---------------------------------------------------------------------
      SUBROUTINE BMJDRV(DT,ITIMESTEP,STEPCU                           &
                       ,RTHCUTEN,RQVCUTEN                             &
!#$                    ,RAINCV,HTOP,HBOT                              &
                       ,RAINCV,LTOP,LBOT,LPBL                         &
                       ,TH,T,QV,PINT,PMID,PI,RHO,DZ8W                 &
                       ,CP,R,ELWV,G,TFRZ,D608                         &
                       ,CLDEFI,LOWLYR,XLAND,CU_ACT_FLAG               &
                       ,IDS,IDE,JDS,JDE,KDS,KDE                       &
                       ,IMS,IME,JMS,JME,KMS,KME                       &
                       ,ITS,ITE,JTS,JTE,KTS,KTE)
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                   &
                           ,IMS,IME,JMS,JME,KMS,KME                   & 
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: ITIMESTEP,STEPCU
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LOWLYR,LPBL
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: LBOT,LTOP
!
      REAL,INTENT(IN) :: CP,DT,ELWV,G,R,TFRZ,D608
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: XLAND
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                         &
          ,INTENT(IN) ::                                              &
                                              TH                      &
                                             ,T                       &
                                             ,QV                      &
                                             ,PINT                    &
                                             ,PMID                    &
                                             ,PI                      &
                                             ,RHO                     &
                                             ,DZ8W
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME)                         &
          ,INTENT(INOUT) ::                                           &
                                              RQVCUTEN,RTHCUTEN      
! 
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: RAINCV         &
                                                      ,CLDEFI         
!#$                                                   ,CLDEFI         &
!#$                                                     ,HTOP         &
!#$                                                     ,HBOT      

      LOGICAL, DIMENSION( ims:ime , jms:jme ),                  &
               INTENT(INOUT) :: CU_ACT_FLAG

!
!--------------------------------------------------------------------- 
!***
!***  LOCAL VARIABLES
!***
      REAL :: CUBOT,CUTOP,DTCNVC,LANDMASK,PCPCOL,PSFC,PTOP
! 
      REAL,DIMENSION(KTS:KTE) :: DPCOL,DQDT,DTDT,PCOL,QCOL,TCOL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: HBOT,HTOP
!
      INTEGER :: I,J,K,KFLIP,ICLDCK,LMH
!--------------------------------------------------------------------- 
!********************************************************************* 
!--------------------------------------------------------------------- 
!
!***  PREPARE TO CALL BMJ CONVECTION SCHEME
!
!---------------------------------------------------------------------
!
!***  CHECK TO SEE IF THIS IS A CONVECTION TIMESTEP
!                                                                        
      ICLDCK=MOD(ITIMESTEP,STEPCU)                                              
!---------------------------------------------------------------------
!                                                                      
!***  COMPUTE CONVECTION EVERY STEPCU*DT/60.0 MINUTES
!                                                                     

      IF(ICLDCK.EQ.0 .or. ITIMESTEP .eq. 1)THEN

        DO J = jts,jte
        DO I= its,ite
           CU_ACT_FLAG(i,j) = .true.
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
          RAINCV(I,J)=0.
          PCPCOL=0.
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
            TCOL(K)=T(I,KFLIP,J)

! change 10/18/00 
! from mixing ratio to specific humidity (prevent -ve 4/23/01)

            QCOL(K)=MAX(0.,QV(I,KFLIP,J)/(1.+QV(I,KFLIP,J)))
            PCOL(K)=PMID(I,KFLIP,J)
!           DPCOL(K)=PINT(I,KFLIP,J)-PINT(I,KFLIP+1,J)
            DPCOL(K)=RHO(I,KFLIP,J)*G*DZ8W(I,KFLIP,J)
          ENDDO
!
!***  LOWEST LAYER ABOVE GROUND MUST ALSO BE FLIPPED
!
          LMH=KTE+1-LOWLYR(I,J)
!***
!***  CALL CONVECTION
!***
          CALL BMJ(I,J,DTCNVC,LMH,LANDMASK,CLDEFI(I,J),HTOP(I,J),HBOT(I,J) &
                  ,DPCOL,PCOL,QCOL,TCOL,PSFC,PTOP                     &
!#$               ,DQDT,DTDT,PCPCOL,LBOT,LTOP                         &
                  ,DQDT,DTDT,PCPCOL,LBOT(I,J),LTOP(I,J),LPBL(I,J)     &
                  ,CP,R,ELWV,G,TFRZ,D608                              &        
                  ,IDS,IDE,JDS,JDE,KDS,KDE                            &        
                  ,IMS,IME,JMS,JME,KMS,KME                            &
                  ,ITS,ITE,JTS,JTE,KTS,KTE,itimestep)
! 
!***  COMPUTE HEATING AND MOISTENING TENDENCIES
!
          DO K=KTS,KTE
            KFLIP=KTE+1-K
            RTHCUTEN(I,K,J)=DTDT(KFLIP)/PI(I,K,J)

! change 10/18/00 
! from specific humidity back to mixing ration

            RQVCUTEN(I,K,J)=DQDT(KFLIP)/(1.-QCOL(KFLIP))**2
          ENDDO
!
!***  ALL UNITS IN BMJ SCHEME ARE MKS, THUS CONVERT PRECIP FROM METERS
!***  TO MILLIMETERS PER STEP
!
          RAINCV(I,J)=PCPCOL*1.E3/STEPCU
!
!***  CUBOT AND CUTOP ARE THE BOTTOM AND TOP LAYER, RESPECTIVELY,
!***  OF THE CONVECTIVE CLOUD.  IF NEED BE THEY CAN BE USED IN
!***  THE RADIATION COMPUTATIONS.
!
!#$       IF(CUBOT.NE.0)CUBOT=REAL(KTE+1-LBOT)
!#$       IF(CUTOP.NE.0)CUTOP=REAL(KTE+1-LTOP)
!
        ENDDO
        ENDDO
!
      ENDIF
!
      END SUBROUTINE BMJDRV
!---------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------------------------------------------
                          SUBROUTINE BMJ                              &
!---------------------------------------------------------------------
       (I,J,DTCNVC,LMH,SM,CLDEFI,HTOP,HBOT                            &
       ,DPRS,PRSMID,Q,T,PSFC,PT                                       &
       ,DQDT,DTDT,PCPCOL,LBOT,LTOP,LPBL                               &
       ,CP,R,ELWV,G,TFRZ,D608                                         &
       ,IDS,IDE,JDS,JDE,KDS,KDE                                       &
       ,IMS,IME,JMS,JME,KMS,KME                                       &
       ,ITS,ITE,JTS,JTE,KTS,KTE,itimestep)
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                   &
                           ,IMS,IME,JMS,JME,KMS,KME                   &
                           ,ITS,ITE,JTS,JTE,KTS,KTE                   &
                           ,I,J,itimestep
! 
      INTEGER,INTENT(IN) :: LMH,LPBL
!
      INTEGER,INTENT(OUT) :: LBOT,LTOP
!
      REAL,INTENT(IN) :: CP,D608,DTCNVC,ELWV,G,PSFC,PT,R,SM,TFRZ
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: DPRS,PRSMID,Q,T
      REAL,INTENT(INOUT) :: HTOP,HBOT
!
      REAL,INTENT(INOUT) :: CLDEFI,PCPCOL
!
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) :: DQDT,DTDT
!
!---------------------------------------------------------------------
!***  DEFINE LOCAL VARIABLES
!---------------------------------------------------------------------
!                                                            
      REAL,DIMENSION(KTS:KTE) :: APEK,APESK,FPK                       &
                                ,PK,PSK,QK,QREFK,QSATK                &
                                ,THERK,THEVRF,THSK                    &
                                ,THVMOD,THVREF,TK,TREFK
!
      REAL,DIMENSION(KTS:KTE) :: APE,DIFQ,DIFT,TREF
!
      LOGICAL :: DEEP,SHALLOW
!
!***
!***  LOCAL SCALARS
!***
      REAL :: APEKL,APEKXX,APEKXY,APESP,APESTS                        &
             ,AVRGT,AVRGTL,BQ,BQK,BQS00K,BQS10K                       &
             ,CAPA,CTHRS,DEN,DENTPY,DEPMIN,DEPTH                      &
             ,DEPWL,DHDT,DIFQL,DIFTL,DPKL,DPMIX                       &
             ,DQREF,DRHDP,DRHEAT,DSP                                  &
             ,DSP0,DSP0K,DSPB,DSPBK,DSPT,DSPTK                        &
             ,DST,DSTQ,DTHEM,DTDP,EFI                                 &
             ,FEFI,FPTK,HCORR,OTSUM,P,P00K,P01K,P10K,P11K             &
             ,PART1,PART2,PART3,PBOT,PBOTFC,PBTK                      &
             ,PK0,PKB,PKL,PKT,PKXXXX,PKXXXY                           &
             ,PLMH,POTSUM,PP1,PPK,PRECK                               &
             ,PRESK,PSP,PSUM,PTHRS,PTOP,PTPK                          &
             ,QBT,QKL,QNEW,QOTSUM,QQ1,QQK,QRFKL,QRFTP,QSUM,RDP0T      &
             ,RDPSUM,RDTCNVC,RHH,RHL,RHMAX,RHNEW,ROTSUM,RTBAR         &
             ,SM1,SMIX,SQ,SQK,SQS00K,SQS10K,STABDL,SUMDE,SUMDP        &
             ,SUMDT,TAUK,TCORR,THBT,THERKX,THERKY                     &
             ,THESP,THSKL,THTPK,THVMKL,TKL                            &
             ,TPSP,TQ,TQK,TREFKX,TRFKL,TSKL,TTH,TTHBT,TTHES,TTHK
!
      INTEGER :: IQ,IQTB,IT,ITER,ITREFI,ITTB,ITTBK,KB,KNUMH,KNUML     &
                ,L,L0,L0M1,LB,LBM1,LCOR,LPT1                          &
                ,LQM,LSHU,LTP1,LTP2,LTSH
!---------------------------------------------------------------------
!
      REAL,PARAMETER :: DSPBSL=DSPBFL*FSL,DSP0SL=DSP0FL*FSL           &
                       ,DSPTSL=DSPTFL*FSL                             &
                       ,DSPBSS=DSPBFS*FSS,DSP0SS=DSP0FS*FSS           &
                       ,DSPTSS=DSPTFS*FSS
!
      REAL,PARAMETER :: AVGEFI=(EFIMN+1.)*.5,STEFI=1.
!
      REAL,PARAMETER :: SLOPBL=(DSPBFL-DSPBSL)/(1.-EFIMN)             &
                       ,SLOP0L=(DSP0FL-DSP0SL)/(1.-EFIMN)             &
                       ,SLOPTL=(DSPTFL-DSPTSL)/(1.-EFIMN)             &
                       ,SLOPBS=(DSPBFS-DSPBSS)/(1.-EFIMN)             &
                       ,SLOP0S=(DSP0FS-DSP0SS)/(1.-EFIMN)             &
                       ,SLOPTS=(DSPTFS-DSPTSS)/(1.-EFIMN)             &
                       ,SLOPE=(1.-EFMNT)/(1.-EFIMN)
!
      REAL :: A23M4L,CPRLG,ELOCP,FCB,RCP
!---------------------------------------------------------------------
!*********************************************************************
!---------------------------------------------------------------------
      CAPA=R/CP
      CPRLG=CP/(ROW*G*ELWV)
      ELOCP=ELIVW/CP
      RCP=1./CP
      A23M4L=A2*(A3-A4)*ELWV
      FCB=1.-FCC
      RDTCNVC=1./DTCNVC
!
      DEEP=.FALSE.
      SHALLOW=.FALSE.
!
      DSP0=0.
      DSPB=0.
      DSPT=0.
      PSP=0.
!---------------------------------------------------------------------
      TAUK=DTCNVC/TREL
      CTHRS=(0.006350/86400.)*DTCNVC/CPRLG
!---------------------------------------------------------------------
!---------------------------PREPARATIONS------------------------------
!---------------------------------------------------------------------
      LBOT=LMH
      THESP=0.
      PCPCOL=0.
      TREF(1)=T(1)
!
      DO L=1,LMH
        APESTS=PRSMID(L)
        APE(L)=(1.E5/APESTS)**CAPA
      ENDDO
!---------------------------------------------------------------------
!--------------SEARCH FOR MAXIMUM BUOYANCY LEVEL----------------------
!---------------------------------------------------------------------
                             DO 170 KB=1,LMH
!---------------------------------------------------------------------
!--------------TRIAL MAXIMUM BUOYANCY LEVEL VARIABLES-----------------
!---------------------------------------------------------------------
!
      PKL=PRSMID(KB)
      PLMH=PRSMID(LMH)
!
!**   SEARCH OVER A SCALED DEPTH IN FINDING THE PARCEL
!***  WITH THE MAX THETA-E 
!
      IF(PKL.GE.0.80*PLMH)THEN
        QBT=Q(KB)
        TTHBT=T(KB)*APE(KB)
        TTH=(TTHBT-THL)*RDTH
        QQ1=TTH-AINT(TTH)
        ITTB=INT(TTH)+1
!--------------KEEPING INDICES WITHIN THE TABLE-----------------------
        IF(ITTB.LT.1)THEN
          ITTB=1
          QQ1=0.
        ENDIF
!
        IF(ITTB.GE.JTB)THEN
          ITTB=JTB-1
          QQ1=0.
        ENDIF
!------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
        ITTBK=ITTB
        BQS00K=QS0(ITTBK)
        SQS00K=SQS(ITTBK)
        BQS10K=QS0(ITTBK+1)
        SQS10K=SQS(ITTBK+1)
!------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
        BQ=(BQS10K-BQS00K)*QQ1+BQS00K
        SQ=(SQS10K-SQS00K)*QQ1+SQS00K
        TQ=(QBT-BQ)/SQ*RDQ
        PP1=TQ-AINT(TQ)
        IQTB=INT(TQ)+1
!--------------KEEPING INDICES WITHIN THE TABLE-----------------------
        IF(IQTB.LT.1)THEN
          IQTB=1
          PP1=0.
        ENDIF
!
        IF(IQTB.GE.ITB)THEN
          IQTB=ITB-1
          PP1=0.
        ENDIF
!------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
        IQ=IQTB
        IT=ITTB
        P00K=PTBL(IQ  ,IT  )
        P10K=PTBL(IQ+1,IT  )
        P01K=PTBL(IQ  ,IT+1)
        P11K=PTBL(IQ+1,IT+1)
!--------------SATURATION POINT VARIABLES AT THE BOTTOM---------------
        TPSP=P00K+(P10K-P00K)*PP1+(P01K-P00K)*QQ1                     &
                 +(P00K-P10K-P01K+P11K)*PP1*QQ1
        APESP=(1.E5/TPSP)**CAPA
        TTHES=TTHBT*EXP(ELOCP*QBT*APESP/TTHBT)
!--------------CHECK FOR MAXIMUM BUOYANCY-----------------------------
        IF(TTHES.GT.THESP)THEN
          PSP  =TPSP
          THBT =TTHBT
          THESP=TTHES
        ENDIF
!
      ENDIF
!---------------------------------------------------------------------
  170 CONTINUE
!---------------------------------------------------------------------
!---------CHOOSE CLOUD BASE AS MODEL LEVEL JUST BELOW PSP-------------
!---------------------------------------------------------------------
      DO L=1,LMH-1
        P=PRSMID(L)
        IF(P.LT.PSP.AND.P.GE.PQM)LBOT=L+1
      ENDDO
!***
!*** WARNING: LBOT MUST NOT BE GT LMH-1 IN SHALLOW CONVECTION
!*** MAKE SURE CLOUD BASE IS AT LEAST PONE ABOVE THE SURFACE
!***
      PBOT=PRSMID(LBOT)
      PLMH=PRSMID(LMH)
      IF(PBOT.GE.PLMH-PONE.OR.LBOT.GE.LMH)THEN
!
        DO L=1,LMH-1
          P=PRSMID(L)
          IF(P.LT.PLMH-PONE)LBOT=L
        ENDDO
!
        PBOT=PRSMID(LBOT)
      ENDIF 
!--------------CLOUD TOP COMPUTATION----------------------------------
      LTOP=LBOT
      PTOP=PBOT
!---------------------------------------------------------------------
!***
!***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT
!***  SCALING PRESSURE & TT TABLE INDEX
!***
!
      DO L=LMH,1,-1
!
        PRESK=PRSMID(L)
!
        IF(PRESK.LT.PLQ)THEN
          CALL TTBLEX(ITB,JTB,PL,PRESK,RDP,RDTHE                      &
                     ,STHE,THE0,THESP,TTBL,TREF(L))
        ELSE
          CALL TTBLEX(ITBQ,JTBQ,PLQ,PRESK,RDPQ,RDTHEQ                 &
                     ,STHEQ,THE0Q,THESP,TTBLQ,TREF(L))
        ENDIF
!
      ENDDO
!
!--------------BUOYANCY CHECK-----------------------------------------
!
      DO L=LMH,1,-1
        IF(TREF(L).GT.T(L)-DTTOP)LTOP=L
      ENDDO
!
!***  CLOUD TOP PRESSURE
!
      PTOP=PRSMID(LTOP)
!---------------------------------------------------------------------
!***
!***  CLEAN UP AND GATHER DEEP CONVECTION POINTS
!***
      IF(LTOP.GE.LBOT)THEN
        LBOT=0
        LTOP=LBOT
        PTOP=PBOT
      ENDIF

!
      IF(PTOP.GT.PBOT-PNO.OR.LTOP.GT.LBOT-2)THEN
!#$      CLDEFI=AVGEFI*SM+STEFI*(1.-SM)
         CLDEFI=1.
      ENDIF
!
!***  DEPTH OF CLOUD REQUIRED TO MAKE THE POINT A DEEP CONVECTION POINT
!***  IS A SCALED VALUE OF PSFC.
!
!#$   DEPMIN=PSH*PSFC*1.E-5
      DEPMIN=PSH*PSFC/101300.
      DEPTH=PBOT-PTOP
!
      IF(DEPTH.GE.DEPMIN)THEN
        DEEP=.TRUE.
      ENDIF
!*********************************************************************
!********************** DEEP CONVECTION ******************************
!*********************************************************************
      IF(.NOT.DEEP)GO TO 600
!*********************************************************************
!DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
!DCDCDCDCDCDC  DEEP CONVECTION   DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
!DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
      LB   =LBOT
      EFI  =CLDEFI
!------------INITIALIZE VARIABLES IN THE CONVECTIVE COLUMN------------
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
      DO L=1,LMH
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
        THERK (L)=TREF(L)*APEKL
      ENDDO
!------------DEEP CONVECTION REFERENCE TEMPERATURE PROFILE------------
      LTP1=LTOP+1
      LBM1=LB-1
      PKB=PK(LB)
      PKT=PK(LTOP)
!------------TEMPERATURE REFERENCE PROFILE BELOW FREEZING LEVEL-------
      L0=LB
      PK0=PK(LB)
      TREFKX=TREFK(LB)
      THERKX=THERK(LB)
      APEKXX=APEK(LB)
      THERKY=THERK(LBM1)
      APEKXY=APEK(LBM1)
!
      DO L=LBM1,LTOP,-1
        IF(T(L+1).LT.TFRZ)GO TO 430
        STABDL=STABD
        TREFKX=((THERKY-THERKX)*STABDL                                &
                +TREFKX*APEKXX)/APEKXY
        TREFK(L)=TREFKX
        APEKXX=APEKXY
        THERKX=THERKY
        APEKXY=APEK(L-1)
        THERKY=THERK(L-1)
        L0=L
        PK0=PK(L0)
      ENDDO
!------------FREEZING LEVEL AT OR ABOVE THE CLOUD TOP-----------------
      L0M1=L0-1
      GO TO 450
!------------TEMPERATURE REFERENCE PROFILE ABOVE FREEZING LEVEL-------
  430 L0M1=L0-1
      RDP0T=1./(PK0-PKT)
      DTHEM=THERK(L0)-TREFK(L0)*APEK(L0)
!
      DO L=LTOP,L0M1
        TREFK(L)=(THERK(L)-(PK(L)-PKT)*DTHEM*RDP0T)/APEK(L)
      ENDDO
!---------------------------------------------------------------------
!------------DEEP CONVECTION REFERENCE HUMIDITY PROFILE---------------
!---------------------------------------------------------------------
!
!***  DEPWL IS THE PRESSURE DIFFERENCE BETWEEN CLOUD BASE AND
!***  THE FREEZING LEVEL
!
  450 DEPWL=PKB-PK0
!#$   DEPTH=PFRZ*PSFC*1.E-5
      DEPTH=PFRZ*PSFC/101300.
      SM1=1.-SM
!     PBOTFC=101300./PSFCIJ
      PBOTFC=1.
!
!----------------------------------------------------------------------
!-------------- ITERATION LOOP FOR CLOUD EFFICIENCY -------------------
!----------------------------------------------------------------------
!
      cloud_efficiency : DO ITREFI=1,ITREFI_MAX  
!
!----------------------------------------------------------------------
      DSPBK=((EFI-EFIMN)*SLOPBS+DSPBSS*PBOTFC)*SM                      &
           +((EFI-EFIMN)*SLOPBL+DSPBSL*PBOTFC)*SM1
      DSP0K=((EFI-EFIMN)*SLOP0S+DSP0SS*PBOTFC)*SM                      &
           +((EFI-EFIMN)*SLOP0L+DSP0SL*PBOTFC)*SM1
      DSPTK=((EFI-EFIMN)*SLOPTS+DSPTSS*PBOTFC)*SM                      &
           +((EFI-EFIMN)*SLOPTL+DSPTSL*PBOTFC)*SM1
!
!----------------------------------------------------------------------
!
      DO L=LTOP,LB
!
!***
!***  SATURATION PRESSURE DIFFERENCE
!***
!       IF(PKB-PK0.GE.PFRZ)THEN
        IF(DEPWL.GE.DEPTH)THEN
          IF(L.LT.L0)THEN
            DSP=((PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)
          ELSE
            DSP=((PKB-PK(L))*DSP0K+(PK(L)-PK0)*DSPBK)/(PKB-PK0)
          ENDIF
        ELSE
!         DSP=DSPC
          DSP=DSP0K
          IF(L.LT.L0)THEN
            DSP=((PK0-PK(L))*DSPTK+(PK(L)-PKT)*DSP0K)/(PK0-PKT)
          ENDIF
        ENDIF
!***
!***  HUMIDITY PROFILE
!***
        IF(PK(L).GT.PQM)THEN
          PSK(L)=PK(L)+DSP
          APESK(L)=(1.E5/PSK(L))**CAPA
          THSK(L)=TREFK(L)*APEK(L)
          QREFK(L)=PQ0/PSK(L)*EXP(A2*(THSK(L)-A3*APESK(L))             &
                                    /(THSK(L)-A4*APESK(L)))
        ELSE
          QREFK(L)=QK(L)
        ENDIF
!
      ENDDO
!----------------------------------------------------------------------
!***
!***  ENTHALPY CONSERVATION INTEGRAL
!***
      LQM=0
!
      enthalpy_conservation : DO ITER=1,2
!
        SUMDE=0.
        SUMDP=0.
!
        DO L=LTOP,LB
          SUMDE=((TK(L)-TREFK(L))*CP+(QK(L)-QREFK(L))*ELWV)*DPRS(L)    &
                +SUMDE
          SUMDP=SUMDP+DPRS(L)
        ENDDO
!
        HCORR=SUMDE/(SUMDP-DPRS(LTOP))
        LCOR=LTOP+1
!***
!***  FIND LQM
!***
        DO L=1,LB
          IF(PK(L).LE.PQM)LQM=L
        ENDDO
!***
!***  ABOVE LQM CORRECT TEMPERATURE ONLY
!***
        IF(LCOR.LE.LQM)THEN
          DO L=LCOR,LQM
            TREFK(L)=TREFK(L)+HCORR*RCP
          ENDDO
          LCOR=LQM+1
        ENDIF
!***
!***  BELOW LQM CORRECT BOTH TEMPERATURE AND MOISTURE
!***
        DO L=LCOR,LB
          TSKL=TREFK(L)*APEK(L)/APESK(L)
          DHDT=QREFK(L)*A23M4L/(TSKL-A4)**2+CP
          TREFK(L)=HCORR/DHDT+TREFK(L)
          THSKL=TREFK(L)*APEK(L)
          QREFK(L)=PQ0/PSK(L)*EXP(A2*(THSKL-A3*APESK(L))               &
                                    /(THSKL-A4*APESK(L)))
        ENDDO
!
      ENDDO  enthalpy_conservation
!----------------------------------------------------------------------
!
!***  HEATING, MOISTENING, PRECIPITATION
!
      DENTPY=0.
      AVRGT =0.
      PRECK =0.
!
      DO L=LTOP,LB
        TKL=TK(L)
        DIFTL=(TREFK(L)-TKL  )*TAUK
        DIFQL=(QREFK(L)-QK(L))*TAUK
        AVRGTL=(TKL+TKL+DIFTL)
        DENTPY=(DIFTL*CP+DIFQL*ELWV)*DPRS(L)/AVRGTL+DENTPY
        AVRGT=AVRGTL*DPRS(L)+AVRGT
        PRECK=DIFTL*DPRS(L)+PRECK
        DIFT(L)=DIFTL
        DIFQ(L)=DIFQL
      ENDDO
!
      DENTPY=DENTPY+DENTPY
      AVRGT =AVRGT/(SUMDP+SUMDP)
!
      DRHEAT=PRECK*CP/AVRGT
      EFI=EFIFC*DENTPY/DRHEAT
!zj   EFI=CLDEFI(I,J)*FCB+EFI*FCC
!----------------------------------------------------------------------
      EFI=MIN(EFI,1.)
      EFI=MAX(EFI,EFIMN)
!
      IF(PRECK.LE.0.)THEN
        EFI=1.
      ENDIF
!----------------------------------------------------------------------
      ENDDO  cloud_efficiency
!----------------------------------------------------------------------
!***
!***  SWAP IF ENTROPY AND/OR PRECIP .LT. 0
!***
!----------------------------------------------------------------------
      IF(DENTPY.LT.EPSNTP.OR.PRECK.LT.0.)THEN
!       CLDEFI=EFIMN*SM+STEFI*(1.-SM)
        CLDEFI=1.
!***
!***  SEARCH FOR SHALLOW CLOUD TOP
!***
        LTSH=LBOT
        LBM1=LBOT-1
        PBTK=PK(LBOT)
        DEPMIN=PSH*PSFC/101300.
        PTPK=PBTK-DEPMIN
!***
!***  CLOUD TOP IS THE LEVEL JUST BELOW PBTK-PSH
!***
        DO L=1,LMH
          IF(PK(L).LE.PTPK)LTOP=L+1
        ENDDO
!
        PTPK=PK(LTOP)
!***
!***  HIGHEST LEVEL ALLOWED IS LEVEL JUST BELOW PSHU
!***
        IF(PTPK.LE.PSHU)THEN
!
          DO L=1,LMH
            IF(PK(L).LE.PSHU)LSHU=L+1
          ENDDO
!
          LTOP=LSHU
          PTPK=PK(LTOP)
        ENDIF
!
        IF(LTOP.GE.LBOT)THEN
          LBOT=LMH
          LTOP=LMH
          PBOT=PK(LBOT)
          PTOP=PK(LTOP)
          GO TO 600
        ENDIF
!
        LTP1=LTOP+1
        LTP2=LTOP+2
!
        DO L=LTOP,LBOT
          QSATK(L)=PQ0/PK(L)*EXP(A2*(TK(L)-A3)/(TK(L)-A4))
        ENDDO
!
        RHH=QK(LTOP)/QSATK(LTOP)
        RHMAX=0.
!
        DO L=LTP1,LBM1
          RHL=QK(L)/QSATK(L)
          DRHDP=(RHH-RHL)/(PK(L-1)-PK(L))
!
          IF(DRHDP.GT.RHMAX)THEN
            LTSH=L-1
            RHMAX=DRHDP
          ENDIF
!
          RHH=RHL
        ENDDO
!
        LTOP=LTSH
!***
!***  CLOUD MUST BE AT LEAST TWO LAYERS THICK
!***
        IF(LBOT-LTOP.LT.2)LTOP=LBOT-2
!
        PTOP=PK(LTOP)
        GO TO 600
!
      ENDIF
!----------------------------------------------------------------------
!------------------ DEEP CONVECTION OTHERWISE -------------------------
!----------------------------------------------------------------------
!
      IF(EFI.GT.1.)EFI=1.
      IF(PRECK.EQ.0.)EFI=1.
      CLDEFI=EFI
!
      FEFI=EFMNT+SLOPE*(EFI-EFIMN)
!     FEFI=AMAX1(EFI,EFMNT)
!
      PRECK=PRECK*FEFI
!***
!***  PRECIPITATION, TEMPERATURE & MOISTURE CHANGES
!***
      PCPCOL=PRECK*CPRLG
!
      DO L=LTOP,LB
        DTDT(L)=DIFT(L)*FEFI*RDTCNVC
        DQDT(L)=DIFQ(L)*FEFI*RDTCNVC
      ENDDO
!
!DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
!DCDCDCDCDCDC          END OF DEEP CONVECTION            DCDCDCDCDCDCD
!DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCD
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
  600 CONTINUE
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!---------------GATHER SHALLOW CONVECTION POINTS-----------------------
!
      IF(PTOP.LE.PBOT-PNO.AND.LTOP.LE.LBOT-2)THEN
        DEPMIN=PSH*PSFC/101300.
!
        IF(LPBL.LT.LBOT)THEN
          LBOT=LPBL
        ENDIF
        IF(LBOT.GT.LMH-1)LBOT=LMH-1
!
        IF(LTOP.LE.LBOT-2.AND.PTOP+1..GE.PBOT-DEPMIN)THEN
          SHALLOW=.TRUE.
        ENDIF
!
      ENDIF
!
!**********************************************************************
      IF(.NOT.SHALLOW)GO TO 800
!**********************************************************************
!---------------------------------------------------------------------
!SCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
!SCSCSCSCSCSC         SHALLOW CONVECTION          CSCSCSCSCSCSCSCSCSCS
!SCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
!---------------------------------------------------------------------
      DO L=1,LMH
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
      ENDDO
!-------------------------SHALLOW CLOUD TOP---------------------------
      LBM1=LBOT-1
      PTPK=PTOP
      LTP1=LTOP-1
!---------------------------------------------------------------------
      IF(PTOP.GT.PBOT-PNO.OR.LTOP.GT.LBOT-2)THEN
        LBOT=0
        LTOP=LBOT
        PTOP=PBOT
        GO TO 800
      ENDIF
!------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX AT TOP-------
!
      THTPK=T(LTP1)*APE(LTP1)
!
      TTHK=(THTPK-THL)*RDTH
      QQK =TTHK-AINT(TTHK)
      IT  =INT(TTHK)+1
!
      IF(IT.LT.1)THEN
        IT=1
        QQK=0.
      ENDIF
!
      IF(IT.GE.JTB)THEN
        IT=JTB-1
        QQK=0.
      ENDIF
!------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY AT TOP--------
      BQS00K=QS0(IT)
      SQS00K=SQS(IT)
      BQS10K=QS0(IT+1)
      SQS10K=SQS(IT+1)
!------------SCALING SPEC. HUMIDITY & TABLE INDEX AT TOP--------------
      BQK=(BQS10K-BQS00K)*QQK+BQS00K
      SQK=(SQS10K-SQS00K)*QQK+SQS00K
!
!     TQK=(Q(LTOP)-BQK)/SQK*RDQ
      TQK=(Q(LTP1)-BQK)/SQK*RDQ
!
      PPK=TQK-AINT(TQK)
      IQ =INT(TQK)+1
!
      IF(IQ.LT.1)THEN
        IQ=1
        PPK=0.
      ENDIF
!
      IF(IQ.GE.ITB)THEN
        IQ=ITB-1
        PPK=0.
      ENDIF
!--------------CLOUD TOP SATURATION POINT PRESSURE--------------------
      PART1=(PTBL(IQ+1,IT)-PTBL(IQ,IT))*PPK
      PART2=(PTBL(IQ,IT+1)-PTBL(IQ,IT))*QQK
      PART3=(PTBL(IQ  ,IT  )-PTBL(IQ+1,IT  )                          &
            -PTBL(IQ  ,IT+1)+PTBL(IQ+1,IT+1))*PPK*QQK
      PTPK=PTBL(IQ,IT)+PART1+PART2+PART3
!---------------------------------------------------------------------
      DPMIX=PTPK-PSP
      IF(ABS(DPMIX).LT.3000.)DPMIX=-3000.
!--------------TEMPERATURE PROFILE SLOPE------------------------------
      SMIX=(THTPK-THBT)/DPMIX*STABS
!
      TREFKX=TREFK(LBOT+1)
      PKXXXX=PK(LBOT+1)
      PKXXXY=PK(LBOT)
      APEKXX=APEK(LBOT+1)
      APEKXY=APEK(LBOT)
!
      DO L=LBOT,LTOP,-1
        TREFKX=((PKXXXY-PKXXXX)*SMIX                                  &
                +TREFKX*APEKXX)/APEKXY
        TREFK(L)=TREFKX
        APEKXX=APEKXY
        PKXXXX=PKXXXY
        APEKXY=APEK(L-1)
        PKXXXY=PK(L-1)
      ENDDO
!--------------TEMPERATURE REFERENCE PROFILE CORRECTION---------------
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
!--------------HUMIDITY PROFILE EQUATIONS-----------------------------
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
        DST   =(TREFK(L)-TK(L))*RTBAR*DPRS(L)+DST
      ENDDO
!
      PSUM  =PSUM*RDPSUM
      QSUM  =QSUM*RDPSUM
      ROTSUM=1./OTSUM
      POTSUM=POTSUM*ROTSUM
      QOTSUM=QOTSUM*ROTSUM
      DST   =DST*ROTSUM*CP/ELWV
!--------------ENSURE POSITIVE ENTROPY CHANGE-------------------------
      IF(DST.GT.0.)THEN
!
!       DSTQ=DST*EPSUP
        LBOT=0
        LTOP=LBOT
        PTOP=PBOT
        GO TO 800
!
      ELSE
        DSTQ=DST*EPSDN
      ENDIF
!--------------CHECK FOR ISOTHERMAL ATMOSPHERE------------------------
      DEN=POTSUM-PSUM
!
      IF(-DEN/PSUM.LT.5.E-5)THEN
        LBOT=0
        LTOP=LBOT
        PTOP=PBOT
        GO TO 800
!
!--------------SLOPE OF THE REFERENCE HUMIDITY PROFILE----------------
      ELSE
        DQREF=(QOTSUM-DSTQ-QSUM)/DEN
      ENDIF
!------------ HUMIDITY DOES NOT INCREASE WITH HEIGHT------------------
      IF(DQREF.LT.0.)THEN
        LBOT=0
        LTOP=LBOT
        PTOP=PBOT
        GO TO 800
      ENDIF
!--------------HUMIDITY AT THE CLOUD TOP------------------------------
      QRFTP=QSUM-DQREF*PSUM
!--------------HUMIDITY PROFILE---------------------------------------
!
      DO L=LTOP,LBOT
        QRFKL=(FPK(L)-FPTK)*DQREF+QRFTP
!
!***  SUPERSATURATION OR NEGATIVE Q NOT ALLOWED
!
        QNEW=(QRFKL-QK(L))*TAUK+QK(L)
!
        IF(QNEW.LT.QSATK(L)*RHLSC.OR.QNEW.GT.QSATK(L)*RHHSC)THEN
          LBOT=0
          LTOP=LBOT
          PTOP=PBOT
          GO TO 800
        ENDIF
!
        THVREF(L)=TREFK(L)*APEK(L)*(QRFKL*D608+1.)
        QREFK(L)=QRFKL
      ENDDO
!
!---------------- ELIMINATE CLOUDS WITH BOTTOMS TOO DRY --------------
!
      RHNEW=((QREFK(LBOT)-QK(LBOT))*TAUK+QK(LBOT))/QSATK(LBOT)
!
      IF(RHNEW.LT.QK(LBOT+1)/QSATK(LBOT+1)*STRESH)THEN
        LBOT=0
        LTOP=LBOT
        PTOP=PBOT
        GO TO 800
      ENDIF
!
!------------ ELIMINATE IMPOSSIBLE SLOPES (BETTS,DTHETA/DQ)------------
!
      DO L=LTOP,LBOT
        DTDP=(THVREF(L-1)-THVREF(L))/(PRSMID(L)-PRSMID(L-1))
!
        IF(DTDP.LT.EPSDT)THEN
          LBOT=0
          LTOP=LBOT
          PTOP=PBOT
          GO TO 800
        ENDIF
!
      ENDDO
!---------------------------------------------------------------------
      DENTPY=0.
!
      DO L=LTOP,LBOT
        DENTPY=((TREFK(L)-TK(L))*CP+(QREFK(L)-QK(L))*ELWV)            &
               /(TK(L)+TREFK(L))*DPRS(L)+DENTPY
      ENDDO
!
!---------------------------------------------------------------------
!------------RELAXATION TOWARD REFERENCE PROFILES---------------------
!---------------------------------------------------------------------
!
      DO L=LTOP,LBOT
        DTDT(L)=(TREFK(L)-TK(L))*TAUK*RDTCNVC
        DQDT(L)=(QREFK(L)-QK(L))*TAUK*RDTCNVC
      ENDDO

      HTOP=MIN(FLOAT(LTOP),HTOP)
      HBOT=MAX(FLOAT(LBOT),HBOT)

!---------------------------------------------------------------------
!SCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
!SCSCSCSCSCSC         END OF SHALLOW CONVECTION        SCSCSCSCSCSCSCS
!SCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCSCS
!---------------------------------------------------------------------
  800 CONTINUE
!---------------------------------------------------------------------
      END SUBROUTINE BMJ
!---------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------------------------------------------
                           SUBROUTINE TTBLEX                          &
       (ITBX,JTBX,PLX,PRSMID,RDPX,RDTHEX,STHE                         &
       ,THE0,THESP,TTBL                                               &
       ,TREF)
!---------------------------------------------------------------------
!     ****************************************************************
!     *                                                              *
!     *         EXTRACT TEMPERATURE OF THE MOIST ADIABAT FROM        *
!     *                    THE APPROPRIATE TTBL                      *
!     *                                                              *
!     ****************************************************************
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
      INTEGER,INTENT(IN) :: ITBX,JTBX
!
      REAL,INTENT(IN) :: PLX,PRSMID,RDPX,RDTHEX,THESP
!
      REAL,DIMENSION(ITBX),INTENT(IN) :: STHE,THE0
!
      REAL,DIMENSION(JTBX,ITBX),INTENT(IN) :: TTBL
!
      REAL,INTENT(OUT) :: TREF
!---------------------------------------------------------------------
      REAL :: BTHE00K,BTHE10K,BTHK,PK,PP,QQ,STHE00K,STHE10K,STHK      &
             ,T00K,T01K,T10K,T11K,TPK,TTHK
!
      INTEGER :: IPTB,ITHTB
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!--------------SCALING PRESSURE & TT TABLE INDEX----------------------
!---------------------------------------------------------------------
      PK=PRSMID
      TPK=(PK-PLX)*RDPX
      QQ=TPK-AINT(TPK)
      IPTB=INT(TPK)+1
!--------------KEEPING INDICES WITHIN THE TABLE-----------------------
      IF(IPTB.LT.1)THEN
        IPTB=1
        QQ=0.
      ENDIF
!
      IF(IPTB.GE.ITBX)THEN
        IPTB=ITBX-1
        QQ=0.
      ENDIF
!--------------BASE AND SCALING FACTOR FOR THETAE---------------------
      BTHE00K=THE0(IPTB)
      STHE00K=STHE(IPTB)
      BTHE10K=THE0(IPTB+1)
      STHE10K=STHE(IPTB+1)
!--------------SCALING THE & TT TABLE INDEX---------------------------
      BTHK=(BTHE10K-BTHE00K)*QQ+BTHE00K
      STHK=(STHE10K-STHE00K)*QQ+STHE00K
      TTHK=(THESP-BTHK)/STHK*RDTHEX
      PP=TTHK-AINT(TTHK)
      ITHTB=INT(TTHK)+1
!--------------KEEPING INDICES WITHIN THE TABLE-----------------------
      IF(ITHTB.LT.1)THEN
        ITHTB=1
        PP=0.
      ENDIF
!
      IF(ITHTB.GE.JTBX)THEN
        ITHTB=JTBX-1
        PP=0.
      ENDIF
!--------------TEMPERATURE AT FOUR SURROUNDING TT TABLE PTS.----------
      T00K=TTBL(ITHTB,IPTB)
      T10K=TTBL(ITHTB+1,IPTB)
      T01K=TTBL(ITHTB,IPTB+1)
      T11K=TTBL(ITHTB+1,IPTB+1)
!---------------------------------------------------------------------
!--------------PARCEL TEMPERATURE-------------------------------------
!---------------------------------------------------------------------
      TREF=(T00K+(T10K-T00K)*PP+(T01K-T00K)*QQ                        &
          +(T00K-T10K-T01K+T11K)*PP*QQ)
!---------------------------------------------------------------------
      END SUBROUTINE TTBLEX
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      SUBROUTINE BMJINIT(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN          &
                        ,CLDEFI,LOWLYR,CP,RD,RESTART                  &
                        ,IDS,IDE,JDS,JDE,KDS,KDE                      &
                        ,IMS,IME,JMS,JME,KMS,KME                      &
                        ,ITS,ITE,JTS,JTE,KTS,KTE)
!---------------------------------------------------------------------   
      IMPLICIT NONE
!---------------------------------------------------------------------
      LOGICAL , INTENT(IN) :: RESTART
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                   &
                           ,IMS,IME,JMS,JME,KMS,KME                   &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,INTENT(IN) :: CP,RD
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) ::                   &
                                                    RTHCUTEN          &
                                                   ,RQVCUTEN          &
                                                   ,RQCCUTEN          &
                                                   ,RQRCUTEN
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: CLDEFI

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: LOWLYR
!
      REAL,PARAMETER :: ELIWV=2.683E6,EPS=1.E-9
!
      REAL, DIMENSION(JTB) :: APP,APT,AQP,AQT,PNEW,POLD,QSNEW,QSOLD   &
                             ,THENEW,THEOLD,TNEW,TOLD,Y2P,Y2T
!
      REAL,DIMENSION(JTBQ) :: APTQ,AQTQ,THENEWQ,THEOLDQ               &
                             ,TNEWQ,TOLDQ,Y2TQ
!
      INTEGER :: I,J,K,ITF,JTF,KTF
      INTEGER :: KTH,KTHM,KTHM1,KP,KPM,KPM1
!
      REAL :: APE,DP,DQS,DTH,DTHE,P,QS,QS0K,SQSK,STHEK                &
             ,TH,THE0K
!---------------------------------------------------------------------
 
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
        CLDEFI(I,J)=1.
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
!---------------------------------------------------------------------
!--------------COARSE LOOK-UP TABLE FOR SATURATION POINT--------------
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
      DO 100 KTH=1,KTHM
!
      TH=TH+DTH
      P=PL-DP
!
      DO KP=1,KPM
        P=P+DP
        APE=(100000./P)**(RD/CP)
        QSOLD(KP)=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
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
      SQS(KTH)=SQSK
!---------------------------------------------------------------------
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
      ENDDO
!---------------------------------------------------------------------
  100 CONTINUE
!---------------------------------------------------------------------
!------------COARSE LOOK-UP TABLE FOR T(P) FROM CONSTANT THE----------
!---------------------------------------------------------------------
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
        QS=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
        TOLD(KTH)=TH/APE
        THEOLD(KTH)=TH*EXP(ELIWV*QS/(CP*TOLD(KTH)))
      ENDDO
!
      THE0K=THEOLD(1)
      STHEK=THEOLD(KTHM)-THEOLD(1)
      THEOLD(1   )=0.
      THEOLD(KTHM)=1.
!
      DO KTH=2,KTHM1
        THEOLD(KTH)=(THEOLD(KTH)-THE0K)/STHEK
        IF((THEOLD(KTH)-THEOLD(KTH-1)).LT.EPS)                        &
            THEOLD(KTH)=THEOLD(KTH-1)  +  EPS
      ENDDO
!
      THE0(KP)=THE0K
      STHE(KP)=STHEK
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
  200 CONTINUE
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
!-------------FINE LOOK-UP TABLE FOR SATURATION POINT-----------------
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
!-------------FINE LOOK-UP TABLE FOR T(P) FROM CONSTANT THE-----------
!---------------------------------------------------------------------
      DO 300 KP=1,KPM
!
      P=P+DP
      TH=THL-DTH
!
      DO KTH=1,KTHM
        TH=TH+DTH
        APE=(1.E5/P)**(RD/CP)
        QS=PQ0/P*EXP(A2*(TH-A3*APE)/(TH-A4*APE))
        TOLDQ(KTH)=TH/APE
        THEOLDQ(KTH)=TH*EXP(ELIWV*QS/(CP*TOLDQ(KTH)))
      ENDDO
!
      THE0K=THEOLDQ(1)
      STHEK=THEOLDQ(KTHM)-THEOLDQ(1)
      THEOLDQ(1   )=0.
      THEOLDQ(KTHM)=1.
!
      DO KTH=2,KTHM1
        THEOLDQ(KTH)=(THEOLDQ(KTH)-THE0K)/STHEK
        IF((THEOLDQ(KTH)-THEOLDQ(KTH-1)).LT.EPS)                      &
            THEOLDQ(KTH)=THEOLDQ(KTH-1)  +  EPS
      ENDDO
!
      THE0Q(KP)=THE0K
      STHEQ(KP)=STHEK
!---------------------------------------------------------------------
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
      CALL SPLINE(JTBQ,KTHM,THEOLDQ,TOLDQ,Y2TQ,KTHM                   &     
                 ,THENEWQ,TNEWQ,APTQ,AQTQ)
!
      DO KTH=1,KTHM
        TTBLQ(KTH,KP)=TNEWQ(KTH)
      ENDDO
!---------------------------------------------------------------------
  300 CONTINUE
!---------------------------------------------------------------------
      END SUBROUTINE BMJINIT
!---------------------------------------------------------------------
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!---------------------------------------------------------------------
      SUBROUTINE SPLINE(JTBX,NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)
!   ******************************************************************
!   *                                                                *
!   *  THIS IS A ONE-DIMENSIONAL CUBIC SPLINE FITTING ROUTINE        *
!   *  PROGRAMED FOR A SMALL SCALAR MACHINE.                         *
!   *                                                                *
!   *  PROGRAMER Z. JANJIC                                           *
!   *                                                                *
!   *  NOLD - NUMBER OF GIVEN VALUES OF THE FUNCTION.  MUST BE GE 3. *
!   *  XOLD - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE     *
!   *         FUNCTION ARE GIVEN.  MUST BE IN ASCENDING ORDER.       *
!   *  YOLD - THE GIVEN VALUES OF THE FUNCTION AT THE POINTS XOLD.   *
!   *  Y2   - THE SECOND DERIVATIVES AT THE POINTS XOLD.  IF NATURAL *
!   *         SPLINE IS FITTED Y2(1)=0. AND Y2(NOLD)=0. MUST BE      *
!   *         SPECIFIED.                                             *
!   *  NNEW - NUMBER OF VALUES OF THE FUNCTION TO BE CALCULATED.     *
!   *  XNEW - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE     *
!   *         FUNCTION ARE CALCULATED.  XNEW(K) MUST BE GE XOLD(1)   *
!   *         AND LE XOLD(NOLD).                                     *
!   *  YNEW - THE VALUES OF THE FUNCTION TO BE CALCULATED.           *
!   *  P, Q - AUXILIARY VECTORS OF THE LENGTH NOLD-2.                *
!   *                                                                *
!   ******************************************************************
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
      INTEGER,INTENT(IN) :: JTBX,NNEW,NOLD
      REAL,DIMENSION(JTBX),INTENT(IN) :: XNEW,XOLD,YOLD
      REAL,DIMENSION(JTBX),INTENT(INOUT) :: P,Q,Y2
      REAL,DIMENSION(JTBX),INTENT(OUT) :: YNEW
!
      INTEGER :: K,K1,K2,KOLD,NOLDM1
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                 &
             ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1
!---------------------------------------------------------------------
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
      IF(NOLD.EQ.3)GO TO 150
!---------------------------------------------------------------------
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
      IF(K.LT.NOLD)GO TO 100
!-----------------------------------------------------------------------
  150 K=NOLDM1
!
  200 Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)
!
      K=K-1
      IF(K.GT.1)GO TO 200
!-----------------------------------------------------------------------
      K1=1
!
  300 XK=XNEW(K1)
!
      DO 400 K2=2,NOLD
!
      IF(XOLD(K2).GT.XK)THEN
        KOLD=K2-1
        GO TO 450
      ENDIF
!
  400 CONTINUE
!
      YNEW(K1)=YOLD(NOLD)
      GO TO 600
!
  450 IF(K1.EQ.1)GO TO 500
      IF(K.EQ.KOLD)GO TO 550
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
      IF(K1.LE.NNEW)GO TO 300
!-----------------------------------------------------------------------
      END SUBROUTINE SPLINE
!---------------------------------------------------------------------
!
      END MODULE MODULE_CU_BMJ

