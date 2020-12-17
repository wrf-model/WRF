!WRF:MODEL_LAYER:PHYSICS
!

MODULE module_cu_kf

   USE module_wrf_error

   REAL    , PARAMETER :: RAD          = 1500.

CONTAINS

!-------------------------------------------------------------
   SUBROUTINE KFCPS(                                         &
               ids,ide, jds,jde, kds,kde                     &
              ,ims,ime, jms,jme, kms,kme                     &
              ,its,ite, jts,jte, kts,kte                     &
              ,DT,KTAU,DX,CUDT,ADAPT_STEP_FLAG               &
              ,rho                                           &
              ,RAINCV,PRATEC,NCA                             &
              ,U,V,TH,T,W,QV,dz8w,Pcps,pi                    &
              ,W0AVG,XLV0,XLV1,XLS0,XLS1,CP,R,G,EP1          &
              ,EP2,SVP1,SVP2,SVP3,SVPT0                      &
              ,STEPCU,CU_ACT_FLAG,warm_rain                  &
            ! optional arguments
              ,F_QV    ,F_QC    ,F_QR    ,F_QI    ,F_QS      &
              ,RQVCUTEN,RQCCUTEN,RQRCUTEN,RQICUTEN,RQSCUTEN  &
              ,RTHCUTEN                                      &
!kf_edrates
              ,UDR_KF,DDR_KF                                 &
              ,UER_KF,DER_KF                                 &
              ,TIMEC_KF,KF_EDRATES                           &
                                                             )
!-------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) ::                            &
                                  ids,ide, jds,jde, kds,kde, & 
                                  ims,ime, jms,jme, kms,kme, & 
                                  its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   ) :: STEPCU
   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV0,XLV1,XLS0,XLS1
   REAL,         INTENT(IN   ) :: CP,R,G,EP1,EP2
   REAL,         INTENT(IN   ) :: SVP1,SVP2,SVP3,SVPT0

   INTEGER,      INTENT(IN   ) :: KTAU                   

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(IN   ) ::                                   &
                                                          U, &
                                                          V, &
                                                          W, &
                                                         TH, &
                                                         QV, &
                                                          T, &
                                                       dz8w, &
                                                       Pcps, &
                                                        rho, &
                                                         pi
!
   REAL,  INTENT(IN   ) :: DT, DX
   REAL,  INTENT(IN   ) :: CUDT
   LOGICAL,OPTIONAL, INTENT(IN  ) :: ADAPT_STEP_FLAG

   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(INOUT) ::                                   &
                                                     RAINCV  &
                                                    ,PRATEC  &
                                                    ,   NCA

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),           &
          INTENT(INOUT) ::                                   &
                                                      W0AVG

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                  &
          INTENT(INOUT) :: CU_ACT_FLAG

!
! Optional arguments
!

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),           &
         OPTIONAL,                                           &
         INTENT(INOUT) ::                                    &
                                                   RTHCUTEN  &
                                                  ,RQVCUTEN  &
                                                  ,RQCCUTEN  &
                                                  ,RQRCUTEN  &
                                                  ,RQICUTEN  &
                                                  ,RQSCUTEN

!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in 
! use or not. 
!

   LOGICAL, OPTIONAL ::                                      &
                                                   F_QV      &
                                                  ,F_QC      &
                                                  ,F_QR      &
                                                  ,F_QI      &
                                                  ,F_QS

!kf_edrates
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                         &
                                                     UDR_KF, &
                                                     DDR_KF, &
                                                     UER_KF, &
                                                     DER_KF

   REAL,  DIMENSION( ims:ime , jms:jme )                   , &
          INTENT(INOUT) ::                         &
                                                   TIMEC_KF

   INTEGER, INTENT(IN) ::              KF_EDRATES

! LOCAL VARS

   REAL, DIMENSION( kts:kte ) ::                             &
                                                        U1D, &
                                                        V1D, &
                                                        T1D, &
                                                       DZ1D, &
                                                       QV1D, &
                                                        P1D, &
                                                      RHO1D, &
                                                    W0AVG1D

   REAL, DIMENSION( kts:kte )::                              &
                                                       DQDT, &
                                                      DQIDT, &
                                                      DQCDT, &
                                                      DQRDT, &
                                                      DQSDT, &
                                                       DTDT

   REAL    ::         TST,tv,PRS,RHOE,W0,SCR1,DXSQ,tmp

   INTEGER :: i,j,k,NTST,ICLDCK

   LOGICAL :: qi_flag , qs_flag
! adjustable time step changes
   REAL    :: lastdt = -1.0
   REAL    :: W0AVGfctr, W0fctr, W0den

!----------------------------------------------------------------------

!--- CALL CUMULUS PARAMETERIZATION                                        
!                                                                        
!...TST IS THE NUMBER OF TIME STEPS IN 10 MINUTES...W0AVG IS CLOSE TO A    
!...RUNNING MEAN VERTICAL VELOCITY...NOTE THAT IF YOU CHANGE TST, IT WIL  
!...CHANGE THE FREQUENCY OF THE CONVECTIVE INTITIATION CHECK (SEE BELOW) 
!...NOTE THAT THE ORDERING OF VERTICAL LAYERS MUST BE REVERSED FOR W0AVG
!...BECAUSE THE ORDERING IS REVERSED IN KFPARA...                         
!                                                                           
   DXSQ=DX*DX
   qi_flag = .FALSE.
   qs_flag = .FALSE.
   IF ( PRESENT( F_QI ) ) qi_flag = f_qi
   IF ( PRESENT( F_QS ) ) qs_flag = f_qs

!----------------------
   NTST=STEPCU
   TST=float(NTST*2)                                                         
!----------------------
!  NTST=NINT(1200./(DT*2.))                                                 
!  TST=float(NTST)                                                         
!  NTST=NINT(0.5*TST)                                                   
!  NTST=MAX0(NTST,1)                                                   
!----------------------
!  ICLDCK=MOD(KTAU,NTST)                                              
!----------------------
!  write(0,*) 'DT = ',DT,'  KTAU = ',KTAU,' DX = ',DX
!  write(0,*) 'CUDT = ',CUDT
!  write(0,*) 'ADAPT_STEP_FLAG = ',ADAPT_STEP_FLAG,' IDS = ',IDS
!  write(0,*) 'STEPCU = ',STEPCU,' warm_rain = ',warm_rain
!  write(0,*) 'F_QV = ',F_QV,' F_QC = ',F_QV
!  write(0,*) 'F_QI = ',F_QI,' F_QS = ',F_QS
!  write(0,*) 'F_QR = ',F_QR
!  stop
   if (lastdt < 0) then
      lastdt = dt
   endif

   if (ADAPT_STEP_FLAG) then
      W0AVGfctr = 2 * MAX(CUDT*60,dt) - dt
      W0fctr = dt
      W0den = 2 * MAX(CUDT*60,dt)
   else
      W0AVGfctr = (TST-1.)
      W0fctr = 1.
      W0den = TST
   endif

   DO J = jts,jte  
      DO K=kts,kte
         DO I= its,ite
!           SCR1=-5.0E-4*G*rho(I,K,J)*(w(I,K,J)+w(I,K+1,J))
!           TV=T(I,K,J)*(1.+EP1*QV(I,K,J))
!           RHOE=Pcps(I,K,J)/(R*TV)
!           W0=-101.9368*SCR1/RHOE
            W0=0.5*(w(I,K,J)+w(I,K+1,J))

!           Old:
!
!           W0AVG(I,K,J)=(W0AVG(I,K,J)*(TST-1.)+W0)/TST
!           New, to support adaptive time step:
!
            W0AVG(I,K,J) = ( W0AVG(I,K,J) * W0AVGfctr + W0 * W0fctr ) / W0den

         ENDDO
      ENDDO
   ENDDO
   lastdt = dt

     DO J = jts,jte  
     DO I= its,ite
        CU_ACT_FLAG(i,j) = .true.
     ENDDO
     ENDDO

     DO J = jts,jte  
        DO I=its,ite
! if (i.eq. 110 .and. j .eq. 59 ) then
!   write(0,*) 'nca = ',nca(i,j),' CU_ACT_FLAG = ',CU_ACT_FLAG(i,j)
!   write(0,*) 'dt = ',dt,' ADAPT_STEP_FLAG = ',ADAPT_STEP_FLAG
! endif
!        IF ( NINT(NCA(I,J)) .gt. 0 ) then
         IF ( NCA(I,J) .gt. 0.5*DT ) then
            CU_ACT_FLAG(i,j) = .false. 
         ELSE

            DO k=kts,kte
               DQDT(k)=0.
               DQIDT(k)=0.      
               DQCDT(k)=0.      
               DQRDT(k)=0.      
               DQSDT(k)=0.      
               DTDT(k)=0.
            ENDDO
!kf_edrates
            IF (KF_EDRATES == 1) THEN
               DO k=kts,kte
                  UDR_KF(I,k,J)=0.
                  DDR_KF(I,k,J)=0.
                  UER_KF(I,k,J)=0.
                  DER_KF(I,k,J)=0.
               ENDDO
               TIMEC_KF(I,J)=0.
            ENDIF
            RAINCV(I,J)=0.
	    PRATEC(I,J)=0.
!
! assign vars from 3D to 1D

            DO K=kts,kte
               U1D(K) =U(I,K,J)
               V1D(K) =V(I,K,J)
               T1D(K) =T(I,K,J)
               RHO1D(K) =rho(I,K,J)
               QV1D(K)=QV(I,K,J)
               P1D(K) =Pcps(I,K,J)
               W0AVG1D(K) =W0AVG(I,K,J)
               DZ1D(k)=dz8w(I,K,J)
            ENDDO

!
            CALL KFPARA(I, J,                       &
                 U1D,V1D,T1D,QV1D,P1D,DZ1D,         &
                 W0AVG1D,DT,DX,DXSQ,RHO1D,          &
                 XLV0,XLV1,XLS0,XLS1,CP,R,G,        &
                 EP2,SVP1,SVP2,SVP3,SVPT0,          &
                 DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT, &
                 RAINCV,PRATEC,NCA,                 &
                 warm_rain,qi_flag,qs_flag,         &
                 ids,ide, jds,jde, kds,kde,         &        
                 ims,ime, jms,jme, kms,kme,         &
                 its,ite, jts,jte, kts,kte,         &
!kf_edrates
                 UDR_KF,DDR_KF,                     &
                 UER_KF,DER_KF,                     &
                 TIMEC_KF,KF_EDRATES                )
 
            IF ( PRESENT( RTHCUTEN ) .AND. PRESENT( RQVCUTEN ) ) THEN
              DO K=kts,kte
                 RTHCUTEN(I,K,J)=DTDT(K)/pi(I,K,J)
                 RQVCUTEN(I,K,J)=DQDT(K)
              ENDDO
            ENDIF

            IF( PRESENT(RQRCUTEN) .AND. PRESENT(RQCCUTEN) .AND. &
                PRESENT(F_QR)                                    ) THEN
              IF ( F_QR            ) THEN
                 DO K=kts,kte
                    RQRCUTEN(I,K,J)=DQRDT(K)
                    RQCCUTEN(I,K,J)=DQCDT(K)
                 ENDDO
               ELSE
! This is the case for Eta microphysics without 3d rain field
                 DO K=kts,kte
                    RQRCUTEN(I,K,J)=0.
                    RQCCUTEN(I,K,J)=DQRDT(K)+DQCDT(K)
                 ENDDO
              ENDIF
            ENDIF

!......     QSTEN STORES GRAUPEL TENDENCY IF IT EXISTS, OTHERISE SNOW (V2)     

            IF( PRESENT( RQICUTEN ) .AND. qi_flag )THEN
              DO K=kts,kte
                 RQICUTEN(I,K,J)=DQIDT(K)
              ENDDO
            ENDIF

            IF( PRESENT ( RQSCUTEN ) .AND. qs_flag )THEN
              DO K=kts,kte
                 RQSCUTEN(I,K,J)=DQSDT(K)
              ENDDO
            ENDIF                                                         
!
         ENDIF                                                         
       ENDDO
     ENDDO

   END SUBROUTINE KFCPS
   
!-----------------------------------------------------------
   SUBROUTINE KFPARA (I, J,                                &
                      U0,V0,T0,QV0,P0,DZQ,W0AVG1D,         &
                      DT,DX,DXSQ,rho,                      &
                      XLV0,XLV1,XLS0,XLS1,CP,R,G,          &
                      EP2,SVP1,SVP2,SVP3,SVPT0,            &
                      DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT,   &
                      RAINCV,PRATEC,NCA,                   &
                      warm_rain,qi_flag,qs_flag,           &
                      ids,ide, jds,jde, kds,kde,           & 
                      ims,ime, jms,jme, kms,kme,           &
                      its,ite, jts,jte, kts,kte,           &
!kf_edrates
                      UDR_KF,DDR_KF,                       &
                      UER_KF,DER_KF,                       &
                      TIMEC_KF,KF_EDRATES                  )
!-----------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------
      INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde, &
                                ims,ime, jms,jme, kms,kme, &
                                its,ite, jts,jte, kts,kte, &
                                I,J
      LOGICAL, INTENT(IN   ) :: warm_rain
      LOGICAL           :: qi_flag, qs_flag

!
      REAL, DIMENSION( kts:kte ),                          &
            INTENT(IN   ) ::                           U0, &
                                                       V0, &
                                                       T0, &
                                                      QV0, &
                                                       P0, &
                                                      rho, &
                                                      DZQ, &
                                                  W0AVG1D
!
      REAL,  INTENT(IN   ) :: DT,DX,DXSQ
!

      REAL,  INTENT(IN   ) :: XLV0,XLV1,XLS0,XLS1,CP,R,G
      REAL,  INTENT(IN   ) :: EP2,SVP1,SVP2,SVP3,SVPT0
!
      REAL, DIMENSION( kts:kte ), INTENT(INOUT) ::         &
                                                     DQDT, &
                                                    DQIDT, &
                                                    DQCDT, &
                                                    DQRDT, &
                                                    DQSDT, &
                                                     DTDT

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(INOUT) ::                       RAINCV, &
                                                   PRATEC, &
                                                      NCA

!kf_edrates
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),      &
            INTENT(INOUT) ::       UDR_KF,       &
                                             DDR_KF,       &
                                             UER_KF,       &
                                             DER_KF

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(INOUT) ::     TIMEC_KF

      INTEGER, INTENT(IN) ::         KF_EDRATES

!
!...DEFINE LOCAL VARIABLES...                                
!                                                            
      REAL, DIMENSION( kts:kte ) ::                        &
            Q0,Z0,TV0,TU,TVU,QU,TZ,TVD,                    &
            QD,QES,THTES,TG,TVG,QG,WU,WD,W0,EMS,EMSD,      &
            UMF,UER,UDR,DMF,DER,DDR,UMF2,UER2,             &
            UDR2,DMF2,DER2,DDR2,DZA,THTA0,THETEE,          &
            THTAU,THETEU,THTAD,THETED,QLIQ,QICE,           &
            QLQOUT,QICOUT,PPTLIQ,PPTICE,DETLQ,DETIC,       &
            DETLQ2,DETIC2,RATIO,RATIO2

      REAL, DIMENSION( kts:kte ) ::                        &
            DOMGDP,EXN,RHOE,TVQU,DP,RH,EQFRC,WSPD,         &
            QDT,FXM,THTAG,THTESG,THPA,THFXTOP,             &
            THFXBOT,QPA,QFXTOP,QFXBOT,QLPA,QLFXIN,         &
            QLFXOUT,QIPA,QIFXIN,QIFXOUT,QRPA,              &
            QRFXIN,QRFXOUT,QSPA,QSFXIN,QSFXOUT,            &
            QL0,QLG,QI0,QIG,QR0,QRG,QS0,QSG
                                         
      REAL, DIMENSION( kts:kte+1 ) :: OMG
      REAL, DIMENSION( kts:kte ) :: RAINFB,SNOWFB

! LOCAL VARS

      REAL    :: P00,T00,CV,B61,RLF,RHIC,RHBC,PIE,         &
                 TTFRZ,TBFRZ,C5,RATE
      REAL    :: GDRY,ROCP,ALIQ,BLIQ,                      & 
                 CLIQ,DLIQ,AICE,BICE,CICE,DICE
      REAL    :: FBFRC,P300,DPTHMX,THMIX,QMIX,ZMIX,PMIX,   &
                 ROCPQ,TMIX,EMIX,TLOG,TDPT,TLCL,TVLCL,     &
                 CPORQ,PLCL,ES,DLP,TENV,QENV,TVEN,TVBAR,   &
                 ZLCL,WKL,WABS,TRPPT,WSIGNE,DTLCL,GDT,WLCL,&
                 TVAVG,QESE,WTW,RHOLCL,AU0,VMFLCL,UPOLD,   &
                 UPNEW,ABE,WKLCL,THTUDL,TUDL,TTEMP,FRC1,   &
                 QNEWIC,RL,R1,QNWFRZ,EFFQ,BE,BOTERM,ENTERM,&
                 DZZ,WSQ,UDLBE,REI,EE2,UD2,TTMP,F1,F2,     &
                 THTTMP,QTMP,TMPLIQ,TMPICE,TU95,TU10,EE1,  &
                 UD1,CLDHGT,DPTT,QNEWLQ,DUMFDP,EE,TSAT,    &
                 THTA,P150,USR,VCONV,TIMEC,SHSIGN,VWS,PEF, &
                 CBH,RCBH,PEFCBH,PEFF,PEFF2,TDER,THTMIN,   &
                 DTMLTD,QS,TADVEC,DPDD,FRC,DPT,RDD,A1,     &
                 DSSDT,DTMP,T1RH,QSRH,PPTFLX,CPR,CNDTNF,   &
                 UPDINC,AINCM2,DEVDMF,PPR,RCED,DPPTDF,     &
                 DMFLFS,DMFLFS2,RCED2,DDINC,AINCMX,AINCM1, &
                 AINC,TDER2,PPTFL2,FABE,STAB,DTT,DTT1,     &
                 DTIME,TMA,TMB,TMM,BCOEFF,ACOEFF,QVDIFF,   &
                 TOPOMG,CPM,DQ,ABEG,DABE,DFDA,FRC2,DR,     &
                 UDFRC,TUC,QGS,RH0,RHG,QINIT,QFNL,ERR2,    &
                 RELERR,RLC,RLS,RNC,FABEOLD,AINCOLD,UEFRC, &
                 DDFRC,TDC,DEFRC         

      INTEGER :: KX,K,KL
!                                                    
      INTEGER :: ISTOP,ML,L5,L4,KMIX,LOW,                  &
                 LC,MXLAYR,LLFC,NLAYRS,NK,                 &
                 KPBL,KLCL,LCL,LET,IFLAG,                  &
                 KFRZ,NK1,LTOP,NJ,LTOP1,                   &
                 LTOPM1,LVF,KSTART,KMIN,LFS,               &
                 ND,NIC,LDB,LDT,ND1,NDK,                   &
                 NM,LMAX,NCOUNT,NOITR,                     &
                 NSTEP,NTC
!                                                    
      DATA P00,T00/1.E5,273.16/                     
      DATA CV,B61,RLF/717.,0.608,3.339E5/          
      DATA RHIC,RHBC/1.,0.90/                                    
      DATA PIE,TTFRZ,TBFRZ,C5/3.141592654,268.16,248.16,1.0723E-3/
      DATA RATE/0.01/                                           
!-----------------------------------------------------------
      GDRY=-G/CP                                               
      ROCP=R/CP
      KL=kte
      KX=kte
!
!     ALIQ = 613.3   
!     BLIQ = 17.502                                                   
!     CLIQ = 4780.8                                                  
!     DLIQ = 32.19                                                  
      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3
      AICE = 613.2  
      BICE = 22.452                                         
      CICE = 6133.0                                        
      DICE = 0.61                                         
!

!...OPTION TO FEED CONVECTIVELY GENERATED RAINWATER      
!...INTO GRID-RESOLVED RAINWATER (OR SNOW/GRAUPEL)      
!...FIELD.  'FBFRC' IS THE FRACTION OF AVAILABLE       
!...PRECIPITATION TO BE FED BACK (0.0 - 1.0)...      
!                                                   
      FBFRC=0.0                                    
!                                                                       
!...SCHEME IS CALLED ONCE  ON EACH NORTH-SOUTH SLICE, THE LOOP BELOW   
!...CHECKS FOR THE POSSIBILITY OF INITIATING PARAMETERIZED            
!...CONVECTION AT EACH POINT WITHIN THE SLICE                        
!                                                                   
!...SEE IF IT IS NECESSARY TO CHECK FOR CONVECTIVE TRIGGERING AT THIS 
!...GRID POINT. IF NCA>0, CONVECTION IS ALREADY ACTIVE AT THIS POINT,
!...JUST FEED BACK THE TENDENCIES SAVED FROM THE TIME WHEN CONVECTION  
!...WAS INITIATED.  IF NCA<0, CONVECTION IS NOT ACTIVE                
!...AND YOU MAY WANT TO CHECK TO SEE IF IT CAN BE ACTIVATED FOR THE  
!...CURRENT CONDITIONS.  IN PREVIOUS APLICATIONS OF THIS SCHEME,    
!...THE VARIABLE ICLDCK WAS USED BELOW TO SAVE TIME BY ONLY CHECKING   
!...FOR THE POSSIBILITY OF CONVECTIVE INITIATION EVERY 5 OR 10        
!...MINUTES...                                                       
!                                                                   

!  10 CONTINUE                                                 
!SUE  P300=1000.*(PSB(I,J)*A(KL)+PTOP-30.)+PP3D(I,J,KL)              

      P300=P0(1)-30000.
!                                                                     
!...PRESSURE PERTURBATION TERM IS ONLY DEFINED AT MID-POINT OF       
!...VERTICAL LAYERS...SINCE TOTAL PRESSURE IS NEEDED AT THE TOP AND 
!...BOTTOM OF LAYERS BELOW, DO AN INTERPOLATION...                       
!                                                                       
!...INPUT A VERTICAL SOUNDING ... NOTE THAT MODEL LAYERS ARE NUMBERED  
!...FROM BOTTOM-UP IN THE KF SCHEME...                                
!                                                                    
      ML=0                                                        
!SUE  tmprpsb=1./PSB(I,J)
!SUE  CELL=PTOP*tmprpsb

      DO 15 K=1,KX                                               
!SUE     P0(K)=1.E3*(A(NK)*PSB(I,J)+PTOP)+PP3D(I,J,NK)          
!                                                                        
!...IF Q0 IS ABOVE SATURATION VALUE, REDUCE IT TO SATURATION LEVEL...   
!                                                                      
         ES=ALIQ*EXP((BLIQ*T0(K)-CLIQ)/(T0(K)-DLIQ))                 
         QES(K)=EP2*ES/(P0(K)-ES)                                 
         Q0(K)=AMIN1(QES(K),QV0(K))                                 
         Q0(K)=AMAX1(0.000001,Q0(K))                                 
         QL0(K)=0.                                                
         QI0(K)=0.                                               
         QR0(K)=0.                                              
         QS0(K)=0.                                             

         TV0(K)=T0(K)*(1.+B61*Q0(K))                                 
         RHOE(K)=P0(K)/(R*TV0(K))                                  

         DP(K)=rho(k)*g*DZQ(k)
!                                                                         
!...DZQ IS DZ BETWEEN SIGMA SURFACES, DZA IS DZ BETWEEN MODEL HALF LEVEL 
!   DP IS THE PRESSURE INTERVAL BETWEEN FULL SIGMA LEVELS...            
!                                                                      
         IF(P0(K).GE.500E2)L5=K                                       
         IF(P0(K).GE.400E2)L4=K                                      
         IF(P0(K).GE.P300)LLFC=K                                    
         IF(T0(K).GT.T00)ML=K                                      
   15   CONTINUE                                                   

        Z0(1)=.5*DZQ(1)   
        DO 20 K=2,KL                                              
          Z0(K)=Z0(K-1)+.5*(DZQ(K)+DZQ(K-1))   
          DZA(K-1)=Z0(K)-Z0(K-1)                                 
   20   CONTINUE                                                       
        DZA(KL)=0.                                                    
        KMIX=1                                                       
   25   LOW=KMIX                                                    

        IF(LOW.GT.LLFC)GOTO 325                                    

        LC=LOW                                                    
        MXLAYR=0                                                 
!                                                                        
!...ASSUME THAT IN ORDER TO SUPPORT A DEEP UPDRAFT YOU NEED A LAYER OF 
!...UNSTABLE AIR 50 TO 100 mb DEEP...TO APPROXIMATE THIS, ISOLATE A      
!...GROUP OF ADJACENT INDIVIDUAL MODEL LAYERS, WITH THE BASE AT LEVEL   
!...LC, SUCH THAT THE COMBINED DEPTH OF THESE LAYERS IS AT LEAST 60 mb..  
!                                                                        
        NLAYRS=0                                                        
        DPTHMX=0.                                                      
        DO 63 NK=LC,KX                                                
          DPTHMX=DPTHMX+DP(NK)                                            
          NLAYRS=NLAYRS+1                                                
   63   IF(DPTHMX.GT.6.E3)GOTO 64                                       
        GOTO 325                                                       
   64   KPBL=LC+NLAYRS-1                                              
        KMIX=LC+1                                                        
   18   THMIX=0.                                                        
        QMIX=0.                                                        
        ZMIX=0.                                                       
        PMIX=0.                                                             
        DPTHMX=0.                                                          
!                                                                         
!...FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY              
!...MASS-WEIGHTING THE CHARACTERISTICS OF THE INDIVIDUAL MODEL         
!...LAYERS...                                                         
!                                                                    
        DO 17 NK=LC,KPBL                                            
          DPTHMX=DPTHMX+DP(NK)                                     
          ROCPQ=0.2854*(1.-0.28*Q0(NK))                           
          THMIX=THMIX+DP(NK)*T0(NK)*(P00/P0(NK))**ROCPQ          
          QMIX=QMIX+DP(NK)*Q0(NK)                               
          ZMIX=ZMIX+DP(NK)*Z0(NK)                              
   17   PMIX=PMIX+DP(NK)*P0(NK)                               
        THMIX=THMIX/DPTHMX                                   
        QMIX=QMIX/DPTHMX                                    
        ZMIX=ZMIX/DPTHMX                                   
        PMIX=PMIX/DPTHMX                                  
        ROCPQ=0.2854*(1.-0.28*QMIX)                               
        TMIX=THMIX*(PMIX/P00)**ROCPQ                             
        EMIX=QMIX*PMIX/(EP2+QMIX)                             
!                                                              
!...FIND THE TEMPERATURE OF THE MIXTURE AT ITS LCL, PRESSURE       
!...LEVEL OF LCL...                                               
!                                                                
        TLOG=ALOG(EMIX/ALIQ)                                    
        TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                             
        TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-  &
             TDPT)                                                    
        TLCL=AMIN1(TLCL,TMIX)                                       
        TVLCL=TLCL*(1.+0.608*QMIX)                                 
        CPORQ=1./ROCPQ                                            
        PLCL=P00*(TLCL/THMIX)**CPORQ                             
        DO 29 NK=LC,KL                                          
          KLCL=NK                                              
          IF(PLCL.GE.P0(NK))GOTO 35                           
   29   CONTINUE                                            
        GOTO 325                                                       
   35   K=KLCL-1                                                      
        DLP=ALOG(PLCL/P0(K))/ALOG(P0(KLCL)/P0(K))                    
!                                                                   
!...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...
!                                                                   
        TENV=T0(K)+(T0(KLCL)-T0(K))*DLP                            
        QENV=Q0(K)+(Q0(KLCL)-Q0(K))*DLP                           
        TVEN=TENV*(1.+0.608*QENV)                                
        TVBAR=0.5*(TV0(K)+TVEN)                                 
!        ZLCL=Z0(K)+R*TVBAR*ALOG(P0(K)/PLCL)/G                 
        ZLCL=Z0(K)+(Z0(KLCL)-Z0(K))*DLP                                 
!                                                                      
!...CHECK TO SEE IF CLOUD IS BUOYANT USING FRITSCH-CHAPPELL TRIGGER  
!...FUNCTION DESCRIBED IN KAIN AND FRITSCH (1992)...W0AVG IS AN     
!...APROXIMATE VALUE FOR THE RUNNING-MEAN GRID-SCALE VERTICAL      
!...VELOCITY, WHICH GIVES SMOOTHER FIELDS OF CONVECTIVE INITIATION 
!...THAN THE INSTANTANEOUS VALUE...FORMULA RELATING TEMPERATURE            
!...PERTURBATION TO VERTICAL VELOCITY HAS BEEN USED WITH THE MOST         
!...SUCCESS AT GRID LENGTHS NEAR 25 km.  FOR DIFFERENT GRID-LENGTHS,     
!...ADJUST VERTICAL VELOCITY TO EQUIVALENT VALUE FOR 25 KM GRID         
!...LENGTH, ASSUMING LINEAR DEPENDENCE OF W ON GRID LENGTH...          
!                                                                     
        WKLCL=0.02*ZLCL/2.5E3                                             
        WKL=(W0AVG1D(K)+(W0AVG1D(KLCL)-W0AVG1D(K))*DLP)*DX/25.E3- & 
            WKLCL                                                           
        WABS=ABS(WKL)+1.E-10                                               
        WSIGNE=WKL/WABS                                                   
        DTLCL=4.64*WSIGNE*WABS**0.33                                     
        GDT=G*DTLCL*(ZLCL-Z0(LC))/(TV0(LC)+TVEN)                        
        WLCL=1.+.5*WSIGNE*SQRT(ABS(GDT)+1.E-10)                        
        IF(TLCL+DTLCL.GT.TENV)GOTO 45                                 
        IF(KPBL.GE.LLFC)GOTO 325                                     
        GOTO 25                                                     
!                                                                       
!...CONVECTIVE TRIGGERING CRITERIA HAS BEEN SATISFIED...COMPUTE           
!...EQUIVALENT POTENTIAL TEMPERATURE                                     
!...(THETEU) AND VERTICAL VELOCITY OF THE RISING PARCEL AT THE LCL...    
!                                                                       
   45   THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))* & 
                  EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))     
        ES=ALIQ*EXP((TENV*BLIQ-CLIQ)/(TENV-DLIQ))                     
        TVAVG=0.5*(TV0(KLCL)+TENV*(1.+0.608*QENV))                   
        PLCL=P0(KLCL)*EXP(G/(R*TVAVG)*(Z0(KLCL)-ZLCL))              
        QESE=EP2*ES/(PLCL-ES)                                    
        GDT=G*DTLCL*(ZLCL-Z0(LC))/(TV0(LC)+TVEN)                  
        WLCL=1.+.5*WSIGNE*SQRT(ABS(GDT)+1.E-10)                      
        THTES(K)=TENV*(1.E5/PLCL)**(0.2854*(1.-0.28*QESE))* &    
                 EXP((3374.6525/TENV-2.5403)*QESE*(1.+0.81*QESE))           
        WTW=WLCL*WLCL                                                      
        IF(WLCL.LT.0.)GOTO 25                                             
        TVLCL=TLCL*(1.+0.608*QMIX)                                       
        RHOLCL=PLCL/(R*TVLCL)                                           
!                                                                      
        LCL=KLCL                                                      
        LET=LCL                                                            
!                                                                         
!*******************************************************************     
!                                                                  *    
!                 COMPUTE UPDRAFT PROPERTIES                       *   
!                                                                  *  
!******************************************************************* 
!                                                                   
!                                                                  
!...ESTIMATE INITIAL UPDRAFT MASS FLUX (UMF(K))...                
!                                                                
        WU(K)=WLCL                                                          
        AU0=PIE*RAD*RAD                                                    
        UMF(K)=RHOLCL*AU0                                                 
        VMFLCL=UMF(K)                                                    
        UPOLD=VMFLCL                                                    
        UPNEW=UPOLD                                                    
!                                                                     
!...RATIO2 IS THE DEGREE OF GLACIATION IN THE CLOUD (0 TO 1),        
!...UER IS THE ENVIR ENTRAINMENT RATE, ABE IS AVAILABLE BUOYANT ENERGY,   
!   TRPPT IS THE TOTAL RATE OF PRECIPITATION PRODUCTION...               
!                                                                         
        RATIO2(K)=0.                                                    
        UER(K)=0.                                                      
        ABE=0.                                                        
        TRPPT=0.                                                     
        TU(K)=TLCL                                                  
        TVU(K)=TVLCL                                               
        QU(K)=QMIX                                                
        EQFRC(K)=1.                                              
        QLIQ(K)=0.                                              
        QICE(K)=0.                                             
        QLQOUT(K)=0.                                          
        QICOUT(K)=0.                                         
        DETLQ(K)=0.                                         
        DETIC(K)=0.                                        
        PPTLIQ(K)=0.                                     
        PPTICE(K)=0.                                    
        IFLAG=0                                        
        KFRZ=LC                                       
!                                                    
!...THE AMOUNT OF CONV AVAIL POT ENERGY (CAPE) IS CALCULATED WITH   
!   RESPECT TO UNDILUTE PARCEL ASCENT; EQ POT TEMP OF UNDILUTE     
!   PARCEL IS THTUDL, UNDILUTE TEMPERATURE IS GIVEN BY TUDL...    
!                                                                
        THTUDL=THETEU(K)                                        
        TUDL=TLCL                                              
!                                                             
!...TTEMP IS USED DURING CALCULATION OF THE LINEAR GLACIATION        
!   PROCESS; IT IS INITIALLY SET TO THE TEMPERATURE AT WHICH        
!   FREEZING IS SPECIFIED TO BEGIN.  WITHIN THE GLACIATION         
!   INTERVAL, IT IS SET EQUAL TO THE UPDRAFT TEMP AT THE          
!   PREVIOUS MODEL LEVEL...                                      
!                                                               
        TTEMP=TTFRZ                                                   
!                                                                    
!...ENTER THE LOOP FOR UPDRAFT CALCULATIONS...CALCULATE UPDRAFT TEMP,    
!   MIXING RATIO, VERTICAL MASS FLUX, LATERAL DETRAINMENT OF MASS AND   
!   MOISTURE, PRECIPITATION RATES AT EACH MODEL LEVEL...               
!                                                                     
        DO 60 NK=K,KL-1
          NK1=NK+1                                                         
          RATIO2(NK1)=RATIO2(NK)                                          
!                                                                        
!...UPDATE UPDRAFT PROPERTIES AT THE NEXT MODEL LVL TO REFLECT          
!   ENTRAINMENT OF ENVIRONMENTAL AIR...                                     
!                                                                          
          FRC1=0.                                                         
          TU(NK1)=T0(NK1)                                                
          THETEU(NK1)=THETEU(NK)                                        
          QU(NK1)=QU(NK)                                               
          QLIQ(NK1)=QLIQ(NK)                                          
          QICE(NK1)=QICE(NK)                                         

          CALL TPMIX(P0(NK1),THETEU(NK1),TU(NK1),QU(NK1),QLIQ(NK1), & 
               QICE(NK1),QNEWLQ,QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0, &
               XLS1,EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)        
          TVU(NK1)=TU(NK1)*(1.+0.608*QU(NK1))                      
!                                                                 
!...CHECK TO SEE IF UPDRAFT TEMP IS WITHIN THE FREEZING INTERVAL,
!   IF IT IS, CALCULATE THE FRACTIONAL CONVERSION TO GLACIATION     
!   AND ADJUST QNEWLQ TO REFLECT THE GRADUAL CHANGE IN THETAU      
!   SINCE THE LAST MODEL LEVEL...THE GLACIATION EFFECTS WILL BE   
!   DETERMINED AFTER THE AMOUNT OF CONDENSATE AVAILABLE AFTER    
!   PRECIP FALLOUT IS DETERMINED...TTFRZ IS THE TEMP AT WHICH   
!   GLACIATION BEGINS, TBFRZ THE TEMP AT WHICH IT ENDS...     
!                                                            
          IF(TU(NK1).LE.TTFRZ.AND.IFLAG.LT.1)THEN                    
            IF(TU(NK1).GT.TBFRZ)THEN                                
              IF(TTEMP.GT.TTFRZ)TTEMP=TTFRZ                        
              FRC1=(TTEMP-TU(NK1))/(TTFRZ-TBFRZ)                  
              R1=(TTEMP-TU(NK1))/(TTEMP-TBFRZ)                   
            ELSE                                                
              FRC1=(TTEMP-TBFRZ)/(TTFRZ-TBFRZ)                
              R1=1.                                          
              IFLAG=1                                       
            ENDIF                                          
            QNWFRZ=QNEWLQ                                 
            QNEWIC=QNEWIC+QNEWLQ*R1*0.5                  
            QNEWLQ=QNEWLQ-QNEWLQ*R1*0.5                 
            EFFQ=(TTFRZ-TBFRZ)/(TTEMP-TBFRZ)           
            TTEMP=TU(NK1)                             
          ENDIF                                      
!                                                  
!  CALCULATE UPDRAFT VERTICAL VELOCITY AND PRECIPITATION FALLOUT...  
!                                                                   
          IF(NK.EQ.K)THEN                                          
            BE=(TVLCL+TVU(NK1))/(TVEN+TV0(NK1))-1.                
            BOTERM=2.*(Z0(NK1)-ZLCL)*G*BE/1.5                    
            ENTERM=0.                                           
            DZZ=Z0(NK1)-ZLCL                                   
          ELSE                                                
            BE=(TVU(NK)+TVU(NK1))/(TV0(NK)+TV0(NK1))-1.      
            BOTERM=2.*DZA(NK)*G*BE/1.5                                        
            ENTERM=2.*UER(NK)*WTW/UPOLD                                      
            DZZ=DZA(NK)                                                     
          ENDIF                                                            
          WSQ=WTW                                                         
          CALL CONDLOAD(QLIQ(NK1),QICE(NK1),WTW,DZZ,BOTERM,ENTERM,RATE, & 
               QNEWLQ,QNEWIC,QLQOUT(NK1),QICOUT(NK1), G)                    
                                                              
!...IF VERT VELOCITY IS LESS THAN ZERO, EXIT THE UPDRAFT LOOP AND,     
!   IF CLOUD IS TALL ENOUGH, FINALIZE UPDRAFT CALCULATIONS...         
!                                                                    
          IF(WTW.LE.0.)GOTO 65                                      
          WABS=SQRT(ABS(WTW))                                      
          WU(NK1)=WTW/WABS                                        
!                                                                
!  UPDATE THE ABE FOR UNDILUTE ASCENT...                        
!                                                                         
          THTES(NK1)=T0(NK1)*(1.E5/P0(NK1))**(0.2854*(1.-0.28*QES(NK1))) & 
                     *                                                   &
                     EXP((3374.6525/T0(NK1)-2.5403)*QES(NK1)*(1.+0.81*   &
                     QES(NK1)))                                          
          UDLBE=((2.*THTUDL)/(THTES(NK)+THTES(NK1))-1.)*DZZ             
          IF(UDLBE.GT.0.)ABE=ABE+UDLBE*G                               
!                                                                     
!  DETERMINE THE EFFECTS OF CLOUD GLACIATION IF WITHIN THE SPECIFIED 
!  TEMP INTERVAL...                                                 
!                                                                  
          IF(FRC1.GT.1.E-6)THEN                                   
            CALL DTFRZNEW(TU(NK1),P0(NK1),THETEU(NK1),QU(NK1),QLIQ(NK1), & 
                 QICE(NK1),RATIO2(NK1),TTFRZ,TBFRZ,QNWFRZ,RL,FRC1,EFFQ,  &
                 IFLAG,XLV0,XLV1,XLS0,XLS1,EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE &  
                 ,CICE,DICE)                                     
          ENDIF                                                 
!                                                              
!  CALL SUBROUTINE TO CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMP.      
!  WITHIN GLACIATION INTERVAL, THETAE MUST BE CALCULATED WITH RESPECT TO     
!  SAME DEGREE OF GLACIATION FOR ALL ENTRAINING AIR...                      
!                                                                          
          CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),THETEE(NK1),RATIO2(NK1), &
               RL,EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                
                                                                         
!...REI IS THE RATE OF ENVIRONMENTAL INFLOW...                          
!                                                                      
          REI=VMFLCL*DP(NK1)*0.03/RAD                                 
          TVQU(NK1)=TU(NK1)*(1.+0.608*QU(NK1)-QLIQ(NK1)-QICE(NK1))   
!                                                                   
!...IF CLOUD PARCELS ARE VIRTUALLY COLDER THAN THE ENVIRONMENT, NO 
!   ENTRAINMENT IS ALLOWED AT THIS LEVEL...                               
!                                                                        
          IF(TVQU(NK1).LE.TV0(NK1))THEN                                 
            UER(NK1)=0.0                                               
            UDR(NK1)=REI                                              
            EE2=0.                                                   
            UD2=1.                                                  
            EQFRC(NK1)=0.                                          
            GOTO 55                                                        
          ENDIF                                                          
          LET=NK1                                                       
          TTMP=TVQU(NK1)                                               
!                                                                          
!...DETERMINE THE CRITICAL MIXED FRACTION OF UPDRAFT AND ENVIRONMENTAL    
!   AIR FOR ESTIMATION OF ENTRAINMENT AND DETRAINMENT RATES...           
!                                                                       
          F1=0.95                                                      
          F2=1.-F1                                                    
          THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)                       
          QTMP=F1*Q0(NK1)+F2*QU(NK1)                                
          TMPLIQ=F2*QLIQ(NK1)                                      
          TMPICE=F2*QICE(NK1)                                                
          CALL TPMIX(P0(NK1),THTTMP,TTMP,QTMP,TMPLIQ,TMPICE,QNEWLQ,      & 
               QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,EP2,ALIQ,BLIQ,CLIQ, &
               DLIQ,AICE,BICE,CICE,DICE)                                    
          TU95=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)                          
          IF(TU95.GT.TV0(NK1))THEN                                        
            EE2=1.                                                       
            UD2=0.                                                      
            EQFRC(NK1)=1.0                                             
            GOTO 50                                                   
          ENDIF                                                              
          F1=0.10                                                           
          F2=1.-F1                                                         
          THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)                            
          QTMP=F1*Q0(NK1)+F2*QU(NK1)                                    
          TMPLIQ=F2*QLIQ(NK1)                                            
          TMPICE=F2*QICE(NK1)                                               
          CALL TPMIX(P0(NK1),THTTMP,TTMP,QTMP,TMPLIQ,TMPICE,QNEWLQ,      & 
               QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,EP2,ALIQ,BLIQ,CLIQ, &
               DLIQ,AICE,BICE,CICE,DICE)                                   
          TU10=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)                         
          IF(TU10.EQ.TVQU(NK1))THEN                                      
            EE2=1.                                                      
            UD2=0.                                                     
            EQFRC(NK1)=1.0                                            
            GOTO 50                                                  
          ENDIF                                                     
          EQFRC(NK1)=(TV0(NK1)-TVQU(NK1))*F1/(TU10-TVQU(NK1))      
          EQFRC(NK1)=AMAX1(0.0,EQFRC(NK1))                        
          EQFRC(NK1)=AMIN1(1.0,EQFRC(NK1))                       
          IF(EQFRC(NK1).EQ.1)THEN                               
            EE2=1.                                             
            UD2=0.                                            
            GOTO 50                                          
          ELSEIF(EQFRC(NK1).EQ.0.)THEN                                       
            EE2=0.                                                          
            UD2=1.                                                         
            GOTO 50                                                       
          ELSE                                                           
!                                                                       
!...SUBROUTINE PROF5 INTEGRATES OVER THE GAUSSIAN DIST TO DETERMINE THE
!   FRACTIONAL ENTRAINMENT AND DETRAINMENT RATES...                   
!                                                                    
            CALL PROF5(EQFRC(NK1),EE2,UD2)                          
          ENDIF                                                    
!                                                                 
   50     IF(NK.EQ.K)THEN                                        
            EE1=1.                                              
            UD1=0.                                             
          ENDIF                                               
!                                                            
!...NET ENTRAINMENT AND DETRAINMENT RATES ARE GIVEN BY THE AVERAGE         
!   FRACTIONAL VALUES IN THE LAYER...                                     
!                                                                        
          UER(NK1)=0.5*REI*(EE1+EE2)                                    
          UDR(NK1)=0.5*REI*(UD1+UD2)                                   
!                                                                     
!...IF THE CALCULATED UPDRAFT DETRAINMENT RATE IS GREATER THAN THE TOTAL  
!   UPDRAFT MASS FLUX, ALL CLOUD MASS DETRAINS, EXIT UPDRAFT CALCULATION    
!                                                                          
   55     IF(UMF(NK)-UDR(NK1).LT.10.)THEN                                
!                                                                       
!...IF THE CALCULATED DETRAINED MASS FLUX IS GREATER THAN THE TOTAL    
!   UPDRAFT FLUX, IMPOSE TOTAL DETRAINMENT OF UPDRAFT MASS AT THE     
!   PREVIOUS MODEL                                                   
!                                                                   
            IF(UDLBE.GT.0.)ABE=ABE-UDLBE*G                         
            LET=NK                                                
!         WRITE(98,1015)P0(NK1)/100.                                 
            GOTO 65                                                 
          ENDIF                                                    
          EE1=EE2                                                 
          UD1=UD2                                                
          UPOLD=UMF(NK)-UDR(NK1)                                
          UPNEW=UPOLD+UER(NK1)                                 
          UMF(NK1)=UPNEW                                      
!                                                            
!...DETLQ AND DETIC ARE THE RATES OF DETRAINMENT OF LIQUID AND ICE IN 
!   THE DETRAINING UPDRAFT MASS...                                   
!                                                                   
          DETLQ(NK1)=QLIQ(NK1)*UDR(NK1)                            
          DETIC(NK1)=QICE(NK1)*UDR(NK1)                           
          QDT(NK1)=QU(NK1)                                       
          QU(NK1)=(UPOLD*QU(NK1)+UER(NK1)*Q0(NK1))/UPNEW        
          THETEU(NK1)=(THETEU(NK1)*UPOLD+THETEE(NK1)*UER(NK1))/UPNEW  
          QLIQ(NK1)=QLIQ(NK1)*UPOLD/UPNEW                            
          QICE(NK1)=QICE(NK1)*UPOLD/UPNEW                           
!                                                                  
!...KFRZ IS THE HIGHEST MODEL LEVEL AT WHICH LIQUID CONDENSATE IS 
!   GENERATING PPTLIQ IS THE RATE OF GENERATION (FALLOUT) OF LIQUID     
!   PRECIP AT A GIVING MODEL LVL, PPTICE THE SAME FOR ICE, TRPPT IS    
!   THE TOTAL RATE OF PRODUCTION OF PRECIP UP TO THE CURRENT MODEL LEVEL 
!                                                                       
          IF(ABS(RATIO2(NK1)-1.).GT.1.E-6)KFRZ=NK1                     
          PPTLIQ(NK1)=QLQOUT(NK1)*(UMF(NK)-UDR(NK1))                  
          PPTICE(NK1)=QICOUT(NK1)*(UMF(NK)-UDR(NK1))                 
          TRPPT=TRPPT+PPTLIQ(NK1)+PPTICE(NK1)                       
          IF(NK1.LE.KPBL)UER(NK1)=UER(NK1)+VMFLCL*DP(NK1)/DPTHMX   
   60   CONTINUE                                                  
!                                                                
!...CHECK CLOUD DEPTH...IF CLOUD IS TALL ENOUGH, ESTIMATE THE EQUILIBRIU
!   TEMPERATURE LEVEL (LET) AND ADJUST MASS FLUX PROFILE AT CLOUD TOP SO
!   THAT MASS FLUX DECREASES TO ZERO AS A LINEAR FUNCTION OF PRESSURE  
!   BETWEEN THE LET AND CLOUD TOP...                                  
!                                                                    
!...LTOP IS THE MODEL LEVEL JUST BELOW THE LEVEL AT WHICH VERTICAL  
!   VELOCITY FIRST BECOMES NEGATIVE...                             
!                                                                 
   65   LTOP=NK                                                  
        CLDHGT=Z0(LTOP)-ZLCL                                    
!                                                              
!...IF CLOUD TOP HGT IS LESS THAN SPECIFIED MINIMUM HEIGHT, GO BACK AND 
!   THE NEXT HIGHEST 60MB LAYER TO SEE IF A BIGGER CLOUD CAN BE OBTAINED
!   THAT SOURCE AIR...                                                 
!                                                                     
!       IF(CLDHGT.LT.4.E3.OR.ABE.LT.1.)THEN                          
        IF(CLDHGT.LT.3.E3.OR.ABE.LT.1.)THEN                         
          DO 70 NK=K,LTOP                                          
            UMF(NK)=0.                                            
            UDR(NK)=0.                                           
            UER(NK)=0.                                          
            DETLQ(NK)=0.                                       
            DETIC(NK)=0.                                      
            PPTLIQ(NK)=0.                                    
   70     PPTICE(NK)=0.                                     
          GOTO 25                                          
        ENDIF                                             
!                                                                    
!...IF THE LET AND LTOP ARE THE SAME, DETRAIN ALL OF THE UPDRAFT MASS 
!   FLUX THIS LEVEL...                                               
!                                                                   
        IF(LET.EQ.LTOP)THEN                                        
          UDR(LTOP)=UMF(LTOP)+UDR(LTOP)-UER(LTOP)                 
          DETLQ(LTOP)=QLIQ(LTOP)*UDR(LTOP)*UPNEW/UPOLD           
          DETIC(LTOP)=QICE(LTOP)*UDR(LTOP)*UPNEW/UPOLD          
          TRPPT=TRPPT-(PPTLIQ(LTOP)+PPTICE(LTOP))              
          UER(LTOP)=0.                                        
          UMF(LTOP)=0.                                       
          GOTO 85                                           
        ENDIF                                              
!                                                         
!   BEGIN TOTAL DETRAINMENT AT THE LEVEL ABOVE THE LET...
!                                                       
        DPTT=0.                                        
        DO 71 NJ=LET+1,LTOP                           
   71   DPTT=DPTT+DP(NJ)                             
        DUMFDP=UMF(LET)/DPTT                        
!                                                  
!...ADJUST MASS FLUX PROFILES, DETRAINMENT RATES, AND PRECIPITATION FALL 
!   RATES TO REFLECT THE LINEAR DECREASE IN MASS FLX BETWEEN THE LET AND 
!   PTOP                                                                
!                                                                      
        DO 75 NK=LET+1,LTOP                                           
          UDR(NK)=DP(NK)*DUMFDP                                      
          UMF(NK)=UMF(NK-1)-UDR(NK)                                 
          DETLQ(NK)=QLIQ(NK)*UDR(NK)                               
          DETIC(NK)=QICE(NK)*UDR(NK)                              
          TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)                      
          PPTLIQ(NK)=(UMF(NK-1)-UDR(NK))*QLQOUT(NK)             
          PPTICE(NK)=(UMF(NK-1)-UDR(NK))*QICOUT(NK)            
          TRPPT=TRPPT+PPTLIQ(NK)+PPTICE(NK)                   
   75   CONTINUE                                             
!                                                           
!...SEND UPDRAFT CHARACTERISTICS TO OUTPUT FILES...        
!                                                         
   85   CONTINUE                                         
!                                                       
!...EXTEND THE UPDRAFT MASS FLUX PROFILE DOWN TO THE SOURCE LAYER FOR 
!   THE UPDRAFT AIR...ALSO, DEFINE THETAE FOR LEVELS BELOW THE LCL...
!                                                                   
        DO 90 NK=1,K                                               
          IF(NK.GE.LC)THEN                                        
            IF(NK.EQ.LC)THEN                                     
              UMF(NK)=VMFLCL*DP(NK)/DPTHMX                      
              UER(NK)=VMFLCL*DP(NK)/DPTHMX                     
            ELSEIF(NK.LE.KPBL)THEN                            
              UER(NK)=VMFLCL*DP(NK)/DPTHMX                   
              UMF(NK)=UMF(NK-1)+UER(NK)                     
            ELSE                                         
              UMF(NK)=VMFLCL                               
              UER(NK)=0.                                
            ENDIF                                         
            TU(NK)=TMIX+(Z0(NK)-ZMIX)*GDRY             
            QU(NK)=QMIX                               
            WU(NK)=WLCL                              
          ELSE                                      
            TU(NK)=0.                              
            QU(NK)=0.                                                      
            UMF(NK)=0.                                                    
            WU(NK)=0.                                                    
            UER(NK)=0.                                                  
          ENDIF                                                        
          UDR(NK)=0.                                                  
          QDT(NK)=0.                                                 
          QLIQ(NK)=0.                                               
          QICE(NK)=0.                                              
          QLQOUT(NK)=0.                                           
          QICOUT(NK)=0.                                          
          PPTLIQ(NK)=0.                                         
          PPTICE(NK)=0.                                        
          DETLQ(NK)=0.                                        
          DETIC(NK)=0.                                       
          RATIO2(NK)=0.                                    
          EE=Q0(NK)*P0(NK)/(EP2+Q0(NK))                 
          TLOG=ALOG(EE/ALIQ)                             
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)             
          TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T0(NK)-T00))*( & 
               T0(NK)-TDPT)                                                
          THTA=T0(NK)*(1.E5/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))            
          THETEE(NK)=THTA*                                               & 
                     EXP((3374.6525/TSAT-2.5403)*Q0(NK)*(1.+0.81*Q0(NK)) &
                     )                                                   
          THTES(NK)=THTA*                                                &
                    EXP((3374.6525/T0(NK)-2.5403)*QES(NK)*(1.+0.81*      &
                    QES(NK)))                          
          EQFRC(NK)=1.0                               
   90   CONTINUE                                     
!                                                   
        LTOP1=LTOP+1                               
        LTOPM1=LTOP-1                              
!                                                  
!...DEFINE VARIABLES ABOVE CLOUD TOP...            
!                                                             
        DO 95 NK=LTOP1,KX                                    
          UMF(NK)=0.                                        
          UDR(NK)=0.                                       
          UER(NK)=0.                                      
          QDT(NK)=0.                                     
          QLIQ(NK)=0.                                                     
          QICE(NK)=0.                                                    
          QLQOUT(NK)=0.                                                 
          QICOUT(NK)=0.                                                
          DETLQ(NK)=0.                                                
          DETIC(NK)=0.                                               
          PPTLIQ(NK)=0.                                             
          PPTICE(NK)=0.                                            
          IF(NK.GT.LTOP1)THEN                                     
            TU(NK)=0.                                            
            QU(NK)=0.                                           
            WU(NK)=0.                                          
          ENDIF                                              
          THTA0(NK)=0.                                      
          THTAU(NK)=0.                                     
          EMS(NK)=DP(NK)*DXSQ/G                           
          EMSD(NK)=1./EMS(NK)                            
          TG(NK)=T0(NK)                                 
          QG(NK)=Q0(NK)                                
          QLG(NK)=0.                                  
          QIG(NK)=0.                                 
          QRG(NK)=0.                                
          QSG(NK)=0.                               
   95   OMG(NK)=0.                                 
        OMG(KL+1)=0.                               
        P150=P0(KLCL)-1.50E4                       
        DO 100 NK=1,LTOP                           
          THTAD(NK)=0.                             
          EMS(NK)=DP(NK)*DXSQ/G                   
          EMSD(NK)=1./EMS(NK)                      
!                                                  
!...INITIALIZE SOME VARIABLES TO BE USED LATER IN THE VERT ADVECTION 
!   SCHEME                                                          
!                                                                  
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QDT(NK)))        
          THTAU(NK)=TU(NK)*EXN(NK)                               
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))       
          THTA0(NK)=T0(NK)*EXN(NK)                             
!                                                             
!...LVF IS THE LEVEL AT WHICH MOISTURE FLUX IS ESTIMATED AS THE BASIS 
!...FOR PRECIPITATION EFFICIENCY CALCULATIONS...                     
!                                                                        
          IF(P0(NK).GT.P150)LVF=NK                                      
  100   OMG(NK)=0.                                                     
        LVF=MIN0(LVF,LET)                                             
        USR=UMF(LVF+1)*(QU(LVF+1)+QLIQ(LVF+1)+QICE(LVF+1))           
        USR=AMIN1(USR,TRPPT)                                        
        IF(USR.LT.1.E-8)USR=TRPPT
!                                                                  
!     WRITE(98,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,          
!    * TMIX-T00,PMIX,QMIX,ABE                                    
!     WRITE(98,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,
!    * WLCL,CLDHGT                                             
!                                                             
!...COMPUTE CONVECTIVE TIME SCALE(TIMEC). THE MEAN WIND AT THE LCL     
!...AND MIDTROPOSPHERE IS USED.                                       
!                                                                    
        WSPD(KLCL)=SQRT(U0(KLCL)*U0(KLCL)+V0(KLCL)*V0(KLCL))        
        WSPD(L5)=SQRT(U0(L5)*U0(L5)+V0(L5)*V0(L5))                 
        WSPD(LTOP)=SQRT(U0(LTOP)*U0(LTOP)+V0(LTOP)*V0(LTOP))      
        VCONV=.5*(WSPD(KLCL)+WSPD(L5))                           
        if (VCONV .gt. 0.) then
           TIMEC=DX/VCONV
        else
           TIMEC=3600.
        endif
!       TIMEC=DX/VCONV
        TADVEC=TIMEC                                           
        TIMEC=AMAX1(1800.,TIMEC)                              
        TIMEC=AMIN1(3600.,TIMEC)                             
        NIC=NINT(TIMEC/DT)                            
        TIMEC=FLOAT(NIC)*DT                            
!                                                         
!...COMPUTE WIND SHEAR AND PRECIPITATION EFFICIENCY.     
!                                                       
!        SHSIGN = CVMGT(1.,-1.,WSPD(LTOP).GT.WSPD(KLCL))
        IF(WSPD(LTOP).GT.WSPD(KLCL))THEN               
          SHSIGN=1.                                   
        ELSE                                         
          SHSIGN=-1.                                
        ENDIF                                      
        VWS=(U0(LTOP)-U0(KLCL))*(U0(LTOP)-U0(KLCL))+(V0(LTOP)-V0(KLCL))* & 
            (V0(LTOP)-V0(KLCL))                                         
        VWS=1.E3*SHSIGN*SQRT(VWS)/(Z0(LTOP)-Z0(LCL))                   
        PEF=1.591+VWS*(-.639+VWS*(9.53E-2-VWS*4.96E-3))               
        PEF=AMAX1(PEF,.2)                                            
        PEF=AMIN1(PEF,.9)                                           
!                                                                  
!...PRECIPITATION EFFICIENCY IS A FUNCTION OF THE HEIGHT OF CLOUD BASE. 
!                                                                      
        CBH=(ZLCL-Z0(1))*3.281E-3                                     
        IF(CBH.LT.3.)THEN                                            
          RCBH=.02                                                  
        ELSE                                                       
          RCBH=.96729352+CBH*(-.70034167+CBH*(.162179896+CBH*(-  & 
               1.2569798E-2+CBH*(4.2772E-4-CBH*5.44E-6))))    
        ENDIF                                                
        IF(CBH.GT.25)RCBH=2.4                               
        PEFCBH=1./(1.+RCBH)                                
        PEFCBH=AMIN1(PEFCBH,.9)                           
!                                                        
!... MEAN PEF. IS USED TO COMPUTE RAINFALL.                           
!                                                                    
        PEFF=.5*(PEF+PEFCBH)                                        
        PEFF2=PEFF                                                 
!        WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS                  
!                                                                
!*****************************************************************   
!                                                                *  
!                  COMPUTE DOWNDRAFT PROPERTIES                  * 
!                                                                *
!*****************************************************************         
!                                                                         
!...LET DOWNDRAFT ORIGINATE AT THE LEVEL OF MINIMUM SATURATION EQUIVALEN 
!...POTENTIAL TEMPERATURE (SEQT) IN THE CLOUD LAYER, EXTEND DOWNWARD TO 
!...SURFACE, OR TO THE LAYER BELOW CLOUD BASE AT WHICH ENVIR SEQT IS LES    
!...THAN MIN SEQT IN THE CLOUD LAYER...LET DOWNDRAFT DETRAIN OVER A LAYE   
!...OF SPECIFIED PRESSURE-DEPTH (DPDD)...                                 
!                                                                       
        TDER=0.                                                        
        KSTART=MAX0(KPBL,KLCL)                                        
        THTMIN=THTES(KSTART+1)                                       
        KMIN=KSTART+1                                               
        DO 104 NK=KSTART+2,LTOP-1                                  
          THTMIN=AMIN1(THTMIN,THTES(NK))                          
          IF(THTMIN.EQ.THTES(NK))KMIN=NK                         
  104   CONTINUE                                                
        LFS=KMIN                                               
        IF(RATIO2(LFS).GT.0.)CALL ENVIRTHT(P0(LFS),T0(LFS),Q0(LFS),     &
          THETEE(LFS),0.,RL,EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)   
        EQFRC(LFS)=(THTES(LFS)-THETEU(LFS))/(THETEE(LFS)-THETEU(LFS)) 
        EQFRC(LFS)=AMAX1(EQFRC(LFS),0.)                              
        EQFRC(LFS)=AMIN1(EQFRC(LFS),1.)                             
        THETED(LFS)=THTES(LFS)                                     
!                                                                 
!...ESTIMATE THE EFFECT OF MELTING PRECIPITATION IN THE DOWNDRAFT...   
!                                                                            
        IF(ML.GT.0)THEN                                                    
          DTMLTD=0.5*(QU(KLCL)-QU(LTOP))*RLF/CP                          
        ELSE                                                            
          DTMLTD=0.                                                    
        ENDIF                                                         
        TZ(LFS)=T0(LFS)-DTMLTD                                       
        ES=ALIQ*EXP((TZ(LFS)*BLIQ-CLIQ)/(TZ(LFS)-DLIQ))             
        QS=EP2*ES/(P0(LFS)-ES)                                   
        QD(LFS)=EQFRC(LFS)*Q0(LFS)+(1.-EQFRC(LFS))*QU(LFS)             
        THTAD(LFS)=TZ(LFS)*(P00/P0(LFS))**(0.2854*(1.-0.28*QD(LFS)))     
        IF(QD(LFS).GE.QS)THEN                                           
          THETED(LFS)=THTAD(LFS)*                                       & 
                      EXP((3374.6525/TZ(LFS)-2.5403)*QS*(1.+0.81*QS))  
        ELSE                                                          
          CALL ENVIRTHT(P0(LFS),TZ(LFS),QD(LFS),THETED(LFS),0.,RL,EP2,ALIQ, & 
               BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                   
        ENDIF                                                       
        DO 107 NK=1,LFS                                            
          ND=LFS-NK                                             
          IF(THETED(LFS).GT.THTES(ND).OR.ND.EQ.1)THEN          
            LDB=ND                                            
!                                                            
!...IF DOWNDRAFT NEVER BECOMES NEGATIVELY BUOYANT OR IF IT  
!...IS SHALLOWER 50 mb, DON'T ALLOW IT TO OCCUR AT ALL...             
!                                                                    
            IF(NK.EQ.1.OR.(P0(LDB)-P0(LFS)).LT.50.E2)GOTO 141       
! testing ---- no downdraft
!           GOTO 141       
            GOTO 110                                               
          ENDIF                                                   
  107   CONTINUE                                                 
!                                                               
!...ALLOW DOWNDRAFT TO DETRAIN IN A SINGLE LAYER, BUT WITH DOWNDRAFT AIR 
!...TYPICALLY FLUSHED UP INTO HIGHER LAYERS AS ALLOWED IN THE TOTAL     
!...VERTICAL ADVECTION CALCULATIONS FARTHER DOWN IN THE CODE...        
!                                                                     
  110   DPDD=DP(LDB)                                                 
        LDT=LDB                                                     
        FRC=1.                                                     
        DPT=0.                                                              
!      DO 115 NK=LDB,LFS                                                   
!        DPT=DPT+DP(NK)                                                   
!        IF(DPT.GT.DPDD)THEN                                             
!          LDT=NK                                                       
!          FRC=(DPDD+DP(NK)-DPT)/DP(NK)                                
!          GOTO 120                                                   
!        ENDIF                                                       
!        IF(NK.EQ.LFS-1)THEN                                        
!         LDT=NK                                                   
!        FRC=1.                                                   
!        DPDD=DPT                                                
!        GOTO 120                                               
!        ENDIF                                                 
!115   CONTINUE                                               
  120   CONTINUE                                             
!                                                           
!...TAKE A FIRST GUESS AT THE INITIAL DOWNDRAFT MASS FLUX..
!                                                         
        TVD(LFS)=T0(LFS)*(1.+0.608*QES(LFS))             
        RDD=P0(LFS)/(R*TVD(LFS))                        
        A1=(1.-PEFF)*AU0                               
        DMF(LFS)=-A1*RDD                              
        DER(LFS)=EQFRC(LFS)*DMF(LFS)                 
        DDR(LFS)=0.                                 
        DO 140 ND=LFS-1,LDB,-1                     
          ND1=ND+1                                 
          IF(ND.LE.LDT)THEN                        
            DER(ND)=0.                                              
            DDR(ND)=-DMF(LDT+1)*DP(ND)*FRC/DPDD                    
            DMF(ND)=DMF(ND1)+DDR(ND)                              
            FRC=1.                                               
            THETED(ND)=THETED(ND1)                              
            QD(ND)=QD(ND1)                                     
          ELSE                                                
            DER(ND)=DMF(LFS)*0.03*DP(ND)/RAD                 
            DDR(ND)=0.                                      
            DMF(ND)=DMF(ND1)+DER(ND)                       
            IF(RATIO2(ND).GT.0.)CALL ENVIRTHT(P0(ND),T0(ND),Q0(ND),      & 
              THETEE(ND),0.,RL,EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)   
            THETED(ND)=(THETED(ND1)*DMF(ND1)+THETEE(ND)*DER(ND))/DMF(ND) 
            QD(ND)=(QD(ND1)*DMF(ND1)+Q0(ND)*DER(ND))/DMF(ND)            
          ENDIF                                                        
  140   CONTINUE                                                      
        TDER=0.                                                      
!                                                                   
!...CALCULATION AN EVAPORATION RATE FOR GIVEN MASS FLUX...        
!                                                                
        DO 135 ND=LDB,LDT                                       
          TZ(ND)=                                                        & 
                 TPDD(P0(ND),THETED(LDT),T0(ND),QS,QD(ND),1.0,XLV0,XLV1, &
                 EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)      
          ES=ALIQ*EXP((TZ(ND)*BLIQ-CLIQ)/(TZ(ND)-DLIQ))        
          QS=EP2*ES/(P0(ND)-ES)                             
          DSSDT=(CLIQ-BLIQ*DLIQ)/((TZ(ND)-DLIQ)*(TZ(ND)-DLIQ))                
          RL=XLV0-XLV1*TZ(ND)                                                
          DTMP=RL*QS*(1.-RHBC)/(CP+RL*RHBC*QS*DSSDT)                        
          T1RH=TZ(ND)+DTMP                                                 
          ES=RHBC*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))                  
          QSRH=EP2*ES/(P0(ND)-ES)                                      
!                                                                       
!...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL   
!...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...          
!                                                                    
          IF(QSRH.LT.QD(ND))THEN                                    
            QSRH=QD(ND)                                            
!          T1RH=T1+(QS-QSRH)*RL/CP                                
            T1RH=TZ(ND)                                          
          ENDIF                                                 
          TZ(ND)=T1RH                                          
          QS=QSRH                                             
          TDER=TDER+(QS-QD(ND))*DDR(ND)                     
          QD(ND)=QS                                                   
  135   THTAD(ND)=TZ(ND)*(P00/P0(ND))**(0.2854*(1.-0.28*QD(ND)))         
!                                                                       
!...IF DOWNDRAFT DOES NOT EVAPORATE ANY WATER FOR SPECIFIED RELATIVE   
!...HUMIDITY, NO DOWNDRAFT IS ALLOWED...                             
!                                                                   
  141   IF(TDER.LT.1.)THEN                                         
!          WRITE(98,3004)I,J                                      
 3004       FORMAT(' ','I=',I3,2X,'J=',I3)                       
          PPTFLX=TRPPT                                          
          CPR=TRPPT                                            
          TDER=0.                                             
          CNDTNF=0.                                          
          UPDINC=1.                                         
          LDB=LFS                                          
          DO 117 NDK=1,LTOP                               
            DMF(NDK)=0.                                                    
            DER(NDK)=0.                                  
            DDR(NDK)=0.                                 
            THTAD(NDK)=0.                              
            WD(NDK)=0.                                
            TZ(NDK)=0.                               
  117     QD(NDK)=0.                                
          AINCM2=100.                                                      
          GOTO 165                                                        
        ENDIF                                                            
!                                                                       
!...ADJUST DOWNDRAFT MASS FLUX SO THAT EVAPORATION RATE IN DOWNDRAFT IS
!...CONSISTENT WITH PRECIPITATION EFFICIENCY RELATIONSHIP...          
!                                                                    
        DEVDMF=TDER/DMF(LFS)                                        
        PPR=0.                                                     
        PPTFLX=PEFF*USR                                           
        RCED=TRPPT-PPTFLX                                        
!                                                               
!...PPR IS THE TOTAL AMOUNT OF PRECIPITATION THAT FALLS  OUT OF THE  
!...UPDRAFT FROM CLOUD BASE TO THE LFS...UPDRAFT MASS FLUX WILL BE  
!...INCREASED UP TO THE LFS TO ACCOUNT FOR UPDRAFT AIR MIXING WITH 
!...ENVIRONMENTAL AIR TO THE UPDRAFT, SO PPR WILL INCREASE        
!...PROPORTIONATELY...                                           
!                                                               
        DO 132 NM=KLCL,LFS                                     
  132   PPR=PPR+PPTLIQ(NM)+PPTICE(NM)                         
        IF(LFS.GE.KLCL)THEN                                  
          DPPTDF=(1.-PEFF)*PPR*(1.-EQFRC(LFS))/UMF(LFS)     
        ELSE                                               
          DPPTDF=0.                                       
        ENDIF                                            
!                                                       
!...CNDTNF IS THE AMOUNT OF CONDENSATE TRANSFERRED ALONG WITH UPDRAFT      
!...MASS THE DOWNDRAFT AT THE LFS...                                      
!                                                                        
        CNDTNF=(QLIQ(LFS)+QICE(LFS))*(1.-EQFRC(LFS))                    
        DMFLFS=RCED/(DEVDMF+DPPTDF+CNDTNF)                             
        IF(DMFLFS.GT.0.)THEN                                         
          TDER=0.                                                   
          GOTO 141                                                 
        ENDIF                                                     
!                                                                
!...DDINC IS THE FACTOR BY WHICH TO INCREASE THE FIRST-GUESS DOWNDRAFT    
!...MASS FLUX TO SATISFY THE PRECIP EFFICIENCY RELATIONSHIP, UPDINC IS T 
!...WHICH TO INCREASE THE UPDRAFT MASS FLUX BELOW THE LFS TO ACCOUNT FOR
!...TRANSFER OF MASS FROM UPDRAFT TO DOWNDRAFT...                     
!                                                                      
!       DDINC=DMFLFS/DMF(LFS)                                        
        IF(LFS.GE.KLCL)THEN                                         
          UPDINC=(UMF(LFS)-(1.-EQFRC(LFS))*DMFLFS)/UMF(LFS)        
!                                                                 
!...LIMIT UPDINC TO LESS THAN OR EQUAL TO 1.5...                 
!                                                                         
          IF(UPDINC.GT.1.5)THEN                                          
            UPDINC=1.5                                                  
            DMFLFS2=UMF(LFS)*(UPDINC-1.)/(EQFRC(LFS)-1.)               
            RCED2=DMFLFS2*(DEVDMF+DPPTDF+CNDTNF)                      
            PPTFLX=PPTFLX+(RCED-RCED2)                               
            PEFF2=PPTFLX/USR                                        
            RCED=RCED2                                             
            DMFLFS=DMFLFS2                                        
          ENDIF                                                  
        ELSE                                                    
          UPDINC=1.                                            
        ENDIF                                                 
        DDINC=DMFLFS/DMF(LFS)                                
        DO 149 NK=LDB,LFS                                   
          DMF(NK)=DMF(NK)*DDINC                            
          DER(NK)=DER(NK)*DDINC                           
          DDR(NK)=DDR(NK)*DDINC                          
  149   CONTINUE                                        
        CPR=TRPPT+PPR*(UPDINC-1.)                      
        PPTFLX=PPTFLX+PEFF*PPR*(UPDINC-1.)            
        PEFF=PEFF2                                   
        TDER=TDER*DDINC                             
!                                                                          
!...ADJUST UPDRAFT MASS FLUX, MASS DETRAINMENT RATE, AND LIQUID WATER AN  
!   DETRAINMENT RATES TO BE CONSISTENT WITH THE TRANSFER OF THE ESTIMATE 
!   FROM THE UPDRAFT TO THE DOWNDRAFT AT THE LFS...                    
!                                                                     
        DO 155 NK=LC,LFS                                             
          UMF(NK)=UMF(NK)*UPDINC                                    
          UDR(NK)=UDR(NK)*UPDINC                                   
          UER(NK)=UER(NK)*UPDINC                                 
          PPTLIQ(NK)=PPTLIQ(NK)*UPDINC                          
          PPTICE(NK)=PPTICE(NK)*UPDINC                         
          DETLQ(NK)=DETLQ(NK)*UPDINC                          
  155   DETIC(NK)=DETIC(NK)*UPDINC                           
!                                                           
!...ZERO OUT THE ARRAYS FOR DOWNDRAFT DATA AT LEVELS ABOVE AND BELOW THE 
!...DOWNDRAFT...                                                        
!                                                                           
        IF(LDB.GT.1)THEN                                                   
          DO 156 NK=1,LDB-1                                               
            DMF(NK)=0.                                                   
            DER(NK)=0.                                                  
            DDR(NK)=0.                                                 
            WD(NK)=0.                                                 
            TZ(NK)=0.                                                
            QD(NK)=0.                                               
            THTAD(NK)=0.                                           
  156     CONTINUE                                                
        ENDIF                                                    
        DO 157 NK=LFS+1,KX                                      
          DMF(NK)=0.                                           
          DER(NK)=0.                                          
          DDR(NK)=0.                                         
          WD(NK)=0.                                         
          TZ(NK)=0.                                        
          QD(NK)=0.                                      
          THTAD(NK)=0.                                  
  157   CONTINUE                                       
        DO 158 NK=LDT+1,LFS-1                         
          TZ(NK)=0.                                  
          QD(NK)=0.                                 
  158   CONTINUE                                    
!                                                                          
!                                                                      
!...SET LIMITS ON THE UPDRAFT AND DOWNDRAFT MASS FLUXES SO THAT THE   
!   INFLOW INTO CONVECTIVE DRAFTS FROM A GIVEN LAYER IS NO MORE THAN 
!   IS AVAILABLE IN THAT LAYER INITIALLY...                         
!                                                                  
  165   AINCMX=1000.                                              
        LMAX=MAX0(KLCL,LFS)                                      
        DO 166 NK=LC,LMAX                                       
          IF((UER(NK)-DER(NK)).GT.0.)AINCM1=EMS(NK)/((UER(NK)-DER(NK))* & 
            TIMEC)                                             
          AINCMX=AMIN1(AINCMX,AINCM1)                         
  166   CONTINUE                                             
        AINC=1.                                             
        IF(AINCMX.LT.AINC)AINC=AINCMX                      
!                                                         
!...SAVE THE RELEVENT VARIABLES FOR A UNIT UPDRFT AND DOWNDRFT...THEY    
!...WILL ITERATIVELY ADJUSTED BY THE FACTOR AINC TO SATISFY THE         
!...STABILIZATION CLOSURE...                                           
!                                                                         
        NCOUNT=0                                                         
        TDER2=TDER                                                      
        PPTFL2=PPTFLX                                                  
        DO 170 NK=1,LTOP                                              
          DETLQ2(NK)=DETLQ(NK)                                       
          DETIC2(NK)=DETIC(NK)                                      
          UDR2(NK)=UDR(NK)                                         
          UER2(NK)=UER(NK)                                        
          DDR2(NK)=DDR(NK)                                       
          DER2(NK)=DER(NK)                                      
          UMF2(NK)=UMF(NK)                                     
          DMF2(NK)=DMF(NK)                                    
  170   CONTINUE                                             
        FABE=1.                                             
        STAB=0.95                                          
        NOITR=0                                           
        IF(AINC/AINCMX.GT.0.999)THEN                     
          NCOUNT=0                                      
          GOTO 255                                     
        ENDIF                                        
        ISTOP=0                                     
  175   NCOUNT=NCOUNT+1                             
!                                                   
!*****************************************************************         
!                                                                *        
!           COMPUTE PROPERTIES FOR COMPENSATIONAL SUBSIDENCE     *       
!                                                                *      
!*****************************************************************     
!                                                                     
!...DETERMINE OMEGA VALUE NECESSARY AT TOP AND BOTTOM OF EACH LAYER TO    
!...SATISFY MASS CONTINUITY...                                           
!                                                                       
  185   CONTINUE                                                       
        DTT=TIMEC                                                     
        DO 200 NK=1,LTOP                                             
          DOMGDP(NK)=-(UER(NK)-DER(NK)-UDR(NK)-DDR(NK))*EMSD(NK)    
          IF(NK.GT.1)THEN                                          
            OMG(NK)=OMG(NK-1)-DP(NK-1)*DOMGDP(NK-1)               
            DTT1=0.75*DP(NK-1)/(ABS(OMG(NK))+1.E-10)             
            DTT=AMIN1(DTT,DTT1)                                 
          ENDIF                                                        
  200   CONTINUE                                                      
        DO 488 NK=1,LTOP                                             
          THPA(NK)=THTA0(NK)                                        
          QPA(NK)=Q0(NK)                                           
          NSTEP=NINT(TIMEC/DTT+1)                                 
          DTIME=TIMEC/FLOAT(NSTEP)                              
          FXM(NK)=OMG(NK)*DXSQ/G                               
  488   CONTINUE                                              
!                                                            
!...DO AN UPSTREAM/FORWARD-IN-TIME ADVECTION OF THETA, QV...
!                                                          
        DO 495 NTC=1,NSTEP                                
!                                                        
!...ASSIGN THETA AND Q VALUES AT THE TOP AND BOTTOM OF EACH LAYER BASED   
!...SIGN OF OMEGA...                                                     
!                                                                       
          DO 493 NK=1,LTOP                                             
            THFXTOP(NK)=0.                                             
            THFXBOT(NK)=0.                                           
            QFXTOP(NK)=0.                                            
  493     QFXBOT(NK)=0.                                            
          DO 494 NK=2,LTOP
            IF(OMG(NK).LE.0.)THEN
              THFXBOT(NK)=-FXM(NK)*THPA(NK-1)
              QFXBOT(NK)=-FXM(NK)*QPA(NK-1)
              THFXTOP(NK-1)=THFXTOP(NK-1)-THFXBOT(NK)
              QFXTOP(NK-1)=QFXTOP(NK-1)-QFXBOT(NK)
            ELSE
              THFXBOT(NK)=-FXM(NK)*THPA(NK)
              QFXBOT(NK)=-FXM(NK)*QPA(NK)
              THFXTOP(NK-1)=THFXTOP(NK-1)-THFXBOT(NK)
              QFXTOP(NK-1)=QFXTOP(NK-1)-QFXBOT(NK)
            ENDIF
  494     CONTINUE
!                                                   
!...UPDATE THE THETA AND QV VALUES AT EACH LEVEL..  
!                                                       
          DO 492 NK=1,LTOP                             
            THPA(NK)=THPA(NK)+(THFXBOT(NK)+UDR(NK)*THTAU(NK)+DDR(NK)*     & 
                     THTAD(NK)+THFXTOP(NK)-(UER(NK)-DER(NK))*THTA0(NK))* &
                     DTIME*EMSD(NK)                                     
            QPA(NK)=QPA(NK)+(QFXBOT(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK)+   & 
                    QFXTOP(NK)-(UER(NK)-DER(NK))*Q0(NK))*DTIME*EMSD(NK)  

  492     CONTINUE                                                     
  495   CONTINUE                                                      
        DO 498 NK=1,LTOP                                             
          THTAG(NK)=THPA(NK)                                        
          QG(NK)=QPA(NK)                                           
  498   CONTINUE                                                  
!                                                                
!...CHECK TO SEE IF MIXING RATIO DIPS BELOW ZERO ANYWHERE;  IF SO,        
!...BORROW MOISTURE FROM ADJACENT LAYERS TO BRING IT BACK UP ABOVE ZERO. 
!                                                                       
        DO 499 NK=1,LTOP                                               
          IF(QG(NK).LT.0.)THEN                                        
            IF(NK.EQ.1)THEN                                          
              CALL wrf_error_fatal ( 'module_cu_kf.F: problem with kf scheme:  qg = 0 at the surface' )
            ENDIF                                                
            NK1=NK+1                                            
            IF(NK.EQ.LTOP)NK1=KLCL                             
            TMA=QG(NK1)*EMS(NK1)                              
            TMB=QG(NK-1)*EMS(NK-1)                           
            TMM=(QG(NK)-1.E-9)*EMS(NK)                      
            BCOEFF=-TMM/((TMA*TMA)/TMB+TMB)                
            ACOEFF=BCOEFF*TMA/TMB                         
            TMB=TMB*(1.-BCOEFF)                          
            TMA=TMA*(1.-ACOEFF)                         
            IF(NK.EQ.LTOP)THEN                                  
              QVDIFF=(QG(NK1)-TMA*EMSD(NK1))*100./QG(NK1)     
              IF(ABS(QVDIFF).GT.1.)THEN                        
            PRINT *,'--WARNING-- CLOUD BASE WATER VAPOR CHANGES BY ',    & 
            QVDIFF,                                                      &
             ' PERCENT WHEN MOISTURE IS BORROWED TO PREVENT NEG VALUES', &   
             ' IN KAIN-FRITSCH'                                             
              ENDIF                                                         
            ENDIF                                                          
            QG(NK)=1.E-9                                                  
            QG(NK1)=TMA*EMSD(NK1)                                        
            QG(NK-1)=TMB*EMSD(NK-1)                                     
          ENDIF                                                           
  499   CONTINUE                                                         
        TOPOMG=(UDR(LTOP)-UER(LTOP))*DP(LTOP)*EMSD(LTOP)                
        IF(ABS(TOPOMG-OMG(LTOP)).GT.1.E-3)THEN                         
!       WRITE(98,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;'      
!    * ,'TOPOMG, OMG =',TOPOMG,OMG(LTOP)                             
        WRITE(6,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;'        & 
       ,'TOPOMG, OMG =',TOPOMG,OMG(LTOP)                            
          ISTOP=1                                                  
          GOTO 265                                                
        ENDIF                                                    
!                                                               
!...CONVERT THETA TO T...                                      
!                                                             
! PAY ATTENTION ...
!
        DO 230 NK=1,LTOP                                     
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QG(NK)))   
          TG(NK)=THTAG(NK)/EXN(NK)                         
          TVG(NK)=TG(NK)*(1.+0.608*QG(NK))                
  230   CONTINUE                                         
!                                                       
!*******************************************************************    
!                                                                  *   
!     COMPUTE NEW CLOUD AND CHANGE IN AVAILABLE BUOYANT ENERGY.    *  
!                                                                  * 
!*******************************************************************
!                                                                  
!...THE FOLLOWING COMPUTATIONS ARE SIMILAR TO THAT FOR UPDRAFT    
!                                                                
        THMIX=0.                                                
        QMIX=0.                                                
        PMIX=0.                                               
        DO 217 NK=LC,KPBL                                    
          ROCPQ=0.2854*(1.-0.28*QG(NK))                     
          THMIX=THMIX+DP(NK)*TG(NK)*(P00/P0(NK))**ROCPQ    
          QMIX=QMIX+DP(NK)*QG(NK)                         
  217   PMIX=PMIX+DP(NK)*P0(NK)                          
        THMIX=THMIX/DPTHMX                              
        QMIX=QMIX/DPTHMX                              
        PMIX=PMIX/DPTHMX                             
        ROCPQ=0.2854*(1.-0.28*QMIX)                 
        TMIX=THMIX*(PMIX/P00)**ROCPQ                
        ES=ALIQ*EXP((TMIX*BLIQ-CLIQ)/(TMIX-DLIQ))                           
        QS=EP2*ES/(PMIX-ES)                                              
!                                                                         
!...REMOVE SUPERSATURATION FOR DIAGNOSTIC PURPOSES, IF NECESSARY...      
!                                                                       
        IF(QMIX.GT.QS)THEN                                             
          RL=XLV0-XLV1*TMIX                                          
          CPM=CP*(1.+0.887*QMIX)                                    
          DSSDT=QS*(CLIQ-BLIQ*DLIQ)/((TMIX-DLIQ)*(TMIX-DLIQ))      
          DQ=(QMIX-QS)/(1.+RL*DSSDT/CPM)                          
          TMIX=TMIX+RL/CP*DQ                                     
          QMIX=QMIX-DQ                                          
          ROCPQ=0.2854*(1.-0.28*QMIX)                          
          THMIX=TMIX*(P00/PMIX)**ROCPQ                        
          TLCL=TMIX                                          
          PLCL=PMIX                                         
        ELSE                                               
          QMIX=AMAX1(QMIX,0.)                             
          EMIX=QMIX*PMIX/(EP2+QMIX)                    
          TLOG=ALOG(EMIX/ALIQ)                          
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)            
          TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-  & 
               TDPT)                                                      
          TLCL=AMIN1(TLCL,TMIX)                                          
          CPORQ=1./ROCPQ                                                
          PLCL=P00*(TLCL/THMIX)**CPORQ                                 
        ENDIF                                                         
        TVLCL=TLCL*(1.+0.608*QMIX)                                   
        DO 235 NK=LC,KL                                             
          KLCL=NK                                                 
  235   IF(PLCL.GE.P0(NK))GOTO 240                               
  240   K=KLCL-1                                                
        DLP=ALOG(PLCL/P0(K))/ALOG(P0(KLCL)/P0(K))              
!                                                             
!...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...
!                                                                          
        TENV=TG(K)+(TG(KLCL)-TG(K))*DLP                                   
        QENV=QG(K)+(QG(KLCL)-QG(K))*DLP                                  
        TVEN=TENV*(1.+0.608*QENV)                                       
        TVBAR=0.5*(TVG(K)+TVEN)                                        
!        ZLCL=Z0(K)+R*TVBAR*ALOG(P0(K)/PLCL)/G                        
        ZLCL=Z0(K)+(Z0(KLCL)-Z0(K))*DLP                                     
        TVAVG=0.5*(TVEN+TG(KLCL)*(1.+0.608*QG(KLCL)))                      
        PLCL=P0(KLCL)*EXP(G/(R*TVAVG)*(Z0(KLCL)-ZLCL))                    
        THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*            & 
                  EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))      
        ES=ALIQ*EXP((TENV*BLIQ-CLIQ)/(TENV-DLIQ))                        
        QESE=EP2*ES/(PLCL-ES)                                              
        THTESG(K)=TENV*(1.E5/PLCL)**(0.2854*(1.-0.28*QESE))*            &
                  EXP((3374.6525/TENV-2.5403)*QESE*(1.+0.81*QESE))       
!                                                                           
!...COMPUTE ADJUSTED ABE(ABEG).                                            
!                                                                         
        ABEG=0.                                                         
        THTUDL=THETEU(K)                                               
        DO 245 NK=K,LTOPM1                                            
          NK1=NK+1                                                   
          ES=ALIQ*EXP((TG(NK1)*BLIQ-CLIQ)/(TG(NK1)-DLIQ))                    
          QESE=EP2*ES/(P0(NK1)-ES)                                        
          THTESG(NK1)=TG(NK1)*(1.E5/P0(NK1))**(0.2854*(1.-0.28*QESE))*    & 
                      EXP((3374.6525/TG(NK1)-2.5403)*QESE*(1.+0.81*QESE)  & 
                      )                                                     
!         DZZ=CVMGT(Z0(KLCL)-ZLCL,DZA(NK),NK.EQ.K)                         
          IF(NK.EQ.K)THEN                                                 
            DZZ=Z0(KLCL)-ZLCL                                            
          ELSE                                                          
            DZZ=DZA(NK)                                                
          ENDIF                                                       
          BE=((2.*THTUDL)/(THTESG(NK1)+THTESG(NK))-1.)*DZZ           
  245   IF(BE.GT.0.)ABEG=ABEG+BE*G                                  
!                                                                  
!...ASSUME AT LEAST 90% OF CAPE (ABE) IS REMOVED BY CONVECTION DURING     
!...THE PERIOD TIMEC...                                                  
!                                                                       
        IF(NOITR.EQ.1)THEN                                             
!        WRITE(98,1060)FABE                                           
          GOTO 265                                                   
        ENDIF                                                       
        DABE=AMAX1(ABE-ABEG,0.1*ABE)                               
        FABE=ABEG/(ABE+1.E-8)                                    
        IF(FABE.GT.1.)THEN                                                
!       WRITE(98,*)'UPDRAFT/DOWNDRAFT COUPLET INCREASES CAPE AT THIS '   
!    *,'GRID POINT; NO CONVECTION ALLOWED!'                             
          GOTO 325                                                     
        ENDIF                                                         
        IF(NCOUNT.NE.1)THEN                                          
          DFDA=(FABE-FABEOLD)/(AINC-AINCOLD)                        
          IF(DFDA.GT.0.)THEN                                       
            NOITR=1                                               
            AINC=AINCOLD                                         
            GOTO 255                                            
          ENDIF                                                
        ENDIF                                                 
        AINCOLD=AINC                                         
        FABEOLD=FABE                                        
        IF(AINC/AINCMX.GT.0.999.AND.FABE.GT.1.05-STAB)THEN 
!      WRITE(98,1055)FABE                                             
          GOTO 265                                                   
        ENDIF                                                       
        IF(FABE.LE.1.05-STAB.AND.FABE.GE.0.95-STAB)GOTO 265        
        IF(NCOUNT.GT.10)THEN                                      
!       WRITE(98,1060)FABE                                       
          GOTO 265                                              
        ENDIF                                                  
!                                                                             
!...IF MORE THAN 10% OF THE ORIGINAL CAPE REMAINS, INCREASE THE              
!...CONVECTIVE MASS FLUX BY THE FACTOR AINC:                                
!                                                                          
        IF(FABE.EQ.0.)THEN                                               
          AINC=AINC*0.5                                                 
        ELSE                                                           
          AINC=AINC*STAB*ABE/(DABE+1.E-8)                             
        ENDIF                                                        
  255   AINC=AMIN1(AINCMX,AINC)                                     
!...IF AINC BECOMES VERY SMALL, EFFECTS OF CONVECTION              
!...WILL BE MINIMAL SO JUST IGNORE IT...                          
        IF(AINC.LT.0.05)GOTO 325                                 
!       AINC=AMAX1(AINC,0.05)                                   
        TDER=TDER2*AINC                                        
        PPTFLX=PPTFL2*AINC                                    
!     WRITE(98,1080)LFS,LDB,LDT,TIMEC,NSTEP,NCOUNT,FABEOLD,AINCOLD     
        DO 260 NK=1,LTOP                                              
          UMF(NK)=UMF2(NK)*AINC                                      
          DMF(NK)=DMF2(NK)*AINC                                     
          DETLQ(NK)=DETLQ2(NK)*AINC                                
          DETIC(NK)=DETIC2(NK)*AINC                               
          UDR(NK)=UDR2(NK)*AINC                                  
          UER(NK)=UER2(NK)*AINC                                 
          DER(NK)=DER2(NK)*AINC                                
          DDR(NK)=DDR2(NK)*AINC                               
  260   CONTINUE                                             
!                                                           
!...GO BACK UP FOR ANOTHER ITERATION...                    
!                                                         
        GOTO 175                                         
  265   CONTINUE                                        

!kf_edrates
!Save up/down entrainment/detrainment rates as 3D variables
        IF (KF_EDRATES == 1) THEN
           DO NK=1,LTOP
             UDR_KF(I,NK,J)=UDR(NK)
             DDR_KF(I,NK,J)=DDR(NK)
             UER_KF(I,NK,J)=UER(NK)
             DER_KF(I,NK,J)=DER(NK)
           ENDDO
        ENDIF

!                                                      
!...CLEAN THINGS UP, CALCULATE CONVECTIVE FEEDBACK TENDENCIES FOR THIS    
!...GRID POINT...                                                        
!                                                                       
!...COMPUTE HYDROMETEOR TENDENCIES AS IS DONE FOR T, QV...             
!                                                                     
!...FRC2 IS THE FRACTION OF TOTAL CONDENSATE                         
!...GENERATED THAT GOES INTO PRECIPITIATION                         
        FRC2=PPTFLX/(CPR*AINC)                                     
        DO 270 NK=1,LTOP                                          
          QLPA(NK)=QL0(NK)                                       
          QIPA(NK)=QI0(NK)                                      
          QRPA(NK)=QR0(NK)                                     
          QSPA(NK)=QS0(NK)                                    
          RAINFB(NK)=PPTLIQ(NK)*AINC*FBFRC*FRC2                         
          SNOWFB(NK)=PPTICE(NK)*AINC*FBFRC*FRC2                        
  270   CONTINUE                                                      
        DO 290 NTC=1,NSTEP                                           
!                                                                   
!...ASSIGN HYDROMETEORS CONCENTRATIONS AT THE TOP AND BOTTOM OF EACH 
!...LAYER BASED ON THE SIGN OF OMEGA...                             
!                                                                  
          DO 275 NK=1,LTOP                                        
            QLFXIN(NK)=0.                                        
            QLFXOUT(NK)=0.                                      
            QIFXIN(NK)=0.                                      
            QIFXOUT(NK)=0.                                    
            QRFXIN(NK)=0.                                    
            QRFXOUT(NK)=0.                                  
            QSFXIN(NK)=0.                                  
            QSFXOUT(NK)=0.                               
  275     CONTINUE                                                     
          DO 280 NK=2,LTOP                                            
            IF(OMG(NK).LE.0.)THEN                                    
              QLFXIN(NK)=-FXM(NK)*QLPA(NK-1)                        
              QIFXIN(NK)=-FXM(NK)*QIPA(NK-1)                       
              QRFXIN(NK)=-FXM(NK)*QRPA(NK-1)                      
              QSFXIN(NK)=-FXM(NK)*QSPA(NK-1)                     
              QLFXOUT(NK-1)=QLFXOUT(NK-1)+QLFXIN(NK)            
              QIFXOUT(NK-1)=QIFXOUT(NK-1)+QIFXIN(NK)           
              QRFXOUT(NK-1)=QRFXOUT(NK-1)+QRFXIN(NK)          
              QSFXOUT(NK-1)=QSFXOUT(NK-1)+QSFXIN(NK)         
            ELSE                                            
              QLFXOUT(NK)=FXM(NK)*QLPA(NK)                 
              QIFXOUT(NK)=FXM(NK)*QIPA(NK)                
              QRFXOUT(NK)=FXM(NK)*QRPA(NK)               
              QSFXOUT(NK)=FXM(NK)*QSPA(NK)             
              QLFXIN(NK-1)=QLFXIN(NK-1)+QLFXOUT(NK)   
              QIFXIN(NK-1)=QIFXIN(NK-1)+QIFXOUT(NK)                      
              QRFXIN(NK-1)=QRFXIN(NK-1)+QRFXOUT(NK)                     
              QSFXIN(NK-1)=QSFXIN(NK-1)+QSFXOUT(NK)                    
            ENDIF                                                     
  280     CONTINUE                                                  
!                                                                  
!...UPDATE THE HYDROMETEOR CONCENTRATION VALUES AT EACH LEVEL...  
!                                                                
          DO 285 NK=1,LTOP                                                 
            QLPA(NK)=QLPA(NK)+(QLFXIN(NK)+DETLQ(NK)-QLFXOUT(NK))*DTIME*  & 
                     EMSD(NK)                                               
            QIPA(NK)=QIPA(NK)+(QIFXIN(NK)+DETIC(NK)-QIFXOUT(NK))*DTIME*  & 
                     EMSD(NK)                                             
            QRPA(NK)=QRPA(NK)+(QRFXIN(NK)+QLQOUT(NK)*UDR(NK)-QRFXOUT(NK) & 
                     +RAINFB(NK))*DTIME*EMSD(NK)                          
            QSPA(NK)=QSPA(NK)+(QSFXIN(NK)+QICOUT(NK)*UDR(NK)-QSFXOUT(NK) &  
                     +SNOWFB(NK))*DTIME*EMSD(NK)                           
  285     CONTINUE                                                        
  290   CONTINUE                                                         
        DO 295 NK=1,LTOP                                                
          QLG(NK)=QLPA(NK)                                             
          QIG(NK)=QIPA(NK)                                            
          QRG(NK)=QRPA(NK)                                           
          QSG(NK)=QSPA(NK)                                          
  295   CONTINUE                                                  

!kf_edrates
!Save convective timescale (TIMEC) as 2D variable
        IF (KF_EDRATES == 1) THEN
           TIMEC_KF(I,J)=TIMEC
        ENDIF

!     WRITE(98,1080)LFS,LDB,LDT,TIMEC,NSTEP,NCOUNT,FABE,AINC     
!                                                               
!...SEND FINAL PARAMETERIZED VALUES TO OUTPUT FILES...         
!                                                             
        IF(ISTOP.EQ.1)THEN                                   
        WRITE(6,1070)'  P  ','   DP ',' DT K/D ',' DR K/D ','   OMG  ',  & 
      ' DOMGDP ','   UMF  ','   UER  ','   UDR  ','   DMF  ','   DER  '  & 
      ,'   DDR  ','   EMS  ','    W0  ','  DETLQ ',' DETIC '              
          DO 300 K=LTOP,1,-1                                                   
            DTT=(TG(K)-T0(K))*86400./TIMEC                                 
            RL=XLV0-XLV1*TG(K)                                            
            DR=-(QG(K)-Q0(K))*RL*86400./(TIMEC*CP)                       
            UDFRC=UDR(K)*TIMEC*EMSD(K)                                  
            UEFRC=UER(K)*TIMEC*EMSD(K)                                 
            DDFRC=DDR(K)*TIMEC*EMSD(K)                                
            DEFRC=-DER(K)*TIMEC*EMSD(K)                              
            WRITE (6,1075)P0(K)/100.,DP(K)/100.,DTT,DR,OMG(K),DOMGDP(K)* &  
                          1.E4,UMF(K)/1.E6,UEFRC,UDFRC,DMF(K)/1.E6,DEFRC & 
                          ,DDFRC,EMS(K)/1.E11,W0AVG1D(K)*1.E2,DETLQ(K) &
                          *TIMEC*EMSD(K)*1.E3,DETIC(K)*TIMEC*EMSD(K)*    & 
                          1.E3                                            
  300     CONTINUE                                                       
        WRITE(6,1085)'K','P','Z','T0','TG','DT','TU','TD','Q0','QG',     & 
                  'DQ','QU','QD','QLG','QIG','QRG','QSG','RH0','RHG'    
          DO 305 K=KX,1,-1                                               
            DTT=TG(K)-T0(K)                                          
            TUC=TU(K)-T00                                                    
            IF(K.LT.LC.OR.K.GT.LTOP)TUC=0.                                  
            TDC=TZ(K)-T00                                                  
            IF((K.LT.LDB.OR.K.GT.LDT).AND.K.NE.LFS)TDC=0.                 
            ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))                  
            QGS=ES*EP2/(P0(K)-ES)                                     
            RH0=Q0(K)/QES(K)                                              
            RHG=QG(K)/QGS                                                
            WRITE (6,1090)K,P0(K)/100.,Z0(K),T0(K)-T00,TG(K)-T00,DTT,TUC &
                          ,TDC,Q0(K)*1000.,QG(K)*1000.,(QG(K)-Q0(K))*    &   
                          1000.,QU(K)*1000.,QD(K)*1000.,QLG(K)*1000.,    &  
                          QIG(K)*1000.,QRG(K)*1000.,QSG(K)*1000.,RH0,RHG   
  305     CONTINUE                                                        
!                                                                        
!...IF CALCULATIONS ABOVE SHOW AN ERROR IN THE MASS BUDGET, PRINT OUT A 
!...TO BE USED LATER FOR DIAGNOSTIC PURPOSES, THEN ABORT RUN...        
!                                                                     
          IF(ISTOP.EQ.1)THEN                                         
            DO 310 K=1,KX                                           
              WRITE ( wrf_err_message , 1115 )                         &
                            Z0(K),P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,   &
                            U0(K),V0(K),DP(K)/100.,W0AVG1D(K)         
              CALL wrf_message ( TRIM( wrf_err_message ) )
  310       CONTINUE                                                   
            CALL wrf_error_fatal ( 'module_cu_kf.F: KAIN-FRITSCH' )
          ENDIF                                                      
        ENDIF                                                       
        CNDTNF=(1.-EQFRC(LFS))*(QLIQ(LFS)+QICE(LFS))*DMF(LFS)            
!     WRITE(98,1095)CPR*AINC,TDER+PPTFLX+CNDTNF                            
!                                                                       
!  EVALUATE MOISTURE BUDGET...                                         
!                                                                     
        QINIT=0.                                                     
        QFNL=0.                                                     
        DPT=0.                                                     
        DO 315 NK=1,LTOP                                                    
          DPT=DPT+DP(NK)                                                     
          QINIT=QINIT+Q0(NK)*EMS(NK)                                       
          QFNL=QFNL+QG(NK)*EMS(NK)                                        
          QFNL=QFNL+(QLG(NK)+QIG(NK)+QRG(NK)+QSG(NK))*EMS(NK)            
  315   CONTINUE                                                        
        QFNL=QFNL+PPTFLX*TIMEC*(1.-FBFRC)                              
        ERR2=(QFNL-QINIT)*100./QINIT                                  
!     WRITE(98,1110)QINIT,QFNL,ERR2                                  
!        IF(ABS(ERR2).GT.0.05)STOP 'QVERR'                           
        IF(ABS(ERR2).GT.0.05)CALL wrf_error_fatal( 'module_cu_kf.F: QVERR' )
        RELERR=ERR2*QINIT/(PPTFLX*TIMEC+1.E-10)                   
!     WRITE(98,1120)RELERR                                       
!     WRITE(98,*)'TDER, CPR, USR, TRPPT =',                     
!    *TDER,CPR*AINC,USR*AINC,TRPPT*AINC                        
!                                                             
!...FEEDBACK TO RESOLVABLE SCALE TENDENCIES.                 
!                                                           
!...IF THE ADVECTIVE TIME PERIOD (TADVEC) IS LESS THAN SPECIFIED MINIMUM  
!...TIMEC, ALLOW FEEDBACK TO OCCUR ONLY DURING TADVEC...                 
!                                                                       
        IF(TADVEC.LT.TIMEC)NIC=NINT(TADVEC/DT)                  
        NCA(I,J)=FLOAT(NIC)*DT
        DO 320 K=1,KX                                                
!         IF(IMOIST.NE.2)THEN
!                                                                            
!...IF HYDROMETEORS ARE NOT ALLOWED, THEY MUST BE EVAPORATED OR SUBLIMAT    
!...AND FED BACK AS VAPOR, ALONG WITH ASSOCIATED CHANGES IN TEMPERATURE.   
!...NOTE:  THIS WILL INTRODUCE CHANGES IN THE CONVECTIVE TEMPERATURE AND  
!...WATER VAPOR FEEDBACK TENDENCIES AND MAY LEAD TO SUPERSATURATED VALUE 
!...OF QG...                                                           
!                                                                     
!           RLC=XLV0-XLV1*TG(K)                                      
!           RLS=XLS0-XLS1*TG(K)                                     
!           CPM=CP*(1.+0.887*QG(K))                                
!           TG(K)=TG(K)-(RLC*(QLG(K)+QRG(K))+RLS*(QIG(K)+QSG(K)))/CPM    
!           QG(K)=QG(K)+(QLG(K)+QRG(K)+QIG(K)+QSG(K))                   
!           DQCDT(K)=0.                                           
!           DQIDT(K)=0.                                          
!           DQRDT(K)=0.                                         
!           DQSDT(K)=0.                                        
!         ELSE                                                     
            IF(.NOT. qi_flag .and. warm_rain)THEN                                          
!                                                                         
!...IF ICE PHASE IS NOT ALLOWED, MELT ALL FROZEN HYDROMETEORS...         
!                                                                       
              CPM=CP*(1.+0.887*QG(K))                                  
              TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM                     
              DQCDT(K)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC      
              DQIDT(K)=0.                                      
              DQRDT(K)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC    
              DQSDT(K)=0.                                    
            ELSEIF(.NOT. qi_flag .and. .not. warm_rain)THEN                                          
!                                                                      
!...IF ICE PHASE IS ALLOWED, BUT MIXED PHASE IS NOT, MELT FROZEN HYDROME   
!...BELOW THE MELTING LEVEL, FREEZE LIQUID WATER ABOVE THE MELTING LEVEL  
!                                                                        
              CPM=CP*(1.+0.887*QG(K))                                   
              IF(K.LE.ML)THEN                                         
                TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM                  
              ELSEIF(K.GT.ML)THEN                                              
                TG(K)=TG(K)+(QLG(K)+QRG(K))*RLF/CPM                           
              ENDIF                                                          
              DQCDT(K)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC             
              DQIDT(K)=0.                                             
              DQRDT(K)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC           
              DQSDT(K)=0.                                           
            ELSEIF(qi_flag) THEN
!                                                                           
!...IF MIXED PHASE HYDROMETEORS ARE ALLOWED, FEED BACK CONVECTIVE          
!...TENDENCY OF HYDROMETEORS DIRECTLY...                                  
!                                                                        
              DQCDT(K)=(QLG(K)-QL0(K))/TIMEC                       
              DQIDT(K)=(QIG(K)-QI0(K))/TIMEC                      
              DQRDT(K)=(QRG(K)-QR0(K))/TIMEC                     
              IF (qs_flag ) THEN
                 DQSDT(K)=(QSG(K)-QS0(K))/TIMEC                    
              ELSE
                 DQIDT(K)=DQIDT(K)+(QSG(K)-QS0(K))/TIMEC
              ENDIF
            ELSE                                                    
              CALL wrf_error_fatal ( 'module_cu_kf: THIS COMBINATION OF IMOIST,  IICE NOT ALLOWED' )
            ENDIF                                                 
!         ENDIF                                                  
          DTDT(K)=(TG(K)-T0(K))/TIMEC                      
          DQDT(K)=(QG(K)-Q0(K))/TIMEC                                   
  320   CONTINUE                                                            

! RAINCV is in the unit of mm

        PRATEC(I,J)=PPTFLX*(1.-FBFRC)/DXSQ                       
        RAINCV(I,J)=DT*PRATEC(I,J)
        RNC=RAINCV(I,J)*NIC                                              
!        WRITE(98,909)RNC                                               
 909     FORMAT(' CONVECTIVE RAINFALL =',F8.4,' CM')                   

  325 CONTINUE                                                        

1000  FORMAT(' ',10A8)                                                     
1005  FORMAT(' ',F6.0,2X,F6.4,2X,F7.3,1X,F6.4,2X,4(F6.3,2X),2(F7.3,1X))   
1010  FORMAT(' ',' VERTICAL VELOCITY IS NEGATIVE AT ',F4.0,' MB')        
1015   FORMAT(' ','ALL REMAINING MASS DETRAINS BELOW ',F4.0,' MB')          
1025   FORMAT(5X,' KLCL=',I2,' ZLCL=',F7.1,'M',                          &
        ' DTLCL=',F5.2,' LTOP=',I2,' P0(LTOP)=',-2PF5.1,'MB FRZ LV=',    & 
        I2,' TMIX=',0PF4.1,1X,'PMIX=',-2PF6.1,' QMIX=',3PF5.1,           &
        ' CAPE=',0PF7.1)                                                    
1030   FORMAT(' ',' P0(LET) = ',F6.1,' P0(LTOP) = ',F6.1,' VMFLCL =',    &   
      E12.3,' PLCL =',F6.1,' WLCL =',F6.3,' CLDHGT =',                   & 
      F8.1)                                                               
1035  FORMAT(1X,'PEF(WS)=',F4.2,'(CB)=',F4.2,'LC,LET=',2I3,'WKL='        &
      ,F6.3,'VWS=',F5.2)                                                    
1040          FORMAT(' ','PRECIP EFF = 100%, ENVIR CANNOT SUPPORT DOWND' & 
      ,'RAFTS')                                                          
!1045  FORMAT('NUMBER OF DOWNDRAFT ITERATIONS EXCEEDS 10...PPTFLX'       & 
!      ' IS DIFFERENT FROM THAT GIVEN BY PRECIP EFF RELATION')            
! FLIC HAS TROUBLE WITH THIS ONE.                                         
1045  FORMAT('NUMBER OF DOWNDRAFT ITERATIONS EXCEEDS 10')                
1050  FORMAT(' ','LCOUNT= ',I3,' PPTFLX/CPR, PEFF= ',F5.3,1X,F5.3,       &  
      'DMF(LFS)/UMF(LCL)= ',F5.3)                                          
1055     FORMAT(/'*** DEGREE OF STABILIZATION =',F5.3,', NO MORE MASS F' &
      ,'LUX IS ALLOWED')                                                
!1060     FORMAT(/' ITERATION DOES NOT CONVERGE TO GIVE THE SPECIFIED '  & 
!      'DEGREE OF STABILIZATION!  FABE= ',F6.4)                             
1060  FORMAT(/' ITERATION DOES NOT CONVERGE.  FABE= ',F6.4)                
 1070 FORMAT (16A8)                                                       
 1075 FORMAT (F8.2,3(F8.2),2(F8.3),F8.2,2F8.3,F8.2,6F8.3)                
1080   FORMAT(2X,'LFS,LDB,LDT =',3I3,' TIMEC, NSTEP=',F5.0,I3,           & 
      'NCOUNT, FABE, AINC=',I2,1X,F5.3,F6.2)                              
 1085 FORMAT (A3,16A7,2A8)                                               
 1090 FORMAT (I3,F7.2,F7.0,10F7.2,4F7.3,2F8.3)                          
1095   FORMAT(' ','  PPT PRODUCTION RATE= ',F10.0,' TOTAL EVAP+PPT= ',   &  
       F10.0)                                                           
1105   FORMAT(' ','NET LATENT HEAT RELEASE =',E12.5,' ACTUAL HEATING =', & 
       E12.5,' J/KG-S, DIFFERENCE = ',F9.3,'PERCENT')                    
1110   FORMAT(' ','INITIAL WATER =',E12.5,' FINAL WATER =',E12.5,        & 
       ' TOTAL WATER CHANGE =',F8.2,'PERCENT')                              
 1115 FORMAT (2X,F6.0,2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4 &  
             )                                                            
1120   FORMAT(' ','MOISTURE ERROR AS FUNCTION OF TOTAL PPT =',F9.3,      & 
            'PERCENT')                                                  

   END SUBROUTINE KFPARA 

!-----------------------------------------------------------------------
   SUBROUTINE CONDLOAD(QLIQ,QICE,WTW,DZ,BOTERM,ENTERM,RATE,QNEWLQ,     & 
                       QNEWIC,QLQOUT,QICOUT,G)                           
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
!  9/18/88...THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME US    
!  BY OGURA AND CHO (1973).  LIQUID WATER FALLOUT FROM A PARCEL IS CAL-    
!  CULATED USING THE EQUATION DQ=-RATE*Q*DT, BUT TO SIMULATE A QUASI-     
!  CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON VERTICAL         
!  RESOLUTION THIS IS EXPRESSED AS Q=Q*EXP(-RATE*DZ).                   

      REAL, INTENT(IN   )   :: G
      REAL, INTENT(IN   )   :: DZ,BOTERM,ENTERM,RATE
      REAL, INTENT(INOUT)   :: QLQOUT,QICOUT,WTW,QLIQ,QICE,QNEWLQ,QNEWIC

      REAL :: QTOT,QNEW,QEST,G1,WAVG,CONV,RATIO3,OLDQ,RATIO4,DQ,PPTDRG

      QTOT=QLIQ+QICE                                                  
      QNEW=QNEWLQ+QNEWIC                                            
!                                                                  
!  ESTIMATE THE VERTICAL VELOCITY SO THAT AN AVERAGE VERTICAL VELOCITY C
!  BE CALCULATED TO ESTIMATE THE TIME REQUIRED FOR ASCENT BETWEEN MODEL
!  LEVELS...                                                          
!                                                                    
      QEST=0.5*(QTOT+QNEW)                                          
      G1=WTW+BOTERM-ENTERM-2.*G*DZ*QEST/1.5                        
      IF(G1.LT.0.0)G1=0.                                          
      WAVG=(SQRT(WTW)+SQRT(G1))/2.                               
      CONV=RATE*DZ/WAVG                                         
!                                                              
!  RATIO3 IS THE FRACTION OF LIQUID WATER IN FRESH CONDENSATE, RATIO4 IS  
!  THE FRACTION OF LIQUID WATER IN THE TOTAL AMOUNT OF CONDENSATE INVOLV 
!  IN THE PRECIPITATION PROCESS - NOTE THAT ONLY 60% OF THE FRESH CONDEN
!  SATE IS IS ALLOWED TO PARTICIPATE IN THE CONVERSION PROCESS...      
!                                                                     
      RATIO3=QNEWLQ/(QNEW+1.E-10)                                    
!     OLDQ=QTOT                                                     
      QTOT=QTOT+0.6*QNEW                                           
      OLDQ=QTOT                                                   
      RATIO4=(0.6*QNEWLQ+QLIQ)/(QTOT+1.E-10)                     
      QTOT=QTOT*EXP(-CONV)                                      
!                                                              
!  DETERMINE THE AMOUNT OF PRECIPITATION THAT FALLS OUT OF THE UPDRAFT   
!  PARCEL AT THIS LEVEL...                                              
!                                                                      
      DQ=OLDQ-QTOT                                                    
      QLQOUT=RATIO4*DQ                                               
      QICOUT=(1.-RATIO4)*DQ                                         
!                                                                  
!  ESTIMATE THE MEAN LOAD OF CONDENSATE ON THE UPDRAFT IN THE LAYER, CAL 
!  LATE VERTICAL VELOCITY                                               
!                                                                      
      PPTDRG=0.5*(OLDQ+QTOT-0.2*QNEW)                                 
      WTW=WTW+BOTERM-ENTERM-2.*G*DZ*PPTDRG/1.5                       
!                                                                   
!  DETERMINE THE NEW LIQUID WATER AND ICE CONCENTRATIONS INCLUDING LOSSE  
!  DUE TO PRECIPITATION AND GAINS FROM CONDENSATION...                   
!                                                                       
      QLIQ=RATIO4*QTOT+RATIO3*0.4*QNEW                                 
      QICE=(1.-RATIO4)*QTOT+(1.-RATIO3)*0.4*QNEW                      
      QNEWLQ=0.                                                      
      QNEWIC=0.                                                     

   END SUBROUTINE CONDLOAD

!-----------------------------------------------------------------------
   SUBROUTINE DTFRZNEW(TU,P,THTEU,QVAP,QLIQ,QICE,RATIO2,TTFRZ,TBFRZ,   & 
                       QNWFRZ,RL,FRC1,EFFQ,IFLAG,XLV0,XLV1,XLS0,XLS1,  & 
                       EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE     )        
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: XLV0,XLV1
   REAL,         INTENT(IN   )   :: P,TTFRZ,TBFRZ,EFFQ,XLS0,XLS1,EP2,ALIQ, &
                                    BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE
   REAL,         INTENT(INOUT)   :: TU,THTEU,QVAP,QLIQ,QICE,RATIO2,    &
                                    FRC1,RL,QNWFRZ
   INTEGER,      INTENT(INOUT)   :: IFLAG

   REAL ::       CCP,RV,C5,QLQFRZ,QNEW,ESLIQ,ESICE,RLC,RLS,PI,ES,RLF,A, &
                 B,C,DQVAP,DTFRZ,TU1,QVAP1
!-----------------------------------------------------------------------
!                                                                          
!...ALLOW GLACIATION OF THE UPDRAFT TO OCCUR AS AN APPROXIMATELY LINEAR   
!   FUNCTION OF TEMPERATURE IN THE TEMPERATURE RANGE TTFRZ TO TBFRZ...   
!                                                                       

      RV=461.5                                                         
      C5=1.0723E-3                                                    
!                                                                          
!...ADJUST THE LIQUID WATER CONCENTRATIONS FROM FRESH CONDENSATE AND THA  
!   BROUGHT UP FROM LOWER LEVELS TO AN AMOUNT THAT WOULD BE PRESENT IF N 
!   LIQUID WATER HAD FROZEN THUS FAR...THIS IS NECESSARY BECAUSE THE    
!   EXPRESSION FOR TEMP CHANGE IS MULTIPLIED BY THE FRACTION EQUAL TO TH       
!   PARCEL TEMP DECREASE SINCE THE LAST MODEL LEVEL DIVIDED BY THE TOTAL      
!   GLACIATION INTERVAL, SO THAT EFFECTIVELY THIS APPROXIMATELY ALLOWS A     
!   AMOUNT OF LIQUID WATER TO FREEZE WHICH IS EQUAL TO THIS SAME FRACTIO    
!   OF THE LIQUID WATER THAT WAS PRESENT BEFORE THE GLACIATION PROCESS W   
!   INITIATED...ALSO, TO ALLOW THETAU TO CONVERT APPROXIMATELY LINEARLY   
!   ITS VALUE WITH RESPECT TO ICE, WE NEED TO ALLOW A PORTION OF THE FRE 
!   CONDENSATE TO CONTRIBUTE TO THE GLACIATION PROCESS; THE FRACTIONAL  
!   AMOUNT THAT APPLIES TO THIS PORTION IS 1/2 OF THE FRACTIONAL AMOUNT     
!   FROZEN OF THE "OLD" CONDENSATE BECAUSE THIS FRESH CONDENSATE IS ONLY   
!   PRODUCED GRADUALLY OVER THE LAYER...NOTE THAT IN TERMS OF THE DYNAMI  
!   OF THE PRECIPITATION PROCESS, IE. PRECIPITATION FALLOUT, THIS FRACTI 
!   AMNT OF FRESH CONDENSATE HAS ALREADY BEEN INCLUDED IN THE ICE CATEGO
!                                                                      
      QLQFRZ=QLIQ*EFFQ                                                
      QNEW=QNWFRZ*EFFQ*0.5                                           
      ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                      
      ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))                     
      RLC=2.5E6-2369.276*(TU-273.16)                              
      RLS=2833922.-259.532*(TU-273.16)                           
      RLF=RLS-RLC                                               
      CCP=1005.7*(1.+0.89*QVAP)                                 
!                                                             
!  A = D(ES)/DT IS THAT CALCULATED FROM BUCK`S (1981) EMPIRICAL FORMULAS
!  FOR SATURATION VAPOR PRESSURE...                                    
!                                                                     
      A=(CICE-BICE*DICE)/((TU-DICE)*(TU-DICE))                       
      B=RLS*EP2/P                                                 
      C=A*B*ESICE/CCP                                               
      DQVAP=B*(ESLIQ-ESICE)/(RLS+RLS*C)-RLF*(QLQFRZ+QNEW)/(RLS+RLS/C)  
      DTFRZ=(RLF*(QLQFRZ+QNEW)+B*(ESLIQ-ESICE))/(CCP+A*B*ESICE)        
      TU1=TU                                                         
      QVAP1=QVAP                                                    
      TU=TU+FRC1*DTFRZ                                             
      QVAP=QVAP-FRC1*DQVAP                                        
      ES=QVAP*P/(EP2+QVAP)                                     
      ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                           
      ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))                          
      RATIO2=(ESLIQ-ES)/(ESLIQ-ESICE)                                  
!                                                                     
!  TYPICALLY, RATIO2 IS VERY CLOSE TO (TTFRZ-TU)/(TTFRZ-TBFRZ), USUALLY    
!  WITHIN 1% (USING TU BEFORE GALCIATION EFFECTS ARE APPLIED);  IF THE    
!  INITIAL UPDRAFT TEMP IS BELOW TBFRZ AND RATIO2 IS STILL LESS THAN 1,  
!  AN ADJUSTMENT TO FRC1 AND RATIO2 IS INTRODUCED SO THAT GLACIATION    
!  EFFECTS ARE NOT UNDERESTIMATED; CONVERSELY, IF RATIO2 IS GREATER THAN    
!  FRC1 IS ADJUSTED SO THAT GLACIATION EFFECTS ARE NOT OVERESTIMATED...    
!                                                                        
      IF(IFLAG.GT.0.AND.RATIO2.LT.1)THEN                                
        FRC1=FRC1+(1.-RATIO2)                                          
        TU=TU1+FRC1*DTFRZ                                             
        QVAP=QVAP1-FRC1*DQVAP                                        
        RATIO2=1.                                                   
        IFLAG=1                                                    
        GOTO 20                                                   
      ENDIF                                                                  
      IF(RATIO2.GT.1.)THEN                                                  
        FRC1=FRC1-(RATIO2-1.)                                              
        FRC1=AMAX1(0.0,FRC1)                                              
        TU=TU1+FRC1*DTFRZ                                                
        QVAP=QVAP1-FRC1*DQVAP                                           
        RATIO2=1.                                                      
        IFLAG=1                                                       
      ENDIF                                                                   
!                                                                            
!  CALCULATE A HYBRID VALUE OF THETAU, ASSUMING THAT THE LATENT HEAT OF     
!  VAPORIZATION/SUBLIMATION CAN BE ESTIMATED USING THE SAME WEIGHTING      
!  FUNCTION AS THAT USED TO CALCULATE SATURATION VAPOR PRESSURE, CALCU-   
!  LATE NEW LIQUID WATER AND ICE CONCENTRATIONS...                       
!                                                                       
   20 RLC=XLV0-XLV1*TU                                                 
      RLS=XLS0-XLS1*TU                                                
      RL=RATIO2*RLS+(1.-RATIO2)*RLC                                  
      PI=(1.E5/P)**(0.2854*(1.-0.28*QVAP))                          
      THTEU=TU*PI*EXP(RL*QVAP*C5/TU*(1.+0.81*QVAP))               
      IF(IFLAG.EQ.1)THEN                                           
        QICE=QICE+FRC1*DQVAP+QLIQ                                
        QLIQ=0.                                                             
      ELSE                                                                   
        QICE=QICE+FRC1*(DQVAP+QLQFRZ)                                      
        QLIQ=QLIQ-FRC1*QLQFRZ                                             
      ENDIF                                                              
      QNWFRZ=0.                                                         
                                                                    
   END SUBROUTINE DTFRZNEW

!-----------------------------------------------------------------------
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
!  THIS SUBROUTINE INTEGRATES THE AREA UNDER THE CURVE IN THE GAUSSIAN     
!  DISTRIBUTION...THE NUMERICAL APPROXIMATION TO THE INTEGRAL IS TAKEN F  
!   HANDBOOK OF MATHEMATICAL FUNCTIONS WITH FORMULAS, GRAPHS AND MATHEMA 
!  TABLES  ED. BY ABRAMOWITZ AND STEGUN, NAT L BUREAU OF STANDARDS APPLI
!  MATHEMATICS SERIES.  JUNE, 1964., MAY, 1968.                        
!                                     JACK KAIN                       
!                                     7/6/89                         
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
!***********************************************************************    
!*****    GAUSSIAN TYPE MIXING PROFILE....******************************   
   SUBROUTINE PROF5(EQ,EE,UD)                                          
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: EQ
   REAL,         INTENT(INOUT)   :: EE,UD
   REAL ::       SQRT2P,A1,A2,A3,P,SIGMA,FE,X,Y,EY,E45,T1,T2,C1,C2

      DATA SQRT2P,A1,A2,A3,P,SIGMA,FE/2.506628,0.4361836,-0.1201676,    & 
      0.9372980,0.33267,0.166666667,0.202765151/                        
      X=(EQ-0.5)/SIGMA                                                 
      Y=6.*EQ-3.                                                      
      EY=EXP(Y*Y/(-2))                                               
      E45=EXP(-4.5)                                                 
      T2=1./(1.+P*ABS(Y))                                          
      T1=0.500498                                                 
      C1=A1*T1+A2*T1*T1+A3*T1*T1*T1                              
      C2=A1*T2+A2*T2*T2+A3*T2*T2*T2                             
      IF(Y.GE.0.)THEN                                                    
        EE=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*EQ*EQ/2.
        UD=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*(0.5+EQ*EQ/2.-  &
           EQ)                                                         
      ELSE                                                            
        EE=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*EQ*EQ/2.    
        UD=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*(0.5+EQ* & 
           EQ/2.-EQ)                                                     
      ENDIF                                                             
      EE=EE/FE                                                         
      UD=UD/FE                                                        

   END SUBROUTINE PROF5

!-----------------------------------------------------------------------
   SUBROUTINE TPMIX(P,THTU,TU,QU,QLIQ,QICE,QNEWLQ,QNEWIC,RATIO2,RL,    & 
                    XLV0,XLV1,XLS0,XLS1,                               &
                    EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE        )      
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: XLV0,XLV1
   REAL,         INTENT(IN   )   :: P,THTU,RATIO2,RL,XLS0,             &
                                    XLS1,EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,&
                                    CICE,DICE
   REAL,         INTENT(INOUT)   :: QU,QLIQ,QICE,TU,QNEWLQ,QNEWIC
   REAL    ::    ES,QS,PI,THTGS,F0,T1,T0,C5,RV,ESLIQ,ESICE,F1,DT,QNEW, &
                 DQ, QTOT,DQICE,DQLIQ,RLL,CCP
   INTEGER ::    ITCNT
!-----------------------------------------------------------------------
!                                                                       
!...THIS SUBROUTINE ITERATIVELY EXTRACTS WET-BULB TEMPERATURE FROM EQUIV
!   POTENTIAL TEMPERATURE, THEN CHECKS TO SEE IF SUFFICIENT MOISTURE IS
!   AVAILABLE TO ACHIEVE SATURATION...IF NOT, TEMPERATURE IS ADJUSTED 
!   ACCORDINGLY, IF SO, THE RESIDUAL LIQUID WATER/ICE CONCENTRATION IS
!   DETERMINED...                                                    
!                                                                   
      C5=1.0723E-3                                                 
      RV=461.5                                                    
!                                                                
!   ITERATE TO FIND WET BULB TEMPERATURE AS A FUNCTION OF EQUIVALENT POT 
!   TEMP AND PRS, ASSUMING SATURATION VAPOR PRESSURE...RATIO2 IS THE DEG
!   OF GLACIATION...                                                   
!                                                                     
      IF(RATIO2.LT.1.E-6)THEN                                        
        ES=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                       
        QS=EP2*ES/(P-ES)                                         
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                        
        THTGS=TU*PI*EXP((3374.6525/TU-2.5403)*QS*(1.+0.81*QS))   
      ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN                       
        ES=AICE*EXP((BICE*TU-CICE)/(TU-DICE))                  
        QS=EP2*ES/(P-ES)                                    
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                   
        THTGS=TU*PI*EXP((3114.834/TU-0.278296)*QS*(1.+0.81*QS))          
      ELSE                                                              
        ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))                       
        ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))                      
        ES=(1.-RATIO2)*ESLIQ+RATIO2*ESICE                            
        QS=EP2*ES/(P-ES)                                          
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                         
        THTGS=TU*PI*EXP(RL*QS*C5/TU*(1.+0.81*QS))                 
      ENDIF                                                      
      F0=THTGS-THTU                                             
      T1=TU-0.5*F0                                             
      T0=TU                                                   
      ITCNT=0                                                
   90 IF(RATIO2.LT.1.E-6)THEN                               
        ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))              
        QS=EP2*ES/(P-ES)                                 
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                 
        THTGS=T1*PI*EXP((3374.6525/T1-2.5403)*QS*(1.+0.81*QS))    
      ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN                        
        ES=AICE*EXP((BICE*T1-CICE)/(T1-DICE))                  
        QS=EP2*ES/(P-ES)                                    
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                   
        THTGS=T1*PI*EXP((3114.834/T1-0.278296)*QS*(1.+0.81*QS))     
      ELSE                                                         
        ESLIQ=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))                  
        ESICE=AICE*EXP((BICE*T1-CICE)/(T1-DICE))                 
        ES=(1.-RATIO2)*ESLIQ+RATIO2*ESICE                       
        QS=EP2*ES/(P-ES)                                     
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))                    
        THTGS=T1*PI*EXP(RL*QS*C5/T1*(1.+0.81*QS))            
      ENDIF                                                 
      F1=THTGS-THTU                                        
      IF(ABS(F1).LT.0.01)GOTO 50         
      ITCNT=ITCNT+1                                               
      IF(ITCNT.GT.10)GOTO 50                                     
      DT=F1*(T1-T0)/(F1-F0)                                     
      T0=T1                                                   
      F0=F1                                                  
      T1=T1-DT                                              
      GOTO 90                                              
!                                                         
!   IF THE PARCEL IS SUPERSATURATED, CALCULATE CONCENTRATION OF FRESH    
!   CONDENSATE...                                                       
!                                                                      
   50 IF(QS.LE.QU)THEN                                                
        QNEW=QU-QS                                                   
        QU=QS                                                       
        GOTO 96                                                          
      ENDIF                                                             
!                                                                      
!   IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE  
!   ADJUSTED...IF LIQUID WATER OR ICE IS PRESENT, IT IS ALLOWED TO EVAPO
!   SUBLIMATE.                                                         
!                                                                     
      QNEW=0.                                                        
      DQ=QS-QU                                                      
      QTOT=QLIQ+QICE                                               
!                                                                 
!   IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS   
!   WET BULB VALUE, VAPOR MIXING RATIO IS AT SATURATED LEVEL, AND THE MI 
!   RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE UP THE ORIGINAL SATURA
!   DEFICIT... OTHERWISE, ANY AVAILABLE LIQ OR ICE VAPORIZES AND APPROPR  
!   ADJUSTMENTS TO PARCEL TEMP; VAPOR, LIQUID, AND ICE MIXING RATIOS ARE 
!                                                                       
!...NOTE THAT THE LIQ AND ICE MAY BE PRESENT IN PROPORTIONS SLIGHTLY DIF 
!   THAN SUGGESTED BY THE VALUE OF RATIO2...CHECK TO MAKE SURE THAT LIQ 
!   ICE CONCENTRATIONS ARE NOT REDUCED TO BELOW ZERO WHEN EVAPORATION/
!   SUBLIMATION OCCURS...                                            
!                                                                   
      IF(QTOT.GE.DQ)THEN                                           
        DQICE=0.0                                                 
        DQLIQ=0.0                                                
        QLIQ=QLIQ-(1.-RATIO2)*DQ                                
        IF(QLIQ.LT.0.)THEN                                     
          DQICE=0.0-QLIQ                                      
          QLIQ=0.0                                                   
        ENDIF                                                       
        QICE=QICE-RATIO2*DQ+DQICE                                  
        IF(QICE.LT.0.)THEN                                        
          DQLIQ=0.0-QICE                                         
          QICE=0.0                                              
        ENDIF                                                  
        QLIQ=QLIQ+DQLIQ                                       
        QU=QS                                                
        GOTO 96                                             
      ELSE                                                  
        IF(RATIO2.LT.1.E-6)THEN                             
          RLL=XLV0-XLV1*T1                                             
        ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN                           
          RLL=XLS0-XLS1*T1                                           
        ELSE                                                        
          RLL=RL                                                   
        ENDIF                                                     
        CCP=1005.7*(1.+0.89*QU)                                            
        IF(QTOT.LT.1.E-10)THEN                                           
!                                                                       
!...IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:   
          T1=T1+RLL*(DQ/(1.+DQ))/CCP                                   
          GOTO 96                                                    
        ELSE                                                        
!                                                                  
!...IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE SATURA 
!   THE TEMPERATURE IS GIVEN BY:                                        
          T1=T1+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CCP                             
          QU=QU+QTOT                                                      
          QTOT=0.                                                        
        ENDIF                                                           
        QLIQ=0                                                         
        QICE=0.                                                       
      ENDIF                                                          
   96 TU=T1                                                             
      QNEWLQ=(1.-RATIO2)*QNEW                                          
      QNEWIC=RATIO2*QNEW                                             
      IF(ITCNT.GT.10)PRINT*,'***** NUMBER OF ITERATIONS IN TPMIX =',   & 
        ITCNT                                                       

   END SUBROUTINE TPMIX
!-----------------------------------------------------------------------
   SUBROUTINE ENVIRTHT(P1,T1,Q1,THT1,R1,RL,                            & 
                       EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE     )
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,  INTENT(IN   ) :: P1,T1,Q1,R1,RL,EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,&
                           BICE,CICE,DICE      
   REAL,  INTENT(INOUT) :: THT1
   REAL:: T00,P00,C1,C2,C3,C4,C5,EE,TLOG,TDPT,TSAT,THT,TFPT,TLOGIC,    &
          TSATLQ,TSATIC

      DATA T00,P00,C1,C2,C3,C4,C5/273.16,1.E5,3374.6525,2.5403,3114.834,&
           0.278296,1.0723E-3/                                       
!                                                                   
!  CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE...     
!                                                                 

      IF(R1.LT.1.E-6)THEN                                        
        EE=Q1*P1/(EP2+Q1)                                     
        TLOG=ALOG(EE/ALIQ)                                     
        TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                     
        TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT)
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                        
        THT1=THT*EXP((C1/TSAT-C2)*Q1*(1.+0.81*Q1))                   
      ELSEIF(ABS(R1-1.).LT.1.E-6)THEN                               
        EE=Q1*P1/(EP2+Q1)                                        
        TLOG=ALOG(EE/AICE)                                        
        TFPT=(CICE-DICE*TLOG)/(BICE-TLOG)                        
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                  
        TSAT=TFPT-(.182+1.13E-3*(TFPT-T00)-3.58E-4*(T1-T00))*(T1-TFPT)
        THT1=THT*EXP((C3/TSAT-C4)*Q1*(1.+0.81*Q1))                   
      ELSE                                                          
        EE=Q1*P1/(EP2+Q1)                                        
        TLOG=ALOG(EE/ALIQ)                                        
        TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                        
        TLOGIC=ALOG(EE/AICE)                                    
        TFPT=(CICE-DICE*TLOGIC)/(BICE-TLOGIC)                  
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                
        TSATLQ=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT)                                                       
        TSATIC=TFPT-(.182+1.13E-3*(TFPT-T00)-3.58E-4*(T1-T00))*(T1-TFPT) 
        TSAT=R1*TSATIC+(1.-R1)*TSATLQ                                   
        THT1=THT*EXP(RL*Q1*C5/TSAT*(1.+0.81*Q1))                       
      ENDIF                                                           

   END SUBROUTINE ENVIRTHT

!-----------------------------------------------------------------------
!************************* TPDD.FOR ************************************  
!   THIS SUBROUTINE ITERATIVELY EXTRACTS TEMPERATURE FROM EQUIVALENT   * 
!   POTENTIAL TEMP.  IT IS DESIGNED FOR USE WITH DOWNDRAFT CALCULATIONS.
!   IF RELATIVE HUMIDITY IS SPECIFIED TO BE LESS THAN 100%, PARCEL     * 
!   TEMP, SPECIFIC HUMIDITY, AND LIQUID WATER CONTENT ARE ITERATIVELY  *
!   CALCULATED.                                                        * 
!***********************************************************************
      FUNCTION TPDD(P,THTED,TGS,RS,RD,RH,XLV0,XLV1,                    &        
                    EP2,ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE        ) 
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,   INTENT(IN   )   :: XLV0,XLV1
   REAL,   INTENT(IN   )   :: P,THTED,TGS,RD,RH,EP2,ALIQ,BLIQ,         &
                              CLIQ,DLIQ,AICE,BICE,CICE,DICE 
   REAL,   INTENT(INOUT)   :: RS
   REAL    :: TPDD,ES,PI,THTGS,F0,T1,T0,CCP,F1,DT,RL,DSSDT,T1RH,RSRH
   INTEGER :: ITCNT
!-----------------------------------------------------------------------
      ES=ALIQ*EXP((BLIQ*TGS-CLIQ)/(TGS-DLIQ))                        
      RS=EP2*ES/(P-ES)                                            
      PI=(1.E5/P)**(0.2854*(1.-0.28*RS))                           
      THTGS=TGS*PI*EXP((3374.6525/TGS-2.5403)*RS*(1.+0.81*RS))    
      F0=THTGS-THTED                                             
      T1=TGS-0.5*F0                                             
      T0=TGS                                                   
      CCP=1005.7                                               
!                                                                          
!...ITERATE TO FIND WET-BULB TEMPERATURE...                               
!                                                                        
      ITCNT=0                                                           
   90 ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))                            
      RS=EP2*ES/(P-ES)                                              
      PI=(1.E5/P)**(0.2854*(1.-0.28*RS))                             
      THTGS=T1*PI*EXP((3374.6525/T1-2.5403)*RS*(1.+0.81*RS))        
      F1=THTGS-THTED                                               
      IF(ABS(F1).LT.0.05)GOTO 50                                  
      ITCNT=ITCNT+1                                              
      IF(ITCNT.GT.10)GOTO 50                                    
      DT=F1*(T1-T0)/(F1-F0)                                    
      T0=T1                                                
      F0=F1                                                   
      T1=T1-DT                                               
      GOTO 90                                               
   50 RL=XLV0-XLV1*T1                                        
!                                                           
!...IF RELATIVE HUMIDITY IS SPECIFIED TO BE LESS THAN 100%, ESTIMATE THE  
!   TEMPERATURE AND MIXING RATIO WHICH WILL YIELD THE APPROPRIATE VALUE. 
!                                                                       
      IF(RH.EQ.1.)GOTO 110                                             
      DSSDT=(CLIQ-BLIQ*DLIQ)/((T1-DLIQ)*(T1-DLIQ))                    
      DT=RL*RS*(1.-RH)/(CCP+RL*RH*RS*DSSDT)                           
      T1RH=T1+DT                                                    
      ES=RH*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))                        
      RSRH=EP2*ES/(P-ES)                                               
!                                                                       
!...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL   
!...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...          
!                                                                    
      IF(RSRH.LT.RD)THEN                                            
        RSRH=RD                                                    
        T1RH=T1+(RS-RSRH)*RL/CCP                                   
      ENDIF                                                      
      T1=T1RH                                                   
      RS=RSRH                                                  
  110 TPDD=T1                                                 
      IF(ITCNT.GT.10)PRINT*,'***** NUMBER OF ITERATIONS IN TPDD = ',  & 
        ITCNT                                                         

   END FUNCTION TPDD

!====================================================================
   SUBROUTINE kfinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,           &
                     RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,         &
                     P_FIRST_SCALAR,restart,allowed_to_read,        &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
!--------------------------------------------------------------------   
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  restart, allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_QI,P_QS,P_FIRST_SCALAR

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHCUTEN, &
                                                          RQVCUTEN, &
                                                          RQCCUTEN, &
                                                          RQRCUTEN, &
                                                          RQICUTEN, &
                                                          RQSCUTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   INTEGER :: i, j, k, itf, jtf, ktf
 
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)
 
   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHCUTEN(i,k,j)=0.
        RQVCUTEN(i,k,j)=0.
        RQCCUTEN(i,k,j)=0.
        RQRCUTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     IF (P_QI .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQICUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     IF (P_QS .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQSCUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     DO j=jts,jtf
     DO i=its,itf
        NCA(i,j)=-100.
     ENDDO
     ENDDO

     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        W0AVG(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

   ENDIF

   END SUBROUTINE kfinit

!-------------------------------------------------------

END MODULE module_cu_kf

