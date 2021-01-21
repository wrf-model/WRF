MODULE module_sf_noahlsm
USE module_model_constants, only : CP, R_D, XLF, XLV, RHOWATER, STBOLT, KARMAN
use module_wrf_error













  REAL, PARAMETER      :: RD = 287.04, SIGMA = 5.67E-8,                 &
                          CPH2O = 4.218E+3,CPICE = 2.106E+3,            &
                          LSUBF = 3.335E+5,                             &
                          EMISSI_S = 0.95


        INTEGER :: LUCATS , BARE
        INTEGER :: NATURAL
        INTEGER :: LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11
        integer, PARAMETER :: NLUS=50
        CHARACTER(LEN=256) LUTYPE
        INTEGER, DIMENSION(1:NLUS) :: NROTBL
        real, dimension(1:NLUS) ::  SNUPTBL, RSTBL, RGLTBL, HSTBL,                &
                                    SHDTBL, MAXALB,                               &
                                    EMISSMINTBL, EMISSMAXTBL,                     &
                                    LAIMINTBL, LAIMAXTBL,                         &
                                    Z0MINTBL, Z0MAXTBL,                           &
                                    ALBEDOMINTBL, ALBEDOMAXTBL,                   &
                                    ZTOPVTBL,ZBOTVTBL
        REAL ::   TOPT_DATA,CMCMAX_DATA,CFACTR_DATA,RSMAX_DATA


        INTEGER :: SLCATS
        INTEGER, PARAMETER :: NSLTYPE=30
        CHARACTER(LEN=256) SLTYPE
        REAL, DIMENSION (1:NSLTYPE) :: BB,DRYSMC,F11,                           &
        MAXSMC, REFSMC,SATPSI,SATDK,SATDW, WLTSMC,QTZ


        INTEGER :: SLPCATS
        INTEGER, PARAMETER :: NSLOPE=30
        REAL, DIMENSION (1:NSLOPE) :: SLOPE_DATA
        REAL ::  SBETA_DATA,FXEXP_DATA,CSOIL_DATA,SALP_DATA,REFDK_DATA,           &
                 REFKDT_DATA,FRZK_DATA,ZBOT_DATA,  SMLOW_DATA,SMHIGH_DATA,        &
                        CZIL_DATA
        REAL ::  LVCOEF_DATA

        CHARACTER*256  :: err_message

        integer, private :: iloc, jloc
!$omp threadprivate(iloc, jloc)

CONTAINS


      SUBROUTINE SFLX (IILOC,JJLOC,FFROZP,ISURBAN,DT,ZLVL,NSOIL,SLDPTH, &    
                       LOCAL,                                           &    
                       LLANDUSE, LSOIL,                                 &    
                       LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,  &    
                       COSZ,PRCPRAIN, SOLARDIRECT,                      &    
                       TH2,Q2SAT,DQSDT2,                                &    
                       VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHDMIN,SHDMAX,    &    
                       ALB, SNOALB,TBOT, Z0BRD, Z0, EMISSI, EMBRD,      &    
                       CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,    &    





                       ETA,SHEAT, ETA_KINEMATIC,FDOWN,                  &    
                       EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                   &    
                       BETA,ETP,SSOIL,                                  &    
                       FLX1,FLX2,FLX3,                                  &    
		       FLX4,FVB,FBUR,FGSN,UA_PHYS,                      &    
                       SNOMLT,SNCOVR,                                   &    
                       RUNOFF1,RUNOFF2,RUNOFF3,                         &    
                       RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,             &    
                       SOILW,SOILM,Q1,SMAV,                             &    
                       RDLAI2D,USEMONALB,                               &
                       SNOTIME1,                                        &
                       RIBB,                                            &
                       SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,   &
                       SFHEAD1RT,                                       &    
                       INFXS1RT,ETPND1,OPT_THCND,AOASIS                 &    
                      ,XSDA_QFX,HFX_PHY,QFX_PHY,XQNORM                  &    
                      ,fasdas,HCPCT_FASDAS,IRRIGATION_CHANNEL           )    



































































































































































































      IMPLICIT NONE





      INTEGER, INTENT(IN) :: IILOC, JJLOC
      LOGICAL, INTENT(IN)::  LOCAL
      LOGICAL            ::  FRZGRA, SNOWNG
      CHARACTER (LEN=256), INTENT(IN)::  LLANDUSE, LSOIL




      INTEGER,INTENT(IN) ::  NSOIL,SLOPETYP,SOILTYP,VEGTYP
      INTEGER, INTENT(IN) :: ISURBAN
      INTEGER,INTENT(OUT)::  NROOT
      INTEGER  KZ, K, iout




      LOGICAL, INTENT(IN) :: RDLAI2D
      LOGICAL, INTENT(IN) :: USEMONALB
      INTEGER, INTENT(IN) :: OPT_THCND

      REAL, INTENT(INOUT):: SFHEAD1RT,INFXS1RT, ETPND1

      REAL, INTENT(IN)   :: SHDMIN,SHDMAX,DT,DQSDT2,LWDN,PRCP,PRCPRAIN,     &
                            Q2,Q2SAT,SFCPRS,SFCSPD,SFCTMP, SNOALB,          &
                            SOLDN,SOLNET,TBOT,TH2,ZLVL,                            &
                            FFROZP,AOASIS
      REAL, INTENT(OUT)  :: EMBRD
      REAL, INTENT(OUT)  :: ALBEDO
      REAL, INTENT(INOUT):: COSZ, SOLARDIRECT,CH,CM,                        &
                            CMC,SNEQV,SNCOVR,SNOWH,T1,XLAI,SHDFAC,Z0BRD,    &
                            EMISSI, ALB
      REAL, INTENT(INOUT):: SNOTIME1
      REAL, INTENT(INOUT):: RIBB
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SLDPTH
      REAL, DIMENSION(1:NSOIL), INTENT(OUT):: ET
      REAL, DIMENSION(1:NSOIL), INTENT(OUT):: SMAV
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) ::  SH2O, SMC, STC
      REAL,DIMENSION(1:NSOIL)::   RTDIS, ZSOIL

      REAL,INTENT(OUT)   :: ETA_KINEMATIC,BETA,DEW,DRIP,EC,EDIR,ESNOW,ETA,  &
                            ETP,FLX1,FLX2,FLX3,SHEAT,PC,RUNOFF1,RUNOFF2,    &
                            RUNOFF3,RC,RSMIN,RCQ,RCS,RCSOIL,RCT,SSOIL,      &
                            SMCDRY,SMCMAX,SMCREF,SMCWLT,SNOMLT, SOILM,      &
                            SOILW,FDOWN,Q1
      LOGICAL, INTENT(IN) :: UA_PHYS   
      REAL,INTENT(OUT)    :: FLX4      
      REAL,INTENT(OUT)    :: FVB       
      REAL,INTENT(OUT)    :: FBUR      
      REAL,INTENT(OUT)    :: FGSN      
      REAL                :: ZTOPV     
      REAL                :: ZBOTV     
      REAL                :: GAMA      
      REAL                :: FNET      
      REAL                :: ETPN      
      REAL                :: RU        

      REAL :: BEXP,CFACTR,CMCMAX,CSOIL,CZIL,DF1,DF1H,DF1A,DKSAT,DWSAT,      &
              DSOIL,DTOT,ETT,FRCSNO,FRCSOI,EPSCA,F1,FXEXP,FRZX,HS,          &
              KDT,LVH2O,PRCP1,PSISAT,QUARTZ,R,RCH,REFKDT,RR,RGL,            &
              RSMAX,                                                        &
              RSNOW,SNDENS,SNCOND,SBETA,SN_NEW,SLOPE,SNUP,SALP,SOILWM,      &
              SOILWW,T1V,T24,T2V,TH2V,TOPT,TFREEZ,TSNOW,ZBOT,Z0,PRCPF,      &
              ETNS,PTU,LSUBS
        REAL ::  LVCOEF
      REAL :: INTERP_FRACTION
      REAL :: LAIMIN,    LAIMAX
      REAL :: ALBEDOMIN, ALBEDOMAX
      REAL :: EMISSMIN,  EMISSMAX
      REAL :: Z0MIN,     Z0MAX




      PARAMETER (TFREEZ = 273.15)
      PARAMETER (LVH2O = 2.501E+6)
      PARAMETER (LSUBS = 2.83E+6)
      PARAMETER (R = 287.04)



   INTEGER, INTENT(IN   )  ::  fasdas
   REAL,    INTENT(INOUT)  ::  XSDA_QFX, XQNORM
   REAL,    INTENT(INOUT)  ::  HFX_PHY, QFX_PHY
   REAL,    INTENT(  OUT)  ::  HCPCT_FASDAS




  REAL, OPTIONAL,   INTENT(INOUT)  :: IRRIGATION_CHANNEL




      ILOC = IILOC
      JLOC = JJLOC

      RUNOFF1 = 0.0
      RUNOFF2 = 0.0
      RUNOFF3 = 0.0
      SNOMLT = 0.0

      IF ( .NOT. UA_PHYS ) THEN
          FLX4 = 0.0
          FVB  = 0.0
          FBUR = 0.0
          FGSN = 0.0
      ENDIF






      ZSOIL (1) = - SLDPTH (1)
      DO KZ = 2,NSOIL
         ZSOIL (KZ) = - SLDPTH (KZ) + ZSOIL (KZ -1)
      END DO





         CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,CFACTR,CMCMAX,RSMAX,TOPT,   &
                       REFKDT,KDT,SBETA, SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,    &
                         PSISAT,SLOPE,SNUP,SALP,BEXP,DKSAT,DWSAT,          &
                         SMCMAX,SMCWLT,SMCREF,SMCDRY,F1,QUARTZ,FXEXP,      &
                         RTDIS,SLDPTH,ZSOIL,NROOT,NSOIL,CZIL,              &
                         LAIMIN, LAIMAX, EMISSMIN, EMISSMAX, ALBEDOMIN,    &
                         ALBEDOMAX, Z0MIN, Z0MAX, CSOIL, PTU, LLANDUSE,    &
                         LSOIL,LOCAL,LVCOEF,ZTOPV,ZBOTV)


         IF(VEGTYP==ISURBAN)THEN
              SHDFAC=0.05
              RSMIN=400.0
              SMCMAX = 0.45
              SMCREF = 0.42
              SMCWLT = 0.40
              SMCDRY = 0.40
         ENDIF

         IF ( SHDFAC >= SHDMAX ) THEN
            EMBRD = EMISSMAX
            IF (.NOT. RDLAI2D) THEN
               XLAI  = LAIMAX
            ENDIF
            IF (.NOT. USEMONALB) THEN
               ALB   = ALBEDOMIN
            ENDIF
            Z0BRD = Z0MAX
         ELSE IF ( SHDFAC <= SHDMIN ) THEN
            EMBRD = EMISSMIN
            IF(.NOT. RDLAI2D) THEN
               XLAI  = LAIMIN
            ENDIF
            IF(.NOT. USEMONALB) then
               ALB   = ALBEDOMAX
            ENDIF
            Z0BRD = Z0MIN
         ELSE

            IF ( SHDMAX > SHDMIN ) THEN

               INTERP_FRACTION = ( SHDFAC - SHDMIN ) / ( SHDMAX - SHDMIN )
               
               INTERP_FRACTION = MIN ( INTERP_FRACTION, 1.0 )
               INTERP_FRACTION = MAX ( INTERP_FRACTION, 0.0 )
               
               EMBRD = ( ( 1.0 - INTERP_FRACTION ) * EMISSMIN  ) + ( INTERP_FRACTION * EMISSMAX  )
               IF (.NOT. RDLAI2D) THEN
                  XLAI  = ( ( 1.0 - INTERP_FRACTION ) * LAIMIN    ) + ( INTERP_FRACTION * LAIMAX    )
               ENDIF
               if (.not. USEMONALB) then
                  ALB   = ( ( 1.0 - INTERP_FRACTION ) * ALBEDOMAX ) + ( INTERP_FRACTION * ALBEDOMIN )
               endif
               Z0BRD = ( ( 1.0 - INTERP_FRACTION ) * Z0MIN     ) + ( INTERP_FRACTION * Z0MAX     )

            ELSE

               EMBRD = 0.5 * EMISSMIN  + 0.5 * EMISSMAX
               IF (.NOT. RDLAI2D) THEN
                  XLAI  = 0.5 * LAIMIN    + 0.5 * LAIMAX
               ENDIF
               if (.not. USEMONALB) then
                  ALB   = 0.5 * ALBEDOMIN + 0.5 * ALBEDOMAX
               endif
               Z0BRD = 0.5 * Z0MIN     + 0.5 * Z0MAX

            ENDIF

         ENDIF



         SNOWNG = .FALSE.
         FRZGRA = .FALSE.






         IF ( SNEQV <= 1.E-7 ) THEN 
            SNEQV = 0.0
            SNDENS = 0.0
            SNOWH = 0.0
            SNCOND = 1.0
         ELSE
            SNDENS = SNEQV / SNOWH
            IF(SNDENS > 1.0) THEN
             call wrf_error_fatal3("<stdin>",504,&
'Physical snow depth is less than snow water equiv.'  )
            ENDIF
            CALL CSNOW (SNCOND,SNDENS)
         END IF






         IF (PRCP > 0.0) THEN


            IF (FFROZP .GT. 0.5) THEN
               SNOWNG = .TRUE.
            ELSE
               IF (T1 <= TFREEZ) FRZGRA = .TRUE.
            END IF
         END IF







         IF ( (SNOWNG) .OR. (FRZGRA) ) THEN
            SN_NEW = PRCP * DT * 0.001
            SNEQV = SNEQV + SN_NEW
            PRCPF = 0.0





            CALL SNOW_NEW (SFCTMP,SN_NEW,SNOWH,SNDENS)
            CALL CSNOW (SNCOND,SNDENS)






         ELSE
            PRCPF = PRCP
         ENDIF






         IF (SNEQV  == 0.0) THEN
            SNCOVR = 0.0
            ALBEDO = ALB
            EMISSI = EMBRD
	    IF(UA_PHYS) FGSN = 0.0
	    IF(UA_PHYS) FVB = 0.0
	    IF(UA_PHYS) FBUR = 0.0
         ELSE




            CALL SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR, &
                         XLAI,SHDFAC,FVB,GAMA,FBUR,    &
                         FGSN,ZTOPV,ZBOTV,UA_PHYS)

            IF ( UA_PHYS ) then
              IF(SFCTMP <= T1) THEN
                RU = 0.
              ELSE
                RU = 100.*SHDFAC*FGSN*MIN((SFCTMP-T1)/5., 1.)*(1.-EXP(-XLAI))
              ENDIF
              CH = CH/(1.+RU*CH)
            ENDIF

            SNCOVR = MIN(SNCOVR,0.98) 

            CALL ALCALC (ALB,SNOALB,EMBRD,SHDFAC,SHDMIN,SNCOVR,T1, &
                 ALBEDO,EMISSI,DT,SNOWNG,SNOTIME1,LVCOEF)
         ENDIF
























            CALL TDFCND (DF1,SMC (1),QUARTZ,SMCMAX,SH2O (1),BEXP, PSISAT, SOILTYP, OPT_THCND)


            IF ( VEGTYP == ISURBAN ) DF1=3.24

            DF1 = DF1 * EXP (SBETA * SHDFAC)




            IF (  SNCOVR .GT. 0.97 ) THEN
               DF1 = SNCOND
            ENDIF







         DSOIL = - (0.5 * ZSOIL (1))
         IF (SNEQV == 0.) THEN
            SSOIL = DF1 * (T1- STC (1) ) / DSOIL
         ELSE
            DTOT = SNOWH + DSOIL
            FRCSNO = SNOWH / DTOT



            FRCSOI = DSOIL / DTOT


            DF1H = (SNCOND * DF1)/ (FRCSOI * SNCOND+ FRCSNO * DF1)






            DF1A = FRCSNO * SNCOND+ FRCSOI * DF1






            DF1 = DF1A * SNCOVR + DF1* (1.0- SNCOVR)
            SSOIL = DF1 * (T1- STC (1) ) / DTOT
         END IF




         IF (SNCOVR  > 0. ) THEN
            CALL SNOWZ0 (SNCOVR,Z0,Z0BRD,SNOWH,FBUR,FGSN,SHDMAX,UA_PHYS)
         ELSE
            Z0=Z0BRD
            IF(UA_PHYS) CALL SNOWZ0 (SNCOVR,Z0,Z0BRD,SNOWH,FBUR,FGSN, &
	                             SHDMAX,UA_PHYS)
         END IF







































         FDOWN =  SOLNET + LWDN



         T2V = SFCTMP * (1.0+ 0.61 * Q2 )

         iout=0
         if(iout.eq.1) then
         print*,'before penman'
         print*,' SFCTMP',SFCTMP,'SFCPRS',SFCPRS,'CH',CH,'T2V',T2V,      &
       'TH2',TH2,'PRCP',PRCP,'FDOWN',FDOWN,'T24',T24,'SSOIL',SSOIL,      &
        'Q2',Q2,'Q2SAT',Q2SAT,'ETP',ETP,'RCH',RCH,                       &
        'EPSCA',EPSCA,'RR',RR  ,'SNOWNG',SNOWNG,'FRZGRA',FRZGRA,           &
        'DQSDT2',DQSDT2,'FLX2',FLX2,'SNOWH',SNOWH,'SNEQV',SNEQV,         &
        ' DSOIL',DSOIL,' FRCSNO',FRCSNO,' SNCOVR',SNCOVR,' DTOT',DTOT,   &
       ' ZSOIL (1)',ZSOIL(1),' DF1',DF1,'T1',T1,' STC1',STC(1),          &
        'ALBEDO',ALBEDO,'SMC',SMC,'STC',STC,'SH2O',SH2O
         endif

         CALL PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL,     &
                       Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,          &
                       DQSDT2,FLX2,EMISSI,SNEQV,T1,SNCOVR,AOASIS,        &
		       ALBEDO,SOLDN,FVB,GAMA,STC(1),ETPN,FLX4,UA_PHYS)










         IF ( (SHDFAC > 0.) .AND. (XLAI > 0.) ) THEN
            CALL CANRES (SOLDN,CH,SFCTMP,Q2,SFCPRS,SH2O,ZSOIL,NSOIL,     &
                          SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,  &
                          TOPT,RSMAX,RGL,HS,XLAI,                        &
                          RCS,RCT,RCQ,RCSOIL,EMISSI)
         ELSE
            RC = 0.0
         END IF




         ESNOW = 0.0
         IF (SNEQV  == 0.0) THEN
            CALL NOPAC (ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,                  &
                            SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,           &
                            SHDFAC,                                      &
                            SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,EMISSI,  &
                            SSOIL,                                       &
                            STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,             &
                            SH2O,SLOPE,KDT,FRZX,PSISAT,ZSOIL,            &
                            DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,       &
                            RUNOFF3,EDIR,EC,ET,ETT,NROOT,RTDIS,          &
                            QUARTZ,FXEXP,CSOIL,                          &
                            BETA,DRIP,DEW,FLX1,FLX3,VEGTYP,ISURBAN,      &
                            SFHEAD1RT,INFXS1RT,ETPND1,SOILTYP,OPT_THCND  &
                           ,XSDA_QFX,QFX_PHY,XQNORM,fasdas,HCPCT_FASDAS,IRRIGATION_CHANNEL  ) 

            ETA_KINEMATIC = ETA
         ELSE
            CALL SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,SMC,SMCMAX,SMCWLT,    &
                         SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,              &
                         SBETA,DF1,                                      &
                         Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,  &
                         SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,SNEQV,SNDENS,&
                         SNOWH,SH2O,SLOPE,KDT,FRZX,PSISAT,               &
                         ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,     &
                         RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,    &
                         RTDIS,QUARTZ,FXEXP,CSOIL,                       &
                         BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW,ETNS,EMISSI, &
                         RIBB,SOLDN,                                     &
                         ISURBAN,                                        &
                         VEGTYP,                                         &
                         ETPN,FLX4,UA_PHYS,                              &
                         SFHEAD1RT,INFXS1RT,ETPND1,SOILTYP,OPT_THCND     &
                        ,QFX_PHY,fasdas,HCPCT_FASDAS                     ) 
            ETA_KINEMATIC =  ESNOW + ETNS - 1000.0*DEW
         END IF




     Q1=Q2+ETA_KINEMATIC*CP/RCH





         SHEAT = - (CH * CP * SFCPRS)/ (R * T2V) * ( TH2- T1 )
         IF(UA_PHYS) SHEAT = SHEAT + FLX4   



      IF ( fasdas == 1 ) THEN
        HFX_PHY = SHEAT
      ENDIF






      EDIR = EDIR * LVH2O
      EC = EC * LVH2O
      DO K=1,4
      ET(K) = ET(K) * LVH2O
      ENDDO
      ETT = ETT * LVH2O
      
      ETPND1=ETPND1 * LVH2O

      ESNOW = ESNOW * LSUBS
      ETP = ETP*((1.-SNCOVR)*LVH2O + SNCOVR*LSUBS)
      IF(UA_PHYS) ETPN = ETPN*((1.-SNCOVR)*LVH2O + SNCOVR*LSUBS)
      IF (ETP .GT. 0.) THEN
         ETA = EDIR + EC + ETT + ESNOW
      ELSE
        ETA = ETP
      ENDIF



      IF (ETP == 0.0) THEN
        BETA = 0.0
      ELSE
        BETA = ETA/ETP
      ENDIF






         SSOIL = -1.0* SSOIL







         RUNOFF3 = RUNOFF3/ DT
         RUNOFF2 = RUNOFF2+ RUNOFF3
         SOILM = -1.0* SMC (1)* ZSOIL (1)
         DO K = 2,NSOIL
            SOILM = SOILM + SMC (K)* (ZSOIL (K -1) - ZSOIL (K))
         END DO
         SOILWM = -1.0* (SMCMAX - SMCWLT)* ZSOIL (1)
         SOILWW = -1.0* (SMC (1) - SMCWLT)* ZSOIL (1)

         DO K = 1,NSOIL
            SMAV(K)=(SMC(K) - SMCWLT)/(SMCMAX - SMCWLT)
         END DO

         IF (NROOT >= 2) THEN
            DO K = 2,NROOT
                SOILWM = SOILWM + (SMCMAX - SMCWLT)* (ZSOIL (K -1) - ZSOIL (K))
                SOILWW = SOILWW + (SMC(K) - SMCWLT)* (ZSOIL (K -1) - ZSOIL (K))
            END DO
         END IF
         IF (SOILWM .LT. 1.E-6) THEN
           SOILWM = 0.0
           SOILW  = 0.0
           SOILM  = 0.0
         ELSE
           SOILW = SOILWW / SOILWM
         END IF


  END SUBROUTINE SFLX


      SUBROUTINE ALCALC (ALB,SNOALB,EMBRD,SHDFAC,SHDMIN,SNCOVR,TSNOW,ALBEDO,EMISSI,   &
                         DT,SNOWNG,SNOTIME1,LVCOEF)











      IMPLICIT NONE







      REAL, INTENT(IN)  ::  ALB, SNOALB, EMBRD, SHDFAC, SHDMIN, SNCOVR, TSNOW
      REAL, INTENT(IN)  :: DT
      LOGICAL, INTENT(IN) :: SNOWNG
      REAL, INTENT(INOUT):: SNOTIME1
      REAL, INTENT(OUT) ::  ALBEDO, EMISSI
      REAL              :: SNOALB2
      REAL              :: TM,SNOALB1
      REAL, INTENT(IN)  :: LVCOEF
      REAL, PARAMETER   :: SNACCA=0.94,SNACCB=0.58,SNTHWA=0.82,SNTHWB=0.46



      ALBEDO = ALB + SNCOVR*(SNOALB-ALB)
      EMISSI = EMBRD + SNCOVR*(EMISSI_S - EMBRD)





















































         SNOALB1 = SNOALB+LVCOEF*(0.85-SNOALB)
         SNOALB2=SNOALB1

          IF (SNOWNG) THEN
             SNOTIME1 = 0.
          ELSE
           SNOTIME1=SNOTIME1+DT

                   SNOALB2=SNOALB1*(SNACCA**((SNOTIME1/86400.0)**SNACCB))



          ENDIF

           SNOALB2 = MAX ( SNOALB2, ALB )
           ALBEDO = ALB + SNCOVR*(SNOALB2-ALB)
           IF (ALBEDO .GT. SNOALB2) ALBEDO=SNOALB2










  END SUBROUTINE ALCALC


      SUBROUTINE CANRES (SOLAR,CH,SFCTMP,Q2,SFCPRS,SMC,ZSOIL,NSOIL,       &
                         SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,    &
                         TOPT,RSMAX,RGL,HS,XLAI,                          &
                         RCS,RCT,RCQ,RCSOIL,EMISSI)





































      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NROOT,NSOIL
      INTEGER  K
      REAL,    INTENT(IN) :: CH,DQSDT2,HS,Q2,Q2SAT,RSMIN,RGL,RSMAX,        &
                             SFCPRS,SFCTMP,SMCREF,SMCWLT, SOLAR,TOPT,XLAI, &
                             EMISSI
      REAL,DIMENSION(1:NSOIL), INTENT(IN) :: SMC,ZSOIL
      REAL,    INTENT(OUT):: PC,RC,RCQ,RCS,RCSOIL,RCT
      REAL                :: DELTA,FF,GX,P,RR
      REAL, DIMENSION(1:NSOIL) ::  PART
      REAL, PARAMETER     :: SLV = 2.501000E6





      RCS = 0.0
      RCT = 0.0
      RCQ = 0.0
      RCSOIL = 0.0




      RC = 0.0
      FF = 0.55*2.0* SOLAR / (RGL * XLAI)
      RCS = (FF + RSMIN / RSMAX) / (1.0+ FF)





      RCS = MAX (RCS,0.0001)
      RCT = 1.0- 0.0016* ( (TOPT - SFCTMP)**2.0)





      RCT = MAX (RCT,0.0001)
      RCQ = 1.0/ (1.0+ HS * (Q2SAT - Q2))





      RCQ = MAX (RCQ,0.01)
      GX = (SMC (1) - SMCWLT) / (SMCREF - SMCWLT)
      IF (GX  >  1.) GX = 1.
      IF (GX  <  0.) GX = 0.








      PART (1) = (ZSOIL (1)/ ZSOIL (NROOT)) * GX
      DO K = 2,NROOT
         GX = (SMC (K) - SMCWLT) / (SMCREF - SMCWLT)
         IF (GX >  1.) GX = 1.
         IF (GX <  0.) GX = 0.







         PART (K) = ( (ZSOIL (K) - ZSOIL (K -1))/ ZSOIL (NROOT)) * GX
      END DO
      DO K = 1,NROOT
         RCSOIL = RCSOIL + PART (K)
      END DO








      RCSOIL = MAX (RCSOIL,0.0001)

      RC = RSMIN / (XLAI * RCS * RCT * RCQ * RCSOIL)

      RR = (4.* EMISSI *SIGMA * RD / CP)* (SFCTMP **4.)/ (SFCPRS * CH) &
             + 1.0

      DELTA = (SLV / CP)* DQSDT2

      PC = (RR + DELTA)/ (RR * (1. + RC * CH) + DELTA)


  END SUBROUTINE CANRES


      SUBROUTINE CSNOW (SNCOND,DSNOW)







      IMPLICIT NONE
      REAL, INTENT(IN) :: DSNOW
      REAL, INTENT(OUT):: SNCOND
      REAL             :: C
      REAL, PARAMETER  :: UNIT = 0.11631






      C = 0.328*10** (2.25* DSNOW)
















      SNCOND = 2.0 * UNIT * C


  END SUBROUTINE CSNOW

      SUBROUTINE DEVAP (EDIR,ETP1,SMC,ZSOIL,SHDFAC,SMCMAX,BEXP,         &
                        DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)







      IMPLICIT NONE
      REAL, INTENT(IN) :: ETP1,SMC,BEXP,DKSAT,DWSAT,FXEXP,              &
                          SHDFAC,SMCDRY,SMCMAX,ZSOIL,SMCREF,SMCWLT
      REAL, INTENT(OUT):: EDIR
      REAL             :: FX, SRATIO











      SRATIO = (SMC - SMCDRY) / (SMCMAX - SMCDRY)
      IF (SRATIO > 0.) THEN
        FX = SRATIO**FXEXP
        FX = MAX ( MIN ( FX, 1. ) ,0. )
      ELSE
        FX = 0.
      ENDIF




      EDIR = FX * ( 1.0- SHDFAC ) * ETP1


  END SUBROUTINE DEVAP

      SUBROUTINE DEVAP_hydro (EDIR,ETP1,SMC,ZSOIL,SHDFAC,SMCMAX,BEXP,         &
                        DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP,         &
                        SFHEAD1RT,ETPND1,DT)







      IMPLICIT NONE
      REAL, INTENT(IN) :: ETP1,SMC,BEXP,DKSAT,DWSAT,FXEXP,              &
                          SHDFAC,SMCDRY,SMCMAX,ZSOIL,SMCREF,SMCWLT
      REAL, INTENT(OUT):: EDIR
      REAL             :: FX, SRATIO

      REAL, INTENT(INOUT) :: SFHEAD1RT,ETPND1
      REAL, INTENT(IN   ) :: DT
       REAL             :: EDIRTMP












      SRATIO = (SMC - SMCDRY) / (SMCMAX - SMCDRY)
      IF (SRATIO > 0.) THEN
        FX = SRATIO**FXEXP
        FX = MAX ( MIN ( FX, 1. ) ,0. )
      ELSE
        FX = 0.
      ENDIF


      EDIRTMP = 0.
      ETPND1 = 0.








      EDIRTMP = EDIRTMP * DT


      SFHEAD1RT=SFHEAD1RT * 0.001




      IF (EDIRTMP > 0.) THEN
       IF ( EDIRTMP > SFHEAD1RT ) THEN
         ETPND1 = SFHEAD1RT
         SFHEAD1RT=0.
         EDIRTMP = EDIRTMP - ETPND1
       ELSE
         ETPND1 = EDIRTMP
         EDIRTMP = 0.
         SFHEAD1RT = SFHEAD1RT - ETPND1
       END IF
      END IF


      IF ( SFHEAD1RT /= 0.) SFHEAD1RT=SFHEAD1RT * 1000.


      ETPND1 = ETPND1 / DT
      EDIRTMP = EDIRTMP / DT








       EDIR = FX * EDIRTMP





  END SUBROUTINE DEVAP_hydro


      SUBROUTINE EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,               &
                         SH2O,                                          &
                         SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,             &
                         SMCREF,SHDFAC,CMCMAX,                          &
                         SMCDRY,CFACTR,                                 &
                         EDIR,EC,ET,ETT,SFCTMP,Q2,NROOT,RTDIS,FXEXP,    &
                         SFHEAD1RT,ETPND1)










      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: NSOIL, NROOT
      INTEGER               :: I,K
      REAL,    INTENT(IN)   :: BEXP, CFACTR,CMC,CMCMAX,DKSAT,           &
                                 DT,DWSAT,ETP1,FXEXP,PC,Q2,SFCTMP,           &
                                 SHDFAC,SMCDRY,SMCMAX,SMCREF,SMCWLT
      REAL,    INTENT(OUT)  :: EC,EDIR,ETA1,ETT
      REAL                  :: CMC2MS
      REAL,DIMENSION(1:NSOIL), INTENT(IN) :: RTDIS, SMC, SH2O, ZSOIL
      REAL,DIMENSION(1:NSOIL), INTENT(OUT) :: ET

      REAL,   INTENT(INOUT) :: SFHEAD1RT,ETPND1





      EDIR = 0.
      EC = 0.
      ETT = 0.
      DO K = 1,NSOIL
         ET (K) = 0.
      END DO






      IF (ETP1 > 0.0) THEN
         IF (SHDFAC <  1.) THEN
             CALL DEVAP (EDIR,ETP1,SMC (1),ZSOIL (1),SHDFAC,SMCMAX,      &
                         BEXP,DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)
         END IF





         IF (SHDFAC > 0.0) THEN
            CALL TRANSP (ET,NSOIL,ETP1,SH2O,CMC,ZSOIL,SHDFAC,SMCWLT,     &
                          CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)
            DO K = 1,NSOIL
               ETT = ETT + ET ( K )
            END DO




            IF (CMC > 0.0) THEN
               EC = SHDFAC * ( ( CMC / CMCMAX ) ** CFACTR ) * ETP1
            ELSE
               EC = 0.0
            END IF




            CMC2MS = CMC / DT
            EC = MIN ( CMC2MS, EC )
         END IF
      END IF



      ETA1 = EDIR + ETT + EC


  END SUBROUTINE EVAPO


  SUBROUTINE FAC2MIT(SMCMAX,FLIMIT)
    IMPLICIT NONE		
    REAL, INTENT(IN)  :: SMCMAX
    REAL, INTENT(OUT) :: FLIMIT

    FLIMIT = 0.90

    IF ( SMCMAX == 0.395 ) THEN
       FLIMIT = 0.59
    ELSE IF ( ( SMCMAX == 0.434 ) .OR. ( SMCMAX == 0.404 ) ) THEN
       FLIMIT = 0.85
    ELSE IF ( ( SMCMAX == 0.465 ) .OR. ( SMCMAX == 0.406 ) ) THEN
       FLIMIT = 0.86
    ELSE IF ( ( SMCMAX == 0.476 ) .OR. ( SMCMAX == 0.439 ) ) THEN
       FLIMIT = 0.74
    ELSE IF ( ( SMCMAX == 0.200 ) .OR. ( SMCMAX == 0.464 ) ) THEN
       FLIMIT = 0.80
    ENDIF


  END SUBROUTINE FAC2MIT


      SUBROUTINE FRH2O (FREE,TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)






























      IMPLICIT NONE
      REAL, INTENT(IN)     :: BEXP,PSIS,SH2O,SMC,SMCMAX,TKELV
      REAL, INTENT(OUT)    :: FREE
      REAL                 :: BX,DENOM,DF,DSWL,FK,SWL,SWLK
      INTEGER              :: NLOG,KCOUNT

      REAL, PARAMETER      :: CK = 8.0, BLIM = 5.5, ERROR = 0.005,       &
                              HLICE = 3.335E5, GS = 9.81,DICE = 920.0,   &
                              DH2O = 1000.0, T0 = 273.15






      BX = BEXP




      IF (BEXP >  BLIM) BX = BLIM
      NLOG = 0




      KCOUNT = 0

      IF (TKELV > (T0- 1.E-3)) THEN
          FREE = SMC
      ELSE







         IF (CK /= 0.0) THEN
            SWL = SMC - SH2O



            IF (SWL > (SMC -0.02)) SWL = SMC -0.02




            IF (SWL < 0.) SWL = 0.
 1001       Continue
              IF (.NOT.( (NLOG < 10) .AND. (KCOUNT == 0)))   goto 1002
              NLOG = NLOG +1
              DF = ALOG ( ( PSIS * GS / HLICE ) * ( ( 1. + CK * SWL )**2.) * &
                   ( SMCMAX / (SMC - SWL) )** BX) - ALOG ( - (               &
                   TKELV - T0)/ TKELV)
              DENOM = 2. * CK / ( 1. + CK * SWL ) + BX / ( SMC - SWL )
              SWLK = SWL - DF / DENOM



              IF (SWLK > (SMC -0.02)) SWLK = SMC - 0.02
              IF (SWLK < 0.) SWLK = 0.




              DSWL = ABS (SWLK - SWL)





              SWL = SWLK
              IF ( DSWL <= ERROR ) THEN
                    KCOUNT = KCOUNT +1
              END IF






           goto 1001
 1002   continue
           FREE = SMC - SWL
         END IF








         IF (KCOUNT == 0) THEN
             PRINT *,'Flerchinger USEd in NEW version. Iterations=',NLOG
                  FK = ( ( (HLICE / (GS * ( - PSIS)))*                    &
                       ( (TKELV - T0)/ TKELV))** ( -1/ BX))* SMCMAX

             IF (FK < 0.02) FK = 0.02
             FREE = MIN (FK, SMC)



         END IF
      END IF

  END SUBROUTINE FRH2O


      SUBROUTINE HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,          &
                       TBOT,ZBOT,PSISAT,SH2O,DT,BEXP,SOILTYP,OPT_THCND, &
                       F1,DF1,QUARTZ,CSOIL,AI,BI,CI,VEGTYP,ISURBAN      &
                      ,HCPCT_FASDAS                                     ) 








      IMPLICIT NONE
      LOGICAL              :: ITAVG
      INTEGER, INTENT(IN)  :: OPT_THCND
      INTEGER, INTENT(IN)  :: NSOIL, VEGTYP, SOILTYP
      INTEGER, INTENT(IN)  :: ISURBAN
      INTEGER              :: I, K

      REAL, INTENT(IN)     :: BEXP, CSOIL, DF1, DT,F1,PSISAT,QUARTZ,     &
                              SMCMAX ,TBOT,YY,ZZ1, ZBOT
      REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SMC,STC,ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT):: SH2O
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: RHSTS
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: AI, BI,CI
      REAL                 :: DDZ, DDZ2, DENOM, DF1N, DF1K, DTSDZ,       &
                              DTSDZ2,HCPCT,QTOT,SSOIL,SICE,TAVG,TBK,     &
                              TBK1,TSNSR,TSURF,CSOIL_LOC
      REAL, PARAMETER      :: T0 = 273.15, CAIR = 1004.0, CICE = 2.106E6,&
                              CH2O = 4.2E6




      REAL, INTENT(  OUT)       :: HCPCT_FASDAS





        IF( VEGTYP == ISURBAN ) then
            CSOIL_LOC=3.0E6
        ELSE
            CSOIL_LOC=CSOIL
        ENDIF




       ITAVG = .TRUE.





      HCPCT = SH2O (1)* CH2O + (1.0- SMCMAX)* CSOIL_LOC + (SMCMAX - SMC (1))&
       * CAIR                                                           &
              + ( SMC (1) - SH2O (1) )* CICE



      HCPCT_FASDAS = HCPCT






      DDZ = 1.0 / ( -0.5 * ZSOIL (2) )
      AI (1) = 0.0
      CI (1) = (DF1 * DDZ) / (ZSOIL (1) * HCPCT)







      BI (1) = - CI (1) + DF1 / (0.5 * ZSOIL (1) * ZSOIL (1)* HCPCT *    &
       ZZ1)
      DTSDZ = (STC (1) - STC (2)) / ( -0.5 * ZSOIL (2))
      SSOIL = DF1 * (STC (1) - YY) / (0.5 * ZSOIL (1) * ZZ1)

      DENOM = (ZSOIL (1) * HCPCT)







      RHSTS (1) = (DF1 * DTSDZ - SSOIL) / DENOM




      QTOT = -1.0* RHSTS (1)* DENOM











      SICE = SMC (1) - SH2O (1)
      IF (ITAVG) THEN
         TSURF = (YY + (ZZ1-1) * STC (1)) / ZZ1






         CALL TBND (STC (1),STC (2),ZSOIL,ZBOT,1,NSOIL,TBK)
         IF ( (SICE > 0.) .OR. (STC (1) < T0) .OR.                         &
            (TSURF < T0) .OR. (TBK < T0) ) THEN

            CALL TMPAVG (TAVG,TSURF,STC (1),TBK,ZSOIL,NSOIL,1)
            CALL SNKSRC (TSNSR,TAVG,SMC (1),SH2O (1),                      &
                          ZSOIL,NSOIL,SMCMAX,PSISAT,BEXP,DT,1,QTOT)

            RHSTS (1) = RHSTS (1) - TSNSR / DENOM
         END IF
      ELSE

         IF ( (SICE > 0.) .OR. (STC (1) < T0) ) THEN
            CALL SNKSRC (TSNSR,STC (1),SMC (1),SH2O (1),                   &
                          ZSOIL,NSOIL,SMCMAX,PSISAT,BEXP,DT,1,QTOT)

            RHSTS (1) = RHSTS (1) - TSNSR / DENOM
         END IF



      END IF




      DDZ2 = 0.0
      DF1K = DF1







      DO K = 2,NSOIL
         HCPCT = SH2O (K)* CH2O + (1.0- SMCMAX)* CSOIL_LOC + (SMCMAX - SMC (  &
                K))* CAIR + ( SMC (K) - SH2O (K) )* CICE





         IF (K /= NSOIL) THEN




            CALL TDFCND (DF1N,SMC (K),QUARTZ,SMCMAX,SH2O (K),BEXP, PSISAT, SOILTYP, OPT_THCND)


       IF ( VEGTYP == ISURBAN ) DF1N = 3.24

            DENOM = 0.5 * ( ZSOIL (K -1) - ZSOIL (K +1) )




            DTSDZ2 = ( STC (K) - STC (K +1) ) / DENOM
            DDZ2 = 2. / (ZSOIL (K -1) - ZSOIL (K +1))





            CI (K) = - DF1N * DDZ2 / ( (ZSOIL (K -1) - ZSOIL (K)) *      &
       HCPCT)
            IF (ITAVG) THEN
               CALL TBND (STC (K),STC (K +1),ZSOIL,ZBOT,K,NSOIL,TBK1)
            END IF

         ELSE








            CALL TDFCND (DF1N,SMC (K),QUARTZ,SMCMAX,SH2O (K),BEXP, PSISAT, SOILTYP, OPT_THCND)



       IF ( VEGTYP == ISURBAN ) DF1N = 3.24

            DENOM = .5 * (ZSOIL (K -1) + ZSOIL (K)) - ZBOT




            DTSDZ2 = (STC (K) - TBOT) / DENOM





            CI (K) = 0.
            IF (ITAVG) THEN
               CALL TBND (STC (K),TBOT,ZSOIL,ZBOT,K,NSOIL,TBK1)
            END IF


         END IF



         DENOM = ( ZSOIL (K) - ZSOIL (K -1) ) * HCPCT
         RHSTS (K) = ( DF1N * DTSDZ2- DF1K * DTSDZ ) / DENOM
         QTOT = -1.0* DENOM * RHSTS (K)

         SICE = SMC (K) - SH2O (K)
         IF (ITAVG) THEN
            CALL TMPAVG (TAVG,TBK,STC (K),TBK1,ZSOIL,NSOIL,K)

            IF ( (SICE > 0.) .OR. (STC (K) < T0) .OR.                   &
               (TBK .lt. T0) .OR. (TBK1 .lt. T0) ) THEN
               CALL SNKSRC (TSNSR,TAVG,SMC (K),SH2O (K),ZSOIL,NSOIL,    &
                             SMCMAX,PSISAT,BEXP,DT,K,QTOT)
               RHSTS (K) = RHSTS (K) - TSNSR / DENOM
            END IF
         ELSE

            IF ( (SICE > 0.) .OR. (STC (K) < T0) ) THEN
               CALL SNKSRC (TSNSR,STC (K),SMC (K),SH2O (K),ZSOIL,NSOIL, &
                             SMCMAX,PSISAT,BEXP,DT,K,QTOT)
               RHSTS (K) = RHSTS (K) - TSNSR / DENOM
            END IF
         END IF




         AI (K) = - DF1K * DDZ / ( (ZSOIL (K -1) - ZSOIL (K)) * HCPCT)




         BI (K) = - (AI (K) + CI (K))
         TBK = TBK1
         DF1K = DF1N
         DTSDZ = DTSDZ2
         DDZ = DDZ2
      END DO

  END SUBROUTINE HRT


      SUBROUTINE HSTEP (STCOUT,STCIN,RHSTS,DT,NSOIL,AI,BI,CI)






      IMPLICIT NONE
      INTEGER, INTENT(IN)  :: NSOIL
      INTEGER              :: K

      REAL, DIMENSION(1:NSOIL), INTENT(IN):: STCIN
      REAL, DIMENSION(1:NSOIL), INTENT(OUT):: STCOUT
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT):: RHSTS
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT):: AI,BI,CI
      REAL, DIMENSION(1:NSOIL) :: RHSTSin
      REAL, DIMENSION(1:NSOIL) :: CIin
      REAL                 :: DT




      DO K = 1,NSOIL
         RHSTS (K) = RHSTS (K) * DT
         AI (K) = AI (K) * DT
         BI (K) = 1. + BI (K) * DT
         CI (K) = CI (K) * DT
      END DO



      DO K = 1,NSOIL
         RHSTSin (K) = RHSTS (K)
      END DO
      DO K = 1,NSOIL
         CIin (K) = CI (K)
      END DO



      CALL ROSR12 (CI,AI,BI,CIin,RHSTSin,RHSTS,NSOIL)



      DO K = 1,NSOIL
         STCOUT (K) = STCIN (K) + CI (K)
      END DO

  END SUBROUTINE HSTEP


      SUBROUTINE NOPAC (ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,                 &
                         SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,SHDFAC,      &
                         SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,EMISSI,    &
                         SSOIL,                                         &
                         STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,               &
                         SH2O,SLOPE,KDT,FRZFACT,PSISAT,ZSOIL,           &
                         DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,         &
                         RUNOFF3,EDIR,EC,ET,ETT,NROOT,RTDIS,            &
                         QUARTZ,FXEXP,CSOIL,                            &
                         BETA,DRIP,DEW,FLX1,FLX3,VEGTYP,ISURBAN,        &
                         SFHEAD1RT,INFXS1RT,ETPND1,SOILTYP,OPT_THCND    &
                        ,XSDA_QFX,QFX_PHY,XQNORM,fasdas,HCPCT_FASDAS    &
                        ,IRRIGATION_CHANNEL    ) 








      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: OPT_THCND
      INTEGER, INTENT(IN)  :: NROOT,NSOIL,VEGTYP,SOILTYP
      INTEGER, INTENT(IN)  :: ISURBAN
      INTEGER              :: K

      REAL, INTENT(IN)     :: BEXP,CFACTR, CMCMAX,CSOIL,DKSAT,DT,DWSAT, &
                              EPSCA,ETP,FDOWN,F1,FXEXP,FRZFACT,KDT,PC,  &
                              PRCP,PSISAT,Q2,QUARTZ,RCH,RR,SBETA,SFCTMP,&
                              SHDFAC,SLOPE,SMCDRY,SMCMAX,SMCREF,SMCWLT, &
                              T24,TBOT,TH2,ZBOT,EMISSI
      REAL, INTENT(INOUT)  :: CMC,BETA,T1
      REAL, INTENT(OUT)    :: DEW,DRIP,EC,EDIR,ETA,ETT,FLX1,FLX3,       &
                              RUNOFF1,RUNOFF2,RUNOFF3,SSOIL

      REAL, INTENT(INOUT)  :: SFHEAD1RT,INFXS1RT,ETPND1

      REAL, DIMENSION(1:NSOIL),INTENT(IN)     :: RTDIS,ZSOIL
      REAL, DIMENSION(1:NSOIL),INTENT(OUT)    :: ET
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC,SH2O,STC
      REAL, DIMENSION(1:NSOIL) :: ET1
      REAL                 :: EC1,EDIR1,ETT1,DF1,ETA1,ETP1,PRCP1,YY,    &
                              YYNUM,ZZ1



      REAL                       ::  XSDA_QFX, QFX_PHY, XQNORM
      INTEGER                    ::  fasdas
      REAL , DIMENSION(1:NSOIL)  ::  EFT(NSOIL), wetty(1:NSOIL)
      REAL                       ::  EFDIR, EFC, EALL_now
      REAL, INTENT(  OUT)        ::  HCPCT_FASDAS
      REAL, INTENT(IN),OPTIONAL  ::  IRRIGATION_CHANNEL







      PRCP1 = PRCP * 0.001
      ETP1 = ETP * 0.001
      DEW = 0.0






      QFX_PHY = 0.0



      EDIR = 0.
      EDIR1 = 0.
      EC1 = 0.
      EC = 0.
      DO K = 1,NSOIL
        ET(K) = 0.
        ET1(K) = 0.



        wetty(K) = 1.0



      END DO
      ETT = 0.
      ETT1 = 0.


      ETPND1 = 0.


      IF (ETP > 0.0) THEN
         CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,                  &
                      SH2O,                                             &
                      SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,                &
                      SMCREF,SHDFAC,CMCMAX,                             &
                      SMCDRY,CFACTR,                                    &
                       EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP,  &
                      SFHEAD1RT,ETPND1 )



      IF( fasdas == 1 ) THEN
        DO K=1,NSOIL
        QFX_PHY = QFX_PHY + ET1(K)   

        IF(SMC(K).GE.SMCREF.and.XSDA_QFX.gt.0.0) wetty(K)=0.0
        END DO
       QFX_PHY = EDIR1+EC1+QFX_PHY    
       EALL_now = QFX_PHY           
       QFX_PHY = QFX_PHY*1000.0     

       if(EALL_now.ne.0.0) then
       EFDIR = (EDIR1/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
       EFDIR = EFDIR * wetty(1)
          
           EDIR1 = EDIR1 + EFDIR    
       
       EFC = (EC1/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
       
       EC1 = EC1 + EFC         


       DO K=1,NSOIL
        EFT(K) = (ET1(K)/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
        EFT(K) =  EFT(K) * wetty(K)
        
        ET1(K) = ET1(K) + EFT(K) 
       END DO


       END IF 
      ELSE
        QFX_PHY = 0.0
      ENDIF



         CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &
                      SH2O,SLOPE,KDT,FRZFACT,                           &
                      SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &
                      SHDFAC,CMCMAX,                                    &
                      RUNOFF1,RUNOFF2,RUNOFF3,                          &
                      EDIR1,EC1,ET1,                                    &
                      DRIP, SFHEAD1RT,INFXS1RT,IRRIGATION_CHANNEL)





         ETA = ETA1 * 1000.0





      ELSE
         DEW = - ETP1





         PRCP1 = PRCP1+ DEW



     IF( fasdas == 1 ) THEN
       DO K=1,NSOIL
        QFX_PHY = QFX_PHY + ET1(K)   

        IF(SMC(K).GE.SMCREF.and.XSDA_QFX.gt.0.0) wetty(K)=0.0
       END DO
       QFX_PHY = EDIR1+EC1+QFX_PHY    
        EALL_now = QFX_PHY     
       QFX_PHY = QFX_PHY*1000.0    

       IF(EALL_now.ne.0.0) then
       EFDIR = (EDIR1/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
        EFDIR =  EFDIR * wetty(1)
          
           EDIR1 = EDIR1 + EFDIR    

        EFC = (EC1/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
        
        EC1 = EC1+ EFC         

       DO K=1,NSOIL
        EFT(K) = (ET1(K)/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
        EFT(K) = EFT(K) * wetty(K)
        
        ET1(K) = ET1(K) + EFT(K)   
       END DO

        END IF 
      ELSE
        QFX_PHY = 0.0
      ENDIF



         CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &
                      SH2O,SLOPE,KDT,FRZFACT,                           &
                      SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &
                      SHDFAC,CMCMAX,                                    &
                      RUNOFF1,RUNOFF2,RUNOFF3,                          &
                      EDIR1,EC1,ET1,                                    &
                      DRIP, SFHEAD1RT,INFXS1RT,IRRIGATION_CHANNEL)





      END IF





      IF ( ETP <= 0.0 ) THEN
         BETA = 0.0
         ETA = ETP
         IF ( ETP < 0.0 ) THEN
            BETA = 1.0
         END IF
      ELSE
         BETA = ETA / ETP
      END IF




      EDIR = EDIR1*1000.
      EC = EC1*1000.
      DO K = 1,NSOIL
        ET(K) = ET1(K)*1000.
      END DO
      ETT = ETT1*1000.







      CALL TDFCND (DF1,SMC (1),QUARTZ,SMCMAX,SH2O (1),BEXP, PSISAT, SOILTYP, OPT_THCND)


      IF ( VEGTYP == ISURBAN ) DF1=3.24









      DF1 = DF1 * EXP (SBETA * SHDFAC)




      YYNUM = FDOWN - EMISSI*SIGMA * T24
      YY = SFCTMP + (YYNUM / RCH + TH2- SFCTMP - BETA * EPSCA) / RR

      ZZ1 = DF1 / ( -0.5 * ZSOIL (1) * RCH * RR ) + 1.0


      CALL SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,       &
                  TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,            &
                  QUARTZ,CSOIL,VEGTYP,ISURBAN,SOILTYP,OPT_THCND        &
                 ,HCPCT_FASDAS                                         ) 






      FLX1 = CPH2O * PRCP * (T1- SFCTMP)
      FLX3 = 0.0


  END SUBROUTINE NOPAC


      SUBROUTINE PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL, &
     &                   Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,       &
     &                   DQSDT2,FLX2,EMISSI_IN,SNEQV,T1,SNCOVR,AOASIS,  &
		         ALBEDO,SOLDN,FVB,GAMA,STC1,ETPN,FLX4,UA_PHYS)








      IMPLICIT NONE
      LOGICAL, INTENT(IN)     :: SNOWNG, FRZGRA
      REAL, INTENT(IN)        :: CH, DQSDT2,FDOWN,PRCP,                 &
                                 Q2, Q2SAT,SSOIL, SFCPRS, SFCTMP,       &
                                 T2V, TH2,EMISSI_IN,SNEQV,AOASIS
      REAL, INTENT(IN)        :: T1 , SNCOVR
      REAL, INTENT(IN)        :: ALBEDO,SOLDN,FVB,GAMA,STC1
      LOGICAL, INTENT(IN)     :: UA_PHYS

      REAL, INTENT(OUT)       :: EPSCA,ETP,FLX2,RCH,RR,T24
      REAL, INTENT(OUT)       :: FLX4,ETPN
      REAL                    :: A, DELTA, FNET,RAD,RHO,EMISSI,ELCP1,LVS
      REAL                    :: TOTABS,UCABS,SIGNCK,FNETN,RADN,EPSCAN

      REAL, PARAMETER      :: ELCP = 2.4888E+3, LSUBC = 2.501000E+6,CP = 1004.6
      REAL, PARAMETER      :: LSUBS = 2.83E+6
      REAL, PARAMETER      :: ALGDSN = 0.5, ALVGSN = 0.13







        EMISSI=EMISSI_IN
        ELCP1  = (1.0-SNCOVR)*ELCP  + SNCOVR*ELCP*LSUBS/LSUBC
        LVS    = (1.0-SNCOVR)*LSUBC + SNCOVR*LSUBS

      FLX2 = 0.0

      DELTA = ELCP1 * DQSDT2
      T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP

      RR = EMISSI*T24 * 6.48E-8 / (SFCPRS * CH) + 1.0
      RHO = SFCPRS / (RD * T2V)





      RCH = RHO * CP * CH
      IF (.NOT. SNOWNG) THEN
         IF (PRCP >  0.0) RR = RR + CPH2O * PRCP / RCH
      ELSE
         RR = RR + CPICE * PRCP / RCH
      END IF






      FNET = FDOWN -  EMISSI*SIGMA * T24- SSOIL

      FLX4 = 0.0
      IF(UA_PHYS) THEN
        IF(SNEQV > 0. .AND. FNET > 0. .AND. SOLDN > 0. ) THEN
         TOTABS = (1.-ALBEDO)*SOLDN*FVB           
                                                  
         UCABS = MIN(TOTABS,((1.0-ALGDSN)*(1.0-ALVGSN)*SOLDN*GAMA)*FVB)


                                                  
						  
         FLX4 = MIN(TOTABS - UCABS, MIN(250., 0.5*(1.-ALBEDO)*SOLDN))
        ENDIF

        SIGNCK = (STC1-273.15)*(SFCTMP-273.15)

        IF(FLX4 > 0. .AND. (SIGNCK <= 0. .OR. STC1 < 273.15)) THEN
          IF(FNET >= FLX4) THEN
           FNETN = FNET - FLX4
          ELSE
           FLX4 = FNET
           FNETN = 0.
          ENDIF
        ELSE
          FLX4 = 0.0
          FNETN = 0.
        ENDIF
      ENDIF

      IF (FRZGRA) THEN
         FLX2 = - LSUBF * PRCP
         FNET = FNET - FLX2
         IF(UA_PHYS) FNETN = FNETN - FLX2



      END IF
      RAD = FNET / RCH + TH2- SFCTMP

      A = ELCP1 * (Q2SAT - Q2)
      EPSCA = (A * RR + RAD * DELTA) / (DELTA + RR)

      IF (EPSCA>0.) EPSCA = EPSCA * AOASIS

      ETP = EPSCA * RCH / LVS

      IF(UA_PHYS) THEN
        RADN = FNETN / RCH + TH2- SFCTMP
        EPSCAN = (A * RR + RADN * DELTA) / (DELTA + RR)
        ETPN = EPSCAN * RCH / LVS
      END IF

  END SUBROUTINE PENMAN


      SUBROUTINE REDPRM (VEGTYP,SOILTYP,SLOPETYP,CFACTR,CMCMAX,RSMAX,      &
                         TOPT,                                             &
                         REFKDT,KDT,SBETA, SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,  &
                         PSISAT,SLOPE,SNUP,SALP,BEXP,DKSAT,DWSAT,          &
                         SMCMAX,SMCWLT,SMCREF,SMCDRY,F1,QUARTZ,FXEXP,      &
                         RTDIS,SLDPTH,ZSOIL, NROOT,NSOIL,CZIL,             &
                         LAIMIN, LAIMAX, EMISSMIN, EMISSMAX, ALBEDOMIN,    &
                         ALBEDOMAX, Z0MIN, Z0MAX, CSOIL, PTU, LLANDUSE,    &
                         LSOIL, LOCAL,LVCOEF,ZTOPV,ZBOTV)

      IMPLICIT NONE










































































      INTEGER, PARAMETER     :: MAX_SLOPETYP=30,MAX_SOILTYP=30,MAX_VEGTYP=30
      LOGICAL                :: LOCAL
      CHARACTER (LEN=256), INTENT(IN)::  LLANDUSE, LSOIL


      INTEGER, INTENT(IN)    :: VEGTYP
      INTEGER, INTENT(OUT)   :: NROOT
      REAL, INTENT(INOUT)    :: SHDFAC
      REAL, INTENT(OUT)      :: HS,RSMIN,RGL,SNUP,                          &
                                CMCMAX,RSMAX,TOPT,                          &
                                EMISSMIN,  EMISSMAX,                        &
                                LAIMIN,    LAIMAX,                          &
                                Z0MIN,     Z0MAX,                           &
                                ALBEDOMIN, ALBEDOMAX, ZTOPV, ZBOTV

      INTEGER, INTENT(IN)    :: SOILTYP
      REAL, INTENT(OUT)      :: BEXP,DKSAT,DWSAT,F1,QUARTZ,SMCDRY,          &
                                SMCMAX,SMCREF,SMCWLT,PSISAT

      INTEGER, INTENT(IN)    :: SLOPETYP,NSOIL
      INTEGER                :: I

      REAL,    INTENT(OUT)   :: SLOPE,CZIL,SBETA,FXEXP,                     &
                                CSOIL,SALP,FRZX,KDT,CFACTR,      &
                                ZBOT,REFKDT,PTU
      REAL,    INTENT(OUT)   :: LVCOEF
      REAL,DIMENSION(1:NSOIL),INTENT(IN) :: SLDPTH,ZSOIL
      REAL,DIMENSION(1:NSOIL),INTENT(OUT):: RTDIS
      REAL                   :: FRZFACT,FRZK,REFDK




               IF (SOILTYP .gt. SLCATS) THEN
                        call wrf_error_fatal3("<stdin>",2415,&
'Warning: too many input soil types'  )
               END IF
               IF (VEGTYP .gt. LUCATS) THEN
                     call wrf_error_fatal3("<stdin>",2419,&
'Warning: too many input landuse types'  )
               END IF
               IF (SLOPETYP .gt. SLPCATS) THEN
                     call wrf_error_fatal3("<stdin>",2423,&
'Warning: too many input slope types'  )
               END IF




      CSOIL = CSOIL_DATA
      BEXP = BB (SOILTYP)
      DKSAT = SATDK (SOILTYP)
      DWSAT = SATDW (SOILTYP)
      F1 = F11 (SOILTYP)
      PSISAT = SATPSI (SOILTYP)
      QUARTZ = QTZ (SOILTYP)
      SMCDRY = DRYSMC (SOILTYP)
      SMCMAX = MAXSMC (SOILTYP)
      SMCREF = REFSMC (SOILTYP)
      SMCWLT = WLTSMC (SOILTYP)




      ZBOT = ZBOT_DATA
      SALP = SALP_DATA
      SBETA = SBETA_DATA
      REFDK = REFDK_DATA
      FRZK = FRZK_DATA
      FXEXP = FXEXP_DATA
      REFKDT = REFKDT_DATA
      PTU = 0.    
      KDT = REFKDT * DKSAT / REFDK
      CZIL = CZIL_DATA
      SLOPE = SLOPE_DATA (SLOPETYP)
      LVCOEF = LVCOEF_DATA




      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)
      FRZX = FRZK * FRZFACT




      TOPT = TOPT_DATA
      CMCMAX = CMCMAX_DATA
      CFACTR = CFACTR_DATA
      RSMAX = RSMAX_DATA
      NROOT = NROTBL (VEGTYP)
      SNUP = SNUPTBL (VEGTYP)
      RSMIN = RSTBL (VEGTYP)
      RGL = RGLTBL (VEGTYP)
      HS = HSTBL (VEGTYP)
      EMISSMIN  = EMISSMINTBL  (VEGTYP)
      EMISSMAX  = EMISSMAXTBL  (VEGTYP)
      LAIMIN    = LAIMINTBL    (VEGTYP)
      LAIMAX    = LAIMAXTBL    (VEGTYP)
      Z0MIN     = Z0MINTBL     (VEGTYP)
      Z0MAX     = Z0MAXTBL     (VEGTYP)
      ALBEDOMIN = ALBEDOMINTBL (VEGTYP)
      ALBEDOMAX = ALBEDOMAXTBL (VEGTYP)
      ZTOPV     = ZTOPVTBL     (VEGTYP)
      ZBOTV     = ZBOTVTBL     (VEGTYP)

               IF (VEGTYP .eq. BARE) SHDFAC = 0.0
               IF (NROOT .gt. NSOIL) THEN
                  WRITE (err_message,*) 'Error: too many root layers ',  &
                                                 NSOIL,NROOT
                  call wrf_error_fatal3("<stdin>",2491,&
err_message  )




               END IF
               DO I = 1,NROOT
                  RTDIS (I) = - SLDPTH (I)/ ZSOIL (NROOT)



               END DO
















      END  SUBROUTINE REDPRM

      SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NSOIL)



















      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: NSOIL
      INTEGER               :: K, KK

      REAL, DIMENSION(1:NSOIL), INTENT(IN):: A, B, D
      REAL, DIMENSION(1:NSOIL),INTENT(INOUT):: C,P,DELTA




      C (NSOIL) = 0.0
      P (1) = - C (1) / B (1)







      DELTA (1) = D (1) / B (1)
      DO K = 2,NSOIL
         P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
         DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
                    * P (K -1)))
      END DO



      P (NSOIL) = DELTA (NSOIL)




      DO K = 2,NSOIL
         KK = NSOIL - K + 1
         P (KK) = P (KK) * P (KK +1) + DELTA (KK)
      END DO

  END SUBROUTINE ROSR12



      SUBROUTINE SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL, &
                         TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,     &
                         QUARTZ,CSOIL,VEGTYP,ISURBAN,SOILTYP,OPT_THCND &
                        ,HCPCT_FASDAS                                  ) 








      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: OPT_THCND
      INTEGER, INTENT(IN)   :: NSOIL, VEGTYP, ISURBAN, SOILTYP
      INTEGER               :: I

      REAL, INTENT(IN)      :: BEXP,CSOIL,DF1,DT,F1,PSISAT,QUARTZ,     &
                               SMCMAX, SMCWLT, TBOT,YY, ZBOT,ZZ1
      REAL, INTENT(INOUT)   :: T1
      REAL, INTENT(OUT)     :: SSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SMC,ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: STC
      REAL, DIMENSION(1:NSOIL)             :: AI, BI, CI, STCF,RHSTS
      REAL, PARAMETER       :: T0 = 273.15




      REAL, INTENT(  OUT)     :: HCPCT_FASDAS







      

      CALL HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,TBOT,     &
                ZBOT,PSISAT,SH2O,DT,BEXP,SOILTYP,OPT_THCND,       &
                F1,DF1,QUARTZ,CSOIL,AI,BI,CI,VEGTYP,ISURBAN       &
               ,HCPCT_FASDAS                                      ) 

      CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)

      DO I = 1,NSOIL
         STC (I) = STCF (I)
      ENDDO











      T1 = (YY + (ZZ1- 1.0) * STC (1)) / ZZ1
      SSOIL = DF1 * (STC (1) - T1) / (0.5 * ZSOIL (1))


  END SUBROUTINE SHFLX


      SUBROUTINE SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                   &
     &                   SH2O,SLOPE,KDT,FRZFACT,                        &
     &                   SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                &
     &                   SHDFAC,CMCMAX,                                 &
     &                   RUNOFF1,RUNOFF2,RUNOFF3,                       &
     &                   EDIR,EC,ET,                                    &
     &                   DRIP, SFHEAD1RT,INFXS1RT,                      &
                         IRRIGATION_CHANNEL )










      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: NSOIL
      INTEGER               :: I,K

      REAL, INTENT(IN)      :: BEXP, CMCMAX, DKSAT,DWSAT, DT, EC, EDIR,  &
                               KDT, PRCP1, SHDFAC, SLOPE, SMCMAX, SMCWLT
      REAL, INTENT(OUT)                      :: DRIP, RUNOFF1, RUNOFF2, RUNOFF3
      REAL, INTENT(INOUT)   :: CMC
      REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ET,ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT):: SMC, SH2O
      REAL, DIMENSION(1:NSOIL)             :: AI, BI, CI, STCF,RHSTS, RHSTT, &
                                              SICE, SH2OA, SH2OFG, SH2OIN
      REAL                  :: DUMMY, EXCESS,FRZFACT,PCPDRP,RHSCT,TRHSCT
      REAL :: FAC2
      REAL :: FLIMIT
      REAL, INTENT(IN),OPTIONAL      :: IRRIGATION_CHANNEL
      REAL,    INTENT(INOUT)                 :: SFHEAD1RT,INFXS1RT







      DUMMY = 0.






      RHSCT = SHDFAC * PRCP1- EC
      DRIP = 0.
      TRHSCT = DT * RHSCT
      EXCESS = CMC + TRHSCT





      IF (EXCESS > CMCMAX) DRIP = EXCESS - CMCMAX
      PCPDRP = (1. - SHDFAC) * PRCP1+ DRIP / DT
      IF(PRESENT(IRRIGATION_CHANNEL)) THEN
         IF (IRRIGATION_CHANNEL.NE.0.)PCPDRP =PCPDRP+ 0.001*IRRIGATION_CHANNEL 
      END IF



      DO I = 1,NSOIL
         SICE (I) = SMC (I) - SH2O (I)
      END DO























      FAC2=0.0
      DO I=1,NSOIL
         FAC2=MAX(FAC2,SH2O(I)/SMCMAX)
      ENDDO
      CALL FAC2MIT(SMCMAX,FLIMIT)










      IF ( ( (PCPDRP * DT) > (0.0001*1000.0* (- ZSOIL (1))* SMCMAX) )   &
           .OR. (FAC2 > FLIMIT) ) THEN
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,          &
                    DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &
                    RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI,  &
                    SFHEAD1RT,INFXS1RT)
         CALL SSTEP (SH2OFG,SH2O,DUMMY,RHSTT,RHSCT,DT,NSOIL,SMCMAX,     &
                        CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI,INFXS1RT)
         DO K = 1,NSOIL
            SH2OA (K) = (SH2O (K) + SH2OFG (K)) * 0.5
         END DO
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,ZSOIL,         &
                    DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &
                    RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI,  &
                    SFHEAD1RT,INFXS1RT)
         SH2OIN=SH2O
         CALL SSTEP (SH2O,SH2OIN,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,       &
                        CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI,INFXS1RT)

      ELSE
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,          &
                    DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &
                    RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI,  &
                   SFHEAD1RT,INFXS1RT)
         SH2OIN=SH2O
         CALL SSTEP (SH2O,SH2OIN,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,       &
                     CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI,INFXS1RT)


      END IF


  END SUBROUTINE SMFLX



      SUBROUTINE SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR, &
                         XLAI,SHDFAC,FVB,GAMA,FBUR,    &
                         FGSN,ZTOPV,ZBOTV,UA_PHYS)










      IMPLICIT NONE

      REAL, INTENT(IN)     :: SNEQV,SNUP,SALP,SNOWH
      REAL, INTENT(OUT)    :: SNCOVR
      REAL                 :: RSNOW, Z0N
      LOGICAL, INTENT(IN)  :: UA_PHYS  
      REAL, INTENT(IN)     :: ZTOPV    
      REAL, INTENT(IN)     :: ZBOTV    
      REAL, INTENT(IN)     :: SHDFAC   
      REAL, INTENT(INOUT)  :: XLAI     
      REAL, INTENT(OUT)    :: FVB      
      REAL, INTENT(OUT)    :: GAMA     
      REAL, INTENT(OUT)    :: FBUR     
      REAL, INTENT(OUT)    :: FGSN     

      REAL ::  SNUPGRD = 0.02          





      IF (SNEQV < SNUP) THEN
         RSNOW = SNEQV / SNUP
         SNCOVR = 1. - ( EXP ( - SALP * RSNOW) - RSNOW * EXP ( - SALP))
      ELSE
         SNCOVR = 1.0
      END IF









      IF(UA_PHYS) THEN




        IF (SNEQV < SNUPGRD) THEN
         FGSN = SNEQV / SNUPGRD
        ELSE
         FGSN = 1.0
        END IF






        IF(ZBOTV > 0. .AND. SNOWH > ZBOTV) THEN
          IF(ZBOTV <= 0.5) THEN
            FBUR = (SNOWH - 0.4*ZBOTV) / (0.4*(ZTOPV-ZBOTV)) 
          ELSE
            FBUR = (SNOWH - ZBOTV) / (ZTOPV-ZBOTV)           
          ENDIF
        ELSE
          FBUR = 0.
        ENDIF

        FBUR = MIN(MAX(FBUR,0.0),1.0)


        XLAI = XLAI * (1.0 - FBUR)









        FVB = SHDFAC * FGSN * (1.0 - FBUR)



        GAMA = EXP(-1.* XLAI)
      ELSE
        
        FVB  = 0.0
        GAMA = 0.0
        FBUR = 0.0
        FGSN = 0.0
      END IF    


  END SUBROUTINE SNFRAC


      SUBROUTINE SNKSRC (TSNSR,TAVG,SMC,SH2O,ZSOIL,NSOIL,               &
     &                      SMCMAX,PSISAT,BEXP,DT,K,QTOT)






      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: K,NSOIL
      REAL, INTENT(IN)      :: BEXP, DT, PSISAT, QTOT, SMC, SMCMAX,    &
                               TAVG
      REAL, INTENT(INOUT)   :: SH2O

      REAL, DIMENSION(1:NSOIL), INTENT(IN):: ZSOIL

      REAL                  :: DF, DZ, DZH, FREE, TSNSR,               &
                               TDN, TM, TUP, TZ, X0, XDN, XH2O, XUP

      REAL, PARAMETER       :: DH2O = 1.0000E3, HLICE = 3.3350E5,      &
                               T0 = 2.7315E2

      IF (K == 1) THEN
         DZ = - ZSOIL (1)
      ELSE
         DZ = ZSOIL (K -1) - ZSOIL (K)
      END IF


















      CALL FRH2O (FREE,TAVG,SMC,SH2O,SMCMAX,BEXP,PSISAT)






      XH2O = SH2O + QTOT * DT / (DH2O * HLICE * DZ)
      IF ( XH2O < SH2O .AND. XH2O < FREE) THEN
         IF ( FREE > SH2O ) THEN
            XH2O = SH2O
         ELSE
            XH2O = FREE
         END IF
      END IF





      IF ( XH2O > SH2O .AND. XH2O > FREE ) THEN
         IF ( FREE < SH2O ) THEN
            XH2O = SH2O
         ELSE
            XH2O = FREE
         END IF
      END IF






      IF (XH2O < 0.) XH2O = 0.
      IF (XH2O > SMC) XH2O = SMC
      TSNSR = - DH2O * HLICE * DZ * (XH2O - SH2O)/ DT
      SH2O = XH2O


  END SUBROUTINE SNKSRC


      SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,SMC,SMCMAX,SMCWLT,   &
                          SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,            &
                          SBETA,DF1,                                    &
                          Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,&
                         SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,ESD,SNDENS,&
                          SNOWH,SH2O,SLOPE,KDT,FRZFACT,PSISAT,          &
                          ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,   &
                          RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,  &
                          RTDIS,QUARTZ,FXEXP,CSOIL,                     &
                          BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW,ETNS,EMISSI,&
                          RIBB,SOLDN,                                   &
                          ISURBAN,                                      &
                          VEGTYP,                                       &
                          ETPN,FLX4,UA_PHYS,                            &
                          SFHEAD1RT,INFXS1RT,ETPND1,SOILTYP,OPT_THCND   &
                         ,QFX_PHY,fasdas,HCPCT_FASDAS                   ) 







      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: OPT_THCND
      INTEGER, INTENT(IN)   :: NROOT, NSOIL,VEGTYP,SOILTYP
      INTEGER, INTENT(IN)   :: ISURBAN
      INTEGER               :: K



      INTEGER               :: IT16
      LOGICAL, INTENT(IN)   :: SNOWNG


       REAL, INTENT(INOUT)    ::  SFHEAD1RT,INFXS1RT,ETPND1

      REAL, INTENT(IN)      :: BEXP,CFACTR, CMCMAX,CSOIL,DF1,DKSAT,     &
                               DT,DWSAT, EPSCA,FDOWN,F1,FXEXP,          &
                               FRZFACT,KDT,PC, PRCP,PSISAT,Q2,QUARTZ,   &
                               RCH,RR,SBETA,SFCPRS, SFCTMP, SHDFAC,     &
                               SLOPE,SMCDRY,SMCMAX,SMCREF,SMCWLT, T24,  &
                               TBOT,TH2,ZBOT,EMISSI,SOLDN
      REAL, INTENT(INOUT)   :: CMC, BETA, ESD,FLX2,PRCPF,SNOWH,SNCOVR,  &
                               SNDENS, T1, RIBB, ETP
      REAL, INTENT(OUT)     :: DEW,DRIP,EC,EDIR, ETNS, ESNOW,ETT,       &
                               FLX1,FLX3, RUNOFF1,RUNOFF2,RUNOFF3,      &
                               SSOIL,SNOMLT
      REAL, DIMENSION(1:NSOIL),INTENT(IN)     :: RTDIS,ZSOIL
      REAL, DIMENSION(1:NSOIL),INTENT(OUT)    :: ET
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC,SH2O,STC
      REAL, DIMENSION(1:NSOIL) :: ET1
      REAL                  :: DENOM,DSOIL,DTOT,EC1,EDIR1,ESDFLX,ETA,   &
                               ETT1, ESNOW1, ESNOW2, ETA1,ETP1,ETP2,    &
                               ETP3, ETNS1, ETANRG, ETAX, EX, FLX3X,    &
                               FRCSNO,FRCSOI, PRCP1, QSAT,RSNOW, SEH,   &
                               SNCOND,SSOIL1, T11,T12, T12A, T12AX,     &
                               T12B, T14, YY, ZZ1




      REAL                  :: T15, T16, DTOT2
      REAL, PARAMETER       :: ESDMIN = 1.E-6, LSUBC = 2.501000E+6,     &
                               LSUBS = 2.83E+6, TFREEZ = 273.15,        &
                               SNOEXP = 2.0
      LOGICAL, INTENT(IN)   :: UA_PHYS  
      REAL, INTENT(INOUT)   :: FLX4     
      REAL, INTENT(IN)      :: ETPN     
      REAL                  :: ETP1N    




      REAL                  :: QFX_PHY
      INTEGER               :: fasdas
      REAL, INTENT(  OUT)   :: HCPCT_FASDAS


















      DEW = 0.
      EDIR = 0.
      EDIR1 = 0.
      EC1 = 0.
      EC = 0.


      DO K = 1,NSOIL
         ET (K) = 0.
         ET1 (K) = 0.
      END DO
      ETT = 0.
      ETT1 = 0.


      ETPND1 = 0.


      ETNS = 0.
      ETNS1 = 0.
      ESNOW = 0.
      ESNOW1 = 0.
      ESNOW2 = 0.




      PRCP1 = PRCPF *0.001



      BETA = 1.0
      IF (ETP <= 0.0) THEN
         IF ( ( RIBB >= 0.1 ) .AND. ( FDOWN > 150.0 ) ) THEN
            ETP=(MIN(ETP*(1.0-RIBB),0.)*SNCOVR/0.980 + ETP*(0.980-SNCOVR))/0.980
         ENDIF
         IF(ETP == 0.) BETA = 0.0
         ETP1 = ETP * 0.001
         IF(UA_PHYS) ETP1N = ETPN * 0.001
         DEW = -ETP1
         ESNOW2 = ETP1*DT
         ETANRG = ETP*((1.-SNCOVR)*LSUBC + SNCOVR*LSUBS)
      ELSE
         ETP1 = ETP * 0.001
         IF(UA_PHYS) ETP1N = ETPN * 0.001
         
         IF (SNCOVR <  1.) THEN
            CALL EVAPO (ETNS1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,           &
                         SH2O,                                       &
                         SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,          &
                         SMCREF,SHDFAC,CMCMAX,                       &
                         SMCDRY,CFACTR,                              &
                         EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,   &
                         FXEXP, SFHEAD1RT,ETPND1)

            EDIR1 = EDIR1* (1. - SNCOVR)
            EC1 = EC1* (1. - SNCOVR)
            DO K = 1,NSOIL
               ET1 (K) = ET1 (K)* (1. - SNCOVR)
            END DO
            ETT1 = ETT1*(1.-SNCOVR)

            ETNS1 = ETNS1*(1.-SNCOVR)

            EDIR = EDIR1*1000.
            EC = EC1*1000.
            DO K = 1,NSOIL
               ET (K) = ET1 (K)*1000.
            END DO



            if( fasdas ==  1 ) then
              QFX_PHY = EDIR + EC
              DO K=1,NSOIL
                 QFX_PHY = QFX_PHY + ET(K)
              END DO
            endif



            ETT = ETT1*1000.
            ETNS = ETNS1*1000.



               ETPND1 = ETPND1*1000.




         ENDIF
         ESNOW = ETP*SNCOVR
         IF(UA_PHYS) ESNOW = ETPN*SNCOVR   
         ESNOW1 = ESNOW*0.001
         ESNOW2 = ESNOW1*DT
         ETANRG = ESNOW*LSUBS + ETNS*LSUBC
      ENDIF







      FLX1 = 0.0
      IF (SNOWNG) THEN
         FLX1 = CPICE * PRCP * (T1- SFCTMP)
      ELSE
         IF (PRCP >  0.0) FLX1 = CPH2O * PRCP * (T1- SFCTMP)








      END IF
      DSOIL = - (0.5 * ZSOIL (1))
      DTOT = SNOWH + DSOIL
      DENOM = 1.0+ DF1 / (DTOT * RR * RCH)




      T12A = ( (FDOWN - FLX1- FLX2- EMISSI * SIGMA * T24)/ RCH                    &
                + TH2- SFCTMP - ETANRG / RCH ) / RR

      T12B = DF1 * STC (1) / (DTOT * RR * RCH)













      T12 = (SFCTMP + T12A + T12B) / DENOM
      IF (T12 <=  TFREEZ) THEN
         T1 = T12
         SSOIL = DF1 * (T1- STC (1)) / DTOT

         ESD = MAX(0.0, ESD-ESNOW2)
         FLX3 = 0.0
         EX = 0.0

         SNOMLT = 0.0
         IF(UA_PHYS) FLX4 = 0.0















      ELSE


         T1 = TFREEZ * max(0.01,SNCOVR ** SNOEXP) + T12 * (1.0- max(0.01,SNCOVR ** SNOEXP))
         BETA = 1.0






         SSOIL = DF1 * (T1- STC (1)) / DTOT
         IF (ESD-ESNOW2 <= ESDMIN) THEN
            ESD = 0.0
            EX = 0.0
            SNOMLT = 0.0
            FLX3 = 0.0
            IF(UA_PHYS) FLX4 = 0.0




         ELSE
            ESD = ESD-ESNOW2
            ETP3 = ETP * LSUBC
            SEH = RCH * (T1- TH2)
            T14 = T1* T1
            T14 = T14* T14



            FLX3 = FDOWN - FLX1- FLX2- EMISSI*SIGMA * T14- SSOIL - SEH - ETANRG
            IF (FLX3 <= 0.0) FLX3 = 0.0

            IF(UA_PHYS .AND. FLX4 > 0. .AND. FLX3 > 0.) THEN
              IF(FLX3 >= FLX4) THEN
                FLX3 = FLX3 - FLX4
              ELSE
                FLX4 = FLX3
                FLX3 = 0.
              ENDIF
            ELSE
              FLX4 = 0.0
            ENDIF




            EX = FLX3*0.001/ LSUBF





            SNOMLT = EX * DT
            IF (ESD- SNOMLT >=  ESDMIN) THEN
               ESD = ESD- SNOMLT



            ELSE
               EX = ESD / DT
               FLX3 = EX *1000.0* LSUBF
               SNOMLT = ESD

               ESD = 0.0



            END IF
         END IF











         PRCP1 = PRCP1+ EX







      END IF
      CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &
                   SH2O,SLOPE,KDT,FRZFACT,                           &
                   SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &
                   SHDFAC,CMCMAX,                                    &
                   RUNOFF1,RUNOFF2,RUNOFF3,                          &
                   EDIR1,EC1,ET1,                                    &
                   DRIP, SFHEAD1RT,INFXS1RT)








      ZZ1 = 1.0
      YY = STC (1) -0.5* SSOIL * ZSOIL (1)* ZZ1/ DF1








      T11 = T1
      CALL SHFLX (SSOIL1,STC,SMC,SMCMAX,NSOIL,T11,DT,YY,ZZ1,ZSOIL,     &
                   TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,           &
                   QUARTZ,CSOIL,VEGTYP,ISURBAN,SOILTYP,OPT_THCND       &
                  ,HCPCT_FASDAS                                        ) 





      
      IF (ESD >  0.) THEN
         CALL SNOWPACK (ESD,DT,SNOWH,SNDENS,T1,YY,SNOMLT,UA_PHYS)
      ELSE
         ESD = 0.
         SNOWH = 0.
         SNDENS = 0.
         SNCOND = 1.
         SNCOVR = 0.
      END IF


  END SUBROUTINE SNOPAC



      SUBROUTINE SNOWPACK (ESD,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL,SNOMLT,UA_PHYS)


















      IMPLICIT NONE

      INTEGER                :: IPOL, J
      REAL, INTENT(IN)       :: ESD, DTSEC,TSNOW,TSOIL
      REAL, INTENT(INOUT)    :: SNOWH, SNDENS
      REAL                   :: BFAC,DSX,DTHR,DW,SNOWHC,PEXP,           &
                                TAVGC,TSNOWC,TSOILC,ESDC,ESDCX
      REAL, PARAMETER        :: C1 = 0.01, C2 = 21.0, G = 9.81,         &
                                KN = 4000.0
      LOGICAL, INTENT(IN)    :: UA_PHYS  
      REAL, INTENT(IN)       :: SNOMLT   
      REAL                   :: SNOMLTC  



      SNOWHC = SNOWH *100.
      ESDC = ESD *100.
      IF(UA_PHYS) SNOMLTC = SNOMLT *100.
      DTHR = DTSEC /3600.
      TSNOWC = TSNOW -273.15
      TSOILC = TSOIL -273.15













      TAVGC = 0.5* (TSNOWC + TSOILC)
      IF (ESDC >  1.E-2) THEN
         ESDCX = ESDC
      ELSE
         ESDCX = 1.E-2
      END IF



















      BFAC = DTHR * C1* EXP (0.08* TAVGC - C2* SNDENS)
      IPOL = 4
      PEXP = 0.

      DO J = IPOL,1, -1
         PEXP = (1. + PEXP)* BFAC * ESDCX / REAL (J +1)
      END DO

      PEXP = PEXP + 1.




















      DSX = SNDENS * (PEXP)
      IF (DSX > 0.40) DSX = 0.40
      IF (DSX < 0.05) DSX = 0.05





      SNDENS = DSX
      IF (TSNOWC >=  0.) THEN
         DW = 0.13* DTHR /24.
         IF ( UA_PHYS .AND. TSOILC >= 0.) THEN
             DW = MIN (DW, 0.13*SNOMLTC/(ESDCX+0.13*SNOMLTC))
         ENDIF
         SNDENS = SNDENS * (1. - DW) + DW
         IF (SNDENS >=  0.40) SNDENS = 0.40




      END IF
      SNOWHC = ESDC / SNDENS
      SNOWH = SNOWHC *0.01


  END SUBROUTINE SNOWPACK


      SUBROUTINE SNOWZ0 (SNCOVR,Z0, Z0BRD, SNOWH,FBUR,FGSN,SHDMAX,UA_PHYS)









      IMPLICIT NONE
      REAL, INTENT(IN)        :: SNCOVR, Z0BRD
      REAL, INTENT(OUT)       :: Z0
      REAL, PARAMETER         :: Z0S=0.001
      REAL, INTENT(IN)        :: SNOWH
      REAL                    :: BURIAL
      REAL                    :: Z0EFF
      LOGICAL, INTENT(IN)     :: UA_PHYS   
      REAL, INTENT(IN)        :: FBUR      
      REAL, INTENT(IN)        :: FGSN      
      REAL, INTENT(IN)        :: SHDMAX    
      REAL, PARAMETER         :: Z0G=0.01  
      REAL                    :: FV,A1,A2

      IF(UA_PHYS) THEN

          FV = SHDMAX * (1.-FBUR)
          A1 = (1.-FV)**2*((1.-FGSN**2)*LOG(Z0G) + (FGSN**2)*LOG(Z0S))
          A2 = (1.-(1.-FV)**2)*LOG(Z0BRD)
          Z0 = EXP(A1+A2)

      ELSE


          BURIAL = 7.0*Z0BRD - SNOWH
          IF(BURIAL.LE.0.0007) THEN
              Z0EFF = Z0S
          ELSE      
              Z0EFF = BURIAL/7.0
          ENDIF
      
          Z0 = (1.- SNCOVR)* Z0BRD + SNCOVR * Z0EFF

      ENDIF

  END SUBROUTINE SNOWZ0



      SUBROUTINE SNOW_NEW (TEMP,NEWSN,SNOWH,SNDENS)












      IMPLICIT NONE
      REAL, INTENT(IN)        :: NEWSN, TEMP
      REAL, INTENT(INOUT)     :: SNDENS, SNOWH
      REAL                    :: DSNEW, HNEWC, SNOWHC,NEWSNC,TEMPC




      SNOWHC = SNOWH *100.
      NEWSNC = NEWSN *100.







      TEMPC = TEMP -273.15
      IF (TEMPC <=  -15.) THEN
         DSNEW = 0.05
      ELSE
         DSNEW = 0.05+0.0017* (TEMPC +15.)**1.5
      END IF



      HNEWC = NEWSNC / DSNEW
      IF (SNOWHC + HNEWC .LT. 1.0E-3) THEN
         SNDENS = MAX(DSNEW,SNDENS)
      ELSE
         SNDENS = (SNOWHC * SNDENS + HNEWC * DSNEW)/ (SNOWHC + HNEWC)
      ENDIF
      SNOWHC = SNOWHC + HNEWC
      SNOWH = SNOWHC *0.01


  END SUBROUTINE SNOW_NEW


      SUBROUTINE SRT (RHSTT,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,            &
                       ZSOIL,DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,           &
                     RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZX,SICE,AI,BI,CI,  &
                     SFHEAD1RT,INFXS1RT )








      IMPLICIT NONE
      INTEGER, INTENT(IN)       :: NSOIL
      INTEGER                   :: IALP1, IOHINF, J, JJ,  K, KS


       REAL, INTENT(INOUT)     :: SFHEAD1RT, INFXS1RT
       REAL                    :: SFCWATR,chcksm



      REAL, INTENT(IN)          :: BEXP, DKSAT, DT, DWSAT, EDIR, FRZX,  &
                                   KDT, PCPDRP, SLOPE, SMCMAX, SMCWLT
      REAL, INTENT(OUT)         :: RUNOFF1, RUNOFF2
      REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ET, SH2O, SH2OA, SICE,  &
                                                ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: RHSTT
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: AI, BI, CI
      REAL, DIMENSION(1:NSOIL)  :: DMAX
      REAL                      :: ACRT, DD, DDT, DDZ, DDZ2, DENOM,     &
                                   DENOM2,DICE, DSMDZ, DSMDZ2, DT1,     &
                                   FCR,INFMAX,MXSMC,MXSMC2,NUMER,PDDUM, &
                                   PX, SICEMAX,SLOPX, SMCAV, SSTT,      &
                                   SUM, VAL, WCND, WCND2, WDF, WDF2
      INTEGER, PARAMETER        :: CVFRZ = 3





















      IOHINF = 1
      SICEMAX = 0.0
      DO KS = 1,NSOIL
         IF (SICE (KS) >  SICEMAX) SICEMAX = SICE (KS)



      END DO

      PDDUM = PCPDRP
      RUNOFF1 = 0.0








     IF (PCPDRP /=  0.0) THEN
         DT1 = DT /86400.
         SMCAV = SMCMAX - SMCWLT




         DMAX (1)= - ZSOIL (1)* SMCAV

         DICE = - ZSOIL (1) * SICE (1)
         DMAX (1)= DMAX (1)* (1.0- (SH2OA (1) + SICE (1) - SMCWLT)/      &
                    SMCAV)

         DD = DMAX (1)




         DO KS = 2,NSOIL

            DICE = DICE+ ( ZSOIL (KS -1) - ZSOIL (KS) ) * SICE (KS)
            DMAX (KS) = (ZSOIL (KS -1) - ZSOIL (KS))* SMCAV
            DMAX (KS) = DMAX (KS)* (1.0- (SH2OA (KS) + SICE (KS)        &
                        - SMCWLT)/ SMCAV)
            DD = DD+ DMAX (KS)




         END DO
         VAL = (1. - EXP ( - KDT * DT1))
         DDT = DD * VAL
         PX = PCPDRP * DT
         IF (PX <  0.0) PX = 0.0







         INFMAX = (PX * (DDT / (PX + DDT)))/ DT
         FCR = 1.
         IF (DICE >  1.E-2) THEN
            ACRT = CVFRZ * FRZX / DICE
            SUM = 1.
            IALP1 = CVFRZ - 1
            DO J = 1,IALP1
               K = 1
               DO JJ = J +1,IALP1
                  K = K * JJ
               END DO
               SUM = SUM + (ACRT ** ( CVFRZ - J)) / FLOAT (K)
            END DO
            FCR = 1. - EXP ( - ACRT) * SUM
         END IF







         INFMAX = INFMAX * FCR

         MXSMC = SH2OA (1)
         CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,           &
                         SICEMAX)
         INFMAX = MAX (INFMAX,WCND)

         INFMAX = MIN (INFMAX,PX/DT)
         IF (PCPDRP >  INFMAX) THEN
            RUNOFF1 = PCPDRP - INFMAX
          INFXS1RT = RUNOFF1*DT*1000.
          PDDUM = INFMAX
         END IF






      END IF

      MXSMC = SH2OA (1)
      CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,              &
                    SICEMAX)



      DDZ = 1. / ( - .5 * ZSOIL (2) )
      AI (1) = 0.0
      BI (1) = WDF * DDZ / ( - ZSOIL (1) )





      CI (1) = - BI (1)
      DSMDZ = ( SH2O (1) - SH2O (2) ) / ( - .5 * ZSOIL (2) )
      RHSTT (1) = (WDF * DSMDZ + WCND- PDDUM + EDIR + ET (1))/ ZSOIL (1)




      SSTT = WDF * DSMDZ + WCND+ EDIR + ET (1)




      DDZ2 = 0.0
      DO K = 2,NSOIL
         DENOM2 = (ZSOIL (K -1) - ZSOIL (K))
         IF (K /= NSOIL) THEN






            SLOPX = 1.

            MXSMC2 = SH2OA (K)
            CALL WDFCND (WDF2,WCND2,MXSMC2,SMCMAX,BEXP,DKSAT,DWSAT,     &
                          SICEMAX)



            DENOM = (ZSOIL (K -1) - ZSOIL (K +1))




            DSMDZ2 = (SH2O (K) - SH2O (K +1)) / (DENOM * 0.5)
            DDZ2 = 2.0 / DENOM
            CI (K) = - WDF2 * DDZ2 / DENOM2

         ELSE








            SLOPX = SLOPE
          CALL WDFCND (WDF2,WCND2,SH2OA (NSOIL),SMCMAX,BEXP,DKSAT,DWSAT,     &
                            SICEMAX)








            DSMDZ2 = 0.0
            CI (K) = 0.0



         END IF
         NUMER = (WDF2 * DSMDZ2) + SLOPX * WCND2- (WDF * DSMDZ)         &
                 - WCND+ ET (K)




         RHSTT (K) = NUMER / ( - DENOM2)
         AI (K) = - WDF * DDZ / DENOM2





         BI (K) = - ( AI (K) + CI (K) )
         IF (K .eq. NSOIL) THEN
            RUNOFF2 = SLOPX * WCND2
         END IF
         IF (K .ne. NSOIL) THEN
            WDF = WDF2
            WCND = WCND2
            DSMDZ = DSMDZ2
            DDZ = DDZ2
         END IF
      END DO

  END SUBROUTINE SRT


      SUBROUTINE SSTEP (SH2OOUT,SH2OIN,CMC,RHSTT,RHSCT,DT,              &
                        NSOIL,SMCMAX,CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,     &
                        AI,BI,CI, INFXS1RT)







      IMPLICIT NONE
      INTEGER, INTENT(IN)       :: NSOIL
      INTEGER                   :: I, K, KK11


      REAL, INTENT(INOUT)       :: INFXS1RT
      REAL                      :: AVAIL

      REAL, INTENT(IN)          :: CMCMAX, DT, SMCMAX
      REAL, INTENT(OUT)         :: RUNOFF3
      REAL, INTENT(INOUT)       :: CMC
      REAL, DIMENSION(1:NSOIL), INTENT(IN)     :: SH2OIN, SICE, ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)    :: SH2OOUT
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT)  :: RHSTT, SMC
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT)  :: AI, BI, CI
      REAL, DIMENSION(1:NSOIL)  :: RHSTTin
      REAL, DIMENSION(1:NSOIL)  :: CIin
      REAL                      :: DDZ, RHSCT, STOT, WPLUS





      DO K = 1,NSOIL
         RHSTT (K) = RHSTT (K) * DT
         AI (K) = AI (K) * DT
         BI (K) = 1. + BI (K) * DT
         CI (K) = CI (K) * DT
      END DO



      DO K = 1,NSOIL
         RHSTTin (K) = RHSTT (K)
      END DO
      DO K = 1,NSOIL
         CIin (K) = CI (K)
      END DO



      CALL ROSR12 (CI,AI,BI,CIin,RHSTTin,RHSTT,NSOIL)





      WPLUS = 0.0
      RUNOFF3 = 0.

      DDZ = - ZSOIL (1)
      DO K = 1,NSOIL
         IF (K /= 1) DDZ = ZSOIL (K - 1) - ZSOIL (K)
         SH2OOUT (K) = SH2OIN (K) + CI (K) + WPLUS / DDZ
         STOT = SH2OOUT (K) + SICE (K)
         IF (STOT > SMCMAX) THEN
            IF (K .eq. 1) THEN
               DDZ = - ZSOIL (1)
            ELSE
               KK11 = K - 1
               DDZ = - ZSOIL (K) + ZSOIL (KK11)
            END IF
            WPLUS = (STOT - SMCMAX) * DDZ
         ELSE
            WPLUS = 0.
         END IF
         SMC (K) = MAX ( MIN (STOT,SMCMAX),0.02 )
         SH2OOUT (K) = MAX ( (SMC (K) - SICE (K)),0.0)
      END DO






      RUNOFF3 = WPLUS
      CMC = CMC + DT * RHSCT
      IF (CMC < 1.E-20) CMC = 0.0
      CMC = MIN (CMC,CMCMAX)


  END SUBROUTINE SSTEP


      SUBROUTINE TBND (TU,TB,ZSOIL,ZBOT,K,NSOIL,TBND1)







      IMPLICIT NONE
      INTEGER, INTENT(IN)       :: NSOIL
      INTEGER                   :: K
      REAL, INTENT(IN)          :: TB, TU, ZBOT
      REAL, INTENT(OUT)         :: TBND1
      REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ZSOIL
      REAL                      :: ZB, ZUP
      REAL, PARAMETER           :: T0 = 273.15




     IF (K == 1) THEN
         ZUP = 0.
      ELSE
         ZUP = ZSOIL (K -1)
      END IF




      IF (K ==  NSOIL) THEN
         ZB = 2.* ZBOT - ZSOIL (K)
      ELSE
         ZB = ZSOIL (K +1)
      END IF




      TBND1 = TU + (TB - TU)* (ZUP - ZSOIL (K))/ (ZUP - ZB)

  END SUBROUTINE TBND



      SUBROUTINE TDFCND ( DF, SMC, QZ, SMCMAX, SH2O, BEXP, PSISAT, SOILTYP, OPT_THCND)










      IMPLICIT NONE
      INTEGER, INTENT(IN)       :: SOILTYP, OPT_THCND  
      REAL, INTENT(IN)          :: QZ,  SMC, SMCMAX, SH2O, BEXP, PSISAT
      REAL, INTENT(OUT)         :: DF
      REAL                      :: AKE, GAMMD, THKDRY, THKICE, THKO,    &
                                   THKQTZ,THKSAT,THKS,THKW,SATRATIO,XU, &
                                   XUNFROZ,AKEI,AKEL,PSIF,PF  































IF ( OPT_THCND == 1 .OR. ( OPT_THCND == 2 .AND. (SOILTYP /= 4 .AND. SOILTYP /= 3)) )THEN






      SATRATIO = SMC / SMCMAX

      THKICE = 2.2

      THKW = 0.57


      THKO = 2.0

      THKQTZ = 7.7

      THKS = (THKQTZ ** QZ)* (THKO ** (1. - QZ))


      XUNFROZ = SH2O / SMC

      XU = XUNFROZ * SMCMAX


      THKSAT = THKS ** (1. - SMCMAX)* THKICE ** (SMCMAX - XU)* THKW **   &
         (XU)


      GAMMD = (1. - SMCMAX)*2700.


      THKDRY = (0.135* GAMMD+ 64.7)/ (2700. - 0.947* GAMMD)

         AKEI = SATRATIO







         IF ( SATRATIO >  0.1 ) THEN

            AKEL = LOG10 (SATRATIO) + 1.0


         ELSE

            AKEL = 0.0
         END IF
      AKE = ((SMC-SH2O)*AKEI + SH2O*AKEL)/SMC



      DF = AKE * (THKSAT - THKDRY) + THKDRY

    ELSE
 


         PSIF = PSISAT*100.*(SMCMAX/(SMC))**BEXP

           PF=log10(abs(PSIF))

         IF(PF.LE.5.1) THEN
           DF=420.*EXP(-(PF+2.7))
         ELSE
           DF=.1744
         END IF

    ENDIF  

  END SUBROUTINE TDFCND


      SUBROUTINE TMPAVG (TAVG,TUP,TM,TDN,ZSOIL,NSOIL,K)









      IMPLICIT NONE
      INTEGER  K

      INTEGER  NSOIL
      REAL     DZ
      REAL     DZH
      REAL     T0
      REAL     TAVG
      REAL     TDN
      REAL     TM
      REAL     TUP
      REAL     X0
      REAL     XDN
      REAL     XUP

      REAL     ZSOIL (NSOIL)


      PARAMETER (T0 = 2.7315E2)
      IF (K .eq. 1) THEN
         DZ = - ZSOIL (1)
      ELSE
         DZ = ZSOIL (K -1) - ZSOIL (K)
      END IF

      DZH = DZ *0.5
      IF (TUP .lt. T0) THEN
         IF (TM .lt. T0) THEN



            IF (TDN .lt. T0) THEN
               TAVG = (TUP + 2.0* TM + TDN)/ 4.0



            ELSE
               X0 = (T0- TM) * DZH / (TDN - TM)
               TAVG = 0.5 * (TUP * DZH + TM * (DZH + X0) + T0* (        &
     &               2.* DZH - X0)) / DZ
            END IF
         ELSE



            IF (TDN .lt. T0) THEN
               XUP = (T0- TUP) * DZH / (TM - TUP)
               XDN = DZH - (T0- TM) * DZH / (TDN - TM)
               TAVG = 0.5 * (TUP * XUP + T0* (2.* DZ - XUP - XDN)       &
     &                + TDN * XDN) / DZ



            ELSE
               XUP = (T0- TUP) * DZH / (TM - TUP)
               TAVG = 0.5 * (TUP * XUP + T0* (2.* DZ - XUP)) / DZ
            END IF
         END IF
      ELSE
         IF (TM .lt. T0) THEN



            IF (TDN .lt. T0) THEN
               XUP = DZH - (T0- TUP) * DZH / (TM - TUP)
               TAVG = 0.5 * (T0* (DZ - XUP) + TM * (DZH + XUP)          &
     &                + TDN * DZH) / DZ



            ELSE
               XUP = DZH - (T0- TUP) * DZH / (TM - TUP)
               XDN = (T0- TM) * DZH / (TDN - TM)
               TAVG = 0.5 * (T0* (2.* DZ - XUP - XDN) + TM *            &
     & (XUP + XDN)) / DZ
            END IF
         ELSE



            IF (TDN .lt. T0) THEN
               XDN = DZH - (T0- TM) * DZH / (TDN - TM)
               TAVG = (T0* (DZ - XDN) +0.5* (T0+ TDN)* XDN) / DZ



            ELSE
               TAVG = (TUP + 2.0* TM + TDN) / 4.0
            END IF
         END IF
      END IF

  END SUBROUTINE TMPAVG


      SUBROUTINE TRANSP (ET,NSOIL,ETP1,SMC,CMC,ZSOIL,SHDFAC,SMCWLT,     &
     &                      CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,    &
     &                      RTDIS)






      IMPLICIT NONE
      INTEGER  I
      INTEGER  K
      INTEGER  NSOIL

      INTEGER  NROOT
      REAL     CFACTR
      REAL     CMC
      REAL     CMCMAX
      REAL     DENOM
      REAL     ET (NSOIL)
      REAL     ETP1
      REAL     ETP1A

      REAL     GX (NROOT)
      REAL     PC
      REAL     Q2
      REAL     RTDIS (NSOIL)
      REAL     RTX
      REAL     SFCTMP
      REAL     SGX
      REAL     SHDFAC
      REAL     SMC (NSOIL)
      REAL     SMCREF
      REAL     SMCWLT




      REAL     ZSOIL (NSOIL)
      DO K = 1,NSOIL
         ET (K) = 0.







      END DO
      IF (CMC .ne. 0.0) THEN
         ETP1A = SHDFAC * PC * ETP1 * (1.0- (CMC / CMCMAX) ** CFACTR)
      ELSE
         ETP1A = SHDFAC * PC * ETP1
      END IF
      SGX = 0.0
      DO I = 1,NROOT
         GX (I) = ( SMC (I) - SMCWLT ) / ( SMCREF - SMCWLT )
         GX (I) = MAX ( MIN ( GX (I), 1. ), 0. )
         SGX = SGX + GX (I)
      END DO

      SGX = SGX / NROOT
      DENOM = 0.
      DO I = 1,NROOT
         RTX = RTDIS (I) + GX (I) - SGX
         GX (I) = GX (I) * MAX ( RTX, 0. )
         DENOM = DENOM + GX (I)
      END DO

      IF (DENOM .le. 0.0) DENOM = 1.
      DO I = 1,NROOT
         ET (I) = ETP1A * GX (I) / DENOM





























      END DO

  END SUBROUTINE TRANSP


      SUBROUTINE WDFCND (WDF,WCND,SMC,SMCMAX,BEXP,DKSAT,DWSAT,          &
     &                      SICEMAX)






      IMPLICIT NONE
      REAL     BEXP
      REAL     DKSAT
      REAL     DWSAT
      REAL     EXPON
      REAL     FACTR1
      REAL     FACTR2
      REAL     SICEMAX
      REAL     SMC
      REAL     SMCMAX
      REAL     VKwgt
      REAL     WCND




      REAL     WDF
      FACTR1 = 0.05 / SMCMAX




      FACTR2 = SMC / SMCMAX
      FACTR1 = MIN(FACTR1,FACTR2)
      EXPON = BEXP + 2.0













      WDF = DWSAT * FACTR2 ** EXPON
      IF (SICEMAX .gt. 0.0) THEN
         VKWGT = 1./ (1. + (500.* SICEMAX)**3.)
         WDF = VKWGT * WDF + (1. - VKWGT)* DWSAT * FACTR1** EXPON



      END IF
      EXPON = (2.0 * BEXP) + 3.0
      WCND = DKSAT * FACTR2 ** EXPON


  END SUBROUTINE WDFCND


      SUBROUTINE SFCDIF_off (ZLM,Z0,THZ0,THLM,SFCSPD,CZIL,AKMS,AKHS)








      IMPLICIT NONE
      REAL     WWST, WWST2, G, VKRM, EXCM, BETA, BTG, ELFC, WOLD, WNEW
      REAL     PIHF, EPSU2, EPSUST, EPSIT, EPSA, ZTMIN, ZTMAX, HPBL,     &
     & SQVISC
      REAL     RIC, RRIC, FHNEU, RFC, RFAC, ZZ, PSLMU, PSLMS, PSLHU,     &
     & PSLHS
      REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS, ZLM, Z0, THZ0, THLM
      REAL     SFCSPD, CZIL, AKMS, AKHS, ZILFC, ZU, ZT, RDZ, CXCH
      REAL     DTHV, DU2, BTGH, WSTAR2, USTAR, ZSLU, ZSLT, RLOGU, RLOGT
      REAL     RLMO, ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4


      REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
     &         RLMA

      INTEGER  ITRMX, ILECH, ITR
      PARAMETER                                                         &
     &        (WWST = 1.2,WWST2 = WWST * WWST,G = 9.8,VKRM = 0.40,      &
     &         EXCM = 0.001                                             &
     &        ,BETA = 1./270.,BTG = BETA * G,ELFC = VKRM * BTG          &
     &                  ,WOLD =.15,WNEW = 1. - WOLD,ITRMX = 05,         &
     &                   PIHF = 3.14159265/2.)
      PARAMETER                                                         &
     &         (EPSU2 = 1.E-4,EPSUST = 0.07,EPSIT = 1.E-4,EPSA = 1.E-8  &
     &         ,ZTMIN = -5.,ZTMAX = 1.,HPBL = 1000.0                    &
     &          ,SQVISC = 258.2)
      PARAMETER                                                         &
     &       (RIC = 0.183,RRIC = 1.0/ RIC,FHNEU = 0.8,RFC = 0.191       &
     &        ,RFAC = RIC / (FHNEU * RFC * RFC))






      PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
      PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
      PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)




      PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))
      PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &
     &        +2.* ATAN (XX)                                            &
     &- PIHF
      PSPMS (YY)= 5.* YY
      PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)





      PSPHS (YY)= 5.* YY






      ILECH = 0


      ZILFC = - CZIL * VKRM * SQVISC

      ZU = Z0
      RDZ = 1./ ZLM
      CXCH = EXCM * RDZ
      DTHV = THLM - THZ0




      DU2 = MAX (SFCSPD * SFCSPD,EPSU2)

      BTGH = BTG * HPBL
      IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
         WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
      ELSE
         WSTAR2 = 0.0
      END IF




      USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)


      ZT = EXP (ZILFC * SQRT (USTAR * Z0))* Z0
      ZSLU = ZLM + ZU




      ZSLT = ZLM + ZT
      RLOGU = log (ZSLU / ZU)

      RLOGT = log (ZSLT / ZT)






      RLMO = ELFC * AKHS * DTHV / USTAR **3



      DO ITR = 1,ITRMX
         ZETALT = MAX (ZSLT * RLMO,ZTMIN)
         RLMO = ZETALT / ZSLT
         ZETALU = ZSLU * RLMO
         ZETAU = ZU * RLMO

         ZETAT = ZT * RLMO
         IF (ILECH .eq. 0) THEN
            IF (RLMO .lt. 0.)THEN
               XLU4 = 1. -16.* ZETALU
               XLT4 = 1. -16.* ZETALT
               XU4 = 1. -16.* ZETAU

               XT4 = 1. -16.* ZETAT
               XLU = SQRT (SQRT (XLU4))
               XLT = SQRT (SQRT (XLT4))
               XU = SQRT (SQRT (XU4))

               XT = SQRT (SQRT (XT4))





               PSMZ = PSPMU (XU)
               SIMM = PSPMU (XLU) - PSMZ + RLOGU
               PSHZ = PSPHU (XT)
               SIMH = PSPHU (XLT) - PSHZ + RLOGT
            ELSE
               ZETALU = MIN (ZETALU,ZTMAX)
               ZETALT = MIN (ZETALT,ZTMAX)





               PSMZ = PSPMS (ZETAU)
               SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
               PSHZ = PSPHS (ZETAT)
               SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
            END IF



         ELSE
            IF (RLMO .lt. 0.)THEN





               PSMZ = PSLMU (ZETAU)
               SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
               PSHZ = PSLHU (ZETAT)
               SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
            ELSE
               ZETALU = MIN (ZETALU,ZTMAX)

               ZETALT = MIN (ZETALT,ZTMAX)





               PSMZ = PSLMS (ZETAU)
               SIMM = PSLMS (ZETALU) - PSMZ + RLOGU
               PSHZ = PSLHS (ZETAT)
               SIMH = PSLHS (ZETALT) - PSHZ + RLOGT
            END IF



         END IF




         USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

         ZT = EXP (ZILFC * SQRT (USTAR * Z0))* Z0
         ZSLT = ZLM + ZT

         RLOGT = log (ZSLT / ZT)
         USTARK = USTAR * VKRM
         AKMS = MAX (USTARK / SIMM,CXCH)



         AKHS = MAX (USTARK / SIMH,CXCH)
         IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
            WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
         ELSE
            WSTAR2 = 0.0
         END IF

         RLMN = ELFC * AKHS * DTHV / USTAR **3



         RLMA = RLMO * WOLD+ RLMN * WNEW

         RLMO = RLMA













      END DO

  END SUBROUTINE SFCDIF_off


END MODULE module_sf_noahlsm
