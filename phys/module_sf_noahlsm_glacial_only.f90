MODULE module_sf_noahlsm_glacial_only
use module_model_constants
use module_wrf_error

  USE module_sf_noahlsm, ONLY : RD, SIGMA, CPH2O, CPICE, LSUBF, EMISSI_S, ROSR12
  USE module_sf_noahlsm, ONLY : LVCOEF_DATA

  PRIVATE :: ALCALC
  PRIVATE :: CSNOW
  PRIVATE :: HRTICE
  PRIVATE :: HSTEP
  PRIVATE :: PENMAN
  PRIVATE :: SHFLX
  PRIVATE :: SNOPAC
  PRIVATE :: SNOWPACK
  PRIVATE :: SNOWZ0
  PRIVATE :: SNOW_NEW

  integer, private :: iloc, jloc
!$omp threadprivate(iloc, jloc)

CONTAINS

  SUBROUTINE SFLX_GLACIAL (IILOC,JJLOC,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,     &    
       &                   LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2,           &    
       &                   TH2,Q2SAT,DQSDT2,                            &    
       &                   ALB, SNOALB,TBOT, Z0BRD, Z0, EMISSI, EMBRD,  &    
       &                   T1,STC,SNOWH,SNEQV,ALBEDO,CH,                &    





       &                   ETA,SHEAT, ETA_KINEMATIC,FDOWN,              &    
       &                   ESNOW,DEW,                                   &    
       &                   ETP,SSOIL,                                   &    
       &                   FLX1,FLX2,FLX3,                              &    
       &                   SNOMLT,SNCOVR,                               &    
       &                   RUNOFF1,                                     &    
       &                   Q1,                                          &    
       &                   SNOTIME1,                                    &
       &                   RIBB)















































































































    IMPLICIT NONE

    integer, intent(in) ::  iiloc, jjloc
    INTEGER, INTENT(IN) ::  ISICE

    LOGICAL             ::  FRZGRA, SNOWNG




    INTEGER, INTENT(IN) ::  NSOIL
    INTEGER             ::  KZ





    REAL, INTENT(IN)   :: DT,DQSDT2,LWDN,PRCP,     &
         &                Q2,Q2SAT,SFCPRS,SFCTMP, SNOALB,          &
         &                SOLNET,TBOT,TH2,ZLVL,FFROZP
    REAL, INTENT(OUT)  :: EMBRD, ALBEDO
    REAL, INTENT(INOUT):: CH,SNEQV,SNCOVR,SNOWH,T1,Z0BRD,EMISSI,ALB
    REAL, INTENT(INOUT):: SNOTIME1
    REAL, INTENT(INOUT):: RIBB
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SLDPTH
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) ::  STC
    REAL, DIMENSION(1:NSOIL) ::   ZSOIL

    REAL,INTENT(OUT)   :: ETA_KINEMATIC,DEW,ESNOW,ETA,  &
         &                ETP,FLX1,FLX2,FLX3,SHEAT,RUNOFF1,    &
         &                SSOIL,SNOMLT,FDOWN,Q1
    REAL               :: DF1,DSOIL,DTOT,FRCSNO,FRCSOI,          &
         &                PRCP1,RCH,RR,RSNOW,SNDENS,SNCOND,SN_NEW,     &
         &                T1V,T24,T2V,TH2V,TSNOW,Z0,PRCPF,RHO




    REAL, PARAMETER :: TFREEZ = 273.15
    REAL, PARAMETER :: LVH2O = 2.501E+6
    REAL, PARAMETER :: LSUBS = 2.83E+6
    REAL, PARAMETER :: R = 287.04


    iloc = iiloc
    jloc = jjloc

    ZSOIL (1) = - SLDPTH (1)
    DO KZ = 2,NSOIL
       ZSOIL (KZ) = - SLDPTH (KZ) + ZSOIL (KZ -1)
    END DO





    IF ( SNEQV < 0.10 ) THEN
       SNEQV = 0.10
       SNOWH = 0.50
    ENDIF




    SNDENS = SNEQV / SNOWH
    IF(SNDENS > 1.0) THEN
       call wrf_error_fatal3("<stdin>",220,&
'Physical snow depth is less than snow water equiv.'  )
    ENDIF

    CALL CSNOW (SNCOND,SNDENS)







    SNOWNG = .FALSE.
    FRZGRA = .FALSE.
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







       IF ( SNCOVR .GT. 0.99 ) THEN
          IF ( STC(1) .LT. (TFREEZ - 5.) ) SNDENS = 0.2
          IF ( SNOWNG .AND. (T1.LT.273.) .AND. (SFCTMP.LT.273.) ) SNDENS=0.2
       ENDIF

       CALL CSNOW (SNCOND,SNDENS)





    ELSE
       PRCPF = PRCP
    ENDIF






    SNCOVR = 1.0 





    CALL ALCALC (ALB,SNOALB,EMBRD,T1,ALBEDO,EMISSI,   &
         &       DT,SNOWNG,SNOTIME1) 




    DF1 = SNCOND

    DSOIL = - (0.5 * ZSOIL (1))
    DTOT = SNOWH + DSOIL
    FRCSNO = SNOWH / DTOT



    FRCSOI = DSOIL / DTOT



    DF1 = FRCSNO * SNCOND + FRCSOI * DF1






    IF ( DTOT .GT. 2.*DSOIL ) then
       DTOT = 2.*DSOIL
    ENDIF
    SSOIL = DF1 * ( T1 - STC(1) ) / DTOT






    CALL SNOWZ0 (Z0,Z0BRD,SNOWH)






    FDOWN = SOLNET + LWDN






    T2V = SFCTMP * (1.0+ 0.61 * Q2 )
    RHO = SFCPRS / (RD * T2V)
    RCH = RHO * 1004.6 * CH
    T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP







    
    CALL PENMAN (SFCTMP,SFCPRS,CH,TH2,PRCP,FDOWN,T24,SSOIL,     &
         &       Q2,Q2SAT,ETP,RCH,RR,SNOWNG,FRZGRA,             &
         &       DQSDT2,FLX2,EMISSI,T1)

    CALL SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,NSOIL,DT,DF1,        &
         &       Q2,T1,SFCTMP,T24,TH2,FDOWN,SSOIL,STC,          &
         &       SFCPRS,RCH,RR,SNEQV,SNDENS,SNOWH,ZSOIL,TBOT,   &
         &       SNOMLT,DEW,FLX1,FLX2,FLX3,ESNOW,EMISSI,RIBB)


    ETA_KINEMATIC =  ETP




    Q1=Q2+ETA_KINEMATIC*CP/RCH




    SHEAT = - (CH * CP * SFCPRS)/ (R * T2V) * ( TH2- T1 )




    ESNOW = ESNOW * LSUBS
    ETP   = ETP   * LSUBS
    IF (ETP .GT. 0.) THEN
       ETA = ESNOW
    ELSE
       ETA = ETP
    ENDIF






    SSOIL = -1.0* SSOIL





    RUNOFF1 = SNOMLT / DT


  END SUBROUTINE SFLX_GLACIAL


  SUBROUTINE ALCALC (ALB,SNOALB,EMBRD,TSNOW,ALBEDO,EMISSI,   &
       &             DT,SNOWNG,SNOTIME1)








    IMPLICIT NONE







    REAL,    INTENT(IN)    :: ALB, SNOALB, EMBRD, TSNOW
    REAL,    INTENT(IN)    :: DT
    LOGICAL, INTENT(IN)    :: SNOWNG
    REAL,    INTENT(INOUT) :: SNOTIME1
    REAL,    INTENT(OUT)   :: ALBEDO, EMISSI
    REAL                   :: SNOALB2
    REAL                   :: TM,SNOALB1
    REAL,    PARAMETER     :: SNACCA=0.94,SNACCB=0.58,SNTHWA=0.82,SNTHWB=0.46



    ALBEDO = ALB + (SNOALB-ALB)
    EMISSI = EMBRD + (EMISSI_S - EMBRD)





















































    SNOALB1 = SNOALB+LVCOEF_DATA*(0.85-SNOALB)
    SNOALB2=SNOALB1

    IF (SNOWNG) THEN
       SNOTIME1 = 0.
    ELSE
       SNOTIME1=SNOTIME1+DT

       SNOALB2=SNOALB1*(SNACCA**((SNOTIME1/86400.0)**SNACCB))



    ENDIF

    SNOALB2 = MAX ( SNOALB2, ALB )
    ALBEDO = ALB + (SNOALB2-ALB)
    IF (ALBEDO .GT. SNOALB2) ALBEDO=SNOALB2










  END SUBROUTINE ALCALC


  SUBROUTINE CSNOW (SNCOND,DSNOW)




    IMPLICIT NONE
    REAL, INTENT(IN)  :: DSNOW
    REAL, INTENT(OUT) :: SNCOND
    REAL              :: C
    REAL, PARAMETER   :: UNIT = 0.11631






    C = 0.328*10** (2.25* DSNOW)
















    SNCOND = 2.0 * UNIT * C


  END SUBROUTINE CSNOW


  SUBROUTINE HRTICE (RHSTS,STC,TBOT,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)










    IMPLICIT NONE


    INTEGER,                  INTENT(IN)  :: NSOIL
    REAL,                     INTENT(IN)  :: DF1,YY,ZZ1
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: AI, BI,CI
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: STC, ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: RHSTS
    REAL,                     INTENT(IN)  :: TBOT
    INTEGER                :: K
    REAL                   :: DDZ,DDZ2,DENOM,DTSDZ,DTSDZ2,SSOIL,HCPCT
    REAL                   :: DF1K,DF1N
    REAL                   :: ZMD
    REAL, PARAMETER        :: ZBOT = -25.0





    
    
    
    
    
    HCPCT = 1.E6 * (0.8194 - 0.1309*0.5*ZSOIL(1))
    DF1K = DF1













    DDZ = 1.0 / ( -0.5 * ZSOIL (2) )
    AI (1) = 0.0
    CI (1) = (DF1 * DDZ) / (ZSOIL (1) * HCPCT)






    BI (1) = - CI (1) + DF1/ (0.5 * ZSOIL (1) * ZSOIL (1) * HCPCT *    &
         &   ZZ1)
    DTSDZ = ( STC (1) - STC (2) ) / ( -0.5 * ZSOIL (2) )
    SSOIL = DF1 * ( STC (1) - YY ) / ( 0.5 * ZSOIL (1) * ZZ1 )




    RHSTS (1) = ( DF1 * DTSDZ - SSOIL ) / ( ZSOIL (1) * HCPCT )




    DDZ2 = 0.0
    DF1K = DF1
    DF1N = DF1
    DO K = 2,NSOIL

       ZMD = 0.5 * (ZSOIL(K)+ZSOIL(K-1))
       





       
       
       
       HCPCT = 1.E6 * ( 0.8194 - 0.1309*ZMD )





       
       
       
       DF1N = 0.32333 - ( 0.10073 * ZMD )



       IF (K /= NSOIL) THEN
          DENOM = 0.5 * ( ZSOIL (K -1) - ZSOIL (K +1) )




          DTSDZ2 = ( STC (K) - STC (K +1) ) / DENOM
          DDZ2 = 2. / (ZSOIL (K -1) - ZSOIL (K +1))
          CI (K) = - DF1N * DDZ2 / ( (ZSOIL (K -1) - ZSOIL (K))*HCPCT)




       ELSE




          DTSDZ2 = (STC (K) - TBOT)/ (.5 * (ZSOIL (K -1) + ZSOIL (K)) &
               &   - ZBOT)
          CI (K) = 0.



       END IF
       DENOM = ( ZSOIL (K) - ZSOIL (K -1) ) * HCPCT




       RHSTS (K) = ( DF1N * DTSDZ2- DF1K * DTSDZ ) / DENOM
       AI (K) = - DF1K * DDZ / ( (ZSOIL (K -1) - ZSOIL (K)) * HCPCT)




       BI (K) = - (AI (K) + CI (K))
       DF1K = DF1N
       DTSDZ = DTSDZ2
       DDZ = DDZ2
    END DO

  END SUBROUTINE HRTICE


  SUBROUTINE HSTEP (STCOUT,STCIN,RHSTS,DT,NSOIL,AI,BI,CI)




    IMPLICIT NONE
    INTEGER,                  INTENT(IN)    :: NSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: STCIN
    REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: STCOUT
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: RHSTS
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: AI,BI,CI
    REAL, DIMENSION(1:NSOIL) :: RHSTSin
    REAL, DIMENSION(1:NSOIL) :: CIin
    REAL                     :: DT
    INTEGER                  :: K




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


  SUBROUTINE PENMAN (SFCTMP,SFCPRS,CH,TH2,PRCP,FDOWN,T24,SSOIL, &
       &             Q2,Q2SAT,ETP,RCH,RR,SNOWNG,FRZGRA,       &
       &             DQSDT2,FLX2,EMISSI,T1)






    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: SNOWNG, FRZGRA
    REAL,    INTENT(IN)  :: CH, DQSDT2,FDOWN,PRCP,Q2,Q2SAT,SSOIL,SFCPRS, &
         &                  SFCTMP,TH2,EMISSI,T1,RCH,T24
    REAL,    INTENT(OUT) :: ETP,FLX2,RR

    REAL                 :: A, DELTA, FNET,RAD,ELCP1,LVS,EPSCA

    REAL, PARAMETER      :: ELCP = 2.4888E+3, LSUBC = 2.501000E+6
    REAL, PARAMETER      :: LSUBS = 2.83E+6




    IF ( T1 > 273.15 ) THEN
       ELCP1 = ELCP
       LVS   = LSUBC
    ELSE
       ELCP1 = ELCP*LSUBS/LSUBC
       LVS   = LSUBS
    ENDIF
    DELTA = ELCP1 * DQSDT2
    A = ELCP1 * (Q2SAT - Q2)
    RR = EMISSI*T24 * 6.48E-8 / (SFCPRS * CH) + 1.0





    IF (.NOT. SNOWNG) THEN
       IF (PRCP >  0.0) RR = RR + CPH2O * PRCP / RCH
    ELSE
       RR = RR + CPICE * PRCP / RCH
    END IF





    IF (FRZGRA) THEN
       FLX2 = - LSUBF * PRCP
    ELSE
       FLX2 = 0.0
    ENDIF
    FNET = FDOWN -  ( EMISSI * SIGMA * T24 ) - SSOIL - FLX2




    RAD = FNET / RCH + TH2 - SFCTMP
    EPSCA = (A * RR + RAD * DELTA) / (DELTA + RR)
    ETP = EPSCA * RCH / LVS


  END SUBROUTINE PENMAN


  SUBROUTINE SHFLX (STC,NSOIL,DT,YY,ZZ1,ZSOIL,TBOT,DF1)





    IMPLICIT NONE

    INTEGER,                  INTENT(IN)    :: NSOIL
    REAL,                     INTENT(IN)    :: DF1,DT,TBOT,YY, ZZ1
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: STC

    REAL, DIMENSION(1:NSOIL) :: AI, BI, CI, STCF,RHSTS
    INTEGER                  :: I
    REAL, PARAMETER          :: T0 = 273.15





    CALL HRTICE (RHSTS,STC,TBOT, NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

    CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)

    DO I = 1,NSOIL
       STC (I) = STCF (I)
    END DO

  END SUBROUTINE SHFLX


  SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,NSOIL,DT,DF1,           &
       &             Q2,T1,SFCTMP,T24,TH2,FDOWN,SSOIL,STC,             &
       &             SFCPRS,RCH,RR,SNEQV,SNDENS,SNOWH,ZSOIL,TBOT,      &
       &             SNOMLT,DEW,FLX1,FLX2,FLX3,ESNOW,EMISSI,RIBB)






    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NSOIL
    LOGICAL, INTENT(IN)   :: SNOWNG
    REAL,    INTENT(IN)   :: DF1,DT,FDOWN,PRCP,Q2,RCH,RR,SFCPRS,SFCTMP, &
         &                   T24,TBOT,TH2,EMISSI
    REAL, INTENT(INOUT)   :: SNEQV,FLX2,PRCPF,SNOWH,SNDENS,T1,RIBB,ETP
    REAL, INTENT(OUT)     :: DEW,ESNOW,FLX1,FLX3,SSOIL,SNOMLT
    REAL, DIMENSION(1:NSOIL),INTENT(IN)     :: ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: STC
    REAL, DIMENSION(1:NSOIL) :: ET1
    INTEGER               :: K
    REAL                  :: DENOM,DSOIL,DTOT,ESDFLX,ETA,        &
         &                   ESNOW1,ESNOW2,ETA1,ETP1,ETP2,       &
         &                   ETP3,ETANRG,EX,                     &
         &                   FRCSNO,FRCSOI,PRCP1,QSAT,RSNOW,SEH, &
         &                   SNCOND,T12,T12A,T12B,T14,YY,ZZ1

    REAL, PARAMETER       :: ESDMIN = 1.E-6, LSUBC = 2.501000E+6,     &
         &                   LSUBS = 2.83E+6, TFREEZ = 273.15,        &
         &                   SNOEXP = 2.0
















    SNOMLT = 0.0
    DEW = 0.
    ESNOW = 0.
    ESNOW1 = 0.
    ESNOW2 = 0.




    PRCP1 = PRCPF *0.001



    IF (ETP <= 0.0) THEN
       IF ( ( RIBB >= 0.1 ) .AND. ( FDOWN > 150.0 ) ) THEN
          ETP=(MIN(ETP*(1.0-RIBB),0.)/0.980 + ETP*(0.980-1.0))/0.980
       ENDIF
       ETP1 = ETP * 0.001
       DEW = -ETP1
       ESNOW2 = ETP1*DT
       ETANRG = ETP*LSUBS
    ELSE
       ETP1 = ETP * 0.001
       ESNOW = ETP
       ESNOW1 = ESNOW*0.001
       ESNOW2 = ESNOW1*DT
       ETANRG = ESNOW*LSUBS
    END IF







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
       SNEQV = MAX(0.0, SNEQV-ESNOW2)
       FLX3 = 0.0
       EX = 0.0
       SNOMLT = 0.0
    ELSE















       T1 = TFREEZ
       IF ( DTOT .GT. 2.0*DSOIL ) THEN
          DTOT = 2.0*DSOIL
       ENDIF
       SSOIL = DF1 * (T1- STC (1)) / DTOT
       IF (SNEQV-ESNOW2 <= ESDMIN) THEN
          SNEQV = 0.0
          EX = 0.0
          SNOMLT = 0.0
          FLX3 = 0.0




       ELSE
          SNEQV = SNEQV-ESNOW2
          ETP3 = ETP * LSUBC
          SEH = RCH * (T1- TH2)
          T14 = ( T1 * T1 ) * ( T1 * T1 )
          FLX3 = FDOWN - FLX1- FLX2- EMISSI*SIGMA * T14- SSOIL - SEH - ETANRG
          IF (FLX3 <= 0.0) FLX3 = 0.0
          EX = FLX3*0.001/ LSUBF
          SNOMLT = EX * DT




          IF (SNEQV- SNOMLT >=  ESDMIN) THEN
             SNEQV = SNEQV- SNOMLT
          ELSE



             EX = SNEQV / DT
             FLX3 = EX *1000.0* LSUBF
             SNOMLT = SNEQV

             SNEQV = 0.0
          ENDIF
       ENDIF







    ENDIF








    ZZ1 = 1.0
    YY = STC (1) -0.5* SSOIL * ZSOIL (1)* ZZ1/ DF1




    CALL SHFLX (STC,NSOIL,DT,YY,ZZ1,ZSOIL,TBOT,DF1)





    IF (SNEQV .GE. 0.10) THEN
       CALL SNOWPACK (SNEQV,DT,SNOWH,SNDENS,T1,YY)
    ELSE
       SNEQV = 0.10
       SNOWH = 0.50


    ENDIF

  END SUBROUTINE SNOPAC


  SUBROUTINE SNOWPACK (SNEQV,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL)
















      IMPLICIT NONE

      INTEGER                :: IPOL, J
      REAL, INTENT(IN)       :: SNEQV, DTSEC,TSNOW,TSOIL
      REAL, INTENT(INOUT)    :: SNOWH, SNDENS
      REAL                   :: BFAC,DSX,DTHR,DW,SNOWHC,PEXP,           &
                                TAVGC,TSNOWC,TSOILC,ESDC,ESDCX
      REAL, PARAMETER        :: C1 = 0.01, C2 = 21.0, G = 9.81,         &
                                KN = 4000.0



      SNOWHC = SNOWH *100.
      ESDC   = SNEQV *100.
      DTHR   = DTSEC /3600.
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
         SNDENS = SNDENS * (1. - DW) + DW
         IF (SNDENS >=  0.40) SNDENS = 0.40




      END IF
      SNOWHC = ESDC / SNDENS
      SNOWH = SNOWHC * 0.01


  END SUBROUTINE SNOWPACK


  SUBROUTINE SNOWZ0 (Z0, Z0BRD, SNOWH)





    IMPLICIT NONE
    REAL, INTENT(IN)        :: Z0BRD
    REAL, INTENT(OUT)       :: Z0
    REAL, PARAMETER         :: Z0S=0.001
    REAL, INTENT(IN)        :: SNOWH
    REAL                    :: BURIAL
    REAL                    :: Z0EFF

    BURIAL = 7.0*Z0BRD - SNOWH
    IF(BURIAL.LE.0.0007) THEN
       Z0EFF = Z0S
    ELSE      
       Z0EFF = BURIAL/7.0
    ENDIF

    Z0 = Z0EFF


  END SUBROUTINE SNOWZ0


  SUBROUTINE SNOW_NEW (TEMP,NEWSN,SNOWH,SNDENS)










    IMPLICIT NONE
    REAL, INTENT(IN)        :: NEWSN, TEMP
    REAL, INTENT(INOUT)     :: SNDENS, SNOWH
    REAL                    :: DSNEW, HNEWC, SNOWHC,NEWSNC,TEMPC







    TEMPC = TEMP - 273.15
    IF ( TEMPC <=  -15. ) THEN
       DSNEW = 0.05
    ELSE
       DSNEW = 0.05 + 0.0017 * ( TEMPC + 15. ) ** 1.5
    ENDIF




    SNOWHC = SNOWH * 100.
    NEWSNC = NEWSN * 100.




    HNEWC = NEWSNC / DSNEW
    IF ( SNOWHC + HNEWC < 1.0E-3 ) THEN
       SNDENS = MAX ( DSNEW , SNDENS )
    ELSE
       SNDENS = ( SNOWHC * SNDENS + HNEWC * DSNEW ) / ( SNOWHC + HNEWC )
    ENDIF
    SNOWHC = SNOWHC + HNEWC
    SNOWH = SNOWHC * 0.01


  END SUBROUTINE SNOW_NEW


END MODULE module_sf_noahlsm_glacial_only
