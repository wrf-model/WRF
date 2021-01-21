MODULE module_sf_noah_seaice
use module_model_constants, only : CP, R_D, XLF, XLV, RHOWATER, STBOLT
use module_wrf_error
  use module_sf_noahlsm, only : RD, SIGMA, CPH2O, CPICE, LSUBF, EMISSI_S, &
       &                        HSTEP

  PUBLIC  SFLX_SEAICE
  PRIVATE CSNOW
  PRIVATE HRTICE
  PRIVATE PENMAN
  PRIVATE SHFLX
  PRIVATE SNOPAC
  PRIVATE SNOWPACK
  PRIVATE SNOWZ0
  PRIVATE SNOW_NEW

  INTEGER, PRIVATE :: ILOC
  INTEGER, PRIVATE :: JLOC
!$omp threadprivate(iloc, jloc)

  REAL, PARAMETER, PRIVATE :: TFREEZ = 273.15

CONTAINS

  SUBROUTINE SFLX_SEAICE (IILOC, JJLOC, SEAICE_ALBEDO_OPT, SEAICE_ALBEDO_DEFAULT, &    
       &                  SEAICE_SNOWDEPTH_OPT, SEAICE_SNOWDEPTH_MAX,      &    
       &                  SEAICE_SNOWDEPTH_MIN,                            &    
       &                  FFROZP,DT,ZLVL,NSOIL,                            &    
       &                  SITHICK,                                         &
       &                  LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2,               &    
       &                  TH2,Q2SAT,DQSDT2,                                &    
       &                  SNOALB,TBOT, Z0BRD, Z0, EMISSI,                  &    
       &                  T1,STC,SNOWH,SNEQV,ALBEDO, CH,                   &    
       &                  ALBEDOSI, SNOWONSI,                              &
       &                  ETA,SHEAT,ETA_KINEMATIC,FDOWN,                   &    
       &                  ESNOW,DEW,ETP,SSOIL,FLX1,FLX2,FLX3,              &    
       &                  SNOMLT,SNCOVR,                                   &    
       &                  RUNOFF1,Q1,RIBB)








































































































      IMPLICIT NONE

      integer, intent(in) :: iiloc, jjloc
      INTEGER, INTENT(IN) :: SEAICE_ALBEDO_OPT
      REAL,    INTENT(IN) :: SEAICE_ALBEDO_DEFAULT
      INTEGER, INTENT(IN) :: SEAICE_SNOWDEPTH_OPT
      REAL,    INTENT(IN) :: SEAICE_SNOWDEPTH_MAX
      REAL,    INTENT(IN) :: SEAICE_SNOWDEPTH_MIN

      LOGICAL            ::  FRZGRA, SNOWNG

      INTEGER,INTENT(IN) ::  NSOIL

      REAL, INTENT(IN)   :: DT,DQSDT2,LWDN,PRCP,                   &
                            Q2,Q2SAT,SFCPRS,SFCTMP,SNOALB,ALBEDOSI,          &
                            SOLNET,TBOT,TH2,ZLVL,                            &
                            FFROZP
      REAL, INTENT(OUT)  :: ALBEDO
      REAL, INTENT(INOUT):: CH,                         &
                            SNEQV,SNCOVR,SNOWH,T1,Z0BRD,                    &
                            EMISSI
      REAL, INTENT(IN)   :: SNOWONSI
      REAL, INTENT(IN)   :: SITHICK
      REAL, INTENT(INOUT):: RIBB
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) ::  STC
      REAL,DIMENSION(1:NSOIL)::   ZSOIL

      REAL,INTENT(OUT)   :: ETA_KINEMATIC,DEW,ESNOW,ETA,                    &
                            ETP,FLX1,FLX2,FLX3,SHEAT,RUNOFF1,               &
                            SSOIL,                                          &
                            SNOMLT,                                         &
                            FDOWN,Q1,Z0
      REAL :: DF1,DF1A,                                                     &
              DSOIL,DTOT,FRCSNO,FRCSOI,                                     &
              RCH,RR,                                                       &
              SNDENS,SNCOND,SN_NEW,                                         &
              T24,T2V,TH2V,TSNOW

      REAL :: RHO
      INTEGER  :: KZ, K

      REAL :: ALB_SNOW
      REAL :: ALB_ICE
      REAL :: Z0N
      REAL :: SNCOVRR





      REAL, PARAMETER :: LVH2O = 2.501E+6
      REAL, PARAMETER :: LSUBS = 2.83E+6
      REAL, PARAMETER :: R = 287.04

      iloc = iiloc
      jloc = jjloc




      RUNOFF1 = 0.0
      SNOMLT = 0.0





      DO KZ = 1,NSOIL
         ZSOIL (KZ) = -SITHICK * FLOAT (KZ) / FLOAT (NSOIL)
      END DO



      Z0BRD = 0.001 









      SNOWNG = .FALSE.
      FRZGRA = .FALSE.









      SELECT CASE ( SEAICE_ALBEDO_OPT )

      CASE DEFAULT

         IF ( SNEQV < 0.01 ) THEN
            SNEQV = 0.01
            SNOWH = 0.05
         ENDIF

      CASE ( 1 ) 

         IF ( SNEQV < 0.0001 ) THEN
            SNEQV = 0.0001
            SNOWH = 0.0005
         ENDIF

      END SELECT


      IF ( SEAICE_SNOWDEPTH_OPT == 0 ) THEN

          
          
          

          SNDENS = SNEQV / SNOWH
          SNOWH = MAX ( SEAICE_SNOWDEPTH_MIN , MIN ( SNOWH , SEAICE_SNOWDEPTH_MAX ) )
          SNEQV = SNOWH * SNDENS 

      ELSEIF ( SEAICE_SNOWDEPTH_OPT == 1 ) THEN

          
          
          
          

          SNDENS = 0.3
          SNOWH = SNOWONSI
          SNEQV = SNOWH * SNDENS
      ENDIF






      SNDENS = SNEQV / SNOWH
      IF(SNDENS > 1.0) THEN
         call wrf_error_fatal3("<stdin>",285,&
'Physical snow depth is less than snow water equiv.'  )
      ENDIF
      CALL CSNOW (SNCOND,SNDENS)








      IF (PRCP > 0.0) THEN


         IF (FFROZP .GT. 0.5) THEN
            SNOWNG = .TRUE.
         ELSE
            IF (T1 <= TFREEZ) FRZGRA = .TRUE.
         END IF
      END IF







      IF ( SNOWNG .OR. FRZGRA ) THEN
         SN_NEW = PRCP * DT * 0.001
         SNEQV = SNEQV + SN_NEW






         CALL SNOW_NEW ( SFCTMP , SN_NEW , SNOWH , SNDENS )
         
         
         
         
         IF ( SNCOVR .GT. 0.99 ) THEN
            
            
            
            
            IF ( STC(1) .LT. (TFREEZ - 5.) ) SNDENS = 0.2
            IF ( SNOWNG .AND. (T1.LT.273.) .AND. (SFCTMP.LT.273.) ) SNDENS=0.2
         ENDIF

         CALL CSNOW (SNCOND,SNDENS)

      END IF




      

      SELECT CASE ( SEAICE_ALBEDO_OPT )

      CASE DEFAULT

         SNCOVR = 1.0
         EMISSI = 0.98
         ALBEDO = SEAICE_ALBEDO_DEFAULT





      CASE ( 1 ) 

         
         
         
         IF (T1 < 268.15) THEN
            alb_snow = 0.8
         ELSEIF ( ( T1 >= 268.15 ) .AND. ( T1 < 273.15 ) ) then
            alb_snow = 0.65 - ( 0.03 * (T1 - 273.15) )
         ELSE
            alb_snow = 0.65
         ENDIF

         
         
         
         IF ( SFCTMP <= 273.15 ) THEN
            alb_ice = 0.65
         ELSEIF ( ( SFCTMP > 273.15 ) .and. ( SFCTMP < 278.15 ) ) THEN
            alb_ice = 0.65 - ( 0.04 * (SFCTMP - 273.15) )
         ELSE
            alb_ice = 0.45
         ENDIF

         
         
         
         Z0N = 0.10 
         SNCOVRR = SNOWH / ( SNOWH + Z0N )

         
         
         
         
         ALBEDO = (SNCOVRR * alb_snow ) + ( ( 1.0 - SNCOVRR) * alb_ice )

      CASE ( 2 ) 

         SNCOVR = 1.0
         EMISSI = 0.98
         ALBEDO = ALBEDOSI

      END SELECT




      DF1 = 2.2

      DSOIL = - (0.5 * ZSOIL (1))

      DTOT = SNOWH + DSOIL
      FRCSNO = SNOWH / DTOT



      FRCSOI = DSOIL / DTOT






      DF1A = FRCSNO * SNCOND + FRCSOI * DF1






      DF1 = DF1A * SNCOVR + DF1 * ( 1.0 - SNCOVR )
      
      SSOIL = DF1 * ( T1 - STC(1) ) / DTOT






      CALL SNOWZ0 (SNCOVR,Z0,Z0BRD,SNOWH)





      FDOWN =  SOLNET + LWDN




      T2V = SFCTMP * (1.0+ 0.61 * Q2 )
      T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP
      RHO = SFCPRS / ( RD * T2V )
      
      RCH = RHO * 1004.6 * CH  
                               

                               
                               






      CALL PENMAN (SFCTMP,SFCPRS,CH,TH2,PRCP,FDOWN,T24,SSOIL,     &
           Q2,Q2SAT,ETP,RCH,RR,SNOWNG,FRZGRA,                     &
           DQSDT2,FLX2,EMISSI,T1)

      ESNOW = 0.0
      CALL SNOPAC (ETP,ETA,PRCP,SNOWNG,                    &
           NSOIL,DT,DF1,                                   &
           Q2,T1,SFCTMP,T24,TH2,FDOWN,SSOIL,STC,           &
           SFCPRS,RCH,RR,SNCOVR,SNEQV,SNDENS,              &
           SNOWH,ZSOIL,TBOT,                               &
           SNOMLT,DEW,FLX1,FLX2,FLX3,ESNOW,EMISSI,RIBB,    &
           SEAICE_ALBEDO_OPT)

      ETA_KINEMATIC =  ETP

      IF ( SEAICE_SNOWDEPTH_OPT == 0 ) THEN

          
          
          
          SNDENS = SNEQV / SNOWH
          SNOWH = MAX ( SEAICE_SNOWDEPTH_MIN , MIN ( SNOWH , SEAICE_SNOWDEPTH_MAX ) )
          SNEQV = SNOWH * SNDENS 

      ELSEIF ( SEAICE_SNOWDEPTH_OPT == 1 ) THEN

          
          
          
          
          SNDENS = 0.3
          SNOWH = SNOWONSI
          SNEQV = SNOWH * SNDENS
      ENDIF


      Q1=Q2+ETA_KINEMATIC*CP/RCH





      SHEAT = - (CH * CP * SFCPRS)/ (R * T2V) * ( TH2- T1 )





      ESNOW = ESNOW * LSUBS
      ETP = ETP*((1.-SNCOVR)*LVH2O + SNCOVR*LSUBS)
      IF (ETP .GT. 0.) THEN
         ETA = ESNOW
      ELSE
         ETA = ETP
      ENDIF







      SSOIL = -1.0* SSOIL







      RUNOFF1 = SNOMLT/DT


    END SUBROUTINE SFLX_SEAICE


      SUBROUTINE CSNOW (SNCOND,DSNOW)







      IMPLICIT NONE
      REAL, INTENT(IN) :: DSNOW
      REAL, INTENT(OUT):: SNCOND
      REAL             :: C
      REAL, PARAMETER  :: UNIT = 0.11631






      C = 0.328*10** (2.25* DSNOW)
















      SNCOND = 2.0 * UNIT * C


  END SUBROUTINE CSNOW

  SUBROUTINE HRTICE (RHSTS,STC,TBOT,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)









      IMPLICIT NONE


      INTEGER, INTENT(IN)    :: NSOIL
      INTEGER                :: K

      REAL,    INTENT(IN)    :: DF1,YY,ZZ1
      REAL, DIMENSION(1:NSOIL), INTENT(OUT):: AI, BI,CI
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: STC, ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(OUT):: RHSTS
      REAL,                     INTENT(IN) :: TBOT
      REAL                   :: DDZ,DDZ2,DENOM,DTSDZ,DTSDZ2,SSOIL,       &
                                ZBOT
      REAL                   :: HCPCT
      REAL :: DF1K
      REAL :: DF1N
      REAL :: ZMD





      
      HCPCT = 1.72396E+6













      ZBOT = ZSOIL (NSOIL)
      DDZ = 1.0 / ( -0.5 * ZSOIL (2) )
      AI (1) = 0.0
      CI (1) = (DF1 * DDZ) / (ZSOIL (1) * HCPCT)






      BI (1) = - CI (1) + DF1/ (0.5 * ZSOIL (1) * ZSOIL (1) * HCPCT *    &
       ZZ1)
      DTSDZ = ( STC (1) - STC (2) ) / ( -0.5 * ZSOIL (2) )
      SSOIL = DF1 * ( STC (1) - YY ) / ( 0.5 * ZSOIL (1) * ZZ1 )




      RHSTS (1) = ( DF1 * DTSDZ - SSOIL ) / ( ZSOIL (1) * HCPCT )




      DDZ2 = 0.0
      DF1K = DF1
      DF1N = DF1
      DO K = 2,NSOIL




         IF (K /= NSOIL) THEN
            DENOM = 0.5 * ( ZSOIL (K -1) - ZSOIL (K +1) )




            DTSDZ2 = ( STC (K) - STC (K +1) ) / DENOM
            DDZ2 = 2. / (ZSOIL (K -1) - ZSOIL (K +1))
            CI (K) = - DF1N * DDZ2 / ( (ZSOIL (K -1) - ZSOIL (K))*HCPCT)




         ELSE




            DTSDZ2 = (STC (K) - TBOT)/ (.5 * (ZSOIL (K -1) + ZSOIL (K)) &
                     - ZBOT)
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


  SUBROUTINE PENMAN (SFCTMP,SFCPRS,CH,TH2,PRCP,FDOWN,T24,SSOIL, &
       &             Q2,Q2SAT,ETP,RCH,RR,SNOWNG,FRZGRA,       &
       &             DQSDT2,FLX2,EMISSI,T1)







    IMPLICIT NONE
    LOGICAL, INTENT(IN)     :: SNOWNG, FRZGRA
    REAL, INTENT(IN)        :: CH, DQSDT2, FDOWN, PRCP,           &
         &                     Q2, Q2SAT, SSOIL, SFCPRS, SFCTMP,  &
         &                     TH2,EMISSI
    REAL, INTENT(IN)        :: T1, T24, RCH
    REAL, INTENT(OUT)       :: ETP,FLX2,RR
    REAL                    :: ELCP1, LVS, EPSCA, A, DELTA, FNET, RAD

    REAL, PARAMETER      :: ELCP = 2.4888E+3, LSUBC = 2.501000E+6,CP = 1004.6
    REAL, PARAMETER      :: LSUBS = 2.83E+6





    IF ( T1 > 273.15 ) THEN
       ELCP1=ELCP
       LVS=LSUBC
    ELSE
       ELCP1  = ELCP*LSUBS/LSUBC
       LVS    = LSUBS
    ENDIF

    FLX2 = 0.0
    DELTA = ELCP1 * DQSDT2
    RR = EMISSI * T24 * 6.48E-8 / (SFCPRS * CH) + 1.0






    IF ( PRCP > 0.0 ) THEN
       IF (.NOT. SNOWNG) THEN
          RR = RR + CPH2O * PRCP / RCH
       ELSE
          RR = RR + CPICE * PRCP / RCH
       ENDIF
    ENDIF






    FNET = FDOWN - EMISSI * SIGMA * T24 - SSOIL
    IF (FRZGRA) THEN
       FLX2 = - LSUBF * PRCP
       FNET = FNET - FLX2
    END IF





    RAD = FNET / RCH + TH2 - SFCTMP
    A = ELCP1 * (Q2SAT - Q2)
    EPSCA = (A * RR + RAD * DELTA) / (DELTA + RR)
    ETP = EPSCA * RCH / LVS


  END SUBROUTINE PENMAN


  SUBROUTINE SHFLX (STC,NSOIL,DT,YY,ZZ1,ZSOIL,TBOT,DF1)




      IMPLICIT NONE

      INTEGER,                  INTENT(IN)    :: NSOIL
      REAL,                     INTENT(IN)    :: DF1,DT,TBOT,YY, ZZ1
      REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: STC
      REAL, DIMENSION(1:NSOIL)                :: AI, BI, CI, STCF,RHSTS
      INTEGER                                 :: I
      REAL, PARAMETER                         :: T0 = 273.15





      CALL HRTICE (RHSTS,STC,TBOT,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)
      CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)

      DO I = 1,NSOIL
         STC (I) = STCF (I)
      END DO


  END SUBROUTINE SHFLX


  SUBROUTINE SNOPAC (ETP,ETA,PRCP,SNOWNG,            &
       NSOIL,DT,DF1,                                 &
       Q2,T1,SFCTMP,T24,TH2,FDOWN,SSOIL,STC,         &
       SFCPRS,RCH,RR,SNCOVR,ESD,SNDENS,              &
       SNOWH,ZSOIL,TBOT,                             &
       SNOMLT,DEW,FLX1,FLX2,FLX3,ESNOW,EMISSI,       &
       RIBB, SEAICE_ALBEDO_OPT)








    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NSOIL
    INTEGER               :: K
    LOGICAL, INTENT(IN)   :: SNOWNG
    REAL, INTENT(IN)      :: DF1,                                     &
         &                   DT,FDOWN,                                &
         &                   PRCP,Q2,                                 &
         &                   RCH,RR,SFCPRS, SFCTMP,                   &
         &                   T24,                                     &
         &                   TBOT,TH2,EMISSI
    REAL, INTENT(INOUT)   :: ESD,FLX2,SNOWH,SNCOVR,                   &
         &                   SNDENS, T1, RIBB, ETP
    REAL, INTENT(OUT)     :: DEW,ESNOW,                               &
         &                   FLX1,FLX3, SSOIL,SNOMLT
    REAL, DIMENSION(1:NSOIL),INTENT(IN)     :: ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: STC
    REAL                  :: DENOM,DSOIL,DTOT,ETA,                    &
         &                   ESNOW1, ESNOW2, ETA1,ETP1,ETP2,          &
         &                   ETANRG, EX, SEH,                         &
         &                   SNCOND,T12, T12A,                        &
         &                   T12B, T14, YY, ZZ1
    INTEGER, INTENT(IN)   :: SEAICE_ALBEDO_OPT
    REAL, PARAMETER       :: ESDMIN = 1.E-6, LSUBC = 2.501000E+6,     &
         LSUBS = 2.83E+6, SNOEXP = 2.0














    DEW = 0.
    ESNOW = 0.
    ESNOW1 = 0.
    ESNOW2 = 0.







    IF (ETP <= 0.0) THEN
       IF ( ( RIBB >= 0.1 ) .AND. ( FDOWN > 150.0 ) ) THEN
          ETP=(MIN(ETP*(1.0-RIBB),0.)*SNCOVR/0.980 + ETP*(0.980-SNCOVR))/0.980
       ENDIF
       ETP1 = ETP * 0.001
       DEW = -ETP1
       ESNOW2 = ETP1*DT
       ETANRG = ETP*((1.-SNCOVR)*LSUBC + SNCOVR*LSUBS)
    ELSE
       ETP1 = ETP * 0.001
       ESNOW  = ETP
       ESNOW1 = ESNOW*0.001
       ESNOW2 = ESNOW1*DT
       ETANRG = ESNOW*LSUBS
       ESNOW  = ETP*SNCOVR
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




    T12A = ( (FDOWN - FLX1 - FLX2 - EMISSI * SIGMA * T24)/ RCH                    &
         + TH2 - SFCTMP - ETANRG / RCH ) / RR

    T12B = DF1 * STC (1) / (DTOT * RR * RCH)













    T12 = (SFCTMP + T12A + T12B) / DENOM
    IF (T12 <=  TFREEZ) THEN
       T1 = T12
       SSOIL = DF1 * (T1- STC (1)) / DTOT

       ESD = MAX(0.0, ESD-ESNOW2)
       FLX3 = 0.0
       EX = 0.0

       SNOMLT = 0.0














    ELSE
       T1 = TFREEZ 
       SSOIL = DF1 * (T1- STC (1)) / DTOT






       IF (ESD-ESNOW2 <= ESDMIN) THEN
          ESD = 0.0
          EX = 0.0
          SNOMLT = 0.0
          FLX3 = 0.0




       ELSE
          ESD = ESD-ESNOW2
          SEH = RCH * (T1- TH2)
          T14 = ( T1 * T1 ) * ( T1 * T1 )
          FLX3 = FDOWN - FLX1- FLX2- EMISSI*SIGMA * T14- SSOIL - SEH - ETANRG
          IF (FLX3 <= 0.0) FLX3 = 0.0



          EX = FLX3*0.001/ LSUBF





          SNOMLT = EX * DT
          IF (ESD- SNOMLT >=  ESDMIN) THEN
             ESD = ESD- SNOMLT
          ELSE
             
             
             
             EX = ESD / DT
             FLX3 = EX *1000.0* LSUBF
             SNOMLT = ESD

             ESD = 0.0
          ENDIF
       ENDIF





    ENDIF



















    ZZ1 = 1.0
    YY = STC (1) -0.5* SSOIL * ZSOIL (1)* ZZ1/ DF1





    CALL SHFLX (STC,NSOIL,DT,YY,ZZ1,ZSOIL,TBOT,DF1)





    SELECT CASE ( SEAICE_ALBEDO_OPT )

    CASE DEFAULT

       IF (ESD .GE. 0.01) THEN
          CALL SNOWPACK (ESD,DT,SNOWH,SNDENS,T1,YY)
       ELSE
          ESD = 0.01
          SNOWH = 0.05


          SNCOVR = 1.0
       ENDIF

    CASE ( 1 ) 

       IF ( ESD >= 0.0001 ) THEN
          CALL SNOWPACK (ESD,DT,SNOWH,SNDENS,T1,YY)
       ELSE
          ESD    = 0.0001
          SNOWH  = 0.0005
          SNCOVR = 0.005
       ENDIF

    END SELECT

  END SUBROUTINE SNOPAC


      SUBROUTINE SNOWPACK (ESD,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL)


















      IMPLICIT NONE

      INTEGER                :: IPOL, J
      REAL, INTENT(IN)       :: ESD, DTSEC,TSNOW,TSOIL
      REAL, INTENT(INOUT)    :: SNOWH, SNDENS
      REAL                   :: BFAC,DSX,DTHR,DW,SNOWHC,PEXP,           &
                                TAVGC,TSNOWC,TSOILC,ESDC,ESDCX
      REAL, PARAMETER        :: C1 = 0.01, C2 = 21.0, G = 9.81,         &
                                KN = 4000.0



      SNOWHC = SNOWH *100.
      ESDC = ESD *100.
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
         SNDENS = SNDENS * (1. - DW) + DW
         IF (SNDENS >=  0.40) SNDENS = 0.40




      END IF
      SNOWHC = ESDC / SNDENS
      SNOWH = SNOWHC *0.01


  END SUBROUTINE SNOWPACK


      SUBROUTINE SNOWZ0 (SNCOVR,Z0, Z0BRD, SNOWH)









      IMPLICIT NONE
      REAL, INTENT(IN)        :: SNCOVR, Z0BRD
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
      
      Z0 = (1.- SNCOVR)* Z0BRD + SNCOVR * Z0EFF


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


END MODULE module_sf_noah_seaice
