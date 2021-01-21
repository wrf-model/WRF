MODULE NOAHMP_GLACIER_GLOBALS

  implicit none






  REAL, PARAMETER :: GRAV   = 9.80616   
  REAL, PARAMETER :: SB     = 5.67E-08  
  REAL, PARAMETER :: VKC    = 0.40      
  REAL, PARAMETER :: TFRZ   = 273.16    
  REAL, PARAMETER :: HSUB   = 2.8440E06 
  REAL, PARAMETER :: HVAP   = 2.5104E06 
  REAL, PARAMETER :: HFUS   = 0.3336E06 
  REAL, PARAMETER :: CWAT   = 4.188E06  
  REAL, PARAMETER :: CICE   = 2.094E06  
  REAL, PARAMETER :: CPAIR  = 1004.64   
  REAL, PARAMETER :: TKWAT  = 0.6       
  REAL, PARAMETER :: TKICE  = 2.2       
  REAL, PARAMETER :: TKAIR  = 0.023     
  REAL, PARAMETER :: RAIR   = 287.04    
  REAL, PARAMETER :: RW     = 461.269   
  REAL, PARAMETER :: DENH2O = 1000.     
  REAL, PARAMETER :: DENICE = 917.      






  INTEGER :: OPT_ALB 




  INTEGER :: OPT_SNF 





  INTEGER :: OPT_TBOT 




  INTEGER :: OPT_STC 




  INTEGER :: OPT_GLA 



  REAL, PARAMETER :: Z0SNO  = 0.002  
  REAL, PARAMETER :: SSI    = 0.03   
  REAL, PARAMETER :: SWEMX  = 1.00   
                                     


END MODULE NOAHMP_GLACIER_GLOBALS


MODULE NOAHMP_GLACIER_ROUTINES
  USE NOAHMP_GLACIER_GLOBALS
  IMPLICIT NONE

  public  :: NOAHMP_OPTIONS_GLACIER
  public  :: NOAHMP_GLACIER

  private :: ATM_GLACIER
  private :: ENERGY_GLACIER
  private ::       THERMOPROP_GLACIER
  private ::               CSNOW_GLACIER
  private ::       RADIATION_GLACIER
  private ::               SNOW_AGE_GLACIER
  private ::               SNOWALB_BATS_GLACIER  
  private ::               SNOWALB_CLASS_GLACIER
  private ::       GLACIER_FLUX
  private ::               SFCDIF1_GLACIER                  
  private ::       TSNOSOI_GLACIER
  private ::               HRT_GLACIER
  private ::               HSTEP_GLACIER   
  private ::                         ROSR12_GLACIER
  private ::       PHASECHANGE_GLACIER

  private :: WATER_GLACIER
  private ::       SNOWWATER_GLACIER
  private ::               SNOWFALL_GLACIER
  private ::               COMBINE_GLACIER
  private ::               DIVIDE_GLACIER
  private ::                         COMBO_GLACIER
  private ::               COMPACT_GLACIER
  private ::               SNOWH2O_GLACIER

  private :: ERROR_GLACIER

contains



  SUBROUTINE NOAHMP_GLACIER (&
                   ILOC    ,JLOC    ,COSZ    ,NSNOW   ,NSOIL   ,DT      , & 
                   SFCTMP  ,SFCPRS  ,UU      ,VV      ,Q2      ,SOLDN   , & 
                   PRCP    ,LWDN    ,TBOT    ,ZLVL    ,FICEOLD ,ZSOIL   , & 
                   QSNOW   ,SNEQVO  ,ALBOLD  ,CM      ,CH      ,ISNOW   , & 
                   SNEQV   ,SMC     ,ZSNSO   ,SNOWH   ,SNICE   ,SNLIQ   , & 
                   TG      ,STC     ,SH2O    ,TAUSS   ,QSFC    ,          & 
                   FSA     ,FSR     ,FIRA    ,FSH     ,FGEV    ,SSOIL   , & 
                   TRAD    ,EDIR    ,RUNSRF  ,RUNSUB  ,SAG     ,ALBEDO  , & 
                   QSNBOT  ,PONDING ,PONDING1,PONDING2,T2M     ,Q2E     , & 
                   EMISSI,  FPICE,    CH2B                                & 
                   )





  implicit none


  INTEGER                        , INTENT(IN)    :: ILOC   
  INTEGER                        , INTENT(IN)    :: JLOC   
  REAL                           , INTENT(IN)    :: COSZ   
  INTEGER                        , INTENT(IN)    :: NSNOW  
  INTEGER                        , INTENT(IN)    :: NSOIL  
  REAL                           , INTENT(IN)    :: DT     
  REAL                           , INTENT(IN)    :: SFCTMP 
  REAL                           , INTENT(IN)    :: SFCPRS 
  REAL                           , INTENT(IN)    :: UU     
  REAL                           , INTENT(IN)    :: VV     
  REAL                           , INTENT(IN)    :: Q2     
  REAL                           , INTENT(IN)    :: SOLDN  
  REAL                           , INTENT(IN)    :: PRCP   
  REAL                           , INTENT(IN)    :: LWDN   
  REAL                           , INTENT(IN)    :: TBOT   
  REAL                           , INTENT(IN)    :: ZLVL   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  



  REAL                           , INTENT(INOUT) :: QSNOW  
  REAL                           , INTENT(INOUT) :: SNEQVO 
  REAL                           , INTENT(INOUT) :: ALBOLD 
  REAL                           , INTENT(INOUT) :: CM     
  REAL                           , INTENT(INOUT) :: CH     


  INTEGER                        , INTENT(INOUT) :: ISNOW  
  REAL                           , INTENT(INOUT) :: SNEQV  
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  
  REAL                           , INTENT(INOUT) :: SNOWH  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  
  REAL                           , INTENT(INOUT) :: TG     
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   
  REAL                           , INTENT(INOUT) :: TAUSS  
  REAL                           , INTENT(INOUT) :: QSFC   


  REAL                           , INTENT(OUT)   :: FSA    
  REAL                           , INTENT(OUT)   :: FSR    
  REAL                           , INTENT(OUT)   :: FIRA   
  REAL                           , INTENT(OUT)   :: FSH    
  REAL                           , INTENT(OUT)   :: FGEV   
  REAL                           , INTENT(OUT)   :: SSOIL  
  REAL                           , INTENT(OUT)   :: TRAD   
  REAL                           , INTENT(OUT)   :: EDIR   
  REAL                           , INTENT(OUT)   :: RUNSRF 
  REAL                           , INTENT(OUT)   :: RUNSUB 
  REAL                           , INTENT(OUT)   :: SAG    
  REAL                           , INTENT(OUT)   :: ALBEDO 
  REAL                           , INTENT(OUT)   :: QSNBOT 
  REAL                           , INTENT(OUT)   :: PONDING
  REAL                           , INTENT(OUT)   :: PONDING1
  REAL                           , INTENT(OUT)   :: PONDING2
  REAL                           , INTENT(OUT)   :: T2M     
  REAL                           , INTENT(OUT)   :: Q2E
  REAL                           , INTENT(OUT)   :: EMISSI
  REAL                           , INTENT(OUT)   :: FPICE
  REAL                           , INTENT(OUT)   :: CH2B


  INTEGER                                        :: IZ     
  INTEGER, DIMENSION(-NSNOW+1:NSOIL)             :: IMELT  
  REAL                                           :: RHOAIR 
  REAL, DIMENSION(-NSNOW+1:NSOIL)                :: DZSNSO 
  REAL                                           :: THAIR  
  REAL                                           :: QAIR   
  REAL                                           :: EAIR   
  REAL, DIMENSION(       1:    2)                :: SOLAD  
  REAL, DIMENSION(       1:    2)                :: SOLAI  
  REAL, DIMENSION(       1:NSOIL)                :: SICE   
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNICEV 
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNLIQV 
  REAL, DIMENSION(-NSNOW+1:    0)                :: EPORE  
  REAL                                           :: QDEW   
  REAL                                           :: QVAP   
  REAL                                           :: LATHEA 
  REAL                                           :: QMELT  
  REAL                                           :: SWDOWN 
  REAL                                           :: BEG_WB 
  REAL                                           :: ZBOT = -8.0 

  CHARACTER*256 message




   CALL ATM_GLACIER (SFCPRS ,SFCTMP ,Q2     ,SOLDN  ,COSZ   ,THAIR  , & 
                     QAIR   ,EAIR   ,RHOAIR ,SOLAD  ,SOLAI  ,SWDOWN )

   BEG_WB = SNEQV



     DO IZ = ISNOW+1, NSOIL
         IF(IZ == ISNOW+1) THEN
           DZSNSO(IZ) = - ZSNSO(IZ)
         ELSE
           DZSNSO(IZ) = ZSNSO(IZ-1) - ZSNSO(IZ)
         END IF
     END DO



    CALL ENERGY_GLACIER (NSNOW  ,NSOIL  ,ISNOW  ,DT     ,QSNOW  ,RHOAIR , & 
                         EAIR   ,SFCPRS ,QAIR   ,SFCTMP ,LWDN   ,UU     , & 
                         VV     ,SOLAD  ,SOLAI  ,COSZ   ,ZLVL   ,         & 
                         TBOT   ,ZBOT   ,ZSNSO  ,DZSNSO ,                 & 
                         TG     ,STC    ,SNOWH  ,SNEQV  ,SNEQVO ,SH2O   , & 
                         SMC    ,SNICE  ,SNLIQ  ,ALBOLD ,CM     ,CH     , & 
                         TAUSS  ,QSFC   ,                                 & 
                         IMELT  ,SNICEV ,SNLIQV ,EPORE  ,QMELT  ,PONDING, & 
		         SAG    ,FSA    ,FSR    ,FIRA   ,FSH    ,FGEV   , & 
		         TRAD   ,T2M    ,SSOIL  ,LATHEA ,Q2E    ,EMISSI, CH2B )   

    SICE = MAX(0.0, SMC - SH2O)   
    SNEQVO  = SNEQV

    QVAP = MAX( FGEV/LATHEA, 0.)       
    QDEW = ABS( MIN(FGEV/LATHEA, 0.))  
    EDIR = QVAP - QDEW



     CALL WATER_GLACIER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,PRCP   ,SFCTMP , & 
                         QVAP   ,QDEW   ,FICEOLD,ZSOIL  ,                 & 
                         ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  ,STC    , & 
                         DZSNSO ,SH2O   ,SICE   ,PONDING,ZSNSO  ,FSH    , & 
                         RUNSRF ,RUNSUB ,QSNOW  ,PONDING1       ,PONDING2,QSNBOT,FPICE &  
                        )

     IF(OPT_GLA == 2) THEN
       EDIR = QVAP - QDEW
       FGEV = EDIR * LATHEA
     END IF

     IF(MAXVAL(SICE) < 0.0001) THEN
       WRITE(message,*) "GLACIER HAS MELTED AT:",ILOC,JLOC," ARE YOU SURE THIS SHOULD BE A GLACIER POINT?"
       CALL wrf_debug(10,TRIM(message))
     END IF
     


     CALL ERROR_GLACIER (ILOC   ,JLOC   ,SWDOWN ,FSA    ,FSR    ,FIRA   , &
                         FSH    ,FGEV   ,SSOIL  ,SAG    ,PRCP   ,EDIR   , &
		         RUNSRF ,RUNSUB ,SNEQV  ,DT     ,BEG_WB )

    IF(SNOWH <= 1.E-6 .OR. SNEQV <= 1.E-3) THEN
     SNOWH = 0.0
     SNEQV = 0.0
    END IF

    IF(SWDOWN.NE.0.) THEN
      ALBEDO = FSR / SWDOWN
    ELSE
      ALBEDO = -999.9
    END IF
    

  END SUBROUTINE NOAHMP_GLACIER

  SUBROUTINE ATM_GLACIER (SFCPRS ,SFCTMP ,Q2     ,SOLDN  ,COSZ   ,THAIR  , &
                          QAIR   ,EAIR   ,RHOAIR ,SOLAD  ,SOLAI  , &
                          SWDOWN )     



  IMPLICIT NONE



  REAL                          , INTENT(IN)  :: SFCPRS 
  REAL                          , INTENT(IN)  :: SFCTMP 
  REAL                          , INTENT(IN)  :: Q2     
  REAL                          , INTENT(IN)  :: SOLDN  
  REAL                          , INTENT(IN)  :: COSZ   



  REAL                          , INTENT(OUT) :: THAIR  
  REAL                          , INTENT(OUT) :: QAIR   
  REAL                          , INTENT(OUT) :: EAIR   
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  
  REAL                          , INTENT(OUT) :: RHOAIR 
  REAL                          , INTENT(OUT) :: SWDOWN 



  REAL                                        :: PAIR   


       PAIR   = SFCPRS                   
       THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 

       QAIR   = Q2                       

       EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
       RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)

       IF(COSZ <= 0.) THEN 
          SWDOWN = 0.
       ELSE
          SWDOWN = SOLDN
       END IF 

       SOLAD(1) = SWDOWN*0.7*0.5     
       SOLAD(2) = SWDOWN*0.7*0.5     
       SOLAI(1) = SWDOWN*0.3*0.5     
       SOLAI(2) = SWDOWN*0.3*0.5     

  END SUBROUTINE ATM_GLACIER


  SUBROUTINE ENERGY_GLACIER (NSNOW  ,NSOIL  ,ISNOW  ,DT     ,QSNOW  ,RHOAIR , & 
                             EAIR   ,SFCPRS ,QAIR   ,SFCTMP ,LWDN   ,UU     , & 
                             VV     ,SOLAD  ,SOLAI  ,COSZ   ,ZREF   ,         & 
                             TBOT   ,ZBOT   ,ZSNSO  ,DZSNSO ,                 & 
                             TG     ,STC    ,SNOWH  ,SNEQV  ,SNEQVO ,SH2O   , & 
                             SMC    ,SNICE  ,SNLIQ  ,ALBOLD ,CM     ,CH     , & 
                             TAUSS  ,QSFC   ,                                 & 
                             IMELT  ,SNICEV ,SNLIQV ,EPORE  ,QMELT  ,PONDING, & 
                             SAG    ,FSA    ,FSR    ,FIRA   ,FSH    ,FGEV   , & 
                             TRAD   ,T2M    ,SSOIL  ,LATHEA ,Q2E    ,EMISSI, CH2B )   






  IMPLICIT NONE


  INTEGER                           , INTENT(IN)    :: NSNOW  
  INTEGER                           , INTENT(IN)    :: NSOIL  
  INTEGER                           , INTENT(IN)    :: ISNOW  
  REAL                              , INTENT(IN)    :: DT     
  REAL                              , INTENT(IN)    :: QSNOW  
  REAL                              , INTENT(IN)    :: RHOAIR 
  REAL                              , INTENT(IN)    :: EAIR   
  REAL                              , INTENT(IN)    :: SFCPRS 
  REAL                              , INTENT(IN)    :: QAIR   
  REAL                              , INTENT(IN)    :: SFCTMP 
  REAL                              , INTENT(IN)    :: LWDN   
  REAL                              , INTENT(IN)    :: UU     
  REAL                              , INTENT(IN)    :: VV     
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAD  
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAI  
  REAL                              , INTENT(IN)    :: COSZ   
  REAL                              , INTENT(IN)    :: ZREF   
  REAL                              , INTENT(IN)    :: TBOT   
  REAL                              , INTENT(IN)    :: ZBOT   
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: ZSNSO  
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO 


  REAL                              , INTENT(INOUT) :: TG     
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
  REAL                              , INTENT(INOUT) :: SNOWH  
  REAL                              , INTENT(INOUT) :: SNEQV  
  REAL                              , INTENT(INOUT) :: SNEQVO 
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  
  REAL                              , INTENT(INOUT) :: ALBOLD 
  REAL                              , INTENT(INOUT) :: CM     
  REAL                              , INTENT(INOUT) :: CH     
  REAL                              , INTENT(INOUT) :: TAUSS  
  REAL                              , INTENT(INOUT) :: QSFC   


  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT)   :: IMELT  
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNICEV 
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNLIQV 
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: EPORE  
  REAL                              , INTENT(OUT)   :: QMELT  
  REAL                              , INTENT(OUT)   :: PONDING
  REAL                              , INTENT(OUT)   :: SAG    
  REAL                              , INTENT(OUT)   :: FSA    
  REAL                              , INTENT(OUT)   :: FSR    
  REAL                              , INTENT(OUT)   :: FIRA   
  REAL                              , INTENT(OUT)   :: FSH    
  REAL                              , INTENT(OUT)   :: FGEV   
  REAL                              , INTENT(OUT)   :: TRAD   
  REAL                              , INTENT(OUT)   :: T2M    
  REAL                              , INTENT(OUT)   :: SSOIL  
  REAL                              , INTENT(OUT)   :: LATHEA 
  REAL                              , INTENT(OUT)   :: Q2E
  REAL                              , INTENT(OUT)   :: EMISSI
  REAL                              , INTENT(OUT)   :: CH2B   



  REAL                                              :: UR     
  REAL                                              :: ZLVL   
  REAL                                              :: RSURF  
  REAL                                              :: ZPD    
  REAL                                              :: Z0MG   
  REAL                                              :: EMG    
  REAL                                              :: FIRE   
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: FACT   
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: DF     
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: HCPCT  
  REAL                                              :: GAMMA  
  REAL                                              :: RHSUR  





    UR = MAX( SQRT(UU**2.+VV**2.), 1. )



     Z0MG = Z0SNO
     ZPD  = SNOWH

     ZLVL = ZPD + ZREF



  CALL THERMOPROP_GLACIER (NSOIL   ,NSNOW   ,ISNOW   ,DZSNSO  ,          & 
                           DT      ,SNOWH   ,SNICE   ,SNLIQ   ,          & 
                           DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & 
                           FACT    )                                       



  CALL  RADIATION_GLACIER (DT      ,TG      ,SNEQVO  ,SNEQV   ,COSZ    , & 
                           QSNOW   ,SOLAD   ,SOLAI   ,                   & 
                           ALBOLD  ,TAUSS   ,                            & 
                           SAG     ,FSR     ,FSA)                          



     EMG = 0.98



     RHSUR = 1.0
     RSURF = 1.0



     LATHEA = HSUB
     GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)



    CALL GLACIER_FLUX (NSOIL   ,NSNOW   ,EMG     ,ISNOW   ,DF      ,DZSNSO  ,Z0MG    , & 
                       ZLVL    ,ZPD     ,QAIR    ,SFCTMP  ,RHOAIR  ,SFCPRS  , & 
		       UR      ,GAMMA   ,RSURF   ,LWDN    ,RHSUR   ,SMC     , & 
		       EAIR    ,STC     ,SAG     ,SNOWH   ,LATHEA  ,SH2O    , & 
		       CM      ,CH      ,TG      ,QSFC    ,          & 
		       FIRA    ,FSH     ,FGEV    ,SSOIL   ,          & 
		       T2M     ,Q2E     ,CH2B)                         



    FIRE = LWDN + FIRA

    IF(FIRE <=0.) call wrf_error_fatal3("<stdin>",491,&
"STOP in Noah-MP: emitted longwave <0")

    
    EMISSI = EMG



    
    
    TRAD = ( ( FIRE - (1-EMISSI)*LWDN ) / (EMISSI*SB) ) ** 0.25



    CALL TSNOSOI_GLACIER (NSOIL   ,NSNOW   ,ISNOW   ,DT      ,TBOT    , & 
                          SSOIL   ,SNOWH   ,ZBOT    ,ZSNSO   ,DF      , & 
		          HCPCT   ,                                     & 
                          STC     )                                       


     IF(OPT_STC == 2) THEN
      IF (SNOWH > 0.05 .AND. TG > TFRZ) TG = TFRZ
     END IF



 CALL PHASECHANGE_GLACIER (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & 
                           DZSNSO  ,                                     & 
                           STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & 
                           SMC     ,SH2O    ,                            & 
                           QMELT   ,IMELT   ,PONDING )                     


  END SUBROUTINE ENERGY_GLACIER

  SUBROUTINE THERMOPROP_GLACIER (NSOIL   ,NSNOW   ,ISNOW   ,DZSNSO  , & 
                                 DT      ,SNOWH   ,SNICE   ,SNLIQ   , & 
                                 DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & 
                                 FACT    )                                       


  IMPLICIT NONE


  INTEGER                        , INTENT(IN)  :: NSOIL   
  INTEGER                        , INTENT(IN)  :: NSNOW   
  INTEGER                        , INTENT(IN)  :: ISNOW   
  REAL                           , INTENT(IN)  :: DT      
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNICE   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNLIQ   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO  
  REAL                           , INTENT(IN)  :: SNOWH   


  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: DF      
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: HCPCT   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: FACT    



  INTEGER :: IZ, IZ2
  REAL, DIMENSION(-NSNOW+1:    0)              :: CVSNO   
  REAL, DIMENSION(-NSNOW+1:    0)              :: TKSNO   
  REAL                                         :: ZMID    




    CALL CSNOW_GLACIER (ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & 
                        TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   

    DO IZ = ISNOW+1, 0
      DF   (IZ) = TKSNO(IZ)
      HCPCT(IZ) = CVSNO(IZ)
    END DO



    DO  IZ = 1, NSOIL
       ZMID      = 0.5 * (DZSNSO(IZ))
       DO IZ2 = 1, IZ-1
         ZMID = ZMID + DZSNSO(IZ2)
       END DO
       HCPCT(IZ) = 1.E6 * ( 0.8194 + 0.1309*ZMID )
       DF(IZ)    = 0.32333 + ( 0.10073 * ZMID )
    END DO
       


    DO IZ = ISNOW+1,NSOIL
     FACT(IZ) = DT/(HCPCT(IZ)*DZSNSO(IZ))
    END DO



    IF(ISNOW == 0) THEN
       DF(1) = (DF(1)*DZSNSO(1)+0.35*SNOWH)      / (SNOWH    +DZSNSO(1)) 
    ELSE
       DF(1) = (DF(1)*DZSNSO(1)+DF(0)*DZSNSO(0)) / (DZSNSO(0)+DZSNSO(1))
    END IF


  END SUBROUTINE THERMOPROP_GLACIER


  SUBROUTINE CSNOW_GLACIER (ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & 
                            TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   



  IMPLICIT NONE



  INTEGER,                          INTENT(IN) :: ISNOW  
  INTEGER                        ,  INTENT(IN) :: NSNOW  
  INTEGER                        ,  INTENT(IN) :: NSOIL  
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNICE  
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNLIQ  
  REAL, DIMENSION(-NSNOW+1:NSOIL),  INTENT(IN) :: DZSNSO 



  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: CVSNO  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: TKSNO  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV 
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV 
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE  



  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0) :: BDSNOI  




  DO IZ = ISNOW+1, 0
      SNICEV(IZ)   = MIN(1., SNICE(IZ)/(DZSNSO(IZ)*DENICE) )
      EPORE(IZ)    = 1. - SNICEV(IZ)
      SNLIQV(IZ)   = MIN(EPORE(IZ),SNLIQ(IZ)/(DZSNSO(IZ)*DENH2O))
  ENDDO

  DO IZ = ISNOW+1, 0
      BDSNOI(IZ) = (SNICE(IZ)+SNLIQ(IZ))/DZSNSO(IZ)
      CVSNO(IZ) = CICE*SNICEV(IZ)+CWAT*SNLIQV(IZ)

  enddo



  DO IZ = ISNOW+1, 0
     TKSNO(IZ) = 3.2217E-6*BDSNOI(IZ)**2.           




  ENDDO

  END SUBROUTINE CSNOW_GLACIER

  SUBROUTINE RADIATION_GLACIER (DT      ,TG      ,SNEQVO  ,SNEQV   ,COSZ    , & 
                                QSNOW   ,SOLAD   ,SOLAI   ,                   & 
                                ALBOLD  ,TAUSS   ,                            & 
                                SAG     ,FSR     ,FSA)                          

  IMPLICIT NONE


  REAL, INTENT(IN)                     :: DT     
  REAL, INTENT(IN)                     :: TG     
  REAL, INTENT(IN)                     :: SNEQVO 
  REAL, INTENT(IN)                     :: SNEQV  
  REAL, INTENT(IN)                     :: COSZ   
  REAL, INTENT(IN)                     :: QSNOW  
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAD  
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAI  


  REAL,                  INTENT(INOUT) :: ALBOLD 
  REAL,                  INTENT(INOUT) :: TAUSS  


  REAL, INTENT(OUT)                    :: SAG    
  REAL, INTENT(OUT)                    :: FSR    
  REAL, INTENT(OUT)                    :: FSA    


  INTEGER                              :: IB     
  INTEGER                              :: NBAND  
  REAL                                 :: FAGE   
  REAL, DIMENSION(1:2)                 :: ALBSND 
  REAL, DIMENSION(1:2)                 :: ALBSNI 
  REAL                                 :: ALB    
  REAL                                 :: ABS    
  REAL                                 :: REF    
  REAL                                 :: FSNO   
  REAL, DIMENSION(1:2)                 :: ALBICE 

  REAL,PARAMETER :: MPE = 1.E-6



  NBAND = 2
  ALBSND = 0.0
  ALBSNI = 0.0
  ALBICE(1) = 0.80    
  ALBICE(2) = 0.55



  CALL SNOW_AGE_GLACIER (DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)



  IF(OPT_ALB == 1) &
     CALL SNOWALB_BATS_GLACIER (NBAND,COSZ,FAGE,ALBSND,ALBSNI)
  IF(OPT_ALB == 2) THEN
     CALL SNOWALB_CLASS_GLACIER(NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI)
     ALBOLD = ALB
  END IF



   SAG = 0.
   FSA = 0.
   FSR = 0.
   
   FSNO = 0.0
   IF(SNEQV > 0.0) FSNO = 1.0



  DO IB = 1, NBAND

    ALBSND(IB) = ALBICE(IB)*(1.-FSNO) + ALBSND(IB)*FSNO
    ALBSNI(IB) = ALBICE(IB)*(1.-FSNO) + ALBSNI(IB)*FSNO



    ABS = SOLAD(IB)*(1.-ALBSND(IB)) + SOLAI(IB)*(1.-ALBSNI(IB))
    SAG = SAG + ABS
    FSA = FSA + ABS
    
    REF = SOLAD(IB)*ALBSND(IB) + SOLAI(IB)*ALBSNI(IB)
    FSR = FSR + REF
    
  END DO

  END SUBROUTINE RADIATION_GLACIER

  SUBROUTINE SNOW_AGE_GLACIER (DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)

  IMPLICIT NONE




   REAL, INTENT(IN) :: DT        
   REAL, INTENT(IN) :: TG        
   REAL, INTENT(IN) :: SNEQVO    
   REAL, INTENT(IN) :: SNEQV     


  REAL,  INTENT(INOUT) :: TAUSS  


   REAL, INTENT(OUT) :: FAGE     


   REAL            :: TAGE       
   REAL            :: AGE1       
   REAL            :: AGE2       
   REAL            :: AGE3       
   REAL            :: DELA       
   REAL            :: SGE        
   REAL            :: DELS       
   REAL            :: DELA0      
   REAL            :: ARG        



   IF(SNEQV.LE.0.0) THEN
          TAUSS = 0.
   ELSE IF (SNEQV.GT.800.) THEN
          TAUSS = 0.
   ELSE

          DELA0 = 1.E-6*DT
          ARG   = 5.E3*(1./TFRZ-1./TG)
          AGE1  = EXP(ARG)
          AGE2  = EXP(AMIN1(0.,10.*ARG))
          AGE3  = 0.3
          TAGE  = AGE1+AGE2+AGE3
          DELA  = DELA0*TAGE
          DELS  = AMAX1(0.0,SNEQV-SNEQVO) / SWEMX
          SGE   = (TAUSS+DELA)*(1.0-DELS)
          TAUSS = AMAX1(0.,SGE)
   ENDIF

   FAGE= TAUSS/(TAUSS+1.)

  END SUBROUTINE SNOW_AGE_GLACIER


  SUBROUTINE SNOWALB_BATS_GLACIER (NBAND,COSZ,FAGE,ALBSND,ALBSNI)

  IMPLICIT NONE



  INTEGER,INTENT(IN) :: NBAND  

  REAL,INTENT(IN) :: COSZ    
  REAL,INTENT(IN) :: FAGE    



  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND 
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI 


  REAL :: FZEN                 
  REAL :: CF1                  
  REAL :: SL2                  
  REAL :: SL1                  
  REAL :: SL                   
  REAL, PARAMETER :: C1 = 0.2  
  REAL, PARAMETER :: C2 = 0.5  





        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.



        SL=2.0
        SL1=1./SL
        SL2=2.*SL
        CF1=((1.+SL1)/(1.+SL2*COSZ)-SL1)
        FZEN=AMAX1(CF1,0.)

        ALBSNI(1)=0.95*(1.-C1*FAGE)         
        ALBSNI(2)=0.65*(1.-C2*FAGE)        

        ALBSND(1)=ALBSNI(1)+0.4*FZEN*(1.-ALBSNI(1))    
        ALBSND(2)=ALBSNI(2)+0.4*FZEN*(1.-ALBSNI(2))    

  END SUBROUTINE SNOWALB_BATS_GLACIER


  SUBROUTINE SNOWALB_CLASS_GLACIER (NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI)

  IMPLICIT NONE



  INTEGER,INTENT(IN) :: NBAND  

  REAL,INTENT(IN) :: QSNOW     
  REAL,INTENT(IN) :: DT        
  REAL,INTENT(IN) :: ALBOLD    



  REAL,                INTENT(INOUT) :: ALB        


  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND 
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI 





        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.



         ALB = 0.55 + (ALBOLD-0.55) * EXP(-0.01*DT/3600.)




         IF (QSNOW > 0.) then
           ALB = ALB + MIN(QSNOW*DT,SWEMX) * (0.84-ALB)/(SWEMX)
         ENDIF

         ALBSNI(1)= ALB         
         ALBSNI(2)= ALB         
         ALBSND(1)= ALB         
         ALBSND(2)= ALB         

  END SUBROUTINE SNOWALB_CLASS_GLACIER

  SUBROUTINE GLACIER_FLUX (NSOIL   ,NSNOW   ,EMG     ,ISNOW   ,DF      ,DZSNSO  ,Z0M     , & 
                           ZLVL    ,ZPD     ,QAIR    ,SFCTMP  ,RHOAIR  ,SFCPRS  , & 
			   UR      ,GAMMA   ,RSURF   ,LWDN    ,RHSUR   ,SMC     , & 
			   EAIR    ,STC     ,SAG     ,SNOWH   ,LATHEA  ,SH2O    , & 
                           CM      ,CH      ,TGB     ,QSFC    ,          & 
                           IRB     ,SHB     ,EVB     ,GHB     ,          & 
                           T2MB    ,Q2B     ,EHB2)                         










  IMPLICIT NONE


  INTEGER, INTENT(IN)                         :: NSNOW  
  INTEGER, INTENT(IN)                         :: NSOIL  
  REAL,                            INTENT(IN) :: EMG    
  INTEGER,                         INTENT(IN) :: ISNOW  
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO 
  REAL,                            INTENT(IN) :: Z0M    
  REAL,                            INTENT(IN) :: ZLVL   
  REAL,                            INTENT(IN) :: ZPD    
  REAL,                            INTENT(IN) :: QAIR   
  REAL,                            INTENT(IN) :: SFCTMP 
  REAL,                            INTENT(IN) :: RHOAIR 
  REAL,                            INTENT(IN) :: SFCPRS 
  REAL,                            INTENT(IN) :: UR     
  REAL,                            INTENT(IN) :: GAMMA  
  REAL,                            INTENT(IN) :: RSURF  
  REAL,                            INTENT(IN) :: LWDN   
  REAL,                            INTENT(IN) :: RHSUR  
  REAL,                            INTENT(IN) :: EAIR   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SH2O   
  REAL,                            INTENT(IN) :: SAG    
  REAL,                            INTENT(IN) :: SNOWH  
  REAL,                            INTENT(IN) :: LATHEA 


  REAL,                         INTENT(INOUT) :: CM     
  REAL,                         INTENT(INOUT) :: CH     
  REAL,                         INTENT(INOUT) :: TGB    
  REAL,                         INTENT(INOUT) :: QSFC   



  REAL,                           INTENT(OUT) :: IRB    
  REAL,                           INTENT(OUT) :: SHB    
  REAL,                           INTENT(OUT) :: EVB    
  REAL,                           INTENT(OUT) :: GHB    
  REAL,                           INTENT(OUT) :: T2MB   
  REAL,                           INTENT(OUT) :: Q2B    
  REAL,                           INTENT(OUT) :: EHB2   



  INTEGER :: NITERB  
  REAL    :: MPE     
  REAL    :: DTG        
  INTEGER :: MOZSGN  
  REAL    :: MOZOLD     
  REAL    :: FM2          
  REAL    :: FH2          
  REAL    :: CH2          
  REAL    :: H          
  REAL    :: FV         
  REAL    :: CIR        
  REAL    :: CGH        
  REAL    :: CSH        
  REAL    :: CEV        
  REAL    :: CQ2B       
  INTEGER :: ITER    
  REAL    :: Z0H        
  REAL    :: MOZ        
  REAL    :: FM         
  REAL    :: FH         
  REAL    :: RAMB       
  REAL    :: RAHB       
  REAL    :: RAWB       
  REAL    :: ESTG       
  REAL    :: DESTG      
  REAL    :: ESATW      
  REAL    :: ESATI      
  REAL    :: DSATW      
  REAL    :: DSATI      
  REAL    :: A          
  REAL    :: B          
  REAL    :: T, TDC     
  REAL, DIMENSION(       1:NSOIL) :: SICE   

  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )




        NITERB = 5
        MPE    = 1E-6
        DTG    = 0.
        MOZ    = 0.
        MOZSGN = 0
        MOZOLD = 0.
        H      = 0.
        FV     = 0.1

        CIR = EMG*SB
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)


      loop3: DO ITER = 1, NITERB  

        Z0H = Z0M 



        CALL SFCDIF1_GLACIER(ITER   ,ZLVL   ,ZPD    ,Z0H    ,Z0M    , & 
                     QAIR   ,SFCTMP ,H      ,RHOAIR ,MPE    ,UR     , & 
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2    ,FH2    , & 
       &             FV     ,CM     ,CH     ,CH2)                       

        RAMB = MAX(1.,1./(CM*UR))
        RAHB = MAX(1.,1./(CH*UR))
        RAWB = RAHB



        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        CSH = RHOAIR*CPAIR/RAHB
	IF(SNOWH > 0.0 .OR. OPT_GLA == 1) THEN
          CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)
	ELSE
	  CEV = 0.0   
	END IF



        IRB   = CIR * TGB**4 - EMG*LWDN
        SHB   = CSH * (TGB        - SFCTMP      )
        EVB   = CEV * (ESTG*RHSUR - EAIR        )
        GHB   = CGH * (TGB        - STC(ISNOW+1))

        B     = SAG-IRB-SHB-EVB-GHB
        A     = 4.*CIR*TGB**3 + CSH + CEV*DESTG + CGH
        DTG   = B/A

        IRB = IRB + 4.*CIR*TGB**3*DTG
        SHB = SHB + CSH*DTG
        EVB = EVB + CEV*DESTG*DTG
        GHB = GHB + CGH*DTG


        TGB = TGB + DTG


        H = CSH * (TGB - SFCTMP)

        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
        ELSE
            ESTG  = ESATI
        END IF
        QSFC = 0.622*(ESTG*RHSUR)/(SFCPRS-0.378*(ESTG*RHSUR))

     END DO loop3 




     SICE = SMC - SH2O
     IF(OPT_STC == 1 .OR. OPT_STC ==3) THEN
     IF ((MAXVAL(SICE) > 0.0 .OR. SNOWH > 0.0) .AND. TGB > TFRZ .AND. OPT_GLA == 1) THEN
          TGB = TFRZ
          T = TDC(TGB)                              
          CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
          ESTG  = ESATI
          QSFC = 0.622*(ESTG*RHSUR)/(SFCPRS-0.378*(ESTG*RHSUR))
          IRB = CIR * TGB**4 - EMG*LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR )          
          GHB = SAG - (IRB+SHB+EVB)
     END IF
     END IF


     EHB2  = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
     CQ2B  = EHB2
     IF (EHB2.lt.1.E-5 ) THEN
       T2MB  = TGB
       Q2B   = QSFC
     ELSE
       T2MB  = TGB - SHB/(RHOAIR*CPAIR) * 1./EHB2
       Q2B   = QSFC - EVB/(LATHEA*RHOAIR)*(1./CQ2B + RSURF)
     ENDIF


     CH = 1./RAHB

  END SUBROUTINE GLACIER_FLUX

  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)



  IMPLICIT NONE



  REAL, intent(in)  :: T              



  REAL, intent(out) :: ESW            
  REAL, intent(out) :: ESI            
  REAL, intent(out) :: DESW           
  REAL, intent(out) :: DESI           



  REAL :: A0,A1,A2,A3,A4,A5,A6  
  REAL :: B0,B1,B2,B3,B4,B5,B6  
  REAL :: C0,C1,C2,C3,C4,C5,C6  
  REAL :: D0,D1,D2,D3,D4,D5,D6  

  PARAMETER (A0=6.107799961    , A1=4.436518521E-01,  &
             A2=1.428945805E-02, A3=2.650648471E-04,  &
             A4=3.031240396E-06, A5=2.034080948E-08,  &
             A6=6.136820929E-11)

  PARAMETER (B0=6.109177956    , B1=5.034698970E-01,  &
             B2=1.886013408E-02, B3=4.176223716E-04,  &
             B4=5.824720280E-06, B5=4.838803174E-08,  &
             B6=1.838826904E-10)

  PARAMETER (C0= 4.438099984E-01, C1=2.857002636E-02,  &
             C2= 7.938054040E-04, C3=1.215215065E-05,  &
             C4= 1.036561403E-07, C5=3.532421810e-10,  &
             C6=-7.090244804E-13)

  PARAMETER (D0=5.030305237E-01, D1=3.773255020E-02,  &
             D2=1.267995369E-03, D3=2.477563108E-05,  &
             D4=3.005693132E-07, D5=2.158542548E-09,  &
             D6=7.131097725E-12)

  ESW  = 100.*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
  ESI  = 100.*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
  DESW = 100.*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
  DESI = 100.*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))

  END SUBROUTINE ESAT


  SUBROUTINE SFCDIF1_GLACIER(ITER   ,ZLVL   ,ZPD    ,Z0H    ,Z0M    , & 
                     QAIR   ,SFCTMP ,H      ,RHOAIR ,MPE    ,UR     , & 
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2    ,FH2    , & 
       &             FV     ,CM     ,CH     ,CH2     )                  



    IMPLICIT NONE


    INTEGER,              INTENT(IN) :: ITER   
    REAL,                 INTENT(IN) :: ZLVL   
    REAL,                 INTENT(IN) :: ZPD    
    REAL,                 INTENT(IN) :: Z0H    
    REAL,                 INTENT(IN) :: Z0M    
    REAL,                 INTENT(IN) :: QAIR   
    REAL,                 INTENT(IN) :: SFCTMP 
    REAL,                 INTENT(IN) :: H      
    REAL,                 INTENT(IN) :: RHOAIR 
    REAL,                 INTENT(IN) :: MPE    
    REAL,                 INTENT(IN) :: UR     


    REAL,              INTENT(INOUT) :: MOZ    
    INTEGER,           INTENT(INOUT) :: MOZSGN 
    REAL,              INTENT(INOUT) :: FM     
    REAL,              INTENT(INOUT) :: FH     
    REAL,              INTENT(INOUT) :: FM2    
    REAL,              INTENT(INOUT) :: FH2    


    REAL,                INTENT(OUT) :: FV     
    REAL,                INTENT(OUT) :: CM     
    REAL,                INTENT(OUT) :: CH     
    REAL,                INTENT(OUT) :: CH2    


    REAL    :: MOZOLD                   
    REAL    :: TMPCM                    
    REAL    :: TMPCH                    
    REAL    :: MOL                      
    REAL    :: TVIR                     
    REAL    :: TMP1,TMP2,TMP3           
    REAL    :: FMNEW                    
    REAL    :: FHNEW                    
    REAL    :: MOZ2                     
    REAL    :: TMPCM2                   
    REAL    :: TMPCH2                   
    REAL    :: FM2NEW                   
    REAL    :: FH2NEW                   
    REAL    :: TMP12,TMP22,TMP32        

    REAL    :: CMFM, CHFH, CM2FM2, CH2FH2





    MOZOLD = MOZ
  
    IF(ZLVL <= ZPD) THEN
       write(*,*) 'critical glacier problem: ZLVL <= ZPD; model stops', zlvl, zpd
       call wrf_error_fatal3("<stdin>",1224,&
"STOP in Noah-MP glacier")
    ENDIF

    TMPCM = LOG((ZLVL-ZPD) / Z0M)
    TMPCH = LOG((ZLVL-ZPD) / Z0H)
    TMPCM2 = LOG((2.0 + Z0M) / Z0M)
    TMPCH2 = LOG((2.0 + Z0H) / Z0H)

    IF(ITER == 1) THEN
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    ELSE
       TVIR = (1. + 0.61*QAIR) * SFCTMP
       TMP1 = VKC * (GRAV/TVIR) * H/(RHOAIR*CPAIR)
       IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
       MOL  = -1. * FV**3 / TMP1
       MOZ  = MIN( (ZLVL-ZPD)/MOL, 1.)
       MOZ2  = MIN( (2.0 + Z0H)/MOL, 1.)
    ENDIF



    IF (MOZOLD*MOZ .LT. 0.) MOZSGN = MOZSGN+1
    IF (MOZSGN .GE. 2) THEN
       MOZ = 0.
       FM = 0.
       FH = 0.
       MOZ2 = 0.
       FM2 = 0.
       FH2 = 0.
    ENDIF


    IF (MOZ .LT. 0.) THEN
       TMP1 = (1. - 16.*MOZ)**0.25
       TMP2 = LOG((1.+TMP1*TMP1)/2.)
       TMP3 = LOG((1.+TMP1)/2.)
       FMNEW = 2.*TMP3 + TMP2 - 2.*ATAN(TMP1) + 1.5707963
       FHNEW = 2*TMP2


       TMP12 = (1. - 16.*MOZ2)**0.25
       TMP22 = LOG((1.+TMP12*TMP12)/2.)
       TMP32 = LOG((1.+TMP12)/2.)
       FM2NEW = 2.*TMP32 + TMP22 - 2.*ATAN(TMP12) + 1.5707963
       FH2NEW = 2*TMP22
    ELSE
       FMNEW = -5.*MOZ
       FHNEW = FMNEW
       FM2NEW = -5.*MOZ2
       FH2NEW = FM2NEW
    ENDIF




    IF (ITER == 1) THEN
       FM = FMNEW
       FH = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    ELSE
       FM = 0.5 * (FM+FMNEW)
       FH = 0.5 * (FH+FHNEW)
       FM2 = 0.5 * (FM2+FM2NEW)
       FH2 = 0.5 * (FH2+FH2NEW)
    ENDIF



    FH = MIN(FH,0.9*TMPCH)
    FM = MIN(FM,0.9*TMPCM)
    FH2 = MIN(FH2,0.9*TMPCH2)
    FM2 = MIN(FM2,0.9*TMPCM2)

    CMFM = TMPCM-FM
    CHFH = TMPCH-FH
    CM2FM2 = TMPCM2-FM2
    CH2FH2 = TMPCH2-FH2
    IF(ABS(CMFM) <= MPE) CMFM = MPE
    IF(ABS(CHFH) <= MPE) CHFH = MPE
    IF(ABS(CM2FM2) <= MPE) CM2FM2 = MPE
    IF(ABS(CH2FH2) <= MPE) CH2FH2 = MPE
    CM  = VKC*VKC/(CMFM*CMFM)
    CH  = VKC*VKC/(CMFM*CHFH)
    CH2  = VKC*VKC/(CM2FM2*CH2FH2)
        


    FV = UR * SQRT(CM)
    CH2  = VKC*FV/CH2FH2

  END SUBROUTINE SFCDIF1_GLACIER

  SUBROUTINE TSNOSOI_GLACIER (NSOIL   ,NSNOW   ,ISNOW   ,DT      ,TBOT    , & 
                              SSOIL   ,SNOWH   ,ZBOT    ,ZSNSO   ,DF      , & 
			      HCPCT   ,                                     & 
                              STC     )                                       





  IMPLICIT NONE



    INTEGER,                         INTENT(IN)  :: NSOIL  
    INTEGER,                         INTENT(IN)  :: NSNOW  
    INTEGER,                         INTENT(IN)  :: ISNOW  

    REAL,                            INTENT(IN)  :: DT     
    REAL,                            INTENT(IN)  :: TBOT   
    REAL,                            INTENT(IN)  :: SSOIL  
    REAL,                            INTENT(IN)  :: SNOWH  
    REAL,                            INTENT(IN)  :: ZBOT   
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  



    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC



    INTEGER                                      :: IZ
    REAL                                         :: ZBOTSNO   
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: AI, BI, CI, RHSTS
    REAL                                         :: EFLXB 
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: PHI   





    PHI(ISNOW+1:NSOIL) = 0.



    ZBOTSNO = ZBOT - SNOWH    



      CALL HRT_GLACIER   (NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                          STC       ,TBOT      ,ZBOTSNO   ,DF        , &
                          HCPCT     ,SSOIL     ,PHI       ,            &
                          AI        ,BI        ,CI        ,RHSTS     , &
                          EFLXB     )

      CALL HSTEP_GLACIER (NSNOW     ,NSOIL     ,ISNOW     ,DT        , &
                          AI        ,BI        ,CI        ,RHSTS     , &
                          STC       ) 

  END SUBROUTINE TSNOSOI_GLACIER


  SUBROUTINE HRT_GLACIER (NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , & 
                          STC       ,TBOT      ,ZBOT      ,DF        , & 
                          HCPCT     ,SSOIL     ,PHI       ,            & 
                          AI        ,BI        ,CI        ,RHSTS     , & 
                          BOTFLX    )                                    






    IMPLICIT NONE



    INTEGER,                         INTENT(IN)  :: NSOIL  
    INTEGER,                         INTENT(IN)  :: NSNOW  
    INTEGER,                         INTENT(IN)  :: ISNOW  
    REAL,                            INTENT(IN)  :: TBOT   
    REAL,                            INTENT(IN)  :: ZBOT   
                                                           
    REAL,                            INTENT(IN)  :: SSOIL  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC    
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: PHI    



    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: RHSTS  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: AI     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: BI     
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: CI     
    REAL,                            INTENT(OUT) :: BOTFLX 



    INTEGER                                      :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DENOM
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DTSDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: EFLUX
    REAL                                         :: TEMP1


    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           DENOM(K)  = - ZSNSO(K) * HCPCT(K)
           TEMP1     = - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = DF(K) * DTSDZ(K) - SSOIL - PHI(K)
        ELSE IF (K < NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     = ZSNSO(K-1) - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = (DF(K)*DTSDZ(K) - DF(K-1)*DTSDZ(K-1)) - PHI(K)
        ELSE IF (K == NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     =  ZSNSO(K-1) - ZSNSO(K)
           IF(OPT_TBOT == 1) THEN
               BOTFLX     = 0. 
           END IF
           IF(OPT_TBOT == 2) THEN
               DTSDZ(K)  = (STC(K) - TBOT) / ( 0.5*(ZSNSO(K-1)+ZSNSO(K)) - ZBOT)
               BOTFLX    = -DF(K) * DTSDZ(K)
           END IF
           EFLUX(K)  = (-BOTFLX - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
        END IF
    END DO

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           AI(K)    =   0.0
           CI(K)    = - DF(K)   * DDZ(K) / DENOM(K)
           IF (OPT_STC == 1 .OR. OPT_STC == 3) THEN
              BI(K) = - CI(K)
           END IF                                        
           IF (OPT_STC == 2) THEN
              BI(K) = - CI(K) + DF(K)/(0.5*ZSNSO(K)*ZSNSO(K)*HCPCT(K))
           END IF
        ELSE IF (K < NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = - DF(K  ) * DDZ(K  ) / DENOM(K) 
           BI(K)    = - (AI(K) + CI (K))
        ELSE IF (K == NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = 0.0
           BI(K)    = - (AI(K) + CI(K))
        END IF
           RHSTS(K)  = EFLUX(K)/ (-DENOM(K))
    END DO

  END SUBROUTINE HRT_GLACIER


  SUBROUTINE HSTEP_GLACIER (NSNOW     ,NSOIL     ,ISNOW     ,DT        ,  & 
                            AI        ,BI        ,CI        ,RHSTS     ,  & 
                            STC       )                                     



    implicit none



    INTEGER,                         INTENT(IN)    :: NSOIL
    INTEGER,                         INTENT(IN)    :: NSNOW
    INTEGER,                         INTENT(IN)    :: ISNOW
    REAL,                            INTENT(IN)    :: DT


    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: RHSTS


    INTEGER                                        :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: RHSTSIN
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: CIIN


    DO K = ISNOW+1,NSOIL
       RHSTS(K) =   RHSTS(K) * DT
       AI(K)    =      AI(K) * DT
       BI(K)    = 1. + BI(K) * DT
       CI(K)    =      CI(K) * DT
    END DO



    DO K = ISNOW+1,NSOIL
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    END DO



    CALL ROSR12_GLACIER (CI,AI,BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW)



    DO K = ISNOW+1,NSOIL
       STC (K) = STC (K) + CI (K)
    END DO

  END SUBROUTINE HSTEP_GLACIER

  SUBROUTINE ROSR12_GLACIER (P,A,B,C,D,DELTA,NTOP,NSOIL,NSNOW)


















    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NTOP           
    INTEGER, INTENT(IN)   :: NSOIL,NSNOW
    INTEGER               :: K, KK

    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(IN):: A, B, D
    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(INOUT):: C,P,DELTA




    C (NSOIL) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)



    DELTA (NTOP) = D (NTOP) / B (NTOP)



    DO K = NTOP+1,NSOIL
       P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
            * P (K -1)))
    END DO



    P (NSOIL) = DELTA (NSOIL)



    DO K = NTOP+1,NSOIL
       KK = NSOIL - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    END DO

  END SUBROUTINE ROSR12_GLACIER


  SUBROUTINE PHASECHANGE_GLACIER (NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & 
                                  DZSNSO  ,                                     & 
                                  STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & 
                                  SMC     ,SH2O    ,                            & 
                                  QMELT   ,IMELT   ,PONDING )                     



  IMPLICIT NONE



  INTEGER, INTENT(IN)                             :: NSNOW  
  INTEGER, INTENT(IN)                             :: NSOIL  
  INTEGER, INTENT(IN)                             :: ISNOW  
  REAL, INTENT(IN)                                :: DT     
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: FACT   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: DZSNSO 



  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT)  :: STC    
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNICE  
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNLIQ  
  REAL, INTENT(INOUT)                             :: SNEQV
  REAL, INTENT(INOUT)                             :: SNOWH
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SH2O   
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SMC    


  REAL,                               INTENT(OUT) :: QMELT  
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: IMELT  
  REAL,                               INTENT(OUT) :: PONDING



  INTEGER                         :: J,K         
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HM        
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: XM        
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WMASS0
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WICE0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WLIQ0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MICE      
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MLIQ      
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HEATR     
  REAL                            :: TEMP1     
  REAL                            :: PROPOR
  REAL                            :: XMF       




    QMELT   = 0.
    PONDING = 0.
    XMF     = 0.

    DO J = ISNOW+1,0           
         MICE(J) = SNICE(J)
         MLIQ(J) = SNLIQ(J)
    END DO

    DO J = ISNOW+1,0           
         IMELT(J)    = 0
         HM(J)       = 0.
         XM(J)       = 0.
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)
    ENDDO
    
    DO J = ISNOW+1,0
         IF (MICE(J) > 0. .AND. STC(J) >= TFRZ) THEN  
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > 0. .AND. STC(J)  < TFRZ) THEN  
             IMELT(J) = 2
         ENDIF

    ENDDO



    DO J = ISNOW+1,0
         IF (IMELT(J) > 0) THEN
             HM(J) = (STC(J)-TFRZ)/FACT(J)
             STC(J) = TFRZ
         ENDIF

         IF (IMELT(J) == 1 .AND. HM(J) < 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         IF (IMELT(J) == 2 .AND. HM(J) > 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         XM(J) = HM(J)*DT/HFUS                           
    ENDDO



IF (OPT_GLA == 2) THEN 

    IF (ISNOW == 0 .AND. SNEQV > 0. .AND. STC(1) >= TFRZ) THEN  
        HM(1)    = (STC(1)-TFRZ)/FACT(1)             
        STC(1)   = TFRZ                              
        XM(1)    = HM(1)*DT/HFUS                     

        TEMP1  = SNEQV
        SNEQV  = MAX(0.,TEMP1-XM(1))                 
        PROPOR = SNEQV/TEMP1                         
        SNOWH  = MAX(0.,PROPOR * SNOWH)              
        HEATR(1)  = HM(1) - HFUS*(TEMP1-SNEQV)/DT    
        IF (HEATR(1) > 0.) THEN
              XM(1)  = HEATR(1)*DT/HFUS             
              STC(1) = STC(1) + FACT(1)*HEATR(1)     
        ELSE
              XM(1) = 0.                             
              HM(1) = 0.
        ENDIF
        QMELT   = MAX(0.,(TEMP1-SNEQV))/DT           
        XMF     = HFUS*QMELT                         
        PONDING = TEMP1-SNEQV                        
    ENDIF

END IF  



    DO J = ISNOW+1,0
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN

         HEATR(J) = 0.
         IF (XM(J) > 0.) THEN                            
            MICE(J) = MAX(0., WICE0(J)-XM(J))
            HEATR(J) = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.) THEN                      
            MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            HEATR(J) = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.,WMASS0(J)-MICE(J))

         IF (ABS(HEATR(J)) > 0.) THEN
            STC(J) = STC(J) + FACT(J)*HEATR(J)
            IF (MLIQ(J)*MICE(J)>0.) STC(J) = TFRZ
         ENDIF

         QMELT = QMELT + MAX(0.,(WICE0(J)-MICE(J)))/DT

      ENDIF
    ENDDO

IF (OPT_GLA == 1) THEN     

    DO J = 1, NSOIL            
         MLIQ(J) =  SH2O(J)            * DZSNSO(J) * 1000.
         MICE(J) = (SMC(J) - SH2O(J))  * DZSNSO(J) * 1000.
    END DO

    DO J = 1,NSOIL       
         IMELT(J)    = 0
         HM(J)       = 0.
         XM(J)       = 0.
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)
    ENDDO
    
    DO J = 1,NSOIL
         IF (MICE(J) > 0. .AND. STC(J) >= TFRZ) THEN  
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > 0. .AND. STC(J)  < TFRZ) THEN  
             IMELT(J) = 2
         ENDIF

         
         IF (ISNOW == 0 .AND. SNEQV > 0. .AND. J == 1) THEN
             IF (STC(J) >= TFRZ) THEN
                IMELT(J) = 1
             ENDIF
         ENDIF
    ENDDO



    DO J = 1,NSOIL
         IF (IMELT(J) > 0) THEN
             HM(J) = (STC(J)-TFRZ)/FACT(J)
             STC(J) = TFRZ
         ENDIF

         IF (IMELT(J) == 1 .AND. HM(J) < 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         IF (IMELT(J) == 2 .AND. HM(J) > 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         XM(J) = HM(J)*DT/HFUS                           
    ENDDO



    IF (ISNOW == 0 .AND. SNEQV > 0. .AND. XM(1) > 0.) THEN  
        TEMP1  = SNEQV
        SNEQV  = MAX(0.,TEMP1-XM(1))  
        PROPOR = SNEQV/TEMP1
        SNOWH  = MAX(0.,PROPOR * SNOWH)
        HEATR(1)  = HM(1) - HFUS*(TEMP1-SNEQV)/DT  
        IF (HEATR(1) > 0.) THEN
              XM(1) = HEATR(1)*DT/HFUS             
              HM(1) = HEATR(1) 
	      IMELT(1) = 1                   
        ELSE
              XM(1) = 0.
              HM(1) = 0.
	      IMELT(1) = 0                   
        ENDIF
        QMELT   = MAX(0.,(TEMP1-SNEQV))/DT
        XMF     = HFUS*QMELT
        PONDING = TEMP1-SNEQV
    ENDIF



    DO J = 1,NSOIL
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN

         HEATR(J) = 0.
         IF (XM(J) > 0.) THEN                            
            MICE(J) = MAX(0., WICE0(J)-XM(J))
            HEATR(J) = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.) THEN                      
            MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            HEATR(J) = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.,WMASS0(J)-MICE(J))

         IF (ABS(HEATR(J)) > 0.) THEN
            STC(J) = STC(J) + FACT(J)*HEATR(J)
            IF (J <= 0) THEN                             
               IF (MLIQ(J)*MICE(J)>0.) STC(J) = TFRZ
            END IF
         ENDIF

         IF (J > 0) XMF = XMF + HFUS * (WICE0(J)-MICE(J))/DT

         IF (J < 1) THEN
            QMELT = QMELT + MAX(0.,(WICE0(J)-MICE(J)))/DT
         ENDIF
      ENDIF
    ENDDO
    HEATR = 0.0
    XM = 0.0





    IF (ANY(STC(1:4) > TFRZ) .AND. ANY(STC(1:4) < TFRZ)) THEN
      DO J = 1,NSOIL
        IF ( STC(J) > TFRZ ) THEN                                       
	  HEATR(J) = (STC(J)-TFRZ)/FACT(J)
          DO K = 1,NSOIL
	    IF (J .NE. K .AND. STC(K) < TFRZ .AND. HEATR(J) > 0.1) THEN
	      HEATR(K) = (STC(K)-TFRZ)/FACT(K)
	      IF (ABS(HEATR(K)) > HEATR(J)) THEN  
	        HEATR(K) = HEATR(K) + HEATR(J)
		STC(K) = TFRZ + HEATR(K)*FACT(K)
		HEATR(J) = 0.0
              ELSE
	        HEATR(J) = HEATR(J) + HEATR(K)
		HEATR(K) = 0.0
		STC(K) = TFRZ
              END IF
	    END IF
	  END DO
          STC(J) = TFRZ + HEATR(J)*FACT(J)
        END IF
      END DO
    END IF



    IF (ANY(STC(1:4) > TFRZ) .AND. ANY(STC(1:4) < TFRZ)) THEN
      DO J = 1,NSOIL
        IF ( STC(J) < TFRZ ) THEN                                       
	  HEATR(J) = (STC(J)-TFRZ)/FACT(J)
          DO K = 1,NSOIL
	    IF (J .NE. K .AND. STC(K) > TFRZ .AND. HEATR(J) < -0.1) THEN
	      HEATR(K) = (STC(K)-TFRZ)/FACT(K)
	      IF (HEATR(K) > ABS(HEATR(J))) THEN  
	        HEATR(K) = HEATR(K) + HEATR(J)
		STC(K) = TFRZ + HEATR(K)*FACT(K)
		HEATR(J) = 0.0
              ELSE
	        HEATR(J) = HEATR(J) + HEATR(K)
		HEATR(K) = 0.0
		STC(K) = TFRZ
              END IF
	    END IF
	  END DO
          STC(J) = TFRZ + HEATR(J)*FACT(J)
        END IF
      END DO
    END IF



    IF (ANY(STC(1:4) > TFRZ) .AND. ANY(MICE(1:4) > 0.)) THEN
      DO J = 1,NSOIL
        IF ( STC(J) > TFRZ ) THEN                                       
	  HEATR(J) = (STC(J)-TFRZ)/FACT(J)
          XM(J) = HEATR(J)*DT/HFUS                           
          DO K = 1,NSOIL
	    IF (J .NE. K .AND. MICE(K) > 0. .AND. XM(J) > 0.1) THEN
	      IF (MICE(K) > XM(J)) THEN  
	        MICE(K) = MICE(K) - XM(J)
		XMF = XMF + HFUS * XM(J)/DT
		STC(K) = TFRZ
		XM(J) = 0.0
              ELSE
	        XM(J) = XM(J) - MICE(K)
		XMF = XMF + HFUS * MICE(K)/DT
		MICE(K) = 0.0
		STC(K) = TFRZ
              END IF
              MLIQ(K) = MAX(0.,WMASS0(K)-MICE(K))
	    END IF
	  END DO
	  HEATR(J) = XM(J)*HFUS/DT
          STC(J) = TFRZ + HEATR(J)*FACT(J)
        END IF
      END DO
    END IF



    IF (ANY(STC(1:4) < TFRZ) .AND. ANY(MLIQ(1:4) > 0.)) THEN
      DO J = 1,NSOIL
        IF ( STC(J) < TFRZ ) THEN                                       
	  HEATR(J) = (STC(J)-TFRZ)/FACT(J)
          XM(J) = HEATR(J)*DT/HFUS                           
          DO K = 1,NSOIL
	    IF (J .NE. K .AND. MLIQ(K) > 0. .AND. XM(J) < -0.1) THEN
	      IF (MLIQ(K) > ABS(XM(J))) THEN  
	        MICE(K) = MICE(K) - XM(J)
		XMF = XMF + HFUS * XM(J)/DT
		STC(K) = TFRZ
		XM(J) = 0.0
              ELSE
	        XM(J) = XM(J) + MLIQ(K)
		XMF = XMF - HFUS * MLIQ(K)/DT
		MICE(K) = WMASS0(K)
		STC(K) = TFRZ
              END IF
              MLIQ(K) = MAX(0.,WMASS0(K)-MICE(K))
	    END IF
	  END DO
	  HEATR(J) = XM(J)*HFUS/DT
          STC(J) = TFRZ + HEATR(J)*FACT(J)
        END IF
      END DO
    END IF
    
END IF   

    DO J = ISNOW+1,0             
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    END DO

    DO J = 1, NSOIL              
      IF(OPT_GLA == 1) THEN 
       SH2O(J) =  MLIQ(J)            / (1000. * DZSNSO(J))
       SH2O(J) =  MAX(0.0,MIN(1.0,SH2O(J)))

      ELSEIF(OPT_GLA == 2) THEN 
       SH2O(J) = 0.0             
      END IF
      SMC(J)  = 1.0 
    END DO
   
  END SUBROUTINE PHASECHANGE_GLACIER

  SUBROUTINE WATER_GLACIER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,PRCP   ,SFCTMP , & 
                            QVAP   ,QDEW   ,FICEOLD,ZSOIL  ,                 & 
                            ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  ,STC    , & 
                            DZSNSO ,SH2O   ,SICE   ,PONDING,ZSNSO  ,FSH    , & 
                            RUNSRF ,RUNSUB ,QSNOW  ,PONDING1 ,PONDING2,QSNBOT,FPICE     &   
                            )  




  implicit none


  INTEGER,                         INTENT(IN)    :: NSNOW   
  INTEGER,                         INTENT(IN)    :: NSOIL   
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT   
  REAL,                            INTENT(IN)    :: DT      
  REAL,                            INTENT(IN)    :: PRCP    
  REAL,                            INTENT(IN)    :: SFCTMP  
  REAL,                            INTENT(INOUT)    :: QVAP    
  REAL,                            INTENT(INOUT)    :: QDEW    
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD 
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  


  INTEGER,                         INTENT(INOUT) :: ISNOW   
  REAL,                            INTENT(INOUT) :: SNOWH   
  REAL,                            INTENT(INOUT) :: SNEQV   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE   
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC     
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO  
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O    
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE    
  REAL                           , INTENT(INOUT) :: PONDING 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO   
  REAL                           , INTENT(INOUT) :: FSH     


  REAL,                            INTENT(OUT)   :: RUNSRF  
  REAL,                            INTENT(OUT)   :: RUNSUB  
  REAL,                            INTENT(OUT)   :: QSNOW   
  REAL,                            INTENT(OUT)   :: PONDING1
  REAL,                            INTENT(OUT)   :: PONDING2
  REAL,                            INTENT(OUT)   :: QSNBOT  
  REAL,                            INTENT(OUT)   :: FPICE   


  REAL                                           :: QRAIN   
  REAL                                           :: QSEVA   
  REAL                                           :: QSDEW   
  REAL                                           :: QSNFRO  
  REAL                                           :: QSNSUB  
  REAL                                           :: SNOWHIN 
  REAL                                           :: SNOFLOW 
  REAL                                           :: BDFALL  
  REAL                                           :: REPLACE 
  REAL, DIMENSION(       1:NSOIL)                :: SICE_SAVE  
  REAL, DIMENSION(       1:NSOIL)                :: SH2O_SAVE  
  INTEGER :: ILEV





   SNOFLOW         = 0.
   RUNSUB          = 0.
   RUNSRF          = 0.
   SICE_SAVE       = SICE
   SH2O_SAVE       = SH2O






     IF(OPT_SNF == 1 .OR. OPT_SNF == 4) THEN
       IF(SFCTMP > TFRZ+2.5)THEN
           FPICE = 0.
       ELSE
         IF(SFCTMP <= TFRZ+0.5)THEN
           FPICE = 1.0
         ELSE IF(SFCTMP <= TFRZ+2.)THEN
           FPICE = 1.-(-54.632 + 0.2*SFCTMP)
         ELSE
           FPICE = 0.6
         ENDIF
       ENDIF
     ENDIF

     IF(OPT_SNF == 2) THEN
       IF(SFCTMP >= TFRZ+2.2) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

     IF(OPT_SNF == 3) THEN
       IF(SFCTMP >= TFRZ) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF





     BDFALL = MIN(120.,67.92+51.25*EXP((SFCTMP-TFRZ)/2.59)) 

     QRAIN   = PRCP * (1.-FPICE)
     QSNOW   = PRCP * FPICE
     SNOWHIN = QSNOW/BDFALL




     QSNSUB = QVAP  
     QSNFRO = QDEW

     CALL SNOWWATER_GLACIER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,SFCTMP , & 
                             SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB ,QRAIN  , & 
                             FICEOLD,ZSOIL  ,                         & 
                             ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & 
                             SH2O   ,SICE   ,STC    ,DZSNSO ,ZSNSO  , & 
                             FSH    ,                                 & 
                             QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  

    
    
    RUNSRF = (PONDING+PONDING1+PONDING2)/DT

    IF(ISNOW == 0) THEN
      RUNSRF = RUNSRF + QSNBOT + QRAIN
    ELSE
      RUNSRF = RUNSRF + QSNBOT
    ENDIF

    
    IF(OPT_GLA == 1) THEN
      REPLACE = 0.0
      DO ILEV = 1,NSOIL
       REPLACE = REPLACE + DZSNSO(ILEV)*(SICE(ILEV) - SICE_SAVE(ILEV) + SH2O(ILEV) - SH2O_SAVE(ILEV))
      END DO
      REPLACE = REPLACE * 1000.0 / DT     
    
      SICE = MIN(1.0,SICE_SAVE)
    ELSEIF(OPT_GLA == 2) THEN
      SICE = 1.0
    END IF
    SH2O = 1.0 - SICE
    
    
    

    IF(OPT_GLA == 1) THEN
      RUNSUB       = SNOFLOW + REPLACE
    ELSEIF(OPT_GLA == 2) THEN
      RUNSUB       = SNOFLOW
      QVAP = QSNSUB
      QDEW = QSNFRO
    END IF

  END SUBROUTINE WATER_GLACIER


  SUBROUTINE SNOWWATER_GLACIER (NSNOW  ,NSOIL  ,IMELT  ,DT     ,SFCTMP , & 
                                SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB ,QRAIN  , & 
                                FICEOLD,ZSOIL  ,                         & 
                                ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & 
                                SH2O   ,SICE   ,STC    ,DZSNSO ,ZSNSO  , & 
				FSH    ,                                 & 
                                QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  

  IMPLICIT NONE


  INTEGER,                         INTENT(IN)    :: NSNOW  
  INTEGER,                         INTENT(IN)    :: NSOIL  
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  
  REAL,                            INTENT(IN)    :: DT     
  REAL,                            INTENT(IN)    :: SFCTMP 
  REAL,                            INTENT(IN)    :: SNOWHIN
  REAL,                            INTENT(IN)    :: QSNOW  
  REAL,                            INTENT(INOUT)    :: QSNFRO 
  REAL,                            INTENT(INOUT)    :: QSNSUB 
  REAL,                            INTENT(IN)    :: QRAIN  
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(IN)    :: FICEOLD
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  


  INTEGER,                         INTENT(INOUT) :: ISNOW  
  REAL,                            INTENT(INOUT) :: SNOWH  
  REAL,                            INTENT(INOUT) :: SNEQV  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  
  REAL                           , INTENT(INOUT) :: FSH     


  REAL,                              INTENT(OUT) :: QSNBOT 
  REAL,                              INTENT(OUT) :: SNOFLOW
  REAL,                              INTENT(OUT) :: PONDING1
  REAL,                              INTENT(OUT) :: PONDING2


  INTEGER :: IZ
  REAL    :: BDSNOW  

   SNOFLOW = 0.0
   PONDING1 = 0.0
   PONDING2 = 0.0

   CALL SNOWFALL_GLACIER (NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN, & 
                          SFCTMP ,                                 & 
                          ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE  , & 
                          SNLIQ  ,SNEQV  )                           

   IF(ISNOW < 0) THEN        
     CALL  COMPACT_GLACIER (NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & 
                            SNLIQ  ,IMELT  ,FICEOLD,                 & 
                            ISNOW  ,DZSNSO )                           

     CALL  COMBINE_GLACIER (NSNOW  ,NSOIL  ,                         & 
                            ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & 
                            DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & 
                            PONDING1       ,PONDING2)                  

     CALL   DIVIDE_GLACIER (NSNOW  ,NSOIL  ,                         & 
                            ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO )   
   END IF



   DO IZ = -NSNOW+1, ISNOW
        SNICE(IZ) = 0.
        SNLIQ(IZ) = 0.
        STC(IZ)   = 0.
        DZSNSO(IZ)= 0.
        ZSNSO(IZ) = 0.
   ENDDO

   CALL  SNOWH2O_GLACIER (NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & 
                          QRAIN  ,                                 & 
                          ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & 
                          SNLIQ  ,SH2O   ,SICE   ,STC    ,         & 
			  PONDING1       ,PONDING2       ,FSH    , & 
                          QSNBOT )                                   


       
   IF(SNEQV > 2000.) THEN   
      BDSNOW      = SNICE(0) / DZSNSO(0)
      SNOFLOW     = (SNEQV - 2000.)
      SNICE(0)    = SNICE(0)  - SNOFLOW 
      DZSNSO(0)   = DZSNSO(0) - SNOFLOW/BDSNOW
      SNOFLOW     = SNOFLOW / DT
   END IF



   IF(ISNOW /= 0) THEN
       SNEQV = 0.
       DO IZ = ISNOW+1,0
             SNEQV = SNEQV + SNICE(IZ) + SNLIQ(IZ)
       ENDDO
   END IF



   DO IZ = ISNOW+1, 0
        DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

   DZSNSO(1) = ZSOIL(1)
   DO IZ = 2,NSOIL
        DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
   END DO

   ZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)
   DO IZ = ISNOW+2 ,NSOIL
       ZSNSO(IZ) = ZSNSO(IZ-1) + DZSNSO(IZ)
   ENDDO

   DO IZ = ISNOW+1 ,NSOIL
       DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

  END SUBROUTINE SNOWWATER_GLACIER

  SUBROUTINE SNOWFALL_GLACIER (NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN , & 
                               SFCTMP ,                                  & 
                               ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE   , & 
                               SNLIQ  ,SNEQV  )                            




    IMPLICIT NONE



  INTEGER,                            INTENT(IN) :: NSOIL  
  INTEGER,                            INTENT(IN) :: NSNOW  
  REAL,                               INTENT(IN) :: DT     
  REAL,                               INTENT(IN) :: QSNOW  
  REAL,                               INTENT(IN) :: SNOWHIN
  REAL,                               INTENT(IN) :: SFCTMP 



  INTEGER,                         INTENT(INOUT) :: ISNOW  
  REAL,                            INTENT(INOUT) :: SNOWH  
  REAL,                            INTENT(INOUT) :: SNEQV  
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO 
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  



  INTEGER :: NEWNODE            

    NEWNODE  = 0



    IF(ISNOW == 0 .and. QSNOW > 0.)  THEN
      SNOWH = SNOWH + SNOWHIN * DT
      SNEQV = SNEQV + QSNOW * DT
    END IF


 
    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.05) THEN
      ISNOW    = -1
      NEWNODE  =  1
      DZSNSO(0)= SNOWH
      SNOWH    = 0.
      STC(0)   = MIN(273.16, SFCTMP)   
      SNICE(0) = SNEQV
      SNLIQ(0) = 0.
    END IF



    IF(ISNOW <  0 .AND. NEWNODE == 0 .AND. QSNOW > 0.) then
         SNICE(ISNOW+1)  = SNICE(ISNOW+1)   + QSNOW   * DT
         DZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)  + SNOWHIN * DT
    ENDIF


  END SUBROUTINE SNOWFALL_GLACIER


  SUBROUTINE COMPACT_GLACIER (NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE , & 
                              SNLIQ  ,IMELT  ,FICEOLD,                & 
                              ISNOW  ,DZSNSO )                          


  IMPLICIT NONE


   INTEGER,                         INTENT(IN)    :: NSOIL  
   INTEGER,                         INTENT(IN)    :: NSNOW  
   INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  
   REAL,                            INTENT(IN)    :: DT     
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC    
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNICE  
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNLIQ  
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD


   INTEGER,                         INTENT(INOUT) :: ISNOW  
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO 


   REAL, PARAMETER     :: C2 = 21.e-3   
   REAL, PARAMETER     :: C3 = 2.5e-6   
   REAL, PARAMETER     :: C4 = 0.04     
   REAL, PARAMETER     :: C5 = 2.0      
   REAL, PARAMETER     :: DM = 100.0    
   REAL, PARAMETER     :: ETA0 = 0.8e+6 
                                        
   REAL :: BURDEN 
   REAL :: DDZ1   
   REAL :: DDZ2   
   REAL :: DDZ3   
   REAL :: DEXPF  
   REAL :: TD     
   REAL :: PDZDTC 
   REAL :: VOID   
   REAL :: WX     
   REAL :: BI     
   REAL, DIMENSION(-NSNOW+1:0) :: FICE   

   INTEGER  :: J


    BURDEN = 0.0

    DO J = ISNOW+1, 0

        WX      = SNICE(J) + SNLIQ(J)
        FICE(J) = SNICE(J) / WX
        VOID    = 1. - (SNICE(J)/DENICE + SNLIQ(J)/DENH2O) / DZSNSO(J)

        
        IF (VOID > 0.001 .AND. SNICE(J) > 0.1) THEN
           BI = SNICE(J) / DZSNSO(J)
           TD = MAX(0.,TFRZ-STC(J))
           DEXPF = EXP(-C4*TD)

           

           DDZ1 = -C3*DEXPF

           IF (BI > DM) DDZ1 = DDZ1*EXP(-46.0E-3*(BI-DM))

           

           IF (SNLIQ(J) > 0.01*DZSNSO(J)) DDZ1=DDZ1*C5

           

           DDZ2 = -(BURDEN+0.5*WX)*EXP(-0.08*TD-C2*BI)/ETA0 

           

           IF (IMELT(J) == 1) THEN
              DDZ3 = MAX(0.,(FICEOLD(J) - FICE(J))/MAX(1.E-6,FICEOLD(J)))
              DDZ3 = - DDZ3/DT           
           ELSE
              DDZ3 = 0.
           END IF

           

           PDZDTC = (DDZ1 + DDZ2 + DDZ3)*DT
           PDZDTC = MAX(-0.5,PDZDTC)

           

           DZSNSO(J) = DZSNSO(J)*(1.+PDZDTC)
        END IF

        

        BURDEN = BURDEN + WX

    END DO

  END SUBROUTINE COMPACT_GLACIER

  SUBROUTINE COMBINE_GLACIER (NSNOW  ,NSOIL  ,                         & 
                              ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & 
                              DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & 
                              PONDING1       ,PONDING2)                  

    IMPLICIT NONE



    INTEGER, INTENT(IN)     :: NSNOW                        
    INTEGER, INTENT(IN)     :: NSOIL                        



    INTEGER,                         INTENT(INOUT) :: ISNOW 
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O  
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE  
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE 
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO
    REAL,                            INTENT(INOUT) :: SNEQV 
    REAL,                            INTENT(INOUT) :: SNOWH 
    REAL,                            INTENT(INOUT) :: PONDING1
    REAL,                            INTENT(INOUT) :: PONDING2



    INTEGER :: I,J,K,L               
    INTEGER :: ISNOW_OLD             
    INTEGER :: MSSI                  
    INTEGER :: NEIBOR                
    REAL    :: ZWICE                 
    REAL    :: ZWLIQ                 
    REAL    :: DZMIN(3)              
    DATA DZMIN /0.045, 0.05, 0.2/



       ISNOW_OLD = ISNOW

       DO J = ISNOW_OLD+1,0
          IF (SNICE(J) <= .1) THEN
             IF(J /= 0) THEN
                SNLIQ(J+1) = SNLIQ(J+1) + SNLIQ(J)
                SNICE(J+1) = SNICE(J+1) + SNICE(J)
             ELSE
               IF (ISNOW_OLD < -1) THEN
                SNLIQ(J-1) = SNLIQ(J-1) + SNLIQ(J)
                SNICE(J-1) = SNICE(J-1) + SNICE(J)
               ELSE
                PONDING1 = PONDING1 + SNLIQ(J)       
                SNEQV = SNICE(J)                     
                SNOWH = DZSNSO(J)                    
                SNLIQ(J) = 0.0                       
                SNICE(J) = 0.0                       
                DZSNSO(J) = 0.0
               ENDIF


             ENDIF

             
             IF (J > ISNOW+1 .AND. ISNOW < -1) THEN
                DO I = J, ISNOW+2, -1
                   STC(I)   = STC(I-1)
                   SNLIQ(I) = SNLIQ(I-1)
                   SNICE(I) = SNICE(I-1)
                   DZSNSO(I)= DZSNSO(I-1)
                END DO
             END IF
             ISNOW = ISNOW + 1
          END IF
       END DO



       IF(SICE(1) < 0.) THEN
          SH2O(1) = SH2O(1) + SICE(1)
          SICE(1) = 0.
       END IF

       IF(ISNOW ==0) RETURN   

       SNEQV  = 0.
       SNOWH  = 0.
       ZWICE  = 0.
       ZWLIQ  = 0.

       DO J = ISNOW+1,0
             SNEQV = SNEQV + SNICE(J) + SNLIQ(J)
             SNOWH = SNOWH + DZSNSO(J)
             ZWICE = ZWICE + SNICE(J)
             ZWLIQ = ZWLIQ + SNLIQ(J)
       END DO





       IF (SNOWH < 0.05 .AND. ISNOW < 0 ) THEN
          ISNOW  = 0
          SNEQV = ZWICE
          PONDING2 = PONDING2 + ZWLIQ           
          IF(SNEQV <= 0.) SNOWH = 0.            
       END IF










       IF (ISNOW < -1) THEN

          ISNOW_OLD = ISNOW
          MSSI     = 1

          DO I = ISNOW_OLD+1,0
             IF (DZSNSO(I) < DZMIN(MSSI)) THEN

                IF (I == ISNOW+1) THEN
                   NEIBOR = I + 1
                ELSE IF (I == 0) THEN
                   NEIBOR = I - 1
                ELSE
                   NEIBOR = I + 1
                   IF ((DZSNSO(I-1)+DZSNSO(I)) < (DZSNSO(I+1)+DZSNSO(I))) NEIBOR = I-1
                END IF

                
                IF (NEIBOR > I) THEN
                   J = NEIBOR
                   L = I
                ELSE
                   J = I
                   L = NEIBOR
                END IF

                CALL COMBO_GLACIER (DZSNSO(J), SNLIQ(J), SNICE(J), &
                   STC(J), DZSNSO(L), SNLIQ(L), SNICE(L), STC(L) )

                
                IF (J-1 > ISNOW+1) THEN
                   DO K = J-1, ISNOW+2, -1
                      STC(K)   = STC(K-1)
                      SNICE(K) = SNICE(K-1)
                      SNLIQ(K) = SNLIQ(K-1)
                      DZSNSO(K) = DZSNSO(K-1)
                   END DO
                END IF

                
                ISNOW = ISNOW + 1
                IF (ISNOW >= -1) EXIT
             ELSE

                
                MSSI = MSSI + 1

             END IF
          END DO

       END IF

  END SUBROUTINE COMBINE_GLACIER



  SUBROUTINE COMBO_GLACIER(DZ,  WLIQ,  WICE, T, DZ2, WLIQ2, WICE2, T2)

    IMPLICIT NONE





    REAL, INTENT(IN)    :: DZ2   
    REAL, INTENT(IN)    :: WLIQ2 
    REAL, INTENT(IN)    :: WICE2 
    REAL, INTENT(IN)    :: T2    
    REAL, INTENT(INOUT) :: DZ    
    REAL, INTENT(INOUT) :: WLIQ  
    REAL, INTENT(INOUT) :: WICE  
    REAL, INTENT(INOUT) :: T     



    REAL                :: DZC   
    REAL                :: WLIQC 
    REAL                :: WICEC 
    REAL                :: TC    
    REAL                :: H     
    REAL                :: H2    
    REAL                :: HC    



    DZC = DZ+DZ2
    WICEC = (WICE+WICE2)
    WLIQC = (WLIQ+WLIQ2)
    H = (CICE*WICE+CWAT*WLIQ) * (T-TFRZ)+HFUS*WLIQ
    H2= (CICE*WICE2+CWAT*WLIQ2) * (T2-TFRZ)+HFUS*WLIQ2

    HC = H + H2
    IF(HC < 0.)THEN
       TC = TFRZ + HC/(CICE*WICEC + CWAT*WLIQC)
    ELSE IF (HC.LE.HFUS*WLIQC) THEN
       TC = TFRZ
    ELSE
       TC = TFRZ + (HC - HFUS*WLIQC) / (CICE*WICEC + CWAT*WLIQC)
    END IF

    DZ = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T = TC

  END SUBROUTINE COMBO_GLACIER

  SUBROUTINE DIVIDE_GLACIER (NSNOW  ,NSOIL  ,                         & 
                             ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO  )  

    IMPLICIT NONE



    INTEGER, INTENT(IN)                            :: NSNOW 
    INTEGER, INTENT(IN)                            :: NSOIL 



    INTEGER                        , INTENT(INOUT) :: ISNOW 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE 
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO



    INTEGER                                        :: J     
    INTEGER                                        :: MSNO  
    REAL                                           :: DRR   
    REAL, DIMENSION(       1:NSNOW)                :: DZ    
    REAL, DIMENSION(       1:NSNOW)                :: SWICE 
    REAL, DIMENSION(       1:NSNOW)                :: SWLIQ 
    REAL, DIMENSION(       1:NSNOW)                :: TSNO  
    REAL                                           :: ZWICE 
    REAL                                           :: ZWLIQ 
    REAL                                           :: PROPOR
    REAL                                           :: DTDZ  


    DO J = 1,NSNOW
          IF (J <= ABS(ISNOW)) THEN
             DZ(J)    = DZSNSO(J+ISNOW)
             SWICE(J) = SNICE(J+ISNOW)
             SWLIQ(J) = SNLIQ(J+ISNOW)
             TSNO(J)  = STC(J+ISNOW)
          END IF
    END DO

       MSNO = ABS(ISNOW)

       IF (MSNO == 1) THEN
          
          IF (DZ(1) > 0.05) THEN
             MSNO = 2
             DZ(1)    = DZ(1)/2.
             SWICE(1) = SWICE(1)/2.
             SWLIQ(1) = SWLIQ(1)/2.
             DZ(2)    = DZ(1)
             SWICE(2) = SWICE(1)
             SWLIQ(2) = SWLIQ(1)
             TSNO(2)  = TSNO(1)
          END IF
       END IF

       IF (MSNO > 1) THEN
          IF (DZ(1) > 0.05) THEN
             DRR      = DZ(1) - 0.05
             PROPOR   = DRR/DZ(1)
             ZWICE    = PROPOR*SWICE(1)
             ZWLIQ    = PROPOR*SWLIQ(1)
             PROPOR   = 0.05/DZ(1)
             SWICE(1) = PROPOR*SWICE(1)
             SWLIQ(1) = PROPOR*SWLIQ(1)
             DZ(1)    = 0.05

             CALL COMBO_GLACIER (DZ(2), SWLIQ(2), SWICE(2), TSNO(2), DRR, &
                  ZWLIQ, ZWICE, TSNO(1))

             

             IF (MSNO <= 2 .AND. DZ(2) > 0.10) THEN
                MSNO = 3
                DTDZ = (TSNO(1) - TSNO(2))/((DZ(1)+DZ(2))/2.)
                DZ(2)    = DZ(2)/2.
                SWICE(2) = SWICE(2)/2.
                SWLIQ(2) = SWLIQ(2)/2.
                DZ(3)    = DZ(2)
                SWICE(3) = SWICE(2)
                SWLIQ(3) = SWLIQ(2)
                TSNO(3) = TSNO(2) - DTDZ*DZ(2)/2.
                IF (TSNO(3) >= TFRZ) THEN
                   TSNO(3)  = TSNO(2)
                ELSE
                   TSNO(2) = TSNO(2) + DTDZ*DZ(2)/2.
                ENDIF

             END IF
          END IF
       END IF

       IF (MSNO > 2) THEN
          IF (DZ(2) > 0.2) THEN
             DRR = DZ(2) - 0.2
             PROPOR   = DRR/DZ(2)
             ZWICE    = PROPOR*SWICE(2)
             ZWLIQ    = PROPOR*SWLIQ(2)
             PROPOR   = 0.2/DZ(2)
             SWICE(2) = PROPOR*SWICE(2)
             SWLIQ(2) = PROPOR*SWLIQ(2)
             DZ(2)    = 0.2
             CALL COMBO_GLACIER (DZ(3), SWLIQ(3), SWICE(3), TSNO(3), DRR, &
                  ZWLIQ, ZWICE, TSNO(2))
          END IF
       END IF

       ISNOW = -MSNO

    DO J = ISNOW+1,0
             DZSNSO(J) = DZ(J-ISNOW)
             SNICE(J) = SWICE(J-ISNOW)
             SNLIQ(J) = SWLIQ(J-ISNOW)
             STC(J)   = TSNO(J-ISNOW)
    END DO






  END SUBROUTINE DIVIDE_GLACIER

  SUBROUTINE SNOWH2O_GLACIER (NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & 
                              QRAIN  ,                                 & 
                              ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & 
                              SNLIQ  ,SH2O   ,SICE   ,STC    ,         & 
                              PONDING1       ,PONDING2       ,FSH    , & 
                              QSNBOT )                                   




   IMPLICIT NONE



   INTEGER,                         INTENT(IN)    :: NSNOW  
   INTEGER,                         INTENT(IN)    :: NSOIL  
   REAL,                            INTENT(IN)    :: DT     
   REAL,                            INTENT(INOUT)    :: QSNFRO 
   REAL,                            INTENT(INOUT)    :: QSNSUB 
   REAL,                            INTENT(IN)    :: QRAIN  



   REAL,                            INTENT(OUT)   :: QSNBOT 



   INTEGER,                         INTENT(INOUT) :: ISNOW  
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO 
   REAL,                            INTENT(INOUT) :: SNOWH  
   REAL,                            INTENT(INOUT) :: SNEQV  
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNICE  
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNLIQ  
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    
   REAL,                            INTENT(INOUT) :: PONDING1
   REAL,                            INTENT(INOUT) :: PONDING2
   REAL,                            INTENT(INOUT) :: FSH     



   INTEGER                     :: J         
   REAL                        :: QIN       
   REAL                        :: QOUT      
   REAL                        :: WGDIF     
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_LIQ   
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_ICE   
   REAL, DIMENSION(-NSNOW+1:0) :: EPORE     
   REAL :: PROPOR, TEMP




   IF(SNEQV == 0.) THEN
     IF(OPT_GLA == 1) THEN
       SICE(1) =  SICE(1) + (QSNFRO-QSNSUB)*DT/(DZSNSO(1)*1000.)
     ELSEIF(OPT_GLA == 2) THEN
       FSH = FSH - (QSNFRO-QSNSUB)*HSUB
       QSNFRO = 0.0
       QSNSUB = 0.0
     END IF
   END IF






   IF(ISNOW == 0 .and. SNEQV > 0.) THEN
      IF(OPT_GLA == 1) THEN
        TEMP   = SNEQV
        SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
        PROPOR = SNEQV/TEMP
        SNOWH  = MAX(0.,PROPOR * SNOWH)
      ELSEIF(OPT_GLA == 2) THEN
        FSH = FSH - (QSNFRO-QSNSUB)*HSUB
        QSNFRO = 0.0
        QSNSUB = 0.0
      END IF

      IF(SNEQV < 0.) THEN
         SICE(1) = SICE(1) + SNEQV/(DZSNSO(1)*1000.)
         SNEQV   = 0.
         SNOWH   = 0.
      END IF
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF

   IF(SNOWH <= 1.E-8 .OR. SNEQV <= 1.E-6) THEN
     SNOWH = 0.0
     SNEQV = 0.0
   END IF



   IF ( ISNOW < 0 ) THEN 

      WGDIF = SNICE(ISNOW+1) - QSNSUB*DT + QSNFRO*DT
      SNICE(ISNOW+1) = WGDIF
      IF (WGDIF < 1.e-6 .and. ISNOW <0) THEN
         CALL  COMBINE_GLACIER (NSNOW  ,NSOIL  ,                         & 
                                ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & 
                                DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & 
                               PONDING1, PONDING2 )                        
      ENDIF
      
      IF ( ISNOW < 0 ) THEN 
         SNLIQ(ISNOW+1) = SNLIQ(ISNOW+1) + QRAIN * DT
         SNLIQ(ISNOW+1) = MAX(0., SNLIQ(ISNOW+1))
      ENDIF
      
   ENDIF 



   

   DO J = -NSNOW+1, 0
      IF (J >= ISNOW+1) THEN
         VOL_ICE(J)      = MIN(1., SNICE(J)/(DZSNSO(J)*DENICE))
         EPORE(J)        = 1. - VOL_ICE(J)
         VOL_LIQ(J)      = MIN(EPORE(J),SNLIQ(J)/(DZSNSO(J)*DENH2O))
      END IF
   END DO

   QIN = 0.
   QOUT = 0.

   

   DO J = -NSNOW+1, 0
      IF (J >= ISNOW+1) THEN
         SNLIQ(J) = SNLIQ(J) + QIN
         IF (J <= -1) THEN
            IF (EPORE(J) < 0.05 .OR. EPORE(J+1) < 0.05) THEN
               QOUT = 0.
            ELSE
               QOUT = MAX(0.,(VOL_LIQ(J)-SSI*EPORE(J))*DZSNSO(J))
               QOUT = MIN(QOUT,(1.-VOL_ICE(J+1)-VOL_LIQ(J+1))*DZSNSO(J+1))
            END IF
         ELSE
            QOUT = MAX(0.,(VOL_LIQ(J) - SSI*EPORE(J))*DZSNSO(J))
         END IF
         QOUT = QOUT*1000.
         SNLIQ(J) = SNLIQ(J) - QOUT
         QIN = QOUT
      END IF
   END DO



   QSNBOT = QOUT / DT           

  END SUBROUTINE SNOWH2O_GLACIER


  SUBROUTINE ERROR_GLACIER (ILOC   ,JLOC   ,SWDOWN ,FSA    ,FSR    ,FIRA   , &
                            FSH    ,FGEV   ,SSOIL  ,SAG    ,PRCP   ,EDIR   , &
		            RUNSRF ,RUNSUB ,SNEQV  ,DT     ,BEG_WB )



  IMPLICIT NONE


  INTEGER                        , INTENT(IN) :: ILOC   
  INTEGER                        , INTENT(IN) :: JLOC   
  REAL                           , INTENT(IN) :: SWDOWN 
  REAL                           , INTENT(IN) :: FSA    
  REAL                           , INTENT(IN) :: FSR    
  REAL                           , INTENT(IN) :: FIRA   
  REAL                           , INTENT(IN) :: FSH    
  REAL                           , INTENT(IN) :: FGEV   
  REAL                           , INTENT(IN) :: SSOIL  
  REAL                           , INTENT(IN) :: SAG

  REAL                           , INTENT(IN) :: PRCP   
  REAL                           , INTENT(IN) :: EDIR   
  REAL                           , INTENT(IN) :: RUNSRF 
  REAL                           , INTENT(IN) :: RUNSUB 
  REAL                           , INTENT(IN) :: SNEQV  
  REAL                           , INTENT(IN) :: DT     
  REAL                           , INTENT(IN) :: BEG_WB 

  REAL                                        :: END_WB 
  REAL                                        :: ERRWAT 
  REAL                                        :: ERRENG 
  REAL                                        :: ERRSW  
  CHARACTER(len=256)                          :: message

   ERRSW   = SWDOWN - (FSA + FSR)
   IF (ERRSW > 0.01) THEN            
     WRITE(*,*) "SAG    =",SAG
     WRITE(*,*) "FSA    =",FSA
     WRITE(*,*) "FSR    =",FSR
     WRITE(message,*) 'ERRSW =',ERRSW
     call wrf_message(trim(message))
     call wrf_error_fatal3("<stdin>",2994,&
"Radiation budget problem in NOAHMP GLACIER")
   END IF

   ERRENG = SAG-(FIRA+FSH+FGEV+SSOIL)
   IF(ERRENG > 0.01) THEN
      write(message,*) 'ERRENG =',ERRENG
      call wrf_message(trim(message))
      WRITE(message,'(i6,1x,i6,1x,5F10.4)')ILOC,JLOC,SAG,FIRA,FSH,FGEV,SSOIL
      call wrf_message(trim(message))
      call wrf_error_fatal3("<stdin>",3004,&
"Energy budget problem in NOAHMP GLACIER")
   END IF

   END_WB = SNEQV
   ERRWAT = END_WB-BEG_WB-(PRCP-EDIR-RUNSRF-RUNSUB)*DT

   IF(ABS(ERRWAT) > 0.1) THEN
      if (ERRWAT > 0) then
         call wrf_message ('The model is gaining water (ERRWAT is positive)')
      else
         call wrf_message('The model is losing water (ERRWAT is negative)')
      endif
      write(message, *) 'ERRWAT =',ERRWAT, "kg m{-2} timestep{-1}"
      call wrf_message(trim(message))
      WRITE(message,'("    I      J     END_WB     BEG_WB       PRCP       EDIR      RUNSRF     RUNSUB")')
           call wrf_message(trim(message))
           WRITE(message,'(i6,1x,i6,1x,2f15.3,4f11.5)')ILOC,JLOC,END_WB,BEG_WB,PRCP*DT,&
                EDIR*DT,RUNSRF*DT,RUNSUB*DT
           call wrf_message(trim(message))
           call wrf_error_fatal3("<stdin>",3024,&
"Water budget problem in NOAHMP GLACIER")
        END IF

 END SUBROUTINE ERROR_GLACIER


  SUBROUTINE NOAHMP_OPTIONS_GLACIER(iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc, iopt_gla )

  IMPLICIT NONE

  INTEGER,  INTENT(IN) :: iopt_alb  
  INTEGER,  INTENT(IN) :: iopt_snf  
  INTEGER,  INTENT(IN) :: iopt_tbot 
  INTEGER,  INTENT(IN) :: iopt_stc  
                                    
  INTEGER,  INTENT(IN) :: IOPT_GLA  



  opt_alb  = iopt_alb  
  opt_snf  = iopt_snf  
  opt_tbot = iopt_tbot 
  opt_stc  = iopt_stc
  opt_gla  = iopt_gla
  
  end subroutine noahmp_options_glacier
 
END MODULE NOAHMP_GLACIER_ROUTINES


MODULE MODULE_SF_NOAHMP_GLACIER

  USE NOAHMP_GLACIER_ROUTINES
  USE NOAHMP_GLACIER_GLOBALS

END MODULE MODULE_SF_NOAHMP_GLACIER
