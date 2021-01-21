MODULE module_sf_noahmpdrv





CONTAINS

  SUBROUTINE noahmplsm(ITIMESTEP,        YR,   JULIAN,   COSZIN,XLAT,XLONG, & 
                  DZ8W,       DT,       DZS,    NSOIL,       DX,            & 
	        IVGTYP,   ISLTYP,    VEGFRA,   VEGMAX,      TMN,            & 
		 XLAND,     XICE,XICE_THRES,  CROPCAT,                      & 
	       PLANTING,  HARVEST,SEASON_GDD,                               &
                 IDVEG, IOPT_CRS,  IOPT_BTR, IOPT_RUN, IOPT_SFC, IOPT_FRZ,  & 
              IOPT_INF, IOPT_RAD,  IOPT_ALB, IOPT_SNF,IOPT_TBOT, IOPT_STC,  & 
              IOPT_GLA, IOPT_RSF, IOPT_SOIL,IOPT_PEDO,IOPT_CROP,            & 
              IZ0TLND, SF_URBAN_PHYSICS,                                    & 
	      SOILCOMP,  SOILCL1,  SOILCL2,   SOILCL3,  SOILCL4,            & 
                   T3D,     QV3D,     U_PHY,    V_PHY,   SWDOWN,     SWDDIR,&
                SWDDIF,      GLW,                                           & 
		 P8W3D,PRECIP_IN,        SR,                                & 
                   TSK,      HFX,      QFX,        LH,   GRDFLX,    SMSTAV, & 
                SMSTOT,SFCRUNOFF, UDRUNOFF,    ALBEDO,    SNOWC,     SMOIS, & 
		  SH2O,     TSLB,     SNOW,     SNOWH,   CANWAT,    ACSNOM, & 
		ACSNOW,    EMISS,     QSFC,                                 & 
 		    Z0,      ZNT,                                           & 
               ISNOWXY,     TVXY,     TGXY,  CANICEXY, CANLIQXY,     EAHXY, & 
	         TAHXY,     CMXY,     CHXY,    FWETXY, SNEQVOXY,  ALBOLDXY, & 
               QSNOWXY, WSLAKEXY,    ZWTXY,      WAXY,     WTXY,    TSNOXY, & 
	       ZSNSOXY,  SNICEXY,  SNLIQXY,  LFMASSXY, RTMASSXY,  STMASSXY, & 
	        WOODXY, STBLCPXY, FASTCPXY,    XLAIXY,   XSAIXY,   TAUSSXY, & 
	       SMOISEQ, SMCWTDXY,DEEPRECHXY,   RECHXY,  GRAINXY,    GDDXY,PGSXY,  & 
               GECROS_STATE,                                                & 
	        T2MVXY,   T2MBXY,    Q2MVXY,   Q2MBXY,                      & 
	        TRADXY,    NEEXY,    GPPXY,     NPPXY,   FVEGXY,   RUNSFXY, & 
	       RUNSBXY,   ECANXY,   EDIRXY,   ETRANXY,    FSAXY,    FIRAXY, & 
	        APARXY,    PSNXY,    SAVXY,     SAGXY,  RSSUNXY,   RSSHAXY, & 
		BGAPXY,   WGAPXY,    TGVXY,     TGBXY,    CHVXY,     CHBXY, & 
		 SHGXY,    SHCXY,    SHBXY,     EVGXY,    EVBXY,     GHVXY, & 
		 GHBXY,    IRGXY,    IRCXY,     IRBXY,     TRXY,     EVCXY, & 
              CHLEAFXY,   CHUCXY,   CHV2XY,    CHB2XY, RS,                  & 



               ids,ide,  jds,jde,  kds,kde,                    &
               ims,ime,  jms,jme,  kms,kme,                    &
               its,ite,  jts,jte,  kts,kte,                    &
               MP_RAINC, MP_RAINNC, MP_SHCV, MP_SNOW, MP_GRAUP, MP_HAIL     )

    USE MODULE_SF_NOAHMPLSM

    USE module_sf_noahmp_glacier
    USE NOAHMP_TABLES, ONLY: ISICE_TABLE, CO2_TABLE, O2_TABLE, DEFAULT_CROP_TABLE, ISCROP_TABLE, ISURBAN_TABLE, NATURAL_TABLE, &
                             LCZ_1_TABLE,LCZ_2_TABLE,LCZ_3_TABLE,LCZ_4_TABLE,LCZ_5_TABLE,LCZ_6_TABLE,LCZ_7_TABLE,LCZ_8_TABLE,  &
                             LCZ_9_TABLE,LCZ_10_TABLE,LCZ_11_TABLE
    USE module_sf_urban,    only: IRI_SCHEME
    USE module_ra_gfdleta,  only: cal_mon_day

    IMPLICIT NONE




    INTEGER,                                         INTENT(IN   ) ::  ITIMESTEP 
    INTEGER,                                         INTENT(IN   ) ::  YR        
    REAL,                                            INTENT(IN   ) ::  JULIAN    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  COSZIN    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAT      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLONG     
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  DZ8W      
    REAL,                                            INTENT(IN   ) ::  DT        
    REAL,    DIMENSION(1:nsoil),                     INTENT(IN   ) ::  DZS       
    INTEGER,                                         INTENT(IN   ) ::  NSOIL     
    REAL,                                            INTENT(IN   ) ::  DX        
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  IVGTYP    
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  ISLTYP    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  VEGFRA    
    REAL,    DIMENSION( ims:ime ,         jms:jme ), INTENT(IN   ) ::  VEGMAX    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  TMN       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAND     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XICE      
    REAL,                                            INTENT(IN   ) ::  XICE_THRES
    INTEGER,                                         INTENT(IN   ) ::  IDVEG     
    INTEGER,                                         INTENT(IN   ) ::  IOPT_CRS  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_BTR  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_RUN  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_SFC  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_FRZ  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_INF  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_RAD  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_ALB  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_SNF  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_TBOT 
    INTEGER,                                         INTENT(IN   ) ::  IOPT_STC  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_GLA  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_RSF  
    INTEGER,                                         INTENT(IN   ) ::  IOPT_SOIL 
    INTEGER,                                         INTENT(IN   ) ::  IOPT_PEDO 
    INTEGER,                                         INTENT(IN   ) ::  IOPT_CROP 
    INTEGER,                                         INTENT(IN   ) ::  IZ0TLND   
    INTEGER,                                         INTENT(IN   ) ::  sf_urban_physics   
    REAL,    DIMENSION( ims:ime,       8, jms:jme ), INTENT(IN   ) ::  SOILCOMP  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SOILCL1   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SOILCL2   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SOILCL3   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SOILCL4   
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  T3D       
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  QV3D      
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  U_PHY     
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  V_PHY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDOWN    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDDIF    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDDIR    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  GLW       
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  P8W3D     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  PRECIP_IN 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SR        


    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_RAINC  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_RAINNC 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_SHCV   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_SNOW   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_GRAUP  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ), OPTIONAL ::  MP_HAIL   


    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  CROPCAT   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  PLANTING  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  HARVEST   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SEASON_GDD
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GRAINXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GDDXY     
 INTEGER,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  PGSXY


    REAL,    DIMENSION( ims:ime,       60,jms:jme ), INTENT(INOUT) :: gecros_state 
















    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TSK       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  HFX       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QFX       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LH        
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GRDFLX    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMSTAV    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMSTOT    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SFCRUNOFF 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  UDRUNOFF  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBEDO    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOWC     
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SMOIS     
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SH2O      
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  TSLB      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOW      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOWH     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANWAT    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ACSNOM    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ACSNOW    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EMISS     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSFC      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  Z0        
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ZNT       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  RS        

    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ISNOWXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TVXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TGXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANICEXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CANLIQXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EAHXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TAHXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CMXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  CHXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  FWETXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNEQVOXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBOLDXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSNOWXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WSLAKEXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ZWTXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WAXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WTXY      
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  TSNOXY    
    REAL,    DIMENSION( ims:ime,-2:NSOIL, jms:jme ), INTENT(INOUT) ::  ZSNSOXY   
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  SNICEXY   
    REAL,    DIMENSION( ims:ime,-2:0,     jms:jme ), INTENT(INOUT) ::  SNLIQXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LFMASSXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  RTMASSXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  STMASSXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  WOODXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  STBLCPXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  FASTCPXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  XLAIXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  XSAIXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TAUSSXY   
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SMOISEQ   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SMCWTDXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  DEEPRECHXY 
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  RECHXY    



    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  T2MVXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  T2MBXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  Q2MVXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  Q2MBXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TRADXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  NEEXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GPPXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  NPPXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FVEGXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RUNSFXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RUNSBXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  ECANXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EDIRXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  ETRANXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FSAXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  FIRAXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  APARXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  PSNXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SAVXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SAGXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RSSUNXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  RSSHAXY   
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  BGAPXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  WGAPXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TGVXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TGBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHVXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHGXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHCXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  SHBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVGXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GHVXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  GHBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRGXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRCXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  IRBXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  TRXY      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  EVCXY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHLEAFXY  
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHUCXY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHV2XY    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(OUT  ) ::  CHB2XY    
    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &  
         &                           ims,ime, jms,jme, kms,kme,  &  
         &                           its,ite, jts,jte, kts,kte      






    REAL                                :: COSZ         
    REAL                                :: LAT          
    REAL                                :: Z_ML         
    INTEGER                             :: VEGTYP       
    INTEGER,    DIMENSION(NSOIL)        :: SOILTYP      
    INTEGER                             :: CROPTYPE     
    REAL                                :: FVEG         
    REAL                                :: FVGMAX       
    REAL                                :: TBOT         
    REAL                                :: T_ML         
    REAL                                :: Q_ML         
    REAL                                :: U_ML         
    REAL                                :: V_ML         
    REAL                                :: SWDN         
    REAL                                :: LWDN         
    REAL                                :: P_ML         
    REAL                                :: PSFC         
    REAL                                :: PRCP         
    REAL                                :: PRCPCONV     
    REAL                                :: PRCPNONC     
    REAL                                :: PRCPSHCV     
    REAL                                :: PRCPSNOW     
    REAL                                :: PRCPGRPL     
    REAL                                :: PRCPHAIL     
    REAL                                :: PRCPOTHR     



    REAL                                :: FSH          
    REAL                                :: SSOIL        
    REAL                                :: SALB         
    REAL                                :: FSNO         
    REAL,   DIMENSION( 1:NSOIL)         :: SMCEQ        
    REAL,   DIMENSION( 1:NSOIL)         :: SMC          
    REAL,   DIMENSION( 1:NSOIL)         :: SMH2O        
    REAL,   DIMENSION(-2:NSOIL)         :: STC          
    REAL                                :: SWE          
    REAL                                :: SNDPTH       
    REAL                                :: EMISSI       
    REAL                                :: QSFC1D       



    INTEGER                             :: ISNOW        
    REAL                                :: TV           
    REAL                                :: TG           
    REAL                                :: CANICE       
    REAL                                :: CANLIQ       
    REAL                                :: EAH          
    REAL                                :: TAH          
    REAL                                :: CM           
    REAL                                :: CH           
    REAL                                :: FWET         
    REAL                                :: SNEQVO       
    REAL                                :: ALBOLD       
    REAL                                :: QSNOW        
    REAL                                :: WSLAKE       
    REAL                                :: ZWT          
    REAL                                :: WA           
    REAL                                :: WT           
    REAL                                :: SMCWTD       
    REAL                                :: DEEPRECH     
    REAL                                :: RECH         
    REAL, DIMENSION(-2:NSOIL)           :: ZSNSO        
    REAL, DIMENSION(-2:              0) :: SNICE        
    REAL, DIMENSION(-2:              0) :: SNLIQ        
    REAL                                :: LFMASS       
    REAL                                :: RTMASS       
    REAL                                :: STMASS       
    REAL                                :: WOOD         
    REAL                                :: GRAIN        
    REAL                                :: GDD          
    INTEGER                             :: PGS          
    REAL                                :: STBLCP       
    REAL                                :: FASTCP       
    REAL                                :: PLAI         
    REAL                                :: PSAI         
    REAL                                :: TAUSS        



    REAL                                :: Z0WRF        
    REAL                                :: T2MV         
    REAL                                :: T2MB         
    REAL                                :: Q2MV         
    REAL                                :: Q2MB         
    REAL                                :: TRAD         
    REAL                                :: NEE          
    REAL                                :: GPP          
    REAL                                :: NPP          
    REAL                                :: FVEGMP       
    REAL                                :: RUNSF        
    REAL                                :: RUNSB        
    REAL                                :: ECAN         
    REAL                                :: ETRAN        
    REAL                                :: ESOIL        
    REAL                                :: FSA          
    REAL                                :: FIRA         
    REAL                                :: APAR         
    REAL                                :: PSN          
    REAL                                :: SAV          
    REAL                                :: SAG          
    REAL                                :: RSSUN        
    REAL                                :: RSSHA        
    REAL, DIMENSION(1:2)                :: ALBSND       
    REAL, DIMENSION(1:2)                :: ALBSNI       
    REAL                                :: RB           
    REAL                                :: LAISUN       
    REAL                                :: LAISHA       
    REAL                                :: BGAP         
    REAL                                :: WGAP         
    REAL                                :: TGV          
    REAL                                :: TGB          
    REAL                                :: CHV          
    REAL                                :: CHB          
    REAL                                :: IRC          
    REAL                                :: IRG          
    REAL                                :: SHC          
    REAL                                :: SHG          
    REAL                                :: EVG          
    REAL                                :: GHV          
    REAL                                :: IRB          
    REAL                                :: SHB          
    REAL                                :: EVB          
    REAL                                :: GHB          
    REAL                                :: TR           
    REAL                                :: EVC          
    REAL                                :: CHLEAF       
    REAL                                :: CHUC         
    REAL                                :: CHV2         
    REAL                                :: CHB2         
  REAL   :: PAHV    
  REAL   :: PAHG    
  REAL   :: PAHB    
  REAL   :: PAH     



    REAL                                :: FPICE        
    REAL                                :: FCEV         
    REAL                                :: FGEV         
    REAL                                :: FCTR         
    REAL                                :: QSNBOT       
    REAL                                :: PONDING      
    REAL                                :: PONDING1     
    REAL                                :: PONDING2     



    REAL, DIMENSION(1:60)               :: gecros1d     
    REAL                                :: gecros_dd ,gecros_tbem,gecros_emb ,gecros_ema, &
                                           gecros_ds1,gecros_ds2 ,gecros_ds1x,gecros_ds2x 

    REAL                                :: FSR          
    REAL, DIMENSION(-2:0)               :: FICEOLD      
    REAL                                :: CO2PP        
    REAL                                :: O2PP         
    REAL, DIMENSION(1:NSOIL)            :: ZSOIL        
    REAL                                :: FOLN         

    REAL                                :: QC           
    REAL                                :: PBLH         
    REAL                                :: DZ8W1D       

    INTEGER                             :: I
    INTEGER                             :: J
    INTEGER                             :: K
    INTEGER                             :: ICE
    INTEGER                             :: SLOPETYP
    LOGICAL                             :: IPRINT

    INTEGER                             :: SOILCOLOR          
    INTEGER                             :: IST          
    INTEGER                             :: YEARLEN
    REAL                                :: SOLAR_TIME
    INTEGER                             :: JMONTH, JDAY

    INTEGER, PARAMETER                  :: NSNOW = 3    
    REAL, PARAMETER                     :: undefined_value = -1.E36
    
    REAL, DIMENSION( 1:nsoil ) :: SAND
    REAL, DIMENSION( 1:nsoil ) :: CLAY
    REAL, DIMENSION( 1:nsoil ) :: ORGM
    
    type(noahmp_parameters) :: parameters




    CALL NOAHMP_OPTIONS(IDVEG  ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN  ,IOPT_SFC  ,IOPT_FRZ , &
                     IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC , &
		     IOPT_RSF  ,IOPT_SOIL ,IOPT_PEDO ,IOPT_CROP )

    IPRINT    =  .false.                     

    YEARLEN = 365                            
    if (mod(YR,4) == 0) then
       YEARLEN = 366
       if (mod(YR,100) == 0) then
          YEARLEN = 365
          if (mod(YR,400) == 0) then
             YEARLEN = 366
          endif
       endif
    endif

    ZSOIL(1) = -DZS(1)                    
    DO K = 2, NSOIL
       ZSOIL(K) = -DZS(K) + ZSOIL(K-1)
    END DO

    JLOOP : DO J=jts,jte

       IF(ITIMESTEP == 1)THEN
          DO I=its,ite
             IF((XLAND(I,J)-1.5) >= 0.) THEN    
                IF(XICE(I,J) == 1. .AND. IPRINT) PRINT *,' sea-ice at water point, I=',I,'J=',J
                SMSTAV(I,J) = 1.0
                SMSTOT(I,J) = 1.0
                DO K = 1, NSOIL
                   SMOIS(I,K,J) = 1.0
                    TSLB(I,K,J) = 273.16
                ENDDO
             ELSE
                IF(XICE(I,J) == 1.) THEN        
                   SMSTAV(I,J) = 1.0
                   SMSTOT(I,J) = 1.0
                   DO K = 1, NSOIL
                      SMOIS(I,K,J) = 1.0
                   ENDDO
                ENDIF
             ENDIF
          ENDDO
       ENDIF                                                               



   ILOOP : DO I = its, ite

    IF (XICE(I,J) >= XICE_THRES) THEN
       ICE = 1                            

       SH2O  (i,1:NSOIL,j) = 1.0
       XLAIXY(i,j)         = 0.01

       CYCLE ILOOP 

    ELSE

       IF((XLAND(I,J)-1.5) >= 0.) CYCLE ILOOP   





       COSZ   = COSZIN  (I,J)                         
       LAT    = XLAT  (I,J)                           
       Z_ML   = 0.5*DZ8W(I,1,J)                       
       VEGTYP = IVGTYP(I,J)                           
       if(iopt_soil == 1) then
         SOILTYP= ISLTYP(I,J)                         
       elseif(iopt_soil == 2) then
         SOILTYP(1) = nint(SOILCL1(I,J))              
         SOILTYP(2) = nint(SOILCL2(I,J))              
         SOILTYP(3) = nint(SOILCL3(I,J))              
         SOILTYP(4) = nint(SOILCL4(I,J))              
       elseif(iopt_soil == 3) then
         SOILTYP= ISLTYP(I,J)                         
       end if 
       FVEG   = VEGFRA(I,J)/100.                      
       FVGMAX = VEGMAX (I,J)/100.                     
       TBOT = TMN(I,J)                                
       T_ML   = T3D(I,1,J)                            
       Q_ML   = QV3D(I,1,J)/(1.0+QV3D(I,1,J))         
       U_ML   = U_PHY(I,1,J)                          
       V_ML   = V_PHY(I,1,J)                          
       SWDN   = SWDOWN(I,J)                           
       LWDN   = GLW(I,J)                              
       P_ML   =(P8W3D(I,KTS+1,J)+P8W3D(I,KTS,J))*0.5  
	                                              
       PSFC   = P8W3D(I,1,J)                          
       PRCP   = PRECIP_IN (I,J) / DT                  

       CROPTYPE = 0
       IF (IOPT_CROP > 0 .AND. VEGTYP == ISCROP_TABLE) CROPTYPE = DEFAULT_CROP_TABLE 
       IF (IOPT_CROP > 0 .AND. CROPCAT(I,J) > 0) THEN
         CROPTYPE = CROPCAT(I,J)                      
	 VEGTYP = ISCROP_TABLE
         FVGMAX = 0.95
	 FVEG   = 0.95
       END IF

       IF (PRESENT(MP_RAINC) .AND. PRESENT(MP_RAINNC) .AND. PRESENT(MP_SHCV) .AND. &
           PRESENT(MP_SNOW)  .AND. PRESENT(MP_GRAUP)  .AND. PRESENT(MP_HAIL)   ) THEN

         PRCPCONV  = MP_RAINC (I,J)/DT                
         PRCPNONC  = MP_RAINNC(I,J)/DT                
         PRCPSHCV  = MP_SHCV(I,J)  /DT                
         PRCPSNOW  = MP_SNOW(I,J)  /DT                
         PRCPGRPL  = MP_GRAUP(I,J) /DT                
         PRCPHAIL  = MP_HAIL(I,J)  /DT                

         PRCPOTHR  = PRCP - PRCPCONV - PRCPNONC - PRCPSHCV 
	 PRCPOTHR  = MAX(0.0,PRCPOTHR)
	 PRCPNONC  = PRCPNONC + PRCPOTHR
         PRCPSNOW  = PRCPSNOW + SR(I,J)  * PRCPOTHR 
       ELSE
         PRCPCONV  = 0.
         PRCPNONC  = PRCP
         PRCPSHCV  = 0.
         PRCPSNOW  = SR(I,J) * PRCP
         PRCPGRPL  = 0.
         PRCPHAIL  = 0.
       ENDIF



       ISNOW                 = ISNOWXY (I,J)                
       SMC  (      1:NSOIL)  = SMOIS   (I,      1:NSOIL,J)  
       SMH2O(      1:NSOIL)  = SH2O    (I,      1:NSOIL,J)  
       STC  (-NSNOW+1:    0) = TSNOXY  (I,-NSNOW+1:    0,J) 
       STC  (      1:NSOIL)  = TSLB    (I,      1:NSOIL,J)  
       SWE                   = SNOW    (I,J)                
       SNDPTH                = SNOWH   (I,J)                
       QSFC1D                = QSFC    (I,J)



       TV                    = TVXY    (I,J)                
       TG                    = TGXY    (I,J)                
       CANLIQ                = CANLIQXY(I,J)                
       CANICE                = CANICEXY(I,J)                
       EAH                   = EAHXY   (I,J)                
       TAH                   = TAHXY   (I,J)                
       CM                    = CMXY    (I,J)                
       CH                    = CHXY    (I,J)                
       FWET                  = FWETXY  (I,J)                
       SNEQVO                = SNEQVOXY(I,J)                
       ALBOLD                = ALBOLDXY(I,J)                
       QSNOW                 = QSNOWXY (I,J)                
       WSLAKE                = WSLAKEXY(I,J)                
       ZWT                   = ZWTXY   (I,J)                
       WA                    = WAXY    (I,J)                
       WT                    = WTXY    (I,J)                
       ZSNSO(-NSNOW+1:NSOIL) = ZSNSOXY (I,-NSNOW+1:NSOIL,J) 
       SNICE(-NSNOW+1:    0) = SNICEXY (I,-NSNOW+1:    0,J) 
       SNLIQ(-NSNOW+1:    0) = SNLIQXY (I,-NSNOW+1:    0,J) 
       LFMASS                = LFMASSXY(I,J)                
       RTMASS                = RTMASSXY(I,J)                
       STMASS                = STMASSXY(I,J)                
       WOOD                  = WOODXY  (I,J)                
       STBLCP                = STBLCPXY(I,J)                
       FASTCP                = FASTCPXY(I,J)                
       PLAI                  = XLAIXY  (I,J)                
       PSAI                  = XSAIXY  (I,J)                
       TAUSS                 = TAUSSXY (I,J)                
       SMCEQ(       1:NSOIL) = SMOISEQ (I,       1:NSOIL,J)
       SMCWTD                = SMCWTDXY(I,J)
       RECH                  = 0.
       DEEPRECH              = 0.  

       if(iopt_crop == 2) then   

         gecros1d(1:60)      = gecros_state(I,1:60,J)       
         
         if(croptype == 1) then
           gecros_dd   =  2.5
           gecros_tbem =  2.0
           gecros_emb  = 10.2 
           gecros_ema  = 40.0
           gecros_ds1  =  2.1 
           gecros_ds2  =  2.0 
           gecros_ds1x =  0.0
           gecros_ds2x = 10.0
         end if
           
         if(croptype == 2) then
           gecros_dd   =  5.0
           gecros_tbem =  8.0
           gecros_emb  = 15.0 
           gecros_ema  =  6.0
           gecros_ds1  =  1.78  
           gecros_ds2  =  1.63  
           gecros_ds1x =  0.0
           gecros_ds2x = 14.0
         end if

       end if 

       SLOPETYP     = 1                               
       IST          = 1                               
       SOILCOLOR    = 4                               

       IF(any(SOILTYP == 14) .AND. XICE(I,J) == 0.) THEN
          IF(IPRINT) PRINT *, ' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
          IF(IPRINT) PRINT *, i,j,'RESET SOIL in surfce.F'
          SOILTYP = 7
       ENDIF
         IF( IVGTYP(I,J) == ISURBAN_TABLE    .or. IVGTYP(I,J) == LCZ_1_TABLE .or. IVGTYP(I,J) == LCZ_2_TABLE .or. &
             IVGTYP(I,J) == LCZ_3_TABLE      .or. IVGTYP(I,J) == LCZ_4_TABLE .or. IVGTYP(I,J) == LCZ_5_TABLE .or. &
             IVGTYP(I,J) == LCZ_6_TABLE      .or. IVGTYP(I,J) == LCZ_7_TABLE .or. IVGTYP(I,J) == LCZ_8_TABLE .or. &
             IVGTYP(I,J) == LCZ_9_TABLE      .or. IVGTYP(I,J) == LCZ_10_TABLE .or. IVGTYP(I,J) == LCZ_11_TABLE ) THEN


         IF(SF_URBAN_PHYSICS == 0 ) THEN
           VEGTYP = ISURBAN_TABLE
         ELSE
           VEGTYP = NATURAL_TABLE  
           FVGMAX = 0.96 
         ENDIF

       ENDIF














       CALL TRANSFER_MP_PARAMETERS(VEGTYP,SOILTYP,SLOPETYP,SOILCOLOR,CROPTYPE,parameters)
       
       if(iopt_soil == 3 .and. .not. parameters%urban_flag) then
         
	sand = 0.01 * soilcomp(i,1:4,j)
	clay = 0.01 * soilcomp(i,5:8,j)
        orgm = 0.0

        if(opt_pedo == 1) call pedotransfer_sr2006(nsoil,sand,clay,orgm,parameters)

       end if

       GRAIN = GRAINXY (I,J)                
       GDD   = GDDXY (I,J)                  
       PGS   = PGSXY (I,J)                  

       if(iopt_crop == 1 .and. croptype > 0) then
         parameters%PLTDAY = PLANTING(I,J)
	 parameters%HSDAY  = HARVEST (I,J)
	 parameters%GDDS1  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS1
	 parameters%GDDS2  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS2
	 parameters%GDDS3  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS3
	 parameters%GDDS4  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS4
	 parameters%GDDS5  = SEASON_GDD(I,J) / 1770.0 * parameters%GDDS5
       end if



        IF( IVGTYP(I,J) == ISURBAN_TABLE    .or. IVGTYP(I,J) == LCZ_1_TABLE .or. IVGTYP(I,J) == LCZ_2_TABLE .or. &
               IVGTYP(I,J) == LCZ_3_TABLE      .or. IVGTYP(I,J) == LCZ_4_TABLE .or. IVGTYP(I,J) == LCZ_5_TABLE .or. &
               IVGTYP(I,J) == LCZ_6_TABLE      .or. IVGTYP(I,J) == LCZ_7_TABLE .or. IVGTYP(I,J) == LCZ_8_TABLE .or. &
               IVGTYP(I,J) == LCZ_9_TABLE      .or. IVGTYP(I,J) == LCZ_10_TABLE .or. IVGTYP(I,J) == LCZ_11_TABLE ) THEN

         IF(SF_URBAN_PHYSICS > 0 .AND. IRI_SCHEME == 1 ) THEN
	     SOLAR_TIME = (JULIAN - INT(JULIAN))*24 + XLONG(I,J)/15.0
	     IF(SOLAR_TIME < 0.) SOLAR_TIME = SOLAR_TIME + 24.
             CALL CAL_MON_DAY(INT(JULIAN),YR,JMONTH,JDAY)
             IF (SOLAR_TIME >= 21. .AND. SOLAR_TIME <= 23. .AND. JMONTH >= 5 .AND. JMONTH <= 9) THEN
                SMC(1) = max(SMC(1),parameters%SMCREF(1))
                SMC(2) = max(SMC(2),parameters%SMCREF(2))
             ENDIF
         ENDIF

       ENDIF



       FICEOLD = 0.0
       FICEOLD(ISNOW+1:0) = SNICEXY(I,ISNOW+1:0,J) &  
           /(SNICEXY(I,ISNOW+1:0,J)+SNLIQXY(I,ISNOW+1:0,J))
       CO2PP  = CO2_TABLE * P_ML                      
       O2PP   = O2_TABLE  * P_ML                      
       FOLN   = 1.0                                   
       QC     = undefined_value                       
       PBLH   = undefined_value                       
       DZ8W1D = DZ8W (I,1,J)                          

       IF(VEGTYP == 25) FVEG = 0.0                  
       IF(VEGTYP == 25) PLAI = 0.0 
       IF(VEGTYP == 26) FVEG = 0.0                  
       IF(VEGTYP == 26) PLAI = 0.0
       IF(VEGTYP == 27) FVEG = 0.0
       IF(VEGTYP == 27) PLAI = 0.0

       IF ( VEGTYP == ISICE_TABLE ) THEN
         ICE = -1                           
         CALL NOAHMP_OPTIONS_GLACIER(IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC, IOPT_GLA )
      
         TBOT = MIN(TBOT,263.15)                      
         CALL NOAHMP_GLACIER(     I,       J,    COSZ,   NSNOW,   NSOIL,      DT, & 
                               T_ML,    P_ML,    U_ML,    V_ML,    Q_ML,    SWDN, & 
                               PRCP,    LWDN,    TBOT,    Z_ML, FICEOLD,   ZSOIL, & 
                              QSNOW,  SNEQVO,  ALBOLD,      CM,      CH,   ISNOW, & 
                                SWE,     SMC,   ZSNSO,  SNDPTH,   SNICE,   SNLIQ, & 
                                 TG,     STC,   SMH2O,   TAUSS,  QSFC1D,          & 
                                FSA,     FSR,    FIRA,     FSH,    FGEV,   SSOIL, & 
                               TRAD,   ESOIL,   RUNSF,   RUNSB,     SAG,    SALB, & 
                              QSNBOT,PONDING,PONDING1,PONDING2,    T2MB,    Q2MB, & 
			      EMISSI,  FPICE,    CHB2 &                             
                              )

         FSNO   = 1.0       
         TV     = undefined_value     
         TGB    = TG 
         CANICE = undefined_value 
         CANLIQ = undefined_value 
         EAH    = undefined_value 
         TAH    = undefined_value
         FWET   = undefined_value 
         WSLAKE = undefined_value 

         WA     = undefined_value 
         WT     = undefined_value 
         LFMASS = undefined_value 
         RTMASS = undefined_value 
         STMASS = undefined_value 
         WOOD   = undefined_value 
         GRAIN  = undefined_value
         GDD    = undefined_value
         STBLCP = undefined_value 
         FASTCP = undefined_value 
         PLAI   = undefined_value 
         PSAI   = undefined_value 
         T2MV   = undefined_value 
         Q2MV   = undefined_value 
         NEE    = undefined_value 
         GPP    = undefined_value 
         NPP    = undefined_value 
         FVEGMP = 0.0 
         ECAN   = undefined_value 
         ETRAN  = undefined_value 
         APAR   = undefined_value 
         PSN    = undefined_value 
         SAV    = undefined_value 
         RSSUN  = undefined_value 
         RSSHA  = undefined_value 
         RB     = undefined_value
         LAISUN = undefined_value
         LAISHA = undefined_value
         RS(I,J)= undefined_value
         BGAP   = undefined_value 
         WGAP   = undefined_value 
         TGV    = undefined_value
         CHV    = undefined_value 
         CHB    = CH 
         IRC    = undefined_value 
         IRG    = undefined_value 
         SHC    = undefined_value 
         SHG    = undefined_value 
         EVG    = undefined_value 
         GHV    = undefined_value 
         IRB    = FIRA
         SHB    = FSH
         EVB    = FGEV
         GHB    = SSOIL
         TR     = undefined_value 
         EVC    = undefined_value 
         CHLEAF = undefined_value 
         CHUC   = undefined_value 
         CHV2   = undefined_value 
         FCEV   = undefined_value 
         FCTR   = undefined_value        
         Z0WRF  = 0.002
         QFX(I,J) = ESOIL
         LH (I,J) = FGEV


    ELSE
         ICE=0                              
         CALL NOAHMP_SFLX (parameters, &
            I       , J       , LAT     , YEARLEN , JULIAN  , COSZ    , & 
            DT      , DX      , DZ8W1D  , NSOIL   , ZSOIL   , NSNOW   , & 
            FVEG    , FVGMAX  , VEGTYP  , ICE     , IST     , CROPTYPE, & 
            SMCEQ   ,                                                   & 
            T_ML    , P_ML    , PSFC    , U_ML    , V_ML    , Q_ML    , & 
            QC      , SWDN    , LWDN    ,                               & 
	    PRCPCONV, PRCPNONC, PRCPSHCV, PRCPSNOW, PRCPGRPL, PRCPHAIL, & 
            TBOT    , CO2PP   , O2PP    , FOLN    , FICEOLD , Z_ML    , & 
            ALBOLD  , SNEQVO  ,                                         & 
            STC     , SMH2O   , SMC     , TAH     , EAH     , FWET    , & 
            CANLIQ  , CANICE  , TV      , TG      , QSFC1D  , QSNOW   , & 
            ISNOW   , ZSNSO   , SNDPTH  , SWE     , SNICE   , SNLIQ   , & 
            ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & 
            STMASS  , WOOD    , STBLCP  , FASTCP  , PLAI    , PSAI    , & 
            CM      , CH      , TAUSS   ,                               & 
            GRAIN   , GDD     , PGS     ,                               & 
            SMCWTD  ,DEEPRECH , RECH    ,                               & 
            GECROS1D,                                                   & 
            Z0WRF   ,                                                   &
            FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & 
            FGEV    , FCTR    , ECAN    , ETRAN   , ESOIL   , TRAD    , & 
            TGB     , TGV     , T2MV    , T2MB    , Q2MV    , Q2MB    , & 
            RUNSF   , RUNSB   , APAR    , PSN     , SAV     , SAG     , & 
            FSNO    , NEE     , GPP     , NPP     , FVEGMP  , SALB    , & 
            QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & 
            ALBSND  , ALBSNI  ,                                         & 
            BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & 
            SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & 
	    GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & 
	    CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE   , PAHV    , & 
            PAHG    , PAHB    , PAH     , LAISUN  , LAISHA  , RB        &
            )            
                  
            QFX(I,J) = ECAN + ESOIL + ETRAN
            LH       (I,J)                = FCEV + FGEV + FCTR

   ENDIF 





             TSK      (I,J)                = TRAD
             HFX      (I,J)                = FSH
             GRDFLX   (I,J)                = SSOIL
	     SMSTAV   (I,J)                = 0.0  
             SMSTOT   (I,J)                = 0.0  
             SFCRUNOFF(I,J)                = SFCRUNOFF(I,J) + RUNSF * DT
             UDRUNOFF (I,J)                = UDRUNOFF(I,J)  + RUNSB * DT
             IF ( SALB > -999 ) THEN
                ALBEDO(I,J)                = SALB
             ENDIF
             SNOWC    (I,J)                = FSNO
             SMOIS    (I,      1:NSOIL,J)  = SMC   (      1:NSOIL)
             SH2O     (I,      1:NSOIL,J)  = SMH2O (      1:NSOIL)
             TSLB     (I,      1:NSOIL,J)  = STC   (      1:NSOIL)
             SNOW     (I,J)                = SWE
             SNOWH    (I,J)                = SNDPTH
             CANWAT   (I,J)                = CANLIQ + CANICE
             ACSNOW   (I,J)                = ACSNOW(I,J) +  PRECIP_IN(I,J) * FPICE
             ACSNOM   (I,J)                = ACSNOM(I,J) + QSNBOT*DT + PONDING + PONDING1 + PONDING2
             EMISS    (I,J)                = EMISSI
             QSFC     (I,J)                = QSFC1D

             ISNOWXY  (I,J)                = ISNOW
             TVXY     (I,J)                = TV
             TGXY     (I,J)                = TG
             CANLIQXY (I,J)                = CANLIQ
             CANICEXY (I,J)                = CANICE
             EAHXY    (I,J)                = EAH
             TAHXY    (I,J)                = TAH
             CMXY     (I,J)                = CM
             CHXY     (I,J)                = CH
             FWETXY   (I,J)                = FWET
             SNEQVOXY (I,J)                = SNEQVO
             ALBOLDXY (I,J)                = ALBOLD
             QSNOWXY  (I,J)                = QSNOW
             WSLAKEXY (I,J)                = WSLAKE
             ZWTXY    (I,J)                = ZWT
             WAXY     (I,J)                = WA
             WTXY     (I,J)                = WT
             TSNOXY   (I,-NSNOW+1:    0,J) = STC   (-NSNOW+1:    0)
             ZSNSOXY  (I,-NSNOW+1:NSOIL,J) = ZSNSO (-NSNOW+1:NSOIL)
             SNICEXY  (I,-NSNOW+1:    0,J) = SNICE (-NSNOW+1:    0)
             SNLIQXY  (I,-NSNOW+1:    0,J) = SNLIQ (-NSNOW+1:    0)
             LFMASSXY (I,J)                = LFMASS
             RTMASSXY (I,J)                = RTMASS
             STMASSXY (I,J)                = STMASS
             WOODXY   (I,J)                = WOOD
             STBLCPXY (I,J)                = STBLCP
             FASTCPXY (I,J)                = FASTCP
             XLAIXY   (I,J)                = PLAI
             XSAIXY   (I,J)                = PSAI
             TAUSSXY  (I,J)                = TAUSS



             Z0       (I,J)                = Z0WRF
             ZNT      (I,J)                = Z0WRF
             T2MVXY   (I,J)                = T2MV
             T2MBXY   (I,J)                = T2MB
             Q2MVXY   (I,J)                = Q2MV/(1.0 - Q2MV)  
             Q2MBXY   (I,J)                = Q2MB/(1.0 - Q2MB)  
             TRADXY   (I,J)                = TRAD
             NEEXY    (I,J)                = NEE
             GPPXY    (I,J)                = GPP
             NPPXY    (I,J)                = NPP
             FVEGXY   (I,J)                = FVEGMP
             RUNSFXY  (I,J)                = RUNSF
             RUNSBXY  (I,J)                = RUNSB
             ECANXY   (I,J)                = ECAN
             EDIRXY   (I,J)                = ESOIL
             ETRANXY  (I,J)                = ETRAN
             FSAXY    (I,J)                = FSA
             FIRAXY   (I,J)                = FIRA
             APARXY   (I,J)                = APAR
             PSNXY    (I,J)                = PSN
             SAVXY    (I,J)                = SAV
             SAGXY    (I,J)                = SAG
             RSSUNXY  (I,J)                = RSSUN
             RSSHAXY  (I,J)                = RSSHA
             LAISUN                        = MAX(LAISUN, 0.0)
             LAISHA                        = MAX(LAISHA, 0.0)
             RB                            = MAX(RB, 0.0)


             IF(RSSUN .le. 0.0 .or. RSSHA .le. 0.0 .or. LAISUN .eq. 0.0 .or. LAISHA .eq. 0.0) THEN
                RS    (I,J)                = 0.0
             ELSE
                RS    (I,J)                = ((1.0/(RSSUN+RB)*LAISUN) + ((1.0/(RSSHA+RB))*LAISHA))
                RS    (I,J)                = 1.0/RS(I,J) 
             ENDIF
             BGAPXY   (I,J)                = BGAP
             WGAPXY   (I,J)                = WGAP
             TGVXY    (I,J)                = TGV
             TGBXY    (I,J)                = TGB
             CHVXY    (I,J)                = CHV
             CHBXY    (I,J)                = CHB
             IRCXY    (I,J)                = IRC
             IRGXY    (I,J)                = IRG
             SHCXY    (I,J)                = SHC
             SHGXY    (I,J)                = SHG
             EVGXY    (I,J)                = EVG
             GHVXY    (I,J)                = GHV
             IRBXY    (I,J)                = IRB
             SHBXY    (I,J)                = SHB
             EVBXY    (I,J)                = EVB
             GHBXY    (I,J)                = GHB
             TRXY     (I,J)                = TR
             EVCXY    (I,J)                = EVC
             CHLEAFXY (I,J)                = CHLEAF
             CHUCXY   (I,J)                = CHUC
             CHV2XY   (I,J)                = CHV2
             CHB2XY   (I,J)                = CHB2
             RECHXY   (I,J)                = RECHXY(I,J) + RECH*1.E3 
             DEEPRECHXY(I,J)               = DEEPRECHXY(I,J) + DEEPRECH
             SMCWTDXY(I,J)                 = SMCWTD

             GRAINXY  (I,J) = GRAIN 
             GDDXY    (I,J) = GDD   
	     PGSXY    (I,J) = PGS

             if(iopt_crop == 2) then   

               
               if ((gecros1d(1) >= gecros_ds1).and.(gecros1d(42) < 0)) then   
                 if (checkIfHarvest(gecros_state, DT, gecros_ds1, gecros_ds2, gecros_ds1x, &
                     gecros_ds2x) == 1) then
           
                   call gecros_reinit(gecros1d)
                 endif
               endif                  

               gecros_state (i,1:60,j)     = gecros1d(1:60)
             end if 
 
          ENDIF                                                         

      ENDDO ILOOP                                                       
   ENDDO JLOOP                                                          


  END SUBROUTINE noahmplsm


SUBROUTINE TRANSFER_MP_PARAMETERS(VEGTYPE,SOILTYPE,SLOPETYPE,SOILCOLOR,CROPTYPE,parameters)

  USE NOAHMP_TABLES
  USE MODULE_SF_NOAHMPLSM

  implicit none

  INTEGER, INTENT(IN)    :: VEGTYPE
  INTEGER, INTENT(IN)    :: SOILTYPE(4)
  INTEGER, INTENT(IN)    :: SLOPETYPE
  INTEGER, INTENT(IN)    :: SOILCOLOR
  INTEGER, INTENT(IN)    :: CROPTYPE
    
  type (noahmp_parameters), intent(inout) :: parameters
    
  REAL    :: REFDK
  REAL    :: REFKDT
  REAL    :: FRZK
  REAL    :: FRZFACT
  INTEGER :: ISOIL

  parameters%ISWATER   =   ISWATER_TABLE
  parameters%ISBARREN  =  ISBARREN_TABLE
  parameters%ISICE     =     ISICE_TABLE
  parameters%ISCROP    =    ISCROP_TABLE
  parameters%EBLFOREST = EBLFOREST_TABLE

  parameters%URBAN_FLAG = .FALSE.
   IF( VEGTYPE == ISURBAN_TABLE    .or. VEGTYPE == LCZ_1_TABLE .or. VEGTYPE == LCZ_2_TABLE .or. &
             VEGTYPE == LCZ_3_TABLE      .or. VEGTYPE == LCZ_4_TABLE .or. VEGTYPE == LCZ_5_TABLE .or. &
             VEGTYPE == LCZ_6_TABLE      .or. VEGTYPE == LCZ_7_TABLE .or. VEGTYPE == LCZ_8_TABLE .or. &
             VEGTYPE == LCZ_9_TABLE      .or. VEGTYPE == LCZ_10_TABLE .or. VEGTYPE == LCZ_11_TABLE ) THEN   
    parameters%URBAN_FLAG = .TRUE.
   ENDIF





  parameters%CH2OP  =  CH2OP_TABLE(VEGTYPE)       
  parameters%DLEAF  =  DLEAF_TABLE(VEGTYPE)       
  parameters%Z0MVT  =  Z0MVT_TABLE(VEGTYPE)       
  parameters%HVT    =    HVT_TABLE(VEGTYPE)       
  parameters%HVB    =    HVB_TABLE(VEGTYPE)       
  parameters%DEN    =    DEN_TABLE(VEGTYPE)       
  parameters%RC     =     RC_TABLE(VEGTYPE)       
  parameters%MFSNO  =  MFSNO_TABLE(VEGTYPE)       
  parameters%SAIM   =   SAIM_TABLE(VEGTYPE,:)     
  parameters%LAIM   =   LAIM_TABLE(VEGTYPE,:)     
  parameters%SLA    =    SLA_TABLE(VEGTYPE)       
  parameters%DILEFC = DILEFC_TABLE(VEGTYPE)       
  parameters%DILEFW = DILEFW_TABLE(VEGTYPE)       
  parameters%FRAGR  =  FRAGR_TABLE(VEGTYPE)       
  parameters%LTOVRC = LTOVRC_TABLE(VEGTYPE)       

  parameters%C3PSN  =  C3PSN_TABLE(VEGTYPE)       
  parameters%KC25   =   KC25_TABLE(VEGTYPE)       
  parameters%AKC    =    AKC_TABLE(VEGTYPE)       
  parameters%KO25   =   KO25_TABLE(VEGTYPE)       
  parameters%AKO    =    AKO_TABLE(VEGTYPE)       
  parameters%VCMX25 = VCMX25_TABLE(VEGTYPE)       
  parameters%AVCMX  =  AVCMX_TABLE(VEGTYPE)       
  parameters%BP     =     BP_TABLE(VEGTYPE)       
  parameters%MP     =     MP_TABLE(VEGTYPE)       
  parameters%QE25   =   QE25_TABLE(VEGTYPE)       
  parameters%AQE    =    AQE_TABLE(VEGTYPE)       
  parameters%RMF25  =  RMF25_TABLE(VEGTYPE)       
  parameters%RMS25  =  RMS25_TABLE(VEGTYPE)       
  parameters%RMR25  =  RMR25_TABLE(VEGTYPE)       
  parameters%ARM    =    ARM_TABLE(VEGTYPE)       
  parameters%FOLNMX = FOLNMX_TABLE(VEGTYPE)       
  parameters%TMIN   =   TMIN_TABLE(VEGTYPE)       

  parameters%XL     =     XL_TABLE(VEGTYPE)       
  parameters%RHOL   =   RHOL_TABLE(VEGTYPE,:)     
  parameters%RHOS   =   RHOS_TABLE(VEGTYPE,:)     
  parameters%TAUL   =   TAUL_TABLE(VEGTYPE,:)     
  parameters%TAUS   =   TAUS_TABLE(VEGTYPE,:)     

  parameters%MRP    =    MRP_TABLE(VEGTYPE)       
  parameters%CWPVT  =  CWPVT_TABLE(VEGTYPE)       

  parameters%WRRAT  =  WRRAT_TABLE(VEGTYPE)       
  parameters%WDPOOL = WDPOOL_TABLE(VEGTYPE)       
  parameters%TDLEF  =  TDLEF_TABLE(VEGTYPE)       

  parameters%NROOT  =  NROOT_TABLE(VEGTYPE)       
  parameters%RGL    =    RGL_TABLE(VEGTYPE)       
  parameters%RSMIN  =     RS_TABLE(VEGTYPE)       
  parameters%HS     =     HS_TABLE(VEGTYPE)       
  parameters%TOPT   =   TOPT_TABLE(VEGTYPE)       
  parameters%RSMAX  =  RSMAX_TABLE(VEGTYPE)       





   parameters%ALBSAT    = ALBSAT_TABLE(SOILCOLOR,:)
   parameters%ALBDRY    = ALBDRY_TABLE(SOILCOLOR,:)
   parameters%ALBICE    = ALBICE_TABLE
   parameters%ALBLAK    = ALBLAK_TABLE               
   parameters%OMEGAS    = OMEGAS_TABLE
   parameters%BETADS    = BETADS_TABLE
   parameters%BETAIS    = BETAIS_TABLE
   parameters%EG        = EG_TABLE





  IF(CROPTYPE > 0) THEN
   parameters%PLTDAY    =    PLTDAY_TABLE(CROPTYPE)    
   parameters%HSDAY     =     HSDAY_TABLE(CROPTYPE)    
   parameters%PLANTPOP  =  PLANTPOP_TABLE(CROPTYPE)    
   parameters%IRRI      =      IRRI_TABLE(CROPTYPE)    
   parameters%GDDTBASE  =  GDDTBASE_TABLE(CROPTYPE)    
   parameters%GDDTCUT   =   GDDTCUT_TABLE(CROPTYPE)    
   parameters%GDDS1     =     GDDS1_TABLE(CROPTYPE)    
   parameters%GDDS2     =     GDDS2_TABLE(CROPTYPE)    
   parameters%GDDS3     =     GDDS3_TABLE(CROPTYPE)    
   parameters%GDDS4     =     GDDS4_TABLE(CROPTYPE)    
   parameters%GDDS5     =     GDDS5_TABLE(CROPTYPE)    
   parameters%C3PSN     =     C3PSNI_TABLE(CROPTYPE)   
   parameters%KC25      =      KC25I_TABLE(CROPTYPE)
   parameters%AKC       =       AKCI_TABLE(CROPTYPE)
   parameters%KO25      =      KO25I_TABLE(CROPTYPE)
   parameters%AKO       =       AKOI_TABLE(CROPTYPE)
   parameters%AVCMX     =     AVCMXI_TABLE(CROPTYPE)
   parameters%VCMX25    =    VCMX25I_TABLE(CROPTYPE)
   parameters%BP        =        BPI_TABLE(CROPTYPE)
   parameters%MP        =        MPI_TABLE(CROPTYPE)
   parameters%FOLNMX    =    FOLNMXI_TABLE(CROPTYPE)
   parameters%QE25      =      QE25I_TABLE(CROPTYPE)   
   parameters%C3C4      =      C3C4_TABLE(CROPTYPE)    
   parameters%AREF      =      AREF_TABLE(CROPTYPE)    
   parameters%PSNRF     =     PSNRF_TABLE(CROPTYPE)    
   parameters%I2PAR     =     I2PAR_TABLE(CROPTYPE)    
   parameters%TASSIM0   =   TASSIM0_TABLE(CROPTYPE)    
   parameters%TASSIM1   =   TASSIM1_TABLE(CROPTYPE)    
   parameters%TASSIM2   =   TASSIM2_TABLE(CROPTYPE)    
   parameters%K         =         K_TABLE(CROPTYPE)    
   parameters%EPSI      =      EPSI_TABLE(CROPTYPE)    
   parameters%Q10MR     =     Q10MR_TABLE(CROPTYPE)    
   parameters%FOLN_MX   =   FOLN_MX_TABLE(CROPTYPE)    
   parameters%LEFREEZ   =   LEFREEZ_TABLE(CROPTYPE)    
   parameters%DILE_FC   =   DILE_FC_TABLE(CROPTYPE,:)  
   parameters%DILE_FW   =   DILE_FW_TABLE(CROPTYPE,:)  
   parameters%FRA_GR    =    FRA_GR_TABLE(CROPTYPE)    
   parameters%LF_OVRC   =   LF_OVRC_TABLE(CROPTYPE,:)  
   parameters%ST_OVRC   =   ST_OVRC_TABLE(CROPTYPE,:)  
   parameters%RT_OVRC   =   RT_OVRC_TABLE(CROPTYPE,:)  
   parameters%LFMR25    =    LFMR25_TABLE(CROPTYPE)    
   parameters%STMR25    =    STMR25_TABLE(CROPTYPE)    
   parameters%RTMR25    =    RTMR25_TABLE(CROPTYPE)    
   parameters%GRAINMR25 = GRAINMR25_TABLE(CROPTYPE)    
   parameters%LFPT      =      LFPT_TABLE(CROPTYPE,:)  
   parameters%STPT      =      STPT_TABLE(CROPTYPE,:)  
   parameters%RTPT      =      RTPT_TABLE(CROPTYPE,:)  
   parameters%GRAINPT   =   GRAINPT_TABLE(CROPTYPE,:)  
   parameters%LFCT      =      LFCT_TABLE(CROPTYPE,:)  
   parameters%STCT      =      STCT_TABLE(CROPTYPE,:)  
   parameters%RTCT      =      RTCT_TABLE(CROPTYPE,:)  
   parameters%BIO2LAI   =   BIO2LAI_TABLE(CROPTYPE)    
  END IF





   parameters%CO2        =         CO2_TABLE
   parameters%O2         =          O2_TABLE
   parameters%TIMEAN     =      TIMEAN_TABLE
   parameters%FSATMX     =      FSATMX_TABLE
   parameters%Z0SNO      =       Z0SNO_TABLE
   parameters%SSI        =         SSI_TABLE
   parameters%SWEMX      =       SWEMX_TABLE
   parameters%TAU0         =      TAU0_TABLE
   parameters%GRAIN_GROWTH = GRAIN_GROWTH_TABLE
   parameters%EXTRA_GROWTH = EXTRA_GROWTH_TABLE
   parameters%DIRT_SOOT    =    DIRT_SOOT_TABLE
   parameters%BATS_COSZ    =    BATS_COSZ_TABLE
   parameters%BATS_VIS_NEW = BATS_VIS_NEW_TABLE
   parameters%BATS_NIR_NEW = BATS_NIR_NEW_TABLE
   parameters%BATS_VIS_AGE = BATS_VIS_AGE_TABLE
   parameters%BATS_NIR_AGE = BATS_NIR_AGE_TABLE
   parameters%BATS_VIS_DIR = BATS_VIS_DIR_TABLE
   parameters%BATS_NIR_DIR = BATS_NIR_DIR_TABLE
   parameters%RSURF_SNOW =  RSURF_SNOW_TABLE
   parameters%RSURF_EXP  =   RSURF_EXP_TABLE





    do isoil = 1, size(soiltype)
      parameters%BEXP(isoil)   = BEXP_TABLE   (SOILTYPE(isoil))
      parameters%DKSAT(isoil)  = DKSAT_TABLE  (SOILTYPE(isoil))
      parameters%DWSAT(isoil)  = DWSAT_TABLE  (SOILTYPE(isoil))
      parameters%PSISAT(isoil) = PSISAT_TABLE (SOILTYPE(isoil))
      parameters%QUARTZ(isoil) = QUARTZ_TABLE (SOILTYPE(isoil))
      parameters%SMCDRY(isoil) = SMCDRY_TABLE (SOILTYPE(isoil))
      parameters%SMCMAX(isoil) = SMCMAX_TABLE (SOILTYPE(isoil))
      parameters%SMCREF(isoil) = SMCREF_TABLE (SOILTYPE(isoil))
      parameters%SMCWLT(isoil) = SMCWLT_TABLE (SOILTYPE(isoil))
    end do
    
    parameters%F1     = F1_TABLE(SOILTYPE(1))
    parameters%REFDK  = REFDK_TABLE
    parameters%REFKDT = REFKDT_TABLE




    parameters%CSOIL  = CSOIL_TABLE
    parameters%ZBOT   = ZBOT_TABLE
    parameters%CZIL   = CZIL_TABLE

    FRZK   = FRZK_TABLE
    parameters%KDT    = parameters%REFKDT * parameters%DKSAT(1) / parameters%REFDK
    parameters%SLOPE  = SLOPE_TABLE(SLOPETYPE)

    IF(parameters%URBAN_FLAG)THEN  
       parameters%SMCMAX = 0.45 
       parameters%SMCREF = 0.42 
       parameters%SMCWLT = 0.40 
       parameters%SMCDRY = 0.40 
       parameters%CSOIL  = 3.E6
    ENDIF



    IF(SOILTYPE(1) /= 14) then
      FRZFACT = (parameters%SMCMAX(1) / parameters%SMCREF(1)) * (0.412 / 0.468)
      parameters%FRZX = FRZK * FRZFACT
    END IF

 END SUBROUTINE TRANSFER_MP_PARAMETERS

SUBROUTINE PEDOTRANSFER_SR2006(nsoil,sand,clay,orgm,parameters)

  use module_sf_noahmplsm
  use noahmp_tables
        
  implicit none
        
  integer,                    intent(in   ) :: nsoil     
  real, dimension( 1:nsoil ), intent(inout) :: sand
  real, dimension( 1:nsoil ), intent(inout) :: clay
  real, dimension( 1:nsoil ), intent(inout) :: orgm
    
  real, dimension( 1:nsoil ) :: theta_1500t
  real, dimension( 1:nsoil ) :: theta_1500
  real, dimension( 1:nsoil ) :: theta_33t
  real, dimension( 1:nsoil ) :: theta_33
  real, dimension( 1:nsoil ) :: theta_s33t
  real, dimension( 1:nsoil ) :: theta_s33
  real, dimension( 1:nsoil ) :: psi_et
  real, dimension( 1:nsoil ) :: psi_e
    
  type(noahmp_parameters), intent(inout) :: parameters
  integer :: k

  do k = 1,4
    if(sand(k) <= 0 .or. clay(k) <= 0) then
      sand(k) = 0.41
      clay(k) = 0.18
    end if
    if(orgm(k) <= 0 ) orgm(k) = 0.0
  end do
        
  theta_1500t =   sr2006_theta_1500t_a*sand       &
                + sr2006_theta_1500t_b*clay       &
                + sr2006_theta_1500t_c*orgm       &
                + sr2006_theta_1500t_d*sand*orgm  &
                + sr2006_theta_1500t_e*clay*orgm  &
                + sr2006_theta_1500t_f*sand*clay  &
                + sr2006_theta_1500t_g

  theta_1500  =   theta_1500t                      &
                + sr2006_theta_1500_a*theta_1500t  &
                + sr2006_theta_1500_b

  theta_33t   =   sr2006_theta_33t_a*sand       &
                + sr2006_theta_33t_b*clay       &
                + sr2006_theta_33t_c*orgm       &
                + sr2006_theta_33t_d*sand*orgm  &
                + sr2006_theta_33t_e*clay*orgm  &
                + sr2006_theta_33t_f*sand*clay  &
                + sr2006_theta_33t_g

  theta_33    =   theta_33t                              &
                + sr2006_theta_33_a*theta_33t*theta_33t  &
                + sr2006_theta_33_b*theta_33t            &
                + sr2006_theta_33_c

  theta_s33t  =   sr2006_theta_s33t_a*sand      &
                + sr2006_theta_s33t_b*clay      &
                + sr2006_theta_s33t_c*orgm      &
                + sr2006_theta_s33t_d*sand*orgm &
                + sr2006_theta_s33t_e*clay*orgm &
                + sr2006_theta_s33t_f*sand*clay &
                + sr2006_theta_s33t_g

  theta_s33   = theta_s33t                       &
                + sr2006_theta_s33_a*theta_s33t  &
                + sr2006_theta_s33_b

  psi_et      =   sr2006_psi_et_a*sand           &
                + sr2006_psi_et_b*clay           &
                + sr2006_psi_et_c*theta_s33      &
                + sr2006_psi_et_d*sand*theta_s33 &
                + sr2006_psi_et_e*clay*theta_s33 &
                + sr2006_psi_et_f*sand*clay      &
                + sr2006_psi_et_g
 
  psi_e       =   psi_et                        &
                + sr2006_psi_e_a*psi_et*psi_et  &
                + sr2006_psi_e_b*psi_et         &
                + sr2006_psi_e_c
    
  parameters%smcwlt = theta_1500
  parameters%smcref = theta_33
  parameters%smcmax =   theta_33    &
                      + theta_s33            &
                      + sr2006_smcmax_a*sand &
                      + sr2006_smcmax_b

  parameters%bexp   = 3.816712826 / (log(theta_33) - log(theta_1500) )
  parameters%psisat = psi_e
  parameters%dksat  = 1930.0 * (parameters%smcmax - theta_33) ** (3.0 - 1.0/parameters%bexp)
  parameters%quartz = sand
    

    
  parameters%psisat = max(0.1,parameters%psisat)     
  parameters%psisat = 0.101997 * parameters%psisat   
  parameters%dksat  = parameters%dksat / 3600000.0   
  parameters%dwsat  = parameters%dksat * parameters%psisat *parameters%bexp / parameters%smcmax  
  parameters%smcdry = parameters%smcwlt
  

  
  parameters%smcmax = max(0.32 ,min(parameters%smcmax,             0.50 ))
  parameters%smcref = max(0.17 ,min(parameters%smcref,parameters%smcmax ))
  parameters%smcwlt = max(0.01 ,min(parameters%smcwlt,parameters%smcref ))
  parameters%smcdry = max(0.01 ,min(parameters%smcdry,parameters%smcref ))
  parameters%bexp   = max(2.50 ,min(parameters%bexp,               12.0 ))
  parameters%psisat = max(0.03 ,min(parameters%psisat,             1.00 ))
  parameters%dksat  = max(5.e-7,min(parameters%dksat,              1.e-5))
  parameters%dwsat  = max(1.e-6,min(parameters%dwsat,              3.e-5))
  parameters%quartz = max(0.05 ,min(parameters%quartz,             0.95 ))
    
 END SUBROUTINE PEDOTRANSFER_SR2006

  SUBROUTINE NOAHMP_INIT ( MMINLU, SNOW , SNOWH , CANWAT , ISLTYP ,   IVGTYP, XLAT, &
       TSLB , SMOIS , SH2O , DZS , FNDSOILW , FNDSNOWH ,             &
       TSK, isnowxy , tvxy     ,tgxy     ,canicexy ,         TMN,     XICE,   &
       canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
       fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
       wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
       stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   ,lai      ,           &
       grainxy  ,gddxy    ,                                                   &
       croptype ,cropcat  ,                      &

       t2mvxy   ,t2mbxy   ,chstarxy,            &

       NSOIL, restart,                 &
       allowed_to_read , iopt_run,  iopt_crop,                        &
       sf_urban_physics,                         &  
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte,                &
       smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy, dx, dy, msftx, msfty,&     
       wtddt    ,stepwtd  ,dt       ,qrfsxy     ,qspringsxy  , qslatxy    ,  &      
       fdepthxy ,ht     ,riverbedxy ,eqzwt     ,rivercondxy ,pexpxy       ,  &      
       rechclim,                                                             &      
       gecros_state)                                                                

  USE NOAHMP_TABLES
  use module_sf_gecros, only: seednc,sla0,slnmin,ffat,flig,foac,fmin,npl,seedw,eg,fcrsh,seednc,lnci,cfv


  IMPLICIT NONE



    INTEGER, INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte

    INTEGER, INTENT(IN)       ::     NSOIL, iopt_run, iopt_crop

    LOGICAL, INTENT(IN)       ::     restart,                    &
         &                           allowed_to_read
    INTEGER, INTENT(IN)       ::     sf_urban_physics                              

    REAL,    DIMENSION( NSOIL), INTENT(IN)    ::     DZS  
    REAL,    INTENT(IN) , OPTIONAL ::     DX, DY
    REAL,    DIMENSION( ims:ime, jms:jme ) ,  INTENT(IN) , OPTIONAL :: MSFTX,MSFTY

    REAL,    DIMENSION( ims:ime, NSOIL, jms:jme ) ,    &
         &   INTENT(INOUT)    ::     SMOIS,                      &
         &                           SH2O,                       &
         &                           TSLB

    REAL,    DIMENSION( ims:ime, jms:jme ) ,                     &
         &   INTENT(INOUT)    ::     SNOW,                       &
         &                           SNOWH,                      &
         &                           CANWAT

    INTEGER, DIMENSION( ims:ime, jms:jme ),                      &
         &   INTENT(IN)       ::     ISLTYP,  &
                                     IVGTYP

    LOGICAL, INTENT(IN)       ::     FNDSOILW,                   &
         &                           FNDSNOWH

    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: XLAT         
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: TSK         
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: TMN         
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: XICE         
    INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isnowxy     
    REAL, DIMENSION(ims:ime,-2:NSOIL,jms:jme), INTENT(INOUT) :: zsnsoxy  
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: tsnoxy   
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snicexy  
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snliqxy  
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tvxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tgxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canicexy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canliqxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: eahxy       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tahxy       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: cmxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fwetxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: sneqvoxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: alboldxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qsnowxy     
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wslakexy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: zwtxy       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: waxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wtxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lfmassxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: rtmassxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stmassxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: woodxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: grainxy     
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: gddxy       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stblcpxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fastcpxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xsaixy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lai         

    INTEGER, DIMENSION(ims:ime,  jms:jme), INTENT(OUT) :: cropcat
    REAL   , DIMENSION(ims:ime,5,jms:jme), INTENT(IN ) :: croptype



    REAL, DIMENSION(ims:ime,1:nsoil,jms:jme), INTENT(INOUT) , OPTIONAL :: smoiseq 
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: smcwtdxy    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: deeprechxy  
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: rechxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qrfsxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qspringsxy  
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qslatxy     
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: areaxy      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: FDEPTHXY    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: HT          
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: RIVERBEDXY  
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: EQZWT       
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT), OPTIONAL :: RIVERCONDXY 
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT), OPTIONAL :: PEXPXY      
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: rechclim

    REAL, DIMENSION(ims:ime,60,jms:jme), INTENT(INOUT),   OPTIONAL :: gecros_state                                     

    INTEGER,  INTENT(OUT) , OPTIONAL :: STEPWTD
    REAL, INTENT(IN) , OPTIONAL :: DT, WTDDT


    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mvxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mbxy        
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chstarxy        



    REAL, DIMENSION(1:NSOIL)  :: ZSOIL      
    

    REAL                      :: BEXP, SMCMAX, PSISAT
    REAL                      :: FK, masslai,masssai
    

    REAL ::  hti,rdi,fpro,lncmin,fcar,cfo,clvi,crti,ygo,nlvi,laii,nrti,slnbi


    REAL, PARAMETER           :: BLIM  = 5.5
    REAL, PARAMETER           :: HLICE = 3.335E5
    REAL, PARAMETER           :: GRAV = 9.81
    REAL, PARAMETER           :: T0 = 273.15

    INTEGER                   :: errflag, i,j,itf,jtf,ns

    character(len=240) :: err_message
    character(len=4)  :: MMINSL
    character(len=*), intent(in) :: MMINLU
    MMINSL='STAS'

    call read_mp_veg_parameters(trim(MMINLU))
    call read_mp_soil_parameters()
    call read_mp_rad_parameters()
    call read_mp_global_parameters()
    call read_mp_crop_parameters()
    call read_mp_optional_parameters()

    IF( .NOT. restart ) THEN

       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)

       
       
       
       IF(.NOT.FNDSNOWH)THEN
          
          CALL wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          DO J = jts,jtf
             DO I = its,itf
                SNOWH(I,J)=SNOW(I,J)*0.005               
             ENDDO
          ENDDO
       ENDIF


       

       DO J = jts,jtf
          DO I = its,itf
             IF ( SNOW(i,j) > 0. .AND. SNOWH(i,j) == 0. .OR. SNOWH(i,j) > 0. .AND. SNOW(i,j) == 0.) THEN
               WRITE(err_message,*)"problem with initial snow fields: snow/snowh>0 while snowh/snow=0 at i,j" &
                                     ,i,j,snow(i,j),snowh(i,j)
               CALL wrf_message(err_message)
             ENDIF
             IF ( SNOW( i,j ) > 2000. ) THEN
               SNOWH(I,J) = SNOWH(I,J) * 2000. / SNOW(I,J)      
               SNOW (I,J) = 2000.                               
             ENDIF
          ENDDO
       ENDDO

       errflag = 0
       DO j = jts,jtf
          DO i = its,itf
             IF ( ISLTYP( i,j ) .LT. 1 ) THEN
                errflag = 1
                WRITE(err_message,*)"module_sf_noahlsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
                CALL wrf_message(err_message)
             ENDIF
          ENDDO
       ENDDO
       IF ( errflag .EQ. 1 ) THEN
          CALL wrf_error_fatal3("<stdin>",1591,&
"module_sf_noahlsm.F: lsminit: out of range value "// &
               "of ISLTYP. Is this field in the input?" )
       ENDIF














       DO J = jts , jtf
          DO I = its , itf
	    IF(IVGTYP(I,J)==ISICE_TABLE .AND. XICE(I,J) <= 0.0) THEN
              DO NS=1, NSOIL
	        SMOIS(I,NS,J) = 1.0                     
	        SH2O(I,NS,J) = 0.0
	        TSLB(I,NS,J) = MIN(TSLB(I,NS,J),263.15) 
              END DO
	        
		SNOW(I,J) = MAX(SNOW(I,J), 10.0)        
                SNOWH(I,J)=SNOW(I,J)*0.01               
	    ELSE
	      
              BEXP   =   BEXP_TABLE(ISLTYP(I,J))
              SMCMAX = SMCMAX_TABLE(ISLTYP(I,J))
              PSISAT = PSISAT_TABLE(ISLTYP(I,J))

              DO NS=1, NSOIL
	        IF ( SMOIS(I,NS,J) > SMCMAX )  SMOIS(I,NS,J) = SMCMAX
              END DO
              IF ( ( BEXP > 0.0 ) .AND. ( SMCMAX > 0.0 ) .AND. ( PSISAT > 0.0 ) ) THEN
                DO NS=1, NSOIL
                   IF ( TSLB(I,NS,J) < 273.149 ) THEN    
                      FK=(( (HLICE/(GRAV*(-PSISAT))) *                              &
                           ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BEXP) )*SMCMAX
                      FK = MAX(FK, 0.02)
                      SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )
                   ELSE
                      SH2O(I,NS,J)=SMOIS(I,NS,J)
                   ENDIF
                END DO
              ELSE
                DO NS=1, NSOIL
                   SH2O(I,NS,J)=SMOIS(I,NS,J)
                END DO
              ENDIF
            ENDIF
          ENDDO
       ENDDO



       DO J = jts,jtf
          DO I = its,itf
             tvxy       (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tvxy(I,J) = 273.15
             tgxy       (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tgxy(I,J) = 273.15
             CANWAT     (I,J) = 0.0
             canliqxy   (I,J) = CANWAT(I,J)
             canicexy   (I,J) = 0.
             eahxy      (I,J) = 2000. 
             tahxy      (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tahxy(I,J) = 273.15


             t2mvxy     (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mvxy(I,J) = 273.15
             t2mbxy     (I,J) = TSK(I,J)
	       if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mbxy(I,J) = 273.15
             chstarxy     (I,J) = 0.1


             cmxy       (I,J) = 0.0
             chxy       (I,J) = 0.0
             fwetxy     (I,J) = 0.0
             sneqvoxy   (I,J) = 0.0
             alboldxy   (I,J) = 0.65
             qsnowxy    (I,J) = 0.0
             wslakexy   (I,J) = 0.0

             if(iopt_run.ne.5) then 
                   waxy       (I,J) = 4900.                                       
                   wtxy       (I,J) = waxy(i,j)                                   
                   zwtxy      (I,J) = (25. + 2.0) - waxy(i,j)/1000/0.2            
             else
                   waxy       (I,J) = 0.
                   wtxy       (I,J) = 0.
                   areaxy     (I,J) = (DX * DY) / ( MSFTX(I,J) * MSFTY(I,J) )
             endif

           IF(IVGTYP(I,J) == ISBARREN_TABLE .OR. IVGTYP(I,J) == ISICE_TABLE .OR. &
	      ( SF_URBAN_PHYSICS == 0 .AND. IVGTYP(I,J) == ISURBAN_TABLE )  .OR. &
	      IVGTYP(I,J) == ISWATER_TABLE ) THEN
	     
	     lai        (I,J) = 0.0
             xsaixy     (I,J) = 0.0
             lfmassxy   (I,J) = 0.0
             stmassxy   (I,J) = 0.0
             rtmassxy   (I,J) = 0.0
             woodxy     (I,J) = 0.0
             stblcpxy   (I,J) = 0.0
             fastcpxy   (I,J) = 0.0
             grainxy    (I,J) = 1E-10
             gddxy      (I,J) = 0
	     cropcat    (I,J) = 0

	   ELSE
	     
	     lai        (I,J) = max(lai(i,j),0.05)             
             xsaixy     (I,J) = max(0.1*lai(I,J),0.05)         
             masslai = 1000. / max(SLA_TABLE(IVGTYP(I,J)),1.0) 
             lfmassxy   (I,J) = lai(i,j)*masslai               
             masssai = 1000. / 3.0                             
             stmassxy   (I,J) = xsaixy(i,j)*masssai            
             rtmassxy   (I,J) = 500.0                          
             woodxy     (I,J) = 500.0                          
             stblcpxy   (I,J) = 1000.0                         
             fastcpxy   (I,J) = 1000.0                         
             grainxy    (I,J) = 1E-10
             gddxy      (I,J) = 0    



	     if(iopt_crop == 1 ) then
	       cropcat    (i,j) = default_crop_table
               if(croptype(i,5,j) >= 0.5) then
                 rtmassxy(i,j) = 0.0
                 woodxy  (i,j) = 0.0                    

	         if(    croptype(i,1,j) > croptype(i,2,j) .and. &
		        croptype(i,1,j) > croptype(i,3,j) .and. &
		        croptype(i,1,j) > croptype(i,4,j) ) then   

		   cropcat (i,j) = 1
                   lfmassxy(i,j) =    lai(i,j)/0.015               
                   stmassxy(i,j) = xsaixy(i,j)/0.003

	         elseif(croptype(i,2,j) > croptype(i,1,j) .and. &
		        croptype(i,2,j) > croptype(i,3,j) .and. &
		        croptype(i,2,j) > croptype(i,4,j) ) then   

		   cropcat (i,j) = 2
                   lfmassxy(i,j) =    lai(i,j)/0.030               
                   stmassxy(i,j) = xsaixy(i,j)/0.003

	         else

		   cropcat (i,j) = default_crop_table
                   lfmassxy(i,j) =    lai(i,j)/0.035
                   stmassxy(i,j) = xsaixy(i,j)/0.003

	         end if

	       end if
	     end if



	     if(iopt_crop == 2) then
	       cropcat    (i,j) = 0
               if(croptype(i,5,j) >= 0.5) then
                  if(croptype(i,3,j) > 0.0)             cropcat(i,j) = 1 
                  if(croptype(i,1,j) > croptype(i,3,j)) cropcat(i,j) = 2 
	       end if

               hti    = 0.01
               rdi    = 10.
               fpro   = 6.25*seednc
               lncmin = sla0*slnmin
               fcar   = 1.-fpro-ffat-flig-foac-fmin
               cfo    = 0.444*fcar+0.531*fpro+0.774*ffat+0.667*flig+0.368*foac
               clvi   = npl * seedw * cfo * eg * fcrsh
               crti   = npl * seedw * cfo * eg * (1.-fcrsh)
               ygo    = cfo/(1.275*fcar+1.887*fpro+3.189*ffat+2.231*flig+0.954* &
                        foac)*30./12.
               nlvi   = min(0.75 * npl * seedw * eg * seednc, lnci * clvi/cfv)
               laii   = clvi/cfv*sla0
               nrti   = npl * seedw * eg * seednc - nlvi
               slnbi  = nlvi/laii
    
               call gecros_init(xlat(i,j),hti,rdi,clvi,crti,nlvi,laii,nrti,slnbi,gecros_state(i,:,j))

             end if

	   END IF

          enddo
       enddo
       

       
       
       ZSOIL(1)         = -DZS(1)          
       DO NS=2, NSOIL
          ZSOIL(NS)       = ZSOIL(NS-1) - DZS(NS)
       END DO

       
       
       CALL snow_init ( ims , ime , jms , jme , its , itf , jts , jtf , 3 , &
            &           NSOIL , zsoil , snow , tgxy , snowh ,     &
            &           zsnsoxy , tsnoxy , snicexy , snliqxy , isnowxy )

       

       if(iopt_run.eq.5) then
          IF ( PRESENT(smoiseq)     .AND. &
            PRESENT(smcwtdxy)    .AND. &
            PRESENT(rechxy)      .AND. &
            PRESENT(deeprechxy)  .AND. &
            PRESENT(areaxy)      .AND. &
            PRESENT(dx)          .AND. &
            PRESENT(dy)          .AND. &
            PRESENT(msftx)       .AND. &
            PRESENT(msfty)       .AND. &
            PRESENT(wtddt)       .AND. &
            PRESENT(stepwtd)     .AND. &
            PRESENT(dt)          .AND. &
            PRESENT(qrfsxy)      .AND. &
            PRESENT(qspringsxy)  .AND. &
            PRESENT(qslatxy)     .AND. &
            PRESENT(fdepthxy)    .AND. &
            PRESENT(ht)          .AND. &
            PRESENT(riverbedxy)  .AND. &
            PRESENT(eqzwt)       .AND. &
            PRESENT(rivercondxy) .AND. &
            PRESENT(pexpxy)      .AND. &
            PRESENT(rechclim)    ) THEN

             STEPWTD = nint(WTDDT*60./DT)
             STEPWTD = max(STEPWTD,1)

          ELSE
             CALL wrf_error_fatal3("<stdin>",1833,&
'Not enough fields to use groundwater option in Noah-MP')
          END IF
       endif

    ENDIF
    
  END SUBROUTINE NOAHMP_INIT




  SUBROUTINE SNOW_INIT ( ims , ime , jms , jme , its , itf , jts , jtf ,                  &
       &                 NSNOW , NSOIL , ZSOIL , SWE , TGXY , SNODEP ,                    &
       &                 ZSNSOXY , TSNOXY , SNICEXY ,SNLIQXY , ISNOWXY )










    IMPLICIT NONE

    INTEGER, INTENT(IN)                              :: ims, ime, jms, jme
    INTEGER, INTENT(IN)                              :: its, itf, jts, jtf
    INTEGER, INTENT(IN)                              :: NSNOW
    INTEGER, INTENT(IN)                              :: NSOIL
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SWE 
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SNODEP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: TGXY
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: ZSOIL

    INTEGER, INTENT(OUT), DIMENSION(ims:ime, jms:jme)                :: ISNOWXY 
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: ZSNSOXY 
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: TSNOXY  
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNICEXY 
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNLIQXY 




    INTEGER                           :: I,J,IZ
    REAL,   DIMENSION(-NSNOW+1:    0) :: DZSNO
    REAL,   DIMENSION(-NSNOW+1:NSOIL) :: DZSNSO



    DO J = jts , jtf
       DO I = its , itf
          IF ( SNODEP(I,J) < 0.025 ) THEN
             ISNOWXY(I,J) = 0
             DZSNO(-NSNOW+1:0) = 0.
          ELSE
             IF ( ( SNODEP(I,J) >= 0.025 ) .AND. ( SNODEP(I,J) <= 0.05 ) ) THEN
                ISNOWXY(I,J)    = -1
                DZSNO(0)  = SNODEP(I,J)
             ELSE IF ( ( SNODEP(I,J) > 0.05 ) .AND. ( SNODEP(I,J) <= 0.10 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = SNODEP(I,J)/2.
                DZSNO( 0) = SNODEP(I,J)/2.
             ELSE IF ( (SNODEP(I,J) > 0.10 ) .AND. ( SNODEP(I,J) <= 0.25 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = 0.05
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1)
             ELSE IF ( ( SNODEP(I,J) > 0.25 ) .AND. ( SNODEP(I,J) <= 0.45 ) ) THEN
                ISNOWXY(I,J)    = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.5*(SNODEP(I,J)-DZSNO(-2))
                DZSNO( 0) = 0.5*(SNODEP(I,J)-DZSNO(-2))
             ELSE IF ( SNODEP(I,J) > 0.45 ) THEN
                ISNOWXY(I,J)     = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.20
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1) - DZSNO(-2)
             ELSE
                CALL wrf_error_fatal3("<stdin>",1912,&
"Problem with the logic assigning snow layers.")
             END IF
          END IF

          TSNOXY (I,-NSNOW+1:0,J) = 0.
          SNICEXY(I,-NSNOW+1:0,J) = 0.
          SNLIQXY(I,-NSNOW+1:0,J) = 0.
          DO IZ = ISNOWXY(I,J)+1 , 0
             TSNOXY(I,IZ,J)  = TGXY(I,J)  
             SNLIQXY(I,IZ,J) = 0.00
             SNICEXY(I,IZ,J) = 1.00 * DZSNO(IZ) * (SWE(I,J)/SNODEP(I,J))  
          END DO

          
          DO IZ = ISNOWXY(I,J)+1 , 0
             DZSNSO(IZ) = -DZSNO(IZ)
          END DO

          
          DZSNSO(1) = ZSOIL(1)
          DO IZ = 2 , NSOIL
             DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
          END DO

          
          ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSO(ISNOWXY(I,J)+1)
          DO IZ = ISNOWXY(I,J)+2 , NSOIL
             ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSO(IZ)
          ENDDO

       END DO
    END DO

  END SUBROUTINE SNOW_INIT


    SUBROUTINE GROUNDWATER_INIT (   &
            &            GRID, NSOIL , DZS, ISLTYP, IVGTYP, WTDDT , &
            &            FDEPTH, TOPO, RIVERBED, EQWTD, RIVERCOND, PEXP , AREA ,WTD ,  &
            &            SMOIS,SH2O, SMOISEQ, SMCWTDXY, DEEPRECHXY, RECHXY ,  &
            &            QSLATXY, QRFSXY, QSPRINGSXY,                  &
            &            rechclim  ,                                   &
            &            ids,ide, jds,jde, kds,kde,                    &
            &            ims,ime, jms,jme, kms,kme,                    &
            &            ips,ipe, jps,jpe, kps,kpe,                    &
            &            its,ite, jts,jte, kts,kte                     )


  USE NOAHMP_TABLES, ONLY : BEXP_TABLE,SMCMAX_TABLE,PSISAT_TABLE,SMCWLT_TABLE,DWSAT_TABLE,DKSAT_TABLE, &
                                ISURBAN_TABLE, ISICE_TABLE ,ISWATER_TABLE
  USE module_sf_noahmp_groundwater, ONLY : LATERALFLOW
  USE module_domain, only: domain
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm , ONLY : halo_em_hydro_noahmp_sub


  IMPLICIT NONE


    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           ips,ipe, jps,jpe, kps,kpe,  &
         &                           its,ite, jts,jte, kts,kte
    TYPE(domain) , TARGET :: grid                             
    INTEGER, INTENT(IN)                              :: NSOIL
    REAL,   INTENT(IN)                               ::     WTDDT
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: DZS
    INTEGER, INTENT(IN), DIMENSION(ims:ime, jms:jme) :: ISLTYP, IVGTYP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: FDEPTH, TOPO , AREA
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: rechclim 
    REAL,    INTENT(OUT), DIMENSION(ims:ime, jms:jme) :: RIVERCOND
    REAL,    INTENT(INOUT), DIMENSION(ims:ime, jms:jme) :: WTD, RIVERBED, EQWTD, PEXP
    REAL,     DIMENSION( ims:ime , 1:nsoil, jms:jme ), &
         &    INTENT(INOUT)   ::                          SMOIS, &
         &                                                 SH2O, &
         &                                                 SMOISEQ
    REAL,    INTENT(INOUT), DIMENSION(ims:ime, jms:jme) ::  &
                                                           SMCWTDXY, &
                                                           DEEPRECHXY, &
                                                           RECHXY, &
                                                           QSLATXY, &
                                                           QRFSXY, &
                                                           QSPRINGSXY  

    INTEGER  :: I,J,K,ITER,itf,jtf, NITER, NCOUNT,NS
    REAL :: BEXP,SMCMAX,PSISAT,SMCWLT,DWSAT,DKSAT
    REAL :: FRLIQ,SMCEQDEEP
    REAL :: DELTAT,RCOND,TOTWATER
    REAL :: AA,BBB,CC,DD,DX,FUNC,DFUNC,DDZ,EXPON,SMC,FLUX
    REAL, DIMENSION(1:NSOIL) :: SMCEQ,ZSOIL
    REAL,      DIMENSION( ims:ime, jms:jme )    :: QLAT, QRF
    INTEGER,   DIMENSION( ims:ime, jms:jme )    :: LANDMASK 

       
       
       ZSOIL(1)         = -DZS(1)          
       DO NS=2, NSOIL
          ZSOIL(NS)       = ZSOIL(NS-1) - DZS(NS)
       END DO


       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)


    WHERE(IVGTYP.NE.ISWATER_TABLE.AND.IVGTYP.NE.ISICE_TABLE)
         LANDMASK=1
    ELSEWHERE
         LANDMASK=-1
    ENDWHERE
    
    PEXP = 1.0

    DELTAT=365.*24*3600. 




    WTD=EQWTD

    NCOUNT=0

 DO NITER=1,500







CALL HALO_EM_HYDRO_NOAHMP_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )




IF(NCOUNT.GT.0.OR.NITER.eq.1)THEN
    QLAT = 0.
    CALL LATERALFLOW(ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA       &
                        ,ids,ide,jds,jde,kds,kde                      & 
                        ,ims,ime,jms,jme,kms,kme                      &
                        ,its,ite,jts,jte,kts,kte                      )

    NCOUNT=0
    DO J=jts,jtf
       DO I=its,itf
          IF(LANDMASK(I,J).GT.0)THEN
            IF(QLAT(i,j).GT.1.e-2)THEN
                 NCOUNT=NCOUNT+1
                 WTD(i,j)=min(WTD(i,j)+0.25,0.)
            ENDIF
          ENDIF
        ENDDO
     ENDDO
ENDIF

 ENDDO







CALL HALO_EM_HYDRO_NOAHMP_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


EQWTD=WTD








    DO J=jts,jtf
       DO I=its,itf

        DDZ = EQWTD(I,J)- ( RIVERBED(I,J)-TOPO(I,J) )

        IF(DDZ.LT.0.)then
               RIVERBED(I,J)=TOPO(I,J)+EQWTD(I,J)
               DDZ=0.
        ENDIF


        TOTWATER = AREA(I,J)*(QLAT(I,J)+RECHCLIM(I,J)*0.001)/DELTAT

        IF (TOTWATER.GT.0) THEN
              RIVERCOND(I,J) = TOTWATER / MAX(DDZ,0.05)
        ELSE
              RIVERCOND(I,J)=0.01

              RIVERBED(I,J)=TOPO(I,J)+EQWTD(I,J)
        ENDIF


       ENDDO
    ENDDO



    RIVERBED = min( RIVERBED-TOPO, 0.)



    DELTAT = WTDDT * 60. 




    QLAT = 0.
    CALL LATERALFLOW(ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA       &
                        ,ids,ide,jds,jde,kds,kde                      & 
                        ,ims,ime,jms,jme,kms,kme                      &
                        ,its,ite,jts,jte,kts,kte                      )
                        


    DO J=jts,jtf
       DO I=its,itf
          IF(LANDMASK(I,J).GT.0)THEN
             IF(WTD(I,J) .GT. RIVERBED(I,J) .AND.  EQWTD(I,J) .GT. RIVERBED(I,J)) THEN
               RCOND = RIVERCOND(I,J) * EXP(PEXP(I,J)*(WTD(I,J)-EQWTD(I,J)))
             ELSE    
               RCOND = RIVERCOND(I,J)
             ENDIF
             QRF(I,J) = RCOND * (WTD(I,J)-RIVERBED(I,J)) * DELTAT/AREA(I,J)

             QRF(I,J) = MAX(QRF(I,J),0.) 
          ELSE
             QRF(I,J) = 0.
          ENDIF
       ENDDO
    ENDDO



       DO J = jts,jtf
          DO I = its,itf
             BEXP   =   BEXP_TABLE(ISLTYP(I,J))
             SMCMAX = SMCMAX_TABLE(ISLTYP(I,J))
             SMCWLT = SMCWLT_TABLE(ISLTYP(I,J))
             IF(IVGTYP(I,J)==ISURBAN_TABLE)THEN
                 SMCMAX = 0.45         
                 SMCWLT = 0.40         
             ENDIF 
             DWSAT  =   DWSAT_TABLE(ISLTYP(I,J))
             DKSAT  =   DKSAT_TABLE(ISLTYP(I,J))
             PSISAT = -PSISAT_TABLE(ISLTYP(I,J))
           IF ( ( BEXP > 0.0 ) .AND. ( smcmax > 0.0 ) .AND. ( -psisat > 0.0 ) ) THEN
             
                    CALL EQSMOISTURE(NSOIL ,  ZSOIL , SMCMAX , SMCWLT ,DWSAT, DKSAT  ,BEXP  , & 
                                     SMCEQ                          )  

             SMOISEQ (I,1:NSOIL,J) = SMCEQ (1:NSOIL)


              
             IF(WTD(I,J) < ZSOIL(NSOIL)-DZS(NSOIL)) THEN




                         EXPON = 2. * BEXP + 3.
                         DDZ = ZSOIL(NSOIL) - WTD(I,J)
                         CC = PSISAT/DDZ
                         FLUX = (QLAT(I,J)-QRF(I,J))/DELTAT

                         SMC = 0.5 * SMCMAX

                         DO ITER = 1, 100
                           DD = (SMC+SMCMAX)/(2.*SMCMAX)
                           AA = -DKSAT * DD  ** EXPON
                           BBB = CC * ( (SMCMAX/SMC)**BEXP - 1. ) + 1. 
                           FUNC =  AA * BBB - FLUX
                           DFUNC = -DKSAT * (EXPON/(2.*SMCMAX)) * DD ** (EXPON - 1.) * BBB &
                                   + AA * CC * (-BEXP) * SMCMAX ** BEXP * SMC ** (-BEXP-1.)

                           DX = FUNC/DFUNC
                           SMC = SMC - DX
                           IF ( ABS (DX) < 1.E-6)EXIT
                         ENDDO

                  SMCWTDXY(I,J) = MAX(SMC,1.E-4)

             ELSEIF(WTD(I,J) < ZSOIL(NSOIL))THEN
                  SMCEQDEEP = SMCMAX * ( PSISAT / ( PSISAT - DZS(NSOIL) ) ) ** (1./BEXP)

                  SMCEQDEEP = MAX(SMCEQDEEP,1.E-4)
                  SMCWTDXY(I,J) = SMCMAX * ( WTD(I,J) -  (ZSOIL(NSOIL)-DZS(NSOIL))) + &
                                  SMCEQDEEP * (ZSOIL(NSOIL) - WTD(I,J))

             ELSE 
                  SMCWTDXY(I,J) = SMCMAX
                  DO K=NSOIL,2,-1
                     IF(WTD(I,J) .GE. ZSOIL(K-1))THEN
                          FRLIQ = SH2O(I,K,J) / SMOIS(I,K,J)
                          SMOIS(I,K,J) = SMCMAX
                          SH2O(I,K,J) = SMCMAX * FRLIQ
                     ELSE
                          IF(SMOIS(I,K,J).LT.SMCEQ(K))THEN
                              WTD(I,J) = ZSOIL(K)
                          ELSE
                              WTD(I,J) = ( SMOIS(I,K,J)*DZS(K) - SMCEQ(K)*ZSOIL(K-1) + SMCMAX*ZSOIL(K) ) / &
                                         (SMCMAX - SMCEQ(K))   
                          ENDIF
                          EXIT
                     ENDIF
                  ENDDO
             ENDIF
            ELSE
              SMOISEQ (I,1:NSOIL,J) = SMCMAX
              SMCWTDXY(I,J) = SMCMAX
              WTD(I,J) = 0.
            ENDIF



             DEEPRECHXY(I,J) = 0.
             RECHXY(I,J) = 0.
             QSLATXY(I,J) = 0.
             QRFSXY(I,J) = 0.
             QSPRINGSXY(I,J) = 0.

          ENDDO
       ENDDO




    END  SUBROUTINE GROUNDWATER_INIT


  SUBROUTINE EQSMOISTURE(NSOIL  ,  ZSOIL , SMCMAX , SMCWLT, DWSAT , DKSAT ,BEXP , & 
                         SMCEQ                          )  

  IMPLICIT NONE


  INTEGER,                         INTENT(IN) :: NSOIL 
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL 
  REAL,                            INTENT(IN) :: SMCMAX , SMCWLT, BEXP , DWSAT, DKSAT

  REAL,  DIMENSION(      1:NSOIL), INTENT(OUT) :: SMCEQ  

  INTEGER                                     :: K , ITER
  REAL                                        :: DDZ , SMC, FUNC, DFUNC , AA, BB , EXPON, DX




   DO K=1,NSOIL

            IF ( K == 1 )THEN
                DDZ = -ZSOIL(K+1) * 0.5
            ELSEIF ( K < NSOIL ) THEN
                DDZ = ( ZSOIL(K-1) - ZSOIL(K+1) ) * 0.5
            ELSE
                DDZ = ZSOIL(K-1) - ZSOIL(K)
            ENDIF



            EXPON = BEXP +1.
            AA = DWSAT/DDZ
            BB = DKSAT / SMCMAX ** EXPON

            SMC = 0.5 * SMCMAX

         DO ITER = 1, 100
            FUNC = (SMC - SMCMAX) * AA +  BB * SMC ** EXPON
            DFUNC = AA + BB * EXPON * SMC ** BEXP 

            DX = FUNC/DFUNC
            SMC = SMC - DX
            IF ( ABS (DX) < 1.E-6)EXIT
         ENDDO


             SMCEQ(K) = MIN(MAX(SMC,1.E-4),SMCMAX*0.99)
   ENDDO

END  SUBROUTINE EQSMOISTURE



SUBROUTINE gecros_init(xlat,hti,rdi,clvi,crti,nlvi,laii,nrti,slnbi,state_gecros)   
implicit none
REAL, INTENT(IN)     :: HTI
REAL, INTENT(IN)     :: RDI
REAL, INTENT(IN)     :: CLVI
REAL, INTENT(IN)     :: CRTI
REAL, INTENT(IN)     :: NLVI
REAL, INTENT(IN)     :: LAII
REAL, INTENT(IN)     :: NRTI
REAL, INTENT(IN)     :: SLNBI
REAL, INTENT(IN)     :: XLAT
REAL, DIMENSION(1:60), INTENT(INOUT) :: STATE_GECROS

  
  STATE_GECROS(1) = 0.      
  STATE_GECROS(2) = 0.      
  STATE_GECROS(3) = 0.      
  STATE_GECROS(4) = CLVI    
  STATE_GECROS(5) = 0.      
  STATE_GECROS(6) = 0.      
  STATE_GECROS(7) = 0.      
  STATE_GECROS(8) = CRTI    
  STATE_GECROS(9) =  0.     
  STATE_GECROS(10) = 0.     
  STATE_GECROS(11) = NRTI   
  STATE_GECROS(12) = 0.     
  STATE_GECROS(13) = NLVI   
  STATE_GECROS(14) = 0.     
  STATE_GECROS(15) = NLVI   
  STATE_GECROS(16) = 0.     
  STATE_GECROS(17) = 0.     
  STATE_GECROS(18) = 0.     
  STATE_GECROS(19) = 0.     
  STATE_GECROS(20) = 0.     
  STATE_GECROS(21) = 0.     
  STATE_GECROS(22) = 0.     
  STATE_GECROS(23) = 0.     
  STATE_GECROS(24) = SLNBI  
  STATE_GECROS(25) = LAII   
  STATE_GECROS(26) = 0.     
  STATE_GECROS(27) = 0.     
  STATE_GECROS(28) = 0.     
  STATE_GECROS(29) = 0.     
  STATE_GECROS(30) = 0.     
  STATE_GECROS(31) = 0.     
  STATE_GECROS(32) = 0.01   
  STATE_GECROS(33) = RDI    
  STATE_GECROS(34) = 0.     
  STATE_GECROS(35) = 0.     
  STATE_GECROS(36) = 0.     
  STATE_GECROS(37) = 0.     
  STATE_GECROS(38) = 0.     
  STATE_GECROS(39) = 0.     
  STATE_GECROS(40) = -1.    
  STATE_GECROS(41) = -1.    
  STATE_GECROS(42) = -1.    
  STATE_GECROS(43) = 0.     
  STATE_GECROS(44) = XLAT   
  STATE_GECROS(45) = 0.     
  STATE_GECROS(46) = 0.     
  STATE_GECROS(47) = 0.     
  STATE_GECROS(48) = 0.     
  STATE_GECROS(49) = 0.01   
  STATE_GECROS(50) = 0.01   
  STATE_GECROS(51) = HTI    
  STATE_GECROS(52) = RDI    
  STATE_GECROS(53) = CLVI   
  STATE_GECROS(54) = CRTI   
  STATE_GECROS(55) = NRTI   
  STATE_GECROS(56) = NLVI   
  STATE_GECROS(57) = SLNBI  
  STATE_GECROS(58) = LAII   
    
END SUBROUTINE gecros_init

SUBROUTINE gecros_reinit(STATE_GECROS)   
implicit none
REAL, DIMENSION(1:60), INTENT(INOUT) :: STATE_GECROS

  
  STATE_GECROS(1) = 0.               
  STATE_GECROS(2) = 0.               
  STATE_GECROS(3) = 0.               
  STATE_GECROS(4) = STATE_GECROS(53) 
  STATE_GECROS(5) = 0.               
  STATE_GECROS(6) = 0.               
  STATE_GECROS(7) = 0.               
  STATE_GECROS(8) = STATE_GECROS(54) 
  STATE_GECROS(9) = 0.               
  STATE_GECROS(10) = 0.              
  STATE_GECROS(11) = STATE_GECROS(55)
  STATE_GECROS(12) = 0.              
  STATE_GECROS(13) = STATE_GECROS(56)
  STATE_GECROS(14) = 0.              
  STATE_GECROS(15) = STATE_GECROS(56)
  STATE_GECROS(16) = 0.              
  STATE_GECROS(17) = 0.              
  STATE_GECROS(18) = 0.              
  STATE_GECROS(19) = 0.              
  STATE_GECROS(20) = 0.              
  STATE_GECROS(21) = 0.              
  STATE_GECROS(22) = 0.              
  STATE_GECROS(23) = 0.              
  STATE_GECROS(24) = STATE_GECROS(57)
  STATE_GECROS(25) = STATE_GECROS(58)
  STATE_GECROS(26) = 0.              
  STATE_GECROS(27) = 0.              
  STATE_GECROS(28) = 0.              
  STATE_GECROS(29) = 0.              
  STATE_GECROS(30) = 0.              
  STATE_GECROS(31) = 0.              
  STATE_GECROS(32) = STATE_GECROS(51)
  STATE_GECROS(33) = STATE_GECROS(52)
  STATE_GECROS(34) = 0.              
  STATE_GECROS(35) = 0.              
  STATE_GECROS(36) = 0.              
  STATE_GECROS(37) = 0.              
  STATE_GECROS(38) = 0.              
  STATE_GECROS(39) = 0.              
  STATE_GECROS(40) = -1.             
  STATE_GECROS(41) = -1.             
  STATE_GECROS(42) = 1.              
  STATE_GECROS(43) = 0.              
  STATE_GECROS(45) = 0.              
  STATE_GECROS(46) = 0.              
  STATE_GECROS(47) = 0.              
  STATE_GECROS(48) = 0.              
  STATE_GECROS(49) = 0.01            
  STATE_GECROS(50) = 0.01            
  
END SUBROUTINE gecros_reinit











function checkIfHarvest(STATE_GECROS, DT, harvestDS1, harvestDS2, harvestDS1ExtraDays, harvestDS2ExtraDays) 
implicit none
real :: DT, harvestDS1, harvestDS2
real :: daysSinceDS1, daysSinceDS2
real :: harvestDS1ExtraDays, harvestDS2ExtraDays
integer :: checkIfHarvest
REAL, DIMENSION(1:60), INTENT(INOUT) :: STATE_GECROS


 
 if (STATE_GECROS(1) >= harvestDS1) then
 
    if (STATE_GECROS(38) >= harvestDS1ExtraDays) then
        checkIfHarvest=1
 
    else
        STATE_GECROS(38) = STATE_GECROS(38) + DT/86400.
    endif
 else 
 
 
 
 
 
 
 checkIfHarvest=0
 if (STATE_GECROS(1) >= harvestDS2 ) then

       if (STATE_GECROS(39) >= harvestDS2ExtraDays) then
           checkIfHarvest=1
       else 
           STATE_GECROS(39) = STATE_GECROS(39) + DT/86400.
           checkIfHarvest=0
      endif
 endif
 endif
 return
end function checkIfHarvest



  SUBROUTINE noahmp_urban(sf_urban_physics,   NSOIL,         IVGTYP,  ITIMESTEP,            & 
                                 DT,     COSZ_URB2D,     XLAT_URB2D,                        & 
                                T3D,           QV3D,          U_PHY,      V_PHY,   SWDOWN,  & 
                             SWDDIR,         SWDDIF,                                        &
		                GLW,          P8W3D,         RAINBL,       DZ8W,      ZNT,  & 
                                TSK,            HFX,            QFX,         LH,   GRDFLX,  & 
		             ALBEDO,          EMISS,           QSFC,                        & 
                            ids,ide,        jds,jde,        kds,kde,                        &
                            ims,ime,        jms,jme,        kms,kme,                        &
                            its,ite,        jts,jte,        kts,kte,                        &
                         cmr_sfcdif,     chr_sfcdif,     cmc_sfcdif,                        &
	                 chc_sfcdif,    cmgr_sfcdif,    chgr_sfcdif,                        &
                           tr_urb2d,       tb_urb2d,       tg_urb2d,                        & 
	                   tc_urb2d,       qc_urb2d,       uc_urb2d,                        & 
                         xxxr_urb2d,     xxxb_urb2d,     xxxg_urb2d, xxxc_urb2d,            & 
                          trl_urb3d,      tbl_urb3d,      tgl_urb3d,                        & 
                           sh_urb2d,       lh_urb2d,        g_urb2d,   rn_urb2d,  ts_urb2d, & 
                         psim_urb2d,     psih_urb2d,      u10_urb2d,  v10_urb2d,            & 
                       GZ1OZ0_urb2d,     AKMS_URB2D,                                        & 
                          th2_urb2d,       q2_urb2d,      ust_urb2d,                        & 
                         declin_urb,      omg_urb2d,                                        & 
                    num_roof_layers,num_wall_layers,num_road_layers,                        & 
                                dzr,            dzb,            dzg,                        & 
                         cmcr_urb2d,      tgr_urb2d,     tgrl_urb3d,  smr_urb3d,            & 
                        drelr_urb2d,    drelb_urb2d,    drelg_urb2d,                        & 
                      flxhumr_urb2d,  flxhumb_urb2d,  flxhumg_urb2d,                        & 
                             julian,          julyr,                                        & 
                          frc_urb2d,    utype_urb2d,                                        & 
                                chs,           chs2,           cqs2,                        & 
                      num_urban_ndm,  urban_map_zrd,  urban_map_zwd, urban_map_gd,          & 
                       urban_map_zd,  urban_map_zdf,   urban_map_bd, urban_map_wd,          & 
                      urban_map_gbd,  urban_map_fbd, urban_map_zgrd,                                      & 
                       num_urban_hi,                                                        & 
                          trb_urb4d,      tw1_urb4d,      tw2_urb4d,  tgb_urb4d,            & 
                         tlev_urb3d,     qlev_urb3d,                                        & 
                       tw1lev_urb3d,   tw2lev_urb3d,                                        & 
                        tglev_urb3d,    tflev_urb3d,                                        & 
                        sf_ac_urb3d,    lf_ac_urb3d,    cm_ac_urb3d,                        & 
                       sfvent_urb3d,   lfvent_urb3d,                                        & 
                       sfwin1_urb3d,   sfwin2_urb3d,                                        & 
                         sfw1_urb3d,     sfw2_urb3d,      sfr_urb3d,  sfg_urb3d,            & 
                        ep_pv_urb3d,     t_pv_urb3d,                                        & 
                          trv_urb4d,       qr_urb4d,      qgr_urb3d,  tgr_urb3d,            & 
                        drain_urb4d,  draingr_urb3d,     sfrv_urb3d, lfrv_urb3d,            & 
                          dgr_urb3d,       dg_urb3d,      lfr_urb3d,  lfg_urb3d,            & 
                           lp_urb2d,       hi_urb2d,       lb_urb2d,  hgt_urb2d,            & 
                           mh_urb2d,     stdh_urb2d,       lf_urb2d,                        & 
                             th_phy,            rho,          p_phy,        ust,            & 
                                gmt,         julday,          xlong,       xlat,            & 
                            a_u_bep,        a_v_bep,        a_t_bep,    a_q_bep,            & 
                            a_e_bep,        b_u_bep,        b_v_bep,                        & 
                            b_t_bep,        b_q_bep,        b_e_bep,    dlg_bep,            & 
                           dl_u_bep,         sf_bep,         vl_bep                         & 
                 )

  USE module_sf_urban,    only: urban
  USE module_sf_bep,      only: bep
  USE module_sf_bep_bem,  only: bep_bem
  USE module_ra_gfdleta,  only: cal_mon_day
  USE NOAHMP_TABLES, ONLY: ISURBAN_TABLE
  USE module_model_constants, only: KARMAN, CP, XLV

    IMPLICIT NONE


    INTEGER,                                         INTENT(IN   ) ::  sf_urban_physics   
    INTEGER,                                         INTENT(IN   ) ::  NSOIL     
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  IVGTYP    
    INTEGER,                                         INTENT(IN   ) ::  ITIMESTEP 
    REAL,                                            INTENT(IN   ) ::  DT        
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  COSZ_URB2D
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAT_URB2D
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  T3D       
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  QV3D      
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  U_PHY     
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  V_PHY     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDOWN    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDDIF    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDDIR    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  GLW       
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  P8W3D     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  RAINBL    
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  DZ8W      
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ZNT       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TSK       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  HFX       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QFX       
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LH        
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  GRDFLX    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBEDO    
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EMISS     
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSFC      

    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &  
         &                           ims,ime, jms,jme, kms,kme,  &  
         &                           its,ite, jts,jte, kts,kte      



     INTEGER,                                                INTENT(IN   ) :: num_roof_layers
     INTEGER,                                                INTENT(IN   ) :: num_wall_layers
     INTEGER,                                                INTENT(IN   ) :: num_road_layers

     INTEGER,        DIMENSION( ims:ime, jms:jme ),          INTENT(IN   ) :: UTYPE_URB2D
     REAL,           DIMENSION( ims:ime, jms:jme ),          INTENT(IN   ) :: FRC_URB2D

     REAL, OPTIONAL, DIMENSION(1:num_roof_layers),           INTENT(IN   ) :: DZR
     REAL, OPTIONAL, DIMENSION(1:num_wall_layers),           INTENT(IN   ) :: DZB
     REAL, OPTIONAL, DIMENSION(1:num_road_layers),           INTENT(IN   ) :: DZG
     REAL, OPTIONAL,                                         INTENT(IN   ) :: DECLIN_URB
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(IN   ) :: OMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: TH_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: P_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: RHO

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(INOUT) :: UST
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(INOUT) :: CHS, CHS2, CQS2

     INTEGER,  INTENT(IN   )   ::  julian, julyr                  



     INTEGER :: UTYPE_URB 
     REAL    :: TA_URB       
     REAL    :: QA_URB       
     REAL    :: UA_URB       
     REAL    :: U1_URB       
     REAL    :: V1_URB       
     REAL    :: SSG_URB      
     REAL    :: LLG_URB      
     REAL    :: RAIN_URB     
     REAL    :: RHOO_URB     
     REAL    :: ZA_URB       
     REAL    :: DELT_URB     
     REAL    :: SSGD_URB     
     REAL    :: SSGQ_URB     
     REAL    :: XLAT_URB     
     REAL    :: COSZ_URB     
     REAL    :: OMG_URB      
     REAL    :: ZNT_URB      
     REAL    :: TR_URB
     REAL    :: TB_URB
     REAL    :: TG_URB
     REAL    :: TC_URB
     REAL    :: QC_URB
     REAL    :: UC_URB
     REAL    :: XXXR_URB
     REAL    :: XXXB_URB
     REAL    :: XXXG_URB
     REAL    :: XXXC_URB
     REAL, DIMENSION(1:num_roof_layers) :: TRL_URB  
     REAL, DIMENSION(1:num_wall_layers) :: TBL_URB  
     REAL, DIMENSION(1:num_road_layers) :: TGL_URB  
     LOGICAL  :: LSOLAR_URB



     INTEGER :: jmonth, jday
     REAL    :: DRELR_URB
     REAL    :: DRELB_URB
     REAL    :: DRELG_URB
     REAL    :: FLXHUMR_URB
     REAL    :: FLXHUMB_URB
     REAL    :: FLXHUMG_URB
     REAL    :: CMCR_URB
     REAL    :: TGR_URB

     REAL, DIMENSION(1:num_roof_layers) :: SMR_URB  
     REAL, DIMENSION(1:num_roof_layers) :: TGRL_URB 

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: CMCR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: TGR_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TGRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: SMR_URB3D




     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D



     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIM_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: GZ1OZ0_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: U10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: V10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: TH2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: Q2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: AKMS_URB2D
     REAL,           DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: UST_URB2D




     REAL :: TS_URB           
     REAL :: QS_URB           
     REAL :: SH_URB           
     REAL :: LH_URB           
     REAL :: LH_KINEMATIC_URB 
     REAL :: SW_URB           
     REAL :: ALB_URB          
     REAL :: LW_URB           
     REAL :: G_URB            
     REAL :: RN_URB           
     REAL :: PSIM_URB         
     REAL :: PSIH_URB         
     REAL :: GZ1OZ0_URB       
     REAL :: U10_URB          
     REAL :: V10_URB          
     REAL :: TH2_URB          
     REAL :: Q2_URB           
     REAL :: CHS_URB
     REAL :: CHS2_URB
     REAL :: UST_URB



     REAL :: mh_urb
     REAL :: stdh_urb
     REAL :: lp_urb
     REAL :: hgt_urb
     REAL, DIMENSION(4) :: lf_urb



     INTEGER :: I,J,K
     REAL :: Q1



     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CMR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CHR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CMGR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CHGR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CMC_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: CHC_SFCDIF



     REAL, OPTIONAL,                                                    INTENT(IN   ) :: GMT
     INTEGER, OPTIONAL,                                                 INTENT(IN   ) :: JULDAY
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: XLAT, XLONG
     INTEGER,                                                           INTENT(IN   ) :: num_urban_ndm
     INTEGER,                                                           INTENT(IN   ) :: urban_map_zrd
     INTEGER,                                                           INTENT(IN   ) :: urban_map_zwd
     INTEGER,                                                           INTENT(IN   ) :: urban_map_gd
     INTEGER,                                                           INTENT(IN   ) :: urban_map_zd
     INTEGER,                                                           INTENT(IN   ) :: urban_map_zdf
     INTEGER,                                                           INTENT(IN   ) :: urban_map_bd
     INTEGER,                                                           INTENT(IN   ) :: urban_map_wd
     INTEGER,                                                           INTENT(IN   ) :: urban_map_gbd
     INTEGER,                                                           INTENT(IN   ) :: urban_map_fbd
     INTEGER,                                                           INTENT(IN   ) :: urban_map_zgrd
     INTEGER,                                                           INTENT(IN   ) :: NUM_URBAN_HI
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ),     INTENT(IN   ) :: hi_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: lp_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: lb_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: hgt_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: mh_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: stdh_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ),                  INTENT(IN   ) :: lf_urb2d

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zrd, jms:jme ),    INTENT(INOUT) :: trb_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ),    INTENT(INOUT) :: tw1_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ),    INTENT(INOUT) :: tw2_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gd , jms:jme ),    INTENT(INOUT) :: tgb_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ),    INTENT(INOUT) :: tlev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ),    INTENT(INOUT) :: qlev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ),    INTENT(INOUT) :: tw1lev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ),    INTENT(INOUT) :: tw2lev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gbd, jms:jme ),    INTENT(INOUT) :: tglev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_fbd, jms:jme ),    INTENT(INOUT) :: tflev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: lf_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: sf_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: cm_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: sfvent_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: lfvent_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ),    INTENT(INOUT) :: sfwin1_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ),    INTENT(INOUT) :: sfwin2_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ),    INTENT(INOUT) :: sfw1_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ),    INTENT(INOUT) :: sfw2_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),    INTENT(INOUT) :: sfr_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),    INTENT(INOUT) :: sfg_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ep_pv_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: t_pv_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ),INTENT(INOUT) :: trv_urb4d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ),INTENT(INOUT) :: qr_urb4d 
     REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme ), INTENT(INOUT) :: qgr_urb3d  
     REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme ), INTENT(INOUT) :: tgr_urb3d  
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: drain_urb4d 
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: draingr_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: sfrv_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: lfrv_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: dgr_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: dg_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: lfr_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: lfg_urb3d 
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_u_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_v_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_t_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_q_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_e_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_u_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_v_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_t_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_q_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_e_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: vl_bep    
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: dlg_bep   
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: sf_bep    
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: dl_u_bep  



     REAL,    DIMENSION( its:ite, jts:jte) :: HFX_RURAL,GRDFLX_RURAL          
     REAL,    DIMENSION( its:ite, jts:jte) :: QFX_RURAL                       
     REAL,    DIMENSION( its:ite, jts:jte) :: ALB_RURAL,EMISS_RURAL,TSK_RURAL 
     REAL,    DIMENSION( its:ite, jts:jte) :: HFX_URB,UMOM_URB,VMOM_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: QFX_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: EMISS_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: RL_UP_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: RS_ABS_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: GRDFLX_URB

     REAL :: SIGMA_SB,RL_UP_RURAL,RL_UP_TOT,RS_ABS_TOT,UMOM,VMOM
     REAL :: r1,r2,r3
     REAL :: CMR_URB, CHR_URB, CMC_URB, CHC_URB, CMGR_URB, CHGR_URB
     REAL :: frc_urb,lb_urb
     REAL :: check

    character(len=80) :: message

    DO J=JTS,JTE
    DO I=ITS,ITE
      HFX_RURAL(I,J)                = HFX(I,J)
      QFX_RURAL(I,J)                = QFX(I,J)
      GRDFLX_RURAL(I,J)             = GRDFLX(I,J)
      EMISS_RURAL(I,J)              = EMISS(I,J)
      TSK_RURAL(I,J)                = TSK(I,J)
      ALB_RURAL(I,J)                = ALBEDO(I,J)
    END DO
    END DO

IF (SF_URBAN_PHYSICS == 1 ) THEN         





JLOOP : DO J = jts, jte

ILOOP : DO I = its, ite


  IF( IVGTYP(I,J) == ISURBAN_TABLE .or. IVGTYP(I,J) == 31 .or. &
      IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33 ) THEN

    UTYPE_URB = UTYPE_URB2D(I,J) 

    TA_URB    = T3D(I,1,J)                                
    QA_URB    = QV3D(I,1,J)/(1.0+QV3D(I,1,J))             
    UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
    U1_URB    = U_PHY(I,1,J)
    V1_URB    = V_PHY(I,1,J)
    IF(UA_URB < 1.) UA_URB=1.                             
    SSG_URB   = SWDOWN(I,J)                               
    SSGD_URB  = 0.8*SWDOWN(I,J)                           
    SSGQ_URB  = SSG_URB-SSGD_URB                          
    LLG_URB   = GLW(I,J)                                  
    RAIN_URB  = RAINBL(I,J)                               
    RHOO_URB  = (P8W3D(I,KTS+1,J)+P8W3D(I,KTS,J))*0.5 / (287.04 * TA_URB * (1.0+ 0.61 * QA_URB)) 
    ZA_URB    = 0.5*DZ8W(I,1,J)                           
    DELT_URB  = DT                                        
    XLAT_URB  = XLAT_URB2D(I,J)                           
    COSZ_URB  = COSZ_URB2D(I,J) 
    OMG_URB   = OMG_URB2D(I,J)
    ZNT_URB   = ZNT(I,J)

    LSOLAR_URB = .FALSE.

    TR_URB = TR_URB2D(I,J)
    TB_URB = TB_URB2D(I,J)
    TG_URB = TG_URB2D(I,J)
    TC_URB = TC_URB2D(I,J)
    QC_URB = QC_URB2D(I,J)
    UC_URB = UC_URB2D(I,J)

    TGR_URB     = TGR_URB2D(I,J)
    CMCR_URB    = CMCR_URB2D(I,J)
    FLXHUMR_URB = FLXHUMR_URB2D(I,J)
    FLXHUMB_URB = FLXHUMB_URB2D(I,J)
    FLXHUMG_URB = FLXHUMG_URB2D(I,J)
    DRELR_URB   = DRELR_URB2D(I,J)
    DRELB_URB   = DRELB_URB2D(I,J)
    DRELG_URB   = DRELG_URB2D(I,J)

    DO K = 1,num_roof_layers
      TRL_URB(K) = TRL_URB3D(I,K,J)
      SMR_URB(K) = SMR_URB3D(I,K,J)
      TGRL_URB(K)= TGRL_URB3D(I,K,J)
    END DO

    DO K = 1,num_wall_layers
      TBL_URB(K) = TBL_URB3D(I,K,J)
    END DO

    DO K = 1,num_road_layers
      TGL_URB(K) = TGL_URB3D(I,K,J)
    END DO

    XXXR_URB = XXXR_URB2D(I,J)
    XXXB_URB = XXXB_URB2D(I,J)
    XXXG_URB = XXXG_URB2D(I,J)
    XXXC_URB = XXXC_URB2D(I,J)


    IF (CHS(I,J) < 1.0E-02) THEN
      CHS(I,J)  = 1.0E-02
    ENDIF
    IF (CHS2(I,J) < 1.0E-02) THEN
      CHS2(I,J)  = 1.0E-02
    ENDIF
    IF (CQS2(I,J) < 1.0E-02) THEN
      CQS2(I,J)  = 1.0E-02
    ENDIF

    CHS_URB  = CHS(I,J)
    CHS2(I,J)= CQS2(I,J)      
    CHS2_URB = CHS2(I,J)
    IF (PRESENT(CMR_SFCDIF)) THEN
      CMR_URB = CMR_SFCDIF(I,J)
      CHR_URB = CHR_SFCDIF(I,J)
      CMGR_URB = CMGR_SFCDIF(I,J)
      CHGR_URB = CHGR_SFCDIF(I,J)
      CMC_URB = CMC_SFCDIF(I,J)
      CHC_URB = CHC_SFCDIF(I,J)
    ENDIF



    MH_URB   = MH_URB2D(I,J)
    STDH_URB = STDH_URB2D(I,J)
    LP_URB   = LP_URB2D(I,J)
    HGT_URB  = HGT_URB2D(I,J)
    LF_URB   = 0.0
    DO K = 1,4
      LF_URB(K) = LF_URB2D(I,K,J)
    ENDDO
    FRC_URB  = FRC_URB2D(I,J)
    LB_URB   = LB_URB2D(I,J)
    CHECK    = 0
    IF (I.EQ.73.AND.J.EQ.125)THEN
      CHECK = 1
    END IF



    CALL cal_mon_day(julian,julyr,jmonth,jday)
    CALL urban(LSOLAR_URB,                                                             & 
          num_roof_layers, num_wall_layers, num_road_layers,                           & 
                DZR,        DZB,        DZG, & 
          UTYPE_URB,     TA_URB,     QA_URB,     UA_URB,   U1_URB,  V1_URB, SSG_URB,   & 
           SSGD_URB,   SSGQ_URB,    LLG_URB,   RAIN_URB, RHOO_URB,                     & 
             ZA_URB, DECLIN_URB,   COSZ_URB,    OMG_URB,                               & 
           XLAT_URB,   DELT_URB,    ZNT_URB,                                           & 
            CHS_URB,   CHS2_URB,                                                       & 
             TR_URB,     TB_URB,     TG_URB,     TC_URB,   QC_URB,   UC_URB,           & 
            TRL_URB,    TBL_URB,    TGL_URB,                                           & 
           XXXR_URB,   XXXB_URB,   XXXG_URB,   XXXC_URB,                               & 
             TS_URB,     QS_URB,     SH_URB,     LH_URB, LH_KINEMATIC_URB,             & 
             SW_URB,    ALB_URB,     LW_URB,      G_URB,   RN_URB, PSIM_URB, PSIH_URB, & 
         GZ1OZ0_URB,                                                                   & 
            CMR_URB,    CHR_URB,    CMC_URB,    CHC_URB,                               &
            U10_URB,    V10_URB,    TH2_URB,     Q2_URB,                               & 
            UST_URB,     mh_urb,   stdh_urb,     lf_urb,   lp_urb,                     & 
            hgt_urb,    frc_urb,     lb_urb,      check, CMCR_URB,TGR_URB,             & 
           TGRL_URB,    SMR_URB,   CMGR_URB,   CHGR_URB,   jmonth,                     & 
          DRELR_URB,  DRELB_URB,                                                       & 
          DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB )

    TS_URB2D(I,J) = TS_URB

    ALBEDO(I,J)   = FRC_URB2D(I,J) * ALB_URB + (1-FRC_URB2D(I,J)) * ALBEDO(I,J)        
    HFX(I,J)      = FRC_URB2D(I,J) * SH_URB  + (1-FRC_URB2D(I,J)) * HFX(I,J)           
    QFX(I,J)      = FRC_URB2D(I,J) * LH_KINEMATIC_URB &
                       + (1-FRC_URB2D(I,J))* QFX(I,J)                                  
    LH(I,J)       = FRC_URB2D(I,J) * LH_URB  + (1-FRC_URB2D(I,J)) * LH(I,J)            
    GRDFLX(I,J)   = FRC_URB2D(I,J) * (G_URB) + (1-FRC_URB2D(I,J)) * GRDFLX(I,J)        
    TSK(I,J)      = FRC_URB2D(I,J) * TS_URB  + (1-FRC_URB2D(I,J)) * TSK(I,J)           






                   QSFC(I,J)= FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*QSFC(I,J)               
    UST(I,J)      = FRC_URB2D(I,J) * UST_URB + (1-FRC_URB2D(I,J)) * UST(I,J)     



    TR_URB2D(I,J) = TR_URB
    TB_URB2D(I,J) = TB_URB
    TG_URB2D(I,J) = TG_URB
    TC_URB2D(I,J) = TC_URB
    QC_URB2D(I,J) = QC_URB
    UC_URB2D(I,J) = UC_URB

    TGR_URB2D(I,J)     = TGR_URB
    CMCR_URB2D(I,J)    = CMCR_URB
    FLXHUMR_URB2D(I,J) = FLXHUMR_URB
    FLXHUMB_URB2D(I,J) = FLXHUMB_URB
    FLXHUMG_URB2D(I,J) = FLXHUMG_URB
    DRELR_URB2D(I,J)   = DRELR_URB
    DRELB_URB2D(I,J)   = DRELB_URB
    DRELG_URB2D(I,J)   = DRELG_URB

    DO K = 1,num_roof_layers
      TRL_URB3D(I,K,J) = TRL_URB(K)
      SMR_URB3D(I,K,J) = SMR_URB(K)
      TGRL_URB3D(I,K,J)= TGRL_URB(K)
    END DO
    DO K = 1,num_wall_layers
      TBL_URB3D(I,K,J) = TBL_URB(K)
    END DO
    DO K = 1,num_road_layers
      TGL_URB3D(I,K,J) = TGL_URB(K)
    END DO

    XXXR_URB2D(I,J)    = XXXR_URB
    XXXB_URB2D(I,J)    = XXXB_URB
    XXXG_URB2D(I,J)    = XXXG_URB
    XXXC_URB2D(I,J)    = XXXC_URB

    SH_URB2D(I,J)      = SH_URB
    LH_URB2D(I,J)      = LH_URB
    G_URB2D(I,J)       = G_URB         
    RN_URB2D(I,J)      = RN_URB
    PSIM_URB2D(I,J)    = PSIM_URB
    PSIH_URB2D(I,J)    = PSIH_URB
    GZ1OZ0_URB2D(I,J)  = GZ1OZ0_URB
    U10_URB2D(I,J)     = U10_URB
    V10_URB2D(I,J)     = V10_URB
    TH2_URB2D(I,J)     = TH2_URB
    Q2_URB2D(I,J)      = Q2_URB
    UST_URB2D(I,J)     = UST_URB
    AKMS_URB2D(I,J)    = KARMAN * UST_URB2D(I,J)/(GZ1OZ0_URB2D(I,J)-PSIM_URB2D(I,J))
    IF (PRESENT(CMR_SFCDIF)) THEN
      CMR_SFCDIF(I,J)  = CMR_URB
      CHR_SFCDIF(I,J)  = CHR_URB
      CMGR_SFCDIF(I,J) = CMGR_URB
      CHGR_SFCDIF(I,J) = CHGR_URB
      CMC_SFCDIF(I,J)  = CMC_URB
      CHC_SFCDIF(I,J)  = CHC_URB
    ENDIF

  ENDIF                                 

ENDDO ILOOP                             
ENDDO JLOOP                             

ENDIF                                   









IF (SF_URBAN_PHYSICS == 2) THEN

DO J=JTS,JTE
DO I=ITS,ITE

  EMISS_URB(I,J)       = 0.
  RL_UP_URB(I,J)       = 0.
  RS_ABS_URB(I,J)      = 0.
  GRDFLX_URB(I,J)      = 0.
  B_Q_BEP(I,KTS:KTE,J) = 0.

END DO
END DO

  CALL BEP(frc_urb2d,  utype_urb2d, itimestep,       dz8w,         &
                  dt,        u_phy,     v_phy,                     &
              th_phy,          rho,     p_phy,     swdown,    glw, &
                 gmt,       julday,     xlong,       xlat,         &
          declin_urb,   cosz_urb2d, omg_urb2d,                     &
       num_urban_ndm, urban_map_zrd, urban_map_zwd, urban_map_gd,  &
        urban_map_zd, urban_map_zdf,  urban_map_bd, urban_map_wd,  &
       urban_map_gbd, urban_map_fbd,  num_urban_hi,                &
           trb_urb4d,    tw1_urb4d, tw2_urb4d,  tgb_urb4d,         &
          sfw1_urb3d,   sfw2_urb3d, sfr_urb3d,  sfg_urb3d,         &
            lp_urb2d,     hi_urb2d,  lb_urb2d,  hgt_urb2d,         &
             a_u_bep,      a_v_bep,   a_t_bep,                     &
             a_e_bep,      b_u_bep,   b_v_bep,                     &
             b_t_bep,      b_e_bep,   b_q_bep,    dlg_bep,         &
            dl_u_bep,       sf_bep,    vl_bep,                     &
           rl_up_urb,   rs_abs_urb, emiss_urb, grdflx_urb,         &
         ids,ide, jds,jde, kds,kde,                                &
         ims,ime, jms,jme, kms,kme,                                &
         its,ite, jts,jte, kts,kte )

ENDIF 

IF (SF_URBAN_PHYSICS == 3) THEN

DO J=JTS,JTE
DO I=ITS,ITE

  EMISS_URB(I,J)       = 0.
  RL_UP_URB(I,J)       = 0.
  RS_ABS_URB(I,J)      = 0.
  GRDFLX_URB(I,J)      = 0.
  B_Q_BEP(I,KTS:KTE,J) = 0.

END DO
END DO

  CALL BEP_BEM( frc_urb2d,  utype_urb2d,    itimestep,         dz8w,       &
                       dt,        u_phy,        v_phy,                     &
                   th_phy,          rho,        p_phy,       swdown,  glw, &
                      gmt,       julday,        xlong,         xlat,       &
               declin_urb,   cosz_urb2d,    omg_urb2d,                     &
            num_urban_ndm, urban_map_zrd, urban_map_zwd, urban_map_gd,     &
             urban_map_zd, urban_map_zdf,  urban_map_bd, urban_map_wd,     &
            urban_map_gbd, urban_map_fbd,  urban_map_zgrd,num_urban_hi,    &
                trb_urb4d,    tw1_urb4d,    tw2_urb4d,    tgb_urb4d,       &
               tlev_urb3d,   qlev_urb3d, tw1lev_urb3d, tw2lev_urb3d,       &
              tglev_urb3d,  tflev_urb3d,  sf_ac_urb3d,  lf_ac_urb3d,       &
              cm_ac_urb3d, sfvent_urb3d, lfvent_urb3d,                     &
             sfwin1_urb3d, sfwin2_urb3d,                                   &
               sfw1_urb3d,   sfw2_urb3d,    sfr_urb3d,    sfg_urb3d,       &
              ep_pv_urb3d,   t_pv_urb3d,                                   & 
                trv_urb4d,     qr_urb4d,    qgr_urb3d,   tgr_urb3d,        & 
              drain_urb4d,draingr_urb3d,   sfrv_urb3d,  lfrv_urb3d,        & 
                dgr_urb3d,     dg_urb3d,    lfr_urb3d,   lfg_urb3d,        & 
                   rainbl,       swddir,       swddif,                     &
                 lp_urb2d,     hi_urb2d,     lb_urb2d,    hgt_urb2d,       &
                  a_u_bep,      a_v_bep,      a_t_bep,                     &
                  a_e_bep,      b_u_bep,      b_v_bep,                     &
                  b_t_bep,      b_e_bep,      b_q_bep,      dlg_bep,       &
                 dl_u_bep,       sf_bep,       vl_bep,                     &
                rl_up_urb,   rs_abs_urb,    emiss_urb,   grdflx_urb, qv3d, &
             ids,ide, jds,jde, kds,kde,                                    &
             ims,ime, jms,jme, kms,kme,                                    &
             its,ite, jts,jte, kts,kte )

ENDIF 

IF((SF_URBAN_PHYSICS == 2).OR.(SF_URBAN_PHYSICS == 3))THEN 

  sigma_sb=5.67e-08
  do j = jts, jte
  do i = its, ite
    UMOM_URB(I,J)     = 0.
    VMOM_URB(I,J)     = 0.
    HFX_URB(I,J)      = 0.
    QFX_URB(I,J)      = 0.

    do k=kts,kte
      a_u_bep(i,k,j) = a_u_bep(i,k,j)*frc_urb2d(i,j)
      a_v_bep(i,k,j) = a_v_bep(i,k,j)*frc_urb2d(i,j)
      a_t_bep(i,k,j) = a_t_bep(i,k,j)*frc_urb2d(i,j)
      a_q_bep(i,k,j) = 0.
      a_e_bep(i,k,j) = 0.
      b_u_bep(i,k,j) = b_u_bep(i,k,j)*frc_urb2d(i,j)
      b_v_bep(i,k,j) = b_v_bep(i,k,j)*frc_urb2d(i,j)
      b_t_bep(i,k,j) = b_t_bep(i,k,j)*frc_urb2d(i,j)
      b_q_bep(i,k,j) = b_q_bep(i,k,j)*frc_urb2d(i,j)
      b_e_bep(i,k,j) = b_e_bep(i,k,j)*frc_urb2d(i,j)
      HFX_URB(I,J)   = HFX_URB(I,J) + B_T_BEP(I,K,J)*RHO(I,K,J)*CP*DZ8W(I,K,J)*VL_BEP(I,K,J)
      QFX_URB(I,J)   = QFX_URB(I,J) + B_Q_BEP(I,K,J)*DZ8W(I,K,J)*VL_BEP(I,K,J)
      UMOM_URB(I,J)  = UMOM_URB(I,J)+ (A_U_BEP(I,K,J)*U_PHY(I,K,J)+B_U_BEP(I,K,J))*DZ8W(I,K,J)*VL_BEP(I,K,J)
      VMOM_URB(I,J)  = VMOM_URB(I,J)+ (A_V_BEP(I,K,J)*V_PHY(I,K,J)+B_V_BEP(I,K,J))*DZ8W(I,K,J)*VL_BEP(I,K,J)
      vl_bep(i,k,j)  = (1.-frc_urb2d(i,j)) + vl_bep(i,k,j)*frc_urb2d(i,j)
      sf_bep(i,k,j)  = (1.-frc_urb2d(i,j)) + sf_bep(i,k,j)*frc_urb2d(i,j)
    end do

    a_u_bep(i,1,j)   = (1.-frc_urb2d(i,j))*(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/   &
                          ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+a_u_bep(i,1,j)

    a_v_bep(i,1,j)   = (1.-frc_urb2d(i,j))*(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/   &
                          ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+a_v_bep(i,1,j)

    b_t_bep(i,1,j)   = (1.-frc_urb2d(i,j))*hfx_rural(i,j)/dz8w(i,1,j)/rho(i,1,j)/CP+ & 
                           b_t_bep(i,1,j)

    b_q_bep(i,1,j)   = (1.-frc_urb2d(i,j))*qfx_rural(i,j)/dz8w(i,1,j)/rho(i,1,j)+b_q_bep(i,1,j)

    umom             = (1.-frc_urb2d(i,j))*ust(i,j)*ust(i,j)*u_phy(i,1,j)/               &
                         ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+umom_urb(i,j)

    vmom             = (1.-frc_urb2d(i,j))*ust(i,j)*ust(i,j)*v_phy(i,1,j)/               &
                         ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+vmom_urb(i,j)
    sf_bep(i,1,j)    = 1.



  IF (FRC_URB2D(I,J).GT.0.) THEN
    rl_up_rural   = -emiss_rural(i,j)*sigma_sb*(tsk_rural(i,j)**4.)-(1.-emiss_rural(i,j))*glw(i,j)
    rl_up_tot     = (1.-frc_urb2d(i,j))*rl_up_rural     + frc_urb2d(i,j)*rl_up_urb(i,j)
    emiss(i,j)    = (1.-frc_urb2d(i,j))*emiss_rural(i,j)+ frc_urb2d(i,j)*emiss_urb(i,j)
    ts_urb2d(i,j) = (max(0.,(-rl_up_urb(i,j)-(1.-emiss_urb(i,j))*glw(i,j))/emiss_urb(i,j)/sigma_sb))**0.25
    tsk(i,j)      = (max(0., (-1.*rl_up_tot-(1.-emiss(i,j))*glw(i,j) )/emiss(i,j)/sigma_sb))**.25
    rs_abs_tot    = (1.-frc_urb2d(i,j))*swdown(i,j)*(1.-albedo(i,j))+frc_urb2d(i,j)*rs_abs_urb(i,j)

    if(swdown(i,j) > 0.)then
      albedo(i,j) = 1.-rs_abs_tot/swdown(i,j)
    else
      albedo(i,j) = alb_rural(i,j)
    endif



    grdflx(i,j)   = (1.-frc_urb2d(i,j))*grdflx_rural(i,j)+ frc_urb2d(i,j)*grdflx_urb(i,j)
    qfx(i,j)      = (1.-frc_urb2d(i,j))*qfx_rural(i,j)   + qfx_urb(i,j)
    lh(i,j)       = qfx(i,j)*xlv
    hfx(i,j)      = hfx_urb(i,j)                         + (1-frc_urb2d(i,j))*hfx_rural(i,j)      
    sh_urb2d(i,j) = hfx_urb(i,j)/frc_urb2d(i,j)
    lh_urb2d(i,j) = qfx_urb(i,j)*xlv/frc_urb2d(i,j)
    g_urb2d(i,j)  = grdflx_urb(i,j)
    rn_urb2d(i,j) = rs_abs_urb(i,j)+emiss_urb(i,j)*glw(i,j)-rl_up_urb(i,j)
    ust(i,j)      = (umom**2.+vmom**2.)**.25

  ELSE

    sh_urb2d(i,j)    = 0.
    lh_urb2d(i,j)    = 0.
    g_urb2d(i,j)     = 0.
    rn_urb2d(i,j)    = 0.

  ENDIF

  enddo 
  enddo 

ENDIF 






END SUBROUTINE noahmp_urban



END MODULE module_sf_noahmpdrv
