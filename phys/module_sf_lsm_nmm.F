!WRF:MODEL_LAYER:PHYSICS
!
MODULE MODULE_SF_LSM_NMM

USE MODULE_MPP
USE MODULE_MODEL_CONSTANTS

  REAL, SAVE    :: SCFX(30)

  INTEGER, SAVE :: ISEASON
  CHARACTER*256 :: errmess
 
CONTAINS

!-----------------------------------------------------------------------
      SUBROUTINE NMMLSM(DZ8W,QV3D,P8W3D,RHO3D,                          &
     &               T3D,TH3D,TSK,CHS,                                  &
     &               HFX,QFX,QGH,GSW,GLW,ELFLX,RMOL,                    & ! added for WRF CHEM
     &               SMSTAV,SMSTOT,SFCRUNOFF,                           &
     &               UDRUNOFF,IVGTYP,ISLTYP,VEGFRA,SFCEVP,POTEVP,       &
     &               GRDFLX,SFCEXC,ACSNOW,ACSNOM,SNOPCX,                &
     &               ALBSF,TMN,XLAND,XICE,QZ0,                          &
     &               TH2,Q2,SNOWC,CHS2,QSFC,TBOT,CHKLOWQ,RAINBL,        &
     &               NUM_SOIL_LAYERS,DT,DZS,ITIMESTEP,                  &
     &               SMOIS,TSLB,SNOW,CANWAT,CPM,ROVCP,SR,               & 
     &               ALB,SNOALB,SMLIQ,SNOWH,                            &
     &               IDS,IDE, JDS,JDE, KDS,KDE,                         &
     &               IMS,IME, JMS,JME, KMS,KME,                         &
     &               ITS,ITE, JTS,JTE, KTS,KTE                     )
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    IMPLICIT NONE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-- DZ8W        thickness of layers (m)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P8W3D       3D pressure on layer interfaces (Pa)
!-- FLHC        exchange coefficient for heat (m/s)
!-- FLQC        exchange coefficient for moisture (m/s)
!-- PSFC        surface pressure (Pa)
!-- XLAND       land mask (1 for land, 2 for water)
!-- TMN         soil temperature at lower boundary (K)
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- TSK         surface temperature (K)
!-- GSW         NET downward short wave flux at ground surface (W/m^2)
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- ELFLX       actual latent heat flux (w m-2: positive, if up from surface)
!-- SFCEVP      accumulated surface evaporation (W/m^2)
!-- POTEVP      accumulated potential evaporation (W/m^2)
!-- CAPG        heat capacity for soil (J/K/m^3)
!-- THC         thermal inertia (Cal/cm/K/s^0.5)
!-- TBOT        bottom soil temperature (local yearly-mean sfc air temperature)
!-- SNOWC       flag indicating snow coverage (1 for snow cover)
!-- EMISS       surface emissivity (between 0 and 1)
!-- DELTSM      time step (second)
!-- ROVCP       R/CP
!-- SR          fraction of frozen precip (0.0 to 1.0)
!-- XLV         latent heat of melting (J/kg)
!-- DTMIN       time step (minute)
!-- IFSNOW      ifsnow=1 for snow-cover effects
!-- SVP1        constant for saturation vapor pressure (kPa)
!-- SVP2        constant for saturation vapor pressure (dimensionless)
!-- SVP3        constant for saturation vapor pressure (K)
!-- SVPT0       constant for saturation vapor pressure (K)
!-- EP1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
!-- EP2         constant for specific humidity calculation
!               (R_d/R_v) (dimensionless)
!-- KARMAN      Von Karman constant
!-- EOMEG       angular velocity of earth's rotation (rad/s)
!-- STBOLT      Stefan-Boltzmann constant (W/m^2/K^4)
!-- STEM        soil temperature in 5-layer model
!-- ZS          depths of centers of soil layers
!-- DZS         thicknesses of soil layers
!-- num_soil_layers   the number of soil layers
!-- ACSNOW      accumulated snowfall (water equivalent) (mm)
!-- ACSNOM      accumulated snowmelt (water equivalent) (mm)
!-- SNOPCX      snow phase change heat flux (W/m^2)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,                    &
     &                      IMS,IME,JMS,JME,KMS,KME,                    &
     &                      ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: NUM_SOIL_LAYERS,ITIMESTEP
!
      REAL,INTENT(IN) :: DT,ROVCP
!
      REAL,DIMENSION(IMS:IME,1:NUM_SOIL_LAYERS,JMS:JME),                &
     &     INTENT(INOUT) ::                                      SMOIS, & ! new
					                         SMLIQ, & ! new
                                                                 TSLB     ! 

      REAL,DIMENSION(1:NUM_SOIL_LAYERS),INTENT(IN) :: DZS
!
      REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT) ::                  &
     &                                                             TSK, & !was TGB (temperature)
     &                                                             HFX, &     
     &                                                             QFX, &     
     &                                                             QSFC,&     
     &                                                            SNOW, & !new
     &                                                           SNOWH, & !new
     &                                                             ALB, &
     &                                                          SNOALB, &
     &                                                           ALBSF, &
     &                                                           SNOWC, & 
     &                                                          CANWAT, & ! new
     &                                                          SMSTAV, &
     &                                                          SMSTOT, &
     &                                                       SFCRUNOFF, &
     &                                                        UDRUNOFF, &
     &                                                          SFCEVP, &
     &                                                          POTEVP, &
     &                                                          GRDFLX, &
     &                                                          ACSNOW, &
     &                                                          ACSNOM, &
     &                                                          SNOPCX, &
     &                                                              Q2, &
     &                                                             TH2, &
     &                                                          SFCEXC

      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::          IVGTYP, &
                                                                ISLTYP

      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::                TMN, &
                                                                 XLAND, &
                                                                  XICE, &
                                                                VEGFRA, &
                                                                   GSW, &
                                                                   GLW, &     
                                                                   QZ0, &
                                                                    SR    

      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) ::       QV3D, &
                                                                 P8W3D, &
                                                                 RHO3D, &
                                                                  TH3D, &
                                                                   T3D, &
                                                                  DZ8W

!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::             RAINBL
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::               CHS2, &
                                                                   CHS, &
                                                                   QGH, &
                                                                   CPM
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) ::              TBOT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) ::           CHKLOWQ, &
                                                                 ELFLX
! added for WRF-CHEM, 20041205, JM -- not used in this routine as yet
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) ::            RMOL

! LOCAL VARS

      REAL,DIMENSION(ITS:ITE) ::                                  QV1D, &
     &                                                             T1D, &
     &                                                            TH1D, &
     &                                                            ZA1D, &
     &                                                           P8W1D, &
     &                                                          PSFC1D, &
     &                                                           RHO1D, &
     &                                                          PREC1D
                                                                           
      INTEGER :: I,J
      REAL :: RATIOMX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      DO J=JTS,JTE

        DO I=ITS,ITE
          T1D(I)    = T3D(I,1,J)
          TH1D(I)   = TH3D(I,1,J)
!!!       QV1D(I)   = QV3D(I,1,J)
          RATIOMX   = QV3D(I,1,J)
          QV1D(I)   = RATIOMX/(1.+RATIOMX)
          P8W1D(I)  = (P8W3D(I,KTS+1,j)+P8W3D(i,KTS,j))*0.5
          PSFC1D(I) = P8W3D(I,1,J)
          ZA1D(I)   = 0.5*DZ8W(I,1,J) 
          RHO1D(I)  = RHO3D(I,1,J)
          PREC1D(I) = RAINBL(I,J)/DT
        ENDDO

!FLHC = SFCEXC
    
!-----------------------------------------------------------------------
        CALL SURFCE(J,ZA1D,QV1D,P8W1D,PSFC1D,RHO1D,T1D,TH1D,TSK,        &
                    CHS(IMS,J),PREC1D,HFX,QFX,QGH(IMS,J),GSW,GLW,       &
                    SMSTAV,SMSTOT,SFCRUNOFF,                            &
                    UDRUNOFF,IVGTYP,ISLTYP,VEGFRA,SFCEVP,POTEVP,GRDFLX, &
                    ELFLX,SFCEXC,ACSNOW,ACSNOM,SNOPCX,                  &
                    ALBSF,TMN,XLAND,XICE,QZ0,                           &
                    TH2,Q2,SNOWC,CHS2(IMS,J),QSFC,TBOT,CHKLOWQ,         &
                    NUM_SOIL_LAYERS,DT,DZS,ITIMESTEP,                   &
                    SMOIS,TSLB,SNOW,CANWAT,CPM(IMS,J),ROVCP,SR,         & 
                    ALB,SNOALB,SMLIQ,SNOWH,                             &
                    IMS,IME,JMS,JME,KMS,KME,                            &
                    ITS,ITE,JTS,JTE,KTS,KTE                            ) 
!
      ENDDO

   END SUBROUTINE NMMLSM

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   SUBROUTINE SURFCE(J,ZA,QV,P8W,PSFC,RHO,T,TH,TSK,CHS,PREC,HFX,QFX,   &
                     QGH,GSW,GLW,SMSTAV,SMSTOT,SFCRUNOFF,UDRUNOFF,     &
                     IVGTYP,ISLTYP,VEGFRA,SFCEVP,POTEVP,GRDFLX,        &
                     ELFLX,SFCEXC,ACSNOW,ACSNOM,SNOPCX,                &
                     ALBSF,TMN,XLAND,XICE,QZ0,                         &
                     TH2,Q2,SNOWC,CHS2,QSFC,TBOT,CHKLOWQ,              &
                     NUM_SOIL_LAYERS,DT,DZS,ITIMESTEP,                 &
                     SMOIS,TSLB,SNOW,CANWAT,CPM,ROVCP,SR,              & 
                     ALB,SNOALB,SMLIQ,SNOWH,                           &
                     IMS,IME,JMS,JME,KMS,KME,                          &
                     ITS,ITE,JTS,JTE,KTS,KTE                           ) 
!------------------------------------------------------------------------     
      IMPLICIT NONE                                                     
!------------------------------------------------------------------------     
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                .      .    .                                          
! SUBPROGRAM:    SURFCE      CALCULATE SURFACE CONDITIONS               
!   PRGRMMR: F. CHEN         DATE: 97-12-06                             
!                                                                       
! ABSTRACT:                                                             
!   THIS ROUTINE IS THE DRIVER FOR COMPUTATION OF GROUND CONDITIONS     
!   BY USING A LAND SURFACE MODEL (LSM).                                
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   97-12-06  CHEN - ORIGINATOR                                         
!                                                                       
! REFERENCES:                                                           
!   PAN AND MAHRT (1987) BOUN. LAYER METEOR.                            
!   CHEN ET AL. (1996)  J. GEOPHYS. RES.                                
!   CHEN ET AL. (1997)  BOUN. LAYER METEOR.                             
!   CHEN and Dudhia (2000)  Mon. Wea. Rev. 
!                                                                       
!   SUBPROGRAMS CALLED:                                                 
!     SFLX                                                              
!                                                                       
!     SET LOCAL PARAMETERS.                                             
!----------------------------------------------------------------------
   INTEGER,  INTENT(IN   )   ::           IMS,IME, JMS,JME, KMS,KME,  &
                                          ITS,ITE, JTS,JTE, KTS,KTE,  &
                                          J,ITIMESTEP      

   INTEGER , INTENT(IN)      ::           NUM_SOIL_LAYERS

   REAL,     INTENT(IN   )   ::           DT,ROVCP

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::DZS

                                                 
   REAL, PARAMETER  :: PQ0=379.90516
   REAL, PARAMETER  :: TRESH=.95E0,A2=17.2693882,A3=273.16,A4=35.86,  &
                       T0=273.16E0,T1=274.16E0,ROW=1.E3,              &
                       ELWV=2.50E6,ELIV=XLS,ELIW=XLF,                 &
                       A23M4=A2*(A3-A4), RLIVWV=ELIV/ELWV,            &
                       ROWLIW=ROW*ELIW,ROWLIV=ROW*ELIV,CAPA=R_D/CP

   INTEGER,  PARAMETER  :: NROOT=3
!                                                                       
   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ),       &
             INTENT(INOUT)   ::                          SMOIS,       & ! new
						         SMLIQ,       & ! new
                                                         TSLB           ! new  !STEMP


   REAL,    DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(INOUT)    ::                                  TSK, & !was TGB (temperature)
						                 HFX, & !new
						                 QFX, & !new
						                 QSFC,& !new
						                SNOW, & !new
						               SNOWH, & !new
						 	         ALB, &
						 	      SNOALB, &
						 	       ALBSF, &
                                                               SNOWC, & 
                                                              CANWAT, & ! new
                                                              SMSTAV, &
                                                              SMSTOT, &
                                                           SFCRUNOFF, &
                                                            UDRUNOFF, &
                                                              SFCEVP, &
                                                              POTEVP, &
                                                              GRDFLX, &
                                                              ACSNOW, &
                                                              ACSNOM, &
                                                              SNOPCX

   INTEGER, DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(IN   )    ::                               IVGTYP, &
                                                              ISLTYP

   REAL,    DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(IN   )    ::                                  TMN, &
                                                               XLAND, &
                                                                XICE, &
                                                              VEGFRA, &
                                                                 GSW, &
                                                                 GLW, &
                                                                 QZ0, &
                                                                  SR

   REAL,    DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(INOUT)    ::                                   Q2, &
							         TH2, &
                                                              SFCEXC

   REAL,    DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(OUT)    ::                                   TBOT


   REAL,    DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(OUT)    ::                                CHKLOWQ, &
                                                               ELFLX

   REAL,    DIMENSION( ims:ime )                                    , &
            INTENT(IN   )    ::                                  QGH, &
                                                                 CHS, &
                                                                 CPM, &
                                                                CHS2

! MODULE-LOCAL VARIABLES, DEFINED IN  SUBROUTINE LSM
   REAL,    DIMENSION( its:ite )                                    , &
            INTENT(IN   )    ::                                   ZA, &
                                                                  TH, &
                                                                  QV, &
                                                                   T, &
                                                                 p8w, &
                                                                PSFC, &
                                                                 rho, &
                                                                PREC    ! one time step in mm

   REAL,    DIMENSION( its:ite )   ::                          TGDSA 

! LOCAL VARS

    REAL, DIMENSION(1:num_soil_layers) :: SMLIQ1D,SMOIS1D,STEMP1D

!---------------------------------------------------------------------- 
!***  DECLARATIONS FOR IMPLICIT NONE                                    
 
    REAL :: APELM,APES,FDTLIW,FDTW,Q2SAT,Z,FK,SOLDN,SFCTMP,SFCTH2,    &
            SFCPRS,PRCP,Q2K,DQSDTK,SATFLG,TBOTK,CHK,VGFRCK,T1K,LWDN,  &
            CMCK,Q2M,SNODPK,PLFLX,HFLX,GFLX,RNOF1K,                   &
            RNOF2K,Q1K,SMELTK,SOILQW,SOILQM,T2K,PRESK,CHFF,STIMESTEP, &
            ALB1D,SNOALB1D,SNOWH1D,ALBSF1D,SOLNET,FFROZP,             &
            DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,DUM7

    INTEGER :: I,K,NS,ICE,IVGTPK,ISLTPK,ISPTPK,NOOUT,NSOIL,LZ

!---------------------------------------------------------------------- 
!***********************************************************************
!                         START SURFCE HERE                             
!***                                                                    
!***  SET CONSTANTS CALCULATED HERE FOR CLARITY.                        
!***                                                                    
      FDTLIW=DT/ROWLIW                                              
!      FDTLIV=DT/ROWLIV                                             
      FDTW=DT/(XLV*RHOWATER)
!***                                                                    
!***  SET LSM CONSTANTS AND TIME INDEPENDENT VARIABLES                  
!***  INITIALIZE LSM HISTORICAL VARIABLES                               
!***                                                                    
!-----------------------------------------------------------------------

      NSOIL=num_soil_layers

      IF(ITIMESTEP.EQ.1)THEN                                                 
        DO 50 I=its,ite
!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS                   
          IF((XLAND(I,J)-1.5).GE.0.)THEN                                
! check sea-ice point                                                   
            IF(XICE(I,J).EQ.1.)PRINT*,' sea-ice at water point, I=',I,  &
              'J=',J
!***   Open Water Case                                                  
            SMSTAV(I,J)=1.0                                             
            SMSTOT(I,J)=1.0                                             
            DO NS=1,NSOIL                                               
              SMOIS(I,NS,J)=1.0                                          
              TSLB(I,NS,J)=273.16                                          !STEMP
            ENDDO                                                       
          ELSE                                                          
            IF(XICE(I,J).EQ.1.)THEN                                     
!***        SEA-ICE CASE                                                
              SMSTAV(I,J)=1.0                                           
              SMSTOT(I,J)=1.0                                           
              DO NS=1,NSOIL                                             
                SMOIS(I,NS,J)=1.0                                        
              ENDDO                                                     
            ENDIF                                                       
          ENDIF                                                         
!                                                                       
   50   CONTINUE                                                        
      ENDIF                                                             
!-----------------------------------------------------------------------
      DO 100 I=its,ite                                                    
!       SFCPRS=(A(KL)*PSB(I,J)+PTOP+PP3D(I,J,KL)*0.001)*1.E3          
        SFCPRS=p8w(I)  !Pressure in middle of lowest layer
        Q2SAT=QGH(I)                                                  
!       CHKLOWQ(I,J)=1.
        CHFF=CHS(I)*RHO(I)*CPM(I)
!CHK*RHO*CP                                                             
! TGDSA: potential T
        TGDSA(I)=TSK(I,J)*(1.E5/SFCPRS)**ROVCP 
!
!***  CHECK FOR SATURATION AT THE LOWEST MODEL LEVEL                    
!
        Q2K=QV(I)
        APES=(1.E5/PSFC(I))**CAPA
!
        IF((Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN                                  
          SATFLG=0.                                                   
          CHKLOWQ(I,J)=0.
        ELSE                                                          
          SATFLG=1.0                                                  
          CHKLOWQ(I,J)=1.
        ENDIF                                                         
!
        TBOT(I,J)=273.16
!***                                                                    
!***  LOADING AND UNLOADING MM5/LSM LAND SOIL VARIABLES                 
!***                                                                    
        IF((XLAND(I,J)-1.5).GE.0.)THEN                                  
!*** Water                                                              
          HFX(I,J)=HFX(I,J)/APES
          QFX(I,J)=QFX(I,J)*SATFLG
          SFCEVP(I,J)=SFCEVP(I,J)+QFX(I,J)*DT                       
        ELSE                                                            
!*** LAND OR SEA-ICE                                                    
!ATEC          ICE=INT(XICE(I,J)+0.3)                                   
          IF (XICE(I,J) .GT. 0.5) THEN                                  
             ICE=1                                                      
          ELSE                                                          
             ICE=0                                                      
          ENDIF                                                         
!
          Q2K=MIN(QV(I),Q2SAT)
          Z=ZA(I)                                                    
!          FK=GSW(I,J)+GLW(I,J)                                          
          LWDN=GLW(I,J)
!
!***  GSW is net downward shortwave
!
!          SOLNET=GSW(I,J)
!
!***  GSW is total downward shortwave
!
          SOLDN=GSW(I,J)
!
!***  Simple use of albedo to determine total incoming solar shortwave SOLDN
!***  (no solar zenith angle correction)
!
!          SOLDN=SOLNET/(1.-ALB(I,J))                                  
          SOLNET=SOLDN*(1.-ALB(I,J))
!
          ALBSF1D=ALBSF(I,J)
          SNOALB1D=SNOALB(I,J)
          SFCTMP=T(I)                                               
!!!       SFCTH2=SFCTMP+(0.0097545*Z)                                   
          APELM=(1.E5/SFCPRS)**CAPA
          SFCTH2=SFCTMP*APELM
          SFCTH2=SFCTH2/APES
          PRCP=PREC(I)
!!!       Q2K=QV(I)                                                  
!!!       Q2SAT=PQ0/SFCPRS*EXP(A2*(SFCTMP-A3)/(SFCTMP-A4))              
          DQSDTK=Q2SAT*A23M4/(SFCTMP-A4)**2                             
          IF(ICE.EQ.0)THEN                                              
            TBOTK=TMN(I,J)                                              
          ELSE                                                          
            TBOTK=271.16                                                
          ENDIF                                                         
          CHK=CHS(I)                                                    
          IVGTPK=IVGTYP(I,J)                                            
          IF(IVGTPK.EQ.0)IVGTPK=13
          ISLTPK=ISLTYP(I,J)                                            
          IF(ISLTPK.EQ.0)ISLTPK=9
! hardwire slope type (ISPTPK)=1
          ISPTPK=1
          VGFRCK=VEGFRA(I,J)/100.                                       
          IF(IVGTPK.EQ.25) VGFRCK=0.0001
          IF(ISLTPK.EQ.14.AND.XICE(I,J).EQ.0.)THEN                      
         PRINT*,' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'          
         PRINT*,i,j,'RESET SOIL in surfce.F'                      
!           ISLTYP(I,J)=7                                               
            ISLTPK=7                                                    
          ENDIF                                                         
          T1K=TSK(I,J)
          CMCK=CANWAT(I,J)                                                
!*** convert snow depth from mm to meter                                
          SNODPK=SNOW(I,J)*0.001                                        
          SNOWH1D=SNOWH(I,J)*0.001                                        
!                                                                       
!*** fraction of frozen precip
!                                                                       
          FFROZP=SR(I,J)
!
          DO 70 NS=1,NSOIL                                              
            SMOIS1D(NS)=SMOIS(I,NS,J)                                       
            SMLIQ1D(NS)=SMLIQ(I,NS,J)                                       
            STEMP1D(NS)=TSLB(I,NS,J)                                          !STEMP
   70     CONTINUE                                                      

!                                                                       
!        print*,'BF SFLX','ISLTPK',ISLTPK,'IVGTPK=',IVGTPK,'SMOIS1D',&
!              SMOIS1D,'STEMP1',STEMP1D,'VGFRCK',VGFRCK
!-----------------------------------------------------------------------
! old WRF call to SFLX
!         CALL SFLX(ICE,SATFLG,DT,Z,NSOIL,NROOT,DZS,FK,SOLDN,SFCPRS,    &
!              PRCP,SFCTMP,SFCTH2,Q2K,Q2SAT,DQSDTK,TBOTK,CHK,CHFF,      &
!              IVGTPK,ISLTPK,VGFRCK,PLFLX,ELFLX,HFLX,GFLX,RNOF1K,RNOF2K,&
!              Q1K,SMELTK,T1K,CMCK,SMOIS1D,STEMP1D,SNODPK,SOILQW,SOILQM)      
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Ek 12 June 2002 - NEW CALL SFLX
! ops Eta call to SFLX ...'tailor' this to WRF
!        CALL SFLX
!     I    (ICE,DTK,Z,NSOIL,SLDPTH,
!     I    LWDN,SOLDN,SFCPRS,PRCP,SFCTMP,SFCTH2,Q2K,SFCSPD,Q2SAT,DQSDTK,
!     I    IVGTPK,ISLTPK,ISPTPK,
!     I    VGFRCK,PTU,TBOT,ALB,SNOALB,
!     2    CMCK,T1K,STCK,SMCK,SH2OK,SNOWH,SNODPK,ALB2D,CHK,CMK,
!     O    PLFLX,ELFLX,HFLX,GFLX,RNOF1K,RNOF2K,Q1K,SMELTK,
!     O    SOILQW,SOILQM,DUM1,DUM2,DUM3,DUM4)
!-----------------------------------------------------------------------
        CALL SFLX                                                       &
          (FFROZP,ICE,DT,Z,NSOIL,DZS,                                   &
          LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,SFCTH2,Q2K,DUM5,Q2SAT,   &
          DQSDTK,IVGTPK,ISLTPK,ISPTPK,                                  &
          VGFRCK,DUM6,TBOTK,ALBSF1D,SNOALB1D,                           &
          CMCK,T1K,STEMP1D,SMOIS1D,SMLIQ1D,SNOWH1D,SNODPK,ALB1D,CHK,DUM7, &
          PLFLX,ELFLX(I,J),HFLX,GFLX,RNOF1K,RNOF2K,Q1K,SMELTK,          &
          SOILQW,SOILQM,DUM1,DUM2,DUM3,DUM4,I,J)
!-----------------------------------------------------------------------
!***  DIAGNOSTICS                                                       
!        Convert the water unit into mm                                 
          SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RNOF1K*DT*1000.0                  
          UDRUNOFF(I,J)=UDRUNOFF(I,J)+RNOF2K*DT*1000.0                  
          SMSTAV(I,J)=SOILQW                                            

!mp
	if (abs(SMSTAV(I,J)) .lt. 3.5) then
	else
	write(errmess,*) 'bad SMSTAV: ', I,J,SMSTAV(I,J)
        CALL wrf_message( errmess )
	endif
!mp	

          SMSTOT(I,J)=SOILQM*1000.                                      
          SFCEXC(I,J)=CHK                                               
!       IF(SNOB(I,J).GT.0..OR.SICE(I,J).GT.0.)THEN                      
!         QFC1(I,J)=QFC1(I,J)*RLIVWV                                    
!       ENDIF                                                           
          IF(FFROZP.GT.0.5)THEN
            ACSNOW(I,J)=ACSNOW(I,J)+PREC(I)*DT                     
          ENDIF                                                         
          IF(SNOW(I,J).GT.0.)THEN                                       
            ACSNOM(I,J)=ACSNOM(I,J)+SMELTK*1000.                    
            SNOPCX(I,J)=SNOPCX(I,J)-SMELTK/FDTLIW                       
          ENDIF                                                         
        POTEVP(I,J)=POTEVP(I,J)+PLFLX*FDTW                              
!       POTFLX(I,J)=POTFLX(I,J)-PLFLX                                   
!***  WRF LOWER BOUNDARY CONDITIONS                                     
          GRDFLX(I,J)=GFLX                                              
          HFX(I,J)=HFLX                                                 
          QFX(I,J)=ELFLX(I,J)/ELWV                                           
          SFCEVP(I,J)=SFCEVP(I,J)+QFX(I,J)*DT                       
          TSK(I,J)=T1K
          T2K=T1K-HFX(I,J)/(RHO(I)*CPM(I)*CHS2(I))
          TH2(I,J)=T2K*(1.E5/SFCPRS)**ROVCP                                  
          Q2M=Q1K-QFX(I,J)/(RHO(I)*CHS2(I))                            
!!!!!!    Q2(I,J)=Q2M
!!!!!!    Q2(I,J)=Q2K
!        t2k=th2k/(1.E5/SFCPRS)**ROVCP                                  
!        QS(I,J)=Q1K                                                    
!!!      QSFC(I,J)=Q1K                                                    
!***  UPDATE STATE VARIABLES 
          SNOW(I,J)=SNODPK*1000.0                                       
          SNOWH(I,J)=SNOWH1D*1000.0                                       
          CANWAT(I,J)=CMCK                                                
          IF(SNOW(I,J).GT.1.0)THEN                                      
!           ALB(I,J)=0.01*ALBD(IVGTPK,ISEASON)*(1.+SCFX(IVGTPK))            
            SNOWC(I,J)=1.0                                              
          ELSE                                                          
!           ALB(I,J)=0.01*ALBD(IVGTPK,ISEASON)                              
            SNOWC(I,J)=0.0                                              
          ENDIF                                                         
! update albedo
          ALB(I,J)=ALB1D
! update bottom soil temperature
          TBOT(I,J)=TBOTK

          DO 80 NS=1,NSOIL                                              
           SMOIS(I,NS,J)=SMOIS1D(NS)                                       
           SMLIQ(I,NS,J)=SMLIQ1D(NS)                                       
           TSLB(I,NS,J)=STEMP1D(NS)                                        !  STEMP
   80     CONTINUE                                                      
        ENDIF                                                           
#if 0
        NOOUT=0                                                         

        IF((ITIMESTEP.EQ.1.OR.MOD(ITIMESTEP,1).EQ.0)  &
            .AND. I .EQ.29.AND.J.EQ.23) THEN                                             
!         print*, 'GLW',GLW(I,J),'GSW',GSW(I,J)
          print*, 'T2K',T2K,'T1K',T1K,'HFX',HFX(I,J),'RHO',RHO(I),'CPM',CPM(I), &
                'CHS2',CHS2(I),'soil T',STEMP1D,'soil m', SMOIS1D
!          print*,'Q2M',Q2M,'Q1K',Q1K,'QFX',QFX(I,J),'RHO',RHO(I),'CHS2',CHS2(I),'latent',ELFLX
        ENDIF

        IF(NOOUT.EQ.1)GOTO 100                                          
!     write output to 29                                                
        IF(ITIMESTEP.EQ.1.AND.I.EQ.1.AND.J.EQ.1) &
                                                            WRITE (29,*)&
          'itimestep ','   FK     ','  SOLDN   ','  SFCPR   ',          &
          '  SFCTMP  ','    Q2K   ','   TBOTK  ',          &
          '   CHK    ','  ELFLX   ','   HFLX   ','   GFLX   ',          &
          ' RNOF1K   ',' RNOF2K   ','    T1K   ','   CMCK   ',          &
          '  SMCK1   ','  SMCK2   ','  SMCK3   ','  SMCK4   ',          &
          '  STCK1   ','  STCK2   ','  STCK3   ','  STCK4   ',          &
          ' SNODPK   ','      T2   ',                                   &
          '     Q2   ',' SMSTOT   ',' SFCEVP   ', ' RAIN'                        
        IF((ITIMESTEP.EQ.1.OR.MOD(ITIMESTEP,1).EQ.0)  &
            .AND. I .EQ.29.AND.J.EQ.23) THEN                                             
        print *,'outputting at itimestep =', itimestep
          STIMESTEP=FLOAT(itimestep)
          WRITE (29,1029)STIMESTEP,FK,SOLDN,SFCPRS/100.,SFCTMP,1000.*       &
                         Q2K,TBOTK,1000.*CHK,ELFLX(i,j),HFLX,GFLX,SFCRUNOFF(I,J)&
                         ,UDRUNOFF(I,J),T1K,CMCK,SMOIS1D,STEMP1D,SNODPK,        &
!                       ,UDRUNOFF(I,J),T1K,CMCK,SMOIS1D(3),SMOIS1D(7),SMOIS1D(11),&
!                        SMOIS1D(14),STEMP1D(3), STEMP1D(7),STEMP1D(11), &
!                        STEMP1D(14), SNODPK,        &
                         T2K,1000.*Q2M,SMSTOT(I,J),SFCEVP(I,J),PRCP
 1029     FORMAT (29F10.4)                                              
!         IF(ITIMESTEP.EQ.0)WRITE (39,*)'   P      ','   T      ',      &
!           '   TH     ','   Q      ','   U      ','   V      ',        &
!           '   QC     '                                                
!         WRITE (39,1039)itimestep
!         DO K=kts,kte
!           WRITE (39,1039)PRESK,TX(I,K),THX(I,K),1000.*QX(I,K),UX(I,K),&
!                          VX(I,K),1000.*QCX(I,K)                       
 1039       FORMAT (7F10.5)                                             
!         ENDDO                                                         
        ENDIF                                                           
!                                                                       
#endif
  100 CONTINUE                                                          
!                                                                       
!-----------------------------------------------------------------------
  END SUBROUTINE SURFCE
!-----------------------------------------------------------------------

      SUBROUTINE SFLX (                                                 &
       FFROZP,ICE,DT,ZLVL,NSOIL,SLDPTH,                                 &
       LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,TH2,Q2,SFCSPD,Q2SAT,        &
       DQSDT2,VEGTYP,SOILTYP,SLOPETYP,                                  &
       SHDFAC,PTU,TBOT,ALB,SNOALB,                                      &
       CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,                    &
       ETP,ETA,SHEAT,SSOIL,RUNOFF1,RUNOFF2,Q1,SNOMLT,                   &
       SOILW,SOILM,SMCWLT,SMCDRY,SMCREF,SMCMAX,I,J)
! ----------------------------------------------------------------------
!     &  ETA,SHEAT,                                                      &
! ----------------------------------------------------------------------
! OUTPUTS, DIAGNOSTICS, PARAMETERS BELOW GENERALLY NOT NECESSARY WHEN
! COUPLED WITH E.G. A NWP MODEL (SUCH AS THE NOAA/NWS/NCEP MESOSCALE ETA
! MODEL).  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES. 
! ----------------------------------------------------------------------
!     &  EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                                  &
!     &  BETA,ETP,SSOIL,                                                 &
!     &  FLX1,FLX2,FLX3,                                                 &
!     &  SNOMLT,SNCOVR,                                                  &
!     &  RUNOFF1,RUNOFF2,RUNOFF3,                                        &
!     &  RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,                            &
!     &  SOILW,SOILM,                                                    &
!     &  SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,I,J)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE SFLX - VERSION 2.7 - June 2nd 2003
! ----------------------------------------------------------------------
! SUB-DRIVER FOR "NOAH/OSU LSM" FAMILY OF PHYSICS SUBROUTINES FOR A
! SOIL/VEG/SNOWPACK LAND-SURFACE MODEL TO UPDATE SOIL MOISTURE, SOIL
! ICE, SOIL TEMPERATURE, SKIN TEMPERATURE, SNOWPACK WATER CONTENT,
! SNOWDEPTH, AND ALL TERMS OF THE SURFACE ENERGY BALANCE AND SURFACE
! WATER BALANCE (EXCLUDING INPUT ATMOSPHERIC FORCINGS OF DOWNWARD
! RADIATION AND PRECIP)
! ----------------------------------------------------------------------
! SFLX ARGUMENT LIST KEY:
! ----------------------------------------------------------------------
!  C  CONFIGURATION INFORMATION
!  F  FORCING DATA
!  I  OTHER (INPUT) FORCING DATA
!  S  SURFACE CHARACTERISTICS
!  H  HISTORY (STATE) VARIABLES
!  O  OUTPUT VARIABLES
!  D  DIAGNOSTIC OUTPUT
! ----------------------------------------------------------------------
! 1. CONFIGURATION INFORMATION (C):
! ----------------------------------------------------------------------
!   ICE	       SEA-ICE FLAG  (=1: SEA-ICE, =0: LAND)
!   DT	       TIMESTEP (SEC) (DT SHOULD NOT EXCEED 3600 SECS, RECOMMEND
!                1800 SECS OR LESS)
!   ZLVL       HEIGHT (M) ABOVE GROUND OF ATMOSPHERIC FORCING VARIABLES
!   NSOIL      NUMBER OF SOIL LAYERS (AT LEAST 2, AND NOT GREATER THAN
!                PARAMETER NSOLD SET BELOW)
!   SLDPTH     THE THICKNESS OF EACH SOIL LAYER (M)
! ----------------------------------------------------------------------
! 2. FORCING DATA (F):
! ----------------------------------------------------------------------
!   LWDN       LW DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET LONGWAVE)
!   SOLDN      SOLAR DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET SOLAR)
!   SFCPRS     PRESSURE AT HEIGHT ZLVL ABOVE GROUND (PASCALS)
!   PRCP       PRECIP RATE (KG M-2 S-1) (NOTE, THIS IS A RATE)
!   SFCTMP     AIR TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
!   TH2        AIR POTENTIAL TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
!   Q2         MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
! ----------------------------------------------------------------------
! 3. OTHER FORCING (INPUT) DATA (I):
! ----------------------------------------------------------------------
!   SFCSPD     WIND SPEED (M S-1) AT HEIGHT ZLVL ABOVE GROUND
!   Q2SAT      SAT MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
!   DQSDT2     SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=SFCTMP
!                (KG KG-1 K-1)
! ----------------------------------------------------------------------
! 4. CANOPY/SOIL CHARACTERISTICS (S):
! ----------------------------------------------------------------------
!   VEGTYP     VEGETATION TYPE (INTEGER INDEX)
!   SOILTYP    SOIL TYPE (INTEGER INDEX)
!   SLOPETYP   CLASS OF SFC SLOPE (INTEGER INDEX)
!   SHDFAC     AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!                (FRACTION= 0.0-1.0)
!   SHDMIN     MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!                (FRACTION= 0.0-1.0) <= SHDFAC
!   PTU        PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)
!                (NOT YET USED, BUT PASSED TO REDPRM FOR FUTURE USE IN
!                VEG PARMS)
!   ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION), FOR JULIAN
!                DAY OF YEAR (USUALLY FROM TEMPORAL INTERPOLATION OF
!                MONTHLY MEAN VALUES' CALLING PROG MAY OR MAY NOT
!                INCLUDE DIURNAL SUN ANGLE EFFECT)
!   SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW (E.G. FROM
!                ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)
!   TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR
!                TEMPERATURE)
! ----------------------------------------------------------------------
! 5. HISTORY (STATE) VARIABLES (H):
! ----------------------------------------------------------------------
!  CMC         CANOPY MOISTURE CONTENT (M)
!  T1          GROUND/CANOPY/SNOWPACK) EFFECTIVE SKIN TEMPERATURE (K)
!  STC(NSOIL)  SOIL TEMP (K)
!  SMC(NSOIL)  TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
!  SH2O(NSOIL) UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
!                NOTE: FROZEN SOIL MOISTURE = SMC - SH2O
!  SNOWH       ACTUAL SNOW DEPTH (M)
!  SNEQV       LIQUID WATER-EQUIVALENT SNOW DEPTH (M)
!                NOTE: SNOW DENSITY = SNEQV/SNOWH
!  ALBEDO      SURFACE ALBEDO INCLUDING SNOW EFFECT (UNITLESS FRACTION)
!                =SNOW-FREE ALBEDO (ALB) WHEN SNEQV=0, OR
!                =FCT(MSNOALB,ALB,VEGTYP,SHDFAC,SHDMIN) WHEN SNEQV>0
!  CH          SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
!                (M S-1); NOTE: CH IS TECHNICALLY A CONDUCTANCE SINCE
!                IT HAS BEEN MULTIPLIED BY WIND SPEED.
!  CM          SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM (M S-1); NOTE:
!                CM IS TECHNICALLY A CONDUCTANCE SINCE IT HAS BEEN
!                MULTIPLIED BY WIND SPEED.  CM IS NOT NEEDED IN SFLX
! ----------------------------------------------------------------------
! 6. OUTPUT (O):
! ----------------------------------------------------------------------
! OUTPUT VARIABLES NECESSARY FOR A COUPLED NUMERICAL WEATHER PREDICTION
! MODEL, E.G. NOAA/NWS/NCEP MESOSCALE ETA MODEL.  FOR THIS APPLICATION,
! THE REMAINING OUTPUT/DIAGNOSTIC/PARAMETER BLOCKS BELOW ARE NOT
! NECESSARY.  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.
!   ETA        ACTUAL LATENT HEAT FLUX (W M-2: NEGATIVE, IF UP FROM
!	         SURFACE)
!   SHEAT      SENSIBLE HEAT FLUX (W M-2: NEGATIVE, IF UPWARD FROM
!	         SURFACE)
! ----------------------------------------------------------------------
!   EC         CANOPY WATER EVAPORATION (W M-2)
!   EDIR       DIRECT SOIL EVAPORATION (W M-2)
!   ET(NSOIL)  PLANT TRANSPIRATION FROM A PARTICULAR ROOT (SOIL) LAYER
!                 (W M-2)
!   ETT        TOTAL PLANT TRANSPIRATION (W M-2)
!   ESNOW      SUBLIMATION FROM SNOWPACK (W M-2)
!   DRIP       THROUGH-FALL OF PRECIP AND/OR DEW IN EXCESS OF CANOPY
!                WATER-HOLDING CAPACITY (M)
!   DEW        DEWFALL (OR FROSTFALL FOR T<273.15) (M)
! ----------------------------------------------------------------------
!   BETA       RATIO OF ACTUAL/POTENTIAL EVAP (DIMENSIONLESS)
!   ETP        POTENTIAL EVAPORATION (W M-2)
!   SSOIL      SOIL HEAT FLUX (W M-2: NEGATIVE IF DOWNWARD FROM SURFACE)
! ----------------------------------------------------------------------
!   FLX1       PRECIP-SNOW SFC (W M-2)
!   FLX2       FREEZING RAIN LATENT HEAT FLUX (W M-2)
!   FLX3       PHASE-CHANGE HEAT FLUX FROM SNOWMELT (W M-2)
! ----------------------------------------------------------------------
!   SNOMLT     SNOW MELT (M) (WATER EQUIVALENT)
!   SNCOVR     FRACTIONAL SNOW COVER (UNITLESS FRACTION, 0-1)
! ----------------------------------------------------------------------
!   RUNOFF1    SURFACE RUNOFF (M S-1), NOT INFILTRATING THE SURFACE
!   RUNOFF2    SUBSURFACE RUNOFF (M S-1), DRAINAGE OUT BOTTOM OF LAST
!                SOIL LAYER
!   RUNOFF3    NUMERICAL TRUNCTATION IN EXCESS OF POROSITY (SMCMAX)
!                FOR A GIVEN SOIL LAYER AT THE END OF A TIME STEP
! ----------------------------------------------------------------------
!   RC         CANOPY RESISTANCE (S M-1)
!   PC         PLANT COEFFICIENT (UNITLESS FRACTION, 0-1) WHERE PC*ETP
!                = ACTUAL TRANSPIRATION
!   XLAI       LEAF AREA INDEX (DIMENSIONLESS)
!   RSMIN      MINIMUM CANOPY RESISTANCE (S M-1)
!   RCS        INCOMING SOLAR RC FACTOR (DIMENSIONLESS)
!   RCT        AIR TEMPERATURE RC FACTOR (DIMENSIONLESS)
!   RCQ        ATMOS VAPOR PRESSURE DEFICIT RC FACTOR (DIMENSIONLESS)
!   RCSOIL     SOIL MOISTURE RC FACTOR (DIMENSIONLESS)
! ----------------------------------------------------------------------
! 7. DIAGNOSTIC OUTPUT (D):
! ----------------------------------------------------------------------
!   SOILW      AVAILABLE SOIL MOISTURE IN ROOT ZONE (UNITLESS FRACTION
!	         BETWEEN SMCWLT AND SMCMAX)
!   SOILM      TOTAL SOIL COLUMN MOISTURE CONTENT (FROZEN+UNFROZEN) (M) 
! ----------------------------------------------------------------------
! 8. PARAMETERS (P):
! ----------------------------------------------------------------------
!   SMCWLT     WILTING POINT (VOLUMETRIC)
!   SMCDRY     DRY SOIL MOISTURE THRESHOLD WHERE DIRECT EVAP FRM TOP
!                LAYER ENDS (VOLUMETRIC)
!   SMCREF     SOIL MOISTURE THRESHOLD WHERE TRANSPIRATION BEGINS TO
!                STRESS (VOLUMETRIC)
!   SMCMAX     POROSITY, I.E. SATURATED VALUE OF SOIL MOISTURE
!                (VOLUMETRIC)
!   NROOT      NUMBER OF ROOT LAYERS, A FUNCTION OF VEG TYPE, DETERMINED
!              IN SUBROUTINE REDPRM.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

! ----------------------------------------------------------------------
! DECLARATIONS - LOGICAL
! ----------------------------------------------------------------------
      LOGICAL FRZGRA
      LOGICAL SATURATED
      LOGICAL SNOWNG

! ----------------------------------------------------------------------
! DECLARATIONS - INTEGER
! ----------------------------------------------------------------------
      INTEGER ICE
      INTEGER K
      INTEGER KZ
      INTEGER NSOIL
      INTEGER NROOT
      INTEGER SLOPETYP
      INTEGER SOILTYP
      INTEGER VEGTYP
      INTEGER I
      INTEGER J

! ----------------------------------------------------------------------
! DECLARATIONS - REAL
! ----------------------------------------------------------------------
      REAL ALBEDO
      REAL ALB
      REAL BEXP
      REAL BETA
      REAL CFACTR
      REAL CH
      REAL CM
      REAL CMC
      REAL CMCMAX
      REAL CP
!      REAL CSNOW
      REAL CSOIL
      REAL CZIL
      REAL DEW
      REAL DF1
      REAL DF1H
      REAL DF1A
      REAL DKSAT
      REAL DT
      REAL DWSAT
      REAL DQSDT2
      REAL DSOIL
      REAL DTOT
      REAL DRIP
      REAL EC
      REAL EDIR
      REAL ESNOW
      REAL ET(NSOIL)
      REAL ETT
      REAL FRCSNO
      REAL FRCSOI
      REAL EPSCA
      REAL ETA
      REAL ETP
      REAL FDOWN
      REAL F1
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FXEXP
      REAL FRZX
      REAL SHEAT
      REAL HS
      REAL KDT
      REAL LWDN
      REAL LVH2O
      REAL PC
      REAL PRCP
      REAL PTU
      REAL PRCP1
      REAL PSISAT
      REAL Q2
      REAL Q2SAT
      REAL QUARTZ
      REAL R
      REAL RCH
      REAL REFKDT
      REAL RR
      REAL RTDIS(NSOLD)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RGL
      REAL RUNOFF3
      REAL RSMAX
      REAL RC
      REAL RSMIN
      REAL RCQ
      REAL RCS
      REAL RCSOIL
      REAL RCT
      REAL RSNOW
      REAL SNDENS
      REAL SNCOND 
      REAL SSOIL
      REAL SBETA
      REAL SFCPRS
      REAL SFCSPD
      REAL SFCTMP
      REAL SHDFAC
      REAL SHDMIN
      REAL SH2O(NSOIL)
      REAL SLDPTH(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL SMC(NSOIL)
      REAL SNEQV
      REAL SNCOVR
      REAL SNOWH
      REAL SN_NEW
      REAL SLOPE
      REAL SNUP
      REAL SALP
      REAL SNOALB
      REAL STC(NSOIL)
      REAL SNOMLT
      REAL SOLDN
      REAL SOILM
      REAL SOILW
      REAL SOILWM
      REAL SOILWW
      REAL T1
      REAL T1V
      REAL T24
      REAL T2V
      REAL TBOT
      REAL TH2
      REAL TH2V
      REAL TOPT
      REAL TFREEZ
      REAL TSNOW
      REAL XLAI
      REAL ZLVL
      REAL ZBOT
      REAL Z0
      REAL ZSOIL(NSOLD)

      REAL FFROZP
      REAL SOLNET
      REAL LSUBS

      REAL Q1

! ----------------------------------------------------------------------
! DECLARATIONS - PARAMETERS
! ----------------------------------------------------------------------
      PARAMETER(TFREEZ = 273.15)
      PARAMETER(LVH2O = 2.501E+6)
      PARAMETER(LSUBS = 2.83E+6)
      PARAMETER(R = 287.04)
      PARAMETER(CP = 1004.5)

! ----------------------------------------------------------------------
!   INITIALIZATION
! ----------------------------------------------------------------------
      RUNOFF1 = 0.0
      RUNOFF2 = 0.0
      RUNOFF3 = 0.0
      SNOMLT = 0.0

! ----------------------------------------------------------------------
!  THE VARIABLE "ICE" IS A FLAG DENOTING SEA-ICE CASE 
! ----------------------------------------------------------------------
      IF (ICE .EQ. 1) THEN

! ----------------------------------------------------------------------
! SEA-ICE LAYERS ARE EQUAL THICKNESS AND SUM TO 3 METERS
! ----------------------------------------------------------------------
        DO KZ = 1,NSOIL
          ZSOIL(KZ) = -3.*FLOAT(KZ)/FLOAT(NSOIL)
        END DO

      ELSE

! ----------------------------------------------------------------------
! CALCULATE DEPTH (NEGATIVE) BELOW GROUND FROM TOP SKIN SFC TO BOTTOM OF
!   EACH SOIL LAYER.  NOTE:  SIGN OF ZSOIL IS NEGATIVE (DENOTING BELOW
!   GROUND)
! ----------------------------------------------------------------------
        ZSOIL(1) = -SLDPTH(1)
        DO KZ = 2,NSOIL
          ZSOIL(KZ) = -SLDPTH(KZ)+ZSOIL(KZ-1)
        END DO

      ENDIF
         
! ----------------------------------------------------------------------
! NEXT IS CRUCIAL CALL TO SET THE LAND-SURFACE PARAMETERS, INCLUDING
! SOIL-TYPE AND VEG-TYPE DEPENDENT PARAMETERS.
! ----------------------------------------------------------------------
      CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,                             &
     &      	   CFACTR,CMCMAX,RSMAX,TOPT,REFKDT,KDT,SBETA,           &
     &      	   SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,PSISAT,SLOPE,          &
     &      	   SNUP,SALP,BEXP,DKSAT,DWSAT,SMCMAX,SMCWLT,SMCREF,     &
     &      	   SMCDRY,F1,QUARTZ,FXEXP,RTDIS,SLDPTH,ZSOIL,           &
     &      	   NROOT,NSOIL,Z0,CZIL,XLAI,CSOIL,PTU)

! ----------------------------------------------------------------------
!  INITIALIZE PRECIPITATION LOGICALS.
! ----------------------------------------------------------------------
      SNOWNG = .FALSE.
      FRZGRA = .FALSE.

! ----------------------------------------------------------------------
! IF SEA-ICE CASE, ASSIGN DEFAULT WATER-EQUIV SNOW ON TOP
! ----------------------------------------------------------------------
      IF (ICE .EQ. 1) THEN
        SNEQV = 0.01
        SNOWH = 0.05
        SNDENS = SNEQV/SNOWH
      ENDIF

! ----------------------------------------------------------------------
! IF INPUT SNOWPACK IS NONZERO, THEN COMPUTE SNOW DENSITY "SNDENS" AND
!   SNOW THERMAL CONDUCTIVITY "SNCOND" (NOTE THAT CSNOW IS A FUNCTION
!   SUBROUTINE)
! ----------------------------------------------------------------------
      IF (SNEQV .EQ. 0.0) THEN
        SNDENS = 0.0
        SNOWH = 0.0
        SNCOND = 1.0
      ELSE
        SNDENS = SNEQV/SNOWH
        SNCOND = CSNOW(SNDENS) 
      ENDIF

! ----------------------------------------------------------------------
! DETERMINE IF IT'S PRECIPITATING AND WHAT KIND OF PRECIP IT IS.
! IF IT'S PRCPING AND THE AIR TEMP IS COLDER THAN 0 C, IT'S SNOWING!
! IF IT'S PRCPING AND THE AIR TEMP IS WARMER THAN 0 C, BUT THE GRND
! TEMP IS COLDER THAN 0 C, FREEZING RAIN IS PRESUMED TO BE FALLING.
! ----------------------------------------------------------------------
      IF (PRCP .GT. 0.0) THEN
!        IF (SFCTMP .LE. TFREEZ) THEN
        IF (FFROZP .GT. 0.5) THEN
          SNOWNG = .TRUE.
        ELSE
          IF (T1 .LE. TFREEZ) FRZGRA = .TRUE.
        ENDIF
      ENDIF

! ----------------------------------------------------------------------
! IF EITHER PRCP FLAG IS SET, DETERMINE NEW SNOWFALL (CONVERTING PRCP
! RATE FROM KG M-2 S-1 TO A LIQUID EQUIV SNOW DEPTH IN METERS) AND ADD
! IT TO THE EXISTING SNOWPACK.
! NOTE THAT SINCE ALL PRECIP IS ADDED TO SNOWPACK, NO PRECIP INFILTRATES
! INTO THE SOIL SO THAT PRCP1 IS SET TO ZERO.
! ----------------------------------------------------------------------
      IF ( (SNOWNG) .OR. (FRZGRA) ) THEN
        SN_NEW = PRCP * DT * 0.001
        SNEQV = SNEQV + SN_NEW
        PRCP1 = 0.0

! ----------------------------------------------------------------------
! UPDATE SNOW DENSITY BASED ON NEW SNOWFALL, USING OLD AND NEW SNOW.
! UPDATE SNOW THERMAL CONDUCTIVITY
! ----------------------------------------------------------------------
        CALL SNOW_NEW (SFCTMP,SN_NEW,SNOWH,SNDENS)
        SNCOND = CSNOW (SNDENS) 
      ELSE

! ----------------------------------------------------------------------
! PRECIP IS LIQUID (RAIN), HENCE SAVE IN THE PRECIP VARIABLE THAT
! LATER CAN WHOLELY OR PARTIALLY INFILTRATE THE SOIL (ALONG WITH 
! ANY CANOPY "DRIP" ADDED TO THIS LATER)
! ----------------------------------------------------------------------
        PRCP1 = PRCP

      ENDIF

! ----------------------------------------------------------------------
! DETERMINE SNOWCOVER AND ALBEDO OVER LAND.
! ----------------------------------------------------------------------
      IF (ICE .EQ. 0) THEN

! ----------------------------------------------------------------------
! IF SNOW DEPTH=0, SET SNOW FRACTION=0, ALBEDO=SNOW FREE ALBEDO.
! ----------------------------------------------------------------------
        IF (SNEQV .EQ. 0.0) THEN
          SNCOVR = 0.0
          ALBEDO = ALB

        ELSE
! ----------------------------------------------------------------------
! DETERMINE SNOW FRACTIONAL COVERAGE.
! DETERMINE SURFACE ALBEDO MODIFICATION DUE TO SNOWDEPTH STATE.
! ----------------------------------------------------------------------
          CALL SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR)
! MEK JAN 2006, LIMIT SNOW COVER TO A MAXIMUM FRACTION OF 0.98
          SNCOVR = MIN(SNCOVR,0.98)
          CALL ALCALC (ALB,SNOALB,SHDFAC,SHDMIN,SNCOVR,TSNOW,ALBEDO)
        ENDIF

      ELSE
! ----------------------------------------------------------------------
! SNOW COVER, ALBEDO OVER SEA-ICE
! ----------------------------------------------------------------------
        SNCOVR = 1.0
!   changed in version 2.6 on June 2nd 2003
!        ALBEDO = 0.60
        ALBEDO = 0.65
      ENDIF

! ----------------------------------------------------------------------
! THERMAL CONDUCTIVITY FOR SEA-ICE CASE
! ----------------------------------------------------------------------
      IF (ICE .EQ. 1) THEN
        DF1 = 2.2

      ELSE

! ----------------------------------------------------------------------
! NEXT CALCULATE THE SUBSURFACE HEAT FLUX, WHICH FIRST REQUIRES
! CALCULATION OF THE THERMAL DIFFUSIVITY.  TREATMENT OF THE
! LATTER FOLLOWS THAT ON PAGES 148-149 FROM "HEAT TRANSFER IN 
! COLD CLIMATES", BY V. J. LUNARDINI (PUBLISHED IN 1981 
! BY VAN NOSTRAND REINHOLD CO.) I.E. TREATMENT OF TWO CONTIGUOUS 
! "PLANE PARALLEL" MEDIUMS (NAMELY HERE THE FIRST SOIL LAYER 
! AND THE SNOWPACK LAYER, IF ANY). THIS DIFFUSIVITY TREATMENT 
! BEHAVES WELL FOR BOTH ZERO AND NONZERO SNOWPACK, INCLUDING THE 
! LIMIT OF VERY THIN SNOWPACK.  THIS TREATMENT ALSO ELIMINATES
! THE NEED TO IMPOSE AN ARBITRARY UPPER BOUND ON SUBSURFACE 
! HEAT FLUX WHEN THE SNOWPACK BECOMES EXTREMELY THIN.
! ----------------------------------------------------------------------
! FIRST CALCULATE THERMAL DIFFUSIVITY OF TOP SOIL LAYER, USING
! BOTH THE FROZEN AND LIQUID SOIL MOISTURE, FOLLOWING THE 
! SOIL THERMAL DIFFUSIVITY FUNCTION OF PETERS-LIDARD ET AL.
! (1998,JAS, VOL 55, 1209-1224), WHICH REQUIRES THE SPECIFYING
! THE QUARTZ CONTENT OF THE GIVEN SOIL CLASS (SEE ROUTINE REDPRM)
! ----------------------------------------------------------------------
        CALL TDFCND (DF1,SMC(1),QUARTZ,SMCMAX,SH2O(1))

! ----------------------------------------------------------------------
! NEXT ADD SUBSURFACE HEAT FLUX REDUCTION EFFECT FROM THE 
! OVERLYING GREEN CANOPY, ADAPTED FROM SECTION 2.1.2 OF 
! PETERS-LIDARD ET AL. (1997, JGR, VOL 102(D4))
! ----------------------------------------------------------------------
        DF1 = DF1 * EXP(SBETA*SHDFAC)
      ENDIF

! ----------------------------------------------------------------------
! FINALLY "PLANE PARALLEL" SNOWPACK EFFECT FOLLOWING 
! V.J. LINARDINI REFERENCE CITED ABOVE. NOTE THAT DTOT IS
! COMBINED DEPTH OF SNOWDEPTH AND THICKNESS OF FIRST SOIL LAYER
! ----------------------------------------------------------------------
      DSOIL = -(0.5 * ZSOIL(1))

      IF (SNEQV .EQ. 0.) THEN
        SSOIL = DF1 * (T1 - STC(1) ) / DSOIL
      ELSE
        DTOT = SNOWH + DSOIL
        FRCSNO = SNOWH/DTOT
        FRCSOI = DSOIL/DTOT
!
! 1. HARMONIC MEAN (SERIES FLOW)
!        DF1 = (SNCOND*DF1)/(FRCSOI*SNCOND+FRCSNO*DF1)
        DF1H = (SNCOND*DF1)/(FRCSOI*SNCOND+FRCSNO*DF1)
! 2. ARITHMETIC MEAN (PARALLEL FLOW)
!        DF1 = FRCSNO*SNCOND + FRCSOI*DF1
        DF1A = FRCSNO*SNCOND + FRCSOI*DF1
!
! 3. GEOMETRIC MEAN (INTERMEDIATE BETWEEN HARMONIC AND ARITHMETIC MEAN)
!        DF1 = (SNCOND**FRCSNO)*(DF1**FRCSOI)
! TEST - MBEK, 10 Jan 2002
! weigh DF by snow fraction
!        DF1 = DF1H*SNCOVR + DF1A*(1.0-SNCOVR)
!        DF1 = DF1H*SNCOVR + DF1*(1.0-SNCOVR)
        DF1 = DF1A*SNCOVR + DF1*(1.0-SNCOVR)

! ----------------------------------------------------------------------
! CALCULATE SUBSURFACE HEAT FLUX, SSOIL, FROM FINAL THERMAL DIFFUSIVITY
! OF SURFACE MEDIUMS, DF1 ABOVE, AND SKIN TEMPERATURE AND TOP 
! MID-LAYER SOIL TEMPERATURE
! ----------------------------------------------------------------------
        SSOIL = DF1 * (T1 - STC(1) ) / DTOT
      ENDIF

! MEK -- DEBUG -- AUG 2005
!      WRITE(*,*) 'T1,STC(1),DSOIL=',T1,STC(1),DSOIL
!      WRITE(*,*) 'DF1,SBETA,SHDFAC=',DF1,SBETA,SHDFAC
!      WRITE(*,*) 'SSOIL=',SSOIL

! ----------------------------------------------------------------------
! DETERMINE SURFACE ROUGHNESS OVER SNOWPACK USING SNOW CONDITION FROM
! THE PREVIOUS TIMESTEP.
! ----------------------------------------------------------------------
      IF (SNCOVR .GT. 0.) THEN
        CALL SNOWZ0 (SNCOVR,Z0)
      ENDIF

! ----------------------------------------------------------------------
! NEXT CALL ROUTINE SFCDIF TO CALCULATE THE SFC EXCHANGE COEF (CH) FOR
! HEAT AND MOISTURE.
!
! NOTE !!!
! COMMENT OUT CALL SFCDIF, IF SFCDIF ALREADY CALLED IN CALLING PROGRAM
! (SUCH AS IN COUPLED ATMOSPHERIC MODEL).
!
! NOTE !!!
! DO NOT CALL SFCDIF UNTIL AFTER ABOVE CALL TO REDPRM, IN CASE
! ALTERNATIVE VALUES OF ROUGHNESS LENGTH (Z0) AND ZILINTINKEVICH COEF
! (CZIL) ARE SET THERE VIA NAMELIST I/O.
!
! NOTE !!!
! ROUTINE SFCDIF RETURNS A CH THAT REPRESENTS THE WIND SPD TIMES THE
! "ORIGINAL" NONDIMENSIONAL "Ch" TYPICAL IN LITERATURE.  HENCE THE CH
! RETURNED FROM SFCDIF HAS UNITS OF M/S.  THE IMPORTANT COMPANION
! COEFFICIENT OF CH, CARRIED HERE AS "RCH", IS THE CH FROM SFCDIF TIMES
! AIR DENSITY AND PARAMETER "CP".  "RCH" IS COMPUTED IN "CALL PENMAN".
! RCH RATHER THAN CH IS THE COEFF USUALLY INVOKED LATER IN EQNS.
!
! NOTE !!!
! SFCDIF ALSO RETURNS THE SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM, CM,
! ALSO KNOWN AS THE SURFACE DRAGE COEFFICIENT, BUT CM IS NOT USED HERE.
! ----------------------------------------------------------------------
! CALC VIRTUAL TEMPS AND VIRTUAL POTENTIAL TEMPS NEEDED BY SUBROUTINES
! SFCDIF AND PENMAN.
! ----------------------------------------------------------------------
      T2V = SFCTMP * (1.0 + 0.61 * Q2 )
! ----------------------------------------------------------------------
! COMMENT OUT BELOW 2 LINES IF CALL SFCDIF IS COMMENTED OUT, I.E. IN THE
! COUPLED MODEL.
! ----------------------------------------------------------------------
!      T1V = T1 * (1.0 + 0.61 * Q2)
!      TH2V = TH2 * (1.0 + 0.61 * Q2)
!
!      CALL SFCDIF (ZLVL,Z0,T1V,TH2V,SFCSPD,CZIL,CM,CH)

! ----------------------------------------------------------------------
! CALCULATE TOTAL DOWNWARD RADIATION (SOLAR PLUS LONGWAVE) NEEDED IN
! PENMAN EP SUBROUTINE THAT FOLLOWS
! ----------------------------------------------------------------------
!      FDOWN = SOLDN*(1.0-ALBEDO) + LWDN
      FDOWN = SOLNET + LWDN

! ----------------------------------------------------------------------
! CALL PENMAN SUBROUTINE TO CALCULATE POTENTIAL EVAPORATION (ETP), AND
! OTHER PARTIAL PRODUCTS AND SUMS SAVE IN COMMON/RITE FOR LATER
! CALCULATIONS.
! ----------------------------------------------------------------------
       CALL PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL,      &
     &              Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,            &
     &              DQSDT2,FLX2)

! ----------------------------------------------------------------------
! CALL CANRES TO CALCULATE THE CANOPY RESISTANCE AND CONVERT IT INTO PC
! IF NONZERO GREENNESS FRACTION
! ----------------------------------------------------------------------
      IF (SHDFAC .GT. 0.) THEN
      
! ----------------------------------------------------------------------
!  FROZEN GROUND EXTENSION: TOTAL SOIL WATER "SMC" WAS REPLACED 
!  BY UNFROZEN SOIL WATER "SH2O" IN CALL TO CANRES BELOW
! ----------------------------------------------------------------------
        CALL CANRES (SOLDN,CH,SFCTMP,Q2,SFCPRS,SH2O,ZSOIL,NSOIL,        &
     &               SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,      &
     &               TOPT,RSMAX,RGL,HS,XLAI,                            &
     &               RCS,RCT,RCQ,RCSOIL)

      ENDIF

! ----------------------------------------------------------------------
! NOW DECIDE MAJOR PATHWAY BRANCH TO TAKE DEPENDING ON WHETHER SNOWPACK
! EXISTS OR NOT:
! ----------------------------------------------------------------------
      ESNOW = 0.0
      IF (SNEQV .EQ. 0.0) THEN
        CALL NOPAC (ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,                     &
     &     	    SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,SHDFAC,           &
     &     	    SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,          &
     &     	    STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,                    &
     &     	    SH2O,SLOPE,KDT,FRZX,PSISAT,ZSOIL,                   &
     &     	    DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,              &
     &     	    RUNOFF3,EDIR,EC,ET,ETT,NROOT,ICE,RTDIS,             &
     &     	    QUARTZ,FXEXP,CSOIL,                                 &
     &     	    BETA,DRIP,DEW,FLX1,FLX2,FLX3)
      ELSE
        CALL SNOPAC (ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT,       &
     &               SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,                 &
     &               SBETA,DF1,                                         &
     &               Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,     &
     &               SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,SNEQV,SNDENS,  &
     &               SNOWH,SH2O,SLOPE,KDT,FRZX,PSISAT,SNUP,             &
     &               ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,        &
     &               RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,       &
     &               ICE,RTDIS,QUARTZ,FXEXP,CSOIL,                      &
     &               BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW)
!        ESNOW = ETA
      ENDIF

! ----------------------------------------------------------------------
!   PREPARE SENSIBLE HEAT (H) FOR RETURN TO PARENT MODEL
! ----------------------------------------------------------------------
      SHEAT = -(CH * CP * SFCPRS)/(R * T2V) * ( TH2 - T1 )
          
! ----------------------------------------------------------------------
!  CONVERT UNITS AND/OR SIGN OF TOTAL EVAP (ETA), POTENTIAL EVAP (ETP),
!  SUBSURFACE HEAT FLUX (S), AND RUNOFFS FOR WHAT PARENT MODEL EXPECTS
!  CONVERT ETA FROM KG M-2 S-1 TO W M-2
! ----------------------------------------------------------------------
!      ETA = ETA*LVH2O
!      ETP = ETP*LVH2O

! ----------------------------------------------------------------------
      EDIR = EDIR * LVH2O
      EC = EC * LVH2O
      DO K=1,4
        ET(K) = ET(K) * LVH2O
      ENDDO
      ETT = ETT * LVH2O
      ESNOW = ESNOW * LSUBS
      ETP = ETP*((1.-SNCOVR)*LVH2O + SNCOVR*LSUBS)
      IF (ETP .GT. 0.) THEN
        ETA = EDIR + EC + ETT + ESNOW
      ELSE
        ETA = ETP
      ENDIF
      BETA = ETA/ETP
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! CONVERT THE SIGN OF SOIL HEAT FLUX SO THAT:
!   SSOIL>0: WARM THE SURFACE  (NIGHT TIME)
!   SSOIL<0: COOL THE SURFACE  (DAY TIME)
! ----------------------------------------------------------------------
      SSOIL = -1.0*SSOIL      

! ----------------------------------------------------------------------
!  CONVERT RUNOFF3 (INTERNAL LAYER RUNOFF FROM SUPERSAT) FROM M TO M S-1
!  AND ADD TO SUBSURFACE RUNOFF/DRAINAGE/BASEFLOW
! ----------------------------------------------------------------------
      RUNOFF3 = RUNOFF3/DT
      RUNOFF2 = RUNOFF2+RUNOFF3

! ----------------------------------------------------------------------
! TOTAL COLUMN SOIL MOISTURE IN METERS (SOILM) AND ROOT-ZONE 
! SOIL MOISTURE AVAILABILITY (FRACTION) RELATIVE TO POROSITY/SATURATION
! ----------------------------------------------------------------------
      SOILM = -1.0*SMC(1)*ZSOIL(1)
      DO K = 2,NSOIL
        SOILM = SOILM+SMC(K)*(ZSOIL(K-1)-ZSOIL(K))
      END DO
      SOILWM = -1.0*(SMCMAX-SMCWLT)*ZSOIL(1)
      SOILWW = -1.0*(SMC(1)-SMCWLT)*ZSOIL(1)
      DO K = 2,NROOT
        SOILWM = SOILWM+(SMCMAX-SMCWLT)*(ZSOIL(K-1)-ZSOIL(K))
        SOILWW = SOILWW+(SMC(K)-SMCWLT)*(ZSOIL(K-1)-ZSOIL(K))
      END DO
      SOILW = SOILWW/SOILWM

! ----------------------------------------------------------------------
! END SUBROUTINE SFLX
! ----------------------------------------------------------------------
      END SUBROUTINE SFLX

      SUBROUTINE ALCALC (ALB,SNOALB,SHDFAC,SHDMIN,SNCOVR,TSNOW,ALBEDO)

      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! CALCULATE ALBEDO INCLUDING SNOW EFFECT (0 -> 1)
!   ALB     SNOWFREE ALBEDO
!   SNOALB  MAXIMUM (DEEP) SNOW ALBEDO
!   SHDFAC    AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!   SHDMIN    MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!   SNCOVR  FRACTIONAL SNOW COVER
!   ALBEDO  SURFACE ALBEDO INCLUDING SNOW EFFECT
!   TSNOW   SNOW SURFACE TEMPERATURE (K)
! ----------------------------------------------------------------------
      REAL ALB, SNOALB, SHDFAC, SHDMIN, SNCOVR, ALBEDO, TSNOW
      
! ----------------------------------------------------------------------
! SNOALB IS ARGUMENT REPRESENTING MAXIMUM ALBEDO OVER DEEP SNOW,
! AS PASSED INTO SFLX, AND ADAPTED FROM THE SATELLITE-BASED MAXIMUM 
! SNOW ALBEDO FIELDS PROVIDED BY D. ROBINSON AND G. KUKLA 
! (1985, JCAM, VOL 24, 402-411)
! ----------------------------------------------------------------------
!         changed in version 2.6 on June 2nd 2003
!          ALBEDO = ALB + (1.0-(SHDFAC-SHDMIN))*SNCOVR*(SNOALB-ALB) 
          ALBEDO = ALB + SNCOVR*(SNOALB-ALB)
          IF (ALBEDO .GT. SNOALB) ALBEDO=SNOALB

!     BASE FORMULATION (DICKINSON ET AL., 1986, COGLEY ET AL., 1990)
!          IF (TSNOW.LE.263.16) THEN
!            ALBEDO=SNOALB
!          ELSE
!            IF (TSNOW.LT.273.16) THEN
!              TM=0.1*(TSNOW-263.16)
!              ALBEDO=0.5*((0.9-0.2*(TM**3))+(0.8-0.16*(TM**3)))
!            ELSE
!              ALBEDO=0.67
!            ENDIF
!          ENDIF

!     ISBA FORMULATION (VERSEGHY, 1991; BAKER ET AL., 1990)
!          IF (TSNOW.LT.273.16) THEN
!            ALBEDO=SNOALB-0.008*DT/86400
!          ELSE
!            ALBEDO=(SNOALB-0.5)*EXP(-0.24*DT/86400)+0.5
!          ENDIF

! ----------------------------------------------------------------------
! END SUBROUTINE ALCALC
! ----------------------------------------------------------------------
      END SUBROUTINE ALCALC

      SUBROUTINE CANRES (SOLAR,CH,SFCTMP,Q2,SFCPRS,SMC,ZSOIL,NSOIL,     &
     &                   SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,  &
     &                   TOPT,RSMAX,RGL,HS,XLAI,                        &
     &                   RCS,RCT,RCQ,RCSOIL)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE CANRES                    
! ----------------------------------------------------------------------
! CALCULATE CANOPY RESISTANCE WHICH DEPENDS ON INCOMING SOLAR RADIATION,
! AIR TEMPERATURE, ATMOSPHERIC WATER VAPOR PRESSURE DEFICIT AT THE
! LOWEST MODEL LEVEL, AND SOIL MOISTURE (PREFERABLY UNFROZEN SOIL
! MOISTURE RATHER THAN TOTAL)
! ----------------------------------------------------------------------
! SOURCE:  JARVIS (1976), NOILHAN AND PLANTON (1989, MWR), JACQUEMIN AND
! NOILHAN (1990, BLM)
! SEE ALSO:  CHEN ET AL (1996, JGR, VOL 101(D3), 7251-7268), EQNS 12-14
! AND TABLE 2 OF SEC. 3.1.2         
! ----------------------------------------------------------------------
! INPUT:
!   SOLAR   INCOMING SOLAR RADIATION
!   CH      SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
!   SFCTMP  AIR TEMPERATURE AT 1ST LEVEL ABOVE GROUND
!   Q2      AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
!   Q2SAT   SATURATION AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
!   DQSDT2  SLOPE OF SATURATION HUMIDITY FUNCTION WRT TEMP
!   SFCPRS  SURFACE PRESSURE
!   SMC     VOLUMETRIC SOIL MOISTURE 
!   ZSOIL   SOIL DEPTH (NEGATIVE SIGN, AS IT IS BELOW GROUND)
!   NSOIL   NO. OF SOIL LAYERS
!   NROOT   NO. OF SOIL LAYERS IN ROOT ZONE (1.LE.NROOT.LE.NSOIL)
!   XLAI    LEAF AREA INDEX
!   SMCWLT  WILTING POINT
!   SMCREF  REFERENCE SOIL MOISTURE (WHERE SOIL WATER DEFICIT STRESS
!             SETS IN)
! RSMIN, RSMAX, TOPT, RGL, HS ARE CANOPY STRESS PARAMETERS SET IN
!   SURBOUTINE REDPRM
! OUTPUT:
!   PC  PLANT COEFFICIENT
!   RC  CANOPY RESISTANCE
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER K
      INTEGER NROOT
      INTEGER NSOIL

      REAL CH
      REAL CP
      REAL DELTA
      REAL DQSDT2
      REAL FF
      REAL GX
      REAL HS
      REAL P
      REAL PART(NSOLD) 
      REAL PC
      REAL Q2
      REAL Q2SAT
      REAL RC
      REAL RSMIN
      REAL RCQ
      REAL RCS
      REAL RCSOIL
      REAL RCT
      REAL RD
      REAL RGL
      REAL RR
      REAL RSMAX
      REAL SFCPRS
      REAL SFCTMP
      REAL SIGMA
      REAL SLV
      REAL SMC(NSOIL)
      REAL SMCREF
      REAL SMCWLT
      REAL SOLAR
      REAL TOPT
      REAL SLVCP
      REAL ST1
      REAL TAIR4
      REAL XLAI
      REAL ZSOIL(NSOIL)

      PARAMETER(CP = 1004.5)
      PARAMETER(RD = 287.04)
      PARAMETER(SIGMA = 5.67E-8)
      PARAMETER(SLV = 2.501000E6)

! ----------------------------------------------------------------------
! INITIALIZE CANOPY RESISTANCE MULTIPLIER TERMS.
! ----------------------------------------------------------------------
      RCS = 0.0
      RCT = 0.0
      RCQ = 0.0
      RCSOIL = 0.0
      RC = 0.0

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO INCOMING SOLAR RADIATION
! ----------------------------------------------------------------------
      FF = 0.55*2.0*SOLAR/(RGL*XLAI)
      RCS = (FF + RSMIN/RSMAX) / (1.0 + FF)
      RCS = MAX(RCS,0.0001)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO AIR TEMPERATURE AT FIRST MODEL LEVEL ABOVE GROUND
! RCT EXPRESSION FROM NOILHAN AND PLANTON (1989, MWR).
! ----------------------------------------------------------------------
      RCT = 1.0 - 0.0016*((TOPT-SFCTMP)**2.0)
      RCT = MAX(RCT,0.0001)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT FIRST MODEL LEVEL.
! RCQ EXPRESSION FROM SSIB 
! ----------------------------------------------------------------------
      RCQ = 1.0/(1.0+HS*(Q2SAT-Q2))
      RCQ = MAX(RCQ,0.01)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.
! DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.
! ----------------------------------------------------------------------
      GX = (SMC(1) - SMCWLT) / (SMCREF - SMCWLT)
      IF (GX .GT. 1.) GX = 1.
      IF (GX .LT. 0.) GX = 0.

! ----------------------------------------------------------------------
! USE SOIL DEPTH AS WEIGHTING FACTOR
! ----------------------------------------------------------------------
      PART(1) = (ZSOIL(1)/ZSOIL(NROOT)) * GX
! ----------------------------------------------------------------------
! USE ROOT DISTRIBUTION AS WEIGHTING FACTOR
!      PART(1) = RTDIS(1) * GX
! ----------------------------------------------------------------------
      IF (NROOT .GT. 1) THEN
        DO K = 2,NROOT
          GX = (SMC(K) - SMCWLT) / (SMCREF - SMCWLT)
          IF (GX .GT. 1.) GX = 1.
          IF (GX .LT. 0.) GX = 0.
! ----------------------------------------------------------------------
! USE SOIL DEPTH AS WEIGHTING FACTOR        
! ----------------------------------------------------------------------
          PART(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT)) * GX
! ----------------------------------------------------------------------
! USE ROOT DISTRIBUTION AS WEIGHTING FACTOR
!        PART(K) = RTDIS(K) * GX 
! ----------------------------------------------------------------------
        END DO
      ENDIF

      DO K = 1,NROOT
        RCSOIL = RCSOIL+PART(K)
      END DO
      RCSOIL = MAX(RCSOIL,0.0001)

! ----------------------------------------------------------------------
! DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.  CONVERT CANOPY
! RESISTANCE (RC) TO PLANT COEFFICIENT (PC) TO BE USED WITH POTENTIAL
! EVAP IN DETERMINING ACTUAL EVAP.  PC IS DETERMINED BY:
!   PC * LINERIZED PENMAN POTENTIAL EVAP =
!   PENMAN-MONTEITH ACTUAL EVAPORATION (CONTAINING RC TERM).
! ----------------------------------------------------------------------
      RC = RSMIN/(XLAI*RCS*RCT*RCQ*RCSOIL)

!      TAIR4 = SFCTMP**4.
!      ST1 = (4.*SIGMA*RD)/CP
!      SLVCP = SLV/CP
!      RR = ST1*TAIR4/(SFCPRS*CH) + 1.0
      RR = (4.*SIGMA*RD/CP)*(SFCTMP**4.)/(SFCPRS*CH) + 1.0
      DELTA = (SLV/CP)*DQSDT2

      PC = (RR+DELTA)/(RR*(1.+RC*CH)+DELTA)

! ----------------------------------------------------------------------
! END SUBROUTINE CANRES
! ----------------------------------------------------------------------
      END SUBROUTINE CANRES

      SUBROUTINE DEVAP (EDIR1,ETP1,SMC,ZSOIL,SHDFAC,SMCMAX,BEXP,        &
     &                DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE DEVAP
! ----------------------------------------------------------------------
! CALCULATE DIRECT SOIL EVAPORATION
! ----------------------------------------------------------------------
      REAL BEXP
!      REAL DEVAP
      REAL EDIR1
      REAL DKSAT
      REAL DWSAT
      REAL ETP1
      REAL FX
      REAL FXEXP
      REAL SHDFAC
      REAL SMC
      REAL SMCDRY
      REAL SMCMAX
      REAL ZSOIL
      REAL SMCREF
      REAL SMCWLT
      REAL SRATIO

! ----------------------------------------------------------------------
! DIRECT EVAP A FUNCTION OF RELATIVE SOIL MOISTURE AVAILABILITY, LINEAR
! WHEN FXEXP=1.
! FX > 1 REPRESENTS DEMAND CONTROL
! FX < 1 REPRESENTS FLUX CONTROL
! ----------------------------------------------------------------------
      SRATIO = (SMC - SMCDRY) / (SMCMAX - SMCDRY)
      IF (SRATIO .GT. 0.) THEN
        FX = SRATIO**FXEXP
        FX = MAX ( MIN ( FX, 1. ) ,0. )
      ELSE
        FX = 0.
      ENDIF

! ----------------------------------------------------------------------
! ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE
! ----------------------------------------------------------------------
!      DEVAP = FX * ( 1.0 - SHDFAC ) * ETP1
      EDIR1 = FX * ( 1.0 - SHDFAC ) * ETP1

! ----------------------------------------------------------------------
! END SUBROUTINE DEVAP
! ----------------------------------------------------------------------
      END SUBROUTINE DEVAP

      SUBROUTINE EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,               &
     &                  SH2O,                                           &
     &                  SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,              &
     &                  SMCREF,SHDFAC,CMCMAX,                           &
     &                  SMCDRY,CFACTR,                                  &
     &                  EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE EVAPO
! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER
! UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH
! PROGNOSTIC EQNS. THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.
! FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND
! CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER I
      INTEGER K
      INTEGER NSOIL
      INTEGER NROOT

      REAL BEXP
      REAL CFACTR
      REAL CMC
      REAL CMC2MS
      REAL CMCMAX
!      REAL DEVAP
      REAL DKSAT
      REAL DT
      REAL DWSAT
      REAL EC1
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL ETA1
      REAL ETP1
      REAL ETT1
      REAL FXEXP
      REAL PC
      REAL Q2
      REAL RTDIS(NSOIL)
      REAL SFCTMP
      REAL SHDFAC
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL ZSOIL(NSOIL)

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE IF THE POTENTIAL EVAPOTRANSPIRATION IS
! GREATER THAN ZERO.
! ----------------------------------------------------------------------
      EDIR1 = 0.
      EC1 = 0.
      DO K = 1,NSOIL
        ET1(K) = 0.
      END DO
      ETT1 = 0.

      IF (ETP1 .GT. 0.0) THEN

! ----------------------------------------------------------------------
! RETRIEVE DIRECT EVAPORATION FROM SOIL SURFACE.  CALL THIS FUNCTION
! ONLY IF VEG COVER NOT COMPLETE.
! FROZEN GROUND VERSION:  SH2O STATES REPLACE SMC STATES.
! ----------------------------------------------------------------------
        IF (SHDFAC .LT. 1.) THEN
        CALL DEVAP (EDIR1,ETP1,SH2O(1),ZSOIL(1),SHDFAC,SMCMAX,          &
!          EDIR = DEVAP(ETP1,SH2O(1),ZSOIL(1),SHDFAC,SMCMAX,             &
     &                 BEXP,DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)
        ENDIF

! ----------------------------------------------------------------------
! INITIALIZE PLANT TOTAL TRANSPIRATION, RETRIEVE PLANT TRANSPIRATION,
! AND ACCUMULATE IT FOR ALL SOIL LAYERS.
! ----------------------------------------------------------------------
        IF (SHDFAC.GT.0.0) THEN

          CALL TRANSP (ET1,NSOIL,ETP1,SH2O,CMC,ZSOIL,SHDFAC,SMCWLT,     &
     &                 CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)

          DO K = 1,NSOIL
            ETT1 = ETT1 + ET1(K)
          END DO

! ----------------------------------------------------------------------
! CALCULATE CANOPY EVAPORATION.
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR CMC=0.0.
! ----------------------------------------------------------------------
          IF (CMC .GT. 0.0) THEN
            EC1 = SHDFAC * ( ( CMC / CMCMAX ) ** CFACTR ) * ETP1
          ELSE
            EC1 = 0.0
          ENDIF

! ----------------------------------------------------------------------
! EC SHOULD BE LIMITED BY THE TOTAL AMOUNT OF AVAILABLE WATER ON THE
! CANOPY.  -F.CHEN, 18-OCT-1994
! ----------------------------------------------------------------------
          CMC2MS = CMC / DT
          EC1 = MIN ( CMC2MS, EC1 )
        ENDIF
      ENDIF

! ----------------------------------------------------------------------
! TOTAL UP EVAP AND TRANSP TYPES TO OBTAIN ACTUAL EVAPOTRANSP
! ----------------------------------------------------------------------
      ETA1 = EDIR1 + ETT1 + EC1

! ----------------------------------------------------------------------
! END SUBROUTINE EVAPO
! ----------------------------------------------------------------------
      END SUBROUTINE EVAPO

      SUBROUTINE HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,          &
     &                TBOT,ZBOT,PSISAT,SH2O,DT,BEXP,                    &
     &                F1,DF1,QUARTZ,CSOIL,AI,BI,CI)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE HRT
! ----------------------------------------------------------------------
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
! THERMAL DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
! COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      LOGICAL ITAVG

      INTEGER I
      INTEGER K
      INTEGER NSOIL

! ----------------------------------------------------------------------
! DECLARE WORK ARRAYS NEEDED IN TRI-DIAGONAL IMPLICIT SOLVER
! ----------------------------------------------------------------------
      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)

! ----------------------------------------------------------------------
! DECLARATIONS
! ----------------------------------------------------------------------
      REAL BEXP
      REAL CAIR
      REAL CH2O
      REAL CICE
      REAL CSOIL
      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DF1
      REAL DF1N
      REAL DF1K
      REAL DT
      REAL DTSDZ
      REAL DTSDZ2
      REAL F1
      REAL HCPCT
      REAL PSISAT
      REAL QUARTZ
      REAL QTOT
      REAL RHSTS(NSOIL)
      REAL SSOIL
      REAL SICE
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SMCMAX
!      REAL SNKSRC
      REAL STC(NSOIL)
      REAL T0
      REAL TAVG
      REAL TBK
      REAL TBK1
      REAL TBOT
      REAL ZBOT
      REAL TSNSR
      REAL TSURF
      REAL YY
      REAL ZSOIL(NSOIL)
      REAL ZZ1

      PARAMETER(T0 = 273.15)

! ----------------------------------------------------------------------
! SET SPECIFIC HEAT CAPACITIES OF AIR, WATER, ICE, SOIL MINERAL       
! ----------------------------------------------------------------------
      PARAMETER(CAIR = 1004.0)
      PARAMETER(CH2O = 4.2E6)
      PARAMETER(CICE = 2.106E6)
! NOTE: CSOIL NOW SET IN ROUTINE REDPRM AND PASSED IN
!      PARAMETER(CSOIL = 1.26E6)

! ----------------------------------------------------------------------
! INITIALIZE LOGICAL FOR SOIL LAYER TEMPERATURE AVERAGING.
! ----------------------------------------------------------------------
      ITAVG = .TRUE.
!      ITAVG = .FALSE.

! ----------------------------------------------------------------------
! BEGIN SECTION FOR TOP SOIL LAYER
! ----------------------------------------------------------------------
! CALC THE HEAT CAPACITY OF THE TOP SOIL LAYER
! ----------------------------------------------------------------------
      HCPCT = SH2O(1)*CH2O + (1.0-SMCMAX)*CSOIL + (SMCMAX-SMC(1))*CAIR  &
     &        + ( SMC(1) - SH2O(1) )*CICE

! ----------------------------------------------------------------------
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
! ----------------------------------------------------------------------
      DDZ = 1.0 / ( -0.5 * ZSOIL(2) )
      AI(1) = 0.0
      CI(1) = (DF1 * DDZ) / (ZSOIL(1) * HCPCT)
      BI(1) = -CI(1) + DF1 / (0.5 * ZSOIL(1) * ZSOIL(1)*HCPCT*ZZ1)

! ----------------------------------------------------------------------
! CALCULATE THE VERTICAL SOIL TEMP GRADIENT BTWN THE 1ST AND 2ND SOIL
! LAYERS.  THEN CALCULATE THE SUBSURFACE HEAT FLUX. USE THE TEMP
! GRADIENT AND SUBSFC HEAT FLUX TO CALC "RIGHT-HAND SIDE TENDENCY
! TERMS", OR "RHSTS", FOR TOP SOIL LAYER.
! ----------------------------------------------------------------------
      DTSDZ = (STC(1) - STC(2)) / (-0.5 * ZSOIL(2))
      SSOIL = DF1 * (STC(1) - YY) / (0.5 * ZSOIL(1) * ZZ1)
      RHSTS(1) = (DF1 * DTSDZ - SSOIL) / (ZSOIL(1) * HCPCT)

! ----------------------------------------------------------------------
! NEXT CAPTURE THE VERTICAL DIFFERENCE OF THE HEAT FLUX AT TOP AND
! BOTTOM OF FIRST SOIL LAYER FOR USE IN HEAT FLUX CONSTRAINT APPLIED TO
! POTENTIAL SOIL FREEZING/THAWING IN ROUTINE SNKSRC.
! ----------------------------------------------------------------------
      QTOT = SSOIL - DF1*DTSDZ

! ----------------------------------------------------------------------
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):
! SET TEMP "TSURF" AT TOP OF SOIL COLUMN (FOR USE IN FREEZING SOIL
! PHYSICS LATER IN FUNCTION SUBROUTINE SNKSRC).  IF SNOWPACK CONTENT IS
! ZERO, THEN TSURF EXPRESSION BELOW GIVES TSURF = SKIN TEMP.  IF
! SNOWPACK IS NONZERO (HENCE ARGUMENT ZZ1=1), THEN TSURF EXPRESSION
! BELOW YIELDS SOIL COLUMN TOP TEMPERATURE UNDER SNOWPACK.  THEN
! CALCULATE TEMPERATURE AT BOTTOM INTERFACE OF 1ST SOIL LAYER FOR USE
! LATER IN FUNCTION SUBROUTINE SNKSRC
! ----------------------------------------------------------------------
      IF (ITAVG) THEN 
        TSURF = (YY + (ZZ1-1) * STC(1)) / ZZ1
        CALL TBND (STC(1),STC(2),ZSOIL,ZBOT,1,NSOIL,TBK)
      ENDIF

! ----------------------------------------------------------------------
! CALCULATE FROZEN WATER CONTENT IN 1ST SOIL LAYER. 
! ----------------------------------------------------------------------
      SICE = SMC(1) - SH2O(1)

! ----------------------------------------------------------------------
! IF FROZEN WATER PRESENT OR ANY OF LAYER-1 MID-POINT OR BOUNDING
! INTERFACE TEMPERATURES BELOW FREEZING, THEN CALL SNKSRC TO
! COMPUTE HEAT SOURCE/SINK (AND CHANGE IN FROZEN WATER CONTENT)
! DUE TO POSSIBLE SOIL WATER PHASE CHANGE
! ----------------------------------------------------------------------
      IF ( (SICE   .GT. 0.) .OR. (TSURF .LT. T0) .OR.                   &
     &     (STC(1) .LT. T0) .OR. (TBK   .LT. T0) ) THEN

        IF (ITAVG) THEN 
          CALL TMPAVG(TAVG,TSURF,STC(1),TBK,ZSOIL,NSOIL,1)
        ELSE
          TAVG = STC(1)
        ENDIF
        TSNSR = SNKSRC (TAVG,SMC(1),SH2O(1),                            &
     &    ZSOIL,NSOIL,SMCMAX,PSISAT,BEXP,DT,1,QTOT)

        RHSTS(1) = RHSTS(1) - TSNSR / ( ZSOIL(1) * HCPCT )
      ENDIF
 
! ----------------------------------------------------------------------
! THIS ENDS SECTION FOR TOP SOIL LAYER.
! ----------------------------------------------------------------------
! INITIALIZE DDZ2
! ----------------------------------------------------------------------
      DDZ2 = 0.0

! ----------------------------------------------------------------------
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
! (EXCEPT SUBSFC OR "GROUND" HEAT FLUX NOT REPEATED IN LOWER LAYERS)
! ----------------------------------------------------------------------
      DF1K = DF1
      DO K = 2,NSOIL

! ----------------------------------------------------------------------
! CALCULATE HEAT CAPACITY FOR THIS SOIL LAYER.
! ----------------------------------------------------------------------
        HCPCT = SH2O(K)*CH2O +(1.0-SMCMAX)*CSOIL +(SMCMAX-SMC(K))*CAIR  &
     &        + ( SMC(K) - SH2O(K) )*CICE

        IF (K .NE. NSOIL) THEN
! ----------------------------------------------------------------------
! THIS SECTION FOR LAYER 2 OR GREATER, BUT NOT LAST LAYER.
! ----------------------------------------------------------------------
! CALCULATE THERMAL DIFFUSIVITY FOR THIS LAYER.
! ----------------------------------------------------------------------
          CALL TDFCND (DF1N,SMC(K),QUARTZ,SMCMAX,SH2O(K))

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER
! ----------------------------------------------------------------------
          DENOM = 0.5 * ( ZSOIL(K-1) - ZSOIL(K+1) )
          DTSDZ2 = ( STC(K) - STC(K+1) ) / DENOM

! ----------------------------------------------------------------------
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
! ----------------------------------------------------------------------
          DDZ2 = 2. / (ZSOIL(K-1) - ZSOIL(K+1))
          CI(K) = -DF1N * DDZ2 / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)

! ----------------------------------------------------------------------
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):  CALCULATE
! TEMP AT BOTTOM OF LAYER.
! ----------------------------------------------------------------------
          IF (ITAVG) THEN 
            CALL TBND (STC(K),STC(K+1),ZSOIL,ZBOT,K,NSOIL,TBK1)
          ENDIF
        ELSE

! ----------------------------------------------------------------------
! SPECIAL CASE OF BOTTOM SOIL LAYER:  CALCULATE THERMAL DIFFUSIVITY FOR
! BOTTOM LAYER.
! ----------------------------------------------------------------------
          CALL TDFCND (DF1N,SMC(K),QUARTZ,SMCMAX,SH2O(K))

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU BOTTOM LAYER.
! ----------------------------------------------------------------------
          DENOM = .5 * (ZSOIL(K-1) + ZSOIL(K)) - ZBOT
          DTSDZ2 = (STC(K)-TBOT) / DENOM

! ----------------------------------------------------------------------
! SET MATRIX COEF, CI TO ZERO IF BOTTOM LAYER.
! ----------------------------------------------------------------------
          CI(K) = 0.

! ----------------------------------------------------------------------
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):  CALCULATE
! TEMP AT BOTTOM OF LAST LAYER.
! ----------------------------------------------------------------------
          IF (ITAVG) THEN 
            CALL TBND (STC(K),TBOT,ZSOIL,ZBOT,K,NSOIL,TBK1)
          ENDIF 

        ENDIF
! ----------------------------------------------------------------------
! THIS ENDS SPECIAL LOOP FOR BOTTOM LAYER.
! ----------------------------------------------------------------------
! CALCULATE RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.
! ----------------------------------------------------------------------
        DENOM = ( ZSOIL(K) - ZSOIL(K-1) ) * HCPCT
        RHSTS(K) = ( DF1N * DTSDZ2 - DF1K * DTSDZ ) / DENOM
        QTOT = -1.0*DENOM*RHSTS(K)
        SICE = SMC(K) - SH2O(K)

        IF ( (SICE .GT. 0.) .OR. (TBK .LT. T0) .OR.                     &
     &     (STC(K) .LT. T0) .OR. (TBK1 .LT. T0) ) THEN

          IF (ITAVG) THEN 
            CALL TMPAVG(TAVG,TBK,STC(K),TBK1,ZSOIL,NSOIL,K)
          ELSE
            TAVG = STC(K)
          ENDIF
          TSNSR = SNKSRC(TAVG,SMC(K),SH2O(K),ZSOIL,NSOIL,               &
     &                   SMCMAX,PSISAT,BEXP,DT,K,QTOT)
          RHSTS(K) = RHSTS(K) - TSNSR / DENOM
        ENDIF 

! ----------------------------------------------------------------------
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
! ----------------------------------------------------------------------
        AI(K) = - DF1 * DDZ / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        BI(K) = -(AI(K) + CI(K))

! ----------------------------------------------------------------------
! RESET VALUES OF DF1, DTSDZ, DDZ, AND TBK FOR LOOP TO NEXT SOIL LAYER.
! ----------------------------------------------------------------------
        TBK   = TBK1
        DF1K  = DF1N
        DTSDZ = DTSDZ2
        DDZ   = DDZ2
      END DO

! ----------------------------------------------------------------------
! END SUBROUTINE HRT
! ----------------------------------------------------------------------
      END SUBROUTINE HRT

      SUBROUTINE HRTICE (RHSTS,STC,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE HRTICE
! ----------------------------------------------------------------------
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
! THERMAL DIFFUSION EQUATION IN THE CASE OF SEA-ICE PACK.  ALSO TO
! COMPUTE (PREPARE) THE MATRIX COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX
! OF THE IMPLICIT TIME SCHEME.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER K
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)

      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DF1
      REAL DTSDZ
      REAL DTSDZ2
      REAL HCPCT
      REAL RHSTS(NSOIL)
      REAL SSOIL
      REAL STC(NSOIL)
      REAL TBOT
      REAL YY
      REAL ZBOT
      REAL ZSOIL(NSOIL)
      REAL ZZ1

      DATA TBOT /271.16/

! ----------------------------------------------------------------------
! SET A NOMINAL UNIVERSAL VALUE OF THE SEA-ICE SPECIFIC HEAT CAPACITY,
! HCPCT = 1880.0*917.0.
! ----------------------------------------------------------------------
      PARAMETER(HCPCT = 1.72396E+6)

! ----------------------------------------------------------------------
! THE INPUT ARGUMENT DF1 IS A UNIVERSALLY CONSTANT VALUE OF SEA-ICE
! THERMAL DIFFUSIVITY, SET IN ROUTINE SNOPAC AS DF1 = 2.2.
! ----------------------------------------------------------------------
! SET ICE PACK DEPTH.  USE TBOT AS ICE PACK LOWER BOUNDARY TEMPERATURE
! (THAT OF UNFROZEN SEA WATER AT BOTTOM OF SEA ICE PACK).  ASSUME ICE
! PACK IS OF N=NSOIL LAYERS SPANNING A UNIFORM CONSTANT ICE PACK
! THICKNESS AS DEFINED BY ZSOIL(NSOIL) IN ROUTINE SFLX.
! ----------------------------------------------------------------------
      ZBOT = ZSOIL(NSOIL)

! ----------------------------------------------------------------------
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
! ----------------------------------------------------------------------
      DDZ = 1.0 / ( -0.5 * ZSOIL(2) )
      AI(1) = 0.0
      CI(1) = (DF1 * DDZ) / (ZSOIL(1) * HCPCT)
      BI(1) = -CI(1) + DF1/(0.5 * ZSOIL(1) * ZSOIL(1) * HCPCT * ZZ1)

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT BTWN THE TOP AND 2ND SOIL LAYERS.
! RECALC/ADJUST THE SOIL HEAT FLUX.  USE THE GRADIENT AND FLUX TO CALC
! RHSTS FOR THE TOP SOIL LAYER.
! ----------------------------------------------------------------------
      DTSDZ = ( STC(1) - STC(2) ) / ( -0.5 * ZSOIL(2) )
      SSOIL = DF1 * ( STC(1) - YY ) / ( 0.5 * ZSOIL(1) * ZZ1 )
      RHSTS(1) = ( DF1 * DTSDZ - SSOIL ) / ( ZSOIL(1) * HCPCT )

! ----------------------------------------------------------------------
! INITIALIZE DDZ2
! ----------------------------------------------------------------------
      DDZ2 = 0.0

! ----------------------------------------------------------------------
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
! ----------------------------------------------------------------------
      DO K = 2,NSOIL
        IF (K .NE. NSOIL) THEN

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER.
! ----------------------------------------------------------------------
          DENOM = 0.5 * ( ZSOIL(K-1) - ZSOIL(K+1) )
          DTSDZ2 = ( STC(K) - STC(K+1) ) / DENOM

! ----------------------------------------------------------------------
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT.
! ----------------------------------------------------------------------
          DDZ2 = 2. / (ZSOIL(K-1) - ZSOIL(K+1))
          CI(K) = -DF1 * DDZ2 / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        ELSE

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THE LOWEST LAYER.
! ----------------------------------------------------------------------
          DTSDZ2 = (STC(K)-TBOT)/(.5 * (ZSOIL(K-1) + ZSOIL(K))-ZBOT)

! ----------------------------------------------------------------------
! SET MATRIX COEF, CI TO ZERO.
! ----------------------------------------------------------------------
          CI(K) = 0.
        ENDIF

! ----------------------------------------------------------------------
! CALC RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.
! ----------------------------------------------------------------------
        DENOM = ( ZSOIL(K) - ZSOIL(K-1) ) * HCPCT
        RHSTS(K) = ( DF1 * DTSDZ2 - DF1 * DTSDZ ) / DENOM

! ----------------------------------------------------------------------
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
! ----------------------------------------------------------------------
        AI(K) = - DF1 * DDZ / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        BI(K) = -(AI(K) + CI(K))

! ----------------------------------------------------------------------
! RESET VALUES OF DTSDZ AND DDZ FOR LOOP TO NEXT SOIL LYR.
! ----------------------------------------------------------------------
        DTSDZ = DTSDZ2
        DDZ   = DDZ2

      END DO
! ----------------------------------------------------------------------
! END SUBROUTINE HRTICE
! ----------------------------------------------------------------------
      END SUBROUTINE HRTICE

      SUBROUTINE HSTEP (STCOUT,STCIN,RHSTS,DT,NSOIL,AI,BI,CI)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE HSTEP
! ----------------------------------------------------------------------
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER K
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)
      REAL CIin(NSOLD)
      REAL DT
      REAL RHSTS(NSOIL)
      REAL RHSTSin(NSOIL)
      REAL STCIN(NSOIL)
      REAL STCOUT(NSOIL)

! ----------------------------------------------------------------------
! CREATE FINITE DIFFERENCE VALUES FOR USE IN ROSR12 ROUTINE
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTS(K) = RHSTS(K) * DT
        AI(K) = AI(K) * DT
        BI(K) = 1. + BI(K) * DT
        CI(K) = CI(K) * DT
      END DO

! ----------------------------------------------------------------------
! COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
         RHSTSin(K) = RHSTS(K)
      END DO
      DO K = 1,NSOIL
        CIin(K) = CI(K)
      END DO

! ----------------------------------------------------------------------
! SOLVE THE TRI-DIAGONAL MATRIX EQUATION
! ----------------------------------------------------------------------
      CALL ROSR12(CI,AI,BI,CIin,RHSTSin,RHSTS,NSOIL)

! ----------------------------------------------------------------------
! CALC/UPDATE THE SOIL TEMPS USING MATRIX SOLUTION
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
        STCOUT(K) = STCIN(K) + CI(K)
      END DO

! ----------------------------------------------------------------------
! END SUBROUTINE HSTEP
! ----------------------------------------------------------------------
      END SUBROUTINE HSTEP

      SUBROUTINE NOPAC(ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,                  &
     &                 SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,SHDFAC,        &
     &                 SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,       &
     &                 STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,                 &
     &                 SH2O,SLOPE,KDT,FRZFACT,PSISAT,ZSOIL,             &
     &                 DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,           &
     &                 RUNOFF3,EDIR,EC,ET,ETT,NROOT,ICE,RTDIS,          &
     &                 QUARTZ,FXEXP,CSOIL,                              &
     &                 BETA,DRIP,DEW,FLX1,FLX2,FLX3)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE NOPAC
! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDATE SOIL MOISTURE
! CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN NO SNOW PACK IS
! PRESENT.
! ----------------------------------------------------------------------
      INTEGER ICE
      INTEGER NROOT
      INTEGER NSOIL

      REAL BEXP
      REAL BETA
      REAL CFACTR
      REAL CMC
      REAL CMCMAX
      REAL CP
      REAL CSOIL
      REAL DEW
      REAL DF1
      REAL DKSAT
      REAL DRIP
      REAL DT
      REAL DWSAT
      REAL EC
      REAL EDIR
      REAL EPSCA
      REAL ETA
      REAL ETA1
      REAL ETP
      REAL ETP1
      REAL ET(NSOIL)
      REAL ETT
      REAL FDOWN
      REAL F1
      REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FRZFACT
      REAL KDT
      REAL PC
      REAL PRCP
      REAL PRCP1
      REAL PSISAT
      REAL Q2
      REAL QUARTZ
      REAL RCH
      REAL RR
      REAL RTDIS(NSOIL)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RUNOFF3
      REAL SSOIL
      REAL SBETA
      REAL SFCTMP
      REAL SHDFAC
      REAL SH2O(NSOIL)
      REAL SIGMA
      REAL SLOPE
      REAL SMC(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL STC(NSOIL)
      REAL T1
      REAL T24
      REAL TBOT
      REAL TH2
      REAL YY
      REAL YYNUM
      REAL ZBOT
      REAL ZSOIL(NSOIL)
      REAL ZZ1

      REAL EC1
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL ETT1

      INTEGER K

      PARAMETER(CP = 1004.5)
      PARAMETER(SIGMA = 5.67E-8)

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE:
! CONVERT ETP FROM KG M-2 S-1 TO MS-1 AND INITIALIZE DEW.
! ----------------------------------------------------------------------
      PRCP1 = PRCP * 0.001
      ETP1 = ETP * 0.001
      DEW = 0.0

      EDIR = 0.
      EDIR1 = 0.
      EC = 0.
      EC1 = 0.
      DO K = 1,NSOIL
        ET(K) = 0.
        ET1(K) = 0.
      END DO
      ETT = 0.
      ETT1 = 0.

      IF (ETP .GT. 0.0) THEN

! ----------------------------------------------------------------------
! CONVERT PRCP FROM 'KG M-2 S-1' TO 'M S-1'.
! ----------------------------------------------------------------------
           CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,                &
     &                 SH2O,SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,          &
     &                 SMCREF,SHDFAC,CMCMAX,                            &
     &                 SMCDRY,CFACTR,                                   &
     &                 EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)
           CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                    &
     &                 SH2O,SLOPE,KDT,FRZFACT,                          &
     &                 SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                  &
     &                 SHDFAC,CMCMAX,                                   &
     &                 RUNOFF1,RUNOFF2,RUNOFF3,                         &
     &                 EDIR1,EC1,ET1,                                   &
     &                 DRIP)

! ----------------------------------------------------------------------
!       CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1
! ----------------------------------------------------------------------
!        ETA = ETA1 * 1000.0

! ----------------------------------------------------------------------
!        EDIR = EDIR1 * 1000.0
!        EC = EC1 * 1000.0
!        ETT = ETT1 * 1000.0
!        ET(1) = ET1(1) * 1000.0
!        ET(2) = ET1(2) * 1000.0
!        ET(3) = ET1(3) * 1000.0
!        ET(4) = ET1(4) * 1000.0
! ----------------------------------------------------------------------

      ELSE

! ----------------------------------------------------------------------
! IF ETP < 0, ASSUME DEW FORMS (TRANSFORM ETP1 INTO DEW AND REINITIALIZE
! ETP1 TO ZERO).
! ----------------------------------------------------------------------
        DEW = -ETP1
!        ETP1 = 0.0

! ----------------------------------------------------------------------
! CONVERT PRCP FROM 'KG M-2 S-1' TO 'M S-1' AND ADD DEW AMOUNT.
! ----------------------------------------------------------------------
        PRCP1 = PRCP1 + DEW
!
!      CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
!     &            SH2O,SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,
!     &            SMCREF,SHDFAC,CMCMAX,
!     &            SMCDRY,CFACTR, 
!     &            EDIR1,EC1,ET1,ETT,SFCTMP,Q2,NROOT,RTDIS,FXEXP)
      CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                         &
     &            SH2O,SLOPE,KDT,FRZFACT,                               &
     &            SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                       &
     &            SHDFAC,CMCMAX,                                        &
     &            RUNOFF1,RUNOFF2,RUNOFF3,                              &
     &            EDIR1,EC1,ET1,                                        &
     &            DRIP)

! ----------------------------------------------------------------------
! CONVERT MODELED EVAPOTRANSPIRATION FROM 'M S-1' TO 'KG M-2 S-1'.
! ----------------------------------------------------------------------
!        ETA = ETA1 * 1000.0

! ----------------------------------------------------------------------
!        EDIR = EDIR1 * 1000.0
!        EC = EC1 * 1000.0
!        ETT = ETT1 * 1000.0
!        ET(1) = ET1(1) * 1000.0
!        ET(2) = ET1(2) * 1000.0
!        ET(3) = ET1(3) * 1000.0
!        ET(4) = ET1(4) * 1000.0
! ----------------------------------------------------------------------

      ENDIF

! ----------------------------------------------------------------------
!       CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1
! ----------------------------------------------------------------------
        ETA = ETA1 * 1000.0

! ----------------------------------------------------------------------
      EDIR = EDIR1 * 1000.0
      EC = EC1 * 1000.0
      DO K = 1,NSOIL
        ET(K) = ET1(K) * 1000.0
!        ET(1) = ET1(1) * 1000.0
!        ET(2) = ET1(2) * 1000.0
!        ET(3) = ET1(3) * 1000.0
!        ET(4) = ET1(4) * 1000.0
      ENDDO
      ETT = ETT1 * 1000.0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! BASED ON ETP AND E VALUES, DETERMINE BETA
! ----------------------------------------------------------------------
      IF ( ETP .LE. 0.0 ) THEN
        BETA = 0.0
        IF ( ETP .LT. 0.0 ) THEN
          BETA = 1.0
!          ETA = ETP
        ENDIF
      ELSE
        BETA = ETA / ETP
      ENDIF

! ----------------------------------------------------------------------
! GET SOIL THERMAL DIFFUXIVITY/CONDUCTIVITY FOR TOP SOIL LYR,
! CALC. ADJUSTED TOP LYR SOIL TEMP AND ADJUSTED SOIL FLUX, THEN
! CALL SHFLX TO COMPUTE/UPDATE SOIL HEAT FLUX AND SOIL TEMPS.
! ----------------------------------------------------------------------
      CALL TDFCND (DF1,SMC(1),QUARTZ,SMCMAX,SH2O(1))

! ----------------------------------------------------------------------
! VEGETATION GREENNESS FRACTION REDUCTION IN SUBSURFACE HEAT FLUX 
! VIA REDUCTION FACTOR, WHICH IS CONVENIENT TO APPLY HERE TO THERMAL 
! DIFFUSIVITY THAT IS LATER USED IN HRT TO COMPUTE SUB SFC HEAT FLUX
! (SEE ADDITIONAL COMMENTS ON VEG EFFECT SUB-SFC HEAT FLX IN 
! ROUTINE SFLX)
! ----------------------------------------------------------------------
      DF1 = DF1 * EXP(SBETA*SHDFAC)

! ----------------------------------------------------------------------
! COMPUTE INTERMEDIATE TERMS PASSED TO ROUTINE HRT (VIA ROUTINE 
! SHFLX BELOW) FOR USE IN COMPUTING SUBSURFACE HEAT FLUX IN HRT
! ----------------------------------------------------------------------
      YYNUM = FDOWN - SIGMA * T24
      YY = SFCTMP + (YYNUM/RCH+TH2-SFCTMP-BETA*EPSCA) / RR
      ZZ1 = DF1 / ( -0.5 * ZSOIL(1) * RCH * RR ) + 1.0

      CALL SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,        &
     &            TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,         &
     &            QUARTZ,CSOIL)

! ----------------------------------------------------------------------
! SET FLX1 AND FLX3 (SNOPACK PHASE CHANGE HEAT FLUXES) TO ZERO SINCE
! THEY ARE NOT USED HERE IN SNOPAC.  FLX2 (FREEZING RAIN HEAT FLUX) WAS
! SIMILARLY INITIALIZED IN THE PENMAN ROUTINE.
! ----------------------------------------------------------------------
      FLX1 = 0.0
      FLX3 = 0.0

! ----------------------------------------------------------------------
! END SUBROUTINE NOPAC
! ----------------------------------------------------------------------
      END SUBROUTINE NOPAC

      SUBROUTINE PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL, &
     &                   Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,       &
     &                   DQSDT2,FLX2)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE PENMAN
! ----------------------------------------------------------------------
! CALCULATE POTENTIAL EVAPORATION FOR THE CURRENT POINT.  VARIOUS
! PARTIAL SUMS/PRODUCTS ARE ALSO CALCULATED AND PASSED BACK TO THE
! CALLING ROUTINE FOR LATER USE.
! ----------------------------------------------------------------------
      LOGICAL SNOWNG
      LOGICAL FRZGRA

      REAL A
      REAL BETA
      REAL CH
      REAL CP
      REAL CPH2O
      REAL CPICE
      REAL DELTA
      REAL DQSDT2
      REAL ELCP
      REAL EPSCA
      REAL ETP
      REAL FDOWN
      REAL FLX2
      REAL FNET
      REAL LSUBC
      REAL LSUBF
      REAL PRCP
      REAL Q2
      REAL Q2SAT
      REAL R
      REAL RAD
      REAL RCH
      REAL RHO
      REAL RR
      REAL SSOIL
      REAL SFCPRS
      REAL SFCTMP
      REAL SIGMA
      REAL T24
      REAL T2V
      REAL TH2

      PARAMETER(CP = 1004.6)
      PARAMETER(CPH2O = 4.218E+3)
      PARAMETER(CPICE = 2.106E+3)
      PARAMETER(R = 287.04)
      PARAMETER(ELCP = 2.4888E+3)
      PARAMETER(LSUBF = 3.335E+5)
      PARAMETER(LSUBC = 2.501000E+6)
      PARAMETER(SIGMA = 5.67E-8)

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE:
! ----------------------------------------------------------------------
      FLX2 = 0.0

! ----------------------------------------------------------------------
! PREPARE PARTIAL QUANTITIES FOR PENMAN EQUATION.
! ----------------------------------------------------------------------
      DELTA = ELCP * DQSDT2
      T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP
      RR = T24 * 6.48E-8 /(SFCPRS * CH) + 1.0
      RHO = SFCPRS / (R * T2V)
      RCH = RHO * CP * CH

! ----------------------------------------------------------------------
! ADJUST THE PARTIAL SUMS / PRODUCTS WITH THE LATENT HEAT
! EFFECTS CAUSED BY FALLING PRECIPITATION.
! ----------------------------------------------------------------------
      IF (.NOT. SNOWNG) THEN
        IF (PRCP .GT. 0.0) RR = RR + CPH2O*PRCP/RCH
      ELSE
        RR = RR + CPICE*PRCP/RCH
      ENDIF

      FNET = FDOWN - SIGMA*T24 - SSOIL

! ----------------------------------------------------------------------
! INCLUDE THE LATENT HEAT EFFECTS OF FRZNG RAIN CONVERTING TO ICE ON
! IMPACT IN THE CALCULATION OF FLX2 AND FNET.
! ----------------------------------------------------------------------
      IF (FRZGRA) THEN
        FLX2 = -LSUBF * PRCP
        FNET = FNET - FLX2
      ENDIF

! ----------------------------------------------------------------------
! FINISH PENMAN EQUATION CALCULATIONS.
! ----------------------------------------------------------------------
      RAD = FNET/RCH + TH2 - SFCTMP
      A = ELCP * (Q2SAT - Q2)
      EPSCA = (A*RR + RAD*DELTA) / (DELTA + RR)
      ETP = EPSCA * RCH / LSUBC

! ----------------------------------------------------------------------
! END SUBROUTINE PENMAN
! ----------------------------------------------------------------------
      END SUBROUTINE PENMAN

      SUBROUTINE REDPRM (                                               &
     &                   VEGTYP,SOILTYP,SLOPETYP,                       &
     &                   CFACTR,CMCMAX,RSMAX,TOPT,REFKDT,KDT,SBETA,     &
     &                   SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,PSISAT,SLOPE,    &
     &                   SNUP,SALP,BEXP,DKSAT,DWSAT,                    &
     &                   SMCMAX,SMCWLT,SMCREF,                          &
     &                   SMCDRY,F1,QUARTZ,FXEXP,RTDIS,SLDPTH,ZSOIL,     &
     &                   NROOT,NSOIL,Z0,CZIL,LAI,CSOIL,PTU)

      IMPLICIT NONE
! ----------------------------------------------------------------------
! SUBROUTINE REDPRM
! ----------------------------------------------------------------------
! INTERNALLY SET (DEFAULT VALUESS), OR OPTIONALLY READ-IN VIA NAMELIST
! I/O, ALL SOIL AND VEGETATION PARAMETERS REQUIRED FOR THE EXECUSION OF
! THE NOAH LSM.
!
! OPTIONAL NON-DEFAULT PARAMETERS CAN BE READ IN, ACCOMMODATING UP TO 30
! SOIL, VEG, OR SLOPE CLASSES, IF THE DEFAULT MAX NUMBER OF SOIL, VEG,
! AND/OR SLOPE TYPES IS RESET.
!
! FUTURE UPGRADES OF ROUTINE REDPRM MUST EXPAND TO INCORPORATE SOME OF
! THE EMPIRICAL PARAMETERS OF THE FROZEN SOIL AND SNOWPACK PHYSICS (SUCH
! AS IN ROUTINES FRH2O, SNOWPACK, AND SNOW_NEW) NOT YET SET IN THIS
! REDPRM ROUTINE, BUT RATHER SET IN LOWER LEVEL SUBROUTINES.
!
! SET MAXIMUM NUMBER OF SOIL-, VEG-, AND SLOPETYP IN DATA STATEMENT.
! ----------------------------------------------------------------------
      INTEGER MAX_SLOPETYP
      INTEGER MAX_SOILTYP
      INTEGER MAX_VEGTYP

      PARAMETER(MAX_SLOPETYP = 30)
      PARAMETER(MAX_SOILTYP = 30)
      PARAMETER(MAX_VEGTYP = 30)

! ----------------------------------------------------------------------
! NUMBER OF DEFINED SOIL-, VEG-, AND SLOPETYPS USED.
! ----------------------------------------------------------------------
      INTEGER DEFINED_VEG
      INTEGER DEFINED_SOIL
      INTEGER DEFINED_SLOPE

      DATA DEFINED_VEG/27/
      DATA DEFINED_SOIL/19/
      DATA DEFINED_SLOPE/9/

! ----------------------------------------------------------------------
!  SET-UP SOIL PARAMETERS FOR GIVEN SOIL TYPE
!  INPUT: SOLTYP: SOIL TYPE (INTEGER INDEX)
!  OUTPUT: SOIL PARAMETERS:
!    MAXSMC: MAX SOIL MOISTURE CONTENT (POROSITY)
!    REFSMC: REFERENCE SOIL MOISTURE (ONSET OF SOIL MOISTURE
!            STRESS IN TRANSPIRATION)
!    WLTSMC: WILTING PT SOIL MOISTURE CONTENTS
!    DRYSMC: AIR DRY SOIL MOIST CONTENT LIMITS
!    SATPSI: SATURATED SOIL POTENTIAL
!    SATDK:  SATURATED SOIL HYDRAULIC CONDUCTIVITY
!    BB:     THE 'B' PARAMETER
!    SATDW:  SATURATED SOIL DIFFUSIVITY
!    F11:    USED TO COMPUTE SOIL DIFFUSIVITY/CONDUCTIVITY
!    QUARTZ:  SOIL QUARTZ CONTENT
! ----------------------------------------------------------------------
! SOIL  STATSGO
! TYPE  CLASS
! ----  -------
!   1   SAND
!   2   LOAMY SAND
!   3   SANDY LOAM
!   4   SILT LOAM
!   5   SILT
!   6   LOAM
!   7   SANDY CLAY LOAM
!   8   SILTY CLAY LOAM
!   9   CLAY LOAM
!  10   SANDY CLAY
!  11   SILTY CLAY
!  12   CLAY
!  13   ORGANIC MATERIAL
!  14   WATER
!  15   BEDROCK
!  16   OTHER(land-ice)
!  17   PLAYA
!  18   LAVA
!  19   WHITE SAND
! ----------------------------------------------------------------------

      REAL BB(MAX_SOILTYP)
      REAL DRYSMC(MAX_SOILTYP)
      REAL F11(MAX_SOILTYP)
      REAL MAXSMC(MAX_SOILTYP)
      REAL REFSMC(MAX_SOILTYP)
      REAL SATPSI(MAX_SOILTYP)
      REAL SATDK(MAX_SOILTYP)
      REAL SATDW(MAX_SOILTYP)
      REAL WLTSMC(MAX_SOILTYP)
      REAL QTZ(MAX_SOILTYP)

      REAL BEXP
      REAL DKSAT
      REAL DWSAT
      REAL F1
      REAL PTU
      REAL QUARTZ
      REAL REFSMC1
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL WLTSMC1

! ----------------------------------------------------------------------
! SOIL TEXTURE-RELATED ARRAYS.
! ----------------------------------------------------------------------
      DATA MAXSMC/0.395, 0.421, 0.434, 0.476, 0.476, 0.439,             &
     &            0.404, 0.464, 0.465, 0.406, 0.468, 0.457,             &
     &            0.464, 0.464, 0.200, 0.421, 0.457, 0.200,             &
     &            0.395, 0.000, 0.000, 0.000, 0.000, 0.000,             &
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
! ----------------------------------------------------------------------
      DATA SATPSI/0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,       &
     &            0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,       &
     &            0.3548, 0.3548, 0.0350, 0.0363, 0.4677, 0.0350,       &
     &            0.0350, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,       &
     &            0.000,  0.0000, 0.0000, 0.0000, 0.0000, 0.0000/
! ----------------------------------------------------------------------
      DATA SATDK /1.7600E-4, 1.4078E-5, 5.2304E-6, 2.8089E-6, 2.8089E-6,&
     &            3.3770E-6, 4.4518E-6, 2.0348E-6, 2.4464E-6, 7.2199E-6,&
     &            1.3444E-6, 9.7394E-7, 3.3770E-6, 3.3770E-6, 1.4078E-5,&
     &            1.4078E-5, 9.7394E-7, 1.4078E-5, 1.7600E-4,       0.0,&
     &                  0.0,       0.0,       0.0,       0.0,       0.0,&
     &                  0.0,       0.0,       0.0,       0.0,       0.0/
! ----------------------------------------------------------------------
      DATA BB    /4.05,  4.26,  4.74,  5.33,  5.33,  5.25,              &
     &            6.77,  8.72,  8.17, 10.73, 10.39, 11.55,              &
     &            5.25,  5.25,  4.05,  4.26, 11.55,  4.05,              &
     &            4.05,  0.00,  0.00,  0.00,  0.00,  0.00,              &
     &            0.00,  0.00,  0.00,  0.00,  0.00,  0.00/
! ----------------------------------------------------------------------
      DATA QTZ   /0.92, 0.82, 0.60, 0.25, 0.10, 0.40,                   &
     &            0.60, 0.10, 0.35, 0.52, 0.10, 0.25,                   &
     &            0.05, 0.05, 0.07, 0.25, 0.60, 0.52,                   &
     &            0.92, 0.00, 0.00, 0.00, 0.00, 0.00,                   &
     &            0.00, 0.00, 0.00, 0.00, 0.00, 0.00/

! ----------------------------------------------------------------------
! THE FOLLOWING 5 PARAMETERS ARE DERIVED LATER IN REDPRM.F FROM THE SOIL
! DATA, AND ARE JUST GIVEN HERE FOR REFERENCE AND TO FORCE STATIC
! STORAGE ALLOCATION. -DAG LOHMANN, FEB. 2001
! ----------------------------------------------------------------------
! !!!!!!!!!!!!!! The following values in the table are NOT used
! !!!!!!!!!!!!!! and are just given for reference
      DATA REFSMC/0.196, 0.248, 0.282, 0.332, 0.332, 0.301,             &
     &            0.293, 0.368, 0.361, 0.320, 0.388, 0.389,             &
     &            0.319, 0.000, 0.116, 0.248, 0.389, 0.116,             &
     &            0.196, 0.000, 0.000, 0.000, 0.000, 0.000,             &
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
! !!!!!!!!!!!!!! The following values in the table are NOT used
! !!!!!!!!!!!!!! and are just given for reference
      DATA WLTSMC/0.023, 0.028, 0.047, 0.084, 0.084, 0.066,             &
     &            0.069, 0.120, 0.103, 0.100, 0.126, 0.135,             &
     &            0.069, 0.000, 0.012, 0.028, 0.135, 0.012,             &
     &            0.023, 0.000, 0.000, 0.000, 0.000, 0.000,             &
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
! !!!!!!!!!!!!!! The following values in the table are NOT used
! !!!!!!!!!!!!!! and are just given for reference
      DATA DRYSMC/0.023, 0.028, 0.047, 0.084, 0.084, 0.066,             &
     &            0.069, 0.120, 0.103, 0.100, 0.126, 0.135,             &
     &            0.069, 0.000, 0.012, 0.028, 0.135, 0.012,             &
     &            0.023, 0.000, 0.000, 0.000, 0.000, 0.000,             &
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/

! !!!!!!!!!!!!!! The following values in the table are NOT used
! !!!!!!!!!!!!!! and are just given for reference
      DATA SATDW /0.632E-4, 0.517E-5, 0.807E-5, 0.239E-4, 0.239E-4,     &
     &            0.143E-4, 0.101E-4, 0.236E-4, 0.113E-4, 0.186E-4,     &
     &            0.966E-5, 0.115E-4, 0.136E-4,      0.0, 0.998E-5,     &
     &            0.517E-5, 0.115E-4, 0.998E-5, 0.632E-4,      0.0,     &
     &                 0.0,      0.0,      0.0,      0.0,      0.0,     &
     &                 0.0,      0.0,      0.0,      0.0,      0.0/
! !!!!!!!!!!!!!! The following values in the table are NOT used
! !!!!!!!!!!!!!! and are just given for reference
      DATA F11  /-1.090, -1.041, -0.568,  0.162,  0.162, -0.327,        &
     &           -1.535, -1.118, -1.297, -3.211, -1.916, -2.258,        &
     &           -0.201,  0.000, -2.287, -1.041, -2.258, -2.287,        &
     &           -1.090,  0.000,  0.000,  0.000,  0.000,  0.000,        &
     &            0.000,  0.000,  0.000,  0.000,  0.000,  0.000/

! ----------------------------------------------------------------------
! SET-UP VEGETATION PARAMETERS FOR A GIVEN VEGETAION TYPE:
! INPUT: VEGTYP = VEGETATION TYPE (INTEGER INDEX)
! OUPUT: VEGETATION PARAMETERS
!   SHDFAC: VEGETATION GREENNESS FRACTION
!   RSMIN:  MIMIMUM STOMATAL RESISTANCE
!   RGL:    PARAMETER USED IN SOLAR RAD TERM OF
!           CANOPY RESISTANCE FUNCTION
!   HS:     PARAMETER USED IN VAPOR PRESSURE DEFICIT TERM OF
!           CANOPY RESISTANCE FUNCTION
!   SNUP:   THRESHOLD SNOW DEPTH (IN WATER EQUIVALENT M) THAT
!           IMPLIES 100% SNOW COVER
! ----------------------------------------------------------------------
! CLASS USGS-WRF VEGETATION/SURFACE TYPE
!   1   Urban and Built-Up Land
!   2   Dryland Cropland and Pasture
!   3   Irrigated Cropland and Pasture
!   4   Mixed Dryland/Irrigated Cropland and Pasture
!   5   Cropland/Grassland Mosaic
!   6   Cropland/Woodland Mosaic
!   7   Grassland
!   8   Shrubland
!   9   Mixed Shrubland/Grassland
!  10   Savanna
!  11   Deciduous Broadleaf Forest
!  12   Deciduous Needleleaf Forest
!  13   Evergreen Broadleaf Forest
!  14   Evergreen Needleleaf Forest
!  15   Mixed Forest
!  16   Water Bodies
!  17   Herbaceous Wetland
!  18   Wooded Wetland
!  19   Barren or Sparsely Vegetated
!  20   Herbaceous Tundra
!  21   Wooded Tundra
!  22   Mixed Tundra
!  23   Bare Ground Tundra
!  24   Snow or Ice
!  25   Playa
!  26   Lava
!  27   White Sand
! ----------------------------------------------------------------------

      INTEGER NROOT
      INTEGER NROOT_DATA(MAX_VEGTYP)

      REAL FRZFACT
      REAL HS
      REAL HSTBL(MAX_VEGTYP)
      REAL LAI
      REAL LAI_DATA(MAX_VEGTYP)
      REAL PSISAT
      REAL RSMIN
      REAL RGL
      REAL RGLTBL(MAX_VEGTYP)
      REAL RSMTBL(MAX_VEGTYP)
      REAL SHDFAC
      REAL SNUP
      REAL SNUPX(MAX_VEGTYP)
      REAL Z0
      REAL Z0_DATA(MAX_VEGTYP)

! ----------------------------------------------------------------------
! VEGETATION CLASS-RELATED ARRAYS
! ----------------------------------------------------------------------
!      DATA NROOT_DATA /2,3,3,3,3,3,3,3,3,3,
!     &                 4,4,4,4,4,2,2,2,2,3,
!     &                 3,3,2,2,2,2,2,0,0,0/
      DATA NROOT_DATA /1,3,3,3,3,3,3,3,3,3,                             &
     &  	       4,4,4,4,4,0,2,2,1,3,                             &
     &  	       3,3,2,1,1,1,1,0,0,0/
      DATA RSMTBL /200.0,  70.0,  70.0,  70.0,  70.0,  70.0,            &
     &              70.0, 300.0, 170.0,  70.0, 100.0, 150.0,            &
     &             150.0, 125.0, 125.0, 100.0,  40.0, 100.0,            &
     &             300.0, 150.0, 150.0, 150.0, 200.0, 200.0,            &
     &              40.0, 100.0, 300.0,   0.0,   0.0,   0.0/
      DATA RGLTBL /100.0, 100.0, 100.0, 100.0, 100.0,  65.0,            &
     &             100.0, 100.0, 100.0,  65.0,  30.0,  30.0,            &
     &              30.0,  30.0,  30.0,  30.0, 100.0,  30.0,            &
     &             100.0, 100.0, 100.0, 100.0, 100.0, 100.0,            &
     &             100.0, 100.0, 100.0,   0.0,   0.0,   0.0/
      DATA HSTBL /42.00, 36.25, 36.25, 36.25, 36.25, 44.14,             &
     &            36.35, 42.00, 39.18, 54.53, 54.53, 47.35,             &
     &            41.69, 47.35, 51.93, 51.75, 60.00, 51.93,             &
     &            42.00, 42.00, 42.00, 42.00, 42.00, 42.00,             &
     &            36.25, 42.00, 42.00,  0.00,  0.00,  0.00/
      DATA SNUPX /0.020, 0.020, 0.020, 0.020, 0.020, 0.020,             &
     &            0.020, 0.020, 0.020, 0.040, 0.040, 0.040,             &
     &            0.040, 0.040, 0.040, 0.010, 0.013, 0.020,             &
     &            0.013, 0.020, 0.020, 0.020, 0.020, 0.013,             &
     &            0.013, 0.013, 0.013, 0.000, 0.000, 0.000/
      DATA Z0_DATA / 1.00,  0.07,  0.07,  0.07,  0.07,  0.15,           &
     &               0.08,  0.03,  0.05,  0.86,  0.80,  0.85,           &
     &               2.65,  1.09,  0.80, 0.001,  0.04,  0.05,           &
     &               0.01,  0.04,  0.06,  0.05,  0.03, 0.001,           &
     &               0.01,  0.15,  0.01,  0.00,  0.00,  0.00/
      DATA LAI_DATA /4.0, 4.0, 4.0, 4.0, 4.0, 4.0,                      &
     &               4.0, 4.0, 4.0, 4.0, 4.0, 4.0,                      &
     &               4.0, 4.0, 4.0, 4.0, 4.0, 4.0,                      &
     &               4.0, 4.0, 4.0, 4.0, 4.0, 4.0,                      &
     &               4.0, 4.0, 4.0, 0.0, 0.0, 0.0/

! ----------------------------------------------------------------------
! CLASS PARAMETER 'SLOPETYP' WAS INCLUDED TO ESTIMATE LINEAR RESERVOIR
! COEFFICIENT 'SLOPE' TO THE BASEFLOW RUNOFF OUT OF THE BOTTOM LAYER.
! LOWEST CLASS (SLOPETYP=0) MEANS HIGHEST SLOPE PARAMETER = 1.
! DEFINITION OF SLOPETYP FROM 'ZOBLER' SLOPE TYPE:
! SLOPE CLASS  PERCENT SLOPE
! 1            0-8
! 2            8-30
! 3            > 30
! 4            0-30
! 5            0-8 & > 30
! 6            8-30 & > 30
! 7            0-8, 8-30, > 30
! 9            GLACIAL ICE
! BLANK        OCEAN/SEA
! ----------------------------------------------------------------------
! NOTE:
! CLASS 9 FROM 'ZOBLER' FILE SHOULD BE REPLACED BY 8 AND 'BLANK' 9
! ----------------------------------------------------------------------
      REAL SLOPE
      REAL SLOPE_DATA(MAX_SLOPETYP)

      DATA SLOPE_DATA /0.1,  0.6, 1.0, 0.35, 0.55, 0.8,                 &
     &                 0.63, 0.0, 0.0, 0.0,  0.0,  0.0,                 &
     &                 0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,                 &
     &                 0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,                 &
     &                 0.0 , 0.0, 0.0, 0.0,  0.0,  0.0/

! ----------------------------------------------------------------------
! SET NAMELIST FILE NAME
! ----------------------------------------------------------------------
      CHARACTER*50 NAMELIST_NAME

! ----------------------------------------------------------------------
! SET UNIVERSAL PARAMETERS (NOT DEPENDENT ON SOIL, VEG, SLOPE TYPE)
! ----------------------------------------------------------------------
      INTEGER I
      INTEGER NSOIL
      INTEGER SLOPETYP
      INTEGER SOILTYP
      INTEGER VEGTYP

      INTEGER BARE
!      DATA BARE /11/
      DATA BARE /19/

      LOGICAL LPARAM
      DATA LPARAM /.TRUE./

      LOGICAL LFIRST
      DATA LFIRST /.TRUE./

! ----------------------------------------------------------------------
! PARAMETER USED TO CALCULATE ROUGHNESS LENGTH OF HEAT.
! ----------------------------------------------------------------------
      REAL CZIL
      REAL CZIL_DATA
!   changed in version 2.6 June 2nd 2003
!      DATA CZIL_DATA /0.2/
      DATA CZIL_DATA /0.1/

! ----------------------------------------------------------------------
! PARAMETER USED TO CALUCULATE VEGETATION EFFECT ON SOIL HEAT FLUX.
! ----------------------------------------------------------------------
      REAL SBETA
      REAL SBETA_DATA
      DATA SBETA_DATA /-2.0/

! ----------------------------------------------------------------------
! BARE SOIL EVAPORATION EXPONENT USED IN DEVAP.
! ----------------------------------------------------------------------
      REAL FXEXP
      REAL FXEXP_DATA
      DATA FXEXP_DATA /2.0/

! ----------------------------------------------------------------------
! SOIL HEAT CAPACITY [J M-3 K-1]
! ----------------------------------------------------------------------
      REAL CSOIL
      REAL CSOIL_DATA
!      DATA CSOIL_DATA /1.26E+6/
      DATA CSOIL_DATA /2.00E+6/

! ----------------------------------------------------------------------
! SPECIFY SNOW DISTRIBUTION SHAPE PARAMETER SALP - SHAPE PARAMETER OF
! DISTRIBUTION FUNCTION OF SNOW COVER. FROM ANDERSON'S DATA (HYDRO-17)
! BEST FIT IS WHEN SALP = 2.6
! ----------------------------------------------------------------------
      REAL SALP
      REAL SALP_DATA
!     changed for version 2.6 June 2nd 2003
!      DATA SALP_DATA /2.6/
      DATA SALP_DATA /4.0/

! ----------------------------------------------------------------------
! KDT IS DEFINED BY REFERENCE REFKDT AND DKSAT; REFDK=2.E-6 IS THE SAT.
! DK. VALUE FOR THE SOIL TYPE 2
! ----------------------------------------------------------------------
      REAL REFDK
      REAL REFDK_DATA
      DATA REFDK_DATA /2.0E-6/

      REAL REFKDT
      REAL REFKDT_DATA
      DATA REFKDT_DATA /3.0/

      REAL FRZX
      REAL KDT

! ----------------------------------------------------------------------
! FROZEN GROUND PARAMETER, FRZK, DEFINITION: ICE CONTENT THRESHOLD ABOVE
! WHICH FROZEN SOIL IS IMPERMEABLE REFERENCE VALUE OF THIS PARAMETER FOR
! THE LIGHT CLAY SOIL (TYPE=3) FRZK = 0.15 M.
! ----------------------------------------------------------------------
      REAL FRZK
      REAL FRZK_DATA
      DATA FRZK_DATA /0.15/

      REAL RTDIS(NSOIL)
      REAL SLDPTH(NSOIL)
      REAL ZSOIL(NSOIL)

! ----------------------------------------------------------------------
! SET TWO CANOPY WATER PARAMETERS.
! ----------------------------------------------------------------------
      REAL CFACTR
      REAL CFACTR_DATA
      DATA CFACTR_DATA /0.5/

      REAL CMCMAX
      REAL CMCMAX_DATA
      DATA CMCMAX_DATA /0.5E-3/

! ----------------------------------------------------------------------
! SET MAX. STOMATAL RESISTANCE.
! ----------------------------------------------------------------------
      REAL RSMAX
      REAL RSMAX_DATA
      DATA RSMAX_DATA /5000.0/

! ----------------------------------------------------------------------
! SET OPTIMUM TRANSPIRATION AIR TEMPERATURE.
! ----------------------------------------------------------------------
      REAL TOPT
      REAL TOPT_DATA
      DATA TOPT_DATA /298.0/

! ----------------------------------------------------------------------
! SPECIFY DEPTH[M] OF LOWER BOUNDARY SOIL TEMPERATURE.
! ----------------------------------------------------------------------
      REAL ZBOT
      REAL ZBOT_DATA
!     changed for version 2.5.2
!      DATA ZBOT_DATA /-3.0/
      DATA ZBOT_DATA /-8.0/

! ----------------------------------------------------------------------
! SET TWO SOIL MOISTURE WILT, SOIL MOISTURE REFERENCE PARAMETERS
! ----------------------------------------------------------------------
      REAL SMLOW
      REAL SMLOW_DATA
      DATA SMLOW_DATA /0.5/

      REAL SMHIGH
      REAL SMHIGH_DATA
!     changed in 2.6 from 3 to 6 on June 2nd 2003
      DATA SMHIGH_DATA /3.0/
!     DATA SMHIGH_DATA /6.0/

! ----------------------------------------------------------------------
! NAMELIST DEFINITION:
! ----------------------------------------------------------------------
      NAMELIST /SOIL_VEG/ SLOPE_DATA, RSMTBL, RGLTBL, HSTBL, SNUPX,     &
     &  BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW,          &
     &  WLTSMC, QTZ, LPARAM, ZBOT_DATA, SALP_DATA, CFACTR_DATA,         &
     &  CMCMAX_DATA, SBETA_DATA, RSMAX_DATA, TOPT_DATA,                 &
     &  REFDK_DATA, FRZK_DATA, BARE, DEFINED_VEG, DEFINED_SOIL,         &
     &  DEFINED_SLOPE, FXEXP_DATA, NROOT_DATA, REFKDT_DATA, Z0_DATA,    &
     &  CZIL_DATA, LAI_DATA, CSOIL_DATA

! ----------------------------------------------------------------------
! READ NAMELIST FILE TO OVERRIDE DEFAULT PARAMETERS ONLY ONCE.
! NAMELIST_NAME must be 50 characters or less.
! ----------------------------------------------------------------------
      IF (LFIRST) THEN
!        WRITE(*,*) 'READ NAMELIST'
!        OPEN(58, FILE = 'namelist_filename.txt')
!         READ(58,'(A)') NAMELIST_NAME
!         CLOSE(58)
!         WRITE(*,*) 'Namelist Filename is ', NAMELIST_NAME
!         OPEN(59, FILE = NAMELIST_NAME)
! 50      CONTINUE
!         READ(59, SOIL_VEG, END=100)
!         IF (LPARAM) GOTO 50
! 100     CONTINUE
!         CLOSE(59)
!         WRITE(*,NML=SOIL_VEG)
         LFIRST = .FALSE.
         IF (DEFINED_SOIL .GT. MAX_SOILTYP) THEN
            WRITE(*,*) 'Warning: DEFINED_SOIL too large in namelist'
            STOP 222
         ENDIF
         IF (DEFINED_VEG .GT. MAX_VEGTYP) THEN
            WRITE(*,*) 'Warning: DEFINED_VEG too large in namelist'
            STOP 222
         ENDIF
         IF (DEFINED_SLOPE .GT. MAX_SLOPETYP) THEN
            WRITE(*,*) 'Warning: DEFINED_SLOPE too large in namelist'
            STOP 222
         ENDIF
         
         SMLOW = SMLOW_DATA
         SMHIGH = SMHIGH_DATA
         
         DO I = 1,DEFINED_SOIL
            SATDW(I)  = BB(I)*SATDK(I)*(SATPSI(I)/MAXSMC(I))
            F11(I) = ALOG10(SATPSI(I)) + BB(I)*ALOG10(MAXSMC(I)) + 2.0
            REFSMC1 = MAXSMC(I)*(5.79E-9/SATDK(I))                      &
     &           **(1.0/(2.0*BB(I)+3.0))
            REFSMC(I) = REFSMC1 + (MAXSMC(I)-REFSMC1) / SMHIGH
            WLTSMC1 = MAXSMC(I) * (200.0/SATPSI(I))**(-1.0/BB(I))
            WLTSMC(I) = WLTSMC1 - SMLOW * WLTSMC1
            
! ----------------------------------------------------------------------
! CURRENT VERSION DRYSMC VALUES THAT EQUATE TO WLTSMC.
! FUTURE VERSION COULD LET DRYSMC BE INDEPENDENTLY SET VIA NAMELIST.
! ----------------------------------------------------------------------
            DRYSMC(I) = WLTSMC(I)
         END DO
         
! ----------------------------------------------------------------------
! END LFIRST BLOCK
! ----------------------------------------------------------------------
      ENDIF
      
      IF (SOILTYP .GT. DEFINED_SOIL) THEN
        WRITE(*,*) 'Warning: too many soil types'
        STOP 333
      ENDIF
      IF (VEGTYP .GT. DEFINED_VEG) THEN
        WRITE(*,*) 'Warning: too many veg types'
        STOP 333
      ENDIF
      IF (SLOPETYP .GT. DEFINED_SLOPE) THEN
        WRITE(*,*) 'Warning: too many slope types'
        STOP 333
      ENDIF

! ----------------------------------------------------------------------
! SET-UP UNIVERSAL PARAMETERS (NOT DEPENDENT ON SOILTYP, VEGTYP OR
! SLOPETYP)
! ----------------------------------------------------------------------
      ZBOT = ZBOT_DATA
      SALP = SALP_DATA
      CFACTR = CFACTR_DATA
      CMCMAX = CMCMAX_DATA
      SBETA = SBETA_DATA
      RSMAX = RSMAX_DATA
      TOPT = TOPT_DATA
      REFDK = REFDK_DATA
      FRZK = FRZK_DATA
      FXEXP = FXEXP_DATA
      REFKDT = REFKDT_DATA
      CZIL = CZIL_DATA
      CSOIL = CSOIL_DATA

! ----------------------------------------------------------------------
!  SET-UP SOIL PARAMETERS
! ----------------------------------------------------------------------
      BEXP = BB(SOILTYP)
      DKSAT = SATDK(SOILTYP)
      DWSAT = SATDW(SOILTYP)
      F1 = F11(SOILTYP)
      KDT = REFKDT * DKSAT/REFDK
      PSISAT = SATPSI(SOILTYP)
      QUARTZ = QTZ(SOILTYP)
      SMCDRY = DRYSMC(SOILTYP)
      SMCMAX = MAXSMC(SOILTYP)
      SMCREF = REFSMC(SOILTYP)
      SMCWLT = WLTSMC(SOILTYP)
      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)

! ----------------------------------------------------------------------
! TO ADJUST FRZK PARAMETER TO ACTUAL SOIL TYPE: FRZK * FRZFACT
! ----------------------------------------------------------------------
      FRZX = FRZK * FRZFACT

! ----------------------------------------------------------------------
! SET-UP VEGETATION PARAMETERS
! ----------------------------------------------------------------------
      NROOT = NROOT_DATA(VEGTYP)
      SNUP = SNUPX(VEGTYP)
      RSMIN = RSMTBL(VEGTYP)
      RGL = RGLTBL(VEGTYP)
      HS = HSTBL(VEGTYP)
      Z0 = Z0_DATA(VEGTYP)
      LAI = LAI_DATA(VEGTYP)
      IF (VEGTYP .EQ. BARE) SHDFAC = 0.0

      IF (NROOT .GT. NSOIL) THEN
        WRITE(*,*) 'Warning: too many root layers'
        STOP 333
      ENDIF

! ----------------------------------------------------------------------
! CALCULATE ROOT DISTRIBUTION.  PRESENT VERSION ASSUMES UNIFORM
! DISTRIBUTION BASED ON SOIL LAYER DEPTHS.
! ----------------------------------------------------------------------
      DO I = 1,NROOT
        RTDIS(I) = -SLDPTH(I)/ZSOIL(NROOT)
      END DO

! ----------------------------------------------------------------------
!  SET-UP SLOPE PARAMETER
! ----------------------------------------------------------------------
      SLOPE = SLOPE_DATA(SLOPETYP)

! ----------------------------------------------------------------------
! END SUBROUTINE REDPRM
! ----------------------------------------------------------------------
      END SUBROUTINE REDPRM

      SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NSOIL)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE ROSR12
! ----------------------------------------------------------------------
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
! ###                                            ### ###  ###   ###  ###
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
! # .                                          .   # #  .   # = #   .  #
! # .                                          .   # #  .   #   #   .  #
! # .                                          .   # #  .   #   #   .  #
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
! ###                                            ### ###  ###   ###  ###
! ----------------------------------------------------------------------
      INTEGER K
      INTEGER KK
      INTEGER NSOIL
      
      REAL A(NSOIL)
      REAL B(NSOIL)
      REAL C(NSOIL)
      REAL D(NSOIL)
      REAL DELTA(NSOIL)
      REAL P(NSOIL)
      
! ----------------------------------------------------------------------
! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
! ----------------------------------------------------------------------
      C(NSOIL) = 0.0

! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
! ----------------------------------------------------------------------
      P(1) = -C(1) / B(1)
      DELTA(1) = D(1) / B(1)

! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
      DO K = 2,NSOIL
        P(K) = -C(K) * ( 1.0 / (B(K) + A (K) * P(K-1)) )
        DELTA(K) = (D(K)-A(K)*DELTA(K-1))*(1.0/(B(K)+A(K)*P(K-1)))
      END DO

! ----------------------------------------------------------------------
! SET P TO DELTA FOR LOWEST SOIL LAYER
! ----------------------------------------------------------------------
      P(NSOIL) = DELTA(NSOIL)

! ----------------------------------------------------------------------
! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
      DO K = 2,NSOIL
         KK = NSOIL - K + 1
         P(KK) = P(KK) * P(KK+1) + DELTA(KK)
      END DO

! ----------------------------------------------------------------------
! END SUBROUTINE ROSR12
! ----------------------------------------------------------------------
      END SUBROUTINE ROSR12

      SUBROUTINE SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,  &
     &                  TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,   &
     &                  QUARTZ,CSOIL)
      
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! SUBROUTINE SHFLX
! ----------------------------------------------------------------------
! UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON THE THERMAL
! DIFFUSION EQUATION AND UPDATE THE FROZEN SOIL MOISTURE CONTENT BASED
! ON THE TEMPERATURE.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER I
      INTEGER ICE
      INTEGER IFRZ
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)

      REAL BEXP
      REAL CSOIL
      REAL DF1
      REAL DT
      REAL F1
      REAL PSISAT
      REAL QUARTZ
      REAL RHSTS(NSOLD)
      REAL SSOIL
      REAL SH2O(NSOIL)
      REAL SMC(NSOIL)
      REAL SMCMAX
      REAL SMCWLT
      REAL STC(NSOIL)
      REAL STCF(NSOLD)
      REAL T0
      REAL T1
      REAL TBOT
      REAL YY
      REAL ZBOT
      REAL ZSOIL(NSOIL)
      REAL ZZ1

      PARAMETER(T0 = 273.15)

! ----------------------------------------------------------------------
! HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN
! ----------------------------------------------------------------------
      IF (ICE.EQ.1) THEN

! ----------------------------------------------------------------------
! SEA-ICE CASE
! ----------------------------------------------------------------------
         CALL HRTICE (RHSTS,STC,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

         CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)
         
      ELSE

! ----------------------------------------------------------------------
! LAND-MASS CASE
! ----------------------------------------------------------------------
         CALL HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,TBOT,        &
     &             ZBOT,PSISAT,SH2O,DT,                                 &
     &             BEXP,F1,DF1,QUARTZ,CSOIL,AI,BI,CI)
         
         CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)

      ENDIF

      DO I = 1,NSOIL
         STC(I) = STCF(I)
      END DO
      
! ----------------------------------------------------------------------
! IN THE NO SNOWPACK CASE (VIA ROUTINE NOPAC BRANCH,) UPDATE THE GRND
! (SKIN) TEMPERATURE HERE IN RESPONSE TO THE UPDATED SOIL TEMPERATURE 
! PROFILE ABOVE.  (NOTE: INSPECTION OF ROUTINE SNOPAC SHOWS THAT T1
! BELOW IS A DUMMY VARIABLE ONLY, AS SKIN TEMPERATURE IS UPDATED
! DIFFERENTLY IN ROUTINE SNOPAC) 
! ----------------------------------------------------------------------
      T1 = (YY + (ZZ1 - 1.0) * STC(1)) / ZZ1

! ----------------------------------------------------------------------
! CALCULATE SURFACE SOIL HEAT FLUX
! ----------------------------------------------------------------------
      SSOIL = DF1 * (STC(1) - T1) / (0.5 * ZSOIL(1))

! ----------------------------------------------------------------------
! END SUBROUTINE SHFLX
! ----------------------------------------------------------------------
      END SUBROUTINE SHFLX

      SUBROUTINE SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                   &
     &                  SH2O,SLOPE,KDT,FRZFACT,                         &
     &                  SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                 &
     &                  SHDFAC,CMCMAX,                                  &
     &                  RUNOFF1,RUNOFF2,RUNOFF3,                        &
     &                  EDIR1,EC1,ET1,                                  &
     &                  DRIP)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE SMFLX
! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER
! UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH
! PROGNOSTIC EQNS. THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.
! FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND
! CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER I
      INTEGER K
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)

      REAL BEXP
      REAL CMC
      REAL CMCMAX
      REAL DKSAT
      REAL DRIP
      REAL DT
      REAL DUMMY
      REAL DWSAT
      REAL EC1
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL EXCESS
      REAL FRZFACT
      REAL KDT
      REAL PCPDRP
      REAL PRCP1
      REAL RHSCT
      REAL RHSTT(NSOLD)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RUNOFF3
      REAL SHDFAC
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SICE(NSOLD)
      REAL SH2OA(NSOLD)
      REAL SH2OFG(NSOLD)
      REAL SLOPE
      REAL SMCMAX
      REAL SMCWLT
      REAL TRHSCT
      REAL ZSOIL(NSOIL)

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE.
! ----------------------------------------------------------------------
      DUMMY = 0.

! ----------------------------------------------------------------------
! COMPUTE THE RIGHT HAND SIDE OF THE CANOPY EQN TERM ( RHSCT )
! ----------------------------------------------------------------------
      RHSCT = SHDFAC * PRCP1 - EC1

! ----------------------------------------------------------------------
! CONVERT RHSCT (A RATE) TO TRHSCT (AN AMOUNT) AND ADD IT TO EXISTING
! CMC.  IF RESULTING AMT EXCEEDS MAX CAPACITY, IT BECOMES DRIP AND WILL
! FALL TO THE GRND.
! ----------------------------------------------------------------------
      DRIP = 0.
      TRHSCT = DT * RHSCT
      EXCESS = CMC + TRHSCT
      IF (EXCESS .GT. CMCMAX) DRIP = EXCESS - CMCMAX

! ----------------------------------------------------------------------
! PCPDRP IS THE COMBINED PRCP1 AND DRIP (FROM CMC) THAT GOES INTO THE
! SOIL
! ----------------------------------------------------------------------
      PCPDRP = (1. - SHDFAC) * PRCP1 + DRIP / DT

! ----------------------------------------------------------------------
! STORE ICE CONTENT AT EACH SOIL LAYER BEFORE CALLING SRT & SSTEP
! ----------------------------------------------------------------------
      DO I = 1,NSOIL
        SICE(I) = SMC(I) - SH2O(I)
      END DO
            
! ----------------------------------------------------------------------
! CALL SUBROUTINES SRT AND SSTEP TO SOLVE THE SOIL MOISTURE
! TENDENCY EQUATIONS. 
!
! IF THE INFILTRATING PRECIP RATE IS NONTRIVIAL,
!   (WE CONSIDER NONTRIVIAL TO BE A PRECIP TOTAL OVER THE TIME STEP 
!    EXCEEDING ONE ONE-THOUSANDTH OF THE WATER HOLDING CAPACITY OF 
!    THE FIRST SOIL LAYER)
! THEN CALL THE SRT/SSTEP SUBROUTINE PAIR TWICE IN THE MANNER OF 
!   TIME SCHEME "F" (IMPLICIT STATE, AVERAGED COEFFICIENT)
!   OF SECTION 2 OF KALNAY AND KANAMITSU (1988, MWR, VOL 116, 
!   PAGES 1945-1958)TO MINIMIZE 2-DELTA-T OSCILLATIONS IN THE 
!   SOIL MOISTURE VALUE OF THE TOP SOIL LAYER THAT CAN ARISE BECAUSE
!   OF THE EXTREME NONLINEAR DEPENDENCE OF THE SOIL HYDRAULIC 
!   DIFFUSIVITY COEFFICIENT AND THE HYDRAULIC CONDUCTIVITY ON THE
!   SOIL MOISTURE STATE
! OTHERWISE CALL THE SRT/SSTEP SUBROUTINE PAIR ONCE IN THE MANNER OF
!   TIME SCHEME "D" (IMPLICIT STATE, EXPLICIT COEFFICIENT) 
!   OF SECTION 2 OF KALNAY AND KANAMITSU
! PCPDRP IS UNITS OF KG/M**2/S OR MM/S, ZSOIL IS NEGATIVE DEPTH IN M 
! ----------------------------------------------------------------------
!      IF ( PCPDRP .GT. 0.0 ) THEN
      IF ( (PCPDRP*DT) .GT. (0.001*1000.0*(-ZSOIL(1))*SMCMAX) ) THEN

! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! SMC STATES REPLACED BY SH2O STATES IN SRT SUBR.  SH2O & SICE STATES
! INCLUDED IN SSTEP SUBR.  FROZEN GROUND CORRECTION FACTOR, FRZFACT
! ADDED.  ALL WATER BALANCE CALCULATIONS USING UNFROZEN WATER
! ----------------------------------------------------------------------
        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,         &
     &            DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                      &
     &            RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)
         
        CALL SSTEP (SH2OFG,SH2O,DUMMY,RHSTT,RHSCT,DT,NSOIL,SMCMAX,      &
     &              CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)
         
        DO K = 1,NSOIL
          SH2OA(K) = (SH2O(K) + SH2OFG(K)) * 0.5
        END DO
        
        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2OA,NSOIL,PCPDRP,ZSOIL,        &
     &            DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                      &
     &            RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)
         
        CALL SSTEP (SH2O,SH2O,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,          &
     &              CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)
         
      ELSE
         
        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,         &
     &            DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                      &
     &            RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)

        CALL SSTEP (SH2O,SH2O,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,          &
     &              CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)
         
      ENDIF
      
!      RUNOF = RUNOFF

! ----------------------------------------------------------------------
! END SUBROUTINE SMFLX
! ----------------------------------------------------------------------
      END SUBROUTINE SMFLX

      SUBROUTINE SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR)

      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! SUBROUTINE SNFRAC
! ----------------------------------------------------------------------
! CALCULATE SNOW FRACTION (0 -> 1)
! SNEQV   SNOW WATER EQUIVALENT (M)
! SNUP    THRESHOLD SNEQV DEPTH ABOVE WHICH SNCOVR=1
! SALP    TUNING PARAMETER
! SNCOVR  FRACTIONAL SNOW COVER
! ----------------------------------------------------------------------
      REAL SNEQV, SNUP, SALP, SNCOVR, RSNOW, Z0N, SNOWH
      
! ----------------------------------------------------------------------
! SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD (SET IN ROUTINE
! REDPRM) ABOVE WHICH SNOCVR=1.
! ----------------------------------------------------------------------
          IF (SNEQV .LT. SNUP) THEN
            RSNOW = SNEQV/SNUP
            SNCOVR = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
          ELSE
            SNCOVR = 1.0
          ENDIF

          Z0N=0.035 
!     FORMULATION OF DICKINSON ET AL. 1986

!        SNCOVR=SNOWH/(SNOWH + 5*Z0N)

!     FORMULATION OF MARSHALL ET AL. 1994
!        SNCOVR=SNEQV/(SNEQV + 2*Z0N)

! ----------------------------------------------------------------------
! END SUBROUTINE SNFRAC
! ----------------------------------------------------------------------
      END SUBROUTINE SNFRAC

      SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT,   &
     &                   SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,             &
     &                   SBETA,DF1,                                     &
     &                   Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA, &
     &                   SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,ESD,SNDENS,&
     &                   SNOWH,SH2O,SLOPE,KDT,FRZFACT,PSISAT,SNUP,      &
     &                   ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,    &
     &                   RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,   &
     &                   ICE,RTDIS,QUARTZ,FXEXP,CSOIL,                  &
     &                   BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE SNOPAC
! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES & UPDATE SOIL MOISTURE
! CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN A SNOW PACK IS
! PRESENT.
! ----------------------------------------------------------------------
      INTEGER ICE
      INTEGER NROOT
      INTEGER NSOIL

      LOGICAL SNOWNG

      REAL BEXP
      REAL BETA
      REAL CFACTR
      REAL CMC
      REAL CMCMAX
      REAL CP
      REAL CPH2O
      REAL CPICE
      REAL CSOIL
      REAL DENOM
      REAL DEW
      REAL DF1
      REAL DKSAT
      REAL DRIP
      REAL DSOIL
      REAL DTOT
      REAL DT
      REAL DWSAT
      REAL EC
      REAL EDIR
      REAL EPSCA
      REAL ESD
      REAL ESDMIN
      REAL EXPSNO
      REAL EXPSOI
      REAL ETA
      REAL ETA1
      REAL ETP
      REAL ETP1
      REAL ETP2
      REAL ET(NSOIL)
      REAL ETT
      REAL EX
      REAL EXPFAC
      REAL FDOWN
      REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL F1
      REAL KDT
      REAL LSUBF
      REAL LSUBC
      REAL LSUBS
      REAL PC
      REAL PRCP
      REAL PRCP1
      REAL Q2
      REAL RCH
      REAL RR
      REAL RTDIS(NSOIL)
      REAL SSOIL
      REAL SBETA
      REAL SSOIL1
      REAL SFCTMP
      REAL SHDFAC
      REAL SIGMA
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL SNOMLT
      REAL SNOWH
      REAL STC(NSOIL)
      REAL T1
      REAL T11
      REAL T12
      REAL T12A
      REAL T12B
      REAL T24
      REAL TBOT
      REAL ZBOT
      REAL TH2
      REAL YY
      REAL ZSOIL(NSOIL)
      REAL ZZ1
      REAL TFREEZ
      REAL SALP
      REAL SFCPRS
      REAL SLOPE
      REAL FRZFACT
      REAL PSISAT
      REAL SNUP
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RUNOFF3
      REAL QUARTZ
      REAL SNDENS
      REAL SNCOND
      REAL RSNOW
      REAL SNCOVR
      REAL QSAT
      REAL ETP3
      REAL SEH
      REAL T14
!      REAL CSNOW

      REAL EC1
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL ETT1

      REAL ETNS
      REAL ETNS1
      REAL ESNOW
      REAL ESNOW1
      REAL ESNOW2
      REAL ETANRG

      INTEGER K

      REAL SNOEXP

      PARAMETER(CP = 1004.5)
      PARAMETER(CPH2O = 4.218E+3)
      PARAMETER(CPICE = 2.106E+3)
      PARAMETER(ESDMIN = 1.E-6)
      PARAMETER(LSUBF = 3.335E+5)
      PARAMETER(LSUBC = 2.501000E+6)
      PARAMETER(LSUBS = 2.83E+6)
      PARAMETER(SIGMA = 5.67E-8)
      PARAMETER(TFREEZ = 273.15)

!      DATA SNOEXP /1.0/
      DATA SNOEXP /2.0/

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE:
! CONVERT POTENTIAL EVAP (ETP) FROM KG M-2 S-1 TO M S-1 AND THEN TO AN
! AMOUNT (M) GIVEN TIMESTEP (DT) AND CALL IT AN EFFECTIVE SNOWPACK
! REDUCTION AMOUNT, ETP2 (M).  THIS IS THE AMOUNT THE SNOWPACK WOULD BE
! REDUCED DUE TO EVAPORATION FROM THE SNOW SFC DURING THE TIMESTEP.
! EVAPORATION WILL PROCEED AT THE POTENTIAL RATE UNLESS THE SNOW DEPTH
! IS LESS THAN THE EXPECTED SNOWPACK REDUCTION.
! IF SEAICE (ICE=1), BETA REMAINS=1.
! ----------------------------------------------------------------------
      PRCP1 = PRCP1*0.001

!      ETP2 = ETP * 0.001 * DT
      BETA = 1.0
      IF (ICE .NE. 1) THEN
        IF (ESD .LT. ETP2) THEN
!          BETA = ESD / ETP2
        ENDIF
      ENDIF

! ----------------------------------------------------------------------
      EDIR = 0.0
      EDIR1 = 0.0
      EC = 0.0
      EC1 = 0.0
      DO K = 1,NSOIL
        ET(K) = 0.0
        ET1(K) = 0.0
      ENDDO
      ETT = 0.0
      ETT1 = 0.0
      ETNS = 0.0
      ETNS1 = 0.0
      ESNOW = 0.0
      ESNOW1 = 0.0
      ESNOW2 = 0.0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! IF ETP<0 (DOWNWARD) THEN DEWFALL (=FROSTFALL IN THIS CASE).
! ----------------------------------------------------------------------
      DEW = 0.0
      ETP1 = ETP*0.001
      IF (ETP .LT. 0.0) THEN
!        DEW = -ETP * 0.001
        DEW = -ETP1
!        ESNOW2 = ETP * 0.001 * DT
        ESNOW2 = ETP1 * DT
        ETANRG = ETP*((1.-SNCOVR)*LSUBC + SNCOVR*LSUBS)
!      ENDIF
      ELSE
! ----------------------------------------------------------------------
!      ETP1 = 0.0
!        ETP1 = ETP*0.001
        IF (ICE .NE. 1) THEN
          IF (SNCOVR .LT. 1.) THEN
!          CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
            CALL EVAPO (ETNS1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,              &
     &                  SH2O,SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,         &
     &                  SMCREF,SHDFAC,CMCMAX,                           &
     &                  SMCDRY,CFACTR,                                  &
     &                  EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)
!        ENDIF
! ----------------------------------------------------------------------
            EDIR1 = EDIR1*(1.-SNCOVR)
            EC1 = EC1*(1.-SNCOVR)
            DO K = 1,NSOIL
              ET1(K) = ET1(K)*(1.-SNCOVR)
            END DO
            ETT1 = ETT1*(1.-SNCOVR)
            ETNS1 = ETNS1*(1.-SNCOVR)
! ----------------------------------------------------------------------
            EDIR = EDIR1 * 1000.0
            EC = EC1 * 1000.0
            DO K = 1,NSOIL
              ET(K) = ET1(K) * 1000.0
            END DO
            ETT = ETT1 * 1000.0
            ETNS = ETNS1 * 1000.0
! ----------------------------------------------------------------------
          ENDIF
          ESNOW = ETP*SNCOVR
!          ESNOW1 = ETP*0.001
          ESNOW1 = ESNOW*0.001
          ESNOW2 = ESNOW1*DT
          ETANRG = ESNOW*LSUBS + ETNS*LSUBC
        ENDIF
      ENDIF
   
! ----------------------------------------------------------------------
! IF PRECIP IS FALLING, CALCULATE HEAT FLUX FROM SNOW SFC TO NEWLY
! ACCUMULATING PRECIP.  NOTE THAT THIS REFLECTS THE FLUX APPROPRIATE FOR
! THE NOT-YET-UPDATED SKIN TEMPERATURE (T1).  ASSUMES TEMPERATURE OF THE
! SNOWFALL STRIKING THE GOUND IS =SFCTMP (LOWEST MODEL LEVEL AIR TEMP).
! ----------------------------------------------------------------------
      FLX1 = 0.0
      IF (SNOWNG) THEN
        FLX1 = CPICE * PRCP * (T1 - SFCTMP)
      ELSE
        IF (PRCP .GT. 0.0) FLX1 = CPH2O * PRCP * (T1 - SFCTMP)
      ENDIF

! ----------------------------------------------------------------------
! CALCULATE AN 'EFFECTIVE SNOW-GRND SFC TEMP' (T12) BASED ON HEAT FLUXES
! BETWEEN THE SNOW PACK AND THE SOIL AND ON NET RADIATION.
! INCLUDE FLX1 (PRECIP-SNOW SFC) AND FLX2 (FREEZING RAIN LATENT HEAT)
! FLUXES.
! FLX2 REFLECTS FREEZING RAIN LATENT HEAT FLUX USING T1 CALCULATED IN
! PENMAN.
! ----------------------------------------------------------------------
      DSOIL = -(0.5 * ZSOIL(1))
      DTOT = SNOWH + DSOIL
      DENOM = 1.0 + DF1 / (DTOT * RR * RCH)
!      T12A = ( (FDOWN-FLX1-FLX2-SIGMA*T24)/RCH
!     &       + TH2 - SFCTMP - BETA*EPSCA ) / RR
!      T12A = ( (FDOWN-FLX1-FLX2-SIGMA*T24)/RCH
! M.Ek, 24Nov04, add snow emissivity
      T12A = ((FDOWN-FLX1-FLX2                                          &
     &       -(0.95*SNCOVR+(1.0-SNCOVR))*SIGMA*T24)/RCH                 &
     &       + TH2 - SFCTMP - ETANRG/RCH ) / RR
      T12B = DF1 * STC(1) / (DTOT * RR * RCH)
      T12 = (SFCTMP + T12A + T12B) / DENOM      

! ----------------------------------------------------------------------
! IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS AT OR BELOW FREEZING, NO SNOW
! MELT WILL OCCUR.  SET THE SKIN TEMP TO THIS EFFECTIVE TEMP.  REDUCE
! (BY SUBLIMINATION ) OR INCREASE (BY FROST) THE DEPTH OF THE SNOWPACK,
! DEPENDING ON SIGN OF ETP.
! UPDATE SOIL HEAT FLUX (SSOIL) USING NEW SKIN TEMPERATURE (T1)
! SINCE NO SNOWMELT, SET ACCUMULATED SNOWMELT TO ZERO, SET 'EFFECTIVE'
! PRECIP FROM SNOWMELT TO ZERO, SET PHASE-CHANGE HEAT FLUX FROM SNOWMELT
! TO ZERO.
! ----------------------------------------------------------------------
      IF (T12 .LE. TFREEZ) THEN
        T1 = T12
        SSOIL = DF1 * (T1 - STC(1)) / DTOT
!        ESD = MAX(0.0, ESD-ETP2)
        ESD = MAX(0.0, ESD-ESNOW2)
        FLX3 = 0.0
        EX = 0.0
        SNOMLT = 0.0

      ELSE
! ----------------------------------------------------------------------
! IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS ABOVE FREEZING, SNOW MELT
! WILL OCCUR.  CALL THE SNOW MELT RATE,EX AND AMT, SNOMLT.  REVISE THE
! EFFECTIVE SNOW DEPTH.  REVISE THE SKIN TEMP BECAUSE IT WOULD HAVE CHGD
! DUE TO THE LATENT HEAT RELEASED BY THE MELTING. CALC THE LATENT HEAT
! RELEASED, FLX3. SET THE EFFECTIVE PRECIP, PRCP1 TO THE SNOW MELT RATE,
! EX FOR USE IN SMFLX.  ADJUSTMENT TO T1 TO ACCOUNT FOR SNOW PATCHES.
! CALCULATE QSAT VALID AT FREEZING POINT.  NOTE THAT ESAT (SATURATION
! VAPOR PRESSURE) VALUE OF 6.11E+2 USED HERE IS THAT VALID AT FRZZING
! POINT.  NOTE THAT ETP FROM CALL PENMAN IN SFLX IS IGNORED HERE IN
! FAVOR OF BULK ETP OVER 'OPEN WATER' AT FREEZING TEMP.
! UPDATE SOIL HEAT FLUX (S) USING NEW SKIN TEMPERATURE (T1)
! ----------------------------------------------------------------------
!        T1 = TFREEZ * SNCOVR + T12 * (1.0 - SNCOVR)
! mek Feb2004
! non-linear weighting of snow vs non-snow covered portions of gridbox
! so with SNOEXP = 2.0 (>1), surface skin temperature is higher than for
! the linear case (SNOEXP = 1).
        T1 = TFREEZ * SNCOVR**SNOEXP + T12 * (1.0 - SNCOVR**SNOEXP)
!        QSAT = (0.622*6.11E2)/(SFCPRS-0.378*6.11E2)
!        ETP = RCH*(QSAT-Q2)/CP
!        ETP2 = ETP*0.001*DT
        BETA = 1.0
        SSOIL = DF1 * (T1 - STC(1)) / DTOT
	
! ----------------------------------------------------------------------
! IF POTENTIAL EVAP (SUBLIMATION) GREATER THAN DEPTH OF SNOWPACK.
! BETA<1
! SNOWPACK HAS SUBLIMATED AWAY, SET DEPTH TO ZERO.
! ----------------------------------------------------------------------
!        IF (ESD .LE. ETP2) THEN
!        IF (ESD .LE. ESNOW2) THEN
        IF (ESD-ESNOW2 .LE. ESDMIN) THEN
!          BETA = ESD / ETP2
          ESD = 0.0
          EX = 0.0
          SNOMLT = 0.0

        ELSE
! ----------------------------------------------------------------------
! POTENTIAL EVAP (SUBLIMATION) LESS THAN DEPTH OF SNOWPACK, RETAIN
!   BETA=1.
! SNOWPACK (ESD) REDUCED BY POTENTIAL EVAP RATE
! ETP3 (CONVERT TO FLUX)
! ----------------------------------------------------------------------
!          ESD = ESD-ETP2
          ESD = ESD-ESNOW2
!          ETP3 = ETP*LSUBC
          SEH = RCH*(T1-TH2)
          T14 = T1*T1
          T14 = T14*T14
!          FLX3 = FDOWN - FLX1 - FLX2 - SIGMA*T14 - SSOIL - SEH - ETP3
!          FLX3 = FDOWN - FLX1 - FLX2 - SIGMA*T14 - SSOIL - SEH - ETANRG
! M.Ek, 24Nov04, add snow emissivity
          FLX3 = FDOWN - FLX1 - FLX2 -                                  &
     &       (0.95*SNCOVR+(1.0-SNCOVR))*SIGMA*T14 - SSOIL - SEH - ETANRG
          IF (FLX3 .LE. 0.0) FLX3 = 0.0
          EX = FLX3*0.001/LSUBF

! ----------------------------------------------------------------------
! SNOWMELT REDUCTION DEPENDING ON SNOW COVER
! IF SNOW COVER LESS THAN 5% NO SNOWMELT REDUCTION
! ***NOTE:  DOES 'IF' BELOW FAIL TO MATCH THE MELT WATER WITH THE MELT
!           ENERGY?
! ----------------------------------------------------------------------
!          IF (SNCOVR .GT. 0.05) EX = EX * SNCOVR
          SNOMLT = EX * DT

! ----------------------------------------------------------------------
! ESDMIN REPRESENTS A SNOWPACK DEPTH THRESHOLD VALUE BELOW WHICH WE
! CHOOSE NOT TO RETAIN ANY SNOWPACK, AND INSTEAD INCLUDE IT IN SNOWMELT.
! ----------------------------------------------------------------------
          IF (ESD-SNOMLT .GE. ESDMIN) THEN
            ESD = ESD - SNOMLT

          ELSE
! ----------------------------------------------------------------------
! SNOWMELT EXCEEDS SNOW DEPTH
! ----------------------------------------------------------------------
            EX = ESD/DT
            FLX3 = EX*1000.0*LSUBF
            SNOMLT = ESD
            ESD = 0.0

          ENDIF
! ----------------------------------------------------------------------
! END OF 'ESD .LE. ETP2' IF-BLOCK
! ----------------------------------------------------------------------
        ENDIF

        PRCP1 = PRCP1 + EX

! ----------------------------------------------------------------------
! END OF 'T12 .LE. TFREEZ' IF-BLOCK
! ----------------------------------------------------------------------
      ENDIF
         
! ----------------------------------------------------------------------
! FINAL BETA NOW IN HAND, SO COMPUTE EVAPORATION.  EVAP EQUALS ETP
! UNLESS BETA<1.
! ----------------------------------------------------------------------
!      ETA = BETA*ETP

! ----------------------------------------------------------------------
! SET THE EFFECTIVE POTNL EVAPOTRANSP (ETP1) TO ZERO SINCE THIS IS SNOW
! CASE, SO SURFACE EVAP NOT CALCULATED FROM EDIR, EC, OR ETT IN SMFLX
! (BELOW).
! IF SEAICE (ICE=1) SKIP CALL TO SMFLX.
! SMFLX RETURNS UPDATED SOIL MOISTURE VALUES.  IN THIS, THE SNOW PACK
! CASE, ETA1 IS NOT USED IN CALCULATION OF EVAP.
! ----------------------------------------------------------------------
!      ETP1 = 0.0
      IF (ICE .NE. 1) THEN
!        CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
!     &              SH2O,SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,
!     &              SMCREF,SHDFAC,CMCMAX,
!     &              SMCDRY,CFACTR,
!     &              EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)
        CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                       &
     &              SH2O,SLOPE,KDT,FRZFACT,                             &
     &              SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                     &
     &              SHDFAC,CMCMAX,                                      &
     &              RUNOFF1,RUNOFF2,RUNOFF3,                            &
     &              EDIR1,EC1,ET1,                                      &
     &              DRIP)

      ENDIF

! ----------------------------------------------------------------------
!        EDIR = EDIR1 * 1000.0
!        EC = EC1 * 1000.0
!        ETT = ETT1 * 1000.0
!        ET(1) = ET1(1) * 1000.0
!        ET(2) = ET1(2) * 1000.0
!        ET(3) = ET1(3) * 1000.0
!        ET(4) = ET1(4) * 1000.0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! BEFORE CALL SHFLX IN THIS SNOWPACK CASE, SET ZZ1 AND YY ARGUMENTS TO
! SPECIAL VALUES THAT ENSURE THAT GROUND HEAT FLUX CALCULATED IN SHFLX
! MATCHES THAT ALREADY COMPUTER FOR BELOW THE SNOWPACK, THUS THE SFC
! HEAT FLUX TO BE COMPUTED IN SHFLX WILL EFFECTIVELY BE THE FLUX AT THE
! SNOW TOP SURFACE.  T11 IS A DUMMY ARGUEMENT SO WE WILL NOT USE THE
! SKIN TEMP VALUE AS REVISED BY SHFLX.
! ----------------------------------------------------------------------
      ZZ1 = 1.0
      YY = STC(1)-0.5*SSOIL*ZSOIL(1)*ZZ1/DF1
      T11 = T1

! ----------------------------------------------------------------------
! SHFLX WILL CALC/UPDATE THE SOIL TEMPS.  NOTE:  THE SUB-SFC HEAT FLUX 
! (SSOIL1) AND THE SKIN TEMP (T11) OUTPUT FROM THIS SHFLX CALL ARE NOT
! USED  IN ANY SUBSEQUENT CALCULATIONS. RATHER, THEY ARE DUMMY VARIABLES
! HERE IN THE SNOPAC CASE, SINCE THE SKIN TEMP AND SUB-SFC HEAT FLUX ARE
! UPDATED INSTEAD NEAR THE BEGINNING OF THE CALL TO SNOPAC.
! ----------------------------------------------------------------------
      CALL SHFLX (SSOIL1,STC,SMC,SMCMAX,NSOIL,T11,DT,YY,ZZ1,ZSOIL,      &
     &            TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,         &
     &            QUARTZ,CSOIL)
      
! ----------------------------------------------------------------------
! SNOW DEPTH AND DENSITY ADJUSTMENT BASED ON SNOW COMPACTION.  YY IS
! ASSUMED TO BE THE SOIL TEMPERTURE AT THE TOP OF THE SOIL COLUMN.
! ----------------------------------------------------------------------
      IF (ESD .GT. 0.) THEN
        CALL SNOWPACK (ESD,DT,SNOWH,SNDENS,T1,YY)
      ELSE
        ESD = 0.
        SNOWH = 0.
        SNDENS = 0.
        SNCOND = 1.
        SNCOVR = 0.
      ENDIF

! ----------------------------------------------------------------------
! END SUBROUTINE SNOPAC
! ----------------------------------------------------------------------
      END SUBROUTINE SNOPAC

      SUBROUTINE SNOWPACK (ESD,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE SNOWPACK
! ----------------------------------------------------------------------
! CALCULATE COMPACTION OF SNOWPACK UNDER CONDITIONS OF INCREASING SNOW
! DENSITY, AS OBTAINED FROM AN APPROXIMATE SOLUTION OF E. ANDERSON'S
! DIFFERENTIAL EQUATION (3.29), NOAA TECHNICAL REPORT NWS 19, BY VICTOR
! KOREN, 03/25/95.
! ----------------------------------------------------------------------
! ESD     WATER EQUIVALENT OF SNOW (M)
! DTSEC   TIME STEP (SEC)
! SNOWH   SNOW DEPTH (M)
! SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
! TSNOW   SNOW SURFACE TEMPERATURE (K)
! TSOIL   SOIL SURFACE TEMPERATURE (K)
!
! SUBROUTINE WILL RETURN NEW VALUES OF SNOWH AND SNDENS
! ----------------------------------------------------------------------
      INTEGER IPOL, J

      REAL BFAC,C1,C2,SNDENS,DSX,DTHR,DTSEC,DW,SNOWHC,SNOWH,PEXP,TAVGC, &
     &     TSNOW,TSNOWC,TSOIL,TSOILC,ESD,ESDC,ESDCX,G,KN

      PARAMETER(C1 = 0.01, C2=21.0, G=9.81, KN=4000.0)

! ----------------------------------------------------------------------
! CONVERSION INTO SIMULATION UNITS
! ----------------------------------------------------------------------
      SNOWHC = SNOWH*100.
      ESDC = ESD*100.
      DTHR = DTSEC/3600.
      TSNOWC = TSNOW-273.15
      TSOILC = TSOIL-273.15

! ----------------------------------------------------------------------
! CALCULATING OF AVERAGE TEMPERATURE OF SNOW PACK
! ----------------------------------------------------------------------
      TAVGC = 0.5*(TSNOWC+TSOILC)                                    

! ----------------------------------------------------------------------
! CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION
!  SNDENS=DS0*(EXP(BFAC*ESD)-1.)/(BFAC*ESD)
!  BFAC=DTHR*C1*EXP(0.08*TAVGC-C2*DS0)
! NOTE: BFAC*ESD IN SNDENS EQN ABOVE HAS TO BE CAREFULLY TREATED
! NUMERICALLY BELOW:
!   C1 IS THE FRACTIONAL INCREASE IN DENSITY (1/(CM*HR)) 
!   C2 IS A CONSTANT (CM3/G) KOJIMA ESTIMATED AS 21 CMS/G
! ----------------------------------------------------------------------
      IF (ESDC .GT. 1.E-2) THEN
        ESDCX = ESDC
      ELSE
        ESDCX = 1.E-2
      ENDIF
      BFAC = DTHR*C1*EXP(0.08*TAVGC-C2*SNDENS)

!      DSX = SNDENS*((DEXP(BFAC*ESDC)-1.)/(BFAC*ESDC))
! ----------------------------------------------------------------------
! THE FUNCTION OF THE FORM (e**x-1)/x IMBEDDED IN ABOVE EXPRESSION
! FOR DSX WAS CAUSING NUMERICAL DIFFICULTIES WHEN THE DENOMINATOR "x"
! (I.E. BFAC*ESDC) BECAME ZERO OR APPROACHED ZERO (DESPITE THE FACT THAT
! THE ANALYTICAL FUNCTION (e**x-1)/x HAS A WELL DEFINED LIMIT AS 
! "x" APPROACHES ZERO), HENCE BELOW WE REPLACE THE (e**x-1)/x 
! EXPRESSION WITH AN EQUIVALENT, NUMERICALLY WELL-BEHAVED 
! POLYNOMIAL EXPANSION.
!
! NUMBER OF TERMS OF POLYNOMIAL EXPANSION, AND HENCE ITS ACCURACY, 
! IS GOVERNED BY ITERATION LIMIT "IPOL".
!      IPOL GREATER THAN 9 ONLY MAKES A DIFFERENCE ON DOUBLE
!            PRECISION (RELATIVE ERRORS GIVEN IN PERCENT %).
!       IPOL=9, FOR REL.ERROR <~ 1.6 E-6 % (8 SIGNIFICANT DIGITS)
!       IPOL=8, FOR REL.ERROR <~ 1.8 E-5 % (7 SIGNIFICANT DIGITS)
!       IPOL=7, FOR REL.ERROR <~ 1.8 E-4 % ...
! ----------------------------------------------------------------------
      IPOL = 4
      PEXP = 0.
      DO J = IPOL,1,-1
!        PEXP = (1. + PEXP)*BFAC*ESDC/REAL(J+1) 
        PEXP = (1. + PEXP)*BFAC*ESDCX/REAL(J+1) 
      END DO
      PEXP = PEXP + 1.

      DSX = SNDENS*(PEXP)
! ----------------------------------------------------------------------
! ABOVE LINE ENDS POLYNOMIAL SUBSTITUTION
! ----------------------------------------------------------------------
!     END OF KOREAN FORMULATION

!     BASE FORMULATION (COGLEY ET AL., 1990)
!     CONVERT DENSITY FROM G/CM3 TO KG/M3
!       DSM=SNDENS*1000.0
 
!       DSX=DSM+DTSEC*0.5*DSM*G*ESD/
!    &      (1E7*EXP(-0.02*DSM+KN/(TAVGC+273.16)-14.643))
 
!     CONVERT DENSITY FROM KG/M3 TO G/CM3
!       DSX=DSX/1000.0

!     END OF COGLEY ET AL. FORMULATION 

! ----------------------------------------------------------------------
! SET UPPER/LOWER LIMIT ON SNOW DENSITY
! ----------------------------------------------------------------------
      IF (DSX .GT. 0.40) DSX = 0.40
      IF (DSX .LT. 0.05) DSX = 0.05
      SNDENS = DSX
! ----------------------------------------------------------------------
! UPDATE OF SNOW DEPTH AND DENSITY DEPENDING ON LIQUID WATER DURING
! SNOWMELT.  ASSUMED THAT 13% OF LIQUID WATER CAN BE STORED IN SNOW PER
! DAY DURING SNOWMELT TILL SNOW DENSITY 0.40.
! ----------------------------------------------------------------------
      IF (TSNOWC .GE. 0.) THEN
        DW = 0.13*DTHR/24.
        SNDENS = SNDENS*(1.-DW)+DW
        IF (SNDENS .GT. 0.40) SNDENS = 0.40
      ENDIF

! ----------------------------------------------------------------------
! CALCULATE SNOW DEPTH (CM) FROM SNOW WATER EQUIVALENT AND SNOW DENSITY.
! CHANGE SNOW DEPTH UNITS TO METERS
! ----------------------------------------------------------------------
      SNOWHC = ESDC/SNDENS
      SNOWH = SNOWHC*0.01

! ----------------------------------------------------------------------
! END SUBROUTINE SNOWPACK
! ----------------------------------------------------------------------
      END SUBROUTINE SNOWPACK

      SUBROUTINE SNOWZ0 (SNCOVR,Z0)

      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! SUBROUTINE SNOWZ0
! ----------------------------------------------------------------------
! CALCULATE TOTAL ROUGHNESS LENGTH OVER SNOW
! SNCOVR  FRACTIONAL SNOW COVER
! Z0      ROUGHNESS LENGTH (m)
! Z0S     SNOW ROUGHNESS LENGTH:=0.001 (m)
! ----------------------------------------------------------------------
      REAL SNCOVR, Z0, Z0S
!      PARAMETER (Z0S=0.001)
      
! CURRENT NOAH LSM CONDITION - MBEK, 09-OCT-2001
      Z0S = Z0
!
      Z0 = (1-SNCOVR)*Z0 + SNCOVR*Z0S
! ----------------------------------------------------------------------
! END SUBROUTINE SNOWZ0
! ----------------------------------------------------------------------
      END SUBROUTINE SNOWZ0

      SUBROUTINE SNOW_NEW (TEMP,NEWSN,SNOWH,SNDENS)

      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! SUBROUTINE SNOW_NEW
! ----------------------------------------------------------------------
! CALCULATE SNOW DEPTH AND DENSITITY TO ACCOUNT FOR THE NEW SNOWFALL.
! NEW VALUES OF SNOW DEPTH & DENSITY RETURNED.
!
! TEMP    AIR TEMPERATURE (K)
! NEWSN   NEW SNOWFALL (M)
! SNOWH   SNOW DEPTH (M)
! SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
! ----------------------------------------------------------------------
      REAL SNDENS
      REAL DSNEW
      REAL SNOWHC
      REAL HNEWC
      REAL SNOWH
      REAL NEWSN
      REAL NEWSNC
      REAL TEMP 
      REAL TEMPC
      
! ----------------------------------------------------------------------
! CONVERSION INTO SIMULATION UNITS      
! ----------------------------------------------------------------------
      SNOWHC = SNOWH*100.
      NEWSNC = NEWSN*100.
      TEMPC = TEMP-273.15
      
! ----------------------------------------------------------------------
! CALCULATING NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE
! EQUATION FROM GOTTLIB L. 'A GENERAL RUNOFF MODEL FOR SNOWCOVERED
! AND GLACIERIZED BASIN', 6TH NORDIC HYDROLOGICAL CONFERENCE,
! VEMADOLEN, SWEDEN, 1980, 172-177PP.
!-----------------------------------------------------------------------
      IF (TEMPC .LE. -15.) THEN
        DSNEW = 0.05
      ELSE                                                      
        DSNEW = 0.05+0.0017*(TEMPC+15.)**1.5
      ENDIF
      
! ----------------------------------------------------------------------
! ADJUSTMENT OF SNOW DENSITY DEPENDING ON NEW SNOWFALL      
! ----------------------------------------------------------------------
      HNEWC = NEWSNC/DSNEW
      SNDENS = (SNOWHC*SNDENS+HNEWC*DSNEW)/(SNOWHC+HNEWC)
      SNOWHC = SNOWHC+HNEWC
      SNOWH = SNOWHC*0.01
      
! ----------------------------------------------------------------------
! END SUBROUTINE SNOW_NEW
! ----------------------------------------------------------------------
      END SUBROUTINE SNOW_NEW

      SUBROUTINE SRT (RHSTT,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,            &
     &                ZSOIL,DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,            &
     &                RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZX,SICE,AI,BI,CI)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE SRT
! ----------------------------------------------------------------------
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
! WATER DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
! COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER CVFRZ      
      INTEGER IALP1
      INTEGER IOHINF
      INTEGER J
      INTEGER JJ      
      INTEGER K
      INTEGER KS
      INTEGER NSOIL

      REAL ACRT
      REAL AI(NSOLD)
      REAL BEXP
      REAL BI(NSOLD)
      REAL CI(NSOLD)
      REAL DD
      REAL DDT
      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DENOM2
      REAL DICE
      REAL DKSAT
      REAL DMAX(NSOLD)
      REAL DSMDZ
      REAL DSMDZ2
      REAL DT
      REAL DT1
      REAL DWSAT
      REAL EDIR
      REAL ET(NSOIL)
      REAL FCR
      REAL FRZX
      REAL INFMAX
      REAL KDT
      REAL MXSMC
      REAL MXSMC2
      REAL NUMER
      REAL PCPDRP
      REAL PDDUM
      REAL PX
      REAL RHSTT(NSOIL)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL SH2O(NSOIL)
      REAL SH2OA(NSOIL)
      REAL SICE(NSOIL)
      REAL SICEMAX
      REAL SLOPE
      REAL SLOPX
      REAL SMCAV
      REAL SMCMAX
      REAL SMCWLT
      REAL SSTT
      REAL SUM
      REAL VAL
      REAL WCND
      REAL WCND2
      REAL WDF
      REAL WDF2
      REAL ZSOIL(NSOIL)

! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! REFERENCE FROZEN GROUND PARAMETER, CVFRZ, IS A SHAPE PARAMETER OF
! AREAL DISTRIBUTION FUNCTION OF SOIL ICE CONTENT WHICH EQUALS 1/CV.
! CV IS A COEFFICIENT OF SPATIAL VARIATION OF SOIL ICE CONTENT.  BASED
! ON FIELD DATA CV DEPENDS ON AREAL MEAN OF FROZEN DEPTH, AND IT CLOSE
! TO CONSTANT = 0.6 IF AREAL MEAN FROZEN DEPTH IS ABOVE 20 CM.  THAT IS
! WHY PARAMETER CVFRZ = 3 (INT{1/0.6*0.6}).
! CURRENT LOGIC DOESN'T ALLOW CVFRZ BE BIGGER THAN 3
! ----------------------------------------------------------------------
        PARAMETER(CVFRZ = 3)
        
! ----------------------------------------------------------------------
! DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF.  INCLUDE THE
! INFILTRATION FORMULE FROM SCHAAKE AND KOREN MODEL.
! MODIFIED BY Q DUAN
! ----------------------------------------------------------------------
      IOHINF=1

! ----------------------------------------------------------------------
! LET SICEMAX BE THE GREATEST, IF ANY, FROZEN WATER CONTENT WITHIN SOIL
! LAYERS.
! ----------------------------------------------------------------------
      SICEMAX = 0.0
      DO KS=1,NSOIL
       IF (SICE(KS) .GT. SICEMAX) SICEMAX = SICE(KS)
      END DO

! ----------------------------------------------------------------------
! DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF
! ----------------------------------------------------------------------
      PDDUM = PCPDRP
      RUNOFF1 = 0.0
      IF (PCPDRP .NE. 0.0) THEN

! ----------------------------------------------------------------------
! MODIFIED BY Q. DUAN, 5/16/94
! ----------------------------------------------------------------------
!        IF (IOHINF .EQ. 1) THEN

        DT1 = DT/86400.
        SMCAV = SMCMAX - SMCWLT
        DMAX(1)=-ZSOIL(1)*SMCAV

! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! ----------------------------------------------------------------------
        DICE = -ZSOIL(1) * SICE(1)
          
        DMAX(1)=DMAX(1)*(1.0 - (SH2OA(1)+SICE(1)-SMCWLT)/SMCAV)
        DD=DMAX(1)

        DO KS=2,NSOIL
          
! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! ----------------------------------------------------------------------
          DICE = DICE + ( ZSOIL(KS-1) - ZSOIL(KS) ) * SICE(KS)
         
          DMAX(KS) = (ZSOIL(KS-1)-ZSOIL(KS))*SMCAV
          DMAX(KS) = DMAX(KS)*(1.0 - (SH2OA(KS)+SICE(KS)-SMCWLT)/SMCAV)
          DD = DD+DMAX(KS)
        END DO

! ----------------------------------------------------------------------
! VAL = (1.-EXP(-KDT*SQRT(DT1)))
! IN BELOW, REMOVE THE SQRT IN ABOVE
! ----------------------------------------------------------------------
        VAL = (1.-EXP(-KDT*DT1))
        DDT = DD*VAL
        PX = PCPDRP*DT  
        IF (PX .LT. 0.0) PX = 0.0
        INFMAX = (PX*(DDT/(PX+DDT)))/DT
          
! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! REDUCTION OF INFILTRATION BASED ON FROZEN GROUND PARAMETERS
! ----------------------------------------------------------------------
        FCR = 1. 
        IF (DICE .GT. 1.E-2) THEN 
          ACRT = CVFRZ * FRZX / DICE 
          SUM = 1.
          IALP1 = CVFRZ - 1 
          DO J = 1,IALP1
            K = 1
            DO JJ = J+1,IALP1
              K = K * JJ
            END DO
            SUM = SUM + (ACRT ** ( CVFRZ-J)) / FLOAT (K) 
          END DO
          FCR = 1. - EXP(-ACRT) * SUM 
        ENDIF 
        INFMAX = INFMAX * FCR

! ----------------------------------------------------------------------
! CORRECTION OF INFILTRATION LIMITATION:
! IF INFMAX .LE. HYDROLIC CONDUCTIVITY ASSIGN INFMAX THE VALUE OF
! HYDROLIC CONDUCTIVITY
! ----------------------------------------------------------------------
!         MXSMC = MAX ( SH2OA(1), SH2OA(2) ) 
        MXSMC = SH2OA(1)

        CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,            &
     &               SICEMAX)

        INFMAX = MAX(INFMAX,WCND)
        INFMAX = MIN(INFMAX,PX)

        IF (PCPDRP .GT. INFMAX) THEN
          RUNOFF1 = PCPDRP - INFMAX
          PDDUM = INFMAX
        ENDIF

      ENDIF

! ----------------------------------------------------------------------
! TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN LINE
! BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
! 'MXSMC = MAX(SH2OA(1), SH2OA(2))'
! ----------------------------------------------------------------------
      MXSMC = SH2OA(1)

      CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,              &
     &             SICEMAX)
 
! ----------------------------------------------------------------------
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
! ----------------------------------------------------------------------
      DDZ = 1. / ( -.5 * ZSOIL(2) )
      AI(1) = 0.0
      BI(1) = WDF * DDZ / ( -ZSOIL(1) )
      CI(1) = -BI(1)

! ----------------------------------------------------------------------
! CALC RHSTT FOR THE TOP LAYER AFTER CALC'NG THE VERTICAL SOIL MOISTURE
! GRADIENT BTWN THE TOP AND NEXT TO TOP LAYERS.
! ----------------------------------------------------------------------
      DSMDZ = ( SH2O(1) - SH2O(2) ) / ( -.5 * ZSOIL(2) )
      RHSTT(1) = (WDF * DSMDZ + WCND - PDDUM + EDIR + ET(1))/ZSOIL(1)
      SSTT = WDF * DSMDZ + WCND + EDIR + ET(1)

! ----------------------------------------------------------------------
! INITIALIZE DDZ2
! ----------------------------------------------------------------------
      DDZ2 = 0.0

! ----------------------------------------------------------------------
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABV PROCESS
! ----------------------------------------------------------------------
      DO K = 2,NSOIL
        DENOM2 = (ZSOIL(K-1) - ZSOIL(K))
        IF (K .NE. NSOIL) THEN
          SLOPX = 1.

! ----------------------------------------------------------------------
! AGAIN, TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN
! LINE BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
! 'MXSMC2 = MAX (SH2OA(K), SH2OA(K+1))'
! ----------------------------------------------------------------------
          MXSMC2 = SH2OA(K)

          CALL WDFCND (WDF2,WCND2,MXSMC2,SMCMAX,BEXP,DKSAT,DWSAT,       &
     &                 SICEMAX)

! ----------------------------------------------------------------------
! CALC SOME PARTIAL PRODUCTS FOR LATER USE IN CALC'NG RHSTT
! ----------------------------------------------------------------------
          DENOM = (ZSOIL(K-1) - ZSOIL(K+1))
          DSMDZ2 = (SH2O(K) - SH2O(K+1)) / (DENOM * 0.5)

! ----------------------------------------------------------------------
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
! ----------------------------------------------------------------------
          DDZ2 = 2.0 / DENOM
          CI(K) = -WDF2 * DDZ2 / DENOM2
        ELSE

! ----------------------------------------------------------------------
! SLOPE OF BOTTOM LAYER IS INTRODUCED
! ----------------------------------------------------------------------
          SLOPX = SLOPE

! ----------------------------------------------------------------------
! RETRIEVE THE SOIL WATER DIFFUSIVITY AND HYDRAULIC CONDUCTIVITY FOR
! THIS LAYER
! ----------------------------------------------------------------------
          CALL WDFCND (WDF2,WCND2,SH2OA(NSOIL),SMCMAX,BEXP,DKSAT,DWSAT, &
     &                 SICEMAX)

! ----------------------------------------------------------------------
! CALC A PARTIAL PRODUCT FOR LATER USE IN CALC'NG RHSTT
! ----------------------------------------------------------------------
          DSMDZ2 = 0.0

! ----------------------------------------------------------------------
! SET MATRIX COEF CI TO ZERO
! ----------------------------------------------------------------------
          CI(K) = 0.0
        ENDIF

! ----------------------------------------------------------------------
! CALC RHSTT FOR THIS LAYER AFTER CALC'NG ITS NUMERATOR
! ----------------------------------------------------------------------
        NUMER = (WDF2 * DSMDZ2) + SLOPX * WCND2 - (WDF * DSMDZ)         &
     &    - WCND + ET(K)
        RHSTT(K) = NUMER / (-DENOM2)

! ----------------------------------------------------------------------
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER
! ----------------------------------------------------------------------
        AI(K) = -WDF * DDZ / DENOM2
        BI(K) = -( AI(K) + CI(K) )

! ----------------------------------------------------------------------
! RESET VALUES OF WDF, WCND, DSMDZ, AND DDZ FOR LOOP TO NEXT LYR
! RUNOFF2:  SUB-SURFACE OR BASEFLOW RUNOFF
! ----------------------------------------------------------------------
        IF (K .EQ. NSOIL) THEN
          RUNOFF2 = SLOPX * WCND2
        ENDIF

        IF (K .NE. NSOIL) THEN
          WDF = WDF2
          WCND = WCND2
          DSMDZ = DSMDZ2
          DDZ = DDZ2
        ENDIF
      END DO

! ----------------------------------------------------------------------
! END SUBROUTINE SRT
! ----------------------------------------------------------------------
      END SUBROUTINE SRT

      SUBROUTINE SSTEP (SH2OOUT,SH2OIN,CMC,RHSTT,RHSCT,DT,              &
     &                  NSOIL,SMCMAX,CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,     &
     &                  AI,BI,CI)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE SSTEP
! ----------------------------------------------------------------------
! CALCULATE/UPDATE SOIL MOISTURE CONTENT VALUES AND CANOPY MOISTURE
! CONTENT VALUES.
! ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER I
      INTEGER K 
      INTEGER KK11
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)
      REAL CIin(NSOLD)
      REAL CMC
      REAL CMCMAX
      REAL DDZ
      REAL DT
      REAL RHSCT
      REAL RHSTT(NSOIL)
      REAL RHSTTin(NSOIL)
      REAL RUNOFF3
      REAL SH2OIN(NSOIL)
      REAL SH2OOUT(NSOIL)
      REAL SICE(NSOIL)
      REAL SMC(NSOIL)
      REAL SMCMAX
      REAL STOT
      REAL WPLUS
      REAL ZSOIL(NSOIL)

! ----------------------------------------------------------------------
! CREATE 'AMOUNT' VALUES OF VARIABLES TO BE INPUT TO THE
! TRI-DIAGONAL MATRIX ROUTINE.
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTT(K) = RHSTT(K) * DT
        AI(K) = AI(K) * DT
        BI(K) = 1. + BI(K) * DT
        CI(K) = CI(K) * DT
      END DO

! ----------------------------------------------------------------------
! COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTTin(K) = RHSTT(K)
      END DO
      DO K = 1,NSOIL
        CIin(K) = CI(K)
      END DO

! ----------------------------------------------------------------------
! CALL ROSR12 TO SOLVE THE TRI-DIAGONAL MATRIX
! ----------------------------------------------------------------------
      CALL ROSR12 (CI,AI,BI,CIin,RHSTTin,RHSTT,NSOIL)

! ----------------------------------------------------------------------
! SUM THE PREVIOUS SMC VALUE AND THE MATRIX SOLUTION TO GET A
! NEW VALUE.  MIN ALLOWABLE VALUE OF SMC WILL BE 0.02.
! RUNOFF3: RUNOFF WITHIN SOIL LAYERS
! ----------------------------------------------------------------------
      WPLUS = 0.0
      RUNOFF3 = 0.
      DDZ = -ZSOIL(1)
      
      DO K = 1,NSOIL
        IF (K .NE. 1) DDZ = ZSOIL(K - 1) - ZSOIL(K)
        SH2OOUT(K) = SH2OIN(K) + CI(K) + WPLUS / DDZ

        STOT = SH2OOUT(K) + SICE(K)
        IF (STOT .GT. SMCMAX) THEN
          IF (K .EQ. 1) THEN
            DDZ = -ZSOIL(1)
          ELSE
            KK11 = K - 1
            DDZ = -ZSOIL(K) + ZSOIL(KK11)
          ENDIF
          WPLUS = (STOT-SMCMAX) * DDZ
        ELSE
          WPLUS = 0.
        ENDIF
        SMC(K) = MAX ( MIN(STOT,SMCMAX),0.02 )
        SH2OOUT(K) = MAX((SMC(K)-SICE(K)),0.0)
      END DO

      RUNOFF3 = WPLUS

! ----------------------------------------------------------------------
! UPDATE CANOPY WATER CONTENT/INTERCEPTION (CMC).  CONVERT RHSCT TO 
! AN 'AMOUNT' VALUE AND ADD TO PREVIOUS CMC VALUE TO GET NEW CMC.
! ----------------------------------------------------------------------
      CMC = CMC + DT * RHSCT
      IF (CMC .LT. 1.E-20) CMC=0.0
      CMC = MIN(CMC,CMCMAX)

! ----------------------------------------------------------------------
! END SUBROUTINE SSTEP
! ----------------------------------------------------------------------
      END SUBROUTINE SSTEP

      SUBROUTINE TBND (TU,TB,ZSOIL,ZBOT,K,NSOIL,TBND1)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE TBND
! ----------------------------------------------------------------------
! CALCULATE TEMPERATURE ON THE BOUNDARY OF THE LAYER BY INTERPOLATION OF
! THE MIDDLE LAYER TEMPERATURES
! ----------------------------------------------------------------------
      INTEGER NSOIL
      INTEGER K

      REAL TBND1
      REAL T0
      REAL TU
      REAL TB
      REAL ZB
      REAL ZBOT
      REAL ZUP
      REAL ZSOIL (NSOIL)

      PARAMETER(T0 = 273.15)

! ----------------------------------------------------------------------
! USE SURFACE TEMPERATURE ON THE TOP OF THE FIRST LAYER
! ----------------------------------------------------------------------
      IF (K .EQ. 1) THEN
        ZUP = 0.
      ELSE
        ZUP = ZSOIL(K-1)
      ENDIF

! ----------------------------------------------------------------------
! USE DEPTH OF THE CONSTANT BOTTOM TEMPERATURE WHEN INTERPOLATE
! TEMPERATURE INTO THE LAST LAYER BOUNDARY
! ----------------------------------------------------------------------
      IF (K .EQ. NSOIL) THEN
        ZB = 2.*ZBOT-ZSOIL(K)
      ELSE
        ZB = ZSOIL(K+1)
      ENDIF

! ----------------------------------------------------------------------
! LINEAR INTERPOLATION BETWEEN THE AVERAGE LAYER TEMPERATURES
! ----------------------------------------------------------------------
      TBND1 = TU+(TB-TU)*(ZUP-ZSOIL(K))/(ZUP-ZB)
      
! ----------------------------------------------------------------------
! END SUBROUTINE TBND
! ----------------------------------------------------------------------
      END SUBROUTINE TBND

      SUBROUTINE TDFCND ( DF, SMC, QZ,  SMCMAX, SH2O)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE TDFCND
! ----------------------------------------------------------------------
! CALCULATE THERMAL DIFFUSIVITY AND CONDUCTIVITY OF THE SOIL FOR A GIVEN
! POINT AND TIME.
! ----------------------------------------------------------------------
! PETERS-LIDARD APPROACH (PETERS-LIDARD et al., 1998)
! June 2001 CHANGES: FROZEN SOIL CONDITION.
! ----------------------------------------------------------------------
       REAL DF
       REAL GAMMD
       REAL THKDRY
       REAL AKE
       REAL THKICE
       REAL THKO
       REAL THKQTZ
       REAL THKSAT
       REAL THKS
       REAL THKW
       REAL QZ
       REAL SATRATIO
       REAL SH2O
       REAL SMC
       REAL SMCMAX
       REAL XU
       REAL XUNFROZ

! ----------------------------------------------------------------------
! WE NOW GET QUARTZ AS AN INPUT ARGUMENT (SET IN ROUTINE REDPRM):
!      DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52, 
!     &             0.35, 0.60, 0.40, 0.82/
! ----------------------------------------------------------------------
! IF THE SOIL HAS ANY MOISTURE CONTENT COMPUTE A PARTIAL SUM/PRODUCT
! OTHERWISE USE A CONSTANT VALUE WHICH WORKS WELL WITH MOST SOILS
! ----------------------------------------------------------------------
!  THKW ......WATER THERMAL CONDUCTIVITY
!  THKQTZ ....THERMAL CONDUCTIVITY FOR QUARTZ
!  THKO ......THERMAL CONDUCTIVITY FOR OTHER SOIL COMPONENTS
!  THKS ......THERMAL CONDUCTIVITY FOR THE SOLIDS COMBINED(QUARTZ+OTHER)
!  THKICE ....ICE THERMAL CONDUCTIVITY
!  SMCMAX ....POROSITY (= SMCMAX)
!  QZ .........QUARTZ CONTENT (SOIL TYPE DEPENDENT)
! ----------------------------------------------------------------------
! USE AS IN PETERS-LIDARD, 1998 (MODIF. FROM JOHANSEN, 1975).
!
!                                  PABLO GRUNMANN, 08/17/98
! REFS.:
!      FAROUKI, O.T.,1986: THERMAL PROPERTIES OF SOILS. SERIES ON ROCK 
!              AND SOIL MECHANICS, VOL. 11, TRANS TECH, 136 PP.
!      JOHANSEN, O., 1975: THERMAL CONDUCTIVITY OF SOILS. PH.D. THESIS,
!              UNIVERSITY OF TRONDHEIM,
!      PETERS-LIDARD, C. D., ET AL., 1998: THE EFFECT OF SOIL THERMAL 
!              CONDUCTIVITY PARAMETERIZATION ON SURFACE ENERGY FLUXES
!              AND TEMPERATURES. JOURNAL OF THE ATMOSPHERIC SCIENCES,
!              VOL. 55, PP. 1209-1224.
! ----------------------------------------------------------------------
! NEEDS PARAMETERS
! POROSITY(SOIL TYPE):
!      POROS = SMCMAX
! SATURATION RATIO:
      SATRATIO = SMC/SMCMAX

! PARAMETERS  W/(M.K)
      THKICE = 2.2
      THKW = 0.57
      THKO = 2.0
!      IF (QZ .LE. 0.2) THKO = 3.0
      THKQTZ = 7.7
! SOLIDS' CONDUCTIVITY      
      THKS = (THKQTZ**QZ)*(THKO**(1.- QZ))

! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
      XUNFROZ = (SH2O + 1.E-9) / (SMC + 1.E-9)

! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
      XU=XUNFROZ*SMCMAX 
! SATURATED THERMAL CONDUCTIVITY
      THKSAT = THKS**(1.-SMCMAX)*THKICE**(SMCMAX-XU)*THKW**(XU)

! DRY DENSITY IN KG/M3
      GAMMD = (1. - SMCMAX)*2700.

! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
      THKDRY = (0.135*GAMMD + 64.7)/(2700. - 0.947*GAMMD)

      IF ( (SH2O + 0.0005) .LT. SMC ) THEN
! FROZEN
              AKE = SATRATIO
      ELSE
! UNFROZEN
! RANGE OF VALIDITY FOR THE KERSTEN NUMBER (AKE)
          IF ( SATRATIO .GT. 0.1 ) THEN

! KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT 
! LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)
! (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).

              AKE = LOG10(SATRATIO) + 1.0

          ELSE

! USE K = KDRY
              AKE = 0.0

          ENDIF
      ENDIF

!  THERMAL CONDUCTIVITY

       DF = AKE*(THKSAT - THKDRY) + THKDRY

! ----------------------------------------------------------------------
! END SUBROUTINE TDFCND
! ----------------------------------------------------------------------
      END SUBROUTINE TDFCND

      SUBROUTINE TMPAVG (TAVG,TUP,TM,TDN,ZSOIL,NSOIL,K) 
      
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! SUBROUTINE TMPAVG
! ----------------------------------------------------------------------
! CALCULATE SOIL LAYER AVERAGE TEMPERATURE (TAVG) IN FREEZING/THAWING
! LAYER USING UP, DOWN, AND MIDDLE LAYER TEMPERATURES (TUP, TDN, TM),
! WHERE TUP IS AT TOP BOUNDARY OF LAYER, TDN IS AT BOTTOM BOUNDARY OF
! LAYER.  TM IS LAYER PROGNOSTIC STATE TEMPERATURE.
! ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL

      REAL DZ
      REAL DZH
      REAL T0
      REAL TAVG
      REAL TDN
      REAL TM
      REAL TUP
      REAL X0
      REAL XDN
      REAL XUP
      REAL ZSOIL (NSOIL)

      PARAMETER(T0 = 2.7315E2)

! ----------------------------------------------------------------------
      IF (K .EQ. 1) THEN
        DZ = -ZSOIL(1)
      ELSE
        DZ = ZSOIL(K-1)-ZSOIL(K)
      ENDIF

      DZH=DZ*0.5

      IF (TUP .LT. T0) THEN
        IF (TM .LT. T0) THEN
          IF (TDN .LT. T0) THEN
! ----------------------------------------------------------------------
! TUP, TM, TDN < T0
! ----------------------------------------------------------------------
            TAVG = (TUP + 2.0*TM + TDN)/ 4.0            
          ELSE
! ----------------------------------------------------------------------
! TUP & TM < T0,  TDN >= T0
! ----------------------------------------------------------------------
            X0 = (T0 - TM) * DZH / (TDN - TM)
            TAVG = 0.5 * (TUP*DZH+TM*(DZH+X0)+T0*(2.*DZH-X0)) / DZ
          ENDIF      
        ELSE
          IF (TDN .LT. T0) THEN
! ----------------------------------------------------------------------
! TUP < T0, TM >= T0, TDN < T0
! ----------------------------------------------------------------------
            XUP  = (T0-TUP) * DZH / (TM-TUP)
            XDN  = DZH - (T0-TM) * DZH / (TDN-TM)
            TAVG = 0.5 * (TUP*XUP+T0*(2.*DZ-XUP-XDN)+TDN*XDN) / DZ
          ELSE
! ----------------------------------------------------------------------
! TUP < T0, TM >= T0, TDN >= T0
! ----------------------------------------------------------------------
            XUP  = (T0-TUP) * DZH / (TM-TUP)
            TAVG = 0.5 * (TUP*XUP+T0*(2.*DZ-XUP)) / DZ
          ENDIF   
        ENDIF
      ELSE
        IF (TM .LT. T0) THEN
          IF (TDN .LT. T0) THEN
! ----------------------------------------------------------------------
! TUP >= T0, TM < T0, TDN < T0
! ----------------------------------------------------------------------
            XUP  = DZH - (T0-TUP) * DZH / (TM-TUP)
            TAVG = 0.5 * (T0*(DZ-XUP)+TM*(DZH+XUP)+TDN*DZH) / DZ
          ELSE
! ----------------------------------------------------------------------
! TUP >= T0, TM < T0, TDN >= T0
! ----------------------------------------------------------------------
            XUP  = DZH - (T0-TUP) * DZH / (TM-TUP)
            XDN  = (T0-TM) * DZH / (TDN-TM)
            TAVG = 0.5 * (T0*(2.*DZ-XUP-XDN)+TM*(XUP+XDN)) / DZ
          ENDIF   
        ELSE
          IF (TDN .LT. T0) THEN
! ----------------------------------------------------------------------
! TUP >= T0, TM >= T0, TDN < T0
! ----------------------------------------------------------------------
            XDN  = DZH - (T0-TM) * DZH / (TDN-TM)
            TAVG = (T0*(DZ-XDN)+0.5*(T0+TDN)*XDN) / DZ                 
          ELSE
! ----------------------------------------------------------------------
! TUP >= T0, TM >= T0, TDN >= T0
! ----------------------------------------------------------------------
            TAVG = (TUP + 2.0*TM + TDN) / 4.0
          ENDIF
        ENDIF
      ENDIF
! ----------------------------------------------------------------------
! END SUBROUTINE TMPAVG
! ----------------------------------------------------------------------
      END SUBROUTINE TMPAVG

      SUBROUTINE TRANSP (ET1,NSOIL,ETP1,SMC,CMC,ZSOIL,SHDFAC,SMCWLT,    &
     &                   CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE TRANSP
! ----------------------------------------------------------------------
! CALCULATE TRANSPIRATION FOR THE VEG CLASS.
! ----------------------------------------------------------------------
      INTEGER I
      INTEGER K
      INTEGER NSOIL
      INTEGER NROOT

      REAL CFACTR
      REAL CMC
      REAL CMCMAX
      REAL DENOM
      REAL ET1(NSOIL)
      REAL ETP1
      REAL ETP1A
      REAL GX (7)
!.....REAL PART(NSOIL)
      REAL PC
      REAL Q2
      REAL RTDIS(NSOIL)
      REAL RTX
      REAL SFCTMP
      REAL SGX
      REAL SHDFAC
      REAL SMC(NSOIL)
      REAL SMCREF
      REAL SMCWLT
      REAL ZSOIL(NSOIL)

! ----------------------------------------------------------------------
! INITIALIZE PLANT TRANSP TO ZERO FOR ALL SOIL LAYERS.
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
        ET1(K) = 0.
      END DO

! ----------------------------------------------------------------------
! CALCULATE AN 'ADJUSTED' POTENTIAL TRANSPIRATION
! IF STATEMENT BELOW TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
! NOTE: GX AND OTHER TERMS BELOW REDISTRIBUTE TRANSPIRATION BY LAYER,
! ET(K), AS A FUNCTION OF SOIL MOISTURE AVAILABILITY, WHILE PRESERVING
! TOTAL ETP1A.
! ----------------------------------------------------------------------
      IF (CMC .NE. 0.0) THEN
        ETP1A = SHDFAC * PC * ETP1 * (1.0 - (CMC /CMCMAX) ** CFACTR)
      ELSE
        ETP1A = SHDFAC * PC * ETP1
      ENDIF
      
      SGX = 0.0
      DO I = 1,NROOT
        GX(I) = ( SMC(I) - SMCWLT ) / ( SMCREF - SMCWLT )
        GX(I) = MAX ( MIN ( GX(I), 1. ), 0. )
        SGX = SGX + GX (I)
      END DO
      SGX = SGX / NROOT
      
      DENOM = 0.
      DO I = 1,NROOT
        RTX = RTDIS(I) + GX(I) - SGX
        GX(I) = GX(I) * MAX ( RTX, 0. )
        DENOM = DENOM + GX(I)
      END DO
      IF (DENOM .LE. 0.0) DENOM = 1.

      DO I = 1,NROOT
        ET1(I) = ETP1A * GX(I) / DENOM
      END DO

! ----------------------------------------------------------------------
! ABOVE CODE ASSUMES A VERTICALLY UNIFORM ROOT DISTRIBUTION
! CODE BELOW TESTS A VARIABLE ROOT DISTRIBUTION
! ----------------------------------------------------------------------
!      ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * GX * ETP1A
!      ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * ETP1A
! ----------------------------------------------------------------------
! USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
! ----------------------------------------------------------------------
!      ET(1) = RTDIS(1) * ETP1A
!      ET(1) = ETP1A * PART(1)
! ----------------------------------------------------------------------
! LOOP DOWN THRU THE SOIL LAYERS REPEATING THE OPERATION ABOVE,
! BUT USING THE THICKNESS OF THE SOIL LAYER (RATHER THAN THE
! ABSOLUTE DEPTH OF EACH LAYER) IN THE FINAL CALCULATION.
! ----------------------------------------------------------------------
!      DO K = 2,NROOT
!        GX = ( SMC(K) - SMCWLT ) / ( SMCREF - SMCWLT )
!        GX = MAX ( MIN ( GX, 1. ), 0. )
! TEST CANOPY RESISTANCE
!        GX = 1.0
!        ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*GX*ETP1A
!        ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*ETP1A
! ----------------------------------------------------------------------
! USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
! ----------------------------------------------------------------------
!        ET(K) = RTDIS(K) * ETP1A
!        ET(K) = ETP1A*PART(K)
!      END DO      
! ----------------------------------------------------------------------
! END SUBROUTINE TRANSP
! ----------------------------------------------------------------------
      END SUBROUTINE TRANSP

      SUBROUTINE WDFCND (WDF,WCND,SMC,SMCMAX,BEXP,DKSAT,DWSAT,          &
     &                   SICEMAX)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! SUBROUTINE WDFCND
! ----------------------------------------------------------------------
! CALCULATE SOIL WATER DIFFUSIVITY AND SOIL HYDRAULIC CONDUCTIVITY.
! ----------------------------------------------------------------------
      REAL BEXP
      REAL DKSAT
      REAL DWSAT
      REAL EXPON
      REAL FACTR1
      REAL FACTR2
      REAL SICEMAX
      REAL SMC
      REAL SMCMAX
      REAL VKwgt
      REAL WCND
      REAL WDF

! ----------------------------------------------------------------------
!     CALC THE RATIO OF THE ACTUAL TO THE MAX PSBL SOIL H2O CONTENT
! ----------------------------------------------------------------------
      SMC = SMC
      SMCMAX = SMCMAX
      FACTR1 = 0.2 / SMCMAX
      FACTR2 = SMC / SMCMAX

! ----------------------------------------------------------------------
! PREP AN EXPNTL COEF AND CALC THE SOIL WATER DIFFUSIVITY
! ----------------------------------------------------------------------
      EXPON = BEXP + 2.0
      WDF = DWSAT * FACTR2 ** EXPON

! ----------------------------------------------------------------------
! FROZEN SOIL HYDRAULIC DIFFUSIVITY.  VERY SENSITIVE TO THE VERTICAL
! GRADIENT OF UNFROZEN WATER. THE LATTER GRADIENT CAN BECOME VERY
! EXTREME IN FREEZING/THAWING SITUATIONS, AND GIVEN THE RELATIVELY 
! FEW AND THICK SOIL LAYERS, THIS GRADIENT SUFFERES SERIOUS 
! TRUNCTION ERRORS YIELDING ERRONEOUSLY HIGH VERTICAL TRANSPORTS OF
! UNFROZEN WATER IN BOTH DIRECTIONS FROM HUGE HYDRAULIC DIFFUSIVITY.  
! THEREFORE, WE FOUND WE HAD TO ARBITRARILY CONSTRAIN WDF 
! --
! VERSION D_10CM: ........  FACTR1 = 0.2/SMCMAX
! WEIGHTED APPROACH...................... PABLO GRUNMANN, 28_SEP_1999.
! ----------------------------------------------------------------------
      IF (SICEMAX .GT. 0.0)  THEN
        VKWGT = 1./(1.+(500.*SICEMAX)**3.)
        WDF = VKWGT*WDF + (1.- VKWGT)*DWSAT*FACTR1**EXPON
      ENDIF

! ----------------------------------------------------------------------
! RESET THE EXPNTL COEF AND CALC THE HYDRAULIC CONDUCTIVITY
! ----------------------------------------------------------------------
      EXPON = (2.0 * BEXP) + 3.0
      WCND = DKSAT * FACTR2 ** EXPON

! ----------------------------------------------------------------------
! END SUBROUTINE WDFCND
! ----------------------------------------------------------------------
      END SUBROUTINE WDFCND

  SUBROUTINE nmmlsminit(isn,XICE,VEGFRA,SNOW,SNOWC,CANWAT,SMSTAV,       &
                        SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,       &
                        ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,DZS,SFCEVP,     & !  STEMP
                        TMN,                                            &
                        num_soil_layers,                                &
                        allowed_to_read,                                &
                        ids,ide, jds,jde, kds,kde,                      &
                        ims,ime, jms,jme, kms,kme,                      &
                        its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE 

! Arguments
   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN)       ::     num_soil_layers

   REAL,    DIMENSION( num_soil_layers), INTENT(IN) :: DZS

   REAL,    DIMENSION( ims:ime, num_soil_layers, jms:jme )    , &
            INTENT(INOUT)    ::                          SMOIS, & 
                                                         TSLB      !STEMP

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, & 
                                                         SNOWC, & 
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                        ACSNOW, &
                                                          XICE, &
                                                        VEGFRA, &
                                                        TMN, &
                                                        ACSNOM

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP

!

  INTEGER, INTENT(IN) :: isn
  LOGICAL, INTENT(IN) :: allowed_to_read
! Local
  INTEGER             :: iseason
  INTEGER :: icm,jcm,itf,jtf
  INTEGER ::  I,J,L


   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   icm = ide/2
   jcm = jde/2

   iseason=isn

   DO J=jts,jtf
       DO I=its,itf
!      SNOW(i,j)=0.
       SNOWC(i,j)=0.
!      SMSTAV(i,j)=
!      SMSTOT(i,j)=
!      SFCRUNOFF(i,j)=
!      UDRUNOFF(i,j)=
!      GRDFLX(i,j)=
!      ACSNOW(i,j)=
!      ACSNOM(i,j)=
    ENDDO
   ENDDO

  END SUBROUTINE nmmlsminit

      FUNCTION CSNOW (DSNOW)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! FUNCTION CSNOW
! ----------------------------------------------------------------------
! CALCULATE SNOW TERMAL CONDUCTIVITY
! ----------------------------------------------------------------------
      REAL C
      REAL DSNOW
      REAL CSNOW
      REAL UNIT

      PARAMETER(UNIT = 0.11631) 
                                         
! ----------------------------------------------------------------------
! CSNOW IN UNITS OF CAL/(CM*HR*C), RETURNED IN W/(M*C)
! BASIC VERSION IS DYACHKOVA EQUATION (1960), FOR RANGE 0.1-0.4
! ----------------------------------------------------------------------
      C=0.328*10**(2.25*DSNOW)
!      CSNOW=UNIT*C
! MEK JAN 2006, DOUBLE SNOW THERMAL CONDUCTIVITY
      CSNOW=2.0*UNIT*C

! ----------------------------------------------------------------------
! DE VAUX EQUATION (1933), IN RANGE 0.1-0.6
! ----------------------------------------------------------------------
!      CSNOW=0.0293*(1.+100.*DSNOW**2)
      
! ----------------------------------------------------------------------
! E. ANDERSEN FROM FLERCHINGER
! ----------------------------------------------------------------------
!      CSNOW=0.021+2.51*DSNOW**2        
      
! ----------------------------------------------------------------------
! END FUNCTION CSNOW
! ----------------------------------------------------------------------
      END FUNCTION CSNOW

      FUNCTION FRH2O (TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)

      IMPLICIT NONE

! ----------------------------------------------------------------------
! FUNCTION FRH2O
! ----------------------------------------------------------------------
! CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
! TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION TO
! SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
! (1999, JGR, VOL 104(D16), 19569-19585).
! ----------------------------------------------------------------------
! NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
! ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
! 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
! 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
! REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
! KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
! LIMIT OF FREEZING POINT TEMPERATURE T0.
! ----------------------------------------------------------------------
! INPUT:
!
!   TKELV.........TEMPERATURE (Kelvin)
!   SMC...........TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)
!   SH2O..........LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)
!   SMCMAX........SATURATION SOIL MOISTURE CONTENT (FROM REDPRM)
!   B.............SOIL TYPE "B" PARAMETER (FROM REDPRM)
!   PSIS..........SATURATED SOIL MATRIC POTENTIAL (FROM REDPRM)
!
! OUTPUT:
!   FRH2O.........SUPERCOOLED LIQUID WATER CONTENT
! ----------------------------------------------------------------------
      REAL BEXP
      REAL BLIM
      REAL BX
      REAL CK
      REAL DENOM
      REAL DF
      REAL DH2O
      REAL DICE
      REAL DSWL
      REAL ERROR
      REAL FK
      REAL FRH2O
      REAL GS
      REAL HLICE
      REAL PSIS
      REAL SH2O
      REAL SMC
      REAL SMCMAX
      REAL SWL
      REAL SWLK
      REAL TKELV
      REAL T0

      INTEGER NLOG
      INTEGER KCOUNT

      PARAMETER(CK = 8.0)
!      PARAMETER(CK = 0.0)
      PARAMETER(BLIM = 5.5)
      PARAMETER(ERROR = 0.005)

      PARAMETER(HLICE = 3.335E5)
      PARAMETER(GS = 9.81)
      PARAMETER(DICE = 920.0)
      PARAMETER(DH2O = 1000.0)
      PARAMETER(T0 = 273.15)

! ----------------------------------------------------------------------
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
! ----------------------------------------------------------------------
      BX = BEXP
      IF (BEXP .GT. BLIM) BX = BLIM

! ----------------------------------------------------------------------
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
! ----------------------------------------------------------------------
      NLOG=0
      KCOUNT=0

! ----------------------------------------------------------------------
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC
! ----------------------------------------------------------------------
      IF (TKELV .GT. (T0 - 1.E-3)) THEN
 	FRH2O = SMC
      ELSE
        IF (CK .NE. 0.0) THEN

! ----------------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION FOR NONZERO CK
! IN KOREN ET AL, JGR, 1999, EQN 17
! ----------------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
! ----------------------------------------------------------------------
          SWL = SMC-SH2O

! ----------------------------------------------------------------------
! KEEP WITHIN BOUNDS.
! ----------------------------------------------------------------------
          IF (SWL .GT. (SMC-0.02)) SWL = SMC-0.02
          IF (SWL .LT. 0.) SWL = 0.

! ----------------------------------------------------------------------
!  START OF ITERATIONS
! ----------------------------------------------------------------------
          DO WHILE ( (NLOG .LT. 10) .AND. (KCOUNT .EQ. 0) )
            NLOG = NLOG+1
            DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) *       &
     &        ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
            DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
            SWLK = SWL - DF/DENOM
! ----------------------------------------------------------------------
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
! ----------------------------------------------------------------------
            IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
            IF (SWLK .LT. 0.) SWLK = 0.

! ----------------------------------------------------------------------
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
! ----------------------------------------------------------------------
            DSWL = ABS(SWLK-SWL)
            SWL = SWLK

! ----------------------------------------------------------------------
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! ----------------------------------------------------------------------
            IF ( DSWL .LE. ERROR )  THEN
 	      KCOUNT = KCOUNT+1
            ENDIF
          END DO

! ----------------------------------------------------------------------
!  END OF ITERATIONS
! ----------------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
! ----------------------------------------------------------------------
          FRH2O = SMC - SWL

! ----------------------------------------------------------------------
! END OPTION 1
! ----------------------------------------------------------------------
        ENDIF

! ----------------------------------------------------------------------
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
! ----------------------------------------------------------------------
        IF (KCOUNT .EQ. 0) THEN
          Print*,'Flerchinger used in NEW version. Iterations=',NLOG
 	  FK = (((HLICE/(GS*(-PSIS)))*                                  &
     &      ((TKELV-T0)/TKELV))**(-1/BX))*SMCMAX
 	  IF (FK .LT. 0.02) FK = 0.02
 	  FRH2O = MIN (FK, SMC)
! ----------------------------------------------------------------------
! END OPTION 2
! ----------------------------------------------------------------------
        ENDIF

      ENDIF

! ----------------------------------------------------------------------
! END FUNCTION FRH2O
! ----------------------------------------------------------------------
      END FUNCTION FRH2O

      FUNCTION SNKSRC (TAVG,SMC,SH2O,ZSOIL,NSOIL,                       &
     &                 SMCMAX,PSISAT,BEXP,DT,K,QTOT) 
      
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! FUNCTION SNKSRC
! ----------------------------------------------------------------------
! CALCULATE SINK/SOURCE TERM OF THE TERMAL DIFFUSION EQUATION. (SH2O) IS
! AVAILABLE LIQUED WATER.
! ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL
      
      REAL BEXP
      REAL DF
      REAL DH2O
      REAL DT
      REAL DZ
      REAL DZH
      REAL FREE
!      REAL FRH2O
      REAL HLICE
      REAL PSISAT
      REAL QTOT
      REAL SH2O
      REAL SMC
      REAL SMCMAX
      REAL SNKSRC
      REAL T0
      REAL TAVG
      REAL TDN
      REAL TM
      REAL TUP
      REAL TZ
      REAL X0
      REAL XDN
      REAL XH2O
      REAL XUP
      REAL ZSOIL (NSOIL)

      PARAMETER(DH2O = 1.0000E3)
      PARAMETER(HLICE = 3.3350E5)
      PARAMETER(T0 = 2.7315E2)
      
      IF (K .EQ. 1) THEN
        DZ = -ZSOIL(1)
      ELSE
        DZ = ZSOIL(K-1)-ZSOIL(K)
      ENDIF

! ----------------------------------------------------------------------
! VIA FUNCTION FRH2O, COMPUTE POTENTIAL OR 'EQUILIBRIUM' UNFROZEN
! SUPERCOOLED FREE WATER FOR GIVEN SOIL TYPE AND SOIL LAYER TEMPERATURE.
! FUNCTION FRH20 INVOKES EQN (17) FROM V. KOREN ET AL (1999, JGR, VOL.
! 104, PG 19573).  (ASIDE:  LATTER EQN IN JOURNAL IN CENTIGRADE UNITS.
! ROUTINE FRH2O USE FORM OF EQN IN KELVIN UNITS.)
! ----------------------------------------------------------------------
      FREE = FRH2O(TAVG,SMC,SH2O,SMCMAX,BEXP,PSISAT)

! ----------------------------------------------------------------------
! IN NEXT BLOCK OF CODE, INVOKE EQN 18 OF V. KOREN ET AL (1999, JGR,
! VOL. 104, PG 19573.)  THAT IS, FIRST ESTIMATE THE NEW AMOUNTOF LIQUID
! WATER, 'XH2O', IMPLIED BY THE SUM OF (1) THE LIQUID WATER AT THE BEGIN
! OF CURRENT TIME STEP, AND (2) THE FREEZE OF THAW CHANGE IN LIQUID
! WATER IMPLIED BY THE HEAT FLUX 'QTOT' PASSED IN FROM ROUTINE HRT.
! SECOND, DETERMINE IF XH2O NEEDS TO BE BOUNDED BY 'FREE' (EQUIL AMT) OR
! IF 'FREE' NEEDS TO BE BOUNDED BY XH2O.
! ----------------------------------------------------------------------
      XH2O = SH2O + QTOT*DT/(DH2O*HLICE*DZ)

! ----------------------------------------------------------------------
! FIRST, IF FREEZING AND REMAINING LIQUID LESS THAN LOWER BOUND, THEN
! REDUCE EXTENT OF FREEZING, THEREBY LETTING SOME OR ALL OF HEAT FLUX
! QTOT COOL THE SOIL TEMP LATER IN ROUTINE HRT.
! ----------------------------------------------------------------------
      IF ( XH2O .LT. SH2O .AND. XH2O .LT. FREE) THEN 
        IF ( FREE .GT. SH2O ) THEN
          XH2O = SH2O
        ELSE
          XH2O = FREE
        ENDIF
      ENDIF
              
! ----------------------------------------------------------------------
! SECOND, IF THAWING AND THE INCREASE IN LIQUID WATER GREATER THAN UPPER
! BOUND, THEN REDUCE EXTENT OF THAW, THEREBY LETTING SOME OR ALL OF HEAT
! FLUX QTOT WARM THE SOIL TEMP LATER IN ROUTINE HRT.
! ----------------------------------------------------------------------
      IF ( XH2O .GT. SH2O .AND. XH2O .GT. FREE )  THEN
        IF ( FREE .LT. SH2O ) THEN
          XH2O = SH2O
        ELSE
          XH2O = FREE
        ENDIF
      ENDIF 

      IF (XH2O .LT. 0.) XH2O = 0.
      IF (XH2O .GT. SMC) XH2O = SMC

! ----------------------------------------------------------------------
! CALCULATE PHASE-CHANGE HEAT SOURCE/SINK TERM FOR USE IN ROUTINE HRT
! AND UPDATE LIQUID WATER TO REFLCET FINAL FREEZE/THAW INCREMENT.
! ----------------------------------------------------------------------
      SNKSRC = -DH2O*HLICE*DZ*(XH2O-SH2O)/DT
      SH2O = XH2O
      
! ----------------------------------------------------------------------
! END FUNCTION SNKSRC
! ----------------------------------------------------------------------
      END FUNCTION SNKSRC

END MODULE module_sf_lsm_nmm

