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
          SOLNET=GSW(I,J)
!
!***  Simple use of albedo to determine total incoming solar shortwave SOLDN
!***  (no solar zenith angle correction)
!
          SOLDN=SOLNET/(1.-ALB(I,J))                                  
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
       FFROZP,ICE,DT,Z,NSOIL,SLDPTH,                                    &
       LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,TH2,Q2,SFCSPD,Q2SAT,        &
       DQSDT2,VEGTYP,SOILTYP,SLOPETYP,                                  &
       SHDFAC,PTU,TBOT,ALB,SNOALB,                                      &
       CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,                    &
       ETP,ETA,H,S,RUNOFF1,RUNOFF2,Q1,SNMAX,                            &
       SOILW,SOILM, SMCWLT,SMCDRY,SMCREF,SMCMAX,I,J)
!
      IMPLICIT NONE
!!
! ----------------------------------------------------------------------
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C PURPOSE:  SUB-DRIVER FOR "NOAH/OSU LSM" FAMILY OF PHYSICS SUBROUTINES
!C           FOR A SOIL/VEG/SNOWPACK LAND-SURFACE MODEL TO UPDATE SOIL 
!C           MOISTURE, SOIL ICE, SOIL TEMPERATURE, SKIN TEMPERATURE, 
!C           SNOWPACK WATER CONTENT, SNOWDEPTH, AND ALL TERMS
!C           OF THE SURFACE ENERGY BALANCE AND SURFACE WATER
!C           BALANCE (EXCLUDING INPUT ATMOSPHERIC FORCINGS OF 
!C           DOWNWARD RADIATION AND PRECIP)
!C
!C  VERSION 2.3.1 23 FEBRUARY 2002
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C
! ----------------------------------------------------------------------
! ------------    FROZEN GROUND VERSION     ----------------------------
!     ADDED STATES: SH2O(NSOIL) - UNFROZEN SOIL MOISTURE
!                   SNOWH       - SNOW DEPTH
!
! ----------------------------------------------------------------------
!
! NOTE ON SNOW STATE VARIABLES:
!   SNOWH = actual physical snow depth in m
!   SNEQV = liquid water-equivalent snow depth in m
!            (time-dependent snow density is obtained from SNEQV/SNOWH)
!
! NOTE ON ALBEDO FRACTIONS:
!   Input:
!     ALB    = BASELINE SNOW-FREE ALBEDO, FOR JULIAN DAY OF YEAR 
!   	       (USUALLY FROM TEMPORAL INTERPOLATION OF MONTHLY MEAN VALUES)
!   	       (CALLING PROG MAY OR MAY NOT INCLUDE DIURNAL SUN ANGLE EFFECT)
!     SNOALB = UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW
!   	       (E.G. FROM ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)
!   Output:
!     ALBEDO = COMPUTED ALBEDO WITH SNOWCOVER EFFECTS 
!   	      (COMPUTED USING ALB, SNOALB, SNEQV, AND SHDFAC->green veg frac)
!
!   		 ARGUMENT LIST IN THE CALL TO SFLX
!
! ----------------------------------------------------------------------
! 1. CALLING STATEMENT
!
!     SUBROUTINE SFLX
!    I (ICE,DT,Z,NSOIL,SLDPTH,
!    I LWDN,SOLDN,SFCPRS,PRCP,SFCTMP,TH2,Q2,Q2SAT,DQSDT2,
!    I VEGTYP,SOILTYP,SLOPETYP,
!    I SHDFAC,PTU,TBOT,ALB,SNOALB,
!    I SFCSPD,
!    2 CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,CH,CM,
!    O ETP,ETA,H,S,RUNOFF1,RUNOFF2,Q1,SNMAX,ALBEDO,
!    O SOILW,SOILM,SMCWLT,SMCDRY,SMCREF,SMCMAX)
!
! 2. INPUT (denoted by "I" in column six of argument list at top of routine)
!                  ### GENERAL PARAMETERS ###
!
!          ICE: SEA-ICE FLAG  (=1: SEA-ICE, =0: LAND)
!           DT: TIMESTEP (SEC)
!               (DT SHOULD NOT EXCEED 3600 SECS, RECOMMEND 1800 SECS OR LESS)
!            Z: HEIGHT (M) ABOVE GROUND OF ATMOSPHERIC FORCING VARIABLES
!        NSOIL: NUMBER OF SOIL LAYERS  
!              (at least 2, and not greater than parameter NSOLD set below)
!       SLDPTH: THE THICKNESS OF EACH SOIL LAYER (M) 
!
!                  ### ATMOSPHERIC VARIABLES ###
!
!         LWDN: LW DOWNWARD RADIATION (W M-2; POSITIVE, not net longwave)
!        SOLDN: SOLAR DOWNWARD RADIATION (W M-2; POSITIVE, not net shortwave)
!       SFCPRS: PRESSURE AT HEIGHT Z ABOVE GROUND (PASCALS)
!         PRCP: PRECIP RATE (KG M-2 S-1) (note, this is a rate)
!       SFCTMP: AIR TEMPERATURE (K) AT HEIGHT Z ABOVE GROUND 
!          TH2: AIR POTENTIAL TEMPERATURE (K) AT HEIGHT Z ABOVE GROUND 
!           Q2: MIXING RATIO AT HEIGHT Z ABOVE GROUND (KG KG-1)
!       SFCSPD: WIND SPEED (M S-1) AT HEIGHT Z ABOVE GROUND
!        Q2SAT: SAT MIXING RATIO AT HEIGHT Z ABOVE GROUND (KG KG-1)
!       DQSDT2: SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=SFCTMP (KG KG-1 K-1)
!
!                  ### CANOPY/SOIL CHARACTERISTICS ###
!
!       VEGTYP: VEGETATION TYPE (INTEGER INDEX)
!       SOILTYP: SOIL TYPE (INTEGER INDEX)
!     SLOPETYP: CLASS OF SFC SLOPE (INTEGER INDEX)
!       SHDFAC: AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION (range 0.0-1.0)
!          PTU: PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)
!              (not yet used, but passed to REDPRM for future use in veg parms)
!         TBOT: BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR TEMPERATURE)
!          ALB: BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION)
!       SNOALB: ALBEDO UPPER BOUND OVER DEEP SNOW (FRACTION)
!
! 3. STATE VARIABLES: BOTH INPUT AND OUTPUT
!			 (NOTE: OUTPUT USUALLY MODIFIED FROM INPUT BY PHYSICS)
!
!      (denoted by "2" in column six of argument list at top of routine)
!
!       !!! ########### STATE VARIABLES ##############  !!!
!
!         CMC: CANOPY MOISTURE CONTENT (M)
!          T1: GROUND/CANOPY/SNOWPACK) EFFECTIVE SKIN TEMPERATURE (K)
!
!  STC(NSOIL): SOIL TEMP (K)
!  SMC(NSOIL): TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
! SH2O(NSOIL): UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
!               NOTE: FROZEN SOIL MOISTURE = SMC - SH2O
!
!       SNOWH: SNOW DEPTH (M)
!       SNEQV: WATER-EQUIVALENT SNOW DEPTH (M)
!               NOTE: SNOW DENSITY = SNEQV/SNOWH
!      ALBEDO: SURFACE ALBEDO INCLUDING SNOW EFFECT (UNITLESS FRACTION)
!          CH: SFC EXCH COEF FOR HEAT AND MOISTURE (M S-1)
!          CM: SFC EXCH COEF FOR MOMENTUM (M S-1)
!              NOTE: CH AND CM ARE TECHNICALLY CONDUCTANCES SINCE THEY
!              HAVE BEEN MULTIPLIED BY THE WIND SPEED.
!
! 4. OUTPUT (denoted by "O" in column six of argument list at top of routine)
!
!	NOTE-- SIGN CONVENTION OF SFC ENERGY FLUXES BELOW IS: NEGATIVE IF
!            SINK OF ENERGY TO SURFACE
!
!          ETP: POTENTIAL EVAPORATION (W M-2)
!          ETA: ACTUAL LATENT HEAT FLUX (W M-2: POSITIVE, IF UP FROM SURFACE)
!            H: SENSIBLE HEAT FLUX (W M-2: POSITIVE, IF UPWARD FROM SURFACE)
!            S: SOIL HEAT FLUX (W M-2: POSITIVE, IF DOWNWARD FROM SURFACE)
!      RUNOFF1: SURFACE RUNOFF (M S-1), NOT INFILTRATING THE SURFACE
!      RUNOFF2: SUBSURFACE RUNOFF (M S-1), DRAINAGE OUT BOTTOM OF LAST SOIL LYR
!           Q1: EFFECTIVE MIXING RATIO AT GRND SFC (KG KG-1)
!               (NOTE: Q1 IS NUMERICAL EXPENDIENCY FOR EXPRESSING ETA
!                     EQUIVALENTLY IN A BULK AERODYNAMIC FORM)
!        SNMAX: SNOW MELT (M) (WATER EQUIVALENT)
!        SOILW: AVAILABLE SOIL MOISTURE IN ROOT ZONE (UNITLESS FRACTION BETWEEN
!               SOIL SATURATION AND WILTING POINT)
!        SOILM: TOTAL SOIL COLUMN MOISTURE CONTENT (M) (FROZEN + UNFROZEN)
!
!           FOR DIAGNOSTIC PURPOSES, RETURN SOME PRIMARY PARAMETERS NEXT
!			(SET IN ROUTINE REDPRM)
!
!       SMCWLT: WILTING POINT (VOLUMETRIC)
!       SMCDRY: DRY SOIL MOISTURE THRESHOLD WHERE DIRECT EVAP FRM TOP LYR ENDS
!       SMCREF: SOIL MOISTURE THRESHOLD WHERE TRANSPIRATION BEGINS TO STRESS
!       SMCMAX: POROSITY, I.E. SATURATED VALUE OF SOIL MOISTURE
      INTEGER NSOLD
      PARAMETER (NSOLD = 20)
!tst      PARAMETER (NSOLD = 4)
!
      LOGICAL SNOWNG
      LOGICAL FRZGRA
      LOGICAL SATURATED
!
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER KZ
      INTEGER ICE
      INTEGER NSOIL,VEGTYP,SOILTYP,NROOT
      INTEGER SLOPETYP
!
      REAL ALBEDO
      REAL ALB
      REAL B
      REAL BETA
      REAL CFACTR
!..................CH IS SFC EXCHANGE COEF FOR HEAT/MOIST
!..................CM IS SFC MOMENTUM DRAG (NOT NEEDED IN SFLX)
      REAL CH
      REAL CM
!
      REAL CMC
      REAL CMCMAX
      REAL CP
      REAL CSOIL
      REAL CZIL
      REAL DEW
      REAL DF1
      REAL DF1P
      REAL DKSAT
      REAL DT
      REAL DWSAT
      REAL DQSDT2
      REAL DSOIL
      REAL DTOT
      REAL DRIP
      REAL EC
      REAL EDIR
      REAL ETT
      REAL EXPSNO
      REAL EXPSOI
      REAL EPSCA
      REAL ETA
      REAL ETP
      REAL EDIR1
      REAL EC1
      REAL ETT1
      REAL F
      REAL F1
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FXEXP
      REAL FRZX
      REAL H
      REAL HS
      REAL KDT
      REAL LWDN
      REAL LVH2O
      REAL PC
      REAL PRCP
      REAL PTU
      REAL PRCP1
      REAL PSISAT
      REAL Q1
      REAL Q2
      REAL Q2SAT
      REAL QUARTZ
      REAL R
      REAL RCH
      REAL REFKDT
      REAL RR
      REAL RTDIS (NSOLD)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RGL
      REAL RUNOF
      REAL RUNOFF3
      REAL RSMAX
      REAL RC
      REAL RCMIN
      REAL RSNOW
      REAL SNDENS
      REAL SNCOND 
      REAL S
      REAL SBETA
      REAL SFCPRS
      REAL SFCSPD
      REAL SFCTMP
      REAL SHDFAC
      REAL SH2O(NSOIL)
      REAL SLDPTH(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL SMC(NSOIL)
      REAL SNEQV
      REAL SNOWH
      REAL SNOFAC
      REAL SN_NEW
      REAL SLOPE
      REAL SNUP
      REAL SALP
      REAL SNOALB
      REAL STC(NSOIL)
      REAL SOLDN
      REAL SOLNET
      REAL SNMAX
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
      REAL XLAI
      REAL Z
      REAL ZBOT
      REAL Z0
      REAL ZSOIL(NSOLD)
      REAL FFROZP

      CHARACTER(LEN=4) :: veg_def

!
      PARAMETER ( TFREEZ = 273.15      )
      PARAMETER ( LVH2O  = 2.501000E+6 )
      PARAMETER ( R      = 287.04      )
      PARAMETER ( CP     = 1004.5      )
      

!   INITIALIZATION


!tst
	if (SH2O(1) .lt. 0) then
	write(errmess,*) 'negative SH2O coming INTO SFLX... ', SH2O(1),SMC(1)
        CALL wrf_message( errmess )
	endif
!tst

      RUNOFF1 = 0.0
      RUNOFF2 = 0.0
      RUNOFF3 = 0.0
      SNMAX = 0.0
!
!  THE VARIABLE "ICE" IS A FLAG DENOTING SEA-ICE CASE 

      IF(ICE .EQ. 1) THEN

! SEA-ICE LAYERS ARE EQUAL THICKNESS AND SUM TO 3 METERS
        DO KZ = 1, NSOIL
          ZSOIL(KZ)=-3.*FLOAT(KZ)/FLOAT(NSOIL)
        END DO

      ELSE

! CALCULATE DEPTH (NEGATIVE) BELOW GROUND FROM TOP SKIN SFC TO 
! BOTTOM OF EACH SOIL LAYER.
! NOTE:!!! SIGN OF ZSOIL IS NEGATIVE (DENOTING BELOW GROUND)
        ZSOIL(1)=-SLDPTH(1)
        DO KZ = 2, NSOIL
          ZSOIL(KZ)=-SLDPTH(KZ)+ZSOIL(KZ-1)
        END DO

      ENDIF
         
! ----------------------------------------------------------------------
!C
!C   NEXT IS CRUCIAL CALL TO SET THE LAND-SURFACE PARAMETERS, 
!C   INCLUDING SOIL-TYPE AND VEG-TYPE DEPENDENT PARAMETERS.
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C
        veg_def='9999'
        call nl_get_mminlu(1,veg_def)

        if (veg_def .ne. 'USGS') then
        CALL REDPRM(VEGTYP,SOILTYP, SLOPETYP,                         &
          CFACTR, CMCMAX, RSMAX, TOPT, REFKDT, KDT, SBETA,            &
          SHDFAC, RCMIN, RGL, HS, ZBOT, FRZX, PSISAT, SLOPE,          &
          SNUP, SALP, B, DKSAT, DWSAT, SMCMAX, SMCWLT, SMCREF,        &
          SMCDRY, F1, QUARTZ, FXEXP, RTDIS, SLDPTH, ZSOIL,            &
          NROOT, NSOIL, Z0, CZIL, XLAI, CSOIL, PTU)
	else 
        CALL REDPRM_USGS(VEGTYP,SOILTYP, SLOPETYP,                         &
          CFACTR, CMCMAX, RSMAX, TOPT, REFKDT, KDT, SBETA,            &
          SHDFAC, RCMIN, RGL, HS, ZBOT, FRZX, PSISAT, SLOPE,          &
          SNUP, SALP, B, DKSAT, DWSAT, SMCMAX, SMCWLT, SMCREF,        &
          SMCDRY, F1, QUARTZ, FXEXP, RTDIS, SLDPTH, ZSOIL,            &
          NROOT, NSOIL, Z0, CZIL, XLAI, CSOIL, PTU)
	
	if ( abs(SMCWLT) .lt. 1.5) then
!okay
	else
	write(errmess,*) 'BAD SMCWLT from REDPRM_USGS: ', SMCWLT
        CALL wrf_message(errmess)
	write(errmess,*) 'BAD VEGTYP,SOILTYP:: ', VEGTYP,SOILTYP
        CALL wrf_message(errmess)
	endif
	
        endif

!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C
!C  NEXT CALL ROUTINE SFCDIF TO CALCULATE 
!C    THE SFC EXCHANGE COEF (CH) FOR HEAT AND MOISTURE
!C
!C  NOTE  NOTE  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C    
!C          COMMENT OUT CALL SFCDIF, IF SFCDIF ALREADY CALLED
!C          IN CALLING PROGRAM (SUCH AS IN COUPLED ATMOSPHERIC MODEL)
!C
!C  NOTE !!  DO NOT CALL SFCDIF UNTIL AFTER ABOVE CALL TO REDPRM, 
!C             IN CASE ALTERNATIVE VALUES OF ROUGHNESS LENGTH (Z0) AND 
!C              ZILINTINKEVICH COEF (CZIL) ARE SET THERE VIA NAMELIST I/O
!C
!C   NOTE !! ROUTINE SFCDIF RETURNS A CH THAT REPRESENTS THE WIND SPD
!C          TIMES THE "ORIGINAL" NONDIMENSIONAL "Ch" TYPICAL IN LITERATURE.
!C          HENCE THE CH RETURNED FROM SFCDIF HAS UNITS OF M/S.
!C          THE IMPORTANT COMPANION COEFFICIENT OF CH, CARRIED HERE AS "RCH",
!C          IS THE CH FROM SFCDIF TIMES AIR DENSITY AND PARAMETER "CP".
!C         "RCH" IS COMPUTED IN "CALL PENMAN". RCH RATHER THAN CH IS THE 
!          COEFF USUALLY INVOKED LATER IN EQNS.
!C
!C   NOTE !! SFCDIF ALSO RETURNS THE SURFACE EXCHANGE COEFFICIENT FOR
!            MOMENTUM, CM, ALSO KNOWN AS THE SURFACE DRAGE COEFFICIENT,
!            BUT CM IS NOT USED HERE
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
    
! ----------------------------------------------------------------------
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CALC VIRTUAL TEMPS AND VIRTUAL POTENTIAL TEMPS NEEDED BY 
! SUBROUTINES SFCDIF AND PENMAN
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      T2V  = SFCTMP * (1.0 + 0.61 * Q2 )
! comment out below 2 lines if CALL SFCDIF is commented out, i.e. in
! the coupled model
      T1V  =     T1 * (1.0 + 0.61 * Q2 )
      TH2V =    TH2 * (1.0 + 0.61 * Q2 )

!      CALL SFCDIF ( Z, Z0, T1V, TH2V, SFCSPD, CZIL, CM, CH )

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  INITIALIZE MISC VARIABLES.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SNOWNG = .FALSE.
      FRZGRA = .FALSE.

! IF SEA-ICE CASE,        ASSIGN DEFAULT WATER-EQUIV SNOW ON TOP
      IF(ICE .EQ. 1) THEN
        SNEQV = 0.01
        SNOWH = 0.05
      ENDIF
!
! IF INPUT SNOWPACK IS NONZERO, THEN COMPUTE SNOW DENSITY "SNDENS"
! AND SNOW THERMAL CONDUCTIVITY "SNCOND"
! (NOTE THAT CSNOW IS A FUNCTION SUBROUTINE)
!
      IF(SNEQV .EQ. 0.0) THEN
        SNDENS = 0.0
        SNOWH = 0.0
        SNCOND = 1.0
      ELSE
        SNDENS=SNEQV/SNOWH
        SNCOND = CSNOW (SNDENS) 
      ENDIF

! ----------------------------------------------------------------------
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     DETERMINE IF IT'S PRECIPITATING AND WHAT KIND OF PRECIP IT IS.
!     IF IT'S PRCPING AND THE AIR TEMP IS COLDER THAN 0 C, IT'S SNOWING!
!     IF IT'S PRCPING AND THE AIR TEMP IS WARMER THAN 0 C, BUT THE GRND
!     TEMP IS COLDER THAN 0 C, FREEZING RAIN IS PRESUMED TO BE FALLING.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF ( PRCP .GT. 0.0 ) THEN
!***  Snow defined when fraction of frozen precip (FFROZP) > 0.5, passed
!***  in from model microphysics.
        IF (FFROZP .GT. 0.5) THEN
          SNOWNG = .TRUE.
        ELSE
          IF ( T1 .LE. TFREEZ ) FRZGRA = .TRUE.
        ENDIF
      ENDIF

! ----------------------------------------------------------------------
! If either prcp flag is set, determine new snowfall (converting prcp
! rate from kg m-2 s-1 to a liquid equiv snow depth in meters) and add
! it to the existing snowpack.
! Note that since all precip is added to snowpack, no precip infiltrates
! into the soil so that PRCP1 is set to zero.
      IF ( ( SNOWNG ) .OR. ( FRZGRA ) ) THEN
        SN_NEW = PRCP * DT * 0.001
        SNEQV = SNEQV + SN_NEW
        PRCP1 = 0.0
! ----------------------------------------------------------------------
! Update snow density based on new snowfall, using old and new snow.
      CALL SNOW_NEW (SFCTMP,SN_NEW,SNOWH,SNDENS)
! --- debug ------------------------------------------------------------
!      SNDENS = 0.2
!      SNOWH = SNEQV/SNDENS
! --- debug ------------------------------------------------------------
! ----------------------------------------------------------------------
! Update snow thermal conductivity
      SNCOND = CSNOW (SNDENS) 
! ----------------------------------------------------------------------

      ELSE
!
! PRECIP IS LIQUID (RAIN), HENCE SAVE IN THE PRECIP VARIABLE THAT
! LATER CAN WHOLELY OR PARTIALLY INFILTRATE THE SOIL (ALONG WITH 
! ANY CANOPY "DRIP" ADDED TO THIS LATER)
!
        PRCP1 = PRCP

      ENDIF

! ----------------------------------------------------------------------
! Update albedo, except over sea-ice
      IF (ICE .EQ. 0) THEN

! ----------------------------------------------------------------------
! NEXT IS TIME-DEPENDENT SURFACE ALBEDO MODIFICATION DUE TO 
! TIME-DEPENDENT SNOWDEPTH STATE AND TIME-DEPENDENT CANOPY GREENNESS
      
!      IF ( (SNEQV .EQ. 0.0) .OR. (ALB .GE. SNOALB) ) THEN
        IF (SNEQV .EQ. 0.0) THEN
          ALBEDO = ALB

        ELSE
! ----------------------------------------------------------------------
! SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD (SET IN ROUTINE
! REDPRM)WHERE MAX SNOW ALBEDO EFFECT IS FIRST ATTAINED
          IF (SNEQV .LT. SNUP) THEN
            RSNOW = SNEQV/SNUP
            SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
          ELSE
            SNOFAC = 1.0
          ENDIF
! ----------------------------------------------------------------------
! SNOALB IS ARGUMENT REPRESENTING MAXIMUM ALBEDO OVER DEEP SNOW,
! AS PASSED INTO SFLX, AND ADAPTED FROM THE SATELLITE-BASED MAXIMUM 
! SNOW ALBEDO FIELDS PROVIDED BY D. ROBINSON AND G. KUKLA 
! (1985, JCAM, VOL 24, 402-411)

          ALBEDO = ALB + (1.0-SHDFAC)*SNOFAC*(SNOALB-ALB) 
          IF (ALBEDO .GT. SNOALB) ALBEDO=SNOALB
        ENDIF

      ELSE
! ----------------------------------------------------------------------
! albedo over sea-ice
          ALBEDO = 0.60
          SNOFAC = 1.0
      ENDIF

! ----------------------------------------------------------------------
! Thermal conductivity for sea-ice case
      IF (ICE .EQ. 1) THEN
        DF1=2.2
      ELSE
!
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
!
! ----------------------------------------------------------------------
! FIRST CALCULATE THERMAL DIFFUSIVITY OF TOP SOIL LAYER, USING
! BOTH THE FROZEN AND LIQUID SOIL MOISTURE, FOLLOWING THE 
! SOIL THERMAL DIFFUSIVITY FUNCTION OF PETERS-LIDARD ET AL.
! (1998,JAS, VOL 55, 1209-1224), WHICH REQUIRES THE SPECIFYING
! THE QUARTZ CONTENT OF THE GIVEN SOIL CLASS (SEE ROUTINE REDPRM)
!
        CALL TDFCND ( DF1, SMC(1),QUARTZ,SMCMAX,SH2O(1),I,J)
! ----------------------------------------------------------------------
! NEXT ADD SUBSURFACE HEAT FLUX REDUCTION EFFECT FROM THE 
! OVERLYING GREEN CANOPY, ADAPTED FROM SECTION 2.1.2 OF 
! PETERS-LIDARD ET AL. (1997, JGR, VOL 102(D4))
!
        DF1 = DF1 * EXP(SBETA*SHDFAC)
      ENDIF
! ----------------------------------------------------------------------
! FINALLY "PLANE PARALLEL" SNOWPACK EFFECT FOLLOWING 
! V.J. LINARDINI REFERENCE CITED ABOVE. NOTE THAT DTOT IS
! COMBINED DEPTH OF SNOWDEPTH AND THICKNESS OF FIRST SOIL LAYER
!
      DSOIL = -(0.5 * ZSOIL(1))

      IF (SNEQV .EQ. 0.) THEN
        S = DF1 * (T1 - STC(1) ) / DSOIL
      ELSE
        DTOT = SNOWH + DSOIL
        EXPSNO = SNOWH/DTOT
        EXPSOI = DSOIL/DTOT
! 1. harmonic mean (series flow)
!     DF1 = (SNCOND*DF1)/(EXPSOI*SNCOND+EXPSNO*DF1)
! 2. arithmetic mean (parallel flow)
!     DF1 = EXPSNO*SNCOND + EXPSOI*DF1
      DF1P = EXPSNO*SNCOND + EXPSOI*DF1
! 3. geometric mean (intermediate between 
!                     harmonic and arithmetic mean)
!        DF1 = (SNCOND**EXPSNO)*(DF1**EXPSOI)
! MBEK, 16 Jan 2002
! weigh DF by snow fraction, use parallel flow
        DF1 = DF1P*SNOFAC + DF1*(1.0-SNOFAC)

! ----------------------------------------------------------------------
! CALCULATE SUBSURFACE HEAT FLUX, S, FROM FINAL THERMAL DIFFUSIVITY
! OF SURFACE MEDIUMS, DF1 ABOVE, AND SKIN TEMPERATURE AND TOP 
! MID-LAYER SOIL TEMPERATURE
        S = DF1 * (T1 - STC(1) ) / DTOT
      ENDIF

! ----------------------------------------------------------------------
!  CALCULATE TOTAL DOWNWARD RADIATION (SOLAR PLUS LONGWAVE)
!  NEEDED IN PENMAN EP SUBROUTINE THAT FOLLOWS
          
          F = SOLNET + LWDN

! ----------------------------------------------------------------------
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALL PENMAN SUBROUTINE TO CALCULATE POTENTIAL EVAPORATION (ETP)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       CALL PENMAN ( SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,F,T24,S,Q2,     &
                    Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,DQSDT2,   &
                    FLX2,I,J)
!
! following old constraint is disabled
!.....IF(SATURATED) ETP = 0.0

! ----------------------------------------------------------------------
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALL CANRES TO CALCULATE THE CANOPY RESISTANCE AND CONVERT IT 
!     INTO PC IF MORE THAN TRACE AMOUNT OF CANOPY GREENNESS FRACTION
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!      IF(SHDFAC .GT. 1.E-6) THEN
! make this threshold consistent with the one in SMFLX for TRANSP
! and EC(anopy)
      IF(SHDFAC .GT. 0.) THEN
      
!  FROZEN GROUND EXTENSION: TOTAL SOIL WATER "SMC" WAS REPLACED 
!  BY UNFROZEN SOIL WATER "SH2O" IN CALL TO CANRES BELOW
!      
        CALL CANRES(SOLDN,CH,SFCTMP,Q2,SFCPRS,SH2O,ZSOIL,NSOIL,      &
                  SMCWLT,SMCREF,RCMIN,RC,PC,NROOT,Q2SAT,DQSDT2,      &
                  TOPT,RSMAX,RGL,HS,XLAI)

      ENDIF

! ----------------------------------------------------------------------
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      NOW DECIDE MAJOR PATHWAY BRANCH TO TAKE DEPENDING ON WHETHER
!      SNOWPACK EXISTS OR NOT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        IF ( SNEQV .EQ. 0.0 ) THEN
	if (SH2O(1) .lt. 0) then
	write(errmess,*) 'calling NOPAC with negative SH2O: ', SH2O(1)
        CALL wrf_message(errmess)
	endif

          CALL NOPAC ( ETP, ETA, PRCP, SMC, SMCMAX, SMCWLT,           &
                       SMCREF,SMCDRY, CMC, CMCMAX, NSOIL, DT, SHDFAC, &
                       SBETA,Q1,Q2,T1,SFCTMP,T24,TH2,F,F1,S,STC,      &
                       EPSCA, B, PC, RCH, RR,  CFACTR,                &
                       SH2O, SLOPE, KDT, FRZX, PSISAT, ZSOIL,         &
                       DKSAT, DWSAT, TBOT, ZBOT, RUNOFF1,RUNOFF2,     &
                       RUNOFF3, EDIR1, EC1, ETT1,NROOT,ICE,RTDIS,     &
                       QUARTZ, FXEXP,CSOIL,                           &
                       BETA,DRIP,EC,EDIR,ETT,FLX1,FLX2,FLX3,          &
                       RUNOF,DEW,I,J)

        ELSE

	if (SH2O(1) .lt. 0) then
	write(errmess,*) 'calling SNOPAC with negative SH2O: ', SH2O(1)
        CALL wrf_message(errmess)
	endif

          CALL SNOPAC ( ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT,  &
                      SMCREF, SMCDRY, CMC, CMCMAX, NSOIL, DT,         &
                      SBETA,Q1,DF1,                                   &
                      Q2,T1,SFCTMP,T24,TH2,F,F1,S,STC,EPSCA,SFCPRS,   &
!                      B, PC, RCH, RR, CFACTR, SALP, SNEQV,           &
                      B, PC, RCH, RR, CFACTR, SNOFAC, SNEQV,SNDENS,   &
                      SNOWH, SH2O, SLOPE, KDT, FRZX, PSISAT, SNUP,    &
                      ZSOIL, DWSAT, DKSAT, TBOT, ZBOT, SHDFAC,RUNOFF1,&
                      RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,NROOT,SNMAX,ICE, &
                      RTDIS,QUARTZ, FXEXP,CSOIL,                      &
                      BETA,DRIP,EC,EDIR,ETT,FLX1,FLX2,FLX3,           &
                      RUNOF,DEW,I,J)
        
        ENDIF

! ----------------------------------------------------------------------
!   PREPARE SENSIBLE HEAT (H) FOR RETURN TO PARENT MODEL
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

          H = -(CH * CP * SFCPRS)/(R * T2V) * ( TH2 - T1 )
          
! ----------------------------------------------------------------------
!  CONVERT UNITS AND/OR SIGN OF TOTAL EVAP (ETA), POTENTIAL EVAP (ETP),
!  SUBSURFACE HEAT FLUX (S), AND RUNOFFS FOR WHAT PARENT MODEL EXPECTS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!  CONVERT ETA FROM KG M-2 S-1 TO W M-2
!
      ETA = ETA*LVH2O
      ETP = ETP*LVH2O

! CONVERT THE SIGN OF SOIL HEAT FLUX SO THAT:
!         S>0: WARM THE SURFACE  (NIGHT TIME)
!         S<0: COOL THE SURFACE  (DAY TIME)
!      S=-1.0*S      

!
!  CONVERT RUNOFF3 (INTERNAL LAYER RUNOFF FROM SUPERSAT) FROM M TO M S-1
!  AND ADD TO SUBSURFACE RUNOFF/DRAINAGE/BASEFLOW
!
      RUNOFF3 = RUNOFF3/DT
      RUNOFF2 = RUNOFF2+RUNOFF3
!
! TOTAL COLUMN SOIL MOISTURE IN METERS (SOILM) AND ROOT-ZONE 
! SOIL MOISTURE AVAILABILITY (FRACTION) RELATIVE TO POROSITY/SATURATION

      SOILM=-1.0*SMC(1)*ZSOIL(1)
      
      DO K = 2, NSOIL
        SOILM=SOILM+SMC(K)*(ZSOIL(K-1)-ZSOIL(K))
      END DO
      SOILWM=-1.0*(SMCMAX-SMCWLT)*ZSOIL(1)
      SOILWW=-1.0*(SMC(1)-SMCWLT)*ZSOIL(1)
      DO K = 2, NROOT
        SOILWM=SOILWM+(SMCMAX-SMCWLT)*(ZSOIL(K-1)-ZSOIL(K))
        SOILWW=SOILWW+(SMC(K)-SMCWLT)*(ZSOIL(K-1)-ZSOIL(K))
      END DO
      SOILW=SOILWW/SOILWM

	if (SOILWM .eq. 0) then
	write(0,*) 'div zero in SFLX....SMCMAX, SMCWLT: ', SMCMAX, SMCWLT
	endif
	
	if (abs(SOILW) .lt. 3.5) then
	else
	write(0,*) 'bad SOILW in SFLX...SMCMAX, SMCWLT: ', SMCMAX, SMCWLT
	write(0,*) 'SMC: ', SMC
	endif
!
      END SUBROUTINE SFLX

      SUBROUTINE CANRES(SOLAR,CH,SFCTMP,Q2,SFCPRS,SMC,ZSOIL,NSOIL,    &
                        SMCWLT,SMCREF,RCMIN,RC,PC,NROOT,Q2SAT,DQSDT2, &
                        TOPT,RSMAX,RGL,HS,XLAI)

      IMPLICIT NONE

! ######################################################################
!                        SUBROUTINE CANRES
!                        -----------------
!       THIS ROUTINE CALCULATES THE CANOPY RESISTANCE WHICH DEPENDS ON
!       INCOMING SOLAR RADIATION, AIR TEMPERATURE, ATMOSPHERIC WATER
!       VAPOR PRESSURE DEFICIT AT THE LOWEST MODEL LEVEL, AND SOIL
!       MOISTURE (PREFERABLY UNFROZEN SOIL MOISTURE RATHER THAN TOTAL)
! ----------------------------------------------------------------------
!        SOURCE:  JARVIS (1976), JACQUEMIN AND NOILHAN (1990 BLM)
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!        INPUT:  SOLAR: INCOMING SOLAR RADIATION
!                CH:     SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
!                SFCTMP: AIR TEMPERATURE AT 1ST LEVEL ABOVE GROUND
!                Q2:     AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
!                Q2SAT:  SATURATION AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
!                DQSDT2: SLOPE OF SATURATION HUMIDITY FUNCTION WRT TEMP
!                SFCPRS: SURFACE PRESSURE
!                SMC:    VOLUMETRIC SOIL MOISTURE 
!                ZSOIL:  SOIL DEPTH (NEGATIVE SIGN, AS IT IS BELOW GROUND)
!                NSOIL:  NO. OF SOIL LAYERS
!                NROOT:  NO. OF SOIL LAYERS IN ROOT ZONE (1.LE.NROOT.LE.NSOIL)
!                XLAI:   LEAF AREA INDEX
!                SMCWLT: WILTING POINT
!                SMCREF: REFERENCE SOIL MOISTURE
!                        (WHERE SOIL WATER DEFICIT STRESS SETS IN)
!
! RCMIN, RSMAX, TOPT, RGL, HS: CANOPY STRESS PARAMETERS SET IN SUBR REDPRM
!
!  (SEE EQNS 12-14 AND TABLE 2 OF SEC. 3.1.2 OF 
!       CHEN ET AL., 1996, JGR, VOL 101(D3), 7251-7268)               
!
!        OUTPUT:  PC: PLANT COEFFICIENT
!                 RC: CANOPY RESISTANCE
! ----------------------------------------------------------------------
! ######################################################################

      INTEGER   NSOLD
      PARAMETER (NSOLD = 20)
!tst      PARAMETER (NSOLD = 4)

      INTEGER K
      INTEGER NROOT
      INTEGER NSOIL

      REAL SIGMA, RD, CP, SLV
      REAL SOLAR, CH, SFCTMP, Q2, SFCPRS 
      REAL SMC(NSOIL), ZSOIL(NSOIL), PART(NSOLD) 
      REAL SMCWLT, SMCREF, RCMIN, RC, PC, Q2SAT, DQSDT2
      REAL TOPT, RSMAX, RGL, HS, XLAI, RCS, RCT, RCQ, RCSOIL, FF
      REAL P, QS, GX, TAIR4, ST1, SLVCP, RR, DELTA

      PARAMETER (SIGMA=5.67E-8, RD=287.04, CP=1004.5, SLV=2.501000E6)

      RCS = 0.0
      RCT = 0.0
      RCQ = 0.0
      RCSOIL = 0.0
      RC = 0.0

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO INCOMING SOLAR RADIATION
! ----------------------------------------------------------------------

!C/98/01/05/..disgard old version assuming fixed LAI=1
!C...........FF = 0.55*2.0*SOLAR/RGL

      FF = 0.55*2.0*SOLAR/(RGL*XLAI)
      RCS = (FF + RCMIN/RSMAX) / (1.0 + FF)
      RCS = MAX(RCS,0.0001)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO AIR TEMPERATURE AT FIRST MODEL LEVEL ABOVE GROUND
! ----------------------------------------------------------------------

      RCT = 1.0 - 0.0016*((TOPT-SFCTMP)**2.0)
      RCT = MAX(RCT,0.0001)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT FIRST MODEL LEVEL.
! ----------------------------------------------------------------------

!      P = SFCPRS
      QS = Q2SAT
! RCQ EXPRESSION FROM SSIB 
      RCQ = 1.0/(1.0+HS*(QS-Q2))
      RCQ = MAX(RCQ,0.01)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.
! DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.
! ----------------------------------------------------------------------

      GX = (SMC(1) - SMCWLT) / (SMCREF - SMCWLT)
      IF (GX .GT. 1.) GX = 1.
      IF (GX .LT. 0.) GX = 0.

!####   USING SOIL DEPTH AS WEIGHTING FACTOR
      PART(1) = (ZSOIL(1)/ZSOIL(NROOT)) * GX

!#### USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
!C      PART(1) = RTDIS(1) * GX
      
      DO K = 2, NROOT
        GX = (SMC(K) - SMCWLT) / (SMCREF - SMCWLT)
        IF (GX .GT. 1.) GX = 1.
        IF (GX .LT. 0.) GX = 0.
!####   USING SOIL DEPTH AS WEIGHTING FACTOR        
        PART(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT)) * GX

!#### USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
!C         PART(K) = RTDIS(K) * GX 
               
      END DO

      DO K = 1, NROOT
        RCSOIL = RCSOIL+PART(K)
      END DO

      RCSOIL = MAX(RCSOIL,0.0001)

! ----------------------------------------------------------------------
!         DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.
!         CONVERT CANOPY RESISTANCE (RC) TO PLANT COEFFICIENT (PC).
! ----------------------------------------------------------------------

!C/98/01/05/........RC = RCMIN/(RCS*RCT*RCQ*RCSOIL)
      RC = RCMIN/(XLAI*RCS*RCT*RCQ*RCSOIL)
          
      TAIR4 = SFCTMP**4.
      ST1 = (4.*SIGMA*RD)/CP
      SLVCP = SLV/CP
      RR = ST1*TAIR4/(SFCPRS*CH) + 1.0
      DELTA = SLVCP*DQSDT2
      
      PC = (RR+DELTA)/(RR*(1.+RC*CH)+DELTA)
      
      END SUBROUTINE CANRES

      SUBROUTINE HRT ( RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,   &
                       TBOT, ZBOT, PSISAT, SH2O, DT, B,           &
                       F1, DF1, QUARTZ, CSOIL,AI,BI,CI,II,J)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY
!C    =======   TERM OF THE SOIL THERMAL DIFFUSION EQUATION.  ALSO TO
!C              COMPUTE ( PREPARE ) THE MATRIX COEFFICIENTS FOR THE
!C              TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER NSOLD
      PARAMETER ( NSOLD = 20 )
!tst      PARAMETER ( NSOLD = 4 )

      INTEGER I
      INTEGER II
      INTEGER J
      INTEGER K
      INTEGER NSOIL

! DECLARE WORK ARRAYS NEEDED IN TRI-DIAGONAL IMPLICIT SOLVER

      REAL AI    ( NSOLD )
      REAL BI    ( NSOLD )
      REAL CI    ( NSOLD )

! DECLARE SPECIFIC HEAT CAPACITIES

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
      REAL DTSDZ
      REAL DTSDZ2
      REAL F1
      REAL HCPCT
      REAL QUARTZ
      REAL QTOT
      REAL RHSTS ( NSOIL )
      REAL S
      REAL SMC   ( NSOIL )

      REAL SH2O  ( NSOIL )
      REAL SMCMAX
            
      REAL STC   ( NSOIL )
      REAL TBOT
      REAL ZBOT
      REAL YY
      REAL ZSOIL ( NSOIL )
      REAL ZZ1

      REAL T0, TSURF, PSISAT, DT, B, SICE, TBK, TSNSR, TBK1

!
      PARAMETER ( T0   = 273.15  )

! SET SPECIFIC HEAT CAPACITIES OF AIR, WATER, ICE, SOIL MINERAL       

      PARAMETER ( CAIR =1004.0   )
      PARAMETER ( CH2O = 4.2E6   )
      PARAMETER ( CICE = 2.106E6 )
!.....PARAMETER ( CSOIL=1.26E6   )
!.....
! NOTE: CSOIL NOW SET IN ROUTINE REDPRM AND PASSED IN

!+++++++++++++ BEGIN SECTION FOR TOP SOIL LAYER +++++++++++++++++++++

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC THE HEAT CAPACITY OF THE TOP SOIL LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      HCPCT = SH2O(1)*CH2O + (1.0-SMCMAX)*CSOIL + (SMCMAX-SMC(1))*CAIR &
              + ( SMC(1) - SH2O(1) )*CICE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DDZ = 1.0 / ( -0.5 * ZSOIL(2) )
      AI(1) = 0.0
      CI(1) =  ( DF1 * DDZ ) / ( ZSOIL(1) * HCPCT )
      BI(1) = -CI(1) + DF1 / ( 0.5 * ZSOIL(1) * ZSOIL(1)*HCPCT*ZZ1)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC THE VERTICAL SOIL TEMP GRADIENT BTWN THE 1ST AND 2ND SOIL
!     LAYERS.  THEN CALCULATE THE SUBSURFACE HEAT FLUX. USE THE TEMP
!     GRADIENT AND SUBSFC HEAT FLUX TO CALC "RIGHT-HAND SIDE TENDENCY
!     TERMS", OR "RHSTS", FOR TOP SOIL LAYER.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      DTSDZ = ( STC(1) - STC(2) ) / ( -0.5 * ZSOIL(2) )
      S = DF1 * ( STC(1) - YY ) / ( 0.5 * ZSOIL(1) * ZZ1 )
      RHSTS(1) = ( DF1 * DTSDZ - S ) / ( ZSOIL(1) * HCPCT )

	if (DT*RHSTS(1) .lt. -100.) then
	write(0,*) 'DTSDZ, ZSOIL(1), DF1, S, RHSTS(1): ',  &
             DTSDZ, ZSOIL(1), DF1, S, RHSTS(1)
        endif

! NEXT, SET TEMP "TSURF" AT TOP OF SOIL COLUMN (FOR USE IN FREEZING
! SOIL PHYSICS LATER IN FUNCTION SUBROUTINE SNKSRC). IF SNOWPACK 
! CONTENT IS ZERO, THEN EXPRESSION BELOW GIVES TSURF = SKIN TEMP.
! IF SNOWPACK IS NONZERO (HENCE ARGUMENT ZZ1=1), THEN EXPRESSION
! BELOW YIELDS SOIL COLUMN TOP TEMPERATURE UNDER SNOWPACK.
!
      TSURF = ( YY + ( ZZ1 - 1 ) * STC(1) ) / ZZ1

!
! NEXT CAPTURE THE VERTICAL DIFFERENCE OF THE HEAT FLUX AT TOP 
! AND BOTTOM OF FIRST SOIL LAYER FOR USE IN HEAT FLUX CONSTRAINT 
! APPLIED TO POTENTIAL SOIL FREEZING/THAWING IN ROUTINE SNKSRC
!
      QTOT = S - DF1*DTSDZ

!
! CALCULATE TEMPERATURE AT BOTTOM INTERFACE OF 1ST SOIL LAYER 
! FOR USE LATER IN FCN SUBROUTINE SNKSRC
!
      CALL TBND ( STC(1), STC(2), ZSOIL, ZBOT, 1, NSOIL,TBK)
!
! CALCULATE FROZEN WATER CONTENT IN 1ST SOIL LAYER. 
!
      SICE = SMC(1) - SH2O(1)
!
! IF FROZEN WATER PRESENT OR ANY OF LAYER-1 MID-POINT OR BOUNDING
! INTERFACE TEMPERATURES BELOW FREEZING, THEN CALL SNKSRC TO
! COMPUTE HEAT SOURCE/SINK (AND CHANGE IN FROZEN WATER CONTENT)
! DUE TO POSSIBLE SOIL WATER PHASE CHANGE
!
      IF ( (SICE .GT. 0.) .OR. (TSURF .LT. T0) .OR.            &
           (STC(1) .LT. T0) .OR. (TBK .LT. T0) ) THEN

!	write(0,*) 'call SNKSRC... ', TSURF, STC(1),TBK,SMC(1),SH2O(1)
 
	if (SH2O(1) .lt. 0) then
	write(0,*) 'NEGATIVE SH2O:: ', SH2O(1)
	endif

       TSNSR = SNKSRC ( TSURF, STC(1),TBK, SMC(1), SH2O(1),    &
                 ZSOIL, NSOIL, SMCMAX, PSISAT, B, DT, 1, QTOT )
	
	if (SH2O(1) .lt. 0) then
	write(0,*) 'NEGATIVE SH2O(b):: ', SH2O(1)
	write(0,*) 'return TSNSR: ', TSNSR
	endif

       RHSTS(1) = RHSTS(1) - TSNSR / ( ZSOIL(1) * HCPCT )

	if (DT*RHSTS(1) .lt. -100.) then
	write(0,*) 'TBK, TSNSR,  RHSTS(1): ', TBK,  TSNSR, RHSTS(1)
        endif

      ENDIF
 
! ++++++++++++++ THIS ENDS SECTION FOR TOP SOIL LAYER ++++++++++++++
            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     INITIALIZE DDZ2
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DDZ2 = 0.0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
!(EXCEPT SUBSFC OR "GROUND" HEAT FLUX NOT REPEATED IN LOWER LAYERS)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DF1K = DF1
      DO K = 2, NSOIL

!       CALC THIS SOIL LAYER'S HEAT CAPACITY

        HCPCT = SH2O(K)*CH2O +(1.0-SMCMAX)*CSOIL +(SMCMAX-SMC(K))*CAIR &
              + ( SMC(K) - SH2O(K) )*CICE
!
        IF ( K .NE. NSOIL ) THEN

!+++++++ THIS SECTION FOR LAYER 2 OR GREATER, BUT NOT LAST LAYER +++++

! CALCULATE THERMAL DIFFUSIVITY FOR THIS LAYER

           CALL TDFCND ( DF1N, SMC(K),QUARTZ,SMCMAX,SH2O(K),II,J)

! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER

           DENOM = 0.5 * ( ZSOIL(K-1) - ZSOIL(K+1) )
           DTSDZ2 = ( STC(K) - STC(K+1) ) / DENOM

! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT

           DDZ2 = 2. / (ZSOIL(K-1) - ZSOIL(K+1))
           CI(K) = -DF1N * DDZ2 / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)

! CALCULATE TEMP AT BOTTOM OF LAYER

           CALL TBND ( STC(K),STC(K+1),ZSOIL,ZBOT,K,NSOIL,TBK1 )

        ELSE
!+++++++++++++ SPECIAL CASE OF BOTTOM SOIL LAYER +++++++++++++++++++++

! CALCULATE THERMAL DIFFUSIVITY FOR THIS LAYER

           CALL TDFCND ( DF1N, SMC(K),QUARTZ,SMCMAX,SH2O(K),II,J)

! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER

           DENOM = .5 * (ZSOIL(K-1) + ZSOIL(K)) - ZBOT
           DTSDZ2 = (STC(K)-TBOT) / DENOM

!....SET MATRIX COEF, CI TO ZERO IF BOTTOM LAYER 

           CI(K) = 0.

! CALCULATE TEMP AT BOTTOM OF LAST LAYER

           CALL TBND ( STC(K), TBOT, ZSOIL, ZBOT, K, NSOIL,TBK1 )

        END IF
!+++++++++++++ THIS ENDS SPECIAL CODE FOR BOTTOM LAYER +++++++++

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALC RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        DENOM = ( ZSOIL(K) - ZSOIL(K-1) ) * HCPCT
        RHSTS(K) = ( DF1N * DTSDZ2 - DF1K * DTSDZ ) / DENOM

        QTOT = -1.0*DENOM*RHSTS(K)

        SICE = SMC(K) - SH2O(K)

      IF ( (SICE .GT. 0.) .OR. (TBK .LT. T0) .OR.               &
           (STC(K) .LT. T0) .OR. (TBK1 .LT. T0) ) THEN

       TSNSR = SNKSRC ( TBK, STC(K),TBK1, SMC(K), SH2O(K),      &
                 ZSOIL, NSOIL, SMCMAX, PSISAT, B, DT, K, QTOT)

       RHSTS(K) = RHSTS(K) - TSNSR / DENOM

      ENDIF 
! -------------------------------------------------------------------
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        AI(K) = - DF1 * DDZ / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        BI(K) = -(AI(K) + CI(K))

! RESET VALUES OF DF1, DTSDZ, DDZ, AND TBK FOR LOOP TO NEXT SOIL LYR

        TBK   = TBK1
        DF1K  = DF1N
        DTSDZ = DTSDZ2
        DDZ   = DDZ2
!   
      END DO

      END SUBROUTINE HRT

      SUBROUTINE HRTICE (RHSTS,STC,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY
!C    =======   TERM OF THE SOIL THERMAL DIFFUSION EQUATION IN THE CASE
!C              OF SEA-ICE PACK.  ALSO TO COMPUTE ( PREPARE ) THE
!C              MATRIX COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF 
!C              THE IMPLICIT TIME SCHEME.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER NSOLD
      PARAMETER ( NSOLD = 20 )
!tst      PARAMETER ( NSOLD = 4 )

      INTEGER K
      INTEGER NSOIL

      REAL AI    ( NSOLD )
      REAL BI    ( NSOLD )
      REAL CI    ( NSOLD )

      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DF1
      REAL DTSDZ
      REAL DTSDZ2
      REAL HCPCT
      REAL RHSTS ( NSOIL )
      REAL S
      REAL STC   ( NSOIL )
      REAL TBOT
      REAL YY
      REAL ZBOT
      REAL ZSOIL ( NSOIL )
      REAL ZZ1
!

! THE INPUT ARGUMENT DF1 A UNIVERSALLY CONSTANT VALUE OF
! SEA-ICE THERMAL DIFFUSIVITY, SET IN ROUTINE SNOPAC AS
!  DF1 = 2.2

! SET LOWER BOUNDARY DEPTH AND BOUNDARY TEMPERATURE OF 
! UNFROZEN SEA WATER AT BOTTOM OF SEA ICE PACK.  ASSUME 
! ICE PACK IS OF NSOIL LAYERS SPANNING A UNIFORM CONSTANT
! ICE PACK THICKNESS AS DEFINED IN ROUTINE SFLX

      ZBOT = ZSOIL(NSOIL)
      TBOT = 271.16

! SET A NOMINAL UNIVERSAL VALUE OF THE SEA-ICE SPECIFIC HEAT CAPACITY
      
      HCPCT=1880.0*917.0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DDZ = 1.0 / ( -0.5 * ZSOIL(2) )
      AI(1) = 0.0
      CI(1) =  ( DF1 * DDZ ) / ( ZSOIL(1) * HCPCT )
      BI(1) = -CI(1) + DF1/( 0.5 * ZSOIL(1) * ZSOIL(1) * HCPCT * ZZ1)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC THE VERTICAL SOIL TEMP GRADIENT BTWN THE TOP AND 2ND SOIL
!     LAYERS.  RECALC/ADJUST THE SOIL HEAT FLUX.  USE THE GRADIENT
!     AND FLUX TO CALC RHSTS FOR THE TOP SOIL LAYER.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DTSDZ = ( STC(1) - STC(2) ) / ( -0.5 * ZSOIL(2) )
      S = DF1 * ( STC(1) - YY ) / ( 0.5 * ZSOIL(1) * ZZ1 )
      RHSTS(1) = ( DF1 * DTSDZ - S ) / ( ZSOIL(1) * HCPCT )

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     INITIALIZE DDZ2
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DDZ2 = 0.0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO K = 2, NSOIL

        IF ( K .NE. NSOIL ) THEN

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

          DENOM = 0.5 * ( ZSOIL(K-1) - ZSOIL(K+1) )
          DTSDZ2 = ( STC(K) - STC(K+1) ) / DENOM

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

          DDZ2 = 2. / (ZSOIL(K-1) - ZSOIL(K+1))
          CI(K) = -DF1 * DDZ2 / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)

        ELSE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         CALC THE VERTICAL SOIL TEMP GRADIENT THRU THE LOWEST LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

          DTSDZ2 = (STC(K)-TBOT)/(.5 * (ZSOIL(K-1) + ZSOIL(K))-ZBOT)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         SET MATRIX COEF, CI TO ZERO
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

          CI(K) = 0.
        END IF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALC RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        DENOM = ( ZSOIL(K) - ZSOIL(K-1) ) * HCPCT
        RHSTS(K) = ( DF1 * DTSDZ2 - DF1 * DTSDZ ) / DENOM

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        AI(K) = - DF1 * DDZ / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        BI(K) = -(AI(K) + CI(K))

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       RESET VALUES OF DTSDZ AND DDZ FOR LOOP TO NEXT SOIL LYR
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        DTSDZ = DTSDZ2
        DDZ   = DDZ2

      END DO

      END SUBROUTINE HRTICE

      SUBROUTINE HSTEP ( STCOUT, STCIN, RHSTS, DT, NSOIL,AI,BI,CI,I,J)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER NSOLD
      PARAMETER ( NSOLD = 20 )
!tst      PARAMETER ( NSOLD = 4  )

      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER NSOIL

      REAL AI    ( NSOLD )
      REAL BI    ( NSOLD )
      REAL CI    ( NSOLD )
      REAL CIin  ( NSOLD )
      REAL DT
      REAL RHSTS   ( NSOIL )
      REAL RHSTSin ( NSOIL )
      REAL STCOUT  ( NSOIL )
      REAL STCIN   ( NSOIL )
     
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CREATE FINITE DIFFERENCE VALUES FOR USE IN ROSR12 ROUTINE
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO K = 1 , NSOIL
        RHSTS(K) = RHSTS(K) * DT
        AI(K) = AI(K) * DT
        BI(K) = 1. + BI(K) * DT
        CI(K) = CI(K) * DT
      END DO

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO K = 1 , NSOIL
         RHSTSin(K) = RHSTS(K)
      END DO
      DO K = 1 , NSOLD
         CIin(K) = CI(K)
      END DO
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     SOLVE THE TRI-DIAGONAL MATRIX EQUATION
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL ROSR12 ( CI,AI,BI,CIin,RHSTSin,RHSTS,NSOIL,I,J)

	if (RHSTS(1) .lt. -100) then
	write(0,*) 'RHSTSin was: ', RHSTSin
	write(0,*) 'RHSTS after ROSR12: ', RHSTS
	endif

	
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC/UPDATE THE SOIL TEMPS USING MATRIX SOLUTION
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO K = 1 , NSOIL
         STCOUT(K) = STCIN(K) + CI(K)
!
	if (STCOUT(K) .lt. 100 .and. STCIN(K) .gt. 230) then
	write(0,*) 'STC dropped in HSTEP: ', STCIN(K),STCOUT(K)
	endif

      END DO

      END SUBROUTINE HSTEP

      SUBROUTINE NOPAC ( ETP, ETA, PRCP, SMC, SMCMAX, SMCWLT,         &
                         SMCREF,SMCDRY,CMC,CMCMAX, NSOIL, DT, SHDFAC, &
                         SBETA,                                       &
                         Q1, Q2, T1, SFCTMP, T24, TH2, F, F1, S, STC, &
                         EPSCA, B, PC, RCH, RR,  CFACTR,              &
                         SH2O, SLOPE, KDT, FRZFACT, PSISAT, ZSOIL,    &
                         DKSAT, DWSAT, TBOT, ZBOT, RUNOFF1, RUNOFF2,  &
                         RUNOFF3, EDIR1, EC1, ETT1, NROOT, ICE,RTDIS, &
                         QUARTZ, FXEXP,CSOIL,                         &
                         BETA,DRIP,EC,EDIR,ETT,FLX1,FLX2,FLX3,        &
                         RUNOF,DEW,I,J)


      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDATE
!C    =======   SOIL MOISTURE CONTENT AND SOIL HEAT CONTENT VALUES FOR
!C              THE CASE WHEN NO SNOW PACK IS PRESENT.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER I
      INTEGER J
      INTEGER ICE
      INTEGER NROOT
      INTEGER NSOIL

      REAL B
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
      REAL ETT
      REAL F
      REAL F1
      REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL KDT
      REAL PC
      REAL PRCP
      REAL PRCP1
      REAL Q2
      REAL RCH
      REAL RR
      REAL RTDIS (NSOIL)
      REAL RUNOFF,RUNOF
      REAL S
      REAL SBETA
      REAL SFCTMP
      REAL SHDFAC
      REAL SIGMA
      REAL SMC   ( NSOIL )
      REAL SH2O  ( NSOIL )
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL STC   ( NSOIL )
      REAL T1
      REAL T24
      REAL TBOT
      REAL ZBOT
      REAL TH2
      REAL YY
      REAL YYNUM
      REAL ZSOIL ( NSOIL )
      REAL ZZ1

      REAL Q1, SLOPE, FRZFACT, PSISAT, RUNOFF1, RUNOFF2, RUNOFF3
      REAL EDIR1, EC1, ETT1, QUARTZ

      PARAMETER(CP=1004.5, SIGMA=5.67E-8)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     EXECUTABLE CODE BEGINS HERE.....
!     CONVERT ETP FROM KG M-2 S-1 TO MS-1 AND INITIALIZE DEW.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PRCP1 = PRCP * 0.001
      ETP1 = ETP * 0.001
      DEW = 0.0

      IF ( ETP .GT. 0.0 ) THEN

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CONVERT PRCP FROM  KG M-2 S-1  TO  M S-1
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
           CALL SMFLX ( ETA1,SMC,NSOIL,CMC,ETP1,DT,PRCP1,ZSOIL,     &
                SH2O, SLOPE, KDT, FRZFACT,                          &
                SMCMAX,B,PC,SMCWLT,DKSAT,DWSAT,SMCREF,SHDFAC,       &
                CMCMAX,SMCDRY,CFACTR, RUNOFF1,RUNOFF2, RUNOFF3,     &
                EDIR1,EC1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP,DRIP,    &
                EC,EDIR,ETT,RUNOF,I,J)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        ETA = ETA1 * 1000.0

      ELSE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       IF ETP < 0, ASSUME DEW FORMS (TRANSFORM ETP1 INTO DEW
!       AND REINITIALIZE ETP1 TO ZERO)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        DEW = -ETP1
        ETP1 = 0.0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CONVERT PRCP FROM  KG M-2 S-1  TO  M S-1  AND ADD DEW AMT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        PRCP1 = PRCP1 + DEW
!
      CALL SMFLX ( ETA1,SMC,NSOIL,CMC,ETP1,DT,PRCP1,ZSOIL,          &
                SH2O, SLOPE, KDT, FRZFACT,                          &
                SMCMAX,B,PC,SMCWLT,DKSAT,DWSAT,SMCREF,SHDFAC,       &
                CMCMAX,SMCDRY,CFACTR, RUNOFF1,RUNOFF2, RUNOFF3,     &
                EDIR1,EC1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP,DRIP,    &
                EC,EDIR,ETT,RUNOF,I,J)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        ETA = ETA1 * 1000.0

      ENDIF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     BASED ON ETP AND E VALUES, DETERMINE BETA
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF ( ETP .LE. 0.0 ) THEN
        BETA = 0.0
        IF ( ETP .LT. 0.0 ) THEN
          BETA = 1.0
          ETA = ETP
        ENDIF
      ELSE
        BETA = ETA / ETP
      ENDIF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!    GET SOIL THERMAL DIFFUXIVITY/CONDUCTIVITY FOR TOP SOIL LYR,
!    CALC. ADJUSTED TOP LYR SOIL TEMP AND ADJUSTED SOIL FLUX, THEN
!    CALL SHFLX TO COMPUTE/UPDATE SOIL HEAT FLUX AND SOIL TEMPS.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL TDFCND ( DF1, SMC(1),QUARTZ,SMCMAX,SH2O(1),I,J)

! VEGETATION GREENNESS FRACTION REDUCTION IN SUBSURFACE HEAT FLUX 
! VIA REDUCTION FACTOR, WHICH IS CONVENIENT TO APPLY HERE TO THERMAL 
! DIFFUSIVITY THAT IS LATER USED IN HRT TO COMPUTE SUB SFC HEAT FLUX
! (SEE ADDITIONAL COMMENTS ON VEG EFFECT SUB-SFC HEAT FLX IN 
!  ROUTINE SFLX)

      DF1 = DF1 * EXP(SBETA*SHDFAC)

! COMPUTE INTERMEDIATE TERMS PASSED TO ROUTINE HRT (VIA ROUTINE 
! SHFLX BELOW) FOR USE IN COMPUTING SUBSURFACE HEAT FLUX IN HRT

      YYNUM = F - SIGMA * T24
      YY = SFCTMP + (YYNUM/RCH+TH2-SFCTMP-BETA*EPSCA) / RR
      ZZ1 = DF1 / ( -0.5 * ZSOIL(1) * RCH * RR ) + 1.0

      CALL SHFLX ( S,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,TBOT,  &
                   ZBOT, SMCWLT, PSISAT, SH2O,                      &
                   B,F1,DF1, ICE,                                   &
                   QUARTZ,CSOIL,I,J)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     SET FLX1, AND FLX3 TO ZERO SINCE THEY ARE NOT USED.  FLX2
!     WAS SIMILARLY INITIALIZED IN THE PENMAN ROUTINE.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      FLX1 = 0.0
      FLX3 = 0.0
!
      END SUBROUTINE NOPAC

      SUBROUTINE PENMAN(SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,F,T24,S,Q2,    &
                        Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,DQSDT2, &
                        FLX2,I,J)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE POTENTIAL EVAPORATION FOR THE CURRENT POINT.
!C    =======   VARIOUS PARTIAL SUMS/PRODUCTS ARE ALSO CALCULATED AND
!C              PASSED BACK TO THE CALLING ROUTINE FOR LATER USE.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      LOGICAL SNOWNG
      LOGICAL FRZGRA

      INTEGER I
      INTEGER J

      REAL A
      REAL CH
      REAL CP
      REAL CPH2O
      REAL CPICE
      REAL DELTA
      REAL DEW
      REAL DRIP
      REAL EC
      REAL EDIR
      REAL ELCP
      REAL EPSCA
      REAL ETP
      REAL ETT
      REAL F
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
      REAL S
      REAL SFCPRS
      REAL SFCTMP
      REAL SIGMA
      REAL T24
      REAL T2V
      REAL TH2
      REAL DQSDT2

      PARAMETER(CP=1004.6,CPH2O=4.218E+3,CPICE=2.106E+3,R=287.04,    &
         ELCP=2.4888E+3,LSUBF=3.335E+5,LSUBC=2.501000E+6,SIGMA=5.67E-8)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     EXECUTABLE CODE BEGINS HERE...
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      FLX2 = 0.0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PREPARE PARTIAL QUANTITIES FOR PENMAN EQUATION.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DELTA = ELCP * DQSDT2
      T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP
      RR = T24 * 6.48E-8 / ( SFCPRS * CH ) + 1.0
      RHO = SFCPRS / ( R * T2V )
      RCH = RHO * CP * CH

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ADJUST THE PARTIAL SUMS / PRODUCTS WITH THE LATENT HEAT
!     EFFECTS CAUSED BY FALLING PRECIPITATION.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF ( .NOT. SNOWNG ) THEN
        IF ( PRCP .GT. 0.0 ) RR = RR + CPH2O * PRCP / RCH
      ELSE
        RR = RR + CPICE * PRCP / RCH
      ENDIF

      FNET = F - SIGMA * T24 - S

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     INCLUDE THE LATENT HEAT EFFECTS OF FRZNG RAIN CONVERTING TO
!     ICE ON IMPACT IN THE CALCULATION OF FLX2 AND FNET.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF ( FRZGRA ) THEN
        FLX2 = -LSUBF * PRCP
        FNET = FNET - FLX2
      ENDIF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     FINISH PENMAN EQUATION CALCULATIONS.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      RAD = FNET / RCH + TH2 - SFCTMP
      A = ELCP * ( Q2SAT - Q2 )
      EPSCA = ( A * RR + RAD * DELTA ) / ( DELTA + RR )
      ETP = EPSCA * RCH / LSUBC

      END SUBROUTINE PENMAN

      SUBROUTINE REDPRM(VEGTYP, SOILTYP, SLOPETYP,              &
           CFACTR, CMCMAX, RSMAX, TOPT, REFKDT, KDT, SBETA,     &
           SHDFAC, RCMIN, RGL, HS, ZBOT, FRZX, PSISAT, SLOPE,   &
           SNUP, SALP, B, DKSAT, DWSAT, SMCMAX, SMCWLT, SMCREF, &
           SMCDRY, F1, QUARTZ, FXEXP, RTDIS, SLDPTH, ZSOIL,     &
           NROOT, NSOIL, Z0, CZIL, LAI, CSOIL, PTU)


      IMPLICIT NONE

!  This subroutine internally sets (defaults), or optionally reads-in
!  via namelist I/O, all the soil and vegetation parameters
!  required for the execusion of the NOAH - LSM
! 
! optional non-default parameters can be read in, accommodating up
!  to 30 soil, veg, or slope classes, if the default max number of 
!  soil, veg, and/or slope types is reset.

! future upgrades of routine REDPRM must expand to incorporate some
! of the empirical parameters of the frozen soil and snowpack physics
! (such as in routines FRH2O, SNOWPACK, and SNOW_NEW) not yet set in 
!  this REDPRM routine, but rather set in lower level subroutines

!  Set maximum number of soil-, veg-, and slopetyp in data statement

      INTEGER MAX_SOILTYP
      INTEGER MAX_VEGTYP
      INTEGER MAX_SLOPETYP
      PARAMETER (MAX_SOILTYP  = 30)
      PARAMETER (MAX_VEGTYP   = 30)
      PARAMETER (MAX_SLOPETYP = 30)

!  Number of defined soil-, veg-, and slopetyps used

      INTEGER DEFINED_VEG
      INTEGER DEFINED_SOIL
      INTEGER DEFINED_SLOPE
      DATA DEFINED_VEG/13/
      DATA DEFINED_SOIL/9/
      DATA DEFINED_SLOPE/9/

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
!
! SOIL TYPES   ZOBLER (1986)      COSBY ET AL (1984) (quartz cont.(1))
!  1        COARSE            LOAMY SAND         (0.82)
!  2        MEDIUM            SILTY CLAY LOAM    (0.10)
!  3        FINE              LIGHT CLAY         (0.25)
!  4        COARSE-MEDIUM     SANDY LOAM         (0.60)
!  5        COARSE-FINE       SANDY CLAY         (0.52)
!  6        MEDIUM-FINE       CLAY LOAM          (0.35)
!  7        COARSE-MED-FINE   SANDY CLAY LOAM    (0.60)
!  8        ORGANIC           LOAM               (0.40)
!  9        GLACIAL LAND ICE  LOAMY SAND         (NA using 0.82)

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

      REAL B
      REAL DKSAT
      REAL DWSAT
      REAL SMCMAX
      REAL SMCWLT
      REAL SMCREF
      REAL SMCDRY
      REAL PTU
      REAL F1
      REAL QUARTZ
      REAL REFSMC1
      REAL WLTSMC1

      DATA MAXSMC/0.421, 0.464, 0.468, 0.434, 0.406, 0.465,  &
                  0.404, 0.439, 0.421, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA SATPSI/0.04, 0.62, 0.47, 0.14, 0.10, 0.26,        &
                  0.14, 0.36, 0.04, 0.00, 0.00, 0.00,        &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,        &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,        &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/
      DATA SATDK /1.41E-5, 0.20E-5, 0.10E-5, 0.52E-5, 0.72E-5, &
                  0.25E-5, 0.45E-5, 0.34E-5, 1.41E-5, 0.00,    &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,    &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,    &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,    &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00/
      DATA BB    /4.26,  8.72, 11.55, 4.74, 10.73,  8.17,    & 
                  6.77,  5.25,  4.26, 0.00,  0.00,  0.00,    &
                  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,    &
                  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,    &
                  0.00,  0.00,  0.00, 0.00,  0.00,  0.00/
      DATA QTZ   /0.82, 0.10, 0.25, 0.60, 0.52, 0.35,        &
                  0.60, 0.40, 0.82, 0.00, 0.00, 0.00,        &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,        &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,        &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/

! The following 5 parameters are derived later in REDPRM.f 
! from the soil data, and are just given here for reference 
! and to force static storage allocation
! Dag Lohmann, Feb. 2001

      DATA REFSMC/0.283, 0.387, 0.412, 0.312, 0.338, 0.382,     &
                  0.315, 0.329, 0.283, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/   
      DATA WLTSMC/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,     &
                  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA DRYSMC/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,     &
                  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA SATDW /5.71E-6, 2.33E-5, 1.16E-5, 7.95E-6, 1.90E-5,  &
                  1.14E-5, 1.06E-5, 1.46E-5, 5.71E-6, 0.00,     &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,     &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,     &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,     &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00/
      DATA F11  /-0.999, -1.116, -2.137, -0.572, -3.201, -1.302, &
                 -1.519, -0.329, -0.999,  0.000,  0.000,  0.000, &
                  0.000,  0.000,  0.000,  0.000,  0.000,  0.000, &
                  0.000,  0.000,  0.000,  0.000,  0.000,  0.000, &
                  0.000,  0.000,  0.000,  0.000,  0.000,  0.000/

!#######################################################################

!  SET-UP VEGETATION PARAMETERS FOR A GIVEN VEGETAION TYPE
!
!  INPUT: VEGTYP = VEGETATION TYPE (INTEGER INDEX)
!  OUPUT: VEGETATION PARAMETERS
!         SHDFAC: VEGETATION GREENNESS FRACTION
!         RCMIN:  MIMIMUM STOMATAL RESISTANCE
!         RGL:    PARAMETER USED IN SOLAR RAD TERM OF
!                 CANOPY RESISTANCE FUNCTION
!         HS:     PARAMETER USED IN VAPOR PRESSURE DEFICIT TERM OF
!                 CANOPY RESISTANCE FUNCTION
!         SNUP:   THRESHOLD SNOW DEPTH (IN WATER EQUIVALENT M) THAT
!                 IMPLIES 100% SNOW COVER
!
!  SSIB VEGETATION TYPES (DORMAN AND SELLERS, 1989; JAM)
!
!   1:   BROADLEAF-EVERGREEN TREES  (TROPICAL FOREST)
!   2:   BROADLEAF-DECIDUOUS TREES
!   3:   BROADLEAF AND NEEDLELEAF TREES (MIXED FOREST)
!   4:   NEEDLELEAF-EVERGREEN TREES
!   5:   NEEDLELEAF-DECIDUOUS TREES (LARCH)
!   6:   BROADLEAF TREES WITH GROUNDCOVER (SAVANNA)
!   7:   GROUNDCOVER ONLY (PERENNIAL)
!   8:   BROADLEAF SHRUBS WITH PERENNIAL GROUNDCOVER
!   9:   BROADLEAF SHRUBS WITH BARE SOIL
!  10:   DWARF TREES AND SHRUBS WITH GROUNDCOVER (TUNDRA)
!  11:   BARE SOIL
!  12:   CULTIVATIONS (THE SAME PARAMETERS AS FOR TYPE 7)
!  13:   GLACIAL (THE SAME PARAMETERS AS FOR TYPE 11)

      INTEGER NROOT_DATA(MAX_VEGTYP)
      REAL    RSMTBL(MAX_VEGTYP)
      REAL    RGLTBL(MAX_VEGTYP)
      REAL    HSTBL(MAX_VEGTYP)
      REAL    SNUPX(MAX_VEGTYP)
      REAL    Z0_DATA(MAX_VEGTYP)
      REAL    LAI_DATA(MAX_VEGTYP)

      INTEGER NROOT
      REAL    SHDFAC
      REAL    RCMIN
      REAL    RGL
      REAL    HS
      REAL    FRZFACT
      REAL    PSISAT
      REAL    SNUP
      REAL    Z0
      REAL    LAI

      DATA NROOT_DATA /4,4,4,4,4,4,3,3,3,2,3,3,2,0,0,       &
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      DATA RSMTBL /150.0, 100.0, 125.0, 150.0, 100.0, 70.0, &
                    40.0, 300.0, 400.0, 150.0, 400.0, 40.0, &
                   150.0,   0.0,   0.0,   0.0,   0.0,  0.0, &
                     0.0,   0.0,   0.0,   0.0,   0.0,  0.0, &
                     0.0,   0.0,   0.0,   0.0,   0.0,  0.0/
      DATA RGLTBL /30.0,  30.0,  30.0,  30.0,  30.0,  65.0, &
                  100.0, 100.0, 100.0, 100.0, 100.0, 100.0, &
                  100.0,   0.0,   0.0,   0.0,   0.0,   0.0, &
                    0.0,   0.0,   0.0,   0.0,   0.0,   0.0, &
                    0.0,   0.0,   0.0,   0.0,   0.0,   0.0/
      DATA HSTBL /41.69, 54.53, 51.93, 47.35,  47.35, 54.53, &
                  36.35, 42.00, 42.00, 42.00,  42.00, 36.35, &
                  42.00,  0.00,  0.00,  0.00,   0.00,  0.00, &
                   0.00,  0.00,  0.00,  0.00,   0.00,  0.00, &
                   0.00,  0.00,  0.00,  0.00,   0.00,  0.00/
      DATA SNUPX  /0.080, 0.080, 0.080, 0.080, 0.080, 0.080, &
                   0.040, 0.040, 0.040, 0.040, 0.025, 0.040, &
                   0.025, 0.000, 0.000, 0.000, 0.000, 0.000, &
                   0.000, 0.000, 0.000, 0.000, 0.000, 0.000, &
                   0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA Z0_DATA /2.653, 0.826, 0.563, 1.089, 0.854, 0.856, &
                    0.035, 0.238, 0.065, 0.076, 0.011, 0.035, &
                    0.011, 0.000, 0.000, 0.000, 0.000, 0.000, &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000, &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
!      DATA LAI_DATA /3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
!     *               3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
!     *               3.0, 0.0, 0.0, 0.0, 0.0, 0.0,
!     *               0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
!     *               0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
      DATA LAI_DATA /4.0, 4.0, 4.0, 4.0, 4.0, 4.0,  &
                     4.0, 4.0, 4.0, 4.0, 4.0, 4.0,  &
                     4.0, 0.0, 0.0, 0.0, 0.0, 0.0,  &
                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  &
                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0/

!#######################################################################

!  CLASS PARAMETER 'SLOPETYP' WAS INCLUDED TO ESTIMATE
!  LINEAR RESERVOIR COEFFICIENT 'SLOPE' TO THE BASEFLOW RUNOFF
!  OUT OF THE BOTTOM LAYER. LOWEST CLASS (SLOPETYP=0)MEANS
!  HIGHEST SLOPE PARAMETER= 1
!  DEFINITION OF SLOPETYP FROM 'ZOBLER' SLOPE TYPE
!  SLOPE CLASS      PERCENT SLOPE
!  1                0-8
!  2                8-30
!  3                > 30
!  4                0-30
!  5                0-8 & > 30
!  6                8-30 & > 30
!  7                0-8, 8-30, > 30
!  9                GLACIAL ICE
!  BLANK            OCEAN/SEA
!  NOTE:  CLASS 9 FROM 'ZOBLER' FILE SHOULD BE REPLACED BY 8
!  AND 'BLANK'  9

      REAL SLOPE
      REAL SLOPE_DATA(MAX_SLOPETYP)
      DATA SLOPE_DATA /0.1,  0.6, 1.0, 0.35, 0.55, 0.8,  &
                       0.63, 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0/

!#######################################################################

!  Set namelist file name

      CHARACTER*50 NAMELIST_NAME

!#######################################################################

! SET UNIVERSAL PARAMETERS (NOT DEPENDENT ON SOIL, VEG, SLOPE TYPE)

      INTEGER VEGTYP
      INTEGER SOILTYP
      INTEGER SLOPETYP

      INTEGER NSOIL
      INTEGER I

      INTEGER BARE
      DATA    BARE /11/

      LOGICAL LPARAM
      DATA    LPARAM /.TRUE./

      LOGICAL LFIRST
      DATA    LFIRST /.TRUE./

!  Parameter used to calculate roughness length of heat
      REAL CZIL, CZIL_DATA
      DATA CZIL_DATA /0.2/

!  Parameter used to caluculate vegetation effect on soil heat flux
      REAL SBETA, SBETA_DATA
      DATA SBETA_DATA /-2.0/

! BARE SOIL EVAPORATION EXPONENT USED IN DEVAP

      REAL FXEXP, FXEXP_DATA
      DATA FXEXP_DATA /2.0/

! Soil heat capacity [J/m^3/K]

      REAL CSOIL, CSOIL_DATA
      DATA CSOIL_DATA /2.00E+6/

!  SPECIFY SNOW DISTRIBUTION SHAPE PARAMETER
!  SALP   - SHAPE PARAMETER OF DISTRIBUTION FUNCTION
!  OF SNOW COVER. FROM ANDERSON'S DATA (HYDRO-17)
!  BEST FIT IS WHEN SALP = 2.6
      REAL SALP, SALP_DATA
      DATA SALP_DATA /2.6/

!  KDT IS DEFINED BY REFERENCE REFKDT AND DKSAT
!  REFDK=2.E-6 IS THE SAT. DK. VALUE FOR THE SOIL TYPE 2
      REAL REFDK, REFDK_DATA
      DATA REFDK_DATA /2.0E-6/

      REAL REFKDT, REFKDT_DATA
      DATA REFKDT_DATA /3.0/

      REAL KDT
      REAL FRZX

!  FROZEN GROUND PARAMETER, FRZK, DEFINITION
!  FRZK IS ICE CONTENT THRESHOLD ABOVE WHICH FROZEN SOIL IS IMPERMEABLE
!  REFERENCE VALUE OF THIS PARAMETER FOR THE LIGHT CLAY SOIL (TYPE=3)
!  FRZK = 0.15 M
      REAL FRZK, FRZK_DATA
      DATA FRZK_DATA /0.15/

      REAL RTDIS(NSOIL)
      REAL SLDPTH(NSOIL)
      REAL ZSOIL(NSOIL)

!  Set two canopy water parameters
      REAL CFACTR, CFACTR_DATA
      REAL CMCMAX, CMCMAX_DATA
      DATA CFACTR_DATA /0.5/
      DATA CMCMAX_DATA /0.5E-3/

!  Set max. stomatal resistance
      REAL RSMAX, RSMAX_DATA
      DATA RSMAX_DATA /5000.0/

!  Set optimum transpiration air temperature
      REAL TOPT, TOPT_DATA
      DATA TOPT_DATA /298.0/

!  Specify depth[m] of lower boundary soil temperature
      REAL ZBOT, ZBOT_DATA
      DATA ZBOT_DATA /-3.0/

!#######################################################################

!  Namelist definition

      NAMELIST /SOIL_VEG/ SLOPE_DATA, RSMTBL, RGLTBL, HSTBL, SNUPX,     &
           BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW,       &
           WLTSMC, QTZ, LPARAM, ZBOT_DATA, SALP_DATA, CFACTR_DATA,      &
           CMCMAX_DATA, SBETA_DATA, RSMAX_DATA, TOPT_DATA,              &
           REFDK_DATA, FRZK_DATA, BARE, DEFINED_VEG, DEFINED_SOIL,      &
           DEFINED_SLOPE, FXEXP_DATA, NROOT_DATA, REFKDT_DATA, Z0_DATA, &
           CZIL_DATA, LAI_DATA, CSOIL_DATA

!  Read namelist file to override default parameters
!  only once.
!
!  7/6/01 : E. Rogers commented out read of unit 58 since
!           NCO does not allow hardwired file names in the code.

!     IF (LFIRST) THEN
!        OPEN(58, FILE = 'namelist_filename.txt')
! NAMELIST_NAME must be 50 characters or less.
!        READ(58,'(A)') NAMELIST_NAME
!        CLOSE(58)
!        WRITE(*,*) 'Namelist Filename is ', NAMELIST_NAME
!        OPEN(59, FILE = NAMELIST_NAME)
 50      CONTINUE
!           READ(59, SOIL_VEG, END=100)
!        IF (LPARAM) GOTO 50
 100     CONTINUE
!        CLOSE(59)
!        WRITE(*,NML=SOIL_VEG)
!        LFIRST = .FALSE.
         IF (DEFINED_SOIL .GT. MAX_SOILTYP) THEN
            CALL wrf_error_fatal('Warning: DEFINED_SOIL too large in namelist') 
         END IF
         IF (DEFINED_VEG .GT. MAX_VEGTYP) THEN
            CALL wrf_error_fatal('Warning: DEFINED_VEG too large in namelist')
         END IF
         IF (DEFINED_SLOPE .GT. MAX_SLOPETYP) THEN
            CALL wrf_error_fatal('Warning: DEFINED_SLOPE too large in namelist' )
         END IF

         DO I = 1, DEFINED_SOIL
            SATDW(I)  = BB(I)*SATDK(I)*(SATPSI(I)/MAXSMC(I))
            F11(I)    = ALOG10(SATPSI(I)) + BB(I)*ALOG10(MAXSMC(I)) + 2.0
            REFSMC1   = MAXSMC(I)*(5.79E-9/SATDK(I))  &
                                          **(1.0/(2.0*BB(I)+3.0))
            REFSMC(I) = REFSMC1 + (MAXSMC(I)-REFSMC1) / 3.0
            WLTSMC1   = MAXSMC(I) * (200.0/SATPSI(I))**(-1.0/BB(I))
            WLTSMC(I) = WLTSMC1 - 0.5 * WLTSMC1
! Current version DRYSMC values that equate to WLTSMC
! Future version could let DRYSMC be independently set via namelist 
            DRYSMC(I) = WLTSMC(I)
         END DO

!     END IF

      IF (SOILTYP .GT. DEFINED_SOIL) THEN
         CALL wrf_error_fatal(  'Warning: too many soil types' )
      END IF
      IF (VEGTYP .GT. DEFINED_VEG) THEN
         CALL wrf_error_fatal(  'Warning: too many veg types' )
      END IF
      IF (SLOPETYP .GT. DEFINED_SLOPE) THEN
         CALL wrf_error_fatal(  'Warning: too many slope types' )
      END IF

!  SET-UP UNIVERSAL PARAMETERS 
! (NOT DEPENDENT ON SOILTYP, VEGTYP OR SLOPETYP)
      ZBOT   = ZBOT_DATA
      SALP   = SALP_DATA
      CFACTR = CFACTR_DATA
      CMCMAX = CMCMAX_DATA
      SBETA  = SBETA_DATA
      RSMAX  = RSMAX_DATA
      TOPT   = TOPT_DATA
      REFDK  = REFDK_DATA
      FRZK   = FRZK_DATA
      FXEXP  = FXEXP_DATA
      REFKDT = REFKDT_DATA
      CZIL   = CZIL_DATA
      CSOIL  = CSOIL_DATA

!  SET-UP SOIL PARAMETERS
      B       = BB(SOILTYP)
      SMCDRY  = DRYSMC(SOILTYP)
      F1      = F11(SOILTYP)
      SMCMAX  = MAXSMC(SOILTYP)
      SMCREF  = REFSMC(SOILTYP)
      PSISAT  = SATPSI(SOILTYP)
      DKSAT   = SATDK(SOILTYP)
      DWSAT   = SATDW(SOILTYP)
      SMCWLT  = WLTSMC(SOILTYP)
      QUARTZ  = QTZ(SOILTYP)
      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)
      KDT     = REFKDT * DKSAT/REFDK

!  TO ADJUST FRZK PARAMETER TO ACTUAL SOIL TYPE: FRZK * FRZFACT

      FRZX = FRZK * FRZFACT

!  SET-UP VEGETATION PARAMETERS
      NROOT = NROOT_DATA(VEGTYP)
      SNUP  = SNUPX(VEGTYP)
      RCMIN = RSMTBL(VEGTYP)
      RGL   = RGLTBL(VEGTYP)
      HS    = HSTBL(VEGTYP)
      Z0    = Z0_DATA(VEGTYP)
      LAI   = LAI_DATA(VEGTYP)
      IF(VEGTYP .EQ. BARE) SHDFAC = 0.0

      IF (NROOT .GT. NSOIL) THEN
         CALL wrf_error_fatal ( 'Warning: too many root layers' )
      END IF

!  CALCULATE ROOT DISTRIBUTION
!  PRESENT VERSION ASSUMES UNIFORM DISTRIBUTION BASED ON SOIL LAYERS

      DO I=1,NROOT
         RTDIS(I) = -SLDPTH(I)/ZSOIL(NROOT)
      END DO

!  SET-UP SLOPE PARAMETER
      SLOPE = SLOPE_DATA(SLOPETYP)
!
      END SUBROUTINE REDPRM

! --------------------------------------------------------------

      SUBROUTINE REDPRM_USGS(VEGTYP, SOILTYP, SLOPETYP,              &
           CFACTR, CMCMAX, RSMAX, TOPT, REFKDT, KDT, SBETA,     &
           SHDFAC, RCMIN, RGL, HS, ZBOT, FRZX, PSISAT, SLOPE,   &
           SNUP, SALP, B, DKSAT, DWSAT, SMCMAX, SMCWLT, SMCREF, &
           SMCDRY, F1, QUARTZ, FXEXP, RTDIS, SLDPTH, ZSOIL,     &
           NROOT, NSOIL, Z0, CZIL, LAI, CSOIL, PTU)


      IMPLICIT NONE


!  This subroutine internally sets (defaults), or optionally reads-in
!  via namelist I/O, all the soil and vegetation parameters
!  required for the execusion of the NOAH - LSM
!
! optional non-default parameters can be read in, accommodating up
!  to 30 soil, veg, or slope classes, if the default max number of
!  soil, veg, and/or slope types is reset.

! future upgrades of routine REDPRM must expand to incorporate some
! of the empirical parameters of the frozen soil and snowpack physics
! (such as in routines FRH2O, SNOWPACK, and SNOW_NEW) not yet set in
!  this REDPRM routine, but rather set in lower level subroutines

!  Set maximum number of soil-, veg-, and slopetyp in data statement

      INTEGER MAX_SOILTYP
      INTEGER MAX_VEGTYP
      INTEGER MAX_SLOPETYP
      PARAMETER (MAX_SOILTYP  = 30)
      PARAMETER (MAX_VEGTYP   = 30)
      PARAMETER (MAX_SLOPETYP = 30)

!  Number of defined soil-, veg-, and slopetyps used

      INTEGER DEFINED_VEG
      INTEGER DEFINED_SOIL
      INTEGER DEFINED_SLOPE
      DATA DEFINED_VEG/24/
      DATA DEFINED_SOIL/16/
      DATA DEFINED_SLOPE/9/

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
!
!C     SOIL TYPES    STATSGO (Miller ??, 199?)  Cosby et al (1984)
!C             1          SAND                  SAND
!C             2          LOAMY SAND            LOAMY SAND
!C             3          SANDY LOAM            SANDY LOAM
!C             4          SILT LOAM             SILTY LOAM
!C             5          SILT                  SILTY LOAM
!C             6          LOAM                  LOAM
!C             7          SANDY CLAY LOAM       SANDY CLAY LOAM
!C             8          SILTY CLAY LOAM       SILTY CLAY LOAM
!C             9          CLAY LOAM             CLAY LOAM
!C            10          SANDY CLAY            SANDY CLAY
!C            11          SILTY CLAY            SILTY CLAY
!C            12          CLAY                  LIGHT CLAY
!C            13          ORGANIC MATERIALS     LOAM
!C            14          WATER
!C            15          BEDROCK
!C                        Bedrock is reclassified as class 14
!C            16          OTHER (land-ice)
!C                        the value of this class is the same as in class


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

      REAL B
      REAL DKSAT
      REAL DWSAT
      REAL SMCMAX
      REAL SMCWLT
      REAL SMCREF
      REAL SMCDRY
      REAL PTU
      REAL F1
      REAL QUARTZ
      REAL REFSMC1
      REAL WLTSMC1

      DATA MAXSMC/0.339, 0.421, 0.434, 0.476, 0.476, 0.439,  &
                  0.404, 0.464, 0.465, 0.406, 0.468, 0.468,  &
                  0.439, 1.000, 0.200, 0.421, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA SATPSI/0.069, 0.036, 0.141, 0.759, 0.759, 0.355,   &
                  0.135, 0.617, 0.263, 0.098, 0.324, 0.468,   &
                  0.355, 0.000, 0.069, 0.036, 0.000, 0.000,   &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,   &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA SATDK /1.07E-6, 1.41E-5, 5.23E-6, 2.81E-6, 2.81E-6, &
                  3.38E-6, 4.45E-6, 2.04E-6, 2.45E-6, 7.22E-6, &
                  1.34E-6, 9.74E-7, 3.38E-6, 0.00000, 1.41E-4, &
                  1.41E-5, 0.00   , 0.00   , 0.00   , 0.00,    &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,    &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00/
      DATA BB    /2.79,  4.26,  4.74,  5.33,  5.33,  5.25,    &
                  6.66,  8.72,  8.17, 10.73, 10.39, 11.55,    &
                  5.25,  0.00,  2.79,  4.26,  0.00,  0.00,    &
                  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,    &
                  0.00,  0.00,  0.00,  0.00,  0.00,  0.00/

      DATA QTZ   /0.92, 0.82, 0.60, 0.25, 0.10, 0.40,        &
                  0.60, 0.10, 0.35, 0.52, 0.10, 0.25,        &
                  0.05, 0.60, 0.07, 0.25, 0.00, 0.00,        &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,        &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/

! The following 5 parameters are derived later in REDPRM.f
! from the soil data, and are just given here for reference
! and to force static storage allocation
! Dag Lohmann, Feb. 2001

      DATA REFSMC/0.236, 0.283, 0.312, 0.360, 0.360, 0.329,     &
                  0.314, 0.387, 0.382, 0.338, 0.404, 0.412,     &
                  0.329, 0.000, 0.108, 0.283, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA WLTSMC/0.010, 0.028, 0.047, 0.084, 0.084, 0.066,     &
                  0.067, 0.120, 0.103, 0.100, 0.126, 0.138,     &
                  0.066, 0.000, 0.006, 0.028, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA DRYSMC/0.010, 0.028, 0.047, 0.084, 0.084, 0.066,     &
                  0.067, 0.120, 0.103, 0.100, 0.126, 0.138,     &
                  0.066, 0.000, 0.006, 0.028, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA SATDW /0.608E-6, 0.514E-5, 0.805E-5, 0.239E-4, 0.239E-4,  &
                  0.143E-4, 0.990E-5, 0.237E-4, 0.113E-4, 0.187E-4,  &
                  0.964E-5, 0.112E-4, 0.143E-4, 0.000000, 0.136E-3,  &
                  0.514E-5, 0.00   , 0.00   , 0.00   , 0.00,     &
                  0.00    , 0.00   , 0.00   , 0.00   , 0.00,     &
                  0.00    , 0.00   , 0.00   , 0.00   , 0.00/
      DATA F11  /-0.472, -1.044, -0.569, 0.162, 0.162, -0.327, &
                  -1.491, -1.118, -1.297, -3.209, -1.916, -2.138, &
                  -0.327,  0.000, -1.111, -1.044,  0.000,  0.000, &
                  0.000,  0.000,  0.000,  0.000,  0.000,  0.000, &
                  0.000,  0.000,  0.000,  0.000,  0.000,  0.000/

!#######################################################################

!  SET-UP VEGETATION PARAMETERS FOR A GIVEN VEGETAION TYPE
!
!  INPUT: VEGTYP = VEGETATION TYPE (INTEGER INDEX)
!  OUPUT: VEGETATION PARAMETERS
!         SHDFAC: VEGETATION GREENNESS FRACTION
!         RCMIN:  MIMIMUM STOMATAL RESISTANCE
!         RGL:    PARAMETER USED IN SOLAR RAD TERM OF
!                 CANOPY RESISTANCE FUNCTION
!         HS:     PARAMETER USED IN VAPOR PRESSURE DEFICIT TERM OF
!                 CANOPY RESISTANCE FUNCTION
!         SNUP:   THRESHOLD SNOW DEPTH (IN WATER EQUIVALENT M) THAT
!                 IMPLIES 100% SNOW COVER
!
!     USGS Vegetation Types
!C
!C    1:   Urban and Built-Up Land
!C    2:   Dryland Cropland and Pasture
!     3:   Irrigated Cropland and Pasture
!     4:   Mixed Dryland/Irrigated Cropland and Pasture
!     5:   Cropland/Grassland Mosaic
!C    6:   Cropland/Woodland Mosaic
!C    7:   Grassland
!C    8:   Shrubland
!C    9:   Mixed Shrubland/Grassland
!C   10:   Savanna
!C   11:   Deciduous Broadleaf Forest
!C   12:   Deciduous Needleleaf Forest
!C   13:   Evergreen Broadleaf Forest
!C   14:   Evergreen Needleleaf Forest
!C   15:   Mixed Forest
!C   16:   Water Bodies
!C   17:   Herbaceous Wetland
!C   18:   Wooded Wetland
!C   19:   Barren or Sparsely Vegetated
!C   20:   Herbaceous Tundra
!C   21:   Wooded Tundra
!C   22:   Mixed Tundra
!C   23:   Bare Ground Tundra
!C   24:   Snow or Ice


      INTEGER NROOT_DATA(MAX_VEGTYP)
      REAL    RSMTBL(MAX_VEGTYP)
      REAL    RGLTBL(MAX_VEGTYP)
      REAL    HSTBL(MAX_VEGTYP)
      REAL    SNUPX(MAX_VEGTYP)
      REAL    Z0_DATA(MAX_VEGTYP)
      REAL    LAI_DATA(MAX_VEGTYP)

      INTEGER NROOT
      REAL    SHDFAC
      REAL    RCMIN
      REAL    RGL
      REAL    HS
      REAL    FRZFACT
      REAL    PSISAT
      REAL    SNUP
      REAL    Z0
      REAL    LAI


      DATA NROOT_DATA /1,3,3,3,3,3,3,3,3,3,4,4,4,4,4,       &
                       0,2,2,1,3,3,3,2,1,0,0,0,0,0,0/

      DATA RSMTBL /200.0,  40.0,  40.0,  40.0,  40.0, 70.0, &
                    40.0, 300.0, 170.0,  70.0, 100.0, 150.0, &
                   150.0, 125.0, 125.0, 100.0,  40.0, 100.0, &
                   999.0, 150.0, 150.0, 150.0, 200.0, 999.0, &
                     0.0,   0.0,   0.0,   0.0,   0.0,  0.0/
      DATA RGLTBL / 999., 100., 100., 100., 100.,  65., 100., 100., &
                    100.,  65.,  30.,  30.,  30.,  30.,  30.,  30., &
                    100.,  30., 999., 100., 100., 100., 100., 999., &
                    0.0,  0.0,  0.0,   0.0,  0.0,  0.0/
      DATA HSTBL/999.0, 36.25, 36.25, 36.25, 36.25, 44.14, 36.35, &
                 42.00, 39.18, 54.53, 54.53, 47.35, 41.69, 47.35, &
                 51.93, 51.75, 60.00, 51.93, 999.0, 42.00, 42.00, &
                 42.00, 42.00, 999.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/

      DATA SNUPX  /0.040, 0.040, 0.040, 0.040, 0.040, 0.040, &
                   0.040, 0.030, 0.035, 0.040, 0.080, 0.080, &
                   0.080, 0.080, 0.080, 0.010, 0.010, 0.020, &
                   0.020, 0.025, 0.025, 0.025, 0.020, 0.020, &
                   0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA Z0_DATA /1.00,  0.07,  0.07,  0.07,  0.07,  0.15, &
                    0.08,  0.03,  0.05,  0.86,  0.80,  0.85, &
                    2.65,  1.09,  0.80,  0.001,  0.04,  0.05, &
                    0.01,  0.04,  0.06,  0.05,  0.03,  0.001, &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000/

      DATA LAI_DATA /4.0, 4.0, 4.0, 4.0, 4.0, 4.0,  &
                     4.0, 4.0, 4.0, 4.0, 4.0, 4.0,  &
                     4.0, 4.0, 4.0, 0.0, 4.0, 4.0,  &
                     4.0, 4.0, 4.0, 4.0, 4.0, 4.0,  &
                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0/

!#######################################################################

!  CLASS PARAMETER 'SLOPETYP' WAS INCLUDED TO ESTIMATE
!  LINEAR RESERVOIR COEFFICIENT 'SLOPE' TO THE BASEFLOW RUNOFF
!  OUT OF THE BOTTOM LAYER. LOWEST CLASS (SLOPETYP=0)MEANS
!  HIGHEST SLOPE PARAMETER= 1
!  DEFINITION OF SLOPETYP FROM 'ZOBLER' SLOPE TYPE
!  SLOPE CLASS      PERCENT SLOPE
!  1                0-8
!  2                8-30
!  3                > 30
!  4                0-30
!  5                0-8 & > 30
!  6                8-30 & > 30
!  7                0-8, 8-30, > 30
!  9                GLACIAL ICE
!  BLANK            OCEAN/SEA
!  NOTE:  CLASS 9 FROM 'ZOBLER' FILE SHOULD BE REPLACED BY 8
!  AND 'BLANK'  9

      REAL SLOPE
      REAL SLOPE_DATA(MAX_SLOPETYP)
      DATA SLOPE_DATA /0.1,  0.6, 1.0, 0.35, 0.55, 0.8,  &
                       0.63, 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0/

!#######################################################################

!  Set namelist file name

      CHARACTER*50 NAMELIST_NAME

!#######################################################################

! SET UNIVERSAL PARAMETERS (NOT DEPENDENT ON SOIL, VEG, SLOPE TYPE)

      INTEGER VEGTYP
      INTEGER SOILTYP
      INTEGER SLOPETYP

      INTEGER NSOIL
      INTEGER I

      INTEGER BARE
      DATA    BARE /11/

      LOGICAL LPARAM
      DATA    LPARAM /.TRUE./

      LOGICAL LFIRST
      DATA    LFIRST /.TRUE./

!  Parameter used to calculate roughness length of heat
      REAL CZIL, CZIL_DATA
      DATA CZIL_DATA /0.2/

!  Parameter used to caluculate vegetation effect on soil heat flux
      REAL SBETA, SBETA_DATA
      DATA SBETA_DATA /-2.0/

! BARE SOIL EVAPORATION EXPONENT USED IN DEVAP

      REAL FXEXP, FXEXP_DATA
      DATA FXEXP_DATA /2.0/

! Soil heat capacity [J/m^3/K]

      REAL CSOIL, CSOIL_DATA
      DATA CSOIL_DATA /1.26E+6/

!  SPECIFY SNOW DISTRIBUTION SHAPE PARAMETER
!  SALP   - SHAPE PARAMETER OF DISTRIBUTION FUNCTION
!  OF SNOW COVER. FROM ANDERSON'S DATA (HYDRO-17)
!  BEST FIT IS WHEN SALP = 2.6
      REAL SALP, SALP_DATA
      DATA SALP_DATA /2.6/

!  KDT IS DEFINED BY REFERENCE REFKDT AND DKSAT
!  REFDK=2.E-6 IS THE SAT. DK. VALUE FOR THE SOIL TYPE 2
      REAL REFDK, REFDK_DATA
      DATA REFDK_DATA /2.0E-6/

      REAL REFKDT, REFKDT_DATA
      DATA REFKDT_DATA /3.0/

      REAL KDT
      REAL FRZX

!  FROZEN GROUND PARAMETER, FRZK, DEFINITION
!  FRZK IS ICE CONTENT THRESHOLD ABOVE WHICH FROZEN SOIL IS IMPERMEABLE
!  REFERENCE VALUE OF THIS PARAMETER FOR THE LIGHT CLAY SOIL (TYPE=3)
!  FRZK = 0.15 M
      REAL FRZK, FRZK_DATA
      DATA FRZK_DATA /0.15/

      REAL RTDIS(NSOIL)
      REAL SLDPTH(NSOIL)
      REAL ZSOIL(NSOIL)

!  Set two canopy water parameters
      REAL CFACTR, CFACTR_DATA
      REAL CMCMAX, CMCMAX_DATA
      DATA CFACTR_DATA /0.5/
      DATA CMCMAX_DATA /0.5E-3/

!  Set max. stomatal resistance
      REAL RSMAX, RSMAX_DATA
      DATA RSMAX_DATA /5000.0/

!  Set optimum transpiration air temperature
      REAL TOPT, TOPT_DATA
      DATA TOPT_DATA /298.0/

!  Specify depth[m] of lower boundary soil temperature
      REAL ZBOT, ZBOT_DATA
      DATA ZBOT_DATA /-3.0/

!#######################################################################

!  Namelist definition

      NAMELIST /SOIL_VEG/ SLOPE_DATA, RSMTBL, RGLTBL, HSTBL, SNUPX,     &
           BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW,       &
           WLTSMC, QTZ, LPARAM, ZBOT_DATA, SALP_DATA, CFACTR_DATA,      &
           CMCMAX_DATA, SBETA_DATA, RSMAX_DATA, TOPT_DATA,              &
           REFDK_DATA, FRZK_DATA, BARE, DEFINED_VEG, DEFINED_SOIL,      &
           DEFINED_SLOPE, FXEXP_DATA, NROOT_DATA, REFKDT_DATA, Z0_DATA, &
           CZIL_DATA, LAI_DATA, CSOIL_DATA

!  Read namelist file to override default parameters
!  only once.
!
!  7/6/01 : E. Rogers commented out read of unit 58 since
!           NCO does not allow hardwired file names in the code.

!     IF (LFIRST) THEN
!        OPEN(58, FILE = 'namelist_filename.txt')
! NAMELIST_NAME must be 50 characters or less.
!        READ(58,'(A)') NAMELIST_NAME
!        CLOSE(58)
!        WRITE(*,*) 'Namelist Filename is ', NAMELIST_NAME
!        OPEN(59, FILE = NAMELIST_NAME)
 50      CONTINUE
!           READ(59, SOIL_VEG, END=100)
!        IF (LPARAM) GOTO 50
 100     CONTINUE
!        CLOSE(59)
!        WRITE(*,NML=SOIL_VEG)
!        LFIRST = .FALSE.
         IF (DEFINED_SOIL .GT. MAX_SOILTYP) THEN
            CALL wrf_error_fatal( 'Warning: DEFINED_SOIL too large in namelist' )
         END IF
         IF (DEFINED_VEG .GT. MAX_VEGTYP) THEN
            CALL wrf_error_fatal( 'Warning: DEFINED_VEG too large in namelist')
         END IF
         IF (DEFINED_SLOPE .GT. MAX_SLOPETYP) THEN
            CALL wrf_error_fatal( 'Warning: DEFINED_SLOPE too large in namelist')
         END IF

         DO I = 1, DEFINED_SOIL
!           if(abs(satdk(i))<=1.e-7.or.abs(satpsi(i))<=1.e-7)cycle
!           SATDW(I)  = BB(I)*SATDK(I)*(SATPSI(I)/MAXSMC(I))
!           F11(I)    = ALOG10(SATPSI(I)) + BB(I)*ALOG10(MAXSMC(I)) + 2.0
!           REFSMC1   = MAXSMC(I)*(5.79E-9/SATDK(I))  &
!                                         **(1.0/(2.0*BB(I)+3.0))
!           REFSMC(I) = REFSMC1 + (MAXSMC(I)-REFSMC1) / 3.0
!           WLTSMC1   = MAXSMC(I) * (200.0/SATPSI(I))**(-1.0/BB(I))
!           WLTSMC(I) = WLTSMC1 - 0.5 * WLTSMC1
! Current version DRYSMC values that equate to WLTSMC
! Future version could let DRYSMC be independently set via namelist
!           DRYSMC(I) = WLTSMC(I)
         END DO

!     END IF

      IF (SOILTYP .GT. DEFINED_SOIL) THEN
         CALL wrf_error_fatal( 'Warning: to many soil types' )
      END IF
      IF (VEGTYP .GT. DEFINED_VEG) THEN
         CALL wrf_error_fatal( 'Warning: to many veg types' )
      END IF
      IF (SLOPETYP .GT. DEFINED_SLOPE) THEN
         CALL wrf_error_fatal( 'Warning: to many slope types' )
      END IF

!  SET-UP UNIVERSAL PARAMETERS
! (NOT DEPENDENT ON SOILTYP, VEGTYP OR SLOPETYP)
      ZBOT   = ZBOT_DATA
      SALP   = SALP_DATA
      CFACTR = CFACTR_DATA
      CMCMAX = CMCMAX_DATA
      SBETA  = SBETA_DATA
      RSMAX  = RSMAX_DATA
      TOPT   = TOPT_DATA
      REFDK  = REFDK_DATA
      FRZK   = FRZK_DATA
      FXEXP  = FXEXP_DATA
      REFKDT = REFKDT_DATA
      CZIL   = CZIL_DATA
      CSOIL  = CSOIL_DATA

!  SET-UP SOIL PARAMETERS
      B       = BB(SOILTYP)
      SMCDRY  = DRYSMC(SOILTYP)
      F1      = F11(SOILTYP)
      SMCMAX  = MAXSMC(SOILTYP)
      SMCREF  = REFSMC(SOILTYP)
      PSISAT  = SATPSI(SOILTYP)
      DKSAT   = SATDK(SOILTYP)
      DWSAT   = SATDW(SOILTYP)
      SMCWLT  = WLTSMC(SOILTYP)
      QUARTZ  = QTZ(SOILTYP)
      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)
      KDT     = REFKDT * DKSAT/REFDK

!  TO ADJUST FRZK PARAMETER TO ACTUAL SOIL TYPE: FRZK * FRZFACT

      FRZX = FRZK * FRZFACT

!  SET-UP VEGETATION PARAMETERS
      NROOT = NROOT_DATA(VEGTYP)
      SNUP  = SNUPX(VEGTYP)
      RCMIN = RSMTBL(VEGTYP)
      RGL   = RGLTBL(VEGTYP)
      HS    = HSTBL(VEGTYP)
      Z0    = Z0_DATA(VEGTYP)
      LAI   = LAI_DATA(VEGTYP)
      IF(VEGTYP .EQ. BARE) SHDFAC = 0.0

      IF (NROOT .GT. NSOIL) THEN
         CALL wrf_error_fatal( 'Warning: too many root layers') 
      END IF

!  CALCULATE ROOT DISTRIBUTION
!  PRESENT VERSION ASSUMES UNIFORM DISTRIBUTION BASED ON SOIL LAYERS

      DO I=1,NROOT
         RTDIS(I) = -SLDPTH(I)/ZSOIL(NROOT)
      END DO

!  SET-UP SLOPE PARAMETER
      SLOPE = SLOPE_DATA(SLOPETYP)
!
      END SUBROUTINE REDPRM_USGS

!--------------------------------------------------------------------!


      SUBROUTINE ROSR12 ( P, A, B, C, D, DELTA, NSOIL,I,J)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN
!C    =======   BELOW:
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER KK
      INTEGER NSOIL
      
      REAL P     (NSOIL)
      REAL A     (NSOIL)
      REAL B     (NSOIL)
      REAL C     (NSOIL)
      REAL D     (NSOIL)
      REAL DELTA (NSOIL)
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      C(NSOIL) = 0.0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     SOLVE THE COEFS FOR THE 1ST SOIL LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      P(1) = -C(1) / B(1)
      DELTA(1) = D(1) / B(1)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO K = 2 , NSOIL
        P(K) = -C(K) * ( 1.0 / (B(K) + A (K) * P(K-1)) )
        DELTA(K) = (D(K)-A(K)*DELTA(K-1))*(1.0/(B(K)+A(K)*P(K-1)))
      END Do

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     SET P TO DELTA FOR LOWEST SOIL LAYER.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      P(NSOIL) = DELTA(NSOIL)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO K = 2 , NSOIL
         KK = NSOIL - K + 1
         P(KK) = P(KK) * P(KK+1) + DELTA(KK)
      END DO

      END SUBROUTINE ROSR12

      SUBROUTINE SHFLX(S,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,TBOT, &
           ZBOT, SMCWLT, PSISAT, SH2O, B,F1,DF1,ICE,QUARTZ,CSOIL,II,J)
      
      IMPLICIT NONE
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON
!C              THE THERMAL DIFFUSION EQUATION AND UPDATE THE FROZEN SOIL
!C              MOISTURE CONTENT BASED ON THE TEMPERATURE.
!C      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER NSOLD
      PARAMETER ( NSOLD = 20 )
!tst      PARAMETER ( NSOLD = 4 )

      INTEGER I
      INTEGER II
      INTEGER J
      INTEGER ICE
      INTEGER IFRZ
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL B
      REAL BI(NSOLD)
      REAL CI(NSOLD)
      REAL DF1
      REAL CSOIL
      REAL DT
      REAL F1
      REAL PSISAT
      REAL QUARTZ
!believewrong      REAL RHSTS ( NSOLD )
      REAL RHSTS ( NSOIL )
      REAL S
      REAL SMC   ( NSOIL )
      REAL SH2O  ( NSOIL )
      REAL SMCMAX
      REAL SMCWLT
      REAL STC	(NSOIL)
      REAL STCF	(NSOLD)
      REAL T0
      REAL T1
      REAL TBOT
      REAL ZBOT
      REAL YY
      REAL ZSOIL ( NSOIL )
      REAL ZZ1

!tmp
	REAL STCWAS
!tmp

      PARAMETER ( T0 = 273.15)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!
	STCWAS=STC(1)
!

      IF(ICE.EQ.1) THEN

!..SEA-ICE CASE

         CALL HRTICE(RHSTS,STC,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

	if (minval(RHSTS) .lt. -100.) then
	write(0,*) 'small RHSTS in seaice branch: ', RHSTS(1)
	endif

         CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI,II,J)
         
      ELSE

!..LAND-MASS CASE

         CALL HRT(RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,TBOT, &
              ZBOT, PSISAT, SH2O, DT,                           &
              B,F1,DF1,QUARTZ,CSOIL,AI,BI,CI,II,J)

	if (DT*RHSTS(1) .lt. -100.) then
	write(0,*) 'crazy RHSTS returned from HRT: ', RHSTS
	endif

         
         CALL HSTEP(STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI,II,J)

	if (RHSTS(1) .lt. -100.) then
!	write(0,*) 'crazy RHSTS returned from HSTEP: ', RHSTS
	endif

      ENDIF

      DO I = 1,NSOIL
         STC(I)  = STCF(I)
      END DO
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     IN THE NO SNOWPACK CASE (VIA ROUTINE NOPAC BRANCH,) UPDATE THE
!     GRND (SKIN) TEMPERATURE HERE IN RESPONSE TO THE UPDATED SOIL 
!     TEMPERATURE PROFILE ABOVE.
! (NOTE: INSPECTION OF ROUTINE SNOPAC SHOWS THAT T1 BELOW IS A DUMMY
!     VARIABLE ONLY, AS SKIN TEMPERATURE IS UPDATED DIFFERENTLY
!     IN ROUTINE SNOPAC) 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      T1 = (YY + (ZZ1 - 1.0) * STC(1)) / ZZ1

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC THE SFC SOIL HEAT FLUX
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!      S = DF1 * (STC(1) - T1) / (0.5 * ZSOIL(1))

	if (STC(1) .lt. 230 .and. STCWAS .gt. 230) then
!	write(0,*) 'ICE, STC(1) was, is: ',ICE, STCWAS, STC(1)
!	write(0,*) 'RHSTS in SHFLX: ', RHSTS
	endif

      END SUBROUTINE SHFLX

      SUBROUTINE SMFLX ( ETA1,SMC,NSOIL,CMC,ETP1,DT,PRCP1,ZSOIL,  &
           SH2O, SLOPE, KDT, FRZFACT,                             &
           SMCMAX,B,PC,SMCWLT,DKSAT,DWSAT,SMCREF,SHDFAC,CMCMAX,   &
           SMCDRY,CFACTR, RUNOFF1,RUNOFF2, RUNOFF3, EDIR1, EC1,   &
           ETT1, SFCTMP,Q2,NROOT,RTDIS, FXEXP,DRIP,EC,EDIR,ETT,   &
           RUNOF,II,J)


      IMPLICIT NONE

! ------------    FROZEN GROUND VERSION    --------------------------
!   NEW STATES ADDED: SH2O, AND FROZEN GROUD CORRECTION FACTOR, FRZFACT
!   AND PARAMETER SLOPE 
!

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE
!C    =======   CONTENT (SMC - A PER UNIT VOLUME MEASUREMENT) IS A
!C              DEPENDENT VARIABLE THAT IS UPDATED WITH PROGNOSTIC EQNS.
!C              THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
      INTEGER NSOLD
      PARAMETER ( NSOLD = 20 )
!tst      PARAMETER ( NSOLD = 4 )
      INTEGER II
      INTEGER J
      INTEGER K
      INTEGER NSOIL
      REAL B
      REAL CFACTR
      REAL CMC
      REAL CMCMAX
      REAL DEW
      REAL DKSAT
      REAL DRIP
      REAL DT
      REAL DWSAT
      REAL EC
      REAL EDIR
      REAL ET     ( NSOLD )
      REAL ETA1
      REAL ETP1
      REAL ETT
      REAL EXCESS
      REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL KDT
      REAL PC
      REAL PCPDRP
      REAL PRCP1
      REAL RHSCT
      REAL RHSTT  ( NSOLD )
      REAL RTDIS (NSOIL)
      REAL RUNOF
      REAL RUNOFF,RUNOXX3
      REAL SHDFAC
      REAL SMC    ( NSOIL )

! ---------------    FROZEN GROUND VERSION     ---------------------
      
      REAL SH2O   ( NSOIL )
      REAL SICE   ( NSOLD )
      REAL SH2OA  ( NSOLD )
      REAL SH2OFG ( NSOLD )

!tst
	REAL SH2Oold,SICEold
!tst
! -------------------------------------------------------------------
           
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL TRHSCT
      REAL ZSOIL  ( NSOIL )

! Temperature criteria for snowfall TFREEZ should have 
! same value as in SFLX.f
      REAL TFREEZ
      PARAMETER (TFREEZ = 273.15)

      REAL SLOPE, FRZFACT, RUNOFF1, RUNOFF2, RUNOFF3, EDIR1, EC1
      REAL ETT1, SFCTMP, Q2, DUMMY, CMC2MS

      INTEGER NROOT, I
      real ai(nsold)
      real bi(nsold)
      real ci(nsold)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     EXECUTABLE CODE BEGINS HERE....IF THE POTENTIAL EVAPOTRANS-
!     PIRATION IS GREATER THAN ZERO...
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DUMMY=0.
      EDIR = 0.
      EC = 0.
      ETT = 0.
      DO K = 1, NSOIL
         ET ( K ) = 0.
      END DO
      
! ----------------------------------------------------------------------
      IF ( ETP1 .GT. 0.0 ) THEN

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       RETRIEVE DIRECT EVAPORATION FROM SOIL SURFACE
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! ----------------------------------------------------------------------
! call this function only if veg cover not complete
! --------------     FROZEN GROUND VERSION     ---------------------
!   SMC STATES WERE REPLACED BY SH2O STATES
!
        IF (SHDFAC .LT. 1.) THEN
          EDIR = DEVAP ( ETP1, SH2O(1), ZSOIL(1), SHDFAC, SMCMAX,  &
            B, DKSAT, DWSAT, SMCDRY,SMCREF, SMCWLT, FXEXP,II,J)
        ENDIF
! ----------------------------------------------------------------------
!       INITIALIZE PLANT TOTAL TRANSPIRATION, RETRIEVE PLANT
!       TRANSPIRATION, AND ACCUMULATE IT FOR ALL SOIL LAYERS.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!        ETT = 0.
         
        IF(SHDFAC.GT.0.0) THEN
        
! ----------------------------------------------------------------------
! --------------     FROZEN GROUND VERSION     ---------------------
!   SMC STATES WERE REPLACED BY SH2O STATES
!
          CALL TRANSP ( ET,NSOIL,ETP1,SH2O,CMC,ZSOIL,SHDFAC,SMCWLT,  &
            CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)
          
          DO K = 1 , NSOIL
            ETT = ETT + ET ( K )
          END DO
! move this ENDIF after canopy evap calcs since CMC=0 for SHDFAC=0
!        ENDIF

! ----------------------------------------------------------------------
!       CALCULATE CANOPY EVAPORATION
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!cc If statements to avoid TANGENT LINEAR problems near CMC=zero
          IF (CMC .GT. 0.0) THEN
            EC = SHDFAC * ( ( CMC / CMCMAX ) ** CFACTR ) * ETP1
          ELSE
            EC = 0.0
          ENDIF
! ----------------------------------------------------------------------
!########  EC SHOULD BE LIMITED BY THE TOTAL AMOUNT OF AVAILABLE
!          WATER ON THE CANOPY. MODIFIED BY F.CHEN ON 10/18/94
!########
          CMC2MS = CMC / DT
          EC = MIN ( CMC2MS, EC )
        ENDIF
      ENDIF

! ----------------------------------------------------------------------
!     TOTAL UP EVAP AND TRANSP TYPES TO OBTAIN ACTUAL EVAPOTRANSP
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      EDIR1=EDIR
      EC1=EC
      ETT1=ETT
      
      ETA1 = EDIR + ETT + EC
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     COMPUTE THE RIGHT HAND SIDE OF THE CANOPY EQN TERM ( RHSCT )
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      RHSCT = SHDFAC * PRCP1 - EC

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CONVERT RHSCT (A RATE) TO TRHSCT (AN AMT) AND ADD IT TO EXISTING
!     CMC. IF RESULTING AMT EXCEEDS MAX CAPACITY, IT BECOMES DRIP
!     AND WILL FALL TO THE GRND.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DRIP = 0.
      TRHSCT = DT * RHSCT
      EXCESS =  CMC + TRHSCT
      IF ( EXCESS .GT. CMCMAX ) DRIP = EXCESS - CMCMAX

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PCPDRP IS THE COMBINED PRCP1 AND DRIP (FROM CMC) THAT
!     GOES INTO THE SOIL
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PCPDRP = (1. - SHDFAC) * PRCP1 + DRIP / DT

!      PRINT*,' ################ SMLX ##################'
!      PRINT*,' PCPDRP=', PCPDRP, ' EDIR=', EDIR,' ET=', ET,
!     *      'SMC(1)=', SMC(1), 'SMC(2)=', SMC(2), ' PRCP1=', PRCP1,
!     *      'DRIP = ', DRIP / DT

! ---------------     FROZEN GROUND VERSION     --------------------
!    STORE ICE CONTENT AT EACH SOIL LAYER BEFORE CALLING SRT & SSTEP
!
      DO I = 1,NSOIL
	if (SH2O(I) .lt. 0) then
	write(0,*) 'neg SH2O in SICE calc: ', SH2O(I)
	endif
         SICE(I) = SMC(I) - SH2O(I)
      END DO
! ------------------------------------------------------------------
            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALL SUBROUTINES SRT AND SSTEP TO SOLVE THE SOIL MOISTURE
!     TENDENCY EQUATIONS. 
!
!  IF THE INFILTRATING PRECIP RATE IS NONTRIVIAL,
!
!    (WE CONSIDER NONTRIVIAL TO BE A PRECIP TOTAL OVER THE TIME STEP 
!     EXCEEDING ONE ONE-THOUSANDTH OF THE WATER HOLDING CAPACITY OF 
!     THE FIRST SOIL LAYER)
! 
!  THEN CALL THE SRT/SSTEP SUBROUTINE PAIR TWICE IN THE MANNER OF 
!    TIME SCHEME "F" (IMPLICIT STATE, AVERAGED COEFFICIENT)
!    OF SECTION 2 OF KALNAY AND KANAMITSU (1988, MWR, VOL 116, 
!    PAGES 1945-1958)TO MINIMIZE 2-DELTA-T OSCILLATIONS IN THE 
!    SOIL MOISTURE VALUE OF THE TOP SOIL LAYER THAT CAN ARISE BECAUSE
!    OF THE EXTREME NONLINEAR DEPENDENCE OF THE SOIL HYDRAULIC 
!    DIFFUSIVITY COEFFICIENT AND THE HYDRAULIC CONDUCTIVITY ON THE
!    SOIL MOISTURE STATE
!
!  OTHERWISE CALL THE SRT/SSTEP SUBROUTINE PAIR ONCE IN THE MANNER OF
!    TIME SCHEME "D" (IMPLICIT STATE, EXPLICIT COEFFICIENT) 
!    OF SECTION 2 OF KALNAY AND KANAMITSU
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! PCPDRP IS UNITS OF KG/M**2/S OR MM/S, ZSOIL IS NEGATIVE DEPTH IN M 
!......IF ( PCPDRP .GT. 0.0 ) THEN

      IF ( (PCPDRP*DT) .GT. (0.001*1000.0*(-ZSOIL(1))*SMCMAX) ) THEN

! ---------------    FROZEN GROUND VERSION       ---------------------
!    SMC STATES REPLACED BY SH2O STATES IN SRT SUBR.
!    SH2O & SICE STATES INCLUDED IN SSTEP SUBR.
!    FROZEN GROUND CORRECTION FACTOR, FRZFACT, ADDED
!    ALL WATER BALANCE CALCULATIONS USING UNFROZEN WATER
!
         CALL SRT ( RHSTT,RUNOFF,EDIR,ET,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL, &
              DWSAT,DKSAT,SMCMAX, B, RUNOFF1,                          &
              RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT, SICE,AI,BI,CI,II,J)
         
         CALL SSTEP ( SH2OFG,SH2O,DUMMY,RHSTT,RHSCT,DT,NSOIL,SMCMAX, &
              CMCMAX, RUNOFF3, ZSOIL, SMC, SICE,AI,BI,CI,II,J)
         
         DO K = 1, NSOIL
            SH2OA(K) = ( SH2O(K) + SH2OFG(K) ) * 0.5
         END DO
        
         CALL SRT ( RHSTT,RUNOFF,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,ZSOIL, &
              DWSAT,DKSAT,SMCMAX, B, RUNOFF1,                           &
              RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT, SICE,AI,BI,CI,II,J)
         
         CALL SSTEP ( SH2O,SH2O,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,        &
              CMCMAX, RUNOFF3, ZSOIL,SMC,SICE,AI,BI,CI,II,J)
         
      ELSE


	SH2Oold=SH2O(1)
        SICEold=SICE(1)
         
         CALL SRT ( RHSTT,RUNOFF,EDIR,ET,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL, &
              DWSAT,DKSAT,SMCMAX, B, RUNOFF1,                          &
              RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT, SICE,AI,BI,CI,II,J)
         
         
	if (SICE(1) + SH2O(1) .gt. 1.0) then
	write(0,*) 'bonus moisture into SSTEP'
	write(0,*) 'SH2Oold, SICEold: ', SH2Oold, SICEold
	write(0,*) 'SH2O(1), SICE(1): ', SH2O(1), SICE(1)
	endif
         CALL SSTEP ( SH2O,SH2O,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,       &
              CMCMAX, RUNOFF3, ZSOIL,SMC,SICE,AI,BI,CI,II,J)

	if (SH2O(1) .lt. 0) then
	write(0,*) 'neg SH2O AFTER SRT, SSTEP calls: ', SH2O(1)
	write(0,*) 'SH2O was: ', SH2Oold
	write(0,*) 'SMCMAX, SMCWLT, DWSAT, DKSAT:: ', SMCMAX, SMCWLT, DWSAT, DKSAT
	endif
         
      ENDIF
      
      RUNOF = RUNOFF
      END SUBROUTINE SMFLX

      SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT, &
        SMCREF, SMCDRY, CMC, CMCMAX, NSOIL, DT, SBETA, Q1, DF1,       &
        Q2,T1,SFCTMP,T24,TH2,F,F1,S,STC,EPSCA,SFCPRS,                 &
!        B, PC, RCH, RR, CFACTR, SALP, ESD,                           &
        B, PC, RCH, RR, CFACTR, SNCOVER, ESD, SNDENS,                 &
        SNOWH, SH2O, SLOPE, KDT, FRZFACT, PSISAT,SNUP,                &
        ZSOIL, DWSAT, DKSAT, TBOT, ZBOT, SHDFAC, RUNOFF1,             &
        RUNOFF2,RUNOFF3,EDIR1,EC1,ETT1,NROOT,SNMAX,ICE,               &
        RTDIS,QUARTZ, FXEXP,CSOIL,                                    &
        BETA,DRIP,EC,EDIR,ETT,FLX1,FLX2,FLX3,RUNOF,DEW,I,J)

      IMPLICIT NONE

! ----------------------------------------------------------------------
!C    PURPOSE:  TO CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES & UPDATE
!C    =======   SOIL MOISTURE CONTENT AND SOIL HEAT CONTENT VALUES FOR
!C              THE CASE WHEN A SNOW PACK IS PRESENT.
! ----------------------------------------------------------------------

      INTEGER I
      INTEGER J
      INTEGER ICE
      INTEGER NROOT
      INTEGER NSOIL

      LOGICAL SNOWNG

      REAL B
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
      REAL EXPSNO
      REAL EXPSOI
      REAL ETA
      REAL ETA1
      REAL ETP
      REAL ETP1
      REAL ETP2
      REAL ETT
      REAL EX
      REAL EXPFAC
      REAL F
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
      REAL Q1
      REAL Q2
      REAL RCH
      REAL RR
      REAL RTDIS   ( NSOIL )
      REAL RUNOFF,RUNOF
      REAL S
      REAL SBETA
      REAL S1
      REAL SFCTMP
      REAL SHDFAC
      REAL SIGMA
      REAL SMC     ( NSOIL )
      REAL SH2O    ( NSOIL )
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL SNMAX
      REAL SNOWH
      REAL STC     ( NSOIL )
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
      REAL ZSOIL( NSOIL )
      REAL ZZ1

!tst
	REAL STCWAS
!tst

!
      REAL TFREEZ, SALP, SFCPRS, SLOPE, FRZFACT, PSISAT, SNUP
      REAL RUNOFF1, RUNOFF2, RUNOFF3,RUNOXX3
      REAL EDIR1, EC1, ETT1, QUARTZ
      REAL SNDENS, SNCOND, RSNOW, SNCOVER, QSAT, ETP3, SEH, T14

      PARAMETER(CP=1004.5,CPH2O=4.218E+3,CPICE=2.106E+3,         &
        LSUBF=3.335E+5,LSUBC=2.501000E+6,LSUBS=2.83E+6,SIGMA=5.67E-8)

      PARAMETER ( TFREEZ = 273.15)

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE...
! CONVERT POTENTIAL EVAP (ETP) FROM KG M-2 S-1 TO M S-1 AND THEN TO AN
! AMOUNT (M) GIVEN TIMESTEP (DT) AND CALL IT AN EFFECTIVE SNOWPACK
! REDUCTION AMOUNT, ETP2 (M).  THIS IS THE AMOUNT THE SNOWPACK WOULD BE
! REDUCED DUE TO EVAPORATION FROM THE SNOW SFC DURING THE TIMESTEP.
! EVAPORATION WILL PROCEED AT THE POTENTIAL RATE UNLESS THE SNOW DEPTH
! IS LESS THAN THE EXPECTED SNOWPACK REDUCTION.
! IF SEAICE (ICE=1), BETA REMAINS=1.
! ----------------------------------------------------------------------
      PRCP1 = PRCP1*0.001

      ETP2 = ETP * 0.001 * DT
      BETA = 1.0
      IF(ICE .NE. 1) THEN
        IF (ESD .LT. ETP2) THEN
          BETA = ESD / ETP2
        ENDIF
      ENDIF

! ----------------------------------------------------------------------
! IF ETP<0 (DOWNWARD) THEN DEWFALL (=FROSTFALL IN THIS CASE).
! ----------------------------------------------------------------------
      DEW = 0.0
      IF (ETP .LT. 0.0) THEN
        DEW = -ETP * 0.001
      ENDIF

! ----------------------------------------------------------------------
! If precip is falling, calculate heat flux from snow sfc to newly
! accumulating precip.  Note that this reflects the flux appropriate for
! the not-yet-updated skin temperature (T1).  Assumes temperature of the
! snowfall striking the gound is =SFCTMP (lowest model level air temp).
! ----------------------------------------------------------------------
      FLX1 = 0.0
      IF ( SNOWNG ) THEN
        FLX1 = CPICE * PRCP * ( T1 - SFCTMP )
      ELSE
        IF (PRCP .GT. 0.0) FLX1 = CPH2O * PRCP * (T1 - SFCTMP)
      ENDIF
      DSOIL = -(0.5 * ZSOIL(1))
      DTOT = SNOWH + DSOIL

! ----------------------------------------------------------------------
! Calculate an 'effective snow-grnd sfc temp' (T12) based on heat fluxes
! between the snow pack and the soil and on net radiation.
! Include FLX1 (precip-snow sfc) and FLX2 (freezing rain latent heat)
! fluxes.  FLX1 from above, FLX2 brought in via COMMOM block RITE.
! FLX2 reflects freezing rain latent heat flux using T1 calculated in
! PENMAN.
! ----------------------------------------------------------------------
      DENOM = 1.0 + DF1 / ( DTOT * RR * RCH )
      T12A = ((F - FLX1 - FLX2 - SIGMA * T24) /                         &
             RCH+TH2-SFCTMP-BETA*EPSCA) / RR
      T12B = DF1 * STC(1) / ( DTOT * RR * RCH )
      T12 = (SFCTMP + T12A + T12B ) / DENOM      

! ----------------------------------------------------------------------
! IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS AT OR BELOW FREEZING, NO SNOW
! MELT WILL OCCUR.  SET THE SKIN TEMP TO THIS EFFECTIVE TEMP AND SET THE
! EFFECTIVE PRECIP TO ZERO.
! ----------------------------------------------------------------------
      IF (T12 .LE. TFREEZ) THEN
        ESD = MAX(0.0, ESD-ETP2)

!ggg    update snow depth.
        snowh = esd / sndens
!ggg

        T1 = T12
! ----------------------------------------------------------------------
! Update soil heat flux (S) using new skin temperature (T1)
        S = DF1 * ( T1 - STC(1) ) / ( DTOT )
        FLX3 = 0.0
        EX = 0.0
        SNMAX = 0.0

! ----------------------------------------------------------------------
! IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS ABOVE FREEZING, SNOW MELT
! WILL OCCUR.  CALL THE SNOW MELT RATE,EX AND AMT, SNMAX.  REVISE THE
! EFFECTIVE SNOW DEPTH.  REVISE THE SKIN TEMP BECAUSE IT WOULD HAVE CHGD
! DUE TO THE LATENT HEAT RELEASED BY THE MELTING. CALC THE LATENT HEAT
! RELEASED, FLX3. SET THE EFFECTIVE PRECIP, PRCP1 TO THE SNOW MELT RATE,
! EX FOR USE IN SMFLX.  ADJUSTMENT TO T1 TO ACCOUNT FOR SNOW PATCHES.
! ----------------------------------------------------------------------
      ELSE
!        IF ( (SNUP .GT. 0.0) .AND. (ESD .LT. SNUP) ) THEN
! turn off this block below since SNCOVER is calculated (as SNOFAC) in
! SFLX and now passed to SNOPAC
!        IF (ESD .LT. SNUP) THEN
!          RSNOW = ESD / SNUP
!          SNCOVER = 1.- (EXP(-SALP*RSNOW)-RSNOW*EXP(-SALP))
!        ELSE
!          SNCOVER = 1.
!        ENDIF  
        T1 = TFREEZ * SNCOVER + T12 * ( 1.0 - SNCOVER )
        QSAT = (0.622*6.11E2)/(SFCPRS-0.378*6.11E2)
        ETP = RCH*(QSAT-Q2)/CP
        ETP2 = ETP*0.001*DT
        BETA = 1.0
	
! ----------------------------------------------------------------------
! IF POTENTIAL EVAP (SUBLIMATION) GREATER THAN DEPTH OF SNOWPACK.
! BETA<1
! ----------------------------------------------------------------------
        IF ( ESD .LE. ETP2 ) THEN
          BETA = ESD / ETP2
          ESD = 0.0

!ggg      snow pack has sublimated, set depth to zero
          snowh = 0.0
!ggg

          SNMAX = 0.0
          EX = 0.0
! ----------------------------------------------------------------------
! Update soil heat flux (S) using new skin temperature (T1)
          S = DF1 * ( T1 - STC(1) ) / ( DTOT )
	  
! ----------------------------------------------------------------------
! POTENTIAL EVAP (SUBLIMATION) LESS THAN DEPTH OF SNOWPACK, BETA=1.
! SNOWPACK (ESD) REDUCED BY POT EVAP RATE
! ETP3 (CONVERT TO FLUX)
! UPDATE SOIL HEAT FLUX BECAUSE T1 PREVIOUSLY CHANGED.
! SNOWMELT REDUCTION DEPENDING ON SNOW COVER
! IF SNOW COVER LESS THAN 5% NO SNOWMELT REDUCTION
! ----------------------------------------------------------------------
        ELSE
!          ESD = MAX(0.0, ESD-ETP2)
          ESD = ESD-ETP2

!ggg      snow pack reduced by sublimation, reduce snow depth
          snowh = esd / sndens
!ggg

          ETP3 = ETP*LSUBC
          S = DF1 * ( T1 - STC(1) ) / ( DTOT )
          SEH = RCH*(T1-TH2)
          T14 = T1*T1
          T14 = T14*T14
          FLX3 = F - FLX1 - FLX2 - SIGMA*T14 - S - SEH - ETP3
          IF(FLX3.LE.0.0) FLX3=0.0
          EX = FLX3*0.001/LSUBF
! ----------------------------------------------------------------------
! Does below fail to match the melt water with the melt energy?
          IF ( SNCOVER .GT. 0.05) EX = EX * SNCOVER
          SNMAX = EX * DT
        ENDIF
        
! ----------------------------------------------------------------------
! SNMAX.LT.ESD
! ELSE
! ----------------------------------------------------------------------
!        IF(SNMAX.LT.ESD) THEN
! The 1.E-6 value represents a snowpack depth threshold value (0.1 mm)
! below which we choose not to retain any snowpack, and instead include
! it in snowmelt.
        IF(SNMAX.LT.ESD-1.E-6) THEN
          ESD = ESD - SNMAX

!ggg      snow melt reduced snow pack, reduce snow depth
          snowh = esd / sndens
!ggg

        ELSE
          EX = ESD/DT
          SNMAX = ESD
          ESD = 0.0

!ggg      snow melt exceeds snow depth
          snowh = 0.0
!ggg

          FLX3 = EX*1000.0*LSUBF
        ENDIF
        PRCP1 = PRCP1 + EX

      ENDIF
         
! ----------------------------------------------------------------------
! SET THE EFFECTIVE POTNL EVAPOTRANSP (ETP1) TO ZERO SINCE SNOW CASE SO
! SURFACE EVAP NOT CALCULATED FROM EDIR, EC, OR ETT IN SMFLX (BELOW).
! IF SEAICE (ICE=1) SKIP CALL TO SMFLX.
! SMFLX RETURNS SOIL MOISTURE VALUES AND PRELIMINARY VALUES OF
! EVAPOTRANSPIRATION.  IN THIS, THE SNOW PACK CASE, THE PRELIM VALUES
! (ETA1) ARE NOT USED IN SUBSEQUENT CALCULATION OF EVAP.
! NEW STATES ADDED: SH2O, AND FROZEN GROUND CORRECTION FACTOR
! EVAP EQUALS POTENTIAL EVAP UNLESS BETA<1.
! ----------------------------------------------------------------------
      ETP1 = 0.0
      IF (ICE .NE. 1) THEN
	if (SH2O(1) .lt. 0) then
	write(0,*) 'call SMFLX with neg SH2O:: ', SH2O(1)
	endif
        CALL SMFLX ( ETA1,SMC,NSOIL,CMC,ETP1,DT,PRCP1,ZSOIL,            &
          SH2O, SLOPE, KDT, FRZFACT,                                    &
          SMCMAX,B,PC,SMCWLT,DKSAT,DWSAT,                               &
          SMCREF,SHDFAC,CMCMAX,SMCDRY,CFACTR,RUNOFF1,RUNOFF2,           &
          RUNOFF3, EDIR1, EC1, ETT1,SFCTMP,Q2,NROOT,RTDIS,              &
          FXEXP,DRIP,EC,EDIR,ETT,RUNOF,I,J)
	if (SH2O(1) .lt. 0) then
	write(0,*) 'return SMFLX with neg SH2O:: ', SH2O(1)
	endif

      ENDIF
      ETA = BETA*ETP

! ----------------------------------------------------------------------
! THE 'ADJUSTED TOP SOIL LYR TEMP' (YY) AND THE 'ADJUSTED SOIL HEAT
! FLUX' (ZZ1) ARE SET TO THE TOP SOIL LYR TEMP, AND 1, RESPECTIVELY.
! THESE ARE CLOSE-ENOUGH APPROXIMATIONS BECAUSE THE SFC HEAT FLUX TO BE
! COMPUTED IN SHFLX WILL EFFECTIVELY BE THE FLUX AT THE SNOW TOP
! SURFACE.  T11 IS A DUMMY ARGUEMENT SINCE WE WILL NOT USE ITS VALUE AS
! REVISED BY SHFLX.
! ----------------------------------------------------------------------
      ZZ1 = 1.0
      YY = STC(1)-0.5*S*ZSOIL(1)*ZZ1/DF1

	if (YY .lt. 200) then
!	write(0,*) 'computed YY= ', YY
!	write(0,*) 'STC(1),S,ZSOIL(1),DF1: ', STC(1),S,ZSOIL(1),DF1
	endif


!
      T11 = T1

! ----------------------------------------------------------------------
! SHFLX WILL CALC/UPDATE THE SOIL TEMPS.  NOTE:  THE SUB-SFC HEAT FLUX 
! (S1) AND THE SKIN TEMP (T11) OUTPUT FROM THIS SHFLX CALL ARE NOT USED 
! IN ANY SUBSEQUENT CALCULATIONS. RATHER, THEY ARE DUMMY VARIABLES HERE 
! IN THE SNOPAC CASE, SINCE THE SKIN TEMP AND SUB-SFC HEAT FLUX ARE 
! UPDATED INSTEAD NEAR THE BEGINNING OF THE CALL TO SNOPAC.
! ----------------------------------------------------------------------

	STCWAS=STC(1)
      CALL SHFLX(S1,STC,SMC,SMCMAX,NSOIL,T11,DT,YY,ZZ1,ZSOIL,TBOT,      &
        ZBOT, SMCWLT, PSISAT, SH2O,                                     &
        B,F1,DF1,ICE,                                                   &
        QUARTZ,CSOIL,I,J)

!	write(0,*) 'SNOWH, STC(1), STCWAS: ', SNOWH, STC(1), STCWAS
	
	if (STC(1) .lt. 230 .and. STCWAS .gt. 230) then
	write(0,*) 'in SNOPAC, SHFLX changed from: ', STCWAS, 'to : ', STC(1)
	write(0,*) 'YY,S,DF1,STC(1): ', YY, S,  &
                                              DF1,STC(1)
	write(0,*) ' '
	endif
      
! ----------------------------------------------------------------------
! SNOW DEPTH AND DENSITY ADJUSTMENT BASED ON SNOW COMPACTION.
! YY is assumed to be the soil temperture at the top of the soil column.
! ----------------------------------------------------------------------
      IF (ESD .GT. 0.) THEN
! --- debug ------------------------------------------------------------
!     write(6,*) 'SNOPAC1:ESD,SNOWH,SNDENS=',ESD,SNOWH,SNDENS
! --- debug ------------------------------------------------------------
        CALL SNOWPACK(ESD,DT,SNOWH,SNDENS,T1,YY)
! --- debug ------------------------------------------------------------
!        SNDENS = 0.2
!        SNOWH = ESD/SNDENS
! --- debug ------------------------------------------------------------
! --- debug ------------------------------------------------------------
!     write(6,*) 'SNOPAC2:ESD,SNOWH,SNDENS=',ESD,SNOWH,SNDENS
! --- debug ------------------------------------------------------------
      ELSE
        ESD = 0.
        SNOWH = 0.
        SNDENS = 0.
        SNCOND = 1.
      ENDIF

! ----------------------------------------------------------------------
      END SUBROUTINE SNOPAC

      SUBROUTINE SNOWPACK ( W,DTS,HC,DS,TSNOW,TSOIL )

      IMPLICIT NONE

! ##############################################################
! ##  SUBROUTINE TO CALCULATE COMPACTION OF SNOWPACK  UNDER  ###
! ##  CONDITIONS OF INCREASING SNOW DENSITY, AS OBTAINED     ###
! FROM AN APPROXIMATE SOLUTION OF E. ANDERSON'S DIFFERENTIAL ###
!     EQUATION (3.29), NOAA TECHNICAL REPORT NWS 19,         ###
!                 BY   VICTOR KOREN   03/25/95               ###
! ##############################################################

! ##############################################################
!  W      IS A WATER EQUIVALENT OF SNOW, IN M                ###
!  DTS    IS A TIME STEP, IN SEC                             ###
!  HC     IS A SNOW DEPTH, IN M                              ###
!  DS     IS A SNOW DENSITY, IN G/CM3                        ###
!  TSNOW  IS A SNOW SURFACE TEMPERATURE, K                   ###
!  TSOIL  IS A SOIL SURFACE TEMPERATURE, K                   ###
!      SUBROUTINE WILL RETURN NEW VALUES OF H AND DS         ###
! ##############################################################

      INTEGER IPOL
      INTEGER J

      REAL C1, C2, HC, W, DTS, DS, TSNOW, TSOIL, H, WX
      REAL DT, TSNOWX, TSOILX, TAVG, B, DSX, DW
      REAL PEXP
      REAL WXX

      PARAMETER (C1=0.01, C2=21.0)

! ##  CONVERSION INTO SIMULATION UNITS   ######################### 

      H=HC*100.
      WX=W*100.
      DT=DTS/3600.
      TSNOWX=TSNOW-273.15
      TSOILX=TSOIL-273.15

! ##  CALCULATING OF AVERAGE TEMPERATURE OF SNOW PACK              ###

      TAVG=0.5*(TSNOWX+TSOILX)                                    

! ##  CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION
!              DS=DS0*(EXP(B*W)-1.)/(B*W)
!              B=DT*C1*EXP(0.08*TAVG-C2*DS0)
! NOTE: B*W IN DS EQN ABOVE HAS TO BE CAREFULLY TREATED 
! NUMERICALLY BELOW
! ##  C1 IS THE FRACTIONAL INCREASE IN DENSITY (1/(CM*HR)) 
! ##  C2 IS A CONSTANT (CM3/G) KOJIMA ESTIMATED AS 21 CMS/G

      IF(WX .GT. 1.E-2) THEN
        WXX = WX
      ELSE
        WXX = 1.E-2
      ENDIF
      B=DT*C1*EXP(0.08*TAVG-C2*DS)

!.........DSX=DS*((DEXP(B*WX)-1.)/(B*WX))
!--------------------------------------------------------------------
!  The function of the form (e**x-1)/x imbedded in above expression
!  for DSX was causing numerical difficulties when the denominator "x"
!  (i.e. B*WX) became zero or approached zero (despite the fact that
!  the analytical function (e**x-1)/x has a well defined limit as 
!  "x" approaches zero), hence below we replace the (e**x-1)/x 
!  expression with an equivalent, numerically well-behaved 
!  polynomial expansion.
! 
!  Number of terms of polynomial expansion, and hence its accuracy, 
!  is governed by iteration limit "ipol".
!       ipol greater than 9 only makes a difference on double
!             precision (relative errors given in percent %).
!        ipol=9, for rel.error <~ 1.6 e-6 % (8 significant digits)
!        ipol=8, for rel.error <~ 1.8 e-5 % (7 significant digits)
!        ipol=7, for rel.error <~ 1.8 e-4 % ...

      ipol = 4
      PEXP = 0.
      do j = ipol,1,-1
!        PEXP = (1. + PEXP)*B*WX/real(j+1) 
        PEXP = (1. + PEXP)*B*WXX/real(j+1) 
      end do 
      PEXP = PEXP + 1.
!
      DSX=DS*(PEXP)
!                     above line ends polynomial substitution

      IF(DSX .GT. 0.40) DSX=0.40
! ----------------------------------------------------------------------
! mbek - April 2001
! Set lower limit on snow density, rather than just previous value.
!         IF(DSX .LT. 0.05) DSX=DS
      IF(DSX .LT. 0.05) DSX=0.05

      DS=DSX

! ##  UPDATE OF SNOW DEPTH AND DENSITY DEPENDING ON LIQUID WATER
! ##  DURING SNOWMELT. ASSUMED THAT 13% OF LIQUID WATER CAN BE STORED
! ##  IN SNOW PER DAY DURING SNOWMELT TILL SNOW DENSITY 0.40

!         IF((TSNOWX .GE. 0.) .AND. (H .NE. 0.)) THEN
      IF (TSNOWX .GE. 0.) THEN
        DW=0.13*DT/24.
        DS=DS*(1.-DW)+DW
        IF(DS .GT. 0.40) DS=0.40
      ENDIF
! ----------------------------------------------------------------------
! Calculate snow depth (cm) from snow water equivalent and snow density.
      H=WX/DS
! ----------------------------------------------------------------------
! Change snow depth units to meters
      HC=H*0.01

      END SUBROUTINE SNOWPACK

      SUBROUTINE SNOW_NEW ( T,P,HC,DS )

      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! CALCULATING SNOW DEPTH AND DENSITITY TO ACCOUNT FOR THE NEW SNOWFALL
! T - AIR TEMPERATURE, K
! P - NEW SNOWFALL, M
! HC - SNOW DEPTH, M
! DS - SNOW DENSITY
! NEW VALUES OF SNOW DEPTH & DENSITY WILL BE RETURNED
      REAL HC
      REAL T 
      REAL P
      REAL DS
      REAL H
      REAL PX
      REAL TX
      REAL DS0
      REAL HNEW
!
      REAL ESD
      
! ----------------------------------------------------------------------
! CONVERSION INTO SIMULATION UNITS      
      H=HC*100.
      PX=P*100.
      TX=T-273.15
      
! ----------------------------------------------------------------------
! CALCULATING NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE
! EQUATION FROM GOTTLIB L. 'A GENERAL RUNOFF MODEL FOR SNOWCOVERED
! AND GLACIERIZED BASIN', 6TH NORDIC HYDROLOGICAL CONFERENCE,
! VEMADOLEN, SWEDEN, 1980, 172-177PP.
!-----------------------------------------------------------------------
      IF(TX .LE. -15.) THEN
        DS0=0.05
      ELSE                                                      
        DS0=0.05+0.0017*(TX+15.)**1.5
      ENDIF
      
! ----------------------------------------------------------------------
! ADJUSTMENT OF SNOW DENSITY DEPENDING ON NEW SNOWFALL      
      HNEW=PX/DS0
      DS=(H*DS+HNEW*DS0)/(H+HNEW)
      H=H+HNEW
      HC=H*0.01
      
! ----------------------------------------------------------------------
      END SUBROUTINE SNOW_NEW

      SUBROUTINE SRT (RHSTT,RUNOFF,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,     &
                       ZSOIL,DWSAT,DKSAT,SMCMAX,B, RUNOFF1,             &
                       RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZX,SICE,AI,BI,CI,  &
                       I,JJJ)


      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY
!C    =======   TERM OF THE SOIL WATER DIFFUSION EQUATION.  ALSO TO
!C              COMPUTE ( PREPARE ) THE MATRIX COEFFICIENTS FOR THE
!C              TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      INTEGER NSOLD
      PARAMETER ( NSOLD = 20 )
!tst      PARAMETER ( NSOLD = 4 )

      INTEGER CVFRZ      
      INTEGER I
      INTEGER IALP1
      INTEGER IOHINF
      INTEGER J
      INTEGER JJ      
      INTEGER JJJ      
      INTEGER K
      INTEGER KS
      INTEGER NSOIL

	REAL AI     ( NSOLD )
      REAL B
      REAL BI     ( NSOLD )
      REAL CI     ( NSOLD )
      REAL DMAX   ( NSOLD )
      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DENOM2
      REAL DKSAT
      REAL DSMDZ
      REAL DSMDZ2
      REAL DWSAT
      REAL EDIR
      REAL ET     ( NSOIL )
      REAL INFMAX
      REAL KDT
      REAL MXSMC
      REAL MXSMC2
      REAL NUMER
      REAL PCPDRP
      REAL PDDUM
      REAL RHSTT  ( NSOIL )
      REAL RUNOFF
      
      REAL SH2O   ( NSOIL )
      REAL SH2OA  ( NSOIL )
      REAL SICE   ( NSOIL )
      REAL SICEMAX
      
      REAL SMCMAX
      REAL WCND
      REAL WCND2
      REAL WDF
      REAL WDF2
      REAL ZSOIL  ( NSOIL )

      REAL RUNOFF1, RUNOFF2, DT, SMCWLT, SLOPE, FRZX, DT1
      REAL SMCAV, DICE, DD, VAL, DDT, PX, FCR, ACRT, SUM
      REAL SSTT, SLOPX

!
! -----------     FROZEN GROUND VERSION    -------------------------
!   REFERENCE FROZEN GROUND PARAMETER, CVFRZ, IS A SHAPE PARAMETER OF
!   AREAL DISTRIBUTION FUNCTION OF SOIL ICE CONTENT WHICH EQUALS 1/CV.
!   CV IS A COEFFICIENT OF SPATIAL VARIATION OF SOIL ICE CONTENT. 
!   BASED ON FIELD DATA CV DEPENDS ON AREAL MEAN OF FROZEN DEPTH, AND IT
!   CLOSE TO CONSTANT = 0.6 IF AREAL MEAN FROZEN DEPTH IS ABOVE 20 CM.
!   THAT IS WHY PARAMETER CVFRZ = 3 (INT{1/0.6*0.6})  
!
!   Current logic doesn't allow CVFRZ be bigger than 3
        PARAMETER ( CVFRZ = 3 )
! ------------------------------------------------------------------
     
!      PRINT*,'in SRT, Declaration -----------------------'
!      PRINT*,'NSOIL=' , NSOIL
!      PRINT*,'NSOLD=' , NSOLD
        
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!
! ##INCLUDE THE INFILTRATION FORMULE FROM SCHAAKE AND KOREN MODEL
!
!C    MODIFIED BY Q DUAN
!C      
      IOHINF=1

! Let SICEMAX be the greatest, if any, frozen water content within 
! soil layers.
      SICEMAX = 0.0
      DO KS=1,NSOIL
       IF (SICE(KS) .GT. SICEMAX) SICEMAX = SICE(KS)
      END DO

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PDDUM = PCPDRP
      RUNOFF1 = 0.0
      IF ( PCPDRP .NE. 0.0 ) THEN

!C++  MODIFIED BY Q. DUAN, 5/16/94

!        IF (IOHINF .EQ. 1) THEN
  
          DT1 = DT/86400.
          SMCAV = SMCMAX - SMCWLT
          DMAX(1)=-ZSOIL(1)*SMCAV

! -----------     FROZEN GROUND VERSION    ------------------------
!
          DICE = -ZSOIL(1) * SICE(1)
!-------------------------------------------------------------------
          
          DMAX(1)=DMAX(1)*(1.0 - (SH2OA(1)+SICE(1)-SMCWLT)/SMCAV)
          DD=DMAX(1)
      DO KS=2,NSOIL
          
! -----------     FROZEN GROUND VERSION    ------------------------
!
           DICE = DICE + ( ZSOIL(KS-1) - ZSOIL(KS) ) * SICE(KS)
!------------------------------------------------------------------- 
         
           DMAX(KS)=(ZSOIL(KS-1)-ZSOIL(KS))*SMCAV
           DMAX(KS)=DMAX(KS)*(1.0 - (SH2OA(KS)+SICE(KS)-SMCWLT)/SMCAV)
           DD=DD+DMAX(KS)
      END DO
!C .....VAL = (1.-EXP(-KDT*SQRT(DT1)))
! IN BELOW, REMOVE THE SQRT IN ABOVE
          VAL = (1.-EXP(-KDT*DT1))
          DDT = DD*VAL
          PX = PCPDRP*DT  
          IF(PX.LT.0.0) PX = 0.0
          INFMAX = (PX*(DDT/(PX+DDT)))/DT
          
! -----------     FROZEN GROUND VERSION    --------------------------
!    REDUCTION OF INFILTRATION BASED ON FROZEN GROUND PARAMETERS
!
         FCR = 1. 
         IF ( DICE .GT. 1.E-2) THEN 
           ACRT = CVFRZ * FRZX / DICE 
           SUM = 1.
           IALP1 = CVFRZ - 1 
           DO J = 1,IALP1
              K = 1
              DO JJ = J+1, IALP1
                K = K * JJ
              END DO   
              SUM = SUM + (ACRT ** ( CVFRZ-J)) / FLOAT (K) 
           END DO 
           FCR = 1. - EXP(-ACRT) * SUM 
         END IF 
         INFMAX = INFMAX * FCR
! -------------------------------------------------------------------

! ############    CORRECTION OF INFILTRATION LIMITATION    ##########
!     IF INFMAX .LE. HYDROLIC CONDUCTIVITY ASSIGN INFMAX THE 
!     VALUE OF HYDROLIC CONDUCTIVITY
!
!         MXSMC = MAX ( SH2OA(1), SH2OA(2) ) 
        MXSMC = SH2OA(1)

!      PRINT*,'SRT, BEFORE WDFCND - 1 ------------------------------'
!      PRINT*,'MXSMC,SMCMAX=' , MXSMC,SMCMAX
!      PRINT*,'B,DKSAT,DWSAT=' , B,DKSAT,DWSAT

      CALL WDFCND ( WDF,WCND,MXSMC,SMCMAX,B,DKSAT,DWSAT,            &
                     SICEMAX )

            INFMAX = MAX(INFMAX, WCND)
            INFMAX= MIN(INFMAX,PX)

!      PRINT*,'SRT, AFTER WDFCND - 1 ------------------------------'
!      PRINT*,'WDF,WCND=' , WDF,WCND
!      PRINT*,'MXSMC,SMCMAX=' , MXSMC,SMCMAX
!      PRINT*,'B,DKSAT,DWSAT=' , B,DKSAT,DWSAT
 
!
          IF ( PCPDRP .GT. INFMAX ) THEN
            RUNOFF1 = PCPDRP - INFMAX
            PDDUM = INFMAX
          END IF

      END IF
!
! TO AVOID SPURIOUS DRAINAGE BEHAVIOR IDENTIFIED BY P. GRUNMANN,
! FORMER APPROACH IN LINE BELOW REPLACED WITH NEW APPROACH IN 2ND LINE
!...MXSMC = MAX( SH2OA(1), SH2OA(2) )
        MXSMC =  SH2OA(1)

!      PRINT*,'SRT, BEFORE WDFCND - 2'
!      PRINT*,'MXSMC,SMCMAX=' , MXSMC,SMCMAX
!      PRINT*,'B,DKSAT,DWSAT=' , B,DKSAT,DWSAT

      CALL WDFCND ( WDF,WCND,MXSMC,SMCMAX,B,DKSAT,DWSAT,          &
      SICEMAX )

!      PRINT*,'SRT, AFTER WDFCND - 2'
!      PRINT*,'WDF,WCND=' , WDF,WCND
!      PRINT*,'MXSMC,SMCMAX=' , MXSMC,SMCMAX
!      PRINT*,'B,DKSAT,DWSAT=' , B,DKSAT,DWSAT
 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DDZ = 1. / ( -.5 * ZSOIL(2) )
      AI(1) = 0.0
      BI(1) = WDF * DDZ / ( -ZSOIL(1) )
      CI(1) = -BI(1)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC RHSTT FOR THE TOP LAYER AFTER CALC'NG THE VERTICAL SOIL
!     MOISTURE GRADIENT BTWN THE TOP AND NEXT TO TOP LAYERS.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DSMDZ = ( SH2O(1) - SH2O(2) ) / ( -.5 * ZSOIL(2) )
      RHSTT(1) = (WDF * DSMDZ + WCND - PDDUM + EDIR + ET(1))/ZSOIL(1)
      SSTT = WDF * DSMDZ + WCND + EDIR + ET(1)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     INITIALIZE DDZ2
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DDZ2 = 0.0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABV PROCESS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO K = 2 , NSOIL
         DENOM2 = ( ZSOIL(K-1) - ZSOIL(K) )
         IF ( K .NE. NSOIL ) THEN
            SLOPX = 1.
!
! AGAIN, TO AVOID SPURIOUS DRAINAGE BEHAVIOR IDENTIFIED BY P. GRUNMANN,
! FORMER APPROACH IN LINE BELOW REPLACED WITH NEW APPROACH IN 2ND LINE
!....MXSMC2 = MAX ( SH2OA(K), SH2OA(K+1) )
            MXSMC2 =  SH2OA(K)

!      PRINT*,'SRT, BEFORE WDFCND - 3'
!      PRINT*,'MXSMC2,SMCMAX=' , MXSMC2,SMCMAX
!      PRINT*,'B,DKSAT,DWSAT=' , B,DKSAT,DWSAT
!      PRINT*,'K=' , K

            CALL WDFCND ( WDF2,WCND2,MXSMC2,SMCMAX,B,DKSAT,DWSAT,   &
                 SICEMAX )

!      PRINT*,'SRT, AFTER WDFCND - 3'
!      PRINT*,'WDF2,WCND2=' , WDF2,WCND2
!      PRINT*,'MXSMC2,SMCMAX=' , MXSMC2,SMCMAX
!      PRINT*,'B,DKSAT,DWSAT=' , B,DKSAT,DWSAT
 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALC SOME PARTIAL PRODUCTS FOR LATER USE IN CALC'NG RHSTT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

            DENOM = ( ZSOIL(K-1) - ZSOIL(K+1) )
            DSMDZ2 = ( SH2O(K) - SH2O(K+1) ) / ( DENOM * 0.5 )

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

            DDZ2 = 2.0 / DENOM
            CI(K) = -WDF2 * DDZ2 / DENOM2
         ELSE

!   SLOPE OF BOTTOM LAYER IS INTRODUCED     ############
!
            SLOPX = SLOPE
!--------------------------------------------------------
          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         RETRIEVE THE SOIL WATER DIFFUSIVITY AND HYDRAULIC
!         CONDUCTIVITY FOR THIS LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


!      PRINT*,'SRT, BEFORE WDFCND - 4'
!      PRINT*,'SH2OA(NSOIL),SMCMAX=' , SH2OA(NSOIL),SMCMAX
!      PRINT*,'B,DKSAT,DWSAT=' , B,DKSAT,DWSAT
!      PRINT*,'K=' , K
 
            CALL WDFCND ( WDF2,WCND2,SH2OA(NSOIL),SMCMAX,           &
                 B,DKSAT,DWSAT,SICEMAX )

!      PRINT*,'SRT, AFTER WDFCND - 4'
!      PRINT*,'WDF2,WCND2=' , WDF2,WCND2
!      PRINT*,'SH2OA(NSOIL),SMCMAX=' , SH2OA(NSOIL),SMCMAX
!      PRINT*,'B,DKSAT,DWSAT=' , B,DKSAT,DWSAT
 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         CALC A PARTIAL PRODUCT FOR LATER USE IN CALC'NG RHSTT 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

            DSMDZ2 = 0.0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!         SET MATRIX COEF CI TO ZERO
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

            CI(K) = 0.0
         END IF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALC RHSTT FOR THIS LAYER AFTER CALC'NG ITS NUMERATOR
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         NUMER = (WDF2 * DSMDZ2) + SLOPX * WCND2 - (WDF * DSMDZ)    &
              - WCND + ET(K)
         RHSTT(K) = NUMER / (-DENOM2)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         AI(K) = -WDF * DDZ / DENOM2
         BI(K) = -( AI(K) + CI(K) )

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       RESET VALUES OF WDF, WCND, DSMDZ, AND DDZ FOR LOOP TO NEXT LYR
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         IF(K.EQ.NSOIL) THEN
!############### RUNOFF2: GROUND WATER RUNOFF ###########
            RUNOFF2 = SLOPX * WCND2
         ENDIF

         IF ( K .NE. NSOIL ) THEN
            WDF = WDF2
            WCND = WCND2
            DSMDZ = DSMDZ2
            DDZ = DDZ2
         END IF
      END DO

!      PRINT*,'SRT, final Runoff'
!      PRINT*,'RUNOFF1=' , RUNOFF1
!      PRINT*,'RUNOFF2=' , RUNOFF2
 
      END SUBROUTINE SRT

      SUBROUTINE SSTEP ( SH2OOUT, SH2OIN, CMC, RHSTT, RHSCT, DT,    &
           NSOIL, SMCMAX, CMCMAX, RUNOFF3, ZSOIL,SMC,SICE,AI,BI,CI,II,J)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE/UPDATE THE SOIL MOISTURE CONTENT VALUES
!C    =======   AND THE CANOPY MOISTURE CONTENT VALUES.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER NSOLD
      PARAMETER ( NSOLD = 20 )
!tst      PARAMETER ( NSOLD = 4 )
!
      INTEGER I
      INTEGER II
      INTEGER J
      INTEGER K 
      INTEGER KK11
      INTEGER NSOIL

      REAL AI     ( NSOLD )
      REAL BI     ( NSOLD )
      REAL CI     ( NSOLD )
      REAL CIin   ( NSOLD )
      REAL CMC
      REAL CMCMAX
      REAL DT
      REAL RHSCT
      REAL RHSTT   ( NSOIL )
      REAL RHSTTin ( NSOIL )
      REAL SH2OIN  ( NSOIL )
      REAL SH2OOUT ( NSOIL )
      REAL SICE    ( NSOIL )
      REAL SMC     ( NSOIL )
      REAL SMCMAX
      REAL ZSOIL(NSOIL)

      REAL RUNOFF3, RUNOFS, WPLUS, DDZ, STOT, WFREE, DPLUS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CREATE 'AMOUNT' VALUES OF VARIABLES TO BE INPUT TO THE
!     TRI-DIAGONAL MATRIX ROUTINE.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO K = 1 , NSOIL
        RHSTT(K) = RHSTT(K) * DT
        AI(K) = AI(K) * DT
        BI(K) = 1. + BI(K) * DT
        CI(K) = CI(K) * DT
      END DO

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO K = 1 , NSOIL
         RHSTTin(K) = RHSTT(K)
      END DO
      DO K = 1 , NSOLD
         CIin(K) = CI(K)
      END DO
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALL ROSR12 TO SOLVE THE TRI-DIAGONAL MATRIX
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CALL ROSR12 ( CI, AI, BI, CIin, RHSTTin, RHSTT, NSOIL,II,J)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     SUM THE PREVIOUS SMC VALUE AND THE MATRIX SOLUTION TO GET A
!     NEW VALUE.  MIN ALLOWABLE VALUE OF SMC WILL BE 0.02.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!   ################## RUNOFF3: Runoff within soil layers #######

      RUNOFS = 0.0
      WPLUS = 0.0
      RUNOFF3 = 0.
      DDZ = - ZSOIL(1)

      DO K = 1 , NSOIL
         IF ( K .NE. 1 ) DDZ = ZSOIL(K - 1) - ZSOIL(K)
         SH2OOUT(K) = SH2OIN(K) + CI(K) + WPLUS / DDZ

	if (SH2OOUT(K) .lt. 0) then
	write(0,*) 'prelim val SH2OIN(K), SH2OOUT(K), CI(K), WPLUS: ', SH2OIN(K), &
                       SH2OOUT(K), CI(K), WPLUS,' i=',ii,' j=',j
	endif
        
!      PRINT*,'IN sstep'
!      PRINT*,'SH2OOUT=', SH2OOUT
        
         STOT = SH2OOUT(K) + SICE(K)
         IF ( STOT .GT. SMCMAX ) THEN
            IF ( K .EQ. 1 ) THEN
               DDZ = -ZSOIL(1)
            ELSE
               KK11 = K - 1
               DDZ = -ZSOIL(K) + ZSOIL(KK11)
            END IF
            WPLUS = ( STOT - SMCMAX ) * DDZ
         ELSE
            WPLUS = 0.
         END IF
         SMC(K) = MAX ( MIN( STOT, SMCMAX ), 0.02 )
         SH2OOUT(K) = MAX ( (SMC(K) - SICE(K)), 0.0 )
      END DO

!  ###  V. KOREN   9/01/98    ######
!     WATER BALANCE CHECKING UPWARD

      IF(WPLUS .GT. 0.) THEN
       DO I=NSOIL-1,1,-1
        IF(I .EQ. 1) THEN
         DDZ=-ZSOIL(1)
        ELSE
         DDZ=-ZSOIL(I)+ZSOIL(I-1)
        ENDIF
        WFREE=(SMCMAX-SH2OOUT(I)-SICE(I))*DDZ
        DPLUS=WFREE-WPLUS
        IF(DPLUS .GE. 0.) THEN
         SH2OOUT(I)=SH2OOUT(I)+WPLUS/DDZ
	if (SH2OOUT(I) .lt. 0) then
	write(0,*) 'SH2OOUT(I) made neg down here(b): ', SH2OOUT(I)
	endif
         SMC(I)=SH2OOUT(I)+SICE(I)
         WPLUS=0.
           
        ELSE
         SH2OOUT(I)=SH2OOUT(I)+WFREE/DDZ
	if (SH2OOUT(I) .lt. 0) then
	write(0,*) 'SH2OOUT(I) made neg down here(c): ',I,WFREE,DDZ,SH2OOUT(I)
	write(0,*) 'SMCMAX, SICE, STOT: ', SMCMAX, SICE(I), STOT
	endif
         SMC(I)=SH2OOUT(I)+SICE(I)
         WPLUS=-DPLUS
        ENDIF
       END DO
30     RUNOFF3=WPLUS
      ENDIF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  UPDATE CANOPY WATER CONTENT/INTERCEPTION (CMC).  CONVERT RHSCT TO 
!  AN 'AMOUNT' VALUE AND ADD TO PREVIOUS CMC VALUE TO GET NEW CMC.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      CMC = CMC + DT * RHSCT
      IF (CMC .LT. 1.E-20) CMC=0.0
      CMC = MIN(CMC,CMCMAX)

      END SUBROUTINE SSTEP

      SUBROUTINE TBND (TU, TB, ZSOIL, ZBOT, K, NSOIL, TBND1)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C   PURPOSE:   CALCULATE TEMPERATURE ON THE BOUNDARY OF THE LAYER
!C   =======    BY INTERPOLATION OF THE MIDDLE LAYER TEMPERATURES
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

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

      PARAMETER (T0=273.15)

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C   USE SURFACE TEMPERATURE ON THE TOP OF THE FIRST LAYER
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      IF(K .EQ. 1) THEN
        ZUP=0.
      ELSE
        ZUP=ZSOIL(K-1)
      ENDIF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C   USE DEPTH OF THE CONSTANT BOTTOM TEMPERATURE WHEN INTERPOLATE
!C   TEMPERATURE INTO THE LAST LAYER BOUNDARY
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      IF(K .EQ. NSOIL) THEN
        ZB=2.*ZBOT-ZSOIL(K)
      ELSE
        ZB=ZSOIL(K+1)
      ENDIF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C   LINEAR INTERPOLATION BETWEEN THE AVERAGE LAYER TEMPERATURES
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      TBND1 = TU+(TB-TU)*(ZUP-ZSOIL(K))/(ZUP-ZB)
      
      END SUBROUTINE TBND

      SUBROUTINE TDFCND ( DF, SMC, Q,  SMCMAX, SH2O,I,J)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE THERMAL DIFFUSIVITY AND CONDUCTIVITY OF
!C    =======   THE SOIL FOR A GIVEN POINT AND TIME.
!C
!C    VERSION:  PETERS-LIDARD APPROACH (PETERS-LIDARD et al., 1998)
!C    =======
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       INTEGER I
       INTEGER J
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
       REAL Q
       REAL SATRATIO
       REAL SH2O
       REAL SMC
       REAL SMCMAX
       REAL XU
       REAL XUNFROZ

!!!!!!!SAVE

! WE NOW GET QUARTZ AS AN INPUT ARGUMENT (SET IN ROUTINE REDPRM):
!        DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52, 
!     &              0.35, 0.60, 0.40, 0.82/

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     IF THE SOIL HAS ANY MOISTURE CONTENT COMPUTE A PARTIAL SUM/PRODUCT
!     OTHERWISE USE A CONSTANT VALUE WHICH WORKS WELL WITH MOST SOILS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  
!
!  THKW ......WATER THERMAL CONDUCTIVITY
!  THKQTZ ....THERMAL CONDUCTIVITY FOR QUARTZ
!  THKO ......THERMAL CONDUCTIVITY FOR OTHER SOIL COMPONENTS
!  THKS ......THERMAL CONDUCTIVITY FOR THE SOLIDS COMBINED(QUARTZ+OTHER)
!  THKICE ....ICE THERMAL CONDUCTIVITY
!  SMCMAX ....POROSITY (= SMCMAX)
!  Q .........QUARTZ CONTENT (SOIL TYPE DEPENDENT)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
! 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!  NEEDS PARAMETERS
! POROSITY(SOIL TYPE):
!      POROS = SMCMAX
! SATURATION RATIO:
      SATRATIO = SMC/SMCMAX
!      print *, 'SATRATIO=',SATRATIO
!     PARAMETERS  W/(M.K)
      THKICE = 2.2
      THKW = 0.57
      THKO = 2.0
!      IF (Q .LE. 0.2) THKO = 3.0
      THKQTZ = 7.7
!  SOLIDS' CONDUCTIVITY      
      THKS = (THKQTZ**Q)*(THKO**(1.- Q))
!  UNFROZEN FRACTION (FROM 1.0, I.E., 100%LIQUID, TO 0.0 (100% FROZEN))
      XUNFROZ=(SH2O + 1.E-9)/(SMC + 1.E-9)
!  UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
      XU=XUNFROZ*SMCMAX 
!  SATURATED THERMAL CONDUCTIVITY
      THKSAT = THKS**(1.-SMCMAX)*THKICE**(SMCMAX-XU)*THKW**(XU)
!  DRY DENSITY IN KG/M3
      GAMMD = (1. - SMCMAX)*2700.
!  DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
      THKDRY = (0.135*GAMMD + 64.7)/(2700. - 0.947*GAMMD)
! RANGE OF VALIDITY FOR THE KERSTEN NUMBER
      IF ( SATRATIO .GT. 0.1 ) THEN

!    KERSTEN NUMBER (FINE FORMULA, AT LEAST 5% OF PARTICLES<(2.E-6)M)
           IF ( (XUNFROZ + 0.0005) .LT. SMC ) THEN
!    FROZEN
              AKE = SATRATIO
           ELSE
!    UNFROZEN
              AKE = LOG10(SATRATIO) + 1.0
           ENDIF

      ELSE
        
! USE K = KDRY
        AKE = 0.0
!        print *, 'AKE (ELSE) = ',AKE
      ENDIF
!  THERMAL CONDUCTIVITY

       DF = AKE*(THKSAT - THKDRY) + THKDRY

      END SUBROUTINE TDFCND

      SUBROUTINE TRANSP (ET,NSOIL,ETP1,SMC,CMC,ZSOIL,SHDFAC,SMCWLT,    &
            CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE TRANSPIRATION FROM THE VEGTYP FOR THIS PT.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER I
      INTEGER K
      INTEGER NSOIL
      INTEGER NROOT

      REAL CFACTR
      REAL CMC
      REAL CMCMAX
      REAL ET    ( NSOIL )
      REAL ETP1
      REAL ETP1A
      REAL GX (7)
!.....REAL PART ( NSOIL )
      REAL PC
      REAL RTDIS ( NSOIL )
      REAL SHDFAC
      REAL SMC   ( NSOIL )
      REAL SMCREF
      REAL SMCWLT
      REAL ZSOIL ( NSOIL )

      REAL SFCTMP, Q2, SGX, DENOM, RTX

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       INITIALIZE  PLANT TRANSP TO ZERO FOR ALL SOIL LAYERS.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO K = 1, NSOIL
         ET(K) = 0.
      END DO

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALC AN 'ADJUSTED' POTNTL TRANSPIRATION
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!cc If statements to avoid TANGENT LINEAR problems near zero
      IF (CMC .NE. 0.0) THEN
      ETP1A = SHDFAC * PC * ETP1 * (1.0 - (CMC /CMCMAX) ** CFACTR)
      ELSE
      ETP1A = SHDFAC * PC * ETP1
      ENDIF
      
      SGX = 0.0
      DO I = 1, NROOT
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
      IF ( DENOM .LE. 0.0) DENOM = 1.
      
      DO I = 1, NROOT
         ET(I) = ETP1A * GX(I) / DENOM
      END DO 

! ABOVE CODE ASSUMES A VERTICALLY UNIFORM ROOT DISTRIBUTION
!
! CODE BELOW TESTS A VARIABLE ROOT DISTRIBUTION
!
!     ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * GX * ETP1A
!        ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * ETP1A
!
! ###  USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
!     ET(1) = RTDIS(1) * ETP1A
!         ET(1) =  ETP1A*PART(1)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     LOOP DOWN THRU THE SOIL LAYERS REPEATING THE OPERATION ABOVE,
!     BUT USING THE THICKNESS OF THE SOIL LAYER (RATHER THAN THE
!     ABSOLUTE DEPTH OF EACH LAYER) IN THE FINAL CALCULATION.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
!     DO 10 K = 2, NROOT
!     GX = ( SMC(K) - SMCWLT ) / ( SMCREF - SMCWLT )
!     GX = MAX ( MIN ( GX, 1. ), 0. )
!     TEST CANOPY RESISTANCE
!     GX = 1.0
!     ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*GX*ETP1A
!       ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*ETP1A
!###  USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
!       ET(K) = RTDIS(K) * ETP1A
!         ET(K) = ETP1A*PART(K)
!     10    CONTINUE
      
      END SUBROUTINE TRANSP

      SUBROUTINE WDFCND ( WDF,WCND,SMC,SMCMAX,B,DKSAT,DWSAT,         &
                               SICEMAX )

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SOIL WATER DIFFUSIVITY AND SOIL
!C    =======   HYDRAULIC CONDUCTIVITY.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      REAL B
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

!      PRINT*,'------------ in WDFCND -------------------------------'
!      PRINT*,'BEFORE WDFCND'
!      PRINT*,'B=',B
!      PRINT*,'DKSAT=',DKSAT
!      PRINT*,'DWSAT=',DWSAT
!      PRINT*,'EXPON=',EXPON
!      PRINT*,'FACTR2=',FACTR2
!      PRINT*,'SMC=',SMC
!      PRINT*,'SMCMAX=',SMCMAX
!      PRINT*,'WCND=',WCND
!      PRINT*,'WDF=',WDF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALC THE RATIO OF THE ACTUAL TO THE MAX PSBL SOIL H2O CONTENT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SMC = SMC
      SMCMAX = SMCMAX
      FACTR1 = 0.2 / SMCMAX
      FACTR2 = SMC / SMCMAX

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PREP AN EXPNTL COEF AND CALC THE SOIL WATER DIFFUSIVITY
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      EXPON = B + 2.0
      WDF = DWSAT * FACTR2 ** EXPON

! FROZEN SOIL HYDRAULIC DIFFUSIVITY.  VERY SENSITIVE TO THE VERTICAL
! GRADIENT OF UNFROZEN WATER. THE LATTER GRADIENT CAN BECOME VERY
! EXTREME IN FREEZING/THAWING SITUATIONS, AND GIVEN THE RELATIVELY 
! FEW AND THICK SOIL LAYERS, THIS GRADIENT SUFFERES SERIOUS 
! TRUNCTION ERRORS YIELDING ERRONEOUSLY HIGH VERTICAL TRANSPORTS OF
! UNFROZEN WATER IN BOTH DIRECTIONS FROM HUGE HYDRAULIC DIFFUSIVITY.  
! THEREFORE, WE FOUND WE HAD TO ARBITRARILY CONSTRAIN WDF 
!
! version D_10cm: ........  FACTR1 = 0.2/SMCMAX
! Weighted approach...................... Pablo Grunmann, 09/28/99.
      IF (SICEMAX .GT. 0.0)  THEN
      VKwgt=1./(1.+(500.*SICEMAX)**3.)
      WDF = VKwgt*WDF + (1.- VKwgt)*DWSAT*FACTR1**EXPON
!      PRINT*,'______________________________________________'
!      PRINT*,'Weighted approach:'
!      PRINT*,'  SICEMAX       VKwgt              Dwgt'
!      PRINT*,SICEMAX,  VKwgt, 1.-VKwgt
!      PRINT*,'______________________________________________'
      ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!     RESET THE EXPNTL COEF AND CALC THE HYDRAULIC CONDUCTIVITY
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      EXPON = ( 2.0 * B ) + 3.0
      WCND = DKSAT * FACTR2 ** EXPON

!      PRINT*,' WDFCND Results --------------------------------'
!      PRINT*,'B=',B
!      PRINT*,'DKSAT=',DKSAT
!      PRINT*,'DWSAT=',DWSAT
!      PRINT*,'EXPON=',EXPON
!      PRINT*,'FACTR2=',FACTR2
!      PRINT*,'SMC=',SMC
!      PRINT*,'SMCMAX=',SMCMAX
!      PRINT*,'WCND=',WCND
!      PRINT*,'WDF=',WDF
!      PRINT*,' SMC         WDF           WCND             B'
!      PRINT*,SMC,WDF,WCND,B

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

      FUNCTION CSNOW ( DSNOW )

      IMPLICIT NONE

      REAL C
      REAL DSNOW
      REAL CSNOW
      REAL UNIT

      PARAMETER ( UNIT=0.11631 ) 
                                         
!   ####  SIMULATION OF TERMAL SNOW CONDUCTIVITY                   
!   ####  SIMULATION UNITS OF CSNOW IS CAL/(CM*HR* C) 
!   ####  AND IT WILL BE RETURND IN W/(M* C)
!   ####  BASIC VERSION IS DYACHKOVA EQUATION                                

! #####   DYACHKOVA EQUATION (1960), FOR RANGE 0.1-0.4

      C=0.328*10**(2.25*DSNOW)
      CSNOW=UNIT*C

! #####    DE VAUX EQUATION (1933), IN RANGE 0.1-0.6
!       CSNOW=0.0293*(1.+100.*DSNOW**2)
      
!     #####   E. ANDERSEN FROM FLERCHINGER
!     CSNOW=0.021+2.51*DSNOW**2        
      
      END FUNCTION CSNOW

      FUNCTION DEVAP ( ETP1, SMC, ZSOIL, SHDFAC, SMCMAX, B,         &
                       DKSAT, DWSAT, SMCDRY, SMCREF, SMCWLT, FXEXP, &
                       I,J)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    NAME:  DIRECT EVAPORATION (DEVAP) FUNCTION  VERSION: N/A
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER I
      INTEGER J
      REAL B
      REAL DEVAP
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

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DEVAP = FX * ( 1.0 - SHDFAC ) * ETP1

      END FUNCTION DEVAP

      FUNCTION FRH2O(TKELV,SMC,SH2O,SMCMAX,B,PSIS)

      IMPLICIT NONE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C  PURPOSE:  CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT
!C  IF TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION
!C  TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF
!C  KOREN ET AL. (1999, JGR, VOL 104(D16), 19569-19585).
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! New version (JUNE 2001): much faster and more accurate newton iteration
! achieved by first taking log of eqn cited above -- less than 4
! (typically 1 or 2) iterations achieves convergence.  Also, explicit
! 1-step solution option for special case of parameter !k=0, which reduces
! the original implicit equation to a simpler explicit form, known as the
! ""Flerchinger Eqn". Improved handling of solution in the limit of
! freezing point temperature T0.
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INPUT:
!
!   TKELV.........Temperature (Kelvin)
!   SMC...........Total soil moisture content (volumetric)
!   SH2O..........Liquid soil moisture content (volumetric)
!   SMCMAX........Saturation soil moisture content (from REDPRM)
!   B.............Soil type "B" parameter (from REDPRM)
!   PSIS..........Saturated soil matric potential (from REDPRM)
!
! OUTPUT:
!   FRH2O.........supercooled liquid water content.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      REAL,INTENT(IN) :: TKELV,SMC,SH2O,SMCMAX,B,PSIS

      REAL,PARAMETER :: CK=8.0,BLIM=5.5,ERROR=0.005,HLICE=3.335E5,      &
                        GS=9.81,DICE=920.0,DH2O=1000.0,T0=273.15

      REAL :: BX,DENOM,DF,DSWL,FK,FRH2O,SWL,SWLK

      INTEGER :: KCOUNT,NLOG

!  ###   LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)  ####
!  ###   SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT  ####
!  ###   IS NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES    ####
!##################################################################
!
      BX = B
      IF ( B .GT. BLIM ) BX = BLIM
!------------------------------------------------------------------

! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
      NLOG=0
      KCOUNT=0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (TKELV .GT. (T0 - 1.E-3)) THEN

        FRH2O=SMC

      ELSE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IF (CK .NE. 0.0) THEN

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCC OPTION 1: ITERATED SOLUTION FOR NONZERO CK CCCCCCCCCCC
!CCCCCCCCCCC IN KOREN ET AL, JGR, 1999, EQN 17 CCCCCCCCCCCCCCCCC
!
! INITIAL GUESS FOR SWL (frozen content)
        SWL = SMC-SH2O
! KEEP WITHIN BOUNDS.
         IF (SWL .GT. (SMC-0.02)) SWL=SMC-0.02
         IF(SWL .LT. 0.) SWL=0.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  START OF ITERATIONS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        DO WHILE (NLOG .LT. 10 .AND. KCOUNT .EQ. 0)
         NLOG = NLOG+1
         DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) *   &
              ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
         DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
         SWLK = SWL - DF/DENOM
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
         IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
         IF(SWLK .LT. 0.) SWLK = 0.
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
         DSWL=ABS(SWLK-SWL)
         SWL=SWLK
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
!C WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         IF ( DSWL .LE. ERROR )  THEN
           KCOUNT=KCOUNT+1
         END IF
        END DO
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  END OF ITERATIONS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
        FRH2O = SMC - SWL
!
!CCCCCCCCCCCCCCCCCCCCCCC END OPTION 1 CCCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

       IF (KCOUNT .EQ. 0) THEN

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCC OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0 CCCCCCCC
!CCCCCCCCCCCC IN KOREN ET AL., JGR, 1999, EQN 17  CCCCCCCCCCCCCCC
!
        FK=(((HLICE/(GS*(-PSIS)))*((TKELV-T0)/TKELV))**(-1/BX))*SMCMAX
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
        IF (FK .LT. 0.02) FK = 0.02
        FRH2O = MIN ( FK, SMC )
!
!CCCCCCCCCCCCCCCCCCCCCCCC END OPTION 2 CCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

      ENDIF

      END FUNCTION FRH2O

      FUNCTION SNKSRC ( TUP,TM,TDN, SMC, SH2O, ZSOIL,NSOIL,  &
           SMCMAX, PSISAT, B, DT, K, QTOT) 
      
      IMPLICIT NONE
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C    PURPOSE:  TO CALCULATE SINK/SOURCE TERM OF THE TERMAL DIFFUSION
!C    =======   EQUATION. (SH2O) IS AVAILABLE LIQUED WATER.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER  K
      INTEGER  NSOIL
      
      REAL B
      REAL DF
      REAL DFH2O
      REAL DFICE
      REAL DH2O
      REAL DT
      REAL DZ
      REAL DZH
      REAL FREE
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

      PARAMETER (HLICE=3.3350E5)
      PARAMETER (DH2O =1.0000E3)
      PARAMETER (  T0 =2.7315E2)
      
      IF(K.EQ.1) THEN
        DZ=-ZSOIL(1)
      ELSE
        DZ=ZSOIL(K-1)-ZSOIL(K)
      ENDIF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALCULATE POTENTIAL REDUCTION OF LIQUED WATER CONTENT
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      XH2O=QTOT*DT/(DH2O*HLICE*DZ) + SH2O
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ESTIMATE UNFROZEN WATER AT TEMPERATURE TAVG,
!     AND CHECK IF CALCULATED WATER CONTENT IS REASONABLE 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
        
!  ####   NEW CALCULATION OF AVERAGE TEMPERATURE (TAVG)   ##########
!  ####   IN FREEZING/THAWING LAYER USING UP, DOWN, AND MIDDLE   ###
!  ####   LAYER TEMPERATURES (TUP, TDN, TM)               ##########
   
      DZH=DZ*0.5

      IF (TUP .LT. T0) THEN

        IF (TM .LT. T0) THEN

          IF (TDN .LT. T0) THEN

!           *** TUP, TM, TDN < T0 ***

            TAVG = (TUP + 2.0*TM + TDN)/ 4.0
            
          ELSE

!           *** TUP & TM < T0,  TDN >= T0 ***

            X0 = (T0 - TM) * DZH / (TDN - TM)
            TAVG = 0.5 * (TUP*DZH+TM*(DZH+X0)+T0*(2.*DZH-X0)) / DZ
                       
          ENDIF      

        ELSE
        
          IF (TDN .LT. T0) THEN

!           *** TUP < T0, TM >= T0, TDN < T0 ***

            XUP  = (T0-TUP) * DZH / (TM-TUP)
            XDN  = DZH - (T0-TM) * DZH / (TDN-TM)
            TAVG = 0.5 * (TUP*XUP+T0*(2.*DZ-XUP-XDN)+TDN*XDN) / DZ

          ELSE

!           *** TUP < T0, TM >= T0, TDN >= T0 ***

            XUP  = (T0-TUP) * DZH / (TM-TUP)
            TAVG = 0.5 * (TUP*XUP+T0*(2.*DZ-XUP)) / DZ
                      
          ENDIF   
        
        ENDIF

      ELSE

        IF (TM .LT. T0) THEN

          IF (TDN .LT. T0) THEN

!           *** TUP >= T0, TM < T0, TDN < T0 ***

            XUP  = DZH - (T0-TUP) * DZH / (TM-TUP)
            TAVG = 0.5 * (T0*(DZ-XUP)+TM*(DZH+XUP)+TDN*DZH) / DZ
                      
          ELSE

!           *** TUP >= T0, TM < T0, TDN >= T0 ***

            XUP  = DZH - (T0-TUP) * DZH / (TM-TUP)
            XDN  = (T0-TM) * DZH / (TDN-TM)
            TAVG = 0.5 * (T0*(2.*DZ-XUP-XDN)+TM*(XUP+XDN)) / DZ
                                   
          ENDIF   

        ELSE

          IF (TDN .LT. T0) THEN

!           *** TUP >= T0, TM >= T0, TDN < T0 ***

            XDN  = DZH - (T0-TM) * DZH / (TDN-TM)
            TAVG = (T0*(DZ-XDN)+0.5*(T0+TDN)*XDN) / DZ
                 
          ELSE

!           *** TUP >= T0, TM >= T0, TDN >= T0 ***

            TAVG = (TUP + 2.0*TM + TDN) / 4.0
                      
          ENDIF           

        ENDIF

      ENDIF                      

      FREE=FRH2O(TAVG, SMC, SH2O, SMCMAX, B, PSISAT )

      IF ( XH2O .LT. SH2O .AND. XH2O .LT. FREE) THEN 
         IF ( FREE .GT. SH2O ) THEN
              XH2O = SH2O
          ELSE
              XH2O = FREE
          ENDIF
      ENDIF
              
      IF ( XH2O .GT. SH2O .AND. XH2O .GT. FREE )  THEN
         IF ( FREE .LT. SH2O ) THEN
              XH2O = SH2O
          ELSE
              XH2O = FREE
          ENDIF
      ENDIF 

      IF(XH2O .LT. 0. ) XH2O=0.
      IF(XH2O .GT. SMC) XH2O=SMC

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALCULATE SINK/SOURCE TERM AND REPLACE PREVIOUS WATER CONTENT 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
     
      SNKSRC=-DH2O*HLICE*DZ*(XH2O-SH2O)/DT

      SH2O=XH2O
      
      END FUNCTION SNKSRC

END MODULE module_sf_lsm_nmm

