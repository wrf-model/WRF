!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_slab

   !---SPECIFY CONSTANTS AND LAYERS FOR SOIL MODEL
   !---SOIL DIFFUSION CONSTANT SET (M^2/S)

   REAL, PARAMETER :: DIFSL=5.e-7

   !---FACTOR TO MAKE SOIL STEP MORE CONSERVATIVE

   REAL , PARAMETER :: SOILFAC=1.25

CONTAINS

!----------------------------------------------------------------
   SUBROUTINE SLAB(T3D,QV3D,P3D,FLHC,FLQC,                      &
                   PSFC,XLAND,TMN,HFX,QFX,LH,TSK,QSFC,CHKLOWQ,  &
                   GSW,GLW,CAPG,THC,SNOWC,EMISS,MAVAIL,         &
                   DELTSM,ROVCP,XLV,DTMIN,IFSNOW,               &
                   SVP1,SVP2,SVP3,SVPT0,EP2,                    &
                   KARMAN,EOMEG,STBOLT,                         &
                   TSLB,ZS,DZS,num_soil_layers,radiation,       &
                   P1000mb,                                     &
                   ids,ide, jds,jde, kds,kde,                   &
                   ims,ime, jms,jme, kms,kme,                   &
                   its,ite, jts,jte, kts,kte                    )
!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------
!                                                                        
!     SUBROUTINE SLAB CALCULATES THE GROUND TEMPERATURE TENDENCY 
!     ACCORDING TO THE RESIDUAL OF THE SURFACE ENERGY BUDGET           
!     (BLACKADAR, 1978B).                                              
!                                                                      
!     CHANGES:                                                         
!          FOR SOIL SUB-TIMESTEPS UPDATE SURFACE HFX AND QFX AS TG     
!          CHANGES TO PREVENT POSSIBLE INSTABILITY FOR LONG MODEL      
!          STEPS (DT > ~200 SEC).                                      
!                                                                      
!          PUT SNOW COVER CHECK ON SOIL SUB-TIMESTEPS                  
!                                                                      
!          MAKE UPPER LIMIT ON SOIL SUB-STEP LENGTH MORE CONSERVATIVE  
!                                                                      
!----------------------------------------------------------------          
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- FLHC        exchange coefficient for heat (m/s)
!-- FLQC        exchange coefficient for moisture (m/s)
!-- PSFC        surface pressure (Pa)
!-- XLAND       land mask (1 for land, 2 for water)
!-- TMN         soil temperature at lower boundary (K)
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- LH          latent heat flux at the surface (W/m^2)
!-- TSK         surface temperature (K)
!-- GSW         downward short wave flux at ground surface (W/m^2)      
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- CAPG        heat capacity for soil (J/K/m^3)
!-- THC         thermal inertia (Cal/cm/K/s^0.5)
!-- SNOWC       flag indicating snow coverage (1 for snow cover)
!-- EMISS       surface emissivity (between 0 and 1)
!-- DELTSM      time step (second)
!-- ROVCP       R/CP
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
!-- TSLB        soil temperature in 5-layer model
!-- ZS          depths of centers of soil layers
!-- DZS         thicknesses of soil layers
!-- num_soil_layers   the number of soil layers
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
!----------------------------------------------------------------
   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN)       ::     num_soil_layers
   LOGICAL, INTENT(IN)       ::     radiation

   INTEGER,  INTENT(IN   )   ::     IFSNOW

!
   REAL,     INTENT(IN   )   ::     DTMIN,XLV,ROVCP,DELTSM

   REAL,     INTENT(IN )     ::     SVP1,SVP2,SVP3,SVPT0
   REAL,     INTENT(IN )     ::     EP2,KARMAN,EOMEG,STBOLT
   REAL,     INTENT(IN )     ::     P1000mb

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(INOUT)   :: TSLB

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::ZS,DZS

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
            INTENT(IN   )    ::                           QV3D, &
                                                           P3D, &
                                                           T3D
!
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                          SNOWC, &
                                                         XLAND, &
                                                         EMISS, &
                                                        MAVAIL, &
                                                           TMN, &
                                                           GSW, &
                                                           GLW, &
                                                           THC

!CHKLOWQ is declared as memory size
!
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            HFX, &
                                                           QFX, &
                                                            LH, &
                                                          CAPG, &
                                                           TSK, &
                                                          QSFC, &
                                                       CHKLOWQ

   REAL,     DIMENSION( ims:ime, jms:jme )                    , &
             INTENT(IN   )               ::               PSFC
!
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::     &
                                                          FLHC, &
                                                          FLQC

! LOCAL VARS

   REAL,     DIMENSION( its:ite ) ::                      QV1D, &
                                                           P1D, &
                                                           T1D
   INTEGER ::  I,J

   DO J=jts,jte

      DO i=its,ite
         T1D(i) =T3D(i,1,j)
         QV1D(i)=QV3D(i,1,j)
         P1D(i) =P3D(i,1,j)
      ENDDO

! the indices to the PSFC argument in the following call look
! wrong; however, it is correct to call with its (and not ims)
! because of the way PSFC is defined in SLAB1D. Whether *that*
! is a good idea or not, this commenter cannot comment. JM

      CALL SLAB1D(J,T1D,QV1D,P1D,FLHC(ims,j),FLQC(ims,j),       &
           PSFC(its,j),XLAND(ims,j),TMN(ims,j),HFX(ims,j),      &
           QFX(ims,j),TSK(ims,j),QSFC(ims,j),CHKLOWQ(ims,j),    &
           LH(ims,j),GSW(ims,j),GLW(ims,j),                     &
           CAPG(ims,j),THC(ims,j),SNOWC(ims,j),EMISS(ims,j),    &
           MAVAIL(ims,j),DELTSM,ROVCP,XLV,DTMIN,IFSNOW,         &
           SVP1,SVP2,SVP3,SVPT0,EP2,KARMAN,EOMEG,STBOLT,        &
           TSLB(ims,1,j),ZS,DZS,num_soil_layers,radiation,      &
           P1000mb,                                             &
           ids,ide, jds,jde, kds,kde,                           &
           ims,ime, jms,jme, kms,kme,                           &
           its,ite, jts,jte, kts,kte                            )

   ENDDO

   END SUBROUTINE SLAB

!----------------------------------------------------------------
   SUBROUTINE SLAB1D(J,T1D,QV1D,P1D,FLHC,FLQC,                  &
                   PSFCPA,XLAND,TMN,HFX,QFX,TSK,QSFC,CHKLOWQ,   &
                   LH,GSW,GLW,CAPG,THC,SNOWC,EMISS,MAVAIL,      &
                   DELTSM,ROVCP,XLV,DTMIN,IFSNOW,               &
                   SVP1,SVP2,SVP3,SVPT0,EP2,                    &
                   KARMAN,EOMEG,STBOLT,                         &
                   TSLB2D,ZS,DZS,num_soil_layers,radiation,     &
                   P1000mb,                                     &
                   ids,ide, jds,jde, kds,kde,                   &
                   ims,ime, jms,jme, kms,kme,                   &
                   its,ite, jts,jte, kts,kte                    )
!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------
!                                                                        
!     SUBROUTINE SLAB CALCULATES THE GROUND TEMPERATURE TENDENCY 
!     ACCORDING TO THE RESIDUAL OF THE SURFACE ENERGY BUDGET           
!     (BLACKADAR, 1978B).                                              
!                                                                      
!     CHANGES:                                                         
!          FOR SOIL SUB-TIMESTEPS UPDATE SURFACE HFX AND QFX AS TG     
!          CHANGES TO PREVENT POSSIBLE INSTABILITY FOR LONG MODEL      
!          STEPS (DT > ~200 SEC).                                      
!                                                                      
!          PUT SNOW COVER CHECK ON SOIL SUB-TIMESTEPS                  
!                                                                      
!          MAKE UPPER LIMIT ON SOIL SUB-STEP LENGTH MORE CONSERVATIVE  
!                                                                      
!----------------------------------------------------------------          

   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte,J 

   INTEGER , INTENT(IN)      ::     num_soil_layers
   LOGICAL,  INTENT(IN   )   ::     radiation

   INTEGER,  INTENT(IN   )   ::     IFSNOW
!
   REAL,     INTENT(IN   )   ::     DTMIN,XLV,ROVCP,DELTSM

   REAL,     INTENT(IN )     ::     SVP1,SVP2,SVP3,SVPT0
   REAL,     INTENT(IN )     ::     EP2,KARMAN,EOMEG,STBOLT
   REAL,     INTENT(IN )     ::     P1000mb

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers ),          &
             INTENT(INOUT)   :: TSLB2D

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::ZS,DZS

!
   REAL,    DIMENSION( ims:ime )                              , &
            INTENT(INOUT)    ::                            HFX, &
                                                           QFX, &
                                                            LH, &
                                                          CAPG, &
                                                           TSK, &
                                                          QSFC, &
                                                       CHKLOWQ
!
   REAL,    DIMENSION( ims:ime )                              , &
            INTENT(IN   )    ::                          SNOWC, &
                                                         XLAND, &
                                                         EMISS, &
                                                        MAVAIL, &
                                                           TMN, &
                                                           GSW, &
                                                           GLW, &
                                                           THC
!
   REAL,    DIMENSION( its:ite )                              , &
            INTENT(IN   )    ::                           QV1D, &
                                                           P1D, &
                                                           T1D
!
   REAL,     DIMENSION( its:ite )                             , &
             INTENT(IN   )               ::             PSFCPA

!
   REAL,    DIMENSION( ims:ime ), INTENT(INOUT) ::              &
                                                          FLHC, &
                                                          FLQC
! LOCAL VARS

   REAL,    DIMENSION( its:ite )          ::              PSFC

   REAL,    DIMENSION( its:ite )          ::                    &
                                                           THX, &
                                                            QX, &
                                                          SCR3 

   REAL,    DIMENSION( its:ite )          ::            DTHGDT, &
                                                           TG0, &
                                                         THTMN, &
                                                          XLD1, &
                                                         TSCVN, &
                                                          OLTG, &
                                                        UPFLUX, &
                                                            HM, &
                                                          RNET, &
                                                         XINET, &
                                                            QS, &
                                                         DTSDT
!
   REAL, DIMENSION( its:ite, num_soil_layers )        :: FLUX
!
   INTEGER :: I,K,NSOIL,ITSOIL,L,NK,RADSWTCH
   REAL    :: PS,PS1,XLDCOL,TSKX,RNSOIL,RHOG1,RHOG2,RHOG3,LAMDAG
   REAL    :: THG,ESG,QSG,HFXT,QFXT,CS,CSW,LAMG(4),THCON,PL
 
!----------------------------------------------------------------------          
!-----DETERMINE IF ANY POINTS IN COLUMN ARE LAND (RATHER THAN OCEAN)             
!       POINTS.  IF NOT, SKIP DOWN TO THE PRINT STATEMENTS SINCE OCEAN           
!       SURFACE TEMPERATURES ARE NOT ALLOWED TO CHANGE.                          
!                                                                                
! from sfcrad   
!----------------------------------------------------------------------
   DATA CSW/4.183E6/
   DATA LAMG/1.407E-8, -1.455E-5, 6.290E-3, 0.16857/

   DO i=its,ite
! in cmb
      PSFC(I)=PSFCPA(I)/1000.
   ENDDO


      DO I=its,ite
! PL cmb
         PL=P1D(I)/1000.
         SCR3(I)=T1D(I)
!         THCON=(100./PL)**ROVCP
         THCON=(P1000mb*0.001/PL)**ROVCP
         THX(I)=SCR3(I)*THCON
         QX(I)=0.
      ENDDO

!     IF(IDRY.EQ.1) GOTO 81
      DO I=its,ite
         QX(I)=QV1D(I)
      ENDDO
   81 CONTINUE

!
!-----THE SLAB THERMAL CAPACITY CAPG(I) ARE DEPENDENT ON:
!     THC(I) - SOIL THERMAL INERTIAL, ONLY.
!
      DO I=its,ite
         CAPG(I)=3.298E6*THC(I)
         IF(num_soil_layers .gt. 1)THEN

! CAPG REPRESENTS SOIL HEAT CAPACITY (J/K/M^3) WHEN DIFSL=5.E-7 (M^2/S)
! TO GIVE A CORRECT THERMAL INERTIA (=CAPG*DIFSL^0.5)

            CAPG(I)=5.9114E7*THC(I)
         ENDIF
      ENDDO
!        
      XLDCOL=2.0                                                                 
      DO 10 I=its,ite
        XLDCOL=AMIN1(XLDCOL,XLAND(I))                                          
   10 CONTINUE                                                                   
!                                                                                
      IF(XLDCOL.GT.1.5)GOTO 90                                                   
!                                                                                
!                                                                                
!-----CONVERT SLAB TEMPERATURE TO POTENTIAL TEMPERATURE AND                      
!     SET XLD1(I) = 0. FOR OCEAN POINTS:                                         
!                                                                                
!                                                                                
      DO 20 I=its,ite
        IF((XLAND(I)-1.5).GE.0)THEN                                            
          XLD1(I)=0.                                                             
        ELSE                                                                     
          XLD1(I)=1.                                                             
        ENDIF                                                                    
   20 CONTINUE                                                                   
!                                                                                
!-----CONVERT 'TSK(THETAG)' TO 'TG' FOR 'IUP' CALCULATION ....                   
!       IF WE ARE USING THE BLACKADAR MULTI-LEVEL (HIGH-RESOLUTION)              
!       PBL MODEL                                                                
!                                                                                
      DO 50 I=its,ite
        IF(XLD1(I).LT.0.5)GOTO 50                                                

! PS cmb
        PS=PSFC(I)

! TSK is Temperature at gound sfc
!       TG0(I)=TSK(I)*(PS*0.01)**ROVCP                                         
        TG0(I)=TSK(I)
   50 CONTINUE                                                                   
!                                                                                
!-----COMPUTE THE SURFACE ENERGY BUDGET:                                         
!                                                                                
!     IF(ISOIL.EQ.1)NSOIL=1                                                      
      IF(num_soil_layers .gt. 1)NSOIL=1                                                      


      IF (radiation) then
        RADSWTCH=1
      ELSE
        RADSWTCH=0
      ENDIF

      DO 70 I=its,ite
        IF(XLD1(I).LT.0.5)GOTO 70
!        OLTG(I)=TSK(I)*(100./PSFC(I))**ROVCP
        OLTG(I)=TSK(I)*(P1000mb*0.001/PSFC(I))**ROVCP
        UPFLUX(I)=RADSWTCH*STBOLT*TG0(I)**4                            
        XINET(I)=EMISS(I)*(GLW(I)-UPFLUX(I))    
        RNET(I)=GSW(I)+XINET(I)                                                
        HM(I)=1.18*EOMEG*(TG0(I)-TMN(I))                                       
!       MOISTURE FLUX CALCULATED HERE (OVERWRITES SFC LAYER VALUE FOR LAND)
                ESG=SVP1*EXP(SVP2*(TG0(I)-SVPT0)/(TG0(I)-SVP3))
                QSG=EP2*ESG/(PSFC(I)-ESG)
                THG=TSK(I)*(100./PSFC(I))**ROVCP
                HFX(I)=FLHC(I)*(THG-THX(I))
                QFX(I)=FLQC(I)*(QSG-QX(I))
                LH(I)=QFX(I)*XLV
        QS(I)=HFX(I)+QFX(I)*XLV                                
!       IF(ISOIL.EQ.0)THEN                                                       
        IF(num_soil_layers .EQ. 1)THEN                                                       
          DTHGDT(I)=(RNET(I)-QS(I))/CAPG(I)-HM(I)                              
        ELSE
          DTHGDT(I)=0.                                                           
        ENDIF                                                                    
   70 CONTINUE                                                                   
!     IF(ISOIL.EQ.1)THEN                                                         
      IF(num_soil_layers .gt. 1)THEN                                                         
        NSOIL=1+IFIX(SOILFAC*4*DIFSL/DZS(1)*DELTSM/DZS(1))   
        RNSOIL=1./FLOAT(NSOIL)                                                   
!                                                                                
!     SOIL SUB-TIMESTEP                                                          
!                                                                                
        DO ITSOIL=1,NSOIL                                                        
          DO I=its,ite
             DO L=1,num_soil_layers-1
              IF(XLD1(I).LT.0.5)GOTO 75                                          
              IF(L.EQ.1.AND.ITSOIL.GT.1)THEN                                     
!                PS1=(PSFC(I)*0.01)**ROVCP    
                PS1=(PSFCPA(I)/P1000mb)**ROVCP    

! for rk scheme A and B are the same
                PS=PSFC(I)
                THG=TSLB2D(I,1)/PS1                                              
                ESG=SVP1*EXP(SVP2*(TSLB2D(I,1)-SVPT0)/(TSLB2D(I,1) & 
                    -SVP3))                                                      
                QSG=EP2*ESG/(PS-ESG)                                             
!     UPDATE FLUXES FOR NEW GROUND TEMPERATURE                                   
                HFXT=FLHC(I)*(THG-THX(I))                                     
                QFXT=FLQC(I)*(QSG-QX(I))
                QS(I)=HFXT+QFXT*XLV                                
!     SUM HFX AND QFX OVER SOIL TIMESTEPS                                        
                HFX(I)=HFX(I)+HFXT                                           
                QFX(I)=QFX(I)+QFXT                                           
              ENDIF                                                              
              FLUX(I,1)=RNET(I)-QS(I)                                            
              FLUX(I,L+1)=-DIFSL*CAPG(I)*(TSLB2D(I,L+1)-TSLB2D(I,L))/( & 
                          ZS(L+1)-ZS(L))                                         
              DTSDT(I)=-(FLUX(I,L+1)-FLUX(I,L))/(DZS(L)*CAPG(I))               
              TSLB2D(I,L)=TSLB2D(I,L)+DTSDT(I)*DELTSM*RNSOIL                     
              IF(IFSNOW.EQ.1.AND.L.EQ.1)THEN                              
                IF((SNOWC(I).GT.0..AND.TSLB2D(I,1).GT.273.16))THEN             
                  TSLB2D(I,1)=273.16                                             
                ENDIF                                                            
              ENDIF                                                              
              IF(L.EQ.1)DTHGDT(I)=DTHGDT(I)+RNSOIL*DTSDT(I)                      
              IF(ITSOIL.EQ.NSOIL.AND.L.EQ.1)THEN                                 
!     AVERAGE HFX AND QFX OVER SOIL TIMESTEPS FOR OUTPUT TO PBL                  
                HFX(I)=HFX(I)*RNSOIL                                         
                QFX(I)=QFX(I)*RNSOIL                                         
                LH(I)=QFX(I)*XLV
              ENDIF                                                              
   75         CONTINUE                                                           
            ENDDO                                                                
          ENDDO                                                                  
        ENDDO                                                                    
      ENDIF                                                                      
!                                                                                
      DO 80 I=its,ite
        IF(XLD1(I).LT.0.5) GOTO 80                                                
        TSKX=TG0(I)+DELTSM*DTHGDT(I)                                             

! TSK is temperature
!       TSK(I)=TSKX*(100./PS1)**ROVCP                                          
        TSK(I)=TSKX
   80 CONTINUE                                                                   

!                                                                                
!-----MODIFY THE THE GROUND TEMPERATURE IF THE SNOW COVER EFFECTS ARE            
!     CONSIDERED: LIMIT THE GROUND TEMPERATURE UNDER 0 C.                        
!                                                                                
      IF(IFSNOW.EQ.0)GOTO 90                                              
      DO 85 I=its,ite
        IF(XLD1(I).LT.0.5)GOTO 85                                                
!       PS1=(PSFC(I)*0.01)**ROVCP             
!       TSCVN(I)=TSK(I)*PS1                                            
        TSCVN(I)=TSK(I)
        IF((SNOWC(I).GT.0..AND.TSCVN(I).GT.273.16))THEN                        
          TSCVN(I)=273.16                                                        
        ELSE                                                                     
          TSCVN(I)=TSCVN(I)                                                      
        ENDIF                                                                    
!       TSK(I)=TSCVN(I)/PS1                                                    
        TSK(I)=TSCVN(I)
   85 CONTINUE                                                                   
!                                                                                
   90 CONTINUE                                                                   
      DO I=its,ite
! QSFC and CHKLOWQ needed by Eta PBL
! WA added check for flqc = 0 to accomodate TEMF (and others?)
        if ( FLQC(I) .ne. 0.) then
           QSFC(I)=QX(I)+QFX(I)/FLQC(I)
        else
           QSFC(I) = QX(I)
        end if
        CHKLOWQ(I)=MAVAIL(I)
      ENDDO
!                                                                                
  140 CONTINUE                                                                   

   END SUBROUTINE SLAB1D

!================================================================
   SUBROUTINE slabinit(TSK,TMN,                                 &
                       TSLB,ZS,DZS,num_soil_layers,             &
                       allowed_to_read, start_of_simulation,    &
                       ids,ide, jds,jde, kds,kde,               &
                       ims,ime, jms,jme, kms,kme,               &
                       its,ite, jts,jte, kts,kte                )
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
   LOGICAL , INTENT(IN)      ::      allowed_to_read
   LOGICAL , INTENT(IN)      ::      start_of_simulation
   INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN   )    ::      num_soil_layers
!   
   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ), INTENT(INOUT) :: TSLB

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)  ::  ZS,DZS

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN)    ::                               TSK, &
                                                           TMN

!  LOCAR VAR

   INTEGER                   ::      L,J,I,itf,jtf
   CHARACTER*1024 message

!----------------------------------------------------------------
 
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   END SUBROUTINE slabinit
!-------------------------------------------------------------------          

END MODULE module_sf_slab
