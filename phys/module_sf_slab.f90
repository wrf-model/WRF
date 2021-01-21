

MODULE module_sf_slab

   
   

   REAL, PARAMETER :: DIFSL=5.e-7

   

   REAL , PARAMETER :: SOILFAC=1.25

CONTAINS


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

    IMPLICIT NONE








































































   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN)       ::     num_soil_layers
   LOGICAL, INTENT(IN)       ::     radiation

   INTEGER,  INTENT(IN   )   ::     IFSNOW


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

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                          SNOWC, &
                                                         XLAND, &
                                                         EMISS, &
                                                        MAVAIL, &
                                                           TMN, &
                                                           GSW, &
                                                           GLW, &
                                                           THC



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

   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::     &
                                                          FLHC, &
                                                          FLQC



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

    IMPLICIT NONE

















   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte,J 

   INTEGER , INTENT(IN)      ::     num_soil_layers
   LOGICAL,  INTENT(IN   )   ::     radiation

   INTEGER,  INTENT(IN   )   ::     IFSNOW

   REAL,     INTENT(IN   )   ::     DTMIN,XLV,ROVCP,DELTSM

   REAL,     INTENT(IN )     ::     SVP1,SVP2,SVP3,SVPT0
   REAL,     INTENT(IN )     ::     EP2,KARMAN,EOMEG,STBOLT
   REAL,     INTENT(IN )     ::     P1000mb

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers ),          &
             INTENT(INOUT)   :: TSLB2D

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::ZS,DZS


   REAL,    DIMENSION( ims:ime )                              , &
            INTENT(INOUT)    ::                            HFX, &
                                                           QFX, &
                                                            LH, &
                                                          CAPG, &
                                                           TSK, &
                                                          QSFC, &
                                                       CHKLOWQ

   REAL,    DIMENSION( ims:ime )                              , &
            INTENT(IN   )    ::                          SNOWC, &
                                                         XLAND, &
                                                         EMISS, &
                                                        MAVAIL, &
                                                           TMN, &
                                                           GSW, &
                                                           GLW, &
                                                           THC

   REAL,    DIMENSION( its:ite )                              , &
            INTENT(IN   )    ::                           QV1D, &
                                                           P1D, &
                                                           T1D

   REAL,     DIMENSION( its:ite )                             , &
             INTENT(IN   )               ::             PSFCPA


   REAL,    DIMENSION( ims:ime ), INTENT(INOUT) ::              &
                                                          FLHC, &
                                                          FLQC


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

   REAL, DIMENSION( its:ite, num_soil_layers )        :: FLUX

   INTEGER :: I,K,NSOIL,ITSOIL,L,NK,RADSWTCH
   REAL    :: PS,PS1,XLDCOL,TSKX,RNSOIL,RHOG1,RHOG2,RHOG3,LAMDAG
   REAL    :: THG,ESG,QSG,HFXT,QFXT,CS,CSW,LAMG(4),THCON,PL
 







   DATA CSW/4.183E6/
   DATA LAMG/1.407E-8, -1.455E-5, 6.290E-3, 0.16857/

   DO i=its,ite

      PSFC(I)=PSFCPA(I)/1000.
   ENDDO


      DO I=its,ite

         PL=P1D(I)/1000.
         SCR3(I)=T1D(I)

         THCON=(P1000mb*0.001/PL)**ROVCP
         THX(I)=SCR3(I)*THCON
         QX(I)=0.
      ENDDO


      DO I=its,ite
         QX(I)=QV1D(I)
      ENDDO
   81 CONTINUE





      DO I=its,ite
         CAPG(I)=3.298E6*THC(I)
         IF(num_soil_layers .gt. 1)THEN




            CAPG(I)=5.9114E7*THC(I)
         ENDIF
      ENDDO

      XLDCOL=2.0                                                                 
      DO 10 I=its,ite
        XLDCOL=AMIN1(XLDCOL,XLAND(I))                                          
   10 CONTINUE                                                                   

      IF(XLDCOL.GT.1.5)GOTO 90                                                   






      DO 20 I=its,ite
        IF((XLAND(I)-1.5).GE.0)THEN                                            
          XLD1(I)=0.                                                             
        ELSE                                                                     
          XLD1(I)=1.                                                             
        ENDIF                                                                    
   20 CONTINUE                                                                   





      DO 50 I=its,ite
        IF(XLD1(I).LT.0.5)GOTO 50                                                


        PS=PSFC(I)



        TG0(I)=TSK(I)
   50 CONTINUE                                                                   




      IF(num_soil_layers .gt. 1)NSOIL=1                                                      


      IF (radiation) then
        RADSWTCH=1
      ELSE
        RADSWTCH=0
      ENDIF

      DO 70 I=its,ite
        IF(XLD1(I).LT.0.5)GOTO 70

        OLTG(I)=TSK(I)*(P1000mb*0.001/PSFC(I))**ROVCP
        UPFLUX(I)=RADSWTCH*STBOLT*TG0(I)**4                            
        XINET(I)=EMISS(I)*(GLW(I)-UPFLUX(I))    
        RNET(I)=GSW(I)+XINET(I)                                                
        HM(I)=1.18*EOMEG*(TG0(I)-TMN(I))                                       

                ESG=SVP1*EXP(SVP2*(TG0(I)-SVPT0)/(TG0(I)-SVP3))
                QSG=EP2*ESG/(PSFC(I)-ESG)
                THG=TSK(I)*(100./PSFC(I))**ROVCP
                HFX(I)=FLHC(I)*(THG-THX(I))
                QFX(I)=FLQC(I)*(QSG-QX(I))
                LH(I)=QFX(I)*XLV
        QS(I)=HFX(I)+QFX(I)*XLV                                

        IF(num_soil_layers .EQ. 1)THEN                                                       
          DTHGDT(I)=(RNET(I)-QS(I))/CAPG(I)-HM(I)                              
        ELSE
          DTHGDT(I)=0.                                                           
        ENDIF                                                                    
   70 CONTINUE                                                                   

      IF(num_soil_layers .gt. 1)THEN                                                         
        NSOIL=1+IFIX(SOILFAC*4*DIFSL/DZS(1)*DELTSM/DZS(1))   
        RNSOIL=1./FLOAT(NSOIL)                                                   



        DO ITSOIL=1,NSOIL                                                        
          DO I=its,ite
             DO L=1,num_soil_layers-1
              IF(XLD1(I).LT.0.5)GOTO 75                                          
              IF(L.EQ.1.AND.ITSOIL.GT.1)THEN                                     

                PS1=(PSFCPA(I)/P1000mb)**ROVCP    


                PS=PSFC(I)
                THG=TSLB2D(I,1)/PS1                                              
                ESG=SVP1*EXP(SVP2*(TSLB2D(I,1)-SVPT0)/(TSLB2D(I,1) & 
                    -SVP3))                                                      
                QSG=EP2*ESG/(PS-ESG)                                             

                HFXT=FLHC(I)*(THG-THX(I))                                     
                QFXT=FLQC(I)*(QSG-QX(I))
                QS(I)=HFXT+QFXT*XLV                                

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

                HFX(I)=HFX(I)*RNSOIL                                         
                QFX(I)=QFX(I)*RNSOIL                                         
                LH(I)=QFX(I)*XLV
              ENDIF                                                              
   75         CONTINUE                                                           
            ENDDO                                                                
          ENDDO                                                                  
        ENDDO                                                                    
      ENDIF                                                                      

      DO 80 I=its,ite
        IF(XLD1(I).LT.0.5) GOTO 80                                                
        TSKX=TG0(I)+DELTSM*DTHGDT(I)                                             



        TSK(I)=TSKX
   80 CONTINUE                                                                   





      IF(IFSNOW.EQ.0)GOTO 90                                              
      DO 85 I=its,ite
        IF(XLD1(I).LT.0.5)GOTO 85                                                


        TSCVN(I)=TSK(I)
        IF((SNOWC(I).GT.0..AND.TSCVN(I).GT.273.16))THEN                        
          TSCVN(I)=273.16                                                        
        ELSE                                                                     
          TSCVN(I)=TSCVN(I)                                                      
        ENDIF                                                                    

        TSK(I)=TSCVN(I)
   85 CONTINUE                                                                   

   90 CONTINUE                                                                   
      DO I=its,ite


        if ( FLQC(I) .ne. 0.) then
           QSFC(I)=QX(I)+QFX(I)/FLQC(I)
        else
           QSFC(I) = QX(I)
        end if
        CHKLOWQ(I)=MAVAIL(I)
      ENDDO

  140 CONTINUE                                                                   

   END SUBROUTINE SLAB1D


   SUBROUTINE slabinit(TSK,TMN,                                 &
                       TSLB,ZS,DZS,num_soil_layers,             &
                       allowed_to_read, start_of_simulation,    &
                       ids,ide, jds,jde, kds,kde,               &
                       ims,ime, jms,jme, kms,kme,               &
                       its,ite, jts,jte, kts,kte                )

   IMPLICIT NONE

   LOGICAL , INTENT(IN)      ::      allowed_to_read
   LOGICAL , INTENT(IN)      ::      start_of_simulation
   INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN   )    ::      num_soil_layers

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ), INTENT(INOUT) :: TSLB

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)  ::  ZS,DZS

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN)    ::                               TSK, &
                                                           TMN



   INTEGER                   ::      L,J,I,itf,jtf
   CHARACTER*1024 message


 
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   END SUBROUTINE slabinit


END MODULE module_sf_slab
