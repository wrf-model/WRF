!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_ra_sw

CONTAINS

!------------------------------------------------------------------
   SUBROUTINE SWRAD(dt,RTHRATEN,GSW,XLAT,XLONG,ALBEDO,            &
                    aer_dry,aer_water,                            &
   		    rho_phy,T3D,QV3D,QC3D,QR3D,                   &
                    QI3D,QS3D,QG3D,P3D,pi3D,dz8w,GMT,             &
                    R,CP,G,JULDAY,                                &
                    XTIME,DECLIN,SOLCON,                          &
                    F_QV,F_QC,F_QR,F_QI,F_QS,F_QG,                &
                    RADFRQ,ICLOUD,DEGRAD,warm_rain,               &
                    ids,ide, jds,jde, kds,kde,                    & 
                    ims,ime, jms,jme, kms,kme,                    &
                    its,ite, jts,jte, kts,kte                     ) 
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   INTEGER,    INTENT(IN   ) ::        ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   LOGICAL,    INTENT(IN   ) ::        warm_rain
   INTEGER,    INTENT(IN   ) ::        icloud

   REAL, INTENT(IN    )      ::        RADFRQ,DEGRAD,             &
                                       XTIME,DECLIN,SOLCON
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN    ) ::                                   P3D, &
                                               aer_dry,aer_water, &
                                                            pi3D, &
                                                         rho_phy, &
                                                            dz8w, &
                                                             T3D


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATEN
!
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                          ALBEDO
!
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                   GSW
!
   REAL, INTENT(IN   )   ::                        GMT,R,CP,G,dt
!
   INTEGER, INTENT(IN  ) ::                               JULDAY  
!
! Optional
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(IN    ) ::                                        &
                                                            QV3D, &
                                                            QC3D, &
                                                            QR3D, &
                                                            QI3D, &
                                                            QS3D, &
                                                            QG3D

   LOGICAL, OPTIONAL, INTENT(IN )      ::        F_QV,F_QC,F_QR,F_QI,F_QS,F_QG
 
! LOCAL VARS
 
   REAL, DIMENSION( kts:kte ) ::                                  &
                                                          TTEN1D, &
                                                          RHO01D, &
                                                             P1D, &
                                                              DZ, &
                                                             T1D, &
                                                            QV1D, &
                                                            QC1D, &
                                                            QR1D, &
                                                            QI1D, &
                                                            QS1D, &
                                                            QG1D
!
   REAL::      XLAT0,XLONG0,ALB0,GSW0
!
   INTEGER :: i,j,K,NK
   LOGICAL :: predicate
   real :: aer_dry1(kts:kte),aer_water1(kts:kte)

!------------------------------------------------------------------
   j_loop: DO J=jts,jte
   i_loop: DO I=its,ite

! reverse vars 
         DO K=kts,kte
            QV1D(K)=0.
            QC1D(K)=0.
            QR1D(K)=0.
            QI1D(K)=0.
            QS1D(K)=0.
            QG1D(K)=0.
         ENDDO

         DO K=kts,kte
            NK=kme-1-K+kms
            TTEN1D(K)=0.

            T1D(K)=T3D(I,NK,J)
            P1D(K)=P3D(I,NK,J)
            aer_dry1(k)=aer_dry(i,nk,j)
            aer_water1(k)=aer_water(i,nk,j)
            RHO01D(K)=rho_phy(I,NK,J)
            DZ(K)=dz8w(I,NK,J)
         ENDDO

         IF (PRESENT(F_QV) .AND. PRESENT(QV3D)) THEN
            IF (F_QV) THEN
               DO K=kts,kte
                  NK=kme-1-K+kms
                  QV1D(K)=QV3D(I,NK,J)
                  QV1D(K)=max(0.,QV1D(K))
               ENDDO
            ENDIF
         ENDIF

         IF (PRESENT(F_QC) .AND. PRESENT(QC3D)) THEN
            IF (F_QC) THEN
               DO K=kts,kte
                  NK=kme-1-K+kms
                  QC1D(K)=QC3D(I,NK,J)
                  QC1D(K)=max(0.,QC1D(K))
               ENDDO
            ENDIF
         ENDIF

         IF (PRESENT(F_QR) .AND. PRESENT(QR3D)) THEN
            IF (F_QR) THEN
               DO K=kts,kte
                  NK=kme-1-K+kms
                  QR1D(K)=QR3D(I,NK,J)
                  QR1D(K)=max(0.,QR1D(K))
               ENDDO
            ENDIF
         ENDIF

!
         IF ( PRESENT( F_QI ) ) THEN
            predicate = F_QI
         ELSE
            predicate = .FALSE.
         ENDIF

         IF ( predicate .AND. PRESENT( QI3D ) ) THEN
            DO K=kts,kte
               NK=kme-1-K+kms
               QI1D(K)=QI3D(I,NK,J)
               QI1D(K)=max(0.,QI1D(K))
            ENDDO
         ELSE
            IF (.not. warm_rain) THEN
               DO K=kts,kte
               IF(T1D(K) .lt. 273.15) THEN
                  QI1D(K)=QC1D(K)
                  QC1D(K)=0.
                  QS1D(K)=QR1D(K)
                  QR1D(K)=0.
               ENDIF
               ENDDO
            ENDIF
         ENDIF

         IF (PRESENT(F_QS) .AND. PRESENT(QS3D)) THEN
            IF (F_QS) THEN
               DO K=kts,kte          
                  NK=kme-1-K+kms
                  QS1D(K)=QS3D(I,NK,J)
                  QS1D(K)=max(0.,QS1D(K))
               ENDDO
            ENDIF
         ENDIF

         IF (PRESENT(F_QG) .AND. PRESENT(QG3D)) THEN
            IF (F_QG) THEN
               DO K=kts,kte          
                  NK=kme-1-K+kms
                  QG1D(K)=QG3D(I,NK,J)
                  QG1D(K)=max(0.,QG1D(K))
               ENDDO
            ENDIF
         ENDIF

         XLAT0=XLAT(I,J)
         XLONG0=XLONG(I,J)
         ALB0=ALBEDO(I,J)

         CALL SWPARA(TTEN1D,GSW0,XLAT0,XLONG0,ALB0,              &
                     T1D,QV1D,QC1D,QR1D,QI1D,QS1D,QG1D,P1D,      &
                     XTIME,GMT,RHO01D,DZ,                        &
                     R,CP,G,DECLIN,SOLCON,                       &
                     RADFRQ,ICLOUD,DEGRAD,aer_dry1,aer_water1,   &
                     kts,kte                                     )

         GSW(I,J)=GSW0
         DO K=kts,kte          
            NK=kme-1-K+kms
            RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+TTEN1D(NK)/pi3D(I,K,J)
         ENDDO
!
   ENDDO i_loop
   ENDDO j_loop                                          

   END SUBROUTINE SWRAD

!------------------------------------------------------------------
   SUBROUTINE SWPARA(TTEN,GSW,XLAT,XLONG,ALBEDO,                  &
                     T,QV,QC,QR,QI,QS,QG,P,            		  &
                     XTIME, GMT, RHO0, DZ,             		  &
                     R,CP,G,DECLIN,SOLCON,             		  &
                     RADFRQ,ICLOUD,DEGRAD,aer_dry1,aer_water1,    &
                     kts,kte                            	  )
!------------------------------------------------------------------
!     TO CALCULATE SHORT-WAVE ABSORPTION AND SCATTERING IN CLEAR
!     AIR AND REFLECTION AND ABSORPTION IN CLOUD LAYERS (STEPHENS,
!     1984)
!     CHANGES:
!       REDUCE EFFECTS OF ICE CLOUDS AND PRECIP ON LIQUID WATER PATH
!       ADD EFFECT OF GRAUPEL
!------------------------------------------------------------------

  INTEGER, INTENT(IN ) ::                 kts,kte
!
  REAL, DIMENSION( kts:kte ), INTENT(IN   )  ::                   &
                                                            RHO0, &
                                                               T, &
                                                               P, &
                                                              DZ, &
                                                              QV, &
                                                              QC, &
                                                              QR, &
                                                              QI, &
                                                              QS, &
                                                              QG

   REAL, DIMENSION( kts:kte ), INTENT(INOUT)::              TTEN
!
   REAL, INTENT(IN  )   ::               XTIME,GMT,R,CP,G,DECLIN, &
                                        SOLCON,XLAT,XLONG,ALBEDO, &
                                                  RADFRQ, DEGRAD
!
   REAL, INTENT(INOUT)  ::                                   GSW
!
! LOCAL VARS
!
   REAL, DIMENSION( kts:kte+1 ) ::                         SDOWN

   REAL, DIMENSION( kts:kte )   ::                          XLWP, &
						            XATP, &
						            XWVP, &
                                             aer_dry1,aer_water1, &
						              RO
!
   REAL, DIMENSION( 4, 5 ) ::                             ALBTAB, &
                                                          ABSTAB

   REAL, DIMENSION( 4    ) ::                             XMUVAL

!------------------------------------------------------------------

      DATA ALBTAB/0.,0.,0.,0., &
           69.,58.,40.,15.,    &
           90.,80.,70.,60.,    &
           94.,90.,82.,78.,    &
           96.,92.,85.,80./

      DATA ABSTAB/0.,0.,0.,0., &
           0.,2.5,4.,5.,       &
           0.,2.6,7.,10.,      &
           0.,3.3,10.,14.,     &
           0.,3.7,10.,15./

      DATA XMUVAL/0.,0.2,0.5,1.0/

      GSW=0.0
      bext340=5.E-6
      bexth2o=5.E-6
      SOLTOP=SOLCON
      XT24=AMOD(XTIME+RADFRQ*0.5,1440.)
      TLOCTM=GMT+XT24/60.+XLONG/15.
      HRANG=15.*(TLOCTM-12.)*DEGRAD
      XXLAT=XLAT*DEGRAD
      CSZA=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)

!     RETURN IF NIGHT
      IF(CSZA.LE.1.E-9)GOTO 7
!
      DO K=kts, kte

! P in the unit of 10mb
         RO(K)=P(K)/(R*T(K))
         XWVP(K)=RO(K)*QV(K)*DZ(K)*1000.
! KG/M**2
          XATP(K)=RO(K)*DZ(K)
      ENDDO
!
!     G/M**2
!     REDUCE WEIGHT OF LIQUID AND ICE IN SHORT-WAVE SCHEME
!     ADD GRAUPEL EFFECT (ASSUMED SAME AS RAIN)
!
      IF (ICLOUD.EQ.0)THEN
         DO K=kts, kte
            XLWP(K)=0.
         ENDDO
      ELSE
         DO K=kts, kte
            XLWP(K)=RO(K)*1000.*DZ(K)*(QC(K)+0.1*QI(K)+0.05* &
                    QR(K)+0.02*QS(K)+0.05*QG(K))
         ENDDO
      ENDIF
!
      XMU=CSZA
      SDOWN(1)=SOLTOP*XMU
!     SET WW (G/M**2) LIQUID WATER PATH INTEGRATED DOWN
!     SET UV (G/M**2) WATER VAPOR PATH INTEGRATED DOWN
      WW=0.
      UV=0.
      OLDALB=0.
      OLDABC=0.
      TOTABS=0.
!     CONTRIBUTIONS DUE TO CLEAR AIR AND CLOUD
      DSCA=0.
      DABS=0.
      DSCLD=0.
!
! CONTRIBUTION DUE TO AEROSOLS (FOR CHEMISTRY)
      DABSA=0.
!
      DO 200 K=kts,kte
         WW=WW+XLWP(K)
         UV=UV+XWVP(K)
!     WGM IS WW/COS(THETA) (G/M**2)
!     UGCM IS UV/COS(THETA) (G/CM**2)
         WGM=WW/XMU
         UGCM=UV*0.0001/XMU
!
         OLDABS=TOTABS
!     WATER VAPOR ABSORPTION AS IN LACIS AND HANSEN (1974)
         TOTABS=2.9*UGCM/((1.+141.5*UGCM)**0.635+5.925*UGCM)
!     APPROXIMATE RAYLEIGH + AEROSOL SCATTERING
         XSCA=1.E-5*XATP(K)/XMU
!         XSCA=(1.E-5*XATP(K)+aer_dry1(K)*bext340+aer_water1(K)*bexth2o)/XMU   
!     LAYER VAPOR ABSORPTION DONE FIRST
         XABS=(TOTABS-OLDABS)*(SDOWN(1)-DSCLD-DSCA-DABSA)/SDOWN(K)
!rs   AEROSOL ABSORB (would be elemental carbon). So far XABSA = 0.
         XABSA=0.
         IF(XABS.LT.0.)XABS=0.
!
         ALW=ALOG10(WGM+1.)
         IF(ALW.GT.3.999)ALW=3.999
!
         DO II=1,3
            IF(XMU.GT.XMUVAL(II))THEN
              IIL=II
              IU=II+1
              XI=(XMU-XMUVAL(II))/(XMUVAL(II+1)-XMUVAL(II))+FLOAT(IIL)
            ENDIF
         ENDDO
!
         JJL=IFIX(ALW)+1
         JU=JJL+1
         YJ=ALW+1.
!     CLOUD ALBEDO
         ALBA=(ALBTAB(IU,JU)*(XI-IIL)*(YJ-JJL)   &
              +ALBTAB(IIL,JU)*(IU-XI)*(YJ-JJL)   &
              +ALBTAB(IU,JJL)*(XI-IIL)*(JU-YJ)   &
              +ALBTAB(IIL,JJL)*(IU-XI)*(JU-YJ))  &
             /((IU-IIL)*(JU-JJL))
!     CLOUD ABSORPTION
         ABSC=(ABSTAB(IU,JU)*(XI-IIL)*(YJ-JJL)   &
              +ABSTAB(IIL,JU)*(IU-XI)*(YJ-JJL)   &
              +ABSTAB(IU,JJL)*(XI-IIL)*(JU-YJ)   &
              +ABSTAB(IIL,JJL)*(IU-XI)*(JU-YJ))  &
             /((IU-IIL)*(JU-JJL))
!     LAYER ALBEDO AND ABSORPTION
         XALB=(ALBA-OLDALB)*(SDOWN(1)-DSCA-DABS)/SDOWN(K)
         XABSC=(ABSC-OLDABC)*(SDOWN(1)-DSCA-DABS)/SDOWN(K)
         IF(XALB.LT.0.)XALB=0.
         IF(XABSC.LT.0.)XABSC=0.
         DSCLD=DSCLD+(XALB+XABSC)*SDOWN(K)*0.01
         DSCA=DSCA+XSCA*SDOWN(K)
         DABS=DABS+XABS*SDOWN(K)
         DABSA=DABSA+XABSA*SDOWN(K)
         OLDALB=ALBA
         OLDABC=ABSC
!     LAYER TRANSMISSIVITY
         TRANS0=100.-XALB-XABSC-XABS*100.-XSCA*100.
         IF(TRANS0.LT.1.)THEN
           FF=99./(XALB+XABSC+XABS*100.+XSCA*100.)
           XALB=XALB*FF
           XABSC=XABSC*FF
           XABS=XABS*FF
           XSCA=XSCA*FF
           TRANS0=1.
         ENDIF
         SDOWN(K+1)=AMAX1(1.E-9,SDOWN(K)*TRANS0*0.01)
         TTEN(K)=SDOWN(K)*(XABSC+XABS*100.+XABSA*100.)*0.01/( &
                 RO(K)*CP*DZ(K))
  200   CONTINUE
!
        GSW=(1.-ALBEDO)*SDOWN(kte+1)

    7 CONTINUE
!
   END SUBROUTINE SWPARA

!====================================================================
   SUBROUTINE swinit(RTHRATEN,RTHRATENSW,restart,                   &
                     allowed_to_read ,                              &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           :: restart
   LOGICAL , INTENT(IN)           :: allowed_to_read 
   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,  &
                                     ims, ime, jms, jme, kms, kme,  &
                                     its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::        &
                                                          RTHRATEN, &
                                                        RTHRATENSW
   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHRATEN(i,k,j)=0.
        RTHRATENSW(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   END SUBROUTINE swinit


END MODULE module_ra_sw
