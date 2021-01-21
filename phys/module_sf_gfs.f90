

MODULE module_sf_gfs


CONTAINS


   SUBROUTINE SF_GFS(U3D,V3D,T3D,QV3D,P3D,                              &
		 CP,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2,CPM,             &
                     ZNT,UST,PSIM,PSIH,                                 &
		 XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,                    &
                     QGH,QSFC,U10,V10,                                  &
                     GZ1OZ0,WSPD,BR,ISFFLX,                             &
                     EP1,EP2,KARMAN,itimestep,                          &
                     ids,ide, jds,jde, kds,kde,                         &
                     ims,ime, jms,jme, kms,kme,                         &
                     its,ite, jts,jte, kts,kte                     )

      USE MODULE_GFS_MACHINE, ONLY : kind_phys
      USE MODULE_GFS_FUNCPHYS , ONLY : gfuncphys,fpvs

      IMPLICIT NONE



















































      INTEGER, INTENT(IN) ::            ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        ISFFLX,itimestep

      REAL,    INTENT(IN) ::                                            &
                                        CP,                             &
                                        EP1,                            &
                                        EP2,                            &
                                        KARMAN,                         &
                                        R,                              &
                                        ROVCP,                          &
                                        XLV

      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      &
                                        P3D,                            &
                                        QV3D,                           &
                                        T3D,                            &
                                        U3D,                            &
                                        V3D

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        TSK,                            &
                                        PSFC,                           &
                                        XLAND

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            &
                                        UST,                            &
                                        ZNT

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::                 &
                                        BR,                             &
                                        CHS,                            &
                                        CHS2,                           &
                                        CPM,                            &
                                        CQS2,                           &
                                        FLHC,                           &
                                        FLQC,                           &
                                        GZ1OZ0,                         &
                                        HFX,                            &
                                        LH,                             &
                                        PSIM,                           &
                                        PSIH,                           &
                                        QFX,                            &
                                        QGH,                            &
                                        QSFC,                           &
                                        U10,                            &
                                        V10,                            &
                                        WSPD




      REAL ::                           ESAT

      REAL     (kind=kind_phys) ::                                      &
                                        RHOX

      REAL     (kind=kind_phys), DIMENSION(its:ite) ::                  &
                                        CH,                             &
                                        CM,                             &
                                        DDVEL,                          &
                                        DRAIN,                          &
                                        EP,                             &
                                        EVAP,                           &
                                        FH,                             &
                                        FH2,                            &
                                        FM,                             &
                                        HFLX,                           &
                                        PH,                             &
                                        PM,                             &
                                        PRSL1,                          &
                                        PRSLKI,                         &
                                        PS,                             &
                                        Q1,                             &
                                        Q2M,                            &
                                        QSS,                            &
                                        QSURF,                          &
                                        RB,                             &
                                        RCL,                            &
                                        RHO1,                           &
                                        SLIMSK,                         &
                                        STRESS,                         &
                                        T1,                             &
                                        T2M,                            &
                                        THGB,                           &
                                        THX,                            &
                                        TSKIN,                          &
                                        SHELEG,                         &
                                        U1,                             &
                                        U10M,                           &
                                        USTAR,                          &
                                        V1,                             &
                                        V10M,                           &
                                        WIND,                           &
                                        Z0RL,                           &
                                        Z1


      INTEGER ::                                                        &
                                        I,                              &
                                        IM,                             &
                                        J,                              &
                                        K,                              &
                                        KM


   if(itimestep.eq.0) then
     CALL GFUNCPHYS
   endif

   IM=ITE-ITS+1
   KM=KTE-KTS+1

   DO J=jts,jte

      DO i=its,ite
        DDVEL(I)=0.
        RCL(i)=1.
        PRSL1(i)=P3D(i,kts,j)*.001
        PS(i)=PSFC(i,j)*.001
        Q1(I) = QV3D(i,kts,j)

        QSURF(I)=0.
        SHELEG(I)=0.
        SLIMSK(i)=ABS(XLAND(i,j)-2.)
        TSKIN(i)=TSK(i,j)
        T1(I) = T3D(i,kts,j)
        U1(I) = U3D(i,kts,j)
        USTAR(I) = UST(i,j)
        V1(I) = V3D(i,kts,j)
        Z0RL(I) = ZNT(i,j)*100.
      ENDDO

      DO i=its,ite
         PRSLKI(i)=(PS(I)/PRSL1(I))**ROVCP
         THGB(I)=TSKIN(i)*(100./PS(I))**ROVCP
         THX(I)=T1(i)*(100./PRSL1(I))**ROVCP
         RHO1(I)=PRSL1(I)*1000./(R*T1(I)*(1.+EP1*Q1(I)))
         Q1(I)=Q1(I)/(1.+Q1(I))
      ENDDO


      CALL PROGTM(IM,KM,PS,U1,V1,T1,Q1,                                 &
                  SHELEG,TSKIN,QSURF,                                   &


                  Z0RL,                                                 &

                  U10M,V10M,T2M,Q2M,                                    &

                  CM,CH,RB,                                             &

                  RCL,PRSL1,PRSLKI,SLIMSK,                              &
                  DRAIN,EVAP,HFLX,STRESS,EP,                            &
                  FM,FH,USTAR,WIND,DDVEL,                               &
                  PM,PH,FH2,QSS,Z1                                      )


      DO i=its,ite
        U10(i,j)=U10M(i)
        V10(i,j)=V10M(i)
        BR(i,j)=RB(i)
        CHS(I,J)=CH(I)*WIND(I)
        CHS2(I,J)=USTAR(I)*KARMAN/FH2(I)
        CPM(I,J)=CP*(1.+0.8*QV3D(i,kts,j))
        esat = fpvs(t1(i))
        QGH(I,J)=ep2*esat/(1000.*ps(i)-esat)
        QSFC(I,J)=qss(i)
        PSIH(i,j)=PH(i)
        PSIM(i,j)=PM(i)
        UST(i,j)=ustar(i)
        WSPD(i,j)=WIND(i)
        ZNT(i,j)=Z0RL(i)*.01
      ENDDO

      DO i=its,ite
        FLHC(i,j)=CPM(I,J)*RHO1(I)*CHS(I,J)
        FLQC(i,j)=RHO1(I)*CHS(I,J)
        GZ1OZ0(i,j)=LOG(Z1(I)/(Z0RL(I)*.01))
        CQS2(i,j)=CHS2(I,J)
      ENDDO

      IF (ISFFLX.EQ.0) THEN
        DO i=its,ite
          HFX(i,j)=0.
          LH(i,j)=0.
          QFX(i,j)=0.
        ENDDO
      ELSE
        DO i=its,ite
          IF(XLAND(I,J)-1.5.GT.0.)THEN
            HFX(I,J)=FLHC(I,J)*(THGB(I)-THX(I))
          ELSEIF(XLAND(I,J)-1.5.LT.0.)THEN
            HFX(I,J)=FLHC(I,J)*(THGB(I)-THX(I))
            HFX(I,J)=AMAX1(HFX(I,J),-250.)
          ENDIF
          QFX(I,J)=FLQC(I,J)*(QSFC(I,J)-Q1(I))
          QFX(I,J)=AMAX1(QFX(I,J),0.)
          LH(I,J)=XLV*QFX(I,J)
        ENDDO
      ENDIF


    ENDDO


   END SUBROUTINE SF_GFS




      SUBROUTINE PROGTM(IM,KM,PS,U1,V1,T1,Q1,                           &
     &                  SHELEG,TSKIN,QSURF,                             &


     &                  Z0RL,                                           &

     &                  U10M,V10M,T2M,Q2M,                              &

     &                  CM, CH, RB,                                     &

     &                  RCL,PRSL1,PRSLKI,SLIMSK,                        &
     &                  DRAIN,EVAP,HFLX,STRESS,EP,                      &
     &                  FM,FH,USTAR,WIND,DDVEL,                         &
     &                  PM,PH,FH2,QSS,Z1                                )


      USE MODULE_GFS_MACHINE, ONLY : kind_phys
      USE MODULE_GFS_FUNCPHYS, ONLY : fpvs
      USE MODULE_GFS_PHYSCONS, grav => con_g, SBC => con_sbc, HVAP => con_HVAP &
     &,             CP => con_CP, HFUS => con_HFUS, JCAL => con_JCAL    &
     &,             EPS => con_eps, EPSM1 => con_epsm1, t0c => con_t0c  &
     &,             RVRDM1 => con_FVirt, RD => con_RD
      implicit none



      integer              IM, km

      real(kind=kind_phys), parameter :: cpinv=1.0/cp, HVAPI=1.0/HVAP
      real(kind=kind_phys) DELT
      INTEGER              SOILTYP(IM),  VEGTYPE(IM)
      real(kind=kind_phys) PS(IM),       U1(IM),      V1(IM),           &
     &                     T1(IM),       Q1(IM),      SHELEG(IM),       &
     &                     TSKIN(IM),    QSURF(IM),   SMC(IM,KM),       &
     &                     STC(IM,KM),   DM(IM),      SIGMAF(IM),       &
     &                     CANOPY(IM),   DLWFLX(IM),  SLRAD(IM),        &
     &                     SNOWMT(IM),   Z0RL(IM),    TG3(IM),          &
     &                     GFLUX(IM),    F10M(IM),    U10M(IM),         &
     &                     V10M(IM),     T2M(IM),     Q2M(IM),          &
     &                     ZSOIL(IM,KM), CM(IM),      CH(IM),           &
     &                     RB(IM),       RHSCNPY(IM), RHSMC(IM,KM),     &
     &                     AIM(IM,KM),   BIM(IM,KM),  CIM(IM,KM),       &
     &                     RCL(IM),      PRSL1(IM),   PRSLKI(IM),       &
     &                     SLIMSK(IM),   DRAIN(IM),   EVAP(IM),         &
     &                     HFLX(IM),     RNET(IM),    EP(IM),           &
     &                     FM(IM),       FH(IM),      USTAR(IM),        &
     &                     WIND(IM),     DDVEL(IM),   STRESS(IM)



      integer              k,i

      real(kind=kind_phys) CANFAC(IM),                                  &
     &                     DDZ(IM),     DDZ2(IM),    DELTA(IM),         &
     &                     DEW(IM),     DF1(IM),     DFT0(IM),          &
     &                     DFT2(IM),    DFT1(IM),                       &
     &                     DMDZ(IM),    DMDZ2(IM),   DTDZ1(IM),         &
     &                     DTDZ2(IM),   DTV(IM),     EC(IM),            &
     &                     EDIR(IM),    ETPFAC(IM),                     &
     &                     FACTSNW(IM), FH2(IM),     FM10(IM),          &
     &                     FX(IM),      GX(IM),                         &
     &                     HCPCT(IM),   HL1(IM),     HL12(IM),          &
     &                     HLINF(IM),   PARTLND(IM), PH(IM),            &
     &                     PH2(IM),     PM(IM),      PM10(IM),          &
     &                     PSURF(IM),   Q0(IM),      QS1(IM),           &
     &                     QSS(IM),     RAT(IM),     RCAP(IM),          &
     &                     RCH(IM),     RHO(IM),     RS(IM),            &
     &                     RSMALL(IM),  SLWD(IM),    SMCZ(IM),          &
     &                     SNET(IM),    SNOEVP(IM),  SNOWD(IM),         &
     &                     T1O(IM),     T2MO(IM),    TERM1(IM),         &
     &                     TERM2(IM),   THETA1(IM),  THV1(IM),          &
     &                     TREF(IM),    TSURF(IM),   TV1(IM),           &
     &                     TVS(IM),     TSURFO(IM),  TWILT(IM),         &
     &                     XX(IM),      XRCL(IM),    YY(IM),            &
     &                     Z0(IM),      Z0MAX(IM),   Z1(IM),            &
     &                     ZTMAX(IM),   ZZ(IM),      PS1(IM)

      real(kind=kind_phys) a0,    a0p,      a1,    a1p,     aa,  aa0,   &
     &                     aa1,   adtv,     alpha, arnu,    b1,  b1p,   &
     &                     b2,    b2p,      bb,    bb0,     bb1, bb2,   &
     &                     bfact, ca,       cc,    cc1,    cc2, cfactr, &
     &                     ch2o,  charnock, cice,  convrad, cq,  csoil, &
     &                     ctfil1,ctfil2,   delt2, df2,     dfsnow,     &
     &                     elocp, eth,      ff,  FMS,                   &

     &                     fhs,   g,        hl0,    hl0inf,             &
     &                     hl110, hlt,      hltinf,OLINF,   rcq, rcs,   &
     &                     rct,   restar,   rhoh2o,rnu,     RSI,        &
     &                     rss,   scanop,   sig2k, sigma,   smcdry,     &
     &                     t12,   t14,      tflx,  tgice,   topt,       &
     &                     val,   vis,      zbot,  snomin,  tem



      PARAMETER (CHARNOCK=.014,CA=.4)
      PARAMETER (G=grav,sigma=sbc)

      PARAMETER (ALPHA=5.,A0=-3.975,A1=12.32,B1=-7.755,B2=6.041)
      PARAMETER (A0P=-7.941,A1P=24.75,B1P=-8.705,B2P=7.899,VIS=1.4E-5)
      PARAMETER (AA1=-1.076,BB1=.7045,CC1=-.05808)
      PARAMETER (BB2=-.1954,CC2=.009999)
      PARAMETER (ELOCP=HVAP/CP,DFSNOW=.31,CH2O=4.2E6,CSOIL=1.26E6)
      PARAMETER (SCANOP=.5,CFACTR=.5,ZBOT=-3.,TGICE=271.2)
      PARAMETER (CICE=1880.*917.,topt=298.)
      PARAMETER (RHOH2O=1000.,CONVRAD=JCAL*1.E4/60.)
      PARAMETER (CTFIL1=.5,CTFIL2=1.-CTFIL1)
      PARAMETER (RNU=1.51E-5,ARNU=.135*RNU)
      parameter (snomin=1.0e-9)

      LOGICAL FLAG(IM), FLAGSNW(IM)

      real(kind=kind_phys) KT1(IM),       KT2(IM),                      &
     &                     ET(IM,KM),                                   &
     &                     STSOIL(IM,KM), AI(IM,KM),    BI(IM,KM),      &
     &                     CI(IM,KM),     RHSTC(IM,KM)
      real(kind=kind_phys) rsmax(13), rgl(13),  rsmin(13), hs(13),      &
     &                     smmax(9),  smdry(9), smref(9),  smwlt(9)


















      data rsmax/13*5000./
      data rsmin/150.,100.,125.,150.,100.,70.,40.,                      &
     &           300.,400.,150.,999.,40.,999./
      data rgl/5*30.,65.,4*100.,999.,100.,999./
      data hs/41.69,54.53,51.93,47.35,47.35,54.53,36.35,                &
     &        3*42.00,999.,36.35,999./
      data smmax/.421,.464,.468,.434,.406,.465,.404,.439,.421/
      data smdry/.07,.14,.22,.08,.18,.16,.12,.10,.07/
      data smref/.283,.387,.412,.312,.338,.382,.315,.329,.283/
      data smwlt/.029,.119,.139,.047,.010,.103,.069,.066,.029/








      SIG2K = 1. - 4. * G * 2. / (CP * 280.)












      DO I=1,IM
        XRCL(I)  = SQRT(RCL(I))
        PSURF(I) = 1000. * PS(I)
        PS1(I)   = 1000. * PRSL1(I)







        WIND(I) = XRCL(I) * SQRT(U1(I) * U1(I) + V1(I) * V1(I))         &
     &              + MAX(0.0_kind_phys, MIN(DDVEL(I), 30.0_kind_phys))
        WIND(I) = MAX(WIND(I),1._kind_phys)
        Q0(I) = MAX(Q1(I),1.E-8_kind_phys)
        TSURF(I) = TSKIN(I)
        THETA1(I) = T1(I) * PRSLKI(I)
        TV1(I) = T1(I) * (1. + RVRDM1 * Q0(I))
        THV1(I) = THETA1(I) * (1. + RVRDM1 * Q0(I))
        TVS(I) = TSURF(I) * (1. + RVRDM1 * Q0(I))
        RHO(I) = PS1(I) / (RD * TV1(I))

        qs1(i) = fpvs(t1(i))
        QS1(I) = EPS * QS1(I) / (PS1(I) + EPSM1 * QS1(I))
        QS1(I) = MAX(QS1(I), 1.E-8_kind_phys)
        Q0(I) = min(QS1(I),Q0(I))

        qss(i) = fpvs(tskin(i))
        QSS(I) = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))

        RS(I) = 0.

        Z0(I) = .01 * Z0RL(i)

        DM(I) = 1.

      GOTO 1111

        FACTSNW(I) = 10.
        IF(SLIMSK(I).EQ.2.) FACTSNW(I) = 3.



        SNOWD(I) = SHELEG(I) / 1000.
        FLAGSNW(I) = .FALSE.






        IF(SNOWD(I).GT..001.OR.SLIMSK(I).EQ.2.) RS(I) = 0.
        IF(SNOWD(I).GT..001) FLAGSNW(I) = .TRUE.





        IF(SLIMSK(I).EQ.0.) THEN
          ZSOIL(I,1) = 0.
        ELSEIF(SLIMSK(I).EQ.1.) THEN
          ZSOIL(I,1) = -.10
        ELSE
          ZSOIL(I,1) = -3. / KM
        ENDIF

1111  CONTINUE

      ENDDO



      GOTO 2222

      DO K = 2, KM
        DO I=1,IM
          IF(SLIMSK(I).EQ.0.) THEN
            ZSOIL(I,K) = 0.
          ELSEIF(SLIMSK(I).EQ.1.) THEN
            ZSOIL(I,K) = ZSOIL(I,K-1)                                   &
     &                   + (-2. - ZSOIL(I,1)) / (KM - 1)
          ELSE
            ZSOIL(I,K) = - 3. * FLOAT(K) / FLOAT(KM)
          ENDIF
        ENDDO
      ENDDO

2222  CONTINUE


      DO I=1,IM
        Z1(I) = -RD * TV1(I) * LOG(PS1(I)/PSURF(I)) / G
        DRAIN(I) = 0.
      ENDDO


      DO K = 1, KM
        DO I=1,IM
          ET(I,K) = 0.
          RHSMC(I,K) = 0.
          AIM(I,K) = 0.
          BIM(I,K) = 1.
          CIM(I,K) = 0.
          STSOIL(I,K) = STC(I,K)
        ENDDO
      ENDDO

      DO I=1,IM
        EDIR(I) = 0.
        EC(I) = 0.
        EVAP(I) = 0.
        EP(I) = 0.
        SNOWMT(I) = 0.
        GFLUX(I) = 0.
        RHSCNPY(I) = 0.
        FX(I) = 0.
        ETPFAC(I) = 0.
        CANFAC(I) = 0.
      ENDDO





      DO I=1,IM
        IF(SLIMSK(I).EQ.0.) THEN
          USTAR(I) = SQRT(G * Z0(I) / CHARNOCK)
        ENDIF




        Z0MAX(I) = MIN(Z0(I),0.1 * Z1(I))
        ZTMAX(I) = Z0MAX(I)
        IF(SLIMSK(I).EQ.0.) THEN
          RESTAR = USTAR(I) * Z0MAX(I) / VIS
          RESTAR = MAX(RESTAR,.000001_kind_phys)







          RAT(I) = 2.67 * restar ** .25 - 2.57
          RAT(I) = min(RAT(I),7._kind_phys)
          ZTMAX(I) = Z0MAX(I) * EXP(-RAT(I))
        ENDIF
      ENDDO





      DO I = 1, IM
        DTV(I) = THV1(I) - TVS(I)
        ADTV = ABS(DTV(I))
        ADTV = MAX(ADTV,.001_kind_phys)
        DTV(I) = SIGN(1._kind_phys,DTV(I)) * ADTV
        RB(I) = G * DTV(I) * Z1(I) / (.5 * (THV1(I) + TVS(I))           &
     &          * WIND(I) * WIND(I))
        RB(I) = MAX(RB(I),-5000._kind_phys)


        FM(I) = LOG((Z1(I)) / Z0MAX(I))
        FH(I) = LOG((Z1(I)) / ZTMAX(I))
        HLINF(I) = RB(I) * FM(I) * FM(I) / FH(I)
        FM10(I) = LOG((Z0MAX(I)+10.) / Z0MAX(I))
        FH2(I) = LOG((ZTMAX(I)+2.) / ZTMAX(I))
      ENDDO







      DO I = 1, IM
        IF(DTV(I).GE.0.) THEN
          HL1(I) = HLINF(I)
        ENDIF
        IF(DTV(I).GE.0..AND.HLINF(I).GT..25) THEN
          HL0INF = Z0MAX(I) * HLINF(I) / Z1(I)
          HLTINF = ZTMAX(I) * HLINF(I) / Z1(I)
          AA = SQRT(1. + 4. * ALPHA * HLINF(I))
          AA0 = SQRT(1. + 4. * ALPHA * HL0INF)
          BB = AA
          BB0 = SQRT(1. + 4. * ALPHA * HLTINF)
          PM(I) = AA0 - AA + LOG((AA + 1.) / (AA0 + 1.))
          PH(I) = BB0 - BB + LOG((BB + 1.) / (BB0 + 1.))
          FMS = FM(I) - PM(I)
          FHS = FH(I) - PH(I)
          HL1(I) = FMS * FMS * RB(I) / FHS
        ENDIF
      ENDDO



      DO I = 1, IM
        IF(DTV(I).GE.0.) THEN
          HL0 = Z0MAX(I) * HL1(I) / Z1(I)
          HLT = ZTMAX(I) * HL1(I) / Z1(I)
          AA = SQRT(1. + 4. * ALPHA * HL1(I))
          AA0 = SQRT(1. + 4. * ALPHA * HL0)
          BB = AA
          BB0 = SQRT(1. + 4. * ALPHA * HLT)
          PM(I) = AA0 - AA + LOG((AA + 1.) / (AA0 + 1.))
          PH(I) = BB0 - BB + LOG((BB + 1.) / (BB0 + 1.))
          HL110 = HL1(I) * 10. / Z1(I)
          AA = SQRT(1. + 4. * ALPHA * HL110)
          PM10(I) = AA0 - AA + LOG((AA + 1.) / (AA0 + 1.))
          HL12(I) = HL1(I) * 2. / Z1(I)

          BB = SQRT(1. + 4. * ALPHA * HL12(I))
          PH2(I) = BB0 - BB + LOG((BB + 1.) / (BB0 + 1.))
        ENDIF
      ENDDO











      DO I=1,IM
        IF(DTV(I).LT.0.) THEN
          OLINF = Z1(I) / HLINF(I)
          IF(ABS(OLINF).LE.50. * Z0MAX(I)) THEN
            HLINF(I) = -Z1(I) / (50. * Z0MAX(I))
          ENDIF
        ENDIF
      ENDDO



      DO I = 1, IM
        IF(DTV(I).LT.0..AND.HLINF(I).GE.-.5) THEN
          HL1(I) = HLINF(I)
          PM(I) = (A0 + A1 * HL1(I)) * HL1(I)                           &
     &            / (1. + B1 * HL1(I) + B2 * HL1(I) * HL1(I))
          PH(I) = (A0P + A1P * HL1(I)) * HL1(I)                         &
     &            / (1. + B1P * HL1(I) + B2P * HL1(I) * HL1(I))
          HL110 = HL1(I) * 10. / Z1(I)
          PM10(I) = (A0 + A1 * HL110) * HL110                           &
     &            / (1. + B1 * HL110 + B2 * HL110 * HL110)
          HL12(I) = HL1(I) * 2. / Z1(I)
          PH2(I) = (A0P + A1P * HL12(I)) * HL12(I)                      &
     &            / (1. + B1P * HL12(I) + B2P * HL12(I) * HL12(I))
        ENDIF
        IF(DTV(I).LT.0.AND.HLINF(I).LT.-.5) THEN
          HL1(I) = -HLINF(I)
          PM(I) = LOG(HL1(I)) + 2. * HL1(I) ** (-.25) - .8776
          PH(I) = LOG(HL1(I)) + .5 * HL1(I) ** (-.5) + 1.386
          HL110 = HL1(I) * 10. / Z1(I)
          PM10(I) = LOG(HL110) + 2. * HL110 ** (-.25) - .8776
          HL12(I) = HL1(I) * 2. / Z1(I)
          PH2(I) = LOG(HL12(I)) + .5 * HL12(I) ** (-.5) + 1.386
        ENDIF
      ENDDO



      DO I = 1, IM

        FM(I) = FM(I) - PM(I)
        FH(I) = FH(I) - PH(I)
        FM10(I) = FM10(I) - PM10(I)
        FH2(I) = FH2(I) - PH2(I)
        CM(I) = CA * CA / (FM(I) * FM(I))
        CH(I) = CA * CA / (FM(I) * FH(I))
        CQ = CH(I)
        STRESS(I) = CM(I) * WIND(I) * WIND(I)
        USTAR(I)  = SQRT(STRESS(I))

      ENDDO







      DO I = 1, IM
        IF(SLIMSK(I).EQ.0.) THEN
          Z0(I) = (CHARNOCK / G) * USTAR(I) ** 2





          Z0(I) = MIN(Z0(I),.1_kind_phys)
          Z0(I) = MAX(Z0(I),1.E-7_kind_phys)
          Z0RL(I) = 100. * Z0(I)
        ENDIF
      ENDDO

	  GOTO 5555



      DO I = 1, IM
        RCH(I) = RHO(I) * CP * CH(I) * WIND(I)
      ENDDO





      DO I = 1, IM
        IF(SLIMSK(I).EQ.0.) THEN
          EVAP(I) = ELOCP * RCH(I) * (QSS(I) - Q1(I))
          DM(I) = 1.
          QSURF(I) = QSS(I)
        ENDIF
      ENDDO

        
        
        
        
	    DO I = 1, IM
	      GFLUX(I) = 0.
	      IF(SLIMSK(I).EQ.1.) THEN
	        SMCZ(I) = .5 * (SMC(I,1) + .20)
	        DFT0(I) = KTSOIL(SMCZ(I),SOILTYP(I))
	      ELSEIF(SLIMSK(I).EQ.2.) THEN
        
        
	        DFT0(I) = 2.2
	      ENDIF
	    ENDDO
        
	    DO I=1,IM
	      IF(SLIMSK(I).NE.0.) THEN
        
	        IF(FLAGSNW(I)) THEN
        
        
        
		TFLX = MIN(T1(I), TSURF(I))
		GFLUX(I) = -DFSNOW * (TFLX - STSOIL(I,1))                   &
	   &                 / (FACTSNW(I) * MAX(SNOWD(I),.001_kind_phys))
	        ELSE
		GFLUX(I) = DFT0(I) * (STSOIL(I,1) - TSURF(I))               &
	   &                 / (-.5 * ZSOIL(I,1))
	        ENDIF
	        GFLUX(I) = MAX(GFLUX(I),-200._kind_phys)
	        GFLUX(I) = MIN(GFLUX(I),+200._kind_phys)
	      ENDIF
	    ENDDO
	    DO I = 1, IM
	      FLAG(I) = SLIMSK(I).NE.0.
	      PARTLND(I) = 1.
	      IF(SNOWD(I).GT.0..AND.SNOWD(I).LE..001) THEN
	        PARTLND(I) = 1. - SNOWD(I) / .001
	      ENDIF
	    ENDDO
	    DO I = 1, IM
	      SNOEVP(I) = 0.
	      if(SNOWD(I).gt..001) PARTLND(I) = 0.
	    ENDDO
        
        
        
	    DO I = 1, IM
	      IF(FLAG(I)) THEN
	        T12 = T1(I) * T1(I)
	        T14 = T12 * T12
        
        
        
	        RCAP(I) = -SLWD(I) - SIGMA * T14 + GFLUX(I)                   &
	   &              - RCH(I) * (T1(I) - THETA1(I))
        
        
        
	        RSMALL(I) = 4. * SIGMA * T1(I) * T12 / RCH(I) + 1.
        
        
        
	        DELTA(I) = ELOCP * EPS * HVAP * QS1(I) / (RD * T12)
        
        
        
        
	        TERM1(I) = ELOCP * RSMALL(I) * RCH(I)*(QS1(I)-Q0(I))
	        TERM2(I) = RCAP(I) * DELTA(I)
	        EP(I) = (ELOCP * RSMALL(I) * RCH(I) * (QS1(I) - Q0(I))        &
	   &              + RCAP(I) * DELTA(I))
	        EP(I) = EP(I) / (RSMALL(I) + DELTA(I))
	      ENDIF
	    ENDDO
        
        
        
        
        
	    DO I = 1, IM
	      FLAG(I) = SLIMSK(I).EQ.1..AND.EP(I).GT.0.
	    ENDDO
	    DO I = 1, IM
	      IF(FLAG(I)) THEN
	        DF1(I) = FUNCDF(SMC(I,1),SOILTYP(I))
	        KT1(I) = FUNCKT(SMC(I,1),SOILTYP(I))
	      endif
	      if(FLAG(I).and.STC(I,1).lt.t0c) then
	        DF1(I) = 0.
	        KT1(I) = 0.
	      endif
	      IF(FLAG(I)) THEN
        
	        TREF(I) = smref(SOILTYP(I))
        
	        TWILT(I) = smwlt(SOILTYP(I))
	        smcdry = smdry(SOILTYP(I))
        
        
	        FX(I) = -2. * DF1(I) * (SMC(I,1) - smcdry) / ZSOIL(I,1)       &
	   &            - KT1(I)
	        FX(I) = MIN(FX(I), EP(I)/HVAP)
	        FX(I) = MAX(FX(I),0._kind_phys)
        
        
        
	        EDIR(I) = FX(I) * (1. - SIGMAF(I)) * PARTLND(I)
	      ENDIF
	    ENDDO
        
        
        
	    DO I = 1, IM
	      if(FLAG(I)) then
        
        
        
	        ff = .55 * 2. * SNET(I) / rgl(VEGTYPE(I))
	        rcs = (ff + RS(I)/rsmax(VEGTYPE(I))) / (1. + ff)
	        rcs = max(rcs,.0001_kind_phys)
	        rct = 1.
	        rcq = 1.
        
        
        
        
        
        
        
        
        
        
        
        
        
	        RS(I) = RS(I) / (rcs * rct * rcq)
	      endif
	    ENDDO
        
        
        
	    DO I = 1, IM
	      IF(FLAG(I)) THEN
	        CANFAC(I) = (CANOPY(I) / SCANOP) ** CFACTR
	      endif
	      IF(FLAG(I)) THEN
	        ETPFAC(I) = SIGMAF(I)                                         &
	   &           * (1. - CANFAC(I)) / HVAP
	        GX(I) = (SMC(I,1) - TWILT(I)) / (TREF(I) - TWILT(I))
	        GX(I) = MAX(GX(I),0._kind_phys)
	        GX(I) = MIN(GX(I),1._kind_phys)
        
        
        
	        rss = GX(I) * (ZSOIL(I,1) / ZSOIL(I,km))
	        rss = max(rss,.0001_kind_phys)
	        RSI = RS(I) / rss
        
        
        
	        eth = (TERM1(I) + TERM2(I)) /                                 &
	   &          (DELTA(I) + RSMALL(I) * (1. + RSI * CH(I) * WIND(I)))
	        ET(I,1) = ETPFAC(I) * eth                                     &
	   &            * PARTLND(I)
	      ENDIF
	    ENDDO
        
	    DO K = 2, KM
	      DO I=1,IM
	        IF(FLAG(I)) THEN
		GX(I) = (SMC(I,K) - TWILT(I)) / (TREF(I) - TWILT(I))
		GX(I) = MAX(GX(I),0._kind_phys)
		GX(I) = MIN(GX(I),1._kind_phys)
        
        
        
	        rss = GX(I) * ((ZSOIL(I,k) - ZSOIL(I,k-1))/ZSOIL(I,km))
	        rss = max(rss,1.e-6_kind_phys)
	        RSI = RS(I) / rss
        
        
        
	        eth = (TERM1(I) + TERM2(I)) /                                 &
	   &          (DELTA(I) + RSMALL(I) * (1. + RSI * CH(I) * WIND(I)))
		ET(I,K) = eth                                               &
	   &               * ETPFAC(I) * PARTLND(I)
	        ENDIF
	      ENDDO
	    ENDDO
        
         400  CONTINUE
        
        
        
	    DO I=1,IM
	      IF(FLAG(I)) THEN
	        EC(I) = SIGMAF(I) * CANFAC(I) * EP(I) / HVAP
	        EC(I) = EC(I) * PARTLND(I)
	        EC(I) = min(EC(I),CANOPY(I)/delt)
	      ENDIF
	    ENDDO
        
        
        
	    DO I = 1, IM
	      IF(FLAG(I)) THEN
	       EVAP(I) = EDIR(I) + EC(I)
	      ENDIF
	    ENDDO
        
	    DO K = 1, KM
	      DO I=1,IM
	        IF(FLAG(I)) THEN
		EVAP(I) = EVAP(I) + ET(I,K)
	        ENDIF
	      ENDDO
	    ENDDO
        
        
        
        
	    DO I=1,IM
	      IF(FLAG(I)) THEN
	        EVAP(I) = MIN(EVAP(I)*HVAP,EP(I))
	      ENDIF
	    ENDDO
        

        


        
        
        
        
	    DO I = 1, IM
        
	      IF(SLIMSK(I).EQ.2.) THEN
	        EVAP(I) = PARTLND(I) * EP(I)
	      ENDIF
	    ENDDO
        
        
        
        
        
	    DO I = 1, IM
	      FLAG(I) = SLIMSK(I).NE.0..AND.EP(I).LE.0.
	      DEW(I) = 0.
	    ENDDO
	    DO I = 1, IM
	      IF(FLAG(I)) THEN
	        DEW(I) = -EP(I) * DELT / (HVAP * RHOH2O)
	        EVAP(I) = EP(I)
	        DEW(I) = DEW(I) * PARTLND(I)
	        EVAP(I) = EVAP(I) * PARTLND(I)
	        DM(I) = 1.
	      ENDIF
	    ENDDO
        
        
        
	    DO I = 1, IM
	      FLAG(I) = SLIMSK(I).NE.0..AND.SNOWD(I).GT.0.
	    ENDDO
        
        
        
        
        
	    DO I = 1, IM
	      IF(FLAG(I)) THEN
	        BFACT = SNOWD(I) / (DELT * EP(I) / (HVAP * RHOH2O))
	        BFACT = MIN(BFACT,1._kind_phys)
        
        
        
	        IF(EP(I).LE.0.) BFACT = 1.
	        IF(SNOWD(I).LE..001) THEN
        
        
        
		SNOEVP(I) = bfact * EP(I)
        
		EVAP(I)=EVAP(I)+SNOEVP(I)*(1.-PARTLND(I))
	        ELSE
        
		SNOEVP(I) = bfact * EP(I)
		EVAP(I) = SNOEVP(I)
	        ENDIF
	        TSURF(I) = T1(I) +                                            &
	   &          (RCAP(I) - GFLUX(I) - DFSNOW * (T1(I) - STSOIL(I,1))    &
	   &           /(FACTSNW(I) * MAX(SNOWD(I),.001_kind_phys))           &
        
        
	   &           - SNOEVP(I)) / (RSMALL(I) * RCH(I)                     &
	   &           + DFSNOW / (FACTSNW(I)* MAX(SNOWD(I),.001_kind_phys)))
        
	        SNOWD(I) = SNOWD(I) - SNOEVP(I) * delt / (rhoh2o * hvap)
	        SNOWD(I) = MAX(SNOWD(I),0._kind_phys)
	      ENDIF
	    ENDDO
        
        
        
         500  CONTINUE
	    DO I = 1, IM
	      FLAG(I) = SLIMSK(I).NE.0.                                       &
	   &            .AND.SNOWD(I).GT..0
	    ENDDO
	    DO I = 1, IM
	      IF(FLAG(I).AND.TSURF(I).GT.T0C) THEN
	        SNOWMT(I) = RCH(I) * RSMALL(I) * DELT                         &
	   &              * (TSURF(I) - T0C) / (RHOH2O * HFUS)
	        SNOWMT(I) = min(SNOWMT(I),SNOWD(I))
	        SNOWD(I) = SNOWD(I) - SNOWMT(I)
	        SNOWD(I) = MAX(SNOWD(I),0._kind_phys)
	        TSURF(I) = MAX(T0C,TSURF(I)                                   &
	   &             -HFUS*SNOWMT(I)*RHOH2O/(RCH(I)*RSMALL(I)*DELT))
	      ENDIF
	    ENDDO
        
        
        
        
        
	    DO I = 1, IM
        
	      IF (SNOWD(I) .GT. snomin) THEN
        
	        qss(i) = fpvs(tsurf(i))
	        QSS(I) = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))
	        EVAP(I) = elocp * RCH(I) * (QSS(I) - Q0(I))
	      ENDIF
	    ENDDO
        
        
        
        
        
	    DO I = 1, IM
	      FLAG(I) = SLIMSK(I).EQ.1.
	      if(FLAG(I)) then
	        DF1(I) = FUNCDF(SMCZ(I),SOILTYP(I))
	        KT1(I) = FUNCKT(SMCZ(I),SOILTYP(I))
	      endif
	      if(FLAG(I).and.STC(I,1).lt.t0c) then
	        DF1(I) = 0.
	        KT1(I) = 0.
	      endif
	      IF(FLAG(I)) THEN
	        RHSCNPY(I) = -EC(I) + SIGMAF(I) * RHOH2O * DEW(I) / DELT
	        SMCZ(I) = MAX(SMC(I,1), SMC(I,2))
	        DMDZ(I) = (SMC(I,1) - SMC(I,2)) / (-.5 * ZSOIL(I,2))
	        RHSMC(I,1) = (DF1(I) * DMDZ(I) + KT1(I)                       &
	   &        + (EDIR(I) + ET(I,1))) / (ZSOIL(I,1) * RHOH2O)
	        RHSMC(I,1) = RHSMC(I,1) - (1. - SIGMAF(I)) * DEW(I) /         &
	   &                 ( ZSOIL(I,1) * delt)
	        DDZ(I) = 1. / (-.5 * ZSOIL(I,2))
        
        
        
        
	        AIM(I,1) = 0.
	        BIM(I,1) = DF1(I) * DDZ(I) / (-ZSOIL(I,1) * RHOH2O)
	        CIM(I,1) = -BIM(I,1)
	      ENDIF
	    ENDDO
        
	    DO K = 2, KM
	      IF(K.LT.KM) THEN
	        DO I=1,IM
		IF(FLAG(I)) THEN
		  DF2 = FUNCDF(SMCZ(I),SOILTYP(I))
		  KT2(I) = FUNCKT(SMCZ(I),SOILTYP(I))
		ENDIF
		IF(FLAG(I).and.STC(I,k).lt.t0c) THEN
		  df2 = 0.
		  KT2(I) = 0.
		ENDIF
		IF(FLAG(I)) THEN
		  DMDZ2(I) = (SMC(I,K) - SMC(I,K+1))                        &
	   &                   / (.5 * (ZSOIL(I,K-1) - ZSOIL(I,K+1)))
		  SMCZ(I) = MAX(SMC(I,K), SMC(I,K+1))
		  RHSMC(I,K) = (DF2 * DMDZ2(I) + KT2(I)                     &
	   &             - DF1(I) * DMDZ(I) - KT1(I) + ET(I,K))               &
	   &                     / (RHOH2O*(ZSOIL(I,K) - ZSOIL(I,K-1)))
		  DDZ2(I) = 2. / (ZSOIL(I,K-1) - ZSOIL(I,K+1))
		  CIM(I,K) = -DF2 * DDZ2(I)                                 &
	   &                / ((ZSOIL(I,K-1) - ZSOIL(I,K))*RHOH2O)
		ENDIF
	        ENDDO
	      ELSE
	        DO I = 1, IM
		IF(FLAG(I)) THEN
		  KT2(I) = FUNCKT(SMC(I,K),SOILTYP(I))
		ENDIF
		if(FLAG(I).and.STC(I,k).lt.t0c) KT2(I) = 0.
		IF(FLAG(I)) THEN
		  RHSMC(I,K) = (KT2(I)                                      &
	   &             - DF1(I) * DMDZ(I) - KT1(I) + ET(I,K))               &
	   &                     / (RHOH2O*(ZSOIL(I,K) - ZSOIL(I,K-1)))
		  DRAIN(I) = KT2(I)
		  CIM(I,K) = 0.
		ENDIF
	        ENDDO
	      ENDIF
	      DO I = 1, IM
	        IF(FLAG(I)) THEN
		AIM(I,K) = -DF1(I) * DDZ(I)                                 &
	   &                / ((ZSOIL(I,K-1) - ZSOIL(I,K))*RHOH2O)
		BIM(I,K) = -(AIM(I,K) + CIM(I,K))
		DF1(I) = DF2
		KT1(I) = KT2(I)
		DMDZ(I) = DMDZ2(I)
		DDZ(I) = DDZ2(I)
	        ENDIF
	      ENDDO
	    ENDDO
        
         600  CONTINUE
        
        
        
	    DO I=1,IM
	      FLAG(I) = SLIMSK(I).NE.0.
	    ENDDO
        
        
        
	    DO I=1,IM
        
	      IF(FLAG(I).AND..NOT.FLAGSNW(I)) THEN
	        YY(I) = T1(I) +                                               &
        
	   &          (RCAP(I)-GFLUX(I)                                       &
	   &           - EVAP(I)) / (RSMALL(I) * RCH(I))
	        ZZ(I) = 1. + DFT0(I) / (-.5 * ZSOIL(I,1) * RCH(I) * RSMALL(I))
	        XX(I) = DFT0(I) * (STSOIL(I,1) - YY(I)) /                     &
	   &            (.5 * ZSOIL(I,1) * ZZ(I))
	      ENDIF
        
	      IF(FLAG(I).AND.FLAGSNW(I)) THEN
	        YY(I) = STSOIL(I,1)
        
        
        
	        ZZ(I) = 1.
	        XX(I) = DFSNOW * (STSOIL(I,1) - TSURF(I))                     &
	   &            / (-FACTSNW(I) * MAX(SNOWD(I),.001_kind_phys))
	      ENDIF
	    ENDDO
        
        
        
        
        
	    DO I = 1, IM
	      IF(FLAG(I)) THEN
	        SMCZ(I) = MAX(SMC(I,1), SMC(I,2))
	        DTDZ1(I) = (STSOIL(I,1) - STSOIL(I,2)) / (-.5 * ZSOIL(I,2))
	        IF(SLIMSK(I).EQ.1.) THEN
		DFT1(I) = KTSOIL(SMCZ(I),SOILTYP(I))
		HCPCT(I) = SMC(I,1) * CH2O + (1. - SMC(I,1)) * CSOIL
	        ELSE
		DFT1(I) = DFT0(I)
		HCPCT(I) = CICE
	        ENDIF
	        DFT2(I) = DFT1(I)
	        DDZ(I) = 1. / (-.5 * ZSOIL(I,2))
        
        
        
        
	        AI(I,1) = 0.
	        BI(I,1) = DFT1(I) * DDZ(I) / (-ZSOIL(I,1) * HCPCT(I))
	        CI(I,1) = -BI(I,1)
	        BI(I,1) = BI(I,1)                                             &
	   &            + DFT0(I) / (.5 * ZSOIL(I,1) **2 * HCPCT(I) * ZZ(I))
        
        
        
	        RHSTC(I,1) = (DFT1(I) * DTDZ1(I) - XX(I))                     &
	   &                 / (ZSOIL(I,1) * HCPCT(I))
	      ENDIF
	    ENDDO
        
	    DO K = 2, KM
	      DO I=1,IM
	        IF(SLIMSK(I).EQ.1.) THEN
		HCPCT(I) = SMC(I,K) * CH2O + (1. - SMC(I,K)) * CSOIL
	        ELSEIF(SLIMSK(I).EQ.2.) THEN
		HCPCT(I) = CICE
	        ENDIF
	      ENDDO
	      IF(K.LT.KM) THEN
	        DO I = 1, IM
		IF(FLAG(I)) THEN
		  DTDZ2(I) = (STSOIL(I,K) - STSOIL(I,K+1))                  &
	   &                   / (.5 * (ZSOIL(I,K-1) - ZSOIL(I,K+1)))
		  SMCZ(I) = MAX(SMC(I,K), SMC(I,K+1))
		  IF(SLIMSK(I).EQ.1.) THEN
		    DFT2(I) = KTSOIL(SMCZ(I),SOILTYP(I))
		  ENDIF
		  DDZ2(I) = 2. / (ZSOIL(I,K-1) - ZSOIL(I,K+1))
		  CI(I,K) = -DFT2(I) * DDZ2(I)                              &
	   &                / ((ZSOIL(I,K-1) - ZSOIL(I,K)) * HCPCT(I))
		ENDIF
	        ENDDO
	      ELSE
        
        
        
	        DO I = 1, IM
		IF(SLIMSK(I).EQ.1.) THEN
		  DTDZ2(I) = (STSOIL(I,K) - TG3(I))                         &
	   &              / (.5 * (ZSOIL(I,K-1) + ZSOIL(I,K)) - ZBOT)
		  DFT2(I) = KTSOIL(SMC(I,K),SOILTYP(I))
		  CI(I,K) = 0.
		ENDIF
		IF(SLIMSK(I).EQ.2.) THEN
		  DTDZ2(I) = (STSOIL(I,K) - TGICE)                          &
	   &                   / (.5 * ZSOIL(I,K-1) - .5 * ZSOIL(I,K))
		  DFT2(I) = DFT1(I)
		  CI(I,K) = 0.
		ENDIF
	        ENDDO
	      ENDIF
	      DO I = 1, IM
	        IF(FLAG(I)) THEN
		RHSTC(I,K) = (DFT2(I) * DTDZ2(I) - DFT1(I) * DTDZ1(I))      &
	   &                 / ((ZSOIL(I,K) - ZSOIL(I,K-1)) * HCPCT(I))
		AI(I,K) = -DFT1(I) * DDZ(I)                                 &
	   &                / ((ZSOIL(I,K-1) - ZSOIL(I,K)) * HCPCT(I))
		BI(I,K) = -(AI(I,K) + CI(I,K))
		DFT1(I) = DFT2(I)
		DTDZ1(I) = DTDZ2(I)
		DDZ(I) = DDZ2(I)
	        ENDIF
	      ENDDO
	    ENDDO
        
         700  CONTINUE
        
        
        
	    DO K = 1, KM
	      DO I=1,IM
	        IF(FLAG(I))  THEN
		RHSTC(I,K) = RHSTC(I,K) * DELT2
		AI(I,K) = AI(I,K) * DELT2
		BI(I,K) = 1. + BI(I,K) * DELT2
		CI(I,K) = CI(I,K) * DELT2
	        ENDIF
	      ENDDO
	    ENDDO
        
	    DO I=1,IM
	      IF(FLAG(I)) THEN
	        CI(I,1) = -CI(I,1) / BI(I,1)
	        RHSTC(I,1) = RHSTC(I,1) / BI(I,1)
	      ENDIF
	    ENDDO
        
	    DO K = 2, KM
	      DO I=1,IM
	        IF(FLAG(I)) THEN
		CC = 1. / (BI(I,K) + AI(I,K) * CI(I,K-1))
		CI(I,K) = -CI(I,K) * CC
		RHSTC(I,K) = (RHSTC(I,K) - AI(I,K) * RHSTC(I,K-1)) * CC
	        ENDIF
	      ENDDO
	    ENDDO
        
        
	    DO I=1,IM
	      IF(FLAG(I)) THEN
	        CI(I,KM) = RHSTC(I,KM)
	      ENDIF
	    ENDDO
        
	    DO K = KM-1, 1
	      DO I=1,IM
	        IF(FLAG(I)) THEN
		CI(I,K) = CI(I,K) * CI(I,K+1) + RHSTC(I,K)
	        ENDIF
	      ENDDO
	    ENDDO
        
        
        
	    DO K = 1, KM
	      DO I=1,IM
	        IF(FLAG(I)) THEN
		STSOIL(I,K) = STSOIL(I,K) + CI(I,K)
	        ENDIF
	      ENDDO
	    ENDDO
        
        
        
	    DO I=1,IM
        
	      IF(SLIMSK(I).NE.0..AND..NOT.FLAGSNW(I)) THEN
	        TSURF(I) = (YY(I) + (ZZ(I) - 1.) * STSOIL(I,1)) / ZZ(I)
	      ENDIF
        
	      IF(SLIMSK(I).EQ.2..AND..NOT.FLAGSNW(I)) THEN
	        TSURF(I) = MIN(TSURF(I),T0C)
	      ENDIF
	    ENDDO
        
	    DO K = 1, KM
	      DO I=1,IM
	        IF(SLIMSK(I).EQ.2) THEN
		STSOIL(I,K) = MIN(STSOIL(I,K),T0C)
	        ENDIF
	      ENDDO
	    ENDDO
        
        
        
	    DO I=1,IM
	      IF(SLIMSK(I).NE.0.) THEN
	        TSKIN(I) = CTFIL1 * TSURF(I) + CTFIL2 * TSKIN(I)
	      ENDIF
	    ENDDO
	    DO K = 1, KM
	      DO I=1,IM
	        IF(SLIMSK(I).NE.0.) THEN
		STC(I,K) = CTFIL1 * STSOIL(I,K) + CTFIL2 * STC(I,K)
	        ENDIF
	      ENDDO
	    ENDDO
        
        
        
	    DO I=1,IM
	      FLAG(I) = SLIMSK(I).NE.0.                                       &
        
	   &            .AND.FLAGSNW(I)
	    ENDDO
	    DO I = 1, IM
	      IF(FLAG(I)) THEN
	        GFLUX(I) = -DFSNOW * (TSKIN(I) - STC(I,1))                    &
	   &               / (FACTSNW(I) * MAX(SNOWD(I),.001_kind_phys))
	      ENDIF
	    ENDDO
	    DO I = 1, IM
        
	      IF( SLIMSK(I).NE.0..AND..NOT.FLAGSNW(I)) THEN
	        GFLUX(I) = DFT0(I) * (STC(I,1) - TSKIN(I))                    &
	   &               / (-.5 * ZSOIL(I,1))
	      ENDIF
	    ENDDO


5555  CONTINUE

        
        
        



        
        
        



        
        
        


        

      DO I = 1, IM
        F10M(I) = FM10(I) / FM(I)
        F10M(I) = min(F10M(I),1._kind_phys)
        U10M(I) = F10M(I) * XRCL(I) * U1(I)
        V10M(I) = F10M(I) * XRCL(I) * V1(I)




























      ENDDO




















 6000 FORMAT(8(F8.2,','))








      RETURN
      END SUBROUTINE PROGTM






      SUBROUTINE PROGT2(IM,KM,RHSCNPY,                                  &
     &                  RHSMC,AI,BI,CI,SMC,SLIMSK,                      &
     &                  CANOPY,PRECIP,RUNOFF,SNOWMT,                    &
     &                  ZSOIL,SOILTYP,SIGMAF,DELT,me)

      USE MODULE_GFS_MACHINE     , ONLY : kind_phys
      implicit none
      integer              km, IM, me
      real(kind=kind_phys) delt
      real(kind=kind_phys) RHSCNPY(IM),  RHSMC(IM,KM), AI(IM,KM),       &
     &                     BI(IM,KM),    CI(IM,KM),    SMC(IM,KM),      &
     &                     SLIMSK(IM),   CANOPY(IM),   PRECIP(IM),      &
     &                     RUNOFF(IM),   SNOWMT(IM),   ZSOIL(IM,KM),    &
     &                     SIGMAF(IM)
      INTEGER SOILTYP(IM)

      integer              k, lond, i
      real(kind=kind_phys) CNPY(IM), PRCP(IM),   TSAT(IM),              &
     &                     INF(IM),  INFMAX(IM), SMSOIL(IM,KM)

      real(kind=kind_phys) cc,    ctfil1, ctfil2, delt2,                &
     &                     drip,  rffact, rhoh2o,                       &

     &                     rzero, scanop, tdif, KSAT

      LOGICAL FLAG(IM)

      PARAMETER (SCANOP=.5, RHOH2O=1000.)
      PARAMETER (CTFIL1=.5, CTFIL2=1.-CTFIL1)

      PARAMETER (RFFACT=.15)


      LOND = 353
      DELT2 = DELT * 2.



      DO I=1,IM
        PRCP(I) = RHOH2O * (PRECIP(I)+SNOWMT(I)) / DELT
        RUNOFF(I) = 0.
        CNPY(I) = CANOPY(I)
      ENDDO






      DO I=1,IM
        IF(SLIMSK(I).EQ.1.) THEN
          RHSCNPY(I) = RHSCNPY(I) + SIGMAF(I) * PRCP(I)
          CANOPY(I) = CANOPY(I) + DELT * RHSCNPY(I)
          CANOPY(I) = MAX(CANOPY(I),0._kind_phys)
          PRCP(I) = PRCP(I) * (1. - SIGMAF(I))
          IF(CANOPY(I).GT.SCANOP) THEN
            DRIP = CANOPY(I) - SCANOP
            CANOPY(I) = SCANOP
            PRCP(I) = PRCP(I) + DRIP / DELT
          ENDIF



          INF(I) = PRCP(I)
          TSAT(I) = THSAT(SOILTYP(I))





          INFMAX(I) = (-ZSOIL(I,1)) *                                   &
     &                ((TSAT(I) - SMC(I,1)) / DELT - RHSMC(I,1))        &
     &                * RHOH2O
          INFMAX(I) = MAX(RFFACT*INFMAX(I),0._kind_phys)


          IF(INF(I).GT.INFMAX(I)) THEN
            RUNOFF(I) = INF(I) - INFMAX(I)
            INF(I) = INFMAX(I)
          ENDIF
          INF(I) = INF(I) / RHOH2O
          RHSMC(I,1) = RHSMC(I,1) - INF(I) / ZSOIL(I,1)
        ENDIF
      ENDDO









      DO I=1,IM
        FLAG(I) = SLIMSK(I).EQ.1.
      ENDDO




      DO K = 1, KM
        DO I=1,IM
          IF(FLAG(I))  THEN
            RHSMC(I,K) = RHSMC(I,K) * DELT2
            AI(I,K) = AI(I,K) * DELT2
            BI(I,K) = 1. + BI(I,K) * DELT2
            CI(I,K) = CI(I,K) * DELT2
          ENDIF
        ENDDO
      ENDDO

      DO I=1,IM
        IF(FLAG(I)) THEN
          CI(I,1) = -CI(I,1) / BI(I,1)
          RHSMC(I,1) = RHSMC(I,1) / BI(I,1)
        ENDIF
      ENDDO
      DO K = 2, KM
        DO I=1,IM
          IF(FLAG(I)) THEN
            CC = 1. / (BI(I,K) + AI(I,K) * CI(I,K-1))
            CI(I,K) = -CI(I,K) * CC
            RHSMC(I,K)=(RHSMC(I,K)-AI(I,K)*RHSMC(I,K-1))*CC
          ENDIF
        ENDDO
      ENDDO

      DO I=1,IM
        IF(FLAG(I)) THEN
          CI(I,KM) = RHSMC(I,KM)
        ENDIF
      ENDDO

      DO K = KM-1, 1
        DO I=1,IM
          IF(FLAG(I)) THEN
            CI(I,K) = CI(I,K) * CI(I,K+1) + RHSMC(I,K)
          ENDIF
        ENDDO
      ENDDO
 100  CONTINUE



      DO K = 1, KM
        DO I=1,IM
          IF(FLAG(I)) THEN
            SMSOIL(I,K) = SMC(I,K) + CI(I,K)
            SMSOIL(I,K) = MAX(SMSOIL(I,K),0._kind_phys)
            TDIF = MAX(SMSOIL(I,K) - TSAT(I),0._kind_phys)
            RUNOFF(I) = RUNOFF(I) -                                     &
     &                RHOH2O * TDIF * ZSOIL(I,K) / DELT
            SMSOIL(I,K) = SMSOIL(I,K) - TDIF
          ENDIF
        ENDDO
      ENDDO
      DO K = 1, KM
        DO I=1,IM
          IF(FLAG(I)) THEN
            SMC(I,K) = CTFIL1 * SMSOIL(I,K) + CTFIL2 * SMC(I,K)
          ENDIF
        ENDDO
      ENDDO







      RETURN
      END SUBROUTINE PROGT2
      FUNCTION KTSOIL(THETA,KTYPE)

      USE MODULE_GFS_MACHINE     , ONLY : kind_phys
      USE module_progtm , ONLY : TSAT, DFKT
      implicit none
      integer              ktype,kw
      real(kind=kind_phys) ktsoil, theta, w

      W = (THETA / TSAT(KTYPE)) * 20. + 1.
      KW = W
      KW = MIN(KW,21)
      KW = MAX(KW,1)
      KTSOIL = DFKT(KW,KTYPE)                                           &
     &         + (W - KW) * (DFKT(KW+1,KTYPE) - DFKT(KW,KTYPE))
      RETURN
      END FUNCTION KTSOIL
      FUNCTION FUNCDF(THETA,KTYPE)

      USE MODULE_GFS_MACHINE     , ONLY : kind_phys
      USE module_progtm , ONLY : TSAT, DFK
      implicit none
      integer              ktype,kw
      real(kind=kind_phys) funcdf,theta,w

      W = (THETA / TSAT(KTYPE)) * 20. + 1.
      KW = W
      KW = MIN(KW,21)
      KW = MAX(KW,1)
      FUNCDF = DFK(KW,KTYPE)                                            &
     &         + (W - KW) * (DFK(KW+1,KTYPE) - DFK(KW,KTYPE))
      RETURN
      END FUNCTION FUNCDF
      FUNCTION FUNCKT(THETA,KTYPE)

      USE MODULE_GFS_MACHINE     , ONLY : kind_phys
      USE module_progtm , ONLY : TSAT, KTK
      implicit none
      integer             ktype,kw
      real(kind=kind_phys) funckt,theta,w

      W = (THETA / TSAT(KTYPE)) * 20. + 1.
      KW = W
      KW = MIN(KW,21)
      KW = MAX(KW,1)
      FUNCKT = KTK(KW,KTYPE)                                            &
     &         + (W - KW) * (KTK(KW+1,KTYPE) - KTK(KW,KTYPE))
      RETURN
      END FUNCTION FUNCKT
      FUNCTION THSAT(KTYPE)

      USE MODULE_GFS_MACHINE     , ONLY : kind_phys
      USE module_progtm , ONLY : TSAT
      implicit none
      integer             ktype
      real(kind=kind_phys) thsat

      THSAT = TSAT(KTYPE)
      RETURN
      END FUNCTION THSAT
      FUNCTION TWLT(KTYPE)

      USE MODULE_GFS_MACHINE     , ONLY : kind_phys

      implicit none
      integer              ktype
      real(kind=kind_phys) twlt

      TWLT = .1
      RETURN
      END FUNCTION TWLT

 END MODULE module_sf_gfs
