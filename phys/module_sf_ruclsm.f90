

MODULE module_sf_ruclsm













  USE module_model_constants
  USE module_wrf_error


        INTEGER :: LUCATS , BARE, NATURAL, CROP, URBAN
        integer, PARAMETER :: NLUS=50
        CHARACTER*8 LUTYPE
        INTEGER, DIMENSION(1:NLUS) :: IFORTBL
        real, dimension(1:NLUS) ::  SNUPTBL, RSTBL, RGLTBL, HSTBL, LAITBL,         &
                                    ALBTBL, Z0TBL, LEMITBL, PCTBL, SHDTBL, MAXALB
        REAL ::   TOPT_DATA,CMCMAX_DATA,CFACTR_DATA,RSMAX_DATA

        INTEGER :: SLCATS
        INTEGER, PARAMETER :: NSLTYPE=30
        CHARACTER*8 SLTYPE
        REAL, DIMENSION (1:NSLTYPE) :: BB,DRYSMC,HC,                           &
        MAXSMC, REFSMC,SATPSI,SATDK,SATDW, WLTSMC,QTZ


        INTEGER :: SLPCATS
        INTEGER, PARAMETER :: NSLOPE=30
        REAL, DIMENSION (1:NSLOPE) :: SLOPE_DATA
        REAL ::  SBETA_DATA,FXEXP_DATA,CSOIL_DATA,SALP_DATA,REFDK_DATA,           &
                 REFKDT_DATA,FRZK_DATA,ZBOT_DATA,  SMLOW_DATA,SMHIGH_DATA,        &
                        CZIL_DATA

        CHARACTER*256  :: err_message


CONTAINS


    SUBROUTINE LSMRUC(spp_lsm,                                   &
                   pattern_spp_lsm,field_sf,                     &
                   DT,KTAU,NSL,                                  &
                   lakemodel,lakemask,                           &
                   graupelncv,snowncv,rainncv,                   &
                   ZS,RAINBL,SNOW,SNOWH,SNOWC,FRZFRAC,frpcpn,    &
                   rhosnf,precipfr,                              & 
                   Z3D,P8W,T3D,QV3D,QC3D,RHO3D,                  & 
                   GLW,GSW,EMISS,CHKLOWQ, CHS,                   & 
                   FLQC,FLHC,MAVAIL,CANWAT,VEGFRA,ALB,ZNT,       &
                   Z0,SNOALB,ALBBCK,LAI,                         &  
                   mminlu, landusef, nlcat, mosaic_lu,           &
                   mosaic_soil, soilctop, nscat,                 &  
                   QSFC,QSG,QVG,QCG,DEW,SOILT1,TSNAV,            &
                   TBOT,IVGTYP,ISLTYP,XLAND,                     &
                   ISWATER,ISICE,XICE,XICE_THRESHOLD,            &
                   CP,ROVCP,G0,LV,STBOLT,                        &
                   SOILMOIS,SH2O,SMAVAIL,SMMAX,                  &
                   TSO,SOILT,HFX,QFX,LH,                         &
                   SFCRUNOFF,UDRUNOFF,ACRUNOFF,SFCEXC,           &
                   SFCEVP,GRDFLX,SNOWFALLAC,ACSNOW,SNOM,         &
                   SMFR3D,KEEPFR3DFLAG,                          &
                   myjpbl,shdmin,shdmax,rdlai2d,                 &
                   ids,ide, jds,jde, kds,kde,                    &
                   ims,ime, jms,jme, kms,kme,                    &
                   its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE














































































   INTEGER,     PARAMETER            ::     nvegclas=24+3

   REAL,       INTENT(IN   )    ::     DT
   LOGICAL,    INTENT(IN   )    ::     myjpbl,frpcpn
   INTEGER,    INTENT(IN   )    ::     spp_lsm
   INTEGER,    INTENT(IN   )    ::     NLCAT, NSCAT, mosaic_lu, mosaic_soil
   INTEGER,    INTENT(IN   )    ::     ktau, nsl, isice, iswater, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ids,ide, jds,jde, kds,kde, &
                                       its,ite, jts,jte, kts,kte

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),OPTIONAL::    pattern_spp_lsm
   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),OPTIONAL::    field_sf
   REAL,    DIMENSION( ims:ime, 1  :nsl, jms:jme )         ::    field_sf_loc

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
            INTENT(IN   )    ::                           QV3D, &
                                                          QC3D, &
                                                           p8w, &
                                                         rho3D, &
                                                           T3D, &
                                                           z3D

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                       RAINBL, &
                                                            GLW, &
                                                            GSW, &
                                                         ALBBCK, &
                                                           FLHC, &
                                                           FLQC, &
                                                           CHS , &
                                                           XICE, &
                                                          XLAND, &


                                                           TBOT


   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(INOUT   )    ::                       VEGFRA


   REAL,       OPTIONAL, DIMENSION( ims:ime , jms:jme ),         &
               INTENT(IN   )    ::                   GRAUPELNCV, &
                                                        SNOWNCV, &
                                                        RAINNCV
   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                     lakemask
   INTEGER,    INTENT(IN   )    ::                    LakeModel

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMAX
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMIN
   LOGICAL, intent(in) :: rdlai2d

   REAL,       DIMENSION( 1:nsl), INTENT(IN   )      ::      ZS

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(INOUT)    ::                               &
                                                           SNOW, &
                                                          SNOWH, &
                                                          SNOWC, &
                                                         CANWAT, & 
                                                         SNOALB, &
                                                            ALB, &
                                                          EMISS, &
                                                            LAI, &
                                                         MAVAIL, & 
                                                         SFCEXC, &
                                                            Z0 , &
                                                            ZNT

   REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                               &
                                                        FRZFRAC

   INTEGER,    DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(IN   )    ::                       IVGTYP, &
                                                         ISLTYP
   CHARACTER(LEN=*), INTENT(IN   )    ::                 MMINLU
   REAL,     DIMENSION( ims:ime , 1:nlcat, jms:jme ), INTENT(IN):: LANDUSEF
   REAL,     DIMENSION( ims:ime , 1:nscat, jms:jme ), INTENT(IN):: SOILCTOP

   REAL, INTENT(IN   )          ::         CP,ROVCP,G0,LV,STBOLT,XICE_threshold
 
   REAL,       DIMENSION( ims:ime , 1:nsl, jms:jme )           , &
               INTENT(INOUT)    ::                 SOILMOIS,SH2O,TSO

   REAL,       DIMENSION( ims:ime, jms:jme )                   , &
               INTENT(INOUT)    ::                        SOILT, &
                                                            HFX, &
                                                            QFX, &
                                                             LH, &
                                                         SFCEVP, &
                                                      SFCRUNOFF, &
                                                       UDRUNOFF, &
                                                       ACRUNOFF, &
                                                         GRDFLX, &
                                                         ACSNOW, &
                                                           SNOM, &
                                                            QVG, &
                                                            QCG, &
                                                            DEW, &
                                                           QSFC, &
                                                            QSG, &
                                                        CHKLOWQ, &
                                                         SOILT1, &
                                                          TSNAV

   REAL,       DIMENSION( ims:ime, jms:jme )                   , & 
               INTENT(INOUT)    ::                      SMAVAIL, &
                                                          SMMAX

   REAL,       DIMENSION( its:ite, jts:jte )    ::               &
                                                             PC, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                         EMISSL, &
                                                           ZNTL, &
                                                        LMAVAIL, &
                                                          SMELT, &
                                                           SNOH, &
                                                          SNFLX, &
                                                           EDIR, &
                                                             EC, &
                                                            ETT, &
                                                         SUBLIM, &
                                                           sflx, &
                                                            smf, &
                                                          EVAPL, &
                                                          PRCPL, &
                                                         SEAICE, &
                                                        INFILTR

   REAL,       DIMENSION( its:ite, jts:jte )    ::               &
                                                         budget, &
                                                       acbudget, &
                                                    waterbudget, &
                                                  acwaterbudget, &
                                                       smtotold, &
                                                        snowold, &
                                                      canwatold


   REAL,       DIMENSION( ims:ime, 1:nsl, jms:jme)               &
                                             ::    KEEPFR3DFLAG, &
                                                         SMFR3D

   REAL,       DIMENSION( ims:ime, jms:jme ), INTENT(OUT)     :: &
                                                         RHOSNF, & 
                                                       PRECIPFR, & 
                                                     SNOWFALLAC

   REAL                                                          &
                             ::                           RHOCS, &
                                                       RHONEWSN, &
                                                          RHOSN, &
                                                      RHOSNFALL, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT, &
                                                        CANWATR, &
                                                       SNOWFRAC, &
                                                          SNHEI, &
                                                           SNWE

   REAL                                      ::              CN, &
                                                         SAT,CW, &
                                                           C1SN, &
                                                           C2SN, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT


   REAL,     DIMENSION(1:NSL)                ::          ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:2*(nsl-2))          ::           DTDZS

   REAL,     DIMENSION(1:5001)               ::             TBQ


   REAL,     DIMENSION( 1:nsl )              ::         SOILM1D, & 
                                                          TSO1D, &
                                                        SOILICE, &
                                                        SOILIQW, &
                                                       SMFRKEEP

   REAL,     DIMENSION( 1:nsl )              ::          KEEPFR
                                                
   REAL,     DIMENSION( 1:nlcat )            ::          lufrac
   REAL,     DIMENSION( 1:nscat )            ::          soilfrac

   REAL                           ::                        RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT

   REAL                           ::                     PRCPMS, &
                                                        NEWSNMS, &
                                                      prcpncliq, &
                                                       prcpncfr, &
                                                      prcpculiq, &
                                                       prcpcufr, &
                                                           PATM, &
                                                          PATMB, &
                                                           TABS, &
                                                          QVATM, &
                                                          QCATM, &
                                                          Q2SAT, &
                                                         CONFLX, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS, &
                                                        snowrat, &
                                                       grauprat, &
                                                       graupamt, &
                                                         icerat, &
                                                          curat, &
                                                       INFILTRP
   REAL      ::  cq,r61,r273,arp,brp,x,evs,eis
   REAL      ::  cropsm

   REAL      ::  meltfactor, ac,as, wb
   INTEGER   ::  NROOT
   INTEGER   ::  ILAND,ISOIL,IFOREST
 
   INTEGER   ::  I,J,K,NZS,NZS1,NDDZS
   INTEGER   ::  k1,l,k2,kp,km
   CHARACTER (LEN=132) :: message

   REAL,DIMENSION(ims:ime,1:nsl,jms:jme) :: rstoch 

   REAL,DIMENSION(ims:ime,jms:jme)::EMISSO,VEGFRAO,ALBO,SNOALBO
   REAL,DIMENSION(its:ite,jts:jte)::EMISSLO


         NZS=NSL
         NDDZS=2*(nzs-2)

         rstoch=0.0
         field_sf_loc=0.0

       if (spp_lsm==1) then
         do J=jts,jte
           do i=its,ite
             do k=1,nsl
               rstoch(i,k,j) = pattern_spp_lsm(i,k,j)
               field_sf_loc(i,k,j)=field_sf(i,k,j)
             enddo
           enddo
         enddo 
       endif  

        CQ=173.15-.05
        R273=1./273.15
        R61=6.1153*0.62198
        ARP=77455.*41.9/461.525
        BRP=64.*41.9/461.525

        DO K=1,5001
          CQ=CQ+.05

        EVS=EXP(17.67*(CQ-273.15)/(CQ-29.65))
        EIS=EXP(22.514-6.15E3/CQ)
        if(CQ.ge.273.15) then

        tbq(k) = R61*evs
        else
        tbq(k) = R61*eis
        endif

        END DO




     if(ktau.eq.1) then
     DO J=jts,jte
         DO i=its,ite
            do k=1,nsl
       keepfr3dflag(i,k,j)=0.
            enddo



           snowc(i,j) = min(1.,snow(i,j)/32.)
          if(snow(i,j).le.32.) soilt1(i,j)=tso(i,1,j)

        IF((soilt1(i,j) .LT. 170.) .or. (soilt1(i,j) .GT.400.)) THEN
            IF(snow(i,j).gt.32.) THEN
           soilt1(i,j)=0.5*(soilt(i,j)+tso(i,1,j))
    IF ( wrf_at_debug_level(3000) ) THEN
        WRITE ( message , FMT='(A,F8.3,2I6)' ) &
       'Temperature inside snow is initialized in RUCLSM ', soilt1(i,j),i,j
        CALL wrf_debug ( 0 , message )
    ENDIF
            ELSE
           soilt1(i,j) = tso(i,1,j)
            ENDIF
        ENDIF
           tsnav(i,j) =0.5*(soilt(i,j)+tso(i,1,j))-273.15
           qcg  (i,j) =0.
           patmb=P8w(i,kms,j)*1.e-2
           QSG  (i,j) = QSN(SOILT(i,j),TBQ)/PATMB
        IF((qvg(i,j) .LE. 0.) .or. (qvg(i,j) .GT.0.1)) THEN
           qvg  (i,j) = QSG(i,j)*mavail(i,j)
          IF ( wrf_at_debug_level(3000) ) THEN
           WRITE ( message , FMT='(A,3F8.3,2I6)' ) &
          'QVG is initialized in RUCLSM ', qvg(i,j),mavail(i,j),qsg(i,j),i,j
           CALL wrf_debug ( 0 , message )
          ENDIF
        ENDIF
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))
           SMELT(i,j) = 0.
           SNOM (i,j) = 0.
           SNOWFALLAC(i,j) = 0.
           PRECIPFR(i,j) = 0.
           RHOSNF(i,j) = -1.e3 
           SNFLX(i,j) = 0.
           DEW  (i,j) = 0.
           PC   (i,j) = 0.
           zntl (i,j) = 0.
           RUNOFF1(i,j) = 0.
           RUNOFF2(i,j) = 0.
           SFCRUNOFF(i,j) = 0.
           UDRUNOFF(i,j) = 0.
           ACRUNOFF(i,j) = 0.
           emissl (i,j) = 0.
           budget(i,j) = 0.
           acbudget(i,j) = 0.
           waterbudget(i,j) = 0.
           acwaterbudget(i,j) = 0.
           smtotold(i,j)=0.
           canwatold(i,j)=0.






           chklowq(i,j) = 1.
           infiltr(i,j) = 0.
           snoh  (i,j) = 0.
           edir  (i,j) = 0.
           ec    (i,j) = 0.
           ett   (i,j) = 0.
           sublim(i,j) = 0.
           sflx  (i,j) = 0.
           smf   (i,j) = 0.
           evapl (i,j) = 0.
           prcpl (i,j) = 0.
         ENDDO
     ENDDO

        do k=1,nsl
           soilice(k)=0.
           soiliqw(k)=0.
        enddo
     endif



        PRCPMS = 0.
        newsnms = 0.
        prcpncliq = 0.
        prcpculiq = 0.
        prcpncfr = 0.
        prcpcufr = 0.


   DO J=jts,jte

      DO i=its,ite

    IF ( wrf_at_debug_level(3000) ) THEN
      print *,' IN LSMRUC ','ims,ime,jms,jme,its,ite,jts,jte,nzs', &
                ims,ime,jms,jme,its,ite,jts,jte,nzs
      print *,' IVGTYP, ISLTYP ', ivgtyp(i,j),isltyp(i,j)
      print *,' MAVAIL ', mavail(i,j)
      print *,' SOILT,QVG,P8w',soilt(i,j),qvg(i,j),p8w(i,1,j)
      print *, 'LSMRUC, I,J,xland, QFX,HFX from SFCLAY',i,j,xland(i,j), &
                  qfx(i,j),hfx(i,j)
      print *, ' GSW, GLW =',gsw(i,j),glw(i,j)
      print *, 'SOILT, TSO start of time step =',soilt(i,j),(tso(i,k,j),k=1,nsl)
      print *, 'SOILMOIS start of time step =',(soilmois(i,k,j),k=1,nsl)
      print *, 'SMFROZEN start of time step =',(smfr3d(i,k,j),k=1,nsl)
      print *, ' I,J=, after SFCLAY CHS,FLHC ',i,j,chs(i,j),flhc(i,j)
      print *, 'LSMRUC, IVGTYP,ISLTYP,ALB = ', ivgtyp(i,j),isltyp(i,j),alb(i,j),i,j
      print *, 'LSMRUC  I,J,DT,RAINBL =',I,J,dt,RAINBL(i,j)
      print *, 'XLAND ---->, ivgtype,isoiltyp,i,j',xland(i,j),ivgtyp(i,j),isltyp(i,j),i,j
    ENDIF


         ILAND     = IVGTYP(i,j)
         ISOIL     = ISLTYP(I,J)
         TABS      = T3D(i,kms,j)
         QVATM     = QV3D(i,kms,j)
         QCATM     = QC3D(i,kms,j)
         PATM      = P8w(i,kms,j)*1.e-5



         CONFLX    = Z3D(i,kms,j)*0.5
         RHO       = RHO3D(I,kms,J)

         snowrat = 0.
         grauprat = 0.
         icerat = 0.
         curat = 0.
       IF(FRPCPN) THEN
         prcpncliq = rainncv(i,j)*(1.-frzfrac(i,j))
         prcpncfr = rainncv(i,j)*frzfrac(i,j)



       if(frzfrac(i,j) > 0..and. tabs < 273.) then
         prcpculiq = max(0.,(rainbl(i,j)-rainncv(i,j))*(1.-frzfrac(i,j)))
         prcpcufr = max(0.,(rainbl(i,j)-rainncv(i,j))*frzfrac(i,j))
       else
          if(tabs < 273.) then
            prcpcufr = max(0.,(rainbl(i,j)-rainncv(i,j)))
            prcpculiq = 0.
          else
            prcpcufr = 0.
            prcpculiq = max(0.,(rainbl(i,j)-rainncv(i,j)))
          endif  
       endif  

         PRCPMS   = (prcpncliq + prcpculiq)/DT*1.e-3
         NEWSNMS  = (prcpncfr + prcpcufr)/DT*1.e-3

         IF ( PRESENT( graupelncv ) ) THEN
             graupamt = graupelncv(i,j)
         ELSE
             graupamt = 0.
         ENDIF

         if((prcpncfr + prcpcufr) > 0.) then

         snowrat=min(1.,max(0.,snowncv(i,j)/(prcpncfr + prcpcufr)))
         grauprat=min(1.,max(0.,graupamt/(prcpncfr + prcpcufr)))
         icerat=min(1.,max(0.,(prcpncfr-snowncv(i,j)-graupamt) &
               /(prcpncfr + prcpcufr)))
         curat=min(1.,max(0.,(prcpcufr/(prcpncfr + prcpcufr))))
         endif

       ELSE  
          if (tabs.le.273.15) then
         PRCPMS    = 0.
         NEWSNMS   = RAINBL(i,j)/DT*1.e-3


         snowrat = 1.
          else
         PRCPMS    = RAINBL(i,j)/DT*1.e-3
         NEWSNMS   = 0.
          endif
       ENDIF



          precipfr(i,j) = NEWSNMS * DT *1.e3


         QKMS=FLQC(I,J)/RHO/MAVAIL(I,J)

         TKMS=FLHC(I,J)/RHO/(CP*(1.+0.84*QVATM))  

         SNWE=SNOW(I,J)*1.E-3
         SNHEI=SNOWH(I,J)
         CANWATR=CANWAT(I,J)*1.E-3

         SNOWFRAC=SNOWC(I,J)
         RHOSNFALL=RHOSNF(I,J)

         snowold(i,j)=snwe

             zsmain(1)=0.
             zshalf(1)=0.
          do k=2,nzs
             zsmain(k)= zs(k)
             zshalf(k)=0.5*(zsmain(k-1) + zsmain(k))
          enddo

          do k=1,nlcat
             lufrac(k) = landusef(i,k,j)
          enddo
          do k=1,nscat
             soilfrac(k) = soilctop(i,k,j)
          enddo




        NZS1=NZS-1

    IF ( wrf_at_debug_level(3000) ) THEN
         print *,' DT,NZS1, ZSMAIN, ZSHALF --->', dt,nzs1,zsmain,zshalf
    ENDIF

        DO  K=2,NZS1
          K1=2*K-3
          K2=K1+1
          X=DT/2./(ZSHALF(K+1)-ZSHALF(K))
          DTDZS(K1)=X/(ZSMAIN(K)-ZSMAIN(K-1))
          DTDZS2(K-1)=X
          DTDZS(K2)=X/(ZSMAIN(K+1)-ZSMAIN(K))
        END DO




  
        CW =4.183E6





        KQWRTZ=7.7
        KICE=2.2
        KWT=0.57




        c1sn=0.026

        c2sn=21.



        NROOT= 4


        RHONEWSN = 200.
       if(SNOW(i,j).gt.0. .and. SNOWH(i,j).gt.0.) then
        RHOSN = SNOW(i,j)/SNOWH(i,j)
       else
        RHOSN = 300.
       endif

    IF ( wrf_at_debug_level(3000) ) THEN
       if(ktau.eq.1 .and.(i.eq.358.and.j.eq.260)) &
           print *,'before SOILVEGIN - z0,znt(195,254)',z0(i,j),znt(i,j)
    ENDIF

     CALL SOILVEGIN  ( mosaic_lu, mosaic_soil,soilfrac,nscat,shdmin(i,j),shdmax(i,j),&
                       NLCAT,ILAND,ISOIL,iswater,IFOREST,lufrac,VEGFRA(I,J),         &
                       EMISSL(I,J),PC(I,J),ZNT(I,J),LAI(I,J),RDLAI2D,                &
                       QWRTZ,RHOCS,BCLH,DQM,KSAT,PSIS,QMIN,REF,WILT,i,j )
    IF ( wrf_at_debug_level(3000) ) THEN
      if(ktau.eq.1 .and.(i.eq.358.and.j.eq.260)) &
         print *,'after SOILVEGIN - z0,znt(375,254),lai(375,254)',z0(i,j),znt(i,j),lai(i,j)

      if(ktau.eq.1 .and. (i.eq.358.and.j.eq.260)) then
         print *,'NLCAT,iland,lufrac,EMISSL(I,J),PC(I,J),ZNT(I,J),LAI(I,J)', &
                  NLCAT,iland,lufrac,EMISSL(I,J),PC(I,J),ZNT(I,J),LAI(I,J),i,j
         print *,'NSCAT,soilfrac,QWRTZ,RHOCS,BCLH,DQM,KSAT,PSIS,QMIN,REF,WILT',&
                 NSCAT,soilfrac,QWRTZ,RHOCS,BCLH,DQM,KSAT,PSIS,QMIN,REF,WILT,i,j
      endif
    ENDIF

        CN=CFACTR_DATA   

        SAT = 5.e-4  




     IF(iforest.gt.2) THEN





         meltfactor = 2.0

         do k=2,nzs
         if(zsmain(k).ge.0.4) then
            NROOT=K
            goto  111
         endif
         enddo
     ELSE






         meltfactor = 0.85

         do k=2,nzs
         if(zsmain(k).ge.1.1) then
            NROOT=K
            goto  111
         endif
         enddo
     ENDIF
 111   continue


    IF ( wrf_at_debug_level(3000) ) THEN
         print *,' ZNT, LAI, VEGFRA, SAT, EMIS, PC --->',                &
                   ZNT(I,J),LAI(I,J),VEGFRA(I,J),SAT,EMISSL(I,J),PC(I,J)
         print *,' ZS, ZSMAIN, ZSHALF, CONFLX, CN, SAT, --->', zs,zsmain,zshalf,conflx,cn,sat
         print *,'NROOT, meltfactor, iforest, ivgtyp, i,j ', nroot,meltfactor,iforest,ivgtyp(I,J),I,J

    ENDIF






     if(lakemodel==1. .and. lakemask(i,j)==1.) goto 2999


        IF((XLAND(I,J)-1.5).GE.0.)THEN

           SMAVAIL(I,J)=1.0
             SMMAX(I,J)=1.0
             SNOW(I,J)=0.0
             SNOWH(I,J)=0.0
             SNOWC(I,J)=0.0
           LMAVAIL(I,J)=1.0

           ILAND=iswater
           ISOIL=14

           patmb=P8w(i,1,j)*1.e-2
           qvg  (i,j) = QSN(SOILT(i,j),TBQ)/PATMB
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))
           CHKLOWQ(I,J)=1.
           Q2SAT=QSN(TABS,TBQ)/PATMB

            DO K=1,NZS
              SOILMOIS(I,K,J)=1.0
              SH2O    (I,K,J)=1.0 
              TSO(I,K,J)= SOILT(I,J)
            ENDDO

    IF ( wrf_at_debug_level(3000) ) THEN
              PRINT*,'  water point, I=',I,                      &
              'J=',J, 'SOILT=', SOILT(i,j)
    ENDIF

           ELSE


       if(xice(i,j).ge.xice_threshold) then

           SEAICE(i,j)=1.
       else
           SEAICE(i,j)=0.
       endif

         IF(SEAICE(I,J).GT.0.5)THEN

    IF ( wrf_at_debug_level(3000) ) THEN
              PRINT*,' sea-ice at water point, I=',I,            &
              'J=',J
    ENDIF

            ILAND = isice
            ISOIL = 16
            ZNT(I,J) = 0.011
            snoalb(i,j) = 0.75
            dqm = 1.
            ref = 1.
            qmin = 0.
            wilt = 0.
            emissl(i,j) = 0.98 

           patmb=P8w(i,1,j)*1.e-2
           qvg  (i,j) = QSN(SOILT(i,j),TBQ)/PATMB
           qsg  (i,j) = qvg(i,j)
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))

            DO K=1,NZS
               soilmois(i,k,j) = 1.
               smfr3d(i,k,j)   = 1.
               sh2o(i,k,j)     = 0.
               keepfr3dflag(i,k,j) = 0.
               tso(i,k,j) = min(271.4,tso(i,k,j))
            ENDDO
          ENDIF




           DO k=1,nzs

              soilm1d (k) = min(max(0.,soilmois(i,k,j)-qmin),dqm)

              tso1d   (k) = tso(i,k,j)
              soiliqw (k) = min(max(0.,sh2o(i,k,j)-qmin),soilm1d(k))
              soilice (k) =(soilm1d (k) - soiliqw (k))/0.9 
           ENDDO 

           do k=1,nzs
              smfrkeep(k) = smfr3d(i,k,j)
              keepfr  (k) = keepfr3dflag(i,k,j)
           enddo

              LMAVAIL(I,J)=max(0.00001,min(1.,soilm1d(1)/(REF-QMIN)))


     if(ktau.gt.1) then



     endif

    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'LAND, i,j,tso1d,soilm1d,PATM,TABS,QVATM,QCATM,RHO',  &
                  i,j,tso1d,soilm1d,PATM,TABS,QVATM,QCATM,RHO
   print *,'CONFLX =',CONFLX 
   print *,'SMFRKEEP,KEEPFR   ',SMFRKEEP,KEEPFR
    ENDIF

        smtotold(i,j)=0.
      do k=1,nzs-1
        smtotold(i,j)=smtotold(i,j)+(qmin+soilm1d(k))*             &
                    (zshalf(k+1)-zshalf(k))
      enddo

        smtotold(i,j)=smtotold(i,j)+(qmin+soilm1d(nzs))*           &
                    (zsmain(nzs)-zshalf(nzs))

        canwatold(i,j) = canwatr
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'before SFCTMP, spp_lsm, rstoch, field_sf_loc',      &
      i,j,spp_lsm,(rstoch(i,k,j),k=1,nzs),(field_sf_loc(i,k,j),k=1,nzs)
    ENDIF

         CALL SFCTMP (spp_lsm,rstoch(i,:,j),field_sf_loc(i,:,j), & 
                dt,ktau,conflx,i,j,                              &

                nzs,nddzs,nroot,meltfactor,                      &   
                iland,isoil,xland(i,j),ivgtyp(i,j),isltyp(i,j),  &
                PRCPMS, NEWSNMS,SNWE,SNHEI,SNOWFRAC,             &
                RHOSN,RHONEWSN,RHOSNFALL,                        &
                snowrat,grauprat,icerat,curat,                   &
                PATM,TABS,QVATM,QCATM,RHO,                       &
                GLW(I,J),GSW(I,J),EMISSL(I,J),                   &
                QKMS,TKMS,PC(I,J),LMAVAIL(I,J),                  &
                canwatr,vegfra(I,J),alb(I,J),znt(I,J),           &
                snoalb(i,j),albbck(i,j),lai(i,j),                &   
                myjpbl,seaice(i,j),isice,                        &

                QWRTZ,                                           &
                rhocs,dqm,qmin,ref,                              &
                wilt,psis,bclh,ksat,                             &
                sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &

                cp,rovcp,g0,lv,stbolt,cw,c1sn,c2sn,              &
                KQWRTZ,KICE,KWT,                                 &

                snweprint,snheiprint,rsm,                        &
                soilm1d,tso1d,smfrkeep,keepfr,                   &
                soilt(I,J),soilt1(i,j),tsnav(i,j),dew(I,J),      &
                qvg(I,J),qsg(I,J),qcg(I,J),SMELT(I,J),           &
                SNOH(I,J),SNFLX(I,J),SNOM(I,J),SNOWFALLAC(I,J),  &
                ACSNOW(I,J),edir(I,J),ec(I,J),ett(I,J),qfx(I,J), &
                lh(I,J),hfx(I,J),sflx(I,J),sublim(I,J),          &
                evapl(I,J),prcpl(I,J),budget(i,j),runoff1(i,j),  &
                runoff2(I,J),soilice,soiliqw,infiltrp,smf(i,j))









    IF (lufrac(crop) > 0 .and. lai(i,j) > 1.1) THEN


        do k=1,nroot
             cropsm=1.1*wilt - qmin
          if(soilm1d(k) < cropsm*lufrac(crop)) then
    IF ( wrf_at_debug_level(3000) ) THEN
print * ,'Soil moisture is below wilting in cropland category at time step',ktau  &
              ,'i,j,lufrac(crop),k,soilm1d(k),wilt,cropsm',                       &
                i,j,lufrac(crop),k,soilm1d(k),wilt,cropsm
    ENDIF
             soilm1d(k) = cropsm*lufrac(crop)
    IF ( wrf_at_debug_level(3000) ) THEN
      print * ,'Added soil water to cropland category, i,j,k,soilm1d(k)',i,j,k,soilm1d(k)
    ENDIF
          endif
        enddo

    ELSEIF (ivgtyp(i,j) == natural .and. lai(i,j) > 0.7) THEN

        do k=1,nroot
             cropsm=1.2*wilt - qmin
          if(soilm1d(k) < cropsm*lufrac(natural)*0.4) then
    IF ( wrf_at_debug_level(3000) ) THEN
print * ,'Soil moisture is below wilting in mixed grassland/cropland category at time step',ktau &
              ,'i,j,lufrac(natural),k,soilm1d(k),wilt',                       &
                i,j,lufrac(natural),k,soilm1d(k),wilt
    ENDIF
             soilm1d(k) = cropsm * lufrac(natural)*0.4
    IF ( wrf_at_debug_level(3000) ) THEN
      print * ,'Added soil water to grassland category, i,j,k,soilm1d(k)',i,j,k,soilm1d(k)
    ENDIF
          endif
        enddo
    ENDIF


       if (spp_lsm==1) then
         do k=1,nsl
           field_sf(i,k,j)=field_sf_loc(i,k,j)
         enddo
       endif





        smavail(i,j) = 0.
        smmax (i,j)  = 0.  

      do k=1,nzs-1
        smavail(i,j)=smavail(i,j)+(qmin+soilm1d(k))*             &
                    (zshalf(k+1)-zshalf(k))
        smmax (i,j) =smmax (i,j)+(qmin+dqm)*                     &
                    (zshalf(k+1)-zshalf(k))
      enddo

        smavail(i,j)=smavail(i,j)+(qmin+soilm1d(nzs))*           &
                    (zsmain(nzs)-zshalf(nzs))
        smmax (i,j) =smmax (i,j)+(qmin+dqm)*                     &
                    (zsmain(nzs)-zshalf(nzs))


        SFCRUNOFF(I,J) = SFCRUNOFF(I,J)+RUNOFF1(I,J)*DT*1000.0
        UDRUNOFF (I,J) = UDRUNOFF(I,J)+RUNOFF2(I,J)*DT*1000.0
        ACRUNOFF(I,J)  = ACRUNOFF(I,J)+RUNOFF1(I,J)*DT*1000.0
        SMAVAIL  (I,J) = SMAVAIL(I,J) * 1000.
        SMMAX    (I,J) = SMMAX(I,J) * 1000.
        smtotold (I,J) = smtotold(I,J) * 1000.

        do k=1,nzs


             soilmois(i,k,j) = soilm1d(k) + qmin
             sh2o    (i,k,j) = min(soiliqw(k) + qmin,soilmois(i,k,j))
                  tso(i,k,j) = tso1d(k)
        enddo

        tso(i,nzs,j) = tbot(i,j)

        do k=1,nzs
             smfr3d(i,k,j) = smfrkeep(k)
           keepfr3dflag(i,k,j) = keepfr (k)
        enddo




        Z0       (I,J) = ZNT (I,J)
        SFCEXC   (I,J) = TKMS
        patmb=P8w(i,1,j)*1.e-2
        Q2SAT=QSN(TABS,TBQ)/PATMB
        QSFC(I,J) = QVG(I,J)/(1.+QVG(I,J))

        IF((myjpbl).AND.(QVATM.GE.Q2SAT*0.95).AND.QVATM.LT.qvg(I,J))THEN
          CHKLOWQ(I,J)=0.
        ELSE
          CHKLOWQ(I,J)=1.
        ENDIF

    IF ( wrf_at_debug_level(3000) ) THEN
      if(CHKLOWQ(I,J).eq.0.) then
   print *,'i,j,CHKLOWQ',  &
                  i,j,CHKLOWQ(I,J)
      endif
    ENDIF

        if(snow(i,j)==0.) EMISSL(i,j) = LEMITBL(IVGTYP(i,j))
        EMISS (I,J) = EMISSL(I,J)

        SNOW   (i,j) = SNWE*1000.
        SNOWH  (I,J) = SNHEI 
        CANWAT (I,J) = CANWATR*1000.

        INFILTR(I,J) = INFILTRP

        MAVAIL (i,j) = LMAVAIL(I,J)  
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,' LAND, I=,J=, QFX, HFX after SFCTMP', i,j,lh(i,j),hfx(i,j)
    ENDIF

        SFCEVP (I,J) = SFCEVP (I,J) + QFX (I,J) * DT
        GRDFLX (I,J) = -1. * sflx(I,J)









       if(snowfrac > 0. .and. xice(i,j).ge.xice_threshold ) then
           SNOWFRAC = SNOWFRAC*XICE(I,J)
       endif

       SNOWC(I,J)=SNOWFRAC


       RHOSNF(I,J)=RHOSNFALL


       SFCEVP (I,J) = SFCEVP (I,J) + QFX (I,J) * DT













       ac=0.
       as=0.

       ac=max(0.,canwat(i,j)-canwatold(i,j))
       as=max(0.,snwe-snowold(i,j))
       wb =rainbl(i,j)+smelt(i,j)*dt*1.e3 & 
                      -qfx(i,j)*dt &
                      -runoff1(i,j)*dt*1.e3-runoff2(i,j)*dt*1.e3 &
                      -ac-as - (smavail(i,j)-smtotold(i,j))

       waterbudget(i,j)=rainbl(i,j)+smelt(i,j)*dt*1.e3 & 
                      -qfx(i,j)*dt &
                      -runoff1(i,j)*dt*1.e3-runoff2(i,j)*dt*1.e3 &
                      -ac-as - (smavail(i,j)-smtotold(i,j))



       acwaterbudget(i,j)=acwaterbudget(i,j)+waterbudget(i,j)




    IF ( wrf_at_debug_level(3000) ) THEN
  print *,'Smf=',smf(i,j),i,j
  print *,'Budget',budget(i,j),i,j
  print *,'RUNOFF2= ', i,j,runoff2(i,j)
  print *,'Water budget ', i,j,waterbudget(i,j)
  print *,'rainbl,qfx*dt,runoff1,smelt*dt*1.e3,smchange', &
          i,j,rainbl(i,j),qfx(i,j)*dt,runoff1(i,j)*dt*1.e3, &
          smelt(i,j)*dt*1.e3, &
          (smavail(i,j)-smtotold(i,j))

  print *,'SNOW,SNOWold',i,j,snwe,snowold(i,j)
  print *,'SNOW-SNOWold',i,j,max(0.,snwe-snowold(i,j))
  print *,'CANWATold, canwat ',i,j,canwatold(i,j),canwat(i,j)
  print *,'canwat(i,j)-canwatold(i,j)',max(0.,canwat(i,j)-canwatold(i,j))
    ENDIF


    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'LAND, i,j,tso1d,soilm1d,soilt - end of time step',         &
                  i,j,tso1d,soilm1d,soilt(i,j)
   print *,'LAND, QFX, HFX after SFCTMP', i,j,lh(i,j),hfx(i,j)
    ENDIF


        ENDIF
2999  continue 

      ENDDO

   ENDDO


   END SUBROUTINE LSMRUC




   SUBROUTINE SFCTMP (spp_lsm,rstochcol,fieldcol_sf,             &
                delt,ktau,conflx,i,j,                            &

                nzs,nddzs,nroot,meltfactor,                      &
                ILAND,ISOIL,XLAND,IVGTYP,ISLTYP,PRCPMS,          &
                NEWSNMS,SNWE,SNHEI,SNOWFRAC,                     &
                RHOSN,RHONEWSN,RHOSNFALL,                        &
                snowrat,grauprat,icerat,curat,                   &
                PATM,TABS,QVATM,QCATM,rho,                       &
                GLW,GSW,EMISS,QKMS,TKMS,PC,                      &
                MAVAIL,CST,VEGFRA,ALB,ZNT,                       &
                ALB_SNOW,ALB_SNOW_FREE,lai,                      &
                MYJ,SEAICE,ISICE,                                &

                QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,    &
                sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &

                cp,rovcp,g0,lv,stbolt,cw,c1sn,c2sn,              &
                KQWRTZ,KICE,KWT,                                 &

                snweprint,snheiprint,rsm,                        &
                soilm1d,ts1d,smfrkeep,keepfr,soilt,soilt1,       &
                tsnav,dew,qvg,qsg,qcg,                           &
                SMELT,SNOH,SNFLX,SNOM,SNOWFALLAC,ACSNOW,         &
                edir1,ec1,ett1,eeta,qfx,hfx,s,sublim,            &
                evapl,prcpl,fltot,runoff1,runoff2,soilice,       &
                soiliqw,infiltr,smf)

       IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  isice,i,j,nroot,ktau,nzs ,      &
                                 nddzs                             

   REAL,     INTENT(IN   )   ::  DELT,CONFLX,meltfactor
   REAL,     INTENT(IN   )   ::  C1SN,C2SN
   LOGICAL,    INTENT(IN   )    ::     myj

   REAL                                                        , &
            INTENT(IN   )    ::                            PATM, &
                                                           TABS, &
                                                          QVATM, &
                                                          QCATM
   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                             PC, &
                                                         VEGFRA, &
                                                  ALB_SNOW_FREE, &
                                                            lai, &
                                                         SEAICE, &
                                                          XLAND, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS
                                                             
   INTEGER,   INTENT(IN   )  ::                          IVGTYP, ISLTYP

   REAL                                                        , &
            INTENT(INOUT)    ::                           EMISS, &
                                                         MAVAIL, &
                                                       SNOWFRAC, &
                                                       ALB_SNOW, &
                                                            ALB, &
                                                            CST


   REAL                      ::                                  &
                                                          RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                            SAT, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                             CP, &
                                                          ROVCP, &
                                                             G0, &
                                                             LV, &
                                                         STBOLT, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2 

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::          rstochcol
   REAL,     DIMENSION(1:NZS), INTENT(INOUT) ::     fieldcol_sf


   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:5001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                            TS1D, & 
                                                        SOILM1D, &
                                                       SMFRKEEP
   REAL,  DIMENSION( 1:nzs )                                   , &
             INTENT(INOUT)   ::                          KEEPFR

   REAL,  DIMENSION(1:NZS), INTENT(INOUT)  ::          SOILICE, &
                                                       SOILIQW
          

   INTEGER, INTENT(INOUT)    ::                     ILAND,ISOIL
   INTEGER                   ::                     ILANDs


   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          EVAPL, &
                                                        INFILTR, &
                                                          RHOSN, & 
                                                       RHONEWSN, &
                                                      rhosnfall, &
                                                        snowrat, &
                                                       grauprat, &
                                                         icerat, &
                                                          curat, &
                                                         SUBLIM, &
                                                          PRCPL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                            QFX, &
                                                            HFX, &
                                                          fltot, &
                                                            smf, &
                                                              S, &  
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                         ACSNOW, &
                                                     SNOWFALLAC, &
                                                           SNWE, &
                                                          SNHEI, &
                                                          SMELT, &
                                                           SNOM, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                          TSNAV, &
                                                            ZNT

   REAL,     DIMENSION(1:NZS)              ::                    &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice, &
                                                          TS1DS, &
                                                       SOILM1DS, &
                                                      SMFRKEEPS, &
                                                       SOILIQWS, & 
                                                       SOILICES, &
                                                        KEEPFRS

   REAL :: &
                                                            DEWS, &
                                                        MAVAILS,  &
                                                          EDIR1s, &
                                                            EC1s, &
                                                            csts, &
                                                           ETT1s, &
                                                           EETAs, &
                                                          EVAPLs, &
                                                        INFILTRs, &
                                                          PRCPLS, &
                                                            QVGS, &
                                                            QSGS, &
                                                            QCGS, &
                                                            QFXS, &
                                                            HFXS, &
                                                          fltots, &
                                                        RUNOFF1S, &
                                                        RUNOFF2s, &
                                                              SS, &
                                                          SOILTs

            
                     

   REAL,  INTENT(INOUT)                     ::              RSM, &  
                                                      SNWEPRINT, &
                                                     SNHEIPRINT
   INTEGER,   INTENT(IN)                    ::     spp_lsm     


   INTEGER ::  K,ILNB

   REAL    ::  BSN, XSN                                        , &
               RAINF, SNTH, NEWSN, PRCPMS, NEWSNMS             , &
               T3, UPFLUX, XINET
   REAL    ::  snhei_crit, snhei_crit_newsn, keep_snow_albedo, SNOWFRACnewsn
   REAL    ::  newsnowratio, dd1

   REAL    ::  rhonewgr,rhonewice

   REAL    ::  RNET,GSWNEW,GSWIN,EMISSN,ZNTSN,EMISS_snowfree
   REAL    ::  VEGFRAC, snow_mosaic, snfr, vgfr
   real    ::  cice, albice, albsn, drip, dripsn, dripliq
   real    ::  interw, intersn, infwater, intwratio


        integer,   parameter      ::      ilsnow=99 
        
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,' in SFCTMP',i,j,nzs,nddzs,nroot,                 &
                 SNWE,RHOSN,SNOM,SMELT,TS1D
    ENDIF

        snow_mosaic=0.
        snfr = 1.
        NEWSN=0.
        newsnowratio = 0.
        snowfracnewsn=0.
        if(snhei == 0.) snowfrac=0.
        smelt = 0.
        RAINF = 0.
        RSM=0.
        DD1=0.
        INFILTR=0.







        VEGFRAC=0.01*VEGFRA
        drip = 0.
        dripsn = 0.
        dripliq = 0.
        smf = 0.
        interw=0.
        intersn=0.
        infwater=0.


          do k=1,nzs
            tice(k) = 0.
            rhosice(k) = 0. 
            cice = 0.
            capice(k) = 0.
            thdifice(k) = 0.
          enddo

        GSWnew=GSW
        GSWin=GSW/(1.-alb)
        ALBice=ALB_SNOW_FREE
        ALBsn=alb_snow
        EMISSN = 0.98
        EMISS_snowfree = LEMITBL(IVGTYP)





       if(SEAICE.ge.0.5) then
          do k=1,nzs
            tice(k) = ts1d(k) - 273.15
            rhosice(k) = 917.6/(1-0.000165*tice(k))
            cice = 2115.85 +7.7948*tice(k)
            capice(k) = cice*rhosice(k)
            thdifice(k) = 2.260872/capice(k)
           enddo




       ALBice = MIN(ALB_SNOW_FREE,MAX(ALB_SNOW_FREE - 0.05,   &
               ALB_SNOW_FREE - 0.1*(tice(1)+10.)/10. ))
       endif

    IF ( wrf_at_debug_level(3000) ) THEN

        print *,'alb_snow_free',ALB_SNOW_FREE
        print *,'GSW,GSWnew,GLW,SOILT,EMISS,ALB,ALBice,SNWE',&
                 GSW,GSWnew,GLW,SOILT,EMISS,ALB,ALBice,SNWE
    ENDIF

	if(snhei.gt.0.0081*1.e3/rhosn) then

        BSN=delt/3600.*c1sn*exp(0.08*min(0.,tsnav)-c2sn*rhosn*1.e-3)
       if(bsn*snwe*100..lt.1.e-4) goto 777
        XSN=rhosn*(exp(bsn*snwe*100.)-1.)/(bsn*snwe*100.)
        rhosn=MIN(MAX(58.8,XSN),500.) 
 777   continue

      endif

           newsn=newsnms*delt

       IF(NEWSN.GT.0.) THEN


    IF ( wrf_at_debug_level(3000) ) THEN
      print *, 'THERE IS NEW SNOW, newsn', newsn
    ENDIF

        newsnowratio = min(1.,newsn/(snwe+newsn))




        rhonewsn=min(125.,1000.0/max(8.,(17.*tanh((276.65-Tabs)*0.15))))
        rhonewgr=min(500.,rhowater/max(2.,(3.5*tanh((274.15-Tabs)*0.3333))))
        rhonewice=rhonewsn




         rhosnfall = min(500.,max(58.8,(rhonewsn*snowrat +  &  
                     rhonewgr*grauprat + rhonewice*icerat + rhonewgr*curat)))


         rhonewsn=rhosnfall




         xsn=(rhosn*snwe+rhonewsn*newsn)/                         &
             (snwe+newsn)
         rhosn=MIN(MAX(58.8,XSN),500.) 

       ENDIF 

       IF(PRCPMS.NE.0.) THEN







           RAINF=1.
       ENDIF

        drip = 0.
        intwratio=0.
     if(vegfrac > 0.01) then


         interw=0.25*DELT*PRCPMS*(1.-exp(-0.5*lai))*vegfrac
         intersn=0.25*NEWSN*(1.-exp(-0.5*lai))*vegfrac
         infwater=PRCPMS - interw/delt
    if((interw+intersn) > 0.) then
       intwratio=interw/(interw+intersn)
    endif


         dd1=CST + interw + intersn
         CST=DD1
        IF(CST.GT.SAT) THEN
          CST=SAT
          DRIP=DD1-SAT
        ENDIF
     else
         CST=0.
         DRIP=0.
         interw=0.
         intersn=0.
         infwater=PRCPMS
     endif 


         SNHEI_CRIT=0.01601*1.e3/rhosn
         SNHEI_CRIT_newsn=0.0005*1.e3/rhosn

         SNOWFRAC=MIN(1.,SNHEI/(2.*SNHEI_CRIT))
        if(snowfrac < 0.75) snow_mosaic = 1.

       IF(NEWSN.GT.0.) THEN

         snwe=max(0.,snwe+newsn-intersn)

      if(drip > 0.) then
       if (snow_mosaic==1.) then
         dripliq=drip*intwratio
         dripsn = drip - dripliq
         snwe=snwe+dripsn
         infwater=infwater+dripliq
         dripliq=0.
         dripsn = 0.
       else
         snwe=snwe+drip
       endif
      endif
         snhei=snwe*rhowater/rhosn
         NEWSN=NEWSN*rhowater/rhonewsn
       ENDIF

   IF(SNHEI.GT.0.0) THEN


         ILAND=ISICE











         SNOWFRAC=MIN(1.,SNHEI/(2.*SNHEI_CRIT))

      if(ivgtyp == urban) snowfrac=min(0.75,snowfrac)













       if(snowfrac < 0.75) snow_mosaic = 1.

       if(newsn > 0. ) SNOWFRACnewsn=MIN(1.,SNHEI/SNHEI_CRIT_newsn)

         KEEP_SNOW_ALBEDO = 0.
      IF (NEWSN > 0. .and. snowfracnewsn > 0.99) THEN

             KEEP_SNOW_ALBEDO = 1.
             snow_mosaic=0.  
      ENDIF

    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SNHEI_CRIT,SNOWFRAC,SNHEI_CRIT_newsn,SNOWFRACnewsn', &
               SNHEI_CRIT,SNOWFRAC,SNHEI_CRIT_newsn,SNOWFRACnewsn
    ENDIF





      IF(newsn.eq.0. .and. znt.le.0.2 .and. IVGTYP.ne.isice) then
         if( snhei .le. 2.*ZNT)then
           znt=0.55*znt+0.45*z0tbl(iland)
         elseif( snhei .gt. 2.*ZNT .and. snhei .le. 4.*ZNT)then
           znt=0.2*znt+0.8*z0tbl(iland)
         elseif(snhei > 4.*ZNT) then
           znt=z0tbl(iland)
         endif
       ENDIF






    IF(SEAICE .LT. 0.5) THEN





     if( snow_mosaic == 1.) then
         ALBsn=alb_snow

         Emiss= emissn
     else
         ALBsn   = MAX(keep_snow_albedo*alb_snow,               &
                   MIN((alb_snow_free +                         &
           (alb_snow - alb_snow_free) * snowfrac), alb_snow))

         Emiss   = MAX(keep_snow_albedo*emissn,                 &
                   MIN((emiss_snowfree +                         &
           (emissn - emiss_snowfree) * snowfrac), emissn))
     endif
    IF ( wrf_at_debug_level(3000) ) THEN

  print *,'Snow on soil ALBsn,emiss,snow_mosaic',i,j,ALBsn,emiss,snow_mosaic
    ENDIF













     if(albsn.lt.0.4 .or. keep_snow_albedo==1) then
        ALB=ALBsn

      else

        ALB = MIN(ALBSN,MAX(ALBSN - 0.1*(soilt - 263.15)/       &
                (273.15-263.15)*ALBSN, ALBSN - 0.05))
      endif
    ELSE

     if( snow_mosaic == 1.) then
         ALBsn=alb_snow
         Emiss= emissn
     else
         ALBsn   = MAX(keep_snow_albedo*alb_snow,               &
                   MIN((albice + (alb_snow - albice) * snowfrac), alb_snow))
         Emiss   = MAX(keep_snow_albedo*emissn,                 &
                   MIN((emiss_snowfree +                        &
           (emissn - emiss_snowfree) * snowfrac), emissn))
     endif

    IF ( wrf_at_debug_level(3000) ) THEN
  print *,'Snow on ice snow_mosaic,ALBsn,emiss',i,j,ALBsn,emiss,snow_mosaic
    ENDIF



      if(albsn.lt.alb_snow .or. keep_snow_albedo .eq.1.)then
       ALB=ALBsn
      else

       ALB = MIN(ALBSN,MAX(ALBSN - 0.15*ALBSN*(soilt - 263.15)/  &
                (273.15-263.15), ALBSN - 0.1))
      endif

    ENDIF

    if (snow_mosaic==1.) then 


       if(SEAICE .LT. 0.5) then




         gswnew=GSWin*(1.-alb_snow_free)

         T3      = STBOLT*SOILT*SOILT*SOILT
         UPFLUX  = T3 *SOILT
         XINET   = EMISS_snowfree*(GLW-UPFLUX)
         RNET    = GSWnew + XINET
    IF ( wrf_at_debug_level(3000) ) THEN

     print *,'Fractional snow - snowfrac=',snowfrac
     print *,'Snowfrac<1 GSWin,GSWnew -',GSWin,GSWnew,'SOILT, RNET',soilt,rnet
    ENDIF
           do k=1,nzs
          soilm1ds(k) = soilm1d(k)
          ts1ds(k) = ts1d(k)
          smfrkeeps(k) = smfrkeep(k)
          keepfrs(k) = keepfr(k)
          soilices(k) = soilice(k)
          soiliqws(k) = soiliqw(k)
            enddo
          soilts = soilt
          qvgs = qvg
          qsgs = qsg
          qcgs = qcg
          csts = cst
          mavails = mavail
          smelt=0.
          runoff1s=0.
          runoff2s=0.
       
          ilands = ivgtyp

         CALL SOIL(spp_lsm,rstochcol,fieldcol_sf,               &

            i,j,ilands,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSWnew,gswin,     &
            EMISS_snowfree,RNET,QKMS,TKMS,PC,csts,dripliq,      &
            infwater,rho,vegfrac,lai,myj,                       &

            QWRTZ,rhocs,dqm,qmin,ref,wilt,                      &
            psis,bclh,ksat,sat,cn,                              &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &

            lv,CP,rovcp,G0,cw,stbolt,tabs,                      &
            KQWRTZ,KICE,KWT,                                    &

            soilm1ds,ts1ds,smfrkeeps,keepfrs,                   &
            dews,soilts,qvgs,qsgs,qcgs,edir1s,ec1s,             &
            ett1s,eetas,qfxs,hfxs,ss,evapls,prcpls,fltots,runoff1s, &
            runoff2s,mavails,soilices,soiliqws,                 &
            infiltrs,smf)
        else




         gswnew=GSWin*(1.-albice)

         T3      = STBOLT*SOILT*SOILT*SOILT
         UPFLUX  = T3 *SOILT
         XINET   = EMISS_snowfree*(GLW-UPFLUX)
         RNET    = GSWnew + XINET
    IF ( wrf_at_debug_level(3000) ) THEN

     print *,'Fractional snow - snowfrac=',snowfrac
     print *,'Snowfrac<1 GSWin,GSWnew -',GSWin,GSWnew,'SOILT, RNET',soilt,rnet
    ENDIF
            do k=1,nzs
          ts1ds(k) = ts1d(k)
            enddo
          soilts = soilt
          qvgs = qvg
          qsgs = qsg
          qcgs = qcg
          smelt=0.
          runoff1s=0.
          runoff2s=0.
 
          CALL SICE(                                            &

            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSWnew,           &
            0.98,RNET,QKMS,TKMS,rho,myj,                        &

            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &

            lv,CP,rovcp,cw,stbolt,tabs,                         &

            ts1ds,dews,soilts,qvgs,qsgs,qcgs,                   &
            eetas,qfxs,hfxs,ss,evapls,prcpls,fltots             &
                                                                )
           edir1 = eeta*1.e-3
           ec1 = 0.
           ett1 = 0.
           runoff1 = prcpms
           runoff2 = 0.
           mavail = 1.
           infiltr=0.
           cst=0.
            do k=1,nzs
               soilm1d(k)=1.
               soiliqw(k)=0.
               soilice(k)=1.
               smfrkeep(k)=1.
               keepfr(k)=0.
            enddo
        endif 


    IF ( wrf_at_debug_level(3000) ) THEN

     print *,'gswnew,alb_snow_free,alb',gswnew,alb_snow_free,alb
    ENDIF


    IF ( wrf_at_debug_level(3000) ) THEN

       print *,'Incoming GSWnew snowfrac<1 -',gswnew
    ENDIF
    endif 
                           


         gswnew=GSWin*(1.-alb)


         T3      = STBOLT*SOILT*SOILT*SOILT
         UPFLUX  = T3 *SOILT
         XINET   = EMISS*(GLW-UPFLUX)
         RNET    = GSWnew + XINET
    IF ( wrf_at_debug_level(3000) ) THEN


        print *,'RNET=',rnet
        print *,'SNOW - I,J,newsn,snwe,snhei,GSW,GSWnew,GLW,UPFLUX,ALB',&
                 i,j,newsn,snwe,snhei,GSW,GSWnew,GLW,UPFLUX,ALB
    ENDIF

      if (SEAICE .LT. 0.5) then

           if(snow_mosaic==1.)then
              snfr=1.
           else
              snfr=snowfrac
           endif
         CALL SNOWSOIL (spp_lsm,rstochcol,fieldcol_sf,     & 
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,nroot,         &
            meltfactor,rhonewsn,SNHEI_CRIT,                     &  
            ILAND,PRCPMS,RAINF,NEWSN,snhei,SNWE,snfr,           &
            RHOSN,PATM,QVATM,QCATM,                             &
            GLW,GSWnew,GSWin,EMISS,RNET,IVGTYP,                 &
            QKMS,TKMS,PC,CST,dripsn,infwater,                   &
            RHO,VEGFRAC,ALB,ZNT,lai,                            &
            MYJ,                                                &

            QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,       &
            sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,              & 

            lv,CP,rovcp,G0,cw,stbolt,tabs,                      &
            KQWRTZ,KICE,KWT,                                    &

            ilnb,snweprint,snheiprint,rsm,                      &
            soilm1d,ts1d,smfrkeep,keepfr,                       &
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &
            SMELT,SNOH,SNFLX,SNOM,edir1,ec1,ett1,eeta,          &
            qfx,hfx,s,sublim,prcpl,fltot,runoff1,runoff2,       &
            mavail,soilice,soiliqw,infiltr                      )
       else

           if(snow_mosaic==1.)then
              snfr=1.
           else
              snfr=snowfrac
           endif

         CALL SNOWSEAICE (                                      &
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,               &    
            meltfactor,rhonewsn,SNHEI_CRIT,                     &  
            ILAND,PRCPMS,RAINF,NEWSN,snhei,SNWE,snfr,           &    
            RHOSN,PATM,QVATM,QCATM,                             &    
            GLW,GSWnew,EMISS,RNET,                              &    
            QKMS,TKMS,RHO,myj,                                  &    

            ALB,ZNT,                                            &
            tice,rhosice,capice,thdifice,                       &    
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &    

            lv,CP,rovcp,cw,stbolt,tabs,                         &    

            ilnb,snweprint,snheiprint,rsm,ts1d,                 &    
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &    
            SMELT,SNOH,SNFLX,SNOM,eeta,                         &    
            qfx,hfx,s,sublim,prcpl,fltot                        &    
                                                                )    
           edir1 = eeta*1.e-3
           ec1 = 0.
           ett1 = 0.
           runoff1 = smelt
           runoff2 = 0.
           mavail = 1.
           infiltr=0.
           cst=0.
            do k=1,nzs
               soilm1d(k)=1.
               soiliqw(k)=0.
               soilice(k)=1.
               smfrkeep(k)=1.
               keepfr(k)=0.
            enddo
       endif


         if(snhei.eq.0.) then

         alb=alb_snow_free
         iland=ivgtyp
         endif

     if (snow_mosaic==1.) then


        if(SEAICE .LT. 0.5) then

   IF ( wrf_at_debug_level(3000) ) THEN

      print *,'SOILT snow on land', ktau, i,j,soilt
      print *,'SOILT on snow-free land', i,j,soilts
      print *,'ts1d,ts1ds',i,j,ts1d,ts1ds
      print *,' SNOW flux',i,j, snflx
      print *,' Ground flux on snow-covered land',i,j, s
      print *,' Ground flux on snow-free land', i,j,ss
      print *,' CSTS, CST', i,j,csts,cst
   ENDIF
            do k=1,nzs
          soilm1d(k) = soilm1ds(k)*(1.-snowfrac) + soilm1d(k)*snowfrac
          ts1d(k) = ts1ds(k)*(1.-snowfrac) + ts1d(k)*snowfrac
          smfrkeep(k) = smfrkeeps(k)*(1.-snowfrac) + smfrkeep(k)*snowfrac
       if(snowfrac > 0.5) then
          keepfr(k) = keepfr(k)
       else
          keepfr(k) = keepfrs(k)
       endif
          soilice(k) = soilices(k)*(1.-snowfrac) + soilice(k)*snowfrac
          soiliqw(k) = soiliqws(k)*(1.-snowfrac) + soiliqw(k)*snowfrac
            enddo
          dew = dews*(1.-snowfrac) + dew*snowfrac
          soilt = soilts*(1.-snowfrac) + soilt*snowfrac
          qvg = qvgs*(1.-snowfrac) + qvg*snowfrac
          qsg = qsgs*(1.-snowfrac) + qsg*snowfrac
          qcg = qcgs*(1.-snowfrac) + qcg*snowfrac
          edir1 = edir1s*(1.-snowfrac) + edir1*snowfrac
          ec1 = ec1s*(1.-snowfrac) + ec1*snowfrac
          cst = csts*(1.-snowfrac) + cst*snowfrac
          ett1 = ett1s*(1.-snowfrac) + ett1*snowfrac
          eeta = eetas*(1.-snowfrac) + eeta*snowfrac
          qfx = qfxs*(1.-snowfrac) + qfx*snowfrac
          hfx = hfxs*(1.-snowfrac) + hfx*snowfrac
          s = ss*(1.-snowfrac) + s*snowfrac
          evapl = evapls*(1.-snowfrac)
          sublim = sublim*snowfrac
          prcpl = prcpls*(1.-snowfrac) + prcpl*snowfrac
          fltot = fltots*(1.-snowfrac) + fltot*snowfrac

          ALB   = MAX(keep_snow_albedo*alb,              &
                  MIN((alb_snow_free + (alb - alb_snow_free) * snowfrac), alb))

          Emiss = MAX(keep_snow_albedo*emissn,           &
                  MIN((emiss_snowfree +                  &
              (emissn - emiss_snowfree) * snowfrac), emissn))







          runoff1 = runoff1s*(1.-snowfrac) + runoff1*snowfrac
          runoff2 = runoff2s*(1.-snowfrac) + runoff2*snowfrac
          smelt = smelt * snowfrac
          snoh = snoh * snowfrac
          snflx = snflx * snowfrac
          snom = snom * snowfrac
          mavail = mavails*(1.-snowfrac) + 1.*snowfrac
          infiltr = infiltrs*(1.-snowfrac) + infiltr*snowfrac

    IF ( wrf_at_debug_level(3000) ) THEN
      print *,' Ground flux combined', i,j, s
      print *,'SOILT combined on land', soilt
      print *,'TS combined on land', ts1d
    ENDIF
       else


    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SOILT snow on ice', soilt
    ENDIF
            do k=1,nzs
          ts1d(k) = ts1ds(k)*(1.-snowfrac) + ts1d(k)*snowfrac
            enddo
          dew = dews*(1.-snowfrac) + dew*snowfrac
          soilt = soilts*(1.-snowfrac) + soilt*snowfrac
          qvg = qvgs*(1.-snowfrac) + qvg*snowfrac
          qsg = qsgs*(1.-snowfrac) + qsg*snowfrac
          qcg = qcgs*(1.-snowfrac) + qcg*snowfrac
          eeta = eetas*(1.-snowfrac) + eeta*snowfrac
          qfx = qfxs*(1.-snowfrac) + qfx*snowfrac
          hfx = hfxs*(1.-snowfrac) + hfx*snowfrac
          s = ss*(1.-snowfrac) + s*snowfrac
          sublim = eeta
          prcpl = prcpls*(1.-snowfrac) + prcpl*snowfrac
          fltot = fltots*(1.-snowfrac) + fltot*snowfrac

          ALB   = MAX(keep_snow_albedo*alb,              &
                  MIN((albice + (alb - alb_snow_free) * snowfrac), alb))

          Emiss = MAX(keep_snow_albedo*emissn,           &
                  MIN((emiss_snowfree +                  &
              (emissn - emiss_snowfree) * snowfrac), emissn))



          runoff1 = runoff1s*(1.-snowfrac) + runoff1*snowfrac
          runoff2 = runoff2s*(1.-snowfrac) + runoff2*snowfrac
          smelt = smelt * snowfrac
          snoh = snoh * snowfrac
          snflx = snflx * snowfrac
          snom = snom * snowfrac
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SOILT combined on ice', soilt
    ENDIF
       endif      
     endif 
 


      snowfallac = snowfallac + max(0.,(newsn - rhowater/rhonewsn*smelt*delt*newsnowratio))

   ELSE

           snheiprint=0.
           snweprint=0.
           smelt=0.


         T3      = STBOLT*SOILT*SOILT*SOILT
         UPFLUX  = T3 *SOILT
         XINET   = EMISS*(GLW-UPFLUX)
         RNET    = GSWnew + XINET
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'NO snow on the ground GSWnew -',GSWnew,'RNET=',rnet
    ENDIF

       if(SEAICE .LT. 0.5) then

         CALL SOIL(spp_lsm,rstochcol,fieldcol_sf,               &

            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSWnew,GSWin,     &
            EMISS,RNET,QKMS,TKMS,PC,cst,drip,infwater,          &
            rho,vegfrac,lai,myj,                                &

            QWRTZ,rhocs,dqm,qmin,ref,wilt,                      &
            psis,bclh,ksat,sat,cn,                              &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &

            lv,CP,rovcp,G0,cw,stbolt,tabs,                      &
            KQWRTZ,KICE,KWT,                                    &

            soilm1d,ts1d,smfrkeep,keepfr,                       &
            dew,soilt,qvg,qsg,qcg,edir1,ec1,                    &
            ett1,eeta,qfx,hfx,s,evapl,prcpl,fltot,runoff1,      &
            runoff2,mavail,soilice,soiliqw,                     &
            infiltr,smf)
        else



         if(ALB.ne.ALBice) GSWnew=GSW/(1.-ALB)*(1.-ALBice)
         alb=albice
         RNET    = GSWnew + XINET

          CALL SICE(                                            &

            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSWnew,           &
            EMISS,RNET,QKMS,TKMS,rho,myj,                       &

            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &

            lv,CP,rovcp,cw,stbolt,tabs,                         &

            ts1d,dew,soilt,qvg,qsg,qcg,                         &
            eeta,qfx,hfx,s,evapl,prcpl,fltot                          &
                                                                )
           edir1 = eeta*1.e-3
           ec1 = 0.
           ett1 = 0.
           runoff1 = prcpms
           runoff2 = 0.
           mavail = 1.
           infiltr=0.
           cst=0.
            do k=1,nzs
               soilm1d(k)=1.
               soiliqw(k)=0.
               soilice(k)=1.
               smfrkeep(k)=1.
               keepfr(k)=0.
            enddo
        endif

        ENDIF




   END SUBROUTINE SFCTMP



       FUNCTION QSN(TN,T)

   REAL,     DIMENSION(1:5001),  INTENT(IN   )   ::  T
   REAL,     INTENT(IN  )   ::  TN

      REAL    QSN, R,R1,R2
      INTEGER I

       R=(TN-173.15)/.05+1.
       I=INT(R)
       IF(I.GE.1) goto 10
       I=1
       R=1.
  10   IF(I.LE.5000) GOTO 20
       I=5000
       R=5001.
  20   R1=T(I)
       R2=R-I
       QSN=(T(I+1)-R1)*R2 + R1




  END FUNCTION QSN



        SUBROUTINE SOIL (spp_lsm,rstochcol, fieldcol_sf,     &

            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,&
            PRCPMS,RAINF,PATM,QVATM,QCATM,                   &
            GLW,GSW,GSWin,EMISS,RNET,                        &
            QKMS,TKMS,PC,cst,drip,infwater,rho,vegfrac,lai,  &
            myj,                                             &

            QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,    &
            sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,           &

            xlv,CP,rovcp,G0_P,cw,stbolt,TABS,                &
            KQWRTZ,KICE,KWT,                                 &

            soilmois,tso,smfrkeep,keepfr,                    &
            dew,soilt,qvg,qsg,qcg,                           &
            edir1,ec1,ett1,eeta,qfx,hfx,s,evapl,             &
            prcpl,fltot,runoff1,runoff2,mavail,soilice,      &
            soiliqw,infiltrp,smf)


























































        IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                    
   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX
   LOGICAL,  INTENT(IN   )   ::  myj

   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL,                                                         &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                          GSWin, &
                                                          EMISS, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                            lai, &
                                                       infwater, &
                                                           QKMS, &
                                                           TKMS


   REAL,                                                         &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT, &
                                                            XLV, &
                                                            g0_p


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:5001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                             TSO, &
                                                       SOILMOIS, &
                                                       SMFRKEEP

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::          rstochcol
   REAL,     DIMENSION(1:NZS), INTENT(INOUT) ::     fieldcol_sf


   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                          KEEPFR


   REAL,                                                         &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                           DRIP, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          EVAPL, &
                                                          PRCPL, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                           RNET, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                            SAT, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                          SOILT


   INTEGER                   , INTENT(IN)  ::      spp_lsm   
   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::          SOILICE, &
                                                        SOILIQW



   REAL    ::  INFILTRP, transum                               , &
               RAINF,  PRCPMS                                  , &
               TABS, T3, UPFLUX, XINET
   REAL    ::  CP,rovcp,G0,LV,STBOLT,xlmelt,dzstop             , &
               can,epot,fac,fltot,ft,fq,hft                    , &
               q1,ras,rhoice,sph                               , &
               trans,zn,ci,cvw,tln,tavln,pi                    , &
               DD1,CMC2MS,DRYCAN,WETCAN                        , &
               INFMAX,RIW, X
   REAL,     DIMENSION(1:NZS)  ::  transp,cap,diffu,hydro      , &
                                   thdif,tranf,tav,soilmoism   , &
                                   soilicem,soiliqwm,detal     , &
                                   fwsat,lwsat,told,smold

   REAL                        ::  soiltold,smf
   REAL    :: soilres, alfa, fex, fex_fc, fc, psit

   INTEGER ::  nzs1,nzs2,k





        RHOICE=900.
        CI=RHOICE*2100.
        XLMELT=3.35E+5
        cvw=cw


        prcpl=prcpms

        smf=0.
        soiltold = soilt

        wetcan=0.
        drycan=1.


        DO K=1,NZS
          TRANSP   (K)=0.
          soilmoism(k)=0.
          soilice  (k)=0.
          soiliqw  (k)=0.
          soilicem (k)=0.
          soiliqwm (k)=0.
          lwsat    (k)=0.
          fwsat    (k)=0.
          tav      (k)=0.
          cap      (k)=0.
          thdif    (k)=0.
          diffu    (k)=0.
          hydro    (k)=0.   
          tranf    (k)=0.
          detal    (k)=0.
          told     (k)=0.
          smold    (k)=0.
        ENDDO

          NZS1=NZS-1
          NZS2=NZS-2
        dzstop=1./(zsmain(2)-zsmain(1))
        RAS=RHO*1.E-3
        RIW=rhoice*1.e-3



         DO K=1,NZS

         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                        &
         (tso(k)-273.15)/tso(k)/9.81/psis)                       &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/RIW


       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif

          ENDDO

          DO K=1,NZS1

         tav(k)=0.5*(tso(k)+tso(k+1))
         soilmoism(k)=0.5*(soilmois(k)+soilmois(k+1))
         tavln=log(tav(k)/273.15)

         if(tavln.lt.0.) then
           soiliqwm(k)=(dqm+qmin)*(XLMELT*                       &
         (tav(k)-273.15)/tav(k)/9.81/psis)                       &
          **(-1./bclh)-qmin
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
           soiliqwm(k)=max(0.,soiliqwm(k))
           soiliqwm(k)=min(soiliqwm(k), soilmoism(k))
           soilicem(k)=(soilmoism(k)-soiliqwm(k))/riw

       if(keepfr(k).eq.1.) then
           soilicem(k)=min(soilicem(k),                          &
                   0.5*(smfrkeep(k)+smfrkeep(k+1)))
           soiliqwm(k)=max(0.,soilmoism(k)-soilicem(k)*riw)
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
       endif

         else
           soilicem(k)=0.
           soiliqwm(k)=soilmoism(k)
           lwsat(k)=dqm+qmin
           fwsat(k)=0.
         endif

          ENDDO

          do k=1,nzs
           if(soilice(k).gt.0.) then
             smfrkeep(k)=soilice(k)
           else
             smfrkeep(k)=soilmois(k)/riw
           endif
          enddo





          CALL SOILPROP(spp_lsm,rstochcol,fieldcol_sf,       &

               nzs,fwsat,lwsat,tav,keepfr,                        &
               soilmois,soiliqw,soilice,                          &
               soilmoism,soiliqwm,soilicem,                       &

               QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,               &

               riw,xlmelt,CP,G0_P,cvw,ci,                         &
               kqwrtz,kice,kwt,                                   &

               thdif,diffu,hydro,cap)



 



        FQ=QKMS

        Q1=-QKMS*RAS*(QVATM - QSG)

        DEW=0.
        IF(QVATM.GE.QSG)THEN
          DEW=FQ*(QVATM-QSG)
        ENDIF




























          WETCAN=min(0.25,max(0.,(CST/SAT))**CN)

          DRYCAN=1.-WETCAN




           CALL TRANSF(i,j,                                   &

              nzs,nroot,soiliqw,tabs,lai,gswin,               &

              dqm,qmin,ref,wilt,zshalf,pc,iland,              &

              tranf,transum)



          do k=1,nzs
           told(k)=tso(k)
           smold(k)=soilmois(k)
          enddo



        alfa=1.

        fex=min(1.,soilmois(1)/dqm)
        fex=max(fex,0.01)
        psit=psis*fex ** (-bclh)
        psit = max(-1.e5, psit)
        alfa=min(1.,exp(g*psit/r_v/SOILT))

        alfa=1.

        fc=max(qmin,ref*0.5)
        fex_fc=1.
      if((soilmois(1)+qmin) > fc .or. (qvatm-qvg) > 0.) then
        soilres = 1.
      else
        fex_fc=min(1.,(soilmois(1)+qmin)/fc)
        fex_fc=max(fex_fc,0.01)
        soilres=0.25*(1.-cos(piconst*fex_fc))**2.
      endif
    IF ( wrf_at_debug_level(3000) ) THEN

     print *,'fex,psit,psis,bclh,g,r_v,soilt,alfa,mavail,soilmois(1),fc,ref,soilres,fex_fc', &
              fex,psit,psis,bclh,g,r_v,soilt,alfa,mavail,soilmois(1),fc,ref,soilres,fex_fc
    endif





        CALL SOILTEMP(                                        &

             i,j,iland,isoil,                                 &
             delt,ktau,conflx,nzs,nddzs,nroot,                &
             PRCPMS,RAINF,                                    &
             PATM,TABS,QVATM,QCATM,EMISS,RNET,                &
             QKMS,TKMS,PC,rho,vegfrac, lai,                   &
             thdif,cap,drycan,wetcan,                         & 
             transum,dew,mavail,soilres,alfa,                 &

             dqm,qmin,bclh,zsmain,zshalf,DTDZS,tbq,           &

             xlv,CP,G0_P,cvw,stbolt,                          &

             tso,soilt,qvg,qsg,qcg,x)




        ETT1=0.
        DEW=0.

        IF(QVATM.GE.QSG)THEN
          DEW=QKMS*(QVATM-QSG)
          ETT1=0.
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO
        ELSE

          DO K=1,NROOT
            TRANSP(K)=VEGFRAC*RAS*QKMS*                       &
                    (QVATM-QSG)*                              &
                    TRANF(K)*DRYCAN/ZSHALF(NROOT+1)
               IF(TRANSP(K).GT.0.) TRANSP(K)=0.
            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo
        ENDIF


         DO K=1,NZS

           tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                     &
          (tso(k)-273.15)/tso(k)/9.81/psis)                   & 
           **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw

       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif
         ENDDO





          CALL SOILMOIST (                                     &

               delt,nzs,nddzs,DTDZS,DTDZS2,RIW,                &
               zsmain,zshalf,diffu,hydro,                      &
               QSG,QVG,QCG,QCATM,QVATM,-infwater,              &
               QKMS,TRANSP,DRIP,DEW,0.,SOILICE,VEGFRAC,        &
               0.,soilres,                                     &

               DQM,QMIN,REF,KSAT,RAS,INFMAX,                   &

               SOILMOIS,SOILIQW,MAVAIL,RUNOFF1,                &
               RUNOFF2,INFILTRP)
        








 
        do k=1,nzs
       if (soilice(k).gt.0.) then
          if(tso(k).gt.told(k).and.soilmois(k).gt.smold(k)) then
              keepfr(k)=1.
          else
              keepfr(k)=0.
          endif
       endif
        enddo



          T3      = STBOLT*SOILTold*SOILTold*SOILTold
          UPFLUX  = T3 * 0.5*(SOILTold+SOILT)
          XINET   = EMISS*(GLW-UPFLUX)

          HFT=-TKMS*CP*RHO*(TABS-SOILT)
          HFX=-TKMS*CP*RHO*(TABS-SOILT)                        &
               *(P1000mb*0.00001/Patm)**ROVCP
          Q1=-QKMS*RAS*(QVATM - QSG)

          CMC2MS = 0.
        IF (Q1.LE.0.) THEN

          EC1=0.
          EDIR1=0.
          ETT1=0.
     if(myj) then

          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
          CST= CST-EETA*DELT*vegfrac
    IF ( wrf_at_debug_level(3000) ) THEN

        print *,'Cond MYJ EETA',eeta,eeta*xlv, i,j
    ENDIF
     else 

          EETA= - RHO*DEW
          CST=CST+DELT*DEW*RAS * vegfrac
    IF ( wrf_at_debug_level(3000) ) THEN


       print *,'Cond RUC LSM EETA',EETA,eeta*xlv, i,j
    ENDIF
     endif 
          QFX= XLV*EETA
          EETA= - RHO*DEW
        ELSE

          EDIR1 =-soilres*(1.-vegfrac)*QKMS*RAS*                      &
                  (QVATM-QVG)
          CMC2MS=CST/DELT*RAS
          EC1 = Q1 * WETCAN * vegfrac
    IF ( wrf_at_debug_level(3000) ) THEN

       print *,'CST before update=',cst
       print *,'EC1=',EC1,'CMC2MS=',CMC2MS
     ENDIF


          CST=max(0.,CST-EC1 * DELT)






     if (myj) then

          EETA=-soilres*QKMS*RAS*(QVATM/(1.+QVATM) - QVG/(1.+QVG))*1.E3
     else 
    IF ( wrf_at_debug_level(3000) ) THEN

       print *,'QKMS,RAS,QVATM/(1.+QVATM),QVG/(1.+QVG),QSG ', &
                QKMS,RAS,QVATM/(1.+QVATM),QVG/(1.+QVG),QSG
       print *,'Q1*(1.-vegfrac),EDIR1',Q1*(1.-vegfrac),EDIR1
       print *,'CST,WETCAN,DRYCAN',CST,WETCAN,DRYCAN
       print *,'EC1=',EC1,'ETT1=',ETT1,'CMC2MS=',CMC2MS,'CMC2MS*ras=',CMC2MS*ras

    ENDIF

          EETA = (EDIR1 + EC1 + ETT1)*1.E3
    IF ( wrf_at_debug_level(3000) ) THEN


        print *,'RUC LSM EETA',EETA,eeta*xlv
    ENDIF
     endif 
          QFX= XLV * EETA
          EETA = (EDIR1 + EC1 + ETT1)*1.E3
        ENDIF
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'potential temp HFT ',HFT
     print *,'abs temp HFX ',HFX
    ENDIF

          EVAPL=EETA
          S=THDIF(1)*CAP(1)*DZSTOP*(TSO(1)-TSO(2))

          FLTOT=RNET-HFT-XLV*EETA-S-X
    IF ( wrf_at_debug_level(3000) ) THEN

       print *,'SOIL - FLTOT,RNET,HFT,QFX,S,X=',i,j,FLTOT,RNET,HFT,XLV*EETA,s,x
       print *,'edir1,ec1,ett1,mavail,qkms,qvatm,qvg,qsg,vegfrac',&
                edir1,ec1,ett1,mavail,qkms,qvatm,qvg,qsg,vegfrac
    ENDIF
    if(detal(1) .ne. 0.) then


         smf=fltot
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'detal(1),xlmelt,soiliqwm(1),delt',detal(1),xlmelt,soiliqwm(1),delt
     print *,'Implicit phase change in the first layer - smf=',smf
    ENDIF
    endif


 222    CONTINUE

 1123    FORMAT(I5,8F12.3)
 1133    FORMAT(I7,8E12.4)
  123   format(i6,f6.2,7f8.1)
  122   FORMAT(1X,2I3,6F8.1,F8.3,F8.2)

   END SUBROUTINE SOIL


        SUBROUTINE SICE (                                       &

            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            PRCPMS,RAINF,PATM,QVATM,QCATM,GLW,GSW,              &
            EMISS,RNET,QKMS,TKMS,rho,myj,                       &

            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &

            xlv,CP,rovcp,cw,stbolt,tabs,                        &

            tso,dew,soilt,qvg,qsg,qcg,                          &
            eeta,qfx,hfx,s,evapl,prcpl,fltot                    &
                                                                )






        IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                    
   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX
   LOGICAL,  INTENT(IN   )   ::  myj

   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL,                                                         &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                          EMISS, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS

   REAL,    DIMENSION(1:NZS)                                   , &
            INTENT(IN   )    ::                                  &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice


   REAL,     INTENT(IN   )   ::                                  &
                                                             CW, &
                                                            XLV


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:5001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION( 1:nzs ),  INTENT(INOUT)   ::        TSO

   REAL,                                                         &
             INTENT(INOUT)   ::                             DEW, &
                                                           EETA, &
                                                          EVAPL, &
                                                          PRCPL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                           RNET, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                          SOILT


   REAL    ::  x,x1,x2,x4,tn,denom
   REAL    ::  RAINF,  PRCPMS                                  , &
               TABS, T3, UPFLUX, XINET

   REAL    ::  CP,rovcp,G0,LV,STBOLT,xlmelt,dzstop             , &
               epot,fltot,ft,fq,hft,ras,cvw                    

   REAL    ::  FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11     , &
               PI,H,FKQ,R210,AA,BB,PP,Q1,QS1,TS1,TQ2,TX2       , &
               TDENOM,QGOLD,SNOH

   REAL    ::  AA1,RHCS, icemelt


   REAL,     DIMENSION(1:NZS)  ::   cotso,rhtso

   INTEGER ::  nzs1,nzs2,k,k1,kn,kk





        XLMELT=3.35E+5
        cvw=cw

        prcpl=prcpms

          NZS1=NZS-1
          NZS2=NZS-2
        dzstop=1./(zsmain(2)-zsmain(1))
        RAS=RHO*1.E-3

        do k=1,nzs
           cotso(k)=0.
           rhtso(k)=0.
        enddo

        cotso(1)=0.
        rhtso(1)=TSO(NZS)

        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIFICE(KN-1)
          X2=DTDZS(K1+1)*THDIFICE(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                             &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE



        RHCS=CAPICE(1)
        H=1.
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIFICE(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIFICE(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6
        TDENOM=D9*(1.-D1+R22)+D10+R21+R7                              &
              +RAINF*CVW*PRCPMS
        FKQ=QKMS*RHO
        R210=R211*RHO
        AA=XLS*(FKQ+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLS*(QVATM*FKQ                            &
        +R210*QVG)+D11+D9*(D2+R22*TN)                                 &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                            &
         )/TDENOM
        AA1=AA
        PP=PATM*1.E3
        AA1=AA1/PP
    IF ( wrf_at_debug_level(3000) ) THEN
        PRINT *,' VILKA-SEAICE1'
        print *,'D10,TABS,R21,TN,QVATM,FKQ',                          &
                 D10,TABS,R21,TN,QVATM,FKQ
        print *,'RNET, EMISS, STBOLT, SOILT',RNET, EMISS, STBOLT, SOILT
        print *,'R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM',     &
                 R210,QVG,D11,D9,D2,R22,RAINF,CVW,PRCPMS,TDENOM
        print *,'tn,aa1,bb,pp,fkq,r210',                              &
                 tn,aa1,bb,pp,fkq,r210
    ENDIF
        QGOLD=QSG
        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)

        QVG=QS1
        QSG=QS1
        TSO(1)=min(271.4,TS1)
        QCG=0.


          SOILT=TSO(1)

          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=min(271.4,rhtso(KK)+cotso(KK)*TSO(K-1))
          END DO

        DEW=0.


          T3      = STBOLT*TN*TN*TN
          UPFLUX  = T3 *0.5*(TN+SOILT)
          XINET   = EMISS*(GLW-UPFLUX)

          HFT=-TKMS*CP*RHO*(TABS-SOILT)
          HFX=-TKMS*CP*RHO*(TABS-SOILT)                        &
               *(P1000mb*0.00001/Patm)**ROVCP
          Q1=-QKMS*RAS*(QVATM - QSG)
        IF (Q1.LE.0.) THEN

     if(myj) then

          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'MYJ EETA',eeta
    ENDIF
     else 

          DEW=QKMS*(QVATM-QSG)
          EETA= - RHO*DEW
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'RUC LSM EETA',eeta
    ENDIF
     endif 
          QFX= XLS*EETA
          EETA= - RHO*DEW
        ELSE

     if(myj) then

          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QVG/(1.+QVG))*1.E3
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'MYJ EETA',eeta
    ENDIF
     else 


          EETA = Q1*1.E3
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'RUC LSM EETA',eeta
    ENDIF
     endif 
          QFX= XLS * EETA
          EETA = Q1*1.E3
        ENDIF
          EVAPL=EETA

          S=THDIFICE(1)*CAPICE(1)*DZSTOP*(TSO(1)-TSO(2))

        SNOH=0.

         X= (cp*rho*r211+rhcs*zsmain(2)*0.5/delt)*(SOILT-TN) +   &
            XLS*rho*r211*(QSG-QGOLD)
         X=X &

        -RAINF*CVW*PRCPMS*(max(273.15,TABS)-SOILT)


        icemelt=RNET-XLS*EETA -HFT -S -X
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,'icemelt=',icemelt
    ENDIF

          FLTOT=RNET-XLS*EETA-HFT-S-X-icemelt
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'SICE - FLTOT,RNET,HFT,QFX,S,SNOH,X=', &
                       FLTOT,RNET,HFT,XLS*EETA,s,icemelt,X
    ENDIF


   END SUBROUTINE SICE




        SUBROUTINE SNOWSOIL (spp_lsm,rstochcol,fieldcol_sf,&

             i,j,isoil,delt,ktau,conflx,nzs,nddzs,nroot,       &
             meltfactor,rhonewsn,SNHEI_CRIT,                   & 
             ILAND,PRCPMS,RAINF,NEWSNOW,snhei,SNWE,SNOWFRAC,   &
             RHOSN,                                            &
             PATM,QVATM,QCATM,                                 &
             GLW,GSW,GSWin,EMISS,RNET,IVGTYP,                  &
             QKMS,TKMS,PC,cst,drip,infwater,                   &
             rho,vegfrac,alb,znt,lai,                          & 
             MYJ,                                              &

             QWRTZ,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,     &
             sat,cn,zsmain,zshalf,DTDZS,DTDZS2,tbq,            &

             xlv,CP,rovcp,G0_P,cw,stbolt,TABS,                 &
             KQWRTZ,KICE,KWT,                                  &

             ilnb,snweprint,snheiprint,rsm,                    &
             soilmois,tso,smfrkeep,keepfr,                     &
             dew,soilt,soilt1,tsnav,                           &
             qvg,qsg,qcg,SMELT,SNOH,SNFLX,SNOM,                &
             edir1,ec1,ett1,eeta,qfx,hfx,s,sublim,             &
             prcpl,fltot,runoff1,runoff2,mavail,soilice,             &
             soiliqw,infiltrp                                  )





































































        IMPLICIT NONE



   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs     ,            &
                                 nddzs                         
   INTEGER,  INTENT(IN   )   ::  i,j,isoil

   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS            , &
                                 RAINF,NEWSNOW,RHONEWSN,         &
                                 SNHEI_CRIT,meltfactor

   LOGICAL,    INTENT(IN   )    ::     myj


   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                          GSWin, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                            lai, &
                                                       infwater, &
                                                           QKMS, &
                                                           TKMS

   INTEGER,  INTENT(IN   )   ::                          IVGTYP

   REAL                                                        , &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                            SAT, &
                                                           WILT

   REAL,     INTENT(IN   )   ::                              CN, &
                                                             CW, &
                                                            XLV, &
                                                           G0_P, & 
                                                         KQWRTZ, &
                                                           KICE, &
                                                            KWT 


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:5001), INTENT(IN)  ::              TBQ

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::          rstochcol
   REAL,     DIMENSION(1:NZS), INTENT(INOUT) ::     fieldcol_sf



   REAL,     DIMENSION(  1:nzs )                               , &
             INTENT(INOUT)   ::                             TSO, &
                                                       SOILMOIS, &
                                                       SMFRKEEP

   REAL,  DIMENSION( 1:nzs )                                   , &
             INTENT(INOUT)   ::                          KEEPFR


   INTEGER,  INTENT(INOUT)    ::                           ILAND



   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                           DRIP, &
                                                          EDIR1, &
                                                            EC1, &
                                                           ETT1, &
                                                           EETA, &
                                                          RHOSN, &
                                                         SUBLIM, &
                                                          PRCPL, &
                                                            ALB, &
                                                          EMISS, &
                                                            ZNT, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                        RUNOFF1, &
                                                        RUNOFF2, &
                                                           SNWE, &
                                                          SNHEI, &
                                                          SMELT, &
                                                           SNOM, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                       SNOWFRAC, &
                                                          TSNAV

   INTEGER, INTENT(INOUT)    ::                            ILNB


   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::          SOILICE, &
                                                        SOILIQW

   REAL,     INTENT(OUT)                    ::              RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT
   INTEGER,  INTENT(IN)                    ::       spp_lsm 



   INTEGER ::  nzs1,nzs2,k

   REAL    ::  INFILTRP, TRANSUM                               , &
               SNTH, NEWSN                                     , &
               TABS, T3, UPFLUX, XINET                         , &
               BETA, SNWEPR,EPDT,PP
   REAL    ::  CP,rovcp,G0,LV,xlvm,STBOLT,xlmelt,dzstop        , &
               can,epot,fac,fltot,ft,fq,hft                    , &
               q1,ras,rhoice,sph                               , &
               trans,zn,ci,cvw,tln,tavln,pi                    , &
               DD1,CMC2MS,DRYCAN,WETCAN                        , &
               INFMAX,RIW,DELTSN,H,UMVEG

   REAL,     DIMENSION(1:NZS)  ::  transp,cap,diffu,hydro      , &
                                   thdif,tranf,tav,soilmoism   , &
                                   soilicem,soiliqwm,detal     , &
                                   fwsat,lwsat,told,smold
   REAL                        ::  soiltold, qgold

   REAL                        ::  RNET, X



        cvw=cw
        XLMELT=3.35E+5

        XLVm=XLV+XLMELT














       soiltold=soilt
       qgold=qvg

       x=0.





           DELTSN=0.05*1.e3/rhosn
           snth=0.01*1.e3/rhosn








        IF(SNHEI.GE.DELTSN+SNTH) THEN
          if(snhei-deltsn-snth.lt.snth) deltsn=0.5*(snhei-snth)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'DELTSN is changed,deltsn,snhei,snth',i,j,deltsn,snhei,snth
    ENDIF
        ENDIF 

        RHOICE=900.
        CI=RHOICE*2100.
        RAS=RHO*1.E-3
        RIW=rhoice*1.e-3

        RSM=0.

        DO K=1,NZS
          TRANSP     (K)=0.
          soilmoism  (k)=0.
          soiliqwm   (k)=0.
          soilice    (k)=0.
          soilicem   (k)=0.
          lwsat      (k)=0.
          fwsat      (k)=0.
          tav        (k)=0.
          cap        (k)=0.
          diffu      (k)=0.
          hydro      (k)=0.
          thdif      (k)=0.  
          tranf      (k)=0.
          detal      (k)=0.
          told       (k)=0.
          smold      (k)=0. 
        ENDDO

        snweprint=0.
        snheiprint=0.
        prcpl=prcpms






          NZS1=NZS-1
          NZS2=NZS-2
        DZSTOP=1./(zsmain(2)-zsmain(1))





         DO K=1,NZS

         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                          &
         (tso(k)-273.15)/tso(k)/9.81/psis)                         &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw


       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*rhoice*1.e-3)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif

          ENDDO

          DO K=1,NZS1

         tav(k)=0.5*(tso(k)+tso(k+1))
         soilmoism(k)=0.5*(soilmois(k)+soilmois(k+1))
         tavln=log(tav(k)/273.15)

         if(tavln.lt.0.) then
           soiliqwm(k)=(dqm+qmin)*(XLMELT*                         &
         (tav(k)-273.15)/tav(k)/9.81/psis)                         &
          **(-1./bclh)-qmin
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
           soiliqwm(k)=max(0.,soiliqwm(k))
           soiliqwm(k)=min(soiliqwm(k), soilmoism(k))
           soilicem(k)=(soilmoism(k)-soiliqwm(k))/riw

       if(keepfr(k).eq.1.) then
           soilicem(k)=min(soilicem(k),                            &
                    0.5*(smfrkeep(k)+smfrkeep(k+1)))
           soiliqwm(k)=max(0.,soilmoism(k)-soilicem(k)*riw)
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
       endif

         else
           soilicem(k)=0.
           soiliqwm(k)=soilmoism(k)
           lwsat(k)=dqm+qmin
           fwsat(k)=0.

         endif
          ENDDO

          do k=1,nzs
           if(soilice(k).gt.0.) then
             smfrkeep(k)=soilice(k)
           else
             smfrkeep(k)=soilmois(k)/riw
           endif
          enddo





          CALL SOILPROP(spp_lsm,rstochcol,fieldcol_sf,      &

               nzs,fwsat,lwsat,tav,keepfr,                       &
               soilmois,soiliqw,soilice,                         &
               soilmoism,soiliqwm,soilicem,                      &

               QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,              & 

               riw,xlmelt,CP,G0_P,cvw,ci,                        &
               kqwrtz,kice,kwt,                                  &

               thdif,diffu,hydro,cap)



 

        SMELT=0.

        H=1.

        FQ=QKMS





        DEW=0.
        UMVEG=1.-vegfrac
        EPOT = -FQ*(QVATM-QSG) 

    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SNWE after subtracting intercepted snow - snwe=',snwe,vegfrac,cst
    ENDIF
          SNWEPR=SNWE


         BETA=1.
         EPDT = EPOT * RAS *DELT*UMVEG
         IF(EPDT.gt.0. .and. SNWEPR.LE.EPDT) THEN 
            BETA=SNWEPR/max(1.e-8,EPDT)
            SNWE=0.
         ENDIF

          WETCAN=min(0.25,max(0.,(CST/SAT))**CN)

          DRYCAN=1.-WETCAN




           CALL TRANSF(i,j,                                   &

              nzs,nroot,soiliqw,tabs,lai,gswin,               &

              dqm,qmin,ref,wilt,zshalf,pc,iland,              & 

              tranf,transum)


          do k=1,nzs
           told(k)=tso(k)
           smold(k)=soilmois(k)
          enddo





    IF ( wrf_at_debug_level(3000) ) THEN
print *, 'TSO before calling SNOWTEMP: ', tso
    ENDIF
        CALL SNOWTEMP(                                        &

             i,j,iland,isoil,                                 &
             delt,ktau,conflx,nzs,nddzs,nroot,                &
             snwe,snwepr,snhei,newsnow,snowfrac,              &
             beta,deltsn,snth,rhosn,rhonewsn,meltfactor,      &  
             PRCPMS,RAINF,                                    &
             PATM,TABS,QVATM,QCATM,                           &
             GLW,GSW,EMISS,RNET,                              &
             QKMS,TKMS,PC,rho,vegfrac,                        &
             thdif,cap,drycan,wetcan,cst,                     &
             tranf,transum,dew,mavail,                        &

             dqm,qmin,psis,bclh,                              &
             zsmain,zshalf,DTDZS,tbq,                         &

             xlvm,CP,rovcp,G0_P,cvw,stbolt,                   &

             snweprint,snheiprint,rsm,                        &
             tso,soilt,soilt1,tsnav,qvg,qsg,qcg,              &
             smelt,snoh,snflx,s,ilnb,x)



         DEW=0.
         ETT1=0.
         PP=PATM*1.E3
         EPOT = -FQ*(QVATM-QSG)
       IF(EPOT.GT.0.) THEN

          DO K=1,NROOT
            TRANSP(K)=vegfrac*RAS*FQ*(QVATM-QSG)              &
                     *tranf(K)*DRYCAN/zshalf(NROOT+1)

            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo

        ELSE

          DEW=-EPOT
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO
        ETT1=0.
        ENDIF


         DO K=1,NZS
         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(XLMELT*                    &
         (tso(k)-273.15)/tso(k)/9.81/psis)                   &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw

       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif
         ENDDO





                CALL SOILMOIST (                                   &

               delt,nzs,nddzs,DTDZS,DTDZS2,RIW,                    &
               zsmain,zshalf,diffu,hydro,                          &
               QSG,QVG,QCG,QCATM,QVATM,-INFWATER,                  &
               QKMS,TRANSP,0.,                                     &
               0.,SMELT,soilice,vegfrac,                           &
               snowfrac,1.,                                        &

               DQM,QMIN,REF,KSAT,RAS,INFMAX,                       &

               SOILMOIS,SOILIQW,MAVAIL,RUNOFF1,                    &
               RUNOFF2,infiltrp) 
 



         IF(SNHEI.EQ.0.)  then
          tsnav=soilt-273.15
         ENDIF



        SNOM=SNOM+SMELT*DELT*1.e3










        do k=1,nzs
       if (soilice(k).gt.0.) then
          if(tso(k).gt.told(k).and.soilmois(k).gt.smold(k)) then
              keepfr(k)=1.
          else
              keepfr(k)=0.
          endif
       endif
        enddo


        T3      = STBOLT*SOILTold*SOILTold*SOILTold
        UPFLUX  = T3 *0.5*(SOILTold+SOILT)
        XINET   = EMISS*(GLW-UPFLUX)   

        HFX=-TKMS*CP*RHO*(TABS-SOILT)                        &
               *(P1000mb*0.00001/Patm)**ROVCP
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'potential temp HFX',hfx
    ENDIF
        HFT=-TKMS*CP*RHO*(TABS-SOILT) 
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'abs temp HFX',hft
    ENDIF
        Q1 = - FQ*RAS* (QVATM - QSG)
        CMC2MS=0.
        IF (Q1.LT.0.) THEN

        EDIR1=0.
        EC1=0.
        ETT1=0.

     if(myj) then

          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
          CST= CST-EETA*DELT*vegfrac
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'MYJ EETA cond', EETA
    ENDIF
     else 

          DEW=QKMS*(QVATM-QSG)
          EETA= - RHO*DEW
          CST=CST+DELT*DEW*RAS * vegfrac
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'RUC LSM EETA cond',EETA
    ENDIF
     endif 
          QFX= XLVm*EETA
          EETA= - RHO*DEW
        ELSE

        EDIR1 = Q1*UMVEG *BETA
        CMC2MS=CST/DELT*RAS
        EC1 = Q1 * WETCAN * vegfrac

        CST=max(0.,CST-EC1 * DELT)






    IF ( wrf_at_debug_level(3000) ) THEN
     print*,'Q1,umveg,beta',Q1,umveg,beta
     print *,'wetcan,vegfrac',wetcan,vegfrac
     print *,'EC1,CMC2MS',EC1,CMC2MS
    ENDIF

     if(myj) then

        EETA=-(QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3)*BETA
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'MYJ EETA', EETA*XLVm,EETA
    ENDIF
     else 


        EETA = (EDIR1 + EC1 + ETT1)*1.E3
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'RUC LSM EETA',EETA*XLVm,EETA
    ENDIF
     endif 
        QFX= XLVm * EETA
        EETA = (EDIR1 + EC1 + ETT1)*1.E3
       ENDIF
        S=SNFLX

        sublim=EDIR1*1.E3

        FLTOT=RNET-HFT-XLVm*EETA-S-SNOH-x
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'SNOWSOIL - FLTOT,RNET,HFT,QFX,S,SNOH,X=',FLTOT,RNET,HFT,XLVm*EETA,s,SNOH,X
       print *,'edir1,ec1,ett1,mavail,qkms,qvatm,qvg,qsg,vegfrac,beta',&
                edir1,ec1,ett1,mavail,qkms,qvatm,qvg,qsg,vegfrac,beta
    ENDIF

 222     CONTINUE

 1123    FORMAT(I5,8F12.3)
 1133    FORMAT(I7,8E12.4)
  123   format(i6,f6.2,7f8.1)
 122    FORMAT(1X,2I3,6F8.1,F8.3,F8.2)


   END SUBROUTINE SNOWSOIL


           SUBROUTINE SNOWSEAICE(                               &
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,               &
            meltfactor,rhonewsn,SNHEI_CRIT,                     &  
            ILAND,PRCPMS,RAINF,NEWSNOW,snhei,SNWE,snowfrac,     &
            RHOSN,PATM,QVATM,QCATM,                             &
            GLW,GSW,EMISS,RNET,                                 &
            QKMS,TKMS,RHO,myj,                                  &

            ALB,ZNT,                                            &
            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,DTDZS,DTDZS2,tbq,                     &

            xlv,CP,rovcp,cw,stbolt,tabs,                        &

            ilnb,snweprint,snheiprint,rsm,tso,                  &
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &
            SMELT,SNOH,SNFLX,SNOM,eeta,                         &
            qfx,hfx,s,sublim,prcpl,fltot                        &
                                                                )






        IMPLICIT NONE



   INTEGER,  INTENT(IN   )   ::  ktau,nzs     ,                  &
                                 nddzs                         
   INTEGER,  INTENT(IN   )   ::  i,j,isoil

   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS            , &
                                 RAINF,NEWSNOW,RHONEWSN,         &
                                 meltfactor, snhei_crit
   real                      ::  rhonewcsn

   LOGICAL,  INTENT(IN   )   ::  myj

   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                            RHO, &
                                                           QKMS, &
                                                           TKMS


   REAL,     DIMENSION(1:NZS)                                  , &
            INTENT(IN   )    ::                                  &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice

   REAL,     INTENT(IN   )   ::                                  &
                                                             CW, &
                                                            XLV

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:5001), INTENT(IN)  ::              TBQ



   REAL,     DIMENSION(  1:nzs )                               , &
             INTENT(INOUT)   ::                             TSO

   INTEGER,  INTENT(INOUT)    ::                           ILAND



   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                           EETA, &
                                                          RHOSN, &
                                                         SUBLIM, &
                                                          PRCPL, &
                                                            ALB, &
                                                          EMISS, &
                                                            ZNT, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                            QFX, &
                                                            HFX, &
                                                              S, &
                                                           SNWE, &
                                                          SNHEI, &
                                                          SMELT, &
                                                           SNOM, &
                                                           SNOH, &
                                                          SNFLX, &
                                                          SOILT, &
                                                         SOILT1, &
                                                       SNOWFRAC, &
                                                          TSNAV

   INTEGER, INTENT(INOUT)    ::                            ILNB

   REAL,     INTENT(OUT)                    ::              RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT



   INTEGER ::  nzs1,nzs2,k,k1,kn,kk
   REAL    ::  x,x1,x2,dzstop,ft,tn,denom

   REAL    ::  SNTH, NEWSN                                     , &
               TABS, T3, UPFLUX, XINET                         , &
               BETA, SNWEPR,EPDT,PP
   REAL    ::  CP,rovcp,G0,LV,xlvm,STBOLT,xlmelt               , &
               epot,fltot,fq,hft,q1,ras,rhoice,ci,cvw          , &
               RIW,DELTSN,H

   REAL    ::  rhocsn,thdifsn,                                   &
               xsn,ddzsn,x1sn,d1sn,d2sn,d9sn,r22sn

   REAL    ::  cotsn,rhtsn,xsn1,ddzsn1,x1sn1,ftsnow,denomsn
   REAL    ::  fso,fsn,                                          &
               FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11,      &
               FKQ,R210,AA,BB,QS1,TS1,TQ2,TX2,                   &
               TDENOM,AA1,RHCS,H1,TSOB, SNPRIM,                  &
               SNODIF,SOH,TNOLD,QGOLD,SNOHGNEW
   REAL,     DIMENSION(1:NZS)  ::  cotso,rhtso

   REAL                   :: RNET,rsmfrac,soiltfrac,hsn,icemelt,rr
   integer                ::      nmelt



        XLMELT=3.35E+5

        XLVm=XLV+XLMELT

















           DELTSN=0.05*1.e3/rhosn
           snth=0.01*1.e3/rhosn




        IF(SNHEI.GE.DELTSN+SNTH) THEN
          if(snhei-deltsn-snth.lt.snth) deltsn=0.5*(snhei-snth)
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,'DELTSN ICE is changed,deltsn,snhei,snth', &
                                  i,j, deltsn,snhei,snth
    ENDIF
        ENDIF

        RHOICE=900.
        CI=RHOICE*2100.
        RAS=RHO*1.E-3
        RIW=rhoice*1.e-3
        RSM=0.

        XLMELT=3.35E+5
        RHOCSN=2090.* RHOSN

        RHOnewCSN=2090.* RHOnewSN
        THDIFSN = 0.265/RHOCSN
        RAS=RHO*1.E-3

        SOILTFRAC=SOILT

        SMELT=0.
        SOH=0.
        SNODIF=0.
        SNOH=0.
        SNOHGNEW=0.
        RSM = 0.
        RSMFRAC = 0.
        fsn=1.
        fso=0.
        cvw=cw

          NZS1=NZS-1
          NZS2=NZS-2

        QGOLD=QSG
        TNOLD=SOILT
        DZSTOP=1./(ZSMAIN(2)-ZSMAIN(1))

        snweprint=0.
        snheiprint=0.
        prcpl=prcpms






        H=1.
        SMELT=0.

        FQ=QKMS
        SNHEI=SNWE*1.e3/RHOSN
          SNWEPR=SNWE


         BETA=1.
         EPOT = -FQ*(QVATM-QSG)
         EPDT = EPOT * RAS *DELT
         IF(EPDT.GT.0. .and. SNWEPR.LE.EPDT) THEN
            BETA=SNWEPR/max(1.e-8,EPDT)
            SNWE=0.
         ENDIF





        cotso(1)=0.
        rhtso(1)=TSO(NZS)
        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIFICE(KN-1)
          X2=DTDZS(K1+1)*THDIFICE(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                           &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE


       IF(SNHEI.GE.SNTH) then
        if(snhei.le.DELTSN+SNTH) then

         ilnb=1
         snprim=max(snth,snhei)
         soilt1=tso(1)
         tsob=tso(1)
         XSN = DELT/2./(zshalf(2)+0.5*SNPRIM)
         DDZSN = XSN / SNPRIM
         X1SN = DDZSN * thdifsn
         X2 = DTDZS(1)*THDIFICE(1)
         FT = TSO(1)+X1SN*(SOILT-TSO(1))                              &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN + X2 -X2*cotso(NZS1)
         cotso(NZS)=X1SN/DENOM
         rhtso(NZS)=(FT+X2*rhtso(NZS1))/DENOM
         cotsn=cotso(NZS)
         rhtsn=rhtso(NZS)

         tsnav=0.5*(soilt+tso(1))                                     &
                     -273.15

        else

         ilnb=2
         snprim=deltsn
         tsob=soilt1
         XSN = DELT/2./(0.5*SNHEI)
         XSN1= DELT/2./(zshalf(2)+0.5*(SNHEI-DELTSN))
         DDZSN = XSN / DELTSN
         DDZSN1 = XSN1 / (SNHEI-DELTSN)
         X1SN = DDZSN * thdifsn
         X1SN1 = DDZSN1 * thdifsn
         X2 = DTDZS(1)*THDIFICE(1)
         FT = TSO(1)+X1SN1*(SOILT1-TSO(1))                            &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN1 + X2 - X2*cotso(NZS1)
         cotso(nzs)=x1sn1/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         ftsnow = soilt1+x1sn*(soilt-soilt1)                          &
               -x1sn1*(soilt1-tso(1))
         denomsn = 1. + X1SN + X1SN1 - X1SN1*cotso(NZS)
         cotsn=x1sn/denomsn
         rhtsn=(ftsnow+X1SN1*rhtso(NZS))/denomsn

         tsnav=0.5/snhei*((soilt+soilt1)*deltsn                       &
                     +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                     -273.15
        endif
       ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then


         snprim=SNHEI+zsmain(2)
         fsn=SNHEI/snprim
         fso=1.-fsn
         soilt1=tso(1)
         tsob=tso(2)
         XSN = DELT/2./((zshalf(3)-zsmain(2))+0.5*snprim)
         DDZSN = XSN /snprim
         X1SN = DDZSN * (fsn*thdifsn+fso*thdifice(1))
         X2=DTDZS(2)*THDIFICE(2)
         FT=TSO(2)+X1SN*(SOILT-TSO(2))-                              &
                       X2*(TSO(2)-TSO(3))
         denom = 1. + x1sn + x2 - x2*cotso(nzs-2)
         cotso(nzs1) = x1sn/denom
         rhtso(nzs1)=(FT+X2*rhtso(NZS-2))/denom
         tsnav=0.5*(soilt+tso(1))                                    &
                     -273.15
         cotso(nzs)=cotso(NZS1)
         rhtso(nzs)=rhtso(nzs1)
         cotsn=cotso(NZS)
         rhtsn=rhtso(NZS)
       ENDIF




       nmelt=0
       SNOH=0.

        EPOT=-QKMS*(QVATM-QSG)
        RHCS=CAPICE(1)
        H=1.
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIFICE(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIFICE(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6

      IF(SNHEI.GE.SNTH) THEN 
        if(snhei.le.DELTSN+SNTH) then

          D1SN = cotso(NZS)
          D2SN = rhtso(NZS)
        else

          D1SN = cotsn
          D2SN = rhtsn
        endif
        D9SN= THDIFSN*RHOCSN / SNPRIM
        R22SN = SNPRIM*SNPRIM*0.5/(THDIFSN*DELT)
      ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then

         D1SN = D1
         D2SN = D2
         D9SN = (fsn*THDIFSN*RHOCSN+fso*THDIFICE(1)*RHCS)/           &
                 snprim
         R22SN = snprim*snprim*0.5                                   &
                 /((fsn*THDIFSN+fso*THDIFICE(1))*delt)
      ENDIF

      IF(SNHEI.eq.0.)then

        D9SN = D9
        R22SN = R22
        D1SN = D1
        D2SN = D2
      ENDIF



        TDENOM = D9SN*(1.-D1SN +R22SN)+D10+R21+R7                    &
              +RAINF*CVW*PRCPMS                                      &
              +RHOnewCSN*NEWSNOW/DELT

        FKQ=QKMS*RHO
        R210=R211*RHO
        AA=XLVM*(BETA*FKQ+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLVM*(QVATM*                             &
        (BETA*FKQ)                                                   &
        +R210*QVG)+D11+D9SN*(D2SN+R22SN*TN)                          &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                           &
        + RHOnewCSN*NEWSNOW/DELT*min(273.15,TABS)                    &
         )/TDENOM
        AA1=AA
        PP=PATM*1.E3
        AA1=AA1/PP

 212    continue
        BB=BB-SNOH/TDENOM
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,'VILKA-SNOW on SEAICE'
        print *,'tn,aa1,bb,pp,fkq,r210',                             &
                 tn,aa1,bb,pp,fkq,r210
        print *,'TABS,QVATM,TN,QVG=',TABS,QVATM,TN,QVG
    ENDIF

        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)

        QVG=QS1
        QSG=QS1
        QCG=0.


        SOILT=TS1

    IF ( wrf_at_debug_level(3000) ) THEN
        print *,' AFTER VILKA-SNOW on SEAICE'
        print *,' TS1,QS1: ', ts1,qs1
    ENDIF

       IF(SNHEI.GE.SNTH) THEN
        if(snhei.gt.DELTSN+SNTH) then

          SOILT1=min(273.15,rhtsn+cotsn*SOILT)
          TSO(1)=min(271.4,(rhtso(NZS)+cotso(NZS)*SOILT1))
          tsob=soilt1
        else

          TSO(1)=min(271.4,(rhtso(NZS)+cotso(NZS)*SOILT))
          SOILT1=TSO(1)
          tsob=tso(1)
        endif
       ELSEIF  (SNHEI > 0. .and. SNHEI < SNTH) THEN

         TSO(2)=min(271.4,(rhtso(NZS1)+cotso(NZS1)*SOILT))
         tso(1)=min(271.4,(tso(2)+(soilt-tso(2))*fso))
         SOILT1=TSO(1)
         tsob=TSO(2)
       ELSE

         TSO(1)=min(271.4,SOILT)
         SOILT1=min(271.4,SOILT)
         tsob=tso(1)
       ENDIF

       IF (SNHEI > 0. .and. SNHEI < SNTH) THEN

          DO K=3,NZS
            KK=NZS-K+1
            TSO(K)=min(271.4,rhtso(KK)+cotso(KK)*TSO(K-1))
          END DO
       ELSE
          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=min(271.4,rhtso(KK)+cotso(KK)*TSO(K-1))
          END DO
       ENDIF









      if(nmelt.eq.1) go to 220




   IF(SOILT.GT.273.15.AND.SNWEPR-BETA*EPOT*RAS*DELT.GT.0..AND.SNHEI.GT.0.) THEN

        nmelt = 1

        soiltfrac=snowfrac*273.15+(1.-snowfrac)*min(271.4,SOILT)

        QSG= QSN(soiltfrac,TBQ)/PP
        T3      = STBOLT*TNold*TNold*TNold
        UPFLUX  = T3 * 0.5*(TNold+SOILTfrac)
        XINET   = EMISS*(GLW-UPFLUX)

         EPOT = -QKMS*(QVATM-QSG)
         Q1=EPOT*RAS

        IF (Q1.LE.0.) THEN

          DEW=-EPOT

        QFX= XLVM*RHO*DEW
        EETA=QFX/XLVM
       ELSE

        EETA = Q1 * BETA *1.E3

        QFX= - XLVM * EETA
       ENDIF

         HFX=D10*(TABS-soiltfrac)

       IF(SNHEI.GE.SNTH)then
         SOH=thdifsn*RHOCSN*(soiltfrac-TSOB)/SNPRIM
         SNFLX=SOH
       ELSE
         SOH=(fsn*thdifsn*rhocsn+fso*thdifice(1)*rhcs)*                &
              (soiltfrac-TSOB)/snprim
         SNFLX=SOH
       ENDIF
         X= (R21+D9SN*R22SN)*(soiltfrac-TNOLD) +                        &
            XLVM*R210*(QSG-QGOLD)

        SNOH=RNET+QFX +HFX                                              &
                  +RHOnewCSN*NEWSNOW/DELT*(min(273.15,TABS)-soiltfrac)  &
                  -SOH-X+RAINF*CVW*PRCPMS*                              &
                  (max(273.15,TABS)-soiltfrac)

    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'SNOWSEAICE melt I,J,SNOH,RNET,QFX,HFX,SOH,X',i,j,SNOH,RNET,QFX,HFX,SOH,X
     print *,'RHOnewCSN*NEWSNOW/DELT*(min(273.15,TABS)-soiltfrac)',     &
              RHOnewCSN*NEWSNOW/DELT*(min(273.15,TABS)-soiltfrac)
     print *,'RAINF*CVW*PRCPMS*(max(273.15,TABS)-soiltfrac)',           &
              RAINF*CVW*PRCPMS*(max(273.15,TABS)-soiltfrac)
    ENDIF
        SNOH=AMAX1(0.,SNOH)

        SMELT= SNOH /XLMELT*1.E-3
        SMELT=AMIN1(SMELT,SNWEPR/DELT-BETA*EPOT*RAS)
        SMELT=AMAX1(0.,SMELT)

    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'1-SMELT i,j',smelt,i,j
    ENDIF


        SMELT= amin1 (smelt, 5.6E-8*meltfactor*max(1.,(soilt-273.15)))
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'2-SMELT i,j',smelt,i,j
    ENDIF


        rr=SNWEPR/delt-BETA*EPOT*RAS
        SMELT=min(SMELT,rr)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'3- SMELT i,j,smelt,rr',i,j,smelt,rr
    ENDIF
        SNOHGNEW=SMELT*XLMELT*1.E3
        SNODIF=AMAX1(0.,(SNOH-SNOHGNEW))

        SNOH=SNOHGNEW

    IF ( wrf_at_debug_level(3000) ) THEN
       print*,'soiltfrac,soilt,SNOHGNEW,SNODIF=', &
            i,j,soiltfrac,soilt,snohgnew,snodif
       print *,'SNOH,SNODIF',SNOH,SNODIF
    ENDIF


        rsmfrac=min(0.18,(max(0.08,snwepr/0.10*0.13)))
       if(snhei > 0.01) then
        rsm=rsmfrac*smelt*delt
       else

        rsm=0.
       endif

        SMELT=AMAX1(0.,SMELT-rsm/delt)
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'4-SMELT i,j,smelt,rsm,snwepr,rsmfrac', &
                    i,j,smelt,rsm,snwepr,rsmfrac
    ENDIF



        SNWE = AMAX1(0.,(SNWEPR-                                      &
                    (SMELT+BETA*EPOT*RAS)*DELT                        &

                                         ) )

        soilt=soiltfrac


      ELSE
       if(snhei.ne.0.) then
               EPOT=-QKMS*(QVATM-QSG)
               SNWE = AMAX1(0.,(SNWEPR-                               &
                    BETA*EPOT*RAS*DELT))

       endif

      ENDIF




 220  continue

       if(smelt > 0..and.  rsm > 0.) then
        if(snwe.le.rsm) then
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'SEAICE SNWE<RSM snwe,rsm,smelt*delt,epot*ras*delt,beta', &
                              snwe,rsm,smelt*delt,epot*ras*delt,beta
    ENDIF
        else





         xsn=(rhosn*(snwe-rsm)+1.e3*rsm)/                            &
             snwe
         rhosn=MIN(MAX(58.8,XSN),500.)


        RHOCSN=2090.* RHOSN
        thdifsn = 0.265/RHOCSN
        endif
      endif

        snweprint=snwe




        snheiprint=snweprint*1.E3 / RHOSN

    IF ( wrf_at_debug_level(3000) ) THEN
print *, 'snweprint : ',snweprint
print *, 'D9SN,SOILT,TSOB : ', D9SN,SOILT,TSOB
    ENDIF
      IF(SNHEI.GT.0.) THEN
        if(ilnb.gt.1) then
          tsnav=0.5/snhei*((soilt+soilt1)*deltsn                     &
                    +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                       -273.15
        else
          tsnav=0.5*(soilt+tso(1)) - 273.15
        endif
      ENDIF

         DEW=0.
         PP=PATM*1.E3
         QSG= QSN(SOILT,TBQ)/PP
         EPOT = -FQ*(QVATM-QSG)
       IF(EPOT.LT.0.) THEN

          DEW=-EPOT
        ENDIF

        SNOM=SNOM+SMELT*DELT*1.e3



        T3      = STBOLT*TNold*TNold*TNold
        UPFLUX  = T3 *0.5*(SOILT+TNold)
        XINET   = EMISS*(GLW-UPFLUX)

        HFT=-TKMS*CP*RHO*(TABS-SOILT)
        HFX=-TKMS*CP*RHO*(TABS-SOILT)                        &
               *(P1000mb*0.00001/Patm)**ROVCP
        Q1 = - FQ*RAS* (QVATM - QSG)
        IF (Q1.LT.0.) THEN

      if(myj) then

          EETA=-QKMS*RAS*(QVATM/(1.+QVATM) - QSG/(1.+QSG))*1.E3
      else 

          DEW=QKMS*(QVATM-QSG)
          EETA= - RHO*DEW
      endif 
          QFX= XLVm*EETA
          EETA= - RHO*DEW
          sublim = EETA
        ELSE

      if(myj) then

          EETA=-QKMS*RAS*BETA*(QVATM/(1.+QVATM) - QVG/(1.+QVG))*1.E3
      else 


          EETA = Q1*BETA*1.E3
      endif 
          QFX= XLVm * EETA
          EETA = Q1*BETA*1.E3
          sublim = EETA
        ENDIF

        icemelt=0.
      IF(SNHEI.GE.SNTH)then
         S=thdifsn*RHOCSN*(soilt-TSOB)/SNPRIM
         SNFLX=S
       ELSEIF(SNHEI.lt.SNTH.and.SNHEI.GT.0.) then
         S=(fsn*thdifsn*rhocsn+fso*thdifice(1)*rhcs)*                &
              (soilt-TSOB)/snprim
         SNFLX=S
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SNOW is thin, snflx',i,j,snflx
    ENDIF
       ELSE 
         SNFLX=D9SN*(SOILT-TSOB)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SNOW is GONE, snflx',i,j,snflx
    ENDIF
       ENDIF

        SNHEI=SNWE *1.E3 / RHOSN

    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'SNHEI,SNOH',i,j,SNHEI,SNOH
    ENDIF

         X= (R21+D9SN*R22SN)*(soilt-TNOLD) +              &
            XLVM*R210*(QSG-QGOLD)
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'SNOWSEAICE storage ',i,j,x
     print *,'R21,D9sn,r22sn,soiltfrac,tnold,qsg,qgold,snprim', &
              R21,D9sn,r22sn,soiltfrac,tnold,qsg,qgold,snprim
    ENDIF
         X=X &
        -RHOnewCSN*NEWSNOW/DELT*(min(273.15,TABS)-SOILT)        &
        -RAINF*CVW*PRCPMS*(max(273.15,TABS)-SOILT)


        icemelt = RNET-HFT-XLVm*EETA-S-SNOH-X
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,'SNOWSEAICE icemelt=',icemelt
    ENDIF

        FLTOT=RNET-HFT-XLVm*EETA-S-SNOH-x-icemelt
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'i,j,snhei,qsg,soilt,soilt1,tso,TABS,QVATM', &
                i,j,snhei,qsg,soilt,soilt1,tso,tabs,qvatm
       print *,'SNOWSEAICE - FLTOT,RNET,HFT,QFX,S,SNOH,icemelt,snodif,X,SOILT=' &
                      ,FLTOT,RNET,HFT,XLVm*EETA,s,SNOH,icemelt,snodif,X,SOILT
    ENDIF

         IF(SNHEI.EQ.0.)  then
          tsnav=soilt-273.15
          emiss=0.98
          znt=0.011
          alb=0.55
         ENDIF



   END SUBROUTINE SNOWSEAICE



           SUBROUTINE SOILTEMP(                             &

           i,j,iland,isoil,                                 &
           delt,ktau,conflx,nzs,nddzs,nroot,                &
           PRCPMS,RAINF,PATM,TABS,QVATM,QCATM,              &
           EMISS,RNET,                                      &
           QKMS,TKMS,PC,RHO,VEGFRAC,lai,                    &
           THDIF,CAP,DRYCAN,WETCAN,                         &
           TRANSUM,DEW,MAVAIL,soilres,alfa,                 &

           DQM,QMIN,BCLH,                                   &
           ZSMAIN,ZSHALF,DTDZS,TBQ,                         &

           XLV,CP,G0_P,CVW,STBOLT,                          &

           TSO,SOILT,QVG,QSG,QCG,X)

















































        IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                         
   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS, RAINF
   REAL,     INTENT(INOUT)   ::  DRYCAN,WETCAN,TRANSUM

   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                          EMISS, &
                                                            RHO, &
                                                           RNET, &  
                                                             PC, &
                                                        VEGFRAC, &
                                                            LAI, &
                                                            DEW, & 
                                                           QKMS, &
                                                           TKMS


   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                           BCLH, &
                                                            DQM, &
                                                           QMIN
   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                   soilres,alfa


   REAL,     INTENT(IN   )   ::                              CP, &
                                                            CVW, &
                                                            XLV, &
                                                         STBOLT, &
                                                           TABS, &
                                                           G0_P


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                          THDIF, &
                                                            CAP

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:5001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION( 1:nzs )                                , &
             INTENT(INOUT)   ::                             TSO


   REAL                                                        , &
             INTENT(INOUT)   ::                                  &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                          SOILT




   REAL    ::  x,x1,x2,x4,dzstop,can,ft,sph                    , &
               tn,trans,umveg,denom,fex

   REAL    ::  FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11     , &
               PI,H,FKQ,R210,AA,BB,PP,Q1,QS1,TS1,TQ2,TX2       , &
               TDENOM

   REAL    ::  C,CC,AA1,RHCS,H1, QGOLD

   REAL,     DIMENSION(1:NZS)  ::                   cotso,rhtso

   INTEGER ::  nzs1,nzs2,k,k1,kn,kk, iter




        iter=0

          NZS1=NZS-1
          NZS2=NZS-2
        dzstop=1./(ZSMAIN(2)-ZSMAIN(1))

        qgold=qvg

        do k=1,nzs
           cotso(k)=0.
           rhtso(k)=0.
        enddo








        cotso(1)=0.
        rhtso(1)=TSO(NZS)
        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIF(KN-1)
          X2=DTDZS(K1+1)*THDIF(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                             &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE




        RHCS=CAP(1)

        H=MAVAIL

        TRANS=TRANSUM*DRYCAN/ZSHALF(NROOT+1)
        CAN=WETCAN+TRANS
        UMVEG=(1.-VEGFRAC) * soilres
 2111   continue
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIF(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIF(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6
        TDENOM=D9*(1.-D1+R22)+D10+R21+R7                              &
              +RAINF*CVW*PRCPMS
        FKQ=QKMS*RHO
        R210=R211*RHO
        C=VEGFRAC*FKQ*CAN
        CC=C*XLV/TDENOM
        AA=XLV*(FKQ*UMVEG+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLV*(QVATM*                               &
        (FKQ*UMVEG+C)                                                 & 
        +R210*QVG)+D11+D9*(D2+R22*TN)                                 &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                            &
         )/TDENOM
        AA1=AA+CC

        PP=PATM*1.E3
        AA1=AA1/PP
        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        TQ2=QVATM
        TX2=TQ2*(1.-H)
        Q1=TX2+H*QS1

    IF ( wrf_at_debug_level(3000) ) THEN

        print *,'VILKA1 - TS1,QS1,TQ2,H,TX2,Q1',TS1,QS1,TQ2,H,TX2,Q1
    ENDIF

        IF(Q1.LT.QS1) GOTO 100


   90   QVG=QS1
        QSG=QS1
        TSO(1)=TS1
        QCG=max(0.,Q1-QS1)
    IF ( wrf_at_debug_level(3000) ) THEN

        print *,'90 QVG,QSG,QCG,TSO(1)',QVG,QSG,QCG,TSO(1)
    ENDIF
        GOTO 200
  100   BB=BB-AA*TX2
        AA=(AA*H+CC)/PP
        CALL VILKA(TN,AA,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        Q1=TX2+H*QS1
    IF ( wrf_at_debug_level(3000) ) THEN


        print *,'VILKA2 - TS1,QS1,TQ2,H,TX2,Q1',TS1,QS1,TQ2,H,TX2,Q1
    ENDIF
        IF(Q1.GE.QS1) GOTO 90

        QSG=QS1
        QVG=Q1





        TSO(1)=TS1
        QCG=0.
    IF ( wrf_at_debug_level(3000) ) THEN

       print *,'q1,qsg,qvg,qvatm,alfa,h',q1,qsg,qvg,qvatm,alfa,h
    endif
  200   CONTINUE
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,'200 QVG,QSG,QCG,TSO(1)',QVG,QSG,QCG,TSO(1)
    ENDIF


          SOILT=TS1


          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=rhtso(KK)+cotso(KK)*TSO(K-1)
          END DO

         X= (cp*rho*r211+rhcs*zsmain(2)*0.5/delt)*(SOILT-TN) + &
            XLV*rho*r211*(QVG-QGOLD) 

    IF ( wrf_at_debug_level(3000) ) THEN
        print*,'SOILTEMP storage, i,j,x,soilt,tn,qvg,qvgold', &
                                  i,j,x,soilt,tn,qvg,qgold
        print *,'TEMP term (cp*rho*r211+rhcs*zsmain(2)*0.5/delt)*(SOILT-TN)',&
                 (cp*rho*r211+rhcs*zsmain(2)*0.5/delt)*(SOILT-TN)
        print *,'QV term XLV*rho*r211*(QVG-QGOLD)',XLV*rho*r211*(QVG-QGOLD)
    ENDIF
         X=X &

        -RAINF*CVW*PRCPMS*(max(273.15,TABS)-SOILT)

    IF ( wrf_at_debug_level(3000) ) THEN
        print *,'x=',x
    ENDIF


   END SUBROUTINE SOILTEMP



           SUBROUTINE SNOWTEMP(                                    & 

           i,j,iland,isoil,                                        &
           delt,ktau,conflx,nzs,nddzs,nroot,                       &
           snwe,snwepr,snhei,newsnow,snowfrac,                     &
           beta,deltsn,snth,rhosn,rhonewsn,meltfactor,             &  
           PRCPMS,RAINF,                                           &
           PATM,TABS,QVATM,QCATM,                                  &
           GLW,GSW,EMISS,RNET,                                     &
           QKMS,TKMS,PC,RHO,VEGFRAC,                               &
           THDIF,CAP,DRYCAN,WETCAN,CST,                            &
           TRANF,TRANSUM,DEW,MAVAIL,                               &

           DQM,QMIN,PSIS,BCLH,                                     &
           ZSMAIN,ZSHALF,DTDZS,TBQ,                                &

           XLVM,CP,rovcp,G0_P,CVW,STBOLT,                          &

           SNWEPRINT,SNHEIPRINT,RSM,                               &
           TSO,SOILT,SOILT1,TSNAV,QVG,QSG,QCG,                     &
           SMELT,SNOH,SNFLX,S,ILNB,X)

















































        IMPLICIT NONE



   INTEGER,  INTENT(IN   )   ::  nroot,ktau,nzs                , &
                                 nddzs                             

   INTEGER,  INTENT(IN   )   ::  i,j,iland,isoil
   REAL,     INTENT(IN   )   ::  DELT,CONFLX,PRCPMS            , &
                                 RAINF,NEWSNOW,DELTSN,SNTH     , &
                                 TABS,TRANSUM,SNWEPR           , &
                                 rhonewsn,meltfactor
   real                      ::  rhonewcsn


   REAL,                                                         &
            INTENT(IN   )    ::                            PATM, &
                                                          QVATM, &
                                                          QCATM

   REAL                                                        , &
            INTENT(IN   )    ::                             GLW, &
                                                            GSW, &
                                                            RHO, &
                                                             PC, &
                                                        VEGFRAC, &
                                                           QKMS, &
                                                           TKMS


   REAL                                                        , &
            INTENT(IN   )    ::                                  &
                                                           BCLH, &
                                                            DQM, &
                                                           PSIS, &
                                                           QMIN

   REAL,     INTENT(IN   )   ::                              CP, &
                                                          ROVCP, &
                                                            CVW, &
                                                         STBOLT, &
                                                           XLVM, &
                                                            G0_P


   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::            ZSMAIN, &
                                                         ZSHALF, &
                                                          THDIF, &
                                                            CAP, &
                                                          TRANF 

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     DIMENSION(1:5001), INTENT(IN)  ::              TBQ




   REAL,     DIMENSION(  1:nzs )                               , &
             INTENT(INOUT)   ::                             TSO



   REAL                                                        , &
             INTENT(INOUT)   ::                             DEW, &
                                                            CST, &
                                                          RHOSN, &
                                                          EMISS, &
                                                         MAVAIL, &
                                                            QVG, &
                                                            QSG, &
                                                            QCG, &
                                                           SNWE, &
                                                          SNHEI, &
                                                       SNOWFRAC, &
                                                          SMELT, &
                                                           SNOH, &
                                                          SNFLX, &
                                                              S, &
                                                          SOILT, &
                                                         SOILT1, &
                                                          TSNAV

   REAL,     INTENT(INOUT)                  ::   DRYCAN, WETCAN           

   REAL,     INTENT(OUT)                    ::              RSM, &
                                                      SNWEPRINT, &
                                                     SNHEIPRINT
   INTEGER,  INTENT(OUT)                    ::             ilnb



   INTEGER ::  nzs1,nzs2,k,k1,kn,kk

   REAL    ::  x,x1,x2,x4,dzstop,can,ft,sph,                     &
               tn,trans,umveg,denom

   REAL    ::  cotsn,rhtsn,xsn1,ddzsn1,x1sn1,ftsnow,denomsn

   REAL    ::  t3,upflux,xinet,ras,                              &
               xlmelt,rhocsn,thdifsn,                            &
               beta,epot,xsn,ddzsn,x1sn,d1sn,d2sn,d9sn,r22sn

   REAL    ::  fso,fsn,                                          &
               FKT,D1,D2,D9,D10,DID,R211,R21,R22,R6,R7,D11,      &
               PI,H,FKQ,R210,AA,BB,PP,Q1,QS1,TS1,TQ2,TX2,        &
               TDENOM,C,CC,AA1,RHCS,H1,                          &
               tsob, snprim, sh1, sh2,                           &
               smeltg,snohg,snodif,soh,                          &
               CMC2MS,TNOLD,QGOLD,SNOHGNEW                            

   REAL,     DIMENSION(1:NZS)  ::  transp,cotso,rhtso
   REAL                        ::                         edir1, &
                                                            ec1, &
                                                           ett1, &
                                                           eeta, &
                                                            qfx, &
                                                            hfx

   REAL                        :: RNET,rsmfrac,soiltfrac,hsn,rr
   integer                     ::      nmelt, iter



       iter = 0

       do k=1,nzs
          transp   (k)=0.
          cotso    (k)=0.
          rhtso    (k)=0.
       enddo

    IF ( wrf_at_debug_level(3000) ) THEN
print *, 'SNOWTEMP: SNHEI,SNTH,SOILT1: ',SNHEI,SNTH,SOILT1,soilt 
    ENDIF
        XLMELT=3.35E+5
        RHOCSN=2090.* RHOSN

        RHOnewCSN=2090.* RHOnewSN
        THDIFSN = 0.265/RHOCSN
        RAS=RHO*1.E-3

        SOILTFRAC=SOILT

        SMELT=0.
        SOH=0.
        SMELTG=0.
        SNOHG=0.
        SNODIF=0.
        RSM = 0.
        RSMFRAC = 0.
        fsn=1.
        fso=0.


          NZS1=NZS-1
          NZS2=NZS-2

        QGOLD=QVG
        DZSTOP=1./(ZSMAIN(2)-ZSMAIN(1))










        cotso(1)=0.
        rhtso(1)=TSO(NZS)
        DO 33 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X1=DTDZS(K1)*THDIF(KN-1)
          X2=DTDZS(K1+1)*THDIF(KN)
          FT=TSO(KN)+X1*(TSO(KN-1)-TSO(KN))                           &
             -X2*(TSO(KN)-TSO(KN+1))
          DENOM=1.+X1+X2-X2*cotso(K)
          cotso(K+1)=X1/DENOM
          rhtso(K+1)=(FT+X2*rhtso(K))/DENOM
   33  CONTINUE


       IF(SNHEI.GE.SNTH) then
        if(snhei.le.DELTSN+SNTH) then

    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'1-layer - snth,snhei,deltsn',snth,snhei,deltsn
    ENDIF
         ilnb=1
         snprim=max(snth,snhei)
         tsob=tso(1)
         soilt1=tso(1)
         XSN = DELT/2./(zshalf(2)+0.5*SNPRIM)
         DDZSN = XSN / SNPRIM
         X1SN = DDZSN * thdifsn
         X2 = DTDZS(1)*THDIF(1)
         FT = TSO(1)+X1SN*(SOILT-TSO(1))                              &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN + X2 -X2*cotso(NZS1)
         cotso(NZS)=X1SN/DENOM
         rhtso(NZS)=(FT+X2*rhtso(NZS1))/DENOM
         cotsn=cotso(NZS)
         rhtsn=rhtso(NZS)

         tsnav=0.5*(soilt+tso(1))                                     &
                     -273.15

        else

    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'2-layer - snth,snhei,deltsn',snth,snhei,deltsn
    ENDIF
         ilnb=2
         snprim=deltsn
         tsob=soilt1
         XSN = DELT/2./(0.5*deltsn)
         XSN1= DELT/2./(zshalf(2)+0.5*(SNHEI-DELTSN))
         DDZSN = XSN / DELTSN
         DDZSN1 = XSN1 / (SNHEI-DELTSN)
         X1SN = DDZSN * thdifsn
         X1SN1 = DDZSN1 * thdifsn
         X2 = DTDZS(1)*THDIF(1)
         FT = TSO(1)+X1SN1*(SOILT1-TSO(1))                            &
              -X2*(TSO(1)-TSO(2))
         DENOM = 1. + X1SN1 + X2 - X2*cotso(NZS1)
         cotso(nzs)=x1sn1/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         ftsnow = soilt1+x1sn*(soilt-soilt1)                          &
               -x1sn1*(soilt1-tso(1))
         denomsn = 1. + X1SN + X1SN1 - X1SN1*cotso(NZS)
         cotsn=x1sn/denomsn
         rhtsn=(ftsnow+X1SN1*rhtso(NZS))/denomsn

         tsnav=0.5/snhei*((soilt+soilt1)*deltsn                       &
                     +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                     -273.15
        endif
       ENDIF
       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then



         snprim=SNHEI+zsmain(2)
         fsn=SNHEI/snprim
         fso=1.-fsn
         soilt1=tso(1)
         tsob=tso(2)
         XSN = DELT/2./((zshalf(3)-zsmain(2))+0.5*snprim)
         DDZSN = XSN /snprim
         X1SN = DDZSN * (fsn*thdifsn+fso*thdif(1))
         X2=DTDZS(2)*THDIF(2)
         FT=TSO(2)+X1SN*(SOILT-TSO(2))-                              &
                       X2*(TSO(2)-TSO(3))
         denom = 1. + x1sn + x2 - x2*cotso(nzs-2)
         cotso(nzs1) = x1sn/denom
         rhtso(nzs1)=(FT+X2*rhtso(NZS-2))/denom
         tsnav=0.5*(soilt+tso(1))                                    &
                     -273.15
         cotso(NZS)=cotso(nzs1)
         rhtso(NZS)=rhtso(nzs1)
         cotsn=cotso(NZS)
         rhtsn=rhtso(NZS)

       ENDIF




       nmelt=0
       SNOH=0.

        ETT1=0.
        EPOT=-QKMS*(QVATM-QGOLD)
        RHCS=CAP(1)
        H=1.
        TRANS=TRANSUM*DRYCAN/ZSHALF(NROOT+1)
        CAN=WETCAN+TRANS
        UMVEG=1.-VEGFRAC
        FKT=TKMS
        D1=cotso(NZS1)
        D2=rhtso(NZS1)
        TN=SOILT
        D9=THDIF(1)*RHCS*dzstop
        D10=TKMS*CP*RHO
        R211=.5*CONFLX/DELT
        R21=R211*CP*RHO
        R22=.5/(THDIF(1)*DELT*dzstop**2)
        R6=EMISS *STBOLT*.5*TN**4
        R7=R6/TN
        D11=RNET+R6

      IF(SNHEI.GE.SNTH) THEN
        if(snhei.le.DELTSN+SNTH) then

          D1SN = cotso(NZS)
          D2SN = rhtso(NZS)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'1 layer d1sn,d2sn',i,j,d1sn,d2sn
    ENDIF
        else

          D1SN = cotsn
          D2SN = rhtsn
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'2 layers d1sn,d2sn',i,j,d1sn,d2sn
    ENDIF
        endif
        D9SN= THDIFSN*RHOCSN / SNPRIM
        R22SN = SNPRIM*SNPRIM*0.5/(THDIFSN*DELT)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'1 or 2 layers D9sn,R22sn',d9sn,r22sn
    ENDIF
      ENDIF

       IF(SNHEI.LT.SNTH.AND.SNHEI.GT.0.) then

         D1SN = D1
         D2SN = D2
         D9SN = (fsn*THDIFSN*RHOCSN+fso*THDIF(1)*RHCS)/              &
                 snprim
         R22SN = snprim*snprim*0.5                                   &
                 /((fsn*THDIFSN+fso*THDIF(1))*delt)
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,' Combined  D9SN,R22SN,D1SN,D2SN: ',D9SN,R22SN,D1SN,D2SN
    ENDIF
      ENDIF
      IF(SNHEI.eq.0.)then

        D9SN = D9
        R22SN = R22
        D1SN = D1
        D2SN = D2
    IF ( wrf_at_debug_level(3000) ) THEN
        print *,' SNHEI = 0, D9SN,R22SN,D1SN,D2SN: ',D9SN,R22SN,D1SN,D2SN
    ENDIF
      ENDIF

 2211   continue


 212    continue


        TDENOM = D9SN*(1.-D1SN +R22SN)+D10+R21+R7                    &
              +RAINF*CVW*PRCPMS                                      &
              +RHOnewCSN*NEWSNOW/DELT

        FKQ=QKMS*RHO
        R210=R211*RHO
        C=VEGFRAC*FKQ*CAN
        CC=C*XLVM/TDENOM
        AA=XLVM*(BETA*FKQ*UMVEG+R210)/TDENOM
        BB=(D10*TABS+R21*TN+XLVM*(QVATM*                             &
        (BETA*FKQ*UMVEG+C)                                           &
        +R210*QGOLD)+D11+D9SN*(D2SN+R22SN*TN)                        &
        +RAINF*CVW*PRCPMS*max(273.15,TABS)                           &
        + RHOnewCSN*NEWSNOW/DELT*min(273.15,TABS)                    &
         )/TDENOM
        AA1=AA+CC
        PP=PATM*1.E3
        AA1=AA1/PP
        BB=BB-SNOH/TDENOM

        CALL VILKA(TN,AA1,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        TQ2=QVATM
        TX2=TQ2*(1.-H)
        Q1=TX2+H*QS1
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'VILKA1 - TS1,QS1,TQ2,H,TX2,Q1',TS1,QS1,TQ2,H,TX2,Q1
    ENDIF
        IF(Q1.LT.QS1) GOTO 100


   90   QVG=QS1
        QSG=QS1
        QCG=max(0.,Q1-QS1)
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'90 QVG,QSG,QCG,TSO(1)',QVG,QSG,QCG,TSO(1)
    ENDIF
        GOTO 200
  100   BB=BB-AA*TX2
        AA=(AA*H+CC)/PP
        CALL VILKA(TN,AA,BB,PP,QS1,TS1,TBQ,KTAU,i,j,iland,isoil)
        Q1=TX2+H*QS1
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'VILKA2 - TS1,QS1,H,TX2,Q1',TS1,QS1,TQ2,H,TX2,Q1
    ENDIF
        IF(Q1.GT.QS1) GOTO 90
        QSG=QS1
        QVG=Q1
        QCG=0.
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'No Saturation QVG,QSG,QCG,TSO(1)',QVG,QSG,QCG,TSO(1)
    ENDIF
  200   CONTINUE


        SOILT=TS1
    IF ( wrf_at_debug_level(3000) ) THEN
     IF(i.eq.266.and.j.eq.447) then
            print *,'snwe,snhei,soilt,soilt1,tso',i,j,snwe,snhei,soilt,soilt1,tso
     endif
    ENDIF

       IF(SNHEI.GE.SNTH) THEN
        if(snhei.gt.DELTSN+SNTH) then

          SOILT1=min(273.15,rhtsn+cotsn*SOILT)
          TSO(1)=rhtso(NZS)+cotso(NZS)*SOILT1
          tsob=soilt1
        else

          TSO(1)=rhtso(NZS)+cotso(NZS)*SOILT
          SOILT1=TSO(1)
          tsob=tso(1)
        endif
       ELSEIF (SNHEI > 0. .and. SNHEI < SNTH) THEN

         TSO(2)=rhtso(NZS1)+cotso(NZS1)*SOILT
         tso(1)=(tso(2)+(soilt-tso(2))*fso)
         SOILT1=TSO(1)
         tsob=TSO(2)
       ELSE


         TSO(1)=SOILT
         SOILT1=SOILT
         tsob=TSO(1)

       ENDIF


       IF (SNHEI > 0. .and. SNHEI < SNTH) THEN

          DO K=3,NZS
            KK=NZS-K+1
            TSO(K)=rhtso(KK)+cotso(KK)*TSO(K-1)
          END DO

       ELSE
          DO K=2,NZS
            KK=NZS-K+1
            TSO(K)=rhtso(KK)+cotso(KK)*TSO(K-1)
          END DO
       ENDIF










    IF ( wrf_at_debug_level(3000) ) THEN

   print *,'SOILT,SOILT1,tso,TSOB,QSG',i,j,SOILT,SOILT1,tso,TSOB,QSG,'nmelt=',nmelt
    ENDIF

     if(nmelt.eq.1) go to 220




   IF(SOILT.GT.273.15.AND.SNWEPR-BETA*EPOT*RAS*DELT.GT.0.AND.SNHEI.GT.0.) THEN
        nmelt = 1
        soiltfrac=snowfrac*273.15+(1.-snowfrac)*SOILT
        QSG=min(QSG, QSN(soiltfrac,TBQ)/PP)
        qvg=qsg
        T3      = STBOLT*TN*TN*TN
        UPFLUX  = T3 * 0.5*(TN + SOILTfrac)
        XINET   = EMISS*(GLW-UPFLUX)

         EPOT = -QKMS*(QVATM-QSG)
         Q1=EPOT*RAS

        IF (Q1.LE.0..or.iter==1) THEN

          DEW=-EPOT
          DO K=1,NZS
            TRANSP(K)=0.
          ENDDO

        QFX = -XLVM*RHO*DEW
        EETA = QFX/XLVM
       ELSE

          DO K=1,NROOT
            TRANSP(K)=-VEGFRAC*q1                                     &
                      *TRANF(K)*DRYCAN/zshalf(NROOT+1)

            ETT1=ETT1-TRANSP(K)
          ENDDO
          DO k=nroot+1,nzs
            transp(k)=0.
          enddo

        EDIR1 = Q1*UMVEG * BETA
        EC1 = Q1 * WETCAN * vegfrac
        CMC2MS=CST/DELT*RAS

        EETA = (EDIR1 + EC1 + ETT1)*1.E3

        QFX=  XLVM * EETA
       ENDIF

         HFX=-D10*(TABS-soiltfrac)

       IF(SNHEI.GE.SNTH)then
         SOH=thdifsn*RHOCSN*(soiltfrac-TSOB)/SNPRIM
         SNFLX=SOH
       ELSE
         SOH=(fsn*thdifsn*rhocsn+fso*thdif(1)*rhcs)*                   &
              (soiltfrac-TSOB)/snprim
         SNFLX=SOH
       ENDIF


         X= (R21+D9SN*R22SN)*(soiltfrac-TN) +                        &
            XLVM*R210*(QVG-QGOLD)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SNOWTEMP storage ',i,j,x
      print *,'R21,D9sn,r22sn,soiltfrac,tn,qsg,qvg,qgold,snprim', &
              R21,D9sn,r22sn,soiltfrac,tn,qsg,qvg,qgold,snprim
    ENDIF


        SNOH=RNET-QFX -HFX - SOH - X                                    & 
                  +RHOnewCSN*NEWSNOW/DELT*(min(273.15,TABS)-soiltfrac)  &
                  +RAINF*CVW*PRCPMS*(max(273.15,TABS)-soiltfrac) 
        SNOH=AMAX1(0.,SNOH)

        SMELT= SNOH /XLMELT*1.E-3
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'1- SMELT',i,j,smelt
    ENDIF
        SMELT=AMIN1(SMELT,SNWEPR/DELT-BETA*EPOT*RAS)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'2- SMELT',i,j,smelt
    ENDIF
        SMELT=AMAX1(0.,SMELT)



        SMELT= amin1 (smelt, 5.6E-8*meltfactor*max(1.,(soilt-273.15)))
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'3- SMELT',i,j,smelt
    ENDIF


        rr=max(0.,SNWEPR/delt-BETA*EPOT*RAS)
        SMELT=min(SMELT,rr)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'4- SMELT i,j,smelt,rr',i,j,smelt,rr
    ENDIF
        SNOHGNEW=SMELT*XLMELT*1.E3
        SNODIF=AMAX1(0.,(SNOH-SNOHGNEW))

        SNOH=SNOHGNEW
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SNOH,SNODIF',SNOH,SNODIF
    ENDIF


        rsmfrac=min(0.18,(max(0.08,snwepr/0.10*0.13)))
       if(snhei > 0.01) then
        rsm=rsmfrac*smelt*delt
       else

        rsm=0.
       endif

        SMELT=max(0.,SMELT-rsm/delt)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'5- SMELT i,j,smelt,rsm,snwepr,rsmfrac', &
                        i,j,smelt,rsm,snwepr,rsmfrac
    ENDIF



        SNWE = AMAX1(0.,(SNWEPR-                                      &
                    (SMELT+BETA*EPOT*RAS)*DELT                        &


                                         ) )


      ELSE
       if(snhei.ne.0.) then
               EPOT=-QKMS*(QVATM-QSG)
               SNWE = AMAX1(0.,(SNWEPR-                               &
                    BETA*EPOT*RAS*DELT))

       endif

      ENDIF


     if(nmelt.eq.1) goto 212  
 220  continue

      if(smelt.gt.0..and.rsm.gt.0.) then
       if(snwe.le.rsm) then
    IF ( 1==1 ) THEN
     print *,'SNWE<RSM snwe,rsm,smelt*delt,epot*ras*delt,beta', &
                     snwe,rsm,smelt*delt,epot*ras*delt,beta
    ENDIF
       else




          xsn=(rhosn*(snwe-rsm)+1.e3*rsm)/                            &
              snwe
          rhosn=MIN(MAX(58.8,XSN),500.)


          RHOCSN=2090.* RHOSN
          thdifsn = 0.265/RHOCSN
        endif  
       endif


       IF(SNHEI.GE.SNTH)then
         S=thdifsn*RHOCSN*(soilt-TSOB)/SNPRIM
         SNFLX=S
         S=D9*(tso(1)-tso(2))
       ELSEIF(SNHEI.lt.SNTH.and.SNHEI.GT.0.) then
         S=(fsn*thdifsn*rhocsn+fso*thdif(1)*rhcs)*                   &
              (soilt-TSOB)/snprim
         SNFLX=S
         S=D9*(tso(1)-tso(2))
       ELSE
         S=D9SN*(SOILT-TSOB)
         SNFLX=S
         S=D9*(tso(1)-tso(2))
       ENDIF

        SNHEI=SNWE *1.E3 / RHOSN




        IF(TSO(1).GT.273.15 .and. snhei > 0.) THEN
          if (snhei.GT.deltsn+snth) then
              hsn = snhei - deltsn
    IF ( wrf_at_debug_level(3000) ) THEN
       print*,'2 layer snow - snhei,hsn',snhei,hsn
    ENDIF
          else
    IF ( wrf_at_debug_level(3000) ) THEN
       print*,'1 layer snow or blended - snhei',snhei
    ENDIF
              hsn = snhei
          endif

         soiltfrac=snowfrac*273.15+(1.-snowfrac)*TSO(1)

        SNOHG=(TSO(1)-soiltfrac)*(cap(1)*zshalf(2)+                       &
               RHOCSN*0.5*hsn) / DELT
        SNOHG=AMAX1(0.,SNOHG)
        SNODIF=0.
        SMELTG=SNOHG/XLMELT*1.E-3

        SMELTG=AMIN1(SMELTG, 5.8e-9)


        rr=SNWE/delt
        SMELTG=AMIN1(SMELTG, rr)

        SNOHGNEW=SMELTG*XLMELT*1.e3
        SNODIF=AMAX1(0.,(SNOHG-SNOHGNEW))
    IF ( wrf_at_debug_level(3000) ) THEN

       print *,'TSO(1),soiltfrac,smeltg,SNODIF',TSO(1),soiltfrac,smeltg,SNODIF
    ENDIF


        snwe=max(0.,snwe-smeltg*delt)
        SNHEI=SNWE *1.E3 / RHOSN
      
        if(snhei > 0.) TSO(1) = soiltfrac
    IF ( wrf_at_debug_level(3000) ) THEN

       print *,'Melt from the bottom snwe,snhei',snwe,snhei
       if (snhei==0.) &
       print *,'Snow is all melted on the warm ground'
    ENDIF

       ENDIF
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SNHEI,SNOH',i,j,SNHEI,SNOH
    ENDIF

        snweprint=snwe
        snheiprint=snweprint*1.E3 / RHOSN

    IF ( wrf_at_debug_level(3000) ) THEN
print *, 'snweprint : ',snweprint
print *, 'D9SN,SOILT,TSOB : ', D9SN,SOILT,TSOB
    ENDIF

         X= (R21+D9SN*R22SN)*(soilt-TN) +                     &
            XLVM*R210*(QSG-QGOLD)
    IF ( wrf_at_debug_level(3000) ) THEN
      print *,'SNOWTEMP storage ',i,j,x
      print *,'R21,D9sn,r22sn,soiltfrac,soilt,tn,qsg,qgold,snprim', &
              R21,D9sn,r22sn,soiltfrac,soilt,tn,qsg,qgold,snprim
    ENDIF

         X=X &

        -RHOnewCSN*NEWSNOW/DELT*(min(273.15,TABS)-SOILT)         &
        -RAINF*CVW*PRCPMS*(max(273.15,TABS)-SOILT)
    IF ( wrf_at_debug_level(3000) ) THEN
     print *,'x=',x
     print *,'SNHEI=',snhei
     print *,'SNFLX=',snflx
    ENDIF

      IF(SNHEI.GT.0.) THEN
        if(ilnb.gt.1) then
          tsnav=0.5/snhei*((soilt+soilt1)*deltsn                     &
                    +(soilt1+tso(1))*(SNHEI-DELTSN))                 &
                       -273.15
        else
          tsnav=0.5*(soilt+tso(1)) - 273.15
        endif
      ELSE
          tsnav= soilt - 273.15
      ENDIF


   END SUBROUTINE SNOWTEMP



        SUBROUTINE SOILMOIST (                                  &

              DELT,NZS,NDDZS,DTDZS,DTDZS2,RIW,                  &
              ZSMAIN,ZSHALF,DIFFU,HYDRO,                        &
              QSG,QVG,QCG,QCATM,QVATM,PRCP,                     &
              QKMS,TRANSP,DRIP,                                 &
              DEW,SMELT,SOILICE,VEGFRAC,SNOWFRAC,soilres,       &

              DQM,QMIN,REF,KSAT,RAS,INFMAX,                     &

              SOILMOIS,SOILIQW,MAVAIL,RUNOFF,RUNOFF2,INFILTRP)









































        IMPLICIT NONE


   REAL,     INTENT(IN   )   ::  DELT
   INTEGER,  INTENT(IN   )   ::  NZS,NDDZS



   REAL,     DIMENSION(1:NZS), INTENT(IN   )  ::         ZSMAIN, &
                                                         ZSHALF, &
                                                          DIFFU, &
                                                          HYDRO, &
                                                         TRANSP, &
                                                        SOILICE, &
                                                         DTDZS2

   REAL,     DIMENSION(1:NDDZS), INTENT(IN)  ::           DTDZS

   REAL,     INTENT(IN   )   ::    QSG,QVG,QCG,QCATM,QVATM     , &
                                   QKMS,VEGFRAC,DRIP,PRCP      , &
                                   DEW,SMELT,SNOWFRAC          , &
                                   DQM,QMIN,REF,KSAT,RAS,RIW,SOILRES
                         


   REAL,     DIMENSION(  1:nzs )                               , &

             INTENT(INOUT)   ::                SOILMOIS,SOILIQW
                                                  
   REAL,     INTENT(INOUT)   ::  MAVAIL,RUNOFF,RUNOFF2,INFILTRP, &
                                                        INFMAX



   REAL,     DIMENSION( 1:nzs )  ::  COSMC,RHSMC

   REAL    ::  DZS,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10
   REAL    ::  REFKDT,REFDK,DELT1,F1MAX,F2MAX
   REAL    ::  F1,F2,FD,KDT,VAL,DDT,PX,FK,FKMAX
   REAL    ::  QQ,UMVEG,INFMAX1,TRANS
   REAL    ::  TOTLIQ,FLX,FLXSAT,QTOT
   REAL    ::  DID,X1,X2,X4,DENOM,Q2,Q4
   REAL    ::  dice,fcr,acrt,frzx,sum,cvfrz

   INTEGER ::  NZS1,NZS2,K,KK,K1,KN,ialp1,jj,jk




          NZS1=NZS-1                                                            
          NZS2=NZS-2

 118      format(6(10Pf23.19))

           do k=1,nzs
            cosmc(k)=0.
            rhsmc(k)=0.
           enddo
 
        DID=(ZSMAIN(NZS)-ZSHALF(NZS))
        X1=ZSMAIN(NZS)-ZSMAIN(NZS1)









        DENOM=(1.+DIFFU(nzs1)/X1/DID*DELT+HYDRO(NZS)/(2.*DID)*DELT)
        COSMC(1)=DELT*(DIFFU(nzs1)/DID/X1                                &
                    +HYDRO(NZS1)/2./DID)/DENOM
        RHSMC(1)=(SOILMOIS(NZS)+TRANSP(NZS)*DELT/                         &
               DID)/DENOM










        DENOM=1.+DIFFU(nzs1)/X1/DID*DELT



        COSMC(1)=DELT*(DIFFU(nzs1)/DID/X1                                &  
                    +HYDRO(NZS1)/DID)/DENOM




        RHSMC(1)=(SOILMOIS(NZS)-HYDRO(NZS)*DELT/DID*soilmois(nzs) & 
                 +TRANSP(NZS)*DELT/DID)/DENOM




        COSMC(1)=0.
        RHSMC(1)=SOILMOIS(NZS)

        DO 330 K=1,NZS2
          KN=NZS-K
          K1=2*KN-3
          X4=2.*DTDZS(K1)*DIFFU(KN-1)
          X2=2.*DTDZS(K1+1)*DIFFU(KN)
          Q4=X4+HYDRO(KN-1)*DTDZS2(KN-1)
          Q2=X2-HYDRO(KN+1)*DTDZS2(KN-1)
          DENOM=1.+X2+X4-Q2*COSMC(K)
          COSMC(K+1)=Q4/DENOM
    IF ( wrf_at_debug_level(3000) ) THEN
          print *,'q2,soilmois(kn),DIFFU(KN),x2,HYDRO(KN+1),DTDZS2(KN-1),kn,k' &
                  ,q2,soilmois(kn),DIFFU(KN),x2,HYDRO(KN+1),DTDZS2(KN-1),kn,k
    ENDIF
 330      RHSMC(K+1)=(SOILMOIS(KN)+Q2*RHSMC(K)                            &
                   +TRANSP(KN)                                            &
                   /(ZSHALF(KN+1)-ZSHALF(KN))                             &
                   *DELT)/DENOM



          TRANS=TRANSP(1)
          UMVEG=(1.-VEGFRAC)*soilres

          RUNOFF=0.
          RUNOFF2=0.
          DZS=ZSMAIN(2)
          R1=COSMC(NZS1)
          R2= RHSMC(NZS1)
          R3=DIFFU(1)/DZS
          R4=R3+HYDRO(1)*.5          
          R5=R3-HYDRO(2)*.5
          R6=QKMS*RAS





  191   format (f23.19)



        TOTLIQ=PRCP-DRIP/DELT-UMVEG*DEW*RAS-SMELT
    IF ( wrf_at_debug_level(3000) ) THEN
print *,'UMVEG*PRCP,DRIP/DELT,UMVEG*DEW*RAS,SMELT', &
         UMVEG*PRCP,DRIP/DELT,UMVEG*DEW*RAS,SMELT
    ENDIF




        FLX=TOTLIQ
        INFILTRP=TOTLIQ










         CVFRZ = 3.


         REFKDT=3.
         REFDK=3.4341E-6
         DELT1=DELT/86400.
         F1MAX=DQM*ZSHALF(2)
         F2MAX=DQM*(ZSHALF(3)-ZSHALF(2))
         F1=F1MAX*(1.-SOILMOIS(1)/DQM)
         DICE=SOILICE(1)*ZSHALF(2)
         FD=F1
        do k=2,nzs1
         DICE=DICE+(ZSHALF(k+1)-ZSHALF(k))*SOILICE(K)
         FKMAX=DQM*(ZSHALF(k+1)-ZSHALF(k))
         FK=FKMAX*(1.-SOILMOIS(k)/DQM)
         FD=FD+FK
        enddo
         KDT=REFKDT*KSAT/REFDK
         VAL=(1.-EXP(-KDT*DELT1))
         DDT = FD*VAL
         PX= - TOTLIQ * DELT
         IF(PX.LT.0.0) PX = 0.0
         IF(PX.gt.0.0) THEN
           INFMAX1 = (PX*(DDT/(PX+DDT)))/DELT
         ELSE
           INFMAX1 = 0.
         ENDIF
    IF ( wrf_at_debug_level(3000) ) THEN
  print *,'INFMAX1 before frozen part',INFMAX1
    ENDIF






         FRZX= 0.15*((dqm+qmin)/ref) * (0.412 / 0.468)
         FCR = 1.
         IF ( DICE .GT. 1.E-2) THEN
           ACRT = CVFRZ * FRZX / DICE
           SUM = 1.
           IALP1 = CVFRZ - 1
           DO JK = 1,IALP1
              K = 1
              DO JJ = JK+1, IALP1
                K = K * JJ
              END DO
              SUM = SUM + (ACRT ** ( CVFRZ-JK)) / FLOAT (K)
           END DO
           FCR = 1. - EXP(-ACRT) * SUM
         END IF
    IF ( wrf_at_debug_level(3000) ) THEN
          print *,'FCR--------',fcr
          print *,'DICE=',dice
    ENDIF
         INFMAX1 = INFMAX1* FCR


         INFMAX = MAX(INFMAX1,HYDRO(1)*SOILMOIS(1))
         INFMAX = MIN(INFMAX, -TOTLIQ)
    IF ( wrf_at_debug_level(3000) ) THEN
print *,'INFMAX,INFMAX1,HYDRO(1)*SOILIQW(1),-TOTLIQ', &
         INFMAX,INFMAX1,HYDRO(1)*SOILIQW(1),-TOTLIQ
    ENDIF

          IF (-TOTLIQ.GT.INFMAX)THEN
            RUNOFF=-TOTLIQ-INFMAX
            FLX=-INFMAX
    IF ( wrf_at_debug_level(3000) ) THEN
       print *,'FLX,RUNOFF1=',flx,runoff
    ENDIF
          ENDIF

          INFILTRP=FLX

          R7=.5*DZS/DELT
          R4=R4+R7
          FLX=FLX-SOILMOIS(1)*R7



          R8=UMVEG*R6*(1.-snowfrac)
          QTOT=QVATM+QCATM
          R9=TRANS
          R10=QTOT-QSG


          IF(R10.LE.0.) THEN
            QQ=(R5*R2-FLX+R9)/(R4-R5*R1-R10*R8/(REF-QMIN))
            FLXSAT=-DQM*(R4-R5*R1-R10*R8/(REF-QMIN))                &
                   +R5*R2+R9
          ELSE

            QQ=(R2*R5-FLX+R8*(QTOT-QCG-QVG)+R9)/(R4-R1*R5)
            FLXSAT=-DQM*(R4-R1*R5)+R2*R5+R8*(QTOT-QVG-QCG)+R9
          END IF

          IF(QQ.LT.0.) THEN

            SOILMOIS(1)=1.e-8

          ELSE IF(QQ.GT.DQM) THEN

            SOILMOIS(1)=DQM
    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'FLXSAT,FLX,DELT',FLXSAT,FLX,DELT,RUNOFF2
    ENDIF

            RUNOFF=RUNOFF+(FLXSAT-FLX)
          ELSE
            SOILMOIS(1)=min(dqm,max(1.e-8,QQ))
          END IF

    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'SOILMOIS,SOILIQW, soilice',SOILMOIS,SOILIQW,soilice*riw
   print *,'COSMC,RHSMC',COSMC,RHSMC
    ENDIF


          DO K=2,NZS
            KK=NZS-K+1
            QQ=COSMC(KK)*SOILMOIS(K-1)+RHSMC(KK)


           IF (QQ.LT.0.) THEN

            SOILMOIS(K)=1.e-8 

           ELSE IF(QQ.GT.DQM) THEN

            SOILMOIS(K)=DQM
             IF(K.EQ.NZS)THEN
    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'hydro(k),QQ,DQM,k',hydro(k),QQ,DQM,k
    ENDIF
               RUNOFF2=RUNOFF2+((QQ-DQM)*(ZSMAIN(K)-ZSHALF(K)))/DELT


             ELSE

               RUNOFF2=RUNOFF2+((QQ-DQM)*(ZSHALF(K+1)-ZSHALF(K)))/DELT

             ENDIF
           ELSE
            SOILMOIS(K)=min(dqm,max(1.e-8,QQ))
           END IF
          END DO
    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'END soilmois,soiliqw,soilice',soilmois,SOILIQW,soilice*riw
    ENDIF




           MAVAIL=max(.00001,min(1.,(SOILMOIS(1)/(REF-QMIN)*(1.-snowfrac)+1.*snowfrac)))




    END SUBROUTINE SOILMOIST



          SUBROUTINE SOILPROP(spp_lsm,rstochcol,fieldcol_sf, &

         nzs,fwsat,lwsat,tav,keepfr,                              &
         soilmois,soiliqw,soilice,                                &
         soilmoism,soiliqwm,soilicem,                             &

         QWRTZ,rhocs,dqm,qmin,psis,bclh,ksat,                     &

         riw,xlmelt,CP,G0_P,cvw,ci,                               & 
         kqwrtz,kice,kwt,                                         &

         thdif,diffu,hydro,cap)




















        IMPLICIT NONE



   INTEGER, INTENT(IN   )    ::                            NZS
   REAL                                                        , &
            INTENT(IN   )    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                          QWRTZ, &  
                                                           QMIN

   REAL,    DIMENSION(  1:nzs )                                , &
            INTENT(IN   )    ::                        SOILMOIS, &
                                                         keepfr


   REAL,     INTENT(IN   )   ::                              CP, &
                                                            CVW, &
                                                            RIW, &  
                                                         kqwrtz, &
                                                           kice, &
                                                            kwt, &
                                                         XLMELT, &
                                                            G0_P

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::          rstochcol
   REAL,     DIMENSION(1:NZS), INTENT(INOUT) ::      fieldcol_sf
   INTEGER,  INTENT(IN   )   ::                     spp_lsm      



   REAL,     DIMENSION(1:NZS)                                  , &
            INTENT(INOUT)  ::      cap,diffu,hydro             , &
                                   thdif,tav                   , &
                                   soilmoism                   , &
                                   soiliqw,soilice             , &
                                   soilicem,soiliqwm           , &
                                   fwsat,lwsat


   REAL,     DIMENSION(1:NZS)  ::  hk,detal,kasat,kjpl

   REAL    ::  x,x1,x2,x4,ws,wd,fact,fach,facd,psif,ci
   REAL    ::  tln,tavln,tn,pf,a,am,ame,h
   INTEGER ::  nzs1,k


   REAL    ::  kzero,gamd,kdry,kas,x5,sr,ke       
               

         nzs1=nzs-1


         kzero =2.       


         do k=1,nzs
            detal (k)=0.
            kasat (k)=0.
            kjpl  (k)=0.
            hk    (k)=0.
         enddo

           ws=dqm+qmin
           x1=xlmelt/(g0_p*psis)
           x2=x1/bclh*ws
           x4=(bclh+1.)/bclh

           gamd=(1.-ws)*2700.
           kdry=(0.135*gamd+64.7)/(2700.-0.947*gamd)
           kas=kqwrtz**qwrtz*kzero**(1.-qwrtz)

         DO K=1,NZS1
           tn=tav(k) - 273.15
           wd=ws - riw*soilicem(k)
           psif=psis*100.*(wd/(soiliqwm(k)+qmin))**bclh            &
                * (ws/wd)**3.

           pf=log10(abs(psif))
           fact=1.+riw*soilicem(k)

         IF(PF.LE.5.2) THEN
           HK(K)=420.*EXP(-(PF+2.7))*fact
         ELSE
           HK(K)=.1744*fact
         END IF

           IF(soilicem(k).NE.0.AND.TN.LT.0.) then



              DETAL(K)=273.15*X2/(TAV(K)*TAV(K))*                  &
                     (TAV(K)/(X1*TN))**X4

              if(keepfr(k).eq.1.) then
                 detal(k)=0.
              endif

           ENDIF


           kasat(k)=kas**(1.-ws)*kice**fwsat(k)                    &
                    *kwt**lwsat(k)

           X5=(soilmoism(k)+qmin)/ws
         if(soilicem(k).eq.0.) then
           sr=max(0.101,x5)
           ke=log10(sr)+1.



         else
           ke=x5
         endif

           kjpl(k)=ke*(kasat(k)-kdry)+kdry


            CAP(K)=(1.-WS)*RHOCS                                    &
                  + (soiliqwm(K)+qmin)*CVW                          &
                  + soilicem(K)*CI                                  &
                  + (dqm-soilmoism(k))*CP*1.2                       &
            - DETAL(K)*1.e3*xlmelt

           a=RIW*soilicem(K)

        if((ws-a).lt.0.12)then
           diffu(K)=0.
        else
           H=max(0.,(soilmoism(K)-a)/(max(1.e-8,(dqm-a))))
           facd=1.
        if(a.ne.0.)facd=1.-a/max(1.e-8,soilmoism(K))
          ame=max(1.e-8,dqm-riw*soilicem(K))

          diffu(K)=-BCLH*KSAT*PSIS/ame*                             &
                  (dqm/ame)**3.                                     &
                  *H**(BCLH+2.)*facd
         endif








            thdif(K)=KJPL(K)/CAP(K)

         END DO

    IF ( wrf_at_debug_level(3000) ) THEN
   print *,'soilice*riw,soiliqw,soilmois,ws',soilice*riw,soiliqw,soilmois,ws
    ENDIF
         DO K=1,NZS

         if((ws-riw*soilice(k)).lt.0.12)then
            hydro(k)=0.
         else
            fach=1.
          if(soilice(k).ne.0.)                                     &
             fach=1.-riw*soilice(k)/max(1.e-8,soilmois(k))
         am=max(1.e-8,dqm-riw*soilice(k))

          hydro(K)=min(KSAT,KSAT/am*                                        & 
                  (soiliqw(K)/am)                                  &
                  **(2.*BCLH+2.)                                   &
                  * fach)
          if(hydro(k)<1.e-10)hydro(k)=0.
         endif

       ENDDO


   END SUBROUTINE SOILPROP



           SUBROUTINE TRANSF(i,j,                                &

              nzs,nroot,soiliqw,tabs,lai,gswin,                  &

              dqm,qmin,ref,wilt,zshalf,pc,iland,                 &

              tranf,transum)










        IMPLICIT NONE




   INTEGER,  INTENT(IN   )   ::  i,j,nroot,nzs, iland

   REAL                                                        , &
            INTENT(IN   )    ::                GSWin, TABS, lai

   REAL                                                        , &
            INTENT(IN   )    ::                             DQM, &
                                                           QMIN, &
                                                            REF, &
                                                             PC, &
                                                           WILT

   REAL,     DIMENSION(1:NZS), INTENT(IN)  ::          soiliqw,  &
                                                         ZSHALF


   REAL,     DIMENSION(1:NZS), INTENT(OUT)  ::            TRANF
   REAL,     INTENT(OUT)  ::                            TRANSUM  


   REAL    ::  totliq, did
   INTEGER ::  k


   REAL    ::  gx,sm1,sm2,sm3,sm4,ap0,ap1,ap2,ap3,ap4
   REAL    ::  FTEM, PCtot, fsol, f1, cmin, cmax, totcnd
   REAL,     DIMENSION(1:NZS)   ::           PART


        do k=1,nzs
           part(k)=0.
           tranf(k)=0.
        enddo

        transum=0.
        totliq=soiliqw(1)+qmin
           sm1=totliq
           sm2=sm1*sm1
           sm3=sm2*sm1
           sm4=sm3*sm1
           ap0=0.299
           ap1=-8.152
           ap2=61.653
           ap3=-115.876
           ap4=59.656
           gx=ap0+ap1*sm1+ap2*sm2+ap3*sm3+ap4*sm4
          if(totliq.ge.ref) gx=1.
          if(totliq.le.0.) gx=0.
          if(gx.gt.1.) gx=1.
          if(gx.lt.0.) gx=0.
        DID=zshalf(2)
          part(1)=DID*gx
        IF(TOTLIQ.GT.REF) THEN
          TRANF(1)=DID
        ELSE IF(TOTLIQ.LE.WILT) THEN
          TRANF(1)=0.
        ELSE
          TRANF(1)=(TOTLIQ-WILT)/(REF-WILT)*DID
        ENDIF 



        DO K=2,NROOT
        totliq=soiliqw(k)+qmin
           sm1=totliq
           sm2=sm1*sm1
           sm3=sm2*sm1
           sm4=sm3*sm1
           gx=ap0+ap1*sm1+ap2*sm2+ap3*sm3+ap4*sm4
          if(totliq.ge.ref) gx=1.
          if(totliq.le.0.) gx=0.
          if(gx.gt.1.) gx=1.
          if(gx.lt.0.) gx=0.
          DID=zshalf(K+1)-zshalf(K)
          part(k)=did*gx
        IF(totliq.GE.REF) THEN
          TRANF(K)=DID
        ELSE IF(totliq.LE.WILT) THEN
          TRANF(K)=0.
        ELSE
          TRANF(K)=(totliq-WILT)                                &
                /(REF-WILT)*DID
        ENDIF


        END DO


      if(lai > 4.) then
        pctot=0.8
      else
        pctot=pc



      endif
    IF ( wrf_at_debug_level(3000) ) THEN

     print *,'i,j,pctot,lai,pc',i,j,pctot,lai,pc
    ENDIF



        IF (TABS .LE. 302.15) THEN
          FTEM = 1.0 / (1.0 + EXP(-0.41 * (TABS - 282.05)))
        ELSE
          FTEM = 1.0 / (1.0 + EXP(0.5 * (TABS - 314.0)))
        ENDIF
    IF ( wrf_at_debug_level(3000) ) THEN

     print *,'i,j,tabs,ftem',i,j,tabs,ftem
    ENDIF

     cmin = 1./rsmax_data
     cmax = 1./rstbl(iland)
    if(lai > 1.) then
     cmax = lai/rstbl(iland) 
    endif

       f1=0.









     if (GSWin < rgltbl(iland)) then
      fsol = 1. / (1. + exp(-0.034 * (GSWin - 3.5)))
     else
      fsol = 1.
     endif
    IF ( wrf_at_debug_level(3000) ) THEN

     print *,'i,j,GSWin,lai,f1,fsol',i,j,gswin,lai,f1,fsol
    ENDIF

     totcnd =(cmin + (cmax - cmin)*pctot*ftem*fsol)/cmax

    IF ( wrf_at_debug_level(3000) ) THEN

     print *,'i,j,iland,RGLTBL(iland),RSTBL(iland),RSMAX_DATA,totcnd'  &
             ,i,j,iland,RGLTBL(iland),RSTBL(iland),RSMAX_DATA,totcnd
    ENDIF


          transum=0.
        DO K=1,NROOT

         TRANF(k)=max(cmin,TRANF(k)*totcnd)
         transum=transum+tranf(k)
        END DO
    IF ( wrf_at_debug_level(3000) ) THEN

      print *,'i,j,transum,TRANF',i,j,transum,tranf
    endif


   END SUBROUTINE TRANSF



       SUBROUTINE VILKA(TN,D1,D2,PP,QS,TS,TT,NSTEP,ii,j,iland,isoil)




   REAL,     DIMENSION(1:5001),  INTENT(IN   )   ::  TT
   REAL,     INTENT(IN  )   ::  TN,D1,D2,PP
   INTEGER,  INTENT(IN  )   ::  NSTEP,ii,j,iland,isoil

   REAL,     INTENT(OUT  )  ::  QS, TS

   REAL    ::  F1,T1,T2,RN
   INTEGER ::  I,I1
     
       I=(TN-1.7315E2)/.05+1
       T1=173.1+FLOAT(I)*.05
       F1=T1+D1*TT(I)-D2
       I1=I-F1/(.05+D1*(TT(I+1)-TT(I)))
       I=I1
       IF(I.GT.5000.OR.I.LT.1) GOTO 1
  10   I1=I
       T1=173.1+FLOAT(I)*.05
       F1=T1+D1*TT(I)-D2
       RN=F1/(.05+D1*(TT(I+1)-TT(I)))
       I=I-INT(RN)                      
       IF(I.GT.5000.OR.I.LT.1) GOTO 1
       IF(I1.NE.I) GOTO 10
       TS=T1-.05*RN
       QS=(TT(I)+(TT(I)-TT(I+1))*RN)/PP
       GOTO 20

   1   PRINT *,'     AVOST IN VILKA     Table index= ',I

       print *,'I,J=',ii,j,'LU_index = ',iland, 'Psfc[hPa] = ',pp, 'Tsfc = ',tn
       CALL wrf_error_fatal3("<stdin>",6448,&
'  Crash in surface energy budget  ' )
   20  CONTINUE

   END SUBROUTINE VILKA


     SUBROUTINE SOILVEGIN  ( mosaic_lu,mosaic_soil,soilfrac,nscat,   &
                     shdmin, shdmax,                                 &
                     NLCAT,IVGTYP,ISLTYP,iswater,                    &
                     IFOREST,lufrac,vegfrac,EMISS,PC,ZNT,LAI,RDLAI2D,&
                     QWRTZ,RHOCS,BCLH,DQM,KSAT,PSIS,QMIN,REF,WILT,I,J)




















   IMPLICIT NONE

      integer,   parameter      ::      nsoilclas=19
      integer,   parameter      ::      nvegclas=24+3
      integer,   parameter      ::      ilsnow=99

   INTEGER,    INTENT(IN   )    ::      nlcat, nscat, iswater, i, j

























         REAL  LQMA(nsoilclas),LRHC(nsoilclas),                       &
               LPSI(nsoilclas),LQMI(nsoilclas),                       &
               LBCL(nsoilclas),LKAS(nsoilclas),                       &
               LWIL(nsoilclas),LREF(nsoilclas),                       &
               DATQTZ(nsoilclas)








     DATA LQMA /0.395, 0.410, 0.435, 0.485, 0.485, 0.451, 0.420,      &
                0.477, 0.476, 0.426, 0.492, 0.482, 0.451, 1.0,        &
                0.20,  0.435, 0.468, 0.200, 0.339/






        DATA LREF /0.174, 0.179, 0.249, 0.369, 0.369, 0.314, 0.299,   &
                   0.357, 0.391, 0.316, 0.409, 0.400, 0.314, 1.,      &
                   0.1,   0.249, 0.454, 0.17,  0.236/






        DATA LWIL/0.068, 0.075, 0.114, 0.179, 0.179, 0.155, 0.175,    &
                  0.218, 0.250, 0.219, 0.283, 0.286, 0.155, 0.0,      &
                  0.006, 0.114, 0.030, 0.006, 0.01/





        DATA LQMI/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
                  0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
                  0.004, 0.065, 0.020, 0.004, 0.008/







       DATA LPSI/0.121, 0.090, 0.218, 0.786, 0.786, 0.478, 0.299,     &
                 0.356, 0.630, 0.153, 0.490, 0.405, 0.478, 0.0,       &
                 0.121, 0.218, 0.468, 0.069, 0.069/







        DATA LKAS/1.76E-4, 1.56E-4, 3.47E-5, 7.20E-6, 7.20E-6,         &
                  6.95E-6, 6.30E-6, 1.70E-6, 2.45E-6, 2.17E-6,         &
                  1.03E-6, 1.28E-6, 6.95E-6, 0.0,     1.41E-4,         &
                  3.47E-5, 1.28E-6, 1.41E-4, 1.76E-4/






        DATA LBCL/4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,      &
                  7.75,  8.52, 10.40, 10.40, 11.40,  5.39,  0.0,       &
                  4.05,  4.90, 11.55,  2.79,  2.79/

        DATA LRHC /1.47,1.41,1.34,1.27,1.27,1.21,1.18,1.32,1.23,       &
                   1.18,1.15,1.09,1.21,4.18,2.03,2.10,1.09,2.03,1.47/

        DATA DATQTZ/0.92,0.82,0.60,0.25,0.10,0.40,0.60,0.10,0.35,      &
                    0.52,0.10,0.25,0.00,0.,0.60,0.0,0.25,0.60,0.92/



























































         REAL LALB(nvegclas),LMOI(nvegclas),LEMI(nvegclas),            &
              LROU(nvegclas),LTHI(nvegclas),LSIG(nvegclas),            &
              LPC(nvegclas)






        DATA  LALB/.18,.17,.18,.18,.18,.16,.19,.22,.20,.20,.16,.14,     &
                   .12,.12,.13,.08,.14,.14,.25,.15,.15,.15,.25,.55,     &
                   .30,.16,.60 /
        DATA LEMI/.88,4*.92,.93,.92,.88,.9,.92,.93,.94,                 &
                  .95,.95,.94,.98,.95,.95,.85,.92,.93,.92,.85,.95,      &
                  .85,.85,.90 /



         DATA LROU/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.5,.5,       & 
                   .5,.5,.5,.0001,.2,.4,.05,.1,.15,.1,.065,.05,         &
                   .01,.15,.01 /

        DATA LMOI/.1,.3,.5,.25,.25,.35,.15,.1,.15,.15,.3,.3,            &
                  .5,.3,.3,1.,.6,.35,.02,.5,.5,.5,.02,.95,.40,.50,.40/




       DATA LPC /0.4,0.3,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,5*0.55,0.,0.55,0.55,                   &
                 0.3,0.3,0.4,0.4,0.3,0.,.3,0.,0./








   INTEGER      ::                &
                                                         IVGTYP, &
                                                         ISLTYP
   INTEGER,    INTENT(IN   )    ::     mosaic_lu, mosaic_soil

   REAL,       INTENT(IN )      ::   SHDMAX
   REAL,       INTENT(IN )      ::   SHDMIN
   REAL,       INTENT(IN )      ::   VEGFRAC
   REAL,     DIMENSION( 1:NLCAT ),  INTENT(IN)::         LUFRAC
   REAL,     DIMENSION( 1:NSCAT ),  INTENT(IN)::         SOILFRAC

   REAL                                                        , &
            INTENT (  OUT)            ::                     pc

   REAL                                                        , &
            INTENT (INOUT   )         ::                  emiss, &
                                                            lai, &
                                                            znt
  LOGICAL, intent(in) :: rdlai2d

   REAL                                                        , &
            INTENT(  OUT)    ::                           RHOCS, &
                                                           BCLH, &
                                                            DQM, &
                                                           KSAT, &
                                                           PSIS, &
                                                           QMIN, &
                                                          QWRTZ, &
                                                            REF, &
                                                           WILT
   INTEGER, INTENT (  OUT)   ::                         iforest






   INTEGER   ::   kstart, kfin, lstart, lfin
   INTEGER   ::   k
   REAL      ::   area,  factor, znt1, lb
   REAL,     DIMENSION( 1:NLCAT ) :: ZNTtoday, LAItoday, deltalai











        iforest = IFORTBL(IVGTYP)

    IF ( wrf_at_debug_level(3000) ) THEN
      if(i.eq.375.and.j.eq.254)then
        print *,'ifortbl(ivgtyp),ivgtyp,laitbl(ivgtyp),z0tbl(ivgtyp)', &
            ifortbl(ivgtyp),ivgtyp,laitbl(ivgtyp),z0tbl(ivgtyp)
      endif
    ENDIF

        deltalai(:) = 0.





      if((shdmax - shdmin) .lt. 1) then
        factor = 1. 
      else
        factor = 1. - max(0.,min(1.,(vegfrac - shdmin)/max(1.,(shdmax-shdmin))))
      endif


      do k = 1,nlcat
       if(IFORTBL(k) == 1) deltalai(k)=min(0.2,0.8*LAITBL(K))
       if(IFORTBL(k) == 2 .or. IFORTBL(k) == 7) deltalai(k)=min(0.5,0.8*LAITBL(K))
       if(IFORTBL(k) == 3) deltalai(k)=min(0.45,0.8*LAITBL(K))
       if(IFORTBL(k) == 4) deltalai(k)=min(0.75,0.8*LAITBL(K))
       if(IFORTBL(k) == 5) deltalai(k)=min(0.86,0.8*LAITBL(K))

       if(k.ne.iswater) then

        LAItoday(k) = LAITBL(K) - deltalai(k) * factor

         if(IFORTBL(k) == 7) then

           ZNTtoday(k) = Z0TBL(K) - 0.125 * factor
         else
           ZNTtoday(k) = Z0TBL(K)
         endif
       else
        LAItoday(k) = LAITBL(K)

        ZNTtoday(k) = ZNT 
       endif
      enddo

    IF ( wrf_at_debug_level(3000) ) THEN
      if(i.eq.358.and.j.eq.260)then
        print *,'ivgtyp,factor,vegfrac,shdmin,shdmax,deltalai,laitoday(ivgtyp),znttoday(ivgtyp)', &
         i,j,ivgtyp,factor,vegfrac,shdmin,shdmax,deltalai,laitoday(ivgtyp),znttoday(ivgtyp)
      endif
    ENDIF

        EMISS = 0.
        ZNT   = 0.
        ZNT1  = 0.
        PC    = 0.
        if(.not.rdlai2d) LAI = 0.
        AREA  = 0.




        LB = 5.
      if(mosaic_lu == 1) then
      do k = 1,nlcat
        AREA  = AREA + lufrac(k)
        EMISS = EMISS+ LEMITBL(K)*lufrac(k)
        ZNT   = ZNT  + lufrac(k)/ALOG(LB/ZNTtoday(K))**2.

        ZNT1  = ZNT1 + lufrac(k)*ZNTtoday(K)
        if(.not.rdlai2d) LAI = LAI  + LAItoday(K)*lufrac(k)
        PC    = PC   + PCTBL(K)*lufrac(k)
      enddo

       if (area.gt.1.) area=1.
       if (area <= 0.) then
          print *,'Bad area of grid box', area
          stop
       endif

    IF ( wrf_at_debug_level(3000) ) THEN
      if(i.eq.358.and.j.eq.260) then
        print *,'area=',area,i,j,ivgtyp,nlcat,(lufrac(k),k=1,nlcat),EMISS,ZNT,ZNT1,LAI,PC
      endif
    ENDIF

        EMISS = EMISS/AREA
        ZNT1   = ZNT1/AREA
        ZNT = LB/EXP(SQRT(1./ZNT))
        if(.not.rdlai2d) LAI = LAI/AREA
        PC    = PC /AREA

    IF ( wrf_at_debug_level(3000) ) THEN
      if(i.eq.358.and.j.eq.260) then
        print *,'mosaic=',i,j,ivgtyp,nlcat,(lufrac(k),k=1,nlcat),EMISS,ZNT,ZNT1,LAI,PC
      endif
    ENDIF


      else
        EMISS = LEMITBL(IVGTYP)
        ZNT   = ZNTtoday(IVGTYP)
        PC    = PCTBL(IVGTYP)
        if(.not.rdlai2d) LAI = LAItoday(IVGTYP)
     endif


          RHOCS  = 0.
          BCLH   = 0.
          DQM    = 0.
          KSAT   = 0.
          PSIS   = 0.
          QMIN   = 0.
          REF    = 0.
          WILT   = 0.
          QWRTZ  = 0.
          AREA   = 0.

       if(mosaic_soil == 1 ) then
            do k = 1, nscat
        if(k.ne.14) then

          AREA   = AREA + soilfrac(k)
          RHOCS  = RHOCS + HC(k)*1.E6*soilfrac(k)
          BCLH   = BCLH + BB(K)*soilfrac(k)
          DQM    = DQM + (MAXSMC(K)-                               &
                   DRYSMC(K))*soilfrac(k)
          KSAT   = KSAT + SATDK(K)*soilfrac(k)
          PSIS   = PSIS - SATPSI(K)*soilfrac(k)
          QMIN   = QMIN + DRYSMC(K)*soilfrac(k)
          REF    = REF + REFSMC(K)*soilfrac(k)
          WILT   = WILT + WLTSMC(K)*soilfrac(k)
          QWRTZ  = QWRTZ + QTZ(K)*soilfrac(k)
        endif
            enddo
       if (area.gt.1.) area=1.
       if (area <= 0.) then


          RHOCS  = HC(ISLTYP)*1.E6
          BCLH   = BB(ISLTYP)
          DQM    = MAXSMC(ISLTYP)-                               &
                   DRYSMC(ISLTYP)
          KSAT   = SATDK(ISLTYP)
          PSIS   = - SATPSI(ISLTYP)
          QMIN   = DRYSMC(ISLTYP)
          REF    = REFSMC(ISLTYP)
          WILT   = WLTSMC(ISLTYP)
          QWRTZ  = QTZ(ISLTYP)
       else
          RHOCS  = RHOCS/AREA
          BCLH   = BCLH/AREA
          DQM    = DQM/AREA
          KSAT   = KSAT/AREA
          PSIS   = PSIS/AREA
          QMIN   = QMIN/AREA
          REF    = REF/AREA
          WILT   = WILT/AREA
          QWRTZ  = QWRTZ/AREA
       endif


        else
      if(isltyp.ne.14) then
          RHOCS  = HC(ISLTYP)*1.E6
          BCLH   = BB(ISLTYP)
          DQM    = MAXSMC(ISLTYP)-                               &
                   DRYSMC(ISLTYP)
          KSAT   = SATDK(ISLTYP)
          PSIS   = - SATPSI(ISLTYP)
          QMIN   = DRYSMC(ISLTYP)
          REF    = REFSMC(ISLTYP)
          WILT   = WLTSMC(ISLTYP)
          QWRTZ  = QTZ(ISLTYP)
      endif
        endif













   END SUBROUTINE SOILVEGIN


  SUBROUTINE RUCLSMINIT( SH2O,SMFR3D,TSLB,SMOIS,ISLTYP,IVGTYP,     &
                     mminlu, XICE,mavail,nzs, iswater, isice,      &
                     znt, restart, allowed_to_read ,               &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
   IMPLICIT NONE


   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte,  &
                                    nzs, iswater, isice
   CHARACTER(LEN=*), INTENT(IN   )    ::                 MMINLU

   REAL, DIMENSION( ims:ime, 1:nzs, jms:jme )                    , &
            INTENT(IN)    ::                                 TSLB, &
                                                            SMOIS

   INTEGER, DIMENSION( ims:ime, jms:jme )                        , &
            INTENT(INOUT)    ::                     ISLTYP,IVGTYP

   REAL, DIMENSION( ims:ime, 1:nzs, jms:jme )                    , &
            INTENT(INOUT)    ::                            SMFR3D, &
                                                             SH2O

   REAL, DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(INOUT)    ::                       XICE,MAVAIL

   REAL, DIMENSION( ims:ime, jms:jme )                           , &
            INTENT(  OUT)    ::                               znt

   REAL, DIMENSION ( 1:nzs )  ::                           SOILIQW

   LOGICAL , INTENT(IN) :: restart, allowed_to_read 


  INTEGER ::  I,J,L,itf,jtf
  REAL    ::  RIW,XLMELT,TLN,DQM,REF,PSIS,QMIN,BCLH

  character*8 :: MMINLURUC, MMINSL

   INTEGER                   :: errflag





        RIW=900.*1.e-3
        XLMELT=3.35E+5


   IF ( allowed_to_read ) THEN
     CALL wrf_message( 'INITIALIZE THREE LSM RELATED TABLES' )
      if(mminlu == 'USGS') then
        MMINLURUC='USGS-RUC'
      elseif(mminlu == 'MODIS' .OR. &
        &    mminlu == 'MODIFIED_IGBP_MODIS_NOAH') then
        MMINLURUC='MODI-RUC'
      endif
        MMINSL='STAS-RUC'

    print *,'RUCLSMINIT uses ',mminluruc
     call RUCLSM_SOILVEGPARM( MMINLURUC, MMINSL)   
   ENDIF











 IF(.not.restart)THEN

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   errflag = 0
   DO j = jts,jtf
     DO i = its,itf
       IF ( ISLTYP( i,j ) .LT. 1 ) THEN
         errflag = 1
         WRITE(err_message,*)"module_sf_ruclsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
         CALL wrf_message(err_message)
       ENDIF
     ENDDO
   ENDDO
   IF ( errflag .EQ. 1 ) THEN
      CALL wrf_error_fatal3("<stdin>",7024,&
"module_sf_ruclsm.F: lsminit: out of range value "// &
                            "of ISLTYP. Is this field in the input?" )
   ENDIF

   DO J=jts,jtf
       DO I=its,itf

        ZNT(I,J)   = Z0TBL(IVGTYP(I,J))






          DQM    = MAXSMC   (ISLTYP(I,J)) -                               &
                   DRYSMC   (ISLTYP(I,J))
          REF    = REFSMC   (ISLTYP(I,J))
          PSIS   = - SATPSI (ISLTYP(I,J))
          QMIN   = DRYSMC   (ISLTYP(I,J))
          BCLH   = BB       (ISLTYP(I,J))




    IF(xice(i,j).gt.0.) THEN

         DO L=1,NZS
           smfr3d(i,l,j)=1.
           sh2o(i,l,j)=0.
           mavail(i,j) = 1.
         ENDDO
    ELSE
       if(isltyp(i,j).ne.14 ) then

           mavail(i,j) = max(0.00001,min(1.,(smois(i,1,j)-qmin)/(ref-qmin)))
         DO L=1,NZS

         tln=log(TSLB(i,l,j)/273.15)
          
          if(tln.lt.0.) then
           soiliqw(l)=(dqm+qmin)*(XLMELT*                        &
         (tslb(i,l,j)-273.15)/tslb(i,l,j)/9.81/psis)             &
          **(-1./bclh)

           soiliqw(l)=max(0.,soiliqw(l))
           soiliqw(l)=min(soiliqw(l),smois(i,l,j))
           sh2o(i,l,j)=soiliqw(l)
           smfr3d(i,l,j)=(smois(i,l,j)-soiliqw(l))/RIW
         
          else
           smfr3d(i,l,j)=0.
           sh2o(i,l,j)=smois(i,l,j)
          endif
         ENDDO
    
       else

         DO L=1,NZS
           smfr3d(i,l,j)=0.
           sh2o(i,l,j)=1.
           mavail(i,j) = 1.
         ENDDO
       endif
    ENDIF

    ENDDO
   ENDDO

 ENDIF

  END SUBROUTINE ruclsminit

















        SUBROUTINE RUCLSM_SOILVEGPARM( MMINLURUC, MMINSL)


        IMPLICIT NONE

        integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
        integer :: ierr
        INTEGER , PARAMETER :: OPEN_OK = 0

        character*8 :: MMINLURUC, MMINSL
        character*128 :: mess , message, vege_parm_string
        logical, external :: wrf_dm_on_monitor




























       IF ( wrf_dm_on_monitor() ) THEN

        OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
          CALL wrf_error_fatal3("<stdin>",7159,&
message )
        END IF

        WRITE ( mess, * ) 'INPUT VEGPARM FOR ',MMINLURUC
        CALL wrf_message( mess )

        LUMATCH=0

 2000   FORMAT (A8)
        READ (19,'(A)') vege_parm_string
        outer : DO 
           READ (19,2000,END=2002)LUTYPE
           READ (19,*)LUCATS,IINDEX

           WRITE( mess , * ) 'VEGPARM FOR ',LUTYPE,' FOUND', LUCATS,' CATEGORIES'
           CALL wrf_message( mess )

           IF(LUTYPE.NE.MMINLURUC)THEN    
              write ( mess , * ) 'Skipping ', LUTYPE, ' table'
              CALL wrf_message( mess )
              DO LC=1,LUCATS
                 READ (19,*)
              ENDDO
              inner : DO               
                 READ (19,'(A)',END=2002) vege_parm_string
                 IF (TRIM(vege_parm_string) .EQ. "Vegetation Parameters") THEN
                    EXIT inner
                 END IF
               ENDDO inner
           ELSE
              LUMATCH=1
              write ( mess , * ) 'Found ', LUTYPE, ' table'
              CALL wrf_message( mess )
              EXIT outer                
           END IF

        ENDDO outer

        IF (LUMATCH == 1) then
           write ( mess , * ) 'Reading ',LUTYPE,' table'
           CALL wrf_message( mess )
           DO LC=1,LUCATS
              READ (19,*)IINDEX,ALBTBL(LC),Z0TBL(LC),LEMITBL(LC),PCTBL(LC), &
                         SHDTBL(LC),IFORTBL(LC),RSTBL(LC),RGLTBL(LC),         &
                         HSTBL(LC),SNUPTBL(LC),LAITBL(LC),MAXALB(LC)
           ENDDO

           READ (19,*)
           READ (19,*)TOPT_DATA
           READ (19,*)
           READ (19,*)CMCMAX_DATA
           READ (19,*)
           READ (19,*)CFACTR_DATA
           READ (19,*)
           READ (19,*)RSMAX_DATA
           READ (19,*)
           READ (19,*)BARE
           READ (19,*)
           READ (19,*)NATURAL
           READ (19,*)
           READ (19,*)CROP
           READ (19,*)
           READ (19,*,iostat=ierr)URBAN
           if ( ierr /= 0 ) call wrf_message     (  "-------- VEGPARM.TBL READ ERROR --------")
           if ( ierr /= 0 ) call wrf_message     (  "Problem read URBAN from VEGPARM.TBL")
           if ( ierr /= 0 ) call wrf_message     (  " -- Use updated version of VEGPARM.TBL  ")
           if ( ierr /= 0 ) call wrf_error_fatal3("<stdin>",7226,&
"Problem read URBAN from VEGPARM.TBL")

        ENDIF

 2002   CONTINUE
        CLOSE (19)

    IF ( wrf_at_debug_level(3000) ) THEN
         print *,' LEMITBL, PCTBL, Z0TBL, LAITBL --->', LEMITBL, PCTBL, Z0TBL, LAITBL
    ENDIF


        IF (LUMATCH == 0) then
           CALL wrf_error_fatal3("<stdin>",7240,&
"Land Use Dataset '"//MMINLURUC//"' not found in VEGPARM.TBL.")
        ENDIF

      END IF

      CALL wrf_dm_bcast_string  ( LUTYPE  , 8 )
      CALL wrf_dm_bcast_integer ( LUCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_real    ( ALBTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( Z0TBL   , NLUS )
      CALL wrf_dm_bcast_real    ( LEMITBL , NLUS )
      CALL wrf_dm_bcast_real    ( PCTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SHDTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( IFORTBL , NLUS )
      CALL wrf_dm_bcast_real    ( RSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( RGLTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( HSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SNUPTBL , NLUS )
      CALL wrf_dm_bcast_real    ( LAITBL  , NLUS )
      CALL wrf_dm_bcast_real    ( MAXALB  , NLUS )
      CALL wrf_dm_bcast_real    ( TOPT_DATA    , 1 )
      CALL wrf_dm_bcast_real    ( CMCMAX_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( CFACTR_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( RSMAX_DATA  , 1 )
      CALL wrf_dm_bcast_integer ( BARE        , 1 )
      CALL wrf_dm_bcast_integer ( NATURAL     , 1 )
      CALL wrf_dm_bcast_integer ( CROP        , 1 )
      CALL wrf_dm_bcast_integer ( URBAN       , 1 )




      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
          CALL wrf_error_fatal3("<stdin>",7279,&
message )
        END IF

        WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICATION = ',MMINSL
        CALL wrf_message( mess )

        LUMATCH=0

        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
        READ (19,*)SLCATS,IINDEX
        IF(SLTYPE.NE.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),HC(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
        ENDIF
        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
        READ (19,*)SLCATS,IINDEX
 
        IF(SLTYPE.EQ.MMINSL)THEN
            WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ',SLTYPE,' FOUND', &
                  SLCATS,' CATEGORIES'
            CALL wrf_message ( mess )
          LUMATCH=1
        ENDIF
            IF(SLTYPE.EQ.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),HC(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
           ENDIF

 2003   CONTINUE

        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_string  ( SLTYPE  , 8 )
      CALL wrf_dm_bcast_string  ( MMINSL  , 8 )  
      CALL wrf_dm_bcast_integer ( SLCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_real    ( BB      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( DRYSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( HC      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( MAXSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( REFSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATPSI  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDK   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDW   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( WLTSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( QTZ     , NSLTYPE )

      IF(LUMATCH.EQ.0)THEN
          CALL wrf_message( 'SOIl TEXTURE IN INPUT FILE DOES NOT ' )
          CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
          CALL wrf_error_fatal3("<stdin>",7340,&
'INCONSISTENT OR MISSING SOILPARM FILE' )
      ENDIF




      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
          CALL wrf_error_fatal3("<stdin>",7352,&
message )
        END IF

        READ (19,*)
        READ (19,*)
        READ (19,*) NUM_SLOPE

          SLPCATS=NUM_SLOPE

          DO LC=1,SLPCATS
              READ (19,*)SLOPE_DATA(LC)
          ENDDO

          READ (19,*)
          READ (19,*)SBETA_DATA
          READ (19,*)
          READ (19,*)FXEXP_DATA
          READ (19,*)
          READ (19,*)CSOIL_DATA
          READ (19,*)
          READ (19,*)SALP_DATA
          READ (19,*)
          READ (19,*)REFDK_DATA
          READ (19,*)
          READ (19,*)REFKDT_DATA
          READ (19,*)
          READ (19,*)FRZK_DATA
          READ (19,*)
          READ (19,*)ZBOT_DATA
          READ (19,*)
          READ (19,*)CZIL_DATA
          READ (19,*)
          READ (19,*)SMLOW_DATA
          READ (19,*)
          READ (19,*)SMHIGH_DATA
        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( NUM_SLOPE    ,  1 )
      CALL wrf_dm_bcast_integer ( SLPCATS      ,  1 )
      CALL wrf_dm_bcast_real    ( SLOPE_DATA   ,  NSLOPE )
      CALL wrf_dm_bcast_real    ( SBETA_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( FXEXP_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( CSOIL_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SALP_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( REFDK_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( REFKDT_DATA  ,  1 )
      CALL wrf_dm_bcast_real    ( FRZK_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( ZBOT_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( CZIL_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( SMLOW_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SMHIGH_DATA  ,  1 )



      END SUBROUTINE RUCLSM_SOILVEGPARM



  SUBROUTINE SOILIN (ISLTYP, DQM, REF, PSIS, QMIN, BCLH )


























         integer,   parameter      ::      nsoilclas=19

         integer, intent ( in)  ::                          isltyp
         real,    intent ( out) ::               dqm,ref,qmin,psis

         REAL  LQMA(nsoilclas),LREF(nsoilclas),LBCL(nsoilclas),       &
               LPSI(nsoilclas),LQMI(nsoilclas)








     DATA LQMA /0.395, 0.410, 0.435, 0.485, 0.485, 0.451, 0.420,      &
                0.477, 0.476, 0.426, 0.492, 0.482, 0.451, 1.0,        &
                0.20,  0.435, 0.468, 0.200, 0.339/


        DATA LREF /0.174, 0.179, 0.249, 0.369, 0.369, 0.314, 0.299,   &
                   0.357, 0.391, 0.316, 0.409, 0.400, 0.314, 1.,      &
                   0.1,   0.249, 0.454, 0.17,  0.236/


        DATA LQMI/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
                  0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
                  0.004, 0.065, 0.020, 0.004, 0.008/


       DATA LPSI/0.121, 0.090, 0.218, 0.786, 0.786, 0.478, 0.299,     &
                 0.356, 0.630, 0.153, 0.490, 0.405, 0.478, 0.0,       &
                 0.121, 0.218, 0.468, 0.069, 0.069/


        DATA LBCL/4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,      &
                  7.75,  8.52, 10.40, 10.40, 11.40,  5.39,  0.0,       &
                  4.05,  4.90, 11.55,  2.79,  2.79/


          DQM    = LQMA(ISLTYP)-                               &
                   LQMI(ISLTYP)
          REF    = LREF(ISLTYP)
          PSIS   = - LPSI(ISLTYP)
          QMIN   = LQMI(ISLTYP)
          BCLH   = LBCL(ISLTYP)

  END SUBROUTINE SOILIN

END MODULE module_sf_ruclsm
