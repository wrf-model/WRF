

MODULE module_sf_gfdl



CONTAINS


   SUBROUTINE SF_GFDL(U3D,V3D,T3D,QV3D,P3D,                     &
                     CP,ROVCP,R,XLV,PSFC,CHS,CHS2,CQS2, CPM,    &
                     DT, SMOIS,num_soil_layers,ISLTYP,ZNT,      &
                     UST,PSIM,PSIH,                             &   
                     XLAND,HFX,QFX,TAUX,TAUY,LH,GSW,GLW,TSK,FLHC,FLQC,    & 
                     QGH,QSFC,U10,V10,                          &
                     ICOEF_SF,IWAVECPL,LCURR_SF,CHARN,MSANG,SCURX, SCURY,&
                     pert_Cd, ens_random_seed, ens_Cdamp,       &
                     GZ1OZ0,WSPD,BR,ZKMAX, ISFFLX,              &
                     EP1,EP2,KARMAN,NTSFLG,SFENTH,              &
                     Cd_out,Ch_out,                             &
                     ids,ide, jds,jde, kds,kde,                 &
                     ims,ime, jms,jme, kms,kme,                 &
                     its,ite, jts,jte, kts,kte                  )

      USE MODULE_GFS_MACHINE, ONLY : kind_phys
      USE MODULE_GFS_FUNCPHYS , ONLY : gfuncphys,fpvs
      USE MODULE_GFS_PHYSCONS, grav => con_g

      IMPLICIT NONE


























































      character*255 :: message
      INTEGER, INTENT(IN) ::            ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        ISFFLX,NUM_SOIL_LAYERS,NTSFLG
      INTEGER, INTENT(IN) ::            ICOEF_SF
      INTEGER, INTENT(IN) ::            IWAVECPL
      LOGICAL, INTENT(IN) ::            LCURR_SF
      logical,intent(in)  :: pert_Cd 
      integer,intent(in)  :: ens_random_seed
      real,intent(in)     :: ens_Cdamp

      REAL,    INTENT(IN) ::                                            &
                                        CP,                             &
                                        EP1,                            &
                                        EP2,                            &
                                        KARMAN,                         &
                                        R,                              & 
                                        ROVCP,                          &
                                        DT,                             &
                                        SFENTH,                         &
                                        XLV 

      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      & 
                                        P3D,                            &
                                        QV3D,                           &
                                        T3D,                            &
                                        U3D,                            &
                                        V3D
      INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   ISLTYP
      REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SMOIS

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        PSFC,                           &
                                        GLW,                            &
                                        GSW,                            &
                                        XLAND                           
      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        CHARN,                          &
                                        MSANG,                          &
                                        SCURX,                          &
                                        SCURY
      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            & 
                                        TSK,                            &
                                        BR,                             &
                                        ZKMAX,                          &
                                        CHS,                            &
                                        Cd_out,                         &
                                        Ch_out,                         &
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
                                        UST,                            &
                                        ZNT,                            &
                                        WSPD,                           &
                                        TAUX,                           & 
                                        TAUY

      REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::               &
                                        U10,                            &
                                        V10                            



      REAL ::                           ESAT,                           &
                                        cpcgs,                          &
                                        smc,                            &
                                        smcdry,                         &
                                        smcmax

      REAL     (kind=kind_phys) ::                                      &
                                        RHOX
      REAL, DIMENSION(1:30)    ::       MAXSMC, &
                                        DRYSMC
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

      REAL,    DIMENSION(kms:kme, ims:ime) ::                           &
                                        rpc,                            &
                                        tpc,                            &
                                        upc,                            &
                                        vpc
    
     REAL,    DIMENSION(ims:ime) ::                                     &
                                        pspc,                           &
                                        pkmax,                          &
                                        tstrc,                          &
                                        zoc,                            &
                                        mzoc,                           &  
                                        tzot,                           &  
                                        wetc,                           &
                                        slwdc,                          &
                                        rib,                            &

                                        tkmax,                          &
                                        fxmx,                           &
                                        fxmy,                           &
                                        cdm,                            &
                                        fxh,                            &
                                        fxe,                            &
                                        xxfh,                           &
                                        xxfh2,                          &
                                        wind10,                         &
                                        tjloc,                          &
                                        alpha,                          &
                                        gamma,                          &
                                        xcur,                           &
                                        ycur

      INTEGER ::                                                        &
                                        I,                              &
                                        II,                             &
                                        IGPVS,                          &
                                        IM,                             &
                                        J,                              &
                                        K,                              &
                                        KM

      real :: tmp9,zhalf  

        DATA MAXSMC/0.339, 0.421, 0.434, 0.476, 0.476, 0.439,  &
                    0.404, 0.464, 0.465, 0.406, 0.468, 0.468,  &
                    0.439, 1.000, 0.200, 0.421, 0.000, 0.000,  &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000/

        DATA DRYSMC/0.010, 0.028, 0.047, 0.084, 0.084, 0.066,     &
                    0.067, 0.120, 0.103, 0.100, 0.126, 0.138,     &
                    0.066, 0.000, 0.006, 0.028, 0.000, 0.000,     &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000,     &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA IGPVS/0/
      save igpvs


   if(igpvs.eq.0) then

   endif
   igpvs=1

   IM=ITE-ITS+1
   KM=KTE-KTS+1

   WRITE(message,*)'WITHIN THE GFDL SCHEME, NTSFLG=1 FOR GFDL SLAB  2010 UPGRADS',NTSFLG
   call wrf_debug(2,message)



   DO J=jts,jte

      DO i=its,ite
        DDVEL(I)=0.
        RCL(i)=1.
        PRSL1(i)=P3D(i,kts,j)*.001
         wetc(i)=1.0
         if(xland(i,j).lt.1.99) then
         smc=smois(i,1,j)
         smcdry=drysmc(isltyp(i,j))
         smcmax=maxsmc(isltyp(i,j))
         wetc(i)=(smc-smcdry)/(smcmax-smcdry)
         wetc(i)=amin1(1.,amax1(wetc(i),0.))
         endif  

        pspc(i)=PSFC(i,j)*10.   
        pkmax(i)=P3D(i,kts,j)*10.
        PS(i)=PSFC(i,j)*.001
        Q1(I) = QV3D(i,kts,j)
        rpc(kts,i)=QV3D(i,kts,j)

        QSURF(I)=0.
        SHELEG(I)=0.
        SLIMSK(i)=ABS(XLAND(i,j)-2.)
        TSKIN(i)=TSK(i,j)
        tstrc(i)=TSK(i,j)
        T1(I) = T3D(i,kts,j)
        tpc(kts,i)=T3D(i,kts,j)
        U1(I) = U3D(i,kts,j)
        upc(kts,i)=U3D(i,kts,j) * 100.
        USTAR(I) = UST(i,j)
        V1(I) = V3D(i,kts,j)
        vpc(kts,i)=v3D(i,kts,j) * 100.
        Z0RL(I) = ZNT(i,j)*100.
        zoc(i)=ZNT(i,j)*100.
        cdm(i) = Cd_out(i,j)
         if(XLAND(i,j).gt.1.99)  zoc(i)=- zoc(i)



        slwdc(i)=gsw(i,j)+glw(i,j)
        slwdc(i)=0.239*60.*slwdc(i)*1.e-4
         tjloc(i)=float(j)
        alpha(i) = charn(i,j)
        gamma(i) = msang(i,j)
        xcur(i) = scurx(i,j)
        ycur(i) = scury(i,j)


 
 
          wind10(i)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
        
           zhalf=0.0
          if (wind10(i) <= 1.0e-10 .or. wind10(i) > 150.0) then

           zhalf = -R*tpc(kts,i)*alog(pkmax(i)/pspc(i))/grav
           wind10(i)=sqrt(u1(i)*u1(i)+v1(i)*v1(i))*alog(10.0/znt(i,j))/alog(zhalf/znt(i,j))
          endif
           wind10(i)=wind10(i)*100.0   
           zhalf = -R*tpc(kts,i)*alog(pkmax(i)/pspc(i))/grav





        

      ENDDO

      DO i=its,ite
         PRSLKI(i)=(PS(I)/PRSL1(I))**ROVCP
         THGB(I)=TSKIN(i)*(100./PS(I))**ROVCP
         THX(I)=T1(i)*(100./PRSL1(I))**ROVCP
         RHO1(I)=PRSL1(I)*1000./(R*T1(I)*(1.+EP1*Q1(I)))
         Q1(I)=Q1(I)/(1.+Q1(I))
      ENDDO












     CALL MFLUX2(  fxh,fxe,fxmx,fxmy,cdm,rib,xxfh,zoc,mzoc,tstrc,   &    
                   pspc,pkmax,wetc,slwdc,tjloc,                &
                   icoef_sf,iwavecpl,lcurr_sf,alpha,gamma,xcur,ycur,           &
                   pert_Cd, ens_random_seed, ens_Cdamp,        &
                   upc,vpc,tpc,rpc,dt,J,wind10,xxfh2,ntsflg,SFENTH,   &
                   tzot,   &  
                   ids,ide, jds,jde, kds,kde,                  &
                   ims,ime, jms,jme, kms,kme,                  &
                   its,ite, jts,jte, kts,kte                   )











1010  format(2I4,9F11.6)



















      DO i=its,ite


        IF(NTSFLG==1)    then
        tsk(i,j) = tstrc(i)      


        if(j.eq.jde) then
         tsk(i,j) = tsk(i,j-1)
         endif

        if(j.eq.jds) then
         tsk(i,j) = tsk(i,j+1)
         endif
   
        if(i.eq.ide) tsk(i,j) = tsk(i-1,j)
        if(i.eq.ids) tsk(i,j) = tsk(i+1,j)
        endif


        znt(i,j)= 0.01*abs(zoc(i))
        wspd(i,j) = SQRT(upc(kts,i)*upc(kts,i) + vpc(kts,i)*vpc(kts,i))
        wspd(i,j) = amax1(wspd(i,j)    ,100.)/100.
        u10m(i) = u1(i)*(wind10(i)/wspd(i,j))/100.
        v10m(i) = v1(i)*(wind10(i)/wspd(i,j))/100.



        zkmax(i,j) = -R*tpc(kts,i)*alog(pkmax(i)/pspc(i))/grav


        gz1oz0(i,j)=alog(zkmax(i,j)/znt(i,j))
        ustar   (i)= 0.01*sqrt(cdm(i)*   &
                   (upc(kts,i)*upc(kts,i) + vpc(kts,i)*vpc(kts,i)))

        qfx   (i,j)=-10.*fxe(i)            



        hfx   (i,j)=       -10.*CP*fxh(i)  
        taux  (i,j)= fxmx(i)/10.    
        tauy  (i,j)= fxmy(i)/10.    
        fm(i)         = karman/sqrt(cdm(i))
        fh(i)         = karman*xxfh(i)            
        PSIM(i,j)=GZ1OZ0(i,j)-FM(i)
        PSIH(i,j)=GZ1OZ0(i,j)-FH(i)
        fh2(i)         = karman*xxfh2(i)            
        ch(i)      = karman*karman/(fm(i) * fh(i))
        cm(i)      = cdm(i)
        Cd_out(i,j) = cm(i)
        Ch_out(i,j) = ch(i)


         if ( wind10(i) .ge. 0.1 ) then
           cd_out(i,j)=cm(i)* (wspd(i,j)/(0.01*wind10(i)) )**2
           tmp9=0.01*abs(tzot(i))
           ch_out(i,j)=ch(i)*(wspd(i,j)/(0.01*wind10(i)) ) * &
                     (alog(zkmax(i,j)/tmp9)/alog(10.0/tmp9))





         endif

        U10(i,j)=U10M(i)
        V10(i,j)=V10M(i)
        BR(i,j)=rib(i)
        CHS(I,J)=CH(I)*wspd (i,j)
        CHS2(I,J)=USTAR(I)*KARMAN/FH2(I)


          if (xland(i,j) .lt. 1.9) then
            CHS(I,J)=amin1(CHS(I,J), 0.05)
            CHS2(I,J)=amin1(CHS2(I,J), 0.05)
            if (chs2(i,j) < 0) chs2(i,j)=1.0e-6
          endif



        CPM(I,J)=CP*(1.+0.8*QV3D(i,kts,j))
        esat = fpvs(t1(i))
        QGH(I,J)=ep2*esat/(1000.*ps(i)-esat)
        esat = fpvs(tskin(i))
        qss(i) = ep2*esat/(1000.*ps(i)-esat)
        QSFC(I,J)=qss(i)


        UST(i,j)=ustar(i)




      ENDDO



      DO i=its,ite
        FLHC(i,j)=CPM(I,J)*RHO1(I)*CHS(I,J)
        FLQC(i,j)=RHO1(I)*CHS(I,J)

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




            hfx   (i,j)=       -10.*CP*fxh(i)    
          ELSEIF(XLAND(I,J)-1.5.LT.0.)THEN




            hfx   (i,j)=       -10.*CP*fxh(i)    
            HFX(I,J)=AMAX1(HFX(I,J),-250.)
          ENDIF


          qfx(i,j)=-10.*fxe(i)
          QFX(I,J)=AMAX1(QFX(I,J),0.)
          LH(I,J)=XLV*QFX(I,J)
        ENDDO
      ENDIF




    if(j.eq.jds.or.j.eq.jde) then

    write(message,*) "TSFC in gfdl sf mod,dt, its,ite,jts,jts", dt,its,ite,jts,jte,ids,ide,jds,jde
    call wrf_debug(1,message)
    write(message,*) "TSFC",  (TSK(i,j),i=its,ite)
    call wrf_debug(1,message)
    endif

   ENDDO            































   END SUBROUTINE SF_GFDL


       SUBROUTINE MFLUX2( fxh,fxe,fxmx,fxmy,cdm,rib,xxfh,zoc,mzoc,tstrc,       &    
                          pspc,pkmax,wetc,slwdc,tjloc,                    &
                          icoef_sf,iwavecpl,lcurr_sf,alpha,gamma,xcur,ycur,                &
                          pert_Cd, ens_random_seed, ens_Cdamp,            &
                          upc,vpc,tpc,rpc,dt,jfix,wind10,xxfh2,ntsflg,sfenth,    &
                           tzot, &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          its,ite, jts,jte, kts,kte                       )
  










      USE module_sf_exchcoef
      IMPLICIT NONE
























      integer,intent(in)  :: ids,ide, jds,jde, kds,kde
      integer,intent(in)  :: ims,ime, jms,jme, kms,kme
      integer,intent(in)  :: its,ite, jts,jte, kts,kte
      integer,intent(in)  :: jfix,ntsflg
      integer,intent(in)  :: icoef_sf
      integer,intent(in)  :: iwavecpl
      logical,intent(in)  :: lcurr_sf
      logical,intent(in)  :: pert_Cd 
      integer,intent(in)  :: ens_random_seed
      real,intent(in)     :: ens_Cdamp

      real, intent (out), dimension (ims :ime ) :: fxh
      real, intent (out), dimension (ims :ime ) :: fxe
      real, intent (out), dimension (ims :ime ) :: fxmx
      real, intent (out), dimension (ims :ime ) :: fxmy
      real, intent (inout), dimension (ims :ime ) :: cdm

      real, intent (out), dimension (ims :ime ) :: rib
      real, intent (out), dimension (ims :ime ) :: xxfh
      real, intent (out), dimension (ims :ime ) :: xxfh2
      real, intent (out), dimension (ims :ime ) :: wind10

      real, intent ( inout), dimension (ims :ime ) :: zoc,mzoc    
      real, intent ( inout), dimension (ims :ime ) :: tzot        
      real, intent ( inout), dimension (ims :ime ) :: tstrc

      real, intent ( in)                        :: dt
      real, intent ( in)                        :: sfenth
      real, intent ( in), dimension (ims :ime ) :: pspc
      real, intent ( in), dimension (ims :ime ) :: pkmax
      real, intent ( in), dimension (ims :ime ) :: wetc
      real, intent ( in), dimension (ims :ime ) :: slwdc
      real, intent ( in), dimension (ims :ime ) :: tjloc
      real, intent ( in), dimension (ims :ime ) :: alpha, gamma
      real, intent ( in), dimension (ims :ime ) :: xcur, ycur

      real, intent ( in), dimension (kms:kme, ims :ime ) :: upc
      real, intent ( in), dimension (kms:kme, ims :ime ) :: vpc
      real, intent ( in), dimension (kms:kme, ims :ime ) :: tpc
      real, intent ( in), dimension (kms:kme, ims :ime ) :: rpc





      integer, parameter :: icntx = 30

      integer, dimension(1   :ime) :: ifz
      integer, dimension(1   :ime) :: indx
      integer, dimension(1   :ime) :: istb
      integer, dimension(1   :ime) :: it
      integer, dimension(1   :ime) :: iutb

      real, dimension(1   :ime) :: aap
      real, dimension(1   :ime) :: bq1
      real, dimension(1   :ime) :: bq1p
      real, dimension(1   :ime) :: delsrad
      real, dimension(1   :ime) :: ecof
      real, dimension(1   :ime) :: ecofp
      real, dimension(1   :ime) :: estso
      real, dimension(1   :ime) :: estsop
      real, dimension(1   :ime) :: fmz1
      real, dimension(1   :ime) :: fmz10
      real, dimension(1   :ime) :: fmz2 
      real, dimension(1   :ime) :: fmzo1
      real, dimension(1   :ime) :: foft
      real, dimension(1   :ime) :: foftm
      real, dimension(1   :ime) :: frac
      real, dimension(1   :ime) :: land
      real, dimension(1   :ime) :: pssp
      real, dimension(1   :ime) :: qf
      real, dimension(1   :ime) :: rdiff
      real, dimension(1   :ime) :: rho
      real, dimension(1   :ime) :: rkmaxp
      real, dimension(1   :ime) :: rstso
      real, dimension(1   :ime) :: rstsop
      real, dimension(1   :ime) :: sf10
      real, dimension(1   :ime) :: sf2 
      real, dimension(1   :ime) :: sfm
      real, dimension(1   :ime) :: sfzo
      real, dimension(1   :ime) :: sgzm
      real, dimension(1   :ime) :: slwa
      real, dimension(1   :ime) :: szeta
      real, dimension(1   :ime) :: szetam
      real, dimension(1   :ime) :: t1
      real, dimension(1   :ime) :: t2
      real, dimension(1   :ime) :: tab1
      real, dimension(1   :ime) :: tab2
      real, dimension(1   :ime) :: tempa1
      real, dimension(1   :ime) :: tempa2
      real, dimension(1   :ime) :: theta
      real, dimension(1   :ime) :: thetap
      real, dimension(1   :ime) :: tsg
      real, dimension(1   :ime) :: tsm
      real, dimension(1   :ime) :: tsp
      real, dimension(1   :ime) :: tss
      real, dimension(1   :ime) :: ucom
      real, dimension(1   :ime) :: uf10
      real, dimension(1   :ime) :: uf2 
      real, dimension(1   :ime) :: ufh
      real, dimension(1   :ime) :: ufm
      real, dimension(1   :ime) :: ufzo
      real, dimension(1   :ime) :: ugzm
      real, dimension(1   :ime) :: uzeta
      real, dimension(1   :ime) :: uzetam
      real, dimension(1   :ime) :: vcom
      real, dimension(1   :ime) :: vrtkx
      real, dimension(1   :ime) :: vrts
      real, dimension(1   :ime) :: wind
      real, dimension(1   :ime) :: windp
      real, dimension(1   :ime) :: wind10p  
      real, dimension(1   :ime) :: uvs1

      real, dimension(1   :ime) :: xxfm
      real, dimension(1   :ime) :: xxsh
      real, dimension(1   :ime) :: z10
      real, dimension(1   :ime) :: z2 
      real, dimension(1   :ime) :: zeta
      real, dimension(1   :ime) :: zkmax

      real, dimension(1   :ime) :: pss
      real, dimension(1   :ime) :: tstar
      real, dimension(1   :ime) :: ukmax
      real, dimension(1   :ime) :: vkmax
      real, dimension(1   :ime) :: tkmax
      real, dimension(1   :ime) :: rkmax
      real, dimension(1   :ime) :: zot
      real, dimension(1   :ime) :: fhzo1
      real, dimension(1   :ime) :: sfh

      real :: ux13, yo, y,xo,x,ux21,ugzzo,ux11,ux12,uzetao,xnum,alll
      real :: ux1,ugz,x10,uzo,uq,ux2,ux3,xtan,xden,y10,uzet1o,ugz10
      real :: szet2, zal2,ugz2 
      real :: rovcp,boycon,cmo2,psps1,zog,enrca,rca,cmo1,amask,en,ca,a,c
      real :: sgz,zal10,szet10,fmz,szo,sq,fmzo,rzeta1,zal1g,szetao,rzeta2,zal2g
      real :: hcap,xks,pith,teps,diffot,delten,alevp,psps2,alfus,nstep
      real :: shfx,sigt4,reflect
      real :: cor1,cor2,szetho,zal2gh,cons_p000001,cons_7,vis,ustar,restar,rat
      real :: wndm,ckg
      real :: windmks,znott,znotm
      real :: ubot, vbot
      integer:: i,j,ii,iq,nnest,icnt,ngd,ip




 
      real, dimension (223) :: tab 
      real, dimension (223) :: table
      real, dimension (101) :: tab11
      real, dimension (41) :: table4
      real, dimension (42) :: tab3
      real, dimension (54) :: table2
      real, dimension (54) :: table3
      real, dimension (74) :: table1
      real, dimension (80) :: tab22

      character(len=255) :: message

      equivalence (tab(1),tab11(1))
      equivalence (tab(102),tab22(1))
      equivalence (tab(182),tab3(1))
      equivalence (table(1),table1(1))
      equivalence (table(75),table2(1))
      equivalence (table(129),table3(1))
      equivalence (table(183),table4(1))

      data amask/ -98.0/





      data tab11/21*0.01403,0.01719,0.02101,0.02561,0.03117,0.03784,      &
     &.04584,.05542,.06685,.08049,.09672,.1160,.1388,.1658,.1977,.2353,   &
     &.2796,.3316,.3925,.4638,.5472,.6444,.7577,.8894,1.042,1.220,1.425,  &
     &1.662,1.936,2.252,2.615,3.032,3.511,4.060,4.688,5.406,6.225,7.159,  &
     &8.223,9.432,10.80,12.36,14.13,16.12,18.38,20.92,23.80,27.03,30.67,  &
     &34.76,39.35,44.49,50.26,56.71,63.93,71.98,80.97,90.98,102.1,114.5,  &
     &128.3,143.6,160.6,179.4,200.2,223.3,248.8,276.9,307.9,342.1,379.8,  &
     &421.3,466.9,517.0,572.0,632.3,698.5,770.9,850.2,937.0,1032./

      data tab22/1146.6,1272.0,1408.1,1556.7,1716.9,1890.3,2077.6,2279.6  &
     &,2496.7,2729.8,2980.0,3247.8,3534.1,3839.8,4164.8,4510.5,4876.9,    &
     &5265.1,5675.2,6107.8,6566.2,7054.7,7575.3,8129.4,8719.2,9346.5,     &
     &10013.,10722.,11474.,12272.,13119.,14017.,14969.,15977.,17044.,     &
     &18173.,19367.,20630.,21964.,23373.,24861.,26430.,28086.,29831.,     &
     &31671.,33608.,35649.,37796.,40055.,42430.,44927.,47551.,50307.,     &
     &53200.,56236.,59422.,62762.,66264.,69934.,73777.,77802.,82015.,     &
     &86423.,91034.,95855.,100890.,106160.,111660.,117400.,123400.,       &
     &129650.,136170.,142980.,150070.,157460.,165160.,173180.,181530.,    &
     &190220.,199260./

      data tab3/208670.,218450.,228610.,239180.,250160.,261560.,273400.,  &
     &285700.,298450.,311690.,325420.,339650.,354410.,369710.,385560.,    &
     &401980.,418980.,436590.,454810.,473670.,493170.,513350.,534220.,    &
     &555800.,578090.,601130.,624940.,649530.,674920.,701130.,728190.,    &
     &756110.,784920.,814630.,845280.,876880.,909450.,943020.,977610.,    &
     &1013250.,1049940.,1087740./

      data table1/20*0.0,.3160e-02,.3820e-02,.4600e-02,.5560e-02,.6670e-02, &
     & .8000e-02,.9580e-02,.1143e-01,.1364e-01,.1623e-01,.1928e-01,       &
     &.2280e-01,.2700e-01,.3190e-01,.3760e-01,.4430e-01,.5200e-01,          &
     &.6090e-01,.7130e-01,.8340e-01,.9720e-01,.1133e+00,.1317e-00,          &
     &.1526e-00,.1780e-00,.2050e-00,.2370e-00,.2740e-00,.3160e-00,          &
     &.3630e-00,.4170e-00,.4790e-00,.5490e-00,.6280e-00,.7180e-00,          &
     &.8190e-00,.9340e-00,.1064e+01,.1209e+01,.1368e+01,.1560e+01,          &
     &.1770e+01,.1990e+01,.2260e+01,.2540e+01,.2880e+01,.3230e+01,          &
     &.3640e+01,.4090e+01,.4590e+01,.5140e+01,.5770e+01,.6450e+01,          &
     &.7220e+01/

      data table2/.8050e+01,.8990e+01,.1001e+02,.1112e+02,.1240e+02,      &
     &.1380e+02,.1530e+02,.1700e+02,.1880e+02,.2080e+02,.2310e+02,        &
     &.2550e+02,.2810e+02,.3100e+02,.3420e+02,.3770e+02,.4150e+02,        &
     &.4560e+02,.5010e+02,.5500e+02,.6030e+02,.6620e+02,.7240e+02,        &
     &.7930e+02,.8680e+02,.9500e+02,.1146e+03,.1254e+03,.1361e+03,        &
     &.1486e+03,.1602e+03,.1734e+03,.1873e+03,.2020e+03,.2171e+03,        &
     &.2331e+03,.2502e+03,.2678e+03,.2863e+03,.3057e+03,.3250e+03,        &
     &.3457e+03,.3664e+03,.3882e+03,.4101e+03,.4326e+03,.4584e+03,        &
     &.4885e+03,.5206e+03,.5541e+03,.5898e+03,.6273e+03,.6665e+03,        &
     &.7090e+03/

      data table3/.7520e+03,.7980e+03,.8470e+03,.8980e+03,.9520e+03,      &
     &.1008e+04,.1067e+04,.1129e+04,.1194e+04,.1263e+04,.1334e+04,        &
     &.1409e+04,.1488e+04,.1569e+04,.1656e+04,.1745e+04,.1840e+04,        &
     &.1937e+04,.2041e+04,.2147e+04,.2259e+04,.2375e+04,.2497e+04,        & 
     &.2624e+04,.2756e+04,.2893e+04,.3036e+04,.3186e+04,.3340e+04,        &
     &.3502e+04,.3670e+04,.3843e+04,.4025e+04,.4213e+04,.4408e+04,        &
     &.4611e+04,.4821e+04,.5035e+04,.5270e+04,.5500e+04,.5740e+04,        &
     &.6000e+04,.6250e+04,.6520e+04,.6810e+04,.7090e+04,.7390e+04,        &
     &.7700e+04,.8020e+04,.8350e+04,.8690e+04,.9040e+04,.9410e+04,        &
     &.9780e+04/

      data table4/.1016e+05,.1057e+05,.1098e+05,.1140e+05,.1184e+05,      &
     &.1230e+05,.1275e+05,.1324e+05,.1373e+05,.1423e+05,.1476e+05,        &
     &.1530e+05,.1585e+05,.1642e+05,.1700e+05,.1761e+05,.1822e+05,        &
     &.1886e+05,.1950e+05,.2018e+05,.2087e+05,.2158e+05,.2229e+05,        &
     &.2304e+05,.2381e+05,.2459e+05,.2539e+05,.2621e+05,.2706e+05,        &
     &.2792e+05,.2881e+05,.2971e+05,.3065e+05,.3160e+05,.3257e+05,        &
     &.3357e+05,.3459e+05,.3564e+05,.3669e+05,.3780e+05,.0000e+00/



      real,parameter :: cp    = 1.00464e7
      real,parameter :: g     = 980.6
      real,parameter :: rgas  = 2.87e6
      real,parameter :: og    = 1./g
      integer :: ntstep = 0









      j = IFIX(tjloc(its))


       cor1 = .120
       cor2 = 720.


        cor1 = 0.
        cor2 = 0.


      do i = its,ite
        z10(i) = 1000.
        z2 (i) =  200.
        pss(i) = pspc(i)
        tstar(i) = tstrc(i)

        if ( lcurr_sf .and. zoc(i) .le. 0.0 ) then
          ubot = upc(1,i)  - xcur(i) * 100.0
          vbot = vpc(1,i)  - ycur(i) * 100.0


        else
          ubot = upc(1,i)
          vbot = vpc(1,i)
        endif
        uvs1(i)= amax1( SQRT(ubot*ubot +    &
                             vbot*vbot), 100.0)
        if ( iwavecpl .eq. 1 .and. zoc(i) .le. 0.0 ) then
          ukmax(i) = ( ubot * cos(gamma(i))  -          &
                      vbot * sin(gamma(i)) )            &
                                  * cos(gamma(i))
          vkmax(i) = ( vbot * cos(gamma(i))  -          &
                      ubot * sin(gamma(i)) )            &
                                  * cos(gamma(i))

        else
          ukmax(i) = ubot
          vkmax(i) = vbot
        endif



        tkmax(i) = tpc(1,i)
        rkmax(i) = rpc(1,i)
      enddo





      do i = its,ite
        windp(i) = SQRT(ukmax(i)*ukmax(i) + vkmax(i)*vkmax(i))
        wind (i) = amax1(windp(i),100.)


         wind10p(i) = wind10(i)  
        wind10p(i) = amax1(wind10p(i),100.)



        if (zoc(i) .LT. amask) zoc(i) = -0.0185*0.001*wind10p(i)*wind10p(i)*og
        if (zoc(i) .GT. 0.0) then
          ecof(i) = wetc(i)
          land(i) = 1.0
          zot (i) = zoc(i)
        else
          ecof(i) = wetc(i)
          land(i) = 0.0
          
          windmks=wind10p(i)*.01
          if ( iwavecpl .eq. 1 ) then
            call znot_wind10m(windmks,znott,znotm,icoef_sf)
            
            if ( alpha(i) .ge. 0.2 .and. alpha(i) .le. 5. ) then
              znotm = znotm*alpha(i)
            endif
            zoc(i)  = -100.*znotm
            zot(i) =  -100* znott
          else










            call znot_wind10m(windmks,znott,znotm,icoef_sf)
            zoc(i)  = -100.*znotm
            zot(i) =  -100* znott
          endif
        endif











      mzoc(i) = zoc(i)                
      tzot(i) = zot(i)                 
      enddo












      en     = 2.
      c      = .76
      a      = 5.
      ca     = .4
      cmo1   = .5*a - 1.648
      cmo2   = 17.193 + .5*a - 10.*c
      boycon = .61
        rovcp=rgas/cp






      do i = its,ite

        theta(i) = tkmax(i)/((pkmax(i)/pspc(i))**rovcp)
        vrtkx(i) = 1.0 + boycon*rkmax(i)

        zkmax(i) = -rgas*tkmax(i)*alog(pkmax(i)/pspc(i))*og

      enddo








      do i = its,ite
        tsg  (i) = tstar(i)
        tab1 (i) = tstar(i) - 153.16
        it   (i) = IFIX(tab1(i))
        tab2 (i) = tab1(i) - FLOAT(it(i))
        t1   (i) = tab(min(223,max(1,it(i) + 1)))
        t2   (i) = table(min(223,max(1,it(i) + 1)))
        estso(i) = t1(i) + tab2(i)*t2(i)
         psps1 = (pss(i) - estso(i))
          if(psps1 .EQ. 0.0)then
           psps1 = .1
          endif
        rstso(i) = 0.622*estso(i)/psps1                       
        vrts (i) = 1. + boycon*ecof(i)*rstso(i)
      enddo








      do i = its,ite
        tempa1(i) = theta(i)*vrtkx(i) - tstar(i)*vrts(i)
        tempa2(i) = tempa1(i)*(theta(i) - tstar(i))
        if (tempa2(i) .LT. 0.) tempa1(i) = 1.0e-4
        tab1(i) = ABS(tempa1(i))
        if (tab1(i) .LT. 1.0e-4) tempa1(i) = 1.0e-4





        rib (i) = g*zkmax(i)*tempa1(i)/                             &
                                    (tkmax(i)*vrtkx(i)*wind(i)*wind(i))
        tab2(i) = ABS(zoc(i))
        tab1(i) = 0.95/(c*(1. - tab2(i)/zkmax(i)))
        if (rib(i) .GT. tab1(i)) rib(i) = tab1(i)
      enddo

      do i = its,ite
        zeta(i) = ca*rib(i)/0.03
      enddo













      rca   = 1./ca
      enrca = en*rca

      enrca = 0.0
      zog   = .0185*og





      ip    = 0
      do i = its,ite
        if (zeta(i) .GE. 0.0) then
          ip = ip + 1
          istb(ip) = i
        endif
      enddo

      if (ip .EQ. 0) go to 170
      do i = 1,ip
        szetam(i) = 1.0e+30
        sgzm(i)   = 0.0e+00
        szeta(i)  = zeta(istb(i))
        ifz(i)    = 1
      enddo






      do icnt = 1,icntx
        do i = 1,ip
          if (ifz(i) .EQ. 0) go to 80
          zal1g = ALOG(szeta(i))
          if (szeta(i) .LE. 0.5) then
            fmz1(i) = (zal1g + a*szeta(i))*rca
          else if (szeta(i) .GT. 0.5 .AND. szeta(i) .LE. 10.) then
            rzeta1  = 1./szeta(i)
            fmz1(i) = (8.*zal1g + 4.25*rzeta1 - &
                                          0.5*rzeta1*rzeta1 + cmo1)*rca
          else if (szeta(i) .GT. 10.) then
            fmz1(i) = (c*szeta(i) + cmo2)*rca
          endif
          szetao = ABS(zoc(istb(i)))/zkmax(istb(i))*szeta(i)
          zal2g  = ALOG(szetao)
          if (szetao .LE. 0.5) then
            fmzo1(i) = (zal2g + a*szetao)*rca
            sfzo (i) = 1. + a*szetao
          else if (szetao .GT. 0.5 .AND. szetao .LE. 10.) then
            rzeta2   = 1./szetao
            fmzo1(i) = (8.*zal2g + 4.25*rzeta2 - &
                                          0.5*rzeta2*rzeta2 + cmo1)*rca
            sfzo (i) = 8.0 - 4.25*rzeta2 + rzeta2*rzeta2
          else if (szetao .GT. 10.) then
            fmzo1(i) = (c*szetao + cmo2)*rca
            sfzo (i) = c*szetao
          endif




          szetho = ABS(zot(istb(i)))/zkmax(istb(i))*szeta(i)
          zal2gh = ALOG(szetho)
          if (szetho .LE. 0.5) then
            fhzo1(i) = (zal2gh + a*szetho)*rca
            sfzo (i) = 1. + a*szetho
          else if (szetho .GT. 0.5 .AND. szetho .LE. 10.) then
            rzeta2   = 1./szetho
            fhzo1(i) = (8.*zal2gh + 4.25*rzeta2 -   &
                                          0.5*rzeta2*rzeta2 + cmo1)*rca
            sfzo (i) = 8.0 - 4.25*rzeta2 + rzeta2*rzeta2
          else if (szetho .GT. 10.) then
            fhzo1(i) = (c*szetho + cmo2)*rca
            sfzo (i) = c*szetho
          endif
 





            szet10 = ABS(z10(istb(i)))/zkmax(istb(i))*szeta(i)
            zal10  = ALOG(szet10)
            if (szet10 .LE. 0.5) then
              fmz10(i) = (zal10 + a*szet10)*rca
            else if (szet10 .GT. 0.5 .AND. szet10 .LE. 10.) then
              rzeta2   = 1./szet10
              fmz10(i) = (8.*zal10 + 4.25*rzeta2 - &
                                          0.5*rzeta2*rzeta2 + cmo1)*rca
            else if (szet10 .GT. 10.) then
              fmz10(i) = (c*szet10 + cmo2)*rca
            endif
            sf10(i) = fmz10(i) - fmzo1(i)

            szet2  = ABS(z2 (istb(i)))/zkmax(istb(i))*szeta(i)
            zal2   = ALOG(szet2 )
            if (szet2  .LE. 0.5) then
              fmz2 (i) = (zal2  + a*szet2 )*rca
            else if (szet2  .GT. 0.5 .AND. szet2  .LE. 2.) then
              rzeta2   = 1./szet2 
              fmz2 (i) = (8.*zal2  + 4.25*rzeta2 - &
                                          0.5*rzeta2*rzeta2 + cmo1)*rca
            else if (szet2  .GT. 2.) then
              fmz2 (i) = (c*szet2  + cmo2)*rca
            endif
            sf2 (i) = fmz2 (i) - fmzo1(i)
  

          sfm(i) = fmz1(i) - fmzo1(i)
          sfh(i) = fmz1(i) - fhzo1(i)
          sgz    = ca*rib(istb(i))*sfm(i)*sfm(i)/ &
                                               (sfh(i) + enrca*sfzo(i))
          fmz    = (sgz - szeta(i))/szeta(i)
          fmzo   = ABS(fmz)
          if (fmzo .GE. 5.0e-5) then
            sq        = (sgz - sgzm(i))/(szeta(i) - szetam(i))
            if(sq .EQ. 1) then
             write(message,*)'NCO ERROR DIVIDE BY ZERO IN MFLUX2 (STABLE CASE)'
             call wrf_message(message)
             write(message,*)'sq is 1 ',fmzo,sgz,sgzm(i),szeta(i),szetam(i)
             call wrf_message(message)
            endif
            szetam(i) = szeta(i)
            szeta (i) = (sgz - szeta(i)*sq)/(1.0 - sq)
            sgzm  (i) = sgz
          else
            ifz(i) = 0
          endif
80        continue
        enddo
      enddo

      do i = 1,ip
        if (ifz(i) .GE. 1) go to 110
      enddo

      go to 130

110   continue

      write(6,120)
120   format(2X, ' NON-CONVERGENCE FOR STABLE ZETA IN ROW ')








130   continue
      do i = 1,ip
        szo = zoc(istb(i))
        if (szo .LT. 0.0)  then
        wndm=wind(istb(i))*0.01
          if(wndm.lt.15.0) then
          ckg=0.0185*og
          else


           ckg=(sfenth*(4*0.000308*wndm) + (1.-sfenth)*0.0185 )*og
          endif

       szo =  - ckg*wind(istb(i))*wind(istb(i))/  &
                             (sfm(i)*sfm(i))
        cons_p000001    =    .000001
        cons_7          =         7.
        vis             =     1.4E-1
 
        ustar    = sqrt( -szo / zog)
        restar = -ustar * szo    / vis
        restar = max(restar,cons_p000001) 

        rat    = 2.67 * restar ** .25 - 2.57
        rat    = min(rat   ,cons_7)                      
	rat=0.
        zot(istb(i)) = szo   * exp(-rat)
         else
         zot(istb(i)) = zoc(istb(i))
        endif
        

             zoc(istb(i)) = szo
      enddo

      do i = 1,ip
        xxfm(istb(i)) = sfm(i)
        xxfh(istb(i)) = sfh(i)
        xxfh2(istb(i)) = sf2 (i)
        xxsh(istb(i)) = sfzo(i)
      enddo






        do i = 1,ip
         wind10(istb(i)) = sf10(i)*uvs1(istb(i))/sfm(i)
         wind10(istb(i)) = wind10(istb(i)) * 1.944
           if(wind10(istb(i)) .GT. 6000.0) then
       wind10(istb(i))=wind10(istb(i))+wind10(istb(i))*cor1 &
                        - cor2
           endif

         wind10(istb(i)) = wind10(istb(i)) / 1.944
        enddo   
















170   continue

      iq = 0
      do i = its,ite
        if (zeta(i) .LT. 0.0) then
          iq       = iq + 1
          iutb(iq) = i
        endif
      enddo

      if (iq .EQ. 0) go to 290
      do i = 1,iq
        uzeta (i) = zeta(iutb(i))
        ifz   (i) = 1
        uzetam(i) = 1.0e+30
        ugzm  (i) = 0.0e+00
      enddo






      do icnt = 1,icntx
        do i = 1,iq
          if (ifz(i) .EQ. 0) go to 200
          ugzzo   = ALOG(zkmax(iutb(i))/ABS(zot(iutb(i))))
          uzetao  = ABS(zot(iutb(i)))/zkmax(iutb(i))*uzeta(i)
          ux11    = 1. - 16.*uzeta(i)
          ux12    = 1. - 16.*uzetao
          y       = SQRT(ux11)
          yo      = SQRT(ux12)
          ufzo(i) = 1./yo
          ux13    = (1. + y)/(1. + yo)
          ux21    = ALOG(ux13)
          ufh(i)  = (ugzzo - 2.*ux21)*rca

          ugzzo   = ALOG(zkmax(iutb(i))/ABS(zoc(iutb(i))))
          uzetao  = ABS(zoc(iutb(i)))/zkmax(iutb(i))*uzeta(i)
          ux11    = 1. - 16.*uzeta(i)
          ux12    = 1. - 16.*uzetao
          y       = SQRT(ux11)
          yo      = SQRT(ux12)
          ux13    = (1. + y)/(1. + yo)
          ux21    = ALOG(ux13)

          x       = SQRT(y)
          xo      = SQRT(yo)
          xnum    = (x**2 + 1.)*((x + 1.)**2)
          xden    = (xo**2 + 1.)*((xo + 1.)**2)
          xtan    = ATAN(x) - ATAN(xo)
          ux3     = ALOG(xnum/xden)
          ufm(i)  = (ugzzo - ux3 + 2.*xtan)*rca






            ugz10   = ALOG(z10(iutb(i))/ABS(zoc(iutb(i))))
            uzet1o  = ABS(z10(iutb(i)))/zkmax(iutb(i))*uzeta(i)
            uzetao  = ABS(zoc(iutb(i)))/zkmax(iutb(i))*uzeta(i)
            ux11    = 1. - 16.*uzet1o
            ux12    = 1. - 16.*uzetao
            y       = SQRT(ux11)
            y10     = SQRT(ux12)
            ux13    = (1. + y)/(1. + y10)
            ux21    = ALOG(ux13)
            x       = SQRT(y)
            x10     = SQRT(y10)
            xnum    = (x**2 + 1.)*((x + 1.)**2)
            xden    = (x10**2 + 1.)*((x10 + 1.)**2)
            xtan    = ATAN(x) - ATAN(x10)
            ux3     = ALOG(xnum/xden)
            uf10(i) = (ugz10 - ux3 + 2.*xtan)*rca




          ugz2    = ALOG(z2   (iutb(i))/ABS(zoc(iutb(i))))
          uzet1o  = ABS(z2 (iutb(i)))/zkmax(iutb(i))*uzeta(i)
          uzetao  = ABS(zoc(iutb(i)))/zkmax(iutb(i))*uzeta(i)
          ux11    = 1. - 16.*uzet1o   
          ux12    = 1. - 16.*uzetao
          y       = SQRT(ux11)
          yo      = SQRT(ux12)
          ux13    = (1. + y)/(1. + yo)
          ux21    = ALOG(ux13)
          uf2 (i)  = (ugzzo - 2.*ux21)*rca



          ugz = ca*rib(iutb(i))*ufm(i)*ufm(i)/(ufh(i) + enrca*ufzo(i))
          ux1 = (ugz - uzeta(i))/uzeta(i)
          ux2 = ABS(ux1)
          if (ux2 .GE. 5.0e-5) then
            uq        = (ugz - ugzm(i))/(uzeta(i) - uzetam(i))
            uzetam(i) = uzeta(i)
            if(uq .EQ. 1) then
             call wrf_message('NCO ERROR DIVIDE BY ZERO IN MFLUX2 (UNSTABLE CASE)')
             write(message,*)'uq is 1 ',ux2,ugz,ugzm(i),uzeta(i),uzetam(i) 
             call wrf_message(message)
            endif
            uzeta (i) = (ugz - uzeta(i)*uq)/(1.0 - uq)
            ugzm  (i) = ugz
          else
            ifz(i) = 0
          endif
200       continue
        enddo
      enddo


      do i = 1,iq
        if (ifz(i) .GE. 1) go to 230
      enddo

      go to 250

230   continue
      write(message,240)
      call wrf_message(message)
240   format(2X, ' NON-CONVERGENCE FOR UNSTABLE ZETA IN ROW ')






250   continue







      do i = 1,iq
        uzo = zoc(iutb(i))
        if (zoc(iutb(i)) .LT. 0.0)   then
        wndm=wind(iutb(i))*0.01
         if(wndm.lt.15.0) then
         ckg=0.0185*og
         else


            ckg=(4*0.000308*wndm)*og
            ckg=(sfenth*(4*0.000308*wndm) + (1.-sfenth)*0.0185 )*og
         endif
        uzo         =-ckg*wind(iutb(i))*wind(iutb(i))/(ufm(i)*ufm(i))
        cons_p000001    =    .000001
        cons_7          =         7.
        vis             =     1.4E-1

        ustar    = sqrt( -uzo / zog)
        restar = -ustar * uzo    / vis
        restar = max(restar,cons_p000001)

        rat    = 2.67 * restar ** .25 - 2.57
        rat    = min(rat   ,cons_7)                      
	rat=0.0
        zot(iutb(i)) =  uzo   * exp(-rat)
         else
         zot(iutb(i)) = zoc(iutb(i))
        endif

            zoc(iutb(i)) = uzo
      enddo






        do i = 1,iq
          wind10(iutb(i)) = uf10(i)*uvs1(iutb(i))/ufm(i)
          wind10(iutb(i)) = wind10(iutb(i)) * 1.944
           if(wind10(iutb(i)) .GT. 6000.0) then
         wind10(iutb(i))=wind10(iutb(i))+wind10(iutb(i))*cor1 &
                         - cor2
           endif
 
          wind10(iutb(i)) = wind10(iutb(i)) / 1.944
        enddo   












      do i = 1,iq
        xxfm(iutb(i)) = ufm(i)
        xxfh(iutb(i)) = ufh(i)
        xxfh2(iutb(i)) = uf2 (i)
        xxsh(iutb(i)) = ufzo(i)
      enddo

290   continue

      do i = its,ite
        ucom(i) = ukmax(i)
        vcom(i) = vkmax(i)
        if (windp(i) .EQ. 0.0) then
          windp(i) = 100.0
          ucom (i) = 100.0/SQRT(2.0)
          vcom (i) = 100.0/SQRT(2.0)
        endif
        rho(i) = pss(i)/(rgas*(tsg(i) + enrca*(theta(i) - &
                tsg(i))*xxsh(i)/(xxfh(i) + enrca*xxsh(i))))
        bq1(i) = wind(i)*rho(i)/(xxfm(i)*(xxfh(i) + enrca*xxsh(i)))
      enddo




      if (ntsflg .EQ. 0) go to 370
      alll = 600.
      xks   = 0.01
      hcap  = .5/2.39e-8
      pith  = SQRT(4.*ATAN(1.0))
      alfus = alll/2.39e-8
      teps  = 0.1







      ip    = 0
      do i = its,ite
        if (land(i) .EQ. 1) then
          ip = ip + 1
          indx   (ip) = i

          slwa   (ip) =    slwdc(i)/(2.39e-8*60.)
          tss    (ip) = tstar(i)
          thetap (ip) = theta(i)
          rkmaxp (ip) = rkmax(i)
          aap    (ip) = 5.673e-5
          pssp   (ip) = pss(i)
          ecofp  (ip) = ecof(i)
          estsop (ip) = estso(i)
          rstsop (ip) = rstso(i)
          bq1p   (ip) = bq1(i)
          bq1p   (ip) = amax1(bq1p(ip),0.1e-3)
          delsrad(ip) = dt   *pith/(hcap*SQRT(3600.*24.*xks))
        endif
      enddo





      do i = 1,ip
        ifz  (i) = 1
        tsm  (i) = tss(i)
        rdiff(i) = amin1(0.0,(rkmaxp(i) - rstsop(i)))




300   format(2X, ' SURFACE EQUILIBRIUM CALCULATION ')





          foftm(i) = tss(i) + delsrad(i)*(slwa(i) - aap(i)*tsm(i)**4 - &
           cp*bq1p(i)*(tsm(i) - thetap(i)) + ecofp(i)*alfus*bq1p(i)* &
           rdiff(i))

        tsp(i) = foftm(i)
      enddo





      do icnt = 1,icntx
        do i = 1,ip
          if (ifz(i) .EQ. 0) go to 330
          tab1  (i) = tsp(i) - 153.16
          it    (i) = IFIX(tab1(i))
          tab2  (i) = tab1(i) - FLOAT(it(i))
          t1    (i) = tab(min(223,max(1,it(i) + 1)))
          t2    (i) = table(min(223,max(1,it(i) + 1)))
          estsop(i) = t1(i) + tab2(i)*t2(i)
             psps2 = (pssp(i) - estsop(i))
             if(psps2 .EQ. 0.0)then
               psps2 = .1
             endif
          rstsop(i) = 0.622*estsop(i)/psps2                  
          rdiff (i) = amin1(0.0,(rkmaxp(i) - rstsop(i)))





            foft(i) = tss(i) + delsrad(i)*(slwa(i) - aap(i)*tsp(i)**4 - &
             cp*bq1p(i)*(tsp(i) - thetap(i)) + ecofp(i)*alfus*bq1p(i)* &
             rdiff(i))









          frac(i) = ABS((foft(i) - tsp(i))/tsp(i))




 
          if (frac(i) .GE. teps) then
            qf   (i) = (foft(i) - foftm(i))/(tsp(i) - tsm(i))
            tsm  (i) = tsp(i)
            tsp  (i) = (foft(i) - tsp(i)*qf(i))/(1. - qf(i))
            foftm(i) = foft(i)
          else
            ifz(i) = 0
          endif
330       continue
        enddo
      enddo





      do i = 1,ip
        if (ifz(i) .EQ. 1) then
        write(message, 340) tsp(i), i, j
340   format(2X, ' NON-CONVERGENCE OF T* PREDICTED (T*,I,J) = ', E14.8, &
            2I5)
        call wrf_message(message)

        write(message,345) indx(i), j, tstar(indx(i)), tsp(i), ip
345   format(2X, ' I, J, OLD T*, NEW T*, NPTS ', 2I5, 2E14.8, I5)
        call wrf_message(message)


350   format(2X, ' REFLECT, SIGT4, SHFX, ALEVP, DELTEN, DIFFOT ', &
            6E14.8)


        endif
      enddo

      do i = 1,ip
        ii        = indx(i)
        tstrc(ii) = tsp (i)
      enddo





370   continue
      do i = its,ite

        if ( iwavecpl .eq. 1 .and. zoc(i) .le. 0.0 ) then
          windmks = wind10(i) * 0.01
          call znot_wind10m(windmks,znott,znotm,icoef_sf)
          
          if ( alpha(i) .ge. 0.2 .and. alpha(i) .le. 5. ) then
            znotm = znotm*alpha(i)
          endif
          zoc(i)  = -100.*znotm
          zot(i) =  -100* znott
        endif

        fxh(i) = bq1(i)*(theta(i) - tsg(i))
        fxe(i) = ecof(i)*bq1(i)*(rkmax(i) - rstso(i))
        if (fxe(i) .GT. 0.0) fxe(i) = 0.0
        fxmx(i) = rho(i)/(xxfm(i)*xxfm(i))*wind(i)*wind(i)*ucom(i)/ &
                 windp(i)
        fxmy(i) = rho(i)/(xxfm(i)*xxfm(i))*wind(i)*wind(i)*vcom(i)/ &
        windp(i)
        cdm(i) = 1./(xxfm(i)*xxfm(i))



      enddo
      ntstep = ntstep + 1
      return
      end subroutine MFLUX2

  SUBROUTINE hwrfsfcinit(isn,XICE,VEGFRA,SNOW,SNOWC,CANWAT,SMSTAV,       &
                        SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,       &
                        ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,DZS,SFCEVP,     & 
                        TMN,                                            &
                        num_soil_layers,                                &
                        allowed_to_read,                                &
                        ids,ide, jds,jde, kds,kde,                      &
                        ims,ime, jms,jme, kms,kme,                      &
                        its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE 


   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN)       ::     num_soil_layers

   REAL,    DIMENSION( num_soil_layers), INTENT(IN) :: DZS

   REAL,    DIMENSION( ims:ime, num_soil_layers, jms:jme )    , &
            INTENT(INOUT)    ::                          SMOIS, & 
                                                         TSLB      

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



  INTEGER, INTENT(IN) :: isn
  LOGICAL, INTENT(IN) :: allowed_to_read

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

       SNOWC(i,j)=0.







    ENDDO
   ENDDO

  END SUBROUTINE hwrfsfcinit

END MODULE module_sf_gfdl
