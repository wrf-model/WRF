!WRF:MODEL_LAYER:PHYSICS
!

MODULE module_cu_gf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This convective parameterization is build to attempt     !
!     a smooth transition to cloud resolving scales as proposed!
!     by Arakawa et al (2011, ACP). It currently does not use  !
!     subsidencespreading as in G3. Difference and details     !
!     will be described in a forthcoming paper by              !
!     Grell and Freitas (2013). The parameterization also      !
!     offers options to couple with aerosols. Both, the smooth !
!     transition part as well as the aerosol coupling are      !
!     experimental. While the smooth transition part is turned !
!     on, nd has been tested dow to a resolution of about 3km  !
!     the aerosol coupling is turned off.                      !
!     More clean-up as well as a direct coupling to chemistry  !
!     will follow for V3.5.1                                   !
!                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CONTAINS

!-------------------------------------------------------------
   SUBROUTINE GFDRV(                                            &
               DT,itimestep,DX                                  &
              ,rho,RAINCV,PRATEC                                &
              ,U,V,t,W,q,p,pi                                   &
              ,dz8w,p8w,XLV,CP,G,r_v                            &
              ,htop,hbot,ktop_deep                              &
              ,CU_ACT_FLAG,warm_rain                            &
              ,APR_GR,APR_W,APR_MC,APR_ST,APR_AS                &
              ,APR_CAPMA,APR_CAPME,APR_CAPMI                    &
              ,MASS_FLUX,HT,hfx,qfx,XLAND,gsw,edt_out     &
              ,GDC,GDC2 ,kpbl,k22_shallow,kbcon_shallow         &
              ,ktop_shallow,xmb_shallow                         &
              ,cugd_tten,cugd_qvten ,cugd_qcten                 &
              ,cugd_ttens,cugd_qvtens,cugd_avedx,imomentum      &
              ,ichoice    &
              ,ishallow_g3,ids,ide, jds,jde, kds,kde            &
              ,ims,ime, jms,jme, kms,kme                        &
              ,its,ite, jts,jte, kts,kte                        &
              ,periodic_x,periodic_y                            &
              ,RQVCUTEN,RQCCUTEN,RQICUTEN                       &
              ,RQVFTEN,RTHFTEN,RTHCUTEN,RTHRATEN                &
              ,rqvblten,rthblten                                &
              ,F_QV    ,F_QC    ,F_QR    ,F_QI    ,F_QS         &
#if ( WRF_DFI_RADAR == 1 )
                 ! Optional CAP suppress option
              ,do_capsuppress,cap_suppress_loc                  &
#endif                                 
                                                                )
!-------------------------------------------------------------
   IMPLICIT NONE
! autoconv, 1=old c0, 2=berry c0
      integer, parameter :: autoconv=1
!aeroevap, 1=old,2=?, 3=average
      integer, parameter :: aeroevap=1
      integer, parameter :: training=0
      integer, parameter :: use_excess=0
      integer, parameter :: use_excess_sh=0
      integer, parameter :: maxiens=1
      integer, parameter :: maxens=1
      integer, parameter :: maxens2=1
      integer, parameter :: maxens3=16
      integer, parameter :: ensdim=16
      real, parameter :: ccnclean=250.
      real, parameter :: aodccn=0.1
      real, parameter :: beta=0.02
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) ::                               &
                                  ids,ide, jds,jde, kds,kde,    & 
                                  ims,ime, jms,jme, kms,kme,    & 
                                  its,ite, jts,jte, kts,kte
   LOGICAL periodic_x,periodic_y
               integer, parameter  :: ens4_spread = 3 ! max(3,cugd_avedx)
               integer, parameter  :: ens4=ens4_spread*ens4_spread

   integer, intent (in   )              :: ichoice
  
   INTEGER,      INTENT(IN   ) :: ITIMESTEP,cugd_avedx, &
                                  ishallow_g3,imomentum
   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV, R_v
   REAL,         INTENT(IN   ) :: CP,G

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                          U,    &
                                                          V,    &
                                                          W,    &
                                                         pi,    &
                                                          t,    &
                                                          q,    &
                                                          p,    &
                                                       dz8w,    &
                                                       p8w,    &
                                                        rho
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          OPTIONAL                                         ,    &
          INTENT(INOUT   ) ::                                   &
               GDC,GDC2

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(IN) :: hfx,qfx,GSW,HT,XLAND
   INTEGER, DIMENSION( ims:ime , jms:jme ),INTENT(IN) :: KPBL
   INTEGER, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT) :: k22_shallow, &
                 kbcon_shallow,ktop_shallow
!
   REAL, INTENT(IN   ) :: DT, DX
!

   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(INOUT) ::           pratec,RAINCV, MASS_FLUX,   &
                          APR_GR,APR_W,APR_MC,APR_ST,APR_AS,    &
                         edt_out,APR_CAPMA,APR_CAPME,APR_CAPMI, &
                         htop,hbot,xmb_shallow
!+lxz
!  REAL, DIMENSION( ims:ime , jms:jme ) :: & !, INTENT(INOUT) ::       &
!        HTOP,     &! highest model layer penetrated by cumulus since last reset in radiation_driver
!        HBOT       ! lowest  model layer penetrated by cumulus since last reset in radiation_driver
!                   ! HBOT>HTOP follow physics leveling convention

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                     &
         INTENT(INOUT) ::                       CU_ACT_FLAG

!
! Optionals
!
   INTEGER, DIMENSION( ims:ime,         jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(  OUT) ::                           ktop_deep

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                           RTHFTEN,    &
                            cugd_tten,cugd_qvten,cugd_qcten,    &
                            cugd_ttens,cugd_qvtens,             &
                                                    RQVFTEN

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                                       &
                                                   RTHCUTEN,    &
                                                   RQVCUTEN,    &
                                                   RQVBLTEN,    &
                                                   RTHBLTEN,    &
                                                   RTHRATEN,    &
                                                   RQCCUTEN,    &
                                                   RQICUTEN
!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in
! use or not.
!
   LOGICAL, OPTIONAL ::                                      &
                                                   F_QV      &
                                                  ,F_QC      &
                                                  ,F_QR      &
                                                  ,F_QI      &
                                                  ,F_QS


#if ( WRF_DFI_RADAR == 1 )
!
!  option of cap suppress: 
!        do_capsuppress = 1   do
!        do_capsuppress = other   don't
!
!
   INTEGER,      INTENT(IN   ) ,OPTIONAL   :: do_capsuppress
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(IN   ),OPTIONAL  :: cap_suppress_loc
   REAL, DIMENSION( its:ite ) :: cap_suppress_j
#endif


! LOCAL VARS
     real,    dimension(its:ite,jts:jte,1:ensdim) ::      &
        xf_ens,pr_ens
     real,    dimension ( its:ite , jts:jte , 1:ensdim) ::      &
        massflni,xfi_ens,pri_ens
   REAL, DIMENSION( its:ite , jts:jte ) ::            MASSI_FLX,    &
                          APRi_GR,APRi_W,APRi_MC,APRi_ST,APRi_AS,    &
                         edti_out,APRi_CAPMA,APRi_CAPME,APRi_CAPMI,gswi
     real,    dimension (its:ite,kts:kte) ::                    &
        SUBT,SUBQ,OUTT,OUTQ,OUTQC,phh,subm,cupclw,cupclws,dhdt,         &
        outts,outqs,outqcs
     real,    dimension (its:ite)         ::                    &
        ztexec,zqexec,pret, ter11, aa0, fp,xlandi
!+lxz
     integer, dimension (its:ite) ::                            &
        ierr,ierrs,kbcon, ktop,kpbli,k22s,kbcons,ktops
!.lxz
     integer, dimension (its:ite,jts:jte) ::                    &
        iact_old_gr
     integer :: iens,ibeg,iend,jbeg,jend,n,nn,ens4n
     integer :: ibegh,iendh,jbegh,jendh
     integer :: ibegc,iendc,jbegc,jendc
   real rho_dryar,temp
   real :: PTEN,PQEN,PAPH,ZRHO,PAHFS,PQHFL,ZKHVFL,ZWS,PGEOH


!
! basic environmental input includes moisture convergence (mconv)
! omega (omeg), windspeed (us,vs), and a flag (aaeq) to turn off
! convection for this call only and at that particular gridpoint
!
     real,    dimension (its:ite,kts:kte) ::                    &
        zo,T2d,q2d,PO,P2d,US,VS,rhoi,tn,qo,tshall,qshall
     real,    dimension (its:ite,kts:kte,1:ens4) ::                    &
        omeg
     real, dimension (its:ite)            ::                    &
        ccn,Z1,PSUR,AAEQ,cuten,umean,vmean,pmean,xmbs
     real, dimension (its:ite,1:ens4)     ::                    &
        mconv

   INTEGER :: i,j,k,ICLDCK,ipr,jpr
   REAL    :: tcrit,tscl_KF,dp,dq,sub_spread,subcenter
   INTEGER :: itf,jtf,ktf,iss,jss,nbegin,nend
   INTEGER :: high_resolution
   REAL    :: rkbcon,rktop        !-lxz
   character*50 :: ierrc(its:ite)
   character*50 :: ierrcs(its:ite)
! ruc variable
     real, dimension (its:ite)            ::  tkm

  ! A. Betts for shallow convection: suggestion for the KF timescale < DELTAX  / 25 m/s
   tscl_kf=dx/25.
   ccn(its:ite)=1500.
  !
!   write(0,*)'ishallow = ',ishallow_g3
   high_resolution=0
   subcenter=0.
   iens=1
   ipr=0 !639
   jpr=0 !141
   IF ( periodic_x ) THEN
      ibeg=max(its,ids)
      iend=min(ite,ide-1)
      ibegc=max(its,ids)
      iendc=min(ite,ide-1)
   ELSE
      ibeg=max(its,ids)
      iend=min(ite,ide-1)
      ibegc=max(its,ids+4)
      iendc=min(ite,ide-5)
   END IF
   IF ( periodic_y ) THEN
      jbeg=max(jts,jds)
      jend=min(jte,jde-1)
      jbegc=max(jts,jds)
      jendc=min(jte,jde-1)
   ELSE
      jbeg=max(jts,jds)
      jend=min(jte,jde-1)
      jbegc=max(jts,jds+4)
      jendc=min(jte,jde-5)
   END IF
   do j=jts,jte
   do i=its,ite
     k22_shallow(i,j)=0
     kbcon_shallow(i,j)=0
     ktop_shallow(i,j)=0
     xmb_shallow(i,j)=0
   enddo
   enddo
   tcrit=258.

   itf=MIN(ite,ide-1)
   ktf=MIN(kte,kde-1)
   jtf=MIN(jte,jde-1)
!                                                                      
     DO 100 J = jts,jtf  
     DO n= 1,ensdim
     DO I= its,itf
       xfi_ens(i,j,n)=0.
       pri_ens(i,j,n)=0.
     ENDDO
     ENDDO
     DO I= its,itf
        ierrc(i)=" "
        ierrcs(i)=" "
        ierr(i)=0
        ierrs(i)=0
        kbcon(i)=0
        ktop(i)=0
        tkm(i)=0.
        xmbs(i)=0.
        k22s(i)=0
        kbcons(i)=0
        ktops(i)=0
        HBOT(I,J)  =REAL(KTE)
        HTOP(I,J)  =REAL(KTS)
        iact_old_gr(i,j)=0
        mass_flux(i,j)=0.
        massi_flx(i,j)=0.
        raincv(i,j)=0.
        pratec (i,j)=0.
        edt_out(i,j)=0.
        edti_out(i,j)=0.
        gswi(i,j)=gsw(i,j)
        xlandi(i)=xland(i,j)
        APRi_GR(i,j)=apr_gr(i,j)
        APRi_w(i,j)=apr_w(i,j)
        APRi_mc(i,j)=apr_mc(i,j)
        APRi_st(i,j)=apr_st(i,j)
        APRi_as(i,j)=apr_as(i,j)
        APRi_capma(i,j)=apr_capma(i,j)
        APRi_capme(i,j)=apr_capme(i,j)
        APRi_capmi(i,j)=apr_capmi(i,j)
        CU_ACT_FLAG(i,j) = .true.
     ENDDO
     do k=kts,kte
     DO I= its,itf
       cugd_tten(i,k,j)=0.
       cugd_ttens(i,k,j)=0.
       cugd_qvten(i,k,j)=0.
       cugd_qvtens(i,k,j)=0.
       cugd_qcten(i,k,j)=0.
     ENDDO
     ENDDO
     DO n=1,ens4
     DO I= its,itf
        mconv(i,n)=0.
     ENDDO
     do k=kts,kte
     DO I= its,itf
         omeg(i,k,n)=0.
     ENDDO
     ENDDO
     ENDDO
     DO k=1,ensdim
     DO I= its,itf
        massflni(i,j,k)=0.
     ENDDO
     ENDDO
     !  put hydrostatic pressure on half levels
     DO K=kts,ktf
     DO I=ITS,ITF
         phh(i,k) = p(i,k,j)
     ENDDO
     ENDDO

!ipr= 33 !78
!jpr= 17 !110
     DO I=ITS,ITF
         PSUR(I)=p8w(I,1,J)*.01
!        PSUR(I)=p(I,1,J)*.01
         TER11(I)=max(0.,HT(i,j))
         ZTEXEC(i) = 0.
         ZQEXEC(i) = 0.
         aaeq(i)=0.
         pret(i)=0.
         umean(i)=0.
         vmean(i)=0.
         pmean(i)=0.
         kpbli(i)=kpbl(i,j)
         zo(i,kts)=ter11(i)+.5*dz8w(i,1,j)
         DO K=kts+1,ktf
         zo(i,k)=zo(i,k-1)+.5*(dz8w(i,k-1,j)+dz8w(i,k,j))
         enddo
     ENDDO
!    if(j.eq.jpr .and. (ipr.gt.its .and. ipr.lt.itf))write(0,*)psur(ipr),ter11(ipr),kpbli(ipr)
     DO K=kts,ktf
     DO I=ITS,ITF
         po(i,k)=phh(i,k)*.01
         subm(i,k)=0.
         P2d(I,K)=PO(i,k)
         rhoi(i,k)=rho(i,k,j)
         US(I,K) =u(i,k,j)
         VS(I,K) =v(i,k,j)
         T2d(I,K)=t(i,k,j)
         q2d(I,K)=q(i,k,j)
         IF(Q2d(I,K).LT.1.E-08)Q2d(I,K)=1.E-08
         SUBT(I,K)=0.
         SUBQ(I,K)=0.
         OUTT(I,K)=0.
         OUTQ(I,K)=0.
         OUTQC(I,K)=0.
         OUTQCs(I,K)=0.
         OUTTS(I,K)=0.
         OUTQS(I,K)=0.
         TN(I,K)=t2d(i,k)+(RTHFTEN(i,k,j)+RTHRATEN(i,k,j)+RTHBLTEN(i,k,j)) &
                          *pi(i,k,j)*dt
         QO(I,K)=q2d(i,k)+(RQVFTEN(i,k,j)+RQVBLTEN(i,k,j))*dt
         TSHALL(I,K)=t2d(i,k)+RTHBLTEN(i,k,j)*pi(i,k,j)*dt
         DHDT(I,K)=cp*RTHBLTEN(i,k,j)*pi(i,k,j)+ XLV*RQVBLTEN(i,k,j)
         QSHALL(I,K)=q2d(i,k)+RQVBLTEN(i,k,j)*dt
         IF(TN(I,K).LT.200.)TN(I,K)=T2d(I,K)
         IF(QO(I,K).LT.1.E-08)QO(I,K)=1.E-08
         cupclws(i,k) = 0.
     ENDDO
     ENDDO
     if(use_excess.gt.0 .or. use_excess_sh.gt.0)then
     DO I=ITS,ITF
       ZRHO  = 100.*psur(i)/(287.04*(t2d(i,1)*(1.+0.608*q2d(i,1))))
       !- LE and H fluxes 
       PAHFS=-hfx(i,j) 
       PQHFL=-qfx(i,j)/xlv 

       !- buoyancy flux (H+LE)
       ZKHVFL= (PAHFS/1004.64+0.608*t2d(i,1)*PQHFL)/ZRHO
       !- height of the 1st level
       PGEOH = zo(i,1)-ht(i,j) 
       !-convective-scale velocity w*
       ZWS = max(0.,0.001-1.5*0.41*ZKHVFL*PGEOH/T2D(i,1))

       if(ZWS > TINY(PGEOH)) then
         !-convective-scale velocity w*
         ZWS = 1.2*ZWS**.3333
         !- temperature excess 
         ZTEXEC(i)     = MAX(-1.5*PAHFS/(ZRHO*ZWS*1004.64),0.0)
         !- moisture  excess
         ZQEXEC(i)     = MAX(-1.5*PQHFL/(ZRHO*ZWS),0.)
       endif
     ENDDO
     endif  ! use_excess
     DO K=kts,ktf
     DO I=ITS,ITF
         omeg(I,K,:)= -g*rho(i,k,j)*w(i,k,j)
     enddo
     enddo
     do k=  kts+1,ktf-1
     DO I = its,itf
         if((p2d(i,1)-p2d(i,k)).gt.150.and.p2d(i,k).gt.300)then
            dp=-.5*(p2d(i,k+1)-p2d(i,k-1))
            umean(i)=umean(i)+us(i,k)*dp
            vmean(i)=vmean(i)+vs(i,k)*dp
            pmean(i)=pmean(i)+dp
         endif
     enddo
     enddo
      do n=1,ens4
      DO K=kts,ktf-1
      DO I = its,itf
        dq=(q2d(i,k+1)-q2d(i,k))
        mconv(i,n)=mconv(i,n)+omeg(i,k,n)*dq/g
      enddo
      ENDDO
      ENDDO
      do n=1,ens4
      DO I = its,itf
        if(mconv(i,n).lt.0.)mconv(i,n)=0.
      ENDDO
      ENDDO
!
!---- CALL CUMULUS PARAMETERIZATION
!
#if ( WRF_DFI_RADAR == 1 )
      if(do_capsuppress == 1 ) then
        DO I= its,itf
            cap_suppress_j(i)=cap_suppress_loc(i,j)
        ENDDO
      endif
#endif
      CALL CUP_gf(zo,outqc,j,AAEQ,T2d,Q2d,TER11,subm,TN,QO,PO,PRET,&
           P2d,OUTT,OUTQ,DT,itimestep,PSUR,US,VS,tcrit,iens, &
           ztexec,zqexec,ccn,ccnclean,rhoi,dx,mconv,omeg,  &
           maxiens,maxens,maxens2,maxens3,ensdim,                 &
           APRi_GR,APRi_W,APRi_MC,APRi_ST,APRi_AS,                &
           APRi_CAPMA,APRi_CAPME,APRi_CAPMI,kbcon,ktop,cupclw,    &
           xfi_ens,pri_ens,XLANDi,gswi,subt,subq,        &
           xlv,r_v,cp,g,ichoice,ipr,jpr,ierrc,ens4,    &
           beta,autoconv,aeroevap,itf,jtf,ktf,training, &
#if ( WRF_DFI_RADAR == 1 )
           do_capsuppress,cap_suppress_j,                         &             
#endif
           use_excess,its,ite, jts,jte, kts,kte                               &
                                                             )

      CALL neg_check(j,subt,subq,dt,q2d,outq,outt,outqc,pret,its,ite,kts,kte,itf,ktf)
       if(ishallow_g3 == 1 )then


!
! this turns off shallow convection when deep convection is active
!
!      do i=its,ite
!       if(pret(i).gt.0.)then
!           ierrs(i)=1
!           aaeq(i)=-100.
!       endif
!      enddo
   call CUP_gf_sh(xmbs,zo,OUTQCs,J,AAEQ,T2D,Q2D,TER11,                    &
              Tshall,Qshall,P2d,PRET,P2d,OUTTS,OUTQS,DT,itimestep,PSUR,US,VS,    &
              TCRIT,ztexec,zqexec,ccn,ccnclean,rhoi,dx,dhdt, &
              kpbli,kbcons,ktops,cupclws,k22s,         &   !-lxz
              xlandi,gswi,tscl_kf,               &
              xlv,r_v,cp,g,ichoice,0,0,ierrs,ierrcs,         &
              autoconv,itf,jtf,ktf,               &
              use_excess_sh,its,ite, jts,jte, kts,kte &
                                                              )
        endif



            if(j.lt.jbegc.or.j.gt.jendc)go to 100
             if(ishallow_g3.eq.1)then
               DO I=ibegc,iendc
                 xmb_shallow(i,j)=xmbs(i)
                 k22_shallow(i,j)=k22s(i)
                 kbcon_shallow(i,j)=kbcons(i)
                 ktop_shallow(i,j)=ktops(i)
                 ktop_deep(i,j) = ktop(i)
               ENDDO
            endif
            DO I=ibegc,iendc
              cuten(i)=0.
              if(pret(i).gt.0.)then
                 cuten(i)=1.
!                raincv(i,j)=pret(i)*dt
              endif
            ENDDO
            DO I=ibegc,iendc
            DO K=kts,ktf
               RTHCUTEN(I,K,J)=(outts(i,k)+(subt(i,k)+outt(i,k))*cuten(i))/pi(i,k,j)
               RQVCUTEN(I,K,J)=outqs(i,k)+(subq(i,k)+outq(i,k))*cuten(i)
            ENDDO
            ENDDO
            DO I=ibegc,iendc
              if(pret(i).gt.0.)then
                 raincv(i,j)=pret(i)*dt
                 pratec(i,j)=pret(i)
                 rkbcon = kte+kts - kbcon(i)
                 rktop  = kte+kts -  ktop(i)
                 if (ktop(i)  > HTOP(i,j)) HTOP(i,j) = ktop(i)+.001
                 if (kbcon(i) < HBOT(i,j)) HBOT(i,j) = kbcon(i)+.001
              endif
            ENDDO
            DO n= 1,ensdim
            DO I= ibegc,iendc
              xf_ens(i,j,n)=xfi_ens(i,j,n)
              pr_ens(i,j,n)=pri_ens(i,j,n)
            ENDDO
            ENDDO
            DO I= ibegc,iendc
               APR_GR(i,j)=apri_gr(i,j)
               APR_w(i,j)=apri_w(i,j)
               APR_mc(i,j)=apri_mc(i,j)
               APR_st(i,j)=apri_st(i,j)
               APR_as(i,j)=apri_as(i,j)
               APR_capma(i,j)=apri_capma(i,j)
               APR_capme(i,j)=apri_capme(i,j)
               APR_capmi(i,j)=apri_capmi(i,j)
               mass_flux(i,j)=massi_flx(i,j)
               edt_out(i,j)=edti_out(i,j)
            ENDDO
            IF(PRESENT(RQCCUTEN)) THEN
              IF ( F_QC ) THEN
                DO K=kts,ktf
                DO I=ibegc,iendc
                   RQCCUTEN(I,K,J)=outqcs(i,k)+outqc(I,K)*cuten(i)
                   IF ( PRESENT( GDC ) ) GDC(I,K,J)=cupclws(i,k)+CUPCLW(I,K)*cuten(i)
                   IF ( PRESENT( GDC2 ) ) GDC2(I,K,J)=0.
                ENDDO
                ENDDO
              ENDIF
            ENDIF

!......     QSTEN STORES GRAUPEL TENDENCY IF IT EXISTS, OTHERISE SNOW (V2)     

            IF(PRESENT(RQICUTEN).AND.PRESENT(RQCCUTEN))THEN
              IF (F_QI) THEN
                DO K=kts,ktf
                  DO I=ibegc,iendc
                   if(t2d(i,k).lt.258.)then
                      RQICUTEN(I,K,J)=outqcs(i,k)+outqc(I,K)*cuten(i)
                      RQCCUTEN(I,K,J)=0.
                      IF ( PRESENT( GDC2 ) ) GDC2(I,K,J)=cupclws(i,k)+CUPCLW(I,K)*cuten(i)
                   else
                      RQICUTEN(I,K,J)=0.
                      RQCCUTEN(I,K,J)=outqcs(i,k)+outqc(I,K)*cuten(i)
                      IF ( PRESENT( GDC ) ) GDC(I,K,J)=cupclws(i,k)+CUPCLW(I,K)*cuten(i)
                   endif
                ENDDO
                ENDDO
              ENDIF
            ENDIF

 100    continue

   END SUBROUTINE GFDRV


   SUBROUTINE CUP_gf(zo,OUTQC,J,AAEQ,T,Q,Z1,sub_mas,                    &
              TN,QO,PO,PRE,P,OUTT,OUTQ,DTIME,ktau,PSUR,US,VS,    &
              TCRIT,iens,                                        &
              ztexec,zqexec,ccn,ccnclean,rho,dx,mconv,                               &
              omeg,maxiens,                          &
              maxens,maxens2,maxens3,ensdim,                           &
              APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                       &
              APR_CAPMA,APR_CAPME,APR_CAPMI,kbcon,ktop,cupclw,         &   !-lxz
              xf_ens,pr_ens,xland,gsw,subt,subq,               &
              xl,rv,cp,g,ichoice,ipr,jpr,ierrc,ens4,         &
              beta,autoconv,aeroevap,itf,jtf,ktf,training,               &
#if ( WRF_DFI_RADAR == 1 )
              do_capsuppress,cap_suppress_j,                         &             
#endif
              use_excess,its,ite, jts,jte, kts,kte                                &
                                                )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        autoconv,aeroevap,itf,jtf,ktf,ktau,training,use_excess,        &
        its,ite, jts,jte, kts,kte,ipr,jpr,ens4
     integer, intent (in   )              ::                           &
        j,ensdim,maxiens,maxens,maxens2,maxens3,ichoice,iens
  !
  ! 
  !
     real,    dimension (its:ite,jts:jte,1:ensdim)                     &
        ,intent (inout)                   ::                           &
        xf_ens,pr_ens
     real,    dimension (its:ite,jts:jte)                              &
        ,intent (inout )                  ::                           &
               APR_GR,APR_W,APR_MC,APR_ST,APR_AS,APR_CAPMA,     &
               APR_CAPME,APR_CAPMI
    real, dimension( its:ite , jts:jte )                               &
          :: weight_GR,weight_W,weight_MC,weight_ST,weight_AS
     real,    dimension (its:ite,jts:jte)                              &
        ,intent (in   )                   ::                           &
               gsw

#if ( WRF_DFI_RADAR == 1 )
    INTEGER,      INTENT(IN   ) ,OPTIONAL   :: do_capsuppress
    REAL, DIMENSION( its:ite ) :: cap_suppress_j
#endif

  ! outtem = output temp tendency (per s)
  ! outq   = output q tendency (per s)
  ! outqc  = output qc tendency (per s)
  ! pre    = output precip
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                   ::                           &
        OUTT,OUTQ,OUTQC,subt,subq,sub_mas,cupclw
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pre
     integer,    dimension (its:ite)                                   &
        ,intent (out  )                   ::                           &
        kbcon,ktop
!    integer,    dimension (its:ite)                                   &
!       ,intent (in  )                   ::                           &
!       kpbl
  !
  ! basic environmental input includes moisture convergence (mconv)
  ! omega (omeg), windspeed (us,vs), and a flag (aaeq) to turn off
  ! convection for this call only and at that particular gridpoint
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        rho,T,PO,P,US,VS,tn
     real,    dimension (its:ite,kts:kte,1:ens4)                       &
        ,intent (inout   )                   ::                           &
        omeg
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
         Q,QO
     real, dimension (its:ite)                                         &
        ,intent (in   )                   ::                           &
        ztexec,zqexec,ccn,Z1,PSUR,AAEQ,xland
     real, dimension (its:ite,1:ens4)                                         &
        ,intent (in   )                   ::                           &
        mconv

       
       real                                                            &
        ,intent (in   )                   ::                           &
        beta,dx,ccnclean,dtime,tcrit,xl,cp,rv,g


!
!  local ensemble dependent variables in this routine
!
     real,    dimension (its:ite,1:maxens)  ::                         &
        xaa0_ens
     real,    dimension (1:maxens)  ::                                 &
        mbdt_ens
     real,    dimension (1:maxens2) ::                                 &
        edt_ens
     real,    dimension (its:ite,1:maxens2) ::                         &
        edtc
     real,    dimension (its:ite,kts:kte,1:maxens2) ::                 &
        dellat_ens,dellaqc_ens,dellaq_ens,pwo_ens,subt_ens,subq_ens
!
!
!
!***************** the following are your basic environmental
!                  variables. They carry a "_cup" if they are
!                  on model cloud levels (staggered). They carry
!                  an "o"-ending (z becomes zo), if they are the forced
!                  variables. They are preceded by x (z becomes xz)
!                  to indicate modification by some typ of cloud
!
  ! z           = heights of model levels
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! p           = environmental pressure
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! z_cup       = heights of model cloud levels
  ! q_cup       = environmental q on model cloud levels
  ! qes_cup     = saturation q on model cloud levels
  ! t_cup       = temperature (Kelvin) on model cloud levels
  ! p_cup       = environmental pressure
  ! he_cup = moist static energy on model cloud levels
  ! hes_cup = saturation moist static energy on model cloud levels
  ! gamma_cup = gamma on model cloud levels
!
!
  ! hcd = moist static energy in downdraft
  ! zd normalized downdraft mass flux
  ! dby = buoancy term
  ! entr = entrainment rate
  ! zd   = downdraft normalized mass flux
  ! entr= entrainment rate
  ! hcd = h in model cloud
  ! bu = buoancy term
  ! zd = normalized downdraft mass flux
  ! gamma_cup = gamma on model cloud levels
  ! qcd = cloud q (including liquid water) after entrainment
  ! qrch = saturation q in cloud
  ! pwd = evaporate at that level
  ! pwev = total normalized integrated evaoprate (I2)
  ! entr= entrainment rate
  ! z1 = terrain elevation
  ! entr = downdraft entrainment rate
  ! jmin = downdraft originating level
  ! kdet = level above ground where downdraft start detraining
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! pr_ens = precipitation ensemble
  ! xf_ens = mass flux ensembles
  ! massfln = downdraft mass flux ensembles used in next timestep
  ! omeg = omega from large scale model
  ! mconv = moisture convergence from large scale model
  ! zd      = downdraft normalized mass flux
  ! zu      = updraft normalized mass flux
  ! dir     = "storm motion"
  ! mbdt    = arbitrary numerical parameter
  ! dtime   = dt over which forcing is applied
  ! iact_gr_old = flag to tell where convection was active
  ! kbcon       = LFC of parcel from k22
  ! k22         = updraft originating level
  ! icoic       = flag if only want one closure (usually set to zero!)
  ! dby = buoancy term
  ! ktop = cloud top (output)
  ! xmb    = total base mass flux
  ! hc = cloud moist static energy
  ! hkb = moist static energy at originating level

     real,    dimension (its:ite,kts:kte) ::                           &
        entr_rate_2d,mentrd_rate_2d,he,hes,qes,z,                      &
        heo,heso,qeso,zo,                                              &
        xhe,xhes,xqes,xz,xt,xq,                                        &

        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,      &
        qeso_cup,qo_cup,heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,     &
        tn_cup,                                                        &
        xqes_cup,xq_cup,xhe_cup,xhes_cup,xz_cup,xp_cup,xgamma_cup,     &
        xt_cup,                                                        &

        xlamue,dby,qc,qrcd,pwd,pw,hcd,qcd,dbyd,hc,qrc,zu,zd,clw_all,   &
        dbyo,qco,qrcdo,pwdo,pwo,hcdo,qcdo,dbydo,hco,qrco,zuo,zdo,      &
        xdby,xqc,xqrcd,xpwd,xpw,xhcd,xqcd,xhc,xqrc,xzu,xzd,            &

  ! cd  = detrainment function for updraft
  ! cdd = detrainment function for downdraft
  ! dellat = change of temperature per unit mass flux of cloud ensemble
  ! dellaq = change of q per unit mass flux of cloud ensemble
  ! dellaqc = change of qc per unit mass flux of cloud ensemble

        cd,cdd,DELLAH,DELLAQ,DELLAT,DELLAQC,dsubt,dsubh,dsubq

  ! aa0 cloud work function for downdraft
  ! edt = epsilon
  ! aa0     = cloud work function without forcing effects
  ! aa1     = cloud work function with forcing effects
  ! xaa0    = cloud work function with cloud effects (ensemble dependent)
  ! edt     = epsilon

     real,    dimension (its:ite) ::                                   &
       edt,edto,edtx,AA1,AA0,XAA0,HKB,                          &
       HKBO,XHKB,QKB,QKBO,                                    &
       XMB,XPWAV,XPWEV,PWAV,PWEV,PWAVO,                                &
       PWEVO,BU,BUD,BUO,cap_max,xland1,                                    &
       cap_max_increment,closure_n,psum,psumh,sig,zuhe
     real,    dimension (its:ite,1:ens4) ::                                   &
        axx
     integer,    dimension (its:ite) ::                                &
       kzdown,KDET,K22,KB,JMIN,kstabi,kstabm,K22x,        &   !-lxz
       KBCONx,KBx,KTOPx,ierr,ierr2,ierr3,KBMAX

     integer                              ::                           &
       nall,iedt,nens,nens3,ki,I,K,KK,iresult
     real                                 ::                           &
      day,dz,dzo,mbdt,entr_rate,radius,entrd_rate,mentrd_rate,  &
      zcutdown,edtmax,edtmin,depth_min,zkbmax,z_detr,zktop,      &
      massfld,dh,cap_maxs,trash,frh,xlamdd
      real detdo1,detdo2,entdo,dp,subin,detdo,entup,                &
      detup,subdown,entdoj,entupk,detupk,totmas
      real :: power_entr,zustart,zufinal,dzm1,dzp1


     integer :: k1,k2,kbegzu,kfinalzu,kstart,jmini,levadj
     logical :: keep_going
     real xff_shal(9),blqe,xkshal
     character*50 :: ierrc(its:ite)
     real,    dimension (its:ite,kts:kte) ::                           &
       up_massentr,up_massdetr,dd_massentr,dd_massdetr                 &
      ,up_massentro,up_massdetro,dd_massentro,dd_massdetro
     real,    dimension (kts:kte) :: smth

      levadj=5
      power_entr=2. ! 1.2
      zustart=.1
      zufinal=1.
      day=86400.
      do i=its,itf
        closure_n(i)=16.
        xland1(i)=1.
        if(xland(i).gt.1.5)xland1(i)=0.
        cap_max_increment(i)=25.
        ierrc(i)=" "
!       cap_max_increment(i)=1.
      enddo
!
!--- specify entrainmentrate and detrainmentrate
!--- highly tuneable !
!
      entr_rate=7.e-5
      radius=.1/entr_rate
      frh=3.14*(radius*radius)/dx/dx
      if(frh .gt. 0.7)then
         frh=.7
         radius=sqrt(frh*dx*dx/3.14)
         entr_rate=.2/radius
      endif
      do i=its,itf
         sig(i)=(1.-frh)**2
      enddo
!      sig(:)=1.

!
!--- entrainment of mass
!
      mentrd_rate=entr_rate ! 0.
      xlamdd=mentrd_rate
!
!--- initial detrainmentrates
!
      do k=kts,ktf
      do i=its,itf
        z(i,k)=zo(i,k)
        xz(i,k)=zo(i,k)
        cupclw(i,k)=0.
        cd(i,k)=1.*entr_rate
        cdd(i,k)=xlamdd
        hcdo(i,k)=0.
        qrcdo(i,k)=0.
        dellaqc(i,k)=0.
      enddo
      enddo
!
!--- max/min allowed value for epsilon (ratio downdraft base mass flux/updraft
!    base mass flux
!
      edtmax=1.
      edtmin=.1
!
!--- minimum depth (m), clouds must have
!
      depth_min=1000.   ! gg 500
!
!--- maximum depth (mb) of capping 
!--- inversion (larger cap = no convection)
!
      cap_maxs=75.
      DO i=its,itf
        kbmax(i)=1
        aa0(i)=0.
        aa1(i)=0.
        edt(i)=0.
        kstabm(i)=ktf-1
        IERR(i)=0
        IERR2(i)=0
        IERR3(i)=0
      enddo
      do i=its,itf
          cap_max(i)=cap_maxs
        iresult=0

      enddo
!
!--- max height(m) above ground where updraft air can originate
!
      zkbmax=4000.
!
!--- height(m) above which no downdrafts are allowed to originate
!
      zcutdown=3000.
!
!--- depth(m) over which downdraft detrains all its mass
!
      z_detr=1250.   !1000
!
      do nens=1,maxens
         mbdt_ens(nens)=(float(nens)-3.)*dtime*1.e-3+dtime*5.E-03
      enddo
      do nens=1,maxens2
         edt_ens(nens)=.95-float(nens)*.01
      enddo
!
!--- environmental conditions, FIRST HEIGHTS
!
      do i=its,itf
         if(ierr(i).ne.20)then
            do k=1,maxens*maxens2*maxens3
               xf_ens(i,j,(iens-1)*maxens*maxens2*maxens3+k)=0.
               pr_ens(i,j,(iens-1)*maxens*maxens2*maxens3+k)=0.
            enddo
         endif
      enddo
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(z,qes,he,hes,t,q,p,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_env(zo,qeso,heo,heso,tn,qo,po,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)

!
!--- environmental values on cloud levels
!
      call cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,he_cup, &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_env_clev(tn,qeso,qo,heo,heso,zo,po,qeso_cup,qo_cup, &
           heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,tn_cup,psur,  &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
        if(ierr(i).eq.0)then
        if(aaeq(i).lt.-0.1)then
           ierr(i)=20
        endif
!
      do k=kts,ktf
        if(zo_cup(i,k).gt.zkbmax+z1(i))then
          kbmax(i)=k
          go to 25
        endif
      enddo
 25   continue
!
!--- level where detrainment for downdraft starts
!
      do k=kts,ktf
        if(zo_cup(i,k).gt.z_detr+z1(i))then
          kdet(i)=k
          go to 26
        endif
      enddo
 26   continue
!
      endif
      enddo

!
!
!
!------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT - K22
!
      CALL cup_MAXIMI(HEO_CUP,3,KBMAX,K22,ierr, &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
       DO 36 i=its,itf
         IF(ierr(I).eq.0)THEN
           frh=q_cup(i,k22(i))/qes_cup(i,k22(i))
           IF(omeg(i,k22(i),1).lt.0. .and. frh.ge.0.99 .and. sig(i).lt.0.091)ierr(i)=1200
           IF(K22(I).GE.KBMAX(i))THEN
             ierr(i)=2
             ierrc(i)="could not find k22"
           ENDIF
         ENDIF
 36   CONTINUE
!
!--- DETERMINE THE LEVEL OF CONVECTIVE CLOUD BASE  - KBCON
!
      do i=its,itf
       IF(ierr(I).eq.0)THEN
         if(use_excess == 2) then
             k1=max(1,k22(i)-1)
             k2=k22(i)+1
             hkb(i) =he_cup(i,k22(i)) ! sum(he_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
             hkbo(i)=sum(heo_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
        else if(use_excess <= 1)then
         hkb(i)=he_cup(i,k22(i)) ! +float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
         hkbo(i)=heo_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
        endif  ! excess
       endif ! ierr
      enddo


      call cup_kbcon(ierrc,cap_max_increment,1,k22,kbcon,heo_cup,heso_cup, &
           hkbo,ierr,kbmax,po_cup,cap_max, &
           xl,cp,ztexec,zqexec,use_excess,       &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!--- increase detrainment in stable layers
!
      CALL cup_minimi(HEso_cup,Kbcon,kstabm,kstabi,ierr,  &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      DO i=its,itf
         IF(ierr(I).eq.0)THEN
         do k=k22(i),kbcon(i)
         frh=q_cup(i,k)/qes_cup(i,k)
         if(omeg(i,k,1).lt.-1.e-6 .and. frh.ge.0.99 .and. sig(i).lt.0.091)ierr(i)=1200
         enddo
         endif
      enddo
!
! the following section insures a smooth normalized mass flux profile. See Grell
! and Freitas (2013) for a description
!
      DO i=its,itf
         IF(ierr(I).eq.0)THEN
            do k=kts,ktf
               frh = min(qo_cup(i,k)/qeso_cup(i,k),1.)
               entr_rate_2d(i,k)=entr_rate*(1.3-frh)
            enddo
            zuhe(i)=zustart
            kstart=1
            frh=(zufinal-zustart)/((float(kbcon(i)*kbcon(i)))-(float(kstart*kstart)))
            dh=zuhe(i)-frh*(float(kstart*kstart))
            do k=kstart,kbcon(i)-1
             dz=z_cup(i,k+1)-z_cup(i,k)
!            cd(i,k)=entr_rate_2d(i,kbcon(i))
             if(p_cup(i,k).gt. p_cup(i,kstabi(i)))cd(i,k)=1.e-6
             entr_rate_2d(i,k)=((frh*(float((k+1)*(k+1)))+dh)/zuhe(i)-1.+cd(i,k)*dz)/dz
             zuhe(i)=zuhe(i)+entr_rate_2d(i,k)*dz*zuhe(i)-cd(i,k)*dz*zuhe(i)
            enddo
            kbegzu=kstabi(i)+4
            kbegzu=min(kbegzu,ktf-1)
            kfinalzu=kbegzu+1
            do k=kts,ktf
               cd(i,k)=entr_rate_2d(i,kbcon(i))
            enddo
               do k=kbcon(i),kbegzu
                cd(i,k)=entr_rate_2d(i,kbcon(i))
                if(p_cup(i,k).gt. p_cup(i,kstabi(i)))cd(i,k)=1.e-6
                dz=z_cup(i,k+1)-z_cup(i,k)
                zuhe(i)=zuhe(i)+entr_rate_2d(i,k)*dz*zuhe(i)-cd(i,k)*dz*zuhe(i)
               enddo
         do k=kstabi(i),ktf-2
          if((hkb(i)-hes_cup(i,k)).lt.0)then
              kfinalzu=k-3
              go to 411
          endif
         enddo
411      continue
             kfinalzu=max(kfinalzu,kbegzu+1)
             kfinalzu=min(kfinalzu,ktf-1)
            frh=-(0.2-zuhe(i))/((float(kfinalzu*kfinalzu))-(float(kbegzu*kbegzu)))
            dh=zuhe(i)+frh*(float(kbegzu*kbegzu))
               do k=kbegzu+1,kfinalzu
                 dz=z_cup(i,k+1)-z_cup(i,k)
                 cd(i,k)=-((-frh*(float((k+1)*(k+1)))+dh)/zuhe(i)-1.-entr_rate_2d(i,k)*dz)/dz
                 zuhe(i)=zuhe(i)+entr_rate_2d(i,k)*dz*zuhe(i)-cd(i,k)*dz*zuhe(i)
               enddo
               do k=kfinalzu+1,ktf
                   cd(i,k)=entr_rate_2d(i,k)
               enddo
               do k=kts+1,ktf-2
                 dzm1=z_cup(i,k)-z_cup(i,k-1)
                 dz=z_cup(i,k+1)-z_cup(i,k)
                 dzp1=z_cup(i,k+2)-z_cup(i,k+1)
                 smth(k)=.25*(dzm1*cd(i,k-1)+2.*dz*cd(i,k)+dzp1*cd(i,k+1))
               enddo
               do k=kts+1,ktf-2
                 dzm1=z_cup(i,k)-z_cup(i,k-1)
                 dz=z_cup(i,k+1)-z_cup(i,k)
                 dzp1=z_cup(i,k+2)-z_cup(i,k+1)
                 cd(i,k)=smth(k)/dz ! (.25*(dzm1+2.*dz+dzp1))
               enddo

            smth(:)=0.
            do k=2,ktf-2
                 dzm1=z_cup(i,k)-z_cup(i,k-1)
                 dz=z_cup(i,k+1)-z_cup(i,k)
                 dzp1=z_cup(i,k+2)-z_cup(i,k+1)
              smth(k)=.25*(dzm1*entr_rate_2d(i,k-1)+2.*dz*entr_rate_2d(i,k)+dzp1*entr_rate_2d(i,k+1))
            enddo
            do k=2,ktf-2 
                 dz=z_cup(i,k+1)-z_cup(i,k)
              entr_rate_2d(i,k)=smth(k)/dz
            enddo
            zuhe(i)=zustart
            do k=2,kbegzu 
              dz=z_cup(i,k+1)-z_cup(i,k)
              frh=zuhe(i)
              zuhe(i)=zuhe(i)+entr_rate_2d(i,k)*dz*zuhe(i)-cd(i,k)*dz*zuhe(i)
            enddo
         ENDIF
       enddo

!
! calculate mass entrainment and detrainment
!
      do k=kts,ktf
      do i=its,itf
         hc(i,k)=0.
         DBY(I,K)=0.
         hco(i,k)=0.
         DBYo(I,K)=0.
      enddo
      enddo
      do i=its,itf
       IF(ierr(I).eq.0)THEN
         do k=1,kbcon(i)-1
            hc(i,k)=hkb(i)
            hco(i,k)=hkbo(i)
         enddo
         k=kbcon(i)
         hc(i,k)=hkb(i)
         DBY(I,Kbcon(i))=Hkb(I)-HES_cup(I,K)
         hco(i,k)=hkbo(i)
         DBYo(I,Kbcon(i))=Hkbo(I)-HESo_cup(I,K)
       endif ! ierr
      enddo
!
!
      do i=its,itf
         if(ierr(i).eq.0)then
         zu(i,1)=zustart
         zuo(i,1)=zustart
!    mass entrainment and detrinament is defined on model levels
         do k=2,ktf-1
          dz=zo_cup(i,k)-zo_cup(i,k-1)
          up_massentro(i,k-1)=entr_rate_2d(i,k-1)*dz*zuo(i,k-1)
          up_massdetro(i,k-1)=cd(i,k-1)*dz*zuo(i,k-1)
          zuo(i,k)=zuo(i,k-1)+up_massentro(i,k-1)-up_massdetro(i,k-1)
          if(zuo(i,k).lt.0.05)then
             zuo(i,k)=.05
             up_massdetro(i,k-1)=zuo(i,k-1)-.05  + up_massentro(i,k-1)
             cd(i,k-1)=up_massdetro(i,k-1)/dz/zuo(i,k-1)
          endif
          zu(i,k)=zuo(i,k)
          up_massentr(i,k-1)=up_massentro(i,k-1)
          up_massdetr(i,k-1)=up_massdetro(i,k-1)
         enddo
         do k=kbcon(i)+1,ktf-1
          hc(i,k)=(hc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*hc(i,k-1)+ &
                         up_massentr(i,k-1)*he(i,k-1))   /            &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          dby(i,k)=hc(i,k)-hes_cup(i,k)
          hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+ &
                         up_massentro(i,k-1)*heo(i,k-1))   /            &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
          dbyo(i,k)=hco(i,k)-heso_cup(i,k)
         enddo
         do k=kbcon(i)+1,ktf
          if(dbyo(i,k).lt.0)then
              ktop(i)=k-1
              go to 41
          endif
         enddo
41       continue
         if(ktop(i).lt.kbcon(i)+2)ierr(i)=5
         do k=ktop(i)+1,ktf
           HC(i,K)=hes_cup(i,k)
           HCo(i,K)=heso_cup(i,k)
           DBY(I,K)=0.
           DBYo(I,K)=0.
           zu(i,k)=0.
           zuo(i,k)=0.
           cd(i,k)=0.
           entr_rate_2d(i,k)=0.
           up_massentr(i,k)=0.
           up_massdetr(i,k)=0.
           up_massentro(i,k)=0.
           up_massdetro(i,k)=0.
         enddo
      endif
      enddo
!
      DO 37 i=its,itf
         kzdown(i)=0
         if(ierr(i).eq.0)then
            zktop=(zo_cup(i,ktop(i))-z1(i))*.6
            zktop=min(zktop+z1(i),zcutdown+z1(i))
            do k=kts,ktf
              if(zo_cup(i,k).gt.zktop)then
                 kzdown(i)=k
                 go to 37
              endif
              enddo
         endif
 37   CONTINUE
!
!--- DOWNDRAFT ORIGINATING LEVEL - JMIN
!
      call cup_minimi(HEso_cup,K22,kzdown,JMIN,ierr, &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      DO 100 i=its,itf
         IF(ierr(I).eq.0)THEN
!
!--- check whether it would have buoyancy, if there where
!--- no entrainment/detrainment
!
         jmini = jmin(i)
         keep_going = .TRUE.
         do while ( keep_going )
           keep_going = .FALSE.
           if ( jmini - 1 .lt. kdet(i)   ) kdet(i) = jmini-1
           if ( jmini     .ge. ktop(i)-1 ) jmini = ktop(i) - 2
           ki = jmini
           hcdo(i,ki)=heso_cup(i,ki)
           DZ=Zo_cup(i,Ki+1)-Zo_cup(i,Ki)
           dh=0.
           do k=ki-1,1,-1
             hcdo(i,k)=heso_cup(i,jmini)
             DZ=Zo_cup(i,K+1)-Zo_cup(i,K)
             dh=dh+dz*(HCDo(i,K)-heso_cup(i,k))
             if(dh.gt.0.)then
               jmini=jmini-1
               if ( jmini .gt. 5 ) then
                 keep_going = .TRUE.
               else
                 ierr(i) = 9
                 ierrc(i) = "could not find jmini9"
                 exit
               endif
             endif
           enddo
         enddo
         jmin(i) = jmini 
         if ( jmini .le. 5 ) then
           ierr(i)=4
           ierrc(i) = "could not find jmini4"
         endif
       ENDIF
100   continue
!
! - Must have at least depth_min m between cloud convective base
!     and cloud top.
!
      do i=its,itf
         IF(ierr(I).eq.0)THEN
            IF(-zo_cup(I,KBCON(I))+zo_cup(I,KTOP(I)).LT.depth_min)then
               ierr(i)=6
               ierrc(i)="cloud depth very shallow"
            endif
         endif
      enddo

!
!--- normalized downdraft mass flux profile,also work on bottom detrainment
!--- in this routine
!
      do k=kts,ktf
      do i=its,itf
       zd(i,k)=0.
       zdo(i,k)=0.
       cdd(i,k)=0.
       dd_massentr(i,k)=0.
       dd_massdetr(i,k)=0.
       dd_massentro(i,k)=0.
       dd_massdetro(i,k)=0.
       hcdo(i,k)=heso_cup(i,k)
       dbydo(i,k)=0.
      enddo
      enddo
      do i=its,itf
          bud(i)=0.
          IF(ierr(I).eq.0)then
            mentrd_rate_2d(i,:)=mentrd_rate
            cdd(i,1:jmin(i))=xlamdd
            cdd(i,jmin(i))=0.
! start from dd origin
            zd(i,jmin(i))=0.2
            zdo(i,jmin(i))=0.2
            frh=(zdo(i,jmin(i))-1.)/(-float((jmin(i)-levadj)*(jmin(i)-levadj)) &
                                    +float(jmin(i)*jmin(i)))
            dh=zdo(i,jmin(i))-frh*float(jmin(i)*jmin(i))
            zuhe(i)=zdo(i,jmin(i))
            do ki=jmin(i)-1,jmin(i)-levadj,-1
             cdd(i,ki)=0.
             dz=z_cup(i,ki+1)-z_cup(i,ki)
             mentrd_rate_2d(i,ki)=((frh*float(ki*ki)+dh)/zuhe(i)-1.)/dz
             zuhe(i)=zuhe(i)+mentrd_rate_2d(i,ki)*dz*zuhe(i)
            enddo
! now we know the max zd, for detrainment we will go back to beta at level 1
            kstart=max(kbcon(i),kdet(i))-1
            kstart=min(jmin(i)-levadj,kstart)
            kstart=max(2,kstart)
            if(kstart.lt.jmin(i)-levadj-1)then
              do ki=jmin(i)-levadj-1,kstart,-1
                dz=z_cup(i,ki+1)-z_cup(i,ki)
                mentrd_rate_2d(i,ki)=mentrd_rate
                cdd(i,ki)=xlamdd
                zuhe(i)=zuhe(i)-cdd(i,ki)*dz*zuhe(i)+mentrd_rate_2d(i,ki)*dz*zuhe(i)
              enddo
            endif
            frh=(zuhe(i)-beta)/(float(kstart*kstart)-1.)
            dh=beta-frh
            mentrd_rate_2d(i,kstart)=0.
            do ki=kstart+1,1,-1
             mentrd_rate_2d(i,ki)=0.
             dz=z_cup(i,ki+1)-z_cup(i,ki)
             cdd(i,ki)=max(0.,(1.-(frh*float(ki*ki)+dh)/zuhe(i))/dz)
             zuhe(i)=zuhe(i)-cdd(i,ki)*dz*zuhe(i)
!            if(i.eq.ipr.and.j.eq.jpr)write(0,*)'low cd ',ki,zuhe(i),cdd(i,ki)
            enddo

! now that we have entrainment and detrainment rates, 
! calculate downdraft mass terms
!
            do ki=jmin(i)-1,1,-1
               mentrd_rate=mentrd_rate_2d(i,ki)
               dzo=zo_cup(i,ki+1)-zo_cup(i,ki)
               dd_massentro(i,ki)=mentrd_rate*dzo*zdo(i,ki+1)
               dd_massdetro(i,ki)=cdd(i,ki)*dzo*zdo(i,ki+1)
               zdo(i,ki)=zdo(i,ki+1)+dd_massentro(i,ki)-dd_massdetro(i,ki)
            enddo
! downdraft moist static energy + moisture budget
            dbydo(i,jmin(i))=hcdo(i,jmin(i))-heso_cup(i,jmin(i))
            bud(i)=dbydo(i,jmin(i))*(zo_cup(i,jmin(i)+1)-zo_cup(i,jmin(i)))
            do ki=jmin(i)-1,1,-1
             dzo=zo_cup(i,ki+1)-zo_cup(i,ki)
             hcdo(i,ki)=(hcdo(i,ki+1)*zdo(i,ki+1)                       &
                         -.5*dd_massdetro(i,ki)*hcdo(i,ki+1)+ &
                        dd_massentro(i,ki)*heo(i,ki))   /            &
                        (zdo(i,ki+1)-.5*dd_massdetro(i,ki)+dd_massentro(i,ki))
             dbydo(i,ki)=hcdo(i,ki)-heso_cup(i,ki)
!            if(i.eq.ipr.and.j.eq.jpr)write(0,*)'ki,bud = ',ki,bud(i),hcdo(i,ki)
             bud(i)=bud(i)+dbydo(i,ki)*dzo
            enddo
          endif

        if(bud(i).gt.0)then
          ierr(i)=7
          ierrc(i)='downdraft is not negatively buoyant '
        endif
      enddo
!
!--- calculate moisture properties of downdraft
!
      call cup_dd_moisture_new(ierrc,zdo,hcdo,heso_cup,qcdo,qeso_cup, &
           pwdo,qo_cup,zo_cup,dd_massentro,dd_massdetro,jmin,ierr,gammao_cup, &
           pwevo,bu,qrcdo,qo,heo,tn_cup,1,xl, &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!--- calculate moisture properties of updraft
!
      call cup_up_moisture('deep',ierr,zo_cup,qco,qrco,pwo,pwavo, &
           ccnclean,p_cup,kbcon,ktop,cd,dbyo,clw_all, &
           t_cup,qo,GAMMAo_cup,zuo,qeso_cup,k22,qo_cup,xl,        &
           ZQEXEC,use_excess,ccn,rho,up_massentr,up_massdetr,psum,psumh,&
           autoconv,aeroevap,1,itf,jtf,ktf,j,ipr,jpr, &
           its,ite, jts,jte, kts,kte)
      do k=kts,ktf
      do i=its,itf
         cupclw(i,k)=qrco(i,k)
      enddo
      enddo
!
!--- calculate workfunctions for updrafts
!
      call cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup, &
           kbcon,ktop,ierr,           &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_up_aa0(aa1,zo,zuo,dbyo,GAMMAo_CUP,tn_cup, &
           kbcon,ktop,ierr,           &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
         if(ierr(i).eq.0)then
           if(aa1(i).eq.0.)then
               ierr(i)=17
               ierrc(i)="cloud work function zero"
           endif
         endif
      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


       do i=1,ens4
       axx(:,i)=aa1(:)
       enddo

!
!--- DETERMINE DOWNDRAFT STRENGTH IN TERMS OF WINDSHEAR
!
      call cup_dd_edt(ierr,us,vs,zo,ktop,kbcon,edt,po,pwavo, &
           pwo,ccn,pwevo,edtmax,edtmin,maxens2,edtc,psum,psumh, &
           ccnclean,rho,aeroevap,itf,jtf,ktf,j,ipr,jpr, &
           its,ite, jts,jte, kts,kte)
      do 250 iedt=1,maxens2
        do i=its,itf
         if(ierr(i).eq.0)then
         edt(i)=edtc(i,iedt)
         edto(i)=edtc(i,iedt)
         edtx(i)=edtc(i,iedt)
         if(maxens2.eq.3)then
            edt(i)=edtc(i,3)
            edto(i)=edtc(i,3)
            edtx(i)=edtc(i,3)
         endif
         endif
        enddo
        do k=kts,ktf
        do i=its,itf
           subt_ens(i,k,iedt)=0.
           subq_ens(i,k,iedt)=0.
           dellat_ens(i,k,iedt)=0.
           dellaq_ens(i,k,iedt)=0.
           dellaqc_ens(i,k,iedt)=0.
           pwo_ens(i,k,iedt)=0.
        enddo
        enddo
!
!
!--- change per unit mass that a model cloud would modify the environment
!
!--- 1. in bottom layer
!
      do k=kts,ktf
      do i=its,itf
        dellah(i,k)=0.
        dsubt(i,k)=0.
        dsubh(i,k)=0.
        dellaq(i,k)=0.
        dsubq(i,k)=0.
      enddo
      enddo
!
!----------------------------------------------  cloud level ktop
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level ktop-1
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level k+2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k+1
!
!----------------------------------------------  cloud level k+1
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k
!
!----------------------------------------------  cloud level k
!
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level 3
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 2
!
!----------------------------------------------  cloud level 2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 1

      do i=its,itf
        if(ierr(i).eq.0)then
         dp=100.*(po_cup(i,1)-po_cup(i,2))
         dellah(i,1)=(edto(i)*zdo(i,2)*hcdo(i,2)   &
                     -edto(i)*zdo(i,2)*heo_cup(i,2))*g/dp
         dellaq(i,1)=(edto(i)*zdo(i,2)*qrcdo(i,2)   &
                     -edto(i)*zdo(i,2)*qo_cup(i,2))*g/dp
         dsubt(i,1)=0.
         dsubq(i,1)=0.

         do k=kts+1,ktop(i)
! these three are only used at or near mass detrainment and/or entrainment levels
            entupk=0.
            detupk=0.
            entdoj=0.
! detrainment and entrainment for fowndrafts
            detdo=edto(i)*dd_massdetro(i,k)
            entdo=edto(i)*dd_massentro(i,k)
! entrainment/detrainment for updraft
            entup=up_massentro(i,k)
            detup=up_massdetro(i,k)
! subsidence by downdrafts only
            subin=-zdo(i,k+1)*edto(i)
            subdown=-zdo(i,k)*edto(i)
!
!         SPECIAL LEVELS
!
            if(k.eq.jmin(i))then
               entdoj=edto(i)*zdo(i,k)
            endif
            if(k.eq.ktop(i))then
               detupk=zuo(i,ktop(i))
               subin=0.
               subdown=0.
               detdo=0.
               entdo=0.
               entup=0.
               detup=0.
            endif
            totmas=subin-subdown+detup-entup-entdo+ &
             detdo-entupk-entdoj+detupk+zuo(i,k+1)-zuo(i,k)
!               print *,'*********************',k,totmas
!              write(0,123)k,subin+zuo(i,k+1),subdown-zuo(i,k),detup,entup, &
!                          detdo,entdo,entupk,detupk
!             write(8,*)'totmas = ',k,totmas
            if(abs(totmas).gt.1.e-6)then
               write(0,*)'*********************',i,j,k,totmas
               write(0,123)k,subin,subdown,detup,entup, &
                           detdo,entdo,entupk,detupk
123     formAT(1X,i2,8E12.4)
!        call wrf_error_fatal ( 'totmas .gt.1.e-6' )
            endif
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
            dellah(i,k)=(detup*.5*(HCo(i,K+1)+HCo(i,K)) &
                    +detdo*.5*(HCDo(i,K+1)+HCDo(i,K)) &
                    -entup*heo(i,k) &
                    -entdo*heo(i,k) &
                    +subin*heo_cup(i,k+1) &
                    -subdown*heo_cup(i,k) &
                    +detupk*(hco(i,ktop(i))-heo_cup(i,ktop(i)))    &
                    -entupk*heo_cup(i,k22(i)) &
                    -entdoj*heo_cup(i,jmin(i)) &
                     )*g/dp
            dellaq(i,k)=(detup*.5*(qco(i,K+1)+qco(i,K)-qrco(i,k+1)-qrco(i,k)) &
                    +detdo*.5*(qrcdo(i,K+1)+qrcdo(i,K)) &
                    -entup*qo(i,k) &
                    -entdo*qo(i,k) &
                    +subin*qo_cup(i,k+1) &
                    -subdown*qo_cup(i,k) &
                    +detupk*(qco(i,ktop(i))-qrco(i,ktop(i))-qo_cup(i,ktop(i)))    &
                    -entupk*qo_cup(i,k22(i)) &
                    -entdoj*qo_cup(i,jmin(i)) &
                     )*g/dp
!
! updraft subsidence only
!
           if(k.lt.ktop(i))then
             dsubt(i,k)=(zuo(i,k+1)*heo_cup(i,k+1) &
                    -zuo(i,k)*heo_cup(i,k))*g/dp
             dsubq(i,k)=(zuo(i,k+1)*qo_cup(i,k+1) &
                    -zuo(i,k)*qo_cup(i,k))*g/dp
           endif
!
       enddo   ! k

        endif
      enddo
!
!-- take out cloud liquid water for detrainment
!
      do k=kts,ktf-1
      do i=its,itf
       dellaqc(i,k)=0.
       if(ierr(i).eq.0)then
         if(k.eq.ktop(i)-0)dellaqc(i,k)= &
                      .01*zuo(i,ktop(i))*qrco(i,ktop(i))* &
                      9.81/(po_cup(i,k)-po_cup(i,k+1))
         if(k.lt.ktop(i).and.k.gt.kbcon(i))then
           dz=zo_cup(i,k+1)-zo_cup(i,k)
           dellaqc(i,k)=.01*9.81*up_massdetro(i,k)*.5*(qrco(i,k)+qrco(i,k+1))/ &
                        (po_cup(i,k)-po_cup(i,k+1))
         endif
         dellaqc(i,k)=max(0.,dellaqc(i,k))
       endif
      enddo
      enddo
!
!--- using dellas, calculate changed environmental profiles
!
      mbdt=mbdt_ens(1)
      do i=its,itf
      xaa0_ens(i,:)=0.
      enddo

      do k=kts,ktf
      do i=its,itf
         dellat(i,k)=0.
         if(ierr(i).eq.0)then
!           if(i.eq.ipr.and.j.eq.jpr.and.k.eq.kts)write(0,*)'mbdt = ',mbdt,mbdt_ens,dtime
            dsubh(i,k)=dsubt(i,k)
            XHE(I,K)=(dsubt(i,k)+DELLAH(I,K))*MBDT+HEO(I,K)
            XQ(I,K)=(dsubq(i,k)+DELLAQ(I,K)+dellaqc(i,k))*MBDT+QO(I,K)
            DELLAT(I,K)=(1./cp)*(DELLAH(I,K)-xl*DELLAQ(I,K))
            dSUBT(I,K)=(1./cp)*(dsubt(i,k)-xl*dsubq(i,k))
            XT(I,K)= (DELLAT(I,K)+dsubt(i,k)-dellaqc(i,k)*xl/cp)*MBDT+TN(I,K)
            IF(XQ(I,K).LE.0.)XQ(I,K)=1.E-08
         ENDIF
      enddo
      enddo
      do i=its,itf
      if(ierr(i).eq.0)then
      xhkb(i)=hkbo(i)+(dsubh(i,k22(i))+DELLAH(I,K22(i)))*MBDT
      XHE(I,ktf)=HEO(I,ktf)
      XQ(I,ktf)=QO(I,ktf)
      XT(I,ktf)=TN(I,ktf)
      IF(XQ(I,ktf).LE.0.)XQ(I,ktf)=1.E-08
      endif
      enddo
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(xz,xqes,xhe,xhes,xt,xq,po,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!--- environmental values on cloud levels
!
      call cup_env_clev(xt,xqes,xq,xhe,xhes,xz,po,xqes_cup,xq_cup, &
           xhe_cup,xhes_cup,xz_cup,po_cup,gamma_cup,xt_cup,psur,   &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!
!**************************** static control
!
!--- moist static energy inside cloud
!
!     do i=its,itf
!       if(ierr(i).eq.0)then
!         xhkb(i)=xhe(i,k22(i))
!       endif
!     enddo
      do k=kts,ktf
      do i=its,itf
         xhc(i,k)=0.
         xDBY(I,K)=0.
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
!        if(use_excess == 2) then
!            k1=max(1,k22(i)-1)
!            k2=max(1,min(kbcon(i)-1,k22(i)+1))
!            k1=1
!            k2=k22(i)+1
!            xhkb(i) =sum(xhe_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
!        else if(use_excess <= 1) then
!            xhkb(i)=xhe_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))

!        endif
         do k=1,kbcon(i)-1
            xhc(i,k)=xhkb(i)
         enddo
         k=kbcon(i)
         xhc(i,k)=xhkb(i)
         xDBY(I,Kbcon(i))=xHkb(I)-xHES_cup(I,K)
        endif !ierr
      enddo
!
!
      do i=its,itf
      if(ierr(i).eq.0)then
      xzu(i,:)=zuo(i,:)
      do k=kbcon(i)+1,ktop(i)
       xhc(i,k)=(xhc(i,k-1)*xzu(i,k-1)-.5*up_massdetro(i,k-1)*xhc(i,k-1)+ &
                         up_massentro(i,k-1)*xhe(i,k-1))   /            &
                         (xzu(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
       xdby(i,k)=xhc(i,k)-xhes_cup(i,k)
      enddo
      do k=ktop(i)+1,ktf
           xHC(i,K)=xhes_cup(i,k)
           xDBY(I,K)=0.
           xzu(i,k)=0.
      enddo
      endif
      enddo

!
!--- workfunctions for updraft
!
      call cup_up_aa0(xaa0,xz,xzu,xdby,GAMMA_CUP,xt_cup, &
           kbcon,ktop,ierr,           &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      do 200 nens=1,maxens
      do i=its,itf 
         if(ierr(i).eq.0)then
           xaa0_ens(i,nens)=xaa0(i)
           nall=(iens-1)*maxens3*maxens*maxens2 &
                +(iedt-1)*maxens*maxens3 &
                +(nens-1)*maxens3
           do k=kts,ktf
              if(k.le.ktop(i))then
                 do nens3=1,maxens3
                 if(nens3.eq.7)then
!--- b=0
                 pr_ens(i,j,nall+nens3)=pr_ens(i,j,nall+nens3)  &
!                                +edto(i)*pwdo(i,k)             &
                                    +pwo(i,k) 
!--- b=beta
                 else if(nens3.eq.8)then
                 pr_ens(i,j,nall+nens3)=pr_ens(i,j,nall+nens3)+ &
                                    pwo(i,k)
!--- b=beta/2
                 else if(nens3.eq.9)then
                 pr_ens(i,j,nall+nens3)=pr_ens(i,j,nall+nens3)  &
!                                +.5*edto(i)*pwdo(i,k)          &
                                 +  pwo(i,k)
                 else
                 pr_ens(i,j,nall+nens3)=pr_ens(i,j,nall+nens3)+ &
                                    pwo(i,k) ! +edto(i)*pwdo(i,k)
                 endif
                 enddo
              endif
           enddo
         if(pr_ens(i,j,nall+7).lt.1.e-6)then
            ierr(i)=18
            ierrc(i)="total normalized condensate too small"
!           if(i.eq.ipr.and.j.eq.jpr)write(0,*)ierr(i),ierrc(i)
            do nens3=1,maxens3
               pr_ens(i,j,nall+nens3)=0.
            enddo
         endif
         do nens3=1,maxens3
           if(pr_ens(i,j,nall+nens3).lt.1.e-4)then
            pr_ens(i,j,nall+nens3)=0.
           endif
         enddo
         endif
!     if(i.eq.ipr.and.j.eq.jpr)write(0,*)'ierrc = ',ierr(i),ierrc(i)
      enddo
 200  continue
!
!--- LARGE SCALE FORCING
!
!
!------- CHECK wether aa0 should have been zero, assuming this 
!        ensemble is chosen
!
!
      do i=its,itf
         ierr2(i)=ierr(i)
         ierr3(i)=ierr(i)
         k22x(i)=k22(i)
      enddo
       if(maxens.gt.0)then
!     CALL cup_MAXIMI(HEO_CUP,3,KBMAX,K22x,ierr, &
!          itf,jtf,ktf, &
!          its,ite, jts,jte, kts,kte)
      call cup_kbcon(ierrc,cap_max_increment,2,k22x,kbconx,heo_cup, &
           heso_cup,hkbo,ierr2,kbmax,po_cup,cap_max, &
           xl,cp,ztexec,zqexec,use_excess,       &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_kbcon(ierrc,cap_max_increment,3,k22x,kbconx,heo_cup, &
           heso_cup,hkbo,ierr3,kbmax,po_cup,cap_max, &
           xl,cp,ztexec,zqexec,use_excess,       &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      endif
!
!--- calculate cloud base mass flux
!

      call cup_forcing_ens_3d(closure_n,xland1,aa0,aa1,xaa0_ens,mbdt_ens,dtime,   &
           ierr,ierr2,ierr3,xf_ens,j,'deeps',axx,                 &
           maxens,iens,iedt,maxens2,maxens3,mconv,            &
           po_cup,ktop,omeg,zdo,k22,zuo,pr_ens,edto,kbcon,    &
           ensdim,ichoice,     &
           ipr,jpr,itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte,ens4,ktau)
!
      do k=kts,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
           subt_ens(i,k,iedt)=dsubt(i,k)
           subq_ens(i,k,iedt)=dsubq(i,k)
           dellat_ens(i,k,iedt)=dellat(i,k)
           dellaq_ens(i,k,iedt)=dellaq(i,k)
           dellaqc_ens(i,k,iedt)=dellaqc(i,k)
           pwo_ens(i,k,iedt)=pwo(i,k)+edt(i)*pwdo(i,k)
        else 
           subt_ens(i,k,iedt)=0.
           subq_ens(i,k,iedt)=0.
           dellat_ens(i,k,iedt)=0.
           dellaq_ens(i,k,iedt)=0.
           dellaqc_ens(i,k,iedt)=0.
           pwo_ens(i,k,iedt)=0.
        endif
      enddo
      enddo
 250  continue
!
!--- FEEDBACK
!
       call cup_output_ens_3d(xf_ens,ierr,dellat_ens,dellaq_ens, &
            dellaqc_ens,subt_ens,subq_ens,subt,subq,outt,     &
            outq,outqc,zuo,sub_mas,pre,pwo_ens,xmb,ktop,      &
            j,'deep',maxens2,maxens,iens,ierr2,ierr3,         &
            pr_ens,maxens3,ensdim,                    &
            sig,APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                &
            APR_CAPMA,APR_CAPME,APR_CAPMI,closure_n,xland1,   &
            weight_GR,weight_W,weight_MC,weight_ST,weight_AS,training, &
            ipr,jpr,itf,jtf,ktf,                        &
            its,ite, jts,jte, kts,kte  )
      k=1
      do i=its,itf
          if(ierr(i).eq.0) PRE(I)=MAX(PRE(I),0.)
      enddo
!
!---------------------------done------------------------------
!

   END SUBROUTINE CUP_gf


   SUBROUTINE cup_dd_edt(ierr,us,vs,z,ktop,kbcon,edt,p,pwav, &
              pw,ccn,pwev,edtmax,edtmin,maxens2,edtc,psum2,psumh, &
              ccnclean,rho,aeroevap,itf,jtf,ktf,j,ipr,jpr,          &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        j,ipr,jpr,aeroevap,itf,jtf,ktf,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        maxens2
  !
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        rho,us,vs,z,p,pw
     real,    dimension (its:ite,1:maxens2)                            &
        ,intent (out  )                   ::                           &
        edtc
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        edt
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        pwav,pwev,ccn,psum2,psumh
     real                                                              &
        ,intent (in   )                   ::                           &
        ccnclean,edtmax,edtmin
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        ktop,kbcon
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
!
!  local variables in this routine
!

     integer i,k,kk
     real    einc,pef,pefb,prezk,zkbc
     real,    dimension (its:ite)         ::                           &
      vshear,sdp,vws
     real :: prop_c,pefc,aeroadd,alpha3,beta3,rhoc
     prop_c=8. !10.386
     alpha3 = 1.9
     beta3  = -1.13
     pefc=0.

!
!--- DETERMINE DOWNDRAFT STRENGTH IN TERMS OF WINDSHEAR
!
! */ calculate an average wind shear over the depth of the cloud
!
       do i=its,itf
        edt(i)=0.
        vws(i)=0.
        sdp(i)=0.
        vshear(i)=0.
       enddo
       do k=1,maxens2
       do i=its,itf
        edtc(i,k)=0.
       enddo
       enddo
       do kk = kts,ktf-1
         do 62 i=its,itf
          IF(ierr(i).ne.0)GO TO 62
          if (kk .le. min0(ktop(i),ktf) .and. kk .ge. kbcon(i)) then
             vws(i) = vws(i)+ &
              (abs((us(i,kk+1)-us(i,kk))/(z(i,kk+1)-z(i,kk))) &
          +   abs((vs(i,kk+1)-vs(i,kk))/(z(i,kk+1)-z(i,kk)))) * &
              (p(i,kk) - p(i,kk+1))
            sdp(i) = sdp(i) + p(i,kk) - p(i,kk+1)
          endif
          if (kk .eq. ktf-1)vshear(i) = 1.e3 * vws(i) / sdp(i)
   62   continue
       end do
      do i=its,itf
         IF(ierr(i).eq.0)then
            pef=(1.591-.639*VSHEAR(I)+.0953*(VSHEAR(I)**2) &
               -.00496*(VSHEAR(I)**3))
            if(pef.gt.0.9)pef=0.9
            if(pef.lt.0.1)pef=0.1
!
!--- cloud base precip efficiency
!
            zkbc=z(i,kbcon(i))*3.281e-3
            prezk=.02
            if(zkbc.gt.3.)then
               prezk=.96729352+zkbc*(-.70034167+zkbc*(.162179896+zkbc &
               *(- 1.2569798E-2+zkbc*(4.2772E-4-zkbc*5.44E-6))))
            endif
            if(zkbc.gt.25)then
               prezk=2.4
            endif
            pefb=1./(1.+prezk)
            if(pefb.gt.0.9)pefb=0.9
            if(pefb.lt.0.1)pefb=0.1
            EDT(I)=1.-.5*(pefb+pef)
            if(aeroevap.gt.1)then
               aeroadd=(ccnclean**beta3)*((psumh(i))**(alpha3-1)) !*1.e6
!              if(i.eq.ipr.and.j.eq.jpr)write(0,*)'edt',ccnclean,psumh(i),aeroadd
!              prop_c=.9/aeroadd
               prop_c=.5*(pefb+pef)/aeroadd
               aeroadd=(ccn(i)**beta3)*((psum2(i))**(alpha3-1)) !*1.e6
!              if(i.eq.ipr.and.j.eq.jpr)write(0,*)'edt',ccn(i),psum2(i),aeroadd,prop_c
               aeroadd=prop_c*aeroadd
               pefc=aeroadd
               if(pefc.gt.0.9)pefc=0.9
               if(pefc.lt.0.1)pefc=0.1
               EDT(I)=1.-pefc
               if(aeroevap.eq.2)EDT(I)=1.-.25*(pefb+pef+2.*pefc)
            endif


!--- edt here is 1-precipeff!
            einc=.2*edt(i)
            do k=1,maxens2
                edtc(i,k)=edt(i)+float(k-2)*einc
            enddo
         endif
      enddo
      do i=its,itf
         IF(ierr(i).eq.0)then
            do k=1,maxens2
               EDTC(I,K)=-EDTC(I,K)*PWAV(I)/PWEV(I)
               IF(EDTC(I,K).GT.edtmax)EDTC(I,K)=edtmax
               IF(EDTC(I,K).LT.edtmin)EDTC(I,K)=edtmin
            enddo
         endif
      enddo

   END SUBROUTINE cup_dd_edt


   SUBROUTINE cup_dd_moisture_new(ierrc,zd,hcd,hes_cup,qcd,qes_cup,    &
              pwd,q_cup,z_cup,dd_massentr,dd_massdetr,jmin,ierr,            &
              gamma_cup,pwev,bu,qrcd,                        &
              q,he,t_cup,iloop,xl,           &
              itf,jtf,ktf,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
                                  itf,jtf,ktf,           &
                                  its,ite, jts,jte, kts,kte
  ! cdd= detrainment function 
  ! q = environmental q on model levels
  ! q_cup = environmental q on model cloud levels
  ! qes_cup = saturation q on model cloud levels
  ! hes_cup = saturation h on model cloud levels
  ! hcd = h in model cloud
  ! bu = buoancy term
  ! zd = normalized downdraft mass flux
  ! gamma_cup = gamma on model cloud levels
  ! mentr_rate = entrainment rate
  ! qcd = cloud q (including liquid water) after entrainment
  ! qrch = saturation q in cloud
  ! pwd = evaporate at that level
  ! pwev = total normalized integrated evaoprate (I2)
  ! entr= entrainment rate 
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        zd,t_cup,hes_cup,hcd,qes_cup,q_cup,z_cup,                      &
        dd_massentr,dd_massdetr,gamma_cup,q,he 
     real                                                              &
        ,intent (in   )                   ::                           &
        xl
     integer                                                           &
        ,intent (in   )                   ::                           &
        iloop
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        jmin
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        qcd,qrcd,pwd
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pwev,bu
     character*50 :: ierrc(its:ite)
!
!  local variables in this routine
!

     integer                              ::                           &
        i,k,ki
     real                                 ::                           &
        dh,dz,dqeva

      do i=its,itf
         bu(i)=0.
         pwev(i)=0.
      enddo
      do k=kts,ktf
      do i=its,itf
         qcd(i,k)=0.
         qrcd(i,k)=0.
         pwd(i,k)=0.
      enddo
      enddo
!
!
!
      do 100 i=its,itf
      IF(ierr(I).eq.0)then
      k=jmin(i)
      DZ=Z_cup(i,K+1)-Z_cup(i,K)
      qcd(i,k)=q_cup(i,k)
      DH=HCD(I,k)-HES_cup(I,K)
      if(dh.lt.0)then
        QRCD(I,K)=(qes_cup(i,k)+(1./XL)*(GAMMA_cup(i,k) &
                  /(1.+GAMMA_cup(i,k)))*DH)
        else
          qrcd(i,k)=qes_cup(i,k)
        endif
      pwd(i,jmin(i))=zd(i,jmin(i))*min(0.,qcd(i,k)-qrcd(i,k))
      qcd(i,k)=qrcd(i,k)
      pwev(i)=pwev(i)+pwd(i,jmin(i))
!
      bu(i)=dz*dh
      do ki=jmin(i)-1,1,-1
         DZ=Z_cup(i,Ki+1)-Z_cup(i,Ki)
         qcd(i,ki)=(qcd(i,ki+1)*zd(i,ki+1)                          &
                  -.5*dd_massdetr(i,ki)*qcd(i,ki+1)+ &
                  dd_massentr(i,ki)*q(i,ki))   /            &
                  (zd(i,ki+1)-.5*dd_massdetr(i,ki)+dd_massentr(i,ki))
!        write(0,*)'qcd in dd_moi = ',qcd(i,ki)

!
!--- to be negatively buoyant, hcd should be smaller than hes!
!--- ideally, dh should be negative till dd hits ground, but that is not always
!--- the case
!
         DH=HCD(I,ki)-HES_cup(I,Ki)
         bu(i)=bu(i)+dz*dh
         QRCD(I,Ki)=qes_cup(i,ki)+(1./XL)*(GAMMA_cup(i,ki) &
                  /(1.+GAMMA_cup(i,ki)))*DH
         dqeva=qcd(i,ki)-qrcd(i,ki)
         if(dqeva.gt.0.)then
          dqeva=0.
          qrcd(i,ki)=qcd(i,ki)
         endif
         pwd(i,ki)=zd(i,ki)*dqeva
         qcd(i,ki)=qrcd(i,ki)
         pwev(i)=pwev(i)+pwd(i,ki)
!        if(iloop.eq.1.and.i.eq.102.and.j.eq.62)then
!         print *,'in cup_dd_moi ', hcd(i,ki),HES_cup(I,Ki),dh,dqeva
!        endif
      enddo
!
!--- end loop over i
       if(pwev(I).eq.0.and.iloop.eq.1)then
!        print *,'problem with buoy in cup_dd_moisture',i
         ierr(i)=7
         ierrc(i)="problem with buoy in cup_dd_moisture"
       endif
       if(BU(I).GE.0.and.iloop.eq.1)then
!        print *,'problem with buoy in cup_dd_moisture',i
         ierr(i)=7
         ierrc(i)="problem2 with buoy in cup_dd_moisture"
       endif
      endif
100    continue

   END SUBROUTINE cup_dd_moisture_new

   SUBROUTINE cup_env(z,qes,he,hes,t,q,p,z1,                 &
              psur,ierr,tcrit,itest,xl,cp,                   &
              itf,jtf,ktf,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,jtf,ktf,           &
        its,ite, jts,jte, kts,kte
  !
  ! ierr error value, maybe modified in this routine
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! tv          = environmental virtual temp
  ! p           = environmental pressure
  ! z           = environmental heights
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! 
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        p,t,q
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        he,hes,qes
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
        z
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        psur,z1
     real                                                              &
        ,intent (in   )                   ::                           &
        xl,cp
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     integer                                                           &
        ,intent (in   )                   ::                           &
        itest
!
!  local variables in this routine
!

     integer                              ::                           &
       i,k,iph
      real, dimension (1:2) :: AE,BE,HT
      real, dimension (its:ite,kts:kte) :: tv
      real :: tcrit,e,tvbar
!      real, external :: satvap
!      real :: satvap


      HT(1)=XL/CP
      HT(2)=2.834E6/CP
      BE(1)=.622*HT(1)/.286
      AE(1)=BE(1)/273.+ALOG(610.71)
      BE(2)=.622*HT(2)/.286
      AE(2)=BE(2)/273.+ALOG(610.71)
!      print *, 'TCRIT = ', tcrit,its,ite
      DO k=kts,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
!Csgb - IPH is for phase, dependent on TCRIT (water or ice)
        IPH=1
        IF(T(I,K).LE.TCRIT)IPH=2
!       print *, 'AE(IPH),BE(IPH) = ',AE(IPH),BE(IPH),AE(IPH)-BE(IPH),T(i,k),i,k
!       E=EXP(AE(IPH)-BE(IPH)/T(I,K))
!       print *, 'P, E = ', P(I,K), E
!       QES(I,K)=.622*E/(100.*P(I,K)-E)
        e=satvap(t(i,k))
        qes(i,k)=0.622*e/max(1.e-8,(p(i,k)-e))
        IF(QES(I,K).LE.1.E-08)QES(I,K)=1.E-08
        IF(QES(I,K).LT.Q(I,K))QES(I,K)=Q(I,K)
!       IF(Q(I,K).GT.QES(I,K))Q(I,K)=QES(I,K)
        TV(I,K)=T(I,K)+.608*Q(I,K)*T(I,K)
        endif
      enddo
      enddo
!
!--- z's are calculated with changed h's and q's and t's
!--- if itest=2
!
      if(itest.eq.1 .or. itest.eq.0)then
         do i=its,itf
           if(ierr(i).eq.0)then
             Z(I,1)=max(0.,Z1(I))-(ALOG(P(I,1))- &
                 ALOG(PSUR(I)))*287.*TV(I,1)/9.81
           endif
         enddo

! --- calculate heights
         DO K=kts+1,ktf
         do i=its,itf
           if(ierr(i).eq.0)then
              TVBAR=.5*TV(I,K)+.5*TV(I,K-1)
              Z(I,K)=Z(I,K-1)-(ALOG(P(I,K))- &
               ALOG(P(I,K-1)))*287.*TVBAR/9.81
           endif
         enddo
         enddo
      else if(itest.eq.2)then
         do k=kts,ktf
         do i=its,itf
           if(ierr(i).eq.0)then
             z(i,k)=(he(i,k)-1004.*t(i,k)-2.5e6*q(i,k))/9.81
             z(i,k)=max(1.e-3,z(i,k))
           endif
         enddo
         enddo
      else if(itest.eq.-1)then
      endif
!
!--- calculate moist static energy - HE
!    saturated moist static energy - HES
!
       DO k=kts,ktf
       do i=its,itf
         if(ierr(i).eq.0)then
         if(itest.le.0)HE(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*Q(I,K)
         HES(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*QES(I,K)
         IF(HE(I,K).GE.HES(I,K))HE(I,K)=HES(I,K)
         endif
      enddo
      enddo

   END SUBROUTINE cup_env


   SUBROUTINE cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,   &
              he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
              ierr,z1,xl,rv,cp,                                &
              itf,jtf,ktf,                       &
              its,ite, jts,jte, kts,kte                       )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,jtf,ktf,           &
        its,ite, jts,jte, kts,kte
  !
  ! ierr error value, maybe modified in this routine
  ! q           = environmental mixing ratio
  ! q_cup       = environmental mixing ratio on cloud levels
  ! qes         = environmental saturation mixing ratio
  ! qes_cup     = environmental saturation mixing ratio on cloud levels
  ! t           = environmental temp
  ! t_cup       = environmental temp on cloud levels
  ! p           = environmental pressure
  ! p_cup       = environmental pressure on cloud levels
  ! z           = environmental heights
  ! z_cup       = environmental heights on cloud levels
  ! he          = environmental moist static energy
  ! he_cup      = environmental moist static energy on cloud levels
  ! hes         = environmental saturation moist static energy
  ! hes_cup     = environmental saturation moist static energy on cloud levels
  ! gamma_cup   = gamma on cloud levels
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! 
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        qes,q,he,hes,z,p,t
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        psur,z1
     real                                                              &
        ,intent (in   )                   ::                           &
        xl,rv,cp
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
!
!  local variables in this routine
!

     integer                              ::                           &
       i,k


      do k=kts,ktf
      do i=its,itf
        qes_cup(i,k)=0.
        q_cup(i,k)=0.
        hes_cup(i,k)=0.
        he_cup(i,k)=0.
        z_cup(i,k)=0.
        p_cup(i,k)=0.
        t_cup(i,k)=0.
        gamma_cup(i,k)=0.
      enddo
      enddo
      do k=kts+1,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
        qes_cup(i,k)=.5*(qes(i,k-1)+qes(i,k))
        q_cup(i,k)=.5*(q(i,k-1)+q(i,k))
        hes_cup(i,k)=.5*(hes(i,k-1)+hes(i,k))
        he_cup(i,k)=.5*(he(i,k-1)+he(i,k))
        if(he_cup(i,k).gt.hes_cup(i,k))he_cup(i,k)=hes_cup(i,k)
        z_cup(i,k)=.5*(z(i,k-1)+z(i,k))
        p_cup(i,k)=.5*(p(i,k-1)+p(i,k))
        t_cup(i,k)=.5*(t(i,k-1)+t(i,k))
        gamma_cup(i,k)=(xl/cp)*(xl/(rv*t_cup(i,k) &
                       *t_cup(i,k)))*qes_cup(i,k)
        endif
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
        qes_cup(i,1)=qes(i,1)
        q_cup(i,1)=q(i,1)
!       hes_cup(i,1)=hes(i,1)
!       he_cup(i,1)=he(i,1)
        hes_cup(i,1)=9.81*z1(i)+1004.*t(i,1)+2.5e6*qes(i,1)
        he_cup(i,1)=9.81*z1(i)+1004.*t(i,1)+2.5e6*q(i,1)
        z_cup(i,1)=.5*(z(i,1)+z1(i))
        p_cup(i,1)=.5*(p(i,1)+psur(i))
        z_cup(i,1)=z1(i)
        p_cup(i,1)=psur(i)
        t_cup(i,1)=t(i,1)
        gamma_cup(i,1)=xl/cp*(xl/(rv*t_cup(i,1) &
                       *t_cup(i,1)))*qes_cup(i,1)
        endif
      enddo

   END SUBROUTINE cup_env_clev


   SUBROUTINE cup_forcing_ens_3d(closure_n,xland,aa0,aa1,xaa0,mbdt,dtime,ierr,ierr2,ierr3,&
              xf_ens,j,name,axx,maxens,iens,iedt,maxens2,maxens3,mconv,    &
              p_cup,ktop,omeg,zd,k22,zu,pr_ens,edt,kbcon,      &
              ensdim,icoic,            &
              ipr,jpr,itf,jtf,ktf,               &
              its,ite, jts,jte, kts,kte,ens4,ktau                )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        ipr,jpr,itf,jtf,ktf,           &
        its,ite, jts,jte, kts,kte,ens4,ktau
     integer, intent (in   )              ::                           &
        j,ensdim,maxens,iens,iedt,maxens2,maxens3
  !
  ! ierr error value, maybe modified in this routine
  ! pr_ens = precipitation ensemble
  ! xf_ens = mass flux ensembles
  ! massfln = downdraft mass flux ensembles used in next timestep
  ! omeg = omega from large scale model
  ! mconv = moisture convergence from large scale model
  ! zd      = downdraft normalized mass flux
  ! zu      = updraft normalized mass flux
  ! aa0     = cloud work function without forcing effects
  ! aa1     = cloud work function with forcing effects
  ! xaa0    = cloud work function with cloud effects (ensemble dependent)
  ! edt     = epsilon
  ! dir     = "storm motion"
  ! mbdt    = arbitrary numerical parameter
  ! dtime   = dt over which forcing is applied
  ! iact_gr_old = flag to tell where convection was active
  ! kbcon       = LFC of parcel from k22
  ! k22         = updraft originating level
  ! icoic       = flag if only want one closure (usually set to zero!)
  ! name        = deep or shallow convection flag
  !
     real,    dimension (its:ite,jts:jte,1:ensdim)                     &
        ,intent (inout)                   ::                           &
        pr_ens
     real,    dimension (its:ite,jts:jte,1:ensdim)                     &
        ,intent (out  )                   ::                           &
        xf_ens
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        zd,zu,p_cup
     real,    dimension (its:ite,kts:kte,1:ens4)                              &
        ,intent (in   )                   ::                           &
        omeg
     real,    dimension (its:ite,1:maxens)                             &
        ,intent (in   )                   ::                           &
        xaa0
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        aa1,edt,xland
     real,    dimension (its:ite,1:ens4)                                      &
        ,intent (in   )                   ::                           &
        mconv,axx
     real,    dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        aa0,closure_n
     real,    dimension (1:maxens)                                     &
        ,intent (in   )                   ::                           &
        mbdt
     real                                                              &
        ,intent (in   )                   ::                           &
        dtime
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        k22,kbcon,ktop
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr,ierr2,ierr3
     integer                                                           &
        ,intent (in   )                   ::                           &
        icoic
      character *(*), intent (in)         ::                           &
       name
!
!  local variables in this routine
!

     real,    dimension (1:maxens3)       ::                           &
       xff_ens3
     real,    dimension (1:maxens)        ::                           &
       xk
     integer                              ::                           &
       i,k,nall,n,ne,nens,nens3,iresult,iresultd,iresulte,mkxcrt,kclim
     parameter (mkxcrt=15)
     real                                 ::                           &
       fens4,a1,massfld,a_ave,xff0,xff00,xxx,xomg,aclim1,aclim2,aclim3,aclim4
     real,    dimension(1:mkxcrt)         ::                           &
       pcrit,acrit,acritt

     integer :: nall2,ixxx,irandom
     integer,  dimension (8) :: seed
     real, dimension (its:ite) :: ens_adj


      DATA PCRIT/850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,    &
                 350.,300.,250.,200.,150./
      DATA ACRIT/.0633,.0445,.0553,.0664,.075,.1082,.1521,.2216,       &
                 .3151,.3677,.41,.5255,.7663,1.1686,1.6851/
!  GDAS DERIVED ACRIT
      DATA ACRITT/.203,.515,.521,.566,.625,.665,.659,.688,             &
                  .743,.813,.886,.947,1.138,1.377,1.896/

!
       ens_adj=1.
       seed=0
       do i=its,itf
        if(ierr(i).eq.0)then
          seed(1)=int(aa0(i))
          seed(2)=int(aa1(i))
          exit
        endif
       enddo

       nens=0
       irandom=0
       fens4=float(ens4)

!--- LARGE SCALE FORCING
!
       DO 100 i=its,itf
          if(name.eq.'deeps'.and.ierr(i).gt.995)then
           aa0(i)=0.
           ierr(i)=0
          endif
          IF(ierr(i).eq.0)then
          ens_adj(i)=1.
          if(ierr2(i).gt.0.and.ierr3(i).eq.0)ens_adj(i)=0. ! 2./3.
          if(ierr2(i).gt.0.and.ierr3(i).gt.0)ens_adj(i)=0.
!
!---
!
             if(name.eq.'deeps')then
!
                a_ave=0.
                do ne=1,ens4
                  a_ave=a_ave+axx(i,ne)
!               if(i.eq.ipr.and.j.eq.jpr)write(0,*)'in forcing, a_ave,axx(i,ne) = ',a_ave,axx(i,ne),maxens,xland(i)
                enddo
                a_ave=max(0.,a_ave/fens4)
                a_ave=min(a_ave,aa1(i))
                a_ave=max(0.,a_ave)
                do ne=1,16
                  xff_ens3(ne)=0.
                enddo
                xff0= (AA1(I)-AA0(I))/DTIME
                xff_ens3(1)=max(0.,(AA1(I)-AA0(I))/dtime)
                xff_ens3(2)=max(0.,(a_ave-AA0(I))/dtime)

!               if(i.eq.ipr.and.j.eq.jpr)write(0,*)AA1(I),AA0(I),xff_ens3(1),xff_ens3(2),dtime
                if(irandom.eq.1)then
                   call random_number (xxx)
                   ixxx=min(ens4,max(1,int(fens4*xxx+1.e-8)))
                   xff_ens3(3)=max(0.,(axx(i,ixxx)-AA0(I))/dtime)
                   call random_number (xxx)
                   ixxx=min(ens4,max(1,int(fens4*xxx+1.e-8)))
                   xff_ens3(13)=max(0.,(axx(i,ixxx)-AA0(I))/dtime)
                else
                   xff_ens3(3)=max(0.,(AA1(I)-AA0(I))/dtime)
                   xff_ens3(13)=max(0.,(AA1(I)-AA0(I))/dtime)
                endif
!   
!--- more original Arakawa-Schubert (climatologic value of aa0)
!
!
!--- omeg is in bar/s, mconv done with omeg in Pa/s
!     more like Brown (1979), or Frank-Cohen (199?)
!
                xff_ens3(14)=0.
                do ne=1,ens4
                  xff_ens3(14)=xff_ens3(14)-omeg(i,k22(i),ne)/(fens4*9.81)
                enddo
                if(xff_ens3(14).lt.0.)xff_ens3(14)=0.
                xff_ens3(5)=0.
                do ne=1,ens4
                  xff_ens3(5)=xff_ens3(5)-omeg(i,kbcon(i),ne)/(fens4*9.81)
                enddo
                if(xff_ens3(5).lt.0.)xff_ens3(5)=0.
!  
! minimum below kbcon
!
                   xff_ens3(4)=-omeg(i,2,1)/9.81
                   do k=2,kbcon(i)-1
                   do ne=1,ens4
                     xomg=-omeg(i,k,ne)/9.81
                     if(xomg.lt.xff_ens3(4))xff_ens3(4)=xomg
                   enddo
                   enddo
                   if(xff_ens3(4).lt.0.)xff_ens3(4)=0.
!
! max below kbcon
                   xff_ens3(6)=-omeg(i,2,1)/9.81
                   do k=2,kbcon(i)-1
                   do ne=1,ens4
                     xomg=-omeg(i,k,ne)/9.81
                     if(xomg.gt.xff_ens3(6))xff_ens3(6)=xomg
                   enddo
                   enddo
                   if(xff_ens3(6).lt.0.)xff_ens3(6)=0.
                   xff_ens3(5)=xff_ens3(6)
                   xff_ens3(4)=xff_ens3(6)
!               if(i.eq.ipr.and.j.eq.jpr)write(0,*)xff_ens3(4),xff_ens3(5)
!
!--- more like Krishnamurti et al.; pick max and average values
!
                xff_ens3(7)=mconv(i,1)
                xff_ens3(8)=mconv(i,1)
                xff_ens3(9)=mconv(i,1)
                if(ens4.gt.1)then
                   do ne=2,ens4
                      if (mconv(i,ne).gt.xff_ens3(7))xff_ens3(7)=mconv(i,ne)
                   enddo
                   do ne=2,ens4
                      if (mconv(i,ne).lt.xff_ens3(8))xff_ens3(8)=mconv(i,ne)
                   enddo
                   do ne=2,ens4
                      xff_ens3(9)=xff_ens3(9)+mconv(i,ne)
                   enddo
                   xff_ens3(9)=xff_ens3(9)/fens4
                endif
!               if(i.eq.ipr.and.j.eq.jpr)write(0,*)xff_ens3(7),xff_ens3(8)
!
                if(irandom.eq.1)then
                   call random_number (xxx)
                   ixxx=min(ens4,max(1,int(fens4*xxx+1.e-8)))
                   xff_ens3(15)=mconv(i,ixxx)
                else
                   xff_ens3(15)=mconv(i,1)
                endif
!
!--- more like Fritsch Chappel or Kain Fritsch (plus triggers)
!
                xff_ens3(10)=AA0(i)/(60.*20.)
                xff_ens3(11)=AA0(I)/(60.*20.)
                xff_ens3(16)=AA0(I)/(60.*20.)
                if(irandom.eq.1)then
                   call random_number (xxx)
                   ixxx=min(ens4,max(1,int(fens4*xxx+1.e-8)))
                   xff_ens3(12)=AA0(I)/(60.*20.)
                else
                   xff_ens3(12)=AA0(I)/(60.*20.)
                endif
!  
!--- more original Arakawa-Schubert (climatologic value of aa0)
!
!gtest
                if(icoic.eq.0)then
                if(xff0.lt.0.)then
                     xff_ens3(1)=0.
                     xff_ens3(2)=0.
                     xff_ens3(3)=0.
                     xff_ens3(13)=0.
                     xff_ens3(10)=0.
                     xff_ens3(11)=0.
                     xff_ens3(12)=0.
                endif
                  if(xff0.lt.0 .and. xland(i).lt.0.1)then
                     xff_ens3(:)=0.
                  endif
                endif

!                  if(i.eq.ipr.and.j.eq.jpr)write(0,*)'xff_ens = ',i,j,ipr,jpr,xff_ens3


                do nens=1,maxens
                   XK(nens)=(XAA0(I,nens)-AA1(I))/MBDT(1)
!                  if(i.eq.ipr.and.j.eq.jpr)write(0,*)'xks = ',xk(nens),XAA0(I,nens),AA1(I),mbdt
                   if(xk(nens).le.0.and.xk(nens).gt.-1.e-2) &
                           xk(nens)=-1.e-2
                   if(xk(nens).gt.0.and.xk(nens).lt.1.e-2) &
                           xk(nens)=1.e-2
                enddo
!
!--- add up all ensembles
!
                do 350 ne=1,maxens
!
!--- for every xk, we have maxens3 xffs
!--- iens is from outermost ensemble (most expensive!
!
!--- iedt (maxens2 belongs to it)
!--- is from second, next outermost, not so expensive
!
!--- so, for every outermost loop, we have maxens*maxens2*3
!--- ensembles!!! nall would be 0, if everything is on first
!--- loop index, then ne would start counting, then iedt, then iens....
!
                   iresult=0
                   iresultd=0
                   iresulte=0
                   nall=(iens-1)*maxens3*maxens*maxens2 &
                        +(iedt-1)*maxens*maxens3 &
                        +(ne-1)*maxens3
!                 if(i.eq.ipr.and.j.eq.jpr)write(0,*)'maxens',ne,nall,iens,maxens3,maxens,maxens2,iedt
!
! over water, enfor!e small cap for some of the closures
!
                if(maxens.gt.0 .and. xland(i).lt.0.1)then
                 if(ierr2(i).gt.0.or.ierr3(i).gt.0)then
                      xff_ens3(1) =ens_adj(i)*xff_ens3(1)
                      xff_ens3(2) =ens_adj(i)*xff_ens3(2)
                      xff_ens3(3) =ens_adj(i)*xff_ens3(3)
                      xff_ens3(13) =ens_adj(i)*xff_ens3(13)
                      xff_ens3(10) =ens_adj(i)*xff_ens3(10)
                      xff_ens3(11) =ens_adj(i)*xff_ens3(11)
                      xff_ens3(12) =ens_adj(i)*xff_ens3(12)
                      xff_ens3(16) =ens_adj(i)*xff_ens3(16)
                      xff_ens3(7) =ens_adj(i)*xff_ens3(7)
                      xff_ens3(8) =ens_adj(i)*xff_ens3(8)
                      xff_ens3(9) =ens_adj(i)*xff_ens3(9)
                      xff_ens3(15) =ens_adj(i)*xff_ens3(15)
!                     xff_ens3(7) =0.
!                     xff_ens3(8) =0.
!                     xff_ens3(9) =0.
                 endif
                endif
!
! end water treatment
!
!
!--- check for upwind convection
!                  iresult=0
                   massfld=0.

                   IF(XK(ne).lt.0.and.xff0.gt.0.)iresultd=1
                   iresulte=max(iresult,iresultd)
                   iresulte=1
                   if(iresulte.eq.1)then
!
!--- special treatment for stability closures
!
!                      if(i.eq.ipr.and.j.eq.jpr)write(0,*)'xffs = ',xff_ens3(1:16)

                      if(xff0.ge.0.)then
                         if(xff_ens3(1).gt.0)xf_ens(i,j,nall+1)=max(0.,-xff_ens3(1)/xk(ne))
                         if(xff_ens3(2).gt.0)xf_ens(i,j,nall+2)=max(0.,-xff_ens3(2)/xk(ne))
                         if(xff_ens3(3).gt.0)xf_ens(i,j,nall+3)=max(0.,-xff_ens3(3)/xk(ne))
                         if(xff_ens3(13).gt.0)xf_ens(i,j,nall+13)=max(0.,-xff_ens3(13)/xk(ne))
!                      if(i.eq.ipr.and.j.eq.jpr)write(0,*)'xf_ens(nall+1) ',i,j,nall,xf_ens(i,j,nall+1)
                      endif
!
!--- if iresult.eq.1, following independent of xff0
!
                         xf_ens(i,j,nall+4)=max(0.,xff_ens3(4))
                         xf_ens(i,j,nall+5)=max(0.,xff_ens3(5))
                         xf_ens(i,j,nall+6)=max(0.,xff_ens3(6))
                         xf_ens(i,j,nall+14)=max(0.,xff_ens3(14))
                         a1=max(1.e-3,pr_ens(i,j,nall+7))
                         xf_ens(i,j,nall+7)=max(0.,xff_ens3(7)/a1)
!                      if(i.eq.ipr.and.j.eq.jpr)write(0,*)'a1 = ',xff_ens3(7),a1,xf_ens(i,j,nall+7)
                         a1=max(1.e-3,pr_ens(i,j,nall+8))
                         xf_ens(i,j,nall+8)=max(0.,xff_ens3(8)/a1)
                         a1=max(1.e-3,pr_ens(i,j,nall+9))
                         xf_ens(i,j,nall+9)=max(0.,xff_ens3(9)/a1)
                         a1=max(1.e-3,pr_ens(i,j,nall+15))
                         xf_ens(i,j,nall+15)=max(0.,xff_ens3(15)/a1)
                         if(XK(ne).lt.0.)then
                            xf_ens(i,j,nall+10)=max(0.,-xff_ens3(10)/xk(ne))
                            xf_ens(i,j,nall+11)=max(0.,-xff_ens3(11)/xk(ne))
                            xf_ens(i,j,nall+12)=max(0.,-xff_ens3(12)/xk(ne))
                            xf_ens(i,j,nall+16)=max(0.,-xff_ens3(16)/xk(ne))
                         endif
                      if(icoic.ge.1)then
                      closure_n(i)=0.
                      xf_ens(i,j,nall+1)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+2)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+3)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+4)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+5)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+6)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+7)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+8)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+9)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+10)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+11)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+12)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+13)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+14)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+15)=xf_ens(i,j,nall+icoic)
                      xf_ens(i,j,nall+16)=xf_ens(i,j,nall+icoic)
                      endif
!
! 16 is a randon pick from the oher 15
!
                if(irandom.eq.1)then
                   call random_number (xxx)
                   ixxx=min(15,max(1,int(15.*xxx+1.e-8)))
                   xf_ens(i,j,nall+16)=xf_ens(i,j,nall+ixxx)
!               else
!                  xf_ens(i,j,nall+16)=xf_ens(i,j,nall+1)
                endif
!
!
!--- do some more on the caps!!! ne=1 for 175, ne=2 for 100,....
!
!     do not care for caps here for closure groups 1 and 5,
!     they are fine, do not turn them off here
!
!!!!    NOT USED FOR "NORMAL" APPLICATION (maxens=1)
!
                if(maxens.gt.1)then
                if(ne.eq.2.and.ierr2(i).gt.0)then
                      xf_ens(i,j,nall+1) =0.
                      xf_ens(i,j,nall+2) =0.
                      xf_ens(i,j,nall+3) =0.
                      xf_ens(i,j,nall+4) =0.
                      xf_ens(i,j,nall+5) =0.
                      xf_ens(i,j,nall+6) =0.
                      xf_ens(i,j,nall+7) =0.
                      xf_ens(i,j,nall+8) =0.
                      xf_ens(i,j,nall+9) =0.
                      xf_ens(i,j,nall+10)=0.
                      xf_ens(i,j,nall+11)=0.
                      xf_ens(i,j,nall+12)=0.
                      xf_ens(i,j,nall+13)=0.
                      xf_ens(i,j,nall+14)=0.
                      xf_ens(i,j,nall+15)=0.
                      xf_ens(i,j,nall+16)=0.
                endif
                if(ne.eq.3.and.ierr3(i).gt.0)then
                      xf_ens(i,j,nall+1) =0.
                      xf_ens(i,j,nall+2) =0.
                      xf_ens(i,j,nall+3) =0.
                      xf_ens(i,j,nall+4) =0.
                      xf_ens(i,j,nall+5) =0.
                      xf_ens(i,j,nall+6) =0.
                      xf_ens(i,j,nall+7) =0.
                      xf_ens(i,j,nall+8) =0.
                      xf_ens(i,j,nall+9) =0.
                      xf_ens(i,j,nall+10)=0.
                      xf_ens(i,j,nall+11)=0.
                      xf_ens(i,j,nall+12)=0.
                      xf_ens(i,j,nall+13)=0.
                      xf_ens(i,j,nall+14)=0.
                      xf_ens(i,j,nall+15)=0.
                      xf_ens(i,j,nall+16)=0.
                endif
                endif

                   endif
 350            continue
                if(maxens.gt.1)then
! ne=1, cap=175
!
                   nall=(iens-1)*maxens3*maxens*maxens2 &
                        +(iedt-1)*maxens*maxens3
! ne=2, cap=100
!
                   nall2=(iens-1)*maxens3*maxens*maxens2 &
                        +(iedt-1)*maxens*maxens3 &
                        +(2-1)*maxens3
                      xf_ens(i,j,nall+4) = xf_ens(i,j,nall2+4)
                      xf_ens(i,j,nall+5) =xf_ens(i,j,nall2+5)
                      xf_ens(i,j,nall+6) =xf_ens(i,j,nall2+6)
                      xf_ens(i,j,nall+14) =xf_ens(i,j,nall2+14)
                      xf_ens(i,j,nall+7) =xf_ens(i,j,nall2+7)
                      xf_ens(i,j,nall+8) =xf_ens(i,j,nall2+8)
                      xf_ens(i,j,nall+9) =xf_ens(i,j,nall2+9)
                      xf_ens(i,j,nall+15) =xf_ens(i,j,nall2+15)
                      xf_ens(i,j,nall+10)=xf_ens(i,j,nall2+10)
                      xf_ens(i,j,nall+11)=xf_ens(i,j,nall2+11)
                      xf_ens(i,j,nall+12)=xf_ens(i,j,nall2+12)
!                     if(i.eq.ipr.and.j.eq.jpr)write(0,*)'should not be here'
                   endif
!         if(i.eq.ipr.and.j.eq.jpr)write(0,*)'xff_ens3=',xff_ens3
                go to 100
             endif
          elseif(ierr(i).ne.20.and.ierr(i).ne.0)then
             do n=1,ensdim
               xf_ens(i,j,n)=0.
             enddo
          endif
!         if(i.eq.ipr.and.j.eq.jpr)write(0,*)'xff_ens3=',xff_ens3
 100   continue

   END SUBROUTINE cup_forcing_ens_3d


   SUBROUTINE cup_kbcon(ierrc,cap_inc,iloop,k22,kbcon,he_cup,hes_cup, &
              hkb,ierr,kbmax,p_cup,cap_max,                         &
              xl,cp,ztexec,zqexec,use_excess,       &
              itf,jtf,ktf,                        &
              its,ite, jts,jte, kts,kte                        )

   IMPLICIT NONE
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        use_excess,itf,jtf,ktf,           &
        its,ite, jts,jte, kts,kte
  ! 
  ! 
  ! 
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        he_cup,hes_cup,p_cup
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        ztexec,zqexec,cap_max,cap_inc
     real,intent (in   )                  ::                           &
        xl,cp
     real,    dimension (its:ite)                                      &
        ,intent (inout   )                   ::                           &
        hkb
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbmax
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        kbcon,k22,ierr
     integer                                                           &
        ,intent (in   )                   ::                           &
        iloop
     character*50 :: ierrc(its:ite)
!
!  local variables in this routine
!

     integer                              ::                           &
        i,k,k1,k2
     real                                 ::                           &
        pbcdif,plus,hetest
!
!--- DETERMINE THE LEVEL OF CONVECTIVE CLOUD BASE  - KBCON
!
       DO 27 i=its,itf
      kbcon(i)=1
      IF(ierr(I).ne.0)GO TO 27
      KBCON(I)=K22(I)+1
      if(iloop.eq.5)KBCON(I)=K22(I)
      GO TO 32
 31   CONTINUE
      KBCON(I)=KBCON(I)+1
      IF(KBCON(I).GT.KBMAX(i)+2)THEN
         if(iloop.ne.4)then
                ierr(i)=3
                ierrc(i)="could not find reasonable kbcon in cup_kbcon"
         endif
        GO TO 27
      ENDIF
 32   CONTINUE
      hetest=hkb(i) ! HE_cup(I,K22(I))
      if(iloop.eq.5)then
       hetest=HKB(I)
!      do k=1,k22(i)
!        hetest=max(hetest,he_cup(i,k))
!      enddo
      endif
      IF(HETEST.LT.HES_cup(I,KBCON(I)))then
!       write(0,*)'htest',k22(i),kbcon(i),HETEST,-P_cup(I,KBCON(I))+P_cup(I,K22(I))
        GO TO 31
      endif

!     cloud base pressure and max moist static energy pressure
!     i.e., the depth (in mb) of the layer of negative buoyancy
      if(KBCON(I)-K22(I).eq.1)go to 27
      if(iloop.eq.5 .and. (KBCON(I)-K22(I)).eq.0)go to 27
      PBCDIF=-P_cup(I,KBCON(I))+P_cup(I,K22(I))
      plus=max(25.,cap_max(i)-float(iloop-1)*cap_inc(i))
      if(iloop.eq.4)plus=cap_max(i)
!
! for shallow convection, if cap_max is greater than 25, it is the pressure at pbltop
      if(iloop.eq.5)plus=25.
      if(iloop.eq.5.and.cap_max(i).gt.25)pbcdif=-P_cup(I,KBCON(I))+cap_max(i)
      IF(PBCDIF.GT.plus)THEN
!       write(0,*)'htest',k22(i),kbcon(i),plus,-P_cup(I,KBCON(I))+P_cup(I,K22(I))
        K22(I)=K22(I)+1
        KBCON(I)=K22(I)+1
         if(use_excess == 2) then
             k1=max(1,k22(i)-1)
             k2=max(1,min(kbcon(i)-1,k22(i)+1))  !kbcon(i)-1
             k2=k22(i)+1
             hkb(i)=sum(he_cup(i,k1:k2))/float(k2-k1+1)+(xl*zqexec(i)+cp*ztexec(i))/float(k2-k1+1)
        else if(use_excess <= 1)then
             hkb(i)=he_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
        endif  ! excess

        if(iloop.eq.5)KBCON(I)=K22(I)
        IF(KBCON(I).GT.KBMAX(i)+2)THEN
         if(iloop.ne.4)then
                ierr(i)=3
                ierrc(i)="could not find reasonable kbcon in cup_kbcon"
         endif
        GO TO 27
      ENDIF
        GO TO 32
      ENDIF
 27   CONTINUE

   END SUBROUTINE cup_kbcon


   SUBROUTINE cup_ktop(ierrc,ilo,dby,kbcon,ktop,ierr,              &
              itf,jtf,ktf,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,jtf,ktf,           &
        its,ite, jts,jte, kts,kte
  ! dby = buoancy term
  ! ktop = cloud top (output)
  ! ilo  = flag
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
        dby
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon
     integer                                                           &
        ,intent (in   )                   ::                           &
        ilo
     integer, dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        ktop
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     character*50 :: ierrc(its:ite)
!
!  local variables in this routine
!

     integer                              ::                           &
        i,k
!
        DO 42 i=its,itf
        ktop(i)=1
         IF(ierr(I).EQ.0)then
          DO 40 K=KBCON(I)+1,ktf-1
            IF(DBY(I,K).LE.0.)THEN
                KTOP(I)=K-1
                GO TO 41
             ENDIF
  40      CONTINUE
          if(ilo.eq.1)ierr(i)=5
          if(ilo.eq.1)ierrc(i)="problem with defining ktop"
!         if(ilo.eq.2)ierr(i)=998
          GO TO 42
  41     CONTINUE
         do k=ktop(i)+1,ktf
           dby(i,k)=0.
         enddo
         if(kbcon(i).eq.ktop(i))then
            ierr(i)=55
            ierrc(i)="kbcon == ktop "
         endif
         endif
  42     CONTINUE

   END SUBROUTINE cup_ktop


   SUBROUTINE cup_MAXIMI(ARRAY,KS,KE,MAXX,ierr,              &
              itf,jtf,ktf,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
         itf,jtf,ktf,                                    &
         its,ite, jts,jte, kts,kte
  ! array input array
  ! x output array with return values
  ! kt output array of levels
  ! ks,kend  check-range
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
         array
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
         ierr,ke
     integer                                                           &
        ,intent (in   )                   ::                           &
         ks
     integer, dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
         maxx
     real,    dimension (its:ite)         ::                           &
         x
     real                                 ::                           &
         xar
     integer                              ::                           &
         i,k

       DO 200 i=its,itf
       MAXX(I)=KS
       if(ierr(i).eq.0)then
      X(I)=ARRAY(I,KS)
!
       DO 100 K=KS,KE(i)
         XAR=ARRAY(I,K)
         IF(XAR.GE.X(I)) THEN
            X(I)=XAR
            MAXX(I)=K
         ENDIF
 100  CONTINUE
      endif
 200  CONTINUE

   END SUBROUTINE cup_MAXIMI


   SUBROUTINE cup_minimi(ARRAY,KS,KEND,KT,ierr,              &
              itf,jtf,ktf,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
         itf,jtf,ktf,                                    &
         its,ite, jts,jte, kts,kte
  ! array input array
  ! x output array with return values
  ! kt output array of levels
  ! ks,kend  check-range
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
         array
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
         ierr,ks,kend
     integer, dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
         kt
     real,    dimension (its:ite)         ::                           &
         x
     integer                              ::                           &
         i,k,kstop

       DO 200 i=its,itf
      KT(I)=KS(I)
      if(ierr(i).eq.0)then
      X(I)=ARRAY(I,KS(I))
       KSTOP=MAX(KS(I)+1,KEND(I))
!
       DO 100 K=KS(I)+1,KSTOP
         IF(ARRAY(I,K).LT.X(I)) THEN
              X(I)=ARRAY(I,K)
              KT(I)=K
         ENDIF
 100  CONTINUE
      endif
 200  CONTINUE

   END SUBROUTINE cup_MINIMI


   SUBROUTINE cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup,       &
              kbcon,ktop,ierr,                               &
              itf,jtf,ktf,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,jtf,ktf,                                     &
        its,ite, jts,jte, kts,kte
  ! aa0 cloud work function
  ! gamma_cup = gamma on model cloud levels
  ! t_cup = temperature (Kelvin) on model cloud levels
  ! dby = buoancy term
  ! zu= normalized updraft mass flux
  ! z = heights of model levels 
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        z,zu,gamma_cup,t_cup,dby
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop
!
! input and output
!


     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        aa0
!
!  local variables in this routine
!

     integer                              ::                           &
        i,k
     real                                 ::                           &
        dz,da
!
        do i=its,itf
         aa0(i)=0.
        enddo
        DO 100 k=kts+1,ktf
        DO 100 i=its,itf
         IF(ierr(i).ne.0)GO TO 100
         IF(K.LE.KBCON(I))GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100
         DZ=Z(I,K)-Z(I,K-1)
         da=zu(i,k)*DZ*(9.81/(1004.*( &
                (T_cup(I,K)))))*DBY(I,K-1)/ &
             (1.+GAMMA_CUP(I,K))
         IF(K.eq.KTOP(I).and.da.le.0.)go to 100
         AA0(I)=AA0(I)+da
         if(aa0(i).lt.0.)aa0(i)=0.
100     continue

   END SUBROUTINE cup_up_aa0

!====================================================================
   SUBROUTINE g3init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,           &
                        MASS_FLUX,cp,restart,                       &
                        P_QC,P_QI,P_FIRST_SCALAR,                   &
                        RTHFTEN, RQVFTEN,                           &
                        APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                        APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                        cugd_tten,cugd_ttens,cugd_qvten,            &
                        cugd_qvtens,cugd_qcten,                     &
                        allowed_to_read,                            &
                        ids, ide, jds, jde, kds, kde,               &
                        ims, ime, jms, jme, kms, kme,               &
                        its, ite, jts, jte, kts, kte               )
!--------------------------------------------------------------------   
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_FIRST_SCALAR, P_QI, P_QC
   REAL,     INTENT(IN)           ::  cp

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          CUGD_TTEN,         &
                                                          CUGD_TTENS,        &
                                                          CUGD_QVTEN,        &
                                                          CUGD_QVTENS,       &
                                                          CUGD_QCTEN
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHCUTEN, &
                                                          RQVCUTEN, &
                                                          RQCCUTEN, &
                                                          RQICUTEN   

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHFTEN,  &
                                                          RQVFTEN

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) ::        &
                                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,  &
                                APR_CAPMA,APR_CAPME,APR_CAPMI,      &
                                MASS_FLUX

   INTEGER :: i, j, k, itf, jtf, ktf
 
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)
 
   IF(.not.restart)THEN
     DO j=jts,jte
     DO k=kts,kte
     DO i=its,ite
        RTHCUTEN(i,k,j)=0.
        RQVCUTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO
     DO j=jts,jte
     DO k=kts,kte
     DO i=its,ite
       cugd_tten(i,k,j)=0.
       cugd_ttens(i,k,j)=0.
       cugd_qvten(i,k,j)=0.
       cugd_qvtens(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHFTEN(i,k,j)=0.
        RQVFTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     IF (P_QC .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQCCUTEN(i,k,j)=0.
           cugd_qcten(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     IF (P_QI .ge. P_FIRST_SCALAR) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           RQICUTEN(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     DO j=jts,jtf
     DO i=its,itf
        mass_flux(i,j)=0.
     ENDDO
     ENDDO

     DO j=jts,jtf
     DO i=its,itf
        APR_GR(i,j)=0.
        APR_ST(i,j)=0.
        APR_W(i,j)=0.
        APR_MC(i,j)=0.
        APR_AS(i,j)=0.
        APR_CAPMA(i,j)=0.
        APR_CAPME(i,j)=0.
        APR_CAPMI(i,j)=0.
     ENDDO
     ENDDO

   ENDIF

   END SUBROUTINE g3init

   SUBROUTINE neg_check(j,subt,subq,dt,q,outq,outt,outqc,pret,its,ite,kts,kte,itf,ktf)

   INTEGER,      INTENT(IN   ) ::            j,its,ite,kts,kte,itf,ktf

     real, dimension (its:ite,kts:kte  )                    ,                 &
      intent(inout   ) ::                                                     &
       outq,outt,outqc,subt,subq
     real, dimension (its:ite,kts:kte  )                    ,                 &
      intent(inout   ) ::                                                     &
       q
     real, dimension (its:ite  )                            ,                 &
      intent(inout   ) ::                                                     &
       pret
     real                                                                     &
        ,intent (in  )                   ::                                   &
        dt
     real :: thresh,qmem,qmemf,qmem2,qtest,qmem1
!
! first do check on vertical heating rate
!
      thresh=300.01
      do i=its,itf
      qmemf=1.
      qmem=0.
      do k=kts,ktf
         qmem=(subt(i,k)+outt(i,k))*86400.
         if(qmem.gt.2.*thresh)then
           qmem2=2.*thresh/qmem
           qmemf=min(qmemf,qmem2)
!
!
!          print *,'1',' adjusted massflux by factor ',i,j,k,qmem,qmem2,qmemf,dt
         endif
         if(qmem.lt.-thresh)then
           qmem2=-thresh/qmem
           qmemf=min(qmemf,qmem2)
!
!
!          print *,'2',' adjusted massflux by factor ',i,j,k,qmem,qmem2,qmemf,dt
         endif
      enddo
!     if(qmemf.lt.1)then
!          write(0,*)'1',' adjusted massflux by factor ',i,j,qmemf
!     endif
      do k=kts,ktf
         subq(i,k)=subq(i,k)*qmemf
         subt(i,k)=subt(i,k)*qmemf
         outq(i,k)=outq(i,k)*qmemf
         outt(i,k)=outt(i,k)*qmemf
         outqc(i,k)=outqc(i,k)*qmemf
      enddo
      pret(i)=pret(i)*qmemf 
      enddo
!
! check whether routine produces negative q's. This can happen, since 
! tendencies are calculated based on forced q's. This should have no
! influence on conservation properties, it scales linear through all
! tendencies
!
      thresh=1.e-10
      do i=its,itf
      qmemf=1.
      do k=kts,ktf-1
         qmem=subq(i,k)+outq(i,k)
         if(abs(qmem).gt.0.)then
         qtest=q(i,k)+(subq(i,k)+outq(i,k))*dt
         if(qtest.lt.thresh)then
!
! qmem2 would be the maximum allowable tendency
!
           qmem1=outq(i,k)+subq(i,k)
           qmem2=(thresh-q(i,k))/dt
           qmemf=min(qmemf,qmem2/qmem1)
           qmemf=max(0.,qmemf)
!          write(0,*)'4 adjusted tendencies ',i,k,qmem,qmem2,qmemf
!          write(0,*)'4 adjusted tendencies ',i,j,k,q(i,k),qmem1,qmemf
         endif
         endif
      enddo
!     if(qmemf.lt.1.)write(0,*)'4 adjusted tendencies ',i,j,qmemf
      do k=kts,ktf
         subq(i,k)=subq(i,k)*qmemf
         subt(i,k)=subt(i,k)*qmemf
         outq(i,k)=outq(i,k)*qmemf
         outt(i,k)=outt(i,k)*qmemf
         outqc(i,k)=outqc(i,k)*qmemf
      enddo
      pret(i)=pret(i)*qmemf 
      enddo

   END SUBROUTINE neg_check


   SUBROUTINE cup_output_ens_3d(xf_ens,ierr,dellat,dellaq,dellaqc,  &
              subt_ens,subq_ens,subt,subq,outtem,outq,outqc,     &
              zu,sub_mas,pre,pw,xmb,ktop,                 &
              j,name,nx,nx2,iens,ierr2,ierr3,pr_ens,             &
              maxens3,ensdim,                            &
              sig,APR_GR,APR_W,APR_MC,APR_ST,APR_AS,                 &
              APR_CAPMA,APR_CAPME,APR_CAPMI,closure_n,xland1,    &
              weight_GR,weight_W,weight_MC,weight_ST,weight_AS,training,  &
	      ipr,jpr,itf,jtf,ktf, &
              its,ite, jts,jte, kts,kte)

   IMPLICIT NONE
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        ipr,jpr,itf,jtf,ktf,     &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ensdim,nx,nx2,iens,maxens3,training
  ! xf_ens = ensemble mass fluxes
  ! pr_ens = precipitation ensembles
  ! dellat = change of temperature per unit mass flux of cloud ensemble
  ! dellaq = change of q per unit mass flux of cloud ensemble
  ! dellaqc = change of qc per unit mass flux of cloud ensemble
  ! outtem = output temp tendency (per s)
  ! outq   = output q tendency (per s)
  ! outqc  = output qc tendency (per s)
  ! pre    = output precip
  ! xmb    = total base mass flux
  ! xfac1  = correction factor
  ! pw = pw -epsilon*pd (ensemble dependent)
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,jts:jte,1:ensdim)                     &
        ,intent (inout)                   ::                           &
       xf_ens,pr_ens
!srf ------
!    real,    dimension (its:ite,jts:jte)                              &
     real,    dimension (its:ite,jts:jte)                              &
         ,intent (inout)                   ::                          &
               APR_GR,APR_W,APR_MC,APR_ST,APR_AS,APR_CAPMA,            &
               APR_CAPME,APR_CAPMI 
     real, dimension( its:ite , jts:jte )                      &
         ,intent(in) :: weight_gr,weight_w,weight_mc,weight_st,weight_as
!-srf---
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        outtem,outq,outqc,subt,subq,sub_mas
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        zu
     real,   dimension (its:ite)                                      &
         ,intent (in  )                   ::                           &
        sig
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pre,xmb
     real,    dimension (its:ite)                                      &
        ,intent (inout  )                   ::                           &
        closure_n,xland1
     real,    dimension (its:ite,kts:kte,1:nx)                     &
        ,intent (in   )                   ::                           &
       subt_ens,subq_ens,dellat,dellaqc,dellaq,pw
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        ktop
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr,ierr2,ierr3
!
!  local variables in this routine
!

     integer                              ::                           &
        i,k,n,ncount
     real                                 ::                           &
        outtes,ddtes,dtt,dtq,dtqc,dtpw,prerate,clos_wei,xmbhelp
     real                                 ::                           &
        dtts,dtqs
     real,    dimension (its:ite)         ::                           &
       xfac1,xfac2
     real,    dimension (its:ite)::                           &
       xmb_ske,xmb_ave,xmb_std,xmb_cur,xmbweight
     real,    dimension (its:ite)::                           &
       pr_ske,pr_ave,pr_std,pr_cur
     real,    dimension (its:ite,jts:jte)::                           &
               pr_gr,pr_w,pr_mc,pr_st,pr_as,pr_capma,     &
               pr_capme,pr_capmi
     real, dimension (5) :: weight,wm,wm1,wm2,wm3
     real, dimension (its:ite,5) :: xmb_w

!
      character *(*), intent (in)        ::                           &
       name

!
     weight(1) = -999.  !this will turn off weights
     wm(1)=-999.

!
!
      DO k=kts,ktf
      do i=its,itf
        outtem(i,k)=0.
        outq(i,k)=0.
        outqc(i,k)=0.
        subt(i,k)=0.
        subq(i,k)=0.
        sub_mas(i,k)=0.
      enddo
      enddo
      do i=its,itf
        pre(i)=0.
        xmb(i)=0.
         xfac1(i)=0.
         xfac2(i)=0.
        xmbweight(i)=1.
      enddo
      do i=its,itf
        IF(ierr(i).eq.0)then
        do n=(iens-1)*nx*nx2*maxens3+1,iens*nx*nx2*maxens3
           if(pr_ens(i,j,n).le.0.)then
!            if(i.eq.ipr.and.j.eq.jpr)write(0,*)'pr_ens',n,pr_ens(i,j,n),xf_ens(i,j,n)
             xf_ens(i,j,n)=0.
           endif
        enddo
        endif
      enddo
!
       xmb_w=0.
!
!-- now do feedback
!
      ddtes=100.
      do i=its,itf
        if(ierr(i).eq.0)then
         k=0
         xmb_ave(i)=0.
         do n=(iens-1)*nx*nx2*maxens3+1,iens*nx*nx2*maxens3
          k=k+1
          xmb_ave(i)=xmb_ave(i)+xf_ens(i,j,n)
         enddo
         xmb_ave(i)=xmb_ave(i)/float(k)
         if(xmb_ave(i).le.0.)then
              ierr(i)=13
              xmb_ave(i)=0.
         endif
         xmb(i)=sig(i)*xmb_ave(i)
! --- Now use proper count of how many closures were actually
!       used in cup_forcing_ens (including screening of some
!       closures over water) to properly normalize xmb
           clos_wei=16./max(1.,closure_n(i))

           if(xmb(i).eq.0.)then
              ierr(i)=19
           endif
           if(xmb(i).gt.100.)then
              ierr(i)=19
           endif
           xfac1(i)=xmb(i)
           xfac2(i)=xmb(i)

        endif
      ENDDO
      DO k=kts,ktf
      do i=its,itf
            dtt =0.
            dtts=0.
            dtq =0.
            dtqs=0.
            dtqc=0.
            dtpw=0.
        IF(ierr(i).eq.0.and.k.le.ktop(i))then
           do n=1,nx
              dtt =dtt  + dellat  (i,k,n)
              dtts=dtts + subt_ens(i,k,n)
              dtq =dtq  + dellaq  (i,k,n)
              dtqs=dtqs + subq_ens(i,k,n)
              dtqc=dtqc + dellaqc (i,k,n)
              dtpw=dtpw + pw      (i,k,n)
           enddo
           OUTTEM(I,K)= XMB(I)* dtt /float(nx)
           SUBT  (I,K)= XMB(I)* dtts/float(nx)
           OUTQ  (I,K)= XMB(I)* dtq /float(nx)
           SUBQ  (I,K)= XMB(I)* dtqs/float(nx)
           OUTQC (I,K)= XMB(I)* dtqc/float(nx)
	   PRE(I)=PRE(I)+XMB(I)*dtpw/float(nx)
           sub_mas(i,k)=zu(i,k)*xmb(i)
!          xf_ens(i,j,:)=sig(i)*xf_ens(i,j,:)*dtpw/float(nx)
        endif
       enddo
      enddo

      do i=its,itf
        if(ierr(i).eq.0)then
        do k=(iens-1)*nx*nx2*maxens3+1,iens*nx*nx2*maxens3
          xf_ens(i,j,k)=sig(i)*xf_ens(i,j,k)*xfac1(i)
        enddo
        endif
      ENDDO

!srf-fix for preci
      do i=its,itf
        if(ierr(i).ne. 0)then
            apr_w (i,j)=0.0
	    apr_st(i,j)=0.0
	    apr_gr(i,j)=0.0
	    apr_mc(i,j)=0.0
	    apr_as(i,j)=0.0
        endif
      ENDDO
!srf
   END SUBROUTINE cup_output_ens_3d
!-------------------------------------------------------
   SUBROUTINE cup_up_moisture(name,ierr,z_cup,qc,qrc,pw,pwav,     &
              ccnclean,p_cup,kbcon,ktop,cd,dby,clw_all,&
              t_cup,q,GAMMA_cup,zu,qes_cup,k22,qe_cup,xl,         &
              ZQEXEC,use_excess,ccn,rho, &
              up_massentr,up_massdetr,psum,psumh,                 &
              autoconv,aeroevap,itest,itf,jtf,ktf,j,ipr,jpr,                &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE
  real, parameter :: BDISPM = 0.366       !Berry--size dispersion (maritime)
  REAL, PARAMETER :: BDISPC = 0.146       !Berry--size dispersion (continental)
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
                                  use_excess,itest,autoconv,aeroevap,itf,jtf,ktf,           &
                                  its,ite, jts,jte,j,ipr,jpr, kts,kte
  ! cd= detrainment function 
  ! q = environmental q on model levels
  ! qe_cup = environmental q on model cloud levels
  ! qes_cup = saturation q on model cloud levels
  ! dby = buoancy term
  ! cd= detrainment function 
  ! zu = normalized updraft mass flux
  ! gamma_cup = gamma on model cloud levels
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        t_cup,p_cup,rho,q,zu,gamma_cup,qe_cup,                         &
        up_massentr,up_massdetr,dby,qes_cup,z_cup,cd
     real,    dimension (its:ite)                              &
        ,intent (in   )                   ::                           &
        zqexec
  ! entr= entrainment rate 
     real                                                              &
        ,intent (in   )                   ::                           &
        ccnclean,xl
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop,k22
!
! input and output
!

   ! ierr error value, maybe modified in this routine

     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
      character *(*), intent (in)        ::                           &
       name
   ! qc = cloud q (including liquid water) after entrainment
   ! qrch = saturation q in cloud
   ! qrc = liquid water content in cloud after rainout
   ! pw = condensate that will fall out at that level
   ! pwav = totan normalized integrated condensate (I1)
   ! c0 = conversion rate (cloud to rain)

     real,    dimension (its:ite,kts:kte)                              &
        ,intent (out  )                   ::                           &
        qc,qrc,pw,clw_all
     real,    dimension (its:ite,kts:kte) ::                           &
        qch,qrcb,pwh,clw_allh
     real,    dimension (its:ite)         ::                           &
        pwavh
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pwav,psum,psumh
     real,    dimension (its:ite)                                      &
        ,intent (in  )                   ::                           &
        ccn
!
!  local variables in this routine
!

     integer                              ::                           &
        iounit,iprop,iall,i,k,k1,k2
     real                                 ::                           &
        prop_ave,qrcb_h,bdsp,dp,g,rhoc,dh,qrch,c0,dz,radius,berryc0,q1,berryc
     real,    dimension (kts:kte)         ::                           &
        prop_b
!
        prop_b(kts:kte)=0
        iall=0
        c0=.002
        g=9.81
        bdsp=BDISPM
!
!--- no precip for small clouds
!
        if(name.eq.'shallow')c0=0.
        do i=its,itf
          pwav(i)=0.
          pwavh(i)=0.
          psum(i)=0.
          psumh(i)=0.
        enddo
        do k=kts,ktf
        do i=its,itf
          pw(i,k)=0.
          pwh(i,k)=0.
          qc(i,k)=0.
          if(ierr(i).eq.0)qc(i,k)=qes_cup(i,k)
          if(ierr(i).eq.0)qch(i,k)=qes_cup(i,k)
          clw_all(i,k)=0.
          clw_allh(i,k)=0.
          qrc(i,k)=0.
          qrcb(i,k)=0.
        enddo
        enddo
      if(use_excess < 2 ) then
      do i=its,itf
      if(ierr(i).eq.0.)then
      do k=2,kbcon(i)-1
        DZ=Z_cup(i,K)-Z_cup(i,K-1)
        qc(i,k)=qe_cup(i,k22(i))+float(use_excess)*zqexec(i)
        qch(i,k)=qe_cup(i,k22(i))+float(use_excess)*zqexec(i)
        if(qc(i,k).gt.qes_cup(i,kbcon(i)-1))then
            pw(i,k)=zu(i,k)*(qc(i,k)-qes_cup(i,kbcon(i)-1))
            qc(i,k)=qes_cup(i,kbcon(i)-1)
            qch(i,k)=qes_cup(i,kbcon(i)-1)
            PWAV(I)=PWAV(I)+PW(I,K)
            Psum(I)=Psum(I)+pw(I,K)*dz
        endif
      enddo
      endif
      enddo
      else if(use_excess == 2) then
        do i=its,itf
         if(ierr(i).eq.0.)then
             k1=max(1,k22(i)-1)
             k2=k22(i)+1
          do k=2,kbcon(i)-1
             DZ=Z_cup(i,K)-Z_cup(i,K-1)
             qc (i,k)=sum(qe_cup(i,k1:k2))/float(k2-k1+1) +zqexec(i)
             qch(i,k)=sum(qe_cup(i,k1:k2))/float(k2-k1+1) +zqexec(i)
             if(qc(i,k).gt.qes_cup(i,kbcon(i)-1))then
                 pw(i,k)=zu(i,k)*(qc(i,k)-qes_cup(i,kbcon(i)-1))
                 qc(i,k)=qes_cup(i,kbcon(i)-1)
                 qch(i,k)=qes_cup(i,kbcon(i)-1)
                 PWAV(I)=PWAV(I)+PW(I,K)
                 Psum(I)=Psum(I)+pw(I,K)*dz
             endif
          enddo !k
         endif  !ierr
        enddo !i
      endif  ! use_excess

        DO 100 k=kts+1,ktf
        DO 100 i=its,itf
         IF(ierr(i).ne.0)GO TO 100
         IF(K.Lt.KBCON(I))GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100
         rhoc=.5*(rho(i,k)+rho(i,k-1))
         DZ=Z_cup(i,K)-Z_cup(i,K-1)
         DP=p_cup(i,K)-p_cup(i,K-1)
!
!--- saturation  in cloud, this is what is allowed to be in it
!
         QRCH=QES_cup(I,K)+(1./XL)*(GAMMA_cup(i,k) &
              /(1.+GAMMA_cup(i,k)))*DBY(I,K)
!
!------    1. steady state plume equation, for what could
!------       be in cloud without condensation
!
!
       qc(i,k)=   (qc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)* qc(i,k-1)+ &
                         up_massentr(i,k-1)*q(i,k-1))   /            &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
       qch(i,k)= (qch(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*qch(i,k-1)+ &
                         up_massentr(i,k-1)*q(i,k-1))   /            &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))

        if(qc(i,k).le.qrch)qc(i,k)=qrch
        if(qch(i,k).le.qrch)qch(i,k)=qrch
!
!------- Total condensed water before rainout
!
        clw_all(i,k)=QC(I,K)-QRCH
        QRC(I,K)=(QC(I,K)-QRCH) ! /(1.+C0*DZ*zu(i,k))
        clw_allh(i,k)=QCH(I,K)-QRCH
        QRCB(I,K)=(QCH(I,K)-QRCH) ! /(1.+C0*DZ*zu(i,k))
    IF(autoconv.eq.2) then


! 
! normalized berry
!
! first calculate for average conditions, used in cup_dd_edt!
! this will also determine proportionality constant prop_b, which, if applied,
! would give the same results as c0 under these conditions
!
         q1=1.e3*rhoc*qrcb(i,k)  ! g/m^3 ! g[h2o]/cm^3
         berryc0=q1*q1/(60.0*(5.0 + 0.0366*CCNclean/ &
                ( q1 * BDSP)  ) ) !/(
!     if(i.eq.ipr.and.j.eq.jpr)write(0,*)'cupm',k,rhoc,rho(i,k)
!         qrcb_h=qrcb(i,k)/(1.+c0*dz)
         qrcb_h=((QCH(I,K)-QRCH)*zu(i,k)-qrcb(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                   (zu(i,k)+.5*up_massdetr(i,k-1)+c0*dz*zu(i,k))
         prop_b(k)=c0*qrcb_h*zu(i,k)/(1.e-3*berryc0)
         pwh(i,k)=1.e-3*berryc0*dz*prop_b(k) ! 2.
         berryc=qrcb(i,k)
         qrcb(i,k)=((QCh(I,K)-QRCH)*zu(i,k)-pwh(i,k)-qrcb(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                   (zu(i,k)+.5*up_massdetr(i,k-1))
!        QRCb(I,K) = qrcb(i,k) - pwh(i,k)
         if(qrcb(i,k).lt.0.)then
           berryc0=(qrcb(i,k-1)*(.5*up_massdetr(i,k-1))-(QCh(I,K)-QRCH)*zu(i,k))/zu(i,k)*1.e-3*dz*prop_b(k)
           pwh(i,k)=zu(i,k)*1.e-3*berryc0*dz*prop_b(k)
           qrcb(i,k)=0.
         endif
!     if(i.eq.ipr.and.j.eq.jpr)write(0,*)'cupm',zu(i,k),pwh(i,k),dz,qrch,qrcb(i,k),clw_allh(i,k)
      QCh(I,K)=QRCb(I,K)+qrch
      PWAVH(I)=PWAVH(I)+pwh(I,K)
      Psumh(I)=Psumh(I)+clw_allh(I,K)*zu(i,k) *dz
!
! then the real berry
!
          q1=1.e3*rhoc*qrc(i,k)  ! g/m^3 ! g[h2o]/cm^3
          berryc0=q1*q1/(60.0*(5.0 + 0.0366*CCN(i)/ &
                ( q1 * BDSP)  ) ) !/(
          berryc0=1.e-3*berryc0*dz*prop_b(k) ! 2.
          berryc=qrc(i,k)
          qrc(i,k)=((QC(I,K)-QRCH)*zu(i,k)-zu(i,k)*berryc0-qrc(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                   (zu(i,k)+.5*up_massdetr(i,k-1))
          if(qrc(i,k).lt.0.)then
            berryc0=((QC(I,K)-QRCH)*zu(i,k)-qrc(i,k-1)*(.5*up_massdetr(i,k-1)))/zu(i,k)
            qrc(i,k)=0.
          endif
          pw(i,k)=berryc0*zu(i,k)
          QC(I,K)=QRC(I,K)+qrch
!
!  if not running with berry at all, do the following
!
       ELSE       !c0=.002
         qrc(i,k)=((QC(I,K)-QRCH)*zu(i,k)-qrc(i,k-1)*(.5*up_massdetr(i,k-1)))/ &
                   (zu(i,k)+.5*up_massdetr(i,k-1)+c0*dz*zu(i,k))
         PW(i,k)=c0*dz*QRC(I,K)*zu(i,k)
         if(qrc(i,k).lt.0)then
           qrc(i,k)=0.
           pw(i,k)=0.
         endif
!
!
        if(iall.eq.1)then
          qrc(i,k)=0.
          pw(i,k)=(QC(I,K)-QRCH)*zu(i,k)
          if(pw(i,k).lt.0.)pw(i,k)=0.
        endif
        QC(I,K)=QRC(I,K)+qrch
      endif !autoconv
!
!--- integrated normalized ondensate
!
         PWAV(I)=PWAV(I)+PW(I,K)
         Psum(I)=Psum(I)+clw_all(I,K)*zu(i,k) *dz
 100     CONTINUE
       prop_ave=0.
       iprop=0
       do k=kts,kte
        prop_ave=prop_ave+prop_b(k)
        if(prop_b(k).gt.0)iprop=iprop+1
       enddo
       iprop=max(iprop,1)
!      write(11,*)'prop_ave = ',prop_ave/float(iprop)
!      print *,'pwav = ',pwav(1)

   END SUBROUTINE cup_up_moisture
!====================================================================
   SUBROUTINE gdinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,           &
                        MASS_FLUX,cp,restart,                       &
                        P_QC,P_QI,P_FIRST_SCALAR,                   &
                        RTHFTEN, RQVFTEN,                           &
                        APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                        APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                        allowed_to_read,                            &
                        ids, ide, jds, jde, kds, kde,               &
                        ims, ime, jms, jme, kms, kme,               &
                        its, ite, jts, jte, kts, kte               )
!--------------------------------------------------------------------   
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_FIRST_SCALAR, P_QI, P_QC
   REAL,     INTENT(IN)           ::  cp

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHCUTEN, &
                                                          RQVCUTEN, &
                                                          RQCCUTEN, &
                                                          RQICUTEN   

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHFTEN,  &
                                                          RQVFTEN

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) ::        &
                                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,  &
                                APR_CAPMA,APR_CAPME,APR_CAPMI,      &
                                MASS_FLUX

   IF(.not.restart)THEN
        RTHCUTEN=0.
        RQVCUTEN=0.
        RTHFTEN=0.
        RQVFTEN=0.

     IF (P_QC .ge. P_FIRST_SCALAR) THEN
           RQCCUTEN=0.
     ENDIF

     IF (P_QI .ge. P_FIRST_SCALAR) THEN
           RQICUTEN=0.
     ENDIF

        mass_flux=0.

   ENDIF
        APR_GR=0.
        APR_ST=0.
        APR_W=0.
        APR_MC=0.
        APR_AS=0.
        APR_CAPMA=0.
        APR_CAPME=0.
        APR_CAPMI=0.

   END SUBROUTINE gdinit


!--------------------------------------------------------------------

      real function satvap(temp2)
      implicit none
      real :: temp2, temp, toot, toto, eilog, tsot,  &
     &        ewlog, ewlog2, ewlog3, ewlog4
      temp = temp2-273.155
      if (temp.lt.-20.) then   !!!! ice saturation
        toot = 273.16 / temp2
        toto = 1 / toot
        eilog = -9.09718 * (toot - 1) - 3.56654 * (log(toot) / &
     &    log(10.)) + .876793 * (1 - toto) + (log(6.1071) / log(10.))
        satvap = 10 ** eilog
      else
        tsot = 373.16 / temp2
        ewlog = -7.90298 * (tsot - 1) + 5.02808 * &
     &             (log(tsot) / log(10.))
        ewlog2 = ewlog - 1.3816e-07 * &
     &             (10 ** (11.344 * (1 - (1 / tsot))) - 1)
        ewlog3 = ewlog2 + .0081328 * &
     &             (10 ** (-3.49149 * (tsot - 1)) - 1)
        ewlog4 = ewlog3 + (log(1013.246) / log(10.))
        satvap = 10 ** ewlog4
      end if
      return
      end function
   SUBROUTINE CUP_gf_sh(xmb_out,zo,OUTQC,J,AAEQ,T,Q,Z1,                    &
              TN,QO,PO,PRE,P,OUTT,OUTQ,DTIME,ktau,PSUR,US,VS,    &
              TCRIT,                                        &
              ztexec,zqexec,ccn,ccnclean,rho,dx,dhdt,                               &
              kpbl,kbcon,ktop,cupclws,k22,         &   !-lxz
              xland,gsw,tscl_kf,              &
              xl,rv,cp,g,ichoice,ipr,jpr,ierr,ierrc,         &
              autoconv,itf,jtf,ktf,               &
              use_excess,its,ite, jts,jte, kts,kte                                &
                                                )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        autoconv,itf,jtf,ktf,ktau,use_excess,        &
        its,ite, jts,jte, kts,kte,ipr,jpr
     integer, intent (in   )              ::                           &
        j,ichoice
  !
  ! 
  !
     real,    dimension (its:ite,jts:jte)                              &
        ,intent (in   )                   ::                           &
               gsw
  ! outtem = output temp tendency (per s)
  ! outq   = output q tendency (per s)
  ! outqc  = output qc tendency (per s)
  ! pre    = output precip
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                   ::                           &
        cupclws,OUTT,OUTQ,OUTQC
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        pre,xmb_out
     integer,    dimension (its:ite)                                   &
        ,intent (out  )                   ::                           &
        kbcon,ktop,k22
     integer,    dimension (its:ite)                                   &
        ,intent (in  )                   ::                           &
        kpbl
  !
  ! basic environmental input includes moisture convergence (mconv)
  ! omega (omeg), windspeed (us,vs), and a flag (aaeq) to turn off
  ! convection for this call only and at that particular gridpoint
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        rho,T,PO,P,US,VS,tn,dhdt
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
         Q,QO
     real, dimension (its:ite)                                         &
        ,intent (in   )                   ::                           &
        ztexec,zqexec,ccn,Z1,PSUR,AAEQ,xland
       
       real                                                            &
        ,intent (in   )                   ::                           &
        tscl_kf,dx,ccnclean,dtime,tcrit,xl,cp,rv,g


!
!
!***************** the following are your basic environmental
!                  variables. They carry a "_cup" if they are
!                  on model cloud levels (staggered). They carry
!                  an "o"-ending (z becomes zo), if they are the forced
!                  variables. They are preceded by x (z becomes xz)
!                  to indicate modification by some typ of cloud
!
  ! z           = heights of model levels
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! p           = environmental pressure
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! z_cup       = heights of model cloud levels
  ! q_cup       = environmental q on model cloud levels
  ! qes_cup     = saturation q on model cloud levels
  ! t_cup       = temperature (Kelvin) on model cloud levels
  ! p_cup       = environmental pressure
  ! he_cup = moist static energy on model cloud levels
  ! hes_cup = saturation moist static energy on model cloud levels
  ! gamma_cup = gamma on model cloud levels
!
!
  ! hcd = moist static energy in downdraft
  ! zd normalized downdraft mass flux
  ! dby = buoancy term
  ! entr = entrainment rate
  ! zd   = downdraft normalized mass flux
  ! entr= entrainment rate
  ! hcd = h in model cloud
  ! bu = buoancy term
  ! zd = normalized downdraft mass flux
  ! gamma_cup = gamma on model cloud levels
  ! qcd = cloud q (including liquid water) after entrainment
  ! qrch = saturation q in cloud
  ! pwd = evaporate at that level
  ! pwev = total normalized integrated evaoprate (I2)
  ! entr= entrainment rate
  ! z1 = terrain elevation
  ! entr = downdraft entrainment rate
  ! jmin = downdraft originating level
  ! kdet = level above ground where downdraft start detraining
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! pr_ens = precipitation ensemble
  ! xf_ens = mass flux ensembles
  ! massfln = downdraft mass flux ensembles used in next timestep
  ! omeg = omega from large scale model
  ! mconv = moisture convergence from large scale model
  ! zd      = downdraft normalized mass flux
  ! zu      = updraft normalized mass flux
  ! dir     = "storm motion"
  ! mbdt    = arbitrary numerical parameter
  ! dtime   = dt over which forcing is applied
  ! iact_gr_old = flag to tell where convection was active
  ! kbcon       = LFC of parcel from k22
  ! k22         = updraft originating level
  ! icoic       = flag if only want one closure (usually set to zero!)
  ! dby = buoancy term
  ! ktop = cloud top (output)
  ! xmb    = total base mass flux
  ! hc = cloud moist static energy
  ! hkb = moist static energy at originating level

     real,    dimension (its:ite,kts:kte) ::                           &
        entr_rate_2d,mentrd_rate_2d,he,hes,qes,z,                      &
        heo,heso,qeso,zo,                                              &
        xhe,xhes,xqes,xz,xt,xq,                                        &

        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,      &
        qeso_cup,qo_cup,heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,     &
        tn_cup,                                                        &
        xqes_cup,xq_cup,xhe_cup,xhes_cup,xz_cup,xp_cup,xgamma_cup,     &
        xt_cup,                                                        &

        xlamue,dby,qc,qrcd,pwd,pw,hcd,qcd,dbyd,hc,qrc,zu,zd,clw_all,   &
        dbyo,qco,qrcdo,pwdo,pwo,hcdo,qcdo,dbydo,hco,qrco,zuo,zdo,      &
        xdby,xqc,xqrcd,xpwd,xpw,xhcd,xqcd,xhc,xqrc,xzu,xzd,            &

  ! cd  = detrainment function for updraft
  ! cdd = detrainment function for downdraft
  ! dellat = change of temperature per unit mass flux of cloud ensemble
  ! dellaq = change of q per unit mass flux of cloud ensemble
  ! dellaqc = change of qc per unit mass flux of cloud ensemble

        cd,cdd,DELLAH,DELLAQ,DELLAT,DELLAQC,dsubt,dsubh,dsubq,subt,subq

  ! aa0 cloud work function for downdraft
  ! edt = epsilon
  ! aa0     = cloud work function without forcing effects
  ! aa1     = cloud work function with forcing effects
  ! xaa0    = cloud work function with cloud effects (ensemble dependent)
  ! edt     = epsilon

     real,    dimension (its:ite) ::                                   &
       edt,edto,edtx,AA1,AA0,XAA0,HKB,                          &
       HKBO,XHKB,QKB,QKBO,                                    &
       xmbmax,XMB,XPWAV,XPWEV,PWAV,PWEV,PWAVO,                                &
       PWEVO,BU,BUD,BUO,cap_max,xland1,                                    &
       cap_max_increment,closure_n,psum,psumh,sig,zuhe
     integer,    dimension (its:ite) ::                                &
       kzdown,KDET,KB,JMIN,kstabi,kstabm,K22x,        &   !-lxz
       KBCONx,KBx,KTOPx,ierr,ierr2,ierr3,KBMAX

     integer                              ::                           &
       nall,iedt,nens,nens3,ki,I,K,KK,iresult
     real                                 ::                           &
      day,dz,dzo,mbdt,entr_rate,radius,entrd_rate,mentrd_rate,  &
      zcutdown,edtmax,edtmin,depth_min,zkbmax,z_detr,zktop,      &
      massfld,dh,cap_maxs,trash,frh,xlamdd,fsum
      
      real detdo1,detdo2,entdo,dp,subin,detdo,entup,                &
      detup,subdown,entdoj,entupk,detupk,totmas
      real :: power_entr,zustart,zufinal,dzm1,dzp1


     integer :: tun_lim,jprnt,k1,k2,kbegzu,kfinalzu,kstart,jmini,levadj
     logical :: keep_going
     real xff_shal(9),blqe,xkshal
     character*50 :: ierrc(its:ite)
     real,    dimension (its:ite,kts:kte) ::                           &
       up_massentr,up_massdetr,dd_massentr,dd_massdetr                 &
      ,up_massentro,up_massdetro,dd_massentro,dd_massdetro
     real,    dimension (kts:kte) :: smth
      zustart=.1
      zufinal=1.
      levadj=4
      power_entr=2.
      day=86400.
      do i=its,itf
        xmb_out(i)=0.
        xland1(i)=1.
        if(xland(i).gt.1.5)xland1(i)=0.
        cap_max_increment(i)=25.
        ierrc(i)=" "
      enddo
!
!--- initial entrainment rate (these may be changed later on in the
!--- program
!
      entr_rate =.2/200.
      tun_lim=7
      
!
!--- initial detrainmentrates
!
      do k=kts,ktf
      do i=its,itf
        up_massentro(i,k)=0.
        up_massdetro(i,k)=0.
        z(i,k)=zo(i,k)
        xz(i,k)=zo(i,k)
        qrco(i,k)=0.
        cd(i,k)=1.*entr_rate
        dellaqc(i,k)=0.
      enddo
      enddo
!
!--- max/min allowed value for epsilon (ratio downdraft base mass flux/updraft
!
!--- minimum depth (m), clouds must have
!
      depth_min=50.
!
!--- maximum depth (mb) of capping 
!--- inversion (larger cap = no convection)
!
      cap_maxs=25.
      DO i=its,itf
        kbmax(i)=1
        aa0(i)=0.
        aa1(i)=0.
      enddo
      do i=its,itf
          cap_max(i)=cap_maxs
        iresult=0
      enddo
!
!--- max height(m) above ground where updraft air can originate
!
      zkbmax=4000.
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(z,qes,he,hes,t,q,p,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_env(zo,qeso,heo,heso,tn,qo,po,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)

!
!--- environmental values on cloud levels
!
      call cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,he_cup, &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_env_clev(tn,qeso,qo,heo,heso,zo,po,qeso_cup,qo_cup, &
           heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,tn_cup,psur,  &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
        if(ierr(i).eq.0)then
        if(aaeq(i).lt.-0.1)then
           ierr(i)=20
        endif
!
      do k=kts,ktf
        if(zo_cup(i,k).gt.zkbmax+z1(i))then
          kbmax(i)=k
          go to 25
        endif
      enddo
 25   continue
!
      kbmax(i)=min(kbmax(i),ktf-4)
      endif
      enddo

!
!
!
!------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT - K22
!
      CALL cup_MAXIMI(HEO_CUP,3,KBMAX,K22,ierr, &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
       DO 36 i=its,itf
         if(kpbl(i).gt.5)cap_max(i)=po_cup(i,kpbl(i))
         IF(ierr(I).eq.0.)THEN
         IF(K22(I).GT.KBMAX(i))then
           ierr(i)=2
           ierrc(i)="could not find k22"
         endif
            if(kpbl(i).gt.5)then
               k22(i)=kpbl(i)
               ierr(i)=0
               ierrc(i)="reset to zero becausof kpbl"
             endif
         else
             ierrc(i)="why here? "
         endif
!      if(j.eq.jpr .and. i.eq.ipr)write(0,*)'initial k22 = ',k22(ipr),kpbl(i)
 36   CONTINUE
!
!--- DETERMINE THE LEVEL OF CONVECTIVE CLOUD BASE  - KBCON
!

      do i=its,itf
       IF(ierr(I).eq.0.)THEN
         if(use_excess == 2) then
             k1=max(1,k22(i)-1)
             k2=k22(i)+1
             hkb(i) =sum(he_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
             hkbo(i)=sum(heo_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
             qkbo(i)=sum(qo_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)
!            write(0,*)sum(heo_cup(i,k1:k2))/float(k2-k1+1),heo_cup(i,k1),heo(i,k1:k2)
        else if(use_excess <= 1) then
             hkb(i)=he_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
             hkbo(i)=heo_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
             qkbo(i)=qo_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i))
        endif  ! excess
         do k=1,k22(i)
            hkb(i)=max(hkb(i),he_cup(i,k))
            hkbo(i)=max(hkbo(i),heo_cup(i,k))
            qkbo(i)=max(qkbo(i),qo_cup(i,k))
         enddo
       endif ! ierr
      enddo
      call cup_kbcon(ierrc,cap_max_increment,5,k22,kbcon,heo_cup,heso_cup, &
           hkbo,ierr,kbmax,po_cup,cap_max, &
           xl,cp,ztexec,zqexec,use_excess,       &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!--- increase detrainment in stable layers
!
      DO 887 i=its,itf
         IF(ierr(I).eq.0.)THEN
            if(kbcon(i).gt.ktf-4)then
                ierr(i)=231
                go to 887
            endif
            do k=kts,ktf
               frh = min(qo_cup(i,k)/qeso_cup(i,k),1.)
               entr_rate_2d(i,k)=entr_rate*(1.3-frh)
               cd(i,k)=entr_rate_2d(i,k)
            enddo
            zuhe(i)=zustart
            kstart=1
            frh=(zufinal-zustart)/((float(kbcon(i))**power_entr)-(float(kstart)**power_entr))
            dh=zuhe(i)-frh*(float(kstart)**power_entr)
            do k=kstart,kbcon(i)-1
             dz=z_cup(i,k+1)-z_cup(i,k)
             cd(i,k)=0.
             entr_rate_2d(i,k)=((frh*(float((k+1))**power_entr)+dh)/zuhe(i)-1.+cd(i,k)*dz)/dz
             zuhe(i)=zuhe(i)+entr_rate_2d(i,k)*dz*zuhe(i)-cd(i,k)*dz*zuhe(i)
!            if(i.eq.ipr.and.j.eq.jpr)write(0,*)'entr = ',k,entr_rate_2d(i,k),dh,frh,zuhe(i),dz
            enddo
            frh=-(0.1-zuhe(i))/((float(kbcon(i)+tun_lim)**power_entr)-(float(kbcon(i)-1)**power_entr))
            dh=zuhe(i)+frh*(float(kbcon(i))**power_entr)
               do k=kbcon(i),kbcon(i)+tun_lim
                 dz=z_cup(i,k+1)-z_cup(i,k)
                 cd(i,k)=-((-frh*(float((k+1))**power_entr)+dh)/zuhe(i)-1.-entr_rate_2d(i,k)*dz)/dz
                 zuhe(i)=zuhe(i)+entr_rate_2d(i,k)*dz*zuhe(i)-cd(i,k)*dz*zuhe(i)
!            if(i.eq.ipr.and.j.eq.jpr)write(0,*)'entr = ',k,entr_rate_2d(i,k),cd(i,k),zuhe(i)
               enddo
               do k=kbcon(i)+tun_lim+1,ktf
                entr_rate_2d(i,k)=0.
                cd(i,k)=0.
               enddo


        ENDIF
 887  enddo
!
! calculate mass entrainment and detrainment
!
      do k=kts,ktf
      do i=its,itf
         hc(i,k)=0.
         DBY(I,K)=0.
         hco(i,k)=0.
         DBYo(I,K)=0.
      enddo
      enddo
      do i=its,itf
       IF(ierr(I).eq.0.)THEN
         do k=1,kbcon(i)-1
            hc(i,k)=hkb(i)
            hco(i,k)=hkbo(i)
            qco(i,k)=qkbo(i)
         enddo
         k=kbcon(i)
         hc(i,k)=hkb(i)
         qco(i,k)=qkbo(i)
         DBY(I,Kbcon(i))=Hkb(I)-HES_cup(I,K)
         hco(i,k)=hkbo(i)
         DBYo(I,Kbcon(i))=Hkbo(I)-HESo_cup(I,K)
         trash=QESo_cup(I,K)+(1./XL)*(GAMMAo_cup(i,k) &
              /(1.+GAMMAo_cup(i,k)))*DBYo(I,K)
         qrco(i,k)=max(0.,qco(i,k)-trash)
       endif ! ierr
      enddo
!
!
      do 42 i=its,itf
         if(ierr(i).eq.0)then
         zu(i,1)=zustart
         zuo(i,1)=zustart
!    mass entrainment and detrinament is defined on model levels
         do k=2,ktf-1 !kbcon(i)+4 ! ktf-1
          dz=zo_cup(i,k)-zo_cup(i,k-1)
          up_massentro(i,k-1)=entr_rate_2d(i,k-1)*dz*zuo(i,k-1)
          up_massdetro(i,k-1)=cd(i,k-1)*dz*zuo(i,k-1)
          zuo(i,k)=zuo(i,k-1)+up_massentro(i,k-1)-up_massdetro(i,k-1)
          if(zuo(i,k).lt.0.05)then
             zuo(i,k)=.05
             up_massdetro(i,k-1)=zuo(i,k-1)-.05  + up_massentro(i,k-1)
             cd(i,k-1)=up_massdetro(i,k-1)/dz/zuo(i,k-1)
          endif
          zu(i,k)=zuo(i,k)
          up_massentr(i,k-1)=up_massentro(i,k-1)
          up_massdetr(i,k-1)=up_massdetro(i,k-1)
!          zu(i,k)=max(0.01,zu(i,k-1)+up_massentr(i,k-1)-up_massdetr(i,k-1))
         enddo
         do k=kbcon(i)+1,ktf-1
          hc(i,k)=(hc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*hc(i,k-1)+ &
                         up_massentr(i,k-1)*he(i,k-1))   /            &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          dby(i,k)=hc(i,k)-hes_cup(i,k)
          hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+ &
                         up_massentro(i,k-1)*heo(i,k-1))   /            &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
          dbyo(i,k)=hco(i,k)-heso_cup(i,k)
         enddo
         do k=kbcon(i)+1,ktf
          if(dbyo(i,k).lt.0)then
              ktop(i)=k-1
              go to 41
          endif
         enddo
41       continue
         if(ktop(i).lt.kbcon(i)+1)then
            ierr(i)=5
            ierrc(i)='ktop is less than kbcon+1'
             go to 42
         endif
         if(ktop(i).gt.ktf-2)then
             ierr(i)=5
             ierrc(i)="ktop is larger than ktf-2"
             go to 42
         endif
         do k=kbcon(i)+1,ktop(i)
          trash=QESo_cup(I,K)+(1./XL)*(GAMMAo_cup(i,k) &
              /(1.+GAMMAo_cup(i,k)))*DBYo(I,K)
          qco(i,k)=   (qco(i,k-1)*zuo(i,k-1)-.5*up_massdetr(i,k-1)* qco(i,k-1)+ &
                         up_massentr(i,k-1)*qo(i,k-1))   /            &
                         (zuo(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          qrco(i,k)=max(0.,qco(i,k)-trash)
          cupclws(i,k)=qrco(i,k)*.5
         enddo
         do k=ktop(i)+1,ktf
           HC(i,K)=hes_cup(i,k)
           HCo(i,K)=heso_cup(i,k)
           DBY(I,K)=0.
           DBYo(I,K)=0.
           zu(i,k)=0.
           zuo(i,k)=0.
           cd(i,k)=0.
           entr_rate_2d(i,k)=0.
           up_massentr(i,k)=0.
           up_massdetr(i,k)=0.
           up_massentro(i,k)=0.
           up_massdetro(i,k)=0.
         enddo
!        if(i.eq.ipr.and.j.eq.jpr)then
!           write(0,*)'hcnew = '
!           do k=1,ktf
!             write(0,*)k,hco(i,k),dbyo(i,k)
!           enddo
!        endif
      endif
42    continue
!     enddo
!
!--- calculate workfunctions for updrafts
!
      call cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup, &
           kbcon,ktop,ierr,           &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_up_aa0(aa1,zo,zuo,dbyo,GAMMAo_CUP,tn_cup, &
           kbcon,ktop,ierr,           &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
         if(ierr(i).eq.0)then
           if(aa1(i).eq.0.)then
               ierr(i)=17
               ierrc(i)="cloud work function zero"
           endif
         endif
      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!--- change per unit mass that a model cloud would modify the environment
!
!--- 1. in bottom layer
!
      do k=kts,ktf
      do i=its,itf
        dellah(i,k)=0.
        dsubt(i,k)=0.
        dsubh(i,k)=0.
        dellaq(i,k)=0.
        dsubq(i,k)=0.
      enddo
      enddo
!
!----------------------------------------------  cloud level ktop
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level ktop-1
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level k+2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k+1
!
!----------------------------------------------  cloud level k+1
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k
!
!----------------------------------------------  cloud level k
!
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level 3
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 2
!
!----------------------------------------------  cloud level 2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 1

      do i=its,itf
        if(ierr(i).eq.0)then
         dp=100.*(po_cup(i,1)-po_cup(i,2))
             dsubt(i,1)=0. 
             dsubq(i,1)=0. 
         do k=kts+1,ktop(i)
               subin=0.
               subdown=0.
! these three are only used at or near mass detrainment and/or entrainment levels
            entupk=0.
            detupk=0.
! entrainment/detrainment for updraft
            entup=up_massentro(i,k)
            detup=up_massdetro(i,k)
!
!         SPECIAL LEVELS
!
            if(k.eq.ktop(i))then
               detupk=zuo(i,ktop(i))
               subin=0.
               subdown=0.
               entup=0.
               detup=0.
            endif
            totmas=subin-subdown+detup-entup  &
             -entupk+detupk+zuo(i,k+1)-zuo(i,k)
!               print *,'*********************',k,totmas
!              write(0,123)k,subin+zuo(i,k+1),subdown-zuo(i,k),detup,entup, &
!                          detdo,entdo,entupk,detupk
!             write(8,*)'totmas = ',k,totmas
            if(abs(totmas).gt.1.e-6)then
               write(0,*)'*********************',i,j,k,totmas
               print *,jmin(i),k22(i),kbcon(i),ktop(i)
               write(0,123)k,subin,subdown,detup,entup, &
                           entupk,detupk,zuo(i,k+1),zuo(i,k)
123     formAT(1X,i2,10E12.4)
!        call wrf_error_fatal ( 'totmas .gt.1.e-6' )
            endif
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
            dellah(i,k)=(detup*.5*(HCo(i,K+1)+HCo(i,K)) &
                    -entup*heo(i,k) &
                    +subin*heo_cup(i,k+1) &
                    -subdown*heo_cup(i,k) &
                    +detupk*(hco(i,ktop(i))-heo_cup(i,ktop(i)))    &
                    -entupk*heo_cup(i,k22(i)) &
                     )*g/dp
            dellaq(i,k)=(detup*.5*(qco(i,K+1)+qco(i,K)-qrco(i,k+1)-qrco(i,k)) &
                    -entup*qo(i,k) &
                    +subin*qo_cup(i,k+1) &
                    -subdown*qo_cup(i,k) &
                    +detupk*(qco(i,ktop(i))-qrco(i,ktop(i))-qo_cup(i,ktop(i)))    &
                    -entupk*qo_cup(i,k22(i)) &
                     )*g/dp
          
!
! updraft subsidence only
!
           if(k.lt.ktop(i))then
             dsubt(i,k)=(zuo(i,k+1)*heo_cup(i,k+1) &
                    -zuo(i,k)*heo_cup(i,k))*g/dp
             dsubq(i,k)=(zuo(i,k+1)*qo_cup(i,k+1) &
                    -zuo(i,k)*qo_cup(i,k))*g/dp
!          if(i.eq.ipr.and.j.eq.jpr)then
!           write(0,*)'dq3',k,zuo(i,k+1)*heo_cup(i,k+1),zuo(i,k)*heo_cup(i,k)
!          endif
           endif
!
       enddo   ! k

        endif
      enddo
!
!-- take out cloud liquid water for detrainment
!
      do k=kts,ktf-1
      do i=its,itf
       dellaqc(i,k)=0.
       if(ierr(i).eq.0)then
         if(k.eq.ktop(i)-0)dellaqc(i,k)= &
                      .01*zuo(i,ktop(i))*qrco(i,ktop(i))* &
                      9.81/(po_cup(i,k)-po_cup(i,k+1))
         if(k.lt.ktop(i).and.k.gt.kbcon(i))then
           dz=zo_cup(i,k+1)-zo_cup(i,k)
           dellaqc(i,k)=.01*9.81*up_massdetro(i,k)*.5*(qrco(i,k)+qrco(i,k+1))/ &
                        (po_cup(i,k)-po_cup(i,k+1))
         endif
         if(dellaqc(i,k).lt.0)write(0,*)'neg della',i,j,k,ktop(i),qrco(i,k), &
              qrco(i,k+1),up_massdetro(i,k),zuo(i,ktop(i))
         dellaqc(i,k)=max(0.,dellaqc(i,k))
       endif
      enddo
      enddo
!
!--- using dellas, calculate changed environmental profiles
!
      mbdt=3.e-4

      do k=kts,ktf
      do i=its,itf
         dellat(i,k)=0.
         if(ierr(i).eq.0)then
            dsubh(i,k)=dsubt(i,k)
            dellaq(i,k)=dellaq(i,k)+dellaqc(i,k)
            dellaqc(i,k)=0.
            XHE(I,K)=(dsubt(i,k)+DELLAH(I,K))*MBDT+HEO(I,K)
            XQ(I,K)=(dsubq(i,k)+DELLAQ(I,K))*MBDT+QO(I,K)
            DELLAT(I,K)=(1./cp)*(DELLAH(I,K)-xl*DELLAQ(I,K))
            dSUBT(I,K)=(1./cp)*(dsubt(i,k)-xl*dsubq(i,k))
            XT(I,K)= (DELLAT(I,K)+dsubt(i,k))*MBDT+TN(I,K)
            IF(XQ(I,K).LE.0.)XQ(I,K)=1.E-08
         ENDIF
      enddo
      enddo
      do i=its,itf
      if(ierr(i).eq.0)then
      xhkb(i)=hkbo(i)+(dsubh(i,k22(i))+DELLAH(I,K22(i)))*MBDT
      XHE(I,ktf)=HEO(I,ktf)
      XQ(I,ktf)=QO(I,ktf)
      XT(I,ktf)=TN(I,ktf)
      IF(XQ(I,ktf).LE.0.)XQ(I,ktf)=1.E-08
      endif
      enddo
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(xz,xqes,xhe,xhes,xt,xq,po,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!--- environmental values on cloud levels
!
      call cup_env_clev(xt,xqes,xq,xhe,xhes,xz,po,xqes_cup,xq_cup, &
           xhe_cup,xhes_cup,xz_cup,po_cup,gamma_cup,xt_cup,psur,   &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!
!**************************** static control
!
!--- moist static energy inside cloud
!
!     do i=its,itf
!       if(ierr(i).eq.0)then
!         xhkb(i)=xhe(i,k22(i))
!       endif
!     enddo
      do k=kts,ktf
      do i=its,itf
         xhc(i,k)=0.
         xDBY(I,K)=0.
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
!        if(use_excess == 2) then
!            k1=max(1,k22(i)-1)
!            k2=k22(i)+1
!            xhkb(i) =sum(xhe_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
!        else if(use_excess <= 1) then
!            xhkb(i)=xhe_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
!        endif

         do k=1,kbcon(i)-1
            xhc(i,k)=xhkb(i)
         enddo
          k=kbcon(i)
          xhc(i,k)=xhkb(i)
          xDBY(I,Kbcon(i))=xHkb(I)-xHES_cup(I,K)
        endif !ierr
      enddo
!
!
      do i=its,itf
      if(ierr(i).eq.0)then
      xzu(i,:)=zuo(i,:)
      do k=kbcon(i)+1,ktop(i)
       xhc(i,k)=(xhc(i,k-1)*xzu(i,k-1)-.5*up_massdetro(i,k-1)*xhc(i,k-1)+ &
                         up_massentro(i,k-1)*xhe(i,k-1))   /            &
                         (xzu(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
       xdby(i,k)=xhc(i,k)-xhes_cup(i,k)
      enddo
      do k=ktop(i)+1,ktf
           xHC(i,K)=xhes_cup(i,k)
           xDBY(I,K)=0.
           xzu(i,k)=0.
      enddo
      endif
      enddo

!
!--- workfunctions for updraft
!
      call cup_up_aa0(xaa0,xz,xzu,xdby,GAMMA_CUP,xt_cup, &
           kbcon,ktop,ierr,           &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
! now for shallow forcing
!
       do i=its,itf
        xmb(i)=0.
        xff_shal(1:9)=0.
        if(ierr(i).eq.0)then
          xmbmax(i)=0.1  
          xkshal=(xaa0(i)-aa1(i))/mbdt
          if(xkshal.ge.0.)xkshal=+1.e6
          if(xkshal.gt.-1.e-4 .and. xkshal.lt.0.)xkshal=-1.e-4
          xff_shal(1)=max(0.,-(aa1(i)-aa0(i))/(xkshal*dtime))
          xff_shal(1)=min(xmbmax(i),xff_shal(1))
          xff_shal(2)=max(0.,-(aa1(i)-aa0(i))/(xkshal*dtime))
          xff_shal(2)=min(xmbmax(i),xff_shal(2))
          xff_shal(3)=max(0.,-(aa1(i)-aa0(i))/(xkshal*dtime))
          xff_shal(3)=min(xmbmax(i),xff_shal(3))
          if(aa1(i).le.0)then
           xff_shal(1)=0.
           xff_shal(2)=0.
           xff_shal(3)=0.
          endif
          if(aa1(i)-aa0(i).le.0.)then
           xff_shal(1)=0.
           xff_shal(2)=0.
           xff_shal(3)=0.
          endif
! boundary layer QE (from Saulo Freitas)
          blqe=0.
          trash=0.
          if(k22(i).lt.kpbl(i)+1)then
             do k=1,kbcon(i)-1
                blqe=blqe+100.*dhdt(i,k)*(po_cup(i,k)-po_cup(i,k+1))/g
             enddo
             trash=max((hc(i,kbcon(i))-he_cup(i,kbcon(i))),1.e1)
             xff_shal(7)=max(0.,blqe/trash)
             xff_shal(7)=min(xmbmax(i),xff_shal(7))
          else
             xff_shal(7)=0.
          endif
          if(xkshal.lt.-1.1e-04)then ! .and.  &
!            ((aa1(i)-aa0(i).gt.0.) .or. (xff_shal(7).gt.0)))then
          xff_shal(4)=max(0.,-aa0(i)/(xkshal*tscl_KF))
          xff_shal(4)=min(xmbmax(i),xff_shal(4))
          xff_shal(5)=xff_shal(4)
          xff_shal(6)=xff_shal(4)
          else
           xff_shal(4)=0.
           xff_shal(5)=0.
           xff_shal(6)=0.
          endif
!         write(0,888)'i0=',i,j,kpbl(i),blqe,xff_shal(7)
!888       format(a3,3(1x,i3),2e12.4)
          xff_shal(8)= xff_shal(7)
          xff_shal(9)= xff_shal(7)
          fsum=0.
          do k=1,9
           xmb(i)=xmb(i)+xff_shal(k)
           fsum=fsum+1.
          enddo
          xmb(i)=min(xmbmax(i),xmb(i)/fsum)
!         if(i.eq.ipr.and.j.eq.jpr)write(0,*)',ierr,xffs',ierr(i),xff_shal(1:9),xmb(i),xmbmax(i)
          if(xmb(i).eq.0.)ierr(i)=22
          if(xmb(i).eq.0.)ierrc(i)="22"
          if(xmb(i).lt.0.)then
             ierr(i)=21
             ierrc(i)="21"
!            write(0,*)'neg xmb,i,j,xmb for shallow = ',i,j,k22(i),ierr(i)
          endif
        endif
        if(ierr(i).ne.0)then
           k22(i)=0
           kbcon(i)=0
           ktop(i)=0
           xmb(i)=0
           do k=kts,ktf
              outt(i,k)=0.
              outq(i,k)=0.
              outqc(i,k)=0.
           enddo
        else if(ierr(i).eq.0)then
!
! got the mass flux, sanity check, first for heating rates
!
          trash=0.
!         kmaxx=0
          do k=2,ktop(i)
           trash=max(trash,86400.*(dsubt(i,k)+dellat(i,k))*xmb(i))
          enddo
          if(trash.gt.100.)then
             xmb(i)=xmb(i)*100./trash
          endif
          trash=0.
          do k=2,ktop(i)
           trash=min(trash,86400.*(dsubt(i,k)+dellat(i,k))*xmb(i))
          enddo
          if(trash.lt.-100.)then
              xmb(i)=-xmb(i)*100./trash
          endif
!
! sanity check on moisture tendencies: do not allow anything that may allow neg
! tendencies
!
          do k=2,ktop(i)
           trash=q(i,k)+(dsubq(i,k)+dellaq(i,k))*xmb(i)*dtime
          if(trash.lt.1.e-12)then
! max allowable tendency over tendency that would lead to too small mix ratios
!
            trash=(1.e-12 -q(i,k))/((dsubq(i,k)+dellaq(i,k))*dtime)
            xmb(i)=(1.e-12 -q(i,k))/((dsubq(i,k)+dellaq(i,k))*dtime)
          endif
          enddo
          xmb_out(i)=xmb(i)
! 
! final tendencies
!
          do k=2,ktop(i)
           outt(i,k)=(dsubt(i,k)+dellat(i,k))*xmb(i)
           outq(i,k)=(dsubq(i,k)+dellaq(i,k))*xmb(i)
          enddo
        endif
       enddo
!      
! done shallow
!--------------------------done------------------------------
!

   END SUBROUTINE CUP_gf_sh
END MODULE module_cu_gf
