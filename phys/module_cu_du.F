MODULE module_cu_du

   USE module_wrf_error

   REAL    , PARAMETER :: cincap = -10.
   REAL    , PARAMETER :: capemin = 10.
   REAL    , PARAMETER :: dpthmin = 1000.
   REAL    , PARAMETER :: alpha = 0.00002
   REAL    , PARAMETER :: eps = 0.5
   REAL    , PARAMETER :: Vfall = 5.

!--------------------------------------------------------------------

CONTAINS

   SUBROUTINE DUCU(                                          &
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,DT,KTAU,DX                                     &
             ,rho,RAINCV,NCA, PRATEC                         &  ! add PRATEC by zhuxiao
             ,U,V,TH,T,W,dz8w,Z,Pcps,pi                      &
             ,W0AVG                                          &
             ,CP,RD,RV,G,XLV                                 &  ! constant variable
             ,EP2,SVP1,SVP2,SVP3,SVPT0                       &  ! constant variable
             ,STEPCU,CU_ACT_FLAG,warm_rain,CUTOP,CUBOT       &
             ,QV                                             &
            ! optionals
             ,RTHCUTEN,RQVCUTEN                              &
                                                             )

!-------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) ::                            &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   ) :: STEPCU
   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV
   REAL,         INTENT(IN   ) :: CP,RD,RV,G,EP2
   REAL,         INTENT(IN   ) :: SVP1,SVP2,SVP3,SVPT0

   INTEGER,      INTENT(IN   ) :: KTAU           

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(IN   ) ::                                   &
                                                          U, &
                                                          V, &
                                                          W, &
                                                         TH, &
                                                          T, &
                                                         QV, &
                                                       dz8w, &
                                                          z, &
                                                       Pcps, &
                                                        rho, &
                                                         pi
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                                   &
                                                      W0AVG

   REAL,  INTENT(IN   ) :: DT, DX
!
   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(INOUT) ::                           RAINCV  &
                                                    ,PRATEC  

   REAL,    DIMENSION( ims:ime , jms:jme ),                  &
            INTENT(INOUT) ::                            NCA

   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(OUT) ::                              CUBOT, &
                                                      CUTOP    

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                  &
          INTENT(INOUT) :: CU_ACT_FLAG

!
! Optional arguments
!

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),           &
         OPTIONAL,                                           &
         INTENT(INOUT) ::                                    &
                                                   RTHCUTEN, &
                                                   RQVCUTEN

!
! LOCAL VARS

   LOGICAL :: flag_qr, flag_qi, flag_qs

   REAL, DIMENSION( kts:kte ) ::                             &
                                                        U1D, &
                                                        V1D, &
                                                        T1D, &
                                                       TH1D, &
                                                       DZ1D, &
                                                        Z1D, &
                                                       QV1D, &
                                                        P1D, &
                                                      RHO1D, &
                                                    W0AVG1D

   REAL, DIMENSION( kts:kte )::                              &
                                                      DQVDT, &
                                                      DTHDT

   REAL    :: PPRATE,TST,tv,PRS,RHOE,W0,SCR1,DXSQ,RTHCUMAX

   INTEGER :: i,j,k,i_start,i_end,j_start,j_end,sz,NTST,ICLDCK
!
   DXSQ=DX*DX

   NTST=STEPCU
   ICLDCK=MOD(KTAU,NTST)
   IF(ICLDCK.EQ.0 .or. KTAU .eq. 1) then
!
!  Keep away from specified and relaxation zone (should be for just specified and nested bc)
   sz = 1
   i_start=max(ids+sz,its)
   i_end=min(ide-1-sz,ite)
   j_start=max(jds+sz,jts)
   j_end=min(jde-1-sz,jte)
!
     DO J = j_start, j_end
       DO I= i_start, i_end

            DO k=kts,kte
               DQVDT(k)=0.
               DTHDT(k)=0.
            ENDDO
            RAINCV(I,J)=0.
            PRATEC(I,J)=0.
            CUTOP(I,J)=KTS
            CUBOT(I,J)=KTE+1
!
! assign vars from 3D to 1D

            DO K=kts,kte
               U1D(K) =U(I,K,J)
               V1D(K) =V(I,K,J)
               T1D(K) =T(I,K,J)
               TH1D(K) =TH(I,K,J)
               RHO1D(K) =rho(I,K,J)
               QV1D(K)=QV(I,K,J)
               IF ( QV1D(K) .LT. 1.E-08 ) QV1D(K) = 1.E-08
               P1D(K) =Pcps(I,K,J)
               W0AVG1D(K) =W0AVG(I,K,J)
               DZ1D(k)=dz8w(I,K,J)
               Z1D(k)=z(I,K,J)
            ENDDO
            CALL DUCU1D(I, J,                       &
                 U1D,V1D,T1D,QV1D,P1D,DZ1D,Z1D,     &
                 W0AVG1D,DT,DX,DXSQ,RHO1D,TH1D,     &
                 XLV,CP,RD,RV,G,                    &
                 EP2,SVP1,SVP2,SVP3,SVPT0,          &
                 DQVDT,DTHDT,                       &
                 PPRATE,NCA,NTST,                   &
                 CUTOP,CUBOT,                       &
                 ids,ide, jds,jde, kds,kde,         &
                 ims,ime, jms,jme, kms,kme,         &
                 its,ite, jts,jte, kts,kte)
            IF(PRESENT(rthcuten).AND.PRESENT(rqvcuten)) THEN
              DO K=kts,kte
                 RTHCUTEN(I,K,J)=DTHDT(K)
                 RQVCUTEN(I,K,J)=DQVDT(K)
              ENDDO
              PRATEC(I,J)=PPRATE
              RAINCV(I,J)=PPRATE*DT              
            ENDIF
       ENDDO
     ENDDO
   ENDIF
!
   END SUBROUTINE DUCU
! ****************************************************************************
!-----------------------------------------------------------
   SUBROUTINE DUCU1D (I, J,                           &
                      U0,V0,T0,QV0,P0,DZQ,Z,W0AVG1D,       &
                      DELT,DX,DXSQ,rhoe,TH0,               &
                      XLV,CP,RD,RV,G,                      &
                      EP2,SVP1,SVP2,SVP3,SVPT0,            &
                      DQVDT,DTHDT,                         &
                      PPRATE,NCA,NTST,                     &
                      CUTOP,CUBOT,                         &
                      ids,ide, jds,jde, kds,kde,           &
                      ims,ime, jms,jme, kms,kme,           &
                      its,ite, jts,jte, kts,kte)
!-----------------------------------------------------------
!
      IMPLICIT NONE
!-----------------------------------------------------------
      INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde, &
                                ims,ime, jms,jme, kms,kme, &
                                its,ite, jts,jte, kts,kte, &
                                I,J,NTST

!
      REAL, DIMENSION( kts:kte ),                          &
            INTENT(IN   ) ::                           U0, &
                                                       V0, &
                                                       T0, &
                                                      TH0, &
                                                      QV0, &
                                                       P0, &
                                                     rhoe, &
                                                      DZQ, &
                                                        Z, &
                                                  W0AVG1D
!
      REAL,  INTENT(IN   ) :: DELT,DX,DXSQ
!

      REAL,  INTENT(IN   ) :: XLV,CP,RD,RV,G
      REAL,  INTENT(IN   ) :: EP2,SVP1,SVP2,SVP3,SVPT0

!
      REAL, DIMENSION( kts:kte ), INTENT(INOUT) ::         &
                                                    DQVDT, &
                                                    DTHDT

      REAL,    DIMENSION( ims:ime , jms:jme ),             &
            INTENT(INOUT) ::                          NCA

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(OUT) ::                          CUBOT, &
                                                    CUTOP
      REAL,  INTENT(OUT  ) :: PPRATE
!
!...DEFINE LOCAL VARIABLES...
!
      REAL, DIMENSION( kts:kte ) :: cond,h,hs,qs,x
      REAL    :: buoy,cape,cin,dh,dq,dt,dtm,ep,es, &
                 evap,hp,mp,qp,qsp,rrk,rrkp, &
                 tadp,tdp,zb,zg,zi,zt
      INTEGER :: ipos,isat,k,kb,ki,kt
!
!...DEFINE PROFILES
      DO k=kts,kte
        h(k)=cp*t0(k)+g*z(k)+xlv*qv0(k)
        es=1000.*svp1*EXP(svp2*(t0(k)-svpt0)/(t0(k)-svp3))
        qs(k)=ep2*es/(p0(k)-es)
        hs(k)=cp*t0(k)+g*z(k)+xlv*qs(k)
        x(k)=xlv*xlv*qs(k)/(cp*rv*t0(k)*t0(k))
        dthdt(k)=0.
        dqvdt(k)=0.
      ENDDO
      pprate=0.
      zg=z(1)-0.5*dzq(1)
!
!...LOOP OVER PARCELS
      loop_origin: DO ki=kts,kte
        hp=h(ki)
        qp=qv0(ki)
        mp=alpha*rhoe(ki)*dzq(ki)
        zi=z(ki)
        buoy=0.
        cape=0.
        cin=0.
        dtm=0.
        isat=0
        ipos=0
        kt=0
        kb=0
        zt=0.
        zb=0.
        cond=0.
!
!...LIFT PARCEL
        loop_lift: DO k=ki+1,kte
          tadp=t0(ki)+(g/cp)*(z(ki)-z(k))
          ep=p0(k)*qv0(ki)/(ep2+qv0(ki))
          tdp=(svpt0-(svp3/svp2)*ALOG(0.001*ep/svp1))/(1.-(1./svp2)*ALOG(0.001*ep/svp1))
          IF(tadp.GE.tdp)THEN
!         unsaturated
            IF(isat.EQ.1)THEN
              print *,i,j,'sounding warning: unsat above sat'
            ENDIF
            dt=tadp-t0(k)
            cond(k)=0.
          ELSE
!         saturated
            IF(isat.EQ.0)THEN
              kb=k
              zb=z(k)-0.5*dzq(k)
            ENDIF
            isat=1
            dh=hp-hs(k)
            dt=(dh/cp)/(1.+x(k))
            qsp=qs(k)+(dh/xlv)*x(k)/(1.+x(k))
!...CONDENSATE PRODUCED
            cond(k)=mp*(qp-qsp)
            qp=qsp
          ENDIF
          buoy=buoy+g*dt*dzq(k)/t0(k)
          cape=max(cape,buoy)
          IF(buoy.GE.cincap)cin=min(cin,buoy)
          IF(dt .GE. 0.)THEN
            kt=k
            zt=z(k)+0.5*dzq(k)
          ELSE IF(dt .LT. 0. .AND. dtm .GE. 0.)THEN
! cloud top is level closest to parcel temperature
            IF(abs(dt) .LT. abs(dtm))THEN
              kt=k
              zt=z(k)+0.5*dzq(k)
            ENDIF
          ENDIF
          dtm=dt
! continue lifting until buoyancy is gone
          IF(buoy.LT.cincap)THEN
            EXIT loop_lift
          ENDIF
          IF(buoy.GT.0.)THEN
!         positive area detected
            ipos=1
          ENDIF
          IF(k.EQ.1)THEN
            kt=k
            zt=z(k)+0.5*dzq(k)
            zi=z(ki)
            print *,'sounding warning: cloud top at model top'
          ENDIF
        ENDDO loop_lift
!
!...CHECK FOR CLOUD
        IF(isat.EQ.0)THEN
!       no cloud from lifting - no convection
          CYCLE loop_origin
        ENDIF
        IF(zt-zb.LE.dpthmin)THEN
!       not more than one cloud level - no convection
          CYCLE loop_origin
        ENDIF
        IF(ipos.EQ.0)THEN
!       no buoyancy in cloud - no convection
          CYCLE loop_origin
        ENDIF
        IF(cape.LE.capemin)THEN
!       not enough cape
          CYCLE loop_origin
        ENDIF
!
!...IF CHECK FOR CLOUD SUCCESSFUL
!
!...DETRAINMENT
        dh=hp-hs(kt)
        dt=(dh/cp)/(1.+x(kt))
        dq=qs(kt)+(dh/xlv)*x(kt)/(1.+x(kt))-qv0(kt)
        dthdt(kt)=dthdt(kt)+mp*(th0(kt)/t0(kt))*dt/(rhoe(kt)*dzq(kt))
        dqvdt(kt)=dqvdt(kt)+mp*dq/(rhoe(kt)*dzq(kt))
!
!...SUBSIDENCE
        loop_subsidence: DO k=kt-1,ki,-1 
          dthdt(k)=dthdt(k)+mp*(th0(k+1)-th0(k))/(rhoe(k)*dzq(k))
          dqvdt(k)=dqvdt(k)+mp*(qv0(k+1)-qv0(k))/(rhoe(k)*dzq(k))
        ENDDO loop_subsidence
!
!...RAINFALL AND EVAPORATION
        rrkp=0.
        loop_rainfall: DO k=kt,1,-1
          rrk=rrkp+cond(k)
          evap=dzq(k)*rrkp/Vfall*eps*(qs(k)-qv0(k))

! restrict evap to below cloud base
          IF(k.GE.kb) evap=0.
          evap=min(rrk,evap)
          rrk= rrk-evap

          dqvdt(k)=dqvdt(k)+evap/(rhoe(k)*dzq(k))
          dthdt(k)=dthdt(k)-(xlv/cp)*(th0(kt)/t0(kt))*evap/(rhoe(k)*dzq(k))
          rrkp=rrk
        ENDDO loop_rainfall
        pprate=pprate+rrkp

      ENDDO loop_origin

!-----------------------------------------------------------------------
   END SUBROUTINE DUCU1D
! ***********************************************************************
!====================================================================
   SUBROUTINE ducuinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,         &
                     RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QC,P_QR,         &
                     SVP1,SVP2,SVP3,SVPT0,                          &
                     P_FIRST_SCALAR,restart,allowed_to_read,        &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_QC,P_QR,P_FIRST_SCALAR

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHCUTEN, &
                                                          RQVCUTEN, &
                                                          RQCCUTEN, &
                                                          RQRCUTEN, &
                                                          RQICUTEN, &
                                                          RQSCUTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   INTEGER :: i, j, k, itf, jtf, ktf
   REAL, INTENT(IN)    :: SVP1,SVP2,SVP3,SVPT0

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RTHCUTEN(i,k,j)=0.
         RQVCUTEN(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

      IF (P_QC .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQCCUTEN(i,k,j)=0.
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      DO j=jts,jtf
      DO i=its,itf
         NCA(i,j)=-100.
      ENDDO
      ENDDO

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         W0AVG(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

   endif
 
   END SUBROUTINE ducuinit

END MODULE module_cu_du

