!#define DENG_SHCU1D
!#define DENG_SHCU1D_subsidence
MODULE module_shcu_deng

  USE module_wrf_error
  
  REAL    , PARAMETER ::    TO           = 263.15
! Lookup table variables:
      INTEGER, PARAMETER :: KFNT=250,KFNP=220
      REAL, DIMENSION(KFNT,KFNP),PRIVATE, SAVE :: TTAB,QSTAB
      REAL, DIMENSION(KFNP),PRIVATE, SAVE :: THE0K
      REAL, DIMENSION(200),PRIVATE, SAVE :: ALU
      REAL, PRIVATE, SAVE :: RDPR,RDTHK,PLUTOP

CONTAINS

   SUBROUTINE deng_shcu_driver(                              &
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,DT,KTAU,DX, xtime, gmt                         &
             ,XLV, XLS,XLV0,XLV1,XLS0,XLS1,CP,R,G            &  ! Constants
             ,SVP1,SVP2,SVP3,SVPT0                           &  ! Constants
             ,ADAPT_STEP_FLAG, DSIGMA                             & ! in
             ,XLONG, ht, PBLH                     & ! in 2D
             ,U,V,w,TH,T,QV,QC,QR,dz8w,PCPS,rho,z_at_w,pi            & ! in 3D
             ,tke, kth, bbls                                      &
             ,ten_radl, ten_rads                       & ! in 3D
             ,rainsh, rainshv, rainshvb, pblmax, capesave         & ! inout 2D
             ,xtime1, radsave, clddpthb, cldtopb, MAVAIL, PBLHAVG & ! inout 2D
             ,ainckfsa, ltopb, kdcldtop, kdcldbas                 & ! inout 2D
             ,W0AVG, TKEAVG, cldareaa, cldareab                   & ! inout 3D
             ,cldliqa, cldliqb, cldfra_sh, ca_rad, cw_rad, wub    & ! inout 3D
             ,RUSHTEN, RVSHTEN, RTHSHTEN, RQVSHTEN, RQCSHTEN      & ! inout 3D
             ,RQRSHTEN, RDCASHTEN, RQCDCSHTEN                     & ! inout 3D
                                                           )
!-------------------------------------------------------------
   IMPLICIT NONE
!  ------------ IN ---------------------------------------------
   INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte  
   REAL,         INTENT(IN   ) :: DT, DX, xtime, gmt
   INTEGER,      INTENT(IN   ) :: KTAU
   REAL,         INTENT(IN   ) :: XLV, XLS,XLV0,XLV1,XLS0,XLS1,CP,R,G,  &
                                  SVP1,SVP2,SVP3,SVPT0
   LOGICAL ,     INTENT(IN   ) :: adapt_step_flag
   REAL,         DIMENSION( kms:kme ), INTENT(IN   ) :: dsigma

   REAL,         DIMENSION( ims:ime , jms:jme ),                &
                 INTENT(IN   ) ::  xlong, ht, pblh
   REAL,         DIMENSION( ims:ime , kms:kme , jms:jme )     ,        &
                 INTENT(IN   ) ::  U, V, W, TH, T, QV, QR, dz8w, Pcps, &
                                   rho, z_at_w, pi, tke,            &
                                   kth, bbls, ten_radl, ten_rads
!  ------------ INOUT ------------------------------------------
   REAL,         DIMENSION( ims:ime , jms:jme ),                           &
                 INTENT(INOUT) :: RAINSHV, RAINSH, pblmax, cldtopb, clddpthb,  &
                                  rainshvb, capesave, xtime1, radsave,   &
                                  MAVAIL, PBLHAVG

   REAL,         DIMENSION( ims:ime , 1:100, jms:jme ),          &
                 INTENT(INOUT) ::  ainckfsa
   INTEGER,      DIMENSION( ims:ime , jms:jme ),                 &
                 INTENT(INOUT) :: ltopb, kdcldtop, kdcldbas
   REAL,         DIMENSION( ims:ime , kms:kme , jms:jme )         , &
                 INTENT(INOUT) :: W0AVG, TKEAVG, QC,                &
                                  cldareaa, cldareab, cldliqa, cldliqb,  wub 
   REAL,         DIMENSION( ims:ime , kms:kme , jms:jme )         , &
                 INTENT(  OUT) :: ca_rad, cw_rad, cldfra_sh
   REAL,         DIMENSION( ims:ime , kms:kme , jms:jme ),           &
                 INTENT(INOUT) :: RUSHTEN, RVSHTEN, RTHSHTEN, RQVSHTEN, RQCSHTEN, &
                                  RQRSHTEN, RDCASHTEN, RQCDCSHTEN
!
!  Local Variables
!
   REAL, DIMENSION( its:ite , jts:jte )        :: CAPEI, RADIUSC, AINCKFI
   REAL, DIMENSION( kts:kte )                  :: U1D, V1D, TH1D, T1D, DZ1D, QV1D, &
                                                  QC1D, RH1D, QR1D, P1D, z1d, RHO1D, W0AVG1D,  &
                                                  kth1D, tke1d, bbls1D,            &
#ifdef DENG_SHCU1D
                                                  ca_rad1d,                     &
#endif
                                                  ten_radl1d, ten_rads1d   
   REAL, DIMENSION( kts:kte+1 ) :: z_at_w1d
   REAL, DIMENSION( kts:kte )   :: QVTEN, QCTEN, QRTEN, TTEN, DCATEN, QCDCTEN
   REAL, DIMENSION( its:ite , jts:jte           ) :: pblh_tile, pblhavg_tile
   REAL, DIMENSION( its:ite , kts:kte , jts:jte ) :: w_at_half, w_at_half_mean
   REAL, DIMENSION( its:ite , kts:kte , jts:jte ) :: tke_tile, tkeavg_tile
   REAL, DIMENSION( its:ite , kts:kte , jts:jte ) :: cldareac, cldliqc, rh
 
   REAL                 :: dt2, DXSQ
   INTEGER              :: i, j, k, ii, ntst, kk
   REAL                 :: AMOIS, time_period_avg_min ! time window to perform a running mean value
   INTEGER              :: if_avg_w=1, if_avg_pbl=0, if_avg_tke=0
   REAL                 :: GNUHF, OMUHF
   REAL                 :: es, qes, alf1, alf2, alf3, c1, c2, cs, qctot
#ifdef DENG_SHCU1D_subsidence
   REAL,  DIMENSION(100)   :: P_P, WSUBS_P
   REAL,  DIMENSION(kte)   :: P_SIGMA, WSUBSID, WSUBSID_td, thten_sub
   REAL,  DIMENSION(kte+1) :: th_full
#endif
   INTEGER :: kx, kxp1, kl

   dxsq = dx*dx
   dt2  = 2.0*dt
   GNUHF = 0.1
   OMUHF = 1.-2.*GNUHF
   kx = kte
   kl = kte
   kxp1 = kx + 1

!   print*, ids, ide, jds, jde, kds, kde
!   print*, ims, ime, jms, jme, kms, kme
!   print*, its, ite, jts, jte, kts, kte

#ifdef DENG_SHCU1D
    write(301,*) xtime/60.,pblh(1,1)
#ifdef DENG_SHCU1D_subsidence
      OPEN(3,FILE='input03.dat',STATUS='old')
      READ(3,*)
      DO K=1,35
        READ(3,*) P_P(K),WSUBS_P(K)     ! SUBSIDENCE IN CM/SEC. (from Betts 1994)
      ENDDO
      CLOSE(3)
#endif
#endif
! 
! Time averaging w on the half levels
!
  IF( if_avg_w == 1 ) THEN
    time_period_avg_min = 10.0
    DO J = jts,jte
    DO I = its,ite
    DO k = kts,kte
       w_at_half(i,k,j) = 0.5* ( w(i,k,j) + w(i,k+1,j) ) 
       w_at_half_mean(i,k,j) = w0avg(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    CALL time_avg_3d( dt, ADAPT_STEP_FLAG, time_period_avg_min &
               ,w_at_half, w_at_half_mean                      &
               ,its,ite, jts,jte, kts,kte                      &
                                                             )
    DO J = jts,jte
    DO I = its,ite
    DO k = kts,kte
       w0avg(i,k,j) = w_at_half_mean(i,k,j)
    ENDDO
    ENDDO
    ENDDO
  ELSE
    DO J = jts,jte
    DO I = its,ite
    DO k = kts,kte
       w_at_half(i,k,j) = 0.5* ( w(i,k,j) + w(i,k+1,j) )
       w0avg(i,k,j) = w_at_half(i,k,j)
    ENDDO
    ENDDO
    ENDDO
  ENDIF
!
! Time averaging PBLH
!
  IF( if_avg_pbl == 1 ) THEN
    time_period_avg_min = 120.0
    DO J = jts,jte
    DO I = its,ite
      pblh_tile(i,j) = pblh(i,j)
      pblhavg_tile(i,j) = pblhavg(i,j)
    ENDDO
    ENDDO
    CALL time_avg_2d( dt, ADAPT_STEP_FLAG, time_period_avg_min &
               ,pblh_tile, pblhavg_tile                        &
               ,its,ite, jts,jte                               &
                                                             )
    DO J = jts,jte
    DO I = its,ite
       pblhavg(i,j) = pblhavg_tile(i,j)
    ENDDO
    ENDDO
  ELSE
    DO J = jts,jte
    DO I = its,ite
       pblh_tile(i,j) = pblh(i,j)
       pblhavg(i,j) = pblh_tile(i,j)
    ENDDO
    ENDDO
  ENDIF
#ifdef DENG_SHCU1D
  write(302,*) xtime/60.,PBLHAVG(1,1)
#endif
!
! Time averaging TKE 
!
  IF( if_avg_tke == 1 ) THEN
    time_period_avg_min = 30.0
    DO J = jts,jte
    DO I = its,ite
    DO k = kts,kte
       tke_tile(i,k,j)    = tke(i,k,j)
       tkeavg_tile(i,k,j) = tkeavg(i,k,j)
    ENDDO
    ENDDO
    ENDDO
    CALL time_avg_3d( dt, ADAPT_STEP_FLAG, time_period_avg_min &
               ,tke_tile, tkeavg_tile                               &
               ,its,ite, jts,jte, kts,kte                      &
                                                               )
    DO J = jts,jte
    DO I = its,ite
    DO k = kts,kte
       tkeavg(i,k,j) = tkeavg_tile(i,k,j)
    ENDDO
    ENDDO
    ENDDO
  ELSE
    DO J = jts,jte
    DO I = its,ite
    DO k = kts,kte
       tke_tile(i,k,j) = tke(i,k,j)
       tkeavg(i,k,j)   = tke_tile(i,k,j)
    ENDDO
    ENDDO
    ENDDO
  ENDIF
!
!  Calculate the RH-based cloud fraction, CS, based on Xu and Randall (1996, JAS)  
!  As shown in Deng et al. 2003a, the virtual cloud fraction CAREA is a wighted average
!  of Deng scheme-predicted cloud fraction, CLDLIQA and CS.  This effective cloud
!  fraction is used in the radiation calculation.
!
  DO J = jts,jte
  DO I = its,ite
  DO k = kts,kte
     IF( T(i,k,j) .GE. TO ) THEN
       ES=1.E3*SVP1*EXP(SVP2*(T(i,k,j)-SVPT0)/(T(i,k,j)-SVP3))
     ELSE
       ES=611.0*EXP(22.514-6.15E3/T(i,k,j))
     ENDIF
     QES = 0.622*ES/(PCPS(i,k,j)-ES)
     RH(i,k,j)=AMAX1(QV(i,k,j)/QES,1.0E-25)

     alf1 = 0.25
     alf2 = 0.49
     alf3 = 100.0
     IF( RH(I,K,J) .LT. 0.999) THEN
       C1=(1.0-RH(I,K,J))*QES**alf2
       QCTOT=CLDLIQA(I,K,J)*CLDAREAA(I,K,J)+QC(I,K,J)
       C2=-alf3*QCTOT/C1
       CS=RH(I,K,J)**alf1*(1.0-EXP(C2))
     ELSE
       CS=1.0
     ENDIF

     CA_RAD(I,K,J)=(1.0-CLDAREAA(I,K,J))*CS+CLDAREAA(I,K,J)
     CLDFRA_SH(I,K,J)= CA_RAD(I,K,J)
!    CW_RAD(I,K,J)=CLDLIQA(I,K,J)+QC(I,K,J)
     CW_RAD(I,K,J)=CLDLIQA(I,K,J)*CLDAREAA(I,K,J) + QC(I,K,J)
  ENDDO
  ENDDO
  ENDDO

  DO J = jts,jte
  DO I = its,ite
            DO k=kts,kte
               QVTEN(k)=0.
               QCTEN(k)=0.
               QRTEN(k)=0.
               TTEN(k)=0.
               DCATEN(k)=0.
               QCDCTEN(k)=0.
            ENDDO
            RAINSHV(I,J)=0.
            RAINSH(I,J)=0.

            DO K=kts,kte
               U1D(K) =U(I,K,J)
               V1D(K) =V(I,K,J)
               TH1D(K) =TH(I,K,J)
               T1D(K) =T(I,K,J)
               W0AVG1D(k) = W0AVG(i,k,j)
               tke1d(k)  = tkeavg(i,k,j)
               RHO1D(K) =rho(I,K,J)
               QV1D(K)=QV(I,K,J)
               RH1D(K)=RH(I,K,J)
#ifdef DENG_SHCU1D
               ca_rad1d(k) = ca_rad(i,k,j)
#endif
               QC1D(K)=QC(I,K,J)
               QR1D(K)=QR(I,K,J)
               P1D(K) =Pcps(I,K,J)
               DZ1D(k)=dz8w(I,K,J)
               kth1d(k)  = kth(i,k,j)
               bbls1d(k) = bbls(i,k,j)
               ten_radl1d(k) = ten_radl(i,k,j)
               ten_rads1d(k) = ten_rads(i,k,j)
            ENDDO

            DO K=kts,kte+1
               z_at_w1d(k) = z_at_w(I,K,J)
            ENDDO

#ifdef DENG_SHCU1D_subsidence
      DO K=1,KL
        kk = KL - K + 1
        P_SIGMA(K) = p1d(kk)/100.0
      ENDDO
      CALL INTERP1D( P_SIGMA, P_P, WSUBS_P, WSUBSID_td, KX, 35 )
      DO K = 1,KX
        kk = KX - K + 1
        WSUBSID(K) = WSUBSID_td(KK)/100.0    ! convert to m/s (READ IN INPUT2.F)
      ENDDO
      DO K = 2,KX
        th_full(K) = 0.5*( th1d(k)+th1d(k-1) )
      ENDDO
      DO K = 2,KX-1
        thten_sub(K) = - WSUBSID(K) * ( th_full(K+1)-th_full(K) ) / dz1d(k)
      ENDDO
      thten_sub(1)  = 0.0
      thten_sub(kx) = 0.0
#endif
   CALL deng_shcu(I, J,                               &
                 ids,ide, jds,jde, kds,kde,           &
                 ims,ime, jms,jme, kms,kme,           &
                 its,ite, jts,jte, kts,kte,           &
                 XLV,XLS,XLV0,XLV1,XLS0,XLS1,CP,R,G,  &
                 SVP1,SVP2,SVP3,SVPT0,                &
                 DT, ktau, dt2, DX, DXSQ, xtime, gmt, &
                 PBLHAVG(i,j), ht(i,j), xlong, capesave, radsave, ainckfsa,  &
                 U1D,V1D,T1D,QV1D,rh1d,QC1D,QR1D,p1d,RHO1D,z_at_w1d,DZ1D,W0AVG1D,  &
#ifdef DENG_SHCU1D
                 ca_rad1d, &    ! for 1d printing only
#endif
                 tke1d, kth1d, bbls1d, ten_radl1d, ten_rads1d, dsigma,       &
                 RAINSHV, RAINSH, pblmax, CLDTOPB, CLDDPTHB,    &
                 ltopb, kdcldtop, kdcldbas,                     &
                 cldareab, cldliqb, wub,                        &
                 CAPEI, RADIUSC, AINCKFI,                       &
                 QVTEN,QCTEN,QRTEN,TTEN,DCATEN,QCDCTEN          &
                                                    )

       DO K=kts,kte
         RUSHTEN(i,k,j) = 0.0
         RVSHTEN(i,k,j) = 0.0
         RTHSHTEN(i,k,j) = TTEN(k)/pi(I,K,J)  * 1.0
#ifdef DENG_SHCU1D_subsidence
         RTHSHTEN(i,k,j) = RTHSHTEN(i,k,j) + thten_sub(k)
#endif
         RQVSHTEN(i,k,j) = QVTEN(k)           * 1.0
         RQCSHTEN(i,k,j) = QCTEN(k)           * 1.0
         RQRSHTEN(i,k,j) = QRTEN(k)           * 1.0
         RDCASHTEN(i,k,j) = DCATEN(K)         * 1.0
         RQCDCSHTEN(i,k,j) = QCDCTEN(K)       * 1.0
       ENDDO
!
!  IF THERE IS CONVECTIVE PRECIPITATION, UPDATE THE SURFACE MOISTURE
!  AVAILABILITY THAT THE SURFACE IS WET FOR THE FOLLOWING HOUR
!
        IF(RAINSHV(I,J) .EQ. 0.0 .AND. RAINSHVB(I,J) .NE. 0.0) THEN
          XTIME1(I,J)=XTIME+60.
        ENDIF

        AMOIS = MAVAIL(I,J)
        IF(RAINSHV(I,J) .NE. 0.0 .OR. XTIME .LE. XTIME1(I,J)) THEN
          MAVAIL(I,J)=AMOIS+0.67*(1.0-AMOIS)
        ELSE
          MAVAIL(I,J)=AMOIS
        ENDIF

        RAINSHVB(I,J)=RAINSHV(I,J)
        DO II = 100,2,-1
          AINCKFSA(i,ii,j) = AINCKFSA(i,ii-1,j)
        ENDDO
        RADSAVE(I,J)=RADIUSC(i,j)
        AINCKFSA(i,1,j) = AINCKFI(i,j)
        CAPESAVE(i,j) = CAPEI(i,j)

       ENDDO     ! i-loop
     ENDDO       ! j-loop
!
! NBC prediction using the MM5 leapfog method
!
    DO J = jts,jte
    DO I = its,ite
    DO K = kts,kte
      CLDAREAC(I,K,J) = CLDAREAB(I,K,J) + RDCASHTEN(I,K,J) * DT
      CLDLIQC(I,K,J) = CLDLIQB(I,K,J) + RQCDCSHTEN(I,K,J) * DT
      CLDAREAC(I,K,J) = AMAX1(0.0,CLDAREAC(I,K,J))
      CLDLIQC(I,K,J) = AMAX1(0.0,CLDLIQC(I,K,J))
      IF( CLDAREAC(I,K,J) .GT. 0.99999 ) THEN
        CLDLIQC(I,K,J) = CLDLIQC(I,K,J)*CLDAREAC(I,K,J)
        CLDAREAC(I,K,J) = 1.0
      ENDIF
      IF( CLDAREAC(I,K,J) .LE. 1.0e-17 .OR. CLDLIQC(I,K,J) .LE. 1.0e-17 ) THEN
        CLDAREAC(I,K,J) = 0.0
        CLDLIQC(I,K,J) = 0.0
      ENDIF
    ENDDO
    ENDDO
    ENDDO

    DO J = jts,jte
    DO I = its,ite
    DO K = kts,kte
      CLDAREAB(I,K,J)=OMUHF*CLDAREAA(I,K,J)+GNUHF*(CLDAREAB(I,K,J)+CLDAREAC(I,K,J))
       CLDLIQB(I,K,J)=OMUHF*CLDLIQA(I,K,J) +GNUHF*(CLDLIQB(I,K,J)+CLDLIQC(I,K,J))
      CLDAREAA(I,K,J)=CLDAREAC(I,K,J)
       CLDLIQA(I,K,J)= CLDLIQC(I,K,J)

      IF( CLDAREAA(I,K,J) .LE. 1.0e-17 .OR. CLDLIQA(I,K,J) .LE. 1.0e-17 ) THEN
        CLDAREAA(I,K,J) = 0.0
        CLDLIQA(I,K,J) = 0.0
      ENDIF

      IF(CLDDPTHB(I,J) .LE. 0.0) THEN   ! No active updraft
        IF( CLDAREAB(I,K,J) .LE. 1.0e-3 .OR. CLDLIQB(I,K,J) .LE. 1.0e-17 ) THEN
  !        QC(I,K,J)=QC(I,K,J)+CLDAREAB(I,K,J)*CLDLIQB(I,K,J)
          RQCSHTEN(I,K,J)=RQCSHTEN(I,K,J)+CLDAREAB(I,K,J)*CLDLIQB(I,K,J)/DT
          CLDAREAB(I,K,J) = 0.0
          CLDLIQB(I,K,J) = 0.0
        ENDIF
      ENDIF
    ENDDO
    ENDDO
    ENDDO

  END SUBROUTINE deng_shcu_driver

!================
      SUBROUTINE deng_shcu(I,J,                                    & ! in
                 ids,ide, jds,jde, kds,kde,                        & ! in
                 ims,ime, jms,jme, kms,kme,                        & ! in
                 its,ite, jts,jte, kts,kte,                        & ! in
                 XLV,XLS,XLV0,XLV1,XLS0,XLS1,CP,R,G,               & ! in
                 SVP1,SVP2,SVP3,SVPT0,                             & ! in
                 DT, ktau, dt2, DX, DXSQ, xtime, gmt, zpbl, ht,    & ! in
                 xlong, capesave, radsave, ainckfsa,               & ! in 2D
                 U0,V0,T0,Q0,RH0, QC0_expl,QR0,P0,             & ! in 3D
                 RHOE,z_at_w0, DZQ,W0AVG0,                         & ! in 1D
#ifdef DENG_SHCU1D
                 ca_rad0, &    ! for 1d printing only
#endif
                 tke0, kth0, bbls0, ten_radl0, ten_rads0, dsigma,  & ! in 1D
                 RAINSHV, RAINSH, pblmax, CLDTOPB, CLDDPTHB, & ! inout 2D
                 ltopb, kdcldtop, kdcldbas,                  & ! inout 2D
                 cldareab, cldliqb, wub,                     & ! inout 3D
                 CAPEI,RADIUSC, AINCKFI,                 & ! out 2D
                 QVTEN,QCTEN,QRTEN,TTEN,DCATEN,QCDCTEN   & ! out 1D (output tendencie)
                                                    )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE
!     -----------  IN -------------------------------------------
      INTEGER, INTENT(IN   ) :: i, j,                      &
                                ids,ide, jds,jde, kds,kde, &
                                ims,ime, jms,jme, kms,kme, &
                                its,ite, jts,jte, kts,kte
      REAL,    INTENT(IN   ) :: XLV,XLS,XLV0,XLV1,XLS0,XLS1,  &
                                CP,R,G,SVP1,SVP2,SVP3,SVPT0
      REAL,    INTENT(IN   ) :: DT, dt2, DX, DXSQ, xtime, gmt
      INTEGER, INTENT(IN   ) :: KTAU
      REAL,    INTENT(IN   ) :: zpbl, ht
      REAL,    DIMENSION( ims:ime , jms:jme ),                &
               INTENT(IN   ) ::                     xlong, capesave, radsave
      REAL,    DIMENSION( ims:ime , 1:100, jms:jme ),                     &
               INTENT(IN   ) ::                     ainckfsa
      REAL,    DIMENSION( kts:kte ),                       &
               INTENT(IN   ) ::  U0, V0, T0, QR0, RH0, P0, rhoe, DZQ, W0AVG0,  &
                                 TKE0, KTH0, BBLS0, ten_radl0, ten_rads0
      REAL,    DIMENSION( kms:kme ),                       &
               INTENT(IN   ) ::  dsigma
      REAL,    DIMENSION( kts:kte+1 ),                          &
               INTENT(IN   ) ::                        z_at_w0
!     ------------ INOUT ---------------------------------------------
      REAL,    DIMENSION( kts:kte ),                       &
               INTENT(INOUT) ::  Q0, QC0_expl
      REAL,    DIMENSION( ims:ime, jms:jme ),                &
               INTENT(INOUT) ::               RAINSHV, RAINSH, pblmax, cldtopb, clddpthb
      INTEGER, DIMENSION( ims:ime , jms:jme ),                &
               INTENT(INOUT) ::               ltopb, kdcldtop, kdcldbas
      REAL,    DIMENSION( ims:ime , kms:kme , jms:jme )  , &
               INTENT(INOUT) ::               cldareab, cldliqb, wub
      REAL,    DIMENSION( kts:kte ),                           &
               INTENT(INOUT) ::  QVTEN, QCTEN, QRTEN, TTEN, DCATEN, QCDCTEN
!     ----------- OUT ------------------------------------------------
      REAL,    DIMENSION( its:ite, jts:jte ),                &
               INTENT(  OUT) ::               CAPEI, RADIUSC, AINCKFI
!
!  Local variables
!

      REAL,    DIMENSION( kts:kte ) :: DTDT, DQDT, DQRDT, DQLDT, DDCADT, DQCDCDT

      REAL    , PARAMETER ::    PIE          = 3.141592654
      REAL    , PARAMETER ::    TTFRZ        = 268.16
      REAL    , PARAMETER ::    TBFRZ        = 248.16
      REAL    , PARAMETER ::    RHBC         = 0.90
      REAL    , PARAMETER ::    P00          = 100000.0
      REAL    , PARAMETER ::    T00          = 273.16
      REAL    , PARAMETER ::    RLF          = 3.339E5
      REAL    , PARAMETER ::    AVTS         = 11.72
      REAL    , PARAMETER ::    BVTS         = 0.41
      REAL    , PARAMETER ::    N0R          = 8.E6
!     REAL    , PARAMETER ::    TO           = 263.15
      REAL    , PARAMETER ::    XN0          = 1.E-2
      REAL    , PARAMETER ::    XMMAX        = 9.4E-10 ! from MM5 param.F for autoconvertion of qc to qs
      REAL    , PARAMETER ::    N0S          = 2.E7
      REAL    , PARAMETER ::    ESI          = 0.1
      REAL    , PARAMETER ::    QCTH         = 0.5E-3   ! from MM5 param.F for autoconvertion of qc to qr
      REAL    , PARAMETER ::    QCK1         = 1.0E-3   ! from MM5 param.F for autoconvertion of qc to qr
      REAL    , PARAMETER ::    AVT          = 841.99667
      REAL    , PARAMETER ::    BVT          = 0.8
      REAL    , PARAMETER ::    RHCRIT       = 0.999
 
      REAL, DIMENSION( kts:kte )  ::          SCR3

      REAL       :: nupdraft, ZLCL,ZLFC, TRPPT, CLDTOP, CLDDPTH,   & ! nupdraft is for sure a real number
                    TTLCL, PPLCL, ainc, ainc2, ainc3, ainc4, tenv, qenv, tven, tvbar,   &
                    AINCM1, AINCM2, AINCMX, timeunv, timeloc, wbar, wtke, wpbltp, wnh

      INTEGER    :: LLC, KPBL, KKPBL,KLCL,KDCB,KDCT,LTOP, LTOP1, LTOPM1, LTOP2, &
                    kpblmax, kl, klm, kx, kxp1, kp1, LCL, LET, LFS, L, n

      REAL, DIMENSION( kts:kte )  :: z0, prc, pra, tlong,tshort,      &
                                     QU,TU,TVU,UMF,UER,TZ,QD,TV0,         &
                                     WD,TVD,DMF,DER,TG,QQG,TVG,           &
                                     EMS,EMSD,THETEU,THETED,THETEE,THTAU, &
                                     THTAD,THTA0,RLIQ,RICE,QLQOUT,QICOUT, &
                                     PPTLIQ,PPTICE,DETLQ,DETIC,UMF2,DMF2, &
                                     DETLQ2,DETIC2,UDR,UDR2,DDR,UER2,     &
                                     DER2,RATIO2,DOMGDP,EXN,              &
                                     DZA,TVQU,DP,DMS,EQFRC,WSPD,QDT,FXM,  &
                                     THTES,THTESG,THTAG,DDR2,THPA,QPA,    &
                                     THFXIN,THFXOUT,QFXIN,QFXOUT,DTFM,    &
                                     QC0,QCG,QCPA,QCFXIN,QCFXOUT,         &
                                     DCA,DCAFXIN,DCAFXOUT,DCA0,           &
                                     RGVC,FALOUTC,wu,qes 

      REAL, DIMENSION( kts:kte+1 )  ::  OMG, KV,KR, LFLUX, LFLUX1, LFLUX2

      REAL    :: zagl_bot, zagl_top,  &
                 tmix, thmix,  qmix,  zmix,   pmix, dpthmx,   &
                         qmix1, zmix1,        dpthmx1,  &
                 tmix2, thmix2, qmix2,         pmix2, emix, emix2,    &
                 c1, c2, c3, AREA, AREA1, AREA2, CLQ, CLQ1, CLQ2, &
                 emax, zval, TLCL, PLCL, TVLCL, ES, QS, TVAVG, QESE, WTW, RHOLCL, &
                 sum, val, val0, val1, val2, ff, h, b, rad, &
                 PPBL, TENVPBL, QENVPBL, TVENPBL, TPBL, TVPBL, RHOPBL, ROCPQ, &
                 RHO_INI, T_INI, TV_INI, TVEN_INI, Z_INI, P_INI, &
                 DPTH, PPLSDP, AU0, VMFPBLCL, VMFLCL, UPOLD, UPNEW, &
                 abe, THTUDL, TUDL, TTEMP, RATE, FRC, FRC1, QNEWLQ, QNEWIC, RL, &
                 QNWFRZ, EFFQ, BE, BOTERM, ENTERM, DZZ, WSQ, &
                 WABS, UDLBE, ee, ee1, ee2, ud1, ud2, rei, ttmp, f1, f2, THTTMP, &
                 RTMP, TMPLIQ, TMPICE, TU95, TU10, EQMAX, EQMIN, CTP1, CTP2, WUMAX, &
                 DPTT, DUMFDP, TSAT, THTA, P165, PPTMLT, PPTML2, CLVF, USR, VCONV, &
                 TIMEC, TADVEC, TCMIN, TCMAX, SHSIGN, VWS, PEF, PEFMX, PEFMN,  &
                 CBH, RCBH, PEFCBH, CBHMX, PEFF, TDER, TDER2, THTMIN, DTMLTD, &
                 DPT, DPDD, a1, rdd, DQSSDT, DTMP, QSRH, PPTFLX, PPTFL2, CPR, CNDTNF, &
                 UPDINC, DMFMIN, DEVDMF, PPR, RCED, DPPTDF, DMFLFS, &
                 DDINC, FABE, STAB, TKEMAX, DSOURCE, TIMECS, &
                 DTT, DTT1, GD, CPM, GM, DTEMPDT, DTHETADT, & 
                 TMA, TMB, TMM, BCOEFF, ACOEFF, TOPOMG, DTMLTE, TDP, &
                 DQ, QMIN, TLOG, TDPT, CPORQ, DLP, ZLCL1, ABEG, THATA, &
                 THTFC, DABE, AINCMIN, RF, UPDIN2, &
                 DR, UDFRC, UEFRC, DDFRC, DEFRC, TUC, TDC, QGS, RH_0, RHG, &
                 QINIT, QFNL, QCFINL, ERR2, RELERR, &
                 DIFFK, RLC, E2, EOVLC, RCP, RLOVCP, &
                 RHCR, AREADIFF, FX, FX1, FX2,  &
                 EFOLD, EX, DEPTHMAX, HIN, HOUT, DELTH, QIN, QOUT, &
                 DELTQ, RKAPA, DELTA, RATIO, RATIOMIN, RATIOMAX,  &
                 GAMA, BEITA, SIGMMA, WFUN, BVT3, BVTS3, PPI, G3PB, G3PBS, PRAC, PRACS, &
                 TOUT, SC2, SC3, SC7, RHOS, PPIS, XNC, &
                 RHO2, VT2C, FALTNDC, gdry, p300, factor1,  &
                 z1, p1, t1, w1, z2, p2, t2, w2, r1, &
                 t1rh, dtime, ROVCP

      INTEGER :: ii, k, ks, k1, k2, kk, k0, ndk, nj, nm, nk, nk1,   &
                 kval, kval1, kval2, iflag, kflag, nic, &
                 KMIN, KSTART, ND, NT, ND1, LDB, LDT, LMAX, NCOUNT, ISTOP, &
                 k100, k1000, KAMAX, KLCMAX, dbg_level, i0, j0, KRADLMAX, &
                 NTC, NSTEP, ML, L5, LLFC, LM, LM1, LM2, LC, lvf, kradsmax

      REAL    :: ALIQ, BLIQ, CLIQ, DLIQ

      LOGICAL, EXTERNAL  :: wrf_dm_on_monitor

!     REAL :: tpdd    ! Function declaration
!     REAL :: GAMMA   ! Function declaration

      CHARACTER*1024 message

#ifdef DENG_SHCU1D
      REAL, DIMENSION( its:ite , jts:jte ) ::                        &
                     TPBLG,PPBLG,TLCLG,PLCLG, &             ! used to be 1-d mm5 graphics
                     CLDHGTG,TCLDTPG,PCLDTPG,TLFSG,PLFSG,TLDTG,PLDTG, &
                     TLDBG,PLDBG,TLLCG,PLLCG

      REAL, DIMENSION( its:ite , jts:jte ) ::                        &
                     CLDBMFLX,   &
                     nca_ctr1,nca_ctr2,nca_ctr3,nca_ctr4,  &
                     dcldtop, dcldbase

      REAL, DIMENSION( kts:kte ) :: tt4, tt6, tt7, wsubsub, wutemp, cldaten0, cldaten,   &
                                    cldlctn0, cldlctn1, cldlctn2, cldlctn3, cldlctn4,    &
                                    cldlctn5, thtetmp, thtatmp, cldua, tug, wsp_plot,    &
                                    wdr_plot, td_plot, ca_rad0

      REAL    :: PLET,TLET, aa=60.0*60.0,  bb=60.0*60.0*1000.0, qqq, ees,  &
                 PLOTFRQ = 30.0

      INTEGER :: KTAU1HUR, KCBASE
#endif
!******************************************************************************
!
      CALL get_wrf_debug_level( dbg_level )

      ISTOP= 0
      NT   = 0
      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3

      GDRY = -G/CP
      ROVCP=R/CP

      KL=kte
      klm = kl - 1
      KX=kte
      kxp1 = kx + 1

      i0 = (ite-its)/2+its
      j0 = (jte-jts)/2+jts

#ifdef DENG_SHCU1D
      kcbase=1
      KTAU1HUR = 3600/INT(DT)
      IF( i == 1 .AND. j == 1 ) write(31,*) xtime/60.,zpbl
      do k=1,kl
        tt4(k)=0.0
        tt6(k)=0.0
        tt7(k)=0.0
        wutemp(k)=0.0
        wsubsub(k)=0.0
        cldaten0(k)=0.0
        cldaten(k)=0.0
        cldlctn0(k)=0.0
        cldlctn1(k)=0.0
        cldlctn2(k)=0.0
        cldlctn3(k)=0.0
        cldlctn4(k)=0.0
        cldlctn5(k)=0.0
        thtetmp(k)=0.0
        thtatmp(k)=0.0
      enddo
#endif

      LTOP=1
      KPBLMAX = 1
      CLDDPTH=0.0
      CLDTOP=0.0
      DO K=1,KL
#ifdef DENG_SHCU1D
        CLDUA(K)=0.0
        TUG(K)=0.0
#endif
        WU(K)=0.0
        OMG(K)=0.0
        UMF(K)=0.0
        THTAD(K)=0.0
        WD(K)=0.0
        KV(K)=0.0
        KR(K)=0.0
      ENDDO
      RAINSHV(I,J)=0.0
      AINCKFI(I,J)=0.0
      CAPEI(I,J)=0.0
#ifdef DENG_SHCU1D
      DCLDTOP(I,J)=0.0
      DCLDBASE(I,J)=0.0
      CLDBMFLX(I,J)=0.0
#endif
!
!...INPUT A VERTICAL SOUNDING ... NOTE THAT MODEL LAYERS ARE NUMBERED FROM THE
!...BOTTOM-UP IN THE SHALLOW CONVECTION SCHEME...
!

    P300=P0(1)-30000.
    ML = 0

    DO K=1,KX
      QC0(K)=0.0
      DCA0(K)=0.0
!
!...IF Q0 IS ABOVE SATURATION VALUE, REDUCE IT TO SATURATION LEVEL...
!
!     ES=ALIQ*EXP((BLIQ*T0(K)-CLIQ)/(T0(K)-DLIQ))
      IF( T0(K) .GT. TO ) THEN
        ES=1.E3*SVP1*EXP(SVP2*(T0(K)-SVPT0)/(T0(K)-SVP3))
      ELSE
        ES=611.0*EXP(22.514-6.15E3/T0(K))
      ENDIF
      QES(K)=0.622*ES/(P0(K)-ES)
      Q0(K)=AMIN1(QES(K),Q0(K))
      IF(Q0(K).GT.QES(K))Q0(K)=QES(K)

      TV0(K)=T0(K)*(1.+.608*Q0(K))
!     RHOE(K)=P0(K)/(R*TV0(K))
!
!...DZQ IS DZ BETWEEN SIGMA SURFACES, DZA IS DZ BETWEEN MODEL HALF LEVELS,
!   DP IS THE PRESSURE INTERVAL BETWEEN FULL SIGMA LEVELS...
!
      DP(K)=rhoe(k)*g*DZQ(k)
      IF(P0(K).GE.500E2) L5=K
      IF(P0(K).GE.P300) LLFC=K
      IF(T0(K).GT.T00) ML=K
    ENDDO

    Z0(1)=.5*DZQ(1)
    DO K=2,KL
      Z0(K) = Z0(K-1)+.5*(DZQ(K)+DZQ(K-1))
      DZA(K-1) = Z0(K)-Z0(K-1)
    ENDDO
    DZA(KL) = 0.

      LC = 1
      KFLAG=0
      AINC=999.0
      CTP2=99999.9
      LTOP2=99999
!     ZPBL = PBL(I,J)
!
!...FIND CONVECTIVE SOURCE LAYER WHERE THE INITIAL PARCEL CHARACTERISTICS 
!  ARE DEFINED.  IT IS THE MAXIMUM OF 20% OF PBL AND 2 (IF PBL IS AT LEAST
!  4 LATERS DEEP.  DEFINE THE PARCEL PURTERBATION PROPERTITES FROM THE
!  SOURCE LAYER (THMIX,QMIX,ZMIX,PMIX)
!
    LM1 = 1
    FACTOR1 = 0.20               ! 20% of PBL
    source: DO NK = 1,KL
      IF( (Z0(NK)+0.5*DZQ(NK)) .LT. FACTOR1*ZPBL ) THEN
        LM1=LM1+1
      ELSE
        EXIT source
      ENDIF
    ENDDO source

    kpbl = 1

    loop_ku: DO k = 1, kl
      zagl_bot =  z_at_w0(k)-ht
      zagl_top =  z_at_w0(k+1)-ht
      IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
        kpbl = k
        EXIT loop_ku
      ENDIF
    ENDDO loop_ku

    LM2=1
    IF( KPBL .GE. 4 ) LM2=2
    LM=MAX(LM1,LM2)

    THMIX=0.
    QMIX=0.
    ZMIX=0.
    PMIX=0.
    DPTHMX=0.
    DO NK = 1, LM
      DPTHMX=DPTHMX+DP(NK)
      ROCPQ=0.2854*(1.-0.28*Q0(NK))
      THMIX=THMIX+DP(NK)*T0(NK)*(P00/P0(NK))**ROCPQ
      QMIX=QMIX+DP(NK)*Q0(NK)
      ZMIX=ZMIX+DP(NK)*Z0(NK)
      PMIX=PMIX+DP(NK)*P0(NK)
    ENDDO
    THMIX=THMIX/DPTHMX
    QMIX=QMIX/DPTHMX
    ZMIX=ZMIX/DPTHMX
    PMIX=PMIX/DPTHMX
!
!   FIND THE LCL (TLCL,TVLCL,PLCL,KLCL)
!
      ROCPQ=0.2854*(1.-0.28*QMIX)
      TMIX=THMIX*(PMIX/P00)**ROCPQ
      EMIX=QMIX*PMIX/(0.622+QMIX)
!     TLOG=ALOG(EMIX/ALIQ)
!     TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
      TLOG=ALOG(EMIX/SVP1/1000.)
      TDPT=(SVP2*SVPT0-SVP3*TLOG)/(SVP2-TLOG)
      TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*  &
              (TMIX-TDPT)
      TLCL=AMIN1(TLCL,TMIX)
      TVLCL=TLCL*(1.+0.608*QMIX)
      CPORQ=1./ROCPQ
      PLCL=P00*(TLCL/THMIX)**CPORQ

    DO NK = 1,KL
      KLCL=NK
      IF( PLCL >= P0(NK) ) GO TO 35
    ENDDO
    GOTO 425
 35 CONTINUE
    K=KLCL-1

    IF(KLCL .EQ. 1 ) THEN
#ifdef DENG_SHCU1D
        write(*,*) 'KL layer is saturated and skip over active con.'
        PLET = PLCL
        TLET = TLCL
        PLFSG(I,J) = PLCL
        TLFSG(I,J) = TLCL
        PLDTG(I,J) = PLCL
        TLDTG(I,J) = TLCL
        PLDBG(I,J) = PLCL
        TLDBG(I,J) = TLCL
        PLLCG(I,J) = PLCL
        TLLCG(I,J) = TLCL
        CLDHGTG(I,J)=0.0
        PCLDTPG(I,J)=PLCL
        TCLDTPG(I,J)=TLCL
#endif
 
        IF(ZPBL .LE. Z0(1)) THEN
          PPBL=P0(1)
        ELSE
          loop12: DO KK=1, KL-1
            IF(ZPBL .GE. Z0(KK) .AND. ZPBL .LT. Z0(KK+1)) THEN
              K0=KK
              EXIT loop12
            END IF
          ENDDO loop12

          C1=(ZPBL-Z0(K0))/(Z0(K0+1)-Z0(K0))
          PPBL=P0(K0)*EXP(C1*(ALOG(P0(K0+1))-ALOG(P0(K0))))
        END IF
        TPBL=TLCL*(PPBL/PLCL)**ROCPQ
 
        ZLCL=Z0(KLCL)
        CLDTOP=ZLCL
        ZLFC=Z0(KL)
#ifdef DENG_SHCU1D
      nca_ctr1(i,j)=nca_ctr1(i,j)+1
#endif
        GOTO 425
 
!     KLCL=KLCL+1
!     K=KLCL-1
    ENDIF

    DLP=LOG(PLCL/P0(K))/LOG(P0(KLCL)/P0(K))
!
!...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...
!
      TENV=T0(K)+(T0(KLCL)-T0(K))*DLP
      QENV=Q0(K)+(Q0(KLCL)-Q0(K))*DLP
      TVEN=TENV*(1.+0.608*QENV)
      TVBAR=0.5*(TV0(K)+TVEN)
      ZLCL=Z0(K)+(Z0(KLCL)-Z0(K))*DLP
!
! DETERMINE PURTERBATION VERTICAL VELOCITIES FOR INITIATING PARCEL,
! WPBLTP=WBAR+WTKE+WNH, WHERE WBAR IS THE RESOLVABLE-SCALE MEAN VERTICAL
! MOTION, WTKE IS THE CONTRIBUTION FROM THE PBL TKE AND WNH IS THE NON-HYDROSTAIC
! PUMPING EFFECT
!
      IF( zpbl .GT. PBLMAX(I,J) ) THEN
        PBLMAX(I,J) = zpbl
        KPBLMAX = KPBL
      ENDIF
!
!  CALCULATE LOCAL TIME USING UTC (DATE) AND THE LONGITUDE.
!
      TIMEUNV = gmt  + XTIME/60.
      IF( TIMEUNV .GE. 24.0 ) TIMEUNV = TIMEUNV - 24.0
      TIMELOC = TIMEUNV + XLONG(I,J)/15.0
      IF( TIMELOC .LT. 0.0 ) TIMELOC = TIMELOC + 24.0
      IF( TIMELOC .GE.24.0 ) TIMELOC = TIMELOC - 24.0

      IF( ABS(TIMELOC-6.0) .LT. 0.01 ) THEN
        PBLMAX(I,J) = 0.0             ! SET PBLMAX=0 IF LOCAL TIME=6AM
        KPBLMAX = 1
      END IF
      KVAL=MIN(KLCL,KPBLMAX)

      EMAX = 0.0
      loop154: DO nk = 1, KVAL
        val = TKE0(nK)
        IF( val.LT.EMAX ) CYCLE loop154
        EMAX = val
      ENDDO loop154

      KVAL=MIN(KLCL-1,KPBL)

      WBAR=W0AVG0(KVAL)
      WTKE=SQRT(2.0*EMAX/3.0)

      WPBLTP=WBAR+WTKE

      WNH=0.0
      IF( CLDDPTHB(I,J) .GT. 4.0E3 ) THEN
        ZVAL = ZLCL + 2000.0
        DO NK = 1,KL
          IF( ABS(Z0(NK)-ZVAL) .LT. DZQ(NK)/2.0 ) KVAL = NK
        ENDDO
        IF( WUB(I,KVAL,J) .GT. WPBLTP ) THEN
          WNH=0.25*(WUB(I,KVAL,J)-WPBLTP)
        ELSE
          WNH=0.0
        ENDIF
      ENDIF

      WPBLTP=WPBLTP+WNH
#ifdef DENG_SHCU1D
      IF( i == 1 .AND. j == 1 ) write(45,*) xtime/60.,WBAR,WTKE,WNH,WPBLTP
#endif
!
!...THE PARCEL IS BUOYANT; COMPUTE EQUIVALENT POTENTIAL TEMPERATURE (THETEU)
!...AND VERTICAL VELOCITY OF THE RISING PARCEL AT THE LCL...USE BOLTONS (1980)
!...FORMULA FOR THETE...
!
      THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*  &
            EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))
      ES=1.E3*SVP1*EXP(SVP2*(TENV-SVPT0)/(TENV-SVP3))
      TVAVG=0.5*(TV0(KLCL)+TENV*(1.+0.608*QENV))
      QESE=0.622*ES/(PLCL-ES)
      THTES(K)=TENV*(1.E5/PLCL)**(0.2854*(1.-0.28*QESE))*  &
            EXP((3374.6525/TENV-2.5403)*QESE*(1.+0.81*QESE))
      WTW=WPBLTP*WPBLTP
      TVLCL=TLCL*(1.+0.608*QMIX)
      RHOLCL=PLCL/(R*TVLCL)
      LCL=KLCL
      LET=LCL
!
!  DEFINE THE UPDRAFT RADIUS, RAD, AS A FUNCTION OF DEPTHS OF THE PBL
!  AND UPDRAFT AT THE PREVIOUS TIME STEP.  RAD IS IN THE RANGE OF 150 
!  AND 1500 M
!
      VAL=CLDDPTHB(I,J)
      VAL1=ZPBL
      IF(VAL/1000.0 .GT. 3.9) THEN
        RAD=1500.0
        GO TO 1001
      END IF
      FF=12.0*VAL/(4.0-VAL/1000.)/1000.
      H=VAL1*FF
      B=-0.5*(7.0+2.0*H/1000.)
      RAD=0.5*(-B-SQRT(B*B-12.0*H/1000.0))
      RAD=RAD*0.5*1000.
      IF(RAD .LT. 150.0) RAD=150.0
      IF(RAD .GT.1500.0) RAD=1500.0
 1001 CONTINUE
!     write(*,'(i7, 2i3, 4f10.3)') ktau, i, j, xtime/60.0, rad, zpbl, CLDDPTHB(I,J)

!     Averaging the radius between current and the previous time value
!
      IF(CAPESAVE(I,J) .LT. 100.0) RAD=(RAD+RADSAVE(I,J))/2.0

      RADIUSC(i,j)=RAD
!
!  FIND PBL PROPERTIES
!
      IF( ZPBL .LE. Z0(1) ) THEN
        K = 1
        PPBL = P0(K)
        TENVPBL = T0(K)
        QENVPBL = Q0(K)
        TVENPBL = TV0(K)
      ELSE
        loop13: DO KK = 1, KL-1
          IF( ZPBL .GE. Z0(KK) .AND. ZPBL .LT. Z0(KK+1) ) THEN
            K = KK
            EXIT loop13
          END IF 
        ENDDO loop13

        C1 = ( ZPBL-Z0(K) )/( Z0(K+1)-Z0(K) )
        PPBL = P0(K)*EXP( C1 * ( ALOG(P0(K+1))-ALOG(P0(K)) ) )
        TENVPBL = C1* (T0(K+1)-T0(K)) + T0(K)
        QENVPBL = C1* (Q0(K+1)-Q0(K)) + Q0(K)
        TVENPBL = C1* (TV0(K+1)-TV0(K)) + TV0(K)
      END IF
      KKPBL = K
      TPBL=TLCL*(PPBL/PLCL)**ROCPQ
      TVPBL = TPBL*(1.+0.608*QMIX)
      RHOPBL=PPBL/(R*TVPBL)
!
!    THE LOWER OF LCL AND PBL IS DETERMINED AS THE PARCEL RELEASING POINT
!    WHERE K,RHO_INI, T_INI, TV_INI, TVEN_IN, Z_INI AND P_INI ARE DEFINED
!
      IF( PPBL .GE. PLCL ) THEN
        THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*  &
            EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))
        ES=1.E3*SVP1*EXP(SVP2*(TENVPBL-SVPT0)/(TENVPBL-SVP3))
        QESE=0.622*ES/(PPBL-ES)
        THTES(K)=TENVPBL*(1.E5/PPBL)**(0.2854*(1.-0.28*QESE))*  &
            EXP((3374.6525/TENVPBL-2.5403)*QESE*(1.+0.81*QESE))
        RHO_INI = RHOPBL
        T_INI = TPBL
        TV_INI = TVPBL
        TVEN_INI = TVENPBL
        Z_INI = ZPBL
        P_INI = PPBL
        LET = KKPBL
      ELSE
        K = KLCL-1 
        RHO_INI = RHOLCL
        T_INI = TLCL
        TV_INI = TVLCL
        TVEN_INI= TVEN
        Z_INI = ZLCL
        P_INI = PLCL
      END IF
!
! DETERMIN THE DEPTH OF SOURCE LAYER WHERE THE MASS IS IS REMOVED AS A RESULTS OF
! CONVECTION.  THE DEPTH IS DEFINED AS A FUNCTION OF CLOUD RADIUS AND IS IN THE
! RANGE OF 10 MB TO 60 MB
!
      DPTHMX1 = 0.0
      DO NK = 1, K
        DPTHMX1 = DPTHMX1 + DP(NK)
      ENDDO
      IF( K .EQ. 1 ) THEN
        LLC=1
        GO TO 897
      ENDIF
      DPTH = 100.0*(RAD-150.0)/27.0 + 1000.0
      IF( DPTH .GE. DPTHMX1 ) THEN
        LLC = 1
        GO TO 897
      END IF
      PPLSDP = P0(K+1) + DP(K+1)/2.0 + DPTH
      LLC = K
      DO NK = K-1, 1, -1
       IF( ABS(PPLSDP-P0(NK)) .LE. ABS(PPLSDP-P0(NK+1)) ) LLC = NK
      ENDDO

 897  CONTINUE
      
#ifdef DENG_SHCU1D
      KCBASE=KLCL
      IF( i == 1 .AND. j == 1 ) write(300,*) xtime/60.,K, RHO_INI, T_INI, TV_INI, TVEN_INI, Z_INI, P_INI
#endif
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!     BEGINING OF UPDRAFT CACULATION
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!   COMPUTE INITIAL UPDRAFT MASS FLUX UMF(K)
!
      WU(K)=WPBLTP
      AU0=PIE*RAD*RAD
      UMF(K)=RHO_INI*AU0*WPBLTP
      VMFPBLCL=UMF(K)
      VMFLCL=UMF(K)
      UPOLD=VMFPBLCL
      UPNEW=UPOLD
      RATIO2(K)=0.
!
!...UER IS THE ENVIR ENTRAINMENT RATE, ABE IS AVAILABLE BUOYANT ENERGY,
!   TRPPT IS THE TOTAL RATE OF PRECIPITATION PRODUCTION...
!
      UER(K)=0.
      ABE=0.
      TRPPT=0.
      TU(K)=T_INI
      TVU(K)=TV_INI
      QU(K)=QMIX
      EQFRC(K)=1.
      RLIQ(K)=0.
      RICE(K)=0.
      QLQOUT(K)=0.
      QICOUT(K)=0.
      DETLQ(K)=0.
      DETIC(K)=0.
      PPTLIQ(K)=0.
      PPTICE(K)=0.
      IFLAG = 0
!     KFRZ=LC
!
!...THE AMOUNT OF CONV AVAIL POT ENERGY (CAPE) IS CALCULATED WITH RESPECT TO
!...UNDILUTE PARCEL ASCENT; EQ POT TEMP OF UNDILT PARCEL IS THTUDL, UNDILT
!   TEMP IS GIVEN BY TUDL...
!
      THTUDL=THETEU(K)
      TUDL=T_INI
!
!...TTEMP IS USED DURING CALCULATION OF THE LINEAR GLACIATION PROCESS; IT IS
!   INITIALLY SET TO THE TEMPERATURE AT WHICH FREEZING IS SPECIFIED TO BEGIN;
!   WITHIN THE GLACIATION INTERVAL, IT IS SET EQUAL TO THE UPDR TEMP AT THE
!   PREVIOUS MODEL LEVEL...
!
      TTEMP=TTFRZ
!
!   FOR SHALLOW CONVECTION, SET RATE=0 SO NO LIQUID FALLS OUT
!
      IF( CLDDPTHB(I,J) .LT. 4.E3) THEN
         RATE = 0.0              !  TURN OFF DOWNDRAFT ALSO
      ELSE
         RATE = 0.01
      ENDIF
!
!...ENTER THE LOOP FOR UPDRAFT CALCULATIONS...CALCULATE UPDRAFT TEMP, MIXING
!   RATIO, VERTICAL MASS FLUX, LATERAL DETRAINMENT OF MASS AND MOISTURE, AND
!   PRECIPITATION RATES AT EACH MODEL LEVEL...
!
     EE1=1.
     UD1=0.
     REI = 0.
     loop60: DO NK=K,KLM
         NK1=NK+1
         RATIO2(NK1)=RATIO2(NK)
!
!...UPDATE UPDRAFT PROPERTIES AT THE NEXT MODEL LVL TO REFLECT ENTRAINMNT OF
!   ENVIRONMENTAL AIR...
!
         FRC1=0.
         TU(NK1)=T0(NK1)
         THETEU(NK1)=THETEU(NK)
         QU(NK1)=QU(NK)
         RLIQ(NK1)=RLIQ(NK)
         RICE(NK1)=RICE(NK)
         CALL TPMIX(P0(NK1),THETEU(NK1),TU(NK1),QU(NK1),RLIQ(NK1), &
             RICE(NK1),QNEWLQ,QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1, &
             SVP1,SVP2,SVPT0,SVP3)
!        CALL TPMIXBG(P0(NK1),THETEU(NK1),TU(NK1),QU(NK1),RLIQ(NK1), &
!            RICE(NK1),QNEWLQ,QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1, &
!            SVP1,SVP2,SVPT0,SVP3)

!        CALL TPMIX2(P0(NK1),THETEU(NK1),TU(NK1),QU(NK1),RLIQ(NK1), &
!            RICE(NK1),QNEWLQ,QNEWIC,XLV1,XLV0,ktau,i,j,nk1, 1)
         TVU(NK1)=TU(NK1)*(1.+0.608*QU(NK1))
!
!  CHECK TO SEE IF UPDRAFT TEMP IS WITHIN THE FREEZING INTERVAL, IF IT IS,
!  CALCULATE THE FRACTIONAL CONVERSION TO GLACIATION AND ADJUST QNEWLQ TO
!  REFLECT THE GRADUAL CHANGE IN THETAU SINCE THE LAST MODEL LEVEL...THE
!  GLACIATION EFFECTS WILL BE DETERMINED AFTER THE AMOUNT OF CONDENSATE
!  AVAILABLE AFTER PRECIP FALLOUT IS DETERMINED...TTFRZ IS THE TEMP AT WHICH
!  GLACIATION BEGINS, TBFRZ THE TEMP AT WHICH IT ENDS...
!
         IF(TU(NK1).LE.TTFRZ.AND.IFLAG.LT.1)THEN
           IF(TU(NK1).GT.TBFRZ)THEN
             IF(TTEMP.GT.TTFRZ)TTEMP=TTFRZ
             FRC1=(TTEMP-TU(NK1))/(TTFRZ-TBFRZ)
             R1=(TTEMP-TU(NK1))/(TTEMP-TBFRZ)
           ELSE
             FRC1=(TTEMP-TBFRZ)/(TTFRZ-TBFRZ)
             R1=1.
             IFLAG=1
           ENDIF
           QNWFRZ=QNEWLQ
           QNEWIC=QNEWIC+QNEWLQ*R1*0.5
           QNEWLQ=QNEWLQ-QNEWLQ*R1*0.5
           IF( ABS(TTEMP-TBFRZ) < 1.E-3 ) THEN
             EFFQ= 1.0
           ELSE
             EFFQ=(TTFRZ-TBFRZ)/(TTEMP-TBFRZ)
           ENDIF
           TTEMP=TU(NK1)
         ENDIF
!
!  CALCULATE UPDRAFT VERTICAL VELOCITY AND PRECIPITATION FALLOUT...
!
         IF(NK.EQ.K)THEN
           BE=(TV_INI+TVU(NK1))/(TVEN_INI+TV0(NK1))-1.
           BOTERM=2.*(Z0(NK1)-Z_INI)*G*BE/1.5
           ENTERM=0.
           DZZ=Z0(NK1)-Z_INI
         ELSE
           BE=(TVU(NK)+TVU(NK1))/(TV0(NK)+TV0(NK1))-1.
           BOTERM=2.*DZA(NK)*G*BE/1.5
           ENTERM=2.*UER(NK)*WTW/UPOLD
           DZZ=DZA(NK)
         ENDIF
         WSQ=WTW
         CALL CONDLOAD(RLIQ(NK1),RICE(NK1),WTW,DZZ,BOTERM,ENTERM, &
                       RATE,QNEWLQ,QNEWIC,QLQOUT(NK1),QICOUT(NK1), G)
         IF(WTW < 1.E-3) THEN
           EXIT loop60
         ELSE
           WU(NK1)=SQRT(WTW)
         ENDIF
!
!...IF VERT VELOCITY IS LESS THAN ZERO, EXIT THE UPDRAFT LOOP AND, IF CLOUD
!   IS TALL ENOUGH, FINALIZE UPDRAFT CALCULATIONS...
!
!          IF(WU(NK1).LT.0.) EXIT loop60
!
!  UPDATE THE ABE FOR UNDILUTE ASCENT...
!
      THTES(NK1)=T0(NK1)*(1.E5/P0(NK1))**(0.2854*(1.-0.28*QES(NK1)))* &
           EXP((3374.6525/T0(NK1)-2.5403)*QES(NK1)*(1.+0.81*QES(NK1)))
      UDLBE=((2.*THTUDL)/(THTES(NK)+THTES(NK1))-1.)*DZZ
         IF(UDLBE.GT.0.)ABE=ABE+UDLBE*G
!
!  DETERMINE THE EFFECTS OF CLOUD GLACIATION IF WITHIN THE SPECIFIED
!  TEMP INTERVAL...
!
         IF(FRC1.GT.1.E-6)THEN
           CALL DTFRZNEW(TU(NK1),P0(NK1),THETEU(NK1),QU(NK1),RLIQ(NK1), &
                RICE(NK1),RATIO2(NK1),QNWFRZ,RL,FRC1,EFFQ, &
                IFLAG,XLV0,XLV1,XLS0,XLS1, &
            SVP1,SVP2,SVPT0,SVP3)
         ENDIF
!
!  CALL SUBROUTINE TO CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMP...
!  WITHIN GLACIATION INTERVAL, THETAE MUST BE CALCULATED WITH RESPECT TO THE
!  SAME DEGREE OF GLACIATION FOR ALL ENTRAINING AIR...
!
         CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),   &
               THETEE(NK1),RATIO2(NK1),RL,        &
          SVP1,SVP2,SVPT0,SVP3,I,J)
!        CALL ENVIRTHT2(P0(NK1),T0(NK1),Q0(NK1),   &
!              THETEE(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
!
!...REI IS THE RATE OF ENVIRONMENTAL INFLOW...
!
         REI=VMFPBLCL*DP(NK1)*0.03/RAD
!
!...TVQU IS THE PARCEL TEMP INCLUDING WATER LOADING...
!...IF IT BECOMES GREATER THAN TV0, THEN YOU ARE PAST LFC...
!...
         TVQU(NK1)=TU(NK1)*(1.+0.608*QU(NK1)-RLIQ(NK1)-RICE(NK1))
!
!...IF CLOUD PARCELS ARE VIRTUALLY COLDER THAN THE ENVIRONMENT, NO
!   ENTRAINMENT IS ALLOWED AT THIS LEVEL...
!
         IF(TVQU(NK1).LE.TV0(NK1))THEN
           UER(NK1)=0.0
           UDR(NK1)=REI
           EE2=0.
           UD2=1.
           EQFRC(NK1)=0.
           GOTO 55
         ENDIF
         LET=NK1
         TTMP=TVQU(NK1)
!
!...DETERMINE THE CRITICAL MIXED FRACTION OF UPDRAFT AND ENVIRONMENTAL AIR...
!
        F1=0.95
        F2=1.-F1
        THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)
        RTMP=F1*Q0(NK1)+F2*QU(NK1)
        TMPLIQ=F2*RLIQ(NK1)
        TMPICE=F2*RICE(NK1)
        CALL TPMIX(P0(NK1),THTTMP,TTMP,RTMP,TMPLIQ,TMPICE,QNEWLQ, &
                      QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,  &
          SVP1,SVP2,SVPT0,SVP3)
!       CALL TPMIX2(P0(NK1),THTTMP,TTMP,RTMP,TMPLIQ,TMPICE,QNEWLQ, &
!                     QNEWIC,XLV1,XLV0,ktau,i,j,nk1,2)
!       CALL TPMIXBG(P0(NK1),THTTMP,TTMP,RTMP,TMPLIQ,TMPICE,QNEWLQ, &
!                     QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,  &
!         SVP1,SVP2,SVPT0,SVP3)
        TU95=TTMP*(1.+0.608*RTMP-TMPLIQ-TMPICE)
        IF(TU95.GT.TV0(NK1))THEN
          EE2=1.
          UD2=0.
          EQFRC(NK1)=1.0
          GOTO 50
        ENDIF
        F1=0.10
        F2=1.-F1
        THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)
        RTMP=F1*Q0(NK1)+F2*QU(NK1)
        TMPLIQ=F2*RLIQ(NK1)
        TMPICE=F2*RICE(NK1)
        CALL TPMIX(P0(NK1),THTTMP,TTMP,RTMP,TMPLIQ,TMPICE,QNEWLQ, &
                  QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,  &
                  SVP1,SVP2,SVPT0,SVP3)
!       CALL TPMIX2(P0(NK1),THTTMP,TTMP,RTMP,TMPLIQ,TMPICE,QNEWLQ, &
!                 QNEWIC,XLV1,XLV0,ktau,i,j,nk1,3)
!       CALL TPMIXBG(P0(NK1),THTTMP,TTMP,RTMP,TMPLIQ,TMPICE,QNEWLQ, &
!                 QNEWIC,RATIO2(NK1),RL,XLV0,XLV1,XLS0,XLS1,  &
!                 SVP1,SVP2,SVPT0,SVP3)
        TU10=TTMP*(1.+0.608*RTMP-TMPLIQ-TMPICE)
        IF( ABS(TU10-TVQU(NK1)) < 1.E-3) THEN
          EQFRC(NK1)= 1.0
        ELSE
          EQFRC(NK1)=(TV0(NK1)-TVQU(NK1))*F1/(TU10-TVQU(NK1)+1.E-20)
        ENDIF
        EQMAX = 1.
        EQMIN = 0.
        EQFRC(NK1)=MAX(EQMIN,EQFRC(NK1))
        EQFRC(NK1)=MIN(EQMAX,EQFRC(NK1))
        IF(EQFRC(NK1).EQ.1)THEN
          EE2=1.
          UD2=0.
          GOTO 50
        ELSEIF(EQFRC(NK1).EQ.0.)THEN
          EE2=0.
          UD2=1.
          GOTO 50
        ELSE
!
!...SUBROUTINE PROF5 INTEGRATES OVER THE GAUSSIAN DIST TO DETERMINE THE
!   FRACTIONAL ENTRAINMENT AND DETRAINMENT RATES...
!
          CALL PROF5(EQFRC(NK1),EE2,UD2)
        ENDIF

  50    CONTINUE

!       IF( NK .EQ. K) THEN
!          EE1=1.
!          UD1=0.
!        ENDIF
!
!...NET ENTRAINMENT AND DETRAINMENT RATES ARE GIVEN BY THE AVERAGE FRACTIONAL
!   VALUES IN THE LAYER...
!
         UER(NK1)=0.5*REI*(EE1+EE2)
         UDR(NK1)=0.5*REI*(UD1+UD2)
!
!...IF THE CALCULATED UPDRAFT DETRAINMENT RATE IS GREATER THAN THE TOTAL
!   UPDRAFT MASS FLUX, ALL CLOUD MASS DETRAINS, EXIT UPDRAFT CALCULATIONS...
!
  55    CONTINUE

        IF(UMF(NK)-UDR(NK1).LT.10.)THEN
!
!...IF THE CALCULATED DETRAINED MASS FLUX IS GREATER THAN THE TOTAL UPD MASS
!   FLUX, IMPOSE TOTAL DETRAINMENT OF UPDRAFT MASS AT THE PREVIOUS MODEL LVL..
!
          IF(UDLBE.GT.0.)ABE=ABE-UDLBE*G
          LET=NK
          EXIT loop60
        ENDIF

         EE1=EE2
         UD1=UD2
         UPOLD=UMF(NK)-UDR(NK1)
         UPNEW=UPOLD+UER(NK1)
         UMF(NK1)=UPNEW
!
!...DETLQ AND DETIC ARE THE RATES OF DETRAINMENT OF LIQUID AND ICE IN THE
!   DETRAINING UPDRAFT MASS...
!
         DETLQ(NK1)=RLIQ(NK1)*UDR(NK1)
         DETIC(NK1)=RICE(NK1)*UDR(NK1)
         QDT(NK1)=QU(NK1)
         QU(NK1)=(UPOLD*QU(NK1)+UER(NK1)*Q0(NK1))/UPNEW
         THETEU(NK1)=(THETEU(NK1)*UPOLD+THETEE(NK1)*UER(NK1))/UPNEW
         RLIQ(NK1)=RLIQ(NK1)*UPOLD/UPNEW
         RICE(NK1)=RICE(NK1)*UPOLD/UPNEW
!
!...KFRZ IS THE HIGHEST MODEL LEVEL AT WHICH LIQUID CONDENSATE IS GENERATED...
!   PPTLIQ IS THE RATE OF GENERATION (FALLOUT) OF LIQUID PRECIP AT A GIVEN
!   MODEL LVL, PPTICE THE SAME FOR ICE, TRPPT IS THE TOTAL RATE OF PRODUCTION
!   OF PRECIP UP TO THE CURRENT MODEL LEVEL...
!
!        IF(ABS(RATIO2(NK1)-1.).GT.1.E-6)KFRZ=NK1
         PPTLIQ(NK1)=QLQOUT(NK1)*UMF(NK)
         PPTICE(NK1)=QICOUT(NK1)*UMF(NK)
         TRPPT=TRPPT+PPTLIQ(NK1)+PPTICE(NK1)
    ENDDO loop60

        CAPEI(I,J) = ABE
#ifdef DENG_SHCU1D
      IF( i == 1 .AND. j == 1 ) write(88,*) xtime/60.,ABE
#endif
!
! UPDRAFT TOP IS DEFINED AS THE LAGER VALUE OF LTOP1 (CTP1) AND LTOP1 (CTP2)
! WHERE LTOP1 IS THE KAIN-FRITSCH UPDRAFT TOP AND LTOP2 IS THE HEIGHT CALCULATED
! USING THE EQUATION: CTP2=CLDTOPB(I,J)+0.2*WUMAX*DT2/2.0
!
      LTOP1=NK
      CTP1=Z0(LTOP1)

      WUMAX=WU(1)
      DO NK=2,LTOPB(I,J)
        IF(WU(NK) .GT. WUMAX) WUMAX=WU(NK)
      ENDDO

      IF(LTOPB(I,J) .LE. 1) GOTO 666
      CTP2=CLDTOPB(I,J)+0.2*WUMAX*DT2/2.0
      LTOP2=1
      IF(CTP2 .GE. (Z0(KL)+DZQ(KL)/2.0)) THEN
        CTP2=Z0(KL)
        LTOP2=KL
      ELSE
        DO NK=1,KL
          IF(CTP2 .LT. (Z0(NK)+DZQ(NK)/2.0) .AND.  &
             CTP2 .GE. (Z0(NK)-DZQ(NK)/2.0)) LTOP2 = NK
        ENDDO
        CTP2=MAX(CTP2,ZLCL)
        LTOP2=MAX(LTOP2,KLCL)
      ENDIF

      CLDTOP=MIN(CTP1,CTP2)
      LTOP=MIN(LTOP1,LTOP2)
      GOTO 667
 666  CONTINUE
      CLDTOP=CTP1
      LTOP=LTOP1
 667  CONTINUE
#ifdef DENG_SHCU1D
      IF( i == 1 .AND. j == 1 ) write(89,'(8f10.3,i3)') xtime/60.,CLDTOPB(I,J),CTP1,CTP2,  &
         CLDTOP,ZLCL,WUMAX,ABE, LTOPB(I,J)
#endif
!
!   MODIFY TRPPT CONSIDERING ACTUAL CLOUD TOP WHICH IS
!   LOWER IN MOST CASES
!
      IF(LTOP .EQ. LTOP2 .AND. LTOP1 .GT. LTOP2) THEN
        DO NK=LTOP2+1,LTOP1
          TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)
        ENDDO
      ENDIF
      CLDDPTH=CLDTOP-ZLCL
      IF(CLDDPTH .LE. 0.0) CLDDPTH=0.0
#ifdef DENG_SHCU1D
      CLDHGTG(I,J)=CLDDPTH           ! Graphics
      PCLDTPG(I,J)=P0(LTOP)          ! Graphics
      TCLDTPG(I,J)=TU(LTOP)          ! Graphics
#endif
!
! FIND LEVEL OF FREE CONVECTION.  WHEN THE PARCCEL IS WARMER THAN THE ENVIRONMENT
! AT ITS LCL, IT NEED AT LEAST 400 M TO SAY THAT LFC IF AT LCL
!
      ZLFC=Z0(KL)
      IF(CLDDPTH .LE. 0.0) THEN
        ZLFC=Z0(KL)
        GOTO 67
      ELSE IF(TVQU(KLCL) .GE. TV0(KLCL)) THEN
        SUM=0.0
        KVAL=MAX(KLCL,KKPBL)
        loop61: DO KS=KVAL,LTOP1
          IF(TVQU(KS).GT.TV0(KS)) THEN
            SUM=SUM+DZQ(KS)
          ELSE
            EXIT loop61
          ENDIF
        ENDDO loop61

        IF(SUM .GE. 300.) THEN
          ZLFC=ZLCL
          GO TO 67
        ENDIF
      ENDIF

      KVAL=KLCL
      loop66: DO KK =KVAL,LTOP1-1
      IF(TVQU(KK) .LT. TV0(KK) .AND. TVQU(KK+1).GT.TV0(KK+1)) THEN
        K1=KK
        K2=KK+1
!       VAL0=TVQU(K2)-TV0(K2)+TV0(K1)-TVQU(K1)
!       IF( ABS(VAL0) < 0.001 ) then
!         if( dbg_level > 100 ) then
!           write(message,*) kval, k1, k2, TVQU(K2), TV0(K2), TVQU(K1), TV0(K1), TVQU(K2)-TV0(K2)+TV0(K1)-TVQU(K1)
!           call wrf_message( message )
!         endif
!         STOP 999    ! aj
!       ENDIF
!       VAL1=(TVQU(K2)-TV0(K2))/VAL0
!       VAL2=(TV0(K1)-TVQU(K1))/VAL0
!       VAL=VAL1*Z0(K1)+VAL2*Z0(K2)
        VAL=Z0(K2)

        SUM=0.0
        loop62: DO KS=K2,LTOP1
          IF(TVQU(KS).GT.TV0(KS)) THEN
            SUM=SUM+DZQ(KS)
          ELSE
            EXIT loop62
          ENDIF
        ENDDO loop62

        IF(SUM .GE. 300.) THEN
          ZLFC=VAL
          EXIT loop66
        ELSE
          CYCLE loop66
        ENDIF
      ENDIF

      ENDDO loop66

 67   CONTINUE

      ZLFC=MAX(ZLFC,ZPBL)
!     ZLFC=ZLCL
!     ZLFC=Z0(KL)
!=================================================================
#ifdef DENG_SHCU1D
      DO NK=LLC,K
        TUG(NK)=TLCL*(P0(NK)/PLCL)**ROCPQ
      ENDDO
      DO NK=K+1,LTOP
        TUG(NK)=TU(NK)
      ENDDO
#endif

      IF( CLDTOP .LE. ZLCL .OR. LTOP .LT. KLCL) THEN
#ifdef DENG_SHCU1D
        PLET = P0(1)
        TLET = TLCL*(PLET/PLCL)**ROVCP
        PLFSG(I,J) = P0(1)
        TLFSG(I,J) = TLCL*(PLFSG(I,J)/PLCL)**ROVCP
        PLDTG(I,J) = P0(1)
        TLDTG(I,J) = TLCL*(PLDTG(I,J)/PLCL)**ROVCP
        PLDBG(I,J) = P0(1)
        TLDBG(I,J) = TLCL*(PLDBG(I,J)/PLCL)**ROVCP
#endif
        CLDTOP = Z0(1)
        LTOP = 1
        GO TO 425
      END IF
      IF( LET .GT. LTOP ) LET = LTOP
!
!...IF THE LET AND LTOP ARE THE SAME, DETRAIN ALL OF THE UPDRAFT MASS FLUX AT
!   THIS LEVEL...
!
      IF(LET.EQ.LTOP)THEN
         UPOLD=UMF(LTOP-1)-UDR(LTOP)
         UPNEW=UPOLD+UER(LTOP)
         UDR(LTOP)=UMF(LTOP)+UDR(LTOP)-UER(LTOP)
         IF( ABS(UPOLD) < 0.001 ) STOP 907
         DETLQ(LTOP)=RLIQ(LTOP)*UDR(LTOP)*UPNEW/UPOLD
         DETIC(LTOP)=RICE(LTOP)*UDR(LTOP)*UPNEW/UPOLD
         UER(LTOP)=0.
         UMF(LTOP)=0.
         GOTO 85
      ENDIF
!
!   BEGIN TOTAL DETRAINMENT AT THE LEVEL ABOVE THE LET...
!
       DPTT=0.
       DO NJ=LET+1,LTOP
         DPTT=DPTT+DP(NJ)
       ENDDO
       DUMFDP=UMF(LET)/DPTT
!
!...ADJUST MASS FLUX PROFILES, DETRAINMENT RATES, AND PRECIPITATION FALLOUT
!   RATES TO REFLECT THE LINEAR DECREASE IN MASS FLX BETWEEN THE LET AND LTOP
!
      DO NK=LET+1,LTOP
        UDR(NK)=DP(NK)*DUMFDP
        UMF(NK)=UMF(NK-1)-UDR(NK)
        DETLQ(NK)=RLIQ(NK)*UDR(NK)
        DETIC(NK)=RICE(NK)*UDR(NK)
        IF(NK.GE.LET+2)THEN
          TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)
          PPTLIQ(NK)=UMF(NK-1)*QLQOUT(NK)
          PPTICE(NK)=UMF(NK-1)*QICOUT(NK)
          TRPPT=TRPPT+PPTLIQ(NK)+PPTICE(NK)
        ENDIF
      ENDDO

85    CONTINUE
!
!...EXTEND THE UPDRAFT MASS FLUX PROFILE DOWN TO THE SOURCE LAYER FOR THE
!   UPDRAFT AIR...ALSO, DEFINE THETAE FOR LEVELS BELOW THE LCL...
!
      QMIX1 = 0.0
      ZMIX1 = 0.0
      DPTHMX1 = 0.0
      DO NK = LLC, K
        DPTHMX1 = DPTHMX1 + DP(NK)
        QMIX1 = QMIX1 + DP(NK)*Q0(NK)
        ZMIX1 = ZMIX1 + DP(NK)*Z0(NK)
      ENDDO
      QMIX1 = QMIX1 / DPTHMX1
      ZMIX1 = ZMIX1 / DPTHMX1

      DO NK=1,K
        IF(NK.GE.LLC)THEN
           IF(NK.EQ.LLC)THEN
             UMF(NK)=VMFPBLCL*DP(NK)/DPTHMX1
             UER(NK)=VMFPBLCL*DP(NK)/DPTHMX1
           ELSEIF(NK .LE. K)THEN
             UER(NK)=VMFPBLCL*DP(NK)/DPTHMX1
             UMF(NK)=UMF(NK-1)+UER(NK)
           ELSE
             UMF(NK)=VMFPBLCL
             UER(NK)=0.
           ENDIF
           QU(NK)=QMIX1
           TU(NK)=TLCL+(Z0(NK)-ZLCL)*GDRY
           WU(NK)=WPBLTP
        ELSE
           TU(NK)=0.
           QU(NK)=0.
           UMF(NK)=0.
           WU(NK)=0.
           UER(NK)=0.
        ENDIF
        UDR(NK)=0.
        QDT(NK)=0.
        RLIQ(NK)=0.
        RICE(NK)=0.
        QLQOUT(NK)=0.
        QICOUT(NK)=0.
        PPTLIQ(NK)=0.
        PPTICE(NK)=0.
        DETLQ(NK)=0.
        DETIC(NK)=0.
        RATIO2(NK)=0.
        EE=Q0(NK)*P0(NK)/(0.622+Q0(NK))
        TLOG=ALOG(EE/SVP1/1000.)
        TDPT=(SVP2*SVPT0-SVP3*TLOG)/(SVP2-TLOG)
        TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T0(NK)-T00))*(T0(NK)-TDPT)
        THTA=T0(NK)*(1.E5/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))
        THETEE(NK)=THTA*EXP((3374.6525/TSAT-2.5403)*Q0(NK)*(1.+0.81*Q0(NK)))
        THTES(NK)=THTA*EXP((3374.6525/T0(NK)-2.5403)*QES(NK)*(1.+0.81*QES(NK)))
        EQFRC(NK)=1.0
      ENDDO

      LTOP1=LTOP+1
      LTOPM1=LTOP-1
!
!...DEFINE VARIABLES ABOVE CLOUD TOP...
!
    DO NK = LTOP1, KX
      UMF(NK) = 0.
      UDR(NK)=0.
      UER(NK)=0.
      QDT(NK)=0.
      RLIQ(NK)=0.
      RICE(NK)=0.
      QLQOUT(NK)=0.
      QICOUT(NK)=0.
      DETLQ(NK)=0.
      DETIC(NK)=0.
      PPTLIQ(NK)=0.
      PPTICE(NK)=0.
      IF(NK.GT.LTOP1)THEN
        TU(NK)=0.
        QU(NK)=0.
        WU(NK)=0.
      ENDIF
      THTA0(NK)=0.
      THTAU(NK)=0.
      EMS(NK)=DP(NK)*DXSQ/G
      EMSD(NK)=1./EMS(NK)
      TG(NK)=T0(NK)
      QQG(NK)=Q0(NK)
      QCG(NK)=0.
      DCA(NK)=0.
      DMS(NK)=0.
      DTFM(NK)=0.
      FXM(NK)=0.0
      OMG(NK)=0.
    ENDDO
    OMG(KXP1)=0.

#ifdef DENG_SHCU1D
!
! 1-D GRAPHICS
!
      DO NK=1,KL
        wutemp(nk)=wu(NK)
      ENDDO
#endif

      P165=P0(KLCL)-1.65E4
      PPTMLT=0.
    DO NK=1,LTOP
      EMS(NK)=DP(NK)*DXSQ/G
      EMSD(NK)=1./EMS(NK)
      DTFM(NK)=0.
!
!...INITIALIZE SOME VARIABLES TO BE USED LATER IN THE VERT ADVECTION SCHEME.
!
      EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QDT(NK)))
      THTAU(NK)=TU(NK)*EXN(NK)
      EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))
      THTA0(NK)=T0(NK)*EXN(NK)

      IF(P0(NK).GT.P165) LVF=NK
      PPTMLT=PPTMLT+PPTICE(NK)

      OMG(NK)=0.
    ENDDO

      CLVF=0.
      DO NK=KLCL,LVF+1
        CLVF=CLVF+PPTLIQ(NK)+PPTICE(NK)
      ENDDO
      USR=UMF(LVF+1)*QU(LVF+1)+CLVF
      USR=MIN(USR,TRPPT)
!     WRITE(28,1025)KLCL,ZLCL,LTOP,P0(LTOP),IFLAG,  &
!      TMIX-T00,PMIX,QMIX,ABE
!     WRITE(28,1030)P0(LET)/100.,P0(LTOP)/100.,VMFPBLCL,XTIME/60.  &
!       ,PLCL/100.,WPBLTP,CLDDPTH
!1025   FORMAT(5X,' KLCL=',I2,' ZLCL=',F7.1,'M',                       &
!       ' LTOP=',I2,' P0(LTOP)=',-2PF5.1,'MB FRZ LV=',  &
!       I2,' TMIX=',0PF4.1,1X,'PMIX=',-2PF6.1,' QMIX=',3PF5.1,         &
!       ' CAPE=',0PF7.1)
!1030   FORMAT(' ',' P0(LET) = ',F6.1,' P0(LTOP) = ',F6.1,' VMFLCL =',    &
!     E12.3,' XTIME =',F10.3,' PLCL =',F6.1,' WLCL =',F6.3,' CLDHGT =',   &
!     F8.1)
!
!...COMPUTE CONVECTIVE TIME SCALE(TIMEC). THE MEAN WIND AT THE LCL
!...AND MIDTROPOSPHERE IS USED.
!
      WSPD(KLCL)=SQRT(U0(KLCL)*U0(KLCL)+V0(KLCL)*V0(KLCL))
      WSPD(L5)=SQRT(U0(L5)*U0(L5)+V0(L5)*V0(L5))
      WSPD(LTOP)=SQRT(U0(LTOP)*U0(LTOP)+V0(LTOP)*V0(LTOP))
      VCONV = .5*(WSPD(KLCL)+WSPD(L5))
      TIMEC = DX/VCONV
      TADVEC = TIMEC
      TCMIN = 1800.
      TCMAX = 3600.
      TIMEC = MAX(TCMIN,TIMEC)
      TIMEC = MIN(TCMAX,TIMEC)
      NIC = NINT(TIMEC/(.5*DT2))
      TIMEC = FLOAT(NIC)*.5*DT2
!
!...COMPUTE WIND SHEAR AND PRECIPITATION EFFICIENCY.
!
      IF(WSPD(LTOP).GT.WSPD(KLCL))THEN
        SHSIGN=1.
      ELSE
        SHSIGN=-1.
      ENDIF
      VWS=(U0(LTOP)-U0(KLCL))*(U0(LTOP)-U0(KLCL))+(V0(LTOP)-V0(KLCL))*(V0(LTOP)-V0(KLCL))
      IF(LTOP .LE. LCL) GO TO 425
      VWS=1.E3*SHSIGN*SQRT(VWS)/(Z0(LTOP)-Z0(LCL))
      PEF=1.591+VWS*(-.639+VWS*(9.53E-2-VWS*4.96E-3))
      PEFMX=0.9
      PEFMN=0.2
      PEF=MAX(PEF,PEFMN)
      PEF=MIN(PEF,PEFMX)
!
!...PRECIPITATION EFFICIENCY IS A FUNCTION OF THE HEIGHT OF CLOUD BASE.
!
      CBH=(ZLCL-Z0(1))*3.281E-3
      IF(CBH.LT.3.) THEN
        RCBH=.02
      ELSE
        RCBH=.96729352+CBH*(-.70034167+CBH*(.162179896+CBH*(-  &
               1.2569798E-2+CBH*(4.2772E-4-CBH*5.44E-6))))
      ENDIF
      IF (CBH.GT.25) RCBH=2.4
      PEFCBH=1./(1.+RCBH)
      CBHMX=0.9
      PEFCBH=MIN(PEFCBH,CBHMX)
!
!... MEAN PEF. IS USED TO COMPUTE RAINFALL.
!     DOWNDRAFT IS TURNED OFF BY RATE=0.0
!
      PEFF=.5*(PEF+PEFCBH)
      IF(CLDDPTHB(I,J) .LT. 4.E3) THEN
        USR=TRPPT
        PEFF=0.99999
      ENDIF
!  IF ( wrf_dm_on_monitor()) THEN
!    CALL get_wrf_debug_level( dbg_level )
!    IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
!     WRITE(message,1035)PEF,PEFCBH,LC,LET,VWS
!1035 FORMAT(1X,'PEF(WS)=',F4.2,'(CB)=',F4.2,'LC,LET=',2I3,'VWS=',F5.2)
!     CALL wrf_message( message )
!    ENDIF
!  ENDIF
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!     BEGINING OF DOWNDRAFT CACULATION
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!...LET DOWNDRAFT ORIGINATE AT THE LEVEL OF MINIMUM SATURATION EQUIVALENT
!...POTENTIAL TEMPERATURE (SEQT) IN THE CLOUD LAYER, EXTEND DOWNWARD TO THE
!...SURFACE, OR TO THE LAYER BELOW CLOUD BASE AT WHICH ENVIR SEQT IS LESS
!...THAN MIN SEQT IN THE CLOUD LAYER...LET DOWNDRAFT DETRAIN OVER A LAYER
!...OF SPECIFIED PRESSURE-DEPTH (DPDD)...
!
      TDER=0.
      KSTART=MAX0(KKPBL,KLCL)
      THTMIN=THTES(KSTART+1)
      KMIN=KSTART+1
      DO NK=KSTART+2,LTOP-1
        THTMIN=MIN(THTMIN,THTES(NK))
        IF(THTMIN.EQ.THTES(NK))KMIN=NK
      ENDDO
      LFS=KMIN
      IF(LFS .GE. LTOP) LFS=LTOP-1
      IF(RATIO2(LFS).GT.0.)CALL ENVIRTHT(P0(LFS),T0(LFS),Q0(LFS), &
                                           THETEE(LFS),0.,RL,     &
           SVP1,SVP2,SVPT0,SVP3,I,J)
!     IF(RATIO2(LFS).GT.0.)CALL ENVIRTHT2(P0(LFS),T0(LFS),Q0(LFS), &
!                                          THETEE(LFS),ALIQ,BLIQ,CLIQ,DLIQ)

      IF( ABS(THETEE(LFS)-THETEU(LFS)) < 1.E-3 ) THEN    ! DENG 20140110 fix to avoid dividing by 0
        EQFRC(LFS) = 1.0
      ELSE
        EQFRC(LFS)=(THTES(LFS)-THETEU(LFS))/(THETEE(LFS)-THETEU(LFS))
      ENDIF
      EQFRC(LFS)=MAX(EQFRC(LFS),0.)
      EQFRC(LFS)=MIN(EQFRC(LFS),1.)
      THETED(LFS)=THTES(LFS)
!
!...ESTIMATE THE EFFECT OF MELTING ON THE DOWNDRAFT...
!
      IF(ML.GT.0)THEN
        DTMLTD=0.5*(QU(KLCL)-QU(LTOP))*RLF/CP
      ELSE
        DTMLTD=0.
      ENDIF
      TZ(LFS)=T0(LFS)-DTMLTD
      ES=1.E3*SVP1*EXP(SVP2*(TZ(LFS)-SVPT0)/(TZ(LFS)-SVP3))
      QS = 0.622*ES/(P0(LFS)-ES)
      QD(LFS)=EQFRC(LFS)*Q0(LFS)+(1.-EQFRC(LFS))*QU(LFS)
      THTAD(LFS)=TZ(LFS)*(P00/P0(LFS))**(0.2854*(1.-0.28*QD(LFS)))
      IF( QD(LFS) .GE. QS ) THEN
        THETED(LFS)=THTAD(LFS)* &
                  EXP((3374.6525/TZ(LFS)-2.5403)*QS*(1.+0.81*QS))
      ELSE
        CALL ENVIRTHT(P0(LFS),TZ(LFS),QD(LFS),  &
                                           THETED(LFS),0.,RL,  &
         SVP1,SVP2,SVPT0,SVP3,I,J)
!       CALL ENVIRTHT2(P0(LFS),TZ(LFS),QD(LFS),  &
!                                          THETED(LFS),ALIQ,BLIQ,CLIQ,DLIQ)
      ENDIF
!
!...DETERMINE THE LOWEST LEVEL TO WHICH DOWNDRAFT CAN PENETRATE (LDB)...
!
      loop107: DO NK=1,LFS
        ND=LFS-NK
        IF(THETED(LFS).GT.THTES(ND) .OR. ND.EQ.1)THEN
          LDB=ND
          EXIT loop107
        ENDIF
      ENDDO loop107
!
!...ASSUME ALL OF DOWNDRAFT DETRAINMENT OCCURS AT ITS BASE, SO THE TOP OF THE 
!...DETRAINMENT LAYER (LDT) IS THE SAME AS LDB...
!
      DPDD= 20.0E2
      DPT=0.
      loop115: DO NK=LDB,LFS
        DPT=DPT+DP(NK)
        IF(DPT.GT.DPDD)THEN
          LDT=NK
          FRC=(DPDD+DP(NK)-DPT)/DP(NK)
          EXIT loop115
        ENDIF
        IF(NK.EQ.LFS-1)THEN
          LDT=NK
          FRC=1.
          DPDD=DPT
          EXIT loop115
        ENDIF
      ENDDO loop115

       IF(LDB.EQ.LFS) THEN
         TDER=0.
         GOTO 141
       ENDIF
!
!...MAKE A FIRST GUESS AT INITIAL DOWNDRAFT MASS FLUX, DETERMINE ITS VERTICAL 
!...PROFILE...
!
      TVD(LFS)=T0(LFS)*(1.+0.608*QES(LFS))
      RDD=P0(LFS)/(R*TVD(LFS))
      A1=(1.-PEFF)*AU0
      DMF(LFS)=-A1*RDD
      DER(LFS)=EQFRC(LFS)*DMF(LFS)
      DDR(LFS)=0.
      DO ND=LFS-1,LDB,-1
        ND1=ND+1
        IF(ND.LE.LDT)THEN
          DER(ND)=0.
          DDR(ND)=-DMF(LDT+1)*DP(ND)*FRC/DPDD
          DMF(ND)=DMF(ND1)+DDR(ND)
          FRC=1.
          THETED(ND)=THETED(ND1)
          QD(ND)=QD(ND1)
        ELSE
          DER(ND)=DMF(LFS)*0.03*DP(ND)/RAD
          DDR(ND)=0.
          DMF(ND)=DMF(ND1)+DER(ND)
          IF(RATIO2(ND).GT.0.)CALL ENVIRTHT(P0(ND),T0(ND),Q0(ND), &
                                           THETEE(ND),0.,RL,      &
          SVP1,SVP2,SVPT0,SVP3,I,J)
!         IF(RATIO2(ND).GT.0.)CALL ENVIRTHT2(P0(ND),T0(ND),Q0(ND), &
!                                          THETEE(ND),ALIQ,BLIQ,CLIQ,DLIQ)
          THETED(ND)=(THETED(ND1)*DMF(ND1)+THETEE(ND)*DER(ND))/DMF(ND)
          QD(ND)=(QD(ND1)*DMF(ND1)+Q0(ND)*DER(ND))/DMF(ND)
        ENDIF
      ENDDO
!
!...DETERMINE TOTAL DOWNDRAFT EVAPORATION RATE (TDER)
!
      TDER=0.
      DO ND=LDB,LDT
        TZ(ND)=TPDDBG(P0(ND),THETED(LDT),T0(ND),QS,QD(ND),1.0, &
                       XLV0,XLV1,                            &
            SVP1,SVP2,SVPT0,SVP3)
!       CALL TPMIX2DD(p0(ND),theted(LDT),tz(ND),qs,ktau,i,j,nd)

        ES=1.E3*SVP1*EXP(SVP2*(TZ(ND)-SVPT0)/(TZ(ND)-SVP3))
        QS=0.622*ES/(P0(ND)-ES)
        DQSSDT=SVP2*(SVPT0-SVP3)/((TZ(ND)-SVP3)*(TZ(ND)-SVP3))
        RL=XLV0-XLV1*TZ(ND)
        DTMP=RL*QS*(1.-RHBC)/(CP+RL*RHBC*QS*DQSSDT)
        T1RH=TZ(ND)+DTMP
        ES=RHBC*1.E3*SVP1*EXP(SVP2*(T1RH-SVPT0)/(T1RH-SVP3))
        QSRH=0.622*ES/(P0(ND)-ES)
!
!...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL
!...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...
!
        IF(QSRH.LT.QD(ND))THEN
          QSRH=QD(ND)
          T1RH=TZ(ND)
        ENDIF
        TZ(ND)=T1RH
        QS=QSRH
        TDER=TDER + (QS-QD(ND))*DDR(ND)
        QD(ND)=QS
        THTAD(ND)=TZ(ND)*(P00/P0(ND))**(0.2854*(1.-0.28*QD(ND)))
      ENDDO
!
!...IF DOWNDRAFT DOES NOT EVAPORATE ANY WATER FOR SPECIFIED RELATIVE
!...HUMIDITY, NO DOWNDRAFT IS ALLOWED...
!
 141  CONTINUE 
      IF(TDER.LT.1.)THEN
        PPTFLX=TRPPT
        CPR=TRPPT
        TDER=0.
        CNDTNF=0.
        UPDINC=1.
        LDB=LFS

        DO NDK=1,KX
          DMS(NDK)=0.
          DMF(NDK)=0.
          DER(NDK)=0.
          DDR(NDK)=0.
          THTAD(NDK)=0.
          WD(NDK)=0.
          TZ(NDK)=0.
          QD(NDK)=0.
        ENDDO

        AINCM2=100.
        DMFMIN=0.
        GOTO 165
      ENDIF
!
!...ADJUST DOWNDRAFT MASS FLUX SO THAT EVAPORATION RATE IN DOWNDRAFT IS
!...CONSISTENT WITH PRECIPITATION EFFICIENCY RELATIONSHIP...
!
        DEVDMF=TDER/DMF(LFS)
        PPR=0.
        PPTFLX=PEFF*USR
        RCED=TRPPT-PPTFLX
!
!...PPR IS THE TOTAL AMOUNT OF PRECIPITATION THAT FALLS  OUT OF THE UPDRAFT
!...FROM CLOUD BASE TO THE LFS...UPDRAFT MASS FLUX WILL BE INCREASED UP TO
!...THE LFS TO ACCOUNT FOR UPDRAFT AIR MIXING WITH ENVIRONMENTAL AIR TO MAKE
!...THE UPDRAFT, SO PPR WILL INCREASE PROPORTIONATELY...
!
        DO NM=KLCL,LFS
          PPR=PPR+PPTLIQ(NM)+PPTICE(NM)
        ENDDO
        IF(LFS.GE.KLCL)THEN
          DPPTDF=(1.-PEFF)*PPR*(1.-EQFRC(LFS))/UMF(LFS)
        ELSE
          DPPTDF=0.
        ENDIF
!
!...CNDTNF IS THE AMOUNT OF CONDENSATE TRANSFERRED ALONG WITH UPDRAFT MASS TO
!...THE DOWNDRAFT AT THE LFS...
!
        CNDTNF=(RLIQ(LFS)+RICE(LFS))*(1.-EQFRC(LFS))
        DMFLFS=RCED/(DEVDMF+DPPTDF+CNDTNF)
        IF(DMFLFS.GT.0.)THEN
          TDER=0.
          GOTO 141
        ENDIF
!...DDINC IS THE FACTOR BY WHICH TO INCREASE THE FIRST-GUESS DOWNDRAFT MASS FLUX
!...TO SATISFY THE PRECIP EFFICIENCY RELATIONSHIP, UPDINC IS THE FACTOR BY WHICH
!...TO INCREASE THE UPDRAFT MASS FLUX BELOW THE LFS TO ACCOUNT FOR THE TRANSFER
!...OF MASS FROM UPDRAFT TO DOWNDRAFT...
!
        DDINC=DMFLFS/DMF(LFS)
        IF(LFS.GE.KLCL)THEN
          UPDINC=(UMF(LFS)-(1.-EQFRC(LFS))*DMFLFS)/UMF(LFS)
        ELSE
          UPDINC=1.
        ENDIF
      DO NK=LDB,LFS
        DMF(NK)=DMF(NK)*DDINC
        DER(NK)=DER(NK)*DDINC
        DDR(NK)=DDR(NK)*DDINC
      ENDDO
      CPR=TRPPT+PPR*(UPDINC-1.)
      PPTFLX = PPTFLX+PEFF*PPR*(UPDINC-1.)
      TDER=TDER*DDINC
!
!...ADJUST UPDRAFT MASS FLUX, MASS DETRAINMENT RATE, AND LIQUID WATER AND ICE
!   DETRAINMENT RATES TO BE CONSISTENT WITH THE TRANSFER OF THE ESTIMATED MASS
!   FROM THE UPDRAFT TO THE DOWNDRAFT AT THE LFS...
!
      DO NK=LC,LFS
        UMF(NK)=UMF(NK)*UPDINC
        UDR(NK)=UDR(NK)*UPDINC
        UER(NK)=UER(NK)*UPDINC
        PPTLIQ(NK)=PPTLIQ(NK)*UPDINC
        PPTICE(NK)=PPTICE(NK)*UPDINC
        DETLQ(NK)=DETLQ(NK)*UPDINC
        DETIC(NK)=DETIC(NK)*UPDINC
      ENDDO
      IF(LDB.GT.1)THEN
        DO NK=1,LDB-1
          DMF(NK)=0.
          DER(NK)=0.
          DDR(NK)=0.
          WD(NK)=0.
          TZ(NK)=0.
          QD(NK)=0.
          THTAD(NK)=0.
        ENDDO
      ENDIF

      DO NK=LFS+1,KX
        DMF(NK)=0.
        DER(NK)=0.
        DDR(NK)=0.
        WD(NK)=0.
        TZ(NK)=0.
        QD(NK)=0.
        THTAD(NK)=0.
      ENDDO

      DO NK=LDT+1,LFS-1
        TZ(NK)=0.
        QD(NK)=0.
      ENDDO
!
!...SET LIMITS ON THE UPDRAFT AND DOWNDRAFT MASS FLUXES SO THAT THE INFLOW
!   INTO CONVECTIVE DRAFTS FROM A GIVEN LAYER IS NO MORE THAN IS AVAILABLE
!   IN THAT LAYER INITIALLY...
!
165   CONTINUE
      AINCMX=1000.
      LMAX=MAX0(KLCL,LFS)
      DO NK=LLC,LMAX
        IF((UER(NK)-DER(NK)).GT.0.)  &
          AINCM1=EMS(NK)/((UER(NK)-DER(NK))*TIMEC)
       AINCMX=MIN(AINCMX,AINCM1)
      ENDDO

      AINC=1.
      IF(AINCMX.LT.AINC)AINC=AINCMX
!
!...SAVE THE RELEVENT VARIABLES FOR A UNIT UPDRFT AND DOWNDRFT...THEY WILL BE
!   ADJUSTED BY THE FACTOR AINC TO SATISFY THE STABILIZATION CLOSURE...
!
      NCOUNT=0
      PPTMLT=0.
      TDER2=TDER
      PPTFL2=PPTFLX
      DO NK=1,LTOP
        DETLQ2(NK)=DETLQ(NK)
        DETIC2(NK)=DETIC(NK)
        UDR2(NK)=UDR(NK)
        UER2(NK)=UER(NK)
        DDR2(NK)=DDR(NK)
        DER2(NK)=DER(NK)
        UMF2(NK)=UMF(NK)
        DMF2(NK)=DMF(NK)
        DMS(NK)=0.
        IF(NK.GT.ML .AND. NK.LT.LTOP)THEN
          PPTMLT=PPTMLT+PPTICE(NK+1)
        ENDIF
      ENDDO

      PPTML2=PPTMLT
      FABE=1.
      STAB=0.95
      AINC2=AINC
      IF(AINC/AINCMX.GT.0.999)GOTO 255
      ISTOP=0

      KKPBL=K
!
!...FIND THE MAXIMUM TKE VALUE BETWEEN LLC AND KLCL... AND USE IT TO
!  CALCULATE AINC2
!
      TKEMAX=0.
      DO KK=LLC,KLCL
        TKEMAX=AMAX1(TKEMAX,TKE0(KK))
      ENDDO
      TKEMAX=AMIN1(TKEMAX,10.)
      TKEMAX=AMAX1(TKEMAX,1.0)
      DSOURCE=Z0(K)-Z0(LLC)+0.5*(DZQ(K)+DZQ(LLC))
      TIMECS=DSOURCE/WPBLTP*45.0
      AINC2=TKEMAX*DPTHMX1*DXSQ/(VMFLCL*G*TIMECS)
#ifdef DENG_SHCU1D
      IF( i == 1 .AND. j == 1 ) write(808,'(i7,f10.3, 2i4)') ktau, xtime/60.,LLC, KLCL
      IF( i == 1 .AND. j == 1 ) write(809,'(i7,4f10.3)') ktau, xtime/60.,TKEMAX, &
           DSOURCE, AINC2
#endif
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!     BEGINING OF CLOSURE ASSUMPTION SELECTIONS
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
      IF(CAPEI(I,J) .LT. 100.0) THEN    ! averaging KF number of clouds
        NT = NINT(15.0*60.0/(0.5*DT))-1 ! for 15 min.
      ELSE
        NT = 0
      ENDIF

  175 NCOUNT=NCOUNT+1
!
!
!...DETERMINE OMEGA VALUE NECESSARY AT TOP AND BOTTOM OF EACH LAYER TO
!...SATISFY MASS CONTINUITY...
!
      DTT=TIMEC
      DO NK=1,LTOP
        DOMGDP(NK)=-(UER(NK)-DER(NK)-UDR(NK)-DDR(NK))*EMSD(NK)
        IF(NK.GT.1)THEN
          OMG(NK)=OMG(NK-1)-DP(NK-1)*DOMGDP(NK-1)
          DTT1 = 0.75*DP(NK-1)/(ABS(OMG(NK))+1.E-10)
          DTT=MIN(DTT,DTT1)
        ENDIF
      ENDDO
!
!  NOTE:
!       KFLAG=2: CLOUD TOP LOWER THAN LFC
!       KFLAG=3: CLOUD TOP HIGHER THAN LFC BUT LOWER THAN 4 KM
!       KFLAG=4: CLOUD TOP HIGHER THAN 4 KM
!
      IF(KFLAG .EQ. 2) GO TO 176

      IF((CLDDPTH+ZLCL) .LE. ZLFC) THEN
        AINC=AINC2
        KFLAG=2
        GO TO 255
      ENDIF

 176  CONTINUE

      DO NK=1,LTOP
         THPA(NK)=THTA0(NK)
         QPA(NK)=Q0(NK)
         QCPA(NK)= QC0(NK)
         DCA(NK) = DCA0(NK)
         NSTEP=NINT(TIMEC/DTT+1)
         DTIME=TIMEC/FLOAT(NSTEP)
         FXM(NK)=OMG(NK)*DXSQ/G
#ifdef DENG_SHCU1D
         wsubsub(nk)=-omg(nk)/rhoe(nk)/g
#endif
      ENDDO

      loop495: DO NTC=1,NSTEP
!
!...ASSIGN THETA AND Q VALUES AT THE TOP AND BOTTOM OF EACH LAYER BASED ON THE
!...SIGN OF OMEGA...
!
        DO NK=1,LTOP
          THFXIN(NK)=0.
          THFXOUT(NK)=0.
          QFXIN(NK)=0.
          QFXOUT(NK)=0.
          QCFXIN(NK)=0.
          QCFXOUT(NK)=0.
          DCAFXIN(NK)=0.0
          DCAFXOUT(NK)=0.0
        ENDDO

        DO NK=2,LTOP
!
!  THIS PART IS ALSO MODIFIED TO INCLUDE THE SUBSIDANCE EFFECT
!  ON DEAD CLOUD.  THE FLUXES INTO THE LAYER ARE MODIFIED
!  TO CONSIDER THE EFFECT ON QC,THETA,QV, GD ID ADIABATIC LAPSE
!  RATE AND GM IS MOIST ADIABATIC LAPSE RATE
!
          IF(OMG(NK).LE.0.)THEN
            THFXIN(NK)=-FXM(NK)*THPA(NK-1)
            QFXIN(NK)=-FXM(NK)*QPA(NK-1)
            QCFXIN(NK)=-FXM(NK)*QCPA(NK-1)
            DCAFXIN(NK) = -FXM(NK)*DCA(NK-1)
            THFXOUT(NK-1)=THFXOUT(NK-1)+THFXIN(NK)
            QFXOUT(NK-1)=QFXOUT(NK-1)+QFXIN(NK)
            QCFXOUT(NK-1)=QCFXOUT(NK-1)+QCFXIN(NK)
            DCAFXOUT(NK-1)=DCAFXOUT(NK-1)+DCAFXIN(NK)
          ELSE
            GD=G/CP
            CPM=CP*(1.+0.887*Q0(NK))
                IF(T0(NK) .GT. TO) THEN
                  RL=XLV0-XLV1*T0(NK)
                  DQSSDT=QES(NK)*SVP2*(SVPT0-SVP3) &
                         /((T0(NK)-SVP3)*(T0(NK)-SVP3))
                ELSE
                  RL=XLS
                  DQSSDT=QES(NK)*6.15E3/(T0(NK)*T0(NK))
                ENDIF
            GM=GD/(1.0+RL/CPM*DQSSDT)
            DTEMPDT=(GD-GM)*OMG(NK)/RHOE(NK)/G
            IF((QCPA(NK)-DTEMPDT*CP/RL*DTIME) .LE. 0.0)  &
              DTEMPDT=QCPA(NK)/(CP/RL*DTIME)
            DTHETADT=DTEMPDT*(P00/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))
            THFXOUT(NK)=FXM(NK)*(THPA(NK)-DTHETADT*DTIME)
            QFXOUT(NK)=FXM(NK)*(QPA(NK)  +DTEMPDT*CP/RL*DTIME)
            QCFXOUT(NK)=FXM(NK)*(QCPA(NK)-DTEMPDT*CP/RL*DTIME)
            DCAFXOUT(NK)=FXM(NK)*DCA(NK)
            THFXIN(NK-1)=THFXIN(NK-1)+THFXOUT(NK)
            QFXIN(NK-1)=QFXIN(NK-1)+QFXOUT(NK)
            QCFXIN(NK-1)=QCFXIN(NK-1)+QCFXOUT(NK)
            DCAFXIN(NK-1)=DCAFXIN(NK-1)+DCAFXOUT(NK)
          ENDIF
        ENDDO
!
!...UPDATE THE THETA AND QV VALUES AT EACH LEVEL...
!
     DO NK=LTOP,1,-1
      IF( NK .GT. KKPBL .OR. NK .LT. LLC) THEN
        THPA(NK)=THPA(NK)+   &
               (THFXIN(NK)+UDR(NK)*THTAU(NK)+DDR(NK)*THTAD(NK)-  &
               THFXOUT(NK)-(UER(NK)-DER(NK))*THTA0(NK))*DTIME*EMSD(NK)
        QPA(NK)=QPA(NK)+ &
              (QFXIN(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK) &
              -QFXOUT(NK)-(UER(NK)-DER(NK))*Q0(NK))*DTIME*EMSD(NK)
      ELSE
        THPA(NK)=THPA(NK)+ &
               (THFXIN(NK)+UDR(NK)*THTAU(NK)+DDR(NK)*THTAD(NK)- &
               THFXOUT(NK)-UER(NK)*THMIX+DER(NK)*THTA0(NK))* &
               DTIME*EMSD(NK)
        QPA(NK)=QPA(NK)+ &
              (QFXIN(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK) &
              -QFXOUT(NK)-UER(NK)*QMIX+DER(NK)*Q0(NK))* &
               DTIME*EMSD(NK)
      END IF
        QCPA(NK)=QCPA(NK)+  &
           (QCFXIN(NK)+DETLQ(NK)+DETIC(NK)-QCFXOUT(NK))*DTIME*EMSD(NK)
!
!  ADDING DEAD CLOUD AREA DCA WHICH WILL BE USED IN LOOP 320 TO CALCULATED
!  THE TENDENCY DDCADT IN LOOP 320,  THE TENDENCY (DQCDCDT) FOR DEAD CLOUD
!  LIQUID WATER CONTENT QCDC IS SOLVE AS A RESIDUAL OF DQLDT-DDCADT
!
      DCA(NK)=DCA(NK)+(DCAFXIN(NK)-DCAFXOUT(NK)+UDR(NK))*DTIME*EMSD(NK)
     ENDDO

    ENDDO loop495

      DO NK=LTOP,1,-1
        THTAG(NK)=THPA(NK)
        QQG(NK)=QPA(NK)
        QCG(NK)=QCPA(NK)
      ENDDO
!
!...CHECK TO SEE IF MIXING RATIO DIPS BELOW ZERO ANYWHERE;  IF SO, BORROW
!...MOISTURE FROM ADJACENT LAYERS TO BRING IT BACK UP ABOVE ZERO...
!
      DO NK = 1,LTOP
        IF(QQG(NK).LT.0.)THEN
          TMA = QQG(NK+1)*EMS(NK+1)
          TMB = QQG(NK-1)*EMS(NK-1)
          TMM = (QQG(NK)-1.E-9)*EMS(NK  )
          BCOEFF = -TMM/((TMA*TMA)/TMB+TMB)
          ACOEFF = BCOEFF*TMA/TMB
          TMB = TMB*(1.-BCOEFF)
          TMA = TMA*(1.-ACOEFF)
          QQG(NK) = 1.E-9
          QQG(NK+1) = TMA*EMSD(NK+1)
          QQG(NK-1) = TMB*EMSD(NK-1)
        ENDIF
      ENDDO
      TOPOMG = (UDR(LTOP)-UER(LTOP))*DP(LTOP)*EMSD(LTOP)
      IF(ABS(TOPOMG-OMG(LTOP)).GT. 1.E-3)THEN
        IF ( wrf_dm_on_monitor()) THEN
          CALL get_wrf_debug_level( dbg_level )
          IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
            WRITE(message,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;', &
             ' TOPOMG, OMG =',TOPOMG,OMG(LTOP)
            CALL wrf_message( message )
          ENDIF
        ENDIF
        ISTOP=1
        GOTO 265
      ENDIF
!
!...CONVERT THETA TO T, FREEZE ALL SUPERCOOLED DETRAINED LIQUID WATER BECAUSE
!...SUPERCOOLED WATER NOT ALLOWED IN EXMOIS...FREEZE SUPERCOOLED PRECIP IF
!...FEEDING IT BACK TO EXMOIS...
!
      DO NK=1,LTOP
        EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QQG(NK)))
        TG(NK)=THTAG(NK)/EXN(NK)
        IF(NK.GT.ML)DTFM(NK)=DETLQ(NK)*RLF*EMSD(NK)/CP
        TG(NK)=TG(NK)+DTFM(NK)*timec
        TVG(NK)=TG(NK)*(1.+0.608*QQG(NK))
      ENDDO
!
!...ALLOW FROZEN PRECIP TO MELT OVER A LAYER 200mb DEEP IF PRECIP IS NOT FED 
!...BACK TO EXMOIS...
!
      DTMLTE=0.
      TDP=0.
      loop231: DO K = 1,ML
        NK = ML-K+1
        TDP=TDP+DP(NK)
        IF(TDP.LT.2.E4)THEN
          DTFM(NK)=DTMLTE
          TG(NK)=TG(NK)+DTMLTE*TIMEC
        ELSE
          DTFM(NK)=DTMLTE*(2.E4+DP(NK)-TDP)/DP(NK)
          TG(NK)=TG(NK)+DTMLTE*TIMEC*(2.E4+DP(NK)-TDP)/DP(NK)
          EXIT loop231
        ENDIF
      ENDDO loop231

      IF(KFLAG .EQ. 2 .OR. KFLAG .EQ. 3) GO TO 265
!
!     COMPUTE NEW CLOUD AND CHANGE IN AVAILABLE BUOYANT ENERGY.
!
!...THE FOLLOWING COMPUTATIONS ARE SIMILAR TO THAT FOR UPDRAFT
!
      THMIX2=0.
      QMIX2=0.
      PMIX2=0.
      DO NK = 1,LM
        ROCPQ=0.2854*(1.-0.28*QQG(NK))
        THMIX2=THMIX2+DP(NK)*TG(NK)*(P00/P0(NK))**ROCPQ
        QMIX2=QMIX2+DP(NK)*QQG(NK)
        PMIX2=PMIX2+DP(NK)*P0(NK)
      ENDDO
      THMIX2=THMIX2/DPTHMX
      QMIX2=QMIX2/DPTHMX
      PMIX2=PMIX2/DPTHMX

      ROCPQ=0.2854*(1.-0.28*QMIX2)
      TMIX2=THMIX2*(PMIX2/P00)**ROCPQ
      ES=1.E3*SVP1*EXP(SVP2*(TMIX2-SVPT0)/(TMIX2-SVP3))
      QS=0.622*ES/(PMIX2-ES)
      IF(QMIX2.GT.QS)THEN
        RL=XLV0-XLV1*TMIX2
        CPM=CP*(1.+0.887*QMIX2)
        DQSSDT=QS*SVP2*(SVPT0-SVP3)/((TMIX2-SVP3)*(TMIX2-SVP3))
        DQ=(QMIX2-QS)/(1.+RL*DQSSDT/CPM)
        TMIX2=TMIX2+RL/CP*DQ
        QMIX2=QMIX2-DQ
        ROCPQ=0.2854*(1.-0.28*QMIX2)
        THMIX2=TMIX2*(P00/PMIX2)**ROCPQ
        TTLCL=TMIX2
        PPLCL=PMIX2
      ELSE
        QMIN=0.
        QMIX2=MAX(QMIX2,QMIN)
        EMIX2=QMIX2*PMIX2/(0.622+QMIX2)
        TLOG=LOG(EMIX2/SVP1/1000.)
        TDPT=(SVP2*SVPT0-SVP3*TLOG)/(SVP2-TLOG)
        TTLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX2-T00))* &
            (TMIX2-TDPT)
        TTLCL=MIN(TTLCL,TMIX2)
        CPORQ=1./ROCPQ
        PPLCL=P00*(TTLCL/THMIX2)**CPORQ
      ENDIF
      TVLCL=TTLCL*(1.+0.608*QMIX2)

      loop235: DO NK=LC,KL
        KLCL=NK
        IF(PPLCL.GE.P0(NK)) EXIT loop235
      ENDDO loop235

      K=KLCL-1

      DLP=LOG(PPLCL/P0(K))/LOG(P0(KLCL)/P0(K))
!
!...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...
!
      TENV=TG(K)+(TG(KLCL)-TG(K))*DLP
      QENV=QQG(K)+(QQG(KLCL)-QQG(K))*DLP
      TVEN=TENV*(1.+0.608*QENV)
      TVBAR=0.5*(TVG(K)+TVEN)
      ZLCL1=Z0(K)+R*TVBAR*LOG(P0(K)/PPLCL)/G
      TVAVG=0.5*(TVEN+TG(KLCL)*(1.+0.608*QQG(KLCL)))
      PPLCL=P0(KLCL)*EXP(G/(R*TVAVG)*(Z0(KLCL)-ZLCL1))
      THETEU(K)=TMIX2*(1.E5/PMIX2)**(0.2854*(1.-0.28*QMIX2))*  &
                 EXP((3374.6525/TTLCL-2.5403)*QMIX2*           &
                 (1.+0.81*QMIX2))
      ES=1.E3*SVP1*EXP(SVP2*(TENV-SVPT0)/(TENV-SVP3))
      QESE=0.622*ES/(PPLCL-ES)
      THTESG(K)=TENV*(1.E5/PPLCL)**(0.2854*(1.-0.28*QESE))*   &
          EXP((3374.6525/TENV-2.5403)*QESE*(1.+0.81*QESE))
!
!...COMPUTE ADJUSTED ABE(ABEG).
!
      ABEG=0.
      THTUDL=THETEU(K)
      THATA = TMIX2*(1.E5/PMIX2)**0.286
      THTFC = THATA*EXP((XLV0-XLV1*TTLCL)*QMIX2/(CP*TTLCL))
      DO NK=K,LTOPM1
        NK1=NK+1
        ES=1.E3*SVP1*EXP(SVP2*(TG(NK1)-SVPT0)/(TG(NK1)-SVP3))
        QESE=0.622*ES/(P0(NK1)-ES)
        THTESG(NK1)=TG(NK1)*(1.E5/P0(NK1))**(0.2854*(1.-0.28*QESE))*  &
           EXP((3374.6525/TG(NK1)-2.5403)*QESE*(1.+0.81*QESE))
        IF(NK.EQ.K)THEN
          DZZ=Z0(KLCL)-ZLCL1
        ELSE
          DZZ=DZA(NK)
        ENDIF
        BE=((2.*THTUDL)/(THTESG(NK1)+THTESG(NK))-1.)*DZZ
        IF(BE.GT.0.)ABEG=ABEG+BE*G
      ENDDO
!
!...FIND OUT HOW MUCH CAPE HAS BEEN REMOVED...
!
      C1=999.9
      C2=999.9
      C3=999.9
      AINC3=999.0
      IF(ABEG.EQ.0)THEN
        AINC=AINC*0.5
        IF(NCOUNT .LT. 10) THEN
          GOTO 255
        ELSE
          AINC4=AINC
          IF(CLDDPTH .GE. 4.E3) THEN
            KFLAG=4
            GOTO 265
          ELSE IF((CLDDPTH) .LT. 4.E3 .AND. &
             CLDDPTH+ZLCL .GT. ZLFC) THEN
            C1=CLDDPTH+ZLCL-ZLFC
            C2=4000.0-ZLFC+ZLCL
            IF( ABS(C2) < 0.001 ) STOP 901
            C3=C1/C2
            VAL = 0.0
            DO II = 1, NT
              VAL = VAL + AINCKFSA(I,II,J)
            ENDDO
            AINC=(AINC+VAL)/FLOAT(NT+1)
            AINCKFI(i,j)=AINC
            AINC3=C3*AINC+(1-C3)*AINC2
            AINC=AINC3
            KFLAG=3
            GO TO 255
          ENDIF
        ENDIF
      ENDIF
      DABE=MAX(ABE-ABEG,0.1*ABE)
      FABE=ABEG/(ABE+1.E-8)
      IF(AINC/AINCMX.GT.0.999 .AND. FABE.GT.1.05-STAB) THEN
        AINC4=AINC
        IF(CLDDPTH .GE. 4.E3) THEN
          KFLAG=4
          GOTO 265
        ELSE IF((CLDDPTH) .LT. 4.E3 .AND. &
             CLDDPTH+ZLCL .GT. ZLFC) THEN
          C1=CLDDPTH+ZLCL-ZLFC
          C2=4000.0-ZLFC+ZLCL
          IF( ABS(C2) < 0.001 ) STOP 902
          C3=C1/C2
            VAL = 0.0
            DO II = 1, NT
              VAL = VAL + AINCKFSA(I,II,J)
            ENDDO
            AINC=(AINC+VAL)/FLOAT(NT+1)
            AINCKFI(i,j)=AINC
          AINC3=C3*AINC+(1-C3)*AINC2
          AINC=AINC3
          KFLAG=3
          GO TO 255
        ENDIF
      ENDIF

      IF(NCOUNT.GT.10)THEN
        AINC4=AINC
        IF(CLDDPTH .GE. 4.E3) THEN
          KFLAG=4
          GOTO 265
        ELSE IF((CLDDPTH) .LT. 4.E3 .AND. &
             CLDDPTH+ZLCL .GT. ZLFC) THEN
          C1=CLDDPTH+ZLCL-ZLFC
          C2=4000.0-ZLFC+ZLCL
          IF( ABS(C2) < 0.001 ) STOP 903
          C3=C1/C2
            VAL = 0.0
            DO II = 1, NT
              VAL = VAL + AINCKFSA(I,II,J)
            ENDDO
            AINC=(AINC+VAL)/FLOAT(NT+1)
            AINCKFI(i,j)=AINC
          AINC3=C3*AINC+(1-C3)*AINC2
          AINC=AINC3
          KFLAG=3
          GO TO 255
        ENDIF
      ENDIF

      IF(FABE.LE.1.05-STAB.AND.FABE.GE.0.95-STAB) THEN
        AINC4=AINC
        IF(CLDDPTH .GE. 4.E3) THEN
          KFLAG=4
          GOTO 265
        ELSE IF((CLDDPTH) .LT. 4.E3 .AND. &
             CLDDPTH+ZLCL .GT. ZLFC) THEN
          C1=CLDDPTH+ZLCL-ZLFC
          C2=4000.0-ZLFC+ZLCL
          IF( ABS(C2) < 0.001 ) STOP 904
          C3=C1/C2
            VAL = 0.0
            DO II = 1, NT
              VAL = VAL + AINCKFSA(I,II,J)
            ENDDO
            AINC=(AINC+VAL)/FLOAT(NT+1)
            AINCKFI(i,j)=AINC
          AINC3=C3*AINC+(1-C3)*AINC2
          AINC=AINC3
          KFLAG=3
          GO TO 255
        ENDIF
      ENDIF
!
!...ADJUST MASS FLUX BY THE FACTOR AINC TO CONVERGE TO SPECIFIED DEGREE OF STAB-
!...ILIZATION...
!
   IF ( wrf_dm_on_monitor()) THEN
     CALL get_wrf_debug_level( dbg_level )
     IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
      WRITE(message,1080) LFS,LDB,LDT,TIMEC,NSTEP,NCOUNT,FABE,AINC
 1080   FORMAT(2X,'LFS,LDB,LDT =',3I3,' TIMEC, NSTEP=',F5.0,I3,  &
      'NCOUNT, FABE, AINC=',I2,1X,F5.3,F6.2)
       CALL wrf_message(message)
     ENDIF
   ENDIF

      AINC=AINC*STAB*ABE/(DABE+1.E-8)

255   CONTINUE

      AINC=MIN(AINCMX,AINC)
      AINCMIN = 0.
      AINC=MAX(AINC,AINCMIN)
      PPTMLT=PPTML2*AINC
      TDER=TDER2*AINC
      PPTFLX=PPTFL2*AINC
      DO NK=1,LTOP
        UMF(NK)=UMF2(NK)*AINC
        DMF(NK)=DMF2(NK)*AINC
        DETLQ(NK)=DETLQ2(NK)*AINC
        DETIC(NK)=DETIC2(NK)*AINC
        UDR(NK)=UDR2(NK)*AINC
        UER(NK)=UER2(NK)*AINC
        DER(NK)=DER2(NK)*AINC
        DDR(NK)=DDR2(NK)*AINC
        DTFM(NK)=0.
        DMS(NK)=0.
      ENDDO
!
!...IF THE DOWNDRAFT OVERDRAWS THE INITIAL MASS AVAILABLE AT THE LFS, ALLOW
!...PEFF TO CHANGE SO THAT DOWNDRAFT MASS FLUX IS NOT THE LIMITING FACTOR
!...IN THE TOTAL CONVECTIVE MASS FLUX.  THIS IS USUALLY NECESSARY ONLY WHEN
!...THE DOWNDRAFT IS VERY SHALLOW...
!
      DMFMIN=UER(LFS)-EMS(LFS)/TIMEC
      IF(DER(LFS).LT.DMFMIN .AND. DMFMIN.LT.0.)THEN
        RF=DMFMIN/DER(LFS)
        DO NK=LDB,LFS
          DER2(NK)=DER2(NK)*RF
          DDR2(NK)=DDR2(NK)*RF
          DMF2(NK)=DMF2(NK)*RF
          DER(NK)=DER2(NK)*AINC
          DDR(NK)=DDR2(NK)*AINC
          DMF(NK)=DMF2(NK)*AINC
        ENDDO
        TDER2=TDER2*RF
        TDER=TDER2*AINC
        IF(LFS.GE.KLCL)THEN
          UPDIN2=1.-(1.-EQFRC(LFS))*DMF(LFS)*UPDINC/UMF(LFS)
        ELSE
          UPDIN2=1.
        ENDIF
        CPR=TRPPT+PPR*(UPDIN2-1.)
        PEFF=1.-(TDER+(1.-EQFRC(LFS))*DMF(LFS)*(RLIQ(LFS)+RICE(LFS)))/  &
                                                              (CPR*AINC)
        PPTFL2=PEFF*CPR
        PPTFLX=PPTFL2*AINC
        F1=UPDIN2/(AINC*UPDINC)
        DO NK=LC,LFS
          UMF2(NK)=UMF(NK)*F1
          UMF(NK)=UMF2(NK)*AINC
          UDR2(NK)=UDR(NK)*F1
          UDR(NK)=UDR2(NK)*AINC
          UER2(NK)=UER(NK)*F1
          UER(NK)=UER2(NK)*AINC
          DETLQ2(NK)=DETLQ(NK)*F1
          DETLQ(NK)=DETLQ2(NK)*AINC
          DETIC2(NK)=DETIC(NK)*F1
          DETIC(NK)=DETIC2(NK)*AINC
          PPTML2=PPTML2-PPTICE(NK)*(1.-UPDIN2/UPDINC)
          PPTLIQ(NK)=PPTLIQ(NK)*F1*AINC
          PPTICE(NK)=PPTICE(NK)*F1*AINC
        ENDDO
        PPTMLT=PPTML2*AINC
        UPDINC=UPDIN2
      ENDIF

      GO TO 175

265   CONTINUE

#ifdef DENG_SHCU1D
    DO K=KLCL,LTOP
      CLDUA(K)= AINC*AU0/DXSQ  ! updraft fraction
    ENDDO
#endif

!  IF ( wrf_dm_on_monitor()) THEN
!    CALL get_wrf_debug_level( dbg_level )
!    IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
!      WRITE(message,1080)LFS,LDB,LDT,TIMEC,NSTEP,NCOUNT,FABE,AINC
!      CALL wrf_message(message)
!    ENDIF
!  ENDIF
!
!...SEND FINAL PARAMETERIZED VALUES TO OUTPUT FILES...
!
!  IF ( wrf_dm_on_monitor()) THEN
!    CALL get_wrf_debug_level( dbg_level )
!    IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
!     WRITE(message,3071)EQFRC(LFS),UPDINC
!     CALL wrf_message(message)
!3071 FORMAT('EQFRC(LFS) =',F7.4,'UPDINC =',F8.4)
!     WRITE(message,969)
!     CALL wrf_message(message)
!969  FORMAT(1x,'  P    ','     DP ','   DT K/D ',                      &
!        ' DR K/D ','   OMG  ',                                         &
!     ' DOMGDP ','   UMF  ','  UER  ','   UDR ','    DMF  ','   DER  '  &
!     ,'  DDR  ','  EMS ','   W0 ','  DETLQ ',' DETIC')
!    ENDIF
!  ENDIF

      DO NK=1,LTOP
        K=LTOP-NK+1
        DTT=(TG(K)-T0(K))*86400./TIMEC
        RL=XLV0-XLV1*TG(K)
        DR=-(QQG(K)-Q0(K))*RL*86400./(TIMEC*CP)
        UDFRC=UDR(K)*TIMEC*EMSD(K)
        UEFRC=UER(K)*TIMEC*EMSD(K)
        DDFRC=DDR(K)*TIMEC*EMSD(K)
        DEFRC=-DER(K)*TIMEC*EMSD(K)

!  IF ( wrf_dm_on_monitor()) THEN
!    CALL get_wrf_debug_level( dbg_level )
!    IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
!      WRITE(message,'(F8.2,3(F8.2),2(F8.3),F8.2,2F8.3,F8.2,6F7.3)')       &
!          P0(K)/100.,DP(K)/100.,DTT,DR,OMG(K),DOMGDP(K)*1.E4,             &
!          UMF(K)/1.E6,UEFRC,UDFRC,DMF(K)/1.E6,DEFRC,DDFRC,EMS(K)/1.E11,   &
!          W0AVG0(K)*1.E2,DETLQ(K)*TIMEC*EMSD(K)*1.E3,DETIC(K)*            &
!          TIMEC*EMSD(K)*1.E3
!      CALL wrf_message(message)
!    ENDIF
!  ENDIF

      ENDDO

#ifdef DENG_SHCU1D
    DO k = 1, KL
      tt6(k)=UMF(k)*QU(k)
      tt7(k)=OMG(k)/G*DXSQ*Q0(k)
    ENDDO
    CLDBMFLX(I,J)=UMF(KCBASE)/1.0E+6
#endif

!     IF ( wrf_dm_on_monitor()) THEN
!       CALL get_wrf_debug_level( dbg_level )
!       IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
!          WRITE(message,'(A3,15A7,2A8)')                        &
!              'K','P','Z','T0','TG','DT','TU','TD','Q0','QQG',  &
!              'DQ','QU','QD','QCG','WU','WD','RH0','RHG'
!          CALL wrf_message(message)
!       ENDIF
!     ENDIF

      DO NK=1,KX
        K=KX-NK+1
        DTT=TG(K)-T0(K)
        TUC=TU(K)-T00
        IF(K.LT.LC .OR. K.GT.LTOP)TUC=0.
        TDC=TZ(K)-T00
        IF((K.LT.LDB .OR. K.GT.LDT) .AND. K.NE.LFS)TDC=0.
        ES=1.E3*SVP1*EXP(SVP2*(TG(K)-SVPT0)/(TG(K)-SVP3))
        QGS=ES*0.622/(P0(K)-ES)
        RH_0=Q0(K)/QES(K)
        RHG=QQG(K)/QGS

!       IF ( wrf_dm_on_monitor()) THEN
!         CALL get_wrf_debug_level( dbg_level )
!         IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
!           WRITE(message,'(I3,F7.2,F7.0,10F7.2,F7.3,2F7.2,2F8.3)')            &
!               K,P0(K)/100.,Z0(K),T0(K)-T00,TG(K)-T00,DTT,TUC,                &
!               TDC,Q0(K)*1000.,QQG(K)*1000.,(QQG(K)-Q0(K))*1000.,QU(K)*       &
!               1000.,QD(K)*1000.,QCG(K)*1000.,WU(K),WD(K),RH_0,RHG
!           CALL wrf_message(message)
!         ENDIF
!       ENDIF

      ENDDO
!
!...IF CALCULATIONS ABOVE SHOW AN ERROR IN THE MASS BUDGET, PRINT OUT A SOUNDING
!...TO BE USED LATER FOR DIAGNOSTIC PURPOSES, THEN ABORT RUN...
!
    IF(ISTOP.EQ.1)THEN
      IF ( wrf_dm_on_monitor()) THEN
        CALL get_wrf_debug_level( dbg_level )
        IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
            DO K = 1,KX
              WRITE(message,'(2X,F6.0,2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4)')  &
                     Z0(K),P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,                       &
                     U0(K),V0(K),DP(K)/100.,W0AVG0(K)
              CALL wrf_message(message)
            ENDDO
        ENDIF
      ENDIF
    ENDIF

    CNDTNF=(1.-EQFRC(LFS))*(RLIQ(LFS)+RICE(LFS))*DMF(LFS)

!   IF ( wrf_dm_on_monitor()) THEN
!     CALL get_wrf_debug_level( dbg_level )
!     IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
!       WRITE(message,1095)CPR*AINC,TDER+PPTFLX+CNDTNF
!1095 FORMAT(' ','  PPT PRODUCTION RATE= ',F10.0,' TOTAL EVAP+PPT= ',F10.0)
!       CALL wrf_message(message)
!     ENDIF
!   ENDIF
!
!  EVALUATE MOISTURE BUDGET...
!
      QINIT=0.
      QFNL=0.
      QCFINL=0.
      DO NK=1,LTOP
          QINIT=QINIT+Q0(NK)*EMS(NK)
          QFNL=QFNL+(QQG(NK)+QCG(NK))*DP(NK)*DXSQ/G
          QCFINL=QCFINL+QCG(NK)*DP(NK)*DXSQ/G
      ENDDO
      QFNL=QFNL+PPTFLX*TIMEC
      ERR2=(QFNL-QINIT)*100./QINIT
      RELERR=ERR2*QINIT/(PPTFLX*TIMEC+1.E-10)

    IF ( wrf_dm_on_monitor()) THEN
      CALL get_wrf_debug_level( dbg_level )
      IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
        WRITE(message,*)'QFNL-QINIT, QCFINL =',QFNL-QINIT,QCFINL
        CALL wrf_message(message)
        WRITE(message,1110)QINIT,QFNL,ERR2
        CALL wrf_message(message)
        WRITE(message,1200)RELERR
        CALL wrf_message(message)
 1110  FORMAT(' ','INITIAL WATER =',E12.5,' FINAL WATER =',E12.5,  &
       ' TOTAL WATER CHANGE =',F8.2,'%')
 1200  FORMAT(' ','MOISTURE ERROR AS FUNCTION OF TOTAL PPT =',F9.3,'%')
!       WRITE(message,*)'TDER, CPR, USR, TRPPT =',TDER,CPR*AINC,USR*AINC,TRPPT*AINC
!       CALL wrf_message(message)
      ENDIF
    ENDIF
!
!...IF THE ADVECTIVE TIME PERIOD (TADVEC) IS LESS THAN SPECIFIED MINIMUM FOR
!...TIMEC, ALLOW FEEDBACK TO OCCUR ONLY DURING TADVEC...
!
      IF(TADVEC.LT.TIMEC)NIC=NINT(TADVEC/(0.5*DT2))
!
!...CALCULATE CONVECTIVE TENDENCIES...
!
    loop320: DO K = KX,1,-1
      DTDT(K) = (TG(K)-T0(K))/TIMEC
      DQDT(K) = (QQG(K)-Q0(K))/TIMEC
      DQLDT(K)= (QCG(K)-QC0(K))/TIMEC
      DQRDT(K)= 0.0                  ! set this to zero for now, unlike kfpara (don't remember why yet)
      DDCADT(K)=(DCA(K)-DCA0(K))/TIMEC
      IF(DQLDT(K) .LE. 1.0E-17 &
            .OR. RH0(K) .GT. RHCRIT) DDCADT(K)=0.0
      AREA=AMAX1((CLDAREAB(I,K,J)+DDCADT(K)*DT),0.0)
      IF(AREA .LE. 1.0E-17  &
         .OR. (DQLDT(K) .LE. 1.0E-17) &
         .OR. RH0(K) .GT. RHCRIT) THEN
        DQCDCDT(K)=0.0
      ELSE
        DQCDCDT(K)=(DQLDT(K)-CLDLIQB(I,K,J)*DDCADT(K))/AREA
      ENDIF
#ifdef DENG_SHCU1D
 
      CLDATEN0(K)=DDCADT(K)
      CLDLCTN0(K)=DQCDCDT(K)
      tt4(k) = RLIQ(K)+RICE(K)
#endif
    ENDDO loop320

!
! CALCULATE THE CONVECTIVE RAINFALL
!
        RAINSHV(I,J)=.1*.5*DT2*PPTFLX/DXSQ

    IF ( wrf_dm_on_monitor()) THEN
      CALL get_wrf_debug_level( dbg_level )
      IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
        WRITE(message,909) RAINSHV(I,J)*NIC
 909    FORMAT(' CONVECTIVE RAINFALL =',F8.4,' CM')
        CALL wrf_message(message)
      ENDIF
    ENDIF
!
!...FEED BACK CONVECTIVE TENDENCIES TO RESOLVABLE SCALE EXCEPT THAT QC IS NOT
!   FED BACK TO THE GRID SCALE WHEN THE GRID IS SUB-SATURATED.  INSTEAD, IT
!   IS DETRAINED TO FORM NBC.  DCATEN(I,K): NBC AREA TENDENCY.
!   QCDCTEN(I,K): NBC LIQUID/ICE WATER CONTENT.
!
    DO K=1,KX
      TTEN(K)=TTEN(K)+ DTDT(K)
      QVTEN(K)=QVTEN(K)+ DQDT(K)
      QRTEN(K)=QRTEN(K)+DQRDT(K)
      IF(RH0(K) .GT. RHCRIT) THEN
        QCTEN(K)=QCTEN(K)+DQLDT(K)
      ELSE
        DCATEN(K)=DCATEN(K)+DDCADT(K)
        QCDCTEN(K)=QCDCTEN(K)+DQCDCDT(K)
      ENDIF
    ENDDO

!
!      raincn(i,j) is in cm.  We can convert it to KG/m^2 in following way:
!
!      1cm=1cm*1g/cm^3=1g/cm^2=10^-3Kg/10^-4m^2=10Kg/m^2
!
         RAINSH(I,J) = RAINSH(I,J)+RAINSHV(I,J)

#ifdef DENG_SHCU1D
 
      PLET = P0(LET)
      TLET = TU(LET)
      PLFSG(I,J) = P0(LFS)
      TLFSG(I,J) = TU(LFS)
      PLDTG(I,J) = P0(LDT)
      TLDTG(I,J) = TLCL*(PLDTG(I,J)/PLCL)**ROVCP
      PLDBG(I,J) = P0(LDB)
      TLDBG(I,J) = TLCL*(PLDBG(I,J)/PLCL)**ROVCP
      PLLCG(I,J) = P0(LLC)
      TLLCG(I,J) = TU(LLC)
 
      nca_ctr2(i,j)=nca_ctr2(i,j)+1

#endif

 425  CONTINUE 

#ifdef DENG_SHCU1D
      nca_ctr4(i,j)=nca_ctr4(i,j)+1
      PPBLG(I,J)=PPBL
      TPBLG(I,J)=TPBL
      PLCLG(I,J)=PLCL
      TLCLG(I,J)=TLCL

      IF( i == 1 .AND. j == 1 ) THEN
        write(32,*) xtime/60.,ZLCL
        write(33,*) xtime/60.,CLDDPTH+zlcl
        write(34,*) xtime/60.0,zlfc
!       CALL MAXIM(wutemp,KX,KLCL+1,KX,KVAL1)
!       CALL MINIM(wsubsub,KX,KLCL+1,KX,KVAL2)
!       write(82,*) xtime/60.0,wutemp(kval1),wsubsub(kval2)
        write(63,*) xtime/60.,CLDBMFLX(I,J)
        write(98,*) xtime/60.,tt6(KCBASE),tt7(KCBASE)
      ENDIF
#endif
!
!  FIND NBC TOP (KDCT), NBC BASE (KDCB), LOCATION OF MAXIMUM NBC AREA
!  (KAMAX) AND MAXIMUM CLD LIQUID WATER CONTENT (KLCMAX), AND THE
!  LOCATION 1000M AWAY FROM KAMAX (K1000)
!
      KDCB=1
      KDCT=1
      K1000=KDCT
      loop429: DO K=KL-1,2,-1
        IF(CLDAREAB(I,K,J) .GT. 0.001 .OR. RH0(K) .GT. RHCRIT) THEN
          KDCT=K
          EXIT loop429
        ENDIF
      ENDDO loop429

      loop441: DO K=2,KL-1  ! UPWARD
        IF(CLDAREAB(I,K,J) .GT. 0.001 .OR. RH0(K) .GT. RHCRIT ) THEN
          KDCB=K
          EXIT loop441
        ENDIF
      ENDDO loop441

#ifdef DENG_SHCU1D
      DCLDTOP(I,J)=Z0(KDCT)
      DCLDBASE(I,J)=Z0(KDCB)
#endif
      KDCLDTOP(I,J)=KDCT
      KDCLDBAS(I,J)=KDCB

      IF(AINC .NE. 999.0) THEN
         NUPDRAFT=AINC
      ELSE
         NUPDRAFT=0
      ENDIF

#ifdef DENG_SHCU1D
    IF( i == 1 .AND. j == 1 ) THEN
      write(93,*) xtime/60.,DCLDTOP(I,J)
      write(94,*) xtime/60.,DCLDBASE(I,J)
      IF(AINC .NE. 999.0) THEN
        write(19,*) xtime/60.,RAD
        write(25,*) xtime/60.,NUPDRAFT
      ELSE
        VAL=0.0
        write(19,*) xtime/60.,VAL
        write(25,*) xtime/60.,VAL
      ENDIF
    ENDIF
#endif

      IF(KDCT .LE. 1) THEN 
#ifdef DENG_SHCU1D
        nca_ctr3(i,j)=nca_ctr3(i,j)+1
#endif
        GOTO 345   ! END OF DISSIPATION CALCULATION
      ENDIF

      KAMAX=KDCB
      KLCMAX=KDCB
      DO K=KDCB,KDCT
        IF( CLDAREAB(I,K,J) .GT. CLDAREAB(I,KAMAX,J) ) KAMAX = K
        IF( CLDLIQB(I,K,J) .GT. CLDLIQB(I,KLCMAX,J) ) KLCMAX = K
      ENDDO

      loop430: DO K=KAMAX,1,-1
        IF(Z0(KAMAX)-Z0(K) .GE. 1000.0) THEN
          K1000 = K
          EXIT loop430
        ENDIF
      ENDDO loop430

      K1000=MAX(K1000,KDCB)
!
!  WHEN THE GRID IS SATURATED, CONVERT NBC TO RESOLVABLE-SCALE CLOUD
!
    DO K=1,KL
      IF(RH0(K) .GT. RHCRIT) THEN
        CLQ=AMAX1(CLDLIQB(I,K,J)+QCDCTEN(K)*DT,0.0)
        AREA=AMAX1(CLDAREAB(I,K,J)+DCATEN(K)*DT,0.0)
        QCDCTEN(K)=QCDCTEN(K)-CLQ/DT
        DCATEN(K)=DCATEN(K)-AREA/DT
        QCTEN(K)=QCTEN(K)+AREA*CLQ/DT
      ENDIF
    ENDDO
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!     BEGINING OF DISSIPATION CACULATION
!   goto 345
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!  EVAPORATION OF NBC AT THE CLOUD EDGE *******************************
!
#ifdef DENG_SHCU1D
   IF ( wrf_dm_on_monitor()) THEN
     CALL get_wrf_debug_level( dbg_level )
     IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
       if( mod(ktau,KTAU1HUR) .eq. 0 ) then
          write(*, *) '             ------- a evaporation'
       endif
      ENDIF
   ENDIF
#endif

      DIFFK=1.0E-5
    loop332: DO K=1,KX
      RLC=AMAX1(CLDLIQB(I,K,J)+QCDCTEN(K)*DT,0.0)
      VAL=QES(K)-Q0(K)
      VAL=AMAX1(0.0,VAL)
      E2=(0.01+CLDAREAB(I,K,J))*DIFFK*VAL*SQRT(NUPDRAFT)
      IF(RLC .LE. 1.0E-17) CYCLE loop332
      EOVLC=AMIN1(E2/RLC,CLDAREAB(I,K,J)/DT)
      DCATEN(K)=DCATEN(K)-EOVLC
#ifdef DENG_SHCU1D
      cldaten(k)=-EOVLC
#endif
      QVTEN(K)=QVTEN(K)+EOVLC*RLC
      IF(T0(K) .LE. TO) THEN
        RL=XLS
      ELSE
        RL=XLV0-XLV1*T0(K)
      ENDIF
      RCP=CP*(1.+0.887*Q0(K))
      RLOVCP=RL/RCP
      TTEN(K)=TTEN(K)-EOVLC*RLC*RLOVCP
    ENDDO loop332
!
!  VERTICAL DIFFUSION OF NBC LIQUID/ICE WATER CONTENT **********************
!
!  CALCULAT KH (VERTICAL DIFF COEF ) AND KR ( RADIATIVE INDUCED VERTICAL
!  DIFF COEF).  KR IS CALCULATED AT KAMAX+1 (FULL LAYER AT CLOUD TOP), THEN
!  CONSTANT UNTIL CLOUD TOP.  IT LINEARLY DROPS TO ZERO AT K1000, WHICH IS
!  THE LARGER ONE OF KDCB AND K1000.
!
    DO K=KDCB,KDCT+1    !  UPWARD
      KV(K)=KTH0(K)
    ENDDO

      DO K=1,KL
        tlong(k)=TEN_RADL0(K) 
        tshort(k)=TEN_RADS0(K)
      ENDDO
      K=KAMAX
      KP1=K+1
      CALL MINIM(tlong, KX,KDCB,KDCT,KRADLMAX)
      CALL MAXIM(tshort,KX,KDCB,KDCT,KRADSMAX)

      KR(KP1)=BBLS0(K)*BBLS0(K)/(T0(K)*(P00/P0(K))**ROCPQ) &
           *(ABS(TEN_RADL0(KRADLMAX))*DZQ(K)/15.0  &
               +ABS(TEN_RADS0(KRADSMAX))*DZQ(K)/50.0)

      DO K=K1000,KAMAX+1
        IF(KDCT+1 .NE. K1000) KR(K)=KR(KAMAX+1)*(FLOAT(K-K1000)  &
            /FLOAT(KAMAX+1-K1000))
      ENDDO
      DO K=KAMAX+1,KDCT+1
        KR(K)=KR(KAMAX+1)
      ENDDO
!
!  CALCULATE IN-CLOUD DIFF FLUXES ASSOCIATED WITH KV and KR.  MAKE SURE 
!  THAT UPWARD FLUXES AT CLOUD TOP(KDCT+1) VANISH, SO THAT THERE IS NO
!  LIQUID DIFFUSE UPWARD
!
      DO K=1,KDCB-1
        LFLUX(K)=0.0
        LFLUX1(K)=0.0
        LFLUX2(K)=0.0
      ENDDO

      DO K=KDCT+1,KXP1
        LFLUX(K)=0.0
        LFLUX1(K)=0.0
        LFLUX2(K)=0.0
      ENDDO

    DO K=KDCT,KDCB,-1
      IF(K .EQ. KDCB) THEN
        C1=15.0
      ELSE
        C1=DZA(K-1)
      ENDIF
      AREA1=AMAX1(CLDAREAB(I,K,J)+DCATEN(K)*DT,0.0)
      AREA2=AMAX1(CLDAREAB(I,K-1,J)+DCATEN(K-1)*DT,0.0)
      CLQ1=AMAX1(CLDLIQB(I,K,J)+QCDCTEN(K)*DT,0.0)
      CLQ2=AMAX1(CLDLIQB(I,K-1,J)+QCDCTEN(K-1)*DT,0.0)
      LFLUX1(K)=-RHOE(K)*KV(K)*(AREA1*CLQ1-AREA2*CLQ2)/C1
      LFLUX2(K)=RHOE(K)*KR(K)*(AREA1*CLQ1-AREA2*CLQ2)/C1
      LFLUX1(K)=AMIN1(LFLUX1(K),0.0)
      LFLUX2(K)=AMIN1(LFLUX2(K),0.0)
      LFLUX(K)=LFLUX1(K)+LFLUX2(K)
! 0.05 IS USED HERE TO
! MAKE SURE THAT NOT MORE THAN 5% OF THE TOTAL LIQUID MASS AT THIS LAYER
! IS REMOVED BY IN-CLOUD MIXING IN A SINGLE TIME STEP.
      C1=LFLUX(K+1)-CLQ1*DP(K)/(G*DT)*0.05
      LFLUX(K)=AMAX1(LFLUX(K),C1)
    ENDDO
    DO K=KDCT+1,KDCB,-1
      IF(RH0(K) .GT. RHCRIT .OR.   &
          (CLDAREAB(I,K,J)+DCATEN(K)*DT) .LE. 1.0e-6) then
         LFLUX(K)=0.0
         LFLUX(K+1)=0.0
       ENDIF
    ENDDO
!
!  CALCULATE DIFF TENDENCY ASSOCIATED WITH IN-CLOUD FLUXES CALCULATED ABOVE.
!  FLUXES AT CLD BASE (KDCB) CARRY LIQUID TO THE LAYER BELOW THE CLD BASE
!  AND EVAPORATE IN THAT LAYER
! 
#ifdef DENG_SHCU1D
   IF ( wrf_dm_on_monitor()) THEN
     CALL get_wrf_debug_level( dbg_level )
     IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
       if( mod(ktau,KTAU1HUR) .eq. 0 ) then
         write(*, *) '             --- lc in-cloud mixing'
       endif
     ENDIF
   ENDIF
#endif
!
!     THIS IS TO ENSURE THAT THE LAYER BELOW THE CLOUD BASE (KDCB-1) DOES
!     NOT REACH SATURATION ONLY BECAUSE OF MIXING (USE 95% HERE).  ADJUST
!     LFLUX(KDCB) SO THAT THE AMOUNT OF MASS TRANPORTED TO THIS LAYER IS
!     LESS THAN THE AMOUNT OF MASS NEEDED TO RAISE THE RH TO 95%.
!
      RHCR=0.95
      IF(Q0(KDCB-1)/QES(KDCB-1) .GT. RHCR) THEN
        LFLUX(KDCB)=0.0
      ELSE
        VAL1=-G*(LFLUX(KDCB)-LFLUX(KDCB-1))/DP(KDCB-1)
        VAL2=(RHCR*QES(KDCB-1)-Q0(KDCB-1))/DT
        VAL=AMIN1(VAL1,VAL2)
        LFLUX(KDCB)=-VAL*DP(KDCB-1)/G
      ENDIF

    loop436: DO K = KDCT, KDCB, -1
      AREA=AMAX1(CLDAREAB(I,K,J)+DCATEN(K)*DT,0.0)
      IF( RH0(K) .GT. RHCRIT .OR. AREA .LE. 1.0e-6 ) CYCLE loop436
      QCDCTEN(K)=QCDCTEN(K)-G*(LFLUX(K+1)-LFLUX(K))/DP(K)/AREA
#ifdef DENG_SHCU1D
      cldlctn1(k)=-G*(LFLUX(K+1)-LFLUX(K))/DP(K)/AREA
#endif
    ENDDO loop436

      K=KDCB-1
      IF(T0(K) .LE. TO) THEN
        RL=XLS
      ELSE
        RL=XLV0-XLV1*T0(K)
      ENDIF
      RCP=CP*(1.+0.887*Q0(K))
      RLOVCP=RL/RCP
      QVTEN(K)=QVTEN(K)-G*(LFLUX(K+1)-LFLUX(K))/DP(K)
      TTEN(K)=TTEN(K)+G*(LFLUX(K+1)-LFLUX(K))/DP(K)*RLOVCP
!
!  CALCULATE DIFF TENDENCY ASSOCIATED WITH DOWNWARD FLUXES AT
!  EXPOSED CLD BASE IF CLD AREA INCREASE WITH HEIGHT.  THIS FLUXE WILL
!  CAUSE LIQUID DECREASE IN THE CLD LAYER AND WATER VAPOR INCREASE IN THE
!  LAYER BELOW THE EXPOSED CLD BASE DUE TO EVAPORATION
!
#ifdef DENG_SHCU1D
   IF ( wrf_dm_on_monitor()) THEN
     CALL get_wrf_debug_level( dbg_level )
     IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
        if(mod(ktau,KTAU1HUR) .eq. 0) then
           write(*, *) '             ----lc exp-cld-base mixing'
        endif
      ENDIF
    ENDIF
#endif

    loop440: DO K=KDCT,KDCB+1,-1
      AREADIFF=CLDAREAB(I,K,J)-CLDAREAB(I,K-1,J)
      AREADIFF=AMAX1(AREADIFF,0.0)
      CLQ=AMAX1(CLDLIQB(I,K,J)+QCDCTEN(K)*DT,0.0)
      C1=-CLQ*DP(K)/(G*DT)*0.05      ! limit of flux that remove 5% of
                                     ! all the liquid in the layer above
      FX1=-RHOE(K)*KV(K)*CLDLIQB(I,K,J)/15.*AREADIFF
      FX2=-RHOE(K)*KR(K)*CLDLIQB(I,K,J)/15.*AREADIFF
      FX=FX1+FX2
      FX=AMAX1(FX,C1)

      IF((CLDAREAB(I,K,J)+DCATEN(K)*DT) .LE. 1.0E-6) CYCLE loop440

      IF(RH0(K) .GT. RHCRIT) CYCLE loop440
!
!     THIS IS TO ENSURE THAT THE LAYER BELOW THE EXPOSED CLOUD BASE (K-1) DOES
!     NOT REACH SATURATION ONLY BECAUSE OF MIXING (USE 95% HERE).  ADJUST
!     LFLUX(KDCB) SO THAT THE AMOUNT OF MASS TRANPORTED TO THIS LAYER IS
!     LESS THAN THE AMOUNT OF MASS NEEDED TO RAISE THE RH TO 95%.
!
      RHCR=0.95
      IF(Q0(K-1)/QES(K-1) .GT. RHCR) THEN
        FX=0.0
      ELSE
        VAL1=-G*FX/DP(K-1)
        VAL2=(RHCR*QES(K-1)-Q0(K-1))/DT
        VAL=AMIN1(VAL1,VAL2)
        FX=-VAL*DP(K-1)/G
      ENDIF

      AREA=AMAX1(CLDAREAB(I,K,J)+DCATEN(K)*DT,0.0)
      IF(AREA .LE. 1.0E-6) CYCLE loop440          ! this line was not in MM5
      QCDCTEN(K)=QCDCTEN(K)+G*FX/DP(K)/AREA
#ifdef DENG_SHCU1D
      cldlctn2(k)=G*FX/DP(K)/AREA
#endif
      IF(T0(K-1) .LE. TO) THEN
        RL=XLS
      ELSE
        RL=XLV0-XLV1*T0(K-1)
      ENDIF
      RCP=CP*(1.+0.887*Q0(K-1))
      RLOVCP=RL/RCP
      QVTEN(K-1)=QVTEN(K-1)-G*FX/DP(K-1)
      TTEN(K-1)=TTEN(K-1)+G*FX/DP(K-1)*RLOVCP
    ENDDO loop440
!
!  CLOUD TOP ENTRAINMENT INSTABILITY (CTEI) ********************************
!  RANDALL (JAS 1980), DEARDOFF (JAS 1980) AND DEL GENIO (J. CLIMATE 1996)
!
      RATIO=0.0
      RATIOMIN=0.0
      RATIOMAX=0.0
      IF(RH0(KDCT) .GT. RHCRIT) GOTO 340
      EFOLD=1.0E-4
      EX=1.0
      DEPTHMAX=100.0
      IF(KDCT .LE. 0) GOTO 340
      IF(T0(KDCT) .GE. TO) THEN
        HIN=CP*T0(KDCT)+G*Z0(KDCT)+XLV*QES(KDCT)
      ELSE
        HIN=CP*T0(KDCT)+G*Z0(KDCT)+XLS*QES(KDCT)
      ENDIF
      IF(T0(KDCT+1) .GE. TO) THEN
        HOUT=CP*T0(KDCT+1)+G*Z0(KDCT+1)+XLV*Q0(KDCT+1)
      ELSE
        HOUT=CP*T0(KDCT+1)+G*Z0(KDCT+1)+XLS*Q0(KDCT+1)
      ENDIF
      DELTH=HIN-HOUT
      QIN=QES(KDCT)+CLDLIQB(I,KDCT,J)
      QOUT=Q0(KDCT+1)
      DELTQ=QIN-QOUT
      RATIO=DELTH/(XLV*DELTQ+1.E-8)
      RKAPA=CP*0.5*(T0(KDCT)+T0(KDCT+1))/XLV
      DELTA=0.608
      IF(T0(KDCT) .GT. TO) THEN
        DQSSDT=QES(KDCT)*SVP2*(SVPT0-SVP3)  &
              /((T0(KDCT)-SVP3)*(T0(KDCT)-SVP3))
        RL=XLV0-XLV1*T0(KDCT)
      ELSE
        DQSSDT=QES(KDCT)*6.15E3/(T0(KDCT)*T0(KDCT))
        RL=XLS
      ENDIF
      RCP=CP*(1.+0.887*Q0(KDCT))
      RLOVCP=RL/RCP

      GAMA=RLOVCP*DQSSDT
      BEITA=(1+(1+DELTA)*GAMA*RKAPA)/(1+GAMA)
      RATIOMIN=RKAPA/BEITA
      RATIOMAX=(1+GAMA)*(1+(1-DELTA)*RKAPA)  &
           /(2+(1+(1+DELTA)*RKAPA)*GAMA)

      IF(RATIO .LE. RATIOMIN) THEN
        SIGMMA=0.0
        GOTO 340
      ELSE IF(RATIO .GE. RATIOMAX) THEN
        SIGMMA=EFOLD
      ELSE
        SIGMMA=EFOLD*((RATIO-RATIOMIN)/(RATIOMAX-RATIOMIN))**EX
      ENDIF

#ifdef DENG_SHCU1D
   IF ( wrf_dm_on_monitor()) THEN
     CALL get_wrf_debug_level( dbg_level )
     IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
       if( mod(ktau,KTAU1HUR) .eq. 0 ) then
         write(* ,*) '             -------- lc CTEI '
       endif
     ENDIF
   ENDIF
#endif
!
!   LOCATE K WHERE ITS DISTANCE FROM KDCT IS 100M
!
    K100=KDCT
    loop335: DO L=KDCT-1,KDCB,-1
      IF((Z0(KDCT)+0.5*DZQ(KDCT))-(Z0(L)-0.5*DZQ(L)) .GE. DEPTHMAX) THEN
        K100=K100-1
        EXIT loop335
      ENDIF
      K100=L
    ENDDO loop335

    RHCR=0.95
    IF(RH0(KDCT+1) .GT. RHCR) GOTO 340

    loop344: DO L=KDCT,K100,-1
      IF(RH0(L) .GT. RHCRIT) CYCLE loop344
      WFUN=FLOAT(L-K100+1)/FLOAT(KDCT-K100+1)
      AREA=AMAX1(CLDAREAB(I,L,J)+DCATEN(L)*DT,0.0)
      IF(T0(L) .LE. TO) THEN
        RL=XLS
      ELSE
        RL=XLV0-XLV1*T0(L)
      ENDIF
      RCP=CP*(1.+0.887*Q0(L))
      RLOVCP=RL/RCP

      VAL1=SIGMMA*CLDLIQB(I,L,J)*WFUN
      QCDCTEN(L)=QCDCTEN(L)-VAL1
#ifdef DENG_SHCU1D
      cldlctn5(L)=-VAL1
#endif
      QVTEN(KDCT+1)=QVTEN(KDCT+1)   &
                      +VAL1*AREA*DP(L)/DP(KDCT+1)
      TTEN(L)=TTEN(L)-VAL1*AREA*RLOVCP
      VAL=Q0(KDCT+1) + QVTEN(KDCT+1)*DT
!
!     ENSURING THAT THE LAYER ABOVE CLOUD TOP DOES NOT REACH SATURATION (RH=95%)
!     ONLY BECAUSE OF CTEI.  IF IT HAPPENS DUE TO CTEI THEN REMOVE THIS EFFECT
!
      IF( VAL/QES(KDCT+1) .GT. RHCR ) THEN
        QVTEN(KDCT+1)=QVTEN(KDCT+1)   &
            -VAL1*DP(L)/DP(KDCT+1)
        TTEN(L)=TTEN(L)+VAL1*AREA*RLOVCP
      ENDIF
    ENDDO loop344

 340  CONTINUE
!
!  PRECIPITATION (DRIZZLE) PROCESS ****************************************
!
#ifdef DENG_SHCU1D
   IF ( wrf_dm_on_monitor()) THEN
     CALL get_wrf_debug_level( dbg_level )
     IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
       if( mod(ktau,KTAU1HUR) .eq. 0 ) then
         write( *,*) '             ------ lc drizzle'
       endif
     ENDIF
   ENDIF
#endif

      DO K=1,KL
        PRC(K)=0.
        PRA(K)=0.
      ENDDO

      BVTS3=3.+BVTS
      PPI=1./(PIE*N0R)
      G3PBS=GAMMA(3.+BVTS)
      PRACS=PIE*N0S*AVTS*G3PBS*.25*ESI  ! from MM5

    DO K=1,KX
      TOUT=T0(K)
      SC2=AMAX1(0.0,CLDLIQB(I,K,J))
      SC3=AMAX1(0.0,QR0(K))
      IF(TOUT.GT.TO)THEN
        SC7=(RHOE(K)*SC3*PPI/1000.)**0.25
      ELSE
        RHOS=0.1
        PPIS=1./(PIE*N0S*RHOS) 
        SC7=(RHOE(K)*SC3*PPIS/1000.)**0.25
      ENDIF
      IF(TOUT .LT. TO)THEN
        XNC=XN0*EXP(0.6*(TO-TOUT))/RHOE(K) !   XN0 = 1.E-2 in MM5 domain/initial/param.F
        XNC=AMAX1(XNC,10000./RHOE(K))
!...    AUTOCONVERSION OF CLOUD ICE TO SNOW:
        PRC(K)=AMAX1(0.,(SC2-XMMAX*XNC)/DT)
!...    ACCRETION OF CLOUD ICE BY SNOW:
        IF(SC3 .LT. 1.0E-17)THEN
          PRA(K)=0.
        ELSE
          PRA(K)=PRACS*SC7**BVTS3*SC2
        ENDIF
      ELSE
!---    AUTOCONVERSION OF CLOUD WATER TO RAINWATER:
        PRC(K)=AMAX1(0.,QCK1*(SC2-QCTH))
!---    ACCRETION OF CLOUD WATER BY RAINWATER:
        BVT3=3.+BVT
        IF (SC3 .LT. 1.0E-17) THEN
           PRA(K)=0.
        ELSE
           G3PB=GAMMA(3.+BVT)
           PRAC=PIE*N0R*AVT*G3PB*0.25
           PRA(K)=PRAC*SC7**BVT3*SC2
        ENDIF
      ENDIF

      VAL=PRC(K)+PRA(K)
      VAL=MIN(VAL,CLDLIQB(I,K,J)/DT)
      AREA=AMAX1(CLDAREAB(I,K,J)+DCATEN(K)*DT,0.0)
      IF(AREA .LT. 1.0E-17) VAL=0.0
      QRTEN(K)=QRTEN(K)+VAL*AREA
      QCDCTEN(K)=QCDCTEN(K)-VAL
#ifdef DENG_SHCU1D
      cldlctn3(k)=-VAL
#endif

    ENDDO
!
!  ICE SEDIMENTATION AS A SINK TERM TO CLDLIQ ******************************
!
#ifdef DENG_SHCU1D
   IF ( wrf_dm_on_monitor()) THEN
     CALL get_wrf_debug_level( dbg_level )
     IF( dbg_level >= 10 .AND. i == i0 .AND. j == j0 ) THEN
      if( mod(ktau,KTAU1HUR) .eq. 0 ) then
         write( *,*) '             ----- lc ice settling'
         write(18,*) '             ----- lc ice settling'
       endif
     ENDIF
   ENDIF
#endif

    DO K=1,KL
      CLQ=AMAX1(CLDLIQB(I,K,J)+QCDCTEN(K)*DT,0.0)
      AREA=AMAX1(CLDAREAB(I,K,J)+DCATEN(K)*DT,0.0)
      SCR3(K)=CLQ*AREA
    ENDDO

    NSTEP=1
    DO K=1,KL
!
!  NOTE: RHO2=RHO/(PSB*1000)     WHERE RHO IS THE AIR DENSITY IN KG/M^3
!  NSTEP IS THE # OF TIME STEPS NEEDED FOR A DROPLET TO MOVE FROM THE LAYER-TOP
!  TO THE LAYER-BOTTOM
!
      RHO2=P0(K)/(R*( T0(K)+DT*TTEN(K)))                     
      IF( (T0(K)+DT*TTEN(K)) .GT.TO)THEN                                       
        VT2C=0.                                                     
      ELSE
!
! NOTE: SEDIMENTATION FORMULA OF HEYMSFIELD AND DONNER (1990, JAS)
!
        VT2C=3.29*(RHO2* SCR3(K)  )**0.16       ! m/s
      ENDIF

      RGVC(K)=G*RHO2*VT2C                     ! rho*g*vt2c/(1000*ps)
      NSTEP=MAX0(IFIX(RGVC(K)*DT/DSIGMA(K)+1.),NSTEP)   ! dvt2c*dt/DZ
    ENDDO

    DO N=1,NSTEP
      IF(N.GT.1000)STOP  &
         'IN SUB. SHALLOW (ICE SETTLING), NSTEP TOO LARGE, PROBABLY NAN'

      DO K=1,KL
        FALOUTC(K)=RGVC(K)*SCR3(K)
      ENDDO

     loop341: DO K=KL-1,2,-1
      AREA=AMAX1(CLDAREAB(I,K,J)+DCATEN(K)*DT,0.0)
      CLQ=AMAX1(CLDLIQB(I,K,J)+QCDCTEN(K)*DT,0.0)
      IF(RH0(K) .GT. RHCRIT) CYCLE loop341
      IF(AREA .LE. 1.E-4) CYCLE loop341
      FALTNDC=(FALOUTC(K)-FALOUTC(K+1))/DSIGMA(K)
      C1=AMIN1(FALTNDC,AREA*CLQ/DT*NSTEP)
      C1=AMAX1(0.0,C1)
      QCDCTEN(K)=QCDCTEN(K)-C1/AREA/NSTEP
#ifdef DENG_SHCU1D
      cldlctn4(k) = cldlctn4(k)-C1/AREA/NSTEP !  1-D graphics
#endif
      SCR3(K)=SCR3(K)-C1*DT/NSTEP/AREA
      QCTEN(K-1)=QCTEN(K-1)+C1*DSIGMA(K)/DSIGMA(K-1)/NSTEP
!         if(i.eq.82.and.j.eq.74) then
!         endif
      RGVC(K)=AMAX1(RGVC(K)/DSIGMA(K),RGVC(K+1)/DSIGMA(K+1))*DSIGMA(K)
     ENDDO loop341

    ENDDO
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 345  CONTINUE      ! END OF DISSIPATION CACULATION
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! PUT INTO TAU-1 ARRAYS
!
      CLDDPTHB(I,J)=CLDDPTH
      CLDTOPB(I,J)=CLDTOP
      LTOPB(I,J)=LTOP
      DO K=1,KL
        WUB(I,K,J)=WU(K)
      ENDDO

#ifdef DENG_SHCU1D

    IF( i == 1 .AND. j == 1 ) THEN
 
      DO NK = 1,KX
        EE=Q0(NK)*P0(NK)/(0.622+Q0(NK))
        TLOG=LOG(EE/SVP1/1000.)
        TDPT=(SVP2*SVPT0-SVP3*TLOG)/(SVP2-TLOG)
        TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T0(NK)-T00))* &
           (T0(NK)-TDPT)
        THTA=T0(NK)*(1.E5/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))
        THETEE(NK)=THTA*   &
           EXP((3374.6525/TSAT-2.5403)*Q0(NK)*(1.+0.81*Q0(NK)))
        thtetmp(nk)=THETEE(NK)
        thtatmp(nk)=THTA
      ENDDO

      if(mod(ktau,KTAU1HUR) .eq. 0) then
        write(*,878) xtime/60.0, zpbl, Zlcl,Zlfc,CLDDPTHB(I,J)
        write(18,878) Zlcl,Zlfc,CLDDPTH
 878     format(5x,'xtime=',f6.1,' Zpbl=',f5.1,' Zlcl=',f6.1,1x,'Zlfc=',f8.1,  &
        1x,'CLD_Depth=',f7.1)
      endif

      KK=INT(PLOTFRQ*(KTAU1HUR/60.))
      IF( MOD(KTAU-1,KK) .EQ. 0) THEN
      write(17,*) xtime/60.0,ktau,ZPBL,ZLCL,LLC,KPBL,KLCL,  &
                     CLDTOP,KX-LTOP+1,KX-KDCB+1,KX-KDCT+1,RHCRIT
!     write(43,*) xtime/60.0,ktau,ZPBL,ZLCL,LLC,KPBL,KLCL,  &
!                    CLDTOP,LTOP,KDCB,KDCT
      DO k = KL, 1, -1
         write(17,*) kl-k+1,p0(k)/100.0,z0(k),CLDUA(k)*100.0, &
         CLDAREAB(I,K,J)*100.0,                     &
          CLDLIQB(I,K,J)*1000.0,                    &
         CLDAREAB(I,K,J)*CLDLIQB(I,K,J)*1000.0,         &
         tt4(k)*1000.0,                         &
         RH0(K)*100.0,                           &
         ca_rad0(K)*100.0,                        &
         QC0_expl(K)*1000.
      ENDDO

!     DO k = 1, KL
!       write(43,*) k,p0(k),z0(k),    &
!          cldaten0(k)*aa,                    &
!          cldaten(k)*aa,                     &
!          cldlctn0(k)*bb,                    &
!          cldlctn1(k)*bb,                    &
!          cldlctn2(k)*bb,                    &
!          cldlctn3(k)*bb,                    &
!          cldlctn4(k)*bb,                    &
!          cldlctn5(k)*bb      ! g/hour
!     ENDDO
    ENDIF


      KK=INT(PLOTFRQ*(KTAU1HUR/60.))
      IF( MOD(KTAU-1,KK) .EQ. 0) THEN

         write(10,*) xtime/60.0, ktau-1

         DO k = kl, 1, -1
           wsp_plot(k) = sqrt(u0(k)**2+v0(k)**2)
           if( ABS(wsp_plot(k)) <  0.001 ) then
             wdr_plot(k)=0.
           else if(v0(k) .gt. 0.) then
             wdr_plot(k)=atan(u0(k)/v0(k))
             wdr_plot(k)=wdr_plot(k)*180./pie+180.
           else if (v0(k) .lt. 0.) then
             if( u0(k) .gt. 0.0 ) then
             wdr_plot(k)=atan(u0(k)/v0(k))
             wdr_plot(k)=wdr_plot(k)*180./PIE+360.
             else if( u0(k) .lt. 0.0 ) then
             wdr_plot(k)=atan(u0(k)/v0(k))
             wdr_plot(k)=wdr_plot(k)*180./PIE
             end if
           else if (u0(k) .gt.0.) then
             wdr_plot(k)=270.
           else
             wdr_plot(k)=90.
           end if

           qqq= Q0(K)
           qqq = AMAX1(qqq,1.0E-25)
           EES=qqq*p0(k)/100.0/(0.622+qqq)
           IF( t0(k) .GT. TO ) THEN
             TLOG=LOG(EES/SVP1/10.)
             td_plot(k) = (SVP2*SVPT0-SVP3*TLOG)/(SVP2-TLOG)-SVPT0
           ELSE
             TLOG=LOG(EES/0.611/10.)
             td_plot(k) = 6150./(22.514-TLOG)-SVPT0
           ENDIF

           write(10,*) p0(k)/100.0, t0(k)-SVPT0, td_plot(k), wsp_plot(k), wdr_plot(k)
         ENDDO

         write(10,*) (TUG(K),K=1,KL),TPBLG(I,J),PPBLG(I,J),TLCLG(I,J),  &
         PLCLG(I,J), PCLDTPG(I,J), TCLDTPG(I,J),                         &
         CLDHGTG(I,J),PLET,TLET,PLFSG(I,J),TLFSG(I,J),PLDTG(I,J),        &
         TLDTG(I,J),PLDBG(I,J),TLDBG(I,J),PLLCG(I,J),TLLCG(I,J),         &
         TRPPT
      ENDIF

      KK=INT(PLOTFRQ*(KTAU1HUR/60.))
      IF( MOD(KTAU-1,KK) .EQ. 0) THEN

        write(7,*) xtime/60.0,ktau,ZPBL,ZLCL,LLC,KKPBL,KLCL, &
                       CLDTOP,kx-LTOP+1,kx-KDCB+1,kx-KDCT+1
        do k = kx, 1, -1
          write(7,*) kx-k+1, p0(k)/100.0, z_at_w0(k)-ht, tke0(k)
        enddo

        write(67,*) (TKE0(K),K=KXP1-KL/3+1,1,-1), (z_at_w0(k)-ht,k=KXP1-KL/3+1,1,-1)
       ENDIF

    ENDIF
#endif

      END SUBROUTINE deng_shcu

!====================================================================
   SUBROUTINE  deng_shcu_init(RTHSHTEN,RQVSHTEN,RQCSHTEN,RQRSHTEN,  &
                     RUSHTEN,RVSHTEN,RDCASHTEN,RQCDCSHTEN,W0AVG,    &
                     PBLHAVG, TKEAVG, cldareaa, cldareab,           &
                     cldliqa, cldliqb, ca_rad, cw_rad,        &
                     wub, pblmax, ltopb, clddpthb, cldtopb,         &
                     capesave, ainckfsa, radsave,                   &
                     rainsh, rainshvb, kdcldtop, kdcldbas,                  &
                     xtime1,                                        &
                     restart,                  &
                     SVP1,SVP2,SVP3,SVPT0,   &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte

   LOGICAL , INTENT(IN)           ::  restart
   REAL    , INTENT(IN)           ::  SVP1,SVP2,SVP3,SVPT0

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT) ::      &
                                                    RUSHTEN, &
                                                    RVSHTEN, &
                                                   RTHSHTEN, &
                                                   RQVSHTEN, &
                                                   RQCSHTEN, &
                                                   RQRSHTEN, &
                                                  RDCASHTEN, &
                                                 RQCDCSHTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG,  &
                                      TKEAVG

   REAL , DIMENSION( ims:ime , jms:jme), INTENT(OUT)           ::  pblmax, &
                    rainsh, rainshvb, clddpthb, cldtopb, capesave, radsave, xtime1, &
                    pblhavg

   REAL , DIMENSION( ims:ime , 1:100, jms:jme ), INTENT(OUT) :: ainckfsa

   INTEGER , DIMENSION( ims:ime , jms:jme), INTENT(OUT)      ::  ltopb &
                         ,kdcldtop, kdcldbas

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT(OUT) ::  wub, &
                     cldareaa, cldareab, cldliqa, cldliqb, ca_rad, cw_rad


   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

 IF(.NOT.RESTART)THEN
   DO j=jts,jtf
   DO i=its,itf
   DO k=kts,ktf
      RUSHTEN(i,k,j)    = 0.
      RVSHTEN(i,k,j)    = 0.
      RTHSHTEN(i,k,j)   = 0.
      RQVSHTEN(i,k,j)   = 0.
      RQCSHTEN(i,k,j)   = 0.
      RQRSHTEN(i,k,j)   = 0.
      RDCASHTEN(i,k,j)  = 0.
      RQCDCSHTEN(i,k,j) = 0.
      W0AVG(i,k,j)      = 0.
      TKEAVG(i,k,j)      = 0.
      wub(i,k,j)        = 0.
      cldareaa(i,k,j)   = 0.0
      cldareab(i,k,j)   = 0.0
      cldliqa(i,k,j)    = 0.0
      cldliqb(i,k,j)    = 0.0
      ca_rad(i,k,j)  = 0.0
      cw_rad(i,k,j)  = 0.0
   ENDDO
      PBLHAVG(i,j)   = 0.0
      pblmax(i,j)   = 0.0
      clddpthb(I,J) = 0.0
      cldtopb(I,J)  = 0.0
      rainsh(i,j)  = 0.0
      rainshvb(i,j)  = 0.0
      capesave(i,j)  = 0.0
      radsave(i,j)  = 0.0
      xtime1(i,j)  = 0.0
      ltopb(i,j)       = 1
      kdcldtop(i,j)    = 1
      kdcldbas(i,j)    = 1
   ENDDO
   ENDDO

   DO j=jts,jtf
   DO i=its,itf
   DO k=1,100
      ainckfsa(i,k,j) = 0.0
   ENDDO
   ENDDO
   ENDDO

 ENDIF

  CALL KF_LUTAB(SVP1,SVP2,SVP3,SVPT0)
! CALL KF_LUTABBG(SVP1,SVP2,SVP3,SVPT0)

  END SUBROUTINE deng_shcu_init

!==========================================================
  SUBROUTINE time_avg_2d( dt, ADAPT_STEP_FLAG, time_period_avg_min &
             ,array2d, array2d_avg                           &
             ,its,ite, jts,jte                               &
                                                                      )
  IMPLICIT NONE
  INTEGER,      INTENT(IN   ) :: its,ite, jts,jte

  REAL, INTENT(IN   )  :: time_period_avg_min, dt ! time window to perform 
                                                  !a running mean value
  LOGICAL ,INTENT(IN)  :: adapt_step_flag
  REAL,  DIMENSION( its:ite , jts:jte ), INTENT(IN   ) ::     array2d
  REAL,  DIMENSION( its:ite , jts:jte ), INTENT(INOUT) ::     array2d_avg

  INTEGER              :: i, j, ntst
  REAL                 :: tst, AVGfctr, fctr, den 

  NTST = NINT( time_period_avg_min * 60.0 / dt ) ! number of time steps in time_period_avg_min
  TST = FLOAT(NTST)
  IF( TST <= 0 ) STOP 'Denominator has to be greater than zero - deng_shcu_driver'

  if (ADAPT_STEP_FLAG) then
     AVGfctr = MAX( time_period_avg_min*60,dt ) - dt
     fctr = dt
     den = MAX( time_period_avg_min*60,dt )
  else
     AVGfctr = (TST-1.)
     fctr = 1.
     den = TST
  endif

  DO J = jts,jte
  DO I = its,ite
    array2d_avg(i,j) = ( array2d_avg(i,j) * AVGfctr + array2d(i,j) * fctr ) / den
  ENDDO
  ENDDO

! write(*,'(5f10.4)') array2d(1,1), array2d_avg(1,1), AVGfctr, fctr, den

  END SUBROUTINE time_avg_2d

!==========================================================
  SUBROUTINE time_avg_3d( dt, ADAPT_STEP_FLAG, time_period_avg_min &
             ,array3d, array3d_avg                           &
             ,its,ite, jts,jte, kts,kte                      &
                                                                      )
  IMPLICIT NONE
  INTEGER,      INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL, INTENT(IN   )  :: time_period_avg_min, dt ! time window to perform   
                                                  !a running mean value
  LOGICAL ,INTENT(IN)  :: adapt_step_flag
  REAL,  DIMENSION( its:ite , kts:kte , jts:jte ), INTENT(IN   ) :: array3d
  REAL,  DIMENSION( its:ite , kts:kte , jts:jte ), INTENT(INOUT) :: array3d_avg

  INTEGER              :: i, j, k, ntst
  REAL                 :: tst, AVGfctr, fctr, den 

  NTST = NINT( time_period_avg_min * 60.0 / dt ) ! number of time steps in time_period_avg_min
  TST = FLOAT(NTST)
  IF( TST <= 0 ) STOP 'Denominator has to be greater than zero - deng_shcu_driver'

  if (ADAPT_STEP_FLAG) then
     AVGfctr = MAX( time_period_avg_min*60,dt ) - dt
     fctr = dt
     den = MAX( time_period_avg_min*60,dt )
  else
     AVGfctr = (TST-1.)
     fctr = 1.
     den = TST
  endif

  DO J = jts,jte
  DO I = its,ite
  DO k = kts,kte
    array3d_avg(i,k,j) = ( array3d_avg(i,k,j) * AVGfctr + array3d(i,k,j) * fctr ) / den
  ENDDO
  ENDDO
  ENDDO

!  do k = 1,kte
!    write(*,'(i3,5f10.4)') k, array3d(1,k,1), array3d_avg(1,k,1), AVGfctr, fctr, den
!  enddo

  END SUBROUTINE time_avg_3d

!====================================================================
      SUBROUTINE CONDLOAD(QLIQ,QICE,WTW,DZ,BOTERM,ENTERM,RATE,QNEWLQ,           &
                          QNEWIC,QLQOUT,QICOUT,G)
!  9/18/88...THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME US
!  BY OGURA AND CHO (1973).  LIQUID WATER FALLOUT FROM A PARCEL IS CAL-
!  CULATED USING THE EQUATION DQ=-RATE*Q*DT, BUT TO SIMULATE A QUASI-
!  CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON VERTICAL
!  RESOLUTION THIS IS EXPRESSED AS Q=Q*EXP(-RATE*DZ).
!----------------------
      IMPLICIT NONE
!----------------------
      REAL, INTENT(IN   )   :: G
      REAL, INTENT(IN   )   :: DZ,BOTERM,ENTERM,RATE
      REAL, INTENT(INOUT)   :: QLQOUT,QICOUT,WTW,QLIQ,QICE,QNEWLQ,QNEWIC
      REAL :: QTOT,QNEW,QEST,G1,WAVG,CONV,RATIO3,OLDQ,RATIO4,DQ,PPTDRG

      IF(ABS(WTW).LT.1.E-4)  WTW=1.E-4
      QTOT=QLIQ+QICE
      QNEW=QNEWLQ+QNEWIC
!
!  ESTIMATE THE VERTICAL VELOCITY SO THAT AN AVERAGE VERTICAL VELOCITY
!  BE CALCULATED TO ESTIMATE THE TIME REQUIRED FOR ASCENT BETWEEN MODEL
!  LEVELS...
!
      QEST=0.5*(QTOT+QNEW)
      G1=WTW+BOTERM-ENTERM-2.*G*DZ*QEST/1.5
      IF(G1.LT.0.0)G1=0.
      WAVG=0.5*(SQRT(WTW)+SQRT(G1))
      CONV=RATE*DZ/WAVG
!
!  RATIO3 IS THE FRACTION OF LIQUID WATER IN FRESH CONDENSATE, RATIO4 IS
!  THE FRACTION OF LIQUID WATER IN THE TOTAL AMOUNT OF CONDENSATE INVOLV
!  IN THE PRECIPITATION PROCESS - NOTE THAT ONLY 60% OF THE FRESH CONDEN
!  SATE IS IS ALLOWED TO PARTICIPATE IN THE CONVERSION PROCESS...
!
      RATIO3=QNEWLQ/(QNEW+1.E-8)
!     OLDQ=QTOT
      QTOT=QTOT+0.6*QNEW
      OLDQ=QTOT
      RATIO4=(0.6*QNEWLQ+QLIQ)/(QTOT+1.E-8)
      QTOT=QTOT*EXP(-CONV)
!
!  DETERMINE THE AMOUNT OF PRECIPITATION THAT FALLS OUT OF THE UPDRAFT
!  PARCEL AT THIS LEVEL...
!
      DQ=OLDQ-QTOT
      QLQOUT=RATIO4*DQ
      QICOUT=(1.-RATIO4)*DQ
!
!  ESTIMATE THE MEAN LOAD OF CONDENSATE ON THE UPDRAFT IN THE LAYER, CAL
!  LATE VERTICAL VELOCITY
!
      PPTDRG=0.5*(OLDQ+QTOT-0.2*QNEW)
      WTW=WTW+BOTERM-ENTERM-2.*G*DZ*PPTDRG/1.5
      IF(ABS(WTW).LT.1.E-4)WTW=1.E-4
!
!  DETERMINE THE NEW LIQUID WATER AND ICE CONCENTRATIONS INCLUDING LOSSE
!  DUE TO PRECIPITATION AND GAINS FROM CONDENSATION...
!
      QLIQ=RATIO4*QTOT+RATIO3*0.4*QNEW
      QICE=(1.-RATIO4)*QTOT+(1.-RATIO3)*0.4*QNEW
      QNEWLQ=0.
      QNEWIC=0.

   END SUBROUTINE CONDLOAD
!====================================================================
      SUBROUTINE DTFRZNEW(TU,P,THTEU,QVAP,QLIQ,QICE,RATIO2, &
                     QNWFRZ,RL,FRC1,EFFQ,IFLAG,XLV0,XLV1,XLS0,XLS1,     &
!        ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,                       &
            SVP1,SVP2,SVPT0,SVP3)
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P, SVP1,SVP2,SVPT0,SVP3,   &
                                    XLV0,XLV1,XLS0,XLS1, EFFQ
                                    
   REAL,         INTENT(INOUT)   :: TU, THTEU, QVAP, QLIQ, QICE, RATIO2, &
                                    QNWFRZ, FRC1, RL
   INTEGER,      INTENT(INOUT)   :: IFLAG
   REAL                          :: A, B, C, C5, RLC, RLS, RLF, QVAP1, &
                                    RV, CS, QLQFRZ, QNEW, ESLIQ, ESICE, &
                                    CP, DQVAP, DTFRZ, TU1, ES, pi
!
!...ALLOW GLACIATION OF THE UPDRAFT TO OCCUR AS AN APPROXIMATELY LINEAR
!   FUNCTION OF TEMPERATURE IN THE TEMPERATURE RANGE TTFRZ TO TBFRZ...
!
      RV=461.5
      C5=1.0723E-3
!
!...ADJUST THE LIQUID WATER CONCENTRATIONS FROM FRESH CONDENSATE AND THA
!   BROUGHT UP FROM LOWER LEVELS TO AN AMOUNT THAT WOULD BE PRESENT IF N
!   LIQUID WATER HAD FROZEN THUS FAR...THIS IS NECESSARY BECAUSE THE   
!   EXPRESSION FOR TEMP CHANGE IS MULTIPLIED BY THE FRACTION EQUAL TO TH
!   PARCEL TEMP DECREASE SINCE THE LAST MODEL LEVEL DIVIDED BY THE TOTAL
!   GLACIATION INTERVAL, SO THAT EFFECTIVELY THIS APPROXIMATELY ALLOWS A
!   AMOUNT OF LIQUID WATER TO FREEZE WHICH IS EQUAL TO THIS SAME FRACTIO
!   OF THE LIQUID WATER THAT WAS PRESENT BEFORE THE GLACIATION PROCESS W
!   INITIATED...ALSO, TO ALLOW THETAU TO CONVERT APPROXIMATELY LINEARLY
!   ITS VALUE WITH RESPECT TO ICE, WE NEED TO ALLOW A PORTION OF THE FRE
!   CONDENSATE TO CONTRIBUTE TO THE GLACIATION PROCESS; THE FRACTIONAL 
!   AMOUNT THAT APPLIES TO THIS PORTION IS 1/2 OF THE FRACTIONAL AMOUNT
!   FROZEN OF THE "OLD" CONDENSATE BECAUSE THIS FRESH CONDENSATE IS ONLY
!   PRODUCED GRADUALLY OVER THE LAYER...NOTE THAT IN TERMS OF THE DYNAMI
!   OF THE PRECIPITATION PROCESS, IE. PRECIPITATION FALLOUT, THIS FRACTI
!   AMNT OF FRESH CONDENSATE HAS ALREADY BEEN INCLUDED IN THE ICE CATEGO
!
      QLQFRZ=QLIQ*EFFQ
      QNEW=QNWFRZ*EFFQ*0.5
!     ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
      ESLIQ=1.E3*SVP1*EXP(SVP2*(TU-SVPT0)/(TU-SVP3))
!     ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))
      ESICE=611.0*EXP(22.514-6.15E3/TU)
      RLC=2.5E6-2369.276*(TU-273.16)
      RLS=2833922.-259.532*(TU-273.16)
      RLF=RLS-RLC
      CP=1005.7*(1.+0.89*QVAP)
!
!  A = D(ES)/DT IS THAT CALCULATED FROM BUCK`S (1981) EMPIRICAL FORMULAS
!  FOR SATURATION VAPOR PRESSURE...
!
!     A=(CICE-BICE*DICE)/((TU-DICE)*(TU-DICE))
      A=6.15E+3/(TU*TU)
      B=RLS*0.622/P
      C=A*B*ESICE/CP
      DQVAP=B*(ESLIQ-ESICE)/(RLS+RLS*C)-RLF*(QLQFRZ+QNEW)/(RLS+RLS/C)
      DTFRZ=(RLF*(QLQFRZ+QNEW)+B*(ESLIQ-ESICE))/(CP+A*B*ESICE)
      TU1=TU
      QVAP1=QVAP
      TU=TU+FRC1*DTFRZ
      QVAP=QVAP-FRC1*DQVAP
      ES=QVAP*P/(0.622+QVAP)
!     ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
      ESLIQ=1.E3*SVP1*EXP(SVP2*(TU-SVPT0)/(TU-SVP3))
!     ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))
      ESICE=611.0*EXP(22.514-6.15E3/TU)
      RATIO2=(ESLIQ-ES)/(ESLIQ-ESICE)
!
!  TYPICALLY, RATIO2 IS VERY CLOSE TO (TTFRZ-TU)/(TTFRZ-TBFRZ), USUALLY
!  WITHIN 1% (USING TU BEFORE GALCIATION EFFECTS ARE APPLIED);  IF THE
!  INITIAL UPDRAFT TEMP IS BELOW TBFRZ AND RATIO2 IS STILL LESS THAN 1,
!  AN ADJUSTMENT TO FRC1 AND RATIO2 IS INTRODUCED SO THAT GLACIATION  
!  EFFECTS ARE NOT UNDERESTIMATED; CONVERSELY, IF RATIO2 IS GREATER THAN
!  FRC1 IS ADJUSTED SO THAT GLACIATION EFFECTS ARE NOT OVERESTIMATED...
!
      IF(IFLAG.GT.0.AND.RATIO2.LT.1)THEN
        FRC1=FRC1+(1.-RATIO2)
        TU=TU1+FRC1*DTFRZ
        QVAP=QVAP1-FRC1*DQVAP
        RATIO2=1.
        IFLAG=1
        GOTO 20
      ENDIF
      IF(RATIO2.GT.1.)THEN
        FRC1=FRC1-(RATIO2-1.)
        FRC1=AMAX1(0.0,FRC1)
        TU=TU1+FRC1*DTFRZ
        QVAP=QVAP1-FRC1*DQVAP
        RATIO2=1.
        IFLAG=1
      ENDIF
!
!  CALCULATE A HYBRID VALUE OF THETAU, ASSUMING THAT THE LATENT HEAT OF
!  VAPORIZATION/SUBLIMATION CAN BE ESTIMATED USING THE SAME WEIGHTING
!  FUNCTION AS THAT USED TO CALCULATE SATURATION VAPOR PRESSURE, CALCU-
!  LATE NEW LIQUID WATER AND ICE CONCENTRATIONS...
!
   20 RLC=XLV0-XLV1*TU
      RLS=XLS0-XLS1*TU
      RL=RATIO2*RLS+(1.-RATIO2)*RLC
      PI=(1.E5/P)**(0.2854*(1.-0.28*QVAP))
      THTEU=TU*PI*EXP(RL*QVAP*C5/TU*(1.+0.81*QVAP))
      IF(IFLAG.EQ.1)THEN
        QICE=QICE+FRC1*DQVAP+QLIQ
        QLIQ=0.
      ELSE
        QICE=QICE+FRC1*(DQVAP+QLQFRZ)
        QLIQ=QLIQ-FRC1*QLQFRZ
      ENDIF
      QNWFRZ=0.

      END SUBROUTINE DTFRZNEW
! ***********************************************************************
!====================================================================
!====================================================================
      SUBROUTINE ENVIRTHT(P1,T1,Q1,THT1,R1,RL,     &
!        ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,  &
         SVP1,SVP2,SVPT0,SVP3,I,J)
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P1,T1,Q1,R1,RL,SVP1,SVP2,SVPT0,SVP3
   INTEGER,      INTENT(IN   )   :: I,J
   REAL,         INTENT(  OUT)   :: THT1
   REAL    ::    EE,TLOG,TDPT,TSAT,THT,TFPT
   REAL    ::    T00,P00,C1,C2,C3,C4,C5,tlogic, tsatlq, tsatic
!-----------------------------------------------------------------------
      DATA T00,P00,C1,C2,C3,C4,C5/273.16,1.E5,3374.6525,2.5403,3114.834,  &
           0.278296,1.0723E-3/
!
!  CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE...
!
      IF(R1.LT.1.E-6)THEN
       ! EE=Q1*P1/(0.622+Q1)
        EE=max(Q1*P1/(0.622+Q1),1.e-9)
!       TLOG=ALOG(EE/ALIQ)
!       TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
        TLOG=ALOG(EE/SVP1/1000.)
        TDPT=(SVP2*SVPT0-SVP3*TLOG)/(SVP2-TLOG)
        TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT)
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))
        THT1=THT*EXP((C1/TSAT-C2)*Q1*(1.+0.81*Q1))
      ELSEIF(ABS(R1-1.).LT.1.E-6)THEN
       ! EE=Q1*P1/(0.622+Q1)
        EE=max(Q1*P1/(0.622+Q1),1.e-9)
!       TLOG=ALOG(EE/AICE)
!       TFPT=(CICE-DICE*TLOG)/(BICE-TLOG)
        TLOG=ALOG(EE/611.0)
        TFPT=6150./(22.514-TLOG)
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))
        TSAT=TFPT-(.182+1.13E-3*(TFPT-T00)-3.58E-4*(T1-T00))*(T1-TFPT)
        THT1=THT*EXP((C3/TSAT-C4)*Q1*(1.+0.81*Q1))
      ELSE
       ! EE=Q1*P1/(0.622+Q1)
        EE=max(Q1*P1/(0.622+Q1),1.e-9)
!       TLOG=ALOG(EE/ALIQ)
!       TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
!       TLOGIC=ALOG(EE/AICE)
!       TFPT=(CICE-DICE*TLOGIC)/(BICE-TLOGIC)
        TLOG=ALOG(EE/SVP1/1000.)
        TDPT=(SVP2*SVPT0-SVP3*TLOG)/(SVP2-TLOG)
        TLOGIC=ALOG(EE/611.0)
        TFPT=6150./(22.514-TLOGIC)
        THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))
        TSATLQ=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT)
        TSATIC=TFPT-(.182+1.13E-3*(TFPT-T00)-3.58E-4*(T1-T00))*(T1-TFPT)
        TSAT=R1*TSATIC+(1.-R1)*TSATLQ
        THT1=THT*EXP(RL*Q1*C5/TSAT*(1.+0.81*Q1))
                                                       !  NaN  326.  NaN  30.88963  NaN   0.0    NaN   -Infinity      -Infinity    0
      ENDIF

      END SUBROUTINE ENVIRTHT
!====================================================================
      SUBROUTINE ENVIRTHT2(P1,T1,Q1,THT1,ALIQ,BLIQ,CLIQ,DLIQ)
!
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P1,T1,Q1,ALIQ,BLIQ,CLIQ,DLIQ
   REAL,         INTENT(INOUT)   :: THT1
   REAL    ::    EE,TLOG,ASTRT,AINC,A1,TP,VALUE,AINTRP,TDPT,TSAT,THT,      &
                 T00,P00,C1,C2,C3,C4,C5
   INTEGER ::    INDLU
!-----------------------------------------------------------------------
      DATA T00,P00,C1,C2,C3,C4,C5/273.16,1.E5,3374.6525,2.5403,3114.834,   &
           0.278296,1.0723E-3/
!
!  CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE...
!
! NOTE: Calculations for mixed/ice phase no longer used...jsk 8/00
!        For example, KF90 Eq. 10 no longer used
!
      EE=Q1*P1/(0.622+Q1)
!     TLOG=ALOG(EE/ALIQ)
! ...calculate LOG term using lookup table...
!
      astrt=1.e-3
      ainc=0.075
      a1=ee/aliq
      tp=(a1-astrt)/ainc
      indlu=int(tp)+1
      value=(indlu-1)*ainc+astrt
      aintrp=(a1-value)/ainc
      tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)
!
      TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
      TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT)
      THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))
      THT1=THT*EXP((C1/TSAT-C2)*Q1*(1.+0.81*Q1))
!
      END SUBROUTINE ENVIRTHT2
! ***********************************************************************
!====================================================================
      SUBROUTINE PROF5(EQ,EE,UD)
!-----------------------------------------------------------------------
!  THIS SUBROUTINE INTEGRATES THE AREA UNDER THE CURVE IN THE GAUSSIAN
!  DISTRIBUTION...THE NUMERICAL APPROXIMATION TO THE INTEGRAL IS TAKEN FROM
!  `HANDBOOK OF MATHEMATICAL FUNCTIONS WITH FORMULAS, GRAPHS AND MATHEMATICAL
!  TABLES` ED. BY ABRAMOWITZ AND STEGUN, NAT`L BUREAU OF STANDARDS APPLIED
!  MATHEMATICS SERIES.  JUNE, 1964., MAY, 1968.
!                                     JACK KAIN
!                                     7/6/89
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: EQ
   REAL,         INTENT(  OUT)   :: EE,UD
   REAL ::       SQRT2P,A1,A2,A3,P,SIGMA,FE,X,Y,EY,E45,T1,T2,C1,C2
!-----------------------------------------------------------------------
!*********************************************************************
!*****    GAUSSIAN TYPE MIXING PROFILE....******************************
      DATA SQRT2P,A1,A2,A3,P,SIGMA,FE/2.506628,0.4361836,-0.1201676, &
      0.9372980,0.33267,0.166666667,0.202765151/
      X=(EQ-0.5)/SIGMA
      Y=6.*EQ-3.
      EY=EXP(Y*Y/(-2))
      E45=EXP(-4.5)
      T2=1./(1.+P*ABS(Y))
      T1=0.500498
      C1=A1*T1+A2*T1*T1+A3*T1*T1*T1
      C2=A1*T2+A2*T2*T2+A3*T2*T2*T2
      IF(Y.GE.0.)THEN
        EE=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*EQ*EQ/2.
        UD=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*(0.5+EQ*EQ/2.-EQ)
      ELSE
        EE=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*EQ*EQ/2.
        UD=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*(0.5+EQ*   &
           EQ/2.-EQ)
      ENDIF
      EE=EE/FE
      UD=UD/FE

      END SUBROUTINE PROF5
! ***********************************************************************
!====================================================================
      FUNCTION TPDD(P,THTED,TGS,RS,RD,RH,XLV0,XLV1,   &
!        ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,     &
            SVP1,SVP2,SVPT0,SVP3)
!************************* TPDD.FOR ************************************
!   THIS SUBROUTINE ITERATIVELY EXTRACTS TEMPERATURE FROM EQUIVALENT   *
!   POTENTIAL TEMP.  IT IS DESIGNED FOR USE WITH DOWNDRAFT CALCULATIONS.
!   IF RELATIVE HUMIDITY IS SPECIFIED TO BE LESS THAN 100%, PARCEL     *
!   TEMP, SPECIFIC HUMIDITY, AND LIQUID WATER CONTENT ARE ITERATIVELY  *
!   CALCULATED.                                                        *
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THTED,TGS,RD,RH,XLV0,XLV1,SVP1,SVP2,SVPT0,SVP3
!  REAL,         INTENT( OUT )   :: tpdd
   REAL                          :: tpdd
   REAL,         INTENT(INOUT)   :: RS
   REAL ::       ES, PI, THTGS, F0, T1, T0, CP, F1, DT, DSSDT, T1RH, RSRH, RL
   INTEGER ::    ITCNT
!-----------------------------------------------------------------------
!     ES=ALIQ*EXP((BLIQ*TGS-CLIQ)/(TGS-DLIQ))
      ES=1.E3*SVP1*EXP(SVP2*(TGS-SVPT0)/(TGS-SVP3))
      RS=0.622*ES/(P-ES)
      PI=(1.E5/P)**(0.2854*(1.-0.28*RS))
      THTGS=TGS*PI*EXP((3374.6525/TGS-2.5403)*RS*(1.+0.81*RS))
      F0=THTGS-THTED
      T1=TGS-0.5*F0
      T0=TGS
      CP=1005.7
!
!...ITERATE TO FIND WET-BULB TEMPERATURE...
!
      ITCNT=0
!  90 ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))
   90 ES=1.E3*SVP1*EXP(SVP2*(T1-SVPT0)/(T1-SVP3))
      RS=0.622*ES/(P-ES)
      PI=(1.E5/P)**(0.2854*(1.-0.28*RS))
      THTGS=T1*PI*EXP((3374.6525/T1-2.5403)*RS*(1.+0.81*RS))
      F1=THTGS-THTED
      IF(ABS(F1).LT.0.05)GOTO 50
      ITCNT=ITCNT+1
      IF(ITCNT.GT.10)GOTO 50
      DT=F1*(T1-T0)/(F1-F0)
      T0=T1
      F0=F1
      T1=T1-DT
      GOTO 90
   50 RL=XLV0-XLV1*T1
!
!...IF RELATIVE HUMIDITY IS SPECIFIED TO BE LESS THAN 100%, ESTIMATE THE
!   TEMPERATURE AND MIXING RATIO WHICH WILL YIELD THE APPROPRIATE VALUE.
!
      IF(RH.EQ.1.)GOTO 110
      DSSDT=SVP2*(SVPT0-SVP3)/((T1-SVP3)*(T1-SVP3))
      DT=RL*RS*(1.-RH)/(CP+RL*RH*RS*DSSDT)
      T1RH=T1+DT
!     ES=RH*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))
      ES=RH*1.E3*SVP1*EXP(SVP2*(T1RH-SVPT0)/(T1RH-SVP3))
      RSRH=0.622*ES/(P-ES)
!
!...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL
!...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...
!
      IF(RSRH.LT.RD)THEN
        RSRH=RD
        T1RH=T1+(RS-RSRH)*RL/CP
      ENDIF
      T1=T1RH
      RS=RSRH
  110 TPDD=T1
!     IF(ITCNT.GT.10)PRINT*,'***** NUMBER OF ITERATIONS IN TPDD = ',
!    +  ITCNT
      RETURN
      END FUNCTION TPDD
!====================================================================
      FUNCTION TPDDBG(P,THTED,TGS,RS,RD,RH,XLV0,XLV1,   &
!        ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,     &
            SVP1,SVP2,SVPT0,SVP3)
!************************* TPDD.FOR ************************************
!   THIS SUBROUTINE ITERATIVELY EXTRACTS TEMPERATURE FROM EQUIVALENT   *
!   POTENTIAL TEMP.  IT IS DESIGNED FOR USE WITH DOWNDRAFT CALCULATIONS.
!   IF RELATIVE HUMIDITY IS SPECIFIED TO BE LESS THAN 100%, PARCEL     *
!   TEMP, SPECIFIC HUMIDITY, AND LIQUID WATER CONTENT ARE ITERATIVELY  *
!   CALCULATED.                                                        *
!-----------------------------------------------------------------------
!
!   Modified BJG    1/17/2014
!
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THTED,TGS,RD,RH,XLV0,XLV1,SVP1,SVP2,SVPT0,SVP3
!  REAL,         INTENT( OUT )   :: tpddbg
   REAL                          :: tpddbg
   REAL,         INTENT(INOUT)   :: RS
   REAL ::       ES, PI, THTGS, F0, T1, T0, CP, F1, DT, DSSDT, T1RH, RSRH, RL
   real ::       f2, t2
   INTEGER ::    ITCNT
!-----------------------------------------------------------------------
!     ES=ALIQ*EXP((BLIQ*TGS-CLIQ)/(TGS-DLIQ))
      ES=1.E3*SVP1*EXP(SVP2*(TGS-SVPT0)/(TGS-SVP3))
      RS=0.622*ES/(P-ES)
      PI=(1.E5/P)**(0.2854*(1.-0.28*RS))
      THTGS=TGS*PI*EXP((3374.6525/TGS-2.5403)*RS*(1.+0.81*RS))
      F0=THTGS-THTED
!c      T1=TGS-0.5*F0
      T2=TGS-0.5*F0
      T0=TGS

      t1 = t0
      f1 = f0
      
      CP=1005.7
!
!...ITERATE TO FIND WET-BULB TEMPERATURE...
!
      ITCNT=0
!  90 ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))
   90 ES=1.E3*SVP1*EXP(SVP2*(t2-SVPT0)/(t2-SVP3))
      RS=0.622*ES/(P-ES)
      PI=(1.E5/P)**(0.2854*(1.-0.28*RS))
      THTGS=t2*PI*EXP((3374.6525/t2-2.5403)*RS*(1.+0.81*RS))

      f2=THTGS-THTED
      if((f1 * f2).lt.0.0) then
             f0 = f1
             t0 = t1
      else
             f0 = f0
             t0 = t0
      endif
      f1 = f2
      t1 = t2 

      IF(ABS(F1).LT.0.05)GOTO 50
      ITCNT=ITCNT+1
      IF(ITCNT.GT.10)GOTO 50
      DT=F1*(T1-T0)/(F1-F0)

      if(abs(dt).lt.abs( (t1-t0)/2.0 )) then
         dt = (t1 - t0) / 2.0
      endif

!c      T0=T1
!c      F0=F1
      t2=T1-DT

      GOTO 90
   50 RL=XLV0-XLV1*T1
!
!...IF RELATIVE HUMIDITY IS SPECIFIED TO BE LESS THAN 100%, ESTIMATE THE
!   TEMPERATURE AND MIXING RATIO WHICH WILL YIELD THE APPROPRIATE VALUE.
!
      IF(RH.EQ.1.)GOTO 110
      DSSDT=SVP2*(SVPT0-SVP3)/((T1-SVP3)*(T1-SVP3))
      DT=RL*RS*(1.-RH)/(CP+RL*RH*RS*DSSDT)
      T1RH=T1+DT
!     ES=RH*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))
      ES=RH*1.E3*SVP1*EXP(SVP2*(T1RH-SVPT0)/(T1RH-SVP3))
      RSRH=0.622*ES/(P-ES)
!
!...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL
!...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...
!
      IF(RSRH.LT.RD)THEN
        RSRH=RD
        T1RH=T1+(RS-RSRH)*RL/CP
      ENDIF
      T1=T1RH
      RS=RSRH
  110 TPDDBG=T1
!     IF(ITCNT.GT.10)PRINT*,'***** NUMBER OF ITERATIONS IN TPDD = ',
!    +  ITCNT
      RETURN
      END FUNCTION TPDDBG
!====================================================================
       SUBROUTINE TPMIX(P,THTU,TU,QU,QLIQ,QICE,QNEWLQ,QNEWIC,RATIO2,RL,  &
                         XLV0,XLV1,XLS0,XLS1,  &
!        ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,  &
        SVP1,SVP2,SVPT0,SVP3)
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THTU,RATIO2,RL,   &
                                    XLV0,XLV1,XLS0,XLS1,SVP1,SVP2,SVPT0,SVP3
   REAL,         INTENT(  OUT)   :: QNEWLQ,QNEWIC
   REAL,         INTENT(INOUT)   :: TU,QU,QLIQ,QICE
   REAL ::       C5, RV, ES, QS, PI, THTGS, ESLIQ, ESICE, F0, T1, T0, &
                 F1, DT, QNEW, QTOT, DQICE, DQLIQ, RLL, CP, dq
   INTEGER ::    ITCNT
!-----------------------------------------------------------------------
!
!...THIS SUBROUTINE ITERATIVELY EXTRACTS WET-BULB TEMPERATURE FROM EQUIV
!   POTENTIAL TEMPERATURE, THEN CHECKS TO SEE IF SUFFICIENT MOISTURE IS
!   AVAILABLE TO ACHIEVE SATURATION...IF NOT, TEMPERATURE IS ADJUSTED 
!   ACCORDINGLY, IF SO, THE RESIDUAL LIQUID WATER/ICE CONCENTRATION IS
!   DETERMINED...

      C5=1.0723E-3
      RV=461.5
!
!   ITERATE TO FIND WET BULB TEMPERATURE AS A FUNCTION OF EQUIVALENT POT
!   TEMP AND PRS, ASSUMING SATURATION VAPOR PRESSURE...RATIO2 IS THE DEG
!   OF GLACIATION...
!
      IF(RATIO2.LT.1.E-6)THEN
!       ES=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
        ES=1.E3*SVP1*EXP(SVP2*(TU-SVPT0)/(TU-SVP3))
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=TU*PI*EXP((3374.6525/TU-2.5403)*QS*(1.+0.81*QS))
      ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN 
!       ES=AICE*EXP((BICE*TU-CICE)/(TU-DICE))
        ES=611.0*EXP(22.514-6.15E3/TU)
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=TU*PI*EXP((3114.834/TU-0.278296)*QS*(1.+0.81*QS))
      ELSE
!       ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
!       ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))
        ESLIQ=1.E3*SVP1*EXP(SVP2*(TU-SVPT0)/(TU-SVP3))
        ESICE=611.0*EXP(22.514-6.15E3/TU)
        ES=(1.-RATIO2)*ESLIQ+RATIO2*ESICE
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=TU*PI*EXP(RL*QS*C5/TU*(1.+0.81*QS))
      ENDIF
      F0=THTGS-THTU
      T1=TU-0.5*F0
      T0=TU
      ITCNT=0
   90 IF(RATIO2.LT.1.E-6)THEN
!       ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))
        ES=1.E3*SVP1*EXP(SVP2*(T1-SVPT0)/(T1-SVP3))
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=T1*PI*EXP((3374.6525/T1-2.5403)*QS*(1.+0.81*QS))
      ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN
!       ES=AICE*EXP((BICE*T1-CICE)/(T1-DICE))
        ES=611.0*EXP(22.514-6.15E3/T1)
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=T1*PI*EXP((3114.834/T1-0.278296)*QS*(1.+0.81*QS))
      ELSE
!       ESLIQ=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))
!       ESICE=AICE*EXP((BICE*T1-CICE)/(T1-DICE))
        ESLIQ=1.E3*SVP1*EXP(SVP2*(T1-SVPT0)/(T1-SVP3))
        ESICE=611.0*EXP(22.514-6.15E3/T1)
        ES=(1.-RATIO2)*ESLIQ+RATIO2*ESICE
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=T1*PI*EXP(RL*QS*C5/T1*(1.+0.81*QS))
      ENDIF
      F1=THTGS-THTU
      IF(ABS(F1).LT.0.01)GOTO 50
      ITCNT=ITCNT+1
      IF(ITCNT.GT.10)GOTO 50 
      DT=F1*(T1-T0)/(F1-F0)
      T0=T1
      F0=F1
      T1=T1-DT
      GOTO 90
!
!   IF THE PARCEL IS SUPERSATURATED, CALCULATE CONCENTRATION OF FRESH
!   CONDENSATE...
!
   50 IF(QS.LE.QU)THEN
        QNEW=QU-QS
        QU=QS
        GOTO 96
      ENDIF
!
!   IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE
!   ADJUSTED...IF LIQUID WATER OR ICE IS PRESENT, IT IS ALLOWED TO EVAPO
!   SUBLIMATE.
!
      QNEW=0.
      DQ=QS-QU
      QTOT=QLIQ+QICE
!
!   IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS
!   WET BULB VALUE, VAPOR MIXING RATIO IS AT SATURATED LEVEL, AND THE MI
!   RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE UP THE ORIGINAL SATURA
!   DEFICIT... OTHERWISE, ANY AVAILABLE LIQ OR ICE VAPORIZES AND APPROPR
!   ADJUSTMENTS TO PARCEL TEMP; VAPOR, LIQUID, AND ICE MIXING RATIOS ARE
!
!...NOTE THAT THE LIQ AND ICE MAY BE PRESENT IN PROPORTIONS SLIGHTLY DIF
!   THAN SUGGESTED BY THE VALUE OF RATIO2...CHECK TO MAKE SURE THAT LIQ
!   ICE CONCENTRATIONS ARE NOT REDUCED TO BELOW ZERO WHEN EVAPORATION/
!   SUBLIMATION OCCURS...
!
      IF(QTOT.GE.DQ)THEN
        DQICE=0.0
        DQLIQ=0.0
        QLIQ=QLIQ-(1.-RATIO2)*DQ
        IF(QLIQ.LT.0.)THEN
          DQICE=0.0-QLIQ
          QLIQ=0.0
        ENDIF
        QICE=QICE-RATIO2*DQ+DQICE
        IF(QICE.LT.0.)THEN
          DQLIQ=0.0-QICE
          QICE=0.0
        ENDIF
        QLIQ=QLIQ+DQLIQ
        QU=QS
        GOTO 96
      ELSE
        IF(RATIO2.LT.1.E-6)THEN
          RLL=XLV0-XLV1*T1
        ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN 
          RLL=XLS0-XLS1*T1
        ELSE
          RLL=RL
        ENDIF
        CP=1005.7*(1.+0.89*QU)
        IF(QTOT.LT.1.E-10)THEN
!
!...IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:
          T1=T1+RLL*(DQ/(1.+DQ))/CP
          GOTO 96
        ELSE
!
!...IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE SATURA
!   THE TEMPERATURE IS GIVEN BY:
          T1=T1+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CP
          QU=QU+QTOT
          QTOT=0.
        ENDIF
        QLIQ=0
        QICE=0.
      ENDIF
   96 TU=T1
      QNEWLQ=(1.-RATIO2)*QNEW
      QNEWIC=RATIO2*QNEW
!     IF(ITCNT.GT.10)PRINT*,'***** NUMBER OF ITERATIONS IN TPMIX =',
!    +  ITCNT

      END SUBROUTINE TPMIX

!===============================================================================

   SUBROUTINE TPMIX2(p,thes,tu,qu,qliq,qice,qnewlq,qnewic,XLV1,XLV0,ktau,i,j,nk,tracker)
!
! Lookup table variables:
!     INTEGER, PARAMETER :: (KFNT=250,KFNP=220)
!     REAL, SAVE, DIMENSION(1:KFNT,1:KFNP) :: TTAB,QSTAB
!     REAL, SAVE, DIMENSION(1:KFNP) :: THE0K
!     REAL, SAVE, DIMENSION(1:200) :: ALU
!     REAL, SAVE :: RDPR,RDTHK,PLUTOP
! End of Lookup table variables:
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THES,XLV1,XLV0
   REAL,         INTENT(OUT  )   :: QNEWLQ,QNEWIC
   REAL,         INTENT(INOUT)   :: TU,QU,QLIQ,QICE
   REAL    ::    TP,QQ,BTH,TTH,PP,T00,T10,T01,T11,Q00,Q10,Q01,Q11,          &
                 TEMP,QS,QNEW,DQ,QTOT,RLL,CPP
   INTEGER ::    IPTB,ITHTB
   INTEGER,      INTENT(IN   )   :: ktau,i,j,nk,tracker        ! aj
!-----------------------------------------------------------------------

!c******** LOOKUP TABLE VARIABLES... ****************************
!      parameter(kfnt=250,kfnp=220)
!c
!      COMMON/KFLUT/ ttab(kfnt,kfnp),qstab(kfnt,kfnp),the0k(kfnp),
!     *              alu(200),rdpr,rdthk,plutop
!C***************************************************************
!c
!c***********************************************************************
!c     scaling pressure and tt table index
!c***********************************************************************
!c
      tp=(p-plutop)*rdpr
      qq=tp-aint(tp)
      iptb=int(tp)+1

!
!***********************************************************************
!              base and scaling factor for the
!***********************************************************************
!
!  scaling the and tt table index
      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)
      tth=(thes-bth)*rdthk
      pp   =tth-aint(tth)
      ithtb=int(tth)+1
       IF(IPTB.GE.220 .OR. IPTB.LE.1 .OR. ITHTB.GE.250 .OR. ITHTB.LE.1)THEN
         write(98,'(a,6i5,2f9.2,i3)')'*** OUT OF BOUNDS ***', ktau,i,j,nk,IPTB, ITHTB, &
                    p/100.0, thes, tracker
         call flush(98)
       ENDIF
!
      t00=ttab(ithtb  ,iptb  )
      t10=ttab(ithtb+1,iptb  )
      t01=ttab(ithtb  ,iptb+1)
      t11=ttab(ithtb+1,iptb+1)
!
      q00=qstab(ithtb  ,iptb  )
      q10=qstab(ithtb+1,iptb  )
      q01=qstab(ithtb  ,iptb+1)
      q11=qstab(ithtb+1,iptb+1)
!
!***********************************************************************
!              parcel temperature
!***********************************************************************
!
      temp=(t00+(t10-t00)*pp+(t01-t00)*qq+(t00-t10-t01+t11)*pp*qq)
!
      qs=(q00+(q10-q00)*pp+(q01-q00)*qq+(q00-q10-q01+q11)*pp*qq)
!
      DQ=QS-QU
      IF(DQ.LE.0.)THEN
        QNEW=QU-QS
        QU=QS
      ELSE
!
!   IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE
!   ADJUSTED...IF LIQUID WATER IS PRESENT, IT IS ALLOWED TO EVAPORATE
!
        QNEW=0.
        QTOT=QLIQ+QICE
!
!   IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS AT ITS
!   WET BULB VALUE, VAPOR MIXING RATIO IS AT SATURATED LEVEL, AND THE MIXING
!   RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE UP THE ORIGINAL SATURATION
!   DEFICIT... OTHERWISE, ANY AVAILABLE LIQ OR ICE VAPORIZES AND APPROPRIATE
!   ADJUSTMENTS TO PARCEL TEMP; VAPOR, LIQUID, AND ICE MIXING RATIOS ARE MADE.
!
!...subsaturated values only occur in calculations involving various mixtures of
!...updraft and environmental air for estimation of entrainment and detrainment.
!...For these purposes, assume that reasonable estimates can be given using
!...liquid water saturation calculations only - i.e., ignore the effect of the
!...ice phase in this process only...will not affect conservative properties...
!
        IF(QTOT.GE.DQ)THEN
          qliq=qliq-dq*qliq/(qtot+1.e-10)
          qice=qice-dq*qice/(qtot+1.e-10)
          QU=QS
        ELSE
          RLL=XLV0-XLV1*TEMP
          CPP=1004.5*(1.+0.89*QU)
          IF(QTOT.LT.1.E-10)THEN
!
!...IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:
            TEMP=TEMP+RLL*(DQ/(1.+DQ))/CPP
          ELSE
!
!...IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE SATURATION,
!   THE TEMPERATURE IS GIVEN BY:
!
            TEMP=TEMP+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CPP
            QU=QU+QTOT
            QTOT=0.
            QLIQ=0.
            QICE=0.
          ENDIF
        ENDIF
      ENDIF
      TU=TEMP
      qnewlq=qnew
      qnewic=0.
!
   END SUBROUTINE TPMIX2

!===============================================================================
       SUBROUTINE TPMIXBG(P,THTU,TU,QU,QLIQ,QICE,QNEWLQ,QNEWIC,RATIO2,RL,  &
                         XLV0,XLV1,XLS0,XLS1,  &
!        ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE,  &
        SVP1,SVP2,SVPT0,SVP3)
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THTU,RATIO2,RL,   &
                                    XLV0,XLV1,XLS0,XLS1,SVP1,SVP2,SVPT0,SVP3
   REAL,         INTENT(  OUT)   :: QNEWLQ,QNEWIC
   REAL,         INTENT(INOUT)   :: TU,QU,QLIQ,QICE
   REAL ::       C5, RV, ES, QS, PI, THTGS, ESLIQ, ESICE, F0, T1, T0, &
                 F1, DT, QNEW, QTOT, DQICE, DQLIQ, RLL, CP, dq
   real ::       f2, t2
   INTEGER ::    ITCNT
!-----------------------------------------------------------------------
!
!...THIS SUBROUTINE ITERATIVELY EXTRACTS WET-BULB TEMPERATURE FROM EQUIV
!   POTENTIAL TEMPERATURE, THEN CHECKS TO SEE IF SUFFICIENT MOISTURE IS
!   AVAILABLE TO ACHIEVE SATURATION...IF NOT, TEMPERATURE IS ADJUSTED 
!   ACCORDINGLY, IF SO, THE RESIDUAL LIQUID WATER/ICE CONCENTRATION IS
!   DETERMINED...
!    
!    Modified BJG   1/17/2014   
!

      C5=1.0723E-3
      RV=461.5

      tu = thtu / (1.e5/p) ** 0.2854


!
!   ITERATE TO FIND WET BULB TEMPERATURE AS A FUNCTION OF EQUIVALENT POT
!   TEMP AND PRS, ASSUMING SATURATION VAPOR PRESSURE...RATIO2 IS THE DEG
!   OF GLACIATION...
!
      IF(RATIO2.LT.1.E-6)THEN
!       ES=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
        ES=1.E3*SVP1*EXP(SVP2*(TU-SVPT0)/(TU-SVP3))
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=TU*PI*EXP((3374.6525/TU-2.5403)*QS*(1.+0.81*QS))
      ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN 
!       ES=AICE*EXP((BICE*TU-CICE)/(TU-DICE))
        ES=611.0*EXP(22.514-6.15E3/TU)
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=TU*PI*EXP((3114.834/TU-0.278296)*QS*(1.+0.81*QS))
      ELSE
!       ESLIQ=ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
!       ESICE=AICE*EXP((BICE*TU-CICE)/(TU-DICE))
        ESLIQ=1.E3*SVP1*EXP(SVP2*(TU-SVPT0)/(TU-SVP3))
        ESICE=611.0*EXP(22.514-6.15E3/TU)
        ES=(1.-RATIO2)*ESLIQ+RATIO2*ESICE
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=TU*PI*EXP(RL*QS*C5/TU*(1.+0.81*QS))
      ENDIF
      F0=THTGS-THTU
!c      T1=TU-0.5*F0
      T0=TU
      t1 = t0
      f1 = f0
      t2 = 50.0  
 

   
      ITCNT=0
   90 IF(RATIO2.LT.1.E-6)THEN
!       ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))
        ES=1.E3*SVP1*EXP(SVP2*(t2-SVPT0)/(t2-SVP3))
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=t2*PI*EXP((3374.6525/t2-2.5403)*QS*(1.+0.81*QS))
      ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN
!       ES=AICE*EXP((BICE*T1-CICE)/(T1-DICE))
        ES=611.0*EXP(22.514-6.15E3/t2)
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=t2*PI*EXP((3114.834/t2-0.278296)*QS*(1.+0.81*QS))
      ELSE
!       ESLIQ=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))
!       ESICE=AICE*EXP((BICE*T1-CICE)/(T1-DICE))
        ESLIQ=1.E3*SVP1*EXP(SVP2*(t2-SVPT0)/(t2-SVP3))
        ESICE=611.0*EXP(22.514-6.15E3/t2)
        ES=(1.-RATIO2)*ESLIQ+RATIO2*ESICE
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THTGS=t2*PI*EXP(RL*QS*C5/t2*(1.+0.81*QS))
      ENDIF

      f2=THTGS-THTU
      if((f1 * f2).lt.0.0) then
             f0 = f1
             t0 = t1
      else
             f0 = f0
             t0 = t0
      endif
      f1 = f2
      t1 = t2      

      IF(ABS(F1).LT.0.01)GOTO 50
      ITCNT=ITCNT+1
      IF(ITCNT.GT.10)GOTO 50 
      DT=F1*(T1-T0)/(F1-F0)

      if(abs(dt).lt.abs( (t1-t0)/2.0 )) then
         dt = (t1 - t0) / 2.0
      endif

!c      T0=T1
!c      F0=F1
      t2=T1-DT
      GOTO 90
!
!   IF THE PARCEL IS SUPERSATURATED, CALCULATE CONCENTRATION OF FRESH
!   CONDENSATE...
!
   50 IF(QS.LE.QU)THEN
        QNEW=QU-QS
        QU=QS
        GOTO 96
      ENDIF
!
!   IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE
!   ADJUSTED...IF LIQUID WATER OR ICE IS PRESENT, IT IS ALLOWED TO EVAPO
!   SUBLIMATE.
!
      QNEW=0.
      DQ=QS-QU
      QTOT=QLIQ+QICE
!
!   IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS
!   WET BULB VALUE, VAPOR MIXING RATIO IS AT SATURATED LEVEL, AND THE MI
!   RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE UP THE ORIGINAL SATURA
!   DEFICIT... OTHERWISE, ANY AVAILABLE LIQ OR ICE VAPORIZES AND APPROPR
!   ADJUSTMENTS TO PARCEL TEMP; VAPOR, LIQUID, AND ICE MIXING RATIOS ARE
!
!...NOTE THAT THE LIQ AND ICE MAY BE PRESENT IN PROPORTIONS SLIGHTLY DIF
!   THAN SUGGESTED BY THE VALUE OF RATIO2...CHECK TO MAKE SURE THAT LIQ
!   ICE CONCENTRATIONS ARE NOT REDUCED TO BELOW ZERO WHEN EVAPORATION/
!   SUBLIMATION OCCURS...
!
      IF(QTOT.GE.DQ)THEN
        DQICE=0.0
        DQLIQ=0.0
        QLIQ=QLIQ-(1.-RATIO2)*DQ
        IF(QLIQ.LT.0.)THEN
          DQICE=0.0-QLIQ
          QLIQ=0.0
        ENDIF
        QICE=QICE-RATIO2*DQ+DQICE
        IF(QICE.LT.0.)THEN
          DQLIQ=0.0-QICE
          QICE=0.0
        ENDIF
        QLIQ=QLIQ+DQLIQ
        QU=QS
        GOTO 96
      ELSE
        IF(RATIO2.LT.1.E-6)THEN
          RLL=XLV0-XLV1*T1
        ELSEIF(ABS(RATIO2-1.).LT.1.E-6)THEN 
          RLL=XLS0-XLS1*T1
        ELSE
          RLL=RL
        ENDIF
        CP=1005.7*(1.+0.89*QU)
        IF(QTOT.LT.1.E-10)THEN
!
!...IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:
          T1=T1+RLL*(DQ/(1.+DQ))/CP
          GOTO 96
        ELSE
!
!...IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE SATURA
!   THE TEMPERATURE IS GIVEN BY:
          T1=T1+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CP
          QU=QU+QTOT
          QTOT=0.
        ENDIF
        QLIQ=0
        QICE=0.
      ENDIF
   96 TU=T1
      QNEWLQ=(1.-RATIO2)*QNEW
      QNEWIC=RATIO2*QNEW
!     IF(ITCNT.GT.10)PRINT*,'***** NUMBER OF ITERATIONS IN TPMIX =',
!    +  ITCNT

      END SUBROUTINE TPMIXBG

!===============================================================================
   SUBROUTINE TPMIX2DD(p,thes,ts,qs,ktau,i,j,nk)
!
! Lookup table variables:
!     INTEGER, PARAMETER :: (KFNT=250,KFNP=220)
!     REAL, SAVE, DIMENSION(1:KFNT,1:KFNP) :: TTAB,QSTAB
!     REAL, SAVE, DIMENSION(1:KFNP) :: THE0K
!     REAL, SAVE, DIMENSION(1:200) :: ALU
!     REAL, SAVE :: RDPR,RDTHK,PLUTOP
! End of Lookup table variables:
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THES
   REAL,         INTENT(INOUT)   :: TS,QS
   INTEGER,      INTENT(IN   )   :: ktau,i,j,nk  ! avail for debugging  aj
   REAL    ::    TP,QQ,BTH,TTH,PP,T00,T10,T01,T11,Q00,Q10,Q01,Q11
   INTEGER ::    IPTB,ITHTB
   CHARACTER*256 :: MESS
!-----------------------------------------------------------------------

!
!******** LOOKUP TABLE VARIABLES (F77 format)... ****************************
!     parameter(kfnt=250,kfnp=220)
!
!     COMMON/KFLUT/ ttab(kfnt,kfnp),qstab(kfnt,kfnp),the0k(kfnp),        &
!                   alu(200),rdpr,rdthk,plutop
!***************************************************************
!
!***********************************************************************
!     scaling pressure and tt table index
!***********************************************************************
!
      tp=(p-plutop)*rdpr
      qq=tp-aint(tp)
      iptb=int(tp)+1
!
!***********************************************************************
!              base and scaling factor for the
!***********************************************************************
!
!  scaling the and tt table index
      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)
      tth=(thes-bth)*rdthk
      pp   =tth-aint(tth)
      ithtb=int(tth)+1
       IF(IPTB.GE.220 .OR. IPTB.LE.1 .OR. ITHTB.GE.250 .OR. ITHTB.LE.1)THEN
         write(97,'(a,6i5,2f9.2)')'*** OUT OF BOUNDS ***', ktau,i,j,nk,IPTB, ITHTB, p/100., thes
         call flush(97)
       ENDIF
!
      t00=ttab(ithtb  ,iptb  )
      t10=ttab(ithtb+1,iptb  )
      t01=ttab(ithtb  ,iptb+1)
      t11=ttab(ithtb+1,iptb+1)
!
      q00=qstab(ithtb  ,iptb  )
      q10=qstab(ithtb+1,iptb  )
      q01=qstab(ithtb  ,iptb+1)
      q11=qstab(ithtb+1,iptb+1)
!
!***********************************************************************
!              parcel temperature and saturation mixing ratio
!***********************************************************************
!
      ts=(t00+(t10-t00)*pp+(t01-t00)*qq+(t00-t10-t01+t11)*pp*qq)
!
      qs=(q00+(q10-q00)*pp+(q01-q00)*qq+(q00-q10-q01+q11)*pp*qq)
!
   END SUBROUTINE TPMIX2DD

!===============================================================================

      REAL FUNCTION GAMMA(X)
!D    DOUBLE PRECISION FUNCTION DGAMMA(X)
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: X
!  REAL,         INTENT(  OUT)   :: GAMMA
!----------------------------------------------------------------------
!                                                                     
! THIS ROUTINE CALCULATES THE GAMMA FUNCTION FOR A REAL ARGUMENT X.
!   COMPUTATION IS BASED ON AN ALGORITHM OUTLINED IN REFERENCE 1. 
!   THE PROGRAM USES RATIONAL FUNCTIONS THAT APPROXIMATE THE GAMMA
!   FUNCTION TO AT LEAST 20 SIGNIFICANT DECIMAL DIGITS.  COEFFICIENTS
!   FOR THE APPROXIMATION OVER THE INTERVAL (1,2) ARE UNPUBLISHED.  
!   THOSE FOR THE APPROXIMATION FOR X .GE. 12 ARE FROM REFERENCE 2.
!   THE ACCURACY ACHIEVED DEPENDS ON THE ARITHMETIC SYSTEM, THE   
!   COMPILER, THE INTRINSIC FUNCTIONS, AND PROPER SELECTION OF THE
!   MACHINE-DEPENDENT CONSTANTS.                                 
!                                                               
!                                                              
!*******************************************************************
!*******************************************************************
!                                                                  
! EXPLANATION OF MACHINE-DEPENDENT CONSTANTS                      
!                                                                
! BETA   - RADIX FOR THE FLOATING-POINT REPRESENTATION          
! MAXEXP - THE SMALLEST POSITIVE POWER OF BETA THAT OVERFLOWS  
! XBIG   - THE LARGEST ARGUMENT FOR WHICH GAMMA(X) IS REPRESENTABLE
!          IN THE MACHINE, I.E., THE SOLUTION TO THE EQUATION     
!                  GAMMA(XBIG) = BETA**MAXEXP                    
! XINF   - THE LARGEST MACHINE REPRESENTABLE FLOATING-POINT NUMBER;
!          APPROXIMATELY BETA**MAXEXP                             
! EPS    - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT 
!          1.0+EPS .GT. 1.0                                     
! XMININ - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
!          1/XMININ IS MACHINE REPRESENTABLE                   
!                                                             
!     APPROXIMATE VALUES FOR SOME IMPORTANT MACHINES ARE:    
!                                                           
!                            BETA       MAXEXP        XBIG 
!                                                         
! CRAY-1         (S.P.)        2         8191        966.961 
! CYBER 180/855                                             
!   UNDER NOS    (S.P.)        2         1070        177.803
! IEEE (IBM/XT,                                            
!   SUN, ETC.)   (S.P.)        2          128        35.040
! IEEE (IBM/XT,                                           
!   SUN, ETC.)   (D.P.)        2         1024        171.624
! IBM 3033       (D.P.)       16           63        57.574
! VAX D-FORMAT   (D.P.)        2          127        34.844 
! VAX G-FORMAT   (D.P.)        2         1023        171.489
!                                                          
!                            XINF         EPS        XMININ
!                                                         
! CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466
! CYBER 180/855                                             
!   UNDER NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294
! IEEE (IBM/XT,                                            
!   SUN, ETC.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38 
! IEEE (IBM/XT,                                             
!   SUN, ETC.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308
! IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76
! VAX D-FORMAT   (D.P.)   1.70D+38     1.39D-17    5.88D-39
! VAX G-FORMAT   (D.P.)   8.98D+307    1.11D-16    1.12D-308
!                                                          
!**************************************************************
!*************************************************************
!                                                            
! ERROR RETURNS                                             
!                                                          
!  THE PROGRAM RETURNS THE VALUE XINF FOR SINGULARITIES OR
!     WHEN OVERFLOW WOULD OCCUR.  THE COMPUTATION IS BELIEVED
!     TO BE FREE OF UNDERFLOW AND OVERFLOW.                 
!                                                          
!                                                         
!  INTRINSIC FUNCTIONS REQUIRED ARE:                     
!                                                       
!     INT, DBLE, EXP, LOG, REAL, SIN                   
!                                                     
!                                                    
! REFERENCES: `AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL 
!              FUNCTIONS`, W. J. CODY, LECTURE NOTES IN MATHEMATICS,
!              506, NUMERICAL ANALYSIS DUNDEE, 1975, G. A. WATSON  
!              (ED.), SPRINGER VERLAG, BERLIN, 1976.              
!                                                                
!              COMPUTER APPROXIMATIONS, HART, ET. AL., WILEY AND 
!              SONS, NEW YORK, 1968.                            
!                                                              
!  LATEST MODIFICATION: OCTOBER 12, 1989                      
!                                                            
!  AUTHORS: W. J. CODY AND L. STOLTZ                        
!           APPLIED MATHEMATICS DIVISION                   
!           ARGONNE NATIONAL LABORATORY                   
!           ARGONNE, IL 60439                            
!                                                       
!------------------------------------------------------
      INTEGER I,N                                     
      LOGICAL PARITY                                 
      REAL ::  &                                          
!D    DOUBLE PRECISION                             
          C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,SUM,TWELVE,  &
          TWO,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO      
      DIMENSION C(7),P(8),Q(8)                                 
!-------------------------------------------------------------
!  MATHEMATICAL CONSTANTS                                    
!-----------------------------------------------------------
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/,  &
           SQRTPI/0.9189385332046727417803297E0/,                     &
           PI/3.1415926535897932384626434E0/                      
!D    DATA ONE,HALF,TWELVE,TWO,ZERO/1.0D0,0.5D0,12.0D0,2.0D0,0.0D0/,
!D   1     SQRTPI/0.9189385332046727417803297D0/,                  
!D   2     PI/3.1415926535897932384626434D0/                      
!----------------------------------------------------------------
!  MACHINE DEPENDENT PARAMETERS                                 
!--------------------------------------------------------------
      DATA XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,        &
           XINF/3.4E38/                                      
!D    DATA XBIG,XMININ,EPS/171.624D0,2.23D-308,2.22D-16/,   
!D   1     XINF/1.79D308/                                  
!---------------------------------------------------------
!  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
!     APPROXIMATION OVER (1,2).                              
!-----------------------------------------------------------
      DATA P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1,  &
             -3.79804256470945635097577E+2,6.29331155312818442661052E+2,  &
             8.66966202790413211295064E+2,-3.14512729688483675254357E+4,  &
             -3.61444134186911729807069E+4,6.64561438202405440627855E+4/
      DATA Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2,  &
            -1.01515636749021914166146E+3,-3.10777167157231109440444E+3,  &
              2.25381184209801510330112E+4,4.75584627752788110767815E+3,  &
            -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/
!D    DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,
!D   1       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,
!D   2       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,
!D   3       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/
!D    DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,
!D   1      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,
!D   2        2.25381184209801510330112D+4,4.75584627752788110767815D+3,
!D   3      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/
!----------------------------------------------------------------------
!  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).             
!--------------------------------------------------------------------
      DATA C/-1.910444077728E-03,8.4171387781295E-04,                   &
           -5.952379913043012E-04,7.93650793500350248E-04,              &
           -2.777777777777681622553E-03,8.333333333333333331554247E-02, &
            5.7083835261E-03/                                         
!D    DATA C/-1.910444077728D-03,8.4171387781295D-04,                
!D   1     -5.952379913043012D-04,7.93650793500350248D-04,          
!D   2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
!D   3      5.7083835261D-03/                                         
!--------------------------------------------------------------------
!  STATEMENT FUNCTIONS FOR CONVERSION BETWEEN INTEGER AND FLOAT     
!------------------------------------------------------------------
      CONV(I) = REAL(I)                                           
!D    CONV(I) = DBLE(I)                                          
      PARITY=.FALSE.                                            
      FACT=ONE                                                 
      N=0                                                     
      Y=X                                                    
      IF(Y.LE.ZERO)THEN                                     
!----------------------------------------------------------
!  ARGUMENT IS NEGATIVE                                   
!--------------------------------------------------------
        Y=-X                                            
        Y1=AINT(Y)                                     
        RES=Y-Y1                                      
        IF(RES.NE.ZERO)THEN                          
          IF(Y1.NE.AINT(Y1*HALF)*TWO)PARITY=.TRUE.  
          FACT=-PI/SIN(PI*RES)                     
          Y=Y+ONE                                 
        ELSE                                     
          RES=XINF                              
          GOTO 900                             
        ENDIF                                 
      ENDIF                                  
!-------------------------------------------
!  ARGUMENT IS POSITIVE                    
!-----------------------------------------
      IF(Y.LT.EPS)THEN                   
!---------------------------------------
!  ARGUMENT .LT. EPS                   
!-------------------------------------
        IF(Y.GE.XMININ)THEN          
          RES=ONE/Y                 
        ELSE                       
          RES=XINF                
          GOTO 900               
        ENDIF                   
      ELSEIF(Y.LT.TWELVE)THEN  
        Y1=Y                  
        IF(Y.LT.ONE)THEN     
!---------------------------
!  0.0 .LT. ARGUMENT .LT. 1.0  
!--------------------------
          Z=Y                 
          Y=Y+ONE            
        ELSE                
!--------------------------
!  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY 
!-----------------------------------------------------------
          N=INT(Y)-1                                       
          Y=Y-CONV(N)                                     
          Z=Y-ONE                                        
        ENDIF                                           
!------------------------------------------------------
!  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0 
!-----------------------------------------------------
        XNUM=ZERO                                       
        XDEN=ONE                                       
        DO 260 I=1,8                                  
          XNUM=(XNUM+P(I))*Z                         
          XDEN=XDEN*Z+Q(I)                          
  260   CONTINUE                                   
        RES=XNUM/XDEN+ONE                         
        IF(Y1.LT.Y)THEN                          
!-----------------------------------------------
!  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------
          RES=RES/Y1                                
        ELSEIF(Y1.GT.Y)THEN                        
!-------------------------------------------------
!  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0
!------------------------------------------------
          DO 290 I=1,N                               
            RES=RES*Y                               
            Y=Y+ONE                                
  290     CONTINUE                                
        ENDIF                                    
      ELSE                                      
!----------------------------------------------
!  EVALUATE FOR ARGUMENT .GE. 12.0,           
!--------------------------------------------
        IF(Y.LE.XBIG)THEN                   
          YSQ=Y*Y                          
          SUM=C(7)                        
          DO 350 I=1,6                   
            SUM=SUM/YSQ+C(I)            
  350     CONTINUE                     
          SUM=SUM/Y-Y+SQRTPI          
          SUM=SUM+(Y-HALF)*LOG(Y)    
          RES=EXP(SUM)              
        ELSE                       
          RES=XINF                
          GOTO 900               
        ENDIF                   
      ENDIF                    
!-----------------------------
!  FINAL ADJUSTMENTS AND RETURN 
!------------------------------
      IF(PARITY)RES=-RES      
      IF(FACT.NE.ONE)RES=FACT/RES
  900 GAMMA=RES                 
!D900 DGAMMA = RES             
      RETURN
! ---------- LAST LINE OF GAMMA -
      END FUNCTION GAMMA
!====================================================================
      SUBROUTINE MINIM(ARRAY,KDIM,KS,KEND,KT)
      DIMENSION ARRAY(KDIM)
      KT=KS
      X=ARRAY(KS)

      DO K=KS+1,KEND
        IF(ARRAY(K).LT.X)THEN
          X=ARRAY(K)
          KT=K
        ENDIF
      ENDDO

      END SUBROUTINE MINIM
!====================================================================
      SUBROUTINE MAXIM(ARRAY,KDIM,KS,KE,MAX)
      DIMENSION ARRAY(KDIM)
      MAX=KS
      X=ARRAY(KS)

      DO K=KS,KE
        XAR=ARRAY(K)
        IF(XAR.GE.X)THEN
          X=XAR
          MAX=K
        ENDIF
      ENDDO

      END SUBROUTINE MAXIM
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!  Interpolating from p surface to sigma surface
        subroutine interp1d(pp,p,datain,dataout,kl,km)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IMPLICIT NONE
!---------------------------------
        INTEGER,                INTENT(IN   ) :: kl, km
        REAL,    dimension(km), INTENT(IN   ) :: p, datain
        REAL,    dimension(kl), INTENT(IN   ) :: pp
        REAL,    dimension(kl), INTENT(  OUT) :: dataout

        INTEGER :: i, j, k1, k2, kmin
        REAL    :: pmin, pdif, pmindif

        DO i = 1, kl
          pmin=2000.
          DO  j = 1, km
            pdif = pp(i) - p(j)
            if( abs( pdif ) .le. pmin ) then
              pmin = abs( pdif )
              pmindif = pdif
              kmin = j
             endif
          ENDDO

          if( pmindif .le. 0.0 ) then
            k1 = kmin - 1
            k2 = kmin
          else
            k1 = kmin
            k2 = kmin + 1
          endif

          dataout(i) = (datain(k2)-datain(k1))/LOG(p(k2)/p(k1))    &
                     *LOG(pp(i)/p(k1))+datain(k1)

        ENDDO

        end subroutine interp1d
!====================================================================


      subroutine kf_lutab(SVP1,SVP2,SVP3,SVPT0)
!
!  This subroutine is a lookup table.
!  Given a series of series of saturation equivalent potential
!  temperatures, the temperature is calculated.
!
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
! Lookup table variables
!     INTEGER, SAVE, PARAMETER :: KFNT=250,KFNP=220
!     REAL, SAVE, DIMENSION(1:KFNT,1:KFNP) :: TTAB,QSTAB
!     REAL, SAVE, DIMENSION(1:KFNP) :: THE0K
!     REAL, SAVE, DIMENSION(1:200) :: ALU
!     REAL, SAVE :: RDPR,RDTHK,PLUTOP
! End of Lookup table variables

     INTEGER :: KP,IT,ITCNT,I
     REAL :: DTH,TMIN,TOLER,PBOT,DPR,                               &
             TEMP,P,ES,QS,PI,THES,TGUES,THGUES,F0,T1,T0,THGS,F1,DT, &
             ASTRT,AINC,A1,THTGS
!    REAL    :: ALIQ,BLIQ,CLIQ,DLIQ,SVP1,SVP2,SVP3,SVPT0
     REAL    :: ALIQ,BLIQ,CLIQ,DLIQ
     REAL, INTENT(IN)    :: SVP1,SVP2,SVP3,SVPT0
!
! equivalent potential temperature increment
      data dth/1./
! minimum starting temp
      data tmin/150./
! tolerance for accuracy of temperature
      data toler/0.001/
! top pressure (pascals)
      plutop=5000.0
! bottom pressure (pascals)
      pbot=110000.0

      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3

!
! compute parameters
!
! 1._over_(sat. equiv. theta increment)
      rdthk=1./dth
! pressure increment
!
      DPR=(PBOT-PLUTOP)/REAL(KFNP-1)
!      dpr=(pbot-plutop)/REAL(kfnp-1)
! 1._over_(pressure increment)
      rdpr=1./dpr
! compute the spread of thes
!     thespd=dth*(kfnt-1)
!
! calculate the starting sat. equiv. theta
!
      temp=tmin
      p=plutop-dpr
      do kp=1,kfnp
        p=p+dpr
        es=aliq*exp((bliq*temp-cliq)/(temp-dliq))
        qs=0.622*es/(p-es)
        pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
        the0k(kp)=temp*pi*exp((3374.6525/temp-2.5403)*qs*        &
               (1.+0.81*qs))
      enddo
!
! compute temperatures for each sat. equiv. potential temp.
!
      p=plutop-dpr
      do kp=1,kfnp
        thes=the0k(kp)-dth
        p=p+dpr
        do it=1,kfnt
! define sat. equiv. pot. temp.
          thes=thes+dth
! iterate to find temperature
! find initial guess
          if(it.eq.1) then
            tgues=tmin
          else
            tgues=ttab(it-1,kp)
          endif
          es=aliq*exp((bliq*tgues-cliq)/(tgues-dliq))
          qs=0.622*es/(p-es)
          pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
          thgues=tgues*pi*exp((3374.6525/tgues-2.5403)*qs*      &
               (1.+0.81*qs))
          f0=thgues-thes
          t1=tgues-0.5*f0
          t0=tgues
          itcnt=0
! iteration loop
          do itcnt=1,11
            es=aliq*exp((bliq*t1-cliq)/(t1-dliq))
            qs=0.622*es/(p-es)
            pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
            thtgs=t1*pi*exp((3374.6525/t1-2.5403)*qs*(1.+0.81*qs))
            f1=thtgs-thes
            if(abs(f1).lt.toler)then
              exit
            endif
!           itcnt=itcnt+1
            dt=f1*(t1-t0)/(f1-f0)
            t0=t1
            f0=f1
            t1=t1-dt
          enddo
          ttab(it,kp)=t1
          qstab(it,kp)=qs
        enddo
      enddo
!
! lookup table for tlog(emix/aliq)
!
! set up intial values for lookup tables
!
       astrt=1.e-3
       ainc=0.075
!
       a1=astrt-ainc
       do i=1,200
         a1=a1+ainc
         alu(i)=alog(a1)
       enddo
!
   END SUBROUTINE KF_LUTAB

!====================================================================
      subroutine kf_lutabbg(SVP1,SVP2,SVP3,SVPT0)
!
!  This subroutine is a lookup table.
!  Given a series of series of saturation equivalent potential
!  temperatures, the temperature is calculated.
!
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
! Lookup table variables
!     INTEGER, SAVE, PARAMETER :: KFNT=250,KFNP=220
!     REAL, SAVE, DIMENSION(1:KFNT,1:KFNP) :: TTAB,QSTAB
!     REAL, SAVE, DIMENSION(1:KFNP) :: THE0K
!     REAL, SAVE, DIMENSION(1:200) :: ALU
!     REAL, SAVE :: RDPR,RDTHK,PLUTOP
! End of Lookup table variables

     INTEGER :: KP,IT,ITCNT,I
     REAL :: DTH,TMIN,TOLER,PBOT,DPR,                               &
             TEMP,P,ES,QS,PI,THES,TGUES,THGUES,F0,T1,T0,THGS,F1,DT, &
             ASTRT,AINC,A1,THTGS
     real :: t2, f2
!    REAL    :: ALIQ,BLIQ,CLIQ,DLIQ,SVP1,SVP2,SVP3,SVPT0
     REAL    :: ALIQ,BLIQ,CLIQ,DLIQ
     REAL, INTENT(IN)    :: SVP1,SVP2,SVP3,SVPT0
!
! equivalent potential temperature increment
      data dth/1./
! minimum starting temp
      data tmin/150./
! tolerance for accuracy of temperature
      data toler/0.001/
! top pressure (pascals)
      plutop=5000.0
! bottom pressure (pascals)
      pbot=110000.0

      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3

!
! compute parameters
!
! 1._over_(sat. equiv. theta increment)
      rdthk=1./dth
! pressure increment
!
      DPR=(PBOT-PLUTOP)/REAL(KFNP-1)
!      dpr=(pbot-plutop)/REAL(kfnp-1)
! 1._over_(pressure increment)
      rdpr=1./dpr
! compute the spread of thes
!     thespd=dth*(kfnt-1)
!
! calculate the starting sat. equiv. theta
!
      temp=tmin
      p=plutop-dpr
      do kp=1,kfnp
        p=p+dpr
        es=aliq*exp((bliq*temp-cliq)/(temp-dliq))
        qs=0.622*es/(p-es)
        pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
        the0k(kp)=temp*pi*exp((3374.6525/temp-2.5403)*qs*        &
               (1.+0.81*qs))
      enddo
!
! compute temperatures for each sat. equiv. potential temp.
!
      p=plutop-dpr
      do kp=1,kfnp
        thes=the0k(kp)-dth
        p=p+dpr
        do it=1,kfnt
! define sat. equiv. pot. temp.
          thes=thes+dth
! iterate to find temperature
! find initial guess
          if(it.eq.1) then
            tgues=tmin
          else
            tgues=ttab(it-1,kp)
          endif
          es=aliq*exp((bliq*tgues-cliq)/(tgues-dliq))
          qs=0.622*es/(p-es)
          pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
          thgues=tgues*pi*exp((3374.6525/tgues-2.5403)*qs*      &
               (1.+0.81*qs))
          f0=thgues-thes
!          t1=tgues-0.5*f0
          t2=tgues-0.5*f0
          t0=tgues
          t1 = t0
          f1 = f0


          itcnt=0
! iteration loop
          do itcnt=1,11
!            es=aliq*exp((bliq*t1-cliq)/(t1-dliq))
            es=aliq*exp((bliq*t2-cliq)/(t2-dliq))
            qs=0.622*es/(p-es)
            pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
!            thtgs=t1*pi*exp((3374.6525/t1-2.5403)*qs*(1.+0.81*qs))
            thtgs=t2*pi*exp((3374.6525/t2-2.5403)*qs*(1.+0.81*qs))

!            f1=thtgs-thes
            f2=thtgs-thes

            if((f1 * f2).lt.0.0) then
                   f0 = f1
                   t0 = t1
            else
                   f0 = f0
                   t0 = t0
            endif
            f1 = f2
            t1 = t2 


            if(abs(f1).lt.toler)then
              exit
            endif
!           itcnt=itcnt+1
            dt=f1*(t1-t0)/(f1-f0)

            if(abs(dt).lt.abs( (t1-t0)/2.0 )) then
               dt = (t1 - t0) / 2.0
            endif
            
!            t0=t1
!            f0=f1
!            t1=t1-dt
            t2=t1-dt
          enddo
          ttab(it,kp)=t1
          qstab(it,kp)=qs
        enddo
      enddo
!
! lookup table for tlog(emix/aliq)
!
! set up intial values for lookup tables
!
       astrt=1.e-3
       ainc=0.075
!
       a1=astrt-ainc
       do i=1,200
         a1=a1+ainc
         alu(i)=alog(a1)
       enddo
!
   END SUBROUTINE KF_LUTABBG
!====================================================================

END MODULE module_shcu_deng
