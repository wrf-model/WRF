!WRF:MODEL_LAYER:PHYSICS

!--- The code is based on Lin and Colle (A New Bulk Microphysical Scheme 
!             that Includes Riming Intensity and Temperature Dependent Ice Characteristics, 2011, MWR)
!             and Lin et al. (Parameterization of riming intensity and its impact on ice fall speed using ARM data, 2011, MWR)
!--- NOTE: 1) Prognose variables are: qi,PI(precipitating ice, qs, which includes snow, partially rimed snow and graupel),qw,qr
!---       2) Sedimentation flux is based on Prudue Lin scheme 
!---       2) PI has varying properties depending on riming intensity (Ri, diagnosed currently following Lin et al. (2011, MWR) and T 
!---       3) Autoconverion is based on Liu and Daum (2004)         
!---       4) PI size distribution assuming Gamma distribution, but mu_s=0 (Exponential) currently
!---       5) No density dependent fall speed since the V-D is derived using Best number approach, which already includes density effect 
!---       6) Future work will include radar equivalent reflectivity using the new PI property (A-D, M-D, N(D)). If you use RIP for reflectivity 
!---          computation, please note that snow is (1-Ri)*qs and graupel is Ri*qs. Otherwise, reflectivity will be underestimated.      
!---       7) The Liu and Daum autoconverion is quite sensitive on Nt_c. For mixed-phase cloud and marine environment, Nt_c of 10 or 20 is suggested.
!---          default value is 10E.6. Change accordingly for your use.


      MODULE module_mp_sbu_ylin
      USE    module_wrf_error
!
!..Parameters user might change based on their need
      REAL, PARAMETER, PRIVATE :: RH = 1.0
      REAL, PARAMETER, PRIVATE :: xnor = 8.0e6
      REAL, PARAMETER, PRIVATE :: Nt_c = 10.E6
!..Water vapor and air gas constants at constant pressure
      REAL, PARAMETER, PRIVATE :: Rvapor = 461.5
      REAL, PARAMETER, PRIVATE :: oRv    = 1./Rvapor
      REAL, PARAMETER, PRIVATE :: Rair   = 287.04
      REAL, PARAMETER, PRIVATE :: Cp     = 1004.0
      REAL, PARAMETER, PRIVATE :: grav   = 9.81
      REAL, PARAMETER, PRIVATE :: rhowater = 1000.0
      REAL, PARAMETER, PRIVATE :: rhosnow  = 100.0

      REAL, PARAMETER, PRIVATE :: SVP1=0.6112
      REAL, PARAMETER, PRIVATE :: SVP2=17.67
      REAL, PARAMETER, PRIVATE :: SVP3=29.65
      REAL, PARAMETER, PRIVATE :: SVPT0=273.15
      REAL, PARAMETER, PRIVATE :: EP1=Rvapor/Rair-1.
      REAL, PARAMETER, PRIVATE :: EP2=Rair/Rvapor
!..Enthalpy of sublimation, vaporization, and fusion at 0C.
      REAL, PARAMETER, PRIVATE :: XLS = 2.834E6
      REAL, PARAMETER, PRIVATE :: XLV = 2.5E6
      REAL, PARAMETER, PRIVATE :: XLF = XLS - XLV

!
      REAL, PARAMETER, PRIVATE ::                           &
             qi0 = 1.0e-3,                                  &   !--- ice aggregation to snow threshold
             xmi50 = 4.8e-10, xmi40 = 2.46e-10,             &
             xni0 = 1.0e-2, xmnin = 1.05e-18, bni = 0.5,    &
             di50 = 1.0e-4, xmi = 4.19e-13,                 &   !--- parameters used in BF process
             bv_r = 0.8, bv_i = 0.25,                       &
             o6 = 1./6.,  cdrag = 0.6,                      &
             avisc = 1.49628e-6, adiffwv = 8.7602e-5,       &
             axka = 1.4132e3, cw = 4.187e3,  ci = 2.093e3
CONTAINS

!-------------------------------------------------------------------
!  Lin et al., 1983, JAM, 1065-1092, and
!  Rutledge and Hobbs, 1984, JAS, 2949-2972
!-------------------------------------------------------------------
      SUBROUTINE sbu_ylin(th                                       &
                      ,qv, ql, qr                                  &
                      ,qi, qs, Ri3D                                &
                      ,rho, pii, p                                 &
                      ,dt_in                                       &
                      ,z,ht, dz8w                                  &
                      ,RAINNC, RAINNCV                             &
                      ,ids,ide, jds,jde, kds,kde                   &
                      ,ims,ime, jms,jme, kms,kme                   &
                      ,its,ite, jts,jte, kts,kte                   &
                                                                   )
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!
!
     INTEGER,   INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde , &
                                      ims,ime, jms,jme, kms,kme , &
                                      its,ite, jts,jte, kts,kte

     REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
        INTENT(INOUT) ::                                          &
                                                              th, &
                                                              qv, &
                                                           qi,ql, &
                                                           qs,qr

! YLIN
! Adding RI3D as a variable to the interface
     REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                &
        INTENT(INOUT) :: Ri3D


!
     REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
        INTENT(IN   ) ::                                          &
                                                             rho, &
                                                             pii, &
                                                             z,p, &
                                                            dz8w


     REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::       ht
     REAL, INTENT(IN   ) ::                                   dt_in  

     REAL, DIMENSION( ims:ime , jms:jme ),                           &
        INTENT(INOUT) ::                                  RAINNC, &
                                                          RAINNCV

! LOCAL VAR

     INTEGER             ::                            min_q, max_q

     REAL, DIMENSION( its:ite , jts:jte )                            &
                               ::        rain, snow,ice

     REAL, DIMENSION( kts:kte )   ::                  qvz, qlz, qrz, &
                                                      qiz, qsz, qgz, &
                                                                thz, &
                                                        tothz, rhoz, &
                                                      orhoz, sqrhoz, &
                                                           prez, zz, &
                                                        dzw

! Added vertical profile of Ri (riz) as a variable
     REAL, DIMENSION( kts:kte ) :: riz

!
     REAL    ::         dt, pptice, pptrain, pptsnow, pptgraul, rhoe_s

     INTEGER ::               i,j,k

      dt=dt_in
      rhoe_s=1.29
  
      j_loop:  DO j = jts, jte
      i_loop:  DO i = its, ite
!
!- write data from 3-D to 1-D
!
      DO k = kts, kte
       qvz(k)=qv(i,k,j)
       qlz(k)=ql(i,k,j)
       qrz(k)=qr(i,k,j)
       qiz(k)=qi(i,k,j)
       qsz(k)=qs(i,k,j)
       thz(k)=th(i,k,j)
       rhoz(k)=rho(i,k,j)
       orhoz(k)=1./rhoz(k)
       prez(k)=p(i,k,j)
!       sqrhoz(k)=sqrt(rhoe_s*orhoz(k))
! no density dependence of fall speed as Note #5, you can turn it on to increase fall speed at low pressure.
       sqrhoz(k)=1.0
       tothz(k)=pii(i,k,j)
       zz(k)=z(i,k,j)
       dzw(k)=dz8w(i,k,j)
      END DO
!
      pptrain=0.
      pptsnow=0.
      pptice =0.

!     CALL wrf_debug ( 100 , 'microphysics_driver: calling clphy1d_ylin' )

     CALL clphy1d_ylin(  dt, qvz, qlz, qrz, qiz, qsz,              &
                         thz, tothz, rhoz, orhoz, sqrhoz,          &
                         prez, zz, dzw, ht(I,J),                   &
                         pptrain, pptsnow, pptice,                 &
                         kts, kte, i, j, riz                       )

!
! Precipitation from cloud microphysics -- only for one time step
!
! unit is transferred from m to mm
!
      rain(i,j)= pptrain
      snow(i,j)= pptsnow
      ice(i,j) = pptice
!
      RAINNCV(i,j)= pptrain + pptsnow + pptice
      RAINNC(i,j) = RAINNC(i,j) + pptrain + pptsnow + pptice

!
!- update data from 1-D back to 3-D
!
      DO k = kts, kte
       qv(i,k,j)=qvz(k)
       ql(i,k,j)=qlz(k)
       qr(i,k,j)=qrz(k)
       th(i,k,j)=thz(k)
       qi(i,k,j)=qiz(k)
       qs(i,k,j)=qsz(k)
       ri3d(i,k,j)=riz(k)
      END DO
!
      ENDDO i_loop
      ENDDO j_loop

      END SUBROUTINE sbu_ylin


!-----------------------------------------------------------------------
      SUBROUTINE clphy1d_ylin(dt, qvz, qlz, qrz, qiz, qsz,             &
                      thz, tothz, rho, orho, sqrho,                    &
                      prez, zz, dzw, zsfc, pptrain, pptsnow,pptice,    &
                      kts, kte, i, j,riz                               )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!  This program handles the vertical 1-D cloud micphysics
!-----------------------------------------------------------------------
! avisc: constant in empirical formular for dynamic viscosity of air
!         =1.49628e-6 [kg/m/s] = 1.49628e-5 [g/cm/s]
! adiffwv: constant in empirical formular for diffusivity of water
!          vapor in air
!          = 8.7602e-5 [kgm/s3] = 8.7602 [gcm/s3]
! axka: constant in empirical formular for thermal conductivity of air
!       = 1.4132e3 [m2/s2/K] = 1.4132e7 [cm2/s2/K]
! qi0: mixing ratio threshold for cloud ice aggregation [kg/kg]
! xmi50: mass of a 50 micron ice crystal
!        = 4.8e-10 [kg] =4.8e-7 [g]
! xmi40: mass of a 40 micron ice crystal
!        = 2.46e-10 [kg] = 2.46e-7 [g]
! di50: diameter of a 50 micro (radius) ice crystal
!       =1.0e-4 [m]
! xmi: mass of one cloud ice crystal
!      =4.19e-13 [kg] = 4.19e-10 [g]
! oxmi=1.0/xmi
!
! xni0=1.0e-2 [m-3] The value given in Lin et al. is wrong.(see
!                   Hsie et al.(1980) and Rutledge and Hobbs(1983) )
! bni=0.5 [K-1]
! xmnin: mass of a natural ice nucleus
!    = 1.05e-18 [kg] = 1.05e-15 [g] This values is suggested by
!                    Hsie et al. (1980)
!    = 1.0e-12 [kg] suggested by Rutlegde and Hobbs (1983)

! av_r: av_r in empirical formular for terminal
!         velocity of raindrop
!         =2115.0 [cm**(1-b)/s] = 2115.0*0.01**(1-b) [m**(1-b)/s]
! bv_r: bv_r in empirical formular for terminal
!         velocity of raindrop
!         =0.8
! av_i: av_i in empirical formular for terminal
!         velocity of snow
!         =152.93 [cm**(1-d)/s] = 152.93*0.01**(1-d) [m**(1-d)/s]
! bv_i: bv_i in empirical formular for terminal
!         velocity of snow
!         =0.25
! vf1r: ventilation factors for rain =0.78
! vf2r: ventilation factors for rain =0.31
! vf1s: ventilation factors for snow =0.65
! vf2s: ventilation factors for snow =0.44
!
!----------------------------------------------------------------------

     INTEGER, INTENT(IN   )               :: kts, kte, i, j

     REAL,    DIMENSION( kts:kte ),                                   &
           INTENT(INOUT)               :: qvz, qlz, qrz, qiz, qsz,    &
                                          thz

     REAL,    DIMENSION( kts:kte ),                                   &
           INTENT(IN   )               :: tothz, rho, orho, sqrho,    &
                                          prez, zz, dzw


     REAL,    INTENT(INOUT)               :: pptrain, pptsnow, pptice

     REAL,    INTENT(IN   )               :: dt, zsfc

! local vars

     REAL                                :: obp4, bp3, bp5, bp6, odp4,  &
                                            dp3, dp5, dp5o2

! temperary vars

     REAL                              :: tmp, tmp0, tmp1, tmp2,tmp3,  &
                                          tmp4, tmpa,tmpb,tmpc,tmpd,alpha1,  &
                                          qic, abi,abr, abg, odtberg,  &
                                          vti50,eiw,eri,esi,esr, esw,  &
                                          erw,delrs,term0,term1,       &
                                          Ap, Bp,                      &
                                          factor, tmp_r, tmp_s,tmp_g,  &
                                          qlpqi, rsat, a1, a2, xnin

!
     REAL, DIMENSION( kts:kte )    ::  oprez, tem, temcc, theiz, qswz,    &
                                       qsiz, qvoqswz, qvoqsiz, qvzodt,    &
                                       qlzodt, qizodt, qszodt, qrzodt

!--- microphysical processes

     REAL, DIMENSION( kts:kte )    :: psnow, psaut, psfw,  psfi,  praci,  &
                                      piacr, psaci, psacw, psdep, pssub,  &
                                      pracs, psacr, psmlt, psmltevp,      &
                                      prain, praut, pracw, prevp, pvapor, &
                                      pclw,  pladj, pcli,  pimlt, pihom,  &
                                      pidw,  piadj, pgfr,                 &
                                      qschg
!

     REAL, DIMENSION( kts:kte )    :: qvsbar, rs0, viscmu, visc, diffwv,  &
                                      schmidt, xka
!---- new snow parameters

     REAL, DIMENSION( kts:kte ):: ab_s,ab_r,ab_riming,lamc 
     REAL, DIMENSION( kts:kte ):: cap_s       !---- capacitance of snow
 
     REAL, PARAMETER :: vf1s = 0.65, vf2s = 0.44, vf1r =0.78 , vf2r = 0.31 

     REAL, PARAMETER :: am_c1=0.004, am_c2= 6e-5,    am_c3=0.15
     REAL, PARAMETER :: bm_c1=1.85,  bm_c2= 0.003,   bm_c3=1.25
     REAL, PARAMETER :: aa_c1=1.28,  aa_c2= -0.012,  aa_c3=-0.6
     REAL, PARAMETER :: ba_c1=1.5,   ba_c2= 0.0075,  ba_c3=0.5

     REAL, PARAMETER :: best_a=1.08 ,  best_b = 0.499
     REAL, DIMENSION(kts:kte):: am_s,bm_s,av_s,bv_s,Ri,N0_s,tmp_ss,lams  
     REAL, DIMENSION(kts:kte):: aa_s,ba_s,tmp_sa  
     REAL, PARAMETER :: mu_s=0.,mu_i=0.,mu_r=0.

     REAL :: tc0, disp, Dc_liu, eta, mu_c, R6c      !--- for Liu's autoconversion

! Adding variable Riz, which will duplicate Ri but be a copy passed upward

     REAL, DIMENSION(kts:kte) :: Riz

     REAL, DIMENSION( kts:kte )    :: vtr, vts,                   &
                                      vtrold, vtsold, vtiold,     &
                                      xlambdar, xlambdas,            &
                                      olambdar, olambdas

     REAL                          :: episp0k, dtb, odtb, pi, pio4,       &
                                      pio6, oxLf, xLvocp, xLfocp, av_r,   &
                                      av_i, ocdrag, gambp4, gamdp4,       &
                                      gam4pt5, Cpor, oxmi, gambp3, gamdp3,&
                                      gambp6, gam3pt5, gam2pt75, gambp5o2,&
                                      gamdp5o2, cwoxlf, ocp, xni50, es
!
     REAL                          :: qvmin=1.e-20
     REAL                          :: temc1,save1,save2,xni50mx

! for terminal velocity flux

     INTEGER                       :: min_q, max_q, max_ri_k, k
     REAL                          :: max_ri
     REAL                          :: t_del_tv, del_tv, flux, fluxin, fluxout ,tmpqrz
     LOGICAL                       :: notlast
!
      mu_c = AMIN1(15., (1000.E6/Nt_c + 2.))
      R6c  = 10.0E-6      !---- 10 micron, threshold radius of cloud droplet
      dtb=dt
      odtb=1./dtb
      pi  =acos(-1.)
      pio4=acos(-1.)/4.
      pio6=acos(-1.)/6.
      ocp=1./cp
      oxLf=1./xLf
      xLvocp=xLv/cp
      xLfocp=xLf/cp
      Cpor=cp/Rair
      oxmi=1.0/xmi
      cwoxlf=cw/xlf 
      av_r=2115.0*0.01**(1-bv_r)
      av_i=152.93*0.01**(1-bv_i)
      ocdrag=1./Cdrag
      episp0k=RH*ep2*1000.*svp1
!
      gambp4=ggamma(bv_r+4.)
      gamdp4=ggamma(bv_i+4.)
      gambp3=ggamma(bv_r+3.)
      gambp6=ggamma(bv_r+6)
      gambp5o2=ggamma((bv_r+5.)/2.)
      gamdp5o2=ggamma((bv_i+5.)/2.)
!
!     oprez       1./prez ( prez : pressure)
!     qsw         saturated mixing ratio on water surface
!     qsi         saturated mixing ratio on ice surface
!     episp0k     RH*e*saturated pressure at 273.15 K  = 611.2 hPa (Rogers and Yau 1989)
!     qvoqsw      qv/qsw
!     qvoqsi      qv/qsi
!     qvzodt      qv/dt
!     qlzodt      ql/dt
!     qizodt      qi/dt
!     qszodt      qs/dt
!     qrzodt      qr/dt
!     temcc       temperature in dregee C
!

      obp4=1.0/(bv_r+4.0)
      bp3=bv_r+3.0
      bp5=bv_r+5.0
      bp6=bv_r+6.0
      odp4=1.0/(bv_i+4.0)
      dp3=bv_i+3.0
      dp5=bv_i+5.0
      dp5o2=0.5*(bv_i+5.0)
!
      do k=kts,kte
         oprez(k)=1./prez(k)
         qlz(k)=amax1( 0.0,qlz(k) )
         qiz(k)=amax1( 0.0,qiz(k) )
         qvz(k)=amax1( qvmin,qvz(k) )
         qsz(k)=amax1( 0.0,qsz(k) )
         qrz(k)=amax1( 0.0,qrz(k) )
         tem(k)=thz(k)*tothz(k)
         temcc(k)=tem(k)-273.15
         es=1000.*svp1*exp( svp2*temcc(k)/(tem(k)-svp3) )  !--- RY89 Eq(2.17)
         qswz(k)=ep2*es/(prez(k)-es)
         if (tem(k) .lt. 273.15 ) then
            es=1000.*svp1*exp( 21.8745584*(tem(k)-273.16)/(tem(k)-7.66) )
            qsiz(k)=ep2*es/(prez(k)-es)
            if (temcc(k) .lt. -40.0) qswz(k)=qsiz(k)
         else
            qsiz(k)=qswz(k)
         endif
!
         qvoqswz(k)=qvz(k)/qswz(k)
         qvoqsiz(k)=qvz(k)/qsiz(k)
         qvzodt(k)=amax1( 0.0,odtb*qvz(k) )
         qlzodt(k)=amax1( 0.0,odtb*qlz(k) )
         qizodt(k)=amax1( 0.0,odtb*qiz(k) )
         qszodt(k)=amax1( 0.0,odtb*qsz(k) )
         qrzodt(k)=amax1( 0.0,odtb*qrz(k) )
         theiz(k)=thz(k)+(xlvocp*qvz(k)-xlfocp*qiz(k))/tothz(k)
      enddo

      do k=kts,kte

         psnow(k)=0.0
         psaut(k)=0.0
         psfw(k)=0.0
         psfi(k)=0.0
         praci(k)=0.0
         piacr(k)=0.0
         psaci(k)=0.0
         psacw(k)=0.0
         psdep(k)=0.0
         pssub(k)=0.0
         pracs(k)=0.0
         psacr(k)=0.0
         psmlt(k)=0.0
         psmltevp(k)=0.0

         prain(k)=0.0
         praut(k)=0.0
         pracw(k)=0.0
         prevp(k)=0.0
         pgfr(k)=0.0

         pvapor(k)=0.0

         pclw(k)=0.0
         pladj(k)=0.0

         pcli(k)=0.0
         pimlt(k)=0.0
         pihom(k)=0.0
         pidw(k)=0.0
         piadj(k)=0.0

         qschg(k)=0.

      enddo

!***********************************************************************
!*****  compute viscosity,difusivity,thermal conductivity, and    ******
!*****  Schmidt number                                            ******
!***********************************************************************
!c------------------------------------------------------------------
!c      viscmu: dynamic viscosity of air kg/m/s
!c      visc: kinematic viscosity of air = viscmu/rho (m2/s)
!c      avisc=1.49628e-6 kg/m/s=1.49628e-5 g/cm/s
!c      viscmu=1.718e-5 kg/m/s in RH
!c      diffwv: Diffusivity of water vapor in air
!c      adiffwv = 8.7602e-5 (8.794e-5 in MM5) kgm/s3
!c              = 8.7602 (8.794 in MM5) gcm/s3
!c      diffwv(k)=2.26e-5 m2/s
!c      schmidt: Schmidt number=visc/diffwv
!c      xka: thermal conductivity of air J/m/s/K (Kgm/s3/K)
!c      xka(k)=2.43e-2 J/m/s/K in RH
!c      axka=1.4132e3 (1.414e3 in MM5) m2/s2/k = 1.4132e7 cm2/s2/k
!c------------------------------------------------------------------
      DO k=kts,kte
        viscmu(k)=avisc*tem(k)**1.5/(tem(k)+120.0)
        visc(k)=viscmu(k)*orho(k)
        diffwv(k)=adiffwv*tem(k)**1.81*oprez(k)
        schmidt(k)=visc(k)/diffwv(k)
        xka(k)=axka*viscmu(k)
        rs0(k)=ep2*1000.*svp1/(prez(k)-1000.*svp1)
      END DO
!
! ---- YLIN, set snow variables
!
!---- A+B in depositional growth, the first try just take from Rogers and Yau(1989)
!         ab_s(k) = lsub*lsub*orv/(tcond(k)*temp(k))+&
!                   rv*temp(k)/(diffu(k)*qvsi(k))

       do k = kts, kte
         tc0   = tem(k)-273.15       
        if (rho(k)*qlz(k) .gt. 1e-5 .AND. rho(k)*qsz(k) .gt. 1e-5) then 
         Ri(k) = 1.0/(1.0+6e-5/(rho(k)**1.170*qlz(k)*qsz(k)**0.170))
        else
         Ri(k) = 0
        endif
       enddo
!
!--- make sure Ri does not decrease downward in a column
!
       max_ri_k = MAXLOC(Ri,dim=1)
       max_ri   = MAXVAL(Ri)

       do k = kts, max_ri_k
         Ri(k) = max_ri
       enddo

!--- YLIN, get PI properties
      do k = kts, kte
         Ri(k) = AMAX1(0.,AMIN1(Ri(k),1.0))      
! Store the value of Ri(k) as Riz(k)
         Riz(k) = Ri(k)

         cap_s(k)= 0.25*(1+Ri(k))
         tc0     = AMIN1(-0.1, tem(k)-273.15)          
         N0_s(k) = amin1(2.0E8, 2.0E6*exp(-0.12*tc0))          
         am_s(k) = am_c1+am_c2*tc0+am_c3*Ri(k)*Ri(k)   !--- Heymsfield 2007
         am_s(k) = AMAX1(0.000023,am_s(k))             !--- use the a_min in table 1 of Heymsfield
         bm_s(k) = bm_c1+bm_c2*tc0+bm_c3*Ri(k)
         bm_s(k) = AMIN1(bm_s(k),3.0)                  !---- capped by 3
!---  converting from cgs to SI unit
         am_s(k) =  10**(2*bm_s(k)-3.0)*am_s(k)
         aa_s(k) = aa_c1 + aa_c2*tc0 + aa_c3*Ri(k)
         ba_s(k) = ba_c1 + ba_c2*tc0 + ba_c3*Ri(k)
!---  convert to SI unit as in paper
         aa_s(k) = (1e-2)**(2.0-ba_s(k))*aa_s(k)
!---- get v from Mitchell 1996
         av_s(k) = best_a*viscmu(k)*(2*grav*am_s(k)/rho(k)/aa_s(k)/(viscmu(k)**2))**best_b
         bv_s(k) = best_b*(bm_s(k)-ba_s(k)+2)-1
        
         tmp_ss(k)= bm_s(k)+mu_s+1
         tmp_sa(k)= ba_s(k)+mu_s+1

      enddo 

!
!***********************************************************************
! Calculate precipitation fluxes due to terminal velocities.
!***********************************************************************
!
!- Calculate termianl velocity (vt?)  of precipitation q?z
!- Find maximum vt? to determine the small delta t
!
!-- rain
!
!       CALL wrf_debug ( 100 , 'module_ylin, start precip fluxes' )

      t_del_tv=0.
      del_tv=dtb
      notlast=.true.

      DO while (notlast)
!
       min_q=kte
       max_q=kts-1
!
      do k=kts,kte-1
         if (qrz(k) .gt. 1.0e-8) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)
            tmp1=sqrt(pi*rhowater*xnor/rho(k)/qrz(k))
            tmp1=sqrt(tmp1)
            vtrold(k)=o6*av_r*gambp4*sqrho(k)/tmp1**bv_r
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-zsfc)/vtrold(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtrold(k))
            endif
         else
            vtrold(k)=0.
         endif
      enddo

      if (max_q .ge. min_q) then
!
!- Check if the summation of the small delta t >=  big delta t
!             (t_del_tv)          (del_tv)             (dtb)

         t_del_tv=t_del_tv+del_tv
!
         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif
!
         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rho(k)*vtrold(k)*qrz(k)
            flux=(fluxin-fluxout)/rho(k)/dzw(k)
            tmpqrz=qrz(k)
            qrz(k)=qrz(k)+del_tv*flux
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptrain=pptrain+fluxin*del_tv
         else
            qrz(min_q-1)=qrz(min_q-1)+del_tv*  &
                          fluxin/rho(min_q-1)/dzw(min_q-1)
         endif
!
      else
         notlast=.false.
      endif
      ENDDO

!
!-- snow
!
      t_del_tv=0.
      del_tv=dtb
      notlast=.true.

      DO while (notlast)
!
       min_q=kte
       max_q=kts-1
!
      do k=kts,kte-1
         if (qsz(k) .gt. 1.0e-8) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)

            tmp1= (am_s(k)*N0_s(k)*ggamma(tmp_ss(k))*orho(k)/qsz(k))&
                   **(1./tmp_ss(k))

            vtsold(k)= sqrho(k)*av_s(k)*ggamma(bv_s(k)+tmp_ss(k))/ &
                      ggamma(tmp_ss(k))/(tmp1**bv_s(k))

            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-zsfc)/vtsold(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtsold(k))
            endif
         else
            vtsold(k)=0.
         endif
      enddo

      if (max_q .ge. min_q) then
!
!
!- Check if the summation of the small delta t >=  big delta t
!             (t_del_tv)          (del_tv)             (dtb)

         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif
!
         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rho(k)*vtsold(k)*qsz(k)
            flux=(fluxin-fluxout)/rho(k)/dzw(k)
            qsz(k)=qsz(k)+del_tv*flux
            qsz(k)=amax1(0.,qsz(k))
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptsnow=pptsnow+fluxin*del_tv
         else
            qsz(min_q-1)=qsz(min_q-1)+del_tv*  &
                         fluxin/rho(min_q-1)/dzw(min_q-1)
         endif
!
      else
         notlast=.false.
      endif

      ENDDO

!
!-- cloud ice  (03/21/02) using Heymsfield and Donner (1990) Vi=3.29*qi^0.16
!
      t_del_tv=0.
      del_tv=dtb
      notlast=.true.
!
      DO while (notlast)
!
      min_q=kte
      max_q=kts-1
!
      do k=kts,kte-1
         if (qiz(k) .gt. 1.0e-8) then
            min_q=min0(min_q,k)
            max_q=max0(max_q,k)
            vtiold(k)= 3.29 * (rho(k)* qiz(k))** 0.16  ! Heymsfield and Donner
            if (k .eq. 1) then
               del_tv=amin1(del_tv,0.9*(zz(k)-zsfc)/vtiold(k))
            else
               del_tv=amin1(del_tv,0.9*(zz(k)-zz(k-1))/vtiold(k))
            endif
         else
            vtiold(k)=0.
         endif
      enddo

      if (max_q .ge. min_q) then
!
!- Check if the summation of the small delta t >=  big delta t
!             (t_del_tv)          (del_tv)             (dtb)

         t_del_tv=t_del_tv+del_tv

         if ( t_del_tv .ge. dtb ) then
              notlast=.false.
              del_tv=dtb+del_tv-t_del_tv
         endif

         fluxin=0.
         do k=max_q,min_q,-1
            fluxout=rho(k)*vtiold(k)*qiz(k)
            flux=(fluxin-fluxout)/rho(k)/dzw(k)
            qiz(k)=qiz(k)+del_tv*flux
            qiz(k)=amax1(0.,qiz(k))
            fluxin=fluxout
         enddo
         if (min_q .eq. 1) then
            pptice=pptice+fluxin*del_tv
         else
            qiz(min_q-1)=qiz(min_q-1)+del_tv*  &
                         fluxin/rho(min_q-1)/dzw(min_q-1)
         endif
!
      else
         notlast=.false.
      endif
!
      ENDDO

!     CALL wrf_debug ( 100 , 'module_ylin: end precip flux' )

! Microphpysics processes

      DO 2000 k=kts,kte
!
         qvzodt(k)=amax1( 0.0,odtb*qvz(k) )
         qlzodt(k)=amax1( 0.0,odtb*qlz(k) )
         qizodt(k)=amax1( 0.0,odtb*qiz(k) )
         qszodt(k)=amax1( 0.0,odtb*qsz(k) )
         qrzodt(k)=amax1( 0.0,odtb*qrz(k) )

!***********************************************************************
!*****   diagnose mixing ratios (qrz,qsz), terminal                *****
!*****   velocities (vtr,vts), and slope parameters in size        *****
!*****   distribution(xlambdar,xlambdas) of rain and snow          *****
!*****   follows Nagata and Ogura, 1991, MWR, 1309-1337. Eq (A7)   *****
!***********************************************************************
!
!**** assuming no cloud water can exist in the top two levels due to
!**** radiation consideration
!
!!  if
!!     unsaturated,
!!     no cloud water, rain, ice, snow
!!  then
!!     skip these processes and jump to line 2000
!
!
        tmp=qiz(k)+qlz(k)+qsz(k)+qrz(k)
        if( qvz(k)+qlz(k)+qiz(k) .lt. qsiz(k)  &
            .and. tmp .eq. 0.0 ) go to 2000
!
!! calculate terminal velocity of rain
!
        if (qrz(k) .gt. 1.0e-8) then
            tmp1=sqrt(pi*rhowater*xnor*orho(k)/qrz(k))
            xlambdar(k)=sqrt(tmp1)
            olambdar(k)=1.0/xlambdar(k)
            vtrold(k)=o6*av_r*gambp4*sqrho(k)*olambdar(k)**bv_r
        else
            vtrold(k)=0.
            olambdar(k)=0.
        endif
!
        if (qrz(k) .gt. 1.0e-8) then
            tmp1=sqrt(pi*rhowater*xnor*orho(k)/qrz(k))
            xlambdar(k)=sqrt(tmp1)
            olambdar(k)=1.0/xlambdar(k)
            vtr(k)=o6*av_r*gambp4*sqrho(k)*olambdar(k)**bv_r
        else
            vtr(k)=0.
            olambdar(k)=0.
        endif
!
!! calculate terminal velocity of snow
!
        if (qsz(k) .gt. 1.0e-8) then
            tmp1= (am_s(k)*N0_s(k)*ggamma(tmp_ss(k))*orho(k)/qsz(k))&
                   **(1./tmp_ss(k))
            olambdas(k)=1.0/tmp1
            vtsold(k)= sqrho(k)*av_s(k)*ggamma(bv_s(k)+tmp_ss(k))/ &
                      ggamma(tmp_ss(k))/(tmp1**bv_s(k))

        else
            vtsold(k)=0.
            olambdas(k)=0.
        endif
!
        if (qsz(k) .gt. 1.0e-8) then
             tmp1= (am_s(k)*N0_s(k)*ggamma(tmp_ss(k))*orho(k)/qsz(k))&
                   **(1./tmp_ss(k))
             olambdas(k)=1.0/tmp1
             vts(k)= sqrho(k)*av_s(k)*ggamma(bv_s(k)+tmp_ss(k))/ &
                      ggamma(tmp_ss(k))/(tmp1**bv_s(k))

        else
            vts(k)=0.
            olambdas(k)=0.
        endif

!---------- start of snow/ice processes below freezing

        if (tem(k) .lt. 273.15) then

!
!***********************************************************************
!*********        snow production processes for T < 0 C       **********
!***********************************************************************
!c
!c (1) ICE CRYSTAL AGGREGATION TO SNOW (Psaut): Lin (21)
!c!    psaut=alpha1*(qi-qi0)
!c!    alpha1=1.0e-3*exp(0.025*(T-T0))
!c
           alpha1=1.0e-3*exp( 0.025*temcc(k) )
!
           if(temcc(k) .lt. -20.0) then
             tmp1=-7.6+4.0*exp( -0.2443e-3*(abs(temcc(k))-20)**2.455 )
             qic=1.0e-3*exp(tmp1)*orho(k)
           else
             qic=qi0
           end if

           tmp1=odtb*(qiz(k)-qic)*(1.0-exp(-alpha1*dtb))
           psaut(k)=amax1( 0.0,tmp1 )

!c
!c (2) BERGERON PROCESS TRANSFER OF CLOUD WATER TO SNOW (Psfw)
!c     this process only considered when -31 C < T < 0 C
!c     Lin (33) and Hsie (17)
!c
!c!
!c!    parama1 and parama2 functions must be user supplied
!c!

          if( qlz(k) .gt. 1.0e-10 ) then
            temc1=amax1(-30.99,temcc(k))
            a1=parama1( temc1 )
            a2=parama2( temc1 )
            tmp1=1.0-a2
!!   change unit from cgs to mks
            a1=a1*0.001**tmp1
!!   dtberg is the time needed for a crystal to grow from 40 to 50 um
!!   odtberg=1.0/dtberg
            odtberg=(a1*tmp1)/(xmi50**tmp1-xmi40**tmp1)
!
!!   compute terminal velocity of a 50 micron ice cystal
!
            vti50=av_i*di50**bv_i*sqrho(k)
!
            eiw=1.0
            save1=a1*xmi50**a2
            save2=0.25*pi*eiw*rho(k)*di50*di50*vti50
!
            tmp2=( save1 + save2*qlz(k) )
!
!!  maximum number of 50 micron crystals limited by the amount
!!  of supercool water
!
            xni50mx=qlzodt(k)/tmp2
!
!!   number of 50 micron crystals produced
!
            xni50=qiz(k)*( 1.0-exp(-dtb*odtberg) )/xmi50
            xni50=amin1(xni50,xni50mx)
!
            tmp3=odtb*tmp2/save2*( 1.0-exp(-save2*xni50*dtb) )
            psfw(k)=amin1( tmp3,qlzodt(k) )
!c
!c (3) REDUCTION OF CLOUD ICE BY BERGERON PROCESS (Psfi): Lin (34)
!c     this process only considered when -31 C < T < 0 C
!c
            tmp1=xni50*xmi50-psfw(k)
            psfi(k)=amin1(tmp1,qizodt(k))
          end if
!
!
          if(qrz(k) .le. 0.0) go to 1000
!
! Processes (4) and (5) only need when qrz > 0.0
!
!c
!c (4) CLOUD ICE ACCRETION BY RAIN (Praci): Lin (25)
!c     produce PI
!c
          eri=1.0
          save1=pio4*eri*xnor*av_r*sqrho(k)
          tmp1=save1*gambp3*olambdar(k)**bp3
          praci(k)=qizodt(k)*( 1.0-exp(-tmp1*dtb) )

!c
!c (5) RAIN ACCRETION BY CLOUD ICE (Piacr): Lin (26)
!c
          tmp2=qiz(k)*save1*rho(k)*pio6*rhowater*gambp6*oxmi* &
                   olambdar(k)**bp6
          piacr(k)=amin1( tmp2,qrzodt(k) )

!
1000      continue
!
          if(qsz(k) .le. 0.0) go to 1200
!
! Compute the following processes only when qsz > 0.0
!
!c
!c (6) ICE CRYSTAL ACCRETION BY SNOW (Psaci): Lin (22)
!c
          esi=exp( 0.025*temcc(k) )
          save1 = aa_s(k)*sqrho(k)*N0_s(k)* &
                  ggamma(bv_s(k)+tmp_sa(k))*olambdas(k)**(bv_s(k)+tmp_sa(k))

          tmp1=esi*save1
          psaci(k)=qizodt(k)*( 1.0-exp(-tmp1*dtb) )

!c
!c (7) CLOUD WATER ACCRETION BY SNOW (Psacw): Lin (24)
!c
          esw=1.0
          tmp1=esw*save1
          psacw(k)=qlzodt(K)*( 1.0-exp(-tmp1*dtb) )

!c
!c (8) DEPOSITION/SUBLIMATION OF SNOW (Psdep/Pssub): Lin (31)
!c     includes consideration of ventilation effect
!c
          tmpa=rvapor*xka(k)*tem(k)*tem(k)
          tmpb=xls*xls*rho(k)*qsiz(k)*diffwv(k)
          tmpc=tmpa*qsiz(k)*diffwv(k)
          abi=4.0*pi*cap_s(k)*(qvoqsiz(k)-1.0)*tmpc/(tmpa+tmpb)
          tmp1=av_s(k)*sqrho(k)*olambdas(k)**(5+bv_s(k)+2*mu_s)/visc(k)

!---- YLIN, here there is some approximation assuming mu_s =1, so gamma(2)=1, etc.

          tmp2= abi*N0_s(k)*( vf1s*olambdas(k)*olambdas(k)+ &
                vf2s*schmidt(k)**0.33334* &
                ggamma(2.5+0.5*bv_s(k)+mu_s)*sqrt(tmp1) )

          tmp3=odtb*( qvz(k)-qsiz(k) )
!
          if( tmp2 .le. 0.0) then
            tmp2=amax1( tmp2,tmp3)
            pssub(k)=amax1( tmp2,-qszodt(k) )
            psdep(k)=0.0
          else
            psdep(k)=amin1( tmp2,tmp3 )
            pssub(k)=0.0
          end if

!
          if(qrz(k) .le. 0.0) go to 1200
!
! Compute processes (9) and (10) only when qsz > 0.0 and qrz > 0.0
! these two terms need to be refined in the future, they should be equal
!c
!c (9) ACCRETION OF SNOW BY RAIN (Pracs): Lin (27)
!c
          esr=1.0
          tmpa=olambdar(k)*olambdar(k)
          tmpb=olambdas(k)*olambdas(k)
          tmpc=olambdar(k)*olambdas(k)
          tmp1=pi*pi*esr*xnor*N0_s(k)*abs( vtr(k)-vts(k) )*orho(k)
          tmp2=tmpb*tmpb*olambdar(k)*(5.0*tmpb+2.0*tmpc+0.5*tmpa)
          tmp3=tmp1*rhosnow*tmp2
          pracs(k)=amin1( tmp3,qszodt(k) )
          pracs(k)=0.0
!c
!c (10) ACCRETION OF RAIN BY SNOW (Psacr): Lin (28)
!c
          tmp3=tmpa*tmpa*olambdas(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
          tmp4=tmp1*rhowater*tmp3
          psacr(k)=amin1( tmp4,qrzodt(k) )
!
!c
!c (2) FREEZING OF RAIN TO FORM GRAUPEL  (pgfr): Lin (45), added to PI
!c     positive value
!c     Constant in Bigg freezing Aplume=Ap=0.66 /k
!c     Constant in raindrop freezing equ. Bplume=Bp=100./m/m/m/s
!

            if (qrz(k) .gt. 1.e-8 ) then
               Bp=100.
               Ap=0.66
               tmp1=olambdar(k)*olambdar(k)*olambdar(k)
               tmp2=20.*pi*pi*Bp*xnor*rhowater*orho(k)*  &
                    (exp(-Ap*temcc(k))-1.0)*tmp1*tmp1*olambdar(k)
               pgfr(k)=amin1( tmp2,qrzodt(k) )
            else
               pgfr(k)=0
            endif

1200      continue
!

        else                        

!
!***********************************************************************
!*********        snow production processes for T > 0 C       **********
!***********************************************************************
!
         if (qsz(k) .le. 0.0) go to 1400
!c
!c (1) CLOUD WATER ACCRETION BY SNOW (Psacw): Lin (24)
!c
            esw=1.0

            save1 =aa_s(k)*sqrho(k)*N0_s(k)* &
                   ggamma(bv_s(k)+tmp_sa(k))*olambdas(k)**(bv_s(k)+tmp_sa(k))

            tmp1=esw*save1
            psacw(k)=qlzodt(k)*( 1.0-exp(-tmp1*dtb) )

!c
!c (2) ACCRETION OF RAIN BY SNOW (Psacr): Lin (28)
!c
            esr=1.0
            tmpa=olambdar(k)*olambdar(k)
            tmpb=olambdas(k)*olambdas(k)
            tmpc=olambdar(k)*olambdas(k)
            tmp1=pi*pi*esr*xnor*N0_s(k)*abs( vtr(k)-vts(k) )*orho(k)
            tmp2=tmpa*tmpa*olambdas(k)*(5.0*tmpa+2.0*tmpc+0.5*tmpb)
            tmp3=tmp1*rhowater*tmp2
            psacr(k)=amin1( tmp3,qrzodt(k) )
!c
!c (3) MELTING OF SNOW (Psmlt): Lin (32)
!c     Psmlt is negative value
!
            delrs=rs0(k)-qvz(k)
            term1=2.0*pi*orho(k)*( xlv*diffwv(k)*rho(k)*delrs- &
                  xka(k)*temcc(k) )
            tmp1= av_s(k)*sqrho(k)*olambdas(k)**(5+bv_s(k)+2*mu_s)/visc(k)
            tmp2= N0_s(k)*( vf1s*olambdas(k)*olambdas(k)+ &
                  vf2s*schmidt(k)**0.33334* &
                  ggamma(2.5+0.5*bv_s(k)+mu_s)*sqrt(tmp1) )
            tmp3=term1*oxlf*tmp2-cwoxlf*temcc(k)*( psacw(k)+psacr(k) )
            tmp4=amin1(0.0,tmp3)
            psmlt(k)=amax1( tmp4,-qszodt(k) )
!c
!c (4) EVAPORATION OF MELTING SNOW (Psmltevp): HR (A27)
!c     but use Lin et al. coefficience
!c     Psmltevp is a negative value
!c
            tmpa=rvapor*xka(k)*tem(k)*tem(k)
            tmpb=xlv*xlv*rho(k)*qswz(k)*diffwv(k)
            tmpc=tmpa*qswz(k)*diffwv(k)
            tmpd=amin1( 0.0,(qvoqswz(k)-0.90)*qswz(k)*odtb )

            abr=2.0*pi*(qvoqswz(k)-0.90)*tmpc/(tmpa+tmpb)
!
!**** allow evaporation to occur when RH less than 90%
!**** here not using 100% because the evaporation cooling
!**** of temperature is not taking into account yet; hence,
!**** the qsw value is a little bit larger. This will avoid
!**** evaporation can generate cloud.
!
            tmp1=av_s(k)*sqrho(k)*olambdas(k)**(5+bv_s(k)+2*mu_s)/visc(k)
            tmp2= N0_s(k)*( vf1s*olambdas(k)*olambdas(k)+ &
                  vf2s*schmidt(k)**0.33334* &
                  ggamma(2.5+0.5*bv_s(k)+mu_s)*sqrt(tmp1) )
            tmp3=amin1(0.0,tmp2)
            tmp3=amax1( tmp3,tmpd )
            psmltevp(k)=amax1( tmp3,-qszodt(k) )
1400     continue
!
        end if      !---- end of snow/ice processes

!         CALL wrf_debug ( 100 , 'module_ylin: finish ice/snow processes' )


!***********************************************************************
!*********           rain production processes                **********
!***********************************************************************

!c
!c (1) AUTOCONVERSION OF RAIN (Praut): using Liu and Daum (2004)
!c

!---- YLIN, autoconversion use Liu and Daum (2004), unit = g cm-3 s-1, in the scheme kg/kg s-1, so

       if (qlz(k) .gt. 1e-6) then
           lamc(k) = (Nt_c*rhowater*pi*ggamma(4.+mu_c)/(6.*rho(k)*qlz(k))/ &        !--- N(D) = N0*D^mu*exp(-lamc*D)
                    ggamma(1+mu_c))**0.3333
           Dc_liu  = (ggamma(6+1+mu_c)/ggamma(1+mu_c))**(1./6.)/lamc(k)             !----- R6 in m

           if (Dc_liu .gt. R6c) then
             disp = 1./(mu_c+1.)      !--- square of relative dispersion
             eta  = (0.75/pi/(1e-3*rhowater))**2*1.9e11*((1+3*disp)*(1+4*disp)*&
                   (1+5*disp)/(1+disp)/(1+2*disp))
             praut(k) = eta*(1e-3*rho(k)*qlz(k))**3/(1e-6*Nt_c)                      !--- g cm-3 s-1
             praut(k) = praut(k)/(1e-3*rho(k))                                       !--- kg kg-1 s-1
           else
            praut(k) = 0.0
           endif
       else
         praut(k) = 0.0
       endif 

!c
!c (2) ACCRETION OF CLOUD WATER BY RAIN (Pracw): Lin (51)
!c
        erw=1.0

        tmp1=pio4*erw*xnor*av_r*sqrho(k)* &
             gambp3*olambdar(k)**bp3
        pracw(k)=qlzodt(k)*( 1.0-exp(-tmp1*dtb) )

!c
!c (3) EVAPORATION OF RAIN (Prevp): Lin (52)
!c     Prevp is negative value
!c
!c     Sw=qvoqsw : saturation ratio
!c
         tmpa=rvapor*xka(k)*tem(k)*tem(k)
         tmpb=xlv*xlv*rho(k)*qswz(k)*diffwv(k)
         tmpc=tmpa*qswz(k)*diffwv(k)
         tmpd=amin1(0.0,(qvoqswz(k)-0.90)*qswz(k)*odtb)

         abr=2.0*pi*(qvoqswz(k)-0.90)*tmpc/(tmpa+tmpb)
         tmp1=av_r*sqrho(k)*olambdar(k)**bp5/visc(k)
         tmp2=abr*xnor*( vf1r*olambdar(k)*olambdar(k)+  &
              vf2r*schmidt(k)**0.33334*gambp5o2*sqrt(tmp1) )
         tmp3=amin1( 0.0,tmp2 )
         tmp3=amax1( tmp3,tmpd )
         prevp(k)=amax1( tmp3,-qrzodt(k) )

!        CALL wrf_debug ( 100 , 'module_ylin: finish rain processes' )

!c
!c**********************************************************************
!c*****     combine all processes together and avoid negative      *****
!c*****     water substances
!***********************************************************************
!c
      if ( temcc(k) .lt. 0.0) then
!c
!c  combined water vapor depletions
!c
           tmp=psdep(k)
           if ( tmp .gt. qvzodt(k) ) then
              factor=qvzodt(k)/tmp
              psdep(k)=psdep(k)*factor
           end if
!c
!c  combined cloud water depletions
!c
           tmp=praut(k)+psacw(k)+psfw(k)+pracw(k)
           if ( tmp .gt. qlzodt(k) ) then
              factor=qlzodt(k)/tmp
              praut(k)=praut(k)*factor
              psacw(k)=psacw(k)*factor
              psfw(k)=psfw(k)*factor
              pracw(k)=pracw(k)*factor
           end if
!c
!c  combined cloud ice depletions
!c
           tmp=psaut(k)+psaci(k)+praci(k)+psfi(k)
           if (tmp .gt. qizodt(k) ) then
              factor=qizodt(k)/tmp
              psaut(k)=psaut(k)*factor
              psaci(k)=psaci(k)*factor
              praci(k)=praci(k)*factor
              psfi(k)=psfi(k)*factor
           endif
!c
!c  combined all rain processes
!c
          tmp_r=piacr(k)+psacr(k)-prevp(k)-praut(k)-pracw(k)+pgfr(k) 
          if (tmp_r .gt. qrzodt(k) ) then
             factor=qrzodt(k)/tmp_r
             piacr(k)=piacr(k)*factor
             psacr(k)=psacr(k)*factor
             prevp(k)=prevp(k)*factor
             pgfr(k)=pgfr(k)*factor
          endif
!c
!c   combined all snow processes
!c
          tmp_s=-pssub(k)-(psaut(k)+psaci(k)+psacw(k)+psfw(k)+pgfr(k)+ &
                 psfi(k)+praci(k)+piacr(k)+ &
                 psdep(k)+psacr(k)-pracs(k))
          if ( tmp_s .gt. qszodt(k) ) then
             factor=qszodt(k)/tmp_s
             pssub(k)=pssub(k)*factor
             Pracs(k)=Pracs(k)*factor
          endif

!c
!c  calculate new water substances, thetae, tem, and qvsbar
!c

         pvapor(k)=-pssub(k)-psdep(k)-prevp(k)
         qvz(k)=amax1( qvmin,qvz(k)+dtb*pvapor(k) )
         pclw(k)=-praut(k)-pracw(k)-psacw(k)-psfw(k)
         qlz(k)=amax1( 0.0,qlz(k)+dtb*pclw(k) )
         pcli(k)=-psaut(k)-psfi(k)-psaci(k)-praci(k)
         qiz(k)=amax1( 0.0,qiz(k)+dtb*pcli(k) )
         tmp_r=piacr(k)+psacr(k)-prevp(k)-praut(k)-pracw(k)+pgfr(k)-pracs(k) 
         prain(k)=-tmp_r
         qrz(k)=amax1( 0.0,qrz(k)+dtb*prain(k) )
         tmp_s=-pssub(k)-(psaut(k)+psaci(k)+psacw(k)+psfw(k)+pgfr(k)+  &
                psfi(k)+praci(k)+piacr(k)+  &
                psdep(k)+psacr(k)-pracs(k))
         psnow(k)=-tmp_s
         qsz(k)=amax1( 0.0,qsz(k)+dtb*psnow(k) )

         qschg(k)=qschg(k)+psnow(k)
         qschg(k)=psnow(k)

         tmp=ocp/tothz(k)*xLf*qschg(k)
         theiz(k)=theiz(k)+dtb*tmp
         thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
         tem(k)=thz(k)*tothz(k)

         temcc(k)=tem(k)-273.15

         if( temcc(k) .lt. -40.0 ) qswz(k)=qsiz(k)
         qlpqi=qlz(k)+qiz(k)
         if ( qlpqi .eq. 0.0 ) then
            qvsbar(k)=qsiz(k)
         else
            qvsbar(k)=( qiz(k)*qsiz(k)+qlz(k)*qswz(k) )/qlpqi
         endif

!
      else                  !>0 C
!c
!c  combined cloud water depletions
!c
          tmp=praut(k)+psacw(k)+pracw(k)
          if ( tmp .gt. qlzodt(k) ) then
             factor=qlzodt(k)/tmp
             praut(k)=praut(k)*factor
             psacw(k)=psacw(k)*factor
             pracw(k)=pracw(k)*factor
          end if
!c
!c  combined all snow processes
!c
          tmp_s=-(psmlt(k)+psmltevp(k))
          if (tmp_s .gt. qszodt(k) ) then
             factor=qszodt(k)/tmp_s
             psmlt(k)=psmlt(k)*factor
             psmltevp(k)=psmltevp(k)*factor
          endif
!c
!c  combined all rain processes
!c
          tmp_r=-prevp(k)-(praut(k)+pracw(k)+psacw(k)-psmlt(k)) 
          if (tmp_r .gt. qrzodt(k) ) then
             factor=qrzodt(k)/tmp_r
             prevp(k)=prevp(k)*factor
          endif
!c
!c  calculate new water substances and thetae
!c
          pvapor(k)=-psmltevp(k)-prevp(k)
          qvz(k)=amax1( qvmin,qvz(k)+dtb*pvapor(k))
          pclw(k)=-praut(k)-pracw(k)-psacw(k)
          qlz(k)=amax1( 0.0,qlz(k)+dtb*pclw(k) )
          pcli(k)=0.0
          qiz(k)=amax1( 0.0,qiz(k)+dtb*pcli(k) )
          tmp_r=-prevp(k)-(praut(k)+pracw(k)+psacw(k)-psmlt(k)) 
          prain(k)=-tmp_r
          tmpqrz=qrz(k)
          qrz(k)=amax1( 0.0,qrz(k)+dtb*prain(k) )
          tmp_s=-(psmlt(k)+psmltevp(k))
          psnow(k)=-tmp_s
          qsz(k)=amax1( 0.0,qsz(k)+dtb*psnow(k) )
          qschg(k)=psnow(k)
!
          tmp=ocp/tothz(k)*xLf*qschg(k)
          theiz(k)=theiz(k)+dtb*tmp
          thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)

          tem(k)=thz(k)*tothz(k)
          temcc(k)=tem(k)-273.15
          es=1000.*svp1*exp( svp2*temcc(k)/(tem(k)-svp3) )
          qswz(k)=ep2*es/(prez(k)-es)
          qsiz(k)=qswz(k)
          qvsbar(k)=qswz(k)
!
      end if
!      CALL wrf_debug ( 100 , 'module_ylin: finish sum of all processes' )

!
!***********************************************************************
!**********              saturation adjustment                **********
!***********************************************************************
!
!    allow supersaturation exits linearly from 0% at 500 mb to 50%
!    above 300 mb
!    5.0e-5=1.0/(500mb-300mb)
!
         rsat=1.0
         if( qvz(k)+qlz(k)+qiz(k) .lt. rsat*qvsbar(k) ) then

!c
!c   unsaturated
!c
          qvz(k)=qvz(k)+qlz(k)+qiz(k)
          qlz(k)=0.0
          qiz(k)=0.0

          thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
          tem(k)=thz(k)*tothz(k)
          temcc(k)=tem(k)-273.15

          go to 1800
!
        else
!c
!c   saturated
!c
          pladj(k)=qlz(k)
          piadj(k)=qiz(k)
!

        CALL satadj(qvz, qlz, qiz, prez, theiz, thz, tothz, kts, kte, &
                    k, xLvocp, xLfocp, episp0k, EP2,SVP1,SVP2,SVP3,SVPT0)

!
          pladj(k)=odtb*(qlz(k)-pladj(k))
          piadj(k)=odtb*(qiz(k)-piadj(k))
!
          pclw(k)=pclw(k)+pladj(k)
          pcli(k)=pcli(k)+piadj(k)
          pvapor(k)=pvapor(k)-( pladj(k)+piadj(k) )
!
          thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
          tem(k)=thz(k)*tothz(k)

          temcc(k)=tem(k)-273.15

          es=1000.*svp1*exp( svp2*temcc(k)/(tem(k)-svp3) )
          qswz(k)=ep2*es/(prez(k)-es)
          if (tem(k) .lt. 273.15 ) then
             es=1000.*svp1*exp( 21.8745584*(tem(k)-273.16)/(tem(k)-7.66) )
             qsiz(k)=ep2*es/(prez(k)-es)
             if (temcc(k) .lt. -40.0) qswz(k)=qsiz(k)
          else
             qsiz(k)=qswz(k)
          endif
          qlpqi=qlz(k)+qiz(k)
          if ( qlpqi .eq. 0.0 ) then
             qvsbar(k)=qsiz(k)
          else
             qvsbar(k)=( qiz(k)*qsiz(k)+qlz(k)*qswz(k) )/qlpqi
          endif

        end if

!
!***********************************************************************
!*****     melting and freezing of cloud ice and cloud water       *****
!***********************************************************************
        qlpqi=qlz(k)+qiz(k)
        if(qlpqi .le. 0.0) go to 1800
!
!c
!c (1)  HOMOGENEOUS NUCLEATION WHEN T< -40 C (Pihom)
!c
        if(temcc(k) .lt. -40.0) pihom(k)=qlz(k)*odtb
!c
!c (2)  MELTING OF ICE CRYSTAL WHEN T> 0 C (Pimlt)
!c
        if(temcc(k) .gt. 0.0) pimlt(k)=qiz(k)*odtb
!c
!c (3) PRODUCTION OF CLOUD ICE BY BERGERON PROCESS (Pidw): Hsie (p957)
!c     this process only considered when -31 C < T < 0 C
!c
        if(temcc(k) .lt. 0.0 .and. temcc(k) .gt. -31.0) then
!c!
!c!   parama1 and parama2 functions must be user supplied
!c!
          a1=parama1( temcc(k) )
          a2=parama2( temcc(k) )
!! change unit from cgs to mks
          a1=a1*0.001**(1.0-a2)
          xnin=xni0*exp(-bni*temcc(k))
          pidw(k)=xnin*orho(k)*(a1*xmnin**a2)
        end if
!
        pcli(k)=pcli(k)+pihom(k)-pimlt(k)+pidw(k)
        pclw(k)=pclw(k)-pihom(k)+pimlt(k)-pidw(k)
        qlz(k)=amax1( 0.0,qlz(k)+dtb*(-pihom(k)+pimlt(k)-pidw(k)) )
        qiz(k)=amax1( 0.0,qiz(k)+dtb*(pihom(k)-pimlt(k)+pidw(k)) )

!
        CALL satadj(qvz, qlz, qiz, prez, theiz, thz, tothz, kts, kte, &
                    k, xLvocp, xLfocp, episp0k ,EP2,SVP1,SVP2,SVP3,SVPT0)

        thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)
        tem(k)=thz(k)*tothz(k)

        temcc(k)=tem(k)-273.15

        es=1000.*svp1*exp( svp2*temcc(k)/(tem(k)-svp3) )
        qswz(k)=ep2*es/(prez(k)-es)

        if (tem(k) .lt. 273.15 ) then
           es=1000.*svp1*exp( 21.8745584*(tem(k)-273.16)/(tem(k)-7.66) )
           qsiz(k)=ep2*es/(prez(k)-es)
           if (temcc(k) .lt. -40.0) qswz(k)=qsiz(k)
        else
           qsiz(k)=qswz(k)
        endif
        qlpqi=qlz(k)+qiz(k)

        if ( qlpqi .eq. 0.0 ) then
           qvsbar(k)=qsiz(k)
        else
           qvsbar(k)=( qiz(k)*qsiz(k)+qlz(k)*qswz(k) )/qlpqi
        endif

1800  continue
!
!***********************************************************************
!**********    integrate the productions of rain and snow     **********
!***********************************************************************
!
2000  continue

!
!**** below if qv < qvmin then qv=qvmin, ql=0.0, and qi=0.0
!
      do k=kts+1,kte
         if ( qvz(k) .lt. qvmin ) then
            qlz(k)=0.0
            qiz(k)=0.0
            qvz(k)=amax1( qvmin,qvz(k)+qlz(k)+qiz(k) )
         end if
      enddo
!

!      CALL wrf_debug ( 100 , 'module_ylin: finish saturation adjustment' )

      END SUBROUTINE clphy1d_ylin




!---------------------------------------------------------------------
!                         SATURATED ADJUSTMENT
!---------------------------------------------------------------------
      SUBROUTINE satadj(qvz, qlz, qiz, prez, theiz, thz, tothz,      &
                        kts, kte, k, xLvocp, xLfocp, episp0k, EP2,SVP1,SVP2,SVP3,SVPT0)
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
!  This program use Newton's method for finding saturated temperature
!  and saturation mixing ratio.
!
! In this saturation adjustment scheme we assume
! (1)  the saturation mixing ratio is the mass weighted average of
!      saturation values over liquid water (qsw), and ice (qsi)
!      following Lord et al., 1984 and Tao, 1989
!
! (2) the percentage of cloud liquid and cloud ice will
!      be fixed during the saturation calculation
!---------------------------------------------------------------------
!

     INTEGER, INTENT(IN   )             :: kts, kte, k

     REAL,      DIMENSION( kts:kte ),                                   &
                       INTENT(INOUT) :: qvz, qlz, qiz
!
     REAL,      DIMENSION( kts:kte ),                                   &
                       INTENT(IN   ) :: prez, theiz, tothz

     REAL,     INTENT(IN   )            :: xLvocp, xLfocp, episp0k
     REAL,     INTENT(IN   )            :: EP2,SVP1,SVP2,SVP3,SVPT0

! LOCAL VARS

     INTEGER                            :: n

     REAL, DIMENSION( kts:kte )         :: thz, tem, temcc, qsiz,       &
                                        qswz, qvsbar

     REAL :: qsat, qlpqi, ratql, t0, t1, tmp1, ratqi, tsat, absft,    &
             denom1, denom2, dqvsbar, ftsat, dftsat, qpz,es             
!
!---------------------------------------------------------------------

      thz(k)=theiz(k)-(xLvocp*qvz(k)-xLfocp*qiz(k))/tothz(k)

      tem(k)=tothz(k)*thz(k)
      if (tem(k) .gt. 273.15) then
!        qsat=episp0k/prez(k)*  &
!            exp( svp2*(tem(k)-273.15)/(tem(k)-svp3) )
         es=1000.*svp1*exp( svp2*(tem(k)-svpt0)/(tem(k)-svp3) )
         qsat=ep2*es/(prez(k)-es)
      else
        qsat=episp0k/prez(k)*  &
             exp( 21.8745584*(tem(k)-273.15)/(tem(k)-7.66) )
      end if
      qpz=qvz(k)+qlz(k)+qiz(k)
      if (qpz .lt. qsat) then
         qvz(k)=qpz
         qiz(k)=0.0
         qlz(k)=0.0
         go to 400
      end if
      qlpqi=qlz(k)+qiz(k)
      if( qlpqi .ge. 1.0e-5) then
        ratql=qlz(k)/qlpqi
        ratqi=qiz(k)/qlpqi
      else
        t0=273.15
!       t1=233.15
        t1=248.15
        tmp1=( t0-tem(k) )/(t0-t1)
        tmp1=amin1(1.0,tmp1)
        tmp1=amax1(0.0,tmp1)
        ratqi=tmp1
        ratql=1.0-tmp1
      end if
!
!
!--  saturation mixing ratios over water and ice
!--  at the outset we will follow Bolton 1980 MWR for
!--  the water and Murray JAS 1967 for the ice
!
!-- dqvsbar=d(qvsbar)/dT
!-- ftsat=F(Tsat)
!-- dftsat=d(F(T))/dT
!
!  First guess of tsat

      tsat=tem(k)
      absft=1.0
!
      do 200 n=1,20
         denom1=1.0/(tsat-svp3)
         denom2=1.0/(tsat-7.66)
!        qswz(k)=episp0k/prez(k)*  &
!                exp( svp2*denom1*(tsat-273.15) )
         es=1000.*svp1*exp( svp2*denom1*(tsat-svpt0) )
         qswz(k)=ep2*es/(prez(k)-es)
         if (tem(k) .lt. 273.15) then
!           qsiz(k)=episp0k/prez(k)*  &
!                   exp( 21.8745584*denom2*(tsat-273.15) )
            es=1000.*svp1*exp( 21.8745584*denom2*(tsat-273.15) )
            qsiz(k)=ep2*es/(prez(k)-es)
            if (tem(k) .lt. 233.15) qswz(k)=qsiz(k)
         else
            qsiz(k)=qswz(k)
         endif
         qvsbar(k)=ratql*qswz(k)+ratqi*qsiz(k)
!
!        if( absft .lt. 0.01 .and. n .gt. 3 ) go to 300
         if( absft .lt. 0.01 ) go to 300
!
         dqvsbar=ratql*qswz(k)*svp2*243.5*denom1*denom1+  &
                 ratqi*qsiz(k)*21.8745584*265.5*denom2*denom2
         ftsat=tsat+(xlvocp+ratqi*xlfocp)*qvsbar(k)-  &
               tothz(k)*theiz(k)-xlfocp*ratqi*(qvz(k)+qlz(k)+qiz(k))
         dftsat=1.0+(xlvocp+ratqi*xlfocp)*dqvsbar
         tsat=tsat-ftsat/dftsat
         absft=abs(ftsat)

200   continue
9020  format(1x,'point can not converge, absft,n=',e12.5,i5)
300   continue

      if( qpz .gt. qvsbar(k) ) then
        qvz(k)=qvsbar(k)
        qiz(k)=ratqi*( qpz-qvz(k) )
        qlz(k)=ratql*( qpz-qvz(k) )
      else
        qvz(k)=qpz
        qiz(k)=0.0
        qlz(k)=0.0
      end if
400  continue

      END SUBROUTINE satadj


!----------------------------------------------------------------
     REAL FUNCTION parama1(temp)
!----------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------
!  This program calculate the parameter for crystal growth rate
!  in Bergeron process
!----------------------------------------------------------------

      REAL, INTENT (IN   )   :: temp
      REAL, DIMENSION(32)    :: a1
      INTEGER                :: i1, i1p1
      REAL                   :: ratio

      data a1/0.100e-10,0.7939e-7,0.7841e-6,0.3369e-5,0.4336e-5, &
              0.5285e-5,0.3728e-5,0.1852e-5,0.2991e-6,0.4248e-6, &
              0.7434e-6,0.1812e-5,0.4394e-5,0.9145e-5,0.1725e-4, &
              0.3348e-4,0.1725e-4,0.9175e-5,0.4412e-5,0.2252e-5, &
              0.9115e-6,0.4876e-6,0.3473e-6,0.4758e-6,0.6306e-6, &
              0.8573e-6,0.7868e-6,0.7192e-6,0.6513e-6,0.5956e-6, &
              0.5333e-6,0.4834e-6/

      i1=int(-temp)+1
      i1p1=i1+1
      ratio=-(temp)-float(i1-1)
      parama1=a1(i1)+ratio*( a1(i1p1)-a1(i1) )

      END FUNCTION parama1

!----------------------------------------------------------------
      REAL FUNCTION parama2(temp)
!----------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------
!  This program calculate the parameter for crystal growth rate
!  in Bergeron process
!----------------------------------------------------------------

      REAL, INTENT (IN   )   :: temp
      REAL, DIMENSION(32)    :: a2
      INTEGER                :: i1, i1p1
      REAL                   :: ratio

      data a2/0.0100,0.4006,0.4831,0.5320,0.5307,0.5319,0.5249, &
              0.4888,0.3849,0.4047,0.4318,0.4771,0.5183,0.5463, &
              0.5651,0.5813,0.5655,0.5478,0.5203,0.4906,0.4447, &
              0.4126,0.3960,0.4149,0.4320,0.4506,0.4483,0.4460, &
              0.4433,0.4413,0.4382,0.4361/
      i1=int(-temp)+1
      i1p1=i1+1
      ratio=-(temp)-float(i1-1)
      parama2=a2(i1)+ratio*( a2(i1p1)-a2(i1) )

      END FUNCTION parama2

!+---+-----------------------------------------------------------------+
! THIS FUNCTION CALCULATES THE LIQUID SATURATION VAPOR MIXING RATIO AS
! A FUNCTION OF TEMPERATURE AND PRESSURE
!
      REAL FUNCTION RSLF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESL,X
      REAL, PARAMETER:: C0= .611583699E03
      REAL, PARAMETER:: C1= .444606896E02
      REAL, PARAMETER:: C2= .143177157E01
      REAL, PARAMETER:: C3= .264224321E-1
      REAL, PARAMETER:: C4= .299291081E-3
      REAL, PARAMETER:: C5= .203154182E-5
      REAL, PARAMETER:: C6= .702620698E-8
      REAL, PARAMETER:: C7= .379534310E-11
      REAL, PARAMETER:: C8=-.321582393E-13

      X=MAX(-80.,T-273.16)

!      ESL=612.2*EXP(17.67*X/(T-29.65))
      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSLF=.622*ESL/(P-ESL)

      END FUNCTION RSLF
!
!+---+-----------------------------------------------------------------+
! THIS FUNCTION CALCULATES THE ICE SATURATION VAPOR MIXING RATIO AS A
! FUNCTION OF TEMPERATURE AND PRESSURE
!
      REAL FUNCTION RSIF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESI,X
      REAL, PARAMETER:: C0= .609868993E03
      REAL, PARAMETER:: C1= .499320233E02
      REAL, PARAMETER:: C2= .184672631E01
      REAL, PARAMETER:: C3= .402737184E-1
      REAL, PARAMETER:: C4= .565392987E-3
      REAL, PARAMETER:: C5= .521693933E-5
      REAL, PARAMETER:: C6= .307839583E-7
      REAL, PARAMETER:: C7= .105785160E-9
      REAL, PARAMETER:: C8= .161444444E-12

      X=MAX(-80.,T-273.16)
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSIF=.622*ESI/(P-ESI)

      END FUNCTION RSIF
!+---+-----------------------------------------------------------------+

!----------------------------------------------------------------
      REAL FUNCTION ggamma(X)
!----------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------
      REAL, INTENT(IN   ) :: x
      REAL, DIMENSION(8)  :: B
      INTEGER             ::j, K1
      REAL                ::PF, G1TO2 ,TEMP

      DATA B/-.577191652,.988205891,-.897056937,.918206857,  &
             -.756704078,.482199394,-.193527818,.035868343/

      PF=1.
      TEMP=X
      DO 10 J=1,200
      IF (TEMP .LE. 2) GO TO 20
      TEMP=TEMP-1.
   10 PF=PF*TEMP
!  100 FORMAT(//,5X,'module_mp_lin: INPUT TO GAMMA FUNCTION TOO LARGE, X=',E12.5)
!      WRITE(wrf_err_message,100)X
!      CALL wrf_error_fatal(wrf_err_message)
   20 G1TO2=1.
      TEMP=TEMP - 1.
      DO 30 K1=1,8
   30 G1TO2=G1TO2 + B(K1)*TEMP**K1
      ggamma=PF*G1TO2

      END FUNCTION ggamma

!----------------------------------------------------------------

      END MODULE module_mp_sbu_ylin

