!
 
!>\file module_mp_udm.F
!! 
!! This file is the module_mp_udm microphysics scheme.
!! author songyou Hong(songyou.hong@noaa.gov,songyouhong@gmail.com)
!! 
  module module_mp_udm
 
   use module_mp_radar
   use module_gfs_machine , only : kind_phys
!
!-------------------------------------------------------------------------------
! parameters for microphysical processes
!
   real, parameter, private :: &
          dtcldcr  = 180.  ,&    !< maximum time step for sub-cycling loops [s]
          n0r    = 8.e6    ,&    !< intercept parameter for rain              [m-4]
          n0s    = 2.e6    ,&    !< intercept parameter for snow at 0c        [m-4]
          n0g    = 4.e6    ,&    !< intercept parameter for graupel           [m-4]
          n0h    = 4.e4    ,&    !< intercept parameter hail 
          n0smax = 1.e11   ,&    !< maximum n0s (t=-90c unlimited)
          alpha  = .12     ,&    !< .122 exponent factor for n0s computation
          avtr   = 841.9   ,&    !< a constant for terminal velocity of rain
          bvtr   = 0.8     ,&    !< a constant for terminal velocity of rain
          avts   = 11.72   ,&    !< a constant for terminal velocity of snow
          bvts   = .41     ,&    !< a constant for terminal velocity of snow
          avtg   = 330.    ,&    !< a constant for terminal velocity of graupel 
          bvtg   = 0.8     ,&    !< a constant for terminal velocity of graupel 
          deng   = 500.    ,&    !< density of graupel                      [kgm-3]
          avth = 285.      ,&    !< a constant for terminal velocity of hail
          bvth = 0.8       ,&    !< a constant for terminal velocity of hail
          denh = 912.      ,&    !< density of hail                         [kgm-3]
          lamdasmax = 1.e5 ,&    !< maximum slope for snow       (10micro)    [m-1]
          lamdagmax = 2.e4 ,&    !< maximum slope for graupel    (50micro)    [m-1]
          lamdahmax = 2.e4 ,&    !< maximum slope for hail       (50micro)    [m-1]
          peaut  = .55     ,&    !< collection efficiency in autoconversion
          xncr   = 3.e8    ,&    !< number concentration of cloud droplets [m-3]
          xmyu   = 1.718e-5,&    !< the dynamic viscosity [kgm-1s-1]
          dicon  = 11.9    ,&    !< constant for the cloud-ice diamter
          dimax  = 500.e-6 ,&    !< maximum cloud-ice diamter [m]
          ni0max = 500.e3  ,&    !< maximum ice nuclei particles (inp) [m-3] 
          pfrz1  = 100.    ,&    !< constant in biggs freezing
          pfrz2  = 0.66    ,&    !< constant in biggs freezing
          qcmin  = 1.e-12  ,&    !< minimun values for qr, qs, and qg [kgkg-1]
          qrmin  = 1.e-9   ,&    !< minimun values for qr, qs, and qg [kgkg-1]
          eacrc  = 1.0     ,&    !< snow/cloud-water collection efficiency
          eachs = 1.0      ,&    !< hail/snow collection efficiency
          eachg = 0.5      ,&    !< hail/graupel collection efficiency
          cd    = 0.6      ,&    !< drag coefficient for hailstone
          dens  =  100.0   ,&    !< density of snow                         [kgm-3]
          qs0   =  6.e-4         !< threshold amount for aggretion to occur [kgkg-1]
!
   real, parameter, private :: &                                                   
          drcoeff = (24.)**(.3333333) ,& !< factor for raindrop diameter
          lamdarmax = 1.e5   ,&    !< maximum slope for rain [m-1] (28micro)     
          lamdarmin = 5.e2   ,&    !< minimum slope for rain [m-1] (5.8mm)    
          lamdacmax = 1.e6   ,&    !< maximum slope for cloud[m-1] (1micro)   
          lamdacmin = 1.e4   ,&    !< minimum slope for cloud[m-1] (100micro)   
          nrmin   = 1.e-6    ,&    !< nr minimum [m-3]                                             
          ncmin   = 1.e-3    ,&    !< nc minimum [m-3]                                              
          nrmax   = 2000.e3  ,&    !< nr maximum [m-3]                                              
          ncmax   = 2000.e6  ,&    !< nc maximum [m-3]                                              
          satmax  = 0.0048   ,&    !< supersatuation for ccn activation                                              
          actk    = 0.6      ,&    !< a constant for ccn activation                                              
          actr    = 1.5e-6   ,&    !< initial radius for activated cloud droplets [m]
          ncrk1   = 3.03e3   ,&    !< Long’s collection kernel coefficient [m-3s-1]                                              
          ncrk2   = 2.59e15  ,&    !< Long’s collection kernel coefficient [m-3s-1]                                              
          di5000  = 5000.e-6 ,&    !< diameter [m]                                              
          di2000  = 2000.e-6 ,&    !< diameter [m]                                              
          di1000  = 1000.e-6 ,&    !< diameter [m]                                               
          di600   = 600.e-6  ,&    !< diameter [m]                                              
          di100   = 100.e-6  ,&    !< diameter [m]                                               
          di82    = 82.e-6   ,&    !< diameter [m]                                               
          di50    = 50.e-6   ,&    !< diameter [m]                                               
          di15    = 15.e-6   ,&    !< diameter [m]                                               
          di2     = 2.e-6    ,&    !< diameter [m]                                               
          di1     = 1.e-6          !< diameter [m]                                               
   real, parameter, private :: &                                                   
          ccn_0    = 50.e+6  ,&    !< background ccn [m-3]
          ccnmax  = 20000.e+6,&    !< maximum ccn [m-3] 
          ccnmin  = 50.e+6         !< minimum ccn [m-3] 
!-------------------------------------------------------------------------------
! min max  effective radius (m)
!
   real, parameter, private :: &
          recmin  = 2.51e-6  ,&    !< minimum effective radius for cloud [m]
          recmax  = 50.e-6   ,&    !< maximum effective radius for cloud [m]
          reimin  = 5.01e-6  ,&    !< minimum effective radius for ice   [m]
          reimax  = 125.e-6  ,&    !< maximum effective radius for ice   [m]
          resmin  = 25.e-6   ,&    !< minimum effective radius for snow  [m]
          resmax  = 999.e-6        !< maximum effective radius for snow  [m]
!-------------------------------------------------------------------------------
! semi_lagrangian option sub-stepping (greater than 100 means single-loop)
!
   real, parameter, private :: &
          sedi_semi_cfl = 10000.0        
!-------------------------------------------------------------------------------
! inline expansion table for saturation vapor pressure fsvp_water and fsvp
!
!  use constant, only : cliq_,cice_,cvap_,cpd_,hvap_,hsub_,psat_,rd_,rv_,ttp_
!  
   integer, parameter               ::  nxsvp = 7501
   real                             ::  c1xsvp,c2xsvp
   real   , dimension(nxsvp)        ::  tbsvp
   integer, parameter               ::  nxsvpw = nxsvp
   real                             ::  c1xsvpw,c2xsvpw
   real   , dimension(nxsvpw)       ::  tbsvpw
!
   real, parameter, private :: &
         cliq_ = 4.1900e+3     ,&    !< specific heat of liquid water [jkg-1k-1]
         cice_ = 2.1060e+3     ,&    !< specific heat of ice water    [jkg-1k-1]
         cvap_ = 1.8700e+3     ,&    !< specific heat of vapor water  [jkg-1k-1]
         cpd_   = 1.00464e+3   ,&    !< specific heat of dry air      [jkg-1k-1] 
         hvap_ = 2.5010e+6     ,&    !< latent heat for vaporization     [jkg-1]
         hsub_ = 2.8340e+6     ,&    !< latent heat for sublimation      [jkg-1]
         psat_ = 6.1078e+2     ,&    !< saturated vapor pressure at 0c      [pa]
         rd_   = 2.8704e+2     ,&    !< gas constant for dry air [jkg-1k-1]
         rv_   = 4.6150e+2     ,&    !< gas constant for water vapor [jkg-1k-1]
         ttp_  = 2.7316e+2           !< temperature at triple point [k]
  real, parameter, private :: &
         psatk = psat_,                         &
         dldt  = cvap_-cliq_,                   &
         dldti = cvap_-cice_,                   &
         xa    = -dldt/rv_,                     &
         xb    = xa+hvap_/(rv_*ttp_),           &
         xai   = -dldti/rv_,                    &
         xbi   = xai+hsub_/(rv_*ttp_)
!-------------------------------------------------------------------------------
! inline expansion table for shape parameter dependent sedimentation
!
   integer, parameter    ::  nxmps = 9991 ! 10 micro to 10 mm every 1 micro
   real                             ::  c1xmps,c2xmps
   real   , dimension(nxmps)        ::  tbmps
!-------------------------------------------------------------------------------
! miscellaneous...
!
   real, parameter, private :: &
          zero_0 = 0.0                   ,&
          one_1 = 1.0
   logical, parameter, private :: flgzero = .true.
!-------------------------------------------------------------------------------
! save the microphysics constants that are initialzed in init driver
!
   real, save ::                                                    &
             qc_ocean,qc_land,qck1,pidnc,bvtr1,bvtr2,bvtr3,bvtr4,bvtr5,        &
             bvtr6,bvtr7, bvtr2o5,bvtr3o5,                                     &
             g1pbr,g2pbr,g3pbr,g4pbr,g5pbr,g6pbr,g7pbr,                        &
             g5pbro2,g7pbro2,pi,pisq,                                          &
             pvtr,pvtrn,eacrr,pacrr,pidn0r,pidnr,                              &
             precr1,precr2,xmmax,roqimax,bvts1,bvts2,                          &
             bvts3,bvts4,g1pbs,g3pbs,g4pbs,g5pbso2,                            &
             pvts,pacrs,precs1,precs2,pidn0s,xlv1,pacrc,                       &
             bvtg1,bvtg2,bvtg3,bvtg4,g1pbg,g3pbg,g4pbg,                        &
             g5pbgo2,pvtg,pacrg,precg1,precg2,pidn0g,                          &
             g6pbgh,precg3,bvth2,bvth3,bvth4,g3pbh,g4pbh,g5pbho2,pacrh,pvth,   &
             prech1,prech2,prech3,pidn0h,                                      &
             rslopehmax,rslopehbmax,rslopeh2max,rslopeh3max,                   &
             rslopecmax,rslopec2max,rslopec3max,                               &
             rslopermax,rslopesmax,rslopegmax,                                 &
             rsloperbmax,rslopesbmax,rslopegbmax,                              &
             rsloper2max,rslopes2max,rslopeg2max,                              &
             rsloper3max,rslopes3max,rslopeg3max
!
!-------------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------------
!
subroutine udm(th, q, qc, qr, qi, qs, qg, qh                                   &
                 ,nn, nc, nr                                                   &
                 ,den, pii, p, delz                                            &
                 ,delt,g, cpd, cpv, ccn0, rd, rv, t0c                          &
                 ,ep1, ep2, qmin                                               &
                 ,xls, xlv0, xlf0, den0, denr                                  &
                 ,cliq,cice,psat                                               &
                 ,xland                                                        &
                 ,rain, rainncv                                                &
                 ,snow, snowncv                                                &
                 ,hail, hailncv                                                &
                 ,sr                                                           &
                 ,refl_10cm, diagflag, do_radar_ref                            &
                 ,graupel, graupelncv                                          &
                 ,itimestep                                                    &
                 ,has_reqc, has_reqi, has_reqs                                 &  ! for radiation
                 ,re_cloud, re_ice,   re_snow                                  &  ! for radiation
                 ,ids,ide, jds,jde, kds,kde                                    &
                 ,ims,ime, jms,jme, kms,kme                                    &
                 ,its,ite, jts,jte, kts,kte                                    &
                                                                               )
!-------------------------------------------------------------------------------
  implicit none
!-------------------------------------------------------------------------------
!
  integer,           intent(in   ) :: itimestep
  integer,           intent(in   ) :: ids,ide, jds,jde, kds,kde ,     & 
                                      ims,ime, jms,jme, kms,kme ,     &
                                      its,ite, jts,jte, kts,kte
  real,              intent(in   ) :: delt, g, ccn0, rd, rv, t0c,     &
                                      den0, cpd, cpv, ep1, ep2,       & 
                                      qmin, xls, xlv0, xlf0, cliq,    &
                                      cice, psat, denr
  real, dimension( ims:ime , jms:jme ), intent(in) ::      xland
  real, dimension( ims:ime , kms:kme , jms:jme ),                 &
        intent(in   ) ::                                          &
                                                             den, &
                                                             pii, &
                                                               p, &
                                                            delz
  real, dimension( ims:ime , kms:kme , jms:jme ),                 &
        intent(inout) ::                                          &
                                                             th,  &
                                                              q,  &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              qg, &
                                                              qh
  real, dimension( ims:ime , kms:kme , jms:jme ),                 &
        intent(inout) ::                                          &
                                                             nn,  &
                                                             nc,  &
                                                             nr
  real, dimension( ims:ime , jms:jme ),                           &
        intent(inout) ::                                    rain, &
                                                         rainncv, &
                                                              sr
  real, dimension( ims:ime , jms:jme ), optional,                 &
        intent(inout) ::                                    snow, &
                                                         snowncv, &
                                                         graupel, &
                                                      graupelncv, &
                                                            hail, &
                                                         hailncv
!
! for radiation interation 
!
  integer, intent(in)::                                           &
                                                        has_reqc, &
                                                        has_reqi, &
                                                        has_reqs
  real, dimension( ims:ime, kms:kme, jms:jme ),                   &
        intent(inout)::                                           &
                                                        re_cloud, &
                                                          re_ice, &
                                                         re_snow
!-------------------------------------------------------------------------------
!
! local variables
!
  real, dimension( its:ite , kts:kte )    ::   t
  real, dimension( its:ite , kts:kte, 2 ) ::   qci
  real, dimension( its:ite , kts:kte, 4 ) ::   qrs, ncr
  integer                                 ::   i,j,k
!
! for reflectivlity
!
  real, dimension( kts:kte) :: qv1d, t1d, p1d, qr1d, nr1d, qs1d, qg1d, dbz
  real, dimension( kts:kte ) :: den1d, qc1d, nc1d, qi1d
  real, dimension( kts:kte ) :: re_qc, re_qi, re_qs
  integer,                optional, intent(in)    :: do_radar_ref
  logical,                optional, intent(in)    ::     diagflag 
  real, dimension( ims:ime, kms:kme, jms:jme ),                   &
                          optional, intent(inout) ::    refl_10cm
!
!-------------------------------------------------------------------------------
!
   if (itimestep==1) then                            
     do j = jms,jme                                      
       do k = kms,kme                                    
         do i = ims,ime                                  
           nn(i,k,j) = ccn0                              
         enddo                                           
       enddo                                             
     enddo                                               
   endif                                                 
      do j=jts,jte
        do k=kts,kte
          do i=its,ite
            t(i,k)=th(i,k,j)*pii(i,k,j)
            qci(i,k,1) = qc(i,k,j)
            qci(i,k,2) = qi(i,k,j)
            qrs(i,k,1) = qr(i,k,j)
            qrs(i,k,2) = qs(i,k,j)
            qrs(i,k,3) = qg(i,k,j)
!#ifdef 
            qrs(i,k,4) = qh(i,k,j)
!#endif
!#ifdef 
            ncr(i,k,1) = nn(i,k,j)
            ncr(i,k,2) = nc(i,k,j)
            ncr(i,k,3) = nr(i,k,j)
!#endif
          enddo
        enddo
!
         call udm2d(t, q(ims,kms,j), qci, qrs                     &
                    ,ncr                                           &
                    ,den(ims,kms,j)                                &
                    ,p(ims,kms,j), delz(ims,kms,j)                 &
                    ,delt,g, cpd, cpv, rd, rv, t0c                 &
                    ,ep1, ep2, qmin                                &
                    ,xls, xlv0, xlf0, den0, denr                   &
                    ,cliq,cice,psat                                &
                    ,j                                             &
                    ,xland(ims,j)                                  &
                    ,rain(ims,j),rainncv(ims,j)                    &
                    ,sr(ims,j)                                     &
                    ,ids,ide, jds,jde, kds,kde                     &
                    ,ims,ime, jms,jme, kms,kme                     &
                    ,its,ite, jts,jte, kts,kte                     &
                    ,snow(ims,j),snowncv(ims,j)                    &
                    ,graupel(ims,j),graupelncv(ims,j)              &
                    ,hail(ims,j),hailncv(ims,j)                    &
                                                                   )
        do k=kts,kte
          do i=its,ite
            th(i,k,j)=t(i,k)/pii(i,k,j)
            qc(i,k,j) = qci(i,k,1)
            qi(i,k,j) = qci(i,k,2)
            qr(i,k,j) = qrs(i,k,1)
            qs(i,k,j) = qrs(i,k,2)
            qg(i,k,j) = qrs(i,k,3)
!#ifdef 
            qh(i,k,j) = qrs(i,k,4)
!#endif
!#ifdef 
            nn(i,k,j) = ncr(i,k,1)
            nc(i,k,j) = ncr(i,k,2)
            nr(i,k,j) = ncr(i,k,3)
!#endif
          enddo
        enddo
!-------------------------------------------------------------------------------
        if ( present (diagflag) ) then
          if (diagflag .and. do_radar_ref==1) then
            do i=its,ite
               do k=kts,kte
                  t1d(k)=th(i,k,j)*pii(i,k,j)
                  p1d(k)=p(i,k,j)
                  qv1d(k)=q(i,k,j)
                  qr1d(k)=qr(i,k,j)
                  qs1d(k)=qs(i,k,j)
                  qg1d(k)=qg(i,k,j)
                  nr1d(k) = nr(i,k,j)
               enddo
               call ufs_mp_reflectivity (qv1d, qr1d, qs1d, qg1d,       &
                       nr1d,                                           &
                       t1d, p1d, dbz, kts, kte, i, j)
               do k = kts, kte
                  refl_10cm(i,k,j) = max(-35., dbz(k))
               enddo
            enddo
          endif
        endif
!
        if (has_reqc.ne.0 .and. has_reqi.ne.0 .and. has_reqs.ne.0) then
          do i=its,ite
            do k=kts,kte
              re_qc(k) = recmin
              re_qi(k) = reimin
              re_qs(k) = resmin
              t1d(k)  = th(i,k,j)*pii(i,k,j)
              qc1d(k) = qc(i,k,j)
              qi1d(k) = qi(i,k,j)
              qs1d(k) = qs(i,k,j)
              nc1d(k) = nc(i,k,j)
            enddo
            call ufs_mp_effective_radius(t1d,qc1d,qi1d,qs1d,den1d,qmin, t0c    & 
                               ,nc1d                                           &
                               ,re_qc, re_qi, re_qs, kts, kte, i, j)
            do k=kts,kte
              re_cloud(i,k,j) = max(recmin, min(re_qc(k), recmax))
              re_ice(i,k,j)   = max(reimin, min(re_qi(k), reimax))
              re_snow(i,k,j)  = max(resmin, min(re_qs(k), resmax))
            enddo
          enddo
        endif     ! has_reqc, etc...
!-------------------------------------------------------------------------------
      enddo
end subroutine udm
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
subroutine udm2d(t1, q1                                           & 
                   ,qci1, qrs1                                    &
                   ,ncr1                                          &
                   ,den1, p1, delz1                               &
                   ,delt,g, cpd, cpv, rd, rv, t0c                 &
                   ,ep1, ep2, qmin                                &
                   ,xls, xlv0, xlf0, den0, denr                   &
                   ,cliq,cice,psat                                &
                   ,lat                                           &
                   ,slmsk                                         &
                   ,rain,rainncv                                  &
                   ,sr                                            &
                   ,ids,ide, jds,jde, kds,kde                     &
                   ,ims,ime, jms,jme, kms,kme                     &
                   ,its,ite, jts,jte, kts,kte                     &
                   ,snow,snowncv                                  &
                   ,graupel,graupelncv                            &
                   ,hail,hailncv                                  &
                                                                  )
!                                                                                
!-------------------------------------------------------------------------------
!===============================================================================             
!! unified forecast system (ufs) double moment microphyiscs scheme (udm mp) 
!! utilizes the in-cloud microphysics concept (kim and hong 2018)      
!! udm mp largely adopts ice microphysical processes from hong et al.(2004) 
!! and double moment warm microphysics from lim and hong (2010), but with 
!! bug fixes or revisions based on literature and accumulated realism. 
!! semi-lagrangian sedimentation of juang and hong (2010) is also re-configured
!! for computational efficiency and numerical accuracy. all productions terms        
!! are optimized by introducing cloud top definition for hydrometeors.           
!!                                                                               
!! all units are in m.k.s. and source/sink terms in kgkg-1s-1.                   
!!                                                                               
!!  udm mp scheme, developed by songyou hong, haiqin li, jian-wen bao (noaa)  
!!         jimy dhdhia (ncar), spring, summer, fall, and winter 2021-2024
!!  wdm mp scheme, coded by kyo-sun lim and songyou hong (yonsei univ.),
!!         fall 2008
!!  wsm mp scheme, coded by songyou hong, jeong-ock jade lim (yonsei univ.),
!!         jimy dudhia, and shu-hua chen (ncar),summer 2002
!!  ncep mp scheme, coded by songyou hong, henry juang, qingyun zhao (ncep),
!!         summer 1997
!!  references :
!!             kim and and hong (kh, 2018) j. atmos. sci.
!!             juang and hong (jh, 2010) mon. wea. rev.
!!             lim and hong (lh, 2010) mon. wea. rev.
!!             dudhia, hong and lim (dhl, 2008) j. meteor. soc. japan
!!             hong and lim (hl, 2006) j. korean meteor. soc.
!!             hong, dudhia, and chen (hdc, 2004) mon. wea. rev.
!!             hong, juang, and zhao (hjz, 1998) mon. wea. rev.
!!
!! \section structure
!!            --- udm_mp  ---  |- slope_rain slope_snow slope_graupel slope_hail
!!                             |
!!                             |- semi_lagrange_sedim
!!                             |
!!                             |- cldf_mps_diag
!!                             |
!!                             |- ufsmpinit
!!                             |    |
!!                             |    ------ rgmma
!!                             |
!!                             |- adjust_number_concent
!!                             |
!!                             |- find_cloud_top
!!                             |
!!                             |- ufs_mp_effective_radius
!!                             |
!!                             |- ufs_mp_reflectivity
!!                             |
!!                             |- funct_mps_setup
!!                             |    |
!!                             |    ------ fmpsx, fmps
!!                             |
!!                             |- funct_svp_setup
!!                             |    |
!!                             |    ------ fsvpx, fsvpxw, fsvp, fsvp_water
!!                             |
!! \ section input and output
!!  input :
!!    delt                         - timestep
!!    g, cpd, cpv, t0c, den0,     - constant
!!    rd, rv, ep1, ep2, qmin,
!!    xls, xlv0, xlf0, denr,
!!    cliq, cice, psat
!!    ids, ide, jds, jde, kds, kde - dimension
!!    ims, ime, jms, jme, kms, kme
!!    its, ite, jts, jte, kts, kte
!!    ncloud                       - number of hydrometeor
!!    p                            - pressure, pa
!!    delz                         - depth of model layer, m
!!
!!  inout :
!!    t1                           - temperautre
!!    q1                           - specific humidity
!!    q2                           - mixing ratio of cloud, rain, ice, and snow
!!                                   qc, qr, qi, qc
!!    rain, rainncv                - precipitation
!!    sr                           - ratio of snow to rain
!!
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
! passing variables
!
   integer                                , intent(in   ) ::                   &
                                                    ids,ide, jds,jde, kds,kde, &
                                                    ims,ime, jms,jme, kms,kme, &
                                                    its,ite, jts,jte, kts,kte
   real                        , intent(in   ) :: delt
   real                        , intent(in   ) :: g, cpd, cpv, t0c,&
                                                             rd, rv, ep1, ep2
   real, dimension( ims:ime )   , intent(in   ) :: slmsk
   real                        , intent(in   ) ::                   &
                                                             qmin, xls,        &
                                                             xlv0, xlf0,       &
                                                             den0, denr,       &
                                                             cliq, cice, psat
   integer                                , intent(in   ) :: lat
   real, dimension( ims:ime,kms:kme ), intent(in   ) :: p1
   real, dimension( ims:ime,kms:kme ), intent(in   ) :: delz1
   real, dimension( ims:ime,kms:kme ), intent(inout) :: q1
   real, dimension( its:ite,kts:kte ), intent(inout) :: t1
   real, dimension( ims:ime,kms:kme )  , intent(in   ) :: den1
   real, dimension( its:ite,kts:kte,2 ), intent(inout) :: qci1
   real, dimension( its:ite,kts:kte,4 ), intent(inout) :: qrs1
   real, dimension( its:ite,kts:kte,3 ), intent(inout) :: ncr1
   real, dimension( ims:ime )               , intent(inout) :: rain
   real, dimension( ims:ime ), optional     , intent(inout) :: snow
   real, dimension( ims:ime ), optional     , intent(inout) :: graupel
   real, dimension( ims:ime ), optional     , intent(inout) :: hail
   real, dimension( ims:ime )               , intent(inout) :: rainncv
   real, dimension( ims:ime )               , intent(inout) :: sr
   real, dimension( ims:ime ), optional     , intent(inout) :: snowncv
   real, dimension( ims:ime ), optional     , intent(inout) :: graupelncv
   real, dimension( ims:ime ), optional     , intent(inout) :: hailncv
!
! local variables
!
   real, dimension( its:ite )                           :: dxmeter
   real, dimension( kts:kte , its:ite ) ::                &
                                                                q, &
                                                              den, &
                                                                t, &
                                                                p, &
                                                             delz, &
                                                             dend, &
                                                           denfac, &
                                                              xlv, &
                                                              xlf, &
                                                              cpm
   real, dimension( kts:kte , its:ite , 2) ::             &
                                                              qci
   real, dimension( kts:kte , its:ite , 4) ::             &
                                                              qrs
   real, dimension( kts:kte, 4)  ::                      &
                                                           rslope, &
                                                          rslope2, &
                                                          rslope3, &
                                                          rslopeb
   real, dimension( kts:kte , its:ite , 3) ::         ncr
   real, dimension( kts:kte ) ::                         &
                                                           rh_mul, &
                                                           rh_ice, &
                                                         qsat_mul, &
                                                         qsat_ice, &
                                                              vtr, &
                                                              vts, &
                                                              vtg, &
                                                              vth, &
                                                           vtmean, &
                                                           sumice, &
                                                              vti, &
                                                               ni, &
                                                            denqr, &
                                                            denqs, &
                                                            denqg, &
                                                            denqh, &
                                                               di, &
                                                            denqi, &
                                                           n0sfac, &
                                                           diffus, &
                                                           viscok, &
                                                           viscod, &
                                                           ab_mul, &
                                                           ab_ice, &
                                                           venfac
   real, dimension( kts:kte ) ::                         &
                                                          rslopec, &
                                                         rslopec2, &
                                                         rslopec3, &
                                                               dc, &
                                                               dr, &
                                                            denqn
   real, dimension( kts:kte ) ::                         &
                                                            pigen, &
                                                            pcond, &
                                                            prevp, &
                                                            psevp, &
                                                            pgevp, &
                                                            pidep, &
                                                            psdep, &
                                                            pgdep, &
                                                            phdep, &
                                                            praut, &
                                                            psaut, &
                                                            pgaut, &
                                                            piacr, &
                                                            pracw, &
                                                            praci, &
                                                            pracs, &
                                                            psacw, &
                                                            psaci, &
                                                            psacr, &
                                                            phaut, &
                                                            phacr, &
                                                            phacs, &
                                                            phacg, &
                                                            phaci, &
                                                            phmlt, &
                                                            pheml, &
                                                            phevp, &
                                                            primh, &
                                                            pvapg, &
                                                            pvaph, &
                                                            pgwet, &
                                                            phwet, &
                                                          pgaci_w, &
                                                          phaci_w, &
                                                            nhacw, &
                                                            nhacr, &
                                                            nheml, &
                                                            pgacw, &
                                                            phacw, &
                                                            pracg, &
                                                            pgaci, &
                                                            pgacr, &
                                                            pgacs, &
                                                            paacw, &
                                                            psmlt, &
                                                            pgmlt, &
                                                            pseml, &
                                                            pgeml
   real, dimension( kts:kte ) ::                         &
                                                            pcact, &
                                                            nraut, &
                                                            nracw, &
                                                            ncevp, &
                                                            nccol, &
                                                            nrcol, &
                                                            nsacw, &
                                                            ngacw, &
                                                            niacr, &
                                                            nsacr, &
                                                            ngacr, &
                                                            naacw, &
                                                            nseml, &
                                                            ngeml, &
                                                            ncact
   real, dimension( kts:kte ) ::                         &
                                                           satrdt, &
                                                           supice, &
                                                           supsat, &
                                                           tcelci, &
                                                             cldf
   real ::                                              &
                                                           qrpath, &
                                                           qspath, &
                                                           qgpath, &
                                                           qipath, &
                                                         precip_r, &
                                                         precip_s, &
                                                         precip_g, &
                                                         precip_i
   logical, dimension( kts:kte ) ::                                 &
                                                            ifsat, &
                                                            ifice
!
! miscellaneous variables
! 
   real  ::                                             &
             rdtcld,conden, x, y, z, a, b, c, d, e,                &
             coeres, dtcld, mi, eacrs, acrfac, egi,                &
             ehi, rs0, ghw1, ghw2, ghw3, ghw4, precip_h, qhpath,   &
             qimax, ni0, roqi0, lamdar, diameter,                  &
             precip_sum, precip_ice, factor, source, htotal,       &
             hvalue, pfrzdtc, pfrzdtr,                             &
             tstepsnow, tstepgraupel, tstephail,                   &
             alpha2, delta2, delta3, dtcfl, dr_embryo,temp 
   real  :: gfac, sfac, nfrzdtr, nfrzdtc, qnpath
   real, dimension( kts:kte ) :: qrconcr, qrcon, taucon
   real, dimension( kts:kte ) :: vtn
   integer :: nstep, niter, lond, latd,                            &
              i, j, k, loop, loops, n, idim, kdim
   integer :: ktopini, ktopmax, ktop
   integer :: ktopqc, ktopqi, ktopqr, ktopqs, ktopqg, ktopqh, ktoprh
   real, dimension( kts:kte ) :: tmp1d
   logical :: flgcld, lqc, lqi, lqr, lqs, lqg, lqh
!-------------------------------------------------------------------------------
! preparation ....
!
   idim = ite-its+1
   kdim = kte-kts+1
   ktopini = kte - 1
   lond = (ite-its)/2 + 1
   latd = 1
   dxmeter(:) = 10000.
   dr_embryo =  1./(pi*denr/6.*di82**3)
!-------------------------------------------------------------------------------
! assign passing variables to local arrays 
!
   t(:,:) = 0.; q(:,:) = 0.; p(:,:) =0.
   delz(:,:) = 0.; den(:,:) = 0.; dend(:,:) =0.; denfac(:,:) = 0.
   do k = kts, kte
     do i = its, ite
       t(k,i) = t1(i,k)
       q(k,i) = q1(i,k)
       p(k,i) = p1(i,k)
       delz(k,i) = delz1(i,k)
       den(k,i) = den1(i,k)
       dend(k,i) = (p(k,i)/t(k,i)-den(k,i)*rv)/(rd-rv)  ! dry density
       denfac(k,i) = sqrt(den0/den(k,i))
     enddo
   enddo
!-------------------------------------------------------------------------------
! padding 0 for negative values generated by dynamics - optional 
!
   qci(:,:,:) = qmin
   qrs(:,:,:) = qmin
   ncr(:,:,:) = qmin
!
   ktop = ktopini
   do k = kts, kte
     do i = its, ite
       if(flgzero) then
         qci(k,i,1) = max(qci1(i,k,1),0.0)
         qci(k,i,2) = max(qci1(i,k,2),0.0)
         qrs(k,i,1) = max(qrs1(i,k,1),0.0)
         qrs(k,i,2) = max(qrs1(i,k,2),0.0)
         qrs(k,i,3) = max(qrs1(i,k,3),0.0)
         qrs(k,i,4) = max(qrs1(i,k,4),0.0)
         ncr(k,i,1) = min(max(ncr1(i,k,1),ccnmin),ccnmax)
         ncr(k,i,2) = max(ncr1(i,k,2),0.0)
         ncr(k,i,3) = max(ncr1(i,k,3),0.0)
       else
         qci(k,i,1) = qci1(i,k,1)
         qci(k,i,2) = qci1(i,k,2)
         qrs(k,i,1) = qrs1(i,k,1)
         qrs(k,i,2) = qrs1(i,k,2)
         qrs(k,i,3) = qrs1(i,k,3)
         qrs(k,i,4) = qrs1(i,k,4)
         ncr(k,i,1) = min(max(ncr1(i,k,1),ccnmin),ccnmax)
         ncr(k,i,2) = ncr1(i,k,2)
         ncr(k,i,3) = ncr1(i,k,3)
       endif
     enddo
   enddo
!-------------------------------------------------------------------------------
! initialize the surface rain, snow, graupel and etc....
!
   do i = its, ite
     if(present (snowncv) .and. present (snow)) snowncv(i) = 0.
     if(present (graupelncv) .and. present (graupel)) graupelncv(i) = 0.
     if(present (hailncv) .and. present (hail)) hailncv(i) = 0.
   enddo
   rainncv(:) = 0.;  sr(:) = 0.
   cpm(:,:) = 0.; xlv(:,:) = 0.; xlf(:,:) = 0.
!-------------------------------------------------------------------------------
! latent heat for phase changes and heat capacity. emanuel(1994)
!
   do k = kts, ktop
     do i = its, ite
       cpm(k,i) = cpd*(1.-max(q(k,i),qmin)) + max(q(k,i),qmin)*cpv
       xlv(k,i) = xlv0 - xlv1*(t(k,i)-t0c)
       xlf(k,i) = xls - xlv(k,i)
     enddo
   enddo
!===============================================================================
!
! inner loop wih the time step of dtcldcr (default = 180 sec)
!
!===============================================================================
! compute the sub-cycling time steps.
!
   loops = max(ceiling(delt/dtcldcr),1)
   dtcld = delt/loops
   if(delt<=dtcldcr) dtcld = delt
   rdtcld = 1./dtcld
!
!-------------------------------------------------------------------------------
!
 inner_loop : do loop = 1, loops ! sub-stepping with cld timestep = 180s
!
!-------------------------------------------------------------------------------
!
 i_loop : do i = its, ite ! i-loop for one-dimensional code
!
!-------------------------------------------------------------------------------
!
   qsat_mul(:) = 0.; rh_mul(:)   = 0.
   qsat_ice(:) = 0.; rh_ice(:)   = 0.
   ktopqc = 0; ktopqi = 0; ktopqr = 0; ktopqs = 0; ktopqg = 0; ktoprh = 0
   ktopqh = 0
!   
   ktop = ktopini
   do k = kts, ktop
     qsat_mul(k) = fsvp_water(t(k,i),p(k,i))      !< mul means water in korean
     qsat_mul(k) = ep2 * qsat_mul(k) / (p(k,i) - qsat_mul(k))
     qsat_mul(k) = max(qsat_mul(k),qmin)
     rh_mul(k)   = max(q(k,i) / qsat_mul(k),qmin)
     qsat_ice(k) = fsvp(t(k,i),p(k,i))
     qsat_ice(k) = ep2 * qsat_ice(k) / (p(k,i) - qsat_ice(k))
     qsat_ice(k) = max(qsat_ice(k),qmin)
     rh_ice(k)   = max(q(k,i) / qsat_ice(k),qmin)
   enddo
!===============================================================================
! compute internal functions
! optimizatin : A**B => exp(log(A)*(B))
!-------------------------------------------------------------------------------
! x : temperature, y: pressure, z: density
! diffus: diffusion coefficient of the water vapr ! 8.794e-5*x**1.81/y
!         diffus(x,y) = 8.794e-5 * exp(log(x)*(1.81)) / y   
! viscok: kinematic viscosity(m2s-1)              ! 1.496e-6*x**1.5/(x+120.)/z
!         viscok(x,z) = 1.496e-6 * (x*sqrt(x)) /(x+120.)/z 
! viscod: dynamic viscosity(kgm-1s-1)             ! 1.414e3*viscok*z
!         viscod(x,z) = 1.414e3*viscok(x,z)*z 
! ab: thermodynamic terms, a and b in the denominator associated with
!             heat conduction and vapor diffusion
!         ab(a,b,c,d,e) = d*a*a/(viscod(c,d)*rv*c*c)+1./(e*diffus(c,b))
! venfac: parameter associated with the ventilation effects 
!
   do k = ktop, kts, -1
     diffus(k) = (8.794e-5*exp(log(t(k,i))*(1.81)))/p(k,i)
     viscok(k) = (1.496e-6*(t(k,i)*sqrt(t(k,i))))/(t(k,i)+120.)/den(k,i) 
     viscod(k) = 1.414e3*viscok(k)*den(k,i) 
     ab_mul(k) = ((den(k,i)*xlv(k,i)*xlv(k,i)))/(viscod(k)*(rv*t(k,i)*t(k,i)))  & 
               + 1./(qsat_mul(k)*diffus(k))
     ab_ice(k) = ((den(k,i)*xls*xls))/(viscod(k)*(rv*t(k,i)*t(k,i)))            &
               + 1./(qsat_ice(k)*diffus(k))
     venfac(k) = (exp(.3333333*log(viscok(k)/(diffus(k))))                      &
               * sqrt(sqrt(den0/(den(k,i)))))/sqrt(viscok(k))
   enddo 
!-------------------------------------------------------------------------------
! re-define cloud top for numerical efficiencies
!
   lqc = .false.
   lqi = .false.
   lqr = .false.
   lqs = .false.
   lqg = .false.
   lqh = .false.
   flgcld = .false.
   call find_cloud_top(1,kdim,ktopini,qci(:,i,1),zero_0,ktopqc)
   call find_cloud_top(1,kdim,ktopini,qci(:,i,2),zero_0,ktopqi)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,1),zero_0,ktopqr)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,2),zero_0,ktopqs)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,3),zero_0,ktopqg)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,4),zero_0,ktopqh)
   call find_cloud_top(1,kdim,ktopini,rh_ice(:), one_1,ktoprh)
   if(ktopqc>0.0) lqc = .true.
   if(ktopqi>0.0) lqi = .true.
   if(ktopqr>0.0) lqr = .true.
   if(ktopqs>0.0) lqs = .true.
   if(ktopqg>0.0) lqg = .true.
   if(ktopqh>0.0) lqh = .true.
   ktopmax = max(ktopqc,ktopqi,ktopqr,ktopqs,ktopqg,ktopqh,ktoprh)
!-------------------------------------------------------------------------------
! early checkout 
!
   if(lqc .or. lqi .or. lqr .or. lqs .or. lqg .or. lqh) flgcld = .true.
   if((.not.flgcld) .and. ktoprh>0.0) flgcld = .true.
   if(.not.flgcld) then
     cycle i_loop
   endif
!-------------------------------------------------------------------------------
! initialize the local variables 
!
   temp = 0.         ; hvalue = 0.       ; htotal = 0. ; source = 0.
   pcond(:) = 0.     ; pigen(:) = 0. 
   praut(:) = 0.     ; psaut(:) = 0.     ; pgaut(:) = 0.
   pidep(:) = 0.     ; psdep(:) = 0.     ; pgdep(:) = 0. 
   pracw(:) = 0.     ; praci(:) = 0.     ; pracs(:) = 0.
   psacw(:) = 0.     ; psaci(:) = 0.     ; psacr(:) = 0.
   pgacw(:) = 0.     ; pgaci(:) = 0.     ; pgacr(:) = 0. ; pgacs(:) = 0. 
   paacw(:) = 0.     ; piacr(:) = 0.    
   psmlt(:) = 0.     ; pgmlt(:) = 0.     ; pseml(:) = 0. ; pgeml(:) = 0.
   prevp(:) = 0.     ; psevp(:) = 0.     ; pgevp(:) = 0.
   denqr(:) = 0.     ; denqs(:) = 0.     ; denqi(:) = 0.
   vtr(:)   = 0.     ; vts(:)   = 0.     ; vtg(:)   = 0. ; vti(:)   = 0.
   qrpath   = 0.     ; qspath   = 0.     ; qgpath   = 0. ; qipath   = 0.
   precip_r = 0.     ; precip_s = 0.     ; precip_g = 0. ; precip_i = 0.
   satrdt(:)= 0.     ; supsat(:)= 0.     ; tcelci(:)= 0. ; supice(:)= 0.
   ifsat(:) = .false.; ifice(:) =.false.
   ni(:)   = 1.e3    ; cldf(:) = 1.    
   sumice(:) = 0.    ; vtmean(:) = 0.0   ; n0sfac(:) = 0.
   tstepsnow = 0.    ; tstepgraupel = 0. ; tstephail = 0.
   lamdar = 0.       ; diameter = 0.     ; eacrs = 0.  ; egi =0.
   roqi0 = 0.        ; acrfac = 0.       ; ni0 = 0. 
   qimax = 0.        ; alpha2 = 0.       ; coeres = 0. 
   pfrzdtc = 0.      ; pfrzdtr = 0.      
   pcact(:) = 0.     ; nsacw(:) = 0.     ;  ngacw(:) = 0.  ; naacw(:) = 0.
   niacr(:) = 0.     ; nsacr(:) = 0.     ;  ngacr(:) = 0.  ; nseml(:) = 0.
   ngeml(:) = 0.     ; nracw(:) = 0.     ;  nccol(:) = 0.  ; nrcol(:) = 0.
   ncact(:) = 0.     ; nraut(:) = 0.     ;  ncevp(:) = 0.  ; denqn(:) = 0.
   vtn(:)   = 0.     ; dc(:)  = 0.       ;  dr(:) = 0.     ; di(:)  = 0.
   qrcon(:)= 0.      ; qrconcr(:) = 0.   ;  taucon(:) = 0. ; tmp1d(:) = 0.
   qnpath = 0.       ; sfac = 0.        ;  gfac = 0.
   nfrzdtc = 0.      ; nfrzdtr = 0. 
   phdep(:)= 0.      ; phaut(:) = 0.   ; pracg(:) = 0. ; phacw(:) = 0. 
   phaci(:)= 0.      ; phacr(:) = 0.   ; phacs(:) = 0. ; phacg(:) = 0.
   phmlt(:)= 0.      ; pheml(:) = 0.   ; phevp(:) = 0. ; primh(:) = 0.
   pvapg(:)= 0.      ; pvaph(:) = 0.   ; pgwet(:) = 0. ; phwet(:) = 0.
   pgaci_w(:)= 0.    ; phaci_w(:) = 0. 
   nhacw(:) = 0.     ; nhacr(:) = 0.   ; nheml(:)= 0.      
!===============================================================================
!
! adjustment of nc and nr according to avaialble size of clouds and rain
!
!===============================================================================
   call adjust_number_concent(ktopqr,kdim,qrs(:,i,1),ncr(:,i,3),den(:,i),      &
        pidnr,drcoeff,qrmin,nrmin,nrmax,di1000,di50,di5000)
   call adjust_number_concent(ktopqc,kdim,qci(:,i,1),ncr(:,i,2),den(:,i),      &
        pidnc,one_1,qcmin,ncmin,ncmax,di15,  di1, di100)
!-------------------------------------------------------------------------------
! obtain in-cloud properties
!
   ktop = max(ktopqc,ktopqi)
   call cldf_mps_diag(1,kdim,t(:,i),p(:,i),q(:,i),qci(:,i,1),qci(:,i,2),       &
        dxmeter(i),cldf(:),ktop)
!
   do k = ktop, kts, -1
       ! change condensate variables to in-cloud variables
       if(cldf(k)>0.) then
         qci(k,i,1) = qci(k,i,1) / cldf(k)
         qci(k,i,2) = qci(k,i,2) / cldf(k)
         qrs(k,i,1) = qrs(k,i,1) / cldf(k)
         qrs(k,i,2) = qrs(k,i,2) / cldf(k)
         qrs(k,i,3) = qrs(k,i,3) / cldf(k)
         qrs(k,i,4) = qrs(k,i,4) / cldf(k)
       endif
   enddo
!-------------------------------------------------------------------------------
! update the slope parameters for microphysics computation
!
   ktop = ktopqr
   call slope_rain(1,kdim,ktop,qrs(:,i,1),den(:,i),denfac(:,i),t(:,i),         &
        ncr(:,i,3),                                                            &
        rslope(:,1),rslopeb(:,1),rslope2(:,1),rslope3(:,1),vtr(:))
   ktop = ktopqs
   call slope_snow(1,kdim,ktop,qrs(:,i,2),den(:,i),denfac(:,i),t(:,i),         &
        rslope(:,2),rslopeb(:,2),rslope2(:,2),rslope3(:,2),vts(:))
   ktop = ktopqg
   call slope_graupel(1,kdim,ktop,qrs(:,i,3),den(:,i),denfac(:,i),t(:,i),      &
        rslope(:,3),rslopeb(:,3),rslope2(:,3),rslope3(:,3),vtg(:))
   ktop = ktopqh
   call slope_hail(1,kdim,ktop,qrs(:,i,4),den(:,i),denfac(:,i),t(:,i),         &
        rslope(:,4),rslopeb(:,4),rslope2(:,4),rslope3(:,4),vth(:))
   ktop = ktopqc
   call slope_cloud(1,kdim,ktop,qci(:,i,1),ncr(:,i,2),den(:,i),denfac(:,i),    &
        t(:,i),qcmin,rslopec(:),rslopec2(:),rslopec3(:))
!===============================================================================
!
! melting/freezing
!
!===============================================================================
   ktop = ktopmax
   do k = kts, ktop
     tcelci(k) = t(k,i) - t0c
     if(t(k,i)<t0c) ifice(k) = .true.
     n0sfac(k) = max(min(exp(-alpha*tcelci(k)),n0smax/n0s),1.)
   enddo
!-------------------------------------------------------------------------------
! psmlt: melting of snow [hl a33]
!       (t>t0: s->r)
   if(lqs) then
     ktop = ktopqs
     do k = ktop, kts, -1
       if(.not.ifice(k) .and. qrs(k,i,2)>0.) then
         coeres = rslope2(k,2)*sqrt(rslope(k,2)*rslopeb(k,2))
         psmlt(k) = viscod(k)/xlf0*(t(k,i)-t0c)*pi/2.*n0sfac(k)                  &
                  *(precs1*rslope2(k,2)+precs2*venfac(k)*coeres)/den(k,i)
         psmlt(k) = max(min(psmlt(k)*dtcld,qrs(k,i,2)),0.)
!-------------------------------------------------------------------------------
! nsmlt: melting of snow [LH A27]
!       (T>T0: ->NR)
         if(qrs(k,i,2)>qrmin) then
           sfac = rslope(k,2)*n0s*n0sfac(k)/qrs(k,i,2)
           ncr(k,i,3) = min(ncr(k,i,3) + sfac*psmlt(k), nrmax)
         endif
         qrs(k,i,2) = qrs(k,i,2) - psmlt(k)
         qrs(k,i,1) = qrs(k,i,1) + psmlt(k)
         t(k,i) = t(k,i) - xlf0/cpm(k,i)*psmlt(k)*cldf(k)
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pgmlt: melting of graupel [hl a23]
!       (t>t0: g->r)
   if(lqg) then
     ktop = ktopqg
     do k = ktop, kts, -1
       if(.not.ifice(k) .and. qrs(k,i,3)>0.) then
         coeres = rslope2(k,3)*sqrt(rslope(k,3)*rslopeb(k,3))
         pgmlt(k) = viscod(k)/xlf0*(t(k,i)-t0c)*(precg1*rslope2(k,3)           &
                  + precg2*venfac(k)*coeres)/den(k,i)
         pgmlt(k) = max(min(pgmlt(k)*dtcld,qrs(k,i,3)),0.)
!-------------------------------------------------------------------------------
! ngmlt: melting of graupel [lh a28]
!       (t>t0: ->nr)
         if(qrs(k,i,3)>qrmin) then
           gfac = rslope(k,3)*n0g/qrs(k,i,3)
           ncr(k,i,3) = min(ncr(k,i,3) + gfac*pgmlt(k), nrmax)
         endif
         qrs(k,i,3) = qrs(k,i,3) - pgmlt(k)
         qrs(k,i,1) = qrs(k,i,1) + pgmlt(k)
         t(k,i) = t(k,i) - xlf0/cpm(k,i)*pgmlt(k)*cldf(k)
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! phmlt: melting of hail [bht a22]
!       (t>t0: qh->qr)
   if(lqh) then
     ktop = ktopqh
     do k = ktop, kts, -1
       if(.not.ifice(k) .and. qrs(k,i,4)>0.) then
         coeres = rslope2(k,4)*sqrt(rslope(k,4)*rslopeb(k,4))
         phmlt(k) = viscod(k)/xlf0*(t(k,i)-t0c)*(prech1*rslope2(k,4)           &
                  + prech2*venfac(k)*coeres)/den(k,i)
         phmlt(k) = max(min(phmlt(k)*dtcld,qrs(k,i,4)),0.)
!-------------------------------------------------------------------------------
! nhmlt: melting of hail [lh a28]
!       (t>t0: ->nr)
         if(qrs(k,i,4)>qrmin) then
           gfac = rslope(k,4)*n0h/qrs(k,i,4)
           ncr(k,i,3) = min(ncr(k,i,3) + gfac*phmlt(k), nrmax)
         endif
         qrs(k,i,4) = qrs(k,i,4) - phmlt(k)
         qrs(k,i,1) = qrs(k,i,1) + phmlt(k)
         t(k,i) = t(k,i) - xlf0/cpm(k,i)*phmlt(k)*cldf(k)
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pimlt: instantaneous melting of cloud ice [hl a47]
!       (t>t0: i->c)
   if(lqi) then
     ktop = ktopqi
     do k = ktop, kts, -1
       if(.not.ifice(k) .and. qci(k,i,2)>0.) then
         qci(k,i,1) = qci(k,i,1) + qci(k,i,2)
         t(k,i) = t(k,i) - xlf0/cpm(k,i)*qci(k,i,2)*cldf(k)
         qci(k,i,2) = 0.
!---------------------------------------------------------------
! nimlt: instantaneous melting of cloud ice  [lh a18]
!        (t>t0: ->nc)
         temp = (den(k,i)*max(qci(k,i,2),qcmin))
         temp = sqrt(sqrt(temp*temp*temp))
         ni(k) = min(max(5.38e7*temp,1.e3),1.e6)
         ncr(k,i,2) = min(ncr(k,i,2) + ni(k),ncmax)
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pihmf: homogeneous freezing of cloud water below -40c [hl a45]
!        (t<-40c: c->i)
   if(lqc) then
     ktop = ktopqc
     do k = ktop, kts, -1
       if(tcelci(k)<-40. .and. qci(k,i,1)>0.) then
         qci(k,i,2) = qci(k,i,2) + qci(k,i,1)
         t(k,i) = t(k,i) + xlf(k,i)/cpm(k,i)*qci(k,i,1)*cldf(k)
         qci(k,i,1) = 0.
!---------------------------------------------------------------
! nihmf: homogeneous  freezing of cloud water below -40c [lh a17]
!        (t<-40c: nc-> )
         if(ncr(k,i,2)>0.) ncr(k,i,2) = 0.
       endif
!-------------------------------------------------------------------------------
! pihtf: heterogeneous freezing of cloud water [hl a44]
!        (t0>t>-40c: c->i)
       if(ifice(k) .and. qci(k,i,1)>qcmin) then
         hvalue = max(tcelci(k),-70.)
         pfrzdtc = min(pisq*pfrz1*(exp(-pfrz2*hvalue)-1.)*denr/den(k,i)        &
                   *ncr(k,i,2)*rslopec3(k)*rslopec3(k)/18.*dtcld               &
                   ,qci(k,i,1))
!---------------------------------------------------------------
! nihtf: heterogeneous  freezing of cloud water [LH A16]
!         (T0>T>-40C: NC->)
         if(ncr(k,i,2)>ncmin) then
           nfrzdtc = min(pi*pfrz1*(exp(-pfrz2*hvalue)-1.)*ncr(k,i,2)           &
                     *rslopec3(k)/6.*dtcld,ncr(k,i,2))
           ncr(k,i,2) = max(ncr(k,i,2) - nfrzdtc,0.)
         endif
         qci(k,i,1) = qci(k,i,1) - pfrzdtc
         qci(k,i,2) = qci(k,i,2) + pfrzdtc
         t(k,i) = t(k,i) + xlf(k,i)/cpm(k,i)*pfrzdtc*cldf(k)
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pgfrz: freezing of rain water [hl a20]
!        (t<t0, r->g)
   if(lqr) then
     ktop = ktopqr
     do k = ktop, kts, -1
       if(ifice(k) .and. qrs(k,i,1)>0.) then
         hvalue = max(tcelci(k), -70.)
         pfrzdtr = min(140.*pisq*pfrz1*ncr(k,i,3)*denr/den(k,i)                &
                  *(exp(-pfrz2*hvalue)-1.)*rslope3(k,1)*rslope3(k,1)           &
                  *dtcld,qrs(k,i,1))
!---------------------------------------------------------------
! ngfrz: freezing of rain water [lh a26]
!        (T<T0, NR-> )
         if(ncr(k,i,3)>nrmin) then
           nfrzdtr = min(4.*pi*pfrz1*ncr(k,i,3)*(exp(-pfrz2*hvalue)-1.)        &
                    *rslope3(k,1)*dtcld, ncr(k,i,3))
           ncr(k,i,3) = max(ncr(k,i,3) - nfrzdtr, 0.)
         endif
         qrs(k,i,1) = qrs(k,i,1) - pfrzdtr
         qrs(k,i,3) = qrs(k,i,3) + pfrzdtr
         t(k,i) = t(k,i) + xlf(k,i)/cpm(k,i)*pfrzdtr*cldf(k)
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! re-define cloud top for numerical efficiencies
!
   ktop = ktopini
   do k = kts, ktop 
     qsat_mul(k) = fsvp_water(t(k,i),p(k,i))
     qsat_mul(k) = ep2 * qsat_mul(k) / (p(k,i) - qsat_mul(k))
     qsat_mul(k) = max(qsat_mul(k),qmin)
     rh_mul(k)   = max(q(k,i) / qsat_mul(k),qmin)
     qsat_ice(k) = fsvp(t(k,i),p(k,i))
     qsat_ice(k) = ep2 * qsat_ice(k) / (p(k,i) - qsat_ice(k))
     qsat_ice(k) = max(qsat_ice(k),qmin)
     rh_ice(k)   = max(q(k,i) / qsat_ice(k),qmin)
   enddo
!
   lqc = .false.
   lqi = .false.
   lqr = .false.
   lqs = .false.
   lqg = .false.
   lqh = .false.
   call find_cloud_top(1,kdim,ktopini,qci(:,i,1),zero_0,ktopqc)
   call find_cloud_top(1,kdim,ktopini,qci(:,i,2),zero_0,ktopqi)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,1),zero_0,ktopqr)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,2),zero_0,ktopqs)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,3),zero_0,ktopqg)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,4),zero_0,ktopqh)
   call find_cloud_top(1,kdim,ktopini,rh_ice(:), one_1,ktoprh)
   if(ktopqc>0.0) lqc = .true.
   if(ktopqi>0.0) lqi = .true.
   if(ktopqr>0.0) lqr = .true.
   if(ktopqs>0.0) lqs = .true.
   if(ktopqg>0.0) lqg = .true.
   if(ktopqh>0.0) lqh = .true.
   ktopmax = max(ktopqc,ktopqi,ktopqr,ktopqs,ktopqg,ktopqh,ktoprh)
!-------------------------------------------------------------------------------
! restore grid-mean variables 
!
   ktop = max(ktopqc,ktopqi)
   do k = ktop, kts, -1
       ! change in-cloud condensate variables to grid-mean variables
     if(cldf(k)>0.) then
       qci(k,i,1) = qci(k,i,1) * cldf(k)
       qci(k,i,2) = qci(k,i,2) * cldf(k)
       qrs(k,i,1) = qrs(k,i,1) * cldf(k)
       qrs(k,i,2) = qrs(k,i,2) * cldf(k)
       qrs(k,i,3) = qrs(k,i,3) * cldf(k)
       qrs(k,i,4) = qrs(k,i,4) * cldf(k)
     endif
   enddo
!-------------------------------------------------------------------------------
! obtain in-cloud properties
!
   call cldf_mps_diag(1,kdim,t(:,i),p(:,i),q(:,i),qci(:,i,1),qci(:,i,2),       &
        dxmeter(i),cldf(:),ktop)
   do k = ktop, kts, -1
       ! change condensate variables to in-cloud variables
     if(cldf(k)>0.) then
       qci(k,i,1) = qci(k,i,1) / cldf(k)
       qci(k,i,2) = qci(k,i,2) / cldf(k)
       qrs(k,i,1) = qrs(k,i,1) / cldf(k)
       qrs(k,i,2) = qrs(k,i,2) / cldf(k)
       qrs(k,i,3) = qrs(k,i,3) / cldf(k)
       qrs(k,i,4) = qrs(k,i,4) / cldf(k)
     endif
   enddo
!-------------------------------------------------------------------------------
! update the slope parameters and microphysical parameters
!
   ktop = ktopqr
   call slope_rain(1,kdim,ktop,qrs(:,i,1),den(:,i),denfac(:,i),t(:,i),         &
        ncr(:,i,3),                                                            &
        rslope(:,1),rslopeb(:,1),rslope2(:,1),rslope3(:,1),vtr(:))
   ktop = ktopqs
   call slope_snow(1,kdim,ktop,qrs(:,i,2),den(:,i),denfac(:,i),t(:,i),         &
        rslope(:,2),rslopeb(:,2),rslope2(:,2),rslope3(:,2),vts(:))
   ktop = ktopqg
   call slope_graupel(1,kdim,ktop,qrs(:,i,3),den(:,i),denfac(:,i),t(:,i),      &
        rslope(:,3),rslopeb(:,3),rslope2(:,3),rslope3(:,3),vtg(:))
   ktop = ktopqh
   call slope_hail(1,kdim,ktop,qrs(:,i,4),den(:,i),denfac(:,i),t(:,i),         &
        rslope(:,4),rslopeb(:,4),rslope2(:,4),rslope3(:,4),vth(:))
   ktop = ktopqi
   do k = ktop, kts, -1
     temp = (den(k,i)*max(qci(k,i,2),qcmin))
     temp = sqrt(sqrt(temp*temp*temp))
     ni(k) = min(max(5.38e7*temp,1.e3),1.e6)
     mi = den(k,i)*qci(k,i,2)/ni(k)
     di(k)  = max(min(dicon * sqrt(mi),dimax), qmin)
     vti(k) = 1.49e4*exp(log(di(k))*(1.31))
   enddo
!
   ktop = max(ktopqs,ktopqg,ktopqh)
   do k = ktop, kts, -1
     sumice(k) = max( (qrs(k,i,2)+qrs(k,i,3)+qrs(k,i,4)), qmin)
     if(sumice(k)>qmin) then
       vtmean(k) = (vts(k)*qrs(k,i,2)+vtg(k)*qrs(k,i,3)                        &
                 +vth(k)*qrs(k,i,4))/sumice(k)
     else
       vtmean(k) = 0.
     endif
   enddo
!-------------------------------------------------------------------------------
! compute the mean-volume drop diameter for raindrop distribution    [lh a10]
!
   ktop = ktopqr
   do k = ktop, kts, -1
     dr(k) = rslope(k,1)*drcoeff
   enddo
!--------------------------------------------------------------------
! compute the mean-volume drop diameter for cloud-droplet distribution [lh a7]
!
   ktop = ktopqc
   call slope_cloud(1,kdim,ktop,qci(:,i,1),ncr(:,i,2),den(:,i),denfac(:,i),    &
        t(:,i),qcmin,rslopec(:),rslopec2(:),rslopec3(:))
   do k = ktop, kts, -1
     dc(k) = rslopec(k)
   enddo
!===============================================================================
!
! warm rain processes
!
!===============================================================================
   ktop = max(ktopqc,ktopqr)
   do k = ktop, kts, -1
     qrcon(k)  = 2.7e-2*den(k,i)*qci(k,i,1)*(1.e20/16.*rslopec2(k)             &
               *rslopec2(k)-0.4)
     qrconcr(k) = max(1.2*qrcon(k), qrmin)
   enddo
   if(lqc) then
     ktop = ktopqc
     do k = ktop, kts, -1
!---------------------------------------------------------------
! praut: auto conversion rate from cloud to rain  [lh 9] 
!        (qc->qr)
       if(dc(k)>di15) then
         taucon(k) = 3.7/den(k,i)/qci(k,i,1)/(0.5e6*rslopec(k)-7.5)
         taucon(k) = max(taucon(k), qmin)
         praut(k) = qrcon(k)/(taucon(k)*den(k,i))
         praut(k) = min(max(praut(k),0.),qci(k,i,1)*rdtcld)
!---------------------------------------------------------------
! nraut: auto conversion rate from cloud to rain [lh a6] [cp 18 & 19]
!        (nc->nr)
         nraut(k) = dr_embryo*den(k,i)*praut(k)
         if(qrs(k,i,1)>qrconcr(k)) then
           nraut(k) = ncr(k,i,3)/qrs(k,i,1)*praut(k)
           nraut(k) = min(nraut(k),ncr(k,i,2)*rdtcld)
         endif
       endif
     enddo
   endif
!
   if(lqc) then
     ktop = ktopqc
     do k = ktop, kts, -1
!----------------------------------------------------------------
! nccol: self collection of cloud water             [lh a8] [cp 24 & 25]
!        (nc->)
       if(dc(k)>=di100) then
         nccol(k) = ncrk1*ncr(k,i,2)*ncr(k,i,2)*rslopec3(k)
       else
         nccol(k) = 2.*ncrk2*ncr(k,i,2)*ncr(k,i,2)*rslopec3(k)*rslopec3(k)
       endif
     enddo
   endif
!
   if(lqr) then
     ktop = ktopqr
     do k = ktop, kts, -1
       supsat(k) = max(q(k,i),qmin)-qsat_mul(k)
       satrdt(k) = supsat(k)*rdtcld
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain     [lh 10] [cp 22 & 23]
!        (qc->qr)
! nracw: accretion of cloud water by rain     [lh a9]
!        (nc->)
       if(qrs(k,i,1)>=qrconcr(k)) then
         if(dr(k)>=di100) then
           nracw(k) = min(ncrk1*ncr(k,i,2)*ncr(k,i,3)*(rslopec3(k)             &
                      + 24.*rslope3(k,1)),ncr(k,i,2)*rdtcld)
           pracw(k) = min(pi/6.*(denr/den(k,i))*ncrk1*ncr(k,i,2)               &
                      *ncr(k,i,3)*rslopec3(k)*(2.*rslopec3(k)                  &
                      + 24.*rslope3(k,1)),qci(k,i,1)*rdtcld)
         else
           nracw(k) = min(ncrk2*ncr(k,i,2)*ncr(k,i,3)*(2.*rslopec3(k)          &
                      *rslopec3(k)+5040.*rslope3(k,1)                          &
                      *rslope3(k,1)),ncr(k,i,2)*rdtcld)
           pracw(k) = min(pi/6.*(denr/den(k,i))*ncrk2*ncr(k,i,2)               &
                      *ncr(k,i,3)*rslopec3(k)*(6.*rslopec3(k)                  &
                      *rslopec3(k)+5040.*rslope3(k,1)*rslope3(k,1))            &
                      ,qci(k,i,1)*rdtcld)
         endif
       endif
!----------------------------------------------------------------
! nrcol: self collection of rain-drops and break-up [lh a21] [cp 24 & 25]
!        (nr->)
       if(qrs(k,i,1)>=qrconcr(k)) then
         if(dr(k)<di100) then
           nrcol(k) = 5040.*ncrk2*ncr(k,i,3)*ncr(k,i,3)*rslope3(k,1)           &
                     *rslope3(k,1)
         elseif(dr(k)>=di100 .and. dr(k)<di600) then
           nrcol(k) = 24.*ncrk1*ncr(k,i,3)*ncr(k,i,3)*rslope3(k,1)
         elseif(dr(k)>=di600 .and. dr(k)<di2000) then
           hvalue = -2.5e3*(dr(k)-di600)
           nrcol(k) = 24.*exp(hvalue)*ncrk1*ncr(k,i,3)*ncr(k,i,3)              &
                      *rslope3(k,1)
         else
           nrcol(k) = 0.
         endif
       endif
!-------------------------------------------------------------------------------
! prevp: evaporation/condensation rate of rain [hdc 14]
!        (v->r or r->v)
       if(qrs(k,i,1)>0.) then
         coeres = rslope(k,1)*sqrt(rslope(k,1)*rslopeb(k,1))
         prevp(k) = (rh_mul(k)-1.)*ncr(k,i,3)*(precr1*rslope(k,1)              &
                   +precr2*venfac(k)*coeres)/ab_mul(k)
         if(prevp(k)<=0.) then
           prevp(k) = max(prevp(k),-qrs(k,i,1)*rdtcld)
           prevp(k) = max(prevp(k),satrdt(k)*.5)
!----------------------------------------------------------------
! nrevp: evaporation/condensation rate of rain   [lh a14]
!        (nr->nccn)
           if(prevp(k)==-qrs(k,i,1)*rdtcld) then
             ncr(k,i,1) = min(ncr(k,i,1) + ncr(k,i,3), ccnmax)
             ncr(k,i,3) = 0.
           endif
         else
           prevp(k) = min(prevp(k),satrdt(k)*.5)
         endif
       endif
     enddo
   endif
!===============================================================================
!
! cold rain processes
!
!===============================================================================
   ktop = ktopmax
   ifice(:) = .false.
   do k = ktop, kts, -1
     tcelci(k) = t(k,i) - t0c
     if(t(k,i)<t0c) ifice(k) = .true.
     n0sfac(k) = max(min(exp(-alpha*tcelci(k)),n0smax/n0s),1.)
     supsat(k) = max(q(k,i),qmin)-qsat_ice(k)
     satrdt(k) = supsat(k)*rdtcld
   enddo
!
   if(lqr .and. lqi) then
     ktop = min(ktopqr,ktopqi)
     do k = ktop, kts, -1
       if(ifice(k)) then
         if(qrs(k,i,1)>qrmin .and. qci(k,i,2)>qcmin) then
!-------------------------------------------------------------
! praci: accretion of cloud ice by rain [hl a15] [lfo 25]
!        (t<t0: qi->qr)
           acrfac = 6.*rslope2(k,1)+4.*di(k)*rslope(k,1) + di(k)**2
           praci(k) = pi*qci(k,i,2)*ncr(k,i,3)*abs(vtr(k)-vti(k))*acrfac/4.
             ! reduce collection efficiency (suggested by B. Wilt)
           praci(k) = praci(k)*min(max(0.0,qrs(k,i,1)/qci(k,i,2)),1.)**2
           praci(k) = min(praci(k),qci(k,i,2)*rdtcld)
!-------------------------------------------------------------
! piacr: accretion of rain by cloud ice [hl A19] [lfo 26]
!        (t<t0: qr->qs OR qr->qg)
           piacr(k) = pisq*avtr*ncr(k,i,3)*denr*ni(k)*denfac(k,i)              &
                       *g7pbr*rslope3(k,1)*rslope2(k,1)*rslopeb(k,1)           &
                       /24./den(k,i)
             ! reduce collection efficiency (suggested by B. Wilt)
           piacr(k) = piacr(k)*min(max(0.0,qci(k,i,2)/qrs(k,i,1)),1.)**2
           piacr(k) = min(piacr(k),qrs(k,i,1)*rdtcld)
!-------------------------------------------------------------
! niacr: accretion of rain by cloud ice  [lh a25]
!        (t<t0: nr->)
           if(ncr(k,i,3)>nrmin) then
             niacr(k) = pi*avtr*ncr(k,i,3)*ni(k)*denfac(k,i)*g4pbr             &
                         *rslope2(k,1)*rslopeb(k,1)/4.
             ! reduce collection efficiency (suggested by B. Wilt)
             niacr(k) = niacr(k)*min(max(0.0,qci(k,i,2)/qrs(k,i,1)),1.)**2
             niacr(k) = min(niacr(k),ncr(k,i,3)*rdtcld)
           endif
         endif
       endif
     enddo
   endif
!	
   if(lqs) then
     ktop = ktopqs
     do k = ktop, kts, -1
!
       if(ifice(k)) then
         if(qrs(k,i,2)>qrmin .and. qci(k,i,2)>qcmin) then
           eacrs = exp(0.09*(tcelci(k)))
!-------------------------------------------------------------------------------
! psaci: accretion of cloud ice by rain [hdc 10]
!        (t<t0: i->s)
           acrfac = 2.*rslope3(k,2) + 2.*di(k)*rslope2(k,2)                    &
                   + di(k)**2*rslope(k,2)
           psaci(k) = pi*qci(k,i,2)*eacrs*n0s*n0sfac(k)                        &
                        *abs(vtmean(k)-vti(k))*acrfac/4.
           psaci(k) = psaci(k)*min(max(0.0,qrs(k,i,2)/qci(k,i,2)),1.)**2
           psaci(k) = min(psaci(k),qci(k,i,2)*rdtcld)
         endif
       endif
!-------------------------------------------------------------------------------
! psacw: accretion of cloud water by snow  [hl a7]
!        (t<t0: c->s, and t>=t0: c->r)
       if(qrs(k,i,2)>qrmin .and. qci(k,i,1)>qcmin) then
         psacw(k) = min(pacrc*n0sfac(k)*rslope3(k,2)*rslopeb(k,2)              &
                  *qci(k,i,1)*denfac(k,i),qci(k,i,1)*rdtcld)
             ! reduce collection efficiency (suggested by B. Wilt)
         psacw(k) = psacw(k)*min(max(0.0,qrs(k,i,2)/qci(k,i,1)),1.)**2
         psacw(k) = min(psacw(k),qci(k,i,1)*rdtcld)
       endif
!-------------------------------------------------------------
! nsacw: accretion of cloud water by snow  [lh A12]
!        (NC ->)
       if(qrs(k,i,2)>qrmin .and. ncr(k,i,2)>ncmin) then
         nsacw(k) = min(pacrc*n0sfac(k)*rslope3(k,2)*rslopeb(k,2)              &
             ! reduce collection efficiency (suggested by B. Wilt)
                     *min(max(0.0,qrs(k,i,2)/qci(k,i,1)),1.)**2                &
                     *ncr(k,i,2)*denfac(k,i),ncr(k,i,2)*rdtcld)
       endif
     enddo
   endif
!
   if(lqg) then
     ktop = ktopqg
     do k = ktop, kts, -1
       if(ifice(k)) then
!-------------------------------------------------------------------------------
! pgaci: accretion of cloud ice by graupel [hl a17]
!        (t<t0: i->g)
         if(qrs(k,i,3)>qrmin .and. qci(k,i,2)>qcmin) then
           egi = exp(0.09*(tcelci(k)))
           acrfac = 2.*rslope3(k,3) + 2.*di(k)*rslope2(k,3)                    &
                    +  di(k)**2*rslope(k,3)
           pgaci(k) = pi*egi*qci(k,i,2)*n0g*abs(vtmean(k)-vti(k))*acrfac/4.
           pgaci(k) = min(pgaci(k),qci(k,i,2)*rdtcld)
         endif
       endif
!-------------------------------------------------------------------------------
! pgacw: accretion of cloud water by graupel [hl a6]
!        (t<t0: c->g, and t>=t0: c->r)
       if(qrs(k,i,3)>qrmin .and. qci(k,i,1)>qcmin) then
          pgacw(k) = min(pacrg*rslope3(k,3)*rslopeb(k,3)                       &
             ! reduce collection efficiency (suggested by B. Wilt)
                    *min(max(0.0,qrs(k,i,3)/qci(k,i,1)),1.)**2                 &
                    *qci(k,i,1)*denfac(k,i),qci(k,i,1)*rdtcld)
       endif
!-------------------------------------------------------------
! ngacw: accretion of cloud water by graupel [lh a13]
!        (nc->
       if(qrs(k,i,3)>qrmin .and. ncr(k,i,2)>ncmin) then
         ngacw(k) = min(pacrg*rslope3(k,3)*rslopeb(k,3)*ncr(k,i,2)             &
             ! reduce collection efficiency (suggested by B. Wilt)
                     *min(max(0.0,qrs(k,i,3)/qci(k,i,1)),1.)**2                &
                     *denfac(k,i),ncr(k,i,2)*rdtcld)
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! paacw: accretion of cloud water by averaged snow/graupel
!        (t<t0: c->g or s, and t>=t0: c->r)
   ktop = max(ktopqs,ktopqg)
   do k = ktop, kts, -1
     if(sumice(k)>qmin) then
       paacw(k) = (qrs(k,i,2)*psacw(k) + qrs(k,i,3)*pgacw(k))/sumice(k)
!-------------------------------------------------------------
! naacw: Accretion of cloud water by averaged snow/graupel
!        (nc->)
       naacw(k) = (qrs(k,i,2)*nsacw(k) + qrs(k,i,3)*ngacw(k))/sumice(k)
     endif
   enddo
!
   if(lqh) then
     ktop = ktopqh
     do k = ktop, kts, -1
       if(ifice(k)) then
!-------------------------------------------------------------------------------
! phaci: accretion of cloud ice by hail [bht]
!        (t<t0: i->h)
         if(qrs(k,i,4)>qrmin .and. qci(k,i,2)>qcmin) then
           ehi = exp(0.09*(tcelci(k)))
           acrfac = 2.*rslope3(k,4) + 2.*di(k)*rslope2(k,4)                    &
                    +  di(k)**2*rslope(k,4)
           phaci(k) = pi*ehi*qci(k,i,2)*n0h*abs(vtmean(k)-vti(k))*acrfac/4.
           phaci(k) = min(phaci(k),qci(k,i,2)*rdtcld)
         endif
       endif
!-------------------------------------------------------------------------------
! phacw: rccretion of cloud water by hail [bht a08]
!      (t<t0: qc->qh, AND t>=t0: qc->qr)
       if(qrs(k,i,4)>qrmin .and. qci(k,i,1)>qcmin) then
         phacw(k) = min(pacrh*rslope3(k,4)*rslopeb(k,4)*qci(k,i,1)             &
                        *min(max(0.0,qrs(k,i,4)/qci(k,i,1)),1.)**2             &
                        *denfac(k,i),qci(k,i,1)*rdtcld)
       endif
!           
! nhacw: Accretion of cloud water by hail
!      (nc->)
       if(qrs(k,i,4)>qrmin .and. ncr(k,i,2)>ncmin) then
         nhacw(k) = min(pacrh*rslope3(k,4)*rslopeb(k,4)*ncr(k,i,2)             &
                     *min(max(0.0,qrs(k,i,4)/qci(k,i,1)),1.)**2                &
                     *denfac(k,i),ncr(k,i,2)*rdtcld)
       endif
     enddo
   endif
!
   if(lqr) then
     ktop = ktopqr
     do k = kts, ktop
!-------------------------------------------------------------------------------
! pracs: accretion of snow by rain [hl a11]
!         (t<t0: s->g)
       if(qrs(k,i,2)>qrmin .and. qrs(k,i,1)>qrmin) then
         if(ifice(k)) then
           acrfac = 5.*rslope3(k,2)*rslope3(k,2)                               &
                  + 4.*rslope3(k,2)*rslope2(k,2)*rslope(k,1)                   &
                  + 1.5*rslope2(k,2)*rslope2(k,2)*rslope2(k,1)
           pracs(k) = pisq*ncr(k,i,3)*n0s*n0sfac(k)*abs(vtr(k)-vtmean(k))      &
                      *(dens/den(k,i))*acrfac
             ! reduce collection efficiency (suggested by B. Wilt)
           pracs(k) = pracs(k)*min(max(0.0,qrs(k,i,1)/qrs(k,i,2)),1.)**2
           pracs(k) = min(pracs(k),qrs(k,i,2)*rdtcld)
         endif
!-------------------------------------------------------------
! psacr: accretion of rain by snow [hl a10] [lfo 28]
!         (t<t0:qr->qs or qr->qg) (t>=t0: enhanced melting of snow)
         acrfac = 30.*rslope3(k,1)*rslope2(k,1)*rslope(k,2)                    &
                 +10.*rslope2(k,1)*rslope2(k,1)*rslope2(k,2)                   &
                 + 2.*rslope3(k,1)*rslope3(k,2 )
         psacr(k) = pisq*ncr(k,i,3)*n0s*n0sfac(k)*abs(vtmean(k)-vtr(k))        &
                        *(denr/den(k,i))*acrfac
             ! reduce collection efficiency (suggested by B. Wilt)
         psacr(k) = psacr(k)*min(max(0.0,qrs(k,i,2)/qrs(k,i,1)),1.)**2
         psacr(k) = min(psacr(k),qrs(k,i,1)*rdtcld)
       endif
!-------------------------------------------------------------
! nsacr: accretion of rain by snow  [lh a23]
!       (t<t0:nr->)
       if(qrs(k,i,2)>qrmin .and. ncr(k,i,3)>nrmin) then
           acrfac = 1.5*rslope2(k,1)*rslope(k,2)                               &
                  + 1.0*rslope(k,1)*rslope2(k,2) + .5*rslope3(k,2)
           nsacr(k) = pi*ncr(k,i,3)*n0s*n0sfac(k)*abs(vtmean(k)-vtr(k))        &
                        *acrfac
             ! reduce collection efficiency (suggested by B. Wilt)
           nsacr(k) = nsacr(k)*min(max(0.0,qrs(k,i,2)/qrs(k,i,1)),1.)**2
           nsacr(k) = min(nsacr(k),ncr(k,i,3)*rdtcld)
       endif
!-------------------------------------------------------------
! pracg: Accretion of graupel by rain [bht a17]
!         (t<t0: qg->qh)
       if(qrs(k,i,3)>qrmin .and. qrs(k,i,1)>qrmin) then
         if(ifice(k)) then
           acrfac = 5.*rslope3(k,3)*rslope3(k,3)                               &
                   +4.*rslope3(k,3)*rslope2(k,3)*rslope(k,1)                   &
                   +1.5*rslope2(k,3)*rslope2(k,3)*rslope2(k,1)
           pracg(k) = pisq*ncr(k,i,3)*n0g*abs(vtr(k)-vtmean(k))                &
                       *(deng/den(k,i))*acrfac
           pracg(k) = pracg(k)*min(max(0.0,qrs(k,i,1)/qrs(k,i,3)),1.)**2
           pracg(k) = min(pracg(k),qrs(k,i,3)*rdtcld)
         endif
       endif
!-------------------------------------------------------------------------------
! pgacr: accretion of rain by graupel [hl a12]
!         (t<t0: r->g) (t>=t0: enhanced melting of graupel)
       if(qrs(k,i,3)>qrmin .and. qrs(k,i,1)>qrmin) then
!-------------------------------------------------------------------------------
! pgacr: accretion of rain by graupel [HL A12] [LFO 42]
!         (t<t0: qr->qg) (t>=t0: enhanced melting of graupel)
         acrfac =  30.*rslope3(k,1)*rslope2(k,1)*rslope(k,3)                   &
                 + 10.*rslope2(k,1)*rslope2(k,1)*rslope2(k,3)                  &
                 +  2.*rslope3(k,1)*rslope3(k,3)
         pgacr(k) = pisq*ncr(k,i,3)*n0g*abs(vtmean(k)-vtr(k))*(denr/den(k,i))  &
                       *acrfac
             ! reduce collection efficiency (suggested by B. Wilt)
         pgacr(k) = pgacr(k)*min(max(0.0,qrs(k,i,3)/qrs(k,i,1)),1.)**2
         pgacr(k) = min(pgacr(k),qrs(k,i,1)*rdtcld)
       endif
!-------------------------------------------------------------------------------
! ngacr: accretion of rain by graupel  [lh a24]
!        (t<t0: nr->)
       if(qrs(k,i,3)>qrmin .and. ncr(k,i,3)>nrmin) then
         acrfac = 1.5*rslope2(k,1)*rslope(k,3)                                 &
                 +1.0*rslope(k,1)*rslope2(k,3) + .5*rslope3(k,3)
         ngacr(k) = pi*ncr(k,i,3)*n0g*abs(vtmean(k)-vtr(k))*acrfac
         ngacr(k) = ngacr(k)*min(max(0.0,qrs(k,i,3)/qrs(k,i,1)),1.)**2
         ngacr(k) = min(ngacr(k),ncr(k,i,3)*rdtcld)
       endif
     enddo
   endif
!
   if(lqh) then
     ktop = ktopqh
     do k = kts, ktop
!-------------------------------------------------------------------------------
! phacr: Accretion of rain by hail [bht a13]
!         (t<t0: qr->qh) (t>=t0: enhance melting of hail)
       if(qrs(k,i,4)>qrmin.and.qrs(k,i,1)>qrmin) then
         acrfac = 30.*rslope3(k,1)*rslope2(k,1)*rslope(k,4)                    &
                 +10.*rslope3(k,1)*rslope(k,1)*rslope2(k,4)                    &
                 + 2.*rslope3(k,1)*rslope3(k,4)
         phacr(k) = pisq*ncr(k,i,3)*n0h*abs(vtmean(k)-vtr(k))*(denr/den(k,i))  &
                     *acrfac
         phacr(k) = phacr(k)*min(max(0.0,qrs(k,i,4)/qrs(k,i,1)),1.)**2
         phacr(k) = min(phacr(k),qrs(k,i,1)*rdtcld)
       endif
!-------------------------------------------------------------------------------
! nhacr: accretion of rain by hail
!        (t<t0: nr->)   
       if(qrs(k,i,4)>qrmin .and. ncr(k,i,3)>nrmin) then
         acrfac = 1.5*rslope2(k,1)*rslope(k,4)                                 &
                 + 1.0*rslope(k,1)*rslope2(k,4) + .5*rslope3(k,4)
         nhacr(k) = pi*ncr(k,i,3)*n0h*abs(vtmean(k)-vtr(i))*acrfac
         nhacr(k) = nhacr(k)*min(max(0.0,qrs(k,i,4)/qrs(k,i,1)),1.)**2
         nhacr(k) = min(nhacr(k),ncr(k,i,3)/dtcld)
       endif 
!-------------------------------------------------------------------------------
! phacs: accretion of snow by hail [bht a14] 
!         (t<t0: qs->qh)
       if(qrs(k,i,4)>qrmin.and.qrs(k,i,2)>qrmin) then
         acrfac = 5.*rslope3(k,2)*rslope3(k,2)*rslope(k,4)                     &
                 +2.*rslope3(k,2)*rslope2(k,2)*rslope2(k,4)                    &
                 +.5*rslope2(k,2)*rslope2(k,2)*rslope3(k,4)
         phacs(k) = pisq*eachs*n0s*n0sfac(k)*n0h*abs(vtmean(k)-vtmean(k))      &
                     *(dens/den(k,i))*acrfac
         phacs(k) = min(phacs(k),qrs(k,i,2)*rdtcld)
       endif
!-------------------------------------------------------------------------------
! phacg: accretion of snow by hail [bht a15]
!         (t<t0: qg->qh)
       if(qrs(k,i,4)>qrmin.and.qrs(k,i,3)>qrmin) then
         acrfac = 5.*rslope3(k,3)*rslope3(k,3)*rslope(k,4)                     &
                 +2.*rslope3(k,3)*rslope2(k,3)*rslope2(k,4)                    &
                 +.5*rslope2(k,3)*rslope2(k,3)*rslope3(k,4)
         phacg(k) = pisq*eachg*n0g*n0h*abs(vtmean(k)-vtmean(k))                &
                     *(deng/den(k,i))*acrfac
         phacg(k) = min(phacg(k),qrs(k,i,3)*rdtcld)
       endif
!-------------------------------------------------------------------------------
! pgwet: wet growth of graupel [lfo 43]
!
       if(qrs(k,i,4)>qrmin.or.qrs(k,i,3)>qrmin) then
         rs0 = fsvp(t0c,p(k,i))
         rs0 = ep2*rs0/(p(k,i)-rs0)
         rs0 = max(rs0,qmin)
         ghw1 = den(k,i)*xlv(k,i)*diffus(k)*(rs0-q(k,i)) - viscod(k)*(tcelci(k))
         ghw2 = den(k,i)*(xlf0+cliq*(tcelci(k)))
         ghw3 = venfac(k)
         ghw4 = den(k,i)*(xlf0-cliq*(-tcelci(k))+cice*(-tcelci(k)))
       endif
       if(qrs(k,i,3)>qrmin) then
         if(pgaci(k)>0.0) then
           egi = exp(0.09*(tcelci(k)))
           pgaci_w(k) = pgaci(k)/egi
         else
           pgaci_w(k) = 0.0
         endif
         pgwet(k) = ghw1/ghw2*(precg1*rslope2(k,3)                             &
                   +precg3*ghw3*exp(log(rslope(k,4))*2.75)                     &
                   +ghw4*(pgaci_w(k)+pgacs(k)))
         pgwet(k) = max(pgwet(k), 0.0)
         if(pgacw(k)+pgacr(k).lt.0.95*pgwet(k)) then
           pgaci(k) = 0.0
           pgacs(k) = 0.0
         endif
!-------------------------------------------------------------------------------
       endif
! phwet: wet growth of hail [lfo 43]
!
       if(qrs(k,i,4)>qrmin) then
         if(phaci(k)>0.0) then
           ehi = exp(0.09*(tcelci(k)))
           phaci_w(k) = phaci(k)/ehi
         else
           phaci_w(k) = 0.0
         endif
         phwet(k) = ghw1/ghw2*(prech1*rslope2(k,4)                             &
                   +prech3*ghw3*exp(log(rslope(k,4))*2.75)                     &
                   +ghw4*(phaci_w(k)+phacs(k)))
         phwet(k) = max(phwet(k), 0.0)
         if(phacw(k)+phacr(k).lt.0.95*phwet(k)) then
           phaci(k) = 0.0
           phacs(k) = 0.0
           phacg(k) = 0.0
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pseml: enhanced melting of snow by accretion of water [hl a34]
!        (t>=t0: s->r)
   if(lqs) then
     ktop = ktopqs
     do k = ktop, kts, -1
       if(.not.ifice(k) .and. qrs(k,i,2)>0.) then
         pseml(k) = min(max(-cliq*tcelci(k)*(paacw(k)+psacr(k))/xlf0,          &
                    -qrs(k,i,2)*rdtcld),0.)
!-------------------------------------------------------------------------------
! nseml: enhanced melting of snow by accretion of water    [lh a29]
!        (t>=t0: ->nr)
         if  (qrs(k,i,2)>qrmin) then
           sfac = rslope(k,2)*n0s*n0sfac(k)/qrs(k,i,2)
           nseml(k) = -sfac*pseml(k)
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pgeml: enhanced melting of graupel by accretion of water [hl a24]
!        (t>=t0: g->r)
   if(lqg) then
     ktop = ktopqg
     do k = ktop, kts, -1
       if(.not.ifice(k) .and. qrs(k,i,3)>0.) then
         pgeml(k) = min(max(-cliq*tcelci(k)*(paacw(k)+pgacr(k))/xlf0,          &
                   -qrs(k,i,3)*rdtcld),0.)
!--------------------------------------------------------------
! ngeml: enhanced melting of graupel by accretion of water [lh a30]
!         (t>=t0: -> nr)
         if  (qrs(k,i,3)>qrmin) then
           gfac = rslope(k,3)*n0g/qrs(k,i,3)
           ngeml(k) = -gfac*pgeml(k)
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pheml: enhanced melting of hail by accretion of water [bht a23]
!        (t>=t0: h->r)
   if(lqh) then
     ktop = ktopqh
     do k = ktop, kts, -1
       if(.not.ifice(k) .and. qrs(k,i,4)>0.) then
         pheml(k) = min(max(-cliq*tcelci(k)*(phacw(k)+phacr(k))/xlf0,          &
                   -qrs(k,i,4)*rdtcld),0.)
!--------------------------------------------------------------
! nheml: enhanced melting of graupel by accretion of water [lh a30]
!         (t>=t0: -> nr)
         if  (qrs(k,i,4)>qrmin) then
           gfac = rslope(k,4)*n0h/qrs(k,i,4)
           nheml(k) = -gfac*pheml(k)
         endif
       endif
     enddo
   endif
! -------------------------------------------------------------------------------
! pidep: deposition/sublimation rate of ice [hdc 9]
!       (t<t0: v->i or i->v)
   if(lqi) then
     ktop = ktopqi
     do k = ktop, kts, -1
       if(ifice(k)) then
         if(qci(k,i,2)>0 .and. (.not.ifsat(k))) then
           pidep(k) = 4.*di(k)*ni(k)*(rh_ice(k)-1.)/ab_ice(k)
           supice(k) = satrdt(k) - prevp(k)
           if(pidep(k)<0.) then
             pidep(k) = max(max(pidep(k),satrdt(k)*.5),supice(k))
             pidep(k) = max(pidep(k),-qci(k,i,2)*rdtcld)
           else
             pidep(k) = min(min(pidep(k),satrdt(k)*.5),supice(k))
           endif
           if(abs(prevp(k)+pidep(k))>=abs(satrdt(k))) ifsat(k) = .true.
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! psdep: deposition/sublimation rate of snow [hdc 14]
!        (v->s or s->v)
   if(lqs) then
     ktop = ktopqs
     do k = ktop, kts, -1
       if(ifice(k)) then
         if(qrs(k,i,2)>0. .and. (.not.ifsat(k))) then
           coeres = rslope2(k,2)*sqrt(rslope(k,2)*rslopeb(k,2))
           psdep(k) = (rh_ice(k)-1.)*n0sfac(k)*(precs1*rslope2(k,2)            &
                      + precs2*venfac(k)*coeres)/ab_ice(k)
           supice(k) = satrdt(k)-prevp(k)-pidep(k)
           if(psdep(k)<0.) then
             psdep(k) = max(psdep(k),-qrs(k,i,2)*rdtcld)
             psdep(k) = max(max(psdep(k),satrdt(k)*.5),supice(k))
           else
             psdep(k) = min(min(psdep(k),satrdt(k)*.5),supice(k))
           endif
           if(abs(prevp(k)+pidep(k)+psdep(k))>=abs(satrdt(k)))                 &
             ifsat(k) = .true.
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pgdep: deposition/sublimation rate of graupel [hl a21]
!        (t<t0: v->g or g->v)
   if(lqg) then
     ktop = ktopqg
     do k = ktop, kts, -1
       if(ifice(k)) then
         if(qrs(k,i,3)>0. .and. (.not.ifsat(k))) then
           coeres = rslope2(k,3)*sqrt(rslope(k,3)*rslopeb(k,3))
           pgdep(k) = (rh_ice(k)-1.)*(precg1*rslope2(k,3)                      &
                     + precg2*venfac(k)*coeres)/ab_ice(k)
           supice(k) = satrdt(k)-prevp(k)-pidep(k)-psdep(k)
           if(pgdep(k)<0.) then
             pgdep(k) = max(pgdep(k),-qrs(k,i,3)*rdtcld)
             pgdep(k) = max(max(pgdep(k),satrdt(k)*.5),supice(k))
           else
             pgdep(k) = min(min(pgdep(k),satrdt(k)*.5),supice(k))
           endif
           if(abs(prevp(k)+pidep(k)+psdep(k)+pgdep(k))>=abs(satrdt(k)))        &
             ifsat(k) = .true.
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! phdep: deposition/sublimation rate of hail [bht a19]
!        (t<t0: v->h or h->v)
   if(lqh) then
     ktop = ktopqh
     do k = ktop, kts, -1
       if(ifice(k)) then
         if(qrs(k,i,4)>0. .and. (.not.ifsat(k))) then
           coeres = rslope2(k,4)*sqrt(rslope(k,4)*rslopeb(k,4))
           phdep(k) = (rh_ice(k)-1.)*(prech1*rslope2(k,4)                      &
                     + prech2*venfac(k)*coeres)/ab_ice(k)
           supice(k) = satrdt(k)-prevp(k)-pidep(k)-psdep(k)-pgdep(k)
           if(phdep(k)<0.) then
             phdep(k) = max(phdep(k),-qrs(k,i,4)*rdtcld)
             phdep(k) = max(max(phdep(k),satrdt(k)*.5),supice(k))
           else
             phdep(k) = min(min(phdep(k),satrdt(k)*.5),supice(k))
           endif
           if(abs(prevp(k)+pidep(k)+psdep(k)+pgdep(k)+phdep(k))                &
               >=abs(satrdt(k))) ifsat(k) = .true.
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pigen: generation(nucleation) of ice from vapor [hl a50] [hdc 7-8]
!       (t<t0: v->i)
   ktop = ktoprh
   do k = ktop, kts, -1
     if(ifice(k)) then
       if(supsat(k)>0 .and. (.not.ifsat(k))) then
         supice(k) = satrdt(k)-prevp(k)-pidep(k)-psdep(k)-pgdep(k)
         if(slmsk(i)==0) then      
           ni0 = 1000.*exp(-0.2*tcelci(k)-5.)
         else if(slmsk(i)==1) then
           ni0 = 1000.*exp(-0.15*tcelci(k)-2.5)
         else if(slmsk(i)==2) then
           ni0 = 1000.*exp(-0.35*tcelci(k)-10.)
         endif
         ni0 = min(ni0,ni0max)
         roqi0 = 4.92e-11*exp(log(ni0)*(1.33))
         pigen(k) = max(0.,(roqi0/den(k,i)-max(qci(k,i,2),0.))*rdtcld)
         pigen(k) = min(min(pigen(k),satrdt(k)),supice(k))
       endif
     endif
   enddo
!-------------------------------------------------------------------------------
! psaut: conversion(aggregation) of ice to snow [hdc 12]
!       (t<t0: i->s)
   if(lqi) then
     ktop = ktopqi
     do k = ktop, kts, -1
       if(ifice(k)) then
         if(qci(k,i,2)>0.) then
           qimax = roqimax/den(k,i)
           psaut(k) = max(0.,(qci(k,i,2)-qimax)*rdtcld)
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pgaut: conversion(aggregation) of snow to graupel [hl a4]
!        (t<t0: s->g)
   if(lqs) then
     ktop = ktopqs
     do k = ktop, kts, -1
       if(ifice(k)) then
         if(qrs(k,i,2)>0.) then
           alpha2 = 1.e-3*exp(0.09*(tcelci(k)))
           pgaut(k) = min(max(0.,alpha2*(qrs(k,i,2)-qs0)),qrs(k,i,2)*rdtcld)
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! phaut: conversion(aggregation) of graupel to graupel [bht a18]
!        (t<t0: g->h)
   if(lqh) then
     ktop = ktopqh
     do k = ktop, kts, -1
       if(ifice(k)) then
         if(qrs(k,i,3)>0.) then
           alpha2 = 1.e-3*exp(0.09*(tcelci(k)))
           phaut(k) = min(max(0.,alpha2*(qrs(k,i,3)-qs0)),qrs(k,i,3)*rdtcld)
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! psevp: evaporation of melting snow [hl a35]
!       (t>t0: s->v)
   if(lqs) then
     ktop = ktopqs
     do k = ktop, kts, -1
       if(.not.ifice(k)) then
         if(qrs(k,i,2)>0. .and. rh_mul(k)<1.) then
              coeres = rslope2(k,2)*sqrt(rslope(k,2)*rslopeb(k,2))
              psevp(k) = (rh_mul(k)-1.)*n0sfac(k)*(precs1*rslope2(k,2)          &
                       + precs2*venfac(k)*coeres)/ab_mul(k)
           psevp(k) = min(max(psevp(k),-qrs(k,i,2)*rdtcld),0.)
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! pgevp: evaporation of melting graupel [hl a25]
!       (t>=t0: g->v)
   if(lqg) then
     ktop = ktopqg
     do k = ktop, kts, -1
       if(.not.ifice(k)) then
         if(qrs(k,i,3)>0. .and. rh_mul(k)<1.) then
           coeres = rslope2(k,3)*sqrt(rslope(k,3)*rslopeb(k,3))
           pgevp(k) = (rh_mul(k)-1.)*(precg1*rslope2(k,3)                      &
                     + precg2*venfac(k)*coeres)/ab_mul(k)
           pgevp(k) = min(max(pgevp(k),-qrs(k,i,3)*rdtcld),0.)
         endif
       endif
     enddo
   endif
!-------------------------------------------------------------------------------
! phevp: evaporation of melting hail [bht a20]
!       (t>=t0: h->v)
   if(lqh) then
     ktop = ktopqh
     do k = ktop, kts, -1
       if(.not.ifice(k)) then
         if(qrs(k,i,4)>0. .and. rh_mul(k)<1.) then
           coeres = rslope2(k,4)*sqrt(rslope(k,4)*rslopeb(k,4))
           phevp(k) = (rh_mul(k)-1.)*(prech1*rslope2(k,4)                      &
                     + prech2*venfac(k)*coeres)/ab_mul(k)
           phevp(k) = min(max(phevp(k),-qrs(k,i,4)*rdtcld),0.)
         endif
       endif
     enddo
   endif
!===============================================================================
!
! convert the in-cloud source/sink terms to the grd-mean state
!
!===============================================================================
   ktop = max(ktopqc,ktopqi)
   do k = ktop, kts, -1
       if(cldf(k)>0.) then
           ! change in-cloud condensate variables to grid-mean variables
         qci(k,i,1) = qci(k,i,1) * cldf(k)
         qci(k,i,2) = qci(k,i,2) * cldf(k)
         qrs(k,i,1) = qrs(k,i,1) * cldf(k)
         qrs(k,i,2) = qrs(k,i,2) * cldf(k)
         qrs(k,i,3) = qrs(k,i,3) * cldf(k)
         qrs(k,i,4) = qrs(k,i,4) * cldf(k)
           ! get grid-mean rates
         praut(k) = praut(k) * cldf(k)
         pracw(k) = pracw(k) * cldf(k)
         prevp(k) = prevp(k) * cldf(k)
         praci(k) = praci(k) * cldf(k)
         piacr(k) = piacr(k) * cldf(k)
         psaci(k) = psaci(k) * cldf(k)
         pgaci(k) = pgaci(k) * cldf(k)
         psacw(k) = psacw(k) * cldf(k)
         pgacw(k) = pgacw(k) * cldf(k)
         paacw(k) = paacw(k) * cldf(k)
         pracs(k) = pracs(k) * cldf(k)
         pgacr(k) = pgacr(k) * cldf(k)
         pgacs(k) = pgacs(k) * cldf(k)
         pseml(k) = pseml(k) * cldf(k)
         pgeml(k) = pgeml(k) * cldf(k)
         pidep(k) = pidep(k) * cldf(k)
         psdep(k) = psdep(k) * cldf(k)
         pgdep(k) = pgdep(k) * cldf(k)
         pigen(k) = pigen(k) * cldf(k)
         psaut(k) = psaut(k) * cldf(k)
         pgaut(k) = pgaut(k) * cldf(k)
         psevp(k) = psevp(k) * cldf(k)
         pgevp(k) = pgevp(k) * cldf(k)
         phdep(k) = phdep(k) * cldf(k)
         phaut(k) = phaut(k) * cldf(k)
         pracg(k) = pracg(k) * cldf(k)
         phacw(k) = phacw(k) * cldf(k)
         phaci(k) = phaci(k) * cldf(k)
         phacr(k) = phacr(k) * cldf(k)
         phacs(k) = phacs(k) * cldf(k)
         pheml(k) = pheml(k) * cldf(k)
         phevp(k) = phevp(k) * cldf(k)
         primh(k) = primh(k) * cldf(k)
         pvapg(k) = pvapg(k) * cldf(k)
         pvaph(k) = pvaph(k) * cldf(k)
         pgwet(k) = pgwet(k) * cldf(k)
         phwet(k) = phwet(k) * cldf(k)
         pgaci_w(k) = pgaci_w(k) * cldf(k)
         phaci_w(k) = phaci_w(k) * cldf(k)
       endif
   enddo
!===============================================================================
!
! check mass conservation of source/sink terms and feedback to the large scale
!
!===============================================================================
   ktop = ktopmax
   do k = ktop, kts, -1
!
     delta2 = 0.
     delta3 = 0.
     if(qrs(k,i,1)<1.e-4 .and. qrs(k,i,2)<1.e-4) delta2 = 1.
     if(qrs(k,i,1)<1.e-4) delta3 = 1.
!
     if(ifice(k)) then
!
! cloud water
!
       hvalue = max(qmin,qci(k,i,1))
       source = (praut(k)+pracw(k)+paacw(k)+paacw(k)+phacw(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         praut(k) = praut(k) * factor
         pracw(k) = pracw(k) * factor
         paacw(k) = paacw(k) * factor
         phacw(k) = phacw(k) * factor
       endif
!
! cloud ice
!
       hvalue = max(qmin,qci(k,i,2))
       source = (psaut(k)-pigen(k)-pidep(k)+praci(k)+psaci(k)+pgaci(k)         &
                + phaci(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         psaut(k) = psaut(k) * factor
         pigen(k) = pigen(k) * factor
         pidep(k) = pidep(k) * factor
         praci(k) = praci(k) * factor
         psaci(k) = psaci(k) * factor
         pgaci(k) = pgaci(k) * factor
         phaci(k) = phaci(k) * factor
       endif
!
! rain
!
       hvalue = max(qmin,qrs(k,i,1))
       source = (-praut(k)-prevp(k)-pracw(k)+piacr(k)+psacr(k)+pgacr(k)       &
                + phacr(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         praut(k) = praut(k) * factor
         prevp(k) = prevp(k) * factor
         pracw(k) = pracw(k) * factor
         piacr(k) = piacr(k) * factor
         psacr(k) = psacr(k) * factor
         pgacr(k) = pgacr(k) * factor
         phacr(k) = phacr(k) * factor
       endif
!
! snow
!
       hvalue = max(qmin,qrs(k,i,2))
       source = - (psdep(k)+psaut(k)-pgaut(k)+paacw(k)+piacr(k)*delta3         &
                + pvapg(k)+pvaph(k)                                            &
                + praci(k)*delta3 - pracs(k)*(1.-delta2)                       &
                + psacr(k)*delta2 + psaci(k)-pgacs(k)-phacs(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         psdep(k) = psdep(k) * factor
         psaut(k) = psaut(k) * factor
         pgaut(k) = pgaut(k) * factor
         paacw(k) = paacw(k) * factor
         piacr(k) = piacr(k) * factor
         praci(k) = praci(k) * factor
         psaci(k) = psaci(k) * factor
         pracs(k) = praCs(k) * factor
         psacr(k) = psacr(k) * factor
         pgacs(k) = pgacs(k) * factor
         pvapg(k) = pvapg(k) * factor
         pvaph(k) = pvaph(k) * factor
         phacs(k) = phacs(k) * factor
       endif
!
! graupel
!
       hvalue = max(qmin,qrs(k,i,3))
       source = - (pgdep(k)+pgaut(k)                                           &
                + piacr(k)*(1.-delta3) + praci(k)*(1.-delta3)                  &
                + psacr(k)*(1.-delta2) + pracs(k)*(1.-delta2)                  &
                + pgaci(k)+paacw(k)+pgacr(k)*delta2+pgacs(k)                   &
                - pracg(k)*(1.-delta2)-phacg(k)-phaut(k)                       &
                - pvapg(k)+primh(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         pgdep(k) = pgdep(k) * factor
         pgaut(k) = pgaut(k) * factor
         piacr(k) = piacr(k) * factor
         praci(k) = praci(k) * factor
         psacr(k) = psacr(k) * factor
         pracs(k) = pracs(k) * factor
         paacw(k) = paacw(k) * factor
         pgaci(k) = pgaci(k) * factor
         pgacr(k) = pgacr(k) * factor
         pgacs(k) = pgacs(k) * factor
         phaut(k) = phaut(k) * factor
         pracg(k) = pracg(k) * factor
         phacg(k) = phacg(k) * factor
         pvapg(k) = pvapg(k) * factor
         primh(k) = primh(k) * factor
       endif
!
!     hail
!
       hvalue = max(qmin,qrs(k,i,4))
       source = -(phdep(k)+phaut(k)                                            &
                +pgacr(k)*(1.-delta2)+pracg(k)*(1.-delta2)                     &
                +phacw(k)+phacr(k)+phaci(k)+phacs(k)                           &
                +phacg(k)-pvaph(k)-primh(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         phdep(k) = phdep(k) * factor
         phaut(k) = phaut(k) * factor
         pracg(k) = pracg(k) * factor
         pgacr(k) = pgacr(k) * factor
         phacw(k) = phacw(k) * factor
         phaci(k) = phaci(k) * factor
         phacr(k) = phacr(k) * factor
         phacs(k) = phacs(k) * factor
         phacg(k) = phacg(k) * factor
         pvaph(k) = pvaph(k) * factor
         primh(k) = primh(k) * factor
       endif
!
!     cloud
!
       hvalue = max(qmin,ncr(k,i,2))
       source = (nraut(k)+nccol(k)+nracw(k)+naacw(k)+naacw(k)                  &
                + nhacw(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         nraut(k) = nraut(k) * factor
         nccol(k) = nccol(k) * factor
         nracw(k) = nracw(k) * factor
         naacw(k) = naacw(k) * factor
         nhacw(k) = nhacw(k) * factor
       endif
!
!     rain
!
       hvalue = max(qmin,ncr(k,i,3))
       source = (-nraut(k)+nrcol(k)+niacr(k)+nsacr(k)+ngacr(k)                 &
               + nhacr(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         nraut(k) = nraut(k) * factor
         nrcol(k) = nrcol(k) * factor
         niacr(k) = niacr(k) * factor
         nsacr(k) = nsacr(k) * factor
         ngacr(k) = ngacr(k) * factor
         nhacr(k) = nhacr(k) * factor
       endif
!                                                                                        
! update              
!                                                                                        
       htotal = -(prevp(k)+psdep(k)+pgdep(k)+pigen(k)+pidep(k)+phdep(k))
       q(k,i) = q(k,i)+htotal*dtcld                                             
       qci(k,i,1) = max(qci(k,i,1) - (praut(k)+pracw(k)                        &
                   +paacw(k)+paacw(k)+phacw(k))*dtcld,0.)                             
       qrs(k,i,1) = max(qrs(k,i,1) + (praut(k)+pracw(k)                        &         
                   +prevp(k)-piacr(k)-pgacr(k)-psacr(k)-phacr(k))*dtcld,0.) 
       qci(k,i,2) = max(qci(k,i,2) - (psaut(k)+praci(k)                        &         
                   +phaci(k)                                                   &
                   +psaci(k)+pgaci(k)-pigen(k)-pidep(k))*dtcld,0.) 
       qrs(k,i,2) = max(qrs(k,i,2) + (psdep(k)+psaut(k)+paacw(k)               &         
                   -pgaut(k)+piacr(k)*delta3 + praci(k)*delta3                 &
                   +pvapg(k)+pvaph(k)-phacs(k)                                 &
                   +psaci(k)-pgacs(k)-pracs(k)*(1.-delta2) + psacr(k)*delta2)  &
                   *dtcld,0.)                                                    
       qrs(k,i,3) = max(qrs(k,i,3) + (pgdep(k)+pgaut(k)+piacr(k)*(1.-delta3)   &
                   +praci(k)*(1.-delta3) + psacr(k)*(1.-delta2)                &         
                   +pracs(k)*(1.-delta2) + pgaci(k)+paacw(k)                   &         
                   +pgacr(k)*delta2+pgacs(k)+primh(k)                          &
                   -pracg(k)*(1.-delta2)-phacg(k)-phaut(k)                     &
                   -pvapg(k))*dtcld,0.)
       qrs(k,i,4) = max(qrs(k,i,4)+(phdep(k)+phaut(k)                          &
                   +pgacr(k)*(1.-delta2)+pracg(k)*(1.-delta2)                  &
                   +phacw(k)+phacr(k)+phaci(k)+phacs(k)                        &
                   +phacg(k)-pvaph(k)-primh(k))                                &
                   *dtcld,0.)
       ncr(k,i,2) = max(ncr(k,i,2) + (-nraut(k)-nccol(k)-nracw(k)              &
                   -naacw(k)-naacw(k)-nhacw(i))*dtcld,0.)
       ncr(k,i,3) = max(ncr(k,i,3) + (nraut(k)-nrcol(k)-niacr(k)               &
                   -nsacr(k)-ngacr(k)-nhacr(k))*dtcld,0.)
       hvalue = - xls*(psdep(k)+pgdep(k)+phdep(k)+pidep(k)+pigen(k))           &         
               - xlv(k,i)*prevp(k) - xlf(k,i)*(piacr(k)+paacw(k)+phacw(k)      &         
               + paacw(k)+pgacr(k)+psacr(k)+phacr(k))                                 
       t(k,i) = t(k,i) - hvalue/cpm(k,i)*dtcld                                       
!
     else                                                                           
!
! cloud water
!
       hvalue = max(qmin,qci(k,i,1))
       source=(praut(k)+pracw(k)+paacw(k)+paacw(k)-phacw(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         praut(k) = praut(k) * factor
         pracw(k) = pracw(k) * factor
         paacw(k) = paacw(k) * factor
         phacw(k) = phacw(k) * factor
       endif
!
! rain
!
       hvalue = max(qmin,qrs(k,i,1))
       source = (-prevp(k)-praut(k)+pseml(k)+pgeml(k)+pheml(k)                 &
                 -pracw(k)-paacw(k)-paacw(k)-phacw(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         praut(k) = praut(k) * factor
         prevp(k) = prevp(k) * factor
         pracw(k) = pracw(k) * factor
         paacw(k) = paacw(k) * factor
         pseml(k) = pseml(k) * factor
         pgeml(k) = pgeml(k) * factor
         phacw(k) = phacw(k) * factor
         pheml(k) = pheml(k) * factor
       endif
!
! snow
!
       hvalue = max(qmin,qrs(k,i,2))
       source=(pgacs(k)+phacs(k)-pseml(k)-psevp(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         pgacs(k) = pgacs(k) * factor
         pseml(k) = pseml(k) * factor
         psevp(k) = psevp(k) * factor
         phacs(k) = phacs(k) * factor
       endif
!
! graupel
!
       hvalue = max(qmin,qrs(k,i,3))
       source=-(pgacs(k)+pgevp(k)+pgeml(k)-phacg(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         pgacs(k) = pgacs(k) * factor
         pgevp(k) = pgevp(k) * factor
         pgeml(k) = pgeml(k) * factor
         phacg(k) = phacg(k) * factor
       endif
!
!     hail
!
       hvalue = max(qrmin,qrs(i,k,4))
       source=-(phacs(k)+phacg(k)+phevp(k)+pheml(k))*dtcld
       if (source.gt.hvalue) then
         factor = hvalue/source
         phacs(k) = phacs(k)*factor
         phacg(k) = phacg(k)*factor
         phevp(k) = phevp(k)*factor
         pheml(k) = pheml(k)*factor
       endif
!
!     cloud
!
       hvalue = max(qmin,ncr(k,i,2))
       source = (+nraut(k)+nccol(k)+nracw(k)+naacw(k)+naacw(k)+nhacw(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         nraut(k) = nraut(k) * factor
         nccol(k) = nccol(k) * factor
         nracw(k) = nracw(k) * factor
         naacw(k) = naacw(k) * factor
         nhacw(k) = nhacw(k) * factor
       endif
!
!     rain
!
       hvalue = max(qmin,ncr(k,i,3))
       source = (-nraut(k)+nrcol(k)-nseml(k)-ngeml(k)-nheml(k))*dtcld
       if (source>hvalue) then
         factor = hvalue/source
         nraut(k) = nraut(k) * factor
         nrcol(k) = nrcol(k) * factor
         nseml(k) = nseml(k) * factor
         ngeml(k) = ngeml(k) * factor
         nheml(k) = nheml(k) * factor
       endif
!
! update
!
       htotal = -(prevp(k)+psevp(k)+pgevp(k)+phevp(k))
       q(k,i) = q(k,i) + htotal*dtcld
       qci(k,i,1) = max(qci(k,i,1) - (praut(k)+pracw(k)                        &
                   +paacw(k)+paacw(k)+phacw(k))*dtcld,0.)
       qrs(k,i,1) = max(qrs(k,i,1) + (praut(k)+pracw(k)                        &
                   +prevp(k)+paacw(k)+paacw(k)+phacw(k)                        &
                   -pseml(k)-pgeml(k)-pheml(k))*dtcld,0.)
       qrs(k,i,2) = max(qrs(k,i,2) + (psevp(k)-pgacs(k)-phacs(k)               &
                   +pseml(k))*dtcld,0.)
       qrs(k,i,3) = max(qrs(k,i,3) + (pgacs(k)+pgevp(k)                        &
                   +pgeml(k)-phacg(k))*dtcld,0.)
       qrs(k,i,4) = max(qrs(k,i,4)+(phacs(k)+phacg(k)+phevp(k)                 &
                   +pheml(k))*dtcld,0.)
       ncr(k,i,2) = min(max(ncr(k,i,2) + (-nraut(k)-nccol(k)-nracw(k)          &
                   -naacw(k)-naacw(k)-nhacw(k))*dtcld,0.),ncmax)
       ncr(k,i,3) = min(max(ncr(k,i,3) + (nraut(k)-nrcol(k)+nseml(k)           & 
                   +ngeml(k)+nheml(k))*dtcld,0.),nrmax)
       hvalue = -xlv(k,i)*(prevp(k)+psevp(k)+pgevp(k)+phevp(k))                &
                -xlf(k,i)*(pseml(k)+pgeml(k)+pheml(k))
       t(k,i) = t(k,i) - hvalue/cpm(k,i)*dtcld
     endif
   enddo
!===============================================================================
!
! ccn activaiton and condensation/evaporation of clouds
!
!===============================================================================
!---------------------------------------------------------------
! pcact: qv -> qc                                 [lh 8]  [kk 14]
! ncact: nccn -> nc                               [lh a2] [kk 12]
!
   ktop = ktopini
   do k = ktop, kts, -1
     qsat_mul(k) = fsvp_water(t(k,i),p(k,i))
     qsat_mul(k) = ep2 * qsat_mul(k) / (p(k,i) - qsat_mul(k))
     qsat_mul(k) = max(qsat_mul(k),qmin)
     rh_mul(k)   = max(q(k,i) / qsat_mul(k),qmin)
   enddo
!
   call find_cloud_top(1,kdim,ktopini,rh_mul(:), one_1,ktoprh)
!
   ktop = ktoprh
   do k = ktop, kts, -1
     if(rh_mul(k)>1.) then
       temp = ((rh_mul(k)-1.)/satmax)
       temp = min(1.,exp(log(temp)*actk))
       ncact(k) = max(0.,((ncr(k,i,1)+ncr(k,i,2))*temp - ncr(k,i,2)))*rdtcld
       ncact(k) =min(ncact(k),max(ncr(k,i,1),0.)*rdtcld)
       pcact(k) = min(4.*pi*denr*exp(log(actr)*3)*ncact(k)/                    &
                     (3.*den(k,i)),max(q(k,i),0.)*rdtcld)
       q(k,i) = max(q(k,i) - pcact(k)*dtcld,0.)
       qci(k,i,1) = max(qci(k,i,1) + pcact(k)*dtcld,0.)
       ncr(k,i,1) = max(ncr(k,i,1) - ncact(k)*dtcld, ccnmin)
       ncr(k,i,2) = max(ncr(k,i,2) + ncact(k)*dtcld, ncmin)
       t(k,i) = t(k,i) + pcact(k)*xlv(k,i)/cpm(k,i)*dtcld
     endif
   enddo
!-------------------------------------------------------------------------------
! pcond: condensational/evaporational rate of cloud water [hl a46]
! (qv -> qc, qc->qv) 
   ktop = ktopini
   do k = ktop, kts, -1
     qsat_mul(k) = fsvp_water(t(k,i),p(k,i))
     qsat_mul(k) = ep2 * qsat_mul(k) / (p(k,i) - qsat_mul(k))
     qsat_mul(k) = max(qsat_mul(k),qmin)
   enddo
   call find_cloud_top(1,kdim,ktopini,qci(:,i,1),zero_0,ktopqc)
   call find_cloud_top(1,kdim,ktopini,rh_mul(:), one_1,ktoprh)
   ktop = max(ktopqc,ktoprh)
   do k = ktop, kts, -1
     hvalue = ((max(q(k,i),qmin)-(qsat_mul(k))) /(1.+(xlv(k,i))*(xlv(k,i))     &
              /(rv*(cpm(k,i)))*(qsat_mul(k)) /((t(k,i))*(t(k,i)))))
     pcond(k) = min(max(hvalue*rdtcld,0.),max(q(k,i),0.)*rdtcld)
     if(qci(k,i,1)>0. .and. hvalue<0.) then
       pcond(k) = max(hvalue,-qci(k,i,1))*rdtcld
!----------------------------------------------------------------
! ncevp: evpration of cloud number concentration  [lh A3]
! (nc->nccn)
       if(pcond(k)==-qci(k,i,1)*rdtcld) then
         ncr(k,i,1) = ncr(k,i,1) + ncr(k,i,2)
         ncr(k,i,2) = 0.
       endif
     endif
     q(k,i) = q(k,i) - pcond(k)*dtcld
     qci(k,i,1) = max(qci(k,i,1)+pcond(k)*dtcld,0.)
     t(k,i) = t(k,i) + pcond(k)*xlv(k,i)/cpm(k,i)*dtcld
   enddo
!===============================================================================
!
! sedimentation of hydrometeors
!
!===============================================================================
! re-define cloud top for numerical efficiencies
!
   lqc = .false.
   lqi = .false.
   lqr = .false.
   lqs = .false.
   lqg = .false.
   lqh = .false.
   flgcld = .false.
   call find_cloud_top(1,kdim,ktopini,qci(:,i,1),zero_0,ktopqc)
   call find_cloud_top(1,kdim,ktopini,qci(:,i,2),zero_0,ktopqi)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,1),zero_0,ktopqr)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,2),zero_0,ktopqs)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,3),zero_0,ktopqg)
   call find_cloud_top(1,kdim,ktopini,qrs(:,i,4),zero_0,ktopqh)
   call find_cloud_top(1,kdim,ktopini,rh_ice(:), one_1,ktoprh)
   if(ktopqc>0.0) lqc = .true.
   if(ktopqi>0.0) lqi = .true.
   if(ktopqr>0.0) lqr = .true.
   if(ktopqs>0.0) lqs = .true.
   if(ktopqg>0.0) lqg = .true.
   if(ktopqh>0.0) lqh = .true.
   ktopmax = max(ktopqc,ktopqi,ktopqr,ktopqs,ktopqg,ktopqh,ktoprh)
!
! fall out for rain
!
   if(lqr) then
     ktop = ktopqr
     call adjust_number_concent(ktopqr,kdim,qrs(:,i,1),ncr(:,i,3),den(:,i),    &
          pidnr,drcoeff,qrmin,nrmin,nrmax,di1000,di50,di5000)
     call slope_rain(1,kdim,ktop,qrs(:,i,1),den(:,i),denfac(:,i),t(:,i),       &
        ncr(:,i,3),                                                            &
        rslope(:,1),rslopeb(:,1),rslope2(:,1),rslope3(:,1),vtr(:))
     nstep = 1
     do k = ktop, kts, -1
       vtn(k) = pvtrn*rslopeb(k,1)*denfac(k,i)
       hvalue = max(vtr(k),vtn(k))
       nstep = max(nstep,ceiling(dtcld/delz(k,i)*hvalue))
     enddo
!
     if(ktop>2) then
       do k = ktop-1, kts+1, -1
         temp = (2.*vtr(k)+vtr(k+1)+vtr(k-1))*0.25
         vtr(k) = temp 
       enddo
     endif
!
     do k = ktop, kts, -1
       if(qrs(k,i,1)>qrmin) then
         diameter = drcoeff*rslope(k,1)
         hvalue = fmps(diameter)
        vtn(k) = vtr(k)/hvalue
       else
         vtn(k) = 0.
       endif
     enddo
!
     niter = ceiling(nstep/sedi_semi_cfl)
     dtcfl = dtcld/niter
!
     do n = 1, niter
       do k = ktop, kts, -1
         if(qrs(k,i,1)>qrmin) then
           denqr(k) = dend(k,i)*qrs(k,i,1)
           denqn(k) = dend(k,i)*ncr(k,i,3)
         else
           denqr(k) = 0.0
           denqn(k) = 0.0
           vtr(k) = 0.0
           vtn(k) = 0.0
         endif
       enddo
!
       call semi_lagrange_sedim(1,kdim,ktop,dend(:,i),denfac(:,i),t(:,i),      &
            delz(:,i),vtr(:),denqr(:),qrpath,dtcfl,lat,i)
       call semi_lagrange_sedim(1,kdim,ktop,dend(:,i),denfac(:,i),t(:,i),      &
            delz(:,i),vtn(:),denqn(:),qnpath,dtcfl,lat,i)
       do k = ktop, kts, -1
         if(denqr(k)>qrmin) then
           qrs(k,i,1) = max(denqr(k)/dend(k,i),0.)
           ncr(k,i,3) = max(denqn(k)/dend(k,i),0.)
         else
           qrs(k,i,1) = 0.0
           ncr(k,i,3) = 0.0
         endif
       enddo
!
       precip_r = qrpath/dtcld + precip_r   ! [kgm-2s-1]
!
     enddo
   endif
!-------------------------------------------------------------------------------
! fall out for precipitating ice : mass-weighted combined sedimentation (dhl )
!
   if(lqs .or. lqg .or. lqh) then
     ktop =ktopqs
     call slope_snow(1,kdim,ktop,qrs(:,i,2),den(:,i),denfac(:,i),t(:,i),       &
          rslope(:,2),rslopeb(:,2),rslope2(:,2),rslope3(:,2),vts(:))
     ktop =ktopqg
     call slope_graupel(1,kdim,ktop,qrs(:,i,3),den(:,i),denfac(:,i),t(:,i),    &
          rslope(:,3),rslopeb(:,3),rslope2(:,3),rslope3(:,3),vtg(:))
     ktop =ktopqh
     call slope_hail(1,kdim,ktop,qrs(:,i,4),den(:,i),denfac(:,i),t(:,i),       &
          rslope(:,4),rslopeb(:,4),rslope2(:,4),rslope3(:,4),vth(:))
!
     ktop = max(ktopqs,ktopqg,ktopqh)
     do k = ktop, kts, -1
       sumice(k) = max( (qrs(k,i,2) + qrs(k,i,3) + qrs(k,i,4)), qmin)
       if(sumice(k)>qmin) then
         vtmean(k) = (vts(k)*qrs(k,i,2) + vtg(k)*qrs(k,i,3)                    &
                   + vth(k)*qrs(k,i,4))/sumice(k)
       else
         vtmean(k) = 0.
       endif
     enddo
     if(ktop>2) then
       do k = ktop-1, kts+1, -1
         vtmean(k) = (2*vtmean(k)+vtmean(k+1)+vtmean(k-1))/4.
       enddo
     endif
!
     nstep = max(1,ceiling(maxval(dtcld/delz(:,i)*vtmean(:))))
     niter = ceiling(nstep/sedi_semi_cfl)
     dtcfl = dtcld/niter
!
     do n = 1, niter
!
       ktop =ktopqs
       do k = ktop, kts, -1
         denqs(k) = dend(k,i)*qrs(k,i,2)
       enddo
       call semi_lagrange_sedim(1,kdim,ktop,dend(:,i),denfac(:,i),t(:,i),      &
            delz(:,i),vtmean(:),denqs(:),qspath,dtcfl,lat,i)
       do k = kts, ktop
         qrs(k,i,2) = max(denqs(k)/dend(k,i),0.)
       enddo
       ktop =ktopqg
       do k = ktop, kts, -1
         denqg(k) = dend(k,i)*qrs(k,i,3)
       enddo
       call semi_lagrange_sedim(1,kdim,ktop,dend(:,i),denfac(:,i),t(:,i),      &
            delz(:,i),vtmean(:),denqg(:),qgpath,dtcfl,lat,i)
       do k = kts, ktop
         qrs(k,i,3) = max(denqg(k)/dend(k,i),0.)
       enddo
       ktop =ktopqh
       do k = ktop, kts, -1
         denqh(k) = dend(k,i)*qrs(k,i,4)
       enddo
       call semi_lagrange_sedim(1,kdim,ktop,dend(:,i),denfac(:,i),t(:,i),      &
            delz(:,i),vtmean(:),denqh(:),qhpath,dtcfl,lat,i)
       do k = kts, ktop
         qrs(k,i,4) = max(denqh(k)/dend(k,i),0.)
       enddo
!
       precip_s = qspath/dtcld + precip_s   ! [kgm-2s-1]
       precip_g = qgpath/dtcld + precip_g   ! [kgm-2s-1]
       precip_h = qhpath/dtcld + precip_h   ! [kgm-2s-1]
     enddo
   endif
!-------------------------------------------------------------------------------
! fall out for cloud ice
!
   if(lqi) then
     ktop = ktopqi
     do k = ktop, kts, -1
       temp = (den(k,i)*max(qci(k,i,2),qcmin))
       temp = exp(log(temp)*0.75)
       ni(k) = min(max(5.38e7*temp,1.e3),1.e6)
       if(qci(k,i,2)<=0.) then
         vti(k) = 0.
       else
         mi  = den(k,i)*qci(k,i,2)/ni(k)
         di(k)   = max(min( dicon*sqrt(mi), dimax), qmin)
         vti(k) = 1.49e4*exp(log(di(k))*(1.31))
       endif
     enddo
     if(ktop>2) then
       do k = ktop-1, kts+1, -1
         vti(k) = (2*vti(k)+vti(k+1)+vti(k-1))/4.
       enddo
     endif
!
     nstep = max(1,ceiling(maxval(dtcld/delz(:,i)*vti(:))))
     niter = ceiling(nstep/sedi_semi_cfl)
     dtcfl = dtcld/niter
!
     do n = 1, niter
       do k = ktop, kts, -1
         denqi(k) = dend(k,i)*qci(k,i,2)
       enddo
!
       call semi_lagrange_sedim(1,kdim,ktop,dend(:,i),denfac(:,i),t(:,i),      &
            delz(:,i),vti(:),denqi(:),qipath,dtcfl,lat,i)
       do k = kts, ktop
         qci(k,i,2) = max(denqi(k)/dend(k,i),0.)
       enddo
!
       precip_i = qipath/dtcld + precip_i   ! [kgm-2s-1]
     enddo
   endif
!===============================================================================
!
! precip (den*qrsi*dz/dt) : [kgm-2s-1] ==> rain ( precip/denr*dt*1000 ) : [mm ]
! for wrf unit is mm, whereas it is m for ufs
!
!===============================================================================
   temp = 1000.  
   precip_sum = precip_r + precip_s + precip_i + precip_g + precip_h
   precip_ice = precip_s + precip_i + precip_h
!
   if(precip_sum>0.) then
     hvalue =  precip_sum/denr*dtcld*temp
     rainncv(i) = hvalue + rainncv(i)
     rain(i)    = hvalue + rain(i)
   endif
!
   if(precip_ice>0.) then
     hvalue = precip_ice/denr*dtcld*temp
     tstepsnow   = hvalue + tstepsnow
     if ( present (snowncv) .and. present (snow)) then
       snowncv(i) = hvalue + snowncv(i)
       snow(i)    = hvalue + snow(i)
      endif
   endif
!
   if(precip_g>0.) then
     hvalue = precip_g/denr*dtcld*temp
     tstepgraupel   = hvalue + tstepgraupel
     if ( present (graupelncv) .and. present (graupel)) then
       graupelncv(i) = hvalue + graupelncv(i)
       graupel(i)    = hvalue + graupel(i)
      endif
   endif
!
   if(precip_h>0.) then
     hvalue = precip_h/denr*dtcld*temp
     tstephail   = hvalue + tstephail
     if ( present (hailncv) .and. present (hail)) then
       hailncv(i) = hvalue + hailncv(i)
       hail(i)    = hvalue + hail(i)
      endif
   endif
!
   if ( present (snow) ) then
     if(precip_sum>0.) sr(i) = snowncv(i)/(rainncv(i) + qmin)
   else
     if(precip_sum>0.) sr(i) = tstepsnow/(rainncv(i) + qmin)
   endif
!-------------------------------------------------------------------------------
!
   enddo i_loop                  ! i-loops
!
!===============================================================================
!
   enddo inner_loop                    ! dtcldcr- loops
!
!===============================================================================
!
!-----------------------------------------------------------------------------
! assign local to passing variables
!
   ktop = kte
   do k = kts, ktop
     do i = its, ite
       t1(i,k) = t(k,i)
       q1(i,k) = q(k,i)
       ncr1(i,k,1) = ncr(k,i,1)
       ncr1(i,k,2) = ncr(k,i,2)
       ncr1(i,k,3) = ncr(k,i,3)
     enddo
   enddo
!
   do k = kts, ktop
     do i = its, ite
       qci1(i,k,1) = qci(k,i,1)
       qrs1(i,k,1) = qrs(k,i,1)
       qci1(i,k,2) = qci(k,i,2)
       qrs1(i,k,2) = qrs(k,i,2)
       qrs1(i,k,3) = qrs(k,i,3)
       qrs1(i,k,4) = qrs(k,i,4)
     enddo
   enddo
!
!
end subroutine udm2d
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine slope_rain(idim, kdim, ktop, qrs, den, denfac, t,                &
                         ncr,                                                  &
                         rslope, rslopeb, rslope2, rslope3, vt)
!-------------------------------------------------------------------------------
   implicit none
!
   integer       ::               idim, kdim, ktop
   real, dimension( idim , kdim),                                   &
        intent(in   ) ::                                                       &
                                                                          qrs, &
                                                                          ncr, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  real, dimension( idim, kdim),                                     &
        intent(inout   ) ::                                                    &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt
  real       ::  lamda, lamdar1m, lamdar2m, x, y, z, hvalue
  integer :: i, j, k
!----------------------------------------------------------------
!  size distributions: (x=mixing ratio, y=air density):
   lamdar2m(x,y,z)= exp(log(((pidnr*z)/(x*y)))*((.33333333)))
   lamdar1m(x,y)=   exp(log(pidn0r/(x*y))*(0.25))      ! (pidn0r/(x*y))**.25
!
   do i = 1,idim
     do k = 1,ktop
       if(qrs(i,k)<=qrmin .or. ncr(i,k)<=nrmin ) then
         rslope(i,k) = rslopermax
         rslopeb(i,k) = rsloperbmax
         rslope2(i,k) = rsloper2max
         rslope3(i,k) = rsloper3max
       else
         lamda = min(max(lamdar2m(qrs(i,k),den(i,k),ncr(i,k)),lamdarmin),      &
                 lamdarmax)
         if(qrs(i,k)<=0.1e-3)then
           hvalue = lamdar1m(qrs(i,k),den(i,k))
           lamda = max(lamda,hvalue)
         endif
         rslope(i,k) = 1./lamda
         rslopeb(i,k) = exp(log(rslope(i,k))*(bvtr))
         rslope2(i,k) = rslope(i,k)*rslope(i,k)
         rslope3(i,k) = rslope2(i,k)*rslope(i,k)
       endif
       vt(i,k) = pvtr*rslopeb(i,k)*denfac(i,k)
       if(qrs(i,k)<=0.0) vt(i,k) = 0.0
     enddo
   enddo
end subroutine slope_rain
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine slope_graupel(idim, kdim, ktop, qrs, den, denfac, t, rslope,     &
                         rslopeb, rslope2, rslope3, vt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer       ::               idim, kdim, ktop
   real, dimension( idim , kdim),                                   &
        intent(in   ) ::                                                       &
                                                                          qrs, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  real, dimension( idim, kdim),                                     &
        intent(inout   ) ::                                                    &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt
  real       ::  lamda, lamdag, x, y, z, supcol
  integer :: i, j, k
!----------------------------------------------------------------
!  size distributions: (x=mixing ratio, y=air density):
   lamdag(x,y)=   exp(log(pidn0g/(x*y))*(0.25))      ! (pidn0g/(x*y))**.25
!
   do i = 1,idim
     do k = 1,ktop
       if(qrs(i,k)<=qrmin)then
         rslope(i,k) = rslopegmax
         rslopeb(i,k) = rslopegbmax
         rslope2(i,k) = rslopeg2max
         rslope3(i,k) = rslopeg3max
       else
         lamda = min(lamdag(qrs(i,k),den(i,k)),lamdagmax)
         rslope(i,k) = 1./lamda
         rslopeb(i,k) = exp(log(rslope(i,k))*(bvtg))
         rslope2(i,k) = rslope(i,k)*rslope(i,k)
         rslope3(i,k) = rslope2(i,k)*rslope(i,k)
       endif
       vt(i,k) = pvtg*rslopeb(i,k)*denfac(i,k)
       if(qrs(i,k)<=0.0) vt(i,k) = 0.0
     enddo
   enddo
end subroutine slope_graupel
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine slope_hail(idim, kdim, ktop, qrs, den, denfac, t, rslope,        &
                         rslopeb, rslope2, rslope3, vt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer       ::               idim, kdim, ktop
   real, dimension( idim , kdim),                                   &
        intent(in   ) ::                                                       &
                                                                          qrs, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  real, dimension( idim, kdim),                                     &
        intent(inout   ) ::                                                    &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt
  real       ::  lamda, lamdah, x, y, z, supcol
  integer :: i, j, k
!----------------------------------------------------------------
!  size distributions: (x=mixing ratio, y=air density):
   lamdah(x,y)=   exp(log(pidn0h/(x*y))*(0.25))      ! (pidn0h/(x*y))**.25
!
   do i = 1,idim
     do k = 1,ktop
       if(qrs(i,k)<=qrmin)then
         rslope(i,k) = rslopehmax
         rslopeb(i,k) = rslopehbmax
         rslope2(i,k) = rslopeh2max
         rslope3(i,k) = rslopeh3max
       else
         lamda = min(lamdah(qrs(i,k),den(i,k)),lamdahmax)
         rslope(i,k) = 1./lamda
         rslopeb(i,k) = exp(log(rslope(i,k))*(bvth))
         rslope2(i,k) = rslope(i,k)*rslope(i,k)
         rslope3(i,k) = rslope2(i,k)*rslope(i,k)
       endif
       vt(i,k) = pvth*rslopeb(i,k)*denfac(i,k)
       if(qrs(i,k)<=0.0) vt(i,k) = 0.0
     enddo
   enddo
end subroutine slope_hail
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine slope_cloud(idim, kdim, ktop, qrs, ncr, den, denfac, t, qmin,    &
                          rslope, rslope2, rslope3)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer       ::               idim, kdim, ktop
   real, dimension( idim , kdim),                                   &
        intent(in   ) ::                                                       &
                                                                          qrs, &
                                                                          ncr, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  real, dimension( idim, kdim),                                     &
        intent(inout   ) ::                                                    &
                                                                       rslope, &
                                                                      rslope2, &
                                                                      rslope3
  real       ::  lamda, lamdac, x, y, z, supcol, qmin
  integer :: i, j, k
!----------------------------------------------------------------
!  size distributions: (x=mixing ratio, y=air density):
   lamdac(x,y,z)= exp(log(((pidnc*z)/(x*y)))*((.33333333)))
!
   do i = 1,idim
     do k = 1,ktop
       if(qrs(i,k)<=qmin .or. ncr(i,k)<=ncmin )then
         rslope(i,k) = rslopecmax
         rslope2(i,k) = rslopec2max
         rslope3(i,k) = rslopec3max
       else
         lamda = min(max(lamdac(qrs(i,k),den(i,k),ncr(i,k)),lamdacmin),        &
                 lamdacmax)
         rslope(i,k) = 1./lamda
         rslope2(i,k) = rslope(i,k)*rslope(i,k)
         rslope3(i,k) = rslope2(i,k)*rslope(i,k)
       endif
     enddo
   enddo
end subroutine slope_cloud
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
subroutine slope_snow(idim, kdim, ktop, qrs, den, denfac, t, rslope, rslopeb,  &
                            rslope2, rslope3, vt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer       ::               idim, kdim, ktop
   real, dimension( idim , kdim),                                   &
         intent(in   ) ::                                                      &
                                                                          qrs, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
   real, dimension( idim , kdim),                                   &
         intent(inout   ) ::                                                   &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt
   real, parameter  :: t0c = 273.15
   real, dimension( idim , kdim ) ::                                &
                                                                       n0sfac
   real       ::  lamda, lamdas, x, y, z, supcol
   integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
   lamdas(x,y,z)= exp(log((pidn0s*z)/(x*y))*(0.25))  ! (pidn0s*z/(x*y))**.25
!
   do i = 1,idim
     do k = 1, ktop
       supcol = t0c-t(i,k)
!
! n0s: intercept parameter for snow [m-4] [hdc 6]
!
       n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
       if(qrs(i,k)<=qrmin)then
         rslope(i,k) = rslopesmax
         rslopeb(i,k) = rslopesbmax
         rslope2(i,k) = rslopes2max
         rslope3(i,k) = rslopes3max
       else
         lamda = min(lamdas(qrs(i,k),den(i,k),n0sfac(i,k)),lamdasmax)
         rslope(i,k) = 1./lamda
         rslopeb(i,k) = exp(log(rslope(i,k))*(bvts))
         rslope2(i,k) = rslope(i,k)*rslope(i,k)
         rslope3(i,k) = rslope2(i,k)*rslope(i,k)
       endif
       vt(i,k) = pvts*rslopeb(i,k)*denfac(i,k)
       if(qrs(i,k)<=0.0) vt(i,k) = 0.0
     enddo
   enddo
end subroutine slope_snow
!-------------------------------------------------------------------
!
!
!-------------------------------------------------------------------
   subroutine semi_lagrange_sedim(im, km, ktop1, dendl, denfacl, tkl, dzl,     &
                                  wwl, rql, precip, dt, lat, lon)
!-------------------------------------------------------------------
!
! this routine is a semi-lagrangain forward advection for hydrometeors
! with mass conservation and positive definite advection
!
!  input :
!    im, km   - dimension
!    dendl    - dry air density
!    denfacl  - sqrt(den0/den)
!    tkl      - temperature in k
!    dzl      - depth of model layer in meter
!    wwl      - terminal velocity at model layer in m/s
!    dt       - time step
!    ktop     - top layer for computing
!    id       - kind of precip: 0 test case; 1 raindrop  2: snow
!    iter     - how many time to guess mean terminal velocity: 0 pure forward.
!               0 : use departure wind for advection
!               1 : use mean wind for advection
!               > 1 : use mean wind after iter-1 iterations
!
!  inout :
!    precip   - precipitation
!    rql      - cloud density*mixing ratio
!
! author: hann-ming henry juang <henry.juang@noaa.gov>
!         song-you hong <songyou.hong@noaa.gov>
! reference: Juang, H.-M., and S.-Y. Hong, 2010: Forward semi-Lagrangian advection
!         with mass conservation and positive definiteness for falling
!         hydrometeors. Mon.  Wea. Rev., 138, 1778-1791
!
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer               , intent(in   ) :: im, km, ktop1 
   integer               , intent(in   ) :: lat, lon
   real                  , intent(in   ) :: dt
   real, dimension(im,km), intent(in   ) :: dzl
   real, dimension(im,km), intent(in   ) :: dendl
   real, dimension(im,km), intent(in   ) :: denfacl
   real, dimension(im,km), intent(in   ) :: tkl
   real, dimension(km), intent(in   )    :: wwl
   real, dimension(km), intent(inout)    :: rql
   real, intent(inout)                   :: precip
!
!  local variables
!
   real, dimension(km)   ::                      &
                                                        dz, &
                                                        ww, &
                                                        qq, &
                                                        wd, &
                                                        wa, &
                                                       den, &
                                                    denfac, &
                                                        tk, &
                                                        qn
   real, dimension(km+1) ::                  wi, &
                                                        zi, &
                                                       qmi, &
                                                       qpi, &
                                                       dza, &
                                                        qa
   real, dimension(km+2) ::                  za
!
   integer               :: ktop, i, k, n, m, kk, kb, kt, iter, lond, latd
   real  ::                                                &
                            tl, tl2, qql, dql, qqd                   ,&
                            th, th2, qqh, dqh                        ,&
                            zsum, qsum, dim, dip                     ,&
                            zsumt, qsumt, zsumb, qsumb               ,&
                            allold, allnew, zz, dzamin, cflmax, decfl
   real  ::      tmp
!
   real, parameter       ::                                &
                            cfac = 0.05, fa1 = 9./16., fa2 = 1./16.
!
   lond = 101
   latd = 1
!
   zi(:) = 0.    ; wi(:) = 0.   ; qa(:) = 0.  
   wa(:) = 0.    ; wd(:) = 0.   ; dza(:) = 0.
   precip = 0.0  ; tmp = 0.0
!
   ktop = max(ktop1,2)
!------------------------------------------------------------------
!
   semi_loop : do i = 1,im
!
! assign local variables
     dz(:) = dzl(i,:)
     den(:) = dendl(i,:)
     denfac(:) = denfacl(i,:)
     tk(:) = tkl(i,:)
     qq(:) = rql(:)
     ww(:) = wwl(:)
!
! skip for no precipitation for all layers
     allold = 0.0
     do k = 1,ktop
       allold = allold + qq(k)
     enddo
     if(allold<=0.0) then
       cycle semi_loop
     endif
!
! compute interface value
     zi(1)=0.0
     do k = 1,ktop
       zi(k+1) = zi(k)+dz(k)
     enddo
!
! save departure wind
     wd(:) = ww(:)
     n=1
     wi(1) = ww(1)
     wi(ktop+1) = ww(ktop)
     do k=2,ktop
       wi(k) = (ww(k)*dz(k-1)+ww(k-1)*dz(k))/(dz(k-1)+dz(k))
     enddo
!
! 3rd order interpolation to get wi
     wi(1) = ww(1)
     wi(2) = 0.5*(ww(2)+ww(1))
     do k=3,ktop-1
       wi(k) = fa1*(ww(k)+ww(k-1))-fa2*(ww(k+1)+ww(k-2))
     enddo
     wi(ktop) = 0.5*(ww(ktop)+ww(ktop-1))
     wi(ktop+1) = ww(ktop)
!
! terminate of top of raingroup
     do k=2,ktop
       if( ww(k)==0.0 ) wi(k)=ww(k-1)
     enddo
!
! diffusivity of wi
     do k=ktop,1,-1
       decfl = (wi(k+1)-wi(k))*dt/dz(k)
       if( decfl > cfac ) then
         wi(k) = wi(k+1) - cfac*dz(k)/dt
       endif
     enddo
!
! compute arrival point
     do k=1,ktop+1
       za(k) = zi(k) - wi(k)*dt
     enddo
     za(ktop+2) = zi(ktop+1)   !hmhj
!
     do k=1,ktop+1  !hmhj
       dza(k) = za(k+1)-za(k)
     enddo
!
! compute deformation at arrival point
     do k=1,ktop
       tmp = qq(k)*dz(k)/dza(k)
       qa(k) = tmp
     enddo
     qa(ktop+1) = 0.0
!
! estimate value at arrival cell interface with monotone
     do k=2,ktop
       dip=(qa(k+1)-qa(k))/(dza(k+1)+dza(k))
       dim=(qa(k)-qa(k-1))/(dza(k-1)+dza(k))
       if( dip*dim<=0.0 ) then
         qmi(k)=qa(k)
         qpi(k)=qa(k)
       else
         qpi(k)=qa(k)+0.5*(dip+dim)*dza(k)
         qmi(k)=2.0*qa(k)-qpi(k)
         if( qpi(k)<0.0 .or. qmi(k)<0.0 ) then
           qpi(k) = qa(k)
           qmi(k) = qa(k)
         endif
       endif
     enddo
     qpi(1)=qa(1)
     qmi(1)=qa(1)
     qmi(ktop+1)=qa(ktop+1)
     qpi(ktop+1)=qa(ktop+1)
!
! interpolation to regular point
     qn = 0.0
     kb=1
     kt=1
     intp : do k=1,ktop
            kb=max(kb-1,1)
            kt=max(kt-1,1)
! find kb and kt
            if( zi(k)>=za(ktop+1) ) then
              exit intp
            else
              find_kb : do kk=kb,ktop
                        if( zi(k)<=za(kk+1) ) then
                          kb = kk
                          exit find_kb
                        else
                          cycle find_kb
                        endif
              enddo find_kb
              find_kt : do kk=kt,ktop+2    !hmhj
                        if( zi(k+1)<=za(kk) ) then
                          kt = kk
                          exit find_kt
                        else
                          cycle find_kt
                        endif
              enddo find_kt
              kt = kt - 1
!
! compute q with piecewise parabolic method
              if( kt==kb ) then
                tl=(zi(k)-za(kb))/dza(kb)
                th=(zi(k+1)-za(kb))/dza(kb)
                tl2=tl*tl
                th2=th*th
                qqd=0.5*(qpi(kb)-qmi(kb))
                qqh=qqd*th2+qmi(kb)*th
                qql=qqd*tl2+qmi(kb)*tl
               qn(k) = (qqh-qql)/(th-tl)
              else if( kt>kb ) then
                tl=(zi(k)-za(kb))/dza(kb)
                tl2=tl*tl
                qqd=0.5*(qpi(kb)-qmi(kb))
                qql=qqd*tl2+qmi(kb)*tl
                dql = qa(kb)-qql
                zsum  = (1.-tl)*dza(kb)
                qsum  = dql*dza(kb)
                if( kt-kb>1 ) then
                do m=kb+1,kt-1
                  zsum = zsum + dza(m)
                  qsum = qsum + qa(m) * dza(m)
                enddo
                endif
                th=(zi(k+1)-za(kt))/dza(kt)
                th2=th*th
                qqd=0.5*(qpi(kt)-qmi(kt))
                dqh=qqd*th2+qmi(kt)*th
                zsum  = zsum + th*dza(kt)
                qsum  = qsum + dqh*dza(kt)
                qn(k) = qsum/zsum
              endif
               cycle intp
             endif
!
     enddo intp
!
! rain out
     sum_precip: do k=1,ktop
                   if( za(k)<0.0 .and. za(k+1)<=0.0 ) then          !hmhj
                      precip = precip + qa(k)*dza(k)
                      cycle sum_precip
                   else if ( za(k)<0.0 .and. za(k+1)>0.0 ) then    !hmhj
                      th = (0.0-za(k))/dza(k)               !hmhj
                      th2 = th*th                           !hmhj
                      qqd = 0.5*(qpi(k)-qmi(k))             !hmhj
                      qqh = qqd*th2+qmi(k)*th               !hmhj
                      precip = precip + qqh*dza(k)    !hmhj
                      exit sum_precip
                   endif
                   exit sum_precip
     enddo sum_precip
!
! replace the new values
     rql(:) = qn(:)
!
! ----------------------------------
   enddo semi_loop
!
end subroutine semi_lagrange_sedim
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
subroutine cldf_mps_diag(idim, kdim, t, p, q, qc, qi, dx,cldf, ktop)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer,                     intent(in   ) :: idim, kdim, ktop
   real, dimension(idim),       intent(in   ) :: dx
   real, dimension(idim, kdim), intent(in   ) :: t, p, q
   real, dimension(idim, kdim), intent(in   ) :: qc
   real, dimension(idim, kdim), intent(in   ) :: qi
   real, dimension(idim, kdim), intent(out  ) :: cldf
   ! local
   integer :: i, k, kk
   real, parameter :: cldmin = 50., cldmax = 100.
   real, parameter :: clddiff = cldmax - cldmin
   real, parameter :: cldf_min = 0.5
   real :: cv_w_min,cv_i_min,cvf_min
   real :: cv_w_max,cv_i_max,cvf_max
   real :: dxkm
!
   do k = 1,ktop
     do i = 1,idim
       cldf(i,k) = 0.
     enddo
   enddo
! diagnostic method (kiaps)
   do k = 1,ktop
     do i = 1, idim
       cv_w_min = 4.82*(max(0.,qc(i,k))*1000.)**0.94/1.04
       cv_w_max = 5.77*(max(0.,qc(i,k))*1000.)**1.07/1.04
       cv_i_min = 4.82*(max(0.,qi(i,k))*1000.)**0.94/0.96
       cv_i_max = 5.77*(max(0.,qi(i,k))*1000.)**1.07/0.96
       cvf_min = cv_i_min+cv_w_min
       cvf_max = cv_i_max+cv_w_max
       dxkm    = dx(i)/1000.
       cldf(i,k)= ((dxkm-cldmin)*(cvf_max-cvf_min)+clddiff*cvf_min)/clddiff
     enddo
   enddo
   do k = 1,ktop
     do i = 1,idim
       cldf(i,k)=min(1.,max(0.,cldf(i,k)))
       if(qc(i,k)+qi(i,k)<1.e-6) then
         cldf(i,k) = 0.
       endif
       if(cldf(i,k)<0.01) cldf(i,k) = 0.
       if(cldf(i,k)>0.99) cldf(i,k) = 1.
       if(cldf(i,k)>=0.01 .and. cldf(i,k)<cldf_min) cldf(i,k) = cldf_min ! min
     enddo
   enddo
!
end subroutine cldf_mps_diag
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
subroutine udminit(den0,denr,dens,cl,cv,ccn0,allowed_to_read)
!-------------------------------------------------------------------------------
  implicit none
!-------------------------------------------------------------------------------
   real, intent(in) :: den0,denr,dens,cl,cv,ccn0
   logical, intent(in) :: allowed_to_read
!
   pi = 4.*atan(1.)
   pisq = pi * pi
   xlv1 = cl-cv
!
   pidnc = pi*denr/6.      
!
   bvtr1 = 1.+bvtr
   bvtr2 = 2.+bvtr
   bvtr3 = 3.+bvtr
   bvtr4 = 4.+bvtr
   bvtr6 = 6.+bvtr
   g1pbr = rgmma(bvtr1)
   g3pbr = rgmma(bvtr3)
   g4pbr = rgmma(bvtr4)            ! 17.837825
   g6pbr = rgmma(bvtr6)
   bvtr5 = 5.+bvtr
   bvtr7 = 7.+bvtr
   bvtr2o5 = 2.5+.5*bvtr
   bvtr3o5 = 3.5+.5*bvtr
   g2pbr = rgmma(bvtr2)
   g5pbr = rgmma(bvtr5)
   g7pbr = rgmma(bvtr7)
   g5pbro2 = rgmma(bvtr2o5)
   g7pbro2 = rgmma(bvtr3o5)
   pvtr = avtr*g5pbr/24.
   pvtrn = avtr*g2pbr
   eacrr = 1.0
   pacrr = pi*n0r*avtr*g3pbr*.25*eacrr
   pidn0r =  pi*denr*n0r
   pidnr  = 4.*pi*denr
   precr1 = 2.*pi*1.56
   precr2 = 2.*pi*.31*avtr**.5*g7pbro2
   xmmax = (dimax/dicon)**2
   roqimax = 2.08e22*dimax**8
!
   bvts1 = 1.+bvts
   bvts2 = 2.5+.5*bvts
   bvts3 = 3.+bvts
   bvts4 = 4.+bvts
   g1pbs = rgmma(bvts1)    !.8875
   g3pbs = rgmma(bvts3)
   g4pbs = rgmma(bvts4)    ! 12.0786
   g5pbso2 = rgmma(bvts2)
   pvts = avts*g4pbs/6.
   pacrs = pi*n0s*avts*g3pbs*.25
   precs1 = 4.*n0s*.65
   precs2 = 4.*n0s*.44*avts**.5*g5pbso2
   pidn0r =  pi*denr*n0r
   pidn0s =  pi*dens*n0s
   pacrc = pi*n0s*avts*g3pbs*.25*eacrc
!
   bvtg1 = 1.+bvtg                           
   bvtg2 = 2.5+.5*bvtg                       
   bvtg3 = 3.+bvtg                           
   bvtg4 = 4.+bvtg                           
   g1pbg = rgmma(bvtg1)                      
   g3pbg = rgmma(bvtg3)                      
   g4pbg = rgmma(bvtg4)                      
   pacrg = pi*n0g*avtg*g3pbg*.25             
   g5pbgo2 = rgmma(bvtg2)                    
   pvtg = avtg*g4pbg/6.                      
   precg1 = 2.*pi*n0g*.78                    
   precg2 = 2.*pi*n0g*.31*avtg**.5*g5pbgo2   
   pidn0g =  pi*deng*n0g                     
!
   rslopermax = 1./lamdarmax
   rslopesmax = 1./lamdasmax
   rslopegmax = 1./lamdagmax
   rsloperbmax = rslopermax ** bvtr
   rslopesbmax = rslopesmax ** bvts
   rslopegbmax = rslopegmax ** bvtg
   rsloper2max = rslopermax * rslopermax
   rslopes2max = rslopesmax * rslopesmax
   rslopeg2max = rslopegmax * rslopegmax
   rsloper3max = rsloper2max * rslopermax
   rslopes3max = rslopes2max * rslopesmax
   rslopeg3max = rslopeg2max * rslopegmax
!
   rslopecmax  = 1./lamdacmax
   rslopec2max = rslopecmax * rslopecmax
   rslopec3max = rslopec2max * rslopecmax
!
   g6pbgh = rgmma(2.75)
   precg3 = 2.*pi*n0g*.31*g6pbgh*sqrt(sqrt(4.*deng/3./cd))
   bvth2 = 2.5+.5*bvth
   bvth3 = 3.+bvth
   bvth4 = 4.+bvth
   g3pbh = rgmma(bvth3)
   g4pbh = rgmma(bvth4)
   g5pbho2 = rgmma(bvth2)
   pacrh = pi*n0h*avth*g3pbh*.25
   pvth = avth*g4pbh/6.
   prech1 = 2.*pi*n0h*.78
   prech2 = 2.*pi*n0h*.31*avth**.5*g5pbho2
   prech3 = 2.*pi*n0h*.31*g6pbgh*sqrt(sqrt(4.*denh/3./cd))
   pidn0h = pi*denh*n0h
!
   rslopehmax = 1./lamdahmax
   rslopehbmax = rslopehmax ** bvth
   rslopeh2max = rslopehmax * rslopehmax
   rslopeh3max = rslopeh2max * rslopehmax
!
!..set these variables needed for computing radar reflectivity.  these
!.. get used within radar_init to create other variables used in the
!.. radar module.
!
   xam_r = pi*denr/6.
   xbm_r = 3.
   xmu_r = 0.
   xam_s = pi*dens/6.
   xbm_s = 3.
   xmu_s = 0.
   xam_g = pi*dens/6.      
   xbm_g = 3.
   xmu_g = 0.
!-------------------------------------------------------------------------------
   call radar_init
!-------------------------------------------------------------------------------
!
end subroutine udminit
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
real function rgmma(x)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!     rgmma function:  use infinite product form
   real :: euler
   parameter (euler=0.577215664901532)
   real :: x, y
   integer :: i
   if(x==1.)then
     rgmma=0.
       else
     rgmma=x*exp(euler*x)
     do i=1,10000
       y=float(i)
       rgmma=rgmma*(1.000+x/y)*exp(-x/y)
     enddo
     rgmma=1./rgmma
   endif
end function rgmma
!-------------------------------------------------------------------------------
   subroutine adjust_number_concent(ktop,kdim,qq,nn,den,piconst,drconst,      &
              qqmin,nnmin,nnmax,di0,dimin,dimax)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer , intent(in ) :: ktop, kdim
   real, dimension(kdim) , intent(inout) :: qq,nn,den
   real, intent(in ) :: piconst,drconst,qqmin,nnmin,nnmax
   real, intent(in ) :: di0,dimin,dimax
! local variables
   real(kind=kind_phys) :: lamdar, diameter
   integer :: k
!-------------------------------------------------------------------------------
   do k = ktop, 1, -1
     if(qq(k)>qqmin) then
       if(nn(k) <= nnmin) then
         lamdar = drconst/di0
         nn(k) = den(k)*qq(k)*exp(log(lamdar)*3.)/piconst
       endif
       lamdar = exp(log(((piconst*nn(k))/(den(k)*qq(k))))*((.33333333)))
       diameter = drconst/lamdar
       if (diameter > dimax) then
         lamdar = drconst/dimax
         nn(k) = den(k)*qq(k)*exp(log(lamdar)*3.)/piconst
       elseif (diameter < dimin) then
         lamdar = drconst/dimin
         nn(k) = den(k)*qq(k)*exp(log(lamdar)*3.)/piconst
       endif
       nn(k) = min(nn(k),nnmax)
     else
       qq(k) = 0.0
       nn(k) = 0.0
     endif
   enddo
!
  return
end subroutine adjust_number_concent
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
subroutine find_cloud_top(im,km,ktop,qq,hvalue,ktopout)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer               , intent(in   ) :: im, km, ktop
   real, dimension(km)   , intent(in   ) :: qq
   real                  , intent(in   ) :: hvalue
   integer               , intent(inout) :: ktopout
   integer                               :: i,k
!
! do i = 1,im 
    ktopout = 0
    find_qrtop : do k = ktop,1, -1
      if(qq(k)>hvalue) then
        ktopout = k
        exit find_qrtop
      else
        cycle find_qrtop
      endif
    enddo find_qrtop
! enddo
  return
end subroutine find_cloud_top
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
subroutine ufs_mp_effective_radius (t, qc, qi, qs, rho, qmin, t0c,             &
                                nc,                                            &
                                re_qc, re_qi, re_qs, kts, kte, ii, jj)
!-------------------------------------------------------------------------------
!  compute radiation effective radii of cloud water, ice, and snow for 
!  single-moment microphysics.
!  these are entirely consistent with microphysics assumptions, not
!  constant or otherwise ad hoc as is internal to most radiation
!  schemes.  
!  coded and implemented by soo ya bae, kiaps, january 2015.
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!..sub arguments
   integer, intent(in) :: kts, kte, ii, jj
   real, intent(in) :: qmin
   real, intent(in) :: t0c
   real, dimension( kts:kte ), intent(in)::  t
   real, dimension( kts:kte ), intent(in)::  qc
   real, dimension( kts:kte ), intent(in)::  nc
   real, dimension( kts:kte ), intent(in)::  qi
   real, dimension( kts:kte ), intent(in)::  qs
   real, dimension( kts:kte ), intent(in)::  rho
   real, dimension( kts:kte ), intent(inout):: re_qc
   real, dimension( kts:kte ), intent(inout):: re_qi
   real, dimension( kts:kte ), intent(inout):: re_qs
!..local variables
   integer:: i,k
   integer :: inu_c
   real, dimension( kts:kte ):: ni
   real, dimension( kts:kte ):: rqc
   real, dimension( kts:kte ):: rnc
   real, dimension( kts:kte ):: rqi
   real, dimension( kts:kte ):: rni
   real, dimension( kts:kte ):: rqs
   real :: temp
   real :: lamdac
   real :: supcol, n0sfac, lamdas
   real :: di      ! diameter of ice in m
   real :: bfactor, bfactor2, bfactor3
   double precision :: lamc
   logical :: has_qc, has_qi, has_qs
!..minimum microphys values
   real, parameter :: r1 = 1.e-12
   real, parameter :: r2 = 1.e-6
!..mass power law relations:  mass = am*d**bm
   real, parameter :: bm_r = 3.0
   real, parameter :: obmr = 1.0/bm_r
   real, parameter :: nc0  = 3.e8
   real, parameter :: rqi0  = 50.e-3   ! 50 g m-3
!-----------------------------------------------------------------------
   has_qc = .false.
   has_qi = .false.
   has_qs = .false.
   do k = kts, kte
     ! for cloud
     rqc(k) = max(r1, qc(k)*rho(k))
     rnc(k) = max(R2, nc(k)*rho(k))
     if (rqc(k)>R1 .and. rnc(k)>R2) has_qc = .true.
     if (rqc(k)>r1) has_qc = .true.
     ! for ice
     rqi(k) = max(r1, qi(k)*rho(k))
     temp = (rho(k)*max(qi(k),qmin))
     temp = sqrt(sqrt(temp*temp*temp))
     ni(k) = min(max(5.38e7*temp,1.e3),1.e6)
     rni(k)= max(r2, ni(k)*rho(k))
     if (rqi(k)>r1 .and. rni(k)>r2) has_qi = .true.
     ! for snow
     rqs(k) = max(r1, qs(k)*rho(k))
     if (rqs(k)>r1) has_qs = .true.
   enddo
   if (has_qc) then
     do k=kts,kte
       if (rqc(k)<=R1 .or. rnc(k)<=R2) CYCLE
       lamc = (pidnc*nc(k)/rqc(k))**obmr
       re_qc(k) = max(recmin,min(0.5*(1./lamc),recmax))
     enddo
   endif
  if (has_qi) then
     do k=kts,kte
       if (rqi(k)<=r1 .or. rni(k)<=r2) cycle
       temp = t0c - t(k)
       bfactor = -2.0 + 1.0e-3*temp*sqrt(temp)*log10(rqi(k)/rqi0)
       bfactor2 = bfactor*bfactor
       bfactor3 = bfactor2*bfactor
       temp = 377.4 + 203.3*bfactor+ 37.91*bfactor2 + 2.3696*bfactor3
       re_qi(k) = max(reimin,min(temp*1.e-6,reimax))
     enddo
   endif
   if (has_qs) then
     do k=kts,kte
       if (rqs(k)<=r1) cycle
       supcol = t0c-t(k)
       n0sfac = max(min(exp(alpha*supcol),n0smax/n0s),1.)
       lamdas = sqrt(sqrt(pidn0s*n0sfac/rqs(k)))
       re_qs(k) = max(resmin,min(0.5*(1./lamdas), resmax))
     enddo
   endif
end subroutine ufs_mp_effective_radius
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
subroutine ufs_mp_reflectivity (qv1d, qr1d, qs1d, qg1d,                        &
                          nr1d,                                                &
            t1d, p1d, dbz, kts, kte, ii, jj)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!..sub arguments
   integer, intent(in):: kts, kte, ii, jj
   real, dimension(kts:kte), intent(in):: t1d, p1d
   real, dimension(kts:kte), intent(in):: qv1d, qr1d, qs1d, qg1d
   real, dimension(kts:kte), intent(in):: nr1d
   real, dimension(kts:kte), intent(inout):: dbz
!..local variables
   real, dimension(kts:kte):: temp, pres, qv, rho
   real, dimension(kts:kte):: rr, nr, rs, rg
   real:: temp_c
!
   double precision, dimension(kts:kte):: ilamr, ilams, ilamg
   double precision, dimension(kts:kte):: n0_r, n0_s, n0_g
   double precision:: lamr, lams, lamg
   logical, dimension(kts:kte):: l_qr, l_qs, l_qg
!
   real, dimension(kts:kte):: ze_rain, ze_snow, ze_graupel
   double precision:: fmelt_s, fmelt_g
!
   integer:: i, k, k_0, kbot, n
   logical:: melti
!
   double precision:: cback, x, eta, f_d
   real, parameter:: r=287.
!
!+---+
!
   do k = kts, kte
     dbz(k) = -35.0
   enddo
!+---+-----------------------------------------------------------------+
!..put column of data into local arrays.
!+---+-----------------------------------------------------------------+
   do k = kts, kte
     temp(k) = t1d(k)
     temp_c = min(-0.001, temp(k)-273.15)
     qv(k) = max(1.e-10, qv1d(k))
     pres(k) = p1d(k)
     rho(k) = 0.622*pres(k)/(r*temp(k)*(qv(k)+0.622))
!
     if (qr1d(k) > 1.e-9) then
       rr(k) = qr1d(k)*rho(k)
       nr(k) = nr1d(k)*rho(k)
       lamr = (xam_r*xcrg(3)*xorg2*nr(k)/rr(k))**xobmr
       N0_r(k) = nr(k)*xorg2*lamr**xcre(2)
       ilamr(k) = 1./lamr
       l_qr(k) = .true.
     else
       rr(k) = 1.e-12
       nr(k) = 1.e-12
       l_qr(k) = .false.
     endif
!
     if (qs1d(k) > 1.e-9) then
       rs(k) = qs1d(k)*rho(k)
       n0_s(k) = min(n0smax, n0s*exp(-alpha*temp_c))
       lams = (xam_s*xcsg(3)*n0_s(k)/rs(k))**(1./xcse(1))
       ilams(k) = 1./lams
       l_qs(k) = .true.
     else
       rs(k) = 1.e-12
       l_qs(k) = .false.
     endif
!
     if (qg1d(k) > 1.e-9) then
       rg(k) = qg1d(k)*rho(k)
       n0_g(k) = n0g
       lamg = (xam_g*xcgg(3)*n0_g(k)/rg(k))**(1./xcge(1))
       ilamg(k) = 1./lamg
       l_qg(k) = .true.
     else
       rg(k) = 1.e-12
       l_qg(k) = .false.
     endif
   enddo
!
!+---+-----------------------------------------------------------------+
!..locate k-level of start of melting (k_0 is level above).
!+---+-----------------------------------------------------------------+
   melti = .false.
   k_0 = kts
   do k = kte-1, kts, -1
      if ( (temp(k)>273.15) .and. l_qr(k) .and. l_qs(k+1) ) then
         k_0 = max(k+1, k_0)
         melti=.true.
         goto 195
      endif
   enddo
 195  continue
!+---+-----------------------------------------------------------------+
!..assume rayleigh approximation at 10 cm wavelength. rain (all temps)
!.. and non-water-coated snow and graupel when below freezing are
!.. simple. integrations of m(d)*m(d)*n(d)*dd.
!+---+-----------------------------------------------------------------+
   do k = kts, kte
      ze_rain(k) = 1.e-22
      ze_snow(k) = 1.e-22
      if (l_qr(k)) ze_rain(k) = n0_r(k)*xcrg(4)*ilamr(k)**xcre(4)
      if (l_qs(k)) ze_snow(k) = (0.176/0.93) * (6.0/pi)*(6.0/pi)     &
                              * (xam_s/900.0)*(xam_s/900.0)          &
                              * n0_s(k)*xcsg(4)*ilams(k)**xcse(4)
   enddo
!+---+-----------------------------------------------------------------+
!..special case of melting ice (snow/graupel) particles.  assume the
!.. ice is surrounded by the liquid water.  fraction of meltwater is
!.. extremely simple based on amount found above the melting level.
!.. uses code from uli blahak (rayleigh_soak_wetgraupel and supporting
!.. routines).
!+---+-----------------------------------------------------------------+
   if (melti .and. k_0>=kts+1) then
     do k = k_0-1, kts, -1
!..reflectivity contributed by melting snow
       if (l_qs(k) .and. l_qs(k_0) ) then
        fmelt_s = max(0.005d0, min(1.0d0-rs(k)/rs(k_0), 0.99d0))
        eta = 0.d0
        lams = 1./ilams(k)
        do n = 1, nrbins
           x = xam_s * xxds(n)**xbm_s
           call rayleigh_soak_wetgraupel (x,dble(xocms),dble(xobms), &
                 fmelt_s, melt_outside_s, m_w_0, m_i_0, lamda_radar, &
                 cback, mixingrulestring_s, matrixstring_s,          &
                 inclusionstring_s, hoststring_s,                    &
                 hostmatrixstring_s, hostinclusionstring_s)
           f_d = n0_s(k)*xxds(n)**xmu_s * dexp(-lams*xxds(n))
           eta = eta + f_d * cback * simpson(n) * xdts(n)
        enddo
        ze_snow(k) = sngl(lamda4 / (pi5 * k_w) * eta)
       endif
     enddo
   endif
   do k = kte, kts, -1
     dbz(k) = 10.*log10((ze_rain(k)+ze_snow(k))*1.d18)
   enddo
end subroutine ufs_mp_reflectivity
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine udm_funct_mps_setup
!-------------------------------------------------------------------------------
!
!  abstract: computes the ratio of fall velocity for mass over number
!    concentations, vm/vn, as a function of variable shape parameter.
!    the shape paramter is a diagnosed with respect to diamater of
!    drops. exact shape dependent ratio is calculated in subprogram
!    fmpsx. the current implementation computes a table with a length
!    of 9991 for diameter ranging from 10 micro to 10 mm.
!
!  usage : call funct_mps_setup
!
!  history : first development songyou hong          2023-11-27
!
!  subprograms called :
!    (fmpsx)  - inlinable function to compute velocity ratio
!
!-------------------------------------------------------------------------------
   implicit none
!
   real   , parameter  ::  xmin = 10.e-6, xmax = 10.e-3
   integer             ::  jx
   real                ::  xinc,x,d
!
   xinc = (xmax-xmin)/(nxmps-1)
   c1xmps = 1.-xmin/xinc
   c2xmps = 1./xinc
!
   do jx = 1,nxmps
     x = xmin+(jx-1)*xinc
     d = x
     tbmps(jx) = fmpsx(d)
   enddo
!
   return
   end subroutine udm_funct_mps_setup
!-------------------------------------------------------------------------------
   real function fmpsx(d)
!-------------------------------------------------------------------------------
!
!  abstract : exactly compute velocity ratio from diameter.
!    vratio = gamma(1+myu)*gamma(4+bvtr+myuu)/gamma(1+bvtr+myu)/gamma(4+myu)
!    where bvtr is the power parameter in terminal velocity, and myu is
!    the dignosed shape parameter,which is a function of drop size.
!    myu = 11.8*[1000D**(1/3)-0.7)**2 + 2, from Milbrandt and
!    Mctaggart-Cowan (JAS, 2010). D is the unit in  meters.
!    this function should be expanded inline in the calling routine.
!
!  usage :   mps=fmpsx(d)
!
!  input :
!    d        - real diameter in meters
!
!  output :
!    fmpsx    - real velocity ratio, Vm/Vn
!
!-------------------------------------------------------------------------------
!
   implicit none
!
! passing variable
!
   real   , intent(in   )  ::  d
!
! local variable
!
   real                    ::  shape_rain
!
     shape_rain =  min(11.8*(1000.*d-0.7)**2 + 2.,10.)
     fmpsx = max(rgmma(4.+shape_rain+bvtr)*rgmma(1.+shape_rain)                &
             /rgmma(1.+shape_rain+bvtr)/rgmma(4.+shape_rain),1.)
!
   return
   end function fmpsx
!-------------------------------------------------------------------------------
   real function fmps(d)
!-------------------------------------------------------------------------------
!
!  abstract : compute velocity ratio from the diameter of drops.
!    a linear interpolation is done between values in a lookup table
!    computed in funct_mps_setup.
!    input values outside table range are reset to table extrema.
!
!  usage :   mps=fmps(d)
!
!  input :
!    d        - real diameter in meters
!
!  output :
!    fmps     - real velocity ratio
!
!-------------------------------------------------------------------------------
   implicit none
!
   real   , intent(in   )            ::  d
!
   integer  ::  jx
   real     ::  xj
!
   xj = min(max(c1xmps+c2xmps*d,1.),real(nxmps))
   jx = min(xj,nxmps-1.)
   fmps = tbmps(jx)+(xj-jx)*(tbmps(jx+1)-tbmps(jx))
!
   return
   end function fmps
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine udm_funct_svp_setup
!-------------------------------------------------------------------------------
!
!  abstract: compute saturation vapor pressure table for the table lookup 
!    function fsvp.exact saturation vapor pressures are calculated in 
!    subprogram fsvpx.
!    the current implementation computes a table with a length
!    of 7501 for temperatures ranging from 180. to 330. kelvin.
!  
!  history log:
!    1991-05-07 iredell         made into inlinable function
!    1996-02-19 song-you hong   ice effect & increased range and accuracy
!    2009-10-01 jung-eun kim    f90 format with standard physics modules
!    2010-07-01 myung-seo koo   dimension allocatable with namelist input
!    2023-11-27 song-you hong   clean-up and implemented on ufs model
!
!  usage : call funct_svp_setup
!  
!  subprograms called :
!    (fsvpx)  - inlinable function to compute saturation vapor pressure
!  
!-------------------------------------------------------------------------------
   implicit none
!
   real   , parameter  ::  xmin = 180.0,                                       &
                           xmax = 330.0
   integer             ::  jx
   real                ::  xinc,x,t
!    
   xinc = (xmax-xmin)/(nxsvp-1)
   c1xsvp = 1.-xmin/xinc
   c2xsvp = 1./xinc
   c1xsvpw = c1xsvp
   c2xsvpw = c2xsvp
!    
   do jx = 1,nxsvp
     x = xmin+(jx-1)*xinc
     t = x
     tbsvp(jx) = fsvpx(t)
     tbsvpw(jx) = fsvpxw(t)
   enddo
!
   return
   end subroutine udm_funct_svp_setup
!-------------------------------------------------------------------------------
   real function fsvpx(t)
!-------------------------------------------------------------------------------
!
!  abstract : exactly compute saturation vapor pressure from temperature.
!    the clausius-clapeyron equation is integrated from the triple point
!    to get the formula
!       svp=psatk*(tr**xa)*exp(xb*(1.-tr))
!    where tr is ttp/t and other values are physical constants
!    this function should be expanded inline in the calling routine.
!
!  usage :   svp=fsvpx(t)
!
!  input :
!    t        - real temperature in kelvin
!
!  output :
!    fsvpx    - real saturation vapor pressure in pascale (pa)
!
!-------------------------------------------------------------------------------
   implicit none
!
! passing variable
!
   real   , intent(in   )  ::  t
!
! local variable
!
   real                    ::  tr
!
   tr = ttp_/t
   if (t >= ttp_) then
     fsvpx = psatk*(tr**xa)*exp(xb*(1.-tr))
   else
     fsvpx = psatk*(tr**xai)*exp(xbi*(1.-tr))
   endif
!
   return
   end function fsvpx
!-------------------------------------------------------------------------------
   real function fsvpxw(t)
!-------------------------------------------------------------------------------
!
!  abstract : same to fxvps but water only
!
!-------------------------------------------------------------------------------
   implicit none
!
   real   , intent(in   )  ::  t
   real                    ::  tr
!
   tr = ttp_/t
   fsvpxw = psatk*(tr**xa)*exp(xb*(1.-tr))
!
   return
   end function fsvpxw
!-------------------------------------------------------------------------------
   real function fsvp(t,p)
!-------------------------------------------------------------------------------
!
!  abstract : compute saturation vapor pressure from the temperature.
!    a linear interpolation is done between values in a lookup table
!    computed in funct_svp_setup.
!
!  usage :   svp=fsvp(t)
!
!  input :
!    t        - real temperature in kelvin
!    p        - real pressure in pa (optional)
!
!  output :
!    fsvp     - real saturation vapor pressure in pascals (pa)
!
!-------------------------------------------------------------------------------
   implicit none
!
   real   , intent(in   )            ::  t
   real   , intent(in   ), optional  ::  p
!
   integer  ::  jx
   real     ::  xj
!
   xj = min(max(c1xsvp+c2xsvp*t,1.),real(nxsvp))
   jx = min(xj,nxsvp-1.)
   fsvp = tbsvp(jx)+(xj-jx)*(tbsvp(jx+1)-tbsvp(jx))
!
   if (present(p)) fsvp = min(fsvp,0.99*p)
!
   return
   end function fsvp
!-------------------------------------------------------------------------------
   real function fsvp_water(t,p)
!-------------------------------------------------------------------------------
!
!  abstract : same to fsvp but water only
!
!-------------------------------------------------------------------------------
   implicit none
!
   real   , intent(in   )            ::  t
   real   , intent(in   ), optional  ::  p
!
   integer  ::  jx1
   real     ::  xj1
!
   xj1 = min(max(c1xsvpw+c2xsvpw*t,1.),real(nxsvpw))
   jx1 = min(xj1,nxsvpw-1.)
   fsvp_water = tbsvpw(jx1)+(xj1-jx1)*(tbsvpw(jx1+1)-tbsvpw(jx1))
!
   if (present(p)) fsvp_water = min(fsvp_water,0.99*p)
!
   return
   end function fsvp_water
!-------------------------------------------------------------------------------
!
end module module_mp_udm
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
