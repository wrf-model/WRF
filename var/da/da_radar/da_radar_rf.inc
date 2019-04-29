subroutine da_radar_rf (ra,rs,rg,tc1,ro1,rze)

   !-----------------------------------------------------------------------
   ! Purpose: calculate background/model radar equivalent reflectivity factor
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)    :: ra, rs, rg, tc1, ro1
   real, intent(inout) :: rze

   real                :: zrr, zds, zws, zgr
   real                :: zerr, zews, zeds, zegr

   if (trace_use) call da_trace_entry("da_radar_rf")

   ! ro1 use wrfda value, do not need to scale by 0.001

   zrr = 3.63*1.00e+9  ! rainwater
   zws = 4.26*1.00e+11 ! wet snow
   zds = 9.80*1.00e+8  ! dry snow
   zgr = 4.33*1.00e+10 ! graupel

   zerr = zrr*(ro1*max(0.0,ra))**1.75
   zews = zws*(ro1*max(0.0,rs))**1.75
   zeds = zds*(ro1*max(0.0,rs))**1.75
   zegr = zgr*(ro1*max(0.0,rg))**1.75

   if (tc1.lt.0.0) then
      rze = zerr + zeds + zegr
   else
      rze = zerr + zews + zegr
   end if

   if (rze.lt.1.0e-20) rze=1.0e-20

   if (trace_use) call da_trace_exit("da_radar_rf")

end subroutine da_radar_rf


subroutine RadZIceVar(qvp0,qra0,qsn0,qgr0,qnr0,qns0,qng0,tmk0,prs,dbz,                 &
                      miy,mjx,mkzh,in0r,in0s,in0g,rn0_r,rn0_s,rn0_g,                   &
                      rhos,rhog,dtmk,dqvp,dqra,dqsn,dqgr,dqnr,dqns,dqng,zmm,tlopt,     &
                      gropt,zmm_ref)

!===================================================================
!      Following Jung et al., 2008
!===================================================================

  implicit none
  
  integer,intent(in) :: miy,mjx,mkzh,in0r,in0s,in0g
  integer,intent(in) :: tlopt,gropt  ! tlopt=0 nonlinear,=1 tangent linear; gropt=0 tangent linear; >=1 adjoint

  integer,parameter  :: dualpol_opt=0 ! completed but not yet been tested

  real           ::  rn0_r,rn0_s,rn0_g    ! intercept parameter 1/m^4
  real           ::  qra0,qsn0,qgr0, &    ! these are read in background states
                     qnr0,qns0,qng0, &
	             tmk0,qvp0

  real           :: qra,qvp,        &    ! clone the background
                    qsn,qgr,tmk,    &
                    prs,dbz,qnr,    &
                    qns,qng,ref,    &
                    zrs,zss,zhs,    &
                    zmm,zmm_ref,    &
 ! above: background, below: increment for tlopt==1 and gropt==0 but adjoint for tlopt==1 and gropt>=1
                    dqra,dqsn,dqgr, &    
                    dqnr,dqns,dqng, &
	            dtmk,dqvp

  real           :: rgas=287.04
  real           :: z_e

  integer        :: i,j,k,ii,jj,kk

! reflectivity operator constant
  real,parameter :: rdrwave = 107.0  ! unit mm S band
  real,parameter :: lambda = rdrwave
  real,parameter :: Kw2 = 0.93
  real           :: pi = 3.1415926
  real,parameter :: mm3todBZ=1.0E+9
  real           :: rhor = 1000    ! kg m^-3 rainwater density
  real           :: rhos           ! kg m^-3 snow density
  real           :: rhog           ! kg m^-3 graupel density
  real           :: rhoair         ! air density

  real :: rhows,rhowg  !  wet snow, wet graupel

! temporal mixing ratio
  real           :: prain  ! pure rain mixing ratio
  real           :: dsnow  ! dry snow mixing ratio
  real           :: dgr    ! dry graupel mixing ration 
  real           :: wsnow  ! wet snow mixing ratio
  real           :: wgr    ! wet graupel mixing ratio

!     parameters for rain            
  real           :: alpha_ra = 4.28e-4
  real           :: alpha_rb = 4.28e-4
  real           :: beta_ra = 3.04
  real           :: beta_rb = 2.77
  real           :: alphar = 0
  real           :: dr = 3
  real           :: cr = 3.1415926/6
  real           :: zrh,zrv
     
  real           :: para10r  ! =(1.-(2.*betarx+1.0)/4.)
  real           :: para14r  ! mm3todBZ*(4*lambda**4*alpharx**2/(pi**4*Kw2)) 
                             ! *(pi*rhor/rhoa)**(p2/4.)**(n0r)**para10r         
                             ! *gamma(-(2.*betarx+1.0)*1d0)  
                             ! for rainwater 
  real           :: ronv,gonv,sonv                ! intercept parameter 

! parameters for snow and graupel/hail

  integer,parameter :: npara_alpharxa=7           
  real           :: para_alpha_rxa(npara_alpharxa,2) ! second dimension: 1 for snow 
                                                     ! 2 for hail/graupel
  real,save      :: para_alpha_rxb(npara_alpharxa,2)       ! precalculated coefficients in contribution equations of ice species
  real           :: phimean=0,sigma,ice_abc(3),ice_bac(3)  ! A, B, and C in contribution equations of ice species
  real           :: fw                                  ! water fraction
  real           :: pxabk_all(3)                        !
  real           :: pxkh,pxkv                           ! the sum of fwx term
  real           :: pxkh_tlr,pxkv_tlr,pxkh_tlx,pxkv_tlx ! particle derivative of pxkh/pxkv for rain (tlr) and ice species (tlx)
  real           :: zsh,zsv,zgh,zgv,zdsh,zdsv,zdgh,zdgv ! contribution from snow/graupel
                                                        ! (thelast character means horizontal/vertical)

  real           :: para1sg   ! para1sg=mm3todBZ*gamma(7)*lambda**4/pi**4/Kw2
  real           :: para2sg   ! para2sg=(pi*rhox/rhoa)**1.75

  real           :: cs=3.1415926/6
  real           :: cice=440. 
  real           :: ds=3.
  real           :: dg=3. 
  real           :: alphas = 0
  real           :: alphag = 0

  real           :: alpha_rdsa=0.194*10.**(-4)
  real           :: alpha_rdsb=0.191*10.**(-4)
  real           :: alpha_rdha=0.191*10.**(-3)
  real           :: alpha_rdhb=0.165*10.**(-3)

  real           :: zh_e,zv_e
 
  ! for dry graupel 
  REAL,PARAMETER :: sigmahd = 1.0472
  REAL,PARAMETER :: Ahd = 0.4308
  REAL,PARAMETER :: Bhd = 0.3192
  REAL,PARAMETER :: Chd = 0.1250
  REAL,PARAMETER :: Dhd = 0.3750
  REAL,PARAMETER :: Ckhd = 0.1116
  ! for dry snow
  REAL,PARAMETER :: sigmas = 0.3491
  REAL,PARAMETER :: Asd = 0.8140
  REAL,PARAMETER :: Bsd = 0.0303
  REAL,PARAMETER :: Csd = 0.0778
  REAL,PARAMETER :: Dsd = 0.4221
  REAL,PARAMETER :: Cksd = 0.7837

  real           :: ice_abc_d(3),ice_bac_d(3)
  real           :: pdfrrs,pdfrrg,pdfsrs,pdfgrg    ! partial derivative of F (r&s,r&g) with respect to (r,s,g) 
  real           :: pdfrhot,pdfrhoq                ! partial derivative of rho for t and qv
  
  real           :: prain_coef,dsnow_coef,dgr_coef ! coefficient to determine the ratio of 
                                                   !pure rain, dry swno/graupel in total qr and qs/qg, respectively.
  real           :: qthres

  real           :: temr01,temr02,temr03    ! temporary variable for real
  integer        :: temi01,temi02,temi03    ! temporary variable for integer
  
  !---------------------------------------------------------------------------------

  data para_alpha_rxa(:,1)/0.194e-4,7.094e-4,2.135e-4,-5.225e-4,0,0,0/
  data para_alpha_rxb(:,1)/0.191e-4,6.916e-4,-2.841e-4,-1.160e-4,0,0,0/
  data para_alpha_rxa(:,2)/0.191e-4,2.39e-4,-12.57e-4,38.71e-4,-65.53e-4,56.16e-4,  &
                          -18.98e-4/
  data para_alpha_rxb(:,2)/0.165e-4,1.72e-4,-9.92e-4,32.15e-4,-56.0e-4,48.83e-4,   &
                          -16.69e-4/
  qthres=rf_qthres

  qra=qra0
  qsn=qsn0
  qgr=qgr0
  tmk=tmk0
  qvp=qvp0
  if(qra0<qthres) qra=qthres
  if(qsn0<qthres) qsn=qthres
  if(qgr0<qthres) qgr=qthres

  if(tlopt>=1.and.gropt>=1) then
    dqra=0
    dqgr=0
    dqsn=0
    dqvp=0
    dtmk=0
    dqnr=0
    dqns=0
    dqng=0
  endif

  zrh=0
  zsh=0
  zgh=0
  zdsh=0
  zdgh=0
  zrv=0
  zsv=0
  zgv=0
  zdsv=0
  zdgv=0
 
  if(qra<qthres.and.qsn<qthres.and.qgr<qthres) then
     zrs=0
     zss=0
     zhs=0
     ref=0
     zmm=0
     dbz=0
     return
  endif

  call rhoair_tangent()

!-------------------------------------------------------
! Calculate variable intercept parameters if wanted
!-------------------------------------------------------
  call prepare_interceptpara()
!-------------------------------------------------------
! Calculate mixing ratios (pure rain, dry snow/graupel, wet snow/graupel)
!-------------------------------------------------------
  call prepare_mixingratios()  

  call prepare_zmm_adjoint() 
		 
! ==================FOR RAIN=============================
  call parameter_zrx(para10r,para14r,1.0,rhor,cr,dr,alphar,     &
                     beta_ra,alpha_ra,mm3todBZ,lambda,Kw2,pi,   &
                     ronv)

  if(in0r.eq.1) then
    ! for two moment microphysics scheme, not yet completed
  else
    if(tlopt==0) zrh=para14r*(rhoair*prain)**(1.-para10r)
    if(tlopt>=1) temr01=para_rain_tangent()
    if(tlopt>=1.and.gropt==0) call rain_tangent(zrh,temr01)
    if(tlopt>=1.and.gropt>=1) call rain_adjoint(temr01)
  endif

  if(dualpol_opt==1) then
    call parameter_zrx(para10r,para14r,1.0,rhor,cr,dr,alphar,     &
                       beta_rb,alpha_rb,mm3todBZ,lambda,Kw2,pi,   &
                       ronv)

    if(in0r.eq.1) then
     ! for two moment microphysics scheme, not yet completed
    else
      if(tlopt==0) zrv=para14r*(rhoair*prain)**(1.-para10r)
      if(tlopt>=1) temr01=para_rain_tangent()
      if(tlopt>=1.and.gropt==0) call rain_tangent(zrv,temr01) 
      if(tlopt>=1.and.gropt>=1) call rain_adjoint(temr01)
    endif
  endif
  zrs=zrh 
! ==================FOR snow=============================
  if(rf_noice==0) then
! -------------------for wet snow----------------------
    fw=waterfraction(qra,qsn)
    rhows=rhos*(1.-fw**2)+rhor*fw**2

    call parameter_zxx(para1sg,para2sg,1.0,rhows,                   &
                       cs,ds,alphas,mm3todBZ,lambda,Kw2,pi,         &
                       sonv)

    sigma=sigma_in_abc(qsn,fw,1)  ! for snow
    call calc_ice_abc(phimean,sigma,ice_abc)

    ice_bac(1)=ice_abc(2)
    ice_bac(2)=ice_abc(1)
    ice_bac(3)=ice_abc(3)
    pxkh=0
    pxkv=0
    pxkh_tlr=0
    pxkv_tlr=0
    pxkh_tlx=0
    pxkv_tlx=0
    call cal_tangent_fw4wetice(1,qsn)

     if(in0s.eq.1) then
      ! for two moment microphysics scheme, not yet completed
     else
       if(tlopt==0) zsh= para1sg*para2sg*sonv**(-0.75)*(rhoair*wsnow)**(1.75)*pxkh
       if(tlopt>=1) temr01=para_wetsnow_tangent_dqwx()
       if(tlopt>=1) temr02=para_wetsnow_tangent_dfwx()
       if(tlopt>=1.and.gropt==0) call wetsnow_tangent(zsh,temr01,temr02,pxkh,pxkh_tlr,pxkh_tlx)
       if(tlopt>=1.and.gropt>=1) call wetsnow_adjoint(temr01,temr02,pxkh,pxkh_tlr,pxkh_tlx)
     endif

     if(dualpol_opt==1) then
       if(in0s.eq.1) then
         ! for two moment microphysics scheme, not yet completed
       else
         if(tlopt==0) zsv= para1sg*para2sg*sonv**(-0.75)*(rhoair*wsnow)**(1.75)*pxkv
         if(tlopt>=1) temr01=para_wetsnow_tangent_dqwx()
         if(tlopt>=1) temr02=para_wetsnow_tangent_dfwx()
         if(tlopt>=1.and.gropt==0) call wetsnow_tangent(zsv,temr01,temr02,pxkv,pxkv_tlr,pxkv_tlx) 
         if(tlopt>=1.and.gropt>=1) call wetsnow_adjoint(temr01,temr02,pxkv,pxkv_tlr,pxkv_tlx)
       endif
     endif

!  -------------------for dry snow----------------------
     call parameter_zxx(para1sg,para2sg,1.0,rhos,                    &
                        cs,ds,alphas,mm3todBZ,lambda,Kw2,pi,         &
                        sonv)

     ice_abc_d(1)=Asd
     ice_abc_d(2)=Bsd
     ice_abc_d(3)=Csd
     ice_bac_d(1)=Bsd
     ice_bac_d(2)=Asd						
					
     pxabk_all(1)=alpha_rdsa**2
     pxabk_all(2)=alpha_rdsb**2
     pxabk_all(3)=alpha_rdsb*alpha_rdsa
     pxkh=pkx(ice_abc_d,pxabk_all)
     pxkv=pkx(ice_bac_d,pxabk_all)

     if(in0s.eq.1) then
       ! for two moment microphysics scheme, not yet completed        
     else
       if(tlopt==0) zdsh=para1sg*para2sg*sonv**(-0.75)*(rhoair*dsnow)**(1.75)*pxkh
       if(tlopt>=1) temr01=para_drysnow_tangent_dqwx()
       if(tlopt>=1.and.gropt==0) call drysnow_tangent(zdsh,temr01,pxkh) 
       if(tlopt>=1.and.gropt>=1) call drysnow_adjoint(temr01,pxkh)
     endif

     if(dualpol_opt==1) then
       if(in0s.eq.1) then
        ! for two moment microphysics scheme, not yet completed
       else
         if(tlopt==0) zdsv=para1sg*para2sg*sonv**(-0.75)*(rhoair*dsnow)**(1.75)*pxkv
         if(tlopt>=1) temr01=para_drysnow_tangent_dqwx()
         if(tlopt>=1.and.gropt==0) call drysnow_tangent(zdsv,temr01,pxkv)
         if(tlopt>=1.and.gropt>=1) call drysnow_adjoint(temr01,pxkv) 
       endif
     endif
     zss=zsh+zdsh

! ==================FOR graupel==========================
! -------------------for wet graupel---------------------

    fw=waterfraction(qra,qgr)
    rhowg=rhog*(1.-fw**2)+rhor*fw**2

    call parameter_zxx(para1sg,para2sg,1.0,rhowg,                    &
                       cice,dg,alphag,mm3todBZ,lambda,Kw2,pi,        &
                       gonv)

    sigma=sigma_in_abc(qgr,fw,2)  ! for snow
    call calc_ice_abc(phimean,sigma,ice_abc)

    ice_bac(1)=ice_abc(2)
    ice_bac(2)=ice_abc(1)
    ice_bac(3)=ice_abc(3)
    pxkh=0
    pxkv=0
    pxkh_tlr=0
    pxkv_tlr=0
    pxkh_tlx=0
    pxkv_tlx=0
    call cal_tangent_fw4wetice(2,qgr)

    if(in0g.eq.1) then
       ! for two moment microphysics scheme, not yet completed
    else
      if(tlopt==0) zgh= para1sg*para2sg*gonv**(-0.75)*(rhoair*wgr)**(1.75)*pxkh
      if(tlopt>=1) temr01=para_wetgraupel_tangent_dqwx()
      if(tlopt>=1) temr02=para_wetgraupel_tangent_dfwx()
      if(tlopt>=1.and.gropt==0) call wetgraupel_tangent(zgh,temr01,temr02,pxkh,pxkh_tlr,pxkh_tlx)
      if(tlopt>=1.and.gropt>=1) call wetgraupel_adjoint(temr01,temr02,pxkh,pxkh_tlr,pxkh_tlx)
    endif

    if(dualpol_opt==1) then
       if(in0g.eq.1) then
       ! for two moment microphysics scheme, not yet completed  
       else
         if(tlopt==0) zgv= para1sg*para2sg*sonv**(-0.75)*(rhoair*wgr)**(1.75)*pxkv
         if(tlopt>=1) temr01=para_wetgraupel_tangent_dqwx()
         if(tlopt>=1) temr02=para_wetgraupel_tangent_dfwx()
         if(tlopt>=1.and.gropt==0) call wetgraupel_tangent(zgv,temr01,temr02,pxkv,pxkv_tlr,pxkv_tlx) 
         if(tlopt>=1.and.gropt>=1) call wetgraupel_adjoint(temr01,temr02,pxkv,pxkv_tlr,pxkv_tlx)
       endif
     endif
! --------------------------for dry graupel----------------------
     call parameter_zxx(para1sg,para2sg,1.0,rhog,                     &
                        cice,dg,alphag,mm3todBZ,lambda,Kw2,pi,        &
                        gonv)

     ice_abc_d(1)=Ahd
     ice_abc_d(2)=Bhd
     ice_abc_d(3)=Chd
     ice_bac_d(1)=Bhd
     ice_bac_d(2)=Ahd						
						
     pxabk_all(1)=alpha_rdha**2
     pxabk_all(2)=alpha_rdhb**2
     pxabk_all(3)=alpha_rdhb*alpha_rdha
     pxkh=pkx(ice_abc_d,pxabk_all)
     pxkv=pkx(ice_bac_d,pxabk_all)

     if(in0s.eq.1) then
      ! for two moment microphysics scheme, not yet completed
     else
       if(tlopt==0) zdgh=para1sg*para2sg*gonv**(-0.75)*(rhoair*dgr)**(1.75)*pxkh
       if(tlopt>=1) temr01=para_drygraupel_tangent_dqwx()
       if(tlopt>=1.and.gropt==0) call drygraupel_tangent(zdgh,temr01,pxkh) 
       if(tlopt>=1.and.gropt>=1) call drygraupel_adjoint(temr01,pxkh) 
     endif

     if(dualpol_opt==1) then
       if(in0s.eq.1) then
        ! for two moment microphysics scheme, not yet completed
       else
         if(tlopt==0) zdgv=para1sg*para2sg*gonv**(-0.75)*(rhoair*dgr)**(1.75)*pxkv
         if(tlopt>=1) temr01=para_drygraupel_tangent_dqwx()
         if(tlopt>=1.and.gropt==0) call drygraupel_tangent(zdgv,temr01,pxkv)  
         if(tlopt>=1.and.gropt>=1) call drygraupel_adjoint(temr01,pxkv)
       endif
     endif
     zhs=zgh+zdgh
! ============================done=================================
  endif  !(if rf_noice == 0)

  z_e =zrh+zsh+zgh+zdsh+zdgh
  zv_e=zrv+zsv+zgv+zdsv+zdgv
!-------------------------------------
! Convert to dBZ
!-------------------------------------
  if(tlopt==0) then
     dbz = max(0.,10. * log10(z_e))
  else
     if(zmm>1.0) then
        dbz = 10./(zmm*log(10.0))*z_e !ref
     else
        dbz =0.0
     endif
  endif
! save z_e mm^6 mm^-3
  zmm=z_e

  !==================================================================
  !                                                                 !
  !   subroutines and fucntions used in RadZIceVar                  !
  !                                                                 !
  !==================================================================
  contains

  subroutine rhoair_tangent()
   !----------------------------------------------------------------
   ! purpose: calculate the air density and its tangent linear 
   !----------------------------------------------------------------
    rhoair=prs*1./(rgas*virtual(tmk,qvp))      ! air density
    if(tlopt>=1) then
      pdfrhot=-prs*1./(rgas*virtual(tmk,qvp))*(1./tmk)
      pdfrhoq=-prs*1./(rgas*virtual(tmk,qvp))*(0.61/(1+0.61*qvp))
    endif
    pdfrhot=0 ! not used in current version
    pdfrhoq=0 ! not used in current version

  end subroutine rhoair_tangent


  subroutine prepare_interceptpara()
   !----------------------------------------------------------------------
   ! purpose: assigne the intercept parameter value and convert the unit 
   !----------------------------------------------------------------------
    if (in0s.eq.1) then
       sonv = rn0_s  ! for diagnosing n0 from number concentration, not yet completed
    else
       sonv = rn0_s
    endif

    if (in0g.eq.1) then
      gonv = rn0_g  ! for diagnosing n0 from number concentration, not yet completed
    else
      gonv = rn0_g
    endif

    if (in0r.eq.1) then
      ronv = rn0_r  ! for diagnosing n0 from number concentration, not yet completed
    else
      ronv = rn0_r
    endif

    ronv=ronv*1.e-12   ! convert unit
    gonv=gonv*1.e-12   ! convert unit
    sonv=sonv*1.e-12   ! convert unit
  end subroutine prepare_interceptpara

  subroutine prepare_mixingratios()
   !------------------------------------------------------------------------------
   ! purpose: calculate the mixing ratios of pure rainwater, dry/wet snow/graupel
   !------------------------------------------------------------------------------
     prain_coef=1-upper_f(qra,qgr,qthres,2)-upper_f(qra,qsn,qthres,1)
     if(prain_coef<0.0) then
       prain_coef=0
     endif
     prain=prain_coef*qra

     dsnow_coef=1-upper_f(qra,qsn,qthres,1)
     if(dsnow_coef<0.0) then
       dsnow_coef=0
     endif
     dsnow=dsnow_coef*qsn
     wsnow=(1-dsnow_coef)*(qsn+qra)

     dgr_coef=1-upper_f(qra,qgr,qthres,2)
     if(dgr_coef<0.0) then
       dgr_coef=0.0
     endif
     dgr=dgr_coef*qgr
     wgr=(1-dgr_coef)*(qgr+qra)

     if(tlopt>=1) then
       pdfrrs=0
       pdfrrg=0
       pdfsrs=0
       pdfgrg=0
     endif

  end subroutine prepare_mixingratios

  subroutine prepare_zmm_adjoint()

   !----------------------------------------------------------------
   ! purpose: calculate the adjoint of the logarithmic reflectivity
   !----------------------------------------------------------------

    if(tlopt>=1.and.gropt==2) then
      if(zmm>1.0) then
         zmm_ref=10./(zmm*log(10.0))*zmm_ref
      else
         zmm_ref=0.0
      endif
    endif

  end subroutine prepare_zmm_adjoint

  subroutine cal_tangent_fw4wetice(species_flg,qx) 

   !---------------------------------------------------------------------------------------------
   ! purpose: calculate the sum of fw term and its tangent linear for the contribution from wet ice
   !--------------------------------------------------------------------------------------------

    integer :: species_flg  ! 1 for snow 2 for graupel
    real    :: qx           ! mixing ratio

    do kk=0,2*npara_alpharxa-1
       pxabk_all(1)=pxabk(para_alpha_rxa(:,species_flg),para_alpha_rxa(:,species_flg), &
                          kk,npara_alpharxa)
       pxabk_all(2)=pxabk(para_alpha_rxb(:,species_flg),para_alpha_rxb(:,species_flg), &
                          kk,npara_alpharxa)
       pxabk_all(3)=pxabk(para_alpha_rxa(:,species_flg),para_alpha_rxb(:,species_flg), &
                          kk,npara_alpharxa)
       pxkh=pxkh+pkx(ice_abc,pxabk_all)*fw**kk
       pxkv=pxkv+pkx(ice_bac,pxabk_all)*fw**kk
       if(tlopt==2.and.kk>=1) then
          pxkh_tlr=pxkh_tlr+pkx(ice_abc,pxabk_all)*kk*fw**kk*(1./qra-1./(qra+qx))
          pxkv_tlr=pxkv_tlr+pkx(ice_bac,pxabk_all)*kk*fw**kk*(1./qra-1./(qra+qx))
          pxkh_tlx=pxkh_tlx+pkx(ice_abc,pxabk_all)*kk*fw**kk*(-1./(qra+qx))
          pxkv_tlx=pxkv_tlx+pkx(ice_bac,pxabk_all)*kk*fw**kk*(-1./(qra+qx))
       endif
    enddo

  end subroutine cal_tangent_fw4wetice

  real function para_rain_tangent()

    para_rain_tangent=para14r*(1.-para10r)*(rhoair*prain)**(-para10r)

  end function para_rain_tangent

  subroutine rain_tangent(zrhv,para_rain)

   !----------------------------------------------------------------------
   ! purpose: calculate the tangent linear for rainwater
   !----------------------------------------------------------------------
    real :: zrhv
    real :: para_rain

    zrhv=para_rain*rhoair*prain_coef*dqra         &
        +para_rain*prain*pdfrhot*dtmk             &
        +para_rain*prain*pdfrhoq*dqvp             &
        -para_rain*(pdfrrs+pdfrrg)*qra*dqra       &
        -para_rain*pdfsrs*qra*dqsn                &
        -para_rain*pdfgrg*qra*dqgr

  end subroutine rain_tangent


  subroutine rain_adjoint(para_rain)
   !----------------------------------------------------------------------
   ! purpose: calculate the adjoint associated with the contribution from rainwater
   !----------------------------------------------------------------------
    real :: para_rain

    dqra=dqra+para_rain*rhoair*prain_coef*zmm_ref &
             -para_rain*(pdfrrs+pdfrrg)*qra*zmm_ref
    dqsn=dqsn-para_rain*pdfsrs*qra*zmm_ref
    dqgr=dqgr-para_rain*pdfgrg*qra*zmm_ref
    dtmk=dtmk+para_rain*prain*pdfrhot*zmm_ref
    dqvp=dqvp+para_rain*prain*pdfrhoq*zmm_ref

  end subroutine rain_adjoint


  real function para_wetsnow_tangent_dqwx()

    para_wetsnow_tangent_dqwx=1.75*para1sg*para2sg*sonv**(-0.75)*(rhoair*wsnow)**(0.75)

  end function para_wetsnow_tangent_dqwx

  real function para_wetsnow_tangent_dfwx()

    para_wetsnow_tangent_dfwx=para1sg*para2sg*sonv**(-0.75)*(rhoair*wsnow)**(1.75)

  end function para_wetsnow_tangent_dfwx

  subroutine wetsnow_tangent(zshv,para_wetsnow_dqwx,para_wetsnow_dfwx,pxkhv,pxkhv_tlr,pxkhv_tlx)
   !----------------------------------------------------------------------
   ! purpose: calculate the tangent linear for wet snow
   !----------------------------------------------------------------------
    real :: zshv
    real :: para_wetsnow_dqwx,para_wetsnow_dfwx
    real :: pxkhv,pxkhv_tlr,pxkhv_tlx

    zshv= para_wetsnow_dqwx*pxkhv*rhoair*(1-dsnow_coef)*dqsn   &
         +para_wetsnow_dqwx*pxkhv*rhoair*(1-dsnow_coef)*dqra   &
         +para_wetsnow_dqwx*pxkhv*wsnow*pdfrhot*dtmk           &
         +para_wetsnow_dqwx*pxkhv*wsnow*pdfrhoq*dqvp           &
         +para_wetsnow_dqwx*pxkhv*pdfrrs*qsn*dqra              &
         +para_wetsnow_dqwx*pxkhv*pdfsrs*qsn*dqsn              &
         +para_wetsnow_dfwx*pxkhv_tlr*dqra                     &
         +para_wetsnow_dfwx*pxkhv_tlx*dqsn 

  end subroutine wetsnow_tangent


  subroutine wetsnow_adjoint(para_wetsnow_dqwx,para_wetsnow_dfwx,pxkhv,pxkhv_tlr,pxkhv_tlx)

   !----------------------------------------------------------------------
   ! purpose: calculate the adjoint associated with the contribution from wet snow
   !----------------------------------------------------------------------

    real :: para_wetsnow_dqwx,para_wetsnow_dfwx
    real :: pxkhv,pxkhv_tlr,pxkhv_tlx

    dqsn=dqsn+para_wetsnow_dqwx*pxkhv*rhoair*(1-dsnow_coef)*zmm_ref &
             +para_wetsnow_dqwx*pxkhv*pdfsrs*qsn*zmm_ref
    dqra=dqra+para_wetsnow_dfwx*pxkhv_tlr*zmm_ref &
             +para_wetsnow_dqwx*pxkhv*pdfrrs*qsn*zmm_ref &
             +para_wetsnow_dqwx*pxkhv*rhoair*(1-dsnow_coef)*zmm_ref
    dqsn=dqsn+para_wetsnow_dfwx*pxkhv_tlx*zmm_ref
    dtmk=dtmk+para_wetsnow_dqwx*pxkhv*wsnow*pdfrhot*zmm_ref
    dqvp=dqvp+para_wetsnow_dqwx*pxkhv*wsnow*pdfrhoq*zmm_ref

  end subroutine wetsnow_adjoint


  real function para_drysnow_tangent_dqwx()
  
    para_drysnow_tangent_dqwx=1.75*para1sg*para2sg*sonv**(-0.75)*(rhoair*dsnow)**(0.75)
   
  end function para_drysnow_tangent_dqwx

  subroutine drysnow_tangent(zdshv,para_drysnow_dqwx,pxkhv)
   !----------------------------------------------------------------------
   ! purpose: calculate the tangent linear for dry snow
   !----------------------------------------------------------------------
    real :: zdshv
    real :: para_drysnow_dqwx
    real :: pxkhv

    zdsh=para_drysnow_dqwx*pxkhv*rhoair*dsnow_coef*dqsn         &
        +para_drysnow_dqwx*pxkhv*dsnow*pdfrhot*dtmk             &
        +para_drysnow_dqwx*pxkhv*dsnow*pdfrhoq*dqvp             &
        -para_drysnow_dqwx*pxkhv*pdfrrs*qsn*dqra                &
        -para_drysnow_dqwx*pxkhv*pdfsrs*qsn*dqsn

  end subroutine drysnow_tangent

  subroutine drysnow_adjoint(para_drysnow_dqwx,pxkhv)
   !----------------------------------------------------------------------
   ! purpose: calculate the adjoint associated with the contributino from dry snow
   !----------------------------------------------------------------------
    real :: para_drysnow_dqwx
    real :: pxkhv
  
    dqsn=dqsn+para_drysnow_dqwx*pxkhv*rhoair*dsnow_coef*zmm_ref &
             -para_drysnow_dqwx*pxkhv*pdfsrs*qsn*zmm_ref
    dqra=dqra-para_drysnow_dqwx*pxkhv*pdfrrs*qsn*zmm_ref
    dtmk=dtmk+para_drysnow_dqwx*pxkhv*dsnow*pdfrhot*zmm_ref
    dqvp=dqvp+para_drysnow_dqwx*pxkhv*dsnow*pdfrhoq*zmm_ref   

  end subroutine drysnow_adjoint


  real function para_wetgraupel_tangent_dqwx()

    para_wetgraupel_tangent_dqwx=1.75*para1sg*para2sg*gonv**(-0.75)*(rhoair*wgr)**(0.75)
  
  end function para_wetgraupel_tangent_dqwx

  real function para_wetgraupel_tangent_dfwx()

    para_wetgraupel_tangent_dfwx=para1sg*para2sg*gonv**(-0.75)*(rhoair*wgr)**(1.75) 

  end function para_wetgraupel_tangent_dfwx

  subroutine wetgraupel_tangent(zghv,para_wetgraupel_dqwx,para_wetgraupel_dfwx,pxkhv,pxkhv_tlr,pxkhv_tlx)
   !----------------------------------------------------------------------
   ! purpose: calculate the tangent linear for wet graupel
   !----------------------------------------------------------------------
    real :: zghv
    real :: para_wetgraupel_dqwx,para_wetgraupel_dfwx
    real :: pxkhv,pxkhv_tlr,pxkhv_tlx
  
    zghv= para_wetgraupel_dqwx*pxkhv*rhoair*(1-dgr_coef)*dqgr         &
         +para_wetgraupel_dqwx*pxkhv*rhoair*(1-dgr_coef)*dqra         &
         +para_wetgraupel_dqwx*pxkhv*wgr*pdfrhot*dtmk                 &
         +para_wetgraupel_dqwx*pxkhv*wgr*pdfrhoq*dqvp                 &
         +para_wetgraupel_dqwx*pxkhv*pdfrrg*qgr*dqra                  &
         +para_wetgraupel_dqwx*pxkhv*pdfgrg*qgr*dqgr                  &
         +para_wetgraupel_dfwx*pxkhv_tlr*dqra                         &
         +para_wetgraupel_dfwx*pxkhv_tlx*dqgr  

  end subroutine wetgraupel_tangent

  subroutine wetgraupel_adjoint(para_wetgraupel_dqwx,para_wetgraupel_dfwx,pxkhv,pxkhv_tlr,pxkhv_tlx)
   !----------------------------------------------------------------------
   ! purpose: calculate the adjoint associated with the contribution from wet graupel
   !----------------------------------------------------------------------
    real :: para_wetgraupel_dqwx,para_wetgraupel_dfwx
    real :: pxkhv,pxkhv_tlr,pxkhv_tlx

    dqgr=dqgr+para_wetgraupel_dqwx*pxkhv*rhoair*(1-dgr_coef)*zmm_ref &
             +para_wetgraupel_dqwx*pxkhv*pdfgrg*qgr*zmm_ref
    dqra=dqra+para_wetgraupel_dfwx*pxkhv_tlr*zmm_ref   &
             +para_wetgraupel_dqwx*pxkhv*pdfrrg*qgr*zmm_ref &
             +para_wetgraupel_dqwx*pxkhv*rhoair*(1-dgr_coef)*zmm_ref
    dqgr=dqgr+para_wetgraupel_dfwx*pxkhv_tlx*zmm_ref
    dtmk=dtmk+para_wetgraupel_dqwx*pxkhv*wgr*pdfrhot*zmm_ref
    dqvp=dqvp+para_wetgraupel_dqwx*pxkhv*wgr*pdfrhoq*zmm_ref

  end subroutine wetgraupel_adjoint


  real function para_drygraupel_tangent_dqwx()

    para_drygraupel_tangent_dqwx=1.75*para1sg*para2sg*gonv**(-0.75)*(rhoair*dgr)**(0.75)

  end function para_drygraupel_tangent_dqwx

  subroutine drygraupel_tangent(zdghv,para_drygraupel_dqwx,pxkhv)
   !----------------------------------------------------------------------
   ! purpose: calculate the tangent linear for dry graupel
   !----------------------------------------------------------------------
   real :: zdghv
   real :: para_drygraupel_dqwx
   real :: pxkhv

    zdghv=para_drygraupel_dqwx*pxkhv*rhoair*dgr_coef*dqgr           &
         +para_drygraupel_dqwx*pxkhv*dgr*pdfrhot*dtmk               &
         +para_drygraupel_dqwx*pxkhv*dgr*pdfrhoq*dqvp               &
         -para_drygraupel_dqwx*pxkhv*pdfrrg*qgr*dqra                &
         -para_drygraupel_dqwx*pxkhv*pdfgrg*qgr*dqgr

  end subroutine drygraupel_tangent


  subroutine drygraupel_adjoint(para_drygraupel_dqwx,pxkhv)
   !----------------------------------------------------------------------
   ! purpose: calculate the adjoint associated with the contribution from dry graupel
   !----------------------------------------------------------------------
   real :: para_drygraupel_dqwx
   real :: pxkhv

    dqgr=dqgr+para_drygraupel_dqwx*pxkhv*rhoair*dgr_coef*zmm_ref &
             -para_drygraupel_dqwx*pxkhv*pdfgrg*qgr*zmm_ref
    dqra=dqra-para_drygraupel_dqwx*pxkhv*pdfrrg*qgr*zmm_ref
    dtmk=dtmk+para_drygraupel_dqwx*pxkhv*dgr*pdfrhot*zmm_ref
    dqvp=dqvp+para_drygraupel_dqwx*pxkhv*dgr*pdfrhoq*zmm_ref

  end subroutine drygraupel_adjoint


  subroutine parameter_zrx(p10,p14,rhoa,rhor,cr,dr,alphar,betarx,    &
                           alpharx,mm3todBZ,lambda,Kw2,pi,n0r)

   !----------------------------------------------------------------------
   ! purpose: calculate the constants for the contribution from rainwater
   !----------------------------------------------------------------------

  real :: p1,p2,p3,p4,p5,p7,p8,p9,p10,p11,p12,p13,p14
  real :: rhoa,rhor,cr,dr,alphar,betarx,alpharx
  real :: mm3todBZ,lambda,Kw2,pi,n0r
      
  p1=mm3todBZ*(4*lambda**4*alpharx**2/(pi**4*Kw2))
  p2=-(2.*betarx+1.0)
  p7=(pi*rhor/rhoa)**(p2/4.)
  p10=(1.+p2/4.)
  p14=p1*p7*(n0r)**p10*gamma(-p2*1d0)

  end subroutine parameter_zrx

  function lower_lambdax(rhox,dx,alphax,cx,qnx,qx)

  !-----------------------------------------------------------------------------
  !  for slope parameter using two moment scheme, not used in current version
  !-----------------------------------------------------------------------------

  real :: rhox,dx,alphax,cx,qnx,qx
  real :: gamma
  real :: lower_lambdax
      
  lower_lambdax=( (gamma((1.0+dx+alphax))*1d0*cx*qnx)   &
                 /(gamma((1.0+alphax)*1d0)*rhox*qx)    &
                  )**(1.0/dx)

  end function lower_lambdax
      
  function n0x(qnx,lower_lambda,alphax)
  !-----------------------------------------------------------------------------
  !  for intercept parameter using two moment scheme, not used in current version
  !-----------------------------------------------------------------------------      
  real :: qnx,lower_lambda,alphax
  real :: gamma
  real :: n0x
      
  n0x=qnx*lower_lambda**(1.0+alphax)/gamma(1.0+alphax)
      
  end function n0x

  function upper_f(qr,qice,qthres,flg)
   !----------------------------------------------------------------------
   ! purpose: calculate the fraction F
   !----------------------------------------------------------------------
  real :: qr, qice,qthres
  real :: upper_f
  real :: fmax=0.5
  integer :: flg

  if(flg==1) fmax=0.5 ! for snow
  if(flg==2) fmax=0.3 ! for hail/graupel
      
  upper_f=0
  if(qr>=qthres.and.qice>=qthres) then
    upper_f=fmax*min(qice/qr,qr/qice)**0.3
  endif

  end function upper_f

  function waterfraction(qr,qice)
   !----------------------------------------------------------------------
   ! purpose: calculate the water fraction fw
   !----------------------------------------------------------------------
  real :: qr,qice
  real :: waterfraction
 
  if(qr<1.e-8.and.qice<1.e-8) then
    waterfraction=1.e-8
  else
    waterfraction=qr/(qr+qice) 
  endif

  end function waterfraction


  subroutine calc_ice_abc(phimean,sigma,ice_abc)
   !----------------------------------------------------------------------
   ! purpose: calculate the A B C in ice-phase equations
   !----------------------------------------------------------------------
  real :: phimean,sigma
  real :: ice_abc(3)  ! 1 for A, 2 for B, 3 for C
  real,parameter :: pi=3.1415926
  real :: a2a=pi/180.

  ice_abc(1)=1./8.*(3.+4.*cos(2.*phimean*pi)*exp(-2.*sigma**2)  &
              +cos(4.*phimean)*exp(-8.*sigma**2))
  ice_abc(2)=1./8.*(3.-4.*cos(2.*phimean*pi)*exp(-2.*sigma**2) &
              +cos(4.*phimean)*exp(-8.*sigma**2))
  ice_abc(3)=1./8.*(1.                                          &
              -cos(4.*phimean)*exp(-8.*sigma**2)) 

  end subroutine calc_ice_abc

  function sigma_in_abc(qg,fw,gsflag)
   !----------------------------------------------------------------------
   ! purpose: calculate the standard deviation of canting angle
   !----------------------------------------------------------------------
  real :: qg,fw
  integer :: gsflag
  real :: sigma_in_abc
  real,parameter :: pi=3.1415926
  real :: a2a=pi/180.

  if(gsflag==1) then ! for snow
    sigma_in_abc=20.*a2a
  elseif(gsflag==2) then ! for graupel
    if(qg>0.2*1e-3) then 
      sigma_in_abc=60.*a2a*(1-0.8*fw)
    else
      sigma_in_abc=60.*a2a*(1-4.*qg*1e3*fw)
    endif
  endif

  end function sigma_in_abc

  function pxabk(alpha_rxa,alpha_rxb,k,n)
   !----------------------------------------------------------------------
   ! purpose: calculate PxA, PxB, PxC for Pxk
   !----------------------------------------------------------------------
  integer :: n
  integer :: k
  real :: alpha_rxa(n),alpha_rxb(n)
  real :: pxabk
  integer :: i,j

  pxabk=0
  do j=0,n-1
    do i=0,n-1
      if(i+j==k) then
        pxabk=pxabk+alpha_rxa(i+1)*alpha_rxb(j+1)
      endif
    enddo
  enddo

  end function pxabk

  function pkx(ice_abc,pxabk_all)
   !----------------------------------------------------------------------
   ! purpose: calculate PxK
   !----------------------------------------------------------------------
  real :: ice_abc(3),pxabk_all(3)
  real :: pkx
  integer :: i

  pkx=0
  do i=1,3
    if(i<3) then
      pkx=pkx+ice_abc(i)*pxabk_all(i)
    else
      pkx=pkx+2.*ice_abc(i)*pxabk_all(i)
    endif
  enddo

  end function pkx

  subroutine parameter_zxx(p1,p2,rhoa,rhox,cx,dx,alphax,      &
                           mm3todBZ,lambda,Kw2,pi,n0x)  
   !----------------------------------------------------------------------
   ! purpose: calculate the constants for ice speices
   !----------------------------------------------------------------------
  real :: p1,p2
  real :: rhoa,rhox,cx,dx,alphax
  real :: mm3todBZ,lambda,Kw2,pi,n0x
  real :: gamma

  p1=mm3todBZ*4.*gamma(7.)*lambda**4/(pi**4*Kw2)
  p2=(pi*rhox/rhoa)**(-1.75)

  end subroutine parameter_zxx

  function sum_pxkfw(pxk_all,fw,n)
   !----------------------------------------------------------------------
   ! purpose: calculate the sum of fw terms
   !----------------------------------------------------------------------
  integer :: n
  real :: pxk_all(2*n-1)
  real :: fw
  integer :: k
  real :: sum_pxkfw

  sum_pxkfw=0
  do k=0,2*(n-1)
     sum_pxkfw=sum_pxkfw+pxk_all(k+1)*fw**k
  enddo 

  end function sum_pxkfw

  function virtual(temp,ratmix)
!-----------------------------------------------------------------------
!   This function returns virtual temperature in K, given temperature
!      in K and mixing ratio in kg/kg.
!-----------------------------------------------------------------------
  real,parameter :: eps=0.622
  real :: virtual
  real :: temp
  real :: ratmix
!
  !virtual=temp*(eps+ratmix)/(eps*(1.+ratmix))
  virtual=temp*(1+0.61*ratmix)
  return
  end function virtual

  FUNCTION gamma(xx)
   !----------------------------------------------------------------------
   ! purpose: gamma function
   !----------------------------------------------------------------------
!  Modified from "Numerical Recipes"

  IMPLICIT NONE

! PASSING PARAMETERS:
  DOUBLE PRECISION, INTENT(IN) :: xx

! LOCAL PARAMETERS:
  DOUBLE PRECISION  :: gamma
  INTEGER  :: j
  DOUBLE PRECISION  :: ser,stp,tmp,x,y,cof(6)


  SAVE cof,stp
  DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,               &
       24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,  &
       -.5395239384953d-5,2.5066282746310005d0/
  x=xx
  y=x
  tmp=x+5.5d0
  tmp=(x+0.5d0)*log(tmp)-tmp
  ser=1.000000000190015d0
! do j=1,6   !original
  do j=1,4
!!do j=1,3   !gives result to within ~ 3 %
     y=y+1.d0
     ser=ser+cof(j)/y
  enddo
  gamma=tmp+log(stp*ser/x)
  gamma= exp(gamma)

  END FUNCTION gamma

end subroutine RadZIceVar
