subroutine da_radzicevar(qvp0,qra0,qsn0,qgr0,qnr0,qns0,qng0,tmk0,prs,dbz,                 &
                         in0r,in0s,in0g,rn0_r,rn0_s,rn0_g,                                &
                         rhos,rhog,dtmk,dqvp,dqra,dqsn,dqgr,dqnr,dqns,dqng,zmm,tlopt,     &
                         gropt,zmm_ref)

!===================================================================
!      Following Jung et al., 2008
!===================================================================

  implicit none
  
  integer,intent(in) :: in0r,in0s,in0g
  integer,intent(in) :: tlopt,gropt  ! tlopt=0 nonlinear,=1 tl linear; gropt=0 tl linear; >=1 adj

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
 ! above: background, below: increment for tlopt==1 and gropt==0 but adj for tlopt==1 and gropt>=1
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

  real           :: para1sg   ! para1sg=mm3todBZ*gamma(7)*lambda**4/pi**4/Kw2*(pi*rhox/rhoa)**1.75

  real           :: cs=3.1415926/6
  real           :: cice=440. 
  real           :: ds=3.
  real           :: dg=3. 
  real           :: alphas = 0
  real           :: alphag = 0

  real           :: alpha_rdsa=0.194*10.**(-4)
  real           :: alpha_rdsb=0.191*10.**(-4)
  !---------------------------------------------
  !real           :: alpha_rdha=0.191*10.**(-3)  ! hail
  !real           :: alpha_rdhb=0.165*10.**(-3)  ! hail
  !--------------------------------------------
  real           :: alpha_rdha=0.105*10.**(-3)  ! graupel
  real           :: alpha_rdhb=0.092*10.**(-3)  ! graupel

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

  real           :: para_pr,para_pdx_dq,para_pdx_df        ! for 
  real           :: tc

  !---------------------------------------------------------------------------------

  data para_alpha_rxa(:,1)/0.194e-4,7.094e-4,2.135e-4,-5.225e-4,0,0,0/
  data para_alpha_rxb(:,1)/0.191e-4,6.916e-4,-2.841e-4,-1.160e-4,0,0,0/
  !----hail--------------
  !data para_alpha_rxa(:,2)/0.191e-3,2.39e-3,-12.57e-3,38.71e-3,-65.53e-3,56.16e-3,  &
  !                        -18.98e-3/
  !data para_alpha_rxb(:,2)/0.165e-3,1.72e-3,-9.92e-3,32.15e-3,-56.0e-3,48.83e-3,   &
  !                        -16.69e-3/
  !----graupel-----------
  data para_alpha_rxa(:,2)/1.05E-04,1.82E-03,-3.77E-03,-7.97E-04,1.63E-02,-2.20E-02, &
                           8.74E-03/
  data para_alpha_rxb(:,2)/9.25E-05,1.93E-03,-9.79E-03,2.92E-02,-4.82E-02,3.93E-02, &
                          -1.22E-02/


  qthres=rf_qthres

  qra=qra0
  qsn=qsn0
  qgr=qgr0
  tmk=tmk0
  qvp=qvp0
!  if(qra0<qthres) qra=qthres
!  if(qsn0<qthres) qsn=qthres
!  if(qgr0<qthres) qgr=qthres

  tc=tmk-273.15
  if(tc<=0.0) then
    qra=max(qthres*(1+exp(-(tc-5.0)**2/6.25)),qra)
    qsn=max(qthres*2,qsn)
    qgr=max(qthres*2,qgr)
  else
    qra=max(qthres*2,qra)
    qsn=max(qthres*(1+exp(-(tc-5.0)**2/6.25)),qsn)
    qgr=max(qthres*(1+exp(-(tc-5.0)**2/6.25)),qgr)
  endif

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
 
!  if(qra<qthres.and.qsn<qthres.and.qgr<qthres) then
!     zrs=0
!     zss=0
!     zhs=0
!     ref=0
!     zmm=0
!     dbz=0
!     return
!  endif

  call da_radzicevar_rhoair_tl(tlopt,rhoair,prs,rgas,tmk,qvp,pdfrhot,pdfrhoq)

!-------------------------------------------------------
! Calculate variable intercept parameters if wanted
!-------------------------------------------------------
  call da_radzicevar_prepare_interceptpara(in0s,in0g,in0r,rn0_s,rn0_g,rn0_r,sonv,gonv,ronv)
!-------------------------------------------------------
! Calculate mixing ratios (pure rain, dry snow/graupel, wet snow/graupel)
!-------------------------------------------------------
  call da_radzicevar_prepare_mixingratios(tlopt,prain_coef,dsnow_coef,dgr_coef, &
                            prain,dsnow,wsnow,dgr,wgr,            &
                            qra,qgr,qsn,qthres,                   &
                            pdfrrs,pdfrrg,pdfsrs,pdfgrg           &
                            )

  call da_radzicevar_prepare_zmm_adj(tlopt,gropt,zmm,zmm_ref)
! ==================FOR RAIN=============================
  call da_radzicevar_parameter_zrx(para10r,para14r,para_pr,rhoair,rhor,prain,   &
                     beta_ra,alpha_ra,mm3todBZ,lambda,Kw2,pi,     &
                     ronv)

  if(in0r.eq.1) then
    ! for two moment microphysics scheme, not yet completed
  else
    if(tlopt==0) zrh=para14r*(rhoair*prain)**(1.-para10r)

  endif

  if(dualpol_opt==1) then
    call da_radzicevar_parameter_zrx(para10r,para14r,para_pr,rhoair,rhor,prain,   &
                       beta_rb,alpha_rb,mm3todBZ,lambda,Kw2,pi,     &
                       ronv)

    if(in0r.eq.1) then
     ! for two moment microphysics scheme, not yet completed
    else
      if(tlopt==0) zrv=para14r*(rhoair*prain)**(1.-para10r)

    endif
  endif
  zrs=zrh 
! ==================FOR snow=============================
  if(rf_noice==0) then
! -------------------for wet snow----------------------

    call da_radzicevar_waterfraction(qra,qsn,fw)
    rhows=rhos*(1.-fw**2)+rhor*fw**2

    call da_radzicevar_parameter_zxx(para1sg,para_pdx_dq,para_pdx_df,rhoair,rhows,wsnow,cs,ds,alphas,   &
                       mm3todBZ,lambda,Kw2,pi,sonv)  

    call da_radzicevar_sigma_in_abc(qsn,fw,1,sigma)  ! for snow
    call da_radzicevar_calc_ice_abc(phimean,sigma,ice_abc)

    ice_bac(1)=ice_abc(2)
    ice_bac(2)=ice_abc(1)
    ice_bac(3)=ice_abc(3)
    pxkh=0
    pxkv=0
    pxkh_tlr=0
    pxkv_tlr=0
    pxkh_tlx=0
    pxkv_tlx=0
    call da_radzicevar_cal_tl_fw4wetice(1,qsn,pxabk_all,para_alpha_rxa,para_alpha_rxb,  &
                               pxkh,pxkv,ice_abc,ice_bac,fw,pxkh_tlr,          &
                               pxkv_tlr,pxkh_tlx,pxkv_tlx,qra,tlopt,           &
                               npara_alpharxa) 

     if(in0s.eq.1) then
      ! for two moment microphysics scheme, not yet completed
     else
       if(tlopt==0) zsh= para1sg*sonv**(-0.75)*(rhoair*wsnow)**(1.75)*pxkh

 
     endif

     if(dualpol_opt==1) then
       if(in0s.eq.1) then
         ! for two moment microphysics scheme, not yet completed
       else
         if(tlopt==0) zsv= para1sg*sonv**(-0.75)*(rhoair*wsnow)**(1.75)*pxkv

       endif
     endif

!  -------------------for dry snow----------------------
     call da_radzicevar_parameter_zxx(para1sg,para_pdx_dq,para_pdx_df,rhoair,rhos,dsnow,cs,ds,alphas,   &
                        mm3todBZ,lambda,Kw2,pi,sonv)  
     ice_abc_d(1)=Asd
     ice_abc_d(2)=Bsd
     ice_abc_d(3)=Csd
     ice_bac_d(1)=Bsd
     ice_bac_d(2)=Asd

     pxabk_all(1)=alpha_rdsa**2
     pxabk_all(2)=alpha_rdsb**2
     pxabk_all(3)=alpha_rdsb*alpha_rdsa
     call da_radzicevar_pkx(ice_abc_d,pxabk_all,pxkh)
     call da_radzicevar_pkx(ice_bac_d,pxabk_all,pxkv)

     if(in0s.eq.1) then
       ! for two moment microphysics scheme, not yet completed        
     else
       if(tlopt==0) zdsh=para1sg*sonv**(-0.75)*(rhoair*dsnow)**(1.75)*pxkh

     endif

     if(dualpol_opt==1) then
       if(in0s.eq.1) then
        ! for two moment microphysics scheme, not yet completed
       else
         if(tlopt==0) zdsv=para1sg*sonv**(-0.75)*(rhoair*dsnow)**(1.75)*pxkv

       endif
     endif
     zss=zsh+zdsh
    
! ==================FOR graupel==========================
! -------------------for wet graupel---------------------

    call da_radzicevar_waterfraction(qra,qgr,fw)
    rhowg=rhog*(1.-fw**2)+rhor*fw**2

    call da_radzicevar_parameter_zxx(para1sg,para_pdx_dq,para_pdx_df,rhoair,rhowg,wgr,cice,dg,alphag,   &
                       mm3todBZ,lambda,Kw2,pi,gonv) 

    call da_radzicevar_sigma_in_abc(qgr,fw,2,sigma)  ! for snow
    call da_radzicevar_calc_ice_abc(phimean,sigma,ice_abc)

    ice_bac(1)=ice_abc(2)
    ice_bac(2)=ice_abc(1)
    ice_bac(3)=ice_abc(3)
    pxkh=0
    pxkv=0
    pxkh_tlr=0
    pxkv_tlr=0
    pxkh_tlx=0
    pxkv_tlx=0
    call da_radzicevar_cal_tl_fw4wetice(2,qgr,pxabk_all,para_alpha_rxa,para_alpha_rxb,       &
                               pxkh,pxkv,ice_abc,ice_bac,fw,pxkh_tlr,          &
                               pxkv_tlr,pxkh_tlx,pxkv_tlx,qra,tlopt,           &
                               npara_alpharxa) 

    if(in0g.eq.1) then
       ! for two moment microphysics scheme, not yet completed
    else
      if(tlopt==0) zgh= para1sg*gonv**(-0.75)*(rhoair*wgr)**(1.75)*pxkh


    endif

    if(dualpol_opt==1) then
       if(in0g.eq.1) then
       ! for two moment microphysics scheme, not yet completed  
       else
         if(tlopt==0) zgv= para1sg*sonv**(-0.75)*(rhoair*wgr)**(1.75)*pxkv

       endif
     endif

! --------------------------for dry graupel----------------------
 
     call da_radzicevar_parameter_zxx(para1sg,para_pdx_dq,para_pdx_df,rhoair,rhog,dgr,cice,dg,alphag,   &
                        mm3todBZ,lambda,Kw2,pi,gonv) 

     ice_abc_d(1)=Ahd
     ice_abc_d(2)=Bhd
     ice_abc_d(3)=Chd
     ice_bac_d(1)=Bhd
     ice_bac_d(2)=Ahd

     pxabk_all(1)=alpha_rdha**2
     pxabk_all(2)=alpha_rdhb**2
     pxabk_all(3)=alpha_rdhb*alpha_rdha
     call da_radzicevar_pkx(ice_abc_d,pxabk_all,pxkh)
     call da_radzicevar_pkx(ice_bac_d,pxabk_all,pxkv)

     if(in0s.eq.1) then
      ! for two moment microphysics scheme, not yet completed
     else
       if(tlopt==0) zdgh=para1sg*gonv**(-0.75)*(rhoair*dgr)**(1.75)*pxkh

     endif

     if(dualpol_opt==1) then
       if(in0s.eq.1) then
        ! for two moment microphysics scheme, not yet completed
       else
         if(tlopt==0) zdgv=para1sg*gonv**(-0.75)*(rhoair*dgr)**(1.75)*pxkv

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
     dbz = max(-15.0,10. * log10(z_e))
  endif
! save z_e mm^6 mm^-3
  zmm=z_e

end subroutine da_radzicevar
